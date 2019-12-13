{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Database.Beam.Migrate.Validity where

import           Control.Monad
import           Control.Monad.Except

import           Data.Bifunctor
import           Data.Monoid
import           Data.Foldable
import qualified Data.Map.Strict                         as M
import qualified Data.Set                                as S
import qualified Data.List                               as L
import           Data.Text                                ( Text )

import           Database.Beam.Migrate.Diff
import           Database.Beam.Migrate.Types

-- Simple type that allows us to talk about \"qualified entities\" like columns, which name might not be
-- unique globally (for which we need the 'TableName' to disambiguate things).
data Qualified a = Qualified TableName a deriving Show

data Reason =
    TableDoesntExist   TableName
  | TableAlreadyExist  TableName Table
  | TableConstraintAlreadyExist TableName TableConstraint
  | TableConstraintDoesntExist  TableName TableConstraint
  | ColumnDoesntExist  ColumnName
  | ColumnAlreadyExist ColumnName Column
  | ColumnTypeMismatch ColumnName Column ColumnType
  | ColumnConstraintAlreadyExist (Qualified ColumnName) ColumnConstraint
  | ColumnConstraintDoesntExist  (Qualified ColumnName) ColumnConstraint
  | EnumDoesntExist    EnumerationName
  | EnumAlreadyExist   EnumerationName Enumeration
  | EnumInsertionPointDoesntExist EnumerationName Enumeration Text
  | TableReferencesDeletedColumnInConstraint TableName (Qualified ColumnName) TableConstraint
  | ColumnReferencesDeletedEnum (Qualified ColumnName)
  | ColumnInPrimaryKeyCantBeNull (Qualified ColumnName)
  | NotAllColumnsExist TableName (S.Set ColumnName) (S.Set ColumnName)
  | DeletedConstraintAffectsExternalTables (TableName, TableConstraint) (Qualified ColumnName, TableConstraint)
  deriving Show

data ApplyFailed = InvalidEdit Edit Reason deriving Show

data ValidationFailed =
    InvalidTableConstraint TableConstraint               Reason
  | InvalidRemoveTable     TableName                     Reason
  | InvalidRemoveColumn    (Qualified ColumnName)        Reason
  | InvalidRemoveEnum      EnumerationName               Reason
  | InvalidRemoveColumnConstraint (Qualified ColumnName) Reason
  | InvalidRemoveTableConstraint  TableName              Reason
  deriving Show

-- | Validate removal of a 'Table'.
-- Removing a 'Table' is valid if none of the column fields are referenced in any of the other tables.
validateRemoveTable :: Schema -> TableName -> Table -> Either ValidationFailed ()
validateRemoveTable s tName tbl = do
  let tableColumnNames = map (Qualified tName) $ M.keys (tableColumns tbl)
  let otherTables      = M.delete tName (schemaTables s)
  mapM_ (checkIntegrity tableColumnNames) (M.toList otherTables)
  where
    checkIntegrity :: [Qualified ColumnName] -> (TableName, Table) -> Either ValidationFailed ()
    checkIntegrity colNames (otherTblName, otherTbl) =
      case getAlt $ asum (map (lookupColumnRef otherTblName otherTbl) colNames) of
        Nothing -> pure ()
        Just (qualifiedColName, constr) ->
          let reason = TableReferencesDeletedColumnInConstraint tName qualifiedColName constr
          in Left $ InvalidRemoveTable tName reason

-- | Lookup the input 'ColumnName' in any of the constraints of this 'Table'.
lookupColumnRef :: TableName
                -> Table
                -> Qualified ColumnName
                -> Alt Maybe (Qualified ColumnName, TableConstraint)
lookupColumnRef thisTable (tableConstraints -> constr) (Qualified extTbl colName) =
  asum (map lookupReference (S.toList constr))
  where
    lookupReference :: TableConstraint -> Alt Maybe (Qualified ColumnName, TableConstraint)
    lookupReference con = Alt $ case con of
      PrimaryKey _ cols | thisTable == extTbl ->
          if S.member colName cols then Just (Qualified thisTable colName, con) else Nothing
      PrimaryKey _ _ -> Nothing
      ForeignKey _ extTbl' columnPairs _ _ ->
        let (localCols, referencedCols) = (S.map fst columnPairs, S.map snd columnPairs)
        in if | S.member colName localCols && thisTable == extTbl    -> Just (Qualified extTbl colName, con)
              | S.member colName referencedCols && extTbl == extTbl' -> Just (Qualified extTbl colName, con)
              | otherwise -> Nothing
      Unique _ cols | thisTable == extTbl ->
        if S.member colName cols then Just (Qualified thisTable colName, con) else Nothing
      Unique _ _ -> Nothing

-- | Removing an 'Enum' is valid if none of the 'Schema's tables have columns of this type.
validateRemoveEnum :: Schema -> EnumerationName -> Either ValidationFailed ()
validateRemoveEnum s eName =
  let allTables = M.toList (schemaTables s)
  in mapM_ checkIntegrity allTables
  where
    checkIntegrity :: (TableName, Table) -> Either ValidationFailed ()
    checkIntegrity (tName, tbl) =
      case getAlt $ asum (map lookupEnumRef (M.toList $ tableColumns tbl)) of
        Nothing -> pure ()
        Just colName ->
          let reason = ColumnReferencesDeletedEnum (Qualified tName colName)
          in Left $ InvalidRemoveEnum eName reason

    lookupEnumRef :: (ColumnName, Column) -> Alt Maybe ColumnName
    lookupEnumRef (colName, col) = Alt $
      case columnType col of
        PgSpecificType (PgEnumeration eName') ->
            if eName' == eName then Just colName else Nothing
        _ -> Nothing

-- | Adding a Table constraint is valid IFF:
-- 1. For a 'PrimaryKey', all the referenced columns must exist in the 'Table';
-- 2. For a 'Unique', all the referenced columns must exist in the 'Table';
-- 3. For a 'ForeignKey', all the columns (both local and referenced) must exist;
-- 4. For a 'ForeignKey', the referenced columns must all be UNIQUE or PRIMARY keys.
validateAddTableConstraint :: Schema -> TableName -> Table -> TableConstraint -> Either ValidationFailed ()
validateAddTableConstraint s tName tbl c = case c of
  PrimaryKey _ cols | cols `S.isSubsetOf` allTblColumns  -> Right ()
  PrimaryKey _ cols ->
    Left $ InvalidTableConstraint c (NotAllColumnsExist tName (S.difference cols allTblColumns) allTblColumns)
  ForeignKey _ referencedTable columnPairs _ _ -> checkIntegrity referencedTable columnPairs
  Unique _ cols | cols `S.isSubsetOf` allTblColumns -> Right ()
  Unique _ cols ->
    Left $ InvalidTableConstraint c (NotAllColumnsExist tName (S.difference cols allTblColumns) allTblColumns)
  where
    allTblColumns :: S.Set ColumnName
    allTblColumns = M.keysSet . tableColumns $ tbl

    checkIntegrity :: TableName -> S.Set (ColumnName, ColumnName) -> Either ValidationFailed ()
    checkIntegrity referencedTable columnPairs = runExcept $ liftEither $
      case M.lookup referencedTable (schemaTables s) of
        Nothing     -> throwError $ InvalidTableConstraint c (TableDoesntExist referencedTable)
        Just extTbl -> do
          let allExtColums = M.keysSet (tableColumns extTbl)
          let (localCols, referencedCols) = (S.map fst columnPairs, S.map snd columnPairs)
          if | not (localCols `S.isSubsetOf` allTblColumns) ->
               throwError $ InvalidTableConstraint c (NotAllColumnsExist tName (S.difference localCols allTblColumns) allTblColumns)
             | not (referencedCols `S.isSubsetOf` allExtColums) ->
               throwError $ InvalidTableConstraint c (NotAllColumnsExist referencedTable (S.difference referencedCols allTblColumns) allExtColums)
             | otherwise -> Right () -- TODO(and) Check UNIQUE or PK constraint.

-- | Removing a Table constraint is valid IFF:
-- 1. For a 'PrimaryKey' we need to check that none of the columns appears in any 'ForeignKey' constraints
--    of the other tables;
-- 2. For a 'Unique', we must check that none of the columns appear in any 'ForeignKey' of of the other
--    tables.
-- 3. For a 'ForeignKey', no check is necessary.
validateRemoveTableConstraint :: Schema -> TableName -> TableConstraint -> Either ValidationFailed ()
validateRemoveTableConstraint s tName c = case c of
  PrimaryKey _ cols    ->
    forM_ (M.toList allOtherTables) (checkIntegrity (map (Qualified tName) . S.toList $ cols))
  Unique     _ cols    ->
    forM_ (M.toList allOtherTables) (checkIntegrity (map (Qualified tName) . S.toList $ cols))
  ForeignKey _ _ _ _ _ -> Right ()
  where
    allOtherTables :: Tables
    allOtherTables = M.delete tName (schemaTables s)

    checkIntegrity :: [Qualified ColumnName] -> (TableName, Table) -> Either ValidationFailed ()
    checkIntegrity ourColNames (extTable, tbl) =
      case getAlt $ asum (map (lookupColumnRef extTable tbl) ourColNames) of
        Nothing -> Right ()
        Just (colName, constr) ->
          let reason = DeletedConstraintAffectsExternalTables (tName, c) (colName, constr)
          in Left $ InvalidRemoveTableConstraint tName reason

-- | Removing a 'Column' is valid iff the column is not referenced in any tables' constraints.
validateRemoveColumn :: Schema -> TableName -> ColumnName -> Either ValidationFailed ()
validateRemoveColumn s tName colName = mapM_ checkIntegrity (M.toList (schemaTables s))
  where
    checkIntegrity :: (TableName, Table) -> Either ValidationFailed ()
    checkIntegrity (otherTblName, otherTbl) =
      case getAlt $ asum (map (lookupColumnRef otherTblName otherTbl) [Qualified tName colName]) of
        Nothing -> pure ()
        Just (_, constr) ->
          let reason = TableReferencesDeletedColumnInConstraint otherTblName (Qualified tName colName) constr
          in Left $ InvalidRemoveColumn (Qualified tName colName) reason

-- | Removing a column constraint will violate referential integrity if the constraint is 'NotNull' and
-- this column appears in the primary key.
validateRemoveColumnConstraint :: Table
                               -> Qualified ColumnName
                               -> ColumnConstraint
                               -> Either ValidationFailed ()
validateRemoveColumnConstraint tbl (Qualified tName colName) = \case
  NotNull -> mapM_ checkIntegrity (tableConstraints tbl)
  Default _ -> pure ()
  where
    checkIntegrity :: TableConstraint -> Either ValidationFailed ()
    checkIntegrity constr = case constr of
      PrimaryKey _ cols ->
        let reason = ColumnInPrimaryKeyCantBeNull (Qualified tName colName)
        in if S.member colName cols
              then Left $ InvalidRemoveColumnConstraint (Qualified tName colName) reason
              else Right ()
      ForeignKey _ _ _ _ _ -> Right ()
      Unique _ _ -> Right ()

-- | Convert a 'ValidationFailed' into an 'ApplyFailed'.
toApplyFailed :: Edit -> ValidationFailed -> ApplyFailed
toApplyFailed e (InvalidTableConstraint _ reason)        = InvalidEdit e reason
toApplyFailed e (InvalidRemoveTable     _ reason)        = InvalidEdit e reason
toApplyFailed e (InvalidRemoveColumn    _ reason)        = InvalidEdit e reason
toApplyFailed e (InvalidRemoveEnum      _ reason)        = InvalidEdit e reason
toApplyFailed e (InvalidRemoveColumnConstraint _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveTableConstraint _ reason)  = InvalidEdit e reason

-- | Tries to apply a list of edits to a 'Schema' to generate a new one.
applyEdits :: [WithPriority Edit] -> Schema -> Either ApplyFailed Schema
applyEdits (sortEdits -> edits) s = foldM applyEdit s (map (fst . unPriority) edits)

applyEdit :: Schema -> Edit -> Either ApplyFailed Schema
applyEdit s e = runExcept $ case e of

  TableAdded tName tbl -> liftEither $ do
    tables' <- M.alterF (\case
                       -- Constaints are added as a separate edit step.
      Nothing       -> Right (Just tbl { tableConstraints = mempty })
      Just existing -> Left (InvalidEdit e (TableAlreadyExist tName existing))
                        ) tName (schemaTables s)
    pure $ s { schemaTables = tables' }

  TableRemoved tName   ->
    withExistingTable tName e s (removeTable e s tName)

  TableConstraintAdded   tName con ->
    withExistingTable tName e s (addTableConstraint e s con tName)

  TableConstraintRemoved tName con ->
    withExistingTable tName e s (removeTableConstraint e s con tName)

  ColumnAdded tName colName col ->
    withExistingTable tName e s (addColumn e colName col)

  ColumnRemoved tName colName ->
    withExistingTable tName e s (removeColumn e s colName tName)

  ColumnTypeChanged tName colName oldType newType ->
    withExistingColumn tName colName e s (\_ -> changeColumnType e colName oldType newType)

  ColumnConstraintAdded   tName colName con ->
    withExistingColumn tName colName e s (\_ -> addColumnConstraint e tName con colName)

  ColumnConstraintRemoved tName colName con ->
    withExistingColumn tName colName e s (\tbl -> removeColumnConstraint e tbl tName colName con)

  EnumTypeAdded       eName enum -> liftEither $ do
    enums' <- M.alterF (\case
      Nothing       -> Right (Just enum)
      Just existing -> Left (InvalidEdit e (EnumAlreadyExist eName existing))
                        ) eName (schemaEnumerations s)
    pure $ s { schemaEnumerations = enums' }

  EnumTypeRemoved     eName ->
    withExistingEnum eName e s (removeEnum e s eName)

  EnumTypeValueAdded  eName addedValue insOrder insPoint ->
    withExistingEnum eName e s (addValueToEnum e eName addedValue insOrder insPoint)

--
-- Various combinators for specific parts of a Schema
--

removeTable :: Edit -> Schema -> TableName -> Table -> Either ApplyFailed (Maybe Table)
removeTable e s tName t = runExcept . withExcept (toApplyFailed e) . liftEither $ do
    validateRemoveTable s tName t
    pure Nothing

addColumn :: Edit -> ColumnName -> Column -> Table -> Either ApplyFailed (Maybe Table)
addColumn e colName col tbl = liftEither $ do
  columns' <- M.alterF (\case
                     -- Constaints are added as a separate edit step.
    Nothing       -> Right (Just col { columnConstraints = mempty })
    Just existing -> Left (InvalidEdit e (ColumnAlreadyExist colName existing))
                      ) colName (tableColumns tbl)
  pure $ Just tbl { tableColumns = columns' }

removeColumn :: Edit -> Schema -> ColumnName -> TableName -> Table -> Either ApplyFailed (Maybe Table)
removeColumn e s colName tName tbl = liftEither $ do
  columns' <- M.alterF (\case
    Nothing  -> Left (InvalidEdit e (ColumnDoesntExist colName))
    Just _   -> bimap (toApplyFailed e) id (validateRemoveColumn s tName colName) >> pure Nothing
                      ) colName (tableColumns tbl)
  pure $ Just tbl { tableColumns = columns' }

changeColumnType :: Edit
                 -> ColumnName
                 -> ColumnType
                 -- ^ old type
                 -> ColumnType
                 -- ^ new type
                 -> Column
                 -> Either ApplyFailed (Maybe Column)
changeColumnType e colName oldType newType col =
  if columnType col /= oldType
     then Left  $ InvalidEdit e (ColumnTypeMismatch colName col oldType)
     else pure . Just $ col { columnType = newType }

addColumnConstraint :: Edit
                    -> TableName
                    -> ColumnConstraint
                    -> ColumnName
                    -> Column
                    -> Either ApplyFailed (Maybe Column)
addColumnConstraint e tName constr colName col =
  let constraints = columnConstraints col
  in if S.member constr constraints
       then Left (InvalidEdit e (ColumnConstraintAlreadyExist (Qualified tName colName) constr))
       else pure . Just $ col { columnConstraints = S.insert constr constraints }

removeColumnConstraint :: Edit
                       -> Table
                       -> TableName
                       -> ColumnName
                       -> ColumnConstraint
                       -> Column
                       -> Either ApplyFailed (Maybe Column)
removeColumnConstraint e tbl tName colName constr col = do
  let constraints = columnConstraints col
  constraints' <-
    if S.member constr constraints
       then Left (InvalidEdit e (ColumnConstraintDoesntExist (Qualified tName colName) constr))
       else removeConstraint constraints
  pure . Just $ col { columnConstraints = constraints' }
  where
    removeConstraint :: S.Set ColumnConstraint -> Either ApplyFailed (S.Set ColumnConstraint)
    removeConstraint constraints = runExcept . withExcept (toApplyFailed e) . liftEither $ do
       validateRemoveColumnConstraint tbl (Qualified tName colName) constr
       pure (S.insert constr constraints)

-- | Performs an action over an existing 'Table', failing if the 'Table' doesn't exist.
withExistingTable :: TableName
                  -> Edit
                  -> Schema
                  -> (Table -> Either ApplyFailed (Maybe Table) )
                  -> Except ApplyFailed Schema
withExistingTable tName e s action = liftEither $ do
  tables' <- M.alterF (\case
    Nothing    -> Left (InvalidEdit e (TableDoesntExist tName))
    Just table -> action table
                      ) tName (schemaTables s)
  pure $ s { schemaTables = tables' }

-- | Performs an action over an existing 'Column', failing if the 'Column' doesn't exist.
withExistingColumn :: TableName
                   -> ColumnName
                   -> Edit
                   -> Schema
                   -> (Table -> Column -> Either ApplyFailed (Maybe Column) )
                   -> Except ApplyFailed Schema
withExistingColumn tName colName e s action =
  withExistingTable tName e s (\tbl -> do
    columns' <- M.alterF (\case
      Nothing       -> Left (InvalidEdit e (ColumnDoesntExist colName))
      Just existing -> action tbl existing
                         ) colName (tableColumns tbl)
    pure $ Just tbl { tableColumns = columns' })

-- | Performs an action over an existing 'Enum', failing if the 'Enum' doesn't exist.
withExistingEnum :: EnumerationName
                 -> Edit
                 -> Schema
                 -> (Enumeration -> Either ApplyFailed (Maybe Enumeration) )
                 -> Except ApplyFailed Schema
withExistingEnum eName e s action = liftEither $ do
  enums' <- M.alterF (\case
    Nothing    -> Left (InvalidEdit e (EnumDoesntExist eName))
    Just enum -> action enum
                      ) eName (schemaEnumerations s)
  pure $ s { schemaEnumerations = enums' }

addTableConstraint :: Edit
                   -> Schema
                   -> TableConstraint
                   -> TableName
                   -> Table
                   -> Either ApplyFailed (Maybe Table)
addTableConstraint e s con tName tbl = liftEither $ do
  let constraints = tableConstraints tbl
  constraints' <-
    if S.member con constraints
       then Left (InvalidEdit e (TableConstraintAlreadyExist tName con))
       else addConstraint constraints
  pure $ Just tbl { tableConstraints = constraints' }
  where
    addConstraint :: S.Set TableConstraint -> Either ApplyFailed (S.Set TableConstraint)
    addConstraint cons = runExcept . withExcept (toApplyFailed e) . liftEither $ do
      validateAddTableConstraint s tName tbl con
      pure $ S.insert con cons

removeTableConstraint :: Edit
                      -> Schema
                      -> TableConstraint
                      -> TableName
                      -> Table
                      -> Either ApplyFailed (Maybe Table)
removeTableConstraint e s con tName tbl = liftEither $ do
  let constraints = tableConstraints tbl
  constraints' <-
    if S.member con constraints
       then removeConstraint constraints
       else Left (InvalidEdit e (TableConstraintDoesntExist tName con))
  pure $ Just tbl { tableConstraints = constraints' }
  where
    removeConstraint :: S.Set TableConstraint -> Either ApplyFailed (S.Set TableConstraint)
    removeConstraint cons = runExcept . withExcept (toApplyFailed e) . liftEither $ do
      validateRemoveTableConstraint s tName con
      pure $ S.delete con cons

removeEnum :: Edit
           -> Schema
           -> EnumerationName
           -> Enumeration
           -> Either ApplyFailed (Maybe Enumeration)
removeEnum e s eName _ = runExcept . withExcept (toApplyFailed e) . liftEither $ do
    validateRemoveEnum s eName
    pure Nothing

addValueToEnum :: Edit
               -> EnumerationName
               -> Text
               -- ^ value to insert
               -> InsertionOrder
               -> Text
               -- ^ insertion point
               -> Enumeration
               -> Either ApplyFailed (Maybe Enumeration)
addValueToEnum e eName addedValue insOrder insPoint (Enumeration vals) =
    case insOrder of
      Before ->
        case L.elemIndex insPoint vals of
          Nothing -> Left (InvalidEdit e (EnumInsertionPointDoesntExist eName (Enumeration vals) insPoint))
          Just ix | ix == 0 -> pure . Just $ Enumeration (addedValue : vals)
          Just ix ->
            let (hd, tl) = L.splitAt (ix - 1) vals
            in pure . Just $ Enumeration (hd <> (addedValue : tl))
      After  ->
        let (hd, tl) = L.break ((==) insPoint) vals
        in pure . Just $ Enumeration (hd <> (addedValue : tl))
