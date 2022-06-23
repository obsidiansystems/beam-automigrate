{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Beam.AutoMigrate.Validity
  ( -- * Types
    Reason (..),
    ApplyFailed,
    ValidationFailed,

    -- * Applying edits to a 'Schema'
    applyEdits,

    -- * Validing a 'Schema'
    validateSchema,
    validateSchemaTables,
    validateSchemaEnums,
    validateTableConstraint,
    validateColumn,
  )
where

import Control.Lens (Lens', (^.), lens, at, (<<.=), (.=), ix, preuse, preview, _Nothing, set, _Unwrapped)
import Control.Monad.State.Strict (execStateT)
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import Database.Beam.AutoMigrate.Diff
import Database.Beam.AutoMigrate.Types

data SomeTableConstraint
  = SomePrimaryKey PrimaryKeyConstraint UniqueConstraintOptions
  | SomeForeignKey ForeignKey ForeignKeyConstraintOptions
  | SomeUnique Unique UniqueConstraintOptions
  deriving (Eq, Ord, Show)

someTableConstraintName :: SomeTableConstraint -> Maybe ConstraintName
someTableConstraintName = (^. _someTableConstraintName)

_someTableConstraintName :: Lens' SomeTableConstraint (Maybe ConstraintName)
_someTableConstraintName = lens
  (\case
    SomePrimaryKey _ u -> uniqueConstraintName u
    SomeUnique _ u -> uniqueConstraintName u
    SomeForeignKey _ fkOpts -> foreignKeyConstraintName fkOpts)
  (\case
    SomePrimaryKey s u -> \b -> SomePrimaryKey s $ u {uniqueConstraintName = b}
    SomeUnique s u -> \b -> SomeUnique s $ u {uniqueConstraintName = b}
    SomeForeignKey s fkOpts -> \b -> SomeForeignKey s $ fkOpts {foreignKeyConstraintName = b})


tableConstraintsToList :: TableConstraints -> [SomeTableConstraint]
tableConstraintsToList c = concat
  [ uncurry SomePrimaryKey <$> toList (primaryKeyConstraint c)
  , uncurry SomeForeignKey <$> M.toList (foreignKeyConstraints c)
  , uncurry SomeUnique <$> M.toList (uniqueConstraints c)
  ]



-- | Simple type that allows us to talk about \"qualified entities\" like columns, which name might not be
-- unique globally (for which we need the 'TableName' to disambiguate things).
data Qualified a = Qualified TableName a deriving (Show, Eq)

data Reason
  = -- | The 'Table' we were trying to edit didn't exist.
    TableDoesntExist TableName
  | -- | The 'Table' we were trying to create already existed.
    TableAlreadyExist TableName Table
  | -- | The 'TableConstraint' we were trying to add already existed.
    TableConstraintAlreadyExist TableName (S.Set SomeTableConstraint)
  | -- | The 'TableConstraint' we were trying to delete didn't exist.
    TableConstraintDoesntExist TableName ConstraintName
  | -- | The 'Column' we were trying to edit didn't exist.
    ColumnDoesntExist (Qualified ColumnName)
  | -- | The 'Column' we were trying to add already existed.
    ColumnAlreadyExist ColumnName Column
  | -- | The old type for the input 'Column' didn't match the type contained in the 'Edit' step.
    ColumnTypeMismatch ColumnName Column ColumnType
  | -- | The 'Default' we were trying to delete didn't exist.
    NoDefaultExists (Qualified ColumnName)
  | -- | The 'Enum' we were trying to edit didn't exist.
    EnumDoesntExist EnumerationName
  | -- | The 'Enum' we were trying to add already existed.
    EnumAlreadyExist EnumerationName Enumeration
  | -- | The value in this 'Enum' to be used to insert a new one before/after it didn't exist.
    EnumInsertionPointDoesntExist EnumerationName Enumeration Text
  | -- | The 'Sequence' we were trying to add already existed.
    SequenceAlreadyExist SequenceName (Maybe Sequence)
  | -- | The 'Sequence' we were trying to edit didn't exist.
    SequenceDoesntExist SequenceName
  | -- | This 'Table' references a deleted 'Column' in one of its 'TableConstraint's.
    TableReferencesDeletedColumnInConstraint TableName (Qualified ColumnName) SomeTableConstraint
  | -- | This 'Column' references an 'Enum' which doesn't exist.
    ColumnReferencesNonExistingEnum (Qualified ColumnName) EnumerationName
  | -- | This 'Column' allows NULL values but it has been selected as a PRIMARY key.
    ColumnInPrimaryKeyCantBeNull (Qualified ColumnName)
  | -- | This 'Table' has a 'ForeignKey' constaint in it which references external columns which are either
    -- not unique or not fields of a PRIMARY KEY.
    ColumnsInFkAreNotUniqueOrPrimaryKeyFields TableName [Qualified ColumnName]
  | ColumnStillReferencesSequence SequenceName (Qualified ColumnName)
  | -- | This 'TableConstraint' references one or more 'Column's which don't exist.
    NotAllColumnsExist TableName (S.Set ColumnName) (S.Set ColumnName)
  | -- | Deleting this 'TableConstraint' would affect the selected external 'Column's and some external
    -- 'TableConstraint's.
    DeletedConstraintAffectsExternalTables (TableName, SomeTableConstraint) (Qualified ColumnName, SomeTableConstraint)
  | EnumContainsDuplicateValues EnumerationName [Text]
  | ColumnDoesNotDefaultToOwnedSequence TableName ColumnName SequenceName
  deriving (Show, Eq)

data ApplyFailed
  = InvalidEdit Edit Reason
  deriving (Show, Eq)

data ValidationFailed
  = InvalidTableConstraint SomeTableConstraint Reason
  | InvalidRemoveTable TableName Reason
  | InvalidRemoveColumn (Qualified ColumnName) Reason
  | InvalidRemoveEnum EnumerationName Reason
  | InvalidRemoveSequence SequenceName Reason
  | InvalidEnum EnumerationName Reason
  | InvalidColumn (Qualified ColumnName) Reason
  | InvalidRemoveColumnConstraint (Qualified ColumnName) Reason
  | InvalidRemoveTableConstraint TableName Reason
  deriving (Show, Eq)

--
-- Validating a Schema and a set of edit actions.
--

-- | Validate a 'Schema', returning an error in case the validation didn't succeed. We never contemplate
-- the case where any of the entities names are empty (i.e. the empty string) as that clearly indicates a
-- bug in the library, not a user error that needs to be reported.
validateSchema :: Schema -> Either [ValidationFailed] ()
validateSchema s = runExcept $ do
  liftEither (validateSchemaTables s)
  liftEither (validateSchemaEnums s)

-- | A 'Table' is not valid if:
-- 1. Any of its 'Column's are not valid;
-- 2. Any of its 'TableConstraint's are not valid.
validateSchemaTables :: Schema -> Either [ValidationFailed] ()
validateSchemaTables s = forM_ (M.toList $ schemaTables s) validateTable
  where
    validateTable :: (TableName, Table) -> Either [ValidationFailed] ()
    validateTable (tName, tbl) = do
      forM_ (tableConstraintsToList $ tableConstraints tbl) (first (: []) . validateTableConstraint s tName tbl)
      forM_ (M.toList $ tableColumns tbl) (validateColumn s tName)

-- | Validate a 'TableConstraint', making sure referential integrity is not violated.
-- A Table constraint is valid IFF:
-- 1. For a 'PrimaryKey', all the referenced columns must exist in the 'Table';
-- 2. For a 'Unique', all the referenced columns must exist in the 'Table';
-- 3. For a 'ForeignKey', all the columns (both local and referenced) must exist;
-- 4. For a 'ForeignKey', the referenced columns must all be UNIQUE or PRIMARY keys.
-- TODO: validate constraint name length is shorter than 64 characters (in postgres)
validateTableConstraint :: Schema -> TableName -> Table -> SomeTableConstraint -> Either ValidationFailed ()
validateTableConstraint s tName tbl c = case c of
  SomePrimaryKey (PrimaryKey cols) _ | cols `S.isSubsetOf` allTblColumns -> Right ()
  SomePrimaryKey (PrimaryKey cols) _ ->
    Left $ InvalidTableConstraint c (NotAllColumnsExist tName (S.difference cols allTblColumns) allTblColumns)
  SomeForeignKey (ForeignKey referencedTable columnPairs) _ -> checkFkIntegrity referencedTable columnPairs
  SomeUnique (Unique cols) _ | cols `S.isSubsetOf` allTblColumns -> Right ()
  SomeUnique (Unique cols) _ ->
    Left $ InvalidTableConstraint c (NotAllColumnsExist tName (S.difference cols allTblColumns) allTblColumns)
  where
    allTblColumns :: S.Set ColumnName
    allTblColumns = M.keysSet . tableColumns $ tbl

    checkFkIntegrity :: TableName -> S.Set (ColumnName, ColumnName) -> Either ValidationFailed ()
    checkFkIntegrity referencedTable columnPairs = runExcept $
      liftEither $
        case M.lookup referencedTable (schemaTables s) of
          Nothing -> throwError $ InvalidTableConstraint c (TableDoesntExist referencedTable)
          Just extTbl -> do
            let allExtColumns = M.keysSet (tableColumns extTbl)
            let (localCols, referencedCols) = (S.map fst columnPairs, S.map snd columnPairs)
            if
                | not (localCols `S.isSubsetOf` allTblColumns) ->
                  throwError $ InvalidTableConstraint c (NotAllColumnsExist tName (S.difference localCols allTblColumns) allTblColumns)
                | not (referencedCols `S.isSubsetOf` allExtColumns) ->
                  throwError $ InvalidTableConstraint c (NotAllColumnsExist referencedTable (S.difference referencedCols allTblColumns) allExtColumns)
                | otherwise -> checkColumnsIntegrity referencedTable extTbl referencedCols

    -- Check that all these columns are either 'UNIQUE' or 'PRIMARY KEY' in the input 'Table'.
    checkColumnsIntegrity :: TableName -> Table -> S.Set ColumnName -> Either ValidationFailed ()
    checkColumnsIntegrity extName extTbl referencedCols =
      let checkConstraint extCon = case extCon of
            SomeForeignKey (ForeignKey {}) _ -> Nothing
            SomePrimaryKey (PrimaryKey cols) _ | referencedCols `S.isSubsetOf` cols -> Just ()
            SomePrimaryKey (PrimaryKey {}) _-> Nothing
            SomeUnique (Unique cols) _ | referencedCols `S.isSubsetOf` cols -> Just ()
            SomeUnique (Unique {}) _ -> Nothing
       in case asum (map checkConstraint (tableConstraintsToList $ tableConstraints extTbl)) of
            Nothing ->
              let reason = ColumnsInFkAreNotUniqueOrPrimaryKeyFields tName (map (Qualified extName) (S.toList referencedCols))
               in Left $ InvalidTableConstraint c reason
            Just () -> Right ()

-- | Validate 'Column'.
-- NOTE(adn) For now in this context a 'Column' is always considered valid, /except/ if it references an
-- 'Enum' type which doesn't exist.
validateColumn :: Schema -> TableName -> (ColumnName, Column) -> Either [ValidationFailed] ()
validateColumn s tName (colName, col) =
  when (isPgEnum $ columnType col) $
    forM_ (M.keys $ schemaEnumerations s) $ \eName ->
      case getAlt $ lookupEnumRef eName (colName, col) of
        Nothing ->
          let reason = ColumnReferencesNonExistingEnum (Qualified tName colName) eName
           in Left [InvalidColumn (Qualified tName colName) reason]
        Just _ -> Right ()
  where
    isPgEnum :: ColumnType -> Bool
    isPgEnum (PgSpecificType (PgEnumeration _)) = True
    isPgEnum _ = False

-- | A 'Schema' enum is considered always valid in this context /except/ if it contains duplicate values.
validateSchemaEnums :: Schema -> Either [ValidationFailed] ()
validateSchemaEnums s = forM_ (M.toList $ schemaEnumerations s) validateEnum
  where
    validateEnum :: (EnumerationName, Enumeration) -> Either [ValidationFailed] ()
    validateEnum (eName, (Enumeration vals)) =
      if length vals /= length (S.fromList vals)
        then Left [InvalidEnum eName (EnumContainsDuplicateValues eName vals)]
        else Right ()

-- | Validate removal of a 'Table'.
-- Removing a 'Table' is valid if none of the column fields are referenced in any of the other tables.
validateRemoveTable :: Schema -> TableName -> Table -> Either ValidationFailed ()
validateRemoveTable s tName tbl = do
  let tableColumnNames = map (Qualified tName) $ M.keys (tableColumns tbl)
  let otherTables = M.delete tName (schemaTables s)
  mapM_ (checkIntegrity tableColumnNames) (M.toList otherTables)
  where
    checkIntegrity :: [Qualified ColumnName] -> (TableName, Table) -> Either ValidationFailed ()
    checkIntegrity colNames (otherTblName, otherTbl) =
      case getAlt $ asum (map (lookupColumnRef otherTblName otherTbl) colNames) of
        Nothing -> pure ()
        Just (qualifiedColName, constr) ->
          let reason = TableReferencesDeletedColumnInConstraint tName qualifiedColName constr
           in Left $ InvalidRemoveTable tName reason

-- | The workhorse of the validation engine. It lookups the input 'ColumnName' in any of the constraints
-- of the input 'Table'.
lookupColumnRef ::
  TableName ->
  Table ->
  Qualified ColumnName ->
  Alt Maybe (Qualified ColumnName, SomeTableConstraint)
lookupColumnRef thisTable (tableConstraintsToList . tableConstraints -> constr) (Qualified extTbl colName) =
  asum (map lookupReference constr)
  where
    lookupReference :: SomeTableConstraint -> Alt Maybe (Qualified ColumnName, SomeTableConstraint)
    lookupReference con = Alt $ case con of
      SomePrimaryKey (PrimaryKey cols) _
        | thisTable == extTbl ->
          if S.member colName cols then Just (Qualified thisTable colName, con) else Nothing
      SomePrimaryKey (PrimaryKey _) _ -> Nothing
      SomeForeignKey (ForeignKey extTbl' columnPairs) _ ->
        let (localCols, referencedCols) = (S.map fst columnPairs, S.map snd columnPairs)
         in if
                | S.member colName localCols && thisTable == extTbl -> Just (Qualified extTbl colName, con)
                | S.member colName referencedCols && extTbl == extTbl' -> Just (Qualified extTbl colName, con)
                | otherwise -> Nothing
      SomeUnique (Unique cols) _
        | thisTable == extTbl ->
          if S.member colName cols then Just (Qualified thisTable colName, con) else Nothing
      SomeUnique (Unique _) _ -> Nothing

-- | Check that the input 'Column's type matches the input 'EnumerationName'.
lookupEnumRef :: EnumerationName -> (ColumnName, Column) -> Alt Maybe ColumnName
lookupEnumRef eName (colName, col) = Alt $
  case columnType col of
    PgSpecificType (PgEnumeration eName') ->
      if eName' == eName then Just colName else Nothing
    _ -> Nothing

-- | Removing an 'Enum' is valid if none of the 'Schema's tables have columns of this type.
validateRemoveEnum :: Schema -> EnumerationName -> Either ValidationFailed ()
validateRemoveEnum s eName =
  let allTables = M.toList (schemaTables s)
   in mapM_ checkIntegrity allTables
  where
    checkIntegrity :: (TableName, Table) -> Either ValidationFailed ()
    checkIntegrity (tName, tbl) =
      case getAlt $ asum (map (lookupEnumRef eName) (M.toList $ tableColumns tbl)) of
        Nothing -> pure ()
        Just colName ->
          let reason = ColumnReferencesNonExistingEnum (Qualified tName colName) eName
           in Left $ InvalidRemoveEnum eName reason

-- | Checking that the removal of a 'Sequence' is valid if this sequence
-- is still referenced by the target column.
validateRemoveSequence :: Schema -> SequenceName -> Maybe Sequence -> Either ValidationFailed ()
validateRemoveSequence _ _ Nothing = pure () -- unowned sequences can (probably) be removes all the time
validateRemoveSequence s sName (Just (Sequence targetTable targetColumn)) =
  let mbCol = do
        tbl <- M.lookup targetTable (schemaTables s)
        col <- M.lookup targetColumn (tableColumns tbl)
        pure $ any hasNextValConstraint (columnDefault (columnConstraints col))
   in case mbCol of
        Just True ->
          let reason = ColumnStillReferencesSequence sName (Qualified targetTable targetColumn)
           in Left $ InvalidRemoveSequence sName reason
        _ -> Right ()
  where
    hasNextValConstraint :: DefaultConstraint -> Bool
    hasNextValConstraint (Autoincrement {}) = True
    hasNextValConstraint _ = False

-- | Validate that adding a new 'TableConstraint' doesn't violate referential integrity.
validateAddTableConstraint :: Schema -> TableName -> Table -> SomeTableConstraint -> Either ValidationFailed ()
validateAddTableConstraint = validateTableConstraint

-- | Removing a Table constraint is valid IFF:
-- 1. For a 'PrimaryKey' we need to check that none of the columns appears in any 'ForeignKey' constraints
--    of the other tables;
-- 2. For a 'Unique', we must check that none of the columns appear in any 'ForeignKey' of of the other
--    tables.
-- 3. For a 'ForeignKey', no check is necessary.
validateRemoveTableConstraint :: Schema -> TableName -> SomeTableConstraint -> Either ValidationFailed ()
validateRemoveTableConstraint s tName c = case c of
  SomePrimaryKey (PrimaryKey cols) _ ->
    forM_ (M.toList allOtherTables) (checkIntegrity (map (Qualified tName) . S.toList $ cols))
  SomeUnique (Unique cols) _ ->
    forM_ (M.toList allOtherTables) (checkIntegrity (map (Qualified tName) . S.toList $ cols))
  SomeForeignKey (ForeignKey {}) _ -> Right ()
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

changeColumnNullable ::
  NullableConstraint ->
  Column ->
  Either ApplyFailed (Maybe Column)
changeColumnNullable newNullable col = pure . Just $
  -- TODO: validateRemoveColumnConstraint!!!
  set (_columnConstraints . _columnNullable) newNullable col

-- -- | Removing a column constraint will violate referential integrity if the constraint is 'NotNull' and
-- -- this column appears in the primary key.
-- validateRemoveColumnConstraint ::
--   Table ->
--   Qualified ColumnName ->
--   -- ColumnConstraint ->
--   Either ValidationFailed ()
-- validateRemoveColumnConstraint tbl (Qualified tName colName) = \case
--   NotNull -> mapM_ checkIntegrity (tableConstraintsToList $ tableConstraints tbl)
--   Default _ -> pure ()
--   where
--     checkIntegrity :: SomeTableConstraint -> Either ValidationFailed ()
--     checkIntegrity constr = case constr of
--       SomePrimaryKey (PrimaryKey cols) _ ->
--         let reason = ColumnInPrimaryKeyCantBeNull (Qualified tName colName)
--          in if S.member colName cols
--               then Left $ InvalidRemoveColumnConstraint (Qualified tName colName) reason
--               else Right ()
--       SomeForeignKey {} -> Right ()
--       SomeUnique {} -> Right ()

-- | Convert a 'ValidationFailed' into an 'ApplyFailed'.
toApplyFailed :: Edit -> ValidationFailed -> ApplyFailed
toApplyFailed e (InvalidTableConstraint _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveTable _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveColumn _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveEnum _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveSequence _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidEnum _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidColumn _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveColumnConstraint _ reason) = InvalidEdit e reason
toApplyFailed e (InvalidRemoveTableConstraint _ reason) = InvalidEdit e reason

-- | Tries to apply a list of edits to a 'Schema' to generate a new one. Fails with an 'ApplyFailed' error
-- if the input list of 'Edit's would generate an invalid 'Schema'.
applyEdits :: [WithPriority Edit] -> Schema -> Either ApplyFailed Schema
applyEdits (sortEdits -> edits) s = foldM applyEdit s (map (fst . unPriority) edits)

applyEdit :: Schema -> Edit -> Either ApplyFailed Schema
applyEdit s edit@(Edit e _safety) = runExcept $ case e of
  TableAdded tName tbl -> liftEither $ do
    tables' <-
      M.alterF
        ( \case
            -- Constaints are added as a separate edit step.
            Nothing -> Right (Just tbl {tableConstraints = noTableConstraints})
            Just existing -> Left (InvalidEdit edit (TableAlreadyExist tName existing))
        )
        tName
        (schemaTables s)
    pure $ s {schemaTables = tables'}
  TableRemoved tName ->
    withExistingTable tName edit s (removeTable edit s tName)
  PrimaryKeyAdded tName con conOpt ->
    withExistingTable tName edit s (addTableConstraint edit s (SomePrimaryKey con conOpt) tName)
  UniqueConstraintAdded tName con conOpt ->
    withExistingTable tName edit s (addTableConstraint edit s (SomeUnique con conOpt) tName)
  ForeignKeyAdded tName con conOpt ->
    withExistingTable tName edit s (addTableConstraint edit s (SomeForeignKey con conOpt) tName)

  TableConstraintRemoved tName con ->
    withExistingTable tName edit s (removeTableConstraint edit s con tName)
  RenameConstraint tName oldName newName ->
    withExistingTable tName edit s (renameTableConstraint edit s oldName newName tName)
  ColumnAdded tName colName col ->
    withExistingTable tName edit s (addColumn edit colName col)
  ColumnRemoved tName colName ->
    withExistingTable tName edit s (removeColumn edit s colName tName)
  ColumnTypeChanged tName colName oldType newType ->
    withExistingColumn tName colName edit s (\_ -> changeColumnType edit colName oldType newType)
  ColumnNullableChanged tName colName cCon ->
    withExistingColumn tName colName edit s (\_ -> changeColumnNullable cCon)
  ColumnDefaultChanged tName colName cCon ->
    withExistingColumn tName colName edit s (\_ -> changeColumnDefault cCon)
  EnumTypeAdded eName enum -> liftEither $ do
    enums' <-
      M.alterF
        ( \case
            Nothing -> Right (Just enum)
            Just existing -> Left (InvalidEdit edit (EnumAlreadyExist eName existing))
        )
        eName
        (schemaEnumerations s)
    pure $ s {schemaEnumerations = enums'}
  EnumTypeRemoved eName ->
    withExistingEnum eName edit s (removeEnum edit s eName)
  EnumTypeValueAdded eName addedValue insOrder insPoint ->
    withExistingEnum eName edit s (addValueToEnum edit eName addedValue insOrder insPoint)
  SequenceAdded sName seqq -> liftEither $ do
    seqs' <-
      M.alterF
        ( \case
            Nothing -> Right (Just seqq)
            Just existing -> Left (InvalidEdit edit (SequenceAlreadyExist sName existing))
        )
        sName
        (schemaSequences s)
    pure $ s {schemaSequences = seqs'}
  SequenceRemoved sName ->
    withExistingSequence sName edit s (removeSequence edit s sName)

  SequenceRenamed oldName newName -> flip execStateT s $ do
    -- delete the old sequence, returning its owner
    oldSeq <- _schemaSequences . at oldName <<.= Nothing
    -- verify the sequence actually existed.
    oldOwner <- case oldSeq of
      Nothing -> throwError (InvalidEdit edit (SequenceDoesntExist oldName))
      Just o -> pure o
    -- create the new sequence.
    _schemaSequences . at newName .= oldSeq
    -- update the default on the owning column
    forM_ oldOwner $ \(Sequence tName cName) -> do
      -- overwrite the old constraint with the new name or have a bad time.
      First oldDflt <- _schemaTables . ix tName . _tableColumns . ix cName . _columnConstraints . _columnDefault . _Unwrapped
        <<.= First (Just $ Autoincrement $ Just newName)

      case oldDflt of
        Just (Autoincrement (Just oldName'))
          | oldName == oldName' -> pure () -- everything's fine.
        _ -> throwError $ InvalidEdit edit $ ColumnDoesNotDefaultToOwnedSequence tName cName oldName


  SequenceSetOwner sName newOwner -> flip execStateT s $ do
    -- verify that the new owner exists.
    -- we don't want to actually *create* the default constraint; that's a different edit.
    case newOwner of
      Nothing -> pure ()
      Just (Sequence tName cName) -> do
        existingCol <- preuse (_schemaTables . ix tName . _tableColumns . ix cName)
        forM_ (preview _Nothing existingCol) $ \() ->
          throwError (InvalidEdit edit $ ColumnDoesntExist $ Qualified tName cName)
    -- update the sequence with a new owner
    oldSeq <- _schemaSequences . at sName <<.= Just newOwner
    case oldSeq of
      Nothing -> throwError (InvalidEdit edit (SequenceDoesntExist sName))
      Just _ -> pure ()

--
-- Various combinators for specific parts of a Schema
--

removeTable :: Edit -> Schema -> TableName -> Table -> Either ApplyFailed (Maybe Table)
removeTable e s tName t = runExcept . withExcept (toApplyFailed e) . liftEither $ do
  validateRemoveTable s tName t
  pure Nothing

addColumn :: Edit -> ColumnName -> Column -> Table -> Either ApplyFailed (Maybe Table)
addColumn e colName col tbl = liftEither $ do
  columns' <-
    M.alterF
      ( \case
          Nothing -> Right (Just col)
          Just existing -> Left (InvalidEdit e (ColumnAlreadyExist colName existing))
      )
      colName
      (tableColumns tbl)
  pure $ Just tbl {tableColumns = columns'}

removeColumn :: Edit -> Schema -> ColumnName -> TableName -> Table -> Either ApplyFailed (Maybe Table)
removeColumn e s colName tName tbl = liftEither $ do
  columns' <-
    M.alterF
      ( \case
          Nothing -> Left (InvalidEdit e (ColumnDoesntExist $ Qualified tName colName))
          Just _ -> first (toApplyFailed e) (validateRemoveColumn s tName colName) >> pure Nothing
      )
      colName
      (tableColumns tbl)
  pure $ Just tbl {tableColumns = columns'}

changeColumnType ::
  Edit ->
  ColumnName ->
  -- | old type
  ColumnType ->
  -- | new type
  ColumnType ->
  Column ->
  Either ApplyFailed (Maybe Column)
changeColumnType e colName oldType newType col =
  if columnType col /= oldType
    then Left $ InvalidEdit e (ColumnTypeMismatch colName col oldType)
    else pure . Just $ col {columnType = newType}

changeColumnDefault ::
  Maybe DefaultConstraint ->
  Column ->
  Either ApplyFailed (Maybe Column)
changeColumnDefault newDflt col = pure . Just $
  set (_columnConstraints . _columnDefault) newDflt col

-- | Performs an action over an existing 'Table', failing if the 'Table' doesn't exist.
withExistingTable ::
  TableName ->
  Edit ->
  Schema ->
  (Table -> Either ApplyFailed (Maybe Table)) ->
  Except ApplyFailed Schema
withExistingTable tName e s action = liftEither $ do
  tables' <-
    M.alterF
      ( \case
          Nothing -> Left (InvalidEdit e (TableDoesntExist tName))
          Just table -> action table
      )
      tName
      (schemaTables s)
  pure $ s {schemaTables = tables'}

-- | Performs an action over an existing 'Column', failing if the 'Column' doesn't exist.
withExistingColumn ::
  TableName ->
  ColumnName ->
  Edit ->
  Schema ->
  (Table -> Column -> Either ApplyFailed (Maybe Column)) ->
  Except ApplyFailed Schema
withExistingColumn tName colName e s action =
  withExistingTable
    tName
    e
    s
    ( \tbl -> do
        columns' <-
          M.alterF
            ( \case
                Nothing -> Left (InvalidEdit e (ColumnDoesntExist $ Qualified tName colName))
                Just existing -> action tbl existing
            )
            colName
            (tableColumns tbl)
        pure $ Just tbl {tableColumns = columns'}
    )

-- | Performs an action over an existing 'Enum', failing if the 'Enum' doesn't exist.
withExistingEnum ::
  EnumerationName ->
  Edit ->
  Schema ->
  (Enumeration -> Either ApplyFailed (Maybe Enumeration)) ->
  Except ApplyFailed Schema
withExistingEnum eName e s action = liftEither $ do
  enums' <-
    M.alterF
      ( \case
          Nothing -> Left (InvalidEdit e (EnumDoesntExist eName))
          Just enum -> action enum
      )
      eName
      (schemaEnumerations s)
  pure $ s {schemaEnumerations = enums'}

-- | Performs an action over an existing 'Sequence', failing if the 'Sequence' doesn't exist.
withExistingSequence ::
  SequenceName ->
  Edit ->
  Schema ->
  (Maybe Sequence -> Either ApplyFailed (Maybe (Maybe Sequence))) ->
  Except ApplyFailed Schema
withExistingSequence sName e s action = liftEither $ do
  seqs' <-
    M.alterF
      ( \case
          Nothing -> Left (InvalidEdit e (SequenceDoesntExist sName))
          Just enum -> action enum
      )
      sName
      (schemaSequences s)
  pure $ s {schemaSequences = seqs'}

addTableConstraint ::
  Edit ->
  Schema ->
  SomeTableConstraint ->
  TableName ->
  Table ->
  Either ApplyFailed (Maybe Table)
addTableConstraint e s con tName tbl = liftEither $ do
  case someTableConstraintName con of
    Nothing -> pure ()
    Just conName -> do
      let (removedCons, _) = removeTableConstraintNamed conName (tableConstraints tbl)
      if not $ S.null removedCons
        then Left (InvalidEdit e (TableConstraintAlreadyExist tName removedCons))
        else pure ()

  let (conflicts, constraints') = replaceSomeTableConstraint con (tableConstraints tbl)

  case conflicts of
    Just con' -> Left (InvalidEdit e (TableConstraintAlreadyExist tName $ S.singleton con'))
    Nothing -> do
      addConstraint
      pure $ Just $ tbl {tableConstraints = constraints'}

  where
    addConstraint :: Either ApplyFailed ()
    addConstraint = runExcept . withExcept (toApplyFailed e) . liftEither $ do
      validateAddTableConstraint s tName tbl con

-- | remove a constraint by name, and return all constraints with the name that were removed
removeTableConstraintNamed :: ConstraintName -> TableConstraints -> (S.Set SomeTableConstraint, TableConstraints)
removeTableConstraintNamed cName (TableConstraints pk fk uq) = do
  pk' <- case pk of
    Nothing -> pure Nothing
    Just (pkCon, pkOpts) -> if uniqueConstraintName pkOpts == Just cName
      then (S.singleton $ SomePrimaryKey pkCon pkOpts, Nothing)
      else pure $ Just (pkCon, pkOpts)
  fk' <- forM (M.toList fk) $ \c@(fkCon, fkOpts) ->
    if foreignKeyConstraintName fkOpts == Just cName
      then (S.singleton $ SomeForeignKey fkCon fkOpts, Nothing)
      else pure $ Just c
  uq' <- forM (M.toList uq) $ \c@(uqCon, uqOpts) ->
    if uniqueConstraintName uqOpts == Just cName
      then (S.singleton $ SomeUnique uqCon uqOpts, Nothing)
      else pure $ Just c
  pure $ TableConstraints pk' (M.fromList $ catMaybes fk') (M.fromList $ catMaybes uq')

-- | overwrite a table constraint and return the semantically equivalent one, if it exists
replaceSomeTableConstraint :: SomeTableConstraint -> TableConstraints -> (Maybe SomeTableConstraint, TableConstraints)
replaceSomeTableConstraint constr constraints = case constr of
  SomePrimaryKey c co ->
    ( uncurry SomePrimaryKey <$> primaryKeyConstraint constraints
    , constraints {primaryKeyConstraint = Just (c, co)}
    )
  SomeUnique c co ->
    ( SomeUnique c <$> constraints ^. _uniqueConstraints . at c
    , set (_uniqueConstraints . at c) (Just co) constraints
    )
  SomeForeignKey c co ->
    ( SomeForeignKey c <$> constraints ^. _foreignKeyConstraints . at c
    , set (_foreignKeyConstraints . at c) (Just co) constraints
    )

renameTableConstraint ::
  Edit ->
  Schema ->
  ConstraintName ->
  ConstraintName ->
  TableName ->
  Table ->
  Either ApplyFailed (Maybe Table)
renameTableConstraint e s conName newConName tName tbl = liftEither $ do
  let constraints = tableConstraints tbl
  let (removedConstraints, constraints') = removeTableConstraintNamed conName constraints
  if not $ S.null removedConstraints --  con constraints
    then removeConstraint removedConstraints
    else Left (InvalidEdit e (TableConstraintDoesntExist tName conName))

  tbl' <- foldlM
    (\t c -> fmap join $ forM t $ addTableConstraint e s (set _someTableConstraintName (Just newConName) c) tName ) -- (addTableConstraint e s)
    (Just tbl {tableConstraints = constraints'})
    removedConstraints

  pure $ tbl'
  where
    removeConstraint :: S.Set SomeTableConstraint -> Either ApplyFailed ()
    removeConstraint cons = runExcept . withExcept (toApplyFailed e) . liftEither $ do
      forM_ cons $ validateRemoveTableConstraint s tName

removeTableConstraint ::
  Edit ->
  Schema ->
  ConstraintName ->
  TableName ->
  Table ->
  Either ApplyFailed (Maybe Table)
removeTableConstraint e s conName tName tbl = liftEither $ do
  let constraints = tableConstraints tbl
  let (removedConstraints, constraints') = removeTableConstraintNamed conName constraints
  if not $ S.null removedConstraints --  con constraints
    then removeConstraint removedConstraints
    else Left (InvalidEdit e (TableConstraintDoesntExist tName conName))

  pure $ Just tbl {tableConstraints = constraints'}
  where
    removeConstraint :: S.Set SomeTableConstraint -> Either ApplyFailed ()
    removeConstraint cons = runExcept . withExcept (toApplyFailed e) . liftEither $ do
      forM_ cons $ validateRemoveTableConstraint s tName

removeEnum ::
  Edit ->
  Schema ->
  EnumerationName ->
  Enumeration ->
  Either ApplyFailed (Maybe Enumeration)
removeEnum e s eName _ = runExcept . withExcept (toApplyFailed e) . liftEither $ do
  validateRemoveEnum s eName
  pure Nothing

addValueToEnum ::
  Edit ->
  EnumerationName ->
  -- | value to insert
  Text ->
  InsertionOrder ->
  -- | insertion point
  Text ->
  Enumeration ->
  Either ApplyFailed (Maybe Enumeration)
addValueToEnum e eName addedValue insOrder insPoint (Enumeration vals) =
  case insOrder of
    Before ->
      case L.elemIndex insPoint vals of
        Nothing -> Left (InvalidEdit e (EnumInsertionPointDoesntExist eName (Enumeration vals) insPoint))
        Just i | i == 0 -> pure . Just $ Enumeration (addedValue : vals)
        Just i ->
          let (hd, tl) = L.splitAt (i - 1) vals
           in pure . Just $ Enumeration (hd <> (addedValue : tl))
    After ->
      let (hd, tl) = L.break (insPoint ==) vals
       in pure . Just $ Enumeration (hd <> (addedValue : tl))

removeSequence ::
  Edit ->
  Schema ->
  SequenceName ->
  Maybe Sequence ->
  Either ApplyFailed (Maybe (Maybe Sequence))
removeSequence e s sName sqss = runExcept . withExcept (toApplyFailed e) . liftEither $ do
  validateRemoveSequence s sName sqss
  pure Nothing
