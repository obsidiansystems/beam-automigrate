{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}

module Database.Beam.AutoMigrate.Diff
  ( Diffable (..),
    Diff,
    Priority (..),
    WithPriority (..),

    -- * Reference implementation, for model-testing purposes
    diffColumnReferenceImplementation,
    diffTablesReferenceImplementation,
    diffTableReferenceImplementation,
    diffReferenceImplementation,

    -- * Hopefully-efficient implementation
    diffColumn,
    diffTables,
    diffTable,
    sortEdits,
    sortAutomaticEdits,

    module Database.Beam.AutoMigrate.Diff
  )
where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Applicative ((<|>))
import Control.Lens (preview, _Nothing, ifoldMap, ifor, at, (.=), ix, (<<.=))
import Control.Exception (assert)
import Data.DList (DList)
import qualified Data.DList as D
import Data.Foldable (foldlM, toList)
import Data.List ((\\))
import qualified Data.List as L
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as M
import Data.Function (on)
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import Data.Word (Word8)
import Database.Beam.AutoMigrate.Types

--
-- Simple typeclass to diff things
--

-- | Some notion of 'Priority'. The lower the value, the higher the priority.
newtype Priority = Priority Word8 deriving (Show, Eq, Ord)

newtype WithPriority a = WithPriority {unPriority :: (a, Priority)}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

withoutPriority :: WithPriority a -> a
withoutPriority (WithPriority (x, _)) = x

editPriority :: AutomaticEditAction -> Priority
editPriority = \case
  -- Operations that create tables, sequences or enums have top priority
  EnumTypeAdded {} -> Priority 0
  TableAdded {} -> Priority 1
  SequenceAdded {} -> Priority 2
  -- We cannot create a column if the relevant table (or enum type) is not there.
  ColumnAdded {} -> Priority 3
  ColumnDefaultChanged {} -> Priority 4
  ColumnNullableChanged {} -> Priority 4
  SequenceSetOwner {} -> Priority 5 -- set owner *after* creating the columns that should own it but before the default is created.
  -- Operations that set constraints or change the shape of a type have lower priority
  ColumnTypeChanged {} -> Priority 5
  EnumTypeValueAdded {} -> Priority 6
  -- foreign keys need to go last, as the referenced columns needs to be either UNIQUE or have PKs.
  UniqueConstraintAdded {} -> Priority 7
  TableConstraintRemoved _ _ TableConstraintRemovedType_ForeignKey -> Priority 8
  TableConstraintRemoved _ _ TableConstraintRemovedType_PrimaryKey -> Priority 9 -- we must remove the existing PK before creating a new one.
  PrimaryKeyAdded {} -> Priority 10
  ForeignKeyAdded {} -> Priority 11
  TableConstraintRemoved _ _ TableConstraintRemovedType_Unique -> Priority 12
  -- Destructive operations go last
  ColumnRemoved {} -> Priority 13
  TableRemoved {} -> Priority 14
  EnumTypeRemoved {} -> Priority 15
  SequenceRemoved {} -> Priority 16
  RenameConstraint {} -> Priority 17 -- constraint manipulations are generated from the database
  SequenceRenamed {} -> Priority 17

-- TODO: This needs to support adding conditional queries.
mkEdit :: AutomaticEditAction -> WithPriority Edit
mkEdit e = WithPriority (defMkEdit e, editPriority e)

-- | Sort edits according to their execution order, to make sure they don't reference
-- something which hasn't been created yet.
sortEdits :: [WithPriority Edit] -> [WithPriority Edit]
sortEdits = L.sortOn (snd . unPriority)

sortAutomaticEdits :: [AutomaticEditAction] -> [WithPriority Edit]
sortAutomaticEdits = sortEdits . fmap mkEdit

type DiffA t = Either DiffError (t AutomaticEditAction)

type Diff = DiffA []

diffSorted :: Diffable a => a -> a -> Either DiffError [WithPriority Edit]
diffSorted x y = sortAutomaticEdits <$> diff x y

-- NOTE(adn) Accumulate all the errors independently instead of short circuiting?
class Diffable a where
  diff :: a -> a -> Diff

-- | Computes the diff between two 'Schema's, either failing with a 'DiffError'
-- or returning the list of 'Edit's necessary to turn the first into the second.
instance Diffable Schema where
  diff hsSchema dbSchema = do
    tableDiffs <- diff (schemaTables hsSchema) (schemaTables dbSchema)
    enumDiffs <- diff (schemaEnumerations hsSchema) (schemaEnumerations dbSchema)

    let hsSequences = diffableSequences hsSchema

    let dbSequences = diffableSequences dbSchema

    sequenceDiffs <- diff hsSequences dbSequences
    pure $ tableDiffs <> enumDiffs <> sequenceDiffs

instance Diffable Tables where
  diff t1 = fmap D.toList . diffTables t1

instance Diffable Enumerations where
  diff e1 = fmap D.toList . diffEnums e1

instance Diffable DiffableSequences where
  diff s1 = fmap D.toList . diffSequences s1

--
-- Reference implementation
--

-- | create all of the constraints in a TableConstraints
addTableConstraints :: TableName -> TableConstraints -> [AutomaticEditAction]
addTableConstraints tName hsConstraints = concat
  [ toList $ fmap (uncurry $ PrimaryKeyAdded tName) (primaryKeyConstraint hsConstraints)
  , fmap (uncurry $ UniqueConstraintAdded tName) (M.toList $ uniqueConstraints hsConstraints)
  , fmap (uncurry $ ForeignKeyAdded tName) (M.toList $ foreignKeyConstraints hsConstraints)
  ]

-- | drop all of the constraints in a TableConstraints
dropTableConstraints :: TableName -> TableConstraints -> [AutomaticEditAction]
dropTableConstraints tName dbConstraints = concat
  [ toList $ do
    (_, co) <- primaryKeyConstraint dbConstraints --
    cName <- uniqueConstraintName co
    pure $ TableConstraintRemoved tName cName TableConstraintRemovedType_PrimaryKey
  , do
    (_, co) <- M.toList $ uniqueConstraints dbConstraints
    cName <- toList $ uniqueConstraintName co
    pure $ TableConstraintRemoved tName cName TableConstraintRemovedType_Unique
  , do
    (_, co) <- M.toList $ foreignKeyConstraints dbConstraints
    cName <- toList $ foreignKeyConstraintName co
    pure $ TableConstraintRemoved tName cName TableConstraintRemovedType_ForeignKey
  ]

-- | alter constraints on a table (including add/remove)
alterTableConstraints :: TableName -> TableConstraints -> TableConstraints -> [AutomaticEditAction]
alterTableConstraints tName hsConstraints dbConstraints =
  let
    primaryKeyDifference l r
      | fmap fst l == fmap fst r = Nothing
      | otherwise = l

    tableConstraintsDifference l r = TableConstraints
      { primaryKeyConstraint = primaryKeyDifference (primaryKeyConstraint l) (primaryKeyConstraint r)
      , uniqueConstraints = M.difference (uniqueConstraints l) (uniqueConstraints r)
      , foreignKeyConstraints = M.difference (foreignKeyConstraints l) (foreignKeyConstraints r)
      }
    newConstraints = tableConstraintsDifference hsConstraints dbConstraints
    oldConstraints = tableConstraintsDifference dbConstraints hsConstraints
  in addTableConstraints tName newConstraints
    <> dropTableConstraints tName oldConstraints
    <> (concat . toList) (liftM2 (alterUniqueConstraint tName)
      (snd <$> primaryKeyConstraint hsConstraints)
      (snd <$> primaryKeyConstraint dbConstraints))
    <> (concat . toList) (M.intersectionWith (alterUniqueConstraint tName)
      (uniqueConstraints hsConstraints)
      (uniqueConstraints dbConstraints))
    <> (concat . toList) (M.intersectionWithKey (alterForeignKeyConstraint tName)
      (foreignKeyConstraints hsConstraints)
      (foreignKeyConstraints dbConstraints))

renameConstraint :: TableName -> Maybe ConstraintName -> Maybe ConstraintName -> Maybe AutomaticEditAction
renameConstraint tName hsNames dbNames = do
  hsName <- hsNames
  dbName <- dbNames
  guard (hsName /= dbName)
  pure $ RenameConstraint tName dbName hsName


alterUniqueConstraint :: TableName -> UniqueConstraintOptions -> UniqueConstraintOptions -> [AutomaticEditAction]
alterUniqueConstraint tName hsOpts dbOpts = concat
  [ toList $ renameConstraint tName (uniqueConstraintName hsOpts) (uniqueConstraintName dbOpts)
  ]

alterForeignKeyConstraint :: TableName -> ForeignKey -> ForeignKeyConstraintOptions -> ForeignKeyConstraintOptions -> [AutomaticEditAction]
alterForeignKeyConstraint tName fkCon hsOpts dbOpts =
  let
    recreate = do
      let mkAction f = guard (f hsOpts /= f dbOpts) *> pure (f dbOpts)
          delAction = mkAction onDelete
          updAction = mkAction onUpdate
      _ <- delAction <|> updAction
      -- TODO: this should error out instead of give up if the edit actions disagree but we don't know the constarint name
      cName <- foreignKeyConstraintName hsOpts
      pure
        [ TableConstraintRemoved tName cName TableConstraintRemovedType_ForeignKey
        , ForeignKeyAdded tName fkCon hsOpts
        ]
    rename = renameConstraint tName (foreignKeyConstraintName hsOpts) (foreignKeyConstraintName dbOpts)
  in maybe [] id $ recreate <|> ((:[]) <$> rename)

diffReferenceImplementation :: Schema -> Schema -> Diff
diffReferenceImplementation hsSchema = diff (schemaTables hsSchema) . schemaTables

-- | A slow but hopefully correct implementation of the diffing algorithm, for QuickCheck comparison with
-- more sophisticated ones.
diffTablesReferenceImplementation :: Tables -> Tables -> Diff
diffTablesReferenceImplementation hsTables dbTables = do
  let tablesAdded = M.difference hsTables dbTables
      tablesRemoved = M.difference dbTables hsTables
      diffableTables = M.intersection hsTables dbTables
      diffableTables' = M.intersection dbTables hsTables
  whenBoth <- foldlM go mempty (zip (M.toList diffableTables) (M.toList diffableTables'))
  pure $ whenAdded tablesAdded <> whenRemoved tablesRemoved <> whenBoth
  where
    whenAdded :: Tables -> [AutomaticEditAction]
    whenAdded = concatMap (addEdit TableAdded (\k -> addTableConstraints k . tableConstraints)) . M.toList

    whenRemoved :: Tables -> [AutomaticEditAction]
    whenRemoved =
      concatMap (addEdit (\k _ -> TableRemoved k) (\k -> dropTableConstraints k . tableConstraints)) . M.toList

    go :: [AutomaticEditAction] -> ((TableName, Table), (TableName, Table)) -> Diff
    go e ((hsName, hsTable), (dbName, dbTable)) = assert (hsName == dbName) $ do
      d <- diffTableReferenceImplementation hsName hsTable dbTable
      pure $ e <> d

addEdit
  :: (k -> v -> AutomaticEditAction)
  -> (k -> v -> [AutomaticEditAction])
  -> (k, v)
  -> [AutomaticEditAction]
addEdit onValue onConstr (k, v) = onValue k v : onConstr k v

diffTableReferenceImplementation :: TableName -> Table -> Table -> Diff
diffTableReferenceImplementation tName hsTable dbTable = do
  let columnsAdded = M.difference (tableColumns hsTable) (tableColumns dbTable)
      columnsRemoved = M.difference (tableColumns dbTable) (tableColumns hsTable)
      diffableColumns = M.intersection (tableColumns hsTable) (tableColumns dbTable)
      diffableColumns' = M.intersection (tableColumns dbTable) (tableColumns hsTable)
  whenBoth <- foldlM go mempty (zip (M.toList diffableColumns) (M.toList diffableColumns'))
  let colsAdded = whenAdded columnsAdded
      colsRemoved = whenRemoved columnsRemoved
      constraints = alterTableConstraints tName (tableConstraints hsTable) (tableConstraints dbTable)
  pure $ constraints
      <> colsAdded
      <> colsRemoved
      <> whenBoth
  where
    go :: [AutomaticEditAction] -> ((ColumnName, Column), (ColumnName, Column)) -> Diff
    go e ((hsName, hsCol), (dbName, dbCol)) = assert (hsName == dbName) $ do
      d <- diffColumnReferenceImplementation tName hsName hsCol dbCol
      pure $ e <> d

    whenAdded :: Columns -> [AutomaticEditAction]
    whenAdded = fmap (\(cName, col) -> ColumnAdded tName cName col) . M.toList

    whenRemoved :: Columns -> [AutomaticEditAction]
    whenRemoved = fmap (ColumnRemoved tName) . M.keys

diffColumnReferenceImplementation
  :: (Applicative f, Monoid (f (AutomaticEditAction))) => TableName -> ColumnName -> Column -> Column -> DiffA f
diffColumnReferenceImplementation tName colName hsColumn dbColumn = execWriterT $ do
  unless (on (==) columnType hsColumn dbColumn) $
    tell $ pure $ ColumnTypeChanged tName colName (columnType dbColumn) (columnType hsColumn)

  unless (on (==) (columnNullable . columnConstraints) hsColumn dbColumn) $
    tell $ pure $ ColumnNullableChanged tName colName $ columnNullable $ columnConstraints hsColumn

  unless (on (==) (columnDefault . columnConstraints) hsColumn dbColumn) $
    -- as a special case, filter out the places where we "learned" the sequence name.
    case on (,) (columnDefault . columnConstraints) hsColumn dbColumn of
      (Just (Autoincrement Nothing), Just (Autoincrement _)) -> pure ()
      _ -> tell $ pure $ ColumnDefaultChanged tName colName $ columnDefault $ columnConstraints hsColumn
--
-- Actual implementation
--

--
-- Diffing enums together
--

diffEnums :: Enumerations -> Enumerations -> DiffA DList
diffEnums hsEnums dbEnums =
  M.foldl' D.append mempty <$> mergeA whenEnumsAdded whenEnumsRemoved whenBoth hsEnums dbEnums
  where
    whenEnumsAdded :: WhenMissing (Either DiffError) EnumerationName Enumeration (DList (AutomaticEditAction))
    whenEnumsAdded = traverseMissing (\k v -> Right . D.singleton $ EnumTypeAdded k v)

    whenEnumsRemoved :: WhenMissing (Either DiffError) EnumerationName Enumeration (DList (AutomaticEditAction))
    whenEnumsRemoved = traverseMissing (\k _ -> Right . D.singleton $ EnumTypeRemoved k)

    whenBoth :: WhenMatched (Either DiffError) EnumerationName Enumeration Enumeration (DList (AutomaticEditAction))
    whenBoth = zipWithAMatched diffEnumeration

diffEnumeration :: EnumerationName -> Enumeration -> Enumeration -> DiffA DList
diffEnumeration eName (Enumeration hsEnum) (Enumeration dbEnum) = do
  let valuesRemoved = dbEnum \\ hsEnum
  if L.null valuesRemoved then Right $ D.fromList (computeEnumEdit eName hsEnum dbEnum) else Left $ ValuesRemovedFromEnum eName valuesRemoved

computeEnumEdit :: EnumerationName -> [Text] -> [Text] -> [AutomaticEditAction]
computeEnumEdit _ [] [] = mempty
computeEnumEdit _ [] (_ : _) = mempty
computeEnumEdit eName (x : xs) [] = appendAfter eName xs x
computeEnumEdit eName (x : xs) [y] =
  if x == y
    then appendAfter eName xs y
    else EnumTypeValueAdded eName x Before y : computeEnumEdit eName xs [y]
computeEnumEdit eName (x : xs) (y : ys) =
  if x == y
    then computeEnumEdit eName xs ys
    else EnumTypeValueAdded eName x Before y : computeEnumEdit eName xs (y : ys)

appendAfter :: EnumerationName -> [Text] -> Text -> [AutomaticEditAction]
appendAfter _ [] _ = mempty
appendAfter eName [l] z = [EnumTypeValueAdded eName l After z]
appendAfter eName (l : ls) z = EnumTypeValueAdded eName l After z : appendAfter eName ls l

data DiffableSequences = DiffableSequences
  Sequences -- ^ explicitly named/optionally owned sequences.
  (M.Map Sequence (Maybe SequenceName)) -- ^ autoincrement inferred column defaults
  (M.Map TableName (S.Set ColumnName)) -- ^ all columns that happen to exist
  deriving (Eq, Show)

diffableSequences :: Schema -> DiffableSequences
diffableSequences = DiffableSequences
  <$> schemaSequences
  <*> ifoldMap (\tName -> ifoldMap (getAutoincrementColumns tName) . tableColumns) . schemaTables
  <*> fmap (M.keysSet . tableColumns) . schemaTables
  where
    getAutoincrementColumns :: TableName -> ColumnName -> Column -> M.Map Sequence (Maybe SequenceName)
    getAutoincrementColumns tName cName col = case columnDefault $ columnConstraints col of
      Just (Autoincrement seqName') -> M.singleton (Sequence tName cName) seqName'
      _ -> M.empty

-- | Diffing sequences together
--
-- we need to work out two kinds of sequence:
-- 1. sequences used as the default value on a column of some table (basically;
-- the outcome of a column of type SERIAL or BIGSERIAL.  Sequences of this form
-- don't normally have any particular name; although we need to know what it is
-- eventually in order to actually build the diff.
-- 2. sequences that aren't that.
--
-- It's really important that we don't drop and recreate sequences;  else the
-- numbering may get reset.  The way this will work is that we'll look at eve
-- as far migration goes, there's n things we need to manage:
-- in hsSeqs, unowned sequences should or shouldn't exist.  we never guess names for those
--
diffSequences :: DiffableSequences -> DiffableSequences -> DiffA DList
diffSequences
  (DiffableSequences hsSeqs hsCols _)
  (DiffableSequences dbSeqs dbCols dbColNames) = execWriterT $ do
    let -- organise the dbSeqs by their owner.
        dbSeqsInv = ifoldMap (\k v -> maybe M.empty (flip M.singleton k) v) dbSeqs
        (hsSeqs', hsCols') = ifor hsCols $ \s@(Sequence tName cName) sName ->
          let sName' = maybe (mkSequenceName tName cName) id $ sName <|> M.lookup s dbSeqsInv
          in if (isJust $ preview (ix tName . ix cName) dbColNames)
            then (M.singleton sName' $ Just s, Just sName')
            else (M.empty, sName)

    -- we only keep the sequences we want to rename.
    renames <- mergeA
      (traverseMaybeMissing $ \(Sequence tName cName) sName -> do -- new autoincrement columns
        forM_ sName $ tell . pure . ColumnDefaultChanged tName cName . Just . Autoincrement . Just
        pure Nothing
        )
      (traverseMaybeMissing $ \(Sequence tName cName) sName -> do -- old autoincrement columns
        forM_ sName $ \_ -> tell . pure $ ColumnDefaultChanged tName cName Nothing
        pure Nothing
        )
      (zipWithMaybeAMatched $ \_ ->
        let go hsSName dbSName
              | hsSName == dbSName = pure Nothing
              | otherwise = do
                tell $ pure $ SequenceRenamed dbSName hsSName
                pure (Just (S.singleton (dbSName, hsSName)))
        in \hsSName dbSName -> fmap join $ sequence $ liftM2 go hsSName dbSName)
      hsCols'
      dbCols

    let renames' = foldMap id renames

        -- the previous step handled the renames, update our db state to reflect that.
        dbSeqs' = flip execState dbSeqs $ forM_ renames' $ \(oldName, newName) -> do
          oldOwner <- at oldName <<.= Nothing
          at newName .= oldOwner

    _ <- mergeA
      (traverseMaybeMissing $ \sName seqOwner -> do
        tell $ pure $ SequenceAdded sName seqOwner
        pure Nothing
        ) -- sequence is new
      (traverseMaybeMissing $ \sName _ -> do
        tell $ pure $ SequenceRemoved sName
        pure Nothing
      ) -- sequence is old
      (zipWithMaybeAMatched $ \sName newOwner oldOwner -> do
        unless (oldOwner == newOwner) $
          tell $ pure $ SequenceSetOwner sName newOwner
        pure Nothing
      ) -- sequence preserved
      (M.union hsSeqs hsSeqs')
      dbSeqs'

    pure ()

--
-- Diffing tables together
--

diffTables :: Tables -> Tables -> DiffA DList
diffTables hsTables dbTables =
  M.foldl' D.append mempty <$> mergeA whenTablesAdded whenTablesRemoved whenBoth hsTables dbTables
  where
    whenTablesAdded :: WhenMissing (Either DiffError) TableName Table (DList AutomaticEditAction)
    whenTablesAdded =
      traverseMissing
        ( \k v -> do
            let created = TableAdded k v
                constraintsAdded = addTableConstraints k (tableConstraints v)
            pure $ D.fromList (created : constraintsAdded)
        )

    whenTablesRemoved :: WhenMissing (Either DiffError) TableName Table (DList AutomaticEditAction)
    whenTablesRemoved =
      traverseMissing
        ( \k v -> do
            let removed = TableRemoved k
                constraintsRemoved = dropTableConstraints k (tableConstraints v)
            pure $ D.fromList (removed : constraintsRemoved)
        )

    whenBoth :: WhenMatched (Either DiffError) TableName Table Table (DList AutomaticEditAction)
    whenBoth = zipWithAMatched diffTable

diffTable :: TableName -> Table -> Table -> DiffA DList
diffTable tName hsTable dbTable = do
  let tblConstraints = D.fromList $ alterTableConstraints tName (tableConstraints hsTable) (tableConstraints dbTable)
  diffs <-
    M.foldl' D.append mempty
      <$> mergeA whenColumnAdded whenColumnRemoved whenBoth (tableColumns hsTable) (tableColumns dbTable)
  pure $ tblConstraints <> diffs
  where
    whenColumnAdded :: WhenMissing (Either DiffError) ColumnName Column (DList AutomaticEditAction)
    whenColumnAdded =
      traverseMissing
        ( \k v -> do
            let added = ColumnAdded tName k v
            pure $ D.fromList (added : [])
        )

    whenColumnRemoved :: WhenMissing (Either DiffError) ColumnName Column (DList AutomaticEditAction)
    whenColumnRemoved =
      traverseMissing
        ( \k _ -> do
            let removed = ColumnRemoved tName k
            pure $ D.fromList (removed : [])
        )

    whenBoth :: WhenMatched (Either DiffError) ColumnName Column Column (DList AutomaticEditAction)
    whenBoth = zipWithAMatched (diffColumn tName)

diffColumn :: TableName -> ColumnName -> Column -> Column -> DiffA DList
diffColumn = diffColumnReferenceImplementation
