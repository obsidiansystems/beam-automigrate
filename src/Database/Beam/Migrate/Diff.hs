{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Beam.Migrate.Diff
  ( Diffable(..)
  , Diff
  , Priority(..)
  , WithPriority(..)

  -- * Reference implementation, for model-testing purposes
  , diffColumnReferenceImplementation
  , diffTablesReferenceImplementation
  , diffTableReferenceImplementation
  , diffReferenceImplementation
  -- * Hopefully-efficient implementation
  , diffColumn
  , diffTables
  , diffTable

  , computeEnumEdit
  , appendAfter
  )
where

import           Data.Word                                ( Word8 )
import           Data.DList                               ( DList )
import qualified Data.DList                              as D
import           Data.Maybe
import           Data.Text                                ( Text )
import           Data.List                                ( foldl'
                                                          , (\\)
                                                          )
import qualified Data.List                               as L
import qualified Data.List.NonEmpty                      as NE
import           Control.Monad
import           Control.Exception                        ( assert )
import           Data.Map.Merge.Strict                    ( mergeA
                                                          , traverseMissing
                                                          , zipWithAMatched
                                                          , zipWithAMatched
                                                          , WhenMissing
                                                          , WhenMatched
                                                          )
import qualified Data.Map.Strict                         as M
import qualified Data.Set                                as S
import           Data.Foldable                            ( foldlM )

import           Database.Beam.Migrate.Types

--
-- Simple typeclass to diff things
--

-- | Some notion of 'Priority'. The lower the value, the higher the priority.
newtype Priority = Priority Word8 deriving (Show, Eq, Ord)

newtype WithPriority a = WithPriority { unPriority :: (a, Priority) } deriving (Show, Eq, Ord)

editPriority :: Edit -> Priority
editPriority = \case
  -- Operations that create tables or enums have top priority
  EnumTypeAdded{}           -> Priority 0
  TableAdded{}              -> Priority 1
  -- We cannot create a column if the relevant table (or enum type) is not there.
  ColumnAdded{}             -> Priority 2
  -- Operations that set constraints or change the shape of a type have lower priority
  ColumnTypeChanged{}       -> Priority 3
  EnumTypeRemoved{}         -> Priority 3
  EnumTypeValueAdded{}      -> Priority 3
  TableConstraintAdded{}    -> Priority 3
  TableConstraintRemoved{}  -> Priority 3
  ColumnConstraintAdded{}   -> Priority 3
  ColumnConstraintRemoved{} -> Priority 3
  -- Destructive operations go last
  ColumnRemoved{}           -> Priority 4
  TableRemoved{}            -> Priority 5

mkEdit :: Edit -> WithPriority Edit
mkEdit e = WithPriority (e, editPriority e)

type DiffA t = Either DiffError (t (WithPriority Edit))
type Diff = DiffA []

-- NOTE(adn) Accumulate all the errors independently instead of short circuiting?
class Diffable a where
    diff :: a -> a -> Diff

-- | Computes the diff between two 'Schema's, either failing with a 'DiffError'
-- or returning the list of 'Edit's necessary to turn the first into the second.
instance Diffable Schema where
  diff hsSchema dbSchema = do
      tableDiffs <- diff (schemaTables hsSchema) (schemaTables dbSchema)
      enumDiffs  <- diff (schemaEnumerations hsSchema) (schemaEnumerations dbSchema)
      pure $ tableDiffs <> enumDiffs

instance Diffable Tables where
  diff t1 = fmap D.toList . diffTables t1

instance Diffable Enumerations where
  diff t1 = fmap D.toList . diffEnums t1

--
-- Reference implementation
--

diffReferenceImplementation :: Schema -> Schema -> Diff
diffReferenceImplementation hsSchema = diff (schemaTables hsSchema) . schemaTables

-- | A slow but hopefully correct implementation of the diffing algorithm, for QuickCheck comparison with
-- more sophisticated ones.
diffTablesReferenceImplementation :: Tables -> Tables -> Diff
diffTablesReferenceImplementation hsTables dbTables = do
  let tablesAdded     = M.difference hsTables dbTables
      tableRemoved    = M.difference dbTables hsTables
      diffableTables  = M.intersection hsTables dbTables
      diffableTables' = M.intersection dbTables (hsTables)
  diffs <- foldlM go mempty (zip (M.toList diffableTables) (M.toList diffableTables'))
  pure $ map (mkEdit . uncurry TableAdded) (M.toList tablesAdded) 
      <> map (mkEdit . TableRemoved) (M.keys tableRemoved) <> diffs
  where
    go :: [WithPriority Edit] -> ((TableName, Table), (TableName, Table)) -> Diff
    go e ((hsName, hsTable), (dbName, dbTable)) = assert (hsName == dbName) $ do
      d <- diffTableReferenceImplementation hsName hsTable dbTable
      pure $ e <> d

diffTableReferenceImplementation :: TableName -> Table -> Table -> Diff
diffTableReferenceImplementation tName hsTable dbTable = do
  let constraintsAdded   = S.difference (tableConstraints hsTable) (tableConstraints dbTable)
      constraintsRemoved = S.difference (tableConstraints dbTable) (tableConstraints hsTable)
      columnAdded        = M.difference (tableColumns hsTable) (tableColumns dbTable)
      columnRemoved      = M.difference (tableColumns dbTable) (tableColumns hsTable)
      diffableColumns    = M.intersection (tableColumns hsTable) (tableColumns dbTable)
      diffableColumns'   = M.intersection (tableColumns dbTable) (tableColumns hsTable)
  diffs <- foldlM go mempty (zip (M.toList diffableColumns) (M.toList diffableColumns'))
  let tblConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ map (mkEdit . TableConstraintAdded tName) (S.toList constraintsAdded)
  let tblConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ map (mkEdit . TableConstraintRemoved tName) (S.toList constraintsRemoved)
  let cols         = M.toList columnAdded
  let colAdded     = map (mkEdit . uncurry (ColumnAdded tName)) cols
  let colRemoved = map (mkEdit . ColumnRemoved tName) (M.keys columnRemoved)
  pure $ (join $ catMaybes [tblConstraintsAdded, tblConstraintsRemoved]) 
      <> colAdded 
      <> colRemoved 
      <> diffs
  where
    go :: [WithPriority Edit] -> ((ColumnName, Column), (ColumnName, Column)) -> Diff
    go e ((hsName, hsCol), (dbName, dbCol)) = assert (hsName == dbName) $ do
      d <- diffColumnReferenceImplementation tName hsName hsCol dbCol
      pure $ e <> d

diffColumnReferenceImplementation :: TableName -> ColumnName -> Column -> Column -> Diff
diffColumnReferenceImplementation tName colName hsColumn dbColumn = do
  let constraintsAdded   = S.difference (columnConstraints hsColumn) (columnConstraints dbColumn)
      constraintsRemoved = S.difference (columnConstraints dbColumn) (columnConstraints hsColumn)
  let colConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ map (mkEdit . ColumnConstraintAdded tName colName) (S.toList constraintsAdded)
  let colConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ map (mkEdit . ColumnConstraintRemoved tName colName) (S.toList constraintsRemoved)
  let typeChanged = do
        guard (columnType hsColumn /= columnType dbColumn)
        pure [mkEdit $ ColumnTypeChanged tName colName (columnType dbColumn) (columnType hsColumn) ]
  pure $ join $ catMaybes [colConstraintsAdded, colConstraintsRemoved, typeChanged]

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
    whenEnumsAdded :: WhenMissing (Either DiffError) EnumerationName Enumeration (DList (WithPriority Edit))
    whenEnumsAdded = traverseMissing (\k v -> Right . D.singleton . mkEdit $ EnumTypeAdded k v)

    whenEnumsRemoved :: WhenMissing (Either DiffError) EnumerationName Enumeration (DList (WithPriority Edit))
    whenEnumsRemoved = traverseMissing (\k _ -> Right . D.singleton . mkEdit $ EnumTypeRemoved k)

    whenBoth :: WhenMatched (Either DiffError) EnumerationName Enumeration Enumeration (DList (WithPriority Edit))
    whenBoth          = zipWithAMatched (\k x -> diffEnumeration k x)

diffEnumeration :: EnumerationName -> Enumeration -> Enumeration -> DiffA DList
diffEnumeration eName (Enumeration (NE.toList -> hsEnum)) (Enumeration (NE.toList -> dbEnum)) = do
  let valuesRemoved = dbEnum \\ hsEnum
  case L.null valuesRemoved of
    False -> Left  $ ValuesRemovedFromEnum eName valuesRemoved
    True  -> Right $ D.fromList (computeEnumEdit eName hsEnum dbEnum)

computeEnumEdit :: EnumerationName -> [Text] -> [Text] -> [WithPriority Edit]
computeEnumEdit _ []  []         = mempty
computeEnumEdit _ []  (_:_)      = mempty
computeEnumEdit eName (x:xs) []  = appendAfter eName xs x
computeEnumEdit eName (x:xs) [y] = 
    if x == y then appendAfter eName xs y
              else (mkEdit $ EnumTypeValueAdded eName x Before y) : computeEnumEdit eName xs [y]
computeEnumEdit eName (x:xs) (y:ys) =
    if x == y then computeEnumEdit eName xs ys
              else (mkEdit $ EnumTypeValueAdded eName x Before y) : computeEnumEdit eName xs (y:ys)

appendAfter :: EnumerationName -> [Text] -> Text -> [WithPriority Edit]
appendAfter _ []  _        = mempty    
appendAfter eName [l] z    = [mkEdit $ EnumTypeValueAdded eName l After z]
appendAfter eName (l:ls) z = mkEdit (EnumTypeValueAdded eName l After z) : appendAfter eName ls l

--
-- Diffing tables together
--

diffTables :: Tables -> Tables -> DiffA DList
diffTables hsTables dbTables =
  M.foldl' D.append mempty <$> mergeA whenTablesAdded whenTablesRemoved whenBoth hsTables dbTables
  where
    whenTablesAdded :: WhenMissing (Either DiffError) TableName Table (DList (WithPriority Edit))
    whenTablesAdded = traverseMissing (\k v -> Right . D.singleton . mkEdit $ TableAdded k v)

    whenTablesRemoved :: WhenMissing (Either DiffError) TableName Table (DList (WithPriority Edit))
    whenTablesRemoved = traverseMissing (\k _ -> Right . D.singleton . mkEdit $ TableRemoved k)

    whenBoth :: WhenMatched (Either DiffError) TableName Table Table (DList (WithPriority Edit))
    whenBoth          = zipWithAMatched (\k x -> diffTable k x)

diffTable :: TableName -> Table -> Table -> DiffA DList
diffTable tName hsTable dbTable = do
  let constraintsAdded   = S.difference (tableConstraints hsTable) (tableConstraints dbTable)
      constraintsRemoved = S.difference (tableConstraints dbTable) (tableConstraints hsTable)
      tblConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ D.map (mkEdit . TableConstraintAdded tName) (D.fromList . S.toList $ constraintsAdded)
      tblConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ D.map (mkEdit . TableConstraintRemoved tName) (D.fromList . S.toList $ constraintsRemoved)
  diffs <- M.foldl' D.append mempty <$>
      mergeA whenColumnAdded whenColumnRemoved whenBoth (tableColumns hsTable) (tableColumns dbTable)
  pure $ (foldl' D.append D.empty $ catMaybes [tblConstraintsAdded, tblConstraintsRemoved]) <> diffs
  where
    whenColumnAdded :: WhenMissing (Either DiffError) ColumnName Column (DList (WithPriority Edit))
    whenColumnAdded = traverseMissing (\k v -> Right . D.singleton . mkEdit $ ColumnAdded tName k v)

    whenColumnRemoved :: WhenMissing (Either DiffError) ColumnName Column (DList (WithPriority Edit))
    whenColumnRemoved = traverseMissing (\k _ -> Right . D.singleton . mkEdit $ ColumnRemoved tName k)

    whenBoth :: WhenMatched (Either DiffError) ColumnName Column Column (DList (WithPriority Edit))
    whenBoth          = zipWithAMatched (\k x -> diffColumn tName k x)

diffColumn :: TableName -> ColumnName -> Column -> Column -> DiffA DList
diffColumn tName colName hsColumn dbColumn = do
  let constraintsAdded   = S.difference (columnConstraints hsColumn) (columnConstraints dbColumn)
      constraintsRemoved = S.difference (columnConstraints dbColumn) (columnConstraints hsColumn)
  let colConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ D.map (mkEdit . ColumnConstraintAdded tName colName) (D.fromList . S.toList $ constraintsAdded)
  let colConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ D.map (mkEdit . ColumnConstraintRemoved tName colName) (D.fromList . S.toList $ constraintsRemoved)
  let typeChanged = do
        guard (columnType hsColumn /= columnType dbColumn)
        pure $ D.singleton (mkEdit $ ColumnTypeChanged tName colName (columnType dbColumn) (columnType hsColumn))
  pure $ foldl' D.append D.empty $ catMaybes [colConstraintsAdded, colConstraintsRemoved, typeChanged]
