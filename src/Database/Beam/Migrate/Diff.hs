{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Beam.Migrate.Diff
  ( Diffable(..)
  , Diff

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

import           Data.DList                               ( DList )
import qualified Data.DList                              as D
import           Data.Maybe
import           Data.Text                                ( Text )
import           Data.List                                ( foldl'
                                                          , (\\)
                                                          )
import qualified Data.List                               as L
import qualified Data.List.NonEmpty                      as NE
import           Data.List.NonEmpty                       ( NonEmpty )
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

type DiffA t = Either DiffError (t Edit)
type Diff = DiffA []

-- NOTE(adn) Accumulate all the errors independently instead of short circuiting?
class Diffable a where
    diff :: a -> a -> Diff

-- | Computes the diff between two 'Schema's, either failing with a 'DiffError'
-- or returning the list of 'Edit's necessary to turn the first into the second.
instance Diffable Schema where
  diff hsSchema dbSchema = diff (schemaTables hsSchema) (schemaTables dbSchema) <>
                           diff (schemaEnumerations hsSchema) (schemaEnumerations dbSchema)

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
  pure $ map (uncurry TableAdded) (M.toList tablesAdded) <> map TableRemoved (M.keys tableRemoved) <> diffs
  where
    go :: [Edit] -> ((TableName, Table), (TableName, Table)) -> Diff
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
        pure $ map (TableConstraintAdded tName) (S.toList constraintsAdded)
  let tblConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ map (TableConstraintRemoved tName) (S.toList constraintsRemoved)
  let cols         = M.toList columnAdded
  let colAdded     = map (uncurry (ColumnAdded tName)) cols
  let colRemoved = map (ColumnRemoved tName) (M.keys columnRemoved)
  pure $ (join $ catMaybes [tblConstraintsAdded, tblConstraintsRemoved]) 
      <> colAdded 
      <> colRemoved 
      <> diffs
  where
    go :: [Edit] -> ((ColumnName, Column), (ColumnName, Column)) -> Diff
    go e ((hsName, hsCol), (dbName, dbCol)) = assert (hsName == dbName) $ do
      d <- diffColumnReferenceImplementation tName hsName hsCol dbCol
      pure $ e <> d

diffColumnReferenceImplementation :: TableName -> ColumnName -> Column -> Column -> Diff
diffColumnReferenceImplementation tName colName hsColumn dbColumn = do
  let constraintsAdded   = S.difference (columnConstraints hsColumn) (columnConstraints dbColumn)
      constraintsRemoved = S.difference (columnConstraints dbColumn) (columnConstraints hsColumn)
  let colConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ map (ColumnConstraintAdded tName colName) (S.toList constraintsAdded)
  let colConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ map (ColumnConstraintRemoved tName colName) (S.toList constraintsRemoved)
  let typeChanged = do
        guard (columnType hsColumn /= columnType dbColumn)
        pure [ColumnTypeChanged tName colName (columnType dbColumn) (columnType hsColumn) ]
  pure $ join $ catMaybes [colConstraintsAdded, colConstraintsRemoved, typeChanged]

--
-- Actual implementation
--

diffEnums :: Enumerations -> Enumerations -> DiffA DList
diffEnums hsEnums dbEnums =
  M.foldl' D.append mempty <$> mergeA whenEnumsAdded whenEnumsRemoved whenBoth hsEnums dbEnums
  where
    whenEnumsAdded :: WhenMissing (Either DiffError) EnumerationName Enumeration (DList Edit)
    whenEnumsAdded = traverseMissing (\k v -> Right . D.singleton $ EnumTypeAdded k v)

    whenEnumsRemoved :: WhenMissing (Either DiffError) EnumerationName Enumeration (DList Edit)
    whenEnumsRemoved = traverseMissing (\k _ -> Right . D.singleton $ EnumTypeRemoved k)

    whenBoth :: WhenMatched (Either DiffError) EnumerationName Enumeration Enumeration (DList Edit)
    whenBoth          = zipWithAMatched (\k x -> diffEnumeration k x)

diffEnumeration :: EnumerationName -> Enumeration -> Enumeration -> DiffA DList
diffEnumeration eName (Enumeration (NE.toList -> hsEnum)) (Enumeration (NE.toList -> dbEnum)) = do
  let valuesRemoved = dbEnum \\ hsEnum

  case L.null valuesRemoved of
    False -> Left  $ ValuesRemovedFromEnum eName valuesRemoved
    True  -> Right $ D.fromList (computeEnumEdit hsEnum dbEnum)

computeEnumEdit :: [Text] -> [Text] -> [Edit]
computeEnumEdit []  []        = mempty
computeEnumEdit []  (_:_)     = mempty
computeEnumEdit (x:xs) []     = appendAfter xs x
computeEnumEdit (x:xs) [y]    = 
    if x == y then appendAfter xs y
              else (EnumTypeValueAdded (EnumerationName "todo") x Before y) : computeEnumEdit xs [y]
computeEnumEdit (x:xs) (y:ys) =
    if x == y then computeEnumEdit xs ys
              else (EnumTypeValueAdded (EnumerationName "todo") x Before y) : computeEnumEdit xs (y:ys)

appendAfter :: [Text] -> Text -> [Edit]
appendAfter []  _    = mempty    
appendAfter [l] z    = [EnumTypeValueAdded (EnumerationName "todo") l After z]
appendAfter (l:ls) z = EnumTypeValueAdded (EnumerationName "todo") l After z : appendAfter ls l



diffTables :: Tables -> Tables -> DiffA DList
diffTables hsTables dbTables =
  M.foldl' D.append mempty <$> mergeA whenTablesAdded whenTablesRemoved whenBoth hsTables dbTables
  where
    whenTablesAdded :: WhenMissing (Either DiffError) TableName Table (DList Edit)
    whenTablesAdded = traverseMissing (\k v -> Right . D.singleton $ TableAdded k v)

    whenTablesRemoved :: WhenMissing (Either DiffError) TableName Table (DList Edit)
    whenTablesRemoved = traverseMissing (\k _ -> Right . D.singleton $ TableRemoved k)

    whenBoth :: WhenMatched (Either DiffError) TableName Table Table (DList Edit)
    whenBoth          = zipWithAMatched (\k x -> diffTable k x)

diffTable :: TableName -> Table -> Table -> DiffA DList
diffTable tName hsTable dbTable = do
  let constraintsAdded   = S.difference (tableConstraints hsTable) (tableConstraints dbTable)
      constraintsRemoved = S.difference (tableConstraints dbTable) (tableConstraints hsTable)
      tblConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ D.map (TableConstraintAdded tName) (D.fromList . S.toList $ constraintsAdded)
      tblConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ D.map (TableConstraintRemoved tName) (D.fromList . S.toList $ constraintsRemoved)
  diffs <- M.foldl' D.append mempty <$>
      mergeA whenColumnAdded whenColumnRemoved whenBoth (tableColumns hsTable) (tableColumns dbTable)
  pure $ (foldl' D.append D.empty $ catMaybes [tblConstraintsAdded, tblConstraintsRemoved]) <> diffs
  where
    whenColumnAdded :: WhenMissing (Either DiffError) ColumnName Column (DList Edit)
    whenColumnAdded = traverseMissing (\k v -> Right . D.singleton $ ColumnAdded tName k v)

    whenColumnRemoved :: WhenMissing (Either DiffError) ColumnName Column (DList Edit)
    whenColumnRemoved = traverseMissing (\k _ -> Right . D.singleton $ ColumnRemoved tName k)

    whenBoth :: WhenMatched (Either DiffError) ColumnName Column Column (DList Edit)
    whenBoth          = zipWithAMatched (\k x -> diffColumn tName k x)

diffColumn :: TableName -> ColumnName -> Column -> Column -> DiffA DList
diffColumn tName colName hsColumn dbColumn = do
  let constraintsAdded   = S.difference (columnConstraints hsColumn) (columnConstraints dbColumn)
      constraintsRemoved = S.difference (columnConstraints dbColumn) (columnConstraints hsColumn)
  let colConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ D.map (ColumnConstraintAdded tName colName) (D.fromList . S.toList $ constraintsAdded)
  let colConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ D.map (ColumnConstraintRemoved tName colName) (D.fromList . S.toList $ constraintsRemoved)
  let typeChanged = do
        guard (columnType hsColumn /= columnType dbColumn)
        pure $ D.singleton (ColumnTypeChanged tName colName (columnType dbColumn) (columnType hsColumn))
  pure $ foldl' D.append D.empty $ catMaybes [colConstraintsAdded, colConstraintsRemoved, typeChanged]
