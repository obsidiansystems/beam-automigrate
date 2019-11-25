{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Database.Beam.Migrate.Diff
  ( Diffable(..)
  , Diff
  )
where

import           Data.Maybe
import           Control.Monad
import           Control.Exception              ( assert )
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Foldable                  ( foldlM )

import           Database.Beam.Migrate.Types

--
-- Simple typeclass to diff things
--

type Diff = Either DiffError [Edit]

-- NOTE(adn) Accumulate all the errors independently instead of short circuiting?
class Diffable a where
    diff :: a -> a -> Diff

-- | Computes the diff between two 'Schema's, either failing with a 'DiffError'
-- or returning the list of 'Edit's necessary to turn the first into the second.
-- FIXME(adn) Inefficient for now.
instance Diffable Schema where
  diff hsSchema dbSchema = diff (schemaTables hsSchema) (schemaTables dbSchema)

instance Diffable Tables where
  diff = diffTablesReferenceImplementation

instance Diffable (TableName, Table) where
  diff (tName, t1) (_, t2) = diffTableReferenceImplementation tName (t1, t2)

instance Diffable (ColumnName, Column) where
  diff (cName, c1) (_, c2) = diffColumnReferenceImplementation cName (c1, c2)

--
-- Reference implementation
--

-- | A slow but hopefully correct implementation of the diffing algorithm, for QuickCheck comparison with
-- more sophisticated ones
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
    go e (tpair1@(hsName, _), tpair2@(dbName, _)) = assert (hsName == dbName) $ do
      d <- diff tpair1 tpair2
      pure $ e <> d

diffTableReferenceImplementation :: TableName -> (Table, Table) -> Diff
diffTableReferenceImplementation tName (hsTable, dbTable) = do
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
  let colAdded   = map (uncurry (ColumnAdded tName)) (M.toList columnAdded)
  let colRemoved = map (ColumnRemoved tName) (M.keys columnRemoved)
  pure $ (join $ catMaybes [tblConstraintsAdded, tblConstraintsRemoved]) <> colAdded <> colRemoved <> diffs
  where
    go :: [Edit] -> ((ColumnName, Column), (ColumnName, Column)) -> Diff
    go e (cpair1@(hsName, _), cpair2@(dbName, _)) = assert (hsName == dbName) $ do
      d <- diff cpair1 cpair2
      pure $ e <> d

diffColumnReferenceImplementation :: ColumnName -> (Column, Column) -> Diff
diffColumnReferenceImplementation colName (hsColumn, dbColumn) = do
  let constraintsAdded   = S.difference (columnConstraints hsColumn) (columnConstraints dbColumn)
      constraintsRemoved = S.difference (columnConstraints dbColumn) (columnConstraints hsColumn)
  let colConstraintsAdded = do
        guard (not $ S.null constraintsAdded)
        pure $ map (ColumnConstraintAdded colName) (S.toList constraintsAdded)
  let colConstraintsRemoved = do
        guard (not $ S.null constraintsRemoved)
        pure $ map (ColumnConstraintRemoved colName) (S.toList constraintsRemoved)
  let typeChanged = do
        guard (columnType hsColumn /= columnType dbColumn)
        pure [ColumnTypeChanged colName (columnType hsColumn) (columnType dbColumn)]
  pure $ join $ catMaybes [colConstraintsAdded, colConstraintsRemoved, typeChanged]
