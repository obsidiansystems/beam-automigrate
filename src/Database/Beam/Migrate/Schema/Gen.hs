{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Database.Beam.Migrate.Schema.Gen
    ( genSchema
    ) where

import           Data.Foldable                  ( foldlM )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T

import           Database.Beam.Migrate.Types
import qualified Database.Beam.Backend.SQL.AST as AST

import           Test.QuickCheck

genName :: (Text -> a) -> Gen a
genName f = f . T.pack . getASCIIString <$> arbitrary

genTableName :: Gen TableName
genTableName = genName TableName

genColumnName :: Gen ColumnName
genColumnName = genName ColumnName

genTableConstraints :: Tables -> Gen (Set TableConstraint)
genTableConstraints _allTables = pure mempty

genColumn :: Columns -> Gen Column
genColumn _allColums = do
    constNum <- choose (0, 2)
    constrs <- vectorOf constNum (elements [NotNull, Default "10"])
    pure $ Column AST.DataTypeInteger (S.fromList constrs)

genColumns :: Gen Columns
genColumns = do
  colNum <- choose (1, 50)
  columnNames <- vectorOf colNum genColumnName
  foldlM (\acc cName -> flip (M.insert cName) acc <$> genColumn acc) mempty columnNames

-- | Generate a new 'Table' using the already existing tables to populate the constraints.
genTable :: Tables -> Gen Table
genTable currentTables = do
    Table <$> genTableConstraints currentTables <*> genColumns

genSchema :: Gen Schema
genSchema = sized $ \tableNum -> do
  tableNames <- vectorOf tableNum genTableName
  tbls <- foldlM (\acc tName -> flip (M.insert tName) acc <$> genTable acc) mempty tableNames
  pure $ Schema tbls
