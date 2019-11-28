{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Database.Beam.Migrate.Schema.Gen
    ( genSchema
    , genSimilarSchemas
    ) where

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Foldable                  ( foldlM )
import           Data.Set                       ( Set )
import           Data.Text                      ( Text )
import qualified Data.Set                      as S
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T

import           Database.Beam.Migrate.Types
import qualified Database.Beam.Backend.SQL.AST as AST

import           Test.QuickCheck

genName :: (Text -> a) -> Gen a
genName f = f . T.pack <$> vectorOf 10 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'])

genTableName :: Gen TableName
genTableName = genName TableName

genColumnName :: Gen ColumnName
genColumnName = genName ColumnName

-- | TODO(adn) Generating a 'Unique' constraint is not hard, but at the same time when we /diff/ two
-- schemas, if we delete any column which is referenced as part of this constraint, we also need to drop
-- the constraint, and this is currently not something supported by the diff algorithm.
genUniqueConstraint :: Columns -> Gen (Set TableConstraint)
genUniqueConstraint allCols = do
  if length allCols > 3 
     then do
       --let cols = M.keys allCols
       --colNum <- choose (1, 2)
       --columns <- shuffle cols
       pure mempty -- $ S.singleton (Unique "test_unique" (S.fromList $ take colNum columns))
     else pure mempty 

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
genTable _currentTables = do
    cols <- genColumns
    Table <$> genUniqueConstraint cols <*> pure cols

genSchema :: Gen Schema
genSchema = sized $ \tableNum -> do
  tableNames <- vectorOf tableNum genTableName
  tbls <- foldlM (\acc tName -> flip (M.insert tName) acc <$> genTable acc) mempty tableNames
  pure $ Schema tbls

--
-- Generating Schema(s) which are not too dissimilar.
--

data TablesEditAction =
    AddTable
  | DropTable
  | ModifyTable
  | LeaveTableAlone

data TableEditAction =
    AddColumn
  | DropColumn
  | ModifyColumn
  | LeaveColumnAlone

data ColumnEditAction =
    ChangeType
  | ChangeConstraints
  | NoChange

-- Generate two 'Schema' which are not completely different but rather have /some/ differences.
genSimilarSchemas :: Gen (Schema, Schema)
genSimilarSchemas = do
  initialSchema <- genSchema
  (initialSchema,) <$> fmap Schema (similarTables (schemaTables initialSchema))


similarTables :: Tables -> Gen Tables
similarTables tbls = flip execStateT tbls $ do
  forM_ (M.toList tbls) $ \(tName, tbl) -> do
    tableEditAction <- lift $ frequency [ (1, pure AddTable)
                                        , (1, pure DropTable)
                                        , (1, pure ModifyTable)
                                        , (15, pure LeaveTableAlone)
                                        ]
    case tableEditAction of
      AddTable -> do
          s <- get
          newTableName <- lift genTableName
          newTable <- lift $ genTable s
          modify' (M.insert newTableName newTable)
      DropTable -> modify' (M.delete tName)
      ModifyTable -> do
          table' <- lift $ similarTable tbl
          modify' (M.insert tName table')
      LeaveTableAlone -> pure ()


similarTable :: Table -> Gen Table
similarTable tbl = flip execStateT tbl $ do
  forM_ (M.toList . tableColumns $ tbl) $ \(cName, col) -> do
    tableEditAction <- lift $ frequency [ (1, pure AddColumn)
                                        , (1, pure DropColumn)
                                        , (1, pure ModifyColumn)
                                        , (15, pure LeaveColumnAlone)
                                        ]
    case tableEditAction of
      AddColumn -> do
          s <- get
          newColumnName <- lift genColumnName
          newColumn <- lift $ genColumn (tableColumns s)
          modify' (\st -> st { tableColumns = M.insert newColumnName newColumn (tableColumns st) })
      DropColumn -> modify' (\st -> st { tableColumns = M.delete cName (tableColumns st) })
      ModifyColumn -> do
          col' <- lift $ similarColumn col
          modify' (\st -> st { tableColumns = M.insert cName col' (tableColumns st) })
      LeaveColumnAlone -> pure ()


similarColumn :: Column -> Gen Column
similarColumn col = do
    editAction <- frequency [ (1, pure ChangeType)
                            , (1, pure ChangeConstraints)
                            , (15, pure NoChange)
                            ]
    case editAction of
      ChangeType -> pure $ col { columnType = AST.DataTypeBoolean }
      ChangeConstraints -> do
        constNum <- choose (0, 2)
        constrs <- vectorOf constNum (elements [NotNull, Default "FALSE"])
        pure $ col { columnConstraints = (S.fromList constrs) }
      NoChange -> pure col
