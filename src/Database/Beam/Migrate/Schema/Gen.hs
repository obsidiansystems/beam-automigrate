{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.Migrate.Schema.Gen
    ( genSchema
    , genSimilarSchemas
    , SimilarSchemas(..)
    , shrinkSchema
    ) where

import           GHC.Generics
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Foldable                            ( foldlM )
import           Data.Set                                 ( Set )
import           Data.Functor.Identity
import           Data.Text                                ( Text )
import qualified Data.Set                                as S
import qualified Data.Map.Strict                         as M
import qualified Data.Text                               as T

import           Database.Beam.Migrate.Types
import qualified Database.Beam.Backend.SQL.AST           as AST

import           Test.QuickCheck

--
-- Arbitrary instances
--

instance Arbitrary Schema where
    arbitrary = genSchema
    shrink = shrinkSchema

newtype SimilarSchemas =
    SimilarSchemas { unSchemas :: (Schema, Schema) } deriving (Generic, Show)

instance Arbitrary SimilarSchemas where
    arbitrary = SimilarSchemas <$> genSimilarSchemas
    shrink    = genericShrink

--
-- Generators
--

genName :: (Text -> a) -> Gen a
genName f = f . T.pack <$> vectorOf 10 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'])

genTableName :: Gen TableName
genTableName = genName TableName

genColumnName :: Gen ColumnName
genColumnName = genName ColumnName

genUniqueConstraint :: Columns -> Gen (Set TableConstraint)
genUniqueConstraint allCols = do
  someCols        <- take 32 <$> listOf1 (elements $ M.keys allCols) -- indexes are capped to 32 colums.
  constraintName  <- runIdentity <$> genName Identity
  pure $ S.singleton $ Unique (constraintName <> "_unique") (S.fromList someCols)

-- Generate a PK constraint.
-- /nota bene/: we have to require each and every column that compose this PK to be 'NotNull'. This is
-- important because otherwise Postgres will assume so even though we didn't generate this constraint in
-- the first place, and our roundtrip tests will fail.
genPkConstraint :: Columns -> Gen (Set TableConstraint)
genPkConstraint allCols = do
  someCols        <- take 32 . filter notNull <$> listOf1 (elements $ M.toList allCols) -- indexes are capped to 32 colums.
  case someCols of
    [] -> pure mempty
    _  -> do
      constraintName  <- runIdentity <$> genName Identity
      pure $ S.singleton $ PrimaryKey (constraintName <> "_pk") (S.fromList $ map fst someCols)
  where
    notNull :: (ColumnName, Column) -> Bool
    notNull (_, col) = NotNull `S.member` columnConstraints col

genTableConstraints :: Tables -> Columns -> Gen (Set TableConstraint)
genTableConstraints _allOtherTables ourColums =
  frequency [(60, pure mempty)
            ,(30, genUniqueConstraint ourColums)
            ,(30, genPkConstraint ourColums)
            ,(15, mappend <$> genPkConstraint ourColums <*> genUniqueConstraint ourColums)
            ]

genColumn :: Columns -> Gen Column
genColumn _allColums = do
    constNum <- choose (0, 2)
    constrs <- vectorOf constNum (elements [NotNull, Default "10"])
    pure $ Column (SqlStdType AST.DataTypeInteger) (S.fromList constrs)

genColumns :: Gen Columns
genColumns = do
  colNum <- choose (1, 50)
  columnNames <- vectorOf colNum genColumnName
  foldlM (\acc cName -> flip (M.insert cName) acc <$> genColumn acc) mempty columnNames

-- | Generate a new 'Table' using the already existing tables to populate the constraints.
genTable :: Tables -> Gen Table
genTable currentTables = do
    cols <- genColumns
    Table <$> genTableConstraints currentTables cols <*> pure cols

genSchema :: Gen Schema
genSchema = sized $ \tableNum -> do
  tableNames <- vectorOf tableNum genTableName
  tbls <- foldlM (\acc tName -> flip (M.insert tName) acc <$> genTable acc) mempty tableNames
  pure $ Schema tbls mempty

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
  (initialSchema,) <$> fmap (flip Schema mempty) (similarTables (schemaTables initialSchema))


similarTables :: Tables -> Gen Tables
similarTables tbls = flip execStateT tbls $
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
similarTable tbl = flip execStateT tbl $
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
    -- If we drop or modify a column we need to delete all constraints referencing that column.
    DropColumn -> modify' (\st -> st { tableColumns     = M.delete cName (tableColumns st)
                                     , tableConstraints = deleteConstraintReferencing cName (tableConstraints st)
                                     })
    ModifyColumn -> do
        col' <- lift $ similarColumn col
        modify' (\st -> st { tableColumns = M.insert cName col' (tableColumns st)
                           , tableConstraints = deleteConstraintReferencing cName (tableConstraints st)
                           })
    LeaveColumnAlone -> pure ()


deleteConstraintReferencing :: ColumnName -> Set TableConstraint -> Set TableConstraint
deleteConstraintReferencing cName conss = S.filter (not . doesReference) conss
  where
    doesReference :: TableConstraint -> Bool
    doesReference = \case
      PrimaryKey _ refs -> S.member cName refs
      ForeignKey _ _ refs _ _ -> let ours = S.map snd refs in S.member cName ours
      Unique _ refs -> S.member cName refs


similarColumn :: Column -> Gen Column
similarColumn col = do
    editAction <- frequency [ (1, pure ChangeType)
                            , (1, pure ChangeConstraints)
                            , (15, pure NoChange)
                            ]
    case editAction of
      ChangeType -> pure $ col { columnType = SqlStdType AST.DataTypeBoolean
                               , columnConstraints  = S.singleton (Default "FALSE")
                               }
      ChangeConstraints -> do
        constNum <- choose (0, 2)
        constrs <- vectorOf constNum (elements [NotNull, Default "FALSE"])
        pure $ col { columnType = SqlStdType AST.DataTypeBoolean, columnConstraints = S.fromList constrs }
      NoChange -> pure col

--
-- Shrinking a Schema
--

shrinkSchema :: Schema -> [Schema]
shrinkSchema s =
  noSchema : concatMap shrinkTable (M.toList (schemaTables s))
  where
    shrinkTable :: (TableName, Table) -> [Schema]
    shrinkTable (tName, tbl) =
      s { schemaTables = M.delete tName (schemaTables s) } :
      concatMap (shrinkColumns tName tbl) (M.toList (tableColumns tbl))

    shrinkColumns :: TableName -> Table -> (ColumnName, Column) -> [Schema]
    shrinkColumns tName tbl (cName, _col) =
      let tbl' = tbl { tableColumns = M.delete cName (tableColumns tbl)
                     , tableConstraints = deleteConstraintReferencing cName (tableConstraints tbl)
                     }
      in [s { schemaTables = M.insert tName tbl' (schemaTables s) }]


