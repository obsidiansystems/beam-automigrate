{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.AutoMigrate.Schema.Gen
  ( genSchema,
    genSimilarSchemas,
    SimilarSchemas (..),
    shrinkSchema,
  )
where

import Data.Default.Class (def)
import Control.Lens (over)
import Data.Foldable
import Control.Monad
import Control.Monad.State.Strict
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Scientific (Scientific, scientific)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, LocalTime, TimeOfDay)
import Data.Word
import Database.Beam.AutoMigrate (HasColumnType, defaultColumnType, sqlSingleQuoted)
import Database.Beam.AutoMigrate.Annotated (pgDefaultConstraint)
import Database.Beam.AutoMigrate.Types
import Database.Beam.Backend.SQL (HasSqlValueSyntax, timestampType)
import qualified Database.Beam.Backend.SQL.AST as AST
import Database.Beam.Backend.SQL.Types (SqlSerial (..))
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import Database.Beam.Query (currentTimestamp_, val_)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()
import Text.Printf (printf)

--
-- Arbitrary instances
--

instance Arbitrary Schema where
  arbitrary = genSchema
  shrink = shrinkSchema

newtype SimilarSchemas = SimilarSchemas {unSchemas :: (Schema, Schema)}
  deriving (Generic, Show)

instance Arbitrary SimilarSchemas where
  arbitrary = SimilarSchemas <$> genSimilarSchemas
  shrink = genericShrink

--
-- Generators
--

genAlphaName :: Gen Text
genAlphaName = T.pack <$> vectorOf 10 (elements $ ['a' .. 'z'] ++ ['A' .. 'Z'])

genName :: (Text -> a) -> Gen a
genName f = f <$> genAlphaName

genTableName :: Gen TableName
genTableName = genName TableName

genColumnName :: Gen ColumnName
genColumnName = genName ColumnName

-- | Generates a \"UNIQUE\" constraint. Restricts the eligible columns to only the ones which has
-- \"standard\" SQL types, to avoid the complication of dealing with indexes. For example trying to use
-- a JSON column would have Postgres fail with an error like:
-- \"[..]data type json has no default operator class for access method btree[..]\"
genUniqueConstraint :: Columns -> Gen TableConstraints
genUniqueConstraint allCols = do
  someCols <- map fst . filter isStdType . take 32 <$> listOf1 (elements $ M.toList allCols) -- indexes are capped to 32 colums.
  case someCols of
    [] -> pure noTableConstraints
    _ -> do
      pure noTableConstraints
        { uniqueConstraints = M.singleton (Unique (S.fromList someCols)) def }

isStdType :: (ColumnName, Column) -> Bool
isStdType (_, columnType -> SqlStdType _) = True
isStdType _ = False

-- Generate a PK constraint.
-- /nota bene/: we have to require each and every column that compose this PK to be 'NotNull'. This is
-- important because otherwise Postgres will assume so even though we didn't generate this constraint in
-- the first place, and our roundtrip tests will fail.
-- Same consideration on the \"standard types\" applies as above (crf 'genUniqueConstraint').
genPkConstraint :: Columns -> Gen TableConstraints
genPkConstraint allCols = do
  someCols <- take 32 . filter (\x -> isStdType x && notNull x) <$> listOf1 (elements $ M.toList allCols) -- indexes are capped to 32 colums.
  case someCols of
    [] -> pure noTableConstraints
    _ -> do
      pure noTableConstraints
        { primaryKeyConstraint = Just ( PrimaryKey (S.fromList $ map fst someCols), def) }
  where
    notNull :: (ColumnName, Column) -> Bool
    notNull (_, col) = NotNull == columnNullable (columnConstraints col)

genTableConstraints :: Tables -> Columns -> Gen TableConstraints
genTableConstraints _allOtherTables ourColums =
  frequency
    [ (60, pure noTableConstraints),
      (30, genUniqueConstraint ourColums),
      (30, genPkConstraint ourColums),
      (15, (<>) <$> genPkConstraint ourColums <*> genUniqueConstraint ourColums)
    ]

-- Generate a 'ColumnType' alongside a possible default value.
genColumnType :: Gen Column
genColumnType =
  oneof
    [ genSqlStdType,
      genPgSpecificType
      -- See below why this is commented out. , _genDbEnumeration
    ]

-- | Rather than trying to generate __all__ the possible values, we restrict ourselves to only the types
-- we can conjure via the 'defaultColumnType' combinator at 'Database.Beam.AutoMigrate.Compat', and we piggyback
-- on 'beam-core' machinery in order to generate the default values.
genSqlStdType :: Gen Column
genSqlStdType =
  oneof
    [ genType arbitrary (Proxy @Int32),
      genType arbitrary (Proxy @Int16),
      genType arbitrary (Proxy @Int64),
      genType arbitrary (Proxy @Word16),
      genType arbitrary (Proxy @Word32),
      genType arbitrary (Proxy @Word64),
      genType genAlphaName (Proxy @Text),
      genBitStringType,
      -- Unfortunately subject to rounding errors if a truly arbitrary type is used.
      -- For example '1.0' is rendered '1.0' by Beam but as '1' by Postgres.
      genType (elements [-0.1, 3.5]) (Proxy @Double),
      -- Unfortunately subject to rounding errors if a truly arbitrary type is used.
      genType (pure (scientific (1 :: Integer) (1 :: Int))) (Proxy @Scientific),
      genType arbitrary (Proxy @Day),
      -- Unfortunately subject to rounding errors if a truly arbitrary type is used.
      genType (pure (read "01:00:07.979173" :: TimeOfDay)) (Proxy @TimeOfDay),
      genType arbitrary (Proxy @Bool),
      -- Unfortunately subject to rounding errors if a truly arbitrary type is used.
      genType (pure (read "1864-05-10 13:50:45.919197" :: LocalTime)) (Proxy @LocalTime),
      -- Explicitly test for the 'CURRENT_TIMESTAMP' case.
      pure $ uncurry Column $ (SqlStdType $ timestampType Nothing False, asDefault $ pgDefaultConstraint @LocalTime currentTimestamp_),
      genType (fmap SqlSerial arbitrary) (Proxy @(SqlSerial Int64))
    ]

-- | frequently useful helper to construct a ColumnConstraints from a default
asDefault :: DefaultConstraint -> ColumnConstraints
asDefault x = noColumnConstraints { columnDefault = Just x }

genType ::
  forall a.
  ( HasColumnType a,
    HasSqlValueSyntax Pg.PgValueSyntax a
  ) =>
  Gen a ->
  Proxy a ->
  Gen Column
genType gen Proxy =
  Column <$> pure (defaultColumnType (Proxy @a))
    <*> (gen <&> (\(x :: a) -> asDefault $ pgDefaultConstraint $ val_ x))

-- | From postgres' documentation:
-- \"Bit strings are strings of 1's and 0's. They can be used to store or visualize bit masks. There are
-- two SQL bit types: bit(n) and bit varying(n), where n is a positive integer.
-- bit type data must match the length n exactly; it is an error to attempt to store shorter or longer bit
-- strings. bit varying data is of variable length up to the maximum length n; longer strings will be rejected.
-- Writing bit without a length is equivalent to bit(1), while bit varying without a length specification
-- means unlimited length.\"
-- /NOTE(and)/: This was not generated using the 'defaultsTo_' combinator, because it's unclear how
-- \"Beam\" allows the construction and handling of a 'SqlBitString', considering that it's treated
-- internally as an integer.
genBitStringType :: Gen Column
genBitStringType = do
  varying <- arbitrary
  charPrec <- elements [1 :: Word, 2, 4, 6, 8, 16, 32, 64]
  string <- vectorOf (fromIntegral charPrec) (elements ['0', '1'])
  let txt = sqlSingleQuoted (T.pack string) <> "::bit(" <> (T.pack . show $ charPrec) <> ")"
  case varying of
    False ->
      pure $ uncurry Column
        ( SqlStdType $ AST.DataTypeBit False (Just charPrec),
          asDefault (DefaultExpr txt)
        )
    True ->
      pure $ uncurry Column
        ( SqlStdType $ AST.DataTypeBit True (Just 1),
          asDefault (DefaultExpr txt)
        )

genPgSpecificType :: Gen Column
genPgSpecificType =
  oneof
    [ genType (fmap Pg.PgJSON (arbitrary @Int)) (Proxy @(Pg.PgJSON Int)),
      genType (fmap Pg.PgJSONB (arbitrary @Int)) (Proxy @(Pg.PgJSONB Int))
      -- , genRangeType (Proxy @Pg.PgInt4Range)            (Proxy @Int32)
      -- , genRangeType (Proxy @Pg.PgInt8Range)            (Proxy @Int)
      -- , genRangeType (Proxy @Pg.PgInt8Range)            (Proxy @Int64)
      -- , genRangeType (Proxy @Pg.PgNumRange)             (Proxy @Int)
      -- , genRangeType (Proxy @Pg.PgNumRange)             (Proxy @Word64)
      -- , genRangeType (Proxy @Pg.PgRangeTs)   (Proxy @LocalTime)
      -- , genRangeType (Proxy @Pg.RangeDate)   (Proxy @Day)
      -- , PgEnumeration EnumerationName
    ]

_genRangeType ::
  forall a n.
  ( Ord a,
    Num a,
    Arbitrary a,
    Pg.PgIsRange n,
    HasColumnType (Pg.PgRange n a),
    HasSqlValueSyntax Pg.PgValueSyntax a
  ) =>
  Proxy n ->
  Proxy a ->
  Gen Column
_genRangeType Proxy Proxy = do
  let colType = defaultColumnType (Proxy @(Pg.PgRange n a))
  lowerBoundRange <- elements [Pg.Inclusive, Pg.Exclusive]
  upperBoundRange <- elements [Pg.Inclusive, Pg.Exclusive]
  mbLower <- arbitrary @(Maybe a)
  mbUpper <-
    arbitrary @(Maybe (Positive a)) <&> \u -> case liftM2 (,) u mbLower of
      Nothing -> u
      Just (ub, lb) -> Just $ Positive $ (getPositive ub) + lb + 1
  let dVal =
        pgDefaultConstraint $
          Pg.range_ @n @a lowerBoundRange upperBoundRange (val_ mbLower) (val_ (fmap getPositive mbUpper))
  pure $ Column colType (asDefault dVal)

--
--
-- UNUSED GENERATORS
--
-- These are generators I (adn) wrote in order to hit all the possible 'AST.DataType', but ultimately we
-- are bound to only the types the user can generate via 'defaultsTo_', so we are currently not using these
-- one. If in the future new instances for 'HasColumnType' gets added, these might be handy again.
--

-- NOTE(adn) We currently cannot use this generator because we have no information on the DB side to
-- reconstruct the fact this was an enumeration, so any roundtrip property involving a 'DBEnumeration' will
-- fail.
_genDbEnumeration :: Gen (ColumnType, Text)
_genDbEnumeration = do
  vals <- map sqlSingleQuoted <$> listOf1 genAlphaName
  dVal <- elements vals
  name <- genName EnumerationName
  pure (DbEnumeration name (Enumeration vals), dVal)

_defVal :: forall a. (Arbitrary a, Show a) => Proxy a -> Gen Text
_defVal Proxy = T.pack . show <$> (arbitrary :: Gen a)

-- Postgres has \"float(8)\" which is an alias for \"double precision\", and \"float(4)\" which is
-- an alias for \"real\".
_genFloatType :: Gen (AST.DataType, Text)
_genFloatType = do
  floatPrec <- elements [Nothing, Just 4, Just 8]
  defValue <- _defVal @Float Proxy
  pure (AST.DataTypeFloat floatPrec, defValue)

-- real == float(8), i.e. 4 bytes.
_genRealType :: Gen (AST.DataType, Text)
_genRealType = do
  v <- T.pack . printf "%.1f" <$> arbitrary @Float
  pure (AST.DataTypeReal, sqlSingleQuoted v <> "::real")

_genIntType :: Gen (AST.DataType, Text)
_genIntType = do
  v <- arbitrary @Int32
  pure $
    if v < 0
      then (AST.DataTypeBigInt, sqlSingleQuoted (T.pack . show $ v) <> "::integer")
      else (AST.DataTypeBigInt, T.pack . show $ v)

_genBigIntType :: Gen (AST.DataType, Text)
_genBigIntType = do
  v <- arbitrary @Integer
  pure $
    if v < 0
      then (AST.DataTypeBigInt, sqlSingleQuoted (T.pack . show $ v) <> "::integer")
      else (AST.DataTypeBigInt, T.pack . show $ v)

-- | We do not render all the decimal digits to not incur in any rounding error when converting back from
-- Postgres.
_genDoublePrecisionType :: Gen (AST.DataType, Text)
_genDoublePrecisionType = do
  v <- T.pack . printf "%.1f" <$> arbitrary @Double
  pure (AST.DataTypeDoublePrecision, sqlSingleQuoted v <> "::double precision")

_genNumericType :: (Maybe (Word, Maybe Word) -> AST.DataType) -> Text -> Gen (AST.DataType, Text)
_genNumericType f _cast = do
  numPrec <- choose (1 :: Word, 15)
  numScale <- choose (1 :: Word, numPrec)
  p <- elements [Nothing, Just (numPrec, Nothing), Just (numPrec, Just numScale)]
  let renderNum (a, b) = (T.pack . show $ a) <> "." <> (T.pack . show $ b)
  defaultValue <- case p of
    Nothing ->
      oneof
        [ _defVal @Int32 Proxy,
          fmap renderNum ((,) <$> choose (0 :: Word, 131072) <*> choose (0 :: Word, 16383))
        ]
    Just (_, Nothing) ->
      oneof
        [ _defVal @Int32 Proxy,
          fmap
            renderNum
            ( (,) <$> (choose (0 :: Word, 131072)) -- `suchThat` (\x -> length (show x) <= fromIntegral numPrec))
                <*> choose (0 :: Word, 16383)
            )
        ]
    Just (_, Just _) ->
      oneof
        [ _defVal @Int32 Proxy,
          fmap
            renderNum
            ( (,) <$> choose (0 :: Word, 131072) -- `suchThat` (\x -> length (show x) <= fromIntegral numPrec)
                <*> choose (0 :: Word, 16383) -- `suchThat` (\x -> length (show x) <= fromIntegral numScale)
            )
        ]
  pure (f p, defaultValue)

-- Adjust the generator to the pg-specific caveats and quirks.
_pgSimplify :: (AST.DataType, Text) -> (AST.DataType, Text)
_pgSimplify = \case
  -- From the Postgres' documentation:
  -- \"character without length specifier is equivalent to character(1).\"
  (AST.DataTypeChar varying Nothing c, defValue) -> (AST.DataTypeChar varying (Just 1) c, defValue)
  -- Postgres doesn't distinguish between \"national character varying\" and \"character varying\".
  -- See <here https://stackoverflow.com/questions/57649798/postgresql-support-for-national-character-data-types>.
  (AST.DataTypeNationalChar varying Nothing, defValue) -> (AST.DataTypeChar varying (Just 1) Nothing, defValue)
  (AST.DataTypeNationalChar varying precision, defValue) -> (AST.DataTypeChar varying precision Nothing, defValue)
  -- In Postgres decimal and numeric are isomorphic.
  (AST.DataTypeDecimal v, defValue) -> (AST.DataTypeNumeric v, defValue)
  (AST.DataTypeFloat (Just 4), defValue) -> (AST.DataTypeReal, defValue)
  (AST.DataTypeFloat (Just 8), defValue) -> (AST.DataTypeDoublePrecision, defValue)
  x -> x

-- From the Postgres' documentation:
-- \"character without length specifier is equivalent to character(1).\"
_genCharType :: (Bool -> Maybe Word -> AST.DataType) -> Gen (AST.DataType, Text)
_genCharType f = do
  varying <- arbitrary
  text <- genAlphaName
  charPrec <- choose (1, 2048) -- 2048 is arbitrary (no pun intended) here.
  case varying of
    False -> pure (f False (Just charPrec), sqlSingleQuoted (T.take (fromIntegral charPrec) text) <> "::bpchar")
    True -> pure (f True (Just 1), sqlSingleQuoted text <> "::character varying")

genColumn :: Columns -> Gen Column
genColumn _allColums = do
  constNum <- choose (0, 2)
  Column cType dVal <- genColumnType
  constrs <- vectorOf constNum (elements [Left NotNull, Right $ columnDefault dVal])

  let mkConstr x = \case
        Left y -> x { columnNullable = y }
        Right y -> x { columnDefault = y }
  pure $ Column cType (foldl' mkConstr noColumnConstraints constrs)

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
  pure $ Schema tbls mempty mempty

--
-- Generating Schema(s) which are not too dissimilar.
--

data TablesEditAction
  = AddTable
  | DropTable
  | ModifyTable
  | LeaveTableAlone

data TableEditAction
  = AddColumn
  | DropColumn
  | ModifyColumn
  | LeaveColumnAlone

data ColumnEditAction
  = ChangeType
  | ChangeConstraints
  | NoChange

-- Generate two 'Schema' which are not completely different but rather have /some/ differences.
genSimilarSchemas :: Gen (Schema, Schema)
genSimilarSchemas = do
  initialSchema <- genSchema
  (initialSchema,) <$> fmap (\tbs -> Schema tbs mempty mempty) (similarTables (schemaTables initialSchema))

similarTables :: Tables -> Gen Tables
similarTables tbls = flip execStateT tbls $
  forM_ (M.toList tbls) $ \(tName, tbl) -> do
    tableEditAction <-
      lift $
        frequency
          [ (1, pure AddTable),
            (1, pure DropTable),
            (1, pure ModifyTable),
            (15, pure LeaveTableAlone)
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
    tableEditAction <-
      lift $
        frequency
          [ (1, pure AddColumn),
            (1, pure DropColumn),
            (1, pure ModifyColumn),
            (15, pure LeaveColumnAlone)
          ]
    case tableEditAction of
      AddColumn -> do
        s <- get
        newColumnName <- lift genColumnName
        newColumn <- lift $ genColumn (tableColumns s)
        modify' (\st -> st {tableColumns = M.insert newColumnName newColumn (tableColumns st)})
      -- If we drop or modify a column we need to delete all constraints referencing that column.
      DropColumn ->
        modify'
          ( \st ->
              st
                { tableColumns = M.delete cName (tableColumns st),
                  tableConstraints = deleteConstraintReferencing cName (tableConstraints st)
                }
          )
      ModifyColumn -> do
        col' <- lift $ similarColumn col
        modify'
          ( \st ->
              st
                { tableColumns = M.insert cName col' (tableColumns st),
                  tableConstraints = deleteConstraintReferencing cName (tableConstraints st)
                }
          )
      LeaveColumnAlone -> pure ()

deleteConstraintReferencing :: ColumnName -> TableConstraints -> TableConstraints
deleteConstraintReferencing cName
  = over _primaryKeyConstraint (>>= \c@(PrimaryKey refs, _) -> c <$ guard (not $ S.member cName refs))
  . over _foreignKeyConstraints (M.filterWithKey $ \(ForeignKey _ refs) _ -> let ours = S.map snd refs in not $ S.member cName ours)
  . over _uniqueConstraints (M.filterWithKey $ \(Unique refs) _ -> not $ S.member cName refs)

similarColumn :: Column -> Gen Column
similarColumn col = do
  editAction' <-
    frequency
      [ (15, pure ChangeType),
        (10, pure ChangeConstraints),
        (30, pure NoChange)
      ]
  case editAction' of
    ChangeType -> do
      Column newType newDef <- genColumnType
      let oldConstraints = columnConstraints col
      pure $
        col
          { columnType = newType,
            columnConstraints = ColumnConstraints (columnNullable oldConstraints) (columnDefault newDef)
          }
    ChangeConstraints -> do
      -- At the moment we cannot add a new default value as we don't have a meanigful way of
      -- generating it.
      let oldConstraints = columnConstraints col
      let newConstraints = case oldConstraints of
            ColumnConstraints Null Nothing -> noColumnConstraints {columnNullable = NotNull}
            ColumnConstraints NotNull Nothing -> noColumnConstraints
            ColumnConstraints _ (Just _) -> oldConstraints
      pure $ col {columnConstraints = newConstraints}
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
      s {schemaTables = M.delete tName (schemaTables s)} :
      concatMap (shrinkColumns tName tbl) (M.toList (tableColumns tbl))

    shrinkColumns :: TableName -> Table -> (ColumnName, Column) -> [Schema]
    shrinkColumns tName tbl (cName, _col) =
      let tbl' =
            tbl
              { tableColumns = M.delete cName (tableColumns tbl),
                tableConstraints = deleteConstraintReferencing cName (tableConstraints tbl)
              }
       in [s {schemaTables = M.insert tName tbl' (schemaTables s)}]
