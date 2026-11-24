{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Database.Beam.AutoMigrate.Derivation
  ( tests,
    annotatedDb,
    RegionT,
    CityT,
    pendingAnnotated,
    dupTableDb,
    dupColumnsDb,
  )
where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Monoid (Endo (..))
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import Database.Beam.AutoMigrate
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema
  ( Beamable,
    Columnar,
    Database,
    DatabaseEntity,
    DatabaseSettings,
    Nullable,
    PrimaryKey,
    TableEntity,
    dbModification,
    defaultDbSettings,
    fieldNamed,
    modifyTableFields,
    setEntityName,
    tableModification,
    withDbModification,
  )
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables (EntityModification (..))
import GHC.Generics (Generic, Rep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

--
-- Fixtures
--

data RegionT f = RegionT
  { regionId :: Columnar f Int
  }
  deriving (Generic, Beamable)

instance Beam.Table RegionT where
  data PrimaryKey RegionT f = RegionKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = RegionKey . regionId

data CityT f = CityT
  { cityId :: Columnar f Int,
    cityRegion :: PrimaryKey RegionT f
  }
  deriving (Generic, Beamable)

instance Beam.Table CityT where
  data PrimaryKey CityT f = CityKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = CityKey . cityId

data WeatherT f = WeatherT
  { weatherId :: Columnar f Int,
    weatherCity :: PrimaryKey CityT f,
    weatherRegion :: PrimaryKey RegionT f
  }
  deriving (Generic, Beamable)

instance Beam.Table WeatherT where
  data PrimaryKey WeatherT f = WeatherKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = WeatherKey . weatherId

data ForecastDB f = ForecastDB
  { dbRegions :: f (TableEntity RegionT),
    dbCities :: f (TableEntity CityT),
    dbWeathers :: f (TableEntity WeatherT)
  }
  deriving (Generic, Database be)

annotatedDb :: AnnotatedDatabaseSettings Postgres ForecastDB
annotatedDb = defaultAnnotatedDbSettings defaultDbSettings

schemaWith ::
  forall anns.
  ( GSchema Postgres ForecastDB anns (Rep (AnnotatedDatabaseSettings Postgres ForecastDB)),
    FromAnnotated Postgres ForecastDB DatabaseEntity AnnotatedDatabaseEntity
  ) =>
  Proxy anns ->
  Either [ValidationFailed] Schema
schemaWith = fromAnnotatedDbSettings annotatedDb

deriveOrFail :: Either [ValidationFailed] Schema -> IO Schema
deriveOrFail = either (assertFailure . ("derivation failed: " <>) . show) pure

--
-- Helpers
--

foreignKeyTargets :: Schema -> TableName -> [TableName]
foreignKeyTargets schema tableName =
  sort . mapMaybe target . S.toList . tableConstraints $ table
  where
    table =
      case M.lookup tableName (schemaTables schema) of
        Just tbl -> tbl
        Nothing -> error $ "expected table " <> show tableName <> " in " <> show (M.keys (schemaTables schema))

    target (ForeignKey _ targetTable _ _ _) = Just targetTable
    target _ = Nothing

--
-- Tests
--

tests :: TestTree
tests =
  testGroup
    "Derivation"
    [ testGroup
        "UserDefinedFk annotations"
        [ testCase "no annotations infers every foreign key" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[]))
            foreignKeyTargets schema "cities" @?= ["regions"]
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotation matching the first entry suppresses only that table's outgoing foreign keys" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[ 'UserDefinedFk CityT, 'UserDefinedFk RegionT]))
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotation matching a middle entry suppresses only that table's outgoing foreign keys" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[ 'UserDefinedFk RegionT, 'UserDefinedFk CityT, 'UserDefinedFk RegionT]))
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotation matching the last entry suppresses only that table's outgoing foreign keys" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[ 'UserDefinedFk RegionT, 'UserDefinedFk RegionT, 'UserDefinedFk CityT]))
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "multiple annotations each suppress their own table's outgoing foreign keys" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[ 'UserDefinedFk CityT, 'UserDefinedFk WeatherT]))
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= [],
          testCase "an annotation for an unrelated table leaves every inferred foreign key intact" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[ 'UserDefinedFk RegionT]))
            foreignKeyTargets schema "cities" @?= ["regions"]
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotating a referenced table retains foreign keys pointing at it" $ do
            schema <- deriveOrFail (schemaWith (Proxy @'[ 'UserDefinedFk CityT]))
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"]
            foreignKeyTargets schema "cities" @?= []
        ],
      manualForeignKeys,
      pendingForeignKeyChecks,
      duplicateObjects
    ]

--
-- Check manual foreign key declarations.
--

data ManFooT f = ManFooRow
  { manFooA :: Columnar f Int,
    manFooZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ManBarT f = ManBarRow
  { manBarId :: Columnar f Int,
    manBarA :: Columnar f Int,
    manBarZ :: Columnar f Int,
    manBarA2 :: Columnar f Int,
    manBarZ2 :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ManPkBarT f = ManPkBarRow
  { manPkBarId :: Columnar f Int,
    manPkBarRef :: PrimaryKey ManFooT f
  }
  deriving (Generic, Beamable)

data ManNullableBarT f = ManNullableBarRow
  { manNullableBarId :: Columnar f Int,
    manNullableRef :: PrimaryKey ManFooT (Nullable f)
  }
  deriving (Generic, Beamable)

data ManOtherT f = ManOtherRow
  { manOtherId :: Columnar f Int,
    manOtherRef :: PrimaryKey ManFooT f
  }
  deriving (Generic, Beamable)

data ManDbT f = ManDb
  { manDbFoo :: f (TableEntity ManFooT),
    manDbBar :: f (TableEntity ManBarT),
    manDbPkBar :: f (TableEntity ManPkBarT),
    manDbNullableBar :: f (TableEntity ManNullableBarT),
    manDbOther :: f (TableEntity ManOtherT)
  }
  deriving (Generic, Database be)

instance Beam.Table ManFooT where
  data PrimaryKey ManFooT f = ManFooKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManFooKey <$> manFooA <*> manFooZ

instance Beam.Table ManBarT where
  data PrimaryKey ManBarT f = ManBarKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManBarKey . manBarId

instance Beam.Table ManPkBarT where
  data PrimaryKey ManPkBarT f = ManPkBarKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManPkBarKey . manPkBarId

instance Beam.Table ManNullableBarT where
  data PrimaryKey ManNullableBarT f = ManNullableBarKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManNullableBarKey . manNullableBarId

instance Beam.Table ManOtherT where
  data PrimaryKey ManOtherT f = ManOtherKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManOtherKey . manOtherId

manDb :: DatabaseSettings Postgres ManDbT
manDb =
  defaultDbSettings
    `withDbModification` dbModification
      { manDbFoo =
          setEntityName "man_foo"
            <> modifyTableFields
              tableModification
                { manFooA = fieldNamed "foo_a",
                  manFooZ = fieldNamed "foo_z"
                },
        manDbBar =
          setEntityName "man_bar"
            <> modifyTableFields
              tableModification
                { manBarA = fieldNamed "bar_a",
                  manBarZ = fieldNamed "bar_z",
                  manBarA2 = fieldNamed "bar_a2",
                  manBarZ2 = fieldNamed "bar_z2"
                },
        manDbPkBar =
          setEntityName "man_pk_bar"
            <> modifyTableFields
              tableModification
                { manPkBarRef =
                    ManFooKey (fieldNamed "pk_ref_z") (fieldNamed "pk_ref_a")
                },
        manDbNullableBar =
          setEntityName "man_nullable_bar"
            <> modifyTableFields
              tableModification
                { manNullableRef =
                    ManFooKey (fieldNamed "nullable_z") (fieldNamed "nullable_a")
                },
        manDbOther = setEntityName "man_other"
      }

manAnnotated :: AnnotatedDatabaseSettings Postgres ManDbT
manAnnotated =
  defaultAnnotatedDbSettings manDb
    `withDbModification` dbModification
      { manDbBar =
          foreignKeyOn
            (manDbFoo manDb)
            ((References manBarA manFooA) :| [References manBarZ manFooZ])
            Cascade
            Restrict
            <> foreignKeyOn
              (manDbFoo manDb)
              ((References manBarA2 manFooA) :| [References manBarZ2 manFooZ])
              Restrict
              Cascade,
        manDbPkBar =
          foreignKeyOnPk (manDbFoo manDb) manPkBarRef Restrict Cascade,
        manDbNullableBar =
          foreignKeyOnNullablePk (manDbFoo manDb) manNullableRef SetNull Restrict,
        manDbOther =
          foreignKeyOnPk (manDbFoo manDb) manOtherRef NoAction Cascade
      }

manualForeignKeys :: TestTree
manualForeignKeys =
  testGroup
    "manual foreign keys"
    [ testCase "multiple scalar pairs produce exact, distinct declarations" $ do
        schema <-
          deriveOrFail $
            fromAnnotatedDbSettings
              manAnnotated
              (Proxy @'[ 'UserDefinedFk ManBarT, 'UserDefinedFk ManPkBarT, 'UserDefinedFk ManNullableBarT, 'UserDefinedFk ManOtherT ])
        bar <- lookupTable schema "man_bar"
        let fks = foreignKeyConstraints bar
            pairsOf (ForeignKey _ _ pairs _ _) = pairs
            namesOf (ForeignKey name _ _ _ _) = name
            actionsOf (ForeignKey _ _ _ d u) = (d, u)
        length fks @?= 2
        assertBool
          ("unexpected scalar pairs: " <> show (map pairsOf fks))
          ( S.fromList (map pairsOf fks)
              == S.fromList
                [ (ColumnName "bar_a", ColumnName "foo_a")
                    :| [(ColumnName "bar_z", ColumnName "foo_z")],
                  (ColumnName "bar_a2", ColumnName "foo_a")
                    :| [(ColumnName "bar_z2", ColumnName "foo_z")]
                ]
          )
        assertBool "foreign key names must be distinct" (length (S.fromList (map namesOf fks)) == 2)
        assertBool
          ("unexpected actions: " <> show (map actionsOf fks))
          (S.fromList (map actionsOf fks) == S.fromList [(Cascade, Restrict), (Restrict, Cascade)]),
      testCase "whole-key and nullable helpers preserve structural order and actions" $ do
        schema <-
          deriveOrFail $
            fromAnnotatedDbSettings
              manAnnotated
              (Proxy @'[ 'UserDefinedFk ManBarT, 'UserDefinedFk ManPkBarT, 'UserDefinedFk ManNullableBarT, 'UserDefinedFk ManOtherT ])
        pkBar <- lookupTable schema "man_pk_bar"
        nullableBar <- lookupTable schema "man_nullable_bar"
        case foreignKeyConstraints pkBar of
          [ForeignKey _ target pairs Restrict Cascade] -> do
            target @?= TableName "man_foo"
            pairs
              @?= ( (ColumnName "pk_ref_a", ColumnName "foo_z")
                      :| [(ColumnName "pk_ref_z", ColumnName "foo_a")]
                  )
          bad -> assertFailure ("unexpected whole-key FK: " <> show bad)
        case foreignKeyConstraints nullableBar of
          [ForeignKey _ target pairs SetNull Restrict] -> do
            target @?= TableName "man_foo"
            pairs
              @?= ( (ColumnName "nullable_a", ColumnName "foo_z")
                      :| [(ColumnName "nullable_z", ColumnName "foo_a")]
                  )
          bad -> assertFailure ("unexpected nullable whole-key FK: " <> show bad),
      testCase "an explicit declaration overrides only the same inferred local set" $ do
        schema <- deriveOrFail (fromAnnotatedDbSettings overrideAnnotated (Proxy @'[]))
        src <- lookupTable schema "over_src"
        let fks = foreignKeyConstraints src
            actionsOf (ForeignKey _ _ _ d u) = (d, u)
        length fks @?= 2
        assertBool
          ("expected explicit actions plus the untouched inferred FK, got " <> show (map actionsOf fks))
          (S.fromList (map actionsOf fks) == S.fromList [(Cascade, Restrict), (NoAction, NoAction)]),
      testCase "a manual declaration may target a UNIQUE key that is not the primary key" $ do
        schema <- deriveOrFail (fromAnnotatedDbSettings uniqueAnnotated (Proxy @'[ 'UserDefinedFk UniqueRefT ]))
        ref <- lookupTable schema "unique_ref"
        case foreignKeyConstraints ref of
          [ForeignKey _ target pairs NoAction NoAction] -> do
            target @?= TableName "unique_holder"
            pairs @?= ((ColumnName "ref_value", ColumnName "pair_value") :| [])
          bad -> assertFailure ("unexpected unique-target FK: " <> show bad)
    ]

lookupTable :: Schema -> Text -> IO Table
lookupTable schema name =
  maybe
    (assertFailure ("missing derived table " <> show name <> " in " <> show (M.keys (schemaTables schema))))
    pure
    (M.lookup (TableName name) (schemaTables schema))

foreignKeyConstraints :: Table -> [TableConstraint]
foreignKeyConstraints = filter isFk . S.toList . tableConstraints
  where
    isFk ForeignKey {} = True
    isFk _ = False

--
-- Check that explicit declarations replace the corresponding inferred foreign keys.
--

data OverTargetT f = OverTargetRow {overTargetId :: Columnar f Int}
  deriving (Generic, Beamable)

data OverSrcT f = OverSrcRow
  { overSrcId :: Columnar f Int,
    overSrcA :: PrimaryKey OverTargetT f,
    overSrcB :: PrimaryKey OverTargetT f
  }
  deriving (Generic, Beamable)

data OverrideDbT f = OverrideDb
  { overrideTarget :: f (TableEntity OverTargetT),
    overrideSrc :: f (TableEntity OverSrcT)
  }
  deriving (Generic, Database be)

instance Beam.Table OverTargetT where
  data PrimaryKey OverTargetT f = OverTargetKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = OverTargetKey . overTargetId

instance Beam.Table OverSrcT where
  data PrimaryKey OverSrcT f = OverSrcKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = OverSrcKey . overSrcId

overrideDb :: DatabaseSettings Postgres OverrideDbT
overrideDb =
  defaultDbSettings
    `withDbModification` dbModification
      { overrideTarget = setEntityName "over_target",
        overrideSrc = setEntityName "over_src"
      }

overrideAnnotated :: AnnotatedDatabaseSettings Postgres OverrideDbT
overrideAnnotated =
  defaultAnnotatedDbSettings overrideDb
    `withDbModification` dbModification
      { overrideSrc =
          foreignKeyOnPk (overrideTarget overrideDb) overSrcA Cascade Restrict
      }

--
-- Check references to a UNIQUE key that is not the primary key.
--

data PairTargetT f = PairTargetRow {pairTargetId :: Columnar f Int}
  deriving (Generic, Beamable)

data UniqueHolderT f = UniqueHolderRow
  { uniqueHolderId :: Columnar f Int,
    uniqueHolderPair :: PrimaryKey PairTargetT f
  }
  deriving (Generic, Beamable)

data UniqueRefT f = UniqueRefRow
  { uniqueRefId :: Columnar f Int,
    uniqueRefPair :: Columnar f Int
  }
  deriving (Generic, Beamable)

data UniqueDbT f = UniqueDb
  { uniqueDbTarget :: f (TableEntity PairTargetT),
    uniqueDbHolder :: f (TableEntity UniqueHolderT),
    uniqueDbRef :: f (TableEntity UniqueRefT)
  }
  deriving (Generic, Database be)

instance Beam.Table PairTargetT where
  data PrimaryKey PairTargetT f = PairTargetKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = PairTargetKey . pairTargetId

instance Beam.Table UniqueHolderT where
  data PrimaryKey UniqueHolderT f = UniqueHolderKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = UniqueHolderKey . uniqueHolderId

instance Beam.Table UniqueRefT where
  data PrimaryKey UniqueRefT f = UniqueRefKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = UniqueRefKey . uniqueRefId

uniqueDb :: DatabaseSettings Postgres UniqueDbT
uniqueDb =
  defaultDbSettings
    `withDbModification` dbModification
      { uniqueDbTarget = setEntityName "pair_target",
        uniqueDbHolder =
          setEntityName "unique_holder"
            <> modifyTableFields
              tableModification
                { uniqueHolderPair = PairTargetKey (fieldNamed "pair_value")
                },
        uniqueDbRef =
          setEntityName "unique_ref"
            <> modifyTableFields
              tableModification
                { uniqueRefPair = fieldNamed "ref_value"
                }
      }

uniqueAnnotated :: AnnotatedDatabaseSettings Postgres UniqueDbT
uniqueAnnotated =
  defaultAnnotatedDbSettings uniqueDb
    `withDbModification` dbModification
      { uniqueDbHolder =
          uniqueConstraintOn [U uniqueHolderPair],
        uniqueDbRef =
          foreignKeyOn
            (uniqueDbHolder uniqueDb)
            ((References uniqueRefPair (\tbl -> case uniqueHolderPair tbl of PairTargetKey c -> c)) :| [])
            NoAction
            NoAction
      }

--
-- Check invalid declarations before constructing foreign keys.
--

data PendingT f = PendingRow {pendingId :: Columnar f Int}
  deriving (Generic, Beamable)

data PendingTargetT f = PendingTargetRow {pendingTargetId :: Columnar f Int}
  deriving (Generic, Beamable)

data PendingDbT f = PendingDb
  { pendingTable :: f (TableEntity PendingT),
    pendingTarget :: f (TableEntity PendingTargetT)
  }
  deriving (Generic, Database be)

instance Beam.Table PendingT where
  data PrimaryKey PendingT f = PendingKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = PendingKey . pendingId

instance Beam.Table PendingTargetT where
  data PrimaryKey PendingTargetT f = PendingTargetKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = PendingTargetKey . pendingTargetId

addPending ::
  [PendingForeignKey] ->
  EntityModification (AnnotatedDatabaseEntity Postgres db) Postgres (TableEntity tbl)
addPending pfks =
  EntityModification
    ( Endo
        ( \(AnnotatedDatabaseEntity desc entity) ->
            AnnotatedDatabaseEntity (desc {dbAnnotatedForeignKeys = pfks}) entity
        )
    )

pendingDb :: DatabaseSettings Postgres PendingDbT
pendingDb =
  defaultDbSettings
    `withDbModification` dbModification
      { pendingTable = setEntityName "pending",
        pendingTarget = setEntityName "pending_target"
      }

pendingAnnotated :: [PendingForeignKey] -> AnnotatedDatabaseSettings Postgres PendingDbT
pendingAnnotated pfks =
  defaultAnnotatedDbSettings pendingDb
    `withDbModification` dbModification {pendingTable = addPending pfks}

pendingForeignKeyChecks :: TestTree
pendingForeignKeyChecks =
  testGroup
    "raw pending foreign keys"
    [ testCase "an empty declaration is rejected" $
        expectValidationError
          (\case InvalidForeignKeyDeclaration local target (EmptyKey _ _) -> local == TableName "pending" && target == TableName "pending_target"; _ -> False)
          (fromAnnotatedDbSettings (pendingAnnotated [PendingForeignKey (TableName "pending_target") [] [] NoAction NoAction]) (Proxy @'[])),
      testCase "unequal local and remote arities are rejected" $
        expectValidationError
          ( \v -> case v of
              InvalidForeignKeyDeclaration local target (ForeignKeyArityMismatch l t n m) ->
                local == TableName "pending" && target == TableName "pending_target" && l == local && t == target && n == 2 && m == 1
              _ -> False
          )
          ( fromAnnotatedDbSettings
              (pendingAnnotated [PendingForeignKey (TableName "pending_target") [ColumnName "a", ColumnName "b"] [ColumnName "id"] NoAction NoAction])
              (Proxy @'[])
          ),
      testCase "repeated local or remote columns are rejected" $
        expectValidationError
          (\case InvalidForeignKeyDeclaration _ _ (DuplicateForeignKeyColumns _ _) -> True; _ -> False)
          ( fromAnnotatedDbSettings
              (pendingAnnotated [PendingForeignKey (TableName "pending_target") [ColumnName "a", ColumnName "a"] [ColumnName "x", ColumnName "y"] NoAction NoAction])
              (Proxy @'[])
          ),
      testCase "duplicate explicit declarations are rejected before Set insertion" $
        expectValidationError
          (\case InvalidTableConstraint _ (DuplicateConstraintName t _) -> t == TableName "pending"; _ -> False)
          ( fromAnnotatedDbSettings
              ( pendingAnnotated
                  [ PendingForeignKey (TableName "pending_target") [ColumnName "pending_id"] [ColumnName "pending_target_id"] NoAction NoAction,
                    PendingForeignKey (TableName "pending_target") [ColumnName "pending_id"] [ColumnName "pending_target_id"] NoAction NoAction
                  ]
              )
              (Proxy @'[])
          ),
      testCase "a well formed raw declaration resolves into a constraint" $ do
        schema <-
          deriveOrFail $
            fromAnnotatedDbSettings
              (pendingAnnotated [PendingForeignKey (TableName "pending_target") [ColumnName "id"] [ColumnName "target_id"] Cascade Restrict])
              (Proxy @'[])
        pending <- lookupTable schema "pending"
        case foreignKeyConstraints pending of
          [ForeignKey _ target pairs Cascade Restrict] -> do
            target @?= TableName "pending_target"
            pairs @?= ((ColumnName "id", ColumnName "target_id") :| [])
          bad -> assertFailure ("unexpected pending FK: " <> show bad)
    ]

expectValidationError :: (ValidationFailed -> Bool) -> Either [ValidationFailed] a -> IO ()
expectValidationError predicate result =
  case result of
    Left failures
      | any predicate failures -> pure ()
      | otherwise -> assertFailure ("no matching validation failure in " <> show failures)
    Right _ -> assertFailure "expected validation to fail"

--
-- Check that schema derivation rejects duplicate objects.
--

data DupTableAT f = DupTableARow {dupTableAId :: Columnar f Int}
  deriving (Generic, Beamable)

data DupTableBT f = DupTableBRow {dupTableBId :: Columnar f Int}
  deriving (Generic, Beamable)

data DupTableDbT f = DupTableDb
  { dupTableA :: f (TableEntity DupTableAT),
    dupTableB :: f (TableEntity DupTableBT)
  }
  deriving (Generic, Database be)

instance Beam.Table DupTableAT where
  data PrimaryKey DupTableAT f = DupTableAKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = DupTableAKey . dupTableAId

instance Beam.Table DupTableBT where
  data PrimaryKey DupTableBT f = DupTableBKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = DupTableBKey . dupTableBId

dupTableDb :: DatabaseSettings Postgres DupTableDbT
dupTableDb =
  defaultDbSettings
    `withDbModification` dbModification
      { dupTableA = setEntityName "duplicate_table",
        dupTableB = setEntityName "duplicate_table"
      }

data DupColumnsT f = DupColumnsRow
  { dupColumnA :: Columnar f (SqlSerial Int),
    dupColumnB :: Columnar f (SqlSerial Int)
  }
  deriving (Generic, Beamable)

data DupColumnsDbT f = DupColumnsDb {dupColumnsTable :: f (TableEntity DupColumnsT)}
  deriving (Generic, Database be)

instance Beam.Table DupColumnsT where
  data PrimaryKey DupColumnsT f = DupColumnsKey (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = DupColumnsKey . dupColumnA

dupColumnsDb :: DatabaseSettings Postgres DupColumnsDbT
dupColumnsDb =
  defaultDbSettings
    `withDbModification` dbModification
      { dupColumnsTable =
          setEntityName "dup_columns"
            <> modifyTableFields
              tableModification
                { dupColumnA = fieldNamed "same",
                  dupColumnB = fieldNamed "same"
                }
      }

duplicateObjects :: TestTree
duplicateObjects =
  testGroup
    "duplicate objects"
    [ testCase "two tables with the same physical name are rejected" $
        expectValidationError
          (\case InvalidTableName (TableName n) (DuplicateTableName _) -> n == "duplicate_table"; _ -> False)
          (fromAnnotatedDbSettings (defaultAnnotatedDbSettings dupTableDb) (Proxy @'[])),
      testCase "duplicate columns and their generated sequences are rejected" $
        expectValidationError
          (\case InvalidColumn _ (DuplicateColumnName _ (ColumnName c)) -> c == "same"; _ -> False)
          (fromAnnotatedDbSettings (defaultAnnotatedDbSettings dupColumnsDb) (Proxy @'[])),
      testCase "duplicate generated sequence names are reported as well" $
        expectValidationError
          (\case InvalidSequence (SequenceName n) (DuplicateSequenceName _) -> n == "dup_columns___same___seq"; _ -> False)
          (fromAnnotatedDbSettings (defaultAnnotatedDbSettings dupColumnsDb) (Proxy @'[]))
    ]
