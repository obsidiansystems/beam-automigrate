{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Check that 'fromAnnotatedDbSettingsIndexed' returns the same result as
-- 'fromAnnotatedDbSettings', a 'Schema' or the same validation failures, on every database in this
-- repository that the generic path compiles, and that it reports a missing or ambiguous
-- foreign-key target when the result is evaluated.
module Test.Database.Beam.AutoMigrate.Indexed (indexedTests) where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
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
    DatabaseSettings,
    Nullable,
    PrimaryKey,
    TableEntity,
    dbModification,
    defaultDbSettings,
    withDbModification,
  )
import qualified Database.Beam.Schema as Beam
import qualified Example
import qualified ForeignKeys
import GHC.Generics (Generic)
import qualified Test.Database.Beam.AutoMigrate.Composite as Composite
import qualified Test.Database.Beam.AutoMigrate.Derivation as Derivation
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

indexedTests :: TestTree
indexedTests =
  testGroup
    "Indexed FK discovery"
    [ testGroup
        "same Schema as fromAnnotatedDbSettings"
        [ testCase "Derivation forecast database, no annotations" $
            sameSchema
              (fromAnnotatedDbSettings Derivation.annotatedDb (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed Derivation.annotatedDb (Proxy @'[])),
          testCase "Derivation forecast database, UserDefinedFk on the first and a repeated entry" $
            sameSchema
              (fromAnnotatedDbSettings Derivation.annotatedDb (Proxy @'[ 'UserDefinedFk Derivation.RegionT, 'UserDefinedFk Derivation.CityT, 'UserDefinedFk Derivation.RegionT]))
              (fromAnnotatedDbSettingsIndexed Derivation.annotatedDb (Proxy @'[ 'UserDefinedFk Derivation.RegionT, 'UserDefinedFk Derivation.CityT, 'UserDefinedFk Derivation.RegionT])),
          testCase "composite key paired by structure" $
            sameSchema
              (fromAnnotatedDbSettings (defaultAnnotatedDbSettings Composite.compositeDb) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (defaultAnnotatedDbSettings Composite.compositeDb) (Proxy @'[])),
          testCase "nullable reference to a key that embeds another key" $
            sameSchema
              (fromAnnotatedDbSettings (defaultAnnotatedDbSettings Composite.nestedDb) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (defaultAnnotatedDbSettings Composite.nestedDb) (Proxy @'[])),
          testCase "examples/ForeignKeys.hs" $
            sameSchema
              (fromAnnotatedDbSettings ForeignKeys.forecastDB (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed ForeignKeys.forecastDB (Proxy @'[])),
          testCase "examples/Example.hs: enums, serials, embedded records, foreignKeyOnPk, UserDefinedFk" $
            sameSchema
              Example.hsSchema
              (fromAnnotatedDbSettingsIndexed Example.annotatedDB (Proxy @'[ 'UserDefinedFk Example.LineItemT])),
          testCase "foreignKeyOnPk overriding a discovered key, no annotations" $
            sameSchema
              (fromAnnotatedDbSettings overridesDb (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[])),
          testCase "foreignKeyOnPk with discovery disabled for its table" $
            sameSchema
              (fromAnnotatedDbSettings overridesDb (Proxy @'[ 'UserDefinedFk TownT]))
              (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[ 'UserDefinedFk TownT])),
          testCase "discovery disabled for several tables" $
            sameSchema
              (fromAnnotatedDbSettings overridesDb (Proxy @'[ 'UserDefinedFk TripT, 'UserDefinedFk PersonT]))
              (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[ 'UserDefinedFk TripT, 'UserDefinedFk PersonT])),
          testCase "a well formed raw foreign key declaration" $
            sameSchema
              (fromAnnotatedDbSettings (Derivation.pendingAnnotated [wellFormedPending]) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (Derivation.pendingAnnotated [wellFormedPending]) (Proxy @'[]))
        ],
      testGroup
        "same validation failures as fromAnnotatedDbSettings"
        [ testCase "an empty explicit foreign key" $
            sameFailure
              (fromAnnotatedDbSettings (Derivation.pendingAnnotated [pending [] []]) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (Derivation.pendingAnnotated [pending [] []]) (Proxy @'[])),
          testCase "an explicit foreign key with unequal arities" $
            sameFailure
              (fromAnnotatedDbSettings (Derivation.pendingAnnotated [pending ["a", "b"] ["id"]]) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (Derivation.pendingAnnotated [pending ["a", "b"] ["id"]]) (Proxy @'[])),
          testCase "an explicit foreign key with repeated columns" $
            sameFailure
              (fromAnnotatedDbSettings (Derivation.pendingAnnotated [pending ["a", "a"] ["x", "y"]]) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (Derivation.pendingAnnotated [pending ["a", "a"] ["x", "y"]]) (Proxy @'[])),
          testCase "two explicit foreign keys with the same name" $
            sameFailure
              (fromAnnotatedDbSettings (Derivation.pendingAnnotated [duplicatePending, duplicatePending]) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (Derivation.pendingAnnotated [duplicatePending, duplicatePending]) (Proxy @'[])),
          testCase "two tables with the same name" $
            sameFailure
              (fromAnnotatedDbSettings (defaultAnnotatedDbSettings Derivation.dupTableDb) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (defaultAnnotatedDbSettings Derivation.dupTableDb) (Proxy @'[])),
          testCase "two columns and their sequences with the same name" $
            sameFailure
              (fromAnnotatedDbSettings (defaultAnnotatedDbSettings Derivation.dupColumnsDb) (Proxy @'[]))
              (fromAnnotatedDbSettingsIndexed (defaultAnnotatedDbSettings Derivation.dupColumnsDb) (Proxy @'[]))
        ],
      testGroup
        "the overrides fixture exercises what it claims"
        [ testCase "foreignKeyOnPk replaces the discovered key on the same column" $ do
            let towns = tableOf (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[])) "towns"
            S.map fkActions (foreignKeys towns) @?= S.fromList [(Cascade, Restrict)],
          testCase "UserDefinedFk keeps only the annotated foreign key" $ do
            let towns = tableOf (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[ 'UserDefinedFk TownT])) "towns"
            S.map fkActions (foreignKeys towns) @?= S.fromList [(Cascade, Restrict)],
          testCase "two references to one table give two foreign keys" $ do
            let trips = tableOf (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[])) "trips"
            S.size (foreignKeys trips) @?= 2,
          testCase "a self-reference gives a foreign key to its own table" $ do
            let people = tableOf (fromAnnotatedDbSettingsIndexed overridesDb (Proxy @'[])) "people"
            S.map fkTarget (foreignKeys people) @?= S.fromList [TableName "people", TableName "towns"]
        ],
      testGroup
        "errors raised when the Schema is evaluated"
        [ testCase "a foreign key to a table type the database repeats" $
            expectError "more than once" (fromAnnotatedDbSettingsIndexed Example.annotatedDB (Proxy @'[])),
          testCase "a foreign key to a table type the database lacks" $
            expectError "does not contain" (fromAnnotatedDbSettingsIndexed (defaultAnnotatedDbSettings missingDb) (Proxy @'[]))
        ]
    ]

-- | Both paths derive a 'Schema', and the two are equal. Compare table by table first, so that a
-- failure names the table that differs.
sameSchema :: Either [ValidationFailed] Schema -> Either [ValidationFailed] Schema -> Assertion
sameSchema (Right expected) (Right actual) = sameRightSchema expected actual
sameSchema expected actual =
  assertFailure ("expected two schemas, got\n" <> show expected <> "\nand\n" <> show actual)

sameRightSchema :: Schema -> Schema -> Assertion
sameRightSchema expected actual = do
  M.keys (schemaTables actual) @?= M.keys (schemaTables expected)
  sequence_
    [ assertBool ("table " <> show name <> " differs:\nexpected " <> show e <> "\nactual   " <> show a) (e == a)
      | (name, (e, a)) <- M.toList (M.intersectionWith (,) (schemaTables expected) (schemaTables actual))
    ]
  schemaEnumerations actual @?= schemaEnumerations expected
  schemaSequences actual @?= schemaSequences expected
  assertBool "schemas differ" (actual == expected)

-- | Both paths reject the database, with the same failures in the same order.
sameFailure :: Either [ValidationFailed] Schema -> Either [ValidationFailed] Schema -> Assertion
sameFailure expected actual = do
  assertBool ("expected validation to fail, got " <> show expected) (isLeft expected)
  actual @?= expected

expectError :: String -> Either [ValidationFailed] Schema -> Assertion
expectError needle result = do
  r <- try (evaluate (length (show result)))
  case r of
    Left (ErrorCall msg)
      | needle `isInfixOf` msg -> pure ()
      | otherwise -> assertFailure ("unexpected error: " <> msg)
    Right _ -> assertFailure "expected an error, but the result evaluated"

tableOf :: Either [ValidationFailed] Schema -> Text -> Table
tableOf (Left failures) _ = error ("validation failed: " <> show failures)
tableOf (Right schema) name =
  M.findWithDefault (error ("missing table " <> show name)) (TableName name) (schemaTables schema)

pending :: [Text] -> [Text] -> PendingForeignKey
pending locals remotes =
  PendingForeignKey (TableName "pending_target") (map ColumnName locals) (map ColumnName remotes) NoAction NoAction

wellFormedPending :: PendingForeignKey
wellFormedPending =
  PendingForeignKey (TableName "pending_target") [ColumnName "id"] [ColumnName "target_id"] Cascade Restrict

duplicatePending :: PendingForeignKey
duplicatePending = pending ["pending_id"] ["pending_target_id"]

foreignKeys :: Table -> S.Set TableConstraint
foreignKeys = S.filter isFk . tableConstraints
  where
    isFk ForeignKey {} = True
    isFk _ = False

fkActions :: TableConstraint -> (ReferenceAction, ReferenceAction)
fkActions (ForeignKey _ _ _ onDelete onUpdate) = (onDelete, onUpdate)
fkActions c = error ("not a foreign key: " <> show c)

fkTarget :: TableConstraint -> TableName
fkTarget (ForeignKey _ target _ _ _) = target
fkTarget c = error ("not a foreign key: " <> show c)

--
-- A database with foreignKeyOnPk overrides, a serial key referenced elsewhere, a self-reference,
-- two references to the same table, and a table type repeated but never referenced.
--

data CountyT f = County
  { countyId :: Columnar f (SqlSerial Int64),
    countyName :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Beam.Table CountyT where
  data PrimaryKey CountyT f = CountyKey (Columnar f (SqlSerial Int64))
    deriving (Generic, Beamable)
  primaryKey = CountyKey . countyId

data TownT f = Town
  { townId :: Columnar f Int64,
    townCounty :: PrimaryKey CountyT f
  }
  deriving (Generic, Beamable)

instance Beam.Table TownT where
  data PrimaryKey TownT f = TownKey (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = TownKey . townId

data PersonT f = Person
  { personId :: Columnar f Int64,
    personHome :: PrimaryKey TownT f,
    personManager :: PrimaryKey PersonT (Nullable f)
  }
  deriving (Generic, Beamable)

instance Beam.Table PersonT where
  data PrimaryKey PersonT f = PersonKey (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = PersonKey . personId

data TripT f = Trip
  { tripId :: Columnar f Int64,
    tripFrom :: PrimaryKey TownT f,
    tripTo :: PrimaryKey TownT f
  }
  deriving (Generic, Beamable)

instance Beam.Table TripT where
  data PrimaryKey TripT f = TripKey (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = TripKey . tripId

data AuditT f = Audit
  { auditId :: Columnar f Int64,
    auditNote :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Beam.Table AuditT where
  data PrimaryKey AuditT f = AuditKey (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = AuditKey . auditId

data OverridesDB f = OverridesDB
  { counties :: f (TableEntity CountyT),
    towns :: f (TableEntity TownT),
    people :: f (TableEntity PersonT),
    trips :: f (TableEntity TripT),
    auditsCurrent :: f (TableEntity AuditT),
    auditsArchive :: f (TableEntity AuditT)
  }
  deriving (Generic, Database be)

overridesSettings :: DatabaseSettings Postgres OverridesDB
overridesSettings = defaultDbSettings

overridesDb :: AnnotatedDatabaseSettings Postgres OverridesDB
overridesDb =
  defaultAnnotatedDbSettings overridesSettings
    `withDbModification` dbModification
      { towns = foreignKeyOnPk (counties overridesSettings) townCounty Cascade Restrict
      }

--
-- A database whose only table refers to a table type it does not contain. The generic path rejects
-- it at compile time ('LookupFailed'), so only the indexed path is run on it.
--

data MissingDB f = MissingDB
  { missingTowns :: f (TableEntity TownT)
  }
  deriving (Generic, Database be)

missingDb :: DatabaseSettings Postgres MissingDB
missingDb = defaultDbSettings
