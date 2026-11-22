{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Database.Beam.AutoMigrate.Derivation (tests, annotatedDb, RegionT, CityT) where

import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Database.Beam.AutoMigrate
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema (Beamable, Columnar, Database, DatabaseEntity, PrimaryKey, TableEntity, defaultDbSettings)
import qualified Database.Beam.Schema as Beam
import GHC.Generics (Generic, Rep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
  Schema
schemaWith = fromAnnotatedDbSettings annotatedDb

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
            let schema = schemaWith (Proxy @'[])
            foreignKeyTargets schema "cities" @?= ["regions"]
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotation matching the first entry suppresses only that table's outgoing foreign keys" $ do
            let schema = schemaWith (Proxy @'[ 'UserDefinedFk CityT, 'UserDefinedFk RegionT])
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotation matching a middle entry suppresses only that table's outgoing foreign keys" $ do
            let schema = schemaWith (Proxy @'[ 'UserDefinedFk RegionT, 'UserDefinedFk CityT, 'UserDefinedFk RegionT])
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotation matching the last entry suppresses only that table's outgoing foreign keys" $ do
            let schema = schemaWith (Proxy @'[ 'UserDefinedFk RegionT, 'UserDefinedFk RegionT, 'UserDefinedFk CityT])
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "multiple annotations each suppress their own table's outgoing foreign keys" $ do
            let schema = schemaWith (Proxy @'[ 'UserDefinedFk CityT, 'UserDefinedFk WeatherT])
            foreignKeyTargets schema "cities" @?= []
            foreignKeyTargets schema "weathers" @?= [],
          testCase "an annotation for an unrelated table leaves every inferred foreign key intact" $ do
            let schema = schemaWith (Proxy @'[ 'UserDefinedFk RegionT])
            foreignKeyTargets schema "cities" @?= ["regions"]
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"],
          testCase "annotating a referenced table retains foreign keys pointing at it" $ do
            let schema = schemaWith (Proxy @'[ 'UserDefinedFk CityT])
            foreignKeyTargets schema "weathers" @?= ["cities", "regions"]
            foreignKeyTargets schema "cities" @?= []
        ]
    ]
