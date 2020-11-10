{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ForeignKeys where

import Control.Exception (bracket)
import Data.Proxy
import Data.Text (Text)
import Database.Beam.AutoMigrate (AnnotatedDatabaseSettings, Migration, Schema, defaultAnnotatedDbSettings, fromAnnotatedDbSettings, migrate, printMigration, unsafeRunMigration)
import Database.Beam.Postgres
import Database.Beam.Schema (Beamable, Columnar, Database, PrimaryKey, TableEntity, defaultDbSettings)
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables (primaryKey)
import qualified Database.PostgreSQL.Simple as Pg
import GHC.Generics

--
-- Example
--

data CitiesT f = Flower
  { ctCity :: Columnar f Text,
    ctLocation :: Columnar f Text
  }
  deriving (Generic, Beamable)

data WeatherT f = Weather
  { wtId :: Columnar f Int,
    wtCity :: PrimaryKey CitiesT f,
    wtTempLo :: Columnar f Int,
    wtTempHi :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ForecastDB f = ForecastDB
  { dbCities :: f (TableEntity CitiesT),
    dbWeathers :: f (TableEntity WeatherT)
  }
  deriving (Generic, Database be)

instance Beam.Table CitiesT where
  data PrimaryKey CitiesT f = CityID (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = CityID . ctCity

instance Beam.Table WeatherT where
  data PrimaryKey WeatherT f = WeatherID (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = WeatherID . wtId

-- This are 'AnnotatedDatabaseSettings' derived directly from "stock" 'DatabaseSettings'.
forecastDB :: AnnotatedDatabaseSettings Postgres ForecastDB
forecastDB = defaultAnnotatedDbSettings defaultDbSettings

hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings forecastDB (Proxy @'[])

exampleShowMigration :: IO ()
exampleShowMigration = withBeamTestDb printMigration

withBeamTestDb :: (Migration Pg -> Pg ()) -> IO ()
withBeamTestDb action = do
  let connInfo = "host=localhost port=5432 dbname=beam-test-forecast-db"
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $
      runBeamPostgres conn $ do
        let mig = migrate conn hsSchema
        action mig

exampleAutoMigration :: IO ()
exampleAutoMigration = withBeamTestDb unsafeRunMigration
