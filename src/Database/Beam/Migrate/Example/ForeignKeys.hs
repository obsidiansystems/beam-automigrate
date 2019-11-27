{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
module Database.Beam.Migrate.Example.ForeignKeys where

import           Data.Text                      ( Text )

import           GHC.Generics

import           Database.Beam.Postgres
import           Database.Beam.Schema           ( Beamable
                                                , Columnar
                                                , Database
                                                , DatabaseSettings
                                                , defaultDbSettings
                                                , PrimaryKey
                                                , TableEntity
                                                )
import qualified Database.Beam.Schema          as Beam
import           Database.Beam.Schema.Tables    ( primaryKey )

import           Database.Beam.Migrate          ( Schema
                                                , fromDbSettings
                                                )


--
-- Example
--


data CitiesT f = Flower
  { ctCity     :: Columnar f Text
  , ctLocation :: Columnar f Text
  }
  deriving (Generic, Beamable)

data WeatherT f = Weather
  { wtId             :: Columnar f Int
  , wtCity           :: PrimaryKey CitiesT f
  , wtTempLo         :: Columnar f Int
  , wtTempHi         :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ForecastDB f = ForecastDB
  { dbCities   :: f (TableEntity CitiesT)
  , dbWeathers :: f (TableEntity WeatherT)
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

-- Modify the field names to be compliant with William Yao's format.
forecastDB :: DatabaseSettings Postgres ForecastDB
forecastDB = defaultDbSettings

hsSchema :: Schema
hsSchema = fromDbSettings forecastDB
