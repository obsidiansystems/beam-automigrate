{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Checks on the foreign keys that 'foreignKeyOnPk' contributes to a 'Schema'.
module Test.Database.Beam.AutoMigrate.ForeignKeys (foreignKeyTests) where

import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import Database.Beam.AutoMigrate hiding (Table)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

data CityT f = City
  { cityName :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table CityT where
  data PrimaryKey CityT f = CityId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = CityId . cityName

data WeatherT f = Weather
  { weatherId :: Columnar f Int,
    weatherCity :: PrimaryKey CityT f
  }
  deriving (Generic, Beamable)

instance Table WeatherT where
  data PrimaryKey WeatherT f = WeatherId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = WeatherId . weatherId

data RouteT f = Route
  { routeId :: Columnar f Int,
    routeFrom :: PrimaryKey CityT f,
    routeTo :: PrimaryKey CityT f
  }
  deriving (Generic, Beamable)

instance Table RouteT where
  data PrimaryKey RouteT f = RouteId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = RouteId . routeId

data TravelDB f = TravelDB
  { dbCities :: f (TableEntity CityT),
    dbWeathers :: f (TableEntity WeatherT),
    dbRoutes :: f (TableEntity RouteT)
  }
  deriving (Generic, Database be)

travelDB :: DatabaseSettings be TravelDB
travelDB = defaultDbSettings

annotatedDB :: AnnotatedDatabaseSettings Postgres TravelDB
annotatedDB =
  defaultAnnotatedDbSettings travelDB
    `withDbModification` dbModification
      { dbWeathers = foreignKeyOnPk (dbCities travelDB) weatherCity Cascade Restrict,
        dbRoutes =
          foreignKeyOnPk (dbCities travelDB) routeFrom Cascade NoAction
            <> foreignKeyOnPk (dbCities travelDB) routeTo Restrict NoAction
      }

-- | FK discovery runs for every table, as in the README example.
hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings annotatedDB (Proxy @'[])

foreignKeysOf :: Text -> [TableConstraint]
foreignKeysOf t =
  [ c
    | Just tbl <- [M.lookup (TableName t) (schemaTables hsSchema)],
      c@ForeignKey {} <- S.toList (tableConstraints tbl)
  ]

foreignKeyTests :: TestTree
foreignKeyTests =
  testGroup
    "foreignKeyOnPk"
    [ QC.testProperty "overrides the foreign key found by FK discovery" $
        once $
          foreignKeysOf "weathers"
            === [ ForeignKey
                    "weathers_city__name_fkey"
                    (TableName "cities")
                    (S.fromList [(ColumnName "city__name", ColumnName "name")])
                    Cascade
                    Restrict
                ],
      QC.testProperty "gives distinct names to two keys on the same target table" $
        once $
          S.fromList (foreignKeysOf "routes")
            === S.fromList
              [ ForeignKey
                  "routes_from__name_fkey"
                  (TableName "cities")
                  (S.fromList [(ColumnName "from__name", ColumnName "name")])
                  Cascade
                  NoAction,
                ForeignKey
                  "routes_to__name_fkey"
                  (TableName "cities")
                  (S.fromList [(ColumnName "to__name", ColumnName "name")])
                  Restrict
                  NoAction
              ]
    ]
