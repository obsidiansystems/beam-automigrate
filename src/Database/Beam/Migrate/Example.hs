{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}
module Database.Beam.Migrate.Example where

import           Data.Text                      ( Text )

import           GHC.Generics
import           Control.Exception

import           Database.Beam.Postgres
import           Database.Beam.Schema           ( Beamable
                                                , Columnar
                                                , Database
                                                , DatabaseSettings
                                                , PrimaryKey
                                                , TableEntity
                                                , dbModification
                                                , defaultDbSettings
                                                , fieldNamed
                                                , modifyTableFields
                                                , tableModification
                                                , withDbModification
                                                )
import qualified Database.Beam.Schema          as Beam
import           Database.Beam.Schema.Tables    ( primaryKey )

import           Database.Beam.Migrate.Annotated

import           Database.Beam.Migrate          ( Schema
                                                , Diff
                                                , Migration
                                                , fromAnnotatedDbSettings
                                                , defaultAnnotatedDbSettings
                                                , diff
                                                , runMigration
                                                , printMigration
                                                , migrate
                                                , DbEnum
                                                , PgEnum
                                                , ReferenceAction(..)
                                                )
import           Database.Beam.Migrate.Postgres ( getSchema )

import qualified Database.PostgreSQL.Simple    as Pg
import           Database.Beam.Postgres (runBeamPostgresDebug, PgJSON(..))
import qualified Database.Beam.Postgres as Pg

-- Needed only for the examples, (re)move eventually.
import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Time                      ( UTCTime )
import           Data.Aeson.TH

--
-- Example
--

data MyJson = MyJson {
    foo  :: Int
  , bar  :: Maybe Bool
  , quux :: Text
  }

deriveJSON defaultOptions ''MyJson

data FlowerType = 
    Rose
  | Sunflower
  | Tulip
  deriving (Show, Enum, Bounded)

-- A mixin embedded into another (i.e. 'Address').
data AddressRegion f
  = AddressRegion
  { addressState      :: Columnar f (Maybe Text)
  , addressCountry    :: Columnar f (Maybe Text)
  , addressPostalCode :: Columnar f (Maybe Text)
  } deriving (Generic, Beamable)

data Address f
  = Address
  { address           :: Columnar f (Maybe Text)
  , addressCity       :: Columnar f (Maybe Text)
  , addressRegion     :: AddressRegion f
  } deriving (Generic, Beamable)

data FlowerT f = Flower
  { flowerID         :: Columnar f Int32
  , flowerName       :: Columnar f Text
  , flowerPrice      :: Columnar (Beam.Nullable f) Double
  , flowerDiscounted :: Columnar f (Maybe Bool)
  , flowerSchemaOne  :: Columnar f (PgJSON MyJson)
  , flowerSchemaTwo  :: Columnar f (PgJSONB MyJson)
  , flowerType       :: Columnar f (DbEnum FlowerType)
  , flowerPgType     :: Columnar f (PgEnum FlowerType)
  , flowerAddress    :: Address f
  }
  deriving (Generic, Beamable)

data OrderT f = Order
  { orderID          :: Columnar f Int32
  , orderTime        :: Columnar f UTCTime
  , orderFlowerIdRef :: PrimaryKey FlowerT f
  , orderValidity    :: Columnar f (Pg.PgRange Pg.PgInt4Range Int)
  , orderAddress     :: Address f
  , orderLineItemRef :: PrimaryKey LineItemT f
  }
  deriving (Generic, Beamable)

data LineItemT f = LineItem
  { lineItemOrderID     :: PrimaryKey OrderT f
  , lineItemFlowerID    :: PrimaryKey FlowerT f
  , lineItemQuantity    :: Columnar f Int64
  , lineItemDiscount    :: Columnar f (Maybe Bool)
  , lineItemNullableRef :: PrimaryKey OrderT (Beam.Nullable f)
  }
  deriving (Generic, Beamable)

data LineItemTwoT f = LineItemTwo
  { lineItemTwoID       :: Columnar f Int
  , lineItemTwoFk       :: PrimaryKey LineItemT f
  }
  deriving (Generic, Beamable)

data FlowerDB f = FlowerDB
  { dbFlowers      :: f (TableEntity FlowerT)
  , dbOrders       :: f (TableEntity OrderT)
  , dbLineItems    :: f (TableEntity LineItemT)
  , dbLineItemsTwo :: f (TableEntity LineItemTwoT)
  }
  deriving (Generic, Database be)

instance Beam.Table FlowerT where
  data PrimaryKey FlowerT f = FlowerID (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = FlowerID . flowerID

instance Beam.Table OrderT where
  data PrimaryKey OrderT f = OrderID (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = OrderID . orderID

instance Beam.Table LineItemT where
  data PrimaryKey LineItemT f =
    LineItemID (PrimaryKey OrderT f) (PrimaryKey FlowerT f)
    deriving (Generic, Beamable)
  primaryKey = LineItemID <$> lineItemOrderID <*> lineItemFlowerID

instance Beam.Table LineItemTwoT where
  data PrimaryKey LineItemTwoT f = LineItemTwoID (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = LineItemTwoID <$> lineItemTwoID

-- Modify the field names to be compliant with William Yao's format.
flowerDB :: DatabaseSettings Postgres FlowerDB
flowerDB = defaultDbSettings `withDbModification` dbModification
  { dbFlowers   = modifyTableFields tableModification { flowerID = fieldNamed "id" }
  , dbOrders    = modifyTableFields tableModification { orderID   = fieldNamed "id"
                                                      , orderTime = fieldNamed "order_time"
                                                      }
  , dbLineItems = modifyTableFields tableModification { lineItemFlowerID = FlowerID "flower_id"
                                                      , lineItemOrderID  = OrderID "order_id"
                                                      , lineItemQuantity = fieldNamed "quantity"
                                                      , lineItemNullableRef = 
                                                          OrderID "external_nullable_ref"
                                                      }
  }

annotatedDB :: AnnotatedDatabaseSettings Postgres FlowerDB
annotatedDB = defaultAnnotatedDbSettings flowerDB `withDbModification` dbModification
  { dbFlowers   = annotateTableFields tableModification { flowerDiscounted = defaultsTo True }
               <> annotateTableFields tableModification { flowerPrice = defaultsTo 10.0 }
               <> uniqueFields [U (addressPostalCode . addressRegion . flowerAddress)]
  , dbLineItems = annotateTableFields tableModification { lineItemDiscount = defaultsTo False }
               <> uniqueFields [U lineItemFlowerID, U lineItemOrderID, U lineItemQuantity]
  , dbOrders = foreignKeyOnPk (dbFlowers flowerDB) orderFlowerIdRef Cascade Restrict
             <> uniqueFields [U (addressPostalCode . addressRegion . orderAddress)]
  --, dbLineItemsTwo = foreignKeyOn (dbLineItems flowerDB) [
  --                          lineItemTwoFk `References` LineItemID
  --                        ] Cascade Restrict
  }

hsSchema :: Schema
hsSchema = fromAnnotatedDbSettings annotatedDB

getDbSchema :: String -> IO Schema
getDbSchema dbName = do
  bracket (connect defaultConnectInfo { connectUser = "adinapoli", connectDatabase = dbName }) close getSchema

getFlowerDbSchema :: IO Schema
getFlowerDbSchema = getDbSchema "beam-test-db"

getSchemaDiff :: IO Diff
getSchemaDiff = do
  dbSchema <- getFlowerDbSchema
  pure $ diff hsSchema dbSchema

getGroundhogSchema :: IO Schema
getGroundhogSchema = getDbSchema "groundhog-test-db"

-- | Just a simple example demonstrating a possible workflow for a migration.
example :: IO ()
example = do
  print hsSchema
  dbSchema <- getFlowerDbSchema
  print dbSchema
  print $ dbSchema == hsSchema
  let schemaDiff = diff hsSchema dbSchema
  print schemaDiff
  putStrLn "GROUNDHOG"
  getGroundhogSchema >>= print

exampleShowMigration :: IO ()
exampleShowMigration = withBeamTestDb printMigration

withBeamTestDb :: (Migration Pg -> Pg ()) -> IO ()
withBeamTestDb action = do
  let connInfo = "host=localhost port=5432 dbname=beam-test-db"
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $ runBeamPostgresDebug putStrLn conn $ do
      let mig = migrate conn hsSchema
      action mig

exampleAutoMigration :: IO ()
exampleAutoMigration = withBeamTestDb runMigration
