{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
module Database.Beam.Migrate.Example where

import           Data.Text                      ( Text )

import           GHC.Generics
import           Control.Exception

import           Database.Beam.Postgres
import           Data.Set as S
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
                                                , TableConstraint(..)
                                                , TableName(..)
                                                , ReferenceAction(..)
                                                )
import           Database.Beam.Migrate.Postgres ( getSchema )

import qualified Database.PostgreSQL.Simple    as Pg
import           Database.Beam.Postgres (runBeamPostgresDebug)

-- Needed only for the examples, (re)move eventually.
import           Data.Int                       ( Int32
                                                , Int64
                                                )
import           Data.Time                      ( UTCTime )


--
-- Example
--


data FlowerT f = Flower
  { flowerID         :: Columnar f Int32
  , flowerName       :: Columnar f Text
  , flowerPrice      :: Columnar (Beam.Nullable f) Double
  , flowerDiscounted :: Columnar f (Maybe Bool)
  }
  deriving (Generic, Beamable)

data OrderT f = Order
  { orderID   :: Columnar f Int32
  , orderTime :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

data LineItemT f = LineItem
  { lineItemOrderID  :: PrimaryKey OrderT (Beam.Nullable f)
  , lineItemFlowerID :: PrimaryKey FlowerT f
  , lineItemQuantity :: Columnar f Int64
  , lineItemDiscount :: Columnar f (Maybe Bool)
  }
  deriving (Generic, Beamable)

data FlowerDB f = FlowerDB
  { dbFlowers   :: f (TableEntity FlowerT)
  , dbOrders    :: f (TableEntity OrderT)
  , dbLineItems :: f (TableEntity LineItemT)
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
    LineItemID (PrimaryKey OrderT (Beam.Nullable f)) (PrimaryKey FlowerT f)
    deriving (Generic, Beamable)
  primaryKey = LineItemID <$> lineItemOrderID <*> lineItemFlowerID

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
                                                      }
  }


annotatedDB :: AnnotatedDatabaseSettings Postgres FlowerDB
annotatedDB = defaultAnnotatedDbSettings flowerDB `withDbModification` dbModification
  { dbFlowers   = annotateTableFields tableModification { flowerDiscounted = defaultsTo True }
               <> annotateTableFields tableModification { flowerPrice = defaultsTo 10.0 }
  , dbLineItems = (addTableConstraints $ 
      S.fromList [ Unique "db_line_unique" (S.fromList ["flower_id", "order_id"])
                 --, ForeignKey "lineItemOrderID_fkey" (TableName "orders2") mempty NoAction NoAction
                 ])
               <> annotateTableFields tableModification { lineItemDiscount = defaultsTo False }
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
