{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Control.Exception
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Int (Int32, Int64)
import Data.Proxy
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam.AutoMigrate
  ( DbEnum,
    Diff,
    HasColumnType,
    Migration,
    PgEnum,
    ReferenceAction (..),
    Schema,
    defaultAnnotatedDbSettings,
    diff,
    fromAnnotatedDbSettings,
    migrate,
    printMigration,
    unsafeRunMigration,
    onUpdate,
    onDelete,
  )
import Database.Beam.AutoMigrate.Annotated
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Backend.SQL.Types (SqlSerial (..))
import Database.Beam.Postgres
import qualified Database.Beam.Postgres as Pg
import Database.Beam.Query (currentTimestamp_, val_)
import Database.Beam.Schema
  ( Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    PrimaryKey,
    TableEntity,
    dbModification,
    defaultDbSettings,
    fieldNamed,
    modifyTableFields,
    tableModification,
    withDbModification,
  )
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema.Tables (primaryKey)
import qualified Database.PostgreSQL.Simple as Pg
import GHC.Generics

--
-- Example
--

data MyJson = MyJson
  { foo :: Int,
    bar :: Maybe Bool,
    quux :: Text
  }

deriveJSON defaultOptions ''MyJson

data FlowerType
  = Rose
  | Sunflower
  | Tulip
  deriving (Show, Enum, Bounded)

data Foo
  = Bar
  | Baz
  | Quux
  deriving (Show, Enum, Bounded)
  deriving (HasColumnType) via (PgEnum Foo)

-- A mixin embedded into another (i.e. 'Address').
data AddressRegion f = AddressRegion
  { addressState :: Columnar f (Maybe Text),
    addressCountry :: Columnar f (Maybe Text),
    addressPostalCode :: Columnar f (Maybe Text)
  }
  deriving (Generic, Beamable)

data Address f = Address
  { address :: Columnar f (Maybe Text),
    addressCity :: Columnar f (Maybe Text),
    addressRegion :: AddressRegion f
  }
  deriving (Generic, Beamable)

data FlowerT f = Flower
  { flowerID :: Columnar f Int32,
    flowerName :: Columnar f Text,
    flowerInternalID :: Columnar f (SqlSerial Int),
    flowerPrice :: Columnar (Beam.Nullable f) Double,
    flowerDiscounted :: Columnar f (Maybe Bool),
    flowerSchemaOne :: Columnar f (PgJSON MyJson),
    flowerSchemaTwo :: Columnar f (PgJSONB MyJson),
    flowerType :: Columnar f (DbEnum FlowerType),
    flowerPgType :: Columnar f Foo,
    flowerAddress :: Address f
  }
  deriving (Generic, Beamable)

data OrderT f = Order
  { orderID :: Columnar f (SqlSerial Int64),
    orderTime :: Columnar f LocalTime,
    orderFlowerIdRef :: PrimaryKey FlowerT f,
    orderValidity :: Columnar f (Pg.PgRange Pg.PgInt8Range Int64),
    orderAddress :: Address f,
    orderLineItemRef :: PrimaryKey LineItemT f
  }
  deriving (Generic, Beamable)

data LineItemT f = LineItem
  { lineItemOrderID :: PrimaryKey OrderT f,
    lineItemFlowerID :: PrimaryKey FlowerT f,
    lineItemQuantity :: Columnar f Int64,
    lineItemDiscount :: Columnar f (Maybe Bool),
    lineItemBytearray :: Columnar f ByteString,
    lineItemNullableRef :: PrimaryKey OrderT (Beam.Nullable f)
  }
  deriving (Generic, Beamable)

data LineItemTwoT f = LineItemTwo
  { lineItemTwoID :: Columnar f Int64,
    lineItemTwoFk :: PrimaryKey LineItemT f
  }
  deriving (Generic, Beamable)

data FlowerDB f = FlowerDB
  { dbFlowers :: f (TableEntity FlowerT),
    dbOrders :: f (TableEntity OrderT),
    dbOrders2 :: f (TableEntity OrderT),
    dbLineItems :: f (TableEntity LineItemT),
    dbLineItemsTwo :: f (TableEntity LineItemTwoT)
  }
  deriving (Generic, Database be)

instance Beam.Table FlowerT where
  data PrimaryKey FlowerT f = FlowerID (Columnar f Int32)
    deriving (Generic, Beamable)
  primaryKey = FlowerID . flowerID

instance Beam.Table OrderT where
  data PrimaryKey OrderT f = OrderID (Columnar f (SqlSerial Int64))
    deriving (Generic, Beamable)
  primaryKey = OrderID . orderID

instance Beam.Table LineItemT where
  data PrimaryKey LineItemT f
    = LineItemID (PrimaryKey OrderT f) (PrimaryKey FlowerT f)
    deriving (Generic, Beamable)
  primaryKey = LineItemID <$> lineItemOrderID <*> lineItemFlowerID

instance Beam.Table LineItemTwoT where
  data PrimaryKey LineItemTwoT f = LineItemTwoID (Columnar f Int64)
    deriving (Generic, Beamable)
  primaryKey = LineItemTwoID <$> lineItemTwoID

-- Modify the field names to be compliant with William Yao's format.
flowerDB :: DatabaseSettings Postgres FlowerDB
flowerDB =
  defaultDbSettings
    `withDbModification` dbModification
      { dbFlowers = modifyTableFields tableModification {flowerID = fieldNamed "id"},
        dbOrders =
          modifyTableFields
            tableModification
              { orderID = fieldNamed "id",
                orderTime = fieldNamed "order_time"
              },
        dbLineItems =
          modifyTableFields
            tableModification
              { lineItemFlowerID = FlowerID "flower_id",
                lineItemOrderID = OrderID "order_id",
                lineItemQuantity = fieldNamed "quantity",
                lineItemNullableRef =
                  OrderID "external_nullable_ref"
              }
      }

annotatedDB :: AnnotatedDatabaseSettings Postgres FlowerDB
annotatedDB =
  defaultAnnotatedDbSettings flowerDB
    `withDbModification` dbModification
      { dbFlowers =
          annotateTableFields tableModification {flowerDiscounted = defaultsTo (val_ $ Just True)}
            <> annotateTableFields tableModification {flowerPrice = defaultsTo (val_ $ Just 10.0)}
            <> uniqueConstraintOn [U (addressPostalCode . addressRegion . flowerAddress)],
        dbLineItems =
          annotateTableFields tableModification {lineItemDiscount = defaultsTo (val_ $ Just False)}
            <> uniqueConstraintOn [U lineItemFlowerID, U lineItemOrderID, U lineItemQuantity],
        dbOrders =
          annotateTableFields
            tableModification
              { orderTime = defaultsTo currentTimestamp_,
                orderValidity = defaultsTo (range_ Inclusive Inclusive (val_ $ Just 10) (val_ $ Just 20))
              }
            <> foreignKeyOnWithOptions (dbFlowers flowerDB) orderFlowerIdRef primaryKey (def { onUpdate = Cascade, onDelete = Restrict })
            <> uniqueConstraintOn [U (addressPostalCode . addressRegion . orderAddress)]
            --, dbLineItemsTwo = foreignKeyOn (dbLineItems flowerDB) [
            --                          lineItemTwoFk `References` LineItemID
            --                        ] Cascade Restrict
      }

hsSchema :: Schema
hsSchema =
  fromAnnotatedDbSettings annotatedDB (Proxy @'[ 'UserDefinedFk LineItemT])

-- fromAnnotatedDbSettings annotatedDB (Proxy @'[])

getDbSchema :: String -> IO Schema
getDbSchema dbName =
  bracket (connect defaultConnectInfo {connectDatabase = dbName}) close getSchema

getFlowerDbSchema :: IO Schema
getFlowerDbSchema = getDbSchema "beam-test-db"

getSchemaDiff :: IO Diff
getSchemaDiff = diff hsSchema <$> getFlowerDbSchema

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
    Pg.withTransaction conn $
      runBeamPostgresDebug putStrLn conn $ do
        let mig = migrate conn hsSchema
        action mig

exampleAutoMigration :: IO ()
exampleAutoMigration = withBeamTestDb unsafeRunMigration
