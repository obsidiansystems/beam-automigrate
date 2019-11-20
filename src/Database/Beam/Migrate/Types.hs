{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Types where

import           Data.Map                    (Map)
import qualified Data.Map.Strict             as M
import           Data.Set                    (Set)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Lens.Micro                  ((^.))

import           GHC.Generics

import           Database.Beam.Postgres
import           Database.Beam.Schema        (Beamable, Columnar, Database,
                                              DatabaseSettings, PrimaryKey,
                                              TableEntity, TableSettings,
                                              defaultDbSettings,
                                              withDbModification,
                                              dbModification,
                                              tableModification,
                                              modifyTableFields,
                                              fieldNamed
                                             )
import qualified Database.Beam.Schema        as Beam
import           Database.Beam.Schema.Tables (IsDatabaseEntity,
                                              dbEntityDescriptor, dbEntityName, dbTableSettings)

-- Needed only for the examples, (re)move eventually.
import           Data.Int                    (Int32, Int64)
import           Data.Scientific             (Scientific)
import           Data.Time                   (UTCTime)

--
-- Types (sketched)
--

data Schema = Schema { schemaName   :: SchemaName
                     , schemaTables :: Tables
                     } deriving Show

type Tables = Map TableName Table

newtype SchemaName = SchemaName { unSchemaName :: Text } deriving Show

newtype TableName = TableName { tableName :: Text } deriving (Show, Eq, Ord)

newtype Table = Table { tableColumns :: Map ColumnName Column } deriving Show

instance Semigroup Table where
    (Table t1) <> (Table t2) = Table (t1 <> t2)

instance Monoid Table where
    mempty = Table mempty

newtype ColumnName = ColumnName { columnName :: Text } deriving (Show, Eq, Ord)

data Column = Column {
    columnType       :: ColumnType
  , columnConstrains :: Set ColumnConstraint
  } deriving Show

-- | Basic types for columns, everything is very naive for now.
type ColumnType       = ()

noColumnConstraints :: Set ColumnConstraint
noColumnConstraints = mempty

data ColumnConstraint =
    PrimaryKey
    -- ^ This 'Column' is the Table's primary key.
    deriving (Show, Eq, Ord)

-- | A possible list of edits on a 'Schema'.
data Edit =
    ColumnAdded TableName ColumnName Column
  | ColumnRemoned TableName ColumnName
  | TableAdded TableName Table
  | TableRemoved TableName

-- | A possible enumerations of the reasons why a 'diff' operation might not work.
data DiffError =
    AutomaticDiffNotPossible
    -- ^ The diff couldn't be completed. TODO(adn) We need extra information
    -- we can later on reify into the raw SQL queries users can try to run
    -- themselves.

--
-- Potential API (sketched)
--

class GSchema x where
    gSchema :: x p -> Schema

class GSchemaTables x where
    gSchemaTables :: x p -> Tables

class GSchemaTableEntry x where
    gSchemaTableEntry :: x p -> (TableName, Table)

class GSchemaTable x where
    gSchemaTable :: x p -> Table

class GSchemaColumnEntry x where
    gSchemaColumnEntry :: x p -> (ColumnName, Column)

instance GSchema x => GSchema (D1 f x) where
    gSchema (M1 x) = gSchema x
instance (Constructor f, GSchemaTables x) => GSchema (C1 f x) where
    gSchema (M1 x) =
        let sName = SchemaName . T.pack . conName $ (undefined :: (C1 f g x))
        in Schema { schemaName   = sName
                  , schemaTables = gSchemaTables x
                  }

instance (GSchemaTableEntry a, GSchemaTables b) => GSchemaTables (a :*: b) where
  gSchemaTables (a :*: b) = uncurry M.singleton (gSchemaTableEntry a) <> gSchemaTables b
instance GSchemaTableEntry (S1 f x) => GSchemaTables (S1 f x) where
    gSchemaTables = uncurry M.singleton . gSchemaTableEntry

instance GSchemaTableEntry x => GSchemaTableEntry (S1 f x) where
  gSchemaTableEntry (M1 x) = gSchemaTableEntry x
instance ( IsDatabaseEntity be (TableEntity tbl)
         , GSchemaTable (Rep (TableSettings tbl))
         , Generic (TableSettings tbl)
         )
  => GSchemaTableEntry (K1 R (Beam.DatabaseEntity be db (TableEntity tbl))) where
  gSchemaTableEntry (K1 entity) =
      let tName = entity ^. dbEntityDescriptor . dbEntityName
       in (TableName tName, gSchemaTable . from $ (dbTableSettings $ entity ^. dbEntityDescriptor ))

instance GSchemaTable x => GSchemaTable (D1 f x) where
    gSchemaTable (M1 x) = gSchemaTable x

instance GSchemaTable x => GSchemaTable (C1 f x) where
    gSchemaTable (M1 x) = gSchemaTable x

instance (GSchemaColumnEntry a, GSchemaTable b) => GSchemaTable (a :*: b) where
    gSchemaTable (a :*: b) = 
        Table (uncurry M.singleton (gSchemaColumnEntry a)) <> gSchemaTable b

instance GSchemaTable (S1 m (K1 R (Beam.TableField e t))) where
    gSchemaTable (M1 (K1 e)) = 
        let colName = ColumnName $ e ^. Beam.fieldName
        in  Table $ M.singleton colName (Column () noColumnConstraints)

instance GSchemaColumnEntry (S1 m (K1 R (Beam.TableField e t))) where
    gSchemaColumnEntry (M1 (K1 e)) = 
        let colName = ColumnName $ e ^. Beam.fieldName
        in (colName, Column () noColumnConstraints)

-- TODO(adn) Not quite correct as far as the 'PrimaryKey' is concerned.
instance GSchemaColumnEntry (S1 m (K1 R (PrimaryKey f (Beam.TableField t)))) where
    gSchemaColumnEntry (M1 (K1 _e)) = 
        let colName = ColumnName $ "todo" -- (Beam.pk e) ^. Beam.fieldName
        in (colName, Column () noColumnConstraints)

-- | Turns a Beam's 'DatabaseSettings' into a 'Schema'.
fromDbSettings :: ( Generic (DatabaseSettings be db)
                  , GSchema (Rep (DatabaseSettings be db))
                  )
               => DatabaseSettings be db
               -> Schema
fromDbSettings = gSchema . from

-- | Interpret a single 'Edit' into a function that transform the 'Schema'.
--evalEdit :: Edit -> (Schema -> Schema)
--evalEdit = \case
--    ColumnAdded tblName colName col -> _
--    ColumnRemoned tblName colName -> _
--    TableAdded tblName tbl -> _
--    TableRemoved tblName -> _

-- | Computes the diff between two 'Schema's, either failing with a 'DiffError'
-- or returning the list of 'Edit's necessary to turn the first into the second.
diff :: Schema -> Schema -> Either DiffError [Edit]
diff _old _new = undefined

--
-- Example
--


data FlowerT f = Flower
  { flowerID    :: Columnar f Int32
  , flowerName  :: Columnar f Text
  , flowerPrice :: Columnar f Scientific
  }
  deriving (Generic, Beamable)

data OrderT f = Order
  { orderID   :: Columnar f Int32
  , orderTime :: Columnar f UTCTime
  }
  deriving (Generic, Beamable)

data LineItemT f = LineItem
  { lineItemOrderID  :: PrimaryKey OrderT f
  , lineItemFlowerID :: PrimaryKey FlowerT f
  , lineItemQuantity :: Columnar f Int64
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
    LineItemID (PrimaryKey OrderT f) (PrimaryKey FlowerT f)
    deriving (Generic, Beamable)
  primaryKey = LineItemID
    <$> lineItemOrderID
    <*> lineItemFlowerID

-- Modify the field names to be compliant with William Yao's format.
flowerDB :: DatabaseSettings Postgres FlowerDB
flowerDB = defaultDbSettings `withDbModification`
            dbModification {
              dbFlowers = modifyTableFields tableModification {
                            flowerID = fieldNamed "id"
                          }
            , dbOrders = modifyTableFields tableModification {
                            orderID = fieldNamed "id"
                          , orderTime = fieldNamed "order_time"
                          }
            , dbLineItems = modifyTableFields tableModification {
                            lineItemFlowerID = FlowerID "flower_id"
                          , lineItemOrderID  = OrderID  "order_id"
                          , lineItemQuantity = fieldNamed "quantity"
                          }
            }

example :: IO ()
example = print $ fromDbSettings flowerDB
