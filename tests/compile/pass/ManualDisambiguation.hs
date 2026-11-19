-- | Two manual declarations on an explicitly annotated table: multiple scalar
-- column pairs through 'foreignKeyOn' and a whole-key reference through
-- 'foreignKeyOnPk'.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ManualDisambiguation where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Annotated (ForeignKeyConstraint (..))
import Database.Beam.Postgres
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema
import GHC.Generics (Generic)

data FooT f = FooRow
  { fooA :: Columnar f Int,
    fooZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data BarT f = BarRow
  { barId :: Columnar f Int,
    barFooA :: Columnar f Int,
    barFooZ :: Columnar f Int,
    barFooPk :: PrimaryKey FooT f
  }
  deriving (Generic, Beamable)

data DbT f = Db
  { dbFoo1 :: f (TableEntity FooT),
    dbFoo2 :: f (TableEntity FooT),
    dbBar :: f (TableEntity BarT)
  }
  deriving (Generic, Database be)

instance Beam.Table FooT where
  data PrimaryKey FooT f = FooKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = FooKey <$> fooA <*> fooZ

instance Beam.Table BarT where
  data PrimaryKey BarT f = BarKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = BarKey . barId

db :: DatabaseSettings Postgres DbT
db = defaultDbSettings

annotated :: AnnotatedDatabaseSettings Postgres DbT
annotated =
  defaultAnnotatedDbSettings db
    `withDbModification` dbModification
      { dbBar =
          foreignKeyOn
            (dbFoo1 db)
            ((References barFooA fooA) :| [References barFooZ fooZ])
            Cascade
            Restrict
            <> foreignKeyOnPk (dbFoo2 db) barFooPk Restrict Cascade
      }

schema :: Either [ValidationFailed] Schema
schema =
  fromAnnotatedDbSettings
    annotated
    (Proxy @'[ 'UserDefinedFk BarT ])
