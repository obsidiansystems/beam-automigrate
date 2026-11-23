-- | Check that a manual 'References' pair with incompatible scalar types fails compilation.
-- The control changes only the selector to a compatible Int column.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ManualTypeMismatch where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Text (Text)
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Annotated (ForeignKeyConstraint (..))
import Database.Beam.Postgres
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema
import GHC.Generics (Generic)

data FooT f = FooRow {fooId :: Columnar f Int}
  deriving (Generic, Beamable)

data BarT f = BarRow
  { barId :: Columnar f Int,
    barText :: Columnar f Text
  }
  deriving (Generic, Beamable)

data DbT f = Db
  { dbFoo :: f (TableEntity FooT),
    dbBar :: f (TableEntity BarT)
  }
  deriving (Generic, Database be)

instance Beam.Table FooT where
  data PrimaryKey FooT f = FooKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = FooKey . fooId

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
      { dbBar = foreignKeyOn (dbFoo db) ((References barText fooId) :| []) Cascade Restrict
      }

schema :: Either [ValidationFailed] Schema
schema =
  fromAnnotatedDbSettings
    annotated
    (Proxy @'[ 'UserDefinedFk BarT ])
