{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module EnumArraysSerial where

import Data.Proxy (Proxy (..))
import Data.Vector (Vector)
import Database.Beam.AutoMigrate (HasColumnType, PgEnum, Schema, ValidationFailed, defaultAnnotatedDbSettings, fromAnnotatedDbSettings)
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema (Beamable, Columnar, Database, PrimaryKey, TableEntity, defaultDbSettings)
import qualified Database.Beam.Schema as Beam
import GHC.Generics (Generic)

data Mood = Happy | Sad
  deriving (Show, Enum, Bounded)
  deriving (HasColumnType) via (PgEnum Mood)

data ParentT f = ParentRow
  { parentId :: Columnar f (SqlSerial Int),
    parentMoods :: Columnar f (Vector Mood)
  }
  deriving (Generic, Beamable)

data ChildT f = ChildRow
  { childId :: Columnar f Int,
    childParent :: PrimaryKey ParentT f
  }
  deriving (Generic, Beamable)

data DbT f = Db
  { dbParent :: f (TableEntity ParentT),
    dbChild :: f (TableEntity ChildT)
  }
  deriving (Generic, Database be)

instance Beam.Table ParentT where
  data PrimaryKey ParentT f = ParentKey (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = ParentKey . parentId

instance Beam.Table ChildT where
  data PrimaryKey ChildT f = ChildKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ChildKey . childId

schema :: Either [ValidationFailed] Schema
schema = fromAnnotatedDbSettings
  (defaultAnnotatedDbSettings (defaultDbSettings @Postgres @DbT))
  (Proxy @'[])
