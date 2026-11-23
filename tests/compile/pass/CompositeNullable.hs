-- | Check that renamed composite keys and nullable whole-key declarations compile.
-- Include an inferred nullable reference to a composite key that contains another key.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module CompositeNullable where

import Data.Proxy
import Database.Beam.AutoMigrate
import Database.Beam.Postgres
import qualified Database.Beam.Schema as Beam
import Database.Beam.Schema
import GHC.Generics (Generic)

data ParentT f = ParentRow
  { pA :: Columnar f Int
  , pZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ChildT f = ChildRow
  { cId :: Columnar f Int
  , cZ :: Columnar f Int
  , cA :: Columnar f Int
  , cParent :: PrimaryKey ParentT (Nullable f)
  }
  deriving (Generic, Beamable)

-- Check that inferred nullable references support embedded keys.
data NestedBaseT f = NestedBaseRow
  { nbA :: Columnar f Int
  , nbZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data NestedOuterT f = NestedOuterRow
  { noBase :: PrimaryKey NestedBaseT f
  , noX :: Columnar f Int
  }
  deriving (Generic, Beamable)

data NestedLeafT f = NestedLeafRow
  { nlId :: Columnar f Int
  , nlOuter :: PrimaryKey NestedOuterT (Nullable f)
  }
  deriving (Generic, Beamable)

data DbT f = Db
  { dbParent :: f (TableEntity ParentT)
  , dbChild :: f (TableEntity ChildT)
  , dbNestedBase :: f (TableEntity NestedBaseT)
  , dbNestedOuter :: f (TableEntity NestedOuterT)
  , dbNestedLeaf :: f (TableEntity NestedLeafT)
  }
  deriving (Generic, Database be)

instance Beam.Table ParentT where
  data PrimaryKey ParentT f = ParentKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ParentKey <$> pA <*> pZ

instance Beam.Table ChildT where
  data PrimaryKey ChildT f = ChildKey (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = ChildKey . cId

instance Beam.Table NestedBaseT where
  data PrimaryKey NestedBaseT f = NestedBaseKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NestedBaseKey <$> nbA <*> nbZ

instance Beam.Table NestedOuterT where
  data PrimaryKey NestedOuterT f = NestedOuterKey (PrimaryKey NestedBaseT f) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NestedOuterKey <$> noBase <*> noX

instance Beam.Table NestedLeafT where
  data PrimaryKey NestedLeafT f = NestedLeafKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NestedLeafKey . nlId

-- Preserve field correspondence despite different column-name orders.
-- The child fields are (z,a), and the parent fields are (a,z).
db :: DatabaseSettings Postgres DbT
db =
  defaultDbSettings
    `withDbModification` dbModification
      { dbParent =
          modifyTableFields tableModification
            { pA = fieldNamed "a"
            , pZ = fieldNamed "z"
            },
        dbChild =
          modifyTableFields tableModification
            { cZ = fieldNamed "z"
            , cA = fieldNamed "a"
            },
        dbNestedBase =
          setEntityName "nested_base"
            <> modifyTableFields
              tableModification
                { nbA = fieldNamed "base_a"
                , nbZ = fieldNamed "base_z"
                },
        dbNestedOuter =
          setEntityName "nested_outer"
            <> modifyTableFields
              tableModification
                { noBase = NestedBaseKey (fieldNamed "base_a") (fieldNamed "base_z")
                , noX = fieldNamed "x"
                },
        dbNestedLeaf =
          setEntityName "nested_leaf"
            <> modifyTableFields
              tableModification
                { nlId = fieldNamed "id"
                , nlOuter =
                    NestedOuterKey
                      (NestedBaseKey (fieldNamed "ref_a") (fieldNamed "ref_z"))
                      (fieldNamed "ref_x")
                }
      }

annotated :: AnnotatedDatabaseSettings Postgres DbT
annotated =
  defaultAnnotatedDbSettings db
    `withDbModification` dbModification
      { dbChild =
          foreignKeyOnNullablePk (dbParent db) cParent Restrict Restrict
      }

schema :: Either [ValidationFailed] Schema
schema =
  fromAnnotatedDbSettings
    annotated
    (Proxy @'[ 'UserDefinedFk ChildT ])
