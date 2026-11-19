-- | Renamed composite and nullable whole-key references, installed through the
-- nullable whole-key manual helper, plus a nullable reference to a composite
-- key that itself contains another key (inferred FK discovery).
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

-- A key that itself contains another key, referenced through a nullable
-- whole-key field. This exercises the nested 'GOrderedColumnNames' dictionary.
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

-- The child's structural key order (z,a) deliberately differs from the
-- parent's (a,z); pairing is positional, not name-based.
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
