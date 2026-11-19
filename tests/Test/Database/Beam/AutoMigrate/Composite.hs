{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pure, database-free checks for structurally paired inferred composite
-- foreign keys, including nested keys reached through a nullable whole-key
-- reference.
module Test.Database.Beam.AutoMigrate.Composite (compositeTests, compositeSchema) where

import qualified Data.Map.Strict as M
import Data.Proxy
import qualified Data.Set as S
import Data.Text (Text)
import Database.Beam.AutoMigrate
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema hiding (Table)
import qualified Database.Beam.Schema as Beam
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

compositeSchema :: Schema
compositeSchema =
  fromAnnotatedDbSettings (defaultAnnotatedDbSettings compositeDb) (Proxy @'[])

compositeTests :: TestTree
compositeTests =
  testGroup
    "Composite"
    [ testCase "a composite key pairs structurally, not by sorted name" $ do
        child <- lookupTable compositeSchema "child"
        case foreignKeyConstraints child of
          [ForeignKey name target pairs onDelete onUpdate] -> do
            name @?= "child_z_fkey"
            target @?= TableName "parent"
            -- The local key is structurally (z, a) and the referenced key is
            -- (a, z). Pairing happens before 'Set' canonicalises the pairs, so
            -- the old independent sorts (a->a, z->z) must not reappear.
            pairs
              @?= S.fromList
                [ (ColumnName "z", ColumnName "a"),
                  (ColumnName "a", ColumnName "z")
                ]
            onDelete @?= NoAction
            onUpdate @?= NoAction
          other -> assertFailure ("expected exactly one inferred FK, got " <> show other),
      testCase "a nullable reference to a key containing another key pairs every nested column" $ do
        let schema = fromAnnotatedDbSettings (defaultAnnotatedDbSettings nestedDb) (Proxy @'[])
        leaf <- lookupTable schema "nested_leaf"
        case foreignKeyConstraints leaf of
          [ForeignKey _ target pairs _ _] -> do
            target @?= TableName "nested_outer"
            pairs
              @?= S.fromList
                [ (ColumnName "ref_a", ColumnName "base_a"),
                  (ColumnName "ref_x", ColumnName "x"),
                  (ColumnName "ref_z", ColumnName "base_z")
                ]
          other -> assertFailure ("expected exactly one inferred FK, got " <> show other)
    ]

lookupTable :: Schema -> Text -> IO Table
lookupTable schema name =
  maybe
    (assertFailure ("missing derived table " <> show name <> " in " <> show (M.keys (schemaTables schema))))
    pure
    (M.lookup (TableName name) (schemaTables schema))

foreignKeyConstraints :: Table -> [TableConstraint]
foreignKeyConstraints = filter isFk . S.toList . tableConstraints
  where
    isFk ForeignKey {} = True
    isFk _ = False

--
-- Composite key with renamed, reversed lexical key components
--

data ParentT f = ParentRow
  { pA :: Columnar f Int,
    pZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ChildT f = ChildRow
  { cId :: Columnar f Int,
    cParent :: PrimaryKey ParentT f
  }
  deriving (Generic, Beamable)

data CompositeDbT f = CompositeDb
  { compositeParent :: f (TableEntity ParentT),
    compositeChild :: f (TableEntity ChildT)
  }
  deriving (Generic, Database be)

instance Beam.Table ParentT where
  data PrimaryKey ParentT f = ParentKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ParentKey <$> pA <*> pZ

instance Beam.Table ChildT where
  data PrimaryKey ChildT f = ChildKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ChildKey . cId

compositeDb :: DatabaseSettings Postgres CompositeDbT
compositeDb =
  defaultDbSettings
    `withDbModification` dbModification
      { compositeParent =
          modifyTableFields tableModification
            { pA = fieldNamed "a",
              pZ = fieldNamed "z"
            },
        compositeChild =
          modifyTableFields tableModification
            { cParent = ParentKey (fieldNamed "z") (fieldNamed "a")
            }
      }

--
-- Nested nullable keys
--

-- | A key containing another key: @PrimaryKey NestedOuterT@ embeds
-- @PrimaryKey NestedBaseT@. The leaf references it through a nullable
-- whole-key field, so the ordered traversal must recurse through the nested
-- key inside the nullable wrapper.
data NestedBaseT f = NestedBaseRow
  { nestedBaseA :: Columnar f Int,
    nestedBaseZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data NestedOuterT f = NestedOuterRow
  { nestedOuterBase :: PrimaryKey NestedBaseT f,
    nestedOuterX :: Columnar f Int
  }
  deriving (Generic, Beamable)

data NestedLeafT f = NestedLeafRow
  { nestedLeafId :: Columnar f Int,
    nestedLeafOuter :: PrimaryKey NestedOuterT (Nullable f)
  }
  deriving (Generic, Beamable)

data NestedDbT f = NestedDb
  { nestedDbBase :: f (TableEntity NestedBaseT),
    nestedDbOuter :: f (TableEntity NestedOuterT),
    nestedDbLeaf :: f (TableEntity NestedLeafT)
  }
  deriving (Generic, Database be)

instance Beam.Table NestedBaseT where
  data PrimaryKey NestedBaseT f = NestedBaseKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NestedBaseKey <$> nestedBaseA <*> nestedBaseZ

instance Beam.Table NestedOuterT where
  data PrimaryKey NestedOuterT f = NestedOuterKey (PrimaryKey NestedBaseT f) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NestedOuterKey <$> nestedOuterBase <*> nestedOuterX

instance Beam.Table NestedLeafT where
  data PrimaryKey NestedLeafT f = NestedLeafKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NestedLeafKey . nestedLeafId

nestedDb :: DatabaseSettings Postgres NestedDbT
nestedDb =
  defaultDbSettings
    `withDbModification` dbModification
      { nestedDbBase =
          setEntityName "nested_base"
            <> modifyTableFields
              tableModification
                { nestedBaseA = fieldNamed "base_a",
                  nestedBaseZ = fieldNamed "base_z"
                },
        nestedDbOuter =
          setEntityName "nested_outer"
            <> modifyTableFields
              tableModification
                { nestedOuterBase = NestedBaseKey (fieldNamed "base_a") (fieldNamed "base_z"),
                  nestedOuterX = fieldNamed "x"
                },
        nestedDbLeaf =
          setEntityName "nested_leaf"
            <> modifyTableFields
              tableModification
                { nestedLeafId = fieldNamed "id",
                  nestedLeafOuter =
                    NestedOuterKey
                      (NestedBaseKey (fieldNamed "ref_a") (fieldNamed "ref_z"))
                      (fieldNamed "ref_x")
                }
      }
