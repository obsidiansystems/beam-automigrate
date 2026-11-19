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

-- | Check that inferred composite foreign keys preserve field correspondence.
-- Include embedded keys inside nullable references. These tests require no database.
module Test.Database.Beam.AutoMigrate.Composite
  ( compositeTests,
    compositeSchema,
    manualSchema,
  )
where

import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty (..))
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

compositeSchema :: Either [ValidationFailed] Schema
compositeSchema =
  fromAnnotatedDbSettings (defaultAnnotatedDbSettings compositeDb) (Proxy @'[])

deriveOrFail :: Either [ValidationFailed] Schema -> IO Schema
deriveOrFail = either (assertFailure . ("derivation failed: " <>) . show) pure

compositeTests :: TestTree
compositeTests =
  testGroup
    "Composite"
    [ testCase "a composite key pairs structurally, not by sorted name" $ do
        schema <- deriveOrFail compositeSchema
        child <- lookupTable schema "child"
        case foreignKeyConstraints child of
          [ForeignKey name target pairs onDelete onUpdate] -> do
            name @?= "child_z_a_parent_a_z_fkey"
            target @?= TableName "parent"
            -- Preserve the correspondence between local (z, a) and referenced (a, z) fields.
            -- Pair fields before sorting the complete pairs.
            pairs
              @?= ( (ColumnName "a", ColumnName "z")
                      :| [(ColumnName "z", ColumnName "a")]
                  )
            onDelete @?= NoAction
            onUpdate @?= NoAction
          other -> assertFailure ("expected exactly one inferred FK, got " <> show other),
      testCase "a nullable reference to a key containing another key pairs every nested column" $ do
        schema <- deriveOrFail (fromAnnotatedDbSettings (defaultAnnotatedDbSettings nestedDb) (Proxy @'[]))
        leaf <- lookupTable schema "nested_leaf"
        case foreignKeyConstraints leaf of
          [ForeignKey _ target pairs _ _] -> do
            target @?= TableName "nested_outer"
            pairs
              @?= ( (ColumnName "ref_a", ColumnName "base_a")
                      :| [ (ColumnName "ref_x", ColumnName "x"),
                           (ColumnName "ref_z", ColumnName "base_z")
                         ]
                  )
          other -> assertFailure ("expected exactly one inferred FK, got " <> show other),
      testCase "manual scalar pairs and the nullable whole-key helper coexist" $ do
        schema <- deriveOrFail manualSchema
        child <- lookupTable schema "manual_child"
        let fks = foreignKeyConstraints child
        length fks @?= 2
        let allPairs = [pairs | ForeignKey _ _ pairs _ _ <- fks]
            allActions = [(d, u) | ForeignKey _ _ _ d u <- fks]
        assertBool
          ("expected the scalar pair set and the nullable declaration, got " <> show allPairs)
          ( S.fromList allPairs
              == S.fromList
                [ (ColumnName "a", ColumnName "z") :| [(ColumnName "z", ColumnName "a")],
                  (ColumnName "nullable_a", ColumnName "z")
                    :| [(ColumnName "nullable_z", ColumnName "a")]
                ]
          )
        assertBool
          ("expected the manual nullable actions, got " <> show allActions)
          ((SetNull, Restrict) `elem` allActions)
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
-- Check field pairing when column names have different sort orders.
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
-- Check field pairing through embedded keys and nullable references.
--

-- | Check that nullable references preserve every column of an embedded key.
-- @PrimaryKey NestedOuterT@ contains @PrimaryKey NestedBaseT@.
-- The leaf table references the outer key through a nullable field.
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

--
-- Manual scalar pairs and nullable whole-key reference
--

data ManualParentT f = ManualParentRow
  { mpA :: Columnar f Int,
    mpZ :: Columnar f Int
  }
  deriving (Generic, Beamable)

data ManualChildT f = ManualChildRow
  { mcId :: Columnar f Int,
    mcZ :: Columnar f Int,
    mcA :: Columnar f Int,
    mcParent :: PrimaryKey ManualParentT (Nullable f)
  }
  deriving (Generic, Beamable)

data ManualDbT f = ManualDb
  { manualDbParent :: f (TableEntity ManualParentT),
    manualDbChild :: f (TableEntity ManualChildT)
  }
  deriving (Generic, Database be)

instance Beam.Table ManualParentT where
  data PrimaryKey ManualParentT f = ManualParentKey (Columnar f Int) (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManualParentKey <$> mpA <*> mpZ

instance Beam.Table ManualChildT where
  data PrimaryKey ManualChildT f = ManualChildKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ManualChildKey . mcId

manualDb :: DatabaseSettings Postgres ManualDbT
manualDb =
  defaultDbSettings
    `withDbModification` dbModification
      { manualDbParent =
          setEntityName "manual_parent"
            <> modifyTableFields
              tableModification
                { mpA = fieldNamed "a",
                  mpZ = fieldNamed "z"
                },
        manualDbChild =
          setEntityName "manual_child"
            <> modifyTableFields
              tableModification
                { mcZ = fieldNamed "z",
                  mcA = fieldNamed "a",
                  mcParent =
                    ManualParentKey
                      (fieldNamed "nullable_z")
                      (fieldNamed "nullable_a")
                }
      }

manualAnnotated :: AnnotatedDatabaseSettings Postgres ManualDbT
manualAnnotated =
  defaultAnnotatedDbSettings manualDb
    `withDbModification` dbModification
      { manualDbChild =
          foreignKeyOn
            (manualDbParent manualDb)
            (References mcZ mpA :| [References mcA mpZ])
            Cascade
            Restrict
            <> foreignKeyOnNullablePk
              (manualDbParent manualDb)
              mcParent
              SetNull
              Restrict
      }

manualSchema :: Either [ValidationFailed] Schema
manualSchema = fromAnnotatedDbSettings manualAnnotated (Proxy @'[])
