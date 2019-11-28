{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Database.Beam.Migrate.Annotated where

import           Data.Kind

import           Lens.Micro as Lens
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Monoid (Endo(..))

import qualified Database.Beam                           as Beam
import           Database.Beam.Migrate.Compat
import           Database.Beam.Migrate.Types
import           Database.Beam.Schema.Tables              ( IsDatabaseEntity
                                                          , DatabaseEntityDescriptor
                                                          , TableEntity
                                                          , DatabaseEntity
                                                          , DatabaseEntityDefaultRequirements
                                                          , DatabaseEntityRegularRequirements
                                                          , dbEntityDescriptor
                                                          , FieldModification(..)
                                                          , TableField
                                                          , EntityModification(..)
                                                          )
--
-- Annotating a 'DatabaseSettings' with meta information.
--

type AnnotatedDatabaseSettings be db = db (AnnotatedDatabaseEntity be db)

data AnnotatedDatabaseEntity be (db :: (* -> *) -> *) entityType where
  AnnotatedDatabaseEntity :: (IsAnnotatedDatabaseEntity be entityType, IsDatabaseEntity be entityType)
                          => AnnotatedDatabaseEntityDescriptor be entityType
                          -> DatabaseEntity be db entityType
                          -> AnnotatedDatabaseEntity be db entityType

class IsAnnotatedDatabaseEntity be entityType where
  data AnnotatedDatabaseEntityDescriptor be entityType :: *
  type AnnotatedDatabaseEntityDefaultRequirements be entityType :: Constraint
  type AnnotatedDatabaseEntityRegularRequirements be entityType :: Constraint

instance IsDatabaseEntity be (TableEntity tbl) => IsAnnotatedDatabaseEntity be (TableEntity tbl) where
  data AnnotatedDatabaseEntityDescriptor be (TableEntity tbl) where
    AnnotatedDatabaseTable
      :: Beam.Table tbl =>
       { dbAnnotatedSchema      :: TableSchema tbl 
       , dbAnnotatedConstraints :: Set TableConstraint
       }
      -> AnnotatedDatabaseEntityDescriptor be (TableEntity tbl)
  type AnnotatedDatabaseEntityDefaultRequirements be (TableEntity tbl) =
      DatabaseEntityDefaultRequirements be (TableEntity tbl)
  type AnnotatedDatabaseEntityRegularRequirements be (TableEntity tbl) =
      DatabaseEntityRegularRequirements be (TableEntity tbl)

lowerEntityDescriptor :: SimpleGetter (AnnotatedDatabaseEntity be db entityType) (DatabaseEntityDescriptor be entityType)
lowerEntityDescriptor = Lens.to (\(AnnotatedDatabaseEntity _ e) -> e ^. dbEntityDescriptor)

deannotate :: SimpleGetter (AnnotatedDatabaseEntity be db entityType) (DatabaseEntity be db entityType)
deannotate = Lens.to (\(AnnotatedDatabaseEntity _ e) -> e)

-- | A table schema.
type TableSchema tbl =
    tbl (TableFieldSchema tbl)

-- | A schema for a field within a given table
data TableFieldSchema (table :: (* -> *) -> *) ty where
    TableFieldSchema 
      :: 
      { tableFieldSchema :: FieldSchema ty } 
      -> TableFieldSchema table ty

data FieldSchema ty where
  FieldSchema :: Set ColumnConstraint
              -> FieldSchema ty

annotateTableFields :: tbl (FieldModification (TableFieldSchema tbl)) 
                    -> EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
annotateTableFields modFields = 
    EntityModification (Endo (\(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) 
      -> AnnotatedDatabaseEntity (tbl { 
       dbAnnotatedSchema = Beam.withTableModification modFields (dbAnnotatedSchema tbl) 
                            }) e))


addTableConstraints :: Set TableConstraint 
                    -> EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
addTableConstraints con =
    EntityModification (Endo (\(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) 
      -> AnnotatedDatabaseEntity (tbl { 
       dbAnnotatedConstraints = (dbAnnotatedConstraints tbl) <> con
                            }) e))

defaultsTo :: Show ty => ty -> FieldModification (TableFieldSchema tbl) (Maybe ty)
defaultsTo tyVal = FieldModification $ \old -> 
    case tableFieldSchema old of 
      FieldSchema c -> old { tableFieldSchema = FieldSchema $ S.singleton (Default $ T.pack $ show tyVal) <> c }

--setColumnConstraints :: IsAnnotatedDatabaseEntity be (TableEntity tbl)
--                     => Set ColumnConstraint
--                     -> EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
--setColumnConstraints con =
--    EntityModification (Endo (\(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) 
--      -> AnnotatedDatabaseEntity (tbl { 
--       dbAnnotatedSchema =  TableFieldSchema (FieldSchema con)
--                            }) e))
