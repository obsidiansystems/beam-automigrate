{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Beam.Migrate.Compat where

import           Data.Proxy
import           Data.Kind
import           Data.Text                                ( Text )
import           Data.Scientific                          ( Scientific )
import           Data.Time.Calendar                       ( Day )
import           Data.Time                                ( TimeOfDay )
import           Data.Int
import           Data.Time                                ( UTCTime )
import           Data.Word
import           Lens.Micro as Lens

import           Database.Beam.Backend.SQL
import qualified Database.Beam                           as Beam
import           Database.Beam.Schema.Tables              ( IsDatabaseEntity
                                                          , DatabaseEntityDescriptor
                                                          , TableEntity
                                                          , DatabaseEntity
                                                          , DatabaseEntityDefaultRequirements
                                                          , DatabaseEntityRegularRequirements
                                                          , dbEntityDescriptor
                                                          )

import           Database.Beam.Migrate.Types

{- | This is a module which adapts and simplifies certain things normally provided by "beam-migrate", but
     without the extra complication of importing and using the library itself.
-}

--
-- Annotating a 'DatabaseSettings' with meta information.
--

type AnnotatedDatabaseSettings be db = db (AnnotatedDatabaseEntity be)

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
       { dbAnnotatedSchema :: TableSchema tbl }
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
data TableFieldSchema (table :: (* -> *) -> *) ty
    = TableFieldSchema Text (FieldSchema ty)

data FieldSchema ty where
  FieldSchema :: FieldSchema ty

--
-- Specifying SQL data types and constraints
--

class HasDefaultSqlDataType ty where

  -- | Provide a data type for the given type
  defaultSqlDataType :: Proxy ty       -- ^ Concrete representation of the type
                     -> Bool           -- ^ 'True' if this field is in an embedded
                                       --   key or table, 'False' otherwise
                     -> ColumnType


class HasSchemaConstraints ty where
  -- | Provide arbitrary constraints on a field of the requested type. See
  -- 'FieldCheck' for more information on the formatting of constraints.
  schemaConstraints :: Proxy ty
                    -- ^ Concrete representation of the type
                    -> [ SchemaConstraint ty ]
  schemaConstraints _ = []

class HasSchemaConstraints' (nullary :: Bool) ty where
  -- | Provide arbitrary constraints on a field of the requested type. See
  -- 'FieldCheck' for more information on the formatting of constraints.
  schemaConstraints' :: Proxy nullary -> Proxy ty -> [ SchemaConstraint ty ]

type family SchemaConstraint (k :: *) where
    SchemaConstraint (Beam.TableEntity e)  = TableConstraint
    SchemaConstraint (Beam.TableField e t) = ColumnConstraint

type family IsMaybe (k :: *) :: Bool where
    IsMaybe (Maybe x)                     = 'True
    IsMaybe (Beam.TableField t (Maybe x)) = 'True
    IsMaybe (Beam.TableField t _)         = 'False
    IsMaybe _                             = 'False

instance HasSchemaConstraints' 'True (Beam.TableField e (Beam.TableField e t)) where
  schemaConstraints' Proxy Proxy = []

instance HasSchemaConstraints' 'False (Beam.TableField e (Beam.TableField e t)) where
  schemaConstraints' Proxy Proxy = [NotNull]

instance HasSchemaConstraints' 'True (Beam.TableField e (Maybe t)) where
  schemaConstraints' Proxy Proxy = []

instance HasSchemaConstraints' 'False (Beam.TableField e t) where
  schemaConstraints' Proxy Proxy = [NotNull]

instance ( IsMaybe a ~ nullary
         , HasSchemaConstraints' nullary a
         ) => HasSchemaConstraints a where
  schemaConstraints = schemaConstraints' (Proxy :: Proxy nullary)

--
-- Sql datatype instances for the most common types.
--

instance HasDefaultSqlDataType ty => HasDefaultSqlDataType (Maybe ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)

instance HasDefaultSqlDataType Int where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataType Int32 where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataType Int16 where
  defaultSqlDataType _ _ = intType
instance HasDefaultSqlDataType Int64 where
  defaultSqlDataType _ _ = bigIntType

instance HasDefaultSqlDataType Word where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))

instance HasDefaultSqlDataType Word16 where
  defaultSqlDataType _ _ = numericType (Just (5, Nothing))
instance HasDefaultSqlDataType Word32 where
  defaultSqlDataType _ _ = numericType (Just (10, Nothing))
instance HasDefaultSqlDataType Word64 where
  defaultSqlDataType _ _ = numericType (Just (20, Nothing))

instance HasDefaultSqlDataType Text where
  defaultSqlDataType _ _ = varCharType Nothing Nothing
instance HasDefaultSqlDataType SqlBitString where
  defaultSqlDataType _ _ = varBitType Nothing

instance HasDefaultSqlDataType Double where
  defaultSqlDataType _ _ = doubleType

instance HasDefaultSqlDataType Scientific where
  defaultSqlDataType _ _ = numericType (Just (20, Just 10))

instance HasDefaultSqlDataType Day where
  defaultSqlDataType _ _ = dateType

instance HasDefaultSqlDataType TimeOfDay where
  defaultSqlDataType _ _ = timeType Nothing False

instance HasDefaultSqlDataType Bool where
  defaultSqlDataType _ _ = booleanType

instance HasDefaultSqlDataType UTCTime where
  defaultSqlDataType _ _ = timestampType Nothing False
