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
import           Data.Text                                ( Text )
import           Data.Scientific                          ( Scientific )
import           Data.Time.Calendar                       ( Day )
import           Data.Time                                ( TimeOfDay )
import           Data.Int
import           Data.Time                                ( UTCTime )
import           Data.Word
import           Data.Set                                 ( Set )
import qualified Data.Set                                as S

import           Database.Beam.Backend.SQL
import qualified Database.Beam                           as Beam

import           Database.Beam.Migrate.Types

{- | This is a module which adapts and simplifies certain things normally provided by "beam-migrate", but
     without the extra complication of importing and using the library itself.
-}

--
-- Specifying SQL data types and constraints
--

class HasDefaultSqlDataType ty where

  -- | Provide a data type for the given type
  defaultSqlDataType :: Proxy ty       -- ^ Concrete representation of the type
                     -> Bool           -- ^ 'True' if this field is in an embedded
                                       --   key or table, 'False' otherwise
                     -> ColumnType


class Ord (SchemaConstraint ty) => HasSchemaConstraints ty where
  -- | Provide arbitrary constraints on a field of the requested type. See
  -- 'FieldCheck' for more information on the formatting of constraints.
  schemaConstraints :: Proxy ty
                    -- ^ Concrete representation of the type
                    -> Set (SchemaConstraint ty)
  schemaConstraints _ = mempty

class Ord (SchemaConstraint ty) => HasSchemaConstraints' (nullary :: Bool) ty where
  -- | Provide arbitrary constraints on a field of the requested type. See
  -- 'FieldCheck' for more information on the formatting of constraints.
  schemaConstraints' :: Proxy nullary -> Proxy ty -> Set (SchemaConstraint ty)

type family SchemaConstraint (k :: *) where
    SchemaConstraint (Beam.TableEntity e)  = TableConstraint
    SchemaConstraint (Beam.TableField e t) = ColumnConstraint

type family IsMaybe (k :: *) :: Bool where
    IsMaybe (Maybe x)                     = 'True
    IsMaybe (Beam.TableField t (Maybe x)) = 'True
    IsMaybe (Beam.TableField t _)         = 'False
    IsMaybe _                             = 'False

-- Default /table-level/ constraints.
instance HasSchemaConstraints' 'True (Beam.TableEntity tbl) where
  schemaConstraints' Proxy Proxy = mempty

instance HasSchemaConstraints' 'False (Beam.TableEntity tbl) where
  schemaConstraints' Proxy Proxy = mempty

-- Default /field-level/ constraints.

instance HasSchemaConstraints' 'True (Beam.TableField e (Beam.TableField e t)) where
  schemaConstraints' Proxy Proxy = mempty

instance HasSchemaConstraints' 'False (Beam.TableField e (Beam.TableField e t)) where
  schemaConstraints' Proxy Proxy = S.singleton NotNull

instance HasSchemaConstraints' 'True (Beam.TableField e (Maybe t)) where
  schemaConstraints' Proxy Proxy = mempty

instance HasSchemaConstraints' 'False (Beam.TableField e t) where
  schemaConstraints' Proxy Proxy = S.singleton NotNull

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
