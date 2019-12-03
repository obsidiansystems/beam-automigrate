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

import           Data.Typeable
import           Data.Text                                ( Text )
import           Data.Scientific                          ( Scientific )
import           Data.Time.Calendar                       ( Day )
import           Data.Time                                ( TimeOfDay )
import           Data.Int
import           Data.Time                                ( UTCTime )
import           Data.Word
import           Data.Set                                 ( Set )
import qualified Data.Set                                as S
import qualified Data.Text                               as T
import qualified Data.List.NonEmpty                      as NE
import qualified Data.Map.Strict                         as M

import           Database.Beam.Backend.SQL
import qualified Database.Beam                           as Beam

import           Database.Beam.Migrate.Types
import qualified Database.Beam.Postgres                  as Pg
import           Data.Aeson                              as JSON
                                                          ( FromJSON
                                                          , ToJSON
                                                          )

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

class IsEnumeration' (isEnum :: Bool) ty where
  schemaEnums' :: Proxy isEnum -> Proxy ty -> Enumerations

class IsEnumeration ty where
  schemaEnums :: Proxy ty -> Enumerations

type family SchemaConstraint (k :: *) where
    SchemaConstraint (Beam.TableEntity e)  = TableConstraint
    SchemaConstraint (Beam.TableField e t) = ColumnConstraint

type family IsMaybe (k :: *) :: Bool where
    IsMaybe (Maybe x)                     = 'True
    IsMaybe (Beam.TableField t (Maybe x)) = 'True
    IsMaybe (Beam.TableField t _)         = 'False
    IsMaybe _                             = 'False

type family IsEnum (k :: *) :: Bool where
    IsMaybe (PgEnum x)                    = 'True
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

-- Default instances for enum discovery.

instance (Show a, Typeable a, Enum a, Bounded a) => IsEnumeration' 'True (PgEnum a) where
  schemaEnums' Proxy Proxy = M.singleton ty vals
    where ty   = EnumerationName (T.pack $ showsTypeRep (typeRep (Proxy @a)) mempty)
          vals = Enumeration $ NE.fromList $ map (T.pack . show) ([minBound .. maxBound] :: [a])

instance IsEnumeration' 'False a where
  schemaEnums' Proxy Proxy = mempty

instance ( IsEnum a ~ isEnum
         , IsEnumeration' isEnum a
         ) => IsEnumeration a where
  schemaEnums = schemaEnums' (Proxy :: Proxy isEnum)

--
-- Sql datatype instances for the most common types.
--

instance HasDefaultSqlDataType ty => HasDefaultSqlDataType (Beam.TableField e ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)

instance HasDefaultSqlDataType ty => HasDefaultSqlDataType (Maybe ty) where
  defaultSqlDataType _ = defaultSqlDataType (Proxy @ty)

instance HasDefaultSqlDataType Int where
  defaultSqlDataType _ _ = SqlStdType intType
instance HasDefaultSqlDataType Int32 where
  defaultSqlDataType _ _ = SqlStdType intType
instance HasDefaultSqlDataType Int16 where
  defaultSqlDataType _ _ = SqlStdType intType
instance HasDefaultSqlDataType Int64 where
  defaultSqlDataType _ _ = SqlStdType bigIntType

instance HasDefaultSqlDataType Word where
  defaultSqlDataType _ _ = SqlStdType $ numericType (Just (10, Nothing))

instance HasDefaultSqlDataType Word16 where
  defaultSqlDataType _ _ = SqlStdType $ numericType (Just (5, Nothing))
instance HasDefaultSqlDataType Word32 where
  defaultSqlDataType _ _ = SqlStdType $ numericType (Just (10, Nothing))
instance HasDefaultSqlDataType Word64 where
  defaultSqlDataType _ _ = SqlStdType $ numericType (Just (20, Nothing))

instance HasDefaultSqlDataType Text where
  defaultSqlDataType _ _ = SqlStdType $ varCharType Nothing Nothing
instance HasDefaultSqlDataType SqlBitString where
  defaultSqlDataType _ _ = SqlStdType $ varBitType Nothing

instance HasDefaultSqlDataType Double where
  defaultSqlDataType _ _ = SqlStdType $ doubleType

instance HasDefaultSqlDataType Scientific where
  defaultSqlDataType _ _ = SqlStdType $ numericType (Just (20, Just 10))

instance HasDefaultSqlDataType Day where
  defaultSqlDataType _ _ = SqlStdType dateType

instance HasDefaultSqlDataType TimeOfDay where
  defaultSqlDataType _ _ = SqlStdType $ timeType Nothing False

instance HasDefaultSqlDataType Bool where
  defaultSqlDataType _ _ = SqlStdType booleanType

instance HasDefaultSqlDataType UTCTime where
  defaultSqlDataType _ _ = SqlStdType $ timestampType Nothing False

--
-- support for json types
--

instance (FromJSON a, ToJSON a) => HasDefaultSqlDataType (Pg.PgJSON a) where
  defaultSqlDataType _ _ = PgSpecificType PgJson

instance (FromJSON a, ToJSON a) => HasDefaultSqlDataType (Pg.PgJSONB a) where
  defaultSqlDataType _ _ = PgSpecificType PgJsonB

--
-- support for pg range types
--

instance HasDefaultSqlDataType (Pg.PgRange Pg.PgInt4Range a) where
  defaultSqlDataType _ _ = PgSpecificType PgRangeInt4

instance HasDefaultSqlDataType (Pg.PgRange Pg.PgInt8Range a) where
  defaultSqlDataType _ _ = PgSpecificType PgRangeInt8

instance HasDefaultSqlDataType (Pg.PgRange Pg.PgNumRange a) where
  defaultSqlDataType _ _ = PgSpecificType PgRangeNum

instance HasDefaultSqlDataType (Pg.PgRange Pg.PgTsRange a) where
  defaultSqlDataType _ _ = PgSpecificType PgRangeTs

instance HasDefaultSqlDataType (Pg.PgRange Pg.PgTsTzRange a) where
  defaultSqlDataType _ _ = PgSpecificType PgRangeTsTz

instance HasDefaultSqlDataType (Pg.PgRange Pg.PgDateRange a) where
  defaultSqlDataType _ _ = PgSpecificType PgRangeDate

--
-- support for enum types
--

instance (Show a, Typeable a, Enum a, Bounded a) => HasDefaultSqlDataType (PgEnum a) where
  defaultSqlDataType (Proxy :: (Proxy (PgEnum a))) _ = 
    PgSpecificType (PgEnumeration $ EnumerationName (T.pack $ showsTypeRep (typeRep (Proxy @a)) mempty))
