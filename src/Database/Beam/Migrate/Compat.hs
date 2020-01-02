{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- | This is a module which adapts and simplifies certain things normally provided by "beam-migrate", but
     without the extra complication of importing and using the library itself.
-}

module Database.Beam.Migrate.Compat where

import           Data.Typeable
import           Data.Text                                ( Text )
import           Data.Scientific                          ( Scientific )
import           Data.Time.Calendar                       ( Day )
import           Data.Time                                ( TimeOfDay
                                                          , LocalTime
                                                          )
import           Data.Int
import           Data.Word
import           Data.Set                                 ( Set )
import qualified Data.Set                                as S
import qualified Data.Text                               as T
import qualified Data.Map.Strict                         as M

import           Database.Beam.Backend.SQL
import qualified Database.Beam                           as Beam

import           Database.Beam.Migrate.Types
import qualified Database.Beam.Postgres                  as Pg
import           Data.Aeson                              as JSON
                                                          ( FromJSON
                                                          , ToJSON
                                                          )

--
-- Specifying SQL data types and constraints
--

class HasColumnType ty where

  -- | Provide a 'ColumnType' for the given type
  defaultColumnType :: Proxy ty -> ColumnType

  -- | If @ty@ maps to a DB @ENUM@, use this method to specify which one.
  defaultEnums      :: Proxy ty -> Enumerations
  defaultEnums _ = mempty

class Ord (SchemaConstraint ty) => HasSchemaConstraints ty where

  -- | Provide arbitrary constraints on a field of the requested type.
  schemaConstraints :: Proxy ty -> Set (SchemaConstraint ty)
  schemaConstraints _ = mempty

class Ord (SchemaConstraint ty) => HasSchemaConstraints' (nullary :: Bool) ty where

  schemaConstraints' :: Proxy nullary -> Proxy ty -> Set (SchemaConstraint ty)

type family SchemaConstraint (k :: *) where
  SchemaConstraint (Beam.TableEntity e)  = TableConstraint
  SchemaConstraint (Beam.TableField e t) = ColumnConstraint

type family IsMaybe (k :: *) :: Bool where
  IsMaybe (Maybe x)                     = 'True
  IsMaybe (Beam.TableField t (Maybe x)) = 'True
  IsMaybe (Beam.TableField t _)         = 'False
  IsMaybe _                             = 'False

type family IsPgEnum (k :: *) :: Bool where
  IsPgEnum (PgEnum x)                    = 'True
  IsPgEnum _                             = 'False

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

instance HasColumnType ty => HasColumnType (Beam.TableField e ty) where
  defaultColumnType _ = defaultColumnType (Proxy @ty)

instance HasColumnType ty => HasColumnType (Maybe ty) where
  defaultColumnType _ = defaultColumnType (Proxy @ty)

instance HasColumnType Int where
  defaultColumnType _ = SqlStdType intType

instance HasColumnType Int32 where
  defaultColumnType _ = SqlStdType intType

instance HasColumnType Int16 where
  defaultColumnType _ = SqlStdType intType

instance HasColumnType Int64 where
  defaultColumnType _ = SqlStdType bigIntType

instance HasColumnType Word where
  defaultColumnType _ = SqlStdType $ numericType (Just (10, Nothing))

instance HasColumnType Word16 where
  defaultColumnType _ = SqlStdType $ numericType (Just (5, Nothing))

instance HasColumnType Word32 where
  defaultColumnType _ = SqlStdType $ numericType (Just (10, Nothing))

instance HasColumnType Word64 where
  defaultColumnType _ = SqlStdType $ numericType (Just (20, Nothing))

instance HasColumnType Text where
  defaultColumnType _ = SqlStdType $ varCharType Nothing Nothing

instance HasColumnType SqlBitString where
  defaultColumnType _ = SqlStdType $ varBitType Nothing

instance HasColumnType Double where
  defaultColumnType _ = SqlStdType doubleType

instance HasColumnType Scientific where
  defaultColumnType _ = SqlStdType $ numericType (Just (20, Just 10))

instance HasColumnType Day where
  defaultColumnType _ = SqlStdType dateType

instance HasColumnType TimeOfDay where
  defaultColumnType _ = SqlStdType $ timeType Nothing False

instance HasColumnType Bool where
  defaultColumnType _ = SqlStdType booleanType

instance HasColumnType LocalTime where
  defaultColumnType _ = SqlStdType $ timestampType Nothing False

--
-- support for json types
--

instance (FromJSON a, ToJSON a) => HasColumnType (Pg.PgJSON a) where
  defaultColumnType _ = PgSpecificType PgJson

instance (FromJSON a, ToJSON a) => HasColumnType (Pg.PgJSONB a) where
  defaultColumnType _ = PgSpecificType PgJsonB

--
-- support for pg range types
--

instance HasColumnType (Pg.PgRange Pg.PgInt4Range a) where
  defaultColumnType _ = PgSpecificType PgRangeInt4

instance HasColumnType (Pg.PgRange Pg.PgInt8Range a) where
  defaultColumnType _ = PgSpecificType PgRangeInt8

instance HasColumnType (Pg.PgRange Pg.PgNumRange a) where
  defaultColumnType _ = PgSpecificType PgRangeNum

instance HasColumnType (Pg.PgRange Pg.PgTsRange a) where
  defaultColumnType _ = PgSpecificType PgRangeTs

instance HasColumnType (Pg.PgRange Pg.PgTsTzRange a) where
  defaultColumnType _ = PgSpecificType PgRangeTsTz

instance HasColumnType (Pg.PgRange Pg.PgDateRange a) where
  defaultColumnType _ = PgSpecificType PgRangeDate

--
-- support for enum types
--

instance (Show a, Typeable a, Enum a, Bounded a) => HasColumnType (PgEnum a) where
  defaultColumnType (Proxy :: (Proxy (PgEnum a))) =
    -- Postgres converts enumeration types to lowercase, so we need to call 'toLower' here.
    PgSpecificType (PgEnumeration $ EnumerationName (T.toLower . T.pack $ showsTypeRep (typeRep (Proxy @a)) mempty))

  defaultEnums p@(Proxy :: (Proxy (PgEnum a))) =
    let (PgSpecificType (PgEnumeration ty)) = defaultColumnType p
        vals = Enumeration $ map (T.pack . show) ([minBound .. maxBound] :: [a])
    in M.singleton ty vals

instance (Show a, Typeable a, Enum a, Bounded a) => HasColumnType (DbEnum a) where
  defaultColumnType (Proxy :: (Proxy (DbEnum a))) =
    let vals = Enumeration $ map (T.pack . show) ([minBound .. maxBound] :: [a])
    in DbEnumeration (EnumerationName (T.pack $ showsTypeRep (typeRep (Proxy @a)) mempty)) vals

