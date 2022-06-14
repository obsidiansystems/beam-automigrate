{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- | This is a module which adapts and simplifies certain things normally provided by "beam-migrate", but
--     without the extra complication of importing and using the library itself.
module Database.Beam.AutoMigrate.Compat where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Int
import qualified Data.Map.Strict as M
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime, TimeOfDay, UTCTime)
import Data.Time.Calendar (Day)
import Data.Typeable
import Data.UUID
import Data.Word
import qualified Database.Beam as Beam
import Database.Beam.AutoMigrate.Types
-- import qualified Database.Beam.AutoMigrate.Util as Util
import Database.Beam.Backend.SQL hiding (tableName)
import qualified Database.Beam.Backend.SQL.AST as AST
import qualified Database.Beam.Postgres as Pg
import qualified Database.PostgreSQL.Simple.Types as Psql

--
-- Specifying SQL data types and constraints
--

class HasColumnType ty where
  -- | Provide a 'ColumnType' for the given type
  defaultColumnType :: Proxy ty -> ColumnType

  defaultTypeCast :: Proxy ty -> Maybe Text
  defaultTypeCast _ = Nothing

  -- | If @ty@ maps to a DB @ENUM@, use this method to specify which one.
  defaultEnums :: Proxy ty -> Enumerations
  defaultEnums _ = mempty

class HasSchemaConstraints ty where
  -- | Provide arbitrary constraints on a field of the requested type.
  schemaConstraints :: Proxy ty -> SchemaConstraint ty

class HasSchemaConstraints' (nullary :: Bool) ty where
  schemaConstraints' :: Proxy nullary -> Proxy ty -> SchemaConstraint ty

type family SchemaConstraint (k :: *) where
  SchemaConstraint (Beam.TableEntity e) = TableConstraints
  SchemaConstraint (Beam.TableField e t) = ColumnConstraints

type family IsMaybe (k :: *) :: Bool where
  IsMaybe (Maybe x) = 'True
  IsMaybe (Beam.TableField t (Maybe x)) = 'True
  IsMaybe (Beam.TableField t _) = 'False
  IsMaybe _ = 'False

-- Default /table-level/ constraints.
instance HasSchemaConstraints' 'True (Beam.TableEntity tbl) where
  schemaConstraints' Proxy Proxy = noTableConstraints

instance HasSchemaConstraints' 'False (Beam.TableEntity tbl) where
  schemaConstraints' Proxy Proxy = noTableConstraints

-- Default /field-level/ constraints.

instance HasSchemaConstraints' 'True (Beam.TableField e (Beam.TableField e t)) where
  schemaConstraints' Proxy Proxy = noColumnConstraints

instance HasSchemaConstraints' 'False (Beam.TableField e (Beam.TableField e t)) where
  schemaConstraints' Proxy Proxy = noColumnConstraints { columnNullable = NotNull }

instance HasSchemaConstraints' 'True (Beam.TableField e (Maybe t)) where
  schemaConstraints' Proxy Proxy = noColumnConstraints

instance HasSchemaConstraints' 'False (Beam.TableField e t) where
  schemaConstraints' Proxy Proxy = noColumnConstraints { columnNullable = NotNull }

instance
  ( IsMaybe a ~ nullary,
    HasSchemaConstraints' nullary a
  ) =>
  HasSchemaConstraints a
  where
  schemaConstraints = schemaConstraints' (Proxy :: Proxy nullary)

--
-- Generating \"companion\" sequences when particular types are used.
--

type family GeneratesSqlSequence ty where
  GeneratesSqlSequence (SqlSerial a) = 'True
  GeneratesSqlSequence _ = 'False

class HasCompanionSequence' (generatesSeq :: Bool) ty where
  hasCompanionSequence' ::
    Proxy generatesSeq ->
    Proxy ty ->
    TableName ->
    ColumnName ->
    Maybe ((Maybe SequenceName, Sequence), DefaultConstraint)

class HasCompanionSequence ty where
  hasCompanionSequence ::
    Proxy ty ->
    TableName ->
    ColumnName ->
    Maybe ((Maybe SequenceName, Sequence), DefaultConstraint)

instance
  ( GeneratesSqlSequence ty ~ genSeq,
    HasCompanionSequence' genSeq ty
  ) =>
  HasCompanionSequence ty
  where
  hasCompanionSequence = hasCompanionSequence' (Proxy :: Proxy genSeq)

instance HasCompanionSequence' 'False ty where
  hasCompanionSequence' _ _ _ _ = Nothing

--
-- Sql datatype instances for the most common types.
--

instance HasColumnType ty => HasColumnType (Beam.TableField e ty) where
  defaultColumnType _ = defaultColumnType (Proxy @ty)
  defaultTypeCast _ = defaultTypeCast (Proxy @ty)

instance HasColumnType ty => HasColumnType (Maybe ty) where
  defaultColumnType _ = defaultColumnType (Proxy @ty)
  defaultTypeCast _ = defaultTypeCast (Proxy @ty)
  defaultEnums _ = defaultEnums (Proxy @ty)

instance HasColumnType Int where
  defaultColumnType _ = SqlStdType intType
  defaultTypeCast _ = Just "integer"

instance HasColumnType Int32 where
  defaultColumnType _ = SqlStdType intType
  defaultTypeCast _ = Just "integer"

instance HasColumnType Int16 where
  defaultColumnType _ = SqlStdType intType
  defaultTypeCast _ = Just "integer"

instance HasColumnType Int64 where
  defaultColumnType _ = SqlStdType bigIntType
  defaultTypeCast _ = Just "bigint"

instance HasColumnType Word where
  defaultColumnType _ = SqlStdType $ numericType (Just (10, Nothing))
  defaultTypeCast _ = Just "numeric"

instance HasColumnType Word16 where
  defaultColumnType _ = SqlStdType $ numericType (Just (5, Nothing))
  defaultTypeCast _ = Just "numeric"

instance HasColumnType Word32 where
  defaultColumnType _ = SqlStdType $ numericType (Just (10, Nothing))
  defaultTypeCast _ = Just "numeric"

instance HasColumnType Word64 where
  defaultColumnType _ = SqlStdType $ numericType (Just (20, Nothing))
  defaultTypeCast _ = Just "numeric"

instance HasColumnType Text where
  defaultColumnType _ = SqlStdType $ varCharType Nothing Nothing
  defaultTypeCast _ = Just "character varying"

instance HasColumnType SqlBitString where
  defaultColumnType _ = SqlStdType $ varBitType Nothing
  defaultTypeCast _ = Just "bit"

instance HasColumnType ByteString where
  defaultColumnType _ = SqlStdType AST.DataTypeBinaryLargeObject

instance HasColumnType Double where
  defaultColumnType _ = SqlStdType doubleType
  defaultTypeCast _ = Just "double precision"

instance HasColumnType Scientific where
  defaultColumnType _ = SqlStdType $ numericType (Just (20, Just 10))
  defaultTypeCast _ = Just "numeric"

instance HasColumnType Day where
  defaultColumnType _ = SqlStdType dateType
  defaultTypeCast _ = Just "date"

instance HasColumnType TimeOfDay where
  defaultColumnType _ = SqlStdType $ timeType Nothing False
  defaultTypeCast _ = Just "time without time zone"

instance HasColumnType Bool where
  defaultColumnType _ = SqlStdType booleanType
  defaultTypeCast _ = Just "boolean"

instance HasColumnType LocalTime where
  defaultColumnType _ = SqlStdType $ timestampType Nothing False
  defaultTypeCast _ = Just "timestamp without time zone"

instance HasColumnType UTCTime where
  defaultColumnType _ = SqlStdType $ timestampType Nothing True
  defaultTypeCast _ = Just "timestamp with time zone"

instance HasColumnType UUID where
  defaultColumnType _ = PgSpecificType PgUuid
  defaultTypeCast _ = Just "uuid"

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
-- Support for 'SqlSerial'. \"SERIAL\" is treated by Postgres as syntactic sugar for:
---
-- CREATE SEQUENCE tablename_colname_seq;
-- CREATE TABLE tablename (
--     colname integer DEFAULT nextval('tablename_colname_seq') NOT NULL
-- );
-- ALTER SEQUENCE tablename_colname_seq OWNED BY tablename.colname;
--
-- Historically this was treated as a richer type (i.e. a 'PgSpecificType PgSerial') which had the advantage
-- of being able, for example, to track down when a column type changed so that we were able to drop the
-- relevant sequence if needed. However, this created problems when reconciling the 'Schema' type with the
-- one from the DB in case this type appeared \"behind\" a 'PrimaryKey' constraint. In that case it appeared
-- in the 'Schema' as a 'PgSerial' but in reality that should have been simply an integer. This led to the
-- creation of an auxiliary \"companion type\" concept which was making the overall complication ever so
-- slightly more complicated. Using just 'intType' here simplifies everything, at the cost of not-so-precise
-- \"resource tracking\" (i.e. created-but-now-unused requences remains in the DB).
instance (Integral ty, HasColumnType ty) => HasColumnType (SqlSerial ty) where
  defaultColumnType _ = defaultColumnType (Proxy @ty)

instance HasCompanionSequence' 'True (SqlSerial a) where
  hasCompanionSequence' Proxy Proxy tName cname =
    let s = Nothing
     in Just ((s, Sequence tName cname), Autoincrement s)

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

-- For now a `DbEnum` is isomorphic to a `varCharType`, as we don't have enough information on the Postgres
-- side to reconstruct the enumerated values.
instance (Show a, Typeable a, Enum a, Bounded a) => HasColumnType (DbEnum a) where
  defaultColumnType _ = SqlStdType $ varCharType Nothing Nothing
  defaultTypeCast _ = Just "character varying"

--
-- support for oid
--

instance HasColumnType Psql.Oid where
  defaultColumnType _ = PgSpecificType PgOid
