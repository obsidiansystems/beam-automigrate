{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- | This module provides an 'AnnotatedDatabaseSettings' type to be used as a drop-in replacement for the
-- standard 'DatabaseSettings'. Is it possible to \"downcast\" an 'AnnotatedDatabaseSettings' to a standard
-- 'DatabaseSettings' simply by calling 'deAnnotateDatabase'.
module Database.Beam.AutoMigrate.Annotated
  ( -- * User annotations
    Annotation (..),

    -- * Annotating a 'DatabaseSettings'
    AnnotatedDatabaseSettings,
    AnnotatedDatabaseEntity (..),
    IsAnnotatedDatabaseEntity (..),
    TableSchema,
    TableFieldSchema (..),
    FieldSchema (..),
    dbAnnotatedSchema,
    dbAnnotatedConstraints,
    annotatedDescriptor,
    defaultTableSchema,
    GDefaultTableSchema(..),

    -- * Downcasting annotated types
    lowerEntityDescriptor,
    deannotate,

    -- * Specifying constraints
    -- $specifyingConstraints
    annotateTableFields,

    -- * Specifying Column constraints
    -- $specifyingColumnConstraints
    defaultsTo,

    -- * Specifying Table constraints
    -- $specifyingTableConstraints
    UniqueConstraint (..),

    -- ** Unique constraint
    uniqueConstraintOn,

    -- ** Foreign key constraint
    ForeignKeyConstraint (..),
    foreignKeyOnPk,
    foreignKeyOn,
    foreignKeyOnNullable,
    foreignKeyOnWithOptions,

    -- * Other types and functions
    TableKind,
    DatabaseKind,

    -- * Ports from Beam
    zipTables,
    GZipDatabase,

    -- * Internals
    pgDefaultConstraint,

    -- * whatever, everthing.
    module Database.Beam.AutoMigrate.Annotated,
  )
where

import Data.Kind
import Data.Monoid (Endo (..))
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Database.Beam as Beam
import Database.Beam.AutoMigrate.Compat
import Database.Beam.AutoMigrate.Types
import Data.Default.Class (def)
import Database.Beam.AutoMigrate.Util
import Database.Beam.Backend.SQL (HasSqlValueSyntax (..), displaySyntax)
import Database.Beam.Postgres (Postgres)
import qualified Database.Beam.Postgres.Syntax as Pg
import Database.Beam.Query (QExpr)
import Database.Beam.Schema.Tables
  ( DatabaseEntity,
    DatabaseEntityDefaultRequirements,
    DatabaseEntityDescriptor,
    DatabaseEntityRegularRequirements,
    EntityModification (..),
    FieldModification (..),
    IsDatabaseEntity,
    Nullable,
    PrimaryKey,
    TableEntity,
    dbEntityDescriptor,
    dbEntityName,
    dbTableSettings,
  )
import GHC.Generics as Generic
import Control.Lens (Getter, (^.))
import qualified Control.Lens as Lens
import Unsafe.Coerce (unsafeCoerce)

--
-- Annotating a 'DatabaseSettings' with meta information.
--

-- | To make kind signatures more readable.
type DatabaseKind = (Type -> Type) -> Type

-- | To make kind signatures more readable.
type TableKind = (Type -> Type) -> Type

-- | A user-defined annotation. Currently the only possible annotation is the ability to specify for which
-- tables the FK-discovery algorithm is \"turned\" off.
data Annotation where
  -- | Specifies that the given 'TableKind' (i.e. a table) has user-specified FK constraints. This is
  -- useful in case of ambiguity, i.e. when the automatic FK-discovery algorithm is not capable to
  -- infer the correct 'ForeignKey' constraints for a 'Table'. This can happen when the 'PrimaryKey' type
  -- family is not injective, which means there are multiple tables of table @FooT@ in the DB. Consider a
  -- situation where we have a table @BarT@ having a field of type @barField :: PrimaryKey FooT f@ but
  -- (crucially) there are two tables with type @f (TableEntity FooT)@ in the final database. In this
  -- circumstance the FK-discovery algorithm will bail out with a (static) error, and this is where this
  -- annotation comes into play: it allows us to selectively \"disable\" the discovery for the given
  -- table(s), and manually override the FKs.
  --
  -- /Caveat emptor/: Due to what we said earlier (namely that we cannot enforce that tables are not
  -- repeated multiple times within a DB) there might be situations where also the specified 'TableKind'
  -- is not unique. In this case the annotation would affect all the tables of the same type, but that is
  -- usually unavoidable, as the ambiguity was already present the minute we introduced in the DB two tables
  -- of the same type, and so it makes sense for the user to fully resolve the ambiguity manually.
  UserDefinedFk :: TableKind -> Annotation

-- | Zip tables together. Unfortunately we cannot reuse the stock 'zipTables' from 'beam-core', because it
-- works by supplying a rank-2 function with 'IsDatabaseEntity' and 'DatabaseEntityRegularRequirements' as
-- witnesses, we we need the annotated counterparts instead.
--
-- This function can be written without the need of a typeclass, but alas it requires the /unexported/
-- 'GZipDatabase' from 'beam-core', so we had to re-implement this ourselves for now.
zipTables ::
  ( Generic (db f),
    Generic (db g),
    Generic (db h),
    Monad m,
    GZipDatabase be f g h (Rep (db f)) (Rep (db g)) (Rep (db h))
  ) =>
  Proxy be ->
  (forall tbl. (IsAnnotatedDatabaseEntity be tbl, AnnotatedDatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl)) ->
  db f ->
  db g ->
  m (db h)
-- We need the pattern type signature on 'combine' to get around a type checking bug in GHC 8.0.1.
-- In future releases, we will switch to the standard forall.
zipTables be combine (f :: db f) (g :: db g) =
  refl $ \h ->
    to <$> gZipDatabase (Proxy @f, Proxy @g, h, be) combine (from f) (from g)
  where
    -- For GHC 8.0.1 renamer bug
    refl :: (Proxy h -> m (db h)) -> m (db h)
    refl fn = fn Proxy

-- | See above on why this class has been re-implemented.
class GZipDatabase be f g h x y z where
  gZipDatabase ::
    Monad m =>
    (Proxy f, Proxy g, Proxy h, Proxy be) ->
    (forall tbl. (IsAnnotatedDatabaseEntity be tbl, AnnotatedDatabaseEntityRegularRequirements be tbl) => f tbl -> g tbl -> m (h tbl)) ->
    x () ->
    y () ->
    m (z ())

instance GZipDatabase be f g h x y z => GZipDatabase be f g h (M1 a b x) (M1 a b y) (M1 a b z) where
  gZipDatabase p combine ~(M1 f) ~(M1 g) = M1 <$> gZipDatabase p combine f g

instance
  ( GZipDatabase be f g h ax ay az,
    GZipDatabase be f g h bx by bz
  ) =>
  GZipDatabase be f g h (ax :*: bx) (ay :*: by) (az :*: bz)
  where
  gZipDatabase p combine ~(ax :*: bx) ~(ay :*: by) = do
    a <- gZipDatabase p combine ax ay
    b <- gZipDatabase p combine bx by
    pure (a :*: b)

instance
  ( IsAnnotatedDatabaseEntity be tbl,
    AnnotatedDatabaseEntityRegularRequirements be tbl
  ) =>
  GZipDatabase be f g h (K1 Generic.R (f tbl)) (K1 Generic.R (g tbl)) (K1 Generic.R (h tbl))
  where
  gZipDatabase _ combine ~(K1 x) ~(K1 y) =
    K1 <$> combine x y

instance
  ( Beam.Database be db,
    Generic (db f),
    Generic (db g),
    Generic (db h),
    GZipDatabase be f g h (Rep (db f)) (Rep (db g)) (Rep (db h))
  ) =>
  GZipDatabase be f g h (K1 Generic.R (db f)) (K1 Generic.R (db g)) (K1 Generic.R (db h))
  where
  gZipDatabase _ combine ~(K1 x) ~(K1 y) =
    K1 <$> zipTables (Proxy :: Proxy be) combine x y

--
-- An annotated Database settings.
--

-- | An 'AnnotatedDatabaseSettings' is similar in spirit to a @beam-core@ 'DatabaseSettings', but it
-- embellish the latter with extra metadata this library can use to derive more information about the input
-- DB, like table and column constraints.
type AnnotatedDatabaseSettings be db = db (AnnotatedDatabaseEntity be db)

-- | An 'AnnotatedDatabaseEntity' wraps the underlying 'DatabaseEntity' together with an annotated
-- description called 'AnnotatedDatabaseEntityDescriptor', which is once again similar to the standard
-- 'DatabaseEntityDescriptor' from Beam.
--
-- An 'AnnotatedDatabaseEntityDescriptor' is not a concrete type, but rather a data family provided by the
-- 'IsAnnotatedDatabaseEntity'.
data AnnotatedDatabaseEntity be (db :: (* -> *) -> *) entityType where
  AnnotatedDatabaseEntity ::
    (IsAnnotatedDatabaseEntity be entityType, IsDatabaseEntity be entityType) =>
    AnnotatedDatabaseEntityDescriptor be entityType ->
    DatabaseEntity be db entityType ->
    AnnotatedDatabaseEntity be db entityType

class IsDatabaseEntity be entityType => IsAnnotatedDatabaseEntity be entityType where
  data AnnotatedDatabaseEntityDescriptor be entityType :: *
  type AnnotatedDatabaseEntityDefaultRequirements be entityType :: Constraint
  type AnnotatedDatabaseEntityRegularRequirements be entityType :: Constraint

  dbAnnotatedEntityAuto ::
    AnnotatedDatabaseEntityRegularRequirements be entityType =>
    DatabaseEntityDescriptor be entityType ->
    AnnotatedDatabaseEntityDescriptor be entityType

instance
  IsDatabaseEntity be (TableEntity tbl) =>
  IsAnnotatedDatabaseEntity be (TableEntity tbl)
  where
  data AnnotatedDatabaseEntityDescriptor be (TableEntity tbl) where
    AnnotatedDatabaseTable ::
      Beam.Table tbl =>
      { dbAnnotatedSchema :: TableSchema tbl,
        dbAnnotatedConstraints :: TableConstraints
      } ->
      AnnotatedDatabaseEntityDescriptor be (TableEntity tbl)
  type
    AnnotatedDatabaseEntityDefaultRequirements be (TableEntity tbl) =
      (DatabaseEntityDefaultRequirements be (TableEntity tbl))
  type
    AnnotatedDatabaseEntityRegularRequirements be (TableEntity tbl) =
      ( DatabaseEntityRegularRequirements be (TableEntity tbl),
        GDefaultTableSchema (Rep (TableSchema tbl) ()) (Rep (Beam.TableSettings tbl) ()),
        Generic (TableSchema tbl),
        Generic (Beam.TableSettings tbl)
      )

  dbAnnotatedEntityAuto edesc = AnnotatedDatabaseTable (defaultTableSchema . dbTableSettings $ edesc) noTableConstraints



_dbAnnotatedSchema :: Lens.Lens' (AnnotatedDatabaseEntityDescriptor be (TableEntity tbl)) (TableSchema tbl)
_dbAnnotatedSchema = \a2fb s -> (\b -> s {dbAnnotatedSchema = b}) <$> a2fb (dbAnnotatedSchema s)
{-# INLINE _dbAnnotatedSchema #-}

_dbAnnotatedConstraints :: Lens.Lens' (AnnotatedDatabaseEntityDescriptor be (TableEntity tbl)) TableConstraints
_dbAnnotatedConstraints = \a2fb s -> (\b -> s {dbAnnotatedConstraints = b}) <$> a2fb (dbAnnotatedConstraints s)
{-# INLINE _dbAnnotatedConstraints #-}

-- | A 'Getter' to get a plain 'DatabaseEntityDescriptor' from an 'AnnotatedDatabaseEntity'.
lowerEntityDescriptor :: Getter (AnnotatedDatabaseEntity be db entityType) (DatabaseEntityDescriptor be entityType)
lowerEntityDescriptor = Lens.to (\(AnnotatedDatabaseEntity _ e) -> e ^. dbEntityDescriptor)

annotatedDescriptor :: Getter (AnnotatedDatabaseEntity be db entityType) (AnnotatedDatabaseEntityDescriptor be entityType)
annotatedDescriptor = Lens.to (\(AnnotatedDatabaseEntity e _) -> e)

deannotate :: Getter (AnnotatedDatabaseEntity be db entityType) (DatabaseEntity be db entityType)
deannotate = Lens.to (\(AnnotatedDatabaseEntity _ e) -> e)

-- | A table schema.
type TableSchema tbl =
  tbl (TableFieldSchema tbl)

-- | A schema for a field within a given table
data TableFieldSchema (tbl :: (* -> *) -> *) ty where
  TableFieldSchema ::
    { tableFieldName :: ColumnName,
      tableFieldSchema :: FieldSchema ty
    } ->
    TableFieldSchema tbl ty

data FieldSchema ty where
  FieldSchema ::
    ColumnType ->
    ColumnConstraints ->
    FieldSchema ty

deriving instance Show (FieldSchema ty)

--
-- Deriving a 'TableSchema'.
--

class GDefaultTableSchema x y where
  gDefTblSchema :: Proxy x -> y -> x

instance GDefaultTableSchema (x p) (y p) => GDefaultTableSchema (D1 f x p) (D1 f y p) where
  gDefTblSchema (Proxy :: Proxy (D1 f x p)) (M1 y) =
    M1 $ gDefTblSchema (Proxy :: Proxy (x p)) y

instance GDefaultTableSchema (x p) (y p) => GDefaultTableSchema (C1 f x p) (C1 f y p) where
  gDefTblSchema (Proxy :: Proxy (C1 f x p)) (M1 y) =
    M1 $ gDefTblSchema (Proxy :: Proxy (x p)) y

instance
  (GDefaultTableSchema (a p) (c p), GDefaultTableSchema (b p) (d p)) =>
  GDefaultTableSchema ((a :*: b) p) ((c :*: d) p)
  where
  gDefTblSchema (Proxy :: Proxy ((a :*: b) p)) (c :*: d) =
    gDefTblSchema (Proxy :: Proxy (a p)) c
      :*: gDefTblSchema (Proxy :: Proxy (b p)) d

instance
  ( SchemaConstraint (Beam.TableField tbl ty) ~ ColumnConstraints,
    HasSchemaConstraints (Beam.TableField tbl ty),
    HasColumnType ty
  ) =>
  GDefaultTableSchema
    (S1 f (K1 Generic.R (TableFieldSchema tbl ty)) p)
    (S1 f (K1 Generic.R (Beam.TableField tbl ty)) p)
  where
  gDefTblSchema (_ :: Proxy (S1 f (K1 Generic.R (TableFieldSchema tbl ty)) p)) (M1 (K1 fName)) = M1 (K1 s)
    where
      s = TableFieldSchema (ColumnName $ fName ^. Beam.fieldName) defaultFieldSchema
      defaultFieldSchema =
        FieldSchema
          (defaultColumnType (Proxy @ty))
          (schemaConstraints (Proxy @(Beam.TableField tbl ty)))

-- | Instance where /g/ is things like a 'PrimaryKey' or a /mixin/.
instance
  ( Generic (g (Beam.TableField tbl2)),
    Generic (g (TableFieldSchema tbl2)),
    GDefaultTableSchema
      (Rep (g (TableFieldSchema tbl2)) ())
      (Rep (g (Beam.TableField tbl2)) ())
  ) =>
  GDefaultTableSchema
    (S1 f (K1 Generic.R (g (TableFieldSchema tbl2))) ())
    (S1 f (K1 Generic.R (g (Beam.TableField tbl2))) ())
  where
  gDefTblSchema (_ :: Proxy (S1 f (K1 Generic.R (g (TableFieldSchema tbl2))) ())) (M1 (K1 fName)) =
    M1 (K1 $ to' $ gDefTblSchema Proxy (from' fName))

-- | Instance for things like 'Nullable (TableFieldSchema tbl)'.
instance
  ( Generic (PrimaryKey tbl1 (g (Beam.TableField tbl2))),
    Generic (PrimaryKey tbl1 (g (TableFieldSchema tbl2))),
    GDefaultTableSchema
      (Rep (PrimaryKey tbl1 (g (TableFieldSchema tbl2))) ())
      (Rep (PrimaryKey tbl1 (g (Beam.TableField tbl2))) ())
  ) =>
  GDefaultTableSchema
    (S1 f (K1 Generic.R (PrimaryKey tbl1 (g (TableFieldSchema tbl2)))) p)
    (S1 f (K1 Generic.R (PrimaryKey tbl1 (g (Beam.TableField tbl2)))) p)
  where
  gDefTblSchema (_ :: Proxy (S1 f (K1 Generic.R (PrimaryKey tbl1 (g (TableFieldSchema tbl2)))) p)) (M1 (K1 fName)) =
    M1 (K1 $ to' $ gDefTblSchema Proxy (from' fName))

defaultTableSchema ::
  forall tbl.
  ( GDefaultTableSchema (Rep (TableSchema tbl) ()) (Rep (Beam.TableSettings tbl) ()),
    Generic (TableSchema tbl),
    Generic (Beam.TableSettings tbl)
  ) =>
  Beam.TableSettings tbl ->
  TableSchema tbl
defaultTableSchema tSettings =
  to $ gDefTblSchema (Proxy :: Proxy (Rep (TableSchema tbl) ())) (from' tSettings)

from' :: Generic a => a -> Rep a ()
from' = from

to' :: Generic a => Rep a () -> a
to' = to

--
-- Annotating 'Table's and 'Field's after the default 'AnnotatedDatabaseSettings' has been instantiated.
--

-- $specifyingConstraints
-- Once an 'AnnotatedDatabaseSettings' has been acquired, the user is able to customise the default
-- medatata associated with it. In order to do so, one can reuse the existing machinery from Beam, in
-- particular the `withDbModification`. For example:
--
-- > annotatedDB :: AnnotatedDatabaseSettings Postgres FlowerDB
-- > annotatedDB = defaultAnnotatedDbSettings flowerDB `withDbModification` dbModification
-- >   { dbFlowers   = annotateTableFields tableModification { flowerDiscounted = defaultsTo (val_ $ Just True)
-- >                                                         , flowerPrice = defaultsTo (val_ $ Just 10.0)
-- >                                                         }
-- >                <> uniqueFields [U (addressPostalCode . addressRegion . flowerAddress)]
-- >   , dbLineItems = annotateTableFields tableModification { lineItemDiscount = defaultsTo (val_ $ Just False) }
-- >                <> uniqueFields [U lineItemFlowerID, U lineItemOrderID, U lineItemQuantity]
-- >   , dbOrders = annotateTableFields tableModification { orderTime = defaultsTo (cast_ currentTimestamp_ utctime) }
-- >              <> foreignKeyOnPk (dbFlowers flowerDB) orderFlowerIdRef Cascade Restrict
-- >              <> uniqueFields [U (addressPostalCode . addressRegion . orderAddress)]
-- >   }
--
-- Refer to the rest of the documentation for this module for more information about 'annotateTableFields',
-- 'uniqueFields' and 'foreignKeyOnPk'.

-- | Annotate the table fields for a given 'AnnotatedDatabaseEntity'. Refer to the $specifyingConstraints
-- section for an example.
annotateTableFields ::
  tbl (FieldModification (TableFieldSchema tbl)) ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
annotateTableFields modFields =
  EntityModification
    ( Endo
        ( \(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) ->
            AnnotatedDatabaseEntity
              ( tbl
                  { dbAnnotatedSchema = Beam.withTableModification modFields (dbAnnotatedSchema tbl)
                  }
              )
              e
        )
    )

--
-- Specifying default values (Postgres-specific)
--

-- $specifyingColumnConstraints
-- Due to the fact most column constraints can span /multiple/ columns (think about @UNIQUE@ or
-- @FOREIGN KEY@) the only constraint associated to a 'TableFieldSchema' we allow to customise at the
-- \"single-column-granularity\" is @DEFAULT@.

-- | Specify a default value for an entity. The relevant migration will generate an associated SQL
-- @DEFAULT@. This function accepts any Beam's expression that also the standard 'field' machinery would
-- accept, for example:
--
-- > defaultsTo (val_ $ Just 10)
defaultsTo ::
  (HasColumnType ty, HasSqlValueSyntax Pg.PgValueSyntax ty) =>
  (forall ctx s. Beam.QGenExpr ctx Postgres s ty) ->
  FieldModification (TableFieldSchema tbl) ty
defaultsTo tyVal = FieldModification $ \old ->
  case tableFieldSchema old of
    FieldSchema ty c ->
      old
        { tableFieldSchema =
            FieldSchema ty $ c {columnDefault = Just (pgDefaultConstraint tyVal) }
        }

-- | Postgres-specific function to convert any 'QGenExpr' into a meaningful 'PgExpressionSyntax', so
-- that it can be rendered inside a 'Default' column constraint.
pgDefaultConstraint ::
  forall ty.
  (HasColumnType ty, HasSqlValueSyntax Pg.PgValueSyntax ty) =>
  (forall ctx s. Beam.QGenExpr ctx Postgres s ty) ->
  DefaultConstraint
pgDefaultConstraint tyVal =
  let syntaxFragment = T.pack . displaySyntax . Pg.fromPgExpression $ defaultTo_ tyVal
      dVal = case defaultTypeCast (Proxy @ty) of
        Nothing -> syntaxFragment
        Just tc | T.head syntaxFragment == '\'' -> syntaxFragment <> "::" <> tc
        -- NOTE(and) Special-case handling for CURRENT_TIMESTAMP. See issue #31.
        Just tc | syntaxFragment == "CURRENT_TIMESTAMP" -> "(" <> syntaxFragment <> ")::" <> tc
        Just tc -> "'" <> syntaxFragment <> "'::" <> tc
   in DefaultExpr dVal
  where
    -- NOTE(adn) We are unfortunately once again forced to copy and paste some code from beam-migrate.
    -- In particular, `beam-migrate` wraps the returning 'QExpr' into a 'DefaultValue' newtype wrapper,
    -- which only purpose is to define an instance for 'FieldReturnType' (cfr.
    -- /Database.Beam.AutoMigrate.SQL.Tables/) and the underlying 'BeamSqlBackendExpressionSyntax' is used to
    -- call 'columnSchemaSyntax', which is then used in /their own/ definition of `FieldSchema`, which we
    -- don't follow.
    -- NOTE(adn) It's unclear what \"t\" stands for here, probably \"TablePrefix\". Not documented in
    -- `beam-migrate` itself.
    defaultTo_ :: (forall s. QExpr Postgres s a) -> Pg.PgExpressionSyntax
    defaultTo_ (Beam.QExpr e) = e "t"

--
-- Specifying uniqueness constraints
--

-- $specifyingTableConstraints
-- Is it possible to annotate an 'AnnotatedDatabaseEntity' with @UNIQUE@ and @FOREIGN KEY@ constraints.

data UniqueConstraint (tbl :: ((* -> *) -> *)) where
  -- | Use this to \"tag\" a standard Beam 'TableField' selector or 'PrimaryKey'.
  U :: HasColumnNames entity tbl => (tbl (Beam.TableField tbl) -> entity) -> UniqueConstraint tbl

-- | Given a list of 'TableField' selectors wrapped in a 'UniqueConstraint' type constructor, it adds
-- to the relevant 'AnnotatedDatabaseEntity' a new @UNIQUE@ 'TableConstraint' composed by /all/ the
-- fields specified. To put it differently, every call to 'uniqueConstraintOn' generates a /separate/
-- @UNIQUE@ constraint composed by the listed fields.
-- If a 'PrimaryKey' is passed as input, it will desugar under the hood into as many columns as
-- the primary key refers to.
uniqueConstraintOn ::
  [UniqueConstraint tbl] ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
uniqueConstraintOn us = uniqueConstraintOnWithOptions us def

-- | Given a list of 'TableField' selectors wrapped in a 'UniqueConstraint' type constructor, it adds
-- to the relevant 'AnnotatedDatabaseEntity' a new @UNIQUE@ 'TableConstraint' composed by /all/ the
-- fields specified. To put it differently, every call to 'uniqueConstraintOn' generates a /separate/
-- @UNIQUE@ constraint composed by the listed fields.
-- If a 'PrimaryKey' is passed as input, it will desugar under the hood into as many columns as
-- the primary key refers to.
uniqueConstraintOnWithOptions ::
  [UniqueConstraint tbl] ->
  UniqueConstraintOptions ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
uniqueConstraintOnWithOptions us opts =
  EntityModification
    ( Endo
        ( \(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) ->
            AnnotatedDatabaseEntity
              ( tbl Lens.& Lens.over (_dbAnnotatedConstraints . _uniqueConstraints) (<> M.singleton (mkUniqueConstraint e us) opts)
              )
              e
        )
    )

mkUniqueConstraint :: DatabaseEntity be db (TableEntity tbl) -> [UniqueConstraint tbl] -> Unique
mkUniqueConstraint e us =
  let cols = concatMap (\case (U f) -> colNames (tableSettings e) f) us
  in Unique (S.fromList cols)

--
-- Specifying FK constrainst
--

data ForeignKeyConstraint (tbl :: ((* -> *) -> *)) (tbl' :: ((* -> *) -> *)) where
  References ::
    Beam.Beamable (PrimaryKey tbl') =>
    (tbl (Beam.TableField tbl) -> PrimaryKey tbl' (Beam.TableField tbl)) ->
    (tbl' (Beam.TableField tbl') -> Beam.Columnar Beam.Identity (Beam.TableField tbl' ty)) ->
    ForeignKeyConstraint tbl tbl'



-- | Special-case combinator to use when defining FK constraints referencing the /primary key/ of the
-- target table.
foreignKeyOnPk ::
  ( Beam.Beamable (PrimaryKey tbl'),
    Beam.Beamable tbl',
    Beam.Table tbl',
    PrimaryKey tbl' f ~ PrimaryKey tbl' g
  ) =>
  -- | The 'DatabaseEntity' of the /referenced/ table.
  DatabaseEntity be db (TableEntity tbl') ->
  -- | A function yielding a 'PrimaryKey'. This is usually a record field of the table
  -- you want to define the FK /for/, and it must have /PrimaryKey externalTable f/ as
  -- its column-tag.
  (tbl (Beam.TableField tbl) -> PrimaryKey tbl' (Beam.TableField tbl)) ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
foreignKeyOnPk externalEntity ourColumn = foreignKeyOnWithOptions externalEntity ourColumn Beam.primaryKey def


foreignKeyOn ::
  ( Beam.Beamable tbl'
  , Beam.Beamable key
  ) =>
  DatabaseEntity be db (TableEntity tbl') ->
  (tbl (Beam.TableField tbl) -> key (Beam.TableField tbl)) ->
  (tbl' (Beam.TableField tbl') -> key (Beam.TableField tbl')) ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
foreignKeyOn externalEntity ourColumn theirColumn =
  foreignKeyOnWithOptions externalEntity ourColumn theirColumn def

mkForeignKeyConstraint
  :: (Beam.Beamable key, Beam.Beamable tbl')
  => DatabaseEntity be db (TableEntity tbl)
  -> (tbl (Beam.TableField tbl) -> key (Beam.TableField tbl))
  -> DatabaseEntity be db (TableEntity tbl')
  -> (tbl' (Beam.TableField tbl') -> key (Beam.TableField tbl'))
  -> ForeignKey
mkForeignKeyConstraint e ourColumn externalEntity theirColumn =
  let colPairs =
        zipWith
          (,)
          (fieldAsColumnNames (ourColumn (tableSettings e)))
          (fieldAsColumnNames (theirColumn (tableSettings externalEntity)))
      tName = externalEntity ^. dbEntityDescriptor . dbEntityName
  in ForeignKey (TableName tName) (S.fromList colPairs)

foreignKeyOnWithOptions ::
  ( Beam.Beamable tbl'
  , Beam.Beamable key
  ) =>
  DatabaseEntity be db (TableEntity tbl') ->
  (tbl (Beam.TableField tbl) -> key (Beam.TableField tbl)) ->
  (tbl' (Beam.TableField tbl') -> key (Beam.TableField tbl')) ->
  -- | Foreign Key Constraitn Options
  ForeignKeyConstraintOptions ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
foreignKeyOnWithOptions externalEntity ourColumn theirColumn options =
  EntityModification
    ( Endo
        ( \(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) ->
            AnnotatedDatabaseEntity
              ( Lens.over
                  (_dbAnnotatedConstraints . _foreignKeyConstraints)
                  (<> M.singleton (mkForeignKeyConstraint e ourColumn externalEntity theirColumn) options)
                  tbl
              )
              e
        )
    )

mkNullableForeignKeyConstraint
  :: (Beam.Beamable key, Beam.Beamable tbl')
  => DatabaseEntity be db (TableEntity tbl)
  -> (tbl (Beam.TableField tbl) -> key (Nullable (Beam.TableField tbl)))
  -> DatabaseEntity be db (TableEntity tbl')
  -> (tbl' (Beam.TableField tbl') -> key (Beam.TableField tbl'))
  -> ForeignKey
mkNullableForeignKeyConstraint e ourColumn externalEntity theirColumn =
  let colPairs =
        zipWith
          (,)
          (fieldAsColumnNames (coerceTheNullableAway $ ourColumn (tableSettings e)))
          (fieldAsColumnNames (theirColumn (tableSettings externalEntity)))
      coerceTheNullableAway :: key (Nullable (Beam.TableField tbl)) -> key (Beam.TableField tbl)
      coerceTheNullableAway = (unsafeCoerce :: key (Nullable (Beam.TableField tbl)) -> key (Beam.TableField tbl))
      tName = externalEntity ^. dbEntityDescriptor . dbEntityName
  in ForeignKey (TableName tName) (S.fromList colPairs)


foreignKeyOnNullable ::
  forall key tbl tbl' be db.
  ( Beam.Beamable tbl'
  , Beam.Beamable key
  ) =>
  DatabaseEntity be db (TableEntity tbl') ->
  -- | fields on local table
  (tbl (Beam.TableField tbl) -> key (Nullable (Beam.TableField tbl))) ->
  -- | fields on referenced table
  (tbl' (Beam.TableField tbl') -> key (Beam.TableField tbl')) ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
foreignKeyOnNullable externalEntity ourColumn theirColumn =
  foreignKeyOnNullableWithOptions externalEntity ourColumn theirColumn def

foreignKeyOnNullableWithOptions ::
  forall key tbl tbl' be db.
  ( Beam.Beamable tbl'
  , Beam.Beamable key
  ) =>
  DatabaseEntity be db (TableEntity tbl') ->
  -- | fields on local table
  (tbl (Beam.TableField tbl) -> key (Nullable (Beam.TableField tbl))) ->
  -- | fields on referenced table
  (tbl' (Beam.TableField tbl') -> key (Beam.TableField tbl')) ->
  ForeignKeyConstraintOptions ->
  EntityModification (AnnotatedDatabaseEntity be db) be (TableEntity tbl)
foreignKeyOnNullableWithOptions externalEntity ourColumn theirColumn options =
  EntityModification
    ( Endo
        ( \(AnnotatedDatabaseEntity tbl@(AnnotatedDatabaseTable {}) e) ->
            AnnotatedDatabaseEntity
              ( tbl Lens.& Lens.over (_dbAnnotatedConstraints . _foreignKeyConstraints) (<> M.singleton (mkNullableForeignKeyConstraint e ourColumn externalEntity theirColumn) options)
              )
              e
        )
    )
