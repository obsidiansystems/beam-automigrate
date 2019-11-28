{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Database.Beam.Migrate.Generic where

import           Database.Beam.Migrate.Util     ( pkFieldNames )
import           Database.Beam.Migrate.Types
import           Data.Kind
import           Data.Proxy
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Lens.Micro                     ( (^.) )

import           GHC.Generics
import           GHC.TypeLits

import           Database.Beam.Schema           ( Beamable
                                                , PrimaryKey
                                                , TableEntity
                                                , TableSettings
                                                )
import qualified Database.Beam.Schema          as Beam
import           Database.Beam.Schema.Tables    ( IsDatabaseEntity
                                                , dbEntityDescriptor
                                                , dbEntityName
                                                , dbTableSettings
                                                )

import           Database.Beam.Migrate.Compat

{- Machinery to derive a 'Schema' from a 'DatabaseSettings'. -}

class GSchema be db x where
    gSchema :: Beam.DatabaseSettings be db -> x p -> Schema

-- Table-specific classes

class GTables be db x where
    gTables :: Beam.DatabaseSettings be db -> x p -> Tables

class GTableEntry be db x where
    gTableEntry :: Beam.DatabaseSettings be db -> x p -> (TableName, Table)

class GTable be db x where
    gTable :: Beam.DatabaseSettings be db -> x p -> Table

-- Column-specific classes

class GColumns x where
    gColumns :: x p -> Columns

class GTableConstraintColumns be db x where
    gTableConstraintsColumns :: Beam.DatabaseSettings be db -> [ColumnName] -> x p -> S.Set TableConstraint

class GColumnEntry x where
    gColumnEntry :: x p -> (ColumnName, Column)

class GTableConstraintColumnEntry be db x where
    gTableConstraintsEntry :: Beam.DatabaseSettings be db -> [ColumnName] -> x p -> S.Set TableConstraint

class GColumn x where
    gColumn :: x p -> Column

class GTableConstraintColumn be db x where
    gTableConstraintsColumn :: Beam.DatabaseSettings be db -> [ColumnName] -> x p -> S.Set TableConstraint

--
-- Deriving information about 'Schema's
--

instance GSchema be db x => GSchema be db (D1 f x) where
  gSchema db (M1 x) = gSchema db x

instance (Constructor f, GTables be db x) => GSchema be db (C1 f x) where
  gSchema db (M1 x) = Schema { schemaTables = gTables db x }

--
-- Deriving information about 'Table's.
--

instance GTableEntry be db (S1 f x) => GTables be db (S1 f x) where
  gTables db = uncurry M.singleton . gTableEntry db

instance GTableEntry be db x => GTableEntry be db (S1 f x) where
  gTableEntry db (M1 x) = gTableEntry db x

-- TODO: Check this instance
instance (GTableEntry be db a, GTables be db b) => GTables be db (a :*: b) where
  gTables db (a :*: b) = uncurry M.singleton (gTableEntry db a) <> gTables db b

instance ( IsDatabaseEntity be (TableEntity tbl)
         , GColumns (Rep (TableSettings tbl))
         , Generic (TableSettings tbl)
         , Beam.Table tbl
         , GTableConstraintColumns be db (Rep (TableSettings tbl))
         )
  => GTableEntry be db (K1 R (Beam.DatabaseEntity be' db' (TableEntity tbl))) where
  gTableEntry db (K1 entity) =
    let tName = entity ^. dbEntityDescriptor . dbEntityName
        pks   = S.singleton (PrimaryKey (tName <> "_pkey") (S.fromList $ pkFieldNames entity))
        columns = gColumns . from $ (dbTableSettings $ entity ^. dbEntityDescriptor)
        cNames  = M.keys columns
        constraints = gTableConstraintsColumns db cNames . from $ (dbTableSettings $ entity ^. dbEntityDescriptor)
    in  (TableName tName, Table (S.union pks constraints) columns)

instance GColumns x => GColumns (D1 f x) where
  gColumns (M1 x) = gColumns x

instance GTableConstraintColumns be db x => GTableConstraintColumns be db (D1 f x) where
  gTableConstraintsColumns db cnames (M1 x) = gTableConstraintsColumns db cnames x

instance GColumns x => GColumns (C1 f x) where
  gColumns (M1 x) = gColumns x

instance GTableConstraintColumns be db x => GTableConstraintColumns be db (C1 f x) where
  gTableConstraintsColumns db cnames (M1 x) = gTableConstraintsColumns db cnames x

instance (GColumns a, GColumns b) => GColumns (a :*: b) where
  gColumns (a :*: b) = gColumns a <> gColumns b

instance (GTableConstraintColumns be db a, GTableConstraintColumns be db b) => GTableConstraintColumns be db (a :*: b) where
  gTableConstraintsColumns db cnames (a :*: b) = S.union (gTableConstraintsColumns db cnames a) (gTableConstraintsColumns db cnames b)


--
-- Column entries
--

instance (HasSchemaConstraints (Beam.TableField e t), HasDefaultSqlDataType t)
  => GColumns (S1 m (K1 R (Beam.TableField e t))) where
  gColumns (M1 (K1 e)) =
    let colName = ColumnName $ e ^. Beam.fieldName
        col     = Column (defaultSqlDataType (Proxy @t) False)
                         (S.fromList (schemaConstraints (Proxy @(Beam.TableField e t))))
    in  M.singleton colName col

instance GTableConstraintColumns be db (S1 m (K1 R (Beam.TableField e t))) where
  gTableConstraintsColumns _db _cnames (M1 (K1 _)) = S.empty
    -- e :: Beam.TableField e t

-- TODO: It seems problematic to dictate the functor to be 'Beam.TableField t' here.
instance ( GColumns (Rep (PrimaryKey f (Beam.TableField t)))
         , Generic (PrimaryKey f (Beam.TableField t))
         , Beamable (PrimaryKey f)
         )
    => GColumns (S1 m (K1 R (PrimaryKey f (Beam.TableField t)))) where
  gColumns (M1 (K1 e)) = gColumns (from e)

instance ( Generic (Beam.DatabaseSettings be db)
         , GTableLookupSettings f (Rep (Beam.DatabaseSettings be db))
         ) => GTableConstraintColumns be db (S1 m (K1 R (PrimaryKey f (Beam.TableField t)))) where
  gTableConstraintsColumns db cnames (M1 (K1 _e)) =
    -- e :: PrimaryKey f (Beam.TableField t)
    S.singleton (ForeignKey (fst (gTableLookupSettings (Proxy @f) (from db))) (S.fromList cnames) {- TODO -} NoAction NoAction) -- unclear what the default should be
    -- | ForeignKey TableName (Set ColumnName) ReferenceAction {- onDelete -} ReferenceAction {- onUpdate -}

type DatabaseKind = (Type -> Type) -> Type
type TableKind = (Type -> Type) -> Type

-- We want a type class for the table lookup, because we want to return a
-- value-level table name based on the database settings!

class GTableLookupSettings (tbl :: TableKind) x where
  gTableLookupSettings :: Proxy tbl -> x p -> (TableName, [ColumnName])

class GTableLookupTables (tbl :: TableKind) (x :: Type -> Type) (k :: Type -> Type) where
  gTableLookupTables :: Proxy tbl -> x p -> k p -> (TableName, [ColumnName])

instance
  (GTableLookupSettings tbl x)
  => GTableLookupSettings tbl (D1 f x) where
  gTableLookupSettings tbl (M1 x) = gTableLookupSettings tbl x -- (TableName "foo", [])

instance
  (GTableLookupTables tbl x U1)
  => GTableLookupSettings tbl (C1 f x) where
  gTableLookupSettings tbl (M1 x) = gTableLookupTables tbl x U1

instance
  (GTableLookupTables tbl x k)
  => GTableLookupTables tbl (S1 f x) k where
  gTableLookupTables tbl (M1 x) k = gTableLookupTables tbl x k

instance
  ( GTableLookupTables tbl a (b :*: k)
  ) => GTableLookupTables tbl (a :*: b) k where
  gTableLookupTables tbl (a :*: b) k = gTableLookupTables tbl a (b :*: k)

instance
  ( GTableLookupTable (TestTableEqual tbl tbl') tbl k
  , Beamable tbl'
  ) =>
  GTableLookupTables tbl (K1 R (Beam.DatabaseEntity be db (TableEntity tbl'))) k where
  gTableLookupTables _tbl (K1 entity) k =
    let
      tName = entity ^. dbEntityDescriptor . dbEntityName
    in
      gTableLookupTable (Proxy @(TestTableEqual tbl tbl')) (Proxy @tbl) (TableName tName, []) k

type family TestTableEqual (tbl1 :: TableKind) (tbl2 :: TableKind) :: Bool where
  TestTableEqual tbl tbl = True
  TestTableEqual _   _   = False

class GTableLookupTable (b :: Bool) (tbl :: TableKind) (k :: Type -> Type) where
  gTableLookupTable :: Proxy b -> Proxy tbl -> (TableName, [ColumnName]) -> k p -> (TableName, [ColumnName])

instance GTableLookupTable True tbl k where
  gTableLookupTable _ _ r _ = r

instance TypeError (Text "lookup failed") => GTableLookupTable False tbl U1 where
  gTableLookupTable _ _ _ _ = error "impossible"

instance GTableLookupTables tbl k ks => GTableLookupTable False tbl (k :*: ks) where
  gTableLookupTable _ tbl _ (k :*: ks) =
    gTableLookupTables tbl k ks

-- type family TableLookup  (tbl :: Type -> Type) (db :: (Type -> Type) -> Type) :: [

{-
data ForeignKeyConstraint =
  ForeignKeyConstraint TableName [ColumnName] TableName

-- | To be applied to DatabaseSettings.
class GDiscoverForeignKeys x where
    gDiscoverForeignKeysSettings :: x p -> [ForeignKeyConstraint]

-- Table-specific classes

-- | To be applied to the tables type.
class GDiscoverForeignKeysTables x where
    gDiscoverForeignKeysTables :: x p -> [ForeignKeyConstraint]

class GDiscoverForeignKeysTableEntry x where
    gDiscoverForeignKeysTableEntry :: x p -> [ForeignKeyConstraint]

class GDiscoverForeignKeysTable x where
    gDiscoverForeignKeysTable :: x p -> [ForeignKeyConstraint]

-- Column-specific classes

class GDiscoverForeignKeysColumns x where
    gDiscoverForeignKeysColumns :: x p -> [ForeignKeyConstraint]

class GColumnEntry x where
    gColumnEntry :: x p -> (ColumnName, Column)

class GColumn x where
    gColumn :: x p -> Column

-}
