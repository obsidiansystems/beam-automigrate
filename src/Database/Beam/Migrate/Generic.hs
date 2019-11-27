{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Generic where

import           Database.Beam.Migrate.Util     ( pkFieldNames )
import           Database.Beam.Migrate.Types
import           Data.Proxy
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Lens.Micro                     ( (^.) )

import           GHC.Generics

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

class GSchema x where
    gSchema :: x p -> Schema

-- Table-specific classes

class GTables x where
    gTables :: x p -> Tables

class GTableEntry x where
    gTableEntry :: x p -> (TableName, Table)

class GTable x where
    gTable :: x p -> Table

-- Column-specific classes

class GColumns x where
    gColumns :: x p -> Columns

class GColumnEntry x where
    gColumnEntry :: x p -> (ColumnName, Column)

class GColumn x where
    gColumn :: x p -> Column

--
-- Deriving information about 'Schema's
--

instance GSchema x => GSchema (D1 f x) where
  gSchema (M1 x) = gSchema x

instance (Constructor f, GTables x) => GSchema (C1 f x) where
  gSchema (M1 x) = Schema { schemaTables = gTables x }

--
-- Deriving information about 'Table's.
--

instance GTableEntry (S1 f x) => GTables (S1 f x) where
  gTables = uncurry M.singleton . gTableEntry

instance GTableEntry x => GTableEntry (S1 f x) where
  gTableEntry (M1 x) = gTableEntry x

instance (GTableEntry a, GTables b) => GTables (a :*: b) where
  gTables (a :*: b) = uncurry M.singleton (gTableEntry a) <> gTables b

instance ( IsDatabaseEntity be (TableEntity tbl)
         , GColumns (Rep (TableSettings tbl))
         , Generic (TableSettings tbl)
         , Beam.Table tbl
         )
  => GTableEntry (K1 R (Beam.DatabaseEntity be db (TableEntity tbl))) where
  gTableEntry (K1 entity) =
    let tName = entity ^. dbEntityDescriptor . dbEntityName
        pks   = S.singleton (PrimaryKey (tName <> "_pkey") (S.fromList $ pkFieldNames entity))
        columns = gColumns . from $ (dbTableSettings $ entity ^. dbEntityDescriptor)
    in  (TableName tName, Table pks columns )

instance GColumns x => GColumns (D1 f x) where
  gColumns (M1 x) = gColumns x

instance GColumns x => GColumns (C1 f x) where
  gColumns (M1 x) = gColumns x

instance (GColumns a, GColumns b) => GColumns (a :*: b) where
  gColumns (a :*: b) = gColumns a <> gColumns b


--
-- Column entries
--

instance HasDefaultSqlDataType t => GColumns (S1 m (K1 R (Beam.TableField e t))) where
  gColumns (M1 (K1 e)) =
    let colName = ColumnName $ e ^. Beam.fieldName
        col     = Column (defaultSqlDataType (Proxy @t) False) (S.singleton NotNull) -- TODO(adn) support constraints
    in  M.singleton colName col

instance ( GColumns (Rep (PrimaryKey f (Beam.TableField t)))
         , Generic (PrimaryKey f (Beam.TableField t))
         , Beamable (PrimaryKey f)
         )
    => GColumns (S1 m (K1 R (PrimaryKey f (Beam.TableField t)))) where
  gColumns (M1 (K1 e)) = gColumns (from e)
