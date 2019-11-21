{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Migrate.Generic where

import           Database.Beam.Migrate.Util     ( pkAsColumnNames )
import           Database.Beam.Migrate.Types

import qualified Data.Map.Strict               as M
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


{- Machinery to derive a 'Schema' from a 'DatabaseSettings'. -}

class GSchema x where
    gSchema :: x p -> Schema

class GSchemaTables x where
    gSchemaTables :: x p -> Tables

class GSchemaTableEntry x where
    gSchemaTableEntry :: x p -> (TableName, Table)

class GSchemaTable x where
    gSchemaTable :: x p -> Table

-- | Due to the fact that 'PrimaryKey' expansion can give multiple ColumnName,
-- we return a [(ColumnName, Column)].
class GSchemaColumnEntries x where
    gSchemaColumnEntries :: x p -> [(ColumnName, Column)]

instance GSchema x => GSchema (D1 f x) where
  gSchema (M1 x) = gSchema x
instance (Constructor f, GSchemaTables x) => GSchema (C1 f x) where
  gSchema (M1 x) = Schema { schemaTables = gSchemaTables x }

instance (GSchemaTableEntry a, GSchemaTables b) => GSchemaTables (a :*: b) where
  gSchemaTables (a :*: b) = uncurry M.singleton (gSchemaTableEntry a) <> gSchemaTables b
instance GSchemaTableEntry (S1 f x) => GSchemaTables (S1 f x) where
  gSchemaTables = uncurry M.singleton . gSchemaTableEntry

instance GSchemaTableEntry x => GSchemaTableEntry (S1 f x) where
  gSchemaTableEntry (M1 x) = gSchemaTableEntry x
instance ( IsDatabaseEntity be (TableEntity tbl)
         , GSchemaTable (Rep (TableSettings tbl))
         , Generic (TableSettings tbl)
         )
  => GSchemaTableEntry (K1 R (Beam.DatabaseEntity be db (TableEntity tbl))) where
  gSchemaTableEntry (K1 entity) =
    let tName = entity ^. dbEntityDescriptor . dbEntityName
    in  (TableName tName, gSchemaTable . from $ (dbTableSettings $ entity ^. dbEntityDescriptor))

instance GSchemaTable x => GSchemaTable (D1 f x) where
  gSchemaTable (M1 x) = gSchemaTable x

instance GSchemaTable x => GSchemaTable (C1 f x) where
  gSchemaTable (M1 x) = gSchemaTable x

instance (GSchemaColumnEntries a, GSchemaTable b) => GSchemaTable (a :*: b) where
  gSchemaTable (a :*: b) = Table (M.fromList (gSchemaColumnEntries a)) <> gSchemaTable b

instance GSchemaTable (S1 m (K1 R (Beam.TableField e t))) where
  gSchemaTable (M1 (K1 e)) =
    let colName = ColumnName $ e ^. Beam.fieldName
    in  Table $ M.singleton colName (Column () noColumnConstraints)

instance GSchemaColumnEntries (S1 m (K1 R (Beam.TableField e t))) where
  gSchemaColumnEntries (M1 (K1 e)) =
    let colName = ColumnName $ e ^. Beam.fieldName in [(colName, Column () noColumnConstraints)]

-- TODO(adn) Not quite correct as far as the 'PrimaryKey' is concerned.
instance Beamable (PrimaryKey f)
    => GSchemaColumnEntries (S1 m (K1 R (PrimaryKey f (Beam.TableField t)))) where
  gSchemaColumnEntries (M1 (K1 e)) =
    let colNames = pkAsColumnNames e
        cols     = repeat (Column () noColumnConstraints) -- TODO(adn) fixme
    in  zip colNames cols
