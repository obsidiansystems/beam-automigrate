{-# LANGUAGE FlexibleContexts     #-}
module Database.Beam.Migrate.Util where

import           Lens.Micro                     ( (^.) )

import           Database.Beam.Schema           ( Beamable
                                                , PrimaryKey
                                                , TableEntity
                                                , TableSettings
                                                )
import qualified Database.Beam.Schema          as Beam
import           Database.Beam.Schema.Tables    ( Columnar'(..)
                                                , allBeamValues
                                                , dbEntityDescriptor
                                                , dbTableSettings
                                                , fieldName
                                                , primaryKey
                                                )

import           Database.Beam.Migrate.Types    ( ColumnName(..) )


-- | Extracts the 'TableSettings' out of the input 'DatabaseEntity'.
tableSettings :: Beam.DatabaseEntity be db (TableEntity tbl) -> TableSettings tbl
tableSettings entity = dbTableSettings $ entity ^. dbEntityDescriptor

-- | Extracts the primary key of a table as a list of 'ColumnName'.
pkFieldNames :: (Beamable (PrimaryKey tbl), Beam.Table tbl)
             => Beam.DatabaseEntity be db (TableEntity tbl)
             -> [ColumnName]
pkFieldNames entity =
  map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (primaryKey . tableSettings $ entity))

-- | Similar to 'pkFieldNames', but it works with an input 'PrimaryKey'.
pkAsColumnNames :: Beamable (PrimaryKey table) => PrimaryKey table (Beam.TableField c) -> [ColumnName]
pkAsColumnNames pk = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) pk)

-- | Returns /all/ the 'ColumnName's for a given 'DatabaseEntity'.
allColumnNames :: Beamable tbl => Beam.DatabaseEntity be db (TableEntity tbl) -> [ColumnName]
allColumnNames entity =
  let settings = dbTableSettings $ entity ^. dbEntityDescriptor
  in  map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) settings)
