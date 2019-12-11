{-# LANGUAGE UndecidableInstances #-}
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


--
-- Retrieving all the column names for a beam entity.
--

class HasColumnNames entity tbl where
  colNames :: (tbl (Beam.TableField tbl)) -> (tbl (Beam.TableField tbl) -> entity) -> [ColumnName]

instance Beam.Beamable (PrimaryKey tbl)
    => HasColumnNames (PrimaryKey tbl (Beam.TableField c)) tbl where
    colNames field fn = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (fn field))

instance Beam.Beamable (PrimaryKey tbl)
    => HasColumnNames (PrimaryKey tbl (Beam.TableField c)) tbl' where
    colNames field fn = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (fn field))

instance HasColumnNames (Beam.TableField tbl ty) tbl where
    colNames field fn = [ColumnName $ (fn field ^. Beam.fieldName)]

--
-- General utility functions
--

-- | Extracts the 'TableSettings' out of the input 'DatabaseEntity'.
tableSettings :: Beam.DatabaseEntity be db (TableEntity tbl) -> TableSettings tbl
tableSettings entity = dbTableSettings $ entity ^. dbEntityDescriptor

-- | Extracts the primary key of a table as a list of 'ColumnName'.
pkFieldNames :: (Beamable (PrimaryKey tbl), Beam.Table tbl)
             => Beam.DatabaseEntity be db (TableEntity tbl)
             -> [ColumnName]
pkFieldNames entity =
  map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) (primaryKey . tableSettings $ entity))

-- | Similar to 'pkFieldNames', but it works on any entity that derives 'Beamable'.
fieldAsColumnNames :: Beamable tbl => tbl (Beam.TableField c) -> [ColumnName]
fieldAsColumnNames field = map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) field)

-- | Returns /all/ the 'ColumnName's for a given 'DatabaseEntity'.
allColumnNames :: Beamable tbl => Beam.DatabaseEntity be db (TableEntity tbl) -> [ColumnName]
allColumnNames entity =
  let settings = dbTableSettings $ entity ^. dbEntityDescriptor
  in  map ColumnName (allBeamValues (\(Columnar' x) -> x ^. fieldName) settings)
