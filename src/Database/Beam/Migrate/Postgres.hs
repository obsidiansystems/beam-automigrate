{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Beam.Migrate.Postgres
  ( getSchema
  )
where

import           Data.Function
import           Data.String
import           Data.Maybe                     ( fromMaybe )
import           Data.Bits                      ( (.&.)
                                                , shiftR
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Vector                   as V
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.List                      ( foldl', partition )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Data.ByteString                ( ByteString )

import           Database.Beam.Backend.SQL hiding (tableName)
import qualified Database.PostgreSQL.Simple    as Pg
import Database.PostgreSQL.Simple.FromRow    (FromRow(..), field)
import Database.PostgreSQL.Simple.FromField    (FromField(..), fromField)
import qualified Database.PostgreSQL.Simple.Types
                                               as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static
                                               as Pg

import           Database.Beam.Migrate.Types

--
-- Necessary types to make working with the underlying raw SQL a bit more pleasant
--

data SqlRawConstraintType = 
    SQL_raw_pk
  | SQL_raw_unique
  | SQL_raw_fk
  deriving (Show, Eq)

data SqlRawConstraint = SqlRawConstraint
    { sqlCon_name :: Text
    , sqlCon_ref_origin :: TableName
    , sqlCon_ref_target :: TableName
    , sqlCon_columns    :: V.Vector ColumnName
    , sqlCon_constraint_type :: SqlRawConstraintType
    } deriving (Show, Eq)

instance Pg.FromRow SqlRawConstraint where
  fromRow = SqlRawConstraint <$> field 
                             <*> (fmap TableName field) 
                             <*> (fmap TableName field) 
                             <*> (fmap (V.map ColumnName) field) 
                             <*> field

instance FromField SqlRawConstraintType where
  fromField f dat = do
      t <- fromField f dat
      case t of
        "PRIMARY KEY" -> pure SQL_raw_pk
        "UNIQUE"      -> pure SQL_raw_unique
        "FOREIGN KEY" -> pure SQL_raw_fk
        _ -> fail ("Uknown costraint type: " <> t)

--
-- Postgres queries to extract the schema out of the DB
--

-- | A SQL query to select all user's queries, skipping any beam-related tables (i.e. leftovers from
-- beam-migrate, for example).
userTablesQ :: Pg.Query
userTablesQ = fromString $ unlines
  [ "SELECT cl.oid, relname FROM pg_catalog.pg_class \"cl\" join pg_catalog.pg_namespace \"ns\" "
  , "on (ns.oid = relnamespace) where nspname = any (current_schemas(false)) and relkind='r' "
  , "and relname NOT LIKE 'beam_%'"
  ]

tableColumnsQ :: Pg.Query
tableColumnsQ = fromString $ unlines
  [ "SELECT attname, atttypid, atttypmod, attnotnull, pg_catalog.format_type(atttypid, atttypmod) "
  , "FROM pg_catalog.pg_attribute att WHERE att.attrelid=? AND att.attnum>0 AND att.attisdropped='f'"
  ]

-- | Returns the 'PrimaryKey's for /all/ tables.
primaryKeysQ :: Pg.Query
primaryKeysQ = fromString $ unlines
  [ "SELECT c.relname, array_agg(a.attname ORDER BY k.n ASC) FROM pg_index i CROSS JOIN unnest(i.indkey) "
  , "WITH ORDINALITY k(attid, n) JOIN pg_attribute a ON a.attnum=k.attid AND a.attrelid=i.indrelid JOIN "
  , "pg_class c ON c.oid=i.indrelid WHERE c.relkind='r' and relname NOT LIKE 'beam_%' AND i.indisprimary "
  , "GROUP BY relname, i.indrelid"
  ]

-- | Return all constraints for a given 'Table'.
constraintsQ :: Pg.Query
constraintsQ = fromString $ unlines
  [ "SELECT tc.constraint_name, tc.table_name as ref_origin, u.table_name as ref_target, "
  , "array_agg(column_name)::text[] as column_names, constraint_type FROM information_schema.table_constraints tc "
  , "JOIN information_schema.constraint_column_usage u ON tc.constraint_catalog=u.constraint_catalog AND "
  , "tc.constraint_schema=u.constraint_schema AND tc.constraint_name=u.constraint_name WHERE u.table_name=? "
  , "GROUP BY tc.constraint_name, ref_origin, ref_target, constraint_type"
  ]

-- | Return all \"action types\" for a given constraint, for example 'ON DELETE RESTRICT'.
referenceActionQ :: Pg.Query
referenceActionQ = fromString $ unlines
  [ "SELECT c.conname, sch_parent.nspname, cl_parent.relname, c. confdeltype, c.confupdtype, a_child.attname AS child, a_parent.attname AS parent FROM "
  , "(SELECT r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname, r.confupdtype, r.confdeltype "
  , "FROM pg_catalog.pg_constraint r WHERE r.contype = 'f') AS c "
  , "INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid "
  , "INNER JOIN pg_class cl_parent ON cl_parent.oid = c.confrelid "
  , "INNER JOIN pg_namespace sch_parent ON sch_parent.oid = cl_parent.relnamespace "
  , "INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid "
  , "INNER JOIN pg_class cl_child ON cl_child.oid = c.conrelid "
  , "INNER JOIN pg_namespace sch_child ON sch_child.oid = cl_child.relnamespace "
  , "WHERE sch_child.nspname = current_schema() AND cl_child.relname = ? AND c.conname = ?"
  , "ORDER BY c.conname "
  ]

-- | Connects to a running PostgreSQL database and extract the relevant 'Schema' out of it.
getSchema :: Pg.Connection -> IO Schema
getSchema conn = do
  pgTables <- Pg.query_ conn userTablesQ
  pgPks    <- toPkLookupTable <$> Pg.query_ conn primaryKeysQ
  tables   <- foldlM (getTable pgPks) mempty pgTables
  pure $ Schema tables

  where
    getTable :: Map TableName (Set ColumnName) -> Tables -> (Pg.Oid, Text) -> IO Tables
    getTable pkLookupTable allTables (oid, TableName -> tName) = do
      pgColumns <- Pg.query conn tableColumnsQ (Pg.Only oid)
      _ <- getTableConstraints conn tName
      newTable  <-
        Table (maybe noTableConstraints (S.singleton . PrimaryKey) (M.lookup tName pkLookupTable))
          <$> foldlM getColumns mempty pgColumns
      pure $ M.insert tName newTable allTables

    getColumns :: Columns -> (ByteString, Pg.Oid, Int, Bool, ByteString) -> IO Columns
    getColumns c (attname, atttypid, atttypmod, attnotnull, format_type) = do
      let mbPrecision = if atttypmod == -1 then Nothing else Just (atttypmod - 4)
      case pgTypeToColumnType atttypid mbPrecision of
        Just cType -> do
          let mbConstraints = if attnotnull then Just $ S.fromList [NotNull] else Nothing
          let newColumn     = Column cType (fromMaybe noColumnConstraints mbConstraints)
          pure $ M.insert (ColumnName (TE.decodeUtf8 attname)) newColumn c
        Nothing ->
          fail
            $  "Couldn't convert pgType "
            <> show format_type
            <> " of field "
            <> show attname
            <> " into a valid ColumnType."
    -- Builds a lookup table from a 'TableName' to the set of column names which constitutes a 'PrimaryKey' for
    -- a particular table. Potentially large for big DBs (> 10k tables).
    -- NOTE(adn) Currently the program is optimised for speed, not memory, and this is why this 'toPkLooupTable'
    -- is passed around in 'getTableSchema' as part of a fold, which means it cannot be garbage-collected until
    -- the fold has finished.
    toPkLookupTable :: [(Text, V.Vector Text)] -> Map TableName (Set ColumnName)
    toPkLookupTable pks = foldl' go mempty pks
      where
        go :: Map TableName (Set ColumnName) -> (Text, V.Vector Text) -> Map TableName (Set ColumnName)
        go m (tName, columns) =
          M.insert (TableName tName) (S.fromList . map ColumnName . V.toList $ columns) m

--
-- Postgres type mapping
--

-- | Tries to convert from a Postgres' 'Oid' into 'ColumnType'.
-- Mostly taken from [beam-migrate](Database.Beam.Postgres.Migrate).
pgTypeToColumnType :: Pg.Oid -> Maybe Int -> Maybe ColumnType
pgTypeToColumnType oid width
  | Pg.typoid Pg.int2 == oid
  = Just smallIntType
  | Pg.typoid Pg.int4 == oid
  = Just intType
  | Pg.typoid Pg.int8 == oid
  = Just bigIntType
  | Pg.typoid Pg.bpchar == oid
  = Just (charType (fromIntegral <$> width) Nothing)
  | Pg.typoid Pg.varchar == oid
  = Just (varCharType (fromIntegral <$> width) Nothing)
  | Pg.typoid Pg.bit == oid
  = Just (bitType (fromIntegral <$> width))
  | Pg.typoid Pg.varbit == oid
  = Just (varBitType (fromIntegral <$> width))
  | Pg.typoid Pg.numeric == oid
  = let decimals = fromMaybe 0 width .&. 0xFFFF
        prec     = (fromMaybe 0 width `shiftR` 16) .&. 0xFFFF
    in  Just (numericType (Just (fromIntegral prec, Just (fromIntegral decimals))))
  | Pg.typoid Pg.float4 == oid
  = Just (floatType (fromIntegral <$> width))
  | Pg.typoid Pg.float8 == oid
  = Just doubleType
  | Pg.typoid Pg.date == oid
  = Just dateType
  | Pg.typoid Pg.text == oid
  = Just characterLargeObjectType
  | Pg.typoid Pg.bytea == oid
  = Just binaryLargeObjectType
  | Pg.typoid Pg.bool == oid
  = Just booleanType
  | Pg.typoid Pg.time == oid
  = Just (timeType Nothing False)
  | Pg.typoid Pg.timestamp == oid
  = Just (timestampType Nothing False)
  | Pg.typoid Pg.timestamptz == oid
  = Just (timestampType Nothing True)
  | otherwise 
  = Nothing

--
-- Constraints discovery
--

-- We consider table constraints primary & foreign keys constraints, as they can span
-- multiple columns and are generally easier to process at the table-level.
isTableConstraint :: SqlRawConstraint -> Bool
isTableConstraint (sqlCon_constraint_type -> ctype)
  | ctype == SQL_raw_pk || ctype == SQL_raw_fk = True
  | otherwise = False

-- | Given an input 'TableName', it returns a pair of the \"table-level\" constraints (for example primary
-- keys or foreign keys spanning multiple columns) and \"column-level\" constraints (e.g. 'UNIQUE, 'NOT NULL',
-- etc).
getTableConstraints :: Pg.Connection 
                    -> TableName 
                    -> IO (Map TableName (Set TableConstraint), Map ColumnName (Set ColumnConstraint))
getTableConstraints conn (tableName -> currentTable) = do
    allConstraints <- Pg.query conn constraintsQ (Pg.Only currentTable)

    let (tableLevelRaw, columnLevelRaw) = partition isTableConstraint allConstraints

    columnLevel <- getColumnLevelConstraints columnLevelRaw
    tableLevel  <- getTableLevelConstraints  (TableName currentTable) tableLevelRaw

    putStrLn "=0="
    print currentTable
    print tableLevel
    print columnLevel

    pure (tableLevel, columnLevel)


getTableLevelConstraints :: TableName 
                         -- ^ The 'Table' we are currently processing.
                         -> [SqlRawConstraint] 
                         -> IO (Map TableName (Set TableConstraint))
getTableLevelConstraints currentTable = foldlM go mempty
  where
    go :: Map TableName (Set TableConstraint)
       -> SqlRawConstraint
       -> IO (Map TableName (Set TableConstraint))
    go m SqlRawConstraint{..} = do
        let columnSet = S.fromList . V.toList $ sqlCon_columns
        pure $ case sqlCon_constraint_type of
          SQL_raw_pk -> addTableConstraint currentTable (PrimaryKey columnSet) m
          SQL_raw_fk -> do
              -- Here we need to add two constraints: one for 'ForeignKey' and one for
              -- 'IsForeignKeyOf'.
              -- TODO(adn) actions.
              m & addTableConstraint currentTable (IsForeignKeyOf sqlCon_ref_origin columnSet) 
                . addTableConstraint sqlCon_ref_origin (ForeignKey columnSet Nothing Nothing)
          _ -> m


getColumnLevelConstraints :: [SqlRawConstraint]
                          -> IO (Map ColumnName (Set ColumnConstraint))
getColumnLevelConstraints = foldlM go mempty 
  where
    go :: Map ColumnName (Set ColumnConstraint)
       -> SqlRawConstraint 
       -> IO (Map ColumnName (Set ColumnConstraint))
    go m SqlRawConstraint{..} = do
        pure $ case sqlCon_constraint_type of
          SQL_raw_unique -> addColumnConstraint (V.head sqlCon_columns) Unique m
          _ -> m

--
-- Useful combinators to add constraints for a column or table if already there.
--

addTableConstraint :: TableName
                   -> TableConstraint 
                   -> Map TableName (Set TableConstraint)
                   -> Map TableName (Set TableConstraint)
addTableConstraint tName cns =
  M.alter (\case
      Nothing -> Just $ S.singleton cns
      Just ss -> Just $ S.insert cns ss) tName

addColumnConstraint :: ColumnName
                    -> ColumnConstraint 
                    -> Map ColumnName (Set ColumnConstraint)
                    -> Map ColumnName (Set ColumnConstraint)
addColumnConstraint cName cns =
  M.alter (\case
      Nothing -> Just $ S.singleton cns
      Just ss -> Just $ S.insert cns ss) cName

