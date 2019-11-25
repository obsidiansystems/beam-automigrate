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
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Bits                      ( (.&.)
                                                , shiftR
                                                )
import           Data.Foldable                  ( foldlM )
import qualified Data.Vector                   as V
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.List                      ( partition )
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import qualified Data.Text                     as T
import           Data.ByteString                ( ByteString )

import           Database.Beam.Backend.SQL
                                         hiding ( tableName )
import qualified Database.PostgreSQL.Simple    as Pg
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromField
                                                )
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
  [ "SELECT c.conname, c. confdeltype, c.confupdtype FROM "
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
  allTables <- Pg.query_ conn userTablesQ
  allConstraints <- foldlM (\(tConAcc, cConAcc) (_, tName) -> do
      (tCon, cCon) <- getTableConstraints conn (TableName tName)
      pure (tConAcc <> tCon, cConAcc <> cCon)
      ) mempty allTables
  tables   <- foldlM (getTable allConstraints) mempty allTables
  pure $ Schema tables

  where
    getTable :: AllConstraints -> Tables -> (Pg.Oid, Text) -> IO Tables
    getTable allConstraints allTables (oid, TableName -> tName) = do
      pgColumns <- Pg.query conn tableColumnsQ (Pg.Only oid)
      newTable  <-
        Table (fromMaybe noTableConstraints (M.lookup tName (fst allConstraints)))
          <$> foldlM (getColumns (snd allConstraints)) mempty pgColumns
      pure $ M.insert tName newTable allTables

    getColumns :: AllColumnConstraints -> Columns -> (ByteString, Pg.Oid, Int, Bool, ByteString) -> IO Columns
    getColumns allConstraints c (attname, atttypid, atttypmod, attnotnull, format_type) = do
      let mbPrecision = if atttypmod == -1 then Nothing else Just (atttypmod - 4)
      case pgTypeToColumnType atttypid mbPrecision of
        Just cType -> do
          let nullConstraint = 
                  if attnotnull then S.fromList [NotNull] else mempty
          let columnName = ColumnName (TE.decodeUtf8 attname)
          let newColumn  = 
                  Column cType (maybe noColumnConstraints (mappend nullConstraint) (M.lookup columnName allConstraints))
          pure $ M.insert columnName newColumn c
        Nothing ->
          fail
            $  "Couldn't convert pgType "
            <> show format_type
            <> " of field "
            <> show attname
            <> " into a valid ColumnType."

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

type AllConstraints = (Map TableName (Set TableConstraint), AllColumnConstraints)
type AllColumnConstraints = Map ColumnName (Set ColumnConstraint)

-- We consider table constraints primary & foreign keys constraints, as they can span
-- multiple columns and are generally easier to process at the table-level.
isTableConstraint :: SqlRawConstraint -> Bool
isTableConstraint (sqlCon_constraint_type -> ctype)
  | ctype == SQL_raw_pk || ctype == SQL_raw_fk = True
  | otherwise = False

-- | Given an input 'TableName', it returns a pair of the \"table-level\" constraints (for example primary
-- keys or foreign keys spanning multiple columns) and \"column-level\" constraints (e.g. 'UNIQUE, 'NOT NULL',
-- etc).
-- NOTE(adinapoli) This pair of collections can get very large for a large number of tables (> 10k), so we 
-- might want to optimise things further down the line.
getTableConstraints :: Pg.Connection 
                    -> TableName 
                    -> IO AllConstraints
getTableConstraints conn (tableName -> currentTable) = do
    allConstraints <- Pg.query conn constraintsQ (Pg.Only currentTable)

    let (tableLevelRaw, columnLevelRaw) = partition isTableConstraint allConstraints

    columnLevel <- getColumnLevelConstraints columnLevelRaw
    tableLevel  <- getTableLevelConstraints  conn (TableName currentTable) tableLevelRaw

    pure (tableLevel, columnLevel)


getTableLevelConstraints :: Pg.Connection
                         -> TableName 
                         -- ^ The 'Table' we are currently processing.
                         -> [SqlRawConstraint] 
                         -> IO (Map TableName (Set TableConstraint))
getTableLevelConstraints conn currentTable = foldlM go mempty
  where
    go :: Map TableName (Set TableConstraint)
       -> SqlRawConstraint
       -> IO (Map TableName (Set TableConstraint))
    go m SqlRawConstraint{..} = do
        let columnSet = S.fromList . V.toList $ sqlCon_columns
        case sqlCon_constraint_type of
          SQL_raw_pk -> pure $ addTableConstraint currentTable (PrimaryKey columnSet) m
          SQL_raw_fk -> do
              -- Here we need to add two constraints: one for 'ForeignKey' and one for
              -- 'IsForeignKeyOf'.
              (actions :: [(Text, Text, Text)]) 
                <- Pg.query conn referenceActionQ (tableName sqlCon_ref_origin, sqlCon_name)
              let (onDelete, onUpdate) = 
                      case listToMaybe actions of
                        Nothing -> (NoAction, NoAction)
                        Just (_, onDeleteAction, onUpdateAction) -> 
                            (mkAction onDeleteAction, mkAction onUpdateAction)
              pure $ m & addTableConstraint currentTable (IsForeignKeyOf sqlCon_ref_origin columnSet) 
                   . addTableConstraint sqlCon_ref_origin (ForeignKey columnSet onDelete onUpdate)
          _ -> pure m

    mkAction :: Text -> ReferenceAction
    mkAction c = case c of
      "a" -> NoAction
      "r" -> Restrict
      "c" -> Cascade
      "n" -> SetNull
      "d" -> SetDefault
      _ -> error . T.unpack $ "unknown reference action type: " <> c


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

