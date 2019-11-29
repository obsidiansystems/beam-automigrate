{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Beam.Migrate.Postgres
  ( getSchema
  )
where

import           Data.String
import           Control.Monad.State
import           Data.Maybe                               ( fromMaybe )
import           Data.Bits                                ( (.&.)
                                                          , shiftR
                                                          )
import           Data.Foldable                            ( foldlM )
import           Data.List                                ( foldl' )
import qualified Data.Vector                             as V
import qualified Data.Map.Strict                         as M
import qualified Data.Set                                as S
import           Data.Map                                 ( Map )
import           Data.Set                                 ( Set )
import           Data.Text                                ( Text )
import qualified Data.Text.Encoding                      as TE
import qualified Data.Text                               as T
import           Data.ByteString                          ( ByteString )

import           Database.Beam.Backend.SQL         hiding ( tableName )
import qualified Database.PostgreSQL.Simple              as Pg
import           Database.PostgreSQL.Simple.FromRow       ( FromRow(..)
                                                          , field
                                                          )
import           Database.PostgreSQL.Simple.FromField     ( FromField(..)
                                                          , fromField
                                                          )
import qualified Database.PostgreSQL.Simple.Types        as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static
                                                         as Pg

import           Database.Beam.Migrate.Types

--
-- Necessary types to make working with the underlying raw SQL a bit more pleasant
--

data SqlRawOtherConstraintType = 
    SQL_raw_pk
  | SQL_raw_unique
  deriving (Show, Eq)

data SqlOtherConstraint = SqlOtherConstraint
    { sqlCon_table :: TableName
    , sqlCon_constraint_type :: SqlRawOtherConstraintType
    , sqlCon_fk_colums :: V.Vector ColumnName
    , sqlCon_name :: Text
    } deriving (Show, Eq)

instance Pg.FromRow SqlOtherConstraint where
  fromRow = SqlOtherConstraint <$> (fmap TableName field)
                               <*> field
                               <*> (fmap (V.map ColumnName) field) 
                               <*> field

data SqlForeignConstraint = SqlForeignConstraint
    { sqlFk_foreign_table   :: TableName
    , sqlFk_primary_table   :: TableName
    , sqlFk_fk_columns      :: V.Vector ColumnName
    -- ^ The columns in the /foreign/ table.
    , sqlFk_pk_columns      :: V.Vector ColumnName
    -- ^ The columns in the /current/ table.
    , sqlFk_name            :: Text
    } deriving (Show, Eq)

instance Pg.FromRow SqlForeignConstraint where
  fromRow = SqlForeignConstraint <$> (fmap TableName field)
                                 <*> (fmap TableName field) 
                                 <*> (fmap (V.map ColumnName) field) 
                                 <*> (fmap (V.map ColumnName) field) 
                                 <*> field

instance FromField SqlRawOtherConstraintType where
  fromField f dat = do
      t <- fromField f dat
      case t of
        "PRIMARY KEY" -> pure SQL_raw_pk
        "UNIQUE"      -> pure SQL_raw_unique
        _ -> fail ("Unexpected costraint type: " <> t)

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

-- | Return all foreign key constraints for /all/ 'Table's.

foreignKeysQ :: Pg.Query
foreignKeysQ = fromString $ unlines
  [ "SELECT kcu.table_name as foreign_table,"
  , "       rel_kcu.table_name as primary_table,"
  , "       array_agg(kcu.column_name)::text[] as fk_columns,"
  , "       array_agg(rel_kcu.column_name)::text[] as pk_columns,"
  , "       kcu.constraint_name as cname"
  , "FROM information_schema.table_constraints tco"
  , "JOIN information_schema.key_column_usage kcu"
  , "          on tco.constraint_schema = kcu.constraint_schema"
  , "          and tco.constraint_name = kcu.constraint_name"
  , "JOIN information_schema.referential_constraints rco"
  , "          on tco.constraint_schema = rco.constraint_schema"
  , "          and tco.constraint_name = rco.constraint_name"
  , "JOIN information_schema.key_column_usage rel_kcu"
  , "          on rco.unique_constraint_schema = rel_kcu.constraint_schema"
  , "          and rco.unique_constraint_name = rel_kcu.constraint_name"
  , "          and kcu.ordinal_position = rel_kcu.ordinal_position"
  , "GROUP BY foreign_table, primary_table, cname"
  , "ORDER BY primary_table"
  ]

-- | Return /all other constraints that are not FKs/ (i.e. 'PRIMARY KEY', 'UNIQUE', etc) for all the tables.
otherConstraintsQ :: Pg.Query
otherConstraintsQ = fromString $ unlines
  [ "SELECT kcu.table_name as foreign_table,"
  , "       tco.constraint_type as ctype,"
  , "       array_agg(kcu.column_name)::text[] as fk_columns,"
  , "       kcu.constraint_name as cname"
  , "FROM information_schema.table_constraints tco"
  , "RIGHT JOIN information_schema.key_column_usage kcu"
  , "           on tco.constraint_schema = kcu.constraint_schema"
  , "           and tco.constraint_name = kcu.constraint_name"
  , "LEFT JOIN  information_schema.referential_constraints rco"
  , "           on tco.constraint_schema = rco.constraint_schema"
  , "           and tco.constraint_name = rco.constraint_name"
  , "LEFT JOIN  information_schema.key_column_usage rel_kcu"
  , "           on rco.unique_constraint_schema = rel_kcu.constraint_schema"
  , "           and rco.unique_constraint_name = rel_kcu.constraint_name"
  , "           and kcu.ordinal_position = rel_kcu.ordinal_position"
  , "WHERE tco.constraint_type = 'PRIMARY KEY' OR tco.constraint_type = 'UNIQUE'"
  , "GROUP BY foreign_table, ctype, cname"
  , "ORDER BY ctype"
  ]

-- | Return all \"action types\" for /all/ the constraints.
referenceActionsQ :: Pg.Query
referenceActionsQ = fromString $ unlines
  [ "SELECT c.conname, c. confdeltype, c.confupdtype FROM "
  , "(SELECT r.conrelid, r.confrelid, unnest(r.conkey) AS conkey, unnest(r.confkey) AS confkey, r.conname, r.confupdtype, r.confdeltype "
  , "FROM pg_catalog.pg_constraint r WHERE r.contype = 'f') AS c "
  , "INNER JOIN pg_attribute a_parent ON a_parent.attnum = c.confkey AND a_parent.attrelid = c.confrelid "
  , "INNER JOIN pg_class cl_parent ON cl_parent.oid = c.confrelid "
  , "INNER JOIN pg_namespace sch_parent ON sch_parent.oid = cl_parent.relnamespace "
  , "INNER JOIN pg_attribute a_child ON a_child.attnum = c.conkey AND a_child.attrelid = c.conrelid "
  , "INNER JOIN pg_class cl_child ON cl_child.oid = c.conrelid "
  , "INNER JOIN pg_namespace sch_child ON sch_child.oid = cl_child.relnamespace "
  , "WHERE sch_child.nspname = current_schema() ORDER BY c.conname "
  ]

-- | Connects to a running PostgreSQL database and extract the relevant 'Schema' out of it.
getSchema :: Pg.Connection -> IO Schema
getSchema conn = do
  allConstraints <- getAllConstraints conn
  tables         <- Pg.fold_ conn userTablesQ mempty (getTable allConstraints)
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
                  Column cType (maybe nullConstraint (mappend nullConstraint) (M.lookup columnName allConstraints))
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

type AllTableConstraints = Map TableName (Set TableConstraint)
type AllConstraints = (AllTableConstraints, AllColumnConstraints)
type AllColumnConstraints = Map ColumnName (Set ColumnConstraint)

getAllConstraints :: Pg.Connection -> IO AllConstraints
getAllConstraints conn = do
    allActions <- mkActions <$> Pg.query_ conn referenceActionsQ
    allForeignKeys <- Pg.fold_ conn foreignKeysQ mempty (\acc -> pure . addFkConstraint allActions acc)
    Pg.fold_ conn otherConstraintsQ allForeignKeys (\acc -> pure . addOtherConstraint acc)
  where
      addFkConstraint :: ReferenceActions 
                      -> AllConstraints 
                      -> SqlForeignConstraint
                      -> AllConstraints
      addFkConstraint actions st SqlForeignConstraint{..} = flip execState st $ do
        let currentTable = sqlFk_foreign_table
        let columnSet = S.fromList $ zip (V.toList sqlFk_fk_columns) (V.toList sqlFk_pk_columns)
        -- Here we need to add two constraints: one for 'ForeignKey' and one for
        -- 'IsForeignKeyOf'.
        let (onDelete, onUpdate) = 
                case M.lookup sqlFk_name (getActions actions) of
                  Nothing -> (NoAction, NoAction)
                  Just a  -> (actionOnDelete a, actionOnUpdate a)
        addTableConstraint sqlFk_primary_table (IsForeignKeyOf sqlFk_foreign_table columnSet) 
        addTableConstraint currentTable (ForeignKey sqlFk_name sqlFk_primary_table columnSet onDelete onUpdate)

      addOtherConstraint :: AllConstraints 
                         -> SqlOtherConstraint
                         -> AllConstraints
      addOtherConstraint st SqlOtherConstraint{..} = flip execState st $ do
          let currentTable = sqlCon_table
          let columnSet = S.fromList . V.toList $ sqlCon_fk_colums
          case sqlCon_constraint_type of
            SQL_raw_unique -> addTableConstraint currentTable (Unique sqlCon_name columnSet)
            SQL_raw_pk -> addTableConstraint currentTable (PrimaryKey sqlCon_name columnSet)


newtype ReferenceActions = ReferenceActions { getActions :: Map Text Actions }
newtype RefEntry = RefEntry { unRefEntry :: (Text, ReferenceAction, ReferenceAction) }

mkActions :: [RefEntry] -> ReferenceActions
mkActions = ReferenceActions . M.fromList . map ((\(a,b,c) -> (a, Actions b c)) . unRefEntry)

instance Pg.FromRow RefEntry where
  fromRow = fmap RefEntry ((,,) <$> field 
                                <*> (fmap mkAction field) 
                                <*> (fmap mkAction field))

data Actions = Actions {
    actionOnDelete :: ReferenceAction
  , actionOnUpdate :: ReferenceAction
  }


mkAction :: Text -> ReferenceAction
mkAction c = case c of
  "a" -> NoAction
  "r" -> Restrict
  "c" -> Cascade
  "n" -> SetNull
  "d" -> SetDefault
  _ -> error . T.unpack $ "unknown reference action type: " <> c


--
-- Useful combinators to add constraints for a column or table if already there.
--

addTableConstraint :: TableName
                   -> TableConstraint 
                   -> State AllConstraints ()
addTableConstraint tName cns =
  modify' (\(tcon, ccon) -> (M.alter (\case
                               Nothing -> Just $ S.singleton cns
                               Just ss -> Just $ S.insert cns ss) tName tcon
                            , ccon)
          )

addColumnConstraint :: ColumnName
                    -> ColumnConstraint 
                    -> State AllConstraints ()
addColumnConstraint cName cns =
  modify' (\(tcon, ccon) -> (tcon
                            , M.alter (\case
                              Nothing -> Just $ S.singleton cns
                              Just ss -> Just $ S.insert cns ss) cName ccon)
          )

