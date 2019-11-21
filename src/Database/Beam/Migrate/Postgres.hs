{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Beam.Migrate.Postgres (getSchema) where

import           Data.String
import           Data.Maybe (fromMaybe)
import Data.Bits ((.&.), shiftR)
import           Data.Foldable                  ( foldlM )
import qualified Data.Vector                   as V
import qualified Data.Map.Strict               as M
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.List                      ( foldl' )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Data.ByteString                ( ByteString )

import Database.Beam.Backend.SQL
import qualified Database.PostgreSQL.Simple    as Pg
import qualified Database.PostgreSQL.Simple.Types
                                               as Pg
import qualified Database.PostgreSQL.Simple.TypeInfo.Static
                                               as Pg

import           Database.Beam.Migrate.Types


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
      newTable  <-
        Table (maybe noSchemaConstraints (S.singleton . PrimaryKey) (M.lookup tName pkLookupTable))
          <$> foldlM getColumns mempty pgColumns
      pure $ M.insert tName newTable allTables

    getColumns :: Columns -> (ByteString, Pg.Oid, Int, Bool, ByteString) -> IO Columns
    getColumns c (attname, atttypid, atttypmod, _attnotnull, format_type) = do
      let mbPrecision =  if atttypmod == -1 then Nothing else Just (atttypmod - 4)
      case pgTypeToColumnType atttypid mbPrecision of
        Just cType -> do
          let newColumn = Column cType noSchemaConstraints
          pure $ M.insert (ColumnName (TE.decodeUtf8 attname)) newColumn c
        Nothing ->
            fail $ "Couldn't convert pgType " <> show format_type <> " of field " <> show attname <> " into a valid ColumnType."

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


-- | Tries to convert from a Postgres' 'Oid' into 'ColumnType'.
-- Mostly taken from [beam-migrate](Database.Beam.Postgres.Migrate).
pgTypeToColumnType :: Pg.Oid -> Maybe Int -> Maybe ColumnType
pgTypeToColumnType oid width
  | Pg.typoid Pg.int2    == oid = Just smallIntType
  | Pg.typoid Pg.int4    == oid = Just intType
  | Pg.typoid Pg.int8    == oid = Just bigIntType
  | Pg.typoid Pg.bpchar  == oid = Just (charType (fromIntegral <$> width) Nothing)
  | Pg.typoid Pg.varchar == oid = Just (varCharType (fromIntegral <$> width) Nothing)
  | Pg.typoid Pg.bit     == oid = Just (bitType (fromIntegral <$> width))
  | Pg.typoid Pg.varbit  == oid = Just (varBitType (fromIntegral <$> width))
  | Pg.typoid Pg.numeric == oid =
      let decimals = fromMaybe 0 width .&. 0xFFFF
          prec     = (fromMaybe 0 width `shiftR` 16) .&. 0xFFFF
      in Just (numericType (Just (fromIntegral prec, Just (fromIntegral decimals))))
  | Pg.typoid Pg.float4  == oid = Just (floatType (fromIntegral <$> width))
  | Pg.typoid Pg.float8  == oid = Just doubleType
  | Pg.typoid Pg.date    == oid = Just dateType
  -- We prefer using the standard beam names
  | Pg.typoid Pg.text    == oid = Just characterLargeObjectType
  | Pg.typoid Pg.bytea   == oid = Just binaryLargeObjectType
  | Pg.typoid Pg.bool    == oid = Just booleanType
  -- TODO timestamp prec
  | Pg.typoid Pg.time        == oid = Just (timeType Nothing False)
  | Pg.typoid Pg.timestamp   == oid = Just (timestampType Nothing False)
  | Pg.typoid Pg.timestamptz == oid = Just (timestampType Nothing True)

  -- | Pg.typoid Pg.float4  == oid = Just (floatType (fromIntegral <$> width))
  -- | Pg.typoid Pg.float8  == oid = Just doubleType
  -- | Pg.typoid Pg.date    == oid = Just dateType
  -- -- We prefer using the standard beam names
  -- | Pg.typoid Pg.text    == oid = Just characterLargeObjectType
  -- | Pg.typoid Pg.bytea   == oid = Just binaryLargeObjectType
  -- | Pg.typoid Pg.bool    == oid = Just booleanType
  -- Postgres specific datatypes, haskell versions
  -- NOTE(adn) For the sake of the prototype, let's not worry about this.
  -- | Pg.typoid Pg.uuid        == oid =
  --     Just $ HsDataType (hsVarFrom "uuid" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "UUID")
  --                               (importSome "Data.UUID.Types" [importTyNamed "UUID"]))
  --                       (pgDataTypeSerialized pgUuidType)
  -- | Pg.typoid Pg.money       == oid ->
  --     Just $ HsDataType (hsVarFrom "money" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "PgMoney")
  --                               (importSome "Database.Beam.Postgres" [importTyNamed "PgMoney"]))
  --                       (pgDataTypeSerialized pgMoneyType)
  -- | Pg.typoid Pg.json        == oid ->
  --     Just $ HsDataType (hsVarFrom "json" "Database.Beam.Postgres")
  --                       (HsType (tyApp (tyConNamed "PgJSON") [ tyConNamed "Value" ])
  --                               (importSome "Data.Aeson" [importTyNamed "Value"] <>
  --                                importSome "Database.Beam.Postgres" [importTyNamed "PgJSON"]))
  --                       (pgDataTypeSerialized pgJsonType)
  -- | Pg.typoid Pg.jsonb       == oid ->
  --     Just $ HsDataType (hsVarFrom "jsonb" "Database.Beam.Postgres")
  --                       (HsType (tyApp (tyConNamed "PgJSONB") [ tyConNamed "Value" ])
  --                               (importSome "Data.Aeson" [importTyNamed "Value"] <>
  --                                importSome "Database.Beam.Postgres" [importTyNamed "PgJSONB"]))
  --                       (pgDataTypeSerialized pgJsonType)
  -- | Pg.typoid pgTsVectorTypeInfo == oid ->
  --     Just $ HsDataType (hsVarFrom "tsvector" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "TsVector")
  --                               (importSome "Database.Beam.Postgres" [importTyNamed "TsVector"]))
  --                       (pgDataTypeSerialized pgTsVectorType)
  -- | Pg.typoid pgTsQueryTypeInfo == oid ->
  --     Just $ HsDataType (hsVarFrom "tsquery" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "TsQuery")
  --                               (importSome "Database.Beam.Postgres" [importTyNamed "TsQuery"]))
  --                       (pgDataTypeSerialized pgTsQueryType)
  -- | Pg.typoid Pg.point   == oid ->
  --     Just $ HsDataType (hsVarFrom "point" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "PgPoint")
  --                               (importSome "Database.Beam.Postgres" [ importTyNamed "PgPoint" ]))
  --                       (pgDataTypeSerialized pgPointType)
  -- | Pg.typoid Pg.line    == oid ->
  --     Just $ HsDataType (hsVarFrom "line" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "PgLine")
  --                               (importSome "Database.Beam.Postgres" [ importTyNamed "PgLine" ]))
  --                       (pgDataTypeSerialized pgLineType)
  -- | Pg.typoid Pg.lseg    == oid ->
  --     Just $ HsDataType (hsVarFrom "lineSegment" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "PgLineSegment")
  --                               (importSome "Database.Beam.Postgres" [ importTyNamed "PgLineSegment" ]))
  --                       (pgDataTypeSerialized pgLineSegmentType)
  -- | Pg.typoid Pg.box     == oid ->
  --     Just $ HsDataType (hsVarFrom "box" "Database.Beam.Postgres")
  --                       (HsType (tyConNamed "PgBox")
  --                               (importSome "Database.Beam.Postgres" [ importTyNamed "PgBox" ]))
  --                       (pgDataTypeSerialized pgBoxType)
  | otherwise = Nothing

--enumerationData <- Pg.query_
--  conn
--  (fromString
--    (unlines
--      [ "SELECT t.typname, t.oid, array_agg(e.enumlabel ORDER BY e.enumsortorder)"
--      , "FROM pg_enum e JOIN pg_type t ON t.oid = e.enumtypid"
--      , "GROUP BY t.typname, t.oid"
--      ]
--    )
--  )



--columnChecks <- fmap mconcat . forM tbls $ \(oid, tbl) -> do
--  columns <- Pg.query
--    conn
--    tableColumns
--    (Pg.Only (oid :: Pg.Oid))
--  let columnChecks = map
--        (\(nm, typId :: Pg.Oid, typmod, _, typ :: ByteString) ->
--          let typmod' = if typmod == -1 then Nothing else Just (typmod - 4)

--              pgDataType =
--                  fromMaybe (pgUnknownDataType typId typmod')
--                    $   pgDataTypeFromAtt typ typId typmod'
--                    <|> pgEnumerationTypeFromAtt enumerationData typ typId typmod'
--          in  Db.SomeDatabasePredicate
--                (Db.TableHasColumn (Db.QualifiedName Nothing tbl) nm pgDataType :: Db.TableHasColumn
--                    Postgres
--                )
--        )
--        columns
--      notNullChecks = concatMap
--        (\(nm, _, _, isNotNull, _) -> if isNotNull
--          then
--            [ Db.SomeDatabasePredicate
--                (Db.TableColumnHasConstraint
--                  (Db.QualifiedName Nothing tbl)
--                  nm
--                  (Db.constraintDefinitionSyntax Nothing Db.notNullConstraintSyntax Nothing) :: Db.TableColumnHasConstraint
--                    Postgres
--                )
--            ]
--          else []
--        )
--        columns

--  pure (columnChecks ++ notNullChecks)

--let enumerations = map
--      (\(enumNm, _, options) -> Db.SomeDatabasePredicate (PgHasEnum enumNm (V.toList options)))
--      enumerationData

-- pure (tblsExist ++ columnChecks ++ primaryKeys ++ enumerations)
