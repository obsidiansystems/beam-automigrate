{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Beam.Migrate.Postgres where

import           Data.String
import           Data.Foldable                  ( foldlM )
import           Control.Monad
import qualified Data.Vector                   as V
import qualified Data.Map.Strict               as M
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )
import           Data.List                      ( foldl' )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE
import           Data.ByteString                ( ByteString )

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
getTableSchema :: Pg.Connection -> IO Schema
getTableSchema conn = do
  pgTables     <- Pg.query_ conn userTablesQ
  pgPks        <- toPkLookupTable <$> Pg.query_ conn primaryKeysQ
  schemaTables <- foldlM (getTable pgPks) mempty pgTables
  pure $ Schema schemaTables

  where
    getTable :: Map TableName (Set ColumnName) -> Tables -> (Pg.Oid, Text) -> IO Tables
    getTable pkLookupTable allTables (oid, TableName -> tName) = do
      pgColumns <- Pg.query conn tableColumnsQ (Pg.Only oid)
      newTable  <-
        Table (maybe noSchemaConstraints (S.singleton . PrimaryKey) (M.lookup tName pkLookupTable))
          <$> foldlM getColumns mempty pgColumns
      pure $ M.insert tName newTable allTables

    getColumns :: Columns -> (ByteString, Pg.Oid, Int, Bool, ByteString) -> IO Columns
    getColumns c (attname, atttypid, atttypmod, attnotnull, format_type) = do
      print (attname, atttypid, attnotnull, format_type)
      let newColumn = Column () noSchemaConstraints
      pure $ M.insert (ColumnName (TE.decodeUtf8 attname)) newColumn c

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
