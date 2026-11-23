{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Check that default changes preserve existing rows and affect subsequent inserts through the public runner.
-- Check that removing a nullable column's default makes an omitted value NULL.
-- Check that expression defaults retain their casts.
-- Check generated serial defaults with quotes and backslashes in sequence names.
module Test.Database.Beam.AutoMigrate.Integration.Defaults (defaultsLiveTests) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.Beam.AutoMigrate as BA
import qualified Database.Beam.Backend.SQL.AST as AST
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres (Postgres)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Schema hiding (Table)
import qualified Database.Beam.Schema as Beam
import qualified Database.PostgreSQL.Simple as Pg
import GHC.Generics (Generic)
import Database.Beam.AutoMigrate.TestUtils (withIsolatedDatabase)
import Test.Tasty
import Test.Tasty.HUnit

defaultsLiveTests :: ByteString -> TestTree
defaultsLiveTests dsn =
  testGroup
    "Defaults"
    [ testCase "default replacement and removal preserve existing rows" $
        withIsolatedDatabase dsn $ \conn -> do
          runMigration conn (intSchema (Just (def "1")))
          void (Pg.execute_ conn "INSERT INTO defaults_probe (id, v) VALUES (1, 100)")

          runMigration conn (intSchema (Just (def "2")))
          void (Pg.execute_ conn "INSERT INTO defaults_probe (id) VALUES (2)")
          (Pg.query_ conn "SELECT v FROM defaults_probe WHERE id = 2" :: IO [Pg.Only (Maybe Int32)])
            >>= (@?= [Pg.Only (Just 2)])
          (Pg.query_ conn "SELECT v FROM defaults_probe WHERE id = 1" :: IO [Pg.Only (Maybe Int32)])
            >>= (@?= [Pg.Only (Just 100)])

          runMigration conn (intSchema Nothing)
          void (Pg.execute_ conn "INSERT INTO defaults_probe (id) VALUES (3)")
          (Pg.query_ conn "SELECT v FROM defaults_probe WHERE id = 3" :: IO [Pg.Only (Maybe Int32)])
            >>= (@?= [Pg.Only Nothing])
          (Pg.query_ conn "SELECT v FROM defaults_probe ORDER BY id" :: IO [Pg.Only (Maybe Int32)])
            >>= (@?= [Pg.Only (Just 100), Pg.Only (Just 2), Pg.Only Nothing]),
      testCase "expression defaults execute with their casts" $
        withIsolatedDatabase dsn $ \conn -> do
          runMigration conn expressionSchema
          void (Pg.execute_ conn "INSERT INTO expr_defaults DEFAULT VALUES")
          Pg.query_
              conn
              "SELECT a::text, b::text, c::text, d::text, e::text FROM expr_defaults"
            >>= (@?= ([("3", "7", "2", "1.9", "a'b")] :: [(Text, Text, Text, Text, Text)])),
      testCase "derived serial default survives catalog roundtrip" $
        withIsolatedDatabase dsn $ \conn -> do
          schema <- either (assertFailure . show) pure (BA.fromAnnotatedDbSettings serialDbSettings (Proxy @'[]))
          let defaulted =
                [ (name, tbl)
                  | (name, tbl) <- M.toList (BA.schemaTables schema),
                    any (maybe False (const True) . BA.columnDefault) (M.elems (BA.tableColumns tbl))
                ]
              childTables =
                [ name
                  | (name, _) <- M.toList (BA.schemaTables schema),
                    name `notElem` map fst defaulted
                ]
          (parentName, parentTable) <- case defaulted of
            [p] -> pure p
            other -> assertFailure ("expected one serial parent, got " <> show (map fst other)) >> pure ("", BA.Table mempty mempty)
          let parentColumn = case M.keys (BA.tableColumns parentTable) of
                [c] -> c
                cs -> error ("expected one serial column, got " <> show cs)
              parentSql = BA.sqlEscaped (BA.tableName parentName)
          runMigration conn schema
          BA.diff schema <$> getSchema conn >>= (@?= Right [])
          void (Pg.execute_ conn (mkQuery ("INSERT INTO " <> parentSql <> " DEFAULT VALUES")))
          void (Pg.execute_ conn (mkQuery ("INSERT INTO " <> parentSql <> " DEFAULT VALUES")))
          case childTables of
            [childName] ->
              void (Pg.execute_ conn (mkQuery ("INSERT INTO " <> BA.sqlEscaped (BA.tableName childName) <> " VALUES (1, 1)")))
            other -> assertFailure ("expected one serial child, got " <> show other)
          ( Pg.query_
              conn
              ( mkQuery
                  ( "SELECT " <> BA.sqlEscaped (BA.columnName parentColumn) <> "::int FROM " <> parentSql <> " ORDER BY 1"
                  )
              ) ::
              IO [Pg.Only Int32]
            )
            >>= (@?= [Pg.Only 1, Pg.Only 2]),
      testCase "quoted companion serial names execute and roundtrip" $
        withIsolatedDatabase dsn $ \conn -> do
          let tName = BA.TableName "Weird'Table"
              cName = BA.ColumnName "col\"x"
              schema = companionSchema tName cName
              tableSql = BA.sqlEscaped (BA.tableName tName)
              columnSql = BA.sqlEscaped (BA.columnName cName)
          runMigration conn schema
          BA.diff schema <$> getSchema conn >>= (@?= Right [])
          void (Pg.execute_ conn (mkQuery ("INSERT INTO " <> tableSql <> " DEFAULT VALUES")))
          ( Pg.query_ conn (mkQuery ("SELECT " <> columnSql <> "::int FROM " <> tableSql)) ::
              IO [Pg.Only Int32]
            )
            >>= (@?= [Pg.Only 1]),
      testCase "backslash companion serial names execute" $
        withIsolatedDatabase dsn $ \conn -> do
          let tName = BA.TableName "Back\\Table"
              cName = BA.ColumnName "c"
              schema = companionSchema tName cName
              tableSql = BA.sqlEscaped (BA.tableName tName)
              columnSql = BA.sqlEscaped (BA.columnName cName)
          runMigration conn schema
          void (Pg.execute_ conn (mkQuery ("INSERT INTO " <> tableSql <> " DEFAULT VALUES")))
          ( Pg.query_ conn (mkQuery ("SELECT " <> columnSql <> "::int FROM " <> tableSql)) ::
              IO [Pg.Only Int32]
            )
            >>= (@?= [Pg.Only 1])
    ]

companionSchema :: BA.TableName -> BA.ColumnName -> BA.Schema
companionSchema tName cName =
  case BA.hasCompanionSequence (Proxy @(SqlSerial Int)) tName cName of
    Just ((sq, _), de) ->
      BA.Schema
        { BA.schemaTables =
            M.singleton tName $
              BA.Table
                mempty
                (M.singleton cName (BA.Column (BA.SqlStdType AST.DataTypeInteger) (S.singleton BA.NotNull) (Just de))),
          BA.schemaEnumerations = mempty,
          BA.schemaSequences = M.singleton sq (BA.Sequence tName cName)
        }
    Nothing -> error "expected a companion sequence"

runMigration :: Pg.Connection -> BA.Schema -> IO ()
runMigration conn schema = BA.runMigrationUnsafe conn (BA.migrate conn schema)

intSchema :: Maybe BA.DefaultExpr -> BA.Schema
intSchema mbDefault =
  BA.Schema
    { BA.schemaTables =
        M.singleton "defaults_probe" $
          BA.Table
            mempty
            ( M.fromList
                [ ("id", BA.Column (BA.SqlStdType AST.DataTypeInteger) (S.singleton BA.NotNull) Nothing),
                  ("v", BA.Column (BA.SqlStdType AST.DataTypeInteger) mempty mbDefault)
                ]
            ),
      BA.schemaEnumerations = mempty,
      BA.schemaSequences = mempty
    }

expressionSchema :: BA.Schema
expressionSchema =
  BA.Schema
    { BA.schemaTables =
        M.singleton "expr_defaults" $
          BA.Table
            mempty
            ( M.fromList
                [ ("a", numericColumn "(1 + 2)"),
                  ("b", numericColumn "CASE WHEN true THEN 7 ELSE 8 END"),
                  ("c", numericColumn "1.9::integer"),
                  ("d", numericColumn "1.9::numeric"),
                  ("e", BA.Column (BA.SqlStdType AST.DataTypeCharacterLargeObject) mempty (Just (def "'a''b'")))
                ]
            ),
      BA.schemaEnumerations = mempty,
      BA.schemaSequences = mempty
    }
  where
    numericColumn e = BA.Column (BA.SqlStdType (AST.DataTypeNumeric Nothing)) mempty (Just (def e))

data SerialParentT f = SerialParentRow
  { serialParentId :: Columnar f (SqlSerial Int)
  }
  deriving (Generic, Beamable)

instance Beam.Table SerialParentT where
  data PrimaryKey SerialParentT f = SerialParentKey (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = SerialParentKey . serialParentId

data SerialChildT f = SerialChildRow
  { serialChildId :: Columnar f Int,
    serialChildParent :: PrimaryKey SerialParentT f
  }
  deriving (Generic, Beamable)

instance Beam.Table SerialChildT where
  data PrimaryKey SerialChildT f = SerialChildKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = SerialChildKey . serialChildId

data SerialDbT f = SerialDb
  { serialDbParents :: f (TableEntity SerialParentT),
    serialDbChildren :: f (TableEntity SerialChildT)
  }
  deriving (Generic, Database be)

serialDbSettings :: BA.AnnotatedDatabaseSettings Postgres SerialDbT
serialDbSettings = BA.defaultAnnotatedDbSettings defaultDbSettings

mkQuery :: Text -> Pg.Query
mkQuery = fromString . T.unpack

def :: Text -> BA.DefaultExpr
def e = BA.DefaultExpr e mempty mempty
