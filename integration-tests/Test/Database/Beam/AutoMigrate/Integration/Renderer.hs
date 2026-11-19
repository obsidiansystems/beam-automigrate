{-# LANGUAGE OverloadedStrings #-}

-- | Direct live checks for executable renderer output: quoted identifiers,
-- physical type precision/arrays and the absence of safety markers in SQL.
module Test.Database.Beam.AutoMigrate.Integration.Renderer (rendererLiveTests) where

import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.TestUtils (withIsolatedDatabase)
import qualified Database.Beam.Backend.SQL.AST as AST
import qualified Database.PostgreSQL.Simple as Pg
import Test.Tasty
import Test.Tasty.HUnit

rendererLiveTests :: ByteString -> TestTree
rendererLiveTests dsn =
  testGroup
    "IdentifiersTypes"
    [ testCase "quoted identifiers execute and carry no safety markers" $
        withIsolatedDatabase dsn $ \conn -> do
          exec conn "CREATE TABLE \"select\" (\"old col\" INT)"
          sql <- renderOrFail (Edit (EditAction_Manual (ColumnRenamed "select" "old col" "new col")) (Right Safe))
          assertBool "no unsafe marker" (not ("<UNSAFE>" `T.isInfixOf` sql))
          exec conn sql
          [Pg.Only renamed] <-
            Pg.query
              conn
              "SELECT count(*) FROM pg_attribute WHERE attrelid = to_regclass('\"select\"') AND attname = 'new col'"
              ()
          assertEqual "renamed quoted column exists" (1 :: Int) renamed,
      testCase "rendered physical types match PostgreSQL" $
        withIsolatedDatabase dsn $ \conn -> do
          exec conn "CREATE TABLE renderer_types (id INT)"
          mapM_ (renderCase conn) (zip [1 :: Int ..] typeCases),
      testCase "quoted enum type identifiers execute" $
        withIsolatedDatabase dsn $ \conn -> do
          exec conn "CREATE TABLE renderer_enum (id INT)"
          exec conn "CREATE TYPE \"my enum\" AS ENUM ('a', 'b')"
          let eName = EnumerationName "my enum"
          sql <-
            renderOrFail
              (defMkEdit (ColumnAdded "renderer_enum" "e" (Column (PgSpecificType (PgEnumeration eName)) Set.empty)))
          assertBool "enum is quoted" ("\"my enum\"" `T.isInfixOf` sql)
          exec conn sql
          [Pg.Only actual] <-
            Pg.query
              conn
              "SELECT format_type(atttypid, atttypmod) FROM pg_attribute WHERE attrelid = to_regclass('renderer_enum') AND attname = 'e'"
              ()
          assertBool "enum column has the quoted type" ("my enum" `T.isInfixOf` actual),
      testCase "unsafe DDL is plain executable SQL" $
        withIsolatedDatabase dsn $ \conn -> do
          exec conn "CREATE TABLE renderer_drop (id INT)"
          sql <- renderOrFail (defMkEdit (TableRemoved "renderer_drop"))
          assertEqual "plain drop" "DROP TABLE \"renderer_drop\";\n" sql
          exec conn sql
    ]

typeCases :: [(ColumnType, Text)]
typeCases =
  [ (SqlStdType (AST.DataTypeTimeStamp (Just 3) True), "timestamp(3) with time zone"),
    (SqlStdType (AST.DataTypeTime (Just 0) False), "time(0) without time zone"),
    (SqlArrayType (SqlStdType AST.DataTypeInteger) 2, "integer[]"),
    (SqlStdType (AST.DataTypeNumeric (Just (10, Just 2))), "numeric(10,2)"),
    (SqlStdType (AST.DataTypeChar True Nothing Nothing), "character varying"),
    (SqlStdType (AST.DataTypeChar False Nothing Nothing), "character(1)"),
    (SqlStdType (AST.DataTypeBit True (Just 5)), "bit varying(5)"),
    (SqlStdType AST.DataTypeBoolean, "boolean")
  ]

renderCase :: Pg.Connection -> (Int, (ColumnType, Text)) -> IO ()
renderCase conn (i, (ty, expected)) = do
  let col = ColumnName ("c" <> T.pack (show i))
  sql <- renderOrFail (defMkEdit (ColumnAdded "renderer_types" col (Column ty Set.empty)))
  exec conn sql
  [Pg.Only actual] <-
    Pg.query
      conn
      "SELECT format_type(atttypid, atttypmod) FROM pg_attribute WHERE attrelid = to_regclass('renderer_types') AND attname = ?"
      (Pg.Only (columnName col))
  assertEqual ("format_type for " <> T.unpack (columnName col) <> ": " <> T.unpack actual) expected actual

renderOrFail :: Edit -> IO Text
renderOrFail e = case prettyEditSQL e of
  Left err -> assertFailure ("unexpected render error: " <> show err) >> pure ""
  Right sql -> pure sql

exec :: Pg.Connection -> Text -> IO ()
exec conn sql = () <$ Pg.execute_ conn (fromString (T.unpack sql))
