{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString (ByteString)
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.Schema.Gen ()
import Database.Beam.AutoMigrate.TestUtils (requireTestConnInfo, withIsolatedDatabase)
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg
import qualified Test.Database.Beam.AutoMigrate.Arbitrary as Pretty
import Test.Database.Beam.AutoMigrate.Integration.Harness (harnessTests)
import Test.Database.Beam.AutoMigrate.Integration.Renderer (rendererLiveTests)
import Test.Database.Beam.AutoMigrate.Integration.Composite (compositeLiveTests)
import Test.Database.Beam.AutoMigrate.Integration.Defaults (defaultsLiveTests)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)
import Data.Int
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import PostgresqlSyntax.Ast
import qualified PostgresqlSyntax.Parsing as PgParsing
import qualified PostgresqlSyntax.Rendering as PgRendering

main :: IO ()
main = requireTestConnInfo >>= defaultMain . tests

tests :: ByteString -> TestTree
tests dsn =
  testGroup
    "Tests"
    [ harnessTests dsn,
      rendererLiveTests dsn,
      compositeLiveTests dsn,
      defaultsLiveTests dsn,
      properties dsn
    ]

properties :: ByteString -> TestTree
properties dsn =
  testGroup
    "Integration tests"
    [ -- Check that a migration creates the requested schema.
      QC.testProperty "Migration roundtrip (empty DB)" $
        \hsSchema -> hsSchema /= noSchema ==> dbProperty dsn $ \conn -> do
          let mig = migrate conn hsSchema
          runBeamPostgres conn (unsafeRunMigration mig)
          dbSchema <- getSchema conn
          let hsSchema' = normalizeDefaultExprs hsSchema
              dbSchema' = normalizeDefaultExprs dbSchema
          pure $ hsSchema' Pretty.=== dbSchema'
      -- Check that a completed migration needs no further edits.
    , QC.testProperty "Diffing after a migration yields no edits" $
        \hsSchema -> hsSchema /= noSchema ==> dbProperty dsn $ \conn -> do
          let mig = migrate conn hsSchema
          runBeamPostgres conn (unsafeRunMigration mig)
          dbSchema <- getSchema conn
          pure $ diff (normalizeDefaultExprs hsSchema) (normalizeDefaultExprs dbSchema) === Right []
    ]

-- | Isolate each generated test case to prevent changes to other test databases.
-- Close the connection and remove the database when the case ends.
dbProperty :: Testable prop => ByteString -> (Pg.Connection -> IO prop) -> Property
dbProperty dsn prop = ioProperty (withIsolatedDatabase dsn prop)

class NormalizeDefaultExprs t where
  normalizeDefaultExprs :: t -> t

instance NormalizeDefaultExprs Schema where
  normalizeDefaultExprs s = s { schemaTables = Map.map normalizeDefaultExprs (schemaTables s) }

instance NormalizeDefaultExprs Table where
  normalizeDefaultExprs t = t { tableColumns = Map.map normalizeDefaultExprs (tableColumns t)}

instance NormalizeDefaultExprs Column where
  normalizeDefaultExprs c = c { columnDefault = fmap normalizeDefaultExprs (columnDefault c) }

instance NormalizeDefaultExprs DefaultExpr where
  normalizeDefaultExprs de = de { defaultExpression = normalizeDefaultExpr (defaultExpression de) }

normalizeDefaultExpr :: Text -> Text
normalizeDefaultExpr t = case PgParsing.run PgParsing.aExpr t of
  Left _error -> t
  Right x -> PgRendering.toText . PgRendering.aExpr . everywhere (mkT simplifyAExpr) $ x

simplifyAExpr :: AExpr -> AExpr
simplifyAExpr e = case e of
  TypecastAExpr e' _type -> e'
  CExprAExpr (InParensCExpr e' Nothing) -> e'
  e' | Just n <- evaluableAExprInt64 e' -> CExprAExpr (AexprConstCExpr (IAexprConst n))
  _ -> e

evaluableAExprInt64 :: AExpr -> Maybe Int64
evaluableAExprInt64 e = case e of
  MinusAExpr e' -> fmap negate (evaluableAExprInt64 e')
  CExprAExpr e' -> evaluableCExprInt64 e'
  _ -> Nothing

evaluableCExprInt64 :: CExpr -> Maybe Int64
evaluableCExprInt64 e = case e of
  AexprConstCExpr (IAexprConst n) -> Just n
  AexprConstCExpr (SAexprConst s) -> case reads (T.unpack s) of
    [(n,"")] -> Just n
    _ -> Nothing
  _ -> Nothing
