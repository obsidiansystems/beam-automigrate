{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.BenchUtil (cleanDatabase)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.TestUtils
import Database.Beam.AutoMigrate.Unsafe (unsafeRunMigration, unsafeMigrate)
import Database.Beam.Postgres
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import qualified Test.Database.Beam.AutoMigrate.Arbitrary as Pretty
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.QuickCheck as QC
import Test.Tasty.Runners

import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)
import Data.Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import PostgresqlSyntax.Ast
import PostgresqlSyntax.Data.Orphans ()
import qualified PostgresqlSyntax.Parsing as PgParsing
import qualified PostgresqlSyntax.Rendering as PgRendering

main :: IO ()
main = do
  let opts = includingOptions [Option (Proxy :: Proxy ConnMethod)]
  optset <- parseOptions [opts] (testGroup "" [])
  withConnection (lookupOption optset) $ \conn ->
    defaultMainWithIngredients (opts : defaultIngredients) $ tests conn

tests :: Pg.Connection -> TestTree
tests c = testGroup "Tests" [properties c]

properties :: Pg.Connection -> TestTree
properties conn =
  testGroup
    "Integration tests"
    [ -- We test that if we generate and apply a migration from a 'hsSchema', when we read the
      -- 'Schema' back from the DB, we should end up with the original 'hsSchema'.
      QC.testProperty "Migration roundtrip (empty DB)" $
        \hsSchema -> hsSchema /= noSchema ==> do
          dbProperty conn $ \_ -> do
            let mig = unsafeMigrate conn hsSchema
            runBeamPostgres conn (unsafeRunMigration mig)
            dbSchema <- getSchema conn
            let hsSchema' = normalizeDefaultExprs hsSchema
                dbSchema' = normalizeDefaultExprs dbSchema
            pure $ whenFail (printMigrationIO mig) $ hsSchema' Pretty.=== dbSchema'
      -- We test that after a successful migration, calling 'diff' should yield no edits.
    , QC.testProperty "Diffing after a migration yields no edits" $
        \hsSchema -> hsSchema /= noSchema ==> dbProperty conn $ \_ -> liftIO $ do
          let mig = unsafeMigrate conn hsSchema
          runBeamPostgres conn (unsafeRunMigration mig)
          dbSchema <- getSchema conn
          pure $ diff (normalizeDefaultExprs hsSchema) (normalizeDefaultExprs dbSchema) === Right []
    ]

-- | Execute a monadic 'Property' while also cleaning up any database's data at the end.
dbProperty :: Testable prop => Pg.Connection -> (Pg.Connection -> IO prop) -> Property
dbProperty conn prop = withMaxSuccess 50 $ monadicIO $ liftIO $ Pg.withTransactionSerializable conn $ do
  cleanDatabase conn
  r <- prop conn
  pure r

class NormalizeDefaultExprs t where
  normalizeDefaultExprs :: t -> t

instance NormalizeDefaultExprs Schema where
  normalizeDefaultExprs s = s { schemaTables = Map.map normalizeDefaultExprs (schemaTables s) }

instance NormalizeDefaultExprs Table where
  normalizeDefaultExprs t = t { tableColumns = Map.map normalizeDefaultExprs (tableColumns t)}

instance NormalizeDefaultExprs Column where
  normalizeDefaultExprs c = c { columnConstraints = Set.map normalizeDefaultExprs (columnConstraints c) }

instance NormalizeDefaultExprs ColumnConstraint where
  normalizeDefaultExprs = \case
    NotNull -> NotNull
    Default t -> Default (normalizeDefaultExpr t)

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

