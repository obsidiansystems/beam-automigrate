module Main where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as L
import Data.Proxy
import qualified Data.Text.Lazy as TL
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.BenchUtil (cleanDatabase, tearDownDatabase)
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.Schema.Gen
import Database.Beam.AutoMigrate.TestUtils
import Database.Beam.AutoMigrate.Validity
import qualified Database.PostgreSQL.Simple as Pg
import System.Environment (getArgs)
import qualified Test.Database.Beam.AutoMigrate.Arbitrary as Pretty
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  let opts = includingOptions [Option (Proxy :: Proxy ConnMethod)]
  defaultMainWithIngredients (opts : defaultIngredients) $ askOption tests

tests :: ConnMethod -> TestTree
tests c = testGroup "Tests" [properties c]

properties :: ConnMethod -> TestTree
properties c =
  testGroup
    "Integration tests"
    [ -- We test that if we generate and apply a migration from a 'hsSchema', when we read the
      -- 'Schema' back from the DB, we should end up with the original 'hsSchema'.
      QC.testProperty "Migration roundtrip (empty DB)" $
        \hsSchema -> hsSchema /= noSchema ==> dbProperty c $ \conn -> liftIO $ do
          let mig = migrate conn hsSchema
          runMigrationUnsafe conn mig
          dbSchema <- getSchema conn
          pure $ hsSchema Pretty.=== dbSchema
      -- We test that after a successful migration, calling 'diff' should yield no edits.
    , QC.testProperty "Diffing after a migration yields no edits" $
        \hsSchema -> hsSchema /= noSchema ==> dbProperty c $ \dbConn -> liftIO $ do
          let mig = migrate dbConn hsSchema
          runMigrationUnsafe dbConn mig
          dbSchema <- getSchema dbConn
          pure $ diff hsSchema dbSchema === Right []
    ]

-- | Execute a monadic 'Property' while also cleaning up any database's data at the end.
dbProperty :: Testable prop => ConnMethod -> (Pg.Connection -> IO prop) -> Property
dbProperty cm prop = withMaxSuccess 50 $ monadicIO $ liftIO $ withConnection cm $ \conn -> do
  r <- prop conn
  cleanDatabase conn
  pure r
