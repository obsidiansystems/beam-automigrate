{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Database.Beam.AutoMigrate.Integration.Composite (compositeLiveTests) where

import Control.Exception (try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.TestUtils (withIsolatedDatabase)
import qualified Database.PostgreSQL.Simple as Pg
import Test.Database.Beam.AutoMigrate.Composite (compositeSchema)
import Test.Tasty
import Test.Tasty.HUnit

compositeLiveTests :: ByteString -> TestTree
compositeLiveTests dsn = testGroup "Derivation"
  [ testCase "Composite FK accepts structural pairing and rejects reversed values" $
      withIsolatedDatabase dsn $ \conn -> do
        runMigrationUnsafe conn (migrate conn compositeSchema)
        void (Pg.execute conn "INSERT INTO parent (a, z) VALUES (?, ?)" (1 :: Int, 2 :: Int))
        void (Pg.execute conn "INSERT INTO child (id, z, a) VALUES (?, ?, ?)" (10 :: Int, 1 :: Int, 2 :: Int))
        rejected <- try (Pg.execute conn "INSERT INTO child (id, z, a) VALUES (?, ?, ?)" (11 :: Int, 2 :: Int, 1 :: Int))
        case rejected of
          Left (err :: Pg.SqlError) -> Pg.sqlState err @?= "23503"
          Right _ -> assertFailure "reversed composite values unexpectedly satisfy the foreign key"
        rows <- Pg.query_ conn "SELECT id, z, a FROM child" :: IO [(Int, Int, Int)]
        rows @?= [(10, 1, 2)]
  ]
