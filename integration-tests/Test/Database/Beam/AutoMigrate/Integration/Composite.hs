{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Database.Beam.AutoMigrate.Integration.Composite (compositeLiveTests) where

import Control.Exception (try)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.TestUtils (withIsolatedDatabase)
import qualified Database.PostgreSQL.Simple as Pg
import Test.Database.Beam.AutoMigrate.Composite (compositeSchema, manualSchema)
import Test.Tasty
import Test.Tasty.HUnit

compositeLiveTests :: ByteString -> TestTree
compositeLiveTests dsn = testGroup "Derivation"
  [ testCase "Composite FK accepts structural pairing and rejects reversed values" $
      withIsolatedDatabase dsn $ \conn -> do
        schema <- either (assertFailure . show) pure compositeSchema
        runMigrationUnsafe conn (migrate conn schema)
        observed <- getSchema conn
        diff schema observed @?= Right []
        void (Pg.execute conn "INSERT INTO parent (a, z) VALUES (?, ?)" (1 :: Int, 2 :: Int))
        void (Pg.execute conn "INSERT INTO child (id, z, a) VALUES (?, ?, ?)" (10 :: Int, 1 :: Int, 2 :: Int))
        rejected <- try (Pg.execute conn "INSERT INTO child (id, z, a) VALUES (?, ?, ?)" (11 :: Int, 2 :: Int, 1 :: Int))
        case rejected of
          Left (err :: Pg.SqlError) -> Pg.sqlState err @?= "23503"
          Right _ -> assertFailure "reversed composite values unexpectedly satisfy the foreign key"
        rows <- Pg.query_ conn "SELECT id, z, a FROM child" :: IO [(Int, Int, Int)]
        rows @?= [(10, 1, 2)],
    testCase "Manual scalar and nullable whole-key FKs enforce structural pairing" $
      withIsolatedDatabase dsn $ \conn -> do
        schema <- either (assertFailure . show) pure manualSchema
        runMigrationUnsafe conn (migrate conn schema)
        observed <- getSchema conn
        diff schema observed @?= Right []
        void (Pg.execute conn "INSERT INTO manual_parent (a, z) VALUES (?, ?)" (1 :: Int, 2 :: Int))
        void (Pg.execute conn "INSERT INTO manual_child (id, z, a, nullable_z, nullable_a) VALUES (?, ?, ?, ?, ?)" (10 :: Int, 1 :: Int, 2 :: Int, Nothing :: Maybe Int, Nothing :: Maybe Int))
        void (Pg.execute conn "INSERT INTO manual_child (id, z, a, nullable_z, nullable_a) VALUES (?, ?, ?, ?, ?)" (11 :: Int, 1 :: Int, 2 :: Int, 1 :: Int, 2 :: Int))
        reversedScalar <- try (Pg.execute conn "INSERT INTO manual_child (id, z, a, nullable_z, nullable_a) VALUES (?, ?, ?, ?, ?)" (12 :: Int, 2 :: Int, 1 :: Int, Nothing :: Maybe Int, Nothing :: Maybe Int))
        case reversedScalar of
          Left (err :: Pg.SqlError) -> Pg.sqlState err @?= "23503"
          Right _ -> assertFailure "reversed manual scalar values unexpectedly satisfy the foreign key"
        reversedNullable <- try (Pg.execute conn "INSERT INTO manual_child (id, z, a, nullable_z, nullable_a) VALUES (?, ?, ?, ?, ?)" (13 :: Int, 1 :: Int, 2 :: Int, 2 :: Int, 1 :: Int))
        case reversedNullable of
          Left (err :: Pg.SqlError) -> Pg.sqlState err @?= "23503"
          Right _ -> assertFailure "reversed nullable values unexpectedly satisfy the foreign key"
        rows <- Pg.query_ conn "SELECT id FROM manual_child ORDER BY id" :: IO [Pg.Only Int]
        rows @?= [Pg.Only 10, Pg.Only 11]
  ]
