{-# LANGUAGE OverloadedStrings #-}

module Database.Beam.AutoMigrate.BenchUtil
  ( SpineStrict (..)
  , predictableSchemas
  , connInfo
  , setupDatabase
  , cleanDatabase
  , tearDownDatabase
  )
where

import Control.DeepSeq
import Control.Exception (finally)
import Data.ByteString (ByteString)
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Schema.Gen (genSimilarSchemas)
import qualified Database.PostgreSQL.Simple as Pg
import System.Random.SplitMix (mkSMGen)
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

newtype SpineStrict = SS {unSS :: Diff}

-- For us is enough to make the list of edits spine-strict.
instance NFData SpineStrict where
  rnf (SS (Left e)) = rnf e
  rnf (SS (Right edits)) = length edits `deepseq` ()

predictableSchemas :: Int -> IO (Schema, Schema)
predictableSchemas tableNum = do
  let g = unGen genSimilarSchemas
  let r = QCGen (mkSMGen 42)
  return (g r tableNum)

connInfo :: ByteString
connInfo = "host=localhost port=5432 dbname=beam-migrate-prototype-bench"

setupDatabase :: Schema -> IO Pg.Connection
setupDatabase dbSchema = do
  conn <- Pg.connectPostgreSQL connInfo
  let mig = createMigration (diff dbSchema noSchema)
  runMigrationUnsafe conn mig -- At this point the DB contains the full schema.
  pure conn

cleanDatabase :: Pg.Connection -> IO ()
cleanDatabase conn = do
  Pg.withTransaction conn $ do
    -- Delete all tables to start from a clean slate
    _ <- Pg.execute_ conn "DROP SCHEMA public CASCADE"
    _ <- Pg.execute_ conn "CREATE SCHEMA public"
    _ <- Pg.execute_ conn "GRANT USAGE ON SCHEMA public TO public"
    _ <- Pg.execute_ conn "GRANT CREATE ON SCHEMA public TO public"
    pure ()

tearDownDatabase :: Pg.Connection -> IO ()
tearDownDatabase conn = cleanDatabase conn `finally` Pg.close conn
