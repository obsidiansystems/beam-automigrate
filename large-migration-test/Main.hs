{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time
import           System.Random.SplitMix         ( mkSMGen )
import           Data.ByteString                ( ByteString )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception              ( bracket )
import qualified Data.Map.Strict               as M

import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

import           Database.Beam.Migrate
import           Database.Beam.Migrate.Schema.Gen
                                                ( genSimilarSchemas )
import           Database.Beam.Migrate.Postgres ( getSchema )

import qualified Database.PostgreSQL.Simple    as Pg
import           Database.Beam.Postgres         ( runBeamPostgres )


predictableSchemas :: Int -> IO (Schema, Schema)
predictableSchemas tableNum = do
    let g = unGen genSimilarSchemas 
    let r = QCGen (mkSMGen 42)
    return (g r tableNum)

connInfo :: ByteString
connInfo = "host=localhost port=5432 dbname=beam-migrate-prototype-bench"

pgMigrate :: Schema -> IO ()
pgMigrate hsSchema = do
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $ do
      runBeamPostgres conn $ do
        dbSchema <- liftIO (getSchema conn)
        runMigration (createMigration (diff hsSchema dbSchema))

setupDatabase :: Schema -> IO ()
setupDatabase dbSchema =
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $
      runBeamPostgres conn $ do
        let mig = createMigration (diff dbSchema noSchema)
        runMigration mig -- At this point the DB contains the full schema.

tearDownDatabase :: () -> IO ()
tearDownDatabase () =
  bracket (Pg.connectPostgreSQL connInfo) Pg.close $ \conn ->
    Pg.withTransaction conn $ do
   -- Delete all tables to start from a clean slate
   _ <- Pg.execute_ conn "DROP SCHEMA public CASCADE"
   _ <- Pg.execute_ conn "CREATE SCHEMA public"
   _ <- Pg.execute_ conn "GRANT USAGE ON SCHEMA public TO public"
   _ <- Pg.execute_ conn "GRANT CREATE ON SCHEMA public TO public"
   pure ()

main :: IO ()
main = do
  putStrLn $ "Generating schema with 10_000 tables ..."
  (hsSchema, dbSchema) <- predictableSchemas 10000
  putStrLn $ "Generated schema with " ++  show (M.size . schemaTables $ hsSchema) ++ " tables."
  bracket (setupDatabase dbSchema) tearDownDatabase $ \() -> do
    putStrLn "Starting the migration.."
    startTime <- getCurrentTime 
    pgMigrate hsSchema 
    stopTime  <- getCurrentTime
    putStrLn $ "Total time (seconds): " <> show (round (stopTime `diffUTCTime` startTime))

