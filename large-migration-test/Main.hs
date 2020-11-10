{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Time
import           Control.Monad.IO.Class                   ( liftIO )
import           Control.Exception                        ( bracket )
import qualified Data.Map.Strict                         as M


import           Database.Beam.AutoMigrate
import           Database.Beam.AutoMigrate.Postgres           ( getSchema )

import qualified Database.PostgreSQL.Simple              as Pg
import           Database.Beam.Postgres                   ( runBeamPostgres )

import           Database.Beam.AutoMigrate.BenchUtil

pgMigrate :: Pg.Connection -> Schema -> IO ()
pgMigrate conn hsSchema =
  Pg.withTransaction conn $
    runBeamPostgres conn $ do
      dbSchema <- liftIO (getSchema conn)
      unsafeRunMigration (createMigration (diff hsSchema dbSchema))

main :: IO ()
main = do
  putStrLn $ "Generating schema with 10_000 tables ..."
  (hsSchema, dbSchema) <- predictableSchemas 10000
  --printMigration $ createMigration (diff dbSchema noSchema)
  putStrLn $ "Generated schema with " ++  show (M.size . schemaTables $ hsSchema) ++ " tables."
  bracket (setupDatabase dbSchema) tearDownDatabase $ \conn -> do
    putStrLn "Starting the migration.."
    startTime <- getCurrentTime
    pgMigrate conn hsSchema
    stopTime  <- getCurrentTime
    putStrLn $ "Total time (seconds): " <> show (round (stopTime `diffUTCTime` startTime))

