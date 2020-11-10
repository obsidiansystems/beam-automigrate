{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.BenchUtil
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.Postgres (runBeamPostgres)
import qualified Database.PostgreSQL.Simple as Pg

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
  putStrLn $ "Generated schema with " ++ show (M.size . schemaTables $ hsSchema) ++ " tables."
  bracket (setupDatabase dbSchema) tearDownDatabase $ \conn -> do
    putStrLn "Starting the migration.."
    startTime <- getCurrentTime
    pgMigrate conn hsSchema
    stopTime <- getCurrentTime
    putStrLn $ "Total time (seconds): " <> show (round (stopTime `diffUTCTime` startTime))
