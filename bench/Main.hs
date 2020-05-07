{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception              ( bracket )
import qualified Data.Map.Strict               as M

import           Database.Beam.Migrate.New
import           Database.Beam.Migrate.New.Postgres ( getSchema )

import qualified Database.PostgreSQL.Simple    as Pg
import           Database.Beam.Postgres         ( runBeamPostgres )

import           Criterion.Main
import           Database.Beam.Migrate.New.BenchUtil

pgMigrate :: Pg.Connection -> (Schema -> Schema -> Diff) -> Schema -> IO SpineStrict
pgMigrate conn diffFun hsSchema =
  Pg.withTransaction conn $
    runBeamPostgres conn $ do
      dbSchema <- liftIO (getSchema conn)
      pure . SS $ diffFun hsSchema dbSchema

main :: IO ()
main = do
  putStrLn "Generating schema with 10_000 tables ..."
  (hsSchema, dbSchema) <- predictableSchemas 10000
  putStrLn $ "Generated schema with " ++  show (M.size . schemaTables $ hsSchema) ++ " tables."
  bracket (setupDatabase dbSchema) tearDownDatabase $ \pgConn ->
    defaultMain [
        bgroup "diff" [
            bench "reference/10_000 tables avg. case (similar schema)" $ nf (SS . diffReferenceImplementation hsSchema) dbSchema
          , bench "efficient/10_000 tables avg. case (similar schema)" $ nf (SS . diff hsSchema                       ) dbSchema
          , bench "reference/10_000 tables worst case (no schema)"     $ nf (SS . diffReferenceImplementation hsSchema) noSchema
          , bench "efficient/10_000 tables worst case (no schema)"     $ nf (SS . diff hsSchema                       ) noSchema
          ]
      , bgroup "getSchema" [
            bench "10_000 tables" $ nfIO (getSchema pgConn)
          ]
      , bgroup "full_migration" [
            bench "reference/10_000 tables avg. case (similar schema)"       $ nfIO (pgMigrate pgConn diffReferenceImplementation hsSchema)
          , bench "efficient/10_000 tables avg. case (similar schema)"       $ nfIO (pgMigrate pgConn diff hsSchema)
          , bench "reference/10_000 tables worst case (no previous schema)"  $ nfIO (pgMigrate pgConn diffReferenceImplementation hsSchema)
          , bench "efficient/10_000 tables worst case (no previous schema)"  $ nfIO (pgMigrate pgConn diff hsSchema)
          ]
      ]
