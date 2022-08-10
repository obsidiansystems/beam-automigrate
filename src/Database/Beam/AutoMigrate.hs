{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides the high-level API to migrate a database.
module Database.Beam.AutoMigrate
  ( -- * Annotating a database
    -- $annotatingDbSettings
    defaultAnnotatedDbSettings,

    -- * Downcasting an AnnotatedDatabaseSettings into a simple DatabaseSettings
    deAnnotateDatabase,

    -- * Generating and running migrations
    runMigrationUnsafe,
    runMigrationWithEditUpdate,
    tryRunMigrationsWithEditUpdate,

    -- * Creating a migration from a Diff
    createMigration,

    -- * Migration utility functions
    splitEditsOnSafety,
    fastApproximateRowCountFor,

    -- * Handy re-exports
    module Exports,

    -- * Internals
    sqlSingleQuoted,
  )
where

import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.State.Strict

import Data.Int (Int64)
import Data.List (foldl')
import Data.Proxy
import Data.String.Conv (toS)
import Database.Beam.Backend.SQL hiding (tableName)
import Database.Beam.Schema (Database, DatabaseSettings)
import Database.Beam.Schema.Tables (DatabaseEntity (..))
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import qualified Database.PostgreSQL.Simple as Pg
import GHC.Generics
import Lens.Micro (over, (^.), _1, _2)

import Database.Beam.AutoMigrate.Annotated as Exports
import Database.Beam.AutoMigrate.Compat as Exports
import Database.Beam.AutoMigrate.Diff as Exports
import Database.Beam.AutoMigrate.Eval as Exports
import Database.Beam.AutoMigrate.Generic as Exports
import Database.Beam.AutoMigrate.Types as Exports
import Database.Beam.AutoMigrate.Util hiding (tableName)
import Database.Beam.AutoMigrate.Validity as Exports
import Database.Beam.AutoMigrate.Unsafe

-- | Turns a Beam's 'DatabaseSettings' into an 'AnnotatedDatabaseSettings'.
defaultAnnotatedDbSettings ::
  forall be db.
  ToAnnotated be db DatabaseEntity AnnotatedDatabaseEntity =>
  DatabaseSettings be db ->
  AnnotatedDatabaseSettings be db
defaultAnnotatedDbSettings db =
  runIdentity $
    zipTables (Proxy @be) annotate db (undefined :: AnnotatedDatabaseSettings be db)
  where
    annotate ::
      ( Monad m,
        IsAnnotatedDatabaseEntity be ty,
        AnnotatedDatabaseEntityRegularRequirements be ty
      ) =>
      DatabaseEntity be db ty ->
      AnnotatedDatabaseEntity be db ty ->
      m (AnnotatedDatabaseEntity be db ty)
    annotate (DatabaseEntity edesc) _ =
      pure $ AnnotatedDatabaseEntity (dbAnnotatedEntityAuto edesc) (DatabaseEntity edesc)

-- | Downcast an 'AnnotatedDatabaseSettings' into Beam's standard 'DatabaseSettings'.
deAnnotateDatabase ::
  forall be db.
  FromAnnotated be db DatabaseEntity AnnotatedDatabaseEntity =>
  AnnotatedDatabaseSettings be db ->
  DatabaseSettings be db
deAnnotateDatabase db =
  runIdentity $ zipTables (Proxy @be) (\ann _ -> pure $ ann ^. deannotate) db db

-- | Split the given list of 'Edit's based on their 'EditSafety' setting.
splitEditsOnSafety :: [WithPriority Edit] -> ([WithPriority Edit], [WithPriority Edit])
splitEditsOnSafety =
  foldl'
    ( \acc p ->
        if editSafetyIs Unsafe (fst $ unPriority p)
          then over _1 (p :) acc
          else over _2 (p :) acc
    )
    (mempty, mempty)

-- | Runs the input 'Migration' in a concrete 'Postgres' backend.
--
-- This is unsafe in the sense that it will execute destructive migrations.
runMigrationUnsafe :: MonadBeam Pg.Postgres Pg.Pg => Pg.Connection -> Migration Pg.Pg -> IO ()
runMigrationUnsafe conn mig = Pg.withTransaction conn $ Pg.runBeamPostgres conn (unsafeRunMigration mig)

-- | Run the steps of the migration in priority order, providing a hook to allow the user
-- to take action for 'Unsafe' edits. The given function is only called for unsafe edits.
--
-- This allows you to perform some checks for when the edit safe in some circumstances.
--
-- * Deleting an empty table/column
-- * Making an empty column non-nullable
runMigrationWithEditUpdate ::
  MonadBeam Pg.Postgres Pg.Pg =>
  ([WithPriority Edit] -> [WithPriority Edit]) ->
  Pg.Connection ->
  Schema ->
  IO ()
runMigrationWithEditUpdate editUpdate conn hsSchema = Pg.withTransaction conn $ do
  unsafeRunMigrationWithEditUpdate editUpdate conn hsSchema

-- | Helper query to retrieve the approximate row count from the @pg_class@ table.
--
-- Number of live rows in the table. This is only an estimate used by the planner. It is
-- updated by VACUUM, ANALYZE, and a few DDL commands such as CREATE INDEX.
--
-- This can be used as a check to see if an otherwise 'Unsafe' 'EditAction' is safe to execute.
--
-- See:
-- * <https://wiki.postgresql.org/wiki/Count_estimate PostgreSQL Wiki Count Estimate> and
-- * <https://www.postgresql.org/docs/current/catalog-pg-class.html PostgreSQL Manual for @pg_class@>
-- for more information.
fastApproximateRowCountFor :: TableName -> Pg.Pg (Maybe Int64)
fastApproximateRowCountFor tblName = runReturningOne $ selectCmd $ Pg.PgSelectSyntax $ qry
  where
    qry =
      Pg.emit $
        toS $
          "SELECT reltuples AS approximate_row_count FROM pg_class WHERE relname = "
            <> sqlEscaped (tableName tblName)
            <> ";"

-- | Create the migration from a 'Diff'.
createMigration :: Monad m => Diff -> Migration m
createMigration (Left e) = throwError (DiffFailed e)
createMigration (Right edits) = ExceptT $ do
  put edits
  pure (Right ())

-- | Compare the existing schema in the database with the expected
-- schema in Haskell and try to edit the existing schema as necessary
tryRunMigrationsWithEditUpdate
  :: ( Generic (db (DatabaseEntity be db))
     , (Generic (db (AnnotatedDatabaseEntity be db)))
     , Database be db
     , (GZipDatabase be
         (AnnotatedDatabaseEntity be db)
         (AnnotatedDatabaseEntity be db)
         (DatabaseEntity be db)
         (Rep (db (AnnotatedDatabaseEntity be db)))
         (Rep (db (AnnotatedDatabaseEntity be db)))
         (Rep (db (DatabaseEntity be db)))
       )
     , (GSchema be db '[] (Rep (db (AnnotatedDatabaseEntity be db))))
     )
  => AnnotatedDatabaseSettings be db
  -> Pg.Connection
  -> IO ()
tryRunMigrationsWithEditUpdate annotatedDb conn = Pg.withTransaction conn $
  unsafeTryRunMigrationsWithEditUpdate annotatedDb conn
