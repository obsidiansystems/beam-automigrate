{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
-- | WARNING: This module exposes database actions that
-- can modify the schema and data of a database without being
-- wrapped in a transaction.
--
-- Use with caution
module Database.Beam.AutoMigrate.Unsafe where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.Proxy
import Data.String.Conv (toS)
import qualified Data.Text as T
import Database.Beam.Backend.SQL hiding (tableName)
import Database.Beam.Schema (Database)
import Database.Beam.Schema.Tables (DatabaseEntity (..))
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import GHC.Generics hiding (prec)

import Database.Beam.AutoMigrate.Annotated
import Database.Beam.AutoMigrate.Diff
import Database.Beam.AutoMigrate.Eval
import Database.Beam.AutoMigrate.Generic
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.Types
import Database.Beam.AutoMigrate.Validity

-- | Like 'runMigrationWithEditUpdate' but is not wrapped in a transaction.
-- It *should* be wrapped in a transaction by the user. This is exposed
-- so that you can include the actions of beam-automigrate within a broader
-- transactional context.
--
-- DOES NOT RUN IN A TRANSACTION
unsafeRunMigrationWithEditUpdate
  :: MonadBeam Pg.Postgres Pg.Pg
  => ([WithPriority Edit] -> [WithPriority Edit])
  -> Pg.Connection
  -> Schema
  -> IO ()
unsafeRunMigrationWithEditUpdate editUpdate conn hsSchema = do
  edits <- either throwIO pure =<< evalMigration (unsafeMigrate conn hsSchema)
  -- Apply the user function to possibly update the list of edits to allow the user to
  -- intervene in the event of unsafe edits.
  let newEdits = sortEdits $ editUpdate $ sortEdits edits
  -- If the new list of edits still contains any unsafe edits then fail out.

  when (newEdits /= sortEdits edits) $ do
    putStrLn "Changes requested to diff induced migration. Attempting..."
    prettyPrintEdits newEdits

  when (any (editSafetyIs Unsafe . fst . unPriority) newEdits) $
    throwIO $ UnsafeEditsDetected $ fmap (\(WithPriority (e, _)) -> _editAction e) newEdits

  -- Execute all the edits within a single transaction so we rollback if any of them fail.
  Pg.runBeamPostgres conn $
    forM_ newEdits $ \(WithPriority (edit, _)) -> do
      case _editCondition edit of
        Right Unsafe -> liftIO $ throwIO $ UnsafeEditsDetected [_editAction edit]
        -- Safe or slow, run that edit.
        Right safeMaybeSlow -> safeOrSlow safeMaybeSlow edit
        Left ec -> do
          -- Edit is conditional, run the condition to see how safe it is to run this edit.
          printmsg $ "edit has condition: " <> toS (prettyEditConditionQuery ec)
          checkedSafety <- _editCondition_check ec
          case checkedSafety of
            Unsafe -> do
              -- Edit determined to be unsafe, don't run it.
              printmsg "edit unsafe by condition"
              liftIO $ throwIO $ UnsafeEditsDetected [_editAction edit]
            safeMaybeSlow -> do
              -- Safe or slow, run that edit.
              printmsg "edit condition satisfied"
              safeOrSlow safeMaybeSlow edit
  where
    safeOrSlow safety edit = do
      when (safety == PotentiallySlow) $ do
        printmsg "Running potentially slow edit"
        printmsg $ T.unpack $ prettyEditActionDescription $ _editAction edit

      runNoReturn $ editToSqlCommand edit

    printmsg :: MonadIO m => String -> m ()
    printmsg = liftIO . putStrLn . mappend "[beam-migrate] "

-- | Compare the existing schema in the database with the expected
-- schema in Haskell and try to edit the existing schema as necessary
--
-- DOES NOT RUN IN A TRANSACTION
unsafeTryRunMigrationsWithEditUpdate
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
unsafeTryRunMigrationsWithEditUpdate annotatedDb conn = do
    let expectedHaskellSchema = fromAnnotatedDbSettings annotatedDb (Proxy @'[])
    actualDatabaseSchema <- getSchema conn
    case diff expectedHaskellSchema actualDatabaseSchema of
      Left err -> do
        putStrLn "Error detecting database migration requirements: "
        print err
      Right [] ->
        putStrLn "No database migration required, continuing startup."
      Right edits -> do
        putStrLn "Database migration required, attempting..."
        prettyPrintEdits edits

        try (unsafeRunMigrationWithEditUpdate Prelude.id conn expectedHaskellSchema) >>= \case
          Left (e :: SomeException) ->
            error $ "Database migration error: " <> displayException e
          Right _ ->
            pure ()

-- | Compute the `Diff` consisting of the steps that would be taken to migrate from the current actual
-- database schema to the given one, without actually performing the migration.
--
-- DOES NOT RUN IN A TRANSACTION
unsafeCalcMigrationSteps
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
  -> IO Diff
unsafeCalcMigrationSteps annotatedDb conn = do
  let expectedHaskellSchema = fromAnnotatedDbSettings annotatedDb (Proxy @'[])
  actualDatabaseSchema <- getSchema conn
  pure $ diff expectedHaskellSchema actualDatabaseSchema

-- | Given a 'Connection' to a database and a 'Schema' (which can be generated using 'fromAnnotatedDbSettings')
-- it returns a 'Migration', which can then be executed via 'runMigration'.
--
-- DOES NOT RUN IN A TRANSACTION
unsafeMigrate :: MonadIO m => Pg.Connection -> Schema -> Migration m
unsafeMigrate conn hsSchema = do
  dbSchema <- lift . liftIO $ getSchema conn
  liftEither $ first HaskellSchemaValidationFailed (validateSchema hsSchema)
  liftEither $ first DatabaseSchemaValidationFailed (validateSchema dbSchema)
  let schemaDiff = diff hsSchema dbSchema
  case schemaDiff of
    Left e -> throwError (DiffFailed e)
    Right edits -> lift (put edits)

-- | Runs the input 'Migration' in a concrete 'Postgres' backend.
--
-- __IMPORTANT:__ This function /does not/ run inside a SQL transaction, hence the @unsafe@ prefix.
unsafeRunMigration :: (MonadBeam Pg.Postgres m, MonadIO m) => Migration m -> m ()
unsafeRunMigration m = do
  migs <- evalMigration m
  case migs of
    Left e -> liftIO $ throwIO e
    Right (sortEdits -> edits) ->
      runNoReturn $ Pg.PgCommandSyntax Pg.PgCommandTypeDdl (mconcat . editsToPgSyntax $ edits)

