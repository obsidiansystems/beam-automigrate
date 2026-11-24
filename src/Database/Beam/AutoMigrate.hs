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

    -- * Generating a Schema
    -- $generatingASchema
    fromAnnotatedDbSettings,
    fromAnnotatedDbSettingsIndexed,

    -- * Downcasting an AnnotatedDatabaseSettings into a simple DatabaseSettings
    deAnnotateDatabase,

    -- * Generating and running migrations
    Migration,
    MigrationError (..),
    migrate,
    runMigrationUnsafe,
    runMigrationWithEditUpdate,
    tryRunMigrationsWithEditUpdate,
    calcMigrationSteps,

    -- * Creating a migration from a Diff
    createMigration,

    -- * Migration utility functions
    splitEditsOnSafety,
    fastApproximateRowCountFor,

    -- * Printing migrations for debugging purposes
    prettyEditActionDescription,
    prettyEditSQL,
    showMigration,
    printMigration,
    printMigrationIO,

    -- * Unsafe functions
    unsafeRunMigration,

    -- * Handy re-exports
    module Exports,

    -- * SQL rendering
    RenderError (..),
    renderDataType,
    editToSqlCommand,
    sqlSingleQuoted,
    sqlEscaped,
    mkSequenceName,

    -- * Internals
    FromAnnotated,
    ToAnnotated,
  )
where

import Control.Exception
import Control.Monad (forM, forM_, when)
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Proxy
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.Beam.AutoMigrate.Annotated as Exports
import Database.Beam.AutoMigrate.Compat as Exports
import Database.Beam.AutoMigrate.Diff as Exports
import Database.Beam.AutoMigrate.Generic as Exports
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.Sql (editToSqlCommand, prettyEditSQL, renderDataType)
import Database.Beam.AutoMigrate.Types as Exports
import Database.Beam.AutoMigrate.Util hiding (tableName)
import Database.Beam.AutoMigrate.Validity as Exports
import Database.Beam.Backend.SQL hiding (tableName)
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import Database.Beam.Schema (Database, DatabaseSettings)
import Database.Beam.Schema.Tables (DatabaseEntity (..))
import qualified Database.PostgreSQL.Simple as Pg
import GHC.Generics hiding (prec)
import Lens.Micro (over, (^.), _1, _2)
import qualified Text.Pretty.Simple as PS

-- $annotatingDbSettings
-- The first thing to do in order to be able to use this library is to convert a Beam's 'DatabaseSettings'
-- into an 'AnnotatedDatabaseSettings'. You typically have two options in order to do that:
--
-- 1. If you don't have an existing 'DatabaseSettings' from a previous application, you can simply call
--    'defaultAnnotatedDbSettings' with 'defaultDbSettings', as in @defaultAnnotatedDbSettings defaultDbSettings@;
--
-- 2. If you are starting from an existing 'DatabaseSettings', then simply call 'defaultAnnotatedDbSettings'
--    passing your existing 'DatabaseSettings'.

-- | Simple synonym to make the signatures for 'defaultAnnotatedDbSettings' and 'fromAnnotatedDbSettings'
-- less scary. From a user's standpoint, there is nothing you have to implement.
type ToAnnotated (be :: *) (db :: DatabaseKind) e1 e2 =
  ( Generic (db (e1 be db)),
    Generic (db (e2 be db)),
    Database be db,
    GZipDatabase
      be
      (e1 be db)
      (e2 be db)
      (e2 be db)
      (Rep (db (e1 be db)))
      (Rep (db (e2 be db)))
      (Rep (db (e2 be db)))
  )

-- | Simple class to make the signatures for 'defaultAnnotatedDbSettings' and 'fromAnnotatedDbSettings'
-- less scary. From a user's standpoint, there is nothing you have to implement.
type FromAnnotated (be :: *) (db :: DatabaseKind) e1 e2 =
  ( Generic (db (e1 be db)),
    Generic (db (e2 be db)),
    Database be db,
    GZipDatabase
      be
      (e2 be db)
      (e2 be db)
      (e1 be db)
      (Rep (db (e2 be db)))
      (Rep (db (e2 be db)))
      (Rep (db (e1 be db)))
  )

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

-- $generatingASchema
-- Once you have an 'AnnotatedDatabaseSettings', you can produce a 'Schema' simply by calling
-- 'fromAnnotatedDbSettings'. The second parameter can be used to selectively turn off automatic FK-discovery
-- for one or more tables. For more information about specifying your own table constraints, refer to the
-- 'Database.Beam.AutoMigrate.Annotated' module.

-- | Turns an 'AnnotatedDatabaseSettings' into a 'Schema'. Under the hood, this function will do the
-- following:
--
-- * It will turn each 'TableEntity' of your database into a 'Table';
-- * It will turn each 'PgEnum' enumeration type into an 'Enumeration', which will map to an @ENUM@ type in the DB;
-- * It will run what we call the __/automatic FK-discovery algorithm/__. What this means practically speaking
--   is that if a reference to an external 'PrimaryKey' is found, and such 'PrimaryKey' uniquely identifies
--   another 'TableEntity' in your database, the automatic FK-discovery algorithm will turn into into a
--   'ForeignKey' 'TableConstraint', without any user intervention. In case there is ambiguity instead, the
--   library will fail with a static error until the user won't disable the relevant tables (via the provided
--  'Proxy' type) and annotate them to do the \"right thing\".
fromAnnotatedDbSettings ::
  ( FromAnnotated be db DatabaseEntity AnnotatedDatabaseEntity,
    GSchema be db anns (Rep (AnnotatedDatabaseSettings be db))
  ) =>
  AnnotatedDatabaseSettings be db ->
  Proxy (anns :: [Annotation]) ->
  Either [ValidationFailed] Schema
fromAnnotatedDbSettings db p = do
  derived <- gSchema db p (from db)
  canonical <- canonicalizeSchema derived
  validateSchema canonical
  pure canonical

-- | Produces the same 'Schema' as 'fromAnnotatedDbSettings', with a compile-time cost that is linear
-- in the size of the database instead of quadratic.
--
-- 'fromAnnotatedDbSettings' resolves the target of each discovered foreign key with a type-level
-- search over every table of the database, so the typechecker does O(tables × foreign keys) work.
-- On a database with hundreds of tables, compiling the module that derives the schema dominates
-- build time and memory. This function instead:
--
-- 1. folds the database once into an 'FkIndex' that maps the 'Data.Typeable.TypeRep' of each table
--    type to its name and primary-key columns;
-- 2. assembles each table on its own with 'mkTableEntryNoFkDiscovery';
-- 3. discovers each table's foreign keys by walking only that table's fields, resolving each
--    'Database.Beam.Schema.PrimaryKey' field with one lookup in the index.
--
-- The annotation list works as in 'fromAnnotatedDbSettings': @'UserDefinedFk' tbl@ disables FK
-- discovery for the tables of type @tbl@, and their 'foreignKeyOnPk' annotations are kept. A
-- 'foreignKeyOnPk' key overrides a discovered key on the same columns, and the result goes through
-- the same checks ('validateSchema', duplicate names, foreign-key arity), so a database that
-- 'fromAnnotatedDbSettings' rejects with a 'Left' gets the same 'Left' here.
--
-- Use it when the compile time or memory of 'fromAnnotatedDbSettings' is a problem. The trade-off is
-- where two errors appear. A foreign key whose target table is not in the database, or whose target
-- table type occurs more than once, is a type error with 'fromAnnotatedDbSettings'. Here it is a
-- call to 'error' when that table's constraints are evaluated, for example by 'migrate' or by a test
-- that forces the 'Schema'. A test that compares the result with 'fromAnnotatedDbSettings', or that
-- simply evaluates it, moves the check back before deployment.
fromAnnotatedDbSettingsIndexed ::
  forall be db anns.
  ( Generic (AnnotatedDatabaseSettings be db),
    GTableIndex (Rep (AnnotatedDatabaseSettings be db)),
    GIndexedTables anns (Rep (AnnotatedDatabaseSettings be db)),
    GEnums be db (Rep (AnnotatedDatabaseSettings be db))
  ) =>
  AnnotatedDatabaseSettings be db ->
  Proxy (anns :: [Annotation]) ->
  Either [ValidationFailed] Schema
fromAnnotatedDbSettingsIndexed db p = do
  let rep = from db
  ((tables, sequences), enumerations) <-
    bothEither (gIndexedTables p (fkIndex db) rep) (gEnums db rep)
  let derived =
        Schema
          { schemaTables = tables,
            schemaEnumerations = enumerations,
            schemaSequences = sequences
          }
  canonical <- canonicalizeSchema derived
  validateSchema canonical
  pure canonical

-- | Render all edits before executing DDL to detect rendering errors before any database changes.
renderEditCommands :: [WithPriority Edit] -> Either RenderError [Pg.PgCommandSyntax]
renderEditCommands = traverse (editToSqlCommand . fst . unPriority)

commandsToSyntax :: [Pg.PgCommandSyntax] -> Pg.PgSyntax
commandsToSyntax commands = mconcat [syntax | Pg.PgCommandSyntax _ syntax <- commands]

-- | A database 'Migration'.
type Migration m = ExceptT MigrationError (StateT [WithPriority Edit] m) ()

data MigrationError
  = DiffFailed DiffError
  | HaskellSchemaValidationFailed [ValidationFailed]
  | DatabaseSchemaValidationFailed [ValidationFailed]
  | UnsafeEditsDetected [EditAction]
  deriving (Show)

instance Exception MigrationError

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

-- | Given a 'Connection' to a database and a 'Schema' (which can be generated using 'fromAnnotatedDbSettings')
-- it returns a 'Migration', which can then be executed via 'runMigration'.
migrate :: MonadIO m => Pg.Connection -> Schema -> Migration m
migrate conn hsSchema = do
  dbSchema0 <- lift . liftIO $ getSchema conn
  -- Avoid repeated type-change edits for equivalent PostgreSQL types.
  -- Normalize both the observed schema and the desired schema before comparison.
  hsSchema' <- liftEither $ first HaskellSchemaValidationFailed (canonicalizeSchema hsSchema)
  dbSchema <- liftEither $ first DatabaseSchemaValidationFailed (canonicalizeSchema dbSchema0)
  liftEither $ first HaskellSchemaValidationFailed (validateSchema hsSchema')
  liftEither $ first DatabaseSchemaValidationFailed (validateSchema dbSchema)
  let schemaDiff = diff hsSchema' dbSchema
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
    Right (sortEdits -> edits) -> do
      commands <- either (liftIO . throwIO) pure (renderEditCommands edits)
      runNoReturn $ Pg.PgCommandSyntax Pg.PgCommandTypeDdl (commandsToSyntax commands)

-- | Runs the input 'Migration' in a concrete 'Postgres' backend.
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
runMigrationWithEditUpdate editUpdate conn hsSchema = do
  -- Create the migration with all the safeety information
  edits <- either throwIO pure =<< evalMigration (migrate conn hsSchema)
  -- Apply the user function to possibly update the list of edits to allow the user to
  -- intervene in the event of unsafe edits.
  let newEdits = sortEdits $ editUpdate $ sortEdits edits
  -- If the new list of edits still contains any unsafe edits then fail out.

  when (newEdits /= sortEdits edits) $ do
    putStrLn "Changes requested to diff induced migration. Attempting..."
    prettyPrintEdits newEdits

  when (any (editSafetyIs Unsafe . fst . unPriority) newEdits) $
    throwIO $ UnsafeEditsDetected $ fmap (\(WithPriority (e, _)) -> _editAction e) newEdits

  -- Render all commands first to detect rendering errors before executing any DDL.
  renderedEdits <- forM newEdits $ \(WithPriority (edit, _)) -> do
    command <- either throwIO pure (editToSqlCommand edit)
    pure (edit, command)

  -- Execute all the edits within a single transaction so we rollback if any of them fail.
  Pg.withTransaction conn $
    Pg.runBeamPostgres conn $
      forM_ renderedEdits $ \(edit, command) -> do
        case _editCondition edit of
          Right Unsafe -> liftIO $ throwIO $ UnsafeEditsDetected [_editAction edit]
          -- Safe or slow, run that edit.
          Right safeMaybeSlow -> safeOrSlow safeMaybeSlow edit command
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
                safeOrSlow safeMaybeSlow edit command
  where
    safeOrSlow safety edit command = do
      when (safety == PotentiallySlow) $ do
        printmsg "Running potentially slow edit"
        printmsg $ T.unpack $ prettyEditActionDescription $ _editAction edit

      runNoReturn command

    printmsg :: MonadIO m => String -> m ()
    printmsg = liftIO . putStrLn . mappend "[beam-migrate] "

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

evalMigration :: Monad m => Migration m -> m (Either MigrationError [WithPriority Edit])
evalMigration m = do
  (a, s) <- runStateT (runExceptT m) mempty
  case a of
    Left e -> pure (Left e)
    Right () -> pure (Right s)

-- | Create the migration from a 'Diff'.
createMigration :: Monad m => Diff -> Migration m
createMigration (Left e) = throwError (DiffFailed e)
createMigration (Right edits) = ExceptT $ do
  put edits
  pure (Right ())

-- | Prints the migration to stdout. Useful for debugging and diagnostic.
printMigration :: MonadIO m => Migration m -> m ()
printMigration m = do
  showMigration m >>= liftIO . putStrLn

-- | Pretty-prints the migration. Useful for debugging and diagnostic.
showMigration :: MonadIO m => Migration m -> m String
showMigration m = do
  (a, sortedEdits) <- fmap sortEdits <$> runStateT (runExceptT m) mempty
  case a of
    Left e -> liftIO $ throwIO e
    Right () ->
      case renderEditCommands sortedEdits of
        Left e -> liftIO $ throwIO e
        Right commands ->
          return $ unlines [displaySyntax (Pg.fromPgCommand command) | command <- commands]

printMigrationIO :: Migration Pg.Pg -> IO ()
printMigrationIO mig = Pg.runBeamPostgres (undefined :: Pg.Connection) $ printMigration mig

prettyEditActionDescription :: EditAction -> Text
prettyEditActionDescription = T.unwords . \case
  EditAction_Automatic ea -> case ea of
    TableAdded tblName table ->
      ["create table:", qt tblName, "\n", pshow' table]
    TableRemoved tblName ->
      ["remove table:", qt tblName]
    TableConstraintAdded tblName tableConstraint ->
      ["add table constraint to:", qt tblName, "\n", pshow' tableConstraint]
    TableConstraintRemoved tblName tableConstraint ->
      ["remove table constraint from:", qt tblName, "\n", pshow' tableConstraint]
    ColumnAdded tblName colName column ->
      ["add column:", qc colName, ", from:", qt tblName, "\n", pshow' column]
    ColumnRemoved tblName colName ->
      ["remove column:", qc colName, ", from:", qt tblName]
    ColumnTypeChanged tblName colName oldColumnType newColumnType ->
      [ "change type of column:",
        qc colName,
        "in table:",
        qt tblName,
        "\nfrom:",
        pshow' oldColumnType,
        "\nto:",
        pshow' newColumnType
      ]
    ColumnDefaultChanged tblName colName oldDefault newDefault ->
      [ "change default of column:",
        qc colName,
        "in table:",
        qt tblName,
        "\nfrom:",
        pshow' oldDefault,
        "\nto:",
        pshow' newDefault
      ]
    ColumnConstraintAdded tblName colName columnConstraint ->
      [ "add column constraint to:",
        qc colName,
        "in table:",
        qt tblName,
        "\n",
        pshow' columnConstraint
      ]
    ColumnConstraintRemoved tblName colName columnConstraint ->
      [ "remove column constraint from:",
        qc colName,
        "in table:",
        qt tblName,
        "\n",
        pshow' columnConstraint
      ]
    EnumTypeAdded eName enumeration ->
      ["add enum type:", enumName eName, pshow' enumeration]
    EnumTypeRemoved eName ->
      ["remove enum type:", enumName eName]
    EnumTypeValueAdded eName newValue insertionOrder insertedAt ->
      [ "add enum value to enum:",
        enumName eName,
        ", value:",
        newValue,
        ", with order:",
        pshow' insertionOrder,
        ", at pos",
        insertedAt
      ]
    SequenceAdded sequenceName sequence0 ->
      ["add sequence:", qs sequenceName, pshow' sequence0]
    SequenceRemoved sequenceName ->
      ["remove sequence:", qs sequenceName]
  EditAction_Manual ea -> case ea of
    ColumnRenamed tblName oldName newName ->
      [ "rename column in table:",
        qt tblName,
        "\nfrom:",
        qc oldName,
        "\nto:",
        qc newName
      ]
  where
    q t = "'" <> t <> "'"
    qt = q . tableName
    qc = q . columnName
    qs = q . seqName

    pshow' :: Show a => a -> Text
    pshow' = LT.toStrict . PS.pShow

prettyPrintEdits :: [WithPriority Edit] -> IO ()
prettyPrintEdits edits =
  either throwIO (putStrLn . T.unpack . T.unlines) $
    traverse (prettyEditSQL . fst . unPriority) (sortEdits edits)

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
tryRunMigrationsWithEditUpdate annotatedDb conn = do
    expectedHaskellSchema <-
      either (throwIO . HaskellSchemaValidationFailed) pure (fromAnnotatedDbSettings annotatedDb (Proxy @'[]))
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

        try (runMigrationWithEditUpdate Prelude.id conn expectedHaskellSchema) >>= \case
          Left (e :: SomeException) ->
            error $ "Database migration error: " <> displayException e
          Right _ ->
            pure ()

-- | Compute the `Diff` consisting of the steps that would be taken to migrate from the current actual
-- database schema to the given one, without actually performing the migration.
calcMigrationSteps
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
calcMigrationSteps annotatedDb conn = do
    expectedHaskellSchema <-
      either (throwIO . HaskellSchemaValidationFailed) pure (fromAnnotatedDbSettings annotatedDb (Proxy @'[]))
    actualDatabaseSchema <- getSchema conn
    pure $ diff expectedHaskellSchema actualDatabaseSchema
