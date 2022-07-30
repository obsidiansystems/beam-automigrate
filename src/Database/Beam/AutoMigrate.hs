{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- | This module provides the high-level API to migrate a database.
module Database.Beam.AutoMigrate
  ( -- * Annotating a database
    -- $annotatingDbSettings
    defaultAnnotatedDbSettings,

    -- * Generating a Schema
    -- $generatingASchema
    fromAnnotatedDbSettings,

    -- * Downcasting an AnnotatedDatabaseSettings into a simple DatabaseSettings
    deAnnotateDatabase,

    -- * Generating and running migrations
    Migration,
    migrate,
    runMigrationUnsafe,
    runMigrationWithEditUpdate,
    tryRunMigrationsWithEditUpdate,
    tryRunMigrations,
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

    -- * Internals
    FromAnnotated,
    ToAnnotated,
    sqlSingleQuoted,
    sqlEscaped,
    editToSqlCommand,

    -- * everything else
    module Database.Beam.AutoMigrate
  )
where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Identity (runIdentity)
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict -- TODO: strict writer not strict enough, use "writer-cps"
import Data.Bifunctor (first)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import qualified Data.Set as S
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Database.Beam.AutoMigrate.Annotated as Exports
import Database.Beam.AutoMigrate.Compat as Exports
import Database.Beam.AutoMigrate.Diff as Exports
import Database.Beam.AutoMigrate.Generic as Exports
import Database.Beam.AutoMigrate.Postgres (getSchema)
import Database.Beam.AutoMigrate.Types as Exports
import Database.Beam.AutoMigrate.Util hiding (tableName)
import Database.Beam.AutoMigrate.Validity as Exports
import Database.Beam.Backend.SQL hiding (tableName)
import qualified Database.Beam.Backend.SQL.AST as AST
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import Database.Beam.Schema (Database, DatabaseSettings)
import Database.Beam.Schema.Tables (DatabaseEntity (..))
import qualified Database.PostgreSQL.Simple as Pg
import GHC.Generics hiding (prec)
import Control.Lens (over, (^.), _1, _2, preuse, preuses, uses, use, ix, at)
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
  Schema
fromAnnotatedDbSettings db p = gSchema db p (from db)

editsToPgSyntax :: [WithPriority Edit] -> [Pg.PgSyntax]
editsToPgSyntax = map (toSqlSyntax . fst . unPriority)

-- | A database 'Migration'.
type Migration m = ExceptT MigrationError (StateT [WithPriority Edit] m) ()

data MigrationError
  = DiffFailed DiffError
  | HaskellSchemaValidationFailed [ValidationFailed]
  | DatabaseSchemaValidationFailed [ValidationFailed]
  | UnsafeEditsDetected [EditAction]
  | BadEdit ApplyFailed
  | UserError String
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
  dbSchema <- lift . liftIO $ getSchema conn
  liftEither $ first HaskellSchemaValidationFailed (validateSchema hsSchema)
  liftEither $ first DatabaseSchemaValidationFailed (validateSchema dbSchema)
  let schemaDiff = diffSorted hsSchema dbSchema
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

-- | Runs the input 'Migration' in a concrete 'Postgres' backend.
runMigrationUnsafe :: Pg.Connection -> Migration Pg.Pg -> IO ()
runMigrationUnsafe conn mig = Pg.withTransaction conn $ Pg.runBeamPostgres conn (unsafeRunMigration mig)

-- | Run the steps of the migration in priority order, providing a hook to allow the user
-- to take action for 'Unsafe' edits. The given function is only called for unsafe edits.
--
-- This allows you to perform some checks for when the edit safe in some circumstances.
--
-- * Deleting an empty table/column
-- * Making an empty column non-nullable
runMigrationWithEditUpdate ::
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
    prettyPrintEdits $ fmap withoutPriority newEdits

  tryRunEdits conn $ withoutPriority <$> newEdits

tryRunEdits :: Pg.Connection -> [Edit] -> IO ()
tryRunEdits conn newEdits = do
  when (any (editSafetyIs Unsafe) newEdits) $
    throwIO $ UnsafeEditsDetected $ fmap _editAction newEdits

  tryRunEditsUnsafe conn newEdits


tryRunEditsUnsafe :: Pg.Connection -> [Edit] -> IO ()
tryRunEditsUnsafe conn newEdits =
  try (runEditsUnsafe conn newEdits) >>= \case
    Left (e :: SomeException) -> error $ "Database migration error: " <> displayException e
    Right _ -> pure ()

runEditsUnsafe :: Pg.Connection -> [Edit] -> IO ()
runEditsUnsafe conn newEdits = do
  -- Execute all the edits within a single transaction so we rollback if any of them fail.
  Pg.withTransaction conn $
    Pg.runBeamPostgres conn $
      forM_ newEdits $ \edit -> do
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

-- Unfortunately Postgres' syntax is different when setting or dropping constaints. For example when we
-- drop the default value we /don't/ repeat which was the original default value (which makes sense), but
-- doing so means we have to discriminate between these two events to render the SQL fragment correctly.
data AlterTableAction
  = SetConstraint
  | DropConstraint
  deriving (Show, Eq)

-- | Converts a single 'Edit' into the relevant 'PgSyntax' necessary to generate the final SQL.
toSqlSyntax :: Edit -> Pg.PgSyntax
toSqlSyntax e =
  safetyPrefix $ _editAction e & \case
    EditAction_Automatic ea -> case ea of
      TableAdded tblName tbl ->
        ddlSyntax
          ( "CREATE TABLE " <> sqlEscaped (tableName tblName)
              <> " ("
              <> T.intercalate ", " (map renderTableAddedColumn (M.toList (tableColumns tbl)))
              <> ")"
          )
      TableRemoved tblName ->
        ddlSyntax ("DROP TABLE " <> sqlEscaped (tableName tblName))
      PrimaryKeyAdded tblName cstr copt ->
        updateSyntax (alterTable tblName <> " ADD " <> renderCreatePrimaryKeyConstraint cstr copt)
      UniqueConstraintAdded tblName cstr copt ->
        updateSyntax (alterTable tblName <> " ADD " <> renderCreateUniqueConstraint cstr copt)
      ForeignKeyAdded tblName cstr copt ->
        updateSyntax (alterTable tblName <> " ADD " <> renderCreateForeignKeyConstraint cstr copt)

      RenameConstraint tblName (ConstraintName oldName) (ConstraintName newName) ->
        updateSyntax (alterTable tblName
          <> " RENAME CONSTRAINT "
          <> sqlEscaped oldName
          <> " TO "
          <> sqlEscaped newName)
      TableConstraintRemoved tblName cstr _ ->
        updateSyntax (alterTable tblName <> renderDropConstraint cstr)
      SequenceAdded sName s -> createSequenceSyntax sName s
      SequenceRemoved sName -> dropSequenceSyntax sName
      SequenceRenamed oldName newName -> renameSequenceSyntax oldName newName
      SequenceSetOwner sName newOwner -> setSequenceOwnerSyntax sName newOwner
      EnumTypeAdded tyName vals -> createTypeSyntax tyName vals
      EnumTypeRemoved (EnumerationName tyName) -> ddlSyntax ("DROP TYPE " <> tyName)
      EnumTypeValueAdded (EnumerationName tyName) newVal order insPoint ->
        ddlSyntax
          ( "ALTER TYPE " <> tyName
              <> " ADD VALUE "
              <> sqlSingleQuoted newVal
              <> " "
              <> renderInsertionOrder order
              <> " "
              <> sqlSingleQuoted insPoint
          )
      ColumnAdded tblName colName col ->
        updateSyntax
          ( alterTable tblName
              <> "ADD COLUMN "
              <> sqlEscaped (columnName colName)
              <> " "
              <> renderDataTypeAdd col
          )
      ColumnRemoved tblName colName ->
        updateSyntax (alterTable tblName <> "DROP COLUMN " <> sqlEscaped (columnName colName))
      ColumnTypeChanged tblName colName _old new ->
        updateSyntax
          ( alterTable tblName <> "ALTER COLUMN "
              <> sqlEscaped (columnName colName)
              <> " TYPE "
              <> renderDataType new
          )
      ColumnNullableChanged tblName colName nullConstr -> 
        updateSyntax
          ( alterTable tblName <> "ALTER COLUMN "
            <> sqlEscaped (columnName colName)
            <> case nullConstr of
              Null -> " DROP NOT NULL"
              NotNull -> " SET NOT NULL"
          )
      ColumnDefaultChanged tblName colName dfltConst -> 
        updateSyntax
          ( alterTable tblName <> "ALTER COLUMN "
            <> sqlEscaped (columnName colName)
            <> case dfltConst of
              Nothing -> " DROP DEFAULT "
              Just d -> " SET " <> renderColumnDefault d
          )
    EditAction_Manual ea -> case ea of
      ColumnRenamed tblName oldName newName ->
        updateSyntax
          ( alterTable tblName <> "RENAME COLUMN "
            <> sqlEscaped (columnName oldName)
            <> " TO "
            <> sqlEscaped (columnName newName)
          )
  where
    safetyPrefix query =
      if editSafetyIs Safe e
        then Pg.emit "        " <> query
        else Pg.emit "<UNSAFE>" <> query

    ddlSyntax query = Pg.emit . TE.encodeUtf8 $ query <> ";\n"
    updateSyntax query = Pg.emit . TE.encodeUtf8 $ query <> ";\n"

    alterTable :: TableName -> Text
    alterTable (TableName tName) = "ALTER TABLE " <> sqlEscaped tName <> " "

    renderTableAddedColumn :: (ColumnName, Column) -> Text
    renderTableAddedColumn (colName, col) =
      sqlEscaped (columnName colName) <> " "
        <> renderDataTypeAdd col

    renderInsertionOrder :: InsertionOrder -> Text
    renderInsertionOrder Before = "BEFORE"
    renderInsertionOrder After = "AFTER"

    renderCreateUniqueConstraint :: Unique -> UniqueConstraintOptions -> Text
    renderCreateUniqueConstraint (Unique cols) (UniqueConstraintOptions fname) =
        conKeyword fname
          <> " UNIQUE ("
          <> T.intercalate ", " (map (sqlEscaped . columnName) (S.toList cols))
          <> ")"
    renderCreatePrimaryKeyConstraint :: PrimaryKeyConstraint -> UniqueConstraintOptions -> Text
    renderCreatePrimaryKeyConstraint (PrimaryKey cols) (UniqueConstraintOptions fname) =
        conKeyword fname
          <> " PRIMARY KEY ("
          <> T.intercalate ", " (map (sqlEscaped . columnName) (S.toList cols))
          <> ")"
    renderCreateForeignKeyConstraint :: ForeignKey -> ForeignKeyConstraintOptions -> Text
    renderCreateForeignKeyConstraint (ForeignKey (tableName -> tName) (S.toList -> colPair)) constr  =
        let (fkCols, referenced) =
              ( map (sqlEscaped . columnName . fst) colPair,
                map (sqlEscaped . columnName . snd) colPair
              )
         in conKeyword (foreignKeyConstraintName constr)
              <> " FOREIGN KEY ("
              <> T.intercalate ", " fkCols
              <> ") REFERENCES "
              <> sqlEscaped tName
              <> "("
              <> T.intercalate ", " referenced
              <> ")"
              <> renderAction "ON DELETE" (onDelete constr)
              <> renderAction "ON UPDATE" (onUpdate constr)

    conKeyword = \case
      Nothing -> ""
      Just (ConstraintName fname) -> "CONSTRAINT " <> sqlEscaped fname

    renderDropConstraint :: ConstraintName -> Text
    renderDropConstraint = mappend "DROP CONSTRAINT " . sqlEscaped . unConsraintName

    renderAction actionPrefix = \case
      NoAction -> mempty
      Cascade -> " " <> actionPrefix <> " " <> "CASCADE "
      Restrict -> " " <> actionPrefix <> " " <> "RESTRICT "
      SetNull -> " " <> actionPrefix <> " " <> "SET NULL "
      SetDefault -> " " <> actionPrefix <> " " <> "SET DEFAULT "

    createTypeSyntax :: EnumerationName -> Enumeration -> Pg.PgSyntax
    createTypeSyntax (EnumerationName ty) (Enumeration vals) =
      Pg.emit $
        toS $
          "CREATE TYPE " <> ty <> " AS ENUM (" <> T.intercalate "," (map sqlSingleQuoted vals) <> ");\n"

    createSequenceSyntax :: SequenceName -> Maybe Sequence -> Pg.PgSyntax
    createSequenceSyntax (SequenceName sName) sOwner = Pg.emit $ toS
      $ "CREATE SEQUENCE " <> sqlEscaped sName
      <> sequenceOwnerSyntax sOwner
      <> ";\n"

    renameSequenceSyntax :: SequenceName -> SequenceName -> Pg.PgSyntax
    renameSequenceSyntax (SequenceName sName) (SequenceName sName') = Pg.emit $ toS
      $ "ALTER SEQUENCE " <> sqlEscaped sName
      <> "RENAME TO " <> sqlEscaped sName'
      <> ";\n"

    setSequenceOwnerSyntax :: SequenceName -> Maybe Sequence -> Pg.PgSyntax
    setSequenceOwnerSyntax (SequenceName sName) sOwner = Pg.emit $ toS
      $ "ALTER SEQUENCE " <> sqlEscaped sName
      <> sequenceOwnerSyntax sOwner
      <> ";\n"


    sequenceOwnerSyntax :: Maybe Sequence -> Text
    sequenceOwnerSyntax sOwner = " OWNED BY " <> case sOwner of
      Nothing -> " NONE "
      Just (Sequence (tableName -> tName) (columnName -> cName)) ->
        sqlEscaped tName <> "." <> sqlEscaped cName

    dropSequenceSyntax :: SequenceName -> Pg.PgSyntax
    dropSequenceSyntax (SequenceName s) = Pg.emit $ toS $ "DROP SEQUENCE " <> sqlEscaped s <> ";\n"

renderStdType :: AST.DataType -> Text
renderStdType = \case
  -- From the Postgres' documentation:
  -- \"character without length specifier is equivalent to character(1).\"
  (AST.DataTypeChar False prec charSet) ->
    "CHAR" <> sqlOptPrec (Just $ fromMaybe 1 prec) <> sqlOptCharSet charSet
  (AST.DataTypeChar True prec charSet) ->
    "VARCHAR" <> sqlOptPrec prec <> sqlOptCharSet charSet
  (AST.DataTypeNationalChar varying prec) ->
    let ty = if varying then "NATIONAL CHARACTER VARYING" else "NATIONAL CHAR"
     in ty <> sqlOptPrec prec
  (AST.DataTypeBit varying prec) ->
    let ty = if varying then "BIT VARYING" else "BIT"
     in ty <> sqlOptPrec prec
  (AST.DataTypeNumeric prec) -> "NUMERIC" <> sqlOptNumericPrec prec
  -- Even though beam emits 'DOUBLE here'
  -- (see: https://github.com/tathougies/beam/blob/b245bf2c0b4c810dbac334d08ca572cec49e4d83/beam-postgres/Database/Beam/Postgres/Syntax.hs#L544)
  -- the \"double\" type doesn't exist in Postgres.
  -- Rather, the "NUMERIC" and "DECIMAL" types are equivalent in Postgres, and that's what we use here.
  (AST.DataTypeDecimal prec) -> "NUMERIC" <> sqlOptNumericPrec prec
  AST.DataTypeInteger -> "INT"
  AST.DataTypeSmallInt -> "SMALLINT"
  AST.DataTypeBigInt -> "BIGINT"
  (AST.DataTypeFloat prec) -> "FLOAT" <> sqlOptPrec prec
  AST.DataTypeReal -> "REAL"
  AST.DataTypeDoublePrecision -> "DOUBLE PRECISION"
  AST.DataTypeDate -> "DATE"
  (AST.DataTypeTime prec withTz) -> wTz withTz "TIME" prec <> sqlOptPrec prec
  (AST.DataTypeTimeStamp prec withTz) -> wTz withTz "TIMESTAMP" prec <> sqlOptPrec prec
  (AST.DataTypeInterval _i) ->
    error $
      "Impossible: DataTypeInterval doesn't map to any SQLXX beam typeclass, so we don't know"
        <> " how to render it."
  (AST.DataTypeIntervalFromTo _from _to) ->
    error $
      "Impossible: DataTypeIntervalFromTo doesn't map to any SQLXX beam typeclass, so we don't know"
        <> " how to render it."
  AST.DataTypeBoolean -> "BOOL"
  AST.DataTypeBinaryLargeObject -> "BYTEA"
  AST.DataTypeCharacterLargeObject -> "TEXT"
  (AST.DataTypeArray dt sz) ->
    renderStdType dt <> "[" <> T.pack (show sz) <> "]"
  (AST.DataTypeRow _rows) ->
    error "DataTypeRow not supported both for beam-postgres and this library."
  (AST.DataTypeDomain nm) -> "\"" <> nm <> "\""
  where
    wTz withTz tt prec =
      tt <> sqlOptPrec prec <> (if withTz then " WITH" else " WITHOUT") <> " TIME ZONE"

-- -- as a special case, when adding a column, we can potentially specify SERIAL, and save some effort creating a name for a sequence and setting it's ownership up.k
renderDataTypeAdd :: Column -> Text
renderDataTypeAdd col =
  let
    autoIncrement = Just (Autoincrement Nothing) == columnDefault (columnConstraints col)
    dataType = case (autoIncrement, (columnType col)) of
      (False, cType) -> renderDataType cType
      (True, SqlStdType AST.DataTypeInteger) -> " SERIAL "
      (True, SqlStdType AST.DataTypeSmallInt) -> " SMALLSERIAL "
      (True, SqlStdType AST.DataTypeBigInt) -> " BIGSERIAL "
      (True, dtype) -> error $ "inferred illegal autoincrement column: " <> show dtype
  in mconcat
    [ dataType
    , case columnNullable $ columnConstraints col of
      NotNull -> " NOT NULL "
      Null -> " "
    , foldMap renderColumnDefault $ columnDefault $ columnConstraints col

    ]

-- | render the part of a columnconstraint that sets its' default.  This
-- renders nothing if the sequence name is not known; and so should not be
-- called if that's possible outside of a context that also adds the column as
-- a SERIAL type.   It's up to the caller to prefix SET or DROP when needed.
renderColumnDefault :: DefaultConstraint -> Text
renderColumnDefault = \case
  Autoincrement Nothing -> "" -- Don't render the default, use "serial/bigserial" as tye type instead
  defValue -> " DEFAULT " <> case defValue of
    DefaultExpr defValueExpr -> defValueExpr
    Autoincrement x -> flip foldMap x $ \(SequenceName sName) ->
      "nextval(" <> sqlSingleQuoted sName <> "::regclass)"


-- 
-- This function also overlaps with beam-migrate functionalities.
renderDataType :: ColumnType -> Text
renderDataType = \case
  SqlStdType stdType -> renderStdType stdType
  -- text-based enum types
  DbEnumeration (EnumerationName _) _ ->
    renderDataType (SqlStdType (AST.DataTypeChar True Nothing Nothing))
  -- Json types
  PgSpecificType PgJson -> toS $ displaySyntax Pg.pgJsonType
  PgSpecificType PgJsonB -> toS $ displaySyntax Pg.pgJsonbType
  -- Range types
  PgSpecificType PgRangeInt4 -> toS $ Pg.rangeName @Pg.PgInt4Range
  PgSpecificType PgRangeInt8 -> toS $ Pg.rangeName @Pg.PgInt8Range
  PgSpecificType PgRangeNum -> toS $ Pg.rangeName @Pg.PgNumRange
  PgSpecificType PgRangeTs -> toS $ Pg.rangeName @Pg.PgTsRange
  PgSpecificType PgRangeTsTz -> toS $ Pg.rangeName @Pg.PgTsTzRange
  PgSpecificType PgRangeDate -> toS $ Pg.rangeName @Pg.PgDateRange
  -- UUID
  PgSpecificType PgUuid -> toS $ displaySyntax Pg.pgUuidType
  -- enumerations
  PgSpecificType (PgEnumeration (EnumerationName ty)) -> ty
  -- oid
  PgSpecificType PgOid -> "oid"

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
  put $ sortAutomaticEdits edits
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
    Right () -> return $ unlines $ map displaySyntax $ editsToPgSyntax sortedEdits

printMigrationIO :: Migration Pg.Pg -> IO ()
printMigrationIO mig = Pg.runBeamPostgres (undefined :: Pg.Connection) $ printMigration mig

editToSqlCommand :: Edit -> Pg.PgCommandSyntax
editToSqlCommand = Pg.PgCommandSyntax Pg.PgCommandTypeDdl . toSqlSyntax

prettyEditSQL :: Edit -> Text
prettyEditSQL = T.pack . displaySyntax . Pg.fromPgCommand . editToSqlCommand

prettyEditActionDescription :: EditAction -> Text
prettyEditActionDescription = T.unwords . \case
  EditAction_Automatic ea -> case ea of
    TableAdded tblName table ->
      ["create table:", qt tblName, "\n", pshow' table]
    RenameConstraint tblName oldConstraint newConstraint ->
      ["constraint ", qn oldConstraint, " on table ", qt tblName," renamed to: ", qn newConstraint]
    TableRemoved tblName ->
      ["remove table:", qt tblName]
    PrimaryKeyAdded tblName tableConstraint constraintOptions ->
      ["add table constraint to:", qt tblName, "\n", pshow' tableConstraint, " ", pshow' constraintOptions]
    UniqueConstraintAdded tblName tableConstraint constraintOptions ->
      ["add table constraint to:", qt tblName, "\n", pshow' tableConstraint, " ", pshow' constraintOptions]
    ForeignKeyAdded tblName tableConstraint constraintOptions ->
      ["add table constraint to:", qt tblName, "\n", pshow' tableConstraint, " ", pshow' constraintOptions]
    TableConstraintRemoved tblName tableConstraint _ ->
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
        renderDataType oldColumnType,
        "\nto:",
        renderDataType newColumnType
      ]
    ColumnNullableChanged tblName colName nullConstr ->
      ["column:", qq tblName colName, " chanted to ", pshow' nullConstr]
    ColumnDefaultChanged tblName colName dfltConstr ->
      ["column:", qq tblName colName, " default changed to ", pshow' dfltConstr]
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
    SequenceRenamed sequenceName sequence0 ->
      ["renamed sequence:", qs sequenceName, " to:", qs sequence0]
    SequenceRemoved sequenceName ->
      ["remove sequence:", qs sequenceName]
    SequenceSetOwner sequenceName sequence0 ->
      ["alter sequence:", qs sequenceName, pshow' sequence0]
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
    qn = q . unConsraintName
    qq t c = qt t <> "." <> qc c

    pshow' :: Show a => a -> Text
    pshow' = LT.toStrict . PS.pShow

prettyPrintEdits :: [Edit] -> IO ()
prettyPrintEdits edits = putStrLn $ T.unpack $ T.unlines $ fmap prettyEditSQL edits

-- | Compare the existing schema in the database with the expected
-- schema in Haskell and try to edit the existing schema as necessary
tryRunMigrationsWithEditUpdate
  :: ( Generic (db (DatabaseEntity be db))
     , Generic (db (AnnotatedDatabaseEntity be db))
     , Database be db
     , GZipDatabase be
        (AnnotatedDatabaseEntity be db)
        (AnnotatedDatabaseEntity be db)
        (DatabaseEntity be db)
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (DatabaseEntity be db)))
     , GSchema be db '[] (Rep (db (AnnotatedDatabaseEntity be db)))
     )
  => ([WithPriority Edit] -> [WithPriority Edit])
  -> AnnotatedDatabaseSettings be db
  -> Pg.Connection
  -> IO ()
tryRunMigrationsWithEditUpdate editUpdate annotatedDb conn = do

    let expectedHaskellSchema = fromAnnotatedDbSettings annotatedDb (Proxy @'[])

    actualDatabaseSchema <- getSchema conn

    case diffSorted expectedHaskellSchema actualDatabaseSchema of
      Left err -> do
        putStrLn "Error detecting database migration requirements: "
        print err
      Right [] -> do
        putStrLn "No database migration required, continuing startup."
      Right edits -> do
        putStrLn "Database migration required, attempting..."
        let edits' = withoutPriority <$> editUpdate edits
        prettyPrintEdits edits'
        tryRunEdits conn edits'


-- | Compare the existing schema in the database with the expected
-- schema in Haskell and try to edit the existing schema as necessary
tryRunMigrations
  :: ( Generic (db (DatabaseEntity be db))
     , Generic (db (AnnotatedDatabaseEntity be db))
     , Database be db
     , GZipDatabase be
        (AnnotatedDatabaseEntity be db)
        (AnnotatedDatabaseEntity be db)
        (DatabaseEntity be db)
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (DatabaseEntity be db)))
     , GSchema be db '[] (Rep (db (AnnotatedDatabaseEntity be db)))
     )
  => AnnotatedDatabaseSettings be db
  -> Pg.Connection
  -> IO ()
tryRunMigrations = tryRunMigrationsWithEditUpdate Prelude.id

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
    let expectedHaskellSchema = fromAnnotatedDbSettings annotatedDb (Proxy @'[])
    actualDatabaseSchema <- getSchema conn
    pure $ diff expectedHaskellSchema actualDatabaseSchema

newtype MigrateT be db m a = MigrateT { unMigrateT :: ExceptT MigrationError (RWST (AnnotatedDatabaseSettings be db) [Edit] Schema m) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadError MigrationError
    , MonadReader (AnnotatedDatabaseSettings be db)
    , MonadWriter [Edit]
    , MonadState Schema
    )

runMigrateT :: AnnotatedDatabaseSettings be db -> Schema -> MigrateT be db m a -> m (Either MigrationError a, Schema, [Edit])
runMigrateT hsSchema dbSchema (MigrateT (ExceptT (RWST x))) = x hsSchema dbSchema

class
    ( MonadError MigrationError m
    , MonadReader (AnnotatedDatabaseSettings be db) m
    , MonadWriter [Edit] m
    , MonadState Schema m
    ) => MonadMigrate be db m | m -> be db where
  -- | perform the given edits unconditionally.
  editAlways :: [Edit] -> m ()
  editAlways edits = do
    s <- get
    tell edits
    case applyEdits ((<$ WithPriority ((), Priority 0)) <$> edits) s of
      Left bad -> throwError $ BadEdit bad
      Right s' -> do
        put s'

  -- | compute the remaining edits.  results are not sorted
  remainingEdits :: m [AutomaticEditAction]
  remainingEdits = do
    actualDatabaseSchema <- actualSchema
    expectedHaskellSchema <- expectedSchema
    case diff expectedHaskellSchema actualDatabaseSchema of
      Left bad -> throwError $ DiffFailed bad
      Right good -> pure good

  -- | partially migrated schema
  actualSchema :: m Schema
  actualSchema = get

  -- | the schema we're migrating to
  expectedSchema :: m Schema

instance
    ( Monad m
    , Generic (db (DatabaseEntity be db))
    , Generic (db (AnnotatedDatabaseEntity be db))
    , Database be db
    , GZipDatabase be
        (AnnotatedDatabaseEntity be db)
        (AnnotatedDatabaseEntity be db)
        (DatabaseEntity be db)
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (DatabaseEntity be db)))
    , (GSchema be db '[] (Rep (db (AnnotatedDatabaseEntity be db))))
    ) => MonadMigrate be db (MigrateT be db m) where
  expectedSchema = asks $ \annotatedDb -> fromAnnotatedDbSettings annotatedDb (Proxy @'[])

migrateNicely
  :: ( Generic (db (DatabaseEntity be db))
     , Generic (db (AnnotatedDatabaseEntity be db))
     , Database be db
     , GZipDatabase be
        (AnnotatedDatabaseEntity be db)
        (AnnotatedDatabaseEntity be db)
        (DatabaseEntity be db)
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (AnnotatedDatabaseEntity be db)))
        (Rep (db (DatabaseEntity be db)))
     , GSchema be db '[] (Rep (db (AnnotatedDatabaseEntity be db)))
     )
  => AnnotatedDatabaseSettings be db
  -> (forall m. MonadMigrate be db m => m a)
  -> Pg.Connection
  -> IO a
migrateNicely annotatedDb editMigration conn = do

  actualDatabaseSchema <- getSchema conn


  let (res, _schema, unsafeEdits) = runIdentity $ runMigrateT annotatedDb actualDatabaseSchema $ do
        x <- editMigration
        remainingEdits >>= editAlways . fmap withoutPriority . sortAutomaticEdits
        pure x

  case unsafeEdits of
    [] -> putStrLn "No database migration required, continuing startup."
    _ -> do
      putStrLn "Database migration required, attempting..."
      prettyPrintEdits unsafeEdits

  case res of
    Left err -> do
      putStrLn "Error detecting database migration requirements: "
      print err
      throwIO err
    Right x -> do
      unless (null unsafeEdits) $ tryRunEdits conn unsafeEdits
      pure x

doesColumnExist :: MonadMigrate be db m => TableName -> ColumnName -> m Bool
doesColumnExist tName cName = isJust <$> preuse (_schemaTables . ix tName . _tableColumns . ix cName)

dropColumn, dropColumnIfExists :: MonadMigrate be db m => TableName -> ColumnName -> m ()
dropColumn tName cName = editAlways [Edit (EditAction_Automatic $ ColumnRemoved tName cName) (Right Safe)]

dropColumnIfExists tName cName = do
  x <- doesColumnExist tName cName
  when x $ dropColumn tName cName

renameColumn, renameColumnIfExists :: MonadMigrate be db m => TableName -> ColumnName -> ColumnName -> m ()
renameColumn tName cName cName' = editAlways [Edit (EditAction_Manual $ ColumnRenamed tName cName cName') (Right Safe)]

renameColumnIfExists tName cName cName' = do
  x <- doesColumnExist tName cName
  when x $ renameColumn tName cName cName'

doesForeignKeyExist :: MonadMigrate be db m => TableName -> ForeignKey -> m Bool
doesForeignKeyExist tName fkey = isJust <$> preuse (_schemaTables . ix tName . _tableConstraints . _foreignKeyConstraints . ix fkey)

dropForeignKey :: MonadMigrate be db m => TableName -> ForeignKey -> m ()
dropForeignKey tName fkey = do
  fkeyOpts <- preuse (_schemaTables . ix tName . _tableConstraints . _foreignKeyConstraints . ix fkey)
  case fkeyOpts >>= foreignKeyConstraintName of
    Nothing -> throwError (UserError $ "dropForeignKey: constraint not found" <> show tName <> " " <> show fkey)
    Just fkeyName -> editAlways [Edit (EditAction_Automatic $ TableConstraintRemoved tName fkeyName TableConstraintRemovedType_ForeignKey) (Right Safe)]

dropForeignKeyIfExists :: MonadMigrate be db m => TableName -> ForeignKey -> m ()
dropForeignKeyIfExists tName fkey = do
  x <- doesForeignKeyExist tName fkey
  when x $ dropForeignKey tName fkey
