{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}

{- | This module provides the high-level API to migrate a database. -}

module Database.Beam.Migrate
  ( -- * Annotating a database
    -- $annotatingDbSettings
    defaultAnnotatedDbSettings
    -- * Generating a Schema
    -- $generatingASchema
  , fromAnnotatedDbSettings
  -- * Downcasting an AnnotatedDatabaseSettings into a simple DatabaseSettings
  , deAnnotateDatabase
  -- * Generating and running migrations
  , Migration
  , migrate
  , runMigration
  -- * Creating a migration from a Diff
  , createMigration
  -- * Printing migrations for debugging purposes
  , printMigration
  , printMigrationIO
  -- * Unsafe functions
  , unsafeRunMigration
  -- * Handy re-exports
  , module Exports
  -- * Internals
  , FromAnnotated
  , ToAnnotated
  , sqlSingleQuoted
  , sqlEscaped
  )
where

import           Control.Exception                        ( throwIO, Exception )
import           Control.Monad.Identity                   (runIdentity)
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.IO.Class                   ( liftIO
                                                          , MonadIO
                                                          )
import           Lens.Micro                               ( (^.) )
import           Data.Proxy
import           Data.Maybe                               ( fromMaybe )
import           Data.String.Conv                         ( toS )
import           Data.String                              ( fromString )
import qualified Data.Set                                as S
import qualified Data.Map.Strict                         as M
import           Data.Text                                ( Text )
import           Data.Bifunctor                           ( first )
import qualified Data.Text                               as T
import qualified Data.Text.Encoding                      as TE

import           GHC.Generics                      hiding ( prec )


import           Database.Beam                            ( MonadBeam )
import           Database.Beam.Schema                     ( Database
                                                          , DatabaseSettings
                                                          )
import           Database.Beam.Schema.Tables              ( DatabaseEntity(..)
                                                          )

import           Database.Beam.Migrate.Annotated         as Exports
import           Database.Beam.Migrate.Generic           as Exports
import           Database.Beam.Migrate.Types             as Exports
import           Database.Beam.Migrate.Diff              as Exports
import           Database.Beam.Migrate.Compat            as Exports
import           Database.Beam.Migrate.Validity          as Exports
import           Database.Beam.Migrate.Postgres           ( getSchema )
import qualified Database.Beam.Backend.SQL.AST           as AST

import           Database.Beam.Backend.SQL         hiding ( tableName )

import qualified Database.PostgreSQL.Simple              as Pg
import qualified Database.Beam.Postgres                  as Pg
import qualified Database.Beam.Postgres.Syntax           as Pg

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
  ( Generic (db (e1 be db))
  , Generic (db (e2 be db))
  , Database be db
  , GZipDatabase be (e1 be db)            (e2 be db)            (e2 be db)
                    (Rep (db (e1 be db))) (Rep (db (e2 be db))) (Rep (db (e2 be db)))
  )

-- | Simple class to make the signatures for 'defaultAnnotatedDbSettings' and 'fromAnnotatedDbSettings'
-- less scary. From a user's standpoint, there is nothing you have to implement.
type FromAnnotated (be :: *) (db :: DatabaseKind) e1 e2 =
  ( Generic (db (e1 be db))
  , Generic (db (e2 be db))
  , Database be db
  , GZipDatabase be (e2 be db)            (e2 be db)            (e1 be db)
                    (Rep (db (e2 be db))) (Rep (db (e2 be db))) (Rep (db (e1 be db)))
  )

-- | Turns a Beam's 'DatabaseSettings' into an 'AnnotatedDatabaseSettings'.
defaultAnnotatedDbSettings :: forall be db. ToAnnotated be db DatabaseEntity AnnotatedDatabaseEntity
                           => DatabaseSettings be db
                           -> AnnotatedDatabaseSettings be db
defaultAnnotatedDbSettings db = runIdentity $
  zipTables (Proxy @be) annotate db (undefined :: AnnotatedDatabaseSettings be db)
  where
    annotate :: ( Monad m
                , IsAnnotatedDatabaseEntity be ty
                , AnnotatedDatabaseEntityRegularRequirements be ty)
             => DatabaseEntity be db ty
             -> AnnotatedDatabaseEntity be db ty
             -> m (AnnotatedDatabaseEntity be db ty)
    annotate (DatabaseEntity edesc) _ =
      pure $ AnnotatedDatabaseEntity (dbAnnotatedEntityAuto edesc) (DatabaseEntity edesc)

-- | Downcast an 'AnnotatedDatabaseSettings' into Beam's standard 'DatabaseSettings'.
deAnnotateDatabase :: forall be db. FromAnnotated be db DatabaseEntity AnnotatedDatabaseEntity
                   => AnnotatedDatabaseSettings be db
                   -> DatabaseSettings be db
deAnnotateDatabase db =
  runIdentity $ zipTables (Proxy @be) (\ann _ -> pure $ ann ^. deannotate) db db

-- $generatingASchema
-- Once you have an 'AnnotatedDatabaseSettings', you can produce a 'Schema' simply by calling
-- 'fromAnnotatedDbSettings'. The second parameter can be used to selectively turn off automatic FK-discovery
-- for one or more tables. For more information about specifying your own table constraints, refer to the
-- 'Database.Beam.Migrate.Annotated' module.

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
--
fromAnnotatedDbSettings :: ( FromAnnotated be db DatabaseEntity AnnotatedDatabaseEntity
                           , GSchema be db anns (Rep (AnnotatedDatabaseSettings be db))
                           )
                        => AnnotatedDatabaseSettings be db
                        -> Proxy (anns :: [Annotation])
                        -> Schema
fromAnnotatedDbSettings db p = gSchema db p (from db)

editsToPgSyntax :: [WithPriority Edit] -> [Pg.PgSyntax]
editsToPgSyntax = map (toSqlSyntax . fst . unPriority)

-- | A database 'Migration'.
type Migration m = ExceptT MigrationError (StateT [WithPriority Edit] m) ()

data MigrationError =
    DiffFailed DiffError
  | HaskellSchemaValidationFailed  [ValidationFailed]
  | DatabaseSchemaValidationFailed [ValidationFailed]
  deriving Show

instance Exception MigrationError

-- | Given a 'Connection' to a database and a 'Schema' (which can be generated using 'fromAnnotatedDbSettings')
-- it returns a 'Migration', which can then be executed via 'runMigration'.
migrate :: MonadIO m => Pg.Connection -> Schema -> Migration m
migrate conn hsSchema = do
    dbSchema <- lift . liftIO $ getSchema conn
    liftEither $ first HaskellSchemaValidationFailed  (validateSchema hsSchema)
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

-- | Runs the input 'Migration' in a concrete 'Postgres' backend.
runMigration :: MonadBeam Pg.Postgres Pg.Pg => Pg.Connection -> Migration Pg.Pg -> IO ()
runMigration conn mig = Pg.withTransaction conn $ Pg.runBeamPostgres conn (unsafeRunMigration mig)

-- Unfortunately Postgres' syntax is different when setting or dropping constaints. For example when we
-- drop the default value we /don't/ repeat which was the original default value (which makes sense), but
-- doing so means we have to discriminate between these two events to render the SQL fragment correctly.
data AlterTableAction =
    SetConstraint
  | DropConstraint
  deriving (Show, Eq)

-- | Converts a single 'Edit' into the relevant 'PgSyntax' necessary to generate the final SQL.
toSqlSyntax :: Edit -> Pg.PgSyntax
toSqlSyntax = \case
  TableAdded tblName tbl ->
      ddlSyntax ("CREATE TABLE " <> sqlEscaped (tableName tblName )
                                   <> " ("
                                   <> T.intercalate ", " (map renderTableColumn (M.toList (tableColumns tbl)))
                                   <> ")"
                )
  TableRemoved tblName    ->
      ddlSyntax ("DROP TABLE " <> sqlEscaped (tableName tblName))
  TableConstraintAdded  tblName cstr ->
      updateSyntax (alterTable tblName <> renderAddConstraint cstr)
  TableConstraintRemoved tblName cstr ->
      updateSyntax (alterTable tblName <> renderDropConstraint cstr)
  EnumTypeAdded   tyName vals -> createTypeSyntax tyName vals
  EnumTypeRemoved    (EnumerationName tyName) -> ddlSyntax ("DROP TYPE " <> tyName)
  EnumTypeValueAdded (EnumerationName tyName) newVal order insPoint ->
      ddlSyntax ("ALTER TYPE " <> tyName
                               <> " ADD VALUE "
                               <> sqlSingleQuoted newVal
                               <> " "
                               <> renderInsertionOrder order
                               <> " "
                               <> sqlSingleQuoted insPoint
                )
  ColumnAdded tblName colName col ->
      updateSyntax (alterTable tblName
                                <> "ADD COLUMN "
                                <> sqlEscaped (columnName colName)
                                <> " "
                                <> renderDataType (columnType col)
                                <> " "
                                <> T.intercalate " " (map (renderColumnConstraint SetConstraint) (S.toList $ columnConstraints col))
                   )
  ColumnRemoved tblName colName ->
      updateSyntax (alterTable tblName <> "DROP COLUMN " <> sqlEscaped (columnName colName))
  ColumnTypeChanged tblName colName _old new ->
      updateSyntax (alterTable tblName <> "ALTER COLUMN "
                                       <> sqlEscaped (columnName colName)
                                       <> " TYPE "
                                       <> renderDataType new)
  ColumnConstraintAdded tblName colName cstr ->
      updateSyntax (alterTable tblName <> "ALTER COLUMN "
                                       <> sqlEscaped (columnName colName)
                                       <> " SET "
                                       <> renderColumnConstraint SetConstraint cstr)
  ColumnConstraintRemoved tblName colName cstr ->
      updateSyntax (alterTable tblName <> "ALTER COLUMN "
                                       <> sqlEscaped (columnName colName)
                                       <> " DROP "
                                       <> renderColumnConstraint DropConstraint cstr)
  where
      ddlSyntax query    = Pg.emit . TE.encodeUtf8 $ query <> ";\n"
      updateSyntax query = Pg.emit . TE.encodeUtf8 $ query <> ";\n"

      alterTable :: TableName -> Text
      alterTable (TableName tName) = "ALTER TABLE " <> sqlEscaped tName <> " "

      renderTableColumn :: (ColumnName, Column) -> Text
      renderTableColumn (colName, col) =
          sqlEscaped (columnName colName) <> " "
                                          <> renderDataType (columnType col)
                                          <> " "
                                          <> T.intercalate " " (map (renderColumnConstraint SetConstraint) (S.toList $ columnConstraints col))

      renderInsertionOrder :: InsertionOrder -> Text
      renderInsertionOrder Before = "BEFORE"
      renderInsertionOrder After  = "AFTER"

      renderCreateTableConstraint :: TableConstraint -> Text
      renderCreateTableConstraint = \case
        Unique fname cols ->
            conKeyword <> sqlEscaped fname
                       <> " UNIQUE ("
                       <> T.intercalate ", " (map (sqlEscaped . columnName) (S.toList cols))
                       <> ")"
        PrimaryKey fname cols ->
            conKeyword <> sqlEscaped fname
                       <> " PRIMARY KEY ("
                       <> T.intercalate ", " (map (sqlEscaped . columnName) (S.toList cols))
                       <> ")"
        ForeignKey fname (tableName -> tName) (S.toList -> colPair) onDelete onUpdate ->
            let (fkCols, referenced) = ( map (sqlEscaped . columnName . fst) colPair
                                       , map (sqlEscaped . columnName . snd) colPair)
            in conKeyword <> sqlEscaped fname
                          <> " FOREIGN KEY ("
                          <> T.intercalate ", " fkCols
                          <> ") REFERENCES " <> sqlEscaped tName
                          <> "(" <> T.intercalate ", " referenced <> ")"
                          <> renderAction "ON DELETE" onDelete
                          <> renderAction "ON UPDATE" onUpdate
        where conKeyword = "CONSTRAINT "

      renderAddConstraint :: TableConstraint -> Text
      renderAddConstraint = mappend "ADD " . renderCreateTableConstraint

      renderDropConstraint :: TableConstraint -> Text
      renderDropConstraint tc = case tc of
        Unique     cName _       -> dropC cName
        PrimaryKey cName _       -> dropC cName
        ForeignKey cName _ _ _ _ -> dropC cName
        where dropC = mappend "DROP CONSTRAINT "

      renderAction actionPrefix = \case
        NoAction   -> mempty
        Cascade    -> " " <> actionPrefix <> " " <> "CASCADE "
        Restrict   -> " " <> actionPrefix <> " " <> "RESTRICT "
        SetNull    -> " " <> actionPrefix <> " " <> "SET NULL "
        SetDefault -> " " <> actionPrefix <> " " <> "SET DEFAULT "

      renderColumnConstraint :: AlterTableAction -> ColumnConstraint -> Text
      renderColumnConstraint act = \case
        NotNull                                 -> "NOT NULL"
        Default defValue | act == SetConstraint -> "DEFAULT " <> defValue
        Default _                               -> "DEFAULT"

      createTypeSyntax :: EnumerationName -> Enumeration -> Pg.PgSyntax
      createTypeSyntax (EnumerationName ty) (Enumeration vals) = Pg.emit $ toS $
          "CREATE TYPE " <> ty <> " AS ENUM (" <> T.intercalate "," (map sqlSingleQuoted vals) <> ");\n"

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
        (AST.DataTypeTime prec withTz) ->
          let ty = "TIME" <> sqlOptPrec prec <> if withTz then " WITH TIME ZONE" else mempty
          in ty <> sqlOptPrec prec
        (AST.DataTypeTimeStamp prec withTz) ->
          let ty = "TIMESTAMP" <> sqlOptPrec prec <> if withTz then " WITH TIME ZONE" else mempty
          in ty <> sqlOptPrec prec
        (AST.DataTypeInterval _i) ->
          error $ "Impossible: DataTypeInterval doesn't map to any SQLXX beam typeclass, so we don't know"
               <> " how to render it."
        (AST.DataTypeIntervalFromTo _from _to) ->
          error $ "Impossible: DataTypeIntervalFromTo doesn't map to any SQLXX beam typeclass, so we don't know"
               <> " how to render it."
        AST.DataTypeBoolean -> "BOOL"
        AST.DataTypeBinaryLargeObject -> "BYTEA"
        AST.DataTypeCharacterLargeObject -> "TEXT"
        (AST.DataTypeArray dt sz) ->
           renderStdType dt <> "[" <> T.pack (show sz) <> "]"
        (AST.DataTypeRow _rows) ->
            error "DataTypeRow not supported both for beam-postgres and this library."
        (AST.DataTypeDomain nm) -> "\"" <> nm <> "\""

      -- This function also overlaps with beam-migrate functionalities.
      renderDataType :: ColumnType -> Text
      renderDataType = \case
        SqlStdType stdType -> renderStdType stdType
        -- text-based enum types
        DbEnumeration (EnumerationName _) _ ->
            renderDataType (SqlStdType (AST.DataTypeChar True Nothing Nothing))
        -- Json types
        PgSpecificType PgJson  -> toS $ displaySyntax Pg.pgJsonType
        PgSpecificType PgJsonB -> toS $ displaySyntax Pg.pgJsonbType
        -- Range types
        PgSpecificType PgRangeInt4 -> toS $ Pg.rangeName @Pg.PgInt4Range
        PgSpecificType PgRangeInt8 -> toS $ Pg.rangeName @Pg.PgInt8Range
        PgSpecificType PgRangeNum  -> toS $ Pg.rangeName @Pg.PgNumRange
        PgSpecificType PgRangeTs   -> toS $ Pg.rangeName @Pg.PgTsRange
        PgSpecificType PgRangeTsTz -> toS $ Pg.rangeName @Pg.PgTsTzRange
        PgSpecificType PgRangeDate -> toS $ Pg.rangeName @Pg.PgDateRange
        -- enumerations
        PgSpecificType (PgEnumeration (EnumerationName ty)) -> ty
        -- serial
        PgSpecificType PgSerial -> toS $ displaySyntax Pg.pgSerialType


-- NOTE(adn) Unfortunately these combinators are not re-exported by beam.

sqlOptPrec :: Maybe Word -> Text
sqlOptPrec Nothing = mempty
sqlOptPrec (Just x) = "(" <> fromString (show x) <> ")"

sqlOptCharSet :: Maybe Text -> Text
sqlOptCharSet Nothing = mempty
sqlOptCharSet (Just cs) = " CHARACTER SET " <> cs

sqlEscaped :: Text -> Text
sqlEscaped t = "\"" <> t <> "\""

sqlSingleQuoted :: Text -> Text
sqlSingleQuoted t = "'" <> t <> "'"

sqlOptNumericPrec :: Maybe (Word, Maybe Word) -> Text
sqlOptNumericPrec Nothing = mempty
sqlOptNumericPrec (Just (prec, Nothing)) = sqlOptPrec (Just prec)
sqlOptNumericPrec (Just (prec, Just dec)) = "(" <> fromString (show prec) <> ", " <> fromString (show dec) <> ")"

evalMigration :: Monad m => Migration m -> m (Either MigrationError [WithPriority Edit])
evalMigration m = do
  (a, s) <- runStateT (runExceptT m) mempty
  case a of
    Left e    -> pure (Left  e)
    Right ()  -> pure (Right s)

-- | Create the migration from a 'Diff'.
createMigration :: Monad m => Diff -> Migration m
createMigration (Left e) = throwError (DiffFailed e)
createMigration (Right edits) = ExceptT $ do
  put edits
  pure (Right ())

-- | Prints the migration to stdout. Useful for debugging and diagnostic.
printMigration :: MonadIO m => Migration m -> m ()
printMigration m = do
  (a, sortedEdits) <- fmap sortEdits <$> runStateT (runExceptT m) mempty
  case a of
    Left e    -> liftIO $ throwIO e
    Right ()  -> liftIO $ putStrLn (unlines . map displaySyntax $ editsToPgSyntax sortedEdits)

printMigrationIO :: Migration Pg.Pg -> IO ()
printMigrationIO mig = Pg.runBeamPostgres (undefined :: Pg.Connection) $ printMigration mig
