{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Beam.AutoMigrate.Eval where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Function ((&))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Database.Beam.Backend.SQL hiding (tableName)
import qualified Database.Beam.Backend.SQL.AST as AST
import Database.Beam.Schema (Database)
import Database.Beam.Schema.Tables (DatabaseEntity (..))
import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg
import Data.Proxy
import GHC.Generics hiding (prec)
import qualified Text.Pretty.Simple as PS

import Database.Beam.AutoMigrate.Annotated
import Database.Beam.AutoMigrate.Diff
import Database.Beam.AutoMigrate.Generic
import Database.Beam.AutoMigrate.Types
import Database.Beam.AutoMigrate.Util hiding (tableName)
import Database.Beam.AutoMigrate.Validity

-- | A database 'Migration'.
type Migration m = ExceptT MigrationError (StateT [WithPriority Edit] m) ()

data MigrationError
  = DiffFailed DiffError
  | HaskellSchemaValidationFailed [ValidationFailed]
  | DatabaseSchemaValidationFailed [ValidationFailed]
  | UnsafeEditsDetected [EditAction]
  deriving (Show)

instance Exception MigrationError

evalMigration :: Monad m => Migration m -> m (Either MigrationError [WithPriority Edit])
evalMigration m = do
  (a, s) <- runStateT (runExceptT m) mempty
  case a of
    Left e -> pure (Left e)
    Right () -> pure (Right s)

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

-- Unfortunately Postgres' syntax is different when setting or dropping constaints. For example when we
-- drop the default value we /don't/ repeat which was the original default value (which makes sense), but
-- doing so means we have to discriminate between these two events to render the SQL fragment correctly.
data AlterTableAction
  = SetConstraint
  | DropConstraint
  deriving (Show, Eq)

editsToPgSyntax :: [WithPriority Edit] -> [Pg.PgSyntax]
editsToPgSyntax = map (toSqlSyntax . fst . unPriority)

-- | Converts a single 'Edit' into the relevant 'PgSyntax' necessary to generate the final SQL.
toSqlSyntax :: Edit -> Pg.PgSyntax
toSqlSyntax e =
  safetyPrefix $ _editAction e & \case
    EditAction_Automatic ea -> case ea of
      TableAdded tblName tbl ->
        ddlSyntax
          ( "CREATE TABLE " <> sqlEscaped (tableName tblName)
              <> " ("
              <> T.intercalate ", " (map renderTableColumn (M.toList (tableColumns tbl)))
              <> ")"
          )
      TableRemoved tblName ->
        ddlSyntax ("DROP TABLE " <> sqlEscaped (tableName tblName))
      TableConstraintAdded tblName cstr ->
        updateSyntax (alterTable tblName <> renderAddConstraint cstr)
      TableConstraintRemoved tblName cstr ->
        updateSyntax (alterTable tblName <> renderDropConstraint cstr)
      SequenceAdded sName (Sequence _tName _cName) -> createSequenceSyntax sName
      SequenceRemoved sName -> dropSequenceSyntax sName
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
              <> renderDataType (columnType col)
              <> " "
              <> T.intercalate " " (map (renderColumnConstraint SetConstraint) (S.toList $ columnConstraints col))
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
      ColumnConstraintAdded tblName colName cstr ->
        updateSyntax
          ( alterTable tblName <> "ALTER COLUMN "
              <> sqlEscaped (columnName colName)
              <> " SET "
              <> renderColumnConstraint SetConstraint cstr
          )
      ColumnConstraintRemoved tblName colName cstr ->
        updateSyntax
          ( alterTable tblName <> "ALTER COLUMN "
              <> sqlEscaped (columnName colName)
              <> " DROP "
              <> renderColumnConstraint DropConstraint cstr
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

    renderTableColumn :: (ColumnName, Column) -> Text
    renderTableColumn (colName, col) =
      sqlEscaped (columnName colName) <> " "
        <> renderDataType (columnType col)
        <> " "
        <> T.intercalate " " (map (renderColumnConstraint SetConstraint) (S.toList $ columnConstraints col))

    renderInsertionOrder :: InsertionOrder -> Text
    renderInsertionOrder Before = "BEFORE"
    renderInsertionOrder After = "AFTER"

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
        let (fkCols, referenced) =
              ( map (sqlEscaped . columnName . fst) colPair,
                map (sqlEscaped . columnName . snd) colPair
              )
         in conKeyword <> sqlEscaped fname
              <> " FOREIGN KEY ("
              <> T.intercalate ", " fkCols
              <> ") REFERENCES "
              <> sqlEscaped tName
              <> "("
              <> T.intercalate ", " referenced
              <> ")"
              <> renderAction "ON DELETE" onDelete
              <> renderAction "ON UPDATE" onUpdate
      where
        conKeyword = "CONSTRAINT "

    renderAddConstraint :: TableConstraint -> Text
    renderAddConstraint = mappend "ADD " . renderCreateTableConstraint

    renderDropConstraint :: TableConstraint -> Text
    renderDropConstraint tc = case tc of
      Unique cName _ -> dropC cName
      PrimaryKey cName _ -> dropC cName
      ForeignKey cName _ _ _ _ -> dropC cName
      where
        dropC = mappend "DROP CONSTRAINT " . sqlEscaped

    renderAction actionPrefix = \case
      NoAction -> mempty
      Cascade -> " " <> actionPrefix <> " " <> "CASCADE "
      Restrict -> " " <> actionPrefix <> " " <> "RESTRICT "
      SetNull -> " " <> actionPrefix <> " " <> "SET NULL "
      SetDefault -> " " <> actionPrefix <> " " <> "SET DEFAULT "

    renderColumnConstraint :: AlterTableAction -> ColumnConstraint -> Text
    renderColumnConstraint act = \case
      NotNull -> "NOT NULL"
      Default defValue | act == SetConstraint -> "DEFAULT " <> defValue
      Default _ -> "DEFAULT"

    createTypeSyntax :: EnumerationName -> Enumeration -> Pg.PgSyntax
    createTypeSyntax (EnumerationName ty) (Enumeration vals) =
      Pg.emit $
        toS $
          "CREATE TYPE " <> ty <> " AS ENUM (" <> T.intercalate "," (map sqlSingleQuoted vals) <> ");\n"

    createSequenceSyntax :: SequenceName -> Pg.PgSyntax
    createSequenceSyntax (SequenceName s) = Pg.emit $ toS $ "CREATE SEQUENCE " <> sqlEscaped s <> ";\n"

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

prettyPrintEdits :: [WithPriority Edit] -> IO ()
prettyPrintEdits edits = putStrLn $ T.unpack $ T.unlines $ fmap (prettyEditSQL . fst . unPriority) (sortEdits edits)

prettyEditSQL :: Edit -> Text
prettyEditSQL = T.pack . displaySyntax . Pg.fromPgCommand . editToSqlCommand

editToSqlCommand :: Edit -> Pg.PgCommandSyntax
editToSqlCommand = Pg.PgCommandSyntax Pg.PgCommandTypeDdl . toSqlSyntax

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
        renderDataType oldColumnType,
        "\nto:",
        renderDataType newColumnType
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
