{-# LANGUAGE LambdaCase #-}

-- | Convert schema edits to executable PostgreSQL statements.
-- Return 'Left' for invalid names, unsupported types, or invalid edit structures.
-- Treat default expressions as trusted SQL. PostgreSQL checks these expressions.
module Database.Beam.AutoMigrate.Sql
  ( renderDataType,
    editToSqlCommand,
    prettyEditSQL,
  )
where

import Control.Monad (when)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String.Conv (toS)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam.AutoMigrate.Types
import Database.Beam.AutoMigrate.Util (isValidIdentifier, sqlEscaped, sqlOptNumericPrec, sqlOptPrec, sqlSingleQuoted)
import qualified Database.Beam.Backend.SQL.AST as AST
import qualified Database.Beam.Postgres.Syntax as Pg

checkIdent :: Text -> Either RenderError ()
checkIdent t
  | isValidIdentifier t = Right ()
  | otherwise = Left (InvalidSqlIdentifier t)

-- | Check enum labels against identifier limits, but permit an empty label.
checkLabel :: Text -> Either RenderError ()
checkLabel t
  | T.null t || isValidIdentifier t = Right ()
  | otherwise = Left (InvalidSqlIdentifier t)

-- | Render a column type or return a typed error.
-- Reject invalid precision, width, array rank, and vector dimensions.
-- Reject unsupported AST constructors instead of substituting a text type.
renderDataType :: ColumnType -> Either RenderError Text
renderDataType ty0 = case ty0 of
  SqlStdType stdType -> renderStdType stdType
  -- Store text-based enums as unbounded VARCHAR.
  DbEnumeration _ _ -> Right "VARCHAR"
  PgSpecificType pgType -> renderPgType pgType
  SqlArrayType inner dims
    | dims < 1 || dims > 6 ->
        Left (InvalidSqlType ty0 "array rank must be between 1 and 6")
    | isArray inner ->
        Left (InvalidSqlType ty0 "nested array types are not supported")
    | otherwise ->
        (<> T.replicate (fromIntegral dims) "[]") <$> renderDataType inner
  where
    isArray = \case
      SqlArrayType _ _ -> True
      _ -> False

renderPgType :: PgDataType -> Either RenderError Text
renderPgType = \case
  PgJson -> Right "json"
  PgJsonB -> Right "jsonb"
  PgRangeInt4 -> Right "int4range"
  PgRangeInt8 -> Right "int8range"
  PgRangeNum -> Right "numrange"
  PgRangeTs -> Right "tsrange"
  PgRangeTsTz -> Right "tstzrange"
  PgRangeDate -> Right "daterange"
  PgUuid -> Right "uuid"
  PgOid -> Right "oid"
  PgEnumeration eName -> do
    checkIdent (enumName eName)
    Right (sqlEscaped (enumName eName))
  PgLTree -> Right (sqlEscaped "ltree")
  PgVector Nothing -> Right (sqlEscaped "vector")
  PgVector (Just n)
    | n < 1 || n > 16000 ->
        Left (InvalidSqlType (PgSpecificType (PgVector (Just n))) "vector dimensions must be between 1 and 16000")
    | otherwise ->
        Right (sqlEscaped "vector" <> "(" <> T.pack (show n) <> ")")

renderStdType :: AST.DataType -> Either RenderError Text
renderStdType dt = case dt of
  AST.DataTypeChar varying prec charSet -> do
    case charSet of
      Just _ -> Left (bad "explicit character sets are not supported")
      Nothing -> Right ()
    let renderedWidth = if varying then prec else Just (maybe 1 id prec)
    checkBound "CHAR/VARCHAR width" 1 10485760 renderedWidth
    Right $ (if varying then "VARCHAR" else "CHAR") <> sqlOptPrec renderedWidth
  AST.DataTypeNationalChar varying prec -> do
    checkBound "NATIONAL CHAR width" 1 10485760 prec
    Right $ (if varying then "NATIONAL CHARACTER VARYING" else "NATIONAL CHAR") <> sqlOptPrec prec
  AST.DataTypeBit varying prec -> do
    checkBound "BIT width" 1 83886080 prec
    Right $ (if varying then "BIT VARYING" else "BIT") <> sqlOptPrec prec
  AST.DataTypeNumeric prec -> numeric prec
  -- PostgreSQL treats DECIMAL as NUMERIC.
  AST.DataTypeDecimal prec -> numeric prec
  AST.DataTypeInteger -> Right "INT"
  AST.DataTypeSmallInt -> Right "SMALLINT"
  AST.DataTypeBigInt -> Right "BIGINT"
  AST.DataTypeFloat prec -> do
    checkBound "FLOAT precision" 1 53 prec
    Right $ "FLOAT" <> sqlOptPrec prec
  AST.DataTypeReal -> Right "REAL"
  AST.DataTypeDoublePrecision -> Right "DOUBLE PRECISION"
  AST.DataTypeDate -> Right "DATE"
  AST.DataTypeTime prec withTz -> temporal "TIME" prec withTz
  AST.DataTypeTimeStamp prec withTz -> temporal "TIMESTAMP" prec withTz
  AST.DataTypeBoolean -> Right "BOOL"
  AST.DataTypeBinaryLargeObject -> Right "BYTEA"
  AST.DataTypeCharacterLargeObject -> Right "TEXT"
  _ -> Left (bad "unsupported AST data type constructor")
  where
    bad = InvalidSqlType (SqlStdType dt)

    numeric = \case
      Nothing -> Right "NUMERIC"
      Just (prec, mbScale) -> do
        checkBound "NUMERIC precision" 1 1000 (Just prec)
        mapM_ (\s -> checkBound "NUMERIC scale" 0 1000 (Just s)) mbScale
        Right ("NUMERIC" <> sqlOptNumericPrec (Just (prec, mbScale)))

    temporal baseName prec withTz = do
      checkBound "temporal precision" 0 6 prec
      Right $
        baseName <> sqlOptPrec prec
          <> (if withTz then " WITH TIME ZONE" else " WITHOUT TIME ZONE")

    checkBound label lo hi = \case
      Just w | w < lo || w > hi ->
        Left (bad (label <> " must be between " <> T.pack (show lo) <> " and " <> T.pack (show hi)))
      _ -> Right ()

--
-- Edit rendering
--

-- | Convert one 'Edit' to PostgreSQL command syntax.
-- Keep the SQL independent of the safety classification.
-- Report risk labels in logs, not in executable statements.
editToSqlCommand :: Edit -> Either RenderError Pg.PgCommandSyntax
editToSqlCommand e = do
  sql <- renderEditAction (_editAction e)
  pure $ Pg.PgCommandSyntax Pg.PgCommandTypeDdl (Pg.emit (toS (sql <> ";\n")))

renderEditAction :: EditAction -> Either RenderError Text
renderEditAction action = case action of
  EditAction_Automatic ea -> case ea of
    TableAdded tblName tbl -> do
      checkIdent (tableName tblName)
      -- Emit each column with its nullability and default once.
      -- Use separate actions for table constraints.
      cols <- traverse renderDefinition (M.toList (tableColumns tbl))
      pure $ "CREATE TABLE " <> qual tblName <> " (" <> T.intercalate ", " cols <> ")"
    TableRemoved tblName -> do
      checkIdent (tableName tblName)
      pure ("DROP TABLE " <> qual tblName)
    TableConstraintAdded tblName cstr -> do
      checkIdent (tableName tblName)
      rendered <- renderCreateTableConstraint cstr
      pure ("ALTER TABLE " <> qual tblName <> " ADD " <> rendered)
    TableConstraintRemoved tblName cstr -> do
      checkIdent (tableName tblName)
      name <- validateTableConstraint cstr
      pure ("ALTER TABLE " <> qual tblName <> " DROP CONSTRAINT " <> sqlEscaped name)
    SequenceAdded sName (Sequence tName cName) -> do
      checkIdent (seqName sName)
      checkIdent (tableName tName)
      checkIdent (columnName cName)
      pure ("CREATE SEQUENCE " <> sqlEscaped (seqName sName))
    SequenceRemoved sName -> do
      checkIdent (seqName sName)
      pure ("DROP SEQUENCE " <> sqlEscaped (seqName sName))
    EnumTypeAdded tyName vals -> do
      checkIdent (enumName tyName)
      validateLabels vals
      pure
        ( "CREATE TYPE " <> sqlEscaped (enumName tyName)
            <> " AS ENUM (" <> T.intercalate "," (map sqlSingleQuoted (enumValues vals)) <> ")"
        )
    EnumTypeRemoved tyName -> do
      checkIdent (enumName tyName)
      pure ("DROP TYPE " <> sqlEscaped (enumName tyName))
    EnumTypeValueAdded tyName newVal order insPoint -> do
      checkIdent (enumName tyName)
      checkLabel newVal
      checkLabel insPoint
      pure
        ( "ALTER TYPE " <> sqlEscaped (enumName tyName)
            <> " ADD VALUE " <> sqlSingleQuoted newVal <> " "
            <> renderInsertionOrder order <> " " <> sqlSingleQuoted insPoint
        )
    ColumnAdded tblName colName col -> do
      checkIdent (tableName tblName)
      checkIdent (columnName colName)
      definition <- renderColumnDefinition col
      pure $ alterTable tblName <> "ADD COLUMN " <> sqlEscaped (columnName colName) <> " " <> definition
    ColumnRemoved tblName colName -> do
      checkIdent (tableName tblName)
      checkIdent (columnName colName)
      pure $ alterTable tblName <> "DROP COLUMN " <> sqlEscaped (columnName colName)
    ColumnTypeChanged tblName colName _old new -> do
      checkIdent (tableName tblName)
      checkIdent (columnName colName)
      rendered <- renderDataType new
      pure $ alterTable tblName <> "ALTER COLUMN " <> sqlEscaped (columnName colName) <> " TYPE " <> rendered
    ColumnConstraintAdded tblName colName cstr -> do
      checkIdent (tableName tblName)
      checkIdent (columnName colName)
      pure $ alterTable tblName <> "ALTER COLUMN " <> sqlEscaped (columnName colName) <> " SET " <> renderColumnConstraint SetConstraint cstr
    ColumnConstraintRemoved tblName colName cstr -> do
      checkIdent (tableName tblName)
      checkIdent (columnName colName)
      pure $ alterTable tblName <> "ALTER COLUMN " <> sqlEscaped (columnName colName) <> " DROP " <> renderColumnConstraint DropConstraint cstr
  EditAction_Manual ea -> case ea of
    ColumnRenamed tblName oldName newName -> do
      checkIdent (tableName tblName)
      checkIdent (columnName oldName)
      checkIdent (columnName newName)
      pure $ alterTable tblName <> "RENAME COLUMN " <> sqlEscaped (columnName oldName) <> " TO " <> sqlEscaped (columnName newName)
  where
    ctx = action

    qual :: TableName -> Text
    qual tName = sqlEscaped (tableName tName)

    alterTable :: TableName -> Text
    alterTable tName = "ALTER TABLE " <> qual tName <> " "

    renderDefinition :: (ColumnName, Column) -> Either RenderError Text
    renderDefinition (colName, col) = do
      checkIdent (columnName colName)
      definition <- renderColumnDefinition col
      pure (sqlEscaped (columnName colName) <> " " <> definition)

    renderColumnDefinition :: Column -> Either RenderError Text
    renderColumnDefinition col = do
      ty <- renderDataType (columnType col)
      pure $
        ty
          <> T.concat (map (" " <>) (map (renderColumnConstraint SetConstraint) (S.toList (columnConstraints col))))

    -- Check a constraint before returning its name.
    validateTableConstraint :: TableConstraint -> Either RenderError Text
    validateTableConstraint c = do
      let name = constraintName c
      checkIdent name
      case c of
        Unique _ cols -> validateKey c cols
        PrimaryKey _ cols -> validateKey c cols
        ForeignKey _ target pairs _ _ -> do
          checkIdent (tableName target)
          let pairList = S.toList pairs
              locals = map fst pairList
              remotes = map snd pairList
          when (null pairList) $ Left (InvalidSqlEdit ctx "empty foreign key")
          mapM_ (checkIdent . columnName) locals
          mapM_ (checkIdent . columnName) remotes
          when (length (S.fromList locals) /= length locals) $
            Left (InvalidSqlEdit ctx "duplicate local foreign key columns")
          when (length (S.fromList remotes) /= length remotes) $
            Left (InvalidSqlEdit ctx "duplicate remote foreign key columns")
      pure name

    validateKey :: TableConstraint -> S.Set ColumnName -> Either RenderError ()
    validateKey c cols
      | S.null cols = Left (InvalidSqlEdit ctx ("empty key in " <> constraintKindLabel c))
      | otherwise = mapM_ (checkIdent . columnName) (S.toList cols)

    renderCreateTableConstraint :: TableConstraint -> Either RenderError Text
    renderCreateTableConstraint c = do
      name <- validateTableConstraint c
      case c of
        Unique _ cols ->
          pure $ "CONSTRAINT " <> sqlEscaped name <> " UNIQUE (" <> T.intercalate ", " (map (sqlEscaped . columnName) (S.toList cols)) <> ")"
        PrimaryKey _ cols ->
          pure $ "CONSTRAINT " <> sqlEscaped name <> " PRIMARY KEY (" <> T.intercalate ", " (map (sqlEscaped . columnName) (S.toList cols)) <> ")"
        ForeignKey _ target pairs onDelete onUpdate ->
          pure $
            "CONSTRAINT " <> sqlEscaped name
              <> " FOREIGN KEY (" <> T.intercalate ", " (map (sqlEscaped . columnName . fst) (S.toList pairs)) <> ")"
              <> " REFERENCES " <> qual target
              <> "(" <> T.intercalate ", " (map (sqlEscaped . columnName . snd) (S.toList pairs)) <> ")"
              <> renderAction "ON DELETE" onDelete
              <> renderAction "ON UPDATE" onUpdate

    validateLabels :: Enumeration -> Either RenderError ()
    validateLabels (Enumeration vals) = do
      mapM_ checkLabel vals
      when (length vals /= length (S.fromList vals)) $
        Left (InvalidSqlEdit ctx "duplicate enum label")

constraintName :: TableConstraint -> Text
constraintName = \case
  PrimaryKey name _ -> name
  Unique name _ -> name
  ForeignKey name _ _ _ _ -> name

constraintKindLabel :: TableConstraint -> Text
constraintKindLabel = \case
  PrimaryKey {} -> "PRIMARY KEY"
  Unique {} -> "UNIQUE"
  ForeignKey {} -> "FOREIGN KEY"

renderInsertionOrder :: InsertionOrder -> Text
renderInsertionOrder Before = "BEFORE"
renderInsertionOrder After = "AFTER"

-- Select the PostgreSQL syntax for setting or removing a constraint.
data AlterTableAction = SetConstraint | DropConstraint
  deriving (Eq)

renderColumnConstraint :: AlterTableAction -> ColumnConstraint -> Text
renderColumnConstraint act = \case
  NotNull -> "NOT NULL"
  Default defValue | act == SetConstraint -> "DEFAULT " <> defValue
  Default _ -> "DEFAULT"

renderAction :: Text -> ReferenceAction -> Text
renderAction actionPrefix = \case
  NoAction -> mempty
  Cascade -> " " <> actionPrefix <> " CASCADE"
  Restrict -> " " <> actionPrefix <> " RESTRICT"
  SetNull -> " " <> actionPrefix <> " SET NULL"
  SetDefault -> " " <> actionPrefix <> " SET DEFAULT"

-- | Render one edit as executable SQL text without risk labels.
prettyEditSQL :: Edit -> Either RenderError Text
prettyEditSQL e = toS . Pg.pgRenderSyntaxScript . Pg.fromPgCommand <$> editToSqlCommand e
