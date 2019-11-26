{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
module Database.Beam.Migrate
  ( fromDbSettings
  , Migration
  , migrate
  , runMigration
  , printMigration
  , createMigration
    -- * Handy re-exports
  , module Exports
  )
where

import           Control.Exception (throwIO)
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Data.String                    ( fromString )
import qualified Data.Set                      as S
import qualified Data.Map.Strict               as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import           GHC.Generics hiding (prec)


import           Database.Beam                  ( MonadBeam )
import           Database.Beam.Schema           ( DatabaseSettings )

import           Database.Beam.Migrate.Generic as Exports
import           Database.Beam.Migrate.Types   as Exports
import           Database.Beam.Migrate.Diff    as Exports
import           Database.Beam.Migrate.Postgres ( getSchema )
import qualified Database.Beam.Backend.SQL.AST as AST

import Database.Beam.Backend.SQL hiding (tableName)

import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Postgres.Syntax as Pg

--
-- Potential API (sketched)
--

-- | Turns a Beam's 'DatabaseSettings' into a 'Schema'.
fromDbSettings :: (Generic (DatabaseSettings be db), GSchema (Rep (DatabaseSettings be db)))
               => DatabaseSettings be db
               -> Schema
fromDbSettings = gSchema . from

type Migration m = ExceptT DiffError (StateT [Edit] m) ()

migrate :: MonadIO m => Pg.Connection -> Schema -> Migration m
migrate conn hsSchema = do
    dbSchema <- lift . liftIO $ getSchema conn
    let schemaDiff = diff hsSchema dbSchema
    case schemaDiff of
      Left e -> throwError e
      Right edits -> lift (put edits)

-- | Runs the input 'Migration'.
-- NOTE(adn) Currently this works only against the \"beam-postgres\" backend, as we are using the concrete
-- 'PgCommandSyntax' here.
runMigration :: (MonadBeam Pg.Postgres m, MonadIO m) => Migration m -> m ()
runMigration m = do
  migs <- evalMigration m
  case migs of
    Left e -> liftIO $ throwIO e
    Right edits -> mapM_ runNoReturn (map toSqlSyntax edits)

toSqlSyntax :: Edit -> Pg.PgCommandSyntax
toSqlSyntax = \case
  TableAdded tblName tbl -> 
      ddlSyntax ("CREATE TABLE \"" <> tableName tblName 
                                   <> "\" (" 
                                   <> T.intercalate "," (map renderTableColumn (M.toList (tableColumns tbl)) <>
                                                         map renderCreateTableConstraint (S.toList (tableConstraints tbl))
                                                        )
                                   <> ")"
                )
  TableRemoved tblName    -> 
      ddlSyntax ("DROP TABLE \"" <> tableName tblName <> "\"")
  TableConstraintAdded  tblName cstr -> 
      updateSyntax (alterTable tblName <> "ADD CONSTRAINT " <> renderAlterTableConstraint cstr)
  TableConstraintRemoved tblName cstr -> 
      updateSyntax (alterTable tblName <> "DROP CONSTRAINT " <> renderAlterTableConstraint cstr)
  ColumnAdded tblName colName col ->
      updateSyntax (alterTable tblName <> "ADD COLUMN \"" 
                                       <> columnName colName 
                                       <> "\" " 
                                       <> renderDataType (columnType col)
                                       <> " "
                                       <> T.intercalate " " (map renderColumnConstraint (S.toList $ columnConstraints col))
                   )
  ColumnRemoved tblName colName ->
      updateSyntax (alterTable tblName <> "DROP COLUMN \"" <> columnName colName <> "\"")
  ColumnTypeChanged tblName colName _old new ->
      updateSyntax (alterTable tblName <> "ALTER COLUMN \"" <> columnName colName <> "\" TYPE " <> renderDataType new)
  ColumnConstraintAdded tblName colName cstr -> 
      updateSyntax (alterTable tblName <> "ALTER COLUMN \"" <> columnName colName <> "\" SET " <> renderColumnConstraint cstr)
  ColumnConstraintRemoved tblName colName cstr ->
      updateSyntax (alterTable tblName <> "ALTER COLUMN \"" <> columnName colName <> "\" DROP " <> renderColumnConstraint cstr)
  where
      ddlSyntax query    = Pg.PgCommandSyntax Pg.PgCommandTypeDdl (Pg.emit . TE.encodeUtf8 $ query <> ";")
      updateSyntax query = Pg.PgCommandSyntax Pg.PgCommandTypeDataUpdate (Pg.emit . TE.encodeUtf8 $ query <> ";")

      alterTable :: TableName -> Text
      alterTable (TableName tName) = "ALTER TABLE \"" <> tName <> "\" "

      renderTableColumn :: (ColumnName, Column) -> Text
      renderTableColumn (colName, col) = columnName colName <> " " <> renderDataType (columnType col)

      renderCreateTableConstraint :: TableConstraint -> Text
      renderCreateTableConstraint = \case
        Unique _ cols     -> "UNIQUE (" <> T.intercalate "," (map columnName (S.toList cols)) <> ")"
        PrimaryKey _ cols -> "PRIMARY KEY (" <> T.intercalate "," (map columnName (S.toList cols)) <> ")"
        _ -> error "renderTableConstraint: TODO"

      renderAlterTableConstraint :: TableConstraint -> Text
      renderAlterTableConstraint = \case
        Unique cName _ -> cName
        PrimaryKey cName _ -> cName
        _ -> error "renderTableConstraint: TODO"

      renderColumnConstraint :: ColumnConstraint -> Text
      renderColumnConstraint = \case
        NotNull -> "NOT NULL"
        Default defValue -> "DEFAULT " <> defValue

      -- This function also overlaps with beam-migrate functionalities.
      renderDataType :: ColumnType -> Text
      renderDataType = \case
        AST.DataTypeChar varying prec charSet ->
            let ty = if varying then "VARCHAR" else "CHAR"
            in ty <> sqlOptPrec prec <> sqlOptCharSet charSet
        AST.DataTypeNationalChar varying prec -> 
            let ty = if varying then "NATIONAL CHARACTER VARYING" else "NATIONAL CHAR" 
            in ty <> sqlOptPrec prec
        AST.DataTypeBit varying prec -> 
            let ty = if varying then "BIT VARYING" else "BIT"
            in ty <> sqlOptPrec prec
        AST.DataTypeNumeric prec -> "NUMERIC" <> sqlOptNumericPrec prec
        AST.DataTypeDecimal prec -> "DOUBLE" <> sqlOptNumericPrec prec
        AST.DataTypeInteger -> "INT"
        AST.DataTypeSmallInt -> "SMALLINT"
        AST.DataTypeBigInt -> "BIGINT"
        AST.DataTypeFloat prec -> "FLOAT" <> sqlOptPrec prec
        AST.DataTypeReal -> "REAL"
        AST.DataTypeDoublePrecision -> "DOUBLE PRECISION"
        AST.DataTypeDate -> "DATE"
        AST.DataTypeTime prec withTz ->
          let ty = "TIME" <> sqlOptPrec prec <> if withTz then " WITH TIME ZONE" else mempty
          in ty <> sqlOptPrec prec
        AST.DataTypeTimeStamp prec withTz ->
          let ty = "TIMESTAMP" <> sqlOptPrec prec <> if withTz then " WITH TIME ZONE" else mempty
          in ty <> sqlOptPrec prec
        AST.DataTypeInterval _i -> error "DataTypeInterval not supported yet."
        AST.DataTypeIntervalFromTo _from _to -> error "DataTypeIntervalFromTo not supported yet."
        AST.DataTypeBoolean -> "BOOL"
        AST.DataTypeBinaryLargeObject ->
            error "DataTypeBinaryLargeObject not supported yet."
        AST.DataTypeCharacterLargeObject ->
            error "DataTypeCharacterLargeObject not supported yet."
        AST.DataTypeArray _dt _int ->
            error "DataTypeArray not supported yet."
        AST.DataTypeRow _rows ->
            error "DataTypeRow not supported yet."
        AST.DataTypeDomain nm -> "\"" <> nm <> "\""


-- NOTE(adn) Unfortunately these combinators are not re-exported by beam.

sqlOptPrec :: Maybe Word -> Text
sqlOptPrec Nothing = mempty
sqlOptPrec (Just x) = "(" <> fromString (show x) <> ")"

sqlOptCharSet :: Maybe Text -> Text
sqlOptCharSet Nothing = mempty
sqlOptCharSet (Just cs) = " CHARACTER SET " <> cs

sqlOptNumericPrec :: Maybe (Word, Maybe Word) -> Text
sqlOptNumericPrec Nothing = mempty
sqlOptNumericPrec (Just (prec, Nothing)) = sqlOptPrec (Just prec)
sqlOptNumericPrec (Just (prec, Just dec)) = "(" <> fromString (show prec) <> ", " <> fromString (show dec) <> ")"

evalMigration :: Monad m => Migration m -> m (Either DiffError [Edit])
evalMigration m = do
    (a, s) <- runStateT (runExceptT m) mempty
    case a of
      Left e    -> pure (Left e)
      Right ()  -> pure (Right s)

-- | Create the migration from a 'Diff'.
createMigration :: Monad m => Diff -> Migration m
createMigration (Left e) = throwError e
createMigration (Right edits) = ExceptT $ do
    put edits
    pure (Right ())


printMigration :: MonadIO m => Migration m -> m ()
printMigration m = do
    (a, edits) <- runStateT (runExceptT m) mempty
    case a of
      Left e    -> liftIO $ throwIO e
      Right ()  -> do
        let pgSyntax = map toSqlSyntax edits
        liftIO $ putStrLn (unlines $ map (displaySyntax . Pg.fromPgCommand) pgSyntax)
