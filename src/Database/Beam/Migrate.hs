{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
module Database.Beam.Migrate
  ( fromAnnotatedDbSettings
  , defaultAnnotatedDbSettings
  -- * Converting from an annotated to a simple 'DatabaseSettings'
  , deAnnotateDatabase
  -- * Migrations
  , Migration
  , migrate
  , runMigration
  , printMigration
  , createMigration
    -- * Handy re-exports
  , module Exports
  )
where

import           Control.Exception                        ( throwIO )
import           Control.Monad.Identity                   (runIdentity)
import           Control.Monad.State.Strict
import           Control.Monad.Except
import           Control.Monad.IO.Class                   ( liftIO
                                                          , MonadIO
                                                          )
import           Lens.Micro                               ( (^.) )
import           Data.Proxy
import           Data.String                              ( fromString )
import qualified Data.Set                                as S
import qualified Data.Map.Strict                         as M
import           Data.Text                                ( Text )
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
import           Database.Beam.Migrate.Postgres           ( getSchema )
import qualified Database.Beam.Backend.SQL.AST           as AST

import           Database.Beam.Backend.SQL         hiding ( tableName )

import qualified Database.Beam.Postgres                  as Pg
import qualified Database.Beam.Postgres.Syntax           as Pg

--
-- Potential API (sketched)
--

-- | Turns a Beam's 'DatabaseSettings' into an 'AnnotatedDatabaseSettings'.
defaultAnnotatedDbSettings :: forall be db. 
                           ( Generic (db (DatabaseEntity be db))
                           , Generic (db (AnnotatedDatabaseEntity be db))
                           , Database be db 
                           , GZipDatabase be (DatabaseEntity be db)
                                             (AnnotatedDatabaseEntity be db)
                                             (AnnotatedDatabaseEntity be db)
                                             (Rep (db (DatabaseEntity be db)))
                                             (Rep (db (AnnotatedDatabaseEntity be db)))
                                             (Rep (db (AnnotatedDatabaseEntity be db)))
                           )
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

deAnnotateDatabase :: forall be db. 
                   ( Database be db 
                   , Generic (db (DatabaseEntity be db))
                   , Generic (db (AnnotatedDatabaseEntity be db))
                   , GZipDatabase be (AnnotatedDatabaseEntity be db)
                                     (AnnotatedDatabaseEntity be db)
                                     (DatabaseEntity be db)
                                     (Rep (db (AnnotatedDatabaseEntity be db)))
                                     (Rep (db (AnnotatedDatabaseEntity be db)))
                                     (Rep (db (DatabaseEntity be db)))
                   )
                   => AnnotatedDatabaseSettings be db 
                   -> DatabaseSettings be db
deAnnotateDatabase db = 
    runIdentity $ zipTables (Proxy @be) (\ann _ -> pure $ ann ^. deannotate) db db

-- | Turns an 'AnnotatedDatabaseSettings' into a 'Schema'.
fromAnnotatedDbSettings :: ( Database be db
                           , Generic (db (DatabaseEntity be db))
                           , Generic (db (AnnotatedDatabaseEntity be db))
                           , GZipDatabase be (AnnotatedDatabaseEntity be db)
                                             (AnnotatedDatabaseEntity be db)
                                             (DatabaseEntity be db)
                                             (Rep (db (AnnotatedDatabaseEntity be db)))
                                             (Rep (db (AnnotatedDatabaseEntity be db)))
                                             (Rep (db (DatabaseEntity be db)))
                           , Generic (AnnotatedDatabaseSettings be db)
                           , GSchema be db (Rep (AnnotatedDatabaseSettings be db))
                           )
                        => AnnotatedDatabaseSettings be db 
                        -> Schema
fromAnnotatedDbSettings db = gSchema db (from db)

-- | A database 'Migration'.
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
    Right edits -> runNoReturn $ Pg.PgCommandSyntax Pg.PgCommandTypeDdl (mconcat $ map toSqlSyntax edits)

-- Pg.PgCommandSyntax Pg.PgCommandTypeDdl 

toSqlSyntax :: Edit -> Pg.PgSyntax
toSqlSyntax = \case
  TableAdded tblName tbl -> 
      ddlSyntax ("CREATE TABLE \"" <> tableName tblName 
                                   <> "\" (" 
                                   <> T.intercalate ", " (map renderTableColumn (M.toList (tableColumns tbl)) <>
                                                          filter (not . T.null) (map renderCreateTableConstraint (S.toList (tableConstraints tbl)))
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
      ddlSyntax query    = Pg.emit . TE.encodeUtf8 $ query <> ";\n"
      updateSyntax query = Pg.emit . TE.encodeUtf8 $ query <> ";\n"

      alterTable :: TableName -> Text
      alterTable (TableName tName) = "ALTER TABLE \"" <> tName <> "\" "

      renderTableColumn :: (ColumnName, Column) -> Text
      renderTableColumn (colName, col) = columnName colName <> " " <> renderDataType (columnType col)

      renderCreateTableConstraint :: TableConstraint -> Text
      renderCreateTableConstraint = \case
        Unique _ cols     -> "UNIQUE (" <> T.intercalate ", " (map columnName (S.toList cols)) <> ")"
        PrimaryKey _ cols -> "PRIMARY KEY (" <> T.intercalate ", " (map columnName (S.toList cols)) <> ")"
        ForeignKey fname (tableName -> tName) (S.toList -> colPair) onDelete onUpdate ->
            let (fkCols, referenced) = (map (columnName . fst) colPair, map (columnName . snd) colPair)
            in "CONSTRAINT \"" <> fname 
                               <> "\" FOREIGN KEY (" 
                               <> T.intercalate ", " fkCols
                               <> ") REFERENCES \"" <> tName 
                               <> "\"(" <> T.intercalate ", " referenced <> ")" 
                               <> renderAction "ON DELETE" onDelete 
                               <> renderAction "ON UPDATE" onUpdate
        IsForeignKeyOf _tName _cols -> mempty

      renderAlterTableConstraint :: TableConstraint -> Text
      renderAlterTableConstraint = \case
        Unique cName _ -> cName
        PrimaryKey cName _ -> cName
        _ -> error "renderAlterTableConstraint: TODO"

      renderAction actionPrefix = \case
        NoAction   -> mempty
        Cascade    -> " " <> actionPrefix <> " " <> "CASCADE "
        Restrict   -> " " <> actionPrefix <> " " <> "RESTRICT "
        SetNull    -> " " <> actionPrefix <> " " <> "SET NULL "
        SetDefault -> " " <> actionPrefix <> " " <> "SET DEFAULT "

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
        liftIO $ putStrLn (unlines $ map displaySyntax pgSyntax)
