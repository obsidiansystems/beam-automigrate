{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
module Database.Beam.Migrate
  ( fromDbSettings
  , migrate
  , runMigration
  , printMigration
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
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding            as TE

import           GHC.Generics


import           Database.Beam                  ( MonadBeam )
import           Database.Beam.Schema           ( DatabaseSettings )

import           Database.Beam.Migrate.Generic as Exports
import           Database.Beam.Migrate.Types   as Exports
import           Database.Beam.Migrate.Diff    as Exports
import           Database.Beam.Migrate.Postgres ( getSchema )

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

-- | Interpret a single 'Edit'.
-- NOTE(adn) Until we figure out whether or not we want to use Beam's Query
-- builder, this can for now yield the raw SQL fragment, for simplicity, which
-- can later be interpreted via 'Database.Beam.Query.CustomSQL'.
-- For now this is /very/ naive, we don't want to write custom, raw SQL fragments.
evalEdit :: Edit -> Text
evalEdit = \case
  TableAdded tblName _tbl -> "CREATE TABLE \"" <> tableName tblName <> "\" ()"
  TableRemoved tblName    -> "DROP TABLE \"" <> tableName tblName <> "\""
  TableConstraintsAdded   _tblName _cstr -> "TABLE_CONTRAINTS_ADDED TODO"
  TableConstraintsRemoved _tblName _cstr -> "TABLE_CONTRAINTS_REMOVED TODO"
  ColumnAdded tblName colName _col ->
    "ALTER TABLE \"" <> tableName tblName <> "\" ADD COLUMN \"" <> columnName colName <> "\""
  ColumnRemoved tblName colName ->
    "ALTER TABLE \"" <> tableName tblName <> "\" DROP COLUMN \"" <> columnName colName <> "\""
  ColumnTypeChanged _colName _old _new ->
      "COLUMN TYPE CHANGE TODO"
  ColumnConstraintsAdded _colName _cstr -> 
      "COLUMN CONSTRAINTS ADDED TODO"
  ColumnConstraintsRemoved _colName _cstr ->
      "COLUMN CONSTRAINTS REMOVED TODO"


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
  migs <- createMigration m
  case migs of
    Left e -> liftIO $ throwIO e
    Right edits -> do
        let pgSyntax = map toSqlSyntax edits
        liftIO $ putStrLn (unlines $ map (displaySyntax . Pg.fromPgCommand) pgSyntax)
        mapM_ runNoReturn pgSyntax

toSqlSyntax :: Edit -> Pg.PgCommandSyntax
toSqlSyntax = \case
  TableAdded tblName _tbl -> 
      ddlSyntax ("CREATE TABLE \"" <> tableName tblName <> "\" ()")
  TableRemoved tblName    -> 
      ddlSyntax ("DROP TABLE \"" <> tableName tblName <> "\"")
  TableConstraintsAdded   _tblName _cstr -> 
      updateSyntax ("TABLE_CONTRAINTS_ADDED TODO")
  TableConstraintsRemoved _tblName _cstr -> 
      updateSyntax ("TABLE_CONTRAINTS_REMOVED TODO")
  ColumnAdded tblName colName _col ->
      updateSyntax ("ALTER TABLE \"" <> tableName tblName <> "\" ADD COLUMN \"" <> columnName colName <> "\"")
  ColumnRemoved tblName colName ->
      updateSyntax ("ALTER TABLE \"" <> tableName tblName <> "\" DROP COLUMN \"" <> columnName colName <> "\"")
  ColumnTypeChanged _colName _old _new ->
      updateSyntax ("COLUMN TYPE CHANGE TODO")
  ColumnConstraintsAdded _colName _cstr -> 
      updateSyntax ("COLUMN CONSTRAINTS ADDED TODO")
  ColumnConstraintsRemoved _colName _cstr ->
      updateSyntax ("COLUMN CONSTRAINTS REMOVED TODO")
  where
      ddlSyntax query    = Pg.PgCommandSyntax Pg.PgCommandTypeDdl (Pg.emit . TE.encodeUtf8 $ query <> ";")
      updateSyntax query = Pg.PgCommandSyntax Pg.PgCommandTypeDataUpdate (Pg.emit . TE.encodeUtf8 $ query <> ";")


-- | Creates the migration but doesn't execute it.
createMigration :: Monad m => Migration m -> m (Either DiffError [Edit])
createMigration m = do
    (a, s) <- runStateT (runExceptT m) mempty
    case a of
      Left e    -> pure (Left e)
      Right ()  -> pure (Right s)


printMigration :: MonadIO m => Migration m -> m ()
printMigration m = do
    (a, edits) <- runStateT (runExceptT m) mempty
    case a of
      Left e    -> liftIO $ throwIO e
      Right ()  -> do
        let pgSyntax = map toSqlSyntax edits
        liftIO $ putStrLn (unlines $ map (displaySyntax . Pg.fromPgCommand) pgSyntax)
