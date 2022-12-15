{-# Language OverloadedStrings #-}
module Database.Beam.AutoMigrate.TestUtils where

import Control.Exception (bracket)
import Data.Pool (withResource)
import Data.Typeable
import Database.PostgreSQL.Simple as Pg
import Gargoyle.PostgreSQL.Connect (withDb)
import Test.Tasty.Options

data ConnMethod = ConnMethod_Direct Pg.ConnectInfo | ConnMethod_Gargoyle String
  deriving (Typeable)

instance IsOption ConnMethod where
  defaultValue = ConnMethod_Direct Pg.defaultConnectInfo
  parseValue x = if null x then Nothing else Just $ ConnMethod_Gargoyle x
  optionName = "with-gargoyle"
  optionHelp = "Whether to use a gargoyle database. Supply the database name."

-- | Connect to a database using gargoyle or connect directly to an already-running
-- postgresql instance
withConnection :: ConnMethod -> (Pg.Connection -> IO a) -> IO a
withConnection c f = case c of
  ConnMethod_Gargoyle db -> withDb db $ \pool -> withResource pool $ f
  ConnMethod_Direct connInfo -> bracket (Pg.connect connInfo) Pg.close f
