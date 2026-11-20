{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Create isolated test databases and preserve callback failures during cleanup.
-- PostgreSQL integration tests use a disposable superuser admin connection.
-- Extension and row-security tests need privileges beyond CREATEDB.
-- Application migration APIs do not need these test privileges.
module Database.Beam.AutoMigrate.TestUtils
  ( -- * Legacy entry point (README/Gargoyle demonstration)
    ConnMethod (..),
    withConnection,

    -- * Isolated disposable databases
    SetupError (..),
    requireTestConnInfo,
    conninfoValue,
    quotedIdent,
    withIsolatedDatabase,
  )
where

import Control.Exception
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Pool (withResource)
import Data.Typeable
import qualified Data.UUID.V4 as UUID
import Data.Word (Word8)
import qualified Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.Types (Only (..), Query (..))
import Gargoyle.PostgreSQL.Connect (withDb)
import qualified System.Environment
import System.IO (hPutStrLn, stderr)
import Test.Tasty.Options

data ConnMethod = ConnMethod_Direct Pg.ConnectInfo | ConnMethod_Gargoyle String
  deriving (Typeable)

instance IsOption ConnMethod where
  defaultValue = ConnMethod_Direct Pg.defaultConnectInfo
  parseValue x = if null x then Nothing else Just $ ConnMethod_Gargoyle x
  optionName = "with-gargoyle"
  optionHelp = "Whether to use a gargoyle database. Supply the database name."

-- | Open a connection through gargoyle or directly to a running PostgreSQL
-- instance.
withConnection :: ConnMethod -> (Pg.Connection -> IO a) -> IO a
withConnection c f = case c of
  ConnMethod_Gargoyle db -> withDb db $ \pool -> withResource pool f
  ConnMethod_Direct connInfo -> bracket (Pg.connect connInfo) Pg.close f

--
-- Isolated databases
--

-- | A credential-free setup failure.
-- The module never echoes the supplied connection string or the native libpq
-- diagnostics.
-- Therefore a mistyped DSN cannot leak a password into CI logs.
newtype SetupError = SetupError String
  deriving (Show)

instance Exception SetupError

-- | Encode a Haskell 'String' as UTF-8 bytes.
-- 'BC.pack' truncates every character to one byte and can emit a NUL for a
-- trailing non-ASCII character.
-- A NUL silently drops every byte appended after it, including the @dbname@
-- override.
utf8 :: String -> ByteString
utf8 = BL.toStrict . toLazyByteString . stringUtf8

isAsciiSpace :: Word8 -> Bool
isAsciiSpace w = w == 32 || (w >= 9 && w <= 13)

-- | Read @BEAM_AUTOMIGRATE_TEST_CONNINFO@ as libpq keyword/value bytes.
-- The function rejects missing or whitespace-only input and URI forms before
-- any database exists.
-- libpq itself parses quoting, escapes, SSL, service, options and syntax errors
-- when the connection opens.
-- The function returns the original bytes unchanged, because trimming would
-- corrupt a final backslash-escaped space.
requireTestConnInfo :: IO ByteString
requireTestConnInfo = do
  raw <- maybe BS.empty utf8 <$> System.Environment.lookupEnv "BEAM_AUTOMIGRATE_TEST_CONNINFO"
  when (BS.elem 0 raw) $
    throwIO (SetupError "BEAM_AUTOMIGRATE_TEST_CONNINFO must not contain NUL bytes")
  let trimmed = BS.reverse (BS.dropWhile isAsciiSpace (BS.reverse (BS.dropWhile isAsciiSpace raw)))
  if BS.null trimmed
    then throwIO (SetupError "BEAM_AUTOMIGRATE_TEST_CONNINFO is not set or empty")
    else
      if "postgres://" `BS.isPrefixOf` trimmed || "postgresql://" `BS.isPrefixOf` trimmed
        then throwIO (SetupError "BEAM_AUTOMIGRATE_TEST_CONNINFO must be a libpq keyword/value conninfo, not a URI")
        else pure raw

-- | Encode one value in libpq keyword/value conninfo syntax.
-- The result uses the single-quoted form.
-- Backslashes and apostrophes are the only escapes.
conninfoValue :: ByteString -> ByteString
conninfoValue v = "'" <> BS.concatMap escape v <> "'"
  where
    escape w
      | w == backslash = "\\\\"
      | w == apostrophe = "\\'"
      | otherwise = BS.singleton w
    backslash = 92
    apostrophe = 39

-- | Open one connection.
-- Any synchronous libpq diagnostic becomes a credential-free 'SetupError',
-- because that diagnostic can quote part of a malformed conninfo, password
-- included.
-- Asynchronous cancellation is rethrown unchanged, never wrapped.
connectSanitized :: ByteString -> IO Pg.Connection
connectSanitized info =
  Pg.connectPostgreSQL info `catch` \(err :: SomeException) ->
    case fromException err :: Maybe SomeAsyncException of
      Just _ -> throwIO err
      Nothing ->
        throwIO
          ( SetupError
              ( "could not open a PostgreSQL connection ("
                  <> show (typeOf err)
                  <> "; native diagnostics omitted)"
              )
          )

-- | Connect to @dbname@ and keep every parameter of @conninfo@.
-- libpq uses the last occurrence of a repeated keyword.
-- Therefore the appended @dbname@ overrides only the database.
connectToDatabase :: ByteString -> ByteString -> IO Pg.Connection
connectToDatabase conninfo dbname = connectSanitized (conninfo <> " dbname=" <> conninfoValue dbname)

-- | Take the session-level advisory lock for one generated disposable database
-- name.
-- The creating session holds the lock until its backend exits.
-- A cancellation reconciler waits for the same lock.
-- It then sees the @pg_database@ row only after the CREATE transaction
-- finishes, either committed or aborted.
lockDisposableDatabase :: Pg.Connection -> ByteString -> IO ()
lockDisposableDatabase conn dbname =
  void
    ( Pg.query
        conn
        "SELECT 1 FROM (SELECT pg_advisory_lock(pg_catalog.hashtextextended(?::text, 0::bigint))) AS locked"
        (Only dbname)
        :: IO [Only Int]
    )

-- | Render a cleanup failure without credentials.
-- The result is our own setup message, the SQLSTATE code of a server error, or
-- only the exception type.
-- The native libpq diagnostic stays out, because it can quote parts of the
-- conninfo.
credentialFreeException :: SomeException -> String
credentialFreeException err =
  case fromException err of
    Just (SetupError msg) -> msg
    Nothing -> case fromException err :: Maybe Pg.SqlError of
      Just sqlErr -> "SqlError " <> show (Pg.sqlState sqlErr)
      Nothing -> show (typeOf err)

-- | Double-quote an identifier for SQL text and double every embedded quote.
-- Use this function to keep generated database names separate from SQL syntax.
quotedIdent :: ByteString -> ByteString
quotedIdent name = "\"" <> BS.concatMap (\w -> if w == 34 then "\"\"" else BS.singleton w) name <> "\""

freshDatabaseName :: IO ByteString
freshDatabaseName = do
  u <- UUID.nextRandom
  pure (BC.pack ("beam_automigrate_test_" <> filter (/= '-') (show u)))

-- | Run the callback against a fresh disposable database created from
-- @template0@.
-- The function drops only the name it generated and recorded, and never
-- enumerates or touches another database.
-- The callback connection closes before @DROP ... WITH (FORCE)@.
-- If the action and the cleanup both fail, the original exception wins, so
-- cancellation and typed failures survive.
-- A cleanup failure after a successful action is thrown.
withIsolatedDatabase :: ByteString -> (Pg.Connection -> IO a) -> IO a
withIsolatedDatabase adminConnInfo action
  | BS.elem 0 adminConnInfo =
      throwIO (SetupError "the test conninfo must not contain NUL bytes")
  | otherwise = mask $ \restore -> do
      dbname <- createDb
      outcome <- try (restore (bracket (connectToDatabase adminConnInfo dbname) Pg.close action))
      case outcome of
        Right a -> do
          dropOwnedDb dbname
          pure a
        Left (actionError :: SomeException) -> do
          cleanup <- try (dropOwnedDb dbname)
          case cleanup of
            Right () -> throwIO actionError
            Left (cleanupError :: SomeException) -> do
              -- Report the independent cleanup failure.
              -- Keep the action exception as the one that propagates, including
              -- async cancellation.
              hPutStrLn stderr ("beam-automigrate test cleanup failed: " <> credentialFreeException cleanupError)
              throwIO actionError
  where
    createDb :: IO ByteString
    createDb = do
      dbname <- freshDatabaseName
      -- The admin connection closes before the callback connection opens.
      -- An ordinary CREATE failure throws here and records no name to drop.
      -- The creating backend holds the session advisory lock until it exits,
      -- which fences the cancellation reconciler below.
      created <-
        ( try
            ( bracket (connectSanitized adminConnInfo) Pg.close $ \conn -> do
                lockDisposableDatabase conn dbname
                void (Pg.execute conn (Query ("CREATE DATABASE " <> quotedIdent dbname <> " TEMPLATE template0")) ())
            )
            :: IO (Either SomeException ())
        )
      case created of
        Right () -> pure dbname
        Left (err :: SomeException) ->
          case fromException err :: Maybe SomeAsyncException of
            Nothing -> throwIO err
            -- Cancellation can arrive after PostgreSQL commits CREATE DATABASE
            -- but before the client receives the result.
            -- The reconciler takes the same session advisory lock and waits for
            -- the creating backend to exit.
            -- After that, the pg_database row is either committed and visible
            -- or absent, so DROP IF EXISTS is race-free.
            Just _ -> do
              dropOwnedDb dbname `catch` \(cleanupError :: SomeException) ->
                hPutStrLn stderr ("beam-automigrate test reconcile failed: " <> credentialFreeException cleanupError)
              throwIO err

    -- | Drop exactly the database this call generated.
    -- The function first takes the advisory lock of the creating session.
    -- On the cancelled-CREATE path that acquisition waits for the creating
    -- backend to exit.
    -- Only then is its pg_database row either committed and visible or absent.
    -- The function never enumerates or drops another name.
    dropOwnedDb :: ByteString -> IO ()
    dropOwnedDb dbname =
      bracket (connectSanitized adminConnInfo) Pg.close $ \conn -> do
        lockDisposableDatabase conn dbname
        void (Pg.execute conn (Query ("DROP DATABASE IF EXISTS " <> quotedIdent dbname <> " WITH (FORCE)")) ())
