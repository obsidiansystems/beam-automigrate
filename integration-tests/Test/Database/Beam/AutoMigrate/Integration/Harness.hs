{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Live tests for the disposable-database harness itself. The cases use only
-- the connection bytes and libpq; they do not depend on the migration model, so
-- a harness failure is never mistaken for a migration failure.
module Test.Database.Beam.AutoMigrate.Integration.Harness (harnessTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.List (isInfixOf)
import Data.Unique (hashUnique, newUnique)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.PostgreSQL.Simple as Pg
import Database.Beam.AutoMigrate.TestUtils
import Database.PostgreSQL.Simple.Types (Only (..), Query (..))
import qualified System.Environment as Env
import System.Timeout (timeout)
import Test.Tasty
import Test.Tasty.HUnit

-- | Identify the callback failure so tests can detect a replacement cleanup error.
data Sentinel = Sentinel
  deriving (Show)

instance Exception Sentinel

executeSql :: Pg.Connection -> ByteString -> IO ()
executeSql conn sql = void (Pg.execute conn (Query sql) ())

currentDatabase :: Pg.Connection -> IO ByteString
currentDatabase conn = do
  [Only n] <- Pg.query_ conn "SELECT current_database()" :: IO [Only ByteString]
  pure n

databaseExists :: ByteString -> ByteString -> IO Bool
databaseExists dsn name =
  bracket (Pg.connectPostgreSQL dsn) Pg.close $ \conn -> do
    rows <- Pg.query conn "SELECT 1 FROM pg_database WHERE datname = ?" (Only name) :: IO [Only Int]
    pure (not (null rows))

-- | Generate a database name for a test fixture.
-- The caller must remove any database it creates with this name.
freshDatabaseName :: ByteString -> IO ByteString
freshDatabaseName prefix = do
  n <- hashUnique <$> newUnique
  pure (prefix <> BC.pack (show n))

-- | Name the admin database used to force a cleanup connection failure.
-- The callback removes this database before cleanup starts.
freshAdminDatabaseName :: IO ByteString
freshAdminDatabaseName = freshDatabaseName "beam_automigrate_admin_"

-- | Name an existing database that a failed setup call must leave unchanged.
freshExistingDatabaseName :: IO ByteString
freshExistingDatabaseName = freshDatabaseName "beam_automigrate_test_existing_"

createNamedDatabase :: ByteString -> ByteString -> IO ()
createNamedDatabase dsn name =
  bracket (Pg.connectPostgreSQL dsn) Pg.close $ \conn ->
    executeSql conn ("CREATE DATABASE " <> quotedIdent name)

dropNamedDatabase :: ByteString -> ByteString -> IO ()
dropNamedDatabase dsn name =
  bracket (Pg.connectPostgreSQL dsn) Pg.close $ \conn ->
    executeSql conn ("DROP DATABASE IF EXISTS " <> quotedIdent name <> " WITH (FORCE)")

-- | Run @action@ against a fresh isolated database, then assert that the
-- database is gone. The callback need not record its name; the helper reads it
-- after the callback succeeds.
withDroppedDatabase :: String -> ByteString -> (Pg.Connection -> IO ()) -> IO ()
withDroppedDatabase label dsn action = do
  ref <- newIORef Nothing
  withIsolatedDatabase dsn $ \conn -> do
    action conn
    currentDatabase conn >>= writeIORef ref . Just
  name <- readIORef ref >>= maybe (assertFailure "callback did not run") pure
  databaseExists dsn name >>= assertBool label . not

-- | Decode the admin connection string for environment-variable tests.
dsnString :: ByteString -> String
dsnString = T.unpack . TE.decodeUtf8

-- | Encode a Haskell 'String' as UTF-8 bytes.
utf8String :: String -> ByteString
utf8String = TE.encodeUtf8 . T.pack

-- | Check database isolation, connection-string handling, and cleanup.
harnessTests :: ByteString -> TestTree
harnessTests dsn =
  testGroup
    "Harness"
    [ testCase "a successful callback leaves no disposable database behind" $
        successfulCallbackCase dsn,
      testCase "environment DSN contract is credential-free and preserves bytes" $
        dsnContractCase dsn,
      testCase "quoted passwords and options survive the dbname override" $
        quotedOptionsCase dsn,
      testCase "two concurrent isolated callbacks with identical table names stay isolated" $
        concurrentIsolationCase dsn,
      testCase "a failing CREATE DATABASE leaves existing databases alone" $
        failedCreateKeepsExistingDatabasesCase dsn,
      testCase "an action exception survives a failing cleanup" $
        failingCleanupCase dsn (throwIO Sentinel) isSentinel "the sentinel exception",
      testCase "an asynchronous cancellation survives a failing cleanup" $
        failingCleanupCase dsn (throwIO ThreadKilled) isThreadKilled "the cancellation exception",
      testCase "an asynchronous callback cancellation still removes the owned database" $
        callbackCancellationCase dsn,
      testCase "an in-flight CREATE DATABASE cancellation leaves no owned database" $
        inFlightCreateCancellationCase dsn,
      testCase "a NUL byte in the conninfo is refused before any connection" $
        nulByteConninfoCase dsn,
      testCase "a malformed conninfo never echoes credential fragments" $
        malformedConninfoCase dsn
    ]

-- | Check that a successful callback uses a temporary database and removes it.
successfulCallbackCase :: ByteString -> IO ()
successfulCallbackCase dsn =
  withDroppedDatabase "isolated database must be dropped after a successful callback" dsn $ \conn -> do
    name <- currentDatabase conn
    assertBool ("unexpected isolated database name: " <> BC.unpack name) ("beam_automigrate_test_" `BC.isPrefixOf` name)
    executeSql conn "CREATE TABLE probe (id integer NOT NULL)"
    executeSql conn "INSERT INTO probe VALUES (1)"

-- | Check input rejection without exposing credentials in errors.
-- Also check that valid connection strings retain their bytes.
dsnContractCase :: ByteString -> IO ()
dsnContractCase dsn = do
  previous <- Env.lookupEnv dsnEnv
  let restore = maybe (void (Env.unsetEnv dsnEnv)) (Env.setEnv dsnEnv) previous
  (`finally` restore) $ do
    Env.unsetEnv dsnEnv
    expectSetupError "not set or empty" requireTestConnInfo
    Env.setEnv dsnEnv "   "
    expectSetupError "not set or empty" requireTestConnInfo
    Env.setEnv dsnEnv "postgres://user:secret@host/db"
    expectSetupError "not a URI" requireTestConnInfo
    Env.setEnv dsnEnv "postgresql://user:secret@host/db"
    expectSetupError "not a URI" requireTestConnInfo
    escapedSpaceContractCase dsn
    unicodeContractCase dsn

-- | Require @body@ to raise a 'SetupError' that mentions @expected@ and never
-- echoes a @password=@ fragment.
expectSetupError :: String -> IO a -> IO ()
expectSetupError expected body = do
  r <- try (void body) :: IO (Either SomeException ())
  case r of
    Left e
      | Just (SetupError msg) <- fromException e -> do
          assertBool ("setup error must mention the contract, got: " <> msg) (expected `isInfixOf` msg)
          assertBool "setup error must never echo the supplied DSN" (not ("password=" `isInfixOf` msg))
      | otherwise -> assertFailure ("expected SetupError, got " <> show e)
    Right _ -> assertFailure "expected a setup failure"

-- | A final backslash-escaped space belongs to the value. Trimming it would
-- leave a dangling backslash and change authentication, so the bytes must reach
-- libpq verbatim; the appended @dbname@ override must still select the isolated
-- database.
escapedSpaceContractCase :: ByteString -> IO ()
escapedSpaceContractCase dsn = do
  let escapedDsn = dsnString dsn <> " application_name=escape\\ "
  Env.setEnv dsnEnv escapedDsn
  verbatim <- requireTestConnInfo
  verbatim @?= utf8String escapedDsn
  -- libpq must see the escaped space as part of the application name, and the
  -- dbname override must win (truncation would connect to the admin database).
  withDroppedDatabase "escaped-space DSN database must be dropped" verbatim $ \conn -> do
    [Only appName] <- Pg.query_ conn "SELECT current_setting('application_name')" :: IO [Only String]
    appName @?= "escape "
    name <- currentDatabase conn
    assertBool ("escaped-space DSN lost the dbname override: " <> BC.unpack name) ("beam_automigrate_test_" `BC.isPrefixOf` name)

-- | Non-ASCII characters must travel as UTF-8. Truncating them to a NUL byte
-- would silently drop the appended @dbname@ override.
unicodeContractCase :: ByteString -> IO ()
unicodeContractCase dsn = do
  let unicodeDsn = dsnString dsn <> " options='-c beam.test_unicode=\256' application_name=\256"
  Env.setEnv dsnEnv unicodeDsn
  bytes <- requireTestConnInfo
  bytes @?= utf8String unicodeDsn
  withDroppedDatabase "Unicode-DSN database must be dropped" bytes $ \conn -> do
    -- application_name escapes non-ASCII bytes on the server; a custom setting
    -- preserves the value and checks libpq's UTF-8 transport.
    [Only unicodeValue] <- Pg.query_ conn "SELECT current_setting('beam.test_unicode')" :: IO [Only String]
    unicodeValue @?= "\256"
    name <- currentDatabase conn
    assertBool ("Unicode DSN lost the dbname override: " <> BC.unpack name) ("beam_automigrate_test_" `BC.isPrefixOf` name)

-- | Quoted libpq values (spaces, timeouts, options) must reach the isolated
-- session, and the @dbname@ override must replace only the database.
quotedOptionsCase :: ByteString -> IO ()
quotedOptionsCase dsn = do
  let dsnWithOptions =
        dsn
          <> " connect_timeout='5' application_name='weird name' options='-c statement_timeout=7000'"
  withDroppedDatabase "quoted-options database must be dropped" dsnWithOptions $ \conn -> do
    [Only appName] <- Pg.query_ conn "SELECT current_setting('application_name')" :: IO [Only String]
    [Only stmtTimeout] <- Pg.query_ conn "SELECT current_setting('statement_timeout')" :: IO [Only String]
    appName @?= "weird name"
    stmtTimeout @?= "7s"

-- | Two concurrent callbacks use separate databases even when both create the
-- same table name. Each must see only its own row, and both databases must be
-- dropped.
concurrentIsolationCase :: ByteString -> IO ()
concurrentIsolationCase dsn = do
  ready <- newMVar (0 :: Int)
  go <- newEmptyMVar
  -- Both workers wait here before reading, so a row can only come from the
  -- worker's own database, never from timing luck.
  let barrier = do
        n <- takeMVar ready
        putMVar ready (n + 1)
        if n + 1 == 2 then putMVar go () else readMVar go
      worker :: Int -> IO (Int, Either String ([Int], ByteString))
      worker v = do
        r <-
          try
            ( withIsolatedDatabase dsn $ \conn -> do
                name <- currentDatabase conn
                executeSql conn "CREATE TABLE same_name (id integer NOT NULL)"
                void (Pg.execute conn "INSERT INTO same_name VALUES (?)" (Only v))
                barrier
                rows <- Pg.query_ conn "SELECT id FROM same_name ORDER BY id" :: IO [Only Int]
                pure (map Pg.fromOnly rows, name)
            ) :: IO (Either SomeException ([Int], ByteString))
        pure (v, either (Left . show) Right r)
  first <- newEmptyMVar
  second <- newEmptyMVar
  _ <- forkIO (worker 1 >>= putMVar first)
  _ <- forkIO (worker 2 >>= putMVar second)
  collected <- sequence [takeMVar first, takeMVar second]
  map (\(v, r) -> (v, fmap fst r)) collected @?= [(1, Right [1]), (2, Right [2])]
  forM_ collected $ \(_, result) -> case result of
    Left e -> assertFailure e
    Right (_, name) -> databaseExists dsn name >>= assertBool "worker database must be dropped" . not

-- | A failing @CREATE DATABASE@ must leave every existing database alone. This
-- test creates a database in the suite's disposable namespace before the
-- failing call; that call owns no database yet, so it must not drop this name
-- or any unrelated database.
failedCreateKeepsExistingDatabasesCase :: ByteString -> IO ()
failedCreateKeepsExistingDatabasesCase dsn = do
  let failingDsn = dsn <> " options='-c default_transaction_read_only=on'"
  existing <- freshExistingDatabaseName
  createNamedDatabase dsn existing
  (`finally` dropNamedDatabase dsn existing) $ do
    r <- try (withIsolatedDatabase failingDsn (\_ -> pure ())) :: IO (Either SomeException ())
    case r of
      Left (_ :: SomeException) -> pure ()
      Right () -> assertFailure "CREATE DATABASE in a read-only session must fail"
    databaseExists dsn "postgres" >>= assertBool "an unrelated existing database was dropped"
    databaseExists dsn existing >>= assertBool "an existing disposable database was dropped"

-- | Whether @e@ is the cleanup-failure marker exception.
isSentinel :: SomeException -> Bool
isSentinel e = case fromException e of
  Just Sentinel -> True
  Nothing -> False

-- | Whether @e@ is the asynchronous 'ThreadKilled' exception.
isThreadKilled :: SomeException -> Bool
isThreadKilled e = case fromException e :: Maybe AsyncException of
  Just ThreadKilled -> True
  _ -> False

-- | Cancel a callback from inside itself, then require the cancellation to
-- escape while the owned database is still dropped.
callbackCancellationCase :: ByteString -> IO ()
callbackCancellationCase dsn = do
  ref <- newIORef Nothing
  r <-
    try
      ( withIsolatedDatabase dsn $ \conn -> do
          currentDatabase conn >>= writeIORef ref . Just
          throwIO ThreadKilled
      ) :: IO (Either SomeException ())
  case r of
    Left e
      | isThreadKilled e -> pure ()
      | otherwise -> assertFailure ("cancellation was replaced by: " <> show e)
    Right () -> assertFailure "expected cancellation"
  name <- readIORef ref >>= maybe (assertFailure "callback did not run") pure
  databaseExists dsn name >>= assertBool "cancelled callback database must be dropped" . not

-- | A NUL byte makes libpq truncate the conninfo, so it must be refused before
-- any connection is attempted.
nulByteConninfoCase :: ByteString -> IO ()
nulByteConninfoCase dsn = do
  r <- try (withIsolatedDatabase (dsn <> "\0") (\_ -> pure ())) :: IO (Either SomeException ())
  case r of
    Left e
      | Just (SetupError msg) <- fromException e -> assertBool "setup error must mention NUL" ("NUL" `isInfixOf` msg)
      | otherwise -> assertFailure ("expected SetupError, got " <> show e)
    Right () -> assertFailure "NUL-containing conninfo must fail"

-- | libpq's malformed-conninfo diagnostic can quote a password, so the refusal
-- must not echo any part of it.
malformedConninfoCase :: ByteString -> IO ()
malformedConninfoCase dsn = do
  let malformed = dsn <> " password=sekrit super-secret-fragment"
  r <- try (withIsolatedDatabase malformed (\_ -> pure ())) :: IO (Either SomeException ())
  case r of
    Left e
      | Just (SetupError msg) <- fromException e ->
          assertBool ("setup error echoed credential material: " <> msg) (not ("super-secret-fragment" `isInfixOf` msg))
      | otherwise -> assertFailure ("expected a credential-free SetupError, got " <> show e)
    Right () -> assertFailure "malformed conninfo must fail"

-- | Exercise the action-failure cleanup path: the callback removes the admin
-- database this call depends on, which makes the cleanup reconnect fail, then
-- raises @raise@. The raised exception, with its original type, must be the one
-- that escapes.
failingCleanupCase :: ByteString -> IO () -> (SomeException -> Bool) -> String -> IO ()
failingCleanupCase dsn raise isExpected label = do
  admin <- freshAdminDatabaseName
  createNamedDatabase dsn admin
  leaked <- newIORef Nothing
  let cleanup = do
        nested <- readIORef leaked
        forM_ nested $ \name -> dropNamedDatabase dsn name
        dropNamedDatabase dsn admin
  (`finally` cleanup) $ do
    let nestedDsn = dsn <> " dbname=" <> conninfoValue admin
    r <-
      try
        ( withIsolatedDatabase nestedDsn $ \conn -> do
            nested <- currentDatabase conn
            writeIORef leaked (Just nested)
            -- Destroy the admin database this isolation depends on, so the
            -- cleanup reconnect below cannot succeed.
            bracket (Pg.connectPostgreSQL dsn) Pg.close $ \adminConn ->
              executeSql adminConn ("DROP DATABASE " <> quotedIdent admin <> " WITH (FORCE)")
            raise
        ) :: IO (Either SomeException ())
    case r of
      Left e
        | isExpected e -> pure ()
        | otherwise -> assertFailure (label <> " was replaced by: " <> show e)
      Right () -> assertFailure ("expected " <> label)

-- | Cancel a 'withIsolatedDatabase' call while its @CREATE DATABASE@ is blocked,
-- then prove the generated database does not survive. An @EXCLUSIVE@ lock on
-- @pg_database@ blocks the CREATE's catalog insert but not @pg_stat_activity@'s
-- @ACCESS SHARE@ read, so the blocked backend is identified by its application
-- name and query text.
inFlightCreateCancellationCase :: ByteString -> IO ()
inFlightCreateCancellationCase dsn = do
  appName <- freshDatabaseName "cancel_create_"
  let workerDsn = dsn <> " application_name=" <> conninfoValue appName
  blocker <- Pg.connectPostgreSQL dsn
  let release = void (Pg.execute_ blocker "ROLLBACK") `catch` \(_ :: SomeException) -> pure ()
  (`finally` (release >> Pg.close blocker)) $ do
    executeSql blocker "BEGIN"
    executeSql blocker "LOCK TABLE pg_database IN EXCLUSIVE MODE"
    done <- newEmptyMVar
    worker <-
      forkIO $
        (try (withIsolatedDatabase workerDsn (\_ -> pure ())) :: IO (Either SomeException ())) >>= putMVar done
    pendingName <-
      timeout 10000000 (waitForBlockedCreate dsn appName)
        >>= maybe (assertFailure "the CREATE DATABASE never reached the pg_database lock wait") pure
    thrown <- newEmptyMVar
    _ <- forkIO (throwTo worker ThreadKilled `finally` putMVar thrown ())
    -- The driver's libpq call is a safe (interruptible) FFI call, so the
    -- cancellation must be delivered while the CREATE is still blocked. The
    -- lock is released only after that, so the reconcile path really runs
    -- against an in-flight CREATE: the cleanup waits on the creation session's
    -- advisory lock until the abandoned backend has finished.
    delivered <- timeout 2000000 (takeMVar thrown)
    case delivered of
      Just () -> pure ()
      Nothing -> assertFailure "the driver's CREATE DATABASE wait was not interruptible"
    release
    outcome <-
      timeout 30000000 (takeMVar done)
        >>= maybe (assertFailure "the cancelled CREATE worker never finished") pure
    case outcome of
      Left e
        | isThreadKilled e -> pure ()
        | otherwise -> assertFailure ("cancellation was replaced by: " <> show e)
      Right () -> assertFailure "an in-flight CREATE DATABASE cancellation must not succeed"
    -- The abandoned backend keeps the application name until it has finished.
    -- Only then is the generated name's fate settled, so an implementation that
    -- dropped before the CREATE's row was visible cannot pass by being checked
    -- too early.
    _ <-
      timeout 10000000 (waitForBackendsGone dsn appName)
        >>= maybe (assertFailure "the abandoned CREATE backend never exited") pure
    survived <- databaseExists dsn pendingName
    when survived (dropNamedDatabase dsn pendingName)
    assertBool
      ("in-flight CREATE database survived cancellation: " <> BC.unpack pendingName)
      (not survived)

-- | Poll @pg_stat_activity@ until the uniquely named worker's @CREATE DATABASE@
-- is waiting on a lock, and return the generated database name from its query.
waitForBlockedCreate :: ByteString -> ByteString -> IO ByteString
waitForBlockedCreate dsn appName =
  bracket (Pg.connectPostgreSQL dsn) Pg.close $ \conn -> go conn
  where
    go conn = do
      rows <-
        Pg.query
          conn
          "SELECT substring(query from 'CREATE DATABASE \"([^\"]+)\"') FROM pg_stat_activity WHERE application_name = ? AND query LIKE 'CREATE DATABASE %' AND wait_event_type = 'Lock'"
          (Only appName)
          :: IO [Only (Maybe String)]
      case [BC.pack n | Only (Just n) <- rows] of
        (n : _) -> pure n
        [] -> threadDelay 20000 >> go conn

-- | Wait until no backend still carries the uniquely named worker's application
-- name, that is, until the abandoned @CREATE DATABASE@ has fully finished.
waitForBackendsGone :: ByteString -> ByteString -> IO ()
waitForBackendsGone dsn appName =
  bracket (Pg.connectPostgreSQL dsn) Pg.close $ \conn -> go conn
  where
    go conn = do
      rows <-
        Pg.query
          conn
          "SELECT 1 FROM pg_stat_activity WHERE application_name = ?"
          (Only appName)
          :: IO [Only Int]
      case rows of
        [] -> pure ()
        _ -> threadDelay 20000 >> go conn

-- | Environment variable holding the admin DSN for the live suite.
dsnEnv :: String
dsnEnv = "BEAM_AUTOMIGRATE_TEST_CONNINFO"
