{-# LANGUAGE LambdaCase #-}

-- | Check that valid FK declarations compile and incompatible declarations fail.
-- Run @cabal exec -- runghc tests/compile/Main.hs@ from the repository root.
-- These checks require no database.
module Main (main) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_, unless)
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (proc, readCreateProcessWithExitCode, readProcess)

-- | Require every listed fixture so missing files cannot silently reduce test coverage.
passFixtures :: [FilePath]
passFixtures =
  [ "ManualDisambiguation.hs",
    "CompositeNullable.hs"
  ]

failFixtures :: [FilePath]
failFixtures =
  [ "ManualTypeMismatch.hs"
  ]

main :: IO ()
main = do
  let passDir = "tests/compile/pass"
      failDir = "tests/compile/fail"
      controlDir = "tests/compile/control"
  passResults <- forM passFixtures (checkPass passDir)
  failResults <- concat <$> forM failFixtures (checkFail failDir controlDir)
  let results = passResults <> failResults
      failures = [msg | (False, msg) <- results]
  unless (null failures) $ do
    forM_ failures (hPutStrLn stderr)
    exitFailure
  putStrLn ("compiler fixtures OK (" <> show (length results) <> " checks)")

checkPass :: FilePath -> FilePath -> IO (Bool, String)
checkPass dir name = do
  let path = dir </> name
  exists <- doesFileExist path
  if not exists
    then pure (False, "missing pass fixture: " <> path)
    else withTempDir $ \outDir -> do
      (code, _, err) <- ghcNoCode outDir path
      pure
        ( code == ExitSuccess,
          "pass fixture " <> path <> " failed to compile:\n" <> err
        )

checkFail :: FilePath -> FilePath -> FilePath -> IO [(Bool, String)]
checkFail failDir controlDir name = do
  let failPath = failDir </> name
      controlPath = controlDir </> name
  failExists <- doesFileExist failPath
  controlExists <- doesFileExist controlPath
  if not failExists
    then pure [(False, "missing fail fixture: " <> failPath)]
    else
      if not controlExists
        then pure [(False, "missing control fixture: " <> controlPath)]
        else do
          failedAsExpected <- withTempDir $ \outDir -> do
            (code, _, err) <- ghcNoCode outDir failPath
            pure
              ( code /= ExitSuccess,
                "fail fixture " <> failPath <> " unexpectedly compiled"
                  <> if null err then "" else "\n" <> err
              )
          controlResult <- withTempDir $ \outDir -> do
            (code, _, err) <- ghcNoCode outDir controlPath
            pure
              ( code == ExitSuccess,
                "control fixture " <> controlPath <> " failed to compile:\n" <> err
              )
          pure [failedAsExpected, controlResult]

-- | Check a fixture without generating code or reusing output from another compilation.
ghcNoCode :: FilePath -> FilePath -> IO (ExitCode, String, String)
ghcNoCode outDir path = do
  let args =
        [ "-fno-code",
          "-fforce-recomp",
          "-package",
          "beam-automigrate",
          "-package",
          "beam-core",
          "-package",
          "beam-postgres",
          "-package",
          "text",
          "-package",
          "containers",
          "-package",
          "vector",
          "-odir",
          outDir,
          "-hidir",
          outDir,
          path
        ]
  try (readCreateProcessWithExitCode (proc "ghc" args) "") >>= \case
    Left e -> pure (ExitFailure 125, "", "could not run ghc: " <> show (e :: SomeException))
    Right r -> pure r

-- | Remove temporary compiler output when the check ends, including after cancellation.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action =
  bracket
    (takeWhile (/= '\n') <$> readProcess "mktemp" ["-d"] "")
    removeDirectoryRecursive
    action
