{-# LANGUAGE OverloadedStrings #-}

-- | Pure regression tests for identifier quoting, validation and deterministic
-- bounded name generation.
module Test.Database.Beam.AutoMigrate.Identifiers (identifierTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Beam.AutoMigrate (ColumnName (..), SequenceName (..), TableName (..), mkSequenceName)
import Database.Beam.AutoMigrate.Util (generatedIdentifier, isValidIdentifier, sqlEscaped, sqlSingleQuoted)
import Test.Tasty
import Test.Tasty.HUnit

identifierTests :: TestTree
identifierTests =
  testGroup
    "Identifiers"
    [ testGroup
        "rendering"
        [ testCase "sqlEscaped always double-quotes" $
            sqlEscaped "simple" @?= "\"simple\"",
          testCase "reserved words are double-quoted" $
            sqlEscaped "select" @?= "\"select\"",
          testCase "embedded double quotes are doubled" $
            sqlEscaped "a\"b" @?= "\"a\"\"b\"",
          testCase "apostrophes and backslashes are ordinary identifier characters" $ do
            sqlEscaped "a'b" @?= "\"a'b\""
            sqlEscaped "a\\b" @?= "\"a\\b\"",
          testCase "single quotes and backslashes are E-escaped" $
            sqlSingleQuoted "a'b\\c" @?= "E'a''b\\\\c'"
        ],
      testGroup
        "validation"
        [ testCase "63 UTF-8 bytes are valid" $
            isValidIdentifier (T.replicate 63 "a") @?= True,
          testCase "64 UTF-8 bytes are invalid" $
            isValidIdentifier (T.replicate 64 "a") @?= False,
          testCase "multibyte identifiers are limited by UTF-8 byte length" $ do
            isValidIdentifier (T.replicate 21 "\8364") @?= True
            isValidIdentifier (T.replicate 21 "\8364" <> "a") @?= False,
          testCase "empty identifiers are invalid" $
            isValidIdentifier "" @?= False,
          testCase "NUL-containing identifiers are invalid" $
            isValidIdentifier "a\NULb" @?= False
        ],
      testGroup
        "generated names"
        [ testCase "candidates within 63 bytes are preserved" $ do
            generatedIdentifier "short" ["a"] @?= "short"
            generatedIdentifier (T.replicate 63 "a") ["a"] @?= T.replicate 63 "a"
            generatedIdentifier (T.replicate 21 "\8364") ["a"] @?= T.replicate 21 "\8364",
          testCase "known FNV test vector" $
            generatedIdentifier (T.replicate 64 "a") ["sequence", "t", "c"]
              @?= T.replicate 46 "a" <> "_57290c296df2cea5",
          testCase "multibyte prefixes stop on a character boundary" $ do
            let result = generatedIdentifier (T.replicate 22 "\8364") ["a"]
            assertBool "result fits the identifier limit" (BS.length (TE.encodeUtf8 result) <= 63)
            assertEqual "15 complete euro characters plus digest" 62 (BS.length (TE.encodeUtf8 result)),
          testCase "companion sequences keep the historical spelling" $
            mkSequenceName (TableName "t") (ColumnName "c") @?= SequenceName "t___c___seq",
          testCase "overlong companion sequences fall back to the digest" $
            mkSequenceName (TableName (T.replicate 40 "t")) (ColumnName (T.replicate 40 "c"))
              @?= SequenceName (generatedIdentifier (T.replicate 40 "t" <> "___" <> T.replicate 40 "c" <> "___seq") ["sequence", T.replicate 40 "t", T.replicate 40 "c"])
        ]
    ]
