{-# LANGUAGE OverloadedStrings #-}

-- | Pure regression tests for physical type canonicalization: PostgreSQL alias
-- normalization, supported bounds and contextual schema validation.
module Test.Database.Beam.AutoMigrate.PhysicalTypes (physicalTypesTests) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Beam.AutoMigrate
import qualified Database.Beam.Backend.SQL.AST as AST
import Test.Tasty
import Test.Tasty.HUnit

physicalTypesTests :: TestTree
physicalTypesTests =
  testGroup
    "PhysicalTypes"
    [ testGroup
        "canonical forms"
        [ testCase "CHAR without width becomes CHAR(1)" $
            canonicalizeColumnType (SqlStdType (AST.DataTypeChar False Nothing Nothing))
              @?= Right (SqlStdType (AST.DataTypeChar False (Just 1) Nothing)),
          testCase "unbounded VARCHAR stays unbounded" $
            canonicalizeColumnType (SqlStdType (AST.DataTypeChar True Nothing Nothing))
              @?= Right (SqlStdType (AST.DataTypeChar True Nothing Nothing)),
          testCase "NATIONAL CHAR/VARCHAR become ordinary CHAR/VARCHAR" $ do
            canonicalizeColumnType (SqlStdType (AST.DataTypeNationalChar False (Just 4)))
              @?= Right (SqlStdType (AST.DataTypeChar False (Just 4) Nothing))
            canonicalizeColumnType (SqlStdType (AST.DataTypeNationalChar True (Just 4)))
              @?= Right (SqlStdType (AST.DataTypeChar True (Just 4) Nothing)),
          testCase "DECIMAL becomes NUMERIC" $
            canonicalizeColumnType (SqlStdType (AST.DataTypeDecimal (Just (4, Just 2))))
              @?= Right (SqlStdType (AST.DataTypeNumeric (Just (4, Just 2)))),
          testCase "numeric scale zero becomes absent" $
            canonicalizeColumnType (SqlStdType (AST.DataTypeNumeric (Just (6, Just 0))))
              @?= Right (SqlStdType (AST.DataTypeNumeric (Just (6, Nothing)))),
          testCase "numeric scale may exceed precision" $
            canonicalizeColumnType (SqlStdType (AST.DataTypeNumeric (Just (2, Just 6))))
              @?= Right (SqlStdType (AST.DataTypeNumeric (Just (2, Just 6)))),
          testCase "FLOAT aliases become REAL and DOUBLE PRECISION" $ do
            canonicalizeColumnType (SqlStdType (AST.DataTypeFloat (Just 24))) @?= Right (SqlStdType AST.DataTypeReal)
            canonicalizeColumnType (SqlStdType (AST.DataTypeFloat (Just 25))) @?= Right (SqlStdType AST.DataTypeDoublePrecision)
            canonicalizeColumnType (SqlStdType (AST.DataTypeFloat Nothing)) @?= Right (SqlStdType AST.DataTypeDoublePrecision),
          testCase "BIT without width becomes BIT(1)" $
            canonicalizeColumnType (SqlStdType (AST.DataTypeBit False Nothing))
              @?= Right (SqlStdType (AST.DataTypeBit False (Just 1))),
          testCase "text-backed enums become unbounded VARCHAR" $
            canonicalizeColumnType (DbEnumeration (EnumerationName "color") (Enumeration ["red", "green"]))
              @?= Right (SqlStdType (AST.DataTypeChar True Nothing Nothing)),
          testCase "temporal precision 0 and 6 are preserved" $ do
            canonicalizeColumnType (SqlStdType (AST.DataTypeTime (Just 0) False))
              @?= Right (SqlStdType (AST.DataTypeTime (Just 0) False))
            canonicalizeColumnType (SqlStdType (AST.DataTypeTimeStamp (Just 6) True))
              @?= Right (SqlStdType (AST.DataTypeTimeStamp (Just 6) True)),
          testCase "vectors keep bounded dimensions" $ do
            canonicalizeColumnType (PgSpecificType (PgVector Nothing)) @?= Right (PgSpecificType (PgVector Nothing))
            canonicalizeColumnType (PgSpecificType (PgVector (Just 16000))) @?= Right (PgSpecificType (PgVector (Just 16000))),
          testCase "arrays recurse into their element type" $
            canonicalizeColumnType (SqlArrayType (SqlStdType (AST.DataTypeChar False Nothing Nothing)) 2)
              @?= Right (SqlArrayType (SqlStdType (AST.DataTypeChar False (Just 1) Nothing)) 2),
          testCase "text-backed enum arrays become VARCHAR arrays" $
            canonicalizeColumnType (SqlArrayType (DbEnumeration (EnumerationName "color") (Enumeration ["red"])) 1)
              @?= Right (SqlArrayType (SqlStdType (AST.DataTypeChar True Nothing Nothing)) 1)
        ],
      testGroup
        "rejections"
        [ testCase "explicit character sets are refused" $
            assertRejected (SqlStdType (AST.DataTypeChar True (Just 4) (Just "UTF8"))),
          testCase "width bounds are enforced" $ do
            assertRejected (SqlStdType (AST.DataTypeChar True (Just 0) Nothing))
            assertRejected (SqlStdType (AST.DataTypeChar False (Just 10485761) Nothing))
            assertRejected (SqlStdType (AST.DataTypeBit True (Just 0)))
            assertRejected (SqlStdType (AST.DataTypeBit False (Just 83886081))),
          testCase "numeric precision and scale bounds are enforced" $ do
            assertRejected (SqlStdType (AST.DataTypeNumeric (Just (0, Nothing))))
            assertRejected (SqlStdType (AST.DataTypeNumeric (Just (1001, Nothing))))
            assertRejected (SqlStdType (AST.DataTypeNumeric (Just (10, Just 1001)))),
          testCase "FLOAT precision bounds are enforced" $ do
            assertRejected (SqlStdType (AST.DataTypeFloat (Just 0)))
            assertRejected (SqlStdType (AST.DataTypeFloat (Just 54))),
          testCase "temporal precision outside 0..6 is refused" $
            assertRejected (SqlStdType (AST.DataTypeTimeStamp (Just 7) False)),
          testCase "vector dimensions are bounded" $ do
            assertRejected (PgSpecificType (PgVector (Just 0)))
            assertRejected (PgSpecificType (PgVector (Just 16001))),
          testCase "array rank is bounded and nesting is refused" $ do
            assertRejected (SqlArrayType (SqlStdType AST.DataTypeInteger) 0)
            assertRejected (SqlArrayType (SqlStdType AST.DataTypeInteger) 7)
            assertRejected (SqlArrayType (SqlArrayType (SqlStdType AST.DataTypeInteger) 1) 1),
          testCase "unsupported AST constructors are refused" $ do
            assertRejected (SqlStdType (AST.DataTypeArray AST.DataTypeInteger 1))
            assertRejected (SqlStdType (AST.DataTypeInterval AST.ExtractFieldDateTimeYear))
            assertRejected (SqlStdType (AST.DataTypeRow []))
            assertRejected (SqlStdType (AST.DataTypeDomain "d")),
          testCase "invalid enum type identifiers are refused" $
            assertRejected (PgSpecificType (PgEnumeration (EnumerationName "")))
        ],
      testGroup
        "schema boundaries"
        [ testCase "aliases are normalized and constraints are preserved" $
            case canonicalizeSchema (schemaWith (SqlStdType (AST.DataTypeDecimal (Just (8, Just 3)))) (Just NotNull)) of
              Left errs -> assertFailure ("unexpected failures: " <> show errs)
              Right s -> do
                let col = tableColumns (schemaTables s M.! "t") M.! "c"
                columnType col @?= SqlStdType (AST.DataTypeNumeric (Just (8, Just 3)))
                columnConstraints col @?= S.singleton NotNull,
          testCase "default expressions are never rewritten" $ do
            let expr = "nextval('a___b___seq'::regclass)"
            case canonicalizeSchema (schemaWith (DbEnumeration (EnumerationName "e") (Enumeration ["x"])) (Just (Default expr))) of
              Left errs -> assertFailure ("unexpected failures: " <> show errs)
              Right s -> do
                let col = tableColumns (schemaTables s M.! "t") M.! "c"
                columnType col @?= SqlStdType (AST.DataTypeChar True Nothing Nothing)
                columnConstraints col @?= S.singleton (Default expr),
          testCase "an invalid physical type is reported against its column" $
            case canonicalizeSchema (schemaWith (SqlStdType (AST.DataTypeTime (Just 7) False)) Nothing) of
              Left [InvalidColumn (Qualified tName cName) (InvalidPhysicalColumnType qName original msg)] -> do
                tName @?= "t"
                cName @?= "c"
                qName @?= Qualified "t" "c"
                original @?= SqlStdType (AST.DataTypeTime (Just 7) False)
                assertBool "reason mentions temporal precision" ("temporal precision" `T.isInfixOf` msg)
              other -> assertFailure ("expected a contextual InvalidPhysicalColumnType, got: " <> show other)
        ]
    ]

schemaWith :: ColumnType -> Maybe ColumnConstraint -> Schema
schemaWith ty mbConstraint =
  noSchema
    { schemaTables =
        M.singleton
          "t"
          ( Table
              S.empty
              (M.singleton "c" (Column ty (maybe S.empty S.singleton mbConstraint)))
          )
    }

assertRejected :: ColumnType -> Assertion
assertRejected ty =
  assertBool ("expected rejection of " <> show ty) (isLeft (canonicalizeColumnType ty))

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
