{-# LANGUAGE OverloadedStrings #-}

-- | Check that SQL rendering returns typed errors for invalid inputs.
-- Check quoted identifiers, type precision, edit structures, and SQL without safety markers.
module Test.Database.Beam.AutoMigrate.Renderer (rendererTests) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam.AutoMigrate
import qualified Database.Beam.Backend.SQL.AST as AST
import Test.Tasty
import Test.Tasty.HUnit

rendererTests :: TestTree
rendererTests =
  testGroup
    "Renderer"
    [ testGroup
        "types"
        [ testCase "quoted enum type identifiers" $
            renderType (PgSpecificType (PgEnumeration (EnumerationName "my type")))
              @?= Right "\"my type\"",
          testCase "quoted extension type identifiers" $ do
            renderType (PgSpecificType PgLTree) @?= Right "\"ltree\""
            renderType (PgSpecificType (PgVector Nothing)) @?= Right "\"vector\""
            renderType (PgSpecificType (PgVector (Just 3))) @?= Right "\"vector\"(3)",
          testCase "temporal precision precedes time zone" $
            renderType (SqlStdType (AST.DataTypeTimeStamp (Just 3) True))
              @?= Right "TIMESTAMP(3) WITH TIME ZONE",
          testCase "supported arrays keep their rank" $
            renderType (SqlArrayType (SqlStdType AST.DataTypeInteger) 2)
              @?= Right "INT[][]",
          testCase "zero-rank and nested arrays are refused" $ do
            assertLeftType (SqlArrayType (SqlStdType AST.DataTypeInteger) 0)
            assertLeftType (SqlArrayType (SqlArrayType (SqlStdType AST.DataTypeInteger) 1) 1),
          testCase "temporal precision outside 0..6 is refused" $ do
            assertLeftType (SqlStdType (AST.DataTypeTime (Just 7) False))
            renderType (SqlStdType (AST.DataTypeTime (Just 0) False)) @?= Right "TIME(0) WITHOUT TIME ZONE",
          testCase "width and precision bounds are enforced" $ do
            assertLeftType (SqlStdType (AST.DataTypeChar False (Just 0) Nothing))
            assertLeftType (SqlStdType (AST.DataTypeChar True Nothing (Just "UTF8")))
            assertLeftType (SqlStdType (AST.DataTypeBit False (Just 0)))
            assertLeftType (SqlStdType (AST.DataTypeFloat (Just 0)))
            assertLeftType (SqlStdType (AST.DataTypeFloat (Just 54)))
            assertLeftType (SqlStdType (AST.DataTypeNumeric (Just (0, Nothing))))
            assertLeftType (SqlStdType (AST.DataTypeNumeric (Just (1001, Nothing))))
            assertLeftType (SqlStdType (AST.DataTypeNumeric (Just (10, Just 1001)))),
          testCase "unsupported AST constructors are refused" $ do
            assertLeftType (SqlStdType (AST.DataTypeInterval AST.ExtractFieldDateTimeYear))
            assertLeftType (SqlStdType (AST.DataTypeRow []))
            assertLeftType (SqlStdType (AST.DataTypeDomain "d"))
            assertLeftType (SqlStdType (AST.DataTypeArray AST.DataTypeInteger 1)),
          testCase "vector dimensions are bounded" $ do
            assertLeftType (PgSpecificType (PgVector (Just 0)))
            assertLeftType (PgSpecificType (PgVector (Just 16001)))
        ],
      testGroup
        "edits"
        [ testCase "invalid identifiers are refused" $
            assertLeftIdentifier (defMkEdit (ColumnRemoved (TableName "bad\NULname") "c")),
          testCase "overlong identifiers are refused" $
            assertLeftIdentifier (defMkEdit (TableRemoved (TableName (T.replicate 64 "a")))),
          testCase "empty keys and foreign keys are refused" $ do
            assertLeftEdit (defMkEdit (TableConstraintAdded "t" (PrimaryKey "pk" S.empty)))
            assertLeftEdit (defMkEdit (TableConstraintAdded "t" (ForeignKey "fk" "t" S.empty NoAction NoAction))),
          testCase "duplicate foreign key columns are refused" $ do
            assertLeftEdit
              ( defMkEdit
                  ( TableConstraintAdded
                      "t"
                      (ForeignKey "fk" "target" (S.fromList [("a", "x"), ("a", "y")]) NoAction NoAction)
                  )
              )
            assertLeftEdit
              ( defMkEdit
                  ( TableConstraintAdded
                      "t"
                      (ForeignKey "fk" "target" (S.fromList [("a", "x"), ("b", "x")]) NoAction NoAction)
                  )
              ),
          testCase "invalid enum labels are refused, empty labels allowed" $ do
            assertLeftIdentifier (defMkEdit (EnumTypeAdded (EnumerationName "e") (Enumeration ["ok", T.replicate 64 "a"])))
            assertLeftEdit (defMkEdit (EnumTypeAdded (EnumerationName "e") (Enumeration ["ok", "ok"])))
            sql <- renderEdit (defMkEdit (EnumTypeAdded (EnumerationName "e") (Enumeration ["", "ok"])))
            assertBool "empty enum label is rendered" ("E''" `T.isInfixOf` sql),
          testCase "invalid anchors are refused" $
            assertLeftIdentifier
              (defMkEdit (EnumTypeValueAdded (EnumerationName "e") "new" Before (T.replicate 64 "a"))),
          testCase "unsafe edits carry no safety markers" $ do
            sql <- renderEdit (defMkEdit (TableRemoved "t"))
            assertBool "no unsafe marker" (not ("<UNSAFE>" `T.isInfixOf` sql))
            assertEqual "plain drop" "DROP TABLE \"t\";\n" sql,
          testCase "table additions emit columns once and no constraints" $ do
            let tbl =
                  Table
                    S.empty
                    ( M.singleton
                        "id"
                        (Column (SqlStdType AST.DataTypeInteger) (S.singleton NotNull))
                    )
            sql <- renderEdit (defMkEdit (TableAdded "t" tbl))
            assertEqual "table SQL" "CREATE TABLE \"t\" (\"id\" INT NOT NULL);\n" sql,
          testCase "defaults are passed through as trusted SQL" $ do
            sql <- renderEdit (defMkEdit (ColumnConstraintAdded "t" "c" (Default "nextval('s'::regclass)")))
            assertBool "default is not quoted or cast" ("DEFAULT nextval('s'::regclass)" `T.isInfixOf` sql),
          testCase "quoted identifiers round-trip through the renderer" $ do
            sql <- renderEdit (defMkEdit (TableRemoved "select"))
            assertEqual "reserved word is quoted" "DROP TABLE \"select\";\n" sql
        ]
    ]

renderType :: ColumnType -> Either RenderError Text
renderType = renderDataType

renderEdit :: Edit -> IO Text
renderEdit e = case prettyEditSQL e of
  Left err -> assertFailure ("unexpected render error: " <> show err) >> pure ""
  Right sql -> pure sql

assertLeftType :: ColumnType -> Assertion
assertLeftType ty = case renderDataType ty of
  Left InvalidSqlType {} -> pure ()
  other -> assertFailure ("expected InvalidSqlType, got: " <> show other)

assertLeftIdentifier :: Edit -> Assertion
assertLeftIdentifier e = case prettyEditSQL e of
  Left InvalidSqlIdentifier {} -> pure ()
  other -> assertFailure ("expected InvalidSqlIdentifier, got: " <> show other)

assertLeftEdit :: Edit -> Assertion
assertLeftEdit e = case prettyEditSQL e of
  Left InvalidSqlEdit {} -> pure ()
  other -> assertFailure ("expected InvalidSqlEdit, got: " <> show other)
