{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Pure regression tests for the explicit column-default slot: exactly one
-- edit for a default-only change, matching reference enumeration, exact
-- simulation preconditions, dependency validation and generated serial
-- literal spelling.
module Test.Database.Beam.AutoMigrate.Defaults (defaultTests) where

import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Proxy (Proxy (..))
import Database.Beam.AutoMigrate
import qualified Database.Beam.Backend.SQL.AST as AST
import Database.Beam.Backend.SQL.Types (SqlSerial)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema hiding (Table)
import qualified Database.Beam.Schema as Beam
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

defaultTests :: TestTree
defaultTests =
  testGroup
    "Defaults"
    [ testCase "default-only replacement is exactly one edit" $ do
        let edits = editsOf (diff (schemaWith (Just (def "2"))) (schemaWith (Just (def "1"))))
        case edits of
          [Edit (EditAction_Automatic (ColumnDefaultChanged "t" "v" old new)) _] -> do
            old @?= Just (def "1")
            new @?= Just (def "2")
          other -> assertFailure ("expected one ColumnDefaultChanged, got " <> show other),
      testCase "reference implementation enumerates the same default change" $
        sort (map show (editsOf (diffReferenceImplementation (schemaWith (Just (def "2"))) (schemaWith (Just (def "1"))))))
          @?= sort (map show (editsOf (diff (schemaWith (Just (def "2"))) (schemaWith (Just (def "1")))))),
      testCase "default removal is exactly one edit and reaches the target" $ do
        let current = schemaWith (Just (def "1"))
            desired = schemaWith Nothing
            edits = diff desired current
        either (assertFailure . show) (\es -> length es @?= 1) edits
        either (assertFailure . show) (\es -> applyEdits es current @?= Right desired) edits,
      testCase "simulation checks the stored old default" $ do
        let edit = WithPriority (defMkEdit (ColumnDefaultChanged "t" "v" Nothing (Just (def "1"))), Priority 4)
        case applyEdits [edit] (schemaWith (Just (def "0"))) of
          Left other -> assertContains "ColumnDefaultMismatch" (show other)
          Right other -> assertFailure ("expected ColumnDefaultMismatch, got " <> show other),
      testCase "simulation refuses a default whose sequence dependency is absent" $ do
        let edit = WithPriority (defMkEdit (ColumnDefaultChanged "t" "v" Nothing (Just (seqDefault "missing"))), Priority 4)
        case applyEdits [edit] (schemaWith Nothing) of
          Left other -> assertContains "DefaultReferencesNonExistingSequence" (show other)
          Right other -> assertFailure ("expected typed sequence failure, got " <> show other),
      testCase "schema validation rejects dangling default dependencies" $ do
        case validateSchema (schemaWith (Just (seqDefault "missing"))) of
          Left other -> assertContains "DefaultReferencesNonExistingSequence" (show other)
          Right () -> assertFailure "expected dangling sequence rejection"
        case validateSchema (schemaWith (Just (enumDefault "missing"))) of
          Left other -> assertContains "DefaultReferencesNonExistingEnum" (show other)
          Right () -> assertFailure "expected dangling enum rejection",
      testCase "serial parent default is derived, FK child has none" $ do
        schema <- either (assertFailure . show) pure (fromAnnotatedDbSettings serialDbSettings (Proxy @'[]))
        let defaulted =
              [ de
                | tbl <- M.elems (schemaTables schema),
                  col <- M.elems (tableColumns tbl),
                  Just de <- [columnDefault col]
              ]
        case defaulted of
          [de] -> do
            assertBool "companion sequence default" ("nextval" `T.isInfixOf` defaultExpression de)
            S.size (defaultSequenceDependencies de) @?= 1
          other -> assertFailure ("expected one derived default, got " <> show other)
        M.size (schemaSequences schema) @?= 1,
      testCase "companion sequence uses bounded canonical names" $ do
        let (sq, de) = companion "parents" "p_id"
        seqName sq @?= "parents___p_id___seq"
        defaultExpression de @?= "nextval('parents___p_id___seq'::regclass)"
        defaultSequenceDependencies de @?= S.singleton sq,
      testCase "companion literal quotes apostrophes and double quotes" $ do
        let (sq, de) = companion "Weird'Table" "col\"x"
        seqName sq @?= "Weird'Table___col\"x___seq"
        defaultExpression de @?= "nextval('\"Weird''Table___col\"\"x___seq\"'::regclass)",
      testCase "companion literal uses an E-string when a backslash is present" $ do
        let (sq, de) = companion "Back\\Table" "c"
        seqName sq @?= "Back\\Table___c___seq"
        defaultExpression de @?= "nextval(E'\"Back\\\\Table___c___seq\"'::regclass)",
      testCase "long companion names are bounded and plain" $ do
        let (sq, de) = companion (TableName (T.replicate 80 "a")) "c"
            name = seqName sq
        T.length name @?= 63
        T.take 46 name @?= T.replicate 46 "a"
        assertBool "digest suffix" ("_" `T.isInfixOf` T.drop 46 name)
        defaultExpression de @?= "nextval('" <> name <> "'::regclass)"
        defaultSequenceDependencies de @?= S.singleton sq,
      testCase "a declared dependency is valid" $
        validateSchema
          ( (schemaWith (Just (seqDefault "s")))
              { schemaSequences = M.singleton (SequenceName "s") (Sequence "t" "v")
              }
          )
          @?= Right ()
    ]

companion :: TableName -> ColumnName -> (SequenceName, DefaultExpr)
companion tName cName =
  case hasCompanionSequence (Proxy @(SqlSerial Int)) tName cName of
    Just ((sq, _), de) -> (sq, de)
    Nothing -> error "expected a companion sequence"

editsOf :: Diff -> [Edit]
editsOf = \case
  Left e -> error (show e)
  Right edits -> map fst (map unPriority edits)

schemaWith :: Maybe DefaultExpr -> Schema
schemaWith mbDefault =
  Schema
    { schemaTables =
        M.singleton
          "t"
          ( Table
              mempty
              ( M.singleton "v" (Column (SqlStdType AST.DataTypeInteger) mempty mbDefault)
              )
          ),
      schemaEnumerations = mempty,
      schemaSequences = mempty
    }

--
-- Typed fixture for companion-sequence derivation.
--

data SerialParentT f = SerialParentRow
  { serialParentId :: Columnar f (SqlSerial Int)
  }
  deriving (Generic, Beamable)

instance Beam.Table SerialParentT where
  data PrimaryKey SerialParentT f = SerialParentKey (Columnar f (SqlSerial Int))
    deriving (Generic, Beamable)
  primaryKey = SerialParentKey . serialParentId

data SerialChildT f = SerialChildRow
  { serialChildId :: Columnar f Int,
    serialChildParent :: PrimaryKey SerialParentT f
  }
  deriving (Generic, Beamable)

instance Beam.Table SerialChildT where
  data PrimaryKey SerialChildT f = SerialChildKey (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = SerialChildKey . serialChildId

data SerialDbT f = SerialDb
  { serialDbParents :: f (TableEntity SerialParentT),
    serialDbChildren :: f (TableEntity SerialChildT)
  }
  deriving (Generic, Database be)

serialDbSettings :: AnnotatedDatabaseSettings Postgres SerialDbT
serialDbSettings = defaultAnnotatedDbSettings defaultDbSettings

assertContains :: String -> String -> Assertion
assertContains needle haystack =
  assertBool ("expected " <> needle <> " in " <> haystack) (needle `isInfixOf` haystack)

def :: Text -> DefaultExpr
def x = DefaultExpr x mempty mempty

seqDefault :: Text -> DefaultExpr
seqDefault s = DefaultExpr ("nextval('" <> s <> "'::regclass)") (S.singleton (SequenceName s)) mempty

enumDefault :: Text -> DefaultExpr
enumDefault e = DefaultExpr ("'x'") mempty (S.singleton (EnumerationName e))
