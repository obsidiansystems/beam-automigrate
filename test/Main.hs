module Main where

import Database.Beam.Migrate
import Database.Beam.Migrate.Validity

import qualified Data.List as L
import           Test.Tasty
import           Test.Tasty.QuickCheck         as QC
import           Test.QuickCheck

import Test.Database.Beam.Migrate.Arbitrary


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [diffProps]

diffProps :: TestTree
diffProps = testGroup "Diff algorithm properties"
  [ QC.testProperty "diff algoritm behaves the same as the reference implementation" $
      \(SimilarSchemas (hsSchema, dbSchema)) ->
          fmap (L.sort . map show) (diffReferenceImplementation hsSchema dbSchema) ===
          fmap (L.sort . map show) (diff hsSchema dbSchema)
  --, QC.testProperty "reverse applying the edits of the diff algorithm yield back the Haskell schema" $
  --    \(SimilarSchemas (hsSchema, dbSchema)) ->
  --        case diff hsSchema dbSchema of
  --          Left e -> error (show e)
  --          Right edits -> applyEdits edits dbSchema === Right hsSchema
  ]
