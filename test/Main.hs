module Main where

import Database.Beam.Migrate

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
  ]
