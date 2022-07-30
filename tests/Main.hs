module Main where

import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import Database.Beam.AutoMigrate
import Database.Beam.AutoMigrate.Schema.Gen
import Database.Beam.AutoMigrate.Validity
import Test.Database.Beam.AutoMigrate.Arbitrary hiding ((===))
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Text.Pretty.Simple (pShowNoColor)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties =
  testGroup
    "Diff algorithm properties"
    [ QC.testProperty "diff algoritm behaves the same as the reference implementation" $
        \(SimilarSchemas (hsSchema, dbSchema)) ->
          fmap (L.sort . map show) (diffReferenceImplementation hsSchema dbSchema)
            === fmap (L.sort . map show) (diff hsSchema dbSchema),
      QC.testProperty "reverse applying the edits of the diff algorithm yields back the Haskell schema" $
        \(Pretty (SimilarSchemas (hsSchema, dbSchema))) ->
          case diff hsSchema dbSchema of
            Left e -> error (show e)
            Right edits -> (sortEdits (fmap mkEdit edits), dbSchema) `sameSchema` hsSchema,
      QC.testProperty "reverse applying the edits of the diff algorithm yields back a valid schema" $
        \(Pretty (SimilarSchemas (hsSchema, dbSchema))) ->
          case diff hsSchema dbSchema of
            Left e -> error (show e)
            Right edits ->
              case applyEdits (fmap mkEdit edits) dbSchema of
                Left e' -> error (show e')
                Right s -> validateSchema s === Right ()
    ]

sameSchema :: ([WithPriority Edit], Schema) -> Schema -> Property
sameSchema (fullEdits, dbSchema) hsSchema =
  counterexample (pretty fullEdits ++ pretty schema' ++ interpret res ++ pretty hsSchema) res
  where
    pretty :: Show a => a -> String
    pretty = TL.unpack . pShowNoColor

    schema' :: Either ApplyFailed Schema
    schema' = applyEdits fullEdits dbSchema

    res = schema' == Right hsSchema
    interpret True = " == "
    interpret False = " /= "
