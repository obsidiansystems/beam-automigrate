{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Database.Beam.Migrate.Arbitrary where

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Schema.Gen

import Test.QuickCheck

instance Arbitrary Schema where
    arbitrary = genSchema

newtype SimilarSchemas = 
    SimilarSchemas { unSchemas :: (Schema, Schema) } deriving Show

instance Arbitrary SimilarSchemas where
    arbitrary = SimilarSchemas <$> resize 5 genSimilarSchemas

