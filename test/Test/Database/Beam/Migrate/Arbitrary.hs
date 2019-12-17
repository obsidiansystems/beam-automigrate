{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Database.Beam.Migrate.Arbitrary where

import GHC.Generics

import Database.Beam.Migrate.Types
import Database.Beam.Migrate.Schema.Gen

import Text.Pretty.Simple (pShowNoColor)
import qualified Data.Text.Lazy as TL

import Test.QuickCheck

newtype Pretty a = Pretty { unPretty :: a } deriving (Eq, Arbitrary)

instance Show a => Show (Pretty a) where
  show = TL.unpack . pShowNoColor . unPretty

instance Arbitrary Schema where
    arbitrary = genSchema
    shrink = shrinkSchema

newtype SimilarSchemas =
    SimilarSchemas { unSchemas :: (Schema, Schema) } deriving (Generic, Show)

instance Arbitrary SimilarSchemas where
    arbitrary = SimilarSchemas <$> resize 5 genSimilarSchemas
    shrink    = genericShrink

