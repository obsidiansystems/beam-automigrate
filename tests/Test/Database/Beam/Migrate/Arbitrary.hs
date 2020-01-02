{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
