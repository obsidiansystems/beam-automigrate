module Main where

import           Criterion.Main

import qualified Data.Map.Strict               as M
import           Control.DeepSeq

import           Test.QuickCheck                ( generate
                                                , resize
                                                )

import           Database.Beam.Migrate.Types
import           Database.Beam.Migrate.Diff
import           Database.Beam.Migrate.Schema.Gen
                                                ( genSchema )


newtype SpineStrict = SS { unSS :: Diff }

-- For us is enough to make the list of edits spine-strict.
instance NFData SpineStrict where
    rnf (SS (Left e))      = rnf e
    rnf (SS (Right edits)) = length edits `deepseq` ()


main :: IO ()
main = do
  putStrLn $ "Generating schema with 10_000 tables ..."
  hsSchema <- generate (resize 10000 genSchema)
  putStrLn $ "Generated schema with " ++  show (M.size . schemaTables $ hsSchema) ++ " tables."
  defaultMain [
    bgroup "diff" [ bench "10_000 tables"  $ nf (SS . diff hsSchema) noSchema
                  ]
    ]
