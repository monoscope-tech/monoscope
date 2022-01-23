-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Spec
import Relude

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
