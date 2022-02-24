module Main where

import Relude
import Spec qualified
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)
import Test.Hspec.Formatters
import Test.Hspec.Runner

main :: IO ()
main = do
  mainFromCabal "apitoolkit-server" =<< getArgs
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
