module Main (main) where

import Relude
import Spec qualified
import Test.DocTest (mainFromCabal)
import Test.Hspec.Formatters (progress)
import Test.Hspec.Runner (
  Config (configFormatter),
  defaultConfig,
  hspecWith,
 )


-- https://github.com/hspec/hspec-example
main :: IO ()
main = do
  -- FIXME: figure out why some of the doctests are not working correctly and fix them. Maybe upgrade the dependency first.
  -- mainFromCabal "apitoolkit-server" =<< getArgs
  hspecWith defaultConfig{configFormatter = Just progress} Spec.spec
