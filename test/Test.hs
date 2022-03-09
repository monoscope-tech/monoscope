module Main (main) where

import Relude (IO, Maybe (Just))
import Spec qualified
import Test.DocTest (mainFromCabal)
import Test.Hspec.Formatters (progress)
import Test.Hspec.Runner
  ( Config (configFormatter),
    defaultConfig,
    hspecWith,
  )

main :: IO ()
main = do
  mainFromCabal "apitoolkit-server" [] -- =<< getArgs
  hspecWith defaultConfig {configFormatter = Just progress} Spec.spec
