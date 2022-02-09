module Main where

import Relude
import System.Environment (getArgs)
import Test.DocTest (mainFromCabal)

main :: IO ()
main = mainFromCabal "your-project" =<< getArgs
