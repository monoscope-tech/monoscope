module Main (main) where

import Relude
import Test.DocTest qualified as DocTest 


main :: IO ()
main = do
  -- FIXME: figure out why some of the doctests are not working correctly and fix them. Maybe upgrade the dependency first.
  DocTest.mainFromCabal "apitoolkit-server" =<< getArgs
