module Main (main) where

import Relude
import Test.Hspec
import Test.Hspec.Formatters (progress)
import Test.Hspec.Runner (
  Config (configFormatter),
  defaultConfig,
  hspecWith,
 )
import qualified Pkg.Parser.ExprSpec

-- | Runs unit tests that don't depend on the database
main :: IO ()
main = do
  putStrLn "Running unit tests..."
  hspecWith defaultConfig{configFormatter = Just progress} $ do
    -- Explicitly list unit test modules here
    describe "Unit Tests" $ do
      -- These imports must be manually maintained to include all unit tests
      Pkg.Parser.ExprSpec.spec