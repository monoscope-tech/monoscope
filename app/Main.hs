module Main (main) where

import Data.Effectful.ReviewEvaluation qualified as Evaluation
import Relude
import Start (startApp)


main :: IO ()
main =
  getArgs >>= \case
    [] -> startApp
    ["evaluate-impact", input, output] -> Evaluation.mainEvaluation input output
    _ -> die "Usage: monoscope-server [evaluate-impact CASE.json RESULT.json]"
