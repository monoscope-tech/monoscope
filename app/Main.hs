module Main (main) where

import Data.Effectful.CodeReview.Types (ReviewMode (..))
import Data.Effectful.ReviewEvaluation qualified as Evaluation
import Relude
import Start (startApp)


main :: IO ()
main =
  getArgs >>= \case
    [] -> startApp
    ["evaluate-impact", input, output] -> Evaluation.mainEvaluation (BoundedPatch :| [CheckoutHarness]) input output
    ["evaluate-impact", "bounded-patch", input, output] -> Evaluation.mainEvaluation (BoundedPatch :| []) input output
    ["evaluate-impact", "checkout-harness", input, output] -> Evaluation.mainEvaluation (CheckoutHarness :| []) input output
    ["evaluate-source", input, output] -> Evaluation.mainSourceEvaluation (BoundedPatch :| [CheckoutHarness]) input output
    ["evaluate-source", "bounded-patch", input, output] -> Evaluation.mainSourceEvaluation (BoundedPatch :| []) input output
    ["evaluate-source", "checkout-harness", input, output] -> Evaluation.mainSourceEvaluation (CheckoutHarness :| []) input output
    _ -> die "Usage: monoscope-server [{evaluate-impact|evaluate-source} [bounded-patch|checkout-harness] CASE.json RESULT.json]"
