module Pkg.PatternMerge (
  embeddingTextForError,
  cosineSimilarity,
  assignToCentroids,
  autoMergeThreshold,
  ambiguousThreshold,
  buildJudgePrompt,
  parseJudgeResponse,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _String)
import Control.Lens ((^?), (^..))
import Data.List (maximumBy)
import Data.Text qualified as T
import Relude


-- | Construct embedding text for an error pattern.
--
-- >>> embeddingTextForError "TypeError" "Cannot read property 'x' of undefined"
-- "TypeError: Cannot read property 'x' of undefined"
--
-- >>> embeddingTextForError "" "some message"
-- "some message"
embeddingTextForError :: Text -> Text -> Text
embeddingTextForError errType msg
  | T.null errType = msg
  | otherwise = errType <> ": " <> msg


-- | Cosine similarity between two embedding vectors.
-- Returns 0.0 for zero-length or mismatched vectors.
--
-- >>> cosineSimilarity [1,0,0] [1,0,0]
-- 1.0
--
-- >>> cosineSimilarity [1,0] [0,1]
-- 0.0
--
-- >>> cosineSimilarity [3,4] [4,3]
-- 0.96
--
-- >>> cosineSimilarity [] []
-- 0.0
cosineSimilarity :: [Float] -> [Float] -> Float
cosineSimilarity xs ys
  | null xs || length xs /= length ys = 0.0
  | normA == 0 || normB == 0 = 0.0
  | otherwise = fromIntegral (round (dotP / (normA * normB) * 100 :: Float) :: Int) / 100
  where
    dotP = sum $ zipWith (*) xs ys
    normA = sqrt $ sum $ map (^ (2 :: Int)) xs
    normB = sqrt $ sum $ map (^ (2 :: Int)) ys


autoMergeThreshold :: Float
autoMergeThreshold = 0.95

ambiguousThreshold :: Float
ambiguousThreshold = 0.75


-- | Assign new patterns to existing centroids based on cosine similarity.
-- Returns (auto-merge assignments, ambiguous pairs needing LLM judge).
-- Patterns below ambiguousThreshold remain standalone (not returned).
--
-- >>> assignToCentroids [("c1", [1,0,0])] [("n1", [1,0,0])]
-- ([("n1","c1")],[])
--
-- >>> assignToCentroids [("c1", [1,0,0])] [("n1", [0,1,0])]
-- ([],[])
assignToCentroids :: [(a, [Float])] -> [(a, [Float])] -> ([(a, a)], [(a, a)])
assignToCentroids centroids newPatterns = foldl' classify ([], []) newPatterns
  where
    classify (merges, ambiguous) (newId, newEmb) =
      case bestMatch newEmb centroids of
        Just (centId, sim)
          | sim >= autoMergeThreshold -> ((newId, centId) : merges, ambiguous)
          | sim >= ambiguousThreshold -> (merges, (newId, centId) : ambiguous)
        _ -> (merges, ambiguous)
    bestMatch emb cs = case mapMaybe (\(cid, cemb) -> let s = cosineSimilarity emb cemb in bool Nothing (Just (cid, s)) (s >= ambiguousThreshold)) cs of
      [] -> Nothing
      matches -> Just $ maximumBy (comparing snd) matches


-- | Build an LLM judge prompt for ambiguous pattern pairs.
-- Takes pairs of (pattern text A, pattern text B).
buildJudgePrompt :: [(Text, Text)] -> Text
buildJudgePrompt pairs = systemPart <> "\n\n" <> pairsPart
  where
    systemPart = T.unlines
      [ "You are a pattern deduplication judge. For each pair of error/log patterns below,"
      , "decide if they represent the same underlying issue (MERGE) or distinct issues (KEEP_SEPARATE)."
      , ""
      , "Respond with a JSON array of objects, one per pair, with fields:"
      , "  - \"index\": the pair index (0-based)"
      , "  - \"decision\": \"MERGE\" or \"KEEP_SEPARATE\""
      , ""
      , "Example: [{\"index\": 0, \"decision\": \"MERGE\"}, {\"index\": 1, \"decision\": \"KEEP_SEPARATE\"}]"
      ]
    pairsPart = T.unlines $ zipWith formatPair [0 :: Int ..] pairs
    formatPair i (a, b) = "Pair " <> show i <> ":\n  A: " <> a <> "\n  B: " <> b


-- | Parse the LLM judge response into a list of (index, shouldMerge) pairs.
parseJudgeResponse :: Text -> [(Int, Bool)]
parseJudgeResponse txt = case AE.decodeStrict (encodeUtf8 cleaned) :: Maybe AE.Value of
  Just arr -> mapMaybe parseDecision (arr ^.. _Array . traverse)
  Nothing -> []
  where
    cleaned = fromMaybe txt $ (T.stripPrefix "```json" txt <|> T.stripPrefix "```" txt) >>= T.stripSuffix "```" <&> T.strip
    parseDecision v = do
      idx <- v ^? key "index" >>= asInt
      decision <- v ^? key "decision" . _String
      pure (idx, decision == "MERGE")
    asInt (AE.Number n) = Just (round n)
    asInt _ = Nothing
