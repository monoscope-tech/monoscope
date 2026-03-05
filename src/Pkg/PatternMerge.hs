module Pkg.PatternMerge (
  embeddingTextForError,
  cosineSimilarity,
  assignToCentroids,
  autoMergeThreshold,
  ambiguousThreshold,
  buildJudgePrompt,
  parseJudgeResponse,
  jaccardSimilarity,
  isPlaceholderToken,
  jaccardMergeThreshold,
  mergeByJaccard,
)
where

import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _String)
import Data.List (maximumBy)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Pkg.Drain qualified as Drain
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
cosineSimilarity xs ys = cosineSimU (VU.fromList xs) (VU.fromList ys)


-- | Fast cosine similarity on unboxed vectors with pre-rounded output.
cosineSimU :: VU.Vector Float -> VU.Vector Float -> Float
cosineSimU xs ys
  | VU.null xs || VU.length xs /= VU.length ys = 0.0
  | normA == 0 || normB == 0 = 0.0
  | otherwise = fromIntegral (round (dotP / (normA * normB) * 100 :: Float) :: Int) / 100
  where
    dotP = VU.sum $ VU.zipWith (*) xs ys
    normA = sqrt $ VU.sum $ VU.map (^ (2 :: Int)) xs
    normB = sqrt $ VU.sum $ VU.map (^ (2 :: Int)) ys


-- | Cosine similarity using pre-computed norms for both vectors.
cosineSimWithNorms :: (VU.Vector Float, Float) -> (VU.Vector Float, Float) -> Float
cosineSimWithNorms (xs, normA) (ys, normB)
  | VU.null xs || VU.length xs /= VU.length ys = 0.0
  | normA == 0 || normB == 0 = 0.0
  | otherwise = fromIntegral (round (dotP / (normA * normB) * 100 :: Float) :: Int) / 100
  where
    dotP = VU.sum $ VU.zipWith (*) xs ys


vecNorm :: VU.Vector Float -> Float
vecNorm v = sqrt $ VU.sum $ VU.map (^ (2 :: Int)) v


autoMergeThreshold :: Float
autoMergeThreshold = 0.95


ambiguousThreshold :: Float
ambiguousThreshold = 0.75


-- | Assign new patterns to existing centroids based on cosine similarity.
-- Returns (auto-merge assignments, ambiguous pairs needing LLM judge).
-- Patterns below ambiguousThreshold remain standalone (not returned).
-- Pre-computes centroid norms and uses unboxed vectors for O(n*m) with low constant factor.
--
-- >>> assignToCentroids [("c1", [1,0,0])] [("n1", [1,0,0])]
-- ([("n1","c1")],[])
--
-- >>> assignToCentroids [("c1", [1,0,0])] [("n1", [0,1,0])]
-- ([],[])
assignToCentroids :: [(a, [Float])] -> [(a, [Float])] -> ([(a, a)], [(a, a)])
assignToCentroids centroids newPatterns = foldl' classify ([], []) newPatterns
  where
    centroidsU = map (\(cid, emb) -> let v = VU.fromList emb in (cid, v, vecNorm v)) centroids
    classify (merges, ambiguous) (newId, newEmb) =
      let v = VU.fromList newEmb
          newNormed = (v, vecNorm v)
       in case bestMatch newNormed centroidsU of
            Just (centId, sim)
              | sim >= autoMergeThreshold -> ((newId, centId) : merges, ambiguous)
              | sim >= ambiguousThreshold -> (merges, (newId, centId) : ambiguous)
            _ -> (merges, ambiguous)
    bestMatch newNormed cs = case mapMaybe (\(cid, cemb, cnorm) -> let s = cosineSimWithNorms newNormed (cemb, cnorm) in bool Nothing (Just (cid, s)) (s >= ambiguousThreshold)) cs of
      [] -> Nothing
      matches -> Just $ maximumBy (comparing snd) matches


-- | Build an LLM judge prompt for ambiguous pattern pairs.
-- Takes pairs of (pattern text A, pattern text B).
buildJudgePrompt :: [(Text, Text)] -> Text
buildJudgePrompt pairs = systemPart <> "\n\n" <> pairsPart
  where
    systemPart =
      T.unlines
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


-- | Token considered a placeholder (excluded from Jaccard comparison).
--
-- >>> isPlaceholderToken "<*>"
-- True
--
-- >>> isPlaceholderToken "{integer}"
-- True
--
-- >>> isPlaceholderToken "INFO"
-- False
isPlaceholderToken :: Text -> Bool
isPlaceholderToken t = t == "<*>" || (T.isPrefixOf "{" t && T.isSuffixOf "}" t)


-- | Extract non-placeholder content tokens as a Set (for reuse in hot loops).
contentTokens :: Text -> Set.Set Text
contentTokens = Set.fromList . filter (not . isPlaceholderToken) . words


-- | Jaccard similarity on pre-computed token sets.
jaccardOnSets :: Set.Set Text -> Set.Set Text -> Double
jaccardOnSets tokA tokB =
  let inter = Set.size $ Set.intersection tokA tokB
      union_ = Set.size $ Set.union tokA tokB
   in if union_ == 0 then 1.0 else fromIntegral inter / fromIntegral union_


-- | Jaccard similarity on non-placeholder token sets.
-- Returns 1.0 when both inputs have no content tokens.
--
-- >>> jaccardSimilarity "INFO user logged in" "INFO user logged in"
-- 1.0
--
-- >>> jaccardSimilarity "INFO <*> logged in" "INFO <*> <*> logged in"
-- 1.0
--
-- >>> jaccardSimilarity "INFO <*> <*> <*> done" "INFO <*> done"
-- 1.0
--
-- >>> jaccardSimilarity "<*> <*> <*>" "<*>"
-- 1.0
--
-- >>> jaccardSimilarity "INFO <*> <*> {uuid} done" "INFO <*> {integer} done"
-- 1.0
--
-- >>> jaccardSimilarity "INFO started" "ERROR crashed"
-- 0.0
jaccardSimilarity :: Text -> Text -> Double
jaccardSimilarity a b = jaccardOnSets (contentTokens a) (contentTokens b)


jaccardMergeThreshold :: Double
jaccardMergeThreshold = 0.90


-- | Merge Drain results by Jaccard similarity of non-placeholder tokens.
-- Keeps the first match's templateStr; combines logIds.
mergeByJaccard :: Double -> V.Vector Drain.DrainResult -> V.Vector Drain.DrainResult
mergeByJaccard threshold results = V.fromList $ map fst $ toList $ foldl' tryMerge Seq.empty tagged
  where
    tagged = V.toList results <&> \dr -> (dr, contentTokens dr.templateStr)
    tryMerge acc (x, xToks) =
      case Seq.findIndexL (\(_, aToks) -> jaccardOnSets aToks xToks >= threshold) acc of
        Just idx -> let (a, aToks) = Seq.index acc idx in Seq.update idx (a{Drain.logIds = a.logIds <> x.logIds}, aToks) acc
        Nothing -> acc Seq.|> (x, xToks)
