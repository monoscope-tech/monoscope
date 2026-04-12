module Pkg.PatternMerge (
  embeddingTextForError,
  assignToCentroids,
  autoMergeThreshold,
  ambiguousThreshold,
  buildLogClusterJudgePrompt,
  buildEndpointJudgePrompt,
  buildErrorJudgePrompt,
  parseJudgeResponse,
  isPlaceholderToken,
  jaccardMergeThreshold,
  mergeByJaccard,
  logCanMerge,
  errorCanMerge,
  verifyMergeDecision,
  normalizeForEmbedding,
  normalizeErrorForEmbedding,
)
where

import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _String)
import Data.List (maximumBy)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Pkg.AI qualified as AI
import Pkg.Drain qualified as Drain
import Pkg.ErrorFingerprint qualified as EF
import Relude
import Text.Regex.TDFA ((=~))


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
assignToCentroids centroids = foldl' classify ([], [])
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


-- | Parse the LLM judge response. Returns (index, shouldMerge, maybeCanonicalPath).
-- The canonical field is only present in endpoint-specific responses.
parseJudgeResponse :: Text -> [(Int, Bool, Maybe Text)]
parseJudgeResponse txt = case AE.decodeStrict (encodeUtf8 $ AI.stripCodeBlock txt) :: Maybe AE.Value of
  Just arr -> mapMaybe parseDecision (arr ^.. _Array . traverse)
  Nothing -> []
  where
    parseDecision v = do
      idx <- v ^? key "index" >>= \case AE.Number n -> Just (round n); _ -> Nothing
      decision <- v ^? key "decision" . _String
      let canonical = v ^? key "canonical" . _String
      pure (idx, decision == "MERGE", bool Nothing canonical (decision == "MERGE"))


-- | Build an LLM judge prompt for ambiguous endpoint URL pairs.
-- Deduplicates all paths into a numbered list so the LLM can see the full picture
-- and cluster endpoints that share centroids, rather than evaluating pairs in isolation.
buildEndpointJudgePrompt :: [(Text, Text)] -> Text
buildEndpointJudgePrompt pairs = systemPart <> "\n\n" <> pathsPart <> "\n" <> pairsPart
  where
    -- Deduplicate all paths and assign indices
    allPaths = ordNub $ concatMap (\(a, b) -> [a, b]) pairs
    pathIndex = Map.fromList $ zip allPaths [0 :: Int ..]
    systemPart =
      unlines
        [ "You are an API route deduplication judge for an API monitoring tool."
        , "Below is a numbered list of HTTP endpoint URL paths, followed by candidate pairs."
        , "Your job is to identify URL paths that are the same route but differ only in dynamic"
        , "segments — user IDs, resource slugs, numeric IDs, UUIDs, auth provider prefixes, etc."
        , "These variable segments should be collapsed into a single canonical template."
        , "Multiple pairs may share the same centroid — consider them together as a cluster."
        , ""
        , "MERGE when paths differ only in segments that look like identifiers or variable parameters:"
        , "  /api/v1/users/get-all vs /api/v1/users/list-all → MERGE (synonymous list operations)"
        , "  /api/v1/orders/12345 vs /api/v1/orders/67890 → MERGE (numeric IDs)"
        , "  /api/v1/users/auth0|abc vs /api/v1/users/google|xyz → MERGE (auth provider IDs)"
        , "KEEP_SEPARATE when paths serve genuinely different functions:"
        , "  /api/v1/users vs /api/v1/orders → KEEP_SEPARATE (different resources)"
        , "  /api/v1/users/profile vs /api/v1/users/settings → KEEP_SEPARATE (different sub-resources)"
        , ""
        , "Respond with a JSON array of objects, one per pair, with fields:"
        , "  - \"index\": the pair index (0-based)"
        , "  - \"decision\": \"MERGE\" or \"KEEP_SEPARATE\""
        , "  - \"canonical\": (only when MERGE) the canonical template path using {param} for variable segments"
        , ""
        , "Example: [{\"index\": 0, \"decision\": \"MERGE\", \"canonical\": \"/api/v1/users/{param}\"}, {\"index\": 1, \"decision\": \"KEEP_SEPARATE\"}]"
        ]
    pathsPart = unlines $ "Endpoints:" : zipWith (\i p -> "  [" <> show i <> "] " <> p) [0 :: Int ..] allPaths
    pairsPart = unlines $ "Pairs to evaluate:" : zipWith formatPair [0 :: Int ..] pairs
    formatPair i (a, b) =
      let aIdx = fromMaybe 0 $ Map.lookup a pathIndex
          bIdx = fromMaybe 0 $ Map.lookup b pathIndex
       in "  Pair " <> show i <> ": [" <> show aIdx <> "] vs [" <> show bIdx <> "]"


-- | Token considered a placeholder (excluded from Jaccard comparison).
-- Matches any `{…}` token broadly — suitable for Jaccard where any braced token is a placeholder.
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


-- | Normalize a log pattern for embedding by replacing known Drain placeholders
-- with a uniform `<*>` token. This ensures patterns that differ only in
-- placeholder type (e.g. `{uuid}` vs `<*>`) produce identical embedding text.
-- JSON blobs and URL path segments in braces are preserved.
--
-- >>> normalizeForEmbedding "user {uuid} logged in from {ipv4}"
-- "user <*> logged in from <*>"
--
-- >>> normalizeForEmbedding "user <*> logged in from <*>"
-- "user <*> logged in from <*>"
--
-- >>> normalizeForEmbedding "balance {integer} from {hex} to {hex}"
-- "balance <*> from <*> to <*>"
--
-- >>> normalizeForEmbedding "no placeholders here"
-- "no placeholders here"
--
-- >>> normalizeForEmbedding "{\"context\":\"NestApplication\"}"
-- "{\"context\":\"NestApplication\"}"
--
-- >>> normalizeForEmbedding "error {/payments} not found"
-- "error {/payments} not found"
normalizeForEmbedding :: Text -> Text
normalizeForEmbedding = unwords . map Drain.normalizePlaceholder . words


-- | Extract non-placeholder content tokens as a Set (for reuse in hot loops).
contentTokens :: Text -> S.Set Text
contentTokens = S.fromList . filter (not . isPlaceholderToken) . words


-- | Jaccard similarity on pre-computed token sets.
jaccardOnSets :: S.Set Text -> S.Set Text -> Double
jaccardOnSets tokA tokB =
  let inter = S.size $ S.intersection tokA tokB
      union_ = S.size $ S.union tokA tokB
   in if union_ == 0 then 1.0 else fromIntegral inter / fromIntegral union_


jaccardMergeThreshold :: Double
jaccardMergeThreshold = 0.90


-- | Merge Drain results by Jaccard similarity of non-placeholder tokens.
-- Keeps the first match's templateStr; combines frequencies.
-- Size-pruned: skips pairs where min(|A|,|B|)/max(|A|,|B|) < threshold
-- (necessary condition for Jaccard ≥ threshold), avoiding most comparisons.
mergeByJaccard :: Double -> V.Vector Drain.DrainResult -> V.Vector Drain.DrainResult
mergeByJaccard threshold results = V.fromList $ map (\(dr, _, _) -> dr) $ toList $ foldl' tryMerge Seq.empty tagged
  where
    tagged = V.toList results <&> \dr -> let toks = contentTokens dr.templateStr in (dr, toks, S.size toks)
    tryMerge acc (x, xToks, xLen) =
      case Seq.findIndexL (\(_, aToks, aLen) ->
              let lo = min aLen xLen; hi = max aLen xLen
              in lo > 0 && fromIntegral lo / fromIntegral hi >= threshold
                 && jaccardOnSets aToks xToks >= threshold) acc of
        Just idx -> let (a, aToks, aLen) = Seq.index acc idx in Seq.update idx (a{Drain.frequency = a.frequency + x.frequency}, aToks, aLen) acc
        Nothing -> acc Seq.|> (x, xToks, xLen)


-- | Cluster-aware LLM judge prompt inspired by LogBatcher (batch-as-demonstration)
-- and Lemur (Chain-of-Thought reasoning). Shows all templates in context with sample logs,
-- then asks for structured Structure→Semantics→Decision reasoning per pair.
buildLogClusterJudgePrompt :: [(Text, Text)] -> Text
buildLogClusterJudgePrompt pairs = systemPart <> "\n\n" <> templatesPart <> "\n" <> pairsPart
  where
    allTemplates = ordNub $ concatMap (\(a, b) -> [a, b]) pairs
    templateIndex = Map.fromList $ zip allTemplates [0 :: Int ..]
    systemPart =
      unlines
        [ "You are a log pattern deduplication judge. Below is a numbered list of log pattern"
        , "templates, followed by candidate pairs to evaluate."
        , ""
        , "Placeholders like <*>, {uuid}, {integer}, {ipv4}, {hex} represent variable parameters."
        , "Differences in placeholder TYPE alone (e.g. <*> vs {uuid}) do NOT warrant separation —"
        , "they are all parameter slots. Focus on the fixed tokens that define the pattern's meaning."
        , ""
        , "For each pair, reason through these steps:"
        , "1. STRUCTURE: What fixed tokens differ between the templates?"
        , "2. SEMANTICS: Do the differing tokens change the operational meaning?"
        , "3. DECISION: MERGE if same operation with cosmetic differences; KEEP_SEPARATE if genuinely distinct."
        , ""
        , "MERGE examples:"
        , "  'middleware - <*> <*> get <*> µs' vs 'middleware - <*> <*> post <*> µs'"
        , "    → KEEP_SEPARATE (different HTTP methods are semantically distinct operations)"
        , "  'user <*> logged in from <*>' vs 'user {uuid} logged in from {ipv4}'"
        , "    → MERGE (same event, only placeholder types differ)"
        , ""
        , "Respond with a JSON array of objects, one per pair:"
        , "  - \"index\": the pair index (0-based)"
        , "  - \"decision\": \"MERGE\" or \"KEEP_SEPARATE\""
        , ""
        , "Example: [{\"index\": 0, \"decision\": \"MERGE\"}, {\"index\": 1, \"decision\": \"KEEP_SEPARATE\"}]"
        ]
    templatesPart = unlines $ "Templates:" : zipWith (\i t -> "  [" <> show i <> "] " <> t) [0 :: Int ..] allTemplates
    pairsPart = unlines $ "Pairs to evaluate:" : zipWith formatPair [0 :: Int ..] pairs
    formatPair i (a, b) =
      let aIdx = fromMaybe 0 $ Map.lookup a templateIndex
          bIdx = fromMaybe 0 $ Map.lookup b templateIndex
       in "  Pair " <> show i <> ": [" <> show aIdx <> "] vs [" <> show bIdx <> "]"


-- | Shared trivial tokens excluded from meaningful comparison across all pattern types.
trivialTokens :: S.Set Text
trivialTokens =
  S.fromList
    [ "info"
    , "warn"
    , "error"
    , "debug"
    , "trace"
    , "fatal"
    , "exception"
    , "failed"
    , "invalid"
    , "null"
    , "undefined"
    , "the"
    , "a"
    , "an"
    , "in"
    , "on"
    , "at"
    , "to"
    , "for"
    , "of"
    , "from"
    , "with"
    , "by"
    , "is"
    , "was"
    , "-"
    , "--"
    , "->"
    , "="
    , "=="
    , ":"
    , "::"
    , "|"
    ]


-- | Extract meaningful (non-placeholder, non-trivial) tokens from a pattern.
meaningfulTokens :: Text -> S.Set Text
meaningfulTokens = S.filter (\t -> not (S.member (T.toLower t) trivialTokens) && T.length t > 1) . contentTokens


-- | Check if two patterns share meaningful tokens (or both have none).
shareMeaningfulTokens :: Text -> Text -> Bool
shareMeaningfulTokens a b =
  let toksA = meaningfulTokens a
      toksB = meaningfulTokens b
   in (S.null toksA && S.null toksB) || not (S.null $ S.intersection toksA toksB)


-- | Split "ErrorType: message" into (errorType, message). Returns ("", full) if no ": " separator.
splitErrorType :: Text -> (Text, Text)
splitErrorType t = case T.breakOn ": " t of
  (et, rest) | not (T.null rest) -> (et, T.drop 2 rest)
  _ -> ("", t)


-- | Pre-filter gate: patterns must share at least one meaningful non-placeholder token.
-- Excludes log levels, common prepositions, and single-char punctuation.
--
-- >>> logCanMerge "INFO user <*> logged in" "INFO user <*> signed in"
-- True
--
-- >>> logCanMerge "middleware <*> GET <*>" "payment <*> processed <*>"
-- False
--
-- >>> logCanMerge "<*> <*> <*>" "<*> <*>"
-- True
logCanMerge :: Text -> Text -> Bool
logCanMerge = shareMeaningfulTokens


-- | AdaParser-inspired post-merge verification. Converts templateA to a regex and checks
-- if sampleLogB matches it. Catches nonsensical LLM merge decisions.
--
-- >>> verifyMergeDecision "user <*> logged in from <*>" "user alice logged in from 10.0.0.1"
-- True
--
-- >>> verifyMergeDecision "user <*> logged in" "payment processed for order 123"
-- False
--
-- >>> verifyMergeDecision "GET /api/<*>/users" "GET /api/v2/users"
-- True
--
-- >>> verifyMergeDecision "error in <*> module" "error in auth module"
-- True
verifyMergeDecision :: Text -> Text -> Bool
verifyMergeDecision template sampleLog = (toString sampleLog :: String) =~ (toString regexPattern :: String)
  where
    regexPattern = "^" <> T.intercalate ".+" (map escapeRegex $ T.splitOn "<*>" expanded) <> "$"
    expanded = normalizeForEmbedding template
    escapeRegex = T.concatMap \c -> if c `elem` (".+*?^${}()|[]\\" :: String) then "\\" <> one c else one c


-- | Normalize an error pattern for embedding. Splits on ": " to separate the
-- error type from the message, applies normalizeMessage to the message part
-- (replacing UUIDs, IPs, timestamps, numbers with placeholders), keeps error type verbatim.
--
-- >>> normalizeErrorForEmbedding "TypeError: Cannot read property of user c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
-- "TypeError: Cannot read property of user {uuid}"
--
-- >>> normalizeErrorForEmbedding "Connection refused to 192.168.1.100:5432"
-- "Connection refused to {ipv4}{port}"
--
-- >>> normalizeErrorForEmbedding "NullPointerException: value at index 42"
-- "NullPointerException: value at index {integer}"
normalizeErrorForEmbedding :: Text -> Text
normalizeErrorForEmbedding txt =
  let (errType, msg) = splitErrorType txt
   in if T.null errType then EF.normalizeMessage txt else errType <> ": " <> EF.normalizeMessage msg


-- | Pre-filter gate for error pattern merging. Candidates must either share the
-- same error type OR share a meaningful non-placeholder token in the message.
--
-- >>> errorCanMerge "TypeError: Cannot read x" "TypeError: Cannot read y"
-- True
--
-- >>> errorCanMerge "TypeError: Cannot read x" "ValueError: invalid input"
-- False
--
-- >>> errorCanMerge "Connection refused to host" "Connection refused to server"
-- True
--
-- >>> errorCanMerge "NullPointerException: foo" "TimeoutError: bar"
-- False
errorCanMerge :: Text -> Text -> Bool
errorCanMerge a b =
  let (typeA, msgA) = splitErrorType a
      (typeB, msgB) = splitErrorType b
   in (not (T.null typeA) && typeA == typeB) || shareMeaningfulTokens msgA msgB


-- | Error-aware CoT judge prompt. Shows all error patterns in a numbered list,
-- then pairs. Reasoning: Structure (error type match?) -> Semantics (same root cause?) -> Decision.
buildErrorJudgePrompt :: [(Text, Text)] -> Text
buildErrorJudgePrompt pairs = systemPart <> "\n\n" <> patternsPart <> "\n" <> pairsPart
  where
    allPatterns = ordNub $ concatMap (\(a, b) -> [a, b]) pairs
    patternIndex = Map.fromList $ zip allPatterns [0 :: Int ..]
    systemPart =
      unlines
        [ "You are an error pattern deduplication judge. Below is a numbered list of error"
        , "patterns, followed by candidate pairs to evaluate."
        , ""
        , "For each pair, reason through these steps:"
        , "1. STRUCTURE: Do they share the same error type (e.g. TypeError, NullPointerException)?"
        , "2. SEMANTICS: Do they point to the same root cause, differing only in variable values"
        , "   (IDs, timestamps, hostnames, file paths, line numbers)?"
        , "3. DECISION: MERGE if same root cause with cosmetic differences; KEEP_SEPARATE if genuinely distinct."
        , ""
        , "MERGE when the same error/exception differs only in runtime variables (dates, line numbers,"
        , "IDs, timestamps, counts, file paths) — these are the same bug producing slightly different messages:"
        , "  'NullPointerException at com.app.Service.process(Service.java:42)'"
        , "    vs 'NullPointerException at com.app.Service.process(Service.java:87)'"
        , "    → MERGE (same exception, only line number differs)"
        , "  'TypeError: Cannot read properties of undefined (reading userId)'"
        , "    vs 'TypeError: Cannot read properties of undefined (reading userId)'"
        , "    → MERGE (identical error with different runtime values normalized away)"
        , "  'Query timeout after 30000ms on 2024-01-15' vs 'Query timeout after 30000ms on 2024-02-20'"
        , "    → MERGE (same timeout error, only date differs)"
        , ""
        , "KEEP_SEPARATE when errors represent genuinely different bugs or failure modes:"
        , "  'TypeError: x is not a function' vs 'TypeError: Cannot read properties of undefined'"
        , "    → KEEP_SEPARATE (same type but different bugs — wrong call vs null access)"
        , "  'Auth token expired' vs 'Database connection timeout'"
        , "    → KEEP_SEPARATE (unrelated failure modes)"
        , "  'OutOfMemoryError: heap space' vs 'StackOverflowError: infinite recursion'"
        , "    → KEEP_SEPARATE (different resource exhaustion issues)"
        , ""
        , "Respond with a JSON array of objects, one per pair:"
        , "  - \"index\": the pair index (0-based)"
        , "  - \"decision\": \"MERGE\" or \"KEEP_SEPARATE\""
        , ""
        , "Example: [{\"index\": 0, \"decision\": \"MERGE\"}, {\"index\": 1, \"decision\": \"KEEP_SEPARATE\"}]"
        ]
    patternsPart = unlines $ "Error patterns:" : zipWith (\i p -> "  [" <> show i <> "] " <> p) [0 :: Int ..] allPatterns
    pairsPart = unlines $ "Pairs to evaluate:" : zipWith formatPair [0 :: Int ..] pairs
    formatPair i (a, b) =
      let aIdx = fromMaybe 0 $ Map.lookup a patternIndex
          bIdx = fromMaybe 0 $ Map.lookup b patternIndex
       in "  Pair " <> show i <> ": [" <> show aIdx <> "] vs [" <> show bIdx <> "]"
