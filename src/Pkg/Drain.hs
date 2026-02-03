module Pkg.Drain (
  DrainTree (..),
  defaultDrainConfig,
  emptyDrainTree,
  updateTreeWithLog,
  generateDrainTokens,
  getAllLogGroups,
  -- Exported for testing
  tokenize,
  extractQuoted,
  extractBracketedContent,
) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Vector qualified as V
import Models.Telemetry.Telemetry (SeverityLevel (..))
import Relude
import RequestMessages (valueToFields)
import Utils (extractMessageAndTargetKeyFromLog, extractMessageFromLog, messageKeys, replaceAllFormats)


data LogGroup = LogGroup
  { template :: V.Vector Text
  , templateStr :: Text
  , exampleLog :: Text
  , fieldPath :: Text
  , logIds :: V.Vector Text
  , frequency :: Int
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


data DrainLevelTwo = DrainLevelTwo
  { firstToken :: Text -- The first token used for grouping
  , fieldPath :: Text
  , logGroups :: V.Vector LogGroup -- Leaf clusters
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


data DrainLevelOne = DrainLevelOne
  { tokenCount :: Int
  , nodes :: V.Vector DrainLevelTwo
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


-- Level 0: Root of the DRAIN tree
data DrainTree = DrainTree
  { children :: V.Vector DrainLevelOne
  , totalLogs :: Int
  , totalPatterns :: Int
  , config :: DrainConfig
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


data DrainConfig = DrainConfig
  { similarityThreshold :: Double
  , maxLogGroups :: Int
  , wildcardToken :: Text
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


defaultDrainConfig :: DrainConfig
defaultDrainConfig =
  DrainConfig
    { similarityThreshold = 0.5
    , maxLogGroups = 50
    , wildcardToken = "<*>"
    }


emptyDrainTree :: DrainTree
emptyDrainTree =
  DrainTree
    { children = V.empty
    , totalLogs = 0
    , totalPatterns = 0
    , config = defaultDrainConfig
    }


createLogGroup :: V.Vector Text -> Text -> Text -> Text -> UTCTime -> LogGroup
createLogGroup templateTokens templateString logId field now =
  LogGroup
    { template = templateTokens
    , templateStr = templateString
    , logIds = V.singleton logId
    , exampleLog = templateString
    , fieldPath = field
    , frequency = 1
    , firstSeen = now
    , lastSeen = now
    }


-- Pattern similarity calculation
calculateSimilarity :: V.Vector Text -> V.Vector Text -> Double
calculateSimilarity tokens1 tokens2
  | V.length tokens1 /= V.length tokens2 = 0.0
  | V.null tokens1 = 1.0
  | otherwise =
      let matches = V.length $ V.filter Relude.id $ V.zipWith (==) tokens1 tokens2
          total = V.length tokens1
       in fromIntegral matches / fromIntegral total


updateTreeWithLog :: DrainTree -> Int -> Text -> V.Vector Text -> Text -> Bool -> Text -> Text -> UTCTime -> DrainTree
updateTreeWithLog tree tokenCount firstToken tokensVec logId isSampleLog logContent field now =
  let (updatedChildren, wasUpdated) = updateOrCreateLevelOne (children tree) tokenCount firstToken tokensVec logId isSampleLog logContent field now (config tree)
      newTotalLogs = totalLogs tree + 1
      newTotalPatterns = if wasUpdated then totalPatterns tree else totalPatterns tree + 1
   in tree
        { children = updatedChildren
        , totalLogs = newTotalLogs
        , totalPatterns = newTotalPatterns
        }


updateOrCreateLevelOne :: V.Vector DrainLevelOne -> Int -> Text -> V.Vector Text -> Text -> Bool -> Text -> Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelOne, Bool)
updateOrCreateLevelOne levelOnes targetCount firstToken tokensVec logId isSampleLog logContent field now config =
  case V.findIndex (\level -> level.tokenCount == targetCount) levelOnes of
    Just index ->
      let existingLevel = levelOnes V.! index
          (updatedChildren, wasUpdated) = updateOrCreateLevelTwo (nodes existingLevel) firstToken tokensVec logId isSampleLog logContent field now config
          updatedLevel = existingLevel{nodes = updatedChildren}
          updatedLevelOnes = levelOnes V.// [(index, updatedLevel)]
       in (updatedLevelOnes, wasUpdated)
    Nothing ->
      let newLogGroup = createLogGroup tokensVec (unwords $ V.toList tokensVec) logId field now
          newLevelTwo = DrainLevelTwo{firstToken = firstToken, fieldPath = field, logGroups = V.singleton newLogGroup}
          newLevelOne = DrainLevelOne{tokenCount = targetCount, nodes = V.singleton newLevelTwo}
          updatedLevelOnes = V.cons newLevelOne levelOnes
       in (updatedLevelOnes, False)


updateOrCreateLevelTwo :: V.Vector DrainLevelTwo -> Text -> V.Vector Text -> Text -> Bool -> Text -> Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelTwo, Bool)
updateOrCreateLevelTwo levelTwos targetToken tokensVec logId isSampleLog logContent field now config =
  case V.findIndex (\level -> level.firstToken == targetToken && level.fieldPath == field) levelTwos of
    Just index ->
      let existingLevel = levelTwos V.! index
          (updatedLogGroups, wasUpdated) = updateOrCreateLogGroup (logGroups existingLevel) tokensVec logId isSampleLog logContent field now config
          updatedLevel = existingLevel{logGroups = updatedLogGroups}
          updatedLevelTwos = levelTwos V.// [(index, updatedLevel)]
       in (updatedLevelTwos, wasUpdated)
    Nothing ->
      let newLogGroup = createLogGroup tokensVec (unwords $ V.toList tokensVec) logId field now
          newLevelTwo = DrainLevelTwo{firstToken = targetToken, fieldPath = field, logGroups = V.singleton newLogGroup}
          updatedLevelTwos = V.cons newLevelTwo levelTwos
       in (updatedLevelTwos, False)


leastRecentlyUsedIndex :: V.Vector LogGroup -> Int
leastRecentlyUsedIndex logGroups =
  V.ifoldl'
    ( \acc i g ->
        case acc of
          Nothing -> Just (i, lastSeen g)
          Just (j, t) ->
            if lastSeen g < t
              then Just (i, lastSeen g)
              else Just (j, t)
    )
    Nothing
    logGroups
    & maybe 0 fst


updateOrCreateLogGroup :: V.Vector LogGroup -> V.Vector Text -> Text -> Bool -> Text -> Text -> UTCTime -> DrainConfig -> (V.Vector LogGroup, Bool)
updateOrCreateLogGroup logGroups tokensVec logId isSampleLog logContent field now config =
  case findBestMatch logGroups tokensVec (similarityThreshold config) of
    Just (index, bestGroup) ->
      let updatedTemplate =
            if V.length tokensVec == V.length (template bestGroup)
              then mergeTemplates (template bestGroup) tokensVec (wildcardToken config)
              else template bestGroup
          updatedGroup = updateLogGroupWithTemplate bestGroup updatedTemplate logId isSampleLog logContent field now
          updatedGroups = logGroups V.// [(index, updatedGroup)]
       in (updatedGroups, True)
    Nothing ->
      if V.length logGroups >= maxLogGroups config
        then
          let victimIdx = leastRecentlyUsedIndex logGroups
              newGroup = createLogGroup tokensVec (unwords $ V.toList tokensVec) logId field now
              updatedGroups = logGroups V.// [(victimIdx, newGroup)]
           in (updatedGroups, False)
        else
          let newGroup = createLogGroup tokensVec (unwords $ V.toList tokensVec) logId field now
              updatedGroups = V.cons newGroup logGroups
           in (updatedGroups, False)


findBestMatch :: V.Vector LogGroup -> V.Vector Text -> Double -> Maybe (Int, LogGroup)
findBestMatch logGroups tokensVec threshold =
  let candidates = V.indexed logGroups
      similarities =
        V.map
          ( \(idx, grp) ->
              (idx, grp, calculateSimilarity (template grp) tokensVec)
          )
          candidates
      validMatches = V.filter (\(_, _, sim) -> sim >= threshold) similarities
   in if V.null validMatches
        then Nothing
        else
          let (bestIdx, bestGroup, _) = V.maximumBy (\(_, _, s1) (_, _, s2) -> compare s1 s2) validMatches
           in Just (bestIdx, bestGroup)


mergeTemplates :: V.Vector Text -> V.Vector Text -> Text -> V.Vector Text
mergeTemplates template1 template2 wildcardToken =
  V.zipWith (\t1 t2 -> if t1 == t2 then t1 else wildcardToken) template1 template2


-- Update log group with new template and log information
updateLogGroupWithTemplate :: LogGroup -> V.Vector Text -> Text -> Bool -> Text -> Text -> UTCTime -> LogGroup
updateLogGroupWithTemplate group' newTemplate logId isSampleLog originalLog field now =
  group'
    { template = newTemplate
    , templateStr = unwords $ V.toList newTemplate
    , exampleLog = if isSampleLog then originalLog else exampleLog group'
    , logIds = V.cons logId (logIds group')
    , fieldPath = field
    , frequency = frequency group' + 1
    , lastSeen = now
    }


getAllLogGroups :: DrainTree -> V.Vector (Text, Text, Text, V.Vector Text)
getAllLogGroups tree =
  let levelOnes = children tree
      levelTwos = V.concatMap nodes levelOnes
      allLogGroups = V.concatMap logGroups levelTwos
   in V.map (\grp -> (grp.exampleLog, grp.fieldPath, templateStr grp, logIds grp)) allLogGroups


generateDrainTokens :: T.Text -> (V.Vector T.Text, Text)
generateDrainTokens content
  | looksLikeJson content =
      fromMaybe fallback $ do
        jsonValue <- AE.decodeStrict' (encodeUtf8 content)
        (msg, key) <- extractMessageAndTargetKeyFromLog jsonValue
        pure (tokenizeUnstructured msg, key)
  | otherwise = fallback
  where
    fallback = (tokenizeUnstructured content, "body")


looksLikeJson :: T.Text -> Bool
looksLikeJson t =
  ("{" `T.isInfixOf` t && "}" `T.isSuffixOf` t)
    || ("[" `T.isInfixOf` t && "]" `T.isSuffixOf` t)


-- | Tokenize unstructured log content with smart handling of:
-- - Embedded JSON arrays → [*]
-- - Embedded JSON objects → {*}
-- - Key=value patterns → key=<*>
-- - Quoted strings as single tokens
tokenizeUnstructured :: T.Text -> V.Vector T.Text
tokenizeUnstructured content =
  let preprocessed = replaceAllFormats content
   in V.fromList $ words preprocessed


-- | Tokenize text handling quotes, embedded JSON, and key=value patterns
tokenize :: T.Text -> [T.Text]
tokenize txt
  | T.null txt = []
  | otherwise = go (T.stripStart txt)
  where
    go t
      | T.null t = []
      -- Handle quoted strings as single token
      | T.head t == '"' =
          let (quoted, rest) = extractQuoted '"' (T.tail t)
           in ("\"" <> quoted <> "\"") : go (T.stripStart rest)
      | T.head t == '\'' =
          let (quoted, rest) = extractQuoted '\'' (T.tail t)
           in ("'" <> quoted <> "'") : go (T.stripStart rest)
      -- Handle embedded JSON array
      | T.head t == '[' =
          let (_, rest) = extractBracketedContent '[' ']' t
           in "[*]" : go (T.stripStart rest)
      -- Handle embedded JSON object
      | T.head t == '{' =
          let (_, rest) = extractBracketedContent '{' '}' t
           in "{*}" : go (T.stripStart rest)
      -- Handle regular token (may contain key=value)
      | otherwise =
          let (token, rest) = T.break isTokenDelimiter t
           in if T.null token
                then go (T.drop 1 t)
                else processToken token ++ go (T.stripStart rest)

    isTokenDelimiter c = c `elem` [' ', '\t', '\n', '\r', '[', '{']

    -- Process a token, handling key=value patterns
    processToken :: T.Text -> [T.Text]
    processToken token =
      case T.breakOn "=" token of
        (key, rest)
          | not (T.null rest) && not (T.null key) && T.length rest > 1 ->
              -- Has key=value pattern: keep key=<*>
              [key <> "=<*>"]
          | otherwise -> [token]


-- | Extract content within quotes, handling escaped quotes
extractQuoted :: Char -> T.Text -> (T.Text, T.Text)
extractQuoted quoteChar txt = go txt ""
  where
    go t acc
      | T.null t = (acc, t)
      | T.head t == '\\' && T.length t > 1 && T.index t 1 == quoteChar =
          go (T.drop 2 t) (acc <> one '\\' <> one quoteChar)
      | T.head t == quoteChar = (acc, T.tail t)
      | otherwise = go (T.tail t) (acc <> one (T.head t))


-- | Extract content within brackets, handling nested brackets
extractBracketedContent :: Char -> Char -> T.Text -> (T.Text, T.Text)
extractBracketedContent open close txt
  | T.null txt = ("", txt)
  | T.head txt /= open = ("", txt)
  | otherwise = go (T.tail txt) "" (1 :: Int)
  where
    go t acc depth
      | T.null t = (acc, t)
      | depth == 0 = (acc, t)
      | T.head t == open = go (T.tail t) (acc <> one open) (depth + 1)
      | T.head t == close =
          if depth == 1
            then (acc, T.tail t)
            else go (T.tail t) (acc <> one close) (depth - 1)
      | otherwise = go (T.tail t) (acc <> one (T.head t)) depth
