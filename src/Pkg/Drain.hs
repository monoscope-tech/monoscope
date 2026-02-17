module Pkg.Drain (
  DrainTree (..),
  defaultDrainConfig,
  emptyDrainTree,
  updateTreeWithLog,
  generateDrainTokens,
  getAllLogGroups,
) where

import Data.Char (isSpace)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Vector qualified as V
import Relude
import Utils (replaceAllFormats)


data LogGroup = LogGroup
  { template :: V.Vector Text
  , templateStr :: Text
  , exampleLog :: Text
  , logIds :: V.Vector Text
  , frequency :: Int
  , firstSeen :: UTCTime
  , lastSeen :: UTCTime
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


data DrainLevelTwo = DrainLevelTwo
  { firstToken :: Text -- The first token used for grouping
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


templateText :: V.Vector Text -> Text
templateText = unwords . V.toList


createLogGroup :: V.Vector Text -> Text -> Text -> UTCTime -> LogGroup
createLogGroup templateTokens templateString logId now =
  LogGroup
    { template = templateTokens
    , templateStr = templateString
    , logIds = V.singleton logId
    , exampleLog = templateString
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


updateTreeWithLog :: DrainTree -> Int -> Text -> V.Vector Text -> Text -> Bool -> Text -> UTCTime -> DrainTree
updateTreeWithLog tree tokenCount firstToken tokensVec logId isSampleLog logContent now =
  let (updatedChildren, wasUpdated) = updateOrCreateLevelOne (children tree) tokenCount firstToken tokensVec logId isSampleLog logContent now (config tree)
      newTotalLogs = totalLogs tree + 1
      newTotalPatterns = if wasUpdated then totalPatterns tree else totalPatterns tree + 1
   in tree
        { children = updatedChildren
        , totalLogs = newTotalLogs
        , totalPatterns = newTotalPatterns
        }


updateOrCreateLevelOne :: V.Vector DrainLevelOne -> Int -> Text -> V.Vector Text -> Text -> Bool -> Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelOne, Bool)
updateOrCreateLevelOne levelOnes targetCount firstToken tokensVec logId isSampleLog logContent now config =
  maybe
    (V.cons (DrainLevelOne{tokenCount = targetCount, nodes = V.singleton (DrainLevelTwo{firstToken, logGroups = V.singleton newGroup})}) levelOnes, False)
    (\index ->
      let existing = levelOnes V.! index
          (updatedChildren, wasUpdated) = updateOrCreateLevelTwo (nodes existing) firstToken tokensVec logId isSampleLog logContent now config
       in (levelOnes V.// [(index, existing{nodes = updatedChildren})], wasUpdated))
    (V.findIndex (\level -> tokenCount level == targetCount) levelOnes)
  where
    newGroup = createLogGroup tokensVec (templateText tokensVec) logId now


updateOrCreateLevelTwo :: V.Vector DrainLevelTwo -> Text -> V.Vector Text -> Text -> Bool -> Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelTwo, Bool)
updateOrCreateLevelTwo levelTwos targetToken tokensVec logId isSampleLog logContent now config =
  maybe
    (V.cons (DrainLevelTwo{firstToken = targetToken, logGroups = V.singleton newGroup}) levelTwos, False)
    (\index ->
      let existing = levelTwos V.! index
          (updatedLogGroups, wasUpdated) = updateOrCreateLogGroup (logGroups existing) tokensVec logId isSampleLog logContent now config
       in (levelTwos V.// [(index, existing{logGroups = updatedLogGroups})], wasUpdated))
    (V.findIndex (\level -> firstToken level == targetToken) levelTwos)
  where
    newGroup = createLogGroup tokensVec (templateText tokensVec) logId now


leastRecentlyUsedIndex :: V.Vector LogGroup -> Int
leastRecentlyUsedIndex = V.minIndexBy (comparing lastSeen)


updateOrCreateLogGroup :: V.Vector LogGroup -> V.Vector Text -> Text -> Bool -> Text -> UTCTime -> DrainConfig -> (V.Vector LogGroup, Bool)
updateOrCreateLogGroup logGroups tokensVec logId isSampleLog logContent now config =
  case findBestMatch logGroups tokensVec (similarityThreshold config) of
    Just (index, bestGroup) ->
      let updatedTemplate =
            if V.length tokensVec == V.length (template bestGroup)
              then mergeTemplates (template bestGroup) tokensVec (wildcardToken config)
              else template bestGroup
          updatedGroup = updateLogGroupWithTemplate bestGroup updatedTemplate logId isSampleLog logContent now
          updatedGroups = logGroups V.// [(index, updatedGroup)]
       in (updatedGroups, True)
    Nothing ->
      if V.length logGroups >= maxLogGroups config
        then
          let victimIdx = leastRecentlyUsedIndex logGroups
              newGroup = createLogGroup tokensVec (templateText tokensVec) logId now
              updatedGroups = logGroups V.// [(victimIdx, newGroup)]
           in (updatedGroups, False)
        else
          let newGroup = createLogGroup tokensVec (templateText tokensVec) logId now
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
updateLogGroupWithTemplate :: LogGroup -> V.Vector Text -> Text -> Bool -> Text -> UTCTime -> LogGroup
updateLogGroupWithTemplate group' newTemplate logId isSampleLog originalLog now =
  group'
    { template = newTemplate
    , templateStr = templateText newTemplate
    , exampleLog = if isSampleLog then originalLog else exampleLog group'
    , logIds = V.cons logId (logIds group')
    , frequency = frequency group' + 1
    , lastSeen = now
    }


getAllLogGroups :: DrainTree -> V.Vector (Text, Text, V.Vector Text)
getAllLogGroups tree =
  let levelOnes = children tree
      levelTwos = V.concatMap nodes levelOnes
      allLogGroups = V.concatMap logGroups levelTwos
   in V.map (\grp -> (grp.exampleLog, templateStr grp, logIds grp)) allLogGroups


looksLikeJson :: T.Text -> Bool
looksLikeJson t =
  ("{" `T.isInfixOf` t && "}" `T.isSuffixOf` t)
    || ("[" `T.isInfixOf` t && "]" `T.isSuffixOf` t)


tokenizeJsonLike :: T.Text -> [T.Text]
tokenizeJsonLike txt
  | T.null txt = []
  | otherwise = go txt
  where
    go t
      | T.null t = []
      | T.head t `elem` ['{', '}', '[', ']', ',', ':'] =
          let c = one (T.head t)
           in c : go (T.tail t)
      | T.head t == '"' =
          let (quoted, rest) = T.breakOn "\"" (T.tail t)
              token = "\"" <> quoted <> "\"" -- include the quotes
           in token : go (T.drop 1 rest)
      | isSpace (T.head t) =
          go (T.dropWhile isSpace t)
      | otherwise =
          let (chunk, rest) = T.span (\c -> not (isSpace c) && notElem c ['{', '}', '[', ']', ',', ':']) t
           in chunk : go rest


generateDrainTokens :: T.Text -> V.Vector T.Text
generateDrainTokens content =
  let replaced = replaceAllFormats content
   in if looksLikeJson replaced
        then V.fromList (tokenizeJsonLike replaced)
        else V.fromList $ words replaced
