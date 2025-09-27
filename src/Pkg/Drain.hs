module Pkg.Drain (
  DrainTree (..),
  defaultDrainConfig,
  emptyDrainTree,
  updateTreeWithLog,
  getAllLogGroups,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Relude


data LogGroup = LogGroup
  { template :: V.Vector Text -- Template with wildcards (e.g., ["GET", "<*>", "HTTP/1.1"])
  , templateStr :: Text -- String representation for display
  , logIds :: V.Vector Text -- References to actual logs
  , frequency :: Int -- Number of logs matching this pattern
  , firstSeen :: UTCTime -- When pattern was first created
  , lastSeen :: UTCTime -- When pattern was last updated
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
  { tokenCount :: Int -- Number of tokens (avoiding 'length' name conflict)
  , nodes :: V.Vector DrainLevelTwo -- Second level nodes
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


-- deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake DrainLevelOne

-- Level 0: Root of the DRAIN tree
data DrainTree = DrainTree
  { children :: V.Vector DrainLevelOne -- First level nodes (grouped by length)
  , totalLogs :: Int -- Total logs processed
  , totalPatterns :: Int -- Total unique patterns found
  , config :: DrainConfig -- Configuration parameters
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


data DrainConfig = DrainConfig
  { similarityThreshold :: Double -- Threshold for pattern matching (0.0-1.0)
  , maxLogGroups :: Int -- Maximum clusters per leaf
  , wildcardToken :: Text -- Token used for wildcards (usually "<*>")
  }
  deriving (Generic, Show)
  deriving anyclass (NFData)


defaultDrainConfig :: DrainConfig
defaultDrainConfig =
  DrainConfig
    { similarityThreshold = 0.5
    , maxLogGroups = 1000
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


createLogGroup :: V.Vector Text -> Text -> Text -> UTCTime -> LogGroup
createLogGroup templateTokens templateString logId now =
  LogGroup
    { template = templateTokens
    , templateStr = templateString
    , logIds = V.singleton logId
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


updateTreeWithLog :: DrainTree -> Int -> Text -> V.Vector Text -> Text -> Text -> UTCTime -> DrainTree
updateTreeWithLog tree tokenCount firstToken tokensVec logId logContent now =
  let (updatedChildren, wasUpdated) = updateOrCreateLevelOne (children tree) tokenCount firstToken tokensVec logId logContent now (config tree)
      newTotalLogs = totalLogs tree + 1
      newTotalPatterns = if wasUpdated then totalPatterns tree else totalPatterns tree + 1
   in tree
        { children = updatedChildren
        , totalLogs = newTotalLogs
        , totalPatterns = newTotalPatterns
        }


updateOrCreateLevelOne :: V.Vector DrainLevelOne -> Int -> Text -> V.Vector Text -> Text -> Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelOne, Bool)
updateOrCreateLevelOne levelOnes targetCount firstToken tokensVec logId logContent now config =
  case V.findIndex (\level -> tokenCount level == targetCount) levelOnes of
    Just index ->
      let existingLevel = levelOnes V.! index
          (updatedChildren, wasUpdated) = updateOrCreateLevelTwo (nodes existingLevel) firstToken tokensVec logId logContent now config
          updatedLevel = existingLevel{nodes = updatedChildren}
          updatedLevelOnes = levelOnes V.// [(index, updatedLevel)]
       in (updatedLevelOnes, wasUpdated)
    Nothing ->
      let newLogGroup = createLogGroup tokensVec (T.unwords $ V.toList tokensVec) logId now
          newLevelTwo = DrainLevelTwo{firstToken = firstToken, logGroups = V.singleton newLogGroup}
          newLevelOne = DrainLevelOne{tokenCount = targetCount, nodes = V.singleton newLevelTwo}
          updatedLevelOnes = V.cons newLevelOne levelOnes
       in (updatedLevelOnes, False)


updateOrCreateLevelTwo :: V.Vector DrainLevelTwo -> Text -> V.Vector Text -> Text -> Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelTwo, Bool)
updateOrCreateLevelTwo levelTwos targetToken tokensVec logId logContent now config =
  case V.findIndex (\level -> firstToken level == targetToken) levelTwos of
    Just index ->
      let existingLevel = levelTwos V.! index
          (updatedLogGroups, wasUpdated) = updateOrCreateLogGroup (logGroups existingLevel) tokensVec logId logContent now config
          updatedLevel = existingLevel{logGroups = updatedLogGroups}
          updatedLevelTwos = levelTwos V.// [(index, updatedLevel)]
       in (updatedLevelTwos, wasUpdated)
    Nothing ->
      let newLogGroup = createLogGroup tokensVec (T.unwords $ V.toList tokensVec) logId now
          newLevelTwo = DrainLevelTwo{firstToken = targetToken, logGroups = V.singleton newLogGroup}
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


updateOrCreateLogGroup :: V.Vector LogGroup -> V.Vector Text -> Text -> Text -> UTCTime -> DrainConfig -> (V.Vector LogGroup, Bool)
updateOrCreateLogGroup logGroups tokensVec logId logContent now config =
  case findBestMatch logGroups tokensVec (similarityThreshold config) of
    Just (index, bestGroup) ->
      let updatedTemplate =
            if V.length tokensVec == V.length (template bestGroup)
              then mergeTemplates (template bestGroup) tokensVec (wildcardToken config)
              else template bestGroup
          updatedGroup = updateLogGroupWithTemplate bestGroup updatedTemplate logId logContent now
          updatedGroups = logGroups V.// [(index, updatedGroup)]
       in (updatedGroups, True)
    Nothing ->
      if V.length logGroups >= maxLogGroups config
        then
          let victimIdx = leastRecentlyUsedIndex logGroups
              newGroup = createLogGroup tokensVec (T.unwords $ V.toList tokensVec) logId now
              updatedGroups = logGroups V.// [(victimIdx, newGroup)]
           in (updatedGroups, False)
        else
          let newGroup = createLogGroup tokensVec (T.unwords $ V.toList tokensVec) logId now
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
updateLogGroupWithTemplate :: LogGroup -> V.Vector Text -> Text -> Text -> UTCTime -> LogGroup
updateLogGroupWithTemplate group' newTemplate logId originalLog now =
  group'
    { template = newTemplate
    , templateStr = T.unwords $ V.toList newTemplate
    , logIds = V.cons logId (logIds group')
    , frequency = frequency group' + 1
    , lastSeen = now
    }


getAllLogGroups :: DrainTree -> V.Vector (Text, V.Vector Text)
getAllLogGroups tree =
  let levelOnes = children tree
      levelTwos = V.concatMap nodes levelOnes
      allLogGroups = V.concatMap logGroups levelTwos
   in V.map (\grp -> (templateStr grp, logIds grp)) allLogGroups
