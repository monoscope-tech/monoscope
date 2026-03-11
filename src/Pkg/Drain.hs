{-# LANGUAGE StrictData #-}

module Pkg.Drain (
  DrainTree (..),
  DrainConfig (..),
  DrainResult (..),
  defaultDrainConfig,
  emptyDrainTree,
  updateTreeWithLog,
  buildDrainTree,
  buildDrainTreeWithMapping,
  generateDrainTokens,
  generateSummaryDrainTokens,
  tokenizeForDrain,
  getAllLogGroups,
  normalizePlaceholder,
  drainPlaceholders,
) where

import Data.Char (isSpace)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Vector qualified as V
import Relude
import Utils (replaceAllFormats)


maxLogIdSamples :: Int
maxLogIdSamples = 10000


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
    { similarityThreshold = 0.7
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


templateText :: V.Vector Text -> Text
templateText = unwords . V.toList


createLogGroup :: V.Vector Text -> Text -> Text -> Maybe Text -> UTCTime -> LogGroup
createLogGroup templateTokens templateString logId sampleContent now =
  LogGroup
    { template = templateTokens
    , templateStr = templateString
    , logIds = V.singleton logId
    , exampleLog = fromMaybe templateString sampleContent
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


-- | Returns (updatedTree, matchedTemplateStr)
updateTreeWithLogM :: DrainTree -> Int -> Text -> V.Vector Text -> Text -> Maybe Text -> UTCTime -> (DrainTree, Text)
updateTreeWithLogM tree tokenCount' firstToken tokensVec logId sampleContent now =
  let (updatedChildren, wasUpdated, tpl) = updateOrCreateLevelOne (children tree) tokenCount' firstToken tokensVec logId sampleContent now (config tree)
      newTotalLogs = totalLogs tree + 1
      newTotalPatterns = if wasUpdated then totalPatterns tree else totalPatterns tree + 1
   in (tree{children = updatedChildren, totalLogs = newTotalLogs, totalPatterns = newTotalPatterns}, tpl)


updateTreeWithLog :: DrainTree -> Int -> Text -> V.Vector Text -> Text -> Maybe Text -> UTCTime -> DrainTree
updateTreeWithLog tree tc ft tv lid sc now = fst $ updateTreeWithLogM tree tc ft tv lid sc now


updateOrCreateLevelOne :: V.Vector DrainLevelOne -> Int -> Text -> V.Vector Text -> Text -> Maybe Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelOne, Bool, Text)
updateOrCreateLevelOne levelOnes targetCount firstToken tokensVec logId sampleContent now cfg =
  case V.findIndex (\level -> tokenCount level == targetCount) levelOnes of
    Just index ->
      let existing = levelOnes V.! index
          (updatedChildren, wasUpdated, tpl) = updateOrCreateLevelTwo (nodes existing) firstToken tokensVec logId sampleContent now cfg
       in (levelOnes V.// [(index, existing{nodes = updatedChildren})], wasUpdated, tpl)
    Nothing -> (V.cons (DrainLevelOne{tokenCount = targetCount, nodes = V.singleton (DrainLevelTwo{firstToken, logGroups = V.singleton newGroup})}) levelOnes, False, newTpl)
  where
    newTpl = templateText tokensVec
    newGroup = createLogGroup tokensVec newTpl logId sampleContent now


updateOrCreateLevelTwo :: V.Vector DrainLevelTwo -> Text -> V.Vector Text -> Text -> Maybe Text -> UTCTime -> DrainConfig -> (V.Vector DrainLevelTwo, Bool, Text)
updateOrCreateLevelTwo levelTwos targetToken tokensVec logId sampleContent now cfg =
  case V.findIndex (\level -> firstToken level == targetToken) levelTwos of
    Just index ->
      let existing = levelTwos V.! index
          (updatedLogGroups, wasUpdated, tpl) = updateOrCreateLogGroup (logGroups existing) tokensVec logId sampleContent now cfg
       in (levelTwos V.// [(index, existing{logGroups = updatedLogGroups})], wasUpdated, tpl)
    Nothing -> (V.cons (DrainLevelTwo{firstToken = targetToken, logGroups = V.singleton newGroup}) levelTwos, False, newTpl)
  where
    newTpl = templateText tokensVec
    newGroup = createLogGroup tokensVec newTpl logId sampleContent now


-- | Only called when V.length logGroups >= maxLogGroups (> 0)
leastRecentlyUsedIndex :: V.Vector LogGroup -> Int
leastRecentlyUsedIndex = V.minIndexBy (comparing lastSeen)


updateOrCreateLogGroup :: V.Vector LogGroup -> V.Vector Text -> Text -> Maybe Text -> UTCTime -> DrainConfig -> (V.Vector LogGroup, Bool, Text)
updateOrCreateLogGroup lgs tokensVec logId sampleContent now cfg =
  case findBestMatch lgs tokensVec (similarityThreshold cfg) of
    Just (index, bestGroup) ->
      let updatedTemplate =
            if V.length tokensVec == V.length (template bestGroup)
              then mergeTemplates (template bestGroup) tokensVec (wildcardToken cfg)
              else template bestGroup
          updatedGroup = updateLogGroupWithTemplate bestGroup updatedTemplate logId sampleContent now
          updatedGroups = lgs V.// [(index, updatedGroup)]
       in (updatedGroups, True, updatedGroup.templateStr)
    Nothing ->
      let newTpl = templateText tokensVec
          newGroup = createLogGroup tokensVec newTpl logId sampleContent now
       in if V.length lgs >= maxLogGroups cfg
            then (lgs V.// [(leastRecentlyUsedIndex lgs, newGroup)], False, newTpl)
            else (V.cons newGroup lgs, False, newTpl)


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
updateLogGroupWithTemplate :: LogGroup -> V.Vector Text -> Text -> Maybe Text -> UTCTime -> LogGroup
updateLogGroupWithTemplate group' newTemplate logId sampleContent now =
  group'
    { template = newTemplate
    , templateStr = templateText newTemplate
    , exampleLog = fromMaybe group'.exampleLog sampleContent
    , logIds = if V.length group'.logIds >= maxLogIdSamples then group'.logIds else V.cons logId group'.logIds
    , frequency = group'.frequency + 1
    , lastSeen = now
    }


data DrainResult = DrainResult
  { exampleLog :: Text
  , templateStr :: Text
  , templateTokens :: V.Vector Text
  , logIds :: V.Vector Text
  , frequency :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


getAllLogGroups :: DrainTree -> V.Vector DrainResult
getAllLogGroups tree =
  let levelOnes = children tree
      levelTwos = V.concatMap nodes levelOnes
      allLogGroups = V.concatMap logGroups levelTwos
   in V.map (\LogGroup{exampleLog, templateStr, logIds, template, frequency} -> DrainResult{templateTokens = template, ..}) allLogGroups


looksLikeJson :: T.Text -> Bool
looksLikeJson t =
  ("{\"" `T.isPrefixOf` t && "}" `T.isSuffixOf` t)
    || ("[" `T.isPrefixOf` t && "]" `T.isSuffixOf` t)


tokenizeJsonLike :: T.Text -> [T.Text]
tokenizeJsonLike txt
  | T.null txt = []
  | otherwise = go txt
  where
    go t
      | T.null t = []
      | T.head t `elem` (['{', '}', '[', ']', ',', ':'] :: String) =
          let c = one (T.head t)
           in c : go (T.tail t)
      | T.head t == '"' =
          let (quoted, rest) = T.breakOn "\"" (T.tail t)
              token = "\"" <> quoted <> "\"" -- include the quotes
           in token : go (T.drop 1 rest)
      | isSpace (T.head t) =
          go (T.dropWhile isSpace t)
      | otherwise =
          let (chunk, rest) = T.span (\c -> not (isSpace c) && notElem c (['{', '}', '[', ']', ',', ':'] :: String)) t
           in chunk : go rest


-- | Known typed placeholders produced by 'replaceAllFormats'. Normalized to @\<*\>@
-- before Drain tokenization so patterns differing only in placeholder type
-- (e.g. @{uuid}@ vs @{integer}@) route to the same Drain tree branch.
drainPlaceholders :: S.Set Text
drainPlaceholders =
  S.fromList
    [ "{integer}"
    , "{float}"
    , "{hex}"
    , "{uuid}"
    , "{ipv4}"
    , "{port}"
    , "{email}"
    , "{jwt}"
    , "{ssn}"
    , "{sha256}"
    , "{sha1}"
    , "{md5}"
    , "{YYYY-MM-DD}"
    , "{YYYY-MM-DD HH:MM:SS}"
    , "{YYYY-MM-DDThh:mm:ss.sTZD}"
    , "{HH:MM:SS}"
    , "{HH:MM:SS.mmm}"
    , "{Mon DD, YYYY}"
    , "{DD-Mon-YYYY}"
    ]


-- | Replace a known Drain placeholder with @\<*\>@, pass other tokens through.
--
-- >>> normalizePlaceholder "{uuid}"
-- "<*>"
--
-- >>> normalizePlaceholder "{integer}"
-- "<*>"
--
-- >>> normalizePlaceholder "<*>"
-- "<*>"
--
-- >>> normalizePlaceholder "hello"
-- "hello"
--
-- >>> normalizePlaceholder "{\"context\":\"Nest\"}"
-- "{\"context\":\"Nest\"}"
normalizePlaceholder :: Text -> Text
normalizePlaceholder t = if S.member t drainPlaceholders then "<*>" else t


generateDrainTokens :: T.Text -> V.Vector T.Text
generateDrainTokens content =
  let replaced = replaceAllFormats content
   in V.map normalizePlaceholder
        $ if looksLikeJson replaced
          then V.fromList (tokenizeJsonLike replaced)
          else V.fromList $ words replaced


-- | Tokenize already-normalized text for Drain without re-running replaceAllFormats.
tokenizeForDrain :: T.Text -> V.Vector T.Text
tokenizeForDrain content =
  V.map normalizePlaceholder
    $ if looksLikeJson content
      then V.fromList (tokenizeJsonLike content)
      else V.fromList $ words content


-- | Markup-aware tokenizer for summary fields that preserves @field;style⇒value@ format.
-- Splits by words (avoids looksLikeJson false positives on markup tokens).
-- For tokens containing ⇒: preserves the prefix, normalizes only the value after ⇒.
-- Filters out resource/attributes metadata blobs (high-cardinality container metadata).
-- Collapses consecutive @\<*\>@ wildcards to prevent Drain level-1 branch fragmentation.
--
-- >>> import Data.Vector qualified as V
-- >>> V.toList $ generateSummaryDrainTokens "status_code;badge-2xx⇒200"
-- ["status_code;badge-2xx\8658<*>"]
--
-- >>> V.toList $ generateSummaryDrainTokens "user logged in at 192.168.1.1"
-- ["user","logged","in","at","<*>"]
--
-- >>> V.toList $ generateSummaryDrainTokens "method;bold⇒GET path;code⇒/api/users/123 status_code;badge-2xx⇒200"
-- ["method;bold\8658GET","path;code\8658/api/users/{integer}","status_code;badge-2xx\8658<*>"]
--
-- >>> V.toList $ generateSummaryDrainTokens "method;bold⇒GET resource;text-textWeak⇒{\"container\":{\"id\":\"abc\"}} INFO started"
-- ["method;bold\8658GET","INFO","started"]
generateSummaryDrainTokens :: T.Text -> V.Vector T.Text
generateSummaryDrainTokens content =
  V.fromList $ map normalizeMarkupToken $ filter (not . isMetadataBlob) $ words content
  where
    isMetadataBlob tok =
      any
        (`T.isPrefixOf` tok)
        ["resource;text-textWeak\8658", "attributes;text-textWeak\8658"]
    normalizeMarkupToken tok = case T.breakOn "\8658" tok of
      (prefix, rest)
        | Just val <- T.stripPrefix "\8658" rest -> prefix <> "\8658" <> normalizePlaceholders (replaceAllFormats val)
        | otherwise -> normalizePlaceholders (replaceAllFormats tok)
    normalizePlaceholders = unwords . map normalizePlaceholder . words


-- | Fold items into a DrainTree using a custom tokenizer.
buildDrainTree :: (a -> V.Vector T.Text) -> (a -> Text) -> (a -> Maybe Text) -> DrainTree -> V.Vector a -> UTCTime -> DrainTree
buildDrainTree tokenize logId sampleContent initial items now = fst $ buildDrainTreeWithMapping tokenize logId sampleContent initial items now


-- | Like 'buildDrainTree' but also returns a vector of @(logId, matchedTemplateStr)@ pairs.
buildDrainTreeWithMapping :: (a -> V.Vector T.Text) -> (a -> Text) -> (a -> Maybe Text) -> DrainTree -> V.Vector a -> UTCTime -> (DrainTree, V.Vector (Text, Text))
buildDrainTreeWithMapping tokenize logId sampleContent initial items now =
  let (!finalTree, acc) =
        V.foldl'
          ( \(!tree, !xs) item ->
              let tokens = tokenize item
                  lid = logId item
               in if V.null tokens || T.null lid
                    then (tree, xs)
                    else
                      let (!tree', tpl) = updateTreeWithLogM tree (V.length tokens) (V.head tokens) tokens lid (sampleContent item) now
                       in (tree', (lid, tpl) : xs)
          )
          (initial, [])
          items
   in (finalTree, V.fromList $ reverse acc)
