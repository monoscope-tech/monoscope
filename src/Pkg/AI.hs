{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Pkg.AI
-- Description : Unified AI/LLM system for natural language to KQL query generation
--
-- This module provides LLM-powered query generation with tool calling.
-- The LLM decides when to use tools based on the query complexity.
module Pkg.AI (
  -- * Response Types
  ChatLLMResponse (..),

  -- * Response Parsing
  getAskLLMResponse,
  getNormalTupleReponse,

  -- * Basic LLM Calls
  callOpenAIAPI,
  callOpenAIAPIEff,

  -- * System Prompt
  systemPrompt,

  -- * Agentic Configuration
  AgenticConfig (..),
  ToolLimits (..),

  -- * Agentic Query Execution
  runAgenticQuery,
  defaultAgenticConfig,
  defaultLimits,
) where

import Data.Aeson qualified as AE
import Data.Effectful.LLM (callOpenAIAPI)
import Data.Effectful.LLM qualified as ELLM
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.LLM.OpenAI qualified as OpenAI
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..))
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.RequestDumps (executeSecuredQuery, selectLogTable)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
import OpenAI.V1.Tool qualified as OAITool
import Pkg.Parser (parseQueryToAST)
import Relude hiding (pass)
import System.Types (DB)


data ChatLLMResponse = ChatLLMResponse
  { query :: Text
  , visualization :: Maybe Text
  , timeRange :: Maybe [Text]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


callOpenAIAPIEff :: ELLM.LLM :> es => Text -> Text -> Eff es (Either Text Text)
callOpenAIAPIEff = ELLM.callLLM


getNormalTupleReponse :: Text -> Either Text (Text, Maybe Text)
getNormalTupleReponse response =
  let lines' = lines $ T.strip response
      queryLine = fromMaybe "" (viaNonEmpty head lines')
      vizTypeM = if length lines' > 1 then parseVisualizationType (lines' L.!! 1) else Nothing
      cleanedQuery =
        T.strip
          $ if "```" `T.isPrefixOf` queryLine
            then
              let withoutFirstLine = maybe "" (unlines . toList) $ viaNonEmpty tail (lines queryLine)
                  withoutBackticks = T.takeWhile (/= '`') withoutFirstLine
               in T.strip withoutBackticks
            else queryLine
   in if "Please provide a query"
        `T.isInfixOf` cleanedQuery
        || "I need more"
        `T.isInfixOf` cleanedQuery
        || "Could you please"
        `T.isInfixOf` cleanedQuery
        || T.length cleanedQuery
        < 3
        then Left "INVALID_QUERY_ERROR"
        else Right (cleanedQuery, vizTypeM)


getAskLLMResponse :: Text -> Either Text ChatLLMResponse
getAskLLMResponse response =
  let responseBS = encodeUtf8 response
      decoded = AE.eitherDecode (fromStrict responseBS)
   in case decoded of
        Left err -> Left $ "JSON Decode Error: " <> toText err
        Right apiResponse -> do
          let vType = parseVisualizationType $ fromMaybe "" apiResponse.visualization
              apiResponse' = apiResponse{visualization = vType}
           in Right apiResponse'


parseVisualizationType :: Text -> Maybe Text
parseVisualizationType = \case
  "bar" -> Just "timeseries"
  "line" -> Just "timeseries_line"
  "logs" -> Nothing
  "timeseries" -> Just "timeseries"
  "timeseries_line" -> Just "timeseries_line"
  "bar chart" -> Just "timeseries"
  "line chart" -> Just "timeseries_line"
  "time series" -> Just "timeseries"
  "time series line" -> Just "timeseries_line"
  _ -> Nothing


systemPrompt :: Text
systemPrompt =
  unlines
    [ "You are a helpful assistant that converts natural language queries to KQL (Kusto Query Language) filter expressions."
    , ""
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , ""
    , "Available Operators:"
    , "- Comparison: == != > < >= <="
    , "- Set operations: in !in (e.g., method in (\"GET\", \"POST\"))"
    , "- Text search: has !has (case-insensitive word search)"
    , "- Text collections: has_any has_all (e.g., tags has_any [\"urgent\", \"critical\"])"
    , "- String operations: contains !contains startswith !startswith endswith !endswith"
    , "- Pattern matching: matches =~ (regex, e.g., email matches /.*@company\\.com/)"
    , "- Logical: AND OR (or lowercase and or)"
    , "- Duration values: 100ms 5s 2m 1h (nanoseconds, microseconds, milliseconds, seconds, minutes, hours)"
    , ""
    , "VISUALIZATION TYPES:"
    , "If the query is best visualized as a chart rather than logs, specify the visualization type on a new line after the query:"
    , "- logs: For displaying log entries (default)"
    , "- timeseries (bar): For bar chart time-based visualization"
    , "- timeseries_line (line): For line chart time-based visualization"
    , ""
    , "When to use different visualization types:"
    , "- Use 'logs' for filtering specific log entries or when detailed log information is needed"
    , "- Use 'timeseries' (bar chart) for queries that count occurrences over time, like error counts, status code distribution"
    , "- Use 'timeseries_line' (line chart) for continuous metrics over time, like response times, latency, throughput"
    , ""
    , "IMPORTANT RULES FOR VISUALIZATION:"
    , "1. If the user mentions 'bar chart', 'bar graph', or wants to visualize counts/distributions, ALWAYS add 'visualization: timeseries' on the second line"
    , "2. If the user mentions 'line chart', 'line graph', or wants to visualize trends/metrics over time, ALWAYS add 'visualization: timeseries_line' on the second line"
    , "3. If the query contains a 'summarize' statement, you MUST specify a visualization type on the second line"
    , "4. For chart visualizations (timeseries or timeseries_line), you MUST include a 'summarize' statement in your query"
    , ""
    , "Chart queries follow standard KQL syntax and typically include:"
    , "1. [filters] | summarize <aggregation> by bin_auto(timestamp), [optional field]"
    , ""
    , "The summarize statement can use various aggregation functions like count(), sum(...), avg(...), min(...), max(...), median(...), etc."
    , ""
    , "TIME BINNING:"
    , "- Use bin_auto(timestamp) by DEFAULT - the system will automatically determine the appropriate bin size based on the time range"
    , "- Only use bin(timestamp, <size>) when the user EXPLICITLY specifies a time interval (e.g., 'by hour', 'per minute', 'in 5m intervals')"
    , "- Examples of when to use hardcoded bins: 'show errors by hour' -> bin(timestamp, 1h), 'count per 30 seconds' -> bin(timestamp, 30s)"
    , "- Examples of when to use bin_auto: 'show error trend', 'graph response times', 'chart requests over time' -> bin_auto(timestamp)"
    , ""
    , "Examples of chart queries:"
    , "- \"Show errors by hour\": level == \"ERROR\" | summarize count() by bin(timestamp, 1h)"
    , "- \"Graph request counts by kind in 2h blocks\": | summarize count() by bin(timestamp, 2h), kind"
    , "- \"Line chart of p95 durations by method\": | summarize p95(duration) by bin_auto(timestamp), attributes.http.request.method"
    , ""
    , "Examples:"
    , "- \"show me errors\" -> level == \"ERROR\""
    , "- \"POST requests\" -> attributes.http.request.method == \"POST\""
    , "- \"slow requests\" -> duration > 500ms"
    , "- \"500 errors\" -> attributes.http.response.status_code == \"500\""
    , "- \"GET or POST requests\" -> method in (\"GET\", \"POST\")"
    , "- \"messages containing error\" -> message contains \"error\""
    , "- \"logs with urgent or critical tags\" -> tags has_any [\"urgent\", \"critical\"]"
    , "- \"paths starting with /api\" -> path startswith \"/api\""
    , "- \"emails from company.com\" -> email matches /.*@company\\.com/"
    , "- \"requests taking more than 1 second\" -> duration > 1s"
    , "- \"Show me error count over time\" -> level == \"ERROR\" | summarize count() by bin_auto(timestamp)\nvisualization: timeseries"
    , "- \"Graph of response times\" -> | summarize avg(duration) by bin_auto(timestamp)\nvisualization: timeseries_line"
    , "- \"make a bar chart of requests by kind\" -> | summarize count() by bin_auto(timestamp), kind\nvisualization: timeseries"
    , "- \"line chart of requests over time\" -> | summarize count() by bin_auto(timestamp)\nvisualization: timeseries_line"
    , ""
    , "Time range:"
    , "If the query includes a time range, specify it using the 'timeRange' field in the JSON response:"
    , "{"
    , "  \"timeRange\": [ \"<From: ISO8601>\",  \"<To: ISO8601>\"]"
    , "}"
    , ""
    , "Time range rules:"
    , "DO NOT include timeRange if not requsted by the user"
    , "DO NOT make up a time range if not specified by the user"
    , "DO NOT include timeRange field if the array is empty or incomplete"
    , "DO NOT use code blocks or backticks in your response. Return the raw query directly."
    , "Response format:"
    , "Always return JSON in the following structure:"
    , "{"
    , "  \"query\": \"<KQL filter/query string>\","
    , "  \"visualization\": \"<logs|timeseries|timeseries_line>\","
    , "  \"timeRange\": [\"<From: ISO8601>\", \"<To: ISO8601>\"]"
    , "}"
    , ""
    , "IMPORTANT: Do not include timeRange if not requested by the user"
    , "IMPORTANT: If timerange is empty or incomplete, do not include the timeRange field"
    , "IMPORTANT: Do not use code blocks or backticks in your response. Return the raw query directly."
    ]


-- * Agentic Configuration


data ToolLimits = ToolLimits
  { maxFieldValues :: Int
  , maxSampleLogs :: Int
  , maxServices :: Int
  , defaultFieldLimit :: Int
  , defaultSampleLimit :: Int
  , maxQueryResults :: Int
  , maxDisplayRows :: Int
  , maxBodyPreview :: Int
  }


defaultLimits :: ToolLimits
defaultLimits =
  ToolLimits
    { maxFieldValues = 20
    , maxSampleLogs = 5
    , maxServices = 20
    , defaultFieldLimit = 10
    , defaultSampleLimit = 3
    , maxQueryResults = 100
    , maxDisplayRows = 20
    , maxBodyPreview = 100
    }


data AgenticConfig = AgenticConfig
  { maxIterations :: Int
  , projectId :: Projects.ProjectId
  , timeRange :: (Maybe UTCTime, Maybe UTCTime)
  , facetContext :: Maybe FacetSummary
  , limits :: ToolLimits
  , customContext :: Maybe Text
  }


defaultAgenticConfig :: Projects.ProjectId -> AgenticConfig
defaultAgenticConfig pid =
  AgenticConfig
    { maxIterations = 5
    , projectId = pid
    , timeRange = (Nothing, Nothing)
    , facetContext = Nothing
    , limits = defaultLimits
    , customContext = Nothing
    }


-- * Helper functions


getIntArg :: Text -> Map.Map Text AE.Value -> Maybe Int
getIntArg key args = Map.lookup key args >>= \case AE.Number n -> Just (round n); _ -> Nothing


getTextArg :: Text -> Map.Map Text AE.Value -> Maybe Text
getTextArg key args = Map.lookup key args >>= \case AE.String s -> Just s; _ -> Nothing


getLimitArg :: Text -> Int -> Int -> Map.Map Text AE.Value -> Int
getLimitArg key maxVal defVal args = min maxVal $ fromMaybe defVal (getIntArg key args)


formatSummarizeResults :: V.Vector (V.Vector AE.Value) -> Text
formatSummarizeResults = T.intercalate ", " . V.toList . V.mapMaybe formatRow
  where
    formatRow (V.toList -> [v, c]) = Just $ "\"" <> jsonToText v <> "\" (" <> jsonToText c <> ")"
    formatRow _ = Nothing


formatSampleLogs :: Int -> V.Vector (V.Vector AE.Value) -> Text
formatSampleLogs maxBody = T.intercalate "\n" . V.toList . V.mapMaybe formatRow
  where
    formatRow (V.toList -> (lvl : nm : svc : body)) =
      Just $ "  - [" <> jsonToText lvl <> "] " <> jsonToText nm <> " (" <> jsonToText svc <> "): " <> T.take maxBody (T.intercalate " " $ map jsonToText body)
    formatRow _ = Nothing


formatQueryResults :: Int -> V.Vector (V.Vector AE.Value) -> Int -> Text
formatQueryResults maxRows results count =
  let formatted = T.intercalate "\n" $ take maxRows $ V.toList $ V.map formatRow results
      truncated = if count > maxRows then "\n... +" <> show (count - maxRows) <> " more" else ""
   in "Results (" <> show count <> " rows):\n" <> formatted <> truncated
  where
    formatRow row = "  " <> T.intercalate " | " (V.toList $ V.map jsonToText row)


jsonToText :: AE.Value -> Text
jsonToText = \case
  AE.String s -> s
  AE.Number n -> show n
  AE.Bool b -> bool "false" "true" b
  AE.Null -> "null"
  AE.Array arr -> "[" <> T.intercalate ", " (V.toList $ V.map jsonToText arr) <> "]"
  AE.Object _ -> "{...}"


formatFacetSummary :: FacetSummary -> Text
formatFacetSummary summary =
  let FacetData facetMap = summary.facetJson
      formatField (fieldName, values) =
        let columnName = T.replace "___" "." fieldName
            valueStrs = map (\(FacetValue v c) -> "\"" <> v <> "\" (" <> show c <> ")") $ take 10 values
         in columnName <> ": " <> T.intercalate ", " valueStrs
   in "Facet data:\n" <> T.intercalate "\n" (map formatField $ HM.toList facetMap)


keyFacetFields :: [Text]
keyFacetFields =
  [ "resource___service___name"
  , "level"
  , "status_code"
  , "attributes___http___response___status_code"
  , "attributes___http___request___method"
  , "attributes___error___type"
  , "kind"
  , "name"
  ]


formatFacetContext :: Maybe FacetSummary -> Text
formatFacetContext = \case
  Nothing -> ""
  Just summary ->
    let FacetData facetMap = summary.facetJson
        formatField fieldName =
          HM.lookup fieldName facetMap <&> \values ->
            let columnName = T.replace "___" "." fieldName
                valueStrs = map (\(FacetValue v _) -> "\"" <> v <> "\"") $ take 8 values
             in columnName <> ": " <> T.intercalate ", " valueStrs
        formattedFacets = mapMaybe formatField keyFacetFields
     in if null formattedFacets
          then ""
          else
            unlines
              [ ""
              , "PROJECT DATA CONTEXT (popular values for key fields):"
              , T.intercalate "\n" formattedFacets
              , ""
              ]


-- * OpenAI Tool Definitions


mkToolDef :: Text -> Text -> AE.Value -> OAITool.Tool
mkToolDef name desc params =
  OAITool.Tool_Function
    OAITool.Function
      { OAITool.description = Just desc
      , OAITool.name = name
      , OAITool.parameters = Just params
      , OAITool.strict = Just False
      }


mkSimpleTool :: Text -> Text -> OAITool.Tool
mkSimpleTool name desc = mkToolDef name desc $ AE.object ["type" AE..= ("object" :: Text), "properties" AE..= AE.object []]


mkProp :: Text -> Text -> AE.Value
mkProp typ desc = AE.object ["type" AE..= typ, "description" AE..= desc]


allToolDefs :: [OAITool.Tool]
allToolDefs =
  [ mkToolDef "get_field_values" "Get distinct values for a specific field"
      $ AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.object [("field", mkProp "string" "Field name (e.g., resource.service.name)"), ("limit", mkProp "integer" "Max values (default 10)")]
        , "required" AE..= (["field"] :: [Text])
        ]
  , mkSimpleTool "get_services" "Get list of services in this project"
  , mkToolDef "count_query" "Get count of results for a KQL query"
      $ AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.object [("query", mkProp "string" "KQL query to count")]
        , "required" AE..= (["query"] :: [Text])
        ]
  , mkToolDef "sample_logs" "Get sample log entries matching a query"
      $ AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.object [("query", mkProp "string" "KQL query to match"), ("limit", mkProp "integer" "Max samples (default 3, max 5)")]
        , "required" AE..= (["query"] :: [Text])
        ]
  , mkSimpleTool "get_facets" "Get precomputed facets for common fields like services, status codes, methods"
  , mkSimpleTool "get_schema" "Get schema of available fields in the log/span data"
  , mkToolDef "run_query" "Execute a KQL query and return results"
      $ AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.object [("query", mkProp "string" "KQL query to execute"), ("limit", mkProp "integer" "Max results (default 20)")]
        , "required" AE..= (["query"] :: [Text])
        ]
  , mkToolDef "run_sql_query" "Execute raw SQL on otel_logs_and_spans table. PREFER KQL queries (run_query) when possible - use SQL only for complex JOINs or aggregations not expressible in KQL"
      $ AE.object
        [ "type" AE..= ("object" :: Text)
        , "properties" AE..= AE.object [("query", mkProp "string" "SQL SELECT query (project_id filter auto-injected for security)"), ("limit", mkProp "integer" "Max results (default 20, max 100)")]
        , "required" AE..= (["query"] :: [Text])
        ]
  ]


-- * Main Entry Point


buildSystemPrompt :: AgenticConfig -> Text
buildSystemPrompt config =
  let basePrompt = systemPrompt
      facetSection = formatFacetContext config.facetContext
      customSection = fromMaybe "" config.customContext
   in basePrompt <> facetSection <> customSection


stripCodeBlock :: Text -> Text
stripCodeBlock t
  | "```json" `T.isPrefixOf` stripped = T.strip $ T.dropWhileEnd (== '`') $ T.drop 7 stripped
  | "```" `T.isPrefixOf` stripped = T.strip $ T.dropWhileEnd (== '`') $ T.drop 3 stripped
  | otherwise = stripped
  where
    stripped = T.strip t


parseResponse :: Text -> Either Text ChatLLMResponse
parseResponse responseText =
  let cleaned = stripCodeBlock responseText
   in getAskLLMResponse cleaned


runAgenticQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text ChatLLMResponse)
runAgenticQuery config userQuery apiKey = do
  let openAI = OpenAI.OpenAI{apiKey = apiKey, callbacks = [], baseUrl = Nothing}
      sysPrompt = buildSystemPrompt config
      systemMsg = LLM.Message LLM.System sysPrompt LLM.defaultMessageData
      userMsg = LLM.Message LLM.User userQuery LLM.defaultMessageData
      chatHistory = systemMsg :| [userMsg]
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model "gpt-4o-mini"
          , OpenAIV1.tools = Just $ V.fromList allToolDefs
          , OpenAIV1.messages = V.empty
          }
  runAgenticLoop config openAI chatHistory params 0


runAgenticLoop
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> OpenAI.OpenAI
  -> LLM.ChatHistory
  -> OpenAIV1.CreateChatCompletion
  -> Int
  -> Eff es (Either Text ChatLLMResponse)
runAgenticLoop config openAI chatHistory params iteration
  | iteration >= config.maxIterations = pure $ Left "Maximum tool iterations reached without final response"
  | otherwise = do
      result <- liftIO $ LLM.chat openAI chatHistory (Just params)
      case result of
        Left err -> pure $ Left $ "LLM Error: " <> show err
        Right responseMsg -> case LLM.toolCalls (LLM.messageData responseMsg) of
          Nothing -> pure $ parseResponse $ LLM.content responseMsg
          Just toolCallList -> do
            toolResults <- traverse (executeToolCall config) toolCallList
            let assistantMsg = responseMsg
                toolMsgs = zipWith mkToolResultMsg toolCallList toolResults
            newMessages <- liftIO $ addMessagesToMemory chatHistory (assistantMsg : toolMsgs)
            runAgenticLoop config openAI newMessages params (iteration + 1)


mkToolResultMsg :: LLM.ToolCall -> Text -> LLM.Message
mkToolResultMsg tc result =
  LLM.Message
    { LLM.role = LLM.Tool
    , LLM.content = result
    , LLM.messageData = LLM.defaultMessageData{LLM.name = Just $ LLM.toolCallId tc}
    }


addMessagesToMemory :: LLM.ChatHistory -> [LLM.Message] -> IO LLM.ChatHistory
addMessagesToMemory history newMsgs = do
  let allMsgs = maybe history (history <>) (NE.nonEmpty newMsgs)
      memory = TokenBufferMemory{maxTokens = 8000, tokenBufferMessages = allMsgs}
  result <- messages memory
  pure $ fromMaybe allMsgs (rightToMaybe result)


executeToolCall
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> LLM.ToolCall
  -> Eff es Text
executeToolCall config tc = do
  let funcName = LLM.toolFunctionName (LLM.toolCallFunction tc)
      args = LLM.toolFunctionArguments (LLM.toolCallFunction tc)
  case funcName of
    "get_field_values" -> executeGetFieldValues config args
    "get_services" -> executeGetServices config
    "count_query" -> executeCountQuery config args
    "sample_logs" -> executeSampleLogs config args
    "get_facets" -> pure $ executeGetFacets config
    "get_schema" -> pure $ Schema.generateSchemaForAI Schema.telemetrySchema
    "run_query" -> executeRunQuery config args
    "run_sql_query" -> executeSqlQuery config args
    _ -> pure $ "Unknown tool: " <> funcName


-- * Tool Execution


toolError :: Text -> Text -> Map.Map Text AE.Value -> Text
toolError tool msg args = "Error in " <> tool <> ": " <> msg <> " (received: " <> show (Map.keys args) <> ")"


executeGetFieldValues
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeGetFieldValues config args = case getTextArg "field" args of
  Just field -> do
    let lim = getLimitArg "limit" config.limits.maxFieldValues config.limits.defaultFieldLimit args
        kqlQuery = "| summarize count() by " <> field <> " | sort by _count desc | take " <> show lim
    runKqlAndFormat config kqlQuery [] $ \(results, _, _) ->
      "Values for '" <> field <> "': " <> formatSummarizeResults results
  _ -> pure $ toolError "get_field_values" "missing 'field'" args


executeGetServices :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> Eff es Text
executeGetServices config = do
  let kqlQuery = "| summarize count() by resource.service.name | sort by _count desc | take " <> show config.limits.maxServices
  runKqlAndFormat config kqlQuery [] $ \(results, _, _) ->
    "Available services: " <> formatSummarizeResults results


executeCountQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeCountQuery config args = case getTextArg "query" args of
  Just kqlQuery ->
    runKqlAndFormat config kqlQuery [] $ \(_, _, count) ->
      "Query '" <> kqlQuery <> "' matches " <> show count <> " entries"
  _ -> pure $ toolError "count_query" "missing 'query'" args


executeSampleLogs
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeSampleLogs config args = case getTextArg "query" args of
  Just kqlQuery -> do
    let lim = getLimitArg "limit" config.limits.maxSampleLogs config.limits.defaultSampleLimit args
        fullQuery = if "| take" `T.isInfixOf` kqlQuery then kqlQuery else kqlQuery <> " | take " <> show lim
        sampleColumns = ["level", "name", "resource.service.name", "body"]
    runKqlAndFormat config fullQuery sampleColumns $ \(results, _, _) ->
      "Sample logs:\n" <> formatSampleLogs config.limits.maxBodyPreview results
  _ -> pure $ toolError "sample_logs" "missing 'query'" args


executeGetFacets :: AgenticConfig -> Text
executeGetFacets config = maybe "No facet data available" formatFacetSummary config.facetContext


executeRunQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeRunQuery config args = case getTextArg "query" args of
  Just query -> do
    let lim = getLimitArg "limit" config.limits.maxQueryResults config.limits.maxDisplayRows args
        fullQuery = if "| take" `T.isInfixOf` query then query else query <> " | take " <> show lim
    runKqlAndFormat config fullQuery [] $ \(results, _, count) ->
      formatQueryResults config.limits.maxDisplayRows results count
  _ -> pure $ toolError "run_query" "missing 'query'" args


executeSqlQuery
  :: DB es
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeSqlQuery config args = case getTextArg "query" args of
  Just query -> do
    let lim = getLimitArg "limit" config.limits.maxQueryResults config.limits.maxDisplayRows args
    resultE <- executeSecuredQuery config.projectId query lim
    pure $ case resultE of
      Left err -> "SQL Error: " <> err <> "\nNote: KQL queries (run_query) are preferred when possible."
      Right results ->
        let count = V.length results
         in formatQueryResults config.limits.maxDisplayRows results count
  _ -> pure $ toolError "run_sql_query" "missing 'query'" args


runKqlAndFormat
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> [Text]
  -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> Text)
  -> Eff es Text
runKqlAndFormat config kqlQuery cols formatResult = case parseQueryToAST kqlQuery of
  Left parseErr -> pure $ "Error: Query parse failed - " <> show parseErr
  Right queryAST -> do
    resultE <- selectLogTable config.projectId queryAST kqlQuery Nothing config.timeRange cols Nothing Nothing
    pure $ either ("Error: Query execution failed - " <>) formatResult resultE
