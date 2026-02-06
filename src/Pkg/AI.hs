{-# LANGUAGE GADTs #-}

-- |
-- Module      : Pkg.AI
-- Description : Unified AI/LLM system for natural language to KQL query generation
--
-- This module provides LLM-powered query generation with tool calling.
-- The LLM decides when to use tools based on the query complexity.
module Pkg.AI (
  -- * Response Types
  ChatLLMResponse (..),
  AIOutputType (..),
  ToolCallInfo (..),
  AgenticChatResult (..),

  -- * Response Parsing
  getAskLLMResponse,
  getNormalTupleReponse,

  -- * Basic LLM Calls
  callOpenAIAPI,
  callOpenAIAPIEff,

  -- * System Prompt
  systemPrompt,
  kqlGuide,

  -- * Agentic Configuration
  AgenticConfig (..),
  ToolLimits (..),

  -- * Agentic Query Execution
  runAgenticQuery,
  runAgenticQueryWithHistory,
  runAgenticChatWithHistory,
  defaultAgenticConfig,
  defaultLimits,

  -- * Message Conversion
  dbMessageToLLMMessage,

  -- * Utilities
  stripCodeBlock,
) where

import Data.Aeson qualified as AE
import Deriving.Aeson qualified as DAE
import Data.Effectful.LLM (callOpenAIAPI)
import Data.Effectful.LLM qualified as ELLM
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.LLM.OpenAI qualified as OpenAI
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..))
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps (executeSecuredQuery, selectLogTable)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
import OpenAI.V1.Tool qualified as OAITool
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser (parseQueryToAST)
import Relude
import System.Types (DB)


data AIOutputType = AOText | AOWidget | AOBoth
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "AO", DAE.CamelToSnake]] AIOutputType


-- | Information about a tool call made during agentic execution
data ToolCallInfo = ToolCallInfo
  { name :: Text
  , args :: Map.Map Text AE.Value
  , resultPreview :: Text
  , rawData :: Maybe AE.Value -- Structured query results for widget data reuse
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ToolCallInfo


-- | Result of tool execution with optional raw data for widget reuse
data ToolResult = ToolResult {formatted :: Text, rawData :: Maybe AE.Value}
  deriving stock (Generic, Show)


-- | Result of an agentic chat with tool call history
data AgenticChatResult = AgenticChatResult
  { response :: Text
  , toolCalls :: [ToolCallInfo]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, AE.FromJSON)


data ChatLLMResponse = ChatLLMResponse
  { query :: Maybe Text
  , outputType :: Maybe AIOutputType
  , visualization :: Maybe Text
  , commentary :: Maybe Text
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
      vizTypeM = viaNonEmpty head (drop 1 lines') >>= parseVisualizationType
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
  let cleaned = fixTrailingCommas response
      responseBS = encodeUtf8 cleaned
      decoded = AE.eitherDecode (fromStrict responseBS)
   in case decoded of
        Left err -> Left $ "JSON Decode Error: " <> toText err
        Right apiResponse -> do
          let vType = parseVisualizationType $ fromMaybe "" apiResponse.visualization
              -- Default outputType based on whether query/commentary are present
              defaultOutputType = case (apiResponse.query, apiResponse.commentary) of
                (Just _, Just _) -> Just AOBoth
                (Just _, Nothing) -> Just AOWidget
                (Nothing, Just _) -> Just AOText
                (Nothing, Nothing) -> Just AOWidget -- fallback
              apiResponse' = apiResponse{visualization = vType, outputType = apiResponse.outputType <|> defaultOutputType}
           in Right apiResponse'


-- | Fix trailing commas in JSON (common LLM output issue)
fixTrailingCommas :: Text -> Text
fixTrailingCommas = T.replace ",\n}" "\n}" . T.replace ", }" "}" . T.replace ",}" "}" . T.replace ",\n]" "\n]" . T.replace ", ]" "]" . T.replace ",]" "]"


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
  "distribution" -> Just "distribution"
  "pie_chart" -> Just "pie_chart"
  "pie" -> Just "pie_chart"
  "top_list" -> Just "top_list"
  "table" -> Just "table"
  "stat" -> Just "stat"
  "heatmap" -> Just "heatmap"
  _ -> Nothing


-- | KQL documentation for AI prompts - shared between Log Explorer and Anomalies
kqlGuide :: Text
kqlGuide =
  unlines
    [ "KQL (Kusto Query Language) SYNTAX:"
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
    , "VISUALIZATION TYPES (use these exact strings):"
    , "- \"timeseries\": Bar chart with time on X-axis. Use bin_auto(timestamp) in query."
    , "- \"timeseries_line\": Line chart with time. Use bin_auto(timestamp) in query."
    , "- \"distribution\": Categorical bar chart (no time). Use summarize...by without bin(). For GROUP BY on non-time fields."
    , "- \"pie_chart\": Pie chart for proportions. Use summarize...by without bin()."
    , "- \"top_list\": Ranked list of values."
    , "- \"table\": Raw data rows as table."
    , "- \"stat\": Single numeric value display."
    , "- \"heatmap\": Latency distribution heatmap."
    , "- \"logs\": Log entries list (default when no chart needed)."
    , ""
    , "CATEGORICAL vs TIME-SERIES CHARTS:"
    , "- Time-series: Use bin_auto(timestamp) or bin(timestamp, interval) in GROUP BY -> visualization: 'timeseries' or 'timeseries_line'"
    , "- Categorical: GROUP BY a field WITHOUT time binning -> visualization: 'distribution' or 'pie_chart'"
    , "- Example: 'show requests by service' -> | summarize count() by resource.service.name (no bin!) -> visualization: 'distribution'"
    , "- Example: 'show errors over time' -> | summarize count() by bin_auto(timestamp) -> visualization: 'timeseries'"
    , ""
    , "The summarize statement can use various aggregation functions like count(), sum(...), avg(...), min(...), max(...), median(...), etc."
    , ""
    , "TIME BINNING:"
    , "- Use bin_auto(timestamp) by DEFAULT - the system will automatically determine the appropriate bin size based on the time range"
    , "- Only use bin(timestamp, <size>) when the user EXPLICITLY specifies a time interval (e.g., 'by hour', 'per minute', 'in 5m intervals')"
    , "- Examples of when to use hardcoded bins: 'show errors by hour' -> bin(timestamp, 1h), 'count per 30 seconds' -> bin(timestamp, 30s)"
    , "- Examples of when to use bin_auto: 'show error trend', 'graph response times', 'chart requests over time' -> bin_auto(timestamp)"
    , "- IMPORTANT: For categorical grouping (by service, method, etc.), do NOT use bin() at all!"
    , ""
    , "KQL Query Examples:"
    , "- \"show me errors\" -> level == \"ERROR\" (visualization: logs)"
    , "- \"Show me error count over time\" -> query: level == \"ERROR\" | summarize count() by bin_auto(timestamp), visualization: timeseries"
    , "- \"show requests by service\" -> query: | summarize count() by resource.service.name, visualization: distribution"
    , "- \"which services have the most errors?\" -> query: level == \"ERROR\" | summarize count() by resource.service.name, visualization: distribution"
    , ""
    , "IMPORTANT: ONLY use field names from the schema. Do NOT invent or hallucinate field names like 'value', 'count', 'total', etc. If unsure about a field name, use get_schema or get_field_values tools to discover available fields."
    ]


systemPrompt :: Text
systemPrompt =
  unlines
    [ "You are a helpful assistant that converts natural language queries to KQL (Kusto Query Language) filter expressions."
    , ""
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , ""
    , kqlGuide
    , ""
    , "OUTPUT FORMAT:"
    , "Return a JSON object with these fields:"
    , "- \"outputType\": \"text\" | \"widget\" | \"both\" (required - use 'text' for AOText, 'widget' for AOWidget, 'both' for AOBoth)"
    , "- \"visualization\": widget type string (required if outputType includes widget)"
    , "- \"query\": KQL query string (required if outputType includes widget)"
    , "- \"commentary\": Your analysis/explanation (required if outputType includes text)"
    , "- \"timeRange\": [from, to] ISO8601 timestamps (optional)"
    , ""
    , "WORKFLOW:"
    , "1. If user wants a WIDGET ONLY (e.g., 'show errors over time'), return query directly with outputType='widget'"
    , "2. If user wants TEXT or ANALYSIS (e.g., 'what's my error rate?'), use available tools to get actual data first, then provide insights with outputType='text'"
    , "3. If user wants BOTH (e.g., 'analyze my traffic patterns'), provide widget + commentary with outputType='both'"
    , ""
    , "Time range rules:"
    , "DO NOT include timeRange if not requested by the user"
    , "DO NOT make up a time range if not specified by the user"
    , "DO NOT include timeRange field if the array is empty or incomplete"
    , "DO NOT use code blocks or backticks in your response. Return the raw query directly."
    , ""
    , "Response format:"
    , "Always return JSON in the following structure:"
    , "{"
    , "  \"outputType\": \"<text|widget|both>\","
    , "  \"query\": \"<KQL filter/query string>\","
    , "  \"visualization\": \"<logs|timeseries|timeseries_line|distribution|pie_chart|top_list|table|stat|heatmap>\","
    , "  \"commentary\": \"<Your analysis/explanation if outputType is text or both>\","
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
  , maxTokenBuffer :: Int
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
    , maxTokenBuffer = 8000
    }


data AgenticConfig = AgenticConfig
  { maxIterations :: Int
  , projectId :: Projects.ProjectId
  , timeRange :: (Maybe UTCTime, Maybe UTCTime)
  , facetContext :: Maybe FacetSummary
  , limits :: ToolLimits
  , customContext :: Maybe Text
  , conversationId :: Maybe (UUIDId "conversation")
  , conversationType :: Maybe Issues.ConversationType
  , systemPromptOverride :: Maybe Text -- Custom system prompt for specific use cases (e.g., issue investigation)
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
    , conversationId = Nothing
    , conversationType = Nothing
    , systemPromptOverride = Nothing
    }


-- * Helper functions


getIntArg :: Text -> Map.Map Text AE.Value -> Maybe Int
getIntArg key args = Map.lookup key args >>= \case AE.Number n -> Just (round n); _ -> Nothing


getTextArg :: Text -> Map.Map Text AE.Value -> Maybe Text
getTextArg key args = Map.lookup key args >>= \case AE.String s -> Just s; _ -> Nothing


getLimitArg :: Text -> Int -> Int -> Map.Map Text AE.Value -> Int
getLimitArg key maxVal defVal args = min maxVal $ fromMaybe defVal (getIntArg key args)


-- | Join Vector of Text with separator using fold (avoids intermediate list)
vIntercalate :: Text -> V.Vector Text -> Text
vIntercalate sep = V.ifoldl' (\acc i x -> if i == 0 then x else acc <> sep <> x) ""


formatSummarizeResults :: V.Vector (V.Vector AE.Value) -> Text
formatSummarizeResults = vIntercalate ", " . V.mapMaybe formatRow
  where
    formatRow row
      | V.length row == 2 = Just $ "\"" <> jsonToText (row V.! 0) <> "\" (" <> jsonToText (row V.! 1) <> ")"
      | otherwise = Nothing


formatSampleLogs :: Int -> V.Vector (V.Vector AE.Value) -> Text
formatSampleLogs maxBody = vIntercalate "\n" . V.mapMaybe formatRow
  where
    formatRow row
      | V.length row >= 4 =
          let (lvl, nm, svc) = (row V.! 0, row V.! 1, row V.! 2)
              body = vIntercalate " " $ V.map jsonToText $ V.drop 3 row
           in Just $ "  - [" <> jsonToText lvl <> "] " <> jsonToText nm <> " (" <> jsonToText svc <> "): " <> T.take maxBody body
      | otherwise = Nothing


formatQueryResults :: Int -> V.Vector (V.Vector AE.Value) -> Int -> Text
formatQueryResults maxRows results count =
  let formatted = vIntercalate "\n" $ V.map formatRow $ V.take maxRows results
      truncated = if count > maxRows then "\n... +" <> show (count - maxRows) <> " more" else ""
   in "Results (" <> show count <> " rows):\n" <> formatted <> truncated
  where
    formatRow row = "  " <> vIntercalate " | " (V.map jsonToText row)


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
  let basePrompt = fromMaybe systemPrompt config.systemPromptOverride
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


-- | Convert a DB chat message to an LLM message
dbMessageToLLMMessage :: Issues.AIChatMessage -> LLM.Message
dbMessageToLLMMessage msg =
  LLM.Message
    { LLM.role = case msg.role of
        "user" -> LLM.User
        "assistant" -> LLM.Assistant
        "system" -> LLM.System
        _ -> LLM.User
    , LLM.content = msg.content
    , LLM.messageData = LLM.defaultMessageData
    }


-- | Run agentic query with DB-persisted chat history
runAgenticQueryWithHistory
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text ChatLLMResponse)
runAgenticQueryWithHistory config userQuery apiKey = do
  let openAI = OpenAI.OpenAI{apiKey = apiKey, callbacks = [], baseUrl = Nothing}
      sysPrompt = buildSystemPrompt config
      systemMsg = LLM.Message LLM.System sysPrompt LLM.defaultMessageData
      userMsg = LLM.Message LLM.User userQuery LLM.defaultMessageData
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model "gpt-4o-mini"
          , OpenAIV1.tools = Just $ V.fromList allToolDefs
          , OpenAIV1.messages = V.empty
          }
  case config.conversationId of
    Nothing -> runAgenticLoop config openAI (systemMsg :| [userMsg]) params 0
    Just convId -> do
      -- Load existing chat history from DB
      dbMessages <- Issues.selectChatHistory convId
      let historyMsgs = map dbMessageToLLMMessage dbMessages
          chatHistory = systemMsg :| (historyMsgs <> [userMsg])
      -- Save user message to DB
      Issues.insertChatMessage config.projectId convId "user" userQuery Nothing Nothing
      -- Run the agentic loop
      result <- runAgenticLoop config openAI chatHistory params 0
      -- Save assistant response to DB
      case result of
        Right resp -> whenJust resp.query \q -> Issues.insertChatMessage config.projectId convId "assistant" q Nothing Nothing
        Left _ -> pass
      pure result


-- | Run agentic chat with DB-persisted history, returns response with tool call info
-- This is more flexible than runAgenticQueryWithHistory as it doesn't parse the response
runAgenticChatWithHistory
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text AgenticChatResult)
runAgenticChatWithHistory config userQuery apiKey = do
  let openAI = OpenAI.OpenAI{apiKey = apiKey, callbacks = [], baseUrl = Nothing}
      sysPrompt = buildSystemPrompt config
      systemMsg = LLM.Message LLM.System sysPrompt LLM.defaultMessageData
      userMsg = LLM.Message LLM.User userQuery LLM.defaultMessageData
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model "gpt-4o-mini"
          , OpenAIV1.tools = Just $ V.fromList allToolDefs
          , OpenAIV1.messages = V.empty
          }
  case config.conversationId of
    Nothing -> runAgenticLoopRaw config openAI (systemMsg :| [userMsg]) params 0 []
    Just convId -> do
      dbMessages <- Issues.selectChatHistory convId
      let historyMsgs = map dbMessageToLLMMessage dbMessages
          chatHistory = systemMsg :| (historyMsgs <> [userMsg])
      Issues.insertChatMessage config.projectId convId "user" userQuery Nothing Nothing
      runAgenticLoopRaw config openAI chatHistory params 0 []


-- | Raw agentic loop that returns the response with tool call history
runAgenticLoopRaw
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> OpenAI.OpenAI
  -> LLM.ChatHistory
  -> OpenAIV1.CreateChatCompletion
  -> Int
  -> [ToolCallInfo]
  -> Eff es (Either Text AgenticChatResult)
runAgenticLoopRaw config openAI chatHistory params iteration accumulated
  | iteration >= config.maxIterations = do
      Log.logTrace "AI agentic loop forcing final response" (AE.object ["iteration" AE..= iteration, "maxIterations" AE..= config.maxIterations])
      let paramsNoTools = params{OpenAIV1.tools = Nothing}
      result <- liftIO $ LLM.chat openAI chatHistory (Just paramsNoTools)
      case result of
        Left err -> do
          Log.logAttention "LLM API error in final iteration" (AE.object ["error" AE..= show @Text err])
          pure $ Left "LLM service temporarily unavailable"
        Right responseMsg -> do
          Log.logTrace "AI final response received" (AE.object ["responseLength" AE..= T.length (LLM.content responseMsg)])
          pure $ Right $ AgenticChatResult{response = LLM.content responseMsg, toolCalls = accumulated}
  | otherwise = do
      Log.logTrace "AI agentic loop iteration" (AE.object ["iteration" AE..= iteration, "historySize" AE..= length chatHistory])
      result <- liftIO $ LLM.chat openAI chatHistory (Just params)
      case result of
        Left err -> do
          Log.logAttention "LLM API error in runAgenticLoopRaw" (AE.object ["error" AE..= show @Text err])
          pure $ Left "LLM service temporarily unavailable"
        Right responseMsg -> case LLM.toolCalls (LLM.messageData responseMsg) of
          Nothing -> do
            Log.logTrace "AI response received (no tool calls)" (AE.object ["iteration" AE..= iteration, "responseLength" AE..= T.length (LLM.content responseMsg)])
            pure $ Right $ AgenticChatResult{response = LLM.content responseMsg, toolCalls = accumulated}
          Just toolCallList -> do
            let toolNames = map (LLM.toolFunctionName . LLM.toolCallFunction) toolCallList
            Log.logTrace "AI requesting tool calls" (AE.object ["iteration" AE..= iteration, "tools" AE..= toolNames])
            toolResults <- traverse (executeToolCall config) toolCallList
            let newToolInfos = zipWith mkToolCallInfo toolCallList toolResults
                accumulatedCalls = accumulated <> newToolInfos
                assistantMsg = responseMsg
                toolMsgs = zipWith mkToolResultMsg toolCallList toolResults
            newMessages <- liftIO $ addMessagesToMemory config.limits.maxTokenBuffer chatHistory (assistantMsg : toolMsgs)
            runAgenticLoopRaw config openAI newMessages params (iteration + 1) accumulatedCalls


-- | Create ToolCallInfo from a tool call and its result
mkToolCallInfo :: LLM.ToolCall -> ToolResult -> ToolCallInfo
mkToolCallInfo tc result =
  ToolCallInfo
    { name = LLM.toolFunctionName (LLM.toolCallFunction tc)
    , args = LLM.toolFunctionArguments (LLM.toolCallFunction tc)
    , resultPreview = T.take 2000 result.formatted
    , rawData = result.rawData
    }


runAgenticLoop
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> OpenAI.OpenAI
  -> LLM.ChatHistory
  -> OpenAIV1.CreateChatCompletion
  -> Int
  -> Eff es (Either Text ChatLLMResponse)
runAgenticLoop config openAI chatHistory params iteration
  | iteration >= config.maxIterations = do
      Log.logTrace "AI agentic loop forcing final response" (AE.object ["iteration" AE..= iteration, "maxIterations" AE..= config.maxIterations])
      let paramsNoTools = params{OpenAIV1.tools = Nothing}
      result <- liftIO $ LLM.chat openAI chatHistory (Just paramsNoTools)
      case result of
        Left err -> do
          Log.logAttention "LLM API error in final iteration" (AE.object ["error" AE..= show @Text err])
          pure $ Left "LLM service temporarily unavailable"
        Right responseMsg -> do
          Log.logTrace "AI final response received" (AE.object ["responseLength" AE..= T.length (LLM.content responseMsg)])
          pure $ parseResponse $ LLM.content responseMsg
  | otherwise = do
      Log.logTrace "AI agentic loop iteration" (AE.object ["iteration" AE..= iteration, "historySize" AE..= length chatHistory])
      result <- liftIO $ LLM.chat openAI chatHistory (Just params)
      case result of
        Left err -> do
          Log.logAttention "LLM API error in runAgenticLoop" (AE.object ["error" AE..= show @Text err])
          pure $ Left "LLM service temporarily unavailable"
        Right responseMsg -> case LLM.toolCalls (LLM.messageData responseMsg) of
          Nothing -> do
            Log.logTrace "AI response received (no tool calls)" (AE.object ["iteration" AE..= iteration, "responseLength" AE..= T.length (LLM.content responseMsg)])
            pure $ parseResponse $ LLM.content responseMsg
          Just toolCallList -> do
            let toolNames = map (LLM.toolFunctionName . LLM.toolCallFunction) toolCallList
            Log.logTrace "AI requesting tool calls" (AE.object ["iteration" AE..= iteration, "tools" AE..= toolNames])
            toolResults <- traverse (executeToolCall config) toolCallList
            let assistantMsg = responseMsg
                toolMsgs = zipWith mkToolResultMsg toolCallList toolResults
            newMessages <- liftIO $ addMessagesToMemory config.limits.maxTokenBuffer chatHistory (assistantMsg : toolMsgs)
            runAgenticLoop config openAI newMessages params (iteration + 1)


mkToolResultMsg :: LLM.ToolCall -> ToolResult -> LLM.Message
mkToolResultMsg tc result =
  LLM.Message
    { LLM.role = LLM.Tool
    , LLM.content = result.formatted
    , LLM.messageData = LLM.defaultMessageData{LLM.toolCalls = Just [LLM.ToolCall{LLM.toolCallId = LLM.toolCallId tc, LLM.toolCallType = "function", LLM.toolCallFunction = LLM.ToolFunction "" mempty}]}
    }


addMessagesToMemory :: Int -> LLM.ChatHistory -> [LLM.Message] -> IO LLM.ChatHistory
addMessagesToMemory maxTokens history newMsgs = do
  let allMsgs = maybe history (history <>) (nonEmpty newMsgs)
      memory = TokenBufferMemory{maxTokens, tokenBufferMessages = allMsgs}
  result <- messages memory
  pure $ fromMaybe allMsgs (rightToMaybe result)


executeToolCall
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> LLM.ToolCall
  -> Eff es ToolResult
executeToolCall config tc = do
  let funcName = LLM.toolFunctionName (LLM.toolCallFunction tc)
      args = LLM.toolFunctionArguments (LLM.toolCallFunction tc)
      noRaw t = ToolResult t Nothing
  Log.logTrace "AI executing tool" (AE.object ["tool" AE..= funcName, "args" AE..= args])
  result <- case funcName of
    "get_field_values" -> noRaw <$> executeGetFieldValues config args
    "get_services" -> noRaw <$> executeGetServices config
    "count_query" -> noRaw <$> executeCountQuery config args
    "sample_logs" -> noRaw <$> executeSampleLogs config args
    "get_facets" -> pure $ noRaw $ executeGetFacets config
    "get_schema" -> pure $ noRaw $ Schema.generateSchemaForAI Schema.telemetrySchema
    "run_query" -> executeRunQuery config args
    "run_sql_query" -> noRaw <$> executeSqlQuery config args
    _ -> pure $ noRaw $ "Unknown tool: " <> funcName
  Log.logTrace "AI tool result" (AE.object ["tool" AE..= funcName, "resultLength" AE..= T.length result.formatted, "resultPreview" AE..= T.take 200 result.formatted])
  pure result


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
        kqlQuery = "| summarize count() by " <> field <> " | sort by count_ desc | take " <> show lim
    runKqlAndFormat config kqlQuery [] $ \(results, _, _) ->
      "Values for '" <> field <> "': " <> formatSummarizeResults results
  _ -> pure $ toolError "get_field_values" "missing 'field'" args


executeGetServices :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> Eff es Text
executeGetServices config = do
  let kqlQuery = "| summarize count() by resource.service.name | sort by count_ desc | take " <> show config.limits.maxServices
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


runKqlWithRawData
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> [Text]
  -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> (Text, AE.Value))
  -> Eff es ToolResult
runKqlWithRawData config kqlQuery cols formatResult = case parseQueryToAST kqlQuery of
  Left parseErr -> pure $ ToolResult ("Error: Query parse failed - " <> show parseErr) Nothing
  Right queryAST -> do
    resultE <- selectLogTable config.projectId queryAST kqlQuery Nothing config.timeRange cols Nothing Nothing
    pure $ case resultE of
      Left err -> ToolResult ("Error: Query execution failed - " <> err) Nothing
      Right res -> let (txt, raw) = formatResult res in ToolResult txt (Just raw)


executeRunQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es ToolResult
executeRunQuery config args = case getTextArg "query" args of
  Just query -> do
    let lim = getLimitArg "limit" config.limits.maxQueryResults config.limits.maxDisplayRows args
        fullQuery = if "| take" `T.isInfixOf` query then query else query <> " | take " <> show lim
    runKqlWithRawData config fullQuery [] $ \(results, headers, count) ->
      ( formatQueryResults config.limits.maxDisplayRows results count
      , AE.object ["headers" AE..= headers, "data" AE..= results, "count" AE..= count]
      )
  _ -> pure $ ToolResult (toolError "run_query" "missing 'query'" args) Nothing


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
