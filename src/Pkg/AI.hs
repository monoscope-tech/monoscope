-- |
-- Module      : Pkg.AI
-- Description : Unified AI/LLM system for natural language to KQL query generation
--
-- This module provides LLM-powered query generation with tool calling.
-- The LLM decides when to use tools based on the query complexity.
module Pkg.AI (
  -- * Response Types
  LLMResponse (..),
  ToolCallInfo (..),
  AgenticChatResult (..),

  -- * Response Parsing
  parseLLMResponse,
  parseAgenticResponse,
  getNormalTupleResponse,

  -- * Basic LLM Calls
  callOpenAIAPI,
  callOpenAIAPIEff,

  -- * System Prompt
  systemPrompt,
  kqlGuide,
  outputFormatInstructions,

  -- * Agentic Configuration
  AgenticConfig (..),
  ToolLimits (..),

  -- * Agentic Query Execution
  agenticSetup,
  runAgenticQuery,
  runAgenticChatWithHistory,
  defaultAgenticConfig,
  defaultLimits,

  -- * Message Conversion
  dbMessageToLLMMessage,

  -- * Utilities
  stripCodeBlock,
) where

import Control.Lens ((^?))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.Lens (key, _Array, _Number, _String)
import Data.Aeson.Types (parseMaybe)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.LLM (callOpenAIAPI)
import Data.Effectful.LLM qualified as ELLM
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..))
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries (SecuredSql (..), SqlSource (..), executeSecuredQuery, selectLogTable)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import NeatInterpolation (text)
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Tool qualified as OAITool
import Pkg.Components.TimePicker (TimePicker)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser (parseQueryToAST)
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetSummary (..), FacetValue (..))
import Relude
import System.Tracing (Tracing)
import System.Types (DB)
import Utils (unwrapJsonPrimValue)


-- | Information about a tool call made during agentic execution
data ToolCallInfo = ToolCallInfo
  { name :: Text
  , args :: Map.Map Text AE.Value
  , resultPreview :: Text
  , rawData :: Maybe AE.Value -- Structured query results for widget data reuse
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ToolCallInfo


-- | Result of tool execution with optional raw data for widget reuse
data ToolResult = ToolResult {formatted :: Text, rawData :: Maybe AE.Value}
  deriving stock (Generic, Show)


-- | Result of an agentic chat with tool call history
data AgenticChatResult = AgenticChatResult
  { response :: Text
  , toolCalls :: [ToolCallInfo]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Unified LLM response type for all AI interactions
data LLMResponse = LLMResponse
  { explanation :: Maybe Text -- Markdown analysis/commentary
  , query :: Maybe Text -- Primary KQL query
  , visualization :: Maybe Text -- Chart type
  , widgets :: [Widget.Widget] -- Widget configs
  , timeRange :: Maybe TimePicker -- Time range (relative or absolute)
  , toolCalls :: Maybe [ToolCallInfo] -- Tool execution results (for widget data reuse)
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] LLMResponse


callOpenAIAPIEff :: ELLM.LLM :> es => Text -> Text -> Text -> Eff es (Either Text Text)
callOpenAIAPIEff = ELLM.callLLM


getNormalTupleResponse :: Text -> Either Text (Text, Maybe Text)
getNormalTupleResponse response =
  let lines' = lines $ T.strip response
      queryLine = fromMaybe "" (viaNonEmpty head lines')
      vizTypeM = viaNonEmpty head (drop 1 lines') >>= parseVisualizationType
      -- a fenced first line has no query content left once the fence is dropped
      cleanedQuery = if "```" `T.isPrefixOf` queryLine then "" else T.strip queryLine
   in if T.length cleanedQuery < 3 || any (`T.isInfixOf` cleanedQuery) ["Please provide a query", "I need more", "Could you please"]
        then Left "INVALID_QUERY_ERROR"
        else Right (cleanedQuery, vizTypeM)


-- | Parse raw LLM text into an 'LLMResponse' (@toolCalls@ always 'Nothing').
-- Tolerates markdown fences, trailing commas and partially invalid fields, and
-- falls back to an explanation-only response for non-JSON text.
parseLLMResponse :: Text -> Either Text LLMResponse
parseLLMResponse response =
  let cleaned = fixTrailingCommas $ stripCodeBlock response
      valM = rightToMaybe $ AE.eitherDecodeStrict' @AE.Value (encodeUtf8 cleaned)
      -- Partial decode: tolerates missing/invalid widgets and time_range fields
      partial =
        valM <&> \val ->
          LLMResponse
            { explanation = val ^? key "explanation" . _String
            , query = val ^? key "query" . _String
            , visualization = val ^? key "visualization" . _String
            , widgets = maybe [] (mapMaybe (parseMaybe AE.parseJSON) . V.toList) (val ^? key "widgets" . _Array)
            , timeRange = val ^? key "time_range" >>= parseMaybe AE.parseJSON
            , toolCalls = Nothing
            }
      -- Fallback: treat plain text as explanation-only response
      textFallback = LLMResponse{explanation = Just cleaned, query = Nothing, visualization = Nothing, widgets = [], timeRange = Nothing, toolCalls = Nothing}
   in Right $ fromMaybe (fromMaybe textFallback partial) (valM >>= parseMaybe AE.parseJSON)


-- | Like 'parseLLMResponse' but keeps the tool-call history: each call's @rawData@
-- carries already-executed query results, so widgets don't re-run expensive queries.
parseAgenticResponse :: AgenticChatResult -> Either Text LLMResponse
parseAgenticResponse (AgenticChatResult{response, toolCalls = tcs}) = do
  LLMResponse{explanation, query, visualization, widgets, timeRange} <- parseLLMResponse response
  pure LLMResponse{explanation, query, visualization, widgets, timeRange, toolCalls = Just tcs}


-- | Fix trailing commas in JSON (common LLM output issue)
fixTrailingCommas :: Text -> Text
fixTrailingCommas = T.replace ",\n}" "\n}" . T.replace ", }" "}" . T.replace ",}" "}" . T.replace ",\n]" "\n]" . T.replace ", ]" "]" . T.replace ",]" "]"


parseVisualizationType :: Text -> Maybe Text
parseVisualizationType = flip Map.lookup vizTypeMap
  where
    vizTypeMap =
      Map.fromList
        [ ("bar", "timeseries")
        , ("line", "timeseries_line")
        , ("timeseries", "timeseries")
        , ("timeseries_line", "timeseries_line")
        , ("bar chart", "timeseries")
        , ("line chart", "timeseries_line")
        , ("time series", "timeseries")
        , ("time series line", "timeseries_line")
        , ("distribution", "distribution")
        , ("pie_chart", "pie_chart")
        , ("pie", "pie_chart")
        , ("top_list", "top_list")
        , ("table", "table")
        , ("stat", "stat")
        , ("heatmap", "heatmap")
        ]


-- | KQL documentation for AI prompts - shared between Log Explorer and Anomalies
--
-- >>> Data.Text.isInfixOf "KQL" kqlGuide
-- True
-- >>> Data.Text.isInfixOf "summarize" kqlGuide
-- True
kqlGuide :: Text
kqlGuide =
  [text|
  <kql_reference>
  ## KQL (Kusto Query Language) Syntax

  ### Operators
  - Comparison: `==` `!=` `>` `<` `>=` `<=`
  - Set: `in` `!in` (e.g., `method in ("GET", "POST")`)
  - Text search: `has` `!has` (case-insensitive word search)
  - Text collections: `has_any` `has_all` (e.g., `tags has_any ["urgent", "critical"]`)
  - String: `contains` `!contains` `startswith` `!startswith` `endswith` `!endswith`
  - Regex: `matches` `=~` (e.g., `email matches /.*@company\.com/`)
  - Logical: `AND` `OR` (lowercase also accepted)
  - Duration literals: `100ms` `5s` `2m` `1h` (ns, µs, ms, s, m, h)

  ### Visualization Types (use these exact strings)
  - `timeseries`: Bar chart with time on X-axis. Requires `bin_auto(timestamp)` in query.
  - `timeseries_line`: Line chart with time. Requires `bin_auto(timestamp)` in query.
  - `distribution`: Categorical bar chart (no time). Use `summarize ... by` WITHOUT `bin()`.
  - `pie_chart`: Pie chart for proportions. Use `summarize ... by` WITHOUT `bin()`.
  - `top_list`: Ranked list of values.
  - `table`: Raw data rows as table.
  - `stat`: Single numeric value display.
  - `heatmap`: Latency distribution heatmap.
  - `logs`: Log entries list (default when no chart needed).

  ### Categorical vs Time-Series Charts
  - Time-series → GROUP BY uses `bin_auto(timestamp)` or `bin(timestamp, interval)` → `timeseries` / `timeseries_line`
  - Categorical → GROUP BY a non-time field, no `bin()` → `distribution` / `pie_chart`

  ### Aggregations
  `summarize` accepts `count()`, `sum(...)`, `avg(...)`, `min(...)`, `max(...)`, `median(...)`, etc.

  ### Time Binning Rules
  - DEFAULT: `bin_auto(timestamp)` — the system picks the bin size from the time range.
  - Only hardcode `bin(timestamp, <size>)` when the user EXPLICITLY names an interval (e.g. "by hour" → `bin(timestamp, 1h)`, "per 30 seconds" → `bin(timestamp, 30s)`).
  - For categorical grouping (by service, method, etc.) do NOT use `bin()` at all.

  <examples>
    <example>
      <input>show me errors</input>
      <query>level == "ERROR"</query>
      <visualization>logs</visualization>
    </example>
    <example>
      <input>show me error count over time</input>
      <query>level == "ERROR" | summarize count() by bin_auto(timestamp)</query>
      <visualization>timeseries</visualization>
    </example>
    <example>
      <input>show requests by service</input>
      <query>| summarize count() by resource.service.name</query>
      <visualization>distribution</visualization>
    </example>
    <example>
      <input>which services have the most errors?</input>
      <query>level == "ERROR" | summarize count() by resource.service.name</query>
      <visualization>distribution</visualization>
    </example>
  </examples>

  ### Critical Rules (must follow)
  1. ONLY use field names that appear in the schema. Never invent fields like `value`, `count`, or `total`. If a field is unknown, call `get_schema` or `get_field_values`.
  2. NEVER add timestamp filters in the KQL query (no `where timestamp >= datetime(...)`, no `where timestamp between ...`). Time filtering belongs in the JSON `time_range` field. When the user mentions a relative or absolute time range (e.g. "last 2 hours", "from 6pm to 7pm"), set `time_range` and leave the query free of timestamp predicates.
  </kql_reference>
  |]


-- | Shared output format instructions for all AI interactions
outputFormatInstructions :: Text
outputFormatInstructions =
  [text|
  <output_format>
  ## Response Schema
  Return a single JSON object with these optional fields:
  - `explanation` (string, markdown): your data-driven analysis.
  - `query` (string): KQL query.
  - `visualization` (string): one of `timeseries`, `timeseries_line`, `distribution`, `pie_chart`, `top_list`, `table`, `stat`, `heatmap`, `logs`.
  - `widgets` (array): widget configs of the form `{ "type": "...", "query": "...", "title": "..." }`.
  - `time_range` (object, snake_case keys):
    - Preferred: `{"since": "2H"}` for relative windows (`2H`, `30M`, `7D`, ...).
    - Absolute: `{"from": "<ISO8601>", "to": "<ISO8601>"}` (use real dates derived from the current time, not the placeholders shown here).
    - Use `since` when the user wants recent data; it replaces `from`/`to`.

  ## Workflow
  1. Chart / visualization requests:
     a. Call `run_query` first to fetch real data.
     b. Inspect the results for patterns, trends, spikes, anomalies.
     c. Return `query` + `visualization` + a data-driven `explanation`.
  2. Analysis-only requests: call tools as needed to retrieve data, then explain.
  3. Pure KQL translation (no chart): return just the `query`.

  ## Explanation Guidelines
  - Describe WHAT THE DATA SHOWS, not what the query does.
  - Highlight top contributors, unusual patterns, trends, notable values.
  - Quote specific numbers from the actual results.
  <examples>
    <example label="bad">This query will aggregate events over time...</example>
    <example label="good">Traffic peaked at 15:20 with 985 events. The 'monoscope' service accounts for 45% of total volume.</example>
  </examples>

  ## Response Skeleton (reference shape — do NOT include the surrounding fence in your output)
      {
        "explanation": "<Data-driven analysis in markdown>",
        "query": "<KQL query>",
        "visualization": "<one of the allowed strings>",
        "widgets": [{"type": "timeseries", "query": "...", "title": "..."}],
        "time_range": {"since": "2H"}
      }

  ## Critical Rules
  - Output raw JSON only — no code blocks, no backticks, no surrounding prose.
  - Only include fields that are relevant to the user's request.
  </output_format>
  |]


systemPrompt :: Text
systemPrompt =
  unlines
    [ "You are Monoscope's KQL assistant. Your job is to translate natural-language questions about telemetry (logs, traces, metrics) into correct KQL filter expressions and, when appropriate, into chart/visualization specs."
    , ""
    , "Maintain a precise, technical tone. Be concise — telemetry users are debugging and want answers, not prose."
    , ""
    , "## Telemetry Schema"
    , "The schema below is the complete and authoritative list of fields available. Do not invent fields."
    , ""
    , "<schema>"
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , "</schema>"
    , ""
    , kqlGuide
    , ""
    , outputFormatInstructions
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


-- | Default tool limits for agentic queries
--
-- >>> defaultLimits.maxFieldValues
-- 20
-- >>> defaultLimits.maxSampleLogs
-- 5
-- >>> defaultLimits.maxServices
-- 20
-- >>> defaultLimits.defaultFieldLimit
-- 10
-- >>> defaultLimits.defaultSampleLimit
-- 3
-- >>> defaultLimits.maxQueryResults
-- 100
-- >>> defaultLimits.maxDisplayRows
-- 20
-- >>> defaultLimits.maxBodyPreview
-- 100
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
  , timezone :: Maybe Text -- User's IANA timezone (e.g. "Europe/Berlin")
  , useTimefusion :: Bool -- Route raw-SQL reads to TimeFusion (mirrors env.enableTimefusionReads)
  }


-- | Default agentic configuration with reasonable defaults (maxIterations=5, facetContext=Nothing)
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
    , timezone = Nothing
    , useTimefusion = False
    }


-- * Helper functions


getTextArg :: Text -> Map.Map Text AE.Value -> Maybe Text
getTextArg k args = Map.lookup k args >>= (^? _String)


getLimitArg :: Text -> Int -> Int -> Map.Map Text AE.Value -> Int
getLimitArg k maxVal defVal args = min maxVal $ maybe defVal round $ Map.lookup k args >>= (^? _Number)


formatSummarizeResults :: V.Vector (V.Vector AE.Value) -> Text
formatSummarizeResults = T.intercalate ", " . V.toList . V.mapMaybe formatRow
  where
    formatRow row
      | V.length row == 2 = Just $ "\"" <> unwrapJsonPrimValue True (row V.! 0) <> "\" (" <> unwrapJsonPrimValue True (row V.! 1) <> ")"
      | otherwise = Nothing


formatSampleLogs :: Int -> V.Vector (V.Vector AE.Value) -> Text
formatSampleLogs maxBody = T.intercalate "\n" . V.toList . V.mapMaybe formatRow
  where
    formatRow row
      | V.length row >= 4 =
          let (lvl, nm, svc) = (row V.! 0, row V.! 1, row V.! 2)
              body = unwords $ V.toList $ V.map (unwrapJsonPrimValue True) $ V.drop 3 row
           in Just $ "  - [" <> unwrapJsonPrimValue True lvl <> "] " <> unwrapJsonPrimValue True nm <> " (" <> unwrapJsonPrimValue True svc <> "): " <> T.take maxBody body
      | otherwise = Nothing


formatQueryResults :: Int -> V.Vector (V.Vector AE.Value) -> Int -> Text
formatQueryResults maxRows results count =
  "Results ("
    <> show count
    <> " rows):\n"
    <> T.intercalate "\n" ["  " <> T.intercalate " | " (V.toList $ V.map (unwrapJsonPrimValue True) row) | row <- V.toList (V.take maxRows results)]
    <> memptyIfFalse (count > maxRows) ("\n... +" <> show (count - maxRows) <> " more")


-- | Render one facet field as "column: \"v1\" (n), \"v2\" (n)…", keeping at most @n@ values.
formatFacetField :: (FacetValue -> Text) -> Int -> Text -> [FacetValue] -> Text
formatFacetField render n fieldName values = T.replace "___" "." fieldName <> ": " <> T.intercalate ", " (map render $ take n values)


formatFacetSummary :: FacetSummary -> Text
formatFacetSummary summary =
  let FacetData facetMap = summary.facetJson
   in "Facet data:\n" <> T.intercalate "\n" [formatFacetField (\(FacetValue v c) -> "\"" <> v <> "\" (" <> show c <> ")") 10 f vs | (f, vs) <- HM.toList facetMap]


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
formatFacetContext = maybe "" \summary ->
  let FacetData facetMap = summary.facetJson
      formattedFacets = mapMaybe (\f -> formatFacetField (\(FacetValue v _) -> "\"" <> v <> "\"") 8 f <$> HM.lookup f facetMap) keyFacetFields
   in if null formattedFacets
        then ""
        else unlines ["", "PROJECT DATA CONTEXT (popular values for key fields):", T.intercalate "\n" formattedFacets, ""]


-- * OpenAI Tool Definitions


-- | Tool definition from (property name, JSON type, description) triples; the first
-- property is the required one (none required when the list is empty).
mkToolDef :: Text -> Text -> [(Text, Text, Text)] -> OAITool.Tool
mkToolDef name desc props =
  OAITool.Tool_Function
    OAITool.Function
      { OAITool.description = Just desc
      , OAITool.name = name
      , OAITool.parameters =
          Just
            $ AE.object
              ( [ "type" AE..= ("object" :: Text)
                , "properties" AE..= AE.object [AEK.fromText k AE..= AE.object ["type" AE..= t, "description" AE..= d] | (k, t, d) <- props]
                ]
                  <> ["required" AE..= map (\(k, _, _) -> k) (take 1 props) | not (null props)]
              )
      , OAITool.strict = Just False
      }


allToolDefs :: [OAITool.Tool]
allToolDefs =
  [ mkToolDef "get_field_values" "Get distinct values for a specific field" [("field", "string", "Field name (e.g., resource.service.name)"), ("limit", "integer", "Max values (default 10)")]
  , mkToolDef "get_services" "Get list of services in this project" []
  , mkToolDef "count_query" "Get count of results for a KQL query" [("query", "string", "KQL query to count")]
  , mkToolDef "sample_logs" "Get sample log entries matching a query" [("query", "string", "KQL query to match"), ("limit", "integer", "Max samples (default 3, max 5)")]
  , mkToolDef "get_facets" "Get precomputed facets for common fields like services, status codes, methods" []
  , mkToolDef "get_schema" "Get schema of available fields in the log/span data" []
  , mkToolDef "run_query" "Execute a KQL query and return results" [("query", "string", "KQL query to execute"), ("limit", "integer", "Max results (default 20)")]
  , mkToolDef "run_sql_query" "Execute raw SQL on otel_logs_and_spans table. PREFER KQL queries (run_query) when possible - use SQL only for complex JOINs or aggregations not expressible in KQL" [("query", "string", "SQL SELECT query (project_id filter auto-injected for security)"), ("limit", "integer", "Max results (default 20, max 100)")]
  ]


-- * Main Entry Point


buildSystemPrompt :: AgenticConfig -> UTCTime -> Text
buildSystemPrompt config now =
  let basePrompt = fromMaybe systemPrompt config.systemPromptOverride
      timezoneSection = "\nUSER TIMEZONE: " <> fromMaybe "UTC" config.timezone <> "\nCURRENT TIME (UTC): " <> show now <> "\n"
      facetSection = formatFacetContext config.facetContext
      customSection = fromMaybe "" config.customContext
   in basePrompt <> timezoneSection <> facetSection <> customSection


-- | Strip markdown code blocks from LLM responses
--
-- >>> stripCodeBlock "```json\n{\"key\": \"value\"}\n```"
-- "{\"key\": \"value\"}"
-- >>> stripCodeBlock "```\n{\"key\": \"value\"}\n```"
-- "{\"key\": \"value\"}"
-- >>> stripCodeBlock "{\"key\": \"value\"}"
-- "{\"key\": \"value\"}"
stripCodeBlock :: Text -> Text
stripCodeBlock t
  | "```json" `T.isPrefixOf` stripped = T.strip $ T.dropWhileEnd (== '`') $ T.drop 7 stripped
  | "```" `T.isPrefixOf` stripped = T.strip $ T.dropWhileEnd (== '`') $ T.drop 3 stripped
  | otherwise = stripped
  where
    stripped = T.strip t


runAgenticQuery :: (DB es, ELLM.LLM :> es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> Text -> Text -> Eff es (Either Text LLMResponse)
runAgenticQuery config userQuery model apiKey = do
  (systemMsg, userMsg, params) <- agenticSetup config userQuery model
  -- Text only: these consumers (MCP, bots, log explorer) never read toolCalls; the
  -- tool-data-reuse path is runAgenticChatWithHistory -> AgenticChatResult.
  (>>= parseLLMResponse . (.response)) <$> runAgenticLoopRaw config apiKey (systemMsg :| [userMsg]) params 0 []


-- | System + user messages and chat params shared by both agentic entry points
agenticSetup :: Time.Time :> es => AgenticConfig -> Text -> Text -> Eff es (LLM.Message, LLM.Message, OpenAIV1.CreateChatCompletion)
agenticSetup config userQuery model =
  Time.currentTime <&> \now ->
    ( LLM.Message LLM.System (buildSystemPrompt config now) LLM.defaultMessageData
    , LLM.Message LLM.User userQuery LLM.defaultMessageData
    , let (modelName, effort) = ELLM.modelAndEffort model
       in -- /v1/chat/completions rejects function tools with reasoning_effort other than "none"
          OpenAIV1._CreateChatCompletion{OpenAIV1.model = modelName, OpenAIV1.reasoning_effort = effort $> OpenAIV1.ReasoningEffort_None, OpenAIV1.tools = Just $ V.fromList allToolDefs, OpenAIV1.messages = V.empty}
    )


-- | Convert a DB chat message to an LLM message
dbMessageToLLMMessage :: Issues.AIChatMessage -> LLM.Message
dbMessageToLLMMessage msg =
  LLM.Message
    { LLM.role = case msg.role of
        Issues.ChatUser -> LLM.User
        Issues.ChatAssistant -> LLM.Assistant
        Issues.ChatSystem -> LLM.System
    , LLM.content = msg.content
    , LLM.messageData = LLM.defaultMessageData
    }


-- | Run agentic chat with DB-persisted history; returns the raw response plus tool call info
-- (unlike runAgenticQuery, which parses the response and drops tool metadata)
runAgenticChatWithHistory
  :: (DB es, ELLM.LLM :> es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Text
  -> Eff es (Either Text AgenticChatResult)
runAgenticChatWithHistory config userQuery model apiKey = do
  (systemMsg, userMsg, params) <- agenticSetup config userQuery model
  -- history is read before the new user message is persisted, so it isn't duplicated
  historyMsgs <-
    config.conversationId & maybe (pure []) \convId ->
      map dbMessageToLLMMessage <$> (Issues.selectChatHistory convId <* Issues.insertChatMessage config.projectId convId Issues.ChatUser userQuery Nothing Nothing)
  runAgenticLoopRaw config apiKey (systemMsg :| (historyMsgs <> [userMsg])) params 0 []


-- | Raw agentic loop that returns the response with tool call history
runAgenticLoopRaw :: (DB es, ELLM.LLM :> es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> LLM.ChatHistory -> OpenAIV1.CreateChatCompletion -> Int -> [ToolCallInfo] -> Eff es (Either Text AgenticChatResult)
runAgenticLoopRaw config apiKey chatHistory params iteration accumulated
  | iteration >= config.maxIterations = do
      Log.logTrace "AI agentic loop forcing final response" (AE.object ["iteration" AE..= iteration, "maxIterations" AE..= config.maxIterations])
      ELLM.callAgenticChat chatHistory params{OpenAIV1.tools = Nothing} apiKey >>= either handleError (finalResponse "AI final response")
  | otherwise = do
      let userQuery = maybe "" LLM.content $ viaNonEmpty last $ filter (\m -> LLM.role m == LLM.User) $ toList chatHistory
      Log.logTrace "AI agentic loop iteration" (AE.object ["iteration" AE..= iteration, "historySize" AE..= length chatHistory, "userQuery" AE..= userQuery])
      ELLM.callAgenticChat chatHistory params apiKey >>= either handleError \responseMsg ->
        maybe (finalResponse "AI final response (no tool calls)" responseMsg) (processToolCalls responseMsg) (LLM.toolCalls $ LLM.messageData responseMsg)
  where
    handleError err = Log.logAttention "LLM API error" (AE.object ["error" AE..= show @Text err]) $> Left "LLM service temporarily unavailable"

    finalResponse label responseMsg =
      Log.logTrace label (AE.object ["iteration" AE..= iteration, "response" AE..= LLM.content responseMsg, "responseLength" AE..= T.length (LLM.content responseMsg)])
        $> Right AgenticChatResult{response = LLM.content responseMsg, toolCalls = accumulated}

    processToolCalls responseMsg toolCallList = do
      Log.logTrace "AI requesting tool calls" (AE.object ["iteration" AE..= iteration, "tools" AE..= map (LLM.toolFunctionName . LLM.toolCallFunction) toolCallList])
      toolResults <- traverse (executeToolCall config) toolCallList
      let newToolInfos = zipWith mkToolCallInfo toolCallList toolResults
      Log.logTrace "AI tool calls completed" (AE.object ["iteration" AE..= iteration, "toolCount" AE..= length toolResults, "resultsPreview" AE..= map (\r -> T.take 200 r.formatted) toolResults])
      newMessages <- liftIO $ addMessagesToMemory config.limits.maxTokenBuffer chatHistory (responseMsg : zipWith mkToolResultMsg toolCallList toolResults)
      runAgenticLoopRaw config apiKey newMessages params (iteration + 1) (accumulated <> newToolInfos)


-- | Create ToolCallInfo from a tool call and its result
mkToolCallInfo :: LLM.ToolCall -> ToolResult -> ToolCallInfo
mkToolCallInfo tc result =
  ToolCallInfo
    { name = LLM.toolFunctionName (LLM.toolCallFunction tc)
    , args = LLM.toolFunctionArguments (LLM.toolCallFunction tc)
    , resultPreview = T.take 2000 result.formatted
    , rawData = result.rawData
    }


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
  result <- messages TokenBufferMemory{maxTokens, tokenBufferMessages = allMsgs}
  pure $ fromMaybe allMsgs (rightToMaybe result)


executeToolCall :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> LLM.ToolCall -> Eff es ToolResult
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


-- | Run the continuation on a required text argument, or report it missing
withArg :: Applicative f => Text -> Text -> Map.Map Text AE.Value -> (Text -> f Text) -> f Text
withArg tool k args f = maybe (pure $ toolError tool ("missing '" <> k <> "'") args) f (getTextArg k args)


-- | KQL tool whose raw results are never surfaced to the caller
runKqlText :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> [Text] -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> Text) -> Eff es Text
runKqlText config kqlQuery cols f = (.formatted) <$> runKqlWithRawData config kqlQuery cols ((,AE.Null) . f)


withTake :: Int -> Text -> Text
withTake lim q = if "| take" `T.isInfixOf` q then q else q <> " | take " <> show lim


executeGetFieldValues :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
executeGetFieldValues config args = withArg "get_field_values" "field" args \field ->
  runKqlText
    config
    ("| summarize count() by " <> field <> " | sort by count_ desc | take " <> show (getLimitArg "limit" config.limits.maxFieldValues config.limits.defaultFieldLimit args))
    []
    \(results, _, _) -> "Values for '" <> field <> "': " <> formatSummarizeResults results


executeGetServices :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Eff es Text
executeGetServices config =
  runKqlText
    config
    ("| summarize count() by resource.service.name | sort by count_ desc | take " <> show config.limits.maxServices)
    []
    \(results, _, _) -> "Available services: " <> formatSummarizeResults results


executeCountQuery :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
executeCountQuery config args = withArg "count_query" "query" args \kqlQuery ->
  runKqlText config kqlQuery [] \(_, _, count) -> "Query '" <> kqlQuery <> "' matches " <> show count <> " entries"


executeSampleLogs :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
executeSampleLogs config args = withArg "sample_logs" "query" args \kqlQuery ->
  runKqlText
    config
    (withTake (getLimitArg "limit" config.limits.maxSampleLogs config.limits.defaultSampleLimit args) kqlQuery)
    ["level", "name", "resource.service.name", "body"]
    \(results, _, _) -> "Sample logs:\n" <> formatSampleLogs config.limits.maxBodyPreview results


executeGetFacets :: AgenticConfig -> Text
executeGetFacets config = maybe "No facet data available" formatFacetSummary config.facetContext


runKqlWithRawData :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> [Text] -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> (Text, AE.Value)) -> Eff es ToolResult
runKqlWithRawData config kqlQuery cols formatResult = case parseQueryToAST kqlQuery of
  Left parseErr -> pure $ ToolResult ("Error: Query parse failed - " <> show parseErr) Nothing
  Right queryAST -> do
    resultE <- selectLogTable config.useTimefusion config.projectId queryAST kqlQuery Nothing config.timeRange cols Nothing Nothing Nothing
    pure $ case resultE of
      Left err -> ToolResult ("Error: Query execution failed - " <> err) Nothing
      Right res -> let (txt, raw) = formatResult res in ToolResult txt (Just raw)


executeRunQuery :: (DB es, Labeled "timefusion" Hasql :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es ToolResult
executeRunQuery config args = case getTextArg "query" args of
  Just query ->
    runKqlWithRawData config (withTake (getLimitArg "limit" config.limits.maxQueryResults config.limits.maxDisplayRows args) query) [] \(results, headers, count) ->
      ( formatQueryResults config.limits.maxDisplayRows results count
      , AE.object ["headers" AE..= headers, "data" AE..= results, "count" AE..= count]
      )
  _ -> pure $ ToolResult (toolError "run_query" "missing 'query'" args) Nothing


executeSqlQuery :: (DB es, Labeled "timefusion" Hasql :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
executeSqlQuery config args = withArg "run_sql_query" "query" args \query ->
  executeSecuredQuery config.useTimefusion config.projectId (SecuredSql SqlTimefusion query) (getLimitArg "limit" config.limits.maxQueryResults config.limits.maxDisplayRows args)
    <&> either
      (\err -> "SQL Error: " <> err <> "\nNote: KQL queries (run_query) are preferred when possible.")
      (\results -> formatQueryResults config.limits.maxDisplayRows results (V.length results))
