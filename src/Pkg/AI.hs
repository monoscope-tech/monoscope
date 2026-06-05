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
  getNormalTupleReponse,

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
import Data.Aeson.Lens (key, _Array, _String)
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
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time qualified as Time
import Langchain.LLM.Core qualified as LLM
import Langchain.Memory.Core (BaseMemory (..))
import Langchain.Memory.TokenBufferMemory (TokenBufferMemory (..))
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries (executeSecuredQuery, selectLogTable)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import NeatInterpolation (text)
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
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


-- | Parse LLM response from plain text (no tool calls)
--
-- Converts raw LLM JSON text into structured LLMResponse. Sets toolCalls to Nothing.
--
-- Use this when:
-- - Parsing stored chat messages from database
-- - Testing with mock responses
-- - Processing non-agentic LLM calls (no tools available)
--
-- The function handles common LLM output issues:
-- - Strips markdown code blocks (```json ... ```)
-- - Fixes trailing commas in JSON
parseLLMResponse :: Text -> Either Text LLMResponse
parseLLMResponse response =
  let cleaned = fixTrailingCommas $ stripCodeBlock response
      responseBS = encodeUtf8 cleaned
      -- Try decoding as full JSON first
      fullDecode = first toText $ AE.eitherDecode (fromStrict responseBS)
      -- If that fails, try partial decode (handle missing widgets field)
      partialDecode = case AE.eitherDecode @AE.Value (fromStrict responseBS) of
        Left _ -> Left "Not valid JSON"
        Right val ->
          let widgetList = maybe [] (V.toList >=> \v -> case AE.fromJSON @Widget.Widget v of AE.Success w -> [w]; _ -> []) (val ^? key "widgets" . _Array)
              timePicker = val ^? key "time_range" >>= \v -> case AE.fromJSON v of AE.Success tp -> Just tp; _ -> Nothing
           in Right
                LLMResponse
                  { explanation = val ^? key "explanation" . _String
                  , query = val ^? key "query" . _String
                  , visualization = val ^? key "visualization" . _String
                  , widgets = widgetList
                  , timeRange = timePicker
                  , toolCalls = Nothing
                  }
      -- Fallback: treat plain text as explanation-only response
      textFallback = Right LLMResponse{explanation = Just cleaned, query = Nothing, visualization = Nothing, widgets = [], timeRange = Nothing, toolCalls = Nothing}
   in case fullDecode of
        Right r -> Right r
        Left _ -> case partialDecode of
          Right r -> Right r
          Left _ -> textFallback


-- | Parse agentic execution result (preserves tool call metadata)
--
-- Wraps parseLLMResponse but PRESERVES tool execution history from agentic chat.
-- This is critical for widget data reuse - tool calls contain cached query results
-- in their rawData field, avoiding re-execution of expensive queries.
--
-- Use this when:
-- - Processing live agentic chat results (runAgenticChatWithHistory)
-- - Need tool call debugging info
-- - Widgets should reuse tool-executed query data
--
-- Flow: AgenticChatResult (response + tool calls) → parse text → inject tool metadata → LLMResponse
parseAgenticResponse :: AgenticChatResult -> Either Text LLMResponse
parseAgenticResponse (AgenticChatResult{response, toolCalls = tcs}) = do
  LLMResponse{explanation, query, visualization, widgets, timeRange} <- parseLLMResponse response
  pure LLMResponse{explanation, query, visualization, widgets, timeRange, toolCalls = Just tcs}


-- | Fix trailing commas in JSON (common LLM output issue)
fixTrailingCommas :: Text -> Text
fixTrailingCommas = T.replace ",\n}" "\n}" . T.replace ", }" "}" . T.replace ",}" "}" . T.replace ",\n]" "\n]" . T.replace ", ]" "]" . T.replace ",]" "]"


vizTypeMap :: Map.Map Text Text
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
{-# NOINLINE vizTypeMap #-}


parseVisualizationType :: Text -> Maybe Text
parseVisualizationType = flip Map.lookup vizTypeMap


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
    }


-- * Helper functions


getIntArg :: Text -> Map.Map Text AE.Value -> Maybe Int
getIntArg k args = Map.lookup k args >>= \case AE.Number n -> Just (round n); _ -> Nothing


getTextArg :: Text -> Map.Map Text AE.Value -> Maybe Text
getTextArg k args = Map.lookup k args >>= \case AE.String s -> Just s; _ -> Nothing


getLimitArg :: Text -> Int -> Int -> Map.Map Text AE.Value -> Int
getLimitArg k maxVal defVal args = min maxVal $ fromMaybe defVal (getIntArg k args)


-- | Join Vector of Text with separator using fold (avoids intermediate list)
vIntercalate :: Text -> V.Vector Text -> Text
vIntercalate sep = V.ifoldl' (\acc i x -> if i == 0 then x else acc <> sep <> x) ""


formatSummarizeResults :: V.Vector (V.Vector AE.Value) -> Text
formatSummarizeResults = vIntercalate ", " . V.mapMaybe formatRow
  where
    formatRow :: V.Vector AE.Value -> Maybe Text
    formatRow row
      | V.length row == 2 = Just $ "\"" <> unwrapJsonPrimValue True (row V.! 0) <> "\" (" <> unwrapJsonPrimValue True (row V.! 1) <> ")"
      | otherwise = Nothing


formatSampleLogs :: Int -> V.Vector (V.Vector AE.Value) -> Text
formatSampleLogs maxBody = vIntercalate "\n" . V.mapMaybe formatRow
  where
    formatRow :: V.Vector AE.Value -> Maybe Text
    formatRow row
      | V.length row >= 4 =
          let (lvl, nm, svc) = (row V.! 0, row V.! 1, row V.! 2)
              body = vIntercalate " " $ V.map (unwrapJsonPrimValue True) $ V.drop 3 row
           in Just $ "  - [" <> unwrapJsonPrimValue True lvl <> "] " <> unwrapJsonPrimValue True nm <> " (" <> unwrapJsonPrimValue True svc <> "): " <> T.take maxBody body
      | otherwise = Nothing


formatQueryResults :: Int -> V.Vector (V.Vector AE.Value) -> Int -> Text
formatQueryResults maxRows results count =
  let formatted = vIntercalate "\n" $ V.map formatRow $ V.take maxRows results
      truncated = if count > maxRows then "\n... +" <> show (count - maxRows) <> " more" else ""
   in "Results (" <> show count <> " rows):\n" <> formatted <> truncated
  where
    formatRow :: V.Vector AE.Value -> Text
    formatRow row = "  " <> vIntercalate " | " (V.map (unwrapJsonPrimValue True) row)


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


parseResponse :: Text -> Either Text LLMResponse
parseResponse = parseLLMResponse . stripCodeBlock


runAgenticQuery :: (DB es, ELLM.LLM :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> Text -> Text -> Eff es (Either Text LLMResponse)
runAgenticQuery config userQuery model apiKey = do
  now <- Time.currentTime
  let sysPrompt = buildSystemPrompt config now
      systemMsg = LLM.Message LLM.System sysPrompt LLM.defaultMessageData
      userMsg = LLM.Message LLM.User userQuery LLM.defaultMessageData
      chatHistory = systemMsg :| [userMsg]
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model model
          , OpenAIV1.tools = Just $ V.fromList allToolDefs
          , OpenAIV1.messages = V.empty
          }
  runAgenticLoop config apiKey chatHistory params 0


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


-- | Run agentic chat with DB-persisted history, returns response with tool call info
-- This is more flexible than runAgenticQueryWithHistory as it doesn't parse the response
runAgenticChatWithHistory
  :: (DB es, ELLM.LLM :> es, Log :> es, Time.Time :> es, Tracing :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Text
  -> Eff es (Either Text AgenticChatResult)
runAgenticChatWithHistory config userQuery model apiKey = do
  now <- Time.currentTime
  let sysPrompt = buildSystemPrompt config now
      systemMsg = LLM.Message LLM.System sysPrompt LLM.defaultMessageData
      userMsg = LLM.Message LLM.User userQuery LLM.defaultMessageData
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model model
          , OpenAIV1.tools = Just $ V.fromList allToolDefs
          , OpenAIV1.messages = V.empty
          }
  case config.conversationId of
    Nothing -> runAgenticLoopRaw config apiKey (systemMsg :| [userMsg]) params 0 []
    Just convId -> do
      dbMessages <- Issues.selectChatHistory convId
      let historyMsgs = map dbMessageToLLMMessage dbMessages
          chatHistory = systemMsg :| (historyMsgs <> [userMsg])
      Issues.insertChatMessage config.projectId convId "user" userQuery Nothing Nothing
      runAgenticLoopRaw config apiKey chatHistory params 0 []


-- | Raw agentic loop that returns the response with tool call history
runAgenticLoopRaw :: (DB es, ELLM.LLM :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> LLM.ChatHistory -> OpenAIV1.CreateChatCompletion -> Int -> [ToolCallInfo] -> Eff es (Either Text AgenticChatResult)
runAgenticLoopRaw config apiKey chatHistory params iteration accumulated
  | iteration >= config.maxIterations = do
      Log.logTrace "AI agentic loop forcing final response" (AE.object ["iteration" AE..= iteration, "maxIterations" AE..= config.maxIterations])
      ELLM.callAgenticChat chatHistory params{OpenAIV1.tools = Nothing} apiKey >>= either handleError \responseMsg -> do
        Log.logTrace "AI final response" (AE.object ["iteration" AE..= iteration, "response" AE..= LLM.content responseMsg, "responseLength" AE..= T.length (LLM.content responseMsg)])
        pure $ Right AgenticChatResult{response = LLM.content responseMsg, toolCalls = accumulated}
  | otherwise = do
      let userQuery = maybe "" LLM.content $ viaNonEmpty last $ filter (\m -> LLM.role m == LLM.User) $ toList chatHistory
      Log.logTrace "AI agentic loop iteration" (AE.object ["iteration" AE..= iteration, "historySize" AE..= length chatHistory, "userQuery" AE..= userQuery])
      ELLM.callAgenticChat chatHistory params apiKey >>= either handleError \responseMsg ->
        maybe (logFinalResponse responseMsg) (processToolCalls responseMsg) (LLM.toolCalls $ LLM.messageData responseMsg)
  where
    handleError err = Log.logAttention "LLM API error" (AE.object ["error" AE..= show @Text err]) $> Left "LLM service temporarily unavailable"

    logFinalResponse responseMsg = do
      Log.logTrace "AI final response (no tool calls)" (AE.object ["iteration" AE..= iteration, "response" AE..= LLM.content responseMsg, "responseLength" AE..= T.length (LLM.content responseMsg)])
      pure $ Right AgenticChatResult{response = LLM.content responseMsg, toolCalls = accumulated}

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


runAgenticLoop :: (DB es, ELLM.LLM :> es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> LLM.ChatHistory -> OpenAIV1.CreateChatCompletion -> Int -> Eff es (Either Text LLMResponse)
runAgenticLoop config apiKey chatHistory params iteration
  | iteration >= config.maxIterations = do
      Log.logTrace "AI agentic loop forcing final response" (AE.object ["iteration" AE..= iteration, "maxIterations" AE..= config.maxIterations])
      ELLM.callAgenticChat chatHistory params{OpenAIV1.tools = Nothing} apiKey >>= either handleError \responseMsg -> do
        Log.logTrace "AI final response" (AE.object ["iteration" AE..= iteration, "response" AE..= LLM.content responseMsg, "responseLength" AE..= T.length (LLM.content responseMsg)])
        pure $ parseResponse $ LLM.content responseMsg
  | otherwise = do
      let userQuery = maybe "" LLM.content $ viaNonEmpty last $ filter (\m -> LLM.role m == LLM.User) $ toList chatHistory
      Log.logTrace "AI agentic loop iteration" (AE.object ["iteration" AE..= iteration, "historySize" AE..= length chatHistory, "userQuery" AE..= userQuery])
      ELLM.callAgenticChat chatHistory params apiKey >>= either handleError \responseMsg ->
        maybe (logFinalResponse responseMsg) (processToolCalls responseMsg) (LLM.toolCalls $ LLM.messageData responseMsg)
  where
    handleError err = Log.logAttention "LLM API error" (AE.object ["error" AE..= show @Text err]) $> Left "LLM service temporarily unavailable"

    logFinalResponse responseMsg = do
      Log.logTrace "AI final response (no tool calls)" (AE.object ["iteration" AE..= iteration, "response" AE..= LLM.content responseMsg, "responseLength" AE..= T.length (LLM.content responseMsg)])
      pure $ parseResponse $ LLM.content responseMsg

    processToolCalls responseMsg toolCallList = do
      Log.logTrace "AI requesting tool calls" (AE.object ["iteration" AE..= iteration, "tools" AE..= map (LLM.toolFunctionName . LLM.toolCallFunction) toolCallList])
      toolResults <- traverse (executeToolCall config) toolCallList
      Log.logTrace "AI tool calls completed" (AE.object ["iteration" AE..= iteration, "toolCount" AE..= length toolResults, "resultsPreview" AE..= map (\r -> T.take 200 r.formatted) toolResults])
      newMessages <- liftIO $ addMessagesToMemory config.limits.maxTokenBuffer chatHistory (responseMsg : zipWith mkToolResultMsg toolCallList toolResults)
      runAgenticLoop config apiKey newMessages params (iteration + 1)


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


executeToolCall :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> LLM.ToolCall -> Eff es ToolResult
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


executeGetFieldValues :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
executeGetFieldValues config args = case getTextArg "field" args of
  Just field -> do
    let lim = getLimitArg "limit" config.limits.maxFieldValues config.limits.defaultFieldLimit args
        kqlQuery = "| summarize count() by " <> field <> " | sort by count_ desc | take " <> show lim
    runKqlAndFormat config kqlQuery [] $ \(results, _, _) ->
      "Values for '" <> field <> "': " <> formatSummarizeResults results
  _ -> pure $ toolError "get_field_values" "missing 'field'" args


executeGetServices :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Eff es Text
executeGetServices config = do
  let kqlQuery = "| summarize count() by resource.service.name | sort by count_ desc | take " <> show config.limits.maxServices
  runKqlAndFormat config kqlQuery [] $ \(results, _, _) ->
    "Available services: " <> formatSummarizeResults results


executeCountQuery :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
executeCountQuery config args = case getTextArg "query" args of
  Just kqlQuery ->
    runKqlAndFormat config kqlQuery [] $ \(_, _, count) ->
      "Query '" <> kqlQuery <> "' matches " <> show count <> " entries"
  _ -> pure $ toolError "count_query" "missing 'query'" args


executeSampleLogs :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
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


runKqlWithRawData :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> [Text] -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> (Text, AE.Value)) -> Eff es ToolResult
runKqlWithRawData config kqlQuery cols formatResult = case parseQueryToAST kqlQuery of
  Left parseErr -> pure $ ToolResult ("Error: Query parse failed - " <> show parseErr) Nothing
  Right queryAST -> do
    resultE <- selectLogTable config.projectId queryAST kqlQuery Nothing config.timeRange cols Nothing Nothing
    pure $ case resultE of
      Left err -> ToolResult ("Error: Query execution failed - " <> err) Nothing
      Right res -> let (txt, raw) = formatResult res in ToolResult txt (Just raw)


executeRunQuery :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Map.Map Text AE.Value -> Eff es ToolResult
executeRunQuery config args = case getTextArg "query" args of
  Just query -> do
    let lim = getLimitArg "limit" config.limits.maxQueryResults config.limits.maxDisplayRows args
        fullQuery = if "| take" `T.isInfixOf` query then query else query <> " | take " <> show lim
    runKqlWithRawData config fullQuery [] $ \(results, headers, count) ->
      ( formatQueryResults config.limits.maxDisplayRows results count
      , AE.object ["headers" AE..= headers, "data" AE..= results, "count" AE..= count]
      )
  _ -> pure $ ToolResult (toolError "run_query" "missing 'query'" args) Nothing


executeSqlQuery :: DB es => AgenticConfig -> Map.Map Text AE.Value -> Eff es Text
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


runKqlAndFormat :: (DB es, Log :> es, Time.Time :> es, Tracing :> es) => AgenticConfig -> Text -> [Text] -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> Text) -> Eff es Text
runKqlAndFormat config kqlQuery cols formatResult = case parseQueryToAST kqlQuery of
  Left parseErr -> pure $ "Error: Query parse failed - " <> show parseErr
  Right queryAST -> do
    resultE <- selectLogTable config.projectId queryAST kqlQuery Nothing config.timeRange cols Nothing Nothing
    pure $ either ("Error: Query execution failed - " <>) formatResult resultE
