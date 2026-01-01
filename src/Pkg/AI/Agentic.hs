{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pkg.AI.Agentic (
  -- * Types
  AgenticConfig (..),
  AgenticMode (..),
  Tool (..),
  ToolCall (..),
  ToolResult (..),
  AgenticResponse (..),

  -- * Main functions
  runAgenticQuery,
  defaultAgenticConfig,

  -- * Mode selection
  suggestAgenticMode,
  QueryContext (..),

  -- * Tool definitions
  availableTools,
  toolDescriptions,
) where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Effectful (Eff)
import Effectful.Log (Log)
import Effectful.Time qualified as Time
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.RequestDumps (executeSecuredQuery, selectLogTable)
import Models.Projects.Projects qualified as Projects
import Pkg.AI qualified as AI
import Pkg.Parser (parseQueryToAST)
import Relude
import System.Types (DB)


-- * Constants


maxFieldValues :: Int
maxFieldValues = 20


maxSampleLogs :: Int
maxSampleLogs = 5


maxServices :: Int
maxServices = 20


defaultFieldLimit :: Int
defaultFieldLimit = 10


defaultSampleLimit :: Int
defaultSampleLimit = 3


-- | Mode for LLM interaction
data AgenticMode
  = -- | Fast mode: Skip tool calling, generate query directly (for bots, simple queries)
    FastMode
  | -- | Agentic mode: Allow LLM to call tools to gather context before generating query
    AgenticMode
  deriving (Eq, Show)


-- | Configuration for agentic LLM calls
data AgenticConfig = AgenticConfig
  { mode :: AgenticMode
  -- ^ Whether to use fast mode or allow tool calling
  , maxIterations :: Int
  -- ^ Maximum number of tool-calling iterations (prevents runaway loops)
  , projectId :: Projects.ProjectId
  -- ^ Project ID for executing queries
  , timeRange :: (Maybe UTCTime, Maybe UTCTime)
  -- ^ Time range for queries
  , facetContext :: Maybe FacetSummary
  -- ^ Pre-computed facets to include as context (services, status codes, etc.)
  }


-- | Default configuration for agentic queries
defaultAgenticConfig :: Projects.ProjectId -> AgenticConfig
defaultAgenticConfig pid =
  AgenticConfig
    { mode = FastMode -- Default to fast mode since we include facet context
    , maxIterations = 3
    , projectId = pid
    , timeRange = (Nothing, Nothing)
    , facetContext = Nothing
    }


-- | Context for determining whether to use agentic mode
data QueryContext
  = -- | Log explorer (web UI) - can afford extra latency for better results
    WebExplorer
  | -- | Slack bot - prefer lower latency
    SlackBot
  | -- | Discord bot - prefer lower latency
    DiscordBot
  | -- | WhatsApp bot - prefer lower latency
    WhatsAppBot
  | -- | Dashboard widget - prefer lower latency
    DashboardWidget
  deriving (Eq, Show)


-- | Determine whether to use agentic mode based on query and context
-- Returns FastMode for simple queries or latency-sensitive contexts
-- Returns AgenticMode when the query would benefit from data exploration
suggestAgenticMode :: QueryContext -> Text -> AgenticMode
suggestAgenticMode context userQuery =
  let query = T.toLower $ T.strip userQuery

      -- Patterns that suggest the query needs deeper exploration
      -- (even with facets pre-included, these may need tool calls)
      complexIndicators =
        [ "compare"
        , "correlation"
        , "what changed"
        , "why are there"
        , "investigate"
        , "analyze"
        , "sample"
        , "show me examples"
        ]

      -- Check if query explicitly needs exploration
      needsExploration = any (`T.isInfixOf` query) complexIndicators
   in case context of
        -- All contexts prefer fast mode by default since we pre-include facets
        -- Only use agentic mode for queries that explicitly need exploration
        WebExplorer -> if needsExploration then AgenticMode else FastMode
        SlackBot -> if needsExploration then AgenticMode else FastMode
        DiscordBot -> if needsExploration then AgenticMode else FastMode
        WhatsAppBot -> if needsExploration then AgenticMode else FastMode
        DashboardWidget -> FastMode -- Always fast for widgets


-- | Available tools the LLM can call
data Tool
  = -- | Get distinct values for a field (useful for understanding what values exist)
    GetFieldValues
  | -- | Get a count for a given KQL query
    CountQuery
  | -- | Get available services in the project
    GetServices
  | -- | Sample a few log entries matching a query
    SampleLogs
  | -- | Get precomputed facets (popular values for common fields)
    GetFacets
  | -- | Run a query (KQL preferred, SQL as fallback for complex queries)
    RunQuery
  deriving (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Query type for RunQuery tool
data QueryType = KQL | SQL
  deriving (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | A tool call request from the LLM
data ToolCall = ToolCall
  { tool :: Tool
  , arguments :: AE.Value
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Result from executing a tool
data ToolResult = ToolResult
  { tool :: Tool
  , result :: Text
  , success :: Bool
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Response from the agentic system
data AgenticResponse
  = -- | LLM wants to call tools before responding
    NeedsTools [ToolCall]
  | -- | LLM is ready to provide the final response
    FinalResponse AI.ChatLLMResponse
  | -- | Error occurred
    AgenticError Text
  deriving (Show)


-- | Tool descriptions for the LLM prompt
toolDescriptions :: Text
toolDescriptions =
  T.unlines
    [ ""
    , "AVAILABLE TOOLS:"
    , "You can optionally call tools to gather more context before generating your final query."
    , "Only use tools when you genuinely need more information to generate an accurate query."
    , ""
    , "To call a tool, respond with a JSON object in this format:"
    , "{"
    , "  \"tool_calls\": ["
    , "    {\"tool\": \"<tool_name>\", \"arguments\": {<args>}}"
    , "  ]"
    , "}"
    , ""
    , "Available tools:"
    , ""
    , "1. GetFieldValues - Get distinct values for a specific field"
    , "   Arguments: {\"field\": \"<field_name>\", \"limit\": <number>}"
    , "   Example: {\"tool\": \"GetFieldValues\", \"arguments\": {\"field\": \"resource.service.name\", \"limit\": 10}}"
    , "   Use when: User mentions a service/field value you're not sure exists"
    , ""
    , "2. GetServices - Get list of services in this project"
    , "   Arguments: {} (none required)"
    , "   Example: {\"tool\": \"GetServices\", \"arguments\": {}}"
    , "   Use when: User asks about services but you don't know what services exist"
    , ""
    , "3. CountQuery - Get the count of results for a KQL query"
    , "   Arguments: {\"query\": \"<kql_query>\"}"
    , "   Example: {\"tool\": \"CountQuery\", \"arguments\": {\"query\": \"level == \\\"ERROR\\\"\"}}"
    , "   Use when: You want to verify a query returns results before finalizing"
    , ""
    , "4. SampleLogs - Get sample log entries matching a query"
    , "   Arguments: {\"query\": \"<kql_query>\", \"limit\": <number>}"
    , "   Example: {\"tool\": \"SampleLogs\", \"arguments\": {\"query\": \"level == \\\"ERROR\\\"\", \"limit\": 3}}"
    , "   Use when: You need to understand the structure/content of matching logs"
    , ""
    , "5. GetFacets - Get precomputed facets showing popular values for common fields"
    , "   Arguments: {} (none required)"
    , "   Example: {\"tool\": \"GetFacets\", \"arguments\": {}}"
    , "   Use when: You need to know what values are common for services, status codes, endpoints, etc."
    , ""
    , "6. RunQuery - Execute a query and return results (KQL preferred, SQL for complex cases)"
    , "   Arguments: {\"query\": \"<query>\", \"type\": \"KQL\" | \"SQL\", \"limit\": <number>}"
    , "   Example (KQL): {\"tool\": \"RunQuery\", \"arguments\": {\"query\": \"level == \\\"ERROR\\\" | take 10\", \"type\": \"KQL\"}}"
    , "   Example (SQL): {\"tool\": \"RunQuery\", \"arguments\": {\"query\": \"SELECT level, count(*) FROM otel_logs_and_spans GROUP BY level\", \"type\": \"SQL\", \"limit\": 20}}"
    , "   NOTE: For SQL queries, project_id filter is automatically injected - do NOT include it in your query."
    , "   IMPORTANT: Prefer KQL for most queries. Only use SQL when KQL cannot express the query (e.g., complex JOINs, window functions)."
    , "   Use when: You need to run an exploratory query to understand the data before generating the final response"
    , ""
    , "IMPORTANT GUIDELINES:"
    , "- DO NOT use tools for simple, well-known queries (e.g., 'show errors', 'slow requests')"
    , "- Use tools ONLY when the query requires specific values you're uncertain about"
    , "- PREFER KQL over SQL for all tools - KQL has caching and optimizations"
    , "- After receiving tool results, provide your final response in the standard JSON format"
    , "- Maximum tool calls allowed: 3 iterations"
    , ""
    ]


-- | List of available tools
availableTools :: [Tool]
availableTools = [GetFieldValues, CountQuery, GetServices, SampleLogs, GetFacets, RunQuery]


-- | Parse the LLM response to determine if it's a tool call or final response
parseAgenticResponse :: Text -> AgenticResponse
parseAgenticResponse responseText =
  let trimmed = T.strip responseText
   in AE.eitherDecode @AE.Value (fromStrict $ encodeUtf8 trimmed) & \case
        Left _ -> AgenticError $ "Invalid response format: " <> T.take 100 trimmed
        Right val ->
          parseToolCalls val & \case
            Just calls -> NeedsTools calls
            Nothing -> either AgenticError FinalResponse $ AI.getAskLLMResponse trimmed


-- | Try to parse tool calls from JSON value
parseToolCalls :: AE.Value -> Maybe [ToolCall]
parseToolCalls val = AET.parseMaybe parser val
  where
    parser = AE.withObject "ToolCallResponse" $ \obj -> do
      calls <- obj AE..: "tool_calls"
      AE.parseJSON calls


-- | Execute a single tool call and return the result
executeTool :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> ToolCall -> Eff es ToolResult
executeTool config ToolCall{..} = case tool of
  GetFieldValues -> executeGetFieldValues config arguments
  CountQuery -> executeCountQuery config arguments
  GetServices -> executeGetServices config arguments
  SampleLogs -> executeSampleLogs config arguments
  GetFacets -> executeGetFacets config arguments
  RunQuery -> executeRunQuery config arguments


-- * Tool Result Helpers


-- | Create a successful tool result
successResult :: Tool -> Text -> ToolResult
successResult t msg = ToolResult{tool = t, result = msg, success = True}


-- | Create a failed tool result
errorResult :: Tool -> Text -> ToolResult
errorResult t msg = ToolResult{tool = t, result = msg, success = False}


-- | Parse a required text argument from tool args
getTextArg :: Text -> AE.Value -> Maybe Text
getTextArg key = AET.parseMaybe (AE.withObject "args" (AE..: key))


-- | Parse an optional int argument with default
getIntArg :: Text -> Int -> AE.Value -> Int
getIntArg key def args = fromMaybe def $ AET.parseMaybe (AE.withObject "args" (AE..: key)) args


-- | Parse query type argument (defaults to KQL)
getQueryType :: AE.Value -> QueryType
getQueryType args = fromMaybe KQL $ AET.parseMaybe parser args
  where
    parser = AE.withObject "args" $ \obj -> do
      typeStr <- obj AE..: "type" :: AET.Parser Text
      pure $ case T.toUpper typeStr of
        "SQL" -> SQL
        _ -> KQL


-- | Helper to run a KQL query and format the result
runKqlQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Tool
  -> Text
  -> [Text]
  -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> Text)
  -> Eff es ToolResult
runKqlQuery config t kqlQuery cols formatResult =
  case parseQueryToAST kqlQuery of
    Left parseErr -> pure $ errorResult t $ "Error parsing query: " <> parseErr
    Right queryAST -> do
      resultE <-
        selectLogTable
          config.projectId
          queryAST
          kqlQuery
          Nothing
          config.timeRange
          cols
          Nothing
          Nothing
      pure $ case resultE of
        Left err -> errorResult t $ "Error executing query: " <> err
        Right res -> successResult t (formatResult res)


-- | Execute GetFieldValues tool - get distinct values for a field using KQL
executeGetFieldValues :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> AE.Value -> Eff es ToolResult
executeGetFieldValues config args =
  case getTextArg "field" args of
    Nothing -> pure $ errorResult GetFieldValues "Error: 'field' argument is required"
    Just field -> do
      let limit = min maxFieldValues $ getIntArg "limit" defaultFieldLimit args
          kqlQuery = "| summarize count() by " <> field <> " | sort by _count desc | take " <> show limit
      runKqlQuery config GetFieldValues kqlQuery [] $ \(results, _, _) ->
        "Values for '" <> field <> "': " <> formatSummarizeResults field results


-- | Execute CountQuery tool - count results for a KQL query using the KQL infrastructure
executeCountQuery :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> AE.Value -> Eff es ToolResult
executeCountQuery config args =
  case getTextArg "query" args of
    Nothing -> pure $ errorResult CountQuery "Error: 'query' argument is required"
    Just kqlQuery ->
      runKqlQuery config CountQuery kqlQuery [] $ \(_, _, count) ->
        "Query '" <> kqlQuery <> "' matches " <> show count <> " entries"


-- | Execute GetServices tool - get list of services in the project using KQL
executeGetServices :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> AE.Value -> Eff es ToolResult
executeGetServices config _args =
  let kqlQuery = "| summarize count() by resource.service.name | sort by _count desc | take " <> show maxServices
   in runKqlQuery config GetServices kqlQuery [] $ \(results, _, _) ->
        "Available services: " <> formatSummarizeResults "resource.service.name" results


-- | Execute SampleLogs tool - get sample log entries using KQL
executeSampleLogs :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> AE.Value -> Eff es ToolResult
executeSampleLogs config args =
  case getTextArg "query" args of
    Nothing -> pure $ errorResult SampleLogs "Error: 'query' argument is required"
    Just kqlQuery -> do
      let limit = min maxSampleLogs $ getIntArg "limit" defaultSampleLimit args
          fullQuery =
            if "| take" `T.isInfixOf` kqlQuery
              then kqlQuery
              else kqlQuery <> " | take " <> show limit
          sampleColumns = ["level", "name", "resource.service.name", "body"]
      runKqlQuery config SampleLogs fullQuery sampleColumns $ \(results, _, _) ->
        "Sample logs matching '" <> kqlQuery <> "':\n" <> formatSampleLogs results


-- | Execute GetFacets tool - return precomputed facets from config
executeGetFacets :: Applicative f => AgenticConfig -> AE.Value -> f ToolResult
executeGetFacets config _args =
  pure $ case config.facetContext of
    Nothing -> errorResult GetFacets "No facet data available for this project"
    Just summary -> successResult GetFacets (formatFacetSummary summary)


-- | Execute RunQuery tool - supports both KQL (preferred) and raw SQL
executeRunQuery :: (DB es, Log :> es, Time.Time :> es) => AgenticConfig -> AE.Value -> Eff es ToolResult
executeRunQuery config args =
  case getTextArg "query" args of
    Nothing -> pure $ errorResult RunQuery "Error: 'query' argument is required"
    Just query -> do
      let queryType = getQueryType args
          limit = min 100 $ getIntArg "limit" 20 args
      case queryType of
        KQL -> runKqlQuery config RunQuery query [] $ \(results, _, count) ->
          formatQueryResults results count
        SQL -> runSqlQuery config query limit


-- | Run a raw SQL query (fallback for complex queries KQL can't express)
-- SECURITY: Uses executeSecuredQuery which enforces project_id via parameterized CTE
runSqlQuery :: DB es => AgenticConfig -> Text -> Int -> Eff es ToolResult
runSqlQuery config query limit = do
  -- Validate the query doesn't contain dangerous operations
  let lowerQuery = T.toLower query
      dangerousPatterns = ["drop ", "delete ", "truncate ", "update ", "insert ", "alter ", "create "]
  if any (`T.isInfixOf` lowerQuery) dangerousPatterns
    then pure $ errorResult RunQuery "Error: Only SELECT queries are allowed"
    else do
      -- Use executeSecuredQuery which wraps the query in a CTE that
      -- pre-filters otel_logs_and_spans by project_id using parameterized query
      resultE <- executeSecuredQuery config.projectId query limit
      pure $ case resultE of
        Left err -> errorResult RunQuery $ "Error executing SQL: " <> err
        Right results -> successResult RunQuery $ formatQueryResults results (V.length results)


-- | Format query results for display
formatQueryResults :: V.Vector (V.Vector AE.Value) -> Int -> Text
formatQueryResults results count =
  let rows = V.toList results
      formatted = T.intercalate "\n" $ take 20 $ map formatResultRow rows
      truncated = if count > 20 then "\n... and " <> show (count - 20) <> " more rows" else ""
   in "Results (" <> show count <> " rows):\n" <> formatted <> truncated


-- | Format a single result row
formatResultRow :: V.Vector AE.Value -> Text
formatResultRow row =
  let vals = V.toList row
   in "  " <> T.intercalate " | " (map jsonToText vals)


-- | Format summarize query results (from KQL | summarize count() by <field>)
-- Results come as Vector of Vectors where each row is [field_value, count]
formatSummarizeResults :: Text -> V.Vector (V.Vector AE.Value) -> Text
formatSummarizeResults _fieldName results =
  T.intercalate ", " $ mapMaybe formatRow (V.toList results)
  where
    formatRow row = case V.toList row of
      [value, count] -> Just $ "\"" <> jsonToText value <> "\" (" <> jsonToText count <> ")"
      _ -> Nothing


-- | Format sample log results for display
-- Results come as Vector of Vectors containing log field values
formatSampleLogs :: V.Vector (V.Vector AE.Value) -> Text
formatSampleLogs results =
  T.intercalate "\n" $ mapMaybe formatRow (V.toList results)
  where
    formatRow row = case V.toList row of
      (level : name : service : bodyParts) ->
        let body = T.take 100 $ T.intercalate " " $ map jsonToText bodyParts
         in Just $ "  - [" <> jsonToText level <> "] " <> jsonToText name <> " (" <> jsonToText service <> "): " <> body
      _ -> Nothing


-- | Convert JSON value to text for display
jsonToText :: AE.Value -> Text
jsonToText = \case
  AE.String s -> s
  AE.Number n -> show n
  AE.Bool b -> if b then "true" else "false"
  AE.Null -> "null"
  AE.Array arr -> "[" <> T.intercalate ", " (map jsonToText $ V.toList arr) <> "]"
  AE.Object _ -> "{...}"


-- | Format facet summary for LLM consumption
formatFacetSummary :: FacetSummary -> Text
formatFacetSummary summary =
  let FacetData facetMap = summary.facetJson
      formatField (fieldName, values) =
        let columnName = T.replace "___" "." fieldName
            valueStrs = map (\(FacetValue v c) -> "\"" <> v <> "\" (" <> show c <> ")") $ take 10 values
         in columnName <> ": " <> T.intercalate ", " valueStrs
      formattedFacets = map formatField $ HM.toList facetMap
   in "Facet data (popular values for common fields):\n" <> T.intercalate "\n" formattedFacets


-- | Key fields to include in facet context
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


-- | Format facets as context to include in the prompt
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
            T.unlines
              [ ""
              , "PROJECT DATA CONTEXT:"
              , "The following are popular values for key fields in this project (from the last 24 hours):"
              , T.intercalate "\n" formattedFacets
              , ""
              , "Use these actual values when the user's query matches them."
              , ""
              ]


-- | Format tool results for feeding back to the LLM
formatToolResults :: [ToolResult] -> Text
formatToolResults results =
  T.unlines
    $ ["", "TOOL RESULTS:", ""]
    <> map formatResult results
    <> ["", "Now provide your final response in the standard JSON format."]
  where
    formatResult ToolResult{..} =
      "- "
        <> show tool
        <> ": "
        <> if success then result else "[ERROR] " <> result


-- | Build the full prompt with optional tool context and facet data
buildAgenticPrompt :: AgenticConfig -> Text -> [ToolResult] -> Text
buildAgenticPrompt config userQuery previousResults =
  let basePrompt = AI.systemPrompt
      -- Include facet context for all queries (FastMode and AgenticMode)
      facetSection = formatFacetContext config.facetContext
      toolSection = case config.mode of
        FastMode -> ""
        AgenticMode -> toolDescriptions
      resultsSection = case previousResults of
        [] -> ""
        rs -> formatToolResults rs
   in basePrompt <> facetSection <> toolSection <> resultsSection <> "\n\nUser query: " <> userQuery


-- | Main entry point for agentic LLM queries
-- This runs the agentic loop: LLM -> parse response -> execute tools -> feed back -> repeat
runAgenticQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text AI.ChatLLMResponse)
runAgenticQuery config userQuery apiKey =
  runAgenticLoop config userQuery apiKey [] 0


-- | Internal agentic loop
runAgenticLoop
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> [ToolResult]
  -> Int
  -> Eff es (Either Text AI.ChatLLMResponse)
runAgenticLoop config userQuery apiKey previousResults iteration
  | iteration >= config.maxIterations =
      -- Max iterations reached, force a final response
      pure $ Left "Maximum tool iterations reached without final response"
  | otherwise = do
      -- Build the prompt (includes facet context automatically)
      let prompt = buildAgenticPrompt config userQuery previousResults

      -- Call the LLM
      result <- liftIO $ AI.callOpenAIAPI prompt apiKey

      case result of
        Left err -> pure $ Left err
        Right responseText -> do
          -- Parse the response
          let parsed = parseAgenticResponse responseText

          case parsed of
            AgenticError err -> pure $ Left err
            FinalResponse resp -> pure $ Right resp
            NeedsTools calls -> do
              -- In FastMode, we shouldn't get here, but handle it gracefully
              case config.mode of
                FastMode ->
                  -- Ignore tool calls in fast mode, try to extract response anyway
                  pure $ Left "Tool calls not allowed in fast mode"
                AgenticMode -> do
                  -- Execute all tool calls
                  toolResults <- traverse (executeTool config) calls
                  -- Recurse with tool results
                  runAgenticLoop config userQuery apiKey (previousResults <> toolResults) (iteration + 1)
