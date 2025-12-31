{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pkg.AI qualified as AI
import Pkg.Parser (parseQueryToAST, toQText)
import Relude
import System.Types (DB)


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
    , "IMPORTANT GUIDELINES:"
    , "- DO NOT use tools for simple, well-known queries (e.g., 'show errors', 'slow requests')"
    , "- Use tools ONLY when the query requires specific values you're uncertain about"
    , "- After receiving tool results, provide your final response in the standard JSON format"
    , "- Maximum tool calls allowed: 3 iterations"
    , ""
    ]


-- | List of available tools
availableTools :: [Tool]
availableTools = [GetFieldValues, CountQuery, GetServices, SampleLogs, GetFacets]


-- | Parse the LLM response to determine if it's a tool call or final response
parseAgenticResponse :: Text -> AgenticResponse
parseAgenticResponse responseText =
  let trimmed = T.strip responseText
      -- Try to parse as JSON
      decoded = AE.eitherDecode @AE.Value (fromStrict $ encodeUtf8 trimmed)
   in case decoded of
        Left _ ->
          -- Not valid JSON, treat as error
          AgenticError $ "Invalid response format: " <> T.take 100 trimmed
        Right val ->
          case parseToolCalls val of
            Just calls -> NeedsTools calls
            Nothing ->
              -- Try to parse as final response
              case AI.getAskLLMResponse trimmed of
                Left err -> AgenticError err
                Right resp -> FinalResponse resp


-- | Try to parse tool calls from JSON value
parseToolCalls :: AE.Value -> Maybe [ToolCall]
parseToolCalls val = AET.parseMaybe parser val
  where
    parser = AE.withObject "ToolCallResponse" $ \obj -> do
      calls <- obj AE..: "tool_calls"
      AE.parseJSON calls


-- | Execute a single tool call and return the result
executeTool :: DB es => AgenticConfig -> ToolCall -> Eff es ToolResult
executeTool config ToolCall{..} = case tool of
  GetFieldValues -> executeGetFieldValues config arguments
  CountQuery -> executeCountQuery config arguments
  GetServices -> executeGetServices config arguments
  SampleLogs -> executeSampleLogs config arguments
  GetFacets -> executeGetFacets config arguments


-- | Convert field name to database column format (e.g., "resource.service.name" -> "resource___service___name")
fieldToColumn :: Text -> Text
fieldToColumn = T.replace "." "___"


-- | Get the time range clause for queries
timeRangeClause :: AgenticConfig -> Text
timeRangeClause config =
  let fmtTime = toText . iso8601Show
   in case config.timeRange of
        (Just from, Just to) ->
          "AND timestamp >= '" <> fmtTime from <> "' AND timestamp < '" <> fmtTime to <> "'"
        (Just from, Nothing) ->
          "AND timestamp >= '" <> fmtTime from <> "'"
        (Nothing, Just to) ->
          "AND timestamp < '" <> fmtTime to <> "'"
        (Nothing, Nothing) ->
          "AND timestamp >= NOW() - INTERVAL '24 hours'"


-- | Execute GetFieldValues tool - get distinct values for a field
executeGetFieldValues :: DB es => AgenticConfig -> AE.Value -> Eff es ToolResult
executeGetFieldValues config args = do
  let fieldM = AET.parseMaybe (AE.withObject "args" (AE..: "field")) args :: Maybe Text
      limitM = AET.parseMaybe (AE.withObject "args" (AE..: "limit")) args :: Maybe Int
      limit = min 20 $ fromMaybe 10 limitM

  case fieldM of
    Nothing ->
      pure
        $ ToolResult
          { tool = GetFieldValues
          , result = "Error: 'field' argument is required"
          , success = False
          }
    Just field -> do
      let columnName = fieldToColumn field
          pid = config.projectId.toText
          timeRange = timeRangeClause config
          queryText =
            [text|
              SELECT ${columnName}::text as value, COUNT(*) as cnt
              FROM otel_logs_and_spans
              WHERE project_id = '${pid}' ${timeRange}
                AND ${columnName} IS NOT NULL
              GROUP BY value
              ORDER BY cnt DESC
              LIMIT ${show limit}
            |]

      resultsE <- try @SomeException $ PG.query_ (Query $ encodeUtf8 queryText)
      case resultsE of
        Left err ->
          pure
            $ ToolResult
              { tool = GetFieldValues
              , result = "Error querying field '" <> field <> "': " <> show err
              , success = False
              }
        Right (results :: [(Text, Int)]) ->
          let formatted = T.intercalate ", " $ map (\(v, c) -> "\"" <> v <> "\" (" <> show c <> ")") results
           in pure
                $ ToolResult
                  { tool = GetFieldValues
                  , result = "Values for '" <> field <> "': " <> formatted
                  , success = True
                  }


-- | Execute CountQuery tool - count results for a KQL query
executeCountQuery :: DB es => AgenticConfig -> AE.Value -> Eff es ToolResult
executeCountQuery config args = do
  let queryM = AET.parseMaybe (AE.withObject "args" (AE..: "query")) args :: Maybe Text

  case queryM of
    Nothing ->
      pure
        $ ToolResult
          { tool = CountQuery
          , result = "Error: 'query' argument is required"
          , success = False
          }
    Just kqlQuery -> do
      -- Parse the KQL query to get WHERE conditions
      let parseResult = parseQueryToAST kqlQuery
      case parseResult of
        Left parseErr ->
          pure
            $ ToolResult
              { tool = CountQuery
              , result = "Error parsing query: " <> parseErr
              , success = False
              }
        Right queryAST -> do
          let whereClause = toQText queryAST
              pid = config.projectId.toText
              timeRange = timeRangeClause config
              -- Build a simple count query
              sqlQuery =
                [text|
                  SELECT COUNT(*) as cnt
                  FROM otel_logs_and_spans
                  WHERE project_id = '${pid}' ${timeRange}
                    AND (${whereClause})
                |]

          resultsE <- try @SomeException $ PG.query_ (Query $ encodeUtf8 sqlQuery)
          case resultsE of
            Left err ->
              pure
                $ ToolResult
                  { tool = CountQuery
                  , result = "Error executing count query: " <> show err
                  , success = False
                  }
            Right results ->
              let count = case results of
                    [(c :: Int)] -> c
                    _ -> 0
               in pure
                    $ ToolResult
                      { tool = CountQuery
                      , result = "Query '" <> kqlQuery <> "' matches " <> show count <> " entries"
                      , success = True
                      }


-- | Execute GetServices tool - get list of services in the project
executeGetServices :: DB es => AgenticConfig -> AE.Value -> Eff es ToolResult
executeGetServices config _args = do
  let pid = config.projectId.toText
      timeRange = timeRangeClause config
      queryText =
        [text|
          SELECT resource___service___name::text as service, COUNT(*) as cnt
          FROM otel_logs_and_spans
          WHERE project_id = '${pid}' ${timeRange}
            AND resource___service___name IS NOT NULL
          GROUP BY service
          ORDER BY cnt DESC
          LIMIT 20
        |]

  resultsE <- try @SomeException $ PG.query_ (Query $ encodeUtf8 queryText)
  case resultsE of
    Left err ->
      pure
        $ ToolResult
          { tool = GetServices
          , result = "Error querying services: " <> show err
          , success = False
          }
    Right (results :: [(Text, Int)]) ->
      let formatted = T.intercalate ", " $ map (\(s, c) -> "\"" <> s <> "\" (" <> show c <> " events)") results
       in pure
            $ ToolResult
              { tool = GetServices
              , result = "Available services: " <> formatted
              , success = True
              }


-- | Execute SampleLogs tool - get sample log entries
executeSampleLogs :: DB es => AgenticConfig -> AE.Value -> Eff es ToolResult
executeSampleLogs config args = do
  let queryM = AET.parseMaybe (AE.withObject "args" (AE..: "query")) args :: Maybe Text
      limitM = AET.parseMaybe (AE.withObject "args" (AE..: "limit")) args :: Maybe Int
      limit = min 5 $ fromMaybe 3 limitM

  case queryM of
    Nothing ->
      pure
        $ ToolResult
          { tool = SampleLogs
          , result = "Error: 'query' argument is required"
          , success = False
          }
    Just kqlQuery -> do
      let parseResult = parseQueryToAST kqlQuery
      case parseResult of
        Left parseErr ->
          pure
            $ ToolResult
              { tool = SampleLogs
              , result = "Error parsing query: " <> parseErr
              , success = False
              }
        Right queryAST -> do
          let whereClause = toQText queryAST
              pid = config.projectId.toText
              timeRange = timeRangeClause config
              sqlQuery =
                [text|
                  SELECT
                    COALESCE(level::text, 'N/A') as level,
                    COALESCE(name::text, 'N/A') as name,
                    COALESCE(resource___service___name::text, 'N/A') as service,
                    COALESCE(LEFT(body::text, 100), 'N/A') as body_preview
                  FROM otel_logs_and_spans
                  WHERE project_id = '${pid}' ${timeRange}
                    AND (${whereClause})
                  ORDER BY timestamp DESC
                  LIMIT ${show limit}
                |]

          resultsE <- try @SomeException $ PG.query_ (Query $ encodeUtf8 sqlQuery)
          case resultsE of
            Left err ->
              pure
                $ ToolResult
                  { tool = SampleLogs
                  , result = "Error sampling logs: " <> show err
                  , success = False
                  }
            Right (results :: [(Text, Text, Text, Text)]) ->
              let formatted =
                    T.intercalate "\n"
                      $ map
                        ( \(lvl, nm, svc, bdy) ->
                            "  - [" <> lvl <> "] " <> nm <> " (" <> svc <> "): " <> bdy
                        )
                        results
               in pure
                    $ ToolResult
                      { tool = SampleLogs
                      , result = "Sample logs matching '" <> kqlQuery <> "':\n" <> formatted
                      , success = True
                      }


-- | Execute GetFacets tool - return precomputed facets from config
executeGetFacets :: Applicative f => AgenticConfig -> AE.Value -> f ToolResult
executeGetFacets config _args =
  case config.facetContext of
    Nothing ->
      pure
        $ ToolResult
          { tool = GetFacets
          , result = "No facet data available for this project"
          , success = False
          }
    Just summary ->
      pure
        $ ToolResult
          { tool = GetFacets
          , result = formatFacetSummary summary
          , success = True
          }


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


-- | Format facets as context to include in the prompt
formatFacetContext :: Maybe FacetSummary -> Text
formatFacetContext Nothing = ""
formatFacetContext (Just summary) =
  let FacetData facetMap = summary.facetJson
      -- Key fields to always include in context
      keyFields =
        [ "resource___service___name"
        , "level"
        , "status_code"
        , "attributes___http___response___status_code"
        , "attributes___http___request___method"
        , "attributes___error___type"
        , "kind"
        , "name"
        ]
      formatField fieldName =
        case HM.lookup fieldName facetMap of
          Nothing -> Nothing
          Just values ->
            let columnName = T.replace "___" "." fieldName
                valueStrs = map (\(FacetValue v _) -> "\"" <> v <> "\"") $ take 8 values
             in Just $ columnName <> ": " <> T.intercalate ", " valueStrs
      formattedFacets = catMaybes $ map formatField keyFields
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
  :: DB es
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text AI.ChatLLMResponse)
runAgenticQuery config userQuery apiKey =
  runAgenticLoop config userQuery apiKey [] 0


-- | Internal agentic loop
runAgenticLoop
  :: DB es
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
