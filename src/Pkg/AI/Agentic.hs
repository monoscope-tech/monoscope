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
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful (Eff)
import Effectful.PostgreSQL qualified as PG
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
  }
  deriving (Show)


-- | Default configuration for agentic queries
defaultAgenticConfig :: Projects.ProjectId -> AgenticConfig
defaultAgenticConfig pid =
  AgenticConfig
    { mode = AgenticMode
    , maxIterations = 3
    , projectId = pid
    , timeRange = (Nothing, Nothing)
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
      -- Simple patterns that don't need agentic exploration
      simplePatterns =
        [ "show me errors"
        , "show errors"
        , "show all errors"
        , "errors"
        , "slow requests"
        , "slow"
        , "show me slow"
        , "failed requests"
        , "failures"
        , "5xx"
        , "500"
        , "4xx"
        , "404"
        , "warnings"
        , "show warnings"
        , "debug logs"
        , "all logs"
        , "recent logs"
        , "latest"
        ]

      -- Patterns that suggest the query needs context (specific values, services, etc.)
      complexIndicators =
        [ "from service"
        , "from the"
        , "in service"
        , "by service"
        , "for user"
        , "user id"
        , "endpoint"
        , "api endpoint"
        , "specific"
        , "particular"
        , "named"
        , "called"
        , "what services"
        , "which services"
        , "list services"
        , "what endpoints"
        , "what fields"
        , "what values"
        , "compare"
        , "between"
        , "correlation"
        ]

      -- Check if query is simple
      isSimpleQuery = any (`T.isInfixOf` query) simplePatterns && not (any (`T.isInfixOf` query) complexIndicators)

      -- Check if query explicitly needs context
      needsContext = any (`T.isInfixOf` query) complexIndicators
   in case context of
        -- Bot contexts: prefer fast mode unless the query really needs exploration
        SlackBot -> if needsContext then AgenticMode else FastMode
        DiscordBot -> if needsContext then AgenticMode else FastMode
        WhatsAppBot -> if needsContext then AgenticMode else FastMode
        DashboardWidget -> FastMode -- Always fast for widgets

        -- Web explorer: use agentic mode unless query is very simple
        WebExplorer -> if isSimpleQuery then FastMode else AgenticMode


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
    , "IMPORTANT GUIDELINES:"
    , "- DO NOT use tools for simple, well-known queries (e.g., 'show errors', 'slow requests')"
    , "- Use tools ONLY when the query requires specific values you're uncertain about"
    , "- After receiving tool results, provide your final response in the standard JSON format"
    , "- Maximum tool calls allowed: 3 iterations"
    , ""
    ]


-- | List of available tools
availableTools :: [Tool]
availableTools = [GetFieldValues, CountQuery, GetServices, SampleLogs]


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


-- | Build the full prompt with optional tool context
buildAgenticPrompt :: AgenticMode -> Text -> [ToolResult] -> Text
buildAgenticPrompt mode userQuery previousResults =
  let basePrompt = AI.systemPrompt
      toolSection = case mode of
        FastMode -> ""
        AgenticMode -> toolDescriptions
      resultsSection = case previousResults of
        [] -> ""
        rs -> formatToolResults rs
   in basePrompt <> toolSection <> resultsSection <> "\n\nUser query: " <> userQuery


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
      -- Build the prompt
      let prompt = buildAgenticPrompt config.mode userQuery previousResults

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
