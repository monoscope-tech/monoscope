{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Pkg.AI.Agentic (
  -- * Types
  AgenticConfig (..),
  AgenticMode (..),
  ToolLimits (..),
  AgenticResponse (..),

  -- * Main functions
  runAgenticQuery,
  defaultAgenticConfig,
  defaultLimits,

  -- * Mode selection
  suggestAgenticMode,
  QueryContext (..),
) where

import Data.Aeson qualified as AE
import Data.HashMap.Strict qualified as HM
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
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.RequestDumps (selectLogTable)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
import OpenAI.V1.Tool qualified as OAITool
import Pkg.AI qualified as AI
import Pkg.Parser (parseQueryToAST)
import Relude hiding (pass)
import System.Types (DB)


-- | Tool execution limits
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


-- | Mode for LLM interaction
data AgenticMode = FastMode | AgenticMode
  deriving (Eq, Show)


-- | Configuration for agentic LLM calls
data AgenticConfig = AgenticConfig
  { mode :: AgenticMode
  , maxIterations :: Int
  , projectId :: Projects.ProjectId
  , timeRange :: (Maybe UTCTime, Maybe UTCTime)
  , facetContext :: Maybe FacetSummary
  , limits :: ToolLimits
  }


defaultAgenticConfig :: Projects.ProjectId -> AgenticConfig
defaultAgenticConfig pid =
  AgenticConfig
    { mode = FastMode
    , maxIterations = 3
    , projectId = pid
    , timeRange = (Nothing, Nothing)
    , facetContext = Nothing
    , limits = defaultLimits
    }


-- | Context for determining agentic mode
data QueryContext = WebExplorer | SlackBot | DiscordBot | WhatsAppBot | DashboardWidget
  deriving (Eq, Show)


suggestAgenticMode :: QueryContext -> Text -> AgenticMode
suggestAgenticMode DashboardWidget _ = FastMode
suggestAgenticMode _ userQuery =
  let complexIndicators = ["compare", "correlation", "changed", "investigate", "analyze", "sample", "examples"]
   in if any (`T.isInfixOf` T.toLower userQuery) complexIndicators then AgenticMode else FastMode


-- | Response from the agentic system
data AgenticResponse
  = FinalResponse AI.ChatLLMResponse
  | AgenticError Text
  deriving (Show)


-- * Helper functions


getIntFromArgs :: Text -> Map.Map Text AE.Value -> Maybe Int
getIntFromArgs key args = case Map.lookup key args of
  Just (AE.Number n) -> Just $ round n
  _ -> Nothing


getTextArg :: Text -> Map.Map Text AE.Value -> Maybe Text
getTextArg key args = case Map.lookup key args of
  Just (AE.String s) -> Just s
  _ -> Nothing


formatSummarizeResults :: V.Vector (V.Vector AE.Value) -> Text
formatSummarizeResults = T.intercalate ", " . mapMaybe formatRow . V.toList
  where
    formatRow (V.toList -> [v, c]) = Just $ "\"" <> jsonToText v <> "\" (" <> jsonToText c <> ")"
    formatRow _ = Nothing


formatSampleLogs :: Int -> V.Vector (V.Vector AE.Value) -> Text
formatSampleLogs maxBody = T.intercalate "\n" . mapMaybe formatRow . V.toList
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


-- * OpenAI Tool Definitions (for native tool calling)


mkToolDef :: Text -> Text -> AE.Value -> OAITool.Tool
mkToolDef name desc params =
  OAITool.Tool_Function
    OAITool.Function
      { OAITool.description = Just desc
      , OAITool.name = name
      , OAITool.parameters = Just params
      , OAITool.strict = Just False
      }


getFieldValuesTool :: OAITool.Tool
getFieldValuesTool =
  mkToolDef
    "get_field_values"
    "Get distinct values for a specific field. Use to discover what values exist for fields like service names, status codes, etc."
    $ AE.object
      [ "type" AE..= ("object" :: Text)
      , "properties"
          AE..= AE.object
            [ "field" AE..= AE.object ["type" AE..= ("string" :: Text), "description" AE..= ("Field name (e.g., resource.service.name, level)" :: Text)]
            , "limit" AE..= AE.object ["type" AE..= ("integer" :: Text), "description" AE..= ("Max values to return (default 10)" :: Text)]
            ]
      , "required" AE..= (["field"] :: [Text])
      ]


getServicesTool :: OAITool.Tool
getServicesTool =
  mkToolDef
    "get_services"
    "Get list of services in this project"
    $ AE.object ["type" AE..= ("object" :: Text), "properties" AE..= AE.object []]


countQueryTool :: OAITool.Tool
countQueryTool =
  mkToolDef
    "count_query"
    "Get the count of results for a KQL query. Use to verify a query matches data before finalizing."
    $ AE.object
      [ "type" AE..= ("object" :: Text)
      , "properties"
          AE..= AE.object
            ["query" AE..= AE.object ["type" AE..= ("string" :: Text), "description" AE..= ("KQL query to count" :: Text)]]
      , "required" AE..= (["query"] :: [Text])
      ]


sampleLogsTool :: OAITool.Tool
sampleLogsTool =
  mkToolDef
    "sample_logs"
    "Get sample log entries matching a query. Use to understand the structure/content of matching logs."
    $ AE.object
      [ "type" AE..= ("object" :: Text)
      , "properties"
          AE..= AE.object
            [ "query" AE..= AE.object ["type" AE..= ("string" :: Text), "description" AE..= ("KQL query to match" :: Text)]
            , "limit" AE..= AE.object ["type" AE..= ("integer" :: Text), "description" AE..= ("Max samples (default 3, max 5)" :: Text)]
            ]
      , "required" AE..= (["query"] :: [Text])
      ]


getFacetsTool :: OAITool.Tool
getFacetsTool =
  mkToolDef
    "get_facets"
    "Get precomputed facets showing popular values for common fields like services, status codes, methods, etc."
    $ AE.object ["type" AE..= ("object" :: Text), "properties" AE..= AE.object []]


getSchemaTool :: OAITool.Tool
getSchemaTool =
  mkToolDef
    "get_schema"
    "Get the schema of available fields in the log/span data. Use when you need to know what fields are queryable."
    $ AE.object ["type" AE..= ("object" :: Text), "properties" AE..= AE.object []]


runQueryTool :: OAITool.Tool
runQueryTool =
  mkToolDef
    "run_query"
    "Execute a KQL query and return results. Use for exploratory queries to understand data."
    $ AE.object
      [ "type" AE..= ("object" :: Text)
      , "properties"
          AE..= AE.object
            [ "query" AE..= AE.object ["type" AE..= ("string" :: Text), "description" AE..= ("KQL query to execute" :: Text)]
            , "limit" AE..= AE.object ["type" AE..= ("integer" :: Text), "description" AE..= ("Max results (default 20)" :: Text)]
            ]
      , "required" AE..= (["query"] :: [Text])
      ]


allToolDefs :: [OAITool.Tool]
allToolDefs =
  [ getFieldValuesTool
  , getServicesTool
  , countQueryTool
  , sampleLogsTool
  , getFacetsTool
  , getSchemaTool
  , runQueryTool
  ]


-- * Main Entry Point


-- | Build the system prompt
buildSystemPrompt :: AgenticConfig -> Text
buildSystemPrompt config =
  let basePrompt = AI.systemPrompt
      facetSection = formatFacetContext config.facetContext
   in basePrompt <> facetSection


-- | Strip markdown code blocks from response
stripCodeBlock :: Text -> Text
stripCodeBlock t
  | "```json" `T.isPrefixOf` stripped = T.strip $ T.dropWhileEnd (== '`') $ T.drop 7 stripped
  | "```" `T.isPrefixOf` stripped = T.strip $ T.dropWhileEnd (== '`') $ T.drop 3 stripped
  | otherwise = stripped
  where
    stripped = T.strip t


-- | Parse LLM response to extract the final response
parseResponse :: Text -> Either Text AI.ChatLLMResponse
parseResponse responseText =
  let cleaned = stripCodeBlock responseText
   in AI.getAskLLMResponse cleaned


-- | Main entry point for agentic LLM queries
runAgenticQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text AI.ChatLLMResponse)
runAgenticQuery config userQuery apiKey = do
  case config.mode of
    FastMode -> runFastMode config userQuery apiKey
    AgenticMode -> runAgenticMode config userQuery apiKey


-- | Fast mode: single LLM call without tools
runFastMode
  :: DB es
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text AI.ChatLLMResponse)
runFastMode config userQuery apiKey = do
  let systemPrompt = buildSystemPrompt config
      fullPrompt = systemPrompt <> "\n\nUser query: " <> userQuery
  result <- liftIO $ AI.callOpenAIAPI fullPrompt apiKey
  pure $ result >>= parseResponse


-- | Agentic mode: use langchain-hs chat API with native tool calling
runAgenticMode
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> Text
  -> Eff es (Either Text AI.ChatLLMResponse)
runAgenticMode config userQuery apiKey = do
  let openAI = OpenAI.OpenAI{apiKey = apiKey, callbacks = [], baseUrl = Nothing}
      systemPrompt = buildSystemPrompt config
      systemMsg = LLM.Message LLM.System systemPrompt LLM.defaultMessageData
      userMsg = LLM.Message LLM.User userQuery LLM.defaultMessageData
      messages = systemMsg :| [userMsg]
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model "gpt-4o-mini"
          , OpenAIV1.tools = Just $ V.fromList allToolDefs
          , OpenAIV1.messages = V.empty -- Will be set by langchain-hs chat
          }

  runAgenticLoop config openAI messages params 0


-- | Internal agentic loop using langchain-hs chat API with native tool calling
runAgenticLoop
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> OpenAI.OpenAI
  -> LLM.ChatHistory
  -> OpenAIV1.CreateChatCompletion
  -> Int
  -> Eff es (Either Text AI.ChatLLMResponse)
runAgenticLoop config openAI messages params iteration
  | iteration >= config.maxIterations =
      pure $ Left "Maximum tool iterations reached without final response"
  | otherwise = do
      -- Call LLM using langchain-hs chat API (handles tool calling natively)
      result <- liftIO $ LLM.chat openAI messages (Just params)

      case result of
        Left err -> pure $ Left $ "LLM Error: " <> show err
        Right responseMsg -> do
          -- Check if there are tool calls (langchain-hs parses these from the API response)
          case LLM.toolCalls (LLM.messageData responseMsg) of
            Nothing -> do
              -- No tool calls - this is the final response
              let content = LLM.content responseMsg
              pure $ parseResponse content
            Just toolCallList -> do
              -- Execute tool calls using our effectful handlers
              toolResults <- traverse (executeToolCall config) toolCallList

              -- Build messages: add assistant msg with tool calls, then tool results
              let assistantMsg = responseMsg
                  toolMsgs = zipWith mkToolResultMsg toolCallList toolResults
                  newMessages = NE.fromList $ NE.toList messages ++ [assistantMsg] ++ toolMsgs

              -- Continue the loop with updated conversation history
              runAgenticLoop config openAI newMessages params (iteration + 1)


mkToolResultMsg :: LLM.ToolCall -> Text -> LLM.Message
mkToolResultMsg tc result =
  LLM.Message
    { LLM.role = LLM.Tool
    , LLM.content = result
    , LLM.messageData = LLM.defaultMessageData{LLM.name = Just $ LLM.toolCallId tc}
    }


-- | Execute a single tool call in effectful context
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
    _ -> pure $ "Unknown tool: " <> funcName


-- * Tool Execution (Effectful)


executeGetFieldValues
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeGetFieldValues config args =
  case Map.lookup "field" args of
    Just (AE.String field) -> do
      let lim = min config.limits.maxFieldValues $ fromMaybe config.limits.defaultFieldLimit (getIntFromArgs "limit" args)
          kqlQuery = "| summarize count() by " <> field <> " | sort by _count desc | take " <> show lim
      runKqlAndFormat config kqlQuery [] $ \(results, _, _) ->
        "Values for '" <> field <> "': " <> formatSummarizeResults results
    _ -> pure "Error: Missing or invalid 'field' argument"


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
  _ -> pure "Error: Missing or invalid 'query' argument"


executeSampleLogs
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeSampleLogs config args = case getTextArg "query" args of
  Just kqlQuery -> do
    let lim = min config.limits.maxSampleLogs $ fromMaybe config.limits.defaultSampleLimit (getIntFromArgs "limit" args)
        fullQuery = if "| take" `T.isInfixOf` kqlQuery then kqlQuery else kqlQuery <> " | take " <> show lim
        sampleColumns = ["level", "name", "resource.service.name", "body"]
    runKqlAndFormat config fullQuery sampleColumns $ \(results, _, _) ->
      "Sample logs:\n" <> formatSampleLogs config.limits.maxBodyPreview results
  _ -> pure "Error: Missing or invalid 'query' argument"


executeGetFacets :: AgenticConfig -> Text
executeGetFacets config = maybe "No facet data available" formatFacetSummary config.facetContext


executeRunQuery
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Map.Map Text AE.Value
  -> Eff es Text
executeRunQuery config args = case getTextArg "query" args of
  Just query -> do
    let lim = min config.limits.maxQueryResults $ fromMaybe config.limits.maxDisplayRows (getIntFromArgs "limit" args)
        fullQuery = if "| take" `T.isInfixOf` query then query else query <> " | take " <> show lim
    runKqlAndFormat config fullQuery [] $ \(results, _, count) ->
      formatQueryResults config.limits.maxDisplayRows results count
  _ -> pure "Error: Missing or invalid 'query' argument"


-- | Helper to run KQL query and format result
runKqlAndFormat
  :: (DB es, Log :> es, Time.Time :> es)
  => AgenticConfig
  -> Text
  -> [Text]
  -> ((V.Vector (V.Vector AE.Value), [Text], Int) -> Text)
  -> Eff es Text
runKqlAndFormat config kqlQuery cols formatResult = case parseQueryToAST kqlQuery of
  Left _ -> pure "Error: Query parse failed"
  Right queryAST -> do
    resultE <- selectLogTable config.projectId queryAST kqlQuery Nothing config.timeRange cols Nothing Nothing
    pure $ either (const "Error: Query execution failed") formatResult resultE
