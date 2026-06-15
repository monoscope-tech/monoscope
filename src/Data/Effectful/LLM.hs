module Data.Effectful.LLM (
  LLM (..),
  callLLM,
  callAgenticChat,
  embedDocuments,
  callOpenAIAPI,
  runLLMReal,
  runLLMGolden,
) where

import Control.Lens ((^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key)
import Data.Effectful.Wreq (withGoldenCache)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Langchain.DocumentLoader.Core qualified as DocLoader
import Langchain.Embeddings.Core qualified as EmbCore
import Langchain.Embeddings.OpenAI qualified as EmbOAI
import Langchain.LLM.Core qualified as LLMCore
import Langchain.LLM.OpenAI qualified as OpenAI
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
import Relude


-- Generic-derived JSON for LLMCore.Message (record field names match the prior
-- hand-written keys, so this is byte-identical to the old manual instances).
deriving stock instance Generic LLMCore.Message
deriving anyclass instance AE.ToJSON LLMCore.Message
deriving anyclass instance AE.FromJSON LLMCore.Message


-- Cache entry for agentic chat calls
data AgenticChatCache = AgenticChatCache
  { accHistory :: [LLMCore.Message]
  , accModel :: Text
  , accHasTools :: Bool
  , accResponse :: Either Text LLMCore.Message -- Left = error, Right = success
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


type role LLM phantom nominal
data LLM :: Effect where
  CallLLM :: Text -> Text -> Text -> LLM m (Either Text Text)
  CallAgenticChat :: LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> Text -> LLM m (Either Text LLMCore.Message)
  EmbedDocuments :: EmbOAI.OpenAIEmbeddings -> [DocLoader.Document] -> LLM m (Either Text [[Float]])


type instance DispatchOf LLM = 'Dynamic


makeEffect ''LLM


-- | Real interpreter that makes actual OpenAI API calls
runLLMReal :: IOE :> es => Eff (LLM ': es) a -> Eff es a
runLLMReal = interpret $ \_ -> \case
  CallLLM model prompt apiKey -> liftIO $ callOpenAIAPI model prompt apiKey
  CallAgenticChat history params apiKey -> liftIO do
    let openAI = OpenAI.OpenAI{apiKey, callbacks = [], baseUrl = Nothing}
    result <- LLMCore.chat openAI history (Just params)
    pure $ first show result
  EmbedDocuments config docs -> liftIO $ first show <$> EmbCore.embedDocuments config docs


-- | Golden test interpreter that caches responses
runLLMGolden :: IOE :> es => FilePath -> Eff (LLM ': es) a -> Eff es a
runLLMGolden goldenDir = interpret $ \_ -> \case
  CallLLM model prompt apiKey -> liftIO $ getOrCreateGoldenResponse goldenDir model prompt apiKey
  CallAgenticChat history params apiKey -> liftIO $ getOrCreateAgenticGoldenResponse goldenDir history params apiKey
  EmbedDocuments config docs -> liftIO $ getOrCreateEmbeddingGoldenResponse goldenDir config docs


-- | Sanitize text for use in filenames (replaces special chars with underscores)
sanitizeForFilename :: Text -> String
sanitizeForFilename = toString . T.replace "/" "_" . T.replace "|" "_" . T.replace " " "_" . T.replace "\n" "_" . T.take 50


-- | Helper function to generate cache filename from prompt
promptToFilename :: Text -> String
promptToFilename prompt = "llm_" <> sanitizeForFilename prompt <> ".json"


-- | Data type for storing LLM responses
data LLMResponse = LLMResponse
  { llmPrompt :: Text
  , llmResponse :: Either Text Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Get or create a golden response for an LLM call
getOrCreateGoldenResponse :: FilePath -> Text -> Text -> Text -> IO (Either Text Text)
getOrCreateGoldenResponse goldenDir model prompt apiKey =
  withGoldenCache goldenDir (promptToFilename prompt) ("Failed to decode LLM response from file: " <>) (.llmResponse) $ do
    response <- callOpenAIAPI model prompt apiKey
    pure (LLMResponse{llmPrompt = prompt, llmResponse = response}, response)


-- | Actual OpenAI API call implementation
callOpenAIAPI :: Text -> Text -> Text -> IO (Either Text Text)
callOpenAIAPI model fullPrompt apiKey = do
  let openAI =
        OpenAI.OpenAI
          { OpenAI.apiKey = apiKey
          , OpenAI.callbacks = []
          , OpenAI.baseUrl = Nothing
          }
      userMessage =
        OpenAIV1.User
          { OpenAIV1.content = V.fromList [OpenAIV1.Text{OpenAIV1.text = fullPrompt}]
          , OpenAIV1.name = Nothing
          }
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = Models.Model model
          , OpenAIV1.messages = V.fromList [userMessage]
          }
  -- Use langchain-hs to generate response
  result <- liftIO $ LLMCore.generate openAI fullPrompt (Just params)
  case result of
    Left err -> pure $ Left $ "LLM Error: " <> show err
    Right response -> pure $ Right response


-- | Create deterministic cache key from user query and iteration
cacheKeyFromQuery :: [LLMCore.Message] -> Bool -> String
cacheKeyFromQuery messages hasTools =
  let
    -- Extract original user query from first user message
    isUserMessage m = case AE.toJSON (LLMCore.role m) of
      AE.String role -> role == "User"
      _ -> False
    userQuery = case find isUserMessage messages of
      Just msg -> sanitizeForFilename $ LLMCore.content msg
      Nothing -> "unknown_query"
    -- Count messages to determine iteration (2=initial, 4=after tools, etc)
    iterNum = length messages
    toolSuffix = if hasTools then "_with_tools" else ""
   in
    "agentic_" <> userQuery <> "_iter" <> show iterNum <> toolSuffix


-- Helper to check if CreateChatCompletion has tools (via JSON inspection)
hasToolsInParams :: OpenAIV1.CreateChatCompletion -> Bool
hasToolsInParams p = isJust $ AE.toJSON p ^? key "tools"


-- | Get or create a golden response for an agentic chat call
getOrCreateAgenticGoldenResponse :: FilePath -> LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> Text -> IO (Either Text LLMCore.Message)
getOrCreateAgenticGoldenResponse goldenDir history params apiKey =
  withGoldenCache goldenDir (cacheKeyFromQuery historyMessages hasTools <> ".json") ("Failed to decode agentic chat cache from: " <>) (.accResponse) $ do
    let openAI = OpenAI.OpenAI{apiKey, callbacks = [], baseUrl = Nothing}
        Models.Model modelName = params.model
    response <- first show <$> LLMCore.chat openAI history (Just params)
    pure (AgenticChatCache{accHistory = historyMessages, accModel = modelName, accHasTools = hasTools, accResponse = response}, response)
  where
    historyMessages = toList history
    hasTools = hasToolsInParams params


-- Embedding golden cache
data EmbeddingCache = EmbeddingCache
  { texts :: [Text]
  , result :: Either Text [[Float]]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


getOrCreateEmbeddingGoldenResponse :: FilePath -> EmbOAI.OpenAIEmbeddings -> [DocLoader.Document] -> IO (Either Text [[Float]])
getOrCreateEmbeddingGoldenResponse goldenDir config docs =
  withGoldenCache goldenDir (cacheKey <> ".json") ("Failed to decode embedding cache from: " <>) (.result) $ do
    result <- first show <$> EmbCore.embedDocuments config docs
    pure (EmbeddingCache{texts, result}, result)
  where
    texts = map (toStrict . DocLoader.pageContent) docs
    cacheKey = "emb_" <> sanitizeForFilename (T.intercalate "__" $ take 3 texts) <> "_n" <> show (length docs)
