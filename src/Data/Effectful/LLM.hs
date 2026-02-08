{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Effectful.LLM (
  LLM (..),
  callLLM,
  callAgenticChat,
  callOpenAIAPI,
  runLLMReal,
  runLLMGolden,
) where

import Control.Lens ((^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key)
import Data.Hashable (hash)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Langchain.LLM.Core qualified as LLMCore
import Langchain.LLM.OpenAI qualified as OpenAI
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))


-- Manual JSON instances for LLMCore.Message (can't use Generic due to type structure)
instance AE.ToJSON LLMCore.Message where
  toJSON msg =
    AE.object
      [ "role" AE..= LLMCore.role msg
      , "content" AE..= LLMCore.content msg
      , "messageData" AE..= LLMCore.messageData msg
      ]


instance AE.FromJSON LLMCore.Message where
  parseJSON = AE.withObject "Message" \o ->
    LLMCore.Message
      <$> (o AE..: "role")
      <*> (o AE..: "content")
      <*> (o AE..: "messageData")


-- Cache entry for agentic chat calls
data AgenticChatCache = AgenticChatCache
  { accHistory :: [LLMCore.Message]
  , accModel :: Text
  , accHasTools :: Bool
  , accResponse :: LLMCore.Message
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


type role LLM phantom nominal
data LLM :: Effect where
  CallLLM :: Text -> Text -> LLM m (Either Text Text)
  CallAgenticChat :: LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> Text -> LLM m (Either Text LLMCore.Message)


type instance DispatchOf LLM = 'Dynamic


-- | API function to call LLM
callLLM :: LLM :> es => Text -> Text -> Eff es (Either Text Text)
callLLM prompt apiKey = send (CallLLM prompt apiKey)


-- | API function to call agentic chat
callAgenticChat :: LLM :> es => LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> Text -> Eff es (Either Text LLMCore.Message)
callAgenticChat history params apiKey = send (CallAgenticChat history params apiKey)


-- | Real interpreter that makes actual OpenAI API calls
runLLMReal :: IOE :> es => Eff (LLM ': es) a -> Eff es a
runLLMReal = interpret $ \_ -> \case
  CallLLM prompt apiKey -> liftIO $ callOpenAIAPI prompt apiKey
  CallAgenticChat history params apiKey -> liftIO do
    let openAI = OpenAI.OpenAI{apiKey, callbacks = [], baseUrl = Nothing}
    result <- LLMCore.chat openAI history (Just params)
    pure $ first show result


-- | Golden test interpreter that caches responses
runLLMGolden :: IOE :> es => FilePath -> Eff (LLM ': es) a -> Eff es a
runLLMGolden goldenDir = interpret $ \_ -> \case
  CallLLM prompt apiKey -> liftIO $ getOrCreateGoldenResponse goldenDir prompt apiKey
  CallAgenticChat history params apiKey -> liftIO $ getOrCreateAgenticGoldenResponse goldenDir history params apiKey


-- | Helper function to sanitize prompt for filename
sanitizePrompt :: Text -> String
sanitizePrompt prompt =
  let cleaned = T.replace "\n" "_" $ T.replace " " "_" $ T.take 50 prompt
   in toString cleaned


-- | Helper function to generate cache filename from prompt
promptToFilename :: Text -> String
promptToFilename prompt =
  let sanitized = sanitizePrompt prompt
   in "llm_" <> sanitized <> ".json"


-- | Data type for storing LLM responses
data LLMResponse = LLMResponse
  { llmPrompt :: Text
  , llmResponse :: Either Text Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Get or create a golden response for an LLM call
getOrCreateGoldenResponse :: FilePath -> Text -> Text -> IO (Either Text Text)
getOrCreateGoldenResponse goldenDir prompt apiKey = do
  let fileName = promptToFilename prompt
      filePath = goldenDir </> fileName
  exists <- doesFileExist filePath
  if exists
    then do
      -- Read cached response
      content <- readFileLBS filePath
      case AE.decode content of
        Just (llmResp :: LLMResponse) -> return llmResp.llmResponse
        Nothing -> error $ fromString $ "Failed to decode LLM response from file: " <> filePath
    else do
      -- Make actual API call and cache it
      createDirectoryIfMissing True goldenDir
      response <- callOpenAIAPI prompt apiKey
      let llmResp = LLMResponse{llmPrompt = prompt, llmResponse = response}
      writeFileLBS filePath (AE.encode llmResp)
      return response


-- | Actual OpenAI API call implementation
callOpenAIAPI :: Text -> Text -> IO (Either Text Text)
callOpenAIAPI fullPrompt apiKey = do
  let openAI =
        OpenAI.OpenAI
          { OpenAI.apiKey = apiKey
          , OpenAI.callbacks = []
          , OpenAI.baseUrl = Nothing
          }
      -- Create chat completion parameters with model name
      userMessage =
        OpenAIV1.User
          { OpenAIV1.content = V.fromList [OpenAIV1.Text{OpenAIV1.text = fullPrompt}]
          , OpenAIV1.name = Nothing
          }
      params =
        OpenAIV1._CreateChatCompletion
          { OpenAIV1.model = "gpt-4o-mini"
          , OpenAIV1.messages = V.fromList [userMessage]
          }
  -- Use langchain-hs to generate response
  result <- liftIO $ LLMCore.generate openAI fullPrompt (Just params)
  case result of
    Left err -> pure $ Left $ "LLM Error: " <> show err
    Right response -> pure $ Right response


-- | Create deterministic cache key from request data
hashCacheKey :: [LLMCore.Message] -> Text -> Bool -> String
hashCacheKey messages model hasTools =
  let messageStrs = map (\m -> show (LLMCore.role m) <> ":" <> T.take 100 (LLMCore.content m)) messages
      historyStr = T.intercalate "|" messageStrs
      combined = historyStr <> model <> show hasTools
      hashVal = hash combined
   in "chat_" <> show (abs hashVal)


-- Helper to check if CreateChatCompletion has tools (via JSON inspection)
hasToolsInParams :: OpenAIV1.CreateChatCompletion -> Bool
hasToolsInParams p = isJust $ AE.toJSON p ^? key "tools"


-- | Get or create a golden response for an agentic chat call
getOrCreateAgenticGoldenResponse :: FilePath -> LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> Text -> IO (Either Text LLMCore.Message)
getOrCreateAgenticGoldenResponse goldenDir history params apiKey = do
  let historyMessages = toList history
      modelName = "gpt-4o-mini" -- Model is always gpt-4o-mini in this codebase
      hasTools = hasToolsInParams params
      cacheKey = hashCacheKey historyMessages modelName hasTools
      fileName = "agentic_" <> cacheKey <> ".json"
      filePath = goldenDir </> fileName
  exists <- doesFileExist filePath
  updateGolden <- isJust <$> lookupEnv "UPDATE_GOLDEN"
  if exists && not updateGolden
    then do
      content <- readFileLBS filePath
      case AE.decode content of
        Just (cached :: AgenticChatCache) -> return $ Right cached.accResponse
        Nothing -> error $ toText $ "Failed to decode agentic chat cache from: " <> filePath
    else do
      createDirectoryIfMissing True goldenDir
      let openAI = OpenAI.OpenAI{apiKey, callbacks = [], baseUrl = Nothing}
      result <- LLMCore.chat openAI history (Just params)
      case result of
        Left err -> return $ Left $ show err
        Right responseMsg -> do
          let cached = AgenticChatCache{accHistory = historyMessages, accModel = modelName, accHasTools = hasTools, accResponse = responseMsg}
          writeFileLBS filePath (AE.encode cached)
          return $ Right responseMsg
