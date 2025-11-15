{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Effectful.LLM (
  LLM (..),
  callLLM,
  callOpenAIAPI,
  runLLMReal,
  runLLMGolden,
) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic
import Langchain.LLM.Core qualified as LLMCore
import Langchain.LLM.OpenAI qualified as OpenAI
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import Relude
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))


type role LLM phantom nominal
data LLM :: Effect where
  CallLLM :: Text -> Text -> LLM m (Either Text Text)


type instance DispatchOf LLM = 'Dynamic


-- | API function to call LLM
callLLM :: LLM :> es => Text -> Text -> Eff es (Either Text Text)
callLLM prompt apiKey = send (CallLLM prompt apiKey)


-- | Real interpreter that makes actual OpenAI API calls
runLLMReal :: IOE :> es => Eff (LLM ': es) a -> Eff es a
runLLMReal = interpret $ \_ -> \case
  CallLLM prompt apiKey -> liftIO $ callOpenAIAPI prompt apiKey


-- | Golden test interpreter that caches responses
runLLMGolden :: IOE :> es => FilePath -> Eff (LLM ': es) a -> Eff es a
runLLMGolden goldenDir = interpret $ \_ -> \case
  CallLLM prompt apiKey -> liftIO $ getOrCreateGoldenResponse goldenDir prompt apiKey


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
