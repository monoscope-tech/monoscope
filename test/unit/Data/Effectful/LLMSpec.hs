module Data.Effectful.LLMSpec (spec) where

import Data.Effectful.LLM (openAIEmbeddings)
import Langchain.Embeddings.OpenAI qualified as EmbOAI
import Relude
import Test.Hspec


spec :: Spec
spec = describe "embedding endpoint configuration" do
  let cases :: [(Text, String)]
      cases =
        [ ("", "https://api.openai.com/v1")
        , ("https://api.openai.com/", "https://api.openai.com/v1")
        , ("https://api.openai.com", "https://api.openai.com/v1")
        , ("https://api.openai.com/v1/", "https://api.openai.com/v1")
        , ("https://api.openai.com/v1", "https://api.openai.com/v1")
        , ("https://proxy.example/openai/v1/", "https://proxy.example/openai/v1")
        , ("https://proxy.example/api", "https://proxy.example/api")
        ]
  forM_ cases \(configured, expected) ->
    it ("resolves " <> show configured) do
      let client = openAIEmbeddings "test-key" configured
      EmbOAI.baseUrl client `shouldBe` Just expected
      EmbOAI.apiKey client `shouldBe` "test-key"
