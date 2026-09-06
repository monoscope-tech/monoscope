module Data.Effectful.LLMSpec (spec) where

import Data.ByteString qualified as BS
import Data.Effectful.LLM (embedDocumentsBounded, openAIEmbeddings)
import Data.Text qualified as T
import Langchain.DocumentLoader.Core qualified as Doc
import Langchain.Embeddings.OpenAI qualified as EmbOAI
import Relude
import Test.Hspec


spec :: Spec
spec = do
  describe "embedding endpoint configuration" do
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

  describe "bounded embedding requests" do
    it "preserves long Unicode documents and keeps every request inside provider limits" do
      seen <- newIORef []
      let texts = ["short", T.replicate 100000 "🌍", T.replicate 400000 "abc "]
          request batch = do
            let inputs = map (toStrict . Doc.pageContent) batch
                sizes = map (BS.length . encodeUtf8) inputs
            modifyIORef' seen (<> inputs)
            pure
              $ if all (\n -> n > 0 && n < 8192) sizes && sum sizes <= 300000
                then Right (replicate (length inputs) [1, 0])
                else Left "provider input limit exceeded"
      result <- embedDocumentsBounded request (map (\t -> Doc.Document (toLazy t) mempty) texts)
      result `shouldBe` Right [[1, 0], [1, 0], [1, 0]]
      (mconcat <$> readIORef seen) `shouldReturn` mconcat texts

    it "rejects missing provider vectors instead of shifting document assignments" do
      embedDocumentsBounded (\_ -> pure $ Right []) [Doc.Document "hello" mempty]
        `shouldReturn` Left "Embedding response count does not match request"

    it "does not call the provider for an empty input list" do
      embedDocumentsBounded (\_ -> expectationFailure "unexpected request" $> Left "called") []
        `shouldReturn` Right []

    it "combines all chunks by length and preserves document ordering" do
      let long = T.replicate 8188 "a" <> T.replicate 2047 "b"
          inputs = map (\t -> Doc.Document (toLazy t) mempty) ["first", long, "last"]
          vector doc = case toStrict $ Doc.pageContent doc of
            "first" -> [0, 1]
            "last" -> [-1, 0]
            t | T.all (== 'a') t -> [1, 0]
            _ -> [0, 1]
      result <- embedDocumentsBounded (pure . Right . map vector) inputs
      case result of
        Right [firstVector, [x, y], lastVector] -> do
          firstVector `shouldBe` [0, 1]
          lastVector `shouldBe` [-1, 0]
          abs (x - 4 / sqrt 17) `shouldSatisfy` (< 0.000001)
          abs (y - 1 / sqrt 17) `shouldSatisfy` (< 0.000001)
        _ -> expectationFailure $ show result

    it "stops after a provider failure" do
      calls <- newIORef (0 :: Int)
      let request _ = modifyIORef' calls (+ 1) $> Left "rate limited"
      embedDocumentsBounded request [Doc.Document (toLazy $ T.replicate 100000 "🌍") mempty]
        `shouldReturn` Left "rate limited"
      readIORef calls `shouldReturn` 1
