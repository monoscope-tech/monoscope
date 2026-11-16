module Data.Effectful.LLM (
  LLM (..),
  callLLM,
  callAgenticChat,
  embedDocuments,
  openAIEmbeddings,
  embedDocumentsBounded,
  embedDocumentsWithProgress,
  modelAndEffort,
  LLMTelemetry (..),
  Reply (..),
  unmetered,
  GenAIContentCapture (..),
  capturesContent,
  runLLMReal,
  runLLMGolden,
  stripCodeBlock,
) where

import Control.Lens ((^?))
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key)
import Data.ByteString qualified as BS
import Data.Default (Default (..))
import Data.Effectful.Wreq (withGoldenCache)
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Text.Foreign qualified as TextForeign
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Langchain.DocumentLoader.Core qualified as DocLoader
import Langchain.Embeddings.Core qualified as EmbCore
import Langchain.Embeddings.OpenAI qualified as EmbOAI
import Langchain.LLM.Core qualified as LLMCore
import Langchain.LLM.OpenAICompatible ()
import OpenAI.V1 (Methods (..), getClientEnv, makeMethods)
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import OpenAI.V1.Models qualified as Models
import OpenAI.V1.Usage qualified as Usage
import OpenTelemetry.Attributes qualified as OA
import OpenTelemetry.Trace.Core qualified as Trace
import Pkg.GenAI qualified as GenAI
import Relude
import Relude.Extra.Bifunctor (firstF)
import System.Envy (Var (..))
import UnliftIO.Exception (tryAny)


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


-- | A model reply with the provider's token usage: Nothing for a replayed (golden) reply.
data Reply a = Reply {content :: a, usage :: Maybe GenAI.Usage}
  deriving stock (Functor, Show)


unmetered :: a -> Reply a
unmetered = (`Reply` Nothing)


type role LLM phantom nominal
data LLM :: Effect where
  CallLLM :: Text -> Text -> Text -> LLM m (Either Text Text)
  CallAgenticChat :: LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> Text -> LLM m (Either Text (Reply LLMCore.Message))
  EmbedDocuments :: EmbOAI.OpenAIEmbeddings -> [DocLoader.Document] -> LLM m (Either Text [[Float]])


type instance DispatchOf LLM = 'Dynamic


makeEffect ''LLM


-- | Configure the embedding client from the application settings. Langchain
-- appends /embeddings verbatim, so the base must have no trailing slash.
-- Accept the official host root as its versioned API base; custom proxy paths
-- are already API bases and must retain their path.
openAIEmbeddings :: Text -> Text -> EmbOAI.OpenAIEmbeddings
openAIEmbeddings apiKey baseUrl =
  EmbOAI.defaultOpenAIEmbeddings
    { EmbOAI.apiKey = apiKey
    , EmbOAI.baseUrl = Just $ if T.null baseUrl || normalized == "https://api.openai.com" then "https://api.openai.com/v1" else toString normalized
    }
  where
    normalized = T.dropWhileEnd (== '/') baseUrl


-- | Apply the embedding provider's per-input and batch limits without losing text.
-- UTF-8 byte length is an upper bound on cl100k_base token count. Keep inputs
-- under 8192 bytes, preserving UTF-8 boundaries when splitting chunks.
-- A batch of at most 36 such chunks stays below the 300,000-token request limit.
-- Long documents use a byte-length-weighted mean, normalized for cosine search.
embedDocumentsBounded :: Monad m => ([DocLoader.Document] -> m (Either Text [[Float]])) -> [DocLoader.Document] -> m (Either Text [[Float]])
embedDocumentsBounded request docs
  | any (T.null . toStrict . DocLoader.pageContent) docs = pure $ Left "Cannot embed an empty document"
  | otherwise = runExceptT do
      means <- go mempty chunks
      traverse (finish means) [0 .. length docs - 1]
  where
    -- takeWord8/dropWord8 advance together by up to three bytes to finish a
    -- code point. Reserve those bytes so even that final character fits.
    chunkBytes = fromIntegral (maxInputBytes - 3)
    splitText txt
      | T.null txt = []
      | otherwise = TextForeign.takeWord8 chunkBytes txt : splitText (TextForeign.dropWord8 chunkBytes txt)
    chunks = concat $ zipWith splitDocument [0 :: Int ..] docs
    splitDocument docId doc =
      let txt = toStrict $ DocLoader.pageContent doc
          parts = if byteLength txt <= maxInputBytes then [txt] else splitText txt
       in map (\part -> (docId, byteLength part, doc{DocLoader.pageContent = toLazy part})) parts
    byteLength = BS.length . encodeUtf8
    go means [] = pure means
    go means remaining = do
      let (batch, rest) = splitAt maxBatchChunks remaining
      vectors <- ExceptT $ request $ map (\(_, _, doc) -> doc) batch
      when (length vectors /= length batch) $ throwError "Embedding response count does not match request"
      updated <- foldM add means $ zip batch vectors
      go updated rest
    add means ((docId, weight, _), values)
      | null values || any (\v -> isNaN v || isInfinite v) values = throwError "Embedding response contains an invalid vector"
      | otherwise = do
          let !vector = VU.fromList $ map realToFrac values
          next <- case IntMap.lookup docId means of
            Nothing -> pure $ EmbeddingMean 1 weight vector
            Just (EmbeddingMean count total mean)
              | VU.length mean /= VU.length vector -> throwError "Embedding response dimensions do not match"
              | otherwise ->
                  let fraction = fromIntegral weight / fromIntegral (total + weight)
                      !combined = VU.zipWith (\a b -> a + fraction * (b - a)) mean vector
                   in pure $ EmbeddingMean (count + 1) (total + weight) combined
          pure $ IntMap.insert docId next means
    finish means docId = case IntMap.lookup docId means of
      Nothing -> throwError "Embedding response is missing a document"
      Just (EmbeddingMean count _ mean)
        | count == 1 -> pure $ map realToFrac $ VU.toList mean
        | otherwise ->
            let norm = sqrt $ VU.sum $ VU.map (\v -> v * v) mean :: Double
             in if norm == 0
                  then throwError "Chunk embeddings have a zero-length mean"
                  else pure $ map (realToFrac . (/ norm)) $ VU.toList mean


maxInputBytes, maxBatchChunks :: Int
maxInputBytes = 8191
maxBatchChunks = 300000 `div` maxInputBytes


-- | Save completed documents between bounded groups of provider requests.
-- A document spanning more than one request stays whole: partial chunk means
-- must never be persisted as that document's embedding.
embedDocumentsWithProgress
  :: Monad m
  => ([DocLoader.Document] -> m (Either Text [[Float]]))
  -> ([(a, [Float])] -> m ())
  -> [(a, DocLoader.Document)]
  -> m (Either Text [(a, [Float])])
embedDocumentsWithProgress request save docs = runExceptT $ concat . reverse <$> foldM store [] (groups docs)
  where
    chunks (_, doc) =
      let bytes = BS.length $ encodeUtf8 $ toStrict $ DocLoader.pageContent doc
       in if bytes <= maxInputBytes then 1 else 1 + (bytes - 1) `div` (maxInputBytes - 3)
    groups [] = []
    groups (doc : rest) =
      let (following, remaining) = fill (maxBatchChunks - chunks doc) rest
       in (doc : following) : groups remaining
    fill _ [] = ([], [])
    fill budget pending@(doc : rest)
      | chunks doc > budget = ([], pending)
      | otherwise =
          let (following, remaining) = fill (budget - chunks doc) rest
           in (doc : following, remaining)
    store completed batch = do
      vectors <- ExceptT $ request $ map snd batch
      when (length vectors /= length batch) $ throwError "Embedding response count does not match request"
      let pairs = zip (map fst batch) vectors
      lift $ save pairs
      pure (pairs : completed)


-- Strict fields keep chunk accumulation from retaining previous batch vectors.
data EmbeddingMean = EmbeddingMean !Int !Int !(VU.Vector Double)


-- | @OTEL_INSTRUMENTATION_GENAI_CAPTURE_CONTENT@, the GenAI conventions' opt-in for recording
-- prompt and completion text. Off by default: our prompts carry customers' telemetry.
data GenAIContentCapture = NoContent | SpanOnly | EventOnly | SpanAndEvent
  deriving stock (Bounded, Enum, Eq, Generic, Show)


instance Default GenAIContentCapture where
  def = NoContent


instance Var GenAIContentCapture where
  fromVar v = find ((== v) . toVar) universe
  toVar = \case
    NoContent -> "NO_CONTENT"
    SpanOnly -> "SPAN_ONLY"
    EventOnly -> "EVENT_ONLY"
    SpanAndEvent -> "SPAN_AND_EVENT"


data LLMTelemetry = LLMTelemetry {tracerProvider :: Trace.TracerProvider, capture :: GenAIContentCapture}


-- | Real interpreter that makes actual OpenAI API calls
runLLMReal :: IOE :> es => LLMTelemetry -> Eff (LLM ': es) a -> Eff es a
runLLMReal t = interpret $ \_ -> \case
  CallLLM model prompt apiKey -> liftIO $ fmap (replyText . (.content)) <$> completeChat t apiKey (promptParams model prompt)
  CallAgenticChat history params apiKey -> liftIO $ fmap (fmap LLMCore.from) <$> completeChat t apiKey (withHistory history params)
  EmbedDocuments config docs -> liftIO $ embedDocumentsBounded (firstF show . EmbCore.embedDocuments config) docs


-- | Golden test interpreter that caches responses
runLLMGolden :: IOE :> es => LLMTelemetry -> FilePath -> Eff (LLM ': es) a -> Eff es a
runLLMGolden t goldenDir = interpret $ \_ -> \case
  CallLLM model prompt apiKey -> liftIO $ inference t (promptParams model prompt) (\r -> contentAttr t "gen_ai.output.messages" ([AE.object ["role" AE..= ("assistant" :: Text), "content" AE..= r]] :: [AE.Value])) $ getOrCreateGoldenResponse goldenDir model prompt apiKey
  CallAgenticChat history params apiKey -> liftIO $ fmap unmetered <$> inference t (withHistory history params) (contentAttr t "gen_ai.output.messages" . pure @[] . LLMCore.to @(OpenAIV1.Message (V.Vector OpenAIV1.Content))) (getOrCreateAgenticGoldenResponse goldenDir history params apiKey)
  EmbedDocuments config docs -> liftIO $ getOrCreateEmbeddingGoldenResponse goldenDir config docs


withHistory :: LLMCore.ChatHistory -> OpenAIV1.CreateChatCompletion -> OpenAIV1.CreateChatCompletion
withHistory history params = params{OpenAIV1.messages = V.fromList $ map LLMCore.to $ toList history}


-- | A GenAI inference span (semconv-genai) around one chat completion. It writes to the
-- provider directly: the background stack interprets 'Tracing' before this effect.
inference :: LLMTelemetry -> OpenAIV1.CreateChatCompletion -> (a -> [(Text, OA.Attribute)]) -> IO (Either Text a) -> IO (Either Text a)
inference t params describe call =
  Trace.inSpan' (Trace.makeTracer t.tracerProvider "monoscope.llm" Trace.tracerOptions) ("chat " <> model) Trace.defaultSpanArguments{Trace.kind = Trace.Client, Trace.attributes = HM.fromList requestAttrs} \sp -> do
    result <- call
    case result of
      Left err -> Trace.setStatus sp (Trace.Error err)
      Right reply -> Trace.addAttributes sp $ HM.fromList $ describe reply
    pure result
  where
    Models.Model model = params.model
    requestAttrs =
      [("gen_ai.operation.name", "chat"), ("gen_ai.provider.name", "openai"), ("gen_ai.request.model", OA.toAttribute model)]
        <> [("gen_ai.request.reasoning.level", OA.toAttribute $ T.toLower $ T.drop 16 $ show e) | Just e <- [params.reasoning_effort]]
        <> contentAttr t "gen_ai.input.messages" params.messages


capturesContent :: GenAIContentCapture -> Bool
capturesContent = (`elem` [SpanOnly, SpanAndEvent])


-- | A message attribute, recorded only under content capture.
contentAttr :: AE.ToJSON a => LLMTelemetry -> Text -> a -> [(Text, OA.Attribute)]
contentAttr t k v = [(k, OA.toAttribute $ decodeUtf8 @Text $ AE.encode v) | capturesContent t.capture]


-- | langchain-hs drops the provider's usage, so call the client directly to keep it.
completeChat :: LLMTelemetry -> Text -> OpenAIV1.CreateChatCompletion -> IO (Either Text (Reply (OpenAIV1.Message Text)))
completeChat t apiKey params = (>>= \obj -> (`Reply` Just (usageOf obj)) <$> firstMessage obj) <$> inference t params usageAttrs (requestChat apiKey params)
  where
    usageOf OpenAIV1.ChatCompletionObject{OpenAIV1.usage = Usage.Usage{Usage.prompt_tokens, Usage.completion_tokens, Usage.prompt_tokens_details}} =
      GenAI.Usage (fromIntegral prompt_tokens) (fromIntegral completion_tokens) (fromIntegral $ fromMaybe 0 $ prompt_tokens_details >>= \Usage.PromptTokensDetails{Usage.cached_tokens} -> cached_tokens) 0
    usageAttrs OpenAIV1.ChatCompletionObject{OpenAIV1.id = responseId, OpenAIV1.model = Models.Model responseModel, OpenAIV1.usage = Usage.Usage{Usage.prompt_tokens, Usage.completion_tokens, Usage.prompt_tokens_details, Usage.completion_tokens_details}, OpenAIV1.choices} =
      [ ("gen_ai.response.id", OA.toAttribute responseId)
      , ("gen_ai.response.model", OA.toAttribute responseModel)
      , ("gen_ai.usage.input_tokens", tokens prompt_tokens)
      , ("gen_ai.usage.output_tokens", tokens completion_tokens)
      , ("gen_ai.response.finish_reasons", OA.toAttribute $ V.toList $ V.map (\OpenAIV1.Choice{OpenAIV1.finish_reason} -> finish_reason) choices)
      ]
        <> [("gen_ai.usage.cache_read.input_tokens", tokens n) | Just Usage.PromptTokensDetails{Usage.cached_tokens = Just n} <- [prompt_tokens_details]]
        <> [("gen_ai.usage.reasoning.output_tokens", tokens n) | Just Usage.CompletionTokensDetails{Usage.reasoning_tokens = Just n} <- [completion_tokens_details]]
        <> contentAttr t "gen_ai.output.messages" (V.map (\OpenAIV1.Choice{OpenAIV1.message} -> message) choices)
    tokens = OA.toAttribute @Int . fromIntegral


requestChat :: Text -> OpenAIV1.CreateChatCompletion -> IO (Either Text OpenAIV1.ChatCompletionObject)
requestChat apiKey params = do
  Methods{createChatCompletion} <- (\env -> makeMethods env apiKey Nothing Nothing) <$> getClientEnv "https://api.openai.com"
  first (("LLM Error: " <>) . show) <$> tryAny (createChatCompletion params)


firstMessage :: OpenAIV1.ChatCompletionObject -> Either Text (OpenAIV1.Message Text)
firstMessage OpenAIV1.ChatCompletionObject{OpenAIV1.choices} = maybe (Left "LLM Error: no choices returned") (\OpenAIV1.Choice{OpenAIV1.message} -> Right message) (V.headM choices)


replyText :: OpenAIV1.Message Text -> Text
replyText = LLMCore.content . LLMCore.from


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
    response <- fmap replyText . (>>= firstMessage) <$> requestChat apiKey (promptParams model prompt)
    pure (LLMResponse{llmPrompt = prompt, llmResponse = response}, response)


-- | Split an optional reasoning-effort suffix off a model spec ("gpt-5.6-luna#low").
-- An unrecognized suffix stays part of the model name so config typos fail loudly at the API.
--
-- >>> modelAndEffort "gpt-5.6-luna#high"
-- ("gpt-5.6-luna",Just ReasoningEffort_High)
-- >>> modelAndEffort "gpt-5.6-terra"
-- ("gpt-5.6-terra",Nothing)
-- >>> modelAndEffort "gpt-5.6-luna#hihg"
-- ("gpt-5.6-luna#hihg",Nothing)
-- >>> modelAndEffort "gpt-5.6-luna#none"
-- ("gpt-5.6-luna",Just ReasoningEffort_None)
modelAndEffort :: Text -> (Models.Model, Maybe OpenAIV1.ReasoningEffort)
modelAndEffort spec = case T.breakOnEnd "#" spec of
  (pre, suffix) | not (T.null pre), Just eff <- effort suffix -> (Models.Model (T.dropEnd 1 pre), Just eff)
  _ -> (Models.Model spec, Nothing)
  where
    effort = \case
      "none" -> Just OpenAIV1.ReasoningEffort_None
      "minimal" -> Just OpenAIV1.ReasoningEffort_Minimal
      "low" -> Just OpenAIV1.ReasoningEffort_Low
      "medium" -> Just OpenAIV1.ReasoningEffort_Medium
      "high" -> Just OpenAIV1.ReasoningEffort_High
      _ -> Nothing


promptParams :: Text -> Text -> OpenAIV1.CreateChatCompletion
promptParams model prompt =
  OpenAIV1._CreateChatCompletion
    { OpenAIV1.model = modelName
    , OpenAIV1.messages = V.fromList [OpenAIV1.User{OpenAIV1.content = V.fromList [OpenAIV1.Text{OpenAIV1.text = prompt}], OpenAIV1.name = Nothing}]
    , OpenAIV1.reasoning_effort = effort
    }
  where
    (modelName, effort) = modelAndEffort model


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
    let Models.Model modelName = params.model
    response <- fmap LLMCore.from . (>>= firstMessage) <$> requestChat apiKey (withHistory history params)
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
    result <- embedDocumentsBounded (firstF show . EmbCore.embedDocuments config) docs
    pure (EmbeddingCache{texts, result}, result)
  where
    texts = map (toStrict . DocLoader.pageContent) docs
    cacheKey = "emb_" <> sanitizeForFilename (T.intercalate "__" $ take 3 texts) <> "_n" <> show (length docs)


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
