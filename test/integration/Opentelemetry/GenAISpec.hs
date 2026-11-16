-- | LLM telemetry end to end: every producer dialect lands as GenAI conventions with a cost,
-- and Monoscope's own assistant, traced and re-ingested, reads back as one agent run.
module Opentelemetry.GenAISpec (spec) where

import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, values, _Number, _String)
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.Utils qualified as Bot
import Pages.Components (Deferred (..))
import Pages.LLM qualified as LLM
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Telemetry qualified as TelemetryPage
import Pkg.AI qualified as AI
import Pkg.Components.Table (Table (..))
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Logs.V1.Logs qualified as PL
import System.Config (AuthContext (..), EnvConfig (..))


spec :: Spec
spec = around withTestResources do
  describe "GenAI telemetry" do
    it "maps OpenInference, OpenLLMetry and Vercel AI SDK spans onto the GenAI conventions and prices inference" \tr -> do
      pid <- createTestProject tr "GenAI dialects"
      apiKey <- createTestAPIKey tr pid "genai"
      now <- getCurrentTime
      let emitIn trId n name attrs = ingestSpanLinked tr apiKey trId ("a" <> show n) Nothing name attrs (addUTCTime (fromIntegral n) now)
          emit = emitIn "6e1f"
      emit (1 :: Int) "openinference llm" [("openinference.span.kind", "LLM"), ("llm.model_name", "gpt-4o"), ("llm.token_count.prompt", "1000"), ("llm.token_count.completion", "100"), ("llm.input_messages.0.message.role", "user"), ("llm.input_messages.0.message.content", "Why is checkout slow?"), ("session.id", "conv-1")]
      emitIn "7a2b" 2 "openllmetry chat" [("gen_ai.system", "anthropic"), ("llm.request.type", "chat"), ("gen_ai.request.model", "claude-sonnet-4-5"), ("gen_ai.usage.prompt_tokens", "2000"), ("gen_ai.usage.completion_tokens", "50"), ("gen_ai.usage.cache_read_input_tokens", "1500"), ("gen_ai.completion.0.role", "assistant"), ("gen_ai.completion.0.content", "The pool is exhausted.")]
      emit 3 "ai.generateText" [("ai.operationId", "ai.generateText"), ("ai.model.id", "gpt-4o-mini"), ("ai.usage.promptTokens", "300"), ("ai.telemetry.metadata.sessionId", "conv-1")]
      emit 4 "ai.generateText.doGenerate" [("ai.operationId", "ai.generateText.doGenerate"), ("ai.model.id", "gpt-4o-mini"), ("ai.model.provider", "openai.chat"), ("ai.usage.promptTokens", "300"), ("ai.usage.completionTokens", "20"), ("ai.response.text", "Done."), ("ai.response.finishReason", "length"), ("ai.prompt.messages", "[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Summarise\"}]}]")]
      emit 5 "ai.toolCall" [("ai.operationId", "ai.toolCall"), ("ai.toolCall.name", "search_logs"), ("ai.toolCall.id", "call-9"), ("ai.toolCall.args", "{\"q\":\"timeout\"}")]
      rows <- dbQuery @_ @(Text, AE.Value) tr [sql| SELECT name, attributes->'gen_ai' FROM otel_logs_and_spans WHERE project_id = ? ORDER BY name |] (Only pid)
      let genAI name = fromMaybe AE.Null $ lookup name rows
          path ks v = foldl' (\acc k -> acc >>= (^? key k)) (Just v) ks
      [genAI n ^? key "operation" . key "name" . _String | n <- ["openinference llm", "openllmetry chat", "ai.generateText", "ai.generateText.doGenerate", "ai.toolCall"]]
        `shouldBe` map Just ["chat", "chat", "invoke_agent", "chat", "execute_tool"]
      -- gpt-4o: 1000 in at $2.50/M + 100 out at $10/M; sonnet: 500 uncached at $3, 1500 cached at $0.30, 50 out at $15.
      [path ["usage", "cost"] (genAI n) ^? traverse . _Number | n <- ["openinference llm", "openllmetry chat", "ai.generateText"]] `shouldBe` [Just 0.0035, Just 0.002700, Nothing]
      path ["usage", "cost_source"] (genAI "ai.generateText.doGenerate") `shouldBe` Just "estimated"
      path ["conversation", "id"] (genAI "openinference llm") `shouldBe` Just "conv-1"
      path ["conversation", "id"] (genAI "ai.generateText") `shouldBe` Just "conv-1"
      path ["provider", "name"] (genAI "ai.generateText.doGenerate") `shouldBe` Just "openai"
      genAI "openinference llm" ^.. key "input" . key "messages" . values . key "parts" . values . key "content" . _String `shouldBe` ["Why is checkout slow?"]
      genAI "openllmetry chat" ^.. key "output" . key "messages" . values . key "parts" . values . key "content" . _String `shouldBe` ["The pool is exhausted."]
      genAI "ai.generateText.doGenerate" ^.. key "input" . key "messages" . values . key "parts" . values . key "content" . _String `shouldBe` ["Summarise"]
      map (`path` genAI "ai.toolCall") [["tool", "name"], ["tool", "call", "id"]] `shouldBe` [Just "search_logs", Just "call-9"]
      -- Both conv-1 traces are one conversation; the lone OpenLLMetry call is only a call.
      let (from, to) = around' now
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.ConversationsBody table))) <- call tr $ LLM.llmGetH pid Nothing Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      map (\c -> (c.key, c.llmCalls, c.toolCalls, c.title, c.signals)) (toList table.rows) `shouldBe` [("conv-1", 2, 1, Just "Why is checkout slow?", [LLM.Truncated])]
      -- "LLM calls" is the Explorer over the kind=ai summary element, on both read paths.
      let llmCalls = def{Log.query = Just ("summary[*] == \"" <> Telemetry.genAIMarker <> "\""), Log.from = from, Log.to = to}
          viaTf' = tr{trATCtx = tr.trATCtx{env = tr.trATCtx.env{enableTimefusionReads = True}}}
      -- The TimeFusion lowering (array_has) only runs where CI provides a real TimeFusion.
      realTf <- isJust <$> lookupEnv "TIMEFUSION_PG_TEST_URL"
      forM_ (tr : [viaTf' | realTf]) \t -> call t (Log.logExplorerDataH pid llmCalls) >>= (`shouldBe` 5) . V.length . (.logsData)
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.TopicsBody topics))) <- call tr $ LLM.llmGetH pid (Just "topics") Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      map (\t -> (t.template, length t.conversations)) (toList topics.rows) `shouldBe` [("Why is checkout slow?", 1)]
      -- Rows open the full span view in the drawer: a partial response is a lone hidden tab panel.
      T.isInfixOf "partial" (renderT table) `shouldBe` False
      -- The trace view prices each step and totals the trace: $0.0035 + $0.000057 for gpt-4o-mini.
      [(trId, ts)] <- dbQuery tr [sql| SELECT context___trace_id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND name = 'openinference llm' |] (Only pid)
      renderT <$> call tr (TelemetryPage.traceH pid trId (Just ts) Nothing Nothing Nothing Nothing) >>= (`shouldContainAll` ["1K → 100 tok · $0.0035", "LLM cost", "$0.0036"])

    it "keeps each trace-less Claude Code session its own conversation, and leaves look-alike app logs alone" \tr -> do
      pid <- createTestProject tr "Claude Code"
      apiKey <- createTestAPIKey tr pid "claude-code"
      now <- getCurrentTime
      let event n session promptId attrs = ingestLogRecord tr apiKey ("INFO", PL.SEVERITY_NUMBER_INFO) ("claude_code." <> fromMaybe "" (lookup "event.name" attrs)) ([("session.id", session), ("prompt.id", promptId)] <> attrs) (addUTCTime n now)
      event 1 "sess-a" "p1" [("event.name", "user_prompt"), ("prompt", "Fix the failing build")]
      event 2 "sess-a" "p1" [("event.name", "api_request"), ("model", "claude-sonnet-4-5"), ("input_tokens", "10"), ("cache_read_tokens", "5000"), ("output_tokens", "20"), ("cost_usd", "0.0021")]
      event 3 "sess-b" "p2" [("event.name", "user_prompt"), ("prompt", "Write the tests")]
      event 4 "sess-b" "p2" [("event.name", "api_request"), ("model", "claude-sonnet-4-5"), ("input_tokens", "100"), ("output_tokens", "900"), ("cost_usd", "0.5")]
      ingestLogRecord tr apiKey ("ERROR", PL.SEVERITY_NUMBER_ERROR) "checkout failed" [("event.name", "api_error"), ("session.id", "sess-a")] (addUTCTime 5 now)
      let (from, to) = around' now
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.ConversationsBody table))) <- call tr $ LLM.llmGetH pid Nothing Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      map (\c -> (c.key, c.title, c.llmCalls, c.tokens, c.cost)) (toList table.rows) `shouldBe` [("sess-b", Just "Write the tests", 1, 1000, Just 0.5), ("sess-a", Just "Fix the failing build", 1, 5030, Just 0.0021)]
      renderT <$> call tr (LLM.conversationGetH pid "sess-a" from to Nothing) >>= (`shouldContainAll` ["Fix the failing build", "$0.0021"])

    it "observes Monoscope's own assistant: an agent run, its model calls in order, and its tools" \tr -> do
      apiKey <- createTestAPIKey tr testPid "self-telemetry"
      answer <- withOwnTelemetry tr apiKey \traced ->
        runTestBg frozenTime traced $ Bot.processAIQuery Nothing False AI.ServiceAccess testPid "plot error trend over time" Nothing tr.trATCtx.env.openaiModel tr.trATCtx.env.openaiApiKey
      answer `shouldSatisfy` isRight
      spans <- dbQuery @_ @(Text, AE.Value) tr [sql| SELECT name, attributes->'gen_ai' FROM otel_logs_and_spans WHERE project_id = ? AND attributes->'gen_ai' IS NOT NULL ORDER BY start_time |] (Only testPid)
      let ops = map (\(_, g) -> g ^? key "operation" . key "name" . _String) spans
          chats = [g | (_, g) <- spans, g ^? key "operation" . key "name" . _String == Just "chat"]
      take 1 ops `shouldBe` [Just "invoke_agent"]
      length chats `shouldSatisfy` (>= 2)
      Just "execute_tool" `shouldSatisfy` (`elem` ops)
      [n | (n, g) <- spans, g ^? key "agent" . key "name" . _String == Just "monoscope"] `shouldBe` ["invoke_agent monoscope"]
      -- Each call carries the conversation so far, opening with the system prompt.
      chats ^.. traverse . key "input" . key "messages" . values . key "role" . _String `shouldSatisfy` (\roles -> take 1 roles == ["system"] && "user" `elem` roles)
      chats ^.. traverse . key "output" . key "messages" . values . key "role" . _String `shouldSatisfy` all (== "assistant")
      -- The same run, read back through the pages a user would open.
      -- Through TimeFusion reads, which CI points at a real TimeFusion.
      (from, to) <- around' <$> getCurrentTime
      let viaTf = tr{trATCtx = tr.trATCtx{env = tr.trATCtx.env{enableTimefusionReads = True}}}
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.ConversationsBody table))) <- call viaTf $ LLM.llmGetH testPid Nothing Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      run <- maybe (fail "the assistant run is not listed as a conversation") pure $ viaNonEmpty head $ toList table.rows
      (run.agents, run.title, run.llmCalls >= 2, run.toolCalls >= 1) `shouldBe` (["monoscope"], Just "plot error trend over time", True, True)
      detail <- call viaTf $ LLM.conversationGetH testPid run.key from to Nothing
      renderT detail `shouldContainAll` ["plot error trend over time", "System instructions", "Ask AI about this conversation", "Transcript", "Timeline", "execute_tool"]
      [(chatId, chatTs)] <- dbQuery tr [sql| SELECT id::text, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND attributes->'gen_ai'->'operation'->>'name' = 'chat' ORDER BY start_time LIMIT 1 |] (Only testPid)
      chatUuid <- maybe (fail "span id is not a uuid") pure $ UUID.fromText chatId
      drawer <- call tr $ LogItem.expandAPIlogItemH testPid chatUuid chatTs Nothing Nothing Nothing False
      renderT drawer `shouldContainAll` ["tab-genai", "Conversation", "plot error trend over time"]


-- | An explicit window around the ingested spans, which carry real timestamps.
around' :: UTCTime -> (Maybe Text, Maybe Text)
around' t = (Just $ toText $ iso8601Show $ addUTCTime (-3600) t, Just $ toText $ iso8601Show $ addUTCTime 3600 t)
