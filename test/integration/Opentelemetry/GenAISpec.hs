-- | LLM telemetry end to end: every producer dialect lands as GenAI conventions with a cost,
-- and Monoscope's own assistant, traced and re-ingested, reads back as one agent run.
module Opentelemetry.GenAISpec (spec) where

import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, values, _Number, _String)
import Data.Effectful.LLM qualified as ELLM
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Dispatch.Dynamic (interpose)
import Langchain.LLM.Core qualified as Chat
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.Utils qualified as Bot
import Pages.Components (Deferred (..))
import Pages.LLM qualified as LLM
import Pages.LogExplorer.Log qualified as Log
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Telemetry qualified as TelemetryPage
import Pkg.AI qualified as AI
import Pkg.Components.Table (Table (..))
import Pkg.GenAI qualified as GenAI
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
      -- A producer's own total on the agent span must not be added to its model calls' cost.
      emit 3 "ai.generateText" [("ai.operationId", "ai.generateText"), ("ai.model.id", "gpt-4o-mini"), ("ai.usage.promptTokens", "300"), ("ai.telemetry.metadata.sessionId", "conv-1"), ("gen_ai.usage.cost", "9")]
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
      -- The canonical identity and usage land in the promoted columns, queryable without the attributes blob.
      dbQuery @_ @(Text, Maybe Text, Maybe Text, Maybe Int64, Maybe Double) tr [sql| SELECT name, attributes___gen_ai___operation___name, attributes___gen_ai___request___model, attributes___gen_ai___usage___input_tokens, attributes___gen_ai___usage___cost FROM otel_logs_and_spans WHERE project_id = ? AND name IN ('openinference llm', 'ai.toolCall') ORDER BY name |] (Only pid)
        `shouldReturn` [("ai.toolCall", Just "execute_tool", Nothing, Nothing, Nothing), ("openinference llm", Just "chat", Just "gpt-4o", Just 1000, Just 0.0035)]
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
      map (\c -> (c.key, c.llmCalls, c.toolCalls, c.title, c.signals)) (toList table.rows) `shouldBe` [("conv-1", 2, 1, Just "Why is checkout slow?", [LLM.Detected LLM.Truncated])]
      -- "LLM calls" is the Explorer over the promoted gen_ai.operation.name column, on both read paths.
      let llmCalls aiOnly = def{Log.query = Just $ "attributes.gen_ai.operation.name " <> bool "== null" "!= null" aiOnly, Log.from = from, Log.to = to}
      -- The TimeFusion read path only runs where CI provides a real TimeFusion.
      realTf <- isJust <$> lookupEnv "TIMEFUSION_PG_TEST_URL"
      forM_ (tr : [viaTf tr | realTf]) \t -> forM_ ([(True, 5), (False, 0)] :: [(Bool, Int)]) \(aiOnly, n) -> call t (Log.logExplorerDataH pid (llmCalls aiOnly)) >>= (`shouldBe` n) . V.length . (.logsData)
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.TopicsBody topics))) <- call tr $ LLM.llmGetH pid (Just "topics") Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      map (\t -> (t.template, length t.conversations)) (toList topics.rows) `shouldBe` [("Why is checkout slow?", 1)]
      -- A plain-language signal: one judge call over the day's conversations, matches shown as pills and a facet.
      Right LLM.SignalCreated{} <- call tr $ LLM.llmSignalPostH pid (LLM.SignalForm "Slow checkout" "The user asks why checkout is slow.")
      traverse (call tr . LLM.llmSignalPostH pid) [LLM.SignalForm " slow CHECKOUT " "again", LLM.SignalForm "Tool error" "clashes with a built-in", LLM.SignalForm "  " "blank"]
        `shouldReturn` [Left (LLM.SignalNameTaken "slow CHECKOUT"), Left (LLM.SignalNameTaken "Tool error"), Left LLM.SignalIncomplete]
      prompts <- newIORef []
      let judge = interpose @ELLM.LLM \_ -> \case
            ELLM.CallLLM _ prompt _ -> modifyIORef prompts (prompt :) $> Right "```json\n[{\"key\":\"conv-1\",\"match\":true,\"reason\":\"Asks about checkout latency.\"}]\n```"
            ELLM.CallAgenticChat{} -> pure $ Left "Unexpected agent call"
            ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
      runTestBg (addUTCTime 60 now) tr $ judge $ LLM.evaluateSignals pid
      runTestBg (addUTCTime 60 now) tr $ judge $ LLM.evaluateSignals pid
      readIORef prompts >>= (`shouldSatisfy` \ps -> length ps == 1 && all ("Why is checkout slow?" `T.isInfixOf`) ps)
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.ConversationsBody flagged))) <- call tr $ LLM.llmGetH pid Nothing Nothing Nothing (Just "Slow checkout") Nothing from to Nothing (Just "1")
      (map (.key) (toList flagged.rows), renderT flagged) `shouldSatisfy` \(ks, html) -> ks == ["conv-1"] && "Asks about checkout latency." `T.isInfixOf` html
      -- The verdict reaches the conversation page too.
      renderT <$> call tr (LLM.conversationGetH pid "conv-1" from to Nothing) >>= (`shouldContainAll` ["Slow checkout", "Asks about checkout latency."])
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.SignalsBody signals))) <- call tr $ LLM.llmGetH pid (Just "signals") Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      map (\s' -> (s'.name, s'.matched, s'.judged)) (toList signals.rows) `shouldBe` [("Slow checkout", 1, 1)]
      -- Rows open the full span view in the drawer: a partial response is a lone hidden tab panel.
      T.isInfixOf "partial" (renderT table) `shouldBe` False
      -- A trace that only ran a workflow is a conversation, so its trace page links to it.
      emitIn "9c3d" 7 "pipeline" [("traceloop.span.kind", "workflow"), ("traceloop.entity.name", "pipeline")]
      [(wfTrace, wfTs)] <- dbQuery tr [sql| SELECT context___trace_id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND name = 'pipeline' |] (Only pid)
      renderT <$> call tr (TelemetryPage.traceH pid wfTrace (Just wfTs) Nothing Nothing Nothing Nothing (Just "true")) >>= (`shouldContainAll` ["View conversation"])
      -- "Open trace" is a plain navigation: it gets the page shell and its stylesheet, not a bare fragment.
      renderT <$> call tr (TelemetryPage.traceH pid wfTrace (Just wfTs) Nothing Nothing Nothing Nothing Nothing) >>= (`shouldContainAll` ["<html", "tailwind.min", "Trace Breakdown", "View conversation"])
      -- The trace view prices each step and totals the model calls only: $0.0035 + $0.000057 for gpt-4o-mini.
      [(trId, ts)] <- dbQuery tr [sql| SELECT context___trace_id, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND name = 'openinference llm' |] (Only pid)
      renderT <$> call tr (TelemetryPage.traceH pid trId (Just ts) Nothing Nothing Nothing Nothing (Just "true")) >>= (`shouldContainAll` ["1K → 100 tok · $0.0035", "LLM cost", "$0.0036", "View conversation", "/llm/conversations/conv-1"])

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

    it "bills the assistant from the provider's reported usage, not an estimate" \tr -> do
      let provider = interpose @ELLM.LLM \_ -> \case
            ELLM.CallAgenticChat{} -> pure $ Right $ ELLM.Reply (Chat.Message Chat.Assistant "The pool is exhausted." Chat.defaultMessageData) (Just $ GenAI.Usage 1200 340 800 0)
            ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
            ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
      runTestBg frozenTime tr (provider $ AI.runAgenticChatWithHistory (AI.defaultAgenticConfig testPid) "Why is checkout slow?" "model" "key") >>= (`shouldSatisfy` isRight)
      dbQuery @_ @(Int64, Int64) tr [sql| SELECT input_tokens, output_tokens FROM projects.ai_usage WHERE project_id = ? |] (Only testPid) `shouldReturn` [(1200, 340)]

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
      LLM.LLMPage (PageCtx _ (DeferredBody (LLM.ConversationsBody table))) <- call (viaTf tr) $ LLM.llmGetH testPid Nothing Nothing Nothing Nothing Nothing from to Nothing (Just "1")
      run <- maybe (fail "the assistant run is not listed as a conversation") pure $ viaNonEmpty head $ toList table.rows
      (run.agents, run.title, run.llmCalls >= 2, run.toolCalls >= 1) `shouldBe` (["monoscope"], Just "plot error trend over time", True, True)
      detail <- call (viaTf tr) $ LLM.conversationGetH testPid run.key from to Nothing
      renderT detail `shouldContainAll` ["plot error trend over time", "System instructions", "Ask AI about this conversation", "Transcript", "Timeline", "Cost by model", "Tools", "Open trace"]
      [(chatId, chatTs)] <- dbQuery tr [sql| SELECT id::text, timestamp FROM otel_logs_and_spans WHERE project_id = ? AND attributes->'gen_ai'->'operation'->>'name' = 'chat' ORDER BY start_time LIMIT 1 |] (Only testPid)
      chatUuid <- maybe (fail "span id is not a uuid") pure $ UUID.fromText chatId
      drawer <- call tr $ LogItem.expandAPIlogItemH testPid chatUuid chatTs Nothing Nothing Nothing False
      renderT drawer `shouldContainAll` ["tab-genai", "Conversation", "plot error trend over time"]


-- | An explicit window around the ingested spans, which carry real timestamps.
around' :: UTCTime -> (Maybe Text, Maybe Text)
around' t = (Just $ toText $ iso8601Show $ addUTCTime (-3600) t, Just $ toText $ iso8601Show $ addUTCTime 3600 t)


-- | The same resources reading through TimeFusion.
viaTf :: TestResources -> TestResources
viaTf tr = tr{trATCtx = tr.trATCtx{env = tr.trATCtx.env{enableTimefusionReads = True}}}
