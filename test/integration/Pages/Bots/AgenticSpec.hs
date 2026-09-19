module Pages.Bots.AgenticSpec (spec) where

import BackgroundJobs qualified
import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..))
import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, values, _Array, _String)
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Effectful qualified as Eff
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.Issues qualified as Issues
import Pages.Bots.BotTestHelpers (assertJsonGolden, getOpenAIKey, getOpenAIModel)
import Pages.Bots.SeedTestData (cleanupTelemetryData, seedTelemetryData)
import Pages.Bots.Utils (processAIQuery)
import Pkg.AI qualified as AI
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldContain, shouldNotContain, shouldSatisfy)
import UnliftIO.Exception (throwIO)


spec :: Spec
spec = around withTestResources do
  describe "Agentic Query Processing" do
    describe "Response parsing" do
      it "parses widget response" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = """{"explanation": "Here are the errors", "widgets": [{"type": "timeseries", "query": "level == ERROR", "title": "Error Trend"}], "suggested_query": "level == ERROR"}"""
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            length parsed.widgets `shouldBe` 1
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "handles text-only response" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = """{"explanation": "Your error rate is 5%", "widgets": []}"""
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            null parsed.widgets `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "preserves tool calls from result" \_ -> do
        let toolCall =
              AI.ToolCallInfo
                { AI.name = "run_query"
                , AI.args = mempty
                , AI.resultPreview = "10 rows"
                , AI.rawData = Nothing
                }
            rawResult =
              AI.AgenticChatResult
                { AI.response = "{\"explanation\": \"done\"}"
                , AI.toolCalls = [toolCall]
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> length (fromMaybe [] parsed.toolCalls) `shouldBe` 1
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      it "handles malformed JSON gracefully" \_ -> do
        let rawResult =
              AI.AgenticChatResult
                { AI.response = "This is not JSON, just a text response"
                , AI.toolCalls = []
                }
        case AI.parseAgenticResponse rawResult of
          Right parsed -> do
            isJust parsed.explanation `shouldBe` True
            null parsed.widgets `shouldBe` True
          Left err -> expectationFailure $ "Parse failed: " <> toString err

      -- gpt-5.6-luna rejects function tools + reasoning_effort on /v1/chat/completions (400);
      -- tool-bearing params must downgrade a configured effort to "none"
      it "agenticSetup_toolsWithEffort_sendsEffortNone" \_ -> do
        (_, _, params) <- Eff.runEff $ Time.runTime $ AI.agenticSetup (AI.defaultAgenticConfig testPid) "any query" "gpt-5.6-luna#high"
        let body = AE.toJSON params
        isJust (body ^? key "tools" . _Array) `shouldBe` True
        (body ^? key "reasoning_effort") `shouldBe` Just (AE.String "none")

      it "exposes integration actions only to action-enabled conversations" \_ -> do
        let toolNames config = do
              (_, _, params) <- Eff.runEff $ Time.runTime $ AI.agenticSetup config "any query" "model"
              pure $ AE.toJSON params ^.. key "tools" . values . key "function" . key "name" . _String
        readonly <- toolNames $ AI.defaultAgenticConfig testPid
        actionable <- toolNames (AI.defaultAgenticConfig testPid){AI.invocationMode = AI.InteractiveWithActions}
        readonly `shouldNotContain` ["send_to_slack"]
        actionable `shouldContain` ["send_to_slack"]

    describe "Web conversation persistence" do
      it "lists titled threads and schedules and pauses their routine" \tr -> do
        let convId = UUIDId UUID.nil
        (needsTitle, summary, listed, scheduled, paused) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          needsTitle <- Issues.conversationNeedsTitle testPid convId
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          Issues.setConversationTitle testPid convId "Daily reliability brief"
          summary <- Issues.getConversation testPid convId
          listed <- Issues.listConversations testPid
          scheduled <- Issues.upsertRoutine testPid convId $ fromRightShow $ Issues.mkRoutineInterval 15
          Issues.pauseRoutine testPid convId
          paused <- Issues.getConversation testPid convId
          pure (needsTitle, summary, listed, scheduled, paused)
        needsTitle `shouldBe` True
        (.title) <$> summary `shouldBe` Just "Daily reliability brief"
        map (.conversationId) listed `shouldContain` [convId]
        (.scheduledAt) <$> scheduled `shouldBe` Just (addUTCTime 900 frozenTime)
        ((.routineActive) <$> paused, (.routineNextRunAt) =<< paused) `shouldBe` (Just False, Nothing)

      it "runs a routine once, completes its lease, and keeps the synthetic prompt out of history" \tr -> do
        executions <- IORef.newIORef ([] :: [AI.InvocationMode])
        successors <- IORef.newIORef ([] :: [(UUIDId "ai_routine", UTCTime)])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute config = do
              Eff.liftIO $ IORef.modifyIORef' executions (<> [config.invocationMode])
              pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Routine completed\",\"widgets\":[]}", AI.toolCalls = []}
            enqueueNext routineId at = Eff.liftIO $ IORef.modifyIORef' successors (<> [(routineId, at)])
        (history, routine) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          scheduled <- Issues.upsertRoutine testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          -- A duplicate delivery for the completed scheduled instant must be a no-op.
          BackgroundJobs.runAIRoutineWith 1_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          (,) <$> Issues.selectChatHistory testPid convId <*> Issues.getConversation testPid convId
        IORef.readIORef executions >>= (`shouldBe` [AI.ScheduledRoutine])
        IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)
        map (\message -> (message.role, message.content)) history
          `shouldBe` [(Issues.ChatUser, "Send a daily reliability brief to Slack"), (Issues.ChatAssistant, "Routine completed")]
        ((.routineRunningSince) =<< routine) `shouldBe` Nothing
        ((.routineNextRunAt) =<< routine) `shouldSatisfy` isJust

      it "records a timeout as an execution event and completes the lease" \tr -> do
        successors <- IORef.newIORef ([] :: [UTCTime])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute _ = Eff.liftIO (threadDelay 50_000) $> Right AI.AgenticChatResult{AI.response = "late", AI.toolCalls = []}
            enqueueNext _ at = Eff.liftIO $ IORef.modifyIORef' successors (<> [at])
        (history, routine) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          scheduled <- Issues.upsertRoutine testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          (,) <$> Issues.selectChatHistory testPid convId <*> Issues.getConversation testPid convId
        map (.role) history `shouldBe` [Issues.ChatExecutionEvent]
        all (isNothing . AI.dbMessageToLLMMessage) history `shouldBe` True
        ((.routineRunningSince) =<< routine) `shouldBe` Nothing
        IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)

    describe "Live API calls (uses golden files)" do
      it "processes error trend query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "plot error trend over time" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON agenticResp
            assertJsonGolden "agentic/error_trend_response.json" responseJson
            length agenticResp.widgets `shouldSatisfy` (>= 0)

      it "processes service breakdown query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "show warning and error counts grouped by service" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON agenticResp
            assertJsonGolden "agentic/service_breakdown_response.json" responseJson

      it "processes explanation-only query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "what services have the most errors?" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON agenticResp
            assertJsonGolden "agentic/explanation_only_response.json" responseJson
            isJust agenticResp.explanation || not (null agenticResp.widgets) `shouldBe` True

      it "handles empty API key gracefully" \tr -> do
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "show errors" Nothing (getOpenAIModel tr) ""
        case result of
          Left err -> T.isInfixOf "unavailable" err || T.isInfixOf "error" (T.toLower err) `shouldBe` True
          Right _ -> pass
        cleanupTelemetryData tr

    describe "Widget URL generation" do
      it "generates signed widget PNG URLs correctly" \tr -> do
        let widget = def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Test Chart", Widget.query = Just "service == \"api\""}
            secret = tr.trATCtx.env.apiKeyEncryptionSecretKey
            baseUrl = tr.trATCtx.env.hostUrl

        url <- liftIO $ Eff.runEff $ Logging.runLog "test" tr.trLogger tr.trATCtx.config.logLevel $ Widget.widgetPngUrl secret baseUrl testPid widget Nothing Nothing Nothing

        url `shouldSatisfy` (not . T.null)
        url `shouldSatisfy` T.isInfixOf "widgetZ="
        url `shouldSatisfy` T.isInfixOf "&sig="

      it "rejects URLs exceeding 8000 characters" \tr -> do
        -- gzip compresses repeated chars well, so use unique numbered strings that resist compression
        let incompressible = T.concat [show @Text i <> "abcdefg" | i <- [1 .. 3000 :: Int]]
            hugeWidget = def{Widget.wType = Widget.WTTable, Widget.title = Just incompressible, Widget.query = Just incompressible}
            secret = tr.trATCtx.env.apiKeyEncryptionSecretKey
            baseUrl = tr.trATCtx.env.hostUrl

        url <- liftIO $ Eff.runEff $ Logging.runLog "test" tr.trLogger tr.trATCtx.config.logLevel $ Widget.widgetPngUrl secret baseUrl testPid hugeWidget Nothing Nothing Nothing

        url `shouldBe` ""
