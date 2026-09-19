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
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Effectful qualified as Eff
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid qualified
import Models.Apis.Issues qualified as Issues
import Pages.AIThreads qualified as AIThreads
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

      it "exposes project repository reads while keeping actions permission-gated" \tr -> do
        let toolNames config = do
              (_, _, params) <- Eff.runEff $ Time.runTime $ AI.agenticSetup config "any query" "model"
              pure $ AE.toJSON params ^.. key "tools" . values . key "function" . key "name" . _String
        readonly <- toolNames $ AI.defaultAgenticConfig testPid
        actionable <- toolNames (AI.defaultAgenticConfig testPid){AI.invocationMode = AI.InteractiveWithActions}
        scheduledReadOnly <- toolNames (AI.defaultAgenticConfig testPid){AI.invocationMode = AI.ScheduledRoutine (UUIDId UUID.nil) False}
        scheduledActionable <- toolNames (AI.defaultAgenticConfig testPid){AI.invocationMode = AI.ScheduledRoutine (UUIDId UUID.nil) True}
        repositoryAware <- toolNames (AI.defaultAgenticConfig testPid){AI.sourceConfig = Just tr.trATCtx.config}
        readonly `shouldNotContain` ["send_to_slack"]
        scheduledReadOnly `shouldNotContain` ["send_to_slack"]
        readonly `shouldContain` ["list_issues", "get_issue", "list_incidents", "get_incident", "list_monitors", "get_monitor", "list_endpoints", "get_endpoint", "list_log_patterns", "get_log_pattern", "list_dashboards", "get_dashboard", "get_project"]
        repositoryAware `shouldContain` ["get_code_context", "list_runbooks", "read_runbook", "get_linked_repositories", "get_deployments"]
        actionable `shouldContain` ["send_to_slack"]
        scheduledActionable `shouldContain` ["send_to_slack"]

    describe "Web conversation persistence" do
      it "assistantPages_defaultToNavigation" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml threadPage
        html `shouldSatisfy` T.isInfixOf "id=\"nav-view-menu\" class=\"sr-only peer/menu\" checked"

      it "issueConversation_linksBackToItsIssue" \tr -> do
        let issueId = UUIDId UUID.nil :: Issues.IssueId
            convId = UUIDId issueId.unUUIDId
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTAnomaly})|]
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml threadPage
        html `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/issues/" <> issueId.toText <> "\"")

      it "assistantSectionHeadings_linkAndBackdropCoversViewport" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
        (_, conversationPage) <- testServant tr $ AIThreads.threadsGetH testPid
        (_, routinesPage) <- testServant tr $ AIThreads.routinesGetH testPid
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId
        let render = LT.toStrict . Lucid.renderText . Lucid.toHtml
            conversationHtml = render conversationPage
        all (`T.isInfixOf` conversationHtml) ["href=\"/p/" <> testPid.toText <> "/ai/routines\"", "Routines", "href=\"/p/" <> testPid.toText <> "/ai\"", "Conversations"] `shouldBe` True
        all (`T.isInfixOf` conversationHtml) ["id=\"ai-composer-modal\"", "backdrop:backdrop-blur-sm"] `shouldBe` True
        for_ [conversationHtml, render routinesPage, render threadPage] (`shouldSatisfy` T.isInfixOf "data-header-actions")

      it "lists titled threads and schedules and pauses their routine" \tr -> do
        let convId = UUIDId UUID.nil
        (needsTitle, summary, listed, scheduled, paused, resumed, deleted) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          needsTitle <- Issues.conversationNeedsTitle testPid convId
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          Issues.setConversationTitle testPid convId "Daily reliability brief"
          summary <- Issues.getConversation testPid convId
          listed <- Issues.listConversations testPid
          scheduled <- Issues.upsertRoutine testPid convId $ fromRightShow $ Issues.mkRoutineInterval 15
          Issues.pauseRoutine testPid convId
          paused <- Issues.getConversation testPid convId
          resumed <- Issues.resumeRoutine testPid convId
          Issues.deleteRoutine testPid convId
          deleted <- Issues.getConversation testPid convId
          pure (needsTitle, summary, listed, scheduled, paused, resumed, deleted)
        needsTitle `shouldBe` True
        (.title) <$> summary `shouldBe` Just "Daily reliability brief"
        map (.conversationId) listed `shouldContain` [convId]
        (.scheduledAt) <$> scheduled `shouldBe` Just (addUTCTime 900 frozenTime)
        ((.routineActive) <$> paused, (.routineNextRunAt) =<< paused) `shouldBe` (Just False, Nothing)
        (.scheduledAt) <$> resumed `shouldBe` Just (addUTCTime 900 frozenTime)
        ((.routineInterval) =<< deleted) `shouldBe` Nothing

      it "installs a built-in routine at the next project-local wall-clock time" \tr -> do
        length Issues.routineTemplates `shouldSatisfy` (>= 15)
        case viaNonEmpty head Issues.routineTemplates of
          Nothing -> expectationFailure "routine catalog must not be empty"
          Just template -> do
            let convId = UUIDId UUID.nil
            (scheduled, installed, canAct, slackEnabled) <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              scheduled <- Issues.installRoutineTemplate testPid convId "Europe/Berlin" template
              initialCanAct <- maybe (pure False) (Issues.routineCanAct . (.id)) scheduled
              Issues.setRoutineDestination testPid convId Issues.DestinationSlack
              (scheduled,,initialCanAct,) <$> Issues.getConversation testPid convId <*> maybe (pure False) (Issues.routineCanAct . (.id)) scheduled
            (.scheduledAt) <$> scheduled `shouldBe` Just (addUTCTime (8 * 3600) frozenTime)
            ((.templateKey) =<< installed) `shouldBe` Just template.key
            canAct `shouldBe` False
            ((.routineDestination) =<< installed) `shouldBe` Just Issues.DestinationSlack
            slackEnabled `shouldBe` True

      it "rescheduling a template as an interval keeps that schedule after resume" \tr -> do
        case viaNonEmpty head Issues.routineTemplates of
          Nothing -> expectationFailure "routine catalog must not be empty"
          Just template -> do
            let convId = UUIDId UUID.nil
                interval = fromRightShow $ Issues.mkRoutineInterval 15
            resumed <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              void $ Issues.installRoutineTemplate testPid convId "Europe/Berlin" template
              void $ Issues.upsertRoutine testPid convId interval
              Issues.pauseRoutine testPid convId
              Issues.resumeRoutine testPid convId
            (.scheduledAt) <$> resumed `shouldBe` Just (addUTCTime 900 frozenTime)

      it "runs a routine once, completes its lease, and keeps the synthetic prompt out of history" \tr -> do
        executions <- IORef.newIORef ([] :: [AI.InvocationMode])
        successors <- IORef.newIORef ([] :: [(UUIDId "ai_routine", UTCTime)])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute config = do
              Eff.liftIO $ IORef.modifyIORef' executions (<> [config.invocationMode])
              pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Routine completed\",\"widgets\":[]}", AI.toolCalls = []}
            enqueueNext routineId at = Eff.liftIO $ IORef.modifyIORef' successors (<> [(routineId, at)])
        (history, routine, runRecord, recentRuns) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          scheduled <- Issues.upsertRoutine testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          -- A duplicate delivery for the completed scheduled instant must be a no-op.
          BackgroundJobs.runAIRoutineWith 1_000_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          (,,,)
            <$> Issues.selectChatHistory testPid convId
            <*> Issues.getConversation testPid convId
            <*> Hasql.interpOne @(Text, Bool, Bool, Maybe Text) [HI.sql|SELECT status, findings IS NOT NULL, actions IS NOT NULL, error FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|]
            <*> Issues.listRoutineRuns testPid 10
        IORef.readIORef executions >>= (`shouldSatisfy` \case [AI.ScheduledRoutine _ True] -> True; _ -> False)
        IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)
        map (\message -> (message.role, message.content)) history
          `shouldBe` [(Issues.ChatUser, "Send a daily reliability brief to Slack"), (Issues.ChatAssistant, "Routine completed")]
        ((.routineRunningSince) =<< routine) `shouldBe` Nothing
        ((.routineNextRunAt) =<< routine) `shouldSatisfy` isJust
        runRecord `shouldBe` Just ("succeeded", True, True, Nothing)
        map (\run -> (run.conversationId, run.status)) recentRuns `shouldBe` [(convId, Issues.RunSucceeded)]

      it "suppresses a findings-only routine answer when the agent reports no findings" \tr -> do
        case find ((== "error-regression-radar") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "missing findings-only template"
          Just template -> do
            let convId = UUIDId UUID.nil
                execute _ = pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Nothing material changed\",\"has_findings\":false,\"widgets\":[]}", AI.toolCalls = []}
            (history, runStatus) <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              Issues.insertChatMessage testPid convId Issues.ChatUser template.prompt Nothing Nothing
              scheduled <- Issues.installRoutineTemplate testPid convId "UTC" template >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
              BackgroundJobs.runAIRoutineWith 1_000_000 execute (\_ _ -> pass) tr.trATCtx scheduled.id scheduled.scheduledAt
              (,)
                <$> Issues.selectChatHistory testPid convId
                <*> (fmap fst <$> Hasql.interpOne @(Text, Bool) [HI.sql|SELECT status, TRUE FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|])
            map (.role) history `shouldBe` [Issues.ChatUser]
            runStatus `shouldBe` Just "no_findings"

      it "cancels an executing routine before its answer is published" \tr -> do
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute _ = do
              Issues.cancelRoutineRun testPid convId
              pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Do not publish\",\"has_findings\":true}", AI.toolCalls = []}
        (history, runStatus) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          scheduled <- Issues.upsertRoutine testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000_000 execute (\_ _ -> pass) tr.trATCtx scheduled.id scheduled.scheduledAt
          (,)
            <$> Issues.selectChatHistory testPid convId
            <*> (fmap fst <$> Hasql.interpOne @(Text, Bool) [HI.sql|SELECT status, TRUE FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|])
        history `shouldSatisfy` null
        runStatus `shouldBe` Just "cancelled"

      it "records a routine timeout as an execution event and completes the lease" \tr -> do
        successors <- IORef.newIORef ([] :: [UTCTime])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute _ = Eff.liftIO (threadDelay 50_000) $> Right AI.AgenticChatResult{AI.response = "late", AI.toolCalls = []}
            enqueueNext _ at = Eff.liftIO $ IORef.modifyIORef' successors (<> [at])
        (history, routine, runStatus) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          scheduled <- Issues.upsertRoutine testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
          BackgroundJobs.runAIRoutineWith 1_000 execute enqueueNext tr.trATCtx scheduled.id scheduled.scheduledAt
          (,,)
            <$> Issues.selectChatHistory testPid convId
            <*> Issues.getConversation testPid convId
            <*> (fmap fst <$> Hasql.interpOne @(Text, Bool) [HI.sql|SELECT status, TRUE FROM apis.ai_routine_runs WHERE routine_id = #{scheduled.id}|])
        map (.role) history `shouldBe` [Issues.ChatExecutionEvent]
        all (isNothing . AI.dbMessageToLLMMessage) history `shouldBe` True
        ((.routineRunningSince) =<< routine) `shouldBe` Nothing
        IORef.readIORef successors >>= (\items -> length items `shouldBe` 1)
        runStatus `shouldBe` Just "timed_out"

    describe "Live API calls (uses golden files)" do
      it "processes error trend query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "plot error trend over time" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON (agenticResp{AI.toolCalls = Nothing} :: AI.LLMResponse)
            assertJsonGolden "agentic/error_trend_response.json" responseJson
            length agenticResp.widgets `shouldSatisfy` (>= 0)

      it "processes service breakdown query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "show warning and error counts grouped by service" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON (agenticResp{AI.toolCalls = Nothing} :: AI.LLMResponse)
            assertJsonGolden "agentic/service_breakdown_response.json" responseJson

      it "processes explanation-only query and saves golden response" \tr -> do
        cleanupTelemetryData tr
        seedTelemetryData tr
        result <- runTestBg frozenTime tr $ processAIQuery Nothing False AI.ServiceAccess testPid "what services have the most errors?" Nothing (getOpenAIModel tr) (getOpenAIKey tr)
        case result of
          Left err -> expectationFailure $ "API call failed: " <> toString err
          Right agenticResp -> do
            let responseJson = AE.toJSON (agenticResp{AI.toolCalls = Nothing} :: AI.LLMResponse)
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
