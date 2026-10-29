module Pages.Bots.AgenticSpec (spec) where

import BackgroundJobs qualified
import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..))
import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, values, _Array, _String)
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as ELLM
import Data.IORef qualified as IORef
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful qualified as Eff
import Effectful.Concurrent.Async (concurrently_)
import Effectful.Dispatch.Dynamic (interpose)
import Effectful.Error.Static (catchError)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Langchain.LLM.Core qualified as LLM
import Lucid qualified
import Models.Apis.Investigations (ToolCallInfo (..), ToolResult (..))
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Pages.AIThreads qualified as AIThreads
import Pages.Bots.BotTestHelpers (assertJsonGolden, getOpenAIKey, getOpenAIModel)
import Pages.Bots.SeedTestData (cleanupTelemetryData, seedTelemetryData)
import Pages.Bots.Utils (processAIQuery)
import Pkg.AI qualified as AI
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant (ServerError (..))
import Servant.API.ResponseHeaders (getResponse)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import System.Types (ATBackgroundCtx)
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldContain, shouldNotContain, shouldSatisfy)
import UnliftIO.Exception (throwIO, tryAny)
import Web.FormUrlEncoded (urlDecodeAsForm)


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

      it "starts with project tool discovery while keeping actions permission-gated" \tr -> do
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
        readonly `shouldSatisfy` (\names -> all (`elem` names) ["search_project_tools", "get_project_tool_schema", "list_metrics"])
        readonly `shouldSatisfy` (\names -> all (`notElem` names) ["list_issues", "get_issue", "list_incidents", "get_incident", "list_monitors", "get_monitor", "list_endpoints", "get_endpoint", "list_log_patterns", "get_log_pattern", "list_dashboards", "get_dashboard", "get_project"])
        repositoryAware `shouldContain` ["get_code_context", "list_runbooks", "read_runbook", "get_linked_repositories", "get_deployments"]
        actionable `shouldContain` ["send_to_slack"]
        scheduledActionable `shouldContain` ["send_to_slack"]

    describe "Web conversation persistence" do
      it "requires an explicit per-message grant for project changes" \tr -> do
        let absent = urlDecodeAsForm @AIThreads.AIChatForm "query=check+metrics&mode=chat"
            granted = urlDecodeAsForm @AIThreads.AIChatForm "query=acknowledge+issue&mode=chat&allowActions=true"
        fmap (.allowActions) absent `shouldBe` Right Nothing
        fmap (.allowActions) granted `shouldBe` Right (Just True)
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid (UUIDId UUID.nil) Nothing Nothing
        (LT.toStrict $ Lucid.renderText $ Lucid.toHtml threadPage) `shouldSatisfy` T.isInfixOf "name=\"allowActions\""

      it "assistantPages_defaultToAssistant" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml threadPage
        html `shouldSatisfy` T.isInfixOf "id=\"nav-view-ai\" class=\"sr-only\" checked"

      it "issueConversation_linksBackToItsIssue" \tr -> do
        let issueId = UUIDId UUID.nil :: Issues.IssueId
            convId = UUIDId issueId.unUUIDId
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTAnomaly})|]
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml threadPage
        html `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/issues/" <> issueId.toText <> "\"")

      it "assistantSectionHeadings_linkAndBackdropCoversViewport" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
        (_, conversationPage) <- testServant tr $ AIThreads.threadsGetH testPid
        (_, routinesPage) <- testServant tr $ AIThreads.routinesGetH testPid
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let render = LT.toStrict . Lucid.renderText . Lucid.toHtml
            conversationHtml = render conversationPage
        all (`T.isInfixOf` conversationHtml) ["href=\"/p/" <> testPid.toText <> "/ai/routines\"", "Routines", "href=\"/p/" <> testPid.toText <> "/ai\"", "Conversations"] `shouldBe` True
        all (`T.isInfixOf` conversationHtml) ["id=\"ai-composer-modal\"", "backdrop:backdrop-blur-sm"] `shouldBe` True
        for_ [conversationHtml, render routinesPage, render threadPage] (`shouldSatisfy` T.isInfixOf "data-header-actions")

      it "eventRoutineActivation_appearsInstalledWithoutAnInterval" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        conversations <- runTestBg frozenTime tr $ Issues.listConversations testPid
        conversation <- maybe (fail "Missing Slack routine") pure $ find ((== Just "slack-investigation") . (.templateKey)) conversations
        (_, page) <- testServant tr $ AIThreads.routinesGetH testPid
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
        html `shouldSatisfy` T.isInfixOf ("href=\"/p/" <> testPid.toText <> "/ai/" <> conversation.conversationId.toText <> "\"")
        html `shouldSatisfy` T.isInfixOf "1 routine"
        (_, thread) <- testServant tr $ AIThreads.threadGetH testPid conversation.conversationId Nothing Nothing
        (LT.toStrict $ Lucid.renderText $ Lucid.toHtml thread) `shouldSatisfy` T.isInfixOf "Routine instructions"

        void $ testServant tr $ AIThreads.routinePostH testPid conversation.conversationId (AIThreads.RoutineForm $ fromRightShow $ Issues.mkRoutineInterval 15)
        unchanged <- runQueryEffect tr $ Issues.getConversation testPid conversation.conversationId
        (unchanged >>= (.routineKind)) `shouldBe` Just Issues.ScheduleSlackMessage

      it "eventRoutineCancellation_stopsActiveRunAndRetainsSnapshotsAcrossRetry" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        (routine, run) <- runTestBg frozenTime tr do
          routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing routine") pure
          run <- Issues.startEventRun routine (Issues.SlackMessage $ UUIDId UUID.nil) Issues.ContinueRun routine.conversationId (getResponse tr.trSessAndHeader).user.id "Investigate checkout" >>= maybe (throwIO $ ErrorCall "Missing run") pure
          Issues.saveRunResult testPid run.id "Investigation" "Original evidence"
          pure (routine, run)
        void $ testServant tr $ AIThreads.routineCancelPostH testPid routine.conversationId
        active <- runQueryEffect tr $ Issues.eventRunActive testPid run.id
        active `shouldBe` False
        runQueryEffect tr $ Issues.finishRoutineRun run.id run.startedAt Issues.CompletedCancelled Nothing Nothing (Just "Cancelled by member")
        void $ testServant tr $ AIThreads.routineInstructionsPostH testPid routine.conversationId (AIThreads.RoutineInstructionsForm "Check the new deployment first.")
        void $ testServant tr $ AIThreads.routineResumePostH testPid routine.conversationId
        retry <- runTestBg frozenTime tr do
          retry <- Issues.startEventRun routine (Issues.SlackMessage $ UUIDId UUID.nil) Issues.RestartRun routine.conversationId (getResponse tr.trSessAndHeader).user.id "Investigate checkout again" >>= maybe (throwIO $ ErrorCall "Missing retry") pure
          Issues.saveRunResult testPid retry.id "Investigation" "Revised evidence"
          Issues.finishRoutineRun retry.id retry.startedAt Issues.CompletedSucceeded Nothing Nothing Nothing
          pure retry
        (original, revised, oldInstructions, newInstructions, runs, duplicate) <-
          runTestBg frozenTime tr
            $ (,,,,,)
            <$> Issues.listRunArtifacts testPid run.id
            <*> Issues.listRunArtifacts testPid retry.id
            <*> Issues.getRoutineRunEvidence testPid run.id
            <*> Issues.getRoutineRunEvidence testPid retry.id
            <*> Issues.listConversationRuns testPid routine.conversationId 20
            <*> Issues.startEventRun routine (Issues.SlackMessage $ UUIDId UUID.nil) Issues.ContinueRun routine.conversationId (getResponse tr.trSessAndHeader).user.id "Replay"
        map (.content) original `shouldBe` [Just "Original evidence"]
        map (.content) revised `shouldBe` [Just "Revised evidence"]
        (oldInstructions >>= (.instructions)) `shouldBe` Just routine.instructions
        (newInstructions >>= (.instructions)) `shouldBe` Just "Check the new deployment first."
        map (.attempt) runs `shouldBe` [2, 1]
        fmap (.id) duplicate `shouldBe` Just retry.id
        (_, page) <- testServant tr $ AIThreads.routineRunGetH testPid run.id
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
        for_ ["Original evidence", "Cancelled", "Investigate checkout", "Instructions used for this run"] $ \text -> html `shouldSatisfy` T.isInfixOf text
        html `shouldSatisfy` (not . T.isInfixOf "Revised evidence")
        failed <- runTestBg frozenTime tr do
          failed <- Issues.startEventRun routine (Issues.SlackMessage $ UUIDId UUID.nil) Issues.RestartRun routine.conversationId (getResponse tr.trSessAndHeader).user.id "Try again" >>= maybe (throwIO $ ErrorCall "Missing failed attempt") pure
          Issues.finishRoutineRun failed.id failed.startedAt Issues.CompletedFailed Nothing Nothing (Just "Model unavailable")
          Issues.listRunArtifacts testPid failed.id
        map (.content) failed `shouldBe` []
        other <- createTestProject tr "Other routine project"
        hidden <- runAuthHandler tr $ (AIThreads.routineRunGetH other run.id $> (200 :: Int)) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
        hidden `shouldBe` 404
        pending <-
          runTestBg frozenTime tr
            $ Issues.startEventRun routine (Issues.SlackMessage $ UUIDId UUID.nil) Issues.RestartRun routine.conversationId (getResponse tr.trSessAndHeader).user.id "Work finishing during deletion"
            >>= maybe (throwIO $ ErrorCall "Missing pending attempt") pure
        runQueryEffect tr do
          Issues.deleteConversation testPid routine.conversationId
          Issues.saveRunResult testPid pending.id "Retained result" "The published result survives conversation deletion."
          Issues.insertRunResultMessage testPid pending.id "The published result survives conversation deletion."
          Issues.finishRoutineRun pending.id pending.startedAt Issues.CompletedSucceeded Nothing Nothing Nothing
        retainedArtifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid pending.id
        map (.content) retainedArtifacts `shouldBe` [Just "The published result survives conversation deletion."]
        (_, retained) <- testServant tr $ AIThreads.routineRunGetH testPid run.id
        let retainedHtml = LT.toStrict $ Lucid.renderText $ Lucid.toHtml retained
        retainedHtml `shouldSatisfy` T.isInfixOf "Original evidence"
        retainedHtml `shouldSatisfy` (not . T.isInfixOf "Open conversation and follow up")
        (_, recent) <- testServant tr $ AIThreads.routinesGetH testPid
        (LT.toStrict $ Lucid.renderText $ Lucid.toHtml recent) `shouldSatisfy` T.isInfixOf ("/ai/runs/" <> run.id.toText)

      it "eventRoutinePermissions_readersInspectButCannotActivateOrEdit" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        conversations <- runQueryEffect tr $ Issues.listConversations testPid
        conversation <- maybe (fail "Missing routine") pure $ find ((== Just "slack-investigation") . (.templateKey)) conversations
        let uid = (getResponse tr.trSessAndHeader).user.id
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = #{testPid} AND user_id = #{uid}|]
        (_, page) <- testServant tr $ AIThreads.routinesGetH testPid
        (_, thread) <- testServant tr $ AIThreads.threadGetH testPid conversation.conversationId Nothing Nothing
        let render = LT.toStrict . Lucid.renderText . Lucid.toHtml
        render page `shouldSatisfy` T.isInfixOf "1 routine"
        render page `shouldSatisfy` (not . T.isInfixOf "Add routine")
        render thread `shouldSatisfy` T.isInfixOf "Routine instructions"
        render thread `shouldSatisfy` (not . T.isInfixOf "Save instructions")
        for_ [AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "production-impact-review"), AIThreads.routineInstructionsPostH testPid conversation.conversationId (AIThreads.RoutineInstructionsForm "Overwrite"), AIThreads.routineResumePostH testPid conversation.conversationId] $ \handler -> do
          result <- runAuthHandler tr $ (handler $> 200) `catchError` \_ (err :: ServerError) -> pure err.errHTTPCode
          result `shouldBe` 403
        runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventSlackMessage) >>= (`shouldSatisfy` isNothing)

      it "eventRoutineAuthorization_rechecksAccountProjectAndPlanAccess" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        (routine, run, actor) <- runTestBg frozenTime tr do
          routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing routine") pure
          actor <- Projects.createUser "Routine" "Reader" "" "routine-reader@example.test"
          Projects.insertUser actor
          Hasql.interpExecute_ [HI.sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (#{testPid}, #{actor.id}, 'view')|]
          run <- Issues.startEventRun routine (Issues.SlackCommand routine.conversationId) Issues.StartRun routine.conversationId actor.id "Inspect checkout" >>= maybe (throwIO $ ErrorCall "Missing run") pure
          pure (routine, run, actor)
        runQueryEffect tr (Issues.eventRunActive testPid run.id) >>= (`shouldBe` True)
        for_ [([HI.sql|UPDATE users.users SET active = FALSE WHERE id = #{routine.requestedBy}|], [HI.sql|UPDATE users.users SET active = TRUE WHERE id = #{routine.requestedBy}|]), ([HI.sql|UPDATE projects.projects SET active = FALSE WHERE id = #{testPid}|], [HI.sql|UPDATE projects.projects SET active = TRUE WHERE id = #{testPid}|])] $ \(disable, restore) -> do
          runQueryEffect tr $ Hasql.interpExecute_ disable
          runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventSlackMessage) >>= (`shouldSatisfy` isNothing)
          runQueryEffect tr (Issues.eventRunActive testPid run.id) >>= (`shouldBe` False)
          tryAny (runTestBg frozenTime tr $ AI.requireAgentAccess (AI.MemberAccess routine.requestedBy) testPid) >>= (`shouldSatisfy` isLeft)
          runQueryEffect tr $ Hasql.interpExecute_ restore
        runQueryEffect tr $ Hasql.interpExecute_ [HI.sql|UPDATE projects.projects SET payment_plan = 'Free' WHERE id = #{testPid}|]
        tryAny (runTestBg frozenTime tr $ AI.requireAgentAccess (AI.MemberAccess actor.id) testPid) >>= (`shouldSatisfy` isLeft)
        runQueryEffect tr (Issues.eventRunActive testPid run.id) >>= (`shouldBe` False)

      it "routineResults_everyRunRendersNotJustTheFirst" \tr -> do
        let convId = UUIDId UUID.nil
        void $ runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          Issues.insertChatMessage testPid convId Issues.ChatUser "Watch checkout latency" Nothing Nothing
          for_ ["First run result", "Second run result"] \r -> Issues.insertChatMessage testPid convId Issues.ChatAssistant r Nothing Nothing
        (_, threadPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml threadPage
        filter (not . (`T.isInfixOf` html)) ["First run result", "Second run result"] `shouldBe` []

      it "agentDocuments_savedAppendedAndListedBesideTheConversation" \tr -> do
        let convId = UUIDId UUID.nil
            web = (AI.defaultAgenticConfig testPid){AI.conversationId = Just convId, AI.conversationType = Just Issues.CTWeb}
            routine = web{AI.invocationMode = AI.ScheduledRoutine (UUIDId UUID.nil) False, AI.timezone = Just "America/New_York"}
            call cfg name args = AI.executeToolCall cfg (LLM.ToolCall "call" "function" $ LLM.ToolFunction name $ Map.fromList args)
        (saved, appended, listed, readBack, slack, routinePrompt) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          saved <- call web "save_artifact" [("key", "Checkout Review"), ("title", "Checkout review"), ("content", "## Summary\nAll good.")]
          appended <- call web "save_artifact" [("key", "checkout-review"), ("mode", "append"), ("content", "## Follow-up\nLatency recovered.")]
          listed <- call web "read_artifact" []
          readBack <- call web "read_artifact" [("key", "checkout-review")]
          slack <- call web{AI.conversationType = Just Issues.CTSlackThread} "save_artifact" [("key", "x"), ("content", "y")]
          Issues.insertChatMessage testPid convId Issues.ChatAssistant "Saved." Nothing $ Just $ AE.toJSON ([ToolCallInfo "save_artifact" mempty "" saved.rawData] :: [ToolCallInfo])
          (system, _, _) <- AI.agenticSetup routine "run" "model"
          pure (saved, appended, listed, readBack, slack, system.content)
        saved.formatted `shouldSatisfy` T.isPrefixOf "Saved document 'checkout-review'"
        saved.rawData `shouldBe` Just "checkout-review"
        appended.formatted `shouldSatisfy` T.isInfixOf "Checkout review"
        listed.formatted `shouldSatisfy` T.isInfixOf "\"key\":\"checkout-review\""
        readBack.formatted `shouldBe` "## Summary\nAll good.\n\n## Follow-up\nLatency recovered."
        slack.formatted `shouldSatisfy` T.isPrefixOf "Documents are only available"
        -- frozenTime is 2025-01-01T00:00Z, still New Year's Eve in New York.
        routinePrompt `shouldSatisfy` T.isInfixOf "key \"2024-12-31\""
        (_, chatPage) <- testServant tr $ AIThreads.threadGetH testPid convId Nothing Nothing
        LT.toStrict (Lucid.renderText $ Lucid.toHtml chatPage) `shouldSatisfy` T.isInfixOf "href=\"?doc=checkout-review\""
        (_, docPage) <- testServant tr $ AIThreads.threadGetH testPid convId (rightToMaybe $ Issues.mkArtifactKey "checkout-review") Nothing
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml docPage
        filter (not . (`T.isInfixOf` html)) ["?doc=checkout-review", "<h2 id=\"followup\">", "href=\"#followup\"", "Latency recovered."] `shouldBe` []

      it "agentDocuments_boundedPerDocumentPerConversationAndPerRead" \tr -> do
        let convId = UUIDId UUID.nil
            web = (AI.defaultAgenticConfig testPid){AI.conversationId = Just convId, AI.conversationType = Just Issues.CTWeb}
            call name args = (.formatted) <$> AI.executeToolCall web (LLM.ToolCall "call" "function" $ LLM.ToolFunction name $ Map.fromList args)
            big = AE.String $ T.replicate 90_000 "x"
        (badMode, longRead, overflow, overCap, existing) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          badMode <- call "save_artifact" [("key", "a"), ("content", "y"), ("mode", "merge")]
          void $ call "save_artifact" [("key", "log"), ("content", big)]
          void $ call "save_artifact" [("key", "log"), ("mode", "append"), ("content", big)]
          longRead <- call "read_artifact" [("key", "log")]
          overflow <- call "save_artifact" [("key", "log"), ("mode", "append"), ("content", big)]
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content) SELECT #{testPid}, #{convId}, 'doc-' || n, 'Doc', '' FROM generate_series(1, #{Issues.maxArtifactsPerConversation - 1}) n|]
          overCap <- call "save_artifact" [("key", "one-too-many"), ("content", "y")]
          existing <- call "save_artifact" [("key", "log"), ("content", "rewritten")]
          pure (badMode, longRead, overflow, overCap, existing)
        badMode `shouldBe` "mode must be replace or append."
        T.length longRead `shouldSatisfy` (< 41_000)
        longRead `shouldSatisfy` T.isInfixOf "140002 of 180002 characters omitted"
        overflow `shouldSatisfy` T.isPrefixOf "Document 'log' is full."
        overCap `shouldSatisfy` T.isInfixOf "already holds 500 documents"
        existing `shouldSatisfy` T.isPrefixOf "Saved document 'log'"

      it "routineResult_keepsRunEvidenceWhenTheConversationDocumentLimitIsReached" \tr -> do
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "slack-investigation")
        execution <- runTestBg frozenTime tr do
          routine <- Issues.getEventRoutine testPid Issues.EventSlackMessage >>= maybe (throwIO $ ErrorCall "Missing active template") pure
          run <- Issues.startEventRun routine (Issues.SlackCommand routine.conversationId) Issues.StartRun routine.conversationId routine.requestedBy "Retain this run result" >>= maybe (throwIO $ ErrorCall "Missing run") pure
          void
            $ Hasql.interpExecute
              [HI.sql|INSERT INTO apis.ai_artifacts (project_id, conversation_id, key, title, content)
            SELECT #{testPid}, #{routine.conversationId}, 'doc-' || n, 'Document', 'Evidence' FROM generate_series(1, #{Issues.maxArtifactsPerConversation}) n|]
          Issues.saveRunResult testPid run.id "Retained result" "The run keeps its evidence even when the conversation is full."
          Issues.finishRoutineRun run.id run.startedAt Issues.CompletedSucceeded Nothing Nothing Nothing
          pure run
        count <- runQueryEffect tr $ Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM apis.ai_artifacts WHERE project_id = #{testPid}|]
        count `shouldBe` fromIntegral Issues.maxArtifactsPerConversation
        artifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid execution.id
        mapMaybe (.content) artifacts `shouldSatisfy` elem "The run keeps its evidence even when the conversation is full."

      it "issueEnrichment_requiresActivationAndRetainsAnEditableRoutineRun" \tr -> do
        issueId <- runTestBg frozenTime tr do
          issue <- Issues.createQueryAlertIssue testPid "checkout-alert" "Checkout latency" "service == checkout" 100 200 Issues.Above
          Issues.insertIssueReturningId issue
        let configured = Reader.local (\ctx -> ctx{config = ctx.config{openaiApiKey = "routine-test-key"}})
            job = BackgroundJobs.EnhanceIssuesWithLLM testPid (V.singleton issueId)
            execute = configured $ BackgroundJobs.processBackgroundJob tr.trATCtx job
            model :: Bool -> ATBackgroundCtx a -> ATBackgroundCtx a
            model activated = interpose @ELLM.LLM \_ -> \case
              ELLM.CallLLM _ prompt _ -> do
                liftIO $ activated `shouldBe` True
                liftIO $ prompt `shouldSatisfy` T.isInfixOf "Prefer checkout evidence."
                pure
                  $ Right
                  $ if
                    | "issue titler" `T.isInfixOf` prompt -> "Checkout latency exceeds its threshold"
                    | "issue describer" `T.isInfixOf` prompt -> "Checkout latency is twice the threshold.\nCompare the checkout traces.\nlow"
                    | otherwise -> "critical\n0\n0"
              ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
              ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
        -- Unactivated automation must not spend tokens or change issue metadata.
        runTestBg frozenTime tr $ model False execute
        void $ testServant tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
        conversations <- runQueryEffect tr $ Issues.listConversations testPid
        routine <- maybe (fail "Missing issue enrichment routine") pure $ find ((== Just "issue-enrichment") . (.templateKey)) conversations
        void $ testServant tr $ AIThreads.routineInstructionsPostH testPid routine.conversationId (AIThreads.RoutineInstructionsForm "Prefer checkout evidence.")
        runTestBg frozenTime tr $ model True $ concurrently_ execute execute >> execute
        runs <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
        map (.status) runs `shouldBe` [Issues.RunSucceeded]
        usage <- runQueryEffect tr $ Hasql.interpScalar @Int64 [HI.sql|SELECT count(*) FROM projects.ai_usage WHERE project_id = #{testPid} AND source = 'routine'|]
        usage `shouldBe` 3
        run <- maybe (fail "Missing visible issue enrichment run") pure $ listToMaybe runs
        evidence <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid run.id
        (evidence >>= (.instructions)) `shouldBe` Just "Prefer checkout evidence."
        artifacts <- runQueryEffect tr $ Issues.listRunArtifacts testPid run.id
        T.intercalate " " (mapMaybe (.content) artifacts) `shouldSatisfy` T.isInfixOf "Compare the checkout traces."
        updated <- runQueryEffect tr $ Issues.selectIssueById testPid issueId
        fmap (.title) updated `shouldBe` Just "Checkout latency exceeds its threshold"
        fmap (.recommendedAction) updated `shouldBe` Just "Compare the checkout traces."
        retryIssue <- runTestBg frozenTime tr do
          issue <- Issues.createQueryAlertIssue testPid "retry-alert" "Retry latency" "service == checkout" 100 200 Issues.Above
          Issues.insertIssueReturningId issue
        let retryJob = configured $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton retryIssue)
            failing :: ATBackgroundCtx a -> ATBackgroundCtx a
            failing = interpose @ELLM.LLM \_ -> \case
              ELLM.CallLLM{} -> pure $ Left "Provider unavailable"
              ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
              ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
        failed <- tryAny $ runTestBg frozenTime tr $ failing retryJob
        failed `shouldSatisfy` isLeft
        attempts <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
        map (.status) attempts `shouldSatisfy` elem Issues.RunFailed
        for_ ([("issue describer", "Truncated description"), ("issue-severity", "ambiguous\n-1\n0")] :: [(Text, Text)]) $ \(stage, invalid) -> do
          let malformed :: ATBackgroundCtx a -> ATBackgroundCtx a
              malformed = interpose @ELLM.LLM \_ -> \case
                ELLM.CallLLM _ prompt _ -> pure $ Right $ if stage `T.isInfixOf` prompt then invalid else if "issue describer" `T.isInfixOf` prompt then "Observed latency.\nCompare traces.\nlow" else "critical\n0\n0"
                ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
                ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
          invalidResult <- tryAny $ runTestBg frozenTime tr $ malformed retryJob
          invalidResult `shouldSatisfy` isLeft
        runTestBg frozenTime tr $ model True retryJob
        retried <- runQueryEffect tr $ Issues.listRoutineRuns testPid 12
        sort (map (.attempt) retried) `shouldBe` [1, 1, 2, 3, 4]
        -- The source lease survives a worker interruption; its next delivery resumes
        -- the existing run instead of starting a concurrent or duplicate attempt.
        interrupted <- runTestBg frozenTime tr do
          installed <- Issues.getEventRoutine testPid Issues.EventIssueEnrichment >>= maybe (throwIO $ ErrorCall "Missing active template") pure
          issue <- Issues.createQueryAlertIssue testPid "interrupted-alert" "Interrupted latency" "service == checkout" 100 200 Issues.Above
          iid <- Issues.insertIssueReturningId issue
          let convId = UUIDId iid.unUUIDId
          void $ Issues.getOrCreateConversation testPid convId Issues.CTAnomaly (AE.object [])
          void $ Issues.startEventRun installed (Issues.IssueEnrichment iid) Issues.StartRun convId installed.requestedBy "Interrupted worker"
          pure iid
        runTestBg frozenTime tr $ model True $ configured $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton interrupted)
        recovered <- runQueryEffect tr $ Issues.getEventRun testPid (Issues.IssueEnrichment interrupted)
        fmap (.status) recovered `shouldBe` Just Issues.RunSucceeded
        fmap (.attempt) recovered `shouldBe` Just 1
        -- Saved instructions affect future work, never the existing artifact snapshot.
        void $ testServant tr $ AIThreads.routineInstructionsPostH testPid routine.conversationId (AIThreads.RoutineInstructionsForm "Use the current error budget.")
        stoppedIssue <- runTestBg frozenTime tr do
          issue <- Issues.createQueryAlertIssue testPid "stopped-alert" "Keep original title" "service == checkout" 100 200 Issues.Above
          Issues.insertIssueReturningId issue
        let stoppedJob = configured $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.EnhanceIssuesWithLLM testPid $ V.singleton stoppedIssue)
            stopDuringModel :: ATBackgroundCtx a -> ATBackgroundCtx a
            stopDuringModel = interpose @ELLM.LLM \_ -> \case
              ELLM.CallLLM{} -> Issues.pauseRoutine testPid routine.conversationId $> Right "Unauthorized new title"
              ELLM.CallAgenticChat history params token -> ELLM.callAgenticChat history params token
              ELLM.EmbedDocuments config docs -> ELLM.embedDocuments config docs
        runTestBg frozenTime tr $ stopDuringModel stoppedJob
        cancelled <- runQueryEffect tr $ Issues.getEventRun testPid (Issues.IssueEnrichment stoppedIssue)
        fmap (.status) cancelled `shouldBe` Just Issues.RunCancelled
        original <- runQueryEffect tr $ Issues.selectIssueById testPid stoppedIssue
        fmap (.title) original `shouldBe` Just "Keep original title"
        runTestBg frozenTime tr $ model False stoppedJob
        prior <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid run.id
        (prior >>= (.instructions)) `shouldBe` Just "Prefer checkout evidence."

      it "issueFollowup_readsTheRetainedInvestigationArtifactAsTheSignedInMember" \tr -> do
        let convId = UUIDId UUID.nil
            artifact = fromRightShow $ Issues.mkArtifactKey "issue-evidence"
            config = (AI.defaultAgenticConfig testPid){AI.access = AI.MemberAccess (getResponse tr.trSessAndHeader).user.id, AI.conversationId = Just convId, AI.conversationType = Just Issues.CTAnomaly}
        result <- runTestBg frozenTime tr do
          void $ Issues.getOrCreateConversation testPid convId Issues.CTAnomaly (AE.object [])
          void $ Issues.saveArtifact testPid convId artifact (Just "Investigation evidence") "Checkout recovered after the deployment." Issues.ArtifactReplace
          AI.executeToolCall config (LLM.ToolCall "read-evidence" "function" $ LLM.ToolFunction "read_artifact" $ one ("key", AE.String "issue-evidence"))
        result.formatted `shouldBe` "Checkout recovered after the deployment."

      it "lists titled threads and schedules and pauses their routine" \tr -> do
        let convId = UUIDId UUID.nil
        (needsTitle, summary, listed, scheduled, paused, resumed, deleted) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          needsTitle <- Issues.conversationNeedsTitle testPid convId
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          Issues.setConversationTitle testPid convId "Daily reliability brief"
          summary <- Issues.getConversation testPid convId
          listed <- Issues.listConversations testPid
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId $ fromRightShow $ Issues.mkRoutineInterval 15
          Issues.pauseRoutine testPid convId
          paused <- Issues.getConversation testPid convId
          resumed <- Issues.resumeRoutine (getResponse tr.trSessAndHeader).user.id testPid convId
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

      it "weeklyRoutine_dstTransitions_keepTheNextRunAtItsLocalTime" \tr -> do
        template <- maybe (fail "Missing weekly template") pure $ find ((== "weekly-reliability-review") . (.key)) Issues.routineTemplates
        let clock day hour minute = UTCTime day (fromIntegral $ hour * 3600 + minute * 60)
            uid = (getResponse tr.trSessAndHeader).user.id
            convId = UUIDId UUID.nil
        for_ ([(clock (fromGregorian 2026 10 26) 13 0, clock (fromGregorian 2026 11 2) 14 0, 1, 9, 0), (clock (fromGregorian 2026 3 1) 7 30, clock (fromGregorian 2026 3 15) 6 30, 7, 2, 30)] :: [(UTCTime, UTCTime, Int, Int, Int)]) $ \(now, expected, weekday, hour, minute) -> do
          scheduled <- runTestBg now tr do
            void $ Issues.getOrCreateConversation testPid convId Issues.CTWeb (AE.object [])
            void $ Issues.installRoutineTemplate uid testPid convId "America/New_York" template
            Hasql.interpExecute_
              [HI.sql|UPDATE apis.ai_routines SET schedule_weekday = #{weekday}, schedule_hour = #{hour}, schedule_minute = #{minute}
              WHERE project_id = #{testPid} AND conversation_id = #{convId}|]
            Issues.resumeRoutine uid testPid convId
          fmap (.scheduledAt) scheduled `shouldBe` Just expected

      it "installs a built-in routine at the next project-local wall-clock time" \tr -> do
        length Issues.routineTemplates `shouldSatisfy` (>= 15)
        case find ((== "daily-operations-briefing") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "routine catalog must not be empty"
          Just template -> do
            let convId = UUIDId UUID.nil
            (scheduled, installed, canAct, slackEnabled) <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              scheduled <- Issues.installRoutineTemplate (getResponse tr.trSessAndHeader).user.id testPid convId "Europe/Berlin" template
              initialCanAct <- maybe (pure False) (Issues.routineCanAct . (.id)) scheduled
              Issues.setRoutineDestination (getResponse tr.trSessAndHeader).user.id testPid convId Issues.DestinationSlack
              (scheduled,,initialCanAct,) <$> Issues.getConversation testPid convId <*> maybe (pure False) (Issues.routineCanAct . (.id)) scheduled
            (.scheduledAt) <$> scheduled `shouldBe` Just (addUTCTime (8 * 3600) frozenTime)
            ((.templateKey) =<< installed) `shouldBe` Just template.key
            canAct `shouldBe` False
            ((.routineDestination) =<< installed) `shouldBe` Just Issues.DestinationSlack
            slackEnabled `shouldBe` True

      it "rescheduling a template as an interval keeps that schedule after resume" \tr -> do
        case find ((== "daily-operations-briefing") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "routine catalog must not be empty"
          Just template -> do
            let convId = UUIDId UUID.nil
                interval = fromRightShow $ Issues.mkRoutineInterval 15
            resumed <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              void $ Issues.installRoutineTemplate (getResponse tr.trSessAndHeader).user.id testPid convId "Europe/Berlin" template
              void $ Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval
              Issues.pauseRoutine testPid convId
              Issues.resumeRoutine (getResponse tr.trSessAndHeader).user.id testPid convId
            (.scheduledAt) <$> resumed `shouldBe` Just (addUTCTime 900 frozenTime)

      it "scheduledRoutine_carriesAuthorizingMember_andRunsOnce" \tr -> do
        executions <- IORef.newIORef ([] :: [AI.InvocationMode])
        successors <- IORef.newIORef ([] :: [(UUIDId "ai_routine", UTCTime)])
        let convId = UUIDId UUID.nil
            interval = fromRightShow $ Issues.mkRoutineInterval 15
            execute config = do
              Eff.liftIO $ show config.access `shouldBe` show (AI.MemberAccess (getResponse tr.trSessAndHeader).user.id)
              Eff.liftIO $ IORef.modifyIORef' executions (<> [config.invocationMode])
              pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Routine completed\",\"widgets\":[]}", AI.toolCalls = []}
            enqueueNext routineId at = Eff.liftIO $ IORef.modifyIORef' successors (<> [(routineId, at)])
        (history, routine, runRecord, recentRuns) <- runTestBg frozenTime tr do
          void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
          Issues.insertChatMessage testPid convId Issues.ChatUser "Send a daily reliability brief to Slack" Nothing Nothing
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
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
        for_ recentRuns \run -> do
          evidence <- runQueryEffect tr $ Issues.getRoutineRunEvidence testPid run.id
          (evidence >>= (.input)) `shouldBe` Just "Send a daily reliability brief to Slack"
          (evidence >>= (.instructions)) `shouldBe` Just "Send a daily reliability brief to Slack"

      it "suppresses a findings-only routine answer when the agent reports no findings" \tr -> do
        case find ((== "error-regression-radar") . (.key)) Issues.routineTemplates of
          Nothing -> expectationFailure "missing findings-only template"
          Just template -> do
            let convId = UUIDId UUID.nil
                execute _ = pure $ Right AI.AgenticChatResult{AI.response = "{\"explanation\":\"Nothing material changed\",\"has_findings\":false,\"widgets\":[]}", AI.toolCalls = []}
            (history, runStatus) <- runTestBg frozenTime tr do
              void $ Hasql.interpExecute [HI.sql|INSERT INTO apis.ai_conversations (project_id, conversation_id, conversation_type) VALUES (#{testPid}, #{convId}, #{Issues.CTWeb})|]
              Issues.insertChatMessage testPid convId Issues.ChatUser template.prompt Nothing Nothing
              scheduled <- Issues.installRoutineTemplate (getResponse tr.trSessAndHeader).user.id testPid convId "UTC" template >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
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
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
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
          scheduled <- Issues.upsertRoutine (getResponse tr.trSessAndHeader).user.id testPid convId interval >>= maybe (throwIO $ ErrorCall "routine was not scheduled") pure
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

        url <- fold <$> liftIO (Eff.runEff $ Logging.runLog "test" tr.trLogger tr.trATCtx.config.logLevel $ Widget.widgetPngUrl secret baseUrl testPid widget Nothing Nothing Nothing)

        url `shouldSatisfy` T.isInfixOf "widgetZ="
        url `shouldSatisfy` T.isInfixOf "&sig="

      it "rejects URLs exceeding 8000 characters" \tr -> do
        -- gzip compresses repeated chars well, so use unique numbered strings that resist compression
        let incompressible = T.concat [show @Text i <> "abcdefg" | i <- [1 .. 3000 :: Int]]
            hugeWidget = def{Widget.wType = Widget.WTTable, Widget.title = Just incompressible, Widget.query = Just incompressible}
            secret = tr.trATCtx.env.apiKeyEncryptionSecretKey
            baseUrl = tr.trATCtx.env.hostUrl

        url <- liftIO $ Eff.runEff $ Logging.runLog "test" tr.trLogger tr.trATCtx.config.logLevel $ Widget.widgetPngUrl secret baseUrl testPid hugeWidget Nothing Nothing Nothing

        url `shouldBe` Nothing
