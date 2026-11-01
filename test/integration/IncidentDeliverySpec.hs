module IncidentDeliverySpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors, evaluateQueryMonitorValue, notifyErrorSubscriptions, processProjectErrors, runSlackIncidentDeliveries)
import BackgroundJobs qualified as Jobs
import Control.Exception (ErrorCall (..), bracket_)
import Control.Lens ((.~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, _String)
import Data.Default (def)
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.LLM qualified as ELLM
import Data.Effectful.Notify qualified as Notify
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Dispatch.Dynamic (interpose)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Langchain.LLM.Core qualified as Chat
import Lucid qualified
import Models.Apis.ErrorPatterns qualified as Errors
import Models.Apis.Incidents qualified as I
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Investigations qualified as Investigations
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Pages.AIThreads qualified as AIThreads
import Pages.Bots.BotFixtures (slackCallbackEnvelope, slackInteraction)
import Pages.Bots.BotTestHelpers (receiveSlackEvent, setupLinkedSlackData, setupSlackData, slackRootEvent, withHTTPResponses)
import Pages.Bots.Slack qualified as SlackPage
import Pages.Issues qualified as IssuesPage
import Pkg.AI qualified as AI
import Pkg.AgentCatalog qualified as Catalog
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant qualified
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)
import Test.Hspec
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (throwIO, tryAny)
import Web.Routes qualified as Routes


spec :: Spec
spec = around withTestResources do
  describe "Incident delivery" do
    for_ [("recovery", const I.IncidentRecovered), ("resolution", I.IncidentResolved)] \(label, closing) ->
      it ("proactiveIncident_runsOneAnalysisPerEventAndReportsOnlyRecordedRecovery: " <> label) \tr -> do
        update <- setupWithIssue tr
        let user = (Servant.getResponse tr.trSessAndHeader).user.id
        setupLinkedSlackData tr testPid "T1"
        void $ toBaseServantResponse tr $ SlackPage.slackInteractionsH $ (slackInteraction "/monoscope" "incidents on" "T1"){SlackPage.channel_id = "C1"}
        I.Recorded episode _ <- runTestBgNoReset tr $ I.recordIncidentEvent update
        calls <- newIORef (0 :: Int)
        let model :: ATBackgroundCtx a -> ATBackgroundCtx a
            model = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> do
                modifyIORef' calls (+ 1)
                let recovered = any (\message -> T.isInfixOf "\"phase\":\"recovered\"" message.content || T.isInfixOf "\"phase\":\"resolved\"" message.content) $ toList history
                pure $ Right $ Chat.Message Chat.Assistant (if recovered then "Recorded closing summary." else "Recorded alert analysis.") Chat.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
            drain = do
              queued <- liftIO $ dbQuery tr [sql|SELECT payload FROM background_jobs WHERE payload ->> 'tag' = 'InvestigateIncident' ORDER BY id|] ()
              for_ queued \(PGS.Only (Aeson job)) -> Jobs.processBackgroundJobWithTools (Just Routes.mcpProjectTools) tr.trATCtx job
            work = captureNotifs tr $ model do
              runSlackIncidentDeliveries
              replicateM_ 2 $ drain >> runSlackIncidentDeliveries
            reports notifications = [sd | Notify.SlackNotification sd <- notifications, maybe False (T.isPrefixOf "AI incident analysis") (sd.payload ^? key "text" . _String)]
        (opened, _) <- work
        runs <- dbQuery tr [sql|SELECT count(*) FROM apis.ai_routine_runs WHERE project_id = ?|] (PGS.Only testPid)
        (runs :: [PGS.Only Int64]) `shouldBe` [PGS.Only 1]
        readIORef calls `shouldReturn` 1
        map (.channelId) (reports opened) `shouldBe` ["C1"]
        map (.threadTs) (reports opened) `shouldSatisfy` all isJust
        (replayed, _) <- work
        reports replayed `shouldSatisfy` null
        readIORef calls `shouldReturn` 1
        advanceMinutes tr 1
        at <- getTestTime tr.trTestClock
        void $ runTestBgNoReset tr $ I.recordIncidentEvent update{I.observedAt = at, I.change = I.IncidentDataUnavailable}
        (gap, _) <- work
        reports gap `shouldSatisfy` null
        readIORef calls `shouldReturn` 1
        Just stillActive <- runTestBgNoReset tr $ I.getEpisode testPid episode.id
        stillActive.phase `shouldBe` I.EpisodeActive
        advanceMinutes tr 1
        recoveredAt <- getTestTime tr.trTestClock
        void $ runTestBgNoReset tr $ I.recordIncidentEvent update{I.observedAt = recoveredAt, I.change = closing user}
        (closed, _) <- work
        map (.channelId) (reports closed) `shouldBe` ["C1"]
        mapMaybe (\sd -> sd.payload ^? key "text" . _String) (reports closed) `shouldSatisfy` all (T.isInfixOf "Recorded closing summary.")
        readIORef calls `shouldReturn` 2

        (closedReplay, _) <- work
        reports closedReplay `shouldSatisfy` null
        readIORef calls `shouldReturn` 2

    for_ [(Issues.Observe, False), (Issues.Autonomous, False), (Issues.Autonomous, True)] \(policy, interrupted) ->
      it ("proactiveIncident_usesSharedPolicyAndRejectsPrivateEvidence: " <> show (policy, interrupted)) \tr -> do
        update <- setupWithIssue tr
        setupLinkedSlackData tr testPid "T1"
        void $ toBaseServantResponse tr $ SlackPage.slackInteractionsH $ (slackInteraction "/monoscope" "incidents on" "T1"){SlackPage.channel_id = "C1"}
        channel <- either (fail . toString) pure $ Issues.incidentChannel "T1" "C1"
        Just routine <- runTestBgNoReset tr $ Issues.getIncidentRoutine testPid channel
        runTestBgNoReset tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId policy
        I.Recorded _ event <- runTestBgNoReset tr $ I.recordIncidentEvent update
        void $ captureNotifs tr runSlackIncidentDeliveries
        [PGS.Only root] <- dbQuery tr [sql|SELECT id FROM apis.slack_incident_roots WHERE first_event_id = ? AND channel_id = 'C1'|] (PGS.Only event)
        invoked <- newIORef ([] :: [Text])
        interrupt <- newIORef interrupted
        let original = Routes.mcpProjectTools
            backend = original{AI.invoke = \actor pid tool arguments -> modifyIORef' invoked (<> [tool]) >> AI.invoke original actor pid tool arguments}
            model :: ATBackgroundCtx a -> ATBackgroundCtx a
            model = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> do
                let hasTools = any ((== Chat.Tool) . (.role)) history
                when hasTools do
                  stop <- atomicModifyIORef' interrupt (\once -> (False, once))
                  when stop $ throwIO $ ErrorCall "Interrupted after the durable mutation"
                pure
                  $ Right
                  $ if hasTools
                    then Chat.Message Chat.Assistant "Incident evidence and permitted changes are retained." Chat.defaultMessageData
                    else
                      Chat.Message
                        Chat.Assistant
                        ""
                        Chat.defaultMessageData
                          { Chat.toolCalls =
                              Just
                                [ Chat.ToolCall "private" "function" $ Chat.ToolFunction "list_api_keys" mempty
                                , Chat.ToolCall "remediate" "function" $ Chat.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= ("Incident remediation dashboard" :: Text)])
                                ]
                          }
              ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
            execute = runTestBgNoReset tr $ model $ Jobs.processBackgroundJobWithTools (Just backend) tr.trATCtx $ Jobs.InvestigateIncident testPid event root
        when interrupted do
          tryAny execute >>= (`shouldSatisfy` isLeft)
          readIORef invoked `shouldReturn` ["create_dashboard"]
          Just failed <- runTestBgNoReset tr $ Issues.getEventRun testPid $ Issues.IncidentEvent event root
          failed.status `shouldBe` Issues.RunFailed
        replicateM_ 2 execute
        readIORef invoked `shouldReturn` if policy == Issues.Autonomous then ["create_dashboard"] else []
        dashboards <- dbQuery tr [sql|SELECT title, created_by FROM projects.dashboards WHERE project_id = ? AND title = 'Incident remediation dashboard'|] (PGS.Only testPid)
        (dashboards :: [(Text, Projects.UserId)]) `shouldBe` if policy == Issues.Autonomous then [("Incident remediation dashboard", routine.requestedBy)] else []
        Just run <- runTestBgNoReset tr $ Issues.getEventRun testPid $ Issues.IncidentEvent event root
        run.status `shouldBe` Issues.RunSucceeded
        run.attempt `shouldBe` if interrupted then 2 else 1

    for_ [Just Investigations.ReviewApproved, Just Investigations.ReviewRejected, Nothing] $ \decision ->
      it ("proactiveIncident_reviewResumesItsCheckpointAndPublishesOnce: " <> show decision) \tr -> do
        update <- setupWithIssue tr
        setupLinkedSlackData tr testPid "T1"
        void $ toBaseServantResponse tr $ SlackPage.slackInteractionsH $ (slackInteraction "/monoscope" "incidents on" "T1"){SlackPage.channel_id = "C1"}
        channel <- either (fail . toString) pure $ Issues.incidentChannel "T1" "C1"
        Just routine <- runTestBgNoReset tr $ Issues.getIncidentRoutine testPid channel
        runTestBgNoReset tr $ Issues.setRoutinePolicy routine.requestedBy testPid routine.conversationId $ Issues.ReviewSelected $ Catalog.ReviewSelection (one Catalog.ProjectChanges) Catalog.ReviewEditors
        I.Recorded _ event <- runTestBgNoReset tr $ I.recordIncidentEvent update
        void $ captureNotifs tr runSlackIncidentDeliveries
        [PGS.Only root] <- dbQuery tr [sql|SELECT id FROM apis.slack_incident_roots WHERE first_event_id = ? AND channel_id = 'C1'|] (PGS.Only event)
        calls <- newIORef (0 :: Int)
        invoked <- newIORef ([] :: [Text])
        let job = Jobs.InvestigateIncident testPid event root
            original = Routes.mcpProjectTools
            backend = original{AI.invoke = \actor pid tool arguments -> modifyIORef' invoked (<> [tool]) >> AI.invoke original actor pid tool arguments}
            model :: ATBackgroundCtx a -> ATBackgroundCtx a
            model = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat history _ _ -> do
                modifyIORef' calls (+ 1)
                pure
                  $ Right
                  $ if any ((== Chat.Tool) . (.role)) history
                    then Chat.Message Chat.Assistant "The incident decision and evidence are retained." Chat.defaultMessageData
                    else Chat.Message Chat.Assistant "" Chat.defaultMessageData{Chat.toolCalls = Just [Chat.ToolCall "review-remediation" "function" $ Chat.ToolFunction "create_dashboard" $ one ("body", AE.object ["title" AE..= ("Reviewed incident remediation" :: Text)])]}
              ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
            work = captureNotifs tr $ model $ Jobs.processBackgroundJobWithTools (Just backend) tr.trATCtx job >> runSlackIncidentDeliveries
            reports notifications = [sd | Notify.SlackNotification sd <- notifications, maybe False (T.isPrefixOf "AI incident analysis") (sd.payload ^? key "text" . _String)]
        dbExec tr [sql|DELETE FROM background_jobs WHERE payload = ?|] (PGS.Only $ Aeson $ AE.toJSON job)
        (initial, _) <- work
        reports initial `shouldSatisfy` null
        Just waiting <- runTestBgNoReset tr $ Issues.getEventRun testPid $ Issues.IncidentEvent event root
        waiting.status `shouldBe` Issues.RunRunning
        [operation] <- runTestBgNoReset tr $ Investigations.listOperations @AI.OperationPolicy testPid waiting.id
        (_, page) <- testServant tr $ AIThreads.operationGetH testPid operation.id
        decodeUtf8 @Text (Lucid.renderBS $ Lucid.toHtml page) `shouldSatisfy` T.isInfixOf "Approve action"
        queued <- dbQuery tr [sql|SELECT count(*) FROM background_jobs WHERE payload = ? AND status = 'queued'|] (PGS.Only $ Aeson $ AE.toJSON job)
        (queued :: [PGS.Only Int]) `shouldBe` [PGS.Only 1]
        void work
        readIORef calls `shouldReturn` 1
        readIORef invoked `shouldReturn` []
        case decision of
          Just vote -> void $ testServant tr $ AIThreads.operationReviewPostH testPid operation.id $ AIThreads.OperationReviewForm vote
          Nothing -> advanceTestTime tr (24 * 60 * 60 + 1)
        (resumed, _) <- work
        length (reports resumed) `shouldBe` 1
        (repeated, _) <- work
        reports repeated `shouldSatisfy` null
        readIORef calls `shouldReturn` 2
        readIORef invoked `shouldReturn` ["create_dashboard" | decision == Just Investigations.ReviewApproved]
        Just finished <- runTestBgNoReset tr $ Issues.getEventRun testPid $ Issues.IncidentEvent event root
        (finished.id, finished.attempt, finished.status) `shouldBe` (waiting.id, 1, Issues.RunSucceeded)
        dashboards <- dbQuery tr [sql|SELECT created_by FROM projects.dashboards WHERE project_id = ? AND title = 'Reviewed incident remediation'|] (PGS.Only testPid)
        (dashboards :: [PGS.Only Projects.UserId]) `shouldBe` [PGS.Only routine.requestedBy | decision == Just Investigations.ReviewApproved]

    for_ [minBound .. maxBound] \interruption ->
      it ("proactiveIncident_withholdsWorkAndPublication: " <> show interruption) \tr -> do
        update <- setupWithIssue tr
        setupLinkedSlackData tr testPid "T1"
        let command = (slackInteraction "/monoscope" "incidents on" "T1"){SlackPage.channel_id = "C1"}
            user = (Servant.getResponse tr.trSessAndHeader).user.id
            revoke = liftIO $ dbExec tr [sql|UPDATE projects.project_members SET active = false WHERE project_id = ? AND user_id = ?|] (testPid, user)
        unless (interruption == NoIncidentOptIn) $ void $ toBaseServantResponse tr $ SlackPage.slackInteractionsH command
        channel <- either (fail . toString) pure $ Issues.incidentChannel "T1" "C1"
        let installation = Issues.incidentRoutineConversationId testPid channel
        I.Recorded episode event <- runTestBgNoReset tr $ I.recordIncidentEvent update
        issue <- maybe (fail "Fixture episode has no issue") pure episode.issueId
        void $ captureNotifs tr runSlackIncidentDeliveries
        [PGS.Only root] <- dbQuery tr [sql|SELECT id FROM apis.slack_incident_roots WHERE first_event_id = ? AND channel_id = 'C1'|] (PGS.Only event)
        calls <- newIORef (0 :: Int)
        let model :: ATBackgroundCtx a -> ATBackgroundCtx a
            model = interpose @ELLM.LLM \_ -> \case
              ELLM.CallAgenticChat{} -> do
                modifyIORef' calls (+ 1)
                when (interruption == PauseDuringIncidentModel) $ Issues.pauseRoutine testPid installation
                when (interruption == RevokeDuringIncidentModel) revoke
                when (interruption == RestrictDuringIncidentModel) $ Investigations.retainEvidencePermission testPid [UUIDId issue.unUUIDId] PM.PAdmin
                pure $ Right $ Chat.Message Chat.Assistant "This result must stay in the run history." Chat.defaultMessageData
              ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
              ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
        when (interruption == PauseBeforeIncidentRun) $ runTestBgNoReset tr $ Issues.pauseRoutine testPid installation
        outcome <- runTestBgNoReset tr $ model $ tryAny $ Jobs.processBackgroundJob tr.trATCtx $ Jobs.InvestigateIncident testPid event root
        outcome `shouldSatisfy` isRight
        runTestBgNoReset tr do
          when (interruption == PauseBeforeIncidentReport) $ Issues.pauseRoutine testPid installation
          when (interruption == RevokeBeforeIncidentReport) revoke
          when (interruption == ReplaceBeforeIncidentReport) $ EHasql.interpExecute_ [HI.sql|UPDATE apis.slack SET team_id = 'T_REPLACED' WHERE project_id = #{testPid}|]
        (sent, _) <- captureNotifs tr runSlackIncidentDeliveries
        sent `shouldSatisfy` null
        readIORef calls `shouldReturn` if interruption `elem` [NoIncidentOptIn, PauseBeforeIncidentRun] then 0 else 1
        result <- runTestBgNoReset tr $ Issues.getEventRun testPid $ Issues.IncidentEvent event root
        fmap (.status) result
          `shouldBe` if interruption `elem` [NoIncidentOptIn, PauseBeforeIncidentRun]
            then Nothing
            else Just $ if interruption `elem` [PauseDuringIncidentModel, RevokeDuringIncidentModel, RestrictDuringIncidentModel] then Issues.RunCancelled else Issues.RunSucceeded

    it "proactiveIncident_admissionRejectsForgedContextAndRechecksResumedRequests" \tr -> do
      update <- setupWithIssue tr
      setupLinkedSlackData tr testPid "T1"
      void $ toBaseServantResponse tr $ SlackPage.slackInteractionsH $ (slackInteraction "/monoscope" "incidents on" "T1"){SlackPage.channel_id = "C1"}
      channel <- either (fail . toString) pure $ Issues.incidentChannel "T1" "C1"
      Just routine <- runTestBgNoReset tr $ Issues.getIncidentRoutine testPid channel
      I.Recorded episode event <- runTestBgNoReset tr $ I.recordIncidentEvent update
      issue <- maybe (fail "Fixture episode has no issue") pure episode.issueId
      void $ captureNotifs tr runSlackIncidentDeliveries
      [PGS.Only root, PGS.Only otherRoot] <- dbQuery tr [sql|SELECT id FROM apis.slack_incident_roots WHERE first_event_id = ? ORDER BY channel_id|] (PGS.Only event)
      let conversation = UUIDId issue.unUUIDId
          otherConversation = UUIDId $ UUID.fromWords 0 0 0 983
          run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
          start source destination continuation = run $ Issues.startEventRun routine source continuation destination "Stored incident input"
      for_ [conversation, otherConversation] \conv -> void $ run $ Issues.getOrCreateConversation testPid conv Issues.CTAnomaly (AE.object [])
      I.Recorded _ gap <- run $ I.recordIncidentEvent update{I.observedAt = addUTCTime 60 frozenTime, I.change = I.IncidentDataUnavailable}
      for_ [(UUIDId UUID.nil, root, conversation), (event, otherRoot, conversation), (event, root, otherConversation), (gap, root, conversation)] \(source, parent, destination) ->
        start (Issues.IncidentEventRequest source parent) destination Issues.StartRun >>= (`shouldSatisfy` isNothing)
      Just claimed <- start (Issues.IncidentEventRequest event root) conversation Issues.StartRun
      run (Issues.routineRunActive testPid claimed.id) `shouldReturn` True
      run $ EHasql.interpExecute_ [HI.sql|UPDATE apis.slack SET team_id = 'T_REPLACED' WHERE project_id = #{testPid}|]
      run (Issues.routineRunActive testPid claimed.id) `shouldReturn` False
      start (Issues.IncidentEventRequest event root) conversation Issues.ContinueRun >>= (`shouldSatisfy` isNothing)

    it "proactiveIncident_reconcilesAnAmbiguousReportWithoutRepeatingTheModelOrReply" \tr -> do
      update <- setupWithIssue tr
      let resources = tr{trATCtx = tr.trATCtx{Config.env = tr.trATCtx.env{Config.slackAppId = "A_TEST"}}}
          run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset resources
      setupLinkedSlackData tr testPid "T1"
      void $ toBaseServantResponse resources $ SlackPage.slackInteractionsH $ (slackInteraction "/monoscope" "incidents on" "T1"){SlackPage.channel_id = "C1"}
      I.Recorded _ event <- run $ I.recordIncidentEvent update
      void $ captureNotifs resources runSlackIncidentDeliveries
      [PGS.Only root] <- dbQuery tr [sql|SELECT id FROM apis.slack_incident_roots WHERE first_event_id = ? AND channel_id = 'C1'|] (PGS.Only event)
      calls <- newIORef (0 :: Int)
      let report = T.replicate 5000 "r"
          model :: ATBackgroundCtx a -> ATBackgroundCtx a
          model = interpose @ELLM.LLM \_ -> \case
            ELLM.CallAgenticChat{} -> modifyIORef' calls (+ 1) $> Right (Chat.Message Chat.Assistant report Chat.defaultMessageData)
            ELLM.CallLLM{} -> pure $ Left "Unexpected non-agent call"
            ELLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
          work = run $ model $ Jobs.processBackgroundJob resources.trATCtx $ Jobs.InvestigateIncident testPid event root
          ddl statement = dbExec tr statement ()
      bracket_
        (ddl [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_agent_report CHECK (operation <> 'post_analysis') NOT VALID|])
        (ddl [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_agent_report|])
        (tryAny work >>= (`shouldSatisfy` isLeft))
      Just completed <- run $ Issues.getEventRun testPid $ Issues.IncidentEvent event root
      completed.status `shouldBe` Issues.RunSucceeded
      work
      now <- getTestTime tr.trTestClock
      [delivery] <- run $ I.claimSlackDeliveries now
      I.PostAnalysis parent reportRun <- pure delivery.operation
      Just summary <- pure $ AE.toJSON delivery.payload ^? key "text" . _String
      T.length summary `shouldSatisfy` (< 3500)
      summary `shouldSatisfy` T.isInfixOf reportRun.toText
      retained <- dbQuery tr [sql|SELECT content FROM apis.ai_routine_run_artifacts WHERE run_id = ? AND key = 'latest-result'|] (PGS.Only reportRun)
      (retained :: [PGS.Only Text]) `shouldBe` [PGS.Only report]
      run (I.finishSlackDelivery now delivery $ I.DeliveryUncertain "lost_ack") `shouldReturn` True
      searches <- run $ I.claimIncidentSearches now
      length searches `shouldBe` 1
      [abandoned] <- pure searches
      advanceMinutes tr 3
      observed <- timestamp "1788900100.000001"
      let message =
            AE.object
              [ "ts" AE..= I.slackTimestampText observed
              , "thread_ts" AE..= I.slackTimestampText parent
              , "app_id" AE..= ("A_TEST" :: Text)
              , "bot_id" AE..= ("B_TEST" :: Text)
              , "metadata" AE..= (AE.toJSON (I.correlateSlackDelivery delivery delivery.payload) ^? key "metadata")
              ]
      at <- getTestTime tr.trTestClock
      (requests, _) <-
        runTestBgRecordingHTTP at resources
          $ withHTTPResponses
            ( \opts endpoint -> do
                endpoint `shouldBe` "https://slack.com/api/conversations.replies"
                opts ^. Wreq.param "channel" `shouldBe` ["C1"]
                opts ^. Wreq.param "ts" `shouldBe` [I.slackTimestampText parent]
                pure $ Just $ AE.encode $ AE.object ["ok" AE..= True, "messages" AE..= ([message] :: [AE.Value])]
            )
            SlackPage.reconcileIncidentDeliveries
      map fst requests `shouldBe` ["https://slack.com/api/conversations.replies"]
      run (I.confirmIncidentSearch abandoned observed) `shouldReturn` False
      work
      (sent, _) <- captureNotifs resources runSlackIncidentDeliveries
      sent `shouldSatisfy` null
      readIORef calls `shouldReturn` 1
      stored <- dbQuery tr [sql|SELECT state, message_ts FROM apis.slack_incident_deliveries WHERE id = ?|] (PGS.Only delivery.id)
      (stored :: [(Text, Maybe Text)]) `shouldBe` [("delivered", Just $ I.slackTimestampText observed)]

    it "commits ingested error notification claims with two destination roots and retries an outbox failure" \tr -> do
      setupSlackData tr testPid "T1"
      dbExec tr [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2','C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
      dbExec tr [sql|UPDATE projects.projects SET error_alerts = true WHERE id = ?|] (PGS.Only testPid)
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_ingested_delivery CHECK (false) NOT VALID|] ()
      let sample = def{Errors.when = frozenTime, Errors.hash = "ingested-outbox", Errors.errorType = "CheckoutError", Errors.message = "Payment failed"}
          ingest = captureNotifs tr $ processProjectErrors testPid [sample] frozenTime
          messages ns = [sd | Notify.SlackNotification sd <- ns]
      ingest `shouldThrow` anyException
      Just unnotified <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
      unnotified.lastNotifiedAt `shouldBe` Nothing
      tokens <- dbQuery tr [sql|SELECT count(*) FROM apis.notification_rate_limit WHERE project_id = ?|] (PGS.Only testPid)
      (tokens :: [PGS.Only Int64]) `shouldBe` [PGS.Only 0]
      slots <- dbQuery tr [sql|SELECT last_notified_at FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, sample.hash)
      (slots :: [PGS.Only (Maybe UTCTime)]) `shouldBe` [PGS.Only Nothing]
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_ingested_delivery|] ()
      ((batchA, _), (batchB, _)) <- concurrently ingest ingest
      let initial = messages $ batchA <> batchB
      sort (map (.channelId) initial) `shouldBe` ["C1", "C2"]
      map (.threadTs) initial `shouldBe` [Nothing, Nothing]
      Just notified <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
      notified.lastNotifiedAt `shouldSatisfy` isJust
      dbExec tr [sql|UPDATE apis.error_patterns SET subscribed = true, notify_every_minutes = 1 WHERE id = ?|] (PGS.Only notified.id)
      advanceMinutes tr 2
      (reminder, _) <- captureNotifs tr do
        notifyErrorSubscriptions testPid [sample.hash]
        replicateM_ 2 runSlackIncidentDeliveries
      let updates = messages reminder
      length updates `shouldBe` 4
      length [sd | sd <- updates, isJust sd.threadTs] `shouldBe` 2
      roots <- dbQuery tr [sql|SELECT count(*) FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.source_kind = 'error' AND e.source_id = ?|] (PGS.Only notified.id)
      (roots :: [PGS.Only Int64]) `shouldBe` [PGS.Only 2]
      -- Quiet counters alone cannot close an error or establish recovery.
      dbExec
        tr
        [sql|UPDATE apis.error_patterns SET quiet_minutes = resolution_threshold_minutes + 1,
          occurrences_1m = 0, occurrences_5m = 0, occurrences_1h = 0, occurrences_24h = 0 WHERE id = ?|]
        (PGS.Only notified.id)
      now <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ Errors.updateOccurrenceCountsBatch [testPid] now
      Just quietPattern <- runTestBgNoReset tr $ Errors.getErrorPatternById notified.id
      (quietPattern.state, isNothing quietPattern.resolvedAt, quietPattern.resolvedBy) `shouldBe` (notified.state, True, Nothing)
      -- A newer acknowledged issue must win over an older unnotified escalation.
      dbExec
        tr
        [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ? WHERE project_id = ? AND target_hash = ?|]
        (frozenTime, addUTCTime 3600 frozenTime, testPid, sample.hash)
      newer <- runTestBgNoReset tr $ Issues.createNewErrorIssue testPid notified >>= Issues.insertIssueReturningId
      void
        $ dbExec
          tr
          [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ?, last_notified_at = ? WHERE id = ?|]
          (frozenTime, addUTCTime 3600 frozenTime, frozenTime, newer)
      void
        $ dbExec
          tr
          [sql|UPDATE apis.issues SET acknowledged_at = NULL, acknowledged_until = NULL, last_notified_at = NULL WHERE project_id = ? AND target_hash = ? AND id <> ?|]
          (testPid, sample.hash, newer)
      dbExec tr [sql|UPDATE apis.error_patterns SET state = 'escalating' WHERE id = ?|] (PGS.Only notified.id)
      advanceMinutes tr 1
      (silenced, _) <- captureNotifs tr $ notifyErrorSubscriptions testPid [sample.hash]
      messages silenced `shouldBe` []
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid notified.id.unErrorPatternId False
      (closed, _) <- captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      length (messages closed) `shouldBe` 4
      for_ (messages closed) \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "RESOLVED" . decodeUtf8 . toStrict)

    it "keeps one error episode across different issue records and resolves its original incident link" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let sample = def{Errors.when = frozenTime, Errors.hash = "stable-error-episode", Errors.errorType = "CheckoutError"}
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample] frozenTime
      Just err <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
      issue <- runTestBgNoReset tr do
        created <- Issues.createNewErrorIssue testPid err
        Issues.insertIssue created
        pure created
      let payload iid =
            I.SlackPayload
              $ KM.fromList
                [ "text" AE..= ("Error observed" :: Text)
                , "blocks"
                    AE..= ( [ AE.object
                                [ "type" AE..= ("section" :: Text)
                                , "block_id" AE..= ("incident_actions" :: Text)
                                , "text" AE..= AE.object ["type" AE..= ("mrkdwn" :: Text), "text" AE..= ("<https://app.monoscope.tech/p/" <> testPid.toText <> "/issues/" <> iid.toText <> "|Open incident>")]
                                ]
                            ]
                              :: [AE.Value]
                          )
                ]
          initial = update{I.source = I.ErrorIncident err.id, I.issueId = Just issue.id, I.rootPayload = payload issue.id}
      I.Recorded episode firstEvent <- runTestBgNoReset tr $ I.recordIncidentEvent initial
      void $ captureNotifs tr runSlackIncidentDeliveries
      -- Escalation can produce another issue row while the error remains active.
      dbExec tr [sql|UPDATE apis.issues SET archived_at = ? WHERE id = ?|] (frozenTime, issue.id)
      advanceMinutes tr 1
      nextIssue <- runTestBgNoReset tr do
        created <- Issues.createNewErrorIssue testPid err
        Issues.insertIssue created
        pure created
      nextIssue.id `shouldNotBe` issue.id
      now <- getTestTime tr.trTestClock
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.projectId = UUIDId $ UUID.fromWords 0 0 0 999}) `shouldReturn` I.Rejected I.UnknownIncidentSource
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.issueId = Nothing}) `shouldReturn` I.Rejected I.IncidentIssueMismatch
      dbExec tr [sql|UPDATE apis.issues SET target_hash = 'another-error' WHERE id = ?|] (PGS.Only nextIssue.id)
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.issueId = Just nextIssue.id, I.observedAt = now}) `shouldReturn` I.Rejected I.IncidentIssueMismatch
      dbExec tr [sql|UPDATE apis.issues SET target_hash = ? WHERE id = ?|] (sample.hash, nextIssue.id)
      I.Recorded ongoing _ <- runTestBgNoReset tr $ I.recordIncidentEvent initial{I.issueId = Just nextIssue.id, I.observedAt = now, I.change = I.IncidentReminder}
      (ongoing.id, ongoing.issueId) `shouldBe` (episode.id, Just issue.id)
      void $ captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      advanceMinutes tr 1
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
      (resolution, _) <- captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      let messages = [sd | Notify.SlackNotification sd <- resolution]
      length messages `shouldBe` 4
      for_ messages \sd -> do
        let body = decodeUtf8 @Text $ toStrict $ AE.encode sd.payload
        body `shouldSatisfy` T.isInfixOf issue.id.toText
        body `shouldSatisfy` not . T.isInfixOf nextIssue.id.toText
      roots <- dbQuery tr [sql|SELECT channel_id FROM apis.slack_incident_roots WHERE episode_id = ? ORDER BY channel_id|] (PGS.Only episode.id)
      roots `shouldBe` [PGS.Only ("C1" :: Text), PGS.Only "C2"]
      closedAt <- getTestTime tr.trTestClock
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.observedAt = addUTCTime 1 closedAt}) `shouldReturn` I.Rejected I.InactiveIncidentSource
      runTestBgNoReset tr (I.recordIncidentEvent initial) `shouldReturn` I.AlreadyRecorded episode.id firstEvent
      advanceMinutes tr 1
      againAt <- getTestTime tr.trTestClock
      let canonicalSample = sample{Errors.hash = "canonical-error"} :: Errors.ATError
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample{Errors.when = againAt}, canonicalSample] againAt
      Just canonical <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid canonicalSample.hash
      dbExec tr [sql|UPDATE apis.error_patterns SET canonical_id = ? WHERE id = ?|] (canonical.id, err.id)
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.observedAt = againAt}) `shouldReturn` I.Rejected I.InactiveIncidentSource
      dbExec tr [sql|UPDATE apis.error_patterns SET canonical_id = NULL WHERE id = ?|] (PGS.Only err.id)
      I.Recorded recurring _ <- runTestBgNoReset tr $ I.recordIncidentEvent initial{I.issueId = Just nextIssue.id, I.observedAt = againAt, I.rootPayload = payload nextIssue.id}
      recurring.id `shouldNotBe` episode.id
      void $ captureNotifs tr runSlackIncidentDeliveries
      dbExec tr [sql|DELETE FROM apis.issues WHERE id = ?|] (PGS.Only nextIssue.id)
      advanceMinutes tr 1
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid recurring.id) `shouldReturn` Just I.EpisodeResolved
      (orphaned, _) <- captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      let orphanedMessages = [sd | Notify.SlackNotification sd <- orphaned]
      length orphanedMessages `shouldBe` 4
      for_ orphanedMessages \sd -> do
        let body = decodeUtf8 @Text $ toStrict $ AE.encode sd.payload
        body `shouldSatisfy` T.isInfixOf "Open project issues"
        body `shouldSatisfy` not . T.isInfixOf nextIssue.id.toText

    it "records manual error resolution once and updates both incident threads without claiming recovery" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let sample = def{Errors.when = frozenTime, Errors.hash = "manual-resolution", Errors.errorType = "CheckoutError", Errors.message = "payment declined"}
          actor = (Servant.getResponse tr.trSessAndHeader).user
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample] frozenTime
      Just err <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
      issue <- runTestBgNoReset tr do
        created <- Issues.createNewErrorIssue testPid err
        Issues.insertIssue created
        pure created
      I.Recorded episode _ <- runTestBgNoReset tr $ I.recordIncidentEvent update{I.source = I.IssueIncident issue.id, I.issueId = Just issue.id}
      void $ captureNotifs tr runSlackIncidentDeliveries
      advanceMinutes tr 1
      dbExec tr [sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False
      Just denied <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
      denied.state `shouldBe` Errors.ESNew
      let resolve pid = runTestBgNoReset tr $ I.resolveErrorIncident pid err.id actor.id (addUTCTime 60 frozenTime) (const update.rootPayload)
      resolve testPid `shouldReturn` I.ErrorResolutionDenied
      resolve (UUIDId $ UUID.fromWords 0 0 0 999) `shouldReturn` I.ErrorResolutionDenied
      dbExec tr [sql|UPDATE apis.error_patterns SET assignee_id = ? WHERE id = ?|] (actor.id, err.id)
      runTestBgNoReset tr (I.resolveErrorIncident testPid err.id actor.id (addUTCTime (-1) frozenTime) (const update.rootPayload))
        `shouldReturn` I.ErrorResolutionConflict I.OlderThanCurrentEpisode
      dbExec tr [sql|UPDATE projects.project_members SET active = false WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      resolve testPid `shouldReturn` I.ErrorResolutionDenied
      dbExec tr [sql|UPDATE projects.project_members SET active = true WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_resolution CHECK (false) NOT VALID|] ()
      testServant tr (IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False) `shouldThrow` anyException
      Just rolledBack <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
      (rolledBack.state, rolledBack.resolvedAt, rolledBack.resolvedBy) `shouldBe` (Errors.ESNew, Nothing, Nothing)
      activityBefore <- runTestBgNoReset tr $ Issues.selectIssueActivity testPid issue.id
      activityBefore `shouldSatisfy` all (\a -> a.event `notElem` map Issues.Lifecycle [Issues.IEResolved, Issues.IEAutoResolved])
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_resolution|] ()
      void
        $ concurrently
          (testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False)
          (testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False)
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False
      events <- dbQuery tr [sql|SELECT event_kind, actor_id FROM apis.incident_events WHERE episode_id = ? AND event_kind <> 'alert'|] (PGS.Only episode.id)
      events `shouldBe` [("resolved" :: Text, Just actor.id)]
      activity <- dbQuery tr [sql|SELECT event, created_by FROM apis.issue_activity_log WHERE issue_id = ? AND event IN ('resolved', 'auto_resolved')|] (PGS.Only issue.id)
      activity `shouldBe` [("resolved" :: Text, Just actor.id)]
      runTestBgNoReset tr (Errors.updateErrorPatternState err.id Errors.ESResolved (addUTCTime 61 frozenTime)) `shouldReturn` 0
      Just resolved <- runTestBgNoReset tr $ Errors.getErrorPatternLByHash testPid sample.hash (addUTCTime 61 frozenTime)
      resolved.base.resolvedBy `shouldBe` Just actor.id
      (replies, _) <- captureNotifs tr runSlackIncidentDeliveries
      (edits, _) <- captureNotifs tr runSlackIncidentDeliveries
      let messages = [sd | Notify.SlackNotification sd <- replies <> edits]
      length messages `shouldBe` 4
      sort [(sd.channelId, sd.threadTs) | sd <- messages, isJust sd.threadTs]
        `shouldBe` sort [(sd.channelId, Just ts) | sd <- messages, AE.Object obj <- [sd.payload], Just (AE.String ts) <- [KM.lookup "ts" obj]]
      for_ messages \sd -> do
        let body = decodeUtf8 @Text $ toStrict $ AE.encode sd.payload
        body `shouldSatisfy` T.isInfixOf "RESOLVED"
        body `shouldSatisfy` T.isInfixOf actor.firstName
        body `shouldSatisfy` T.isInfixOf "Measured recovery has not been verified"
        body `shouldSatisfy` T.isInfixOf issue.id.toText
      (again, _) <- captureNotifs tr runSlackIncidentDeliveries
      again `shouldBe` []
      advanceMinutes tr 1
      now <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample{Errors.when = now}] now
      Just regressed <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
      (regressed.state, regressed.resolvedBy) `shouldBe` (Errors.ESRegressed, Nothing)
      I.Recorded recurring _ <- runTestBgNoReset tr $ I.recordIncidentEvent update{I.source = I.IssueIncident issue.id, I.issueId = Just issue.id, I.observedAt = now}
      recurring.id `shouldNotBe` episode.id
      void $ runTestBgNoReset tr $ Errors.updateErrorPatternState err.id Errors.ESResolved now
      activityAfter <- dbQuery tr [sql|SELECT event, created_by FROM apis.issue_activity_log WHERE issue_id = ? AND event IN ('resolved', 'auto_resolved') ORDER BY created_at|] (PGS.Only issue.id)
      activityAfter `shouldBe` [("resolved" :: Text, Just actor.id), ("auto_resolved", Nothing)]
      void $ runTestBgNoReset tr $ Errors.updateErrorPatternState err.id Errors.ESOngoing now
      advanceMinutes tr 1
      autoAt <- getTestTime tr.trTestClock
      -- A SQL state transition can retain the previous resolved_at; activity must use this transition's clock.
      void $ runHasqlEffect tr $ EHasql.interpExecute [HI.sql|UPDATE apis.error_patterns SET state = 'resolved' WHERE id = #{err.id}|]
      autoTimes <- dbQuery tr [sql|SELECT created_at FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'auto_resolved' ORDER BY created_at DESC LIMIT 1|] (PGS.Only issue.id)
      autoTimes `shouldBe` [PGS.Only autoAt]
      let legacy = sample{Errors.hash = "legacy-resolution"} :: Errors.ATError
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [legacy] now
      Just legacyErr <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid legacy.hash
      dbExec tr [sql|UPDATE projects.project_members SET permission = 'edit' WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      let deliveryCount = dbQuery tr [sql|SELECT count(*) FROM apis.slack_incident_deliveries|] () :: IO [PGS.Only Int64]
      beforeLegacy <- deliveryCount
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid legacyErr.id.unErrorPatternId False
      deliveryCount `shouldReturn` beforeLegacy
      Just legacyResolved <- runTestBgNoReset tr $ Errors.getErrorPatternById legacyErr.id
      (legacyResolved.state, legacyResolved.resolvedBy) `shouldBe` (Errors.ESResolved, Just actor.id)

    it "marks missing monitor data in existing threads and waits for a measured recovery" \tr -> do
      update <- setupWithIssue tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      dbExec tr [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
      dbExec tr [sql|UPDATE monitors.query_monitors SET alert_threshold = 60, log_query = 'name == "missing-monitor-reading" | summarize max(duration)', check_interval_mins = 1 WHERE id = ?|] (PGS.Only mid)
      let messages ns = [sd | Notify.SlackNotification sd <- ns]
          check = fst <$> captureNotifs tr (checkTriggeredQueryMonitors >> replicateM_ 2 runSlackIncidentDeliveries)
      runTestBgNoReset tr (I.recordIncidentEvent update{I.change = I.IncidentDataUnavailable}) `shouldReturn` I.NoOpenEpisode
      initial <- messages <$> evaluateMonitor tr mid 84
      length initial `shouldBe` 2
      advanceMinutes tr 2
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_data_gap CHECK (false) NOT VALID|] ()
      check `shouldReturn` []
      Just uncommitted <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      uncommitted.lastEvaluated `shouldBe` Just frozenTime
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_data_gap|] ()
      missing <- messages <$> check
      length missing `shouldBe` 4
      length [sd | sd <- missing, isJust sd.threadTs] `shouldBe` 2
      for_ missing \sd -> do
        let payload = decodeUtf8 $ toStrict $ AE.encode sd.payload
        payload `shouldSatisfy` T.isInfixOf "DATA UNAVAILABLE"
        payload `shouldSatisfy` T.isInfixOf "Recovery is unconfirmed"
      Just current <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      (current.currentStatus, current.currentValue, current.notificationCount) `shouldBe` (Monitors.MSAlerting, 84, 2)
      advanceMinutes tr 1
      repeated <- messages <$> check
      length [sd | sd <- repeated, isJust sd.threadTs] `shouldBe` 0
      advanceMinutes tr 1
      void $ evaluateMonitor tr mid 84
      dbExec tr [sql|UPDATE monitors.query_monitors SET log_query = 'SELECT * FROM missing_monitor_table' WHERE id = ?|] (PGS.Only mid)
      advanceMinutes tr 1
      failed <- messages <$> check
      length failed `shouldBe` 4
      for_ failed \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "The evaluation failed" . decodeUtf8 . toStrict)
      advanceMinutes tr 1
      dbExec tr [sql|UPDATE monitors.query_monitors SET stop_after_count = 3 WHERE id = ?|] (PGS.Only mid)
      void $ evaluateMonitor tr mid 84
      advanceMinutes tr 1
      limited <- messages <$> check
      length [sd | sd <- limited, isJust sd.threadTs] `shouldBe` 0
      advanceMinutes tr 1
      recovered <- messages <$> evaluateMonitor tr mid 18
      length recovered `shouldBe` 4
      for_ recovered \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "RECOVERED" . decodeUtf8 . toStrict)
      roots <- dbQuery tr [sql|SELECT count(*) FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.source_kind = 'monitor' AND e.source_id = ?|] (PGS.Only mid)
      (roots :: [PGS.Only Int64]) `shouldBe` [PGS.Only 2]

    it "keeps warning, escalation, and recovery in two monitor threads and preserves the onset snapshot" \tr -> do
      update <- setupWithIssue tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      dbExec tr [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2','C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
      dbExec tr [sql|UPDATE monitors.query_monitors SET alert_threshold = 60, warning_threshold = 40 WHERE id = ?|] (PGS.Only mid)
      let evaluate = evaluateMonitor tr mid
          slackMessages ns = [sd | Notify.SlackNotification sd <- ns]
      (initialA, initialB) <- concurrently (evaluate 50) (evaluate 50)
      let initial = slackMessages $ initialA <> initialB
      length initial `shouldBe` 2
      map (.threadTs) initial `shouldBe` [Nothing, Nothing]
      for_ initial \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "WARNING" . decodeUtf8 . toStrict)
      advanceMinutes tr 1
      escalated <- slackMessages <$> evaluate 84
      length escalated `shouldBe` 4
      advanceMinutes tr 1
      recovered <- slackMessages <$> evaluate 18
      length recovered `shouldBe` 4
      let replies = [(sd.channelId, sd.threadTs) | sd <- recovered, isJust sd.threadTs]
          edits = [(sd.channelId, KM.lookup "ts" obj, sd.payload) | sd <- recovered, AE.Object obj <- [sd.payload], KM.member "ts" obj]
      sort [(cid, ts) | (cid, ts, _) <- edits] `shouldBe` sort [(cid, AE.String <$> ts) | (cid, ts) <- replies]
      for_ edits \(_, _, payload) -> do
        let body = decodeUtf8 @Text $ toStrict $ AE.encode payload
        body `shouldSatisfy` T.isInfixOf "RECOVERED"
        body `shouldSatisfy` T.isInfixOf "Initial value: 50.0"
        body `shouldSatisfy` T.isInfixOf "widgetZ="
        body `shouldSatisfy` T.isInfixOf "/issues/"
      episodes <- dbQuery tr [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid)
      (episodes :: [PGS.Only Text]) `shouldBe` [PGS.Only "recovered"]
      advanceMinutes tr 1
      recurring <- slackMessages <$> evaluate 84
      length recurring `shouldBe` 2
      map (.threadTs) recurring `shouldBe` [Nothing, Nothing]

    it "records recovery while muted and refreshes the root when delivery resumes" \tr -> do
      update <- setupWithIssue tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      dbExec tr [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
      void $ evaluateMonitor tr mid 84
      dbExec tr [sql|UPDATE monitors.query_monitors SET muted_until = ? WHERE id = ?|] (addUTCTime 180 frozenTime, mid)
      advanceMinutes tr 1
      evaluateMonitor tr mid 18 `shouldReturn` []
      phases <- dbQuery tr [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid)
      (phases :: [PGS.Only Text]) `shouldBe` [PGS.Only "recovered"]
      advanceMinutes tr 2
      (resumed, _) <- captureNotifs tr runSlackIncidentDeliveries
      let updates = [obj | Notify.SlackNotification sd <- resumed, AE.Object obj <- [sd.payload], KM.member "ts" obj]
      length updates `shouldBe` 1
      for_ updates \obj -> KM.lookup "text" obj `shouldSatisfy` maybe False (\case AE.String text -> "RECOVERED" `T.isInfixOf` text; _ -> False)
      advanceMinutes tr 1
      recurring <- evaluateMonitor tr mid 84
      length [sd | Notify.SlackNotification sd <- recurring, isNothing sd.threadTs] `shouldBe` 1

    it "rolls back monitor state, measurement, and issue creation if the Slack outbox cannot commit" \tr -> do
      update <- setup tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      dbExec tr [sql|UPDATE projects.teams SET disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_test_delivery CHECK (false) NOT VALID|] ()
      Just monitor <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      runTestBgNoReset tr (evaluateQueryMonitorValue monitor frozenTime 84) `shouldThrow` anyException
      Just unchanged <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      (unchanged.currentStatus, unchanged.notificationCount, unchanged.lastEvaluated) `shouldBe` (Monitors.MSNormal, 0, Nothing)
      runTestBgNoReset tr (Monitors.getEvaluations testPid mid frozenTime frozenTime) `shouldReturn` []
      counts <- dbQuery tr [sql|SELECT count(*) FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, Monitors.queryAlertKey mid)
      (counts :: [PGS.Only Int64]) `shouldBe` [PGS.Only 0]
      dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_test_delivery|] ()
      runTestBgNoReset tr $ evaluateQueryMonitorValue unchanged frozenTime 84
      Just committed <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      committed.currentStatus `shouldBe` Monitors.MSAlerting

    it "delivers channel-specific roots, replies, and updates through the notification worker" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      void $ runTestBgNoReset tr $ I.recordIncidentEvent update
      (rootNotifications, _) <- captureNotifs tr runSlackIncidentDeliveries
      let messages ns = [sd | Notify.SlackNotification sd <- ns]
      sort (map (.channelId) (messages rootNotifications)) `shouldBe` ["C1", "C2"]
      map (.threadTs) (messages rootNotifications) `shouldBe` [Nothing, Nothing]
      void $ runTestBgNoReset tr $ I.recordIncidentEvent update{I.observedAt = addUTCTime 60 frozenTime, I.change = I.IncidentRecovered}
      advanceMinutes tr 1
      (replies, _) <- captureNotifs tr runSlackIncidentDeliveries
      let parents = sort [(sd.channelId, sd.threadTs) | sd <- messages replies]
      map snd parents `shouldSatisfy` all isJust
      length (ordNub $ map snd parents) `shouldBe` 2
      (edits, _) <- captureNotifs tr runSlackIncidentDeliveries
      sort [(sd.channelId, KM.lookup "ts" obj) | sd <- messages edits, AE.Object obj <- [sd.payload]]
        `shouldBe` [(cid, AE.String <$> ts) | (cid, ts) <- parents]
      for_ ([("post_root", "monoscope_incident_root", rootNotifications), ("post_reply", "monoscope_incident_delivery", replies), ("update_root", "monoscope_incident_root", edits)] :: [(Text, Text, [Notify.Notification])]) $ \(operation, eventType, notifications) ->
        for_ (messages notifications) $ \message -> do
          message.payload ^? key "metadata" . key "event_type" . _String `shouldBe` Just eventType
          Just metadata <- pure $ message.payload ^? key "metadata" . key "event_payload"
          stored <-
            dbQuery
              tr
              [sql|SELECT jsonb_build_object('root_id', r.id, 'delivery_id', d.id)
            FROM apis.slack_incident_deliveries d JOIN apis.slack_incident_roots r ON r.id = d.root_id
            WHERE r.channel_id = ? AND d.operation = ?|]
              (message.channelId, operation :: Text)
          stored `shouldBe` [PGS.Only $ Aeson metadata]
      (again, _) <- captureNotifs tr runSlackIncidentDeliveries
      again `shouldBe` []

    it "deduplicates destinations and events, orders recovery behind roots, and starts a new episode on recurrence" \tr -> do
      update <- setupWithIssue tr
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
          recoveredAt = addUTCTime 60 update.observedAt
          recovered = update{I.observedAt = recoveredAt, I.change = I.IncidentRecovered, I.destinations = [I.SlackDestination "T1" "C3"]}
      I.Recorded episode event <- run $ I.recordIncidentEvent update
      run (I.recordIncidentEvent update) `shouldReturn` I.AlreadyRecorded episode.id event
      I.Recorded closed recoveryEvent <- run $ I.recordIncidentEvent recovered
      (closed.id, closed.phase, closed.closedAt) `shouldBe` (episode.id, I.EpisodeRecovered, Just recoveredAt)
      (batchA, batchB) <- concurrently (run $ I.claimSlackDeliveries recoveredAt) (run $ I.claimSlackDeliveries recoveredAt)
      let roots = sortOn (.channelId) $ batchA <> batchB
      sort (map (.channelId) roots) `shouldBe` ["C1", "C2"]
      map (.operation) roots `shouldBe` [I.PostRoot, I.PostRoot]
      run (I.claimSlackDeliveries recoveredAt) `shouldReturn` []
      timestamps <- traverse timestamp ["1788900000.000001", "1788900000.000002"]
      for_ (zip roots timestamps) \(delivery, ts) ->
        run (I.finishSlackDelivery recoveredAt delivery (I.DeliveryConfirmed ts)) `shouldReturn` True
      replies <- run $ I.claimSlackDeliveries recoveredAt
      map (.operation) replies `shouldBe` map I.PostReply timestamps
      replyTs <- timestamp "1788900060.000003"
      for_ replies \delivery ->
        run (I.finishSlackDelivery recoveredAt delivery (I.DeliveryConfirmed replyTs)) `shouldReturn` True
      edits <- run $ I.claimSlackDeliveries recoveredAt
      map (.operation) edits `shouldBe` map I.UpdateRoot timestamps
      for_ (zip edits timestamps) \(delivery, ts) ->
        run (I.finishSlackDelivery recoveredAt delivery (I.DeliveryConfirmed ts)) `shouldReturn` True
      I.Recorded recurring _ <- run $ I.recordIncidentEvent update{I.observedAt = addUTCTime 120 update.observedAt}
      recurring.id `shouldNotBe` episode.id
      run (I.recordIncidentEvent recovered) `shouldReturn` I.AlreadyRecorded episode.id recoveryEvent
      fmap (.phase) <$> run (I.getEpisode testPid recurring.id) `shouldReturn` Just I.EpisodeActive

    it "keeps ambiguous roots pending reconciliation and rejects an old worker after a scheduled retry" \tr -> do
      update <- setupWithIssue tr
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
          now = update.observedAt
          retryAt = addUTCTime 30 now
      void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [initial] <- run $ I.claimSlackDeliveries now
      run (I.finishSlackDelivery now initial (I.DeliveryRetry retryAt "ratelimited")) `shouldReturn` True
      run (I.claimSlackDeliveries now) `shouldReturn` []
      [retry] <- run $ I.claimSlackDeliveries retryAt
      retry.leaseToken `shouldNotBe` initial.leaseToken
      ts <- timestamp "1788900030.000001"
      run (I.finishSlackDelivery retryAt initial (I.DeliveryConfirmed ts)) `shouldReturn` False
      run (I.claimSlackDeliveries (addUTCTime 121 retryAt)) `shouldReturn` []
      run (I.finishSlackDelivery (addUTCTime 122 retryAt) retry (I.DeliveryConfirmed ts)) `shouldReturn` True
      run (I.claimSlackDeliveries (addUTCTime 123 retryAt)) `shouldReturn` []

    it "rejects foreign sources, ignores stale transitions, and makes webhook acceptance wait for a root timestamp" \tr -> do
      update <- setupWithIssue tr
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
          now = update.observedAt
      run (I.recordIncidentEvent update{I.projectId = UUIDId $ UUID.fromWords 0 0 0 999}) `shouldReturn` I.Rejected I.UnknownIncidentSource
      run (I.recordIncidentEvent update{I.change = I.IncidentRecovered}) `shouldReturn` I.NoOpenEpisode
      I.Recorded episode _ <- run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      run (I.recordIncidentEvent update{I.observedAt = addUTCTime (-1) now, I.change = I.IncidentRecovered}) `shouldReturn` I.Rejected I.OlderThanCurrentEpisode
      [root] <- run $ I.claimSlackDeliveries now
      run (I.finishSlackDelivery now root (I.WebhookAccepted I.ThreadCapable)) `shouldReturn` True
      void $ run $ I.recordIncidentEvent update{I.observedAt = addUTCTime 60 now, I.change = I.IncidentRecovered, I.destinations = [I.SlackDestination "T1" "C1"]}
      run (I.claimSlackDeliveries (addUTCTime 300 now)) `shouldReturn` []
      run (I.getEpisode (UUIDId $ UUID.fromWords 0 0 0 999) episode.id) `shouldReturn` Nothing

    -- A monitor's issue is its history. Before, a firing that arrived after the
    -- previous one had been acknowledged or archived started a fresh row, so
    -- "how often has this fired" was scattered across issues nobody joined up.
    it "reuses one long-lived issue across firings, and an unexpired ack stays a snooze" \tr -> do
      update <- setup tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      let issueRows :: IO [(Issues.IssueId, Maybe UTCTime, Maybe UTCTime)]
          issueRows = dbQuery tr [sql|SELECT id, acknowledged_until, archived_at FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, Monitors.queryAlertKey mid)
      void $ evaluateMonitor tr mid 84
      [(firstIssue, _, _)] <- issueRows
      -- Archived, then firing again: the same row comes back rather than a second one.
      advanceMinutes tr 1
      dbExec tr [sql|UPDATE apis.issues SET archived_at = ? WHERE id = ?|] (frozenTime, firstIssue)
      void $ evaluateMonitor tr mid 18
      advanceMinutes tr 1
      void $ evaluateMonitor tr mid 84
      issueRows >>= \rows -> map (\(i, _, _) -> i) rows `shouldBe` [firstIssue]
      issueRows >>= \rows -> map (\(_, _, arch) -> arch) rows `shouldBe` [Nothing]
      -- Acknowledged with a live window: a firing inside it must not un-silence
      -- the issue, or "snooze for an hour" would mean nothing to a noisy monitor.
      advanceMinutes tr 1
      let ackUntil = addUTCTime 86400 frozenTime
      dbExec tr [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ? WHERE id = ?|] (frozenTime, ackUntil, firstIssue)
      void $ evaluateMonitor tr mid 18
      advanceMinutes tr 1
      void $ evaluateMonitor tr mid 84
      issueRows >>= \rows -> map (\(i, u, _) -> (i, isJust u)) rows `shouldBe` [(firstIssue, True)]

    -- The issue write bumps occurrence_count and affected_requests, so it must
    -- happen on a transition and not on every evaluation tick. Unguarded, a
    -- monitor alerting once on a 1-minute interval reported one "occurrence" per
    -- minute on its issue page and in every alert body it fed.
    it "counts firings, not evaluation ticks" \tr -> do
      update <- setup tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      let affected :: IO [PGS.Only Int64]
          affected = dbQuery tr [sql|SELECT affected_requests FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, Monitors.queryAlertKey mid)
      void $ evaluateMonitor tr mid 84
      firstCount <- affected
      -- Three more ticks, all still alerting: the status never changes, so none
      -- of them is a new firing.
      for_ [1 :: Int, 2, 3] \_ -> do
        advanceMinutes tr 1
        void $ evaluateMonitor tr mid 84
      affected >>= (`shouldBe` firstCount)

    -- The insert-time guard cannot cover a root that is not threadless *yet*: a
    -- root only becomes threadless when its post_root settles, so an event
    -- arriving before that queues an edit that is unclaimable a moment later —
    -- and one unclaimable row blocks every delivery sequenced behind it.
    it "settles an edit queued before its root turned out to be threadless" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
      void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [root] <- run $ I.claimSlackDeliveries update.observedAt
      -- A second event lands while the root is still unsettled, so its edit is
      -- queued against a root that does not know yet that it cannot be edited.
      advanceMinutes tr 1
      now <- getTestTime tr.trTestClock
      void $ run $ I.recordIncidentEvent update{I.observedAt = now, I.change = I.IncidentReminder}
      run (I.finishSlackDelivery now root (I.WebhookAccepted I.Threadless)) `shouldReturn` True
      -- Claiming settles the stray edit rather than leaving it to block the root.
      void $ run $ I.claimSlackDeliveries now
      stuck <-
        dbQuery
          tr
          [sql|SELECT d.operation, d.state FROM apis.slack_incident_deliveries d
               JOIN apis.slack_incident_roots r ON r.id = d.root_id
               WHERE r.threadless AND d.state NOT IN ('delivered','failed','sending')|]
          ()
      (stuck :: [(Text, Text)]) `shouldBe` []

    -- An episode is only ever read from its issue's page, so one without an issue
    -- is unreachable: it must be refused rather than written and lost.
    -- 104 roots sat in waiting_root in production, and the 49 follow-ups queued
    -- behind them were never claimed at all: an incident posted once and then
    -- went quiet. A webhook install cannot return a timestamp and cannot call
    -- conversations.history to recover one, so the wait never ends.
    it "keeps sending when the destination cannot be threaded" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
      void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [root] <- run $ I.claimSlackDeliveries update.observedAt
      -- Accepted by a transport that can never produce a timestamp.
      run (I.finishSlackDelivery update.observedAt root (I.WebhookAccepted I.Threadless)) `shouldReturn` True
      rootState <- dbQuery tr [sql|SELECT d.state, r.threadless, r.message_ts IS NULL FROM apis.slack_incident_deliveries d JOIN apis.slack_incident_roots r ON r.id = d.root_id WHERE d.id = ?|] (PGS.Only root.id)
      (rootState :: [(Text, Bool, Bool)]) `shouldBe` [("delivered", True, True)]
      -- The follow-up is claimable, and goes out as a standalone post rather
      -- than a reply to a thread that does not exist.
      advanceMinutes tr 1
      now <- getTestTime tr.trTestClock
      void $ run $ I.recordIncidentEvent update{I.observedAt = now, I.change = I.IncidentRecovered}
      followups <- run $ I.claimSlackDeliveries now
      map (.operation) followups `shouldContain` [I.PostStandalone]
      -- Nothing may be left queued that can never be claimed. `shouldContain`
      -- alone passed while an unclaimable update_root sat pending behind the
      -- reply — and because noEarlierUnsettledDelivery blocks on the earliest
      -- unsettled row, that one row silences the whole root from then on. Assert
      -- on the queue itself, not just on what came back.
      stranded <-
        dbQuery
          tr
          [sql|SELECT d.operation, d.state FROM apis.slack_incident_deliveries d
               JOIN apis.slack_incident_roots r ON r.id = d.root_id
               WHERE r.threadless AND d.state NOT IN ('delivered','failed','sending')|]
          ()
      (stranded :: [(Text, Text)]) `shouldBe` []

    -- A bot token is not proof the timestamp is recoverable: the installs behind
    -- the 110 stranded roots had one, with zero scopes.
    it "stops waiting for a timestamp recovery that never arrives" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
      void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [root] <- run $ I.claimSlackDeliveries update.observedAt
      run (I.finishSlackDelivery update.observedAt root (I.WebhookAccepted I.ThreadCapable)) `shouldReturn` True
      advanceMinutes tr 1
      early <- getTestTime tr.trTestClock
      void $ run $ I.recordIncidentEvent update{I.observedAt = early, I.change = I.IncidentRecovered}
      -- Inside the window the root is still worth waiting for, so nothing moves.
      run (I.claimSlackDeliveries early) `shouldReturn` []
      advanceMinutes tr 60
      late <- getTestTime tr.trTestClock
      followups <- run $ I.claimSlackDeliveries late
      -- Standalone, not a root: the install still has a token, so settling it as
      -- ThreadCapable used to park a post_reply row in waiting_root — which the
      -- operation/state CHECK rejects, losing the completion and wedging the root.
      map (.operation) followups `shouldContain` [I.PostStandalone]
      for_ followups \followup -> run (I.finishSlackDelivery late followup (I.WebhookAccepted I.ThreadCapable)) `shouldReturn` True
      unsettled <-
        dbQuery
          tr
          [sql|SELECT d.operation, d.state FROM apis.slack_incident_deliveries d
               WHERE d.state NOT IN ('delivered','failed')|]
          ()
      (unsettled :: [(Text, Text)]) `shouldBe` []

    it "refuses to open an episode with no issue behind it" \tr -> do
      update <- setup tr
      setupSlackData tr testPid "T1"
      runTestBgNoReset tr (I.recordIncidentEvent update) `shouldReturn` I.Rejected I.EpisodeWithoutIssue
      episodes <- dbQuery tr [sql|SELECT count(*) FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, case update.source of I.MonitorIncident m -> m; _ -> error "monitor source")
      (episodes :: [PGS.Only Int64]) `shouldBe` [PGS.Only 0]

    -- Ask the bot in an alert thread and the answer belongs to that alert's issue,
    -- so the issue page's chat and the Slack thread are one conversation.
    it "resolves an alert thread to its issue's conversation, and a loose thread to the thread's own" \tr -> do
      update <- setupWithIssue tr
      setupLinkedSlackData tr testPid "T1"
      Just issueId <- pure update.issueId
      void $ runTestBgNoReset tr $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [root] <- runTestBgNoReset tr $ I.claimSlackDeliveries update.observedAt
      ts <- timestamp "1788900000.000009"
      void $ runTestBgNoReset tr $ I.finishSlackDelivery update.observedAt root (I.DeliveryConfirmed ts)
      runTestBgNoReset tr (I.threadConversationId testPid "T1" "C1" "1788900000.000009")
        `shouldReturn` UUIDId issueId.unUUIDId
      -- No alert root behind it: there is no issue to belong to.
      runTestBgNoReset tr (I.threadConversationId testPid "T1" "C1" "1788900000.000099")
        `shouldReturn` Issues.slackScopedConversationId testPid "T1" "C1" "1788900000.000099"
      let conversation = UUIDId issueId.unUUIDId
      principal <- runQueryEffect tr (Integrations.resolveSlackPrincipal "T1" "U0123ABCDEF" $ Just testPid) >>= maybe (fail "Missing linked principal") pure
      runQueryEffect tr do
        void $ Integrations.bindSlackInvestigation principal "T1" "C1" "1788900000.000009"
        void $ Issues.getOrCreateConversation testPid conversation Issues.CTAnomaly (AE.object [])
        Issues.setConversationTitle testPid conversation "Generated issue title"
      void $ receiveSlackEvent tr $ slackCallbackEnvelope "T1" "EvIncidentTitle" $ AE.object ["type" AE..= ("agent_session_title_changed" :: Text), "team_id" AE..= ("T1" :: Text), "channel" AE..= ("C1" :: Text), "thread_ts" AE..= ("1788900000.000009" :: Text), "user" AE..= ("U0123ABCDEF" :: Text), "event_ts" AE..= ("1788900001.000009" :: Text), "title" AE..= ("Operator incident title" :: Text)]
      runQueryEffect tr (Issues.getConversation testPid conversation) >>= (\thread -> fmap (.title) thread `shouldBe` Just "Operator incident title")
      runQueryEffect tr (Issues.getConversation testPid $ Issues.slackScopedConversationId testPid "T1" "C1" "1788900000.000009") >>= (`shouldSatisfy` isNothing)

    it "captures only this app's signed root in the original workspace and channel" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
      void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [root] <- run $ I.claimSlackDeliveries update.observedAt
      run (I.finishSlackDelivery update.observedAt root (I.WebhookAccepted I.ThreadCapable)) `shouldReturn` True
      for_ ([("EvOtherApp", "T1", "C1", "1788900000.000001", "A_OTHER"), ("EvOtherTeam", "T2", "C1", "1788900000.000001", "A_TEST"), ("EvOtherChannel", "T1", "C2", "1788900000.000001", "A_TEST"), ("EvBadTs", "T1", "C1", "invalid", "A_TEST")] :: [(Text, Text, Text, Text, Text)]) \(eid, workspace, channel, ts, author) -> do
        receipt <- receiveSlackEvent tr $ slackRootEvent eid workspace channel root.rootId ts author
        run (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` False
      receipt <- receiveSlackEvent tr $ slackRootEvent "EvOwnRoot" "T1" "C1" root.rootId "1788900000.000001" "A_TEST"
      run (I.captureSlackRoot "" receipt) `shouldReturn` False
      run (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` True
      run (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` True
      conflict <- receiveSlackEvent tr $ slackRootEvent "EvConflict" "T1" "C1" root.rootId "1788900000.000002" "A_TEST"
      run (I.captureSlackRoot "A_TEST" conflict) `shouldReturn` False

    for_ [I.WebhookAccepted I.ThreadCapable, I.DeliveryUncertain "lost_ack"] $ \accepted ->
      it ("recovers an incident root through leased history pages after " <> show accepted) \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let resources = tr{trATCtx = tr.trATCtx{Config.env = tr.trATCtx.env{Config.slackAppId = "A_TEST"}}}
            run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset resources
        void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
        [root] <- run $ I.claimSlackDeliveries frozenTime
        run (I.finishSlackDelivery frozenTime root accepted) `shouldReturn` True
        void $ run $ I.recordIncidentEvent update{I.observedAt = addUTCTime 60 frozenTime, I.change = I.IncidentRecovered, I.destinations = [I.SlackDestination "T1" "C1"]}
        dbExec tr [sql|UPDATE apis.slack_incident_deliveries SET history_retry_at = ? WHERE id = ?|] (frozenTime, root.id)
        [abandoned] <- run $ I.claimIncidentSearches frozenTime
        AE.Object fields <- maybe (fail "Expected root event") pure $ slackRootEvent "unused" "T1" "C1" root.rootId "1788900000.000001" "A_TEST" ^? key "event"
        let message = AE.Object fields
            page messages next = AE.object ["ok" AE..= True, "messages" AE..= (messages :: [AE.Value]), "response_metadata" AE..= AE.object ["next_cursor" AE..= (next :: Text)]]
            wrong = page [message & key "app_id" . _String .~ "A_OTHER", AE.Object $ KM.insert "thread_ts" (AE.String "1788900000.000002") fields, message & key "metadata" . key "event_payload" . key "root_id" . _String .~ "00000000-0000-0000-0000-000000000000"] "next"
            matching = page [message] ""
        pages <- newIORef [(Nothing, wrong), (Just "next", matching), (Just "next", matching)]
        replaceInstallation <- newIORef True
        let work = do
              now <- getTestTime tr.trTestClock
              runTestBgRecordingHTTP now resources
                $ withHTTPResponses
                  ( \opts endpoint -> do
                      endpoint `shouldBe` "https://slack.com/api/conversations.history"
                      opts ^. Wreq.param "channel" `shouldBe` ["C1"]
                      opts ^. Wreq.param "include_all_metadata" `shouldBe` ["true"]
                      opts ^. Wreq.param "limit" `shouldBe` ["15"]
                      next <- atomicModifyIORef' pages $ \case
                        [] -> ([], Nothing)
                        item : rest -> (rest, Just item)
                      (cursor, body) <- maybe (fail "Unexpected root history request") pure next
                      opts ^. Wreq.param "cursor" `shouldBe` maybeToList cursor
                      when (cursor == Just "next") do
                        replace <- atomicModifyIORef' replaceInstallation (\flag -> (False, flag))
                        when replace $ dbExec tr [sql|UPDATE apis.slack SET team_id = 'T_OTHER' WHERE project_id = ?|] (PGS.Only testPid)
                      pure $ Just $ AE.encode body
                  )
                  SlackPage.reconcileIncidentDeliveries
        (leased, _) <- work
        leased `shouldBe` []
        advanceMinutes tr 3
        (searched, _) <- work
        map fst searched `shouldBe` ["https://slack.com/api/conversations.history"]
        run $ I.saveIncidentSearchCursor abandoned (I.SearchAdvanced (Just "stale")) frozenTime
        cursors <- dbQuery tr [sql|SELECT history_cursor FROM apis.slack_incident_deliveries WHERE id = ?|] (PGS.Only root.id)
        cursors `shouldBe` [PGS.Only (Just "next" :: Maybe Text)]
        advanceMinutes tr 1
        void work
        run (I.claimSlackDeliveries (addUTCTime 300 frozenTime)) `shouldReturn` []
        dbExec tr [sql|UPDATE apis.slack SET team_id = 'T1' WHERE project_id = ?|] (PGS.Only testPid)
        advanceMinutes tr 1
        void work
        [reply] <- run $ I.claimSlackDeliveries (addUTCTime 300 frozenTime)
        expected <- timestamp "1788900000.000001"
        reply.operation `shouldBe` I.PostReply expected
        readIORef pages >>= (`shouldBe` [])

    for_ [False, True] $ \lostAck ->
      it ("reconciles lifecycle replies and edits after " <> if lostAck then "lost acknowledgements" else "expired send leases") \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let resources = tr{trATCtx = tr.trATCtx{Config.env = tr.trATCtx.env{Config.slackAppId = "A_TEST"}}}
            run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset resources
        void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
        [root] <- run $ I.claimSlackDeliveries frozenTime
        parent <- timestamp "1788900000.000001"
        run (I.finishSlackDelivery frozenTime root $ I.DeliveryConfirmed parent) `shouldReturn` True
        void $ run $ I.recordIncidentEvent update{I.observedAt = addUTCTime 60 frozenTime, I.change = I.IncidentRecovered, I.destinations = [I.SlackDestination "T1" "C1"]}
        advanceMinutes tr 1
        replicateM_ 2 do
          now <- getTestTime tr.trTestClock
          [delivery] <- run $ I.claimSlackDeliveries now
          when lostAck $ run (I.finishSlackDelivery now delivery $ I.DeliveryUncertain "lost_ack") >>= (`shouldBe` True)
          dbExec tr [sql|UPDATE apis.slack_incident_deliveries SET history_retry_at = ? WHERE id = ?|] (now, delivery.id)
          unless lostAck do
            run (I.claimIncidentSearches now) >>= (\searches -> length searches `shouldBe` 0)
            advanceMinutes tr 3
          claimedAt <- getTestTime tr.trTestClock
          [abandoned] <- run $ I.claimIncidentSearches claimedAt
          (method, expectedTs, expectedCursor) <- case delivery.operation of
            I.PostReply _ -> pure ("conversations.replies", "1788900000.000002", Just "next")
            I.UpdateRoot _ -> pure ("conversations.history", "1788900000.000001", Nothing)
            I.PostRoot -> fail "Expected a lifecycle delivery"
            I.PostStandalone -> fail "Expected a lifecycle delivery"
            I.PostAnalysis{} -> fail "Expected an incident fact delivery"
          Just metadata <- pure $ AE.toJSON (I.correlateSlackDelivery delivery delivery.payload) ^? key "metadata"
          let message = AE.object ["ts" AE..= (expectedTs :: Text), "thread_ts" AE..= I.slackTimestampText parent, "app_id" AE..= ("A_TEST" :: Text), "bot_id" AE..= ("B_TEST" :: Text), "metadata" AE..= metadata]
              page messages cursor = AE.object ["ok" AE..= True, "messages" AE..= (messages :: [AE.Value]), "response_metadata" AE..= AE.object ["next_cursor" AE..= fromMaybe "" (cursor :: Maybe Text)]]
              wrong = message & key "metadata" . key "event_payload" . key "delivery_id" . _String .~ "00000000-0000-0000-0000-000000000000"
              wrongThread = message & key "thread_ts" . _String .~ "1788900999.000001"
              wrongApp = message & key "app_id" . _String .~ "A_OTHER"
              wrongRoot = message & key "metadata" . key "event_payload" . key "root_id" . _String .~ "00000000-0000-0000-0000-000000000000"
          pages <- newIORef [(Nothing, page [wrong, wrongThread, wrongApp, wrongRoot] expectedCursor), (expectedCursor, page [message] Nothing)]
          let work = do
                at <- getTestTime tr.trTestClock
                runTestBgRecordingHTTP at resources
                  $ withHTTPResponses
                    ( \opts endpoint -> do
                        endpoint `shouldBe` "https://slack.com/api/" <> method
                        opts ^. Wreq.param "channel" `shouldBe` ["C1"]
                        opts ^. Wreq.param "include_all_metadata" `shouldBe` ["true"]
                        case delivery.operation of
                          I.PostReply _ -> opts ^. Wreq.param "ts" `shouldBe` [I.slackTimestampText parent]
                          I.PostAnalysis{} -> fail "Expected an incident fact delivery"
                          I.UpdateRoot _ -> do
                            opts ^. Wreq.param "oldest" `shouldBe` [I.slackTimestampText parent]
                            opts ^. Wreq.param "latest" `shouldBe` [I.slackTimestampText parent]
                            opts ^. Wreq.param "inclusive" `shouldBe` ["true"]
                          I.PostRoot -> fail "Unexpected root search"
                          I.PostStandalone -> fail "Unexpected root search"
                        next <- atomicModifyIORef' pages $ \case
                          [] -> ([], Nothing)
                          item : rest -> (rest, Just item)
                        (cursor, body) <- maybe (fail "Unexpected lifecycle history request") pure next
                        opts ^. Wreq.param "cursor" `shouldBe` maybeToList cursor
                        pure $ Just $ AE.encode body
                    )
                    SlackPage.reconcileIncidentDeliveries
          advanceMinutes tr 3
          (unmatched, _) <- work
          map fst unmatched `shouldBe` ["https://slack.com/api/" <> toText method]
          found <- timestamp expectedTs
          run (I.confirmIncidentSearch abandoned found) `shouldReturn` False
          pendingAt <- getTestTime tr.trTestClock
          run (I.claimSlackDeliveries pendingAt) `shouldReturn` []
          advanceMinutes tr 1
          (matched, _) <- work
          map fst matched `shouldBe` ["https://slack.com/api/" <> toText method]
          stored <- dbQuery tr [sql|SELECT state, message_ts FROM apis.slack_incident_deliveries WHERE id = ?|] (PGS.Only delivery.id)
          stored `shouldBe` [("delivered" :: Text, Just expectedTs)]
          readIORef pages >>= (`shouldBe` [])
        completedAt <- getTestTime tr.trTestClock
        run (I.claimSlackDeliveries completedAt) `shouldReturn` []


data IncidentInterruption
  = NoIncidentOptIn
  | PauseBeforeIncidentRun
  | PauseDuringIncidentModel
  | RevokeDuringIncidentModel
  | RestrictDuringIncidentModel
  | PauseBeforeIncidentReport
  | RevokeBeforeIncidentReport
  | ReplaceBeforeIncidentReport
  deriving stock (Bounded, Enum, Eq, Show)


setup :: TestResources -> IO I.IncidentUpdate
setup tr = do
  let mid = Monitors.QueryMonitorId $ UUID.fromWords 0 0 0 950
      now = frozenTime
  void $ runTestBgNoReset tr $ Monitors.queryMonitorUpsert def{Monitors.id = mid, Monitors.projectId = testPid, Monitors.alertThreshold = 60, Monitors.checkIntervalMins = 1, Monitors.timeWindowMins = 1}
  payload <- maybe (fail "expected an object payload") pure $ I.slackPayload $ AE.object ["text" AE..= ("Measured threshold exceeded" :: Text)]
  pure
    I.IncidentUpdate
      { I.projectId = testPid
      , I.source = I.MonitorIncident mid
      , I.observedAt = now
      , I.change = I.IncidentAlert
      , I.delivery = I.PublishIncident
      , I.issueId = Nothing
      , I.rootPayload = payload
      , I.replyPayload = payload
      , I.destinations = [I.SlackDestination "T1" "C1", I.SlackDestination "T1" "C2", I.SlackDestination "T1" "C1"]
      }


-- | The same update, carrying the monitor's issue.
--
-- An episode cannot open without an issue, so a fixture that records an alert
-- directly has to commit the row 'commitQueryMonitorEvaluation' commits for it.
-- 'setup' deliberately stops short of that: the rollback spec asserts the issue
-- is created inside the outbox transaction and vanishes with it.
setupWithIssue :: TestResources -> IO I.IncidentUpdate
setupWithIssue tr = do
  update <- setup tr
  I.MonitorIncident mid <- pure update.source
  issue <- runTestBgNoReset tr do
    created <- Issues.createQueryAlertIssue testPid (Monitors.queryAlertKey mid) "Measured threshold exceeded" "" 60 84 Issues.Above
    Issues.insertIssue created
    pure created
  pure (update{I.issueId = Just issue.id} :: I.IncidentUpdate)


timestamp :: Text -> IO I.SlackTimestamp
timestamp = maybe (fail "invalid fixture timestamp") pure . I.slackTimestamp


evaluateMonitor :: TestResources -> Monitors.QueryMonitorId -> Double -> IO [Notify.Notification]
evaluateMonitor tr mid value =
  fst <$> captureNotifs tr do
    now <- Time.currentTime
    current <- Monitors.queryMonitorById mid
    for_ current \monitor -> evaluateQueryMonitorValue monitor now value
    replicateM_ 3 runSlackIncidentDeliveries
