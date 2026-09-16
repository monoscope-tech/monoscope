module IncidentDeliverySpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors, evaluateQueryMonitorValue, notifyErrorSubscriptions, processProjectErrors, runSlackIncidentDeliveries)
import Control.Lens ((.~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, _String)
import Data.Default (def)
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.Notify qualified as Notify
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns qualified as Errors
import Models.Apis.Incidents qualified as I
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Pages.Issues qualified as IssuesPage
import Pages.Bots.BotTestHelpers (receiveSlackEvent, setupSlackData, slackRootEvent, withHTTPResponses)
import Pages.Bots.Slack qualified as SlackPage
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant qualified
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)
import Test.Hspec
import UnliftIO.Async (concurrently)


spec :: Spec
spec = around withTestResources do
  describe "Incident delivery" do
    it "commits ingested error notification claims with two destination roots and retries an outbox failure" \tr -> do
      setupSlackData tr testPid "T1"
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2','C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
        void $ PGS.execute conn [sql|UPDATE projects.projects SET error_alerts = true WHERE id = ?|] (PGS.Only testPid)
        void $ PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_ingested_delivery CHECK (false) NOT VALID|]
      let sample = def{Errors.when = frozenTime, Errors.hash = "ingested-outbox", Errors.errorType = "CheckoutError", Errors.message = "Payment failed"}
          ingest = captureNotifs tr $ processProjectErrors testPid [sample] frozenTime
          messages ns = [sd | Notify.SlackNotification sd <- ns]
      ingest `shouldThrow` anyException
      Just unnotified <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
      unnotified.lastNotifiedAt `shouldBe` Nothing
      tokens <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT count(*) FROM apis.notification_rate_limit WHERE project_id = ?|] (PGS.Only testPid)
      (tokens :: [PGS.Only Int64]) `shouldBe` [PGS.Only 0]
      slots <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT last_notified_at FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, sample.hash)
      (slots :: [PGS.Only (Maybe UTCTime)]) `shouldBe` [PGS.Only Nothing]
      void $ withResource tr.trPool \conn -> PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_ingested_delivery|]
      ((batchA, _), (batchB, _)) <- concurrently ingest ingest
      let initial = messages $ batchA <> batchB
      sort (map (.channelId) initial) `shouldBe` ["C1", "C2"]
      map (.threadTs) initial `shouldBe` [Nothing, Nothing]
      Just notified <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
      notified.lastNotifiedAt `shouldSatisfy` isJust
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.error_patterns SET subscribed = true, notify_every_minutes = 1 WHERE id = ?|] (PGS.Only notified.id)
      advanceMinutes tr 2
      (reminder, _) <- captureNotifs tr do
        notifyErrorSubscriptions testPid [sample.hash]
        replicateM_ 2 runSlackIncidentDeliveries
      let updates = messages reminder
      length updates `shouldBe` 4
      length [sd | sd <- updates, isJust sd.threadTs] `shouldBe` 2
      roots <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT count(*) FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.source_kind = 'error' AND e.source_id = ?|] (PGS.Only notified.id)
      (roots :: [PGS.Only Int64]) `shouldBe` [PGS.Only 2]
      -- Quiet counters alone cannot close an error or establish recovery.
      void $ withResource tr.trPool \conn ->
        PGS.execute
          conn
          [sql|UPDATE apis.error_patterns SET quiet_minutes = resolution_threshold_minutes + 1,
          occurrences_1m = 0, occurrences_5m = 0, occurrences_1h = 0, occurrences_24h = 0 WHERE id = ?|]
          (PGS.Only notified.id)
      now <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ Errors.updateOccurrenceCountsBatch [testPid] now
      Just quietPattern <- runTestBgNoReset tr $ Errors.getErrorPatternById notified.id
      (quietPattern.state, isNothing quietPattern.resolvedAt, quietPattern.resolvedBy) `shouldBe` (notified.state, True, Nothing)
      -- A newer acknowledged issue must win over an older unnotified escalation.
      withResource tr.trPool \conn ->
        void
          $ PGS.execute
            conn
            [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ? WHERE project_id = ? AND target_hash = ?|]
            (frozenTime, addUTCTime 3600 frozenTime, testPid, sample.hash)
      newer <- runTestBgNoReset tr $ Issues.createNewErrorIssue testPid notified >>= Issues.insertIssueReturningId
      withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ?, last_notified_at = ? WHERE id = ?|]
            (frozenTime, addUTCTime 3600 frozenTime, frozenTime, newer)
        void
          $ PGS.execute
            conn
            [sql|UPDATE apis.issues SET acknowledged_at = NULL, acknowledged_until = NULL, last_notified_at = NULL WHERE project_id = ? AND target_hash = ? AND id <> ?|]
            (testPid, sample.hash, newer)
        void $ PGS.execute conn [sql|UPDATE apis.error_patterns SET state = 'escalating' WHERE id = ?|] (PGS.Only notified.id)
      advanceMinutes tr 1
      (silenced, _) <- captureNotifs tr $ notifyErrorSubscriptions testPid [sample.hash]
      messages silenced `shouldBe` []
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid notified.id.unErrorPatternId
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
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.issues SET archived_at = ? WHERE id = ?|] (frozenTime, issue.id)
      advanceMinutes tr 1
      nextIssue <- runTestBgNoReset tr do
        created <- Issues.createNewErrorIssue testPid err
        Issues.insertIssue created
        pure created
      nextIssue.id `shouldNotBe` issue.id
      now <- getTestTime tr.trTestClock
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.projectId = UUIDId $ UUID.fromWords 0 0 0 999}) `shouldReturn` I.UnknownIncidentSource
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.issueId = Nothing}) `shouldReturn` I.IncidentIssueMismatch
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.issues SET target_hash = 'another-error' WHERE id = ?|] (PGS.Only nextIssue.id)
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.issueId = Just nextIssue.id, I.observedAt = now}) `shouldReturn` I.IncidentIssueMismatch
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.issues SET target_hash = ? WHERE id = ?|] (sample.hash, nextIssue.id)
      I.Recorded ongoing _ <- runTestBgNoReset tr $ I.recordIncidentEvent initial{I.issueId = Just nextIssue.id, I.observedAt = now, I.change = I.IncidentReminder}
      (ongoing.id, ongoing.issueId) `shouldBe` (episode.id, Just issue.id)
      void $ captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      advanceMinutes tr 1
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
      (resolution, _) <- captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      let messages = [sd | Notify.SlackNotification sd <- resolution]
      length messages `shouldBe` 4
      for_ messages \sd -> do
        let body = decodeUtf8 @Text $ toStrict $ AE.encode sd.payload
        body `shouldSatisfy` T.isInfixOf issue.id.toText
        body `shouldSatisfy` not . T.isInfixOf nextIssue.id.toText
      roots <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT channel_id FROM apis.slack_incident_roots WHERE episode_id = ? ORDER BY channel_id|] (PGS.Only episode.id)
      roots `shouldBe` [PGS.Only ("C1" :: Text), PGS.Only "C2"]
      closedAt <- getTestTime tr.trTestClock
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.observedAt = addUTCTime 1 closedAt}) `shouldReturn` I.InactiveIncidentSource
      runTestBgNoReset tr (I.recordIncidentEvent initial) `shouldReturn` I.AlreadyRecorded episode.id firstEvent
      advanceMinutes tr 1
      againAt <- getTestTime tr.trTestClock
      let canonicalSample = sample{Errors.hash = "canonical-error"} :: Errors.ATError
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample{Errors.when = againAt}, canonicalSample] againAt
      Just canonical <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid canonicalSample.hash
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.error_patterns SET canonical_id = ? WHERE id = ?|] (canonical.id, err.id)
      runTestBgNoReset tr (I.recordIncidentEvent initial{I.observedAt = againAt}) `shouldReturn` I.InactiveIncidentSource
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.error_patterns SET canonical_id = NULL WHERE id = ?|] (PGS.Only err.id)
      I.Recorded recurring _ <- runTestBgNoReset tr $ I.recordIncidentEvent initial{I.issueId = Just nextIssue.id, I.observedAt = againAt, I.rootPayload = payload nextIssue.id}
      recurring.id `shouldNotBe` episode.id
      void $ captureNotifs tr runSlackIncidentDeliveries
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|DELETE FROM apis.issues WHERE id = ?|] (PGS.Only nextIssue.id)
      advanceMinutes tr 1
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId
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
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId
      Just denied <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
      denied.state `shouldBe` Errors.ESNew
      let resolve pid = runTestBgNoReset tr $ I.resolveErrorIncident pid err.id actor.id (addUTCTime 60 frozenTime) (const update.rootPayload)
      resolve testPid `shouldReturn` I.ErrorResolutionDenied
      resolve (UUIDId $ UUID.fromWords 0 0 0 999) `shouldReturn` I.ErrorResolutionDenied
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.error_patterns SET assignee_id = ? WHERE id = ?|] (actor.id, err.id)
      runTestBgNoReset tr (I.resolveErrorIncident testPid err.id actor.id (addUTCTime (-1) frozenTime) (const update.rootPayload))
        `shouldReturn` I.ErrorResolutionConflict I.OlderThanCurrentEpisode
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE projects.project_members SET active = false WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      resolve testPid `shouldReturn` I.ErrorResolutionDenied
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql|UPDATE projects.project_members SET active = true WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
        void $ PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_resolution CHECK (false) NOT VALID|]
      testServant tr (IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId) `shouldThrow` anyException
      Just rolledBack <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
      (rolledBack.state, rolledBack.resolvedAt, rolledBack.resolvedBy) `shouldBe` (Errors.ESNew, Nothing, Nothing)
      activityBefore <- runTestBgNoReset tr $ Issues.selectIssueActivity testPid issue.id
      activityBefore `shouldSatisfy` all (\a -> a.event `notElem` map Issues.Lifecycle [Issues.IEResolved, Issues.IEAutoResolved])
      void $ withResource tr.trPool \conn -> PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_resolution|]
      void
        $ concurrently
          (testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId)
          (testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId)
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId
      events <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT event_kind, actor_id FROM apis.incident_events WHERE episode_id = ? AND event_kind <> 'alert'|] (PGS.Only episode.id)
      events `shouldBe` [("resolved" :: Text, Just actor.id)]
      activity <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT event, created_by FROM apis.issue_activity_log WHERE issue_id = ? AND event IN ('resolved', 'auto_resolved')|] (PGS.Only issue.id)
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
      activityAfter <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT event, created_by FROM apis.issue_activity_log WHERE issue_id = ? AND event IN ('resolved', 'auto_resolved') ORDER BY created_at|] (PGS.Only issue.id)
      activityAfter `shouldBe` [("resolved" :: Text, Just actor.id), ("auto_resolved", Nothing)]
      void $ runTestBgNoReset tr $ Errors.updateErrorPatternState err.id Errors.ESOngoing now
      advanceMinutes tr 1
      autoAt <- getTestTime tr.trTestClock
      -- A SQL state transition can retain the previous resolved_at; activity must use this transition's clock.
      void $ runHasqlEffect tr $ EHasql.interpExecute [HI.sql|UPDATE apis.error_patterns SET state = 'resolved' WHERE id = #{err.id}|]
      autoTimes <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT created_at FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'auto_resolved' ORDER BY created_at DESC LIMIT 1|] (PGS.Only issue.id)
      autoTimes `shouldBe` [PGS.Only autoAt]
      let legacy = sample{Errors.hash = "legacy-resolution"} :: Errors.ATError
      void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [legacy] now
      Just legacyErr <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid legacy.hash
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE projects.project_members SET permission = 'edit' WHERE project_id = ? AND user_id = ?|] (testPid, actor.id)
      let deliveryCount = withResource tr.trPool \conn -> PGS.query_ conn [sql|SELECT count(*) FROM apis.slack_incident_deliveries|] :: IO [PGS.Only Int64]
      beforeLegacy <- deliveryCount
      void $ testServant tr $ IssuesPage.resolveErrorPostH testPid legacyErr.id.unErrorPatternId
      deliveryCount `shouldReturn` beforeLegacy
      Just legacyResolved <- runTestBgNoReset tr $ Errors.getErrorPatternById legacyErr.id
      (legacyResolved.state, legacyResolved.resolvedBy) `shouldBe` (Errors.ESResolved, Just actor.id)

    it "marks missing monitor data in existing threads and waits for a measured recovery" \tr -> do
      update <- setupWithIssue tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
        void $ PGS.execute conn [sql|UPDATE monitors.query_monitors SET alert_threshold = 60, log_query = 'name == "missing-monitor-reading" | summarize max(duration)', check_interval_mins = 1 WHERE id = ?|] (PGS.Only mid)
      let messages ns = [sd | Notify.SlackNotification sd <- ns]
          check = fst <$> captureNotifs tr (checkTriggeredQueryMonitors >> replicateM_ 2 runSlackIncidentDeliveries)
      runTestBgNoReset tr (I.recordIncidentEvent update{I.change = I.IncidentDataUnavailable}) `shouldReturn` I.NoOpenEpisode
      initial <- messages <$> evaluateMonitor tr mid 84
      length initial `shouldBe` 2
      advanceMinutes tr 2
      void $ withResource tr.trPool \conn -> PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_data_gap CHECK (false) NOT VALID|]
      check `shouldReturn` []
      Just uncommitted <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      uncommitted.lastEvaluated `shouldBe` Just frozenTime
      void $ withResource tr.trPool \conn -> PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_data_gap|]
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
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE monitors.query_monitors SET log_query = 'SELECT * FROM missing_monitor_table' WHERE id = ?|] (PGS.Only mid)
      advanceMinutes tr 1
      failed <- messages <$> check
      length failed `shouldBe` 4
      for_ failed \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "The evaluation failed" . decodeUtf8 . toStrict)
      advanceMinutes tr 1
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE monitors.query_monitors SET stop_after_count = 3 WHERE id = ?|] (PGS.Only mid)
      void $ evaluateMonitor tr mid 84
      advanceMinutes tr 1
      limited <- messages <$> check
      length [sd | sd <- limited, isJust sd.threadTs] `shouldBe` 0
      advanceMinutes tr 1
      recovered <- messages <$> evaluateMonitor tr mid 18
      length recovered `shouldBe` 4
      for_ recovered \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "RECOVERED" . decodeUtf8 . toStrict)
      roots <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT count(*) FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.source_kind = 'monitor' AND e.source_id = ?|] (PGS.Only mid)
      (roots :: [PGS.Only Int64]) `shouldBe` [PGS.Only 2]

    it "keeps warning, escalation, and recovery in two monitor threads and preserves the onset snapshot" \tr -> do
      update <- setupWithIssue tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2','C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
        void $ PGS.execute conn [sql|UPDATE monitors.query_monitors SET alert_threshold = 60, warning_threshold = 40 WHERE id = ?|] (PGS.Only mid)
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
      episodes <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid)
      (episodes :: [PGS.Only Text]) `shouldBe` [PGS.Only "recovered"]
      advanceMinutes tr 1
      recurring <- slackMessages <$> evaluate 84
      length recurring `shouldBe` 2
      map (.threadTs) recurring `shouldBe` [Nothing, Nothing]

    it "records recovery while muted and refreshes the root when delivery resumes" \tr -> do
      update <- setupWithIssue tr
      I.MonitorIncident mid <- pure update.source
      setupSlackData tr testPid "T1"
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
      void $ evaluateMonitor tr mid 84
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE monitors.query_monitors SET muted_until = ? WHERE id = ?|] (addUTCTime 180 frozenTime, mid)
      advanceMinutes tr 1
      evaluateMonitor tr mid 18 `shouldReturn` []
      phases <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid)
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
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql|UPDATE projects.teams SET disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (PGS.Only testPid)
        void $ PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_test_delivery CHECK (false) NOT VALID|]
      Just monitor <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      runTestBgNoReset tr (evaluateQueryMonitorValue monitor frozenTime 84) `shouldThrow` anyException
      Just unchanged <- runTestBgNoReset tr $ Monitors.queryMonitorById mid
      (unchanged.currentStatus, unchanged.notificationCount, unchanged.lastEvaluated) `shouldBe` (Monitors.MSNormal, 0, Nothing)
      runTestBgNoReset tr (Monitors.getEvaluations testPid mid frozenTime frozenTime) `shouldReturn` []
      counts <- withResource tr.trPool \conn -> PGS.query conn [sql|SELECT count(*) FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, show @Text mid)
      (counts :: [PGS.Only Int64]) `shouldBe` [PGS.Only 0]
      void $ withResource tr.trPool \conn -> PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_test_delivery|]
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
          stored <- withResource tr.trPool $ \conn ->
            PGS.query
              conn
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
      run (I.recordIncidentEvent update{I.projectId = UUIDId $ UUID.fromWords 0 0 0 999}) `shouldReturn` I.UnknownIncidentSource
      run (I.recordIncidentEvent update{I.change = I.IncidentRecovered}) `shouldReturn` I.NoOpenEpisode
      I.Recorded episode _ <- run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      run (I.recordIncidentEvent update{I.observedAt = addUTCTime (-1) now, I.change = I.IncidentRecovered}) `shouldReturn` I.OlderThanCurrentEpisode
      [root] <- run $ I.claimSlackDeliveries now
      run (I.finishSlackDelivery now root I.WebhookAccepted) `shouldReturn` True
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
          issueRows = withResource tr.trPool \conn ->
            PGS.query conn [sql|SELECT id, acknowledged_until, archived_at FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, show @Text mid)
      void $ evaluateMonitor tr mid 84
      [(firstIssue, _, _)] <- issueRows
      -- Archived, then firing again: the same row comes back rather than a second one.
      advanceMinutes tr 1
      void $ withResource tr.trPool \conn -> PGS.execute conn [sql|UPDATE apis.issues SET archived_at = ? WHERE id = ?|] (frozenTime, firstIssue)
      void $ evaluateMonitor tr mid 18
      advanceMinutes tr 1
      void $ evaluateMonitor tr mid 84
      issueRows >>= \rows -> map (\(i, _, _) -> i) rows `shouldBe` [firstIssue]
      issueRows >>= \rows -> map (\(_, _, arch) -> arch) rows `shouldBe` [Nothing]
      -- Acknowledged with a live window: a firing inside it must not un-silence
      -- the issue, or "snooze for an hour" would mean nothing to a noisy monitor.
      advanceMinutes tr 1
      let ackUntil = addUTCTime 86400 frozenTime
      void $ withResource tr.trPool \conn ->
        PGS.execute conn [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ? WHERE id = ?|] (frozenTime, ackUntil, firstIssue)
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
          affected = withResource tr.trPool \conn ->
            PGS.query conn [sql|SELECT affected_requests FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, show @Text mid)
      void $ evaluateMonitor tr mid 84
      firstCount <- affected
      -- Three more ticks, all still alerting: the status never changes, so none
      -- of them is a new firing.
      for_ [1 :: Int, 2, 3] \_ -> do
        advanceMinutes tr 1
        void $ evaluateMonitor tr mid 84
      affected >>= (`shouldBe` firstCount)

    -- An episode is only ever read from its issue's page, so one without an issue
    -- is unreachable: it must be refused rather than written and lost.
    it "refuses to open an episode with no issue behind it" \tr -> do
      update <- setup tr
      setupSlackData tr testPid "T1"
      runTestBgNoReset tr (I.recordIncidentEvent update) `shouldReturn` I.EpisodeWithoutIssue
      episodes <- withResource tr.trPool \conn ->
        PGS.query conn [sql|SELECT count(*) FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, case update.source of I.MonitorIncident m -> m; _ -> error "monitor source")
      (episodes :: [PGS.Only Int64]) `shouldBe` [PGS.Only 0]

    -- Ask the bot in an alert thread and the answer belongs to that alert's issue,
    -- so the issue page's chat and the Slack thread are one conversation.
    it "resolves an alert thread to its issue's conversation, and a loose thread to the thread's own" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
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

    it "captures only this app's signed root in the original workspace and channel" \tr -> do
      update <- setupWithIssue tr
      setupSlackData tr testPid "T1"
      let run :: ATBackgroundCtx a -> IO a
          run = runTestBgNoReset tr
      void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
      [root] <- run $ I.claimSlackDeliveries update.observedAt
      run (I.finishSlackDelivery update.observedAt root I.WebhookAccepted) `shouldReturn` True
      for_ ([("EvOtherApp", "T1", "C1", "1788900000.000001", "A_OTHER"), ("EvOtherTeam", "T2", "C1", "1788900000.000001", "A_TEST"), ("EvOtherChannel", "T1", "C2", "1788900000.000001", "A_TEST"), ("EvBadTs", "T1", "C1", "invalid", "A_TEST")] :: [(Text, Text, Text, Text, Text)]) \(eid, workspace, channel, ts, author) -> do
        receipt <- receiveSlackEvent tr $ slackRootEvent eid workspace channel root.rootId ts author
        run (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` False
      receipt <- receiveSlackEvent tr $ slackRootEvent "EvOwnRoot" "T1" "C1" root.rootId "1788900000.000001" "A_TEST"
      run (I.captureSlackRoot "" receipt) `shouldReturn` False
      run (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` True
      run (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` True
      conflict <- receiveSlackEvent tr $ slackRootEvent "EvConflict" "T1" "C1" root.rootId "1788900000.000002" "A_TEST"
      run (I.captureSlackRoot "A_TEST" conflict) `shouldReturn` False

    for_ [I.WebhookAccepted, I.DeliveryUncertain "lost_ack"] $ \accepted ->
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
        withResource tr.trPool $ \conn -> void $ PGS.execute conn [sql|UPDATE apis.slack_incident_deliveries SET history_retry_at = ? WHERE id = ?|] (frozenTime, root.id)
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
                        when replace $ withResource tr.trPool $ \conn -> void $ PGS.execute conn [sql|UPDATE apis.slack SET team_id = 'T_OTHER' WHERE project_id = ?|] (PGS.Only testPid)
                      pure $ Just $ AE.encode body
                  )
                  SlackPage.reconcileIncidentDeliveries
        (leased, _) <- work
        leased `shouldBe` []
        advanceMinutes tr 3
        (searched, _) <- work
        map fst searched `shouldBe` ["https://slack.com/api/conversations.history"]
        run $ I.saveIncidentSearchCursor abandoned (Just "stale") frozenTime
        cursors <- withResource tr.trPool $ \conn -> PGS.query conn [sql|SELECT history_cursor FROM apis.slack_incident_deliveries WHERE id = ?|] (PGS.Only root.id)
        cursors `shouldBe` [PGS.Only (Just "next" :: Maybe Text)]
        advanceMinutes tr 1
        void work
        run (I.claimSlackDeliveries (addUTCTime 300 frozenTime)) `shouldReturn` []
        withResource tr.trPool $ \conn -> void $ PGS.execute conn [sql|UPDATE apis.slack SET team_id = 'T1' WHERE project_id = ?|] (PGS.Only testPid)
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
          withResource tr.trPool $ \conn -> void $ PGS.execute conn [sql|UPDATE apis.slack_incident_deliveries SET history_retry_at = ? WHERE id = ?|] (now, delivery.id)
          unless lostAck do
            run (I.claimIncidentSearches now) >>= (\searches -> length searches `shouldBe` 0)
            advanceMinutes tr 3
          claimedAt <- getTestTime tr.trTestClock
          [abandoned] <- run $ I.claimIncidentSearches claimedAt
          (method, expectedTs, expectedCursor) <- case delivery.operation of
            I.PostReply _ -> pure ("conversations.replies", "1788900000.000002", Just "next")
            I.UpdateRoot _ -> pure ("conversations.history", "1788900000.000001", Nothing)
            I.PostRoot -> fail "Expected a lifecycle delivery"
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
                          I.UpdateRoot _ -> do
                            opts ^. Wreq.param "oldest" `shouldBe` [I.slackTimestampText parent]
                            opts ^. Wreq.param "latest" `shouldBe` [I.slackTimestampText parent]
                            opts ^. Wreq.param "inclusive" `shouldBe` ["true"]
                          I.PostRoot -> fail "Unexpected root search"
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
          stored <- withResource tr.trPool $ \conn -> PGS.query conn [sql|SELECT state, message_ts FROM apis.slack_incident_deliveries WHERE id = ?|] (PGS.Only delivery.id)
          stored `shouldBe` [("delivered" :: Text, Just expectedTs)]
          readIORef pages >>= (`shouldBe` [])
        completedAt <- getTestTime tr.trTestClock
        run (I.claimSlackDeliveries completedAt) `shouldReturn` []


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
    created <- Issues.createQueryAlertIssue testPid (show mid) "Measured threshold exceeded" "" 60 84 Issues.Above
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
