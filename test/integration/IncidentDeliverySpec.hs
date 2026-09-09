module IncidentDeliverySpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors, evaluateQueryMonitorValue, notifyErrorSubscriptions, processProjectErrors, runSlackIncidentDeliveries)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.Notify qualified as Notify
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns qualified as Errors
import Models.Apis.Incidents qualified as I
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pages.Anomalies qualified as Anomalies
import Pages.Bots.BotTestHelpers (receiveSlackEvent, setupSlackData, slackRootEvent)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant qualified
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
      void $ testServant tr $ Anomalies.resolveErrorPostH testPid notified.id.unErrorPatternId
      (closed, _) <- captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      length (messages closed) `shouldBe` 4
      for_ (messages closed) \sd -> AE.encode sd.payload `shouldSatisfy` (T.isInfixOf "RESOLVED" . decodeUtf8 . toStrict)

    it "keeps one error episode across different issue records and resolves its original incident link" \tr -> do
      update <- setup tr
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
      void $ testServant tr $ Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId
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
      void $ testServant tr $ Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid recurring.id) `shouldReturn` Just I.EpisodeResolved
      (orphaned, _) <- captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
      let orphanedMessages = [sd | Notify.SlackNotification sd <- orphaned]
      length orphanedMessages `shouldBe` 4
      for_ orphanedMessages \sd -> do
        let body = decodeUtf8 @Text $ toStrict $ AE.encode sd.payload
        body `shouldSatisfy` T.isInfixOf "Open project issues"
        body `shouldSatisfy` not . T.isInfixOf nextIssue.id.toText

    it "records manual error resolution once and updates both incident threads without claiming recovery" \tr -> do
      update <- setup tr
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
      void $ testServant tr $ Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId
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
      testServant tr (Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId) `shouldThrow` anyException
      Just rolledBack <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
      (rolledBack.state, rolledBack.resolvedAt, rolledBack.resolvedBy) `shouldBe` (Errors.ESNew, Nothing, Nothing)
      activityBefore <- runTestBgNoReset tr $ Issues.selectIssueActivity testPid issue.id
      activityBefore `shouldSatisfy` all (\a -> a.event /= Issues.IEResolved && a.event /= Issues.IEAutoResolved)
      void $ withResource tr.trPool \conn -> PGS.execute_ conn [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_resolution|]
      void
        $ concurrently
          (testServant tr $ Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId)
          (testServant tr $ Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId)
      fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
      void $ testServant tr $ Anomalies.resolveErrorPostH testPid err.id.unErrorPatternId
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
      void $ testServant tr $ Anomalies.resolveErrorPostH testPid legacyErr.id.unErrorPatternId
      deliveryCount `shouldReturn` beforeLegacy
      Just legacyResolved <- runTestBgNoReset tr $ Errors.getErrorPatternById legacyErr.id
      (legacyResolved.state, legacyResolved.resolvedBy) `shouldBe` (Errors.ESResolved, Just actor.id)

    it "marks missing monitor data in existing threads and waits for a measured recovery" \tr -> do
      update <- setup tr
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
      update <- setup tr
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
      update <- setup tr
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
      update <- setup tr
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
      (again, _) <- captureNotifs tr runSlackIncidentDeliveries
      again `shouldBe` []

    it "deduplicates destinations and events, orders recovery behind roots, and starts a new episode on recurrence" \tr -> do
      update <- setup tr
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
      update <- setup tr
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
      update <- setup tr
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

    it "captures only this app's signed root in the original workspace and channel" \tr -> do
      update <- setup tr
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


timestamp :: Text -> IO I.SlackTimestamp
timestamp = maybe (fail "invalid fixture timestamp") pure . I.slackTimestamp


evaluateMonitor :: TestResources -> Monitors.QueryMonitorId -> Double -> IO [Notify.Notification]
evaluateMonitor tr mid value =
  fst <$> captureNotifs tr do
    now <- Time.currentTime
    current <- Monitors.queryMonitorById mid
    for_ current \monitor -> evaluateQueryMonitorValue monitor now value
    replicateM_ 3 runSlackIncidentDeliveries
