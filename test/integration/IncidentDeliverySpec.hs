module IncidentDeliverySpec (spec) where

import BackgroundJobs (evaluateQueryMonitorValue, runSlackIncidentDeliveries)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Effectful.Notify qualified as Notify
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Time qualified as Time
import Models.Apis.Incidents qualified as I
import Models.Apis.Monitors qualified as Monitors
import Pages.Bots.BotTestHelpers (receiveSlackEvent, setupSlackData, slackRootEvent)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import System.Types (ATBackgroundCtx)
import Test.Hspec
import UnliftIO.Async (concurrently)


spec :: Spec
spec = around withTestResources do
  describe "Incident delivery" do
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
