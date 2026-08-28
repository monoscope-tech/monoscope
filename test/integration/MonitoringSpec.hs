module MonitoringSpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors, evaluateQueryMonitorValue)
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.BotTestHelpers (setupDiscordData, setupSlackData)
import Pages.Dashboards qualified as Dashboards
import Pages.Monitors (AlertUpsertForm (..), convertToQueryMonitor)
import Pages.Projects qualified as ProjectPages
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


spec :: Spec
-- Examples share state across the file (later tests reuse rows earlier ones create),
-- so this keeps aroundAll and runs sequentially — opting out of the suite's per-test
-- isolation + parallelism (same as GitSyncSpec).
spec = sequential $ aroundAll withTestResources do
  describe "Query Log Monitors" do
    it "should create monitor with no triggers" $ \tr -> do
      currentTime <- getCurrentTime
      let queryMonitor =
            convertToQueryMonitor (UUIDId UUID.nil) currentTime (Monitors.QueryMonitorId UUID.nil)
              $ AlertUpsertForm
                { alertId = Nothing
                , warningThreshold = Just "3"
                , alertThreshold = 4
                , recipientEmails = ["test@monoscope.tech"]
                , recipientSlacks = ["default"]
                , recipientEmailAll = Just True
                , direction = "above"
                , title = "Test Query Monitor"
                , severity = "Warning"
                , subject = "Test Query Subject"
                , message = "Test Query Message"
                , query = "status_code==200"
                , since = "7d"
                , from = ""
                , to = ""
                , frequency = Nothing
                , timeWindow = Nothing
                , conditionType = Nothing
                , source = Nothing
                , vizType = Nothing
                , teams = []
                , alertRecoveryThreshold = Nothing
                , warningRecoveryThreshold = Nothing
                , widgetId = Nothing
                , dashboardId = Nothing
                , notifyAfterCheck = Nothing
                , notifyAfter = Nothing
                , stopAfterCheck = Nothing
                , stopAfter = Nothing
                }
      respC <- runTestBg frozenTime tr $ Monitors.queryMonitorUpsert queryMonitor
      respC `shouldBe` 1
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", toStrict $ AE.encode reqMsg1)
            , ("m2", toStrict $ AE.encode reqMsg1)
            , ("m4", toStrict $ AE.encode reqMsg1)
            , ("m5", toStrict $ AE.encode reqMsg1)
            , ("m5", toStrict $ AE.encode reqMsg2)
            ]
      r <- runTestBg frozenTime tr $ processMessages msgs HashMap.empty
      case r of
        Right (ids, _poison) -> ids `shouldBe` ["m1", "m2", "m4", "m5", "m5"]
        Left _ -> expectationFailure "processMessages returned Left WriteFailure"

      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      void $ runAllBackgroundJobs frozenTime tr.trATCtx

  describe "Widget Monitor Lifecycle" do
    it "creates a monitor for the saved widget, alerts, recovers after an edit, and disappears with the widget" \tr -> do
      void
        $ testServant tr
        $ Dashboards.dashboardsPostH
          testPid
          Dashboards.DashboardForm{Dashboards.title = "Widget Monitor Lifecycle", Dashboards.file = "widget-monitor.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
      (_, dashboardsPage) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing (Dashboards.DashboardFilters [])
      dashboardId <- case dashboardsPage of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) ->
          maybe (fail "the dashboard was not saved") (pure . (.id)) $ V.find ((== "Widget Monitor Lifecycle") . (.title)) dashboards
        _ -> fail "the dashboard list did not load"
      let otherPid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
      isNothing <$> runTestBgNoReset tr (DashboardModel.getDashboardByProjectId otherPid dashboardId) `shouldReturn` True

      (_, savedWidget) <-
        testServant tr
          $ Dashboards.dashboardWidgetPutH
            testPid
            dashboardId
            Nothing
            Nothing
            def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Checkout errors", Widget.query = Just "name == \"checkout\""}
      widgetId <- maybe (fail "the saved widget has no id") pure savedWidget.id
      let alertForm =
            Dashboards.WidgetAlertForm
              { widgetId
              , query = "name == \"checkout\""
              , vizType = Just "timeseries"
              , alertEnabled = Just "on"
              , alertThreshold = 1
              , warningThreshold = Nothing
              , direction = "above"
              , showThresholdLines = Just "on_breach"
              , alertRecoveryThreshold = Nothing
              , warningRecoveryThreshold = Nothing
              , frequency = Just "1m"
              , title = "Checkout errors"
              , timeWindow = Just "1h"
              , conditionType = Just "threshold_exceeded"
              , severity = Just "Error"
              , subject = Just "Checkout errors"
              , message = Just "A checkout request failed"
              , recipientEmailAll = Nothing
              , teams = []
              }
      setupSlackData tr testPid "T_MONITOR_SIM"
      setupDiscordData tr testPid "G_MONITOR_SIM"
      void $ withResource tr.trPool \conn ->
        PGS.execute
          conn
          [sql|UPDATE projects.teams
                SET notify_emails = ARRAY['alerts@example.com'],
                    discord_channels = ARRAY['C_MONITOR'],
                    phone_numbers = ARRAY['+15550001111'],
                    pagerduty_services = ARRAY['widget-monitor-test'],
                    disabled_channels = '{}'
                WHERE project_id = ? AND handle = 'everyone'|]
          (PGS.Only testPid)
      void $ testServant tr $ Dashboards.widgetAlertUpsertH testPid widgetId (Just $ unUUIDId dashboardId) alertForm
      isNothing <$> runTestBgNoReset tr (Monitors.queryMonitorByWidgetId otherPid widgetId) `shouldReturn` True

      apiKey <- createTestAPIKey tr testPid "widget-monitor"
      ingestionTime <- getCurrentTime
      ingestTrace tr apiKey "checkout" ingestionTime
      advanceMinutes tr 1
      fired <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      firedMonitor <- runTestBgNoReset tr $ Monitors.queryMonitorByWidgetId testPid widgetId
      ((\m -> (m.currentStatus, m.currentValue, m.notificationCount)) <$> firedMonitor) `shouldBe` Just (Monitors.MSAlerting, 1, 1)
      deliverySummary fired
        `shouldBe` [ "discord:C_MONITOR"
                   , "email:alerts@example.com"
                   , "pagerduty:widget-monitor-test:PDTrigger"
                   , "slack:C_NOTIF_CHANNEL"
                   , "whatsapp:+15550001111"
                   ]

      advanceMinutes tr 1
      duplicate <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      duplicate `shouldBe` []

      let recoveryQuery = "name == \"search\""
          updatedWidget = savedWidget{Widget.query = Just recoveryQuery}
      void $ testServant tr $ Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing updatedWidget
      synced <- runTestBgNoReset tr $ Monitors.queryMonitorByWidgetId testPid widgetId
      (.logQuery) <$> synced `shouldBe` Just recoveryQuery

      advanceMinutes tr 1
      recovered <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      deliverySummary recovered
        `shouldBe` [ "discord:C_MONITOR"
                   , "email:alerts@example.com"
                   , "pagerduty:widget-monitor-test:PDResolve"
                   , "slack:C_NOTIF_CHANNEL"
                   , "whatsapp:+15550001111"
                   ]

      void $ testServant tr $ ProjectPages.updateNotificationsChannel testPid $ ProjectPages.NotifListForm ["email", "discord", "pagerduty"] [] ["alerts@example.com"] []
      void $ testServant tr $ Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing savedWidget
      advanceMinutes tr 1
      gated <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      deliverySummary gated
        `shouldBe` [ "discord:C_MONITOR"
                   , "email:alerts@example.com"
                   , "pagerduty:widget-monitor-test:PDTrigger"
                   ]
      void $ testServant tr $ ProjectPages.updateNotificationsChannel testPid $ ProjectPages.NotifListForm ["email", "slack", "discord", "phone", "pagerduty"] ["+15550001111"] ["alerts@example.com"] []

      currentMonitor <- maybe (fail "the widget monitor disappeared before deletion") pure firedMonitor
      let otherMonitorId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "a11e7ed0-0000-0000-0000-000000000001"
          otherMonitor =
            currentMonitor
              { Monitors.id = otherMonitorId
              , Monitors.projectId = otherPid
              , Monitors.currentStatus = Monitors.MSNormal
              }
      void $ runTestBgNoReset tr $ Monitors.queryMonitorUpsert otherMonitor

      void $ testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashboardId Nothing Map.empty
      isNothing <$> runTestBgNoReset tr (Monitors.queryMonitorByWidgetId testPid widgetId) `shouldReturn` True
      otherAfterDelete <- runTestBgNoReset tr $ Monitors.queryMonitorByWidgetId otherPid widgetId
      (.id) <$> otherAfterDelete `shouldBe` Just otherMonitorId
      void $ runTestBgNoReset tr $ Monitors.deleteMonitorsByWidgetIds otherPid [widgetId]

  describe "Monitor Hysteresis Integration" do
    it "should store and retrieve monitors with recovery thresholds" \tr -> do
      currentTime <- getCurrentTime
      -- Create monitor with recovery thresholds for hysteresis
      let queryMonitor =
            convertToQueryMonitor testPid currentTime (Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "11111111-1111-1111-1111-111111111111")
              $ AlertUpsertForm
                { alertId = Just "11111111-1111-1111-1111-111111111111"
                , warningThreshold = Just "80"
                , alertThreshold = 100
                , recipientEmails = []
                , recipientSlacks = []
                , recipientEmailAll = Nothing
                , direction = "above"
                , title = "Hysteresis Test Monitor"
                , severity = "Error"
                , subject = "Hysteresis Test"
                , message = "Testing hysteresis"
                , query = ""
                , since = "1h"
                , from = ""
                , to = ""
                , frequency = Just "1m"
                , timeWindow = Nothing
                , conditionType = Just "threshold_exceeded"
                , source = Nothing
                , vizType = Nothing
                , teams = []
                , alertRecoveryThreshold = Just "60"
                , warningRecoveryThreshold = Just "50"
                , widgetId = Nothing
                , dashboardId = Nothing
                , notifyAfterCheck = Nothing
                , notifyAfter = Nothing
                , stopAfterCheck = Nothing
                , stopAfter = Nothing
                }
      -- Insert the monitor
      _ <- runTestBg frozenTime tr $ Monitors.queryMonitorUpsert queryMonitor

      -- Verify monitor was created with correct thresholds
      monitorM <- runTestBg frozenTime tr $ Monitors.queryMonitorById (Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "11111111-1111-1111-1111-111111111111")
      case monitorM of
        Just m -> do
          m.alertThreshold `shouldBe` 100
          m.warningThreshold `shouldBe` Just 80
          m.alertRecoveryThreshold `shouldBe` Just 60
          m.warningRecoveryThreshold `shouldBe` Just 50
          m.currentStatus `shouldBe` Monitors.MSNormal
        Nothing -> error "Monitor not found"

      -- Update status to alerting via direct DB update (simulating alert trigger)
      _ <- withResource tr.trPool \conn ->
        PGS.execute conn [sql|UPDATE monitors.query_monitors SET current_status = 'alerting', current_value = 110 WHERE id = '11111111-1111-1111-1111-111111111111'|] ()

      -- Verify status was updated
      monitorM' <- runTestBg frozenTime tr $ Monitors.queryMonitorById (Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "11111111-1111-1111-1111-111111111111")
      case monitorM' of
        Just m -> do
          m.currentStatus `shouldBe` Monitors.MSAlerting
          m.currentValue `shouldBe` 110
        Nothing -> error "Monitor not found after status update"

  describe "Query Monitor Pipeline" do
    let pipelineMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "22222222-2222-2222-2222-222222222222"
        t0 = Unsafe.read "2025-06-01 12:00:00 UTC" :: UTCTime

    it "alerts once, suppresses duplicate delivery, and recovers" \tr -> do
      insertPipelineMonitor tr pipelineMonId "SELECT 150::float8" 100 Nothing "above" Nothing Nothing
      evalMonitorValueAt t0 tr pipelineMonId 150
      alerted <- fetchMonitor tr pipelineMonId
      alerted.currentStatus `shouldBe` Monitors.MSAlerting
      alerted.currentValue `shouldBe` 150
      isJust alerted.alertLastTriggered `shouldBe` True

      resetLastEvaluated tr pipelineMonId
      evalMonitorValueAt (addUTCTime 300 t0) tr pipelineMonId 150
      repeated <- fetchMonitor tr pipelineMonId
      repeated.currentStatus `shouldBe` Monitors.MSAlerting
      repeated.alertLastTriggered `shouldBe` alerted.alertLastTriggered

      resetLastEvaluated tr pipelineMonId
      evalMonitorValueAt (addUTCTime 3660 t0) tr pipelineMonId 150
      delayed <- fetchMonitor tr pipelineMonId
      delayed.alertLastTriggered `shouldBe` alerted.alertLastTriggered
      delayed.notificationCount `shouldBe` alerted.notificationCount

      evalMonitorValueAt (addUTCTime 3900 t0) tr pipelineMonId 50
      recovered <- fetchMonitor tr pipelineMonId
      recovered.currentStatus `shouldBe` Monitors.MSNormal
      recovered.alertLastTriggered `shouldBe` Nothing
      recovered.warningLastTriggered `shouldBe` Nothing

    it "Warning → Alerting transition with hysteresis" \tr -> do
      let hystMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "33333333-3333-3333-3333-333333333333"
      insertPipelineMonitor tr hystMonId "SELECT 90::float8" 100 (Just 80) "above" (Just 60) Nothing

      -- Value 90 → MSWarning (above warning=80, below alert=100)
      evalMonitorValueAt t0 tr hystMonId 90
      m1 <- fetchMonitor tr hystMonId
      m1.currentStatus `shouldBe` Monitors.MSWarning

      -- Value 110 → MSAlerting
      evalMonitorValueAt (addUTCTime 120 t0) tr hystMonId 110
      m2 <- fetchMonitor tr hystMonId
      m2.currentStatus `shouldBe` Monitors.MSAlerting

      -- Value 70 → still MSAlerting (above recovery threshold of 60)
      evalMonitorValueAt (addUTCTime 240 t0) tr hystMonId 70
      m3 <- fetchMonitor tr hystMonId
      m3.currentStatus `shouldBe` Monitors.MSAlerting

      -- Value 55 → MSNormal (below recovery threshold of 60)
      evalMonitorValueAt (addUTCTime 360 t0) tr hystMonId 55
      m4 <- fetchMonitor tr hystMonId
      m4.currentStatus `shouldBe` Monitors.MSNormal

    it "Monitor with interval not yet elapsed is skipped" \tr -> do
      let skipMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "44444444-4444-4444-4444-444444444444"
      insertPipelineMonitor tr skipMonId "SELECT 150::float8" 100 Nothing "above" Nothing Nothing
      void $ withResource tr.trPool \conn ->
        PGS.execute
          conn
          [sql|UPDATE monitors.query_monitors SET check_interval_mins = 60, last_evaluated = ? WHERE id = ?|]
          (addUTCTime (-1800) t0, skipMonId)
      evalMonitorsAt t0 tr
      m <- fetchMonitor tr skipMonId
      m.currentStatus `shouldBe` Monitors.MSNormal

    -- The guard against an infinite re-evaluation loop: a monitor whose query throws must
    -- still have its watermark advanced, or every tick re-picks it and hammers whatever
    -- backend is already failing. checkTriggeredQueryMonitors catches per monitor and
    -- updates lastEvaluated on that path specifically.
    it "a monitor whose evaluation throws still advances lastEvaluated" \tr -> do
      let brokenMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "66666666-6666-6666-6666-666666666666"
      insertPipelineMonitor tr brokenMonId "SELECT 1::float8" 100 Nothing "above" Nothing Nothing
      setMonitorQuery tr brokenMonId "SELECT * FROM a_table_that_does_not_exist"

      evalMonitorsAt t0 tr

      m <- fetchMonitor tr brokenMonId
      m.lastEvaluated `shouldSatisfy` maybe False (> addUTCTime (-86400) t0)
      -- Pins that this went down the failure branch rather than quietly succeeding: a query
      -- that cannot run must not also produce an alert.
      m.currentStatus `shouldBe` Monitors.MSNormal

    -- ...and one broken monitor must not stop the ones after it in the same tick.
    it "a broken monitor does not prevent the others from being evaluated" \tr -> do
      let brokenId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "77777777-7777-7777-7777-777777777777"
          healthyId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "88888888-8888-8888-8888-888888888888"
      insertPipelineMonitor tr brokenId "SELECT 1::float8" 100 Nothing "above" Nothing Nothing
      setMonitorQuery tr brokenId "SELECT * FROM another_missing_table"
      insertPipelineMonitor tr healthyId "" 0 Nothing "above" Nothing Nothing
      resetLastEvaluated tr healthyId

      evalMonitorsAt t0 tr

      healthy <- fetchMonitor tr healthyId
      healthy.currentStatus `shouldBe` Monitors.MSAlerting

    -- Regression: a monitor that has never been evaluated has last_evaluated NULL, which the
    -- decoder rejected as a non-nullable UTCTime. Hasql fails the whole row set, not one row,
    -- so a single never-evaluated monitor silently stopped every monitor in the deployment
    -- from being checked — 10,384 failed QueryMonitorsCheck jobs from 2026-08-07 onward.
    it "neverEvaluatedMonitor_isDueImmediatelyAndDoesNotBlindTheOthers" \tr -> do
      let neverId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "99999999-9999-9999-9999-999999999999"
          siblingId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "aaaaaaaa-9999-4999-8999-999999999999"
      insertPipelineMonitor tr neverId "" 0 Nothing "above" Nothing Nothing
      insertPipelineMonitor tr siblingId "" 0 Nothing "above" Nothing Nothing
      void $ withResource tr.trPool \conn ->
        PGS.execute conn [sql|UPDATE monitors.query_monitors SET last_evaluated = NULL WHERE id = ?|] (PGS.Only neverId)

      -- The read itself is what used to throw, taking every monitor down with it.
      actives <- runTestBg t0 tr Monitors.getActiveQueryMonitors
      let activeIds = map (.id) actives
      (neverId `elem` activeIds, siblingId `elem` activeIds) `shouldBe` (True, True)
      (find ((== neverId) . (.id)) actives >>= (.lastEvaluated)) `shouldBe` Nothing

      -- Never evaluated means due now, not "wait one interval".
      evalMonitorsAt t0 tr
      never <- fetchMonitor tr neverId
      never.currentStatus `shouldBe` Monitors.MSAlerting
      never.lastEvaluated `shouldSatisfy` isJust

    describe "Renotify and Stop-After" do
      let renotifyMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "55555555-5555-5555-5555-555555555555"

      it "delivers reminders only when due, stops at the limit, and always reports recovery" \tr -> do
        void $ withResource tr.trPool \conn ->
          PGS.execute conn [sql|UPDATE monitors.query_monitors SET check_interval_mins = 99999 WHERE id != ?|] (PGS.Only renotifyMonId)
        insertRenotifyMonitor tr renotifyMonId "SELECT 150::float8" 100
        notifs1 <- evalMonitorValueWithNotifs t0 tr renotifyMonId 150
        m1 <- fetchMonitor tr renotifyMonId
        m1.currentStatus `shouldBe` Monitors.MSAlerting
        m1.notificationCount `shouldBe` 1
        any (\case EmailNotification{} -> True; _ -> False) notifs1 `shouldBe` True
        any (\case PagerdutyNotification{} -> True; _ -> False) notifs1 `shouldBe` True

        resetLastEvaluated tr renotifyMonId
        notifs2 <- evalMonitorValueWithNotifs (addUTCTime 3660 t0) tr renotifyMonId 150
        m2 <- fetchMonitor tr renotifyMonId
        m2.notificationCount `shouldBe` 1
        m2.alertLastTriggered `shouldBe` m1.alertLastTriggered
        length notifs2 `shouldBe` 0

        setMonitorRenotifyConfig tr renotifyMonId (Just 30) Nothing 1
        resetLastEvaluated tr renotifyMonId
        setAlertLastTriggered tr renotifyMonId (Just $ addUTCTime (6 * 60) t0)
        early <- evalMonitorValueWithNotifs (addUTCTime (35 * 60) t0) tr renotifyMonId 150
        length early `shouldBe` 0

        resetLastEvaluated tr renotifyMonId
        setAlertLastTriggered tr renotifyMonId (Just t0)
        due <- evalMonitorValueWithNotifs (addUTCTime (35 * 60) t0) tr renotifyMonId 150
        dueMonitor <- fetchMonitor tr renotifyMonId
        dueMonitor.notificationCount `shouldBe` 2
        length due `shouldSatisfy` (> 0)

        setMonitorRenotifyConfig tr renotifyMonId (Just 30) (Just 3) 2
        resetLastEvaluated tr renotifyMonId
        setAlertLastTriggered tr renotifyMonId (Just t0)
        finalReminder <- evalMonitorValueWithNotifs (addUTCTime (35 * 60) t0) tr renotifyMonId 150
        atLimit <- fetchMonitor tr renotifyMonId
        atLimit.notificationCount `shouldBe` 3
        length finalReminder `shouldSatisfy` (> 0)

        resetLastEvaluated tr renotifyMonId
        setAlertLastTriggered tr renotifyMonId (Just t0)
        stopped <- evalMonitorValueWithNotifs (addUTCTime (70 * 60) t0) tr renotifyMonId 150
        stoppedMonitor <- fetchMonitor tr renotifyMonId
        stoppedMonitor.notificationCount `shouldBe` 3
        length stopped `shouldBe` 0

        recoveryNotifs <- evalMonitorValueWithNotifs (addUTCTime (75 * 60) t0) tr renotifyMonId 50
        recovered <- fetchMonitor tr renotifyMonId
        recovered.currentStatus `shouldBe` Monitors.MSNormal
        recovered.notificationCount `shouldBe` 0
        recovered.alertLastTriggered `shouldBe` Nothing
        length recoveryNotifs `shouldSatisfy` (> 0)

      it "suppresses delivery while muted and resumes after the window" \tr -> do
        let muteMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "66666666-6666-6666-6666-666666666666"
            evalT = addUTCTime (200 * 60) t0
        void $ withResource tr.trPool \conn ->
          PGS.execute conn [sql|UPDATE monitors.query_monitors SET check_interval_mins = 99999 WHERE id != ?|] (PGS.Only muteMonId)
        insertRenotifyMonitor tr muteMonId "SELECT 150::float8" 100
        setMonitorRenotifyConfig tr muteMonId Nothing Nothing 0

        notifs0 <- evalMonitorValueWithNotifs evalT tr muteMonId 150
        length notifs0 `shouldSatisfy` (> 0)

        void $ withResource tr.trPool \conn ->
          PGS.execute
            conn
            [sql|UPDATE monitors.query_monitors SET muted_until = ?, notification_count = 0, alert_last_triggered = NULL WHERE id = ?|]
            (addUTCTime 3600 evalT, muteMonId)

        resetLastEvaluated tr muteMonId
        notifs30 <- evalMonitorValueWithNotifs (addUTCTime 1800 evalT) tr muteMonId 150
        length notifs30 `shouldBe` 0
        (.notificationCount) <$> fetchMonitor tr muteMonId `shouldReturn` 0

        resetLastEvaluated tr muteMonId
        notifs61 <- evalMonitorValueWithNotifs (addUTCTime 3660 evalT) tr muteMonId 150
        length notifs61 `shouldSatisfy` (> 0)
        resumed <- fetchMonitor tr muteMonId
        resumed.currentStatus `shouldBe` Monitors.MSAlerting
        resumed.notificationCount `shouldBe` 1


deliverySummary :: [Notification] -> [Text]
deliverySummary =
  sort . map \case
    EmailNotification d -> "email:" <> d.receiver
    SlackNotification d -> "slack:" <> d.channelId
    DiscordNotification d -> "discord:" <> d.channelId
    WhatsAppNotification d -> "whatsapp:" <> Notify.to d
    PagerdutyNotification d -> "pagerduty:" <> Notify.integrationKey d <> ":" <> show (Notify.eventAction d)


-- | Insert a monitor for deterministic state-machine simulation.
insertPipelineMonitor :: TestResources -> Monitors.QueryMonitorId -> Text -> Double -> Maybe Double -> Text -> Maybe Double -> Maybe Double -> IO ()
insertPipelineMonitor tr monId _ threshold warnThreshold direction alertRecovery warnRecovery = do
  let monitor :: Monitors.QueryMonitor
      monitor =
        def
          { Monitors.id = monId
          , Monitors.projectId = testPid
          , Monitors.logQuery = "summarize count()"
          , Monitors.logQueryAsSql = ""
          , Monitors.alertThreshold = threshold
          , Monitors.warningThreshold = warnThreshold
          , Monitors.triggerLessThan = direction == "below"
          , Monitors.checkIntervalMins = 1
          , Monitors.timeWindowMins = 60
          , Monitors.alertConfig = def{Monitors.title = "Pipeline Test Monitor"}
          , Monitors.alertRecoveryThreshold = alertRecovery
          , Monitors.warningRecoveryThreshold = warnRecovery
          }
  void $ runTestBg frozenTime tr $ Monitors.queryMonitorUpsert monitor
  -- Ensure it's evaluatable: set lastEvaluated far in the past
  void $ withResource tr.trPool \conn ->
    PGS.execute conn [sql|UPDATE monitors.query_monitors SET last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (PGS.Only monId)


-- | Run checkTriggeredQueryMonitors at a specific frozen time
evalMonitorsAt :: UTCTime -> TestResources -> IO ()
evalMonitorsAt t tr = runTestBackground t tr.trATCtx checkTriggeredQueryMonitors


evalMonitorValueAt :: UTCTime -> TestResources -> Monitors.QueryMonitorId -> Double -> IO ()
evalMonitorValueAt t tr monId value = do
  monitor <- fetchMonitor tr monId
  runTestBackground t tr.trATCtx $ evaluateQueryMonitorValue monitor t value


-- | Reset lastEvaluated to far past so the monitor gets re-evaluated
resetLastEvaluated :: TestResources -> Monitors.QueryMonitorId -> IO ()
resetLastEvaluated tr monId = void $ withResource tr.trPool \conn ->
  PGS.execute conn [sql|UPDATE monitors.query_monitors SET last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (PGS.Only monId)


-- | Replace source KQL and reset lastEvaluated (used by query-failure tests).
setMonitorQuery :: TestResources -> Monitors.QueryMonitorId -> Text -> IO ()
setMonitorQuery tr monId q = void $ withResource tr.trPool \conn ->
  PGS.execute conn [sql|UPDATE monitors.query_monitors SET log_query = ?, last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (q, monId)


-- | Fetch a monitor by ID (asserts it exists)
fetchMonitor :: TestResources -> Monitors.QueryMonitorId -> IO Monitors.QueryMonitor
fetchMonitor tr monId = do
  m <- runTestBg frozenTime tr $ Monitors.queryMonitorById monId
  maybe (error "Monitor not found") pure m


-- | Insert a pipeline monitor with a pagerduty service on the everyone team so notifications are captured via Notify effect
insertRenotifyMonitor :: TestResources -> Monitors.QueryMonitorId -> Text -> Double -> IO ()
insertRenotifyMonitor tr monId sqlQuery threshold = do
  insertPipelineMonitor tr monId sqlQuery threshold Nothing "above" Nothing Nothing
  void $ withResource tr.trPool \conn ->
    PGS.execute conn [sql|UPDATE projects.teams SET pagerduty_services = ARRAY['test-integration-key'] WHERE project_id = ? AND handle = 'everyone'|] (PGS.Only testPid)


evalMonitorValueWithNotifs :: UTCTime -> TestResources -> Monitors.QueryMonitorId -> Double -> IO [Notification]
evalMonitorValueWithNotifs t tr monId value = do
  monitor <- fetchMonitor tr monId
  fst <$> runTestBackgroundWithNotifications t tr.trLogger tr.trATCtx (evaluateQueryMonitorValue monitor t value)


-- | Set renotify config columns via direct SQL
setMonitorRenotifyConfig :: TestResources -> Monitors.QueryMonitorId -> Maybe Int -> Maybe Int -> Int -> IO ()
setMonitorRenotifyConfig tr monId renotifyMins stopAfter notifCount = void $ withResource tr.trPool \conn ->
  PGS.execute
    conn
    [sql|UPDATE monitors.query_monitors SET renotify_interval_mins = ?, stop_after_count = ?, notification_count = ? WHERE id = ?|]
    (renotifyMins, stopAfter, notifCount, monId)


-- | Set alertLastTriggered timestamp
setAlertLastTriggered :: TestResources -> Monitors.QueryMonitorId -> Maybe UTCTime -> IO ()
setAlertLastTriggered tr monId t = void $ withResource tr.trPool \conn ->
  PGS.execute conn [sql|UPDATE monitors.query_monitors SET alert_last_triggered = ? WHERE id = ?|] (t, monId)
