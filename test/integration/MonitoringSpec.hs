module MonitoringSpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors)
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
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
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Dashboards qualified as Dashboards
import Pages.Monitors (AlertUpsertForm (..), convertToQueryMonitor)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


spec :: Spec
spec = aroundAll withTestResources do
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
      r `shouldBe` ["m1", "m2", "m4", "m5", "m5"]

      _ <- withResource tr.trPool \conn -> PGS.execute conn [sql|CALL monitors.check_triggered_query_monitors(0, '{}'::jsonb)|] ()

      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      void $ runAllBackgroundJobs frozenTime tr.trATCtx

  describe "Widget Alert Cascade Deletion" do
    it "should delete alert when widget is removed from dashboard" \tr -> do
      -- Create dashboard
      let dashboardForm = Dashboards.DashboardForm{Dashboards.title = "Alert Cascade Test Dashboard", Dashboards.file = "test.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
      _ <- testServant tr $ Dashboards.dashboardsPostH testPid dashboardForm

      -- Get dashboard to retrieve its ID
      (_, dashboardsResp) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing (Dashboards.DashboardFilters [])
      dashId <- case dashboardsResp of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> do
          let dashM = V.find (\x -> x.title == ("Alert Cascade Test Dashboard" :: Text)) dashboards
          pure $ maybe (error "Dashboard not found") (\x -> x.id) dashM
        _ -> error "Unexpected response"

      -- Add widget to dashboard
      let widget = def{Widget.wType = Widget.WTTimeseries, Widget.id = Just "cascade-test-widget", Widget.title = Just "Test Widget", Widget.query = Just "status_code==500"}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing widget

      -- Create alert for widget
      let alertForm =
            Dashboards.WidgetAlertForm
              { widgetId = "cascade-test-widget"
              , query = "status_code==500"
              , vizType = Just "timeseries"
              , alertEnabled = Just "on"
              , alertThreshold = 100
              , warningThreshold = Nothing
              , direction = "above"
              , showThresholdLines = Nothing
              , alertRecoveryThreshold = Nothing
              , warningRecoveryThreshold = Nothing
              , frequency = Nothing
              , title = "Cascade Test Alert"
              , timeWindow = Nothing
              , conditionType = Nothing
              , severity = Nothing
              , subject = Nothing
              , message = Nothing
              , recipientEmailAll = Nothing
              , teams = []
              }
      _ <- testServant tr $ Dashboards.widgetAlertUpsertH testPid "cascade-test-widget" (Just $ unUUIDId dashId) alertForm

      -- Verify alert exists
      alertM <- runTestBg frozenTime tr $ Monitors.queryMonitorByWidgetId "cascade-test-widget"
      isJust alertM `shouldBe` True

      -- Delete alert via widgetAlertDeleteH handler
      _ <- testServant tr $ Dashboards.widgetAlertDeleteH testPid "cascade-test-widget"

      -- Verify alert was deleted
      alertM' <- runTestBg frozenTime tr $ Monitors.queryMonitorByWidgetId "cascade-test-widget"
      isNothing alertM' `shouldBe` True

  describe "Widget Alert Query Sync" do
    it "should sync alert query when widget query changes" \tr -> do
      -- Reuse dashboard from previous test (tests share state via aroundAll)
      (_, dashboardsResp) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing (Dashboards.DashboardFilters [])
      dashId <- case dashboardsResp of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> do
          let dashM = V.find (\x -> x.title == ("Alert Cascade Test Dashboard" :: Text)) dashboards
          pure $ maybe (error "Dashboard not found - previous test may have failed") (\x -> x.id) dashM
        _ -> error "Unexpected response"

      -- Add widget with initial query
      let initialQuery = "status_code==200"
      let widget = def{Widget.wType = Widget.WTTimeseries, Widget.id = Just "sync-test-widget", Widget.title = Just "Sync Test Widget", Widget.query = Just initialQuery}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing widget

      -- Create alert for widget
      let alertForm =
            Dashboards.WidgetAlertForm
              { widgetId = "sync-test-widget"
              , query = initialQuery
              , vizType = Just "timeseries"
              , alertEnabled = Just "on"
              , alertThreshold = 100
              , warningThreshold = Nothing
              , direction = "above"
              , showThresholdLines = Nothing
              , alertRecoveryThreshold = Nothing
              , warningRecoveryThreshold = Nothing
              , frequency = Nothing
              , title = "Sync Test Alert"
              , timeWindow = Nothing
              , conditionType = Nothing
              , severity = Nothing
              , subject = Nothing
              , message = Nothing
              , recipientEmailAll = Nothing
              , teams = []
              }
      _ <- testServant tr $ Dashboards.widgetAlertUpsertH testPid "sync-test-widget" (Just $ unUUIDId dashId) alertForm

      -- Verify initial query
      alertM <- runTestBg frozenTime tr $ Monitors.queryMonitorByWidgetId "sync-test-widget"
      (Unsafe.fromJust alertM).logQuery `shouldBe` initialQuery

      -- Update widget with new query
      let newQuery = "status_code==500"
      let updatedWidget = def{Widget.wType = Widget.WTTimeseries, Widget.id = Just "sync-test-widget", Widget.title = Just "Sync Test Widget", Widget.query = Just newQuery}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just "sync-test-widget") Nothing updatedWidget

      -- Verify query was synced
      alertM' <- runTestBg frozenTime tr $ Monitors.queryMonitorByWidgetId "sync-test-widget"
      (Unsafe.fromJust alertM').logQuery `shouldBe` newQuery

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

    it "Normal → Alerting transition updates status and timestamps" \tr -> do
      insertPipelineMonitor tr pipelineMonId "SELECT 150::float8" 100 Nothing "above" Nothing Nothing
      evalMonitorsAt t0 tr
      m <- fetchMonitor tr pipelineMonId
      m.currentStatus `shouldBe` Monitors.MSAlerting
      m.currentValue `shouldBe` 150
      isJust m.alertLastTriggered `shouldBe` True

    it "Repeated evaluation preserves status without updating timestamps (spam prevention)" \tr -> do
      prev <- fetchMonitor tr pipelineMonId
      resetLastEvaluated tr pipelineMonId
      evalMonitorsAt (addUTCTime 300 t0) tr -- +5 min
      m <- fetchMonitor tr pipelineMonId
      m.currentStatus `shouldBe` Monitors.MSAlerting
      -- alertLastTriggered should NOT be updated (no re-notification)
      m.alertLastTriggered `shouldBe` prev.alertLastTriggered

    it "Periodic reminder after 1 hour updates timestamp" \tr -> do
      prev <- fetchMonitor tr pipelineMonId
      resetLastEvaluated tr pipelineMonId
      evalMonitorsAt (addUTCTime 3660 t0) tr -- +61 min
      m <- fetchMonitor tr pipelineMonId
      m.currentStatus `shouldBe` Monitors.MSAlerting
      -- alertLastTriggered should be updated (reminder sent)
      m.alertLastTriggered `shouldNotBe` prev.alertLastTriggered

    it "Alerting → Normal recovery clears timestamps" \tr -> do
      setMonitorQuery tr pipelineMonId "SELECT 50::float8"
      evalMonitorsAt (addUTCTime 3900 t0) tr -- +65 min
      m <- fetchMonitor tr pipelineMonId
      m.currentStatus `shouldBe` Monitors.MSNormal
      m.alertLastTriggered `shouldBe` Nothing
      m.warningLastTriggered `shouldBe` Nothing

    it "Warning → Alerting transition with hysteresis" \tr -> do
      let hystMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "33333333-3333-3333-3333-333333333333"
      insertPipelineMonitor tr hystMonId "SELECT 90::float8" 100 (Just 80) "above" (Just 60) Nothing

      -- Value 90 → MSWarning (above warning=80, below alert=100)
      evalMonitorsAt t0 tr
      m1 <- fetchMonitor tr hystMonId
      m1.currentStatus `shouldBe` Monitors.MSWarning

      -- Value 110 → MSAlerting
      setMonitorQuery tr hystMonId "SELECT 110::float8"
      evalMonitorsAt (addUTCTime 120 t0) tr
      m2 <- fetchMonitor tr hystMonId
      m2.currentStatus `shouldBe` Monitors.MSAlerting

      -- Value 70 → still MSAlerting (above recovery threshold of 60)
      setMonitorQuery tr hystMonId "SELECT 70::float8"
      evalMonitorsAt (addUTCTime 240 t0) tr
      m3 <- fetchMonitor tr hystMonId
      m3.currentStatus `shouldBe` Monitors.MSAlerting

      -- Value 55 → MSNormal (below recovery threshold of 60)
      setMonitorQuery tr hystMonId "SELECT 55::float8"
      evalMonitorsAt (addUTCTime 360 t0) tr
      m4 <- fetchMonitor tr hystMonId
      m4.currentStatus `shouldBe` Monitors.MSNormal

    it "Monitor with interval not yet elapsed is skipped" \tr -> do
      let skipMonId = Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "44444444-4444-4444-4444-444444444444"
      insertPipelineMonitor tr skipMonId "SELECT 150::float8" 100 Nothing "above" Nothing Nothing
      void $ withResource tr.trPool \conn ->
        PGS.execute conn [sql|UPDATE monitors.query_monitors SET check_interval_mins = 60, last_evaluated = ? WHERE id = ?|]
          (addUTCTime (-1800) t0, skipMonId)
      evalMonitorsAt t0 tr
      m <- fetchMonitor tr skipMonId
      m.currentStatus `shouldBe` Monitors.MSNormal


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


-- | Insert a pipeline test monitor with deterministic SQL query
insertPipelineMonitor :: TestResources -> Monitors.QueryMonitorId -> Text -> Double -> Maybe Double -> Text -> Maybe Double -> Maybe Double -> IO ()
insertPipelineMonitor tr monId sqlQuery threshold warnThreshold direction alertRecovery warnRecovery = do
  let monitor :: Monitors.QueryMonitor
      monitor =
        def
          { Monitors.id = monId
          , Monitors.projectId = testPid
          , Monitors.logQuery = "test"
          , Monitors.logQueryAsSql = sqlQuery
          , Monitors.alertThreshold = threshold
          , Monitors.warningThreshold = warnThreshold
          , Monitors.triggerLessThan = direction == "below"
          , Monitors.checkIntervalMins = 1
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


-- | Reset lastEvaluated to far past so the monitor gets re-evaluated
resetLastEvaluated :: TestResources -> Monitors.QueryMonitorId -> IO ()
resetLastEvaluated tr monId = void $ withResource tr.trPool \conn ->
  PGS.execute conn [sql|UPDATE monitors.query_monitors SET last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (PGS.Only monId)


-- | Update the deterministic SQL query for a monitor and reset lastEvaluated
setMonitorQuery :: TestResources -> Monitors.QueryMonitorId -> Text -> IO ()
setMonitorQuery tr monId q = void $ withResource tr.trPool \conn ->
  PGS.execute conn [sql|UPDATE monitors.query_monitors SET log_query_as_sql = ?, last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (q, monId)


-- | Fetch a monitor by ID (asserts it exists)
fetchMonitor :: TestResources -> Monitors.QueryMonitorId -> IO Monitors.QueryMonitor
fetchMonitor tr monId = do
  m <- runTestBg frozenTime tr $ Monitors.queryMonitorById monId
  maybe (error "Monitor not found") pure m
