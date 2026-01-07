module MonitoringSpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors)
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
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
      respC <- runTestBg tr $ Monitors.queryMonitorUpsert queryMonitor
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
      r <- runTestBg tr $ processMessages msgs HashMap.empty
      r `shouldBe` ["m1", "m2", "m4", "m5", "m5"]

      _ <- withResource tr.trPool \conn -> PGS.execute conn [sql|CALL monitors.check_triggered_query_monitors(0, '{}'::jsonb)|] ()

      pendingJobs <- getPendingBackgroundJobs tr.trATCtx
      logBackgroundJobsInfo tr.trLogger pendingJobs

      void $ runAllBackgroundJobs tr.trATCtx

  describe "Widget Alert Cascade Deletion" do
    it "should delete alert when widget is removed from dashboard" \tr -> do
      -- Create dashboard
      let dashboardForm = Dashboards.DashboardForm{Dashboards.title = "Alert Cascade Test Dashboard", Dashboards.file = "test.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
      _ <- testServant tr $ Dashboards.dashboardsPostH testPid dashboardForm

      -- Get dashboard to retrieve its ID
      (_, dashboardsResp) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing (Dashboards.DashboardFilters [])
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
              }
      _ <- testServant tr $ Dashboards.widgetAlertUpsertH testPid "cascade-test-widget" (Just $ unUUIDId dashId) alertForm

      -- Verify alert exists
      alertM <- runTestBg tr $ Monitors.queryMonitorByWidgetId "cascade-test-widget"
      isJust alertM `shouldBe` True

      -- Delete alert via widgetAlertDeleteH handler
      _ <- testServant tr $ Dashboards.widgetAlertDeleteH testPid "cascade-test-widget"

      -- Verify alert was deleted
      alertM' <- runTestBg tr $ Monitors.queryMonitorByWidgetId "cascade-test-widget"
      isNothing alertM' `shouldBe` True

  describe "Widget Alert Query Sync" do
    it "should sync alert query when widget query changes" \tr -> do
      -- Reuse dashboard from previous test (tests share state via aroundAll)
      (_, dashboardsResp) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing (Dashboards.DashboardFilters [])
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
              }
      _ <- testServant tr $ Dashboards.widgetAlertUpsertH testPid "sync-test-widget" (Just $ unUUIDId dashId) alertForm

      -- Verify initial query
      alertM <- runTestBg tr $ Monitors.queryMonitorByWidgetId "sync-test-widget"
      (Unsafe.fromJust alertM).logQuery `shouldBe` initialQuery

      -- Update widget with new query
      let newQuery = "status_code==500"
      let updatedWidget = def{Widget.wType = Widget.WTTimeseries, Widget.id = Just "sync-test-widget", Widget.title = Just "Sync Test Widget", Widget.query = Just newQuery}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just "sync-test-widget") Nothing updatedWidget

      -- Verify query was synced
      alertM' <- runTestBg tr $ Monitors.queryMonitorByWidgetId "sync-test-widget"
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
      _ <- runTestBg tr $ Monitors.queryMonitorUpsert queryMonitor

      -- Verify monitor was created with correct thresholds
      monitorM <- runTestBg tr $ Monitors.queryMonitorById (Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "11111111-1111-1111-1111-111111111111")
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
      monitorM' <- runTestBg tr $ Monitors.queryMonitorById (Monitors.QueryMonitorId $ Unsafe.fromJust $ UUID.fromText "11111111-1111-1111-1111-111111111111")
      case monitorM' of
        Just m -> do
          m.currentStatus `shouldBe` Monitors.MSAlerting
          m.currentValue `shouldBe` 110
        Nothing -> error "Monitor not found after status update"


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil
