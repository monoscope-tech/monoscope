module MonitoringSpec (spec) where

import BackgroundJobs (checkTriggeredQueryMonitors, evaluateQueryMonitorValue, notifyErrorSubscriptions, processProjectErrors, runSlackIncidentDeliveries)
import Codec.Compression.GZip qualified as GZip
import Control.Lens ((.~), (^.), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.CaseInsensitive qualified as CI
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.UUID qualified as UUID
import Data.UUID.Quasi (uuid)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns qualified as Errors
import Models.Apis.Incidents qualified as I
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.Wreq qualified as Wreq
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.BotTestHelpers (receiveSlackEvent, setupDiscordData, setupSlackData, slackRootEvent, withHTTPResponses)
import Pages.Bots.Slack qualified as SlackPage
import Pages.Dashboards qualified as Dashboards
import Pages.Issues qualified as IssuesPage
import Pages.Monitors qualified as Alerts
import Pages.Projects qualified as ProjectPages
import Pages.Settings qualified as Settings
import Pkg.Components.Widget qualified as Widget
import Pkg.Mail qualified as Mail
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import System.Config qualified as Config
import System.Types (ATAuthCtx, ATBackgroundCtx)
import Test.Hspec (anyIOException)
import UnliftIO.Async (concurrently)


spec :: Spec
spec = describe "Monitoring" do
  it "Slack error fallbacks stay concise and keep metadata subordinate to the exception" do
    let err = def{Errors.errorType = "HttpException", Errors.message = T.replicate 50 "HttpExceptionRequest Request { host = \"api.example.com\" path = \"/oauth/token\" } ", Errors.serviceName = Just "monoscope-dev"}
        impact = Mail.AlertImpact "Monoscope" (Just "Acme Corp") (Just "Ada Lovelace") (Just "ada@example.com")
        messages trend = Mail.errorIncidentMessages Mail.NewRuntimeError err frozenTime Nothing "https://example.com" "https://example.com/issues/1" trend (Just "1/hr") impact
        (root, reply) = messages (Just "https://example.com/error-trend.png")
        withoutTrend = jsonText $ fst $ messages Nothing
    jsonText root `shouldContainAll` ["```HttpExceptionRequest", "HTTP request failed · api.example.com/oauth/token", "Project Monoscope · Tenant Acme Corp · monoscope-dev · 1/hr · Observed", "Ada Lovelace &lt;ada@example.com&gt;", "https://example.com/error-trend.png"]
    jsonText root `shouldNotSatisfy` T.isInfixOf err.message
    (withoutTrend, jsonText reply) `shouldSatisfy` \(w, r) -> "Trend unavailable yet" `T.isInfixOf` w && not ("image_url" `T.isInfixOf` w) && "Open issue" `T.isInfixOf` r
    slackPayloadViolations (AE.toJSON root) <> slackPayloadViolations (AE.toJSON reply) `shouldBe` []

  around withTestResources do
    it "web monitor lifecycle: form save, scoped compiled SQL, list/single/overview, invalid KQL refused, edits keep deactivation" \tr -> do
      let mid = Monitors.QueryMonitorId UUID.nil
      length <$> runQueryEffect tr (Monitors.queryMonitorsAll testPid) `shouldReturn` 0
      call tr (Alerts.alertUpsertPostH testPid alertForm) >>= \case
        Alerts.AlertNoContent d -> d `shouldBe` ""
        _ -> fail "unexpected response"
      monitors <- runQueryEffect tr $ Monitors.queryMonitorsAll testPid
      [(m.id, m.alertThreshold, m.warningThreshold, m.environment, m.service) | m <- monitors] `shouldBe` [(mid, 1, Nothing, Just "production", Just "checkout")]
      for_ monitors \m -> m.logQueryAsSql `shouldContainAll` ["resource___deployment___environment___name = 'production'", "resource___service___name = 'checkout'"]
      call tr (Alerts.alertSingleGetH testPid mid) >>= \case
        Alerts.AlertSingle _ (Just m) -> (m.id, m.alertThreshold, m.warningThreshold) `shouldBe` (mid, 1, Nothing)
        _ -> fail "unexpected response"
      -- A healthy monitor once rendered a grey "Normal" badge beside its green dot.
      overview <- renderT <$> call tr (Alerts.unifiedMonitorOverviewH testPid (UUID.toText UUID.nil))
      let unSvg t = case T.breakOn "<svg" t of
            (a, b) | T.null b -> a
            (a, b) -> a <> unSvg (T.drop 6 $ snd $ T.breakOn "</svg>" b)
      goldenParts "monitor-overview-status" $ ordNub $ map unSvg $ elementsWith ["badge-success", "Environment: production", "Service: checkout"] overview
      -- Row actions carry accessible names; each mute popover has a per-layout id.
      list <- renderT <$> call tr (Alerts.unifiedMonitorsGetH testPid Nothing Nothing)
      goldenParts "monitor-row-actions" $ tagsWith ["aria-label=\"Deactivate\"", "aria-label=\"Mute\"", "aria-label=\"Delete\"", "id=\"mute-pop-"] list
      -- Unparseable KQL crashed the handler when the upsert forced the compiled SQL.
      let badId = UUID.fromWords 1 2 3 4
      void $ testServant tr $ Alerts.alertUpsertPostH testPid alertForm{Alerts.alertId = Just (UUID.toText badId), Alerts.query = "status_code == (("}
      runQueryEffect tr (Monitors.queryMonitorById (Monitors.QueryMonitorId badId)) >>= (`shouldSatisfy` isNothing)
      -- The form has no active control, so saving an edit must not re-activate a deactivated monitor.
      runQueryEffect tr (Monitors.monitorsBulkUpdate testPid Monitors.BADeactivate Nothing [mid]) `shouldReturn` 1
      void $ testServant tr $ Alerts.alertUpsertPostH testPid alertForm{Alerts.title = "Renamed while deactivated"}
      saved <- runQueryEffect tr $ Monitors.queryMonitorById mid
      (isJust (saved >>= (.deactivatedAt)), (.alertConfig.title) <$> saved) `shouldBe` (True, Just "Renamed while deactivated")
      -- Ingestion and the job runner tolerate a stored monitor.
      nowTxt <- toText . formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime
      let msg f = toStrict $ AE.encode $ Unsafe.fromJust $ convert $ f nowTxt
          (m1, m2) = (msg testRequestMsgs.reqMsg1, msg testRequestMsgs.reqMsg2)
      runTestBg frozenTime tr (processMessages [("m1", m1), ("m2", m1), ("m4", m1), ("m5", m1), ("m5", m2)] HashMap.empty) >>= \case
        Right (ids, _) -> ids `shouldBe` ["m1", "m2", "m4", "m5", "m5"]
        Left _ -> expectationFailure "processMessages returned Left WriteFailure"
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

    -- Monitor queries and bulk actions were keyed on monitor id alone, so a user on one
    -- project could mute/deactivate/resolve/delete or toggle another tenant's monitors.
    it "tenant boundary: bulk actions and the single toggle never touch another project's monitor; non-members are refused" \tr -> do
      otherPid <- createTestProject tr "monitors-authz-victim"
      let victimId = Monitors.QueryMonitorId [uuid|0000ffff-0000-0000-0000-00000000cafe|]
      void $ runTestBgNoReset tr $ Monitors.queryMonitorUpsert def{Monitors.id = victimId, Monitors.projectId = otherPid, Monitors.logQuery = "status_code == 200", Monitors.alertThreshold = 1, Monitors.checkIntervalMins = 5, Monitors.timeWindowMins = 5}
      void $ testServant tr $ Alerts.alertSingleToggleActiveH testPid victimId
      (runQueryEffect tr (Monitors.queryMonitorById victimId) <&> (>>= (.deactivatedAt))) `shouldReturn` Nothing
      -- Every action, so a newly added one is covered without anyone remembering to add it here.
      for_ [minBound .. maxBound] \action -> void $ testServant tr $ Alerts.alertBulkActionH testPid action (ProjectPages.TBulkActionForm [victimId.unwrap])
      map (.id) <$> runQueryEffect tr (Monitors.queryMonitorsAll otherPid) `shouldReturn` [victimId]
      assertNonMemberDenied tr otherPid $ Alerts.unifiedMonitorsGetH otherPid Nothing Nothing
      assertNonMemberDenied tr otherPid $ Alerts.alertSingleGetH otherPid victimId
      assertNonMemberDenied tr otherPid $ Alerts.alertSingleToggleActiveH otherPid victimId
      assertNonMemberDenied tr otherPid $ Alerts.alertUpsertPostH otherPid alertForm
      assertNonMemberDenied tr otherPid $ Alerts.alertBulkActionH otherPid Monitors.BADelete (ProjectPages.TBulkActionForm [victimId.unwrap])

    it "widget monitor: alerts and recovers for the saved widget, follows renames, links copies to their dashboard, deletes with the widget" \tr -> do
      void $ testServant tr $ Dashboards.dashboardsPostH testPid Dashboards.DashboardForm{Dashboards.title = "Widget Monitor Lifecycle", Dashboards.file = "widget-monitor.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
      dashboardId <-
        call tr (Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing (Dashboards.DashboardFilters [])) >>= \case
          Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> maybe (fail "the dashboard was not saved") (pure . (.id)) $ V.find ((== "Widget Monitor Lifecycle") . (.title)) dashboards
          _ -> fail "the dashboard list did not load"
      let otherPid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
      isNothing <$> runTestBgNoReset tr (DashboardModel.getDashboardByProjectId otherPid dashboardId) `shouldReturn` True
      savedWidget <- call tr $ Dashboards.dashboardWidgetPutH testPid dashboardId Nothing Nothing def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Checkout errors", Widget.query = Just "name == \"checkout\""}
      widgetId <- maybe (fail "the saved widget has no id") pure savedWidget.id
      let widgetMonitor pid = runTestBgNoReset tr $ Monitors.queryMonitorByWidgetId pid (Just dashboardId) widgetId
          widgetUrl = "/p/" <> testPid.toText <> "/dashboards/" <> dashboardId.toText <> "?expand=" <> widgetId
          linksTo url notification = unless (url `T.isInfixOf` jsonText notification) $ expectationFailure $ "Missing widget link in " <> show (deliverySummary [notification])
          putWidget w = void $ testServant tr $ Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing w
          rejected :: ATAuthCtx a -> IO ()
          rejected k = void (toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader k) `shouldThrow` anyIOException
          check = advanceMinutes tr 1 >> deliverySummary . fst <$> captureNotifs tr checkTriggeredQueryMonitors
          alertForm' =
            Dashboards.WidgetAlertForm
              { widgetId
              , query = "name == \"checkout\""
              , vizType = Just "timeseries"
              , unit = Just "events"
              , alertThreshold = Just "1"
              , warningThreshold = Nothing
              , direction = Issues.Above
              , alertRecoveryThreshold = Nothing
              , warningRecoveryThreshold = Nothing
              , frequency = Just "1m"
              , timeWindow = Just "1h"
              , severity = Just "Error"
              , subject = Just "Checkout errors"
              , message = Just "A checkout request failed"
              , notifyAfterCheck = Nothing
              , notifyAfter = Nothing
              , stopAfterCheck = Just True
              , stopAfter = Just 1
              , teams = []
              }
      dbExec
        tr
        [sql|UPDATE projects.teams SET notify_emails = ARRAY['alerts@example.com'], discord_channels = ARRAY['C_MONITOR'], phone_numbers = ARRAY['+15550001111'],
               pagerduty_services = ARRAY['widget-monitor-test'], disabled_channels = '{}' WHERE project_id = ? AND handle = 'everyone'|]
        (Only testPid)
      -- A raw-SQL widget posts an empty query, which compiled to an all-traffic count.
      rejected $ Dashboards.widgetAlertUpsertH testPid widgetId (Just dashboardId) alertForm'{Dashboards.query = ""}
      rejected $ Dashboards.widgetAlertUpsertH testPid widgetId (Just dashboardId) alertForm'{Dashboards.alertThreshold = Nothing}
      isNothing <$> widgetMonitor testPid `shouldReturn` True
      void $ testServant tr $ Dashboards.widgetAlertUpsertH testPid widgetId (Just dashboardId) alertForm'
      createdMonitor <- widgetMonitor testPid
      ((\m -> (m.alertConfig.title, m.alertConfig.emailAll)) <$> createdMonitor) `shouldBe` Just ("Checkout errors · Widget Monitor Lifecycle", True)
      overview <- call tr $ Alerts.unifiedMonitorOverviewH testPid (maybe "" (.id.toText) createdMonitor)
      renderT overview `shouldSatisfy` T.isInfixOf ("href=\"" <> widgetUrl <> "\"")
      setupSlackData tr testPid "T_MONITOR_SIM"
      setupDiscordData tr testPid "G_MONITOR_SIM"
      putWidget savedWidget{Widget.title = Just "Checkout failures", Widget.alertThreshold = Just 1, Widget.unit = Just "events"}
      fmap (.alertConfig.title) <$> widgetMonitor testPid `shouldReturn` Just "Checkout failures · Widget Monitor Lifecycle"
      void $ testServant tr $ Dashboards.dashboardRenamePatchH testPid dashboardId Dashboards.DashboardRenameForm{Dashboards.title = "Renamed Dashboard", Dashboards.fileDir = Nothing}
      monitorId <- widgetMonitor testPid >>= maybe (fail "the widget monitor was not saved") (\m -> m.id <$ (m.alertConfig.title `shouldBe` "Checkout failures · Renamed Dashboard"))
      -- An explorer edit of a widget monitor keeps the derived title, not the form's stale hidden one.
      void $ testServant tr $ Alerts.alertUpsertPostH testPid alertForm{Alerts.unit = Just "events", Alerts.alertId = Just monitorId.toText, Alerts.recipientEmails = [], Alerts.recipientEmailAll = Nothing, Alerts.title = "Stale hidden title", Alerts.severity = "Error", Alerts.subject = "Checkout errors", Alerts.message = "A checkout request failed", Alerts.query = "name == \"checkout\"", Alerts.since = "1h", Alerts.from = "", Alerts.to = "", Alerts.frequency = Just "1m", Alerts.timeWindow = Just "1h", Alerts.source = Just "widget", Alerts.vizType = Just "timeseries", Alerts.widgetId = Just widgetId, Alerts.dashboardId = Just dashboardId.toText, Alerts.environment = Nothing, Alerts.service = Nothing}
      fmap (.alertConfig.title) <$> widgetMonitor testPid `shouldReturn` Just "Checkout failures · Renamed Dashboard"
      savedDash <- runTestBgNoReset tr $ DashboardModel.getDashboardByProjectId testPid dashboardId
      (savedDash >>= (.schema) >>= find ((== Just widgetId) . (.id)) . DashboardModel.allWidgets >>= (.alertThreshold)) `shouldBe` Just 1
      isNothing <$> widgetMonitor otherPid `shouldReturn` True
      -- Re-saving the alert form keeps a paused monitor paused.
      void $ testServant tr $ Dashboards.widgetAlertToggleH testPid widgetId (Just dashboardId)
      isJust . (>>= (.deactivatedAt)) <$> widgetMonitor testPid `shouldReturn` True
      void $ testServant tr $ Dashboards.widgetAlertUpsertH testPid widgetId (Just dashboardId) alertForm'
      isJust . (>>= (.deactivatedAt)) <$> widgetMonitor testPid `shouldReturn` True
      void $ testServant tr $ Dashboards.widgetAlertToggleH testPid widgetId (Just dashboardId)

      apiKey <- createTestAPIKey tr testPid "widget-monitor"
      getCurrentTime >>= ingestTrace tr apiKey "checkout"
      advanceMinutes tr 1
      fired <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      firedMonitor <- widgetMonitor testPid >>= maybe (fail "the widget monitor disappeared") pure
      (firedMonitor.alertConfig.unit, firedMonitor.alertThreshold, firedMonitor.warningThreshold) `shouldBe` (Just "events", 1, Nothing)
      (firedMonitor.currentStatus, firedMonitor.currentValue, firedMonitor.notificationCount) `shouldBe` (Monitors.MSAlerting, 1, 1)
      deliverySummary fired `shouldBe` ["discord:C_MONITOR", "email:alerts@example.com", "pagerduty:widget-monitor-test:PDTrigger", "slack:C_NOTIF_CHANNEL", "whatsapp:+15550001111"]
      mapM_ (linksTo widgetUrl) fired
      check `shouldReturn` []
      -- A webhook accepts the root without its timestamp: capture the app's own message before lifecycle replies.
      rootId <- dbOne tr [sql|SELECT r.id FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.project_id = ? AND e.source_id = ? AND r.channel_id = 'C_NOTIF_CHANNEL'|] (testPid, firedMonitor.id)
      receipt <- receiveSlackEvent tr $ slackRootEvent "EvMonitorRoot" "T_MONITOR_SIM" "C_NOTIF_CHANNEL" rootId "1788900000.000001" "A_TEST"
      runTestBgNoReset tr (I.captureSlackRoot "A_TEST" receipt) `shouldReturn` True
      deliverySummary . fst <$> captureNotifs tr runSlackIncidentDeliveries `shouldReturn` ["slack-update:C_NOTIF_CHANNEL"]

      rejected $ Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing savedWidget{Widget.alertThreshold = Nothing}
      putWidget savedWidget{Widget.query = Just "name == \"search\"", Widget.alertThreshold = Just 2}
      fmap (\m -> (m.logQuery, m.alertThreshold)) <$> widgetMonitor testPid `shouldReturn` Just ("name == \"search\"", 2)
      advanceMinutes tr 1
      recovered <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      deliverySummary recovered `shouldBe` ["discord:C_MONITOR", "email:alerts@example.com", "pagerduty:widget-monitor-test:PDResolve", "slack-update:C_NOTIF_CHANNEL", "slack:C_NOTIF_CHANNEL", "whatsapp:+15550001111"]
      mapM_ (linksTo widgetUrl) recovered
      -- Project-level channel toggles gate delivery.
      void $ testServant tr $ ProjectPages.updateNotificationsChannel testPid $ ProjectPages.NotifListForm ["email", "discord", "pagerduty"] [] ["alerts@example.com"] [] Nothing
      putWidget savedWidget{Widget.alertThreshold = Just 1, Widget.unit = Just "events"}
      check `shouldReturn` ["discord:C_MONITOR", "email:alerts@example.com", "pagerduty:widget-monitor-test:PDTrigger"]
      void $ testServant tr $ ProjectPages.updateNotificationsChannel testPid $ ProjectPages.NotifListForm ["email", "slack", "discord", "phone", "pagerduty"] ["+15550001111"] ["alerts@example.com"] [] Nothing

      -- A copied widget gets its own monitor with fresh state, titled and linked for its dashboard.
      (.notificationCount) <$> (widgetMonitor testPid >>= maybe (fail "the source widget has no monitor") pure) `shouldReturn` 1
      let copyDashboardId = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef1
      sourceDashboardVM <- maybe (fail "the source dashboard disappeared") pure savedDash
      void $ runTestBgNoReset tr $ DashboardModel.insert sourceDashboardVM{DashboardModel.id = copyDashboardId, DashboardModel.title = "Widget Monitor Copy", DashboardModel.schema = Just (def :: DashboardModel.Dashboard){DashboardModel.title = Just "Widget Monitor Copy"}, DashboardModel.filePath = Nothing}
      copyWidget <- call tr $ Dashboards.dashboardDuplicateWidgetPostH testPid copyDashboardId widgetId (Just dashboardId)
      copyWidgetId <- maybe (fail "the copied widget has no id") pure copyWidget.id
      copyMonitor <- runTestBgNoReset tr (Monitors.queryMonitorByWidgetId testPid (Just copyDashboardId) copyWidgetId) >>= maybe (fail "the copied widget has no monitor") pure
      (copyMonitor.notificationCount, copyMonitor.currentValue, copyMonitor.alertConfig.title) `shouldBe` (0, 0, "Checkout errors · Widget Monitor Copy")
      copyNotifs <- fst <$> captureNotifs tr (evaluateQueryMonitorValue copyMonitor (addUTCTime 3600 frozenTime) 1)
      deliverySummary copyNotifs `shouldBe` ["discord:C_MONITOR", "email:alerts@example.com", "pagerduty:widget-monitor-test:PDTrigger", "whatsapp:+15550001111"]
      mapM_ (linksTo $ "/p/" <> testPid.toText <> "/dashboards/" <> copyDashboardId.toText <> "?expand=" <> copyWidgetId) copyNotifs
      void $ testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid copyDashboardId Nothing Map.empty

      -- Deleting the widget deletes its monitor, and only this project's.
      let otherMonitorId = Monitors.QueryMonitorId [uuid|a11e7ed0-0000-0000-0000-000000000001|]
      void $ runTestBgNoReset tr $ Monitors.queryMonitorUpsert firedMonitor{Monitors.id = otherMonitorId, Monitors.projectId = otherPid, Monitors.currentStatus = Monitors.MSNormal}
      void $ testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashboardId Nothing Map.empty
      isNothing <$> widgetMonitor testPid `shouldReturn` True
      fmap (.id) <$> widgetMonitor otherPid `shouldReturn` Just otherMonitorId

    it "state machine: thresholds round-trip, alert once, hysteresis between warning and alerting, one issue per firing, scoped and pruned readings" \tr -> do
      let hystStored = Monitors.QueryMonitorId [uuid|11111111-1111-1111-1111-111111111111|]
          once = Monitors.QueryMonitorId [uuid|22222222-2222-2222-2222-222222222222|]
          hyst = Monitors.QueryMonitorId [uuid|33333333-3333-3333-3333-333333333333|]
          readings = Monitors.QueryMonitorId $ UUID.fromWords 0 0 0 902
      stored <-
        either (fail . toString) pure
          $ Alerts.convertToQueryMonitor testPid frozenTime hystStored
          $ alertForm{Alerts.alertId = Just (UUID.toText hystStored.unwrap), Alerts.warningThreshold = Just "80", Alerts.alertThreshold = 100, Alerts.query = "", Alerts.frequency = Just "1m", Alerts.alertRecoveryThreshold = Just "60", Alerts.warningRecoveryThreshold = Just "50", Alerts.environment = Nothing, Alerts.service = Nothing}
      runTestBg frozenTime tr (Monitors.queryMonitorUpsert stored) `shouldReturn` 1
      m0 <- fetchMonitor tr hystStored
      (m0.alertThreshold, m0.warningThreshold, m0.alertRecoveryThreshold, m0.warningRecoveryThreshold, m0.currentStatus) `shouldBe` (100, Just 80, Just 60, Just 50, Monitors.MSNormal)

      -- Alerts once; neither a repeat nor a late repeat re-triggers; recovery clears both triggers.
      insertPipelineMonitor tr once 100 Nothing Nothing
      evalMonitorValueAt t0 tr once 150
      alerted <- fetchMonitor tr once
      (alerted.currentStatus, alerted.currentValue, isJust alerted.alertLastTriggered) `shouldBe` (Monitors.MSAlerting, 150, True)
      for_ [300, 3660] \dt -> do
        resetLastEvaluated tr once
        evalMonitorValueAt (addUTCTime dt t0) tr once 150
        m <- fetchMonitor tr once
        (m.currentStatus, m.alertLastTriggered, m.notificationCount) `shouldBe` (Monitors.MSAlerting, alerted.alertLastTriggered, alerted.notificationCount)
      evalMonitorValueAt (addUTCTime 3900 t0) tr once 50
      recovered <- fetchMonitor tr once
      (recovered.currentStatus, recovered.alertLastTriggered, recovered.warningLastTriggered) `shouldBe` (Monitors.MSNormal, Nothing, Nothing)

      -- 90 warns (80 < 90 < 100), 110 alerts, 70 stays alerting above the 60 recovery, 55 recovers.
      -- Each transition rewrites the one open issue (even a legacy threshold-bearing title) and links it.
      insertPipelineMonitor tr hyst 100 (Just 80) (Just 60)
      let assertIssue threshold value notifications = do
            rows <- dbQuery @_ @(Issues.IssueId, Text, Aeson Issues.QueryAlertData) tr [sql| SELECT id, title, issue_data FROM apis.issues WHERE project_id = ? AND target_hash = ? AND issue_type = 'query_alert' AND acknowledged_at IS NULL AND archived_at IS NULL |] (testPid, Monitors.queryAlertKey hyst)
            [(title, d.thresholdValue, d.actualValue) | (_, title, Aeson d) <- rows] `shouldBe` [("Pipeline Test Monitor", threshold, value)]
            let emails = [email.htmlBody | EmailNotification email <- notifications]
            emails `shouldSatisfy` (not . null)
            for_ rows \(iid, _, _) -> for_ emails (`shouldSatisfy` T.isInfixOf ("/issues/" <> iid.toText))
          statusAfter dt v = evalMonitorValueAt (addUTCTime dt t0) tr hyst v >> (.currentStatus) <$> fetchMonitor tr hyst
      evalMonitorValueWithNotifs t0 tr hyst 90 >>= assertIssue 80 90
      (.currentStatus) <$> fetchMonitor tr hyst `shouldReturn` Monitors.MSWarning
      dbExec tr [sql|UPDATE apis.issues SET title = 'Pipeline Test Monitor threshold above 80' WHERE project_id = ? AND target_hash = ?|] (testPid, Monitors.queryAlertKey hyst)
      evalMonitorValueWithNotifs (addUTCTime 120 t0) tr hyst 110 >>= assertIssue 100 110
      (.currentStatus) <$> fetchMonitor tr hyst `shouldReturn` Monitors.MSAlerting
      statusAfter 240 70 `shouldReturn` Monitors.MSAlerting
      statusAfter 360 55 `shouldReturn` Monitors.MSNormal

      -- Actual scalar readings are recorded once per evaluation, scoped to the project, pruned by age.
      let from = addUTCTime (-300) t0
          to = addUTCTime 60 t0
      insertPipelineMonitor tr readings 60 Nothing Nothing
      evalMonitorValueAt from tr readings 84
      evalMonitorValueAt t0 tr readings 18
      runTestBgNoReset tr $ Monitors.recordEvaluation testPid readings t0 999 Monitors.MSAlerting
      runTestBgNoReset tr (Monitors.getEvaluations testPid readings from to) `shouldReturn` [(from, 84), (t0, 18)]
      runTestBgNoReset tr (Monitors.getEvaluations (UUIDId $ UUID.fromWords 0 0 0 999) readings from to) `shouldReturn` []
      runTestBgNoReset tr $ Monitors.pruneEvaluations t0
      runTestBgNoReset tr (Monitors.getEvaluations testPid readings from to) `shouldReturn` [(t0, 18)]

    it "scheduler robustness: intervals respected, broken and never-evaluated monitors neither loop nor blind the rest, missing telemetry invents nothing, metrics evaluate" \tr -> do
      let skip = Monitors.QueryMonitorId [uuid|44444444-4444-4444-4444-444444444444|]
          broken = Monitors.QueryMonitorId [uuid|66666666-6666-6666-6666-666666666666|]
          healthy = Monitors.QueryMonitorId [uuid|88888888-8888-8888-8888-888888888888|]
          never = Monitors.QueryMonitorId [uuid|99999999-9999-9999-9999-999999999999|]
          gauge = Monitors.QueryMonitorId $ UUID.fromWords 0 0 0 903
          metric = Monitors.QueryMonitorId $ UUID.fromWords 0 0 0 901
      insertPipelineMonitor tr skip 100 Nothing Nothing
      dbExec tr [sql|UPDATE monitors.query_monitors SET check_interval_mins = 60, last_evaluated = ? WHERE id = ?|] (addUTCTime (-1800) t0, skip)
      insertPipelineMonitor tr broken 100 Nothing Nothing
      setMonitorQuery tr broken "SELECT * FROM a_table_that_does_not_exist"
      insertPipelineMonitor tr healthy 0 Nothing Nothing
      insertPipelineMonitor tr never 0 Nothing Nothing
      -- last_evaluated NULL failed the decoder for the whole row set: one never-evaluated monitor
      -- stopped every monitor from being checked (10,384 failed QueryMonitorsCheck jobs from 2026-08-07).
      dbExec tr [sql|UPDATE monitors.query_monitors SET last_evaluated = NULL WHERE id = ?|] (Only never)
      actives <- runTestBg t0 tr Monitors.getActiveQueryMonitors
      [i | i <- [skip, broken, healthy, never], i `notElem` map (.id) actives] `shouldBe` []
      (find ((== never) . (.id)) actives >>= (.lastEvaluated)) `shouldBe` Nothing
      evalMonitorsAt t0 tr
      (.currentStatus) <$> fetchMonitor tr skip `shouldReturn` Monitors.MSNormal
      -- A throwing query still advances its watermark (else every tick re-picks it and hammers the
      -- failing backend), raises no alert, and the monitors after it in the same tick still run.
      b <- fetchMonitor tr broken
      (b.lastEvaluated > Just (addUTCTime (-86400) t0), b.currentStatus) `shouldBe` (True, Monitors.MSNormal)
      (.currentStatus) <$> fetchMonitor tr healthy `shouldReturn` Monitors.MSAlerting
      n <- fetchMonitor tr never
      (n.currentStatus, isJust n.lastEvaluated) `shouldBe` (Monitors.MSAlerting, True)

      -- An alerting gauge that loses telemetry keeps its last reading rather than a zero or a recovery.
      insertPipelineMonitor tr gauge 60 Nothing Nothing
      evalMonitorValueAt t0 tr gauge 84
      setMonitorQuery tr gauge "name == \"missing-slack-agent-gauge\" | summarize max(duration)"
      evalMonitorsAt (addUTCTime 120 t0) tr
      g <- fetchMonitor tr gauge
      (g.currentStatus, g.currentValue) `shouldBe` (Monitors.MSAlerting, 84)
      runTestBgNoReset tr (Monitors.getEvaluations testPid gauge t0 (addUTCTime 120 t0)) `shouldReturn` [(t0, 84)]

      apiKey <- createTestAPIKey tr testPid "monitor-metric-values"
      now <- getCurrentTime
      ingestMetric tr apiKey [] [] "monitor.regression.pressure" 28 (addUTCTime (-2) now)
      ingestMetric tr apiKey [] [] "monitor.regression.pressure" 80 (addUTCTime (-1) now)
      insertPipelineMonitor tr metric 70 Nothing Nothing
      setMonitorQuery tr metric "metrics | where metric_name == \"monitor.regression.pressure\" | summarize max(value)"
      evalMonitorsAt now tr
      ((,) <$> (.currentValue) <*> (.currentStatus)) <$> fetchMonitor tr metric `shouldReturn` (80, Monitors.MSAlerting)

    it "notification policy: free tier emails only the owner, chart snapshot, reminders until stop-after, recovery always, mute suppresses then resumes" \tr -> do
      let free = Monitors.QueryMonitorId [uuid|22222222-2222-2222-2222-222222222222|]
          snapshot = Monitors.QueryMonitorId $ UUID.fromWords 0 0 0 904
          renotify = Monitors.QueryMonitorId [uuid|55555555-5555-5555-5555-555555555555|]
          muted = Monitors.QueryMonitorId [uuid|66666666-6666-6666-6666-666666666666|]
          emailsTo ns = [email.receiver | EmailNotification email <- ns]
      insertPipelineMonitor tr free 100 Nothing Nothing
      dbExec tr [sql|UPDATE projects.projects SET payment_plan = 'Free' WHERE id = ?|] (Only testPid)
      owner <- runQueryEffect tr (ProjectMembers.selectActiveProjectMembers testPid) >>= maybe (fail "Owner missing") pure . listToMaybe
      let ownerEmail = CI.original owner.email
      dbExec tr [sql|UPDATE projects.teams SET notify_emails = ARRAY[?, 'nonowner@example.com'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (ownerEmail, testPid)
      (testEmails, _) <- testServantWithNotifications tr $ Settings.notificationsTestPostH testPid (Settings.TestForm "new_endpoint" Settings.TCEmail Nothing)
      emailsTo testEmails `shouldBe` [ownerEmail]
      emailsTo <$> evalMonitorValueWithNotifs t0 tr free 150 `shouldReturn` [ownerEmail]
      void $ evalMonitorValueWithNotifs (addUTCTime 60 t0) tr free 50
      dbExec tr [sql|UPDATE projects.projects SET payment_plan = 'PAID' WHERE id = ?|] (Only testPid)
      emailsTo <$> evalMonitorValueWithNotifs (addUTCTime 120 t0) tr free 150 >>= (`shouldMatchList` [ownerEmail, "nonowner@example.com"])

      -- The email embeds a bounded snapshot of actual readings with a gap, not a rewritten scalar query.
      let earlier = addUTCTime (-300) t0
          millis = floor . (* 1000) . utcTimeToPOSIXSeconds :: UTCTime -> Int
          row at value = AE.toJSON ([AE.toJSON (millis at), AE.toJSON value] :: [AE.Value])
      insertRenotifyMonitor tr snapshot 60
      original <- fetchMonitor tr snapshot
      void $ runTestBgNoReset tr $ Monitors.queryMonitorUpsert original{Monitors.alertConfig = original.alertConfig{Monitors.unit = Just "s"}}
      setMonitorRenotifyConfig tr snapshot (Just 1) Nothing 0
      evalMonitorValueAt earlier tr snapshot 84
      notifications <- evalMonitorValueWithNotifs t0 tr snapshot 100
      suffix <- maybe (fail "monitor notification had no chart snapshot") pure $ listToMaybe [s | EmailNotification email <- notifications, s <- drop 1 $ T.splitOn "widgetZ=" email.htmlBody]
      compressed <- either (fail . toString) pure $ B64URL.decodeBase64Untyped $ encodeUtf8 $ T.takeWhile (/= '&') suffix
      widget <- either fail pure $ AE.eitherDecode @Widget.Widget $ GZip.decompress $ fromStrict compressed
      (widget.query, widget.unit, widget.alertThreshold, widget.pngProfile, Widget.mapWidgetTypeToChartType widget.wType) `shouldBe` (Nothing, Just "s", Just 60, Just Widget.PngSlack, "line")
      Widget.pngExportSize widget.pngProfile `shouldBe` (960, 320)
      dataset <- maybe (fail "monitor snapshot had no dataset") pure widget.dataset
      (dataset.from, dataset.to) `shouldBe` (Just $ millis $ addUTCTime (-900) t0, Just $ millis t0)
      dataset.source `shouldBe` AE.toJSON @[AE.Value] [AE.toJSON (["timestamp", "Monitor value"] :: [Text]), row earlier (Just (84 :: Double)), row (addUTCTime 60 earlier) (Nothing :: Maybe Double), row t0 (Just (100 :: Double))]
      configured <- fetchMonitor tr snapshot
      let (root, _) = Mail.monitorIncidentMessages configured 100 Monitors.MSAlerting t0 Nothing "https://example.com/incident" "https://example.com/monitor" Nothing
      jsonText root `shouldContainAll` ["100.0 s", "60.0 s", "Chart unavailable."]
      jsonText (Mail.monitorDataUnavailableMessage configured Monitors.NoMeasurements t0 (Just (earlier, 84)) "https://example.com/incident" "https://example.com/monitor") `shouldSatisfy` T.isInfixOf "84.0 s"

      -- Reminders: none by default, only when the interval is due, counted up to stop-after; recovery always reports.
      insertRenotifyMonitor tr renotify 100
      let at mins v = do
            resetLastEvaluated tr renotify
            setAlertLastTriggered tr renotify (Just t0)
            ns <- evalMonitorValueWithNotifs (addUTCTime (mins * 60) t0) tr renotify v
            (length ns,) . (.notificationCount) <$> fetchMonitor tr renotify
      notifs1 <- evalMonitorValueWithNotifs t0 tr renotify 150
      m1 <- fetchMonitor tr renotify
      (m1.currentStatus, m1.notificationCount) `shouldBe` (Monitors.MSAlerting, 1)
      (any (\case EmailNotification{} -> True; _ -> False) notifs1, any (\case PagerdutyNotification{} -> True; _ -> False) notifs1) `shouldBe` (True, True)
      resetLastEvaluated tr renotify
      length <$> evalMonitorValueWithNotifs (addUTCTime 3660 t0) tr renotify 150 `shouldReturn` 0
      (\m -> (m.notificationCount, m.alertLastTriggered)) <$> fetchMonitor tr renotify `shouldReturn` (1, m1.alertLastTriggered)
      setMonitorRenotifyConfig tr renotify (Just 30) Nothing 1
      resetLastEvaluated tr renotify
      setAlertLastTriggered tr renotify (Just $ addUTCTime (36 * 60) t0)
      length <$> evalMonitorValueWithNotifs (addUTCTime (65 * 60) t0) tr renotify 150 `shouldReturn` 0
      (\(n, c) -> (n > 0, c)) <$> at 66 150 `shouldReturn` (True, 2)
      setMonitorRenotifyConfig tr renotify (Just 30) (Just 3) 2
      (\(n, c) -> (n > 0, c)) <$> at 67 150 `shouldReturn` (True, 3)
      at 100 150 `shouldReturn` (0, 3)
      recoveryNotifs <- evalMonitorValueWithNotifs (addUTCTime (105 * 60) t0) tr renotify 50
      r <- fetchMonitor tr renotify
      (r.currentStatus, r.notificationCount, r.alertLastTriggered, null recoveryNotifs) `shouldBe` (Monitors.MSNormal, 0, Nothing, False)

      let evalT = addUTCTime (200 * 60) t0
      insertRenotifyMonitor tr muted 100
      setMonitorRenotifyConfig tr muted Nothing Nothing 0
      evalMonitorValueWithNotifs evalT tr muted 150 >>= (`shouldSatisfy` not . null)
      dbExec tr [sql|UPDATE monitors.query_monitors SET muted_until = ?, notification_count = 0, alert_last_triggered = NULL WHERE id = ?|] (addUTCTime 3600 evalT, muted)
      resetLastEvaluated tr muted
      length <$> evalMonitorValueWithNotifs (addUTCTime 1800 evalT) tr muted 150 `shouldReturn` 0
      (.notificationCount) <$> fetchMonitor tr muted `shouldReturn` 0
      resetLastEvaluated tr muted
      evalMonitorValueWithNotifs (addUTCTime 3660 evalT) tr muted 150 >>= (`shouldSatisfy` not . null)
      resumed <- fetchMonitor tr muted
      (resumed.currentStatus, resumed.notificationCount) `shouldBe` (Monitors.MSAlerting, 1)

    describe "incident delivery" do
      it "commits ingested error notification claims with two destination roots and retries an outbox failure" \tr -> do
        setupSlackData tr testPid "T1"
        dbExec tr [sql|UPDATE projects.teams SET slack_channels = ARRAY['C1','C2','C1'], disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (Only testPid)
        dbExec tr [sql|UPDATE projects.projects SET error_alerts = true WHERE id = ?|] (Only testPid)
        rejectDeliveries tr
        let sample = def{Errors.when = frozenTime, Errors.hash = "ingested-outbox", Errors.errorType = "CheckoutError", Errors.message = "Payment failed"}
            ingest = captureNotifs tr $ processProjectErrors testPid [sample] frozenTime
        ingest `shouldThrow` anyException
        Just unnotified <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
        unnotified.lastNotifiedAt `shouldBe` Nothing
        dbOne @_ @Int64 tr [sql|SELECT count(*) FROM apis.notification_rate_limit WHERE project_id = ?|] (Only testPid) `shouldReturn` 0
        dbQuery tr [sql|SELECT last_notified_at FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, sample.hash) `shouldReturn` [Only (Nothing :: Maybe UTCTime)]
        acceptDeliveries tr
        ((batchA, _), (batchB, _)) <- concurrently ingest ingest
        let initial = slack $ batchA <> batchB
        (sort (map (.channelId) initial), map (.threadTs) initial) `shouldBe` (["C1", "C2"], [Nothing, Nothing])
        Just notified <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
        notified.lastNotifiedAt `shouldSatisfy` isJust
        dbExec tr [sql|UPDATE apis.error_patterns SET subscribed = true, notify_every_minutes = 1 WHERE id = ?|] (Only notified.id)
        advanceMinutes tr 2
        updates <- slack . fst <$> captureNotifs tr (notifyErrorSubscriptions testPid [sample.hash] >> replicateM_ 2 runSlackIncidentDeliveries)
        (length updates, length (filter (isJust . (.threadTs)) updates)) `shouldBe` (4, 2)
        dbOne @_ @Int64 tr [sql|SELECT count(*) FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.source_kind = 'error' AND e.source_id = ?|] (Only notified.id) `shouldReturn` 2
        -- Quiet counters alone cannot close an error or establish recovery.
        dbExec tr [sql|UPDATE apis.error_patterns SET quiet_minutes = resolution_threshold_minutes + 1, occurrences_1m = 0, occurrences_5m = 0, occurrences_1h = 0, occurrences_24h = 0 WHERE id = ?|] (Only notified.id)
        now <- getTestTime tr.trTestClock
        void $ runTestBgNoReset tr $ Errors.updateOccurrenceCountsBatch [testPid] now
        Just quiet <- runTestBgNoReset tr $ Errors.getErrorPatternById notified.id
        (quiet.state, isNothing quiet.resolvedAt, quiet.resolvedBy) `shouldBe` (notified.state, True, Nothing)
        -- A newer acknowledged issue must win over an older unnotified escalation.
        dbExec tr [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ? WHERE project_id = ? AND target_hash = ?|] (frozenTime, addUTCTime 3600 frozenTime, testPid, sample.hash)
        newer <- runTestBgNoReset tr $ Issues.createNewErrorIssue testPid notified >>= Issues.insertIssueReturningId
        dbExec tr [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ?, last_notified_at = ? WHERE id = ?|] (frozenTime, addUTCTime 3600 frozenTime, frozenTime, newer)
        dbExec tr [sql|UPDATE apis.issues SET acknowledged_at = NULL, acknowledged_until = NULL, last_notified_at = NULL WHERE project_id = ? AND target_hash = ? AND id <> ?|] (testPid, sample.hash, newer)
        dbExec tr [sql|UPDATE apis.error_patterns SET state = 'escalating' WHERE id = ?|] (Only notified.id)
        advanceMinutes tr 1
        slack . fst <$> captureNotifs tr (notifyErrorSubscriptions testPid [sample.hash]) >>= (`shouldSatisfy` null)
        void $ testServant tr $ IssuesPage.resolveErrorPostH testPid notified.id.unErrorPatternId False
        closed <- slack . fst <$> captureNotifs tr (replicateM_ 2 runSlackIncidentDeliveries)
        length closed `shouldBe` 4
        for_ closed \sd -> slackText sd `shouldSatisfy` T.isInfixOf "RESOLVED"

      it "keeps one error episode across different issue records and resolves its original incident link" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let sample = def{Errors.when = frozenTime, Errors.hash = "stable-error-episode", Errors.errorType = "CheckoutError"}
            newIssue err = runTestBgNoReset tr do
              created <- Issues.createNewErrorIssue testPid err
              created <$ Issues.insertIssue created
            payload iid = I.SlackPayload $ KM.fromList ["text" AE..= ("Error observed" :: Text), "blocks" AE..= ([AE.object ["type" AE..= ("section" :: Text), "block_id" AE..= ("incident_actions" :: Text), "text" AE..= AE.object ["type" AE..= ("mrkdwn" :: Text), "text" AE..= ("<https://app.monoscope.tech/p/" <> testPid.toText <> "/issues/" <> iid.toText <> "|Open incident>")]]] :: [AE.Value])]
        void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample] frozenTime
        Just err <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
        issue <- newIssue err
        let initial = update{I.source = I.ErrorIncident err.id, I.issueId = Just issue.id, I.rootPayload = payload issue.id}
            record :: I.IncidentUpdate -> IO I.RecordResult
            record u = runTestBgNoReset tr $ I.recordIncidentEvent u
            resolve = advanceMinutes tr 1 >> void (testServant tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False)
        I.Recorded episode firstEvent <- record initial
        void $ captureNotifs tr runSlackIncidentDeliveries
        -- Escalation can produce another issue row while the error remains active.
        dbExec tr [sql|UPDATE apis.issues SET archived_at = ? WHERE id = ?|] (frozenTime, issue.id)
        advanceMinutes tr 1
        nextIssue <- newIssue err
        nextIssue.id `shouldNotBe` issue.id
        now <- getTestTime tr.trTestClock
        record initial{I.projectId = UUIDId $ UUID.fromWords 0 0 0 999} `shouldReturn` I.Rejected I.UnknownIncidentSource
        record initial{I.issueId = Nothing} `shouldReturn` I.Rejected I.IncidentIssueMismatch
        dbExec tr [sql|UPDATE apis.issues SET target_hash = 'another-error' WHERE id = ?|] (Only nextIssue.id)
        record initial{I.issueId = Just nextIssue.id, I.observedAt = now} `shouldReturn` I.Rejected I.IncidentIssueMismatch
        dbExec tr [sql|UPDATE apis.issues SET target_hash = ? WHERE id = ?|] (sample.hash, nextIssue.id)
        I.Recorded ongoing _ <- record initial{I.issueId = Just nextIssue.id, I.observedAt = now, I.change = I.IncidentReminder}
        (ongoing.id, ongoing.issueId) `shouldBe` (episode.id, Just issue.id)
        void $ captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
        resolve
        fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
        resolution <- slack . fst <$> captureNotifs tr (replicateM_ 2 runSlackIncidentDeliveries)
        length resolution `shouldBe` 4
        for_ resolution \sd -> (issue.id.toText `T.isInfixOf` slackText sd, nextIssue.id.toText `T.isInfixOf` slackText sd) `shouldBe` (True, False)
        dbQuery tr [sql|SELECT channel_id FROM apis.slack_incident_roots WHERE episode_id = ? ORDER BY channel_id|] (Only episode.id) `shouldReturn` [Only ("C1" :: Text), Only "C2"]
        closedAt <- getTestTime tr.trTestClock
        record initial{I.observedAt = addUTCTime 1 closedAt} `shouldReturn` I.Rejected I.InactiveIncidentSource
        record initial `shouldReturn` I.AlreadyRecorded episode.id firstEvent
        advanceMinutes tr 1
        againAt <- getTestTime tr.trTestClock
        let canonicalSample = sample{Errors.hash = "canonical-error"} :: Errors.ATError
        void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample{Errors.when = againAt}, canonicalSample] againAt
        Just canonical <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid canonicalSample.hash
        dbExec tr [sql|UPDATE apis.error_patterns SET canonical_id = ? WHERE id = ?|] (canonical.id, err.id)
        record initial{I.observedAt = againAt} `shouldReturn` I.Rejected I.InactiveIncidentSource
        dbExec tr [sql|UPDATE apis.error_patterns SET canonical_id = NULL WHERE id = ?|] (Only err.id)
        I.Recorded recurring _ <- record initial{I.issueId = Just nextIssue.id, I.observedAt = againAt, I.rootPayload = payload nextIssue.id}
        recurring.id `shouldNotBe` episode.id
        void $ captureNotifs tr runSlackIncidentDeliveries
        dbExec tr [sql|DELETE FROM apis.issues WHERE id = ?|] (Only nextIssue.id)
        resolve
        fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid recurring.id) `shouldReturn` Just I.EpisodeResolved
        orphaned <- slack . fst <$> captureNotifs tr (replicateM_ 2 runSlackIncidentDeliveries)
        length orphaned `shouldBe` 4
        for_ orphaned \sd -> ("Open project issues" `T.isInfixOf` slackText sd, nextIssue.id.toText `T.isInfixOf` slackText sd) `shouldBe` (True, False)

      it "records manual error resolution once and updates both incident threads without claiming recovery" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let sample = def{Errors.when = frozenTime, Errors.hash = "manual-resolution", Errors.errorType = "CheckoutError", Errors.message = "payment declined"}
            actor = (Servant.getResponse tr.trSessAndHeader).user
            resolvePost eid = testServant tr $ IssuesPage.resolveErrorPostH testPid eid False
            activityOf iid = dbQuery tr [sql|SELECT event, created_by FROM apis.issue_activity_log WHERE issue_id = ? AND event IN ('resolved', 'auto_resolved') ORDER BY created_at|] (Only iid)
            membership = dbExec tr [sql|UPDATE projects.project_members SET permission = ?, active = ? WHERE project_id = ? AND user_id = ?|]
        void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample] frozenTime
        Just err <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid sample.hash
        issue <- runTestBgNoReset tr do
          created <- Issues.createNewErrorIssue testPid err
          created <$ Issues.insertIssue created
        I.Recorded episode _ <- runTestBgNoReset tr $ I.recordIncidentEvent update{I.source = I.IssueIncident issue.id, I.issueId = Just issue.id}
        void $ captureNotifs tr runSlackIncidentDeliveries
        advanceMinutes tr 1
        -- Viewers, inactive members and other projects cannot resolve; neither can a stale clock.
        membership ("view" :: Text, True, testPid, actor.id)
        void $ resolvePost err.id.unErrorPatternId
        (.state) <$> (runTestBgNoReset tr (Errors.getErrorPatternById err.id) >>= maybe (fail "error missing") pure) `shouldReturn` Errors.ESNew
        let resolve pid at = runTestBgNoReset tr $ I.resolveErrorIncident pid err.id actor.id at (const update.rootPayload)
        resolve testPid (addUTCTime 60 frozenTime) `shouldReturn` I.ErrorResolutionDenied
        resolve (UUIDId $ UUID.fromWords 0 0 0 999) (addUTCTime 60 frozenTime) `shouldReturn` I.ErrorResolutionDenied
        dbExec tr [sql|UPDATE apis.error_patterns SET assignee_id = ? WHERE id = ?|] (actor.id, err.id)
        resolve testPid (addUTCTime (-1) frozenTime) `shouldReturn` I.ErrorResolutionConflict I.OlderThanCurrentEpisode
        membership ("view" :: Text, False, testPid, actor.id)
        resolve testPid (addUTCTime 60 frozenTime) `shouldReturn` I.ErrorResolutionDenied
        membership ("view" :: Text, True, testPid, actor.id)
        -- An outbox failure rolls the whole resolution back.
        rejectDeliveries tr
        resolvePost err.id.unErrorPatternId `shouldThrow` anyException
        Just rolledBack <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
        (rolledBack.state, rolledBack.resolvedAt, rolledBack.resolvedBy) `shouldBe` (Errors.ESNew, Nothing, Nothing)
        runTestBgNoReset tr (Issues.selectIssueActivity testPid issue.id) >>= (`shouldSatisfy` all (\a -> a.event `notElem` map Issues.Lifecycle [Issues.IEResolved, Issues.IEAutoResolved]))
        acceptDeliveries tr
        void $ concurrently (resolvePost err.id.unErrorPatternId) (resolvePost err.id.unErrorPatternId)
        fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid episode.id) `shouldReturn` Just I.EpisodeResolved
        void $ resolvePost err.id.unErrorPatternId
        dbQuery tr [sql|SELECT event_kind, actor_id FROM apis.incident_events WHERE episode_id = ? AND event_kind <> 'alert'|] (Only episode.id) `shouldReturn` [("resolved" :: Text, Just actor.id)]
        activityOf issue.id `shouldReturn` [("resolved" :: Text, Just actor.id)]
        runTestBgNoReset tr (Errors.updateErrorPatternState err.id Errors.ESResolved (addUTCTime 61 frozenTime)) `shouldReturn` 0
        Just resolved <- runTestBgNoReset tr $ Errors.getErrorPatternLByHash testPid sample.hash (addUTCTime 61 frozenTime)
        resolved.base.resolvedBy `shouldBe` Just actor.id
        (replies, _) <- captureNotifs tr runSlackIncidentDeliveries
        (edits, _) <- captureNotifs tr runSlackIncidentDeliveries
        let messages = slack $ replies <> edits
        length messages `shouldBe` 4
        sort [(sd.channelId, sd.threadTs) | sd <- messages, isJust sd.threadTs] `shouldBe` sort [(sd.channelId, Just ts) | sd <- messages, AE.Object obj <- [sd.payload], Just (AE.String ts) <- [KM.lookup "ts" obj]]
        for_ messages \sd -> slackText sd `shouldContainAll` ["RESOLVED", actor.firstName, "Measured recovery has not been verified", issue.id.toText]
        fst <$> captureNotifs tr runSlackIncidentDeliveries `shouldReturn` []
        -- Recurrence regresses the error and opens a new episode; auto-resolution is attributed to nobody, at this transition's time.
        advanceMinutes tr 1
        now <- getTestTime tr.trTestClock
        void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [sample{Errors.when = now}] now
        Just regressed <- runTestBgNoReset tr $ Errors.getErrorPatternById err.id
        (regressed.state, regressed.resolvedBy) `shouldBe` (Errors.ESRegressed, Nothing)
        I.Recorded recurring _ <- runTestBgNoReset tr $ I.recordIncidentEvent update{I.source = I.IssueIncident issue.id, I.issueId = Just issue.id, I.observedAt = now}
        recurring.id `shouldNotBe` episode.id
        void $ runTestBgNoReset tr $ Errors.updateErrorPatternState err.id Errors.ESResolved now
        activityOf issue.id `shouldReturn` [("resolved" :: Text, Just actor.id), ("auto_resolved", Nothing)]
        void $ runTestBgNoReset tr $ Errors.updateErrorPatternState err.id Errors.ESOngoing now
        advanceMinutes tr 1
        autoAt <- getTestTime tr.trTestClock
        -- A SQL state transition can retain the previous resolved_at; activity must use this transition's clock.
        void $ runHasqlEffect tr $ EHasql.interpExecute [HI.sql|UPDATE apis.error_patterns SET state = 'resolved' WHERE id = #{err.id}|]
        dbQuery tr [sql|SELECT created_at FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'auto_resolved' ORDER BY created_at DESC LIMIT 1|] (Only issue.id) `shouldReturn` [Only autoAt]
        -- An error with no incident resolves without touching the outbox.
        let legacy = sample{Errors.hash = "legacy-resolution"} :: Errors.ATError
        void $ runTestBgNoReset tr $ Errors.batchUpsertErrorPatterns testPid [legacy] now
        Just legacyErr <- runTestBgNoReset tr $ Errors.getErrorPatternByHash testPid legacy.hash
        membership ("edit" :: Text, True, testPid, actor.id)
        let deliveryCount = dbOne @_ @Int64 tr [sql|SELECT count(*) FROM apis.slack_incident_deliveries|] ()
        beforeLegacy <- deliveryCount
        void $ resolvePost legacyErr.id.unErrorPatternId
        deliveryCount `shouldReturn` beforeLegacy
        Just legacyResolved <- runTestBgNoReset tr $ Errors.getErrorPatternById legacyErr.id
        (legacyResolved.state, legacyResolved.resolvedBy) `shouldBe` (Errors.ESResolved, Just actor.id)

      it "marks missing monitor data in existing threads and waits for a measured recovery" \tr -> do
        update <- setupWithIssue tr
        let mid = monitorOf update
        setupSlackData tr testPid "T1"
        slackChannels tr ["C1", "C2"]
        dbExec tr [sql|UPDATE monitors.query_monitors SET alert_threshold = 60, log_query = 'name == "missing-monitor-reading" | summarize max(duration)', check_interval_mins = 1 WHERE id = ?|] (Only mid)
        let check = slack . fst <$> captureNotifs tr (checkTriggeredQueryMonitors >> replicateM_ 2 runSlackIncidentDeliveries)
            threaded = length . filter (isJust . (.threadTs))
            tick = advanceMinutes tr 1
        runTestBgNoReset tr (I.recordIncidentEvent update{I.change = I.IncidentDataUnavailable}) `shouldReturn` I.NoOpenEpisode
        length . slack <$> evaluateMonitor tr mid 84 `shouldReturn` 2
        advanceMinutes tr 2
        rejectDeliveries tr
        check `shouldReturn` []
        (.lastEvaluated) <$> fetchMonitor tr mid `shouldReturn` Just frozenTime
        acceptDeliveries tr
        missing <- check
        (length missing, threaded missing) `shouldBe` (4, 2)
        for_ missing \sd -> slackText sd `shouldContainAll` ["DATA UNAVAILABLE", "Recovery is unconfirmed"]
        current <- fetchMonitor tr mid
        (current.currentStatus, current.currentValue, current.notificationCount) `shouldBe` (Monitors.MSAlerting, 84, 2)
        tick
        threaded <$> check `shouldReturn` 0
        tick
        void $ evaluateMonitor tr mid 84
        dbExec tr [sql|UPDATE monitors.query_monitors SET log_query = 'SELECT * FROM missing_monitor_table' WHERE id = ?|] (Only mid)
        tick
        failed <- check
        length failed `shouldBe` 4
        for_ failed \sd -> slackText sd `shouldSatisfy` T.isInfixOf "The evaluation failed"
        tick
        dbExec tr [sql|UPDATE monitors.query_monitors SET stop_after_count = 3 WHERE id = ?|] (Only mid)
        void $ evaluateMonitor tr mid 84
        tick
        threaded <$> check `shouldReturn` 0
        tick
        recovered <- slack <$> evaluateMonitor tr mid 18
        length recovered `shouldBe` 4
        for_ recovered \sd -> slackText sd `shouldSatisfy` T.isInfixOf "RECOVERED"
        dbOne @_ @Int64 tr [sql|SELECT count(*) FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id WHERE e.source_kind = 'monitor' AND e.source_id = ?|] (Only mid) `shouldReturn` 2

      it "keeps warning, escalation, and recovery in two monitor threads and preserves the onset snapshot" \tr -> do
        mid <- monitorOf <$> setupWithIssue tr
        setupSlackData tr testPid "T1"
        slackChannels tr ["C1", "C2", "C1"]
        dbExec tr [sql|UPDATE monitors.query_monitors SET alert_threshold = 60, warning_threshold = 40 WHERE id = ?|] (Only mid)
        let evaluate = fmap slack . evaluateMonitor tr mid
        (initialA, initialB) <- concurrently (evaluate 50) (evaluate 50)
        let initial = initialA <> initialB
        map (.threadTs) initial `shouldBe` [Nothing, Nothing]
        for_ initial \sd -> slackText sd `shouldSatisfy` T.isInfixOf "WARNING"
        advanceMinutes tr 1
        length <$> evaluate 84 `shouldReturn` 4
        advanceMinutes tr 1
        recovered <- evaluate 18
        length recovered `shouldBe` 4
        let edits = [(sd.channelId, KM.lookup "ts" obj, sd) | sd <- recovered, AE.Object obj <- [sd.payload], KM.member "ts" obj]
        sort [(cid, ts) | (cid, ts, _) <- edits] `shouldBe` sort [(sd.channelId, AE.String <$> sd.threadTs) | sd <- recovered, isJust sd.threadTs]
        for_ edits \(_, _, sd) -> slackText sd `shouldContainAll` ["RECOVERED", "Initial value: 50.0", "widgetZ=", "/issues/"]
        dbQuery tr [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid) `shouldReturn` [Only ("recovered" :: Text)]
        advanceMinutes tr 1
        map (.threadTs) <$> evaluate 84 `shouldReturn` [Nothing, Nothing]

      it "records recovery while muted and refreshes the root when delivery resumes" \tr -> do
        mid <- monitorOf <$> setupWithIssue tr
        setupSlackData tr testPid "T1"
        slackChannels tr ["C1"]
        void $ evaluateMonitor tr mid 84
        dbExec tr [sql|UPDATE monitors.query_monitors SET muted_until = ? WHERE id = ?|] (addUTCTime 180 frozenTime, mid)
        advanceMinutes tr 1
        evaluateMonitor tr mid 18 `shouldReturn` []
        dbQuery tr [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid) `shouldReturn` [Only ("recovered" :: Text)]
        advanceMinutes tr 2
        resumed <- slack . fst <$> captureNotifs tr runSlackIncidentDeliveries
        [t | AE.Object obj <- map (.payload) resumed, KM.member "ts" obj, Just (AE.String t) <- [KM.lookup "text" obj]] `shouldSatisfy` \case [t] -> "RECOVERED" `T.isInfixOf` t; _ -> False
        advanceMinutes tr 1
        length . filter (isNothing . (.threadTs)) . slack <$> evaluateMonitor tr mid 84 `shouldReturn` 1

      -- `setup` stops short of the issue row: the rollback asserts the issue is created inside the outbox transaction.
      it "an issue-less episode is refused, an outbox failure rolls back the evaluation, and one long-lived issue counts firings, not ticks" \tr -> do
        update <- setup tr
        let mid = monitorOf update
            issueRows = dbQuery @_ @(Issues.IssueId, Maybe UTCTime, Maybe UTCTime, Int64) tr [sql|SELECT id, acknowledged_until, archived_at, affected_requests FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, Monitors.queryAlertKey mid)
            fireAgain = advanceMinutes tr 1 >> void (evaluateMonitor tr mid 18) >> advanceMinutes tr 1 >> void (evaluateMonitor tr mid 84)
        setupSlackData tr testPid "T1"
        dbExec tr [sql|UPDATE projects.teams SET disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (Only testPid)
        -- An episode is only read from its issue's page, so one without an issue would be written and lost.
        runTestBgNoReset tr (I.recordIncidentEvent update) `shouldReturn` I.Rejected I.EpisodeWithoutIssue
        dbOne @_ @Int64 tr [sql|SELECT count(*) FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid) `shouldReturn` 0
        rejectDeliveries tr
        monitor <- fetchMonitor tr mid
        runTestBgNoReset tr (evaluateQueryMonitorValue monitor frozenTime 84) `shouldThrow` anyException
        unchanged <- fetchMonitor tr mid
        (unchanged.currentStatus, unchanged.notificationCount, unchanged.lastEvaluated) `shouldBe` (Monitors.MSNormal, 0, Nothing)
        runTestBgNoReset tr (Monitors.getEvaluations testPid mid frozenTime frozenTime) `shouldReturn` []
        issueRows `shouldReturn` []
        acceptDeliveries tr
        runTestBgNoReset tr $ evaluateQueryMonitorValue unchanged frozenTime 84
        (.currentStatus) <$> fetchMonitor tr mid `shouldReturn` Monitors.MSAlerting
        -- Ticks that stay alerting are not new firings, so occurrence counts do not move.
        [(firstIssue, _, _, firstCount)] <- issueRows
        replicateM_ 3 $ advanceMinutes tr 1 >> void (evaluateMonitor tr mid 84)
        map (\(_, _, _, c) -> c) <$> issueRows `shouldReturn` [firstCount]
        -- Archived, then firing again: the same row comes back rather than a second one.
        dbExec tr [sql|UPDATE apis.issues SET archived_at = ? WHERE id = ?|] (frozenTime, firstIssue)
        fireAgain
        map (\(i, _, a, _) -> (i, a)) <$> issueRows `shouldReturn` [(firstIssue, Nothing)]
        -- A firing inside a live ack window must not un-silence the issue: an ack is a snooze.
        dbExec tr [sql|UPDATE apis.issues SET acknowledged_at = ?, acknowledged_until = ? WHERE id = ?|] (frozenTime, addUTCTime 86400 frozenTime, firstIssue)
        fireAgain
        map (\(i, u, _, _) -> (i, isJust u)) <$> issueRows `shouldReturn` [(firstIssue, True)]

      it "delivers channel-specific roots, replies, and updates through the notification worker" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        void $ runTestBgNoReset tr $ I.recordIncidentEvent update
        rootNotifications <- slack . fst <$> captureNotifs tr runSlackIncidentDeliveries
        (sort (map (.channelId) rootNotifications), map (.threadTs) rootNotifications) `shouldBe` (["C1", "C2"], [Nothing, Nothing])
        void $ runTestBgNoReset tr $ I.recordIncidentEvent update{I.observedAt = addUTCTime 60 frozenTime, I.change = I.IncidentRecovered}
        advanceMinutes tr 1
        replies <- slack . fst <$> captureNotifs tr runSlackIncidentDeliveries
        let parents = sort [(sd.channelId, sd.threadTs) | sd <- replies]
        (all (isJust . snd) parents, length (ordNub $ map snd parents)) `shouldBe` (True, 2)
        edits <- slack . fst <$> captureNotifs tr runSlackIncidentDeliveries
        sort [(sd.channelId, KM.lookup "ts" obj) | sd <- edits, AE.Object obj <- [sd.payload]] `shouldBe` [(cid, AE.String <$> ts) | (cid, ts) <- parents]
        -- Message metadata correlates every post with its stored root and delivery.
        for_ ([("post_root", "monoscope_incident_root", rootNotifications), ("post_reply", "monoscope_incident_delivery", replies), ("update_root", "monoscope_incident_root", edits)] :: [(Text, Text, [Notify.SlackData])]) \(operation, eventType, messages) ->
          for_ messages \message -> do
            message.payload ^? key "metadata" . key "event_type" . _String `shouldBe` Just eventType
            Just metadata <- pure $ message.payload ^? key "metadata" . key "event_payload"
            dbQuery tr [sql|SELECT jsonb_build_object('root_id', r.id, 'delivery_id', d.id) FROM apis.slack_incident_deliveries d JOIN apis.slack_incident_roots r ON r.id = d.root_id WHERE r.channel_id = ? AND d.operation = ?|] (message.channelId, operation)
              `shouldReturn` [Only $ Aeson metadata]
        fst <$> captureNotifs tr runSlackIncidentDeliveries `shouldReturn` []

      it "deduplicates destinations and events, orders recovery behind roots, and starts a new episode on recurrence" \tr -> do
        update <- setupWithIssue tr
        let run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset tr
            recoveredAt = addUTCTime 60 update.observedAt
            recovered = update{I.observedAt = recoveredAt, I.change = I.IncidentRecovered, I.destinations = [I.SlackDestination "T1" "C3"]}
            confirmAll deliveries tss = for_ (zip deliveries tss) \(d, ts) -> run (I.finishSlackDelivery recoveredAt d (I.DeliveryConfirmed ts)) `shouldReturn` True
        I.Recorded episode event <- run $ I.recordIncidentEvent update
        run (I.recordIncidentEvent update) `shouldReturn` I.AlreadyRecorded episode.id event
        I.Recorded closed recoveryEvent <- run $ I.recordIncidentEvent recovered
        (closed.id, closed.phase, closed.closedAt) `shouldBe` (episode.id, I.EpisodeRecovered, Just recoveredAt)
        (batchA, batchB) <- concurrently (run $ I.claimSlackDeliveries recoveredAt) (run $ I.claimSlackDeliveries recoveredAt)
        let roots = sortOn (.channelId) $ batchA <> batchB
        map (\d -> (d.channelId, d.operation)) roots `shouldBe` [("C1", I.PostRoot), ("C2", I.PostRoot)]
        run (I.claimSlackDeliveries recoveredAt) `shouldReturn` []
        timestamps <- traverse timestamp ["1788900000.000001", "1788900000.000002"]
        confirmAll roots timestamps
        replies <- run $ I.claimSlackDeliveries recoveredAt
        map (.operation) replies `shouldBe` map I.PostReply timestamps
        replyTs <- timestamp "1788900060.000003"
        confirmAll replies (repeat replyTs)
        edits <- run $ I.claimSlackDeliveries recoveredAt
        map (.operation) edits `shouldBe` map I.UpdateRoot timestamps
        confirmAll edits timestamps
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
            c1 = update{I.destinations = [I.SlackDestination "T1" "C1"]}
        run (I.recordIncidentEvent update{I.projectId = UUIDId $ UUID.fromWords 0 0 0 999}) `shouldReturn` I.Rejected I.UnknownIncidentSource
        run (I.recordIncidentEvent update{I.change = I.IncidentRecovered}) `shouldReturn` I.NoOpenEpisode
        I.Recorded episode _ <- run $ I.recordIncidentEvent c1
        run (I.recordIncidentEvent update{I.observedAt = addUTCTime (-1) now, I.change = I.IncidentRecovered}) `shouldReturn` I.Rejected I.OlderThanCurrentEpisode
        [root] <- run $ I.claimSlackDeliveries now
        run (I.finishSlackDelivery now root I.WebhookAccepted) `shouldReturn` True
        void $ run $ I.recordIncidentEvent c1{I.observedAt = addUTCTime 60 now, I.change = I.IncidentRecovered}
        run (I.claimSlackDeliveries (addUTCTime 300 now)) `shouldReturn` []
        run (I.getEpisode (UUIDId $ UUID.fromWords 0 0 0 999) episode.id) `shouldReturn` Nothing

      -- Ask the bot in an alert thread and the answer belongs to that alert's issue.
      it "resolves an alert thread to its issue's conversation, and a loose thread to the thread's own" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        Just issueId <- pure update.issueId
        let run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset tr
        void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
        [root] <- run $ I.claimSlackDeliveries update.observedAt
        ts <- timestamp "1788900000.000009"
        void $ run $ I.finishSlackDelivery update.observedAt root (I.DeliveryConfirmed ts)
        run (I.threadConversationId testPid "T1" "C1" "1788900000.000009") `shouldReturn` UUIDId issueId.unUUIDId
        run (I.threadConversationId testPid "T1" "C1" "1788900000.000099") `shouldReturn` Issues.slackScopedConversationId testPid "T1" "C1" "1788900000.000099"

      it "captures only this app's signed root in the original workspace and channel" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset tr
        void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
        [webhookRoot] <- run $ I.claimSlackDeliveries update.observedAt
        run (I.finishSlackDelivery update.observedAt webhookRoot I.WebhookAccepted) `shouldReturn` True
        let event eid workspace channel ts' author = receiveSlackEvent tr $ slackRootEvent eid workspace channel webhookRoot.rootId ts' author
        for_ ([("EvOtherApp", "T1", "C1", "1788900000.000001", "A_OTHER"), ("EvOtherTeam", "T2", "C1", "1788900000.000001", "A_TEST"), ("EvOtherChannel", "T1", "C2", "1788900000.000001", "A_TEST"), ("EvBadTs", "T1", "C1", "invalid", "A_TEST")] :: [(Text, Text, Text, Text, Text)]) \(eid, workspace, channel, ts', author) ->
          event eid workspace channel ts' author >>= run . I.captureSlackRoot "A_TEST" >>= (`shouldBe` False)
        receipt <- event "EvOwnRoot" "T1" "C1" "1788900000.000001" "A_TEST"
        mapM (\appId -> run (I.captureSlackRoot appId receipt)) ["", "A_TEST", "A_TEST"] `shouldReturn` [False, True, True]
        event "EvConflict" "T1" "C1" "1788900000.000002" "A_TEST" >>= run . I.captureSlackRoot "A_TEST" >>= (`shouldBe` False)

      for_ [I.WebhookAccepted, I.DeliveryUncertain "lost_ack"] \accepted ->
        it ("recovers an incident root through leased history pages after " <> show accepted) \tr -> do
          update <- setupWithIssue tr
          setupSlackData tr testPid "T1"
          let resources = slackAppResources tr
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
              wrong = historyPage [message & key "app_id" . _String .~ "A_OTHER", AE.Object $ KM.insert "thread_ts" (AE.String "1788900000.000002") fields, message & key "metadata" . key "event_payload" . key "root_id" . _String .~ "00000000-0000-0000-0000-000000000000"] (Just "next")
              matching = historyPage [message] Nothing
          pages <- newIORef [(Nothing, wrong), (Just "next", matching), (Just "next", matching)]
          replaceInstallation <- newIORef True
          let work = reconcile tr resources pages \opts endpoint cursor -> do
                endpoint `shouldBe` "https://slack.com/api/conversations.history"
                opts ^. Wreq.param "limit" `shouldBe` ["15"]
                when (cursor == Just "next") do
                  replace <- atomicModifyIORef' replaceInstallation (False,)
                  when replace $ dbExec tr [sql|UPDATE apis.slack SET team_id = 'T_OTHER' WHERE project_id = ?|] (Only testPid)
          work `shouldReturn` []
          advanceMinutes tr 3
          work `shouldReturn` ["https://slack.com/api/conversations.history"]
          run $ I.saveIncidentSearchCursor abandoned (Just "stale") frozenTime
          dbQuery tr [sql|SELECT history_cursor FROM apis.slack_incident_deliveries WHERE id = ?|] (Only root.id) `shouldReturn` [Only (Just "next" :: Maybe Text)]
          advanceMinutes tr 1
          void work
          run (I.claimSlackDeliveries (addUTCTime 300 frozenTime)) `shouldReturn` []
          dbExec tr [sql|UPDATE apis.slack SET team_id = 'T1' WHERE project_id = ?|] (Only testPid)
          advanceMinutes tr 1
          void work
          [reply] <- run $ I.claimSlackDeliveries (addUTCTime 300 frozenTime)
          expected <- timestamp "1788900000.000001"
          reply.operation `shouldBe` I.PostReply expected
          readIORef pages `shouldReturn` []

      for_ [False, True] \lostAck ->
        it ("reconciles lifecycle replies and edits after " <> if lostAck then "lost acknowledgements" else "expired send leases") \tr -> do
          update <- setupWithIssue tr
          setupSlackData tr testPid "T1"
          let resources = slackAppResources tr
              run :: ATBackgroundCtx a -> IO a
              run = runTestBgNoReset resources
              c1 = update{I.destinations = [I.SlackDestination "T1" "C1"]}
          void $ run $ I.recordIncidentEvent c1
          [root] <- run $ I.claimSlackDeliveries frozenTime
          parent <- timestamp "1788900000.000001"
          run (I.finishSlackDelivery frozenTime root $ I.DeliveryConfirmed parent) `shouldReturn` True
          void $ run $ I.recordIncidentEvent c1{I.observedAt = addUTCTime 60 frozenTime, I.change = I.IncidentRecovered}
          advanceMinutes tr 1
          replicateM_ 2 do
            now <- getTestTime tr.trTestClock
            [delivery] <- run $ I.claimSlackDeliveries now
            when lostAck $ run (I.finishSlackDelivery now delivery $ I.DeliveryUncertain "lost_ack") `shouldReturn` True
            dbExec tr [sql|UPDATE apis.slack_incident_deliveries SET history_retry_at = ? WHERE id = ?|] (now, delivery.id)
            unless lostAck do
              length <$> run (I.claimIncidentSearches now) `shouldReturn` 0
              advanceMinutes tr 3
            [abandoned] <- getTestTime tr.trTestClock >>= run . I.claimIncidentSearches
            (method, expectedTs, expectedCursor) <- case delivery.operation of
              I.PostReply _ -> pure ("conversations.replies", "1788900000.000002", Just "next")
              I.UpdateRoot _ -> pure ("conversations.history", "1788900000.000001", Nothing)
              I.PostRoot -> fail "Expected a lifecycle delivery"
            Just metadata <- pure $ AE.toJSON (I.correlateSlackDelivery delivery delivery.payload) ^? key "metadata"
            let message = AE.object ["ts" AE..= (expectedTs :: Text), "thread_ts" AE..= I.slackTimestampText parent, "app_id" AE..= ("A_TEST" :: Text), "bot_id" AE..= ("B_TEST" :: Text), "metadata" AE..= metadata]
                wrong = message & key "metadata" . key "event_payload" . key "delivery_id" . _String .~ "00000000-0000-0000-0000-000000000000"
                wrongThread = message & key "thread_ts" . _String .~ "1788900999.000001"
                wrongApp = message & key "app_id" . _String .~ "A_OTHER"
                wrongRoot = message & key "metadata" . key "event_payload" . key "root_id" . _String .~ "00000000-0000-0000-0000-000000000000"
                endpointUrl = "https://slack.com/api/" <> method
            pages <- newIORef [(Nothing, historyPage [wrong, wrongThread, wrongApp, wrongRoot] expectedCursor), (expectedCursor, historyPage [message] Nothing)]
            let work = reconcile tr resources pages \opts endpoint _ -> do
                  toText endpoint `shouldBe` endpointUrl
                  case delivery.operation of
                    I.PostReply _ -> opts ^. Wreq.param "ts" `shouldBe` [I.slackTimestampText parent]
                    I.UpdateRoot _ -> map (\p -> opts ^. Wreq.param p) ["oldest", "latest", "inclusive"] `shouldBe` [[I.slackTimestampText parent], [I.slackTimestampText parent], ["true"]]
                    I.PostRoot -> fail "Unexpected root search"
            advanceMinutes tr 3
            work `shouldReturn` [endpointUrl]
            found <- timestamp expectedTs
            run (I.confirmIncidentSearch abandoned found) `shouldReturn` False
            getTestTime tr.trTestClock >>= run . I.claimSlackDeliveries >>= (`shouldBe` [])
            advanceMinutes tr 1
            work `shouldReturn` [endpointUrl]
            dbQuery tr [sql|SELECT state, message_ts FROM apis.slack_incident_deliveries WHERE id = ?|] (Only delivery.id) `shouldReturn` [("delivered" :: Text, Just expectedTs)]
            readIORef pages `shouldReturn` []
          getTestTime tr.trTestClock >>= run . I.claimSlackDeliveries >>= (`shouldBe` [])


t0 :: UTCTime
t0 = Unsafe.read "2025-06-01 12:00:00 UTC"


alertForm :: Alerts.AlertUpsertForm
alertForm =
  Alerts.AlertUpsertForm
    { unit = Nothing
    , title = "Test Alert"
    , message = "This is a test alert"
    , severity = "warning"
    , subject = "Test Alert"
    , query = "status_code == 200"
    , since = "1"
    , from = "1"
    , to = "1"
    , recipientEmails = ["example.com"]
    , recipientSlacks = []
    , recipientEmailAll = Just True
    , direction = Issues.Above
    , alertThreshold = 1
    , warningThreshold = Nothing
    , alertId = Just (UUID.toText UUID.nil)
    , frequency = Nothing
    , timeWindow = Nothing
    , conditionType = Just "threshold_exceeded"
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
    , environment = Just "production"
    , service = Just "checkout"
    }


jsonText :: AE.ToJSON a => a -> Text
jsonText = decodeUtf8 . AE.encode


slack :: [Notification] -> [Notify.SlackData]
slack ns = [sd | SlackNotification sd <- ns]


slackText :: Notify.SlackData -> Text
slackText sd = jsonText sd.payload


deliverySummary :: [Notification] -> [Text]
deliverySummary =
  sort . map \case
    EmailNotification d -> "email:" <> d.receiver
    SlackNotification d -> (case d.payload of AE.Object obj | KM.member "ts" obj -> "slack-update:"; _ -> "slack:") <> d.channelId
    DiscordNotification d -> "discord:" <> d.channelId
    WhatsAppNotification d -> "whatsapp:" <> Notify.to d
    PagerdutyNotification d -> "pagerduty:" <> Notify.integrationKey d <> ":" <> show (Notify.eventAction d)


-- | A due monitor (evaluated long ago) for deterministic state-machine simulation.
insertPipelineMonitor :: TestResources -> Monitors.QueryMonitorId -> Double -> Maybe Double -> Maybe Double -> IO ()
insertPipelineMonitor tr monId threshold warnThreshold alertRecovery = do
  void $ runTestBg frozenTime tr $ Monitors.queryMonitorUpsert def{Monitors.id = monId, Monitors.projectId = testPid, Monitors.logQuery = "summarize count()", Monitors.alertThreshold = threshold, Monitors.warningThreshold = warnThreshold, Monitors.checkIntervalMins = 1, Monitors.timeWindowMins = 60, Monitors.alertConfig = def{Monitors.title = "Pipeline Test Monitor"}, Monitors.alertRecoveryThreshold = alertRecovery}
  resetLastEvaluated tr monId


-- | Plus a pagerduty service on the everyone team, so every alert produces a captured notification.
insertRenotifyMonitor :: TestResources -> Monitors.QueryMonitorId -> Double -> IO ()
insertRenotifyMonitor tr monId threshold = do
  insertPipelineMonitor tr monId threshold Nothing Nothing
  dbExec tr [sql|UPDATE projects.teams SET pagerduty_services = ARRAY['test-integration-key'] WHERE project_id = ? AND handle = 'everyone'|] (Only testPid)


evalMonitorsAt :: UTCTime -> TestResources -> IO ()
evalMonitorsAt t tr = runTestBackground t tr.trATCtx checkTriggeredQueryMonitors


evalMonitorValueAt :: UTCTime -> TestResources -> Monitors.QueryMonitorId -> Double -> IO ()
evalMonitorValueAt t tr monId value = void $ evalMonitorValueWithNotifs t tr monId value


-- | Evaluate without running the Slack worker, so notifications are exactly what evaluation sent.
evalMonitorValueWithNotifs :: UTCTime -> TestResources -> Monitors.QueryMonitorId -> Double -> IO [Notification]
evalMonitorValueWithNotifs t tr monId value = do
  monitor <- fetchMonitor tr monId
  fst <$> runTestBackgroundWithNotifications t tr.trLogger tr.trATCtx (evaluateQueryMonitorValue monitor t value)


-- | Evaluate at the test clock, then drain the Slack worker.
evaluateMonitor :: TestResources -> Monitors.QueryMonitorId -> Double -> IO [Notification]
evaluateMonitor tr mid value =
  fst <$> captureNotifs tr do
    now <- Time.currentTime
    Monitors.queryMonitorById mid >>= traverse_ \monitor -> evaluateQueryMonitorValue monitor now value
    replicateM_ 3 runSlackIncidentDeliveries


resetLastEvaluated :: TestResources -> Monitors.QueryMonitorId -> IO ()
resetLastEvaluated tr monId = dbExec tr [sql|UPDATE monitors.query_monitors SET last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (Only monId)


setMonitorQuery :: TestResources -> Monitors.QueryMonitorId -> Text -> IO ()
setMonitorQuery tr monId q = dbExec tr [sql|UPDATE monitors.query_monitors SET log_query = ?, last_evaluated = '2020-01-01'::timestamptz WHERE id = ?|] (q, monId)


fetchMonitor :: TestResources -> Monitors.QueryMonitorId -> IO Monitors.QueryMonitor
fetchMonitor tr monId = runTestBgNoReset tr (Monitors.queryMonitorById monId) >>= maybe (fail "Monitor not found") pure


setMonitorRenotifyConfig :: TestResources -> Monitors.QueryMonitorId -> Maybe Int -> Maybe Int -> Int -> IO ()
setMonitorRenotifyConfig tr monId renotifyMins stopAfter notifCount =
  dbExec tr [sql|UPDATE monitors.query_monitors SET renotify_interval_mins = ?, stop_after_count = ?, notification_count = ? WHERE id = ?|] (renotifyMins, stopAfter, notifCount, monId)


setAlertLastTriggered :: TestResources -> Monitors.QueryMonitorId -> Maybe UTCTime -> IO ()
setAlertLastTriggered tr monId t = dbExec tr [sql|UPDATE monitors.query_monitors SET alert_last_triggered = ? WHERE id = ?|] (t, monId)


slackChannels :: TestResources -> [Text] -> IO ()
slackChannels tr cs = dbExec tr [sql|UPDATE projects.teams SET slack_channels = ?, disabled_channels = '{}' WHERE project_id = ? AND is_everyone|] (V.fromList cs, testPid)


-- | Make every Slack outbox write fail (or succeed again), to prove the enclosing transaction rolls back.
rejectDeliveries, acceptDeliveries :: TestResources -> IO ()
rejectDeliveries tr = dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries ADD CONSTRAINT reject_test_delivery CHECK (false) NOT VALID|] ()
acceptDeliveries tr = dbExec tr [sql|ALTER TABLE apis.slack_incident_deliveries DROP CONSTRAINT reject_test_delivery|] ()


setup :: TestResources -> IO I.IncidentUpdate
setup tr = do
  let mid = Monitors.QueryMonitorId $ UUID.fromWords 0 0 0 950
  void $ runTestBgNoReset tr $ Monitors.queryMonitorUpsert def{Monitors.id = mid, Monitors.projectId = testPid, Monitors.alertThreshold = 60, Monitors.checkIntervalMins = 1, Monitors.timeWindowMins = 1}
  payload <- maybe (fail "expected an object payload") pure $ I.slackPayload $ AE.object ["text" AE..= ("Measured threshold exceeded" :: Text)]
  pure
    I.IncidentUpdate
      { I.projectId = testPid
      , I.source = I.MonitorIncident mid
      , I.observedAt = frozenTime
      , I.change = I.IncidentAlert
      , I.delivery = I.PublishIncident
      , I.issueId = Nothing
      , I.rootPayload = payload
      , I.replyPayload = payload
      , I.destinations = [I.SlackDestination "T1" "C1", I.SlackDestination "T1" "C2", I.SlackDestination "T1" "C1"]
      }


-- | 'setup' plus the issue row 'commitQueryMonitorEvaluation' would commit: an episode cannot open without one.
setupWithIssue :: TestResources -> IO I.IncidentUpdate
setupWithIssue tr = do
  update <- setup tr
  issue <- runTestBgNoReset tr do
    created <- Issues.createQueryAlertIssue testPid (Monitors.queryAlertKey $ monitorOf update) "Measured threshold exceeded" "" 60 84 Issues.Above
    created <$ Issues.insertIssue created
  pure (update{I.issueId = Just issue.id} :: I.IncidentUpdate)


monitorOf :: I.IncidentUpdate -> Monitors.QueryMonitorId
monitorOf u = case u.source of
  I.MonitorIncident mid -> mid
  _ -> error "fixture source is a monitor"


timestamp :: Text -> IO I.SlackTimestamp
timestamp = maybe (fail "invalid fixture timestamp") pure . I.slackTimestamp


slackAppResources :: TestResources -> TestResources
slackAppResources tr = tr{trATCtx = tr.trATCtx{Config.env = tr.trATCtx.env{Config.slackAppId = "A_TEST"}}}


historyPage :: [AE.Value] -> Maybe Text -> AE.Value
historyPage messages next = AE.object ["ok" AE..= True, "messages" AE..= messages, "response_metadata" AE..= AE.object ["next_cursor" AE..= fromMaybe "" next]]


-- | Run the reconciler against queued history pages, checking the common params and the
-- cursor of each request; returns the URLs it called.
reconcile :: TestResources -> TestResources -> IORef [(Maybe Text, AE.Value)] -> (Wreq.Options -> String -> Maybe Text -> IO ()) -> IO [Text]
reconcile tr resources pages expect = do
  now <- getTestTime tr.trTestClock
  fmap (map fst . fst) $ runTestBgRecordingHTTP now resources $ withHTTPResponses
    ( \opts endpoint -> do
        opts ^. Wreq.param "channel" `shouldBe` ["C1"]
        opts ^. Wreq.param "include_all_metadata" `shouldBe` ["true"]
        (cursor, body) <- atomicModifyIORef' pages (\case [] -> ([], Nothing); item : rest -> (rest, Just item)) >>= maybe (fail "Unexpected history request") pure
        opts ^. Wreq.param "cursor" `shouldBe` maybeToList cursor
        expect opts endpoint cursor
        pure $ Just $ AE.encode body
    )
    SlackPage.reconcileIncidentDeliveries
