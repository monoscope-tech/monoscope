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
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types.URI (urlDecode)
import Network.Wreq qualified as Wreq
import Pages.BodyWrapper (PageCtx (..))
import Pages.Bots.Slack qualified as SlackPage
import Pages.Dashboards qualified as Dashboards
import Pages.Issues qualified as IssuesPage
import Pages.Monitors qualified as Alerts
import Pages.Projects qualified as ProjectPages
import Pages.Settings qualified as Settings
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as ET
import Pkg.Mail qualified as Mail
import BackgroundJobs qualified as Jobs
import Control.Exception (ErrorCall (..), bracket_)
import Data.Effectful.LLM qualified as ELLM
import Database.PostgreSQL.Simple qualified as PGS
import Effectful.Dispatch.Dynamic (interpose)
import Langchain.LLM.Core qualified as Chat
import Lucid qualified
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Investigations qualified as Investigations
import Models.Projects.ProjectMembers qualified as PM
import Pages.AIThreads qualified as AIThreads
import Pkg.AI qualified as AI
import Pkg.AgentCatalog qualified as Catalog
import UnliftIO.Exception (throwIO, tryAny)
import Web.Routes qualified as Routes
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)
import UnliftIO.Async (concurrently)


spec :: Spec
spec = describe "Monitoring" do
  it "Slack error fallbacks stay concise and keep metadata subordinate to the exception" do
    let err = def{Errors.errorType = "HttpException", Errors.message = T.replicate 50 "HttpExceptionRequest Request { host = \"api.example.com\" path = \"/oauth/token\" } ", Errors.serviceName = Just "monoscope-dev"}
        impact = Mail.AlertImpact "Monoscope" (Just "Acme Corp") (Just "Ada Lovelace") (Just "ada@example.com")
        messages trend = Mail.errorIncidentMessages Mail.NewRuntimeError err frozenTime Nothing Nothing "https://example.com" "https://example.com/issues/1" trend (Just "1/hr") impact
        (root, reply) = messages (Just "https://example.com/error-trend.png")
        withoutTrend = jsonText $ fst $ messages Nothing
    jsonText root `shouldContainAll` ["```HttpExceptionRequest", ":red_circle: *New error*  <https://example.com/issues/1|HTTP request failed · api.example.com/oauth/token>", "*monoscope-dev*  ·  *1/hr*", "Started Jan 1, 00:00 UTC  ·  User Ada Lovelace  ·  Tenant Acme Corp  ·  Project Monoscope", "https://example.com/error-trend.png", "#E5484D"]
    jsonText root `shouldNotSatisfy` T.isInfixOf err.message
    (withoutTrend, jsonText reply) `shouldSatisfy` \(w, r) -> "Chart unavailable" `T.isInfixOf` w && not ("image_url" `T.isInfixOf` w) && ":red_circle: *New error*  ·  *1/hr*" `T.isInfixOf` r
    slackPayloadViolations (AE.toJSON root) <> slackPayloadViolations (AE.toJSON reply) `shouldBe` []

  -- One snapshot of every Slack alert shape: a reviewer reads (or renders) the
  -- whole notification vocabulary in one file and sees any drift in the diff.
  it "Slack alert gallery: every alert type shares one card anatomy and one-line replies" do
    let chart = Just "https://example.com/chart.png"
        issue = "https://example.com/issues/1"
        err =
          def
            { Errors.errorType = "TypeError"
            , Errors.message = "Cannot read properties of undefined (reading 'id')"
            , Errors.stackTrace = "TypeError: Cannot read properties of undefined (reading 'id')\n    at getOrderItems (/app/src/routes/orders.ts:87:24)"
            , Errors.serviceName = Just "orders-api"
            , Errors.environment = Just "production"
            , Errors.requestMethod = Just "GET"
            , Errors.requestPath = Just "/api/v1/orders/:orderId/items"
            , Errors.traceId = Just "296e419eef5ed7da296e419eef5ed7da"
            , Errors.when = frozenTime
            }
        impact = Mail.AlertImpact "Acme API" Nothing (Just "Ada Lovelace") (Just "ada@example.com")
        episode = Just I.Episode{I.id = UUIDId UUID.nil, I.projectId = testPid, I.issueId = Nothing, I.phase = I.EpisodeActive, I.startedAt = addUTCTime (-9000) frozenTime, I.lastEventAt = frozenTime, I.closedAt = Nothing}
        errorMessages t ongoing = Mail.errorIncidentMessages t err frozenTime episode ongoing "https://example.com" issue chart (Just "38/hr") impact
        monitor = (def :: Monitors.QueryMonitor){Monitors.alertThreshold = 1200, Monitors.warningThreshold = Just 1000, Monitors.checkIntervalMins = 1, Monitors.alertConfig = def{Monitors.title = "Checkout p95 latency", Monitors.unit = Just "ms"}}
        monitorMessages v st = Mail.monitorIncidentMessages monitor{Monitors.currentStatus = Monitors.MSAlerting} v st (addUTCTime 1380 frozenTime) episode issue "https://example.com/monitors/1" chart
        (resolvedRoot, resolvedReply) = Mail.resolvedErrorMessage err "Ada Lovelace" frozenTime "https://example.com" (Just issue)
        logPattern = ET.NewLogPattern{issueUrl = issue, patternText = "payment provider timeout after <*> ms", sampleMessage = Just "payment provider timeout after 30000 ms (provider=adyen, attempt=3)", logLevel = Just "error", serviceName = Just "payments", sourceField = "body", occurrenceCount = 214, isError = True}
        endpoints = V.fromList [ET.EndpointAlertRow "POST /api/v1/refunds" (Just "api.acme.io") (Just "payments") (Just "production"), ET.EndpointAlertRow "GET /api/v1/refunds/:id" (Just "api.acme.io") (Just "payments") (Just "production")]
        spike = Mail.cardMessages $ Mail.rateChangeCard (Mail.RateChange issue "connection reset by peer <*>" (Just "warn") (Just "gateway") Issues.Spike 1920 140 1271 False) "Acme API"
        note event meta = Issues.ActivityNote{Issues.event, Issues.issue = Just "TypeError: Cannot read properties of undefined", Issues.actor = Just "Ada Lovelace", Issues.assignee = Just "Grace Hopper", Issues.metadata = Just $ AE.object meta, Issues.at = frozenTime}
        activity =
          [ note Issues.IEAcknowledged ["until" AE..= addUTCTime 3600 frozenTime]
          , note Issues.IEAssigned []
          , note Issues.IECommented ["body" AE..= ("Rolling back the orders deploy; the null check was dropped in #4821" :: Text)]
          , note Issues.IELinked ["url" AE..= ("https://github.com/acme/orders/issues/4821" :: Text), "title" AE..= ("acme/orders#4821" :: Text)]
          , note Issues.IEAckExpired []
          , note Issues.IEResolved []
          ]
        entry :: Text -> I.SlackPayload -> [I.SlackPayload] -> AE.Value
        entry label root thread = AE.object ["label" AE..= label, "payload" AE..= root, "thread" AE..= thread]
        gallery =
          [ entry "new error" (fst $ errorMessages Mail.NewRuntimeError Nothing) [snd $ errorMessages Mail.EscalatingErrors Nothing, snd $ errorMessages Mail.NewRuntimeError (Just "3 hours"), resolvedReply]
          , entry "issue lifecycle in the thread" (fst $ errorMessages Mail.NewRuntimeError Nothing) (Mail.activityReply <$> activity)
          , entry "root while someone holds it" (Mail.withOwner (Just $ Issues.Ownership (Just "Ada Lovelace") (Just $ addUTCTime 3600 frozenTime) (Just "Grace Hopper")) $ fst $ errorMessages Mail.NewRuntimeError Nothing) []
          , entry "error root after resolve" (Mail.retainSlackSnapshot (fst $ errorMessages Mail.NewRuntimeError Nothing) resolvedRoot) []
          , entry "regressed error" (fst $ errorMessages Mail.RegressedErrors Nothing) []
          , entry "monitor alerting" (fst $ monitorMessages 1840 Monitors.MSAlerting) [snd $ monitorMessages 1050 Monitors.MSWarning, snd $ Mail.monitorDataUnavailableMessage monitor Monitors.NoMeasurements frozenTime (Just (frozenTime, 1840)) issue "", snd $ monitorMessages 640 Monitors.MSNormal]
          , entry "monitor root after recovery" (Mail.retainSlackSnapshot (fst $ monitorMessages 1840 Monitors.MSAlerting) (fst $ monitorMessages 640 Monitors.MSNormal)) []
          , entry "new log pattern" (Mail.card $ Mail.logPatternCard logPattern "Acme API") []
          , entry "log spike" (fst spike) [snd spike]
          , entry "new endpoints" (Mail.slackNewEndpointsAlert "Acme API" endpoints issue "https://example.com") []
          , entry "weekly report" (Mail.slackReportAlert Projects.RTWeekly "2025-01-01T00:00:00Z" "2025-01-08T00:00:00Z" 42 125000 (V.fromList [("orders-api", 30, 90000), ("payments", 12, 35000)]) "Acme API" "https://example.com/reports/1" chart chart) []
          ]
    goldenText "slack/alert-gallery.json" $ decodeUtf8 $ AE.encode gallery
    concatMap slackPayloadViolations gallery `shouldBe` []

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
      void $ testServant tr $ Dashboards.dashboardsPostH testPid Dashboards.DashboardForm{Dashboards.title = "Widget Monitor Lifecycle", Dashboards.file = "widget-monitor.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing, Dashboards.copyWidgetId = Nothing, Dashboards.sourceDashboardId = Nothing, Dashboards.widgetJson = Nothing}
      dashboardId <-
        call tr (Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing (Dashboards.DashboardFilters [])) >>= \case
          Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> maybe (fail "the dashboard was not saved") (pure . (.id)) $ V.find ((== "Widget Monitor Lifecycle") . (.title)) dashboards
          _ -> fail "the dashboard list did not load"
      let otherPid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
      isNothing <$> runTestBgNoReset tr (DashboardModel.getDashboardByProjectId otherPid dashboardId) `shouldReturn` True
      savedWidget <- (.widget) . fromRightShow <$> call tr (Dashboards.dashboardWidgetPutH testPid dashboardId Nothing Nothing def{Widget.wType = Widget.WTTimeseries, Widget.title = Just "Checkout errors", Widget.query = Just "name == \"checkout\""})
      widgetId <- maybe (fail "the saved widget has no id") pure savedWidget.id
      let widgetMonitor pid = runTestBgNoReset tr $ Monitors.queryMonitorByWidgetId pid (Just dashboardId) widgetId
          widgetUrl = "/p/" <> testPid.toText <> "/dashboards/" <> dashboardId.toText <> "?expand=" <> widgetId
          -- A one-line thread reply links nothing; its root carries the links.
          linksTo url = \case
            SlackNotification sd | isJust sd.threadTs -> pass
            notification -> unless (url `T.isInfixOf` jsonText notification) $ expectationFailure $ "Missing widget link in " <> show (deliverySummary [notification])
          putWidget w = (void <$> call tr (Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing w)) `shouldReturn` Right ()
          upsertAlert = call tr . Dashboards.widgetAlertUpsertH testPid widgetId (Just dashboardId)
          check = advanceMinutes tr 1 >> deliverySummary . fst <$> captureNotifs tr checkTriggeredQueryMonitors
          invalidQuery :: Either Dashboards.WidgetMonitorRejection a -> Bool
          invalidQuery = \case Left (Dashboards.MonitorQueryInvalid _) -> True; _ -> False
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
      upsertAlert alertForm'{Dashboards.query = ""} `shouldReturn` Left Dashboards.RawSqlWidget
      upsertAlert alertForm'{Dashboards.alertThreshold = Nothing} `shouldReturn` Left Dashboards.MissingThreshold
      -- A refused monitor form used to have already written its threshold onto the widget.
      let storedThreshold = runTestBgNoReset tr (DashboardModel.getDashboardByProjectId testPid dashboardId) <&> \d -> d >>= (.schema) >>= find ((== Just widgetId) . (.id)) . DashboardModel.allWidgets >>= (.alertThreshold)
      thresholdBefore <- storedThreshold
      upsertAlert alertForm'{Dashboards.query = "name ==", Dashboards.alertThreshold = Just "7"} >>= (`shouldSatisfy` invalidQuery)
      storedThreshold `shouldReturn` thresholdBefore
      isNothing <$> widgetMonitor testPid `shouldReturn` True
      upsertAlert alertForm' `shouldReturn` Right Dashboards.WidgetMonitorSaved
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
      upsertAlert alertForm' `shouldReturn` Right Dashboards.WidgetMonitorSaved
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

      (void <$> call tr (Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing savedWidget{Widget.alertThreshold = Nothing})) `shouldReturn` Left Dashboards.MonitorNeedsThreshold
      putWidget savedWidget{Widget.query = Just "name == \"search\"", Widget.alertThreshold = Just 2}
      fmap (\m -> (m.logQuery, m.alertThreshold)) <$> widgetMonitor testPid `shouldReturn` Just ("name == \"search\"", 2)
      -- A query that doesn't parse used to store the new KQL beside the old SQL, which kept alerting.
      stored <- fmap (\m -> (m.logQuery, m.logQueryAsSql)) <$> widgetMonitor testPid
      call tr (Dashboards.dashboardWidgetPutH testPid dashboardId (Just widgetId) Nothing savedWidget{Widget.query = Just "name ==", Widget.alertThreshold = Just 2}) >>= (`shouldSatisfy` invalidQuery)
      fmap (\m -> (m.logQuery, m.logQueryAsSql)) <$> widgetMonitor testPid `shouldReturn` stored
      advanceMinutes tr 1
      recovered <- fst <$> captureNotifs tr checkTriggeredQueryMonitors
      deliverySummary recovered `shouldBe` ["discord:C_MONITOR", "email:alerts@example.com", "pagerduty:widget-monitor-test:PDResolve", "slack-update:C_NOTIF_CHANNEL", "slack:C_NOTIF_CHANNEL", "whatsapp:+15550001111"]
      mapM_ (linksTo widgetUrl) recovered
      -- Project-level channel toggles gate delivery.
      void $ call tr $ ProjectPages.updateNotificationsChannel testPid $ ProjectPages.NotifListForm ["email", "discord", "pagerduty"] [] ["alerts@example.com"] [] Nothing
      -- Past the flap window, so the re-alert fans out like a fresh one.
      advanceMinutes tr 60
      putWidget savedWidget{Widget.alertThreshold = Just 1, Widget.unit = Just "events"}
      check `shouldReturn` ["discord:C_MONITOR", "email:alerts@example.com", "pagerduty:widget-monitor-test:PDTrigger"]
      void $ call tr $ ProjectPages.updateNotificationsChannel testPid $ ProjectPages.NotifListForm ["email", "slack", "discord", "phone", "pagerduty"] ["+15550001111"] ["alerts@example.com"] [] Nothing

      -- A copied widget gets its own monitor with fresh state, titled and linked for its dashboard.
      (.notificationCount) <$> (widgetMonitor testPid >>= maybe (fail "the source widget has no monitor") pure) `shouldReturn` 1
      let copyDashboardId = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef1
      sourceDashboardVM <- maybe (fail "the source dashboard disappeared") pure savedDash
      void $ runTestBgNoReset tr $ DashboardModel.insert sourceDashboardVM{DashboardModel.id = copyDashboardId, DashboardModel.title = "Widget Monitor Copy", DashboardModel.schema = Just (def :: DashboardModel.Dashboard){DashboardModel.title = Just "Widget Monitor Copy"}, DashboardModel.filePath = Nothing}
      copyWidget <- call tr $ Dashboards.dashboardDuplicateWidgetPostH testPid copyDashboardId widgetId (Just dashboardId)
      copyWidgetId <- maybe (fail "the copied widget has no id") pure copyWidget.id
      copyMonitor <- runTestBgNoReset tr (Monitors.queryMonitorByWidgetId testPid (Just copyDashboardId) copyWidgetId) >>= maybe (fail "the copied widget has no monitor") pure
      (copyMonitor.notificationCount, copyMonitor.currentValue, copyMonitor.alertConfig.title) `shouldBe` (0, 0, "Checkout errors · Widget Monitor Copy")
      copyAt <- addUTCTime 60 <$> getTestTime tr.trTestClock
      copyNotifs <- fst <$> captureNotifs tr (evaluateQueryMonitorValue copyMonitor copyAt 1)
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
            rows <- dbQuery @_ @(Issues.IssueId, Text, Aeson Issues.QueryAlertData) tr [sql| SELECT id, title, issue_data FROM apis.issues WHERE project_id = ? AND target_hash = ? AND issue_type = 'query_alert' AND acknowledged_at IS NULL AND archived_at IS NULL |] (testPid, hyst.toText)
            [(title, d.thresholdValue, d.actualValue) | (_, title, Aeson d) <- rows] `shouldBe` [("Pipeline Test Monitor", threshold, value)]
            let emails = [email.htmlBody | EmailNotification email <- notifications]
            emails `shouldSatisfy` (not . null)
            for_ rows \(iid, _, _) -> for_ emails (`shouldSatisfy` T.isInfixOf ("/issues/" <> iid.toText))
          statusAfter dt v = evalMonitorValueAt (addUTCTime dt t0) tr hyst v >> (.currentStatus) <$> fetchMonitor tr hyst
      evalMonitorValueWithNotifs t0 tr hyst 90 >>= assertIssue 80 90
      (.currentStatus) <$> fetchMonitor tr hyst `shouldReturn` Monitors.MSWarning
      dbExec tr [sql|UPDATE apis.issues SET title = 'Pipeline Test Monitor threshold above 80' WHERE project_id = ? AND target_hash = ?|] (testPid, hyst.toText)
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

    it "uptime checks and cron monitors: failures open downtime/missed/failed issues, recovery resolves them" \tr -> do
      pid <- createTestProject tr "uptime-cron"
      -- An accepted submission re-renders the form blank above the list (htmx 4 needs the explicit :inherited targets).
      (void <$> call tr (Alerts.uptimeCheckPostH pid (Alerts.UptimeForm "internal" "http://10.0.0.1/health" (Just 60) (Just 200)))) `shouldReturn` Left Alerts.UnsafeUrl
      added <- fromRightShow <$> call tr (Alerts.uptimeCheckPostH pid (Alerts.UptimeForm "Shop" "https://shop.example.com/health" (Just 60) (Just 200)))
      renderT added `shouldContainAll` ["https://shop.example.com/health"]
      cronAdded <- fromRightShow <$> call tr (Alerts.cronMonitorPostH pid (Alerts.CronForm "nightly-billing" "Nightly billing" 3600 300))
      goldenParts "uptime-cron-forms" . ordNub $ tagsWith ["value name=\"url\"", "hx-target:inherited=\"this\""] (renderT added) <> tagsWith ["value name=\"slug\"", "hx-target:inherited=\"this\""] (renderT cronAdded)

      void $ runQueryEffect tr $ ProjectMembers.createEveryoneTeam pid (Servant.getResponse tr.trSessAndHeader).user.id
      setupSlackData tr pid "T_UPTIME"
      dbExec tr [sql|UPDATE projects.projects SET error_alerts = TRUE WHERE id = ?|] (Only pid)
      -- One failure is a blip; two open a downtime issue; a success resolves it.
      let uptimeNamed n = maybe (fail $ "no uptime check " <> toString n) pure . find ((== n) . (.name)) =<< runQueryEffect tr (PromCfg.configsByProjectId pid PromCfg.CKUptime)
      shop <- uptimeNamed "Shop"
      let probe res = runTestBg frozenTime tr $ Jobs.applyUptimeResult shop frozenTime 120 res
          downtime = runQueryEffect tr (Issues.selectIssueByHash pid (Issues.uptimeTargetHash shop.id.toText) Issues.AnyIssue)
      probe (Right 503)
      (isNothing <$> downtime) `shouldReturn` True
      probe (Left "connection refused")
      opened <- downtime >>= maybe (fail "no downtime issue after two failures") pure
      (opened.title, isNothing opened.archivedAt) `shouldBe` ("Downtime detected for https://shop.example.com/health", True)
      probe (Right 200)
      resolved <- downtime >>= maybe (fail "issue vanished") pure
      isJust resolved.archivedAt `shouldBe` True
      runTestBg frozenTime tr (Issues.selectLatestStateEvent resolved.id) `shouldReturn` Just Issues.IEResolved
      -- Downtime posts one Slack card; recovery replies in its thread and edits the card, once.
      dbQuery @_ @(Only Text) tr [sql|SELECT d.operation FROM apis.slack_incident_deliveries d JOIN apis.incident_episodes e ON e.id = d.episode_id WHERE e.issue_id = ? ORDER BY d.sequence|] (Only opened.id)
        `shouldReturn` map Only ["post_root", "post_reply", "update_root"]
      -- Through the worker, a refused connection is recorded as a failure.
      void $ runQueryEffect tr $ PromCfg.insertConfig pid PromCfg.CKUptime "refused" "http://127.0.0.1:1/health" 60 Nothing (AE.object []) (Just 200)
      refused <- uptimeNamed "refused"
      runTestBg frozenTime tr $ Jobs.processBackgroundJob tr.trATCtx (Jobs.PrometheusScrapeOne refused.id)
      Just probed <- runQueryEffect tr (PromCfg.getConfig refused.id)
      (probed.consecutiveFailures, fmap ("request failed" `T.isPrefixOf`) probed.lastStatus) `shouldBe` (1, Just True)

      -- Cron: a duplicate slug is refused; a check-in keeps it healthy, a missed window and an error
      -- run open issues, a later check-in resolves. Check-ins go through ingestion (TimeFusion on
      -- CI), so each evaluation retries until the check-in it depends on is visible.
      (void <$> call tr (Alerts.cronMonitorPostH pid (Alerts.CronForm "nightly-billing" "Again" 3600 300))) `shouldReturn` Left (Alerts.DuplicateSlug "nightly-billing")
      [m] <- runQueryEffect tr (Monitors.cronMonitorsByProject pid)
      apiKey <- createTestAPIKey tr pid "cron-key"
      let at dt = addUTCTime dt frozenTime
          checkin status dt = ingestSessionEvent tr apiKey "cron.checkin" [("monitor.slug", "nightly-billing"), ("monitor.status", status)] False (at dt)
          evaluateAt dt = do
            fresh <- runQueryEffect tr (Monitors.cronMonitorsByProject pid) >>= maybe (fail "monitor gone") pure . listToMaybe
            runTestBg (at dt) tr $ Jobs.evaluateCronMonitor (at dt) fresh
          issue sc = runQueryEffect tr (Issues.selectIssueByHash pid (Issues.cronTargetHash (UUID.toText m.id)) sc)
          evaluateUntil dt sc ok = eventually (evaluateAt dt >> issue sc) ok
      checkin "ok" 0
      void $ eventually (evaluateAt 60 >> (.lastCheckinAt) <<$>> runQueryEffect tr (Monitors.cronMonitorsByProject pid)) (== [Just (at 0)])
      (isNothing <$> issue Issues.AnyIssue) `shouldReturn` True
      missed <- evaluateUntil (3600 + 300 + 120) Issues.AnyIssue isJust >>= maybe (fail "no missed issue") pure
      (missed.title, isNothing missed.archivedAt) `shouldBe` ("Cron missed: Nightly billing", True)
      checkin "ok" 4200
      (fmap (isJust . (.archivedAt)) <$> evaluateUntil 4260 Issues.AnyIssue (any (isJust . (.archivedAt)))) `shouldReturn` Just True
      checkin "error" 8000
      failed <- evaluateUntil 8060 (Issues.OpenOfType Issues.Cron) isJust >>= maybe (fail "no failed issue") pure
      failed.title `shouldBe` "Cron failed: Nightly billing"

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
      -- Re-alerting within the hour of a recovery is a flap: neither it nor its recovery emails anyone.
      evalMonitorValueWithNotifs (addUTCTime 120 t0) tr free 150 `shouldReturn` []
      evalMonitorValueWithNotifs (addUTCTime 180 t0) tr free 50 `shouldReturn` []
      emailsTo <$> evalMonitorValueWithNotifs (addUTCTime 3800 t0) tr free 150 >>= (`shouldMatchList` [ownerEmail, "nonowner@example.com"])

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
      compressed <- either (fail . toString) pure $ B64URL.decodeBase64Untyped $ urlDecode True $ encodeUtf8 $ T.takeWhile (/= '&') suffix
      widget <- either fail pure $ AE.eitherDecode @Widget.Widget $ GZip.decompress $ fromStrict compressed
      (widget.query, widget.unit, widget.alertThreshold, widget.pngProfile, Widget.mapWidgetTypeToChartType widget.wType) `shouldBe` (Nothing, Just "s", Just 60, Just Widget.PngSlack, "line")
      Widget.pngExportSize widget.pngProfile `shouldBe` (960, 280)
      dataset <- maybe (fail "monitor snapshot had no dataset") pure widget.dataset
      (dataset.from, dataset.to) `shouldBe` (Just $ millis $ addUTCTime (-900) t0, Just $ millis t0)
      dataset.source `shouldBe` AE.toJSON @[AE.Value] [AE.toJSON (["timestamp", "Monitor value"] :: [Text]), row earlier (Just (84 :: Double)), row (addUTCTime 60 earlier) (Nothing :: Maybe Double), row t0 (Just (100 :: Double))]
      configured <- fetchMonitor tr snapshot
      let (root, _) = Mail.monitorIncidentMessages configured 100 Monitors.MSAlerting t0 Nothing "https://example.com/incident" "https://example.com/monitor" Nothing
      jsonText root `shouldContainAll` ["*100 s*  ·  above 60 s", "Chart unavailable"]
      jsonText (snd $ Mail.monitorDataUnavailableMessage configured Monitors.NoMeasurements t0 (Just (earlier, 84)) "https://example.com/incident" "https://example.com/monitor") `shouldSatisfy` T.isInfixOf "last value 84 s"

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
        void $ call tr $ IssuesPage.resolveErrorPostH testPid notified.id.unErrorPatternId False
        closed <- slack . fst <$> captureNotifs tr (replicateM_ 2 runSlackIncidentDeliveries)
        length closed `shouldBe` 4
        -- The root says it is resolved; the reply says by whom.
        for_ closed \sd -> slackText sd `shouldSatisfy` T.isInfixOf (bool "*Resolved*" "*Resolved by " (isJust sd.threadTs))

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
            resolve = advanceMinutes tr 1 >> void (call tr $ IssuesPage.resolveErrorPostH testPid err.id.unErrorPatternId False)
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
        [issue.id.toText `T.isInfixOf` slackText sd | sd <- resolution, isNothing sd.threadTs] `shouldBe` [True, True]
        for_ resolution \sd -> slackText sd `shouldNotSatisfy` T.isInfixOf nextIssue.id.toText
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
        void $ captureNotifs tr $ replicateM_ 2 runSlackIncidentDeliveries
        dbExec tr [sql|DELETE FROM apis.issues WHERE id = ?|] (Only nextIssue.id)
        resolve
        fmap (.phase) <$> runTestBgNoReset tr (I.getEpisode testPid recurring.id) `shouldReturn` Just I.EpisodeResolved
        orphaned <- slack . fst <$> captureNotifs tr (replicateM_ 2 runSlackIncidentDeliveries)
        length orphaned `shouldBe` 4
        -- With its issue gone, the root links the project's issue list instead.
        ["/issues|" `T.isInfixOf` slackText sd | sd <- orphaned, isNothing sd.threadTs] `shouldBe` [True, True]
        for_ orphaned \sd -> slackText sd `shouldNotSatisfy` T.isInfixOf nextIssue.id.toText

      it "relays issue lifecycle actions as replies in each channel's thread and never as channel posts" \tr -> do
        mid <- monitorOf <$> setupWithIssue tr
        setupSlackData tr testPid "T1"
        slackChannels tr ["C1", "C2"]
        map (.threadTs) . slack <$> evaluateMonitor tr mid 84 `shouldReturn` [Nothing, Nothing]
        iid <- dbOne @_ @Issues.IssueId tr [sql|SELECT issue_id FROM apis.incident_episodes WHERE source_id = ?|] (Only mid)
        rootTs <- dbQuery @_ @(Text, Text) tr [sql|SELECT channel_id, message_ts FROM apis.slack_incident_roots ORDER BY channel_id|] ()
        let actor = (Servant.getResponse tr.trSessAndHeader).user
            who = "*" <> Mail.personName actor <> "*"
            -- A root sends one delivery per tick, in order: three replies and two root edits.
            deliver = slack . fst <$> captureNotifs tr (replicateM_ 6 runSlackIncidentDeliveries)
            stranded = dbQuery @_ @(Text, Text) tr [sql|SELECT operation, state FROM apis.slack_incident_deliveries WHERE state NOT IN ('delivered', 'failed')|] ()
        void $ call tr $ IssuesPage.acknowledgeIssueGetH testPid True iid (Just 60)
        void $ call tr $ IssuesPage.triagePostH testPid iid (IssuesPage.TriageForm Nothing (Just actor.id.toText))
        void $ call tr $ IssuesPage.commentPostH testPid iid (IssuesPage.CommentForm "rolling back <the> deploy")
        delivered <- deliver
        let lifecycle = filter (isJust . (.threadTs)) delivered
            rootEdits = [slackText sd | sd <- delivered, sd.payload ^? key "metadata" . key "event_type" . _String == Just "monoscope_incident_root", sd.channelId == "C1"]
        sort [(sd.channelId, sd.threadTs) | sd <- lifecycle] `shouldBe` sort [(c, Just ts) | (c, ts) <- rootTs, _ <- [1 .. 3 :: Int]]
        -- Acknowledging and assigning also edit the root, so the channel sees who holds it.
        rootEdits `shouldSatisfy` \case [_, assigned] -> all (`T.isInfixOf` assigned) [":eyes: Acknowledged by " <> who, ":bust_in_silhouette: Assigned to " <> who]; _ -> False
        [slackText sd | sd <- lifecycle, sd.channelId == "C1"]
          `shouldSatisfy` \case
            [acked, took, commented] ->
              all (uncurry T.isInfixOf) [(":eyes: " <> who <> " acknowledged this  ·  alerts paused until ", acked), (who <> " took this", took), ("> rolling back &lt;the&gt; deploy", commented)]
            _ -> False
        concatMap (slackPayloadViolations . (.payload)) lifecycle `shouldBe` []
        stranded `shouldReturn` []
        -- A webhook-only root has no thread; a lifecycle note there would land in the channel.
        dbExec tr [sql|UPDATE apis.slack_incident_roots SET message_ts = NULL, threadless = TRUE WHERE channel_id = 'C2'|] ()
        void $ call tr $ IssuesPage.acknowledgeIssueGetH testPid False iid Nothing
        sort . map (\sd -> (sd.channelId, isJust sd.threadTs)) <$> deliver `shouldReturn` [("C1", False), ("C1", True)]
        stranded `shouldReturn` []

      it "records manual error resolution once and updates both incident threads without claiming recovery" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let sample = def{Errors.when = frozenTime, Errors.hash = "manual-resolution", Errors.errorType = "CheckoutError", Errors.message = "payment declined"}
            actor = (Servant.getResponse tr.trSessAndHeader).user
            resolvePost eid = void <$> call tr (IssuesPage.resolveErrorPostH testPid eid False)
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
        resolvePost err.id.unErrorPatternId `shouldReturn` Left IssuesPage.ResolveDenied
        (.state) <$> (runTestBgNoReset tr (Errors.getErrorPatternById err.id) >>= maybe (fail "error missing") pure) `shouldReturn` Errors.ESNew
        let resolve pid at = runTestBgNoReset tr $ I.resolveErrorIncident pid err.id actor.id at (const (update.rootPayload, update.replyPayload))
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
        for_ messages \sd -> slackText sd `shouldContainAll` [bool "*Resolved*" "*Resolved by " (isJust sd.threadTs), actor.firstName]
        -- A person resolved it: the root says so and does not claim a measured recovery.
        for_ [sd | sd <- messages, isNothing sd.threadTs] \sd -> (slackText sd `shouldContainAll` [":white_check_mark: *Resolved*", issue.id.toText]) >> (slackText sd `shouldNotSatisfy` T.isInfixOf "Recovered")
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
        for_ missing \sd -> slackText sd `shouldContainAll` ["*No data*", "evaluation"]
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
        for_ failed \sd -> slackText sd `shouldSatisfy` T.isInfixOf "evaluation failed"
        tick
        dbExec tr [sql|UPDATE monitors.query_monitors SET stop_after_count = 3 WHERE id = ?|] (Only mid)
        void $ evaluateMonitor tr mid 84
        tick
        threaded <$> check `shouldReturn` 0
        tick
        recovered <- slack <$> evaluateMonitor tr mid 18
        length recovered `shouldBe` 4
        for_ recovered \sd -> slackText sd `shouldSatisfy` T.isInfixOf "*Recovered*"
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
        for_ initial \sd -> slackText sd `shouldSatisfy` T.isInfixOf "*Warning*"
        advanceMinutes tr 1
        length <$> evaluate 84 `shouldReturn` 4
        advanceMinutes tr 1
        recovered <- evaluate 18
        length recovered `shouldBe` 4
        let edits = [(sd.channelId, KM.lookup "ts" obj, sd) | sd <- recovered, AE.Object obj <- [sd.payload], KM.member "ts" obj]
        sort [(cid, ts) | (cid, ts, _) <- edits] `shouldBe` sort [(sd.channelId, AE.String <$> sd.threadTs) | sd <- recovered, isJust sd.threadTs]
        for_ edits \(_, _, sd) -> slackText sd `shouldContainAll` [":large_green_circle: *Recovered*", "*18*  ·  below 60  ·  lasted 2 min", "Started Jan 1, 00:00 UTC", "widgetZ=", "/issues/"]
        dbQuery tr [sql|SELECT phase FROM apis.incident_episodes WHERE project_id = ? AND source_id = ?|] (testPid, mid) `shouldReturn` [Only ("recovered" :: Text)]
        -- Re-alerting continues each channel's thread instead of posting a new root;
        -- the first message of a new day in that thread is also sent to the channel.
        let replies ns = [(sd.channelId, AE.String <$> sd.threadTs, sd.payload ^? key "reply_broadcast") | sd <- ns, isJust sd.threadTs]
            roots ns = [sd | sd <- ns, isNothing sd.threadTs, AE.Object obj <- [sd.payload], not (KM.member "ts" obj)]
        advanceMinutes tr 1
        continued <- evaluate 84
        (sort (replies continued), roots continued) `shouldBe` (sort [(cid, ts, Nothing) | (cid, ts, _) <- edits], [])
        advanceMinutes tr 1
        void $ evaluate 18
        advanceMinutes tr (24 * 60)
        nextDay <- evaluate 84
        (sort (replies nextDay), roots nextDay) `shouldBe` (sort [(cid, ts, Just (AE.Bool True)) | (cid, ts, _) <- edits], [])

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
        [t | AE.Object obj <- map (.payload) resumed, KM.member "ts" obj, Just (AE.String t) <- [KM.lookup "text" obj]] `shouldSatisfy` \case [t] -> "Recovered" `T.isInfixOf` t; _ -> False
        advanceMinutes tr 1
        length . filter (isJust . (.threadTs)) . slack <$> evaluateMonitor tr mid 84 `shouldReturn` 1

      -- `setup` stops short of the issue row: the rollback asserts the issue is created inside the outbox transaction.
      it "an issue-less episode is refused, an outbox failure rolls back the evaluation, and one long-lived issue counts firings, not ticks" \tr -> do
        update <- setup tr
        let mid = monitorOf update
            issueRows = dbQuery @_ @(Issues.IssueId, Maybe UTCTime, Maybe UTCTime, Int64) tr [sql|SELECT id, acknowledged_until, archived_at, affected_requests FROM apis.issues WHERE project_id = ? AND target_hash = ?|] (testPid, mid.toText)
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
        run (I.finishSlackDelivery now root (I.WebhookAccepted I.ThreadCapable)) `shouldReturn` True
        void $ run $ I.recordIncidentEvent c1{I.observedAt = addUTCTime 60 now, I.change = I.IncidentRecovered}
        run (I.claimSlackDeliveries (addUTCTime 300 now)) `shouldReturn` []
        run (I.getEpisode (UUIDId $ UUID.fromWords 0 0 0 999) episode.id) `shouldReturn` Nothing

      -- Ask the bot in an alert thread and the answer belongs to that alert's issue.
      it "resolves an alert thread to its issue's conversation, and a loose thread to the thread's own" \tr -> do
        update <- setupWithIssue tr
        setupLinkedSlackData tr testPid "T1"
        Just issueId <- pure update.issueId
        let run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset tr
        void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
        [root] <- run $ I.claimSlackDeliveries update.observedAt
        ts <- timestamp "1788900000.000009"
        void $ run $ I.finishSlackDelivery update.observedAt root (I.DeliveryConfirmed ts)
        run (I.threadConversationId testPid "T1" "C1" "1788900000.000009") `shouldReturn` UUIDId issueId.unUUIDId
        run (I.threadConversationId testPid "T1" "C1" "1788900000.000099") `shouldReturn` Issues.slackScopedConversationId testPid "T1" "C1" "1788900000.000099"

        let conversation = UUIDId issueId.unUUIDId
        principal <- runQueryEffect tr (Integrations.resolveSlackPrincipal "T1" "U0123ABCDEF" $ Just testPid) >>= maybe (fail "Missing linked principal") pure
        void $ run $ Integrations.bindSlackInvestigation principal "T1" "C1" "1788900000.000009"
        void $ run $ Issues.getOrCreateConversation testPid conversation Issues.CTAnomaly (AE.object [])
        void $ receiveSlackEvent tr $ slackCallbackEnvelope "T1" "EvIncidentTitle" $ AE.object ["type" AE..= ("agent_session_title_changed" :: Text), "team_id" AE..= ("T1" :: Text), "channel" AE..= ("C1" :: Text), "thread_ts" AE..= ("1788900000.000009" :: Text), "user" AE..= ("U0123ABCDEF" :: Text), "event_ts" AE..= ("1788900001.000009" :: Text), "title" AE..= ("Operator incident title" :: Text)]
        runQueryEffect tr (Issues.getConversation testPid conversation) >>= (\thread -> fmap (.title) thread `shouldBe` Just "Operator incident title")
        runQueryEffect tr (Issues.getConversation testPid $ Issues.slackScopedConversationId testPid "T1" "C1" "1788900000.000009") >>= (`shouldSatisfy` isNothing)

      it "captures only this app's signed root in the original workspace and channel" \tr -> do
        update <- setupWithIssue tr
        setupSlackData tr testPid "T1"
        let run :: ATBackgroundCtx a -> IO a
            run = runTestBgNoReset tr
        void $ run $ I.recordIncidentEvent update{I.destinations = [I.SlackDestination "T1" "C1"]}
        [webhookRoot] <- run $ I.claimSlackDeliveries update.observedAt
        run (I.finishSlackDelivery update.observedAt webhookRoot (I.WebhookAccepted I.ThreadCapable)) `shouldReturn` True
        let event eid workspace channel ts' author = receiveSlackEvent tr $ slackRootEvent eid workspace channel webhookRoot.rootId ts' author
        for_ ([("EvOtherApp", "T1", "C1", "1788900000.000001", "A_OTHER"), ("EvOtherTeam", "T2", "C1", "1788900000.000001", "A_TEST"), ("EvOtherChannel", "T1", "C2", "1788900000.000001", "A_TEST"), ("EvBadTs", "T1", "C1", "invalid", "A_TEST")] :: [(Text, Text, Text, Text, Text)]) \(eid, workspace, channel, ts', author) ->
          event eid workspace channel ts' author >>= run . I.captureSlackRoot "A_TEST" >>= (`shouldBe` False)
        receipt <- event "EvOwnRoot" "T1" "C1" "1788900000.000001" "A_TEST"
        mapM (\appId -> run (I.captureSlackRoot appId receipt)) ["", "A_TEST", "A_TEST"] `shouldReturn` [False, True, True]
        event "EvConflict" "T1" "C1" "1788900000.000002" "A_TEST" >>= run . I.captureSlackRoot "A_TEST" >>= (`shouldBe` False)

      for_ [(I.WebhookAccepted I.ThreadCapable), I.DeliveryUncertain "lost_ack"] \accepted ->
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
          run $ I.saveIncidentSearchCursor abandoned (I.SearchAdvanced $ Just "stale") frozenTime
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
              I.PostStandalone -> fail "Expected a lifecycle delivery"
              I.PostAnalysis{} -> fail "Expected an incident fact delivery"
              I.PostActivity{} -> fail "Expected an incident fact delivery"
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
                    I.PostStandalone -> fail "Unexpected root search"
                    I.PostAnalysis{} -> fail "Expected an incident fact delivery"
                    I.PostActivity{} -> fail "Expected an incident fact delivery"
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


  around withTestResources $ describe "Autonomous incident routines" do
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
        channel <- either (fail . show) pure $ Issues.incidentChannel "T1" "C1"
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
        channel <- either (fail . show) pure $ Issues.incidentChannel "T1" "C1"
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
        channel <- either (fail . show) pure $ Issues.incidentChannel "T1" "C1"
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
      channel <- either (fail . show) pure $ Issues.incidentChannel "T1" "C1"
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
    created <- Issues.createQueryAlertIssue testPid (monitorOf update).toText "Measured threshold exceeded" "" 60 84 Issues.Above
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
