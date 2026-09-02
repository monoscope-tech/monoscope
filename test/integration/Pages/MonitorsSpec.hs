module Pages.MonitorsSpec (spec) where

import Models.Apis.Monitors
import Test.Hspec

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid qualified
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils

import Pages.Monitors qualified as Alerts
import Pages.Projects qualified as Projects
import Relude
import Relude.Unsafe qualified as Unsafe


alertId :: UUID.UUID
alertId = UUID.nil


alertForm :: Alerts.AlertUpsertForm
alertForm =
  Alerts.AlertUpsertForm
    { title = "Test Alert"
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
    , direction = "down"
    , alertThreshold = 1
    , warningThreshold = Nothing
    , alertId = Just (UUID.toText alertId)
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
    }


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Check Alerts" do
    it "should return an empty list" \tr -> do
      monitors <- runQueryEffect tr $ queryMonitorsAll testPid
      length monitors `shouldBe` 0

    it "should insert an alert" \tr -> do
      (_, pg) <-
        testServant tr $ Alerts.alertUpsertPostH testPid alertForm
      case pg of
        Alerts.AlertNoContent d -> do
          d `shouldBe` ""
        _ -> fail "unexpected response"
    it "should return a list with the inserted alert" \tr -> do
      monitors <- runQueryEffect tr $ queryMonitorsAll testPid
      case monitors of
        [alert] -> do
          alert.warningThreshold `shouldBe` Nothing
          alert.alertThreshold `shouldBe` 1
          alert.id `shouldBe` QueryMonitorId alertId
        _ -> expectationFailure "expected exactly one monitor"
    it "should get single alert" \tr -> do
      (_, pg) <-
        testServant tr $ Alerts.alertSingleGetH testPid (QueryMonitorId alertId)
      case pg of
        Alerts.AlertSingle pid monitorM -> do
          isJust monitorM `shouldBe` True
          let monitor = Unsafe.fromJust monitorM
          monitor.warningThreshold `shouldBe` Nothing
          monitor.alertThreshold `shouldBe` 1
          monitor.id `shouldBe` QueryMonitorId alertId
        _ -> fail "unexpected response"

    it "monitorRowActions_haveAccessibleNames" \tr -> do
      _ <- testServant tr $ Alerts.alertUpsertPostH testPid alertForm
      (_, page) <- testServant tr $ Alerts.unifiedMonitorsGetH testPid Nothing Nothing
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      for_ ["Deactivate", "Mute", "Delete"] \action ->
        html `shouldSatisfy` T.isInfixOf ("aria-label=\"" <> action <> "\"")
      html `shouldSatisfy` T.isInfixOf ("id=\"mute-pop-" <> UUID.toText alertId <> "-desktop\"")
      html `shouldSatisfy` T.isInfixOf ("id=\"mute-pop-" <> UUID.toText alertId <> "-mobile\"")

    -- A bulk action authenticates the session against `pid`, then mutates monitors by
    -- id alone: none of the six monitorXByIds queries carried a project_id predicate,
    -- so any authenticated user could mute/deactivate/resolve/delete another tenant's
    -- monitors by supplying their ids. Reachable from both the UI path
    -- (Pages.Monitors.alertBulkActionH) and the API path (ApiHandlers.apiMonitorBulk,
    -- which ignored its project id outright).
    it "bulkAction_doesNotTouchAnotherProjectsMonitors" \tr -> do
      let otherPid = UUIDId $ Unsafe.fromJust $ UUID.fromString "0000ffff-0000-0000-0000-00000000beef"
          otherMonId = Unsafe.fromJust $ UUID.fromString "0000ffff-0000-0000-0000-00000000cafe"
      withPool tr.trPool do
        void
          $ DBT.execute
            [sql| INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
                  VALUES (?, 'monitors-authz-victim', 'Free', true, NULL, false, false)
                  ON CONFLICT (id) DO NOTHING |]
            (Only otherPid.unwrap)
        void
          $ DBT.execute
            [sql| INSERT INTO monitors.query_monitors (id, project_id, check_interval_mins, alert_config, log_query, alert_threshold)
                  VALUES (?, ?, 5, '{"title":"victim","severity":"warning","subject":"s","message":"m","emails":[],"email_all":false,"slack_channels":[]}'::jsonb, 'status_code == 200', 1)
                  ON CONFLICT (id) DO NOTHING |]
            (otherMonId, otherPid.unwrap)

      -- Acting as testPid, aim every bulk action at the other project's monitor.
      for_ ["deactivate", "mute", "resolve", "delete"] \action ->
        void $ testServant tr $ Alerts.alertBulkActionH testPid action (Projects.TBulkActionForm [otherMonId])

      victims <- runQueryEffect tr $ queryMonitorsAll otherPid
      map (.id) victims `shouldBe` [QueryMonitorId otherMonId]
