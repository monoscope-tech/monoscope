module Pages.MonitorsSpec (spec) where

import Models.Apis.Monitors
import Test.Hspec

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils

import Pages.Monitors qualified as Alerts
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
spec = aroundAll withTestResources do
  describe "Check Alerts" do
    it "should insert an alert" \tr -> do
      (_, pg) <-
        testServant tr $ Alerts.alertUpsertPostH testPid alertForm
      case pg of
        Alerts.AlertNoContent d -> do
          d `shouldBe` ""
        _ -> fail "unexpected response"
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
