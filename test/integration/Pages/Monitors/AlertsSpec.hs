module Pages.Monitors.AlertsSpec (spec) where

import Data.Vector qualified as V
import Models.Apis.Monitors
import Test.Hspec

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils

import Pages.Monitors.Alerts qualified as Alerts
import Relude
import Relude.Unsafe qualified as Unsafe


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


alertId :: UUID.UUID
alertId = UUID.nil


alertForm :: Alerts.AlertUpsertForm
alertForm =
  Alerts.AlertUpsertForm
    { title = "Test Alert"
    , message = "This is a test alert"
    , severity = "warning"
    , subject = "Test Alert"
    , query = "SELECT 1"
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
    , conditionType = Nothing
    , source = Nothing
    }


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Alerts" do
    it "should return an empty list" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Alerts.alertListGetH testPid
      case pg of
        Alerts.AlertListGet monitors -> do
          length monitors `shouldBe` 0
        _ -> fail "unexpected response"

    it "should insert an alert" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Alerts.alertUpsertPostH testPid alertForm
      case pg of
        Alerts.AlertNoContent d -> do
          d `shouldBe` ""
        _ -> fail "unexpected response"
    it "should return a list with the inserted alert" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Alerts.alertListGetH testPid
      case pg of
        Alerts.AlertListGet monitors -> do
          length monitors `shouldBe` 1
          let alert = V.head monitors
          alert.warningThreshold `shouldBe` Nothing
          alert.alertThreshold `shouldBe` 1
          alert.id `shouldBe` QueryMonitorId alertId
        _ -> fail "unexpected response"
    it "should get single alert" \TestResources{..} -> do
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Alerts.alertSingleGetH testPid (QueryMonitorId alertId)
      case pg of
        Alerts.AlertSingle pid monitorM -> do
          isJust monitorM `shouldBe` True
          let monitor = Unsafe.fromJust monitorM
          monitor.warningThreshold `shouldBe` Nothing
          monitor.alertThreshold `shouldBe` 1
          monitor.id `shouldBe` QueryMonitorId alertId
        _ -> fail "unexpected response"
