module Pages.MonitorsSpec (spec) where

import Models.Apis.Monitors
import Test.Hspec

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID qualified as UUID
import Lucid qualified
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
