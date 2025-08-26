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
testPid = Projects.ProjectId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0


alertId :: UUID.UUID
alertId = UUID.fromWords 0x11111111 0x22222222 0x33333333 0x44444444


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
    , recipientEmails = ["test@example.com"]
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
spec = do
  describe "Check Alerts" do
    around withTestResources $ do
      it "should return an empty list" \TestResources{..} -> do
        pg <-
          toServantResponse trATCtx trSessAndHeader trLogger $ Alerts.alertListGetH testPid
        case pg of
          Alerts.AlertListGet monitors -> do
            length monitors `shouldBe` 0
          _ -> fail "unexpected response"

    around withTestResources $ do
      it "should insert an alert" \TestResources{..} -> do
        -- Insert the alert
        pg <-
          toServantResponse trATCtx trSessAndHeader trLogger $ Alerts.alertUpsertPostH testPid alertForm
        case pg of
          Alerts.AlertNoContent d -> do
            d `shouldBe` ""
          _ -> fail "unexpected response when inserting"
