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
import Servant qualified
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


--   , alertThreshold :: Int
--   , warningThreshold :: Maybe Text
--   , recipientEmails :: [Text]
--   , recipientSlacks :: [Text]
--   , recipientEmailAll :: Maybe Bool
--   , -- , checkIntervalMins :: Int
--     direction :: Text
--   , title :: Text
--   , severity :: Text
--   , subject :: Text
--   , message :: Text
--   , query :: Text
--   , since :: Text
--   , from :: Text
--   , to :: Text

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
    , alertId = (Just (UUID.toText alertId))
    }


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Alerts" do
    it "should return an empty list" \TestResources{..} -> do
      pg <-
        Alerts.alertListGetH testPid
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      case pg of
        Alerts.AlertListGet (monitors) -> do
          length monitors `shouldBe` 0
        _ -> fail "unexpected response"

    it "should insert an alert" \TestResources{..} -> do
      pg <-
        Alerts.alertUpsertPostH testPid alertForm
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      case pg of
        Alerts.AlertNoContent d -> do
          d `shouldBe` ""
        _ -> fail "unexpected response"
    it "should return a list with the inserted alert" \TestResources{..} -> do
      pg <-
        Alerts.alertListGetH testPid
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      case pg of
        Alerts.AlertListGet (monitors) -> do
          length monitors `shouldBe` 1
          let alert = V.head monitors
          alert.warningThreshold `shouldBe` Nothing
          alert.alertThreshold `shouldBe` 1
          alert.id `shouldBe` (QueryMonitorId alertId)
        _ -> fail "unexpected response"
    it "should get single alert" \TestResources{..} -> do
      pg <-
        Alerts.alertSingleGetH testPid (QueryMonitorId alertId)
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse
      case pg of
        Alerts.AlertSingle pid monitorM -> do
          isJust monitorM `shouldBe` True
          let monitor = Unsafe.fromJust monitorM
          monitor.warningThreshold `shouldBe` Nothing
          monitor.alertThreshold `shouldBe` 1
          monitor.id `shouldBe` (QueryMonitorId alertId)
        _ -> fail "unexpected response"
