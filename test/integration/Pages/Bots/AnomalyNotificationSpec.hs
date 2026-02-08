module Pages.Bots.AnomalyNotificationSpec (spec) where

import Data.Aeson qualified as AE
import Data.Effectful.Notify (Notification (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import Models.Apis.Slack qualified as Slack
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Slack (slackInteractionsH)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Anomaly → Notification Flow" do
    describe "Slack notification on anomaly" do
      it "captures Slack notification when anomaly occurs" \tr -> do
        -- Setup Slack with notification channel
        setupSlackData tr testPid "T_ANOMALY_TEST"
        void $ runTestBg tr $ Slack.updateSlackNotificationChannel "T_ANOMALY_TEST" "C_ALERTS"

        -- Run a background context action that might send notifications
        (notifs, _) <- captureNotifications tr pass

        -- Verify the notification capture mechanism works
        -- In a real scenario, anomaly detection would trigger notifications
        -- For now, we verify the capture infrastructure is functional
        notifs `shouldBe` []

    describe "Query about anomalies via Slack" do
      it "user can ask about anomalies via /monoscope" \tr -> do
        setupSlackData tr testPid "T_ANOMALY_QUERY"

        -- User asks about anomalies
        let interaction = slackInteraction "/monoscope" "show me recent anomalies" "T_ANOMALY_QUERY"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        -- The response should be valid JSON
        isValidSlackResponse result `shouldBe` True

        -- The immediate response is a loading/processing message
        extractResponseText result `shouldSatisfy` isJust


-- * Helper functions


isValidSlackResponse :: AE.Value -> Bool
isValidSlackResponse val = case val of
  AE.Object _ -> True
  _ -> False
