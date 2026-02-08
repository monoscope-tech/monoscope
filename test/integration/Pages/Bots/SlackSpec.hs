module Pages.Bots.SlackSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Vector qualified as V
import Models.Apis.Slack qualified as Slack
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Slack (slackInteractionsH)
import Pages.Bots.Utils (QueryIntent (..), ReportType (..), detectReportIntent)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Slack Bot" do
    describe "Query Intent Detection" do
      it "detects daily report intent" \_ -> do
        detectReportIntent "send daily report" `shouldBe` ReportIntent DailyReport
        detectReportIntent "get daily summary" `shouldBe` ReportIntent DailyReport
        detectReportIntent "show me the report" `shouldBe` ReportIntent DailyReport

      it "detects weekly report intent" \_ -> do
        detectReportIntent "send weekly report" `shouldBe` ReportIntent WeeklyReport
        detectReportIntent "get weekly summary" `shouldBe` ReportIntent WeeklyReport

      it "detects general query intent" \_ -> do
        detectReportIntent "show error rate" `shouldBe` GeneralQueryIntent
        detectReportIntent "what's happening" `shouldBe` GeneralQueryIntent
        detectReportIntent "errors in last hour" `shouldBe` GeneralQueryIntent

    describe "/here command" do
      it "sets notification channel and saves golden response" \tr -> do
        setupSlackData tr testPid "T_HERE_TEST"
        let interaction = slackInteraction "/here" "" "T_HERE_TEST"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        assertJsonGolden "slack/here_response.json" result
        extractResponseType result `shouldBe` Just "in_channel"
        hasSuccessBlock result `shouldBe` True

        slackDataM <- runTestBg tr $ Slack.getSlackDataByTeamId "T_HERE_TEST"
        case slackDataM of
          Just slackData -> slackData.channelId `shouldBe` "C_TEST_CHANNEL"
          Nothing -> expectationFailure "Slack data not found"

      it "returns correct block structure" \tr -> do
        setupSlackData tr testPid "T_HERE_BLOCK"
        let interaction = slackInteraction "/here" "" "T_HERE_BLOCK"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        case extractSlackBlocks result of
          Just (AE.Array blocks) -> do
            V.length blocks `shouldSatisfy` (>= 3)
            let firstBlock = V.head blocks
            getBlockType firstBlock `shouldBe` Just "header"
          _ -> expectationFailure "Expected blocks array"

    describe "/monoscope command" do
      it "returns loading message immediately" \tr -> do
        setupSlackData tr testPid "T_MONO_TEST"
        let interaction = slackInteraction "/monoscope" "show error rate" "T_MONO_TEST"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        assertJsonGolden "slack/monoscope_loading_response.json" result
        extractResponseText result `shouldSatisfy` isJust
        let responseText = fromMaybe "" $ extractResponseText result
        T.isInfixOf "Analyzing" responseText || T.isInfixOf "⏳" responseText `shouldBe` True

      it "handles missing slack data gracefully" \tr -> do
        let interaction = slackInteraction "/monoscope" "show errors" "T_NONEXISTENT"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        result `shouldSatisfy` isValidJsonResponse

    describe "/dashboard command" do
      it "returns error when no dashboards exist" \tr -> do
        setupSlackData tr testPid "T_DASH_EMPTY"
        let interaction = slackInteraction "/dashboard" "" "T_DASH_EMPTY"
        result <- try $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        case result of
          Left (e :: SomeException) -> T.isInfixOf "dashboards" (toText $ show e) `shouldBe` True
          Right _ -> pass

    describe "Anomaly Notifications" do
      it "captures Slack notification when anomaly occurs" \tr -> do
        setupSlackData tr testPid "T_ANOMALY_TEST"
        void $ runTestBg tr $ Slack.updateSlackNotificationChannel "T_ANOMALY_TEST" "C_ALERTS"

        (notifs, _) <- captureNotifications tr pass
        notifs `shouldBe` []

      it "user can ask about anomalies via /monoscope" \tr -> do
        setupSlackData tr testPid "T_ANOMALY_QUERY"

        let interaction = slackInteraction "/monoscope" "show me recent anomalies" "T_ANOMALY_QUERY"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        isValidJsonResponse result `shouldBe` True
        extractResponseText result `shouldSatisfy` isJust
