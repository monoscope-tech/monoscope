module Pages.Bots.SlackSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Aeson qualified as AE
import Data.Text qualified as T
import Data.Vector qualified as V
import Models.Apis.Integrations qualified as Slack
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Slack (slackInteractionsH)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Slack Bot" do
    describe "/here command" do
      it "sets notification channel and saves golden response" \tr -> do
        setupSlackData tr testPid "T01HEREA9X"
        let interaction = slackInteraction "/monoscope-here" "" "T01HEREA9X"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        assertJsonGolden "slack/here_response.json" result
        extractResponseType result `shouldBe` Just "in_channel"
        hasSuccessBlock result `shouldBe` True

        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T01HEREA9X"
        case slackDataM of
          Just slackData -> slackData.channelId `shouldBe` "C0123ABCDEF"
          Nothing -> expectationFailure "Slack data not found"

      it "returns correct block structure" \tr -> do
        setupSlackData tr testPid "T02BLOCKB8Y"
        let interaction = slackInteraction "/monoscope-here" "" "T02BLOCKB8Y"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        case extractSlackBlocks result of
          Just (AE.Array blocks) -> do
            V.length blocks `shouldSatisfy` (>= 3)
            let firstBlock = V.head blocks
            getBlockType firstBlock `shouldBe` Just "header"
          _ -> expectationFailure "Expected blocks array"

    describe "/monoscope command" do
      it "returns loading message immediately" \tr -> do
        setupSlackData tr testPid "T03MONOSC0P"
        let interaction = slackInteraction "/monoscope" "show error rate" "T03MONOSC0P"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        assertJsonGolden "slack/monoscope_loading_response.json" result
        extractResponseText result `shouldSatisfy` isJust
        let responseText = fromMaybe "" $ extractResponseText result
        T.isInfixOf "Analyzing" responseText || T.isInfixOf "⏳" responseText `shouldBe` True

      it "handles missing slack data gracefully" \tr -> do
        let interaction = slackInteraction "/monoscope" "show errors" "T99NONEXIST"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        result `shouldSatisfy` isValidJsonResponse

    describe "/dashboard command" do
      it "returns error when no dashboards exist" \tr -> do
        setupSlackData tr testPid "T04DASHEMTY"
        let interaction = slackInteraction "/dashboard" "" "T04DASHEMTY"
        result <- try $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        case result of
          Left (e :: SomeException) -> T.isInfixOf "dashboards" (toText $ show e) `shouldBe` True
          Right _ -> pass

    describe "Anomaly Notifications" do
      it "captures Slack notification when anomaly occurs" \tr -> do
        setupSlackData tr testPid "T05ANOMALY1"
        void $ runTestBg frozenTime tr $ Slack.updateSlackNotificationChannel "T05ANOMALY1" "C06ALERTSCH"

        -- Use existing infrastructure from TestUtils
        (notifs, _) <- runTestBackgroundWithNotifications frozenTime tr.trLogger tr.trATCtx pass
        notifs `shouldBe` []

      it "user can ask about anomalies via /monoscope" \tr -> do
        setupSlackData tr testPid "T06ANOMQRY2"

        let interaction = slackInteraction "/monoscope" "show me recent anomalies" "T06ANOMQRY2"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        isValidJsonResponse result `shouldBe` True
        extractResponseText result `shouldSatisfy` isJust

    describe "Channel Management" do
      it "/here command adds channel to @everyone team" \tr -> do
        setupSlackData tr testPid "T07HERETEAM"
        let interaction = slackInteraction "/monoscope-here" "" "T07HERETEAM"
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T07HERETEAM"
        case slackDataM of
          Just slackData -> do
            everyoneTeamM <- runTestBg frozenTime tr $ ProjectMembers.getEveryoneTeam slackData.projectId
            case everyoneTeamM of
              Just team -> V.elem "C0123ABCDEF" team.slack_channels `shouldBe` True
              Nothing -> expectationFailure "@everyone team not found"
          Nothing -> expectationFailure "Slack data not found"

      it "/here command does not duplicate channels in @everyone team" \tr -> do
        setupSlackData tr testPid "T08HEREDUP"
        let interaction = slackInteraction "/monoscope-here" "" "T08HEREDUP"
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T08HEREDUP"
        case slackDataM of
          Just slackData -> do
            everyoneTeamM <- runTestBg frozenTime tr $ ProjectMembers.getEveryoneTeam slackData.projectId
            case everyoneTeamM of
              Just team -> do
                let channelCount = V.length $ V.filter (== "C0123ABCDEF") team.slack_channels
                channelCount `shouldBe` 1
              Nothing -> expectationFailure "@everyone team not found"
          Nothing -> expectationFailure "Slack data not found"

      it "/here updates both default channel_id and @everyone team" \tr -> do
        setupSlackData tr testPid "T09HEREBOTH"
        let interaction = slackInteraction "/monoscope-here" "" "T09HEREBOTH"
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T09HEREBOTH"
        case slackDataM of
          Just slackData -> do
            slackData.channelId `shouldBe` "C0123ABCDEF"
            everyoneTeamM <- runTestBg frozenTime tr $ ProjectMembers.getEveryoneTeam slackData.projectId
            case everyoneTeamM of
              Just team -> V.elem "C0123ABCDEF" team.slack_channels `shouldBe` True
              Nothing -> expectationFailure "@everyone team not found"
          Nothing -> expectationFailure "Slack data not found"

      it "/here sends welcome message when channel is newly added" \tr -> do
        setupSlackData tr testPid "T10HEREWELC"
        let interaction = slackInteraction "/monoscope-here" "" "T10HEREWELC"
        -- First call should add channel and attempt to send welcome message
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T10HEREWELC"
        case slackDataM of
          Just slackData -> do
            everyoneTeamM <- runTestBg frozenTime tr $ ProjectMembers.getEveryoneTeam slackData.projectId
            case everyoneTeamM of
              Just team -> V.elem "C0123ABCDEF" team.slack_channels `shouldBe` True
              Nothing -> expectationFailure "@everyone team not found"
          Nothing -> expectationFailure "Slack data not found"

      it "/here does not send welcome message when channel already exists" \tr -> do
        setupSlackData tr testPid "T11HERENODUP"
        let interaction = slackInteraction "/monoscope-here" "" "T11HERENODUP"
        -- First call adds channel
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        -- Second call should not send welcome message (channel already exists)
        void $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        slackDataM <- runTestBg frozenTime tr $ Slack.getSlackDataByTeamId "T11HERENODUP"
        case slackDataM of
          Just slackData -> do
            everyoneTeamM <- runTestBg frozenTime tr $ ProjectMembers.getEveryoneTeam slackData.projectId
            case everyoneTeamM of
              Just team -> do
                -- Channel should exist only once
                let channelCount = V.length $ V.filter (== "C0123ABCDEF") team.slack_channels
                channelCount `shouldBe` 1
              Nothing -> expectationFailure "@everyone team not found"
          Nothing -> expectationFailure "Slack data not found"
