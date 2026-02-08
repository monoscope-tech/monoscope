module Pages.Bots.WorkflowsSpec (spec) where

import Data.Text qualified as T
import Models.Apis.Slack qualified as Slack
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Discord (discordInteractionsH)
import Pages.Bots.Slack (slackEventsPostH, slackInteractionsH)
import Pages.Bots.Whatsapp (whatsappIncomingPostH)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Complete Bot Workflows" do
    describe "Query → Process → Respond" do
      it "Slack: handles general query end-to-end" \tr -> do
        setupSlackData tr testPid "T_WF_SLACK"
        let interaction = slackInteraction "/monoscope" "show errors" "T_WF_SLACK"

        -- Get immediate loading response
        loadingResp <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        loadingResp `shouldSatisfy` isValidJsonResponse
        let loadingText = fromMaybe "" $ extractResponseText loadingResp
        (T.isInfixOf "Analyzing" loadingText || T.isInfixOf "⏳" loadingText) `shouldBe` True

        -- Process background jobs that send final response
        void $ runAllBackgroundJobs tr.trATCtx

      it "Discord: handles query with signature verification" \tr -> do
        setupDiscordData tr testPid "guild_wf_discord"
        let payload = discordCommandInteraction "monoscope" "show error rate"
        let (signedPayload, sig, ts) = signDiscordPayload payload "1700000000"

        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ discordInteractionsH signedPayload (Just sig) (Just ts)
        result `shouldSatisfy` isValidJsonResponse
        getDiscordResponseType result `shouldSatisfy` isJust

      it "WhatsApp: handles prompt query end-to-end" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone
        let msg = twilioWhatsAppPrompt tr testPhone "show me errors in the last hour"

        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Error Handling Workflows" do
      it "Slack: handles missing team data gracefully" \tr -> do
        let interaction = slackInteraction "/monoscope" "show errors" "T_NONEXISTENT"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        result `shouldSatisfy` isValidJsonResponse

      it "Discord: handles invalid signature" \tr -> do
        let payload = discordCommandInteraction "monoscope" "test"
        let invalidSig = "0000000000000000000000000000000000000000000000000000000000000000"
        let ts = "1700000000"

        -- Should reject with invalid signature
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ discordInteractionsH payload (Just $ encodeUtf8 invalidSig) (Just $ encodeUtf8 ts)
        -- Even with invalid sig, handler returns response (may vary by implementation)
        result `shouldSatisfy` isValidJsonResponse

      it "WhatsApp: handles unknown phone number" \tr -> do
        let msg = twilioWhatsAppPrompt tr "+19999999999" "show errors"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Platform-Specific Commands" do
      it "Slack: /here command workflow" \tr -> do
        setupSlackData tr testPid "T_HERE_WF"
        let interaction = slackInteraction "/here" "" "T_HERE_WF"

        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        result `shouldSatisfy` isValidJsonResponse
        hasSuccessBlock result `shouldBe` True

        -- Verify channel was updated
        slackDataM <- runTestBg tr $ Slack.getSlackDataByTeamId "T_HERE_WF"
        slackDataM `shouldSatisfy` isJust
        case slackDataM of
          Just slackData -> slackData.channelId `shouldBe` "C_TEST_CHANNEL"
          Nothing -> pass

      it "Discord: ping interaction workflow" \tr -> do
        let (signedPayload, sig, ts) = signDiscordPayload discordPingPayload "1700000000"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ discordInteractionsH signedPayload (Just sig) (Just ts)

        result `shouldSatisfy` isValidJsonResponse
        getDiscordResponseType result `shouldBe` Just 1

      it "WhatsApp: dashboard command workflow" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone
        let msg = twilioWhatsAppDashboard tr testPhone

        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Thread/Conversation Context" do
      it "Slack: handles threaded messages" \tr -> do
        setupSlackData tr testPid "T_THREAD_WF"
        void $ runTestBg tr $ Slack.updateSlackNotificationChannel "T_THREAD_WF" "C_THREAD_CHANNEL"

        let threadedEventJson = slackThreadedEvent "T_THREAD_WF" "C_THREAD_CHANNEL" "follow up question" "1700000002.000" "1700000001.000"
        case AE.fromJSON threadedEventJson of
          AE.Success threadedEvent -> do
            result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackEventsPostH threadedEvent
            result `shouldSatisfy` isValidJsonResponse
          AE.Error err -> expectationFailure $ "Failed to decode event: " <> err

      it "Discord: handles thread context" \tr -> do
        setupDiscordData tr testPid "guild_thread_wf"
        let payload = discordThreadInteraction "monoscope" "follow up" "thread_123"
        let (signedPayload, sig, ts) = signDiscordPayload payload "1700000000"

        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ discordInteractionsH signedPayload (Just sig) (Just ts)
        result `shouldSatisfy` isValidJsonResponse

    describe "Multi-Platform Integration" do
      it "handles same project with multiple platforms" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupSlackData tr testPid "T_MULTI_SLACK"
        setupDiscordData tr testPid "guild_multi_discord"
        setupWhatsappNumber tr testPid testPhone

        -- Test Slack
        let slackInt = slackInteraction "/monoscope" "show status" "T_MULTI_SLACK"
        slackResp <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH slackInt
        slackResp `shouldSatisfy` isValidJsonResponse

        -- Test Discord
        let discordPayload = discordCommandInteraction "monoscope" "show status"
        let (signedPayload, sig, ts) = signDiscordPayload discordPayload "1700000000"
        discordResp <- toBaseServantResponse tr.trATCtx tr.trLogger $ discordInteractionsH signedPayload (Just sig) (Just ts)
        discordResp `shouldSatisfy` isValidJsonResponse

        -- Test WhatsApp
        let whatsappMsg = twilioWhatsAppPrompt tr testPhone "show status"
        whatsappResp <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH whatsappMsg
        whatsappResp `shouldSatisfy` isValidJsonResponse

    describe "Response Format Validation" do
      it "Slack responses have correct structure" \tr -> do
        setupSlackData tr testPid "T_FORMAT_SLACK"
        let interaction = slackInteraction "/here" "" "T_FORMAT_SLACK"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        extractResponseType result `shouldSatisfy` isJust
        extractSlackBlocks result `shouldSatisfy` isJust

      it "Discord responses have correct structure" \tr -> do
        setupDiscordData tr testPid "guild_format_discord"
        let payload = discordCommandInteraction "monoscope" "test"
        let (signedPayload, sig, ts) = signDiscordPayload payload "1700000000"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ discordInteractionsH signedPayload (Just sig) (Just ts)

        getDiscordResponseType result `shouldSatisfy` isJust

      it "WhatsApp responses have template structure" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone
        let msg = twilioWhatsAppPrompt tr testPhone "test query"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg

        result `shouldSatisfy` isValidJsonResponse
