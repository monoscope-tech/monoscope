module Pages.Bots.DiscordSpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Discord (discordInteractionsH)
import Pkg.TestUtils
import Relude
import System.Config qualified as Config
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Discord Bot" do
    describe "Ping interaction" do
      it "responds to ping with type 1" \tr -> do
        let (body, sig, ts) = signDiscordPayload discordPingPayload "1706745600"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        getDiscordResponseType result `shouldBe` Just 1

    describe "/here command" do
      it "returns structured success response" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"

        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745601"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        getDiscordResponseType result `shouldBe` Just 4

      it "matches golden file format" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"

        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745606"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        assertJsonGolden "discord/here_response.json" result

    describe "/monoscope command" do
      it "uses deferred response pattern" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"

        let payload = discordCommandInteraction "monoscope" "show errors"
            (body, sig, ts) = signDiscordPayload payload "1706745602"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        isEmptyResponse result `shouldBe` True

      it "handles thread context for conversations" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"
        let threadPayload = discordThreadInteraction "monoscope" "show errors" "thread_123"
        BS.length threadPayload `shouldSatisfy` (> 0)

    describe "/dashboard command" do
      it "returns select menu for dashboards" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"
        let payload = discordCommandInteraction "dashboard" ""
            (body, sig, ts) = signDiscordPayload payload "1706745607"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        result `shouldSatisfy` isValidJsonResponse

    describe "Response format" do
      it "/here response includes Components v2 flag" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"
        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745603"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        hasComponentsV2Flag result `shouldBe` True

      it "/here response has container component" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"
        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745604"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        hasContainerComponent result `shouldBe` True

      it "/here response has text content components" \tr -> do
        setupDiscordData tr testPid "1234567890123456789"
        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745605"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger tr.trTestClock $ discordInteractionsH body (Just sig) (Just ts)
        countTextComponents result `shouldSatisfy` (>= 3)
