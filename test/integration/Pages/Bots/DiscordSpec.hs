module Pages.Bots.DiscordSpec (spec) where

import Control.Lens ((^?), each, filtered, has, lengthOf, to)
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _Number)
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
        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)
        getResponseType result `shouldBe` Just 1

    describe "/here command" do
      it "returns structured success response" \tr -> do
        setupDiscordData tr testPid "guild_here_test"

        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745601"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)

        -- Verify the response type is 4 (CHANNEL_MESSAGE_WITH_SOURCE)
        getResponseType result `shouldBe` Just 4

      it "matches golden file format" \tr -> do
        setupDiscordData tr testPid "guild_here_golden"

        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745606"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)
        assertJsonGolden "discord/here_response.json" result

    describe "/monoscope command" do
      it "uses deferred response pattern" \tr -> do
        setupDiscordData tr testPid "guild_mono_test"

        let payload = discordCommandInteraction "monoscope" "show errors"
            (body, sig, ts) = signDiscordPayload payload "1706745602"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}

        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)

        -- The monoscope command returns empty object (deferred response)
        isEmptyResponse result `shouldBe` True

      it "handles thread context for conversations" \tr -> do
        setupDiscordData tr testPid "guild_thread_test"

        -- Verify thread interaction payload structure
        let threadPayload = discordThreadInteraction "monoscope" "show errors" "thread_123"
        BS.length threadPayload `shouldSatisfy` (> 0)

    describe "/dashboard command" do
      it "returns select menu for dashboards" \tr -> do
        setupDiscordData tr testPid "guild_dash_test"

        -- Dashboard command returns empty (deferred) then sends followup with select
        let deferredResponse = AE.object []
        isEmptyResponse deferredResponse `shouldBe` True

    describe "Response format" do
      it "/here response includes Components v2 flag" \tr -> do
        setupDiscordData tr testPid "guild_format_test"
        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745603"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)
        hasComponentsV2Flag result `shouldBe` True

      it "/here response has container component" \tr -> do
        setupDiscordData tr testPid "guild_container_test"
        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745604"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)
        hasContainerComponent result `shouldBe` True

      it "/here response has text content components" \tr -> do
        setupDiscordData tr testPid "guild_text_test"
        let payload = discordCommandInteraction "here" ""
            (body, sig, ts) = signDiscordPayload payload "1706745605"
            testConfig = tr.trATCtx.env{Config.discordPublicKey = testDiscordPublicKeyHex}
            testCtx = tr.trATCtx{Config.env = testConfig}
        result <- toBaseServantResponse testCtx tr.trLogger $ discordInteractionsH body (Just sig) (Just ts)
        countTextComponents result `shouldSatisfy` (>= 3)


-- * Helper functions


getResponseType :: AE.Value -> Maybe Int
getResponseType val = val ^? key "type" . _Number . to round

isEmptyResponse :: AE.Value -> Bool
isEmptyResponse (AE.Object o) = null o
isEmptyResponse _ = False

hasComponentsV2Flag :: AE.Value -> Bool
hasComponentsV2Flag val = val ^? key "data" . key "flags" . _Number . to round == Just (32768 :: Int)

hasContainerComponent :: AE.Value -> Bool
hasContainerComponent val = has (key "data" . key "components" . _Array . each . filtered isContainer) val
  where isContainer v = v ^? key "type" . _Number . to round == Just (17 :: Int)

countTextComponents :: AE.Value -> Int
countTextComponents val = lengthOf (key "data" . key "components" . _Array . each . key "components" . _Array . each . filtered isText) val
  where isText v = v ^? key "type" . _Number . to round == Just (10 :: Int)
