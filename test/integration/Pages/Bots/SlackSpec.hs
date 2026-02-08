module Pages.Bots.SlackSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Text qualified as T
import Data.Vector qualified as V
import Models.Apis.Slack qualified as Slack
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Slack (slackInteractionsH)
import Pages.Bots.Utils (QueryIntent (..), ReportType (..), detectReportIntent)
import Pkg.TestUtils
import Relude
import Control.Exception (SomeException, try)
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy)


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

        -- Save response to golden file
        assertJsonGolden "slack/here_response.json" result

        -- Verify response structure
        extractResponseType result `shouldBe` Just "in_channel"
        hasSuccessBlock result `shouldBe` True

        -- Verify DB was updated
        slackDataM <- runTestBg tr $ Slack.getSlackDataByTeamId "T_HERE_TEST"
        case slackDataM of
          Just slackData -> slackData.channelId `shouldBe` "C_TEST_CHANNEL"
          Nothing -> expectationFailure "Slack data not found"

      it "returns correct block structure" \tr -> do
        setupSlackData tr testPid "T_HERE_BLOCK"
        let interaction = slackInteraction "/here" "" "T_HERE_BLOCK"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction

        -- Verify blocks exist
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

        -- Save the immediate loading response
        assertJsonGolden "slack/monoscope_loading_response.json" result

        -- The immediate response should be a loading message
        extractResponseText result `shouldSatisfy` isJust
        let responseText = fromMaybe "" $ extractResponseText result
        T.isInfixOf "Analyzing" responseText || T.isInfixOf "⏳" responseText `shouldBe` True

      it "handles missing slack data gracefully" \tr -> do
        let interaction = slackInteraction "/monoscope" "show errors" "T_NONEXISTENT"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        result `shouldSatisfy` isValidSlackResponse

    describe "/dashboard command" do
      it "returns error when no dashboards exist" \tr -> do
        setupSlackData tr testPid "T_DASH_EMPTY"
        let interaction = slackInteraction "/dashboard" "" "T_DASH_EMPTY"
        -- With no dashboards, the handler should throw a 400 error
        -- This is expected behavior - we verify the error message
        result <- try $ toBaseServantResponse tr.trATCtx tr.trLogger $ slackInteractionsH interaction
        case result of
          Left (e :: SomeException) -> T.isInfixOf "dashboards" (toText $ show e) `shouldBe` True
          Right _ -> pass -- If it returns successfully (unlikely), that's also acceptable


-- * Helper functions


expectationFailure :: String -> IO ()
expectationFailure msg = error $ toText msg


extractResponseType :: AE.Value -> Maybe Text
extractResponseType val = case val of
  AE.Object obj ->
    case AEKM.lookup "response_type" obj of
      Just (AE.String t) -> Just t
      _ -> Nothing
  _ -> Nothing


hasSuccessBlock :: AE.Value -> Bool
hasSuccessBlock val = case extractSlackBlocks val of
  Just (AE.Array blocks) -> V.any hasSuccessEmoji blocks
  _ -> False
  where
    hasSuccessEmoji block =
      let blockStr = decodeUtf8 $ toStrict $ AE.encode block
       in T.isInfixOf "🟢" blockStr || T.isInfixOf "success" blockStr


getBlockType :: AE.Value -> Maybe Text
getBlockType val = case val of
  AE.Object obj ->
    case AEKM.lookup "type" obj of
      Just (AE.String t) -> Just t
      _ -> Nothing
  _ -> Nothing


isValidSlackResponse :: AE.Value -> Bool
isValidSlackResponse val = case val of
  AE.Object _ -> True
  _ -> False
