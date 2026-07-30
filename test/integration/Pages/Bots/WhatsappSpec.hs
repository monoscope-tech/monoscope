module Pages.Bots.WhatsappSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Text qualified as T
import Data.Vector qualified as V
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Whatsapp (BodyType (..), TwilioWhatsAppMessage (..), getWhatsappList, parseWhatsappBody, whatsappIncomingPostH)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = around withTestResources do
  describe "WhatsApp Bot" do
    describe "Message parsing" do
      it "parses /dashboard command" \tr -> do
        let testPhone = getTestPhoneNumber tr
            msg = twilioWhatsAppDashboard tr testPhone
        msg.body `shouldBe` "/dashboard"

      it "parses prompt messages" \tr -> do
        let testPhone = getTestPhoneNumber tr
            msg = twilioWhatsAppPrompt tr testPhone "show errors"
        msg.body `shouldBe` "show errors"

      it "extracts phone number correctly" \tr -> do
        let testPhone = getTestPhoneNumber tr
            msg = twilioWhatsAppPrompt tr testPhone "test"
        T.isPrefixOf "whatsapp:" msg.from `shouldBe` True
        T.isInfixOf testPhone msg.from `shouldBe` True

    describe "Project lookup" do
      it "handles unknown phone number gracefully" \tr -> do
        let msg = twilioWhatsAppPrompt tr "+19999999999" "show errors"
        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg
        isEmptyJsonObject result `shouldBe` True

      it "processes message for linked phone number" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone

        let msg = twilioWhatsAppPrompt tr testPhone "/dashboard"
        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse

    describe "Body type detection" do
      it "detects dashboard load command" \_ -> do
        isDashboardCommand "/dashboard" `shouldBe` True

      it "detects widget select format" \_ -> do
        isWidgetSelect "widg___Error Rate___dash123" `shouldBe` True

      it "detects prompt messages" \_ -> do
        isPrompt "show me error trends" `shouldBe` True

      it "detects dashboard pagination" \_ -> do
        isDashboardPagination "dashboard___2" `shouldBe` True

      -- Regression: the handler used to discard the parsed skip and always send
      -- page 0, so the dashboard list's "Load More" button never advanced.
      it "Load More advances the dashboard list window" \_ -> do
        case parseWhatsappBody "dashboard___2" of
          DashboardLoad n -> n `shouldBe` 2
          _ -> expectationFailure "expected DashboardLoad"
        let dashes = V.fromList [(l, "dash___" <> l) | l <- ["a", "b", "c", "d", "e"]]
            varsAt skip = case getWhatsappList "dashboard" "pick" dashes skip of
              AE.Object o -> o
              _ -> mempty
        -- Page 0: first item is "a", Load More points at skip=2.
        KEM.lookup "2" (varsAt 0) `shouldBe` Just (AE.String "a")
        KEM.lookup "7" (varsAt 0) `shouldBe` Just (AE.String "dashboard___2")
        -- Page 1 (skip=2): window actually advances to "c".
        KEM.lookup "2" (varsAt 2) `shouldBe` Just (AE.String "c")

    describe "Response format" do
      it "uses template-based responses" \_ -> do
        let templateVars =
              AE.object
                [ "1" AE..= ("Title" :: Text)
                , "2" AE..= ("Query" :: Text)
                ]
        hasRequiredTemplateVars templateVars `shouldBe` True

    describe "Agentic queries" do
      it "handles general queries via async fork" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone

        let msg = twilioWhatsAppPrompt tr testPhone "what's my error rate?"
        result <- toBaseServantResponse tr $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse
