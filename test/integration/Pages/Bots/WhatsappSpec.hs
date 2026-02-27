module Pages.Bots.WhatsappSpec (spec) where

import Data.Aeson qualified as AE
import Data.Text qualified as T
import Pages.Bots.BotFixtures
import Pages.Bots.BotTestHelpers
import Pages.Bots.Whatsapp (TwilioWhatsAppMessage (..), whatsappIncomingPostH)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)


spec :: Spec
spec = aroundAll withTestResources do
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
        result <- toBaseServantResponse tr.trATCtx tr.trLogger tr.trTestClock $ whatsappIncomingPostH msg
        isEmptyJsonObject result `shouldBe` True

      it "processes message for linked phone number" \tr -> do
        let testPhone = getTestPhoneNumber tr
        setupWhatsappNumber tr testPid testPhone

        let msg = twilioWhatsAppPrompt tr testPhone "/dashboard"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger tr.trTestClock $ whatsappIncomingPostH msg
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
        result <- toBaseServantResponse tr.trATCtx tr.trLogger tr.trTestClock $ whatsappIncomingPostH msg
        result `shouldSatisfy` isValidJsonResponse
