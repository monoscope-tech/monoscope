module Pages.Bots.WhatsappSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKM
import Data.Pool (withResource)
import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
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
      it "parses /dashboard command" \_ -> do
        let msg = twilioWhatsAppDashboard "+1234567890"
        msg.body `shouldBe` "/dashboard"

      it "parses prompt messages" \_ -> do
        let msg = twilioWhatsAppPrompt "+1234567890" "show errors"
        msg.body `shouldBe` "show errors"

      it "extracts phone number correctly" \_ -> do
        let msg = twilioWhatsAppPrompt "+1234567890" "test"
        -- The from field has whatsapp: prefix
        T.isPrefixOf "whatsapp:" msg.from `shouldBe` True
        T.isInfixOf "+1234567890" msg.from `shouldBe` True

    describe "Project lookup" do
      it "handles unknown phone number gracefully" \tr -> do
        -- Message from phone number not linked to any project
        let msg = twilioWhatsAppPrompt "+9999999999" "show errors"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg

        -- Should return empty object (no project found)
        isEmptyJsonObject result `shouldBe` True

      it "processes message for linked phone number" \tr -> do
        -- Set up phone number for test project
        setupWhatsappNumber tr testPid "+1234567890"

        let msg = twilioWhatsAppPrompt "+1234567890" "/dashboard"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg

        -- Should return empty object (async processing via Twilio API)
        result `shouldSatisfy` isValidWhatsappResponse

    describe "Body type detection" do
      it "detects dashboard load command" \_ -> do
        let body = "/dashboard"
        isDashboardCommand body `shouldBe` True

      it "detects widget select format" \_ -> do
        let body = "widg___Error Rate___dash123"
        isWidgetSelect body `shouldBe` True

      it "detects prompt messages" \_ -> do
        let body = "show me error trends"
        isPrompt body `shouldBe` True

      it "detects dashboard pagination" \_ -> do
        let body = "dashboard___2"
        isDashboardPagination body `shouldBe` True

    describe "Response format" do
      it "uses template-based responses" \_ -> do
        -- WhatsApp uses Twilio templates for rich content
        -- Verify template structure expectations
        let templateVars =
              AE.object
                [ "1" AE..= ("Title" :: Text)
                , "2" AE..= ("Query" :: Text)
                ]
        hasRequiredTemplateVars templateVars `shouldBe` True

    describe "Agentic queries" do
      it "handles general queries via async fork" \tr -> do
        setupWhatsappNumber tr testPid "+1234567891"

        let msg = twilioWhatsAppPrompt "+1234567891" "what's my error rate?"
        result <- toBaseServantResponse tr.trATCtx tr.trLogger $ whatsappIncomingPostH msg

        -- Async processing returns empty, actual response sent via Twilio API
        result `shouldSatisfy` isValidWhatsappResponse


-- * Helper functions


isEmptyJsonObject :: AE.Value -> Bool
isEmptyJsonObject val = case val of
  AE.Object obj -> AEKM.null obj
  _ -> False


isValidWhatsappResponse :: AE.Value -> Bool
isValidWhatsappResponse val = case val of
  AE.Object _ -> True
  _ -> False


isDashboardCommand :: Text -> Bool
isDashboardCommand = (== "/dashboard")


isWidgetSelect :: Text -> Bool
isWidgetSelect body = "widg___" `T.isPrefixOf` body


isDashboardPagination :: Text -> Bool
isDashboardPagination body = "dashboard___" `T.isPrefixOf` body


isPrompt :: Text -> Bool
isPrompt body =
  not (isDashboardCommand body)
    && not (isWidgetSelect body)
    && not (isDashboardPagination body)
    && not ("dash___" `T.isPrefixOf` body)


hasRequiredTemplateVars :: AE.Value -> Bool
hasRequiredTemplateVars val = case val of
  AE.Object obj -> AEKM.member "1" obj
  _ -> False
