module Pages.Fields.FieldDetailsSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Fields qualified as Fields
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Relude
import Test.Hspec

import Models.Apis.Formats qualified as Formats
import Pages.Fields.FieldDetails qualified as FieldDetails
import ProcessMessage (processRequestMessages)
import Relude.Unsafe qualified as Unsafe
import RequestMessages (toXXHash)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Field Details" do
    it "should get update field info" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            concat
              $ replicate 2
              $ [ ("m1", reqMsg1)
                , ("m2", reqMsg2)
                ]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx
      fieldsAll <- withPool trPool $ Fields.selectFields testPid $ (toXXHash $ testPid.toText <> "api.test.com" <> "POST" <> "/api/v1/user/login")
      let reqBodyFields = V.filter (\field -> field.fieldCategory == Fields.FCRequestBody) fieldsAll
      length reqBodyFields `shouldBe` 2
      let reqBodyField = reqBodyFields V.! 0

      let editFieldForm =
            FieldDetails.EditFieldForm
              { isRequired = Just "on"
              , isEnum = Just "on"
              , description = "User profile bio"
              , fieldType = "text"
              , formats = ["integer", "text", "date"]
              , fieldHash = reqBodyField.hash
              }
      _ <- toServantResponse trATCtx trSessAndHeader trLogger $ FieldDetails.fieldPutH testPid reqBodyField.id editFieldForm

      fieldM <- withPool trPool $ Fields.fieldById reqBodyField.id
      isJust fieldM `shouldBe` True
      let field = Unsafe.fromJust fieldM
      field.fieldCategory `shouldBe` Fields.FCRequestBody
      field.isRequired `shouldBe` True
      field.isEnum `shouldBe` True
      field.description `shouldBe` "User profile bio"
      fieldFormats <- withPool trPool $ Formats.formatsByFieldHash field.hash
      length fieldFormats `shouldBe` 3
      (.fieldFormat) <$> fieldFormats `shouldBe` ["text", "integer", "date"]
