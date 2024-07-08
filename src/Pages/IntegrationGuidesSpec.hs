module Pages.IntegrationGuidesSpec (spec) where

import Test.Hspec

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.Api qualified as Api
import Pages.BodyWrapper (PageCtx (..))
import Pages.IntegrationGuides qualified as IntegrationGuides
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Integration Guides" do
    it "should guides WITHOUT api key" \TestResources{..} -> do
      IntegrationGuides.IntegrationsGet (PageCtx bwconf (pid, sdk, apiKey, errReportM, reqMonM)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ IntegrationGuides.getH testPid Nothing Nothing Nothing

      pid `shouldBe` testPid
      sdk `shouldBe` "express"
      apiKey `shouldBe` "<API_KEY>"
      isNothing errReportM `shouldBe` True
      isNothing reqMonM `shouldBe` True

    it "should guides WITH api key" \TestResources{..} -> do
      let apikeyForm = Api.GenerateAPIKeyForm{title = "Test", from = Nothing}
      (Api.ApiPost _pid apiKeys apiKeyM) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiPostH testPid apikeyForm

      IntegrationGuides.IntegrationsGet (PageCtx bwconf (pid, sdk, apiKey, errReportM, reqMonM)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ IntegrationGuides.getH testPid (Just "gin") (Just "error") (Just "mon")
      let (_apiKeyData, apiKeyText) = Unsafe.fromJust apiKeyM
      pid `shouldBe` testPid
      sdk `shouldBe` "gin"
      apiKey `shouldBe` apiKeyText
      isJust errReportM `shouldBe` True
      isJust reqMonM `shouldBe` True
