module Pages.ApiSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.ProjectApiKeys (ProjectApiKey (..))
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec

import Pages.Api qualified as Api


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check API Keys" do
    it "should have one default apikey" \TestResources{..} -> do
      (Api.ApiGet (PageCtx bwconf (pid, apiKeys))) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiGetH testPid
      length apiKeys `shouldBe` 1 -- Default API key from test setup

    it "should add apikey" \TestResources{..} -> do
      let apikeyForm = Api.GenerateAPIKeyForm{title = "Test", from = Nothing}
      (Api.ApiPost pid apiKeys apiKeyM) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiPostH testPid apikeyForm
      length apiKeys `shouldBe` 2
      testPid `shouldBe` pid
      isJust apiKeyM `shouldBe` True
      let (apiKey, keyText) = Unsafe.fromJust apiKeyM
      apiKey.title `shouldBe` "Test"
      apiKey.active `shouldBe` True
      apiKey.keyPrefix `shouldBe` keyText

    it "should get apikey list" \TestResources{..} -> do
      (Api.ApiGet (PageCtx bwconf (pid, apiKeys))) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiGetH testPid
      length apiKeys `shouldBe` 2

    it "should delete apikey" \TestResources{..} -> do
      (Api.ApiGet (PageCtx bwconf (_pid, apiKeys))) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiGetH testPid
      length apiKeys `shouldBe` 2
      let apiKey = V.head apiKeys

      (Api.ApiPost pid apiKys apiKeyM) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiDeleteH testPid apiKey.id
      isNothing apiKeyM `shouldBe` True
      length apiKys `shouldBe` 2
      let apk = V.head apiKys
      apk.active `shouldBe` False
      pid `shouldBe` testPid
    it "should get apikey list (revoked)" \TestResources{..} -> do
      (Api.ApiGet (PageCtx _ (_pid, apks))) <- toServantResponse trATCtx trSessAndHeader trLogger $ Api.apiGetH testPid
      length apks `shouldBe` 2
      -- Find the revoked key (the one we just deleted)
      let revokedKey = V.find (not . (.active)) apks
      isJust revokedKey `shouldBe` True
