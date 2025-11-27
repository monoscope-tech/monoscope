module Pages.ApiSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.ProjectApiKeys (ProjectApiKey (..))
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec

import Pages.Api qualified as Api


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check API Keys" do
    it "should have one default apikey" \tr -> do
      (_, Api.ApiGet (PageCtx bwconf (pid, apiKeys))) <- testServant tr $ Api.apiGetH testPid
      length apiKeys `shouldBe` 1 -- Default API key from test setup

    it "should add apikey" \tr -> do
      let apikeyForm = Api.GenerateAPIKeyForm{title = "Test", from = Nothing}
      (_, Api.ApiPost pid apiKeys apiKeyM) <- testServant tr $ Api.apiPostH testPid apikeyForm
      length apiKeys `shouldBe` 2
      testPid `shouldBe` pid
      isJust apiKeyM `shouldBe` True
      let (apiKey, keyText) = Unsafe.fromJust apiKeyM
      apiKey.title `shouldBe` "Test"
      apiKey.active `shouldBe` True
      apiKey.keyPrefix `shouldBe` keyText

    it "should get apikey list" \tr -> do
      (_, Api.ApiGet (PageCtx bwconf (pid, apiKeys))) <- testServant tr $ Api.apiGetH testPid
      length apiKeys `shouldBe` 2

    it "should delete apikey" \tr -> do
      (_, Api.ApiGet (PageCtx bwconf (_pid, apiKeys))) <- testServant tr $ Api.apiGetH testPid
      length apiKeys `shouldBe` 2
      let apiKey = V.head apiKeys

      (_, Api.ApiPost pid apiKys apiKeyM) <- testServant tr $ Api.apiDeleteH testPid apiKey.id
      isNothing apiKeyM `shouldBe` True
      length apiKys `shouldBe` 2
      let apk = V.head apiKys
      apk.active `shouldBe` False
      pid `shouldBe` testPid
    it "should get apikey list (revoked)" \tr -> do
      (_, Api.ApiGet (PageCtx _ (_pid, apks))) <- testServant tr $ Api.apiGetH testPid
      length apks `shouldBe` 2
      -- Find the revoked key (the one we just deleted)
      let revokedKey = V.find (not . (.active)) apks
      isJust revokedKey `shouldBe` True
