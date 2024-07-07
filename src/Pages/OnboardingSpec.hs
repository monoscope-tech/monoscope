module Pages.OnboardingSpec (spec) where

import Test.Hspec

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Onboarding qualified as Onboarding
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Onboarding" do
    it "should return onboarding with no request" \TestResources{..} -> do
      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Onboarding.onboardingGetH testPid Nothing Nothing Nothing

      case res of
        Onboarding.OnboardingGetNM (PageCtx bwconf (pid, _apikey, hasRequest, ans, redi, ctb)) -> do
          pid `shouldBe` testPid
          hasRequest `shouldBe` False
          ans `shouldBe` False
          redi `shouldBe` False
          ctb `shouldBe` "express"
        _ -> error "Unexpected response"
    it "should return onboarding with request" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs = concat $ replicate 2 $ [("m1", reqMsg1), ("m2", reqMsg2)]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs
      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Onboarding.onboardingGetH testPid Nothing Nothing Nothing

      case res of
        Onboarding.OnboardingGetNM (PageCtx bwconf (pid, _apikey, hasRequest, _ans, _redi, ctb)) -> do
          pid `shouldBe` testPid
          hasRequest `shouldBe` True
          ctb `shouldBe` "express"
        _ -> error "Unexpected response"

      pass
