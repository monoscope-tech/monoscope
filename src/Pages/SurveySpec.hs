module Pages.SurveySpec (spec) where

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pkg.TestUtils
import Relude
import Test.Hspec

import Pages.Survey qualified as Survey


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Survey Page" do
    it "should get survey page" \TestResources{..} -> do
      (Survey.SurveyGet (PageCtx bwconf (pid, full_name, phoneNumber))) <- toServantResponse trATCtx trSessAndHeader trLogger $ Survey.surveyGetH testPid
      pid `shouldBe` testPid
      full_name `shouldBe` "FN LN"
      phoneNumber `shouldBe` ""
