module Pages.DashboardSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Dashboard
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Dashboard" do
    it "should return a dashboard" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            concat
              $ replicate
                100
                [ ("m1", reqMsg1)
                , ("m2", reqMsg2)
                ]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx
      _ <- withPool trPool $ refreshMaterializedView "apis.project_request_stats"

      (PageCtx _ dat) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ dashboardGetH testPid Nothing Nothing Nothing
      let (pid, _paramInput, _currTime, projectRequestStats, newEndpoints, _reqLatenciesRolledByStepsJ, _, (_fromD, _toD), freeTierExceeded, hasRequests) = dat.unwrap

      testPid `shouldBe` pid
      projectRequestStats.totalRequests `shouldBe` 200
      length newEndpoints `shouldBe` 2
      freeTierExceeded `shouldBe` False
      hasRequests `shouldBe` True
      projectRequestStats.totalEndpoints `shouldBe` 2
      projectRequestStats.totalShapes `shouldBe` 2
      projectRequestStats.totalAnomalies `shouldBe` 2
