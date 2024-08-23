module Pages.ReportsSpec (spec) where

import Test.Hspec

import OddJobs.Job (createJob)

import Data.Pool (withResource)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Models.Apis.Reports
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Reports qualified as Reports

import BackgroundJobs qualified
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Reports" do
    it "should toggle reports notifs" \TestResources{..} -> do
      _ <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Reports.reportsPostH testPid "daily"
      _ <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Reports.reportsPostH testPid "weekly"
      res <- toServantResponse trATCtx trSessAndHeader trLogger $ Reports.reportsGetH testPid Nothing Nothing Nothing
      case res of
        Reports.ReportsGetMain (PageCtx _ (pid, reports, next, daily, weekly)) -> do
          pid `shouldBe` testPid
          daily `shouldBe` True
          weekly `shouldBe` False
          length reports `shouldBe` 0
        _ -> error "Unexpected response"

    it "should create and get all reports" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt

      let reqMsg3 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 "2024-07-05T13:06:26.620094239Z"
      let reqMsg4 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 "2024-07-05T12:06:26.620094239Z"

      let msgs = concat (replicate 100 $ [("m1", reqMsg1), ("m2", reqMsg2)]) ++ [("m3", reqMsg3), ("m4", reqMsg4)]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs

      _ <- liftIO $ withResource trPool \conn -> do
        _ <- createJob conn "background_jobs" $ BackgroundJobs.DailyReports testPid
        _ <- createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports testPid
        pass

      _ <- runAllBackgroundJobs trATCtx

      res <- toServantResponse trATCtx trSessAndHeader trLogger $ Reports.reportsGetH testPid Nothing Nothing Nothing
      case res of
        Reports.ReportsGetMain (PageCtx _ (pid, reports, next, daily, weekly)) -> do
          pid `shouldBe` testPid
          daily `shouldBe` True
          weekly `shouldBe` False
          length reports `shouldBe` 2
          let dailyReport = Unsafe.fromJust $ find (\r -> r.reportType == "daily") reports
          let weeklyReport = Unsafe.fromJust $ find (\r -> r.reportType == "weekly") reports
          dailyReport.reportType `shouldBe` "daily"
          weeklyReport.reportType `shouldBe` "weekly"

          r <- toServantResponse trATCtx trSessAndHeader trLogger $ Reports.singleReportGetH testPid dailyReport.id
          case r of
            Reports.ReportsGetSingle (PageCtx _ (prid, reportM)) -> do
              prid `shouldBe` testPid
              isJust reportM `shouldBe` True
              let report = Unsafe.fromJust reportM
              report.id `shouldBe` dailyReport.id
              pass
            _ -> error "Unexpected response"
        _ -> error "Unexpected response"
