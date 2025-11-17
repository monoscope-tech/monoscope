module Pages.ReportsSpec (spec) where

import Test.Hspec

import OddJobs.Job (createJob)

import Data.Pool (withResource)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime, addUTCTime)
import Data.UUID qualified as UUID
import Models.Apis.Reports qualified as Reports
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Reports qualified as PageReports

import BackgroundJobs qualified
import BackgroundJobs (processFiveMinuteSpans)
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Simple.SqlQQ (sql)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Reports" do
    it "should toggle reports notifs" \tr -> do
      -- Get initial state
      (_, res1) <- testServant tr $ PageReports.reportsGetH testPid Nothing Nothing Nothing
      case res1 of
        PageReports.ReportsGetMain (PageCtx _ (_, _, _, daily1, weekly1)) -> do
          -- Initial state should be daily=True, weekly=True from testSessionHeader
          daily1 `shouldBe` True
          weekly1 `shouldBe` True
        _ -> error "Unexpected response"
        
      -- Toggle daily
      _ <-
        testServant tr $ PageReports.reportsPostH testPid "daily"
      -- Toggle weekly  
      _ <-
        testServant tr $ PageReports.reportsPostH testPid "weekly"
        
      -- Check final state - fetch project from DB to avoid cache
      projectM <- withPool tr.trPool $ Projects.projectById testPid
      case projectM of
        Just project -> do
          project.dailyNotif `shouldBe` False  -- toggled from True to False
          project.weeklyNotif `shouldBe` False -- toggled from True to False
        Nothing -> error "Project not found"
      
      -- Also check via the reports page
      (_, res) <- testServant tr $ PageReports.reportsGetH testPid Nothing Nothing Nothing
      case res of
        PageReports.ReportsGetMain (PageCtx _ (pid, reports, _, _, _)) -> do
          pid `shouldBe` testPid
          length reports `shouldBe` 0
        _ -> error "Unexpected response"

    -- it "should create and get all reports" \tr -> do
    --   -- Ensure the project has notifications enabled
    --   _ <- withPool tr.trPool $ DBT.execute
    --     [sql|UPDATE projects.projects SET weekly_notif = true, daily_notif = true WHERE id = ?|]
    --     (Only testPid)
    --
    --   currentTime <- getCurrentTime
    --   let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
    --   -- Update the messages to use the test project ID
    --   let updateProjectId :: AE.Value -> AE.Value
    --       updateProjectId v = case v of
    --         AE.Object obj -> AE.Object $ KeyMap.insert "project_id" (AE.String testPid.toText) obj
    --         _ -> v
    --   let reqMsg1 = Unsafe.fromJust $ convert $ updateProjectId $ testRequestMsgs.reqMsg1 nowTxt
    --   let reqMsg2 = Unsafe.fromJust $ convert $ updateProjectId $ testRequestMsgs.reqMsg2 nowTxt
    --
    --   let reqMsg3 = Unsafe.fromJust $ convert $ updateProjectId $ testRequestMsgs.reqMsg1 "2024-07-05T13:06:26.620094239Z"
    --   let reqMsg4 = Unsafe.fromJust $ convert $ updateProjectId $ testRequestMsgs.reqMsg2 "2024-07-05T12:06:26.620094239Z"
    --
    --   let msgs = concat (replicate 100 [("m1", BL.toStrict $ AE.encode reqMsg1), ("m2", BL.toStrict $ AE.encode reqMsg2)]) ++ [("m3", BL.toStrict $ AE.encode reqMsg3), ("m4", BL.toStrict $ AE.encode reqMsg4)]
    --   _ <- runTestBackground tr.trATCtx $ processMessages msgs HashMap.empty
    --
    --   -- Process the spans to generate endpoints and other data needed for reports
    --   -- Use a time slightly after the messages to ensure they're captured
    --   let processTime = addUTCTime 1 currentTime -- 1 second after the messages
    --   _ <- runTestBackground tr.trATCtx $ processFiveMinuteSpans processTime
    --
    --   -- Make sure there's data to report on by processing background jobs first
    --   createRequestDumps tr testPid 10
    --   processAllBackgroundJobsMultipleTimes tr
    --
    --   -- Create report jobs
    --   _ <- liftIO $ withResource tr.trPool \conn -> do
    --     _ <- createJob conn "background_jobs" $ BackgroundJobs.DailyReports testPid
    --     _ <- createJob conn "background_jobs" $ BackgroundJobs.WeeklyReports testPid
    --     pass
    --
    --   -- Run all background jobs to process the reports
    --   _ <- runAllBackgroundJobs tr.trATCtx
    --
    --   -- Additional background job processing to ensure reports are created
    --   processAllBackgroundJobsMultipleTimes tr
    --
    --   -- Check if reports were actually created in the database
    --   dbReports <- withPool tr.trPool $ Reports.reportHistoryByProject testPid 0
    --
    --
    --   length dbReports `shouldBe` 2
    --
    --   res <- testServant tr $ PageReports.reportsGetH testPid Nothing Nothing Nothing
    --   case res of
    --     PageReports.ReportsGetMain (PageCtx _ (pid, reports, next, daily, weekly)) -> do
    --       pid `shouldBe` testPid
    --       daily `shouldBe` True
    --       weekly `shouldBe` False
    --       length reports `shouldBe` 2
    --       let dailyReport = Unsafe.fromJust $ find (\r -> r.reportType == "daily") reports
    --       let weeklyReport = Unsafe.fromJust $ find (\r -> r.reportType == "weekly") reports
    --       dailyReport.reportType `shouldBe` "daily"
    --       weeklyReport.reportType `shouldBe` "weekly"
    --
    --       r <- testServant tr $ PageReports.singleReportGetH testPid dailyReport.id Nothing
    --       case r of
    --         PageReports.ReportsGetSingle (PageCtx _ (prid, reportM)) -> do
    --           prid `shouldBe` testPid
    --           isJust reportM `shouldBe` True
    --           let report = Unsafe.fromJust reportM
    --           report.id `shouldBe` dailyReport.id
    --           pass
    --         _ -> error "Unexpected response"
    --     _ -> error "Unexpected response"
