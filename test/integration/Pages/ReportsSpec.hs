module Pages.ReportsSpec (spec) where

import BackgroundJobs qualified
import Data.Effectful.Notify (Notification (..))
import Data.Pool (withResource)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Issues (ReportListItem (..))
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Reports qualified as Reports
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec


spec :: Spec
spec = around withTestResources do
  describe "Report Notification Lifecycle" do
    it "delivers enabled daily and weekly reports, records a muted report, and isolates report details" \tr -> do
      (_, initialPage) <- testServant tr $ Reports.reportsGetH testPid Nothing Nothing Nothing
      case initialPage of
        Reports.ReportsGetMain (PageCtx _ (pid, _, _)) -> pid `shouldBe` testPid
        _ -> fail "the reports page did not load"
      initial <- runTestBg frozenTime tr $ Projects.projectById testPid
      ((.dailyNotif) <$> initial, (.weeklyNotif) <$> initial) `shouldBe` (Just False, Just True)

      void $ testServant tr $ Reports.reportsPostH testPid "daily"
      void $ testServant tr $ Reports.reportsPostH testPid "weekly"
      void $ withResource tr.trPool \conn ->
        PGS.execute conn
          [sql|UPDATE projects.teams
                SET discord_channels = ARRAY['daily-reports'], disabled_channels = '{}'
                WHERE project_id = ? AND handle = 'everyone'|]
          (PGS.Only testPid)

      sent <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports testPid)
      sent `shouldSatisfy` any (\case DiscordNotification{} -> True; _ -> False)

      void $ testServant tr $ Reports.reportsPostH testPid "daily"
      advanceDays tr 1
      muted <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports testPid)
      muted `shouldBe` []

      void $ testServant tr $ Reports.reportsPostH testPid "weekly"
      weekly <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.WeeklyReports testPid)
      weekly `shouldSatisfy` any (\case DiscordNotification{} -> True; _ -> False)

      (_, reportsPage) <- testServant tr $ Reports.reportsGetH testPid Nothing Nothing Nothing
      reportId <- case reportsPage of
        Reports.ReportsGetMain (PageCtx _ (_, reports, _)) -> do
          V.length reports `shouldBe` 3
          V.any ((== "weekly") . (.reportType)) reports `shouldBe` True
          maybe (fail "the daily report was not listed") (pure . (.id)) $ V.find ((== "daily") . (.reportType)) reports
        _ -> fail "the reports page did not load after generation"
      (_, reportPage) <- testServant tr $ Reports.singleReportGetH testPid reportId Nothing
      case reportPage of
        Reports.ReportsGetSingle (PageCtx _ (reportType, dateLabel, emailHtml)) -> do
          reportType `shouldBe` "daily"
          dateLabel `shouldNotBe` ""
          emailHtml `shouldNotBe` ""
        _ -> fail "the report detail did not load"

      let otherPid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0
      (_, otherProjectPage) <- testServant tr $ Reports.singleReportGetH otherPid reportId Nothing
      case otherProjectPage of
        Reports.ReportsGetSingle (PageCtx _ (reportType, dateLabel, emailHtml)) ->
          (reportType, dateLabel, emailHtml) `shouldBe` ("unknown", "Report not found", "")
        _ -> fail "the other project report page did not load"
