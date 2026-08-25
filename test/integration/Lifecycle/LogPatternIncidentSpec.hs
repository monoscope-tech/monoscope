module Lifecycle.LogPatternIncidentSpec (spec) where

import BackgroundJobs qualified
import Control.Exception (try)
import Data.Default (def)
import Data.Effectful.Notify (Notification (..))
import Data.Pool (withResource)
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pages.Projects qualified as ProjectPages
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant qualified
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


spec :: Spec
spec = around withTestResources do
  describe "Log Pattern Incident Lifecycle" do
    it "ingest -> baseline -> spike -> notify -> ack for 24h -> re-spike suppressed -> ack expires -> re-spike fires" \tr -> do
      runTestBg frozenTime tr pass

      let patHash = "incident-lifecycle-hash" :: Text
          srcField = "summary" :: Text
          uid = (Servant.getResponse tr.trSessAndHeader).user.id

      -- 1. Seed pattern + 50h of stable baseline at 600/hr
      void $ runTestBg frozenTime tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
        { projectId = pid, logPattern = "Lifecycle <*> pattern", hash = patHash
        , sourceField = srcField, serviceName = Just "test-svc", logLevel = Just "ERROR"
        , traceId = Nothing, sampleMessage = Just "lifecycle pattern", eventCount = 600
        , isError = True }
      forM_ ([-50 .. -1] :: [Int]) \h ->
        void $ runTestBg frozenTime tr $ LogPatterns.upsertHourlyStat pid srcField patHash
          (addUTCTime (fromIntegral h * 3600) frozenTime) 600
      runTestBg frozenTime tr $ BackgroundJobs.calculateLogPatternBaselines pid

      -- 2. Spike in the current hour: raw 600 -> projected 600*4 = 2400 vs baseline ~600
      void $ runTestBg frozenTime tr $ LogPatterns.upsertHourlyStat pid srcField patHash frozenTime 600
      runTestBg frozenTime tr $ BackgroundJobs.detectLogPatternSpikes pid frozenTime tr.trATCtx
      runTestBg frozenTime tr $ BackgroundJobs.detectLogPatternSpikes pid frozenTime tr.trATCtx

      issues1 <- withResource tr.trPool \conn ->
        PGS.query conn
          [sql| SELECT id FROM apis.issues
                WHERE project_id = ? AND target_hash = ? AND issue_type = 'log_pattern_rate_change'
                  AND acknowledged_at IS NULL
                ORDER BY created_at DESC LIMIT 1 |]
          (pid, patHash) :: IO [Only Issues.IssueId]
      let issueId = case issues1 of (Only iid : _) -> iid; _ -> error "no issue fired on initial spike"

      -- 3. Acknowledge for 24h -> silenced for exactly that window
      tAck <- getTestTime tr.trTestClock
      runTestBgNoReset tr $ void $ Issues.setAckState pid [issueId] $ Just Issues.AckSet{at = tAck, by = Just uid, window = Issues.AckFor (24 * 60)}

      -- 4. +12h: seed fresh spike in the new hour; the ack should suppress it
      advanceHours tr 12
      t12 <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ LogPatterns.upsertHourlyStat pid srcField patHash t12 600
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t12 tr.trATCtx
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t12 tr.trATCtx
      countAfter12 <- countOpenIssues tr patHash
      countAfter12 `shouldBe` 0

      -- 5. +25h since ack: the window lapsed, a new spike fires again
      advanceHours tr 13
      t25 <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ LogPatterns.upsertHourlyStat pid srcField patHash t25 600
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t25 tr.trATCtx
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes pid t25 tr.trATCtx
      countAfter25 <- countOpenIssues tr patHash
      countAfter25 `shouldSatisfy` (>= 1)

  describe "Deleted Project Notification Lifecycle" do
    it "stops ingest, reports, and alerts after the customer deletes the project" \tr -> do
      let monitorId = Monitors.QueryMonitorId $ fromMaybe (error "invalid test UUID") $ UUID.fromText "de1e7ed0-0000-0000-0000-000000000001"
          monitor =
            def
              { Monitors.id = monitorId
              , Monitors.projectId = pid
              , -- A real KQL query, not a canned logQueryAsSql: evaluateQueryMonitor
                -- re-parses logQuery on every run so parser fixes reach existing
                -- monitors, so the stored SQL is never what actually executes. An empty
                -- logQuery parses to no alert SQL and the evaluation throws before it can
                -- notify — which is silent, because the throw is caught and logged.
                Monitors.logQuery = "summarize count()"
              , Monitors.logQueryAsSql = ""
              , -- One log is ingested before the project is deleted, so a threshold of 0
                -- is what makes the monitor fire on the way in.
                Monitors.alertThreshold = 0
              , Monitors.checkIntervalMins = 1
              , -- Not from `def`: that is 0, and migration 0135 added
                -- query_monitors_positive_time_window, so a def-constructed monitor no
                -- longer inserts. Every production path sets this explicitly and rejects
                -- a non-positive value at the API boundary (Web.ApiHandlers).
                Monitors.timeWindowMins = 60
              , Monitors.alertConfig = def{Monitors.title = "Deleted project guard"}
              }

      apiKey <- createTestAPIKey tr pid "deleted-project-ingest"
      ingestLog tr apiKey "before project deletion" frozenTime
      countLogs tr "before project deletion" `shouldReturn` 1

      void $ runTestBg frozenTime tr $ Monitors.queryMonitorUpsert monitor
      void $ withResource tr.trPool \conn ->
        PGS.execute conn
          [sql|
            UPDATE projects.teams
            SET pagerduty_services = ARRAY['deleted-project-test'],
                disabled_channels = array_remove(disabled_channels, 'pagerduty')
            WHERE project_id = ? AND handle = 'everyone'
          |]
          (Only pid)

      before <- fst <$> captureNotifs tr BackgroundJobs.checkTriggeredQueryMonitors
      before `shouldSatisfy` any (\case PagerdutyNotification{} -> True; _ -> False)

      void $ testServant tr $ ProjectPages.deleteProjectGetH pid
      void $ try @SomeException $ ingestLog tr apiKey "after project deletion" (addUTCTime 1 frozenTime)
      countLogs tr "after project deletion" `shouldReturn` 0
      isNothing <$> runTestBgNoReset tr (Projects.activeProjectById pid) `shouldReturn` True
      reportsBefore <- countReports tr
      reportNotifs <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports pid)
      reportNotifs `shouldBe` []
      countReports tr `shouldReturn` reportsBefore
      void $ withResource tr.trPool \conn ->
        PGS.execute conn
          [sql|UPDATE monitors.query_monitors
                SET current_status = 'normal', alert_last_triggered = NULL,
                    notification_count = 0, last_evaluated = '2020-01-01'
                WHERE id = ?|]
          (Only monitorId)

      after <- fst <$> captureNotifs tr BackgroundJobs.checkTriggeredQueryMonitors
      after `shouldBe` []


countOpenIssues :: TestResources -> Text -> IO Int
countOpenIssues tr h = countQuery tr
    [sql| SELECT COUNT(*)::INT FROM apis.issues
          WHERE project_id = ? AND target_hash = ? AND issue_type = 'log_pattern_rate_change'
            AND acknowledged_at IS NULL |]
    (pid, h)


countLogs :: TestResources -> Text -> IO Int
countLogs tr marker = countQuery tr
    [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
          WHERE project_id = ? AND body::text LIKE '%' || ? || '%' |]
    (pid, marker)


countReports :: TestResources -> IO Int
countReports tr = countQuery tr [sql|SELECT COUNT(*)::INT FROM apis.reports WHERE project_id = ?|] (Only pid)


countQuery :: PGS.ToRow q => TestResources -> PGS.Query -> q -> IO Int
countQuery tr query params = withResource tr.trPool \conn -> do
  [Only n] <- PGS.query conn query params
  pure n
