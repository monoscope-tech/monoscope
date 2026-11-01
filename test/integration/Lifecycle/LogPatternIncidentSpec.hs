module Lifecycle.LogPatternIncidentSpec (spec) where

import BackgroundJobs qualified
import Control.Exception (try)
import Data.Effectful.Notify (Notification (..))
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Pages.Projects qualified as ProjectPages
import Pkg.TestUtils
import Servant qualified


spec :: Spec
spec = around withTestResources do
  describe "Log Pattern Incident Lifecycle" do
    it "updates baselines across batches without touching merged or inactive patterns" \tr -> do
      runTestBg frozenTime tr pass
      void
        $ dbExec
          tr
          [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, first_seen_at)
              SELECT ?, 'baseline fixture', 'batch-' || n, 'summary', ? FROM generate_series(1,1001) n |]
          (testPid, frozenTime)
      void
        $ dbExec
          tr
          [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, first_seen_at)
              SELECT ?, 'baseline fixture', hash, field, ? FROM
              (VALUES ('batch-1','body'), ('no-stats','summary'), ('expired','summary'), ('merged','summary')) v(hash,field) |]
          (testPid, frozenTime)
      void
        $ dbExec
          tr
          [sql| UPDATE apis.log_patterns SET canonical_id =
              (SELECT id FROM apis.log_patterns WHERE project_id = ? AND pattern_hash='batch-1' AND source_field='summary')
              WHERE project_id = ? AND pattern_hash='merged' |]
          (testPid, testPid)
      void
        $ dbExec
          tr
          [sql| INSERT INTO apis.log_pattern_hourly_stats (project_id, source_field, pattern_hash, hour_bucket, event_count)
              SELECT project_id, source_field, pattern_hash,
                ?::timestamptz - CASE WHEN pattern_hash='expired' THEN interval '49 hours' ELSE interval '1 hour' END,
                CASE WHEN source_field='body' THEN 100 ELSE 600 END
              FROM apis.log_patterns WHERE project_id = ? AND pattern_hash <> 'no-stats' |]
          (frozenTime, testPid)
      runTestBg frozenTime tr $ BackgroundJobs.calculateLogPatternBaselines testPid
      actual <-
        dbQuery
          tr
          [sql| SELECT source_field, baseline_state, baseline_volume_hourly_mean, baseline_samples, count(*)::bigint
                FROM apis.log_patterns WHERE project_id = ?
                GROUP BY 1,2,3,4 ORDER BY 1,2,3 NULLS LAST |]
          (Only testPid)
          :: IO [(Text, Text, Maybe Double, Int, Int64)]
      actual `shouldBe` [("body", "learning", Just 100, 1, 1), ("summary", "established", Just 600, 1, 1001), ("summary", "learning", Nothing, 0, 3)]

    it "ingest -> baseline -> spike -> notify -> ack for 24h -> re-spike suppressed -> ack expires -> re-spike fires" \tr -> do
      runTestBg frozenTime tr pass

      let patHash = "incident-lifecycle-hash" :: Text
          srcField = "summary" :: Text
          uid = (Servant.getResponse tr.trSessAndHeader).user.id

      -- 1. Seed pattern + 50h of stable baseline at 600/hr
      void
        $ runTestBg frozenTime tr
        $ LogPatterns.upsertLogPattern
          LogPatterns.UpsertPattern
            { projectId = testPid
            , logPattern = "Lifecycle <*> pattern"
            , hash = patHash
            , sourceField = srcField
            , serviceName = Just "test-svc"
            , logLevel = Just "ERROR"
            , traceId = Nothing
            , sampleMessage = Just "lifecycle pattern"
            , eventCount = 600
            , isError = True
            }
      forM_ ([-50 .. -1] :: [Int]) \h ->
        void
          $ runTestBg frozenTime tr
          $ LogPatterns.upsertHourlyStat
            testPid
            srcField
            patHash
            (addUTCTime (fromIntegral h * 3600) frozenTime)
            600
      runTestBg frozenTime tr $ BackgroundJobs.calculateLogPatternBaselines testPid

      -- 2. Spike in the current hour: raw 600 -> projected 600*4 = 2400 vs baseline ~600
      void $ runTestBg frozenTime tr $ LogPatterns.upsertHourlyStat testPid srcField patHash frozenTime 600
      runTestBg frozenTime tr $ BackgroundJobs.detectLogPatternSpikes testPid frozenTime tr.trATCtx
      runTestBg frozenTime tr $ BackgroundJobs.detectLogPatternSpikes testPid frozenTime tr.trATCtx

      issues1 <-
        dbQuery
          tr
          [sql| SELECT id FROM apis.issues
                WHERE project_id = ? AND target_hash = ? AND issue_type = 'log_pattern_rate_change'
                  AND acknowledged_at IS NULL
                ORDER BY created_at DESC LIMIT 1 |]
          (testPid, patHash)
          :: IO [Only Issues.IssueId]
      let issueId = case issues1 of (Only iid : _) -> iid; _ -> error "no issue fired on initial spike"

      -- 3. Acknowledge for 24h -> silenced for exactly that window
      tAck <- getTestTime tr.trTestClock
      runTestBgNoReset tr $ void $ Issues.setAckState testPid [issueId] $ Just Issues.AckSet{at = tAck, by = Just uid, window = Issues.AckFor (24 * 60)}

      -- 4. +12h: seed fresh spike in the new hour; the ack should suppress it
      advanceHours tr 12
      t12 <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ LogPatterns.upsertHourlyStat testPid srcField patHash t12 600
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes testPid t12 tr.trATCtx
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes testPid t12 tr.trATCtx
      countAfter12 <- countOpenIssues tr patHash
      countAfter12 `shouldBe` 0

      -- 5. +25h since ack: the window lapsed, a new spike fires again
      advanceHours tr 13
      t25 <- getTestTime tr.trTestClock
      void $ runTestBgNoReset tr $ LogPatterns.upsertHourlyStat testPid srcField patHash t25 600
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes testPid t25 tr.trATCtx
      runTestBgNoReset tr $ BackgroundJobs.detectLogPatternSpikes testPid t25 tr.trATCtx
      countAfter25 <- countOpenIssues tr patHash
      countAfter25 `shouldSatisfy` (>= 1)

  describe "Deleted Project Notification Lifecycle" do
    it "stops ingest, reports, and alerts after the customer deletes the project" \tr -> do
      let monitorId = Monitors.QueryMonitorId $ fromMaybe (error "invalid test UUID") $ UUID.fromText "de1e7ed0-0000-0000-0000-000000000001"
          monitor =
            def
              { Monitors.id = monitorId
              , Monitors.projectId = testPid
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

      apiKey <- createTestAPIKey tr testPid "deleted-project-ingest"
      ingestLog tr apiKey "before project deletion" frozenTime
      countLogs tr "before project deletion" `shouldReturn` 1

      void $ runTestBg frozenTime tr $ Monitors.queryMonitorUpsert monitor
      dbExec
        tr
        [sql|
            UPDATE projects.teams
            SET pagerduty_services = ARRAY['deleted-project-test'],
                disabled_channels = array_remove(disabled_channels, 'pagerduty')
            WHERE project_id = ? AND handle = 'everyone'
          |]
        (Only testPid)

      before <- fst <$> captureNotifs tr BackgroundJobs.checkTriggeredQueryMonitors
      before `shouldSatisfy` any (\case PagerdutyNotification{} -> True; _ -> False)

      void $ testServant tr $ ProjectPages.deleteProjectGetH testPid
      void $ try @SomeException $ ingestLog tr apiKey "after project deletion" (addUTCTime 1 frozenTime)
      countLogs tr "after project deletion" `shouldReturn` 0
      isNothing <$> runTestBgNoReset tr (Projects.activeProjectById testPid) `shouldReturn` True
      reportsBefore <- countReports tr
      reportNotifs <- fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports testPid)
      reportNotifs `shouldBe` []
      countReports tr `shouldReturn` reportsBefore
      dbExec
        tr
        [sql|UPDATE monitors.query_monitors
                SET current_status = 'normal', alert_last_triggered = NULL,
                    notification_count = 0, last_evaluated = '2020-01-01'
                WHERE id = ?|]
        (Only monitorId)

      after <- fst <$> captureNotifs tr BackgroundJobs.checkTriggeredQueryMonitors
      after `shouldBe` []


countOpenIssues :: TestResources -> Text -> IO Int
countOpenIssues tr h =
  dbOne @_ @Int
    tr
    [sql| SELECT COUNT(*)::INT FROM apis.issues
          WHERE project_id = ? AND target_hash = ? AND issue_type = 'log_pattern_rate_change'
            AND acknowledged_at IS NULL |]
    (testPid, h)


countLogs :: TestResources -> Text -> IO Int
countLogs tr marker =
  dbOne @_ @Int
    tr
    [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans
          WHERE project_id = ? AND body::text LIKE '%' || ? || '%' |]
    (testPid, marker)


countReports :: TestResources -> IO Int
countReports tr = dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.reports WHERE project_id = ?|] (Only testPid)


