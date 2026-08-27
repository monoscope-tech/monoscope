-- | Regression guard for daily scheduling drift.
--
-- @ensureDailyJobScheduled@ is what puts a @DailyJob@ on the queue, and @DailyJob@ is what
-- seeds every per-project job including @ReportUsage@ — so when it stops firing, billing
-- stops with it, silently. Production drifted to running it on 08-24 and 08-26 and not at
-- all on 08-25 or 08-27.
module BackgroundJobs.DailyScheduleSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, around, describe, it, shouldBe)


-- | Queue a job by tag at an offset from midnight today, mimicking a row the scheduler left
-- behind on an earlier run.
queueAt :: TestResources -> Text -> Text -> IO ()
queueAt tr tag offset = withResource tr.trPool \conn ->
  void
    $ PGS.execute
      conn
      [sql| INSERT INTO background_jobs (run_at, status, payload)
            VALUES (date_trunc('day', now()) + ?::interval, 'queued', jsonb_build_object('tag', ?)) |]
      (offset, tag)


countTag :: TestResources -> Text -> IO Int
countTag tr tag = withResource tr.trPool \conn -> do
  [PGS.Only n] <-
    PGS.query
      conn
      [sql| SELECT COUNT(*)::INT FROM background_jobs WHERE payload->>'tag' = ? |]
      (PGS.Only tag)
  pure n


spec :: Spec
spec = around withTestResources do
  describe "ensureDailyJobScheduled" do
    -- THE BUG. runDailyJobScheduling seeds 24 HourlyJobs starting at `now()`, not aligned to
    -- the day, so a run at 17:43 leaves rows dated up to 16:43 *tomorrow*. The guard treats
    -- any HourlyJob dated today as proof that today's DailyJob already happened, so those
    -- spilled rows suppress it — and because each run drifts later, the suppression repeats.
    -- Measured in production 2026-08-27: HourlyJobs created 08-26 17:43:59 with run_at up to
    -- 08-27 16:43, zero DailyJob rows, and ReportUsage last created 08-24.
    it "dailyJob_isScheduled_evenWhenYesterdaysHourlyJobsSpillIntoToday" \tr -> do
      queueAt tr "HourlyJob" "13 hours"
      queueAt tr "HourlyJob" "16 hours"
      BackgroundJobs.ensureDailyJobScheduled tr.trATCtx
      countTag tr "DailyJob" >>= (`shouldBe` 1)

    -- The other half of the same guard: a DailyJob already queued for today must suppress a
    -- second one. Without this the 30-minute backstop thread re-inserts one every half hour
    -- after each successful run, because odd-jobs deletes the completed row.
    it "dailyJob_isNotDuplicated_whenOneIsAlreadyQueuedToday" \tr -> do
      queueAt tr "DailyJob" "9 hours"
      BackgroundJobs.ensureDailyJobScheduled tr.trATCtx
      countTag tr "DailyJob" >>= (`shouldBe` 1)
