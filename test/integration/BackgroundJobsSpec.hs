-- | Scheduled work end to end: daily/weekly system reports (evidence, links, emails, the live
-- preview) → error alerts (sweep, toggles, acknowledgement, subscriptions, endpoint gate) →
-- the hourly notification budget and its digest → retention expiry → dashboard auto-provision
-- → the daily scheduler that seeds every per-project job, ReportUsage included.
module BackgroundJobsSpec (spec) where

import BackgroundJobs qualified
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket_)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Effectful.Notify (EmailData (..), Notification (..), SlackData (..))
import Data.List (lookup)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Notification qualified as PGN
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers qualified as Containers
import Network.HTTP.Types.URI (parseQueryText, urlDecode)
import OddJobs.Types (pgEventName)
import Pages.BodyWrapper (PageCtx (..))
import Pages.LogExplorer.Log qualified as Log
import Pages.Projects qualified as ProjectPages
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as Email
import Pkg.Parser (QueryComponents (..), SqlQueryCfg (..), defSqlQueryCfg, parseQueryToAST, parseQueryToComponents)
import Pkg.TestUtils
import Servant qualified
import System.Config qualified as Config
import System.Timeout qualified as Timeout
import UnliftIO.Async (concurrently)
import Utils (formatWithCommas)


spec :: Spec
spec = around withTestResources do
  it "error alerts: orphan sweep, settings toggle, acknowledgement window, aged subscription, one fingerprint, endpoint gate" \tr -> do
    seedSlackChannel tr
    saveAlertToggle tr (\form v -> form{ProjectPages.errorAlerts = v}) True
    apiKey <- createTestAPIKey tr testPid "notif-key"
    -- 3h back, so processOneMinuteErrors's 15-minute inline notify window misses it.
    ingestTraceWithException tr apiKey "GET /a" "TypeError" "x is undefined" "TypeError: x is undefined\n    at f (/a.js:1:1)" (addUTCTime (-3 * 3600) frozenTime)
    drainExtractionWorker tr
    void $ runAllBackgroundJobs frozenTime tr.trATCtx
    let clearNotified = dbExec tr [sql|UPDATE apis.error_patterns SET last_notified_at = NULL WHERE project_id = ?|] (Only testPid)
        sweep = clearNotified >> fst <$> captureNotifs tr (BackgroundJobs.sweepErrorSubscriptions testPid)
        delivered = sort . map (\case EmailNotification e -> e.receiver; SlackNotification{} -> "slack"; n -> show n) <$> sweep
        -- The incident is already open, so Slack gets a thread reply and its root edit, in one run.
        alerted = delivered `shouldReturn` ["hello@monoscope.tech", "slack", "slack", "test@monoscope.tech"]
    -- The orphan state seen in prod (last_notified_at NULL) is swept, and stamped so the next tick doesn't double-fire.
    alerted
    dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.error_patterns WHERE project_id = ? AND last_notified_at IS NOT NULL|] (Only testPid) `shouldReturn` 1
    saveAlertToggle tr (\form v -> form{ProjectPages.errorAlerts = v}) False
    sweep `shouldReturn` []
    saveAlertToggle tr (\form v -> form{ProjectPages.errorAlerts = v}) True
    -- A re-sweep at the same instant is the same incident event, so nothing new is sent.
    -- (A one-delivery-per-tick worker used to leak the first alert's root edit into this sweep.)
    delivered `shouldReturn` []

    -- An acknowledgement silences the issue for exactly its window, then hands it back with a trail.
    issueId <- dbOne @_ @Issues.IssueId tr [sql|SELECT id FROM apis.issues WHERE project_id = ? AND issue_type = 'runtime_exception'|] (Only testPid)
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
    runTestBg frozenTime tr $ void $ Issues.setAckState testPid [issueId] $ Just Issues.AckSet{at = frozenTime, by = Just uid, window = Issues.AckFor 60}
    sweep `shouldReturn` []
    advanceMinutes tr 61
    alerted
    runTestBgNoReset tr BackgroundJobs.expireLapsedAcks
    dbQuery tr [sql|SELECT i.acknowledged_at IS NULL, EXISTS (SELECT 1 FROM apis.issue_activity_log a WHERE a.issue_id = i.id AND a.event = 'ack_expired') FROM apis.issues i WHERE i.id = ?|] (Only issueId)
      `shouldReturn` [(True, True)]

    -- "Notify every N" died once its pattern was a day old: project selection bounded every row by created_at.
    -- created_at is compared against SQL now(), the cadence against the app clock.
    let later = addUTCTime 3660 frozenTime
    dbExec tr [sql|UPDATE apis.error_patterns SET created_at = now() - interval '3 days', subscribed = TRUE, notify_every_minutes = 30, last_notified_at = ? WHERE project_id = ?|] (addUTCTime (-3600) later, testPid)
    (subscribed, _) <- runTestBackgroundWithNotifications later tr.trLogger tr.trATCtx $ BackgroundJobs.runNotificationSweep later
    -- The reminder's thread reply and its root edit.
    [sd.threadTs | SlackNotification sd <- subscribed] `shouldSatisfy` \ts -> length ts == 2 && length (filter isJust ts) == 1

    -- Unsubscribed reminders for a recurring error decay: daily in its first week, weekly after, never past 30 days.
    hash <- dbOne @_ @Text tr [sql|SELECT target_hash FROM apis.issues WHERE id = ?|] (Only issueId)
    let remindAt t (ageDays, sinceHours) = do
          dbExec tr [sql|UPDATE apis.error_patterns SET subscribed = FALSE, state = 'ongoing', created_at = ?, last_notified_at = ? WHERE project_id = ? AND hash = ?|] (addUTCTime (-(ageDays * 86400)) t, addUTCTime (-(sinceHours * 3600)) t, testPid, hash)
          dbExec tr [sql|INSERT INTO apis.error_hourly_stats (project_id, error_id, hour_bucket, event_count, user_count) SELECT project_id, id, date_trunc('hour', ?::timestamptz), 1, 1 FROM apis.error_patterns WHERE project_id = ? AND hash = ? ON CONFLICT DO NOTHING|] (t, testPid, hash)
          void $ runTestBackgroundWithNotifications t tr.trLogger tr.trATCtx (BackgroundJobs.notifyErrorSubscriptions testPid (V.singleton hash))
          (== Just t) <$> dbOne @_ @(Maybe UTCTime) tr [sql|SELECT last_notified_at FROM apis.error_patterns WHERE project_id = ? AND hash = ?|] (testPid, hash)
    zipWithM remindAt [addUTCTime (fromIntegral h * 3600) later | h <- [2 :: Int ..]] [(3, 25), (10, 25), (10, 169), (31, 169)] `shouldReturn` [True, False, True, False]

    -- One Warp transport error from different stacks groups to one pattern.
    let warp = "Warp: Client closed connection prematurely"
    ingestTraceWithException tr apiKey "GET /route-a" "InvalidRequest" warp "at Warp.run (Server.hs:42:1)\nat handle (/app/x.hs:7:3)" (addUTCTime (-120) frozenTime)
    ingestTraceWithException tr apiKey "POST /route-b" "InvalidRequest" warp "at Warp.run (Server.hs:42:1)\nat different_caller (/app/y.hs:9:5)" (addUTCTime (-60) frozenTime)
    drainExtractionWorker tr
    void $ runAllBackgroundJobs frozenTime tr.trATCtx
    patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns testPid Nothing 50 0
    length (filter ((== "InvalidRequest") . (.errorType)) patterns) `shouldBe` 1

    -- With new-endpoint alerts off the issue was once stamped notified anyway, so re-enabling recovered nothing.
    let endpointAlerts = saveAlertToggle tr \form v -> form{ProjectPages.endpointAlerts = v}
        row = Email.EndpointAlertRow{label = "GET /gated", host = Nothing, service = Nothing, environment = Nothing}
    endpointAlerts False
    iid <- dbOne @_ @Issues.IssueId tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, endpoint_hash) VALUES (?, 'api_change', 'GET /gated', 'endpoint-gate-hash', 'endpoint-gate-hash') RETURNING id|] (Only testPid)
    let send = fst <$> captureNotifs tr (BackgroundJobs.sendNewEndpointAlerts testPid "endpoint-gate-hash" [(iid, row)])
        notified = dbOne @_ @(Maybe UTCTime) tr [sql|SELECT last_notified_at FROM apis.issues WHERE id = ?|] (Only iid)
    send `shouldReturn` []
    notified `shouldReturn` Nothing
    endpointAlerts True
    length <$> send `shouldReturn` 3
    isJust <$> notified `shouldReturn` True

  it "notification budget: 20 per hour, held for the hour, reset by the next, overflow digested on the test clock" \tr -> do
    let consume = runTestBgNoReset tr $ Time.currentTime >>= BackgroundJobs.consumeNotificationToken testPid
        digestRow title at = UUIDV4.nextRandom >>= \i -> dbExec tr [sql|INSERT INTO apis.notification_digest_queue (id, project_id, reason, title, created_at) VALUES (?, ?, 'rate_limit', ?, ?)|] (i, testPid, title :: Text, at)
    runTestBg frozenTime tr pass
    dbExec tr [sql|UPDATE projects.projects SET error_alerts = true WHERE id = ?|] (Only testPid)
    replicateM 22 consume `shouldReturn` replicate 20 True <> [False, False]
    replicateM_ 4 $ digestRow "t" frozenTime
    advanceMinutes tr 30
    consume `shouldReturn` False
    advanceMinutes tr 31
    consume `shouldReturn` True
    -- The 24h window only selects projects: once selected, every queued row flushes, stamped with
    -- the test clock rather than the wall clock.
    let now' = addUTCTime (61 * 60) frozenTime
    digestRow "too-old" (addUTCTime (-25 * 3600) now')
    runTestBgNoReset tr $ BackgroundJobs.runNotificationDigest now'
    dbQuery tr [sql|SELECT sent_at FROM apis.notification_digest_queue WHERE project_id = ?|] (Only testPid) `shouldReturn` replicate 5 (Only (Just now'))

  -- issue_type (an enum) and users.email (citext) decoded as text failed the step, and tryStep
  -- swallowed it: both sections vanished from every daily summary.
  it "admin daily summary posts its New Issues and Active Users sections" \tr -> do
    dbExec tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, created_at) VALUES (?, 'runtime_exception', 'summary probe', 'summary-probe', ?)|] (testPid, frozenTime)
    dbExec tr [sql|UPDATE users.persistent_sessions SET updated_at = ?|] (Only frozenTime)
    (posts, ()) <- runTestBgRecordingHTTP frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx BackgroundJobs.MonoscopeAdminDaily
    let body = T.concat [decodeUtf8 b | (_, b) <- posts]
    map (`T.isInfixOf` body) ["**New Issues**", "**Active Users**"] `shouldBe` [True, True]

  -- Share links keep 30d + 48h grace; replay data (no BYO bucket) keeps 30d. Both cut on the test clock.
  it "retention expires share events after 32 days and replay data after 30" \tr -> do
    share <- UUIDV4.nextRandom
    session <- UUIDV4.nextRandom
    runTestBg frozenTime tr pass
    eventId <- UUIDV4.nextRandom
    dbExec tr [sql|INSERT INTO apis.share_events (id, project_id, created_at, event_id, event_type, event_created_at) VALUES (?, ?, ?, ?, 'log', ?)|] (share, testPid, frozenTime, eventId, frozenTime)
    dbExec tr [sql|INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at) VALUES (?, ?, ?)|] (session, testPid, frozenTime)
    let expireAfter days = do
          advanceDays tr days
          for_ @[] [BackgroundJobs.ExpireShareEvents, BackgroundJobs.ExpireReplayData] $ runTestBgNoReset tr . BackgroundJobs.processBackgroundJob tr.trATCtx
          (,) <$> dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.share_events WHERE id = ?|] (Only share) <*> dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM projects.replay_sessions WHERE session_id = ?|] (Only session)
    expireAfter 29 `shouldReturn` (1, 1)
    expireAfter 2 `shouldReturn` (1, 0)
    expireAfter 2 `shouldReturn` (0, 0)

  it "auto-provision creates a dashboard per detected namespace once, adopts a manual one, and respects deletion" \tr -> do
    let seedMetric name = dbExec tr [sql|INSERT INTO otel_metrics_meta (project_id, metric_name, metric_type, first_seen_at, last_seen_at, first_timestamp, last_timestamp) VALUES (?, ?, 'SUM', now(), now(), now(), now()) ON CONFLICT DO NOTHING|] (testPid, name :: Text)
        provision = void $ runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.DashboardsAutoProvision frozenTime)
        -- Only this spec's templates, so fixtures like _overview.yaml can't interfere.
        provisioned = map fromOnly <$> dbQuery @_ @(Only Text) tr [sql|SELECT base_template FROM projects.dashboards WHERE project_id = ? AND base_template IN ('postgresql.yaml', 'redis.yaml', 'mysql.yaml') ORDER BY base_template|] (Only testPid)
    seedMetric "mysql.threads"
    dbExec tr [sql|INSERT INTO projects.dashboards (project_id, created_by, base_template, title) VALUES (?, '00000000-0000-0000-0000-000000000001', 'mysql.yaml', 'My MySQL')|] (Only testPid)
    seedMetric "postgresql.backends"
    provision
    provisioned `shouldReturn` ["mysql.yaml", "postgresql.yaml"]
    seedMetric "redis.memory.used"
    provision >> provision
    provisioned `shouldReturn` ["mysql.yaml", "postgresql.yaml", "redis.yaml"]
    dbExec tr [sql|DELETE FROM projects.dashboards WHERE project_id = ? AND base_template = 'postgresql.yaml'|] (Only testPid)
    provision
    provisioned `shouldReturn` ["mysql.yaml", "redis.yaml"]

  -- DailyJob seeds every per-project job, ReportUsage included, so when it stops firing billing
  -- stops with it. Prod ran it 08-24 and 08-26, not 08-25 or 08-27.
  it "the daily job is scheduled despite yesterday's spilled hourly jobs, never twice, wakes the listener, and a disabled worker leaves the queue alone" \tr -> do
    let queueAt tag offset = dbExec tr [sql|INSERT INTO background_jobs (run_at, status, payload) VALUES (date_trunc('day', now()) + ?::interval, 'queued', jsonb_build_object('tag', ?))|] (offset :: Text, tag :: Text)
        dailyJobs = dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM background_jobs WHERE payload->>'tag' = 'DailyJob'|] ()
    -- HourlyJobs are seeded from now(), not the day, so a 17:43 run leaves rows up to 16:43
    -- tomorrow; the guard read those as proof today's DailyJob ran.
    queueAt "HourlyJob" "13 hours"
    queueAt "HourlyJob" "16 hours"
    BackgroundJobs.ensureDailyJobScheduled tr.trATCtx
    dailyJobs `shouldReturn` 1
    -- odd-jobs deletes the completed row, so without this the 30-minute backstop re-inserts every half hour.
    BackgroundJobs.ensureDailyJobScheduled tr.trATCtx
    dailyJobs `shouldReturn` 1
    withResource tr.trPool \listener -> do
      let channel = Only $ pgEventName "background_jobs"
      bracket_ (PGS.execute listener "LISTEN ?" channel) (PGS.execute listener "UNLISTEN ?" channel) do
        queueAt "DailyJob" "-1 hour"
        fmap PGN.notificationChannel <$> Timeout.timeout 5_000_000 (PGN.getNotification listener) `shouldReturn` Just "jobs_created_background_jobs"
    let disabled = tr.trATCtx{Config.config = tr.trATCtx.config{Config.enableBackgroundJobs = False, Config.enableDailyJobScheduling = False}}
    Timeout.timeout 1_000_000 (BackgroundJobs.jobsWorkerInit tr.trLogger disabled tr.trTracerProvider) `shouldReturn` Just ()
    dailyJobs `shouldReturn` 2

  -- A session lock taken and released by separate pool checkouts only worked while both landed on
  -- one server session; a lost unlock would skip every later day, usage metering included.
  it "dailyJob_releasesItsSchedulingLock_soTheNextRunSchedules" \tr -> do
    let enabled = tr.trATCtx{Config.config = tr.trATCtx.config{Config.enableDailyJobScheduling = True}}
        runDaily = void $ runTestBg frozenTime tr $ BackgroundJobs.processBackgroundJob enabled BackgroundJobs.DailyJob
        dailyJobs = dbOne @_ @Int tr [sql|SELECT count(*)::int FROM background_jobs WHERE payload->>'tag' = 'DailyJob'|] ()
        held = dbOne @_ @Int tr [sql|SELECT count(*)::int FROM pg_locks WHERE locktype = 'advisory' AND objsubid = 1 AND objid = (hashtext('daily_job_scheduling')::int8 & 4294967295)::oid|] ()
        lockAs conn f = void (PGS.query conn [sql|SELECT pg_advisory_lock(hashtext('daily_job_scheduling'))::text|] () :: IO [Only Text]) >> f >> void (PGS.query conn [sql|SELECT pg_advisory_unlock(hashtext('daily_job_scheduling'))|] () :: IO [Only Bool])
    before <- dailyJobs
    -- Another pod holding the lock: this one schedules nothing.
    withResource tr.trPool \other -> lockAs other runDaily
    dailyJobs `shouldReturn` before
    runDaily
    dailyJobs `shouldReturn` before + 1
    held `shouldReturn` 0
    dbExec tr [sql|DELETE FROM background_jobs WHERE payload->>'tag' = 'DailyJob'|] ()
    runDaily
    dailyJobs `shouldReturn` 1

  it "dual-exec: the TimeFusion arm outwaits a deploy drain instead of dropping the UPDATE" \tr -> do
    -- Two drain refusals, then the write lands: what TimeFusion returns across a handoff.
    dbExec
      tr
      [sql|CREATE SEQUENCE tf_drain_calls; CREATE TABLE tf_drain_target (n int); INSERT INTO tf_drain_target VALUES (0);
           CREATE FUNCTION tf_drain_probe() RETURNS int LANGUAGE plpgsql AS $$ BEGIN
             IF nextval('tf_drain_calls') <= 2 THEN
               RAISE EXCEPTION 'TimeFusion is draining for deployment; retry on the replacement' USING ERRCODE = 'XX000';
             END IF;
             RETURN 1;
           END $$|]
      ()
    let ctx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.enableTimefusionWrites = True}}
    void $ runTestBg frozenTime tr $ BackgroundJobs.dualExecPgTfWithSql ctx [HI.sql|UPDATE tf_drain_target SET n = n|] [HI.sql|UPDATE tf_drain_target SET n = tf_drain_probe()|]
    dbOne @() @Int tr [sql|SELECT n FROM tf_drain_target|] () `shouldReturn` 1


-- Round-trips the real settings form so the toggle is written the way the UI writes it.
saveAlertToggle :: TestResources -> (ProjectPages.CreateProjectForm -> Maybe Text -> ProjectPages.CreateProjectForm) -> Bool -> IO ()
saveAlertToggle tr set enabled = do
  ProjectPages.CreateProject (PageCtx _ (_, _, _, _, _, form, _, _)) <- call tr $ ProjectPages.projectSettingsGetH testPid
  void $ testServant tr $ ProjectPages.createProjectPostH testPid $ set form $ bool Nothing (Just "on") enabled
