module BackgroundJobs.NotificationsSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, shouldBe, shouldSatisfy)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


-- Seed slack creds + enable error alerts so notify paths don't short-circuit.
-- Idempotent — callable from every test.
seedSlackChannel :: TestResources -> IO ()
seedSlackChannel tr = withResource tr.trPool \conn -> do
  void $ PGS.execute conn
    [sql| INSERT INTO apis.slack (project_id, webhook_url, team_id, channel_id, team_name, bot_token)
          VALUES (?, 'https://hooks.slack.com/test', 'T_TEST', 'C_TEST', 'TestTeam', 'xoxb-test')
          ON CONFLICT (project_id) DO UPDATE SET channel_id = 'C_TEST', bot_token = 'xoxb-test' |]
    (PGS.Only pid)
  void $ PGS.execute conn
    [sql| UPDATE projects.projects SET notifications_channel = '{slack}', error_alerts = true WHERE id = ? |]
    (PGS.Only pid)


spec :: Spec
spec = aroundAll withTestResources do
  describe "Error notification sweep + rate limiting" do

    it "1. Orphan patterns older than 1h are still picked up by the sweep (regression guard for 60-min cutoff bug)" \tr -> do
      seedSlackChannel tr
      apiKey <- createTestAPIKey tr pid "notif-orphan-key"
      -- Ingest an error 3h before frozenTime so processOneMinuteErrors's inline notify window (15 min) misses it.
      let stack = "TypeError: x is undefined\n    at f (/a.js:1:1)"
      ingestTraceWithException tr apiKey "GET /a" "TypeError" "x is undefined" stack (addUTCTime (-3 * 3600) frozenTime)
      drainExtractionWorker tr
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      -- Simulate the orphan state: last_notified_at=NULL (as happened in prod).
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| UPDATE apis.error_patterns SET last_notified_at = NULL WHERE project_id = ? |]
          (PGS.Only pid)

      (notifs, _) <- runTestBackgroundWithNotifications frozenTime tr.trLogger tr.trATCtx
        $ BackgroundJobs.sweepErrorSubscriptions pid
      notifs `shouldSatisfy` (not . null)

      -- last_notified_at must get stamped so the next sweep tick doesn't double-fire.
      stamped <- withResource tr.trPool \conn ->
        PGS.query conn
          [sql| SELECT COUNT(*)::INT FROM apis.error_patterns WHERE project_id = ? AND last_notified_at IS NOT NULL |]
          (PGS.Only pid) :: IO [PGS.Only Int]
      case stamped of
        [PGS.Only n] -> n `shouldSatisfy` (> 0)
        _ -> expectationFailure "stamped count query returned no row"

    it "2. Rate limit enforces 20/hr; overflow lands in notification_digest_queue" \tr -> do
      seedSlackChannel tr
      -- Purge any existing bookkeeping to keep this test deterministic.
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| DELETE FROM apis.notification_rate_limit WHERE project_id = ? |] (PGS.Only pid)
        void $ PGS.execute conn [sql| DELETE FROM apis.notification_digest_queue WHERE project_id = ? |] (PGS.Only pid)

      -- 22 consume attempts: first 20 succeed, last 2 are blocked.
      results <- runTestBg frozenTime tr $ forM ([1 .. 22] :: [Int]) \_ ->
        BackgroundJobs.consumeNotificationToken pid frozenTime
      length (filter id results) `shouldBe` 20
      length (filter not results) `shouldBe` 2

    it "3. Same Warp transport error from different stacks groups to one pattern (fingerprint regression)" \tr -> do
      apiKey <- createTestAPIKey tr pid "notif-transport-key"
      let msg = "Warp: Client closed connection prematurely"
          sA = "at Warp.run (Server.hs:42:1)\nat handle (/app/x.hs:7:3)"
          sB = "at Warp.run (Server.hs:42:1)\nat different_caller (/app/y.hs:9:5)"
      ingestTraceWithException tr apiKey "GET /route-a" "InvalidRequest" msg sA (addUTCTime (-120) frozenTime)
      ingestTraceWithException tr apiKey "POST /route-b" "InvalidRequest" msg sB (addUTCTime (-60) frozenTime)
      drainExtractionWorker tr
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 50 0
      let warpPats = filter (\p -> p.errorType == "InvalidRequest") patterns
      length warpPats `shouldBe` 1
