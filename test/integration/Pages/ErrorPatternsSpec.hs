module Pages.ErrorPatternsSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.ErrorPatterns (ErrorState (..))
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (BaselineState (..))
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.Anomalies qualified
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.ErrorFingerprint qualified as EF
import Pkg.TestUtils
import Relude
import Servant qualified
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)
import Text.Read (read)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


frozenTime :: UTCTime
frozenTime = read "2025-01-01 00:00:00 UTC"


countIssues :: TestResources -> Issues.IssueType -> IO Int
countIssues tr issueType = withResource tr.trPool \conn -> do
  [PGS.Only n] <- PGS.query conn
    [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE project_id = ? AND issue_type = ? |]
    (pid, issueType)
  pure n


spec :: Spec
spec = aroundAll withTestResources do
  describe "Error Pattern Pipeline" do

    it "1. Ingest spans with exceptions → extract error patterns" \tr -> do
      apiKey <- createTestAPIKey tr pid "error-test-key"
      let nodeStack = "TypeError: Cannot read properties of undefined (reading 'id')\n    at UserController.getUser (/app/src/controllers/user.js:42:15)\n    at Layer.handle (/app/node_modules/express/lib/router/layer.js:95:5)"
          connStack = "ConnectionRefusedError: connect ECONNREFUSED 127.0.0.1:5432\n    at TCPConnectWrap.afterConnect (/app/node_modules/pg/lib/connection.js:79:18)"
      -- Span A: TypeError
      ingestTraceWithException tr apiKey "GET /api/users/:id" "TypeError" "Cannot read properties of undefined (reading 'id')" nodeStack (addUTCTime (-30) frozenTime)
      -- Span B: Same error (should dedup to same pattern)
      ingestTraceWithException tr apiKey "GET /api/users/:id" "TypeError" "Cannot read properties of undefined (reading 'id')" nodeStack (addUTCTime (-20) frozenTime)
      -- Span C: Different error type
      ingestTraceWithException tr apiKey "GET /api/users/:id" "ConnectionRefusedError" "connect ECONNREFUSED 127.0.0.1:5432" connStack (addUTCTime (-10) frozenTime)

      -- Process errors in the [frozenTime-2min, frozenTime) window
      runTestBg tr $ BackgroundJobs.processOneMinuteErrors frozenTime pid

      patterns <- runTestBg tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      length patterns `shouldBe` 2

      -- Verify hourly stats exist
      statsCount <- withResource tr.trPool \conn -> do
        [PGS.Only n] <- PGS.query conn
          [sql| SELECT COUNT(*)::INT FROM apis.error_hourly_stats WHERE project_id = ? |] (PGS.Only pid)
        pure (n :: Int)
      statsCount `shouldSatisfy` (> 0)

      -- Verify pattern fields populated
      forM_ patterns \p -> do
        p.errorType `shouldSatisfy` (/= "")
        p.message `shouldSatisfy` (/= "")
        p.stacktrace `shouldSatisfy` (/= "")

    it "2. NewErrorDetected creates RuntimeException issues" \tr -> do
      -- DB trigger on INSERT already fired during test 1
      void $ runAllBackgroundJobs tr.trATCtx
      issueCount <- countIssues tr Issues.RuntimeException
      issueCount `shouldSatisfy` (> 0)

    it "3. Resolved → Regressed on new occurrence" \tr -> do
      apiKey <- createTestAPIKey tr pid "error-regression-key"
      -- Find a pattern to resolve
      patterns <- runTestBg tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      let pat = fromMaybe (error "no patterns") $ listToMaybe patterns
      void $ runTestBg tr $ ErrorPatterns.resolveErrorPattern pat.id
      resolvedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.state) resolvedPat `shouldBe` Just ESResolved

      -- Ingest same error again → should regress
      ingestTraceWithException tr apiKey "GET /api/users/:id" pat.errorType pat.message pat.stacktrace (addUTCTime 60 frozenTime)
      runTestBg tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 120 frozenTime) pid

      regressedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.state) regressedPat `shouldBe` Just ESRegressed
      fmap (isJust . (.regressedAt)) regressedPat `shouldBe` Just True

    it "4. Baseline calculation from hourly stats" \tr -> do
      patterns <- runTestBg tr $ ErrorPatterns.getErrorPatterns pid Nothing 1 0
      let pat = fromMaybe (error "no patterns") $ listToMaybe patterns
      -- Seed 50 hours of stats (exceeds 24h minimum) with variance so stddev > 0
      forM_ ([-50 .. -1] :: [Int]) \h ->
        void $ runTestBg tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid
          (addUTCTime (fromIntegral h * 3600) frozenTime)
          (V.singleton (pat.hash, 600 + (h `mod` 7) * 10, 10))

      runTestBg tr $ BackgroundJobs.calculateErrorBaselines pid

      updatedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      case updatedPat of
        Just p -> do
          p.baselineState `shouldBe` BSEstablished
          isJust p.baselineErrorRateMean `shouldBe` True
          isJust p.baselineErrorRateStddev `shouldBe` True
          p.baselineSamples `shouldSatisfy` (>= 24)
        Nothing -> error "Pattern not found after baseline calculation"

    it "5. Spike detection creates escalating issue" \tr -> do
      let sess = Servant.getResponse tr.trSessAndHeader
      -- Un-resolve established patterns (test 3 resolved them)
      errRates <- runTestBg tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      forM_ errRates \r ->
        when (r.baselineState == BSEstablished && r.state == ESResolved) $
          void $ runTestBg tr $ ErrorPatterns.updateErrorPatternState r.errorId ESOngoing

      -- Acknowledge existing issues so ON CONFLICT doesn't deduplicate the new spike issue
      (issues, _) <- runTestBg tr $ Issues.selectIssues pid Nothing (Just False) Nothing 100 0 Nothing Nothing
      forM_ issues \issue -> runTestBg tr $ Issues.acknowledgeIssue issue.id sess.user.id

      -- Find an established pattern with stddev > 0
      errRates' <- runTestBg tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      let established = find (\r -> r.baselineState == BSEstablished && isJust r.baselineMean && maybe False (> 0) r.baselineStddev && r.state /= ESResolved) errRates'

      case established of
        Just errRate -> do
          let mean = fromMaybe 0 errRate.baselineMean
              stddev = fromMaybe 0 errRate.baselineStddev
              spikeCount = ceiling (mean + 3 * stddev + 50) :: Int
          -- Insert spike at frozenTime (Time effect controls the hour bucket)
          void $ runTestBg tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid frozenTime (V.singleton (errRate.hash, spikeCount, 5))

          issuesBefore <- countIssues tr Issues.RuntimeException
          runTestBg tr $ BackgroundJobs.detectErrorSpikes pid
          issuesAfter <- countIssues tr Issues.RuntimeException
          issuesAfter `shouldSatisfy` (> issuesBefore)

          -- Verify state is escalating
          spikedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById errRate.errorId
          fmap (.state) spikedPat `shouldBe` Just ESEscalating

        Nothing -> error "No established pattern found for spike test"

    it "6. User action handlers (assign, resolve, subscribe)" \tr -> do
      patterns <- runTestBg tr $ ErrorPatterns.getErrorPatterns pid Nothing 1 0
      let pat = fromMaybe (error "no patterns") $ listToMaybe patterns
          errUuid = pat.id.unErrorPatternId

      -- Test assign
      let sess = Servant.getResponse tr.trSessAndHeader
      void $ testServant tr $ Pages.Anomalies.assignErrorPostH pid errUuid (Pages.Anomalies.AssignErrorForm (Just $ UUID.toText sess.user.id.getUserId))
      assignedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.assigneeId) assignedPat `shouldBe` Just (Just sess.user.id)

      -- Test resolve
      void $ testServant tr $ Pages.Anomalies.resolveErrorPostH pid errUuid
      resolvedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.state) resolvedPat `shouldBe` Just ESResolved

      -- Test subscribe
      void $ testServant tr $ Pages.Anomalies.errorSubscriptionPostH pid errUuid (Pages.Anomalies.ErrorSubscriptionForm (Just 30))
      subscribedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.subscribed) subscribedPat `shouldBe` Just True

      -- Test unsubscribe
      void $ testServant tr $ Pages.Anomalies.errorSubscriptionPostH pid errUuid (Pages.Anomalies.ErrorSubscriptionForm Nothing)
      unsubPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.subscribed) unsubPat `shouldBe` Just False

    it "7. Occurrence count decay and auto-resolution" \tr -> do
      patterns <- runTestBg tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      -- Find or create a non-resolved pattern
      let activePatM = find (\p -> p.state /= ESResolved) patterns
      pat <- case activePatM of
        Just p -> pure p
        Nothing -> case patterns of
          (p1 : _) -> do
            void $ runTestBg tr $ ErrorPatterns.updateErrorPatternState p1.id ESOngoing
            fromMaybe (error "no pattern") <$> runTestBg tr (ErrorPatterns.getErrorPatternById p1.id)
          [] -> error "no patterns available for decay test"

      -- Set quiet_minutes just below threshold (no production function for this test-specific state setup)
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| UPDATE apis.error_patterns SET quiet_minutes = resolution_threshold_minutes - 1, state = 'ongoing', occurrences_1m = 0
                WHERE id = ? |]
          (PGS.Only pat.id)

      void $ runTestBg tr ErrorPatterns.updateOccurrenceCounts

      updatedPat <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      case updatedPat of
        Just p -> do
          p.state `shouldBe` ESResolved
          p.occurrences_1m `shouldBe` 0
        Nothing -> error "Pattern not found after occurrence count update"

    it "8. Notification threading stores and reuses thread IDs" \tr -> do
      -- Set up Slack + Discord integrations for the test project
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn
          [sql| INSERT INTO apis.slack (project_id, webhook_url, team_id, channel_id, team_name, bot_token)
                VALUES (?, 'https://hooks.slack.com/test', 'T_TEST', 'C_TEST', 'TestTeam', 'xoxb-test')
                ON CONFLICT (project_id) DO UPDATE SET channel_id = 'C_TEST', bot_token = 'xoxb-test' |]
          (PGS.Only pid)
        void $ PGS.execute conn
          [sql| INSERT INTO apis.discord (project_id, guild_id, notifs_channel_id)
                VALUES (?, 'G_TEST', 'DC_TEST')
                ON CONFLICT (project_id) DO UPDATE SET notifs_channel_id = 'DC_TEST' |]
          (PGS.Only pid)
        void $ PGS.execute conn
          [sql| UPDATE projects.projects SET notifications_channel = '{slack,discord}', error_alerts = true WHERE id = ? |]
          (PGS.Only pid)

      -- Find a subscribed, non-resolved pattern with a matching issue
      patterns <- runTestBg tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      let pat = fromMaybe (error "no patterns") $ find (\p -> p.state /= ESResolved) patterns
      -- Ensure it's subscribed
      void $ testServant tr $ Pages.Anomalies.errorSubscriptionPostH pid pat.id.unErrorPatternId (Pages.Anomalies.ErrorSubscriptionForm (Just 30))
      -- Ensure matching issue exists
      void $ runAllBackgroundJobs tr.trATCtx

      -- First notification: should create thread IDs
      runTestBg tr $ BackgroundJobs.notifyErrorSubscriptions pid (V.singleton pat.hash)
      pat1 <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      let slackTs1 = pat1 >>= (.slackThreadTs)
          discordId1 = pat1 >>= (.discordMessageId)
      isJust slackTs1 `shouldBe` True
      isJust discordId1 `shouldBe` True

      -- Make notification due again by pushing last_notified_at into the past
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| UPDATE apis.error_patterns SET last_notified_at = NOW() - INTERVAL '2 hours' WHERE id = ? |]
          (PGS.Only pat.id)

      -- Second notification: should reuse same thread IDs (threading preserved)
      runTestBg tr $ BackgroundJobs.notifyErrorSubscriptions pid (V.singleton pat.hash)
      pat2 <- runTestBg tr $ ErrorPatterns.getErrorPatternById pat.id
      (pat2 >>= (.slackThreadTs)) `shouldBe` slackTs1
      (pat2 >>= (.discordMessageId)) `shouldBe` discordId1

    it "9. Error fingerprint stability across IPs and timestamps" \tr -> do
      apiKey <- createTestAPIKey tr pid "fingerprint-test-key"
      let stack = "Error: connection failed\n    at DbPool.connect (/app/src/db.js:25:10)\n    at Server.start (/app/src/server.js:50:5)"

      -- Same error with different IPs in message and different timestamps → same fingerprint
      ingestTraceWithException tr apiKey "POST /api/data" "ConnectionError" "connect ECONNREFUSED 10.0.0.1:5432" stack (addUTCTime 300 frozenTime)
      ingestTraceWithException tr apiKey "POST /api/data" "ConnectionError" "connect ECONNREFUSED 192.168.1.100:5432" stack (addUTCTime 310 frozenTime)
      runTestBg tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 360 frozenTime) pid

      -- Both should have matched the same pattern (IP normalized to {ipv4})
      patM <- runTestBg tr $ ErrorPatterns.getErrorPatternByHash pid
        (EF.computeErrorFingerprint (UUID.toText UUID.nil) Nothing (Just "POST /api/data") "unknown" "ConnectionError" "connect ECONNREFUSED 10.0.0.1:5432" stack)
      isJust patM `shouldBe` True

      -- Different error type → different fingerprint (separate pattern)
      prevCount <- length <$> runTestBg tr (ErrorPatterns.getErrorPatterns pid Nothing 100 0)
      ingestTraceWithException tr apiKey "POST /api/data" "TimeoutError" "request timed out after 30000ms" stack (addUTCTime 320 frozenTime)
      runTestBg tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 380 frozenTime) pid
      newCount <- length <$> runTestBg tr (ErrorPatterns.getErrorPatterns pid Nothing 100 0)
      newCount `shouldSatisfy` (> prevCount)
