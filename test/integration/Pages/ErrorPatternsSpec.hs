module Pages.ErrorPatternsSpec (spec) where

import BackgroundJobs qualified
import Data.Effectful.Notify (Notification (..), SlackData (..))
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (addUTCTime)
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


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


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
      runTestBg frozenTime tr $ BackgroundJobs.processOneMinuteErrors frozenTime pid

      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      length patterns `shouldBe` 2

      -- Verify hourly stats have correct event_count per pattern
      hourlyStats <- withResource tr.trPool \conn ->
        PGS.query conn
          [sql| SELECT e.error_type, h.event_count::INT
                FROM apis.error_hourly_stats h
                JOIN apis.error_patterns e ON e.id = h.error_id
                WHERE h.project_id = ?
                ORDER BY e.error_type |]
          (PGS.Only pid) :: IO [(Text, Int)]
      -- 2 TypeErrors and 1 ConnectionRefusedError → two rows with correct counts
      let statsMap = Map.fromList hourlyStats
      length hourlyStats `shouldBe` 2
      Map.lookup "TypeError" statsMap `shouldBe` Just 2
      Map.lookup "ConnectionRefusedError" statsMap `shouldBe` Just 1

      -- Verify pattern fields populated
      forM_ patterns \p -> do
        p.errorType `shouldSatisfy` (/= "")
        p.message `shouldSatisfy` (/= "")
        p.stacktrace `shouldSatisfy` (/= "")

    it "2. NewErrorDetected creates RuntimeException issues" \tr -> do
      -- DB trigger on INSERT already fired during test 1
      void $ runAllBackgroundJobs frozenTime tr.trATCtx
      issueCount <- countIssues tr Issues.RuntimeException
      issueCount `shouldSatisfy` (> 0)

    it "3. Resolved → Regressed on new occurrence" \tr -> do
      apiKey <- createTestAPIKey tr pid "error-regression-key"
      -- Find a pattern to resolve
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      let pat = fromMaybe (error "no patterns") $ listToMaybe patterns
      void $ runTestBg frozenTime tr $ ErrorPatterns.resolveErrorPattern pat.id
      resolvedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.state) resolvedPat `shouldBe` Just ESResolved

      -- Ingest same error again → should regress
      ingestTraceWithException tr apiKey "GET /api/users/:id" pat.errorType pat.message pat.stacktrace (addUTCTime 60 frozenTime)
      runTestBg frozenTime tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 120 frozenTime) pid

      regressedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.state) regressedPat `shouldBe` Just ESRegressed
      fmap (isJust . (.regressedAt)) regressedPat `shouldBe` Just True

    it "3a. Regressed error uses regressed notification template" \tr -> do
      -- Set up notification channels (may already exist from prior tests, ON CONFLICT handles that)
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn
          [sql| INSERT INTO apis.slack (project_id, webhook_url, team_id, channel_id, team_name, bot_token)
                VALUES (?, 'https://hooks.slack.com/test', 'T_TEST', 'C_TEST', 'TestTeam', 'xoxb-test')
                ON CONFLICT (project_id) DO UPDATE SET channel_id = 'C_TEST', bot_token = 'xoxb-test' |]
          (PGS.Only pid)
        void $ PGS.execute conn
          [sql| UPDATE projects.projects SET notifications_channel = '{slack}', error_alerts = true WHERE id = ? |]
          (PGS.Only pid)
      -- Find the regressed pattern from test 3
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      let regressedM = find (\p -> p.state == ESRegressed) patterns
      case regressedM of
        Just pat -> do
          (notifs, _) <- runTestBackgroundWithNotifications frozenTime tr.trLogger tr.trATCtx
            $ BackgroundJobs.processNewError pid pat.hash
          -- Should produce at least one notification (Slack or email)
          notifs `shouldSatisfy` (not . null)
          -- Verify an issue was created with the correct type
          issueCount <- countIssues tr Issues.RuntimeException
          issueCount `shouldSatisfy` (> 0)
        Nothing -> error "No regressed pattern found for notification template test"

    it "4. Baseline calculation from hourly stats" \tr -> do
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 1 0
      let pat = fromMaybe (error "no patterns") $ listToMaybe patterns
      -- Seed 50 hours of stats (exceeds 24h minimum) with variance so stddev > 0
      forM_ ([-50 .. -1] :: [Int]) \h ->
        void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid
          (addUTCTime (fromIntegral h * 3600) frozenTime)
          (V.singleton (pat.hash, 600 + (h `mod` 7) * 10, 10))

      runTestBg frozenTime tr $ BackgroundJobs.calculateErrorBaselines pid

      updatedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      case updatedPat of
        Just p -> do
          p.baselineState `shouldBe` BSEstablished
          isJust p.baselineErrorRateMean `shouldBe` True
          isJust p.baselineErrorRateStddev `shouldBe` True
          p.baselineSamples `shouldSatisfy` (>= 24)
        Nothing -> error "Pattern not found after baseline calculation"

    it "4a. Baseline is outlier-robust (median not pulled by spikes)" \tr -> do
      -- Use the second pattern (ConnectionRefusedError) to avoid conflicting with test 4's data
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      let pat = fromMaybe (error "need 2+ patterns") $ listToMaybe $ drop 1 patterns
      -- Clear existing hourly stats for this pattern to get clean test
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| DELETE FROM apis.error_hourly_stats WHERE error_id = (SELECT id FROM apis.error_patterns WHERE project_id = ? AND hash = ?) |]
          (pid, pat.hash)
      -- Seed 40 normal hours (100-200) plus 10 outlier hours (5000)
      forM_ ([-50 .. -11] :: [Int]) \h ->
        void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid
          (addUTCTime (fromIntegral h * 3600) frozenTime)
          (V.singleton (pat.hash, 100 + (h `mod` 10) * 10, 5))
      forM_ ([-10 .. -1] :: [Int]) \h ->
        void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid
          (addUTCTime (fromIntegral h * 3600) frozenTime)
          (V.singleton (pat.hash, 5000, 50))

      runTestBg frozenTime tr $ BackgroundJobs.calculateErrorBaselines pid

      updatedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      case updatedPat of
        Just p -> do
          p.baselineState `shouldBe` BSEstablished
          -- Median should be near 150 (the normal range), not near 5000 (the outliers)
          let mean = fromMaybe 0 p.baselineErrorRateMean
          mean `shouldSatisfy` (< 500)
          mean `shouldSatisfy` (> 50)
        Nothing -> error "Pattern not found after outlier baseline calculation"

    it "5. Spike detection creates escalating issue" \tr -> do
      let sess = Servant.getResponse tr.trSessAndHeader
      -- Un-resolve established patterns (test 3 resolved them)
      errRates <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      forM_ errRates \r ->
        when (r.baselineState == BSEstablished && r.state == ESResolved) $
          void $ runTestBg frozenTime tr $ ErrorPatterns.updateErrorPatternState r.errorId ESOngoing

      -- Acknowledge existing issues so ON CONFLICT doesn't deduplicate the new spike issue
      (issues, _) <- runTestBg frozenTime tr $ Issues.selectIssues pid Nothing (Just False) Nothing 100 0 Nothing Nothing
      forM_ issues \issue -> runTestBg frozenTime tr $ Issues.acknowledgeIssue issue.id sess.user.id

      -- Find an established pattern with stddev > 0
      errRates' <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      let established = find (\r -> r.baselineState == BSEstablished && isJust r.baselineMean && maybe False (> 0) r.baselineStddev && r.state /= ESResolved) errRates'

      case established of
        Just errRate -> do
          let mean = fromMaybe 0 errRate.baselineMean
              stddev = fromMaybe 0 errRate.baselineStddev
              spikeCount = ceiling (mean + 3 * stddev + 50) :: Int
          -- Insert spike at frozenTime (Time effect controls the hour bucket)
          void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid frozenTime (V.singleton (errRate.hash, spikeCount, 5))

          issuesBefore <- countIssues tr Issues.RuntimeException
          runTestBg frozenTime tr $ BackgroundJobs.detectErrorSpikes pid
          issuesAfter <- countIssues tr Issues.RuntimeException
          issuesAfter `shouldSatisfy` (> issuesBefore)

          -- Verify state is escalating
          spikedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById errRate.errorId
          fmap (.state) spikedPat `shouldBe` Just ESEscalating

        Nothing -> error "No established pattern found for spike test"

    it "5a. Spike idempotence — repeated detection does not create duplicate issues" \tr -> do
      -- detectErrorSpikes already ran in test 5 and set ESEscalating.
      -- Running it again with same spike data should NOT create another issue.
      issuesBefore <- countIssues tr Issues.RuntimeException
      runTestBg frozenTime tr $ BackgroundJobs.detectErrorSpikes pid
      issuesAfter <- countIssues tr Issues.RuntimeException
      issuesAfter `shouldBe` issuesBefore

    it "5b. De-escalation: Escalating → Ongoing when spike subsides" \tr -> do
      -- Find the escalating pattern from test 5
      errRates <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      let escalatingM = find (\r -> r.state == ESEscalating && r.baselineState == BSEstablished) errRates
      case escalatingM of
        Just errRate -> do
          let mean = fromMaybe 0 errRate.baselineMean
          -- Insert a normal-rate hourly stat (at mean, well below spike threshold)
          -- Use a fresh hour bucket so the spike from test 5 doesn't persist
          let normalTime = addUTCTime 3600 frozenTime
          void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid normalTime (V.singleton (errRate.hash, ceiling mean, 2))

          runTestBg normalTime tr $ BackgroundJobs.detectErrorSpikes pid

          deescalatedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById errRate.errorId
          fmap (.state) deescalatedPat `shouldBe` Just ESOngoing
        Nothing -> error "No escalating pattern found for de-escalation test"

    it "5c. Flat baseline (stddev=0) still detects spikes via MAD floor" \tr -> do
      -- Find the pattern from 5b (now ESOngoing) and give it a flat baseline (stddev=0)
      errRates <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      let ongoingM = find (\r -> r.state == ESOngoing && r.baselineState == BSEstablished) errRates
      case ongoingM of
        Just errRate -> do
          -- Set stddev to 0 (perfectly flat baseline)
          withResource tr.trPool \conn ->
            void $ PGS.execute conn
              [sql| UPDATE apis.error_patterns SET baseline_error_rate_stddev = 0, baseline_error_rate_mean = 100 WHERE id = ? |]
              (PGS.Only errRate.errorId)
          -- Acknowledge existing RuntimeException issues to avoid ON CONFLICT dedup
          let sess = Servant.getResponse tr.trSessAndHeader
          (issues, _) <- runTestBg frozenTime tr $ Issues.selectIssues pid Nothing (Just False) Nothing 100 0 Nothing Nothing
          forM_ issues \issue -> runTestBg frozenTime tr $ Issues.acknowledgeIssue issue.id sess.user.id

          -- Insert a massive spike (200, well above mean=100 + minAbsoluteDelta=50)
          let spikeTime = addUTCTime 7200 frozenTime
          void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid spikeTime (V.singleton (errRate.hash, 200, 5))

          issuesBefore <- countIssues tr Issues.RuntimeException
          runTestBg spikeTime tr $ BackgroundJobs.detectErrorSpikes pid
          issuesAfter <- countIssues tr Issues.RuntimeException
          issuesAfter `shouldSatisfy` (> issuesBefore)

          spikedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById errRate.errorId
          fmap (.state) spikedPat `shouldBe` Just ESEscalating
        Nothing -> error "No ongoing pattern found for flat baseline test"

    it "5d. Full cycle: Regressed error that spikes transitions to ESEscalating" \tr -> do
      -- Find an established pattern and resolve it
      errRates <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      let estM = find (\r -> r.baselineState == BSEstablished && r.state /= ESResolved) errRates
      case estM of
        Just errRate -> do
          void $ runTestBg frozenTime tr $ ErrorPatterns.resolveErrorPattern errRate.errorId
          -- Re-ingest to regress
          apiKey <- createTestAPIKey tr pid "regress-spike-key"
          ingestTraceWithException tr apiKey "GET /regress-spike" errRate.errorType errRate.message errRate.stacktrace (addUTCTime 9000 frozenTime)
          runTestBg frozenTime tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 9060 frozenTime) pid

          regressedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById errRate.errorId
          fmap (.state) regressedPat `shouldBe` Just ESRegressed

          -- Now spike the regressed error — should transition to ESEscalating with a new issue
          let mean = fromMaybe 0 errRate.baselineMean
              stddev = fromMaybe 0 errRate.baselineStddev
              spikeCount = ceiling (mean + 3 * stddev + 100) :: Int
              spikeTime = addUTCTime 10800 frozenTime
          void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid spikeTime (V.singleton (errRate.hash, spikeCount, 5))

          -- Acknowledge existing issues to avoid ON CONFLICT dedup
          let sess = Servant.getResponse tr.trSessAndHeader
          (issues, _) <- runTestBg frozenTime tr $ Issues.selectIssues pid Nothing (Just False) Nothing 100 0 Nothing Nothing
          forM_ issues \issue -> runTestBg frozenTime tr $ Issues.acknowledgeIssue issue.id sess.user.id

          issuesBefore <- countIssues tr Issues.RuntimeException
          runTestBg spikeTime tr $ BackgroundJobs.detectErrorSpikes pid
          issuesAfter <- countIssues tr Issues.RuntimeException
          issuesAfter `shouldSatisfy` (> issuesBefore)

          spikedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById errRate.errorId
          fmap (.state) spikedPat `shouldBe` Just ESEscalating
        Nothing -> error "No established pattern found for full-cycle test"

    it "5e. Concurrent spikes: two patterns spiking simultaneously both get issues" \tr -> do
      -- Ensure we have at least 2 established, non-resolved patterns
      errRates <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternsWithCurrentRates pid frozenTime
      let established = filter (\r -> r.baselineState == BSEstablished && isJust r.baselineMean && r.state /= ESResolved) errRates
      case established of
        (r1 : r2 : _) -> do
          -- Set both to ESOngoing so spike detection can transition them
          forM_ [r1, r2] \r -> void $ runTestBg frozenTime tr $ ErrorPatterns.updateErrorPatternState r.errorId ESOngoing
          -- Acknowledge existing issues to avoid ON CONFLICT dedup
          let sess = Servant.getResponse tr.trSessAndHeader
          (issues, _) <- runTestBg frozenTime tr $ Issues.selectIssues pid Nothing (Just False) Nothing 100 0 Nothing Nothing
          forM_ issues \issue -> runTestBg frozenTime tr $ Issues.acknowledgeIssue issue.id sess.user.id
          -- Spike both patterns in the same hour bucket
          let concurrentTime = addUTCTime 14400 frozenTime
          forM_ [r1, r2] \r -> do
            let mean = fromMaybe 0 r.baselineMean
                stddev = fromMaybe 0 r.baselineStddev
                spikeCount = ceiling (mean + 3 * stddev + 100) :: Int
            void $ runTestBg frozenTime tr $ ErrorPatterns.upsertErrorPatternHourlyStats pid concurrentTime (V.singleton (r.hash, spikeCount, 5))

          issuesBefore <- countIssues tr Issues.RuntimeException
          runTestBg concurrentTime tr $ BackgroundJobs.detectErrorSpikes pid
          issuesAfter <- countIssues tr Issues.RuntimeException
          -- Both should have created separate issues
          issuesAfter `shouldSatisfy` (>= issuesBefore + 2)

          -- Both should be ESEscalating
          p1 <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById r1.errorId
          p2 <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById r2.errorId
          fmap (.state) p1 `shouldBe` Just ESEscalating
          fmap (.state) p2 `shouldBe` Just ESEscalating
        _ -> error "Need at least 2 established patterns for concurrent spike test"

    it "6. User action handlers (assign, resolve, subscribe)" \tr -> do
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 1 0
      let pat = fromMaybe (error "no patterns") $ listToMaybe patterns
          errUuid = pat.id.unErrorPatternId

      -- Test assign
      let sess = Servant.getResponse tr.trSessAndHeader
      void $ testServant tr $ Pages.Anomalies.assignErrorPostH pid errUuid (Pages.Anomalies.AssignErrorForm (Just $ UUID.toText sess.user.id.getUserId))
      assignedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.assigneeId) assignedPat `shouldBe` Just (Just sess.user.id)

      -- Test resolve
      void $ testServant tr $ Pages.Anomalies.resolveErrorPostH pid errUuid
      resolvedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.state) resolvedPat `shouldBe` Just ESResolved

      -- Test subscribe
      void $ testServant tr $ Pages.Anomalies.errorSubscriptionPostH pid errUuid (Pages.Anomalies.ErrorSubscriptionForm (Just 30))
      subscribedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.subscribed) subscribedPat `shouldBe` Just True

      -- Test unsubscribe
      void $ testServant tr $ Pages.Anomalies.errorSubscriptionPostH pid errUuid (Pages.Anomalies.ErrorSubscriptionForm Nothing)
      unsubPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      fmap (.subscribed) unsubPat `shouldBe` Just False

    it "7. Occurrence count decay and auto-resolution" \tr -> do
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      -- Find or create a non-resolved pattern
      let activePatM = find (\p -> p.state /= ESResolved) patterns
      pat <- case activePatM of
        Just p -> pure p
        Nothing -> case patterns of
          (p1 : _) -> do
            void $ runTestBg frozenTime tr $ ErrorPatterns.updateErrorPatternState p1.id ESOngoing
            fromMaybe (error "no pattern") <$> runTestBg frozenTime tr (ErrorPatterns.getErrorPatternById p1.id)
          [] -> error "no patterns available for decay test"

      -- Set quiet_minutes just below threshold (no production function for this test-specific state setup)
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| UPDATE apis.error_patterns SET quiet_minutes = resolution_threshold_minutes - 1, state = 'ongoing', occurrences_1m = 0
                WHERE id = ? |]
          (PGS.Only pat.id)

      void $ runTestBg frozenTime tr ErrorPatterns.updateOccurrenceCounts

      updatedPat <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
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

      -- Find a non-resolved pattern (test 7 may have resolved all, so re-open one if needed)
      patterns <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatterns pid Nothing 10 0
      pat <- case find (\p -> p.state /= ESResolved) patterns of
        Just p -> pure p
        Nothing -> case patterns of
          (p1 : _) -> do
            void $ runTestBg frozenTime tr $ ErrorPatterns.updateErrorPatternState p1.id ESOngoing
            fromMaybe (error "no pattern") <$> runTestBg frozenTime tr (ErrorPatterns.getErrorPatternById p1.id)
          [] -> error "no patterns available"
      -- Ensure it's subscribed
      void $ testServant tr $ Pages.Anomalies.errorSubscriptionPostH pid pat.id.unErrorPatternId (Pages.Anomalies.ErrorSubscriptionForm (Just 30))
      -- Ensure matching issue exists
      void $ runAllBackgroundJobs frozenTime tr.trATCtx

      -- First notification: should create thread IDs and produce Slack + Discord notifications
      (notifs1, _) <- runTestBackgroundWithNotifications frozenTime tr.trLogger tr.trATCtx
        $ BackgroundJobs.notifyErrorSubscriptions pid (V.singleton pat.hash)
      -- Verify notification types and content
      let slackNotifs1 = [sd | SlackNotification sd <- notifs1]
          discordNotifs1 = [dd | DiscordNotification dd <- notifs1]
      length slackNotifs1 `shouldSatisfy` (>= 1)
      length discordNotifs1 `shouldSatisfy` (>= 1)
      -- Slack payload should mention the error type
      forM_ slackNotifs1 \sd -> do
        let payloadText = show @Text sd.payload
        T.isInfixOf pat.errorType payloadText `shouldBe` True

      pat1 <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
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
      (notifs2, _) <- runTestBackgroundWithNotifications frozenTime tr.trLogger tr.trATCtx
        $ BackgroundJobs.notifyErrorSubscriptions pid (V.singleton pat.hash)
      length [() | SlackNotification _ <- notifs2] `shouldSatisfy` (>= 1)

      pat2 <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternById pat.id
      (pat2 >>= (.slackThreadTs)) `shouldBe` slackTs1
      (pat2 >>= (.discordMessageId)) `shouldBe` discordId1

    it "9. Error fingerprint stability across IPs and timestamps" \tr -> do
      apiKey <- createTestAPIKey tr pid "fingerprint-test-key"
      let stack = "Error: connection failed\n    at DbPool.connect (/app/src/db.js:25:10)\n    at Server.start (/app/src/server.js:50:5)"

      -- Same error with different IPs in message and different timestamps → same fingerprint
      ingestTraceWithException tr apiKey "POST /api/data" "ConnectionError" "connect ECONNREFUSED 10.0.0.1:5432" stack (addUTCTime 300 frozenTime)
      ingestTraceWithException tr apiKey "POST /api/data" "ConnectionError" "connect ECONNREFUSED 192.168.1.100:5432" stack (addUTCTime 310 frozenTime)
      runTestBg frozenTime tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 360 frozenTime) pid

      -- Both should have matched the same pattern (IP normalized to {ipv4})
      patM <- runTestBg frozenTime tr $ ErrorPatterns.getErrorPatternByHash pid
        (EF.computeErrorFingerprint (UUID.toText UUID.nil) Nothing (Just "POST /api/data") "nodejs" "ConnectionError" "connect ECONNREFUSED 10.0.0.1:5432" stack)
      isJust patM `shouldBe` True

      -- Different error type → different fingerprint (separate pattern)
      prevCount <- length <$> runTestBg frozenTime tr (ErrorPatterns.getErrorPatterns pid Nothing 100 0)
      ingestTraceWithException tr apiKey "POST /api/data" "TimeoutError" "request timed out after 30000ms" stack (addUTCTime 320 frozenTime)
      runTestBg frozenTime tr $ BackgroundJobs.processOneMinuteErrors (addUTCTime 380 frozenTime) pid
      newCount <- length <$> runTestBg frozenTime tr (ErrorPatterns.getErrorPatterns pid Nothing 100 0)
      newCount `shouldSatisfy` (> prevCount)
