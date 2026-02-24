module Pages.LogPatternsSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (BaselineState (..), LogPatternState (..))
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Users
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant qualified
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldSatisfy)
import Text.Read (read)


pid :: Projects.ProjectId
pid = UUIDId UUID.nil


frozenTime :: UTCTime
frozenTime = read "2025-01-01 00:00:00 UTC"


-- | Insert a synthetic log row for pattern extraction to pick up
insertTestLog :: TestResources -> Text -> Text -> UTCTime -> IO ()
insertTestLog tr body serviceName ts = do
  logId <- UUID.nextRandom
  withResource tr.trPool \conn ->
    void $ PGS.execute conn
      [sql| INSERT INTO otel_logs_and_spans
              (id, project_id, timestamp, start_time, kind, summary, resource___service___name, date, hashes)
            VALUES (?::uuid, ?, ?, ?, 'log', string_to_array(?, ' '), ?, ?::date, '{}')
            ON CONFLICT DO NOTHING |]
      (show logId, pid.toText, ts, ts, body, serviceName, ts)


-- | Insert pre-aggregated hourly stats for a pattern to build baselines quickly
insertHourlyStat :: TestResources -> Text -> Text -> UTCTime -> Int64 -> IO ()
insertHourlyStat tr sourceField patternHash hourBucket count =
  withResource tr.trPool \conn ->
    void $ PGS.execute conn
      [sql| INSERT INTO apis.log_pattern_hourly_stats (project_id, source_field, pattern_hash, hour_bucket, event_count)
            VALUES (?, ?, ?, date_trunc('hour', ?::timestamptz), ?)
            ON CONFLICT (project_id, source_field, pattern_hash, hour_bucket)
            DO UPDATE SET event_count = apis.log_pattern_hourly_stats.event_count + EXCLUDED.event_count |]
      (pid, sourceField, patternHash, hourBucket, count)


-- | Count log patterns in the database for the test project
countPatterns :: TestResources -> IO Int
countPatterns tr = withResource tr.trPool \conn -> do
  [PGS.Only n] <- PGS.query conn [sql| SELECT COUNT(*)::INT FROM apis.log_patterns WHERE project_id = ? |] (PGS.Only pid)
  pure n


-- | Count issues of a given type
countIssues :: TestResources -> Issues.IssueType -> IO Int
countIssues tr issueType = withResource tr.trPool \conn -> do
  [PGS.Only n] <- PGS.query conn
    [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE project_id = ? AND issue_type = ? |]
    (pid, issueType)
  pure n


spec :: Spec
spec = aroundAll withTestResources do
  describe "Log Pattern Pipeline" do

    it "1. Extract patterns from ingested logs" \tr -> do
      let msgs = [ "User login succeeded for user_id=123 from ip=10.0.0.1"
                 , "User login succeeded for user_id=456 from ip=10.0.0.2"
                 , "User login succeeded for user_id=789 from ip=10.0.0.3"
                 , "Payment processed amount=49.99 currency=USD order_id=A001"
                 , "Payment processed amount=19.99 currency=USD order_id=A002"
                 , "Payment processed amount=99.99 currency=EUR order_id=A003"
                 , "Database connection timeout after 30s host=db-primary"
                 , "Database connection timeout after 30s host=db-replica"
                 ]
      forM_ (zip [0 ..] msgs) \(i, body) ->
        insertTestLog tr body "test-service" (addUTCTime (fromIntegral (i :: Int) - 60) frozenTime)

      -- Window is [frozenTime-5min, frozenTime], derived from scheduledTime
      runTestBg tr $ BackgroundJobs.logsPatternExtraction frozenTime pid

      -- Verify patterns were created
      patternCount <- countPatterns tr
      patternCount `shouldSatisfy` (> 0)

      -- Verify at least one pattern has hourly stats
      statsCount <- withResource tr.trPool \conn -> do
        [PGS.Only n] <- PGS.query conn
          [sql| SELECT COUNT(*)::INT FROM apis.log_pattern_hourly_stats WHERE project_id = ? |] (PGS.Only pid)
        pure (n :: Int)
      statsCount `shouldSatisfy` (> 0)

    it "2. Upsert & batch-update baselines (exercises PGArray fix)" \tr -> do
      -- Insert a pattern directly to control the test
      let patHash = "test-baseline-hash-001"
          sourceField = "summary"
      void $ runTestBg tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
        { projectId = pid, logPattern = "Test <*> pattern for baseline", hash = patHash
        , sourceField, serviceName = Just "test-svc", logLevel = Just "INFO"
        , traceId = Nothing, sampleMessage = Just "Test pattern baseline", eventCount = 100
        }

      -- Seed 50 hours of stats (exceeds baselineWindowHours=48) to get enough data
      -- 600/hour exceeds minMedianForEstablished=500 for immediate establishment
      let hourOffsets = [-50 .. -1] :: [Int]
      forM_ hourOffsets \h ->
        insertHourlyStat tr sourceField patHash (addUTCTime (fromIntegral h * 3600) frozenTime) 600

      -- Run baseline calculation
      runTestBg tr $ BackgroundJobs.calculateLogPatternBaselines pid

      -- Verify the pattern got established baseline
      patternM <- runTestBg tr $ LogPatterns.getLogPatternByHash pid sourceField patHash
      case patternM of
        Just p -> do
          p.baselineState `shouldBe` BSEstablished
          isJust p.baselineVolumeHourlyMean `shouldBe` True
          isJust p.baselineVolumeHourlyMad `shouldBe` True
        Nothing -> error "Pattern not found after baseline calculation"

    it "3. getLogPatternsByIds returns correct patterns (exercises PGArray fix)" \tr -> do
      allPatterns <- runTestBg tr $ LogPatterns.getLogPatterns pid 100 0
      allPatterns `shouldSatisfy` (not . null)
      let ids = V.fromList $ map (.id) $ take 3 allPatterns
      fetched <- runTestBg tr $ LogPatterns.getLogPatternsByIds ids
      V.length fetched `shouldBe` V.length ids

    it "4. Detect spike and create rate-change issue" \tr -> do
      -- Find an established pattern
      established <- withResource tr.trPool \conn ->
        PGS.query conn
          [sql| SELECT pattern_hash, source_field, baseline_volume_hourly_mean, baseline_volume_hourly_mad
                FROM apis.log_patterns
                WHERE project_id = ? AND baseline_state = 'established'
                  AND baseline_volume_hourly_mean IS NOT NULL AND baseline_volume_hourly_mad > 0
                LIMIT 1 |]
          (PGS.Only pid) :: IO [(Text, Text, Double, Double)]

      -- Spike detection uses the current hour + projected rate.
      -- frozenTime is exactly :00, so minutesIntoHour = max 5 0 = 5, scaleFactor = 12.0
      -- Stats go into the current hour bucket and get projected: count * 12
      case established of
        [(patHash, srcField, mean, mad)] -> do
          -- projected = rawCount * 12 must exceed mean + 5*mad + 50
          let rawCount = ceiling ((mean + 5 * mad + 100) / 12) :: Int64
          insertHourlyStat tr srcField patHash frozenTime rawCount

          issuesBefore <- countIssues tr Issues.LogPatternRateChange
          runTestBg tr $ BackgroundJobs.detectLogPatternSpikes pid tr.trATCtx
          issuesAfter <- countIssues tr Issues.LogPatternRateChange
          issuesAfter `shouldSatisfy` (> issuesBefore)

        _ -> do
          -- No established pattern with positive MAD — seed one manually
          let patHash = "test-spike-hash-002"
              srcField = "summary"
          void $ runTestBg tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
            { projectId = pid, logPattern = "Spike test <*> pattern", hash = patHash
            , sourceField = srcField, serviceName = Just "test-svc", logLevel = Just "INFO"
            , traceId = Nothing, sampleMessage = Just "Spike test", eventCount = 50
            }
          -- Seed baseline: 48 hours at 600 events/hour with some variance
          forM_ ([-48 .. -1] :: [Int]) \h ->
            insertHourlyStat tr srcField patHash (addUTCTime (fromIntegral h * 3600) frozenTime)
              (600 + fromIntegral (h `mod` 5))
          runTestBg tr $ BackgroundJobs.calculateLogPatternBaselines pid
          -- Spike in current hour; projected = 200 * 12 = 2400 >> baseline ~600
          insertHourlyStat tr srcField patHash frozenTime 200
          issuesBefore <- countIssues tr Issues.LogPatternRateChange
          runTestBg tr $ BackgroundJobs.detectLogPatternSpikes pid tr.trATCtx
          issuesAfter <- countIssues tr Issues.LogPatternRateChange
          issuesAfter `shouldSatisfy` (> issuesBefore)

    it "5. Process new patterns and create log_pattern issues" \tr -> do
      -- Insert patterns in 'new' state with enough volume to pass the low-volume check
      let patHashes = ["new-pat-001", "new-pat-002"] :: [Text]
      forM_ patHashes \h -> do
        void $ runTestBg tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
          { projectId = pid, logPattern = "New pattern " <> h <> " <*>", hash = h
          , sourceField = "summary", serviceName = Just "test-svc", logLevel = Just "WARN"
          , traceId = Nothing, sampleMessage = Just ("Sample " <> h), eventCount = 10
          }
        insertHourlyStat tr "summary" h frozenTime 1500

      -- Also insert a url_path pattern — should be acknowledged but NO issue created
      let urlPathHash = "url-path-pat-001"
      void $ runTestBg tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
        { projectId = pid, logPattern = "/api/users/<*>", hash = urlPathHash
        , sourceField = "url_path", serviceName = Just "test-svc", logLevel = Nothing
        , traceId = Nothing, sampleMessage = Just "/api/users/123", eventCount = 50
        }
      insertHourlyStat tr "url_path" urlPathHash frozenTime 1500

      issuesBefore <- countIssues tr Issues.LogPattern
      runTestBg tr $ BackgroundJobs.processNewLogPatterns pid tr.trATCtx
      issuesAfter <- countIssues tr Issues.LogPattern
      -- Only summary patterns create issues, not url_path
      issuesAfter `shouldSatisfy` (>= issuesBefore + length patHashes)

      -- Verify ALL patterns (including url_path) were auto-acknowledged
      forM_ patHashes \h -> do
        patM <- runTestBg tr $ LogPatterns.getLogPatternByHash pid "summary" h
        case patM of
          Just p -> p.state `shouldBe` LPSAcknowledged
          Nothing -> error $ "Pattern " <> h <> " not found after processNewLogPatterns"
      urlPathPat <- runTestBg tr $ LogPatterns.getLogPatternByHash pid "url_path" urlPathHash
      case urlPathPat of
        Just p -> p.state `shouldBe` LPSAcknowledged
        Nothing -> error "url_path pattern not found after processNewLogPatterns"

    it "6. Acknowledge log patterns (exercises PGArray fix)" \tr -> do
      -- Insert fresh patterns to test acknowledgment
      let ackHashes = ["ack-test-001", "ack-test-002"] :: [Text]
          sourceField = "summary"
      forM_ ackHashes \h ->
        void $ runTestBg tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
          { projectId = pid, logPattern = "Ack test " <> h <> " <*>", hash = h
          , sourceField, serviceName = Just "test-svc", logLevel = Just "INFO"
          , traceId = Nothing, sampleMessage = Just ("Ack sample " <> h), eventCount = 5
          }

      let hashes = V.fromList ackHashes
          sess = Servant.getResponse tr.trSessAndHeader
      updated <- runTestBg tr $ LogPatterns.acknowledgeLogPatterns pid (Just sess.user.id) hashes
      updated `shouldBe` fromIntegral (V.length hashes)

      -- Verify state change
      forM_ ackHashes \h -> do
        patM <- runTestBg tr $ LogPatterns.getLogPatternByHash pid sourceField h
        case patM of
          Just p -> p.state `shouldBe` LPSAcknowledged
          Nothing -> error $ "Pattern " <> h <> " not found after acknowledge"

    it "7. Prune stale acknowledged patterns" \tr -> do
      let staleHash = "stale-prune-001"
          sourceField = "summary"
      void $ runTestBg tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern
        { projectId = pid, logPattern = "Stale pattern <*>", hash = staleHash
        , sourceField, serviceName = Just "test-svc", logLevel = Just "INFO"
        , traceId = Nothing, sampleMessage = Just "Stale test", eventCount = 5
        }
      -- Acknowledge it so it's eligible for pruning
      void $ runTestBg tr $ LogPatterns.acknowledgeLogPatterns pid Nothing (V.singleton staleHash)
      -- Backdate last_seen_at to 60 days ago (exceeds stalePatternDays=30)
      withResource tr.trPool \conn ->
        void $ PGS.execute conn
          [sql| UPDATE apis.log_patterns SET last_seen_at = ?::timestamptz - INTERVAL '60 days'
                WHERE project_id = ? AND pattern_hash = ? |]
          (frozenTime, pid, staleHash)

      -- Also insert an old hourly stat (exceeds baselineWindowHours + 24 = 72h)
      insertHourlyStat tr sourceField staleHash (addUTCTime (-100 * 3600) frozenTime) 100

      beforeCount <- countPatterns tr
      pruned <- runTestBg tr $ LogPatterns.pruneStalePatterns pid frozenTime 30
      pruned `shouldSatisfy` (>= 1)

      afterCount <- countPatterns tr
      afterCount `shouldSatisfy` (< beforeCount)

      -- Verify stale hourly stats are also prunable
      statsPruned <- runTestBg tr $ LogPatterns.pruneOldHourlyStats pid frozenTime 72
      statsPruned `shouldSatisfy` (>= 1)
