module Pages.LogPatternsSpec (spec) where

import BackgroundJobs qualified
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
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
            DO UPDATE SET event_count = GREATEST(apis.log_pattern_hourly_stats.event_count, EXCLUDED.event_count) |]
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
    (pid, Issues.issueTypeToText issueType)
  pure n


spec :: Spec
spec = aroundAll withTestResources do
  describe "Log Pattern Pipeline" do

    it "1. Extract patterns from ingested logs" \tr -> do
      -- logsPatternExtraction uses liftIO Time.currentTime (real wall clock) for the query window,
      -- so we must insert logs at real time and pass a matching scheduledTime.
      now <- getCurrentTime
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
        insertTestLog tr body "test-service" (addUTCTime (fromIntegral (i :: Int) - 60) now)

      -- scheduledTime = now so the window [now-5min, now] covers our inserts
      runTestBg tr $ BackgroundJobs.logsPatternExtraction now pid

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
      let hourOffsets = [-50 .. -1] :: [Int]
      forM_ hourOffsets \h ->
        insertHourlyStat tr sourceField patHash (addUTCTime (fromIntegral h * 3600) frozenTime) 150

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

      case established of
        [(patHash, srcField, mean, mad)] -> do
          -- Insert a spike: current hour count well above the baseline
          -- z-score threshold is 3.0 and minDelta is 10; set count to mean + 5*mad + 20 to guarantee detection
          let spikeCount = ceiling (mean + 5 * mad + 20) :: Int64
          insertHourlyStat tr srcField patHash frozenTime spikeCount

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
          -- Seed baseline: 48 hours at 100 events/hour with some variance
          forM_ ([-48 .. -1] :: [Int]) \h ->
            insertHourlyStat tr srcField patHash (addUTCTime (fromIntegral h * 3600) frozenTime)
              (100 + fromIntegral (h `mod` 5))
          runTestBg tr $ BackgroundJobs.calculateLogPatternBaselines pid
          -- Now spike at 500
          insertHourlyStat tr srcField patHash frozenTime 500
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
        -- Ensure total event volume >= 2000 to pass the low-volume guard
        insertHourlyStat tr "summary" h frozenTime 1500

      issuesBefore <- countIssues tr Issues.LogPattern
      runTestBg tr $ BackgroundJobs.processNewLogPatterns pid tr.trATCtx
      issuesAfter <- countIssues tr Issues.LogPattern
      issuesAfter `shouldSatisfy` (>= issuesBefore + length patHashes)

    it "6. Acknowledge log patterns (exercises PGArray fix)" \tr -> do
      patterns <- runTestBg tr $ LogPatterns.getLogPatterns pid 100 0
      let newPatterns = filter (\p -> p.state == LPSNew) patterns
      newPatterns `shouldSatisfy` (not . null) -- there may be some from step 1

      let hashes = V.fromList $ map (.patternHash) $ take 2 newPatterns
          sess = Servant.getResponse tr.trSessAndHeader
      updated <- runTestBg tr $ LogPatterns.acknowledgeLogPatterns pid sess.user.id hashes
      updated `shouldBe` fromIntegral (V.length hashes)

      -- Verify state change
      forM_ (V.toList hashes) \h -> do
        patM <- runTestBg tr $ LogPatterns.getLogPatternByHash pid (fromMaybe "summary" $ (.sourceField) <$> find (\p -> p.patternHash == h) newPatterns) h
        case patM of
          Just p -> p.state `shouldBe` LPSAcknowledged
          Nothing -> error $ "Pattern " <> h <> " not found after acknowledge"
