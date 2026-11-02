module Pages.LogPatternsSpec (spec) where

import BackgroundJobs qualified
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, zonedTimeToUTC)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Effectful.Time qualified as Time
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (LogPatternState (..))
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.PatternMerge qualified as PatternMerge
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Servant qualified
import System.Mem (performGC)


spec :: Spec
spec = around withTestResources $ describe "Log patterns" do
  it "baselines: batched updates skip merged and stat-less patterns; ingestion feeds patterns and hourly stats" \tr -> do
    runTestBg frozenTime tr pass
    -- 1001 rows spans two baseline pages; 'merged' must not be baselined, 'expired' has stats only outside the window.
    dbExec tr [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, first_seen_at)
                    SELECT ?, 'baseline fixture', 'batch-' || n, 'summary', ? FROM generate_series(1,1001) n |] (testPid, frozenTime)
    dbExec tr [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, first_seen_at)
                    SELECT ?, 'baseline fixture', hash, field, ? FROM
                    (VALUES ('batch-1','body'), ('no-stats','summary'), ('expired','summary'), ('merged','summary')) v(hash,field) |] (testPid, frozenTime)
    dbExec tr [sql| UPDATE apis.log_patterns SET canonical_id =
                    (SELECT id FROM apis.log_patterns WHERE project_id = ? AND pattern_hash='batch-1' AND source_field='summary')
                    WHERE project_id = ? AND pattern_hash='merged' |] (testPid, testPid)
    dbExec tr [sql| INSERT INTO apis.log_pattern_hourly_stats (project_id, source_field, pattern_hash, hour_bucket, event_count)
                    SELECT project_id, source_field, pattern_hash,
                      ?::timestamptz - CASE WHEN pattern_hash='expired' THEN interval '49 hours' ELSE interval '1 hour' END,
                      CASE WHEN source_field='body' THEN 100 ELSE 600 END
                    FROM apis.log_patterns WHERE project_id = ? AND pattern_hash <> 'no-stats' |] (frozenTime, testPid)
    job tr $ BackgroundJobs.calculateLogPatternBaselines testPid
    dbQuery @_ @(Text, Text, Maybe Double, Maybe Double, Int, Int64) tr
      [sql| SELECT source_field, baseline_state, baseline_volume_hourly_mean, baseline_volume_hourly_mad, baseline_samples, count(*)::bigint
            FROM apis.log_patterns WHERE project_id = ? GROUP BY 1,2,3,4,5 ORDER BY 1,2,3 NULLS LAST |] (Only testPid)
      `shouldReturn` [("body", "learning", Just 100, Just 0, 1, 1), ("summary", "established", Just 600, Just 0, 1, 1001), ("summary", "learning", Nothing, Nothing, 0, 3)]

    apiKey <- createTestAPIKey tr testPid "logpat-test-key"
    let msgs = [ "User login succeeded for user_id=123 from ip=10.0.0.1", "User login succeeded for user_id=456 from ip=10.0.0.2"
               , "User login succeeded for user_id=789 from ip=10.0.0.3", "Payment processed amount=49.99 currency=USD order_id=A001"
               , "Payment processed amount=19.99 currency=USD order_id=A002", "Payment processed amount=99.99 currency=EUR order_id=A003"
               , "Database connection timeout after 30s host=db-primary", "Database connection timeout after 30s host=db-replica" ]
    forM_ (zip [0 ..] msgs) \(i, body) -> ingestLog tr apiKey body (addUTCTime (fromIntegral (i :: Int) - 60) frozenTime)
    drainExtractionWorker tr
    dbQuery @_ @(Int, Int) tr
      [sql| SELECT (SELECT count(*)::int FROM apis.log_patterns WHERE project_id = ? AND log_pattern <> 'baseline fixture'),
                   (SELECT count(*)::int FROM apis.log_pattern_hourly_stats WHERE project_id = ? AND pattern_hash NOT LIKE 'batch-%' AND pattern_hash NOT IN ('no-stats', 'expired', 'merged')) |]
      (testPid, testPid)
      >>= (`shouldSatisfy` all (\(pats, stats) -> pats > 0 && stats > 0))

  it "rate change: only alertable levels fire, after two windows, projected per partial hour, silenced for exactly the ack window" \tr -> do
    runTestBg frozenTime tr pass
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
        levels :: [(Text, Maybe Text)]
        levels = [("gate-info-lc", Just "info"), ("gate-debug", Just "DEBUG"), ("gate-null", Nothing), ("gate-error", Just "ERROR")]
        detect = do t <- now tr; replicateM_ 2 (job tr $ BackgroundJobs.detectLogPatternSpikes testPid t tr.trATCtx)
        spike h = now tr >>= \t -> stat tr "summary" h t 600
        open = dbQuery @_ @(Issues.IssueId, Text, Text) tr [sql| SELECT id, target_hash, title FROM apis.issues WHERE project_id = ? AND issue_type = 'log_pattern_rate_change' AND acknowledged_at IS NULL ORDER BY 2 |] (Only testPid)
        silenced = runHasqlEffect tr (Issues.isSilenced testPid "gate-error" Issues.LogPatternRateChange =<< Time.currentTime)
        ackAs w iid = now tr >>= \t -> void (job tr $ Issues.setAckState testPid [iid] $ Just Issues.AckSet{at = t, by = Just uid, window = w})
    forM_ levels \(h, lvl) -> upsert tr "summary" h ("Gate test " <> h <> " <*>") lvl 100 >> baseline tr "summary" h
    job tr $ BackgroundJobs.calculateLogPatternBaselines testPid
    -- frozenTime is :00, so the projection floors at 15 minutes: raw 600 -> 2400/hr against a 600/hr baseline.
    mapM_ (spike . fst) levels
    job tr $ BackgroundJobs.detectLogPatternSpikes testPid frozenTime tr.trATCtx
    countIssues tr Issues.LogPatternRateChange `shouldReturn` 0
    job tr $ BackgroundJobs.detectLogPatternSpikes testPid frozenTime tr.trATCtx
    -- Lowercase levels are matched case-insensitively; DEBUG and a missing level never alert. showRate already appends "/hr".
    fired <- open
    map (\(_, h, t) -> (h, t)) fired `shouldBe` [("gate-error", "test-svc · Gate test gate-error <*> · spike 300% (2400/hr vs 600/hr)")]
    let iid = maybe (error "no rate-change issue") (\(i, _, _) -> i) (listToMaybe fired)

    ackAs (Issues.AckFor $ 24 * 60) iid
    dbOne @_ @(Maybe UTCTime) tr [sql| SELECT acknowledged_until FROM apis.issues WHERE id = ? |] (Only iid) `shouldReturn` Just (addUTCTime (24 * 3600) frozenTime)
    advanceHours tr 12
    spike "gate-error" >> detect
    open `shouldReturn` []
    silenced `shouldReturn` True
    -- +25h45m: the window lapsed, and at :45 raw 600 projects to 800/hr — a spike only because of the partial-hour scale.
    advanceHours tr 13 >> advanceMinutes tr 45
    spike "gate-error" >> detect
    refired <- open
    map (\(_, h, t) -> (h, t)) refired `shouldBe` [("gate-error", "test-svc · Gate test gate-error <*> · spike 33% (800/hr vs 600/hr)")]
    -- An indefinite ack outlives what a 24h one would have covered.
    mapM_ (\(i, _, _) -> ackAs Issues.AckIndefinite i) refired
    advanceHours tr 25
    silenced `shouldReturn` True

  it "new patterns: issue-worthy ones open issues, all are acknowledged on the test clock, then age out" \tr -> do
    runTestBg frozenTime tr pass
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
        newHashes = ["new-pat-001", "new-pat-002"] :: [Text]
        states hs = forM hs \(src, h) -> (.state) <$> byHash tr src h
        remaining (hs :: [Text]) = dbQuery @_ @(Only Text) tr [sql| SELECT pattern_hash FROM apis.log_patterns WHERE project_id = ? AND pattern_hash = ANY(?) ORDER BY 1 |] (testPid, V.fromList hs)
        ackedAt h = (fmap zonedTimeToUTC . (.acknowledgedAt)) <$> byHash tr "summary" h
        prune = job tr $ BackgroundJobs.pruneStaleLogPatterns testPid
        anchor col h t = dbExec tr ("UPDATE apis.log_patterns SET " <> col <> " = ? WHERE project_id = ? AND pattern_hash = ?") (t, testPid, h :: Text)
    forM_ newHashes \h -> upsert tr "summary" h ("New pattern " <> h <> " <*>") (Just "WARN") 10 >> stat tr "summary" h frozenTime 1500
    upsert tr "url_path" "url-path-pat-001" "/api/users/<*>" Nothing 50 >> stat tr "url_path" "url-path-pat-001" frozenTime 1500
    -- created_at defaults to the DB's wall clock; new patterns are only picked up once 15 minutes old.
    forM_ ("url-path-pat-001" : newHashes) \h -> anchor "created_at" h (addUTCTime (-900) frozenTime)
    job tr $ BackgroundJobs.processNewLogPatterns testPid tr.trATCtx
    dbQuery @_ @(Only Text) tr [sql| SELECT target_hash FROM apis.issues WHERE project_id = ? AND issue_type = 'log_pattern' ORDER BY 1 |] (Only testPid)
      `shouldReturn` map Only newHashes
    states (("url_path", "url-path-pat-001") : map ("summary",) newHashes) `shouldReturn` replicate 3 LPSAcknowledged

    -- Manual acknowledgement stamps the test clock.
    forM_ @[] ["prune-a", "prune-b", "ack-late", "stale-prune", "autoack-new"] \h -> upsert tr "summary" h ("Prune " <> h <> " <*>") (Just "INFO") 5
    job tr (LogPatterns.acknowledgeLogPatterns testPid (Just uid) (V.fromList $ map ("summary",) ["prune-a", "prune-b", "stale-prune"])) `shouldReturn` 3
    mapM @[] ackedAt ["prune-a", "prune-b"] `shouldReturn` replicate 2 (Just frozenTime)
    states (map ("summary",) ["prune-a", "prune-b", "stale-prune"]) `shouldReturn` replicate 3 LPSAcknowledged
    forM_ @[] ["prune-a", "prune-b"] \h -> anchor "last_seen_at" h frozenTime
    anchor "last_seen_at" "stale-prune" (addUTCTime (-60 * 86400) frozenTime)
    anchor "created_at" "autoack-new" frozenTime
    stat tr "summary" "stale-prune" (addUTCTime (-100 * 3600) frozenTime) 100
    -- Open discovery issues archive once their updated_at is 14 days stale.
    forM_ @[] [("autoarchive-stale-pat", Issues.LogPattern, 20), ("autoarchive-fresh-pat", Issues.LogPattern, 5), ("autoarchive-stale-rc", Issues.LogPatternRateChange, 20)] \(h, ty, d) ->
      seedIssue tr (issueSeed ty h){at = addUTCTime (-d * 86400) frozenTime}

    prune
    remaining ["prune-a", "prune-b", "stale-prune"] `shouldReturn` map Only ["prune-a", "prune-b"]
    dbOne @_ @Int tr [sql| SELECT count(*)::int FROM apis.log_pattern_hourly_stats WHERE project_id = ? AND hour_bucket < ?::timestamptz - interval '72 hours' |] (testPid, frozenTime) `shouldReturn` 0
    dbQuery @_ @(Text, Bool) tr [sql| SELECT target_hash, archived_at IS NOT NULL FROM apis.issues WHERE project_id = ? AND target_hash LIKE 'autoarchive-%' ORDER BY 1 |] (Only testPid)
      `shouldReturn` [("autoarchive-fresh-pat", False), ("autoarchive-stale-pat", True), ("autoarchive-stale-rc", True)]

    advanceHours tr 25
    void $ runHasqlEffect tr $ LogPatterns.acknowledgeLogPatterns testPid Nothing (V.singleton ("summary", "ack-late"))
    ackedAt "ack-late" `shouldReturn` Just (addUTCTime (25 * 3600) frozenTime)
    -- 'new' patterns auto-acknowledge after 7 days.
    advanceDays tr 5 >> prune
    states [("summary", "autoack-new")] `shouldReturn` [LPSNew]
    advanceDays tr 2 >> prune
    states [("summary", "autoack-new")] `shouldReturn` [LPSAcknowledged]
    ackedAt "autoack-new" `shouldReturn` Just (addUTCTime (8 * 86400 + 3600) frozenTime)
    -- Acknowledged patterns are pruned 30 days after they were last seen; a touch resets the clock.
    advanceDays tr 21 >> prune
    remaining ["prune-a", "prune-b"] `shouldReturn` map Only ["prune-a", "prune-b"]
    now tr >>= anchor "last_seen_at" "prune-a"
    advanceDays tr 2 >> prune
    remaining ["prune-a", "prune-b"] `shouldReturn` [Only "prune-a"]

  it "merging: embeddings merge look-alikes, members fold into their canonical, unmerge is project-scoped and sticky" \tr -> do
    runTestBg frozenTime tr pass
    -- The embedding/LLM cache is keyed by the request: keep these texts and the embed order byte-identical.
    upsertWithSample tr "summary" "emb-merge-a1" "user <*> logged in from <*>" (Just "INFO") 20 "user alice logged in from 10.0.0.1"
    upsertWithSample tr "summary" "emb-merge-a2" "user {uuid} logged in from {ipv4}" (Just "INFO") 15 "user bob logged in from 192.168.1.1"
    upsert tr "summary" "emb-nomerge-c1" "werkzeug.exceptions.MethodNotAllowed: {integer} Method Not Allowed" (Just "ERROR") 10
    upsert tr "summary" "emb-nomerge-c2" "CurrencyModule dependencies initialized" (Just "INFO") 8
    let canonicalOf p = dbOne @_ @(Maybe LogPatterns.LogPatternId) tr [sql| SELECT canonical_id FROM apis.log_patterns WHERE id = ? |] (Only p.id)
    job tr $ BackgroundJobs.patternEmbeddingAndMerge testPid
    -- a1 is now a centroid; re-embedding a2 compares it against a1.
    a2Before <- byHash tr "summary" "emb-merge-a2"
    dbExec tr [sql| UPDATE apis.log_patterns SET embedding = NULL, embedding_at = NULL WHERE id = ? |] (Only a2Before.id)
    job tr $ BackgroundJobs.patternEmbeddingAndMerge testPid
    [a1, a2, c1, c2] <- mapM @[] (byHash tr "summary") ["emb-merge-a1", "emb-merge-a2", "emb-nomerge-c1", "emb-nomerge-c2"]
    dbQuery @_ @(Only Bool) tr [sql| SELECT embedding IS NOT NULL FROM apis.log_patterns WHERE id = ANY(?) |] (Only $ V.fromList [a1.id, a2.id]) `shouldReturn` replicate 2 (Only True)
    canonicalOf a2 `shouldReturn` Just a1.id
    map (.id) <$> job tr (PatternMerge.getPatternGroupMembers testPid a1.id) >>= (`shouldSatisfy` elem a2.id)
    -- logCanMerge blocks the unrelated pair before any similarity check.
    canonicalOf c1 >>= (`shouldNotBe` Just c2.id)
    canonicalOf c2 >>= (`shouldNotBe` Just c1.id)
    V.length <$> job tr (LogPatterns.getLogPatternsByIds $ V.fromList [a1.id, a2.id]) `shouldReturn` 2

    -- Regression (unmergeLogPattern_otherProject_isNoop): unmerge was keyed on id alone.
    listedBefore <- length <$> job tr (LogPatterns.getLogPatterns testPid 100 0)
    otherPid <- UUIDId <$> UUID.nextRandom
    job tr (PatternMerge.unmergePattern otherPid a2.id) `shouldReturn` 0
    void $ job tr $ PatternMerge.unmergePattern testPid a2.id
    length <$> job tr (LogPatterns.getLogPatterns testPid 100 0) `shouldReturn` listedBefore + 1
    -- merge_override keeps it out of both re-embedding and canonical candidacy.
    map fst <$> job tr (PatternMerge.getUnembeddedPatterns @LogPatterns.LogPatternId testPid) >>= (`shouldNotSatisfy` elem a2.id)
    map fst <$> job tr (PatternMerge.getCanonicalPatterns @LogPatterns.LogPatternId testPid) >>= (`shouldNotSatisfy` elem a2.id)

    -- A merged member drops out of the drain texts and spike candidates; its counts roll into the canonical.
    upsert tr "summary" "canon-001" "Canonical drain <*>" (Just "WARN") 50
    upsert tr "summary" "member-001" "Member drain <*>" (Just "WARN") 50
    baseline tr "summary" "canon-001"
    job tr $ BackgroundJobs.calculateLogPatternBaselines testPid
    [canon, member] <- mapM @[] (byHash tr "summary") ["canon-001", "member-001"]
    void $ job tr $ PatternMerge.assignPatternsToCanonical [(member.id, canon.id)]
    stat tr "summary" "canon-001" frozenTime 100
    stat tr "summary" "member-001" frozenTime 70
    (texts, _) <- job tr $ LogPatterns.getLogPatternTexts testPid "summary" Nothing
    filter (`elem` ["Canonical drain <*>", "Member drain <*>"]) texts `shouldBe` ["Canonical drain <*>"]
    rates <- job tr $ LogPatterns.getPatternsWithCurrentRates testPid frozenTime
    [(r.patternHash, r.currentHourCount) | r <- rates, r.patternHash `elem` ["canon-001", "member-001"]] `shouldBe` [("canon-001", 170)]

  it "canonical embeddings: decode unboxed without retaining boxed floats, and reject malformed arrays" \tr -> do
    runTestBg frozenTime tr pass
    let canonicals = job tr (PatternMerge.getCanonicalPatterns @LogPatterns.LogPatternId testPid)
    ids <- dbQuery tr [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, embedding)
                            SELECT ?, 'embedding memory ' || n, 'embedding-memory-' || n, array_fill(0.1::real, ARRAY[1536])
                            FROM generate_series(1, 1500) AS n RETURNING id |] (Only testPid)
    performGC
    before <- gcdetails_live_bytes . gc <$> getRTSStats
    loaded <- canonicals
    _ <- evaluateWHNF $ sum $ map (VU.sum . snd) loaded
    performGC
    after <- gcdetails_live_bytes . gc <$> getRTSStats
    let inserted = filter ((`S.member` S.fromList [i | Only i <- ids]) . fst) loaded
    length inserted `shouldBe` 1500
    all (\(_, emb) -> VU.length emb == 1536 && VU.all (== 0.1) emb) inserted `shouldBe` True
    ((fromIntegral after :: Int64) - fromIntegral before) `shouldSatisfy` (< 40_000_000)
    forM_ @[] [("{{0.1,0.2},{0.3,0.4}}", "Expected one-dimensional embedding"), ("{NULL}", "Unexpected NULL")] \(embedding, message :: Text) -> do
      dbExec tr [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, embedding) VALUES (?, 'invalid embedding', 'invalid-embedding', ?::float4[]) |] (testPid, embedding :: Text)
      canonicals `shouldThrow` \(e :: SomeException) -> message `T.isInfixOf` show e
      dbExec tr [sql| DELETE FROM apis.log_patterns WHERE project_id = ? AND pattern_hash = 'invalid-embedding' |] (Only testPid)
  where
    job tr = runTestBgNoReset tr
    now tr = getTestTime tr.trTestClock
    upsert tr src h p lvl n = upsertWithSample tr src h p lvl n p
    upsertWithSample tr src h p lvl n sample =
      void $ job tr $ LogPatterns.upsertLogPattern LogPatterns.UpsertPattern{projectId = testPid, logPattern = p, hash = h, sourceField = src, serviceName = Just "test-svc", logLevel = lvl, traceId = Nothing, sampleMessage = Just sample, eventCount = n, isError = lvl == Just "ERROR"}
    stat tr src h t n = void $ job tr $ LogPatterns.upsertHourlyStat testPid src h t n
    -- 48h at 600/hr: past the 500/hr median that establishes a baseline immediately.
    baseline tr src h = forM_ ([-48 .. -1] :: [Int]) \hr -> stat tr src h (addUTCTime (fromIntegral hr * 3600) frozenTime) 600
    byHash tr src h = job tr (LogPatterns.getLogPatternByHash testPid src h) >>= maybe (fail $ "no pattern " <> toString h) pure
