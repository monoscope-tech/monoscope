module Pages.ErrorPatternsSpec (spec) where

import BackgroundJobs qualified
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as LLM
import Data.Effectful.Notify (Notification (..), SlackData (..))
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, addUTCTime, zonedTimeToUTC)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Effectful.Dispatch.Dynamic (interpose)
import Effectful.Reader.Static qualified as Reader
import Hasql.Transaction.Sessions qualified as TxS
import Lucid (toHtmlRaw)
import Models.Apis.ErrorPatterns (ErrorState (..))
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (BaselineState (..))
import Models.Apis.PatternMerge qualified as PatternMerge
import Models.Projects.Projects qualified as Projects
import Pages.AIThreads qualified as AIThreads
import Pages.BodyWrapper (PageCtx (..))
import Pages.Issues qualified
import Pkg.Components.Table qualified as Table
import Pkg.ErrorFingerprint qualified as EF
import Pkg.TestUtils
import Servant qualified
import Web.ApiHandlers qualified as ApiH
import Web.ApiTypes qualified as ApiT
import System.Config (AuthContext (..), EnvConfig (..))


spec :: Spec
spec = around withTestResources $ describe "Error patterns" do
  it "extraction: spans and error records become fingerprinted, context-rich patterns with one scoped issue each" \tr -> do
    apiKey <- createTestAPIKey tr testPid "extract-key"
    otherPid <- createTestProject tr "other"
    otherKey <- createTestAPIKey tr otherPid "other-key"
    let undefinedMsg = "Cannot read properties of undefined (reading 'id')"
        nodeStack = "TypeError: " <> undefinedMsg <> "\n    at UserController.getUser (/app/src/controllers/user.js:42:15)\n    at Layer.handle (/app/node_modules/express/lib/router/layer.js:95:5)"
        dbStack = "Error: connection failed\n    at DbPool.connect (/app/src/db.js:25:10)\n    at Server.start (/app/src/server.js:50:5)"
        ioMsg = "openBinaryFile: does not exist (No such file or directory)"
        exc = ingestTraceWithException tr apiKey
        ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36"
        contextEvent rel dt =
          ingestTraceWithExceptionAttrs
            tr
            apiKey
            ( [("user_agent.original", ua), ("geo.country.iso_code", "US"), ("geo.locality.name", "Santa Clara"), ("thread.name", "main"), ("http.request.method", "GET"), ("url.full", "https://shop.example.com/api/context?cart=7&coupon=x"), ("url.query", "cart=7&coupon=x"), ("http.request.header.x-request-id", "req-42"), ("screenshot.url", "https://cdn.example.com/shot.png"), ("attachment.url", "javascript:alert(1)")]
            , [("service.version", rel), ("deployment.environment.name", "production")]
            , [("exception.escaped", "true")]
            )
            "GET /api/context"
            "ContextError"
            "context probe"
            "ContextError: context probe\n    at probe (/app/src/probe.js:1:1)"
            (at dt)
    exc "GET /api/users/:id" "TypeError" undefinedMsg nodeStack (at (-30))
    exc "GET /api/users/:id" "TypeError" undefinedMsg nodeStack (at (-20))
    exc "GET /api/users/:id" "ConnectionRefusedError" "connect ECONNREFUSED 127.0.0.1:5432" "ConnectionRefusedError: connect ECONNREFUSED 127.0.0.1:5432\n    at TCPConnectWrap.afterConnect (/app/node_modules/pg/lib/connection.js:79:18)" (at (-10))
    exc "POST /api/data" "ConnectionError" "connect ECONNREFUSED 10.0.0.1:5432" dbStack (at 300)
    exc "POST /api/data" "ConnectionError" "connect ECONNREFUSED 192.168.1.100:5432" dbStack (at 310)
    exc "POST /api/data" "TimeoutError" "request timed out after 30000ms" dbStack (at 320)
    forM_ ["GET /api/users", "POST /api/orders", "GET /api/reports", "DELETE /api/sessions"] \sp -> exc sp "IOException" ioMsg "" (at 300)
    -- Log records carry no exception event: OTel's exception.* and the browser SDK's error.*.
    ingestErrorLog tr apiKey "user is not defined" [("exception.type", "ReferenceError"), ("exception.message", "user is not defined"), ("exception.stacktrace", "ReferenceError: user is not defined\n    at (/app.js:123:5)")] (at (-5))
    ingestErrorLog tr apiKey "" [("error.name", "TypeError"), ("error.type", "uncaught_exception"), ("error.message", "Cannot read properties of null"), ("error.stack", "TypeError: Cannot read properties of null\n    at handleClick (app.bundle.js:42:11)")] (at (-4))
    contextEvent "26.3.1" (-15)
    ingestTraceWithException tr otherKey "GET /isolated" "IsolationError" "should not appear in testPid" "IsolationError\n    at test (/app/test.js:1:1)" (at (-30))
    extract tr

    -- IPs normalise into one fingerprint, one message across span names is pre-merged, and nothing leaks across projects.
    pats <- patterns tr testPid
    sort (map (.errorType) pats) `shouldBe` ["ConnectionError", "ConnectionRefusedError", "ContextError", "IOException", "ReferenceError", "TimeoutError", "TypeError", "TypeError"]
    map (.errorType) <$> patterns tr otherPid `shouldReturn` ["IsolationError"]
    sort [(p.message, "handleClick" `T.isInfixOf` p.stacktrace) | p <- pats, p.errorType `elem` ["ReferenceError", "TypeError"]]
      `shouldBe` [("Cannot read properties of null", True), (undefinedMsg, False), ("user is not defined", False)]
    dbQuery @_ @(Text, Int) tr [sql| SELECT e.error_type, sum(h.event_count)::int FROM apis.error_hourly_stats h JOIN apis.error_patterns e ON e.id = h.error_id WHERE h.project_id = ? AND e.error_type IN ('TypeError', 'ConnectionRefusedError', 'ConnectionError') AND e.message NOT LIKE '%null%' GROUP BY 1 ORDER BY 1 |] (Only testPid)
      `shouldReturn` [("ConnectionError", 2), ("ConnectionRefusedError", 1), ("TypeError", 2)]
    bg tr (ErrorPatterns.getErrorPatternByHash testPid (EF.computeErrorFingerprint testPid.toText (Just "test-service") (Just "POST /api/data") EF.RNodejs "ConnectionError" "connect ECONNREFUSED 10.0.0.1:5432" dbStack)) >>= (`shouldSatisfy` isJust)
    io <- dbQuery @_ @(Text, Maybe ErrorPatterns.ErrorPatternId) tr [sql| SELECT hash, canonical_id FROM apis.error_patterns WHERE project_id = ? AND error_type = 'IOException' |] (Only testPid)
    (length io, length (filter (isNothing . snd) io)) `shouldBe` (4, 1)
    length . catMaybes <$> forM io (\(h, _) -> bg tr (Issues.selectIssueByHash testPid h Issues.AnyIssue)) `shouldReturn` 1

    -- The snapshot carries release, handled, environment, client, geo and only safe attachments.
    ctx <- patternOf tr "ContextError"
    let e = ctx.errorData
    (e.release, e.environment, e.handled, e.mechanism) `shouldBe` (Just "26.3.1", Just "production", Just False, Just ErrorPatterns.CMExceptionEvent)
    (e.browser, e.os, e.device, e.userAgent) `shouldBe` (Just "Chrome", Just "Windows", Just "Desktop", Just ua)
    (e.geoCountry, e.geoCity, e.threadName, e.attachments) `shouldBe` (Just "US", Just "Santa Clara", Just "main", Just ["https://cdn.example.com/shot.png"])
    bg tr $ Hasql.transaction TxS.ReadCommitted TxS.Write $ ErrorPatterns.updateErrorPatternAnalysis ctx.id "The cart line has no price" "data"
    contextEvent "26.3.2" (-14)
    extract tr
    ((.lastRelease) &&& fmap zonedTimeToUTC . (.lastReleaseSince)) <$> patternOf tr "ContextError" `shouldReturn` (Just "26.3.2", Just (at (-14)))
    -- created_at defaults to the DB's wall clock.
    dbExec tr [sql| UPDATE apis.error_patterns SET created_at = ? WHERE id = ? |] (at (-15), ctx.id)
    ctxIssue <- issueOf tr e.hash
    page <- detail tr testPid ctxIssue.id
    goldenHtml "error-detail-context" $ toHtmlRaw @Text $ maybe id (`T.replace` "<span-id>") e.spanId $ renderT page

    -- The issue is titled "<type>: <message>", shows its own trace, and is invisible to other projects.
    typeErr <- maybe (fail "no TypeError") pure $ find ((== undefinedMsg) . (.message)) pats
    issue <- issueOf tr typeErr.hash
    v <- found =<< detail tr testPid issue.id
    (issue.title, fst <$> v.traceRef) `shouldBe` ("TypeError: " <> undefinedMsg, typeErr.recentTraceId)
    detail tr otherPid issue.id >>= \case
      Pages.Issues.IssueNotFound _ -> pass
      Pages.Issues.IssueFound _ -> expectationFailure "another project read this issue"

    -- Repeat upserts must not rewrite toastable fields (157 GB of TOAST bloat in prod); a regression refreshes them.
    let mutated = (def :: ErrorPatterns.ATError){ErrorPatterns.when = frozenTime, ErrorPatterns.errorType = typeErr.errorType, ErrorPatterns.message = "MUTATED", ErrorPatterns.stackTrace = typeErr.stacktrace, ErrorPatterns.hash = typeErr.hash, ErrorPatterns.parentHash = Just "mutated-parent", ErrorPatterns.isFramework = not typeErr.isFramework}
        upsertAt err t = void $ bg tr $ ErrorPatterns.batchUpsertErrorPatterns testPid (V.singleton err) t
    upsertAt mutated frozenTime
    ((.message) &&& (.parentHash)) <$> byId tr typeErr.id `shouldReturn` (typeErr.message, typeErr.parentHash)
    void $ bg tr $ ErrorPatterns.updateErrorPatternState typeErr.id ESResolved frozenTime
    upsertAt mutated frozenTime
    ((.message) &&& (.state)) <$> byId tr typeErr.id `shouldReturn` ("MUTATED", ESRegressed)

    -- Trace-less records stored trace id '' and sent the detail page on a 40-60s scan for trace ''.
    let traceless = (mkErr "EmptyTraceError" Nothing "empty-trace-hash" "boom"){ErrorPatterns.traceId = Just ""}
        refs = bg tr (ErrorPatterns.selectErrorTraceRefs testPid "empty-trace-hash") >>= maybe (fail "no refs") pure
        refPairs r = ((r.firstTraceId, r.firstTraceAt), (r.recentTraceId, r.recentTraceAt))
    upsertAt traceless frozenTime
    ((.firstTraceId) &&& (.recentTraceId)) <$> byHash tr "empty-trace-hash" `shouldReturn` (Nothing, Nothing)
    -- A real id backfills first and refreshes recent once the 5-min HOT guard opens; a delayed later event moves only recent.
    dbExec tr [sql| UPDATE apis.error_patterns SET updated_at = ? WHERE project_id = ? AND hash = 'empty-trace-hash' |] (frozenTime, testPid)
    upsertAt traceless{ErrorPatterns.traceId = Just "abc123"} (at 400)
    refPairs <$> refs `shouldReturn` ((Just "abc123", Just frozenTime), (Just "abc123", Just frozenTime))
    upsertAt traceless{ErrorPatterns.traceId = Just "def456", ErrorPatterns.when = at 60} (at 800)
    refPairs <$> refs `shouldReturn` ((Just "abc123", Just frozenTime), (Just "def456", Just (at 60)))

    -- A sample stored before identity capture left the issue's User card anonymous forever;
    -- the first identified occurrence replaces it once, and later ones don't rewrite it.
    let anon = mkErr "AnonError" Nothing "anon-hash" "boom"
        sampleEmail = fmap ((.userEmail) . (.errorData)) . byHash tr
    upsertAt anon frozenTime
    upsertAt anon{ErrorPatterns.userEmail = Just "a@b.c"} frozenTime
    sampleEmail "anon-hash" `shouldReturn` Just "a@b.c"
    upsertAt anon{ErrorPatterns.userEmail = Just "z@y.x"} frozenTime
    sampleEmail "anon-hash" `shouldReturn` Just "a@b.c"

    -- The hourly job schedules baselines and spike detection for projects with fresh telemetry.
    bg tr $ BackgroundJobs.runHourlyJob frozenTime 0
    let errorJob = \case BackgroundJobs.ErrorBaselineCalculation p -> [("baseline" :: Text, p)]; BackgroundJobs.ErrorSpikeDetection p -> [("spike", p)]; _ -> []
    sort . filter ((== testPid) . snd) . concatMap (errorJob . snd) . V.toList <$> getPendingBackgroundJobs tr.trATCtx `shouldReturn` [("baseline", testPid), ("spike", testPid)]
    void $ runBackgroundJobsWhere frozenTime tr.trATCtx (not . null . errorJob)

  it "state: users and releases, resolve in the next release, regress, assign, subscribe and quiet decay" \tr -> do
    apiKey <- createTestAPIKey tr testPid "release-key"
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
        emit rel extra dt = ingestTraceWithExceptionAttrs tr apiKey (extra, [("service.version", rel)], []) "GET /api/release" "ReleaseError" "release probe" "ReleaseError: probe\n    at probe (/app/src/release.js:1:1)" (at dt) >> extract tr
        current = patternOf tr "ReleaseError"
    emit "1.0" [("user.id", "u-1")] (-50)
    emit "1.0" [("client.address", "10.0.0.9")] (-45)
    emit "1.0" [("user.id", "u-1")] (-40)
    p0 <- current
    (p0.firstRelease, p0.lastRelease, p0.usersCount) `shouldBe` (Just "1.0", Just "1.0", 2)
    let errUuid = p0.id.unErrorPatternId
    void $ call tr $ Pages.Issues.resolveErrorPostH testPid errUuid True
    emit "1.0" [] (-35)
    ((.state) &&& (.resolvedInRelease)) <$> current `shouldReturn` (ESResolved, Just "1.0")
    emit "1.1" [] (-30)
    p2 <- current
    (p2.state, p2.lastRelease, p2.resolvedInRelease) `shouldBe` (ESRegressed, Just "1.1", Nothing)
    -- The chart marks where each release began; a later event of the same release must not move it.
    emit "1.1" [] (-25)
    fmap zonedTimeToUTC . (.lastReleaseSince) <$> current `shouldReturn` Just (at (-30))
    -- Alerts render the stored sample: a tenant first sent on a later occurrence (id only) must reach it.
    emit "1.1" [("user.id", "u-1"), ("tenant.id", "acme")] (-22)
    ((.userId) &&& (.tenantId)) . (.errorData) <$> current `shouldReturn` (Just "u-1", Just "acme")

    -- A malformed assignee is refused instead of unassigning.
    let assign = fmap void . call tr . Pages.Issues.assignErrorPostH testPid errUuid . Pages.Issues.AssignErrorForm . Just
    assign uid.toText `shouldReturn` Right ()
    assign "bob" `shouldReturn` Left (Pages.Issues.ErrorAssignee (Pages.Issues.MalformedAssignee "bob"))
    (.assigneeId) <$> current `shouldReturn` Just uid
    void $ call tr $ Pages.Issues.resolveErrorPostH testPid errUuid False
    (.state) <$> current `shouldReturn` ESResolved
    issue <- issueOf tr p0.hash
    rows <- fmap (\(PageCtx _ t) -> V.toList t.rows) . tablePage =<< call tr (Pages.Issues.issueListGetH testPid (Just "Inbox") Nothing Nothing Nothing Nothing Nothing (Just "24h") [] [])
    [r.latestStateEvent | Pages.Issues.IssueVM _ r <- rows, r.base.id == issue.id] `shouldBe` [Just Issues.IEResolved]
    forM_ [(Just (30 :: Int), True), (Nothing, False)] \(freq, subscribed) -> do
      void $ call tr $ Pages.Issues.errorSubscriptionPostH testPid errUuid (Pages.Issues.ErrorSubscriptionForm freq)
      (.subscribed) <$> current `shouldReturn` subscribed
    emit "1.1" [] (-20)
    ((.state) &&& isJust . (.regressedAt)) <$> current `shouldReturn` (ESRegressed, True)

    -- Silence one minute short of the threshold decays counts but never infers resolution;
    -- each window then sheds the one below it (5m -= 1m, 1h -= 5m, 24h -= 1h).
    let tick = bg tr (ErrorPatterns.updateOccurrenceCountsBatch (V.singleton testPid) frozenTime) >> current
        counts p = (p.state, p.occurrences_1m, p.occurrences_5m, p.occurrences_1h, p.occurrences_24h, p.quietMinutes)
    dbExec tr [sql| UPDATE apis.error_patterns SET quiet_minutes = resolution_threshold_minutes - 1, state = 'ongoing', occurrences_1m = 0 WHERE id = ? |] (Only p0.id)
    ((.state) &&& (.occurrences_1m)) <$> tick `shouldReturn` (ESOngoing, 0)
    dbExec tr [sql| UPDATE apis.error_patterns SET occurrences_1m = 10, occurrences_5m = 30, occurrences_1h = 100, occurrences_24h = 500, state = 'ongoing', quiet_minutes = 0 WHERE id = ? |] (Only p0.id)
    map counts <$> replicateM 2 tick `shouldReturn` [(ESOngoing, 0, 20, 70, 400, 0), (ESOngoing, 0, 20, 50, 330, 1)]

  it "spikes: robust baselines escalate once, wake archived issues, de-escalate, skip ignored and re-escalate after regression" \tr -> do
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
        hourly t xs = void $ bg tr $ ErrorPatterns.upsertErrorPatternHourlyStats testPid t (V.fromList xs)
        detect t = runTestBg t tr $ BackgroundJobs.detectErrorSpikes testPid
        rate p = bg tr (ErrorPatterns.getErrorPatternsWithCurrentRates testPid frozenTime) >>= maybe (fail "no rate") pure . find ((== p.id) . (.errorId))
        stateOf p = (.state) <$> byId tr p.id
        openIssue p = bg tr (Issues.selectIssueByHash testPid p.hash (Issues.OpenOfType Issues.RuntimeException))
        spikeOver r k = ceiling (fromMaybe 0 r.baselineMean + 3 * fromMaybe 0 r.baselineStddev + k) :: Int
    [a, b, c, d] <- seed tr "SpikeError" Nothing [("spike-" <> x, "spike " <> x) | x <- ["a", "b", "c", "d"]]
    -- b's last ten hours are 5000/h outliers.
    forM_ [-50 .. -1 :: Int] \h ->
      hourly (at (fromIntegral h * 3600))
        $ [(a.hash, 600 + (h `mod` 7) * 10, 10), (b.hash, if h <= -11 then 100 + (h `mod` 10) * 10 else 5000, 5)]
        <> [(p.hash, 500 + (h `mod` 5) * 10, 10) | p <- [c, d]]
    bg tr $ BackgroundJobs.calculateErrorBaselines testPid
    pa <- byId tr a.id
    (pa.baselineState, isJust pa.baselineErrorRateStddev, pa.baselineSamples >= 24) `shouldBe` (BSEstablished, True, True)
    (.baselineErrorRateMean) <$> byId tr b.id >>= (`shouldSatisfy` maybe False (\m -> m > 80 && m < 250))

    ra <- rate a
    hourly frozenTime [(a.hash, spikeOver ra 50, 5)]
    detect frozenTime
    (,) <$> stateOf a <*> (isJust <$> openIssue a) `shouldReturn` (ESEscalating, True)
    n <- countIssues tr Issues.RuntimeException
    detect frozenTime
    countIssues tr Issues.RuntimeException `shouldReturn` n
    hourly (at 3600) [(a.hash, ceiling (fromMaybe 0 ra.baselineMean), 2)]
    detect (at 3600)
    stateOf a `shouldReturn` ESOngoing

    -- One hour: a re-spikes under an archive-until-escalating; c has a flat baseline (stddev 0)
    -- and must still clear the absolute floor; d spikes alongside; b is ignored.
    iid <- maybe (fail "no issue for a") (pure . (.id)) =<< openIssue a
    void $ bg tr $ Issues.setArchiveState testPid [iid] (Just (frozenTime, Issues.ArchiveUntilEscalating))
    dbExec tr [sql| UPDATE apis.error_patterns SET baseline_error_rate_stddev = 0, baseline_error_rate_mean = 100 WHERE id = ? |] (Only c.id)
    dbExec tr [sql| UPDATE apis.error_patterns SET is_ignored = true WHERE id = ? |] (Only b.id)
    rd <- rate d
    hourly (at 7200) [(a.hash, spikeOver ra 50, 5), (c.hash, 200, 5), (d.hash, spikeOver rd 100, 5), (b.hash, 100000, 5)]
    detect (at 7200)
    woken <- issueById tr iid
    (isNothing woken.archivedAt, woken.archiveUntilEscalating) `shouldBe` (True, False)
    dbOne tr [sql| SELECT count(*)::int FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'escalated' |] (Only iid) `shouldReturn` (1 :: Int)
    mapM stateOf [a, b, c, d] `shouldReturn` [ESEscalating, ESNew, ESEscalating, ESEscalating]
    mapM (fmap isJust . openIssue) [b, c, d] `shouldReturn` [False, True, True]

    -- Resolved, regressed by a replayed event, then spiking: escalates with a new issue.
    let regressionTime = at 9000
        spikeTime = at 10800
    void $ runTestBg regressionTime tr $ ErrorPatterns.updateErrorPatternState a.id ESResolved regressionTime
    resolvedA <- byId tr a.id
    runTestBg regressionTime tr $ BackgroundJobs.processProjectErrors testPid (V.singleton resolvedA.errorData{ErrorPatterns.when = regressionTime}) regressionTime
    ((.state) &&& fmap zonedTimeToUTC . (.regressedAt)) <$> byId tr a.id `shouldReturn` (ESRegressed, Just regressionTime)
    (open, _) <- runTestBg regressionTime tr $ Issues.selectIssues testPid Issues.PIssueL Issues.defIssueFilters{Issues.ack = Issues.IsNull, Issues.period = "24h", Issues.limit = 100}
    forM_ open \i -> runTestBg regressionTime tr $ ackIssue testPid uid i.base.id
    hourly spikeTime [(a.hash, spikeOver ra 100, 5)]
    before <- countIssues tr Issues.RuntimeException
    detect spikeTime
    (,) <$> stateOf a <*> countIssues tr Issues.RuntimeException `shouldReturn` (ESEscalating, before + 1)

  it "grouping: LLM enrichment, embedding merge and unmerge, bulk merge, applied reviews, and merged patterns stay silent" \tr -> do
    apiKey <- createTestAPIKey tr testPid "grouping-key"
    let exc = ingestTraceWithException tr apiKey
    exc "GET /api/users/:id" "TypeError" "Cannot read properties of undefined (reading 'userId')" "TypeError: Cannot read properties of undefined (reading 'userId')\n    at UserController.getUser (/app/src/controllers/user.js:42:15)\n    at Layer.handle (/app/node_modules/express/lib/router/layer.js:95:5)" (at (-30))
    exc "GET /api/orders/:id" "TypeError" "Cannot read properties of undefined (reading 'orderId')" "TypeError: Cannot read properties of undefined (reading 'orderId')\n    at OrderController.getOrder (/app/src/controllers/order.js:38:12)\n    at Layer.handle (/app/node_modules/express/lib/router/layer.js:95:5)" (at (-20))
    exc "POST /api/connect" "ConnectionRefusedError" "connect ECONNREFUSED 10.0.0.1:5432" "ConnectionRefusedError: connect ECONNREFUSED 10.0.0.1:5432\n    at TCPConnectWrap.afterConnect (/app/node_modules/pg/lib/connection.js:79:18)" (at (-10))
    forM_ ([("MergeAError", -50), ("MergeBError", -40), ("MergeCError", -35), ("MergeDError", -30)] :: [(Text, NominalDiffTime)]) \(ty, dt) -> exc "GET /api/merge" ty "merge probe" (ty <> ": merge probe\n    at probe (/app/src/" <> ty <> ".js:1:1)") (at dt)
    -- Keep the queued enrichment jobs for the deterministic LLM interpreter below.
    drainExtractionWorker tr
    void $ runBackgroundJobsWhere frozenTime tr.trATCtx quiet

    void $ call tr $ AIThreads.routineInstallPostH testPid (AIThreads.RoutineTemplateForm "issue-enrichment")
    routine <- runQueryEffect tr (Issues.getEventRoutine testPid Issues.EventIssueEnrichment) >>= maybe (fail "Missing activated enrichment template") pure
    queued <- getPendingBackgroundJobs tr.trATCtx
    let enhancements = [job | (_, job@BackgroundJobs.EnhanceIssuesWithLLM{}) <- toList queued]
        provider = interpose @LLM.LLM \_ -> \case
          LLM.CallLLM _ prompt _ ->
            pure
              $ Right
              $ if
                | "issue titler" `T.isInfixOf` prompt -> "Checkout runtime failure"
                | "issue describer" `T.isInfixOf` prompt -> "A runtime failure was observed.\nCompare the application trace.\nlow"
                | "issue-severity" `T.isInfixOf` prompt -> "critical\n0\n0"
                | otherwise -> "The fixture application dereferenced an absent value.\nruntime"
          LLM.CallAgenticChat{} -> pure $ Left "Unexpected agent call"
          LLM.EmbedDocuments{} -> pure $ Left "Unexpected embedding call"
        appCtx = tr.trATCtx{config = tr.trATCtx.config{openaiApiKey = "issue-routine-test-key"}}
    enhancements `shouldSatisfy` (not . null)
    runTestBg frozenTime tr $ Reader.local (const appCtx) $ provider $ traverse_ (BackgroundJobs.processBackgroundJob appCtx) enhancements
    executions <- runQueryEffect tr $ Issues.listConversationRuns testPid routine.conversationId 100
    executions `shouldSatisfy` (not . null)
    map (.status) executions `shouldSatisfy` all (== Issues.RunSucceeded)
    artifacts <- runQueryEffect tr $ concat <$> traverse (Issues.listRunArtifacts testPid . (.id)) executions
    mapMaybe (.content) artifacts `shouldSatisfy` any (T.isInfixOf "The fixture application dereferenced an absent value.")
    void $ call tr $ AIThreads.routinePausePostH testPid routine.conversationId

    titles <- map fromOnly <$> dbQuery tr [sql| SELECT title FROM apis.issues WHERE project_id = ? AND issue_type = 'runtime_exception' |] (Only testPid)
    titles `shouldSatisfy` any (== "Checkout runtime failure")
    dbOne tr [sql| SELECT count(*)::int FROM apis.error_patterns WHERE project_id = ? AND root_cause IS NOT NULL AND error_category IS NOT NULL |] (Only testPid) >>= (`shouldSatisfy` (>= (1 :: Int)))

    let typeEmb = [0.9, 0.3, 0.1] :: [Float]
        embed ty emb = dbExec tr [sql| UPDATE apis.error_patterns SET embedding = ?::float4[], embedding_at = NOW() WHERE project_id = ? AND error_type = ? |] (PGArray emb, testPid, ty :: Text)
        canonicalOf i = (.canonicalId) <$> byId tr i
        canonicals = bg tr (PatternMerge.getCanonicalPatterns @ErrorPatterns.ErrorPatternId testPid)
    embed "TypeError" typeEmb
    embed "ConnectionRefusedError" [0.1, 0.8, 0.4]
    [canonId, childId] <- map fromOnly <$> dbQuery tr [sql| SELECT id FROM apis.error_patterns WHERE project_id = ? AND error_type = 'TypeError' ORDER BY created_at, id |] (Only testPid)
    lookup canonId <$> canonicals `shouldReturn` Just (VU.fromList typeEmb)
    void $ bg tr $ PatternMerge.assignPatternsToCanonical [(childId, canonId)]
    canonicalOf childId `shouldReturn` Just canonId
    dbOne tr [sql| SELECT count(*)::int FROM apis.error_patterns WHERE project_id = ? AND error_type = 'ConnectionRefusedError' AND canonical_id IS NULL |] (Only testPid) `shouldReturn` (1 :: Int)
    -- An unmerge restores the pattern and fences it off from re-embedding and canonical candidacy.
    listed <- length <$> patterns tr testPid
    void $ bg tr $ PatternMerge.unmergePattern testPid childId
    length <$> patterns tr testPid `shouldReturn` listed + 1
    unembedded <- map fst <$> bg tr (PatternMerge.getUnembeddedPatterns @ErrorPatterns.ErrorPatternId testPid)
    (childId `elem` unembedded,) . (childId `elem`) . map fst <$> canonicals `shouldReturn` (False, False)

    -- Bulk merge folds the newer error into the oldest and archives its issue. One extraction pass can stamp
    -- both rows with the same created_at, so "oldest" must come from first events (seed-dependent failure, 2026-09-26).
    [ma, mb] <- forM ["MergeAError", "MergeBError"] (patternOf tr)
    [ia, ib] <- forM [ma, mb] (issueOf tr . (.hash))
    dbExec tr [sql| UPDATE apis.error_patterns SET created_at = ? WHERE id IN (?, ?) |] (frozenTime, ma.id, mb.id)
    void $ call tr $ Pages.Issues.issueBulkActionsPostH testPid Pages.Issues.BAMerge Nothing Nothing Pages.Issues.IssueBulk{itemId = (.id.toText) <$> [ib, ia]}
    ((.canonicalId) &&& (.mergeOverride)) <$> byId tr mb.id `shouldReturn` (Just ma.id, True)
    mapM (fmap (isJust . (.archivedAt)) . issueById tr . (.id)) [ia, ib] `shouldReturn` [False, True]
    dbOne tr [sql| SELECT count(*)::int FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'merged' |] (Only ib.id) `shouldReturn` (1 :: Int)
    -- The API merge the triage routine calls records why, and unmerge undoes it; an issue never merged is untouched.
    [mc, md] <- forM ["MergeCError", "MergeDError"] (patternOf tr)
    [ic, idd] <- forM [mc, md] (issueOf tr . (.hash))
    let bulk action ids = (.succeeded) <$> toBaseServantResponse tr (ApiH.apiIssuesBulk ApiT.ApiKeyPrincipal testPid ApiT.BulkAction{ApiT.action = action, ApiT.ids = ids, ApiT.durationMinutes = Nothing, ApiT.reason = Just "same throw site"})
    sort <$> bulk "merge" [idd.id, ic.id] `shouldReturn` sort [ic.id, idd.id]
    (.canonicalId) <$> byId tr md.id `shouldReturn` Just mc.id
    dbOne tr [sql| SELECT metadata->>'reason' FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'merged' |] (Only idd.id) `shouldReturn` ("same throw site" :: Text)
    bulk "unmerge" [idd.id, ic.id] `shouldReturn` [idd.id]
    ((.canonicalId) &&& (.mergeOverride)) <$> byId tr md.id `shouldReturn` (Nothing, True)
    mapM (fmap (isJust . (.archivedAt)) . issueById tr . (.id)) [ic, idd] `shouldReturn` [False, False]
    -- The routine merging again does not undo that unmerge.
    bulk "merge" [idd.id, ic.id] `shouldReturn` []
    isJust . (.archivedAt) <$> issueById tr idd.id `shouldReturn` False
    -- Unmerge reads the latest merge event, so archiving an unmerged issue does not make it unmergeable.
    void $ bulk "archive" [idd.id]
    bulk "unmerge" [idd.id] `shouldReturn` []

    -- A pattern merged after its issue exists is silent on every sweep path (canonical_id once gated only one).
    [canon, dupe] <- seed tr "MergeSilenceError" (Just "silence-shape") [("silence-canon", "canonical form"), ("silence-dupe", "duplicate form")]
    void $ bg tr $ forM_ [canon, dupe] (Issues.insertIssue <=< Issues.createNewErrorIssue testPid)
    dbExec tr [sql| UPDATE apis.error_patterns SET last_notified_at = NULL, subscribed = TRUE, state = 'new' WHERE id IN (?, ?) |] (canon.id, dupe.id)
    void $ bg tr $ PatternMerge.setCanonicalId dupe.id canon.id
    fst <$> runTestBackgroundWithNotifications frozenTime tr.trLogger tr.trATCtx (BackgroundJobs.notifyErrorSubscriptions testPid (V.singleton dupe.hash)) `shouldReturn` []
    rates <- map (.hash) <$> bg tr (ErrorPatterns.getErrorPatternsWithCurrentRates testPid frozenTime)
    ("silence-dupe" `elem` rates, "silence-canon" `elem` rates) `shouldBe` (False, True)

    -- Reverting an applied group unassigns its members and fences them: a merge the model disowned is not re-litigated.
    [rCanon, rDupe] <- seed tr "RevertError" (Just "revert-shape") [("revert-canon", "canonical form"), ("revert-dupe", "duplicate form")]
    void $ bg tr $ PatternMerge.setCanonicalId rDupe.id rCanon.id
    void $ bg tr $ PatternMerge.revertErrorGroupApply testPid "revert-shape" (V.singleton rCanon.id)
    ((.canonicalId) &&& (.mergeOverride)) <$> byId tr rDupe.id `shouldReturn` (Nothing, True)
    void $ bg tr $ PatternMerge.setCanonicalId rDupe.id rCanon.id
    canonicalOf rDupe.id `shouldReturn` Nothing

    -- The review cursor restarts at the end: a shape that gains members later must be re-examined.
    let cursor = bg tr $ PatternMerge.getReviewCursor testPid
        setCursor = void . bg tr . PatternMerge.setReviewCursor testPid
    cursor `shouldReturn` ""
    (setCursor "shape-042" >> cursor) `shouldReturn` "shape-042"
    (setCursor "" >> cursor) `shouldReturn` ""

    -- An APPLIED review pre-merges the next variant of its shape before it mints an issue; byte-equality alone missed it.
    let msgFor o = "payment declined for order SB-" <> o
        shapeOf o = (EF.computeErrorHashes testPid.toText Nothing Nothing EF.RGeneric "PreMergeError" (msgFor o) "").shape
        shapeKey = shapeOf "3FDBA2D0A0A2"
    [pCanon] <- seed tr "PreMergeError" (Just shapeKey) [("premerge-canon", msgFor "3FDBA2D0A0A2")]
    dbExec
      tr
      [sql| INSERT INTO apis.error_group_reviews (project_id, group_key, members_hash, member_count, verdict, confirmations, survived_refute, applied_at, applied_canonical_ids)
            VALUES (?, ?, 'mh', 2, 'param', 2, TRUE, NOW(), ARRAY[?]::uuid[]) |]
      (testPid, shapeKey, pCanon.id)
    shapeOf "90770E7BA9FD" `shouldBe` shapeKey
    bg tr (PatternMerge.canonicalForAppliedShape testPid shapeKey Nothing) `shouldReturn` Just pCanon.id
    bg tr $ BackgroundJobs.processProjectErrors testPid (V.singleton (mkErr "PreMergeError" (Just shapeKey) "premerge-next" (msgFor "90770E7BA9FD"))) frozenTime
    (canonicalOf . (.id) =<< byHash tr "premerge-next") `shouldReturn` Just pCanon.id
    bg tr (Issues.selectIssueByHash testPid "premerge-next" Issues.AnyIssue) >>= (`shouldSatisfy` isNothing)

  it "notifications: Slack and Discord alerts thread their reminders, and a regression alerts again" \tr -> do
    seedSlackChannel tr
    dbExec tr [sql| INSERT INTO apis.discord (project_id, guild_id) VALUES (?, 'G_TEST') ON CONFLICT (project_id) DO UPDATE SET guild_id = 'G_TEST' |] (Only testPid)
    dbExec tr [sql| UPDATE projects.teams SET discord_channels = ARRAY['DC_TEST']::text[] WHERE project_id = ? AND is_everyone = TRUE AND deleted_at IS NULL |] (Only testPid)
    apiKey <- createTestAPIKey tr testPid "notify-key"
    let emit dt = ingestTraceWithException tr apiKey "POST /api/notify" "HardenNotifyError" "notify threading probe" "HardenNotifyError: boom\n    at Handler.run (/app/src/handler.js:7:3)" (at dt) >> extract tr
    emit 400
    pat <- patternOf tr "HardenNotifyError"
    void $ call tr $ Pages.Issues.errorSubscriptionPostH testPid pat.id.unErrorPatternId (Pages.Issues.ErrorSubscriptionForm (Just 30))
    void $ runBackgroundJobsWhere frozenTime tr.trATCtx quiet
    let notifyAt t = fmap fst $ runTestBackgroundWithNotifications t tr.trLogger tr.trATCtx do
          BackgroundJobs.notifyErrorSubscriptions testPid (V.singleton pat.hash)
          replicateM_ 2 BackgroundJobs.runSlackIncidentDeliveries
        roots =
          dbQuery @_ @(Text, Maybe Text)
            tr
            [sql| SELECT r.channel_id, r.message_ts FROM apis.slack_incident_roots r JOIN apis.incident_episodes e ON e.id = r.episode_id
                  WHERE e.source_kind = 'error' AND e.source_id = ? ORDER BY r.channel_id |]
            (Only pat.id)
        discordId = (.discordMessageId) <$> byId tr pat.id
    -- Ingestion created the root; reminders must preserve it.
    notifs1 <- notifyAt (at 3600)
    let slack1 = [sd | SlackNotification sd <- notifs1]
    (null slack1, null [() | DiscordNotification _ <- notifs1]) `shouldBe` (False, False)
    -- The "View trace" link filters on the trace_id KQL alias; a style:"default" button once got every
    -- trace-bearing alert rejected with invalid_attachments.
    -- The reminder replies in one line; the root edit keeps the full card.
    [show @Text sd.payload | sd <- slack1, isNothing sd.threadTs] `shouldSatisfy` \ps -> not (null ps) && all (\p -> all (`T.isInfixOf` p) [pat.errorType, "log_explorer?query=trace_id"]) ps
    concatMap (slackPayloadViolations . (.payload)) slack1 `shouldBe` []
    roots1 <- roots
    (map fst roots1, all (isJust . snd) roots1) `shouldBe` (["C_TEST"], True)
    discord1 <- discordId
    discord1 `shouldSatisfy` isJust
    notifs2 <- notifyAt (at 7200)
    [(sd.channelId, sd.threadTs) | SlackNotification sd <- notifs2, isJust sd.threadTs] `shouldBe` roots1
    roots `shouldReturn` roots1
    discordId `shouldReturn` discord1

    void $ bg tr $ ErrorPatterns.updateErrorPatternState pat.id ESResolved frozenTime
    emit 500
    (.state) <$> byId tr pat.id `shouldReturn` ESRegressed
    dbExec tr [sql| UPDATE apis.error_patterns SET last_notified_at = NULL WHERE id = ? |] (Only pat.id)
    notifyAt (at 10800) >>= (`shouldSatisfy` not . null)
  where
    at = (`addUTCTime` frozenTime)
    bg tr = runTestBg frozenTime tr
    -- LLM-backed jobs read cached responses keyed by their inputs; only the grouping lifecycle runs them.
    quiet = \case
      BackgroundJobs.PatternEmbeddingAndMerge{} -> False
      BackgroundJobs.EnhanceIssuesWithLLM{} -> False
      BackgroundJobs.ErrorGroupReview{} -> False
      _ -> True
    extract tr = drainExtractionWorker tr >> void (runBackgroundJobsWhere frozenTime tr.trATCtx quiet)
    patterns tr pid = bg tr $ ErrorPatterns.getErrorPatterns pid Nothing 200 0
    patternOf tr ty = patterns tr testPid >>= maybe (fail $ "no " <> toString ty) pure . find ((== ty) . (.errorType))
    byId tr i = bg tr (ErrorPatterns.getErrorPatternById i) >>= maybe (fail "pattern vanished") pure
    byHash tr h = bg tr (ErrorPatterns.getErrorPatternByHash testPid h) >>= maybe (fail $ "no pattern " <> toString h) pure
    issueOf tr h = bg tr (Issues.selectIssueByHash testPid h Issues.AnyIssue) >>= maybe (fail "no issue") pure
    issueById tr i = bg tr (Issues.selectIssueById testPid i) >>= maybe (fail "issue vanished") pure
    mkErr ty shape h m = (def :: ErrorPatterns.ATError){ErrorPatterns.projectId = Just testPid, ErrorPatterns.when = frozenTime, ErrorPatterns.errorType = ty, ErrorPatterns.rootErrorType = ty, ErrorPatterns.message = m, ErrorPatterns.rootErrorMessage = m, ErrorPatterns.stackTrace = "", ErrorPatterns.hash = h, ErrorPatterns.shapeHash = shape}
    seed tr ty shape hms = do
      void $ bg tr $ ErrorPatterns.batchUpsertErrorPatterns testPid (V.fromList [mkErr ty shape h m | (h, m) <- hms]) frozenTime
      forM hms (byHash tr . fst)
    detail tr pid iid = (\(PageCtx _ x) -> x) <$> call tr (Pages.Issues.issueDetailGetH pid iid Nothing Nothing Nothing Nothing Nothing)
    found = \case
      Pages.Issues.IssueFound v -> pure v
      Pages.Issues.IssueNotFound _ -> fail "issue not found"
