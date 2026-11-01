module Pages.IssuesSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.Hasql qualified as EHasql
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.UUID qualified as DataUUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Hasql.Interpolate qualified as HI
import Lucid (toHtml)
import Models.Apis.ApiChanges qualified as ApiChanges
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Issues qualified as IssuesPage
import Pages.Telemetry qualified as Trace
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker (TimePicker (..))
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (Spec, around, describe, expectationFailure, it, shouldBe, shouldNotSatisfy, shouldReturn, shouldSatisfy)


spec :: Spec
spec = around withTestResources $ describe "Issues" do
  it "api change: discovered, acked, drifts, evidence degrades, groups by host, deep-links by hash and range" \tr -> do
    processMessagesAndBackgroundJobs tr [(k, encMsg $ testRequestMsgs.reqMsg1 isoNow) | k <- ["m1", "m2", "m3", "m4"]]
    runAnomalyJobs tr
    dbOne tr [sql| SELECT count(*)::int FROM apis.endpoints WHERE project_id=? |] (Only testPid) `shouldReturn` (1 :: Int)
    [iid] <- apiChangeIds tr
    map issueIdOf <$> listRows tr "Inbox" [] `shouldReturn` [iid]
    dbOne tr [sql| SELECT recommended_action FROM apis.issues WHERE id=? |] (Only iid) `shouldReturn` Issues.defaultRecommendedAction

    void $ call tr $ IssuesPage.acknowledgeIssueGetH testPid True iid Nothing
    fmap (isJust . (.acknowledgedAt)) <$> runTestBg frozenTime tr (Issues.selectIssueById testPid iid) `shouldReturn` Just True
    otherPid <- createTestProject tr "other"
    runTestBg frozenTime tr (Issues.selectIssueById otherPid iid) >>= (`shouldSatisfy` isNothing)
    map issueIdOf <$> listRows tr "Acknowledged" [] `shouldReturn` [iid]

    -- Field drift reaches the open issue: new fields, then type changes, never critical.
    let drift path body = processMessagesAndBackgroundJobs tr [("drift", encMsg $ apiMsg path body)] >> runAnomalyJobs tr
        driftIssue = do
          h <- dbOne tr [sql| SELECT hash FROM apis.endpoints WHERE project_id=? AND url_path='/v1/drift' |] (Only testPid)
          i <- runTestBg frozenTime tr (Issues.selectIssueByHash testPid h (Issues.OpenOfType Issues.ApiChange)) >>= maybe (fail "no open drift issue") pure
          case Issues.issuePayload i of
            Just (Issues.ApiChangeP d) -> pure (i, d)
            _ -> fail "api_change issue without an ApiChangeP payload"
        hasField f = V.any (f `T.isSuffixOf`)
    drift "/v1/drift" "eyJ1c2VybmFtZSI6ImFAYi5jbyJ9" -- {"username":"a@b.co"}
    (i0, _) <- driftIssue
    Issues.isNewEndpointOnly i0 `shouldBe` True
    drift "/v1/drift" "eyJ1c2VybmFtZSI6ImFAYi5jbyIsInBhc3N3b3JkIjoieCIsIm5pY2tuYW1lIjoiYSJ9" -- + password, nickname
    (i1, d1) <- driftIssue
    (hasField "password" d1.newFields, V.null d1.modifiedFields, i1.critical) `shouldBe` (True, True, False)
    drift "/v1/drift" "eyJ1c2VybmFtZSI6MiwicGFzc3dvcmQiOiJ4Iiwibmlja25hbWUiOjJ9" -- username, nickname: string -> number
    (i2, d2) <- driftIssue
    (hasField "password" d2.newFields, hasField "username" d2.modifiedFields, hasField "password" d2.modifiedFields, i2.id == i0.id, i2.critical, i2.severity) `shouldBe` (True, True, False, True, False, Issues.Warning)
    current <- detail tr i2.id
    -- Legacy rows captured no field evidence; NULL evidence (checked last) must not hide the issue either.
    dbExec tr [sql| UPDATE apis.anomalies SET field_evidence='[]', created_at=CASE WHEN cardinality(modified_fields)=0 THEN '2026-01-01'::timestamptz ELSE '2026-01-02'::timestamptz END WHERE project_id=? AND target_hash IN ? |] (testPid, PGS.In $ V.toList d2.anomalyHashes)
    legacy <- detail tr i2.id

    -- Same-host api changes collapse into one group without losing their endpoints.
    drift "/v1/grouped-drift" "eyJ1c2VybmFtZSI6ImFAYi5jbyJ9"
    drift "/v1/grouped-drift" "eyJ1c2VybmFtZSI6Mn0="
    dbExec tr [sql| UPDATE apis.issues SET service=NULL WHERE project_id=? AND issue_type='api_change' |] (Only testPid)
    let typed perPage = call tr $ IssuesPage.issueListGetH testPid (Just "Inbox") Nothing Nothing Nothing (Just perPage) Nothing Nothing [] ["api_change"]
    grouped <- listHtml =<< typed "100"
    singleton <- listHtml =<< typed "1"
    goldenHtml "issue-list-api-change-groups" do grouped; singleton

    -- Detail resolves the originating trace and honours relative and absolute ranges on both routes.
    (.traceRef) <$> found (detail tr iid) >>= (`shouldSatisfy` isJust)
    hashed <- found $ inner $ call tr $ IssuesPage.issueDetailHashGetH testPid i0.targetHash Nothing (Just "14D") Nothing Nothing Nothing
    hashed.tp.since `shouldBe` Just "14D"
    let (from, to) = ("2026-09-02T06:04:43Z", "2026-09-02T10:04:43Z")
    absolute <-
      forM [IssuesPage.issueDetailGetH testPid i0.id, IssuesPage.issueDetailHashGetH testPid i0.targetHash] \h ->
        found (inner $ call tr $ h Nothing Nothing (Just from) (Just to) Nothing)
    map (\v -> (v.tp.from, v.tp.to, v.isFirst)) absolute `shouldBe` replicate 2 (Just from, Just to, False)
    dbExec tr [sql| UPDATE apis.anomalies SET field_evidence='null'::jsonb WHERE project_id=? |] (Only testPid)
    unavailable <- detail tr i2.id
    goldenHtml "issue-api-change-evidence" do toHtml current; toHtml legacy; toHtml unavailable

  it "detail renders each issue type from its typed view" \tr -> do
    let errorIssue :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> IO Issues.IssueId
        errorIssue ty msg stack runtime tidM svc env = do
          let h = "err-" <> ty
          let errData = [aesonQQ|{ "when": #{frozenTime}, "error_type": #{ty}, "root_error_type": #{ty}, "message": #{msg}, "root_error_message": #{msg}, "stack_trace": #{stack}, "hash": #{h}, "is_framework": false, "runtime": #{runtime} }|]
          dbExec
            tr
            [sql| INSERT INTO apis.error_patterns (project_id, error_type, message, stacktrace, hash, runtime, first_trace_id, recent_trace_id, error_data, created_at, updated_at)
                  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) |]
            ((testPid, ty, msg, stack, h, runtime, tidM, tidM, errData) PGS.:. (frozenTime, frozenTime))
          seedIssue tr (issueSeed Issues.RuntimeException h){title = ty <> " issue", service = svc, environment = env, payload = [aesonQQ|{ "error_type": #{ty}, "error_message": #{msg}, "stack_trace": #{stack}, "occurrence_count": 1, "first_seen": #{frozenTime}, "last_seen": #{frozenTime} }|]}
    java <- errorIssue "NullPointerException" "boom" "at com.example.MyClass.doWork(MyClass.java:25)\nat org.springframework.web.servlet.DispatcherServlet.doDispatch(DispatcherServlet.java:1067)" (Just "java") Nothing (Just "checkout") Nothing
    go <- errorIssue "*errors.errorString" "shipping quote failure" "" (Just "go") Nothing (Just "checkout") Nothing
    noTrace <- errorIssue "TypeError" "boom" "" Nothing Nothing (Just "checkout") (Just "production")

    -- The trace is deferred to its own fragment, which is time-budgeted.
    apiKey <- createTestAPIKey tr testPid "trace-key"
    let spanTrace = "5d0e0000000000000000000000000001"
    ingestSpanLinked tr apiKey spanTrace "5d0e000000000001" Nothing "GET /checkout" [] frozenTime
    traced <- errorIssue "SyntaxError" "JSON parse failed" "at parse (a.js:1)" Nothing (Just spanTrace) (Just "checkout") Nothing
    fmap fst . (.traceRef) <$> found (inner $ call tr $ IssuesPage.issueDetailGetH testPid traced (Just "true") Nothing Nothing Nothing Nothing) `shouldReturn` Just spanTrace
    let fetchTrace tr' t = call tr' $ Trace.traceH testPid t (Just frozenTime) Nothing Nothing (Just "true") Nothing
        starved = tr{trATCtx = (tr.trATCtx){env = (tr.trATCtx.env){traceViewTimeoutSecs = 0}}}
    fetchTrace tr spanTrace >>= \case Trace.TraceDetails{} -> pass; _ -> expectationFailure "default budget renders the trace"
    timedOut <- fetchTrace starved spanTrace
    missing <- fetchTrace tr "missing-trace"
    case (timedOut, missing) of
      (Trace.TraceDetailsUnavailable retry _ _ Trace.TraceTimedOut, Trace.TraceDetailsUnavailable _ _ _ Trace.TraceEmpty) -> retry `shouldSatisfy` T.isInfixOf ("/traces/" <> spanTrace)
      _ -> expectationFailure "expected timed-out and empty trace states"

    -- A query alert carries its own query and monitor; its id is stored as a `show` blob.
    let monitorId = "7a3e1b52-0000-4000-8000-000000000001" :: Text
        storedQueryId = "QueryMonitorId {unQueryMonitorId = " <> monitorId <> "}"
    alert <-
      seedIssue tr (issueSeed Issues.QueryAlert "alert-hash"){title = "checkout throughput dropped", service = Just "Monitoring", payload = [aesonQQ|{ "query_id": #{storedQueryId}, "query_name": "checkout throughput", "query_expression": "resource.service.name == \"checkout\" | summarize count(*) by bin_auto(timestamp)", "threshold_value": 5, "actual_value": 0, "threshold_type": "below", "triggered_at": #{frozenTime} }|]}

    -- A log pattern shows its pattern's live counts, not the counts frozen at detection.
    let detectedAt = addUTCTime (-86400) frozenTime
        pat = "kind;text-weak\8658rpc duration;badge-neutral\8658<*> min" :: Text
    dbExec tr [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash, source_field, first_seen_at, last_seen_at, occurrence_count) VALUES (?, ?, 'live-pattern', 'summary', ?, ?, 92) |] (testPid, pat, detectedAt, addUTCTime (-120) frozenTime)
    logPattern <- seedIssue tr (issueSeed Issues.LogPattern "live-pattern"){service = Just "checkout", payload = AE.toJSON $ Issues.LogPatternData "live-pattern" pat Nothing Nothing (Just "checkout") "summary" detectedAt 2, at = detectedAt}
    ((.occurrenceCount) <$>) . (.patternM) <$> found (detail tr logPattern) `shouldReturn` Just (92 :: Int64)

    -- A span schema has no HTTP endpoint, so no trace may be looked up by placeholder method/path.
    let subject = ApiChanges.SpanSchema Catalog.emptyScope{Catalog.service = Just "checkout", Catalog.spanName = Just "process-order"}
    dbExec tr [sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, attributes___http___request___method, attributes___url___path, context___trace_id, summary) VALUES (gen_random_uuid(), ?, ?, ?, 'UNKNOWN', '/', 'placeholder-trace', '{}') |] (testPid, frozenTime, frozenTime)
    spanSchema <- seedIssue tr (issueSeed Issues.ApiChange "span-schema"){title = "Span schema changed", payload = [aesonQQ|{ "subject": #{subject}, "endpoint_method": "UNKNOWN", "endpoint_path": "/", "endpoint_host": "Unknown", "anomaly_hashes": [], "shape_changes": [], "format_changes": [], "new_fields": [], "deleted_fields": [], "modified_fields": [] }|]}
    (.traceRef) <$> found (detail tr spanSchema) `shouldReturn` Nothing

    pages <- forM [java, go, noTrace, traced, alert, logPattern, spanSchema] (detail tr)
    goldenHtml "issue-detail-types" $ traverse_ toHtml pages

  it "triage: ack, archive, priority, assign, resolve, spam, expiry, collaboration and saved views" \tr -> do
    [a, b] <- forM ([(0, "triage-a"), (60, "triage-b")] :: [(NominalDiffTime, Text)]) \(dt, h) -> seedIssue tr (issueSeed Issues.LogPattern h){title = "triage " <> h, at = addUTCTime dt frozenTime}
    member : _ <- runTestBg frozenTime tr (ProjectMembers.selectActiveProjectMembers testPid)
    let bulk action v ids = void $ call tr $ IssuesPage.issueBulkActionsPostH testPid action Nothing v IssuesPage.IssueBulk{itemId = (.toText) <$> ids}
        reload iid = runTestBg frozenTime tr (Issues.selectIssueById testPid iid) >>= maybe (fail "issue vanished") pure
        inTab tab iid = elem iid . map issueIdOf <$> listRows tr tab []

    bulk IssuesPage.BAAcknowledge Nothing [a]
    (,) <$> inTab "Inbox" a <*> inTab "Acknowledged" a `shouldReturn` (False, True)
    bulk IssuesPage.BAUnacknowledge Nothing [a]
    bulk IssuesPage.BAArchive Nothing [a]
    (,) <$> inTab "Inbox" a <*> inTab "Archived" a `shouldReturn` (False, True)
    bulk IssuesPage.BAUnarchive Nothing [a]
    inTab "Inbox" a `shouldReturn` True

    void $ call tr $ IssuesPage.triagePostH testPid a (IssuesPage.TriageForm (Just Issues.Warning) (Just member.userId.toText))
    ((.severity) &&& (.assigneeId)) <$> reload a `shouldReturn` (Issues.Warning, Just member.userId)
    bulk IssuesPage.BAPriority (Just "warning") [b]
    bulk IssuesPage.BAAssign (Just member.userId.toText) [b]
    ((.severity) &&& (.assigneeId)) <$> reload b `shouldReturn` (Issues.Warning, Just member.userId)
    listed <- listHtml =<< call tr (IssuesPage.issueListGetH testPid (Just "Inbox") (Just "-users_count") (Just "7d") Nothing (Just "100") Nothing (Just "24h") [] [])
    goldenHtml "issue-list-triaged" listed

    -- Timed archive expires; until-escalating wakes only on escalation.
    void $ call tr $ IssuesPage.archiveIssueGetH testPid a (Just (Issues.ArchiveFor 60))
    runTestBg frozenTime tr (Issues.expireArchives (addUTCTime 1800 frozenTime)) `shouldReturn` []
    runTestBg frozenTime tr (Issues.expireArchives (addUTCTime 7200 frozenTime)) `shouldReturn` [a]
    err <- seedIssue tr (issueSeed Issues.RuntimeException "triage-err")
    void $ call tr $ IssuesPage.archiveIssueGetH testPid err (Just Issues.ArchiveUntilEscalating)
    runTestBg frozenTime tr (Issues.expireArchives (addUTCTime 86400 frozenTime)) `shouldReturn` []
    runTestBg frozenTime tr (Issues.wakeOnEscalation testPid "triage-err") `shouldReturn` [err]

    -- Two acked recurrences of one signal expire into one open issue, not a failed sweep.
    void $ call tr $ IssuesPage.acknowledgeIssueGetH testPid True err (Just 1)
    again <- seedIssue tr (issueSeed Issues.RuntimeException "triage-err")
    void $ call tr $ IssuesPage.acknowledgeIssueGetH testPid True again (Just 1)
    expired <- runTestBg frozenTime tr (Issues.expireAcks (addUTCTime 120 frozenTime))
    length (filter (`elem` [err, again]) expired) `shouldBe` 1
    length . filter id <$> forM [err, again] (inTab "Inbox") `shouldReturn` 1

    bulk IssuesPage.BASpam Nothing [b]
    (,) <$> inTab "Inbox" b <*> inTab "Spam" b `shouldReturn` (False, True)
    bulk IssuesPage.BAUnspam Nothing [b]
    inTab "Spam" b `shouldReturn` False
    bulk IssuesPage.BAResolve Nothing [a, b]
    forM [a, b] (runTestBg frozenTime tr . Issues.selectLatestStateEvent) `shouldReturn` replicate 2 (Just Issues.IEResolved)

    -- Views are posted by the page once shown (GET would fire on hover), one per day; comments,
    -- links, acks and monitor recoveries share the timeline.
    replicateM_ 2 $ call tr $ IssuesPage.issueViewedPostH testPid a
    void $ call tr $ IssuesPage.commentPostH testPid a (IssuesPage.CommentForm "Rolled back the cart service")
    forM_ [IssuesPage.LinkForm "https://github.com/acme/shop/issues/42" (Just "Cart crash"), IssuesPage.LinkForm "javascript:alert(1)" Nothing] (call tr . IssuesPage.linkPostH testPid a)
    void $ call tr $ IssuesPage.acknowledgeIssueGetH testPid True a Nothing
    Just slack <- pure $ Incidents.slackPayload $ AE.object ["text" AE..= ("checkout alert" :: Text)]
    forM_ [(frozenTime, Incidents.IncidentAlert), (addUTCTime 60 frozenTime, Incidents.IncidentRecovered)] \(observedAt, change) ->
      runTestBg frozenTime tr $ Incidents.recordIncidentEvent Incidents.IncidentUpdate{projectId = testPid, source = Incidents.IssueIncident a, observedAt, change, delivery = Incidents.PublishIncident, issueId = Just a, rootPayload = slack, replyPayload = slack, destinations = []}
    dbOne tr [sql| SELECT count(*)::int FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'viewed' |] (Only a) `shouldReturn` (1 :: Int)
    activity <- call tr $ IssuesPage.issueActivityGetH testPid a Nothing Nothing
    renderT activity `shouldNotSatisfy` T.isInfixOf "javascript:alert"
    goldenHtml "issue-activity" activity

    let saved = map (.name) <$> runTestBg frozenTime tr (Issues.selectIssueViews testPid)
    void $ call tr $ IssuesPage.saveViewPostH testPid (IssuesPage.SaveViewForm "My errors" "filter=Inbox&sort=-created_at&period=24h&type=runtime_exception")
    saved `shouldReturn` ["My errors"]
    views <- forM [[], ["runtime_exception"]] \types -> listHtml =<< call tr (IssuesPage.issueListGetH testPid (Just "Inbox") Nothing Nothing Nothing Nothing Nothing (Just "24h") [] types)
    goldenHtml "issue-list-saved-views" $ sequence_ views
    runTestBg frozenTime tr (Issues.selectIssueViews testPid) >>= traverse_ (call tr . IssuesPage.deleteViewPostH testPid . (.id))
    saved `shouldReturn` []

  -- Seeded through ingestion: CI's detectors read TimeFusion, which a Postgres INSERT never reaches.
  it "detectors open performance, frontend and feedback issues with their evidence" \tr -> do
    apiKey <- createTestAPIKey tr testPid "detector-key"
    let at = (`addUTCTime` frozenTime)
        span' (sid, parent, name, q, durNs, dt) = ingestSpanReq tr . withSpanDuration durNs $ mkSpanRequest "0e7f0000000000000000000000000001" sid parent name [] Nothing (mkAttr "db.system.name" "postgresql" : [mkAttr "db.query.text" q' | Just q' <- [q]]) (mkResource apiKey [mkAttr "service.name" "catalog"]) (at dt)
        click (name, sid, label, dt) = do
          spanId <- T.replace "-" "" . DataUUID.toText <$> UUID.nextRandom
          ingestSpanLinked tr apiKey spanId spanId Nothing name [("session.id", sid), ("page.url", "https://shop.example.com/checkout"), ("monoscope.display.label", label), ("target.tag_name", "button"), ("target.id", "place-order")] (at dt)
        feedback tid msg = ingestSpanLinked tr apiKey tid tid Nothing "user.feedback" [("feedback.message", msg), ("feedback.contact_email", "ada@example.com"), ("user.name", "Ada"), ("url.full", "https://shop.example.com/cart")] (at (-60))
        detect job ty = runTestBg frozenTime tr (job testPid) >> issuesOfType tr ty :: IO [(Issues.IssueId, Text)]
    span' ("1", Nothing, "GET /products", Nothing, 900_000_000, -60)
    span' ("2", Just "1", "ProductController.index", Nothing, 800_000_000, -59)
    forM_ [1 .. 6 :: Integer] \i -> span' ("3" <> show i, Just "2", "SELECT reviews", Just ("SELECT * FROM reviews WHERE product_id = " <> show i), 20_000_000, fromIntegral (i - 58))
    span' ("4", Just "1", "SELECT promotions", Just "SELECT * FROM promotions WHERE active", 1_500_000_000, -40)
    forM_ [0, 0.4, 0.9, 1.4] \dt -> click ("click", "sess-rage", "Place order", -120 + dt)
    click ("click", "sess-rage", "Continue shopping", -100)
    forM_ ([("sess-a", -90), ("sess-b", -80)] :: [(Text, NominalDiffTime)]) \(sid, dt) -> click ("dead_click", sid, "Apply coupon", dt)
    let tid = "fb000000000000000000000000000001"
    ingestSpanReq tr . withTraceId tid =<< createOtelTraceWithExceptionAtTime apiKey ([], [], []) "POST /cart" "CartError" "boom" "CartError: boom\n    at add (/app/src/cart.js:1:1)" (at (-70))
    feedback tid "The cart button does nothing"
    feedback "fb000000000000000000000000000002" "   "

    void $ eventually (detect BackgroundJobs.detectPerformanceIssues Issues.Performance) ((== 2) . length)
    void $ eventually (detect BackgroundJobs.detectFrontendIssues Issues.Frontend) ((== 2) . length)
    void $ eventually (detect BackgroundJobs.detectFeedback Issues.Feedback) (not . null)
    -- A second pass over the same window folds into the same issues.
    perf <- detect BackgroundJobs.detectPerformanceIssues Issues.Performance
    frontend <- detect BackgroundJobs.detectFrontendIssues Issues.Frontend
    fb <- detect BackgroundJobs.detectFeedback Issues.Feedback
    map (T.takeWhile (/= ':') . snd) perf `shouldBe` ["N+1 Query", "Slow DB Query"]
    map snd frontend `shouldBe` ["Dead Click: Apply coupon", "Rage Click: Place order"]
    map snd fb `shouldBe` ["Feedback: The cart button does nothing"]
    pages <- forM (map fst $ take 1 perf <> drop 1 frontend <> fb) (detail tr)
    goldenHtml "issue-detail-detected" $ traverse_ toHtml pages

  it "events: stepping, chosen event window, user journey and range-scoped log samples" \tr -> do
    apiKey <- createTestAPIKey tr testPid "step-key"
    let at = (`addUTCTime` frozenTime)
        (ta, tb, tc) = ("57e9000000000000000000000000000a", "57e9000000000000000000000000000b", "57e9000000000000000000000000000c") :: (Text, Text, Text)
        emit tid extra dt = ingestSpanReq tr . withTraceId tid =<< createOtelTraceWithExceptionAtTime apiKey (extra, [], []) "GET /step" "StepProbeError" "step probe" "StepProbeError: step probe\n    at step (/app/src/step.js:1:1)" (at dt)
    -- The oldest carries a replay session and a user, so it is the Recommended one.
    emit ta [("session.id", "sess-1"), ("user.id", "u-1")] (-600)
    emit tb [] (-300)
    emit tc [] 0
    drainExtractionWorker tr
    void $ runAllBackgroundJobs frozenTime tr.trATCtx
    pat <- runTestBg frozenTime tr (ErrorPatterns.getErrorPatterns testPid Nothing 500 0) >>= maybe (fail "no StepProbeError") pure . find ((== "StepProbeError") . (.errorType))
    iid <- (.id) <$> (runTestBg frozenTime tr (Issues.selectIssueByHash testPid pat.hash Issues.AnyIssue) >>= maybe (fail "no issue") pure)
    let step dir from = runTestBg frozenTime tr (Telemetry.adjacentHashEvent testPid ("err:" <> pat.hash) dir from)
    mapM (uncurry step) [(Telemetry.Older, at 0), (Telemetry.Newer, at (-600)), (Telemetry.Older, at (-600)), (Telemetry.Recommended, at 1)]
      `shouldReturn` [Just (tb, at (-300)), Just (tb, at (-300)), Nothing, Just (ta, at (-600))]
    -- Past the oldest event the reader stays where they were.
    let stepTo dir ev =
          runAuthHandler tr (IssuesPage.issueStepGetH testPid iid (Just dir) (Just ev)) >>= \h -> case lookupResponseHeader @"Location" h of
            Header loc -> pure loc
            _ -> fail "no Location"
    void $ eventually (stepTo Telemetry.Older (IssuesPage.EventRef tc (at 0))) (T.isInfixOf tb)
    stepTo Telemetry.Older (IssuesPage.EventRef ta (at (-600))) >>= (`shouldSatisfy` T.isInfixOf ta)
    -- A triage edit must not move a chosen event's window off the event.
    dbExec tr [sql| UPDATE apis.issues SET updated_at=? WHERE id=? |] (at 86400, iid)
    chosen <- found $ inner $ call tr $ IssuesPage.issueDetailGetH testPid iid Nothing Nothing Nothing Nothing (Just $ IssuesPage.EventRef tb (at (-300)))
    (chosen.traceRef, chosen.tp.from, chosen.tp.to) `shouldBe` (Just (tb, at (-300)), Just "2024-12-31T21:55:00Z", Just "2025-01-01T01:55:00Z")

    -- gRPC instrumentation emits a bare `message` event per RPC message; the journey skips them.
    let journeyTrace = "70a0000000000000000000000000000a"
        checkoutEvents = [mkSpanEvent "message" [("rpc.message.type", "SENT")] (at 0), mkSpanEvent "log" [("message", "checkout declined")] (at 0)]
    forM_ @[] [(-5, "a0", "page loaded", []), (-4, "b0", "cart opened", []), (-3, "c0", "shipping selected", []), (-2, "a1", "GET /cart", []), (-1, "a2", "cart item missing price", []), (0, "a3", "POST /checkout", checkoutEvents)] \(dt, sid, name, evs) ->
      ingestSpanReq tr $ mkSpanRequest journeyTrace sid Nothing name evs Nothing [] (mkResource apiKey []) (at dt)
    journey <- eventually (call tr (IssuesPage.issueActivityGetH testPid iid (Just journeyTrace) (Just frozenTime))) (T.isInfixOf "checkout declined" . renderT)
    goldenHtml "issue-journey" journey

    -- A log pattern's sample is the latest event inside the reader's range, or the stored snapshot.
    let selectedAt = at (-86400)
        iso = toText . formatTime defaultTimeLocale "%FT%TZ"
        window dt = (Just $ iso $ addUTCTime (-60) dt, Just $ iso $ addUTCTime 60 dt)
        sampleTrace = "5a0000000000000000000000000000aa"
    forM_ [(selectedAt, "selected occurrence" :: Text), (frozenTime, "outside occurrence")] \(t, message) ->
      dbExec tr [sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, kind, hashes, summary, context___trace_id) VALUES (gen_random_uuid(), ?, ?, ?, 'log', ARRAY['pat:sample-pattern'], ARRAY[?], ?) |] (testPid, t, t, message, sampleTrace)
    sampled <- seedIssue tr (issueSeed Issues.LogPattern "sample-pattern"){service = Just "checkout", payload = AE.toJSON $ Issues.LogPatternData "sample-pattern" "payment failed <*>" (Just "saved occurrence") Nothing (Just "checkout") "body" selectedAt 2, at = selectedAt}
    let sample pid (from, to) = call tr $ IssuesPage.issueSampleGetH pid sampled Nothing from to
    sample testPid (window selectedAt) >>= \case
      IssuesPage.IssueSample _ (Just "saved occurrence") (IssuesPage.SampleFound t summary (Just tr')) -> (t, summary, tr') `shouldBe` (selectedAt, V.singleton "selected occurrence", sampleTrace)
      _ -> expectationFailure "expected the in-range event"
    sample testPid (window $ at 3600) >>= \case
      IssuesPage.IssueSample _ (Just "saved occurrence") IssuesPage.SampleEmpty -> pass
      _ -> expectationFailure "expected an empty range with the stored snapshot"
    otherPid <- createTestProject tr "other"
    sample otherPid (window selectedAt) >>= \case
      IssuesPage.NoIssueSample -> pass
      _ -> expectationFailure "another project must not read this issue's sample"

  it "tenant isolation: ack cascade, archive, error groups, unmerge and AI chat stay in their project" \tr -> do
    let uid = (Servant.getResponse tr.trSessAndHeader).user.id
    otherPid <- createTestProject tr "other"
    let seed pid h = seedIssue tr (issueSeed Issues.RuntimeException h){pid}
        acked iid = isJust <$> dbOne @_ @(Maybe UTCTime) tr [sql| SELECT acknowledged_at FROM apis.issues WHERE id=? |] (Only iid)
    -- The cascade sweeps by hash prefix, including targets shorter than the indexed prefix, and
    -- reports every swept sibling so its timeline can say why it went quiet.
    parent <- seed testPid "casc-legacy-001"
    child <- seed testPid "casc-legacy-001:child"
    foreignChild <- seed otherPid "casc-legacy-001:child"
    sort <$> runTestBg frozenTime tr (Issues.ackCascade testPid uid Issues.AckIndefinite [parent]) `shouldReturn` sort [parent, child]
    short <- seed testPid "ab1"
    shortChild <- seed testPid "ab1cdef9999"
    void $ runTestBg frozenTime tr $ Issues.ackCascade testPid uid Issues.AckIndefinite [short]
    mapM acked [child, foreignChild, shortChild] `shouldReturn` [True, False, True]

    -- A tampered bulk form naming another project's issue archives only the caller's.
    own <- seed testPid "arch-shared"
    theirs <- seed otherPid "arch-shared"
    void $ runTestBg frozenTime tr $ Issues.setArchiveState testPid [own, theirs] (Just (frozenTime, Issues.ArchiveIndefinite))
    mapM (\i -> isJust <$> dbOne @_ @(Maybe UTCTime) tr [sql| SELECT archived_at FROM apis.issues WHERE id=? |] (Only i)) [own, theirs] `shouldReturn` [True, False]

    -- Error-group reads and unmerges are scoped to the caller's project.
    [canonical, member] <- replicateM 2 UUID.nextRandom
    let errData = [aesonQQ|{"when":"2026-01-01T00:00:00Z","error_type":"SecretTypeError","root_error_type":"SecretTypeError","message":"victim-only-message","root_error_message":"victim-only-message","stack_trace":"","hash":"group-authz","is_framework":false}|]
    dbExec tr [sql| INSERT INTO apis.error_patterns (id, project_id, hash, error_type, message, stacktrace, error_data) VALUES (?, ?, 'group-canonical', 'CanonicalType', 'canonical', '', ?) |] (canonical, otherPid, errData)
    dbExec tr [sql| INSERT INTO apis.error_patterns (id, project_id, hash, error_type, message, stacktrace, error_data, canonical_id, merge_override) VALUES (?, ?, 'group-authz', 'SecretTypeError', 'victim-only-message', '', ?, ?, FALSE) |] (member, otherPid, errData, canonical)
    members <- renderT <$> call tr (IssuesPage.errorGroupMembersGetH testPid canonical)
    members `shouldNotSatisfy` T.isInfixOf "victim-only-message"
    void $ call tr $ IssuesPage.errorUnmergePostH testPid member
    dbOne tr [sql| SELECT merge_override FROM apis.error_patterns WHERE id=? |] (Only member) `shouldReturn` False

    assertNonMemberDenied tr otherPid $ IssuesPage.aiChatPostH otherPid parent (IssuesPage.AIChatForm "summarise this")
    dbOne tr [sql| SELECT count(*)::int FROM apis.ai_chat_messages WHERE project_id=? |] (Only otherPid) `shouldReturn` (0 :: Int)

    -- Hover preloads every hx-get, so no issue mutation may be reachable by GET.
    let actions = renderT do IssuesPage.issueAcknowledgeButton testPid parent frozenTime Nothing; IssuesPage.issueArchiveButton testPid parent False
    (T.count "hx-post=" actions > 2, T.count "hx-get=" actions) `shouldBe` (True, 0)

  it "the test clock drives auto-resolve stamps, the 24h window and scoped lists" \tr -> do
    let errHash = "auto-resolve-hash" :: Text
    dbExec tr [sql| INSERT INTO apis.error_patterns (project_id, error_type, message, stacktrace, hash, state, created_at, updated_at) VALUES (?, 'TestError', 'm', 's', ?, 'new', ?, ?) |] (testPid, errHash, frozenTime, frozenTime)
    resolved <- seedIssue tr (issueSeed Issues.RuntimeException errHash)
    advanceDays tr 5
    expected <- getTestTime tr.trTestClock
    runHasqlEffect tr $ EHasql.interpExecute_ [HI.sql| UPDATE apis.error_patterns SET state = 'resolved' WHERE project_id = #{testPid} AND hash = #{errHash} |]
    dbQuery tr [sql| SELECT created_at FROM apis.issue_activity_log WHERE issue_id = ? AND event = 'auto_resolved' |] (Only resolved) `shouldReturn` [Only expected]

    recent <- seedIssue tr (issueSeed Issues.RuntimeException "window-target"){at = expected}
    let window = map (\r -> (r.base.id, V.sum r.activityBuckets)) . fst <$> runHasqlEffect tr (Issues.selectIssues testPid Issues.PIssueL Issues.defIssueFilters{Issues.period = "24h", Issues.limit = 100})
    advanceHours tr 23
    find ((== recent) . fst) <$> window >>= (`shouldSatisfy` isJust)
    advanceHours tr 2
    find ((== recent) . fst) <$> window >>= (`shouldSatisfy` maybe True ((== 0) . snd))

    [prod, _, _] <- forM ([("scope-checkout", "production"), ("scope-catalog", "production"), ("scope-checkout", "staging")] :: [(Text, Text)]) \(svc, env) ->
      seedIssue tr (issueSeed Issues.RuntimeException (svc <> "-" <> env)){service = Just svc, environment = Just env}
    map (.id) . fst <$> runHasqlEffect tr (Issues.selectIssues testPid Issues.PIssue Issues.defIssueFilters{Issues.service = Just "scope-checkout", Issues.environment = Just "production", Issues.limit = 100})
      `shouldReturn` [prod]
    let scoped = tr{trSessAndHeader = (\s -> s{Projects.service = Just "scope-checkout", Projects.environment = Just "production"}) <$> tr.trSessAndHeader}
    call scoped (IssuesPage.issueListGetH testPid (Just "Inbox") Nothing Nothing Nothing (Just "100") Nothing (Just "24h") ["scope-catalog"] []) >>= rowsOf >>= (`shouldSatisfy` V.null)
  where
    isoNow = toText $ formatTime defaultTimeLocale "%FT%T%QZ" frozenTime
    encMsg = toStrict . AE.encode . Unsafe.fromJust . convert
    -- msg1 at another path with another request body.
    apiMsg path body = case testRequestMsgs.reqMsg1 isoNow of
      AE.Object o -> AE.Object $ KM.fromList [("raw_url", AE.String path), ("url_path", AE.String path), ("request_body", AE.String body)] <> o
      v -> v
    runAnomalyJobs tr = void $ runBackgroundJobsWhere frozenTime tr.trATCtx \case
      BackgroundJobs.NewAnomaly{} -> True
      _ -> False
    apiChangeIds tr = map fromOnly <$> dbQuery tr [sql| SELECT id FROM apis.issues WHERE project_id=? AND issue_type='api_change' ORDER BY created_at |] (Only testPid)
    issuesOfType tr ty = dbQuery @_ @(Issues.IssueId, Text) tr [sql| SELECT id, title FROM apis.issues WHERE project_id = ? AND issue_type = ? ORDER BY title |] (testPid, ty)
    inner = fmap \(PageCtx _ x) -> x
    detail tr iid = inner $ call tr $ IssuesPage.issueDetailGetH testPid iid Nothing Nothing Nothing Nothing Nothing
    found act =
      act >>= \case
        IssuesPage.IssueFound v -> pure v
        IssuesPage.IssueNotFound _ -> fail "issue not found"
    listPage = \case
      Table.TablePage (PageCtx bw t) -> pure (bw, t)
      _ -> fail "expected a full issue-list page"
    rowsOf = fmap ((.rows) . snd) . listPage
    -- The category and saved-view menus live in the shell's nav tabs, beside the table.
    listHtml = fmap (\(bw, t) -> sequence_ bw.navTabs >> toHtml t) . listPage
    listRows tr tab types = V.toList <$> (rowsOf =<< call tr (IssuesPage.issueListGetH testPid (Just tab) Nothing Nothing Nothing (Just "100") Nothing Nothing [] types))
    issueIdOf (IssuesPage.IssueVM _ i) = i.base.id
