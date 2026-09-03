module Pages.AnomaliesSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Effectful.Hasql qualified as EHasql
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as DataUUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Hasql.Interpolate qualified as HI
import Lucid (ToHtml, renderText, toHtml)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects (Session (..))
import Models.Projects.Projects qualified as Projects
import Pages.Anomalies qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Telemetry qualified as Trace
import Pkg.Components.Table qualified as Table
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import Test.Hspec (Spec, aroundAll, describe, expectationFailure, it, sequential, shouldBe, shouldSatisfy)


spec :: Spec
-- Examples seed state for later ones ("previous test" assertions below), so this
-- keeps aroundAll and runs sequentially — opting out of the suite's per-test
-- isolation + parallelism (same as GitSyncSpec).
spec = sequential $ aroundAll withTestResources do
  describe "Check Anomaly List" do
    it "should return an empty list" \tr -> do
      rows <- listAnomalies tr Nothing
      length rows `shouldBe` 0

    -- Preloading a lifecycle action would fire it on hover. Assert the invariant
    -- rather than a count, so adding a duration option can't silently arm one.
    it "does not preload issue acknowledge or archive actions" \_ -> do
      issueId <- UUIDId <$> UUID.nextRandom
      let html = TL.toStrict $ renderText do
            AnomalyList.anomalyAcknowledgeButton testPid issueId frozenTime Nothing
            AnomalyList.anomalyArchiveButton testPid issueId False
      T.count "hx-get=" html `shouldSatisfy` (> 2) -- ack, its duration options, archive
      T.count "preload=\"false\"" html `shouldBe` T.count "hx-get=" html

    it "should create endpoint anomalies (not visible in anomaly list)" \tr -> do
      nowTxt <- nowText
      let reqMsg1 = encMsg $ testRequestMsgs.reqMsg1 nowTxt
      -- same message four times
      processMessagesAndBackgroundJobs tr [(k, reqMsg1) | k <- ["m1", "m2", "m3", "m4"]]

      countQ tr [sql| SELECT COUNT(*)::INT FROM apis.endpoints WHERE project_id=? |] (Only testPid)
        >>= (`shouldBe` 1)

      -- NewAnomaly jobs are what create issues from anomalies
      runAnomalyJobs tr \case
        BackgroundJobs.NewAnomaly{} -> True
        _ -> False
      createTestSpans tr testPid 10

      apiChangeIssueIds tr >>= \issues -> length issues `shouldBe` 1
      -- API change issues are visible in the anomaly list
      anomalies <- listAnomalies tr Nothing
      length anomalies `shouldBe` 1

    it "should acknowledge endpoint anomaly" \tr -> do
      issues <- apiChangeIssueIds tr
      length issues `shouldBe` 1
      issueId <- maybe (fail "Expected at least one API change issue") pure $ listToMaybe issues

      runAnomalyJobs tr shapeOrField

      -- Acknowledge the endpoint anomaly directly using Issues module
      let sess = Servant.getResponse tr.trSessAndHeader
      runTestBg frozenTime tr $ ackIssue testPid sess.user.id issueId

      acknowledgedIssue <- runTestBg frozenTime tr $ Issues.selectIssueById testPid issueId
      maybe (error "Issue not found after acknowledgment") (isJust . (.acknowledgedAt)) acknowledgedIssue `shouldBe` True

      -- Regression (selectIssueById_otherProject_returnsNothing): the lookup used to be
      -- unscoped, so any tenant knowing an issue id could read the row.
      otherPid <- UUIDId @"project" <$> UUID.nextRandom
      -- isNothing rather than `shouldReturn Nothing`: Issue has no Eq instance.
      runTestBg frozenTime tr (Issues.selectIssueById otherPid issueId) >>= (`shouldSatisfy` isNothing)

      -- After acknowledging, the issue should appear in the Acknowledged filter
      acked <- listAnomalies tr (Just "Acknowledged")
      length (V.filter isApiChange acked) `shouldSatisfy` (> 0)

    it "should detect new shape anomaly after processing new messages" \tr -> do
      nowTxt <- nowText
      let reqMsg2 = encMsg $ testRequestMsgs.reqMsg2 nowTxt
      processMessagesAndBackgroundJobs
        tr
        [ ("m1", encMsg $ testRequestMsgs.reqMsg1 nowTxt)
        , ("m2", reqMsg2)
        , ("m3", encMsg $ msgWithBody shapeBody nowTxt) -- same endpoint, different shape
        , ("m4", reqMsg2)
        ]
      createTestSpans tr testPid 10

      runAnomalyJobs tr shapeOrField

      -- Issues exist in the database (they may be acknowledged from previous test)
      apiChangeIssueIds tr >>= \issues -> length issues `shouldSatisfy` (>= 1)

    it "should detect new format anomaly" \tr -> do
      nowTxt <- nowText
      runAnomalyJobs tr shapeOrField

      apiChangeIssueIds tr >>= \issues -> length issues `shouldSatisfy` (>= 1)
      issueId <- pickApiChangeIssue tr
      let sess = Servant.getResponse tr.trSessAndHeader
      runTestBg frozenTime tr $ ackIssue testPid sess.user.id issueId

      -- Now send a message with a different format
      processMessagesAndBackgroundJobs tr [("m4", encMsg $ msgWithBody formatBody nowTxt)]
      runAnomalyJobs tr \case
        BackgroundJobs.NewAnomaly{anomalyType = "format"} -> True
        _ -> False

      -- In the new Issues system, format anomalies are part of API changes
      anomalies <- listAnomalies tr Nothing
      length (V.filter isApiChange anomalies) `shouldSatisfy` (>= 1)
      length anomalies `shouldSatisfy` (> 0)

    it "should get acknowledged anomalies" \tr -> do
      rows <- listAnomalies tr (Just "Acknowledged")
      -- We acknowledged at least one API change issue in the previous test
      length (V.filter isApiChange rows) `shouldSatisfy` (>= 1)
      length rows `shouldSatisfy` (> 0)

    it "filters, paginates, and renders an empty anomaly list through the page handler" \tr -> do
      -- Put the issues created above back in one customer-visible list. The bulk handler
      -- is the same action the Acknowledged tab offers, and avoids fixture-only state.
      acknowledged <- listAnomalies tr (Just "Acknowledged")
      let acknowledgedIds = [issue.base.id.toText | AnomalyList.IssueVM _ _ issue <- V.toList acknowledged]
      unless (null acknowledgedIds) do
        void $ testServant tr $ AnomalyList.anomalyBulkActionsPostH testPid "unacknowledge" Nothing AnomalyList.AnomalyBulk{itemId = acknowledgedIds}

      let load page perPage services types = do
            (_, response) <- testServant tr $ AnomalyList.anomalyListGetH testPid (Just "Inbox") Nothing Nothing (Just $ show page) (Just $ show perPage) Nothing (Just "24h") services types
            case response of
              AnomalyList.ALPage (PageCtx _ table) -> pure (response, table)
              _ -> fail "expected a full anomaly-list page"

      (_, allIssues) <- load 0 100 [] []
      V.length allIssues.rows `shouldSatisfy` (>= 2)
      sample <- maybe (fail "expected an issue with a service") pure $ V.find (isJust . (.base.service) . issueOf) allIssues.rows
      service <- maybe (fail "expected an issue service") pure $ (issueOf sample).base.service
      let issueType = display (issueOf sample).base.issueType

      (_, filtered) <- load 0 100 [service] [issueType]
      filtered.rows `shouldSatisfy` (not . V.null)
      for_ filtered.rows \row -> do
        (issueOf row).base.service `shouldBe` Just service
        display (issueOf row).base.issueType `shouldBe` issueType

      (_, firstPage) <- load 0 1 [] []
      (_, secondPage) <- load 1 1 [] []
      V.length firstPage.rows `shouldBe` 1
      V.length secondPage.rows `shouldBe` 1
      firstIssue <- onlyIssue firstPage.rows
      secondIssue <- onlyIssue secondPage.rows
      issueIdOf firstIssue `shouldSatisfy` (/= issueIdOf secondIssue)

      (emptyResponse, emptyTable) <- load 0 25 ["service-that-does-not-exist"] []
      emptyTable.rows `shouldSatisfy` V.null
      renderPage emptyResponse `shouldSatisfy` T.isInfixOf "Nothing to triage"

      -- Timed acknowledgement expiry uses the same uniqueness invariant as manual
      -- unacknowledge. Recreate two acknowledged recurrences and prove the sweep
      -- leaves one actionable issue instead of failing the whole background job.
      let uid = (Servant.getResponse tr.trSessAndHeader).user.id
          selectedIds = mapMaybe (fmap UUIDId . DataUUID.fromText) acknowledgedIds
      void $ runTestBg frozenTime tr $ Issues.setAckState testPid selectedIds $ Just Issues.AckSet{at = frozenTime, by = Just uid, window = Issues.AckFor 1}
      void $ runTestBg frozenTime tr $ Issues.setArchiveState testPid selectedIds Nothing
      expired <- runTestBg frozenTime tr $ Issues.expireAcks (addUTCTime 120 frozenTime)
      length (filter (`elem` selectedIds) expired) `shouldBe` 1
      inboxAfterExpiry <- listAnomalies tr Nothing
      archivedAfterExpiry <- listAnomalies tr (Just "Archived")
      length (filter (`elem` selectedIds) $ map issueIdOf $ V.toList inboxAfterExpiry) `shouldBe` 1
      length (filter (`elem` selectedIds) $ map issueIdOf $ V.toList archivedAfterExpiry) `shouldBe` length selectedIds - 1

    -- Regression: posting via the bulk handler used the wrong form field name
    -- (`anomalyId` instead of `itemId`) so selected ids were silently dropped and
    -- the handler reported "No items selected". Subsequent attempt to read
    -- `anomaly_hashes` as a column returned 500. This test goes through the real
    -- handler so both bugs are caught together.
    it "bulk acknowledge cascades from issues to underlying anomalies" \tr -> do
      issueId <- pickApiChangeIssue tr

      -- Reset state — earlier tests in this describe block may have acknowledged it.
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| UPDATE apis.issues    SET acknowledged_at=NULL, acknowledged_by=NULL WHERE id=? |] (Only issueId)
        void $ PGS.execute conn [sql| UPDATE apis.anomalies SET acknowledged_at=NULL, acknowledged_by=NULL WHERE project_id=? |] (Only testPid)

      _ <- testServant tr $ AnomalyList.anomalyBulkActionsPostH testPid "acknowledge" Nothing AnomalyList.AnomalyBulk{itemId = [DataUUID.toText issueId.unUUIDId]}

      countQ tr [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE id=? AND acknowledged_at IS NOT NULL |] (Only issueId)
        >>= (`shouldBe` 1)
      -- Cascade: every anomaly referenced via issue_data->'anomaly_hashes' must be acknowledged.
      fst <$> cascadePending tr issueId >>= (`shouldBe` 0)

    -- Regression: archive path posted to apis.anomalies by issue id (which is
    -- never an anomaly id), so the cascade silently no-op'd and acknowledged_at
    -- never propagated. Now archives apis.issues by id and cascades through
    -- issue_data.anomaly_hashes; this test asserts both halves fire.
    it "bulk archive cascades to anomalies referenced by issue_data.anomaly_hashes" \tr -> do
      issueId <- pickApiChangeIssue tr
      let containsIssue = V.any \(AnomalyList.IssueVM _ _ issue) -> issue.base.id == issueId

      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| UPDATE apis.issues    SET archived_at=NULL, acknowledged_at=NULL, acknowledged_by=NULL WHERE id=? |] (Only issueId)
        void $ PGS.execute conn [sql| UPDATE apis.anomalies SET archived_at=NULL, acknowledged_at=NULL, acknowledged_by=NULL WHERE project_id=? |] (Only testPid)

      inboxBefore <- listAnomalies tr Nothing
      containsIssue inboxBefore `shouldBe` True

      _ <- testServant tr $ AnomalyList.anomalyBulkActionsPostH testPid "archive" Nothing AnomalyList.AnomalyBulk{itemId = [DataUUID.toText issueId.unUUIDId]}

      inboxAfter <- listAnomalies tr Nothing
      containsIssue inboxAfter `shouldBe` False
      archived <- listAnomalies tr (Just "Archived")
      containsIssue archived `shouldBe` True
      countQ tr [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE id=? AND archived_at IS NOT NULL |] (Only issueId)
        >>= (`shouldBe` 1)
      snd <$> cascadePending tr issueId >>= (`shouldBe` 0)

    -- Regression: the issues UPDATE in acknowlegeCascade used target_hash=ANY() with
    -- %-suffixed prefixes, so the legacy-hash sweep never matched any issue row.
    it "acknowlegeCascade prefix-sweeps issues as well as anomalies" \tr -> do
      runTestBg frozenTime tr pass
      let sess = Servant.getResponse tr.trSessAndHeader
          prefix = "casc-legacy-001" :: Text
      iid <- UUIDId <$> UUID.nextRandom
      withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.issues
                  (id, project_id, issue_type, target_hash, endpoint_hash, title,
                   severity, critical, affected_requests, affected_clients,
                   issue_data, created_at, updated_at)
                VALUES (?, ?, 'runtime_exception', ?, ?, 't', 'warning',
                        false, 1, 1, '{}'::jsonb, ?, ?) |]
            (iid, testPid, prefix <> ":child", prefix <> ":child", frozenTime, frozenTime)
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.anomalies (project_id, target_hash)
                VALUES (?, ?) ON CONFLICT DO NOTHING |]
            (testPid, prefix <> ":anom")

      -- A second project holding the SAME hash. The sweep is by content-derived hash, which is
      -- not unique across projects, so an unscoped sweep silences another tenant's issue —
      -- and unscoped it also could not use any index on these tables (every one leads with
      -- project_id), which is what made acknowledging an issue a pair of sequential scans.
      otherPid <- UUIDId <$> UUID.nextRandom
      otherIid <- UUIDId <$> UUID.nextRandom
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| INSERT INTO projects.projects (id, title, description, active) VALUES (?, 'other', '', true) ON CONFLICT DO NOTHING |] (Only otherPid)
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.issues
                  (id, project_id, issue_type, target_hash, endpoint_hash, title,
                   severity, critical, affected_requests, affected_clients,
                   issue_data, created_at, updated_at)
                VALUES (?, ?, 'runtime_exception', ?, ?, 't', 'warning',
                        false, 1, 1, '{}'::jsonb, ?, ?) |]
            (otherIid, otherPid, prefix <> ":child", prefix <> ":child", frozenTime, frozenTime)

      void $ runTestBg frozenTime tr $ Anomalies.acknowlegeCascade testPid sess.user.id Issues.indefiniteUntil (V.singleton prefix)

      countQ tr [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE id=? AND acknowledged_at IS NULL |] (Only otherIid)
        >>= (`shouldBe` 1)

      countQ tr [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE id=? AND acknowledged_at IS NOT NULL |] (Only iid)
        >>= (`shouldBe` 1)
      countQ
        tr
        [sql| SELECT COUNT(*)::INT FROM apis.anomalies
              WHERE project_id=? AND target_hash=? AND acknowledged_at IS NOT NULL |]
        (testPid, prefix <> ":anom")
        >>= (`shouldBe` 1)

    -- Regression: archiveAnomaliesAndIssues took no project at all. The issue ids come off a
    -- bulk-action form, so any tenant could archive any issue by id; and the anomaly cascade
    -- matched on a content-derived target_hash, which collides across tenants.
    it "archiveAnomaliesAndIssues_crossTenant_archivesOnlyTheCallersProject" \tr -> do
      runTestBg frozenTime tr pass
      let sharedHash = "arch-shared-hash-01" :: Text
          insertIssue conn iid pid =
            void
              $ PGS.execute
                conn
                [sql| INSERT INTO apis.issues
                        (id, project_id, issue_type, target_hash, endpoint_hash, title,
                         severity, critical, affected_requests, affected_clients,
                         issue_data, created_at, updated_at)
                      VALUES (?, ?, 'runtime_exception', ?, ?, 't', 'warning',
                              false, 1, 1, jsonb_build_object('anomaly_hashes', jsonb_build_array(?::text)), ?, ?) |]
                (iid, pid, sharedHash, sharedHash, sharedHash, frozenTime, frozenTime)
      ownIid <- UUIDId <$> UUID.nextRandom
      otherPid <- UUIDId <$> UUID.nextRandom
      otherIid <- UUIDId <$> UUID.nextRandom
      withResource tr.trPool \conn -> do
        void $ PGS.execute conn [sql| INSERT INTO projects.projects (id, title, description, active) VALUES (?, 'other-arch', '', true) ON CONFLICT DO NOTHING |] (Only otherPid)
        insertIssue conn ownIid testPid
        insertIssue conn otherIid otherPid
        forM_ [testPid, otherPid] \p ->
          PGS.execute conn [sql| INSERT INTO apis.anomalies (project_id, target_hash) VALUES (?, ?) ON CONFLICT DO NOTHING |] (p, sharedHash)

      -- Both ids submitted together, exactly as a tampered bulk form would.
      void
        $ runTestBg frozenTime tr
        $ Anomalies.archiveAnomaliesAndIssues testPid (V.fromList $ DataUUID.toText . (.unUUIDId) <$> [ownIid, otherIid])

      let archivedIssue iid = countQ tr [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE id=? AND archived_at IS NOT NULL |] (Only iid)
          archivedAnomaly p = countQ tr [sql| SELECT COUNT(*)::INT FROM apis.anomalies WHERE project_id=? AND target_hash=? AND archived_at IS NOT NULL |] (p, sharedHash)
      archivedIssue ownIid >>= (`shouldBe` 1)
      archivedAnomaly testPid >>= (`shouldBe` 1)
      archivedIssue otherIid >>= (`shouldBe` 0)
      archivedAnomaly otherPid >>= (`shouldBe` 0)

    -- The sweep narrows on LEFT(target_hash, 8) to reach migration 0130's index, which is only
    -- a valid superset while the target is at least that wide. A shorter one must fall back to
    -- the plain LIKE rather than silently matching nothing — this is the case where the fast
    -- path would be wrong, so it is the one worth a test of its own.
    it "still sweeps a target shorter than the indexed hash prefix" \tr -> do
      runTestBg frozenTime tr pass
      let sess = Servant.getResponse tr.trSessAndHeader
          shortPrefix = "ab1" :: Text
      shortIid <- UUIDId <$> UUID.nextRandom
      withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.issues
                  (id, project_id, issue_type, target_hash, endpoint_hash, title,
                   severity, critical, affected_requests, affected_clients,
                   issue_data, created_at, updated_at)
                VALUES (?, ?, 'runtime_exception', ?, ?, 't', 'warning',
                        false, 1, 1, '{}'::jsonb, ?, ?) |]
            (shortIid, testPid, shortPrefix <> "cdef9999", shortPrefix <> "cdef9999", frozenTime, frozenTime)

      void $ runTestBg frozenTime tr $ Anomalies.acknowlegeCascade testPid sess.user.id Issues.indefiniteUntil (V.singleton shortPrefix)

      countQ tr [sql| SELECT COUNT(*)::INT FROM apis.issues WHERE id=? AND acknowledged_at IS NOT NULL |] (Only shortIid)
        >>= (`shouldBe` 1)

    -- Regression: createAPIChangeIssue hard-coded a string that differed from
    -- defaultRecommendedAction, so the detail page's "not yet LLM-enhanced" check
    -- never matched api_change issues and always rendered the boilerplate.
    it "api_change issues carry defaultRecommendedAction" \tr -> do
      -- Seed through the real pipeline when running in isolation; the file's
      -- earlier tests normally created the api_change issue already.
      existing <- apiChangeIssueIds tr
      when (null existing) do
        nowTxt <- nowText
        processMessagesAndBackgroundJobs tr [("m1", encMsg $ testRequestMsgs.reqMsg1 nowTxt)]
        runAnomalyJobs tr \case
          BackgroundJobs.NewAnomaly{} -> True
          _ -> False
      ras <- withResource tr.trPool \conn ->
        PGS.query
          conn
          [sql| SELECT DISTINCT recommended_action FROM apis.issues WHERE project_id=? AND issue_type='api_change' |]
          (Only testPid)
          :: IO [Only Text]
      map fromOnly ras `shouldBe` [Issues.defaultRecommendedAction]

    -- Regression: the ApiChange detail page used to render an empty Investigation
    -- panel because hashPrefix returned Nothing for ApiChange and there was no
    -- query to find the originating trace. Now hashPrefix is "" and
    -- getEndpointTraceId locates spans by (method, url_path); seed one and
    -- assert the rendered page surfaces its trace id.
    it "new-endpoint issue detail surfaces originating trace via getEndpointTraceId" \tr -> do
      issueId <- pickApiChangeIssue tr
      -- Un-archive so the detail handler renders normally
      withResource tr.trPool \conn ->
        void $ PGS.execute conn [sql| UPDATE apis.issues SET archived_at=NULL WHERE id=? |] (Only issueId)
      traceIdText <- DataUUID.toText <$> UUID.nextRandom
      spanIdText <- DataUUID.toText <$> UUID.nextRandom
      withResource tr.trPool \conn ->
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO otel_logs_and_spans
                (id, project_id, timestamp, start_time,
                 attributes___http___request___method, attributes___url___path,
                 context___trace_id, context___span_id,
                 context, kind, status_code, summary)
              VALUES (gen_random_uuid(), ?, ?, ?, 'GET', '/', ?, ?,
                      jsonb_build_object('trace_id', ?::text, 'span_id', ?::text),
                      'SERVER', '200', '{}') |]
            (testPid, frozenTime, frozenTime, traceIdText, spanIdText, traceIdText, spanIdText)

      (_, pageById) <- testServant tr $ AnomalyList.anomalyDetailGetH testPid issueId Nothing Nothing
      -- The trace id should be embedded somewhere in the rendered investigation panel.
      renderPage pageById `shouldSatisfy` (traceIdText `T.isInfixOf`)

      issue <- runTestBg frozenTime tr $ Issues.selectIssueById testPid issueId
      let targetHash = maybe (error "Expected API change issue") (.targetHash) issue
      (_, pageByHash) <- testServant tr $ AnomalyList.anomalyDetailHashGetH testPid targetHash Nothing (Just "14D")
      renderPage pageByHash `shouldSatisfy` T.isInfixOf "since=14D"

    -- Regression: an issue that never captured a trace id used to render the logs tab as
    -- `context___trace_id==""`, a predicate that filters nothing — so the tab fetched the
    -- project's entire retention window (~6s / 550KB of unrelated logs, on ~24% of issues).
    -- With no trace to pin to it must scope to the issue's service over a bounded window.
    it "issue detail with no trace id scopes the logs tab to service and a bounded window" \tr -> do
      noTraceHash <- T.take 8 . DataUUID.toText <$> UUID.nextRandom
      issueId <- withResource tr.trPool \conn ->
        -- INSERT ... RETURNING always yields the row, but `head` is partial and
        -- -Werror=x-partial rejects it; an empty result should say so, not crash later.
        maybe (fail "INSERT ... RETURNING id returned no row") (pure . fromOnly)
          . listToMaybe
          =<< PGS.query
            conn
            [sql| INSERT INTO apis.issues (project_id, issue_type, title, target_hash, service, created_at, updated_at)
                  VALUES (?, 'runtime_exception', 'no-trace issue', ?, 'checkout', ?, ?) RETURNING id |]
            (testPid, noTraceHash, frozenTime, frozenTime)
      (_, page) <- testServant tr $ AnomalyList.anomalyDetailGetH testPid (UUIDId issueId) Nothing Nothing
      let html = renderPage page
      -- The closed global-data drawer remains mounted on an issue page. Its transformed panel
      -- must be clipped by the drawer, or it creates a page-level horizontal scroll range.
      html `shouldSatisfy` T.isInfixOf "drawer-side top-0 left-0 w-full h-full flex z-10000 overflow-y-scroll overflow-x-hidden"
      html `shouldSatisfy` not . T.isInfixOf "context___trace_id%3D%3D%22%22"
      html `shouldSatisfy` T.isInfixOf "service%3D%3D%22checkout%22"
      -- Lucid escapes the attribute, so the separators render as &amp; — assert on both
      -- ends of the window: an unbounded fallback would carry neither.
      html `shouldSatisfy` T.isInfixOf "&amp;from="
      html `shouldSatisfy` T.isInfixOf "&amp;to="

    -- Regression: a query alert used to render its KQL string and nothing else — not
    -- the threshold, not the value that crossed it, no chart, no monitor link. Below
    -- that, the Investigation panel reserved lg:h-[70vh] to say "No trace data
    -- available", which a query alert can never have (mTraceRef is Nothing by
    -- construction for this type).
    it "query alert issue shows the breach and no trace panel" \tr -> do
      monitorId <- DataUUID.toText <$> UUID.nextRandom
      alertHash <- T.take 8 . DataUUID.toText <$> UUID.nextRandom
      issueId <- withResource tr.trPool \conn ->
        maybe (fail "INSERT ... RETURNING id returned no row") (pure . fromOnly)
          . listToMaybe
          =<< PGS.query
            conn
            [sql| INSERT INTO apis.issues (project_id, issue_type, title, target_hash, service, issue_data, created_at, updated_at)
                  VALUES (?, 'query_alert', 'checkout throughput dropped', ?, 'Monitoring', ?::jsonb, ?, ?) RETURNING id |]
            ( testPid
            , alertHash
            , AE.encode
                -- query_id is stored as a `show` of the newtype, which is what every
                -- row already in the table looks like; the page must still find the id.
                -- Hoisted out of the quasi-quote: aesonQQ parses the braces inside
                -- this Haskell string literal as its own interpolation syntax.
                let storedQueryId = "QueryMonitorId {unQueryMonitorId = " <> monitorId <> "}"
                 in [aesonQQ|{ "query_id": #{storedQueryId}
                         , "query_name": "checkout throughput", "query_expression": "resource.service.name == \"checkout\" | summarize count(*) by bin_auto(timestamp)"
                         , "threshold_value": 5, "actual_value": 0, "threshold_type": "below"
                         , "triggered_at": #{frozenTime} }|]
            , frozenTime
            , frozenTime
            )
      (_, page) <- testServant tr $ AnomalyList.anomalyDetailGetH testPid (UUIDId issueId) Nothing Nothing
      let html = renderPage page
      -- The two numbers that were compared, and the direction, are all on the page.
      html `shouldSatisfy` T.isInfixOf "Threshold Breach"
      html `shouldSatisfy` T.isInfixOf "Actual"
      html `shouldSatisfy` T.isInfixOf "Threshold (below)"
      -- The alert's own query is charted, carrying its threshold as a mark line.
      html `shouldSatisfy` T.isInfixOf "alertThreshold: 5.0"
      -- The monitor id survives the `show` blob it is stored inside.
      html `shouldSatisfy` T.isInfixOf ("/monitors/" <> monitorId <> "/overview")
      -- ...and the void is gone: no trace panel, and no boilerplate subtitle.
      html `shouldSatisfy` not . T.isInfixOf "No trace data available"
      html `shouldSatisfy` not . T.isInfixOf Issues.queryAlertRecommendedAction

    -- Regression: a 1300-span trace read cold from TimeFusion took 56s, so the whole
    -- issue page 504'd behind the gateway. The trace is supporting evidence — past
    -- 'traceViewTimeoutSecs' the page must still render everything else.
    it "issue detail defers the trace to its own fragment and survives a starved trace fetch" \tr -> do
      errHash <- T.take 8 . DataUUID.toText <$> UUID.nextRandom
      traceIdText <- T.replace "-" "" . DataUUID.toText <$> UUID.nextRandom
      spanIdText <- T.take 16 . T.replace "-" "" . DataUUID.toText <$> UUID.nextRandom
      -- Ingested, not INSERTed: the fragment reads whichever store the env points
      -- at (TimeFusion in CI), and a raw INSERT only ever lands in Postgres.
      apiKey <- createTestAPIKey tr testPid "slow-trace-key"
      ingestSpanLinked tr apiKey traceIdText spanIdText Nothing "GET /checkout" [] frozenTime
      issueId <- withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns
                  (project_id, error_type, message, stacktrace, hash, first_trace_id, recent_trace_id, error_data, created_at, updated_at)
                VALUES (?, 'SyntaxError', 'JSON parse failed', 'at parse (a.js:1)', ?, ?, ?, ?::jsonb, ?, ?) |]
            ( testPid
            , errHash
            , traceIdText
            , traceIdText
            , AE.encode
                [aesonQQ|{ "when": #{frozenTime}, "error_type": "SyntaxError", "root_error_type": "SyntaxError"
                       , "message": "JSON parse failed", "root_error_message": "JSON parse failed"
                       , "stack_trace": "at parse (a.js:1)", "hash": #{errHash}, "is_framework": false }|]
            , frozenTime
            , frozenTime
            )
        maybe (fail "INSERT ... RETURNING id returned no row") (pure . fromOnly)
          . listToMaybe
          =<< PGS.query
            conn
            [sql| INSERT INTO apis.issues (project_id, issue_type, title, target_hash, service, issue_data, created_at, updated_at)
                  VALUES (?, 'runtime_exception', 'slow-trace issue', ?, 'checkout', ?::jsonb, ?, ?) RETURNING id |]
            ( testPid
            , errHash
            , AE.encode
                [aesonQQ|{ "error_type": "SyntaxError", "error_message": "JSON parse failed"
                         , "stack_trace": "at parse (a.js:1)", "occurrence_count": 1
                         , "first_seen": #{frozenTime}, "last_seen": #{frozenTime} }|]
            , frozenTime
            , frozenTime
            )

      -- The page ships a shell that fetches the waterfall itself — no span read
      -- inline, so a slow trace can no longer delay (or 504) the issue.
      (_, page) <- testServant tr $ AnomalyList.anomalyDetailGetH testPid (UUIDId issueId) (Just "true") Nothing
      let html = renderPage page
      html `shouldSatisfy` T.isInfixOf ("/traces/" <> traceIdText)
      -- Losing the timestamp turns the fragment's +/-5min window into a 3-day scan.
      html `shouldSatisfy` T.isInfixOf "timestamp="
      html `shouldSatisfy` T.isInfixOf "slow-trace issue"
      html `shouldSatisfy` not . T.isInfixOf "Waterfall"
      -- The user journey rides along in the activity fragment; drop these params
      -- and it disappears with every other assertion still passing.
      html `shouldSatisfy` T.isInfixOf "/activity?trace_id="
      html `shouldSatisfy` T.isInfixOf "trace_ts="
      html `shouldSatisfy` not . T.isInfixOf "No trace data available"

      -- And the fragment itself is budgeted: starved, it answers with a retryable
      -- state rather than hanging past the gateway timeout.
      (_, frag) <- testServant tr $ Trace.traceH testPid traceIdText (Just frozenTime) Nothing Nothing (Just "true") Nothing
      case frag of
        Trace.TraceDetails{} -> pass
        _ -> expectationFailure "expected the trace fragment to render with the default budget"
      let starved = tr{trATCtx = (tr.trATCtx){env = (tr.trATCtx.env){traceViewTimeoutSecs = 0}}}
      (_, starvedFrag) <- testServant starved $ Trace.traceH testPid traceIdText (Just frozenTime) Nothing Nothing (Just "true") Nothing
      case starvedFrag of
        Trace.TraceDetailsNotFound _ retryUrl _ -> retryUrl `shouldSatisfy` T.isInfixOf ("/traces/" <> traceIdText)
        _ -> expectationFailure "expected a starved trace fetch to yield the retryable not-found state"

    -- Regression: every runtime exception without frames (151 of 151 in the demo
    -- project — OTel's exception event carries message and type, and the Go SDKs
    -- never opt into exception.stacktrace) rendered "common for browser console
    -- errors", which is wrong for anyone whose backend is not a browser. Name the
    -- runtime that stayed silent instead.
    it "a stackless runtime exception names its runtime instead of blaming browsers" \tr -> do
      errHash <- T.take 8 . DataUUID.toText <$> UUID.nextRandom
      issueId <- withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns
                  (project_id, error_type, message, stacktrace, hash, runtime, error_data, created_at, updated_at)
                VALUES (?, '*errors.errorString', 'shipping quote failure', '', ?, 'go', ?::jsonb, ?, ?) |]
            ( testPid
            , errHash
            , AE.encode
                [aesonQQ|{ "when": #{frozenTime}, "error_type": "*errors.errorString", "root_error_type": "*errors.errorString"
                         , "message": "shipping quote failure", "root_error_message": "shipping quote failure"
                         , "stack_trace": "", "hash": #{errHash}, "is_framework": false, "runtime": "go" }|]
            , frozenTime
            , frozenTime
            )
        maybe (fail "INSERT ... RETURNING id returned no row") (pure . fromOnly)
          . listToMaybe
          =<< PGS.query
            conn
            [sql| INSERT INTO apis.issues (project_id, issue_type, title, target_hash, service, issue_data, created_at, updated_at)
                  VALUES (?, 'runtime_exception', 'stackless go error', ?, 'checkout', ?::jsonb, ?, ?) RETURNING id |]
            ( testPid
            , errHash
            , AE.encode
                [aesonQQ|{ "error_type": "*errors.errorString", "error_message": "shipping quote failure"
                         , "stack_trace": "", "occurrence_count": 1
                         , "first_seen": #{frozenTime}, "last_seen": #{frozenTime} }|]
            , frozenTime
            , frozenTime
            )
      (_, page) <- testServant tr $ AnomalyList.anomalyDetailGetH testPid (UUIDId issueId) Nothing Nothing
      let html = renderPage page
      html `shouldSatisfy` T.isInfixOf "No stack trace in this event"
      html `shouldSatisfy` T.isInfixOf "The go SDK reported this exception without frames."
      html `shouldSatisfy` not . T.isInfixOf "browser console errors"

    -- Migration 0095 swapped now() → app_now() in apis.log_auto_resolve_activity
    -- so the auto-resolve activity row's created_at honours the test clock.
    it "auto_resolved activity record uses app_now() (migration 0095)" \tr -> do
      runTestBg frozenTime tr pass
      let errHash = "test-095-auto-resolve-hash" :: Text
          issueId = UUIDId $ Unsafe.fromJust $ DataUUID.fromText "00000000-0000-0000-0000-000000000095"
      withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns
                  (project_id, error_type, message, stacktrace, hash, state, created_at, updated_at)
                VALUES (?, 'TestError', 'm', 's', ?, 'new', ?, ?)
                ON CONFLICT (project_id, hash) DO UPDATE SET state='new' |]
            (testPid, errHash, frozenTime, frozenTime)
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.issues
                  (id, project_id, issue_type, target_hash, title, created_at, updated_at)
                VALUES (?, ?, 'runtime_exception', ?, 'test issue 095', ?, ?)
                ON CONFLICT (id) DO NOTHING |]
            (issueId, testPid, errHash, frozenTime, frozenTime)
        void
          $ PGS.execute
            conn
            [sql| DELETE FROM apis.issue_activity_log
                WHERE issue_id = ? AND event = 'auto_resolved' |]
            (Only issueId)

      advanceDays tr 5
      expectedTime <- getTestTime tr.trTestClock

      -- runHasqlEffect now syncs app.current_time on every Session via
      -- runHasqlPoolSynced, so the trigger reads app_now() = expectedTime.
      runHasqlEffect tr
        $ EHasql.interpExecute_
          [HI.sql| UPDATE apis.error_patterns SET state = 'resolved'
                  WHERE project_id = #{testPid} AND hash = #{errHash} |]

      rows <- withResource tr.trPool \conn ->
        PGS.query
          conn
          [sql| SELECT created_at FROM apis.issue_activity_log
                WHERE issue_id = ? AND event = 'auto_resolved' |]
          (Only issueId)
          :: IO [Only UTCTime]
      rows `shouldBe` [Only expectedTime]

    -- selectIssues' 24h period uses Time.currentTime for the bucket window —
    -- after the test clock advances past created_at, the issue must drop out.
    it "selectIssues 24h period respects test clock" \tr -> do
      runTestBg frozenTime tr pass
      iid <- UUIDId <$> UUID.nextRandom
      let tgt = "selectIssues-24h-target" :: Text
      withResource tr.trPool \conn ->
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.issues
                  (id, project_id, issue_type, target_hash, endpoint_hash, title,
                   severity, critical, affected_requests, affected_clients,
                   issue_data, created_at, updated_at)
                VALUES (?, ?, 'runtime_exception', ?, ?, 't', 'warning',
                        false, 1, 1, '{}'::jsonb, ?, ?) |]
            (iid, testPid, tgt, tgt, frozenTime, frozenTime)

      -- 23h in: still within the 24h window
      advanceHours tr 23
      (within, _) <-
        runHasqlEffect tr
          $ Issues.selectIssues testPid Issues.PIssueL Issues.defIssueFilters{Issues.period = "24h", Issues.limit = 100}
      map (.base.id) within `shouldSatisfy` elem iid

      -- 25h in: outside window — query still returns row, but activityBuckets all zero
      advanceHours tr 2
      (after, _) <-
        runHasqlEffect tr
          $ Issues.selectIssues testPid Issues.PIssueL Issues.defIssueFilters{Issues.period = "24h", Issues.limit = 100}
      whenJust (find (\r -> r.base.id == iid) after) \row ->
        V.sum row.activityBuckets `shouldBe` 0 -- absent entirely is also acceptable

    -- Read-side sibling of errorUnmerge_doesNotTouchAnotherProjectsPattern, in the
    -- adjacent handler. `errorGroupMembersGetH` checks the caller may access `pid`, then
    -- listed group members by pattern id alone — disclosing another project's error types
    -- and messages, and rendering unmerge links for them under the caller's own project
    -- path.
    it "errorGroupMembers_doesNotDiscloseAnotherProjectsPatterns" \tr -> do
      let otherPid = UUIDId $ Unsafe.fromJust $ DataUUID.fromString "0000ffff-0000-0000-0000-00000000cd01" :: Projects.ProjectId
      canonicalId <- UUID.nextRandom
      memberId <- UUID.nextRandom
      withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
                VALUES (?, 'group-read-authz-victim', 'Free', true, NULL, false, false)
                ON CONFLICT (id) DO NOTHING |]
            (PGS.Only otherPid.unwrap)
        -- canonical_id is a self-referencing FK, so the parent has to exist before a
        -- member can point at it. Its own columns are deliberately bland: the assertions
        -- below look for the *member's* strings leaking, not the group's.
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns (id, project_id, hash, error_type, message, stacktrace)
                VALUES (?, ?, 'group-read-authz-canonical', 'CanonicalType', 'canonical', '')
                ON CONFLICT (id) DO NOTHING |]
            (canonicalId, otherPid.unwrap)
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns (id, project_id, hash, error_type, message, stacktrace, canonical_id, merge_override)
                VALUES (?, ?, 'group-read-authz-hash', 'SecretTypeError', 'victim-only-message', '', ?, FALSE)
                ON CONFLICT (id) DO NOTHING |]
            (memberId, otherPid.unwrap, canonicalId)

      -- Acting as testPid, ask for the other project's group members.
      (_, html) <- testServant tr $ AnomalyList.errorGroupMembersGetH testPid canonicalId
      let rendered = TL.toStrict $ renderText $ toHtml html
      rendered `shouldSatisfy` not . T.isInfixOf "victim-only-message"
      rendered `shouldSatisfy` not . T.isInfixOf "SecretTypeError"

    -- Sibling of the monitor-toggle authz test. `errorUnmergePostH` checks the caller may
    -- access `pid`, then unmerged by pattern id alone — unlike the three sibling handlers
    -- in the same module, which each guard with `err.projectId /= pid`. So a user on one
    -- project could detach another project's error pattern from its group and corrupt its
    -- error grouping. The fix scopes the UPDATE itself, covering every caller.
    it "errorUnmerge_doesNotTouchAnotherProjectsPattern" \tr -> do
      let otherPid = UUIDId $ Unsafe.fromJust $ DataUUID.fromString "0000ffff-0000-0000-0000-00000000ab01" :: Projects.ProjectId
      otherErrId <- UUID.nextRandom
      canonicalId <- UUID.nextRandom
      withResource tr.trPool \conn -> do
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO projects.projects (id, title, payment_plan, active, deleted_at, weekly_notif, daily_notif)
                VALUES (?, 'unmerge-authz-victim', 'Free', true, NULL, false, false)
                ON CONFLICT (id) DO NOTHING |]
            (PGS.Only otherPid.unwrap)
        -- A pattern in the victim project, merged under a canonical parent. The parent
        -- is inserted first: canonical_id is a self-referencing FK.
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns (id, project_id, hash, error_type, message, stacktrace)
                VALUES (?, ?, 'unmerge-authz-canonical', 'TypeError', 'canonical', '')
                ON CONFLICT (id) DO NOTHING |]
            (canonicalId, otherPid.unwrap)
        void
          $ PGS.execute
            conn
            [sql| INSERT INTO apis.error_patterns (id, project_id, hash, error_type, message, stacktrace, canonical_id, merge_override)
                VALUES (?, ?, 'unmerge-authz-hash', 'TypeError', 'victim', '', ?, FALSE)
                ON CONFLICT (id) DO NOTHING |]
            (otherErrId, otherPid.unwrap, canonicalId)

      -- Acting as testPid, aim the unmerge at the other project's pattern.
      _ <- testServant tr $ AnomalyList.errorUnmergePostH testPid otherErrId

      rows <-
        withResource tr.trPool \conn ->
          PGS.query conn [sql| SELECT merge_override FROM apis.error_patterns WHERE id = ? |] (PGS.Only otherErrId)
            :: IO [PGS.Only Bool]
      -- Still merged: the unmerge must not have crossed the tenant boundary.
      map PGS.fromOnly rows `shouldBe` [False]


isApiChange :: AnomalyList.IssueVM -> Bool
isApiChange (AnomalyList.IssueVM _ _ c) = c.base.issueType == Issues.ApiChange


issueOf :: AnomalyList.IssueVM -> Issues.IssueL
issueOf (AnomalyList.IssueVM _ _ issue) = issue


issueIdOf :: AnomalyList.IssueVM -> Issues.IssueId
issueIdOf = (.base.id) . issueOf


onlyIssue :: V.Vector AnomalyList.IssueVM -> IO AnomalyList.IssueVM
onlyIssue rows = case V.toList rows of
  [row] -> pure row
  _ -> fail $ "expected exactly one paginated issue, got " <> show (V.length rows)


nowText :: IO Text
nowText = toText . formatTime defaultTimeLocale "%FT%T%QZ" <$> getCurrentTime


encMsg :: AE.Value -> ByteString
encMsg = toStrict . AE.encode . Unsafe.fromJust . convert


listAnomalies :: TestResources -> Maybe Text -> IO (V.Vector AnomalyList.IssueVM)
listAnomalies tr filterT = do
  (_, pg) <- testServant tr $ AnomalyList.anomalyListGetH testPid filterT Nothing Nothing Nothing Nothing Nothing Nothing [] []
  case pg of
    AnomalyList.ALPage (PageCtx _ tbl) -> pure tbl.rows
    _ -> error "Unexpected response from anomaly list"


renderPage :: ToHtml a => a -> Text
renderPage = TL.toStrict . renderText . toHtml


countQ :: PGS.ToRow q => TestResources -> PGS.Query -> q -> IO Int
countQ tr q args = withResource tr.trPool \conn ->
  maybe (fail "count query returned no row") (pure . fromOnly) . listToMaybe =<< PGS.query conn q args


-- | Anomalies referenced by the issue's @issue_data.anomaly_hashes@ that are still
-- (un-acknowledged, un-archived) — both cascade halves in one round trip.
cascadePending :: TestResources -> Issues.IssueId -> IO (Int, Int)
cascadePending tr issueId = withResource tr.trPool \conn ->
  maybe (fail "cascade count returned no row") pure
    . listToMaybe
    =<< PGS.query
      conn
      [sql| WITH related AS (
              SELECT jsonb_array_elements_text(COALESCE(issue_data->'anomaly_hashes','[]'::jsonb)) AS h
              FROM apis.issues WHERE id=?
            )
            SELECT (COUNT(*) FILTER (WHERE a.acknowledged_at IS NULL))::INT
                 , (COUNT(*) FILTER (WHERE a.archived_at IS NULL))::INT
            FROM apis.anomalies a
            WHERE a.project_id=? AND a.target_hash IN (SELECT h FROM related) |]
      (issueId, testPid)


apiChangeIssueIds :: TestResources -> IO [Issues.IssueId]
apiChangeIssueIds tr =
  map fromOnly <$> withResource tr.trPool \conn ->
    PGS.query
      conn
      [sql| SELECT id FROM apis.issues WHERE project_id=? AND issue_type='api_change' ORDER BY created_at |]
      (Only testPid)


-- | Pull the first ApiChange issue id created by earlier tests. Fails the test
-- with a useful message if the seed step (test 2) didn't run or left no issues.
pickApiChangeIssue :: TestResources -> IO Issues.IssueId
pickApiChangeIssue tr =
  maybe (error "pickApiChangeIssue: no ApiChange issue seeded — earlier tests in this spec must run first") pure
    . listToMaybe
    =<< apiChangeIssueIds tr


-- | Dump the pending queue (diagnostics on failure), then run the matching jobs.
runAnomalyJobs :: TestResources -> (BackgroundJobs.BgJobs -> Bool) -> IO ()
runAnomalyJobs tr p = do
  getPendingBackgroundJobs tr.trATCtx >>= logBackgroundJobsInfo tr.trLogger
  void $ runBackgroundJobsWhere frozenTime tr.trATCtx p


shapeOrField :: BackgroundJobs.BgJobs -> Bool
shapeOrField = \case
  BackgroundJobs.NewAnomaly{anomalyType = t} -> t `elem` ["shape", "field"]
  _ -> False


-- Base64 request bodies for the msg1 endpoint: a different body shape (shape anomaly)
-- and a username typed as a number instead of a string (format anomaly).
shapeBody, formatBody :: Text
shapeBody = "eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOiJhZG1pbkBncm92ZXBheS5jby51ayJ9"
formatBody = "eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOjJ9"


-- Same endpoint as msg1, with a caller-supplied request body.
msgWithBody :: Text -> Text -> AE.Value
msgWithBody body timestamp =
  [aesonQQ|{"duration":476434,
            "host":"172.31.29.11",
            "method":"GET",
            "path_params":{},
            "project_id":"00000000-0000-0000-0000-000000000000",
            "proto_minor":1,
            "proto_major":1,"query_params":{},
            "raw_url":"/","referer":"","request_body": #{body},
            "request_headers":{
              "connection":["upgrade"],"host":["172.31.29.11"],
              "x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],
              "user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},
              "response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],
              "vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],
              "content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]
            },
            "sdk_type":"JsExpress",
            "status_code":200,
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[]}
      |]
