-- | Lifecycle tests that drive the real CLI top-down, in-process: the actual
-- optparse parser and command pipeline run against real handlers + test DB via
-- 'runCLILifecycle' (HTTP effect interpreted by 'runHTTPtoServant'). Each
-- @describe@ block walks one user workflow end-to-end the way the skills
-- (github.com/monoscope-tech/skills) document it — these tests pin the
-- envelopes and behaviors the skills promise.
module CLI.CLILifecycleSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Pages.Share qualified as Share
import Pkg.TestUtils
import Relude
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import Test.Hspec
import UnliftIO.Exception (finally)


-- | Events array out of a search envelope.
eventsOf :: AE.Value -> [AE.Value]
eventsOf = \case
  AE.Object o | Just (AE.Array evs) <- KM.lookup "events" o -> V.toList evs
  _ -> []


-- | Rows from a CLI list response — accepts both the normalised
-- @{data: [...]}@ envelope and a bare JSON array.
listRows :: AE.Value -> [AE.Value]
listRows = \case
  AE.Object o | Just (AE.Array a) <- KM.lookup "data" o -> V.toList a
  AE.Array a -> V.toList a
  _ -> []


rowId :: AE.Value -> String
rowId = \case
  AE.Object o | Just (AE.String i) <- KM.lookup "id" o -> toString i
  v -> error $ "expected row object with id, got: " <> show v


-- | Decode captured stdout as a single JSON value.
jsonOut :: Text -> IO AE.Value
jsonOut out = case AE.eitherDecodeStrict' (encodeUtf8 out) of
  Right v -> pure v
  Left err -> do
    expectationFailure $ "expected single JSON value on stdout: " <> err <> "\n--- stdout ---\n" <> take 500 (toString out)
    error "unreachable"


shouldHaveKeys :: AE.Value -> [Text] -> Expectation
shouldHaveKeys (AE.Object obj) keys = forM_ keys \k -> unless (KM.member (fromString (toString k)) obj) do
  expectationFailure $ "missing key " <> toString k <> " in: " <> take 300 (decodeUtf8 (AE.encode obj))
shouldHaveKeys other _ = expectationFailure $ "expected JSON object, got: " <> show other


spec :: Spec
spec = aroundAll withTestResources do
  describe "CLI lifecycle (in-process, real parser + handlers)" do
    it "version prints and exits zero (harness smoke)" \tr -> do
      (ec, out) <- runCLILifecycle tr ["version"]
      ec `shouldBe` ExitSuccess
      toString out `shouldContain` "monoscope"

    it "me --json returns project identity envelope" \tr -> do
      (ec, out) <- runCLILifecycle tr ["--json", "me"]
      ec `shouldBe` ExitSuccess
      v <- jsonOut out
      v `shouldHaveKeys` ["project", "project_id"]

  describe "monitors-as-code lifecycle" do
    it "apply is idempotent by title; mute/unmute/delete round-trip" \tr -> do
      let yamlPath = "/tmp/monoscope-lifecycle-monitor.yaml"
      writeFileText yamlPath
        $ unlines
          [ "title: Lifecycle error-rate alert"
          , "query: 'severity.text == \"ERROR\"'"
          , "alert_threshold: 10"
          , "trigger_less_than: false"
          , "check_interval_mins: 5"
          , "time_window_mins: 15"
          , "emails: []"
          , "slack_channels: []"
          , "teams: []"
          ]

      (ec1, _) <- runCLILifecycle tr ["--json", "monitors", "apply", yamlPath]
      ec1 `shouldBe` ExitSuccess
      -- Re-applying the same id-less file must update the existing monitor
      -- (natural key: title), not create a duplicate.
      (ec2, _) <- runCLILifecycle tr ["--json", "monitors", "apply", yamlPath]
      ec2 `shouldBe` ExitSuccess

      (_, listOut) <- runCLILifecycle tr ["--json", "monitors", "list"]
      listV <- jsonOut listOut
      listRows listV `shouldSatisfy` ((== 1) . length)

      let mid = case listRows listV of
            [row] -> rowId row
            _ -> error "expected exactly one monitor"

      -- yaml dump round-trips through apply without creating a duplicate
      (ecY, yamlDump) <- runCLILifecycle tr ["monitors", "yaml", mid]
      ecY `shouldBe` ExitSuccess
      let dumpPath = "/tmp/monoscope-lifecycle-monitor-dump.yaml"
      writeFileText dumpPath yamlDump
      (ecA3, _) <- runCLILifecycle tr ["--json", "monitors", "apply", dumpPath]
      ecA3 `shouldBe` ExitSuccess
      (_, listOut3) <- runCLILifecycle tr ["--json", "monitors", "list"]
      jsonOut listOut3 >>= \v -> listRows v `shouldSatisfy` ((== 1) . length)

      -- mute with a duration, unmute, then delete — the triage-skill loop
      (ecMute, _) <- runCLILifecycle tr ["--json", "monitors", "mute", mid, "--for", "30"]
      ecMute `shouldBe` ExitSuccess
      (ecUnmute, _) <- runCLILifecycle tr ["--json", "monitors", "unmute", mid]
      ecUnmute `shouldBe` ExitSuccess
      (ecDel, _) <- runCLILifecycle tr ["--json", "monitors", "delete", mid]
      ecDel `shouldBe` ExitSuccess

      (_, listOut2) <- runCLILifecycle tr ["--json", "monitors", "list"]
      listV2 <- jsonOut listOut2
      listRows listV2 `shouldBe` []

  describe "incident investigation lifecycle (search → get → context → share)" do
    it "finds an ingested span, pulls context, and mints a share link that resolves unauthenticated" \tr -> do
      apiKey <- createTestAPIKey tr testPid "lifecycle-share-key"
      ingestTrace tr apiKey "GET /api/lifecycle/share" frozenTime
      let fromT = iso8601Show (addUTCTime (-3600) frozenTime)
          toT = iso8601Show (addUTCTime 3600 frozenTime)

      -- search --first --id-only → bare event id on stdout (skill: pipeline shortcut)
      (ecS, idOut) <- runCLILifecycle tr ["--json", "traces", "search", "", "--from", fromT, "--to", toT, "--first", "--id-only"]
      ecS `shouldBe` ExitSuccess
      let eid = T.strip idOut
      eid `shouldSatisfy` (not . T.null)

      -- events get → stable events envelope; pull the timestamp for chaining
      (ecG, getOut) <- runCLILifecycle tr ["--json", "events", "get", toString eid]
      ecG `shouldBe` ExitSuccess
      gv <- jsonOut getOut
      ts <- case gv of
        AE.Object o
          | Just (AE.Array evs) <- KM.lookup "events" o
          , Just (AE.Object e0) <- evs V.!? 0
          , Just (AE.String t) <- KM.lookup "timestamp" e0 ->
              pure t
        _ -> expectationFailure ("no events[0].timestamp in events get output: " <> take 300 (toString getOut)) >> error "unreachable"

      -- temporal context with per-trace summary (skill step 4)
      (ecCx, cxOut) <- runCLILifecycle tr ["--json", "events", "context", "--at", toString ts, "--window", "5m", "--summary"]
      ecCx `shouldBe` ExitSuccess
      cxV <- jsonOut cxOut
      cxV `shouldHaveKeys` ["events"]

      -- mint a share link for the smoking-gun event
      (ecSh, shOut) <- runCLILifecycle tr ["--json", "share-link", "create", "--event-id", toString eid, "--created-at", toString ts, "--type", "span"]
      ecSh `shouldBe` ExitSuccess
      shV <- jsonOut shOut
      shV `shouldHaveKeys` ["id", "url"]
      url <- case shV of
        AE.Object o | Just (AE.String u) <- KM.lookup "url" o -> pure u
        _ -> error "unreachable: shouldHaveKeys passed"
      toString url `shouldContain` "/share/r/"

      -- the teammate-opens-the-link assertion: the share view resolves with
      -- no auth context (shareLinkGetH throws err404 if the link is dead)
      let sid = fromMaybe (error $ "share url did not end in a UUID: " <> url) (UUID.fromText (T.takeWhileEnd (/= '/') url))
      void $ runAsBase tr (Share.shareLinkGetH sid)

  describe "dashboards-as-code lifecycle" do
    it "yaml dump round-trips through apply keyed on file_path; duplicate/star/delete" \tr -> do
      let createPath = "/tmp/monoscope-lifecycle-dashboard.yaml"
          dumpPath = "/tmp/monoscope-lifecycle-dashboard-dump.yaml"
      writeFileText createPath
        $ unlines
          [ "title: Lifecycle dashboard"
          , "file_path: dashboards/lifecycle.yaml"
          ]
      (ecC, _) <- runCLILifecycle tr ["--json", "dashboards", "create", createPath]
      ecC `shouldBe` ExitSuccess

      (_, listOut) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      did <-
        jsonOut listOut >>= \v -> case listRows v of
          [row] -> pure (rowId row)
          rows -> expectationFailure ("expected exactly one dashboard, got " <> show (length rows)) >> error "unreachable"

      -- the canonical as-code loop: dump → apply (twice) → still exactly one
      (ecY, yamlDump) <- runCLILifecycle tr ["dashboards", "yaml", did]
      ecY `shouldBe` ExitSuccess
      writeFileText dumpPath yamlDump
      (ecA1, _) <- runCLILifecycle tr ["--json", "dashboards", "apply", dumpPath]
      ecA1 `shouldBe` ExitSuccess
      (ecA2, _) <- runCLILifecycle tr ["--json", "dashboards", "apply", dumpPath]
      ecA2 `shouldBe` ExitSuccess
      (_, listOut2) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      jsonOut listOut2 >>= \v -> listRows v `shouldSatisfy` ((== 1) . length)

      (ecStar, _) <- runCLILifecycle tr ["--json", "dashboards", "star", did]
      ecStar `shouldBe` ExitSuccess
      (ecDup, _) <- runCLILifecycle tr ["--json", "dashboards", "duplicate", did]
      ecDup `shouldBe` ExitSuccess
      (_, listOut3) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      ids <- jsonOut listOut3 >>= \v -> pure (map rowId (listRows v))
      length ids `shouldBe` 2

      forM_ ids \i -> do
        (ecDel, _) <- runCLILifecycle tr ["--json", "dashboards", "delete", i]
        ecDel `shouldBe` ExitSuccess
      (_, listOut4) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      jsonOut listOut4 >>= \v -> listRows v `shouldBe` []

  describe "chunked search lifecycle (single-envelope contract)" do
    it "wide --since window emits exactly one envelope; --limit caps across slices; --chunk-hours 0 equivalent" \tr -> do
      apiKey <- createTestAPIKey tr testPid "lifecycle-chunk-key"
      -- chunk windows resolve against the real clock (SQL NOW()), so ingest
      -- relative to now: one event per hourly slice of a 3h window
      now <- getCurrentTime
      forM_ [600, 4200, 7800] \secsAgo ->
        ingestLog tr apiKey ("chunkevt marker " <> show (secsAgo :: Int)) (addUTCTime (negate (fromIntegral secsAgo)) now)

      -- one JSON document — this fails if slices leak as NDJSON
      (ec1, out1) <- runCLILifecycle tr ["--json", "events", "search", "chunkevt", "--since", "3h"]
      ec1 `shouldBe` ExitSuccess
      v1 <- jsonOut out1
      let evs1 = eventsOf v1
      length evs1 `shouldBe` 3
      -- deduped: ids unique; ordering: newest first
      let ids1 = map rowId evs1
          tsOf = \case AE.Object o | Just (AE.String t) <- KM.lookup "timestamp" o -> t; _ -> ""
          tss = map tsOf evs1
      length (ordNub ids1) `shouldBe` 3
      tss `shouldBe` sortBy (comparing Down) tss

      -- --limit budgets across slices and reports has_more for the rest
      (ec2, out2) <- runCLILifecycle tr ["--json", "events", "search", "chunkevt", "--since", "3h", "--limit", "2"]
      ec2 `shouldBe` ExitSuccess
      v2 <- jsonOut out2
      length (eventsOf v2) `shouldBe` 2
      case v2 of
        AE.Object o -> KM.lookup "has_more" o `shouldBe` Just (AE.Bool True)
        _ -> expectationFailure "expected envelope object"

      -- single-request path (explicit --from/--to disables slicing; the
      -- in-process server resolves --since against the frozen test clock, so
      -- the equivalence check pins the same absolute window instead)
      (ec3, out3) <- runCLILifecycle tr ["--json", "events", "search", "chunkevt", "--from", iso8601Show (addUTCTime (-10800) now), "--to", iso8601Show (addUTCTime 60 now), "--chunk-hours", "0"]
      ec3 `shouldBe` ExitSuccess
      v3 <- jsonOut out3
      map rowId (eventsOf v3) `shouldBe` ids1

  describe "metrics SLO lifecycle" do
    it "KQL summarize expression returns data; --assert gates the exit code; chart renders" \tr -> do
      apiKey <- createTestAPIKey tr testPid "lifecycle-metrics-key"
      -- the server resolves --since against the frozen test clock, so the
      -- fixture events anchor on frozenTime (unlike the chunk test, whose
      -- windows are resolved client-side against the real clock)
      forM_ [60, 120] \secsAgo ->
        ingestLog tr apiKey "metricsevt marker" (addUTCTime (negate (fromIntegral (secsAgo :: Int))) frozenTime)

      (ec1, out1) <- runCLILifecycle tr ["--json", "metrics", "query", "body has \"metricsevt\" | summarize count()", "--since", "1h"]
      ec1 `shouldBe` ExitSuccess
      v1 <- jsonOut out1
      v1 `shouldHaveKeys` ["dataset"]

      -- the CI-gate usage the investigate skill documents
      (ecPass, _) <- runCLILifecycle tr ["--json", "metrics", "query", "body has \"metricsevt\" | summarize count()", "--since", "1h", "--assert", "> 0"]
      ecPass `shouldBe` ExitSuccess
      (ecFail, _) <- runCLILifecycle tr ["--json", "metrics", "query", "body has \"metricsevt\" | summarize count()", "--since", "1h", "--assert", "< 0"]
      ecFail `shouldBe` ExitFailure 1

      (ecChart, chartOut) <- runCLILifecycle tr ["metrics", "chart", "body has \"metricsevt\" | summarize count() by bin_auto(timestamp)", "--since", "1h"]
      ecChart `shouldBe` ExitSuccess
      chartOut `shouldSatisfy` (not . T.null)

  describe "send-event lifecycle (real OTLP/HTTP wire)" do
    it "ships a CLI-authored span through the live server into search" \tr -> do
      apiKey <- createTestAPIKey tr testPid "lifecycle-send-key"
      (ecSend, _) <- withLiveServer tr \port ->
        do
          setEnv "MONOSCOPE_TEST_API_KEY" (toString apiKey)
          setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" ("http://127.0.0.1:" <> show port)
          runCLILifecycle tr ["send-event", "-m", "Deploy completed lifecycleping", "--service", "lifecycle-cli", "-t", "deploy.id:d1"]
          `finally` (unsetEnv "OTEL_EXPORTER_OTLP_ENDPOINT" >> unsetEnv "MONOSCOPE_TEST_API_KEY")
      ecSend `shouldBe` ExitSuccess
      -- the real assertion: the span must land in search (send-event prints
      -- "Event sent." even when the exporter fails, so exit code alone proves
      -- nothing — the poll catches silent export failures)
      -- absolute window: the span lands at real wall-clock time, while the
      -- in-process server resolves --since against the frozen test clock
      now <- getCurrentTime
      let fromT = iso8601Show (addUTCTime (-3600) now)
          toT = iso8601Show (addUTCTime 3600 now)
          poll :: Int -> IO ()
          poll 0 = expectationFailure "send-event span never appeared in events search"
          poll n = do
            (_, out) <- runCLILifecycle tr ["--json", "events", "search", "lifecycleping", "--from", fromT, "--to", toT]
            v <- jsonOut out
            if null (eventsOf v) then threadDelay 500_000 >> poll (n - 1) else pass
      poll 20
