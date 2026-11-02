-- | The CLI end to end: the built binary (help, packaging, config files, and the
-- wire format against a live server) and the real parser and command pipeline
-- in-process against real handlers ('runCLILifecycle'), one workflow per @it@
-- the way the skills (github.com/monoscope-tech/skills) document it.
module CLI.CLISpec (spec) where

import CLI.Harness (runCLILifecycle, runCLILifecycleLive, runCLILifecyclePicking, withCliBin)
import Control.Lens ((^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, values, _Number, _Object, _String)
import Data.Bits ((.&.))
import Data.Effectful.Hasql qualified as Hasql
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Hasql.Interpolate qualified as HI
import Log.Backend.LogList qualified as LogList
import Log.Data (showLogMessage)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.Log qualified as Log
import Pages.Share qualified as Share
import Pkg.TestUtils
import Servant qualified
import System.Environment (getEnvironment, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, setFileMode)
import System.Process qualified as Proc
import UnliftIO.Exception (bracket_, finally)


-- | Captured stdout as one JSON document: a search leaking slices as NDJSON fails here.
jsonOut :: Text -> IO AE.Value
jsonOut out = either (\e -> fail $ e <> " in stdout: " <> toString (T.take 500 out)) pure $ AE.eitherDecodeStrict' (encodeUtf8 out)


shouldHaveKeys :: HasCallStack => AE.Value -> [AE.Key] -> Expectation
v `shouldHaveKeys` ks = filter (`notElem` foldMap KM.keys (v ^? _Object)) ks `shouldBe` []


-- | The @id@s of the rows under @field@ (@data@ for lists, @events@ for searches).
idsAt :: AE.Key -> AE.Value -> [Text]
idsAt field v = v ^.. key field . values . key "id" . _String


cliOk :: HasCallStack => TestResources -> [String] -> IO Text
cliOk tr args = do
  (ec, out) <- runCLILifecycle tr args
  (args, ec) `shouldBe` (args, ExitSuccess)
  pure out


cliJson :: HasCallStack => TestResources -> [String] -> IO AE.Value
cliJson tr args = cliOk tr ("--json" : args) >>= jsonOut


withEnv :: [(String, String)] -> IO a -> IO a
withEnv vars = bracket_ (for_ vars (uncurry setEnv)) (for_ vars (unsetEnv . fst))


spec :: Spec
spec = around withCliResources $ describe "CLI" do
  it "binary without a server: packaged release, usage for every command, parse and agent-mode errors" \_ -> withCliBin \bin -> do
    path <- fromMaybe "" <$> lookupEnv "PATH"
    let run extra args = do
          (ec, out, err) <- Proc.readCreateProcessWithExitCode (Proc.proc bin args){Proc.env = Just $ [("HOME", "/tmp/no-such-monoscope-home-for-tests"), ("PATH", path)] <> extra} ""
          pure (ec, toText out, toText err)
    withSystemTempDirectory "monoscope-release" \dir -> do
      (status, output, errors) <- Proc.readProcessWithExitCode "scripts/package-cli.sh" [bin, dir <> "/monoscope"] ""
      unless (status == ExitSuccess) $ expectationFailure $ output <> errors
      (versionStatus, version, _) <- Proc.readProcessWithExitCode (dir <> "/monoscope") ["--version"] ""
      (versionStatus, "monoscope " `T.isInfixOf` toText version) `shouldBe` (ExitSuccess, True)
    -- A wrong parser wiring prints some other command's usage: a leaf's usage line
    -- names exactly the command, a group's starts with it.
    let leaves = [["me"], ["version"], ["config", "init"], ["monitors", "list"], ["dashboards", "list"], ["api-keys", "list"], ["project", "get"], ["teams", "list"], ["members", "list"]]
        groups =
          map words
            $ [ "monitors", "monitors create", "monitors update", "monitors patch", "monitors apply", "monitors bulk"
              , "dashboards", "dashboards create", "dashboards update", "dashboards patch", "dashboards bulk", "dashboards widget"
              , "dashboards widget upsert", "dashboards widget delete", "dashboards widget reorder", "dashboards render"
              , "chart", "status", "open", "api-keys deactivate", "api-keys revoke", "share-link", "share-link create", "project", "project patch"
              , "issues", "issues list", "issues ack", "issues bulk", "endpoints", "endpoints list", "log-patterns", "log-patterns list", "log-patterns bulk"
              , "teams", "teams create", "teams update", "teams patch", "teams bulk", "members", "members add", "members patch", "members remove"
              , "schema", "facets", "services list", "events search", "events context", "events tail", "metrics query", "metrics chart"
              , "auth login", "completion", "logs search"
              ]
    misusage <- forM (map (True,) leaves <> map (False,) groups) \(leaf, args) -> do
      (ec, out, _) <- run [] (map toString args <> ["--help"])
      let expected = unwords ("Usage: monoscope" : args)
          usage = maybe "" T.strip $ viaNonEmpty head (lines out)
          ok = ec == ExitSuccess && (usage == expected || not leaf && (expected <> " ") `T.isPrefixOf` usage)
      pure [(args, ec, usage) | not ok]
    concat misusage `shouldBe` []
    forM_ [([], ["events", "metrics", "auth"]), (["events", "search"], ["--since", "--from"]), (["logs", "search"], ["--since", "--from"]), (["auth", "login"], ["--token"])] \(args, needles) -> do
      (ec, out, _) <- run [] (args <> ["--help"])
      ec `shouldBe` ExitSuccess
      out `shouldContainAll` needles
    -- Audit findings B11 (hint names `monoscope`), C7 (agent mode refuses interactive login), C12 (unknown shell fails loudly).
    forM_ @[]
      [ ([], ["chart", "summarize count()", "--type", "bogus"], ExitFailure 1, "line|bar|stat|table")
      , ([], ["--table", "auth", "status"], ExitSuccess, "Not authenticated. Run: monoscope auth login")
      , ([("MONOSCOPE_AGENT_MODE", "1")], ["auth", "login"], ExitFailure 1, "pass --token; interactive login is disabled")
      , ([], ["completion", "tcsh"], ExitFailure 1, "unknown shell 'tcsh'; supported: bash|zsh|fish")
      ]
      \(extra, args, code, message) -> do
        (ec, out, err) <- run extra args
        ec `shouldBe` code
        (out <> err) `shouldContainAll` [message]

  it "binary config scripts write owner-only files, redact secrets and report errors on stderr without prompting" \_ -> withCliBin \bin -> withSystemTempDirectory "monoscope-config" \dir -> do
    inherited <- getEnvironment
    let isolated = ("XDG_CONFIG_HOME", dir) : filter (\(k, _) -> k /= "XDG_CONFIG_HOME" && not ("MONOSCOPE_" `isPrefixOf` k)) inherited
        run args = do
          (ec, out, err) <- Proc.readCreateProcessWithExitCode (Proc.proc bin ("--json" : args)){Proc.cwd = Just dir, Proc.env = Just isolated} ""
          pure (ec, toText out, toText err)
        mode file = (.&. 0o077) . fileMode <$> getFileStatus (dir <> "/monoscope" <> file)
    forM_ [("/config.yaml", ["config", "set", "api_key"]), ("/tokens.json", ["auth", "login", "--token"])] \(file, args) ->
      forM_ [False, True] \overwrite -> do
        when overwrite $ setFileMode (dir <> "/monoscope") 0o755 >> setFileMode (dir <> "/monoscope" <> file) 0o644
        (ec, out, err) <- run (args <> ["fixture-secret"])
        ec `shouldBe` ExitSuccess
        (out <> err) `shouldNotSatisfy` T.isInfixOf "fixture-secret"
        mapM mode [file, ""] `shouldReturn` [0, 0]
    forM_ [["get"], ["get", "api_key"]] \args -> do
      (ec, out, err) <- run ("config" : args)
      ec `shouldBe` ExitSuccess
      (out <> err) `shouldNotSatisfy` T.isInfixOf "fixture-secret"
      out `shouldSatisfy` T.isInfixOf "********"
    forM_ [["get", "bogus"], ["set", "bogus", "value"], ["init"]] \args -> do
      (ec, out, err) <- run ("config" : args)
      (ec, out) `shouldBe` (ExitFailure 1, "")
      err `shouldSatisfy` T.isInfixOf "message"

  -- Wire-format regressions (URL paths, KQL operators, kind mapping, envelopes) only
  -- show against a real HTTP server. MONOSCOPE_API_KEY (+ MONOSCOPE_API_URL,
  -- MONOSCOPE_PROJECT) points this at an external server instead of a local one.
  it "binary against a live server: identity, schema, facets, search filters and validation, services, resource lists" \tr -> withCliBin \bin -> do
    path <- fromMaybe "" <$> lookupEnv "PATH"
    let walk url apiKey project = do
          let run args = do
                (ec, out, err) <- Proc.readCreateProcessWithExitCode (Proc.proc bin args){Proc.env = Just [("MONOSCOPE_API_URL", url), ("MONOSCOPE_API_KEY", apiKey), ("MONOSCOPE_PROJECT", project), ("MONOSCOPE_AGENT_MODE", "1"), ("PATH", path)]} ""
                pure (ec, toText out, toText err)
              runJson args = do
                (ec, out, err) <- run args
                unless (ec == ExitSuccess) $ expectationFailure $ show args <> " exited " <> show ec <> ": " <> toString err
                jsonOut out
          (versionEc, version, _) <- run ["version"]
          (versionEc, "monoscope " `T.isInfixOf` version) `shouldBe` (ExitSuccess, True)
          let search = ["--since", "1h", "--limit", "1"]
          forM_
            ( [ (["me"], ["project_id"])
              , (["events", "search", "", "--since", "1h", "--limit", "5"], ["events", "count", "has_more"])
              , (["events", "search", "", "--service", "monoscope"] <> search, ["events"])
              , (["events", "search", "", "--level", "error"] <> search, ["events"])
              , (["traces", "search", ""] <> search, ["events"])
              , (["events", "search", "", "--kind", "log"] <> search, ["events"])
              , (["events", "search", "", "--since", "1H", "--limit", "1"], ["events"])
              , (["services", "list"], ["services", "count"])
              ]
                <> [([resource, "list"], ["data", "pagination"]) | resource <- ["issues", "endpoints", "log-patterns", "monitors", "dashboards", "api-keys", "teams", "members", "incidents"]]
            )
            \(args, keys) -> runJson args >>= (`shouldHaveKeys` keys)
          forM_ @[]
            [ (["events", "search", "", "--since", "1xyz", "--limit", "1"], "--since")
            , (["events", "search", "", "--kind", "banana"] <> search, "--kind")
            , (["events", "search", "|| invalid {{"] <> search, "invalid query: Syntax error")
            ]
            \(args, message) -> do
              (ec, _, err) <- run args
              (args, ec == ExitSuccess, message `T.isInfixOf` err) `shouldBe` (args, False, True)
          schema <- runJson ["schema"]
          let fields = foldMap KM.toList (schema ^? key "fields" . _Object)
          map fst fields `shouldSatisfy` elem "resource.service.name"
          forM_ fields \(_, field) -> field `shouldHaveKeys` ["field_type", "description"]
          runJson ["facets"] >>= (`shouldSatisfy` isJust . (^? _Object))
          serviceFacets <- runJson ["facets", "resource.service.name", "--top", "3"]
          foldMap KM.keys (serviceFacets ^? _Object) `shouldSatisfy` all (`elem` ["resource.service.name", "source"])
          length (serviceFacets ^.. key "resource.service.name" . values) `shouldSatisfy` (<= 3)
    lookupEnv "MONOSCOPE_API_KEY" >>= \case
      Nothing -> do
        apiKey <- createTestAPIKey tr testPid "binary-e2e"
        withLiveServer tr \port -> walk ("http://127.0.0.1:" <> show port) (toString apiKey) (UUID.toString UUID.nil)
      Just apiKey -> do
        url <- fromMaybe "https://api.monoscope.tech" <$> lookupEnv "MONOSCOPE_API_URL"
        project <- fromMaybe (UUID.toString UUID.nil) <$> lookupEnv "MONOSCOPE_PROJECT"
        (reachable, _, err) <- Proc.readCreateProcessWithExitCode (Proc.proc bin ["me"]){Proc.env = Just [("MONOSCOPE_API_URL", url), ("MONOSCOPE_API_KEY", apiKey), ("MONOSCOPE_PROJECT", project), ("MONOSCOPE_AGENT_MODE", "1")]} ""
        if reachable == ExitSuccess then walk url apiKey project else pendingWith $ "external server " <> url <> " rejected `me`: " <> err

  it "scripts: invalid input and missing ids exit 1 before querying; pickers; identity, project and bulk routes" \tr -> do
    let missingIds = [["dashboards", "render"], ["dashboards", "get"], ["monitors", "get"], ["issues", "get"], ["incidents", "get"], ["endpoints", "get"], ["log-patterns", "get"], ["teams", "get"], ["members", "get"], ["api-keys", "get"], ["dashboards", "patch", "dashboard.yaml"], ["members", "patch", "view"], ["dashboards", "widget", "delete", "widget-id"]]
    forM_ ([["events", "search", "", "--limit", l] | l <- ["0", "-1"]] <> [["metrics", "query", "summarize count()", "--since", "nonsense"], ["events", "context", "--at", "not-a-timestamp"]] <> missingIds) \args ->
      (args,) <$> runCLILifecycle tr ("--json" : args) `shouldReturn` (args, (ExitFailure 1, ""))
    (fst <$> runCLILifecyclePicking tr (\_ _ -> expectationFailure "opened an empty picker" >> pure Nothing) ["--table", "dashboards", "get"]) `shouldReturn` ExitFailure 1
    patternId <- dbOne @_ @Int64 tr [sql| INSERT INTO apis.log_patterns (project_id, log_pattern, pattern_hash) VALUES (?, 'picker test', 'picker-pattern') RETURNING id |] (Only testPid)
    let choosePattern _ choices = do
          map fst choices `shouldBe` [show patternId]
          pure (fst <$> viaNonEmpty head choices)
    (pickedEc, picked) <- runCLILifecyclePicking tr choosePattern ["--table", "log-patterns", "get"]
    (pickedEc, "picker test" `T.isInfixOf` picked) `shouldBe` (ExitSuccess, True)
    cliJson tr ["log-patterns", "bulk", "acknowledge", "--ids", show patternId] `shouldReturn` AE.object ["succeeded" AE..= ([patternId] :: [Int64]), "failed" AE..= ([] :: [AE.Value])]
    cliOk tr ["version"] >>= (`shouldSatisfy` T.isInfixOf "monoscope")
    cliJson tr ["me"] >>= (`shouldHaveKeys` ["project", "project_id"])
    cliJson tr ["project", "get"] >>= (`shouldHaveKeys` ["summary"])
    cliJson tr ["incidents", "list"] >>= (`shouldHaveKeys` ["data", "pagination"])

  it "issues: status totals beyond the first page, latest-event recency in list and get, picker, bulk acknowledge" \tr -> do
    dbExec
      tr
      [sql| INSERT INTO apis.issues (id, project_id, issue_type, target_hash, endpoint_hash, title, severity, critical, affected_requests, affected_clients, issue_data, created_at, updated_at)
            SELECT gen_random_uuid(), ?, 'runtime_exception', gen_random_uuid()::text, '', 'CLI status issue', 'warning', false, 1, 1, '{}'::jsonb, now(), now() FROM generate_series(1, 6) |]
      (Only testPid)
    status <- cliJson tr ["status"]
    status ^? key "open_issues" . _Number `shouldBe` Just 6
    cliOk tr ["--table", "status"] >>= (`shouldSatisfy` T.isInfixOf "CLI status issue")
    (pickedEc, _) <- runCLILifecyclePicking tr (\_ choices -> (length choices `shouldBe` 6) >> pure (fst <$> viaNonEmpty last choices)) ["--table", "issues", "get"]
    pickedEc `shouldBe` ExitSuccess

    -- `last_seen` comes from the latest pattern event, not the stale issue payload, grouped or not.
    forM_ [False, True] \grouped -> do
      uid <- UUID4.nextRandom
      let target = UUID.toText uid
          patternHash = if grouped then target <> "-child" else target
          latest = addUTCTime 300 frozenTime
          sampled = addUTCTime 60 frozenTime
          payload = AE.object ["error_type" AE..= ("Error" :: Text), "error_message" AE..= ("boom" :: Text), "stack_trace" AE..= ("frame" :: Text), "occurrence_count" AE..= (1 :: Int), "first_seen" AE..= frozenTime, "last_seen" AE..= frozenTime]
      void $ runTestBg frozenTime tr do
        _ <-
          Hasql.interpExecute
            [HI.sql| INSERT INTO apis.error_patterns (project_id, error_type, message, stacktrace, hash, error_data, created_at, updated_at, recent_trace_at, last_event_at, parent_hash, is_framework)
                     VALUES (#{testPid}, 'Error', 'boom', 'frame', #{patternHash}, '{}'::jsonb, #{frozenTime}, #{frozenTime}, #{sampled}, #{latest}, #{if grouped then Just target else Nothing}, #{grouped}) |]
        Hasql.interpExecute
          [HI.sql| INSERT INTO apis.issues (id, project_id, issue_type, title, target_hash, is_framework, issue_data, created_at, updated_at)
                   VALUES (#{uid}, #{testPid}, 'runtime_exception', 'stale last seen', #{target}, #{grouped}, #{Aeson payload}, #{frozenTime}, #{frozenTime}) |]
      issue <- cliJson tr ["issues", "get", toString target]
      issue ^? key "issue_data" . key "last_seen" `shouldBe` Just (AE.toJSON latest)

    oldId <- UUID4.nextRandom
    newerId <- UUID4.nextRandom
    groupId <- UUID4.nextRandom
    let (oldHash, newerHash, groupHash) = (UUID.toText oldId, UUID.toText newerId, UUID.toText groupId)
        later = addUTCTime 60 frozenTime
        latest = addUTCTime 300 frozenTime
        groupLatest = addUTCTime 120 latest
        metadataUpdate = addUTCTime 60 latest
        quietData = AE.toJSON Issues.RuntimeExceptionData{Issues.errorType = "Error", Issues.errorMessage = "quiet now", Issues.stackTrace = "frame", Issues.requestPath = Nothing, Issues.requestMethod = Nothing, Issues.occurrenceCount = 1, Issues.firstSeen = later, Issues.lastSeen = later}
    void $ runTestBg frozenTime tr do
      _ <-
        Hasql.interpExecute
          [HI.sql| INSERT INTO apis.error_patterns (project_id, error_type, message, stacktrace, hash, parent_hash, is_framework, error_data, last_event_at)
                   VALUES (#{testPid}, 'Error', 'still happening', 'frame', #{oldHash}, NULL, false, '{}'::jsonb, #{latest}),
                          (#{testPid}, 'Error', 'framework child', 'frame', #{groupHash <> "-child"}, #{groupHash}, true, '{}'::jsonb, #{groupLatest}) |]
      Hasql.interpExecute
        [HI.sql| INSERT INTO apis.issues (id, project_id, issue_type, title, target_hash, service, is_framework, issue_data, created_at, updated_at)
                 VALUES (#{oldId}, #{testPid}, 'runtime_exception', 'active old issue', #{oldHash}, 'recency-smoke', false, '{}'::jsonb, #{frozenTime}, #{frozenTime}),
                        (#{newerId}, #{testPid}, 'runtime_exception', 'newer quiet issue', #{newerHash}, 'recency-smoke', false, #{Aeson quietData}, #{later}, #{metadataUpdate}),
                        (#{groupId}, #{testPid}, 'runtime_exception', 'grouped active issue', #{groupHash}, 'recency-smoke', true, '{}'::jsonb, #{frozenTime}, #{frozenTime}) |]
    idsAt "data" <$> cliJson tr ["issues", "list", "--service", "recency-smoke", "--per-page", "10"] `shouldReturn` [groupHash, oldHash, newerHash]
    (issues, _) <- runTestBg frozenTime tr $ Issues.selectIssues testPid Issues.PIssueL Issues.defIssueFilters{Issues.service = Just "recency-smoke", Issues.order = Just "-last_seen"}
    map (\i -> (i.base.targetHash, i.lastSeen)) issues `shouldBe` [(groupHash, groupLatest), (oldHash, latest), (newerHash, later)]
    cliJson tr ["issues", "bulk", "acknowledge", "--ids", toString (T.intercalate "," [oldHash, newerHash])] `shouldReturn` AE.object ["succeeded" AE..= ([oldHash, newerHash] :: [Text]), "failed" AE..= ([] :: [AE.Value])]

  it "monitors as code: apply is idempotent by title, the yaml dump round-trips, mute, unmute and delete" \tr -> do
    let yamlPath = "/tmp/monoscope-lifecycle-monitor.yaml"
        dumpPath = "/tmp/monoscope-lifecycle-monitor-dump.yaml"
        monitorIds = idsAt "data" <$> cliJson tr ["monitors", "list"]
    writeFileText yamlPath $ unlines ["title: Lifecycle error-rate alert", "query: 'severity.severity_text == \"error\"'", "alert_threshold: 10", "trigger_less_than: false", "check_interval_mins: 5", "time_window_mins: 15", "emails: []", "slack_channels: []", "teams: []"]
    replicateM_ 2 $ cliOk tr ["--json", "monitors", "apply", yamlPath]
    [mid] <- monitorIds
    cliOk tr ["--table", "monitors", "list"] >>= (`shouldContainAll` ["Lifecycle error-rate alert", mid])
    cliOk tr ["monitors", "yaml", toString mid] >>= writeFileText dumpPath
    void $ cliOk tr ["--json", "monitors", "apply", dumpPath]
    monitorIds `shouldReturn` [mid]
    forM_ [["mute", toString mid, "--for", "30"], ["unmute", toString mid], ["delete", toString mid]] \args -> cliOk tr ("--json" : "monitors" : args)
    monitorIds `shouldReturn` []

  it "investigation: search to share link that resolves unauthenticated; trace lookup by time; historical context window" \tr -> do
    apiKey <- createTestAPIKey tr testPid "lifecycle-share-key"
    ingestTrace tr apiKey "GET /api/lifecycle/share" frozenTime
    let fromT = iso8601Show (addUTCTime (-3600) frozenTime)
        toT = iso8601Show (addUTCTime 3600 frozenTime)
    -- Scoped to this span: in CI the telemetry store is shared across specs and shards.
    eid <- T.strip <$> cliOk tr ["--json", "traces", "search", "name==\"GET /api/lifecycle/share\"", "--from", fromT, "--to", toT, "--first", "--id-only"]
    eid `shouldSatisfy` (not . T.null)
    event <- cliJson tr ["events", "get", toString eid]
    Just (ts, tid) <- pure $ (,) <$> event ^? key "events" . values . key "timestamp" . _String <*> event ^? key "events" . values . key "trace_id" . _String
    -- A trace id is not an event UUID: --at still finds the span, and only around its time.
    forM_ [(ts, [eid]), (toText toT, [])] \(at, expected) ->
      idsAt "events" <$> cliJson tr ["traces", "get", toString tid, "--at", toString at, "--since", "90d"] `shouldReturn` expected
    cliJson tr ["events", "get", toString eid, "--at", toString ts, "--field", "id"] `shouldReturn` AE.object ["id" AE..= eid]
    cliJson tr ["events", "context", "--at", toString ts, "--window", "5m", "--summary"] >>= (`shouldHaveKeys` ["events"])
    noMatch <- cliJson tr ["events", "search", "nonexistent_xyzzy_42", "--from", fromT, "--to", toT]
    (noMatch ^.. key "events" . values, noMatch ^? key "count" . _Number) `shouldBe` ([], Just 0)
    -- the log explorer's own JSON, which web-components/src/log-worker.ts destructures
    explorer <- AE.toJSON <$> call tr (Log.logExplorerDataH testPid def{Log.from = Just (toText fromT), Log.to = Just (toText toT)})
    explorer `shouldHaveKeys` ["cols", "logsData", "count", "serviceColors", "hasMore"]
    share <- cliJson tr ["share-link", "create", "--event-id", toString eid, "--created-at", toString ts, "--type", "span"]
    share `shouldHaveKeys` ["id", "url"]
    let url = fromMaybe "" (share ^? key "url" . _String)
    url `shouldSatisfy` T.isInfixOf "/share/r/"
    -- the teammate opening the link: shareLinkGetH throws err404 for a dead link
    shareId <- maybe (fail $ "share url did not end in a UUID: " <> toString url) pure $ UUID.fromText (T.takeWhileEnd (/= '/') url)
    void $ toBaseServantResponse tr (Share.shareLinkGetH shareId)

    let center = addUTCTime (-86400) frozenTime
    forM_ [-180, -60, 60, 180] \offset -> ingestLog tr apiKey "context-window" (addUTCTime offset center)
    context <- cliJson tr ["events", "context", "--at", iso8601Show center, "--window", "5m"]
    map (AE.fromJSON @UTCTime) (context ^.. key "events" . values . key "timestamp") `shouldMatchList` [AE.Success (addUTCTime offset center) | offset <- [-60, 60]]

  it "dashboards as code: the yaml dump applies idempotently keyed on file_path; picker, star, duplicate, delete" \tr -> do
    let createPath = "/tmp/monoscope-lifecycle-dashboard.yaml"
        dumpPath = "/tmp/monoscope-lifecycle-dashboard-dump.yaml"
        dashboardIds = idsAt "data" <$> cliJson tr ["dashboards", "list"]
    writeFileText createPath $ unlines ["title: Lifecycle dashboard", "file_path: dashboards/lifecycle.yaml"]
    void $ cliOk tr ["--json", "dashboards", "create", createPath]
    [did] <- dashboardIds
    cliOk tr ["--table", "dashboards", "list"] >>= (`shouldContainAll` ["Lifecycle dashboard", did])
    let chooseDashboard title choices = do
          (title, map fst choices) `shouldBe` ("Select Dashboards", [did])
          map snd choices `shouldSatisfy` any (T.isInfixOf "Lifecycle dashboard")
          pure (Just did)
    (pickEc, picked) <- runCLILifecyclePicking tr chooseDashboard ["--table", "dashboards", "get"]
    pickEc `shouldBe` ExitSuccess
    jsonOut picked >>= (`shouldHaveKeys` ["summary", "schema"])
    runCLILifecyclePicking tr (\_ _ -> pure Nothing) ["--table", "dashboards", "delete"] `shouldReturn` (ExitSuccess, "")
    cliOk tr ["dashboards", "yaml", toString did] >>= writeFileText dumpPath
    replicateM_ 2 $ cliOk tr ["--json", "dashboards", "apply", dumpPath]
    dashboardIds `shouldReturn` [did]
    forM_ ["star", "duplicate"] \action -> cliOk tr ["--json", "dashboards", action, toString did]
    ids <- dashboardIds
    length ids `shouldBe` 2
    forM_ ids \i -> cliOk tr ["--json", "dashboards", "delete", toString i]
    dashboardIds `shouldReturn` []

  it "chunked search: a wide --since window emits one deduped newest-first envelope; --limit spans slices; --chunk-hours 0 agrees" \tr -> do
    apiKey <- createTestAPIKey tr testPid "lifecycle-chunk-key"
    -- chunk windows resolve against the real clock: one event per hourly slice of a 3h window
    now <- getCurrentTime
    forM_ @[] [600, 4200, 7800] \secsAgo -> ingestLog tr apiKey ("chunkevt marker " <> show (secsAgo :: Int)) (addUTCTime (negate (fromIntegral secsAgo)) now)
    wide <- cliJson tr ["events", "search", "chunkevt", "--since", "3h"]
    let ids = idsAt "events" wide
        timestamps = wide ^.. key "events" . values . key "timestamp" . _String
    (length ids, length (ordNub ids)) `shouldBe` (3, 3)
    timestamps `shouldBe` sortOn Down timestamps
    capped <- cliJson tr ["events", "search", "chunkevt", "--since", "3h", "--limit", "2"]
    (length (capped ^.. key "events" . values), capped ^? key "has_more") `shouldBe` (2, Just (AE.Bool True))
    -- explicit --from/--to is the single-request path; the in-process server resolves
    -- --since against the frozen clock, so pin the same absolute window
    idsAt "events" <$> cliJson tr ["events", "search", "chunkevt", "--from", iso8601Show (addUTCTime (-10800) now), "--to", iso8601Show (addUTCTime 60 now), "--chunk-hours", "0"] `shouldReturn` ids

  it "metrics: query, --assert gates the exit code (and refuses non-scalars), chart, scoped catalog pages, binned stat coercion" \tr -> do
    apiKey <- createTestAPIKey tr testPid "lifecycle-metrics-key"
    -- the server resolves --since against the frozen clock
    forM_ [60, 120] \secsAgo -> ingestLog tr apiKey "metricsevt marker" (addUTCTime (negate secsAgo) frozenTime)
    let count = ["metrics", "query", "body has \"metricsevt\" | summarize count()", "--since", "1h"]
    cliJson tr count >>= (`shouldHaveKeys` ["dataset"])
    cliJson tr ["metrics", "query", "summarize count() by bin_auto(timestamp) | where resource.service.name == 'nonexistent_xyzzy'", "--since", "1h"] >>= \v -> v ^? key "rows_count" . _Number `shouldBe` Just 0
    forM_ [("> 0", ExitSuccess), ("< 0", ExitFailure 1)] \(assertion, code) -> fst <$> runCLILifecycle tr ("--json" : count <> ["--assert", assertion]) `shouldReturn` code
    (nonScalarEc, nonScalar) <- runCLILifecycle tr ["--json", "metrics", "query", "summarize count() by resource.service.name", "--assert", "< 100"]
    nonScalarEc `shouldBe` ExitFailure 1
    jsonOut nonScalar >>= (`shouldHaveKeys` ["dataset"])
    cliOk tr ["metrics", "chart", "body has \"metricsevt\" | summarize count() by bin_auto(timestamp)", "--since", "1h"] >>= (`shouldSatisfy` (not . T.null))
    stat <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTFloat) (Just testPid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing Nothing (Just $ toText $ iso8601Show $ addUTCTime (-3600) frozenTime) (Just $ toText $ iso8601Show $ addUTCTime 3600 frozenTime) (Just "spans") Nothing []
    (stat.error, isJust stat.dataFloat) `shouldBe` (Nothing, True)

    let oldTime = addUTCTime (-(8 * 24 * 3600)) frozenTime
    forM_ @[] [("checkout.duration", frozenTime), ("checkout.requests", frozenTime), ("checkout.old", oldTime)] \(name :: Text, seen) ->
      dbExec
        tr
        [sql| INSERT INTO otel_metrics_meta (project_id, metric_name, metric_type, metric_unit, metric_description, service_name, scope_name, metric_labels, first_seen_at, last_seen_at, first_timestamp, last_timestamp)
              VALUES (?, ?, 'GAUGE', 'ms', 'request metric', 'checkout', 'test', '{}', ?, ?, ?, ?) |]
        (testPid, name, seen, seen, seen, seen)
    let catalog args = cliJson tr (["metrics", "list", "--service", "checkout"] <> args)
        names v = v ^.. key "metrics" . values . key "metricName" . _String
    page <- catalog ["--limit", "1"]
    page `shouldHaveKeys` ["metrics", "total", "active", "hasMore", "activeSince"]
    names page `shouldBe` ["checkout.duration"]
    cliOk tr ["--table", "metrics", "list", "--service", "checkout", "--limit", "1"] >>= (`shouldSatisfy` T.isInfixOf "More metrics are available")
    catalog ["--offset", "5"] >>= \v -> v ^? key "total" `shouldBe` Just (AE.Number 2)
    inactive <- catalog ["--inactive"]
    (names inactive, inactive ^? key "active", inactive ^? key "total") `shouldBe` (["checkout.old"], Just (AE.Bool False), Just (AE.Number 1))

  it "telemetry export: send-event lands in search, a rejected export exits 1 without claiming delivery, a browser-login session needs no exporter overrides" \tr -> do
    apiKey <- createTestAPIKey tr testPid "lifecycle-send-key"
    runQueryEffect tr $ Projects.insertSession (Projects.PersistentSessionId $ UUID.fromWords 0 0 0 2) (Servant.getResponse tr.trSessAndHeader).user.id (Projects.SessionData mempty)
    withLiveServer tr \port -> do
      let server = "http://127.0.0.1:" <> show port
      withEnv [("MONOSCOPE_TEST_API_KEY", toString apiKey), ("OTEL_EXPORTER_OTLP_ENDPOINT", server <> "/missing")]
        $ forM_ [["telemetrygen", "--kind=trace", "--count=1", "--rate=1000"], ["send-event", "-m", "rejected-event"]] \args -> do
          (ec, out) <- runCLILifecycleLive args
          ec `shouldBe` ExitFailure 1
          out `shouldNotSatisfy` \o -> "Sent " `T.isInfixOf` o || "Event sent" `T.isInfixOf` o
      withEnv [("MONOSCOPE_TEST_API_KEY", toString apiKey), ("OTEL_EXPORTER_OTLP_ENDPOINT", server)]
        $ void
        $ cliOk tr ["send-event", "-m", "Deploy completed lifecycleping", "--service", "lifecycle-cli", "-t", "deploy.id:d1"]
      unsetEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
      (loginEc, _) <- withEnv [("MONOSCOPE_TEST_API_KEY", "00000000-0000-0000-0000-000000000002"), ("MONOSCOPE_API_URL", server)] $ runCLILifecycleLive ["telemetrygen", "--kind=trace", "--count=3", "--rate=1000", "--service", "cli-login-regression", "--resource", "service.name:overridden"]
      loginEc `shouldBe` ExitSuccess
    -- spans land at wall-clock time; the in-process server resolves --since against the frozen clock
    now <- getCurrentTime
    let window = ["--from", iso8601Show (addUTCTime (-3600) now), "--to", iso8601Show (addUTCTime 3600 now)]
    sent <- eventually (cliJson tr (["events", "search", "lifecycleping"] <> window)) (not . null . (^.. key "events" . values))
    sent ^.. key "events" . values `shouldSatisfy` (not . null)
    loginSpans <- cliJson tr (["traces", "search", "resource.service.name == \"cli-login-regression\""] <> window)
    length (loginSpans ^.. key "events" . values) `shouldBe` 3
  where
    -- Server logs use their own sink while the in-process CLI captures stdout.
    withCliResources run = withTestResources \tr -> do
      messages <- LogList.newLogList
      LogList.withLogListLogger messages (\logger -> run tr{trLogger = logger})
        `finally` (LogList.getLogList messages >>= mapM_ (putTextLn . showLogMessage Nothing))
