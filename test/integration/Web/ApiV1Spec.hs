module Web.ApiV1Spec (spec) where

import Control.Lens (_Just, (^.), (^..), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.Lens (key, _Array, _Number, _Object, _String)
import Data.Default (def)
import Data.Map qualified as Map
import Data.OpenApi (OpenApi, info, title, version)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as PM
import Models.Telemetry.Schema qualified as Schema
import Pages.Charts.Charts qualified as Charts
import Pages.Charts.Types (MetricsData (..))
import Pages.LogExplorer.Log qualified as Log
import Pkg.TestUtils
import Relude
import Servant (NoContent (..))
import System.Types (ATBaseCtx)
import Test.Hspec
import Web.ApiHandlers qualified as ApiH
import Web.ApiTypes qualified as ApiT
import Web.Auth (resolveApiKeyProject)
import Web.MCP qualified as MCP
import Web.Routes (apiV1OpenApiSpec, apiV1Server)
import Web.Routes qualified as Routes

import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Types (effToServantHandlerTest)


specJson :: AE.Value
specJson = AE.toJSON apiV1OpenApiSpec


spec :: Spec
spec = aroundAll withTestResources do
  describe "API v1" do
    describe "OpenAPI spec" do
      it "round-trips through JSON and has correct metadata" $ \_tr -> do
        AE.eitherDecode @OpenApi (AE.encode apiV1OpenApiSpec) `shouldSatisfy` isRight
        (apiV1OpenApiSpec ^. info . title) `shouldBe` "Monoscope API"
        (apiV1OpenApiSpec ^. info . version) `shouldBe` "1.0"

      it "has server base path and security definitions" $ \_tr -> do
        (specJson ^? key "servers" . _Array) `shouldSatisfy` \case
          Just arr -> not (null arr)
          Nothing -> False
        (specJson ^? key "components" . key "securitySchemes" . key "BearerAuth") `shouldSatisfy` isJust
        (specJson ^? key "security" . _Array) `shouldSatisfy` \case
          Just arr -> not (null arr)
          Nothing -> False

      -- This list doubles as the CLI contract: every path the CLI constructs
      -- (without the /api/v1 prefix the Servant base already provides) must
      -- appear here. If a Servant route is renamed, this test fails first.
      it "declares all expected endpoint paths" $ \_tr -> do
        let expectedPaths :: [Text]
            expectedPaths =
              [ "/events"
              , "/metrics"
              , "/schema"
              , "/monitors"
              , "/monitors/{monitor_id}"
              , "/monitors/{monitor_id}/mute"
              , "/monitors/{monitor_id}/unmute"
              , "/monitors/{monitor_id}/resolve"
              , "/monitors/{monitor_id}/toggle_active"
              , "/monitors/bulk"
              , "/dashboards"
              , "/dashboards/{dashboard_id}"
              , "/dashboards/apply"
              , "/dashboards/{dashboard_id}/duplicate"
              , "/dashboards/{dashboard_id}/star"
              , "/dashboards/{dashboard_id}/yaml"
              , "/dashboards/{dashboard_id}/widgets"
              , "/dashboards/{dashboard_id}/widgets/{widget_id}"
              , "/dashboards/{dashboard_id}/widgets/order"
              , "/dashboards/bulk"
              , "/api_keys"
              , "/api_keys/{key_id}"
              , "/api_keys/{key_id}/activate"
              , "/api_keys/{key_id}/deactivate"
              , "/events/query"
              , "/share"
              , "/me"
              , "/project"
              , "/issues"
              , "/issues/{issue_id}"
              , "/issues/{issue_id}/ack"
              , "/issues/{issue_id}/unack"
              , "/issues/{issue_id}/archive"
              , "/issues/{issue_id}/unarchive"
              , "/issues/bulk"
              , "/endpoints"
              , "/endpoints/{endpoint_id}"
              , "/log_patterns"
              , "/log_patterns/{pattern_id}"
              , "/log_patterns/{pattern_id}/ack"
              , "/log_patterns/bulk"
              , "/teams"
              , "/teams/{team_id}"
              , "/teams/bulk"
              , "/members"
              , "/members/{user_id}"
              ]
        for_ expectedPaths $ \p ->
          (specJson ^? key "paths" . key (AEK.fromText p)) `shouldSatisfy` isJust

      it "includes schema definitions for response types" $ \_tr -> do
        let schemas = specJson ^? key "components" . key "schemas" . _Object
        schemas `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "LogResult") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "LogResult" . key "properties" . _Object) `shouldSatisfy` \case
          Just obj -> not (null obj)
          Nothing -> False
        (specJson ^? key "components" . key "schemas" . key "TraceTreeEntry") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "MetricsData") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "Schema") `shouldSatisfy` isJust
        (specJson ^? key "components" . key "schemas" . key "QueryMonitor") `shouldSatisfy` isJust

    describe "API key auth" do
      it "resolves valid keys and rejects invalid ones" $ \tr -> do
        apiKey <- createTestAPIKey tr testPid "test-v1-key"
        result <- runQueryEffect tr $ resolveApiKeyProject apiKey
        result `shouldBe` Just testPid

        invalidResult <- runQueryEffect tr $ resolveApiKeyProject "invalid-key"
        invalidResult `shouldBe` Nothing

        emptyResult <- runQueryEffect tr $ resolveApiKeyProject ""
        emptyResult `shouldBe` Nothing

    describe "Schema endpoint" do
      it "returns schema with expected fields and valid JSON round-trip" $ \_tr -> do
        let schema = Schema.telemetrySchema
        schema.fields `shouldSatisfy` (not . null)
        Map.lookup "timestamp" schema.fields `shouldSatisfy` isJust
        Map.lookup "status_code" schema.fields `shouldSatisfy` isJust
        Map.lookup "body" schema.fields `shouldSatisfy` isJust
        AE.eitherDecode @Schema.Schema (AE.encode schema) `shouldBe` Right schema

    describe "Monitors endpoint" do
      it "returns JSON-serializable monitors list for project" $ \tr -> do
        result <- runQueryEffect tr $ Monitors.queryMonitorsAll testPid
        AE.eitherDecode @[Monitors.QueryMonitor] (AE.encode result) `shouldSatisfy` isRight

    describe "Events endpoint" do
      it "returns valid LogResult with expected JSON structure" $ \tr -> do
        result <-
          toBaseServantResponse tr.trATCtx tr.trLogger
            $ Log.queryEvents testPid (Just "") (Just "1h") Nothing Nothing Nothing Nothing
        let json = AE.toJSON result
        (json ^? key "logsData" . _Array) `shouldSatisfy` isJust
        (json ^? key "cols" . _Array) `shouldSatisfy` isJust
        (json ^? key "colIdxMap" . _Object) `shouldSatisfy` isJust
        (json ^? key "count" . _Number) `shouldSatisfy` isJust
        (json ^? key "traces" . _Array) `shouldSatisfy` isJust
        (json ^? key "hasMore") `shouldSatisfy` isJust

      it "returns 400 for malformed query" $ \tr -> do
        (toBaseServantResponse tr.trATCtx tr.trLogger
          (Log.queryEvents testPid (Just "|| invalid {{") (Just "1h") Nothing Nothing Nothing Nothing)
          >>= evaluateWHNF_) `shouldThrow` anyException

    describe "Metrics endpoint" do
      it "returns valid MetricsData for count query with JSON round-trip" $ \tr -> do
        result <-
          toBaseServantResponse tr.trATCtx tr.trLogger
            $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just testPid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing (Just "1h") Nothing Nothing (Just "spans") []
        result.rowsCount `shouldSatisfy` (>= 0)
        AE.eitherDecode @MetricsData (AE.encode result) `shouldSatisfy` isRight

    describe "Monitor CRUD + lifecycle" do
      it "create → get → patch → mute → unmute → delete round-trip" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            input = (def :: ApiT.MonitorInput){ApiT.title = "t1", ApiT.query = "count(*)", ApiT.alertThreshold = 5, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
        created <- runB $ ApiH.apiMonitorCreate testPid input
        created.alertConfig.title `shouldBe` "t1"
        got <- runB $ ApiH.apiMonitorGet testPid created.id
        got.id `shouldBe` created.id
        patched <- runB $ ApiH.apiMonitorPatch testPid created.id ((def :: ApiT.MonitorPatch){ApiT.title = Just "t2"})
        patched.alertConfig.title `shouldBe` "t2"
        muted <- runB $ ApiH.apiMonitorMute testPid created.id (Just 60)
        muted.mutedUntil `shouldSatisfy` isJust
        unmuted <- runB $ ApiH.apiMonitorUnmute testPid created.id
        unmuted.mutedUntil `shouldBe` Nothing
        NoContent <- runB $ ApiH.apiMonitorDelete testPid created.id
        pass

      it "bulk delete marks monitors as deleted" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            mkInput n = (def :: ApiT.MonitorInput){ApiT.title = "bulk-" <> n, ApiT.query = "count(*)", ApiT.alertThreshold = 1, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
        m1 <- runB $ ApiH.apiMonitorCreate testPid (mkInput "a")
        m2 <- runB $ ApiH.apiMonitorCreate testPid (mkInput "b")
        res <- runB $ ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [m1.id.unQueryMonitorId, m2.id.unQueryMonitorId], ApiT.durationMinutes = Nothing}
        length res.succeeded `shouldBe` 2
        res.failed `shouldBe` []

      it "bulk delete against non-existent ids reports them in failed with error text" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        bogus1 <- UUIDV4.nextRandom
        bogus2 <- UUIDV4.nextRandom
        res <- runB $ ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [bogus1, bogus2], ApiT.durationMinutes = Nothing}
        res.succeeded `shouldBe` []
        length res.failed `shouldBe` 2
        -- SDK-facing wire shape: each failure is {id, error}, not a tuple
        fmap (.id) res.failed `shouldMatchList` [bogus1, bogus2]
        fmap (.error) res.failed `shouldSatisfy` all (== "not applied")

      it "bulk with unknown action raises 400" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        runB (ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "nuke", ApiT.ids = [], ApiT.durationMinutes = Nothing})
          `shouldThrow` anyException

      it "get with non-existent id raises 404" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        bogus <- UUIDV4.nextRandom
        runB (ApiH.apiMonitorGet testPid (Monitors.QueryMonitorId bogus))
          `shouldThrow` anyException

      it "patch propagates notification fields (emails, emailAll, slackChannels)" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            input = (def :: ApiT.MonitorInput){ApiT.title = "notify-t", ApiT.query = "count(*)", ApiT.alertThreshold = 5, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
        created <- runB $ ApiH.apiMonitorCreate testPid input
        patched <-
          runB
            $ ApiH.apiMonitorPatch
              testPid
              created.id
              ((def :: ApiT.MonitorPatch){ApiT.emails = Just ["a@b.com"], ApiT.emailAll = Just True, ApiT.slackChannels = Just ["#alerts"], ApiT.severity = Just "critical"})
        length patched.alertConfig.emails `shouldBe` 1
        patched.alertConfig.emailAll `shouldBe` True
        toList patched.alertConfig.slackChannels `shouldBe` ["#alerts"]
        patched.alertConfig.severity `shouldBe` "critical"

    describe "Dashboard CRUD" do
      it "create → star → unstar → delete round-trip" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            input = ApiT.DashboardInput{ApiT.title = "d1", ApiT.tags = Just ["a"], ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
        created <- runB $ ApiH.apiDashboardCreate testPid input
        created.summary.title `shouldBe` "d1"
        starred <- runB $ ApiH.apiDashboardStar testPid created.summary.id
        starred.summary.starred `shouldBe` True
        NoContent <- runB $ ApiH.apiDashboardUnstar testPid created.summary.id
        NoContent <- runB $ ApiH.apiDashboardDelete testPid created.summary.id
        pass

      it "bulk delete removes multiple dashboards" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            mkInput n = ApiT.DashboardInput{ApiT.title = "bulk-d-" <> n, ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
        d1 <- runB $ ApiH.apiDashboardCreate testPid (mkInput "a")
        d2 <- runB $ ApiH.apiDashboardCreate testPid (mkInput "b")
        res <- runB $ ApiH.apiDashboardBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [d1.summary.id.unwrap, d2.summary.id.unwrap], ApiT.durationMinutes = Nothing}
        length res.succeeded `shouldBe` 2

      it "apply upserts by file_path (idempotent)" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            doc = ApiT.DashboardYAMLDoc{ApiT.filePath = "dashes/foo.yaml", ApiT.title = Just "foo", ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.schema = def}
        d1 <- runB $ ApiH.apiDashboardApply testPid doc
        d2 <- runB $ ApiH.apiDashboardApply testPid doc
        d1.summary.id `shouldBe` d2.summary.id -- same row updated, not inserted again

    describe "API keys CRUD" do
      it "create returns plaintext once; list includes it; delete deactivates" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        created <- runB $ ApiH.apiKeyCreate testPid ApiT.ApiKeyCreate{ApiT.title = "k1"}
        created.summary.title `shouldBe` "k1"
        created.key `shouldSatisfy` not . T.null
        keys <- runB $ ApiH.apiKeysList testPid
        (created.summary.id `elem` fmap (.id) keys) `shouldBe` True
        deact <- runB $ ApiH.apiKeyDeactivate testPid created.summary.id
        deact.active `shouldBe` False

    describe "Events body query" do
      it "accepts EventsQuery payload and returns LogResult" $ \tr -> do
        result <-
          toBaseServantResponse tr.trATCtx tr.trLogger
            $ ApiH.apiEventsQuery testPid (def :: ApiT.EventsQuery){ApiT.query = Just "", ApiT.since = Just "1h"}
        let json = AE.toJSON result
        (json ^? key "logsData" . _Array) `shouldSatisfy` isJust
        (json ^? key "count" . _Number) `shouldSatisfy` isJust

    describe "Plan B — /me + project singular" do
      it "returns project id and summary for /me" $ \tr -> do
        res <- runAsBase tr (ApiH.apiMe testPid)
        res.projectId `shouldBe` testPid
        res.project.id `shouldBe` testPid

      it "project get/patch round-trip updates title" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            emptyPatch = ApiT.ProjectPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        original <- runB (ApiH.apiProjectGet testPid)
        patched <- runB (ApiH.apiProjectPatch testPid emptyPatch{ApiT.title = Just "Plan B Patched"})
        patched.summary.title `shouldBe` "Plan B Patched"
        _ <- runB (ApiH.apiProjectPatch testPid emptyPatch{ApiT.title = Just original.summary.title})
        pass

    describe "Plan B — issues/endpoints/log-patterns listing" do
      it "issues list returns a Paged envelope" $ \tr -> do
        res <- runAsBase tr (ApiH.apiIssuesList testPid Nothing Nothing Nothing Nothing Nothing)
        res.perPage `shouldSatisfy` (> 0)
        res.totalCount `shouldSatisfy` (>= 0)

      it "issues bulk with empty ids succeeds with empty result" $ \tr -> do
        res <- runAsBase tr (ApiH.apiIssuesBulk testPid ApiT.BulkAction{ApiT.action = "acknowledge", ApiT.ids = [], ApiT.durationMinutes = Nothing})
        res.succeeded `shouldBe` []

      it "endpoints list returns a Paged envelope" $ \tr -> do
        res <- runAsBase tr (ApiH.apiEndpointsList testPid Nothing Nothing Nothing Nothing)
        res.perPage `shouldSatisfy` (> 0)

      it "log patterns list returns a Paged envelope" $ \tr -> do
        res <- runAsBase tr (ApiH.apiLogPatternsList testPid Nothing Nothing)
        res.perPage `shouldSatisfy` (> 0)

      it "log patterns bulk with empty ids succeeds with empty result" $ \tr -> do
        res <- runAsBase tr (ApiH.apiLogPatternsBulk testPid ApiT.BulkAction{ApiT.action = "acknowledge", ApiT.ids = [], ApiT.durationMinutes = Nothing})
        res.succeeded `shouldBe` []

    describe "Plan B — teams CRUD" do
      it "create → get → patch → delete round-trip" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            input =
              ApiT.TeamInput
                { ApiT.name = "ops"
                , ApiT.handle = "ops-team"
                , ApiT.description = Just "Ops oncall"
                , ApiT.members = Nothing
                , ApiT.notifyEmails = Just ["ops@example.com"]
                , ApiT.slackChannels = Nothing
                , ApiT.discordChannels = Nothing
                , ApiT.phoneNumbers = Nothing
                , ApiT.pagerdutyServices = Nothing
                }
        created <- runB $ ApiH.apiTeamCreate testPid input
        created.summary.name `shouldBe` "ops"
        created.summary.handle `shouldBe` "ops-team"
        created.summary.isEveryone `shouldBe` False
        got <- runB $ ApiH.apiTeamGet testPid created.summary.id
        got.summary.id `shouldBe` created.summary.id
        patched <-
          runB
            $ ApiH.apiTeamPatch
              testPid
              created.summary.id
              (ApiT.TeamPatch (Just "ops-v2") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        patched.summary.name `shouldBe` "ops-v2"
        NoContent <- runB $ ApiH.apiTeamDelete testPid created.summary.id
        pass

      it "create with 'everyone' handle is rejected" $ \tr -> do
        let input =
              ApiT.TeamInput
                { ApiT.name = "everyone"
                , ApiT.handle = "everyone"
                , ApiT.description = Nothing
                , ApiT.members = Nothing
                , ApiT.notifyEmails = Nothing
                , ApiT.slackChannels = Nothing
                , ApiT.discordChannels = Nothing
                , ApiT.phoneNumbers = Nothing
                , ApiT.pagerdutyServices = Nothing
                }
        (runAsBase tr (ApiH.apiTeamCreate testPid input) >>= evaluateWHNF_) `shouldThrow` anyException

      it "teams list includes the built-in everyone team" $ \tr -> do
        teams <- runAsBase tr (ApiH.apiTeamsList testPid)
        any (.isEveryone) teams `shouldBe` True

      it "bulk delete over teams returns success count" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
            mkInput n =
              ApiT.TeamInput
                { ApiT.name = "t-" <> n
                , ApiT.handle = "t-" <> n
                , ApiT.description = Nothing
                , ApiT.members = Nothing
                , ApiT.notifyEmails = Nothing
                , ApiT.slackChannels = Nothing
                , ApiT.discordChannels = Nothing
                , ApiT.phoneNumbers = Nothing
                , ApiT.pagerdutyServices = Nothing
                }
        t1 <- runB $ ApiH.apiTeamCreate testPid (mkInput "a")
        t2 <- runB $ ApiH.apiTeamCreate testPid (mkInput "b")
        res <-
          runB
            $ ApiH.apiTeamsBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [t1.summary.id, t2.summary.id], ApiT.durationMinutes = Nothing}
        length res.succeeded `shouldBe` 2

    describe "Plan B — members" do
      it "add by email → patch permission → remove" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        added <-
          runB
            $ ApiH.apiMemberAdd
              testPid
              ApiT.MemberAdd{ApiT.email = Just "member@example.com", ApiT.userId = Nothing, ApiT.permission = Just PM.PView}
        added.email `shouldBe` "member@example.com"
        added.permission `shouldBe` PM.PView
        patched <- runB $ ApiH.apiMemberPatch testPid added.userId (ApiT.MemberPatch PM.PAdmin)
        patched.permission `shouldBe` PM.PAdmin
        NoContent <- runB $ ApiH.apiMemberRemove testPid added.userId
        pass

      it "add without email or user_id is rejected" $ \tr -> do
        (runAsBase tr (ApiH.apiMemberAdd testPid ApiT.MemberAdd{ApiT.email = Nothing, ApiT.userId = Nothing, ApiT.permission = Nothing})
          >>= evaluateWHNF_) `shouldThrow` anyException

    describe "MCP" do
      let reg = MCP.allTools apiV1OpenApiSpec
          dummyApp _ = error "dummyApp invoked unexpectedly"
          buildTestApp tr pid =
            genericServeTWithContext
              (effToServantHandlerTest tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
              (apiV1Server tr.trLogger tr.trATCtx tr.trTracerProvider pid)
              Servant.EmptyContext
          -- Top-level Servant app for the e2e tests below — same wiring as
          -- mkServer in production minus the WAI middleware stack. Hitting
          -- /api/v1/mcp through this exercises auth + JSON-RPC + dispatch.
          mkTopApp tr =
            genericServeTWithContext
              (effToServantHandlerTest tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
              (Routes.server tr.trLogger tr.trATCtx tr.trTracerProvider)
              (Routes.genAuthServerContext tr.trLogger tr.trATCtx)
          mcpHttp tr authHdr body =
            let req =
                  WT.setPath
                    Wai.defaultRequest
                      { Wai.requestMethod = H.methodPost
                      , Wai.requestHeaders = ("Content-Type", "application/json") : authHdr
                      }
                    "/api/v1/mcp"
             in WT.runSession (WT.srequest (WT.SRequest req (AE.encode body))) (mkTopApp tr)
          rpcCall body =
            AE.object
              [ "jsonrpc" AE..= ("2.0" :: Text)
              , "id" AE..= (1 :: Int)
              , "method" AE..= ("tools/call" :: Text)
              , "params" AE..= body
              ]
          rpcCallNamed name args =
            rpcCall (AE.object ["name" AE..= (name :: Text), "arguments" AE..= args])
          rpcSimple m =
            AE.object
              [ "jsonrpc" AE..= ("2.0" :: Text)
              , "id" AE..= (1 :: Int)
              , "method" AE..= (m :: Text)
              ]

      it "registry uses verb-first canonical tool names" $ \_tr -> do
        Map.member "search_events" reg `shouldBe` True
        Map.member "list_events" reg `shouldBe` True
        Map.member "list_monitors" reg `shouldBe` True
        Map.member "get_monitor" reg `shouldBe` True
        Map.member "mute_monitor" reg `shouldBe` True
        Map.member "apply_dashboard" reg `shouldBe` True
        Map.member "get_dashboard_yaml" reg `shouldBe` True
        Map.member "get_schema" reg `shouldBe` True
        Map.member "whoami" reg `shouldBe` True

      it "does not expose the MCP endpoint as its own tool" $ \_tr -> do
        Map.member "post_mcp" reg `shouldBe` False
        Map.member "mcp_post" reg `shouldBe` False

      it "tools/list returns named tools with name/description/inputSchema" $ \tr -> do
        let req =
              AE.object
                [ "jsonrpc" AE..= ("2.0" :: Text)
                , "id" AE..= (1 :: Int)
                , "method" AE..= ("tools/list" :: Text)
                ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        let tools = resp ^.. key "result" . key "tools" . _Array . traverse
        tools `shouldSatisfy` (not . null)
        case tools of
          (t : _) -> for_ ["name", "description", "inputSchema"] $ \k ->
            (t ^? key (AEK.fromText k)) `shouldSatisfy` isJust
          [] -> expectationFailure "expected at least one tool"

      it "initialize returns protocolVersion + serverInfo" $ \tr -> do
        let req =
              AE.object
                [ "jsonrpc" AE..= ("2.0" :: Text)
                , "id" AE..= (1 :: Int)
                , "method" AE..= ("initialize" :: Text)
                ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        (resp ^? key "result" . key "protocolVersion") `shouldSatisfy` isJust
        (resp ^? key "result" . key "serverInfo" . key "name") `shouldSatisfy` isJust

      it "tools/call get_schema round-trips through to Schema.telemetrySchema" $ \tr -> do
        let req =
              rpcCall
                $ AE.object
                  [ "name" AE..= ("get_schema" :: Text)
                  , "arguments" AE..= AE.object []
                  ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg (buildTestApp tr) testPid req)
        let isErr = resp ^? key "result" . key "isError"
            content = resp ^? key "result" . key "content" . _Array . traverse . key "text"
        when (isErr /= Just (AE.Bool False))
          $ expectationFailure ("schema_get returned isError; body: " <> show content)
        let structured = resp ^? key "result" . key "structuredContent"
        case structured of
          Just v -> case AE.fromJSON @Schema.Schema v of
            AE.Success s -> Map.keys s.fields `shouldMatchList` Map.keys Schema.telemetrySchema.fields
            AE.Error e -> expectationFailure ("structuredContent did not decode as Schema: " <> e)
          Nothing -> expectationFailure ("structuredContent missing; body: " <> show content)

      it "tools/call with unknown tool name returns isError result, not JSON-RPC error" $ \tr -> do
        let req =
              rpcCall
                $ AE.object
                  [ "name" AE..= ("does_not_exist" :: Text)
                  , "arguments" AE..= AE.object []
                  ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        (resp ^? key "result" . key "isError") `shouldBe` Just (AE.Bool True)
        (resp ^? key "error") `shouldBe` Nothing

      it "tools/list includes composite workflow tools alongside REST tools" $ \tr -> do
        let req =
              AE.object
                [ "jsonrpc" AE..= ("2.0" :: Text)
                , "id" AE..= (1 :: Int)
                , "method" AE..= ("tools/list" :: Text)
                ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        let names :: [Text]
            names =
              resp
                ^.. key "result"
                  . key "tools"
                  . _Array
                  . traverse
                  . key "name"
                  . _String
        ("find_error_patterns" `elem` names) `shouldBe` True
        ("search_events_nl" `elem` names) `shouldBe` True
        ("analyze_issue" `elem` names) `shouldBe` True
        -- And REST tools still present
        ("get_schema" `elem` names) `shouldBe` True

      it "tools/call find_error_patterns returns OK with patterns array" $ \tr -> do
        let req =
              rpcCall
                $ AE.object
                  [ "name" AE..= ("find_error_patterns" :: Text)
                  , "arguments" AE..= AE.object ["limit" AE..= (5 :: Int)]
                  ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        (resp ^? key "result" . key "isError") `shouldBe` Just (AE.Bool False)
        (resp ^? key "result" . key "structuredContent" . key "patterns" . _Array) `shouldSatisfy` isJust
        (resp ^? key "result" . key "structuredContent" . key "as_of") `shouldSatisfy` isJust

      it "tools/call search_events_nl with empty input returns isError" $ \tr -> do
        let req =
              rpcCall
                $ AE.object
                  [ "name" AE..= ("search_events_nl" :: Text)
                  , "arguments" AE..= AE.object ["input" AE..= ("" :: Text)]
                  ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        (resp ^? key "result" . key "isError") `shouldBe` Just (AE.Bool True)

      it "tools/call analyze_issue with bad UUID returns isError, not JSON-RPC error" $ \tr -> do
        let req =
              rpcCall
                $ AE.object
                  [ "name" AE..= ("analyze_issue" :: Text)
                  , "arguments" AE..= AE.object ["issue_id" AE..= ("not-a-uuid" :: Text)]
                  ]
        resp <- runAsBase tr (MCP.handleJsonRpc reg dummyApp testPid req)
        (resp ^? key "result" . key "isError") `shouldBe` Just (AE.Bool True)
        (resp ^? key "error") `shouldBe` Nothing

      it "e2e: rejects /api/v1/mcp without a valid API key" $ \tr -> do
        resp <- mcpHttp tr [] (rpcSimple "tools/list")
        H.statusCode (WT.simpleStatus resp) `shouldBe` 401

      it "e2e: tools/list through real HTTP route returns the registry" $ \tr -> do
        apiKey <- createTestAPIKey tr testPid "mcp-e2e-list"
        resp <- mcpHttp tr [("Authorization", "Bearer " <> encodeUtf8 apiKey)] (rpcSimple "tools/list")
        H.statusCode (WT.simpleStatus resp) `shouldBe` 200
        let body = AE.decode (WT.simpleBody resp) :: Maybe AE.Value
            names = body ^.. _Just . key "result" . key "tools" . _Array . traverse . key "name" . _String
        for_ ["get_schema", "list_monitors", "find_error_patterns"] $ \n ->
          (n `elem` (names :: [Text])) `shouldBe` True

      it "e2e: tools/call get_schema through real HTTP route returns the schema" $ \tr -> do
        apiKey <- createTestAPIKey tr testPid "mcp-e2e-call"
        resp <-
          mcpHttp tr
            [("Authorization", "Bearer " <> encodeUtf8 apiKey)]
            (rpcCallNamed "get_schema" (AE.object []))
        H.statusCode (WT.simpleStatus resp) `shouldBe` 200
        let body = AE.decode (WT.simpleBody resp) :: Maybe AE.Value
        (body ^? _Just . key "result" . key "isError") `shouldBe` Just (AE.Bool False)
        case body ^? _Just . key "result" . key "structuredContent" of
          Just v -> case AE.fromJSON @Schema.Schema v of
            AE.Success s -> Map.keys s.fields `shouldMatchList` Map.keys Schema.telemetrySchema.fields
            AE.Error e -> expectationFailure ("structuredContent did not decode as Schema: " <> e)
          Nothing -> expectationFailure ("structuredContent missing in body: " <> show body)

    describe "Share link create" do
      it "returns id and url containing /share/r/<id>" $ \tr -> do
        let runB :: ATBaseCtx a -> IO a
            runB k = runAsBase tr k
        eventId <- UUIDV4.nextRandom
        now <- getCurrentTime
        res <- runB $ ApiH.apiShareLinkCreate testPid ApiH.ShareLinkCreate{ApiH.eventId = eventId, ApiH.eventCreatedAt = now, ApiH.eventType = Just "log"}
        UUID.toText res.id `shouldSatisfy` (not . T.null)
        ("/share/r/" `T.isInfixOf` res.url) `shouldBe` True
