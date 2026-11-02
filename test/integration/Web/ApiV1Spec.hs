module Web.ApiV1Spec (spec) where

import Control.Lens (cosmos, ix, (^.), (^..), (^?), _Just)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Array, _Number, _Object, _String)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq (HTTP, runHTTPGolden, runHTTPRecord)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.OpenApi (OpenApi, info, title, version)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful (Eff, IOE, (:>))
import Hasql.Interpolate qualified as HI
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Network.GRPC.Common.Protobuf (Proto (..))
import Network.HTTP.Types qualified as H
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.Charts.Charts qualified as Charts
import Pages.Charts.Types (MetricsData (..))
import Pages.LogExplorer.Log qualified as Log
import Pkg.Components.Widget qualified as Widget
import Pkg.Git qualified as Git
import Pkg.Parser.Expr qualified as ParserExpr
import CLI.Resource qualified as Resource
import Data.Set qualified as S
import Pages.Dashboards qualified as DashPage
import Pkg.AI qualified as AI
import Pkg.AgentCatalog qualified as Catalog
import Pkg.TestUtils
import Servant (NoContent (..))
import Servant qualified
import Servant.Server.Generic (genericServeTWithContext)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATBaseCtx, ApiPrincipal (..), effToServantHandlerTestHTTP)
import Web.ApiHandlers qualified as ApiH
import Web.ApiTypes qualified as ApiT
import Web.Auth (resolveApiKeyProject)
import Web.MCP qualified as MCP
import Web.Routes (apiV1OpenApiSpec, apiV1Server)
import Web.Routes qualified as Routes
import Web.Wire qualified as Wire


-- | The whole server minus production's WAI middleware stack.
topAppWithHTTP :: (forall x es. IOE :> es => Eff (HTTP ': es) x -> Eff es x) -> TestResources -> Wai.Application
topAppWithHTTP http tr =
  genericServeTWithContext
    (effToServantHandlerTestHTTP http tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
    (Routes.server (OtlpServer.httpTracesExport tr.trLogger tr.trATCtx tr.trTracerProvider) (OtlpServer.httpLogsExport tr.trLogger tr.trATCtx tr.trTracerProvider))
    (Routes.genAuthServerContext tr.trLogger tr.trATCtx)


request :: Wai.Application -> H.Method -> ByteString -> H.RequestHeaders -> LByteString -> IO WT.SResponse
request app method path headers body = WT.runSession (WT.srequest (WT.SRequest (WT.setPath Wai.defaultRequest{Wai.requestMethod = method, Wai.requestHeaders = headers} path) body)) app


getPath :: TestResources -> ByteString -> H.RequestHeaders -> IO WT.SResponse
getPath tr path headers = request (topAppWithHTTP (runHTTPGolden "./tests/golden/") tr) H.methodGet path headers ""


status :: WT.SResponse -> Int
status = H.statusCode . WT.simpleStatus


demo, json :: H.Header
demo = ("X-Project-Id", "00000000-0000-0000-0000-000000000000")
json = (H.hContentType, "application/json")


bearer :: Text -> H.Header
bearer k = ("Authorization", encodeUtf8 $ "Bearer " <> k)


rpc :: Text -> [AET.Pair] -> AE.Value
rpc method params = AE.object $ ["jsonrpc" AE..= ("2.0" :: Text), "id" AE..= (1 :: Int), "method" AE..= method] <> ["params" AE..= AE.object params | not (null params)]


toolCall :: Text -> [AET.Pair] -> AE.Value
toolCall name args = rpc "tools/call" ["name" AE..= name, "arguments" AE..= AE.object args]


team :: Text -> Text -> ApiT.TeamInput
team name handle = ApiT.TeamInput name handle Nothing Nothing Nothing Nothing Nothing Nothing Nothing


liveSchemaFields :: Maybe AE.Value -> Either String [Text]
liveSchemaFields = maybe (Left "structuredContent missing") (fmap (Map.keys . (.fields)) . AET.parseEither (AE.parseJSON @Schema.Schema))


spec :: Spec
spec = describe "API v1" do
  it "OpenAPI spec, telemetry schema and MCP registry form the CLI/agent contract" do
    let specJson = AE.toJSON apiV1OpenApiSpec
        at p = foldlM (\v k -> v ^? key (AEK.fromText k)) specJson (T.splitOn "." p)
        filled = maybe False (not . null)
        params p = at ("paths." <> p <> ".get.parameters") ^.. _Just . _Array . traverse . key "name" . _String
        reg = MCP.allTools apiV1OpenApiSpec (\pid tz input -> pure $ Right $ AE.object ["query" AE..= input, "project_id" AE..= pid, "timezone" AE..= tz])
    AE.eitherDecode @OpenApi (AE.encode apiV1OpenApiSpec) `shouldSatisfy` isRight
    (apiV1OpenApiSpec ^. info . title, apiV1OpenApiSpec ^. info . version) `shouldBe` ("Monoscope API", "1.0")
    (filled (at "servers" ^? _Just . _Array), filled (at "security" ^? _Just . _Array), isJust (at "components.securitySchemes.BearerAuth")) `shouldBe` (True, True, True)
    [(p, n) | p <- ["/metrics", "/events"], n <- ["environment", "service"], n `notElem` params p] `shouldBe` []
    (at "components.schemas.EventsQuery.properties" ^? _Just . _Object) `shouldSatisfy` maybe False (\o -> all (`AEKM.member` o) ["environment", "service"])
    (at "components.schemas.LogResult.properties" ^? _Just . _Object) `shouldSatisfy` maybe False (not . null)
    -- Every path the CLI constructs (minus the /api/v1 base): a renamed Servant route fails here first.
    [p | p <- words "/events /metrics /metrics/catalog /schema /monitors /monitors/{monitor_id} /monitors/{monitor_id}/mute /monitors/{monitor_id}/unmute /monitors/{monitor_id}/resolve /monitors/{monitor_id}/toggle_active /monitors/{monitor_id}/yaml /monitors/apply /monitors/bulk /dashboards /dashboards/{dashboard_id} /dashboards/apply /dashboards/{dashboard_id}/duplicate /dashboards/{dashboard_id}/star /dashboards/{dashboard_id}/yaml /dashboards/{dashboard_id}/widgets /dashboards/{dashboard_id}/widgets/{widget_id} /dashboards/{dashboard_id}/widgets/order /dashboards/bulk /api_keys /api_keys/{key_id} /api_keys/{key_id}/activate /api_keys/{key_id}/deactivate /events/query /share /me /project /issues /issues/{issue_id} /issues/{issue_id}/ack /issues/{issue_id}/unack /issues/{issue_id}/archive /issues/{issue_id}/unarchive /issues/bulk /incidents /incidents/{incident_id} /endpoints /endpoints/{endpoint_id} /log_patterns /log_patterns/{pattern_id} /log_patterns/{pattern_id}/ack /log_patterns/bulk /teams /teams/{team_id} /teams/bulk /members /members/{user_id}", isNothing (specJson ^? key "paths" . key (AEK.fromText p))]
      `shouldBe` []
    [s | s <- words "LogResult TraceTreeEntry MetricsData Schema QueryMonitor", isNothing (at ("components.schemas." <> s))] `shouldBe` []
    let schema = Schema.telemetrySchema
    [f | f <- ["timestamp", "status_code", "body"], not (Map.member f schema.fields)] `shouldBe` []
    AE.eitherDecode @Schema.Schema (AE.encode schema) `shouldBe` Right schema
    -- Verb-first canonical toolCall names; the MCP endpoint itself is never a tool.
    [n | n <- words "search_events list_events list_monitors get_monitor mute_monitor apply_dashboard get_dashboard_yaml get_schema whoami list_incidents get_incident list_metrics", not (Map.member n reg)] `shouldBe` []
    filter (`Map.member` reg) ["post_mcp", "mcp_post"] `shouldBe` []

  around withTestResources do
    it "HTTP auth: API keys gate /api/v1 and MCP, the demo project is open, secrets are never echoed or cached" \tr -> do
      apiKey <- createTestAPIKey tr testPid "ingestion-key-auth"
      runQueryEffect tr (traverse (`resolveApiKeyProject` Nothing) ([apiKey, "invalid-key", ""] :: [Text])) `shouldReturn` [Just testPid, Nothing, Nothing]
      let app = topAppWithHTTP (runHTTPGolden "./tests/golden/") tr
          widgetBody extra = AE.encode $ case AE.toJSON ((def :: Widget.Widget){Widget.warningThreshold = Just 7}) of
            AE.Object fields -> AE.Object $ fields <> foldMap (\field -> AEKM.singleton field (AE.toJSON (7 :: Double))) extra
            value -> value
          cases :: [(String, H.Method, ByteString, H.RequestHeaders, LByteString, Int)]
          cases =
            [ ("no auth", H.methodGet, "/api/v1/schema", [], "", 401)
            , ("demo project", H.methodGet, "/api/v1/schema", [demo], "", 200)
            , ("ingestion key, no key", H.methodGet, "/api/v1/ingestion-key", [demo], "", 401)
            , ("ingestion key, invalid key", H.methodGet, "/api/v1/ingestion-key", [demo, bearer "invalid-key"], "", 401)
            , ("mcp, no key", H.methodPost, "/api/v1/mcp", [json], AE.encode (rpc "tools/list" []), 401)
            , -- /chart_data_shot predated signed PNG export and ran the caller's KQL/SQL against any pid, unauthenticated.
              ("chart_data_shot", H.methodGet, "/chart_data_shot?data_type=text&pid=" <> encodeUtf8 testPid.toText <> "&query=summarize%20count()", [], "", 404)
            , ("strict JSON API sort", H.methodGet, "/api/v1/dashboards?sort=bogus", [demo], "", 400)
            , ("widget JSON", H.methodPost, "/p/00000000-0000-0000-0000-000000000000/widget", [json], widgetBody Nothing, 200)
            , ("widget JSON, legacy field", H.methodPost, "/p/00000000-0000-0000-0000-000000000000/widget", [json], widgetBody (Just "arning_threshold"), 400)
            , ("widget JSON, unknown field", H.methodPost, "/p/00000000-0000-0000-0000-000000000000/widget", [json], widgetBody (Just "unexpected_field"), 400)
            ]
      responses <- forM cases \(_, m, p, h, b, _) -> request app m p h b
      [(l, status r) | ((l, _, _, _, _, _), r) <- zip cases responses] `shouldBe` [(l, s) | (l, _, _, _, _, s) <- cases]
      [l | ((l, _, _, _, _, s), r) <- zip cases responses, s == 401, apiKey `T.isInfixOf` decodeUtf8 (WT.simpleBody r)] `shouldBe` []
      accepted <- getPath tr "/api/v1/ingestion-key" [demo, bearer apiKey]
      status accepted `shouldBe` 200
      List.lookup "Cache-Control" (WT.simpleHeaders accepted) `shouldBe` Just "no-store"
      returned <- either fail pure $ AE.eitherDecode @Wire.IngestionKey (WT.simpleBody accepted)
      runQueryEffect tr (resolveApiKeyProject returned.key Nothing) `shouldReturn` Just testPid
      -- MCP through the real route with a key: the registry and a live toolCall call.
      mcpKey <- createTestAPIKey tr testPid "mcp-e2e"
      let mcp body = request app H.methodPost "/api/v1/mcp" [json, bearer mcpKey] (AE.encode body) <&> \r -> (status r, AE.decode @AE.Value (WT.simpleBody r))
      (listStatus, listed) <- mcp (rpc "tools/list" [])
      (callStatus, called) <- mcp (toolCall "get_schema" [])
      (listStatus, callStatus) `shouldBe` (200, 200)
      let names = listed ^.. _Just . key "result" . key "tools" . _Array . traverse . key "name" . _String
      filter (`notElem` names) ["get_schema", "list_monitors", "find_error_patterns"] `shouldBe` []
      called ^? _Just . key "result" . key "isError" `shouldBe` Just (AE.Bool False)
      liveSchemaFields (called ^? _Just . key "result" . key "structuredContent") `shouldBe` Right (Map.keys (Schema.deriveSchema ParserExpr.flattenedOtelAttributes).fields)

    it "HTTP pages: dashboard form fragment, onboarding fallback, avatars, proxied docs" \tr -> do
      newForm <- getPath tr "/p/00000000-0000-0000-0000-000000000000/dashboards/new" []
      status newForm `shouldBe` 200
      let formBody = decodeUtf8 @Text (WT.simpleBody newForm)
      formBody `shouldContainAll` ["id=\"newDashboardForm\"", "data-template=\"nginx.yaml\""]
      filter (`T.isInfixOf` formBody) ["<!DOCTYPE", "id=\"main-sidenav\"", "id=\"dashboardsTableContainer\""] `shouldBe` []
      -- Old onboarding links carry free-text steps: unknown ones fall back to Info.
      onboarding <- getPath tr "/p/00000000-0000-0000-0000-000000000000/onboarding?step=bogus" [demo]
      (status onboarding, "firstName" `T.isInfixOf` decodeUtf8 (WT.simpleBody onboarding)) `shouldBe` (200, True)
      -- No display image: one Gravatar probe with d=404, then local initials, cached a week.
      let uid = (Servant.getResponse tr.trSessAndHeader).persistentSession.user.getUser.id
      runHasqlEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE users.users SET display_image_url = '' WHERE id = #{uid} |]
      requests <- newIORef []
      avatar <- request (topAppWithHTTP (runHTTPRecord requests) tr) H.methodGet (encodeUtf8 $ "/api/avatar/" <> uid.toText) [] ""
      status avatar `shouldBe` 200
      (map fst <$> readIORef requests) >>= (`shouldSatisfy` \case [url] -> "https://www.gravatar.com/avatar/" `T.isPrefixOf` url && "?d=404" `T.isSuffixOf` url; _ -> False)
      List.lookup H.hCacheControl (WT.simpleHeaders avatar) `shouldBe` Just "public, max-age=604800"
      -- nginx keeps the first of duplicate Content-Types, which served the SVG fallback as octet-stream.
      unknown <- getPath tr "/api/avatar/00000000-0000-0000-0000-00000000dead" []
      status unknown `shouldBe` 200
      filter ((== H.hContentType) . fst) (WT.simpleHeaders unknown) `shouldBe` [(H.hContentType, "image/svg+xml")]
      decodeUtf8 @Text (WT.simpleBody unknown) `shouldSatisfy` T.isInfixOf "?</text>"
      docs <- getPath tr "/proxy/docs/sdks/nodejs/expressjs" [(H.hAccept, "text/html"), ("HX-Request", "true")]
      status docs `shouldBe` 200
      filter ((== H.hContentType) . fst) (WT.simpleHeaders docs) `shouldBe` [(H.hContentType, "text/html;charset=utf-8")]
      decodeUtf8 @Text (WT.simpleBody docs) `shouldContainAll` ["<article id=\"mainArticle\">", "<h1>ExpressJS integration</h1>", "href=\"https://monoscope.tech/docs/sdks/\""]

    it "monitors: CRUD, persisted active/scope/compiled SQL, notification fields, bulk results and 404" \tr -> do
      let runB :: ATBaseCtx a -> IO a
          runB = toBaseServantResponse tr
          stored mid = runQueryEffect tr (Monitors.queryMonitorById mid) >>= maybe (fail "monitor not stored") pure
          input t q = (def :: ApiT.MonitorInput){ApiT.title = t, ApiT.query = q, ApiT.alertThreshold = 5, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
      created <- runB $ ApiH.apiMonitorCreate testPid (input "t1" "status_code == 200 | summarize count()" :: ApiT.MonitorInput){ApiT.unit = Just " s ", ApiT.environment = Just "production", ApiT.service = Just "checkout"}
      (created.alertConfig.title, created.logQuery, created.environment, created.service) `shouldBe` ("t1", "status_code == 200 | summarize count()", Just "production", Just "checkout")
      -- Scope lives outside the customer's KQL, in the compiled SQL alert evaluation runs.
      (.logQueryAsSql) <$> stored created.id >>= (`shouldContainAll` ["resource___deployment___environment___name = 'production'", "resource___service___name = 'checkout'", "200"])
      runQueryEffect tr (Monitors.queryMonitorsAll testPid) >>= \all' -> do
        AE.eitherDecode @[Monitors.QueryMonitor] (AE.encode all') `shouldSatisfy` isRight
        map (.id) all' `shouldBe` [created.id]
      got <- runB $ ApiH.apiMonitorGet testPid created.id
      (got.id, got.alertConfig.unit) `shouldBe` (created.id, Just "s")
      (.alertConfig.title) <$> runB (ApiH.apiMonitorPatch testPid created.id (def :: ApiT.MonitorPatch){ApiT.title = Just "t2"}) `shouldReturn` "t2"
      (.alertConfig.unit) <$> runB (ApiH.apiMonitorGet testPid created.id) `shouldReturn` Just "s"
      void $ runB $ ApiH.apiMonitorPatch testPid created.id (def :: ApiT.MonitorPatch){ApiT.unit = Just "ms"}
      exported <- runB $ ApiH.apiMonitorYaml testPid created.id
      (exported.unit, exported.environment, exported.service) `shouldBe` (Just "ms", Just "production", Just "checkout")
      -- PATCH used to keep the OLD compiled SQL, so the monitor kept alerting on its previous query.
      void $ runB $ ApiH.apiMonitorPatch testPid created.id (def :: ApiT.MonitorPatch){ApiT.query = Just "status_code == 500"}
      recompiled <- stored created.id
      recompiled.logQuery `shouldBe` "status_code == 500"
      recompiled.logQueryAsSql `shouldSatisfy` T.isInfixOf "500"
      notify <- runB $ ApiH.apiMonitorPatch testPid created.id (def :: ApiT.MonitorPatch){ApiT.emails = Just ["a@b.com"], ApiT.emailAll = Just True, ApiT.slackChannels = Just ["#alerts"], ApiT.severity = Just "critical"}
      (length notify.alertConfig.emails, notify.alertConfig.emailAll, toList notify.alertConfig.slackChannels, notify.alertConfig.severity) `shouldBe` (1, True, ["#alerts"], "critical")
      (.mutedUntil) <$> runB (ApiH.apiMonitorMute testPid created.id (Just 60)) >>= (`shouldSatisfy` isJust)
      (.mutedUntil) <$> runB (ApiH.apiMonitorUnmute testPid created.id) `shouldReturn` Nothing
      -- deactivated_at was DB-inert on upsert: assert against a re-read, never the handler's return.
      void $ runB $ ApiH.apiMonitorUpdate testPid created.id (input "toggle" "count(*)" :: ApiT.MonitorInput){ApiT.active = Just False}
      (.deactivatedAt) <$> stored created.id >>= (`shouldSatisfy` isJust)
      void $ runB $ ApiH.apiMonitorUpdate testPid created.id (input "toggle" "count(*)" :: ApiT.MonitorInput){ApiT.active = Just True}
      (.deactivatedAt) <$> stored created.id `shouldReturn` Nothing
      inactive <- runB $ ApiH.apiMonitorCreate testPid (input "inactive-on-create" "count(*)" :: ApiT.MonitorInput){ApiT.active = Just False}
      inactive.deactivatedAt `shouldSatisfy` isJust
      (.deactivatedAt) <$> stored inactive.id >>= (`shouldSatisfy` isJust)
      spare <- runB $ ApiH.apiMonitorCreate testPid (input "bulk-b" "count(*)")
      deleted <- runB $ ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [inactive.id.unwrap, spare.id.unwrap], ApiT.durationMinutes = Nothing}
      (length deleted.succeeded, deleted.failed) `shouldBe` (2, [])
      bogus <- replicateM 2 UUIDV4.nextRandom
      missing <- runB $ ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = bogus, ApiT.durationMinutes = Nothing}
      -- SDK wire shape: each failure is {id, error}, not a tuple.
      missing.succeeded `shouldBe` []
      map (\f -> (f.id, f.error)) missing.failed `shouldMatchList` map (,"not applied") bogus
      runB (ApiH.apiMonitorBulk testPid ApiT.BulkAction{ApiT.action = "nuke", ApiT.ids = [], ApiT.durationMinutes = Nothing}) `shouldThrow` anyException
      runB (ApiH.apiMonitorGet testPid (Monitors.QueryMonitorId UUID.nil)) `shouldThrow` anyException
      NoContent <- runB $ ApiH.apiMonitorDelete testPid created.id
      pass

    it "dashboards: create into everyone, star, apply idempotently without taking over repository-owned paths, bulk and single delete" \tr -> do
      let runB :: ATBaseCtx a -> IO a
          runB = toBaseServantResponse tr
          input t = ApiT.DashboardInput{ApiT.title = t, ApiT.tags = Just ["a"], ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
          doc p t = ApiT.DashboardYAMLDoc{ApiT.filePath = p, ApiT.title = Just t, ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.schema = def}
      created <- runB $ ApiH.apiDashboardCreate ApiT.ApiKeyPrincipal testPid (input "d1")
      everyone <- runB $ PM.getEveryoneTeam testPid
      (created.summary.title, created.summary.teams) `shouldBe` ("d1", V.fromList (map (.id) $ maybeToList everyone))
      V.length created.summary.teams `shouldBe` 1
      (.summary.id) <$> runB (ApiH.apiDashboardGet testPid created.summary.id) `shouldReturn` created.summary.id
      (.summary.starred) <$> runB (ApiH.apiDashboardStar testPid created.summary.id) `shouldReturn` True
      NoContent <- runB $ ApiH.apiDashboardUnstar testPid created.summary.id
      applied <- runB $ ApiH.apiDashboardApply ApiT.ApiKeyPrincipal testPid (doc "dashes/foo.yaml" "foo")
      (.summary.id) <$> runB (ApiH.apiDashboardApply ApiT.ApiKeyPrincipal testPid (doc "dashes/foo.yaml" "foo")) `shouldReturn` applied.summary.id
      Just sync <- runQueryEffect tr $ GitSync.insertGitHubSync (encodeUtf8 tr.trATCtx.config.apiKeyEncryptionSecretKey) testPid Git.GitHub Nothing "team" "dashboards" "main" (GitSync.AppInstallation 42) Nothing ""
      remote <- runB $ ApiH.apiDashboardApply ApiT.ApiKeyPrincipal testPid (doc "overview.yaml" "Repository overview")
      runQueryEffect tr (GitSync.assignDashboardRepository testPid remote.summary.id sync.id) `shouldReturn` Right ()
      localDash <- runB $ ApiH.apiDashboardApply ApiT.ApiKeyPrincipal testPid (doc "overview.yaml" "Local overview")
      localDash.summary.id `shouldNotBe` remote.summary.id
      fmap (.title) <$> runQueryEffect tr (Dashboards.getDashboardByProjectId testPid remote.summary.id) `shouldReturn` Just "Repository overview"
      bulk <- runB $ ApiH.apiDashboardBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [applied.summary.id.unwrap, localDash.summary.id.unwrap], ApiT.durationMinutes = Nothing}
      length bulk.succeeded `shouldBe` 2
      NoContent <- runB $ ApiH.apiDashboardDelete testPid created.summary.id
      pass

    it "teams, members, API keys, project and /me round-trip" \tr -> do
      let runB :: ATBaseCtx a -> IO a
          runB = toBaseServantResponse tr
      created <- runB $ ApiH.apiTeamCreate testPid $ ApiT.TeamInput "ops" "ops-team" (Just "Ops oncall") Nothing (Just ["ops@example.com"]) Nothing Nothing Nothing Nothing
      (created.summary.name, created.summary.handle, created.summary.isEveryone) `shouldBe` ("ops", "ops-team", False)
      (.summary.id) <$> runB (ApiH.apiTeamGet testPid created.summary.id) `shouldReturn` created.summary.id
      (.summary.name) <$> runB (ApiH.apiTeamPatch testPid created.summary.id (ApiT.TeamPatch (Just "ops-v2") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)) `shouldReturn` "ops-v2"
      (runB (ApiH.apiTeamCreate testPid (team "everyone" "everyone")) >>= evaluateWHNF_) `shouldThrow` anyException
      runB (ApiH.apiTeamsList testPid) >>= (`shouldSatisfy` any (.isEveryone))
      NoContent <- runB $ ApiH.apiTeamDelete testPid created.summary.id
      t1 <- runB $ ApiH.apiTeamCreate testPid (team "t-a" "t-a")
      t2 <- runB $ ApiH.apiTeamCreate testPid (team "t-b" "t-b")
      (length . (.succeeded)) <$> runB (ApiH.apiTeamsBulk testPid ApiT.BulkAction{ApiT.action = "delete", ApiT.ids = [t1.summary.id, t2.summary.id], ApiT.durationMinutes = Nothing}) `shouldReturn` 2

      added <- runB $ ApiH.apiMemberAdd testPid ApiT.MemberAdd{ApiT.email = Just "member@example.com", ApiT.userId = Nothing, ApiT.permission = Just PM.PView}
      (added.email, added.permission) `shouldBe` ("member@example.com", PM.PView)
      (.permission) <$> runB (ApiH.apiMemberPatch testPid added.userId (ApiT.MemberPatch PM.PAdmin)) `shouldReturn` PM.PAdmin
      NoContent <- runB $ ApiH.apiMemberRemove testPid added.userId
      (runB (ApiH.apiMemberAdd testPid ApiT.MemberAdd{ApiT.email = Nothing, ApiT.userId = Nothing, ApiT.permission = Nothing}) >>= evaluateWHNF_) `shouldThrow` anyException

      key' <- runB $ ApiH.apiKeyCreate testPid ApiT.ApiKeyCreate{ApiT.title = "k1"}
      (key'.summary.title, T.null key'.key) `shouldBe` ("k1", False)
      runB (ApiH.apiKeysList testPid) >>= (`shouldSatisfy` elem key'.summary.id . fmap (.id))
      (.id) <$> runB (ApiH.apiKeyGet testPid key'.summary.id) `shouldReturn` key'.summary.id
      (.active) <$> runB (ApiH.apiKeyDeactivate testPid key'.summary.id) `shouldReturn` False

      me <- runB $ ApiH.apiMe testPid
      (me.projectId, me.project.id) `shouldBe` (testPid, testPid)
      let emptyPatch = Projects.ProjectPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      (.summary.title) <$> runB (ApiH.apiProjectPatch testPid emptyPatch{Projects.title = Just "Plan B Patched"}) `shouldReturn` "Plan B Patched"
      (.summary.title) <$> runB (ApiH.apiProjectGet testPid) `shouldReturn` "Plan B Patched"

    it "tenant scoping: another project's resources are 404 by id and absent from listings" \tr -> do
      let runB :: ATBaseCtx a -> IO a
          runB = toBaseServantResponse tr
          denied :: IO () -> IO ()
          denied = (`shouldThrow` anyException)
      otherPid <- createTestProject tr "Other API project"
      monitor <- runB $ ApiH.apiMonitorCreate otherPid (def :: ApiT.MonitorInput){ApiT.title = "foreign", ApiT.query = "count(*)", ApiT.alertThreshold = 1, ApiT.checkIntervalMins = 5, ApiT.timeWindowMins = 15}
      dash <- runB $ ApiH.apiDashboardCreate ApiT.ApiKeyPrincipal otherPid ApiT.DashboardInput{ApiT.title = "foreign", ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
      foreignTeam <- runB $ ApiH.apiTeamCreate otherPid (team "foreign" "foreign")
      foreignKey <- runB $ ApiH.apiKeyCreate otherPid ApiT.ApiKeyCreate{ApiT.title = "foreign"}
      [ownId, foreignId] <- replicateM 2 (UUIDId <$> UUIDV4.nextRandom)
      sourceId <- UUIDV4.nextRandom
      runB
        $ Hasql.interpExecute_
          [HI.sql|INSERT INTO apis.incident_episodes (id, project_id, source_kind, source_id, phase, started_at, last_event_at)
            VALUES (#{ownId}, #{testPid}, 'issue', #{sourceId}, 'active', #{frozenTime}, #{frozenTime}),
                   (#{foreignId}, #{otherPid}, 'issue', #{sourceId}, 'active', #{frozenTime}, #{frozenTime})|]
      map (.id) <$> runB (ApiH.apiIncidentsList testPid Nothing Nothing) `shouldReturn` [ownId]
      (.id) <$> runB (ApiH.apiIncidentGet testPid ownId) `shouldReturn` ownId
      sequence_
        [ denied $ void $ runB (ApiH.apiIncidentGet testPid foreignId)
        , denied $ void $ runB (ApiH.apiMonitorGet testPid monitor.id)
        , denied $ void $ runB (ApiH.apiDashboardGet testPid dash.summary.id)
        , denied $ void $ runB (ApiH.apiTeamGet testPid foreignTeam.summary.id)
        , denied $ void $ runB (ApiH.apiKeyGet testPid foreignKey.summary.id)
        ]

    it "events, metrics and listings: envelopes, malformed KQL, opt-in attributes and with_children descendants" \tr -> do
      let runB :: ATBaseCtx a -> IO a
          runB = toBaseServantResponse tr
      events <- AE.toJSON <$> runB (Log.queryEvents testPid (Just "") (Just "1h") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
      [k | (k, present) <- [("logsData", isJust (events ^? key "logsData" . _Array)), ("cols", isJust (events ^? key "cols" . _Array)), ("colIdxMap", isJust (events ^? key "colIdxMap" . _Object)), ("count", isJust (events ^? key "count" . _Number)), ("traces", isJust (events ^? key "traces" . _Array)), ("hasMore", isJust (events ^? key "hasMore"))], not present] `shouldBe` ([] :: [Text])
      (runB (Log.queryEvents testPid (Just "|| invalid {{") (Just "1h") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) >>= evaluateWHNF_) `shouldThrow` anyException
      metrics <- runB $ Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just testPid) (Just "summarize count(*) by bin_auto(timestamp)") Nothing (Just "1h") Nothing Nothing (Just "spans") Nothing []
      metrics.rowsCount `shouldSatisfy` (>= 0)
      AE.eitherDecode @MetricsData (AE.encode metrics) `shouldSatisfy` isRight
      let body = (def :: ApiT.EventsQuery){ApiT.query = Just "", ApiT.since = Just "1h"}
      bodyJson <- AE.toJSON <$> runB (ApiH.apiEventsQuery Log.queryEvents testPid body)
      (isJust (bodyJson ^? key "logsData" . _Array), isJust (bodyJson ^? key "count" . _Number)) `shouldBe` (True, True)
      (.cols) <$> runB (ApiH.apiEventsQuery Log.queryEvents testPid (body :: ApiT.EventsQuery){ApiT.includeAttributes = Just True}) >>= (`shouldContain` ["attributes"])

      -- Search used to return every span in a matching trace; now exact match, and
      -- with_children adds only the descendants of each match (C under B under A, not D under E).
      apiKey <- createTestAPIKey tr testPid "with-children-key"
      trId <- show <$> UUIDV4.nextRandom
      [spanA, spanB, spanC, spanE, spanD] <- replicateM 5 (show <$> UUIDV4.nextRandom)
      marker <- ("wc-" <>) . UUID.toText <$> UUIDV4.nextRandom
      let ingest sid pidM name extras = void $ OtlpServer.traceServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider $ Proto (mkSpanRequest trId sid pidM name [] Nothing extras (mkResource apiKey []) (addUTCTime (-30) frozenTime))
      ingest spanA Nothing "tree.root" [mkAttr "wc.marker" marker]
      ingest spanB (Just spanA) "tree.child" []
      ingest spanC (Just spanB) "tree.grandchild" []
      ingest spanE Nothing "tree.unrelated.root" []
      ingest spanD (Just spanE) "tree.unrelated.child" []
      let tree = (body :: ApiT.EventsQuery){ApiT.query = Just $ "attributes.wc.marker == \"" <> marker <> "\""}
      V.length . (.logsData) <$> runB (ApiH.apiEventsQuery Log.queryEvents testPid tree) `shouldReturn` 1
      V.length . (.logsData) <$> runB (ApiH.apiEventsQuery Log.queryEvents testPid (tree :: ApiT.EventsQuery){ApiT.withChildren = Just True}) `shouldReturn` 3

      issues <- runB $ ApiH.apiIssuesList testPid Nothing Nothing Nothing Nothing Nothing
      (issues.perPage > 0, issues.totalCount >= 0) `shouldBe` (True, True)
      (.succeeded) <$> runB (ApiH.apiIssuesBulk ApiT.ApiKeyPrincipal testPid ApiT.BulkAction{ApiT.action = "acknowledge", ApiT.ids = [], ApiT.durationMinutes = Nothing}) `shouldReturn` []
      (.perPage) <$> runB (ApiH.apiLogPatternsList testPid Nothing Nothing) >>= (`shouldSatisfy` (> 0))
      (.succeeded) <$> runB (ApiH.apiLogPatternsBulk ApiT.ApiKeyPrincipal testPid ApiT.BulkAction{ApiT.action = "acknowledge", ApiT.ids = [], ApiT.durationMinutes = Nothing}) `shouldReturn` []
      runQueryEffect tr $ Endpoints.bulkInsertEndpoints $ fromList [(def :: Endpoints.Endpoint){Endpoints.projectId = testPid, Endpoints.urlPath = "/api-v1-probe", Endpoints.urlParams = AE.object [], Endpoints.method = "GET", Endpoints.host = "probe.test", Endpoints.hash = "api-v1-probe", Endpoints.serviceName = Just "probe"}]
      endpoints <- runB $ ApiH.apiEndpointsList testPid (Just "api-v1-probe") Nothing Nothing Nothing
      map (\e -> (e.urlPath, e.host, e.serviceName)) endpoints.items `shouldBe` [("/api-v1-probe", "probe.test", Just "probe")]
      ep <- maybe (fail "probe endpoint not listed") pure (viaNonEmpty head endpoints.items)
      (.summary.hash) <$> runB (ApiH.apiEndpointGet testPid ep.id) `shouldReturn` "api-v1-probe"

    it "MCP JSON-RPC: initialize, tools/list, tool results as isError not RPC errors, POST bodies forwarded, notifications null" \tr -> do
      let reg = MCP.allTools apiV1OpenApiSpec (\pid tz input -> pure $ Right $ AE.object ["query" AE..= input, "project_id" AE..= pid, "timezone" AE..= tz])
          app = genericServeTWithContext (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider) (apiV1Server (Authenticated testPid)) Servant.EmptyContext
          via a = toBaseServantResponse tr . MCP.handleJsonRpc reg a testPid
          -- Protocol and composite-toolCall paths must never reach the REST app.
          send = via (error "REST app invoked unexpectedly")
          isError r = r ^? key "result" . key "isError"
      initialized <- send (rpc "initialize" [])
      (isJust (initialized ^? key "result" . key "protocolVersion"), isJust (initialized ^? key "result" . key "serverInfo" . key "name")) `shouldBe` (True, True)
      tools <- (^.. key "result" . key "tools" . _Array . traverse) <$> send (rpc "tools/list" [])
      case tools of
        t : _ -> [k | k <- ["name", "description", "inputSchema"], isNothing (t ^? key k)] `shouldBe` []
        [] -> expectationFailure "expected at least one tool"
      -- Composite workflow tools sit alongside the REST tools.
      filter (`notElem` (tools ^.. traverse . key "name" . _String)) ["find_error_patterns", "search_events_nl", "analyze_issue", "get_schema"] `shouldBe` []
      schema <- via app (toolCall "get_schema" [])
      isError schema `shouldBe` Just (AE.Bool False)
      liveSchemaFields (schema ^? key "result" . key "structuredContent") `shouldBe` Right (Map.keys (Schema.deriveSchema ParserExpr.flattenedOtelAttributes).fields)
      patterns <- send (toolCall "find_error_patterns" ["limit" AE..= (5 :: Int)])
      (isError patterns, isJust (patterns ^? key "result" . key "structuredContent" . key "patterns" . _Array), isJust (patterns ^? key "result" . key "structuredContent" . key "as_of")) `shouldBe` (Just (AE.Bool False), True, True)
      failures <- traverse send [toolCall "does_not_exist" [], toolCall "search_events_nl" ["input" AE..= ("" :: Text)], toolCall "analyze_issue" ["issue_id" AE..= ("not-a-uuid" :: Text)]]
      map (\r -> (isError r, r ^? key "error")) failures `shouldBe` replicate 3 (Just (AE.Bool True), Nothing)
      send (rpc "notifications/initialized" []) `shouldReturn` AE.Null
      teamHandle <- ("body-test-" <>) . UUID.toText <$> UUIDV4.nextRandom
      created <- via app (toolCall "create_team" ["body" AE..= AE.object ["name" AE..= teamHandle, "handle" AE..= teamHandle]])
      (isError created, created ^? key "result" . key "structuredContent" . key "summary" . key "handle" . _String) `shouldBe` (Just (AE.Bool False), Just teamHandle)

    let specJson = AE.toJSON apiV1OpenApiSpec
    let reg = MCP.allTools apiV1OpenApiSpec (\pid tz input -> pure $ Right $ AE.object ["query" AE..= input, "project_id" AE..= pid, "timezone" AE..= tz])
        dummyApp = error "dummyApp invoked unexpectedly"
        buildTestApp tr pid =
          genericServeTWithContext
            (effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider)
            (apiV1Server (Authenticated pid))
            Servant.EmptyContext
        rpcCall body =
          AE.object
            [ "jsonrpc" AE..= ("2.0" :: Text)
            , "id" AE..= (1 :: Int)
            , "method" AE..= ("tools/call" :: Text)
            , "params" AE..= body
            ]
        rpcCallNamed name args =
          rpcCall (AE.object ["name" AE..= (name :: Text), "arguments" AE..= args])



    it "returns an empty dashboard's title and widget list" $ \tr -> do
      created <- toBaseServantResponse tr $ ApiH.apiDashboardCreate ApiT.ApiKeyPrincipal testPid ApiT.DashboardInput{ApiT.title = "empty dashboard", ApiT.tags = Nothing, ApiT.teams = Nothing, ApiT.filePath = Nothing, ApiT.schema = Nothing}
      rendered <- toBaseServantResponse tr $ DashPage.apiDashboardData testPid created.summary.id Nothing Nothing (Just "1h") Nothing Nothing []
      rendered.id `shouldBe` created.summary.id
      rendered.title `shouldBe` "empty dashboard"
      rendered.widgets `shouldSatisfy` null

    it "runs the Agent project adapter through the same MCP route" \tr -> do
      let backend = Routes.mcpProjectTools
      result <- toBaseServantResponse tr $ AI.invoke backend ApiT.ApiKeyPrincipal testPid "get_project" Map.empty
      (result ^? key "isError") `shouldBe` Just (AE.Bool False)
      (result ^? key "structuredContent" . key "summary" . key "id" . _String) `shouldBe` Just testPid.toText
      direct <- toBaseServantResponse tr $ ApiH.apiProjectGet testPid
      result ^? key "structuredContent" `shouldBe` Just (AE.toJSON direct)

    it "catalog covers every published operation once and shares the CLI resource routes" $ \_tr -> do
      let documented =
            S.fromList
              [ (H.renderStdMethod method, AEK.toText path)
              | Just (AE.Object paths) <- [specJson ^? key "paths"]
              , (path, AE.Object methods) <- AEKM.toList paths
              , (verb, _) <- AEKM.toList methods
              , Right method <- [H.parseMethod $ encodeUtf8 $ T.toUpper $ AEK.toText verb]
              , (H.renderStdMethod method, AEK.toText path) /= (H.methodPost, "/mcp")
              ]
          exposed = S.fromList [(binding.method, binding.path) | tool <- Map.elems reg, MCP.ViaOpenApi binding <- [tool.dispatch]]
          names = map (fst . snd) (Map.toList Catalog.apiOperations) <> map (.name) Catalog.compositeTools <> map (.name) (Map.elems Catalog.localTools)
      exposed `shouldBe` documented
      Map.keysSet Catalog.apiOperations `shouldBe` documented
      length names `shouldBe` S.size (S.fromList names)
      Map.size Catalog.localTools `shouldBe` length [minBound .. maxBound :: Catalog.LocalOperation]
      for_ [Resource.Monitors, Resource.Dashboards, Resource.ApiKeys, Resource.Teams, Resource.Members, Resource.Issues, Resource.Incidents, Resource.Endpoints, Resource.LogPatterns] \kind ->
        ((H.methodGet,) <$> T.stripPrefix "/api/v1" (Resource.resourcePath kind)) `shouldSatisfy` maybe False (`S.member` documented)

    it "requires an explicit Agent policy or exclusion for every MCP operation" $ \_tr -> do
      let policies = Map.mapMaybe (.agentPolicy) reg
          covered = Map.keysSet policies `S.union` Map.keysSet MCP.agentExclusions
      Map.keysSet reg `shouldBe` covered
      Map.keysSet policies `S.intersection` Map.keysSet MCP.agentExclusions `shouldBe` S.empty
      Map.lookup "list_metrics" policies `shouldBe` Just (MCP.ReadOnly PM.PView)
      Map.lookup "search_events" policies `shouldBe` Just (MCP.ReadOnly PM.PView)
      Map.lookup "create_monitor" policies `shouldBe` Just (MCP.ProjectWrite)
      for_ (["bulk_monitors", "remove_member", "delete_dashboard"] :: [Text]) \name -> Map.lookup name policies `shouldBe` Just (MCP.Administration PM.PView)
      for_ (["create_api_key", "activate_api_key", "deactivate_api_key", "delete_api_key"] :: [Text]) \name -> Map.lookup name policies `shouldBe` Just (MCP.Administration PM.PAdmin)

    it "toolsList_bodySchemaIncludesReferencedApiFields" $ \tr -> do
      let req = AE.object ["jsonrpc" AE..= ("2.0" :: Text), "id" AE..= (1 :: Int), "method" AE..= ("tools/list" :: Text)]
      resp <- toBaseServantResponse tr (MCP.handleJsonRpc reg dummyApp testPid req)
      let tools = resp ^.. key "result" . key "tools" . _Array . traverse
          body name = do
            input <- find (\tool -> (tool ^? key "name" . _String) == Just name) tools >>= (^? key "inputSchema")
            schema <- input ^? key "properties" . key "body"
            pure $ fromMaybe schema do
              reference <- schema ^? key "$ref" . _String
              definition <- T.stripPrefix "#/$defs/" reference
              input ^? key "$defs" . key (AEK.fromText definition)
      for_ ([("create_monitor", ["title", "query", "alert_threshold"]), ("search_events", ["query", "since"])] :: [(Text, [Text])]) $ \(name, fields) -> do
        schema <- maybe (fail $ "Missing body schema: " <> toString name) pure $ body name
        for_ fields $ \field -> schema ^? key "properties" . key (AEK.fromText field) `shouldSatisfy` isJust
      for_ (tools ^.. traverse . key "inputSchema") $ \schema ->
        for_ (schema ^.. cosmos . key "$ref" . _String) $ \reference -> do
          definition <- maybe (fail $ "Unbound schema reference: " <> toString reference) pure $ T.stripPrefix "#/$defs/" reference
          schema ^? key "$defs" . key (AEK.fromText definition) `shouldSatisfy` isJust

    it "tools/list derives read hints from operation policy, including POST queries and privileged reads" $ \tr -> do
      let req = AE.object ["jsonrpc" AE..= ("2.0" :: Text), "id" AE..= (1 :: Int), "method" AE..= ("tools/list" :: Text)]
      resp <- toBaseServantResponse tr (MCP.handleJsonRpc reg dummyApp testPid req)
      let tools = resp ^.. key "result" . key "tools" . _Array . traverse
          hint name = find (\tool -> (tool ^? key "name" . _String) == Just name) tools >>= (^? key "annotations" . key "readOnlyHint")
      for_ ([("search_events", True), ("get_api_key", True), ("analyze_issue", True), ("create_monitor", False), ("delete_dashboard", False)] :: [(Text, Bool)]) $ \(name, readOnly) ->
        hint name `shouldBe` Just (AE.Bool readOnly)
      hint "submit_rrweb_event" `shouldBe` Nothing

    it "tools/call list_metrics returns a project-scoped catalog page" $ \tr -> do
      runQueryEffect tr
        $ Hasql.interpExecute_
          [HI.sql|INSERT INTO otel_metrics_meta
          (project_id, metric_name, metric_type, metric_unit, metric_description, service_name, scope_name, metric_labels, first_seen_at, last_seen_at, first_timestamp, last_timestamp)
          VALUES (#{testPid}, 'checkout.duration', 'GAUGE', 'ms', 'request metric', 'checkout', 'test', '{}', #{frozenTime}, #{frozenTime}, #{frozenTime}, #{frozenTime})|]
      let oldTime = addUTCTime (-(8 * 24 * 3600)) frozenTime
      runQueryEffect tr
        $ Hasql.interpExecute_
          [HI.sql|INSERT INTO otel_metrics_meta
          (project_id, metric_name, metric_type, metric_unit, metric_description, service_name, scope_name, metric_labels, first_seen_at, last_seen_at, first_timestamp, last_timestamp)
          VALUES (#{testPid}, 'checkout.old', 'GAUGE', 'ms', 'old metric', 'checkout', 'test', '{}', #{oldTime}, #{oldTime}, #{oldTime}, #{oldTime})|]
      resp <-
        toBaseServantResponse tr
          $ MCP.handleJsonRpc
            reg
            (buildTestApp tr testPid)
            testPid
            (rpcCallNamed "list_metrics" $ AE.object ["service" AE..= ("checkout" :: Text), "limit" AE..= (1 :: Int)])
      (resp ^? key "result" . key "isError") `shouldBe` Just (AE.Bool False)
      (resp ^? key "result" . key "structuredContent" . key "metrics" . _Array . ix 0 . key "metricName" . _String) `shouldBe` Just "checkout.duration"
      inactive <-
        toBaseServantResponse tr
          $ MCP.handleJsonRpc
            reg
            (buildTestApp tr testPid)
            testPid
            (rpcCallNamed "list_metrics" $ AE.object ["service" AE..= ("checkout" :: Text), "active" AE..= False])
      (inactive ^? key "result" . key "structuredContent" . key "active") `shouldBe` Just (AE.Bool False)
      (inactive ^? key "result" . key "structuredContent" . key "metrics" . _Array . ix 0 . key "metricName" . _String) `shouldBe` Just "checkout.old"

    it "tools/call search_events_nl passes project and timezone to the registered translator" $ \tr -> do
      let req = rpcCallNamed "search_events_nl" $ AE.object ["input" AE..= ("checkout errors" :: Text), "timezone" AE..= ("Europe/Berlin" :: Text)]
      resp <- toBaseServantResponse tr (MCP.handleJsonRpc reg dummyApp testPid req)
      (resp ^? key "result" . key "structuredContent" . key "project_id") `shouldBe` Just (AE.toJSON testPid)
      (resp ^? key "result" . key "structuredContent" . key "query") `shouldBe` Just (AE.String "checkout errors")
      (resp ^? key "result" . key "structuredContent" . key "timezone") `shouldBe` Just (AE.String "Europe/Berlin")
