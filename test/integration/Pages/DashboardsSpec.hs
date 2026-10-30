module Pages.DashboardsSpec (spec) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Concurrent (runConcurrent)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.ProjectMembers (TeamVM (..))
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.BodyWrapper qualified as BodyWrapper
import Pages.Charts.Types (MetricsData (..), MetricsStats (..))
import Pages.Dashboards qualified as Dashboards
import Pages.Projects (TeamForm (..))
import Pages.Projects qualified as ManageMembers
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), mkHasqlPool)
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Pkg.TestUtils
import Relude
import Servant qualified
import System.Config (AuthContext (..), DeploymentEnv (Prod, Staging), EnvConfig (..))
import System.Directory (withCurrentDirectory)
import System.Environment qualified as Env
import System.IO.Temp (withSystemTempDirectory)
import System.Types (addRespHeaders)
import Test.Hspec
import Web.Routes qualified as Routes
import "cryptonite" Crypto.Hash qualified as Crypto


filters :: Dashboards.DashboardFilters
filters = Dashboards.DashboardFilters []


spec :: Spec
-- Later tests reuse dashboards created by earlier ones (e.g. "bulk add teams to
-- dashboards"), so this keeps aroundAll and runs sequentially — opting out of the
-- suite's per-test isolation + parallelism (same as GitSyncSpec).
spec = sequential $ aroundAll withTestResources do
  describe "Dashboards Tests" do
    let mkDashboard t = Dashboards.DashboardForm{Dashboards.title = t, Dashboards.file = "overview.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
        postDashboards ts t = traverse_ (Dashboards.dashboardsPostH testPid . mkDashboard) ts >> Dashboards.dashboardsPostH testPid (mkDashboard t)
        mkVar k m v = DashboardModel.Variable{key = k, title = Nothing, multi = m, required = Nothing, reloadOnChange = Nothing, helpText = Nothing, _vType = DashboardModel.VTValues, sql = Nothing, facetField = Nothing, query = Nothing, options = Nothing, value = v, dependsOn = Nothing}
        renderDash t dash = renderT $ Dashboards.DashboardGet testPid (UUIDId UUID.nil) dash (DashboardModel.mkDashboardVM (UUIDId UUID.nil) testPid frozenTime (Projects.UserId UUID.nil)){DashboardModel.schema = Just dash, DashboardModel.title = t} []
        listDashboards tr = do
          (_, pg) <- testServant tr (Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters)
          PageCtx _ d <- asDashboardsGet pg
          pure d.dashboards
        byTitle :: Text -> V.Vector DashboardModel.DashboardVM -> IO DashboardModel.DashboardVM
        byTitle t = maybe (fail $ "no dashboard titled " <> toString t) pure . V.find ((== t) . (.title))
        asDashboardsGet = \case
          Dashboards.DashboardsGet pc -> pure pc
          _ -> fail "Expected full dashboard list response"
        expectNoContent = \case
          Dashboards.DashboardNoContent -> pass
          _ -> fail "Expected DashboardNoContent response"

    -- Regression: the variable input emitted data-tagify-mode twice for multi
    -- variables; Lucid merges duplicate attrs with (<>), so multi-select vars
    -- silently rendered as single-select ("" <> "select"). Multi vars must carry
    -- NO tagify-mode attr (main.ts only sets options.mode when it is present).
    it "dashboardVariables_renderCorrectlyAboveWidgetLayers" \_ -> do
      let chart = (def :: Widget.Widget){Widget.wType = Widget.WTTimeseriesLine}
          html = renderDash "t" (def :: DashboardModel.Dashboard){DashboardModel.variables = Just [mkVar "single" Nothing Nothing, mkVar "multi" (Just True) Nothing], DashboardModel.widgets = [chart]}
      T.count "data-tagify-mode" html `shouldBe` 1
      html `shouldContainAll` ["data-tagify-mode=\"select\"", "sticky top-0 z-10", "dashboard-grid-wrapper relative isolate z-0"]

    it "table widget loading shell defers headings until data arrives" \_ -> do
      let column field title = (def :: Widget.TableColumn){Widget.field = field, Widget.title = title}
          widget =
            (def :: Widget.Widget)
              { Widget.wType = Widget.WTTable
              , Widget.title = Just "Status Code Breakdown"
              , Widget.columns = Just [column "status_code" "Status Code", column "count" "Count", column "avg_latency" "Avg Latency"]
              }
          html = renderT $ Widget.widget_ widget
      html `shouldSatisfy` (not . T.isInfixOf "<thead")

    -- The canonical tab URL stays a normal full-document URL, but its htmx response is
    -- supplied by the shared BodyWrapper navigation contract.  There is no synthetic
    -- /content route or imperative active-tab mutation.
    it "dashboardTabs_useTheSharedNavigationContract_notAContentEndpoint" \_ -> do
      let mkTab n = (def :: DashboardModel.Tab){DashboardModel.name = n}
          html = renderDash "t" (def :: DashboardModel.Dashboard){DashboardModel.tabs = Just [mkTab "Overview", mkTab "Errors"]}
      -- `morph`/`:morph` are htmx 2 + idiomorph names; htmx 4 calls them outerMorph/innerMorph
      -- and throws "Unknown swap style: morph" at swap time, so the names are asserted here.
      html `shouldContainAll` ["hx-target=\"#dashboard-tabs-content\"", "hx-swap=\"outerMorph\"", "hx-push-url=\"true\""]
      html `shouldSatisfy` (not . T.isInfixOf "hx-select=\"")
      -- No /content partial, and no hyperscript managing the active class.
      html `shouldSatisfy` (not . T.isInfixOf "/content")
      html `shouldSatisfy` (not . T.isInfixOf "remove .tab-active")

    it "reports a bounded dashboard-settlement sample after initial and tab renders" \_ -> do
      renderDash "Performance sample" (def :: DashboardModel.Dashboard){DashboardModel.tabs = Just [((def :: DashboardModel.Tab){DashboardModel.name = "Overview"} :: DashboardModel.Tab)]}
        `shouldContainAll` ["/dashboards/00000000-0000-0000-0000-000000000000/settled", "duration_ms", "response_bytes", "htmx:before:request", "htmx:after:settle", "keepalive: true"]

    it "dashboardSettledPostH_nonMember_cannotProbeDashboards" \tr -> do
      victim <- createTestProject tr "victim"
      _ <- testServant tr $ Dashboards.dashboardsPostH victim (mkDashboard "Victim")
      dash <- runTestBg frozenTime tr (DashboardModel.selectDashboardsSortedBy victim DashboardModel.Asc DashboardModel.DSTitle) >>= maybe (fail "no dashboard") pure . viaNonEmpty head
      assertNonMemberDenied tr victim $ Dashboards.dashboardSettledPostH victim dash.id (Dashboards.DashboardSettleSample 1 2 Dashboards.DashboardInitial)

    it "accepts only the closed dashboard navigation vocabulary" \_ -> do
      let sample navigation =
            AE.eitherDecode @Dashboards.DashboardSettleSample
              $ "{\"duration_ms\":1,\"response_bytes\":2,\"navigation\":\""
              <> navigation
              <> "\"}"
      sample "initial" `shouldBe` Right (Dashboards.DashboardSettleSample 1 2 Dashboards.DashboardInitial)
      sample "tab" `shouldBe` Right (Dashboards.DashboardSettleSample 1 2 Dashboards.DashboardTab)
      sample "typo" `shouldSatisfy` isLeft

    -- Dashboard variables are backend-persisted schema, but their live values come from
    -- the URL. The precedence rule is what makes a dashboard shareable: a link carries the
    -- reader's selections, and clearing one must stay cleared rather than snapping back to
    -- the dashboard's default on the next load.
    it "URL variable values win over the dashboard's declared defaults" \_ -> do
      let vars = Just [mkVar "env" Nothing (Just "prod"), mkVar "region" Nothing (Just "eu")]
          resolve params = Dashboards.addVariableDefaults params vars

      -- Nothing supplied: both defaults apply.
      sortOn fst (resolve []) `shouldBe` [("var-env", Just "prod"), ("var-region", Just "eu")]

      -- A supplied value wins, and the untouched variable still gets its default.
      sortOn fst (resolve [("var-env", Just "staging")]) `shouldBe` [("var-env", Just "staging"), ("var-region", Just "eu")]

      -- Explicitly cleared counts as supplied: the default must not come back.
      sortOn fst (resolve [("var-env", Nothing)]) `shouldBe` [("var-env", Nothing), ("var-region", Just "eu")]

      -- A dashboard with no variables leaves the request's params untouched.
      Dashboards.addVariableDefaults [("since", Just "1H")] Nothing `shouldBe` [("since", Just "1H")]

    it "overview template uses the native metric store" \_ -> do
      overview <- DashboardModel.readDashboardFile "static/public/dashboards" "_overview.yaml"
      overview `shouldSatisfy` isJust
      show overview `shouldNotContain` "telemetry.metrics"
      show overview `shouldNotContain` "metric_value"

    -- Regression: the overview's Infra stats selected `…::text || '%'`, and the stat path
    -- decodes a number, so every one rendered "Query execution failed".
    it "every template's SQL stat widget decodes as a number" \tr -> do
      stats <- filter (\w -> w.wType == Widget.WTStat && isJust w.sql) . foldMap DashboardModel.allWidgets <$> DashboardModel.readDashboardsFromDisk "static/public/dashboards"
      length stats `shouldSatisfy` (> 3)
      for_ stats \w -> do
        md <- runQueryEffect tr $ runConcurrent $ Dashboards.widgetMetrics testPid (Just "24h", Nothing, Nothing) [("const-endpointFilter", Just "hashes @> ARRAY['']")] w
        (w.title, md.error :: Maybe Text) `shouldBe` (w.title, Nothing)

    it "dashboard SQL source migrations remain byte-for-byte immutable" \_ -> do
      migration0119 <- readFileBS "static/migrations/0119_endpoint_dashboard_sql_source.sql"
      migration0120 <- readFileBS "static/migrations/0120_repair_dashboard_sql_source.sql"
      show (Crypto.hash migration0119 :: Crypto.Digest Crypto.MD5) `shouldBe` ("27fc8228a167e579b4b7124893921372" :: Text)
      show (Crypto.hash migration0120 :: Crypto.Digest Crypto.MD5) `shouldBe` ("19ac1c6651e59fb3860366eec198201f" :: Text)

    it "endpoint dashboard SQL source, not query text, selects Postgres" \tr -> do
      endpointDashboard <- DashboardModel.readDashboardFile "static/public/dashboards" "endpoint-stats.yaml"
      map (fmap (.source) . (.sql)) (fold $ endpointDashboard >>= (.variables)) `shouldBe` [Just LogQueries.SqlPostgres, Just LogQueries.SqlPostgres]
      noApisPool <- mkHasqlPool 1 (tr.trConnStr <> " dbname=postgres")
      let tr' = tr{trATCtx = tr.trATCtx{hasqlTimefusionPool = noApisPool}}
          query = "SELECT COUNT(*)::bigint FROM apis.endpoints WHERE project_id='" <> testPid.toText <> "' AND 'otel_logs_and_spans' = 'otel_logs_and_spans'"
      result <- runQueryEffect tr' $ LogQueries.executeSecuredQuery True testPid (LogQueries.SecuredSql LogQueries.SqlPostgres query) 1
      result `shouldSatisfy` isRight

    -- Regression: every widget went through the chart query shaper, which appended
    -- `| summarize count(*) by bin_auto(timestamp)` to a stat's scalar query. The
    -- resulting three-column series hit the one-slot DTFloat decoder
    -- ("mismatch between number of columns to convert and number in target type")
    -- and every stat on the overview dashboard rendered an error overlay.
    it "filters ingested data by time and URL parameters, reports a bad query, and recovers" \tr -> do
      apiKey <- createTestAPIKey tr testPid "stat-widget-key"
      ingestTrace tr apiKey "GET /api/stat-widget" frozenTime
      ingestTrace tr apiKey "GET /api/other-widget" frozenTime
      ingestTrace tr apiKey "GET /api/old-widget" (addUTCTime (-48 * 60 * 60) frozenTime)
      let widget wt q = (def :: Widget.Widget){Widget.wType = wt, Widget.query = Just q}
          runWith timeRange params wt q = runQueryEffect tr $ runConcurrent $ Dashboards.widgetMetrics testPid timeRange params (widget wt q)
          run = runWith (Just "24h", Nothing, Nothing) []

      logs <- run Widget.WTLogs "name == \"GET /api/stat-widget\""
      table <- run Widget.WTTable "name != null | summarize count(*) by service"
      series <- run Widget.WTTimeseries "name != null"
      for_ [logs, table, series] \md -> (md.error :: Maybe Text) `shouldBe` Nothing
      V.length logs.dataText `shouldSatisfy` (> 0)
      V.length table.dataText `shouldSatisfy` (> 0)
      V.length series.dataset `shouldSatisfy` (> 0)

      -- a scalar summarize, and a filter-only stat that has to grow one
      for_ ["name != null | summarize dcount(name)", "name != null"] \q -> do
        md <- run Widget.WTStat q
        (md.error :: Maybe Text) `shouldBe` Nothing
        (md.dataFloat :: Maybe Double) `shouldSatisfy` (> Just 0)

      -- Dashboard URL parameters are interpolated into the real query, while the
      -- selected range still excludes the otherwise matching 48-hour-old trace.
      filtered <- runWith (Just "24h", Nothing, Nothing) [("var-operation", Just "GET /api/stat-widget")] Widget.WTLogs "name == \"{{var-operation}}\""
      (filtered.error :: Maybe Text) `shouldBe` Nothing
      V.length filtered.dataText `shouldBe` 1

      -- A malformed edit becomes widget-local error data. Correcting the query on the
      -- next request clears it and returns data, which is the browser's retry contract.
      broken <- run Widget.WTTimeseries "name =="
      (broken.error :: Maybe Text) `shouldSatisfy` isJust
      recovered <- run Widget.WTTimeseries "name == \"GET /api/stat-widget\""
      (recovered.error :: Maybe Text) `shouldBe` Nothing
      V.length recovered.dataset `shouldSatisfy` (> 0)

    -- Dashboard variables reached widget SQL verbatim, so a quote in a URL value ended the
    -- string literal and the rest ran as SQL; and request params were merged after the
    -- server presets, so `project_id` itself could be re-pointed at another tenant.
    it "dashboardSql_escapesVariables_andKeepsServerPresets" \tr -> do
      let otherPid = "11111111-1111-1111-1111-111111111111"
          widget = (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.sql = Just "SELECT v, project_id FROM (SELECT '{{var-service}}'::text AS v, '{{project_id}}'::text AS project_id) t WHERE project_id='{{project_id}}'"}
      md <- runQueryEffect tr $ runConcurrent $ Dashboards.widgetMetrics testPid (Just "24h", Nothing, Nothing) [("var-service", Just "a' OR 'x'='x"), ("project_id", Just otherPid)] widget
      (md.error :: Maybe Text) `shouldBe` Nothing
      md.dataText `shouldBe` [["a' OR 'x'='x", testPid.toText]]

    it "does not cache query failures as successful empty PNGs" \tr -> do
      let render query appearance = do
            let widget = (def :: Widget.Widget){Widget.query = Just query, Widget.pngProfile = Just Widget.PngSlack}
                encoded = decodeUtf8 @Text $ LBS.toStrict $ AE.encode widget
                signature = Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded
            toBaseServantResponse tr $ Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing (Just "2025-06-01T11:45:00Z") (Just "2025-06-01T12:00:00Z") Nothing Nothing (Just signature) [("appearance", Just appearance)]
      failed <- render "name ==" "light"
      failedDark <- render "name ==" "dark"
      emptyResponse <- render "name == \"no-such-png-fixture-operation\"" "light"
      for_ [failed, failedDark, emptyResponse] \response ->
        LBS.take 8 (Servant.getResponse response) `shouldBe` "\137PNG\r\n\26\n"
      List.lookup "Cache-Control" (Servant.getHeaders failed) `shouldBe` Just "no-store"
      List.lookup "Cache-Control" (Servant.getHeaders emptyResponse) `shouldBe` Just "public, max-age=31536000, immutable"
      Servant.getResponse failed `shouldNotBe` Servant.getResponse emptyResponse
      captureDir <- Env.lookupEnv "SLACK_PNG_CAPTURE_DIR"
      for_ captureDir \dir -> for_ [("failure", failed), ("failure-dark", failedDark), ("empty", emptyResponse)] \(name, response) ->
        LBS.writeFile (dir <> "/" <> name <> ".png") $ Servant.getResponse response

    it "renders a grouped status chart PNG with missing series values" \tr -> for_ [Nothing, Just (def :: MetricsStats){maxGroupSum = 202}] \stats -> do
      let dataset =
            (def :: Widget.WidgetDataset)
              { Widget.source =
                  AE.toJSON
                    ( [ [AE.String "timestamp", AE.String "ERROR", AE.String "OK", AE.String "UNSET", AE.String "null"]
                      , [AE.Number 1788451200000, AE.Null, AE.Null, AE.Number 187, AE.Number 5]
                      , [AE.Number 1788451320000, AE.Number 1, AE.Null, AE.Number 201, AE.Null]
                      ]
                        :: [[AE.Value]]
                    )
              , Widget.from = Just 1788451200000
              , Widget.to = Just 1788465600000
              , Widget.stats = stats
              }
          widget =
            (def :: Widget.Widget)
              { Widget.id = Just "log-explorer-all-traces"
              , Widget.wType = Widget.WTTimeseries
              , Widget.query = Just "summarize count(*) by bin_auto(timestamp), status_code"
              , Widget.dataset = Just dataset
              , Widget.yAxis = Just (def :: Widget.WidgetAxis){Widget.showOnlyMaxLabel = Just True}
              , Widget.unit = Just "rows"
              , Widget.legendPosition = Just "top-right"
              , Widget.legendSize = Just Widget.LSXs
              , Widget.standalone = Just True
              , Widget.allowZoom = Just True
              }
          encoded = decodeUtf8 @Text $ LBS.toStrict $ AE.encode widget
          signature = Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded
      response <- toBaseServantResponse tr $ Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just signature) []
      LBS.take 8 (Servant.getResponse response) `shouldBe` "\137PNG\r\n\26\n"

    it "Should create a dashboard" \tr -> do
      testServant tr (postDashboards ["Dashboard 1", "Dashboard 2"] "Test Dashboard") >>= expectNoContent . snd

    it "Should not create a dashboard with an empty title" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsPostH testPid (mkDashboard "")
      case pg of
        Dashboards.DashboardPostError message -> message `shouldBe` "Dashboard title is required"
        _ -> fail "Expected DashboardPostError response"

    -- The empty-state CTA passed a modal id where a URL was expected, and the
    -- ZeroState type collapsed its Either with `either id id`, so it rendered as
    -- href="newDashboardMdl" -- a dead relative link. The header opens the same modal
    -- with a <label for>, and so must this.
    it "dashboardsZeroState_opensTheNewDashboardModal_notADeadHref" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters{Dashboards.tag = ["no-such-tag-so-the-list-is-empty"]}
      let html = renderT pg
      html `shouldContainAll` ["No dashboards yet", "for=\"newDashboardMdl\""]
      html `shouldSatisfy` (not . T.isInfixOf "href=\"newDashboardMdl\"")

    -- Removing a tag filter deleted "&tag=<tag>" by substring replace, so dropping
    -- "prod" also mangled "&tag=production" into "&uction" and lost that filter.
    it "activeFilters_removeTag_keepsTagsSharingItsPrefix" \tr -> do
      let dashId = UUIDId $ UUID.fromWords 0xda5 0x7a9 0x9 0x1
      _ <- runTestBg frozenTime tr $ DashboardModel.insert (DashboardModel.mkDashboardVM dashId testPid frozenTime (Projects.UserId UUID.nil)){DashboardModel.title = "Tagged", DashboardModel.tags = ["prod", "production"]}
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters{Dashboards.tag = ["prod", "production"]}
      let removeLink tag = T.takeWhile (/= '"') . T.drop 8 . snd . T.breakOn "hx-get=\"" . snd $ T.breakOn ("aria-label=\"Remove filter: " <> tag <> "\"") (renderT pg)
          base = "/p/" <> testPid.toText <> "/dashboards?sort=-updated_at"
      removeLink "prod" `shouldBe` base <> "&amp;tag=production"
      removeLink "production" `shouldBe` base <> "&amp;tag=prod"
      -- Later tests in this sequential spec count the project's dashboards.
      void $ runTestBg frozenTime tr $ DashboardModel.deleteDashboardsByIds testPid [dashId]

    it "dashboardTemplatePicker_defersHiddenPreviewsUntilOpened" \tr -> do
      (_, initial) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      let initialHtml = renderT initial
      initialHtml `shouldSatisfy` (not . T.isInfixOf "dashboardTemplatePreviews")
      initialHtml `shouldContainAll` ["hx-get=\"/p/" <> testPid.toText <> "/dashboards/new\"", "hx-trigger=\"change[target.checked] from:#newDashboardMdl\""]
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing (Just "true") filters
      let html = renderT pg
      html `shouldContainAll` ["value=\"Blank dashboard\"", "data-title=\"Nginx\"", "data-template=\"nginx.yaml\"", "Requests"]
      html `shouldSatisfy` (not . T.isInfixOf "http-stats.svg")
      PageCtx _ d <- asDashboardsGet pg
      for_ d.dashTemplates \template ->
        html `shouldSatisfy` T.isInfixOf ("data-template=\"" <> fromMaybe "" template.file <> "\"")

    it "dashboardTemplates_deployedEnvironmentsDoNotReadYamlPerRequest" \tr ->
      withSystemTempDirectory "dashboard-templates" $ \dir -> withCurrentDirectory dir $ for_ [Prod, Staging] \env -> do
        let deployed = tr{trATCtx = tr.trATCtx{config = tr.trATCtx.config{environment = env, liveReloadDashboards = True}}}
        (_, pg) <- testServant deployed $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
        PageCtx _ d <- asDashboardsGet pg
        d.dashTemplates `shouldSatisfy` (not . null)

    it "dashboardList_skipsUnusedTelemetryScopeButPreservesShellData" \tr -> do
      let scopeFields = ["service.name", "deployment.environment.name"]
          doc = Catalog.SummaryDoc (HM.fromList [(k, Catalog.FieldStruct mempty mempty Catalog.FCResource False) | k <- scopeFields]) (V.singleton "checkout") (HM.fromList [(k, Catalog.TopK 1 (HM.singleton "production" 1)) | k <- scopeFields])
      runHasqlEffect tr $ SchemaCatalog.upsertSummary $ V.singleton (testPid, doc)
      (_, (_, _, fullShell)) <- testServant tr $ BodyWrapper.mkPageCtx testPid >>= addRespHeaders
      fullShell.envOptions `shouldBe` V.singleton "production"
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      PageCtx bw _ <- asDashboardsGet pg
      (bw.envOptions, bw.serviceOptions) `shouldBe` (V.empty, V.empty)
      map (.conversationId) bw.conversations `shouldBe` map (.conversationId) fullShell.conversations
      isJust bw.activationProgressM `shouldBe` isJust fullShell.activationProgressM

    it "Should update a dashboard" \tr -> do
      created <- listDashboards tr >>= byTitle "Test Dashboard"
      _ <- testServant tr $ Dashboards.dashboardRenamePatchH testPid created.id Dashboards.DashboardRenameForm{Dashboards.title = "Updated Dashboard", Dashboards.fileDir = Nothing}
      void $ listDashboards tr >>= byTitle "Updated Dashboard"

    it "Should delete a dashboard" \tr -> do
      created <- listDashboards tr >>= byTitle "Dashboard 1"
      testServant tr (Dashboards.dashboardDeleteH testPid created.id) >>= expectNoContent . snd
      V.any ((== "Dashboard 1") . (.title)) <$> listDashboards tr >>= (`shouldBe` False)

    it "Should duplicate dashboard" \tr -> do
      created <- listDashboards tr >>= byTitle "Updated Dashboard"
      _ <- testServant tr $ Dashboards.dashboardDuplicatePostH testPid created.id
      void $ listDashboards tr >>= byTitle "Updated Dashboard (Copy)"

    it "Should handle bulk add teams to dashboards" \tr -> do
      let team = ManageMembers.TeamForm{teamName = "Hello", teamDescription = "", teamHandle = "hello", notifEmails = [], teamMembers = [], discordChannels = [], slackChannels = [], phoneNumbers = [], pagerdutyServices = [], teamId = Nothing}
      for_ ["hello", "hii", "broo"] \h -> testServant tr $ ManageMembers.manageTeamPostH testPid team{teamHandle = h} Nothing
      teams' <-
        testServant tr (ManageMembers.manageTeamsGetH testPid (Just "")) >>= \case
          (_, ManageMembers.ManageTeamsGet' (_, _, _, _, ts)) -> pure ts
          _ -> fail "Expected ManageTeamsGet' response"
      let teamIds = V.toList $ V.map (.id) $ V.filter ((/= "broo") . (.handle)) teams'
      dIds <- V.toList . V.map (.id) . V.filter ((/= "Updated Dashboard (Copy)") . (.title)) <$> listDashboards tr
      _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid Dashboards.BAAddTeams Dashboards.DashboardBulkActionForm{itemId = dIds, teamIds}
      dashboards <- listDashboards tr
      length dashboards `shouldBe` 3
      for_ dashboards \db ->
        if db.title == "Updated Dashboard (Copy)"
          then length db.teams `shouldBe` 1
          else -- the selected teams plus the implicit @everyone, never the unselected broo
            (length db.teams, sort $ mapMaybe (\tid -> (.handle) <$> V.find ((== tid) . (.id)) teams') (V.toList db.teams)) `shouldBe` (3, ["everyone", "hello", "hii"])

    it "Should handle bulk delete of dashboards" \tr -> do
      dIds <- V.toList . V.map (.id) <$> listDashboards tr
      _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid Dashboards.BADelete Dashboards.DashboardBulkActionForm{itemId = dIds, teamIds = []}
      listDashboards tr >>= (`shouldBe` 0) . V.length

    it "Should star and unstar a dashboard" \tr -> do
      _ <- testServant tr $ postDashboards ["Dashboard A", "Dashboard B", "Dashboard C"] "Star Test Dashboard"
      dash <- listDashboards tr >>= byTitle "Star Test Dashboard"
      dash.starredSince `shouldSatisfy` isNothing
      for_ [isJust, isNothing] \starred -> do
        _ <- testServant tr $ Dashboards.dashboardStarPostH testPid dash.id
        toggled <- listDashboards tr >>= byTitle "Star Test Dashboard"
        toggled.starredSince `shouldSatisfy` starred

    it "Should sort starred dashboards first" \tr -> do
      dashboards <- listDashboards tr
      for_ ["Dashboard C", "Dashboard A"] \t -> byTitle t dashboards >>= testServant tr . Dashboards.dashboardStarPostH testPid . (.id)
      map (isJust . (.starredSince)) . V.toList <$> listDashboards tr >>= (`shouldBe` [True, True, False, False])
