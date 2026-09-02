module Pages.DashboardsSpec (spec) where

import "cryptonite" Crypto.Hash qualified as Crypto
import Data.Default (def)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.Concurrent (runConcurrent)
import Lucid (renderText, toHtml)
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.ProjectMembers (TeamVM (..))
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Charts.Types (MetricsData (..))
import Pages.Dashboards (DashboardFilters (..))
import Pages.Dashboards qualified as Dashboards
import Pages.Projects (TeamForm (..))
import Pages.Projects qualified as ManageMembers
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), mkHasqlPool)
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (..))
import Test.Hspec


filters :: Dashboards.DashboardFilters
filters =
  Dashboards.DashboardFilters
    { tag = []
    }


spec :: Spec
-- Later tests reuse dashboards created by earlier ones (e.g. "bulk add teams to
-- dashboards"), so this keeps aroundAll and runs sequentially — opting out of the
-- suite's per-test isolation + parallelism (same as GitSyncSpec).
spec = sequential $ aroundAll withTestResources do
  describe "Dashboards Tests" do
    let mkDashboard t = Dashboards.DashboardForm{Dashboards.title = t, Dashboards.file = "overview.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
        dashboard = mkDashboard "Test Dashboard"

    -- Regression: the variable input emitted data-tagify-mode twice for multi
    -- variables; Lucid merges duplicate attrs with (<>), so multi-select vars
    -- silently rendered as single-select ("" <> "select"). Multi vars must carry
    -- NO tagify-mode attr (main.ts only sets options.mode when it is present).
    it "renders data-tagify-mode=select only for single-select variables" \_ -> do
      let mkVar k m =
            DashboardModel.Variable
              { key = k
              , title = Nothing
              , multi = m
              , required = Nothing
              , reloadOnChange = Nothing
              , helpText = Nothing
              , _vType = DashboardModel.VTValues
              , sql = Nothing
              , facetField = Nothing
              , query = Nothing
              , options = Nothing
              , value = Nothing
              , dependsOn = Nothing
              }
          dash = (def :: DashboardModel.Dashboard){DashboardModel.variables = Just [mkVar "single" Nothing, mkVar "multi" (Just True)]}
          vm =
            DashboardVM
              { id = UUIDId UUID.nil
              , projectId = testPid
              , createdAt = frozenTime
              , updatedAt = frozenTime
              , createdBy = Projects.UserId UUID.nil
              , baseTemplate = Nothing
              , schema = Just dash
              , starredSince = Nothing
              , homepageSince = Nothing
              , tags = V.empty
              , title = "t"
              , teams = V.empty
              , filePath = Nothing
              , fileSha = Nothing
              }
          html = TL.toStrict $ renderText $ toHtml $ Dashboards.DashboardGet testPid (UUIDId UUID.nil) dash vm []
      T.count "data-tagify-mode" html `shouldBe` 1
      html `shouldSatisfy` T.isInfixOf "data-tagify-mode=\"select\""

    -- CLAUDE.md's tab/nav swap rule: fetch the full page URL and hx-select the content
    -- container out of it, morphing the tab strip out-of-band so the active class comes
    -- across for free. These tabs had drifted to a separate /content partial endpoint plus
    -- hyperscript that hand-moved .tab-active -- the exact pattern the rule forbids.
    it "dashboardTabs_followTheProjectSwapPattern_notAContentPartial" \_ -> do
      let mkTab n = (def :: DashboardModel.Tab){DashboardModel.name = n}
          dash = (def :: DashboardModel.Dashboard){DashboardModel.tabs = Just [mkTab "Overview", mkTab "Errors"]}
          vm =
            DashboardVM
              { id = UUIDId UUID.nil
              , projectId = testPid
              , createdAt = frozenTime
              , updatedAt = frozenTime
              , createdBy = Projects.UserId UUID.nil
              , baseTemplate = Nothing
              , schema = Just dash
              , starredSince = Nothing
              , homepageSince = Nothing
              , tags = V.empty
              , title = "t"
              , teams = V.empty
              , filePath = Nothing
              , fileSha = Nothing
              }
          html = TL.toStrict $ renderText $ toHtml $ Dashboards.DashboardGet testPid (UUIDId UUID.nil) dash vm []
      for_ ["hx-select=\"#dashboard-tabs-content\"", "hx-select-oob=\"#dashboard-tabs-container:morph\"", "hx-swap=\"morph\"", "hx-push-url=\"true\""] \attr ->
        html `shouldSatisfy` T.isInfixOf attr
      -- No /content partial, and no hyperscript managing the active class.
      html `shouldSatisfy` (not . T.isInfixOf "/content")
      html `shouldSatisfy` (not . T.isInfixOf "remove .tab-active")

    -- Dashboard variables are backend-persisted schema, but their live values come from
    -- the URL. The precedence rule is what makes a dashboard shareable: a link carries the
    -- reader's selections, and clearing one must stay cleared rather than snapping back to
    -- the dashboard's default on the next load.
    it "URL variable values win over the dashboard's declared defaults" \_ -> do
      let var k v =
            DashboardModel.Variable
              { key = k
              , title = Nothing
              , multi = Nothing
              , required = Nothing
              , reloadOnChange = Nothing
              , helpText = Nothing
              , _vType = DashboardModel.VTValues
              , sql = Nothing
              , facetField = Nothing
              , query = Nothing
              , options = Nothing
              , value = v
              , dependsOn = Nothing
              }
          vars = Just [var "env" (Just "prod"), var "region" (Just "eu")]
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

    it "Should create a dashboard" \tr -> do
      (_, pg) <- testServant tr do
        let dashboard1 = mkDashboard "Dashboard 1"
            dashboard2 = mkDashboard "Dashboard 2"
        _ <- Dashboards.dashboardsPostH testPid dashboard1
        _ <- Dashboards.dashboardsPostH testPid dashboard2
        Dashboards.dashboardsPostH testPid dashboard
      case pg of
        Dashboards.DashboardNoContent -> 1 `shouldBe` 1
        _ -> fail "Expected DashboardGet' response"

    it "Should not create a dashboard with an empty title" \tr -> do
      let invalidDashboard = mkDashboard ""
      (_, pg) <- testServant tr $ Dashboards.dashboardsPostH testPid invalidDashboard
      case pg of
        Dashboards.DashboardPostError message -> do
          message `shouldBe` "Dashboard title is required"
        _ -> fail "Expected DashboardPostError response"

    it "Should update a dashboard" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> do
          let createdDashboard = Unsafe.fromJust $ V.find (\x -> x.title == "Test Dashboard") dashboards
          let fm = Dashboards.DashboardRenameForm{Dashboards.title = "Updated Dashboard", Dashboards.fileDir = Nothing}
          _ <- testServant tr $ Dashboards.dashboardRenamePatchH testPid createdDashboard.id fm
          (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
          case pg' of
            Dashboards.DashboardsGet (PageCtx _ d) -> do
              let updated = V.find (\x -> x.title == "Updated Dashboard") d.dashboards
              isJust updated `shouldBe` True
            _ -> fail "Expected DashboardGet' response"
        _ -> fail "Expected DashboardGet' response"

    it "Should delete a dashboard" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let createdDashboard = V.find (\db -> db.title == "Dashboard 1") d.dashboards & Unsafe.fromJust
          (_, pg') <- testServant tr $ Dashboards.dashboardDeleteH testPid createdDashboard.id
          case pg' of
            Dashboards.DashboardNoContent -> do
              (_, pg'') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
              case pg'' of
                Dashboards.DashboardsGet (PageCtx _ dd) -> do
                  let v = V.find (\db -> db.title == "Dashboard 1") dd.dashboards
                  isNothing v `shouldBe` True
                _ -> fail "Expected DashboardGet' response"
            _ -> fail "Expected DashboardDelete response"
        _ -> fail "Expected DashboardGet' response"

    it "Should duplicate dashboard" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) -> do
          let createdDashboard = Unsafe.fromJust $ V.find (\db -> db.title == "Updated Dashboard") dashboards
          _ <- testServant tr $ Dashboards.dashboardDuplicatePostH testPid createdDashboard.id
          (_, pg2) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
          case pg2 of
            Dashboards.DashboardsGet (PageCtx _ d) -> do
              let duplicatedDashboard = V.find (\db -> db.title == "Updated Dashboard (Copy)") d.dashboards
              isJust duplicatedDashboard `shouldBe` True
            _ -> fail "Expected DashboardGet' response"
        _ -> fail "Expected DashboardGet' response"

    it "Should handle bulk add teams to dashboards" \tr -> do
      -- Note: Dashboards already created by "Should create a dashboard" test
      -- Create test teams
      let team = ManageMembers.TeamForm{teamName = "Hello", teamDescription = "", teamHandle = "hello", notifEmails = [], teamMembers = [], discordChannels = [], slackChannels = [], phoneNumbers = [], pagerdutyServices = [], teamId = Nothing}
      _ <- testServant tr $ ManageMembers.manageTeamPostH testPid team Nothing
      _ <- testServant tr $ ManageMembers.manageTeamPostH testPid team{teamHandle = "hii"} Nothing
      _ <- testServant tr $ ManageMembers.manageTeamPostH testPid team{teamHandle = "broo"} Nothing
      (_, tm) <- testServant tr $ ManageMembers.manageTeamsGetH testPid (Just "")
      case tm of
        ManageMembers.ManageTeamsGet' (pid, members, slackChannels, discordChannels, teams') -> do
          -- Get all team IDs (not handles - the field name is misleading)
          let selectedTeams = V.filter (\t -> t.handle /= "broo") teams'
          let teamIds = V.toList $ V.map (\t -> t.id) selectedTeams
          (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing (DashboardFilters [])
          case pg of
            Dashboards.DashboardsGet (PageCtx _ d) -> do
              let dIds = V.toList $ V.map (.id) $ V.filter (\db -> db.title /= "Updated Dashboard (Copy)") d.dashboards
              let bulkActionForm = Dashboards.DashboardBulkActionForm{itemId = dIds, teamHandles = teamIds}
              _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid "add_teams" bulkActionForm
              (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
              case pg' of
                Dashboards.DashboardsGet (PageCtx _ dd) -> do
                  length dd.dashboards `shouldBe` 3
                  forM_ dd.dashboards $ \db -> do
                    if db.title == "Updated Dashboard (Copy)"
                      then length db.teams `shouldBe` 0
                      else do
                        -- Should have 3 teams: hello, hii, and @everyone
                        length db.teams `shouldBe` 3
                        -- Get team handles for assertions
                        let dbTeamHandles = mapMaybe (\tid -> V.find (\t -> t.id == tid) teams' <&> (.handle)) (V.toList db.teams)
                        -- Verify required teams are present and broo is excluded
                        all (`elem` dbTeamHandles) ["hello", "hii", "everyone"] `shouldBe` True
                        dbTeamHandles `shouldSatisfy` notElem "broo"
                _ -> fail "Expected DashboardDelete response"
            _ -> fail "Expected DashboardGet' response"
        _ -> fail "Expected ManageTeamsGet' response"

    it "Should handle bulk delete of dashboards" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let dIds = V.toList $ V.map (.id) d.dashboards
          let bulkActionForm = Dashboards.DashboardBulkActionForm{itemId = dIds, teamHandles = []}
          _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid "delete" bulkActionForm
          (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
          case pg' of
            Dashboards.DashboardsGet (PageCtx _ dd) -> do
              length dd.dashboards `shouldBe` 0
            _ -> fail "Expected DashboardDelete response"
        _ -> fail "Expected DashboardGet' response"

    it "Should star and unstar a dashboard" \tr -> do
      let dashboard1 = mkDashboard "Star Test Dashboard"
          dashboardA = mkDashboard "Dashboard A"
          dashboardB = mkDashboard "Dashboard B"
          dashboardC = mkDashboard "Dashboard C"
      _ <- testServant tr do
        _ <- Dashboards.dashboardsPostH testPid dashboardA
        _ <- Dashboards.dashboardsPostH testPid dashboardB
        _ <- Dashboards.dashboardsPostH testPid dashboardC
        Dashboards.dashboardsPostH testPid dashboard1
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let dash = Unsafe.fromJust $ V.find (\x -> x.title == "Star Test Dashboard") d.dashboards
          dash.starredSince `shouldSatisfy` isNothing

          -- Star the dashboard
          _ <- testServant tr $ Dashboards.dashboardStarPostH testPid dash.id
          (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
          case pg' of
            Dashboards.DashboardsGet (PageCtx _ d') -> do
              let starredDash = Unsafe.fromJust $ V.find (\x -> x.id == dash.id) d'.dashboards
              starredDash.starredSince `shouldSatisfy` isJust

              -- Unstar the dashboard
              _ <- testServant tr $ Dashboards.dashboardStarPostH testPid dash.id
              (_, pg'') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
              case pg'' of
                Dashboards.DashboardsGet (PageCtx _ d'') -> do
                  let unstarredDash = Unsafe.fromJust $ V.find (\x -> x.id == dash.id) d''.dashboards
                  unstarredDash.starredSince `shouldSatisfy` isNothing
                  pass
                _ -> fail "Expected DashboardsGet response"
            _ -> fail "Expected DashboardsGet response"
        _ -> fail "Expected DashboardsGet response"

    it "Should sort starred dashboards first" \tr -> do
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let dashA = Unsafe.fromJust $ V.find (\x -> x.title == "Dashboard A") d.dashboards
          let dashC = Unsafe.fromJust $ V.find (\x -> x.title == "Dashboard C") d.dashboards

          -- Star Dashboard C and Dashboard A
          _ <- testServant tr $ Dashboards.dashboardStarPostH testPid dashC.id
          _ <- testServant tr $ Dashboards.dashboardStarPostH testPid dashA.id

          -- Fetch dashboards and verify starred ones appear first
          (_, pg') <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing filters
          case pg' of
            Dashboards.DashboardsGet (PageCtx _ d') -> do
              let dashboards = d'.dashboards
              let starredDashboards = V.filter (\x -> isJust x.starredSince) dashboards
              let unstarredDashboards = V.filter (\x -> isNothing x.starredSince) dashboards

              -- Verify starred dashboards come first
              V.length starredDashboards `shouldBe` 2
              V.length unstarredDashboards `shouldBe` 2

              -- Check that first dashboards in the list are starred
              let firstTwo = V.take 2 dashboards
              V.all (\x -> isJust x.starredSince) firstTwo `shouldBe` True
            _ -> fail "Expected DashboardsGet response"
        _ -> fail "Expected DashboardsGet response"
