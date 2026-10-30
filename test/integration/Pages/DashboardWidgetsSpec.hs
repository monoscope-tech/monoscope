-- | The dashboard canvas, end to end: add a widget of every type, move it, resize it,
-- delete it, and confirm the backend still agrees after a reload.
--
-- DashboardsSpec covers dashboard CRUD. Nothing covered the widgets *on* a dashboard —
-- which is the entire product surface — so a widget type could be unsaveable, a drag
-- could silently drop a widget, and no test would notice. These drive the same handlers
-- the browser calls, and enumerate WidgetType via Bounded so a new widget type joins the
-- suite by existing rather than by someone remembering to add it.
module Pages.DashboardWidgetsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception qualified as E
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Models.Apis.LogQueries qualified as LogQueries
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ qualified as SqlQQ
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful.Error.Static (catchError)
import GHC.Clock (getMonotonicTime)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.DashboardTemplates (dashboardTemplatesCompiled, loadDashboardFromVM)
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.Projects (Session (..))
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (BWConfig (..), NavigationResponse (..), PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards (DashboardFilters (..))
import Pages.Dashboards qualified as Dashboards
import Pages.Issues qualified as Issues
import Pages.LogExplorer.Log qualified as Log
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (browserKql, parseQueryToAST)
import Pkg.QueryCache qualified as QueryCache
import Pkg.TestClock (getTestTime, setTestTime)
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Relude
import Servant qualified
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import System.Config qualified as Config
import System.IO.Error (userError)
import System.Timeout (timeout)
import System.Types (addRespHeaders)
import Test.Hspec
import Text.Slugify (slugify)
import UnliftIO.Async (mapConcurrently_, wait, withAsync)
import Utils qualified
import Web.ApiHandlers qualified as Api
import Web.ApiTypes qualified as ApiTypes
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)
import Web.FormUrlEncoded (urlDecodeAsForm)
import Web.Routes qualified as Routes


-- Flattened: the eager widgets on these templates are children of a group.
renderedWidgets :: Dashboards.DashboardGet -> [Widget.Widget]
renderedWidgets (Dashboards.DashboardGet _ _ dash _ _) = foldMap (foldMap (concatMap flatten . (.widgets))) dash.tabs
  where
    flatten w = w : concatMap flatten (fold w.children)


tabDashboard :: NavigationResponse Dashboards.DashboardGet -> Dashboards.DashboardGet
tabDashboard = \case
  NavigationFull (PageCtx _ dashboard) -> dashboard
  NavigationPartial dashboard _ -> dashboard


getTab :: TestResources -> DashboardModel.DashboardId -> Text -> Maybe Text -> [(Text, Maybe Text)] -> IO (NavigationResponse Dashboards.DashboardGet)
getTab tr dashId slug hx params = snd <$> testServant tr (Dashboards.dashboardTabGetH testPid dashId slug Nothing Nothing Nothing (Just "24H") hx params)


partialHtml :: NavigationResponse a -> IO Text
partialHtml = \case
  NavigationPartial _ fragment -> pure $ renderT fragment
  NavigationFull _ -> expectationFailure "htmx navigation returned a full document" $> ""


rumLocation :: TestResources -> Projects.ProjectId -> [Text] -> [(Text, Maybe Text)] -> IO Text
rumLocation tr pid view ps = runAuthHandler tr (Dashboards.rumRedirectH pid view ps) >>= \h -> case lookupResponseHeader @"Location" h of
  Header l -> pure l
  _ -> fail "redirect carried no Location header"


explorerLinkParams :: Projects.ProjectId -> Text -> [[(Text, Text)]]
explorerLinkParams pid url = linkParams ("/p/" <> pid.toText <> "/log_explorer") ("href=\"" <> url <> "\"")


rumTemplate :: IO DashboardModel.Dashboard
rumTemplate = maybe (fail "missing rum.yaml") pure . find (\d -> d.file == Just "rum.yaml") =<< DashboardModel.readDashboardsFromDisk "static/public/dashboards"


widgetTitled :: Text -> [Widget.Widget] -> IO Widget.Widget
widgetTitled t = maybe (fail $ "missing widget " <> toString t) pure . find ((== Just t) . (.title))


-- The test session narrowed to the production environment and checkout service.
checkoutSession :: TestResources -> IO (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Projects.Session)
checkoutSession tr =
  fromRightShow
    <$> runTestEffect tr.trPool (Config.hasqlPool tr.trATCtx) tr.trLogger tr.trTracerProvider (Auth.sessionByID (Just s.sessionId) s.requestID s.isSidebarClosed s.theme s.lang (Just "production") (Just "checkout") Nothing Auth.ChallengeRedirect)
  where
    s = Servant.getResponse tr.trSessAndHeader


-- A stream's last frame carries the same result the JSON endpoint returns.
completesWith :: Servant.Headers hs (Servant.SourceIO AE.Value) -> Charts.MetricsData -> Expectation
completesWith response md = (fmap (viaNonEmpty last) <$> streamFrames response) `shouldReturn` Right (Just $ AE.object ["type" AE..= ("complete" :: Text), "data" AE..= md])


allWidgetTypes :: [Widget.WidgetType]
allWidgetTypes = [minBound .. maxBound]


noFilters :: Dashboards.DashboardFilters
noFilters = Dashboards.DashboardFilters{tag = []}


-- | Create a dashboard and hand back its id.
--
-- Clears existing dashboards first: the test UUID interpreter is deterministic and
-- restarts per request, so the id minted here is the same in every example and a second
-- create would collide on the primary key. This spec's database is created fresh from a
-- template and is private to the file, so clearing is safe.
newDashboard :: TestResources -> Text -> Text -> IO DashboardModel.DashboardId
newDashboard tr file title = do
  (_, existing) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
  case existing of
    Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) ->
      for_ dashboards \d -> void $ testServant tr $ Dashboards.dashboardDeleteH testPid d.id
    _ -> pass
  _ <- testServant tr $ Dashboards.dashboardsPostH testPid Dashboards.DashboardForm{Dashboards.title = title, Dashboards.file = file, Dashboards.teams = [], Dashboards.fileDir = Nothing}
  (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
  case pg of
    Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) ->
      maybe (fail $ "dashboard not found after creating it: " <> toString title) (pure . (.id)) $ V.find (\d -> d.title == title) dashboards
    _ -> fail "expected the dashboard list"


-- | The widgets currently stored on a dashboard (root level), read back through the same
-- effect stack a request runs in — i.e. what the next page load would render.
storedWidgets :: TestResources -> DashboardModel.DashboardId -> IO [Widget.Widget]
storedWidgets tr dashId = snd <$> testServant tr (Dashboards.getDashAndVM testPid dashId Nothing >>= addRespHeaders . (.widgets) . snd)


widgetOf :: Widget.WidgetType -> Text -> Widget.Widget
widgetOf wt title =
  (def :: Widget.Widget)
    { Widget.wType = wt
    , Widget.title = Just title
    , Widget.query = Just "name != null"
    , Widget.layout = Just def{Widget.w = Just 3, Widget.h = Just 3, Widget.x = Just 0, Widget.y = Just 0}
    }


-- Total accessors: -Werror=x-partial rules out head, and a failed example should say
-- what it expected rather than crash inside a partial function.
onlyWidget :: [Widget.Widget] -> IO Widget.Widget
onlyWidget = \case
  [w] -> pure w
  ws -> fail $ "expected exactly one widget, got " <> show (length ws)


firstWidgetId :: [Widget.Widget] -> IO Text
firstWidgetId ws = case ws of
  (w : _) -> maybe (fail "the stored widget has no id") pure w.id
  [] -> fail "expected at least one widget"


-- Add several widgets in a single request block. The test UUID interpreter restarts per
-- request, so widgets added by separate calls would all be minted with the same id —
-- which cannot happen in production and would make the ids useless as canvas handles.
addWidgets :: TestResources -> DashboardModel.DashboardId -> [Widget.Widget] -> IO ()
addWidgets tr dashId ws = void $ testServant tr $ traverse_ (Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing) ws >> addRespHeaders ()


numberedWidgets :: Int -> [Widget.Widget]
numberedWidgets n = [widgetOf Widget.WTTimeseries ("w" <> show i) | i <- [1 .. n]]


reorder :: Text -> Int -> Int -> Int -> Int -> Map Text Dashboards.WidgetReorderItem
reorder wid x y w h = Map.singleton wid (def :: Dashboards.WidgetReorderItem){Dashboards.x = Just x, Dashboards.y = Just y, Dashboards.w = Just w, Dashboards.h = Just h}


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Shared dashboard views" do
    it "savesArbitraryVariables_withoutFreezingTheTemplate_andKeepsTabAndWindow" \tr -> do
      did <- newDashboard tr "rum.yaml" "Shared views"
      let params = [("var-browser", Just "Safari"), ("since", Just "7D"), ("activeTabSlug", Just "errors")]
          root = "/p/" <> testPid.toText <> "/dashboards/" <> did.toText <> "/tab/errors"
      form <- either (fail . toString) pure $ urlDecodeAsForm @Dashboards.DashboardViewForm "name=Mobile+Safari&var-browser=Safari&since=7D&activeTabSlug=errors"
      (_, saved) <- testServant tr $ Dashboards.dashboardViewPostH testPid did form
      linkParams root (renderT saved) `shouldSatisfy` any (\ps -> lookup "var-browser" ps == Just "Safari" && lookup "since" ps == Just "7D")
      (vm, _) <- runAuthHandler tr $ Dashboards.getDashAndVM testPid did Nothing
      vm.schema `shouldSatisfy` isNothing
      map (.name) (V.toList vm.savedViews) `shouldBe` ["Mobile Safari"]
      (_, deleted) <- testServant tr $ Dashboards.dashboardViewDeleteH testPid did "Mobile Safari" params
      renderT deleted `shouldNotSatisfy` T.isInfixOf ">Mobile Safari</a>"

    it "rendersConfiguredViews_andTabScopedVariables_onAnyDashboard" \tr -> do
      did <- newDashboard tr "overview.yaml" "Shared views"
      _ <- testServant tr $ Dashboards.dashboardYamlPutH testPid did $ Dashboards.YamlForm "title: Shared views\nwidgets: []\nvariables:\n  - {key: service, title: Service, type: values, options: [[api]]}\n  - {key: region, title: Region, type: values, options: [[eu]]}\nviews:\n  - {name: Europe API, variables: {service: api, region: eu}}\ntabs:\n  - {name: Overview, variables: [service], widgets: []}\n  - {name: Errors, variables: [service, region], widgets: []}\n"
      html <- renderT <$> getTab tr did "overview" Nothing []
      html `shouldContainAll` [">Europe API</a>", "name=\"service\""]
      html `shouldNotSatisfy` T.isInfixOf "name=\"region\""
      errors <- getTab tr did "errors" (Just "div#dashboard-tabs-content") []
      renderT errors `shouldSatisfy` T.isInfixOf "name=\"region\""
      form <- either (fail . toString) pure $ urlDecodeAsForm @Dashboards.DashboardViewForm "name=US+worker&var-service=worker&var-region=us&var-unknown=ignored&since=24H&activeTabSlug=errors"
      _ <- testServant tr $ Dashboards.dashboardViewPostH testPid did form
      (saved, _) <- runAuthHandler tr $ Dashboards.getDashAndVM testPid did Nothing
      map (.variables) (V.toList saved.savedViews) `shouldBe` [Map.fromList [("service", "worker"), ("region", "us")]]
      (_, overridden) <- testServant tr $ Dashboards.dashboardViewPostH testPid did form{Dashboards.name = "Europe API"}
      renderT overridden `shouldNotSatisfy` T.isInfixOf "var-region=eu"
      (_, revealed) <- testServant tr $ Dashboards.dashboardViewDeleteH testPid did "Europe API" form.params
      renderT revealed `shouldSatisfy` T.isInfixOf "var-region=eu"
      reloaded <- getTab tr did "overview" (Just "div#dashboard-tabs-content") [("since", Just "24H")]
      let root = "/p/" <> testPid.toText <> "/dashboards/" <> did.toText <> "/tab/overview"
      linkParams root (renderT reloaded) `shouldSatisfy` any (\ps -> lookup "var-service" ps == Just "worker" && lookup "var-region" ps == Just "us" && lookup "since" ps == Just "24H")

    it "rejectsInvalidNames_andCrossProjectChanges" \tr -> do
      did <- newDashboard tr "rum.yaml" "Private views"
      other <- createTestProject tr "Other saved views"
      let save pid name = Dashboards.dashboardViewPostH pid did (Dashboards.DashboardViewForm name [])
          status action = runAuthHandler tr $ catchError @Servant.ServerError (action $> (200 :: Int)) (\_ err -> pure err.errHTTPCode)
      status (save testPid "  ") `shouldReturn` 400
      status (save testPid $ T.replicate 101 "x") `shouldReturn` 400
      status (save other "Wrong project") `shouldReturn` 404
      status (Dashboards.dashboardViewDeleteH other did "Missing" []) `shouldReturn` 404
      assertNonMemberDenied tr other (save other "Denied")
      assertNonMemberDenied tr other (Dashboards.dashboardViewDeleteH other did "Denied" [])

    it "overviewTemplate_scopesFiltersByTab_andRestoresAllServices" \tr -> do
      did <- newDashboard tr "_overview.yaml" "Overview views"
      for_ ([("overview", ["service", "resource"]), ("service", ["service", "resource"]), ("logs", ["service"]), ("databases", ["database"]), ("infra", [])] :: [(Text, [Text])]) \(slug, keys) -> do
        html <- renderT <$> getTab tr did slug Nothing [("var-service", Just "api"), ("var-database", Just "postgresql")]
        ("id=\"dash-vars-toggle\"" `T.isInfixOf` html) `shouldBe` not (null keys)
        for_ ["service", "resource", "database"] \key ->
          ("name=\"" <> key <> "\"") `T.isInfixOf` html `shouldBe` (key `elem` keys)
        let root = "/p/" <> testPid.toText <> "/dashboards/" <> did.toText <> "/tab/" <> slug
        linkParams root html `shouldSatisfy` any (\ps -> lookup "var-service" ps == Just "" && lookup "var-database" ps == Just "")
      form <- either (fail . toString) pure $ urlDecodeAsForm @Dashboards.DashboardViewForm "name=API&var-service=api&since=7D&activeTabSlug=logs"
      (_, saved) <- testServant tr $ Dashboards.dashboardViewPostH testPid did form
      renderT saved `shouldSatisfy` T.isInfixOf "Saved views: API"

    it "concurrentSaves_keepOtherViews_andReplaceTheSameNameOnce" \tr -> do
      did <- newDashboard tr "rum.yaml" "Concurrent views"
      let names = ["View " <> show i | i <- [1 .. 8 :: Int]]
          save name browser = void $ testServant tr $ Dashboards.dashboardViewPostH testPid did (Dashboards.DashboardViewForm name [("var-browser", Just browser)])
      mapConcurrently_ (`save` "Safari") names
      (saved, _) <- runAuthHandler tr $ Dashboards.getDashAndVM testPid did Nothing
      map (.name) (toList saved.savedViews) `shouldMatchList` names
      mapConcurrently_ (save "View 1") ["Chrome", "Firefox"]
      (updated, _) <- runAuthHandler tr $ Dashboards.getDashAndVM testPid did Nothing
      map (.name) (toList updated.savedViews) `shouldMatchList` names
      [Map.lookup "browser" v.variables | v <- toList updated.savedViews, v.name == "View 1"] `shouldSatisfy` (`elem` ([[Just "Chrome"], [Just "Firefox"]] :: [[Maybe Text]]))

    it "migratesLegacySegments_evenWithoutAnExistingDashboard" \tr -> do
      pid <- createTestProject tr "Unopened RUM dashboard"
      did <- newDashboard tr "rum.yaml" "Existing RUM dashboard"
      migration <- Query <$> readFileBS "static/migrations/0255_dashboard_saved_views.sql"
      withResource tr.trPool \conn -> do
        PG.execute_ conn "ALTER TABLE projects.dashboards DROP COLUMN saved_views" `shouldReturn` 0
        _ <- PG.execute_ conn "CREATE TABLE projects.rum_segments (project_id uuid, name text, filters jsonb)"
        _ <- PG.execute conn "INSERT INTO projects.rum_segments VALUES (?, 'Safari', '{\"browser\":\"Safari\",\"app\":null,\"errored\":true}'), (?, 'Safari', '{\"browser\":\"Safari\",\"errored\":false}')" (testPid, pid)
        _ <- PG.execute_ conn migration
        rows <- PG.query conn "SELECT project_id, saved_views[1]->>'name', saved_views[1]->'variables' FROM projects.dashboards WHERE id = ? OR project_id = ? ORDER BY project_id" (did, pid)
        (rows :: [(Projects.ProjectId, Text, Aeson AE.Value)]) `shouldMatchList` [(testPid, "Safari", Aeson $ AE.object ["browser" AE..= ("Safari" :: Text), "errors" AE..= ("true" :: Text)]), (pid, "Safari", Aeson $ AE.object ["browser" AE..= ("Safari" :: Text), "errors" AE..= ("false" :: Text)])]

  describe "Widget PNG export" do
    it "tablePng_aggregateChangesAlterRenderedImage" \tr ->
      for_ [Nothing, Just Widget.PngSlack] \profile -> do
        pngs <- forM [2, 3 :: Int] \count -> do
          let widget =
                (def :: Widget.Widget)
                  { Widget.wType = Widget.WTTable
                  , Widget.pngProfile = profile
                  , Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.source = AE.toJSON ([[AE.String "events"], [AE.toJSON count]] :: [[AE.Value]])}
                  }
              encoded = decodeUtf8 @Text $ AE.encode widget
              signature = Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded
          response <- toBaseServantResponse tr $ Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just signature) []
          let bytes = Servant.getResponse response
          LBS.take 8 bytes `shouldBe` "\137PNG\r\n\26\n"
          pure bytes
        length (ordNub pngs) `shouldBe` 2

  describe "Widget type contract" do
    it "widgetWarningThresholdMigration_repairsNestedKeysAndPreservesCanonicalValues" \tr -> do
      dashId <- newDashboard tr "" "Legacy warning thresholds"
      let legacy = "{\"widgets\":[{\"type\":\"stat\",\"arning_threshold\":7,\"query\":\"arning_threshold\"},{\"type\":\"group\",\"children\":[{\"type\":\"stat\",\"warning_threshold\":9,\"arning_threshold\":4},{\"type\":\"stat\",\"arning_threshold\":null}]}],\"tabs\":[{\"name\":\"Nested\",\"widgets\":[{\"type\":\"stat\",\"arning_threshold\":3}]}]}" :: Text
          expected = "{\"widgets\":[{\"type\":\"stat\",\"warning_threshold\":7,\"query\":\"arning_threshold\"},{\"type\":\"group\",\"children\":[{\"type\":\"stat\",\"warning_threshold\":9},{\"type\":\"stat\",\"warning_threshold\":null}]}],\"tabs\":[{\"name\":\"Nested\",\"widgets\":[{\"type\":\"stat\",\"warning_threshold\":3}]}]}"
      migration <- Query <$> readFileBS "static/migrations/0212_widget_warning_threshold.sql"
      withResource tr.trPool \conn -> PG.withTransaction conn do
        void $ PG.execute conn "UPDATE projects.dashboards SET schema = ?::jsonb WHERE id = ?" (legacy, dashId)
        AE.eitherDecodeStrict' @DashboardModel.Dashboard (encodeUtf8 legacy) `shouldSatisfy` isLeft
        replicateM_ 2 do
          void $ PG.execute_ conn migration
          [PG.Only (Aeson actual)] <- PG.query conn "SELECT schema FROM projects.dashboards WHERE id = ?" (PG.Only dashId)
          Right actual `shouldBe` AE.eitherDecode @AE.Value expected
          AE.fromJSON @DashboardModel.Dashboard actual `shouldSatisfy` \case
            AE.Success _ -> True
            AE.Error _ -> False

    -- The wire tag is what every stored dashboard YAML and every saved widget already
    -- contains. Renaming a constructor silently orphans them, and nothing else notices.
    it "every widget type round-trips through its JSON tag" \_ -> do
      for_ allWidgetTypes \wt -> do
        let encoded = AE.encode wt
        AE.eitherDecode @Widget.WidgetType encoded `shouldSatisfy` \case
          Right decoded -> show @Text decoded == show @Text wt
          Left _ -> False

  describe "Adding widgets to a dashboard" do
    it "widthless widgets default to four per row" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Default Width"
      addWidgets tr dashId [w{Widget.layout = Nothing} | w <- numberedWidgets 5]

      stored <- storedWidgets tr dashId
      for_ stored \widget -> renderT (Widget.widget_ widget) `shouldSatisfy` T.isInfixOf "gs-w=\"3\""
      let widgets = Widget.normalizeWidgetLayouts stored
      map (\w -> (w.layout >>= (.x), w.layout >>= (.y), w.layout >>= (.w))) widgets
        `shouldBe` [(Just 0, Just 0, Just 3), (Just 3, Just 0, Just 3), (Just 6, Just 0, Just 3), (Just 9, Just 0, Just 3), (Just 0, Just 1, Just 3)]

    it "edits a widget in place and retains an omitted query" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Edit"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTTimeseries "original")
      wid <- firstWidgetId =<< storedWidgets tr dashId

      -- The render-time tab slug round-trips through the posted JSON and must not be stored.
      let edited = (widgetOf Widget.WTStat "renamed"){Widget.query = Nothing, Widget.rawQuery = Nothing, Widget._tabSlug = Just "stale-tab"}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just wid) Nothing edited
      only <- onlyWidget =<< storedWidgets tr dashId

      (only.title, only._tabSlug) `shouldBe` (Just "renamed", Nothing)
      show @Text only.wType `shouldBe` show @Text Widget.WTStat
      only.query `shouldBe` Just "name != null"

    it "compactMetricCard_rendersAtChartHeightAndConfirmsDashboardAdd" \tr -> do
      let metricCard = (widgetOf Widget.WTTimeseriesLine "metric"){Widget.layout = Just def{Widget.x = Just 0, Widget.y = Just 0, Widget.w = Just 2, Widget.h = Just 1}}
      normalized <- onlyWidget $ Widget.normalizeWidgetLayouts [metricCard]
      (normalized.layout >>= (.w), normalized.layout >>= (.h)) `shouldBe` (Just 2, Just 3)
      let html = renderT $ Widget.widget_ metricCard{Widget.expandBtnFn = Just "/details"}
      html `shouldContainAll` ["aria-label=\"Expand widget\"", "gap-0.5 flex flex-col", "min-h-8 px-1", "min-h-0 p-3"]

      _ <- newDashboard tr "overview.yaml" "Widget destination"
      (_, picker) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing (Just "true") Nothing (Just "metric-widget") Nothing Nothing noFilters
      -- Rows PUT to the widget upsert, whose response carries the "Widget added" toast.
      renderT picker `shouldSatisfy` T.isInfixOf "hx-put=\"/p/"
      (_, dashboards) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
      renderT dashboards
        `shouldSatisfy` T.isInfixOf "aria-label=\"Add dashboard to favorites\""

    it "chartWidget_groupsTitleWithCardAndGuidesTheEmptyState" \_ -> do
      let html = renderT $ Widget.widget_ (widgetOf Widget.WTTimeseriesLine "requests"){Widget.id = Just "requests-chart"}
      html `shouldNotSatisfy` T.isInfixOf "gap-1.5 flex flex-col"
      html `shouldContainAll` ["gap-0.5 flex flex-col", "id=\"requests-chart_empty\"", "role=\"status\"", "No data in this time range", "Try a wider time range or adjust the filters.", "aria-label=\"View widget query\"", "language=\"kql\""]
      let sqlHtml = renderT $ Widget.widget_ (widgetOf Widget.WTTimeseriesLine "requests"){Widget.sql = Just "SELECT '<script>'", Widget.expandBtnFn = Just "/details"}
      sqlHtml `shouldContainAll` ["language=\"sql\"", "SELECT &#39;&lt;script&gt;&#39;"]

  -- The full canvas lifecycle, once per widget type, in one example: add it, drag it,
  -- resize it, then re-read the dashboard the way the next page load does. Every type goes
  -- through the same three handlers, so looping is what makes "every single kind of widget"
  -- cheap to assert — and a type that silently fails to persist shows up as its own line in
  -- the diff rather than as one opaque failure.
  it "every widget type survives add, move, resize and reload" \tr -> do
    dashId <- newDashboard tr "overview.yaml" "Widget Lifecycle"
    addWidgets tr dashId [widgetOf wt (show @Text wt) | wt <- allWidgetTypes]
    added <- storedWidgets tr dashId
    map (show @Text . (.wType)) added `shouldBe` map (show @Text) allWidgetTypes

    -- Move and resize every one to a position and size unique to its index, so a widget
    -- landing on another's coordinates cannot pass.
    let placed = [(wid, (i * 2) `mod` 12, i, 1 + (i `mod` 4), 2 + (i `mod` 3)) | (i, wid) <- zip [0 ..] (mapMaybe (.id) added)]
        patch = fromList [(wid, (def :: Dashboards.WidgetReorderItem){Dashboards.x = Just x, Dashboards.y = Just y, Dashboards.w = Just w, Dashboards.h = Just h}) | (wid, x, y, w, h) <- placed]
    _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing patch

    -- The reload: read it back through the same path a fresh request renders from.
    reloaded <- storedWidgets tr dashId
    let geometryOf w = (w.id, w.layout >>= (.x), w.layout >>= (.y), w.layout >>= (.w), w.layout >>= (.h))
        expected = sort [(Just wid, Just x, Just y, Just w, Just h) | (wid, x, y, w, h) <- placed]
    sort (map geometryOf reloaded) `shouldBe` expected
    -- And nothing changed identity along the way: same types, same count, no duplicates.
    sort (map (show @Text . (.wType)) reloaded) `shouldBe` sort (map (show @Text) allWidgetTypes)
    length (ordNub (mapMaybe (.id) reloaded)) `shouldBe` length allWidgetTypes

  describe "Moving and resizing on the canvas" do
    it "moving a widget to the origin is not mistaken for an absent coordinate" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Origin"
      let atSix = (widgetOf Widget.WTTimeseries "origin"){Widget.layout = Just def{Widget.x = Just 6, Widget.y = Just 6, Widget.w = Just 3, Widget.h = Just 3}}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing atSix
      wid <- firstWidgetId =<< storedWidgets tr dashId

      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder wid 0 0 3 3)
      only <- onlyWidget =<< storedWidgets tr dashId

      (only.layout >>= (.x), only.layout >>= (.y)) `shouldBe` (Just 0, Just 0)

    it "a widget dropped from the patch is removed from the dashboard" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Delete"
      addWidgets tr dashId (numberedWidgets 3)
      wids <- mapMaybe (.id) <$> storedWidgets tr dashId
      let kept = take 1 wids <> drop 2 wids

      let patch = Map.fromList [(w, (def :: Dashboards.WidgetReorderItem){Dashboards.x = Just 0, Dashboards.y = Just 0, Dashboards.w = Just 3, Dashboards.h = Just 3}) | w <- kept]
      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing patch
      stored <- storedWidgets tr dashId

      sort (mapMaybe (.id) stored) `shouldBe` sort kept

    -- A patch built before an HTMX swap can name only stale ids. Applying it would wipe
    -- the dashboard, since reorderWidgets rebuilds the list purely from the patch.
    it "a patch naming only unknown widgets is ignored rather than emptying the canvas" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Stale Patch"
      addWidgets tr dashId (numberedWidgets 2)
      priorWidgets <- storedWidgets tr dashId

      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder "not-a-real-widget" 0 0 3 3)
      afterStale <- storedWidgets tr dashId

      map (.id) afterStale `shouldBe` map (.id) priorWidgets

  -- Tabs give a dashboard several independent canvases. Every widget write takes a tab
  -- slug, and getting that routing wrong puts a widget on the wrong canvas — or, for the
  -- reorder patch, deletes one tab's widgets while the user was rearranging another.
  describe "Tabbed dashboards" do
    let tabbedYaml =
          Dashboards.YamlForm
            { Dashboards.yaml =
                "title: Tabbed\nwidgets: []\ntabs:\n  - name: First Tab\n    widgets:\n      - type: timeseries\n        id: first-widget\n        title: First\n        layout: { x: 0, y: 0, w: 3, h: 3 }\n  - name: Second Tab\n    widgets:\n      - type: stat\n        id: second-widget\n        title: Second\n        layout: { x: 0, y: 0, w: 3, h: 3 }\n"
            }
        tabWidgets tr dashId slug = do
          (_, dash) <- testServant tr (Dashboards.getDashAndVM testPid dashId Nothing >>= addRespHeaders . snd)
          pure $ maybe [] (.widgets) $ find (\t -> slugify t.name == slug) (fold dash.tabs)
        newTabbedDashboard tr = do
          dashId <- newDashboard tr "overview.yaml" "Tabbed"
          _ <- testServant tr $ Dashboards.dashboardYamlPutH testPid dashId tabbedYaml
          pure dashId

    it "loads tabs, edits one canvas, duplicates a widget, and preserves the other canvas" \tr -> do
      dashId <- newTabbedDashboard tr
      initialOne <- tabWidgets tr dashId "first-tab"
      initialTwo <- tabWidgets tr dashId "second-tab"
      map (.id) initialOne `shouldBe` [Just "first-widget"]
      map (.id) initialTwo `shouldBe` [Just "second-widget"]

      void $ toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardWidgetPutH testPid dashId Nothing (Just "second-tab") (widgetOf Widget.WTStat "Added To Second")
      void $ toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardDuplicateWidgetPostH testPid dashId "second-widget" Nothing
      void $ toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardWidgetReorderPatchH testPid dashId (Just "first-tab") (reorder "first-widget" 6 4 6 2)

      tabOne <- tabWidgets tr dashId "first-tab"
      tabTwo <- tabWidgets tr dashId "second-tab"
      map (\w -> (w.layout >>= (.x), w.layout >>= (.w))) tabOne `shouldBe` [(Just 6, Just 6)]
      map (.title) tabTwo `shouldBe` [Just "Second", Just "Added To Second", Just "Second (Copy)"]
      let tabTwoIds = mapMaybe (.id) tabTwo
      length tabTwoIds `shouldBe` 3
      length (ordNub tabTwoIds) `shouldBe` 3

    -- Regression: the list counted only root widgets, so a tabbed template (root
    -- `widgets: []`) showed 0 and group children were never counted.
    it "the dashboard list counts tab and group widgets" \tr -> do
      _ <- newDashboard tr "_overview.yaml" "Tabbed Count"
      (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
      case pg of
        Dashboards.DashboardsGet (PageCtx _ d) -> do
          let leaves = filter ((/= Widget.WTGroup) . (.wType)) $ foldMap DashboardModel.allWidgets $ V.toList d.dashboards >>= maybeToList . loadDashboardFromVM d.dashTemplates
          length leaves `shouldSatisfy` (> 20)
          renderT pg `shouldSatisfy` T.isInfixOf (show (length leaves) <> " widgets")
        _ -> fail "Expected DashboardsGet response"

    -- Regression: the expand drawer saved without a tab slug, so a rename of a tab's
    -- widget (or a group child) updated the root list, matched nothing, and still toasted.
    it "renames tab and group widgets despite a missing or wrong tab slug" \tr -> do
      dashId <- newTabbedDashboard tr
      let save wid slug t = toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardWidgetPutH testPid dashId (Just wid) slug (widgetOf Widget.WTStat t)
      void $ save "second-widget" Nothing "Renamed Second"
      map (.title) <$> tabWidgets tr dashId "second-tab" >>= (`shouldBe` [Just "Renamed Second"])
      void $ save "second-widget" (Just "first-tab") "Still Second"
      map (.title) <$> tabWidgets tr dashId "second-tab" >>= (`shouldBe` [Just "Still Second"])
      save "no-such-widget" Nothing "Ghost" `shouldThrow` anyIOException
      -- Ids repeat across tabs (slugified template titles), so a sent tab that holds the id wins.
      _ <- testServant tr $ Dashboards.dashboardYamlPutH testPid dashId Dashboards.YamlForm{Dashboards.yaml = "title: Dup\nwidgets: []\ntabs:\n  - name: First Tab\n    widgets:\n      - { type: stat, id: dup, title: One, layout: { x: 0, y: 0, w: 3, h: 3 } }\n  - name: Second Tab\n    widgets:\n      - { type: stat, id: dup, title: Two, layout: { x: 0, y: 0, w: 3, h: 3 } }\n"}
      void $ save "dup" (Just "second-tab") "Renamed Two"
      (,) <$> (map (.title) <$> tabWidgets tr dashId "first-tab") <*> (map (.title) <$> tabWidgets tr dashId "second-tab") >>= (`shouldBe` ([Just "One"], [Just "Renamed Two"]))

      groupDash <- newDashboard tr "overview.yaml" "Grouped"
      _ <- testServant tr $ Dashboards.dashboardYamlPutH testPid groupDash Dashboards.YamlForm{Dashboards.yaml = "title: Grouped\nwidgets:\n  - type: group\n    id: grp\n    title: Group\n    layout: { x: 0, y: 0, w: 12, h: 4 }\n    children:\n      - type: stat\n        id: inner\n        title: Inner\n        layout: { x: 0, y: 0, w: 3, h: 2 }\n"}
      void $ toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardWidgetPutH testPid groupDash (Just "inner") Nothing (widgetOf Widget.WTStat "Renamed Inner")
      map (.title) . foldMap (fold . (.children)) <$> storedWidgets tr groupDash >>= (`shouldBe` [Just "Renamed Inner"])

  -- Rendering, not persistence: a widget the canvas cannot address is a widget the next
  -- drag deletes, because the reorder patch is built by walking `#<id>_widgetEl` elements
  -- and the handler rebuilds the widget list purely from that patch.
  describe "Rendering a widget onto the canvas" do
    it "every widget type renders a grid item the canvas can address and position" \_ -> do
      for_ allWidgetTypes \wt -> do
        let w = (widgetOf wt "Canvas Widget"){Widget.id = Just "wgt-1", Widget.layout = Just def{Widget.x = Just 3, Widget.y = Just 2, Widget.w = Just 6, Widget.h = Just 4}}
            html = renderT $ Widget.widget_ w
            claim :: Text -> Bool -> IO ()
            claim what ok = (show @Text wt <> ": " <> what, ok) `shouldBe` (show @Text wt <> ": " <> what, True)

        claim "renders something" (not $ T.null html)
        -- These four are the exact contract web-components/src/widgets.ts relies on:
        -- buildWidgetOrder selects `:scope > .grid-stack-item`, reads the `_widgetEl` id
        -- suffix for the widget key, and GridStack hydrates position/size from gs-*. A type
        -- that renders without any one of them is invisible to the serializer, so the next
        -- drag sends a patch that omits it — and the reorder handler rebuilds the widget
        -- list purely from the patch, i.e. that widget is deleted.
        claim "is a grid item the canvas serializer selects" (T.isInfixOf "grid-stack-item" html)
        claim "has the _widgetEl handle buildWidgetOrder keys on" (T.isInfixOf "id=\"wgt-1_widgetEl\"" html)
        claim "declares its column position" (T.isInfixOf "gs-x=\"3\"" html && T.isInfixOf "gs-y=\"2\"" html)
        claim "declares both dimensions" (T.isInfixOf "gs-w=\"6\"" html && T.isInfixOf "gs-h=" html)

    -- A naked widget is rendered standalone (the expanded viewer, a shared link), where
    -- there is no grid to belong to. It must NOT emit a grid handle, or the canvas would
    -- try to position something that is not on it.
    it "a naked widget renders without grid chrome" \_ -> do
      let w = (widgetOf Widget.WTTimeseries "Standalone"){Widget.id = Just "wgt-1", Widget.naked = Just True}
          html = renderT $ Widget.widget_ w
      html `shouldNotSatisfy` T.isInfixOf "wgt-1_widgetEl"

    it "a group renders its children inside its own nested grid" \_ -> do
      let child = (widgetOf Widget.WTStat "Child"){Widget.id = Just "child-1"}
          groupWidget = (widgetOf Widget.WTGroup "Group"){Widget.id = Just "group-1", Widget.children = Just [child]}
          html = renderT $ Widget.widget_ groupWidget
      html `shouldContainAll` ["nested-grid", "group-1_widgetEl", "child-1_widgetEl"]

  -- Every drag and resize is persisted by one hidden form. A failure there loses the
  -- reader's work, and it used to do so silently: htmx swallows both a transport error and
  -- a throw out of `hx-vals` (buildWidgetOrder is undefined whenever the web-components
  -- bundle fails to load, e.g. a stale asset manifest after a deploy), and nothing in the
  -- app listened for either.
  describe "Saving the canvas layout" do
    it "dashboard pages omit the global service selector" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Dashboard Scope"
      (_, PageCtx bw _) <- testServant tr $ Dashboards.dashboardGetH testPid dashId Nothing Nothing Nothing Nothing Nothing []
      V.null bw.serviceOptions `shouldBe` True

    it "the widget-order form reports a failed save instead of losing it silently" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Save Failure Signal"
      (_, pg) <- testServant tr $ Dashboards.dashboardGetH testPid dashId Nothing Nothing Nothing Nothing Nothing []
      let rendered = case pg of PageCtx _ d -> renderT d
          formTag = T.takeWhile (/= '>') $ snd $ T.breakOn "id=\"widget-order-trigger\"" rendered

      -- It still saves through the widget-order endpoint, and says so when that fails.
      formTag `shouldContainAll` ["widgets_order", "htmx:responseError", "htmx:sendError", "errorToast"]

  describe "The add-widget experience" do
    it "widget chart thresholds are editable in Edit and Monitors" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Display Thresholds"
      (_, newEditor) <- testServant tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
      let newHtml = renderT newEditor
      newHtml `shouldContainAll` ["Chart thresholds", "Widget name", "page-drawer-tab-monitors", "Add widget and monitor"]
      snd (T.breakOn "Name your widget" newHtml) `shouldSatisfy` T.isInfixOf "Chart thresholds"

      let widget = (widgetOf Widget.WTTimeseries "Thresholded"){Widget.alertThreshold = Just 10, Widget.warningThreshold = Just 7, Widget.unit = Just "requests/s"}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing widget
      wid <- firstWidgetId =<< storedWidgets tr dashId
      (_, editEditor) <- testServant tr $ Dashboards.dashboardWidgetExpandGetH testPid dashId wid Nothing
      let editHtml = renderT editEditor
      T.take 120 (snd $ T.breakOn "group/thresholds" editHtml) `shouldSatisfy` T.isInfixOf "open"
      (_, monitorEditor) <- testServant tr $ Dashboards.dashboardWidgetExpandGetH testPid dashId wid (Just "alerts")
      T.take 180 (snd $ T.breakOn "page-drawer-tab-monitors" $ renderT monitorEditor) `shouldSatisfy` T.isInfixOf "checked"
      editHtml `shouldContainAll` ["Chart thresholds", "Monitors", "Measurement unit", "widgetJSON.alert_threshold", "widgetJSON.warning_threshold", "name=\"alertThreshold\"", "Create monitor"]
      editHtml `shouldNotSatisfy` T.isInfixOf "Save thresholds"
      fst (T.breakOn "widget-preview-container" editHtml) `shouldNotSatisfy` T.isInfixOf "Configure query"
      only <- onlyWidget =<< storedWidgets tr dashId
      (only.alertThreshold, only.warningThreshold, only.unit) `shouldBe` (Just 10, Just 7, Just "requests/s")

      tableDashId <- newDashboard tr "overview.yaml" "Table Without Thresholds"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid tableDashId Nothing Nothing (widgetOf Widget.WTTable "Rows")
      tableId <- firstWidgetId =<< storedWidgets tr tableDashId
      (_, tableEditor) <- testServant tr $ Dashboards.dashboardWidgetExpandGetH testPid tableDashId tableId Nothing
      renderT tableEditor `shouldNotSatisfy` T.isInfixOf ">Chart thresholds</strong>"

      sqlDashId <- newDashboard tr "overview.yaml" "SQL Chart Thresholds"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid sqlDashId Nothing Nothing (widgetOf Widget.WTTimeseries "SQL Chart"){Widget.query = Nothing, Widget.sql = Just "SELECT 1"}
      sqlId <- firstWidgetId =<< storedWidgets tr sqlDashId
      (_, sqlEditor) <- testServant tr $ Dashboards.dashboardWidgetExpandGetH testPid sqlDashId sqlId Nothing
      let sqlHtml = renderT sqlEditor
      sqlHtml `shouldContainAll` ["Chart thresholds", "Monitors evaluate a KQL query"]

    -- Opening "add widget" on a dashboard must start on a chart. Logs is a full log
    -- table: it is the wrong thing to drop on a dashboard by default, and it is the
    -- most expensive one to render.
    it "defaults to a timeseries chart, not the logs table" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Add Widget Default"
      (_, html) <- testServant tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
      -- Assert on the seeded `widgetJSON` rather than on which radio carries `checked`:
      -- that object is what the save actually posts, and every tab's inline hyperscript
      -- mentions `checked` anyway, so the markup is not a reliable signal.
      let rendered = renderT html
          seededType = T.takeWhile (/= '"') <$> T.stripPrefix "\"type\":\"" (snd $ T.breakOn "\"type\":\"" rendered)

      seededType `shouldBe` Just "timeseries"

    -- Regression: the editor offered Patterns and Sessions tabs. Selecting one set
    -- widgetJSON.type to a string with no WidgetType constructor, so the widget could
    -- not be decoded on save — the tab looked available and simply did not work.
    it "offers only visualizations the API can actually store as a widget" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Add Widget Tabs"
      (_, html) <- testServant tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
      let rendered = renderT html
          offered = [T.takeWhile (/= '"') seg | seg <- drop 1 (T.splitOn "id=\"viz-" rendered)]
      offered `shouldSatisfy` (not . null)
      for_ offered \vizType ->
        (vizType, isRight (AE.eitherDecode @Widget.WidgetType (AE.encode vizType))) `shouldBe` (vizType, True)

  -- Regression: every KQL-backed widget (i.e. every chart in Log Explorer and most
  -- dashboard widgets) has Widget.sql = Nothing, since .sql is reserved for the raw-SQL
  -- data-source widget type. "Copy SQL" used to fall back to widgetData.query in that
  -- case, so it silently copied the KQL instead — see widgetSqlTextGetH.
  describe "Copy SQL widget menu action" do
    it "does not fall back to copying the KQL for a widget with no .sql field" \_ -> do
      let w = (widgetOf Widget.WTTimeseries "All traces"){Widget.id = Just "wgt-1", Widget._projectId = Just testPid}
          copySqlBlock = snd $ T.breakOn "Copy generated SQL to clipboard" $ renderT $ Widget.widget_ w
      copySqlBlock `shouldSatisfy` T.isInfixOf ("/p/" <> testPid.toText <> "/widget/sql-text?query=")
      copySqlBlock `shouldNotSatisfy` T.isInfixOf "widgetData.sql or widgetData.query"

    it "translates a KQL query into real SQL, not an echo of the input" \tr -> do
      (_, sql) <- testServant tr $ Dashboards.widgetSqlTextGetH testPid (Just "status_code == \"ERROR\"") Nothing Nothing Nothing Nothing
      T.toUpper sql `shouldSatisfy` T.isInfixOf "SELECT"
      sql `shouldNotBe` "status_code == \"ERROR\""

    it "copies SQL scoped to the authenticated environment and service" \tr -> do
      scopedSession <- checkoutSession tr
      let sqlFor dashboardIdM =
            Servant.getResponse
              <$> toBaseServantResponse
                tr
                ( atAuthToBase scopedSession
                    $ Dashboards.widgetSqlTextGetH testPid (Just "status_code == \"ERROR\"") dashboardIdM Nothing Nothing Nothing
                )
      sql <- sqlFor Nothing
      sql `shouldContainAll` ["resource___deployment___environment___name = 'production'", "resource___service___name = 'checkout'"]

      dashboardSql <- sqlFor $ Just "overview"
      dashboardSql `shouldSatisfy` T.isInfixOf "resource___deployment___environment___name = 'production'"
      dashboardSql `shouldNotSatisfy` T.isInfixOf "resource___service___name"

    it "copies the chart query with its category grouping" \tr -> do
      (_, sql) <- testServant tr $ Dashboards.widgetSqlTextGetH testPid (Just "summarize count(*) by bin_auto(timestamp), coalesce(status_code, level)") Nothing Nothing Nothing Nothing
      sql `shouldNotSatisfy` T.isInfixOf "jsonb_build_array"
      let groupBy = snd $ T.breakOn "GROUP BY" sql
      groupBy `shouldContainAll` ["status_code", "level"]

    it "reports a clear message instead of blank/failing when no query is given" \tr -> do
      (_, sql) <- testServant tr $ Dashboards.widgetSqlTextGetH testPid Nothing Nothing Nothing Nothing Nothing
      sql `shouldBe` "No query provided"

  -- A raw-SQL widget renders whatever its query selects, so the text decoder must accept
  -- whatever the column types happen to be. It used to demand `text` for every column, and
  -- postgresql-simple's FromField Text rejects anything else — so one uncast column failed
  -- the *whole* widget with `Incompatible {errSQLType = "int4", errHaskellType = "Text"}`
  -- and the user got an error overlay instead of their table.
  describe "Raw-SQL widget column types" do
    let runSql tr q = runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just testPid) Nothing (Just q) (Just "24H") Nothing Nothing Nothing Nothing []
        ingestScoped tr apiKey route env service traceIdValue spanId =
          ingestSpanReq tr $ mkSpanRequest traceIdValue spanId Nothing (route <> service) [] Nothing [] (mkResource apiKey [mkAttr "deployment.environment.name" env, mkAttr "service.name" service]) frozenTime

    it "renders a non-text column instead of failing the widget" \tr -> do
      md <- runSql tr "SELECT 1::int4 AS n, 'ok'::text AS s"
      md.error `shouldBe` Nothing
      md.dataText `shouldBe` V.singleton (V.fromList ["1", "ok"])

    it "accepts the column types a real widget actually selects" \tr -> do
      -- bigint/float8/bool/timestamptz all reached us uncast from user dashboards; the
      -- reported failure was date_part(EPOCH, time_bucket(...)), which comes back float8.
      md <- runSql tr "SELECT 42::bigint, 1.5::float8, true, '2026-01-02 03:04:05+00'::timestamptz"
      md.error `shouldBe` Nothing
      (md.dataText V.!? 0) `shouldSatisfy` \case
        Just row -> V.length row == 4 && V.head row == "42"
        Nothing -> False

    it "renders NULL as empty rather than failing the widget" \tr -> do
      md <- runSql tr "SELECT NULL::int4 AS n, 'after'::text AS s"
      md.error `shouldBe` Nothing
      md.dataText `shouldBe` V.singleton (V.fromList ["", "after"])

    -- Raw SQL has no general-purpose safe insertion point. Under the sticky
    -- environment selector, a template must explicitly reserve the generated
    -- AST predicate instead of silently returning every environment.
    it "rejects an unscoped raw-SQL template when an environment is selected" \tr -> do
      let sql = "SELECT 1::int4 AS n"
          scoped = [("environment", Just "production")]
          message = "This raw SQL widget must include {{query_ast_filters}} to respect the selected environment."
      complete <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing scoped
      complete.error `shouldBe` Just message
      response <- runQueryEffect tr $ Charts.queryMetricsStream Nothing (Just Charts.DTText) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing scoped
      frames <- streamFrames response
      frames `shouldSatisfy` \case
        Right values -> any (\case AE.Object obj -> KM.lookup "error" obj == Just (AE.String message); _ -> False) values
        Left _ -> False

    it "applies environment and service scope through a raw-SQL template" \tr -> do
      apiKey <- createTestAPIKey tr testPid "raw-template-scope"
      let ingest = ingestScoped tr apiKey "GET /raw/"
          raw = "SELECT resource___service___name FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND name LIKE 'GET /raw/%' ORDER BY 1"
          scope = [("environment", Just "production"), ("service", Just "checkout")]
      ingest "production" "checkout" "10000000000000000000000000000001" "1000000000000001"
      ingest "production" "catalog" "20000000000000000000000000000002" "2000000000000002"
      ingest "staging" "checkout" "30000000000000000000000000000003" "3000000000000003"
      scoped <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just testPid) Nothing (Just raw) (Just "24H") Nothing Nothing Nothing Nothing scope
      scoped.error `shouldBe` Nothing
      scoped.dataText `shouldBe` V.singleton (V.singleton "checkout")

    -- The SQL executor's tenant boundary was a check that `project_id='<pid>'` appeared somewhere
    -- in the text, so `... OR TRUE` read every project. Each tenant table must reach the
    -- statement already filtered to the project, on both stores; a statement that cannot be
    -- scoped (it reuses a tenant table's name for its own CTE) may fail instead.
    it "userSql_readsOnlyItsOwnProject_onPostgresAndTimefusion" \tr -> do
      otherPid <- createTestProject tr "Other tenant"
      mine <- createTestAPIKey tr testPid "tenant-scope-mine"
      theirs <- createTestAPIKey tr otherPid "tenant-scope-theirs"
      ingestTrace tr mine "GET /tenant-scope" frozenTime
      ingestTrace tr theirs "GET /tenant-scope" frozenTime
      let own = "project_id='" <> testPid.toText <> "'"
          projects = fmap \rows -> nubOrd [v | row <- V.toList rows, AE.String v <- V.toList row]
          run source sql = projects <$> runQueryEffect tr (LogQueries.executeSecuredQuery True testPid (LogQueries.SecuredSql source sql) 100)
      for_ [LogQueries.SqlPostgres, LogQueries.SqlTimefusion] \source -> do
        for_
          [ "SELECT project_id::text FROM otel_logs_and_spans WHERE " <> own <> " OR TRUE"
          , "SELECT p FROM (SELECT project_id::text AS p, project_id FROM otel_logs_and_spans) s WHERE " <> own <> " OR TRUE"
          , "SELECT project_id::text FROM otel_logs_and_spans WHERE name = 'GET /tenant-scope' AND (" <> own <> " OR project_id IN (SELECT project_id FROM otel_logs_and_spans))"
          ]
          \sql -> run source sql `shouldReturn` Right [testPid.toText]
        run source ("WITH otel_logs_and_spans AS (SELECT * FROM otel_logs_and_spans) SELECT project_id::text FROM otel_logs_and_spans WHERE " <> own <> " OR TRUE")
          >>= (`shouldSatisfy` either (const True) (== [testPid.toText]))

    -- A dashboard supplies its own service variable, while non-dashboard charts still use
    -- the sticky selector. Both keep the authenticated environment boundary.
    it "uses sticky service scope outside dashboards but not inside them" \tr -> do
      apiKey <- createTestAPIKey tr testPid "chart-route-scope"
      let ingest = ingestScoped tr apiKey "GET /chart-scope/"
          raw = "SELECT resource___service___name FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND name LIKE 'GET /chart-scope/%' ORDER BY 1"
      ingest "production" "checkout" "40000000000000000000000000000004" "4000000000000004"
      ingest "staging" "catalog" "50000000000000000000000000000005" "5000000000000005"
      ingest "production" "catalog" "60000000000000000000000000000006" "6000000000000006"
      scopedSession <- checkoutSession tr
      result <-
        toBaseServantResponse tr
          $ atAuthToBase scopedSession
          $ Routes.chartsDataGetH
            Nothing
            (Just Charts.DTText)
            (Just testPid)
            Nothing
            (Just raw)
            (Just "24H")
            Nothing
            Nothing
            Nothing
            Nothing
            [("environment", Just "staging"), ("service", Just "catalog")]
      result.error `shouldBe` Nothing
      result.dataText `shouldBe` V.singleton (V.singleton "checkout")
      dashboardResult <-
        toBaseServantResponse tr
          $ atAuthToBase scopedSession
          $ Routes.chartsDataGetH
            Nothing
            (Just Charts.DTText)
            (Just testPid)
            Nothing
            (Just raw)
            (Just "24H")
            Nothing
            Nothing
            Nothing
            Nothing
            [("dashboard_id", Just "overview"), ("environment", Just "staging"), ("service", Just "catalog")]
      dashboardResult.error `shouldBe` Nothing
      dashboardResult.dataText `shouldBe` V.fromList [V.singleton "catalog", V.singleton "checkout"]

    -- The same trap one decoder over: a plotted widget's SQL runs as DTMetric, whose leading
    -- column the pivot reads as an epoch number. `time_bucket('1m', timestamp) AS timestamp`
    -- — the obvious way to bucket by hand, and what the KQL builder writes before wrapping it
    -- in extract(epoch …) — is a timestamptz, and took the whole widget down with
    -- `Incompatible {errSQLType = "timestamptz", errSQLField = "timestamp", errHaskellType = "Int"}`.
    it "plots a timestamptz bucket column, not just an epoch number" \tr -> do
      let plotSql q = runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just q) (Just "24H") Nothing Nothing Nothing Nothing []
      stamped <- plotSql "SELECT '2026-01-02 03:04:05+00'::timestamptz AS timestamp, 'value'::text, 7::double precision"
      stamped.error `shouldBe` Nothing
      -- Epoch milliseconds, the same wire shape an epoch-number bucket lands in.
      (stamped.dataset V.!? 0 >>= (V.!? 0)) `shouldBe` Just (Just 1767323045000)
      epoch <- plotSql "SELECT 1767323045::int4 AS timestamp, 'value'::text, 7::double precision"
      epoch.dataset `shouldBe` stamped.dataset
      -- A first column that is neither still fails: there is nothing to plot a series against.
      wrong <- plotSql "SELECT 'not a bucket'::text, 'value'::text, 7::double precision"
      wrong.error `shouldSatisfy` isJust

  -- A dashboard variable declares the store its statement belongs to (`source: postgres`
  -- for apis.endpoints). The page render honoured it, but the client re-runs the same
  -- statement through /chart_data on every refresh and that carried no source — so it was
  -- planned against TimeFusion, which answers "table 'datafusion.apis.endpoints' not
  -- found", and the Endpoint Analytics picker silently stopped updating.
  describe "Client-supplied SQL routing" do
    let endpointsSql = "select hash::text, method || ' ' || url_path from apis.endpoints where project_id='{{project_id}}' limit 5"
    -- Through the route, not queryMetrics directly: the route is where the source was
    -- being dropped. (The guard the same route applies to client SQL is doctested on
    -- 'Web.Routes.clientPostgresSqlRejection'.)
    it "routes the statement at the store the variable declared" \tr -> do
      (_, md) <- testServant tr $ addRespHeaders =<< Routes.chartsDataGetH (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just endpointsSql) (Just "24H") Nothing Nothing Nothing Nothing []
      md.error `shouldBe` Nothing

    -- The issue volume chart's KQL scans every row of its window on TimeFusion (4-27s
    -- for a day in prod, 90s for three live hours on a busy project); a covered, unscoped
    -- window is answered from the hourly rollup instead.
    it "answers a covered window from the hourly rollup and an uncovered one from the query" \tr -> do
      now <- getTestTime tr.trTestClock
      _ <- withResource tr.trPool \conn -> PG.execute conn [SqlQQ.sql|INSERT INTO apis.log_pattern_hourly_stats (project_id, pattern_hash, hour_bucket, event_count) VALUES (?, 'rollup-route', date_trunc('hour', ?::timestamptz) - interval '2 hours', 7)|] (testPid, now)
      let rollup = Issues.hourlyRollupSql "apis.log_pattern_hourly_stats" "pattern_hash = 'rollup-route'"
          total coveredFor since = testServant tr (addRespHeaders =<< Routes.chartsDataGetH Nothing (Just Charts.DTMetric) (Just testPid) (Just "hashes[*]==\"pat:rollup-route\" | summarize count(*) by bin_auto(timestamp)") Nothing (Just since) Nothing Nothing Nothing Nothing [("rollup_sql", Just rollup), ("rollup_from", Just $ Utils.isoT $ addUTCTime (-coveredFor) now)]) <&> \(_, md) -> sum (md.dataset >>= V.catMaybes . V.drop 1)
      -- A window reaching before the rollup's coverage (pruned hours) must not read it as zero.
      (,,) <$> total 172800 "24H" <*> total 172800 "3H" <*> total 3600 "24H" >>= (`shouldBe` (7, 7, 0))

  describe "Scalar monitor charts" do
    it "scalarMonitor_plotsMaxPerBucketInsteadOfCountingTheAggregate" \tr -> do
      now <- getTestTime tr.trTestClock
      withResource tr.trPool \conn ->
        void
          $ PG.execute
            conn
            [SqlQQ.sql| INSERT INTO otel_metrics (project_id, timestamp, id, series_id, metric_name, metric_type, value)
                    VALUES (?, ?, gen_random_uuid(), 'monitor-chart', 'timefusion.mem_buffer.oldest_bucket_age_seconds', 'GAUGE', 42) |]
            (testPid, now)
      let widget = (widgetOf Widget.WTTimeseries "Query Results"){Widget.query = Just "metrics | where metric_name == \"timefusion.mem_buffer.oldest_bucket_age_seconds\" | summarize max(value)"}
          (query, decoder) = Widget.chartQuery widget
          rendered = renderT $ Widget.widget_ widget
      rendered `shouldContainAll` ["max(value) by bin_auto(timestamp)", "aria-label=\"View widget query\""]
      Widget.chartQuery widget{Widget.query = Just "metrics | where metric_name == \"summarize\""}
        `shouldBe` (Just "metrics | where metric_name == \"summarize\" | summarize count(*) by bin_auto(timestamp)", decoder)
      Widget.chartQuery widget{Widget.wType = Widget.WTStat, Widget.query = Just "metrics | where metric_name == \"summarize\""}
        `shouldBe` (Just "metrics | where metric_name == \"summarize\" | summarize count(*)", Charts.DTFloat)
      md <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just decoder) (Just testPid) query Nothing (Just "1H") Nothing Nothing Nothing Nothing []
      md.error `shouldBe` Nothing
      V.toList (md.dataset >>= V.catMaybes . V.drop 1) `shouldBe` [42]
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just decoder) (Just testPid) query Nothing (Just "1H") Nothing Nothing Nothing Nothing []
      response `completesWith` md

  describe "Streaming chart results" do
    let sql = "SELECT i::bigint, 'value'::text, i::double precision FROM generate_series(1, 3) i"
    it "emits partial data and the same final chart as the JSON endpoint" \tr -> do
      expected <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing []
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing []
      values <- streamFrames response >>= either fail pure
      length values `shouldSatisfy` (> 1)
      take 1 values `shouldSatisfy` all (\case AE.Object obj -> KM.lookup "type" obj == Just (AE.String "partial"); _ -> False)
      viaNonEmpty last values `shouldBe` Just (AE.object ["type" AE..= ("complete" :: Text), "data" AE..= expected])

    it "renders rounded numeric chart and stat values from widget SQL" \tr -> do
      let numericSql = "SELECT 1::bigint, 'cpu'::text, ROUND(AVG(1.25::numeric), 2)"
      expected <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just numericSql) (Just "24H") Nothing Nothing Nothing Nothing []
      expected.error `shouldBe` Nothing
      expected.dataset `shouldBe` V.singleton (V.fromList [Just 1000, Just 1.25])
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just numericSql) (Just "24H") Nothing Nothing Nothing Nothing []
      response `completesWith` expected
      let scalarSql = "SELECT ROUND(AVG(1.25::numeric), 2)"
      scalar <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just scalarSql) (Just "24H") Nothing Nothing Nothing Nothing []
      scalar.error `shouldBe` Nothing
      scalar.dataFloat `shouldBe` Just 1.25
      scalarResponse <- runQueryEffect tr $ Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just scalarSql) (Just "24H") Nothing Nothing Nothing Nothing []
      scalarResponse `completesWith` scalar

    it "discards a connection when a row consumer fails and keeps the pool usable" \tr -> do
      -- Two large rows flush a complete first row through PostgreSQL's socket buffer.
      -- The third row stays asleep until the consumer cancels the query.
      result <- timeout 3000000 $ E.try @E.IOException $ withResource tr.trPool \conn ->
        Charts.streamQuery_ conn "SELECT i::bigint, repeat('x', 10000) FROM generate_series(1, 3) i CROSS JOIN LATERAL pg_sleep(CASE WHEN i <= 2 THEN 0 ELSE 30 END)" \(_ :: (Int64, Text)) -> E.throwIO $ userError "consumer stopped"
      result `shouldSatisfy` maybe False isLeft
      withResource tr.trPool (\conn -> PG.query_ conn "SELECT 42::bigint") `shouldReturn` [PG.Only (42 :: Int64)]

    it "ends decoder failures with an error frame" \tr -> do
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) Nothing (Just "SELECT 'wrong type'::text, 'value'::text, 1::double precision") (Just "24H") Nothing Nothing Nothing Nothing []
      frames <- streamFrames response
      frames `shouldSatisfy` \case
        Right [AE.Object obj] -> KM.lookup "type" obj == Just (AE.String "error")
        _ -> False

  -- Every /widget request carries its whole widget definition on the request line. For a
  -- table widget that is multi-KB of SQL, and past ~9.6KB nginx tears down the entire
  -- HTTP/2 connection rather than just that stream — so Cloudflare answered 520 for the
  -- oversized request *and* for every unrelated request multiplexed onto the same
  -- connection. That is what made dashboard charts fail in scattered, retry-able subsets
  -- ("Couldn't load this chart") with no query ever reaching the database.
  describe "Table sorting before the server limit" do
    it "groups metric resource extractions on both backends" \_ -> withTfChartResources \tr -> do
      pid <- createTestProject tr "Grouped metric resources"
      key <- createTestAPIKey tr pid "grouped-metrics"
      now <- getTestTime tr.trTestClock
      for_ ([(Just "alpha", 2), (Just "alpha", 4), (Just "beta", 7), (Nothing, 9), (Nothing, 11)] :: [(Maybe Text, Double)]) \(container, value) ->
        ingestMetric tr key (maybe [] (pure . mkAttr "container.name") container) [] "grouped.metric" value now
      tfUrl <- lookupEnv "TIMEFUSION_PG_TEST_URL"
      let query = "metrics | where metric_name == \"grouped.metric\" | summarize min(value), max(value) by resource.container.name"
          cell i value
            | i == 0 = AE.String value
            | otherwise = maybe (AE.String value) AE.toJSON $ readMaybe @Double $ toString value
          row label lo hi = V.fromList [AE.String label, AE.toJSON (lo :: Double), AE.toJSON (hi :: Double)]
      for_ (Hasql.SqlPostgres : [Hasql.SqlTimefusion | isJust tfUrl]) \db -> do
        withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache"
        result <- runQueryEffect tr $ Charts.queryMetrics (Just db) (Just Charts.DTText) (Just pid) (Just query) Nothing (Just "24h") Nothing Nothing Nothing Nothing []
        result.error `shouldBe` Nothing
        (V.imap cell <$> result.dataText) `shouldBe` V.fromList [row "null" 9 11, row "beta" 7 7, row "alpha" 2 4]

    it "counts paired and unpaired overview resource samples on both backends" \_ -> withTfChartResources \tr -> do
      pid <- createTestProject tr "Resource sample counts"
      key <- createTestAPIKey tr pid "resource-samples"
      now <- getTestTime tr.trTestClock
      forM_
        [ (service, metric, value, stamp)
        | (service, cpu, memory, stamp) <- [("paired", Just 0.25, Just 0.4, now), ("cpu-only", Just 0.75, Nothing, now), ("memory-only", Nothing, Just 0.6, now), ("expired", Just 0.9, Just 0.9, addUTCTime (-48 * 3600) now)]
        , (metric, value) <- catMaybes [("container.cpu.utilization",) <$> cpu, ("container.memory.percent",) <$> memory]
        ]
        $ \(service, metric, value, stamp) -> ingestMetric tr key [mkAttr "service.name" service] [] metric value stamp
      overview <- DashboardModel.readDashboardFile "static/public/dashboards" "_overview.yaml"
      resource <- widgetTitled "Service Resource Usage" $ foldMap DashboardModel.allWidgets overview
      tfUrl <- lookupEnv "TIMEFUSION_PG_TEST_URL"
      let (_, sql) = Widget.tableQuery resource Nothing
          row service cpu memory points = V.fromList [AE.String service, AE.toJSON (cpu :: Maybe Double), AE.toJSON cpu, AE.toJSON (memory :: Maybe Double), AE.toJSON memory, AE.toJSON (points :: Int)]
          cell i value
            | i == 0 = AE.String value
            | T.null value = AE.Null
            | otherwise = maybe (AE.String value) AE.toJSON $ readMaybe @Double $ toString value
      for_ (Hasql.SqlPostgres : [Hasql.SqlTimefusion | isJust tfUrl]) \db -> do
        withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache"
        result <- runQueryEffect tr $ Charts.queryMetrics (Just db) (Just Charts.DTText) (Just pid) Nothing sql (Just "24h") Nothing Nothing Nothing Nothing []
        result.error `shouldBe` Nothing
        (V.imap cell <$> result.dataText) `shouldBe` V.fromList [row "cpu-only" (Just 75) Nothing 1, row "paired" (Just 25) (Just 0.4) 2, row "memory-only" Nothing (Just 0.6) 1]

    it "renders the overview resource table through the dashboard JSON API" \tr -> do
      overview <- DashboardModel.readDashboardFile "static/public/dashboards" "_overview.yaml"
      resource <- widgetTitled "Service Resource Usage" $ foldMap DashboardModel.allWidgets overview
      dashId <- newDashboard tr "" "Resource table API"
      let widget = resource{Widget.id = Just "resources", Widget.dbSource = Just Hasql.SqlPostgres}
      void $ toBaseServantResponse tr $ Api.apiDashboardWidgetUpsert testPid dashId widget
      now <- getTestTime tr.trTestClock
      withResource tr.trPool \conn ->
        forM_
          [ (testPid, stamp, service, metric, value, service)
          | (service, cpu, memory, stamp) <- [("backend" :: Text, 0.25, 0.4, now), ("frontend", 0.5, 0.8, now), ("expired", 0.9, 0.9, addUTCTime (-48 * 3600) now)]
          , (metric, value) <- [("container.cpu.utilization" :: Text, cpu), ("container.memory.percent", memory :: Double)]
          ]
          $ \row ->
            void
              $ PG.execute
                conn
                [SqlQQ.sql| INSERT INTO otel_metrics (project_id, timestamp, id, series_id, metric_name, metric_type, value, resource___service___name)
                        VALUES (?, ?, gen_random_uuid(), ?, ?, 'GAUGE', ?, ?) |]
                row
      let backend = ["backend", "25.0", "25.0", "0.4", "0.4", "2"]
          frontend = ["frontend", "50.0", "50.0", "0.8", "0.8", "2"]
      forM_ [(Nothing :: Maybe Text, [frontend, backend]), (Just "+service", [backend, frontend]), (Just "-service", [frontend, backend]), (Just "-service; SELECT 1", [frontend, backend])] \(sortParam, expected) -> do
        result <- toBaseServantResponse tr $ Api.apiDashboardData testPid dashId Nothing (Just "resources") (Just "24h") Nothing Nothing [("table-sort", sortParam)]
        let widgets = result.widgets :: [ApiTypes.RenderedWidget]
        map (.error) widgets `shouldBe` [Nothing]
        concatMap (.textRows) widgets `shouldBe` expected
        map (.columns) widgets `shouldBe` [["Service/Container", "CPU Avg", "CPU Max", "Mem Avg", "Mem Max", "Samples"]]

    it "returns rows outside the original top twenty and keeps numbers numeric" \tr -> do
      let widget =
            (def :: Widget.Widget)
              { Widget.wType = Widget.WTTable
              , Widget.dbSource = Just Hasql.SqlPostgres
              , Widget.columns = Just [def{Widget.field = "duration", Widget.title = "Duration", Widget.sortable = Just True}]
              , Widget.sql = Just "SELECT n AS duration FROM generate_series(1, 25) n ORDER BY {{table_sort}} LIMIT 20"
              , Widget.defaultSort = Widget.mkSqlOrder "duration ASC"
              }
          fetch sortParam = runQueryEffect tr do
            let (query, sql) = Widget.tableQuery widget sortParam
            Charts.queryMetrics widget.dbSource (Just Charts.DTText) (Just testPid) query sql Nothing Nothing Nothing Nothing Nothing []
      original <- fetch Nothing
      sorted <- fetch (Just "-duration")
      rejected <- fetch (Just "-duration; DROP TABLE users")
      original.dataText `shouldBe` V.fromList [V.singleton $ show @Text n | n <- [1 .. 20 :: Int]]
      sorted.dataText `shouldBe` V.fromList [V.singleton $ show @Text n | n <- [25, 24 .. 6 :: Int]]
      rejected.dataText `shouldBe` original.dataText

    it "resolves table sorting in the eager widget endpoint" \tr -> do
      let widget =
            (def :: Widget.Widget)
              { Widget.wType = Widget.WTTable
              , Widget.dbSource = Just Hasql.SqlPostgres
              , Widget.columns = Just [def{Widget.field = "duration", Widget.title = "Duration", Widget.sortable = Just True}]
              , Widget.sql = Just "SELECT n AS duration FROM generate_series(1, 25) n ORDER BY {{table_sort}} LIMIT 20"
              , Widget.defaultSort = Widget.mkSqlOrder "duration ASC"
              }
          fetch params = testServant tr $ Routes.widgetGetH testPid (Just $ Utils.encodeText widget) Nothing Nothing Nothing Nothing params
      (_, initial) <- fetch []
      (_, descending) <- fetch [("table-sort", Just "-duration")]
      (initial.html, descending.html)
        `shouldSatisfy` \case
          (Just ascHtml, Just descHtml) -> T.isInfixOf ">1<" (toStrict ascHtml) && T.isInfixOf ">25<" (toStrict descHtml)
          _ -> False

    it "exercises ascending and descending server sorts for every built-in sortable table column" \tr -> do
      templates <- DashboardModel.readDashboardsFromDisk "static/public/dashboards"
      let cases =
            [ col
            | w <- foldMap DashboardModel.allWidgets templates
            , w.wType == Widget.WTTable
            , col <- fromMaybe [] w.columns
            , col.sortable == Just True
            ]
          fetch widget sortParam = runQueryEffect tr do
            let (query, sql) = Widget.tableQuery widget sortParam
            Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) query sql Nothing Nothing Nothing Nothing Nothing []
      cases `shouldSatisfy` not . null
      forM_ cases \col -> do
        let quoted = "\"" <> T.replace "\"" "\"\"" col.field <> "\""
            widget =
              (def :: Widget.Widget)
                { Widget.wType = Widget.WTTable
                , Widget.columns = Just [col]
                , Widget.sql = Just $ "SELECT n AS " <> quoted <> " FROM generate_series(1, 25) n ORDER BY {{table_sort}} LIMIT 20"
                , Widget.defaultSort = Widget.mkSqlOrder "abs(n - 13) ASC"
                }
        original <- fetch widget Nothing
        ascending <- fetch widget (Just $ "+" <> col.field)
        descending <- fetch widget (Just $ "-" <> col.field)
        V.length ascending.dataText `shouldBe` 20
        V.length descending.dataText `shouldBe` 20
        ascending.dataText `shouldBe` V.fromList [V.singleton $ show @Text n | n <- [1 .. 20 :: Int]]
        descending.dataText `shouldBe` V.fromList [V.singleton $ show @Text n | n <- [25, 24 .. 6 :: Int]]
        V.any (`notElem` original.dataText) ascending.dataText `shouldBe` True
        V.any (`notElem` original.dataText) descending.dataText `shouldBe` True

  describe "Widget fetch URL size" do
    let bigSqlWidget =
          (def :: Widget.Widget)
            { Widget.wType = Widget.WTTable
            , Widget.title = Just "Query Optimization Targets"
            , Widget.sql = Just $ "SELECT " <> T.intercalate ", " [T.replicate 4 "attributes___db___system___name" <> " AS c" <> show n | n <- [1 :: Int .. 100]] <> " FROM otel_logs_and_spans"
            , Widget._projectId = Just testPid
            }

    it "keeps a table widget's URL far below the proxy's header limit" \_ -> do
      let raw = "/p/" <> testPid.toText <> "/widget?widgetJSON=" <> Utils.toUriStr (Utils.encodeText bigSqlWidget)
      -- The shape that broke: uncompressed, this widget alone overruns the limit.
      T.length raw `shouldSatisfy` (> 9600)
      T.length (Widget.widgetFetchUrl bigSqlWidget) `shouldSatisfy` (< 4000)

    it "round-trips the widget through the compressed parameter the URL now uses" \tr -> do
      let widgetZ = snd $ T.breakOnEnd "widgetZ=" $ Widget.widgetFetchUrl bigSqlWidget
      (_, got) <- testServant tr $ Routes.widgetGetH testPid Nothing (Just widgetZ) (Just "24H") Nothing Nothing []
      got.sql `shouldBe` bigSqlWidget.sql
      got.title `shouldBe` bigSqlWidget.title

    it "rejects a corrupt blob instead of decompressing something arbitrary" \_ ->
      Widget.decodeWidgetZ "not-a-gzip-blob" `shouldReturn` Nothing

  -- Every widget once pinned kind=="server", so the ~56% of a project's endpoints that
  -- are outgoing calls (apis.endpoints.outgoing, discovered from client spans) rendered an
  -- entirely empty dashboard: "0 reqs", "No data in this time range", while the log
  -- explorer showed the traffic. Reported by a customer for dellyman.com/api/v3.0/GetQuotes.
  describe "Endpoint Analytics covers outgoing endpoints" do
    let outgoingHash = "ba3431d5"
        seedClientSpan tr = withResource tr.trPool \conn ->
          PG.execute
            conn
            [SqlQQ.sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, date, name, kind, status_code, duration, hashes, summary)
                  VALUES (gen_random_uuid(), ?, ?, ?, ?, 'POST', 'client', '200', 5000000, ARRAY[?], ARRAY['POST']) |]
            (testPid.toText, frozenTime, frozenTime, frozenTime, outgoingHash)

    it "returns the endpoint's spans when they are client spans" \tr -> do
      void $ seedClientSpan tr
      md <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) (Just $ "hashes[*]==\"" <> outgoingHash <> "\" | summarize count() by bin_auto(timestamp)") Nothing (Just "24H") Nothing Nothing Nothing Nothing []
      md.error `shouldBe` Nothing
      V.length md.dataset `shouldSatisfy` (> 0)
      -- The filter that caused the blank dashboard: it matches nothing for this endpoint.
      excluded <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) (Just $ "kind==\"server\" AND hashes[*]==\"" <> outgoingHash <> "\" | summarize count() by bin_auto(timestamp)") Nothing (Just "24H") Nothing Nothing Nothing Nothing []
      V.length excluded.dataset `shouldBe` 0

    it "ships a template that does not filter the endpoint's widgets by span kind" \_ -> do
      template <- decodeUtf8 <$> readFileBS "static/public/dashboards/endpoint-stats.yaml"
      template `shouldSatisfy` T.isInfixOf "{{const-endpointFilter}}"
      template `shouldNotSatisfy` T.isInfixOf "kind"

    it "uses the route with a path fallback for literal endpoints and retains hashes for templates" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint selectors"
      let endpoint hash path =
            (def :: Endpoints.Endpoint)
              { Endpoints.projectId = testPid
              , Endpoints.host = "integrations.routelift.com"
              , Endpoints.method = "POST"
              , Endpoints.urlPath = path
              , Endpoints.hash = hash
              , Endpoints.outgoing = True
              }
          filters hash = do
            response <- getTab tr dashId "overview" (Just "div#dashboard-tabs-content") [("var-host", Just "integrations.routelift.com"), ("var-endpointHash", Just hash)]
            let Dashboards.DashboardGet _ _ _ _ params = tabDashboard response
                values = Map.fromList params
            pure (Map.lookup "const-endpointFilter" values, Map.lookup "const-endpointFilter-kql" values)
          hostless :: Endpoints.Endpoint
          hostless = (endpoint "hostless-route" "/health"){Endpoints.host = ""}
      runQueryEffect tr $ Endpoints.bulkInsertEndpoints $ V.fromList [endpoint "literal-route" "/v1/deliveries/estimate/v2", endpoint "template-route" "/v1/deliveries/{id}", hostless]
      filters "literal-route"
        `shouldReturn` ( Just (Just "attributes___server___address IN ('integrations.routelift.com') AND attributes___http___request___method IN ('POST') AND (attributes___http___route IN ('/v1/deliveries/estimate/v2') OR ((attributes___http___route IS NULL OR attributes___http___route = '') AND attributes___url___path IN ('/v1/deliveries/estimate/v2')))")
                       , Just (Just "attributes.server.address in (\"integrations.routelift.com\") AND attributes.http.request.method in (\"POST\") AND (attributes.http.route in (\"/v1/deliveries/estimate/v2\") or (isempty(attributes.http.route) and attributes.url.path in (\"/v1/deliveries/estimate/v2\")))")
                       )
      filters "template-route"
        `shouldReturn` ( Just (Just "(attributes___server___address IN ('integrations.routelift.com') AND attributes___http___request___method IN ('POST') AND attributes___http___route IN ('/v1/deliveries/{id}') OR hashes @> ARRAY['template-route'])")
                       , Just (Just "(attributes.server.address in (\"integrations.routelift.com\") AND attributes.http.request.method in (\"POST\") AND attributes.http.route in (\"/v1/deliveries/{id}\") or hashes[*] in (\"template-route\"))")
                       )
      filters "hostless-route" `shouldReturn` (Just (Just "hashes @> ARRAY['hostless-route']"), Just (Just "hashes[*] in (\"hostless-route\")"))
      filters "" `shouldReturn` (Just (Just "hashes @> ARRAY['']"), Just (Just "hashes[*] in (\"\")"))
      for_ ["literal-route", "template-route", "hostless-route", ""] \hash -> do
        (_, kql) <- filters hash
        join kql `shouldSatisfy` maybe False (isRight . parseQueryToAST)

    it "recomputes endpoint filters when the dashboard URL carries stale constants" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint selectors"
      runQueryEffect tr
        $ Endpoints.bulkInsertEndpoints
        $ V.singleton
          (def :: Endpoints.Endpoint)
            { Endpoints.projectId = testPid
            , Endpoints.host = "integrations.routelift.com"
            , Endpoints.method = "POST"
            , Endpoints.urlPath = "/v1/deliveries/estimate/v2"
            , Endpoints.hash = "faa041ff"
            , Endpoints.outgoing = True
            }
      (_, response) <-
        testServant tr
          $ Dashboards.dashboardTabGetH
            testPid
            dashId
            "overview"
            Nothing
            Nothing
            Nothing
            (Just "3D")
            (Just "div#dashboard-tabs-content")
            [ ("const-endpointFilter", Just "hashes @> ARRAY['']")
            , ("const-endpointFilter-kql", Just "hashes[*] in (\"\")")
            , ("var-endpointHash", Just "faa041ff")
            , ("var-host", Just "integrations.routelift.com")
            ]
      let Dashboards.DashboardGet _ _ _ _ params = tabDashboard response
      [value | (key, value) <- params, key == "const-endpointFilter"]
        `shouldBe` [Just "attributes___server___address IN ('integrations.routelift.com') AND attributes___http___request___method IN ('POST') AND (attributes___http___route IN ('/v1/deliveries/estimate/v2') OR ((attributes___http___route IS NULL OR attributes___http___route = '') AND attributes___url___path IN ('/v1/deliveries/estimate/v2')))"]
      [value | (key, value) <- params, key == "const-endpointFilter-kql"]
        `shouldBe` [Just "attributes.server.address in (\"integrations.routelift.com\") AND attributes.http.request.method in (\"POST\") AND (attributes.http.route in (\"/v1/deliveries/estimate/v2\") or (isempty(attributes.http.route) and attributes.url.path in (\"/v1/deliveries/estimate/v2\")))"]

    -- Shipbubble 2026-10-19: 17 MySQL errors in 1.4M queries showed as "0.00%", and the
    -- tab had no way to read the error itself.
    it "databasesTab_rareErrors_areCountedAndReadable" \tr -> do
      let dbTab = [t | d <- dashboardTemplatesCompiled, d.file == Just "_overview.yaml", t <- fold d.tabs, t.name == "Databases"]
          widgets = concatMap (\w -> w : fold w.children) (concatMap (.widgets) dbTab)
          errTable = find ((== Just "Recent Database Errors") . (.title)) widgets
      [c.field | w <- widgets, c <- fold w.columns, c.field == "error_rate"] `shouldBe` []
      pid <- createTestProject tr "db-tab-errors"
      apiKey <- createTestAPIKey tr pid "db-tab-key"
      ingestSpanReq tr =<< createOtelTraceWithExceptionAtTime apiKey ([("db.system.name", "mysql"), ("db.query.text", "update orders set x = ?")], [], []) "update orders" "QueryError" "Deadlock found when trying to get lock; try restarting transaction" "" frozenTime
      md <- eventually (runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just pid) Nothing (snd . (`Widget.tableQuery` Nothing) =<< errTable) (Just "24H") Nothing Nothing Nothing Nothing [("var-database", Just "mysql")]) (not . V.null . (.dataText))
      md.error `shouldBe` Nothing
      V.toList (V.take 2 <$> md.dataText) `shouldBe` [V.fromList ["Deadlock found when trying to get lock; try restarting transaction", "update orders set x = ?"]]

    it "copyable cells get a copy button; rows that go nowhere do not look clickable" \_ -> do
      let column = (def :: Widget.TableColumn){Widget.field = "q", Widget.title = "Query", Widget.copyable = Just True}
          html = renderT $ Widget.renderTableWithDataAndParams (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.columns = Just [column]} (V.singleton $ V.singleton "select 1") []
      html `shouldContainAll` ["data-copy=\"select 1\"", "navigator.clipboard.writeText"]
      snd (T.breakOn "<tbody" html) `shouldSatisfy` (not . T.isInfixOf "cursor-pointer")

    it "renders a dashboard table link with the current project and URL-encoded row value" \_ -> do
      let column = (def :: Widget.TableColumn){Widget.field = "session_id", Widget.title = "Session", Widget.link = Just "/p/{{project_id}}/log_explorer?viz_type=sessions&query={{row.session_id}}"}
          widget = (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.columns = Just [column], Widget._projectId = Just testPid}
          html = renderT $ Widget.renderTableWithDataAndParams widget (V.singleton $ V.singleton "session / id") []
      html `shouldSatisfy` T.isInfixOf ("/p/" <> testPid.toText <> "/log_explorer?viz_type=sessions&amp;query=session%20%2F%20id")

    -- Regression: rows on a tabless dashboard had no swap target and ignored clicks.
    it "tableRowClick_navigatesOnTablessAndTabbedDashboards" \_ -> do
      let column = (def :: Widget.TableColumn){Widget.field = "service", Widget.title = "Service"}
          rowNav tabM =
            renderT
              $ Widget.renderTableWithDataAndParams
                (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.columns = Just [column], Widget._projectId = Just testPid, Widget._dashboardId = Just "d1", Widget._tabSlug = tabM, Widget.onRowClick = Just def{Widget.setVariable = Just "service"}}
                (V.singleton $ V.singleton "api")
                [("var-env", Just "prod")]
          dashUrl = "/p/" <> testPid.toText <> "/dashboards/d1"
      rowNav Nothing `shouldContainAll` ["hx-get=\"" <> dashUrl <> "?var-env=prod&amp;var-service=api\"", "hx-target=\"#main-content\"", "hx-select=\"#main-content\""]
      rowNav (Just "overview") `shouldContainAll` ["hx-get=\"" <> dashUrl <> "/tab/overview?var-env=prod&amp;var-service=api\"", "hx-target=\"#dashboard-tabs-content\""]
      rowNav (Just "overview") `shouldSatisfy` (not . T.isInfixOf "hx-select=")

    it "renders a scoped dependency map with an endpoint-preserving Service Map link" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint dependency map"
      dg <- tabDashboard <$> getTab tr dashId "dependencies" Nothing [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "endpoint / hash")]
      let mapWidget = find ((== Widget.WTServiceMap) . (.wType)) (renderedWidgets dg)
      (mapWidget >>= (.cta) <&> (.title)) `shouldBe` Just "Open Service Map"
      (mapWidget >>= (.cta) <&> (.url)) `shouldBe` Just ("/p/" <> testPid.toText <> "/service_map?endpoint_hash=endpoint%20%2F%20hash&since=24H")
      (mapWidget >>= (.html)) `shouldSatisfy` maybe False (T.isInfixOf "No direct dependencies in this range" . toStrict)

  describe "Endpoint Analytics complete-result SQL cache" do
    it "reuses a successful complete result and isolates endpoint keys" \tr -> do
      withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache WHERE source = 'raw-sql'"
      let sql = "SELECT count(*)::double precision FROM query_cache WHERE source = 'raw-sql'"
          fetch endpoint =
            runQueryEffect tr
              $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing [("var-endpointHash", Just endpoint)]
      initial <- fetch "endpoint-a"
      repeated <- fetch "endpoint-a"
      otherEndpoint <- fetch "endpoint-b"
      initial.dataFloat `shouldBe` Just 0
      repeated.dataFloat `shouldBe` initial.dataFloat
      otherEndpoint.dataFloat `shouldBe` Just 1

    it "shares complete results between streaming and JSON requests" \tr -> do
      withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache WHERE source = 'raw-sql'"
      let sql = "SELECT count(*)::double precision FROM query_cache WHERE source = 'raw-sql'"
          params = [("var-endpointHash", Just "stream-cache")]
          fetch = runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing params
          stream = completeFrame @Charts.MetricsData =<< runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing params)
      initialStream <- stream
      cachedJson <- fetch
      repeatedStream <- stream
      map (.dataFloat) [initialStream, cachedJson, repeatedStream] `shouldBe` replicate 3 (Just 0)
      rows <- withResource tr.trPool \conn -> PG.query_ conn "SELECT count(*)::bigint FROM query_cache WHERE source = 'raw-sql'" :: IO [PG.Only Int]
      rows `shouldBe` [PG.Only 1]

    it "expires fixed-window results so late telemetry can replace a cached result" \tr -> E.bracket (getTestTime tr.trTestClock) (setTestTime tr.trTestClock) \_ -> do
      withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache WHERE source = 'raw-sql'"
      let fetch = runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just "SELECT count(*)::double precision FROM query_cache WHERE source = 'raw-sql'") Nothing (Just "2025-01-01T00:00:00Z") (Just "2025-01-02T00:00:00Z") Nothing Nothing [("var-endpointHash", Just "expiry")]
      initial <- fetch
      advanceTestTime tr 59
      fresh <- fetch
      advanceTestTime tr 2
      expired <- fetch
      map (.dataFloat) [initial, fresh, expired] `shouldBe` [Just 0, Just 0, Just 1]
      advanceTestTime tr 601
      runQueryEffect tr QueryCache.cleanupExpiredCache `shouldReturn` 1

    it "reuses the default live range when time parameters are omitted" \tr -> E.bracket (getTestTime tr.trTestClock) (setTestTime tr.trTestClock) \_ -> do
      withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache WHERE source = 'raw-sql'"
      let fetch = runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just "SELECT count(*)::double precision FROM query_cache WHERE source = 'raw-sql'") Nothing Nothing Nothing Nothing Nothing [("var-endpointHash", Just "default-range")]
      -- Both snapshots are within the same fifteen-second cache boundary.
      initial <- fetch
      advanceTestTime tr 0.001
      repeated <- fetch
      repeated.dataFloat `shouldBe` initial.dataFloat

    it "shares a query until its cache write finishes" \tr -> do
      let setup = withResource tr.trPool \conn -> do
            void $ PG.execute_ conn "DELETE FROM query_cache WHERE source = 'raw-sql'; CREATE SEQUENCE cache_race_calls"
            void $ PG.execute_ conn "CREATE FUNCTION slow_cache_write() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_sleep(0.5); RETURN NEW; END $$; CREATE TRIGGER slow_cache_write BEFORE INSERT ON query_cache FOR EACH ROW EXECUTE FUNCTION slow_cache_write()"
          cleanup = withResource tr.trPool \conn -> void $ PG.execute_ conn "DROP TRIGGER slow_cache_write ON query_cache; DROP FUNCTION slow_cache_write(); DROP SEQUENCE cache_race_calls"
          fetch = runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just "SELECT nextval('cache_race_calls')::double precision") (Just "24H") Nothing Nothing Nothing Nothing [("var-endpointHash", Just "write-race")]
          waitForWrite = do
            sleeping <- withResource tr.trPool \conn -> PG.query_ conn "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE datname = current_database() AND wait_event = 'PgSleep')" :: IO [PG.Only Bool]
            unless (sleeping == [PG.Only True]) $ threadDelay 10000 >> waitForWrite
      E.bracket_ setup cleanup $ withAsync fetch \leader -> do
        timeout 5000000 waitForWrite `shouldReturn` Just ()
        follower <- fetch
        original <- wait leader
        map (.dataFloat) [original, follower] `shouldBe` [Just 1, Just 1]

    it "returns results when the cache is unavailable and excludes oversized entries" \tr -> do
      let fetch sql = runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing [("var-endpointHash", Just "cache-fallback")]
          rename sql = withResource tr.trPool \conn -> void $ PG.execute_ conn sql
      E.bracket_ (rename "ALTER TABLE query_cache RENAME TO unavailable_query_cache") (rename "ALTER TABLE unavailable_query_cache RENAME TO query_cache") do
        result <- fetch "SELECT 'still available'::text"
        result.error `shouldBe` Nothing
        result.dataText `shouldBe` V.singleton (V.singleton "still available")
      rename "DELETE FROM query_cache WHERE source = 'raw-sql'"
      large <- fetch "SELECT repeat('x', 1048576)"
      large.error `shouldBe` Nothing
      V.map (V.map T.length) large.dataText `shouldBe` V.singleton (V.singleton 1048576)
      rows <- withResource tr.trPool \conn -> PG.query_ conn "SELECT count(*)::bigint FROM query_cache WHERE source = 'raw-sql'" :: IO [PG.Only Int]
      rows `shouldBe` [PG.Only 0]

    it "measures cold and cached requests with one backend execution per window" \tr -> do
      let execute statement = withResource tr.trPool \conn -> void $ PG.execute_ conn statement
          sql = "SELECT q.call::text, n::text, repeat('x', 80) FROM (SELECT nextval('cache_benchmark_calls') AS call, pg_sleep(0.1)) q CROSS JOIN generate_series(1, 1000) n"
          params = [("var-endpointHash", Just "benchmark")]
          fetch window streaming =
            if streaming
              then completeFrame @Charts.MetricsData =<< runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just sql) (Just window) Nothing Nothing Nothing Nothing params)
              else runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just sql) (Just window) Nothing Nothing Nothing Nothing params
          measured action = do
            start <- getMonotonicTime
            result <- action
            end <- getMonotonicTime
            pure ((end - start) * 1000, result)
      E.bracket_ (execute "CREATE SEQUENCE cache_benchmark_calls") (execute "DROP SEQUENCE cache_benchmark_calls")
        $ forM_ ["24H", "3D", "7D"] \window -> do
          execute "DELETE FROM query_cache WHERE source = 'raw-sql'; ALTER SEQUENCE cache_benchmark_calls RESTART WITH 1"
          (coldMs, cold) <- measured $ fetch window True
          cold.error `shouldBe` Nothing
          V.length cold.dataText `shouldBe` 1000
          warm <- traverse (measured . fetch window . even) [1 .. 10 :: Int]
          forM_ warm $ \(_, result) -> result.dataText `shouldBe` cold.dataText
          calls <- withResource tr.trPool (\conn -> PG.query_ conn "SELECT last_value FROM cache_benchmark_calls") :: IO [PG.Only Int]
          calls `shouldBe` [PG.Only 1]
          putTextLn
            $ "endpoint_cache_benchmark "
            <> Utils.encodeText
              ( AE.object
                  [ "window" AE..= window
                  , "requests" AE..= (11 :: Int)
                  , "backend_queries" AE..= (1 :: Int)
                  , "avoided_queries" AE..= (10 :: Int)
                  , "hit_rate" AE..= (10 / 11 :: Double)
                  , "result_bytes" AE..= LBS.length (AE.encode cold)
                  , "cold_ms" AE..= coldMs
                  , "cached_ms" AE..= map fst warm
                  ]
              )

    it "never stores failed endpoint SQL" \tr -> do
      withResource tr.trPool \conn -> void $ PG.execute_ conn "DELETE FROM query_cache WHERE source = 'raw-sql'"
      failed <-
        runQueryEffect tr
          $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just "SELECT missing_cache_column::double precision") (Just "24H") Nothing Nothing Nothing Nothing [("var-endpointHash", Just "endpoint-failure")]
      failed.error `shouldSatisfy` isJust
      rows <- withResource tr.trPool \conn -> PG.query_ conn "SELECT count(*)::bigint FROM query_cache WHERE source = 'raw-sql'" :: IO [PG.Only Int]
      rows `shouldBe` [PG.Only 0]

  -- The variable picker replaces the tab's content, so every widget the render produced
  -- was thrown away. The gate lived in the view, so the handler ran the whole widget
  -- phase first — with the required variable interpolated to '', which for Endpoint
  -- Analytics means hashes[*]=="" scanning until the render budget kills it. The picker,
  -- the first screen a user of such a dashboard ever sees, cost as much as the fully
  -- populated dashboard (5.4s vs 5.5s measured against a customer project).
  describe "Dashboards prompting for a required variable" do
    let
      -- `eager` alone proves nothing: the template declares it. Server work shows up as
      -- rendered html or a fetched dataset.
      prefilled :: Widget.Widget -> Bool
      prefilled w = isJust w.html || isJust w.dataset
      openTab tr dashId = fmap tabDashboard . getTab tr dashId "overview" Nothing

    it "skips the widget phase whose results the picker would discard" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint Analytics"
      -- host is set, endpointHash is not: the picker is what renders.
      gated <- openTab tr dashId [("var-host", Just "dellyman.com")]
      let ws = renderedWidgets gated
      -- Not vacuous: the tab really does carry widgets, and the picker is what renders.
      ws `shouldSatisfy` not . null
      map (fromMaybe "<untitled>" . (.title)) (filter prefilled ws) `shouldBe` []
      renderT gated `shouldSatisfy` T.isInfixOf "var-picker"

    it "successive required pickers stay inside the dashboard fragment" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Successive variable pickers"
      runQueryEffect tr
        $ Endpoints.bulkInsertEndpoints
        $ V.singleton
        $ (def :: Endpoints.Endpoint)
          { Endpoints.projectId = testPid
          , Endpoints.urlPath = "/orders"
          , Endpoints.method = "GET"
          , Endpoints.host = "dellyman.com"
          , Endpoints.hash = "ba3431d5"
          }
      let open = getTab tr dashId "overview" (Just "div#dashboard-tabs-content")
      firstHtml <- partialHtml =<< open []
      firstHtml `shouldSatisfy` T.isInfixOf "Select Domain"
      firstHtml `shouldContainAll` ["hx-target=\"#dashboard-tabs-content\"", "hx-swap=\"outerMorph\"", "hx-push-url=\"true\"", "name=\"dashboard-variable-option\""]
      firstHtml `shouldNotSatisfy` T.isInfixOf "hx-select=\"#main-content\""

      secondHtml <- partialHtml =<< open [("var-endpointHash", Just ""), ("var-host", Just "dellyman.com")]
      secondHtml `shouldContainAll` ["Select Endpoint", "id=\"dashboard-tabs-content\""]

    it "dashboardTab_variableControlsReflectURLValues" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Tab variable controls"
      html <- partialHtml =<< getTab tr dashId "errors" (Just "div#dashboard-tabs-content") [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "ba3431d5")]
      html `shouldContainAll` ["id=\"dashboard-variables\"", "hx-swap-oob=\"true\"", "value=\"dellyman.com\"", "value=\"ba3431d5\""]

  -- Picking a second domain left the Endpoint dropdown listing the first domain's
  -- endpoints. The input carries its own statement so the client can re-fetch options
  -- on change, but the server rendered it already substituted — the old host frozen in —
  -- so every re-fetch asked for the old domain again.
  describe "Dependent variable keeps a live template" do
    it "endpointHash_parentHostChanges_dropdownRefetchesForNewHost" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Dependent Vars"
      html <- renderT . tabDashboard <$> getTab tr dashId "overview" Nothing [("var-host", Just "dellyman.com")]
      html `shouldSatisfy` T.isInfixOf "{{var-host}}"
      html `shouldNotSatisfy` T.isInfixOf "host=&#39;dellyman.com&#39;"

  -- A swap already has a painted page around it, so it ships skeletons that fetch
  -- themselves rather than blocking on the widget phase (measured 4.9s -> 1.2s).
  describe "Prefill on a full load, skeletons on a swap" do
    let openTab tr dashId hx = getTab tr dashId "overview" hx [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "ba3431d5")]
        eagerOnes = filter (\w -> w.eager == Just True || isJust w.html || isJust w.dataset)

    it "keeps the server-side prefill for a full page load" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Prefill Full"
      full <- openTab tr dashId Nothing
      eagerOnes (renderedWidgets $ tabDashboard full) `shouldSatisfy` not . null
      T.count "id=\"log_details_container\"" (renderT full) `shouldBe` 1

    it "hands a swap the same widgets with nothing prefilled" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Prefill Swap"
      swapped <- openTab tr dashId (Just "div#dashboard-tabs-content")
      let ws = renderedWidgets $ tabDashboard swapped
      ws `shouldSatisfy` not . null
      -- lazyWidget strips `eager` too: renderStatContent reads the flag as "data is here"
      -- and would otherwise spin forever.
      map (fromMaybe "<untitled>" . (.title)) (eagerOnes ws) `shouldBe` []

    it "dashboardTab_sidebarNavigationFromSessions_keepsMainContent" \tr -> do
      dashId <- newDashboard tr "rum.yaml" "RUM sidebar navigation"
      (_, response) <- testServant tr $ Dashboards.dashboardTabGetH testPid dashId "overview" Nothing Nothing Nothing (Just "24H") (Just "div#main-content") []
      case response of
        NavigationFull page -> do
          let html = renderT page
          html `shouldSatisfy` T.isInfixOf "id=\"main-content\""
          html `shouldSatisfy` T.isInfixOf "id=\"dashboardPage\""
        NavigationPartial _ _ -> expectationFailure "sidebar navigation returned a tab fragment, blanking the page"

    it "returns the shared fragment contract for an htmx tab navigation" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Partial navigation"
      html <- partialHtml =<< getTab tr dashId "errors" (Just "div#dashboard-tabs-content") [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "ba3431d5")]
      html `shouldContainAll` ["id=\"dashboard-tabs-content\"", "id=\"dashboard-tabs-container\"", "hx-swap-oob=\"outerMorph\"", "id=\"dashboard-variables\"", "tab-active", "hx-push-url=\"true\""]
      html `shouldNotSatisfy` T.isInfixOf "<html"

  -- RUM is this template plus the Explorer's sessions view; the template must hold up on
  -- real browser telemetry, not just parse.
  describe "Real User Monitoring dashboard" do
    -- No var-app in the request, as on a first visit: an unset variable must read as empty
    -- (every application), not as the literal placeholder that matched nothing.
    it "rumDashboard_everyWidgetRuns_andVitalsReadHistogramBuckets" \tr -> do
      projectId <- createTestProject tr "RUM dashboard template"
      apiKey <- createTestAPIKey tr projectId "rum-dashboard-key"
      let browserSpan n name attrs ua = ingestSpanReq tr $ mkSpanRequest ("e" <> T.justifyRight 31 '0' (show @Text n)) ("e" <> T.justifyRight 15 '0' (show @Text n)) Nothing name [] Nothing (mkAttr "session.id" ("dash-session-" <> show n) : attrs) (mkResource apiKey [mkAttr "service.name" "storefront", mkAttr "user_agent.original" ua]) frozenTime
      browserSpan (1 :: Int) "documentLoad" [mkAttr "url.full" "https://shop.example/cart"] "Mozilla/5.0 (X11; Linux x86_64) Chrome/151"
      browserSpan 2 "Pageview · /orders/123" [mkAttr "url.path" "/orders/123", mkAttr "exception.type" "TypeError"] "Mozilla/5.0 (iPhone; CPU iPhone OS 17_0 like Mac OS X) Mobile/15E148 Safari/604.1"
      -- Three DELTA exports merge to [3,9,3,1,0] over 16 observations; the 12th lands on the
      -- 2500 edge of (1000, 2500], so P75 is exactly 2500 and rated Good from its bucket alone.
      let at = (`addUTCTime` frozenTime)
          nanos = floor . (* 1000000000) . utcTimeToPOSIXSeconds
          point end start counts = defMessage & PMF.timeUnixNano .~ nanos (at end) & PMF.startTimeUnixNano .~ nanos (at start) & PMF.attributes .~ [mkAttr "page.url" "https://shop.example/checkout"] & PMF.count .~ sum counts & PMF.bucketCounts .~ counts & PMF.explicitBounds .~ [1000, 2500, 4000, 8000]
          exportVital name points = void $ OtlpServer.metricsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ defMessage & MSF.resourceMetrics .~ [defMessage & PMF.resource .~ mkResource apiKey [mkAttr "service.name" "storefront"] & PMF.scopeMetrics .~ [defMessage & PMF.metrics .~ [defMessage & PMF.name .~ ("browser.web_vital." <> name) & PMF.histogram .~ (defMessage & PMF.aggregationTemporality .~ PM.AGGREGATION_TEMPORALITY_DELTA & PMF.dataPoints .~ points) :: PM.Metric]]])
      exportVital "lcp" [point (-30) (-40) [2, 5, 1, 0, 0], point (-20) (-30) [0, 3, 2, 1, 0], point (-10) (-20) [1, 1, 0, 0, 0 :: Word64]]
      -- INP's P75 (rank 3 of 4) sits in (100, 300], straddling the 200 ms threshold but narrow
      -- enough to estimate: 100 + 200·3/4. CLS in OTel's default (0, 5] bucket cannot be placed.
      exportVital "inp" [point (-10) (-20) [0, 4, 0, 0] & PMF.explicitBounds .~ [100, 300, 600]]
      exportVital "cls" [point (-10) (-20) [0, 4, 0] & PMF.explicitBounds .~ [0, 5]]
      rum <- rumTemplate
      let from = toText $ iso8601Show $ at (-3600)
          to = toText $ iso8601Show $ at 3600
          dataType = \case Widget.WTTable -> Charts.DTText; Widget.WTStat -> Charts.DTFloat; _ -> Charts.DTMetric
          run ws = forM ws \w -> (w.title,) <$> runQueryEffect tr (Charts.queryMetrics Nothing (Just $ dataType w.wType) (Just projectId) w.query w.sql Nothing (Just from) (Just to) Nothing Nothing [])
          widgetNamed t = widgetTitled t (DashboardModel.allWidgets rum)
          table rs title = maybe V.empty (.dataText) $ lookup (Just title) rs
          vitalRow rs name = V.toList . V.take 5 <$> find ((== Just name) . (V.!? 0)) (table rs "Core Web Vitals")
      results <- run [w | w <- DashboardModel.allWidgets rum, isJust w.sql]
      [(title, m.error) | (title, m) <- results, isJust m.error] `shouldBe` []
      vitalRow results "LCP" `shouldBe` Just ["LCP", "2500 ms", "rating;success-strong⇒Good", "1000 – 2500", "16"]
      vitalRow results "INP" `shouldBe` Just ["INP", "≈ 250 ms", "rating;warning-weak⇒Likely needs improvement", "100 – 300", "4"]
      vitalRow results "CLS" `shouldBe` Just ["CLS", "—", "rating;info-weak⇒Buckets too coarse to rate", "0 – 5", "4"]
      -- A vital with nothing in the window says so instead of vanishing.
      vitalRow results "FCP" `shouldBe` Just ["FCP", "—", "rating;info-weak⇒No observations in this window", "", "0"]
      -- Abbreviations explain themselves on hover: the row's hidden description on the cell,
      -- the column's on its header.
      vitals <- widgetNamed "Core Web Vitals"
      let vitalsHtml = renderT $ Widget.renderTableWithDataAndParams vitals (table results "Core Web Vitals") []
      vitalsHtml `shouldContainAll` ["data-tippy-content=\"Largest Contentful Paint: when the main content becomes visible.", "aria-description=\"Real-user observations in the selected window.\""]
      vitalsHtml `shouldNotSatisfy` T.isInfixOf ">About<"
      -- An SDK upgrade mixes bucket layouts: a [2500]-bounded export coarsens every point to
      -- the shared 2500 edge, giving [16, 4] over 20, so the 15th observation sits at 2500·15/16.
      exportVital "lcp" [point (-5) (-10) [4, 0] & PMF.explicitBounds .~ [2500]]
      mixed <- run [vitals]
      vitalRow mixed "LCP" `shouldBe` Just ["LCP", "2344 ms", "rating;success-strong⇒Good", "0 – 2500", "20"]
      sort [name | row <- V.toList $ table results "Devices", Just name <- [row V.!? 0]] `shouldBe` ["Desktop", "Mobile"]
      sort (V.toList $ V.mapMaybe (V.!? 0) $ table results "Top pages") `shouldBe` ["/cart", "/orders/:id"]
      -- Each page row's hidden Explorer query finds the events it counted, and the rendered
      -- link carries it with the dashboard's window while the column itself stays off screen.
      forM_ (table results "Top pages") \row -> do
        (_, events) <- testServant tr $ Log.logExplorerDataH projectId def{Log.query = row V.!? 3, Log.from = Just from, Log.to = Just to}
        (row V.!? 0, events.error, events.queryResultCount) `shouldBe` (row V.!? 0, Nothing, 1)
      -- The browser filter exists twice, as SQL for widgets and KQL for the Explorer; both must
      -- select the same rows, and a backend span neither.
      ingestTrace tr apiKey "GET /backend-only" frozenTime
      (_, sessions) <- testServant tr $ Log.logExplorerDataH projectId def{Log.query = Just browserKql, Log.from = Just from, Log.to = Just to}
      viaSql <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTFloat) (Just projectId) Nothing (Just "SELECT count(*)::float FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND {{browser_filter}}") Nothing (Just from) (Just to) Nothing Nothing []
      (sessions.error, sessions.queryResultCount, viaSql.dataFloat) `shouldBe` (Nothing, 2, Just 2)
      topPages <- widgetNamed "Top pages"
      let html = renderT $ Widget.renderTableWithDataAndParams topPages{Widget._projectId = Just projectId} (table results "Top pages") [("since", Just "7D")]
      html `shouldNotSatisfy` T.isInfixOf "Explorer query"
      map (\ps -> (lookup "since" ps, lookup "query" ps)) (linkParams ("/p/" <> projectId.toText <> "/log_explorer") html)
        `shouldBe` [(Just "7D", row V.!? 3) | row <- V.toList $ table results "Top pages"]

    it "audience_countsEachSessionOnce_keepsUnknown_andFiltersCohorts" \tr -> do
      pid <- createTestProject tr "RUM audience"
      apiKey <- createTestAPIKey tr pid "audience-key"
      let emit n session ua attrs = ingestSpanReq tr $ mkSpanRequest ("a" <> T.justifyRight 31 '0' (show @Text n)) ("a" <> T.justifyRight 15 '0' (show @Text n)) Nothing "documentLoad" [] Nothing (mkAttr "session.id" session : attrs) (mkResource apiKey $ mkAttr "service.name" "storefront" : [mkAttr "user_agent.original" agent | agent <- toList ua]) frozenTime
      emit (1 :: Int) "safari" (Just "iPhone Mobile Safari/604") [mkAttr "user.id" "alice", mkAttr "user.email" "alice@example.com"]
      emit 2 "safari" (Just "iPhone Mobile Safari/604") [mkAttr "user.id" "alice", mkAttr "exception.type" "TypeError"]
      emit 3 "chrome" (Just "Windows Chrome/151 Safari/537") [mkAttr "user.id" "bob"]
      emit 4 "unknown" Nothing []
      emit 5 "" (Just "Windows Chrome/151") [mkAttr "exception.type" "TypeError"]
      rum <- rumTemplate
      let from = toText $ iso8601Show $ addUTCTime (-3600) frozenTime
          to = toText $ iso8601Show $ addUTCTime 3600 frozenTime
          readTable params title = do
            w <- widgetTitled title (DashboardModel.allWidgets rum)
            result <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just pid) w.query w.sql Nothing (Just from) (Just to) Nothing Nothing params
            result.error `shouldBe` Nothing
            pure result.dataText
      browsers <- readTable [] "Browsers"
      sort (map (V.toList . V.take 5) $ V.toList browsers) `shouldBe` [["Chrome", "1", "33.3", "0", "0.0"], ["Safari", "1", "33.3", "1", "100.0"], ["Unknown", "1", "33.3", "0", "0.0"]]
      filtered <- readTable [("var-browser", Just "Safari")] "Devices"
      map (V.toList . V.take 5) (V.toList filtered) `shouldBe` [["Mobile", "1", "100.0", "1", "100.0"]]
      users <- readTable [("var-errors", Just "true")] "Identified users"
      map (V.toList . V.take 3) (V.toList users) `shouldBe` [["alice@example.com", "1", "1"]]
      forM_ ([([("var-browser", Just "Safari")], 2), ([("var-os", Just "iOS"), ("var-device", Just "Mobile")], 2), ([("var-browser", Just "Unknown")], 1), ([("var-errors", Just "true")], 2)] :: [([(Text, Maybe Text)], Int)]) \(params, expected) -> do
        query <- (lookup "query" <=< listToMaybe) . explorerLinkParams pid <$> rumLocation tr pid ["sessions"] params
        (_, events) <- testServant tr $ Log.logExplorerDataH pid def{Log.query = query, Log.from = Just from, Log.to = Just to}
        (events.error, events.queryResultCount) `shouldBe` (Nothing, expected)
      readTable [("var-app", Just "missing-app")] "Browsers" `shouldReturn` V.empty
      readTable [("environment", Just "prod")] "Browsers" `shouldReturn` V.empty
      found <- readTable [("table-search", Just "ALICE")] "Identified users"
      map (V.toList . V.take 3) (V.toList found) `shouldBe` [["alice@example.com", "1", "1"]]
      readTable [("table-search", Just "missing-user")] "Identified users" `shouldReturn` V.empty

    -- /rum and its older /rum/dashboard open the dashboard; /rum/sessions the Explorer. Links from
    -- emails and alerts carry a window, which both must keep.
    it "rumRedirects_openTheDashboardOrSessions_andKeepTheWindow" \tr -> do
      let location = rumLocation tr testPid
          window = [("since", Just "7D"), ("environment", Just "prod")]
      dashboard <- location [] window
      dashboard `shouldContainAll` ["/p/" <> testPid.toText <> "/dashboards/", "since=7D"]
      location ["dashboard"] window `shouldReturn` dashboard
      sessions <- location ["sessions"] window
      -- A trailing slash captures an empty last segment; it must not turn into a 404.
      location ["sessions", ""] window `shouldReturn` sessions
      explorerLinkParams testPid sessions `shouldBe` [[("since", "7D"), ("viz_type", "sessions"), ("query", browserKql)]]
      -- A link shared from the old page lands on that session, not the dashboard.
      legacy <- location [] [("tab", Just "sessions"), ("session", Just "abc")]
      explorerLinkParams testPid legacy `shouldBe` [[("viz_type", "sessions"), ("query", "attributes.session.id == \"abc\"")]]
