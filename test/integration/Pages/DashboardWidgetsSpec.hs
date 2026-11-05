-- | The dashboard canvas, end to end: every widget type through add, edit, move, resize,
-- drop and reload; the SQL a widget runs, its scoping and its cache; and the templates
-- (Endpoint Analytics, RUM, Service Overview) that ship on top of them.
module Pages.DashboardWidgetsSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception qualified as E
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.ProtoLens (defMessage)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Types (Query (..))
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (catchError)
import Lucid (Html, toHtml, toHtmlRaw)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as ApiIssues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.DashboardTemplates (dashboardTemplatesCompiled, loadDashboardFromVM)
import Models.Projects.Dashboards (DashboardVM (savedViews, schema))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (Session (..))
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (BWConfig (..), NavigationResponse (..), PageCtx (..))
import Pages.BodyWrapper qualified as BodyWrapper
import Pages.Charts.Types (MetricsStats (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards qualified as Dashboards
import Pages.Issues qualified as Issues
import Pages.LogExplorer.Log qualified as Log
import Pages.Projects qualified as ManageMembers
import Pkg.Components.LogQueryBox (VizSurface (..), VizType (..), visTypes)
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (mkHasqlPool)
import Pkg.Parser (TemplateLang (..), browserKql, fillQueryTemplate, parseQueryToAST, variablePresets)
import Pkg.QueryCache qualified as QueryCache
import Pkg.SchemaLearning qualified as SchemaLearning
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics qualified as PM
import Proto.Opentelemetry.Proto.Metrics.V1.Metrics_Fields qualified as PMF
import Servant qualified
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import System.Config qualified as Config
import System.Directory (withCurrentDirectory)
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Test.Hspec (anyIOException)
import Text.Slugify (slugify)
import UnliftIO.Async (mapConcurrently_, wait, withAsync)
import Utils qualified
import Web.ApiHandlers qualified as Api
import Web.ApiTypes qualified as ApiTypes
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)
import Web.FormUrlEncoded (urlDecodeAsForm)
import Web.Routes qualified as Routes
import "cryptonite" Crypto.Hash qualified as Crypto


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
getTab tr dashId slug hx params = call tr (Dashboards.dashboardTabGetH testPid dashId slug Nothing Nothing Nothing (Just "24H") hx params)


tabsContent :: Maybe Text
tabsContent = Just "div#dashboard-tabs-content"


partialHtml :: NavigationResponse a -> IO (Html ())
partialHtml = \case
  NavigationPartial _ fragment -> pure fragment
  NavigationFull _ -> expectationFailure "htmx navigation returned a full document" $> pass


rumLocation :: TestResources -> Projects.ProjectId -> [Text] -> [(Text, Maybe Text)] -> IO Text
rumLocation tr pid view ps =
  runAuthHandler tr (Dashboards.rumRedirectH pid view ps) >>= \h -> case lookupResponseHeader @"Location" h of
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


-- | Raw widget SQL through the chart pipeline over the last 24h, as the browser fetches it.
sqlChart :: TestResources -> Maybe Hasql.SqlSource -> Charts.DataType -> Text -> [(Text, Maybe Text)] -> IO Charts.MetricsData
sqlChart tr src dt q ps = runQueryEffect tr $ Charts.queryMetrics src (Just dt) (Just testPid) Nothing (Just q) (Just "24H") Nothing Nothing Nothing Nothing ps


sqlStream :: TestResources -> Maybe Hasql.SqlSource -> Charts.DataType -> Text -> [(Text, Maybe Text)] -> IO (Servant.Headers '[Servant.Header "Cache-Control" Text, Servant.Header "X-Accel-Buffering" Text] (Servant.SourceIO AE.Value))
sqlStream tr src dt q ps = runQueryEffect tr $ Charts.queryMetricsStream src (Just dt) (Just testPid) Nothing (Just q) (Just "24H") Nothing Nothing Nothing Nothing ps


-- A stream's last frame carries the same result the JSON endpoint returns.
completesWith :: Servant.Headers hs (Servant.SourceIO AE.Value) -> Charts.MetricsData -> Expectation
completesWith response md = (fmap (viaNonEmpty last) <$> streamFrames response) `shouldReturn` Right (Just $ AE.object ["type" AE..= ("complete" :: Text), "data" AE..= md])


allWidgetTypes :: [Widget.WidgetType]
allWidgetTypes = [minBound .. maxBound]


dashboardList :: TestResources -> Maybe Text -> IO Dashboards.DashboardsGet
dashboardList tr copyWidget = call tr $ Dashboards.dashboardsGetH testPid Nothing (Just "true") Nothing copyWidget Nothing Nothing (Dashboards.DashboardFilters [])


-- | Create a dashboard and hand back its id, deleting any other first: the test UUID
-- interpreter restarts per request, so a second create would mint the same id.
newDashboard :: TestResources -> Text -> Text -> IO DashboardModel.DashboardId
newDashboard tr file title = do
  let ids = dashboardList tr Nothing <&> \case Dashboards.DashboardsGetSlim d -> V.toList d.dashboards; Dashboards.DashboardsGet (PageCtx _ d) -> V.toList d.dashboards
  ids >>= traverse_ \d -> testServant tr $ Dashboards.dashboardDeleteH testPid d.id
  _ <- testServant tr $ Dashboards.dashboardsPostH testPid Dashboards.DashboardForm{Dashboards.title = title, Dashboards.file = file, Dashboards.teams = [], Dashboards.fileDir = Nothing}
  ids >>= \case
    [d] -> pure d.id
    ds -> fail $ "expected one dashboard, got " <> show (length ds)


-- | The root widgets a dashboard stores: what the next page load renders.
storedWidgets :: TestResources -> DashboardModel.DashboardId -> IO [Widget.Widget]
storedWidgets tr dashId = call tr (Dashboards.getDashAndVM testPid dashId Nothing >>= addRespHeaders . (.widgets) . snd)


widgetOf :: Widget.WidgetType -> Text -> Widget.Widget
widgetOf wt title =
  (def :: Widget.Widget)
    { Widget.wType = wt
    , Widget.title = Just title
    , Widget.query = Just "name != null"
    , Widget.layout = Just def{Widget.w = Just 3, Widget.h = Just 3, Widget.x = Just 0, Widget.y = Just 0}
    }


-- In one request block: the test UUID interpreter restarts per request, so separately
-- added widgets would share an id, which cannot happen in production.
addWidgets :: TestResources -> DashboardModel.DashboardId -> [Widget.Widget] -> IO ()
addWidgets tr dashId ws = void $ testServant tr $ traverse_ (Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing) ws >> addRespHeaders ()


reorder :: [(Text, (Int, Int, Int, Int))] -> Map Text Dashboards.WidgetReorderItem
reorder ps = fromList [(wid, (def :: Dashboards.WidgetReorderItem){Dashboards.x = Just x, Dashboards.y = Just y, Dashboards.w = Just w, Dashboards.h = Just h}) | (wid, (x, y, w, h)) <- ps]


geometry :: Widget.Widget -> (Maybe Text, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
geometry w = (w.id, w.layout >>= (.x), w.layout >>= (.y), w.layout >>= (.w), w.layout >>= (.h))


tableWidget :: Widget.Widget
tableWidget =
  (def :: Widget.Widget)
    { Widget.wType = Widget.WTTable
    , Widget.dbSource = Just Hasql.SqlPostgres
    , Widget.columns = Just [def{Widget.field = "duration", Widget.title = "Duration", Widget.sortable = Just True}]
    , Widget.sql = Just "SELECT n AS duration FROM generate_series(1, 25) n ORDER BY {{table_sort}} LIMIT 20"
    , Widget.defaultSort = Widget.mkSqlOrder "duration ASC"
    }


spec :: Spec
spec = around withTestResources $ describe "Dashboard widgets" do
  it "canvas: every widget type survives add, edit, move, resize, drop and reload" \tr -> do
    dashId <- newDashboard tr "overview.yaml" "Widget Lifecycle"
    (_, PageCtx bw page) <- testServant tr $ Dashboards.dashboardGetH testPid dashId Nothing Nothing Nothing Nothing Nothing []
    V.null bw.serviceOptions `shouldBe` True
    -- Every drag persists through this form; htmx swallows a transport error and a throw out of
    -- hx-vals (a stale bundle), so it must surface both instead of losing the layout silently.
    goldenParts "dashboard-widget-order-form" $ tagsWith ["id=\"widget-order-trigger\""] $ renderT page
    addWidgets tr dashId $ [(widgetOf Widget.WTTimeseries ("w" <> show i)){Widget.layout = Nothing} | i <- [1 .. 5 :: Int]] <> [widgetOf wt (show wt) | wt <- allWidgetTypes]
    added <- storedWidgets tr dashId
    map (show @Text . (.wType)) added `shouldBe` replicate 5 "WTTimeseries" <> map show allWidgetTypes
    numbered@(w1 : _) <- maybe (fail "a stored widget has no id") pure $ traverse (.id) (take 5 added)
    let typed = mapMaybe (.id) (drop 5 added)
    -- Widthless widgets default to four per row.
    map (\(_, x, y, w, _) -> (x, y, w)) (geometry <$> Widget.normalizeWidgetLayouts (take 5 added))
      `shouldBe` [(Just 0, Just 0, Just 3), (Just 3, Just 0, Just 3), (Just 6, Just 0, Just 3), (Just 9, Just 0, Just 3), (Just 0, Just 1, Just 3)]

    -- Edit in place: an omitted query is retained, and the render-time tab slug is not stored.
    let edited = (widgetOf Widget.WTStat "renamed"){Widget.query = Nothing, Widget.rawQuery = Nothing, Widget._tabSlug = Just "stale-tab", Widget.layout = Just def{Widget.x = Just 6, Widget.y = Just 6, Widget.w = Just 3, Widget.h = Just 3}}
    _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just w1) Nothing edited
    after <- storedWidgets tr dashId
    [(w.title, show @Text w.wType, w.query, w._tabSlug, geometry w) | w <- after, w.id == Just w1]
      `shouldBe` [(Just "renamed", "WTStat", Just "name != null", Nothing, (Just w1, Just 6, Just 6, Just 3, Just 3))]
    length after `shouldBe` length added

    -- A patch built before an HTMX swap can name only stale ids; applying it would wipe the canvas.
    _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder [("not-a-real-widget", (0, 0, 3, 3))])
    map (.id) <$> storedWidgets tr dashId `shouldReturn` map (.id) after

    -- Every typed widget gets a position and size unique to its index; w1 moves to the
    -- origin (not an absent coordinate); w2..w5 are left out of the patch, so deleted.
    let placed = [(wid, ((i * 2) `mod` 12, i, 1 + (i `mod` 4), 2 + (i `mod` 3))) | (i, wid) <- zip [0 ..] typed]
    _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder $ (w1, (0, 0, 3, 3)) : placed)
    reloaded <- storedWidgets tr dashId
    sort (map geometry reloaded) `shouldBe` sort [(Just wid, Just x, Just y, Just w, Just h) | (wid, (x, y, w, h)) <- (w1, (0, 0, 3, 3)) : placed]
    sort (map (show @Text . (.wType)) reloaded) `shouldBe` sort ("WTStat" : map show allWidgetTypes)
    filter (`elem` drop 1 numbered) (mapMaybe (.id) reloaded) `shouldBe` []
    length (ordNub (mapMaybe (.id) reloaded)) `shouldBe` length reloaded

  it "tabs and groups: every write lands on the canvas it addresses; the list counts them" \tr -> do
    dashId <- newDashboard tr "overview.yaml" "Tabbed"
    let yaml = void . testServant tr . Dashboards.dashboardYamlPutH testPid dashId . Dashboards.YamlForm
        -- The base runner mints real UUIDs, so separate writes get distinct widget ids.
        write :: ATAuthCtx (RespHeaders a) -> IO ()
        write = void . toBaseServantResponse tr . atAuthToBase tr.trSessAndHeader
        save wid slug t = write $ Dashboards.dashboardWidgetPutH testPid dashId (Just wid) slug (widgetOf Widget.WTStat t)
        tabWidgets slug = call tr (Dashboards.getDashAndVM testPid dashId Nothing >>= addRespHeaders . snd) <&> \dash -> maybe [] (.widgets) $ find (\t -> slugify t.name == slug) (fold dash.tabs)
        titles slug = map (.title) <$> tabWidgets slug
    yaml "title: Tabbed\nwidgets: []\ntabs:\n  - name: First Tab\n    widgets:\n      - { type: timeseries, id: first-widget, title: First, layout: { x: 0, y: 0, w: 3, h: 3 } }\n  - name: Second Tab\n    widgets:\n      - { type: stat, id: second-widget, title: Second, layout: { x: 0, y: 0, w: 3, h: 3 } }\n"
    (,) <$> (map (.id) <$> tabWidgets "first-tab") <*> (map (.id) <$> tabWidgets "second-tab") `shouldReturn` ([Just "first-widget"], [Just "second-widget"])
    write $ Dashboards.dashboardWidgetPutH testPid dashId Nothing (Just "second-tab") (widgetOf Widget.WTStat "Added To Second")
    write $ Dashboards.dashboardDuplicateWidgetPostH testPid dashId "second-widget" Nothing
    write $ Dashboards.dashboardWidgetReorderPatchH testPid dashId (Just "first-tab") (reorder [("first-widget", (6, 4, 6, 2))])
    map (\w -> (w.layout >>= (.x), w.layout >>= (.w))) <$> tabWidgets "first-tab" `shouldReturn` [(Just 6, Just 6)]
    tabTwo <- tabWidgets "second-tab"
    (map (.title) tabTwo, length (ordNub $ mapMaybe (.id) tabTwo)) `shouldBe` ([Just "Second", Just "Added To Second", Just "Second (Copy)"], 3)

    -- The expand drawer saved without a tab slug, so a rename of a tab's widget hit the
    -- root list, matched nothing, and still toasted.
    save "second-widget" Nothing "Renamed Second"
    titles "second-tab" `shouldReturn` [Just "Renamed Second", Just "Added To Second", Just "Second (Copy)"]
    save "second-widget" (Just "first-tab") "Still Second"
    titles "second-tab" `shouldReturn` [Just "Still Second", Just "Added To Second", Just "Second (Copy)"]
    save "no-such-widget" Nothing "Ghost" `shouldThrow` anyIOException
    -- Ids repeat across tabs (slugified template titles), so a sent tab that holds the id wins.
    yaml "title: Dup\nwidgets: []\ntabs:\n  - name: First Tab\n    widgets:\n      - { type: stat, id: dup, title: One, layout: { x: 0, y: 0, w: 3, h: 3 } }\n  - name: Second Tab\n    widgets:\n      - { type: stat, id: dup, title: Two, layout: { x: 0, y: 0, w: 3, h: 3 } }\n"
    save "dup" (Just "second-tab") "Renamed Two"
    (,) <$> titles "first-tab" <*> titles "second-tab" `shouldReturn` ([Just "One"], [Just "Renamed Two"])
    yaml "title: Grouped\nwidgets:\n  - type: group\n    id: grp\n    title: Group\n    layout: { x: 0, y: 0, w: 12, h: 4 }\n    children:\n      - { type: stat, id: inner, title: Inner, layout: { x: 0, y: 0, w: 3, h: 2 } }\n"
    save "inner" Nothing "Renamed Inner"
    map (.title) . foldMap (fold . (.children)) <$> storedWidgets tr dashId `shouldReturn` [Just "Renamed Inner"]

    -- The list counted only root widgets, so a tabbed template (root `widgets: []`) showed 0.
    _ <- newDashboard tr "_overview.yaml" "Tabbed Count"
    list <- dashboardList tr Nothing
    picker <- dashboardList tr (Just "metric-widget")
    case list of
      Dashboards.DashboardsGetSlim d -> length (filter ((/= Widget.WTGroup) . (.wType)) $ foldMap DashboardModel.allWidgets $ V.toList d.dashboards >>= maybeToList . loadDashboardFromVM d.dashTemplates) `shouldSatisfy` (> 20)
      Dashboards.DashboardsGet _ -> expectationFailure "embedded list rendered a full page"
    -- Picker rows PUT to the widget upsert: a mutation is never a GET.
    renderT picker `shouldSatisfy` T.isInfixOf "hx-put=\"/p/"
    goldenHtml "dashboard-list" do toHtml list; toHtml picker

  it "rendering: every widget type is a canvas grid item; tables, fetch URLs, PNGs and wire tags hold" \tr -> do
    -- The canvas serializer (widgets.ts buildWidgetOrder) selects `.grid-stack-item`, keys on
    -- the `_widgetEl` id and reads gs-*; a type rendered without them is dropped by the next
    -- drag, since the reorder handler rebuilds the list from the patch.
    let canvas = [(widgetOf wt (show wt)){Widget.id = Just ("wgt-" <> show i), Widget._projectId = Just testPid, Widget.layout = Just def{Widget.x = Just 3, Widget.y = Just 2, Widget.w = Just 6, Widget.h = Just 4}} | (i, wt) <- zip [1 :: Int ..] allWidgetTypes]
        chart = widgetOf Widget.WTTimeseriesLine "requests"
        sqlWidget = chart{Widget.sql = Just "SELECT '<script>'", Widget.expandBtnFn = Just "/details"}
        metricCard = chart{Widget.title = Just "metric", Widget.layout = Just def{Widget.x = Just 0, Widget.y = Just 0, Widget.w = Just 2, Widget.h = Just 1}, Widget.expandBtnFn = Just "/details"}
        standalone = (widgetOf Widget.WTTimeseries "Standalone"){Widget.id = Just "naked-1", Widget.naked = Just True}
        groupW = (widgetOf Widget.WTGroup "Group"){Widget.id = Just "group-1", Widget.children = Just [(widgetOf Widget.WTStat "Child"){Widget.id = Just "child-1"}]}
        widthless = (widgetOf Widget.WTTimeseries "Widthless"){Widget.layout = Nothing}
        scalarMonitor = (widgetOf Widget.WTTimeseries "Query Results"){Widget.query = Just "metrics | where metric_name == \"timefusion.mem_buffer.oldest_bucket_age_seconds\" | summarize max(value)"}
        html = renderT . Widget.widget_
        gridTags = tagsWith ["class=\"grid-stack"] . html
    html sqlWidget `shouldSatisfy` T.isInfixOf "SELECT &#39;&lt;script&gt;&#39;"
    -- A naked widget (expanded viewer, shared link) is on no grid, so carries no grid chrome.
    gridTags standalone `shouldBe` []
    goldenParts "dashboard-widgets" $ foldMap gridTags (canvas <> [groupW, widthless]) <> [html chart{Widget.id = Just "requests-chart"}] <> elementsWith ["aria-label=\"Expand widget\"", "query-preview language"] (html sqlWidget)
    (\w -> (w.layout >>= (.w), w.layout >>= (.h))) <$> Widget.normalizeWidgetLayouts [metricCard] `shouldBe` [(Just 2, Just 3)]

    let column = def :: Widget.TableColumn
        table cols extra rows ps = Widget.renderTableWithDataAndParams ((def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.columns = Just cols, Widget._projectId = Just testPid} & extra) rows ps
        linked = table [column{Widget.field = "session_id", Widget.title = "Session", Widget.link = Just "/p/{{project_id}}/log_explorer?viz_type=sessions&query={{row.session_id}}"}] id (V.singleton $ V.singleton "session / id") []
        -- Rows on a tabless dashboard had no swap target and ignored clicks.
        rowNav tabM = table [column{Widget.field = "service", Widget.title = "Service"}] (\w -> w{Widget._dashboardId = Just "d1", Widget._tabSlug = tabM, Widget.onRowClick = Just def{Widget.setVariable = Just "service"}}) (V.singleton $ V.singleton "api") [("var-env", Just "prod")]
    renderT linked `shouldSatisfy` T.isInfixOf "query=session%20%2F%20id"
    goldenHtml "dashboard-widget-tables" do
      table [column{Widget.field = "q", Widget.title = "Query", Widget.copyable = Just True}] id (V.singleton $ V.singleton "select 1") []
      linked
      rowNav Nothing
      rowNav (Just "overview")

    -- The wire tag is in every stored dashboard YAML; renaming a constructor orphans them.
    map (fmap (show @Text) . AE.eitherDecode @Widget.WidgetType . AE.encode) allWidgetTypes `shouldBe` map (Right . show) allWidgetTypes
    -- The editor offered Patterns and Sessions, which no WidgetType decodes, so the widget could not save.
    [v.key | v <- visTypes, v.surface /= ExplorerOnly, isLeft (AE.eitherDecode @Widget.WidgetType (AE.encode v.key))] `shouldBe` []
    let literal = scalarMonitor{Widget.query = Just "metrics | where metric_name == \"summarize\""}
    map Widget.chartQuery [literal, literal{Widget.wType = Widget.WTStat}]
      `shouldBe` [(Just "metrics | where metric_name == \"summarize\" | summarize count(*) by bin_auto(timestamp)", Charts.DTMetric), (Just "metrics | where metric_name == \"summarize\" | summarize count(*)", Charts.DTFloat)]

    -- Past ~9.6KB nginx tore down the whole HTTP/2 connection, 520ing every multiplexed request.
    let bigSqlWidget = (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.title = Just "Query Optimization Targets", Widget._projectId = Just testPid, Widget.sql = Just $ "SELECT " <> T.intercalate ", " [T.replicate 4 "attributes___db___system___name" <> " AS c" <> show n | n <- [1 :: Int .. 100]] <> " FROM otel_logs_and_spans"}
    T.length ("/p/" <> testPid.toText <> "/widget?widgetJSON=" <> Utils.toUriStr (Utils.encodeText bigSqlWidget)) `shouldSatisfy` (> 9600)
    T.length (Widget.widgetFetchUrl bigSqlWidget) `shouldSatisfy` (< 4000)
    got <- call tr $ Routes.widgetGetH testPid Nothing (Just $ snd $ T.breakOnEnd "widgetZ=" $ Widget.widgetFetchUrl bigSqlWidget) (Just "24H") Nothing Nothing []
    (got.sql, got.title) `shouldBe` (bigSqlWidget.sql, bigSqlWidget.title)
    Widget.decodeWidgetZ "not-a-gzip-blob" `shouldReturn` Nothing

    -- statPng_rendersTheScalarInsteadOfAnEmptyChart: missing data, zero and a scalar differ.
    for_ [(profile, appearance) | profile <- [Nothing, Just Widget.PngSlack], appearance <- ["light", "dark"]] \(profile, appearance) -> do
      pngs <- forM [Nothing, Just 0, Just 44] \value -> do
        let widget =
              (def :: Widget.Widget)
                { Widget.wType = Widget.WTStat
                , Widget.pngProfile = profile
                , Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.value = value}
                }
            encoded = decodeUtf8 @Text $ AE.encode widget
            signature = Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded
        response <- toBaseServantResponse tr $ Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just signature) [("appearance", Just appearance)]
        let bytes = Servant.getResponse response
        LBS.take 8 bytes `shouldBe` "\137PNG\r\n\26\n"
        pure bytes
      length (ordNub pngs) `shouldBe` 3

    -- A signed table widget renders to a PNG that changes with its data, on both profiles.
    for_ [Nothing, Just Widget.PngSlack] \profile -> do
      pngs <- forM [2, 3 :: Int] \count -> do
        let encoded = decodeUtf8 @Text $ AE.encode (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.pngProfile = profile, Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.source = AE.toJSON ([[AE.String "events"], [AE.toJSON count]] :: [[AE.Value]])}}
        Servant.getResponse <$> toBaseServantResponse tr (Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just $ Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded) [])
      map (LBS.take 8) pngs `shouldBe` replicate 2 "\137PNG\r\n\26\n"
      length (ordNub pngs) `shouldBe` 2

    -- A table's loading shell defers its headings until data arrives.
    html (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.title = Just "Status Code Breakdown", Widget.columns = Just [def{Widget.field = "status_code", Widget.title = "Status Code"}, def{Widget.field = "count", Widget.title = "Count"}]} `shouldNotSatisfy` T.isInfixOf "<thead"

    -- A failed query renders an uncached PNG, never one cached like a genuinely empty result.
    let pngFor query appearance = do
          let encoded = decodeUtf8 @Text $ AE.encode (def :: Widget.Widget){Widget.query = Just query, Widget.pngProfile = Just Widget.PngSlack}
          toBaseServantResponse tr $ Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing (Just "2025-06-01T11:45:00Z") (Just "2025-06-01T12:00:00Z") Nothing Nothing (Just $ Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded) [("appearance", Just appearance)]
    failed <- pngFor "name ==" "light"
    failedDark <- pngFor "name ==" "dark"
    emptyPng <- pngFor "name == \"no-such-png-fixture-operation\"" "light"
    map (LBS.take 8 . Servant.getResponse) [failed, failedDark, emptyPng] `shouldBe` replicate 3 "\137PNG\r\n\26\n"
    map (lookup "Cache-Control" . Servant.getHeaders) [failed, emptyPng] `shouldBe` [Just "no-store", Just "public, max-age=31536000, immutable"]
    Servant.getResponse failed `shouldNotBe` Servant.getResponse emptyPng

    -- A grouped status chart with missing series values still renders, with and without stats.
    for_ [Nothing, Just (def :: MetricsStats){maxGroupSum = 202}] \stats -> do
      let rows = AE.toJSON ([[AE.String "timestamp", AE.String "ERROR", AE.String "OK", AE.String "UNSET", AE.String "null"], [AE.Number 1788451200000, AE.Null, AE.Null, AE.Number 187, AE.Number 5], [AE.Number 1788451320000, AE.Number 1, AE.Null, AE.Number 201, AE.Null]] :: [[AE.Value]])
          encoded = decodeUtf8 @Text $ AE.encode (def :: Widget.Widget){Widget.id = Just "log-explorer-all-traces", Widget.wType = Widget.WTTimeseries, Widget.query = Just "summarize count(*) by bin_auto(timestamp), status_code", Widget.dataset = Just (def :: Widget.WidgetDataset){Widget.source = rows, Widget.from = Just 1788451200000, Widget.to = Just 1788465600000, Widget.stats = stats}, Widget.yAxis = Just (def :: Widget.WidgetAxis){Widget.showOnlyMaxLabel = Just True}, Widget.unit = Just "rows", Widget.legendPosition = Just "top-right", Widget.legendSize = Just Widget.LSXs, Widget.standalone = Just True, Widget.allowZoom = Just True}
      response <- toBaseServantResponse tr $ Routes.widgetPngGetH testPid (Just encoded) Nothing Nothing Nothing Nothing Nothing Nothing (Just $ Widget.signWidgetUrl tr.trATCtx.env.apiKeyEncryptionSecretKey testPid encoded) []
      LBS.take 8 (Servant.getResponse response) `shouldBe` "\137PNG\r\n\26\n"

  it "saved views: save, scope by tab, override, concurrent writes, reject bad names and other projects" \tr -> do
    did <- newDashboard tr "rum.yaml" "Shared views"
    other <- createTestProject tr "Other saved views"
    let form = either (fail . toString) pure . urlDecodeAsForm @Dashboards.DashboardViewForm
        views = fmap (V.toList . (.savedViews) . fst) . runAuthHandler tr $ Dashboards.getDashAndVM testPid did Nothing
        save name service = void $ testServant tr $ Dashboards.dashboardViewPostH testPid did (Dashboards.DashboardViewForm name [("var-service", Just service)])
        swapParts = elementsWith ["id=\"dashboard-variables\"", "id=\"dashboard-views-menu\""] . renderT
        status action = runAuthHandler tr $ catchError @Servant.ServerError (action $> (200 :: Int)) (\_ err -> pure err.errHTTPCode)
        saveIn pid name = Dashboards.dashboardViewPostH pid did (Dashboards.DashboardViewForm name [])
    -- Arbitrary variables, window and tab are kept without freezing the template into the row.
    saved <- call tr . Dashboards.dashboardViewPostH testPid did =<< form "name=Mobile+Safari&var-browser=Safari&since=7D&activeTabSlug=errors"
    (vm, _) <- runAuthHandler tr $ Dashboards.getDashAndVM testPid did Nothing
    (isNothing vm.schema, map (.name) (V.toList vm.savedViews)) `shouldBe` (True, ["Mobile Safari"])
    deleted <- call tr $ Dashboards.dashboardViewDeleteH testPid did "Mobile Safari" [("var-browser", Just "Safari"), ("since", Just "7D"), ("activeTabSlug", Just "errors")]
    views `shouldReturn` []
    traverse status [saveIn testPid "  ", saveIn testPid (T.replicate 101 "x"), saveIn other "Wrong project", Dashboards.dashboardViewDeleteH other did "Missing" []] `shouldReturn` ([400, 400, 404, 404] :: [Int])
    assertNonMemberDenied tr other (saveIn other "Denied")
    assertNonMemberDenied tr other (Dashboards.dashboardViewDeleteH other did "Denied" [])

    -- Configured views and tab-scoped variables on any dashboard; a saved view of the same
    -- name overrides the configured one until it is deleted.
    _ <- testServant tr $ Dashboards.dashboardYamlPutH testPid did $ Dashboards.YamlForm "title: Shared views\nwidgets: []\nvariables:\n  - {key: service, title: Service, type: values, options: [[api]]}\n  - {key: region, title: Region, type: values, options: [[eu]]}\nviews:\n  - {name: Europe API, variables: {service: api, region: eu}}\ntabs:\n  - {name: Overview, variables: [service], widgets: []}\n  - {name: Errors, variables: [service, region], widgets: []}\n"
    overview <- partialHtml =<< getTab tr did "overview" tabsContent []
    errorsTab <- getTab tr did "errors" tabsContent []
    let Dashboards.DashboardGet _ _ dash _ _ = tabDashboard errorsTab
        tabVars name = map (.key) $ DashboardModel.variablesForTab dash (find ((== name) . (.name)) (fold dash.tabs))
    (tabVars "Overview", tabVars "Errors") `shouldBe` (["service"], ["service", "region"])
    worker <- form "name=US+worker&var-service=worker&var-region=us&var-unknown=ignored&since=24H&activeTabSlug=errors"
    _ <- testServant tr $ Dashboards.dashboardViewPostH testPid did worker
    views >>= (`shouldBe` [Map.fromList [("service", "worker"), ("region", "us")]]) . \vs -> [v.variables | v <- vs, v.name == "US worker"]
    overridden <- call tr $ Dashboards.dashboardViewPostH testPid did worker{Dashboards.name = "Europe API"}
    revealed <- call tr $ Dashboards.dashboardViewDeleteH testPid did "Europe API" worker.params
    reloaded <- partialHtml =<< getTab tr did "overview" tabsContent [("since", Just "24H")]
    let root slug = "/p/" <> testPid.toText <> "/dashboards/" <> did.toText <> "/tab/" <> slug
    linkParams (root "overview") (renderT reloaded) `shouldSatisfy` any (\ps -> lookup "var-service" ps == Just "worker" && lookup "var-region" ps == Just "us" && lookup "since" ps == Just "24H")
    errorsHtml <- partialHtml errorsTab

    -- Concurrent saves keep the other views and replace the same name once.
    let names = ["View " <> show i | i <- [1 .. 8 :: Int]]
    mapConcurrently_ (`save` "api") names
    map (.name) <$> views >>= (`shouldMatchList` ("US worker" : names))
    mapConcurrently_ (save "View 1") ["worker", "billing"]
    updated <- views
    map (.name) updated `shouldMatchList` ("US worker" : names)
    [Map.lookup "service" v.variables | v <- updated, v.name == "View 1"] `shouldSatisfy` (`elem` ([[Just "worker"], [Just "billing"]] :: [[Maybe Text]]))

    -- Service Overview scopes its filters per tab, and each tab can restore all services.
    ovId <- newDashboard tr "_overview.yaml" "Overview views"
    ovBars <- forM ([("overview", ["service", "resource"]), ("service", ["service", "resource"]), ("logs", ["service"]), ("databases", ["database"]), ("infra", [])] :: [(Text, [Text])]) \(slug, keys) -> do
      tab <- getTab tr ovId slug Nothing [("var-service", Just "api"), ("var-database", Just "postgresql")]
      let Dashboards.DashboardGet _ _ ov _ _ = tabDashboard tab
      (slug, map (.key) $ DashboardModel.variablesForTab ov (find ((== slug) . slugify . (.name)) (fold ov.tabs))) `shouldBe` (slug, keys)
      linkParams ("/p/" <> testPid.toText <> "/dashboards/" <> ovId.toText <> "/tab/" <> slug) (renderT tab) `shouldSatisfy` any (\ps -> lookup "var-service" ps == Just "" && lookup "var-database" ps == Just "")
      pure $ tagsWith ["id=\"dash-vars-toggle\"", "class=\"dash-variable-input"] (renderT tab)
    ovSaved <- call tr . Dashboards.dashboardViewPostH testPid ovId =<< form "name=API&var-service=api&since=7D&activeTabSlug=logs"
    goldenParts "dashboard-saved-views" $ map renderT [saved, deleted] <> swapParts overview <> swapParts errorsHtml <> map renderT [overridden, revealed] <> swapParts reloaded <> [renderT ovSaved] <> fold ovBars

  it "migrations repair legacy warning thresholds and move RUM segments into saved views" \tr -> do
    let migrate file = withResource tr.trPool \conn -> void . PG.execute_ conn . Query =<< readFileBS file
    dashId <- newDashboard tr "" "Legacy warning thresholds"
    let legacy = "{\"widgets\":[{\"type\":\"stat\",\"arning_threshold\":7,\"query\":\"arning_threshold\"},{\"type\":\"group\",\"children\":[{\"type\":\"stat\",\"warning_threshold\":9,\"arning_threshold\":4},{\"type\":\"stat\",\"arning_threshold\":null}]}],\"tabs\":[{\"name\":\"Nested\",\"widgets\":[{\"type\":\"stat\",\"arning_threshold\":3}]}]}" :: Text
        expected = "{\"widgets\":[{\"type\":\"stat\",\"warning_threshold\":7,\"query\":\"arning_threshold\"},{\"type\":\"group\",\"children\":[{\"type\":\"stat\",\"warning_threshold\":9},{\"type\":\"stat\",\"warning_threshold\":null}]}],\"tabs\":[{\"name\":\"Nested\",\"widgets\":[{\"type\":\"stat\",\"warning_threshold\":3}]}]}"
    dbExec tr "UPDATE projects.dashboards SET schema = ?::jsonb WHERE id = ?" (legacy, dashId)
    AE.eitherDecodeStrict' @DashboardModel.Dashboard (encodeUtf8 legacy) `shouldSatisfy` isLeft
    replicateM_ 2 do
      migrate "static/migrations/0212_widget_warning_threshold.sql"
      actual <- dbOne tr "SELECT schema FROM projects.dashboards WHERE id = ?" (Only dashId)
      Right actual `shouldBe` AE.eitherDecode @AE.Value expected
      AE.eitherDecode @DashboardModel.Dashboard (AE.encode actual) `shouldSatisfy` isRight

    -- Runs even for a project that never opened its RUM dashboard. Last: it drops a column.
    pid <- createTestProject tr "Unopened RUM dashboard"
    did <- newDashboard tr "rum.yaml" "Existing RUM dashboard"
    dbExec tr "ALTER TABLE projects.dashboards DROP COLUMN saved_views; CREATE TABLE projects.rum_segments (project_id uuid, name text, filters jsonb)" ()
    dbExec tr "INSERT INTO projects.rum_segments VALUES (?, 'Safari', '{\"browser\":\"Safari\",\"app\":null,\"errored\":true}'), (?, 'Safari', '{\"browser\":\"Safari\",\"errored\":false}')" (testPid, pid)
    migrate "static/migrations/0255_dashboard_saved_views.sql"
    rows <- dbQuery tr "SELECT project_id, saved_views[1]->>'name', saved_views[1]->'variables' FROM projects.dashboards WHERE id = ? OR project_id = ?" (did, pid)
    (rows :: [(Projects.ProjectId, Text, AE.Value)]) `shouldMatchList` [(testPid, "Safari", AE.object ["browser" AE..= ("Safari" :: Text), "errors" AE..= ("true" :: Text)]), (pid, "Safari", AE.object ["browser" AE..= ("Safari" :: Text), "errors" AE..= ("false" :: Text)])]

  it "widget editor: starts on a chart and edits thresholds and monitors in place" \tr -> do
    dashId <- newDashboard tr "overview.yaml" "Widget Display Thresholds"
    newEditor <- call tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
    addWidgets tr dashId [(widgetOf Widget.WTTimeseries "Thresholded"){Widget.alertThreshold = Just 10, Widget.warningThreshold = Just 7, Widget.unit = Just "requests/s"}, widgetOf Widget.WTTable "Rows", (widgetOf Widget.WTTimeseries "SQL Chart"){Widget.query = Nothing, Widget.sql = Just "SELECT 1"}]
    stored <- storedWidgets tr dashId
    [(w.alertThreshold, w.warningThreshold, w.unit) | w <- take 1 stored] `shouldBe` [(Just 10, Just 7, Just "requests/s")]
    editors <- forM [(w, tab) | (w, tabs) <- zip (mapMaybe (.id) stored) [[Nothing, Just "alerts"], [Nothing], [Nothing]], tab <- tabs] \(wid, tab) ->
      call tr $ Dashboards.dashboardWidgetExpandGetH testPid dashId wid tab
    -- Per editor: tab radios, actions, seeded widget type, step order, thresholds, monitor form.
    goldenParts "dashboard-widget-editor" $ foldMap (elementsWith ["-drawer-tab\"", "type=\"submit\" form=\"", "Add widget and monitor", "var widgetJSON", "widget-preview-heading", "Configure query", "Name your widget", "Widget name", "class=\"group/thresholds", "name=\"alertThreshold\"", "empty-state"] . renderT) (newEditor : editors)

  it "raw-SQL widgets decode any column type, stream what JSON returns, and route by store" \tr -> do
    -- One uncast column failed the whole widget with `Incompatible {errSQLType = "int4"}`.
    let texts q = (\md -> (md.error, md.dataText)) <$> sqlChart tr Nothing Charts.DTText q []
    texts "SELECT 1::int4 AS n, 'ok'::text AS s" `shouldReturn` (Nothing, V.singleton (V.fromList ["1", "ok"]))
    texts "SELECT NULL::int4 AS n, 'after'::text AS s" `shouldReturn` (Nothing, V.singleton (V.fromList ["", "after"]))
    -- date_part(EPOCH, time_bucket(...)) comes back float8.
    (fst <$> texts "SELECT 42::bigint, 1.5::float8, true, '2026-01-02 03:04:05+00'::timestamptz") `shouldReturn` Nothing
    (fmap (V.toList . V.take 1) . (V.!? 0) . snd <$> texts "SELECT 42::bigint, 1.5::float8, true, '2026-01-02 03:04:05+00'::timestamptz") `shouldReturn` Just ["42"]

    -- A plotted widget's leading column is read as epoch; `time_bucket(...) AS timestamp` is a timestamptz.
    let plot q = sqlChart tr (Just Hasql.SqlPostgres) Charts.DTMetric q []
    stamped <- plot "SELECT '2026-01-02 03:04:05+00'::timestamptz AS timestamp, 'value'::text, 7::double precision"
    (stamped.error, stamped.dataset V.!? 0 >>= (V.!? 0)) `shouldBe` (Nothing, Just (Just 1767323045000))
    (.dataset) <$> plot "SELECT 1767323045::int4 AS timestamp, 'value'::text, 7::double precision" `shouldReturn` stamped.dataset
    (.error) <$> plot "SELECT 'not a bucket'::text, 'value'::text, 7::double precision" >>= (`shouldSatisfy` isJust)

    -- Streaming: partial frames, then the JSON endpoint's result; decoder failures end in an error frame.
    let series = "SELECT i::bigint, 'value'::text, i::double precision FROM generate_series(1, 3) i"
    expected <- plot series
    frames <- streamFrames =<< sqlStream tr (Just Hasql.SqlPostgres) Charts.DTMetric series []
    frames `shouldSatisfy` \case
      Right vs@(AE.Object opening : _ : _) -> KM.lookup "type" opening == Just (AE.String "partial") && viaNonEmpty last vs == Just (AE.object ["type" AE..= ("complete" :: Text), "data" AE..= expected])
      _ -> False
    sqlStream tr (Just Hasql.SqlPostgres) Charts.DTMetric "SELECT 'wrong type'::text, 'value'::text, 1::double precision" [] >>= streamFrames >>= (`shouldSatisfy` \case Right [AE.Object o] -> KM.lookup "type" o == Just (AE.String "error"); _ -> False)
    -- Rounded numeric values from widget SQL, for a chart and a stat.
    let (numericQ, scalarQ) = ("SELECT 1::bigint, 'cpu'::text, ROUND(AVG(1.25::numeric), 2)", "SELECT ROUND(AVG(1.25::numeric), 2)")
    numeric <- plot numericQ
    (numeric.error, numeric.dataset) `shouldBe` (Nothing, V.singleton (V.fromList [Just 1000, Just 1.25]))
    scalar <- sqlChart tr (Just Hasql.SqlPostgres) Charts.DTFloat scalarQ []
    (scalar.error, scalar.dataFloat) `shouldBe` (Nothing, Just 1.25)
    for_ [(Charts.DTMetric, numericQ, numeric), (Charts.DTFloat, scalarQ, scalar)] \(dt, q, md) -> sqlStream tr (Just Hasql.SqlPostgres) dt q [] >>= (`completesWith` md)

    -- A failing row consumer discards its connection; two large rows flush the first through
    -- the socket buffer while the third sleeps until the query is cancelled.
    result <- timeout 3000000 $ E.try @E.IOException $ withResource tr.trPool \conn ->
      Charts.streamQuery_ conn "SELECT i::bigint, repeat('x', 10000) FROM generate_series(1, 3) i CROSS JOIN LATERAL pg_sleep(CASE WHEN i <= 2 THEN 0 ELSE 30 END)" \(_ :: (Int64, Text)) -> E.throwIO $ userError "consumer stopped"
    result `shouldSatisfy` maybe False isLeft
    dbOne tr "SELECT 42::bigint" () `shouldReturn` (42 :: Int64)

    -- A scalar monitor plots its max per bucket instead of counting the aggregate.
    now <- getTestTime tr.trTestClock
    dbExec tr [sql| INSERT INTO otel_metrics (project_id, timestamp, id, series_id, metric_name, metric_type, value) VALUES (?, ?, gen_random_uuid(), 'monitor-chart', 'timefusion.mem_buffer.oldest_bucket_age_seconds', 'GAUGE', 42) |] (testPid, now)
    let (monitorQuery, decoder) = Widget.chartQuery (widgetOf Widget.WTTimeseries "Query Results"){Widget.query = Just "metrics | where metric_name == \"timefusion.mem_buffer.oldest_bucket_age_seconds\" | summarize max(value)"}
    monitorQuery `shouldBe` Just "metrics | where metric_name == \"timefusion.mem_buffer.oldest_bucket_age_seconds\" | summarize max(value) by bin_auto(timestamp)"
    monitor <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just decoder) (Just testPid) monitorQuery Nothing (Just "1H") Nothing Nothing Nothing Nothing []
    (monitor.error, V.toList (monitor.dataset >>= V.catMaybes . V.drop 1)) `shouldBe` (Nothing, [42])
    runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just decoder) (Just testPid) monitorQuery Nothing (Just "1H") Nothing Nothing Nothing Nothing []) >>= (`completesWith` monitor)

    -- Source selection, variables and blank time params agree across both transports.
    let query = Just "where metric_name == \"{{var-metric}}\" | summarize max(value) by bin_auto(timestamp)"
        params = [("var-metric", Just "timefusion.mem_buffer.oldest_bucket_age_seconds")]
        from = Just $ toText $ iso8601Show $ addUTCTime (-3600) now
        to = Just $ toText $ iso8601Show now
    prepared <- runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just decoder) (Just testPid) query Nothing (Just "") from to (Just "metrics") Nothing params
    (prepared.error, V.toList (prepared.dataset >>= V.catMaybes . V.drop 1)) `shouldBe` (Nothing, [42])
    runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just decoder) (Just testPid) query Nothing (Just "") from to (Just "metrics") Nothing params) >>= (`completesWith` prepared)

    -- A variable's `source: postgres` must reach /chart_data, or apis.endpoints is planned on TimeFusion.
    (.error) <$> call tr (addRespHeaders =<< Routes.chartsDataGetH (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just "select hash::text, method || ' ' || url_path from apis.endpoints where project_id='{{project_id}}' limit 5") (Just "24H") Nothing Nothing Nothing Nothing []) `shouldReturn` Nothing
    -- A covered, unscoped window is answered from the hourly rollup; one reaching before its coverage is not read as zero.
    dbExec tr [sql|INSERT INTO apis.log_pattern_hourly_stats (project_id, pattern_hash, hour_bucket, event_count) VALUES (?, 'rollup-route', date_trunc('hour', ?::timestamptz) - interval '2 hours', 7)|] (testPid, now)
    let rollup = Issues.hourlyRollupSql "apis.log_pattern_hourly_stats" "pattern_hash = 'rollup-route'"
        total coveredFor since = call tr (addRespHeaders =<< Routes.chartsDataGetH Nothing (Just Charts.DTMetric) (Just testPid) (Just "hashes[*]==\"pat:rollup-route\" | summarize count(*) by bin_auto(timestamp)") Nothing (Just since) Nothing Nothing Nothing Nothing [("rollup_sql", Just rollup), ("rollup_from", Just $ Utils.isoT $ addUTCTime (-coveredFor) now)]) <&> \md -> sum (md.dataset >>= V.catMaybes . V.drop 1)
    (,,) <$> total 172800 "24H" <*> total 172800 "3H" <*> total 3600 "24H" `shouldReturn` (7, 7, 0)

  it "SQL scoping: environment, service and project boundaries hold for widgets, copy-SQL and client SQL" \tr -> do
    apiKey <- createTestAPIKey tr testPid "raw-template-scope"
    let ingest route env service n = ingestSpanReq tr $ mkSpanRequest (T.replicate 32 n) (T.replicate 16 n) Nothing (route <> service) [] Nothing [] (mkResource apiKey [mkAttr "deployment.environment.name" env, mkAttr "service.name" service]) frozenTime
        rawFor route = "SELECT resource___service___name FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND name LIKE '" <> route <> "%' ORDER BY 1"
        rows md = (md.error, md.dataText)
    -- Raw SQL has no safe insertion point: under an environment it must reserve {{query_ast_filters}}.
    let unscopedMsg = "This raw SQL widget must include {{query_ast_filters}} to respect the selected environment."
        envOnly = [("environment", Just "production")]
        serviceOnly = [("service", Just "checkout")]
    for_ @[] [(envOnly, unscopedMsg), (serviceOnly, "This raw SQL widget must include {{query_ast_filters}} to respect the selected service."), (envOnly <> serviceOnly, "This raw SQL widget must include {{query_ast_filters}} to respect the selected environment and service.")] \(scope, message) ->
      for_ @[] [Nothing, Just "name =="] \query -> do
        -- Scope rejection precedes KQL diagnostics on both transports.
        result <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just testPid) query (Just "SELECT 1::int4 AS n") (Just "24H") Nothing Nothing Nothing Nothing scope
        result.error `shouldBe` Just message
        stream <- runQueryEffect tr $ Charts.queryMetricsStream Nothing (Just Charts.DTText) (Just testPid) query (Just "SELECT 1::int4 AS n") (Just "24H") Nothing Nothing Nothing Nothing scope
        streamFrames stream >>= (`shouldSatisfy` either (const False) (any \case AE.Object o -> KM.lookup "error" o == Just (AE.String message); _ -> False))
    ingest "GET /raw/" "production" "checkout" "1"
    ingest "GET /raw/" "production" "catalog" "2"
    ingest "GET /raw/" "staging" "checkout" "3"
    rows <$> sqlChart tr Nothing Charts.DTText (rawFor "GET /raw/") [("environment", Just "production"), ("service", Just "checkout")] `shouldReturn` (Nothing, V.singleton (V.singleton "checkout"))

    -- Outside dashboards the session's sticky service applies; a dashboard supplies its own. Both keep the session environment.
    ingest "GET /chart-scope/" "production" "checkout" "4"
    ingest "GET /chart-scope/" "staging" "catalog" "5"
    ingest "GET /chart-scope/" "production" "catalog" "6"
    scopedSession <- checkoutSession tr
    let asCheckout :: ATAuthCtx a -> IO a
        asCheckout = toBaseServantResponse tr . atAuthToBase scopedSession
        chartData ps = rows <$> asCheckout (Routes.chartsDataGetH Nothing (Just Charts.DTText) (Just testPid) Nothing (Just $ rawFor "GET /chart-scope/") (Just "24H") Nothing Nothing Nothing Nothing ps)
        staging = [("environment", Just "staging"), ("service", Just "catalog")]
    chartData staging `shouldReturn` (Nothing, V.singleton (V.singleton "checkout"))
    chartData (("dashboard_id", Just "overview") : staging) `shouldReturn` (Nothing, V.fromList [V.singleton "catalog", V.singleton "checkout"])
    -- Copy SQL translates KQL (not an echo) under the same scope, and keeps the chart's grouping.
    let copySql q dash = Servant.getResponse <$> asCheckout (Dashboards.widgetSqlTextGetH testPid q dash Nothing Nothing Nothing)
    copied <- copySql (Just "status_code == \"ERROR\"") Nothing
    copied `shouldContainAll` ["SELECT", "resource___deployment___environment___name = 'production'", "resource___service___name = 'checkout'"]
    onDashboard <- copySql (Just "status_code == \"ERROR\"") (Just "overview")
    (T.isInfixOf "environment___name = 'production'" onDashboard, T.isInfixOf "resource___service___name" onDashboard) `shouldBe` (True, False)
    grouped <- call tr $ Dashboards.widgetSqlTextGetH testPid (Just "summarize count(*) by bin_auto(timestamp), coalesce(status_code, level)") Nothing Nothing Nothing Nothing
    T.isInfixOf "jsonb_build_array" grouped `shouldBe` False
    snd (T.breakOn "GROUP BY" grouped) `shouldContainAll` ["status_code", "level"]
    call tr (Dashboards.widgetSqlTextGetH testPid Nothing Nothing Nothing Nothing Nothing) `shouldReturn` "No query provided"

    -- The tenant boundary was a `project_id='<pid>'` substring check, so `... OR TRUE` read every
    -- project. Each tenant table must reach the statement filtered, on both stores; a statement
    -- that cannot be scoped (a CTE reusing a tenant table's name) may fail instead.
    otherPid <- createTestProject tr "Other tenant"
    theirs <- createTestAPIKey tr otherPid "tenant-scope-theirs"
    ingestTrace tr apiKey "GET /tenant-scope" frozenTime
    ingestTrace tr theirs "GET /tenant-scope" frozenTime
    let own = "project_id='" <> testPid.toText <> "'"
        run source q = fmap (\rs -> ordNub [v | row <- V.toList rs, AE.String v <- V.toList row]) <$> runQueryEffect tr (LogQueries.executeSecuredQuery True testPid (LogQueries.SecuredSql source q) 100)
    for_ [LogQueries.SqlPostgres, LogQueries.SqlTimefusion] \source -> do
      for_
        [ "SELECT project_id::text FROM otel_logs_and_spans WHERE " <> own <> " OR TRUE"
        , "SELECT p FROM (SELECT project_id::text AS p, project_id FROM otel_logs_and_spans) s WHERE " <> own <> " OR TRUE"
        , "SELECT project_id::text FROM otel_logs_and_spans WHERE name = 'GET /tenant-scope' AND (" <> own <> " OR project_id IN (SELECT project_id FROM otel_logs_and_spans))"
        ]
        \q -> run source q `shouldReturn` Right [testPid.toText]
      run source ("WITH otel_logs_and_spans AS (SELECT * FROM otel_logs_and_spans) SELECT project_id::text FROM otel_logs_and_spans WHERE " <> own <> " OR TRUE") >>= (`shouldSatisfy` either (const True) (== [testPid.toText]))

    -- Every statement a template ships must still scope, and on Postgres a tenant CTE read
    -- twice must inline rather than materialize a full-project scan.
    templates <- DashboardModel.readDashboardsFromDisk "static/public/dashboards"
    let fill = fillQueryTemplate SqlTemplate (variablePresets def testPid.toText Nothing Nothing [] frozenTime <> Map.fromList [("query_ast_filters", "TRUE"), ("table_sort", "1")])
        statements = [(fromMaybe "" d.file, s) | d <- templates, s <- mapMaybe (.sql) (DashboardModel.allWidgets d) <> [c.statement | v <- fold d.variables, Just c <- [v.sql]] <> [c.statement | k <- fold d.constants, Just c <- [k.sql]]]
    statements `shouldSatisfy` (not . null)
    [(file, s, postgres) | (file, s) <- statements, postgres <- [True, False], isLeft (LogQueries.scopeUserSql postgres testPid (fill s))] `shouldBe` []
    scoped <- either (fail . toString) pure $ LogQueries.scopeUserSql True testPid "SELECT count(*) FROM otel_logs_and_spans a WHERE timestamp > now() - interval '1 hour' AND EXISTS (SELECT 1 FROM otel_logs_and_spans b WHERE b.timestamp > now() - interval '1 hour')"
    plan <- dbQuery tr (fromString $ toString $ "EXPLAIN " <> scoped) ()
    [l | Only l <- plan, "CTE Scan" `T.isInfixOf` l] `shouldBe` ([] :: [Text])

    -- /chart_data ran whatever SQL the browser sent; a viewer may run only server-signed SQL.
    let clientSql = "SELECT 'x'::text FROM otel_logs_and_spans WHERE project_id='" <> testPid.toText <> "' LIMIT 1"
        userId = (Servant.getResponse tr.trSessAndHeader).user.id
        setPermission p = dbExec tr "UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?" (p, testPid, userId)
        chart source ps = toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Routes.chartsDataGetH (Just source) (Just Charts.DTText) (Just testPid) Nothing (Just clientSql) (Just "24H") Nothing Nothing Nothing Nothing ps
        signed = [("sql_sig", Just $ Widget.sqlSignature tr.trATCtx.env.apiKeyEncryptionSecretKey testPid (Just clientSql) Nothing)]
    original <- dbOne tr "SELECT permission FROM projects.project_members WHERE project_id = ? AND user_id = ?" (testPid, userId)
    for_ [Hasql.SqlPostgres, Hasql.SqlTimefusion] \source -> do
      (.error) <$> chart source [] `shouldReturn` Nothing
      E.bracket_ (setPermission ProjectMembers.PView) (setPermission (original :: ProjectMembers.Permissions)) do
        E.try @E.SomeException (chart source []) >>= (`shouldSatisfy` isLeft)
        (.error) <$> chart source signed `shouldReturn` Nothing

    -- Dashboard variables reached widget SQL verbatim, so a quote ended the literal and the rest ran
    -- as SQL; and request params merged after the server presets could re-point `project_id`.
    let injected = (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.sql = Just "SELECT v, project_id FROM (SELECT '{{var-service}}'::text AS v, '{{project_id}}'::text AS project_id) t WHERE project_id='{{project_id}}'"}
    md <- runQueryEffect tr $ runConcurrent $ Dashboards.widgetMetrics testPid (Just "24h", Nothing, Nothing) [("var-service", Just "a' OR 'x'='x"), ("project_id", Just "11111111-1111-1111-1111-111111111111")] injected
    (md.error :: Maybe Text) `shouldBe` Nothing
    md.dataText `shouldBe` [["a' OR 'x'='x", testPid.toText]]

  -- The probe counts real executions (each run advances the sequence), so a hit repeats a value.
  it "complete-result SQL cache: hits, isolates keys, shares across stream and JSON, expires, single-flights, degrades" \tr -> do
    let probe = "SELECT (nextval('raw_cache_probe') - 1)::double precision"
        reset = dbExec tr "DELETE FROM query_cache WHERE source = 'raw-sql'; DROP SEQUENCE IF EXISTS raw_cache_probe; CREATE SEQUENCE raw_cache_probe" ()
        cached = dbOne tr "SELECT count(*)::int FROM query_cache WHERE source = 'raw-sql'" () :: IO Int
        floats q endpoint = (.dataFloat) <$> sqlChart tr (Just Hasql.SqlPostgres) Charts.DTFloat q [("var-endpointHash", Just endpoint)]
        windowed from to endpoint = (.dataFloat) <$> runQueryEffect tr (Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTFloat) (Just testPid) Nothing (Just probe) Nothing from to Nothing Nothing [("var-endpointHash", Just endpoint)])
    reset
    (.error) <$> sqlChart tr (Just Hasql.SqlPostgres) Charts.DTFloat "SELECT missing_cache_column::double precision" [("var-endpointHash", Just "endpoint-failure")] >>= (`shouldSatisfy` isJust)
    cached `shouldReturn` 0
    traverse (floats probe) ["endpoint-a", "endpoint-a", "endpoint-b" :: Text] `shouldReturn` ([Just 0, Just 0, Just 1] :: [Maybe Double])

    reset
    let shared = [("var-endpointHash", Just "stream-cache")]
        stream = (.dataFloat) <$> (completeFrame @Charts.MetricsData =<< sqlStream tr (Just Hasql.SqlPostgres) Charts.DTFloat probe shared)
    sequence [stream, (.dataFloat) <$> sqlChart tr (Just Hasql.SqlPostgres) Charts.DTFloat probe shared, stream] `shouldReturn` replicate 3 (Just 0)
    cached `shouldReturn` 1

    -- A fixed window expires so late telemetry can replace it; an omitted range reuses the live one.
    reset
    let fixed = windowed (Just "2025-01-01T00:00:00Z") (Just "2025-01-02T00:00:00Z") "expiry"
    initial <- fixed
    advanceTestTime tr 59
    fresh <- fixed
    advanceTestTime tr 2
    expired <- fixed
    (initial, fresh, expired) `shouldBe` (Just 0, Just 0, Just 1)
    advanceTestTime tr 601
    runQueryEffect tr QueryCache.cleanupExpiredCache `shouldReturn` 1
    reset
    live <- windowed Nothing Nothing "default-range"
    advanceTestTime tr 0.001
    windowed Nothing Nothing "default-range" `shouldReturn` live

    -- A follower waits for the leader's cache write instead of running the query again.
    let slowWrite = dbExec tr "CREATE SEQUENCE cache_race_calls; CREATE FUNCTION slow_cache_write() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN PERFORM pg_sleep(0.5); RETURN NEW; END $$; CREATE TRIGGER slow_cache_write BEFORE INSERT ON query_cache FOR EACH ROW EXECUTE FUNCTION slow_cache_write()" ()
        dropSlowWrite = dbExec tr "DROP TRIGGER slow_cache_write ON query_cache; DROP FUNCTION slow_cache_write(); DROP SEQUENCE cache_race_calls" ()
        race = floats "SELECT nextval('cache_race_calls')::double precision" "write-race"
        waitForWrite = dbOne tr "SELECT EXISTS (SELECT 1 FROM pg_stat_activity WHERE datname = current_database() AND wait_event = 'PgSleep')" () >>= \sleeping -> unless sleeping (threadDelay 10000 >> waitForWrite)
    E.bracket_ slowWrite dropSlowWrite $ withAsync race \leader -> do
      timeout 5000000 waitForWrite `shouldReturn` Just ()
      follower <- race
      wait leader `shouldReturn` follower
      follower `shouldBe` Just 1

    -- Results still return with the cache table gone; oversized results are not stored.
    let texts q = (\md -> (md.error, md.dataText)) <$> sqlChart tr (Just Hasql.SqlPostgres) Charts.DTText q [("var-endpointHash", Just "cache-fallback")]
        exec q = dbExec tr q ()
    E.bracket_ (exec "ALTER TABLE query_cache RENAME TO unavailable_query_cache") (exec "ALTER TABLE unavailable_query_cache RENAME TO query_cache") do
      texts "SELECT 'still available'::text" `shouldReturn` (Nothing, V.singleton (V.singleton "still available"))
    reset
    second (V.map (V.map T.length)) <$> texts "SELECT repeat('x', 1048576)" `shouldReturn` (Nothing, V.singleton (V.singleton 1048576))
    cached `shouldReturn` 0

    -- One backend execution per window across a cold request and ten cached ones, streamed or not.
    let heavy = "SELECT q.call::text, n::text, repeat('x', 80) FROM (SELECT nextval('cache_benchmark_calls') AS call, pg_sleep(0.1)) q CROSS JOIN generate_series(1, 1000) n"
        fetch window streaming =
          if streaming
                then completeFrame @Charts.MetricsData =<< runQueryEffect tr (Charts.queryMetricsStream (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just heavy) (Just window) Nothing Nothing Nothing Nothing [("var-endpointHash", Just "benchmark")])
                else runQueryEffect tr $ Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) Nothing (Just heavy) (Just window) Nothing Nothing Nothing Nothing [("var-endpointHash", Just "benchmark")]
    exec "CREATE SEQUENCE cache_benchmark_calls"
    for_ ["24H", "3D", "7D"] \window -> do
      exec "DELETE FROM query_cache WHERE source = 'raw-sql'; ALTER SEQUENCE cache_benchmark_calls RESTART WITH 1"
      cold <- fetch window True
      (cold.error, V.length cold.dataText) `shouldBe` (Nothing, 1000)
      warm <- traverse (fetch window . even) [1 .. 10 :: Int]
      filter (/= cold.dataText) (map (.dataText) warm) `shouldBe` []
      dbOne tr "SELECT last_value FROM cache_benchmark_calls" () `shouldReturn` (1 :: Int)

  it "table widgets sort on the server and resource tables group on both backends" \tr -> withTfChartPool tr \tr' -> do
    tfUrl <- lookupEnv "TIMEFUSION_PG_TEST_URL"
    now <- getTestTime tr'.trTestClock
    let backends = Hasql.SqlPostgres : [Hasql.SqlTimefusion | isJust tfUrl]
        cell i value
          | i == (0 :: Int) = AE.String value
          | T.null value = AE.Null
          | otherwise = maybe (AE.String value) AE.toJSON $ readMaybe @Double $ toString value
        onBoth pid query sqlQ expected = for_ backends \db -> do
          dbExec tr' "DELETE FROM query_cache" ()
          result <- runQueryEffect tr' $ Charts.queryMetrics (Just db) (Just Charts.DTText) (Just pid) query sqlQ (Just "24h") Nothing Nothing Nothing Nothing []
          (db, result.error, V.imap cell <$> result.dataText) `shouldBe` (db, Nothing, expected)
    grouped <- createTestProject tr' "Grouped metric resources"
    groupedKey <- createTestAPIKey tr' grouped "grouped-metrics"
    for_ ([(Just "alpha", 2), (Just "alpha", 4), (Just "beta", 7), (Nothing, 9), (Nothing, 11)] :: [(Maybe Text, Double)]) \(container, value) ->
      ingestMetric tr' groupedKey (maybe [] (pure . mkAttr "container.name") container) [] "grouped.metric" value now
    let row label lo hi = V.fromList [AE.String label, AE.toJSON (lo :: Double), AE.toJSON (hi :: Double)]
    onBoth grouped (Just "metrics | where metric_name == \"grouped.metric\" | summarize min(value), max(value) by resource.container.name") Nothing (V.fromList [row "null" 9 11, row "beta" 7 7, row "alpha" 2 4])

    -- Overview resource samples: paired and unpaired cpu/memory each count, expired ones do not.
    samples <- createTestProject tr' "Resource sample counts"
    samplesKey <- createTestAPIKey tr' samples "resource-samples"
    for_
      [ (service, metric, value, stamp)
      | (service, cpu, memory, stamp) <- [("paired", Just 0.25, Just 0.4, now), ("cpu-only", Just 0.75, Nothing, now), ("memory-only", Nothing, Just 0.6, now), ("expired", Just 0.9, Just 0.9, addUTCTime (-48 * 3600) now)]
      , (metric, value) <- catMaybes [("container.cpu.utilization",) <$> cpu, ("container.memory.percent",) <$> memory]
      ]
      \(service, metric, value, stamp) -> ingestMetric tr' samplesKey [mkAttr "service.name" service] [] metric value stamp
    overview <- DashboardModel.readDashboardFile "static/public/dashboards" "_overview.yaml"
    resource <- widgetTitled "Service Resource Usage" $ foldMap DashboardModel.allWidgets overview
    let sample service cpu memory points = V.fromList [AE.String service, AE.toJSON (cpu :: Maybe Double), AE.toJSON cpu, AE.toJSON (memory :: Maybe Double), AE.toJSON memory, AE.toJSON (points :: Int)]
    onBoth samples Nothing (snd $ Widget.tableQuery resource Nothing) (V.fromList [sample "cpu-only" (Just 75) Nothing 1, sample "paired" (Just 25) (Just 0.4) 2, sample "memory-only" Nothing (Just 0.6) 1])

    -- Through the dashboard JSON API, with a client sort that cannot inject SQL.
    dashId <- newDashboard tr' "" "Resource table API"
    void $ toBaseServantResponse tr' $ Api.apiDashboardWidgetUpsert testPid dashId resource{Widget.id = Just "resources", Widget.dbSource = Just Hasql.SqlPostgres}
    for_
      [ (testPid, stamp, service, metric, value, service)
      | (service, cpu, memory, stamp) <- [("backend" :: Text, 0.25, 0.4, now), ("frontend", 0.5, 0.8, now), ("expired", 0.9, 0.9, addUTCTime (-48 * 3600) now)]
      , (metric, value) <- [("container.cpu.utilization" :: Text, cpu), ("container.memory.percent", memory :: Double)]
      ]
      $ dbExec tr' [sql| INSERT INTO otel_metrics (project_id, timestamp, id, series_id, metric_name, metric_type, value, resource___service___name) VALUES (?, ?, gen_random_uuid(), ?, ?, 'GAUGE', ?, ?) |]
    let backend = ["backend", "25.0", "25.0", "0.4", "0.4", "2"]
        frontend = ["frontend", "50.0", "50.0", "0.8", "0.8", "2"]
    for_ [(Nothing :: Maybe Text, [frontend, backend]), (Just "+service", [backend, frontend]), (Just "-service", [frontend, backend]), (Just "-service; SELECT 1", [frontend, backend])] \(sortParam, expected) -> do
      result <- toBaseServantResponse tr' $ Dashboards.apiDashboardData testPid dashId Nothing (Just "resources") (Just "24h") Nothing Nothing [("table-sort", sortParam)]
      [(w.error, w.textRows, w.columns) | w <- result.widgets :: [ApiTypes.RenderedWidget]] `shouldBe` [(Nothing, expected, ["Service/Container", "CPU Avg", "CPU Max", "Mem Avg", "Mem Max", "Samples"])]

    -- The sort reaches rows outside the original top twenty, keeps numbers numeric, and rejects injection.
    let fetch widget sortParam = fmap (.dataText) $ runQueryEffect tr' do
          let (query, sqlQ) = Widget.tableQuery widget sortParam
          Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTText) (Just testPid) query sqlQ Nothing Nothing Nothing Nothing Nothing []
        numbers ns = V.fromList [V.singleton $ show @Text n | n <- ns :: [Int]]
    traverse (fetch tableWidget) [Nothing, Just "-duration", Just ("-duration; DROP TABLE users" :: Text)] `shouldReturn` [numbers [1 .. 20], numbers [25, 24 .. 6], numbers [1 .. 20]]
    eager <- forM [[], [("table-sort", Just "-duration")] :: [(Text, Maybe Text)]] \ps -> call tr' $ Routes.widgetGetH testPid (Just $ Utils.encodeText tableWidget) Nothing Nothing Nothing Nothing ps
    goldenHtml "dashboard-table-sort" $ traverse_ (toHtmlRaw . fold . (.html)) eager
    -- Every sortable column a template ships, both directions, past the default top twenty.
    templatesOnDisk <- DashboardModel.readDashboardsFromDisk "static/public/dashboards"
    let cols = [col | w <- foldMap DashboardModel.allWidgets templatesOnDisk, w.wType == Widget.WTTable, col <- fold w.columns, col.sortable == Just True]
    cols `shouldSatisfy` not . null
    for_ cols \col -> do
      let widget = (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.columns = Just [col], Widget.sql = Just $ "SELECT n AS \"" <> T.replace "\"" "\"\"" col.field <> "\" FROM generate_series(1, 25) n ORDER BY {{table_sort}} LIMIT 20", Widget.defaultSort = Widget.mkSqlOrder "abs(n - 13) ASC"}
      original <- fetch widget Nothing
      sorted <- traverse (fetch widget . Just . (<> col.field)) ["+", "-" :: Text]
      (col.field, sorted) `shouldBe` (col.field, [numbers [1 .. 20], numbers [25, 24 .. 6]])
      (col.field, map (V.any (`notElem` original)) sorted) `shouldBe` (col.field, [True, True])

  it "endpoint analytics: client spans count, selectors build route filters, pickers gate the widget phase, swaps ship skeletons" \tr -> do
    -- Every widget once pinned kind=="server", blanking the dashboard for outgoing (client-span)
    -- endpoints, ~56% of a project's. Reported for dellyman.com/api/v3.0/GetQuotes.
    dbExec tr [sql| INSERT INTO otel_logs_and_spans (id, project_id, timestamp, start_time, date, name, kind, status_code, duration, hashes, summary) VALUES (gen_random_uuid(), ?, ?, ?, ?, 'POST', 'client', '200', 5000000, ARRAY['ba3431d5'], ARRAY['POST']) |] (testPid.toText, frozenTime, frozenTime, frozenTime)
    let buckets q = (\md -> (md.error, V.length md.dataset)) <$> runQueryEffect tr (Charts.queryMetrics (Just Hasql.SqlPostgres) (Just Charts.DTMetric) (Just testPid) (Just $ q <> "hashes[*]==\"ba3431d5\" | summarize count() by bin_auto(timestamp)") Nothing (Just "24H") Nothing Nothing Nothing Nothing [])
    buckets "" >>= (`shouldSatisfy` \(err, n) -> isNothing err && n > 0)
    snd <$> buckets "kind==\"server\" AND " `shouldReturn` 0
    template <- decodeUtf8 <$> readFileBS "static/public/dashboards/endpoint-stats.yaml"
    (T.isInfixOf "{{const-endpointFilter}}" template, T.isInfixOf "kind" template) `shouldBe` (True, False)

    dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint Analytics"
    let endpoint :: Bool -> Text -> Text -> Text -> Text -> Endpoints.Endpoint
        endpoint outgoing host method hash path = (def :: Endpoints.Endpoint){Endpoints.projectId = testPid, Endpoints.host = host, Endpoints.method = method, Endpoints.urlPath = path, Endpoints.hash = hash, Endpoints.outgoing = outgoing}
        routelift = endpoint True "integrations.routelift.com" "POST"
    runQueryEffect tr $ Endpoints.bulkInsertEndpoints $ V.fromList [routelift "literal-route" "/v1/deliveries/estimate/v2", routelift "template-route" "/v1/deliveries/{id}", endpoint True "" "POST" "hostless-route" "/health", routelift "faa041ff" "/v1/deliveries/estimate/v2", endpoint False "dellyman.com" "GET" "ba3431d5" "/orders"]
    let tabParams slug since hx ps = call tr (Dashboards.dashboardTabGetH testPid dashId slug Nothing Nothing Nothing since hx ps) <&> \r -> let Dashboards.DashboardGet _ _ _ _ params = tabDashboard r in params
        filters ps = tabParams "overview" (Just "24H") tabsContent ps <&> \params -> (join $ lookup "const-endpointFilter" params, join $ lookup "const-endpointFilter-kql" params)
        routeliftHash hash = filters [("var-host", Just "integrations.routelift.com"), ("var-endpointHash", Just hash)]
        literal = (Just "attributes___server___address IN ('integrations.routelift.com') AND attributes___http___request___method IN ('POST') AND (attributes___http___route IN ('/v1/deliveries/estimate/v2') OR ((attributes___http___route IS NULL OR attributes___http___route = '') AND attributes___url___path IN ('/v1/deliveries/estimate/v2')))", Just "attributes.server.address in (\"integrations.routelift.com\") AND attributes.http.request.method in (\"POST\") AND (attributes.http.route in (\"/v1/deliveries/estimate/v2\") or (isempty(attributes.http.route) and attributes.url.path in (\"/v1/deliveries/estimate/v2\")))")
    -- A literal route falls back to its path; a templated one keeps its hash.
    traverse routeliftHash ["literal-route", "template-route", "hostless-route", "" :: Text]
      `shouldReturn` [ literal
                     , (Just "(attributes___server___address IN ('integrations.routelift.com') AND attributes___http___request___method IN ('POST') AND attributes___http___route IN ('/v1/deliveries/{id}') OR hashes @> ARRAY['template-route'])", Just "(attributes.server.address in (\"integrations.routelift.com\") AND attributes.http.request.method in (\"POST\") AND attributes.http.route in (\"/v1/deliveries/{id}\") or hashes[*] in (\"template-route\"))")
                     , (Just "hashes @> ARRAY['hostless-route']", Just "hashes[*] in (\"hostless-route\")")
                     , (Just "hashes @> ARRAY['']", Just "hashes[*] in (\"\")")
                     ]
    traverse (fmap (fmap parseQueryToAST . snd) . routeliftHash) ["literal-route", "template-route", "hostless-route", "" :: Text] >>= (`shouldSatisfy` all (maybe False isRight))
    -- Stale constants carried on the URL are recomputed from the selected endpoint.
    filters [("const-endpointFilter", Just "hashes @> ARRAY['']"), ("const-endpointFilter-kql", Just "hashes[*] in (\"\")"), ("var-endpointHash", Just "faa041ff"), ("var-host", Just "integrations.routelift.com")] `shouldReturn` literal
    dependencies <- tabDashboard <$> getTab tr dashId "dependencies" Nothing [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "endpoint / hash")]
    let mapWidget = find ((== Widget.WTServiceMap) . (.wType)) (renderedWidgets dependencies)
    ((\c -> (c.title, c.url)) <$> (mapWidget >>= (.cta))) `shouldBe` Just ("Open Service Map", "/p/" <> testPid.toText <> "/service_map?endpoint_hash=endpoint%20%2F%20hash&since=24H")

    -- The picker replaced the tab, yet the handler first ran the whole widget phase with the
    -- required variable as '' (5.4s vs 5.5s for the populated dashboard). Server work shows as
    -- rendered html or a fetched dataset; `eager` alone is declared by the template.
    gated <- tabDashboard <$> getTab tr dashId "overview" Nothing [("var-host", Just "dellyman.com")]
    let prefilled = map (fromMaybe "<untitled>" . (.title)) . filter (\w -> isJust w.html || isJust w.dataset)
    renderedWidgets gated `shouldSatisfy` not . null
    prefilled (renderedWidgets gated) `shouldBe` []
    -- Successive pickers stay inside the dashboard fragment; the dependent Endpoint input keeps
    -- its `{{var-host}}` template so a new domain re-fetches its own endpoints.
    domainPicker <- partialHtml =<< getTab tr dashId "overview" tabsContent []
    endpointPicker <- partialHtml =<< getTab tr dashId "overview" tabsContent [("var-endpointHash", Just ""), ("var-host", Just "dellyman.com")]
    -- A full load keeps the server prefill; a swap already has a painted page, so ships skeletons
    -- (lazyWidget strips `eager` too, which renderStatContent reads as "data is here").
    let selected = [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "ba3431d5")]
        eagerOnes = prefilled . filter (\w -> w.eager == Just True || isJust w.html || isJust w.dataset)
    full <- getTab tr dashId "overview" Nothing selected
    eagerOnes (renderedWidgets $ tabDashboard full) `shouldSatisfy` not . null
    T.count "id=\"log_details_container\"" (renderT full) `shouldBe` 1
    swapped <- getTab tr dashId "overview" tabsContent selected
    renderedWidgets (tabDashboard swapped) `shouldSatisfy` not . null
    eagerOnes (renderedWidgets $ tabDashboard swapped) `shouldBe` []
    -- A tab navigation is the shared fragment contract, reflecting URL values into the variables bar.
    errorsTab <- partialHtml =<< getTab tr dashId "errors" tabsContent selected
    goldenParts "dashboard-endpoint-tabs" $
      foldMap (elementsWith ["id=\"dashboard-tabs-content\"", "name=\"endpointHash\""] . renderT) [domainPicker, endpointPicker]
        <> tagsWith ["swap-oob=", "id=\"dashboard-tabs-content\"", "class=\"dash-variable-input"] (renderT errorsTab)
        <> elementsWith ["tab-active\""] (renderT errorsTab)
        <> [toStrict $ fold $ mapWidget >>= (.html)]

    -- Shipbubble 2026-10-19: 17 MySQL errors in 1.4M queries showed as "0.00%", and the
    -- Databases tab had no way to read the error itself.
    let dbTab = [t | d <- dashboardTemplatesCompiled, d.file == Just "_overview.yaml", t <- fold d.tabs, t.name == "Databases"]
        dbWidgets = concatMap (\w -> w : fold w.children) (concatMap (.widgets) dbTab)
    [c.field | w <- dbWidgets, c <- fold w.columns, c.field == "error_rate"] `shouldBe` []
    dbPid <- createTestProject tr "db-tab-errors"
    dbKey <- createTestAPIKey tr dbPid "db-tab-key"
    ingestSpanReq tr =<< createOtelTraceWithExceptionAtTime dbKey ([("db.system.name", "mysql"), ("db.query.text", "update orders set x = ?")], [], []) "update orders" "QueryError" "Deadlock found when trying to get lock; try restarting transaction" "" frozenTime
    let errTable = find ((== Just "Recent Database Errors") . (.title)) dbWidgets
    md <- eventually (runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just dbPid) Nothing (snd . (`Widget.tableQuery` Nothing) =<< errTable) (Just "24H") Nothing Nothing Nothing Nothing [("var-database", Just "mysql")]) (not . V.null . (.dataText))
    (md.error, V.toList (V.take 2 <$> md.dataText)) `shouldBe` (Nothing, [V.fromList ["Deadlock found when trying to get lock; try restarting transaction", "update orders set x = ?"]])

    -- Sidebar navigation targets #main-content, so it must get the full page, not a tab fragment.
    rumId <- newDashboard tr "rum.yaml" "RUM sidebar navigation"
    call tr (Dashboards.dashboardTabGetH testPid rumId "overview" Nothing Nothing Nothing (Just "24H") (Just "div#main-content") []) >>= \case
      NavigationFull _ -> pass
      NavigationPartial _ _ -> expectationFailure "sidebar navigation returned a tab fragment, blanking the page"

  -- RUM is this template plus the Explorer's sessions view; it must hold up on real browser telemetry.
  it "RUM: every widget runs, vitals read histogram buckets, audiences count sessions once, redirects keep the window" \tr -> do
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
    -- No var-app, as on a first visit: an unset variable reads as every application.
    let from = toText $ iso8601Show $ at (-3600)
        to = toText $ iso8601Show $ at 3600
        dataType = \case Widget.WTTable -> Charts.DTText; Widget.WTStat -> Charts.DTFloat; _ -> Charts.DTMetric
        run pid ps ws = forM ws \w -> (w.title,) <$> runQueryEffect tr (Charts.queryMetrics Nothing (Just $ dataType w.wType) (Just pid) w.query w.sql Nothing (Just from) (Just to) Nothing Nothing ps)
        widgetNamed t = widgetTitled t (DashboardModel.allWidgets rum)
        table rs title = maybe V.empty (.dataText) $ lookup (Just title) rs
        vitalRow rs name = V.toList . V.take 5 <$> find ((== Just name) . (V.!? 0)) (table rs "Core Web Vitals")
    results <- run projectId [] [w | w <- DashboardModel.allWidgets rum, isJust w.sql]
    [(title, m.error) | (title, m) <- results, isJust m.error] `shouldBe` []
    -- A vital with nothing in the window says so instead of vanishing.
    map (vitalRow results) ["LCP", "INP", "CLS", "FCP"]
      `shouldBe` [ Just ["LCP", "2500 ms", "rating;success-strong⇒Good", "1000 – 2500", "16"]
                 , Just ["INP", "≈ 250 ms", "rating;warning-weak⇒Likely needs improvement", "100 – 300", "4"]
                 , Just ["CLS", "—", "rating;info-weak⇒Buckets too coarse to rate", "0 – 5", "4"]
                 , Just ["FCP", "—", "rating;info-weak⇒No observations in this window", "", "0"]
                 ]
    vitals <- widgetNamed "Core Web Vitals"
    -- An SDK upgrade mixes bucket layouts: a [2500]-bounded export coarsens every point to the
    -- shared 2500 edge, giving [16, 4] over 20, so the 15th observation sits at 2500·15/16.
    exportVital "lcp" [point (-5) (-10) [4, 0] & PMF.explicitBounds .~ [2500]]
    mixed <- run projectId [] [vitals]
    vitalRow mixed "LCP" `shouldBe` Just ["LCP", "2344 ms", "rating;success-strong⇒Good", "0 – 2500", "20"]
    sort [name | row <- V.toList $ table results "Devices", Just name <- [row V.!? 0]] `shouldBe` ["Desktop", "Mobile"]
    sort (V.toList $ V.mapMaybe (V.!? 0) $ table results "Top pages") `shouldBe` ["/cart", "/orders/:id"]
    -- Each page row's hidden Explorer query finds the events it counted, and its link carries it with the dashboard's window.
    forM_ (table results "Top pages") \row -> do
      events <- call tr $ Log.logExplorerDataH projectId def{Log.query = row V.!? 3, Log.from = Just from, Log.to = Just to}
      (row V.!? 0, events.error, events.queryResultCount) `shouldBe` (row V.!? 0, Nothing, 1)
    topPages <- widgetNamed "Top pages"
    let topPagesHtml = Widget.renderTableWithDataAndParams topPages{Widget._projectId = Just projectId} (table results "Top pages") [("since", Just "7D")]
    map (\ps -> (lookup "since" ps, lookup "query" ps)) (linkParams ("/p/" <> projectId.toText <> "/log_explorer") (renderT topPagesHtml))
      `shouldBe` [(Just "7D", row V.!? 3) | row <- V.toList $ table results "Top pages"]
    -- Abbreviations explain themselves on hover; the hidden query column stays off screen.
    goldenHtml "dashboard-rum-tables" do Widget.renderTableWithDataAndParams vitals (table results "Core Web Vitals") []; topPagesHtml
    -- The browser filter exists as SQL for widgets and KQL for the Explorer; both select the same rows, and a backend span neither.
    ingestTrace tr apiKey "GET /backend-only" frozenTime
    sessions <- call tr $ Log.logExplorerDataH projectId def{Log.query = Just browserKql, Log.from = Just from, Log.to = Just to}
    viaSql <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTFloat) (Just projectId) Nothing (Just "SELECT count(*)::float FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND {{browser_filter}}") Nothing (Just from) (Just to) Nothing Nothing []
    (sessions.error, sessions.queryResultCount, viaSql.dataFloat) `shouldBe` (Nothing, 2, Just 2)

    -- Audience: each session counts once, unknown agents are kept, cohorts filter.
    pid <- createTestProject tr "RUM audience"
    audienceKey <- createTestAPIKey tr pid "audience-key"
    let emit n session ua attrs = ingestSpanReq tr $ mkSpanRequest ("a" <> T.justifyRight 31 '0' (show @Text n)) ("a" <> T.justifyRight 15 '0' (show @Text n)) Nothing "documentLoad" [] Nothing (mkAttr "session.id" session : attrs) (mkResource audienceKey $ mkAttr "service.name" "storefront" : [mkAttr "user_agent.original" agent | agent <- toList ua]) frozenTime
    emit (1 :: Int) "safari" (Just "iPhone Mobile Safari/604") [mkAttr "user.id" "alice", mkAttr "user.email" "alice@example.com"]
    emit 2 "safari" (Just "iPhone Mobile Safari/604") [mkAttr "user.id" "alice", mkAttr "exception.type" "TypeError"]
    emit 3 "chrome" (Just "Windows Chrome/151 Safari/537") [mkAttr "user.id" "bob"]
    emit 4 "unknown" Nothing []
    emit 5 "" (Just "Windows Chrome/151") [mkAttr "exception.type" "TypeError"]
    let readTable ps title n = do
          w <- widgetNamed title
          [(_, result)] <- run pid ps [w]
          result.error `shouldBe` Nothing
          pure $ sort $ map (V.toList . V.take n) $ V.toList result.dataText
    readTable [] "Browsers" 5 `shouldReturn` [["Chrome", "1", "33.3", "0", "0.0"], ["Safari", "1", "33.3", "1", "100.0"], ["Unknown", "1", "33.3", "0", "0.0"]]
    readTable [("var-browser", Just "Safari")] "Devices" 5 `shouldReturn` [["Mobile", "1", "100.0", "1", "100.0"]]
    readTable [("var-errors", Just "true")] "Identified users" 3 `shouldReturn` [["alice@example.com", "1", "1"]]
    readTable [("table-search", Just "ALICE")] "Identified users" 3 `shouldReturn` [["alice@example.com", "1", "1"]]
    traverse (\ps -> readTable ps "Browsers" 1) [[("var-app", Just "missing-app")], [("environment", Just "prod")] :: [(Text, Maybe Text)]] `shouldReturn` [[], []]
    readTable [("table-search", Just "missing-user")] "Identified users" 1 `shouldReturn` []
    forM_ ([([("var-browser", Just "Safari")], 2), ([("var-os", Just "iOS"), ("var-device", Just "Mobile")], 2), ([("var-browser", Just "Unknown")], 1), ([("var-errors", Just "true")], 2)] :: [([(Text, Maybe Text)], Int)]) \(ps, expected) -> do
      query <- (lookup "query" <=< listToMaybe) . explorerLinkParams pid <$> rumLocation tr pid ["sessions"] ps
      events <- call tr $ Log.logExplorerDataH pid def{Log.query = query, Log.from = Just from, Log.to = Just to}
      (ps, events.error, events.queryResultCount) `shouldBe` (ps, Nothing, expected)

    -- /rum and its older /rum/dashboard open the dashboard, /rum/sessions the Explorer; links
    -- from emails and alerts carry a window, which both keep.
    let location = rumLocation tr testPid
        window = [("since", Just "7D"), ("environment", Just "prod")]
    dashboard <- location [] window
    dashboard `shouldContainAll` ["/p/" <> testPid.toText <> "/dashboards/", "since=7D"]
    location ["dashboard"] window `shouldReturn` dashboard
    sessionsUrl <- location ["sessions"] window
    -- A trailing slash captures an empty last segment; it must not 404.
    location ["sessions", ""] window `shouldReturn` sessionsUrl
    explorerLinkParams testPid sessionsUrl `shouldBe` [[("since", "7D"), ("viz_type", "sessions"), ("query", browserKql)]]
    -- A link shared from the old page lands on that session.
    explorerLinkParams testPid <$> location [] [("tab", Just "sessions"), ("session", Just "abc")] `shouldReturn` [[("viz_type", "sessions"), ("query", "attributes.session.id == \"abc\"")]]

  it "dashboard list: zero state, template picker, telemetry-free shell, create, tags, rename, delete, duplicate, bulk teams and delete, star and sort" \tr -> do
    let filters = Dashboards.DashboardFilters []
        fetch tr' previews fs = call tr' (Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing previews fs)
        page tr' previews fs =
          fetch tr' previews fs >>= \case
            Dashboards.DashboardsGet pc -> pure pc
            Dashboards.DashboardsGetSlim _ -> fail "expected the full dashboard list"
        listed = page tr Nothing filters <&> \(PageCtx _ d) -> V.toList d.dashboards
        titled t = listed >>= maybe (fail $ "no dashboard titled " <> toString t) pure . find ((== t) . (.title))
        -- In one request, as the page does; the test UUID supply would otherwise repeat ids.
        create ts = map (show @Text . Servant.getResponse) <$> call tr (traverse (\t -> Dashboards.dashboardsPostH testPid Dashboards.DashboardForm{Dashboards.title = t, Dashboards.file = "overview.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}) ts >>= addRespHeaders)
    -- The empty state opens the new-dashboard modal (a <label for>), not a dead href="newDashboardMdl".
    zero <- renderT <$> fetch tr Nothing filters{Dashboards.tag = ["no-such-tag-so-the-list-is-empty"]}
    zero `shouldContainAll` ["No dashboards yet", "for=\"newDashboardMdl\""]
    zero `shouldNotSatisfy` T.isInfixOf "href=\"newDashboardMdl\""
    -- Template previews load only once the modal opens.
    initial <- renderT <$> fetch tr Nothing filters
    initial `shouldNotSatisfy` T.isInfixOf "dashboardTemplatePreviews"
    initial `shouldContainAll` ["hx-get=\"/p/" <> testPid.toText <> "/dashboards/new\"", "hx-trigger=\"change[target.checked] from:#newDashboardMdl\""]
    picker <- renderT <$> fetch tr (Just "true") filters
    PageCtx _ pd <- page tr (Just "true") filters
    picker `shouldContainAll` (["value=\"Blank dashboard\"", "data-title=\"Nginx\"", "Requests"] <> [("data-template=\"" <> fromMaybe "" t.file <> "\"") | t <- pd.dashTemplates])
    picker `shouldNotSatisfy` T.isInfixOf "http-stats.svg"
    -- Deployed environments serve templates compiled in, never re-reading YAML from the working directory.
    withSystemTempDirectory "dashboard-templates" \dir -> withCurrentDirectory dir $ for_ [Config.Prod, Config.Staging] \env -> do
      PageCtx _ d <- page tr{trATCtx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.environment = env, Config.liveReloadDashboards = True}}} Nothing filters
      d.dashTemplates `shouldSatisfy` (not . null)
    -- The list skips the telemetry scope pickers but keeps the rest of the shell.
    let scopeFields = ["service.name", "deployment.environment.name"]
    runHasqlEffect tr $ SchemaCatalog.upsertSummary $ V.singleton (testPid, SchemaLearning.SummaryDoc (HM.fromList [(k, SchemaLearning.FieldStruct mempty mempty SchemaLearning.FCResource False) | k <- scopeFields]) (V.singleton "checkout") (HM.fromList [(k, SchemaLearning.TopK 1 (HM.singleton "production" 1)) | k <- scopeFields]))
    (_, (_, _, shell)) <- testServant tr $ BodyWrapper.mkPageCtx testPid >>= addRespHeaders
    shell.envOptions `shouldBe` V.singleton "production"
    PageCtx bw _ <- page tr Nothing filters
    (bw.envOptions, bw.serviceOptions, map (.conversationId) bw.conversations, isJust bw.activationProgressM) `shouldBe` (V.empty, V.empty, map (.conversationId) shell.conversations, isJust shell.activationProgressM)

    create ["Dashboard 1", "Dashboard 2", "Test Dashboard"] `shouldReturn` replicate 3 "DashboardNoContent"
    create [""] `shouldReturn` ["DashboardPostError \"Dashboard title is required\""]
    -- Removing one tag filter deleted "&tag=prod" by substring, mangling "&tag=production" too.
    let tagged = UUIDId $ UUID.fromWords 0xda5 0x7a9 0x9 0x1
    _ <- runTestBg frozenTime tr $ DashboardModel.insert (DashboardModel.mkDashboardVM tagged testPid frozenTime (Projects.UserId UUID.nil)){DashboardModel.title = "Tagged", DashboardModel.tags = ["prod", "production"]}
    tags <- renderT <$> fetch tr Nothing filters{Dashboards.tag = ["prod", "production"]}
    let removeLink tag = T.takeWhile (/= '"') . T.drop 8 . snd . T.breakOn "hx-get=\"" . snd $ T.breakOn ("aria-label=\"Remove filter: " <> tag <> "\"") tags
        base = "/p/" <> testPid.toText <> "/dashboards?sort=-updated_at"
    map removeLink ["prod", "production"] `shouldBe` [base <> "&amp;tag=production", base <> "&amp;tag=prod"]
    void $ runTestBg frozenTime tr $ DashboardModel.deleteDashboardsByIds testPid [tagged]

    renamed <- titled "Test Dashboard"
    _ <- testServant tr $ Dashboards.dashboardRenamePatchH testPid renamed.id Dashboards.DashboardRenameForm{Dashboards.title = "Updated Dashboard", Dashboards.fileDir = Nothing}
    removed <- titled "Dashboard 1"
    show @Text <$> call tr (Dashboards.dashboardDeleteH testPid removed.id) `shouldReturn` "DashboardNoContent"
    _ <- titled "Updated Dashboard" >>= testServant tr . Dashboards.dashboardDuplicatePostH testPid . (.id)
    sort . map (.title) <$> listed `shouldReturn` ["Dashboard 2", "Updated Dashboard", "Updated Dashboard (Copy)"]

    -- Bulk-adding teams adds exactly the selected ones, plus the implicit @everyone.
    let team = ManageMembers.TeamForm{teamName = "Hello", teamDescription = "", teamHandle = "hello", notifEmails = [], teamMembers = [], discordChannels = [], slackChannels = [], phoneNumbers = [], pagerdutyServices = [], teamId = Nothing}
    for_ ["hello", "hii", "broo"] \h -> testServant tr $ ManageMembers.manageTeamPostH testPid team{ManageMembers.teamHandle = h} Nothing
    teams <-
      call tr (ManageMembers.manageTeamsGetH testPid (Just "")) >>= \case
        ManageMembers.ManageTeamsGet' (_, _, _, _, ts) -> pure ts
        _ -> fail "expected ManageTeamsGet'"
    dIds <- map (.id) . filter ((/= "Updated Dashboard (Copy)") . (.title)) <$> listed
    _ <- testServant tr $ Dashboards.dashboardBulkActionPostH testPid Dashboards.BAAddTeams Dashboards.DashboardBulkActionForm{itemId = dIds, teamIds = V.toList $ V.map (.id) $ V.filter ((/= "broo") . (.handle)) teams}
    let handles db = sort $ mapMaybe (\tid -> (.handle) <$> V.find ((== tid) . (.id)) teams) (V.toList db.teams)
    sort . map (\db -> (db.title, handles db)) <$> listed
      `shouldReturn` [("Dashboard 2", ["everyone", "hello", "hii"]), ("Updated Dashboard", ["everyone", "hello", "hii"]), ("Updated Dashboard (Copy)", ["everyone"])]
    listed >>= \ds -> void $ testServant tr $ Dashboards.dashboardBulkActionPostH testPid Dashboards.BADelete Dashboards.DashboardBulkActionForm{itemId = map (.id) ds, teamIds = []}
    null <$> listed `shouldReturn` True

    -- Starring toggles, and starred dashboards sort first.
    create ["Dashboard A", "Dashboard B", "Dashboard C", "Star Test Dashboard"] `shouldReturn` replicate 4 "DashboardNoContent"
    star <- titled "Star Test Dashboard"
    star.starredSince `shouldSatisfy` isNothing
    for_ [isJust, isNothing] \starred -> do
      _ <- testServant tr $ Dashboards.dashboardStarPostH testPid star.id
      (.starredSince) <$> titled "Star Test Dashboard" >>= (`shouldSatisfy` starred)
    for_ ["Dashboard C", "Dashboard A"] \t -> titled t >>= testServant tr . Dashboards.dashboardStarPostH testPid . (.id)
    map (isJust . (.starredSince)) <$> listed `shouldReturn` [True, True, False, False]

  it "dashboard page and templates: variables, tabs, settle probe, URL precedence; template SQL runs; KQL widgets filter, fail and recover" \tr -> do
    let var k m v = DashboardModel.Variable{key = k, title = Nothing, multi = m, required = Nothing, reloadOnChange = Nothing, helpText = Nothing, _vType = DashboardModel.VTValues, sql = Nothing, facetField = Nothing, query = Nothing, options = Nothing, value = v, dependsOn = Nothing, sqlSig = Nothing}
        render dash = renderT $ Dashboards.DashboardGet testPid (UUIDId UUID.nil) dash (DashboardModel.mkDashboardVM (UUIDId UUID.nil) testPid frozenTime (Projects.UserId UUID.nil)){DashboardModel.schema = Just dash, DashboardModel.title = "t"} []
        tab n = (def :: DashboardModel.Tab){DashboardModel.name = n}
    -- Lucid merged a duplicated data-tagify-mode, silently making multi-select variables single-select.
    let vars = render (def :: DashboardModel.Dashboard){DashboardModel.variables = Just [var "single" Nothing Nothing, var "multi" (Just True) Nothing], DashboardModel.widgets = [(def :: Widget.Widget){Widget.wType = Widget.WTTimeseriesLine}]}
    T.count "data-tagify-mode" vars `shouldBe` 1
    vars `shouldContainAll` ["data-tagify-mode=\"select\"", "sticky top-0 z-10", "dashboard-grid-wrapper relative isolate z-0"]
    -- Tabs use the shared navigation contract (htmx 4 swap names), no /content partial and no hyperscript.
    let tabs = render (def :: DashboardModel.Dashboard){DashboardModel.tabs = Just [tab "Overview", tab "Errors"]}
    tabs `shouldContainAll` ["hx-target=\"#dashboard-tabs-content\"", "hx-swap=\"outerMorph\"", "hx-push-url=\"true\"", "/dashboards/00000000-0000-0000-0000-000000000000/settled", "duration_ms", "response_bytes", "htmx:before:request", "htmx:after:settle", "keepalive: true"]
    filter (`T.isInfixOf` tabs) ["hx-select=\"", "/content", "remove .tab-active"] `shouldBe` []
    -- The settle probe accepts only its closed navigation vocabulary, and only from members.
    let sample n = AE.eitherDecode @Dashboards.DashboardSettleSample $ "{\"duration_ms\":1,\"response_bytes\":2,\"navigation\":\"" <> n <> "\"}"
    (sample "initial", sample "tab", isLeft (sample "typo")) `shouldBe` (Right (Dashboards.DashboardSettleSample 1 2 Dashboards.DashboardInitial), Right (Dashboards.DashboardSettleSample 1 2 Dashboards.DashboardTab), True)
    victim <- createTestProject tr "victim"
    _ <- testServant tr $ Dashboards.dashboardsPostH victim Dashboards.DashboardForm{Dashboards.title = "Victim", Dashboards.file = "overview.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
    victimDash <- runTestBg frozenTime tr (DashboardModel.selectDashboardsSortedBy victim DashboardModel.Asc DashboardModel.DSTitle) >>= maybe (fail "no victim dashboard") pure . viaNonEmpty head
    assertNonMemberDenied tr victim $ Dashboards.dashboardSettledPostH victim victimDash.id (Dashboards.DashboardSettleSample 1 2 Dashboards.DashboardInitial)
    -- URL values beat declared defaults, so a shared link carries its selections; a cleared value stays cleared.
    let resolve ps = sortOn fst $ Dashboards.addVariableDefaults ps (Just [var "env" Nothing (Just "prod"), var "region" Nothing (Just "eu")])
    map resolve [[], [("var-env", Just "staging")], [("var-env", Nothing)]]
      `shouldBe` [[("var-env", Just "prod"), ("var-region", Just "eu")], [("var-env", Just "staging"), ("var-region", Just "eu")], [("var-env", Nothing), ("var-region", Just "eu")]]
    Dashboards.addVariableDefaults [("since", Just "1H")] Nothing `shouldBe` [("since", Just "1H")]

    -- Templates: the overview reads the native metric store, every SQL stat decodes as a number
    -- (`…::text || '%'` rendered "Query execution failed"), and the endpoint template's declared
    -- SQL source, not its query text, routes to Postgres.
    overview <- DashboardModel.readDashboardFile "static/public/dashboards" "_overview.yaml"
    overview `shouldSatisfy` isJust
    filter (`T.isInfixOf` show overview) ["telemetry.metrics", "metric_value"] `shouldBe` []
    stats <- filter (\w -> w.wType == Widget.WTStat && isJust w.sql) . foldMap DashboardModel.allWidgets <$> DashboardModel.readDashboardsFromDisk "static/public/dashboards"
    length stats `shouldSatisfy` (> 3)
    for_ stats \w -> do
      md <- runQueryEffect tr $ runConcurrent $ Dashboards.widgetMetrics testPid (Just "24h", Nothing, Nothing) [("const-endpointFilter", Just "hashes @> ARRAY['']")] w
      (w.title, md.error :: Maybe Text) `shouldBe` (w.title, Nothing)
    endpoint <- DashboardModel.readDashboardFile "static/public/dashboards" "endpoint-stats.yaml"
    map (fmap (.source) . (.sql)) (fold $ endpoint >>= (.variables)) `shouldBe` [Just LogQueries.SqlPostgres, Just LogQueries.SqlPostgres]
    noApisPool <- mkHasqlPool 1 (tr.trConnStr <> " dbname=postgres")
    runQueryEffect tr{trATCtx = tr.trATCtx{Config.hasqlTimefusionPool = noApisPool}} (LogQueries.executeSecuredQuery True testPid (LogQueries.SecuredSql LogQueries.SqlPostgres $ "SELECT COUNT(*)::bigint FROM apis.endpoints WHERE project_id='" <> testPid.toText <> "' AND 'otel_logs_and_spans' = 'otel_logs_and_spans'") 1)
      >>= (`shouldSatisfy` isRight)
    -- The dashboard SQL-source migrations have already run in production.
    forM ["0119_endpoint_dashboard_sql_source", "0120_repair_dashboard_sql_source"] (\m -> show @Text . Crypto.hash @_ @Crypto.MD5 <$> readFileBS ("static/migrations/" <> m <> ".sql"))
      `shouldReturn` ["27fc8228a167e579b4b7124893921372", "19ac1c6651e59fb3860366eec198201f"]

    -- KQL widgets filter by time range and URL variables, report a malformed query as widget-local
    -- data, and recover on the next request (the browser's retry contract). A stat that only filters
    -- grows a count rather than a binned series its decoder cannot read.
    apiKey <- createTestAPIKey tr testPid "stat-widget-key"
    ingestTrace tr apiKey "GET /api/stat-widget" frozenTime
    ingestTrace tr apiKey "GET /api/other-widget" frozenTime
    ingestTrace tr apiKey "GET /api/old-widget" (addUTCTime (-48 * 60 * 60) frozenTime)
    let run params wt q = runQueryEffect tr $ runConcurrent $ Dashboards.widgetMetrics testPid (Just "24h", Nothing, Nothing) params (def :: Widget.Widget){Widget.wType = wt, Widget.query = Just q}
    logs <- run [] Widget.WTLogs "name == \"GET /api/stat-widget\""
    table <- run [] Widget.WTTable "name != null | summarize count(*) by service"
    series <- run [] Widget.WTTimeseries "name != null"
    map (.error) [logs, table, series] `shouldBe` [Nothing, Nothing, Nothing :: Maybe Text]
    (V.length logs.dataText, V.null table.dataText, V.null series.dataset) `shouldBe` (1, False, False)
    for_ ["name != null | summarize dcount(name)", "name != null"] \q -> do
      md <- run [] Widget.WTStat q
      (md.error :: Maybe Text, (> 0) <$> (md.dataFloat :: Maybe Double)) `shouldBe` (Nothing, Just True)
    filtered <- run [("var-operation", Just "GET /api/stat-widget")] Widget.WTLogs "name == \"{{var-operation}}\""
    (filtered.error :: Maybe Text, V.length filtered.dataText) `shouldBe` (Nothing, 1)
    (.error) <$> run [] Widget.WTTimeseries "name ==" >>= (`shouldSatisfy` isJust @Text)
    recovered <- run [] Widget.WTTimeseries "name == \"GET /api/stat-widget\""
    (recovered.error :: Maybe Text, V.null recovered.dataset) `shouldBe` (Nothing, False)
