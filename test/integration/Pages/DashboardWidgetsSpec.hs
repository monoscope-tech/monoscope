-- | The dashboard canvas, end to end: add a widget of every type, move it, resize it,
-- delete it, and confirm the backend still agrees after a reload.
--
-- DashboardsSpec covers dashboard CRUD. Nothing covered the widgets *on* a dashboard —
-- which is the entire product surface — so a widget type could be unsaveable, a drag
-- could silently drop a widget, and no test would notice. These drive the same handlers
-- the browser calls, and enumerate WidgetType via Bounded so a new widget type joins the
-- suite by existing rather than by someone remembering to add it.
module Pages.DashboardWidgetsSpec (spec) where

import Control.Exception qualified as E
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ qualified as SqlQQ
import Lucid (renderText, toHtml)
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.Projects (Session (..))
import Pages.BodyWrapper (BWConfig (..), NavigationResponse (..), PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pages.Dashboards (DashboardFilters (..))
import Pages.Dashboards qualified as Dashboards
import Pkg.Components.Widget qualified as Widget
import Pkg.TestUtils
import Relude
import Servant qualified
import Servant.Types.SourceT qualified as Source
import System.Config qualified as Config
import System.IO.Error (userError)
import System.Timeout (timeout)
import System.Types (addRespHeaders)
import Test.Hspec
import Text.Slugify (slugify)
import Utils qualified
import Web.Auth qualified as Auth
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
  describe "Widget type contract" do
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
      for_ stored \widget -> toStrict (renderText $ Widget.widget_ widget) `shouldSatisfy` T.isInfixOf "gs-w=\"3\""
      let widgets = Widget.normalizeWidgetLayouts stored
      map (\w -> (w.layout >>= (.x), w.layout >>= (.y), w.layout >>= (.w))) widgets
        `shouldBe` [(Just 0, Just 0, Just 3), (Just 3, Just 0, Just 3), (Just 6, Just 0, Just 3), (Just 9, Just 0, Just 3), (Just 0, Just 1, Just 3)]

    it "edits a widget in place and retains an omitted query" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Widget Edit"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTTimeseries "original")
      wid <- firstWidgetId =<< storedWidgets tr dashId

      let edited = (widgetOf Widget.WTStat "renamed"){Widget.query = Nothing, Widget.rawQuery = Nothing}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just wid) Nothing edited
      only <- onlyWidget =<< storedWidgets tr dashId

      only.title `shouldBe` Just "renamed"
      show @Text only.wType `shouldBe` show @Text Widget.WTStat
      only.query `shouldBe` Just "name != null"

    it "compactMetricCard_rendersAtChartHeightAndConfirmsDashboardAdd" \tr -> do
      let metricCard = (widgetOf Widget.WTTimeseriesLine "metric"){Widget.layout = Just def{Widget.x = Just 0, Widget.y = Just 0, Widget.w = Just 2, Widget.h = Just 1}}
      normalized <- onlyWidget $ Widget.normalizeWidgetLayouts [metricCard]
      (normalized.layout >>= (.w), normalized.layout >>= (.h)) `shouldBe` (Just 2, Just 3)
      let html = toStrict $ renderText $ Widget.widget_ metricCard{Widget.expandBtnFn = Just "/details"}
      for_ ["aria-label=\"Expand widget\"", "gap-0.5 flex flex-col", "min-h-8 px-1", "min-h-0 p-3"] \fragment ->
        html `shouldSatisfy` T.isInfixOf fragment

      _ <- newDashboard tr "overview.yaml" "Widget destination"
      (_, picker) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing (Just "true") Nothing (Just "metric-widget") Nothing Nothing noFilters
      toStrict (renderText $ toHtml picker) `shouldSatisfy` T.isInfixOf "successToast"
      (_, dashboards) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
      toStrict (renderText $ toHtml dashboards)
        `shouldSatisfy` T.isInfixOf "aria-label=\"Add dashboard to favorites\""

    it "chartWidget_groupsTitleWithCardAndGuidesTheEmptyState" \_ -> do
      let html = toStrict $ renderText $ Widget.widget_ (widgetOf Widget.WTTimeseriesLine "requests"){Widget.id = Just "requests-chart"}
      html `shouldSatisfy` T.isInfixOf "gap-0.5 flex flex-col"
      html `shouldNotSatisfy` T.isInfixOf "gap-1.5 flex flex-col"
      for_ ["id=\"requests-chart_empty\"", "role=\"status\"", "No data in this time range", "Try a wider time range or adjust the filters."] \fragment ->
        html `shouldSatisfy` T.isInfixOf fragment

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

      void $ runAsBase tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardWidgetPutH testPid dashId Nothing (Just "second-tab") (widgetOf Widget.WTStat "Added To Second")
      void $ runAsBase tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardDuplicateWidgetPostH testPid dashId "second-widget" Nothing
      void $ runAsBase tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.dashboardWidgetReorderPatchH testPid dashId (Just "first-tab") (reorder "first-widget" 6 4 6 2)

      tabOne <- tabWidgets tr dashId "first-tab"
      tabTwo <- tabWidgets tr dashId "second-tab"
      map (\w -> (w.layout >>= (.x), w.layout >>= (.w))) tabOne `shouldBe` [(Just 6, Just 6)]
      map (.title) tabTwo `shouldBe` [Just "Second", Just "Added To Second", Just "Second (Copy)"]
      let tabTwoIds = mapMaybe (.id) tabTwo
      length tabTwoIds `shouldBe` 3
      length (ordNub tabTwoIds) `shouldBe` 3

  -- Rendering, not persistence: a widget the canvas cannot address is a widget the next
  -- drag deletes, because the reorder patch is built by walking `#<id>_widgetEl` elements
  -- and the handler rebuilds the widget list purely from that patch.
  describe "Rendering a widget onto the canvas" do
    it "every widget type renders a grid item the canvas can address and position" \_ -> do
      for_ allWidgetTypes \wt -> do
        let w = (widgetOf wt "Canvas Widget"){Widget.id = Just "wgt-1", Widget.layout = Just def{Widget.x = Just 3, Widget.y = Just 2, Widget.w = Just 6, Widget.h = Just 4}}
            html = toStrict $ renderText $ Widget.widget_ w
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
          html = toStrict $ renderText $ Widget.widget_ w
      T.isInfixOf "wgt-1_widgetEl" html `shouldBe` False

    it "a group renders its children inside its own nested grid" \_ -> do
      let child = (widgetOf Widget.WTStat "Child"){Widget.id = Just "child-1"}
          groupWidget = (widgetOf Widget.WTGroup "Group"){Widget.id = Just "group-1", Widget.children = Just [child]}
          html = toStrict $ renderText $ Widget.widget_ groupWidget
      T.isInfixOf "nested-grid" html `shouldBe` True
      T.isInfixOf "group-1_widgetEl" html `shouldBe` True
      T.isInfixOf "child-1_widgetEl" html `shouldBe` True

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
      let rendered = case pg of PageCtx _ d -> toStrict $ renderText $ toHtml d
          formTag = T.takeWhile (/= '>') $ snd $ T.breakOn "id=\"widget-order-trigger\"" rendered

      formTag `shouldSatisfy` (not . T.null)
      -- It still saves through the widget-order endpoint...
      formTag `shouldSatisfy` T.isInfixOf "widgets_order"
      -- ...and now says so when that fails, on the app's own toast channel.
      formTag `shouldSatisfy` T.isInfixOf "htmx:responseError"
      formTag `shouldSatisfy` T.isInfixOf "htmx:sendError"
      formTag `shouldSatisfy` T.isInfixOf "errorToast"

  describe "The add-widget experience" do
    -- Opening "add widget" on a dashboard must start on a chart. Logs is a full log
    -- table: it is the wrong thing to drop on a dashboard by default, and it is the
    -- most expensive one to render.
    it "defaults to a timeseries chart, not the logs table" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Add Widget Default"
      (_, html) <- testServant tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
      -- Assert on the seeded `widgetJSON` rather than on which radio carries `checked`:
      -- that object is what the save actually posts, and every tab's inline hyperscript
      -- mentions `checked` anyway, so the markup is not a reliable signal.
      let rendered = toStrict $ renderText html
          seededType = T.takeWhile (/= '"') <$> T.stripPrefix "\"type\":\"" (snd $ T.breakOn "\"type\":\"" rendered)

      seededType `shouldBe` Just "timeseries"

    -- Regression: the editor offered Patterns and Sessions tabs. Selecting one set
    -- widgetJSON.type to a string with no WidgetType constructor, so the widget could
    -- not be decoded on save — the tab looked available and simply did not work.
    it "offers only visualizations the API can actually store as a widget" \tr -> do
      dashId <- newDashboard tr "overview.yaml" "Add Widget Tabs"
      (_, html) <- testServant tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
      let rendered = toStrict $ renderText html
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
          copySqlBlock = snd $ T.breakOn "Copy generated SQL to clipboard" $ toStrict $ renderText $ Widget.widget_ w
      copySqlBlock `shouldSatisfy` T.isInfixOf ("/p/" <> testPid.toText <> "/widget/sql-text?query=")
      copySqlBlock `shouldNotSatisfy` T.isInfixOf "widgetData.sql or widgetData.query"

    it "translates a KQL query into real SQL, not an echo of the input" \tr -> do
      (_, sql) <- testServant tr $ Dashboards.widgetSqlTextGetH testPid (Just "status_code == \"ERROR\"") Nothing Nothing Nothing Nothing
      T.toUpper sql `shouldSatisfy` T.isInfixOf "SELECT"
      sql `shouldNotBe` "status_code == \"ERROR\""

    it "copies SQL scoped to the authenticated environment and service" \tr -> do
      let session = Servant.getResponse tr.trSessAndHeader
      scopedSession <-
        fromRightShow
          <$> runTestEffect
            tr.trPool
            (Config.hasqlPool tr.trATCtx)
            tr.trLogger
            tr.trTracerProvider
            (Auth.sessionByID (Just session.sessionId) session.requestID session.isSidebarClosed session.theme session.lang (Just "production") (Just "checkout") Nothing Auth.ChallengeRedirect)
      let sqlFor dashboardIdM =
            Servant.getResponse
              <$> runAsBase
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
      groupBy `shouldSatisfy` T.isInfixOf "status_code"
      groupBy `shouldSatisfy` T.isInfixOf "level"

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
      frames <- runExceptT $ Source.runSourceT $ Servant.getResponse response
      frames `shouldSatisfy` \case
        Right values -> any (\case AE.Object obj -> KM.lookup "error" obj == Just (AE.String message); _ -> False) values
        Left _ -> False

    it "applies environment and service scope through a raw-SQL template" \tr -> do
      apiKey <- createTestAPIKey tr testPid "raw-template-scope"
      let ingest env service traceIdValue spanId =
            ingestSpanReq tr
              $ mkSpanRequest
                traceIdValue
                spanId
                Nothing
                ("GET /raw/" <> service)
                []
                Nothing
                []
                (mkResource apiKey [mkAttr "deployment.environment.name" env, mkAttr "service.name" service])
                frozenTime
          raw = "SELECT resource___service___name FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND name LIKE 'GET /raw/%' ORDER BY 1"
          scope = [("environment", Just "production"), ("service", Just "checkout")]
      ingest "production" "checkout" "10000000000000000000000000000001" "1000000000000001"
      ingest "production" "catalog" "20000000000000000000000000000002" "2000000000000002"
      ingest "staging" "checkout" "30000000000000000000000000000003" "3000000000000003"
      scoped <- runQueryEffect tr $ Charts.queryMetrics Nothing (Just Charts.DTText) (Just testPid) Nothing (Just raw) (Just "24H") Nothing Nothing Nothing Nothing scope
      scoped.error `shouldBe` Nothing
      scoped.dataText `shouldBe` V.singleton (V.singleton "checkout")

    -- A dashboard supplies its own service variable, while non-dashboard charts still use
    -- the sticky selector. Both keep the authenticated environment boundary.
    it "uses sticky service scope outside dashboards but not inside them" \tr -> do
      apiKey <- createTestAPIKey tr testPid "chart-route-scope"
      let ingest env service traceIdValue spanId =
            ingestSpanReq tr
              $ mkSpanRequest
                traceIdValue
                spanId
                Nothing
                ("GET /chart-scope/" <> service)
                []
                Nothing
                []
                (mkResource apiKey [mkAttr "deployment.environment.name" env, mkAttr "service.name" service])
                frozenTime
          raw = "SELECT resource___service___name FROM otel_logs_and_spans WHERE {{query_ast_filters}} AND name LIKE 'GET /chart-scope/%' ORDER BY 1"
          session = Servant.getResponse tr.trSessAndHeader
      ingest "production" "checkout" "40000000000000000000000000000004" "4000000000000004"
      ingest "staging" "catalog" "50000000000000000000000000000005" "5000000000000005"
      ingest "production" "catalog" "60000000000000000000000000000006" "6000000000000006"
      scopedSession <-
        fromRightShow
          <$> runTestEffect
            tr.trPool
            (Config.hasqlPool tr.trATCtx)
            tr.trLogger
            tr.trTracerProvider
            (Auth.sessionByID (Just session.sessionId) session.requestID session.isSidebarClosed session.theme session.lang (Just "production") (Just "checkout") Nothing Auth.ChallengeRedirect)
      result <-
        runAsBase tr
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
        runAsBase tr
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
      let plotSql q = runQueryEffect tr $ Charts.queryMetrics (Just "postgres") (Just Charts.DTMetric) (Just testPid) Nothing (Just q) (Just "24H") Nothing Nothing Nothing Nothing []
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
      (_, md) <- testServant tr $ addRespHeaders =<< Routes.chartsDataGetH (Just "postgres") (Just Charts.DTText) (Just testPid) Nothing (Just endpointsSql) (Just "24H") Nothing Nothing Nothing Nothing []
      md.error `shouldBe` Nothing

  describe "Streaming chart results" do
    let sql = "SELECT i::bigint, 'value'::text, i::double precision FROM generate_series(1, 3) i"
    it "emits partial data and the same final chart as the JSON endpoint" \tr -> do
      expected <- runQueryEffect tr $ Charts.queryMetrics (Just "postgres") (Just Charts.DTMetric) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing []
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just "postgres") (Just Charts.DTMetric) (Just testPid) Nothing (Just sql) (Just "24H") Nothing Nothing Nothing Nothing []
      frames <- runExceptT $ Source.runSourceT $ Servant.getResponse response
      case frames of
        Right values -> do
          length values `shouldSatisfy` (> 1)
          take 1 values `shouldSatisfy` all (\case AE.Object obj -> KM.lookup "type" obj == Just (AE.String "partial"); _ -> False)
          viaNonEmpty last values `shouldBe` Just (AE.object ["type" AE..= ("complete" :: Text), "data" AE..= expected])
        Left message -> expectationFailure message

    it "discards a connection when a row consumer fails and keeps the pool usable" \tr -> do
      -- Two large rows flush a complete first row through PostgreSQL's socket buffer.
      -- The third row stays asleep until the consumer cancels the query.
      result <- timeout 3000000 $ E.try @E.IOException $ withResource tr.trPool \conn ->
        Charts.streamQuery_ conn "SELECT i::bigint, repeat('x', 10000) FROM generate_series(1, 3) i CROSS JOIN LATERAL pg_sleep(CASE WHEN i <= 2 THEN 0 ELSE 30 END)" \(_ :: (Int64, Text)) -> E.throwIO $ userError "consumer stopped"
      result `shouldSatisfy` maybe False isLeft
      withResource tr.trPool (\conn -> PG.query_ conn "SELECT 42::bigint") `shouldReturn` [PG.Only (42 :: Int64)]

    it "ends decoder failures with an error frame" \tr -> do
      response <- runQueryEffect tr $ Charts.queryMetricsStream (Just "postgres") (Just Charts.DTMetric) (Just testPid) Nothing (Just "SELECT 'wrong type'::text, 'value'::text, 1::double precision") (Just "24H") Nothing Nothing Nothing Nothing []
      frames <- runExceptT $ Source.runSourceT $ Servant.getResponse response
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
    it "returns rows outside the original top twenty and keeps numbers numeric" \tr -> do
      let widget =
            (def :: Widget.Widget)
              { Widget.wType = Widget.WTTable
              , Widget.dbSource = Just "postgres"
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

  -- Endpoint Analytics filters by endpoint hash, which already identifies one endpoint.
  -- Every widget *also* pinned kind=="server", so the ~56% of a project's endpoints that
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
      md <- runQueryEffect tr $ Charts.queryMetrics (Just "postgres") (Just Charts.DTMetric) (Just testPid) (Just $ "hashes[*]==\"" <> outgoingHash <> "\" | summarize count() by bin_auto(timestamp)") Nothing (Just "24H") Nothing Nothing Nothing Nothing []
      md.error `shouldBe` Nothing
      V.length md.dataset `shouldSatisfy` (> 0)
      -- The filter that caused the blank dashboard: it matches nothing for this endpoint.
      excluded <- runQueryEffect tr $ Charts.queryMetrics (Just "postgres") (Just Charts.DTMetric) (Just testPid) (Just $ "kind==\"server\" AND hashes[*]==\"" <> outgoingHash <> "\" | summarize count() by bin_auto(timestamp)") Nothing (Just "24H") Nothing Nothing Nothing Nothing []
      V.length excluded.dataset `shouldBe` 0

    it "ships a template that does not filter the endpoint's widgets by span kind" \_ -> do
      template <- decodeUtf8 <$> readFileBS "static/public/dashboards/endpoint-stats.yaml"
      template `shouldSatisfy` T.isInfixOf "hashes[*]==\"{{var-endpointHash}}\""
      template `shouldNotSatisfy` T.isInfixOf "kind"

    it "renders a dashboard table link with the current project and URL-encoded row value" \_ -> do
      let column = (def :: Widget.TableColumn){Widget.field = "session_id", Widget.title = "Session", Widget.link = Just "/p/{{project_id}}/rum?tab=sessions&session={{row.session_id}}"}
          widget = (def :: Widget.Widget){Widget.wType = Widget.WTTable, Widget.columns = Just [column], Widget._projectId = Just testPid}
          html = toStrict $ renderText $ Widget.renderTableWithDataAndParams widget (V.singleton $ V.singleton "session / id") []
      html `shouldSatisfy` T.isInfixOf ("/p/" <> testPid.toText <> "/rum?tab=sessions&amp;session=session%20%2F%20id")

    it "renders a scoped dependency map with an endpoint-preserving Service Map link" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint dependency map"
      dg <-
        (tabDashboard . snd)
          <$> testServant
            tr
            ( Dashboards.dashboardTabGetH
                testPid
                dashId
                "dependencies"
                Nothing
                Nothing
                Nothing
                (Just "24H")
                Nothing
                [ ("var-host", Just "dellyman.com")
                , ("var-endpointHash", Just "endpoint / hash")
                ]
            )
      let mapWidget = find ((== Widget.WTServiceMap) . (.wType)) (renderedWidgets dg)
      (mapWidget >>= (.cta) <&> (.title)) `shouldBe` Just "Open Service Map"
      (mapWidget >>= (.cta) <&> (.url)) `shouldBe` Just ("/p/" <> testPid.toText <> "/service_map?endpoint_hash=endpoint%20%2F%20hash&since=24H")
      (mapWidget >>= (.html)) `shouldSatisfy` maybe False (T.isInfixOf "No direct dependencies in this range" . toStrict)

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
      openTab tr dashId params = do
        (tabDashboard . snd) <$> testServant tr (Dashboards.dashboardTabGetH testPid dashId "overview" Nothing Nothing Nothing (Just "24H") Nothing params)

    it "skips the widget phase whose results the picker would discard" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Endpoint Analytics"
      -- host is set, endpointHash is not: the picker is what renders.
      gated <- openTab tr dashId [("var-host", Just "dellyman.com")]
      let ws = renderedWidgets gated
      -- Not vacuous: the tab really does carry widgets, and the picker is what renders.
      ws `shouldSatisfy` not . null
      map (fromMaybe "<untitled>" . (.title)) (filter prefilled ws) `shouldBe` []
      toStrict (renderText $ toHtml gated) `shouldSatisfy` T.isInfixOf "var-picker"

  -- Picking a second domain left the Endpoint dropdown listing the first domain's
  -- endpoints. The input carries its own statement so the client can re-fetch options
  -- on change, but the server rendered it already substituted — the old host frozen in —
  -- so every re-fetch asked for the old domain again.
  describe "Dependent variable keeps a live template" do
    it "endpointHash_parentHostChanges_dropdownRefetchesForNewHost" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Dependent Vars"
      dg <- (tabDashboard . snd) <$> testServant tr (Dashboards.dashboardTabGetH testPid dashId "overview" Nothing Nothing Nothing (Just "24H") Nothing [("var-host", Just "dellyman.com")])
      let html = toStrict $ renderText $ toHtml (dg :: Dashboards.DashboardGet)
      html `shouldSatisfy` T.isInfixOf "{{var-host}}"
      html `shouldNotSatisfy` T.isInfixOf "host=&#39;dellyman.com&#39;"

  -- A swap already has a painted page around it, so it ships skeletons that fetch
  -- themselves rather than blocking on the widget phase (measured 4.9s -> 1.2s).
  describe "Prefill on a full load, skeletons on a swap" do
    let openTab tr dashId hx = do
          (tabDashboard . snd) <$> testServant tr (Dashboards.dashboardTabGetH testPid dashId "overview" Nothing Nothing Nothing (Just "24H") hx [("var-host", Just "dellyman.com"), ("var-endpointHash", Just "ba3431d5")])
        eagerOnes = filter (\w -> w.eager == Just True || isJust w.html || isJust w.dataset)

    it "keeps the server-side prefill for a full page load" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Prefill Full"
      full <- openTab tr dashId Nothing
      eagerOnes (renderedWidgets full) `shouldSatisfy` not . null

    it "hands a swap the same widgets with nothing prefilled" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Prefill Swap"
      swapped <- openTab tr dashId (Just "true")
      let ws = renderedWidgets swapped
      ws `shouldSatisfy` not . null
      -- lazyWidget strips `eager` too: renderStatContent reads the flag as "data is here"
      -- and would otherwise spin forever.
      map (fromMaybe "<untitled>" . (.title)) (eagerOnes ws) `shouldBe` []

    it "returns the shared fragment contract for an htmx tab navigation" \tr -> do
      dashId <- newDashboard tr "endpoint-stats.yaml" "Partial navigation"
      (_, response) <-
        testServant tr
          $ Dashboards.dashboardTabGetH
            testPid
            dashId
            "errors"
            Nothing
            Nothing
            Nothing
            (Just "24H")
            (Just "true")
            [ ("var-host", Just "dellyman.com")
            , ("var-endpointHash", Just "ba3431d5")
            ]
      case response of
        NavigationFull _ -> expectationFailure "htmx navigation returned a full document"
        NavigationPartial _ fragment -> do
          let html = toStrict $ renderText fragment
          html `shouldSatisfy` T.isInfixOf "id=\"dashboard-tabs-content\""
          html `shouldSatisfy` T.isInfixOf "id=\"dashboard-tabs-container\""
          html `shouldSatisfy` T.isInfixOf "hx-swap-oob=\"outerMorph\""
          html `shouldSatisfy` T.isInfixOf "tab-active"
          html `shouldSatisfy` T.isInfixOf "hx-push-url=\"true\""
          html `shouldNotSatisfy` T.isInfixOf "<html"
