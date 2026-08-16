-- | The dashboard canvas, end to end: add a widget of every type, move it, resize it,
-- delete it, and confirm the backend still agrees after a reload.
--
-- DashboardsSpec covers dashboard CRUD. Nothing covered the widgets *on* a dashboard —
-- which is the entire product surface — so a widget type could be unsaveable, a drag
-- could silently drop a widget, and no test would notice. These drive the same handlers
-- the browser calls, and enumerate WidgetType via Bounded so a new widget type joins the
-- suite by existing rather than by someone remembering to add it.
module Pages.DashboardWidgetsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Lucid (renderText, toHtml)
import Models.Projects.Dashboards (DashboardVM (..))
import Models.Projects.Dashboards qualified as DashboardModel
import Models.Projects.GitSync qualified as GitSync
import Pages.BodyWrapper (PageCtx (..))
import Pages.Dashboards (DashboardFilters (..))
import Pages.Dashboards qualified as Dashboards
import Pkg.Components.Widget qualified as Widget
import Pkg.TestUtils
import Relude
import System.Types (addRespHeaders)
import Test.Hspec
import Text.Slugify (slugify)


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
newDashboard :: TestResources -> Text -> IO DashboardModel.DashboardId
newDashboard tr title = do
  (_, existing) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
  case existing of
    Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) ->
      for_ dashboards \d -> void $ testServant tr $ Dashboards.dashboardDeleteH testPid d.id
    _ -> pass
  _ <- testServant tr $ Dashboards.dashboardsPostH testPid Dashboards.DashboardForm{Dashboards.title = title, Dashboards.file = "overview.yaml", Dashboards.teams = [], Dashboards.fileDir = Nothing}
  (_, pg) <- testServant tr $ Dashboards.dashboardsGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing noFilters
  case pg of
    Dashboards.DashboardsGet (PageCtx _ Dashboards.DashboardsGetD{dashboards}) ->
      maybe (fail $ "dashboard not found after creating it: " <> toString title) (pure . (.id)) $ V.find (\d -> d.title == title) dashboards
    _ -> fail "expected the dashboard list"


-- | The widgets currently stored on a dashboard (root level), read back through the same
-- effect stack a request runs in — i.e. what the next page load would render.
storedWidgets :: TestResources -> DashboardModel.DashboardId -> IO [Widget.Widget]
storedWidgets tr dashId = snd <$> testServant tr (Dashboards.getDashAndVM dashId Nothing >>= addRespHeaders . (.widgets) . snd)


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
    it "stores a widget of every type, preserving its type and layout" \tr -> do
      dashId <- newDashboard tr "Every Widget Type"
      addWidgets tr dashId [widgetOf wt ("w-" <> show i) | (i, wt) <- zip [0 :: Int ..] allWidgetTypes]
      stored <- storedWidgets tr dashId
      length stored `shouldBe` length allWidgetTypes
      -- Type survives the store/reload, and so does the layout the canvas gave it.
      map (show @Text . (.wType)) stored `shouldBe` map (show @Text) allWidgetTypes
      for_ stored \w -> (w.layout >>= (.w), w.layout >>= (.h)) `shouldBe` (Just 3, Just 3)

    it "gives every added widget an id, so the canvas can address it" \tr -> do
      dashId <- newDashboard tr "Widget Ids"
      addWidgets tr dashId (numberedWidgets 3)
      stored <- storedWidgets tr dashId
      let wids = mapMaybe (.id) stored
      length wids `shouldBe` 3
      length (ordNub wids) `shouldBe` 3

    it "editing a widget by id updates it in place instead of appending a copy" \tr -> do
      dashId <- newDashboard tr "Widget Edit"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTTimeseries "original")
      wid <- firstWidgetId =<< storedWidgets tr dashId

      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just wid) Nothing (widgetOf Widget.WTStat "renamed")
      only <- onlyWidget =<< storedWidgets tr dashId

      only.title `shouldBe` Just "renamed"
      show @Text only.wType `shouldBe` show @Text Widget.WTStat

    -- The editor sends the layout/type on save but not always the query; losing it would
    -- blank the widget the next time the dashboard loads.
    it "an update that omits the query keeps the stored one" \tr -> do
      dashId <- newDashboard tr "Widget Query Retention"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTTimeseries "keeps-query")
      wid <- firstWidgetId =<< storedWidgets tr dashId

      let withoutQuery = (widgetOf Widget.WTTimeseries "keeps-query"){Widget.query = Nothing, Widget.rawQuery = Nothing}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId (Just wid) Nothing withoutQuery
      only <- onlyWidget =<< storedWidgets tr dashId

      only.query `shouldBe` Just "name != null"

  -- The full canvas lifecycle, once per widget type, in one example: add it, drag it,
  -- resize it, then re-read the dashboard the way the next page load does. Every type goes
  -- through the same three handlers, so looping is what makes "every single kind of widget"
  -- cheap to assert — and a type that silently fails to persist shows up as its own line in
  -- the diff rather than as one opaque failure.
  it "every widget type survives add, move, resize and reload" \tr -> do
    dashId <- newDashboard tr "Widget Lifecycle"
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
    it "a drag persists the new position across a reload" \tr -> do
      dashId <- newDashboard tr "Widget Drag"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTTimeseries "draggable")
      wid <- firstWidgetId =<< storedWidgets tr dashId

      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder wid 6 2 3 3)
      only <- onlyWidget =<< storedWidgets tr dashId

      (only.layout >>= (.x), only.layout >>= (.y)) `shouldBe` (Just 6, Just 2)

    it "a resize persists both dimensions" \tr -> do
      dashId <- newDashboard tr "Widget Resize"
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing (widgetOf Widget.WTTimeseries "resizable")
      wid <- firstWidgetId =<< storedWidgets tr dashId

      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder wid 0 0 12 8)
      only <- onlyWidget =<< storedWidgets tr dashId

      (only.layout >>= (.w), only.layout >>= (.h)) `shouldBe` (Just 12, Just 8)

    it "moving a widget to the origin is not mistaken for an absent coordinate" \tr -> do
      dashId <- newDashboard tr "Widget Origin"
      let atSix = (widgetOf Widget.WTTimeseries "origin"){Widget.layout = Just def{Widget.x = Just 6, Widget.y = Just 6, Widget.w = Just 3, Widget.h = Just 3}}
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing Nothing atSix
      wid <- firstWidgetId =<< storedWidgets tr dashId

      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing (reorder wid 0 0 3 3)
      only <- onlyWidget =<< storedWidgets tr dashId

      (only.layout >>= (.x), only.layout >>= (.y)) `shouldBe` (Just 0, Just 0)

    it "reordering keeps every widget that is still on the canvas" \tr -> do
      dashId <- newDashboard tr "Widget Reorder Many"
      addWidgets tr dashId (numberedWidgets 4)
      wids <- mapMaybe (.id) <$> storedWidgets tr dashId

      let patch = Map.fromList [(w, (def :: Dashboards.WidgetReorderItem){Dashboards.x = Just i, Dashboards.y = Just 0, Dashboards.w = Just 3, Dashboards.h = Just 3}) | (i, w) <- zip [0 ..] wids]
      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId Nothing patch
      stored <- storedWidgets tr dashId

      sort (mapMaybe (.id) stored) `shouldBe` sort wids

    it "a widget dropped from the patch is removed from the dashboard" \tr -> do
      dashId <- newDashboard tr "Widget Delete"
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
      dashId <- newDashboard tr "Widget Stale Patch"
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
          (_, dash) <- testServant tr (Dashboards.getDashAndVM dashId Nothing >>= addRespHeaders . snd)
          pure $ maybe [] (.widgets) $ find (\t -> slugify t.name == slug) (fold dash.tabs)
        newTabbedDashboard tr = do
          dashId <- newDashboard tr "Tabbed"
          _ <- testServant tr $ Dashboards.dashboardYamlPutH testPid dashId tabbedYaml
          pure dashId

    -- Two guards, so a failure below says whether the fixture YAML is wrong or the
    -- handler is: first that the YAML parses to two tabs at all, then that storing and
    -- reloading it preserves them.
    it "the fixture YAML parses into two tabs" \_ -> do
      case GitSync.yamlToDashboard (encodeUtf8 tabbedYaml.yaml) of
        Left err -> expectationFailure $ "fixture YAML did not parse: " <> toString err
        Right dash -> map (.name) (fold dash.tabs) `shouldBe` ["First Tab", "Second Tab"]

    it "storing the fixture keeps both tabs and their widgets" \tr -> do
      dashId <- newTabbedDashboard tr
      tabOne <- tabWidgets tr dashId "first-tab"
      tabTwo <- tabWidgets tr dashId "second-tab"
      map (.id) tabOne `shouldBe` [Just "first-widget"]
      map (.id) tabTwo `shouldBe` [Just "second-widget"]

    it "a widget added to a tab lands on that tab only" \tr -> do
      dashId <- newTabbedDashboard tr
      _ <- testServant tr $ Dashboards.dashboardWidgetPutH testPid dashId Nothing (Just "second-tab") (widgetOf Widget.WTStat "Added To Second")

      tabOne <- tabWidgets tr dashId "first-tab"
      tabTwo <- tabWidgets tr dashId "second-tab"
      map (.title) tabOne `shouldBe` [Just "First"]
      map (.title) tabTwo `shouldBe` [Just "Second", Just "Added To Second"]

    -- Duplicating is a canvas action: the copy has to land where the user is looking.
    it "a duplicated widget stays on the tab it was copied from" \tr -> do
      dashId <- newTabbedDashboard tr
      _ <- testServant tr $ Dashboards.dashboardDuplicateWidgetPostH testPid dashId "second-widget" Nothing

      tabOne <- tabWidgets tr dashId "first-tab"
      tabTwo <- tabWidgets tr dashId "second-tab"
      map (.title) tabTwo `shouldBe` [Just "Second", Just "Second (Copy)"]
      map (.title) tabOne `shouldBe` [Just "First"]

    it "a duplicate is a distinct widget the canvas can address separately" \tr -> do
      dashId <- newTabbedDashboard tr
      _ <- testServant tr $ Dashboards.dashboardDuplicateWidgetPostH testPid dashId "first-widget" Nothing

      tabOne <- tabWidgets tr dashId "first-tab"
      let ids = mapMaybe (.id) tabOne
      length ids `shouldBe` 2
      length (ordNub ids) `shouldBe` 2
      map (\w -> w.layout >>= (.w)) tabOne `shouldBe` [Just 3, Just 3]

    it "rearranging one tab leaves the other tab's widgets alone" \tr -> do
      dashId <- newTabbedDashboard tr
      _ <- testServant tr $ Dashboards.dashboardWidgetReorderPatchH testPid dashId (Just "first-tab") (reorder "first-widget" 6 4 6 2)

      tabOne <- tabWidgets tr dashId "first-tab"
      tabTwo <- tabWidgets tr dashId "second-tab"
      map (\w -> (w.layout >>= (.x), w.layout >>= (.w))) tabOne `shouldBe` [(Just 6, Just 6)]
      -- The dangerous shape: reorderWidgets rebuilds from the patch, so a patch scoped to
      -- the wrong tab would empty the tab the user was not even looking at.
      map (.id) tabTwo `shouldBe` [Just "second-widget"]

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
    it "the widget-order form reports a failed save instead of losing it silently" \tr -> do
      dashId <- newDashboard tr "Save Failure Signal"
      (_, pg) <- testServant tr $ Dashboards.dashboardGetH testPid dashId Nothing Nothing Nothing Nothing []
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
      dashId <- newDashboard tr "Add Widget Default"
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
      dashId <- newDashboard tr "Add Widget Tabs"
      (_, html) <- testServant tr $ Dashboards.dashboardWidgetNewGetH testPid dashId Nothing Nothing Nothing
      let rendered = toStrict $ renderText html
          offered = [T.takeWhile (/= '"') seg | seg <- drop 1 (T.splitOn "id=\"viz-" rendered)]
      offered `shouldSatisfy` (not . null)
      for_ offered \vizType ->
        (vizType, isRight (AE.eitherDecode @Widget.WidgetType (AE.encode vizType))) `shouldBe` (vizType, True)
