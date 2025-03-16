module Pages.Dashboards (dashboardGetH, entrypointRedirectGetH, DashboardGet (..), dashboardsGetH, DashboardsGet (..), dashboardsPostH, DashboardForm (..), dashboardWidgetPutH, dashboardWidgetReorderPatchH, WidgetReorderItem (..), dashboardDeleteH, dashboardRenamePatchH, DashboardRenameForm (..), dashboardDuplicatePostH, dashboardMoveWidgetPostH, WidgetMoveForm (..), dashboardDuplicateWidgetPostH, dashboardWidgetExpandGetH) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as Wreq
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity qualified as DBT
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query_)
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Entity.Types qualified as DBT
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxExt_, hxPatch_, hxPost_, hxPut_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation
import Network.HTTP.Types.URI qualified as URI
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Pages.Components qualified as Components
import Pkg.Components qualified as Components
import Pkg.Components.Widget qualified as Widget
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (NoContent (..), ServerError, err404, errBody)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Types
import Text.Slugify (slugify)
import Utils (faSprite_)
import Utils qualified
import Web.FormUrlEncoded (FromForm)


data DashboardGet = DashboardGet Projects.ProjectId Dashboards.DashboardId Dashboards.Dashboard Dashboards.DashboardVM


instance ToHtml DashboardGet where
  toHtml (DashboardGet pid dashId dash dashVM) = toHtml $ dashboardPage_ pid dashId dash dashVM
  toHtmlRaw = toHtml


dashboardPage_ :: Projects.ProjectId -> Dashboards.DashboardId -> Dashboards.Dashboard -> Dashboards.DashboardVM -> Html ()
dashboardPage_ pid dashId dash dashVM = do
  Components.modal_ "pageTitleModalId" ""
    $ form_
      [ class_ "flex flex-col p-3 gap-3"
      , hxPatch_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/rename")
      , hxSwap_ "innerHTML"
      , hxTrigger_ "submit"
      , hxTarget_ "#pageTitleText"
      ]
    $ fieldset_ [class_ "fieldset"] do
      label_ [class_ "label"] "Change Dashboard Title"
      input_ [class_ "input", name_ "title", placeholder_ "Insert new title", value_ $ if dashVM.title == "" then "Untitled" else dashVM.title]
      div_ [class_ "mt-3 flex justify-end gap-2"] do
        label_ [Lucid.for_ "pageTitleModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
        button_ [type_ "submit", class_ "btn btn-primary"] "Save"

  Components.modal_ "pageAddWidgetModalId" ""
    $ form_
      [class_ "flex flex-col p-3 gap-3"]
    $ fieldset_ [class_ "fieldset"] do
      label_ [class_ "label"] "Change Dashboard Title"
      input_ [class_ "input w-full max-w-xs", placeholder_ "Insert new title", value_ $ if dashVM.title == "" then "Untitled" else dashVM.title]

  whenJust dash.variables \variables -> do
    div_ [class_ "flex bg-fillWeaker px-6 py-2 gap-2"] $
      forM_ variables \var -> fieldset_ [class_ "border border-strokeStrong bg-fillWeaker p-0 inline-block rounded-lg overflow-hidden dash-variable text-sm"] do
        legend_ [class_ "px-1 ml-2 text-xs"] $ toHtml $ fromMaybe var.key var.title <> memptyIfFalse (var.required == Just True) " *"
        let whitelist =
              maybe
                "[]"
                ( TE.decodeUtf8
                    . fromLazy
                    . AE.encode
                    . map \opt ->
                      AE.object
                        [ "value" AE..= (opt Unsafe.!! 0)
                        , "name" AE..= fromMaybe (opt Unsafe.!! 0) (opt !!? 1)
                        ]
                )
                var.options

        input_ $
          [ type_ "text"
          , name_ var.key
          , class_ "tagify-select-input"
          , data_ "whitelistjson" whitelist
          , data_ "enforce-whitelist" "true"
          , data_ "mode" $ if var.multi == Just True then "" else "select"
          , data_ "query_sql" $ maybeToMonoid var.sql
          , data_ "query_raw" $ maybeToMonoid var.query
          , data_ "reload_on_change" $ maybe "false" (T.toLower . show) var.reloadOnChange
          , value_ $ maybeToMonoid var.value
          ]
            <> memptyIfFalse (var.multi == Just True) [data_ "mode" "select"]
    script_
      [text|
  const tagifyInstances = new Map();
  document.querySelectorAll('.tagify-select-input').forEach(input => {
    const tgfy = new Tagify(input, {
      whitelist: JSON.parse(input.dataset.whitelistjson || "[]"),
      enforceWhitelist: true,
      tagTextProp: 'name',
      mode: input.dataset.mode || "",
      dropdown: { 
        enabled: 0,
        mapValueTo: "name",
        highlightFirst: true,
        searchKeys: ["name", "value"],
        placeAbove: false,
        maxItems: 50
      },
    })
    
    const inputKey = input.getAttribute('name') || input.id;
    tagifyInstances.set(inputKey, tgfy);

    tgfy.on('change', (e)=>{
      const varName = e.detail.tagify.DOM.originalInput.getAttribute('name');
      const url = new URL(window.location);
      url.searchParams.set('var-'+varName, e.detail?.tagify?.value[0]?.value);
      history.pushState({}, '', url);
      window.dispatchEvent(new Event('update-query'));
    })
    return tgfy
  });

  window.addEventListener('update-query', async () => {
    document.querySelectorAll('.tagify-select-input[data-reload_on_change="true"]').forEach(async input => {
      const { query_sql, query_raw } = input.dataset;
      if (!query_sql && !query_raw) return;
      
      try {
        const tagify = tagifyInstances.get(input.getAttribute('name') || input.id);
        tagify?.loading(true);
        
        const params = new URLSearchParams({ ...Object.fromEntries(new URLSearchParams(location.search)), 
          query_raw, query_sql, data_type: 'text' });
        
        const { data_text } = await fetch(`/chart_data?$${params}`).then(res => res.json());
        
        if (tagify) {
          tagify.settings.whitelist = data_text.map(i => i.length === 1 ? i[0] : { value: i[0], name: i[1] });
          tagify.loading(false);
        }
      } catch (e) {
console.error(`Error fetching data for ${input.name}:`, e);
      }
    });
  });

    |]
  section_ [class_ "pb-12 h-full"] $ div_ [class_ "mx-auto mb-20 pt-5 pb-6 px-6 gap-3.5 w-full flex flex-col h-full overflow-y-scroll pb-2 group/pg", id_ "dashboardPage"] do
    div_
      [ class_ "grid-stack  -m-2 "
      , [__|
             on htmx:afterSettle gridStackInstance.makeWidget('.grid-stack .grid-stack-item:last-child')
                  then set #page-data-drawer.checked to false
      |]
      ]
      do
        forM_ dash.widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
        when (null dash.widgets) $ label_ [id_ "add_a_widget_label", class_ "grid-stack-item pb-8 cursor-pointer bg-fillBrand-weak border-2 border-strokeBrand-strong border-dashed text-strokeSelected rounded-sm rounded-lg flex flex-col gap-3 items-center justify-center *:right-0!  *:bottom-0! ", term "gs-w" "3", term "gs-h" "2", Lucid.for_ "page-data-drawer"] do
          faSprite_ "plus" "regular" "h-8 w-8"
          span_ "Add a widget"
    let projectId = pid.toText
    let dashboardId = dashId.toText
    script_
      [text|
        // Initialize the main grid.
        var gridStackInstance = GridStack.init({
          column: 12,
          acceptWidgets: true,
          cellHeight: '5rem',
          // margin: '0.5rem',
          marginTop: '0.05rem',
          marginLeft: '0.5rem',
          marginRight: '0.5rem',
          marginBottom: '2rem',
          handleClass: 'grid-stack-handle',
          styleInHead: true,
          staticGrid: false,
        },document.querySelector('.grid-stack'));
         // gridStackInstance.compact()
        gridStackInstance.on('removed change', debounce(updateWidgetOrder('${projectId}', '${dashboardId}'), 200));

        // Initialize nested grids and bind change events.
        const nestedGridInstances = [];
        document.querySelectorAll('.nested-grid').forEach(nestedEl => {
          const nestedInstance = GridStack.init({
            column: 12,
            acceptWidgets: true,
            cellHeight: '4.9rem',
            // margin: '0.5rem',
            marginTop: '0.01rem',
            marginLeft: '0.5rem',
            marginRight: '0.5rem',
            marginBottom: '1rem',
            handleClass: 'grid-stack-handle',
            styleInHead: true,
            staticGrid: false,
          }, nestedEl);
          // nestedInstance.compact()
          nestedInstance.on('removed change', debounce(updateWidgetOrder('${projectId}', '${dashboardId}'), 200));
          nestedGridInstances.push(nestedInstance);
        });
        
        // Listen for widget-remove-requested custom events
        document.addEventListener('widget-remove-requested', function(e) {
          const widgetId = e.detail.widgetId;
          const widgetEl = document.getElementById(widgetId + '_widgetEl');
          if (widgetEl) {
            // Find which nested grid contains this widget
            for (const nestedInstance of nestedGridInstances) {
              try {
                // If the widget belongs to this grid, it will be removed
                // If not, nothing happens
                nestedInstance.removeWidget(widgetEl, true);
                break;
              } catch (err) {
                // Continue to the next instance if this one doesn't contain the widget
                continue;
              }
            }
          }
        });
      |]


loadDashboardFromVM :: Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM dashVM = case dashVM.schema of
  Just schema_ -> pure schema_
  Nothing -> find (\d -> d.file == dashVM.baseTemplate) dashboardTemplates


-- Process a single dashboard variable recursively.
processVariable :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Dashboards.Variable -> ATAuthCtx Dashboards.Variable
processVariable pid now (sinceStr, fromDStr, toDStr) allParams variableBase = do
  let (fromD, toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)
  let variable = Dashboards.replaceQueryVariables pid fromD toD allParams variableBase
  let variable' = variable{Dashboards.value = (join $ M.lookup ("var-" <> variable.key) $ M.fromList allParams) <|> variable.value}

  case variable'._vType of
    Dashboards.VTQuery -> case variable.sql of
      Nothing -> pure variable
      Just sqlQuery -> do
        queryResults <- dbtToEff $ query_ Select (Query $ TE.encodeUtf8 sqlQuery)
        pure variable'{Dashboards.options = Just $ V.toList queryResults}
    _ -> pure variable'


-- Process a single widget recursively.
processWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processWidget pid now (sinceStr, fromDStr, toDStr) allParams widgetBase = do
  let (_fromD, _toD, _currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)
  let widgetBase' = if widgetBase._projectId == Nothing then widgetBase{Widget._projectId = Just pid} else widgetBase

  let widget = widgetBase'
  widget' <-
    if (widget.eager == Just True || widget.wType `elem` [Widget.WTAnomalies])
      then do
        case widget.wType of
          Widget.WTAnomalies -> do
            issues <- dbtToEff $ Anomalies.selectIssues pid Nothing (Just False) (Just False) Nothing (Just 2) (0)
            let issuesVM = V.map (AnomalyList.IssueVM False now "24h") issues
            pure $
              widget
                & #html
                  ?~ ( renderText $
                        div_ [class_ "flex flex-col gap-4 h-full w-full overflow-hidden"] $
                          forM_ issuesVM (\x -> div_ [class_ "border border-strokeWeak rounded-2xl overflow-hidden"] $ toHtml x)
                     )
          Widget.WTStat -> do
            stat <- Charts.queryMetrics (Just Charts.DTFloat) (Just pid) widget.query Nothing widget.sql sinceStr fromDStr toDStr Nothing allParams
            pure $
              widget
                & #dataset
                  ?~ def
                    { Widget.source = AE.Null
                    , Widget.value = stat.dataFloat
                    }
          _ -> do
            metricsD <-
              Charts.queryMetrics (Just Charts.DTMetric) (Just pid) widget.query Nothing widget.sql sinceStr fromDStr toDStr Nothing allParams
            pure $
              widget
                & #dataset
                  ?~ Widget.WidgetDataset
                    { source =
                        AE.toJSON $
                          V.cons
                            (AE.toJSON <$> metricsD.headers)
                            (AE.toJSON <<$>> metricsD.dataset)
                    , rowsPerMin = metricsD.rowsPerMin
                    , value = Just metricsD.rowsCount
                    , from = metricsD.from
                    , to = metricsD.to
                    , stats = metricsD.stats
                    }
      else pure widget
  -- Recursively process child widgets, if any.
  case widget'.children of
    Nothing -> pure widget'
    Just childWidgets -> do
      -- Process child widgets, preserving any dashboard ID from the parent
      let processWithParentContext childWidget =
            processWidget
              pid
              now
              (sinceStr, fromDStr, toDStr)
              allParams
              ( if isJust widget'._dashboardId && isNothing childWidget._dashboardId
                  then childWidget{Widget._dashboardId = widget'._dashboardId}
                  else childWidget
              )

      newChildren <- traverse processWithParentContext childWidgets
      pure $ widget' & #children .~ Just newChildren


-- dashboardWdgetPutH adds a widget at the end of
dashboardWidgetPutH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Widget.Widget -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardWidgetPutH pid dashId widgetIdM widget = do
  (_, dash) <- getDashAndVM dashId Nothing
  uid <- UUID.genUUID >>= pure . UUID.toText
  let (dash', widget') = case widgetIdM of
        Just wID -> (dash, widget)
        Nothing -> do
          let widgetUpdated = widget{Widget.standalone = Nothing, Widget.naked = Nothing, Widget.id = Just uid}
          (dash{Dashboards.widgets = dash.widgets <> [widgetUpdated]}, widgetUpdated)
  _ <- dbtToEff $ DBT.updateFieldsBy @Dashboards.DashboardVM [[DBT.field| schema |]] ([DBT.field| id |], dashId) (Only dash')
  addRespHeaders $ widget'


data WidgetReorderItem = WidgetReorderItem
  { w :: Maybe Int
  , h :: Maybe Int
  , x :: Maybe Int
  , y :: Maybe Int
  , children :: Maybe (Map Text WidgetReorderItem)
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake WidgetReorderItem


dashboardWidgetReorderPatchH
  :: Projects.ProjectId
  -> Dashboards.DashboardId
  -> Map Text WidgetReorderItem
  -- ^ The ordered list of widget IDs
  -> ATAuthCtx (RespHeaders NoContent)
dashboardWidgetReorderPatchH pid dashId widgetOrder = do
  (_, dash) <- getDashAndVM dashId Nothing

  let sortedWidgets = reorderWidgets widgetOrder dash.widgets
      newDash = dash{Dashboards.widgets = sortedWidgets}

  _ <-
    dbtToEff $
      DBT.updateFieldsBy @Dashboards.DashboardVM
        [[DBT.field| schema |]]
        ([DBT.field| id |], dashId)
        (Only newDash)

  addRespHeaders NoContent


-- | Rebuild the widget tree based solely on the reorder patch.
-- Widgets not mentioned in the patch are dropped.
reorderWidgets :: Map Text WidgetReorderItem -> [Widget.Widget] -> [Widget.Widget]
reorderWidgets patch ws = go patch
  where
    go :: Map Text WidgetReorderItem -> [Widget.Widget]
    go =
      mapMaybe
        ( \(wid, item) ->
            Map.lookup wid (flattenWidgets ws) >>= \orig ->
              let newLayout = mergeLayout (Widget.layout orig) item
                  newChildren = item.children <&> go
               in Just (orig{Widget.layout = newLayout, Widget.children = newChildren})
        )
        . Map.toList

    flattenWidgets :: [Widget.Widget] -> Map Text Widget.Widget
    flattenWidgets =
      foldr
        (\w acc -> Map.insert (fromMaybe (maybeToMonoid $ slugify <$> w.title) w.id) w (Map.union (flattenWidgets (fromMaybe [] w.children)) acc))
        Map.empty

    mergeLayout :: Maybe Widget.Layout -> WidgetReorderItem -> Maybe Widget.Layout
    mergeLayout mLayout wri = Just (Widget.Layout (wri.x <|> ox) (wri.y <|> oy) (wri.w <|> ow) (wri.h <|> oh))
      where
        Widget.Layout ox oy ow oh = fromMaybe (Widget.Layout Nothing Nothing Nothing Nothing) mLayout


getDashAndVM :: (DB :> es, Wreq.HTTP :> es, Error ServerError :> es) => Dashboards.DashboardId -> Maybe Text -> Eff es (Dashboards.DashboardVM, Dashboards.Dashboard)
getDashAndVM dashId fileM = do
  dashVM <-
    (dbtToEff $ DBT.selectById @Dashboards.DashboardVM (Only dashId)) >>= \case
      Just v -> pure v
      Nothing -> throwError $ err404{errBody = ("Dashboard with ID not found. ID:" <> encodeUtf8 dashId.toText)}

  dash <- case fileM of
    Just file -> Dashboards.readDashboardEndpoint file
    Nothing -> pure $ fromMaybe def (loadDashboardFromVM dashVM)

  pure (dashVM, dash)


dashboardGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId fileM fromDStr toDStr sinceStr allParams = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)
  (dashVM, dash) <- getDashAndVM dashId fileM
  dash' <- forOf (#variables . traverse . traverse) dash (processVariable pid now (sinceStr, fromDStr, toDStr) allParams)

  -- Process widgets and add the dashboard ID to each one
  let processWidgetWithDashboardId w = do
        processed <- processWidget pid now (sinceStr, fromDStr, toDStr) allParams w
        pure $ processed{Widget._dashboardId = Just dashId.toText}

  dash'' <- forOf (#widgets . traverse) dash' processWidgetWithDashboardId

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "Dashboards"
          , pageTitle = if dashVM.title == "" then "Untitled" else dashVM.title
          , pageTitleModalId = Just "pageTitleModalId"
          , pageActions = Just $ div_ [class_ "inline-flex gap-3 items-center leading-[0]"] do
              Components.timepicker_ Nothing currentRange
              div_ [class_ "join"] do
                label_
                  [ class_ "cursor-pointer px-3 flex items-center border border-strokeStrong shadow-sm leading-none join-item"
                  , data_ "tippy-content" "Refresh"
                  , [__| on click trigger 'update-query' on window then
                      add .animate-spin to the first <svg/> in me then wait 1 seconds then
                      remove .animate-spin from the first <svg/> in me |]
                  ]
                  $ faSprite_ "arrows-rotate" "regular" "w-3 h-3"

                div_ [class_ "dropdown dropdown-end leading-none join-item border-y border-r border-strokeStrong shadow-sm group/rf"] do
                  div_ [class_ "cursor-pointer py-2 px-3 flex gap-2 items-center leading-none", tabindex_ "0", data_ "tippy-content" "Refresh frequency"] do
                    span_ [class_ "auto-refresh-span"] "Off"
                    faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral "
                  ul_ [class_ "dropdown-content menu p-2 shadow-lg bg-base-100 rounded-box z-[1] mt-2", tabindex_ "0", id_ "auto-refresh-menu"] do
                    let refreshOptions :: [(Text, Text, Text)]
                        refreshOptions =
                          [ ("off", "Off", "0")
                          , ("15s", "15 seconds", "15000")
                          , ("30s", "30 seconds", "30000")
                          , ("1m", "1 minute", "60000")
                          , ("5m", "5 minutes", "300000")
                          , ("15m", "15 minutes", "900000")
                          , ("30m", "30 minutes", "1800000")
                          , ("1h", "1 hour", "3600000")
                          , ("2h", "2 hours", "7200000")
                          , ("1d", "1 day", "86400000")
                          ]
                    forM_ refreshOptions \(label, title, ms) ->
                      li_
                        $ a_
                          [ data_ "value" ms
                          , data_ "tippy-content" title
                          , [__| on click 
                              set .auto-refresh-span.innerText to my.textContent then
                              send setRefreshInterval(interval: parseInt(@data-value)) to window then
                              focus(document.body)
                          |]
                          ]
                        $ toHtml label

              div_ [class_ "flex items-center"] do
                span_ [class_ "text-fillDisabled mr-2"] "|"
                Components.drawer_ "page-data-drawer" Nothing (Just $ newWidget_ pid currentRange) $ span_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg", data_ "tippy-content" "Add a new widget"] $ faSprite_ "plus" "regular" "w-3 h-3"
                div_ [class_ "dropdown dropdown-end"] do
                  div_ [tabindex_ "0", role_ "button", class_ "text-iconNeutral cursor-pointer  p-2 hover:bg-fillWeak rounded-lg", data_ "tippy-content" "Context Menu"] $ faSprite_ "ellipsis" "regular" "w-4 h-4"
                  ul_ [tabindex_ "0", class_ "dropdown-content menu menu-md bg-base-100 rounded-box p-2 w-52 shadow-sm leading-none"] do
                    li_ $ label_ [Lucid.for_ "pageTitleModalId", class_ "p-2"] "Rename dashboard"
                    li_ $
                      button_
                        [ class_ "p-2 w-full text-left"
                        , hxPost_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/duplicate")
                        , hxSwap_ "none"
                        , data_ "tippy-content" "Creates a copy of this dashboard"
                        ]
                        "Duplicate dashboard"
                    li_ $
                      button_
                        [ class_ "p-2 w-full text-left text-textError"
                        , hxDelete_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText)
                        , hxSwap_ "none"
                        , hxConfirm_ "Are you sure you want to delete this dashboard? This action cannot be undone."
                        , data_ "tippy-content" "Permanently deletes this dashboard"
                        ]
                        "Delete dashboard"
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/dashboard/"
          }
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dashId dash'' dashVM


-- | A unified widget viewer/editor component that uses DaisyUI tabs without JavaScript
-- @param pid Project ID
-- @param dashboardIdM Optional dashboard ID
-- @param currentRange Time range for the widget
-- @param existingWidgetM Optional existing widget (for edit mode)
-- @param activeTab Which tab should be active initially ("edit" or "overview")
widgetViewerEditor_ :: Projects.ProjectId -> Maybe Dashboards.DashboardId -> Maybe Text -> Maybe Widget.Widget -> Text -> Html ()
widgetViewerEditor_ pid dashboardIdM currentRange existingWidgetM activeTab = div_ [class_ "group/wgtexp"] do
  -- Determine if this is a new widget or editing existing one
  let isNewWidget = isNothing existingWidgetM
  -- Set the actual active tab based on whether this is a new widget or editing an existing one
  let effectiveActiveTab = if isNewWidget then "edit" else activeTab

  -- Define widget data (either existing or default)
  let defaultWidget =
        (def :: Widget.Widget)
          { Widget.wType = Widget.WTTimeseries
          , Widget.id = Just "newWidget"
          , Widget.standalone = Just True
          , Widget.naked = Just True
          , Widget.title = Just "New Widget"
          , Widget.hideSubtitle = Just True
          , Widget.query = Just "timechart count(*)"
          , Widget.unit = Just "ms"
          , Widget.hideLegend = Just True
          , Widget._projectId = Just pid
          , Widget._dashboardId = dashboardIdM <&> (.toText)
          , Widget.layout = Just $ def{Widget.w = Just 3, Widget.h = Just 3}
          }
      widgetToUse' = fromMaybe defaultWidget existingWidgetM
      widgetToUse = (fromMaybe defaultWidget existingWidgetM){Widget.id = Just $ maybeToMonoid widgetToUse'.id <> "Expanded", Widget.standalone = Just True, Widget.naked = Just True}

  let widgetJSON = TE.decodeUtf8 $ fromLazy $ AE.encode widgetToUse
  let formAction = case (dashboardIdM, existingWidgetM) of
        (Just dashId, Just w) -> "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/widgets/" <> fromMaybe "" w.id
        (Just dashId, Nothing) -> "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/widgets"
        _ -> ""
  let wid = maybeToMonoid widgetToUse.id
  -- Form for saving widgets
  form_
    [ class_ "hidden"
    , id_ "widget-form"
    , hxPut_ formAction
    , hxVals_ "js:{...widgetJSON}"
    , hxExt_ "json-enc"
    , hxSwap_ "beforeend"
    , hxTarget_ ".grid-stack"
    , hxTrigger_ "submit"
    , [__| on htmx:beforeRequest 
            if not widgetJSON.id
              gridStackInstance.removeWidget('#add_a_widget_label', true, false)
            end
        |]
    ]
    ""

  -- Widget title and close button
  div_ [class_ "flex justify-between items-center mb-4"] do
    div_ [class_ "flex justify-between"] do
      -- Only show tabs when viewing an existing widget, not for new widgets
      when (not isNewWidget) $
        div_ [class_ "tabs tabs-box tabs-md p-0 tabs-outline items-center border"] do
          label_ [role_ "tab", class_ $ "tab h-auto! has-[:checked]:tab-active"] do
            input_ ([type_ "radio", value_ "Overview", class_ "hidden page-drawer-tab-overview", name_ $ wid <> "-drawer-tab"] <> if effectiveActiveTab /= "edit" then [checked_] else mempty)
            "Overview"

          label_ [role_ "tab", class_ $ "tab h-auto! has-[:checked]:tab-active "] do
            input_ ([type_ "radio", value_ "Edit", class_ "hidden page-drawer-tab page-drawer-tab-edit", name_ $ wid <> "-drawer-tab"] <> if effectiveActiveTab == "edit" then [checked_] else mempty)
            "Edit"

      -- For new widgets, just show a title
      when isNewWidget $
        h3_ [class_ "text-lg font-normal"] "Add a new widget"

    -- Action buttons
    div_ [class_ "flex items-center gap-2"] do
      Components.timepicker_ Nothing currentRange

      label_
        [ class_ "cursor-pointer py-2 px-3 border border-strokeStrong rounded-lg shadow-sm leading-none"
        , data_ "tippy-content" "Refresh"
        , [__| on click trigger 'update-query' on window then
                add .animate-spin to the first <svg/> in me then 
                wait 1s then remove .animate-spin from the first <svg/> in me 
            |]
        ]
        $ faSprite_ "arrows-rotate" "regular" "w-3 h-3"

      -- Close drawer button
      let drawerToClose = if isJust existingWidgetM then "global-data-drawer" else "page-data-drawer"
      span_ [class_ "text-fillDisabled"] "|"
      button_ [class_ "leading-none rounded-lg px-4 py-2 cursor-pointer btn btn-primary shadow-sm leading-none hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block", type_ "submit", form_ "newWidgetForm"] $ "Save changes"
      label_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg leading-none", data_ "tippy-content" "Close Drawer", Lucid.for_ drawerToClose] $ faSprite_ "xmark" "regular" "w-3 h-3"

  -- Only show overview when viewing existing widgets and the overview tab is selected
  when (not isNewWidget) $ div_ [class_ " hidden group-has-[.page-drawer-tab-overview:checked]/wgtexp:block"] do
    div_ [class_ "w-full aspect-4/1 p-3 rounded-lg bg-fillWeaker mb-4"] do
      toHtml $
        widgetToUse{Widget.standalone = Just True, Widget.naked = Just True, Widget.id = Just $ maybeToMonoid widgetToUse.id <> "Expanded"}
    h3_ [class_ "text-lg font-normal text-center"] $ toHtml $ maybeToMonoid widgetToUse.title

  -- Always show edit for new widgets, otherwise only when edit tab is selected
  div_ [class_ $ if isNewWidget then "block" else "hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block"] do
    div_ [class_ "w-full aspect-4/1 p-3 rounded-lg bg-fillWeaker mb-4"] do
      script_
        [class_ "hidden"]
        [text| var widgetJSON = ${widgetJSON}; |]
      div_
        [ id_ "widget-preview"
        , class_ "h-full w-full"
        , hxPost_ "/widget"
        , hxTrigger_ "intersect, update-widget"
        , hxTarget_ "this"
        , hxSwap_ "innerHTML"
        , hxVals_ "js:{...widgetJSON}"
        , hxExt_ "json-enc"
        ]
        ""

    -- Widget configuration UI
    div_ [class_ "space-y-7"] do
      div_ [class_ "flex gap-3"] do
        span_ [class_ "inline-block rounded-full bg-fillWeak px-3 py-1 leading-none"] "1"
        strong_ [class_ "text-lg font-semibold"] "Select your Visualization"
      div_ [class_ "grid grid-cols-12 gap-3 px-5"] $
        let visTypes :: [(Text, Text, Text)]
            visTypes =
              [ ("bar-chart", "Bar", "timeseries")
              , ("duo-line-chart", "Line", "timeseries_line")
              , ("duo-pie-chart", "Pie", "pie_chart")
              , ("duo-scatter-chart", "Scatter", "distribution")
              , ("hashtag", "Number", "stat")
              , ("guage", "Guage", "")
              , ("text", "Text", "")
              ]
         in iforM_ visTypes \idx (icon, title, widgetType) ->
              label_
                [ class_ "col-span-1 p-4 aspect-square gap-3 flex flex-col border border-strokeWeak rounded-lg items-center justify-center has-checked:border-strokeBrand-strong has-checked:bg-fillBrand-weak"
                , data_ "widgetType" widgetType
                , [__| on click set widgetJSON.type to @data-widgetType then trigger 'update-widget' on #widget-preview |]
                ]
                do
                  input_
                    ( [ class_ "hidden"
                      , name_ "widgetType"
                      , type_ "radio"
                      ]
                        <> if idx == 0 then [checked_] else mempty
                    )
                  span_ [class_ "block"] $ faSprite_ icon "regular" "w-4 h-4"
                  span_ [class_ "text-textWeak block leading-none"] $ toHtml title

      div_ [class_ "space-y-7"] do
        div_ [class_ "flex gap-3"] do
          span_ [class_ "inline-block rounded-full bg-fillWeak px-3 py-1 leading-none"] "2"
          strong_ [class_ "text-lg font-semibold"] "Graph your Data"
        div_ [class_ "px-5"] $
          textarea_
            [ class_ "w-full h-20 textarea"
            , value_ $ fromMaybe "" widgetToUse.query
            , [__| on change set widgetJSON.query to my value then trigger 'update-widget' on #widget-preview |]
            ]
            ""

      div_ [class_ "space-y-7"] do
        div_ [class_ "flex gap-3"] do
          span_ [class_ "inline-block rounded-full bg-fillWeak px-3 py-1 leading-none"] "3"
          strong_ [class_ "text-lg font-semibold"] "Give your graph a title"
        div_ [class_ "space-x-8 px-5"] $
          input_
            [ class_ "p-3 border border-strokeWeak w-full rounded-lg bg-transparent"
            , placeholder_ "Throughput"
            , required_ "required"
            , value_ $ fromMaybe "" widgetToUse.title
            , [__| on change set widgetJSON.title to my value then trigger 'update-widget' on #widget-preview |]
            ]


-- | Backward compatibility wrapper for the new widget editor
newWidget_ :: Projects.ProjectId -> Maybe Text -> Html ()
newWidget_ pid currentRange = widgetViewerEditor_ pid Nothing currentRange Nothing "edit"


--------------------------------------------------------------------
-- Dashboard List
--

data DashboardsGet = DashboardsGet
  { dashboards :: V.Vector Dashboards.DashboardVM
  , projectId :: Projects.ProjectId
  }
  deriving (Show, Generic)


instance ToHtml DashboardsGet where
  toHtml dash = toHtml $ dashboardsGet_ dash
  toHtmlRaw = toHtml


renderDashboardListItem :: Bool -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Html ()
renderDashboardListItem checked tmplClass title value description icon prview = label_
  [ class_
      [text| cursor-pointer group/it border border-transparent hover:bg-fillWeaker hover:border-strokeWeak rounded-lg flex p-1.5 gap-2 items-center
      group-has-[input:checked]/it:bg-fillWeaker group-has-[input:checked]/it:border-strokeWeak dashboardListItem|]
  , term "data-title" title
  , term "data-description" $ maybeToMonoid description
  , term "data-icon" $ fromMaybe "square-dashed" icon
  , term "data-preview" $ fromMaybe "/public/assets/svgs/screens/dashboard_blank.svg" prview
  , [__| on mouseover set #dItemPreview.src to my @data-preview
              then set #dItemTitle.innerText to my @data-title
              then set #dItemDescription.innerText to my @data-description
              then set #dItemIcon.innerHTML to `<svg class='w-8 h-8'><use href='/public/assets/svgs/fa-sprites/regular.svg#${my @data-icon}'></use></svg>`
          on mouseout
              put (<.dashboardListItem:has(input:checked)/>) into checkedLabel
              set #dItemPreview.src to checkedLabel's @data-preview
              then set #dItemTitle.innerText to checkedLabel's @data-title
              then set #dItemDescription.innerText to checkedLabel's @data-description
              then set #dItemIcon.innerHTML to `<svg class='w-8 h-8'><use href='/public/assets/svgs/fa-sprites/regular.svg#${checkedLabel's @data-icon}'></use></svg>`
              |]
  ]
  do
    input_ ([class_ $ "hidden " <> tmplClass, type_ "radio", name_ "file", value_ value] <> [checked_ | checked])
    span_ [class_ "p-1 px-2 bg-fillWeak rounded-md"] $ faSprite_ (fromMaybe "square-dashed" icon) "regular" "w-4 h-4"
    span_ [class_ "grow"] $ toHtml title
    span_ [class_ "px-2 p-1 invisible group-has-[input:checked]/it:visible"] $ faSprite_ "chevron-right" "regular" "w-4 h-4"


dashboardsGet_ :: DashboardsGet -> Html ()
dashboardsGet_ dg = do
  Components.modal_ "newDashboardMdl" "" $ form_
    [ class_ "grid grid-cols-7 overflow-hidden h-full gap-4 group/md"
    , hxPost_ ""
    ]
    do
      div_ [class_ "col-span-2 space-y-4"] do
        strong_ "Create dashboard"
        label_ [class_ "input input-sm flex items-center "] do
          faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
          input_
            [ type_ "text"
            , class_ "grow pl-2"
            , placeholder_ "Search"
            , [__|
              on keyup
                if the event's key is 'Escape' set my value to '' then trigger keyup
                else show <.dashboardListItem/> in #dashListItemParent when its textContent.toLowerCase() contains my value.toLowerCase() |]
            ]
          kbd_ [class_ "kbd kbd-sm"] "/"
        div_ [class_ "space-y-1", id_ "dashListItemParent"] do
          renderDashboardListItem True "tmplRadio0" "Blank dashboard" "" (Just "Get started from a blank slate") (Just "cards-blank") Nothing
          iforM_ dashboardTemplates \idx dashTmpl -> do
            let tmplItemClass = "tmplRadio" <> (show $ idx + 1)
            renderDashboardListItem False tmplItemClass (maybeToMonoid dashTmpl.title) (maybeToMonoid dashTmpl.file) dashTmpl.description dashTmpl.icon dashTmpl.preview

      div_ [class_ "col-span-5 px-3 py-5 divide-y h-full overflow-y-scroll "] do
        div_ [class_ "flex gap-3 pb-5"] do
          div_ $ div_ [class_ "p-2 bg-fillWeaker rounded-lg ", id_ "dItemIcon"] $ faSprite_ "cards-blank" "regular" "w-8 h-8"
          div_ [class_ "flex-1"] do
            strong_ [class_ "text-xl", id_ "dItemTitle"] "Custom Dashboard"
            p_ [class_ "text-sm line-clamp-2 min-h-10", id_ "dItemDescription"] "Get started from a blank slate"
          div_ [class_ "flex items-center justify-center shrink"] $ button_ [class_ "leading-none rounded-lg p-3 cursor-pointer bg-fillBrand-strong shadow-sm text-white", type_ "submit"] "Select template"
        div_ [class_ "pt-5"] $
          div_ [class_ "bg-[#1e9cff] px-5 py-8 rounded-xl aspect-square w-full flex items-center"] $
            img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full", id_ "dItemPreview"]

  div_ [id_ "itemsListPage", class_ "mx-auto px-6 pt-4 gap-8 w-full flex flex-col h-full overflow-hidden pb-2  group/pg"] do
    div_ [class_ "flex"] do
      label_ [class_ "input input-md flex-1 flex bg-fillWeaker border-slate-200 shadow-none overflow-hidden items-center gap-2"] do
        faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , [__|on keyup if the event's key is 'Escape' set my value to '' then trigger keyup
                         else show <.itemListItem/> in #itemsListPage when its textContent.toLowerCase() contains my value.toLowerCase() |]
          ]
    -- button_
    div_ [class_ "grid grid-cols-2 gap-5"] do
      forM_ dg.dashboards \dashVM -> do
        let dash = loadDashboardFromVM dashVM
        div_ [class_ "rounded-xl border border-strokeWeak gap-3.5 p-4 bg-fillWeaker flex itemListItem"] do
          div_ [class_ "flex-1 space-y-2"] do
            div_ [class_ "flex items-center gap-2"] do
              a_ [class_ "hover:underline underline-offset-2", href_ ("/p/" <> dg.projectId.toText <> "/dashboards/" <> dashVM.id.toText)] $
                strong_ [class_ "font-medium"] (toHtml $ bool "Untitled" dashVM.title (dashVM.title /= ""))
              span_ [class_ "leading-none", term "data-tippy-content" "This dashboard is currently your homepage."] do
                when (isJust dashVM.homepageSince) $ (faSprite_ "house" "regular" "w-4 h-4")
            div_ [class_ "gap-2 flex items-center"] do
              time_ [class_ "mr-2 text-slate-400", term "data-tippy-content" "Date of dashboard creation", datetime_ $ Utils.formatUTC dashVM.createdAt] $ toHtml $ formatTime defaultTimeLocale "%eth %b %Y" dashVM.createdAt
              forM_ dashVM.tags (span_ [class_ "badge badge-neutral"] . toHtml @Text)
          div_ [class_ "flex items-end justify-center gap-5"] do
            button_ [class_ "leading-none", term "data-tippy-content" "click star this dashboard"] $
              if isJust dashVM.starredSince
                then (faSprite_ "star" "solid" "w-5 h-5")
                else (faSprite_ "star" "regular" "w-5 h-5")
            let widgetCount = maybe "0" (show . length . (.widgets)) dash
            div_ [class_ "flex items-end gap-2", term "data-tippy-content" $ "There are " <> widgetCount <> " charts/widgets in this dashboard"] $ faSprite_ "chart-area" "regular" "w-5 h-5 text-iconNeutral" >> (span_ [class_ "leading-none"] . toHtml $ widgetCount)


dashboardsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx DashboardsGet))
dashboardsGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  dashboards <- dbtToEff $ DBT.selectManyByField @Dashboards.DashboardVM [DBT.field| project_id |] pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Dashboards"
          , pageActions = Just $ (label_ [Lucid.for_ "newDashboardMdl", class_ "leading-none rounded-xl shadow-sm p-3 cursor-pointer bg-fillBrand-strong text-white"] "New Dashboard")
          }
  addRespHeaders $
    PageCtx bwconf $
      DashboardsGet{dashboards, projectId = pid}


data DashboardForm = DashboardForm
  { file :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


dashboardsPostH :: Projects.ProjectId -> DashboardForm -> ATAuthCtx (RespHeaders NoContent)
dashboardsPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  did <- Dashboards.DashboardId <$> UUID.genUUID
  let dashM = find (\dashboard -> dashboard.file == Just form.file) dashboardTemplates
  let redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> (did.toText)
  dbtToEff $
    DBT.insert @Dashboards.DashboardVM $
      Dashboards.DashboardVM
        { id = did
        , projectId = pid
        , createdAt = now
        , updatedAt = now
        , createdBy = sess.user.id
        , baseTemplate = if form.file == "" then Nothing else Just form.file
        , schema = Nothing
        , starredSince = Nothing
        , homepageSince = Nothing
        , tags = V.fromList $ fromMaybe [] $ dashM >>= (.tags)
        , title = fromMaybe [] $ dashM >>= (.title)
        }
  redirectCS redirectURI
  addRespHeaders NoContent


-- -- Template Haskell splice to generate the list of dashboards by reading the dashboards folder in filesystem
dashboardTemplates :: [Dashboards.Dashboard]
dashboardTemplates = $(Dashboards.readDashboardsFromDirectory "static/public/dashboards")


-- THe current /p/:projectId/  handler. Redirects users to the overview dashboard if it exists, or creates it.
entrypointRedirectGetH
  :: Text
  -> Text
  -> [Text]
  -> Projects.ProjectId
  -> [(Text, Maybe Text)]
  -> ATAuthCtx (Headers '[Header "Location" Text] NoContent)
entrypointRedirectGetH baseTemplate title tags pid qparams = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let mkPath p d = "/p/" <> pid.toText <> p <> d <> "?" <> toQueryParams qparams
      q = [sql|select id::text from projects.dashboards where project_id=? and (homepage_since is not null or base_template=?)|]
      newDashboard = do
        did <- Dashboards.DashboardId <$> UUID.genUUID
        dbtToEff $
          DBT.insert @Dashboards.DashboardVM
            Dashboards.DashboardVM
              { id = did
              , projectId = pid
              , createdAt = now
              , updatedAt = now
              , createdBy = sess.user.id
              , baseTemplate = Just baseTemplate
              , schema = Nothing
              , starredSince = Nothing
              , homepageSince = Nothing
              , tags = V.fromList tags
              , title = title
              }
        pure did.toText
  redirectTo <-
    if project.paymentPlan == "ONBOARDING"
      then pure $ mkPath "/onboarding" ""
      else (mkPath "/dashboards/") <$> (maybe newDashboard pure =<< dbtToEff (DBT.queryOne DBT.Select q (pid, baseTemplate)))
  pure $ addHeader redirectTo NoContent


-- | Convert a list of query parameters into a percent-encoded query string.
-- For example, [("key", Just "value"), ("empty", Nothing)] becomes "key=value&empty".
toQueryParams :: [(Text, Maybe Text)] -> Text
toQueryParams qs =
  TE.decodeUtf8 $
    URI.renderQuery False $
      map (\(k, mv) -> (TE.encodeUtf8 k, fmap TE.encodeUtf8 mv)) qs


-- | Form data for renaming a dashboard
data DashboardRenameForm = DashboardRenameForm
  { title :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


-- | Handler for renaming a dashboard.
-- It updates the title of the specified dashboard.
dashboardRenamePatchH :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardRenameForm -> ATAuthCtx (RespHeaders (Html ()))
dashboardRenamePatchH pid dashId form = do
  mDashboard <- dbtToEff $ DBT.selectOneByField @Dashboards.DashboardVM [DBT.field| id |] (Only dashId)
  case mDashboard of
    Nothing -> throwError $ err404{errBody = "Dashboard not found or does not belong to this project"}
    Just dashVM -> do
      _ <- dbtToEff $ DBT.updateFieldsBy @Dashboards.DashboardVM [[DBT.field| title |]] ([DBT.field| id |], dashId) (Only form.title)

      when (isJust dashVM.schema) do
        let updatedSchema = dashVM.schema & _Just . #title .~ Just form.title
        _ <-
          dbtToEff $
            DBT.updateFieldsBy @Dashboards.DashboardVM
              [[DBT.field| schema |]]
              ([DBT.field| id |], dashId)
              (Only updatedSchema)
        pass

      addSuccessToast "Dashboard renamed successfully" Nothing
      addTriggerEvent "closeModal" ""
      addRespHeaders (toHtml form.title)


-- | Handler for duplicating a dashboard.
-- It creates a new dashboard with the same content but with "(Copy)" appended to the title.
dashboardDuplicatePostH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders NoContent)
dashboardDuplicatePostH pid dashId = do
  mDashboard <- dbtToEff $ DBT.selectOneByField @Dashboards.DashboardVM [DBT.field| id |] (Only dashId)

  case mDashboard of
    Nothing -> throwError $ err404{errBody = "Dashboard not found or does not belong to this project"}
    Just dashVM -> do
      (sess, _) <- Sessions.sessionAndProject pid
      now <- Time.currentTime
      -- Generate new dashboard ID
      newDashId <- Dashboards.DashboardId <$> UUID.genUUID

      -- Create new title with "(Copy)" suffix
      let copyTitle = if dashVM.title == "" then "Untitled (Copy)" else dashVM.title <> " (Copy)"
      -- Update schema title if it exists
      let updatedSchema =
            dashVM.schema
              & _Just . #title %~ \t ->
                Just $ case t of
                  Nothing -> copyTitle
                  Just "" -> "Untitled (Copy)"
                  Just title -> title <> " (Copy)"

      -- Insert the new dashboard
      _ <-
        dbtToEff $
          DBT.insert @Dashboards.DashboardVM $
            dashVM
              { Dashboards.id = newDashId
              , Dashboards.createdAt = now
              , Dashboards.updatedAt = now
              , Dashboards.createdBy = sess.user.id
              , Dashboards.title = copyTitle
              , Dashboards.schema = updatedSchema
              , Dashboards.starredSince = Nothing
              , Dashboards.homepageSince = Nothing
              }

      -- Redirect to the new dashboard
      let redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> newDashId.toText
      redirectCS redirectURI
      addSuccessToast "Dashboard was duplicated successfully" Nothing
      addRespHeaders NoContent


-- | Handler for deleting a dashboard.
-- It verifies the dashboard exists and belongs to the project before deletion.
-- After deletion, redirects to the dashboard list page.
dashboardDeleteH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders NoContent)
dashboardDeleteH pid dashId = do
  mDashboard <- dbtToEff $ DBT.selectOneByField @Dashboards.DashboardVM [DBT.field| id |] (Only dashId)
  case mDashboard of
    Nothing -> throwError $ err404{errBody = "Dashboard not found or does not belong to this project"}
    Just _ -> do
      dbtToEff $ DBT.delete @Dashboards.DashboardVM (Only dashId)

      -- Redirect to dashboard list page
      let redirectURI = "/p/" <> pid.toText <> "/dashboards"
      redirectCS redirectURI
      addSuccessToast "Dashboard was deleted successfully" Nothing
      addRespHeaders NoContent


-- | Form data for moving a widget between dashboards
data WidgetMoveForm = WidgetMoveForm
  { widgetId :: Text
  , sourceDashboardId :: Dashboards.DashboardId
  , targetDashboardId :: Dashboards.DashboardId
  }
  deriving stock (Show, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Handler for moving a widget from one dashboard to another.
-- It removes the widget from the source dashboard and adds it to the target dashboard.
dashboardMoveWidgetPostH :: Projects.ProjectId -> WidgetMoveForm -> ATAuthCtx (RespHeaders NoContent)
dashboardMoveWidgetPostH pid form = do
  -- Get source and target dashboards
  sourceDashVM <-
    dbtToEff (DBT.selectById @Dashboards.DashboardVM (Only form.sourceDashboardId)) >>= \case
      Just vm -> pure vm
      Nothing -> throwError $ err404{errBody = "Source dashboard not found"}

  targetDashVM <-
    dbtToEff (DBT.selectById @Dashboards.DashboardVM (Only form.targetDashboardId)) >>= \case
      Just vm -> pure vm
      Nothing -> throwError $ err404{errBody = "Target dashboard not found"}

  -- Load dashboard schemas
  (_, sourceDash) <- getDashAndVM form.sourceDashboardId Nothing

  -- Find the widget to move
  let widgetToMoveM = find (\w -> (w.id == Just form.widgetId) || (maybeToMonoid (slugify <$> w.title) == form.widgetId)) sourceDash.widgets

  case widgetToMoveM of
    Nothing -> throwError $ err404{errBody = "Widget not found in source dashboard"}
    Just widgetToMove -> do
      -- Get the target dashboard schema (or template if not set)
      targetDash <- case targetDashVM.schema of
        Just schema -> pure schema
        Nothing -> case loadDashboardFromVM targetDashVM of
          Just template -> pure template
          Nothing -> throwError $ err404{errBody = "Could not load target dashboard schema or template"}

      -- Get current time for updated_at field
      now <- Time.currentTime

      -- Remove widget from source dashboard
      let updatedSourceWidgets = filter (\w -> (w.id /= Just form.widgetId) && (maybeToMonoid (slugify <$> w.title) /= form.widgetId)) sourceDash.widgets
      let updatedSourceDash = sourceDash{Dashboards.widgets = updatedSourceWidgets}

      -- Add widget to target dashboard
      let updatedTargetDash = targetDash{Dashboards.widgets = targetDash.widgets <> [widgetToMove]}

      -- Update both dashboards in the database
      _ <-
        dbtToEff $
          DBT.updateFieldsBy @Dashboards.DashboardVM
            [[DBT.field| schema |], [DBT.field| updated_at |]]
            ([DBT.field| id |], form.sourceDashboardId)
            (updatedSourceDash, now)

      _ <-
        dbtToEff $
          DBT.updateFieldsBy @Dashboards.DashboardVM
            [[DBT.field| schema |], [DBT.field| updated_at |]]
            ([DBT.field| id |], form.targetDashboardId)
            (updatedTargetDash, now)

      addSuccessToast "Widget moved successfully" Nothing
      addRespHeaders NoContent


-- | Handler for duplicating a widget within the same dashboard.
-- It creates a copy of the widget with "(Copy)" appended to the title.
-- Returns the duplicated widget that will be converted to HTML automatically.
dashboardDuplicateWidgetPostH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardDuplicateWidgetPostH pid dashId widgetId = do
  -- Load the dashboard
  (_, dash) <- getDashAndVM dashId Nothing

  -- Find the widget to duplicate
  let widgetToDuplicateM = find (\w -> (w.id == Just widgetId) || (maybeToMonoid (slugify <$> w.title) == widgetId)) dash.widgets

  case widgetToDuplicateM of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just widgetToDuplicate -> do
      -- Generate a new ID for the widget
      newWidgetId <- UUID.genUUID >>= pure . UUID.toText

      -- Create a copy of the widget with a new ID and modified title
      let widgetCopy =
            widgetToDuplicate
              { Widget.id = Just newWidgetId
              , Widget.title = case widgetToDuplicate.title of
                  Nothing -> Just "Widget Copy"
                  Just "" -> Just "Widget Copy"
                  Just title -> Just (title <> " (Copy)")
              , Widget._projectId = Just pid
              , Widget._dashboardId = Just dashId.toText
              }

      -- Add the copy to the dashboard
      let updatedDash = dash{Dashboards.widgets = dash.widgets <> [widgetCopy]}

      -- Update the dashboard in the database
      now <- Time.currentTime
      _ <-
        dbtToEff $
          DBT.updateFieldsBy @Dashboards.DashboardVM
            [[DBT.field| schema |], [DBT.field| updated_at |]]
            ([DBT.field| id |], dashId)
            (updatedDash, now)

      addSuccessToast "Widget duplicated successfully" Nothing

      -- Return the widget directly (will be converted to HTML by ToHtml instance)
      addRespHeaders widgetCopy


-- | Handler for expanding a widget to full-screen view in a drawer.
-- Returns an HTML representation of the expanded widget with controls.
dashboardWidgetExpandGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders (Html ()))
dashboardWidgetExpandGetH pid dashId widgetId = do
  -- Load the dashboard
  (_, dash) <- getDashAndVM dashId Nothing
  now <- Time.currentTime

  -- Find the widget to expand
  let widgetToExpandM = find (\w -> (w.id == Just widgetId) || (maybeToMonoid (slugify <$> w.title) == widgetId)) dash.widgets

  case widgetToExpandM of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just widgetToExpand -> do
      -- Process the widget to ensure data is loaded
      processedWidget <- processWidget pid now (Nothing, Nothing, Nothing) [] widgetToExpand

      -- Create an expanded view of the widget using our unified component
      -- Start with "overview" tab active by default, but user can switch to edit if they want
      let expandedWidgetHtml = widgetViewerEditor_ pid (Just dashId) Nothing (Just processedWidget) "overview"

      addRespHeaders expandedWidgetHtml
