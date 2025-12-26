module Pages.Dashboards (
  dashboardGetH,
  entrypointRedirectGetH,
  DashboardGet (..),
  dashboardsGetH,
  DashboardsGet (..),
  DashboardFilters (..),
  dashboardsPostH,
  DashboardForm (..),
  dashboardWidgetPutH,
  dashboardWidgetReorderPatchH,
  WidgetReorderItem (..),
  dashboardDeleteH,
  dashboardRenamePatchH,
  DashboardRenameForm (..),
  dashboardDuplicatePostH,
  dashboardStarPostH,
  WidgetMoveForm (..),
  DashboardBulkActionForm (..),
  DashboardRes (..),
  DashboardsGetD (..),
  dashboardDuplicateWidgetPostH,
  dashboardWidgetExpandGetH,
  visTypes,
  processEagerWidget,
  dashboardBulkActionPostH,
) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as Wreq
import Data.Generics.Labels ()
import Data.HashMap.Lazy qualified as HM
import Data.HashMap.Lazy qualified as M
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Types (Query (Query))
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxConfirm_, hxDelete_, hxExt_, hxGet_, hxPatch_, hxPost_, hxPushUrl_, hxPut_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ProjectMembers qualified as ManageMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation
import Network.HTTP.Types.URI qualified as URI
import Pages.Anomalies qualified as AnomalyList
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Pages.Components qualified as Components
import Pages.GitSync qualified as GitSyncPage
import Pages.LogExplorer.LogItem (getServiceName)
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..), logQueryBox_, visTypes)
import Pkg.Components.Table (BulkAction (..), Table (..))
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import Servant (NoContent (..), ServerError, err404, errBody)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Config (AuthContext (..))
import System.Types
import Text.Slugify (slugify)
import UnliftIO.Exception (try)
import Utils
import Web.FormUrlEncoded (FromForm)


-- | Sync file_path and file_sha for a dashboard after any update.
-- Only recomputes SHA when the schema content has actually changed.
syncDashboardFileInfo :: DB es => Dashboards.DashboardId -> Eff es ()
syncDashboardFileInfo dashId = do
  dashM <- Dashboards.getDashboardById dashId
  forM_ dashM \dash -> do
    teams <- ManageMembers.getTeamsById dash.projectId dash.teams
    let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
        filePath = GitSync.titleToFilePath dash.title
        newSha = GitSync.computeContentSha $ GitSync.dashboardToYaml schema
    when (dash.fileSha /= Just newSha || dash.filePath /= Just filePath)
      $ void
      $ GitSync.updateDashboardGitInfo dashId filePath newSha


-- | Sync dashboard file info and queue a git push if sync is configured.
-- This is the consolidated function to use in handlers after dashboard changes.
syncDashboardAndQueuePush :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx ()
syncDashboardAndQueuePush pid dashId = do
  syncDashboardFileInfo dashId
  GitSyncPage.queueGitSyncPush pid dashId


-- Filter record for dashboard list
newtype DashboardFilters = DashboardFilters
  { tag :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Default)


instance FromForm DashboardFilters


data DashboardGet = DashboardGet Projects.ProjectId Dashboards.DashboardId Dashboards.Dashboard Dashboards.DashboardVM [(Text, Maybe Text)]


instance ToHtml DashboardGet where
  toHtml (DashboardGet pid dashId dash dashVM allParams) = toHtml $ dashboardPage_ pid dashId dash dashVM allParams
  toHtmlRaw = toHtml


dashboardPage_ :: Projects.ProjectId -> Dashboards.DashboardId -> Dashboards.Dashboard -> Dashboards.DashboardVM -> [(Text, Maybe Text)] -> Html ()
dashboardPage_ pid dashId dash dashVM allParams = do
  -- when  $ freeTierLimitExceededBanner pid.toText
  Components.modal_ "pageTitleModalId" ""
    $ form_
      [ class_ "flex flex-col p-3 gap-3"
      , hxPatch_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/rename")
      , hxSwap_ "innerHTML"
      , hxTrigger_ "submit"
      , hxTarget_ "#pageTitleText"
      ]
    $ fieldset_ [class_ "fieldset min-w-xs"] do
      label_ [class_ "label"] "Change Dashboard Title"
      input_ [class_ "input", name_ "title", placeholder_ "Insert new title", value_ $ if dashVM.title == "" then "Untitled" else dashVM.title]
      div_ [class_ "mt-3 flex justify-end gap-2"] do
        label_ [Lucid.for_ "pageTitleModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
        button_ [type_ "submit", class_ "btn btn-primary"] "Save"

  -- Render variables and tabs in the same container
  when (isJust dash.variables || isJust dash.tabs) $ div_ [class_ "flex bg-fillWeaker px-6 py-2 gap-4 items-center flex-wrap"] do
    -- Tabs section (on the left)
    whenJust dash.tabs \tabs -> do
      let activeTabIdx = fromMaybe 0 $ readMaybe . toString =<< join (L.lookup "tab" allParams)
      div_ [role_ "tablist", class_ "tabs tabs-box tabs-outline"] do
        forM_ (zip [0 ..] tabs) \(idx, tab) -> do
          let tabId = "dashboard-tab-" <> dashId.toText <> "-" <> show idx
          label_
            [ role_ "tab"
            , class_ "tab group flex items-center gap-2 has-[:checked]:tab-active"
            ]
            do
              input_
                ( [ type_ "radio"
                  , name_ $ "dashboard-tabs-" <> dashId.toText
                  , id_ tabId
                  , class_ "hidden"
                  , onchange_ $ "const url = new URL(location); url.searchParams.set('tab', '" <> show idx <> "'); history.pushState({}, '', url)"
                  ]
                    <> [checked_ | idx == activeTabIdx]
                )
              toHtml tab.name

    -- Variables section (pushed to the right)
    whenJust dash.variables \variables -> do
      div_ [class_ $ "flex gap-2 flex-wrap " <> if isJust dash.tabs then "ml-auto" else ""] do
        forM_ variables \var -> fieldset_ [class_ "border border-strokeStrong bg-fillWeaker p-0 inline-block rounded-lg dash-variable text-sm"] do
          legend_ [class_ "px-1 ml-2 text-xs"] $ toHtml $ fromMaybe var.key var.title <> memptyIfFalse (var.required == Just True) " *"
          let whitelist =
                maybe
                  "[]"
                  ( decodeUtf8
                      . fromLazy
                      . AE.encode
                      . map \opt ->
                        AE.object
                          [ "value" AE..= (opt Unsafe.!! 0)
                          , "name" AE..= fromMaybe (opt Unsafe.!! 0) (opt !!? 1)
                          ]
                  )
                  var.options

          input_
            $ [ type_ "text"
              , name_ var.key
              , class_ "tagify-select-input"
              , data_ "whitelistjson" whitelist
              , data_ "enforce-whitelist" "true"
              , data_ "mode" $ if var.multi == Just True then "" else "select"
              , data_ "query_sql" $ maybeToMonoid var.sql
              , data_ "query" $ maybeToMonoid var.query
              , data_ "reload_on_change" $ maybe "false" (T.toLower . show) var.reloadOnChange
              , value_ $ maybeToMonoid var.value
              ]
            <> memptyIfFalse (var.multi == Just True) [data_ "mode" "select"]
    script_
      [text|
  window.addEventListener('DOMContentLoaded', () => {
    const tagifyInstances = new Map();
    document.querySelectorAll('.tagify-select-input').forEach(input => {
      const tgfy = createTagify(input, {
        whitelist: JSON.parse(input.dataset.whitelistjson || "[]"),
        enforceWhitelist: true,
        tagTextProp: 'name',
        mode: input.dataset.mode || "",
      });

      const inputKey = input.getAttribute('name') || input.id;
      tagifyInstances.set(inputKey, tgfy);

      tgfy.on('change', (e) => {
        const varName = e.detail.tagify.DOM.originalInput.getAttribute('name');
        const url = new URL(window.location);
        url.searchParams.set('var-' + varName, e.detail?.tagify?.value[0]?.value);
        history.pushState({}, '', url);
        window.dispatchEvent(new Event('update-query'));
      });
    });

    window.addEventListener('update-query', async (e) => {
      document.querySelectorAll('.tagify-select-input[data-reload_on_change="true"]').forEach(async input => {
        const { query_sql, query } = input.dataset;
        if (!query_sql && !query) return;

        try {
          const tagify = tagifyInstances.get(input.getAttribute('name') || input.id);
          tagify?.loading(true);

          const params = new URLSearchParams({ ...Object.fromEntries(new URLSearchParams(location.search)),
            query, query_sql, data_type: 'text' });

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
  });
    |]
  section_ [class_ "h-full"] $ div_ [class_ "mx-auto mb-20 pt-5 pb-6 px-6 gap-3.5 w-full flex flex-col h-full overflow-y-scroll pb-20 group/pg", id_ "dashboardPage"] do
    let activeTabIdx = fromMaybe 0 $ readMaybe . toString =<< join (L.lookup "tab" allParams)
    case dash.tabs of
      Just tabs -> do
        -- Tab system with CSS-based switching
        div_ [class_ "dashboard-tabs-container"] do
          forM_ (zip [0 ..] tabs) \(idx, tab) -> do
            -- Tab content panel
            div_
              [ class_ $ "tab-panel grid-stack -m-2" <> if idx == activeTabIdx then "" else " hidden"
              , data_ "tab-index" (show idx)
              , id_ $ "tab-panel-" <> dashId.toText <> "-" <> show idx
              ]
              do
                forM_ tab.widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
                when (null tab.widgets) $ label_ [id_ $ "add_widget_tab_" <> show idx, class_ "grid-stack-item pb-8 cursor-pointer bg-fillBrand-weak border-2 border-strokeBrand-strong border-dashed text-strokeSelected rounded-sm rounded-lg flex flex-col gap-3 items-center justify-center", term "gs-w" "3", term "gs-h" "2", Lucid.for_ "page-data-drawer"] do
                  faSprite_ "plus" "regular" "h-8 w-8"
                  span_ "Add a widget"
      Nothing -> do
        -- Fall back to old behavior for dashboards without tabs
        div_
          [class_ "grid-stack -m-2"]
          do
            forM_ (dash :: Dashboards.Dashboard).widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
            when (null (dash :: Dashboards.Dashboard).widgets) $ label_ [id_ "add_a_widget_label", class_ "grid-stack-item pb-8 cursor-pointer bg-fillBrand-weak border-2 border-strokeBrand-strong border-dashed text-strokeSelected rounded-sm rounded-lg flex flex-col gap-3 items-center justify-center *:right-0!  *:bottom-0! ", term "gs-w" "3", term "gs-h" "2", Lucid.for_ "page-data-drawer"] do
              faSprite_ "plus" "regular" "h-8 w-8"
              span_ "Add a widget"
    let projectId = pid.toText
    let dashboardId = dashId.toText
    -- Add hidden element for the auto-refresh handler
    div_ [id_ "dashboard-refresh-handler", class_ "hidden"] ""

    script_
      [text|
      document.addEventListener('DOMContentLoaded', () => {
        // Handle tab switching
        document.querySelectorAll('input[name="dashboard-tabs-${dashboardId}"]').forEach(radio => {
          radio.addEventListener('change', function() {
            if (this.checked) {
              const tabIndex = this.id.split('-').pop();
              // Hide all panels
              document.querySelectorAll('.tab-panel').forEach(p => p.classList.add('hidden'));
              // Show active panel
              const activePanel = document.getElementById('tab-panel-${dashboardId}-' + tabIndex);
              activePanel.classList.remove('hidden');
              // Update gridStackInstance to point to the active tab's grid
              if (activePanel.gridstack) {
                window.gridStackInstance = activePanel.gridstack;
              }
            }
          });
        });
        
        GridStack.renderCB = function(el, w) {
          el.innerHTML = w.content;
          const scripts = Array.from(el.querySelectorAll('script'));
          scripts.forEach(oldScript => {
            const newScript = document.createElement('script');
            Array.from(oldScript.attributes).forEach(attr => newScript.setAttribute(attr.name, attr.value));
            if (oldScript.textContent) {newScript.textContent = oldScript.textContent}
            oldScript.parentNode.replaceChild(newScript, oldScript);
          });
        };

        // Initialize all grids (for both tabs and non-tab dashboards)
        const gridInstances = [];
        document.querySelectorAll('.grid-stack').forEach(gridEl => {
          if (!gridEl.classList.contains('grid-stack-initialized')) {
            const grid = GridStack.init({
              column: 12,
              acceptWidgets: true,
              cellHeight: '5rem',
              marginTop: '0.05rem',
              marginLeft: '0.5rem',
              marginRight: '0.5rem',
              marginBottom: '2rem',
              handleClass: 'grid-stack-handle',
              styleInHead: true,
              staticGrid: false,
            }, gridEl);

            grid.on('removed change', debounce(updateWidgetOrder('${projectId}', '${dashboardId}'), 200));
            gridEl.classList.add('grid-stack-initialized');
            gridInstances.push(grid);
            // Set global gridStackInstance to the first (or visible) grid for hyperscript access
            if (!window.gridStackInstance) {
              window.gridStackInstance = grid;
            }
          }
        });

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
            handleClass: 'nested-grid-stack-handle',
            styleInHead: true,
            staticGrid: false,
          }, nestedEl);
          // nestedInstance.compact()
          nestedInstance.on('removed change', debounce(updateWidgetOrder('${projectId}', '${dashboardId}'), 200));
          nestedGridInstances.push(nestedInstance);
        });
      })
        // Listen for widget-remove-requested custom events
        document.addEventListener('widget-remove-requested', function(e) {
          const widgetEl = document.getElementById(e.detail.widgetId + '_widgetEl');
          if (widgetEl) {
             gridStackInstance.removeWidget(widgetEl, true);
            // Find which nested grid contains this widget
            for (const nestedInstance of nestedGridInstances) {
              try {
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
processVariable pid now timeRange@(sinceStr, fromDStr, toDStr) allParams variableBase = do
  let (fromD, toD, _) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)
      paramsMap = Map.fromList allParams
      variable' = Dashboards.replaceQueryVariables pid fromD toD allParams now variableBase
      variable = variable'{Dashboards.value = join (Map.lookup ("var-" <> variable'.key) paramsMap) <|> variable'.value}

  case variable._vType of
    Dashboards.VTQuery | Just sqlQuery <- variable.sql -> do
      result <- try $ PG.query_ (Query $ encodeUtf8 sqlQuery)
      case result of
        Right queryResults -> pure variable{Dashboards.options = Just queryResults}
        Left (_ :: SomeException) -> pure variable -- Return unchanged on error
    _ -> pure variable


-- Process a single widget recursively.
processWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processWidget pid now timeRange@(sinceStr, fromDStr, toDStr) allParams widgetBase = do
  let widget = widgetBase & #_projectId %~ (<|> Just pid)

  widget' <-
    if widget.eager == Just True || widget.wType == Widget.WTAnomalies
      then processEagerWidget pid now timeRange allParams widget
      else pure widget

  -- Recursively process child widgets
  forOf (#children . _Just . traverse) widget' $ \child ->
    processWidget pid now timeRange allParams
      $ child
      & #_dashboardId %~ (<|> widget'._dashboardId)


processEagerWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processEagerWidget pid now (sinceStr, fromDStr, toDStr) allParams widget = case widget.wType of
  Widget.WTAnomalies -> do
    (issues, _) <- Issues.selectIssues pid Nothing (Just False) (Just False) 2 0 Nothing Nothing
    let issuesVM = V.fromList $ map (AnomalyList.IssueVM False True now "24h") issues
    pure $ widget
      & #html
        ?~ renderText
          ( div_ [class_ "flex flex-col gap-4 h-full w-full overflow-hidden"] $ forM_ issuesVM \vm@(AnomalyList.IssueVM hideByDefault _ _ _ issue) ->
              div_ [class_ "border border-strokeWeak rounded-2xl overflow-hidden"] do
                Table.renderRowWithColumns
                  [ class_ $ "flex gap-8 items-start itemsListItem " <> if hideByDefault then "surface-raised rounded-2xl" else "px-0.5 py-4"
                  , style_ (if hideByDefault then "display:none" else "")
                  ]
                  (AnomalyList.issueColumns issue.projectId)
                  vm
          )
  Widget.WTStat -> do
    stat <- Charts.queryMetrics (Just Charts.DTFloat) (Just pid) widget.query widget.sql sinceStr fromDStr toDStr Nothing allParams
    pure $ widget & #dataset ?~ def{Widget.source = AE.Null, Widget.value = stat.dataFloat}
  Widget.WTTable -> do
    -- Fetch table data
    tableData <- Charts.queryMetrics (Just Charts.DTText) (Just pid) widget.query widget.sql sinceStr fromDStr toDStr Nothing allParams
    -- Render the table with data server-side
    pure
      $ widget
      & #html
        ?~ renderText (Widget.renderTableWithDataAndParams widget tableData.dataText allParams)
  Widget.WTTraces -> do
    tracesD <- Charts.queryMetrics (Just Charts.DTText) (Just pid) widget.query widget.sql sinceStr fromDStr toDStr Nothing allParams
    let trIds = V.map V.last tracesD.dataText
    shapeWithDuration <- Telemetry.getTraceShapes pid trIds
    let grouped = M.fromListWith (++) [(trId, [(spanName, duration, events)]) | (trId, spanName, duration, events) <- shapeWithDuration]

    spanRecords' <- Telemetry.getSpanRecordsByTraceIds pid trIds Nothing
    let spanRecords = V.fromList $ mapMaybe Telemetry.convertOtelLogsAndSpansToSpanRecord spanRecords'
        serviceColors = getServiceColors ((\x -> getServiceName x.resource) <$> spanRecords)
    let colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList serviceColors]
    let spansGrouped = M.fromListWith (++) [(sp.traceId, [sp]) | sp <- V.toList spanRecords]
    shapesAvgs <- Telemetry.getTraceShapes pid trIds

    pure
      $ widget
      & #html
        ?~ renderText (Widget.renderTraceDataTable widget tracesD.dataText grouped spansGrouped colorsJson)
  _ -> do
    metricsD <- Charts.queryMetrics (Just Charts.DTMetric) (Just pid) widget.query widget.sql sinceStr fromDStr toDStr Nothing allParams
    pure
      $ widget
      & #dataset
        ?~ Widget.WidgetDataset
          { source = AE.toJSON $ V.cons (AE.toJSON <$> metricsD.headers) (AE.toJSON <<$>> metricsD.dataset)
          , rowsPerMin = metricsD.rowsPerMin
          , value = Just metricsD.rowsCount
          , from = metricsD.from
          , to = metricsD.to
          , stats = metricsD.stats
          }


dashboardWidgetPutH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Widget.Widget -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardWidgetPutH pid dashId widgetIdM widget = do
  (_, dash) <- getDashAndVM dashId Nothing
  uid <- UUID.genUUID <&> UUID.toText
  let (dash', widget') = case widgetIdM of
        Just wID -> do
          let normalizedWidgetId = T.replace "Expanded" "" wID

          let updateWidget w =
                if (w.id == Just normalizedWidgetId) || (maybeToMonoid (slugify <$> w.title) == normalizedWidgetId)
                  then widget{Widget.standalone = Nothing, Widget.naked = Nothing, Widget.id = Just normalizedWidgetId}
                  else w
              updatedWidgets = map updateWidget (dash :: Dashboards.Dashboard).widgets
              updatedWidget = widget{Widget.standalone = Nothing, Widget.naked = Nothing, Widget.id = Just normalizedWidgetId}
          ((dash :: Dashboards.Dashboard) & #widgets .~ updatedWidgets, updatedWidget)
        Nothing -> do
          -- When adding a new widget
          let widgetUpdated = widget{Widget.standalone = Nothing, Widget.naked = Nothing, Widget.id = Just uid, Widget._centerTitle = Nothing}
          ((dash :: Dashboards.Dashboard) & #widgets %~ (<> [widgetUpdated]), widgetUpdated)

  _ <- Dashboards.updateSchema dashId dash'
  syncDashboardAndQueuePush pid dashId

  let successMsg = case widgetIdM of
        Just _ -> "Widget updated successfully"
        Nothing -> "Widget added to dashboard successfully"

  addSuccessToast successMsg Nothing
  addTriggerEvent "closeModal" ""
  addRespHeaders widget'


data WidgetReorderItem = WidgetReorderItem
  { w :: Maybe Int
  , h :: Maybe Int
  , x :: Maybe Int
  , y :: Maybe Int
  , children :: Maybe (Map Text WidgetReorderItem)
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake WidgetReorderItem


dashboardWidgetReorderPatchH
  :: Projects.ProjectId
  -> Dashboards.DashboardId
  -> Map Text WidgetReorderItem
  -- ^ The ordered list of widget IDs
  -> ATAuthCtx (RespHeaders NoContent)
dashboardWidgetReorderPatchH pid dashId widgetOrder = do
  (_, dash) <- getDashAndVM dashId Nothing

  let sortedWidgets = reorderWidgets widgetOrder (dash :: Dashboards.Dashboard).widgets
      newDash = (dash :: Dashboards.Dashboard) & #widgets .~ sortedWidgets

  _ <- Dashboards.updateSchema dashId newDash
  syncDashboardAndQueuePush pid dashId

  addRespHeaders NoContent


-- | Rebuild the widget tree based solely on the reorder patch.
-- Widgets not mentioned in the patch are dropped.
reorderWidgets :: Map Text WidgetReorderItem -> [Widget.Widget] -> [Widget.Widget]
reorderWidgets patch ws = mapMaybe findAndUpdate (Map.toList patch)
  where
    widgetMap = mkWidgetMap ws

    findAndUpdate (wid, item) = do
      orig <- Map.lookup wid widgetMap
      let newLayout =
            Just
              $ maybe def Relude.id orig.layout
              & #x %~ (<|> item.x)
              & #y %~ (<|> item.y)
              & #w %~ (<|> item.w)
              & #h %~ (<|> item.h)
      pure
        orig
          { Widget.layout = newLayout
          , Widget.children = item.children <&> (`reorderWidgets` fold orig.children)
          }

    mkWidgetMap = Map.fromList . concatMap flatten
    flatten w = (widgetId w, w) : maybe [] (concatMap flatten) w.children
    widgetId w = fromMaybe (maybeToMonoid $ slugify <$> w.title) w.id


getDashAndVM :: (DB es, Error ServerError :> es, Wreq.HTTP :> es) => Dashboards.DashboardId -> Maybe Text -> Eff es (Dashboards.DashboardVM, Dashboards.Dashboard)
getDashAndVM dashId fileM = do
  dashVM <-
    Dashboards.getDashboardById dashId >>= \case
      Just v -> pure v
      Nothing -> throwError $ err404{errBody = "Dashboard with ID not found. ID:" <> encodeUtf8 dashId.toText}

  dash <- case fileM of
    Just file -> Dashboards.readDashboardEndpoint file
    Nothing -> pure $ fromMaybe def (loadDashboardFromVM dashVM)

  pure (dashVM, dash)


dashboardGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId fileM fromDStr toDStr sinceStr allParams = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)

  (dashVM, dash) <- getDashAndVM dashId fileM
  dash' <- forOf (#variables . traverse . traverse) dash (processVariable pid now (sinceStr, fromDStr, toDStr) allParams)

  let processWidgetWithDashboardId w = do
        processed <- processWidget pid now (sinceStr, fromDStr, toDStr) allParams w
        pure $ processed{Widget._dashboardId = Just dashId.toText}

  -- Process widgets in the main widgets array
  dash'' <- forOf (#widgets . traverse) dash' processWidgetWithDashboardId

  -- Also process widgets in tabs if they exist
  dash''' <- forOf (#tabs . _Just . traverse . #widgets . traverse) dash'' processWidgetWithDashboardId

  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "Dashboards"
          , pageTitle = if dashVM.title == "" then "Untitled" else dashVM.title
          , pageTitleModalId = Just "pageTitleModalId"
          , config = appCtx.config
          , freeTierExceeded = freeTierExceeded
          , pageActions = Just $ div_ [class_ "flex gap-3 items-center"] do
              TimePicker.timepicker_ Nothing currentRange Nothing
              TimePicker.refreshButton_
              div_ [class_ "flex items-center"] do
                span_ [class_ "text-fillDisabled mr-2"] "|"
                Components.drawer_ "page-data-drawer" Nothing (Just $ newWidget_ pid currentRange) $ span_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg", data_ "tippy-content" "Add a new widget"] $ faSprite_ "plus" "regular" "w-3 h-3"
                div_ [class_ "dropdown dropdown-end"] do
                  div_ [tabindex_ "0", role_ "button", class_ "text-iconNeutral cursor-pointer  p-2 hover:bg-fillWeak rounded-lg", data_ "tippy-content" "Context Menu"] $ faSprite_ "ellipsis" "regular" "w-4 h-4"
                  ul_ [tabindex_ "0", class_ "dropdown-content menu menu-md bg-base-100 rounded-box p-2 w-52 shadow-sm leading-none"] do
                    li_ $ label_ [Lucid.for_ "pageTitleModalId", class_ "p-2"] "Rename dashboard"
                    li_
                      $ button_
                        [ class_ "p-2 w-full text-left"
                        , hxPost_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/duplicate")
                        , hxSwap_ "none"
                        , data_ "tippy-content" "Creates a copy of this dashboard"
                        ]
                        "Duplicate dashboard"
                    li_
                      $ button_
                        [ class_ "p-2 w-full text-left text-textError"
                        , hxDelete_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText)
                        , hxSwap_ "none"
                        , hxConfirm_ "Are you sure you want to delete this dashboard? This action cannot be undone."
                        , data_ "tippy-content" "Permanently deletes this dashboard"
                        ]
                        "Delete dashboard"
          , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/dashboard/"
          }
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dashId dash''' dashVM allParams


-- | A unified widget viewer/editor component that uses DaisyUI tabs without JavaScript
-- @param pid Project ID
-- @param dashboardIdM Optional dashboard ID
-- @param currentRange Time range for the widget
-- @param existingWidgetM Optional existing widget (for edit mode)
-- @param activeTab Which tab should be active initially ("edit" or "overview")
widgetViewerEditor_ :: Projects.ProjectId -> Maybe Dashboards.DashboardId -> Maybe (Text, Text) -> Maybe Widget.Widget -> Text -> Html ()
widgetViewerEditor_ pid dashboardIdM currentRange existingWidgetM activeTab = div_ [class_ "group/wgtexp"] do
  let isNewWidget = isNothing existingWidgetM
  let effectiveActiveTab = if isNewWidget then "edit" else activeTab

  let defaultWidget =
        (def :: Widget.Widget)
          { Widget.wType = Widget.WTTimeseries
          , Widget.id = Just "newWidget"
          , Widget.standalone = Just True
          , Widget.naked = Just True
          , Widget.title = Just "New Widget"
          , Widget.hideSubtitle = Just True
          , Widget.query = Nothing
          , Widget.unit = Just "ms"
          , Widget._projectId = Just pid
          , Widget._dashboardId = dashboardIdM <&> (.toText)
          , Widget.layout = Just $ def{Widget.w = Just 3, Widget.h = Just 3}
          }
      widgetToUse' = fromMaybe defaultWidget existingWidgetM
      widgetToUse = (fromMaybe defaultWidget existingWidgetM){Widget.id = Just $ maybeToMonoid widgetToUse'.id <> "Expanded", Widget.standalone = Just True, Widget.naked = Just True}

  -- Generate unique IDs for all elements based on widget ID to prevent conflicts
  let wid = maybeToMonoid widgetToUse.id
      sourceWid = T.replace "Expanded" "" wid
      widPrefix = "id" <> T.take 8 wid
      widgetFormId = widPrefix <> "-widget-form"
      widgetPreviewId = widPrefix <> "-widget-preview"
      widgetTitleInputId = widPrefix <> "-widget-title-input"
      -- queryBuilderId = widPrefix <> "-queryBuilder" -- removed unused variable
      -- filterElementId = widPrefix <> "-filterElement" -- removed unused variable
      -- widgetTypeNameId = widPrefix <> "-widgetType" -- removed unused variable
      drawerStateCheckbox = if isJust existingWidgetM then "global-data-drawer" else "page-data-drawer"

  let widgetJSON = decodeUtf8 $ fromLazy $ AE.encode widgetToUse
  let formAction = case (dashboardIdM, existingWidgetM) of
        (Just dashId, Just w) -> "?widget_id=" <> fromMaybe "" w.id
        _ -> ""

  form_
    [ class_ "hidden"
    , id_ widgetFormId
    , hxPut_ formAction
    , hxVals_ "js:{...widgetJSON}"
    , hxExt_ "json-enc"
    , data_ "formMode" $ if isNewWidget then "new" else "edit"
    , hxTarget_ ("#" <> widgetFormId)
    , hxTrigger_ "submit"
    , term
        "_"
        [text| on htmx:beforeRequest 
            set widgetJSON.title to #{'${widgetTitleInputId}'}.value then 
            if not widgetJSON.id
              gridStackInstance.removeWidget('#add_a_widget_label', true, false)
            end
           on htmx:beforeSwap 
            set event.detail.shouldSwap to false then
            set #${drawerStateCheckbox}.checked to false then
            if @data-formMode == "edit"
              call gridStackInstance.update(#{'${sourceWid}_widgetEl'}, {content: event.detail.serverResponse})
            else
              call gridStackInstance.addWidget({w: 3, h: 3, content: event.detail.serverResponse})
            end
        |]
    ]
    ""

  div_ [class_ "flex justify-between items-center mb-4"] do
    div_ [class_ "flex justify-between"] do
      unless isNewWidget
        $ div_ [class_ "tabs tabs-box tabs-outline"] do
          let mkTab tabName isActive = label_ [role_ "tab", class_ "tab has-[:checked]:tab-active"] do
                input_
                  $ [ type_ "radio"
                    , value_ tabName
                    , class_ $ "hidden page-drawer-tab-" <> T.toLower tabName
                    , name_ $ wid <> "-drawer-tab"
                    ]
                  <> [checked_ | isActive]
                toHtml tabName
          mkTab "Overview" (effectiveActiveTab /= "edit")
          mkTab "Edit" (effectiveActiveTab == "edit")
      when isNewWidget $ h3_ [class_ "text-lg font-normal"] "Add a new widget"

    div_ [class_ "flex items-center gap-2"] do
      TimePicker.timepicker_ Nothing currentRange (Just "widget")
      TimePicker.refreshButton_
      span_ [class_ "text-fillDisabled"] "|"
      if isNewWidget
        then button_ [class_ "leading-none rounded-lg px-4 py-2 cursor-pointer btn btn-primary shadow-sm leading-none !h-auto", type_ "submit", form_ widgetFormId] "Save changes"
        else button_ [class_ "leading-none rounded-lg px-4 py-2 cursor-pointer btn btn-primary shadow-sm leading-none hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block", type_ "submit", form_ widgetFormId] "Save changes"
      label_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg leading-none", data_ "tippy-content" "Close Drawer", Lucid.for_ drawerStateCheckbox] $ faSprite_ "xmark" "regular" "w-3 h-3"

  div_ [class_ "w-full aspect-4/1 p-3 rounded-lg bg-fillWeaker mb-4"] do
    script_ [text| var widgetJSON = ${widgetJSON}; |]
    div_
      [ id_ widgetPreviewId
      , class_ "h-full w-full"
      , hxPost_ ("/p/" <> pid.toText <> "/widget")
      , hxTrigger_ "intersect once, update-widget"
      , hxTarget_ "this"
      , hxSwap_ "innerHTML"
      , hxVals_ "js:{...widgetJSON}"
      , hxExt_ "json-enc"
      , term
          "_"
          [text| on 'update-widget-query'
               set widgetJSON.query to event.detail.value then
               set widgetJSON.title to #{'${widgetTitleInputId}'}.value then
               trigger 'update-widget' on me |]
      ]
      ""
  div_ [class_ $ if isNewWidget then "block" else "hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block"] do
    div_ [class_ "space-y-7"] do
      -- Select your visualization section removed to avoid circular dependencies

      div_ [class_ "space-y-4"] do
        div_ [class_ "flex gap-3"] do
          span_ [class_ "inline-block rounded-full bg-fillWeak px-3 py-1 leading-none"] "1"
          strong_ [class_ "text-lg font-semibold"] "Configure Query"
        div_ [class_ "px-5 flex flex-col gap-2"] do
          logQueryBox_
            LogQueryBoxConfig
              { pid = pid
              , currentRange = Nothing
              , source = Nothing
              , targetSpan = Nothing
              , query = widgetToUse.query
              , vizType = Just $ case widgetToUse.wType of
                  Widget.WTTimeseries -> "timeseries"
                  Widget.WTTimeseriesLine -> "timeseries_line"
                  Widget.WTLogs -> "logs"
                  _ -> "timeseries"
              , queryLibRecent = V.empty
              , queryLibSaved = V.empty
              , updateUrl = False
              , targetWidgetPreview = Just widgetPreviewId
              , alert = False
              , patternSelected = Nothing
              }

      div_ [class_ "space-y-4"] do
        div_ [class_ "flex gap-3"] do
          span_ [class_ "inline-block rounded-full bg-fillWeak px-3 py-1 leading-none"] "2"
          strong_ [class_ "text-lg font-semibold"] "Give your graph a title"
        div_ [class_ "space-x-8 px-5"]
          $ input_
            [ class_ "p-3 border border-strokeWeak w-full rounded-lg bg-transparent widget-title-input"
            , id_ widgetTitleInputId
            , placeholder_ "Throughput"
            , required_ "required"
            , value_ $ fromMaybe "" widgetToUse.title
            , term
                "_"
                [text| on change 
                 set widgetJSON.title to my value then 
                 trigger 'update-widget' on #{'${widgetPreviewId}'}
               |]
            ]


-- visTypes is now imported from LogQueryBox to avoid circular dependencies

-- | Backward compatibility wrapper for the new widget editor
newWidget_ :: Projects.ProjectId -> Maybe (Text, Text) -> Html ()
newWidget_ pid currentRange = widgetViewerEditor_ pid Nothing currentRange Nothing "edit"


--------------------------------------------------------------------
-- Dashboard List
--

data DashboardsGetD = DashboardsGetD
  { dashboards :: V.Vector Dashboards.DashboardVM
  , projectId :: Projects.ProjectId
  , embedded :: Bool -- Whether to render in embedded mode (for modals, no table headers)
  , hideActions :: Bool -- Whether to hide bulk actions (for team views)
  , teams :: V.Vector ManageMembers.Team
  , tableActions :: Maybe Table.TableHeaderActions
  , filters :: DashboardFilters
  , availableTags :: [Text]
  }
  deriving (Generic, Show)
data DashboardsGet
  = DashboardsGet (PageCtx DashboardsGetD)
  | DashboardsGetSlim DashboardsGetD
  deriving (Generic, Show)


instance ToHtml DashboardsGet where
  toHtml (DashboardsGet (PageCtx pc dg)) = toHtml $ PageCtx pc $ dashboardsGet_ dg
  toHtml (DashboardsGetSlim dash) = toHtml $ dashboardsGet_ dash
  toHtmlRaw = toHtml


renderDashboardListItem :: Bool -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Html ()
renderDashboardListItem checked tmplClass title value description icon prview = label_
  [ class_
      [text| cursor-pointer group/it text-sm border border-transparent hover:bg-fillWeaker hover:border-strokeWeak rounded-lg flex p-1.5 gap-2 items-center
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
    input_ $ [class_ $ "hidden " <> tmplClass, type_ "radio", name_ "file", value_ value] <> [checked_ | checked]
    span_ [class_ "p-1 px-2 bg-fillWeak rounded-md"] $ faSprite_ (fromMaybe "square-dashed" icon) "regular" "w-3 h-3"
    span_ [class_ "grow"] $ toHtml title
    span_ [class_ "px-2 p-1 invisible group-has-[input:checked]/it:visible"] $ faSprite_ "chevron-right" "regular" "w-3 h-3"


starButton_ :: Projects.ProjectId -> Dashboards.DashboardId -> Bool -> Html ()
starButton_ pid dashId isStarred = do
  let starIconType = if isStarred then "solid" else "regular"
  button_
    [ id_ $ "star-btn-" <> dashId.toText
    , class_ $ "leading-none cursor-pointer " <> if isStarred then "" else "opacity-0 group-hover/row:opacity-100"
    , data_ "tippy-content" $ if isStarred then "Click to unstar this dashboard" else "Click to star this dashboard"
    , hxPost_ $ "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/star"
    , hxTarget_ $ "#star-btn-" <> dashId.toText
    , hxSwap_ "outerHTML"
    ]
    $ faSprite_ "star" starIconType
    $ "w-4 h-4 "
    <> if isStarred then "text-yellow-500" else "text-iconNeutral"


dashboardsGet_ :: DashboardsGetD -> Html ()
dashboardsGet_ dg = do
  unless dg.embedded $ Components.modal_ "newDashboardMdl" "" $ form_
    [ class_ "flex  h-[90vh] gap-4 group/md"
    , hxPost_ ""
    , hxVals_ "js:{ teams: getSelectedTeams() }"
    ]
    do
      div_ [class_ "w-2/7 space-y-4 h-full flex flex-col"] do
        div_ [class_ "flex flex-col gap-2 border-b pb-4"] do
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
        div_ [class_ "space-y-1 h-auto overflow-auto", id_ "dashListItemParent"] do
          renderDashboardListItem True "tmplRadio0" "Blank dashboard" "" (Just "Get started from a blank slate") (Just "cards-blank") Nothing
          iforM_ dashboardTemplates \idx dashTmpl -> do
            let tmplItemClass = "tmplRadio" <> show (idx + 1)
            renderDashboardListItem False tmplItemClass (maybeToMonoid dashTmpl.title) (maybeToMonoid dashTmpl.file) dashTmpl.description dashTmpl.icon dashTmpl.preview

      div_ [class_ "w-5/7 px-3 py-5 h-full overflow-y-scroll "] do
        div_ [class_ "flex items-end justify-between gap-2"] do
          div_ [class_ "flex items-center w-full justify-between gap-2"] do
            label_ [class_ "flex flex-col gap-1 w-full"] do
              span_ [class_ "text-sm font-medium"] "Dashboard name"
              input_
                [ type_ "text"
                , class_ "input input-sm w-full shrink-1"
                , placeholder_ "Dashboard Title"
                , name_ "title"
                , required_ "required"
                ]
            label_ [class_ "flex flex-col gap-1 w-full"] do
              span_ [class_ "text-sm font-medium"] "Teams"
              input_
                [ type_ "text"
                , class_ "input input-sm w-full shrink-1"
                , id_ "teamHandlesInput"
                , name_ "teams"
                , placeholder_ "Add teams"
                ]

          div_ [class_ "flex items-center justify-center shrink"] $ button_ [class_ "btn btn-primary btn-sm", type_ "submit"] "Create"
        div_ [class_ "py-2 border-b border-b-strokeWeak"] do
          span_ [class_ "text-sm "] "Using "
          span_ [class_ "text-sm font-medium", id_ "dItemTitle"] "Custom Dashboard"
          span_ [class_ "text-sm "] " template"
          p_ [class_ "text-xs text-textWeak w-full overflow-ellipsis truncate", id_ "dItemDescription"] "Get started from a blank slate"
        div_ [class_ "pt-5"]
          $ div_ [class_ "bg-fillBrand-strong px-2 py-4 rounded-xl w-full flex items-center"]
          $ img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full rounded overflow-hidden", id_ "dItemPreview"]
        let teamList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= x.handle, "value" AE..= x.id]) <$> dg.teams
        script_
          [text|
            let tagify;
            window.addEventListener('DOMContentLoaded', (event) => {
              tagify = createTagify('#teamHandlesInput', {tagTextProp: 'name',whitelist: $teamList,});
            });
            const getSelectedTeams = () => {
              return tagify.value.map(item => item.value);
            }
        |]

  div_ [id_ "itemsListPage", class_ "mx-auto gap-8 w-full flex flex-col h-full overflow-hidden group/pg"] do
    let getTeams x = mapMaybe (\xx -> find (\t -> t.id == xx) dg.teams) (V.toList x.teams)

    let getDashIcon dash = maybe "square-dashed" (\d -> fromMaybe "square-dashed" d.icon) (loadDashboardFromVM dash)
        getWidgetCount dash = maybe 0 (length . (.widgets)) (loadDashboardFromVM dash)

    let renderNameCol dash = do
          let baseUrl = "/p/" <> dg.projectId.toText <> "/dashboards/" <> dash.id.toText
          span_ [class_ "flex items-center gap-2"] do
            span_ [class_ "p-1 px-2 bg-fillWeak rounded-md", data_ "tippy-content" "Dashboard icon"] $ faSprite_ (getDashIcon dash) "regular" "w-3 h-3"
            a_ [href_ baseUrl, class_ "font-medium text-textStrong hover:text-textBrand hover:underline underline-offset-2"] $ toHtml $ if dash.title == "" then "Untitled" else dash.title
            starButton_ dg.projectId dash.id (isJust dash.starredSince)

    let renderModifiedCol dash = span_ [class_ "monospace text-textWeak", data_ "tippy-content" "Last modified date"] $ toHtml $ toText $ formatTime defaultTimeLocale "%b %-e, %-l:%M %P" dash.updatedAt

    let renderTeamsCol dash = forM_ (getTeams dash) \team -> span_ [class_ "badge badge-sm badge-neutral mr-1"] $ toHtml team.handle

    let baseUrl = "/p/" <> dg.projectId.toText <> "/dashboards"
    let renderTagsCol dash = forM_ (V.toList dash.tags) \tag ->
          a_
            [ class_ "badge badge-sm badge-neutral mr-1 cursor-pointer hover:badge-primary"
            , hxGet_ $ baseUrl <> "?tag=" <> toUriStr tag
            , hxTarget_ "#dashboardsTableContainer"
            , hxSelect_ "#dashboardsTableContainer"
            , hxPushUrl_ "true"
            , hxSwap_ "outerHTML"
            ]
            $ toHtml tag

    let renderWidgetsCol dash = do
          let count = getWidgetCount dash
          span_ [class_ "flex items-center gap-2", data_ "tippy-content" $ "There are " <> show count <> " charts/widgets in this dashboard"] do
            faSprite_ "chart-area" "regular" "w-4 h-4 text-iconNeutral"
            span_ [class_ "leading-none monospace"] $ toHtml $ show count

    let tableCols =
          [ Table.col "Name" renderNameCol & Table.withAttrs [class_ "min-w-0"] & Table.withSort "title"
          , Table.col "Last Modified" renderModifiedCol & Table.withAttrs [class_ "w-44"] & Table.withSort "updated_at"
          , Table.col "Teams" renderTeamsCol & Table.withAttrs [class_ "w-48"]
          , Table.col "Tags" renderTagsCol & Table.withAttrs [class_ "w-48"]
          , Table.col "Widgets" renderWidgetsCol & Table.withAttrs [class_ "w-24"]
          ]

    let noBulkActions = dg.embedded || dg.hideActions
        table =
          Table
            { config = def{Table.elemID = "dashboardsTable", Table.showHeader = not dg.embedded, Table.addPadding = not dg.embedded && not dg.hideActions, Table.renderAsTable = not dg.embedded, Table.bulkActionsInHeader = if noBulkActions then Nothing else Just 0, Table.noSurface = dg.hideActions}
            , columns = tableCols
            , rows = dg.dashboards
            , features =
                def
                  { Table.rowId = if noBulkActions then Nothing else Just \dash -> dash.id.toText
                  , Table.rowAttrs = Just $ const [class_ "group/row hover:bg-fillWeaker"]
                  , Table.bulkActions =
                      if noBulkActions
                        then []
                        else
                          [ Table.BulkAction{icon = Just "plus", title = "Add teams", uri = "/p/" <> dg.projectId.toText <> "/dashboards/bulk_action/add_teams"}
                          , Table.BulkAction{icon = Just "trash", title = "Delete", uri = "/p/" <> dg.projectId.toText <> "/dashboards/bulk_action/delete"}
                          ]
                  , Table.search = if dg.embedded || dg.hideActions then Nothing else Just Table.ClientSide
                  , Table.tableHeaderActions = dg.tableActions
                  , Table.header = if dg.embedded || null dg.filters.tag then Nothing else Just $ activeFilters_ dg.projectId baseUrl dg.filters
                  , Table.zeroState = if dg.embedded then Nothing else Just Table.ZeroState{icon = "chart-area", title = "No dashboards yet", description = "Create your first dashboard to visualize your data", actionText = "Create Dashboard", destination = Left "newDashboardMdl"}
                  }
            }

    div_ [class_ "w-full", id_ "dashboardsTableContainer"] do
      toHtml table


activeFilters_ :: Projects.ProjectId -> Text -> DashboardFilters -> Html ()
activeFilters_ pid baseUrl filters = div_ [class_ "flex items-center gap-2 mb-4"] do
  let basePath = "/p/" <> pid.toText <> "/dashboards"
      -- Remove a specific tag from the URL
      removeTag tag = T.replace ("&tag=" <> toUriStr tag) "" baseUrl
  span_ [class_ "text-sm text-textWeak"] "Filtered by:"
  forM_ filters.tag \tag ->
    span_ [class_ "badge badge-sm badge-primary gap-1"] do
      toHtml tag
      a_
        [ class_ "cursor-pointer"
        , hxGet_ $ removeTag tag
        , hxTarget_ "#dashboardsTableContainer"
        , hxSelect_ "#dashboardsTableContainer"
        , hxPushUrl_ "true"
        , hxSwap_ "outerHTML"
        ]
        $ faSprite_ "xmark" "regular" "w-3 h-3"
  a_
    [ class_ "text-xs text-textBrand hover:underline cursor-pointer"
    , hxGet_ basePath
    , hxTarget_ "#dashboardsTableContainer"
    , hxSelect_ "#dashboardsTableContainer"
    , hxPushUrl_ "true"
    , hxSwap_ "outerHTML"
    ]
    "Clear all"


dashboardsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UUID.UUID -> DashboardFilters -> ATAuthCtx (RespHeaders DashboardsGet)
dashboardsGetH pid sortM embeddedM teamIdM filters = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext

  -- Sort and filter configuration
  let currentSort = fromMaybe "-updated_at" sortM
      orderByClause = Table.sortFieldsToSQL $ Table.parseSortParam currentSort Nothing
      basePath = "/p/" <> pid.toText <> "/dashboards"
      -- Build URL with current state (sort + existing filters) for proper multi-select
      currentParams = "?sort=" <> toUriStr currentSort <> foldMap (\t -> "&tag=" <> toUriStr t) filters.tag
      baseUrl = basePath <> currentParams

  dashboards' <- case teamIdM of
    Just teamId -> V.fromList <$> Dashboards.selectDashboardsByTeam pid teamId
    Nothing -> V.fromList <$> Dashboards.selectDashboardsSortedBy pid orderByClause

  -- Collect all available tags from all dashboards (before filtering)
  let availableTags = L.nub $ concatMap (V.toList . (.tags)) (V.toList dashboards')

  -- Apply tag filtering
  let dashboards = if null filters.tag then dashboards' else V.filter (\d -> any (`elem` filters.tag) (V.toList d.tags)) dashboards'

  teams <- V.fromList <$> ManageMembers.getTeams pid

  -- Check if we're requesting in embedded mode (for modals, etc.)
  let embedded = embeddedM == Just "true" || embeddedM == Just "1" || embeddedM == Just "yes"
      isTeamView = isJust teamIdM

  if embedded || isTeamView
    then -- For embedded/team mode, use a minimal BWConfig that will still work with ToHtml instance
      addRespHeaders $ DashboardsGetSlim DashboardsGetD{dashboards, projectId = pid, embedded, hideActions = isTeamView, teams, tableActions = Nothing, filters, availableTags}
    else do
      freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = Just project
              , pageTitle = "Dashboards"
              , freeTierExceeded = freeTierExceeded
              , config = appCtx.config
              , pageActions = Just $ label_ [Lucid.for_ "newDashboardMdl", class_ "btn btn-sm btn-primary gap-2"] do
                  faSprite_ "plus" "regular" "h-4 w-4"
                  "New Dashboard"
              }
          tagFilterMenu = Table.FilterMenu{label = "Tags", paramName = "tag", multiSelect = True, options = map (\t -> Table.FilterOption{label = t, value = t, isActive = t `elem` filters.tag}) availableTags}
          tableActions =
            Just
              Table.TableHeaderActions
                { baseUrl
                , targetId = "dashboardsTableContainer"
                , sortOptions = [("Newest", "Most recently modified", "-updated_at"), ("Oldest", "Least recently modified", "+updated_at"), ("Name (A-Z)", "Sort alphabetically", "+title"), ("Name (Z-A)", "Sort reverse alphabetically", "-title")]
                , currentSort
                , filterMenus = [tagFilterMenu | not (null availableTags)]
                , activeFilters = [("Tags", filters.tag) | not (null filters.tag)]
                }
      addRespHeaders $ DashboardsGet (PageCtx bwconf $ DashboardsGetD{dashboards, projectId = pid, embedded = False, hideActions = False, teams, tableActions, filters, availableTags})


data DashboardRes = DashboardNoContent | DashboardPostError Text
  deriving (Generic, Show)


instance ToHtml DashboardRes where
  toHtml DashboardNoContent = ""
  toHtml (DashboardPostError msg) = div_ [class_ "text-textError"] $ toHtml msg
  toHtmlRaw = toHtml


data DashboardForm = DashboardForm
  { file :: Text
  , teams :: [UUID.UUID]
  , title :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


dashboardsPostH :: Projects.ProjectId -> DashboardForm -> ATAuthCtx (RespHeaders DashboardRes)
dashboardsPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  did <- UUIDId <$> UUID.genUUID
  if form.title == ""
    then do
      addErrorToast "Dashboard title is required" Nothing
      addRespHeaders $ DashboardPostError "Dashboard title is required"
    else do
      let dashM = find (\dashboard -> dashboard.file == Just form.file) dashboardTemplates
      let redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> did.toText
      let dbd =
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
              , tags = V.fromList $ fold $ dashM >>= (.tags)
              , title = form.title
              , teams = V.fromList form.teams
              , filePath = Nothing
              , fileSha = Nothing
              }
      _ <- Dashboards.insert dbd
      syncDashboardAndQueuePush pid dbd.id
      redirectCS redirectURI
      addRespHeaders DashboardNoContent


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
      shouldBeStarred = baseTemplate `elem` ["_overview.yaml", "endpoint-stats.yaml"]
      newDashboard = do
        did <- UUIDId <$> UUID.genUUID
        _ <-
          Dashboards.insert
            Dashboards.DashboardVM
              { id = did
              , projectId = pid
              , createdAt = now
              , updatedAt = now
              , createdBy = sess.user.id
              , baseTemplate = Just baseTemplate
              , schema = Nothing
              , starredSince = if shouldBeStarred then Just now else Nothing
              , homepageSince = Nothing
              , tags = V.fromList tags
              , title = title
              , teams = V.empty
              , filePath = Nothing
              , fileSha = Nothing
              }
        syncDashboardAndQueuePush pid did
        pure did.toText
  redirectTo <-
    if project.paymentPlan == "ONBOARDING"
      then pure $ mkPath "/onboarding" ""
      else mkPath "/dashboards/" <$> (maybe newDashboard (pure . (.toText)) =<< Dashboards.getDashboardByBaseTemplate pid baseTemplate)
  pure $ addHeader redirectTo NoContent


-- | Convert a list of query parameters into a percent-encoded query string.
-- For example, [("key", Just "value"), ("empty", Nothing)] becomes "key=value&empty".
toQueryParams :: [(Text, Maybe Text)] -> Text
toQueryParams qs =
  decodeUtf8
    $ URI.renderQuery False
    $ map (bimap encodeUtf8 (fmap encodeUtf8)) qs


-- | Form data for renaming a dashboard
newtype DashboardRenameForm = DashboardRenameForm
  { title :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Handler for renaming a dashboard.
-- It updates the title of the specified dashboard.
dashboardRenamePatchH :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardRenameForm -> ATAuthCtx (RespHeaders DashboardRes)
dashboardRenamePatchH pid dashId form = do
  mDashboard <- Dashboards.getDashboardById dashId
  case mDashboard of
    Nothing -> do
      addErrorToast "Dashboard not found or does not belong to this project" Nothing
      addRespHeaders $ DashboardPostError "Dashboard not found or does not belong to this project"
    Just dashVM -> do
      _ <- Dashboards.updateTitle dashId form.title

      whenJust dashVM.schema \_ ->
        void $ Dashboards.updateSchema dashId (fromMaybe def dashVM.schema & #title ?~ form.title)

      syncDashboardAndQueuePush pid dashId
      addSuccessToast "Dashboard renamed successfully" Nothing
      addTriggerEvent "closeModal" ""
      addRespHeaders DashboardNoContent


-- | Handler for duplicating a dashboard.
-- It creates a new dashboard with the same content but with "(Copy)" appended to the title.
dashboardDuplicatePostH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders DashboardRes)
dashboardDuplicatePostH pid dashId = do
  mDashboard <- Dashboards.getDashboardById dashId
  case mDashboard of
    Nothing -> do
      addErrorToast "Dashboard not found or does not belong to this project" Nothing
      addRespHeaders $ DashboardPostError "Dashboard not found or does not belong to this project"
    Just dashVM -> do
      (sess, _) <- Sessions.sessionAndProject pid
      now <- Time.currentTime
      newDashId <- UUIDId <$> UUID.genUUID

      let copyTitle = if dashVM.title == "" then "Untitled (Copy)" else dashVM.title <> " (Copy)"
          updatedSchema = dashVM.schema & _Just . #title %~ fmap (<> " (Copy)") . (<|> Just "Untitled")

      _ <-
        Dashboards.insert
          $ dashVM
            { Dashboards.id = newDashId
            , Dashboards.createdAt = now
            , Dashboards.updatedAt = now
            , Dashboards.createdBy = sess.user.id
            , Dashboards.title = copyTitle
            , Dashboards.schema = updatedSchema
            , Dashboards.starredSince = Nothing
            , Dashboards.homepageSince = Nothing
            }
      syncDashboardAndQueuePush pid newDashId

      -- Redirect to the new dashboard
      let redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> newDashId.toText
      redirectCS redirectURI
      addSuccessToast "Dashboard was duplicated successfully" Nothing
      addRespHeaders DashboardNoContent


dashboardStarPostH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders (Html ()))
dashboardStarPostH pid dashId = do
  _ <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  mDashboard <- Dashboards.getDashboardById dashId
  case mDashboard of
    Nothing -> throwError $ err404{errBody = "Dashboard not found"}
    Just dashVM -> do
      let newStarredSince = if isJust dashVM.starredSince then Nothing else Just now
      _ <- Dashboards.updateStarredSince dashId newStarredSince
      let msg = if isJust newStarredSince then "Dashboard starred" else "Dashboard unstarred"
      addSuccessToast msg Nothing
      addRespHeaders $ starButton_ pid dashId (isJust newStarredSince)


-- | Handler for deleting a dashboard.
-- It verifies the dashboard exists and belongs to the project before deletion.
-- After deletion, redirects to the dashboard list page.
dashboardDeleteH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders DashboardRes)
dashboardDeleteH pid dashId = do
  mDashboard <- Dashboards.getDashboardById dashId
  case mDashboard of
    Nothing -> throwError $ err404{errBody = "Dashboard not found or does not belong to this project"}
    Just _ -> do
      _ <- Dashboards.deleteDashboard dashId
      let redirectURI = "/p/" <> pid.toText <> "/dashboards"
      redirectCS redirectURI
      addSuccessToast "Dashboard was deleted successfully" Nothing
      addRespHeaders DashboardNoContent


data DashboardBulkActionForm = DashboardBulkActionForm
  { itemId :: [Dashboards.DashboardId]
  , teamHandles :: [UUID.UUID]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


dashboardBulkActionPostH :: Projects.ProjectId -> Text -> DashboardBulkActionForm -> ATAuthCtx (RespHeaders NoContent)
dashboardBulkActionPostH pid action DashboardBulkActionForm{..} = do
  case action of
    "delete" -> do
      _ <- Dashboards.deleteDashboardsByIds pid $ V.fromList itemId
      addSuccessToast "Selected dashboards were deleted successfully" Nothing
    "add_teams" -> do
      teams <- V.fromList <$> ManageMembers.getTeamsById pid (V.fromList teamHandles)
      if V.length teams /= length teamHandles
        then addErrorToast "Some teams not found or don't belong to this project" Nothing
        else
          Dashboards.addTeamsToDashboards pid (V.fromList itemId) (V.fromList teamHandles) >>= \case
            n | n > 0 -> addSuccessToast "Teams added to selected dashboards successfully" Nothing
            _ -> addErrorToast "No dashboards were updated" Nothing
    _ -> addErrorToast "Invalid action" Nothing
  addRespHeaders NoContent


-- | Form data for moving a widget between dashboards
data WidgetMoveForm = WidgetMoveForm
  { widgetId :: Text
  , sourceDashboardId :: Dashboards.DashboardId
  , targetDashboardId :: Dashboards.DashboardId
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Handler for duplicating a widget within the same dashboard.
-- It creates a copy of the widget with "(Copy)" appended to the title.
-- Returns the duplicated widget that will be converted to HTML automatically.
dashboardDuplicateWidgetPostH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardDuplicateWidgetPostH pid dashId widgetId = do
  (_, dash) <- getDashAndVM dashId Nothing
  let widgetToDuplicateM = find (\w -> (w.id == Just widgetId) || (maybeToMonoid (slugify <$> w.title) == widgetId)) (dash :: Dashboards.Dashboard).widgets
  case widgetToDuplicateM of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just widgetToDuplicate -> do
      newWidgetId <- UUID.genUUID <&> UUID.toText
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

      let updatedDash = (dash :: Dashboards.Dashboard) & #widgets %~ (<> [widgetCopy])
      now <- Time.currentTime
      _ <- Dashboards.updateSchemaAndUpdatedAt dashId updatedDash now
      syncDashboardAndQueuePush pid dashId

      addWidgetJSON $ decodeUtf8 $ fromLazy $ AE.encode widgetCopy
      addSuccessToast "Widget duplicated successfully" Nothing
      addRespHeaders widgetCopy


dashboardWidgetExpandGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders (Html ()))
dashboardWidgetExpandGetH pid dashId widgetId = do
  (_, dash) <- getDashAndVM dashId Nothing
  now <- Time.currentTime
  let widgetToExpandM = find (\w -> (w.id == Just widgetId) || (maybeToMonoid (slugify <$> w.title) == widgetId)) (dash :: Dashboards.Dashboard).widgets

  case widgetToExpandM of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just widgetToExpand -> do
      processedWidget <- processWidget pid now (Nothing, Nothing, Nothing) [] widgetToExpand
      addRespHeaders $ widgetViewerEditor_ pid (Just dashId) Nothing (Just processedWidget) "edit"
