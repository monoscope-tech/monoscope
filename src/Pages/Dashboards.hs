module Pages.Dashboards (
  dashboardGetH,
  dashboardTabGetH,
  dashboardTabContentGetH,
  dashboardTabRenamePatchH,
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
  lazyWidget,
  fetchWidgetData,
  widgetMetrics,
  getDashAndVM,
  findTabBySlug,
  WidgetData,
  resolveDashboardParams,
  dashboardBulkActionPostH,
  TabRenameForm (..),
  TabRenameRes (..),
  -- Widget alerts
  WidgetAlertForm (..),
  widgetAlertUpsertH,
  widgetAlertDeleteH,
  -- SQL debug preview
  widgetSqlPreviewGetH,
  -- YAML schema editing
  YamlForm (..),
  dashboardYamlGetH,
  dashboardYamlPutH,
) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Default
import Data.Effectful.Hasql qualified
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as Wreq
import Data.Generics.Labels ()
import Data.HashMap.Lazy qualified as HM
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (concurrently, pooledForConcurrently)
import Effectful.Error.Static (Error, throwError)
import Effectful.Labeled qualified
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxConfirm_, hxDelete_, hxExt_, hxGet_, hxPatch_, hxPost_, hxPushUrl_, hxPut_, hxSelect_, hxSwapOob_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ProjectMembers qualified as ManageMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation
import Network.HTTP.Types.URI qualified as URI
import Pages.Anomalies qualified as AnomalyList
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Pages.Components (FieldCfg (..), FieldSize (..), ModalCfg (..), formField_, primaryButton_, tagInput_)
import Pages.Components qualified as Components
import Pages.GitSync qualified as GitSyncPage
import Pages.LogExplorer.LogItem (getServiceName)
import Pages.Monitors qualified as Alerts
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..), logQueryBox_, visTypes)
import Pkg.Components.Table (BulkAction (..), Table (..))
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), hashAssetFile)
import Pkg.Parser (QueryComponents (..), SqlQueryCfg (..), constantToKQLList, constantToSQLList, defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
import Pkg.SchemaLearning.Catalog qualified as Catalog
import Relude hiding (ask)
import Servant (NoContent (..), ServerError, err302, err404, errBody, errHeaders)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Config (AuthContext (..), EnvConfig (..))
import System.FilePath.Posix (takeDirectory)
import System.Logging qualified as Log
import System.Tracing (Tracing)
import System.Types
import Text.Slugify (slugify)
import UnliftIO qualified
import UnliftIO.Exception (try)
import Utils
import Web.FormUrlEncoded (FromForm)


-- | Head content for dashboard pages - loads highlight.js and sql-formatter for SQL preview
dashboardHeadContent_ :: Html ()
dashboardHeadContent_ = do
  link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/deps/highlightjs/atom-one-dark.min.css")]
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/highlight.min.js"), defer_ "true"] ("" :: Text)
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/sql.min.js"), defer_ "true"] ("" :: Text)
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/sql-formatter.min.js"), defer_ "true"] ("" :: Text)


folderFromPath :: Maybe Text -> Text
folderFromPath = maybe "" \path -> case takeDirectory (toString path) of
  "." -> ""
  dir -> toText dir <> "/"


-- | Git file path for a dashboard: folder (trailing slash normalized) + generated filename.
dashFilePath :: Text -> Text -> Text
dashFilePath dir title = (if T.null dir || T.last dir == '/' then dir else dir <> "/") <> GitSync.titleToFilePath title


normalizeWidgetId :: Text -> Text
normalizeWidgetId = T.replace "Expanded" ""


dashTitle :: Text -> Text
dashTitle "" = "Untitled"
dashTitle t = t


-- | Sync file_path and file_sha for a dashboard after any update.
-- Only recomputes SHA when the schema content has actually changed.
-- Skips template-based dashboards (schema = Nothing) since they have no custom content to sync.
syncDashboardFileInfo :: (DB es, Time.Time :> es) => Dashboards.DashboardId -> Eff es ()
syncDashboardFileInfo dashId = do
  dashM <- Dashboards.getDashboardById dashId
  forM_ dashM \dash -> when (isJust dash.schema) do
    teams <- ManageMembers.getTeamsById dash.projectId dash.teams
    let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
        filePath = dashFilePath (folderFromPath dash.filePath) dash.title
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
  deriving anyclass (Default, FromForm)


data DashboardGet = DashboardGet Projects.ProjectId Dashboards.DashboardId Dashboards.Dashboard Dashboards.DashboardVM [(Text, Maybe Text)]


instance ToHtml DashboardGet where
  toHtml (DashboardGet pid dashId dash dashVM allParams) = toHtml $ dashboardPage_ pid dashId dash dashVM allParams
  toHtmlRaw = toHtml


dashboardPage_ :: Projects.ProjectId -> Dashboards.DashboardId -> Dashboards.Dashboard -> Dashboards.DashboardVM -> [(Text, Maybe Text)] -> Html ()
dashboardPage_ pid dashId dash dashVM allParams = do
  let pidText = pid.toText
      dashIdText = dashId.toText
      allTabs = fold dash.tabs
      activeTabSlug = join (lookup activeTabSlugKey allParams)
      activeTabInfo = activeTabSlug >>= findTabBySlug allTabs
      activeTabIdx = maybe 0 fst activeTabInfo
      -- Slug used for content/widget-order, falling back to the first tab when unset
      renderTabSlug = dash.tabs *> (activeTabSlug <|> (slugify . (.name) <$> listToMaybe allTabs))
      renderTab = renderTabSlug >>= findTabBySlug allTabs <&> snd
  -- Modal for renaming dashboard
  Components.modal_ "pageTitleModalId" ""
    $ form_
      [ class_ "flex flex-col p-3 gap-3"
      , hxPatch_ ("/p/" <> pidText <> "/dashboards/" <> dashIdText <> "/rename")
      , hxSwap_ "innerHTML"
      , hxTrigger_ "submit"
      , hxTarget_ "#pageTitleText"
      ]
    $ do
      formField_ FieldSm def{value = dashTitle dashVM.title, placeholder = "Insert new title"} "Dashboard Title" "title" False Nothing
      formField_ FieldSm def{value = folderFromPath dashVM.filePath, placeholder = "reports/"} "Folder" "fileDir" False Nothing
      Components.formActionsModal_ "pageTitleModalId" $ button_ [type_ "submit", class_ "btn btn-primary"] "Save"

  -- Modal for renaming tab (only shown for dashboards with tabs)
  when (isJust dash.tabs)
    $ Components.modal_ "tabRenameModalId" ""
    $ form_
      [ class_ "flex flex-col p-3 gap-3"
      , hxPatch_ ("/p/" <> pidText <> "/dashboards/" <> dashIdText <> "/tab/" <> fromMaybe "" activeTabSlug <> "/rename")
      , hxSwap_ "none"
      , hxTrigger_ "submit"
      ]
    $ do
      formField_ FieldSm def{value = maybe "" ((.name) . snd) activeTabInfo, placeholder = "Enter tab name"} "Tab Name" "newName" False Nothing
      Components.formActionsModal_ "tabRenameModalId" $ button_ [type_ "submit", class_ "btn btn-primary"] "Save"

  -- Variable picker modal - auto-opens when required vars are unset (from tab.requires or variable.required)
  whenJust dash.variables \variables ->
    whenJust (findVarToPrompt (snd <$> activeTabInfo) variables) \v -> variablePickerModal_ pid dashId activeTabSlug allParams v False

  -- Render variables and tabs in the same container
  when (isJust dash.variables || isJust dash.tabs) $ div_ [class_ "flex bg-bgRaised backdrop-blur-xs max-md:px-2 px-4 py-1 max-md:py-0.5 gap-4 max-md:gap-2 items-center flex-wrap sticky top-0 z-10"] do
    -- Tabs section (on the left) - now using htmx for lazy loading
    whenJust dash.tabs \tabs -> do
      -- Build query string from current params (excluding internal keys and expand param)
      let queryStr = queryStringFrom $ filter (\(k, _) -> k `notElem` [activeTabSlugKey, "expand"]) allParams
      div_ [role_ "tablist", class_ "tabs tabs-box tabs-outline max-md:flex-nowrap max-md:overflow-x-auto max-md:scrollbar-none max-md:[mask-image:linear-gradient(to_right,black_85%,transparent)]", id_ "dashboard-tabs-container", term "hx-preload:inherited" "mouseover"] do
        forM_ (zip [0 ..] tabs) \(idx, tab) -> do
          let tabUrl = "/p/" <> pidText <> "/dashboards/" <> dashIdText <> "/tab/" <> slugify tab.name <> queryStr
          a_
            [ role_ "tab"
            , href_ tabUrl
            , class_ $ "tab flex items-center gap-2 max-md:whitespace-nowrap" <> memptyIfFalse (idx == activeTabIdx) " tab-active"
            , hxGet_ tabUrl
            , hxTarget_ "#dashboard-tabs-content"
            , hxSelect_ "#dashboard-tabs-content"
            , term "hx-select-oob" "#dashboard-tabs-container:outerMorph"
            , hxSwap_ "innerHTML"
            , hxPushUrl_ "true"
            , [__|on click set my.preloadState to 'DONE'|]
            ]
            do
              whenJust tab.icon \icon -> faSprite_ icon "regular" "w-4 h-4"
              toHtml tab.name

    -- Variables section (pushed to the right, collapsible on mobile)
    whenJust dash.variables \variables -> do
      -- Mobile toggle button
      input_ [type_ "checkbox", class_ "hidden peer/vars", id_ "dash-vars-toggle"]
      label_ [Lucid.for_ "dash-vars-toggle", class_ "md:hidden cursor-pointer text-xs text-textWeak bg-fillWeaker border border-strokeWeak rounded-lg px-2 py-1 flex items-center gap-1 peer-checked/vars:hidden"] do
        faSprite_ "filter" "regular" "w-3 h-3"
        "Filters"
      label_ [Lucid.for_ "dash-vars-toggle", class_ "md:hidden cursor-pointer text-xs text-textWeak bg-fillWeak border border-strokeWeak rounded-lg px-2 py-1 hidden peer-checked/vars:flex items-center gap-1"] do
        faSprite_ "filter" "regular" "w-3 h-3"
        "Hide Filters"
      div_ [class_ $ "max-md:hidden max-md:peer-checked/vars:flex flex gap-2 flex-wrap max-md:w-full max-md:pt-1 " <> if isJust dash.tabs then "ml-auto max-md:ml-0" else ""] do
        forM_ variables \var -> fieldset_ [class_ "border border-strokeStrong bg-fillWeaker p-0 inline-block rounded-lg dash-variable text-sm"] do
          legend_ [class_ "px-1 ml-2 text-xs"] $ toHtml $ fromMaybe var.key var.title <> memptyIfFalse (var.required == Just True) " *"
          let whitelist =
                maybe
                  "[]"
                  ( decodeUtf8
                      . fromLazy
                      . AE.encode
                      . map \opt ->
                        let v = maybeToMonoid (opt !!? 0)
                         in AE.object ["value" AE..= v, "name" AE..= fromMaybe v (opt !!? 1)]
                  )
                  var.options

          input_
            $ [ type_ "text"
              , name_ var.key
              , class_ "dash-variable-input"
              , data_ "tagify" ""
              , data_ "tagify-whitelist" whitelist
              , data_ "tagify-enforce-whitelist" ""
              , data_ "tagify-text-prop" "name"
              , data_ "tagify-query-sql" $ maybeToMonoid $ (.statement) <$> var.sql
              , data_ "tagify-query" $ maybeToMonoid var.query
              , data_ "tagify-reload-on-change" $ maybe "false" (T.toLower . show) var.reloadOnChange
              , value_ $ maybeToMonoid var.value
              ]
            -- Multi vars must carry NO tagify-mode attr (main.ts only sets options.mode
            -- when present); a second data_ attr would be (<>)-merged by Lucid.
            <> memptyIfFalse (var.multi /= Just True) [data_ "tagify-mode" "select"]
  let widgetOrderUrl = "/p/" <> pidText <> "/dashboards/" <> dashIdText <> "/widgets_order" <> maybe "" ("?tab=" <>) renderTabSlug
      constantsJson = decodeUtf8 $ AE.encode $ HM.fromList [(k, fromMaybe "" v) | (k, v) <- allParams, "const-" `T.isPrefixOf` k]

  section_ [class_ "h-full"] $ div_ [class_ "mx-auto mb-20 pt-2 pb-6 max-md:pb-20 max-md:px-2 px-4 gap-3.5 w-full flex flex-col group/pg", id_ "dashboardPage", data_ "constants" constantsJson] do
    -- Only warn when a constant actually ran and returned zero rows (Just []).
    -- result == Nothing means the query was skipped (nothing references it) or
    -- failed (logged separately) — neither is a "no data" condition.
    let emptyConstants = [c.key | c <- fold dash.constants, c.result == Just []]
    unless (null emptyConstants) $ div_ [class_ "alert alert-warning text-sm"] do
      faSprite_ "circle-exclamation" "regular" "w-4 h-4"
      span_ $ toHtml $ "Constants with no data: " <> T.intercalate ", " emptyConstants
    div_ [class_ "dashboard-grid-wrapper relative min-h-[400px]"] do
      dashboardSkeleton_
      case dash.tabs of
        Just tabs ->
          -- Tab system with htmx lazy loading - only render active tab content
          div_ [class_ "dashboard-tabs-container", id_ "dashboard-tabs-content"]
            $ whenJust (tabs !!? activeTabIdx) \activeTab -> do
              tabContentPanel_ pid dashIdText activeTabIdx activeTab.name activeTab.widgets False
              -- Variable picker modal inside content div so it's included in HTMX tab swaps
              whenJust dash.variables \variables ->
                whenJust (findVarToPrompt renderTab variables) \v ->
                  variablePickerModal_ pid dashId renderTabSlug allParams v False
        -- Fall back to old behavior for dashboards without tabs
        Nothing -> do
          let rootWidgets = (dash :: Dashboards.Dashboard).widgets
          div_ [class_ "grid-stack -m-2"] do
            forM_ rootWidgets \w -> toHtml w{Widget._projectId = Just pid}
            when (null rootWidgets) $ label_ [id_ "add_a_widget_label", class_ "grid-stack-item pb-8 cursor-pointer bg-fillBrand-weak border-2 border-strokeBrand-strong border-dashed text-strokeSelected rounded-sm rounded-lg flex flex-col gap-3 items-center justify-center *:right-0!  *:bottom-0! ", term "gs-w" "3", term "gs-h" "2", Lucid.for_ "page-data-drawer"] do
              faSprite_ "plus" "regular" "h-8 w-8"
              span_ "Add a widget"

    -- Hidden form for widget order PATCH via HTMX (tab slug hardcoded in URL)
    widgetOrderTriggerForm_ widgetOrderUrl False

    script_
      [text|
      document.addEventListener('DOMContentLoaded', () => {
        window.interpolateVarTemplates = window.interpolateVarTemplates || function() {};
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

        function initializeGrids() {
          document.querySelectorAll('.dashboard-grid-wrapper').forEach(wrapper => {
            if (!wrapper._skeletonTimeout && !wrapper.classList.contains('dashboard-loaded')) {
              wrapper._skeletonTimeout = setTimeout(() => wrapper.classList.add('dashboard-loaded'), 5000);
            }
          });
          const gridInstances = [];
          document.querySelectorAll('.grid-stack').forEach(gridEl => {
            if (!gridEl.classList.contains('grid-stack-initialized')) {
              const wrapper = gridEl.closest('.dashboard-grid-wrapper');
              try {
                const grid = GridStack.init({
                  column: 12,
                  cellHeight: '5rem',
                  margin: '1rem 0.5rem',
                  handleClass: 'grid-stack-handle',
                  styleInHead: true,
                  float: false,
                  animate: true,
                  columnOpts: {
                    breakpointForWindow: true,
                    breakpoints: [{w: 768, c: 1}],
                    layout: 'list'
                  },
                }, gridEl);
                let lastCol = grid.getColumn();
                if (lastCol === 1) grid.setStatic(true);
                new ResizeObserver(() => {
                  const col = grid.getColumn();
                  if (col !== lastCol) {
                    lastCol = col;
                    grid.setStatic(col === 1);
                  }
                }).observe(gridEl);

                // Track real user interactions only — programmatic .update() calls
                // (e.g. autoFitGroupToChildren on initial mount) emit 'change' but not
                // dragstart/resizestart. Without this gate, every page load saves a
                // template snapshot into the schema column, defeating YAML live-reload.
                grid.on('dragstart resizestart', () => { gridEl._userInteracted = true; });
                grid.on('removed change', debounce(() => {
                  if (grid.getColumn() === 1) return;
                  if (gridEl.offsetParent === null) return;
                  const collapsingWidget = gridEl.querySelector('[data-collapse-action]');
                  if (collapsingWidget) { delete collapsingWidget.dataset.collapseAction; return; }
                  if (!gridEl._userInteracted) return;
                  htmx.trigger(document.body, 'widget-order-changed');
                }, 500));
                // 'removed' fires when a widget is deleted via DOM removal — that's a real edit.
                grid.on('removed', () => { gridEl._userInteracted = true; });
                gridEl.classList.add('grid-stack-initialized');
                gridInstances.push(grid);
                window.gridStackInstance = grid;
              } finally {
                if (wrapper) {
                  wrapper.classList.add('dashboard-loaded');
                  if (wrapper._skeletonTimeout) clearTimeout(wrapper._skeletonTimeout);
                }
                window.interpolateVarTemplates();
              }
            }
          });

          // Initialize nested grids
          document.querySelectorAll('.nested-grid').forEach(nestedEl => {
            if (!nestedEl.classList.contains('grid-stack-initialized')) {
              const parentWidget = nestedEl.closest('.grid-stack-item');
              // Store original YAML height for partial-width groups
              if (parentWidget) {
                parentWidget.dataset.originalH = parentWidget.getAttribute('gs-h') || '0';
              }

              const nestedInstance = GridStack.init({
                column: 12,
                acceptWidgets: true,
                cellHeight: '5rem',
                margin: '1rem 0.5rem',
                handleClass: 'nested-grid-stack-handle',
                styleInHead: true,
                animate: true,
                columnOpts: {
                  breakpointForWindow: true,
                  breakpoints: [{w: 768, c: 1}],
                  layout: 'list'
                },
              }, nestedEl);

              // Auto-fit group to children
              function autoFitGroupToChildren() {
                const items = nestedInstance.getGridItems();
                const node = parentWidget?.gridstackNode;
                if (!node) return;

                // Don't resize if group is collapsed
                if (parentWidget.classList.contains('collapsed')) return;

                const isFullWidth = node.w === 12;
                const maxRow = items.length
                  ? Math.max(1, ...items.map(item => (item.gridstackNode?.y || 0) + (item.gridstackNode?.h || 1)))
                  : 1;

                const requiredHeight = 1 + maxRow;  // 1 for header + content
                const yamlHeight = parseInt(parentWidget.dataset.originalH) || requiredHeight;

                // Full-width: always auto-fit. Partial-width: max of YAML and required
                const targetHeight = isFullWidth ? requiredHeight : Math.max(yamlHeight, requiredHeight);

                if (node.h !== targetHeight && window.gridStackInstance) {
                  window.gridStackInstance.update(parentWidget, { h: targetHeight });
                }
              }

              nestedInstance.on('change added removed', autoFitGroupToChildren);
              requestAnimationFrame(autoFitGroupToChildren);
              nestedInstance.on('dragstart resizestart removed', () => { nestedEl._userInteracted = true; });
              nestedInstance.on('removed change', debounce(() => {
                if (window.innerWidth < 768) return;
                if (nestedEl.offsetParent === null) return;
                const collapsingWidget = nestedEl.closest('[data-collapse-action]');
                if (collapsingWidget) { delete collapsingWidget.dataset.collapseAction; return; }
                if (!nestedEl._userInteracted) return;
                htmx.trigger(document.body, 'widget-order-changed');
              }, 500));

              nestedEl.classList.add('grid-stack-initialized');
            }
          });
        }

        // Initialize grids on page load (wait for deferred GridStack)
        function waitForGridStack() {
          if (typeof GridStack === 'undefined') { setTimeout(waitForGridStack, 50); return; }
          initializeGrids();
        }
        waitForGridStack();

        // Re-initialize grids after htmx settles new tab content
        document.body.addEventListener('htmx:after:swap', function(e) {
          if (e.detail.target && e.detail.target.id === 'dashboard-tabs-content') {
            initializeGrids();
            window.interpolateVarTemplates();
          }
        });

        // Auto-expand widget if expand param is in URL
        const expandWId = new URLSearchParams(window.location.search).get('expand');
        if (expandWId) {
          const drawer = document.getElementById('global-data-drawer');
          const drawerContent = document.getElementById('global-data-drawer-content');
          const loaderTmp = document.getElementById('loader-tmp');
          if (drawer && drawerContent) {
            drawer.checked = true;
            document.body.classList.add('overflow-hidden');
            if (loaderTmp) drawerContent.innerHTML = loaderTmp.innerHTML;
            fetch('/p/$pidText/dashboards/$dashIdText/widgets/' + expandWId + '/expand')
              .then(r => r.text())
              .then(html => {
                drawerContent.innerHTML = html;
                htmx.process(drawerContent);
                if (typeof _hyperscript !== 'undefined') _hyperscript.processNode(drawerContent);
                if (typeof window.evalScriptsFromContent === 'function') window.evalScriptsFromContent(drawerContent);
              });
          }
        }
      });

      // Listen for widget-remove-requested custom events
      document.addEventListener('widget-remove-requested', function(e) {
        const widgetEl = document.getElementById(e.detail.widgetId + '_widgetEl');
        if (widgetEl) {
          const gridEl = widgetEl.closest('.grid-stack');
          if (gridEl && gridEl.gridstack) {
            gridEl.gridstack.removeWidget(widgetEl, true);
          }
        }
      });

      function compactGrid(grid, el) {
        if (!el) return;
        const items = Array.from(el.querySelectorAll(':scope > .grid-stack-item')).sort((a, b) => (a.gridstackNode?.y || 0) - (b.gridstackNode?.y || 0));
        const rows = {};
        items.forEach(item => { const y = item.gridstackNode?.y || 0; (rows[y] = rows[y] || []).push(item); });
        let nextY = 0, needsUpdate = false;
        const updates = [];
        Object.keys(rows).map(Number).sort((a, b) => a - b).forEach(y => {
          rows[y].forEach(item => {
            if (item.gridstackNode?.y !== nextY) { updates.push({ item, y: nextY }); needsUpdate = true; }
          });
          nextY += Math.max(...rows[y].map(item => item.gridstackNode?.h || 1));
        });
        if (needsUpdate) {
          grid.batchUpdate();
          updates.forEach(({ item, y }) => grid.update(item, { y }));
          grid.batchUpdate(false);
        }
      }

      // Delegated handler for collapse toggle
      document.addEventListener('click', function(e) {
        const collapseBtn = e.target.closest('.collapse-toggle');
        if (!collapseBtn) return;
        const parentWidget = collapseBtn.closest('.grid-stack-item');
        const grid = window.gridStackInstance;
        if (!parentWidget || !grid) return;

        // Use requestAnimationFrame for smoother animation after class toggle
        requestAnimationFrame(() => {
          const isCollapsed = parentWidget.classList.contains('collapsed');
          const mainGridEl = document.querySelector('.grid-stack:not(.nested-grid)');

          parentWidget.dataset.collapseAction = 'true';

          if (isCollapsed) {
            grid.update(parentWidget, { h: 1 });
          } else {
            const nestedInstance = parentWidget.querySelector('.nested-grid')?.gridstack;
            const items = nestedInstance?.getGridItems() || [];
            const maxRow = items.length ? Math.max(1, ...items.map(item => (item.gridstackNode?.y || 0) + (item.gridstackNode?.h || 1))) : 1;
            grid.update(parentWidget, { h: 1 + maxRow });
          }
          compactGrid(grid, mainGridEl);
        });
      });
      |]


widgetOrderTriggerForm_ :: Text -> Bool -> Html ()
widgetOrderTriggerForm_ url isOob =
  form_
    ( [ id_ "widget-order-trigger"
      , class_ "hidden"
      , hxPatch_ url
      , hxVals_ "js:{...buildWidgetOrder(getActiveGrid())}"
      , hxExt_ "json-enc"
      , hxSwap_ "none"
      , hxTrigger_ "widget-order-changed from:body"
      ]
        <> [hxSwapOob_ "true" | isOob]
    )
    ""


loadDashboardFromVM :: [Dashboards.Dashboard] -> Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM templates dashVM = dashVM.schema <|> find (\d -> d.file == dashVM.baseTemplate) templates


-- | Flatten a secured-query result set into plain text rows.
queryRowsToText :: V.Vector (V.Vector AE.Value) -> [[Text]]
queryRowsToText = map (map valueToText . V.toList) . V.toList
  where
    valueToText = \case
      AE.String t -> t
      AE.Number n -> show n
      AE.Bool b -> bool "false" "true" b
      AE.Null -> ""
      v -> decodeUtf8 $ AE.encode v


-- Process a single dashboard variable recursively.
processVariable :: WidgetData es => Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Dashboards.Variable -> Eff es Dashboards.Variable
processVariable pid now (sinceStr, fromDStr, toDStr) allParams variableBase = do
  let (fromD, toD, _) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)
      paramsMap = Map.fromList allParams
      variable' = Dashboards.replaceQueryVariables pid fromD toD allParams now variableBase
      variable = variable'{Dashboards.value = join (Map.lookup ("var-" <> variable'.key) paramsMap) <|> variable'.value}

  -- Prefer the precomputed facet catalog (a single indexed jsonb read) over a
  -- live DISTINCT scan of the day partition — the latter blocks page render for
  -- seconds on high-volume projects (e.g. the databases-tab picker).
  facetOpts <- case variable.facetField of
    Just field -> do
      docM <- SchemaCatalog.getSummary pid
      pure $ docM >>= \(d :: Catalog.SummaryDoc) -> HM.lookup field d.topValuesByField <&> \tk -> map (one . fst) $ sortWith (Down . snd) $ HM.toList tk.top
    Nothing -> pure Nothing
  case facetOpts of
    Just opts | not (null opts) -> pure variable{Dashboards.options = Just opts}
    -- Dependent variables (depends_on) are lazy: their options are scoped to the
    -- parent's value and can't be facet-served, so we skip the (multi-second)
    -- DISTINCT scan at render time. The client fetches the whitelist on demand
    -- (dropdown open / update-query) via /chart_data, keeping the scoped query
    -- off the page's critical path.
    _ | isJust variable.dependsOn -> pure variable
    _ -> case variable._vType of
      Dashboards.VTQuery | Just sqlQuery <- variable.sql -> do
        -- SECURITY: Use secured query execution with project_id filtering
        useTf <- (.env.enableTimefusionReads) <$> ask @AuthContext
        -- Budgeted for the same reason as the facet/dependent short-circuits above:
        -- an option list is never worth blocking the shell on.
        withRenderBudget ("variable:" <> variable.key) variable
          $ LogQueries.executeSecuredQuery useTf pid sqlQuery 1000
          <&> \case
            Right queryResults -> variable{Dashboards.options = Just $ queryRowsToText queryResults}
            Left _ -> variable -- Return unchanged on error
      _ -> pure variable


-- | Process all dashboard variables concurrently.
processVariablesConcurrently :: WidgetData es => Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Dashboards.Dashboard -> Eff es Dashboards.Dashboard
processVariablesConcurrently pid now timeParams allParams dash =
  forOf (#variables . _Just) dash $ flip pooledForConcurrently (processVariable pid now timeParams allParams)


-- | Find variable that needs to be prompted (from tab.requires or variable.required,
-- the latter only once its @dependsOn@ parent has a value).
findVarToPrompt :: Maybe Dashboards.Tab -> [Dashboards.Variable] -> Maybe Dashboards.Variable
findVarToPrompt activeTab variables =
  (activeTab >>= (.requires) >>= \reqKey -> find (\v -> v.key == reqKey && isNothing v.value) variables)
    <|> find shouldPrompt variables
  where
    setKeys = [v.key | v <- variables, isJust v.value]
    shouldPrompt v = v.required == Just True && isNothing v.value && maybe True (`elem` setKeys) v.dependsOn


-- | Render inline variable picker (command-palette style selector)
variablePickerModal_ :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> [(Text, Maybe Text)] -> Dashboards.Variable -> Bool -> Html ()
variablePickerModal_ pid dashId activeTabSlug allParams var useOob = do
  let varTitle = fromMaybe var.key var.title
      varKey = "var-" <> var.key
      queryBase = queryStringFrom $ filter (\(k, _) -> k /= varKey && k /= activeTabSlugKey) allParams
      tabPath = maybe "" ("/tab/" <>) activeTabSlug
      urlPrefix = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> tabPath <> queryBase <> (if T.null queryBase then "?" else "&") <> varKey <> "="
      oobAttr = if useOob then [id_ $ "varPicker-" <> var.key <> "-container", hxSwapOob_ "beforeend:body"] else []
      opts = fold var.options
      optCount = length opts
  div_ oobAttr do
    div_ [class_ "var-picker-backdrop fixed inset-0 flex flex-col items-center pt-[15vh] bg-black/40", style_ "z-index:99999", [__|on click if event.target is me remove me|]] do
      div_ [class_ "w-full max-w-lg flex items-center justify-between mb-2 px-3"] do
        span_ [class_ "text-2xs font-medium text-white dark:text-white/70 uppercase tracking-wider"] $ toHtml $ "Select " <> varTitle
        span_ [class_ "var-picker-count text-xs text-white/80 dark:text-white/50", data_ "total" (show optCount)] $ toHtml $ show optCount <> " items"
      div_ [class_ "var-picker w-full max-w-lg bg-base-100 rounded-lg shadow-2xl border border-base-300 overflow-hidden", [__|on click halt the event's bubbling|]] do
        div_ [class_ "px-3 border-b border-base-300"] do
          input_
            [ type_ "text"
            , class_ "w-full py-2.5 bg-transparent outline-none text-sm"
            , placeholder_ $ "Search " <> T.toLower varTitle <> "s..."
            , autofocus_
            , [__|on input
                set :q to my value.toLowerCase()
                show <.var-opt/> in closest .var-picker when its textContent.toLowerCase() contains :q
                for opt in <a.var-opt/> in closest .var-picker remove .active from opt end
                set :first to the first <a.var-opt:not([style*='display: none'])/> in closest .var-picker
                if :first then add .active to :first end
                set :visible to the <a.var-opt:not([style*='display: none'])/> in closest .var-picker
                set :empty to the first <.var-picker-empty/> in closest .var-picker
                set :counter to the first <.var-picker-count/> in closest .var-picker
                if :visible.length === 0 then show :empty else hide :empty end
                set :total to :counter.dataset.total
                if :q.length > 0 then put `${:visible.length} of ${:total} items` into :counter
                else put `${:total} items` into :counter end
              end
              on keydown[key=='Escape']
                set :bd to closest .var-picker-backdrop
                remove :bd
              end
              on keydown[key=='Enter']
                set :a to the first <a.active/> in closest .var-picker
                if :a then call :a.click() end
              end
              on keydown[key=='ArrowDown'] halt the event
                set :a to the first <a.active/> in closest .var-picker
                if :a then
                  set :n to :a.nextElementSibling
                  repeat while :n and :n.style.display === 'none' set :n to :n.nextElementSibling end
                  if :n then remove .active from :a then add .active to :n then call :n.scrollIntoView({block:'nearest'}) end
                else
                  set :f to the first <a.var-opt:not([style*='display: none'])/> in closest .var-picker
                  if :f then add .active to :f end
                end
              end
              on keydown[key=='ArrowUp'] halt the event
                set :a to the first <a.active/> in closest .var-picker
                if :a then
                  set :p to :a.previousElementSibling
                  repeat while :p and :p.style.display === 'none' set :p to :p.previousElementSibling end
                  if :p then remove .active from :a then add .active to :p then call :p.scrollIntoView({block:'nearest'}) end
                end
              |]
            ]
        div_ [class_ "max-h-80 overflow-y-auto p-1"] do
          forM_ (zip [0 :: Int ..] opts) \(idx, opt) -> do
            let optVal = maybeToMonoid (opt !!? 0)
                optLbl = fromMaybe optVal (opt !!? 1)
                isCurrent = var.value == Just optVal
            a_
              [ class_
                  $ "var-opt flex items-center gap-2 px-3 py-2 rounded text-sm cursor-pointer transition-colors"
                  <> bool "" " active" (idx == 0)
                  <> bool "" " var-opt-current" isCurrent
              , href_ $ urlPrefix <> optVal
              ]
              do
                span_ [class_ "truncate flex-1"] $ toHtml optLbl
                when isCurrent $ faSprite_ "check" "regular" "w-3 h-3 text-primary shrink-0"
          div_ [class_ "var-picker-empty px-3 py-8 text-center text-sm text-base-content/40", style_ "display:none"] "No matching results"
      -- Keyboard hints
      div_ [class_ "var-picker-hints flex items-center gap-6 mt-3 text-xs text-white dark:text-white/80 drop-shadow"]
        $ forM_ ([("Navigate", ["\x2191", "\x2193"]), ("Select", ["\x21B5"]), ("Close", ["esc"])] :: [(Text, [Text])]) \(label, keys) ->
          div_ [class_ "flex items-center gap-1.5"] do
            toHtml label
            forM_ keys $ kbd_ [class_ "kbd kbd-xs"] . toHtml


-- | Wall-clock budget for a single TimeFusion-backed query run *during page render*
-- (a constant, a variable's option list, an eager widget prefill). These all used to
-- be unbounded, so a slow or wedged TF held the dashboard shell hostage: on
-- 2026-08-02 every render thread parked on TF, the three replicas grew to their 24GB
-- cap and Swarm OOM-killed them in a loop while /status timed out.
--
-- The prefill is an optimisation, never a requirement — every widget kind can fetch
-- its own data client-side (see 'withRenderBudget'). So we cap the wait and ship the
-- shell; a blown budget costs a spinner, not a page.
--
-- This is per query, and render runs three sequential phases (constants, then
-- variables, then widgets), each internally concurrent — so a total TF stall bounds
-- the shell at ~3x this, not 1x. Healthy TF is unaffected: these queries return in
-- milliseconds and the timeout never fires.
renderQueryBudgetMicros :: Int
renderQueryBudgetMicros = 4_000_000


-- | Run a render-time query under 'renderQueryBudgetMicros', falling back to
-- @fallback@ when the budget is blown.
--
-- Every caller's fallback leaves the widget/variable/constant in its *un-prefilled*
-- state, which is exactly the state the client already knows how to recover from:
-- tables render their spinner and fire @hx-trigger=\"load\"@, stats include @load@ in
-- their trigger whenever they have no data, and charts show \"Loading chart…\" and
-- fetch on intersection when @dataset.source@ is empty. So a slow TF degrades the
-- dashboard from server-rendered to client-fetched rather than from working to down.
--
-- Abandoning the query is safe: resource-pool destroys a connection whose borrower
-- died, so we drop the connection rather than return a half-read one to the pool.
withRenderBudget :: (IOE :> es, Log :> es) => Text -> a -> Eff es a -> Eff es a
withRenderBudget label fallback action =
  UnliftIO.timeout renderQueryBudgetMicros action >>= \case
    Just a -> pure a
    Nothing ->
      Log.logWarn "Dashboard render query exceeded budget; degrading to client-side fetch" label $> fallback


-- | Process a single dashboard constant by executing its SQL or KQL query and populating the result.
-- Constants are executed once and their results are made available to all widgets.
processConstant :: forall es. WidgetData es => Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Dashboards.Constant -> Eff es Dashboards.Constant
processConstant pid now (sinceStr, fromDStr, toDStr) allParams constantBase = do
  let (fromD, toD, _) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)
      constant = Dashboards.replaceConstantVariables pid fromD toD allParams now constantBase
      runQuery :: forall a. Text -> Eff es (Either Text a) -> (a -> [[Text]]) -> Eff es Dashboards.Constant
      runQuery label action toResult = do
        (res, duration) <- Log.timeAction action
        either
          (\err -> Log.logWarn ("Dashboard constant " <> label <> " query failed") (constant.key, err, duration) $> constant)
          (\val -> Log.logDebug ("Dashboard constant " <> label <> " query completed") (constant.key, duration) $> constant{Dashboards.result = Just $ toResult val})
          res
  useTf <- (.env.enableTimefusionReads) <$> ask @AuthContext
  case (constant.sql, constant.query) of
    -- SECURITY: Use secured query execution with project_id filtering
    (Just sqlQuery, _) -> runQuery "SQL" (LogQueries.executeSecuredQuery useTf pid sqlQuery 1000) queryRowsToText
    (Nothing, Just kqlQuery) ->
      runQuery
        "KQL"
        (first (\(e :: SomeException) -> show e) <$> try (Charts.queryMetrics Nothing (Just Charts.DTText) (Just pid) (Just kqlQuery) Nothing sinceStr fromDStr toDStr Nothing allParams))
        (map V.toList . V.toList . (.dataText))
    _ -> pure constant


-- Process a single widget recursively. Keeps sql/query with {{var-*}} templates intact
-- so they can be interpolated at data fetch time with current URL params.
processWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processWidget pid now timeRange allParams widgetBase = do
  let widget = widgetBase & #_projectId %~ (<|> Just pid) & #rawQuery .~ widgetBase.query

  -- The prefill is best-effort: past the budget we hand back the widget with no
  -- html/dataset and no `eager` flag, which is precisely the shape whose renderer
  -- emits a spinner plus a self-fetch.
  widget' <-
    if
      -- Anomalies first, ahead of the `eager` check. They read Postgres rather than
      -- TF, and `widget_` renders `whenJust w.html toHtmlRaw` — nothing at all
      -- without html — so there is no client-side fetch to degrade to and a blown
      -- budget would blank the card. `eager` is a free `Maybe Bool` with no
      -- type-level tie to `wType`, so a custom dashboard can set both; ordering the
      -- guard this way makes the exclusion hold for every input, not just the
      -- built-in templates.
      | widget.wType == Widget.WTAnomalies -> processEagerWidget pid now timeRange allParams widget
      -- Label by id, not title: untitled widgets would all log the same string.
      | widget.eager == Just True ->
          withRenderBudget ("widget:" <> maybeToMonoid widget.id) (lazyWidget widget)
            $ processEagerWidget pid now timeRange allParams widget
      | otherwise -> pure widget

  -- Recursively process child widgets concurrently, inheriting the parent's dashboard id
  forOf (#children . _Just) widget' \kids ->
    pooledForConcurrently kids $ processWidget pid now timeRange allParams . (#_dashboardId %~ (<|> widget'._dashboardId))


-- | Strip every trace of a server-side prefill so the widget renders as if it had
-- never been eager. @eager@ must go too: 'Widget.renderStatContent' treats the flag
-- itself as \"data is present\" and drops @load@ from its HTMX trigger, so a widget
-- left flagged-but-empty would render a spinner that never resolves.
lazyWidget :: Widget.Widget -> Widget.Widget
lazyWidget w = w & #eager .~ Nothing & #html .~ Nothing & #dataset .~ Nothing


-- | Everything the data-fetching half of the dashboard pipeline needs. Spelled
-- as a constraint row rather than 'ATAuthCtx' so the same code serves the HTML
-- handlers, the PNG export and the @/api/v1@ JSON endpoint the CLI renders from
-- (none of which have a session).
type WidgetData es = (Concurrent :> es, DB es, Effectful.Labeled.Labeled "timefusion" Data.Effectful.Hasql.Hasql :> es, Effectful.Reader.Static.Reader AuthContext :> es, Error ServerError :> es, IOE :> es, Log :> es, Time.Time :> es, Tracing :> es)


-- | Run a widget's query in the shape its type decodes — 'Widget.chartQuery'
-- picks both together.
widgetMetrics :: WidgetData es => Projects.ProjectId -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> Eff es Charts.MetricsData
widgetMetrics pid (sinceStr, fromDStr, toDStr) allParams widget =
  Charts.queryMetrics widget.dbSource (Just dataType) (Just pid) query widget.sql sinceStr fromDStr toDStr Nothing allParams
  where
    (query, dataType) = Widget.chartQuery widget


-- | Fetch widget data based on widget type (for stat and chart widgets)
fetchWidgetData :: WidgetData es => Projects.ProjectId -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> Eff es Widget.Widget
fetchWidgetData pid timeRange allParams widget = do
  md <- widgetMetrics pid timeRange allParams widget
  pure $ widget
    & #dataset ?~ case widget.wType of
      Widget.WTStat -> def{Widget.source = AE.Null, Widget.value = md.dataFloat}
      _ -> Widget.toWidgetDataset md


processEagerWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processEagerWidget pid now timeRange@(sinceStr, fromDStr, toDStr) allParams widget = case widget.wType of
  Widget.WTAnomalies -> do
    (issues, _) <- Issues.selectIssues pid (Just False) (Just False) 2 0 Nothing Nothing "24h" [] []
    pure
      $ widget
      & #html
        ?~ renderText
          (div_ [class_ "flex flex-col gap-3 h-full w-full overflow-hidden"] $ forM_ issues $ AnomalyList.issueCardCompact_ pid now)
  Widget.WTTable -> do
    -- Fetch table data
    tableData <- Charts.queryMetrics widget.dbSource (Just Charts.DTText) (Just pid) widget.query widget.sql sinceStr fromDStr toDStr Nothing allParams
    -- Render the table with data server-side
    pure
      $ widget
      & #html
        ?~ renderText (Widget.renderTableWithDataAndParams widget tableData.dataText allParams)
  Widget.WTTraces -> do
    tracesD <- Charts.queryMetrics widget.dbSource (Just Charts.DTText) (Just pid) widget.query widget.sql sinceStr fromDStr toDStr Nothing allParams
    let trIds = V.map V.last tracesD.dataText
    (shapeWithDuration, spanRecords') <-
      concurrently
        (Telemetry.getTraceShapes pid trIds)
        (Telemetry.getSpanRecordsByTraceIds pid trIds Nothing)
    let grouped = HM.fromListWith (++) [(trId, [(spanName, duration, events)]) | (trId, spanName, duration, events) <- shapeWithDuration]
        spanRecords = V.fromList $ mapMaybe Telemetry.convertOtelLogsAndSpansToSpanRecord spanRecords'
        serviceColors = getServiceColors ((\x -> getServiceName x.resource) <$> spanRecords)
        colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList serviceColors]
        spansGrouped = HM.fromListWith (++) [(sp.traceId, [sp]) | sp <- V.toList spanRecords]

    pure
      $ widget
      & #html
        ?~ renderText (Widget.renderTraceDataTable widget tracesD.dataText grouped spansGrouped colorsJson)
  _ -> fetchWidgetData pid timeRange allParams widget


-- | Populate widgets with their alert statuses
populateWidgetAlertStatuses :: DB es => [Widget.Widget] -> Eff es [Widget.Widget]
populateWidgetAlertStatuses widgets = do
  let widgetIds = V.fromList $ mapMaybe (.id) widgets
  if V.null widgetIds
    then pure widgets
    else do
      statuses <- Monitors.getWidgetAlertStatuses widgetIds
      let statusMap = foldMap (\s -> one (s.widgetId, s)) statuses
      pure $ map (applyAlertStatus statusMap) widgets
  where
    applyAlertStatus statusMap w = fromMaybe w do
      status <- (.id) w >>= (`Map.lookup` statusMap)
      pure
        w
          { Widget.alertId = Just $ Monitors.unQueryMonitorId status.monitorId & UUID.toText
          , Widget.alertThreshold = Just status.alertThreshold
          , Widget.warningThreshold = status.warningThreshold
          , Widget.alertStatus = Just $ display status.alertStatus
          }


populateWidgetPngUrls :: Log :> es => Text -> Text -> Projects.ProjectId -> (Maybe Text, Maybe Text, Maybe Text) -> [Widget.Widget] -> Eff es [Widget.Widget]
populateWidgetPngUrls secret hostUrl pid (sinceStr, fromDStr, toDStr) = mapM \w -> do
  url <- Widget.widgetPngUrl secret hostUrl pid w sinceStr fromDStr toDStr
  pure $ if T.null url then w else w{Widget.pngUrl = Just url}


dashboardWidgetPutH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Widget.Widget -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardWidgetPutH pid dashId widgetIdM tabSlugM widget = do
  _ <- Projects.sessionAndProject pid
  (_, dash) <- getDashAndVM dashId Nothing
  uid <- UUID.genUUID <&> UUID.toText
  let normalizedWidgetIdM = normalizeWidgetId <$> widgetIdM
      widgetUpdated = normalizeWidget widget normalizedWidgetIdM uid

  _ <- Dashboards.updateSchema dashId $ updateDashboardWidgets dash tabSlugM normalizedWidgetIdM widgetUpdated
  syncDashboardAndQueuePush pid dashId
  whenJust normalizedWidgetIdM \nwid -> syncWidgetAlert pid nwid widget

  addSuccessToast (if isJust normalizedWidgetIdM then "Widget updated successfully" else "Widget added to dashboard successfully") Nothing
  addTriggerEvent "closeModal" ""
  addRespHeaders widgetUpdated


-- | A newly created widget (no id yet) also drops any centered title.
normalizeWidget :: Widget.Widget -> Maybe Text -> Text -> Widget.Widget
normalizeWidget widget normalizedWidgetIdM generatedId =
  widget
    { Widget.standalone = Nothing
    , Widget.naked = Nothing
    , Widget.id = Just $ fromMaybe generatedId normalizedWidgetIdM
    , Widget._centerTitle = normalizedWidgetIdM *> widget._centerTitle
    }


updateDashboardWidgets :: Dashboards.Dashboard -> Maybe Text -> Maybe Text -> Widget.Widget -> Dashboards.Dashboard
updateDashboardWidgets dash tabSlugM normalizedWidgetIdM widgetUpdated =
  overDashWidgets tabSlugM (\ws -> maybe (ws <> [widgetUpdated]) (\nwid -> map (updateWidget nwid) ws) normalizedWidgetIdM) dash
  where
    updateWidget nwid w = if widgetMatches nwid w then mergeWidgetPreservingQuery w widgetUpdated else w


-- | A widget is addressed either by its explicit id or by its slugified title.
widgetMatches :: Text -> Widget.Widget -> Bool
widgetMatches wid w = w.id == Just wid || maybeToMonoid (slugify <$> w.title) == wid


-- | Apply @f@ to the widget list of @tabSlugM@'s tab on a tabbed dashboard, else to the root widgets.
overDashWidgets :: Maybe Text -> ([Widget.Widget] -> [Widget.Widget]) -> Dashboards.Dashboard -> Dashboards.Dashboard
overDashWidgets tabSlugM f dash = case (tabSlugM, dash.tabs) of
  (Just slug, Just _) -> updateTabBySlug slug (#widgets %~ f) dash
  _ -> dash & #widgets %~ f


-- | Merge widgets, preserving the original query/rawQuery if the new widget doesn't have one.
-- This ensures we don't lose the original KQL query when a widget is updated.
mergeWidgetPreservingQuery :: Widget.Widget -> Widget.Widget -> Widget.Widget
mergeWidgetPreservingQuery original updated =
  updated
    & #query %~ (<|> original.query)
    & #rawQuery %~ (<|> original.rawQuery)


syncWidgetAlert :: DB es => Projects.ProjectId -> Text -> Widget.Widget -> Eff es ()
syncWidgetAlert pid widgetId widget = do
  existingMonitor <- Monitors.queryMonitorByWidgetId widgetId
  whenJust existingMonitor \monitor -> do
    let newQuery = fromMaybe "" widget.query
    when (monitor.logQuery /= newQuery) do
      let sqlQueryCfg = (defSqlQueryCfg pid fixedUTCTime Nothing Nothing){presetRollup = Just "5m"}
          newSqlQuery = case parseQueryToComponents sqlQueryCfg newQuery of
            Right (_, qc) -> fromMaybe "" qc.finalAlertQuery
            Left _ -> monitor.logQueryAsSql -- Keep previous SQL on parse failure
          updatedMonitor = (monitor :: Monitors.QueryMonitor){Monitors.logQuery = newQuery, Monitors.logQueryAsSql = newSqlQuery}
      void $ Monitors.queryMonitorUpsert updatedMonitor


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
  -> Maybe Text
  -- ^ Optional tab slug for tabbed dashboards
  -> Map Text WidgetReorderItem
  -- ^ The ordered list of widget IDs
  -> ATAuthCtx (RespHeaders NoContent)
dashboardWidgetReorderPatchH pid dashId tabSlugM widgetOrder = do
  _ <- Projects.sessionAndProject pid
  (_, dash) <- getDashAndVM dashId Nothing

  let oldWidgets = case (tabSlugM, dash.tabs) of
        (Just slug, Just tabs) -> foldMap (.widgets) $ find (\t -> slugify t.name == slug) tabs
        _ -> dash.widgets
      oldWidgetIds = mapMaybe (.id) oldWidgets
      newWidgetIds = Map.keys widgetOrder
      reorderedWidgets = reorderWidgets widgetOrder oldWidgets
      deletedWidgetIds = filter (`notElem` newWidgetIds) oldWidgetIds

  -- Safety: if patch has IDs but none match existing widgets, it's a stale/race request — skip
  if not (null oldWidgets) && not (Map.null widgetOrder) && null reorderedWidgets
    then addRespHeaders NoContent
    else do
      -- Delete alerts for removed widgets first (before updating dashboard to avoid orphaned monitors)
      unless (null deletedWidgetIds) $ void $ Monitors.deleteMonitorsByWidgetIds deletedWidgetIds

      _ <- Dashboards.updateSchema dashId $ overDashWidgets tabSlugM (const reorderedWidgets) dash
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
              $ fromMaybe def orig.layout
              & #x .~ (item.x <|> (orig.layout >>= (.x)))
              & #y .~ (item.y <|> (orig.layout >>= (.y)))
              & #w .~ (item.w <|> (orig.layout >>= (.w)))
              & #h .~ (item.h <|> (orig.layout >>= (.h)))
      pure
        orig
          { Widget.layout = newLayout
          , Widget.children = item.children <&> (`reorderWidgets` fold orig.children)
          }

    mkWidgetMap = Map.fromList . concatMap flatten
    flatten w = (widgetId w, w) : foldMap (concatMap flatten) w.children
    widgetId w = fromMaybe (maybeToMonoid $ slugify <$> w.title) w.id


updateTabBySlug :: Text -> (Dashboards.Tab -> Dashboards.Tab) -> Dashboards.Dashboard -> Dashboards.Dashboard
updateTabBySlug slug f dash = dash & #tabs %~ fmap (map updateTab)
  where
    updateTab tab = if slugify tab.name == slug then f tab else tab


getDashAndVM :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, Error ServerError :> es, Wreq.HTTP :> es) => Dashboards.DashboardId -> Maybe Text -> Eff es (Dashboards.DashboardVM, Dashboards.Dashboard)
getDashAndVM dashId fileM = do
  appCtx <- ask @AuthContext
  templates <- getDashboardTemplates appCtx.config.liveReloadDashboards
  dashVM <-
    Dashboards.getDashboardById dashId
      `whenNothingM` throwError err404{errBody = "Dashboard with ID not found. ID:" <> encodeUtf8 dashId.toText}
  dash <- maybe (pure $ fromMaybe def (loadDashboardFromVM templates dashVM)) Dashboards.readDashboardEndpoint fileM
  pure (dashVM, dash)


-- | Page shell shared by the dashboard and dashboard-tab handlers.
-- @tabM@ is @Just (slug, name)@ on tab pages, enabling the tab breadcrumb + rename modal.
dashboardBWConf :: BWConfig -> Projects.ProjectId -> Text -> Dashboards.DashboardId -> Text -> Maybe (Text, Maybe Text) -> Maybe (Text, Text) -> FreeTierStatus -> BWConfig
dashboardBWConf bw pid paymentPlan dashId title tabM currentRange freeTierStatus =
  bw
    { prePageTitle = Just "Dashboards"
    , pageTitle = dashTitle title
    , pageTitleSuffix = tabM >>= snd
    , pageTitleModalId = Just "pageTitleModalId"
    , pageTitleSuffixModalId = "tabRenameModalId" <$ tabM
    , freeTierStatus = freeTierStatus
    , headContent = Just dashboardHeadContent_
    , needsGridStack = True
    , pageActions = Just $ div_ [class_ "flex gap-3 max-md:gap-1 items-center"] do
        TimePicker.timepicker_ Nothing currentRange Nothing
        TimePicker.refreshButton_
        dashboardActions_ pid paymentPlan dashId (fst <$> tabM) currentRange
    , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/dashboard/"
    }


dashboardGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId fileM fromDStr toDStr sinceStr allParams = do
  (_, project, bw) <- mkPageCtx pid
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)

  (dashVM, dash) <- getDashAndVM dashId fileM

  -- If the dashboard has tabs, redirect to the first tab's URL
  -- This ensures users always land on a tab-based URL for dashboards with tabs
  case getDefaultTabSlug dash.tabs of
    Just firstTabSlug -> do
      let redirectUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/tab/" <> firstTabSlug <> queryStringFrom allParams
      throwError $ err302{errHeaders = [("Location", encodeUtf8 redirectUrl)]}
    Nothing -> do
      -- No tabs - render the dashboard normally (existing behavior for non-tabbed dashboards)
      let timeParams = (sinceStr, fromDStr, toDStr)
      (dash', allParamsWithConstants) <- resolveDashboardParams pid now timeParams allParams dash
      dash'' <- (\ws -> dash' & #widgets .~ ws) <$> processDashWidgets pid dashId now timeParams allParamsWithConstants dash'.widgets

      bwconf <- dashboardBWConf bw pid project.paymentPlan dashId dashVM.title Nothing currentRange <$> checkFreeTierStatus pid project.paymentPlan
      addRespHeaders $ PageCtx bwconf $ DashboardGet pid dashId dash'' dashVM allParams


numberedStep_ :: Int -> Text -> Html () -> Html ()
numberedStep_ n title content = div_ [class_ "space-y-4"] do
  div_ [class_ "flex items-start gap-3"] do
    span_ [class_ "flex-shrink-0 inline-flex items-center justify-center w-7 h-7 rounded-full bg-fillWeak text-sm font-medium tabular-nums"] $ toHtml (show n)
    strong_ [class_ "text-base font-semibold text-textStrong"] $ toHtml title
  div_ [class_ "pl-10"] content


widgetViewerEditor_ :: Projects.ProjectId -> Text -> Maybe Dashboards.DashboardId -> Maybe Text -> Maybe (Text, Text) -> Maybe Widget.Widget -> Text -> Html ()
widgetViewerEditor_ pid paymentPlan dashboardIdM tabSlugM currentRange existingWidgetM activeTab = div_ [class_ "group/wgtexp"] do
  let isNewWidget = isNothing existingWidgetM
      effectiveActiveTab = if isNewWidget then "edit" else activeTab
      defaultWidget =
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
      widgetBase = fromMaybe defaultWidget existingWidgetM
      widgetToUse = widgetBase{Widget.id = Just $ maybeToMonoid widgetBase.id <> "Expanded", Widget.standalone = Just True, Widget.naked = Just True}
      -- Element ids are derived from the widget id to prevent conflicts
      wid = maybeToMonoid widgetToUse.id
      sourceWid = normalizeWidgetId wid
      widPrefix = "id" <> T.take 8 wid
      widgetFormId = widPrefix <> "-widget-form"
      widgetPreviewId = widPrefix <> "-widget-preview"
      widgetTitleInputId = widPrefix <> "-widget-title-input"
      drawerStateCheckbox = if isJust existingWidgetM then "global-data-drawer" else "page-data-drawer"
      stickySentinelId = widPrefix <> "-sticky-sentinel"
      stickyContainerId = widPrefix <> "-sticky-container"
      widgetJSON = Widget.encodeText widgetToUse
      formAction = flip foldMap dashboardIdM \dashId ->
        let params = catMaybes [("widget_id=" <>) . maybeToMonoid . (.id) <$> existingWidgetM, ("tab=" <>) <$> tabSlugM]
         in "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> memptyIfFalse (not (null params)) ("?" <> T.intercalate "&" params)

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
        [text| on htmx:before:request 
            set widgetJSON.title to #{'${widgetTitleInputId}'}.value then 
            if not widgetJSON.id
              gridStackInstance.removeWidget('#add_a_widget_label', true, false)
            end
           on htmx:before:swap 
            call event.preventDefault() then
            set #${drawerStateCheckbox}.checked to false then
            if @data-formMode == "edit"
              call gridStackInstance.update(#{'${sourceWid}_widgetEl'}, {content: event.detail.ctx.text})
            else
              call gridStackInstance.addWidget({w: 3, h: 3, content: event.detail.ctx.text})
            end
        |]
    ]
    ""

  -- Sentinel for sticky header detection
  div_ [id_ stickySentinelId, class_ "h-px w-full", Aria.hidden_ "true"] ""

  -- Sticky container for header + preview
  let widgetTypeAttr = case widgetToUse.wType of
        Widget.WTTable -> "table"
        Widget.WTStat -> "stat"
        Widget.WTTimeseriesStat -> "stat"
        _ -> "chart"
  div_
    [ id_ stickyContainerId
    , class_ "sticky top-0 z-20 -mx-8 px-8 pb-2 bg-bgRaised widget-drawer-sticky"
    , term "_" [text|on load js(me) { window.setupStickyObserver('${stickySentinelId}', '${stickyContainerId}') } end|]
    ]
    do
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
              mkTab "Overview" (effectiveActiveTab `notElem` ["edit", "alerts"])
              mkTab "Edit" (effectiveActiveTab == "edit")
              mkTab "Monitors" (effectiveActiveTab == "alerts")
          when isNewWidget $ h3_ [class_ "text-lg font-semibold text-textStrong"] "Add a new widget"

        div_ [class_ "flex items-center gap-3"] do
          TimePicker.timepicker_ Nothing currentRange (Just "widget")
          TimePicker.refreshButton_
          div_ [class_ "w-px h-5 bg-strokeWeak"] ""
          button_ [class_ $ "btn btn-primary btn-sm shadow-sm" <> memptyIfFalse (not isNewWidget) " hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block", type_ "submit", form_ widgetFormId] "Save changes"
          label_ [class_ "btn btn-ghost btn-circle btn-sm tap-target text-iconNeutral hover:text-iconBrand", Aria.label_ "Close drawer", data_ "tippy-content" "Close Drawer", Lucid.for_ drawerStateCheckbox] $ faSprite_ "xmark" "regular" "w-3 h-3"

      div_ [class_ "w-full aspect-4/1 p-4 rounded-xl bg-fillWeaker border border-strokeWeak widget-preview-container", data_ "widget-type" widgetTypeAttr] do
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
          Components.chartSkeleton_

  div_ [class_ $ if isNewWidget then "block mt-6" else "hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block mt-6"] do
    div_ [class_ "space-y-8"] do
      numberedStep_ 1 "Configure Query" $ div_ [class_ "flex flex-col gap-3"] do
        logQueryBox_
          LogQueryBoxConfig
            { pid = pid
            , currentRange = Nothing
            , source = Nothing
            , targetSpan = Nothing
            , query = widgetToUse.rawQuery <|> widgetToUse.query
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
            , mobileExtra = Nothing
            , parseError = Nothing
            }
        details_ [class_ "text-xs text-textWeak"] do
          summary_ [class_ "cursor-pointer hover:text-textStrong select-none transition-colors"] "Show generated SQL"
          div_
            [ id_ $ widPrefix <> "-sql-preview"
            , hxGet_ $ "/p/" <> pid.toText <> "/widget/sql-preview"
            , hxVals_ "js:{query: widgetJSON.raw_query || widgetJSON.query}"
            , hxTrigger_ "toggle from:closest details"
            , hxSwap_ "innerHTML"
            ]
            $ loadingIndicator_ LdXS LdSpinner

      numberedStep_ 2 "Give your graph a title"
        $ input_
          [ class_ "input input-bordered w-full"
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

  -- Alerts tab content
  unless isNewWidget do
    let alertFormId = widPrefix <> "-alert-form"
        alertEndpoint = flip foldMap dashboardIdM \dashId -> "/p/" <> pid.toText <> "/widgets/" <> sourceWid <> "/alert?dashboard_id=" <> dashId.toText
    div_ [class_ "group/walert hidden group-has-[.page-drawer-tab-alerts:checked]/wgtexp:block mt-6"] do
      widgetAlertConfig_ pid paymentPlan alertFormId alertEndpoint wid sourceWid widgetToUse


-- | Widget alert configuration form (unified with Log Explorer form structure)
widgetAlertConfig_ :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Widget.Widget -> Html ()
widgetAlertConfig_ _pid paymentPlan alertFormId alertEndpoint chartTargetId widgetId widget = do
  let hasAlert = isJust widget.alertId
      defaultTitle = fromMaybe "Widget Alert" widget.title <> " - Threshold Alert"
  -- Enable Alert toggle
  label_ [class_ "flex items-center justify-between p-4 bg-fillWeaker rounded-xl border border-strokeWeak cursor-pointer mb-4"] do
    div_ [] do
      h4_ [class_ "font-medium text-textStrong"] "Enable Alert"
      p_ [class_ "text-xs text-textWeak"] "Get notified when this widget's value crosses thresholds"
    input_ $ [type_ "checkbox", name_ "alertEnabled", class_ "toggle toggle-primary alert-enable"] <> [checked_ | hasAlert]
  form_
    [ id_ alertFormId
    , hxPost_ alertEndpoint
    , hxSwap_ "none"
    , hxTrigger_ "submit"
    , hxVals_ "js:{teams: window.getTagValues('#teamHandlesInput')}"
    , class_ "flex flex-col gap-3 hidden group-has-[.alert-enable:checked]/walert:flex"
    , [__|on htmx:after:request if event.detail.ctx.response.status < 400 set my value to '' then call me.reset() end|]
    ]
    do
      input_ [type_ "hidden", name_ "widgetId", value_ widgetId]
      input_ [type_ "hidden", name_ "query", value_ $ fromMaybe "" widget.query]
      input_ [type_ "hidden", name_ "vizType", value_ $ case widget.wType of Widget.WTTimeseries -> "timeseries"; Widget.WTTimeseriesLine -> "timeseries_line"; _ -> "timeseries"]

      Components.formField_ Components.FieldSm def{Components.value = defaultTitle, Components.placeholder = "e.g. High error rate monitor"} "Name" "title" True Nothing
      -- Monitor Schedule section (shared component)
      Alerts.monitorScheduleSection_ paymentPlan 5 5 (Just "threshold_exceeded") (Just chartTargetId)
      -- Thresholds section (shared component)
      Alerts.thresholdsSection_ (Just chartTargetId) widget.alertThreshold widget.warningThreshold False Nothing Nothing
      -- Widget-specific: Show threshold lines option
      let currentLines = fromMaybe "always" widget.showThresholdLines
      div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak p-3"]
        $ Components.formSelectField_ Components.FieldSm "Show threshold lines on chart" "showThresholdLines" False
        $ forM_ ([("always", "Always"), ("on_breach", "Only when breached"), ("never", "Never")] :: [(Text, Text)]) \(v, lbl) ->
          option_ ([value_ v] <> [selected_ "" | v == currentLines]) $ toHtml lbl

      -- Notification Settings section (shared component) - empty teams, users can configure after creation
      Alerts.notificationSettingsSection_ Nothing Nothing Nothing True V.empty V.empty alertFormId Nothing

      -- Action buttons
      div_ [class_ "flex items-center justify-end gap-2 pt-4 pb-20 mt-4 border-t border-strokeWeak"] do
        when hasAlert $ button_ [type_ "button", class_ "btn btn-ghost btn-sm", hxDelete_ alertEndpoint, hxSwap_ "none"] "Remove monitor"
        primaryButton_ [type_ "submit"] do
          faSprite_ "plus" "regular" "w-3.5 h-3.5"
          if hasAlert then "Update monitor" else "Create monitor"


--------------------------------------------------------------------
-- Widget Alert Handlers
--

data WidgetAlertForm = WidgetAlertForm
  { widgetId :: Text
  , query :: Text
  , vizType :: Maybe Text
  , alertEnabled :: Maybe Text -- "on" when checked
  , alertThreshold :: Double
  , warningThreshold :: Maybe Text
  , direction :: Text
  , showThresholdLines :: Maybe Text
  , alertRecoveryThreshold :: Maybe Text
  , warningRecoveryThreshold :: Maybe Text
  , frequency :: Maybe Text
  , timeWindow :: Maybe Text
  , conditionType :: Maybe Text
  , title :: Text
  , severity :: Maybe Text
  , subject :: Maybe Text
  , message :: Maybe Text
  , recipientEmailAll :: Maybe Text
  , teams :: [UUID.UUID]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


widgetAlertUpsertH :: Projects.ProjectId -> Text -> Maybe UUID.UUID -> WidgetAlertForm -> ATAuthCtx (RespHeaders (Html ()))
widgetAlertUpsertH pid _widgetIdPath dashboardIdM form = do
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime

  -- Reuse the widget's existing monitor id when there is one
  queryMonitorId <-
    Monitors.queryMonitorByWidgetId form.widgetId
      >>= maybe (liftIO $ Monitors.QueryMonitorId <$> UUID.nextRandom) (pure . (.id))

  -- Update widget's showThresholdLines in the dashboard
  whenJust dashboardIdM \dashId -> do
    let dashboardId = UUIDId dashId
    (_, dash) <- getDashAndVM dashboardId Nothing
    let updateWidget w = if w.id == Just form.widgetId then w{Widget.showThresholdLines = form.showThresholdLines} else w
        dash' = dash & #widgets %~ map updateWidget & #tabs %~ fmap (map (\t -> t & #widgets %~ map updateWidget))
    void $ Dashboards.updateSchema dashboardId dash'

  -- If alertEnabled is not checked, delete the monitor
  case form.alertEnabled of
    Nothing -> do
      _ <- Monitors.deleteMonitorsByWidgetIds [form.widgetId]
      addSuccessToast "Monitor removed from widget" Nothing
      addRespHeaders $ toHtml ("" :: Text)
    Just _ -> do
      -- Convert to AlertUpsertForm and reuse convertToQueryMonitor
      let alertForm =
            Alerts.AlertUpsertForm
              { alertId = Just $ Monitors.unQueryMonitorId queryMonitorId & UUID.toText
              , alertThreshold = form.alertThreshold
              , warningThreshold = form.warningThreshold
              , recipientEmails = []
              , recipientSlacks = []
              , recipientEmailAll = Just (form.recipientEmailAll == Just "true")
              , direction = form.direction
              , title = form.title
              , severity = fromMaybe "Warning" form.severity
              , subject = fromMaybe form.title form.subject
              , message = fromMaybe "" form.message
              , query = form.query
              , since = "1h"
              , from = ""
              , to = ""
              , frequency = form.frequency
              , timeWindow = form.timeWindow
              , conditionType = form.conditionType <|> Just "threshold_exceeded"
              , source = Just "widget"
              , vizType = form.vizType
              , teams = form.teams
              , alertRecoveryThreshold = form.alertRecoveryThreshold
              , warningRecoveryThreshold = form.warningRecoveryThreshold
              , widgetId = Just form.widgetId
              , dashboardId = UUID.toText <$> dashboardIdM
              , notifyAfterCheck = Nothing
              , notifyAfter = Nothing
              , stopAfterCheck = Nothing
              , stopAfter = Nothing
              }

      let queryMonitor = Alerts.convertToQueryMonitor pid now queryMonitorId alertForm
      _ <- Monitors.queryMonitorUpsert queryMonitor
      addSuccessToast "Widget monitor configured successfully" Nothing
      addRespHeaders $ toHtml ("" :: Text)


widgetAlertDeleteH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
widgetAlertDeleteH pid widgetId = do
  _ <- Projects.sessionAndProject pid
  _ <- Monitors.deleteMonitorsByWidgetIds [widgetId]
  addSuccessToast "Monitor removed from widget" Nothing
  addRespHeaders $ toHtml ("" :: Text)


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
  , copyMode :: Maybe (Text, Dashboards.DashboardId) -- (widgetId, sourceDashboardId) for copy-to-dashboard mode
  , dashTemplates :: [Dashboards.Dashboard]
  , showNew :: Bool
  }
  deriving (Generic)
data DashboardsGet
  = DashboardsGet (PageCtx DashboardsGetD)
  | DashboardsGetSlim DashboardsGetD
  deriving (Generic)


instance ToHtml DashboardsGet where
  toHtml (DashboardsGet (PageCtx pc dg)) = toHtml $ PageCtx pc $ dashboardsGet_ dg
  toHtml (DashboardsGetSlim dash) = toHtml $ dashboardsGet_ dash
  toHtmlRaw = toHtml


renderDashboardListItem :: Bool -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Html ()
renderDashboardListItem checked title value description icon prview = label_
  [ class_
      [text| cursor-pointer group/it text-sm border border-transparent hover:bg-fillWeaker hover:border-strokeWeak rounded-lg flex p-1.5 gap-2 items-center
      group-has-[input:checked]/it:bg-fillWeaker group-has-[input:checked]/it:border-strokeWeak dashboardListItem|]
  , term "data-title" title
  , term "data-description" $ maybeToMonoid description
  , term "data-preview" $ fromMaybe "/public/assets/svgs/screens/dashboard_blank.svg" prview
  , [__| on mouseover set #dItemPreview.src to my @data-preview
              then set #dItemTitle.innerText to my @data-title
              then set #dItemDescription.innerText to my @data-description
          on mouseout
              put (<.dashboardListItem:has(input:checked)/>) into checkedLabel
              set #dItemPreview.src to checkedLabel's @data-preview
              then set #dItemTitle.innerText to checkedLabel's @data-title
              then set #dItemDescription.innerText to checkedLabel's @data-description
              |]
  ]
  do
    input_ $ [class_ "hidden", type_ "radio", name_ "file", value_ value] <> [checked_ | checked]
    span_ [class_ "p-1 px-2 bg-fillWeak rounded-md"] $ faSprite_ (fromMaybe "square-dashed" icon) "regular" "w-3 h-3"
    span_ [class_ "grow"] $ toHtml title
    span_ [class_ "px-2 p-1 invisible group-has-[input:checked]/it:visible"] $ faSprite_ "chevron-right" "regular" "w-3 h-3"


starButton_ :: Projects.ProjectId -> Dashboards.DashboardId -> Bool -> Html ()
starButton_ pid dashId isStarred =
  button_
    [ id_ $ "star-btn-" <> dashId.toText
    , class_ $ "leading-none cursor-pointer " <> if isStarred then "" else "opacity-0 group-hover/row:opacity-100"
    , data_ "tippy-content" $ if isStarred then "Click to unstar this dashboard" else "Click to star this dashboard"
    , hxPost_ $ "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/star"
    , hxTarget_ $ "#star-btn-" <> dashId.toText
    , hxSwap_ "outerHTML"
    ]
    $ faSprite_ "star" (if isStarred then "solid" else "regular")
    $ "w-4 h-4 "
    <> if isStarred then "text-iconWarning" else "text-iconNeutral"


dashboardsGet_ :: DashboardsGetD -> Html ()
dashboardsGet_ dg = do
  unless dg.embedded $ Components.modalWith_ "newDashboardMdl" def{autoOpen = dg.showNew} Nothing $ form_
    [ class_ "flex  h-[90vh] gap-4 group/md"
    , hxPost_ ""
    , hxVals_ "js:{ teams: window.getTagValues('#teamHandlesInput') }"
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
          renderDashboardListItem True "Blank dashboard" "" (Just "Get started from a blank slate") (Just "cards-blank") Nothing
          forM_ dg.dashTemplates \dashTmpl ->
            renderDashboardListItem False (maybeToMonoid dashTmpl.title) (maybeToMonoid dashTmpl.file) dashTmpl.description dashTmpl.icon dashTmpl.preview

      div_ [class_ "w-5/7 px-3 py-5 h-full overflow-y-scroll "] do
        div_ [class_ "flex items-end gap-2"] do
          div_ [class_ "flex w-full gap-2"] do
            formField_ FieldSm def{placeholder = "Dashboard Title"} "Dashboard name" "title" True Nothing
            let teamList = Widget.encodeText $ (\x -> AE.object ["name" AE..= x.handle, "value" AE..= x.id]) <$> dg.teams
            formField_ FieldSm def{placeholder = "Add teams"} "Teams" "teamHandlesInput" False $ Just $ tagInput_ "teamHandlesInput" "Add teams" [data_ "tagify-text-prop" "name", data_ "tagify-whitelist" teamList]
            formField_ FieldSm def{placeholder = "reports/"} "Folder" "fileDir" False Nothing
          div_ [class_ "shrink"] $ primaryButton_ [type_ "submit"] "Create"
        div_ [class_ "py-2 border-b border-b-strokeWeak"] do
          span_ [class_ "text-sm "] "Using "
          span_ [class_ "text-sm font-medium", id_ "dItemTitle"] "Custom Dashboard"
          span_ [class_ "text-sm "] " template"
          p_ [class_ "text-xs text-textWeak w-full overflow-ellipsis truncate", id_ "dItemDescription"] "Get started from a blank slate"
        div_ [class_ "pt-5"]
          $ div_ [class_ "bg-fillBrand-strong px-2 py-4 rounded-xl w-full flex items-center"]
          $ img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full rounded overflow-hidden", id_ "dItemPreview", term "loading" "lazy", term "decoding" "async"]

  div_ [id_ "itemsListPage", class_ "mx-auto gap-8 w-full flex flex-col h-full overflow-hidden group/pg"] do
    let getTeams x = mapMaybe (\xx -> find (\t -> t.id == xx) dg.teams) (V.toList x.teams)
        getDashIcon dash = fromMaybe "square-dashed" (loadDashboardFromVM dg.dashTemplates dash >>= (.icon))
        getWidgetCount dash = maybe 0 (length . (.widgets)) (loadDashboardFromVM dg.dashTemplates dash)
        noBulkActions = dg.embedded || dg.hideActions || isJust dg.copyMode
        inCopyMode = isJust dg.copyMode
        baseUrl = "/p/" <> dg.projectId.toText <> "/dashboards"

        renderNameCol dash = do
          let dashUrl = baseUrl <> "/" <> dash.id.toText
              folder = folderFromPath dash.filePath
          span_ [class_ "flex items-center gap-2"] do
            span_ [class_ "p-1 px-2 bg-fillWeak rounded-md", data_ "tippy-content" "Dashboard icon"] $ faSprite_ (getDashIcon dash) "regular" "w-3 h-3"
            unless (T.null folder) $ span_ [class_ "text-xs text-textWeak font-mono", data_ "tippy-content" "Folder path for git sync"] $ toHtml folder
            if inCopyMode
              then span_ [class_ "font-medium text-textStrong"] $ toHtml $ dashTitle dash.title
              else a_ [href_ dashUrl, class_ "font-medium text-textStrong hover:text-textBrand hover:underline underline-offset-2"] $ toHtml $ dashTitle dash.title
            unless inCopyMode $ starButton_ dg.projectId dash.id (isJust dash.starredSince)
          div_ [class_ "hidden max-md:flex items-center gap-2 mt-1 text-xs text-textWeak flex-wrap"] do
            span_ [class_ "tabular-nums"] $ toHtml $ toText $ formatTime defaultTimeLocale "%b %-e" dash.updatedAt
            forM_ (getTeams dash) \team -> span_ [class_ "badge badge-sm badge-neutral"] $ toHtml team.handle
            forM_ (V.toList dash.tags) $ span_ [class_ "badge badge-sm badge-neutral"] . toHtml

        renderModifiedCol dash = span_ [class_ "text-xs text-textWeak tabular-nums", data_ "tippy-content" "Last modified date"] $ Components.localTimeFmt_ "MMM d, h:mm aaa" dash.updatedAt
        renderTeamsCol dash = forM_ (getTeams dash) \team -> span_ [class_ "badge badge-sm badge-neutral mr-1"] $ toHtml team.handle
        renderTagsCol dash = forM_ (V.toList dash.tags) \tag ->
          if inCopyMode
            then span_ [class_ "badge badge-sm badge-neutral mr-1"] $ toHtml tag
            else a_ [class_ "badge badge-sm badge-neutral mr-1 cursor-pointer hover:badge-primary", hxGet_ $ baseUrl <> "?tag=" <> toUriStr tag, hxTarget_ "#dashboardsTableContainer", hxSelect_ "#dashboardsTableContainer", hxPushUrl_ "true", hxSwap_ "outerHTML"] $ toHtml tag

        renderWidgetsCol dash = do
          let count = getWidgetCount dash
          span_ [class_ "flex items-center gap-1.5 text-textWeak", data_ "tippy-content" $ show count <> " widget" <> if count == 1 then "" else "s"] do
            faSprite_ "grid" "regular" "w-3.5 h-3.5 text-iconNeutral"
            span_ [class_ "leading-none tabular-nums"] $ toHtml $ show count

    let nameCol = Table.col "Name" renderNameCol & Table.withAttrs [class_ "min-w-0"]
        tableCols =
          [if inCopyMode then nameCol else nameCol & Table.withSort "title"]
            <> [Table.col "Last Modified" renderModifiedCol & Table.withAttrs [class_ "w-44 max-md:hidden"] & Table.withSort "updated_at" | not inCopyMode]
            <> [ Table.col "Teams" renderTeamsCol & Table.withAttrs [class_ "w-48 max-md:hidden"]
               , Table.col "Tags" renderTagsCol & Table.withAttrs [class_ "w-48 max-md:hidden"]
               , Table.col "Widgets" renderWidgetsCol & Table.withAttrs [class_ "w-24 max-md:hidden"]
               ]

    let table =
          Table
            { config = def{Table.elemID = "dashboardsTable", Table.showHeader = not dg.embedded || inCopyMode, Table.addPadding = not dg.embedded && not dg.hideActions && not inCopyMode, Table.renderAsTable = not dg.embedded || inCopyMode, Table.bulkActionsInHeader = if noBulkActions then Nothing else Just 0, Table.noSurface = dg.hideActions || inCopyMode}
            , columns = tableCols
            , rows = dg.dashboards
            , features =
                def
                  { Table.rowId = if noBulkActions then Nothing else Just \dash -> dash.id.toText
                  , Table.rowAttrs = Just $ \dash -> case dg.copyMode of
                      Just (widgetId, sourceDashId) ->
                        [ class_ "cursor-pointer hover:bg-fillWeak tap-target"
                        , hxPost_ $ "/p/" <> dg.projectId.toText <> "/dashboards/" <> dash.id.toText <> "/widgets/" <> widgetId <> "/duplicate?source_dashboard_id=" <> sourceDashId.toText
                        , hxSwap_ "none"
                        , [__| on htmx:after:request set #dashboards-modal.checked to false |]
                        ]
                      Nothing -> [class_ "group/row"]
                  , Table.bulkActions =
                      if noBulkActions
                        then []
                        else
                          [ Table.BulkAction{icon = Just "plus", title = "Add teams", uri = "/p/" <> dg.projectId.toText <> "/dashboards/bulk_action/add_teams"}
                          , Table.BulkAction{icon = Just "trash", title = "Delete", uri = "/p/" <> dg.projectId.toText <> "/dashboards/bulk_action/delete"}
                          ]
                  , Table.search = if noBulkActions then Nothing else Just Table.ClientSide
                  , Table.tableHeaderActions = dg.tableActions
                  , Table.header = if dg.embedded || null dg.filters.tag || inCopyMode then Nothing else Just $ activeFilters_ dg.projectId baseUrl dg.filters
                  , Table.zeroState = if dg.embedded then Nothing else Just Table.ZeroState{icon = "chart-area", title = "No dashboards yet", description = "Create your first dashboard to visualize your data", actionText = "Create Dashboard", destination = Left "newDashboardMdl"}
                  }
            }

    div_ [class_ "w-full", id_ "dashboardsTableContainer"] do
      when inCopyMode $ div_ [class_ "mb-4 p-3 bg-fillWeak rounded-lg text-sm text-textStrong"] do
        faSprite_ "circle-info" "regular" "w-4 h-4 inline mr-2"
        "Select a dashboard to copy this widget to"
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
        , Aria.label_ $ "Remove filter: " <> tag
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


dashboardsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UUID.UUID -> Maybe Text -> Maybe UUID.UUID -> Maybe Text -> DashboardFilters -> ATAuthCtx (RespHeaders DashboardsGet)
dashboardsGetH pid sortM embeddedM teamIdM copyWidgetIdM sourceDashIdM newM filters = do
  (_, project, bw) <- mkPageCtx pid

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
  let availableTags = ordNub $ concatMap (V.toList . (.tags)) (V.toList dashboards')

  -- Apply tag filtering
  let dashboards = if null filters.tag then dashboards' else V.filter (\d -> any (`elem` filters.tag) (V.toList d.tags)) dashboards'

  teams <- V.fromList <$> ManageMembers.getTeams pid

  -- Check if we're requesting in embedded mode (for modals, etc.)
  let embedded = maybe False (`elem` ["true", "1", "yes"]) embeddedM
      isTeamView = isJust teamIdM
      copyMode = (,) <$> copyWidgetIdM <*> (UUIDId <$> sourceDashIdM)

  templates <- getDashboardTemplates bw.config.liveReloadDashboards
  if embedded || isTeamView
    then -- For embedded/team mode, use a minimal BWConfig that will still work with ToHtml instance
      addRespHeaders $ DashboardsGetSlim DashboardsGetD{dashboards, projectId = pid, embedded, hideActions = isTeamView, teams, tableActions = Nothing, filters, copyMode, dashTemplates = templates, showNew = False}
    else do
      freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
      let bwconf =
            bw
              { pageTitle = "Dashboards"
              , freeTierStatus = freeTierStatus
              , headContent = Just dashboardHeadContent_
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
                , headerExtra = Nothing
                }
      addRespHeaders $ DashboardsGet (PageCtx bwconf $ DashboardsGetD{dashboards, projectId = pid, embedded = False, hideActions = False, teams, tableActions, filters, copyMode, dashTemplates = templates, showNew = isJust newM})


data DashboardRes = DashboardNoContent | DashboardPostError Text | DashboardRenameSuccess Text
  deriving (Generic, Show)


instance ToHtml DashboardRes where
  toHtml DashboardNoContent = ""
  toHtml (DashboardPostError msg) = div_ [class_ "text-textError"] $ toHtml msg
  toHtml (DashboardRenameSuccess title) = toHtml $ dashTitle title
  toHtmlRaw = toHtml


data DashboardForm = DashboardForm
  { file :: Text
  , teams :: [UUID.UUID]
  , title :: Text
  , fileDir :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


dashboardsPostH :: Projects.ProjectId -> DashboardForm -> ATAuthCtx (RespHeaders DashboardRes)
dashboardsPostH pid form = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  did <- UUIDId <$> UUID.genUUID
  templates <- getDashboardTemplates appCtx.config.liveReloadDashboards
  if form.title == ""
    then toastError "Dashboard title is required" (DashboardPostError "Dashboard title is required")
    else do
      let dashM = find (\dashboard -> dashboard.file == Just form.file) templates
          redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> did.toText
          dir = fromMaybe "" form.fileDir
          filePath = if T.null dir then Nothing else Just $ dashFilePath dir form.title
          dbd =
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
              , filePath = filePath
              , fileSha = Nothing
              }
      _ <- Dashboards.insert dbd
      syncDashboardAndQueuePush pid dbd.id
      redirectCS redirectURI
      addRespHeaders DashboardNoContent


-- TH splice: reads all dashboard YAML files from static/public/dashboards at compile time
dashboardTemplatesCompiled :: [Dashboards.Dashboard]
dashboardTemplatesCompiled = $(Dashboards.readDashboardsFromDirectory "static/public/dashboards")


-- When liveReload is True, reads from disk on every access (for dev iteration without restart).
getDashboardTemplates :: IOE :> es => Bool -> Eff es [Dashboards.Dashboard]
getDashboardTemplates liveReload
  | liveReload = liftIO $ Dashboards.readDashboardsFromDisk "static/public/dashboards"
  | otherwise = pure dashboardTemplatesCompiled


-- THe current /p/:projectId/  handler. Redirects users to the overview dashboard if it exists, or creates it.
entrypointRedirectGetH
  :: Text
  -> Text
  -> [Text]
  -> Projects.ProjectId
  -> [(Text, Maybe Text)]
  -> ATAuthCtx (Headers '[Header "Location" Text] NoContent)
entrypointRedirectGetH baseTemplate title tags pid qparams = do
  (sess, project) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  let mkPath p d = "/p/" <> pid.toText <> p <> d <> queryStringFrom qparams
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
    if Projects.isOnboarding project.paymentPlan
      then pure $ mkPath "/onboarding" ""
      else mkPath "/dashboards/" <$> (maybe newDashboard (pure . (.toText)) =<< Dashboards.getDashboardByBaseTemplate pid baseTemplate)
  pure $ addHeader redirectTo NoContent


-- | Form data for renaming a dashboard
data DashboardRenameForm = DashboardRenameForm
  { title :: Text
  , fileDir :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Handler for renaming a dashboard.
-- It updates the title and optionally the git file path.
dashboardRenamePatchH :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardRenameForm -> ATAuthCtx (RespHeaders DashboardRes)
dashboardRenamePatchH pid dashId form = do
  _ <- Projects.sessionAndProject pid
  Dashboards.getDashboardById dashId >>= \case
    Nothing -> toastError "Dashboard not found or does not belong to this project" (DashboardPostError "Dashboard not found or does not belong to this project")
    Just dashVM -> do
      _ <- Dashboards.updateTitle dashId form.title

      whenJust dashVM.schema \schema ->
        void $ Dashboards.updateSchema dashId (schema & #title ?~ form.title)

      let newPath = dashFilePath (fromMaybe "" form.fileDir) form.title
      when (Just newPath /= dashVM.filePath)
        $ void
        $ GitSync.updateDashboardGitInfo dashId newPath ""

      syncDashboardAndQueuePush pid dashId
      addSuccessToast "Dashboard updated successfully" Nothing
      addTriggerEvent "closeModal" ""
      addRespHeaders $ DashboardRenameSuccess form.title


-- | Handler for duplicating a dashboard.
-- It creates a new dashboard with the same content but with "(Copy)" appended to the title.
dashboardDuplicatePostH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders DashboardRes)
dashboardDuplicatePostH pid dashId = do
  _ <- Projects.sessionAndProject pid
  Dashboards.getDashboardById dashId >>= \case
    Nothing -> toastError "Dashboard not found or does not belong to this project" (DashboardPostError "Dashboard not found or does not belong to this project")
    Just dashVM -> do
      sess <- Projects.getSession
      now <- Time.currentTime
      newDashId <- UUIDId <$> UUID.genUUID

      let copyTitle = dashTitle dashVM.title <> " (Copy)"
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
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime
  dashVM <- Dashboards.getDashboardById dashId `whenNothingM` throwError err404{errBody = "Dashboard not found"}
  let newStarredSince = if isJust dashVM.starredSince then Nothing else Just now
  _ <- Dashboards.updateStarredSince dashId newStarredSince
  addSuccessToast (if isJust newStarredSince then "Dashboard starred" else "Dashboard unstarred") Nothing
  addRespHeaders $ starButton_ pid dashId (isJust newStarredSince)


-- | Handler for deleting a dashboard.
-- It verifies the dashboard exists and belongs to the project before deletion.
-- After deletion, redirects to the dashboard list page.
dashboardDeleteH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders DashboardRes)
dashboardDeleteH pid dashId = do
  _ <- Projects.sessionAndProject pid
  _ <- Dashboards.getDashboardById dashId `whenNothingM` throwError err404{errBody = "Dashboard not found or does not belong to this project"}
  _ <- Dashboards.deleteDashboard dashId
  redirectCS $ "/p/" <> pid.toText <> "/dashboards"
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
  _ <- Projects.sessionAndProject pid
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
-- If the original widget has an alert, the alert is also duplicated with the new widget.
-- When source_dashboard_id is provided and differs from target dashboard, this copies the widget to a different dashboard.
dashboardDuplicateWidgetPostH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> Maybe UUID.UUID -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardDuplicateWidgetPostH pid targetDashId widgetId sourceDashIdM = do
  _ <- Projects.sessionAndProject pid
  let sourceDashId = maybe targetDashId UUIDId sourceDashIdM
      isCrossDashboard = sourceDashId /= targetDashId

  -- Get source dashboard to find the widget
  (_, sourceDash) <- getDashAndVM sourceDashId Nothing
  case findWidgetInDashboard widgetId sourceDash of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just (tabSlugM, widgetToDuplicate) -> do
      newWidgetId <- UUID.genUUID <&> UUID.toText
      now <- Time.currentTime

      -- Clone alert if original widget has one
      existingMonitorM <- Monitors.queryMonitorByWidgetId (normalizeWidgetId widgetId)
      newAlertIdM <- forM existingMonitorM \monitor -> do
        newMonitorId <- Monitors.QueryMonitorId <$> liftIO UUID.nextRandom
        let Monitors.QueryMonitor{id = _, widgetId = _, createdAt = _, updatedAt = _, alertLastTriggered = _, warningLastTriggered = _, currentStatus = _, ..} = monitor
            newMonitor =
              Monitors.QueryMonitor
                { id = newMonitorId
                , createdAt = now
                , updatedAt = now
                , widgetId = Just newWidgetId
                , alertLastTriggered = Nothing
                , warningLastTriggered = Nothing
                , currentStatus = Monitors.MSNormal
                , ..
                }
        void $ Monitors.queryMonitorUpsert newMonitor
        pure newMonitorId.toText

      -- Target dashboard (might be the source); its title names the toast on a cross-dashboard copy
      (targetVM, targetDash) <- getDashAndVM targetDashId Nothing
      let targetDashNameM = dashTitle targetVM.title <$ guard isCrossDashboard
          titleSuffix = bool " (Copy)" "" isCrossDashboard
          widgetCopy =
            widgetToDuplicate
              { Widget.id = Just newWidgetId
              , Widget.title = Just $ (case maybeToMonoid widgetToDuplicate.title of "" -> "Widget"; t -> t) <> titleSuffix
              , Widget._projectId = Just pid
              , Widget._dashboardId = Just targetDashId.toText
              , Widget.alertId = newAlertIdM
              , Widget.alertStatus = Nothing
              }

      let targetTabs = fold targetDash.tabs
          firstTabM = viaNonEmpty head targetTabs

      Log.logTrace "Widget duplication"
        $ AE.object
          [ "widgetId" AE..= widgetId
          , "targetDashboardId" AE..= targetDashId
          , "addedToTab" AE..= isJust firstTabM
          , "targetTabCount" AE..= length targetTabs
          ]

      let updatedDash = maybe (targetDash & #widgets %~ (<> [widgetCopy])) (\t -> updateTabBySlug (slugify t.name) (#widgets %~ (<> [widgetCopy])) targetDash) firstTabM
      _ <- Dashboards.updateSchemaAndUpdatedAt targetDashId updatedDash now
      syncDashboardAndQueuePush pid targetDashId

      addWidgetJSON $ Widget.encodeText widgetCopy
      addSuccessToast (maybe "Widget duplicated successfully" ("Widget copied to " <>) targetDashNameM) Nothing
      addRespHeaders widgetCopy


dashboardWidgetExpandGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders (Html ()))
dashboardWidgetExpandGetH pid dashId widgetId = do
  (_, project) <- Projects.sessionAndProject pid
  (_, dash) <- getDashAndVM dashId Nothing
  now <- Time.currentTime
  let timeParams = (Nothing, Nothing, Nothing)
      paramsWithVarDefaults = addVariableDefaults [] dash.variables
  (_, allParamsWithConstants) <- processConstantsAndExtendParams pid now timeParams paramsWithVarDefaults (dashboardQueryText dash) (fold dash.constants)
  widgetToExpand <- (snd <$> findWidgetInDashboard widgetId dash) `whenNothing` throwError err404{errBody = "Widget not found in dashboard"}
  processedWidget <- processWidget pid now timeParams allParamsWithConstants widgetToExpand
  addRespHeaders $ widgetViewerEditor_ pid project.paymentPlan (Just dashId) Nothing Nothing (Just processedWidget) "edit"


-- | SQL preview endpoint for debugging KQL queries (shows generated SQL)
widgetSqlPreviewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
widgetSqlPreviewGetH pid queryM sinceStr fromDStr toDStr = do
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime
  let (fromD, toD, _) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)
      sqlCfg = defSqlQueryCfg pid now Nothing Nothing & #dateRange .~ (fromD, toD)
  addRespHeaders case queryM of
    Nothing -> div_ [class_ "p-3 text-textWeak text-xs"] "No query provided"
    Just query -> case parseQueryToComponents sqlCfg query of
      Left err -> div_ [class_ "p-3 space-y-2"] do
        div_ [class_ "text-textError text-xs font-medium"] "Parse Error"
        pre_ [class_ "whitespace-pre-wrap break-all bg-fillError/10 p-2 rounded text-xs overflow-x-auto"] $ toHtml err
      Right (_, qc) -> div_ [class_ "space-y-3 p-3 bg-fillWeaker rounded-lg text-xs sql-preview-container"] do
        sqlBlock_ "Main Query" qc.finalSqlQuery
        whenJust qc.finalSummarizeQuery $ sqlBlock_ "Summarize Query"
        whenJust qc.finalAlertQuery $ sqlBlock_ "Alert Query"
        script_
          """
          document.querySelectorAll('.sql-preview-container pre code').forEach(el => {
            el.textContent = sqlFormatter.format(el.textContent, { language: 'postgresql' });
            hljs.highlightElement(el);
          });
          """
  where
    sqlBlock_ :: Text -> Text -> Html ()
    sqlBlock_ label sql =
      let sqlEsc = T.replace "`" "\\`" sql
       in div_ [class_ "space-y-1"] do
            div_ [class_ "flex justify-between items-center"] do
              span_ [class_ "text-textWeak font-sans"] $ toHtml label
              button_
                [ class_ "text-textBrand hover:underline font-sans text-xs"
                , term "_" [text| on click writeText(`${sqlEsc}`) to the navigator's clipboard then set my.innerText to 'Copied!' then wait 1.5s then set my.innerText to 'Copy' |]
                ]
                "Copy"
            pre_ [class_ "bg-fillWeak p-2 rounded overflow-x-auto max-h-48"] $ code_ [class_ "language-sql text-xs !bg-transparent"] $ toHtml sql


-- | Find a tab by its slug, returns (index, tab) if found
findTabBySlug :: [Dashboards.Tab] -> Text -> Maybe (Int, Dashboards.Tab)
findTabBySlug tabs tabSlug = find ((== tabSlug) . slugify . (.name) . snd) (zip [0 ..] tabs)


-- | Find widget by ID in dashboard, searching tabs, root, and recursively into children. Returns (Maybe tabSlug, Widget)
findWidgetInDashboard :: Text -> Dashboards.Dashboard -> Maybe (Maybe Text, Widget.Widget)
findWidgetInDashboard wid dash = tabResult <|> rootResult
  where
    -- Recursively search widget and its children
    findInWidget :: Widget.Widget -> Maybe Widget.Widget
    findInWidget w = mfilter (widgetMatches wid) (pure w) <|> asum (foldMap (map findInWidget) w.children)
    tabResult = listToMaybe [(Just $ slugify t.name, w') | t <- fold dash.tabs, w <- t.widgets, w' <- maybeToList (findInWidget w)]
    rootResult = (Nothing,) <$> asum (map findInWidget dash.widgets)


-- | Get the first tab as default if available
getDefaultTabSlug :: Maybe [Dashboards.Tab] -> Maybe Text
getDefaultTabSlug = fmap (slugify . (.name)) . (>>= viaNonEmpty head)


-- | Render breadcrumb suffix with out-of-band swap for htmx
breadcrumbSuffixOob_ :: Monad m => Text -> HtmlT m ()
breadcrumbSuffixOob_ tabName =
  span_ [id_ "pageTitleSuffix", class_ "flex items-center gap-1", hxSwapOob_ "true"] do
    faSprite_ "chevron-right" "regular" "w-3 h-3"
    span_ [class_ "font-normal text-xl p-1 leading-none text-textWeak", id_ "pageTitleSuffixText"] $ toHtml tabName


-- | Internal param key for passing active tab slug between handlers and rendering
activeTabSlugKey :: Text
activeTabSlugKey = "activeTabSlug"


-- | Percent-encoded query string, prefixed with @?@ when non-empty.
-- @[("key", Just "value"), ("empty", Nothing)]@ becomes @"?key=value&empty"@.
queryStringFrom :: [(Text, Maybe Text)] -> Text
queryStringFrom = decodeUtf8 . URI.renderQuery True . map (bimap encodeUtf8 (fmap encodeUtf8))


-- | Add variable defaults to params for any variable not already in params.
-- This ensures constants can reference variables like {{var-resource}} even when not in URL.
addVariableDefaults :: [(Text, Maybe Text)] -> Maybe [Dashboards.Variable] -> [(Text, Maybe Text)]
addVariableDefaults params varsM = params <> defaults
  where
    paramsMap = Map.fromList params
    defaults = [("var-" <> v.key, v.value) | v <- fold varsM, not (Map.member ("var-" <> v.key) paramsMap)]


-- | All query text that can reference a @{{const-…}}@ placeholder, across a
-- dashboard's widgets (incl. children), tab widgets, and variables. Lets us
-- skip constants nothing references — an unused constant otherwise runs its
-- query (e.g. a top_resources GROUP BY full-day scan) on every page load.
dashboardQueryText :: Dashboards.Dashboard -> Text
dashboardQueryText dash =
  T.concat $ concatMap widgetText (dash.widgets <> foldMap (.widgets) (fold dash.tabs)) <> foldMap varText (fold dash.variables)
  where
    widgetText w = catMaybes [w.query, w.sql] <> concatMap widgetText (fold w.children)
    varText v = catMaybes [(.statement) <$> v.sql, v.query]


-- | Process dashboard constants concurrently and build extended params with constant results.
-- Constants whose key is not referenced anywhere in @haystack@ skip their query entirely.
processConstantsAndExtendParams
  :: WidgetData es
  => Projects.ProjectId
  -> UTCTime
  -> (Maybe Text, Maybe Text, Maybe Text)
  -> [(Text, Maybe Text)]
  -> Text
  -> [Dashboards.Constant]
  -> Eff es ([Dashboards.Constant], [(Text, Maybe Text)])
processConstantsAndExtendParams pid now timeParams allParams haystack constants =
  pooledForConcurrently constants processOne <&> \pc ->
    ( pc
    , allParams
        <> [("const-" <> c.key, Just $ constantToSQLList $ fold c.result) | c <- pc]
        <> [("const-" <> c.key <> "-kql", Just $ constantToKQLList $ fold c.result) | c <- pc]
    )
  where
    processOne c
      -- A blown budget leaves the constant result-less, which renders as the same
      -- empty-sentinel params an unreferenced constant produces.
      | ("{{const-" <> c.key) `T.isInfixOf` haystack || ("{{" <> c.key <> "}}") `T.isInfixOf` haystack =
          withRenderBudget ("constant:" <> c.key) c $ processConstant pid now timeParams allParams c
      | otherwise = pure c -- unreferenced: emit empty-sentinel params without running the query


-- | Resolve a dashboard's constants then its variables, returning the enriched
-- dashboard and the params extended with variable defaults and constant results.
resolveDashboardParams :: WidgetData es => Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Dashboards.Dashboard -> Eff es (Dashboards.Dashboard, [(Text, Maybe Text)])
resolveDashboardParams pid now timeParams allParams dash = do
  (constants, params) <- processConstantsAndExtendParams pid now timeParams (addVariableDefaults allParams dash.variables) (dashboardQueryText dash) (fold dash.constants)
  dash' <- processVariablesConcurrently pid now timeParams params (dash & #constants ?~ constants)
  pure (dash', params)


-- | Full render pipeline for a set of widgets: concurrent processing (tagged with
-- the dashboard id), then alert statuses and PNG URLs.
processDashWidgets
  :: Projects.ProjectId
  -> Dashboards.DashboardId
  -> UTCTime
  -> (Maybe Text, Maybe Text, Maybe Text)
  -> [(Text, Maybe Text)]
  -> [Widget.Widget]
  -> ATAuthCtx [Widget.Widget]
processDashWidgets pid dashId now timeParams paramsWithConstants widgets = do
  appCtx <- ask @AuthContext
  pooledForConcurrently widgets (fmap (#_dashboardId ?~ dashId.toText) . processWidget pid now timeParams paramsWithConstants)
    >>= populateWidgetAlertStatuses
    >>= populateWidgetPngUrls appCtx.env.apiKeyEncryptionSecretKey appCtx.config.hostUrl pid timeParams


-- | Handler for dashboard with tab in path: /p/{pid}/dashboards/{dash_id}/tab/{tab_slug}
-- This renders the full page with the specified tab active
dashboardTabGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardTabGetH pid dashId tabSlug fileM fromDStr toDStr sinceStr allParams = do
  (_, project, bw) <- mkPageCtx pid
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)

  (dashVM, dash) <- getDashAndVM dashId fileM

  -- Find the active tab by slug
  let activeTabInfo = dash.tabs >>= (`findTabBySlug` tabSlug)
      activeTabIdx = maybe 0 fst activeTabInfo
      activeTabName = fmap ((.name) . snd) activeTabInfo
      timeParams = (sinceStr, fromDStr, toDStr)

  (dash', allParamsWithConstants) <- resolveDashboardParams pid now timeParams allParams dash

  -- Only process widgets for the ACTIVE tab (lazy loading - other tabs load via htmx).
  -- Note: We don't process dash.widgets here since this is a tab-based dashboard
  dash'' <- forOf (#tabs . _Just) dash' \tabs ->
    forM (zip [0 ..] tabs) \(idx, tab) ->
      if idx == activeTabIdx
        then (\ws -> tab & #widgets .~ ws) <$> processDashWidgets pid dashId now timeParams allParamsWithConstants tab.widgets
        else pure tab

  bwconf <- dashboardBWConf bw pid project.paymentPlan dashId dashVM.title (Just (tabSlug, activeTabName)) currentRange <$> checkFreeTierStatus pid project.paymentPlan
  -- Pass the active tab slug and computed constants in params for rendering
  -- Including constants allows HTMX tab switches to skip re-executing constant queries
  let paramsWithTab = (activeTabSlugKey, Just tabSlug) : allParamsWithConstants
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dashId dash'' dashVM paramsWithTab


-- | Handler for tab content partial (htmx): /p/{pid}/dashboards/{dash_id}/tab/{tab_slug}/content
-- This returns only the tab content panel for htmx swapping
dashboardTabContentGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (Html ()))
dashboardTabContentGetH pid dashId tabSlug fileM fromDStr toDStr sinceStr allParams = do
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime
  (_, dash) <- getDashAndVM dashId fileM
  let timeParams = (sinceStr, fromDStr, toDStr)
      paramsWithVarDefaults = addVariableDefaults allParams dash.variables
      -- Check if constants are already in params (passed from initial page load)
      hasConstants = any (T.isPrefixOf "const-" . fst) allParams

  tabs <- dash.tabs `whenNothing` throwError err404{errBody = "Dashboard has no tabs"}
  (idx, tab) <- findTabBySlug tabs tabSlug `whenNothing` throwError err404{errBody = "Tab not found: " <> encodeUtf8 tabSlug}

  -- Skip constant processing if already provided via params (avoids redundant SQL queries)
  allParamsWithConstants <-
    if hasConstants
      then pure paramsWithVarDefaults
      else snd <$> processConstantsAndExtendParams pid now timeParams paramsWithVarDefaults (dashboardQueryText dash) (fold dash.constants)

  -- Process variables to check if tab requires one that's not set
  dash' <- processVariablesConcurrently pid now timeParams allParamsWithConstants dash
  widgetsWithPngUrls <- processDashWidgets pid dashId now timeParams allParamsWithConstants tab.widgets

  -- Render tab content panel + OOB modal if variable needs prompting + OOB form URL update
  addRespHeaders do
    tabContentPanel_ pid dashId.toText idx tab.name widgetsWithPngUrls True
    whenJust (findVarToPrompt (Just tab) (fold dash'.variables)) \v -> variablePickerModal_ pid dashId (Just tabSlug) allParamsWithConstants v True
    widgetOrderTriggerForm_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/widgets_order?tab=" <> tabSlug) True


-- | Skeleton loader shown while GridStack initializes
dashboardSkeleton_ :: Html ()
dashboardSkeleton_ = div_ [class_ "dashboard-skeleton absolute inset-0 z-10 bg-bgBase flex flex-col items-center justify-center"] do
  loadingIndicatorWith_ LdLG LdSpinner "text-fillBrand-strong"
  p_ [class_ "text-sm text-textWeak mt-3"] "Loading dashboard..."
  div_ [class_ "grid grid-cols-12 max-md:grid-cols-1 gap-4 mt-8 w-full max-w-4xl px-8"] do
    div_ [class_ "col-span-8 max-md:col-span-1 h-32 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 max-md:col-span-1 h-32 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 max-md:col-span-1 h-24 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 max-md:hidden h-24 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 max-md:hidden h-24 rounded-lg skeleton-shimmer"] ""


-- | Render a single tab content panel.
-- isPartial: True for HTMX partial loads (include OOB swap), False for full page loads
tabContentPanel_ :: Projects.ProjectId -> Text -> Int -> Text -> [Widget.Widget] -> Bool -> Html ()
tabContentPanel_ pid dashboardId idx tabName widgets isPartial = do
  when isPartial $ breadcrumbSuffixOob_ tabName
  div_
    [ class_ "tab-panel grid-stack -m-2"
    , data_ "tab-index" (show idx)
    , id_ $ "tab-panel-" <> dashboardId <> "-" <> show idx
    ]
    do
      forM_ widgets \w -> toHtml w{Widget._projectId = Just pid}
      when (null widgets) $ label_ [id_ $ "add_widget_tab_" <> show idx, class_ "grid-stack-item pb-8 cursor-pointer bg-fillBrand-weak border-2 border-strokeBrand-strong border-dashed text-strokeSelected rounded-sm rounded-lg flex flex-col gap-3 items-center justify-center", term "gs-w" "3", term "gs-h" "2", Lucid.for_ "page-data-drawer"] do
        faSprite_ "plus" "regular" "h-8 w-8"
        span_ "Add a widget"


-- | Form for renaming a tab
newtype TabRenameForm = TabRenameForm
  { newName :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Response for tab rename containing new tab name and slug
data TabRenameRes = TabRenameRes
  { newName :: Text
  , newSlug :: Text
  }
  deriving stock (Generic, Show)


instance ToHtml TabRenameRes where
  toHtml res = breadcrumbSuffixOob_ res.newName
  toHtmlRaw = toHtml


-- | Handler for renaming a tab
dashboardTabRenamePatchH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> TabRenameForm -> ATAuthCtx (RespHeaders TabRenameRes)
dashboardTabRenamePatchH pid dashId tabSlug form = do
  _ <- Projects.sessionAndProject pid
  (_, dash) <- getDashAndVM dashId Nothing
  now <- Time.currentTime

  tabs <- dash.tabs `whenNothing` throwError err404{errBody = "Dashboard has no tabs"}
  (idx, _) <- findTabBySlug tabs tabSlug `whenNothing` throwError err404{errBody = "Tab not found: " <> encodeUtf8 tabSlug}

  let newSlug = slugify form.newName
  _ <- Dashboards.updateSchemaAndUpdatedAt dashId (dash & #tabs ?~ (tabs & ix idx . #name .~ form.newName)) now
  syncDashboardAndQueuePush pid dashId

  addSuccessToast "Tab renamed successfully" Nothing
  redirectCS $ "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/tab/" <> newSlug
  addRespHeaders TabRenameRes{newName = form.newName, newSlug}


-- | Unified dashboard actions (add widget button, yaml drawer, context menu)
dashboardActions_ :: Projects.ProjectId -> Text -> Dashboards.DashboardId -> Maybe Text -> Maybe (Text, Text) -> Html ()
dashboardActions_ pid paymentPlan dashId tabSlugM currentRange = div_ [class_ "flex items-center"] do
  span_ [class_ "text-fillDisabled mr-2 max-md:hidden"] "|"
  div_ [class_ "max-md:hidden"] $ Components.drawer_ "page-data-drawer" False Nothing (Just $ widgetViewerEditor_ pid paymentPlan (Just dashId) tabSlugM currentRange Nothing "edit") $ span_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Add a new widget", data_ "tippy-content" "Add a new widget"] $ faSprite_ "plus" "regular" "w-3 h-3"
  div_ [class_ "max-md:hidden"] $ yamlEditorDrawer_ pid dashId
  let dashActionsPop = "dash-actions-" <> dashId.toText
  div_ [class_ "inline-block"] do
    button_ ([type_ "button", class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Open context menu", data_ "tippy-content" "Context Menu"] <> popoverTrigger_ dashActionsPop) $ faSprite_ "ellipsis" "regular" "w-4 h-4"
    ul_ ([class_ "dropdown dropdown-end menu menu-md bg-bgRaised rounded-box border border-strokeWeak p-2 w-52 shadow-lg leading-none"] <> popoverPanel_ dashActionsPop) do
      li_ $ label_ [Lucid.for_ "pageTitleModalId", class_ "p-2"] "Rename dashboard"
      when (isJust tabSlugM) $ li_ $ label_ [Lucid.for_ "tabRenameModalId", class_ "p-2"] "Rename tab"
      li_ $ button_ [class_ "p-2 w-full text-left", hxPost_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/duplicate"), hxSwap_ "none", data_ "tippy-content" "Creates a copy of this dashboard"] "Duplicate dashboard"
      li_ $ label_ [Lucid.for_ "yaml-editor-drawer", class_ "p-2", data_ "tippy-content" "View and edit the dashboard schema as YAML"] "Edit YAML"
      li_ $ button_ [class_ "p-2 w-full text-left text-textError", hxDelete_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText), hxSwap_ "none", hxConfirm_ "Are you sure you want to delete this dashboard? This action cannot be undone.", data_ "tippy-content" "Permanently deletes this dashboard"] "Delete dashboard"


-- | YAML Editor Drawer component
yamlEditorDrawer_ :: Projects.ProjectId -> Dashboards.DashboardId -> Html ()
yamlEditorDrawer_ pid dashId = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  let drawerId = "yaml-editor-drawer"
      yamlUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/yaml"
  input_ [id_ drawerId, type_ "checkbox", class_ "drawer-toggle", [__|on keyup if the event's key is 'Escape' set my.checked to false end on closeYamlDrawer from window set my.checked to false|]]
  div_ [class_ "drawer-side top-0 left-0 w-full h-full flex z-10000 overflow-hidden"] do
    label_ [Lucid.for_ drawerId, class_ "drawer-overlay w-full grow flex-1"] ""
    div_ [style_ "width: min(90vw, 1000px)", class_ "bg-bgRaised h-full overflow-hidden flex flex-col"] do
      div_ [class_ "flex justify-between items-center p-4 border-b border-strokeWeak shrink-0"] do
        h2_ [class_ "text-lg font-semibold"] "Edit Dashboard Schema"
        div_ [class_ "flex items-center gap-2"] do
          label_ [class_ "btn btn-outline btn-sm cursor-pointer", Lucid.for_ "yaml-import-input"] do
            faSprite_ "upload" "regular" "w-3 h-3 mr-1"
            "Import"
          input_ [id_ "yaml-import-input", type_ "file", accept_ ".yaml,.yml", class_ "hidden", [__|on change call yamlEditorImport(me.files[0]) then set my.value to ''|]]
          button_ [class_ "btn btn-outline btn-sm", [__|on click call yamlEditorExport()|]] do
            faSprite_ "download" "regular" "w-3 h-3 mr-1"
            "Export"
          label_ [class_ "btn btn-ghost btn-sm", Aria.label_ "Close YAML editor", Lucid.for_ drawerId] $ faSprite_ "xmark" "regular" "w-3 h-3"
      div_ [class_ "flex-1 overflow-hidden", id_ "yaml-editor-wrapper", hxGet_ yamlUrl, hxTrigger_ "intersect once", hxTarget_ "#yaml-editor-content"] do
        div_ [id_ "yaml-editor-content", class_ "h-full flex items-center justify-center"] $ loadingIndicator_ LdMD LdDots
      div_ [class_ "p-4 border-t border-strokeWeak flex justify-between items-center shrink-0"] do
        div_ [id_ "yaml-status", class_ "text-sm"] ""
        button_ [class_ "btn btn-primary", hxPut_ yamlUrl, hxTarget_ "#yaml-status", hxVals_ "js:{yaml: window.yamlEditor?.getValue() || ''}"] "Save Changes"


-- | Form for YAML schema editing
newtype YamlForm = YamlForm {yaml :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Get dashboard schema as YAML (returns HTML with yaml-editor component)
dashboardYamlGetH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders (Html ()))
dashboardYamlGetH pid dashId = do
  _ <- Projects.sessionAndProject pid
  (dashVM, dash) <- getDashAndVM dashId Nothing
  teams <- ManageMembers.getTeamsById pid dashVM.teams
  let schema = GitSync.buildSchemaWithMeta (Just dash) dashVM.title (V.toList dashVM.tags) (map (.handle) teams)
      yamlText = decodeUtf8 $ GitSync.dashboardToYaml schema
  addRespHeaders $ yamlEditorContent_ yamlText


-- | Render the yaml-editor component with initial content
yamlEditorContent_ :: Text -> Html ()
yamlEditorContent_ yamlText = term "yaml-editor" [class_ "h-full w-full block", id_ "yaml-editor-instance", data_ "initial-value" yamlText] ""


-- | Save dashboard schema from YAML (validates and saves)
dashboardYamlPutH :: Projects.ProjectId -> Dashboards.DashboardId -> YamlForm -> ATAuthCtx (RespHeaders (Html ()))
dashboardYamlPutH pid dashId form = do
  _ <- Projects.sessionAndProject pid
  case GitSync.yamlToDashboard (encodeUtf8 form.yaml) of
    Left err -> addRespHeaders $ yamlValidationError_ err
    Right dashboard -> do
      now <- Time.currentTime
      _ <- Dashboards.updateSchemaAndUpdatedAt dashId dashboard now
      whenJust dashboard.title $ \t -> void $ Dashboards.updateTitle dashId t
      syncDashboardAndQueuePush pid dashId
      addSuccessToast "Dashboard schema updated" Nothing
      addTriggerEvent "closeYamlDrawer" ""
      redirectCS $ "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText
      addRespHeaders $ yamlValidationSuccess_ dashboard


-- | Render validation error HTML
yamlValidationError_ :: Text -> Html ()
yamlValidationError_ err = div_ [id_ "yaml-status", class_ "text-textError"] do
  div_ [class_ "flex items-center gap-2 font-semibold mb-2"] do
    faSprite_ "circle-exclamation" "solid" "w-4 h-4"
    "Invalid YAML"
  pre_ [class_ "text-xs bg-fillError-weak p-3 rounded overflow-x-auto whitespace-pre-wrap"] $ toHtml err


-- | Render validation success HTML with schema summary
yamlValidationSuccess_ :: Dashboards.Dashboard -> Html ()
yamlValidationSuccess_ dash = div_ [id_ "yaml-status", class_ "text-textBrand"] do
  div_ [class_ "flex items-center gap-2"] do
    faSprite_ "circle-check" "solid" "w-4 h-4"
    "Schema saved successfully"
  ul_ [class_ "text-xs text-textWeak mt-2 list-disc pl-5"] do
    li_ $ toHtml $ show (length dash.widgets) <> " widgets"
    whenJust dash.variables $ \vs -> li_ $ toHtml $ show (length vs) <> " variables"
    whenJust dash.tabs $ \ts -> li_ $ toHtml $ show (length ts) <> " tabs"
    whenJust dash.constants $ \cs -> li_ $ toHtml $ show (length cs) <> " constants"
