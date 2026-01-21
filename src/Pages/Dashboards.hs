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
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as Wreq
import Data.Generics.Labels ()
import Data.HashMap.Lazy qualified as HM
import Data.HashMap.Lazy qualified as M
import Data.List qualified as L
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (pooledForConcurrently)
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxConfirm_, hxDelete_, hxExt_, hxGet_, hxIndicator_, hxPatch_, hxPost_, hxPushUrl_, hxPut_, hxSelect_, hxSwapOob_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.RequestDumps qualified as RequestDumps
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
import Pages.Monitors qualified as Alerts
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..), logQueryBox_, visTypes)
import Pkg.Components.Table (BulkAction (..), Table (..))
import Pkg.Components.Table qualified as Table
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DashboardUtils qualified as DashboardUtils
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Parser (QueryComponents (..), SqlQueryCfg (..), defSqlQueryCfg, finalAlertQuery, fixedUTCTime, parseQueryToComponents, presetRollup)
import Pkg.THUtils (hashAssetFile)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import Servant (NoContent (..), ServerError, err302, err404, errBody, errHeaders)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Config (AuthContext (..))
import System.FilePath.Posix (takeDirectory)
import System.Logging qualified as Log
import System.Types
import Text.Slugify (slugify)
import UnliftIO.Exception (try)
import Utils
import Web.FormUrlEncoded (FromForm)


-- | Head content for dashboard pages - loads highlight.js and sql-formatter for SQL preview
dashboardHeadContent_ :: Html ()
dashboardHeadContent_ = do
  link_ [rel_ "stylesheet", href_ $(hashAssetFile "/public/assets/deps/highlightjs/atom-one-dark.min.css")]
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/highlight.min.js")] ("" :: Text)
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/sql.min.js")] ("" :: Text)
  script_ [src_ $(hashAssetFile "/public/assets/deps/highlightjs/sql-formatter.min.js")] ("" :: Text)


folderFromPath :: Maybe Text -> Text
folderFromPath Nothing = ""
folderFromPath (Just path) = let dir = takeDirectory (toString path) in if dir == "." then "" else toText dir <> "/"


normalizeWidgetId :: Text -> Text
normalizeWidgetId = T.replace "Expanded" ""


dashTitle :: Text -> Text
dashTitle "" = "Untitled"
dashTitle t = t


-- | Sync file_path and file_sha for a dashboard after any update.
-- Only recomputes SHA when the schema content has actually changed.
-- Skips template-based dashboards (schema = Nothing) since they have no custom content to sync.
syncDashboardFileInfo :: DB es => Dashboards.DashboardId -> Eff es ()
syncDashboardFileInfo dashId = do
  dashM <- Dashboards.getDashboardById dashId
  forM_ dashM \dash -> forM_ dash.schema \_ -> do
    teams <- ManageMembers.getTeamsById dash.projectId dash.teams
    let schema = GitSync.buildSchemaWithMeta dash.schema dash.title (V.toList dash.tags) (map (.handle) teams)
        existingDir = folderFromPath dash.filePath
        filePath = existingDir <> GitSync.titleToFilePath dash.title
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
  -- Modal for renaming dashboard
  Components.modal_ "pageTitleModalId" ""
    $ form_
      [ class_ "flex flex-col p-3 gap-3"
      , hxPatch_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/rename")
      , hxSwap_ "innerHTML"
      , hxTrigger_ "submit"
      , hxTarget_ "#pageTitleText"
      ]
    $ fieldset_ [class_ "fieldset min-w-xs"] do
      label_ [class_ "label"] "Dashboard Title"
      input_ [class_ "input", name_ "title", placeholder_ "Insert new title", value_ $ dashTitle dashVM.title]
      label_ [class_ "label mt-2"] "Folder"
      input_ [class_ "input font-mono text-sm", name_ "fileDir", placeholder_ "reports/", value_ $ folderFromPath dashVM.filePath]
      div_ [class_ "mt-3 flex justify-end gap-2"] do
        label_ [Lucid.for_ "pageTitleModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
        button_ [type_ "submit", class_ "btn btn-primary"] "Save"

  -- Modal for renaming tab (only shown for dashboards with tabs)
  whenJust dash.tabs \tabs -> do
    let activeTabSlug = join (L.lookup activeTabSlugKey allParams)
        activeTabInfo = activeTabSlug >>= findTabBySlug tabs
        activeTabName = maybe "" ((.name) . snd) activeTabInfo
        currentTabSlug = fromMaybe "" activeTabSlug
    Components.modal_ "tabRenameModalId" ""
      $ form_
        [ class_ "flex flex-col p-3 gap-3"
        , hxPatch_ ("/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/tab/" <> currentTabSlug <> "/rename")
        , hxSwap_ "none"
        , hxTrigger_ "submit"
        ]
      $ fieldset_ [class_ "fieldset min-w-xs"] do
        label_ [class_ "label"] "Tab Name"
        input_ [class_ "input", name_ "newName", placeholder_ "Enter tab name", value_ activeTabName]
        div_ [class_ "mt-3 flex justify-end gap-2"] do
          label_ [Lucid.for_ "tabRenameModalId", class_ "btn btn-outline cursor-pointer"] "Cancel"
          button_ [type_ "submit", class_ "btn btn-primary"] "Save"

  -- Variable picker modal - auto-opens when required vars are unset (from tab.requires or variable.required)
  whenJust dash.variables \variables -> do
    let activeTabSlug' = join (L.lookup activeTabSlugKey allParams)
        activeTab = activeTabSlug' >>= \slug -> dash.tabs >>= (`findTabBySlug` slug) <&> snd
    whenJust (findVarToPrompt activeTab variables) \v -> variablePickerModal_ pid dashId activeTabSlug' allParams v False

  -- Render variables and tabs in the same container
  when (isJust dash.variables || isJust dash.tabs) $ div_ [class_ "flex bg-fillWeaker px-4 py-2 gap-4 items-center flex-wrap"] do
    -- Tabs section (on the left) - now using htmx for lazy loading
    whenJust dash.tabs \tabs -> do
      -- Get active tab from path-based slug or fall back to first tab
      let activeTabSlug = join (L.lookup activeTabSlugKey allParams)
          activeTabIdx = case activeTabSlug of
            Just slug -> maybe 0 fst (findTabBySlug tabs slug)
            Nothing -> 0
          baseTabUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/tab/"
          -- Build query string from current params (excluding activeTabSlugKey which is internal)
          queryStr = queryStringFrom $ filter (\(k, _) -> k /= activeTabSlugKey) allParams
      div_ [role_ "tablist", class_ "tabs tabs-box tabs-outline", id_ "dashboard-tabs-container"] do
        forM_ (zip [0 ..] tabs) \(idx, tab) -> do
          let tabSlug = slugify tab.name
              isActive = idx == activeTabIdx
              tabUrl = baseTabUrl <> tabSlug
              tabContentUrl = tabUrl <> "/content" <> queryStr
          a_
            [ role_ "tab"
            , href_ $ tabUrl <> queryStr
            , class_ $ "tab flex items-center gap-2" <> if isActive then " tab-active" else ""
            , id_ $ "tab-link-" <> dashId.toText <> "-" <> show idx
            , hxGet_ tabContentUrl
            , hxTarget_ "#dashboard-tabs-content"
            , hxSwap_ "innerHTML transition:true"
            , hxPushUrl_ $ tabUrl <> queryStr
            , hxIndicator_ $ "#tab-indicator-" <> dashId.toText <> "-" <> show idx
            , -- Update active tab styling via htmx hyperscript
              term "_" "on htmx:afterOnLoad remove .tab-active from .tab in #dashboard-tabs-container then add .tab-active to me"
            ]
            do
              whenJust tab.icon \icon -> faSprite_ icon "regular" "w-4 h-4"
              toHtml tab.name
              span_ [class_ "htmx-indicator", id_ $ "tab-indicator-" <> dashId.toText <> "-" <> show idx] $ faSprite_ "spinner" "regular" "w-3 h-3 animate-spin"

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
  // Interpolate {{var-*}} placeholders in elements with data-var-template attribute
  (function() {
    let cachedSearch = '', cachedParams = null, pending = false;
    window.interpolateVarTemplates = function() {
      if (pending) return;
      pending = true;
      requestAnimationFrame(() => {
        pending = false;
        if (window.location.search !== cachedSearch) {
          cachedSearch = window.location.search;
          cachedParams = new URLSearchParams(cachedSearch);
        }
        document.querySelectorAll('[data-var-template]').forEach(el => {
          let text = el.dataset.varTemplate;
          cachedParams.forEach((value, key) => {
            if (key.startsWith('var-')) text = text.replaceAll('{{' + key + '}}', value || '');
          });
          el.textContent = text;
        });
      });
    };
  })();

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

      window.interpolateVarTemplates();
    });

    window.interpolateVarTemplates();
  });
    |]
  let activeTabSlug = dash.tabs >>= \tabs -> join (L.lookup activeTabSlugKey allParams) <|> (slugify . (.name) <$> listToMaybe tabs)
      widgetOrderUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/widgets_order" <> maybe "" ("?tab=" <>) activeTabSlug
      constantsJson = decodeUtf8 $ AE.encode $ M.fromList [(k, fromMaybe "" v) | (k, v) <- allParams, "const-" `T.isPrefixOf` k]

  section_ [class_ "h-full"] $ div_ [class_ "mx-auto mb-20 pt-2 pb-6 px-4 gap-3.5 w-full flex flex-col h-full overflow-y-scroll pb-20 group/pg", id_ "dashboardPage", data_ "constants" constantsJson] do
    let emptyConstants = [c.key | c <- fromMaybe [] dash.constants, c.result `elem` [Nothing, Just []]]
    unless (null emptyConstants) $ div_ [class_ "alert alert-warning text-sm"] do
      faSprite_ "circle-exclamation" "regular" "w-4 h-4"
      span_ $ toHtml $ "Constants with no data: " <> T.intercalate ", " emptyConstants
    div_ [class_ "dashboard-grid-wrapper relative min-h-[400px]"] do
      dashboardSkeleton_
      case dash.tabs of
        Just tabs -> do
          let activeTabIdx = case activeTabSlug of
                Just slug -> maybe 0 fst (findTabBySlug tabs slug)
                Nothing -> 0
          -- Tab system with htmx lazy loading - only render active tab content
          div_ [class_ "dashboard-tabs-container", id_ "dashboard-tabs-content"] do
            -- Only render the active tab's content (other tabs load via htmx)
            case tabs !!? activeTabIdx of
              Just activeTab -> tabContentPanel_ pid dashId.toText activeTabIdx activeTab.name activeTab.widgets True False
              Nothing -> pass
        Nothing -> do
          -- Fall back to old behavior for dashboards without tabs
          div_
            [class_ "grid-stack -m-2"]
            do
              forM_ (dash :: Dashboards.Dashboard).widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
              when (null (dash :: Dashboards.Dashboard).widgets) $ label_ [id_ "add_a_widget_label", class_ "grid-stack-item pb-8 cursor-pointer bg-fillBrand-weak border-2 border-strokeBrand-strong border-dashed text-strokeSelected rounded-sm rounded-lg flex flex-col gap-3 items-center justify-center *:right-0!  *:bottom-0! ", term "gs-w" "3", term "gs-h" "2", Lucid.for_ "page-data-drawer"] do
                faSprite_ "plus" "regular" "h-8 w-8"
                span_ "Add a widget"

    -- Add hidden element for the auto-refresh handler
    div_ [id_ "dashboard-refresh-handler", class_ "hidden"] ""

    -- Hidden form for widget order PATCH via HTMX (tab slug hardcoded in URL)
    form_
      [ id_ "widget-order-trigger"
      , class_ "hidden"
      , hxPatch_ widgetOrderUrl
      , hxVals_ "js:{...buildWidgetOrder(document.querySelector('.grid-stack'))}"
      , hxExt_ "json-enc"
      , hxSwap_ "none"
      , hxTrigger_ "widget-order-changed from:body"
      ]
      ""

    script_
      [text|
      document.addEventListener('DOMContentLoaded', () => {
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
                  acceptWidgets: true,
                  cellHeight: '5rem',
                  margin: '1rem 0.5rem 1rem 0.5rem',
                  handleClass: 'grid-stack-handle',
                  styleInHead: true,
                  staticGrid: false,
                  float: false,
                  animate: true,
                }, gridEl);

                grid.on('removed change', debounce(() => {
                  const collapsingWidget = gridEl.querySelector('[data-collapse-action]');
                  if (collapsingWidget) { delete collapsingWidget.dataset.collapseAction; return; }
                  htmx.trigger(document.body, 'widget-order-changed');
                }, 500));
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
                margin: '1rem 0.5rem 1rem 0.5rem',
                handleClass: 'nested-grid-stack-handle',
                styleInHead: true,
                staticGrid: false,
                animate: true,
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
              nestedInstance.on('removed change', debounce(() => {
                const collapsingWidget = nestedEl.closest('[data-collapse-action]');
                if (collapsingWidget) { delete collapsingWidget.dataset.collapseAction; return; }
                htmx.trigger(document.body, 'widget-order-changed');
              }, 500));

              nestedEl.classList.add('grid-stack-initialized');
            }
          });
        }

        // Initialize grids on page load
        initializeGrids();

        // Re-initialize grids after htmx settles new tab content
        document.body.addEventListener('htmx:afterSettle', function(e) {
          if (e.detail.target && e.detail.target.id === 'dashboard-tabs-content') {
            initializeGrids();
            window.interpolateVarTemplates();
          }
        });
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
      -- SECURITY: Use secured query execution with project_id filtering
      result <- RequestDumps.executeSecuredQuery pid sqlQuery 1000
      case result of
        Right queryResults -> pure variable{Dashboards.options = Just $ map (map valueToText . V.toList) $ V.toList queryResults}
        Left _ -> pure variable -- Return unchanged on error
    _ -> pure variable
  where
    valueToText :: AE.Value -> Text
    valueToText (AE.String t) = t
    valueToText (AE.Number n) = show n
    valueToText (AE.Bool b) = if b then "true" else "false"
    valueToText AE.Null = ""
    valueToText v = decodeUtf8 $ AE.encode v


-- | Get required variables without values (respecting dependsOn order)
unsetRequiredVars :: [Dashboards.Variable] -> [Dashboards.Variable]
unsetRequiredVars vars = filter shouldShow vars
  where
    setKeys = [v.key | v <- vars, isJust v.value]
    shouldShow v = v.required == Just True && isNothing v.value && maybe True (`elem` setKeys) v.dependsOn


-- | Find variable that needs to be prompted (from tab.requires or variable.required)
findVarToPrompt :: Maybe Dashboards.Tab -> [Dashboards.Variable] -> Maybe Dashboards.Variable
findVarToPrompt activeTab variables =
  let tabRequiredVar = activeTab >>= (.requires) >>= \reqKey -> find (\v -> v.key == reqKey && isNothing v.value) variables
   in tabRequiredVar <|> listToMaybe (unsetRequiredVars variables)


-- | Render variable picker modal (auto-opens via hyperscript init)
variablePickerModal_ :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> [(Text, Maybe Text)] -> Dashboards.Variable -> Bool -> Html ()
variablePickerModal_ pid dashId activeTabSlug allParams var useOob = do
  let modalId = "varPicker-" <> var.key
      varTitle = fromMaybe var.key var.title
      queryBase = queryStringFrom $ filter (\(k, _) -> k /= "var-" <> var.key && k /= activeTabSlugKey) allParams
      tabPath = maybe "" ("/tab/" <>) activeTabSlug
      oobAttr = if useOob then [id_ $ modalId <> "-container", hxSwapOob_ "beforeend:body"] else []
  div_ oobAttr do
    input_ [class_ "modal-toggle", id_ modalId, type_ "checkbox", [__|init set my.checked to true on keyup if event's key is 'Escape' set my.checked to false|]]
    div_ [class_ "modal w-screen", role_ "dialog"] do
      label_ [class_ "modal-backdrop", Lucid.for_ modalId] ""
      div_ [class_ "modal-box min-w-80 max-w-md flex flex-col gap-4"] do
        Components.modalCloseButton_ modalId
        h3_ [class_ "font-bold text-lg"] $ toHtml $ "Select " <> varTitle
        p_ [class_ "text-sm text-textWeak"] $ toHtml $ "This view requires a " <> varTitle <> " to be selected."
        whenJust var.helpText $ p_ [class_ "text-sm text-textWeak italic"] . toHtml
        input_
          [ type_ "text"
          , class_ "input input-bordered w-full"
          , placeholder_ "Search..."
          , [__|on keyup if event's key is 'Escape' set my value to '' then trigger keyup else show <.var-opt/> in closest .modal-box when its textContent.toLowerCase() contains my value.toLowerCase()|]
          ]
        div_ [class_ "max-h-64 overflow-y-auto flex flex-col gap-1"]
          $ forM_ (fromMaybe [] var.options) \opt -> do
            let optVal = opt Unsafe.!! 0
                optLbl = fromMaybe optVal (opt !!? 1)
                url = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> tabPath <> queryBase <> (if T.null queryBase then "?" else "&") <> "var-" <> var.key <> "=" <> optVal
            a_ [class_ "var-opt p-2 rounded hover:bg-fillWeak cursor-pointer", href_ url] $ toHtml optLbl


-- | Process a single dashboard constant by executing its SQL or KQL query and populating the result.
-- Constants are executed once and their results are made available to all widgets.
processConstant :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Dashboards.Constant -> ATAuthCtx Dashboards.Constant
processConstant pid now (sinceStr, fromDStr, toDStr) allParams constantBase = do
  let (fromD, toD, _) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)
      constant = Dashboards.replaceConstantVariables pid fromD toD allParams now constantBase
      runQuery :: forall a. Text -> ATAuthCtx a -> (a -> [[Text]]) -> ATAuthCtx Dashboards.Constant
      runQuery label action toResult = do
        (res, duration) <- Log.timeAction $ try action
        either
          (\(err :: SomeException) -> Log.logWarn ("Dashboard constant " <> label <> " query failed") (constant.key, show err, duration) $> constant)
          (\val -> Log.logDebug ("Dashboard constant " <> label <> " query completed") (constant.key, duration) $> constant{Dashboards.result = Just $ toResult val})
          res
  case (constant.sql, constant.query) of
    (Just sqlQuery, _) -> do
      -- SECURITY: Use secured query execution with project_id filtering
      (res, duration) <- Log.timeAction $ RequestDumps.executeSecuredQuery pid sqlQuery 1000
      case res of
        Left err -> Log.logWarn "Dashboard constant SQL query failed" (constant.key, err, duration) $> constant
        Right queryResults -> do
          let toTextList = map (map valueToText . V.toList) $ V.toList queryResults
          Log.logDebug "Dashboard constant SQL query completed" (constant.key, duration) $> constant{Dashboards.result = Just toTextList}
    (Nothing, Just kqlQuery) -> runQuery "KQL" (Charts.queryMetrics (Just Charts.DTText) (Just pid) (Just kqlQuery) Nothing sinceStr fromDStr toDStr Nothing allParams) (map V.toList . V.toList . (.dataText))
    _ -> pure constant
  where
    valueToText :: AE.Value -> Text
    valueToText (AE.String t) = t
    valueToText (AE.Number n) = show n
    valueToText (AE.Bool b) = if b then "true" else "false"
    valueToText AE.Null = ""
    valueToText v = decodeUtf8 $ AE.encode v


-- Process a single widget recursively. Keeps sql/query with {{var-*}} templates intact
-- so they can be interpolated at data fetch time with current URL params.
processWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processWidget pid now timeRange allParams widgetBase = do
  let widget = widgetBase & #_projectId %~ (<|> Just pid) & #rawQuery .~ widgetBase.query

  widget' <-
    if widget.eager == Just True || widget.wType == Widget.WTAnomalies
      then processEagerWidget pid now timeRange allParams widget
      else pure widget

  -- Recursively process child widgets concurrently
  case widget'.children of
    Nothing -> pure widget'
    Just childWidgets -> do
      let addDashboardId child = child & #_dashboardId %~ (<|> widget'._dashboardId)
      processedChildren <- pooledForConcurrently childWidgets (processWidget pid now timeRange allParams . addDashboardId)
      pure $ widget' & #children ?~ processedChildren


processEagerWidget :: Projects.ProjectId -> UTCTime -> (Maybe Text, Maybe Text, Maybe Text) -> [(Text, Maybe Text)] -> Widget.Widget -> ATAuthCtx Widget.Widget
processEagerWidget pid now (sinceStr, fromDStr, toDStr) allParams widget = case widget.wType of
  Widget.WTAnomalies -> do
    (issues, _) <- Issues.selectIssues pid Nothing (Just False) (Just False) 2 0 Nothing Nothing
    let issuesVM = V.fromList $ map (AnomalyList.IssueVM False True now "24h") issues
    pure
      $ widget
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


dashboardWidgetPutH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Widget.Widget -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardWidgetPutH pid dashId widgetIdM tabSlugM widget = do
  (_, dash) <- getDashAndVM dashId Nothing
  uid <- UUID.genUUID <&> UUID.toText
  let normalizedWidgetIdM = normalizeWidgetId <$> widgetIdM
  let widgetUpdated = normalizeWidget widget normalizedWidgetIdM uid
  let dash' = updateDashboardWidgets dash tabSlugM normalizedWidgetIdM widgetUpdated

  _ <- Dashboards.updateSchema dashId dash'
  syncDashboardAndQueuePush pid dashId
  whenJust normalizedWidgetIdM \nwid -> syncWidgetAlert pid nwid widget

  let successMsg = if isJust normalizedWidgetIdM then "Widget updated successfully" else "Widget added to dashboard successfully"
  addSuccessToast successMsg Nothing
  addTriggerEvent "closeModal" ""
  addRespHeaders widgetUpdated


normalizeWidget :: Widget.Widget -> Maybe Text -> Text -> Widget.Widget
normalizeWidget widget normalizedWidgetIdM generatedId = case normalizedWidgetIdM of
  Just nwid -> widget{Widget.standalone = Nothing, Widget.naked = Nothing, Widget.id = Just nwid}
  Nothing -> widget{Widget.standalone = Nothing, Widget.naked = Nothing, Widget.id = Just generatedId, Widget._centerTitle = Nothing}


updateDashboardWidgets :: Dashboards.Dashboard -> Maybe Text -> Maybe Text -> Widget.Widget -> Dashboards.Dashboard
updateDashboardWidgets dash tabSlugM normalizedWidgetIdM widgetUpdated =
  let updateWidgets ws = case normalizedWidgetIdM of
        Just nwid ->
          let updateWidget w =
                if (w.id == Just nwid) || (maybeToMonoid (slugify <$> w.title) == nwid)
                  then mergeWidgetPreservingQuery w widgetUpdated
                  else w
           in map updateWidget ws
        Nothing -> ws <> [widgetUpdated]
   in case (tabSlugM, dash.tabs) of
        (Just slug, Just _) -> updateTabBySlug slug (#widgets %~ updateWidgets) dash
        _ -> dash & #widgets %~ updateWidgets


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
dashboardWidgetReorderPatchH _ _ _ widgetOrder | Map.null widgetOrder = addRespHeaders NoContent
dashboardWidgetReorderPatchH pid dashId tabSlugM widgetOrder = do
  (_, dash) <- getDashAndVM dashId Nothing

  let oldWidgets = case (tabSlugM, dash.tabs) of
        (Just slug, Just tabs) -> maybe [] (.widgets) $ find (\t -> slugify t.name == slug) tabs
        _ -> dash.widgets
      oldWidgetIds = mapMaybe (.id) oldWidgets
      newWidgetIds = Map.keys widgetOrder
      deletedWidgetIds = filter (`notElem` newWidgetIds) oldWidgetIds

  -- Delete alerts for removed widgets first (before updating dashboard to avoid orphaned monitors)
  unless (null deletedWidgetIds) $ void $ Monitors.deleteMonitorsByWidgetIds deletedWidgetIds

  let newDash = case (tabSlugM, dash.tabs) of
        (Just slug, Just _) -> updateTabBySlug slug (\tab -> tab & #widgets .~ reorderWidgets widgetOrder tab.widgets) dash
        _ -> dash & #widgets .~ reorderWidgets widgetOrder dash.widgets

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
    flatten w = (widgetId w, w) : maybe [] (concatMap flatten) w.children
    widgetId w = fromMaybe (maybeToMonoid $ slugify <$> w.title) w.id


updateTabBySlug :: Text -> (Dashboards.Tab -> Dashboards.Tab) -> Dashboards.Dashboard -> Dashboards.Dashboard
updateTabBySlug slug f dash = dash & #tabs %~ fmap (map updateTab)
  where
    updateTab tab = if slugify tab.name == slug then f tab else tab


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

  -- If the dashboard has tabs, redirect to the first tab's URL
  -- This ensures users always land on a tab-based URL for dashboards with tabs
  case getDefaultTabSlug dash.tabs of
    Just firstTabSlug -> do
      let redirectUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/tab/" <> firstTabSlug <> queryStringFrom allParams
      throwError $ err302{errHeaders = [("Location", encodeUtf8 redirectUrl)]}
    Nothing -> do
      -- No tabs - render the dashboard normally (existing behavior for non-tabbed dashboards)
      let timeParams = (sinceStr, fromDStr, toDStr)
          paramsWithVarDefaults = addVariableDefaults allParams dash.variables
      (processedConstants, allParamsWithConstants) <- processConstantsAndExtendParams pid now timeParams paramsWithVarDefaults (fromMaybe [] dash.constants)

      let dashWithConstants = dash & #constants ?~ processedConstants
          processWidgetWithDashboardId = mkWidgetProcessor pid dashId now timeParams allParamsWithConstants

      dash' <- forOf (#variables . traverse . traverse) dashWithConstants (processVariable pid now timeParams allParamsWithConstants)
      -- Process widgets concurrently
      processedWidgets <- pooledForConcurrently dash'.widgets processWidgetWithDashboardId
      -- Populate alert statuses for widgets
      widgetsWithAlerts <- populateWidgetAlertStatuses processedWidgets
      let dash'' = dash' & #widgets .~ widgetsWithAlerts

      freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = Just project
              , prePageTitle = Just "Dashboards"
              , pageTitle = dashTitle dashVM.title
              , pageTitleModalId = Just "pageTitleModalId"
              , config = appCtx.config
              , freeTierExceeded = freeTierExceeded
              , headContent = Just dashboardHeadContent_
              , pageActions = Just $ div_ [class_ "flex gap-3 items-center"] do
                  TimePicker.timepicker_ Nothing currentRange Nothing
                  TimePicker.refreshButton_
                  dashboardActions_ pid dashId Nothing currentRange
              , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/dashboard/"
              }
      addRespHeaders $ PageCtx bwconf $ DashboardGet pid dashId dash'' dashVM allParams


-- | A unified widget viewer/editor component that uses DaisyUI tabs without JavaScript
-- @param pid Project ID
-- @param dashboardIdM Optional dashboard ID
-- @param tabSlugM Optional tab slug for tabbed dashboards
-- @param currentRange Time range for the widget
-- @param existingWidgetM Optional existing widget (for edit mode)
-- @param activeTab Which tab should be active initially ("edit" or "overview")
widgetViewerEditor_ :: Projects.ProjectId -> Maybe Dashboards.DashboardId -> Maybe Text -> Maybe (Text, Text) -> Maybe Widget.Widget -> Text -> Html ()
widgetViewerEditor_ pid dashboardIdM tabSlugM currentRange existingWidgetM activeTab = div_ [class_ "group/wgtexp"] do
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
      sourceWid = normalizeWidgetId wid
      widPrefix = "id" <> T.take 8 wid
      widgetFormId = widPrefix <> "-widget-form"
      widgetPreviewId = widPrefix <> "-widget-preview"
      widgetTitleInputId = widPrefix <> "-widget-title-input"
      -- queryBuilderId = widPrefix <> "-queryBuilder" -- removed unused variable
      -- filterElementId = widPrefix <> "-filterElement" -- removed unused variable
      -- widgetTypeNameId = widPrefix <> "-widgetType" -- removed unused variable
      drawerStateCheckbox = if isJust existingWidgetM then "global-data-drawer" else "page-data-drawer"

  let widgetJSON = decodeUtf8 $ fromLazy $ AE.encode widgetToUse
  let formAction = case dashboardIdM of
        Just dashId ->
          let baseUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText
              params = catMaybes [("widget_id=" <>) . fromMaybe "" . (.id) <$> existingWidgetM, ("tab=" <>) <$> tabSlugM]
           in baseUrl <> if null params then "" else "?" <> T.intercalate "&" params
        Nothing -> ""

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

  div_ [class_ "flex justify-between items-center mb-6"] do
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
          mkTab "Overview" (effectiveActiveTab /= "edit" && effectiveActiveTab /= "alerts")
          mkTab "Edit" (effectiveActiveTab == "edit")
          mkTab "Alerts" (effectiveActiveTab == "alerts")
      when isNewWidget $ h3_ [class_ "text-lg font-semibold text-textStrong"] "Add a new widget"

    div_ [class_ "flex items-center gap-3"] do
      TimePicker.timepicker_ Nothing currentRange (Just "widget")
      TimePicker.refreshButton_
      div_ [class_ "w-px h-5 bg-strokeWeak"] ""
      if isNewWidget
        then button_ [class_ "btn btn-primary btn-sm shadow-sm !h-auto", type_ "submit", form_ widgetFormId] "Save changes"
        else button_ [class_ "btn btn-primary btn-sm shadow-sm hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block !h-auto", type_ "submit", form_ widgetFormId] "Save changes"
      label_ [class_ "btn btn-ghost btn-circle btn-sm tap-target", Aria.label_ "Close drawer", data_ "tippy-content" "Close Drawer", Lucid.for_ drawerStateCheckbox] $ faSprite_ "xmark" "regular" "w-4 h-4"

  div_ [class_ "w-full aspect-4/1 p-4 rounded-xl bg-fillWeaker border border-strokeWeak mb-6"] do
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
  div_ [class_ $ if isNewWidget then "block" else "hidden group-has-[.page-drawer-tab-edit:checked]/wgtexp:block"] do
    div_ [class_ "space-y-8"] do
      div_ [class_ "space-y-4"] do
        div_ [class_ "flex items-start gap-3"] do
          span_ [class_ "flex-shrink-0 inline-flex items-center justify-center w-7 h-7 rounded-full bg-fillWeak text-sm font-medium tabular-nums"] "1"
          strong_ [class_ "text-base font-semibold text-textStrong"] "Configure Query"
        div_ [class_ "pl-10 flex flex-col gap-3"] do
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
              $ loadingIndicator_ "xs" "spinner"

      div_ [class_ "space-y-4"] do
        div_ [class_ "flex items-start gap-3"] do
          span_ [class_ "flex-shrink-0 inline-flex items-center justify-center w-7 h-7 rounded-full bg-fillWeak text-sm font-medium tabular-nums"] "2"
          strong_ [class_ "text-base font-semibold text-textStrong"] "Give your graph a title"
        div_ [class_ "pl-10"]
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
        alertEndpoint = case dashboardIdM of
          Just dashId -> "/p/" <> pid.toText <> "/widgets/" <> sourceWid <> "/alert?dashboard_id=" <> dashId.toText
          Nothing -> ""
    div_ [class_ "hidden group-has-[.page-drawer-tab-alerts:checked]/wgtexp:block"] do
      widgetAlertConfig_ pid alertFormId alertEndpoint sourceWid widgetToUse


-- | Widget alert configuration form
widgetAlertConfig_ :: Projects.ProjectId -> Text -> Text -> Text -> Widget.Widget -> Html ()
widgetAlertConfig_ pid alertFormId alertEndpoint widgetId widget = do
  let hasAlert = isJust widget.alertId
  form_
    [ id_ alertFormId
    , hxPost_ alertEndpoint
    , hxSwap_ "none"
    , hxTrigger_ "submit"
    , class_ "space-y-6"
    ]
    do
      input_ [type_ "hidden", name_ "widgetId", value_ widgetId]
      input_ [type_ "hidden", name_ "query", value_ $ fromMaybe "" widget.query]
      input_
        [ type_ "hidden"
        , name_ "vizType"
        , value_ $ case widget.wType of
            Widget.WTTimeseries -> "timeseries"
            Widget.WTTimeseriesLine -> "timeseries_line"
            _ -> "timeseries"
        ]

      label_ [class_ "flex items-center justify-between p-4 bg-fillWeaker rounded-xl border border-strokeWeak cursor-pointer"] do
        div_ [] do
          h4_ [class_ "font-medium text-textStrong"] "Enable Alert"
          p_ [class_ "text-xs text-textWeak"] "Get notified when this widget's value crosses thresholds"
        input_ $ [type_ "checkbox", name_ "alertEnabled", class_ "toggle toggle-primary"] <> [checked_ | hasAlert]

      div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak p-4 space-y-4"] do
        h4_ [class_ "text-sm font-medium text-textStrong"] "Thresholds"
        div_ [class_ "grid grid-cols-2 gap-4"] do
          Alerts.thresholdInput_ "alertThreshold" "bg-fillError-weak" "Alert threshold" True "input-bordered w-full" [] widget.alertThreshold
          Alerts.thresholdInput_ "warningThreshold" "bg-fillWarning-weak" "Warning threshold" False "input-bordered w-full" [] widget.warningThreshold
        Alerts.directionSelect_ False "select-bordered w-full"
        fieldset_ [class_ "fieldset"] do
          label_ [class_ "label text-xs"] "Show threshold lines"
          select_ [name_ "showThresholdLines", class_ "select select-bordered w-full"] do
            let isAlways = widget.showThresholdLines == Just "always" || isNothing widget.showThresholdLines
                isOnBreach = widget.showThresholdLines == Just "on_breach"
                isNever = widget.showThresholdLines == Just "never"
            option_ ([value_ "always"] <> [selected_ "" | isAlways]) "Always"
            option_ ([value_ "on_breach"] <> [selected_ "" | isOnBreach]) "Only when breached"
            option_ ([value_ "never"] <> [selected_ "" | isNever]) "Never"

      Alerts.collapsibleSection_ "rotate-left" "regular" "Recovery thresholds" (Just "(prevents flapping)") do
        p_ [class_ "text-xs text-textWeak mb-3"] "Alert recovers only when value crosses these thresholds"
        div_ [class_ "grid grid-cols-2 gap-4"] do
          Alerts.recoveryInput_ "alertRecoveryThreshold" "bg-fillError-weak" "Alert recovery" "input-bordered w-full" Nothing
          Alerts.recoveryInput_ "warningRecoveryThreshold" "bg-fillWarning-weak" "Warning recovery" "input-bordered w-full" Nothing

      div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak p-4"] $ Alerts.frequencySelect_ 5 True "select-bordered w-full"

      div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak p-4"] do
        label_ [class_ "text-xs font-medium text-textStrong block mb-2"] "Alert title"
        input_
          [ type_ "text"
          , name_ "title"
          , class_ "input input-bordered w-full"
          , placeholder_ "Widget threshold exceeded"
          , required_ "required"
          , value_ $ fromMaybe (fromMaybe "Widget Alert" widget.title <> " - Threshold Alert") Nothing
          ]

      div_ [class_ "flex justify-end gap-3 pt-4"] do
        when hasAlert do
          button_
            [ type_ "button"
            , class_ "btn btn-ghost btn-sm"
            , hxDelete_ alertEndpoint
            , hxSwap_ "none"
            ]
            "Remove Alert"
        button_ [type_ "submit", class_ "btn btn-primary btn-sm"] $ if hasAlert then "Update Alert" else "Create Alert"


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
  , title :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


widgetAlertUpsertH :: Projects.ProjectId -> Text -> Maybe UUID.UUID -> WidgetAlertForm -> ATAuthCtx (RespHeaders (Html ()))
widgetAlertUpsertH pid _widgetIdPath dashboardIdM form = do
  now <- Time.currentTime

  -- Check if alert already exists for this widget
  existingMonitor <- Monitors.queryMonitorByWidgetId form.widgetId
  queryMonitorId <- case existingMonitor of
    Just m -> pure m.id
    Nothing -> liftIO $ Monitors.QueryMonitorId <$> UUID.nextRandom

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
      addSuccessToast "Alert removed from widget" Nothing
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
              , recipientEmailAll = Nothing
              , direction = form.direction
              , title = form.title
              , severity = "warning"
              , subject = form.title
              , message = ""
              , query = form.query
              , since = "1h"
              , from = ""
              , to = ""
              , frequency = form.frequency
              , timeWindow = Nothing
              , conditionType = Just "threshold_exceeded"
              , source = Just "widget"
              , vizType = form.vizType
              , teams = []
              , alertRecoveryThreshold = form.alertRecoveryThreshold
              , warningRecoveryThreshold = form.warningRecoveryThreshold
              , widgetId = Just form.widgetId
              , dashboardId = UUID.toText <$> dashboardIdM
              }

      let queryMonitor = Alerts.convertToQueryMonitor pid now queryMonitorId alertForm
      _ <- Monitors.queryMonitorUpsert queryMonitor
      addSuccessToast "Widget alert configured successfully" Nothing
      addRespHeaders $ toHtml ("" :: Text)


widgetAlertDeleteH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
widgetAlertDeleteH _pid widgetId = do
  _ <- Monitors.deleteMonitorsByWidgetIds [widgetId]
  addSuccessToast "Alert removed from widget" Nothing
  addRespHeaders $ toHtml ("" :: Text)


-- visTypes is now imported from LogQueryBox to avoid circular dependencies

-- | Wrapper for the new widget editor
newWidget_ :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe (Text, Text) -> Html ()
newWidget_ pid dashId tabSlugM currentRange = widgetViewerEditor_ pid (Just dashId) tabSlugM currentRange Nothing "edit"


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
            label_ [class_ "flex flex-col gap-1 w-full"] do
              span_ [class_ "text-sm font-medium"] "Folder"
              input_ [type_ "text", class_ "input input-sm w-full font-mono", name_ "fileDir", placeholder_ "reports/"]

          div_ [class_ "flex items-center justify-center shrink"] $ button_ [class_ "btn btn-primary btn-sm", type_ "submit"] "Create"
        div_ [class_ "py-2 border-b border-b-strokeWeak"] do
          span_ [class_ "text-sm "] "Using "
          span_ [class_ "text-sm font-medium", id_ "dItemTitle"] "Custom Dashboard"
          span_ [class_ "text-sm "] " template"
          p_ [class_ "text-xs text-textWeak w-full overflow-ellipsis truncate", id_ "dItemDescription"] "Get started from a blank slate"
        div_ [class_ "pt-5"]
          $ div_ [class_ "bg-fillBrand-strong px-2 py-4 rounded-xl w-full flex items-center"]
          $ img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full rounded overflow-hidden", id_ "dItemPreview", term "loading" "lazy", term "decoding" "async"]
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
              folder = folderFromPath dash.filePath
          span_ [class_ "flex items-center gap-2"] do
            span_ [class_ "p-1 px-2 bg-fillWeak rounded-md", data_ "tippy-content" "Dashboard icon"] $ faSprite_ (getDashIcon dash) "regular" "w-3 h-3"
            unless (T.null folder) $ span_ [class_ "text-xs text-textWeak font-mono", data_ "tippy-content" "Folder path for git sync"] $ toHtml folder
            a_ [href_ baseUrl, class_ "font-medium text-textStrong hover:text-textBrand hover:underline underline-offset-2"] $ toHtml $ dashTitle dash.title
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
                }
      addRespHeaders $ DashboardsGet (PageCtx bwconf $ DashboardsGetD{dashboards, projectId = pid, embedded = False, hideActions = False, teams, tableActions, filters, availableTags})


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
          dir = fromMaybe "" form.fileDir
          filePath = if T.null dir then Nothing else Just $ (if T.last dir == '/' then dir else dir <> "/") <> GitSync.titleToFilePath form.title
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
              , filePath = filePath
              , fileSha = Nothing
              }
      _ <- Dashboards.insert dbd
      syncDashboardAndQueuePush pid dbd.id
      redirectCS redirectURI
      addRespHeaders DashboardNoContent


-- TH splice: reads all dashboard YAML files from static/public/dashboards at compile time
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
  mDashboard <- Dashboards.getDashboardById dashId
  case mDashboard of
    Nothing -> do
      addErrorToast "Dashboard not found or does not belong to this project" Nothing
      addRespHeaders $ DashboardPostError "Dashboard not found or does not belong to this project"
    Just dashVM -> do
      _ <- Dashboards.updateTitle dashId form.title

      whenJust dashVM.schema \_ ->
        void $ Dashboards.updateSchema dashId (fromMaybe def dashVM.schema & #title ?~ form.title)

      -- Update file path: directory + auto-generated filename
      let dir = fromMaybe "" form.fileDir
          newPath = (if T.null dir || T.last dir == '/' then dir else dir <> "/") <> GitSync.titleToFilePath form.title
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
  mDashboard <- Dashboards.getDashboardById dashId
  case mDashboard of
    Nothing -> do
      addErrorToast "Dashboard not found or does not belong to this project" Nothing
      addRespHeaders $ DashboardPostError "Dashboard not found or does not belong to this project"
    Just dashVM -> do
      (sess, _) <- Sessions.sessionAndProject pid
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
-- If the original widget has an alert, the alert is also duplicated with the new widget.
dashboardDuplicateWidgetPostH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders Widget.Widget)
dashboardDuplicateWidgetPostH pid dashId widgetId = do
  (_, dash) <- getDashAndVM dashId Nothing
  case findWidgetInDashboard widgetId dash of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just (tabSlugM, widgetToDuplicate) -> do
      newWidgetId <- UUID.genUUID <&> UUID.toText
      now <- Time.currentTime

      -- Clone alert if original widget has one
      existingMonitorM <- Monitors.queryMonitorByWidgetId (normalizeWidgetId widgetId)
      newAlertIdM <- forM existingMonitorM \monitor -> do
        newMonitorId <- Monitors.QueryMonitorId <$> liftIO UUID.nextRandom
        let newMonitor =
              (monitor :: Monitors.QueryMonitor)
                { Monitors.id = newMonitorId
                , Monitors.createdAt = now
                , Monitors.updatedAt = now
                , Monitors.widgetId = Just newWidgetId
                , Monitors.alertLastTriggered = Nothing
                , Monitors.warningLastTriggered = Nothing
                , Monitors.currentStatus = Monitors.MSNormal
                }
        void $ Monitors.queryMonitorUpsert newMonitor
        pure newMonitorId.toText

      let widgetCopy =
            widgetToDuplicate
              { Widget.id = Just newWidgetId
              , Widget.title = case widgetToDuplicate.title of
                  Nothing -> Just "Widget Copy"
                  Just "" -> Just "Widget Copy"
                  Just title -> Just (title <> " (Copy)")
              , Widget._projectId = Just pid
              , Widget._dashboardId = Just dashId.toText
              , Widget.alertId = newAlertIdM
              , Widget.alertStatus = Nothing
              }

      let updatedDash = case tabSlugM of
            Just slug -> updateTabBySlug slug (#widgets %~ (<> [widgetCopy])) dash
            Nothing -> dash & #widgets %~ (<> [widgetCopy])
      _ <- Dashboards.updateSchemaAndUpdatedAt dashId updatedDash now
      syncDashboardAndQueuePush pid dashId

      addWidgetJSON $ decodeUtf8 $ fromLazy $ AE.encode widgetCopy
      addSuccessToast "Widget duplicated successfully" Nothing
      addRespHeaders widgetCopy


dashboardWidgetExpandGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> ATAuthCtx (RespHeaders (Html ()))
dashboardWidgetExpandGetH pid dashId widgetId = do
  (_, dash) <- getDashAndVM dashId Nothing
  now <- Time.currentTime
  let timeParams = (Nothing, Nothing, Nothing)
      paramsWithVarDefaults = addVariableDefaults [] dash.variables
  (_, allParamsWithConstants) <- processConstantsAndExtendParams pid now timeParams paramsWithVarDefaults (fromMaybe [] dash.constants)
  case snd <$> findWidgetInDashboard widgetId dash of
    Nothing -> throwError $ err404{errBody = "Widget not found in dashboard"}
    Just widgetToExpand -> do
      processedWidget <- processWidget pid now timeParams allParamsWithConstants widgetToExpand
      addRespHeaders $ widgetViewerEditor_ pid (Just dashId) Nothing Nothing (Just processedWidget) "edit"


-- | SQL preview endpoint for debugging KQL queries (shows generated SQL)
widgetSqlPreviewGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
widgetSqlPreviewGetH pid queryM sinceStr fromDStr toDStr = do
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
    sqlBlock_ label sql = div_ [class_ "space-y-1"] do
      div_ [class_ "flex justify-between items-center"] do
        span_ [class_ "text-textWeak font-sans"] $ toHtml label
        button_
          [ class_ "text-textBrand hover:underline font-sans text-xs"
          , term "_" [text| on click writeText(`${T.replace "`" "\\`" sql}`) to the navigator's clipboard then set my.innerText to 'Copied!' then wait 1.5s then set my.innerText to 'Copy' |]
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
    match w = w.id == Just wid || maybeToMonoid (slugify <$> w.title) == wid
    -- Recursively search widget and its children
    findInWidget :: Widget.Widget -> Maybe Widget.Widget
    findInWidget w = mfilter match (pure w) <|> asum (maybe [] (map findInWidget) w.children)
    tabResult = listToMaybe [(Just $ slugify t.name, w') | t <- fromMaybe [] dash.tabs, w <- t.widgets, w' <- maybeToList (findInWidget w)]
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


-- | Build query string from params, prefixed with ? if non-empty
queryStringFrom :: [(Text, Maybe Text)] -> Text
queryStringFrom params = let qs = toQueryParams params in if T.null qs then "" else "?" <> qs


-- | Add variable defaults to params for any variable not already in params.
-- This ensures constants can reference variables like {{var-resource}} even when not in URL.
addVariableDefaults :: [(Text, Maybe Text)] -> Maybe [Dashboards.Variable] -> [(Text, Maybe Text)]
addVariableDefaults params varsM = params <> defaults
  where
    paramsMap = Map.fromList params
    defaults = [("var-" <> v.key, v.value) | v <- fromMaybe [] varsM, not (Map.member ("var-" <> v.key) paramsMap)]


-- | Process dashboard constants concurrently and build extended params with constant results
processConstantsAndExtendParams
  :: Projects.ProjectId
  -> UTCTime
  -> (Maybe Text, Maybe Text, Maybe Text)
  -> [(Text, Maybe Text)]
  -> [Dashboards.Constant]
  -> ATAuthCtx ([Dashboards.Constant], [(Text, Maybe Text)])
processConstantsAndExtendParams pid now timeParams allParams constants =
  pooledForConcurrently constants (processConstant pid now timeParams allParams) <&> \pc ->
    ( pc
    , allParams
        <> [("const-" <> c.key, Just $ DashboardUtils.constantToSQLList $ fromMaybe [] c.result) | c <- pc]
        <> [("const-" <> c.key <> "-kql", Just $ DashboardUtils.constantToKQLList $ fromMaybe [] c.result) | c <- pc]
    )


-- | Create a widget processor that adds dashboard ID to processed widgets.
-- Pre-computes replacement maps once for efficiency across all widgets.
mkWidgetProcessor
  :: Projects.ProjectId
  -> Dashboards.DashboardId
  -> UTCTime
  -> (Maybe Text, Maybe Text, Maybe Text)
  -> [(Text, Maybe Text)]
  -> Widget.Widget
  -> ATAuthCtx Widget.Widget
mkWidgetProcessor pid dashId now timeParams paramsWithConstants =
  fmap (#_dashboardId ?~ dashId.toText) . processWidget pid now timeParams paramsWithConstants


-- | Handler for dashboard with tab in path: /p/{pid}/dashboards/{dash_id}/tab/{tab_slug}
-- This renders the full page with the specified tab active
dashboardTabGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardTabGetH pid dashId tabSlug fileM fromDStr toDStr sinceStr allParams = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = TimePicker.parseTimeRange now (TimePicker.TimePicker sinceStr fromDStr toDStr)

  (dashVM, dash) <- getDashAndVM dashId fileM

  -- Find the active tab by slug
  let activeTabInfo = dash.tabs >>= (`findTabBySlug` tabSlug)
      activeTabIdx = maybe 0 fst activeTabInfo
      activeTabName = fmap ((.name) . snd) activeTabInfo
      timeParams = (sinceStr, fromDStr, toDStr)
      paramsWithVarDefaults = addVariableDefaults allParams dash.variables

  -- Process constants and variables
  (processedConstants, allParamsWithConstants) <- processConstantsAndExtendParams pid now timeParams paramsWithVarDefaults (fromMaybe [] dash.constants)
  let dashWithConstants = dash & #constants ?~ processedConstants
      processWidgetWithDashboardId = mkWidgetProcessor pid dashId now timeParams allParamsWithConstants

  dash' <- forOf (#variables . traverse . traverse) dashWithConstants (processVariable pid now timeParams allParamsWithConstants)

  -- Only process widgets for the ACTIVE tab (lazy loading - other tabs load via htmx)
  -- Note: We don't process dash.widgets here since this is a tab-based dashboard
  dash'' <- case dash'.tabs of
    Just tabs -> do
      processedTabs <- forM (zip [0 ..] tabs) \(idx, tab) ->
        if idx == activeTabIdx
          then do
            -- Process widgets concurrently for faster initial page load
            processedWidgets <- pooledForConcurrently tab.widgets processWidgetWithDashboardId
            widgetsWithAlerts <- populateWidgetAlertStatuses processedWidgets
            pure $ tab & #widgets .~ widgetsWithAlerts
          else pure tab -- Don't process widgets for inactive tabs
      pure $ dash' & #tabs ?~ processedTabs
    Nothing -> pure dash'

  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "Dashboards"
          , pageTitle = dashTitle dashVM.title
          , pageTitleSuffix = activeTabName -- Show current tab in breadcrumbs
          , pageTitleModalId = Just "pageTitleModalId"
          , pageTitleSuffixModalId = Just "tabRenameModalId" -- Modal for renaming tab
          , config = appCtx.config
          , freeTierExceeded = freeTierExceeded
          , headContent = Just dashboardHeadContent_
          , pageActions = Just $ div_ [class_ "flex gap-3 items-center"] do
              TimePicker.timepicker_ Nothing currentRange Nothing
              TimePicker.refreshButton_
              dashboardActions_ pid dashId (Just tabSlug) currentRange
          , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/dashboard/"
          }
  -- Pass the active tab slug and computed constants in params for rendering
  -- Including constants allows HTMX tab switches to skip re-executing constant queries
  let paramsWithTab = (activeTabSlugKey, Just tabSlug) : allParamsWithConstants
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dashId dash'' dashVM paramsWithTab


-- | Handler for tab content partial (htmx): /p/{pid}/dashboards/{dash_id}/tab/{tab_slug}/content
-- This returns only the tab content panel for htmx swapping
dashboardTabContentGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> [(Text, Maybe Text)] -> ATAuthCtx (RespHeaders (Html ()))
dashboardTabContentGetH pid dashId tabSlug fileM fromDStr toDStr sinceStr allParams = do
  now <- Time.currentTime
  (_dashVM, dash) <- getDashAndVM dashId fileM
  let timeParams = (sinceStr, fromDStr, toDStr)
      paramsWithVarDefaults = addVariableDefaults allParams dash.variables
      -- Check if constants are already in params (passed from initial page load)
      hasConstants = any (\(k, _) -> "const-" `T.isPrefixOf` k) allParams

  -- Find the tab by slug
  case dash.tabs of
    Nothing -> throwError $ err404{errBody = "Dashboard has no tabs"}
    Just tabs -> case findTabBySlug tabs tabSlug of
      Nothing -> throwError $ err404{errBody = "Tab not found: " <> encodeUtf8 tabSlug}
      Just (idx, tab) -> do
        -- Skip constant processing if already provided via params (avoids redundant SQL queries)
        allParamsWithConstants <-
          if hasConstants
            then pure paramsWithVarDefaults
            else snd <$> processConstantsAndExtendParams pid now timeParams paramsWithVarDefaults (fromMaybe [] dash.constants)
        let processWidgetWithDashboardId = mkWidgetProcessor pid dashId now timeParams allParamsWithConstants

        -- Process variables to check if tab requires one that's not set
        processedVars <- traverse (traverse (processVariable pid now timeParams allParamsWithConstants)) dash.variables
        let varToPrompt = findVarToPrompt (Just tab) (fromMaybe [] processedVars)

        -- Process widgets concurrently to speed up tabs with multiple eager widgets
        processedWidgets <- pooledForConcurrently tab.widgets processWidgetWithDashboardId
        widgetsWithAlerts <- populateWidgetAlertStatuses processedWidgets

        -- Render tab content panel + OOB modal if variable needs prompting + OOB form URL update
        let widgetOrderUrl = "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/widgets_order?tab=" <> tabSlug
        addRespHeaders $ do
          tabContentPanel_ pid dashId.toText idx tab.name widgetsWithAlerts True True
          whenJust varToPrompt \v -> variablePickerModal_ pid dashId (Just tabSlug) allParamsWithConstants v True
          -- OOB swap to update widget-order-trigger form's hx-patch URL for the current tab
          form_
            [ id_ "widget-order-trigger"
            , class_ "hidden"
            , hxPatch_ widgetOrderUrl
            , hxVals_ "js:{...buildWidgetOrder(document.querySelector('.grid-stack'))}"
            , hxExt_ "json-enc"
            , hxSwap_ "none"
            , hxTrigger_ "widget-order-changed from:body"
            , hxSwapOob_ "true"
            ]
            ""


-- | Skeleton loader shown while GridStack initializes
dashboardSkeleton_ :: Html ()
dashboardSkeleton_ = div_ [class_ "dashboard-skeleton absolute inset-0 z-10 bg-bgBase flex flex-col items-center justify-center"] do
  loadingIndicatorWith_ "lg" "spinner" "text-fillBrand-strong"
  p_ [class_ "text-sm text-textWeak mt-3"] "Loading dashboard..."
  div_ [class_ "grid grid-cols-12 gap-4 mt-8 w-full max-w-4xl px-8"] do
    div_ [class_ "col-span-8 h-32 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 h-32 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 h-24 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 h-24 rounded-lg skeleton-shimmer"] ""
    div_ [class_ "col-span-4 h-24 rounded-lg skeleton-shimmer"] ""


-- | Render a single tab content panel
-- isPartial: True for HTMX partial loads (include OOB swap), False for full page loads
tabContentPanel_ :: Projects.ProjectId -> Text -> Int -> Text -> [Widget.Widget] -> Bool -> Bool -> Html ()
tabContentPanel_ pid dashboardId idx tabName widgets isActive isPartial = do
  when isPartial $ breadcrumbSuffixOob_ tabName
  -- Tab content panel
  div_
    [ class_ $ "tab-panel grid-stack -m-2" <> if isActive then "" else " hidden"
    , data_ "tab-index" (show idx)
    , id_ $ "tab-panel-" <> dashboardId <> "-" <> show idx
    ]
    do
      forM_ widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
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
  (dashVM, dash) <- getDashAndVM dashId Nothing
  now <- Time.currentTime

  case dash.tabs of
    Nothing -> throwError $ err404{errBody = "Dashboard has no tabs"}
    Just tabs -> case findTabBySlug tabs tabSlug of
      Nothing -> throwError $ err404{errBody = "Tab not found: " <> encodeUtf8 tabSlug}
      Just (idx, _) -> do
        let updatedTabs = tabs & ix idx . #name .~ form.newName
            updatedDash = dash & #tabs ?~ updatedTabs
            newSlug = slugify form.newName

        -- Update the dashboard schema
        _ <- Dashboards.updateSchemaAndUpdatedAt dashId updatedDash now
        syncDashboardAndQueuePush pid dashId

        addSuccessToast "Tab renamed successfully" Nothing
        -- Redirect to new tab URL after rename
        redirectCS $ "/p/" <> pid.toText <> "/dashboards/" <> dashId.toText <> "/tab/" <> newSlug
        addRespHeaders $ TabRenameRes{newName = form.newName, newSlug = newSlug}


-- | Unified dashboard actions (add widget button, yaml drawer, context menu)
dashboardActions_ :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe (Text, Text) -> Html ()
dashboardActions_ pid dashId tabSlugM currentRange = div_ [class_ "flex items-center"] do
  span_ [class_ "text-fillDisabled mr-2"] "|"
  Components.drawer_ "page-data-drawer" Nothing (Just $ newWidget_ pid dashId tabSlugM currentRange) $ span_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Add a new widget", data_ "tippy-content" "Add a new widget"] $ faSprite_ "plus" "regular" "w-3 h-3"
  yamlEditorDrawer_ pid dashId
  div_ [class_ "dropdown dropdown-end"] do
    div_ [tabindex_ "0", role_ "button", class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Open context menu", data_ "tippy-content" "Context Menu"] $ faSprite_ "ellipsis" "regular" "w-4 h-4"
    ul_ [tabindex_ "0", class_ "dropdown-content menu menu-md bg-base-100 rounded-box p-2 w-52 shadow-sm leading-none"] do
      li_ $ label_ [Lucid.for_ "pageTitleModalId", class_ "p-2"] "Rename dashboard"
      whenJust tabSlugM $ \_ -> li_ $ label_ [Lucid.for_ "tabRenameModalId", class_ "p-2"] "Rename tab"
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
          label_ [class_ "btn btn-ghost btn-sm", Aria.label_ "Close YAML editor", Lucid.for_ drawerId] $ faSprite_ "xmark" "regular" "w-4 h-4"
      div_ [class_ "flex-1 overflow-hidden", id_ "yaml-editor-wrapper", hxGet_ yamlUrl, hxTrigger_ "intersect once", hxTarget_ "#yaml-editor-content"] do
        div_ [id_ "yaml-editor-content", class_ "h-full flex items-center justify-center"] $ loadingIndicator_ "md" "dots"
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
