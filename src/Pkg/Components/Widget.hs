module Pkg.Components.Widget (Widget (..), WidgetDataset (..), widget_, Layout (..), WidgetType (..), TableColumn (..), RowClickAction (..), mapChatTypeToWidgetType, mapWidgetTypeToChartType, widgetToECharts, WidgetAxis (..), SummarizeBy (..), widgetPostH, renderTableWithData, renderTraceDataTable, renderTableWithDataAndParams) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as K
import Data.Default
import Data.Generics.Labels ()
import Data.HashMap.Lazy qualified as HM
import Data.Map.Strict qualified as M
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Language.Haskell.TH.Syntax qualified as THS
import Lucid
import Lucid.Htmx (hxExt_, hxGet_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Network.HTTP.Types (urlEncode)
import Pages.Charts.Charts qualified as Charts
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils (faSprite_, getServiceColors, onpointerdown_, prettyPrintCount, prettyPrintDuration)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)


-- Generic instance for parsing JSON arrays from form data
instance AE.FromJSON a => FromHttpApiData [a] where
  parseQueryParam = first T.pack . AE.eitherDecodeStrict . encodeUtf8


-- Generic instance for parsing JSON values from form data
instance {-# OVERLAPPABLE #-} AE.FromJSON a => FromHttpApiData a where
  parseQueryParam = first T.pack . AE.eitherDecodeStrict . encodeUtf8


data Query = Query
  { query :: Maybe Text
  , sql :: Maybe Text
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Query


data Layout = Layout
  { x :: Maybe Int
  , y :: Maybe Int
  , w :: Maybe Int
  , h :: Maybe Int
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Layout


data WidgetType
  = WTGroup
  | WTLogs
  | WTTimeseries
  | WTTimeseriesLine
  | WTTimeseriesStat
  | WTStat
  | WTList
  | WTTopList
  | WTDistribution
  | WTGeomap
  | WTFunnel
  | WTTreeMap
  | WTPieChart
  | WTAnomalies
  | WTTable
  | WTTraces
  | WTFlamegraph
  deriving stock (Enum, Eq, Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "WT", DAE.CamelToSnake]] WidgetType


data SummarizeBy
  = SBSum
  | SBMax
  | SBMin
  | SBCount
  deriving stock (Enum, Eq, Generic, Show, THS.Lift)
  deriving anyclass (Default, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SB", DAE.CamelToSnake]] SummarizeBy


summarizeByPrefix :: SummarizeBy -> Text
summarizeByPrefix SBSum = ""
summarizeByPrefix SBMax = "<"
summarizeByPrefix SBMin = ">"
summarizeByPrefix SBCount = ""


-- when processing widgets we'll do them async, so eager queries are loaded upfront
data Widget = Widget
  { wType :: WidgetType -- Widget type: "timeseries", "table", etc.
  , id :: Maybe Text
  , naked :: Maybe Bool
  , showTooltip :: Maybe Bool
  , title :: Maybe Text -- Widget title
  , subtitle :: Maybe Text
  , hideSubtitle :: Maybe Bool
  , icon :: Maybe Text
  , timeseriesStatAggregate :: Maybe Text -- average, min, max, sum, etc
  , sql :: Maybe Text
  , summarizeBy :: Maybe SummarizeBy
  , query :: Maybe Text
  , queries :: Maybe [Query] -- Multiple queries for combined visualizations
  , layout :: Maybe Layout -- Layout (x, y, w, h)
  , xAxis :: Maybe WidgetAxis
  , yAxis :: Maybe WidgetAxis -- Optional y-axis label
  , unit :: Maybe Text
  , value :: Maybe Int -- value could represent a number or a count
  , wData :: Maybe AE.Value
  , hideLegend :: Maybe Bool
  , legendPosition :: Maybe Text -- Position of the legend: "top" or "bottom" (default)
  , theme :: Maybe Text
  , dataset :: Maybe WidgetDataset
  , -- eager
    eager :: Maybe Bool
  , _projectId :: Maybe Projects.ProjectId
  , _dashboardId :: Maybe Text -- Dashboard ID for context
  , _isNested :: Maybe Bool
  , _centerTitle :: Maybe Bool
  , expandBtnFn :: Maybe Text
  , children :: Maybe [Widget]
  , html :: Maybe LText
  , standalone :: Maybe Bool -- Not used in a grid stack
  , allowZoom :: Maybe Bool -- Allow zooming in the chart
  , showMarkArea :: Maybe Bool -- Show mark area in the chart
  , columns :: Maybe [TableColumn] -- Table columns
  , onRowClick :: Maybe RowClickAction -- Action when table row is clicked
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] Widget


instance ToHtml Widget where
  toHtml w = toHtml $ widget_ w
  toHtmlRaw = toHtml


data WidgetDataset = WidgetDataset
  { source :: AE.Value
  , rowsPerMin :: Maybe Double
  , value :: Maybe Double
  , from :: Maybe Int
  , to :: Maybe Int
  , stats :: Maybe Charts.MetricsStats
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetDataset


data WidgetAxis = WidgetAxis
  { label :: Maybe Text
  , showAxisLabel :: Maybe Bool
  , series :: Maybe [WidgetAxis]
  , showOnlyMaxLabel :: Maybe Bool
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetAxis


data TableColumn = TableColumn
  { field :: Text
  , title :: Text
  , unit :: Maybe Text
  , clickable :: Maybe Bool
  , link :: Maybe Text
  , width :: Maybe Text
  , align :: Maybe Text
  , progress :: Maybe Text -- "column_percent" or "value_percent"
  , progressVariant :: Maybe Text -- "default", "info", "error", etc.
  , columnType :: Maybe Text -- "number", "duration", "text" (default)
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] TableColumn


data RowClickAction = RowClickAction
  { setVariable :: Maybe Text
  , value :: Maybe Text
  , navigateToTab :: Maybe Text
  }
  deriving stock (Generic, Show, THS.Lift)
  deriving anyclass (Default, FromForm, NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RowClickAction


-- Used when converting a widget json to its html representation. Eg in a query chart builder
widgetPostH :: Projects.ProjectId -> Widget -> ATAuthCtx (RespHeaders Widget)
widgetPostH pid widget = addRespHeaders (widget & (#_projectId ?~ pid))


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ = widgetHelper_


widgetHelper_ :: Widget -> Html ()
widgetHelper_ w' = case w.wType of
  WTAnomalies -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] do
    renderWidgetHeader w (maybeToMonoid w.id) w.title Nothing Nothing Nothing (Just ("View all", "/p/" <> maybeToMonoid (w._projectId <&> (.toText)) <> "/anomalies")) (w.hideSubtitle == Just True)
    whenJust w.html toHtmlRaw
  WTGroup -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ div_ [class_ "h-full flex flex-col gap-4"] do
    div_ [class_ $ "group/h gap-1 leading-none flex justify-between items-center " <> gridStackHandleClass] do
      div_ [class_ "inline-flex gap-1 items-center"] do
        span_ [class_ "hidden group-hover/h:inline-flex"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
        whenJust w.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
        span_ [class_ "text-sm"] $ toHtml $ maybeToMonoid w.title
    div_ [class_ "grid-stack nested-grid  h-full -mx-2"] $ forM_ (fromMaybe [] w.children) (\wChild -> widgetHelper_ (wChild{_isNested = Just True}))
  WTTable -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ renderTable w
  WTLogs -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ div_ [class_ "p-3"] "Logs widget coming soon"
  WTTraces -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ renderTraceTable w
  WTFlamegraph -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ div_ [class_ "p-3"] "Flamegraph widget coming soon"
  _ -> gridItem_ $ div_ [class_ $ " w-full h-full group/wgt " <> paddingBtm] $ renderChart w
  where
    w = w' & #id %~ maybe (slugify <$> w'.title) Just
    gridStackHandleClass = if w._isNested == Just True then "nested-grid-stack-handle" else "grid-stack-handle"
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w)), ("h", (.h))]
    attrs = concat [maybe [] (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= layoutField) | (name, layoutField) <- layoutFields]
    paddingBtm = if w.standalone == Just True then "" else bool " pb-8 " " standalone pb-4 " (w._isNested == Just True)
    -- Serialize the widget to JSON for easy copying
    widgetJson = decodeUtf8 $ fromLazy $ AE.encode w
    gridItem_ =
      if w.naked == Just True
        then Relude.id
        else div_ ([class_ "grid-stack-item h-full flex-1 [.nested-grid_&]:overflow-hidden ", id_ $ maybeToMonoid w.id <> "_widgetEl", data_ "widget" widgetJson] <> attrs) . div_ [class_ "grid-stack-item-content h-full"]


renderWidgetHeader :: Widget -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text) -> Bool -> Html ()
renderWidgetHeader widget wId title valueM subValueM expandBtnFn ctaM hideSub = div_ [class_ $ "leading-none flex justify-between items-center  " <> bool "grid-stack-handle" "" (widget.standalone == Just True), id_ $ wId <> "_header"] do
  when (widget._centerTitle == Just True) $ div_ ""
  div_ [class_ "inline-flex gap-3 items-center group/h"] do
    span_ [class_ "text-sm flex items-center gap-1"] do
      unless (widget.standalone == Just True) $ span_ [class_ "hidden group-hover/h:inline-flex"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
      whenJust widget.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
      toHtml $ maybeToMonoid title
    span_ [class_ $ "bg-fillWeak border border-strokeWeak text-sm font-semibold px-2 py-1 rounded-3xl leading-none " <> if isJust valueM then "" else "hidden", id_ $ wId <> "Value"]
      $ whenJust valueM toHtml
    span_ [class_ $ "text-textWeak widget-subtitle text-sm " <> bool "" "hidden" hideSub, id_ $ wId <> "Subtitle"] $ toHtml $ maybeToMonoid subValueM
    -- Add hidden loader with specific ID that can be toggled from JS
    span_ [class_ "hidden", id_ $ wId <> "_loader"] $ Utils.faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
  div_ [class_ "text-iconNeutral flex items-center"] do
    -- Add expand button that's visible on hover

    whenJust ctaM \(ctaTitle, uri) -> a_ [class_ "underline underline-offset-2 text-textBrand", href_ uri] $ toHtml ctaTitle
    whenJust expandBtnFn \fn ->
      button_
        [ term "_" fn
        , class_ "p-2 cursor-pointer"
        , data_ "tippy-content" "Expand widget"
        ]
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    when (isJust widget._dashboardId)
      $ let pid = maybeToMonoid (widget._projectId <&> (.toText))
            dashId = maybeToMonoid widget._dashboardId
         in button_
              [ class_ "p-2 cursor-pointer hidden group-hover/wgt:block"
              , title_ "Expand widget"
              , data_ "tippy-content" "Expand widget"
              , term
                  "_"
                  [text| on pointerdown or click 
            set #global-data-drawer.checked to true
            then set #global-data-drawer-content.innerHTML to #loader-tmp.innerHTML
            then fetch `/p/${pid}/dashboards/${dashId}/widgets/${wId}/expand`
            then set #global-data-drawer-content.innerHTML to it
            then htmx.process(#global-data-drawer-content)
            then _hyperscript.processNode(#global-data-drawer-content)
            then window.evalScriptsFromContent(#global-data-drawer-content)
         |]
              ]
              $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    details_ [class_ "dropdown dropdown-end"] do
      summary_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg", data_ "tippy-content" "Widget Menu"]
        $ Utils.faSprite_ "ellipsis" "regular" "w-4 h-4"
      ul_ [class_ "text-textStrong menu menu-md dropdown-content bg-base-100 rounded-box p-2 w-52 shadow-sm leading-none z-10"] do
        -- Only show the "Move to dashboard" option if we're in a dashboard context

        let dashId = fromMaybe "" widget._dashboardId
        li_
          $ a_
            [ class_ "p-2 w-full text-left block cursor-pointer"
            , data_ "tippy-content" "Copy this widget to another dashboard"
            , id_ $ wId <> "_copy_link"
            , term
                "_"
                [text|
              on click 
              set #dashboards-modal.checked to true
              then set #dashboards-modal-widget-id.value to "${wId}"
              then set #dashboards-modal-source-dashboard-id.value to "${dashId}"
              then set (the closest <details/>).open to false 
            |]
            ]
            "Copy to dashboard"

        -- Only show the "Duplicate widget" option if we're in a dashboard context
        when (isJust widget._dashboardId) do
          li_
            $ a_
              [ class_ "p-2 w-full text-left block cursor-pointer"
              , data_ "tippy-content" "Create a copy of this widget"
              , hxPost_
                  $ "/p/"
                  <> maybeToMonoid (widget._projectId <&> (.toText))
                  <> "/dashboards/"
                  <> maybeToMonoid widget._dashboardId
                  <> "/widgets/"
                  <> wId
                  <> "/duplicate"
              , hxTrigger_ "click"
              , [__| on click set (the closest <details/>).open to false
                     on htmx:beforeSwap
                        set event.detail.shouldSwap to false then
                        set widgetData to JSON.parse(event.detail.xhr.getResponseHeader('X-Widget-JSON')) then
                        call gridStackInstance.addWidget({
                          w: widgetData.layout.w, 
                          h: widgetData.layout.h, 
                          x: widgetData.layout.x, 
                          y: widgetData.layout.y,
                          content: event.detail.serverResponse
                        })
                 |]
              ]
              "Duplicate widget"
          li_
            $ button_
              [ class_ "p-2 w-full text-left text-textError cursor-pointer"
              , data_ "tippy-content" "Permanently delete this widget"
              , onpointerdown_
                  [text|
                  if(confirm('Are you sure you want to delete this widget? This action cannot be undone.')) {
                    const widgetEl = document.getElementById('${wId}_widgetEl');
                    if (document.getElementById('${wId}_widgetEl')) {
                      widgetEl.dispatchEvent(new CustomEvent('widget-remove-requested', {
                        bubbles: true,
                        detail: { widgetId: '${wId}' }
                      }));
                    }
                  }
                  return false;
                |]
              ]
              "Delete widget"


renderTraceTable :: Widget -> Html ()
renderTraceTable widget = do
  let tableId = maybeToMonoid widget.id
  let eagerWidget = widget & #eager ?~ True
  let widgetJson = fromLazy $ AE.encode eagerWidget
  div_ [class_ "gap-0.5 flex flex-col h-full"] do
    -- Widget header outside the card
    unless (widget.naked == Just True)
      $ renderWidgetHeader widget tableId widget.title Nothing Nothing Nothing Nothing (widget.hideSubtitle == Just True)
    -- Card container that takes remaining space
    div_ [class_ "flex-1 flex min-h-0"] do
      div_
        [ class_
            $ "h-full w-full flex flex-col "
            <> if widget.naked == Just True then "" else "rounded-2xl border border-strokeWeak bg-fillWeaker"
        , id_ $ tableId <> "_bordered"
        ]
        do
          -- Single scrollable table container
          div_
            [ class_ "h-full overflow-auto p-3"
            , hxGet_ $ "/p/" <> fromMaybe "" (widget._projectId <&> (.toText)) <> "/widget?widgetJSON=" <> decodeUtf8 (urlEncode True widgetJson)
            , hxTrigger_ "load, update-query from:window"
            , hxTarget_ $ "#" <> tableId
            , hxSelect_ $ "#" <> tableId
            , hxSwap_ "outerHTML"
            , hxExt_ "forward-page-params"
            ]
            do
              case widget.html of
                Just html -> toHtmlRaw html -- Use pre-rendered HTML if available
                Nothing -> do
                  table_
                    [ class_ "table table-zebra table-sm w-full relative"
                    , id_ tableId
                    ]
                    do
                      -- Table header
                      thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"] do
                        tr_ [] do
                          forM_ (zip (["Resource", "Span name", "Duration", "Latency breakdown"]) [0 ..]) \(col, idx) ->
                            th_
                              [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group "
                              , onclick_ $ "window.sortTable('" <> tableId <> "', " <> T.pack (show idx) <> ", this)"
                              , data_ "sort-direction" "none"
                              ]
                              do
                                div_ [class_ "flex items-center justify-between"] do
                                  toHtml col
                                  span_ [class_ "sort-arrow ml-1 text-iconNeutral opacity-0 group-hover:opacity-100", data_ "sort" "none"] "↕"
                      -- Table body with loading indicator
                      tbody_ []
                        $ tr_ []
                        $ td_ [colspan_ "100", class_ "text-center py-8"]
                        $ span_ [class_ "loading loading-spinner loading-sm"] ""
      script_
        [type_ "text/javascript"]
        [text| htmx.process("#$tableId") |]


-- Table widget rendering
-- class_ "progress-brand "
renderTable :: Widget -> Html ()
renderTable widget = do
  let tableId = maybeToMonoid widget.id
  -- Make table widget eager by default so it fetches data server-side
  let eagerWidget = widget & #eager ?~ True
  let widgetJson = decodeUtf8 $ fromLazy $ AE.encode eagerWidget
  div_ [class_ "gap-0.5 flex flex-col h-full"] do
    -- Widget header outside the card
    unless (widget.naked == Just True)
      $ renderWidgetHeader widget tableId widget.title Nothing Nothing Nothing Nothing (widget.hideSubtitle == Just True)
    -- Card container that takes remaining space
    div_ [class_ "flex-1 flex min-h-0"] do
      div_
        [ class_
            $ "h-full w-full flex flex-col "
            <> if widget.naked == Just True then "" else "rounded-2xl border border-strokeWeak bg-fillWeaker"
        , id_ $ tableId <> "_bordered"
        ]
        do
          -- Single scrollable table container
          div_
            [ class_ "h-full overflow-auto p-3"
            , hxGet_ $ "/p/" <> fromMaybe "" (widget._projectId <&> (.toText)) <> "/widget?widgetJSON=" <> widgetJson
            , hxTrigger_ "load, update-query from:window"
            , hxTarget_ $ "#" <> tableId
            , hxSelect_ $ "#" <> tableId
            , hxSwap_ "outerHTML"
            , hxExt_ "forward-page-params"
            ]
            do
              case widget.html of
                Just html -> toHtmlRaw html -- Use pre-rendered HTML if available
                Nothing -> do
                  -- Otherwise render table structure with HTMX for updates
                  table_
                    [ class_ "table table-zebra table-sm w-full relative"
                    , id_ tableId
                    ]
                    do
                      -- Table header
                      thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"] do
                        tr_ [] do
                          forM_ (zip (fromMaybe [] widget.columns) [0 ..]) \(col, idx) ->
                            th_
                              [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" col.align
                              , onclick_ $ "window.sortTable('" <> tableId <> "', " <> T.pack (show idx) <> ", this)"
                              , data_ "sort-direction" "none"
                              ]
                              do
                                div_ [class_ "flex items-center justify-between"] do
                                  toHtml col.title
                                  span_ [class_ "sort-arrow ml-1 text-iconNeutral opacity-0 group-hover:opacity-100", data_ "sort" "none"] "↕"
                      -- Table body with loading indicator
                      tbody_ []
                        $ tr_ []
                        $ td_ [colspan_ "100", class_ "text-center py-8"]
                        $ span_ [class_ "loading loading-spinner loading-sm"] ""

    -- Add row click handler script if needed
    whenJust widget.onRowClick \onRowClick ->
      script_
        [type_ "text/javascript"]
        [text|
        (function() {
          const tableId = '${tableId}';
          const onRowClick = ${decodeUtf8 $ AE.encode onRowClick};
          const columns = ${decodeUtf8 $ AE.encode widget.columns};
          
          // Delegate click events to table rows
          document.getElementById(tableId).addEventListener('click', function(e) {
            const tr = e.target.closest('tr[data-row]');
            if (!tr) return;
            
            const rowData = JSON.parse(tr.dataset.row);
            
            // Set variable
            if (onRowClick.setVariable) {
              const varName = onRowClick.setVariable;
              const value = onRowClick.value ? 
                onRowClick.value.replace(/\{\{row\.(\w+)\}\}/g, (_, field) => rowData[field]) :
                rowData[columns[0].field];
              
              const url = new URL(window.location.href);
              url.searchParams.set('var-' + varName, value);
              history.pushState({}, '', url.toString());
              window.dispatchEvent(new Event('update-query'));
              
              // Navigate to tab if specified
              if (onRowClick.navigateToTab && window.switchDashboardTab) {
                const tabs = document.querySelectorAll('[data-tab-index]');
                tabs.forEach((tab, idx) => {
                  if (tab.textContent.includes(onRowClick.navigateToTab)) {
                    window.switchDashboardTab(idx);
                  }
                });
              }
            }
          });
        })();
        |]


renderChart :: Widget -> Html ()
renderChart widget = do
  let rateM = widget.dataset >>= (.rowsPerMin) >>= \r -> Just $ Utils.prettyPrintCount (round r) <> " rows/min"
  let chartId = maybeToMonoid widget.id
  let valueM = widget.dataset >>= (.value) >>= \x -> Just $ Utils.prettyPrintCount $ round x
  let isStat = widget.wType `elem` [WTTimeseriesStat, WTStat]
  let gridStackHandleClass = if widget._isNested == Just True then "nested-grid-stack-handle" else "grid-stack-handle"
  div_ [class_ "gap-0.5 flex flex-col h-full justify-end "] do
    unless (widget.naked == Just True || widget.wType `elem` [WTTimeseriesStat, WTStat])
      $ renderWidgetHeader widget chartId widget.title valueM rateM widget.expandBtnFn Nothing (widget.hideSubtitle == Just True)
    div_ [class_ $ "flex-1 flex " <> bool "" gridStackHandleClass isStat] do
      div_
        [ class_
            $ "h-full w-full flex flex-col justify-end "
            <> if widget.naked == Just True then "" else " rounded-2xl border border-strokeWeak bg-fillWeaker"
        , id_ $ chartId <> "_bordered"
        ]
        do
          when isStat $ div_ [class_ "px-3 py-3 flex-1 flex flex-col justify-end "] do
            div_ [class_ "flex flex-col gap-1"] do
              strong_ [class_ "text-textSuccess-strong text-4xl font-normal", id_ $ chartId <> "Value"]
                $ whenJust valueM toHtml
              div_ [class_ "inline-flex gap-1 items-center text-sm"] do
                whenJust widget.icon \icon -> Utils.faSprite_ icon "regular" "w-4 h-4 text-iconBrand"
                toHtml $ maybeToMonoid widget.title
                Utils.faSprite_ "circle-info" "regular" "w-4 h-4 text-iconNeutral"
          unless (widget.wType == WTStat) $ div_ [class_ $ "h-full w-full flex-1 " <> bool "p-3" "" (isStat || widget.naked == Just True)] do
            div_ [class_ "h-full w-full", id_ $ maybeToMonoid widget.id] ""
            let theme = fromMaybe "default" widget.theme
            let echartOpt = decodeUtf8 $ AE.encode $ widgetToECharts widget
            let yAxisLabel = fromMaybe (maybeToMonoid widget.unit) (widget.yAxis >>= (.label))
            let query = decodeUtf8 $ AE.encode widget.query
            let pid = decodeUtf8 $ AE.encode $ widget._projectId <&> (.toText)
            let querySQL = maybeToMonoid widget.sql
            let chartType = mapWidgetTypeToChartType widget.wType
            let summarizeBy = T.toLower $ T.drop 2 $ show $ fromMaybe SBSum widget.summarizeBy
            let summarizeByPfx = summarizeByPrefix $ fromMaybe SBSum widget.summarizeBy
            let wType = decodeUtf8 $ AE.encode widget.wType
            let legendPos = fromMaybe "bottom" widget.legendPosition
            let widgetUnit = maybeToMonoid widget.unit
            script_
              [type_ "text/javascript"]
              [text|

              // IIFE to avoid global variable conflicts
              (function() {
                // Configuration for this specific widget
                const config = {
                  chartId: "${chartId}",
                  echartOpt: `${echartOpt}`,
                  chartType: '${chartType}',
                  widgetType: ${wType},
                  query: ${query},
                  querySQL: `${querySQL}`,
                  theme: "${theme}",
                  yAxisLabel: "${yAxisLabel}",
                  pid: ${pid},
                  summarizeBy: '${summarizeBy}',
                  summarizeByPrefix: '${summarizeByPfx}',
                  legendPosition: "${legendPos}",
                  unit: "${widgetUnit}"
                };

                // Function to initialize this specific widget
                function initializeThisWidget() {
                  if (typeof window.bindFunctionsToObjects !== 'function' || typeof window.chartWidget !== 'function') {
                    // If dependencies aren't loaded yet, retry after a short delay
                    setTimeout(initializeThisWidget, 100);
                    return;
                  }

                  // Parse chart options
                  const echartOpt = JSON.parse(config.echartOpt, (key, value) => {
                    if (typeof value === 'string' && value.trim().startsWith("function(")) {
                      try {
                        return eval('(' + value + ')');
                      } catch (error) {
                        console.error(`Error evaluating function for key "$${key}":`, error);
                        return value;
                      }
                    }
                    return value;
                  });
                  
                  // Check if the chart element exists
                  const chartEl = document.getElementById(config.chartId);
                  if (!chartEl) return;
                  
                  // Dispose of any existing chart instance before initializing a new one
                  const existingChart = window.echarts && window.echarts.getInstanceByDom(chartEl);
                  if (existingChart) {
                    existingChart.dispose();
                  }
                  
                  window.bindFunctionsToObjects(echartOpt, echartOpt);
                  window.chartWidget({
                    chartType: config.chartType,
                    widgetType: config.widgetType,
                    opt: echartOpt,
                    chartId: config.chartId,
                    query: config.query,
                    querySQL: config.querySQL,
                    theme: config.theme,
                    yAxisLabel: config.yAxisLabel,
                    pid: config.pid,
                    summarizeBy: config.summarizeBy,
                    summarizeByPrefix: config.summarizeByPrefix,
                    legendPosition: config.legendPosition,
                    unit: config.unit
                  });
                }

                // Initialize on page load
                if (document.readyState === 'loading') {
                  window.addEventListener('DOMContentLoaded', initializeThisWidget);
                } else {
                  // DOM already loaded, initialize now
                  initializeThisWidget();
                }

                // Register HTMX event handler
                document.addEventListener('htmx:afterSwap', function(event) {
                  const swappedEl = event.detail.elt;
                  if (swappedEl && swappedEl.contains(document.getElementById(config.chartId))) {
                    initializeThisWidget();
                  }
                });
              })();
            
            |]


-----------------------------------------------------------------------------
-- Echarts Logic
-----------------------------------------------------------------------------

-- -- -- Helper: Select tooltip formatter
-- -- selectFormatter :: WidgetType -> Text
-- -- selectFormatter WTTimeseries = "{a} <br/>{b}: {c}ms"
-- -- selectFormatter _ = "{a} <br/>{b}: {c}"

-- Function to convert Widget to ECharts options
widgetToECharts :: Widget -> AE.Value
widgetToECharts widget =
  let isStat = widget.wType == WTTimeseriesStat
      axisVisibility = not isStat
      gridLinesVisibility = not isStat
      legendVisibility = not isStat && widget.hideLegend /= Just True
   in AE.object
        [ "tooltip"
            AE..= AE.object
              [ "show" AE..= fromMaybe True widget.showTooltip
              , "trigger" AE..= ("axis" :: Text)
              , "axisPointer"
                  AE..= AE.object
                    ["type" AE..= ("shadow" :: Text)]
              , "valueFormatter"
                  AE..= let durationUnits = [Just "ns", Just "μs", Just "us", Just "ms", Just "s", Just "m", Just "h"] :: [Maybe Text]
                            isDuration = widget.unit `elem` durationUnits
                            unit = fromMaybe "" widget.unit
                         in if isDuration
                              then "function(value) { return formatDuration(convertToNanoseconds(value, '" <> unit <> "')); }"
                              else "function(value) { return formatNumber(value); }"
              ]
        , "legend"
            AE..= AE.object
              [ "show" AE..= legendVisibility
              , "type" AE..= "scroll"
              , "top" AE..= fromMaybe "bottom" widget.legendPosition
              , "textStyle" AE..= AE.object ["fontSize" AE..= AE.Number 12] -- // default is usually 12 or 14
              --  Shrink the symbol/icon size
              , "itemWidth" AE..= AE.Number 14 -- default is 25
              , "itemHeight" AE..= AE.Number 12 -- default is 14
              , "itemGap" AE..= AE.Number 8 -- defalt is 10
              , "padding" AE..= AE.Array [AE.Number 2, AE.Number 4, AE.Number 2, AE.Number 4] -- [top, right, bottom, left]
              , "data" AE..= fromMaybe [] (extractLegend widget)
              ]
        , "grid"
            AE..= AE.object
              [ "width" AE..= ("100%" :: Text)
              , "left" AE..= ("0%" :: Text)
              , "top" AE..= if widget.legendPosition == Just "top" && legendVisibility then "18%" else if widget.naked == Just True then "10%" else "5%"
              , "bottom" AE..= if widget.legendPosition /= Just "top" && legendVisibility then "18%" else "1.8%"
              , "containLabel" AE..= True
              , "show" AE..= False
              ]
        , "xAxis"
            AE..= AE.object
              [ "type" AE..= ("time" :: Text)
              , "scale" AE..= True
              , "min" AE..= maybe AE.Null (AE.Number . fromIntegral . (* 1000)) (widget ^? #dataset . _Just . #from . _Just)
              , "max" AE..= maybe AE.Null (AE.Number . fromIntegral . (* 1000)) (widget ^? #dataset . _Just . #to . _Just)
              , "boundaryGap" AE..= ([0, 0.01] :: [Double])
              , "splitLine"
                  AE..= AE.object
                    [ "show" AE..= False
                    ]
              , "axisLine" AE..= AE.object ["show" AE..= axisVisibility, "lineStyle" AE..= AE.object ["color" AE..= "#000833A6", "type" AE..= "solid", "opacity" AE..= 0.1]]
              , "axisLabel" AE..= AE.object ["show" AE..= (axisVisibility && fromMaybe True (widget ^? #xAxis . _Just . #showAxisLabel . _Just))]
              , "show" AE..= (axisVisibility || fromMaybe False (widget ^? #xAxis . _Just . #showAxisLabel . _Just))
              ]
        , "yAxis"
            AE..= AE.object
              [ "type" AE..= ("value" :: Text)
              , "min" AE..= (0 :: Int)
              , "max" AE..= maybe AE.Null (AE.Number . fromFloatDigits) (widget ^? #dataset . _Just . #stats . _Just . #maxGroupSum)
              , "splitLine"
                  AE..= AE.object
                    [ "show" AE..= gridLinesVisibility
                    , "lineStyle" AE..= AE.object ["type" AE..= "dotted", "color" AE..= "#0011661A"]
                    , "interval"
                        AE..= if fromMaybe False $ widget ^? #yAxis . _Just . #showOnlyMaxLabel . _Just
                          then "function(index, value) { return value === this.yAxis.max }"
                          else AE.Null
                    ]
              , "axisTick" AE..= AE.object ["show" AE..= False]
              , "axisLine" AE..= AE.object ["show" AE..= False]
              , "axisLabel"
                  AE..= AE.object
                    [ "show" AE..= (axisVisibility && fromMaybe True (widget ^? #yAxis . _Just . #showAxisLabel . _Just))
                    , "inside" AE..= False
                    , "formatter"
                        AE..= let durationUnits = [Just "ns", Just "μs", Just "us", Just "ms", Just "s", Just "m", Just "h"] :: [Maybe Text]
                                  isDuration = widget.unit `elem` durationUnits
                                  unit = fromMaybe "" widget.unit
                                  fmt = if isDuration then "formatDuration(convertToNanoseconds(value, '" <> unit <> "'))" else "formatNumber(value)"
                                  showOnlyMax = fromMaybe False $ widget ^? #yAxis . _Just . #showOnlyMaxLabel . _Just
                               in if showOnlyMax
                                    then "function(value, index) { return (value === this.yAxis.max || value == 0) ? " <> fmt <> " : ''; }"
                                    else "function(value, index) { return " <> fmt <> "; }"
                    ]
              , "show" AE..= axisVisibility
              ]
        , "dataset"
            AE..= AE.object
              ["source" AE..= maybe AE.Null (.source) widget.dataset]
        , "series" AE..= map (createSeries widget.wType) ([] :: [Maybe Query])
        , "animation" AE..= False
        , if widget.allowZoom == Just True
            then
              "toolbox"
                AE..= AE.object
                  [ "feature" AE..= AE.object ["dataZoom" AE..= AE.object ["show" AE..= True, "yAxisIndex" AE..= "none", "icon" AE..= AE.object ["zoom" AE..= "none", "back" AE..= "none"]]]
                  ]
            else "toolbox" AE..= AE.object []
        ]


-- Helper: Extract legend data
extractLegend :: Widget -> Maybe [Text]
extractLegend widget = fmap (map (fromMaybe "Unnamed Series" . (.query))) widget.queries


-- Helper: Create series
createSeries :: WidgetType -> Maybe Query -> AE.Value
createSeries widgetType query =
  let isStat = widgetType == WTTimeseriesStat
      gradientStyle =
        AE.object
          [ "color"
              AE..= AE.object
                [ "type" AE..= ("linear" :: Text)
                , "x" AE..= (0 :: Int)
                , "y" AE..= (0 :: Int)
                , "x2" AE..= (0 :: Int)
                , "y2" AE..= (1 :: Int)
                -- , "colorStops"
                --     AE..= AE. [ AE.object ["offset" AE..= (0 :: Double), "color" AE..= ("rgba(0, 136, 212, 0.7)" :: Text)]
                --           , AE.object ["offset" AE..= (1 :: Double), "color" AE..= ("rgba(0, 136, 212, 0.1)" :: Text)]
                --           ]
                ]
          ]
   in AE.object
        [ "name" AE..= fromMaybe "Unnamed Series" (query >>= (.query))
        , "type" AE..= mapWidgetTypeToChartType widgetType
        , "stack" AE..= ("Stack" :: Text)
        , "markArea"
            AE..= AE.object
              [ "show" AE..= True
              , "data"
                  AE..= AE.Array V.empty
              ]
        , "showBackground" AE..= not isStat
        , "backgroundStyle"
            AE..= AE.object
              ["color" AE..= ("rgba(240,248,255, 0.4)" :: Text)] -- This will be overridden in JS based on theme
        , "areaStyle" AE..= if isStat then gradientStyle else AE.Null
        , "lineStyle" AE..= AE.object ["width" AE..= if isStat then 0 else 1]
        ]


-- Helper: Map widget type to ECharts chart type
mapWidgetTypeToChartType :: WidgetType -> Text
mapWidgetTypeToChartType WTTimeseries = "bar"
mapWidgetTypeToChartType WTTimeseriesLine = "line"
mapWidgetTypeToChartType WTTimeseriesStat = "line"
mapWidgetTypeToChartType WTDistribution = "bar"
mapWidgetTypeToChartType _ = "bar"


mapChatTypeToWidgetType :: Text -> WidgetType
mapChatTypeToWidgetType "line" = WTTimeseriesLine
mapChatTypeToWidgetType "timeseries_line" = WTTimeseriesLine
mapChatTypeToWidgetType _ = WTTimeseries


-- Server-side table data rendering
renderTableWithData :: Widget -> V.Vector (V.Vector Text) -> Html ()
renderTableWithData widget dataRows = renderTableWithDataAndParams widget dataRows []


renderTableWithDataAndParams :: Widget -> V.Vector (V.Vector Text) -> [(Text, Maybe Text)] -> Html ()
renderTableWithDataAndParams widget dataRows params = do
  let columns = fromMaybe [] widget.columns
  let tableId = maybeToMonoid widget.id
  let currentVar = widget.onRowClick >>= (.setVariable) >>= \var -> find ((== "var-" <> var) . fst) params >>= snd

  -- Render complete table with data
  table_ [class_ "table table-zebra table-sm w-full relative", id_ tableId] do
    -- Table header
    thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"] do
      tr_ [] do
        forM_ (zip columns [0 ..]) \(col, idx) -> do
          th_
            [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" col.align
            , onclick_ $ "window.sortTable('" <> tableId <> "', " <> T.pack (show idx) <> ", this)"
            , data_ "sort-direction" "none"
            ]
            do
              div_ [class_ "flex items-center justify-between"] do
                toHtml col.title
                span_ [class_ "sort-arrow ml-1 text-iconNeutral opacity-0 group-hover:opacity-100", data_ "sort" "none"] "↕"

    -- Table body with data
    tbody_ [] do
      -- Calculate max values for column percentages
      let maxValues = calculateMaxValues columns dataRows

      -- Calculate max formatted width for each progress column
      let valueWidths =
            M.fromList
              [ (col.field, foldr max 5 [T.length (formatColumnValue col (fromMaybe "" $ row V.!? idx)) | row <- V.toList dataRows])
              | (col, idx) <- zip columns [0 ..]
              , isJust col.progress
              ]

      -- Render table rows
      forM_ (V.toList dataRows) \row -> do
        let rowData = AE.object [(K.fromText col.field, AE.String $ getRowValue col idx row) | (col, idx) <- zip columns [0 ..]]
        let firstColValue = maybe "" (\c -> getRowValue c 0 row) (listToMaybe columns)
        let rowValue = case widget.onRowClick >>= (.value) of
              Just tmpl -> T.replace "{{row.resource_name}}" firstColValue tmpl
              Nothing -> firstColValue
        let isSelected = Just rowValue == currentVar

        tr_
          [ class_ $ "hover cursor-pointer" <> if isSelected then " bg-fillBrand/20 border-l-4 border-borderBrand" else ""
          , data_ "row" (decodeUtf8 $ fromLazy $ AE.encode rowData)
          ]
          do
            forM_ (zip columns [0 ..]) \(col, idx) -> do
              let value = getRowValue col idx row
              td_ [class_ $ fromMaybe "" col.align <> if col.columnType `elem` [Just ("number" :: Text), Just ("duration" :: Text)] then " monospace" else ""] do
                if isJust col.progress
                  then renderProgressCell col value maxValues valueWidths
                  else toHtml $ formatColumnValue col value


renderTraceDataTable :: Widget -> V.Vector (V.Vector Text) -> HashMap Text [(Text, Int, Int)] -> Html ()
renderTraceDataTable widget dataRows spGroup = do
  let columns = fromMaybe [] widget.columns
  let tableId = maybeToMonoid widget.id

  -- Render complete table with data
  table_ [class_ "table table-sm w-full relative", id_ tableId] do
    thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"] do
      tr_ [] do
        forM_ (zip columns [0 ..]) \(col, idx) -> do
          th_
            [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" col.align
            , data_ "sort-direction" "none"
            ]
            do
              div_ [class_ "flex items-center justify-between"] do
                toHtml col.title
    tbody_ [] do
      forM_ (V.toList dataRows) \row -> do
        let val = V.last row
        let cdrn = (fromMaybe [] $ HM.lookup val spGroup)
        tr_ [[__|on click toggle .hidden on the next <tr/>|], class_ "cursor-pointer"] do
          forM_ (zip columns [0 ..]) \(col, idx) -> do
            let value = getRowValue col idx row
            if col.field == "latency_breakdown"
              then td_ [class_ "py-2"] do
                renderLatencyBreakdown cdrn
              else td_ [class_ $ fromMaybe "" col.align <> if col.columnType `elem` [Just ("number" :: Text), Just ("duration" :: Text)] then " monospace" else ""] do
                toHtml $ formatColumnValue col value
        tr_ [class_ "hidden"] do
          td_ [colspan_ "100%"] do
            whenJust widget._projectId \p -> do
              div_
                [ class_ "w-full group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                , id_ $ "t" <> val
                ]
                do
                  a_ [hxTrigger_ "intersect once", hxGet_ $ "/p/" <> p.toText <> "/widget/flamegraph/" <> val <> "?shapeView=true", hxTarget_ $ "#t" <> val, hxSwap_ "innerHTML"] pass


renderLatencyBreakdown :: [(Text, Int, Int)] -> Html ()
renderLatencyBreakdown groups = do
  div_ [class_ "flex h-5 overflow-hidden relative bg-fillWeak rounded", style_ $ "width:150px"] $ do
    let totalDur = sum (map (\(_, d, _) -> fromIntegral d) groups) :: Double
    let colors = getServiceColors $ V.fromList (fmap (\(n, _, _) -> n) groups)
    mapM_ (renderGroup totalDur colors) (zip [0 ..] groups)
  where
    renderGroup :: Double -> HM.HashMap Text Text -> (Int, (Text, Int, Int)) -> Html ()
    renderGroup totalDur colors (i, (name, dur, _)) = do
      let barWidth = 150.0
          width = (fromIntegral dur / totalDur) * barWidth
          left =
            if i == 0
              then 0.0
              else (sum (map (\(_, d, _) -> fromIntegral d) (take i groups)) / totalDur) * barWidth
          color = fromMaybe "bg-black" $ HM.lookup name colors
          tooltip = name <> ": " <> T.pack (show (dur `div` 1000000)) <> " ms"
      div_ [class_ ("h-full absolute top-0 border  " <> color), title_ tooltip, style_ $ "width:" <> show width <> "px;" <> "left:" <> show left <> "px;"] pass


-- Helper to get row value by column index or field name
getRowValue :: TableColumn -> Int -> V.Vector Text -> Text
getRowValue col idx row = fromMaybe "" $ row V.!? idx


-- Calculate max values for column percentage progress bars
calculateMaxValues :: [TableColumn] -> V.Vector (V.Vector Text) -> M.Map Text Double
calculateMaxValues columns dataRows =
  M.fromList
    [ (col.field, maxVal)
    | (col, idx) <- zip columns [0 ..]
    , col.progress == Just "column_percent"
    , let values = V.mapMaybe (\row -> row V.!? idx >>= readMaybe . T.unpack) dataRows
    , let maxVal = if V.null values then 0 else V.maximum values
    ]


-- Render a progress bar cell
renderProgressCell :: TableColumn -> Text -> M.Map Text Double -> M.Map Text Int -> Html ()
renderProgressCell col value maxValues valueWidths = do
  div_ [class_ "flex items-center gap-2"] do
    -- Format and display the value using the same logic as formatColumnValue
    let formattedValue = formatColumnValue col value
    -- Use calculated max width for this column
    let width = M.findWithDefault 8 col.field valueWidths

    span_ [class_ "inline-block text-left monospace", style_ $ "width: " <> show width <> "ch"] do
      toHtml formattedValue

    -- Calculate progress percentage
    let numValue = fromMaybe 0 $ readMaybe (T.unpack value) :: Double
    let percentage = case col.progress of
          Just "value_percent" -> min 100 (max 0 numValue)
          Just "column_percent" -> case M.lookup col.field maxValues of
            Just maxVal | maxVal > 0 -> (numValue / maxVal) * 100
            _ -> 0
          _ -> 0

    -- Render progress bar
    let progressClass = "progress w-12 ml-2 " <> getProgressVariantClass col.progressVariant
    progress_ [class_ progressClass, value_ (T.pack $ show percentage), max_ "100"] ""


-- Get progress bar variant class
getProgressVariantClass :: Maybe Text -> Text
getProgressVariantClass variant = case variant of
  Just "error" -> "progress-error"
  Just "warning" -> "progress-warning"
  Just "success" -> "progress-success"
  _ -> "progress-brand"


-- Format column value based on column type
formatColumnValue :: TableColumn -> Text -> Text
formatColumnValue col value = case col.columnType of
  Just "number" ->
    case readMaybe (T.unpack value) :: Maybe Double of
      Just n ->
        let formatted =
              if n < 100 && n /= fromIntegral (round n :: Int)
                then T.pack $ printf "%.2g" n -- Keep significant digits for small numbers
                else prettyPrintCount (round n) -- Use pretty print for larger numbers
         in formatted <> foldMap (" " <>) col.unit
      Nothing -> value <> foldMap (" " <>) col.unit
  Just "duration" ->
    case readMaybe (T.unpack value) :: Maybe Double of
      Just v ->
        -- Convert to nanoseconds based on unit, then format
        let nsValue = case col.unit of
              Just "ms" -> v * 1_000_000 -- milliseconds to nanoseconds
              Just "s" -> v * 1_000_000_000 -- seconds to nanoseconds
              Just "μs" -> v * 1_000 -- microseconds to nanoseconds
              Just "us" -> v * 1_000 -- microseconds to nanoseconds (alt)
              Just "ns" -> v -- already nanoseconds
              _ -> v -- assume nanoseconds if no unit
         in prettyPrintDuration nsValue
      Nothing -> value <> foldMap (" " <>) col.unit
  _ -> value <> foldMap (" " <>) col.unit
