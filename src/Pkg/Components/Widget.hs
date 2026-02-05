{-# LANGUAGE PackageImports #-}

module Pkg.Components.Widget (Widget (..), WidgetDataset (..), toWidgetDataset, widget_, Layout (..), WidgetType (..), TableColumn (..), RowClickAction (..), mapChatTypeToWidgetType, mapWidgetTypeToChartType, widgetToECharts, WidgetAxis (..), SummarizeBy (..), widgetPostH, renderTableWithData, renderTraceDataTable, renderTableWithDataAndParams) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as AE.KeyMap
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
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxExt_, hxGet_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation
import Network.HTTP.Types (urlEncode)
import Pages.Charts.Charts qualified as Charts
import Pages.LogExplorer.LogItem (getServiceName, spanHasErrors)
import Relude
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Effectful (Eff, IOE, (:>))
import Effectful.Log (Log)
import Effectful.Reader.Static qualified
import Log qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData, parseQueryParam)


-- Generic instance for parsing JSON arrays from form data
instance AE.FromJSON a => FromHttpApiData [a] where
  parseQueryParam = first toText . AE.eitherDecodeStrict . encodeUtf8


-- Generic instance for parsing JSON values from form data
instance {-# OVERLAPPABLE #-} AE.FromJSON a => FromHttpApiData a where
  parseQueryParam = first toText . AE.eitherDecodeStrict . encodeUtf8


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
  | WTList -- https://docs.datadoghq.com/dashboards/widgets/list/ not supported yet
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
  | WTServiceMap -- Service dependency graph visualization
  | WTHeatmap -- Latency distribution heatmap
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
  , rawQuery :: Maybe Text -- Original KQL query with {{const-...}} placeholders (for editor display)
  , summarizeBy :: Maybe SummarizeBy
  , query :: Maybe Text
  , queries :: Maybe [Query] -- Multiple queries for combined visualizations
  , layout :: Maybe Layout -- Layout (x, y, w, h)
  , xAxis :: Maybe WidgetAxis
  , yAxis :: Maybe WidgetAxis -- Optional y-axis label
  , unit :: Maybe Text
  , value :: Maybe Double -- value could represent a number or a count
  , wData :: Maybe AE.Value
  , hideLegend :: Maybe Bool
  , legendPosition :: Maybe Text -- Legend position: "top", "bottom", "top-right", "top-left", "bottom-right", "bottom-left"
  , legendSize :: Maybe Text -- Legend size: "xs" (default), "sm", "md"
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
  -- Alert fields (populated from QueryMonitor at render time)
  , alertId :: Maybe Text -- Linked QueryMonitor ID
  , alertThreshold :: Maybe Double -- For threshold line rendering
  , warningThreshold :: Maybe Double
  , showThresholdLines :: Maybe Text -- 'always' | 'on_breach' | 'never'
  , alertStatus :: Maybe Text -- 'normal' | 'warning' | 'alerting' (runtime)
  , description :: Maybe Text -- Help text shown in info icon tooltip
  , pngUrl :: Maybe Text -- Pre-signed PNG download URL (runtime)
  , _staticRender :: Maybe Bool -- For PNG export: disables scroll legend
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


-- | Convert MetricsData to WidgetDataset (timestamps already in ms from queryMetrics)
toWidgetDataset :: Charts.MetricsData -> WidgetDataset
toWidgetDataset md =
  WidgetDataset
    { source = AE.toJSON $ V.cons (AE.toJSON <$> md.headers) (AE.toJSON <<$>> md.dataset)
    , rowsPerMin = md.rowsPerMin
    , value = Just md.rowsCount
    , from = md.from
    , to = md.to
    , stats = md.stats
    }


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


renderRowClickScript :: Text -> RowClickAction -> Maybe [TableColumn] -> Html ()
renderRowClickScript tableId onRowClick columns = do
  let onRowClickJson = decodeUtf8 $ AE.encode onRowClick
      columnsJson = decodeUtf8 $ AE.encode columns
  script_
    [type_ "text/javascript"]
    [text|
    (function() {
      const tableId = '${tableId}';
      const onRowClick = ${onRowClickJson};
      const columns = ${columnsJson};

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


-- Used when converting a widget json to its html representation. Eg in a query chart builder
widgetPostH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Widget -> ATAuthCtx (RespHeaders Widget)
widgetPostH pid sinceM fromM toM widget = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let widgetWithPid = widget & #_projectId ?~ pid
  pngUrl <- widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid widgetWithPid sinceM fromM toM
  addRespHeaders $ if T.null pngUrl then widgetWithPid else widgetWithPid{pngUrl = Just pngUrl}


widgetPngUrl :: (IOE :> es, Log :> es) => Text -> Text -> Projects.ProjectId -> Widget -> Maybe Text -> Maybe Text -> Maybe Text -> Eff es Text
widgetPngUrl secret hostUrl pid widget since from to =
  let widgetJson = decodeUtf8 @Text $ toStrict $ AE.encode widget
      encodedJson = decodeUtf8 @Text $ urlEncode True $ encodeUtf8 widgetJson
      sig = signWidgetUrl secret pid widgetJson
      timeParams = foldMap (\(k, mv) -> maybe "" (\v -> "&" <> k <> "=" <> v) mv) ([("since", since), ("from", from), ("to", to)] :: [(Text, Maybe Text)])
      url = hostUrl <> "p/" <> pid.toText <> "/widget.png?widgetJSON=" <> encodedJson <> timeParams <> "&sig=" <> sig
   in if T.length url > 8000 then Log.logAttention "Widget PNG URL too large" (AE.object ["projectId" AE..= pid, "urlLength" AE..= T.length url]) >> pure "" else pure url


signWidgetUrl :: Text -> Projects.ProjectId -> Text -> Text
signWidgetUrl secret pid widgetJson =
  let payload = pid.toText <> ":" <> widgetJson
   in decodeUtf8 @Text $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) (encodeUtf8 payload :: ByteString) :: HMAC.HMAC SHA256)


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ = widgetHelper_


widgetHelper_ :: Widget -> Html ()
widgetHelper_ w' = case w.wType of
  WTAnomalies -> gridItem_ $ div_ [class_ $ "h-full group/wgt " <> paddingBtm] $ div_ [class_ "gap-0.5 flex flex-col h-full"] do
    unless (w.naked == Just True) $ renderWidgetHeader w (maybeToMonoid w.id) w.title Nothing Nothing Nothing (Just ("View all", "/p/" <> maybeToMonoid (w._projectId <&> (.toText)) <> "/anomalies")) (w.hideSubtitle == Just True)
    div_ [class_ "flex-1 flex min-h-0"] $ div_ [class_ $ "h-full w-full " <> if w.naked == Just True then "" else "surface-raised rounded-2xl", id_ $ maybeToMonoid w.id <> "_bordered"] $ div_ [class_ "h-full overflow-auto p-3"] $ whenJust w.html toHtmlRaw
  WTGroup -> gridItem_ $ div_ [class_ "h-full flex flex-col border border-strokeWeak rounded-lg surface-raised overflow-hidden group/wgt"] do
    -- Header: auto height (no flex), group-header class for CSS targeting when collapsed
    div_ [class_ $ "group-header py-2 px-4 flex items-center justify-between " <> gridStackHandleClass] do
      div_ [class_ "inline-flex gap-2 items-center group/h"] do
        span_ [class_ "hidden group-hover/h:inline-flex cursor-move"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
        whenJust w.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-5 h-5"
        span_ ([class_ "text-lg font-medium"] <> foldMap (\t -> [data_ "var-template" t | "{{var-" `T.isInfixOf` t]) w.title) $ toHtml $ maybeToMonoid w.title
        whenJust w.description \desc -> span_ [class_ "hidden group-hover/wgt:inline-flex items-center", data_ "tippy-content" desc] $ Utils.faSprite_ "circle-info" "regular" "w-4 h-4"
      -- Collapse chevron: only for full-width groups
      when isFullWidth $ button_ [class_ "collapse-toggle p-2 rounded hover:bg-fillWeak transition-colors cursor-pointer tap-target", Aria.label_ "Toggle group", [__|on click toggle .hidden on .nested-grid in closest .grid-stack-item then toggle .collapsed on closest .grid-stack-item|]] $ Utils.faSprite_ "chevron-up" "regular" "w-5 h-5 transition-transform"
    -- Nested grid: flex-1 fills remaining space
    div_ [class_ "grid-stack nested-grid flex-1"] $ forM_ (fromMaybe [] w.children) (\wChild -> widgetHelper_ (wChild{_isNested = Just True}))
  WTTable -> gridItem_ $ div_ [class_ $ "h-full group/wgt " <> paddingBtm] $ renderTable w
  WTLogs -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ div_ [class_ "p-3"] "Logs widget coming soon"
  WTTraces -> gridItem_ $ div_ [class_ $ "h-full group/wgt " <> paddingBtm] $ renderTraceTable w
  WTFlamegraph -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ div_ [class_ "p-3"] "Flamegraph widget coming soon"
  _ -> gridItem_ $ div_ [class_ $ " w-full h-full group/wgt " <> paddingBtm] $ renderChart w
  where
    w = w' & #id %~ maybe (slugify <$> w'.title) Just
    gridStackHandleClass = if w._isNested == Just True then "nested-grid-stack-handle" else "grid-stack-handle"
    isFullWidth = (== Just 12) $ w.layout >>= (.w)
    groupRequiredHeight = case w.wType of
      WTGroup ->
        let childWidgets = fromMaybe [] w.children
            maxRow = foldl' (\acc c -> max acc $ fromMaybe 0 (c.layout >>= (.y)) + fromMaybe 1 (c.layout >>= (.h))) 1 childWidgets
         in Just (1 + maxRow)
      _ -> Nothing
    -- For groups: full-width uses requiredHeight, partial-width uses max(yamlH, requiredHeight)
    effectiveHeight = case groupRequiredHeight of
      Just reqH -> Just $ if isFullWidth then reqH else maybe reqH (max reqH) (w.layout >>= (.h))
      Nothing -> w.layout >>= (.h)
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w))] :: [(Text, Layout -> Maybe Int)]
    attrs =
      foldMap (\(name, field) -> foldMap (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= field)) layoutFields
        <> foldMap (\h -> [term "gs-h" (show h)]) effectiveHeight
    paddingBtm
      | w.standalone == Just True = ""
      | otherwise = ""
    widgetJson = decodeUtf8 $ fromLazy $ AE.encode w
    gridItem_ =
      if w.naked == Just True
        then Relude.id
        else div_ ([class_ "grid-stack-item h-full flex-1 [.nested-grid_&]:overflow-hidden ", id_ $ maybeToMonoid w.id <> "_widgetEl", data_ "widget" widgetJson] <> attrs) . div_ [class_ "grid-stack-item-content h-full [.grid-stack_&]:h-auto"]


renderWidgetHeader :: Widget -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text) -> Bool -> Html ()
renderWidgetHeader widget wId title valueM subValueM expandBtnFn ctaM hideSub = div_ [class_ $ "leading-none flex justify-between items-center  " <> bool "grid-stack-handle" "" (widget.standalone == Just True), id_ $ wId <> "_header"] do
  when (widget._centerTitle == Just True) $ div_ ""
  div_ [class_ "inline-flex gap-3 items-center group/h"] do
    span_ [class_ "text-sm text-textWeak flex items-center gap-1"] do
      unless (widget.standalone == Just True) $ span_ [class_ "hidden group-hover/h:inline-flex"] $ Utils.faSprite_ "grip-dots-vertical" "regular" "w-4 h-4"
      whenJust widget.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
      span_ (foldMap (\t -> [data_ "var-template" t | "{{var-" `T.isInfixOf` t]) title) $ toHtml $ maybeToMonoid title
      whenJust widget.description \desc -> span_ [class_ "hidden group-hover/wgt:inline-flex items-center", data_ "tippy-content" desc] $ Utils.faSprite_ "circle-info" "regular" "w-4 h-4"
    span_ [class_ $ "bg-fillWeak border border-strokeWeak text-sm font-semibold px-2 py-1 rounded-3xl leading-none text-textWeak " <> if isJust valueM then "" else "hidden", id_ $ wId <> "Value"]
      $ whenJust valueM toHtml
    span_ ([class_ $ "text-textDisabled widget-subtitle text-sm " <> bool "" "hidden" hideSub, id_ $ wId <> "Subtitle"] <> foldMap (\t -> [data_ "var-template" t | "{{var-" `T.isInfixOf` t]) subValueM) $ toHtml $ maybeToMonoid subValueM
    -- Add hidden loader with specific ID that can be toggled from JS
    span_ [class_ "hidden", id_ $ wId <> "_loader"] $ Utils.faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
  div_ [class_ "text-iconNeutral flex items-center gap-0.5"] do
    -- Alert status indicator (visible on hover, always visible when alerting/warning)
    whenJust widget.alertId \_ -> do
      let (iconColor, iconType, tooltip) = case widget.alertStatus of
            Just "alerting" -> ("text-fillError-strong", "bell-exclamation", "Alert triggered")
            Just "warning" -> ("text-fillWarning-strong", "bell", "Warning threshold exceeded")
            _ -> ("text-iconNeutral", "bell", "Alert configured")
          visibilityClass = case widget.alertStatus of
            Just "alerting" -> ""
            Just "warning" -> ""
            _ -> "opacity-0 group-hover/wgt:opacity-100 touch:opacity-50"
      span_
        [ class_ $ "p-1 transition-opacity " <> visibilityClass
        , data_ "tippy-content" tooltip
        , id_ $ wId <> "_alert_indicator"
        ]
        $ Utils.faSprite_ iconType "regular" ("w-3.5 h-3.5 " <> iconColor)

    whenJust ctaM \(ctaTitle, uri) -> a_ [class_ "underline underline-offset-2 text-textBrand", href_ uri] $ toHtml ctaTitle
    whenJust expandBtnFn \fn ->
      button_
        [ term "_" fn
        , class_ "p-2 cursor-pointer tap-target"
        , data_ "tippy-content" "Expand widget"
        ]
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    when (isJust widget._dashboardId)
      $ let pid = maybeToMonoid (widget._projectId <&> (.toText))
            dashId = maybeToMonoid widget._dashboardId
         in button_
              [ class_ "p-2 cursor-pointer opacity-0 group-hover/wgt:opacity-100 touch:opacity-100 tap-target transition-opacity"
              , title_ "Expand widget"
              , data_ "tippy-content" "Expand widget"
              , data_ "expand-btn" wId
              , term
                  "_"
                  [text| on pointerdown or click
            add .pointer-events-none to me
            set :icon to my.querySelector('svg')
            if :icon then add .animate-spin to :icon end
            js { const url = new URL(window.location); url.searchParams.set('expand', '${wId}'); history.replaceState({}, '', url); } end
            set #global-data-drawer.checked to true
            then set #global-data-drawer-content.innerHTML to #loader-tmp.innerHTML
            then fetch `/p/${pid}/dashboards/${dashId}/widgets/${wId}/expand`
            then if :icon then remove .animate-spin from :icon end
            then remove .pointer-events-none from me
            then set #global-data-drawer-content.innerHTML to it
            then htmx.process(#global-data-drawer-content)
            then _hyperscript.processNode(#global-data-drawer-content)
            then window.evalScriptsFromContent(#global-data-drawer-content)
         |]
              ]
              $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    details_ [class_ "dropdown dropdown-end"] do
      summary_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg tap-target", Aria.label_ "Widget menu", data_ "tippy-content" "Widget Menu"]
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
        li_
          $ a_
            [ class_ "p-2 w-full text-left block cursor-pointer"
            , data_ "tippy-content" "Copy generated SQL to clipboard"
            , term
                "_"
                [text|
                on click
                set widgetEl to the closest <[data-widget]/>
                set widgetData to JSON.parse(widgetEl.dataset.widget)
                set sql to widgetData.sql or widgetData.query or 'No SQL available'
                if 'clipboard' in window.navigator then
                  call navigator.clipboard.writeText(sql)
                  send successToast(value:['SQL copied to clipboard']) to <body/>
                end
              |]
            ]
            "Copy SQL"
        li_
          $ a_
            [ class_ "p-2 w-full text-left block cursor-pointer"
            , data_ "tippy-content" "Copy KQL query to clipboard"
            , term
                "_"
                [text|
                on click
                set widgetEl to the closest <[data-widget]/>
                set widgetData to JSON.parse(widgetEl.dataset.widget)
                set kql to widgetData.query or 'No KQL available'
                if 'clipboard' in window.navigator then
                  call navigator.clipboard.writeText(kql)
                  send successToast(value:['KQL copied to clipboard']) to <body/>
                end
              |]
            ]
            "Copy KQL"
        whenJust widget.pngUrl \url ->
          li_
            $ a_
              [ class_ "p-2 w-full text-left block cursor-pointer"
              , data_ "tippy-content" "Download widget as PNG image"
              , href_ url
              , download_ $ maybeToMonoid widget.title <> ".png"
              , target_ "_blank"
              ]
              "Download PNG"

        -- Only show the "Duplicate widget" option if we're in a dashboard context
        when (isJust widget._dashboardId) do
          li_
            $ a_
              [ class_ "p-2 w-full text-left block cursor-pointer"
              , data_ "tippy-content" "Create a copy of this widget"
              , hxPost_ ("/p/" <> maybeToMonoid (widget._projectId <&> (.toText)) <> "/dashboards/" <> maybeToMonoid widget._dashboardId <> "/widgets/" <> wId <> "/duplicate")
              , hxTrigger_ "click"
              , hxSwap_ "none"
              , [__| on click set (the closest <details/>).open to false
                     on htmx:beforeSwap
                        set event.detail.shouldSwap to false then
                        set widgetData to JSON.parse(event.detail.xhr.getResponseHeader('X-Widget-JSON')) then
                        set gridEl to me.closest('.grid-stack') then
                        set layout to widgetData.layout or {w: 3, h: 3} then
                        make a <template/> called tpl then
                        set tpl.innerHTML to event.detail.serverResponse then
                        set widgetEl to tpl.content.firstElementChild then
                        set newId to widgetEl.id then
                        call gridEl.gridstack.addWidget(widgetEl, {w: layout.w, h: layout.h}) then
                        set addedEl to document.getElementById(newId) then
                        js(addedEl) htmx.process(addedEl); _hyperscript.processNode(addedEl); window.evalScriptsFromContent(addedEl) end
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
            <> if widget.naked == Just True then "" else "surface-raised rounded-2xl"
        , id_ $ tableId <> "_bordered"
        ]
        do
          -- Single scrollable table container
          div_
            [ class_ "h-full overflow-auto p-3"
            , hxGet_ $ "/p/" <> maybe "" (.toText) widget._projectId <> "/widget?widgetJSON=" <> decodeUtf8 (urlEncode True widgetJson)
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
                          ifor_ (["Resource", "Span name", "Duration", "Latency breakdown"] :: [Text]) \idx col ->
                            th_
                              [ class_ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group "
                              , onclick_ $ "window.sortTable('" <> tableId <> "', " <> show (idx :: Int) <> ", this)"
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
                        $ loadingIndicator_ LdSM LdSpinner
      script_ [type_ "text/javascript"] """htmx.process(".widget-target")"""


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
            <> if widget.naked == Just True then "" else "surface-raised rounded-2xl"
        , id_ $ tableId <> "_bordered"
        ]
        do
          -- Single scrollable table container
          div_
            [ class_ "h-full overflow-auto p-3"
            , hxGet_ $ "/p/" <> maybe "" (.toText) widget._projectId <> "/widget?widgetJSON=" <> widgetJson
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
                          ifor_ (fromMaybe [] widget.columns) \idx col ->
                            th_
                              [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" col.align
                              , onclick_ $ "window.sortTable('" <> tableId <> "', " <> show (idx :: Int) <> ", this)"
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
                        $ loadingIndicator_ LdSM LdSpinner

    -- Add row click handler script if needed
    whenJust widget.onRowClick \action -> renderRowClickScript tableId action widget.columns


-- | Render stat widget content with HTMX lazy loading support
-- Always includes HTMX attributes so widget can refresh on update-query events
renderStatContent :: Widget -> Text -> Maybe Text -> Html ()
renderStatContent widget chartId valueM = do
  let statContentId = chartId <> "_stat"
      hasData = widget.eager == Just True || isJust (widget.dataset >>= (.value))
      paddingClass = "px-3 flex flex-col " <> bool "py-3 " "py-2 " (widget._isNested == Just True)
      -- Always use eager widget JSON for HTMX requests
      eagerWidget = widget & #eager ?~ True
      widgetJson = fromLazy $ AE.encode eagerWidget
  -- Always include HTMX attributes for refresh capability
  div_
    [ id_ statContentId
    , class_ paddingClass
    , hxGet_ $ "/p/" <> maybe "" (.toText) widget._projectId <> "/widget?widgetJSON=" <> decodeUtf8 (urlEncode True widgetJson)
    , hxTrigger_ $ if hasData then "update-query from:window" else "load, update-query from:window"
    , hxTarget_ $ "#" <> statContentId
    , hxSelect_ $ "#" <> statContentId
    , hxSwap_ "outerHTML"
    , hxExt_ "forward-page-params"
    ]
    $ if hasData
      then renderStatValue widget chartId valueM
      else renderStatPlaceholder widget chartId


-- | Render placeholder with loading spinner for lazy-loaded stats
renderStatPlaceholder :: Widget -> Text -> Html ()
renderStatPlaceholder widget chartId = div_ [class_ "flex flex-col gap-1"] do
  strong_ [class_ "text-textSuccess-strong text-4xl font-normal", id_ $ chartId <> "Value"]
    $ loadingIndicator_ LdSM LdSpinner
  div_ [class_ "inline-flex gap-1 items-center text-sm"] do
    whenJust widget.icon \icon -> Utils.faSprite_ icon "regular" "w-4 h-4 text-iconBrand"
    toHtml $ maybeToMonoid widget.title
    whenJust widget.description \desc -> span_ [class_ "hidden group-hover/wgt:inline-flex items-center", data_ "tippy-content" desc] $ Utils.faSprite_ "circle-info" "regular" "w-4 h-4 text-iconNeutral"


-- | Render actual stat value content
renderStatValue :: Widget -> Text -> Maybe Text -> Html ()
renderStatValue widget chartId valueM = div_ [class_ "flex flex-col gap-1"] do
  strong_ [class_ "text-textSuccess-strong text-4xl font-normal", id_ $ chartId <> "Value"]
    $ whenJust valueM toHtml
  div_ [class_ "inline-flex gap-1 items-center text-sm"] do
    whenJust widget.icon \icon -> Utils.faSprite_ icon "regular" "w-4 h-4 text-iconBrand"
    toHtml $ maybeToMonoid widget.title
    whenJust widget.description \desc -> span_ [class_ "hidden group-hover/wgt:inline-flex items-center", data_ "tippy-content" desc] $ Utils.faSprite_ "circle-info" "regular" "w-4 h-4 text-iconNeutral"


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
    div_ [class_ $ "flex-1 flex min-h-0 " <> bool "" gridStackHandleClass isStat] do
      div_
        [ class_
            $ "h-full w-full flex flex-col justify-end "
            <> bool "min-h-0 " "" isStat
            <> if widget.naked == Just True then "" else "surface-raised rounded-2xl"
        , id_ $ chartId <> "_bordered"
        ]
        do
          when isStat $ renderStatContent widget chartId valueM
          unless (widget.wType == WTStat) $ div_ [class_ $ "h-0 max-h-full overflow-hidden w-full flex-1 min-h-0" <> bool " p-2" "" isStat] do
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
                  if (!window.widgetDepsReady) {
                    // Dependencies not ready - wait for DOMContentLoaded (modules load before it fires)
                    if (document.readyState === 'loading') {
                      document.addEventListener('DOMContentLoaded', initializeThisWidget, { once: true });
                    } else {
                      // DOM loaded but deps still not ready - rare case, short retry
                      setTimeout(initializeThisWidget, 50);
                    }
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

-- Helper: Extract series names from dataset source (headers[1:])
extractSeriesNamesFromDataset :: Maybe WidgetDataset -> [Text]
extractSeriesNamesFromDataset (Just wd) = case wd.source of
  AE.Array arr | not (V.null arr) -> case V.head arr of
    AE.Array headers | V.length headers > 1 -> mapMaybe getText $ V.toList $ V.tail headers
    _ -> []
  _ -> []
  where
    getText (AE.String t) = Just t; getText _ = Nothing
extractSeriesNamesFromDataset Nothing = []


-- Function to convert Widget to ECharts options
widgetToECharts :: Widget -> AE.Value
widgetToECharts widget =
  let isStat = widget.wType == WTTimeseriesStat
      axisVisibility = not isStat
      gridLinesVisibility = not isStat
      legendVisibility = not isStat && widget.hideLegend /= Just True
      seriesNames = extractSeriesNamesFromDataset widget.dataset
      -- Detect categorical widget types (no time axis)
      isCategorical = widget.wType `elem` [WTDistribution, WTPieChart, WTTopList, WTTreeMap, WTFunnel]
      xAxisType = if isCategorical then "category" else "time"
   in AE.object
        [ "tooltip"
            AE..= AE.object
              [ "show" AE..= fromMaybe True widget.showTooltip
              , "trigger" AE..= ("axis" :: Text)
              , "appendToBody" AE..= True
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
              ( let pos = fromMaybe "bottom" widget.legendPosition
                    (vPos, hPos) = case T.splitOn "-" pos of
                      [v, h] | h == "right" || h == "left" -> (v, Just h)
                      [v] -> (v, Nothing)
                      _ -> ("bottom", Nothing)
                    (fontSize, itemSize, itemGap, pad) = case fromMaybe "sm" widget.legendSize of
                      "xs" -> (10 :: Int, 6 :: Int, 6 :: Int, [2, 4, 2, 4] :: [Int])
                      "md" -> (14, 12, 12, [4, 8, 4, 8])
                      "lg" -> (16, 14, 14, [5, 10, 5, 10])
                      _ -> (12, 9, 9, [3, 6, 3, 6]) -- sm (default)
                    isStatic = widget._staticRender == Just True
                 in [ "show" AE..= legendVisibility
                    , "type" AE..= if isStatic then "plain" else "scroll"
                    , "top" AE..= vPos
                    , "textStyle" AE..= AE.object ["fontSize" AE..= AE.Number (fromIntegral fontSize), "padding" AE..= AE.Array [AE.Number 0, AE.Number 0, AE.Number 0, AE.Number (-2)]]
                    , "itemWidth" AE..= AE.Number (fromIntegral itemSize)
                    , "itemHeight" AE..= AE.Number (fromIntegral itemSize)
                    , "itemGap" AE..= AE.Number (fromIntegral itemGap)
                    , "padding" AE..= AE.Array (V.fromList $ map (AE.Number . fromIntegral) pad)
                    , "data" AE..= fromMaybe seriesNames (extractLegend widget) -- Use series names from dataset if no explicit queries
                    ]
                      <> [K.fromText h AE..= (0 :: Int) | Just h <- [hPos]]
              )
        , "grid"
            AE..= AE.object
              [ "width" AE..= ("100%" :: Text)
              , "left" AE..= ("0%" :: Text)
              , "top" AE..= if maybe False (T.isPrefixOf "top") widget.legendPosition && legendVisibility then "14%" else if widget.naked == Just True then "10%" else "5%"
              , "bottom" AE..= if not (maybe False (T.isPrefixOf "top") widget.legendPosition) && legendVisibility then "14%" else "1.8%"
              , "containLabel" AE..= True
              , "show" AE..= False
              ]
        , "xAxis"
            AE..= AE.object
              ( [ "type" AE..= xAxisType
                , "scale" AE..= True
                , "boundaryGap" AE..= if isCategorical then AE.Bool True else AE.Array (V.fromList [AE.Number 0, AE.Number 0.01])
                , "splitLine" AE..= AE.object ["show" AE..= False]
                , "axisLine" AE..= AE.object ["show" AE..= axisVisibility, "lineStyle" AE..= AE.object ["color" AE..= "#000833A6", "type" AE..= "solid", "opacity" AE..= 0.1]]
                , "axisLabel" AE..= AE.object ["show" AE..= (axisVisibility && fromMaybe True (widget ^? #xAxis . _Just . #showAxisLabel . _Just))]
                , "show" AE..= (axisVisibility || fromMaybe False (widget ^? #xAxis . _Just . #showAxisLabel . _Just))
                ]
                  <> if isCategorical
                    then [] -- For categorical, ECharts will derive categories from dataset
                    else
                      [ "min" AE..= maybe AE.Null (AE.Number . fromIntegral) (widget ^? #dataset . _Just . #from . _Just)
                      , "max" AE..= maybe AE.Null (AE.Number . fromIntegral) (widget ^? #dataset . _Just . #to . _Just)
                      ]
              )
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
        , "series" AE..= addMarkLinesToFirstSeries widget (createSeriesFromHeaders widget.wType seriesNames)
        , "animation" AE..= False
        , if widget.allowZoom == Just True
            then
              "toolbox"
                AE..= AE.object
                  [ "feature" AE..= AE.object ["dataZoom" AE..= AE.object ["show" AE..= True, "yAxisIndex" AE..= "none", "icon" AE..= AE.object ["zoom" AE..= "none", "back" AE..= "none"]]]
                  ]
            else "toolbox" AE..= AE.object []
        ]


-- Helper: Add markLines to first series for alert thresholds
addMarkLinesToFirstSeries :: Widget -> [AE.Value] -> [AE.Value]
addMarkLinesToFirstSeries widget series
  | shouldShowLines
  , not (null markLineData) = case series of
      [] -> series
      (s : rest) -> addMarkLine s : rest
  | otherwise = series
  where
    shouldShowLines = case widget.showThresholdLines of
      Just "never" -> False
      Just "on_breach" -> widget.alertStatus == Just "warning" || widget.alertStatus == Just "alerting"
      _ -> isJust widget.alertThreshold || isJust widget.warningThreshold

    mkMarkLine color label threshold =
      AE.object
        [ "yAxis" AE..= threshold
        , "lineStyle" AE..= AE.object ["color" AE..= (color :: Text), "type" AE..= ("dashed" :: Text), "width" AE..= (2 :: Int)]
        , "label" AE..= AE.object ["formatter" AE..= ((label <> ": {c}") :: Text), "position" AE..= ("insideEndTop" :: Text)]
        ]

    markLineData :: [AE.Value]
    markLineData =
      catMaybes
        [ widget.alertThreshold <&> mkMarkLine "#dc2626" "Alert"
        , widget.warningThreshold <&> mkMarkLine "#f59e0b" "Warning"
        ]

    addMarkLine :: AE.Value -> AE.Value
    addMarkLine (AE.Object obj) = AE.Object $ AE.KeyMap.insert (K.fromText "markLine") markLineObj obj
    addMarkLine v = v

    markLineObj =
      AE.object
        [ "silent" AE..= True
        , "symbol" AE..= ("none" :: Text)
        , "data" AE..= markLineData
        ]


-- Helper: Extract legend data
extractLegend :: Widget -> Maybe [Text]
extractLegend widget = fmap (map (fromMaybe "Unnamed Series" . (.query))) widget.queries


-- Helper: Create series from dataset headers with colors and encode for dataset binding
createSeriesFromHeaders :: WidgetType -> [Text] -> [AE.Value]
createSeriesFromHeaders wType = zipWith (createSeries wType) [1 ..]


-- Helper: Create a single series with name, column index, and color
createSeries :: WidgetType -> Int -> Text -> AE.Value
createSeries widgetType colIdx name =
  let isStat = widgetType == WTTimeseriesStat
      gradientStyle = AE.object ["color" AE..= AE.object ["type" AE..= ("linear" :: Text), "x" AE..= (0 :: Int), "y" AE..= (0 :: Int), "x2" AE..= (0 :: Int), "y2" AE..= (1 :: Int)]]
   in AE.object
        [ "name" AE..= name
        , "type" AE..= mapWidgetTypeToChartType widgetType
        , "stack" AE..= ("Stack" :: Text)
        , "encode" AE..= AE.object ["x" AE..= (0 :: Int), "y" AE..= colIdx]
        , "itemStyle" AE..= AE.object ["color" AE..= getSeriesColorHex name]
        , "showBackground" AE..= not isStat
        , "backgroundStyle" AE..= AE.object ["color" AE..= ("rgba(240,248,255, 0.4)" :: Text)]
        , "areaStyle" AE..= if isStat then gradientStyle else AE.Null
        , "lineStyle" AE..= AE.object ["width" AE..= if isStat then 0 else 1]
        ]


-- Helper: Map widget type to ECharts chart type
mapWidgetTypeToChartType :: WidgetType -> Text
mapWidgetTypeToChartType WTTimeseries = "bar"
mapWidgetTypeToChartType WTTimeseriesLine = "line"
mapWidgetTypeToChartType WTTimeseriesStat = "line"
mapWidgetTypeToChartType WTDistribution = "bar"
mapWidgetTypeToChartType WTServiceMap = "graph" -- ECharts force-directed graph
mapWidgetTypeToChartType WTHeatmap = "heatmap" -- ECharts heatmap
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
      tableId = maybeToMonoid widget.id
      currentVar = widget.onRowClick >>= (.setVariable) >>= \var -> find ((== "var-" <> var) . fst) params >>= snd

  -- Render complete table with data
  table_ [class_ "table table-zebra table-sm w-full relative", id_ tableId] do
    -- Table header
    thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"] do
      tr_ [] do
        ifor_ columns \idx col ->
          th_
            [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" col.align
            , onclick_ $ "window.sortTable('" <> tableId <> "', " <> show (idx :: Int) <> ", this)"
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
              [ (col.field, V.foldl' (\acc row -> max acc (T.length $ formatColumnValue col (fromMaybe "" $ row V.!? idx))) 5 dataRows)
              | (col, idx) <- zip columns [0 ..]
              , isJust col.progress
              ]

      -- Render table rows
      V.forM_ dataRows \row -> do
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
            ifor_ columns \idx col -> do
              let value = getRowValue col idx row
              td_ [class_ $ fromMaybe "" col.align <> if col.columnType `elem` [Just ("number" :: Text), Just ("duration" :: Text)] then " monospace" else ""] do
                if isJust col.progress
                  then renderProgressCell col value maxValues valueWidths
                  else toHtml $ formatColumnValue col value

  -- Add row click handler script if needed
  whenJust widget.onRowClick \action -> renderRowClickScript tableId action widget.columns


renderTraceDataTable :: Widget -> V.Vector (V.Vector Text) -> HashMap Text [(Text, Int, Int)] -> HashMap Text [Telemetry.SpanRecord] -> Text -> Html ()
renderTraceDataTable widget dataRows spGroup spansGrouped colorsJson = do
  let columns = fromMaybe [] widget.columns
  let tableId = maybeToMonoid widget.id

  -- Render complete table with data
  table_ [class_ "table table-sm w-full relative", id_ tableId] do
    thead_ [class_ "sticky top-0 z-10 before:content-[''] before:absolute before:left-0 before:right-0 before:bottom-0 before:h-px before:bg-strokeWeak"] do
      tr_ [] do
        forM_ columns \col ->
          th_
            [ class_ $ "text-left bg-bgRaised sticky top-0 cursor-pointer hover:bg-fillWeak transition-colors group " <> fromMaybe "" col.align
            , data_ "sort-direction" "none"
            ]
            do
              div_ [class_ "flex items-center justify-between"] do
                toHtml col.title
    tbody_ [] do
      V.forM_ dataRows \row -> do
        let val = V.last row
        let cdrn = fromMaybe [] $ HM.lookup val spGroup
        let spansJson =
              ( \x ->
                  let targ = find (\(n, _, _) -> n == x.spanName) cdrn
                   in getSpanJson targ x
              )
                <$> fromMaybe [] (HM.lookup val spansGrouped)
        let spjson = decodeUtf8 $ fromLazy $ AE.encode spansJson
        let clcFun = [text|on click toggle .hidden on the next <tr/> then call flameGraphChart($spjson, "$val", $colorsJson)|]
        tr_ [term "_" clcFun, class_ "cursor-pointer"] do
          ifor_ columns \idx col -> do
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
                , id_ $ "flame-graph-container-" <> val
                ]
                do
                  renderFlameGraph val


getSpanJson :: Maybe (Text, Int, Int) -> Telemetry.SpanRecord -> AE.Value
getSpanJson tgtM sp =
  AE.object
    [ "spanId" AE..= sp.spanId
    , "name" AE..= sp.spanName
    , "value" AE..= maybe sp.spanDurationNs (\(_, d, _) -> fromIntegral d) tgtM
    , "start" AE..= start
    , "parentId" AE..= sp.parentSpanId
    , "serviceName" AE..= getServiceName sp.resource
    , "hasErrors" AE..= spanHasErrors sp
    , "totalSpans" AE..= maybe 1 (\(_, _, c) -> c) tgtM
    ]
  where
    start = utcTimeToNanoseconds sp.startTime


renderFlameGraph :: Text -> Html ()
renderFlameGraph trId = do
  div_ [class_ "w-full sticky top-0 border-b border-b-strokeWeak h-6 text-xs relative", id_ $ "time-container-" <> trId] pass
  div_ [class_ "w-full overflow-x-hidden min-h-56 h-full relative", id_ $ "a" <> trId] pass
  div_ [class_ "h-full top-0  absolute z-50 hidden", id_ $ "time-bar-indicator-" <> trId] do
    div_ [class_ "relative h-full"] do
      div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ $ "line-time-" <> trId] "2 ms"
      div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-strokeWeak"] pass


renderLatencyBreakdown :: [(Text, Int, Int)] -> Html ()
renderLatencyBreakdown groups = do
  div_ [class_ "flex h-5 overflow-hidden relative bg-fillWeak rounded", style_ "width:150px"] $ do
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
          tooltip = name <> ": " <> show (dur `div` 1000000) <> " ms"
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
    , let values = V.mapMaybe (\row -> row V.!? idx >>= readMaybe . toString) dataRows
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
    let numValue = fromMaybe 0 $ readMaybe (toString value) :: Double
    let percentage = case col.progress of
          Just "value_percent" -> min 100 (max 0 numValue)
          Just "column_percent" -> case M.lookup col.field maxValues of
            Just maxVal | maxVal > 0 -> (numValue / maxVal) * 100
            _ -> 0
          _ -> 0

    -- Render progress bar
    let progressClass = "progress w-12 ml-2 " <> getProgressVariantClass col.progressVariant
    progress_ [class_ progressClass, value_ (show percentage), max_ "100"] ""


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
    case readMaybe (toString value) :: Maybe Double of
      Just n ->
        let formatted =
              if n < 100 && n /= fromIntegral (round n :: Int)
                then toText (printf "%.2g" n :: String) -- Keep significant digits for small numbers
                else prettyPrintCount (round n) -- Use pretty print for larger numbers
         in formatted <> foldMap (" " <>) col.unit
      Nothing -> value <> foldMap (" " <>) col.unit
  Just "duration" ->
    case readMaybe (toString value) :: Maybe Double of
      Just v -> toText $ getDurationNSMS (fromIntegral (round v :: Int))
      Nothing -> value <> foldMap (" " <>) col.unit
  _ -> value <> foldMap (" " <>) col.unit
