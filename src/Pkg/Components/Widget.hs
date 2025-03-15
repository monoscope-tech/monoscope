module Pkg.Components.Widget (Widget (..), WidgetDataset (..), widget_, Layout (..), WidgetType (..), WidgetAxis (..), SummarizeBy (..), widgetPostH) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Generics.Labels ()
import Data.Scientific (fromFloatDigits)
import Data.Text qualified as T
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Fmt qualified as Ft
import Language.Haskell.TH.Syntax qualified as THS
import Lucid
import Lucid.Htmx (hxExt_, hxPost_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Pages.Charts.Charts qualified as Charts
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils (faSprite_)


data Query = Query
  { query :: Maybe Text
  , sql :: Maybe Text
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Query


data Layout = Layout
  { x :: Maybe Int
  , y :: Maybe Int
  , w :: Maybe Int
  , h :: Maybe Int
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Layout


data WidgetType
  = WTGroup
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
  deriving stock (Show, Eq, Generic, Enum, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "WT", DAE.CamelToSnake]] WidgetType


-- TODO: Delete after upgrading > 9.10
instance Default WidgetType where
  def = WTTimeseries


data SummarizeBy
  = SBSum
  | SBMax
  | SBMin
  | SBCount
  deriving stock (Show, Eq, Generic, Enum, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "SB", DAE.CamelToSnake]] SummarizeBy


-- TODO: Delete after upgrading > 9.10
instance Default SummarizeBy where
  def = SBSum


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
  , theme :: Maybe Text
  , dataset :: Maybe WidgetDataset
  , -- eager
    eager :: Maybe Bool
  , _projectId :: Maybe Projects.ProjectId
  , _dashboardId :: Maybe Text -- Dashboard ID for context
  , expandBtnFn :: Maybe Text
  , children :: Maybe [Widget]
  , html :: Maybe LText
  , standalone :: Maybe Bool -- Not used in a grid stack
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData, Default)
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
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetDataset


data WidgetAxis = WidgetAxis
  { label :: Maybe Text
  , showAxisLabel :: Maybe Bool
  , series :: Maybe [WidgetAxis]
  , showOnlyMaxLabel :: Maybe Bool
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData, Default)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetAxis


widgetPostH :: Widget -> ATAuthCtx (RespHeaders (Html ()))
widgetPostH = addRespHeaders . widget_


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ w = widgetHelper_ False w


widgetHelper_ :: Bool -> Widget -> Html ()
widgetHelper_ isChild w' = case w.wType of
  WTAnomalies -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] do
    renderWidgetHeader w (maybeToMonoid w.id) w.title Nothing Nothing Nothing (Just ("View all", "/p/" <> maybeToMonoid (w._projectId <&> (.toText)) <> "/anomalies")) (w.hideSubtitle == Just True)
    whenJust w.html toHtmlRaw
  WTGroup -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ div_ [class_ "h-full flex flex-col gap-4"] do
    div_ [class_ "leading-none flex justify-between items-center grid-stack-handle"] do
      div_ [class_ "inline-flex gap-3 items-center"] do
        whenJust w.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
        span_ [class_ "text-sm"] $ toHtml $ maybeToMonoid w.title
    div_ [class_ "grid-stack nested-grid  h-full -mx-2"] $ forM_ (fromMaybe [] w.children) (widgetHelper_ True)
  _ -> gridItem_ $ div_ [class_ $ " w-full h-full " <> paddingBtm] $ renderChart w
  where
    w = (w' & #id %~ maybe (slugify <$> w.title) Just)
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w)), ("h", (.h))]
    attrs = concat [maybe [] (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= layoutField) | (name, layoutField) <- layoutFields]
    paddingBtm = if w.standalone == Just True then "" else (bool " pb-8 " " pb-4 " isChild)
    gridItem_ =
      if w.naked == Just True
        then Relude.id
        else (div_ ([class_ "grid-stack-item h-full flex-1 overflow-hidden ", id_ $ maybeToMonoid w.id <> "_widgetEl"] <> attrs) . div_ [class_ "grid-stack-item-content h-full"])


renderWidgetHeader :: Widget -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text) -> Bool -> Html ()
renderWidgetHeader widget wId title valueM subValueM expandBtnFn ctaM hideSub = div_ [class_ "leading-none flex justify-between items-center grid-stack-handle", id_ $ wId <> "_header"] do
  div_ [class_ "inline-flex gap-3 items-center"] do
    span_ [class_ "text-sm"] $ toHtml $ maybeToMonoid title
    span_ [class_ $ "bg-fillWeak border border-strokeWeak text-sm font-semibold px-2 py-1 rounded-3xl " <> if (isJust valueM) then "" else "hidden", id_ $ wId <> "Value"] $
      whenJust valueM toHtml
    span_ [class_ $ "text-textWeak widget-subtitle text-sm " <> bool "" "hidden" hideSub, id_ $ wId <> "Subtitle"] $ toHtml $ maybeToMonoid subValueM
    -- Add hidden loader with specific ID that can be toggled from JS
    span_ [class_ "hidden", id_ $ wId <> "_loader"] $ Utils.faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
  div_ [class_ "text-iconNeutral"] do
    whenJust ctaM \(ctaTitle, uri) -> a_ [class_ "underline underline-offset-2 text-textBrand", href_ uri] $ toHtml ctaTitle
    whenJust expandBtnFn \fn ->
      button_
        [ term "_" $ fn
        , class_ "p-2 cursor-pointer"
        ]
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    details_ [class_ "dropdown dropdown-end"] do
      summary_ [class_ "text-iconNeutral cursor-pointer p-2 hover:bg-fillWeak rounded-lg", data_ "tippy-content" "Widget Menu"] $
        Utils.faSprite_ "ellipsis" "regular" "w-4 h-4"
      ul_ [class_ "text-textStrong menu menu-md dropdown-content bg-base-100 rounded-box p-2 w-52 shadow-sm leading-none z-10"] do
        -- Only show the "Move to dashboard" option if we're in a dashboard context
        when (isJust widget._dashboardId) $
          li_ $
            a_
              [ class_ "p-2 w-full text-left block"
              , data_ "tippy-content" "Move this widget to another dashboard"
              , id_ $ wId <> "_move_link"
              , -- TODO: Spin up a modal dialog to select a dshboard to move chart into.
                -- This logic would exist on the log explorer too, so it should be portable
                -- Instead of moving, should be a clone
                -- , onclick_
                --     [text|
                --     const targetDashboardId = prompt('Enter the target dashboard ID:', '');
                --     if (!targetDashboardId) return false;
                --
                --     if(!confirm('Are you sure you want to move this widget to dashboard ' + targetDashboardId + '?'))
                --       return false;
                --
                --     // Set the hx-vals attribute with the form data
                --     this.setAttribute('hx-vals', JSON.stringify({
                --       widget_id: '${wId}',
                --       source_dashboard_id: '${fromMaybe "" widget._dashboardId}',
                --       target_dashboard_id: targetDashboardId
                --     }));
                --
                --     return true; // Allow the htmx request to proceed
                --   |]
                hxPost_ $ "/p/" <> fromMaybe "" (widget._projectId <&> (.toText)) <> "/dashboards/move_widget"
              , hxSwap_ "none"
              , hxTrigger_ "click"
              , hxExt_ "json-enc"
              -- , [__| on htmx:afterRequest[detail.successful]
              --       set widgetEl to document.getElementById('${wId}_widgetEl')
              --       call gridStackInstance.removeWidget(widgetEl, true)
              --       if document.getElementById('${wId}_widgetEl')
              --         call widgetEl.dispatchEvent(new CustomEvent('widget-remove-requested',
              --                                     {bubbles: true, detail: { widgetId: '${wId}' }}))
              --       end
              --   |]
              ]
              "Move to dashboard"

        -- Only show the "Duplicate widget" option if we're in a dashboard context
        when (isJust widget._dashboardId) $
          li_ $
            a_
              [ class_ "p-2 w-full text-left block"
              , data_ "tippy-content" "Create a copy of this widget"
              , hxPost_ $
                  "/p/"
                    <> fromMaybe "" (widget._projectId <&> (.toText))
                    <> "/dashboards/"
                    <> fromMaybe "" widget._dashboardId
                    <> "/widgets/"
                    <> wId
                    <> "/duplicate"
              , hxSwap_ "none"
              , hxTrigger_ "click"
              , onclick_ "return confirm('Are you sure you want to duplicate this widget?');"
              , [__| on htmx:afterRequest[detail.successful]
                    location.reload()
                |]
              ]
              "Duplicate widget"
        li_ $
          button_
            [ class_ "p-2 w-full text-left text-textError"
            , data_ "tippy-content" "Permanently delete this widget"
            , onclick_
                [text|
                if(confirm('Are you sure you want to delete this widget? This action cannot be undone.')) {
                  const widgetEl = document.getElementById('${wId}_widgetEl');
                  
                  // Try to remove using the main gridStackInstance
                  // The removeWidget method will only remove widgets that belong to this instance,
                  // so it's safe to try even if the widget is in a nested grid
                  gridStackInstance.removeWidget(widgetEl, true);
                  
                  // If the widget is still in the DOM, it might be in a nested grid
                  // We can trigger a custom event that the dashboard page script can listen for
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


renderChart :: Widget -> Html ()
renderChart widget = do
  let rateM = widget.dataset >>= (.rowsPerMin) >>= \r -> Just $ toText $ printf "%.2f" r <> " rows/min"
  let chartId = maybeToMonoid widget.id
  let valueM = widget.dataset >>= (.value) >>= \x -> Just $ Ft.fmt $ Ft.commaizeF $ round x
  let isStat = widget.wType `elem` [WTTimeseriesStat, WTStat]
  div_ [class_ "gap-0.5 flex flex-col h-full justify-end"] do
    unless (widget.naked == Just True || widget.wType `elem` [WTTimeseriesStat, WTStat]) $
      renderWidgetHeader widget chartId widget.title valueM rateM widget.expandBtnFn Nothing (widget.hideSubtitle == Just True)
    div_ [class_ $ "flex-1 flex " <> bool "" "grid-stack-handle" isStat] do
      div_
        [ class_ $
            "h-full w-full flex flex-col justify-end "
              <> if widget.naked == Just True then "" else " rounded-2xl border border-strokeWeak bg-fillWeaker "
        ]
        do
          when (isStat) $ div_ [class_ "px-3 py-3 flex-1 flex flex-col justify-end "] do
            div_ [class_ "flex flex-col gap-1"] do
              strong_ [class_ "text-textSuccess-strong text-4xl font-normal", id_ $ chartId <> "Value"] $
                whenJust valueM toHtml
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
            script_
              [type_ "text/javascript"]
              [text|
              (()=>{

                const echartOptTxt = `${echartOpt}`
                const echartOpt = JSON.parse(echartOptTxt, (key, value) => {
                  if (typeof value === 'string' && value.trim().startsWith("function(")) {
                    try {
                      return eval('(' + value + ')');
                    } catch (error) {
                      console.error(`Error evaluating function for key "$${key}":`, error);
                      return value;
                    }
                  }
                  return value;
                })

                bindFunctionsToObjects(echartOpt, echartOpt);
                chartWidget({
                  chartType: '${chartType}',
                  widgetType: ${wType},
                  opt: echartOpt,
                  chartId: "${chartId}",
                  query: ${query},
                  querySQL: `${querySQL}`,
                  theme: "${theme}",
                  yAxisLabel: "${yAxisLabel}",
                  pid: ${pid},
                  summarizeBy: '${summarizeBy}',
                  summarizeByPrefix: '${summarizeByPfx}'
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
                    ["type" AE..= ("shadow-sm" :: Text)]
              ]
        , "legend"
            AE..= AE.object
              [ "show" AE..= legendVisibility
              , "type" AE..= "scroll"
              , "top" AE..= "bottom"
              , "data" AE..= fromMaybe [] (extractLegend widget)
              ]
        , "grid"
            AE..= AE.object
              [ "width" AE..= ("100%" :: Text)
              , "left" AE..= ("0%" :: Text)
              , "top" AE..= if widget.naked == Just True then "10%" else "5%"
              , "bottom" AE..= if (fromMaybe False widget.hideLegend || widget.wType == WTTimeseriesStat) then "1.8%" else "22%"
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
                        AE..= if (fromMaybe False $ widget ^? #yAxis . _Just . #showOnlyMaxLabel . _Just)
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
                        AE..= if (fromMaybe False $ widget ^? #yAxis . _Just . #showOnlyMaxLabel . _Just)
                          then "function(value, index) { return (value === this.yAxis.max || value == 0) ? formatNumber(value) : ''; }"
                          else "function(value, index) { return formatNumber(value); }"
                    ]
              , "show" AE..= axisVisibility
              ]
        , "dataset"
            AE..= AE.object
              ["source" AE..= fromMaybe AE.Null (widget.dataset <&> (.source))]
        , "series" AE..= map (createSeries widget.wType) []
        , "animation" AE..= False
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
        , "showBackground" AE..= not isStat
        , "backgroundStyle"
            AE..= AE.object
              ["color" AE..= ("rgba(240,248,255, 0.4)" :: Text)]
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
