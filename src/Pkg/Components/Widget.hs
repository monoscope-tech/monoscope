module Pkg.Components.Widget (Widget (..), WidgetDataset (..), widget_, Layout (..), WidgetType (..), SummarizeBy (..)) where

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
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Pages.Charts.Charts qualified as Charts
import Relude
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
  , expandBtnFn :: Maybe Text
  , children :: Maybe [Widget]
  , html :: Maybe LText
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
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetAxis


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ w = widgetHelper_ False w


widgetHelper_ :: Bool -> Widget -> Html ()
widgetHelper_ isChild w = case w.wType of
  WTAnomalies -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] do
    renderWidgetHeader (maybeToMonoid w.id) w.title Nothing Nothing Nothing (Just ("View all", "/p/" <> maybeToMonoid (w._projectId <&> (.toText)) <> "/anomalies")) (w.hideSubtitle == Just True)
    whenJust w.html toHtmlRaw
  WTGroup -> gridItem_ $ div_ [class_ $ "h-full" <> paddingBtm] $ div_ [class_ "flex flex-col gap-4"] do
    div_ [class_ "leading-none flex justify-between items-center"] do
      div_ [class_ "inline-flex gap-3 items-center"] do
        whenJust w.icon \icon -> span_ [] $ Utils.faSprite_ icon "regular" "w-4 h-4"
        span_ [] $ toHtml $ maybeToMonoid w.title
    div_ [class_ "grid-stack nested-grid  h-full -m-2"] $ forM_ (fromMaybe [] w.children) (widgetHelper_ True)
  _ -> gridItem_ $ div_ [class_ $ "h-full " <> paddingBtm] $ renderChart (w & #id .~ (slugify <$> w.title))
  where
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w)), ("h", (.h))]
    attrs = concat [maybe [] (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= layoutField) | (name, layoutField) <- layoutFields]
    paddingBtm = bool "pb-8" "pb-4" isChild
    gridItem_ = div_ ([class_ "grid-stack-item h-full"] <> attrs) . div_ [class_ "grid-stack-item-content h-full"]


renderWidgetHeader :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Text, Text) -> Bool -> Html ()
renderWidgetHeader wId title valueM subValueM expandBtnFn ctaM hideSub = div_ [class_ "leading-none flex justify-between items-center"] do
  div_ [class_ "inline-flex gap-3 items-center"] do
    span_ [] $ toHtml $ maybeToMonoid title
    span_ [class_ $ "bg-fillWeak border border-strokeWeak text-sm font-semibold px-2 py-1 rounded-3xl " <> if (isJust valueM) then "" else "hidden", id_ $ wId <> "Value"]
      $ whenJust valueM toHtml
    span_ [class_ $ "text-textWeak widget-subtitle text-sm " <> bool "" "hidden" hideSub, id_ $ wId <> "Subtitle"] $ toHtml $ maybeToMonoid subValueM
  div_ [class_ "text-iconNeutral"] do
    whenJust ctaM \(ctaTitle, uri) -> a_ [class_ "underline underline-offset-2 text-textBrand", href_ uri] $ toHtml ctaTitle
    whenJust expandBtnFn \fn ->
      button_
        [ term "_" $ fn
        , class_ "p-2 cursor-pointer"
        ]
        $ Utils.faSprite_ "expand-icon" "regular" "w-3 h-3"
    button_
      [ class_ "p-2 cursor-pointer"
      ]
      $ Utils.faSprite_ "ellipsis" "regular" "w-4 h-4"


renderChart :: Widget -> Html ()
renderChart widget = do
  let rateM = widget.dataset >>= (.rowsPerMin) >>= (\r -> Just $ toText $ printf "%.2f" r <> " rows/min")
  let chartId = maybeToMonoid widget.id
  let valueM = (widget.dataset >>= (.value) >>= (\x -> Just $ Ft.fmt $ Ft.commaizeF $ round x))
  div_ [class_ "gap-1.5 flex flex-col h-full"] do
    unless (widget.wType `elem` [WTTimeseriesStat, WTStat])
      $ renderWidgetHeader chartId widget.title valueM rateM widget.expandBtnFn Nothing (widget.hideSubtitle == Just True)
    div_ [class_ "flex-1 flex"] do
      div_ [class_ "h-full w-full rounded-2xl border border-strokeWeak p-3 bg-fillWeaker flex "] do
        when (widget.wType `elem` [WTTimeseriesStat, WTStat]) $ div_ [class_ "flex flex-col justify-between"] do
          div_ $ whenJust widget.icon \icon -> span_ [class_ "p-3 bg-fillWeak rounded-lg leading-[0] inline-block text-strokeSelected"] $ Utils.faSprite_ icon "regular" "w-5 h-5"
          div_ [class_ "flex flex-col"] do
            strong_ [class_ "text-textSuccess-strong text-xl"]
              $ whenJust valueM toHtml
            div_ [class_ "inline-flex gap-2 items-center justify-center"] do
              span_ [] $ toHtml $ maybeToMonoid widget.title
              span_ [class_ "inline-flex items-center"] $ Utils.faSprite_ "circle-info" "regular" "w-5 h-5"
        unless (widget.wType == WTStat) $ div_ [class_ "h-full w-full flex-1"] do
          div_ [class_ "h-full w-full", id_ $ maybeToMonoid widget.id] ""
          let theme = maybeToMonoid widget.theme
          let echartOpt = decodeUtf8 $ AE.encode $ widgetToECharts widget
          let yAxisLabel = fromMaybe (maybeToMonoid widget.unit) (widget.yAxis >>= (.label))
          let query = decodeUtf8 $ AE.encode widget.query
          let pid = decodeUtf8 $ AE.encode $ widget._projectId <&> (.toText)
          let querySQL = maybeToMonoid widget.sql
          let chartType = mapWidgetTypeToChartType widget.wType
          let summarizeBy = T.toLower $ T.drop 2 $ show $ fromMaybe SBSum widget.summarizeBy
          let summarizeByPfx = summarizeByPrefix $ fromMaybe SBSum widget.summarizeBy
          -- let widgetJSON = decodeUtf8 $ AE.encode $ widget
          -- let seriesDefault = decodeUtf8 $ AE.encode $ createSeries widget.wType Nothing
          script_
            [type_ "text/javascript"]
            [text|

              (()=>{
                  let intervalId = null;
                  const FETCH_INTERVAL = 5000; // 5sec
                  const DEFAULT_BACKGROUND_STYLE = { color: 'rgba(240,248,255, 0.4)' };
                  const CHART_TYPE = '${chartType}'
                  const SUMMARIZE_BY = '${summarizeBy}'
                  const SUMMARIZE_BY_PREFIX = '${summarizeByPfx}'

                  const createSeriesConfig = (name, index, yAxisLabel) => ({
                    type: CHART_TYPE,
                    name,
                    stack: CHART_TYPE == 'line'? undefined : yAxisLabel!='' ? yAxisLabel : 'units',
                    showSymbol: false,
                    showBackground: true,
                    backgroundStyle: DEFAULT_BACKGROUND_STYLE,
                    barMaxWidth: '10',
                    barMinHeight: '1',
                    encode: { x: 0, y: index + 1 }
                  });
                  const updateChartConfiguration = (opt, data, yAxisLabel) => {
                    if (!data) return opt;
                    const columnNames = data[0]?.slice(1);
                    opt.series = columnNames?.map((name, index) => createSeriesConfig(name, index, yAxisLabel));
                    opt.legend.data = columnNames;
                    return opt;
                  };
                  const updateChartData = async (chart, opt, shouldFetch, widgetData) => {
                    if (!shouldFetch) return;

                    try {
                      const paramM = new URLSearchParams(window.location.search);
                      paramM.set("pid", widgetData.pid)
                      paramM.set("query_raw",widgetData.query);
                      if (widgetData.querySQL!="") paramM.set("query_sql",widgetData.querySQL);

                      const response = await (await fetch(`/chart_data?${paramM.toString()}`)).json();
                      opt.xAxis = opt.xAxis || {};
                      opt.xAxis.min = response.from * 1000;
                      opt.xAxis.max = response.to * 1000;
                      // opt.yAxis.max = response.stats.max;

                      // opt.dataset.source = [response.headers, ...response.dataset];
                      opt.dataset.source = [response.headers, ...response.dataset.map(row => [row[0] * 1000, ...row.slice(1)])];
                      document.getElementById(widgetData.chartId + "Subtitle").innerHTML = `${response.rows_per_min.toFixed(2)} rows/min`
                      document.getElementById(widgetData.chartId + "Value").innerHTML = `$${SUMMARIZE_BY_PREFIX} ${Number(response.stats[SUMMARIZE_BY]).toLocaleString()}`
                      document.getElementById(widgetData.chartId + "Value").classList.remove("hidden");

                      // How should rowsPerMin and Count be updated? Direct to DOM? Passing the ids as an arg?
                      const finalOpts = updateChartConfiguration(opt, opt.dataset.source, widgetData.yAxisLabel)
                      chart.hideLoading();
                      chart.setOption(finalOpts);
                    } catch (error) {
                      console.error('Failed to fetch new data:', error);
                    }
                  };
                 const init = (opt, chartId, query, querySQL, theme, yAxisLabel, pid) => {
                    const chartEl = document.getElementById(chartId);
                    const chart = echarts.init(chartEl, theme);
                    chart.group = 'default';
                    const liveStreamCheckbox = document.getElementById('streamLiveData');

                    // multiply by 1000 to convert unix timestamp in seconds to milliseconds needed by echarts. Useful for eager charts
                    opt.dataset.source = opt.dataset?.source?.map(row => [row[0] * 1000, ...row.slice(1)]) ?? null;

                    chart.setOption(updateChartConfiguration(opt, opt.dataset.source, yAxisLabel));

                    const resizeObserver = new ResizeObserver((_entries) => requestAnimationFrame(() => echarts.getInstanceByDom(chartEl).resize()));
                    resizeObserver.observe(chartEl);
                    const widgetData = {yAxisLabel, pid, query,querySQL, chartId}

                    if (liveStreamCheckbox) {
                      liveStreamCheckbox.addEventListener('change', () => {
                        if (checkbox.checked) {
                          intervalId = setInterval(() => updateChartData(chart, opt, true, widgetData), FETCH_INTERVAL);
                        } else {
                          clearInterval(intervalId);
                          intervalId = null;
                        }
                      });
                    }

                    if (!opt.dataset.source) {
                      chart.showLoading();
                      const observer = new IntersectionObserver(entries => {
                          if (entries[0]?.isIntersecting) {
                             updateChartData(chart, opt,true, widgetData)
                             observer.disconnect();
                          }
                      });
                      observer.observe(document.getElementById(chartId));
                    }

                    ['submit', 'add-query', 'update-query'].forEach(event =>
                      document.querySelector(event === 'submit' ? '#log_explorer_form' : '#filterElement')?.addEventListener(event, () => updateChartData(chart, opt, query, true, widgetData))
                    );

                    window.addEventListener('unload', () => {
                      clearInterval(intervalId);
                      resizeObserver.disconnect();
                    });
                  };

                  init(${echartOpt}, "${chartId}", ${query},
                  `${querySQL}`,
                  "${theme}", "${yAxisLabel}", ${pid});
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
      legendVisibility = not isStat
   in AE.object
        [ "tooltip"
            AE..= AE.object
              [ "trigger" AE..= ("axis" :: Text)
              , "axisPointer"
                  AE..= AE.object
                    ["type" AE..= ("shadow" :: Text)]
              ]
        , "legend"
            AE..= AE.object
              [ "show" AE..= legendVisibility
              , "type" AE..= ("scroll" :: Text)
              , "top" AE..= ("bottom" :: Text)
              , "data" AE..= fromMaybe [] (extractLegend widget)
              ]
        , "grid"
            AE..= AE.object
              [ "width" AE..= ("100%" :: Text)
              , "left" AE..= ("0%" :: Text)
              , "top" AE..= ("2%" :: Text)
              , "bottom" AE..= if fromMaybe False widget.hideLegend then "1.8%" else "22%"
              , "containLabel" AE..= True
              , "show" AE..= gridLinesVisibility
              ]
        , "xAxis"
            AE..= AE.object
              [ "type" AE..= ("time" :: Text)
              , "scale" AE..= True
              , "min" AE..= maybe AE.Null (AE.Number . fromIntegral . (* 1000)) (widget ^? #dataset . _Just . #from . _Just)
              , "max" AE..= maybe AE.Null (AE.Number . fromIntegral . (* 1000)) (widget ^? #dataset . _Just . #to . _Just)
              , "boundaryGap" AE..= ([0, 0.01] :: [Double])
              , "axisLabel" AE..= AE.object ["show" AE..= axisVisibility]
              , "show" AE..= axisVisibility
              ]
        , "yAxis"
            AE..= AE.object
              [ "type" AE..= ("value" :: Text)
              , "min" AE..= (0 :: Int)
              , "max" AE..= maybe AE.Null (AE.Number . fromFloatDigits) (widget ^? #dataset . _Just . #stats . _Just . #max)
              , "splitLine" AE..= AE.object ["show" AE..= gridLinesVisibility]
              , "axisLabel" AE..= AE.object ["show" AE..= axisVisibility]
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
