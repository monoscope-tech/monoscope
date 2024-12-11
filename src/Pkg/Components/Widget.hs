module Pkg.Components.Widget (Widget (..), WidgetDataset (..), widget_, Layout (..), WidgetType (..)) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Generics.Labels ()
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Fmt qualified as Ft
import Language.Haskell.TH.Syntax qualified as THS
import Lucid
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Relude
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils qualified


data Query = Query
  { query :: Maybe Text
  , sql :: Maybe Text
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Query


data Layout = Layout
  { x :: Maybe Int
  , y :: Maybe Int
  , w :: Maybe Int
  , h :: Maybe Int
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Layout


data WidgetType = WTTimeseries | WTList | WTTopList | WTDistribution | WTGeomap | WTFunnel | WTTreeMap | WTPieChart
  deriving stock (Show, Generic, Enum, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "WT", DAE.CamelToSnake]] WidgetType


-- when processing widgets we'll do them async, so eager queries are loaded upfront
data Widget = Widget
  { wType :: WidgetType -- Widget type: "timeseries", "table", etc.
  , id :: Maybe Text
  , title :: Maybe Text -- Widget title
  , subtitle :: Maybe Text
  , sql :: Maybe Text
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
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData)
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
  }
  deriving stock (Show, Generic, THS.Lift)
  deriving anyclass (NFData)
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
widget_ w =
  div_ ([class_ "grid-stack-item h-full"] <> attrs) $
    div_ [class_ "grid-stack-item-content !overflow-hidden h-full"] $
      renderChart (w & #id .~ (slugify <$> w.title))
  where
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w)), ("h", (.h))]
    attrs = concat [maybe [] (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= layoutField) | (name, layoutField) <- layoutFields]

    renderChart :: Widget -> Html ()
    renderChart widget = do
      let rateM = (widget.dataset >>= (.rowsPerMin)) <&> (\r -> printf "%.2f" r <> " rows/min")
      let chartId = maybeToMonoid widget.id
      let hasValue = isJust $ widget.dataset >>= (.value)
      div_ [class_ "gap-1.5 flex flex-col h-full box-border mb-1"] do
        div_ [class_ "leading-none flex justify-between items-center"] do
          div_ [class_ "inline-flex gap-3 items-center"] do
            span_ [] $ toHtml $ maybeToMonoid widget.title
            span_ [class_ $ "bg-slate-200 px-2 py-1 rounded-3xl " <> if hasValue then "" else "hidden", id_ $ chartId <> "Value"] $
              whenJust (widget.dataset >>= (.value)) (\x -> toHtml @String $ Ft.fmt $ Ft.commaizeF $ round x)
            span_ [class_ "text-slate-400 widget-subtitle text-sm", id_ $ chartId <> "Subtitle"] $ toHtml $ maybeToMonoid rateM
          button_
            [ term "_" $ fromMaybe "" widget.expandBtnFn
            , class_ "rounded-full border border-slate-300 p-2 inline-flex cursor-pointer"
            ]
            do
              Utils.faSprite_ "up-right-and-down-left-from-center" "regular" "w-3 h-3"
        div_ [class_ "flex-1 pb-1"] do
          div_ [class_ "h-full rounded-2xl border border-slate-200 p-3 bg-slate-50", id_ $ maybeToMonoid widget.id] ""
          let theme = maybeToMonoid widget.theme
          let echartOpt = decodeUtf8 $ AE.encode $ widgetToECharts widget
          let yAxisLabel = fromMaybe (maybeToMonoid widget.unit) (widget.yAxis >>= (.label))
          let query = decodeUtf8 $ AE.encode widget.query
          let pid = decodeUtf8 $ AE.encode $ widget._projectId <&> (.toText)
          script_
            [type_ "text/javascript"]
            [text|

            (()=>{
                let intervalId = null;
                const FETCH_INTERVAL = 5000; // 5sec
                const DEFAULT_BACKGROUND_STYLE = { color: 'rgba(240,248,255, 0.4)' };

                const createSeriesConfig = (name, index, yAxisLabel) => ({
                  type: 'bar',
                  name,
                  stack: yAxisLabel!='' ? yAxisLabel : 'units',
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
                const updateChartData = async (chart, opt, query, shouldFetch, widgetData) => {
                  if (!shouldFetch) return;

                  try {
                    const paramM = new URLSearchParams(window.location.search);
                    paramM.set("pid", widgetData.pid)
                    paramM.set("query_raw",widgetData.query);

                    const response = await (await fetch(`/chart_data?${paramM.toString()}`)).json();
                    opt.dataset.source = [response.headers, ...response.dataset];
                    document.getElementById(widgetData.chartId + "Subtitle").innerHTML = `${response.rows_per_min.toFixed(2)} rows/min`
                    document.getElementById(widgetData.chartId + "Value").innerHTML = `${Number(response.rows_count).toLocaleString()}`
                    document.getElementById(widgetData.chartId + "Value").classList.remove("hidden");

                    // How should rowsPerMin and Count be updated? Direct to DOM? Passing the ids as an arg?
                    const finalOpts = updateChartConfiguration(opt, opt.dataset.source, widgetData.yAxisLabel)
                    chart.hideLoading();
                    chart.setOption(finalOpts);
                  } catch (error) {
                    console.error('Failed to fetch new data:', error);
                  }
                };
               const init = (opt, chartId, query, theme, yAxisLabel, pid) => {
                  const chartEl = document.getElementById(chartId);
                  const chart = echarts.init(chartEl, theme);
                  const liveStreamCheckbox = document.getElementById('streamLiveData');
                  chart.setOption(updateChartConfiguration(opt, opt.dataset.source, yAxisLabel));

                  const resizeObserver = new ResizeObserver((_entries) => requestAnimationFrame(() => echarts.getInstanceByDom(chartEl).resize()));
                  resizeObserver.observe(chartEl);
                  const widgetData = {yAxisLabel, pid, query, chartId}

                  if (liveStreamCheckbox) {
                    liveStreamCheckbox.addEventListener('change', () => {
                      if (checkbox.checked) {
                        intervalId = setInterval(() => updateChartData(chart, opt, query, true, widgetData), FETCH_INTERVAL);
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
                           updateChartData(chart, opt,query, true, widgetData)
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

                init(${echartOpt}, "${chartId}", ${query}, "${theme}", "${yAxisLabel}", ${pid});
              })();
          |]


-----------------------------------------------------------------------------
-- Echarts Logic
-----------------------------------------------------------------------------

-- Function to convert Widget to ECharts options
widgetToECharts :: Widget -> AE.Value
widgetToECharts widget =
  AE.object
    [ "tooltip"
        AE..= AE.object
          [ "trigger" AE..= ("axis" :: Text)
          , "axisPointer"
              AE..= AE.object
                ["type" AE..= ("shadow" :: Text)]
                -- , "formatter" AE..= selectFormatter widget.wType
          ]
    , "legend"
        AE..= AE.object
          [ "show" AE..= True
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
          ]
    , "xAxis"
        AE..= AE.object
          [ "type" AE..= ("time" :: Text)
          , "scale" AE..= True
          , "min" AE..= maybe AE.Null (AE.Number . fromIntegral) (widget ^? #dataset . _Just . #from . _Just)
          , "max" AE..= maybe AE.Null (AE.Number . fromIntegral) (widget ^? #dataset . _Just . #to . _Just)
          , "boundaryGap" AE..= ([0, 0.01] :: [Double])
          , "axisLabel"
              AE..= AE.object
                [ "show" AE..= fromMaybe True (widget.xAxis >>= (.showAxisLabel))
                ]
          ]
    , "yAxis"
        AE..= AE.object
          [ "type" AE..= ("value" :: Text)
          , "min" AE..= (0 :: Int)
          , "splitLine" AE..= AE.object ["show" AE..= True]
          , "axisLabel"
              AE..= AE.object
                [ "show" AE..= fromMaybe True (widget.yAxis >>= (.showAxisLabel))
                ]
          ]
    , "dataset"
        AE..= AE.object
          ["source" AE..= fromMaybe AE.Null (widget.dataset <&> (.source))]
    , "series" AE..= map (createSeries widget.wType) (fromMaybe [] widget.queries)
    , "animation" AE..= False
    ]


-- -- Helper: Select tooltip formatter
-- selectFormatter :: WidgetType -> Text
-- selectFormatter WTTimeseries = "{a} <br/>{b}: {c}ms"
-- selectFormatter _ = "{a} <br/>{b}: {c}"

-- Helper: Extract legend data
extractLegend :: Widget -> Maybe [Text]
extractLegend widget = fmap (map (fromMaybe "Unnamed Series" . (.query))) widget.queries


-- Helper: Create series
createSeries :: WidgetType -> Query -> AE.Value
createSeries widgetType query =
  AE.object
    [ "name" AE..= fromMaybe "Unnamed Series" query.query
    , "type" AE..= mapWidgetTypeToChartType widgetType
    , "stack" AE..= ("Endpoints" :: Text)
    , "showBackground" AE..= True
    , "backgroundStyle"
        AE..= AE.object
          ["color" AE..= ("rgba(240,248,255, 0.4)" :: Text)]
    ]


-- Helper: Map widget type to ECharts chart type
mapWidgetTypeToChartType :: WidgetType -> Text
mapWidgetTypeToChartType WTTimeseries = "line"
mapWidgetTypeToChartType WTDistribution = "bar"
mapWidgetTypeToChartType _ = "bar"
