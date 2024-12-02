{-# LANGUAGE NoFieldSelectors #-}

module Pages.Dashboards (dashboardGetH, DashboardId (..), DashboardGet (..)) where

import Control.Lens ((^.))
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as W
import Data.Yaml qualified as Yml
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Effectful.Error.Static (throwError)
import Effectful.Time qualified as Time
import Log qualified
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper
import Pkg.Components qualified as Components
import Pkg.Components.TimePicker qualified as TimePicker
import Relude
import Servant (FromHttpApiData, err401)
import System.FilePath (takeExtension)
import System.Types
import Utils qualified
import Text.Slugify (slugify)
import NeatInterpolation

newtype DashboardId = DashboardId {unDashboardId :: UUID.UUID}
  deriving stock (Generic, Show, Read)
  deriving newtype (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, Default, Hashable, NFData, FromHttpApiData)
  deriving anyclass (FromRow, ToRow)

data Query = Query
  { query :: Maybe Text,
    sql :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Query

data Layout = Layout
  { x :: Maybe Int,
    y :: Maybe Int,
    w :: Maybe Int,
    h :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Layout

data WidgetType = WTTimeseries | WTList | WTTopList | WTDistribution | WTGeomap | WTFunnel | WTTreeMap | WTPieChart
  deriving stock (Show, Generic, Enum)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier '[DAE.StripPrefix "WT", DAE.CamelToSnake]] WidgetType

-- when processing widgets we'll do them async, so eager queries are loaded upfront
data Widget = Widget
  { wType :: WidgetType, -- Widget type: "timeseries", "table", etc.
    id :: Maybe Text,
    title :: Maybe Text, -- Widget title
    subtitle :: Maybe Text,
    sql :: Maybe Text,
    query :: Maybe Text,
    queries :: Maybe [Query], -- Multiple queries for combined visualizations
    layout :: Maybe Layout, -- Layout (x, y, w, h)
    xAxis :: Maybe WidgetAxis,
    yAxis :: Maybe WidgetAxis, -- Optional y-axis label
    unit :: Maybe Text,
    value :: Maybe Int, -- value could represent a number or a count
    wData :: Maybe AE.Value,
    hideLegend :: Maybe Bool,
    showAxes :: Maybe Bool,
    theme :: Maybe Text,
    dataset :: Maybe WidgetDataset,
    -- eager
    eager :: Maybe Bool,
    _projectId :: Maybe Projects.ProjectId
    
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] Widget

instance ToHtml Widget where
  toHtml w = toHtml $ widget_ w
  toHtmlRaw = toHtml

data WidgetDataset = WidgetDataset 
  { source:: AE.Value
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetDataset

data WidgetAxis = WidgetAxis
  { label :: Maybe Text,
    series :: Maybe [WidgetAxis]
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetAxis

data Dashboard = Dashboard
  { title :: Maybe Text, -- Dashboard title
    refreshInterval :: Maybe Text, -- Refresh interval
    timeRange :: Maybe TimePicker.TimePicker,
    widgets :: [Widget] -- List of widgets
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Dashboard

data DashboardGet = DashboardGet Projects.ProjectId Dashboard

instance ToHtml DashboardGet where
  toHtml (DashboardGet pid dash) = toHtml $ dashboardPage_ pid dash
  toHtmlRaw = toHtml

dashboardPage_ :: Projects.ProjectId -> Dashboard -> Html ()
dashboardPage_ pid dash = div_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-hidden pb-12 group/pg", id_ "dashboardPage"] do
  div_ "" -- variables selector area
  div_ [class_ "grid-stack"] $ forM_ dash.widgets (\w -> widget_ (w {_projectId=Just pid}) )
  script_ "GridStack.init()"

-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ w =
  div_ ([class_ "grid-stack-item"] <> attrs)
    $ div_ [class_ "grid-stack-item-content"]
    $ renderChart (w {id = slugify <$> w.title})
  where
    layoutFields = [("x", (.x)), ("y", (.y)), ("w", (.w)), ("h", (.h))]
    attrs = concat [maybe [] (\v -> [term ("gs-" <> name) (show v)]) (w.layout >>= field) | (name, field) <- layoutFields]

    renderChart :: Widget -> Html ()
    renderChart widget = do
      let rateM = Just "20 req/sec"
      div_ [class_ "flex-1 space-y-1.5 overflow-x-hidden flex-grow flex flex-col h-full"] do
        div_ [class_ "leading-none flex justify-between items-center"] do
          div_ [class_ "inline-flex gap-3 items-center"] do
            span_ $ toHtml $ maybeToMonoid widget.title
            whenJust widget.unit $ span_ [class_ "bg-slate-200 px-2 py-1 rounded-3xl"] . toHtml
            whenJust rateM $ span_ [class_ "text-slate-300"] . toHtml
          label_ [class_ "rounded-full border border-slate-300 p-2 inline-flex cursor-pointer"] $ Utils.faSprite_ "up-right-and-down-left-from-center" "regular" "w-3 h-3"
        div_ [class_ "rounded-2xl border border-slate-100 p-3 flex-1 bg-slate-50", id_ $ maybeToMonoid widget.id] ""
        let theme = maybeToMonoid widget.theme
        let chartId = maybeToMonoid widget.id
        let echartOpt = decodeUtf8 $ AE.encode $ widgetToECharts widget
        let echartData = decodeUtf8 $ AE.encode $ fromMaybe AE.Null widget.wData
        script_ [type_ "text/javascript"] [text|
          (()=>{
              const opt = ${echartOpt}
              const data = ${echartData}
              const chartEl = document.getElementById("${chartId}")
              const chart = echarts.init(chartEl, "${theme}");
              if (data){
                const timestamps = data[0]; // Extract timestamps
                const seriesData = data.slice(1); // Extract series values
                opt.series = seriesLabels.slice(1).map((label, index) => ({
                    name: label,
                    type: 'line',
                    data: seriesData[index], // Use series data aligned with the label
                }))
              } else {
                chart.showLoading();
              }
              chart.setOption(opt);
              (new ResizeObserver((_entries) => window.requestAnimationFrame(() => echarts.getInstanceByDom(chartEl).resize()))).observe(chartEl);

            })();
        |]
        -- div_
        --   [ class_ $ "rounded-2xl border border-slate-200 log-chart p-3 flex-1 bg-slate-50",
        --     hxGet_ $ "/charts_html?id=" <> maybeToMonoid widget.id <> "&show_legend=false&pid=" <> maybe "" (.toText) widget._projectId <> "&query_raw=" <> (Utils.escapedQueryPartial $ maybeToMonoid widget.query),
        --     hxTrigger_ "intersect, submit from:#log_explorer_form, add-query from:#filterElement, update-query from:#filterElement",
        --     hxVals_ $ "js:{since: params().since, from: params().from, to:params().to, cols:params().cols, layout:'all', source: params().source}",
        --     hxSwap_ "innerHTML"
        --   ]
        -- ""

decodeDashboard :: Text -> ByteString -> Either Text Dashboard
decodeDashboard url content =
  case takeExtension $ toString url of
    ".json" -> bimap fromString Relude.id $ AE.eitherDecodeStrict content
    ".yaml" -> bimap show Relude.id $ Yml.decodeEither content
    ".yml" -> bimap show Relude.id $ Yml.decodeEither content
    _ -> Left "Unsupported file extension. Use .json or .yaml/.yml."

dashboardGetH :: Projects.ProjectId -> DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId file fromDStr toDStr sinceStr = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)
  fileResp <- W.get $ toString $ maybeToMonoid file
  dash <- case decodeDashboard (maybeToMonoid file) (toStrict $ fileResp ^. W.responseBody) of
    Left err -> Log.logAttention "Auth Error in clientMetadata" (toString err) >> throwError err401
    Right d -> pure $ d

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession,
            currProject = Just project,
            pageTitle = "Dashboards > " <> maybeToMonoid dash.title,
            pageActions = Just $ Components.timepicker_ Nothing currentRange
          }
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dash

-----------------------------------------------------------------------------
-- Echarts Logic
-----------------------------------------------------------------------------

-- Function to convert Widget to ECharts options
widgetToECharts :: Widget -> AE.Value
widgetToECharts widget = AE.object
  [ "tooltip" AE..= AE.object
      [ "trigger" AE..= ("axis" :: Text)
      , "axisPointer" AE..= AE.object
          [ "type" AE..= ("shadow" :: Text) ]
      , "formatter" AE..= selectFormatter widget.wType
      ]
  , "legend" AE..= AE.object
      [ "show" AE..= True
      , "type" AE..= ("scroll" :: Text)
      , "top" AE..= ("bottom" :: Text)
      , "data" AE..= fromMaybe [] (extractLegend widget)
      ]
  , "grid" AE..= AE.object
      [ "width" AE..= ("100%" :: Text)
      , "left" AE..= ("0%" :: Text)
      , "top" AE..= ("2%" :: Text)
      , "bottom" AE..= if fromMaybe False widget.hideLegend then "1.8%" else "22%"
      , "containLabel" AE..= True
      ]
  , "xAxis" AE..= AE.object
      [ "type" AE..= ("time" :: Text)
      , "scale" AE..= True
      , "min" AE..= fromMaybe AE.Null (extractTimeBoundary widget "from")
      , "max" AE..= fromMaybe AE.Null (extractTimeBoundary widget "to")
      , "boundaryGap" AE..= ([0, 0.01]::[Double])
      , "axisLabel" AE..= AE.object
          [ "show" AE..= fromMaybe False widget.showAxes ]
      , "data" AE..= fromMaybe AE.Null widget.wData
      ]
  , "yAxis" AE..= AE.object
      [ "type" AE..= ("value" :: Text)
      , "min" AE..= (0 :: Int)
      , "axisLabel" AE..= AE.object
          [ "show" AE..= widget.showAxes
          , "formatter" AE..= selectFormatter widget.wType
          ]
  , "dataset" AE..= AE.object 
      [ "source" AE..= fromMaybe AE.Null (widget.dataset <&> (.source)) ]
      ]
  , "series" AE..= map (createSeries widget.wType) (fromMaybe [] widget.queries)
  , "animation" AE..= False
  ]

-- Helper: Select tooltip formatter
selectFormatter :: WidgetType -> Text
selectFormatter WTTimeseries = "{a} <br/>{b}: {c}ms"
selectFormatter _ = "{a} <br/>{b}: {c}"

-- Helper: Extract legend data
extractLegend :: Widget -> Maybe [Text]
extractLegend widget = fmap (map (fromMaybe "Unnamed Series" . (.query))) widget.queries

-- Helper: Extract time boundary
extractTimeBoundary :: Widget -> Text -> Maybe AE.Value
extractTimeBoundary widget key =
  if key == "from" then Just (AE.Number 1630000000) else Just (AE.Number 1730000000) -- Example; replace with actual logic.

-- Helper: Create series
createSeries :: WidgetType -> Query -> AE.Value
createSeries widgetType query = AE.object
  [ "name" AE..= fromMaybe "Unnamed Series" query.query
  , "type" AE..= mapWidgetTypeToChartType widgetType
  , "stack" AE..= ("Endpoints" :: Text)
  , "showBackground" AE..= True
  , "backgroundStyle" AE..= AE.object
      [ "color" AE..= ("rgba(240,248,255, 0.4)" :: Text) ]
  ]

-- Helper: Map widget type to ECharts chart type
mapWidgetTypeToChartType :: WidgetType -> Text
mapWidgetTypeToChartType WTTimeseries = "line"
mapWidgetTypeToChartType WTDistribution = "bar"
mapWidgetTypeToChartType _ = "bar"
