module Pages.Dashboards (dashboardGetH, DashboardId (..), DashboardGet (..), dashboardsGetH, DashboardsGet (..), dashboardsPostH) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W
import Data.Generics.Labels ()
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Yml
import Database.PostgreSQL.Entity qualified as DBT
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt qualified as Ft
import GHC.Records (HasField (getField))
import Lucid
import Lucid.Htmx (hxPost_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Pkg.Components qualified as Components
import Pkg.Components.TimePicker qualified as TimePicker
import Relude
import Servant (FromHttpApiData, NoContent (NoContent), ServerError, err401, err404, errBody)
import System.FilePath (takeExtension)
import System.Types
import Text.Printf (printf)
import Text.Slugify (slugify)
import Utils (faIcon_, faSprite_)
import Utils qualified


-- For OverloadedLabels

newtype DashboardId = DashboardId {unDashboardId :: UUID.UUID}
  deriving stock (Generic, Show, Read)
  deriving newtype (Eq, Ord, AE.ToJSON, AE.FromJSON, FromField, ToField, Default, Hashable, NFData, FromHttpApiData)
  deriving anyclass (FromRow, ToRow)


instance HasField "unwrap" DashboardId UUID.UUID where
  getField = coerce


instance HasField "toText" DashboardId Text where
  getField = UUID.toText . unDashboardId


data Query = Query
  { query :: Maybe Text
  , sql :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Query


data Layout = Layout
  { x :: Maybe Int
  , y :: Maybe Int
  , w :: Maybe Int
  , h :: Maybe Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Layout


data WidgetType = WTTimeseries | WTList | WTTopList | WTDistribution | WTGeomap | WTFunnel | WTTreeMap | WTPieChart
  deriving stock (Show, Generic, Enum)
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
  }
  deriving stock (Show, Generic)
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
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetDataset


data WidgetAxis = WidgetAxis
  { label :: Maybe Text
  , showAxisLabel :: Maybe Bool
  , series :: Maybe [WidgetAxis]
  }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "w", DAE.CamelToSnake]] WidgetAxis


data Dashboard = Dashboard
  { title :: Maybe Text -- Dashboard title
  , refreshInterval :: Maybe Text -- Refresh interval
  , timeRange :: Maybe TimePicker.TimePicker
  , widgets :: [Widget] -- List of widgets
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake Dashboard
  deriving anyclass (NFData)
  deriving (FromField, ToField) via Aeson Dashboard


data DashboardGet = DashboardGet Projects.ProjectId Dashboard


instance ToHtml DashboardGet where
  toHtml (DashboardGet pid dash) = toHtml $ dashboardPage_ pid dash
  toHtmlRaw = toHtml


dashboardPage_ :: Projects.ProjectId -> Dashboard -> Html ()
dashboardPage_ pid dash = div_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-hidden pb-12 group/pg", id_ "dashboardPage"] do
  div_ "" -- variables selector area
  div_ [class_ "grid-stack"] $ forM_ dash.widgets (\w -> widget_ (w{_projectId = Just pid}))
  script_ "GridStack.init()"


-- use either index or the xxhash as id
widget_ :: Widget -> Html ()
widget_ w =
  div_ ([class_ "grid-stack-item"] <> attrs)
    $ div_ [class_ "grid-stack-item-content !overflow-hidden"]
    $ renderChart (w & #id .~ (slugify <$> w.title))
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
            span_ [class_ $ "bg-slate-200 px-2 py-1 rounded-3xl " <> if hasValue then "" else "hidden", id_ $ chartId <> "Value"]
              $ whenJust (widget.dataset >>= (.value)) (\x -> toHtml @String $ Ft.fmt $ Ft.commaizeF $ round x)
            span_ [class_ "text-slate-400 widget-subtitle text-sm", id_ $ chartId <> "Subtitle"] $ toHtml $ maybeToMonoid rateM
          label_ [class_ "rounded-full border border-slate-300 p-2 inline-flex cursor-pointer"] $ Utils.faSprite_ "up-right-and-down-left-from-center" "regular" "w-3 h-3"
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
                  console.log("updateChartConfiguration")
                  if (!data) return opt;
                  const columnNames = data[0]?.slice(1);
                  console.log(columnNames, "columnNames")
                  opt.series = columnNames?.map((name, index) => createSeriesConfig(name, index, yAxisLabel));
                  console.log("series", opt.series)
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


decodeDashboard :: Text -> ByteString -> Either Text Dashboard
decodeDashboard url content =
  case takeExtension $ toString url of
    ".json" -> bimap fromString Relude.id $ AE.eitherDecodeStrict content
    ".yaml" -> bimap show Relude.id $ Yml.decodeEither' content
    ".yml" -> bimap show Relude.id $ Yml.decodeEither' content
    _ -> Left "Unsupported file extension. Use .json or .yaml/.yml."


-- Utility to load dashboard either from file or fallback
loadDashboardURI :: (HTTP :> es, Error ServerError :> es) => Text -> Eff es Dashboard
loadDashboardURI file = do
  fileResp <- W.get (toString file)
  decodeDashboard file (toStrict $ fileResp ^. W.responseBody)
    & either
      (\e -> throwError $ err401{errBody = ("Error decoding dashboard: " <> encodeUtf8 e)})
      pure


loadDashboardFromVM :: (HTTP :> es, Error ServerError :> es) => DashboardVM -> Eff es Dashboard
loadDashboardFromVM dashVM =
  case dashVM.schema of
    Just schema_ -> pure schema_
    Nothing ->
      case dashVM.baseTemplate of
        Just uri -> loadDashboardURI uri
        Nothing -> pure defaultDashboard
  where
    defaultDashboard = Dashboard Nothing Nothing Nothing []


dashboardGetH :: Projects.ProjectId -> DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId fileM fromDStr toDStr sinceStr = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)

  dashVM <-
    (dbtToEff $ DBT.selectById @DashboardVM (Only dashId)) >>= \case
      Just v -> pure v
      Nothing -> throwError $ err404{errBody = ("Dashboard with ID not found. ID:" <> encodeUtf8 dashId.toText)}

  dash <- case fileM of
    Just file -> loadDashboardURI file
    Nothing -> loadDashboardFromVM dashVM

  dash' <- forOf (#widgets . traverse) dash \widget ->
    if (widget.eager == Just True)
      then do
        metricsD <- Charts.queryMetrics (Just pid) widget.query Nothing sinceStr fromDStr toDStr Nothing
        pure
          $ widget
          & #dataset
            ?~ WidgetDataset
              { source = AE.toJSON $ V.cons (AE.toJSON <$> metricsD.headers) (AE.toJSON <<$>> metricsD.dataset)
              , rowsPerMin = Just metricsD.rowsPerMin
              , value = Just $ fromIntegral metricsD.rowsCount
              , from = metricsD.from
              , to = metricsD.to
              }
      else pure widget

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Dashboards > " <> maybeToMonoid dash'.title
          , pageActions = Just $ Components.timepicker_ Nothing currentRange
          }
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dash'


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


--------------------------------------------------------------------
-- Dashboard List
--

data DashboardItem = DashboardItem
  { createdAt :: UTCTime
  , title :: Text
  , isHome :: Bool
  , isStared :: Bool
  , tags :: V.Vector Text
  }
  deriving (Show, Generic)


data DashboardsGet = DashboardsGet
  { dashboards :: V.Vector DashboardItem
  }
  deriving (Show, Generic)


instance ToHtml DashboardsGet where
  toHtml dash = toHtml $ dashboardsGet_ dash
  toHtmlRaw = toHtml


dashboardsGet_ :: DashboardsGet -> Html ()
dashboardsGet_ dg = do
  Components.modal_ "newDashboardMdl" "" $ form_
    [ class_ "grid grid-cols-4 bg-slate-25 overflow-hidden h-full gap-4"
    , hxPost_ ""
    ]
    do
      div_ [class_ "col-span-1 space-y-4"] do
        strong_ "Create dashboard"
        label_ [class_ "input input-sm input-bordered flex items-center "] do
          faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
          input_ [type_ "text", class_ "grow pl-2", placeholder_ "Search"]
          kbd_ [class_ "kbd kbd-sm"] "/"
        div_ [class_ "space-y-1"] do
          div_ [class_ "group bg-slate-100 hover:bg-slate-100 border rounded-lg flex p-1.5 gap-2 items-center"] do
            input_ [class_ "hidden", type_ "radio"]
            span_ [class_ "p-1 px-2 bg-slate-200 rounded-md"] $ faIcon_ "cards-blank" "fa-cards-blank fa-regular" "icon w-4 h-4"
            span_ [class_ "grow"] "Blank dashboard"
            span_ [class_ "px-2 p-1 group-hover:block"] $ faSprite_ "chevron-right" "regular" "w-4 h-4"
          div_ [class_ "cursor-pointer group hover:bg-slate-100 hover:border rounded-lg flex p-1.5 gap-2 items-center"] do
            input_ [class_ "hidden", type_ "radio", name_ "file", value_ "docker"]
            span_ [class_ "p-1 px-2 bg-slate-200 rounded-md"] $ faIcon_ "docker" "fa-docker fa-brands" "icon w-4 h-4"
            span_ [class_ "grow"] "Docker"
            span_ [class_ "px-2 p-1 hidden group-hover:block"] $ faSprite_ "chevron-right" "regular" "w-4 h-4"

      div_ [class_ "col-span-3 px-3 py-5 divide-y h-full overflow-y-scroll "] do
        div_ [class_ "flex gap-3 pb-5"] do
          div_ [class_ "p-2 bg-slate-100 rounded-lg"] $ faIcon_ "cards-blank" "regular" "w-8 h-8"
          div_ [class_ "flex-1"] do
            strong_ [class_ "text-xl"] "Custom Dashboard"
            p_ [class_ "text-sm"] "Get started from a blank slate"
          div_ [class_ "flex items-center justify-center shrink"] $ button_ [class_ "leading-none rounded-xl p-3 cursor-pointer bg-gradient-to-b from-[#067cff] to-[#0850c5] text-white"] "Select dashboard"
        div_ [class_ "pt-5"] do
          div_ [class_ "bg-[#1e9cff] px-5 py-8 rounded-xl"] $ img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full"]

  div_ [id_ "itemsListPage", class_ "mx-auto px-6 pt-4 gap-8 w-full flex flex-col h-full overflow-hidden pb-12  group/pg"] do
    div_ [class_ "flex"] do
      label_ [class_ "input input-md input-bordered flex-1 flex bg-slate-100 border-slate-200 shadow-none overflow-hidden items-center gap-2"] do
        faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
        input_ [type_ "text", class_ "grow", placeholder_ "Search", [__|on input show .itemsListItem in #itemsListPage when its textContent.toLowerCase() contains my value.toLowerCase()|]]
    -- button_
    div_ [class_ "grid grid-cols-2 gap-5"] do
      forM_ dg.dashboards \dash -> div_ [class_ "rounded-xl border border-slate-200 gap-3.5 p-4 bg-slate-100 flex"] do
        div_ [class_ "flex-1 space-y-2"] do
          div_ [class_ "flex items-center gap-2"] do
            strong_ [class_ "font-medium"] (toHtml dash.title)
            a_ [class_ "leading-none", term "data-tippy-content" "This dashboard is currently your homepage."] $ when dash.isHome $ (faSprite_ "house" "regular" "w-4 h-4")
          div_ [class_ "gap-2 flex items-center"] do
            time_ [class_ "mr-2 text-slate-400", datetime_ $ Utils.formatUTC dash.createdAt] $ toHtml $ formatTime defaultTimeLocale "%eth %b %Y" dash.createdAt
            forM_ dash.tags (a_ [class_ "cbadge-sm badge-neutral cbadge bg-slate-200"] . toHtml @Text)
        div_ [class_ "flex items-center justify-center gap-3"] do
          a_ [class_ "rounded-full border border-slate-300 p-2 leading-none text-gray-700"]
            $ if dash.isStared
              then (faSprite_ "star" "solid" "w-5 h-5")
              else (faSprite_ "star" "regular" "w-5 h-5")
          div_ [class_ "space-x-2"] $ faSprite_ "chart-area" "regular" "w-5 h-5" >> (span_ "4 charts")


dashboardsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx DashboardsGet))
dashboardsGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Dashboards"
          , pageActions = Just $ (label_ [Lucid.for_ "newDashboardMdl", class_ "leading-none rounded-xl p-3 cursor-pointer bg-gradient-to-b from-[#067cff] to-[#0850c5] text-white"] "New Dashboard")
          }
  addRespHeaders
    $ PageCtx bwconf
    $ DashboardsGet
      { dashboards =
          [ DashboardItem
              { title = "OpenTelemetry Traces (otel-demo-traces)"
              , isHome = True
              , isStared = True
              , tags = V.fromList ["Tag 1", "Tag 2"]
              , createdAt = now
              }
          , DashboardItem
              { title = "Title"
              , isHome = False
              , isStared = True
              , tags = V.singleton "Tag 1"
              , createdAt = now
              }
          , DashboardItem
              { title = "Github - Charm"
              , isHome = False
              , isStared = False
              , tags = V.singleton "Tag 1"
              , createdAt = now
              }
          ]
      }


data DashboardVM = DashboardVM
  { id :: DashboardId
  , projectId :: Projects.ProjectId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , createdBy :: Users.UserId
  , baseTemplate :: Maybe Text
  , schema :: Maybe Dashboard
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (DBT.Entity)
    via (GenericEntity '[Schema "projects", TableName "dashboards", PrimaryKey "id", FieldModifiers '[CamelToSnake]] DashboardVM)


dashboardsPostH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders NoContent)
dashboardsPostH pid templateFileM = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  did <- DashboardId <$> UUID.genUUID
  let redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> (did.toText)
  dbtToEff
    $ DBT.insert @DashboardVM
    $ DashboardVM
      { id = did
      , projectId = pid
      , createdAt = now
      , updatedAt = now
      , createdBy = sess.user.id
      , baseTemplate = templateFileM
      , schema = Nothing
      }
  redirectCS redirectURI
  addRespHeaders NoContent
