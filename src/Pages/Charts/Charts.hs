module Pages.Charts.Charts (anomalyThroughput, chartInit, throughputEndpoint, throughputEndpointHTML) where

import Config (DashboardM, pool)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Relude
import Witch (from)

throughputEndpoint :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM ByteString
throughputEndpoint _ pid endpointHash shapeHash formatHash = do
  pool <- asks pool
  chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid endpointHash shapeHash formatHash
  pure $ from @Text chartData

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
throughputEndpointHTML _ pid endpointHash shapeHash formatHash = do
  pool <- asks pool
  chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid endpointHash shapeHash formatHash
  let entityId = fromMaybe "" $ endpointHash <|> shapeHash <|> formatHash
  pure $ do
    div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
    script_ [text| throughputEChart("id-$entityId", $chartData, []) |]

anomalyTypeToQueryKey :: Anomalies.AnomalyTypes -> Text
anomalyTypeToQueryKey Anomalies.ATEndpoint = "endpoint_hash"
anomalyTypeToQueryKey Anomalies.ATShape = "shape_hash"
anomalyTypeToQueryKey Anomalies.ATFormat = "format_hash"
anomalyTypeToQueryKey _ = error "anomalyTypeToQueryKey should be unreachable"

-- This endpoint will return a throughput chart partial
anomalyThroughput :: Projects.ProjectId -> Anomalies.AnomalyTypes -> Text -> Html ()
anomalyThroughput pid anType queryValue = do
  let pidT = pid.toText
  let queryKey = anomalyTypeToQueryKey anType
  div_
    [ id_ $ "id-" <> queryValue,
      -- style_ "height:250px",
      class_ "w-full h-full",
      hxGet_ [text| /p/$pidT/charts_html/throughput?$queryKey=$queryValue |],
      hxTrigger_ "intersect",
      hxSwap_ "outerHTML"
    ]
    ""

-- chartInit is registered once in the BodyWrapper to be injected once into the webpage
chartInit :: Text
chartInit =
  [text|
  function throughputChart(renderAt, data){
    const chart = new FusionCharts({
      type: "timeseries",
      renderAt: renderAt,
      width: "100%",
      height: 250,
      dataSource: {
        data: new FusionCharts.DataStore().createDataTable((data).concat([[currentISOTimeStringVar, 0]]), 
          [{"name": "Time",
            "type": "date",
            "format": "%Y-%m-%dT%H:%M:%S%Z" // https://www.fusioncharts.com/dev/fusiontime/fusiontime-attributes
          },{"name": "Count","type": "number"}]),
        chart: {},
        navigator: {"enabled": 0},
        series: "StatusCode",
        yaxis: [{plot:[{value: "Count",type: "smooth-line"}],title: ""}]
      }
    });
    // chart.setJSONUrl("http://localhost:8080/p/51d14050-7480-4d14-be7b-3d806d1be43b/charts_json/throughput?endpoint_hash=c1356656");
    chart.render();
    return chart
  }

function throughputEChart(renderAt, data, groupby){
  const myChart = echarts.init(document.getElementById(renderAt));
  const option = {
    legend: {show: false},
    tooltip: {
      trigger: 'axis',
    },
    dataset: {
      source:data,
      dimensions: ['timestamp'].concat(groupby,  ['throughput']),
    },
    xAxis: { show: false, type: 'time' },
    yAxis: { show: false, scale: true },
    series: [
      {
         name: 'throughput',
         type: 'bar',
         barMinHeight: '1.5',
         barMinWidth: '1.5',
         encode: {
           x: 'timestamp',
           y: 'count'
         }
      }
    ]
  };
  myChart.setOption(option);
 }

  |]
