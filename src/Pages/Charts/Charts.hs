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

throughputEndpoint :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> DashboardM ByteString
throughputEndpoint _ pid endpointHash shapeHash = do
  pool <- asks pool
  liftIO $ withPool pool $ from @Text <$> RequestDumps.throughputBy pid endpointHash shapeHash

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> DashboardM (Html ())
throughputEndpointHTML _ pid endpointHash shapeHash = do
  pool <- asks pool
  chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid endpointHash shapeHash
  let entityId = fromMaybe "" $ endpointHash <|> shapeHash
  pure $ do
    div_ [id_ $ "id-" <> entityId, style_ "height:250px", class_ "w-full"] ""
    script_ [text| throughputChart("id-$entityId", $chartData) |]

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
      style_ "height:250px",
      class_ "w-full",
      hxGet_ [text| /p/$pidT/charts_html/throughput?$queryKey=$queryValue |],
      hxTrigger_ "intersect"
    ]
    ""
  -- [__| init call throughputChart(my @data-entityId) then
  -- on intersection(intersecting) fetch/|]
  -- ] ""
  script_ [text| throughputChart("id-$queryValue", [])|]

-- anomalyChartScript :: Anomalies.AnomalyVM -> Text -> Text
-- anomalyChartScript anomaly anomalyGraphId =
--   let timeSeriesData = fromMaybe "[]" $ anomaly.timeSeries
--    in -- Adding the current day and time to the end of the chart data, so that the chart is scaled to include the current day/time
--       -- currentISOTimeStringVar is declared on every page, in case they need a string for the current time in ISO format
--       [text|
--       new FusionCharts({
--         type: "timeseries",
--         renderAt: "$anomalyGraphId",
--         width: "100%",
--         height: 250,
--         dataSource: {
--           data: new FusionCharts.DataStore().createDataTable(($timeSeriesData).concat([[currentISOTimeStringVar, 0]]),
--           [{"name": "Time",
--             "type": "date",
--             "format": "%Y-%m-%dT%H:%M:%S%Z" // https://www.fusioncharts.com/dev/fusiontime/fusiontime-attributes
--           },{"name": "Count","type": "number"}]),
--           chart: {},
--           navigator: {"enabled": 0},
--           series: "StatusCode",
--           yaxis: [{plot:[{value: "Count",type: "smooth-line"}],title: ""}]
--         }
--       }).render();
--      |]

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
    return chart
  }
  |]
