module Pages.Charts.Charts (throughput, chartInit, throughputEndpointHTML, QueryBy (..), GroupBy (..)) where

import Config (DashboardM, pool)
import Data.Text qualified as T
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Relude

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> DashboardM (Html ())
throughputEndpointHTML _ pid idM groupBy_ endpointHash shapeHash formatHash intervalM limitM showLegend_ = do
  pool <- asks pool
  chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid groupBy_ endpointHash shapeHash formatHash (fromMaybe 0 intervalM) limitM
  let entityId = fromMaybe "" idM
  let groupBy = maybe "" (\x -> "\"" <> x <> "\"") groupBy_
  let showLegend = T.toLower $ show $ fromMaybe False showLegend_
  pure $ do
    div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
    script_ [text| throughputEChart("id-$entityId", $chartData, [$groupBy], $showLegend) |]

data QueryBy
  = QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text

runQueryBy :: QueryBy -> Text
runQueryBy (QBEndpointHash t) = "endpoint_hash=" <> t
runQueryBy (QBShapeHash t) = "shape_hash=" <> t
runQueryBy (QBFormatHash t) = "format_hash=" <> t

data GroupBy = GBEndpoint

runGroupBy :: GroupBy -> Text
runGroupBy GBEndpoint = "group_by=endpoint"

-- This endpoint will return a throughput chart partial
throughput :: Projects.ProjectId -> Text -> Maybe QueryBy -> Maybe GroupBy -> Int -> Maybe Int -> Bool -> Html ()
throughput pid elemID qByM gByM intervalMinutes' limit' showLegend' = do
  let pidT = pid.toText
  let queryBy = runQueryBy <$> qByM
  let gBy = runGroupBy <$> gByM
  let queryStr = [queryBy, gBy] & catMaybes & T.intercalate "&"
  let showLegend = T.toLower $ show showLegend'
  let intervalMinutes = show intervalMinutes'
  let limit = maybe "" (\x -> "limit=" <> show x) limit'

  div_
    [ id_ $ "id-" <> elemID,
      -- style_ "height:250px",
      class_ "w-full h-full",
      hxGet_ [text| /p/$pidT/charts_html/throughput?id=$elemID&show_legend=$showLegend&interval=$intervalMinutes&$limit&$queryStr |],
      hxTrigger_ "intersect",
      hxSwap_ "outerHTML"
    ]
    ""

-- chartInit is registered once in the BodyWrapper to be injected once into the webpage
chartInit :: Text
chartInit =
  [text|
function throughputEChart(renderAt, data, gb, showLegend){
  let series ={
      name: "Throughput",
      type: 'bar',
      barMinHeight: '1.5',
      barMinWidth: '1.5',
      encode: {
        x: 'timestamp',
        y: 'throughput',
      },
      data: data,
    } 
  if (gb.length>0){
    const newData = data.reduce((mp, curr) => {
      if (!mp[curr[1]]) mp[curr[1]] = [];
      mp[curr[1]].push([curr[0], curr[2]]);
      return mp;
    }, {});
    series = Object.entries(newData).map(([k, v]) => ({
      name: k,
      type: 'bar',
      stack: 'Endpoints',
      barMinHeight: '1.5',
      barMinWidth: '1.5',
      encode: {
        x: 'timestamp',
        y: 'throughput',
      },
      data: v,
    }));
  }

  const myChart = echarts.init(document.getElementById(renderAt));
  const option = {
    legend: {show: showLegend, type: 'scroll', top: 'bottom'},
    tooltip: {
      trigger: 'axis',
    },
    xAxis: { show: showLegend, type: 'time' },
    yAxis: { show: showLegend, scale: true },
    series: series,
  };
  myChart.setOption(option);
 }

  |]
