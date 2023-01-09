module Pages.Charts.Charts (throughput, chartInit, throughputEndpointHTML, QueryBy (..), GroupBy (..)) where

import Config (DashboardM, pool)
import Data.Aeson.Combinators.Decode (decode, zonedTime)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Relude
import Witch (from)

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> DashboardM (Html ())
throughputEndpointHTML _ pid idM groupBy_ endpointHash shapeHash formatHash intervalM limitM showLegend_ fromDStr toDStr = do
  pool <- asks pool
  let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
  let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
  chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid groupBy_ endpointHash shapeHash formatHash (fromMaybe 0 intervalM) limitM Nothing (fromD, toD)
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

data GroupBy
  = GBEndpoint
  | GBStatusCode

runGroupBy :: GroupBy -> Text
runGroupBy GBEndpoint = "group_by=endpoint"
runGroupBy GBStatusCode = "group_by=status_code"

-- This endpoint will return a throughput chart partial
throughput :: Projects.ProjectId -> Text -> Maybe QueryBy -> Maybe GroupBy -> Int -> Maybe Int -> Bool -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
throughput pid elemID qByM gByM intervalMinutes' limit' showLegend' (fromD, toD) = do
  let pidT = pid.toText
  let queryBy = runQueryBy <$> qByM
  let gBy = runGroupBy <$> gByM
  let queryStr = [queryBy, gBy] & catMaybes & T.intercalate "&"
  let showLegend = T.toLower $ show showLegend'
  let intervalMinutes = show intervalMinutes'
  let limit = maybe "" (\x -> "limit=" <> show x) limit'
  let fromDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) fromD
  let toDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) toD

  div_
    [ id_ $ "id-" <> elemID,
      class_ "w-full h-full",
      hxGet_ [text| /p/$pidT/charts_html/throughput?id=$elemID&show_legend=$showLegend&interval=$intervalMinutes&$limit&$queryStr&from=$fromDStr&to=$toDStr |],
      hxTrigger_ "intersect",
      hxSwap_ "outerHTML"
    ]
    ""

-- chartInit is registered once in the BodyWrapper to be injected once into the webpage
chartInit :: Text
chartInit =
  [text|
// gb = group by
function throughputEChart(renderAt, data, gb, showLegend){
  let backgroundStyle = {
    color: 'rgba(240,248,255, 0.4)'
  }
  let series ={
      name: "Throughput",
      type: 'bar',
      showBackground: true,
      backgroundStyle: backgroundStyle,
      barWidth: '60%',
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
      showBackground: true,
      backgroundStyle: backgroundStyle,
      barWidth: '60%',
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
    grid: {
        width: '100%',
        left: '0%',
        top: '1%',
        bottom: '1.5%',
        containLabel: true
    },
    tooltip: {
      trigger: 'axis',
    },
    xAxis: { show: showLegend, type: 'time', scale: true },
    yAxis: { show: showLegend, scale: true },
    series: series,
  };
  if (showLegend) {
    option.grid.bottom = '9%'
  }
  myChart.setOption(option);
 }


function latencyHistogram(renderAt, pc, data){
  const myChart = echarts.init(document.getElementById(renderAt));
  const option = {
    grid: {width: '100%', width: '100%', left: '1%',right: '-1%', top: '5%',bottom: '1.5%', containLabel: true},
    xAxis: { show: true, type: 'value', scale: true, splitLine: {show: false},
    axisLabel: {
      formatter: function (params) {
        if (params>1000){
          return `$${params/1000}s`
        }
        return `$${params}ms`
      },
      show: true,
      position: 'inside'
    },
           },
    yAxis: { show: true, type: 'value', scale: true,},
    series: {
      name: 'Direct',
      type: 'bar',
      barWidth: '60%',
      data: data,
      markLine: {
        data: [
          [
            { name: "p50", coord: [pc.p50, 0]  },
            { name: "end", coord: [pc.p50, 'max'] },
          ],
          [
            { name: "p75", coord: [pc.p75, 0] },
            { name: "end", coord: [pc.p75, 'max'] },
          ],
          [
            { name: "p90", coord: [pc.p90, 0] },
            { name: "end", coord: [pc.p90, 'max'] },
          ],
          [
            { name: "p95", coord: [pc.p95, 0] },
            { name: "end", coord: [pc.p95, 'max'] },
          ],
          [
            { name: "p99", coord: [pc.p99, 0] },
            { name: "end", coord: [pc.p99, 'max'] },
          ],
          [
            { name: "max", coord: [pc.max, 0] },
            { name: "end", coord: [pc.max, 'max'] },
          ]
        ],
      },
    },
  };
  console.log(option);
  myChart.setOption(option);
 }
  |]
