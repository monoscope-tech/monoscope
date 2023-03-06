module Pages.Charts.Charts (throughput, latency, chartInit, throughputEndpointHTML, latencyEndpointHTML, QueryBy (..), GroupBy (..)) where

import Config (DashboardM, pool)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, utc, utcToZonedTime, zonedTimeToUTC)
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

throughputEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text-> DashboardM (Html ())
throughputEndpointHTML _ pid idM groupBy_ endpointHash shapeHash formatHash statusCodeGT numSlotsM limitM showLegend_ fromDStr toDStr chartTheme= do
  pool <- asks pool
  let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
  let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
  chartData <- liftIO $ withPool pool $ RequestDumps.throughputBy pid groupBy_ endpointHash shapeHash formatHash statusCodeGT (fromMaybe 0 numSlotsM) limitM Nothing (fromD, toD)
  let entityId = fromMaybe "" idM
  let groupBy = maybe "" (\x -> "\"" <> x <> "\"") groupBy_
  let showLegend = T.toLower $ show $ fromMaybe False showLegend_
  let chartThemeTxt = fromMaybe "" chartTheme
  pure $ do
    div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
    script_ [text| throughputEChart("id-$entityId", $chartData, [$groupBy], $showLegend, "$chartThemeTxt") |]

latencyEndpointHTML :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Int-> Maybe Text -> Maybe Text  -> Maybe Text-> DashboardM (Html ())
latencyEndpointHTML _ pid idM endpointHash numSlotsM fromDStr toDStr chartTheme = do
  pool <- asks pool
  let fromD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
  let toD = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
  chartData <- liftIO $ withPool pool $ RequestDumps.latencyBy pid endpointHash  (fromMaybe 0 numSlotsM) (fromD, toD)
  let chartThemeTxt = fromMaybe "" chartTheme
  let entityId = fromMaybe "" idM
  pure $ do
    div_ [id_ $ "id-" <> entityId, class_ "w-full h-full"] ""
    script_ [text| latencyEChart("id-$entityId", $chartData, "$chartThemeTxt") |]


data QueryBy
  = QBEndpointHash Text
  | QBShapeHash Text
  | QBFormatHash Text
  | QBStatusCodeGT Int
  | QBAnd QueryBy QueryBy

runQueryBy :: QueryBy -> Text
runQueryBy (QBEndpointHash t) = "endpoint_hash=" <> t
runQueryBy (QBShapeHash t) = "shape_hash=" <> t
runQueryBy (QBFormatHash t) = "format_hash=" <> t
runQueryBy (QBStatusCodeGT t) = "status_code_gt=" <> show t
runQueryBy (QBAnd a b ) = runQueryBy a <> "&" <> runQueryBy b

data GroupBy
  = GBEndpoint
  | GBStatusCode

runGroupBy :: GroupBy -> Text
runGroupBy GBEndpoint = "group_by=endpoint"
runGroupBy GBStatusCode = "group_by=status_code"

-- This endpoint will return a throughput chart partial
throughput :: Projects.ProjectId -> Text -> Maybe QueryBy -> Maybe GroupBy -> Int -> Maybe Int -> Bool -> (Maybe ZonedTime, Maybe ZonedTime) -> Maybe Text -> Html ()
throughput pid elemID qByM gByM numSlots' limit' showLegend' (fromD, toD) chartTheme= do
  let pidT = pid.toText
  let queryBy = runQueryBy <$> qByM
  let gBy = runGroupBy <$> gByM
  let queryStr = [queryBy, gBy] & catMaybes & T.intercalate "&"
  let showLegend = T.toLower $ show showLegend'
  let numSlots = show numSlots' 
  let limit = maybe "" (\x -> "limit=" <> show x) limit'
  let fromDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) fromD
  let toDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) toD
  let theme = fromMaybe "" chartTheme

  div_
    [ id_ $ "id-" <> elemID
    , class_ "w-full h-full"
    , hxGet_ [text| /p/$pidT/charts_html/throughput?id=$elemID&show_legend=$showLegend&num_slots=$numSlots&$limit&$queryStr&from=$fromDStr&to=$toDStr&theme=$theme |]
    , hxTrigger_ "intersect"
    , hxSwap_ "outerHTML"
    ]
    ""

latency :: Projects.ProjectId -> Text -> Maybe QueryBy -> Int -> (Maybe ZonedTime, Maybe ZonedTime) -> Maybe Text -> Html ()
latency pid elemID qByM numSlots' (fromD, toD) chartTheme= do
  let pidT = pid.toText
  let queryBy = runQueryBy <$> qByM
  let queryStr = [queryBy] & catMaybes & T.intercalate "&"
  let numSlots = show numSlots' 
  let fromDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) fromD
  let toDStr = from @String @Text $ maybe "" (iso8601Show . zonedTimeToUTC) toD
  let theme = fromMaybe "" chartTheme

  div_
    [ id_ $ "id-" <> elemID
    , class_ "w-full h-full"
    , hxGet_ [text| /p/$pidT/charts_html/latency?id=$elemID&num_slots=$numSlots&$queryStr&from=$fromDStr&to=$toDStr&theme=$theme |]
    , hxTrigger_ "intersect"
    , hxSwap_ "outerHTML"
    ]
    ""

-- chartInit is registered once in the BodyWrapper to be injected once into the webpage
chartInit :: Text
chartInit =
  [text|
// gb = group by
function throughputEChart(renderAt, data, gb, showLegend, theme){
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

  const myChart = echarts.init(document.getElementById(renderAt), theme);
  const option = {
    legend: {show: showLegend, type: 'scroll', top: 'bottom'},
    grid: {
        width: '100%',
        left: '0%',
        top: '5%',
        bottom: '1.8%',
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
console.log(option)
  myChart.setOption(option);
 }

  function latencyEChart(renderAt, data, theme ) {
    const showLegend = true;
    const option = {
      tooltip: {
        trigger: 'axis'
      },
      legend: {show: showLegend, type: 'scroll', top: 'bottom'},
      xAxis: {
        type: 'category',
        scale: true
      },
      yAxis: {
        type: 'value',
        scale: true,
        boundaryGap: ['5%', '5%'],
        axisLabel: {
          formatter: function (params) {
            if (params>1000){
              return `$${params/1000}s`
            }
            return `$${params}ms`
          },
          show: true,
          position: 'inside'
        }
      },
      grid: {
        width: '100%',
        left: '0%',
        top: '5%',
        bottom: '12%',
        containLabel: true
      },
      dataset: {
        dimensions: ['date', 'p50', 'p75', 'p90'],
        source: data 
      },
      series: [
        {
          type: 'line',
          name: 'p50',
          encode: {
            x: 'date',
            y: 'p50'
          },
          smooth: true
        },
        {
          type: 'line',
          name: 'p75',
          encode: {
            x: 'date',
            y: 'p75'
          },
          smooth: true
        },
        {
          type: 'line',
          name: 'p90',
          encode: {
            x: 'date',
            y: 'p90'
          },
          smooth: true
        }
      ]
    };
    const myChart = echarts.init(document.getElementById(renderAt), theme);
    myChart.setOption(option);
  }


function latencyHistogram(renderAt, pc, data){
  const myChart = echarts.init(document.getElementById(renderAt));
  const option = {
    grid: {width: '100%', width: '100%', left: '1%',right: '-1%', top: '10%',bottom: '1.5%', containLabel: true},
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
  myChart.setOption(option);
 }
  |]
