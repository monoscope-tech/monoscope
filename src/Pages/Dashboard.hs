module Pages.Dashboard (dashboardGetH) where

import Config
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Fmt (fixedF, fmt)
import Lucid
import Lucid.Hyperscript
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper
import Relude
import Relude.Unsafe qualified as Unsafe

dashboardGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
dashboardGetH sess pid = do
  pool <- asks pool
  (project, projectRequestStats, reqsByEndpoint, reqLatenciesRolledByStepsLabeled, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      projectRequestStats <- Unsafe.fromJust <$> Projects.projectRequestStatsByProject pid
      reqsByEndpoint <- RequestDumps.selectRequestsByEndpointsStatByMin pid

      let maxV = round (projectRequestStats ^. #max) :: Int
      let steps' = (maxV `quot` 100) :: Int
      let steps = if steps' == 0 then 100 else steps'
      reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledByStepsForProject maxV steps pid

      let reqLatencyPercentileSteps =
            ( (round (projectRequestStats ^. #max) `div` steps) * steps,
              (round (projectRequestStats ^. #p90) `div` steps) * steps,
              (round (projectRequestStats ^. #p75) `div` steps) * steps,
              (round (projectRequestStats ^. #p50) `div` steps) * steps
            )

      let reqLatenciesRolledByStepsLabeled = Vector.toList reqLatenciesRolledBySteps & map \(x, y) -> RequestDumps.labelRequestLatency reqLatencyPercentileSteps (x, y)
      anomalies <- Anomalies.selectOngoingAnomalies pid
      pure (project, projectRequestStats, reqsByEndpoint, concat reqLatenciesRolledByStepsLabeled, anomalies)

  let reqsByEndpointJ = decodeUtf8 $ AE.encode reqsByEndpoint
  let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Dashboard"
          }
  pure $ bodyWrapper bwconf $ dashboardPage projectRequestStats reqsByEndpointJ reqLatenciesRolledByStepsJ anomalies

dashboardPage :: Projects.ProjectRequestStats -> Text -> Text -> Vector Anomalies.AnomalyVM -> Html ()
dashboardPage projectStats reqsByEndpointJ reqLatenciesRolledByStepsJ anomalies = do
  section_ [class_ "p-8 container mx-auto px-4 space-y-16 10 pb-24"] $ do
    section_ $ AnomaliesList.anomalyListSlider anomalies
    dStats projectStats
  script_
    [text|
        new FusionCharts({
          type: "timeseries",
          renderAt: "reqByStatusCode",
          width: "95%",
          height: 350,
          dataSource: {
            data: new FusionCharts.DataStore().createDataTable($reqsByEndpointJ, [{
            "name": "Time",
            "type": "date",
            "format": "%Y-%m-%dT%H:%M:%S%Z" // https://www.fusioncharts.com/dev/fusiontime/fusiontime-attributes
        }, {
            "name": "StatusCode",
            "type": "string"
        },{
            "name": "Count",
            "type": "number"
        }]),
            chart: {},
            navigator: {
                "enabled": 0
            },
            series: "StatusCode",
            yaxis: [
              {
                plot:[{
                  value: "Count",
                  type: "column"
                }],
                title: ""
              }
            ]
          }
        }).render();


        new FusionCharts({
          type: "column2d",
          renderAt: "reqsLatencyHistogram",
          width: "100%",
          // height: "auto",
          dataSource: {
            data:  $reqLatenciesRolledByStepsJ,
            "chart": {
                "theme": "fusion",
                "xAxisName": "Latency in ms",
                "yAxisName": "Count",
                // "numberSuffix": "K"
            },
          }
        }).render();

      |]

dStats :: Projects.ProjectRequestStats -> Html ()
dStats projReqStats =
  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]] $
          img_
            [ src_ "/assets/svgs/cheveron-down.svg",
              class_ "h-4 mr-3 mt-1 w-4"
            ]
        span_ [class_ "text-lg text-slate-700"] "Stats"

    div_ [class_ "grid grid-cols-3  gap-5 reqResSubSection"] $ do
      div_ [class_ "col-span-3 flex flex-row gap-5"] $ do
        div_ [class_ "col-span-1 shrink content-between space-y-2"] $ do
          div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-2xl p-5 flex flex-row justify-between "] $ do
            div_ [class_ "flex flex-col justify-center"] $ do
              span_ "Total Requests"
              div_ [class_ "inline-block flex flex-row content-between"] $ do
                strong_ [class_ "text-xl"] $ toHtml @Text $ show (projReqStats ^. #totalRequests)

          div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-xl p-5 flex flex-row content-between "] $ do
            div_ $ do
              span_ "Total Anomaliesk"
              div_ [class_ "inline-block flex flex-row content-between"] $ do
                strong_ [class_ "text-xl"] $ toHtml @Text $ show (projReqStats ^. #totalAnomalies)

          div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-xl p-5 flex flex-row content-between "] $ do
            div_ $ do
              span_ "Managed Endpoints"
              div_ [class_ "inline-block flex flex-row content-between"] $ do
                strong_ [class_ "text-xl"] $ toHtml @Text $ show (projReqStats ^. #totalEndpoints)

          div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-xl p-5 flex flex-row content-between "] $ do
            div_ $ do
              span_ "Total shapes"
              div_ [class_ "inline-block flex flex-row content-between"] $ do
                strong_ [class_ "text-xl"] $ toHtml @Text $ show (projReqStats ^. #totalShapes)

          div_ [class_ "col-span-1 bg-white  border border-gray-100  rounded-xl p-5 flex flex-row content-between "] $ do
            div_ $ do
              span_ "Total Fields"
              div_ [class_ "inline-block flex flex-row content-between"] $ do
                strong_ [class_ "text-xl"] $ toHtml @Text $ show (projReqStats ^. #totalFields)

        div_ [class_ "flex-1 grow bg-white  border border-gray-100  row-span-2 rounded-2xl p-3"] $ do
          div_ [class_ "p-4"] $ do
            select_ [] $ do
              option_ "Reqs Grouped by Endpoint"
              option_ "Avg Reqs per minute"
          div_ [id_ "reqByStatusCode", class_ ""] ""

      div_ [class_ "col-span-3 bg-white   border border-gray-100  rounded-xl py-3 px-6"] $ do
        div_ [class_ "p-4"] $ do
          select_ [] $ do
            option_ "Request Latency Distribution"
            option_ "Avg Reqs per minute"
        div_ [class_ "flex flex-row gap-5"] $ do
          div_ [id_ "reqsLatencyHistogram", class_ "grow"] ""
          div_ [class_ "flex-1 space-y-2 min-w-[20%]"] $ do
            strong_ [class_ "block"] "Latency Percentiles"
            ul_ [class_ "space-y-1"] $ do
              percentileRow "max" $ projReqStats ^. #max
              percentileRow "p99" $ projReqStats ^. #p99
              percentileRow "p95" $ projReqStats ^. #p95
              percentileRow "p90" $ projReqStats ^. #p90
              percentileRow "p75" $ projReqStats ^. #p75
              percentileRow "p50" $ projReqStats ^. #p50
              percentileRow "min" $ projReqStats ^. #min

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  li_ [class_ "flex flex-row content-between justify-between"] $ do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block"] $ do
      span_ $ toHtml ((fmt $ fixedF 2 p) :: Text)
      span_ "ms"
