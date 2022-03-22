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
import Lucid.Svg qualified as Svg
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
  (project, reqLatencyPercentiles, reqsByEndpoint, reqLatenciesRolledByStepsLabeled, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      reqLatencyPercentilesM <- RequestDumps.selectReqLatencyPercentilesForProject pid
      let reqLatencyPercentiles = Unsafe.fromJust reqLatencyPercentilesM
      reqsByEndpoint <- RequestDumps.selectRequestsByEndpointsStatByMin pid

      let maxV = round (reqLatencyPercentiles ^. #max) :: Int
      let steps' = (maxV `quot` 100) :: Int
      let steps = if steps' == 0 then 100 else steps'
      reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledByStepsForProject maxV steps pid

      let reqLatencyPercentileSteps =
            ( (round (reqLatencyPercentiles ^. #max) `div` steps) * steps,
              (round (reqLatencyPercentiles ^. #p90) `div` steps) * steps,
              (round (reqLatencyPercentiles ^. #p75) `div` steps) * steps,
              (round (reqLatencyPercentiles ^. #p50) `div` steps) * steps
            )

      let reqLatenciesRolledByStepsLabeled = Vector.toList reqLatenciesRolledBySteps & map \(x, y) -> RequestDumps.labelRequestLatency reqLatencyPercentileSteps (x, y)
      anomalies <- Anomalies.selectOngoingAnomalies pid
      pure (project, reqLatencyPercentiles, reqsByEndpoint, concat reqLatenciesRolledByStepsLabeled, anomalies)

  let reqsByEndpointJ = decodeUtf8 $ AE.encode reqsByEndpoint
  let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Dashboard"
          }
  pure $ bodyWrapper bwconf $ dashboardPage reqLatencyPercentiles reqsByEndpointJ reqLatenciesRolledByStepsJ anomalies

dashboardPage :: RequestDumps.Percentiles -> Text -> Text -> Vector Anomalies.AnomalyVM -> Html ()
dashboardPage percentiles reqsByEndpointJ reqLatenciesRolledByStepsJ anomalies = do
  section_ [class_ "p-8 container mx-auto px-4 pt-10 pb-24"] $ do
    section_ $ AnomaliesList.anomalyListSlider anomalies
    dStats percentiles
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

dStats :: RequestDumps.Percentiles -> Html ()
dStats percentiles =
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
      div_ [class_ "col-span-1 content-between space-y-8"] $ do
        div_ [class_ "row-span-1 col-span-1 bg-white  border border-gray-100  rounded-2xl p-3 flex flex-row justify-between"] $ do
          div_ [class_ "flex flex-col justify-center"] $ do
            span_ "avg Reqs per minute"
            div_ [class_ "inline-block flex flex-row content-between"] $ do
              strong_ [class_ "text-xl"] "3.5k"
              div_ [class_ "inline-flex justify-center text-red-700 "] $ do
                img_ [class_ "inline-block", src_ "/assets/svgs/down-arrow-red.svg"]
                span_ "10.5%"
          div_ $ do
            Svg.svg_ [Svg.width_ "90", Svg.height_ "88", Svg.viewBox_ "0 0 90 88", Svg.fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] $ do
              Svg.mask_ [Svg.id_ "mask0_4463_66717", Svg.style_ "mask-type:alpha", Svg.maskUnits_ "userSpaceOnUse", Svg.x_ "0", Svg.y_ "0", Svg.width_ "90", height_ "88"] $ do
                Svg.rect_ [Svg.opacity_ "0.1", Svg.x_ "0.51416", Svg.width_ "89.2571", Svg.height_ "88", Svg.rx_ "22", Svg.fill_ "#FF965D"]
              Svg.g_ [Svg.mask_ "url(#mask0_4463_66717)"] $ do
                Svg.rect_ [Svg.x_ "0.51416", Svg.width_ "89.2571", Svg.height_ "88", Svg.fill_ "#FF965D"]
              Svg.path_ [Svg.d_ "M25.6685 50.55C25.6685 49.5835 26.5161 48.8 27.5618 48.8H33.5123C34.5579 48.8 35.4056 49.5835 35.4056 50.55V60.8H25.6685V50.55Z", Svg.fill_ "#FF965D"]
              Svg.path_ [Svg.d_ "M40.2744 38.7091C40.2744 37.6547 41.1221 36.8 42.1678 36.8H48.1182C49.1639 36.8 50.0116 37.6547 50.0116 38.7091V60.8H40.2744V38.7091Z", Svg.fill_ "#FF965D"]
              Svg.path_ [Svg.d_ "M54.8799 29.16C54.8799 28.0775 55.7276 27.2 56.7732 27.2H62.7237C63.7694 27.2 64.617 28.0775 64.617 29.16V60.8H54.8799V29.16Z", Svg.fill_ "#FF965D"]
        div_ [class_ "row-span-1 col-span-1 bg-white  border border-gray-100  rounded-xl p-3 flex flex-row content-between"] $ do
          div_ $ do
            span_ "Anomalies this week"
            div_ [class_ "inline-block flex flex-row content-between"] $ do
              strong_ [class_ "text-xl"] "3"
              div_ [class_ "inline-block text-red-700"] $ do
                img_ [class_ "inline-block", src_ "/assets/svgs/down-arrow-red.svg"]
                span_ "10.5%"
          div_ $ do
            Svg.svg_ [Svg.width_ "86", Svg.height_ "86", Svg.viewBox_ "0 0 86 86", Svg.fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] $ do
              Svg.circle_ [Svg.cx_ "43", Svg.cy_ "43", Svg.r_ "40.5", Svg.stroke_ "#F8F8F8", Svg.stroke_width_ "5"]
              Svg.path_ [Svg.d_ "M43 2.5C51.5528 2.5 59.886 5.20763 66.8053 10.2348C73.7246 15.262 78.8748 22.3507 81.5178 30.4848C84.1607 38.619 84.1607 47.381 81.5178 55.5152C78.8748 63.6493 73.7246 70.738 66.8053 75.7652C59.886 80.7924 51.5528 83.5 43 83.5C34.4472 83.5 26.114 80.7924 19.1947 75.7652C12.2754 70.738 7.12516 63.6493 4.48221 55.5152C1.83926 47.381 1.83926 38.619 4.48221 30.4848", Svg.stroke_ "#FF965D", Svg.stroke_width_ "5", Svg.stroke_linecap_ "round"]
              Svg.text_ "80%"
        div_ [class_ "row-span-1 col-span-1 bg-white  border border-gray-100  rounded-xl p-3 flex flex-row content-between"] $ do
          div_ $ do
            span_ "Anomalies this week"
            div_ [class_ "inline-block flex flex-row content-between"] $ do
              strong_ [class_ "text-xl"] "3"
              div_ [class_ "inline-block text-red-700"] $ do
                img_ [class_ "inline-block", src_ "/assets/svgs/down-arrow-red.svg"]
                span_ "10.5%"
          div_ $ do
            Svg.svg_ [Svg.width_ "86", Svg.height_ "86", Svg.viewBox_ "0 0 86 86", Svg.fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] $ do
              Svg.circle_ [Svg.cx_ "43", Svg.cy_ "43", Svg.r_ "40.5", Svg.stroke_ "#F8F8F8", Svg.stroke_width_ "5"]
              Svg.path_ [Svg.d_ "M43 2.5C51.5528 2.5 59.886 5.20763 66.8053 10.2348C73.7246 15.262 78.8748 22.3507 81.5178 30.4848C84.1607 38.619 84.1607 47.381 81.5178 55.5152C78.8748 63.6493 73.7246 70.738 66.8053 75.7652C59.886 80.7924 51.5528 83.5 43 83.5C34.4472 83.5 26.114 80.7924 19.1947 75.7652C12.2754 70.738 7.12516 63.6493 4.48221 55.5152C1.83926 47.381 1.83926 38.619 4.48221 30.4848", Svg.stroke_ "#FF965D", Svg.stroke_width_ "5", Svg.stroke_linecap_ "round"]
              Svg.text_ "80%"
      div_ [class_ "col-span-2 bg-white  border border-gray-100  row-span-2 rounded-2xl p-3"] $ do
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
              percentileRow "max" $ percentiles ^. #max
              percentileRow "p99" $ percentiles ^. #p99
              percentileRow "p95" $ percentiles ^. #p95
              percentileRow "p90" $ percentiles ^. #p90
              percentileRow "p75" $ percentiles ^. #p75
              percentileRow "p50" $ percentiles ^. #p50
              percentileRow "min" $ percentiles ^. #min

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  li_ [class_ "flex flex-row content-between justify-between"] $ do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block"] $ do
      span_ $ toHtml ((fmt $ fixedF 2 p) :: Text)
      span_ "ms"
