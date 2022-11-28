module Pages.Dashboard (dashboardGetH) where

import Config
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Time (UTCTime, getCurrentTime)
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
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Relude

dashboardGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
dashboardGetH sess pid = do
  pool <- asks pool
  (project, projectRequestStats, reqsByEndpointJ, reqLatenciesRolledByStepsLabeled, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      projectRequestStats <- fromMaybe (def :: Projects.ProjectRequestStats) <$> Projects.projectRequestStatsByProject pid
      reqsByEndpoint <- RequestDumps.selectRequestsByEndpointsStatByMin pid

      let maxV = round (projectRequestStats.max) :: Int
      let steps' = (maxV `quot` 100) :: Int
      let steps = if steps' == 0 then 100 else steps'
      reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledByStepsForProject maxV steps pid

      let reqLatencyPercentileSteps =
            ( (round (projectRequestStats.max) `div` steps) * steps,
              (round (projectRequestStats.p90) `div` steps) * steps,
              (round (projectRequestStats.p75) `div` steps) * steps,
              (round (projectRequestStats.p50) `div` steps) * steps
            )

      let reqLatenciesRolledByStepsLabeled = Vector.toList reqLatenciesRolledBySteps & map \(x, y) -> RequestDumps.labelRequestLatency reqLatencyPercentileSteps (x, y)
      anomalies <- Anomalies.selectAnomalies pid Nothing (Just False) (Just False) Nothing
      pure (project, projectRequestStats, reqsByEndpoint, concat reqLatenciesRolledByStepsLabeled, anomalies)
  let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Dashboard"
          }
  currTime <- liftIO $ getCurrentTime
  pure $ bodyWrapper bwconf $ dashboardPage currTime projectRequestStats reqsByEndpointJ reqLatenciesRolledByStepsJ anomalies

dashboardPage :: UTCTime -> Projects.ProjectRequestStats -> Text -> Text -> Vector Anomalies.AnomalyVM -> Html ()
dashboardPage currTime projectStats reqsByEndpointJ reqLatenciesRolledByStepsJ anomalies = do
  section_ [class_ "p-8 container mx-auto px-4 space-y-16 10 pb-24"] $ do
    section_ $ AnomaliesList.anomalyListSlider currTime anomalies
    dStats projectStats reqsByEndpointJ
  script_
    [text|
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

statBox :: Text -> Text -> Html ()
statBox title val = do
  div_ [class_ "col-span-1 card-round p-5 flex flex-row content-between "] $ do
    div_ $ do
      div_ [class_ "inline-block flex flex-row content-between"] $ do
        strong_ [class_ "font-normal text-2xl"] $ toHtml val
      small_ $ toHtml title

dStats :: Projects.ProjectRequestStats -> Text -> Html ()
dStats projReqStats reqsByEndpointJ = do
  when (reqsByEndpointJ == "[]") do
    section_ [class_ "card-round p-5 sm:p-10 space-y-4 text-lg"] $ do
      h2_ [class_ "text-2xl"] "Welcome onboard APIToolkit."
      p_ "You're currently not sending any data to apitoolkit from your backends yet. Here's a guide to get you setup for your tech stack."
      a_ [href_ "https://apitoolkit.io/docs/quickstarts/", class_ "btn-indigo btn-sm my-3 -ml-0 mt-6", target_ "_blank"] "Read the setup guide"

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
          statBox "Total Requests" (show (projReqStats.totalRequests))
          statBox "Total Anomalies" (show (projReqStats.totalAnomalies))
          statBox "Managed Endpoints" (show (projReqStats.totalEndpoints))
          statBox "Total Shapes" (show (projReqStats.totalShapes))
          statBox "Total Fields" (show (projReqStats.totalFields))

        div_ [class_ "flex-1 grow card-round row-span-2 p-3"] $ do
          div_ [class_ "p-4 h-full space-y-6"] $ do
            select_ [] $ do
              option_ [class_ "text-2xl font-normal"] "Reqs Grouped by Endpoint"
            Charts.throughput projReqStats.projectId "reqsByEndpoints" Nothing (Just Charts.GBEndpoint) (3 * 60) Nothing True

      div_ [class_ "col-span-3 card-round py-3 px-6"] $ do
        div_ [class_ "p-4"] $ do
          select_ [] $ do
            option_ "Request Latency Distribution"
            option_ "Avg Reqs per minute"
        div_ [class_ "flex flex-row gap-8"] $ do
          div_ [id_ "reqsLatencyHistogram", class_ "grow"] ""
          div_ [class_ "flex-1 space-y-4 min-w-[20%]"] $ do
            span_ [class_ "block text-right text-lg"] "Latency Percentiles"
            ul_ [class_ "space-y-1 divide-y divide-slate-100"] $ do
              percentileRow "max" $ projReqStats.max
              percentileRow "p99" $ projReqStats.p99
              percentileRow "p95" $ projReqStats.p95
              percentileRow "p90" $ projReqStats.p90
              percentileRow "p75" $ projReqStats.p75
              percentileRow "p50" $ projReqStats.p50
              percentileRow "min" $ projReqStats.min

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  li_ [class_ "flex flex-row content-between justify-between"] $ do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block"] $ do
      span_ $ toHtml ((fmt $ fixedF 2 p) :: Text)
      span_ "ms"
