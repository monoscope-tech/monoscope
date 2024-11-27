module Pages.Dashboard (dashboardGetH, DashboardGet (..)) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Text qualified as T
import Data.Time (
  NominalDiffTime,
  UTCTime,
  addUTCTime,
  diffUTCTime,
  formatTime,
  getCurrentTime,
  getCurrentTimeZone,
  getZonedTime,
  secondsToNominalDiffTime,
  zonedTimeToUTC,
 )
import Data.Time.Format (defaultTimeLocale)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as Vector
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (fixedF, fmt)
import Lucid
import Lucid.Htmx (hxPost_, hxSwap_)
import Lucid.Hyperscript (__)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper (
  BWConfig (currProject, pageTitle, sessM),
  PageCtx (..),
  pageActions,
 )
import Pages.Charts.Charts qualified as C
import Pages.Charts.Charts qualified as Charts
import Pages.Components (emptyState_, statBox)
import Pages.Endpoints.EndpointList (renderEndpoint)
import Pkg.Components qualified as Components
import Relude hiding (max, min)
import System.Types
import Text.Interpolation.Nyan (int, rmode')
import Utils (convertToDHMS, faSprite_, freeTierLimitExceededBanner, parseTime)
import Witch (from)


data ParamInput = ParamInput
  { currentURL :: Text
  , sinceStr :: Maybe Text
  , dateRange :: (Maybe UTCTime, Maybe UTCTime)
  }


data DashboardGet = DashboardGet
  { unwrap :: (Projects.ProjectId, ParamInput, UTCTime, Projects.ProjectRequestStats, Vector.Vector Endpoints.EndpointRequestStats, Text, Text, (Maybe UTCTime, Maybe UTCTime), Bool, Bool)
  }


instance ToHtml DashboardGet where
  toHtml (DashboardGet (pid, paramInput, now, stats, endpoints, lastRequestTime, daysLeft, (fromD, toD), exceededFree, hasRequest)) = toHtml $ dashboardPage pid paramInput now stats endpoints lastRequestTime daysLeft (fromD, toD) exceededFree hasRequest
  toHtmlRaw = toHtml


dashboardGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid fromDStr toDStr sinceStr' = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let sinceStr = if isNothing fromDStr && isNothing toDStr && isNothing sinceStr' || fromDStr == Just "" then Just "24H" else sinceStr'
  hasRequests <- dbtToEff $ RequestDumps.hasRequest pid
  newEndpoints <- dbtToEff $ Endpoints.endpointRequestStatsByProject pid False False Nothing Nothing Nothing 0 "Incoming"
  -- TODO: Replace with a duration parser.
  let (fromD, toD, currentRange) = parseTime fromDStr toDStr sinceStr now

  (projectRequestStats, reqLatenciesRolledByStepsLabeled, freeTierExceeded) <- dbtToEff do
    projectRequestStats <- fromMaybe (def :: Projects.ProjectRequestStats) <$> Projects.projectRequestStatsByProject pid
    let maxV = round projectRequestStats.p99 :: Int
    let steps' = (maxV `quot` 100) :: Int
    let steps = if steps' == 0 then 100 else steps'
    reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledByStepsForProject maxV steps pid (fromD, toD)
    freeTierExceeded <-
      if project.paymentPlan == "Free"
        then do
          totalRequest <- RequestDumps.getLastSevenDaysTotalRequest pid
          return $ totalRequest > 5000
        else do
          return False
    pure (projectRequestStats, Vector.toList reqLatenciesRolledBySteps, freeTierExceeded)

  let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Dashboard"
          , pageActions = Just $ Components.timepicker_ Nothing currentRange
          }
  currTime <- liftIO getCurrentTime
  let createdUTc = zonedTimeToUTC project.createdAt
      (days, hours, minutes, _seconds) = convertToDHMS $ diffUTCTime currTime createdUTc
      daysLeft =
        if days >= 0 && project.paymentPlan /= "Free"
          then show days <> " days, " <> show hours <> " hours, " <> show minutes <> " minutes"
          else "-"
      currentURL = "/p/" <> pid.toText <> "?&from=" <> fromMaybe "" fromDStr <> "&to=" <> fromMaybe "" toDStr
      paramInput = ParamInput{currentURL = currentURL, sinceStr = sinceStr, dateRange = (fromD, toD)}
  addRespHeaders $ PageCtx bwconf $ DashboardGet (pid, paramInput, currTime, projectRequestStats, newEndpoints, reqLatenciesRolledByStepsJ, daysLeft, (fromD, toD), freeTierExceeded, hasRequests)


dashboardPage :: Projects.ProjectId -> ParamInput -> UTCTime -> Projects.ProjectRequestStats -> Vector.Vector Endpoints.EndpointRequestStats -> Text -> Text -> (Maybe UTCTime, Maybe UTCTime) -> Bool -> Bool -> Html ()
dashboardPage pid paramInput currTime projectStats newEndpoints reqLatenciesRolledByStepsJ daysLeft dateRange exceededFreeTier hasRequest = do
  let bulkActionBase = "/p/" <> pid.toText <> "/anomalies/bulk_actions"
  section_ [class_ "  mx-auto px-6 w-full space-y-12 pb-24 overflow-y-scroll  h-full"] do
    unless (null newEndpoints) $
      div_ [id_ "modalContainer"] do
        input_ [type_ "checkbox", id_ "newEndpointsModal", class_ "modal-toggle"]
        div_ [class_ "modal", role_ "dialog", hxSwap_ "outerHTML"] do
          form_
            [ class_ "modal-box w-1/2 max-w-5xl"
            , hxPost_ $ bulkActionBase <> "/acknowlege"
            , id_ "endpointsForm"
            ]
            do
              div_ [class_ "flex items-start py-2 border-b justify-between"] do
                h3_ [class_ "text-xl font-bold text-gray-900"] "New Endpoints Detected"
                button_ [type_ "button", class_ "btn btn btn-sm btn-circle btn-ghost text-xl", onclick_ "closeNewEndpointsModal(event)"] do
                  faSprite_ "xmark" "regular" "h-4 w-4 inline-block"
              div_ [class_ "w-full"] do
                div_ [class_ "text-xl space-y-6 overflow-y-auto", style_ "min-height:30vh;max-height:70vh; width:100%"] do
                  mapM_ (renderEndpoint False currTime) newEndpoints
              div_ [class_ "flex w-full justify-end items-center p-6 gap-4 space-x-2 border-t border-gray-200 rounded-b"] do
                button_
                  [ class_ "btn btn-primary"
                  , [__|on click set .endpoint_anomaly_input.checked to true then htmx.trigger("#endpointsForm", "submit")|]
                  ]
                  "Acknowledge All"
          label_ [class_ "modal-backdrop", Lucid.for_ "newEndpointsModal"] "Close"

    -- button_ [class_ "", id_ "checkin", onclick_ "window.picker.show()"] "timepicker"
    section_ $ AnomaliesList.anomalyListSlider currTime pid Nothing Nothing
    dStats pid projectStats reqLatenciesRolledByStepsJ dateRange hasRequest
  -- TODO delete most of this
  script_
    [text|

    function closeNewEndpointsModal(event) {
      document.getElementById("newEndpointsModal").checked = false
      sessionStorage.setItem('closedNewEndpointsModal', 'true')
    }

    document.addEventListener('DOMContentLoaded', function() {
       if(!sessionStorage.getItem('closedNewEndpointsModal')) {
           document.getElementById("newEndpointsModal").checked = true
        }
    })

    const picker = new easepick.create({
      element: '#startTime',
      css: [
        'https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.css',
      ],
      inline: true,
      plugins: ['RangePlugin', 'TimePlugin'],
      autoApply: false,
      setup(picker) {
        picker.on('select', (e) => {
          const start = JSON.stringify(e.detail.start).slice(1, -1);
          const end = JSON.stringify(e.detail.end).slice(1, -1);

          const url = new URL(window.location.href);
          url.searchParams.set('x', true);
          url.searchParams.set('from', start);
          url.searchParams.set('to', end);
          url.searchParams.delete('since');
          window.location.href = url.toString();
        });
      },
    });
    window.picker = picker;
    |]


dStats :: Projects.ProjectId -> Projects.ProjectRequestStats -> Text -> (Maybe UTCTime, Maybe UTCTime) -> Bool -> Html ()
dStats pid projReqStats@Projects.ProjectRequestStats{..} reqLatenciesRolledByStepsJ dateRange@(fromD, toD) hasRequest = do
  section_ [class_ "space-y-3"] do
    unless hasRequest do
      let url = Just $ "/p/" <> pid.toText <> "/integration_guides"
          subTxt = "You're currently not sending any data to APItoolkit from your backends yet."
      emptyState_ "Waiting for events..." subTxt url "Read the setup guide"

    div_ [class_ "flex justify-between mt-4"] $ div_ [class_ "flex flex-row"] do
      a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]] $
        faSprite_ "chevron-down" "regular" "h-4 w-4 mr-3 inline-block"
      span_ [class_ "text-lg text-slate-700"] "Analytics"

    div_ [class_ "reqResSubSection space-y-5"] do
      div_ [class_ "grid grid-cols-5 gap-5 overflow-x-hidden"] do
        statBox (Just pid) "Requests (14d)" "Total requests in the last 2 weeks" projReqStats.totalRequests Nothing
        statBox (Just pid) "Anomalies" "Total anomalies still active this week vs last week" projReqStats.totalAnomalies (Just projReqStats.totalAnomaliesLastWeek)
        statBox (Just pid) "Endpoints" "Total endpoints now vs last week" projReqStats.totalEndpoints (Just projReqStats.totalEndpointsLastWeek)
        statBox (Just pid) "Req Shapes" "Total request signatures which are active now vs last week" projReqStats.totalShapes (Just projReqStats.totalShapesLastWeek)
        statBox (Just pid) "Reqs per min" "Total requests per minute this week vs last week" projReqStats.requestsPerMin (Just projReqStats.requestsPerMinLastWeek)

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] do
          div_ [class_ "flex gap-4 items-center"] do
            select_ [] $ option_ [class_ "text-2xl font-normal mr-4"] "Requests by Status Code"
            span_ [class_ "inline-block", term "data-tippy-content" "HTTP status code distribution for all requests."] $ faSprite_ "circle-info" "regular" "w-4 h-4"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ C.QBPId pid : catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE]

        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] do
          div_ [class_ "flex gap-4 items-center"] do
            select_ [] $ option_ [class_ "text-2xl font-normal"] "Latency Percentiles"
            span_ [class_ "inline-block", term "data-tippy-content" "Response time distribution at the 50th, 75th, and 90th percentiles"] $ faSprite_ "circle-info" "regular" "w-4 h-4"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ C.QBPId pid : catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBDurationPercentile, C.SlotsE 120, C.ShowLegendE, C.TypeE C.LineCT]

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] do
          div_ [class_ "flex gap-4 items-center"] do
            select_ [] $ option_ [class_ "text-2xl font-normal"] "Errors"
            span_ [class_ "inline-block", term "data-tippy-content" "Requests with error status responses grouped by status code"] $ faSprite_ "circle-info" "regular" "w-4 h-4"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ [C.QBPId pid, C.QBStatusCodeGT 400] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE, C.Theme "roma"]

        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] do
          div_ [class_ "flex gap-4 items-center"] do
            select_ [] $ option_ [class_ "text-2xl font-normal"] "Requests by Endpoint"
            span_ [class_ "inline-block", term "data-tippy-content" "All requests grouped by endpoint"] $ faSprite_ "circle-info" "regular" "w-4 h-4"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ C.QBPId pid : catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBEndpoint, C.SlotsE 120, C.ShowLegendE]

      div_ [class_ "col-span-3 card-round py-3 px-6"] do
        div_ [class_ "p-4"] $ select_ [] do
          option_ "Request Latency Distribution"
          option_ "Avg Reqs per minute"
        div_ [class_ "grid grid-cols-9  gap-8 w-full"] do
          div_ [id_ "reqsLatencyHistogram", class_ "col-span-7 h-72"] ""
          div_ [class_ "col-span-2 space-y-4 "] do
            span_ [class_ "block text-right"] "Latency Percentiles"
            ul_ [class_ "space-y-1 divide-y divide-slate-100"] do
              percentileRow "max" projReqStats.max
              percentileRow "p99" projReqStats.p99
              percentileRow "p95" projReqStats.p95
              percentileRow "p90" projReqStats.p90
              percentileRow "p75" projReqStats.p75
              percentileRow "p50" projReqStats.p50
              percentileRow "min" projReqStats.min
        script_ [int|| latencyHistogram('reqsLatencyHistogram',{p50:#{p50}, p75:#{p75}, p90:#{p90}, p95:#{p95}, p99:#{p99}, max:#{max}},  #{reqLatenciesRolledByStepsJ}) |]


percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  let (d, unit) = fmtDuration p
  li_ [class_ "flex flex-row content-between justify-between"] do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block monospace"] do
      span_ [class_ "tabular-nums"] $ toHtml d
      span_ $ toHtml unit


fmtDuration :: Double -> (Text, Text)
fmtDuration d
  | d > 1000 = (fmt $ fixedF 2 (d / 1000), "s")
  | otherwise = (fmt $ fixedF 0 d, "ms")
