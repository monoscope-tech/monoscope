module Pages.Dashboard (dashboardGetH) where

import Config
import Data.Aeson qualified as AE
import Data.Default ( def, def )
import Data.Time
    ( UTCTime,
      ZonedTime,
      addUTCTime,
      formatTime,
      getCurrentTime,
      secondsToNominalDiffTime,
      utc,
      utcToZonedTime,
      UTCTime,
      ZonedTime,
      addUTCTime,
      formatTime,
      getCurrentTime,
      secondsToNominalDiffTime,
      utc,
      utcToZonedTime )
import Data.Time.Format ( defaultTimeLocale, defaultTimeLocale )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Fmt
import Lucid
import Lucid.Hyperscript ( __, __ )
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.Anomalies.AnomalyList qualified as AnomaliesList
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Servant
    ( Union,
      WithStatus(..),
      respond )
import Utils ( GetOrRedirect, redirect, deleteParam, mIcon_ )
import Pages.Components (statBox)
import Pages.Charts.Charts qualified as C
import Relude hiding (max, min)
import System.Clock
import Text.Interpolation.Nyan
import Witch (from)

timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour")
  , ("24H", "Last 24 Hours")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]

data ParamInput = ParamInput
  { currentURL :: Text
  , sinceStr :: Maybe Text
  , dateRange :: (Maybe ZonedTime, Maybe ZonedTime)
  , currentPickerTxt :: Text
  }

dashboardGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Union GetOrRedirect)
dashboardGetH sess pid fromDStr toDStr sinceStr' = do
  pool <- asks pool
  now <- liftIO getCurrentTime
  let sinceStr = if isNothing fromDStr && isNothing toDStr && isNothing sinceStr' || fromDStr == Just "" then Just "14D" else sinceStr'

  (hasApikeys, hasRequest) <- liftIO $
    withPool pool $ do
      apiKeys <- ProjectApiKeys.countProjectApiKeysByProjectId pid
      requestDumps <- RequestDumps.countRequestDumpByProject pid
      pure (apiKeys > 0, requestDumps > 0)
  -- TODO: Replace with a duration parser.
  let (fromD, toD) = case sinceStr of
        Just "1H" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just $ utcToZonedTime utc now)
        Just "24H" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just $ utcToZonedTime utc now)
        Just "7D" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 7) now, Just $ utcToZonedTime utc now)
        Just "14D" -> (Just $ utcToZonedTime utc $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 14) now, Just $ utcToZonedTime utc now)
        Nothing -> do
          let f = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
          let t = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
          (f, t)
        _ -> do
          let f = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" fromDStr) :: Maybe UTCTime)
          let t = utcToZonedTime utc <$> (iso8601ParseM (from @Text $ fromMaybe "" toDStr) :: Maybe UTCTime)
          (f, t)

  startTime <- liftIO $ getTime Monotonic
  (project, projectRequestStats, reqLatenciesRolledByStepsLabeled) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)

      projectRequestStats <- fromMaybe (def :: Projects.ProjectRequestStats) <$> Projects.projectRequestStatsByProject pid
      let maxV = round (projectRequestStats.p99) :: Int
      let steps' = (maxV `quot` 100) :: Int
      let steps = if steps' == 0 then 100 else steps'
      reqLatenciesRolledBySteps <- RequestDumps.selectReqLatenciesRolledByStepsForProject maxV steps pid (fromD, toD)
      pure (project, projectRequestStats, Vector.toList reqLatenciesRolledBySteps)

  let reqLatenciesRolledByStepsJ = decodeUtf8 $ AE.encode reqLatenciesRolledByStepsLabeled
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Dashboard"
          }
  currTime <- liftIO getCurrentTime
  let currentURL = "/p/" <> pid.toText <> "?&from=" <> fromMaybe "" fromDStr <> "&to=" <> fromMaybe "" toDStr
  let currentPickerTxt = fromMaybe (maybe "" (toText . formatTime defaultTimeLocale "%F %T") fromD <> " - " <> maybe "" (toText . formatTime defaultTimeLocale "%F %T") toD) sinceStr
  let paramInput = ParamInput{currentURL = currentURL, sinceStr = sinceStr, dateRange = (fromD, toD), currentPickerTxt = currentPickerTxt}
  if not hasApikeys || not hasRequest
    then respond $ WithStatus @302 $ redirect ("/p/" <> pid.toText <> "/onboarding")
    else respond $ WithStatus @200 $ bodyWrapper bwconf $ dashboardPage pid paramInput currTime projectRequestStats reqLatenciesRolledByStepsJ (fromD, toD)

dashboardPage :: Projects.ProjectId -> ParamInput -> UTCTime -> Projects.ProjectRequestStats -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
dashboardPage pid paramInput currTime projectStats reqLatenciesRolledByStepsJ dateRange = do
  let currentURL' = deleteParam "to" $ deleteParam "from" $ deleteParam "since" paramInput.currentURL
  section_ [class_ "p-8 container mx-auto px-4 space-y-12 pb-24"] $ do
    div_ [class_ "relative p-1 "] do
      div_ [class_ "relative"] do
        a_
          [ class_ "relative px-3 py-2 border border-1 border-black-200 space-x-2  inline-block relative cursor-pointer rounded-md"
          , [__| on click toggle .hidden on #timepickerBox|]
          ]
          do
            mIcon_ "clock" "h-4 w-4"
            span_ [class_ "inline-block"] $ toHtml paramInput.currentPickerTxt
            img_
              [ src_ "/assets/svgs/cheveron-down.svg"
              , class_ "h-4 w-4 inline-block"
              ]
        div_ [id_ "timepickerBox", class_ "hidden absolute z-10 mt-1  rounded-md flex"] do
          div_ [class_ "inline-block w-84 overflow-auto bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"] do
            timePickerItems
              & mapM_ \(val, title) ->
                a_
                  [ class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 "
                  , href_ $ currentURL' <> "&since=" <> val
                  ]
                  $ toHtml title
            a_ [class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ", [__| on click toggle .hidden on #timepickerSidebar |]] "Custom date range"
          div_ [class_ "inline-block relative hidden", id_ "timepickerSidebar"] do
            div_ [id_ "startTime", class_ "hidden"] ""

    -- button_ [class_ "", id_ "checkin", onclick_ "window.picker.show()"] "timepicker"
    section_ $ AnomaliesList.anomalyListSlider currTime (projectStats.projectId) Nothing Nothing
    dStats pid projectStats reqLatenciesRolledByStepsJ dateRange
  script_
    [text|
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

dStats :: Projects.ProjectId -> Projects.ProjectRequestStats -> Text -> (Maybe ZonedTime, Maybe ZonedTime) -> Html ()
dStats pid projReqStats@Projects.ProjectRequestStats{..} reqLatenciesRolledByStepsJ dateRange@(fromD, toD) = do
  let _ = min
  when (projReqStats.totalRequests == 0) do
    section_ [class_ "card-round p-5 sm:p-10 space-y-4 text-lg"] $ do
      h2_ [class_ "text-2xl"] "Welcome onboard APIToolkit."
      p_ "You're currently not sending any data to apitoolkit from your backends yet. Here's a guide to get you setup for your tech stack."
      a_ [href_ "https://apitoolkit.io/docs/quickstarts/", class_ "btn-indigo btn-sm my-3 -ml-0 mt-6", target_ "_blank"] "Read the setup guide"

  section_ [class_ "space-y-3"] $ do
    div_ [class_ "flex justify-between mt-4"] $ div_ [class_ "flex flex-row"] $ do
      a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]] $
        img_
          [ src_ "/assets/svgs/cheveron-down.svg"
          , class_ "h-4 mr-3 mt-1 w-4"
          ]
      span_ [class_ "text-lg text-slate-700"] "Stats"

    div_ [class_ "reqResSubSection space-y-5"] $ do
      div_ [class_ "grid grid-cols-5 gap-5"] $ do
        statBox "Requests" "Total requests in the last 2 weeks" (projReqStats.totalRequests) Nothing
        statBox "Anomalies" "Total anomalies still active this week vs last week" projReqStats.totalAnomalies (Just projReqStats.totalAnomaliesLastWeek)
        statBox "Endpoints" "Total endpoints now vs last week" projReqStats.totalEndpoints (Just projReqStats.totalEndpointsLastWeek)
        statBox "Signatures" "Total request signatures which are active now vs last week" projReqStats.totalShapes (Just projReqStats.totalShapesLastWeek)
        statBox "Fields" "Total fields which are active now vs last week" projReqStats.totalFields (Just projReqStats.totalFieldsLastWeek)

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] $ do
          select_ [] $ option_ [class_ "text-2xl font-normal"] "Throughput by Status Code"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ C.QBPId pid : catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode, C.SlotsE 120, C.ShowLegendE]

        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] $ do
          select_ [] $ option_ [class_ "text-2xl font-normal"] "Latency Percentiles"
          div_ [class_ "h-64 "] do
            Charts.latency pid "reqsLatencyPercentiles" Nothing 120 dateRange Nothing

      div_ [class_ "flex gap-5"] do
        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] $ do
          select_ [] $ option_ [class_ "text-2xl font-normal"] "Error Rates"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ [C.QBPId pid, C.QBStatusCodeGT 400] ++ catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBStatusCode , C.SlotsE 120, C.ShowLegendE, C.Theme "roma"]

        div_ [class_ "flex-1 card-round p-3"] $ div_ [class_ "p-4 space-y-6"] $ do
          select_ [] $ option_ [class_ "text-2xl font-normal"] "Reqs Grouped by Endpoint"
          div_ [class_ "h-64 "] do
            Charts.lazy [C.QByE $ C.QBPId pid : catMaybes [C.QBFrom <$> fromD, C.QBTo <$> toD], C.GByE C.GBEndpoint, C.SlotsE 120, C.ShowLegendE]

      div_ [class_ "col-span-3 card-round py-3 px-6"] $ do
        div_ [class_ "p-4"] $ select_ [] $ do
          option_ "Request Latency Distribution"
          option_ "Avg Reqs per minute"
        div_ [class_ "grid grid-cols-9  gap-8 w-full"] $ do
          div_ [id_ "reqsLatencyHistogram", class_ "col-span-7 h-72"] ""
          div_ [class_ "col-span-2 space-y-4 "] $ do
            span_ [class_ "block text-right"] "Latency Percentiles"
            ul_ [class_ "space-y-1 divide-y divide-slate-100"] $ do
              percentileRow "max" $ projReqStats.max
              percentileRow "p99" $ projReqStats.p99
              percentileRow "p95" $ projReqStats.p95
              percentileRow "p90" $ projReqStats.p90
              percentileRow "p75" $ projReqStats.p75
              percentileRow "p50" $ projReqStats.p50
              percentileRow "min" $ projReqStats.min
        script_ [int|| latencyHistogram('reqsLatencyHistogram',{p50:#{p50}, p75:#{p75}, p90:#{p90}, p95:#{p95}, p99:#{p99}, max:#{max}},  #{reqLatenciesRolledByStepsJ}) |]

percentileRow :: Text -> Double -> Html ()
percentileRow key p = do
  let (d, unit) = fmtDuration p
  li_ [class_ "flex flex-row content-between justify-between"] $ do
    span_ [class_ "inline-block"] $ toHtml key
    span_ [class_ "inline-block font-mono"] $ do
      span_ [class_ "tabular-nums"] $ toHtml d
      span_ $ toHtml unit

fmtDuration :: Double -> (Text, Text)
fmtDuration d
  | d > 1000 = (fmt $ fixedF 2 (d / 1000), "s")
  | otherwise = (fmt $ fixedF 0 d, "ms")
