module Pages.Reports (
  reportsGetH,
  singleReportGetH,
  reportsLiveGetH,
  reportsPostH,
  buildReportJson',
  PerformanceReport (..),
  ReportsGet (..),
  ReportsPost (..),
  getSpanTypeStats,
  computeDurationChanges,
  EndpointStatsTuple,
  anomalyTypeCounts,
  eventsWidget,
  errorsWidget,
)
where

import Control.Lens (view, _2, _3)
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map.Lazy qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Time.Zones (loadTZFromDB, utcToLocalTimeTZ)
import Data.Vector qualified as V
import Effectful.Concurrent.Async (concurrently)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Bots.Utils qualified as BotUtils
import Pages.Charts.Charts qualified as Charts
import Pkg.Components.Widget (WidgetType (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as ET
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Utils (LoadingSize (..), LoadingType (..), checkFreeTierExceeded, faSprite_, loadingIndicatorWith_)


data PerformanceReport = PerformanceReport
  { urlPath :: Text
  , method :: Text
  , host :: Text
  , averageDuration :: Integer
  , durationDiffPct :: Double
  , requestCount :: Int
  , requestDiffPct :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data StatData = StatData
  { total :: Integer
  , change :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data SpanTypeStats = SpanTypeStats
  { spanType :: Text
  , eventCount :: Integer
  , eventChange :: Double
  , averageDuration :: Double
  , durationChange :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data DBQueryStat = DBQueryStat
  { query :: Text
  , averageDuration :: Double
  , totalEvents :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


data ReportData = ReportData
  { endpoints :: [PerformanceReport]
  , errors :: StatData
  , events :: StatData
  , spanTypeStats :: [SpanTypeStats]
  , slowDbQueries :: [DBQueryStat]
  , errorDataset :: Widget.WidgetDataset
  , eventsDataset :: Widget.WidgetDataset
  , issues :: [Issues.IssueSummary]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


-- | Shared widget definitions for chart URL generation
eventsWidget, errorsWidget :: Widget.Widget
eventsWidget = def{Widget.wType = WTTimeseries, Widget.query = Just "summarize count(*) by bin_auto(timestamp), coalesce(status_code, level)"}
errorsWidget = eventsWidget{Widget.query = Just "status_code == \"ERROR\" | summarize count(*) by bin_auto(timestamp), status_code", Widget.theme = Just "roma"}


anomalyTypeCounts :: Foldable f => (a -> Issues.IssueType) -> f a -> (Int, Int, Int, Int, Int)
anomalyTypeCounts getType =
  foldl'
    ( \(e, a, m, lp, rc) x -> case getType x of
        Issues.RuntimeException -> (e + 1, a, m, lp, rc)
        Issues.ApiChange -> (e, a + 1, m, lp, rc)
        Issues.QueryAlert -> (e, a, m + 1, lp, rc)
        Issues.LogPattern -> (e, a, m, lp + 1, rc)
        Issues.LogPatternRateChange -> (e, a, m, lp, rc + 1)
    )
    (0, 0, 0, 0, 0)


buildReportJson' :: Int -> Int -> Double -> Double -> V.Vector (Text, Int, Double, Int, Double) -> V.Vector (Text, Text, Text, Int, Double, Int, Double) -> V.Vector (Text, Int, Int) -> Charts.MetricsData -> Charts.MetricsData -> V.Vector Issues.IssueSummary -> AE.Value
buildReportJson' totalEvents totalErrors eventsChange errorsChange spanTypeStatsDiff' endpointsPerformance slowDbQueries chartEv chartErr issues =
  let spanStatsDiff = (\(t, e, chang, dur, durChange) -> AE.object ["spanType" AE..= t, "eventCount" AE..= e, "eventChange" AE..= chang, "averageDuration" AE..= dur, "durationChange" AE..= durChange]) <$> spanTypeStatsDiff'
      perf = (\(u, m, p, d, dc, req, cc) -> AE.object ["host" AE..= u, "urlPath" AE..= p, "method" AE..= m, "averageDuration" AE..= d, "durationDiffPct" AE..= dc, "requestCount" AE..= req, "requestDiffPct" AE..= cc]) <$> V.take 10 endpointsPerformance
      slowDbQueries' = (\(q, d, c) -> AE.object ["query" AE..= q, "averageDuration" AE..= d, "totalEvents" AE..= c]) <$> slowDbQueries
   in AE.object
        [ "endpoints" AE..= perf
        , "events" AE..= AE.object ["total" AE..= totalEvents, "change" AE..= eventsChange]
        , "errors" AE..= AE.object ["total" AE..= totalErrors, "change" AE..= errorsChange]
        , "spanTypeStats" AE..= spanStatsDiff
        , "slowDbQueries" AE..= slowDbQueries'
        , "errorDataset" AE..= Widget.toWidgetDataset chartErr
        , "eventsDataset" AE..= Widget.toWidgetDataset chartEv
        , "issues" AE..= issues
        ]


-- | Moved from BackgroundJobs
type EndpointStatsTuple = (Text, Text, Text, Int, Int)


getSpanTypeStats :: V.Vector (Text, Int, Int) -> V.Vector (Text, Int, Int) -> V.Vector (Text, Int, Double, Int, Double)
getSpanTypeStats current prev =
  let spanTypes = ordNub $ V.toList (V.map (\(t, _, _) -> t) current <> V.map (\(t, _, _) -> t) prev)
      getStats t =
        let evtCount = maybe 0 (\(_, c, _) -> c) (V.find (\(st, _, _) -> st == t) current)
            prevEvtCount = maybe 0 (\(_, c, _) -> c) (V.find (\(st, _, _) -> st == t) prev)
            evtChange' = if prevEvtCount == 0 then 0.00 :: Double else fromIntegral (evtCount - prevEvtCount) / fromIntegral prevEvtCount * 100
            evtChange = fromIntegral (round (evtChange' * 100)) / 100
            avgDuration = maybe 0 (\(_, _, d) -> d) (V.find (\(st, _, _) -> st == t) current)
            prevAvgDuration = maybe 0 (\(_, _, d) -> d) (V.find (\(st, _, _) -> st == t) prev)
            durationChange' = if prevAvgDuration == 0 then 0.00 :: Double else fromIntegral (avgDuration - prevAvgDuration) / fromIntegral prevAvgDuration * 100
            durationChange = fromIntegral (round (durationChange' * 100)) / 100
         in (t, evtCount, evtChange, avgDuration, durationChange)
   in V.fromList (map getStats spanTypes)


computeDurationChanges :: V.Vector EndpointStatsTuple -> V.Vector EndpointStatsTuple -> V.Vector (Text, Text, Text, Int, Double, Int, Double)
computeDurationChanges current prev =
  let prevMap :: Map.Map (Text, Text, Text) Int
      prevMap = Map.fromList [((h, m, u), dur) | (h, m, u, dur, _req) <- V.toList prev]
      prevMapReq :: Map.Map (Text, Text, Text) Int
      prevMapReq = Map.fromList [((h, m, u), req) | (h, m, u, _dur, req) <- V.toList prev]
      compute (h, m, u, dur, req) =
        let change = case Map.lookup (h, m, u) prevMap of
              Just prevDur | prevDur > 0 -> Just $ (fromIntegral (dur - prevDur) / fromIntegral prevDur) * 100
              _ -> Nothing
            reqChange = case Map.lookup (h, m, u) prevMapReq of
              Just prevReq | prevReq > 0 -> Just $ (fromIntegral (req - prevReq) / fromIntegral prevReq) * 100
              _ -> Nothing
         in (h, m, u, dur, maybe 100.00 (\x -> fromIntegral (round (x * 100)) / 100) change, req, maybe 100.00 (\x -> fromIntegral (round (x * 100)) / 100) reqChange)
   in V.map compute current


-- | Shared email rendering: builds WeeklyReportData from inputs, generates chart URLs, renders email
-- Returns (dateLabel based on endTime, rendered email HTML)
renderWeeklyEmail :: Text -> Projects.Project -> Projects.ProjectId -> Text -> UTCTime -> UTCTime -> Int -> Int -> Double -> Double -> V.Vector Issues.IssueSummary -> V.Vector (Text, Text, Text, Int, Double, Int, Double) -> V.Vector (Text, Int, Int) -> V.Vector (Text, Int64, Text) -> Bool -> ATAuthCtx (Text, Text)
renderWeeklyEmail reportUrl project pid userName startTime endTime totalEvents totalErrors eventsChangePct errorsChangePct anomalies performance slowQueries topPatterns freeTierExceeded = do
  ctx <- ask @AuthContext
  tz <- liftIO $ loadTZFromDB (toString project.timeZone)
  let reportUrl' = ctx.env.hostUrl <> reportUrl
      dayStart = show $ localDay (utcToLocalTimeTZ tz startTime)
      dayEnd = show $ localDay (utcToLocalTimeTZ tz endTime)
      stmTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" startTime
      endTxt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" endTime
      (errTotal, apiTotal, qTotal, lpTotal, rcTotal) = anomalyTypeCounts (.issueType) anomalies
  eventsUrl <- BotUtils.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid eventsWidget Nothing (Just stmTxt) (Just endTxt)
  errorsUrl <- BotUtils.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid errorsWidget Nothing (Just stmTxt) (Just endTxt)
  let projectUrl = ctx.env.hostUrl <> "p/" <> pid.toText
      reportData =
        ET.WeeklyReportData
          { userName
          , projectName = project.title
          , reportUrl = reportUrl'
          , projectUrl
          , startDate = dayStart
          , endDate = dayEnd
          , eventsChartUrl = eventsUrl
          , errorsChartUrl = errorsUrl
          , totalEvents
          , totalErrors
          , eventsChangePct
          , errorsChangePct
          , runtimeErrorsCount = errTotal
          , apiChangesCount = apiTotal
          , alertsCount = qTotal
          , logPatternCount = lpTotal
          , rateChangeCount = rcTotal
          , anomalies
          , performance
          , slowQueries
          , topPatterns
          , freeTierExceeded
          }
      (subj, html) = ET.weeklyReportEmail reportData
  pure (dayEnd, ET.renderEmail subj html)


-- | Reconstruct the email HTML from a stored Report's reportJson. Returns (dateLabel, emailHtml)
reportToEmailHtml :: Issues.Report -> Projects.Project -> Text -> ATAuthCtx (Text, Text)
reportToEmailHtml report project userName = case AE.fromJSON @ReportData report.reportJson of
  AE.Error _ -> pure ("", "Error: Could not parse report data")
  AE.Success rd -> do
    let anomalies' = V.fromList rd.issues
        performance = V.fromList $ (\ep -> (ep.host, ep.method, ep.urlPath, fromIntegral ep.averageDuration :: Int, ep.durationDiffPct, ep.requestCount, ep.requestDiffPct)) <$> rd.endpoints
        slowQueries = V.fromList $ (\q -> (q.query, round q.averageDuration :: Int, fromIntegral q.totalEvents :: Int)) <$> rd.slowDbQueries
        reportUrl = "/p/" <> project.id.toText <> "/reports/" <> report.id.toText
    renderWeeklyEmail reportUrl project project.id userName report.startTime report.endTime (fromIntegral rd.events.total) (fromIntegral rd.errors.total) rd.events.change rd.errors.change anomalies' performance slowQueries V.empty False


-- | Build live "week to date" email preview. Returns (dateLabel, emailHtml)
buildLiveReportEmailHtml :: Projects.ProjectId -> Projects.Project -> Text -> ATAuthCtx (Text, Text)
buildLiveReportEmailHtml pid project userName = do
  currentTime <- Time.currentTime
  let startTime = addUTCTime (negate $ 6 * 86400) currentTime
      prevStart = addUTCTime (negate $ 12 * 86400) currentTime
  ( (stats, statsPrev)
    , ((slowQueriesL, (endpointStats, endpointStatsPrev)), ((anomalies, _), (patterns, totalRequest)))
    ) <-
    concurrently
      ( concurrently
          (Telemetry.getProjectStatsForReport pid startTime currentTime)
          (Telemetry.getProjectStatsForReport pid prevStart startTime)
      )
      ( concurrently
          ( concurrently
              (Telemetry.getDBQueryStats pid startTime currentTime)
              ( concurrently
                  (V.fromList <$> Telemetry.getEndpointStats pid startTime currentTime)
                  (V.fromList <$> Telemetry.getEndpointStats pid prevStart startTime)
              )
          )
          ( concurrently
              (Issues.selectIssues pid Nothing (Just False) Nothing 100 0 (Just (startTime, currentTime)) Nothing "7d" [] [])
              (concurrently (LogPatterns.getLogPatterns pid 10 0) (LogQueries.getLastSevenDaysTotalRequest pid))
          )
      )
  let totalErrors = sum $ map (view _2) stats
      totalEvents = sum $ map (view _3) stats
      totalErrorsPrev = sum $ map (view _2) statsPrev
      totalEventsPrev = sum $ map (view _3) statsPrev
      pctChange cur prev = if prev == 0 then 0.0 else fromIntegral (round ((fromIntegral (cur - prev) / fromIntegral prev * 100 :: Double) * 100)) / 100
      eventsChangePct = pctChange totalEvents totalEventsPrev :: Double
      errorsChangePct = pctChange totalErrors totalErrorsPrev :: Double
      slowQueries = V.fromList slowQueriesL
      performance = computeDurationChanges endpointStats endpointStatsPrev
      anomalies' = V.fromList $ Issues.toIssueSummary <$> anomalies
      topPatterns = V.fromList $ patterns <&> \p -> (p.logPattern, p.occurrenceCount, LogPatterns.sourceFieldLabel p.sourceField)
  let reportUrl = "/p/" <> pid.toText <> "/reports"
      freeTierExceeded = project.paymentPlan == "FREE" && totalRequest > 5000
  renderWeeklyEmail reportUrl project pid userName startTime currentTime totalEvents totalErrors eventsChangePct errorsChangePct anomalies' performance slowQueries topPatterns freeTierExceeded


reportsPostH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders ReportsPost)
reportsPostH pid t = do
  _ <- Projects.sessionAndProject pid
  _ <- Projects.updateProjectReportNotif pid t
  addSuccessToast "Report notifications updated Successfully" Nothing
  addRespHeaders $ ReportsPost "updated"


newtype ReportsPost = ReportsPost Text


instance ToHtml ReportsPost where
  toHtml (ReportsPost _t) = ""
  toHtmlRaw = toHtml


wrapSingleResponse :: Projects.Session -> Projects.Project -> Bool -> EnvConfig -> Text -> Maybe Text -> (Text, Text, Text) -> ATAuthCtx (RespHeaders ReportsGet)
wrapSingleResponse sess project freeTierExceeded config pageTitle hxRequestM content = case hxRequestM of
  Just _ -> addRespHeaders $ ReportsGetSingle' content
  _ -> do
    let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle, freeTierExceeded, config}
    addRespHeaders $ ReportsGetSingle $ PageCtx bwconf content


singleReportGetH :: Projects.ProjectId -> Issues.ReportId -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
singleReportGetH pid rid hxRequestM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  reportM <- Issues.getReportById rid
  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan
  content <- case reportM of
    Nothing -> pure ("unknown", "Report not found", "")
    Just report -> do
      (dateLabel, emailHtml) <- reportToEmailHtml report project sess.user.firstName
      pure (report.reportType, dateLabel, emailHtml)
  wrapSingleResponse sess project freeTierExceeded appCtx.env "Report" hxRequestM content


reportsLiveGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsLiveGetH pid hxRequestM = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  (dateLabel, emailHtml) <- buildLiveReportEmailHtml pid project sess.user.firstName
  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan
  let content = ("weekly" :: Text, dateLabel, emailHtml)
  wrapSingleResponse sess project freeTierExceeded appCtx.env "Reports" hxRequestM content


reportsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsGetH pid page hxRequest hxBoosted = do
  (sess, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let p = toString (fromMaybe "0" page)
  let pg = fromMaybe 0 (readMaybe p :: Maybe Int)

  reportsList <- Issues.reportHistoryByProject pid pg
  let reports = V.fromList reportsList
  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan
  let nextUrl =
        if V.length reports < 20
          then Nothing
          else Just $ "/p/" <> pid.toText <> "/reports?page=" <> show (pg + 1)
  case (hxRequest, hxBoosted) of
    (Just "true", Nothing) -> addRespHeaders $ ReportsGetList pid reports nextUrl
    _ -> do
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = Just project
              , pageTitle = "Reports"
              , freeTierExceeded = freeTierExceeded
              , config = appCtx.env
              }
      addRespHeaders $ ReportsGetMain $ PageCtx bwconf (pid, reports, nextUrl, project.dailyNotif, project.weeklyNotif)


-- | (reportType, dateLabel, emailHtml)
data ReportsGet
  = ReportsGetMain (PageCtx (Projects.ProjectId, V.Vector Issues.ReportListItem, Maybe Text, Bool, Bool))
  | ReportsGetList Projects.ProjectId (V.Vector Issues.ReportListItem) (Maybe Text)
  | ReportsGetSingle (PageCtx (Text, Text, Text))
  | ReportsGetSingle' (Text, Text, Text)


instance ToHtml ReportsGet where
  toHtml (ReportsGetMain (PageCtx conf (pid, reports, next, daily, weekly))) = toHtml $ PageCtx conf $ reportsPage pid reports next daily weekly
  toHtml (ReportsGetList pid reports next) = toHtml $ reportListItems pid reports next
  toHtml (ReportsGetSingle (PageCtx conf content)) = toHtml $ PageCtx conf $ singleReportPage content
  toHtml (ReportsGetSingle' content) = toHtml $ singleReportPage content
  toHtmlRaw = toHtml


singleReportPage :: (Text, Text, Text) -> Html ()
singleReportPage (reportType, dateLabel, emailHtml) =
  div_ [class_ "w-full flex flex-col h-full"] do
    div_ [class_ "flex w-full justify-between items-center border-b p-4"] do
      h3_ [class_ "text-textStrong font-medium capitalize"] $ toHtml reportType <> " report"
      span_ [class_ "text-sm text-textWeak"] $ toHtml dateLabel
    if T.null emailHtml
      then h3_ [class_ "p-4"] "Report Not Found"
      else iframe_ [term "srcdoc" emailHtml, style_ "width:100%;height:100%;border:none;", term "sandbox" "allow-same-origin"] ""


reportsPage :: Projects.ProjectId -> V.Vector Issues.ReportListItem -> Maybe Text -> Bool -> Bool -> Html ()
reportsPage pid reports nextUrl _daily _weekly =
  div_ [class_ "flex flex-col md:flex-row h-full w-full border-t"] do
    when (V.null reports)
      $ div_ [class_ "flex h-full w-full justify-center items-center"]
      $ div_ [class_ "flex flex-col items-center justify-center py-16 px-4"]
      $ do
        div_ [class_ "rounded-full flex items-center justify-center mb-3"]
          $ faSprite_ "empty" "regular" "h-12 w-12"
        h3_ [class_ "text-xl text-textStrong mb-2"] "No reports generated for this project yet."
    unless (V.null reports) do
      div_ [class_ "w-full md:w-1/3 md:border-r border-b md:border-b-0 border-strokeWeak p-4 overflow-x-auto md:overflow-y-auto"] do
        div_ [class_ "mt-4 flex flex-row md:flex-col gap-4 w-full"] do
          -- "Week to Date" live preview entry
          div_ [class_ "shrink-0 w-64 md:w-full flex flex-col border border-strokeBrand-weak bg-fillBrand-weak/10 rounded-lg cursor-pointer hover:bg-fillWeaker"] do
            div_ [class_ "w-full"] do
              a_
                [ class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"
                , hxGet_ $ "/p/" <> pid.toText <> "/reports/live"
                , hxTarget_ "#detailSidebar"
                , hxSwap_ "innerHTML"
                ]
                $ div_ [class_ "flex flex-col grow gap-4"] do
                  div_ [class_ "flex items-center w-full justify-between gap-2"] do
                    div_ [class_ "flex items-center gap-2"] do
                      div_ [class_ "bg-fillBrand-weak text-xs font-medium px-2.5 py-1 rounded-full"] "Weekly report"
                      span_ [class_ "bg-fillSuccess-strong text-white text-[10px] font-bold px-1.5 py-0.5 rounded-full uppercase"] "Live"
                    faSprite_ "chevron-right" "regular" "w-3 h-3"
                  h4_ [class_ "font-medium text-sm flex items-center gap-2"] do
                    faSprite_ "calendar" "regular" "w-4 h-4"
                    "Week to Date"
          -- Historical reports
          reportListItems pid reports nextUrl
      div_ [class_ "w-full md:w-2/3 overflow-y-auto"] do
        div_ [class_ "flex h-full", id_ "detailSidebar"] do
          -- Auto-load the live preview on initial page load
          a_
            [ class_ "w-full text-center cursor-pointer"
            , hxGet_ $ "/p/" <> pid.toText <> "/reports/live"
            , hxTarget_ "#detailSidebar"
            , hxSwap_ "innerHTML"
            , hxTrigger_ "intersect once"
            ]
            $ div_ [class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"] do
              loadingIndicatorWith_ LdSM LdDots "text-textWeak"


reportListItems :: Projects.ProjectId -> V.Vector Issues.ReportListItem -> Maybe Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "flex flex-row md:flex-col gap-4 w-full"] do
    forM_ reports $ \report -> do
      let isWeeklyData = report.reportType == "weekly"
      div_ [class_ "shrink-0 w-64 md:w-full flex flex-col border border-strokeWeak rounded-lg cursor-pointer hover:bg-fillWeaker"] do
        div_ [class_ "w-full"] do
          a_
            [ class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"
            , hxGet_ $ "/p/" <> pid.toText <> "/reports/" <> report.id.toText
            , hxTarget_ "#detailSidebar"
            , hxSwap_ "innerHTML"
            ]
            $ div_ [class_ "flex flex-col grow gap-4"] do
              div_ [class_ "flex items-center w-full justify-between gap-2"] do
                div_ [class_ $ (if isWeeklyData then "bg-fillBrand-weak" else "bg-fillWeak") <> " text-xs font-medium px-2.5 py-1 rounded-full capitalize"] $ toHtml report.reportType <> " report"
                faSprite_ "chevron-right" "regular" "w-3 h-3"
              h4_ [class_ "font-medium text-sm flex items-center gap-2"] do
                faSprite_ "calendar" "regular" "w-4 h-4"
                toHtml $ formatTime defaultTimeLocale "%a, %b %d %Y" (zonedTimeToLocalTime report.createdAt)
    whenJust nextUrl \url ->
      a_ [class_ "w-full cursor-pointer block p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center mb-4", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] "LOAD MORE"
