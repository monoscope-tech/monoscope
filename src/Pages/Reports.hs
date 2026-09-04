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
  pctChange,
  eventsWidget,
  errorsWidget,
  renderWeeklyEmail,
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map.Lazy qualified as Map
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Time.Zones (utcTZ, utcToLocalTimeTZ)
import Data.Time.Zones.All qualified as TZ
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import Effectful.Concurrent.Async (concurrently)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Charts.Charts qualified as Charts
import Pages.Components (EmptyStateCfg (..), emptyState_)
import Pkg.Components.Widget (WidgetType (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as ET
import Relude hiding (Reader, ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Utils (FreeTierStatus, LoadingSize (..), LoadingType (..), checkFreeTierStatus, faSprite_, formatUTCMicros, hostPath, loadingIndicatorWith_)


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
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SpanTypeStats = SpanTypeStats
  { spanType :: Text
  , eventCount :: Integer
  , eventChange :: Double
  , averageDuration :: Double
  , durationChange :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data DBQueryStat = DBQueryStat
  { query :: Text
  , averageDuration :: Double
  , totalEvents :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


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
  deriving anyclass (AE.FromJSON, AE.ToJSON)


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


buildReportJson' :: Int -> Int -> Double -> Double -> V.Vector (Text, Int, Double, Int, Double) -> V.Vector (Text, Text, Text, Int64, Double, Int64, Double) -> V.Vector (Text, Int, Int) -> Charts.MetricsData -> Charts.MetricsData -> V.Vector Issues.IssueSummary -> AE.Value
buildReportJson' totalEvents totalErrors eventsChange errorsChange spanTypeStatsDiff' endpointsPerformance slowDbQueries chartEv chartErr issues =
  AE.toJSON
    ReportData
      { endpoints = (\(u, m, p, d, dc, req, cc) -> PerformanceReport{host = u, urlPath = p, method = m, averageDuration = fromIntegral d, durationDiffPct = dc, requestCount = fromIntegral req, requestDiffPct = cc}) <$> V.toList (V.take 10 endpointsPerformance)
      , events = StatData{total = fromIntegral totalEvents, change = eventsChange}
      , errors = StatData{total = fromIntegral totalErrors, change = errorsChange}
      , spanTypeStats = (\(t, e, chang, dur, durChange) -> SpanTypeStats{spanType = t, eventCount = fromIntegral e, eventChange = chang, averageDuration = fromIntegral dur, durationChange = durChange}) <$> V.toList spanTypeStatsDiff'
      , slowDbQueries = (\(q, d, c) -> DBQueryStat{query = q, averageDuration = fromIntegral d, totalEvents = fromIntegral c}) <$> V.toList slowDbQueries
      , errorDataset = Widget.toWidgetDataset chartErr
      , eventsDataset = Widget.toWidgetDataset chartEv
      , issues = V.toList issues
      }


type EndpointStatsTuple = (Text, Text, Text, Int64, Int64)


-- | Percent change of @cur@ over @prev@, rounded to 2dp; 0 when there's no baseline.
pctChange :: Integral a => a -> a -> Double
pctChange cur prev
  | prev == 0 = 0
  | otherwise = fromIntegral (round (ratio * 10000) :: Int) / 100
  where
    ratio = fromIntegral (cur - prev) / fromIntegral prev :: Double


getSpanTypeStats :: V.Vector (Text, Int, Int) -> V.Vector (Text, Int, Int) -> V.Vector (Text, Int, Double, Int, Double)
getSpanTypeStats current prev =
  V.fromList
    [ (t, c, pctChange c pc, d, pctChange d pd)
    | t <- ordNub [st | (st, _, _) <- V.toList current <> V.toList prev]
    , let (c, d) = Map.findWithDefault (0, 0) t curMap
    , let (pc, pd) = Map.findWithDefault (0, 0) t prevMap
    ]
  where
    -- first row wins on duplicate span types, matching the pre-map lookup
    toMap v = Map.fromListWith (\_ old -> old) [(t, (c, d)) | (t, c, d) <- V.toList v]
    curMap = toMap current
    prevMap = toMap prev


computeDurationChanges :: V.Vector EndpointStatsTuple -> V.Vector EndpointStatsTuple -> V.Vector (Text, Text, Text, Int64, Double, Int64, Double)
computeDurationChanges current prev = V.map compute current
  where
    prevMap :: Map.Map (Text, Text, Text) (Int64, Int64)
    prevMap = Map.fromList [((h, m, u), (dur, req)) | (h, m, u, dur, req) <- V.toList prev]
    -- no positive baseline reads as "all new": 100%
    change cur = maybe 100 (\p -> if p > 0 then pctChange cur p else 100)
    compute (h, m, u, dur, req) =
      let pv = Map.lookup (h, m, u) prevMap
       in (h, m, u, dur, change dur (fst <$> pv), req, change req (snd <$> pv))


-- $setup
-- Relude hides partial @read@, so the examples below cannot build a 'UTCTime' from a
-- string literal. @marchUTC d@ is 20:00 UTC on 2026-03-0/d/.
--
-- >>> import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
-- >>> :set -XOverloadedStrings
-- >>> let marchUTC d = UTCTime (fromGregorian 2026 3 d) (secondsToDiffTime 72000)


-- | The report window's start and end day, rendered in the *project's* timezone rather
-- than the server's. An unknown or empty zone falls back to UTC.
--
-- Extracted from 'renderWeeklyEmail' so the invariant can be pinned: the same UTC instant
-- must label differently either side of a zone's midnight, or a customer in Auckland gets
-- an email dated a day behind the week it covers.
--
-- >>> reportDayLabels "Pacific/Auckland" (marchUTC 1) (marchUTC 8)
-- ("2026-03-02","2026-03-09")
--
-- >>> reportDayLabels "UTC" (marchUTC 1) (marchUTC 8)
-- ("2026-03-01","2026-03-08")
--
-- Empty and unrecognised zones both fall back to UTC rather than failing:
--
-- >>> map (\z -> fst $ reportDayLabels z (marchUTC 1) (marchUTC 8)) ["", "Not/AZone"]
-- ["2026-03-01","2026-03-01"]
reportDayLabels :: Text -> UTCTime -> UTCTime -> (Text, Text)
reportDayLabels zone startTime endTime =
  let tz = fromMaybe utcTZ $ TZ.tzByName $ encodeUtf8 $ if T.null zone then "UTC" else zone
      label = show . localDay . utcToLocalTimeTZ tz
   in (label startTime, label endTime)


-- | Shared email rendering: builds WeeklyReportData from inputs, generates chart URLs, renders email.
-- Returns @(dateLabel from endTime, subject, rendered email HTML)@. Generalised over the effect
-- row so both the request handlers and the weekly-report background job render the same email;
-- @reportUrl@ is host-relative.
renderWeeklyEmail :: (Log :> es, Reader AuthContext :> es) => Text -> Projects.Project -> Text -> UTCTime -> UTCTime -> Int -> Int -> Double -> Double -> V.Vector Issues.IssueSummary -> V.Vector (Text, Text, Text, Int64, Double, Int64, Double) -> V.Vector (Text, Int, Int) -> V.Vector (Text, Int64, Text) -> Bool -> Eff es (Text, Text, Text)
renderWeeklyEmail reportUrl project userName startTime endTime totalEvents totalErrors eventsChangePct errorsChangePct anomalies performance slowQueries topPatterns freeTierExceeded = do
  ctx <- ask @AuthContext
  let pid = project.id
      reportUrl' = hostPath ctx.env.hostUrl reportUrl
      (dayStart, dayEnd) = reportDayLabels project.timeZone startTime endTime
      stmTxt = formatUTCMicros startTime
      endTxt = formatUTCMicros endTime
      (errTotal, apiTotal, qTotal, lpTotal, rcTotal) = anomalyTypeCounts (.issueType) anomalies
  eventsUrl <- Widget.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid eventsWidget Nothing (Just stmTxt) (Just endTxt)
  errorsUrl <- Widget.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid errorsWidget Nothing (Just stmTxt) (Just endTxt)
  let projectUrl = hostPath ctx.env.hostUrl ("p/" <> pid.toText)
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
  pure (dayEnd, subj, ET.renderEmail subj html)


-- | Reconstruct the email HTML from a stored Report's reportJson. Returns (dateLabel, emailHtml)
reportToEmailHtml :: Issues.Report -> Projects.Project -> Text -> ATAuthCtx (Text, Text)
reportToEmailHtml report project userName = case AE.fromJSON @ReportData report.reportJson of
  AE.Error err -> ("", "Error: Could not parse report data") <$ Log.logAttention "Unparseable stored report json" (report.id.toText, toText err)
  AE.Success rd -> do
    let anomalies' = V.fromList rd.issues
        performance = V.fromList $ (\ep -> (ep.host, ep.method, ep.urlPath, fromIntegral ep.averageDuration :: Int64, ep.durationDiffPct, fromIntegral ep.requestCount :: Int64, ep.requestDiffPct)) <$> rd.endpoints
        slowQueries = V.fromList $ (\q -> (q.query, round q.averageDuration :: Int, fromIntegral q.totalEvents :: Int)) <$> rd.slowDbQueries
        reportUrl = "/p/" <> project.id.toText <> "/reports/" <> report.id.toText
    dropSubject <$> renderWeeklyEmail reportUrl project userName report.startTime report.endTime (fromIntegral rd.events.total) (fromIntegral rd.errors.total) rd.events.change rd.errors.change anomalies' performance slowQueries V.empty False


-- | The page views show the email body; only the background job needs its subject line.
dropSubject :: (Text, Text, Text) -> (Text, Text)
dropSubject (dateLabel, _, html) = (dateLabel, html)


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
              (Issues.selectIssues pid Issues.PIssueL Issues.defIssueFilters{Issues.ack = Issues.IsNull, Issues.timeRange = Just (startTime, currentTime), Issues.limit = 100})
              (concurrently (LogPatterns.getLogPatterns pid 10 0) (LogQueries.getLastSevenDaysTotalRequest pid))
          )
      )
  let totals xs = (sum [e | (_, e, _) <- xs], sum [v | (_, _, v) <- xs])
      (totalErrors, totalEvents) = totals stats
      (totalErrorsPrev, totalEventsPrev) = totals statsPrev
      eventsChangePct = pctChange totalEvents totalEventsPrev
      errorsChangePct = pctChange totalErrors totalErrorsPrev
      slowQueries = V.fromList slowQueriesL
      performance = computeDurationChanges endpointStats endpointStatsPrev
      anomalies' = V.fromList $ Issues.toIssueSummary <$> anomalies
      topPatterns = V.fromList $ patterns <&> \p -> (p.logPattern, p.occurrenceCount, LogPatterns.sourceFieldLabel p.sourceField)
      reportUrl = "/p/" <> pid.toText <> "/reports"
      freeTierExceeded = Projects.isFreeTier project.paymentPlan && totalRequest > 5000
  dropSubject <$> renderWeeklyEmail reportUrl project userName startTime currentTime totalEvents totalErrors eventsChangePct errorsChangePct anomalies' performance slowQueries topPatterns freeTierExceeded


reportsPostH :: Projects.ProjectId -> Projects.ReportType -> ATAuthCtx (RespHeaders ReportsPost)
reportsPostH pid t = do
  _ <- Projects.sessionAndProject pid
  _ <- Projects.updateProjectReportNotif pid t
  addSuccessToast "Report notifications updated Successfully" Nothing
  addRespHeaders $ ReportsPost "updated"


newtype ReportsPost = ReportsPost Text


instance ToHtml ReportsPost where
  toHtml (ReportsPost _t) = ""
  toHtmlRaw = toHtml


wrapSingleResponse :: BWConfig -> FreeTierStatus -> Text -> Maybe Text -> (Text, Text, Text) -> ATAuthCtx (RespHeaders ReportsGet)
wrapSingleResponse bw freeTierStatus pageTitle hxRequestM content =
  addRespHeaders $ maybe (ReportsGetSingle $ PageCtx bw{pageTitle, freeTierStatus} content) (const $ ReportsGetSingle' content) hxRequestM


singleReportGetH :: Projects.ProjectId -> Issues.ReportId -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
singleReportGetH pid rid hxRequestM = do
  (sess, project, bw) <- mkPageCtx pid
  reportM <- Issues.getReportById pid rid
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  content <- case reportM of
    Nothing -> pure ("unknown", "Report not found", "")
    Just report -> do
      (dateLabel, emailHtml) <- reportToEmailHtml report project sess.user.firstName
      pure (display report.reportType, dateLabel, emailHtml)
  wrapSingleResponse bw freeTierStatus "Report" hxRequestM content


reportsLiveGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsLiveGetH pid hxRequestM = do
  (sess, project, bw) <- mkPageCtx pid
  (dateLabel, emailHtml) <- buildLiveReportEmailHtml pid project sess.user.firstName
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  let content = ("weekly" :: Text, dateLabel, emailHtml)
  wrapSingleResponse bw freeTierStatus "Reports" hxRequestM content


reportsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsGetH pid page hxRequest hxBoosted = do
  (_, project, bw) <- mkPageCtx pid
  let pg = fromMaybe 0 (readMaybe . toString =<< page) :: Int
  reports <- V.fromList <$> Issues.reportHistoryByProject pid pg
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  let nextUrl =
        if V.length reports < 20
          then Nothing
          else Just $ "/p/" <> pid.toText <> "/reports?page=" <> show (pg + 1)
  case (hxRequest, hxBoosted) of
    (Just "true", Nothing) -> addRespHeaders $ ReportsGetList pid reports nextUrl
    _ -> addRespHeaders $ ReportsGetMain $ PageCtx bw{pageTitle = "Reports", freeTierStatus} (pid, reports, nextUrl)


data ReportsGet
  = ReportsGetMain (PageCtx (Projects.ProjectId, V.Vector Issues.ReportListItem, Maybe Text))
  | ReportsGetList Projects.ProjectId (V.Vector Issues.ReportListItem) (Maybe Text)
  | ReportsGetSingle (PageCtx (Text, Text, Text))
  | ReportsGetSingle' (Text, Text, Text)


instance ToHtml ReportsGet where
  toHtml (ReportsGetMain (PageCtx conf (pid, reports, next))) = toHtml $ PageCtx conf $ reportsPage pid reports next
  toHtml (ReportsGetList pid reports next) = toHtml $ reportListItems pid reports next
  toHtml (ReportsGetSingle (PageCtx conf content)) = toHtml $ PageCtx conf $ singleReportPage content
  toHtml (ReportsGetSingle' content) = toHtml $ singleReportPage content
  toHtmlRaw = toHtml


-- | (reportType, dateLabel, emailHtml)
singleReportPage :: (Text, Text, Text) -> Html ()
singleReportPage (reportType, dateLabel, emailHtml) =
  div_ [class_ "w-full flex flex-col h-full"] do
    div_ [class_ "flex w-full justify-between items-center border-b p-4"] do
      h3_ [class_ "text-textStrong font-medium capitalize"] $ toHtml reportType <> " report"
      span_ [class_ "text-sm text-textWeak"] $ toHtml dateLabel
    if T.null emailHtml
      then h3_ [class_ "p-4"] "Report Not Found"
      else iframe_ [term "srcdoc" emailHtml, style_ "width:100%;height:100%;border:none;", term "sandbox" "allow-same-origin"] ""


-- | HTMX attrs loading @url@ into the report detail pane.
detailPaneAttrs :: Text -> [Attribute]
detailPaneAttrs url = [hxGet_ url, hxTarget_ "#detailSidebar", hxSwap_ "innerHTML"]


-- | Clickable report card: @borderCls@ distinguishes the live entry from history.
reportCard_ :: Text -> Text -> Html () -> Html ()
reportCard_ borderCls url body =
  div_ [class_ $ "shrink-0 w-64 md:w-full flex flex-col border rounded-lg hover:bg-fillWeaker " <> borderCls]
    $ a_ (class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer" : detailPaneAttrs url)
    $ div_ [class_ "flex flex-col grow gap-4"] body


-- | Card header row: a pill, optional trailing badge, and the chevron affordance.
reportCardHead_ :: Text -> Html () -> Html () -> Html ()
reportCardHead_ pillCls pill badge = div_ [class_ "flex items-center w-full justify-between gap-2"] do
  div_ [class_ "flex items-center gap-2"] do
    div_ [class_ $ pillCls <> " text-xs font-medium px-2.5 py-1 rounded-full"] pill
    badge
  faSprite_ "chevron-right" "regular" "w-3 h-3"


reportCardTitle_ :: Html () -> Html ()
reportCardTitle_ title = h4_ [class_ "font-medium text-sm flex items-center gap-2"] do
  faSprite_ "calendar" "regular" "w-4 h-4"
  title


reportsPage :: Projects.ProjectId -> V.Vector Issues.ReportListItem -> Maybe Text -> Html ()
reportsPage pid reports nextUrl =
  div_ [class_ "flex flex-col md:flex-row h-full w-full border-t"] do
    if V.null reports
      then
        div_ [class_ "flex h-full w-full justify-center items-center"]
          $ emptyState_ def{icon = Just "empty"} "No reports generated yet" "Scheduled digests will appear here once your project has activity to summarize."
      else do
        div_ [class_ "w-full md:w-1/3 md:border-r border-b md:border-b-0 border-strokeWeak p-4 overflow-x-auto md:overflow-y-auto"]
          $ div_ [class_ "mt-4 flex flex-row md:flex-col gap-4 w-full"] do
            reportCard_ "border-strokeBrand-weak bg-fillBrand-weak/10" ("/p/" <> pid.toText <> "/reports/live") do
              reportCardHead_ "bg-fillBrand-weak" "Weekly report"
                $ span_ [class_ "bg-fillSuccess-strong text-textInverse-strong text-2xs font-bold px-1.5 py-0.5 rounded-full uppercase"] "Live"
              reportCardTitle_ "Week to Date"
            reportListItems pid reports nextUrl
        div_ [class_ "w-full md:w-2/3 overflow-y-auto"]
          $ div_ [class_ "flex h-full", id_ "detailSidebar"]
          $ a_ (class_ "w-full text-center cursor-pointer" : hxTrigger_ "intersect once" : detailPaneAttrs ("/p/" <> pid.toText <> "/reports/live"))
          $ div_ [class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"]
          $ loadingIndicatorWith_ LdSM LdDots "text-textWeak"


reportListItems :: Projects.ProjectId -> V.Vector Issues.ReportListItem -> Maybe Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "flex flex-row md:flex-col gap-4 w-full"] do
    forM_ reports \report ->
      reportCard_ "border-strokeWeak" ("/p/" <> pid.toText <> "/reports/" <> report.id.toText) do
        reportCardHead_ (if report.reportType == Projects.RTWeekly then "bg-fillBrand-weak capitalize" else "bg-fillWeak capitalize") (toHtml (display report.reportType) <> " report") mempty
        reportCardTitle_ $ toHtml $ formatTime defaultTimeLocale "%a, %b %d %Y" (zonedTimeToLocalTime report.createdAt)
    whenJust nextUrl \url ->
      a_ [class_ "w-full cursor-pointer block p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center mb-4", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] "LOAD MORE"
