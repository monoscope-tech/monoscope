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
  collectSystemReport,
  renderSystemEmail,
  snapshotTotals,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Map.Lazy qualified as Map
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Time.Zones (utcTZ, utcToLocalTimeTZ)
import Data.Time.Zones.All qualified as TZ
import Data.Vector qualified as V
import Effectful (Eff, IOE, type (:>))
import Effectful.Concurrent (forkIO)
import Effectful.Concurrent.Async (Concurrent, concurrently)
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Lucid
import Lucid.Htmx (hxGet_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Containers qualified as Containers
import Models.Telemetry.Report qualified as Report
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Components (EmptyStateCfg (..), emptyState_)
import Pkg.Components.Widget (WidgetType (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.EmailTemplates qualified as ET
import Relude hiding (Reader, ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Tracing (forkWithCtx)
import System.Types (ATAuthCtx, DB, RespHeaders, addRespHeaders, addSuccessToast)
import UnliftIO (tryAny)
import UnliftIO qualified
import Utils (FreeTierStatus, LoadingSize (..), LoadingType (..), checkFreeTierStatus, faSprite_, formatUTCMicros, freeTierDailyMaxEvents, hostPath, loadingIndicatorWith_)


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
errorsWidget = eventsWidget{Widget.query = Just "(status_code == \"ERROR\" or attributes.exception.type != null or severity.severity_number >= 17 or level =~ /(?i)^(error|fatal)$/) | summarize count(*) by bin_auto(timestamp), status_code", Widget.theme = Just "roma"}


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


-- | New reports persist one evidence snapshot. The legacy decoder below remains for
-- existing reports; new collection no longer computes unused chart datasets.
buildReportJson' :: Report.ReportSnapshot -> AE.Value
buildReportJson' snapshot = AE.object ["systemSnapshot" AE..= snapshot]


snapshotTotals :: Report.ReportSnapshot -> (Int, Int, Double, Double)
snapshotTotals snapshot = (events, errors, pctChange events (fromIntegral oldEvents), pctChange errors (fromIntegral oldErrors))
  where
    current = mapMaybe (.current) snapshot.services
    previous = mapMaybe (.previous) snapshot.services
    events = fromIntegral $ sum $ map (.events) current
    errors = fromIntegral $ sum $ map (.errorEvents) current
    oldEvents = sum $ map (.events) previous
    oldErrors = sum $ map (.errorEvents) previous


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


-- | Collect once per report; each optional source preserves absence versus failure.
collectSystemReport
  :: (Concurrent :> es, DB es, Labeled "timefusion" Hasql :> es, Log :> es, Reader AuthContext :> es)
  => Projects.ProjectId -> UTCTime -> UTCTime -> Eff es Report.ReportSnapshot
collectSystemReport pid start end = do
  ctx <- ask @AuthContext
  let useTf = ctx.env.enableTimefusionReads
      previousStart = addUTCTime (negate $ diffUTCTime end start) start
      infraStart = max start (addUTCTime (-Containers.freshnessWindow) end)
  ((current, previous), ((infrastructure, monitors), ((issues, topPatterns), (performance, (databases, workloads))))) <-
    concurrently
      (concurrently (Report.serviceStats useTf pid start end) (Report.serviceStats useTf pid previousStart start))
      ( concurrently
          ( concurrently
              (optionalReportSection pid "infrastructure" $ Report.infrastructureStats infraStart end <$> Containers.containersForReport useTf pid infraStart end)
              (optionalReportSection pid "monitors" $ Report.monitorStats <$> Monitors.queryMonitorsAll pid)
          )
          ( concurrently
              ( concurrently
                  (optionalReportSection pid "issues" $ Report.issueStats pid start end)
                  (optionalReportSection pid "log patterns" $ map (\p -> (p.logPattern, p.occurrenceCount, LogPatterns.sourceFieldLabel p.sourceField)) <$> LogPatterns.getLogPatterns pid 5 0)
              )
              ( concurrently
                  (optionalReportSection pid "endpoint performance" $ uncurry Report.compareEndpoints <$> concurrently (Report.endpointStats useTf pid start end) (Report.endpointStats useTf pid previousStart start))
                  ( concurrently
                      (optionalReportSection pid "database performance" $ Report.databaseStats useTf pid start end)
                      (optionalReportSection pid "workload composition" $ Report.workloadStats useTf pid start end)
                  )
              )
          )
      )
  project <- Projects.projectById pid
  trends <- Just <$> optionalReportSection pid "activity trends" (Report.trendStats useTf pid start end)
  let ingestionCapped = project <&> \p -> Projects.isFreeTier p.paymentPlan && fromIntegral (sum $ map (.recentDayEvents) current) >= freeTierDailyMaxEvents
  pure Report.ReportSnapshot{services = Report.compareServices current previous, infrastructure, monitors, issues, generatedAt = end, startTime = start, endTime = end, topPatterns, performance, databases, workloads, ingestionCapped, trends}


optionalReportSection :: (IOE :> es, Log :> es) => Projects.ProjectId -> Text -> Eff es a -> Eff es (Report.ReportSection a)
optionalReportSection pid label action =
  tryAny action
    >>= either
      (\err -> Report.Unavailable <$ Log.logAttention "Report section unavailable" (pid, label, displayException err))
      (pure . Report.Available)


-- | Shared email rendering: builds WeeklyReportData from inputs, generates chart URLs, renders email.
-- Returns @(dateLabel from endTime, subject, rendered email HTML)@. Generalised over the effect
-- row so both the request handlers and the weekly-report background job render the same email;
-- @reportUrl@ is host-relative.
renderWeeklyEmail :: (Log :> es, Reader AuthContext :> es) => Projects.ReportType -> Text -> Projects.Project -> Text -> UTCTime -> UTCTime -> Bool -> ET.ReportEvidence -> Eff es (Text, Text, Text)
renderWeeklyEmail reportType reportUrl project userName startTime endTime fullReport evidence = do
  ctx <- ask @AuthContext
  let pid = project.id
      reportUrl' = hostPath ctx.env.hostUrl reportUrl
      (dayStart, dayEnd) = reportDayLabels project.timeZone startTime endTime
      stmTxt = formatUTCMicros startTime
      endTxt = formatUTCMicros endTime
      savedTrends = case evidence of ET.SystemEvidence snapshot -> snapshot.trends; ET.HistoricalEvidence _ -> Nothing
      chartUrl label value widget = case savedTrends of
        Just Report.Unavailable -> pure ""
        _ ->
          let prepared = case savedTrends of
                Just (Report.Available points) ->
                  widget
                    { Widget.query = Nothing
                    , Widget.dataset =
                        Just
                          def
                            { Widget.source = AE.toJSON $ AE.toJSON (["Time", label] :: [Text]) : [AE.toJSON ([p.epochSeconds * 1000, value p] :: [Int64]) | p <- points]
                            , Widget.from = Just $ floor $ utcTimeToPOSIXSeconds startTime * 1000
                            , Widget.to = Just $ floor $ utcTimeToPOSIXSeconds endTime * 1000
                            }
                    }
                _ -> widget
           in Widget.widgetPngUrl ctx.env.apiKeyEncryptionSecretKey ctx.env.hostUrl pid prepared Nothing (Just stmTxt) (Just endTxt)
  eventsUrl <- chartUrl ("Events" :: Text) (.events) eventsWidget
  errorsUrl <- chartUrl "Errors" (.errors) errorsWidget
  let projectUrl = hostPath ctx.env.hostUrl ("p/" <> pid.toText)
      reportData =
        ET.WeeklyReportData
          { reportType
          , userName
          , projectName = project.title
          , reportUrl = reportUrl'
          , projectUrl
          , startDate = dayStart
          , endDate = dayEnd
          , eventsChartUrl = eventsUrl
          , errorsChartUrl = errorsUrl
          , evidence
          , fullReport
          , timeZone = if isJust (TZ.tzByName $ encodeUtf8 project.timeZone) then project.timeZone else "UTC"
          , fromTime = stmTxt
          , toTime = endTxt
          }
      (subj, html) = ET.weeklyReportEmail reportData
  pure (dayEnd, subj, ET.renderEmail subj html)


-- | Reconstruct the email HTML from a stored Report's reportJson. Returns (dateLabel, emailHtml)
reportToEmailHtml :: Issues.Report -> Projects.Project -> Text -> ATAuthCtx (Text, Text)
reportToEmailHtml report project userName =
  case AET.parseMaybe (AE.withObject "Report" (AE..:? "systemSnapshot")) report.reportJson of
    Just (Just snapshot) -> dropSubject <$> renderSystemEmail report.reportType reportUrl project userName True snapshot
    Nothing -> invalid "Invalid system report snapshot"
    Just Nothing -> case AE.fromJSON @ReportData report.reportJson of
      AE.Error err -> invalid (toText err)
      AE.Success rd -> do
        let anomalies' = V.fromList rd.issues
            performance = V.fromList $ (\ep -> (ep.host, ep.method, ep.urlPath, fromIntegral ep.averageDuration :: Int64, ep.durationDiffPct, fromIntegral ep.requestCount :: Int64, ep.requestDiffPct)) <$> rd.endpoints
            slowQueries = V.fromList $ (\q -> (q.query, round q.averageDuration :: Int, fromIntegral q.totalEvents :: Int)) <$> rd.slowDbQueries
            historical =
              ET.HistoricalReportEvidence
                { totalEvents = fromIntegral rd.events.total
                , totalErrors = fromIntegral rd.errors.total
                , anomalies = anomalies'
                , performance
                , slowQueries
                }
        dropSubject <$> renderWeeklyEmail report.reportType reportUrl project userName report.startTime report.endTime True (ET.HistoricalEvidence historical)
  where
    reportUrl = "/p/" <> project.id.toText <> "/reports/" <> report.id.toText
    invalid err = ("", "Error: Could not parse report data") <$ Log.logAttention "Unparseable stored report json" (report.id.toText, err)


renderSystemEmail :: (Log :> es, Reader AuthContext :> es) => Projects.ReportType -> Text -> Projects.Project -> Text -> Bool -> Report.ReportSnapshot -> Eff es (Text, Text, Text)
renderSystemEmail reportType reportUrl project userName fullReport snapshot =
  renderWeeklyEmail reportType reportUrl project userName snapshot.startTime snapshot.endTime fullReport (ET.SystemEvidence snapshot)


-- | The page views show the email body; only the background job needs its subject line.
dropSubject :: (Text, Text, Text) -> (Text, Text)
dropSubject (dateLabel, _, html) = (dateLabel, html)


-- | Generation runs in the app-lifetime scope, not the HTTP request. A database
-- lease deduplicates work across replicas; shutdown leaves a recoverable lease.
startLivePreview :: Projects.ProjectId -> UTCTime -> ATAuthCtx ()
startLivePreview pid token = do
  ctx <- ask @AuthContext
  let worker = do
        result <- UnliftIO.tryAny $ UnliftIO.timeout (300 * 1_000_000) $ collectSystemReport pid (addUTCTime (-(7 * 86400)) token) token
        status <- case result of
          Right (Just snapshot) -> pure $ Report.PreviewReady snapshot
          Right Nothing -> Report.PreviewFailed <$ Log.logAttention "Live report generation timed out" pid
          Left err -> Report.PreviewFailed <$ Log.logAttention "Live report generation failed" (pid, displayException err)
        Report.finishPreview pid token status
      guardedWorker = UnliftIO.tryAny worker >>= either (\err -> Log.logAttention "Live report worker failed" (pid, displayException err)) pure
  case ctx.backgroundScope of
    Just scope -> void $ forkWithCtx scope guardedWorker
    Nothing -> void $ forkIO guardedWorker


reportsPostH :: Projects.ProjectId -> Projects.ReportType -> ATAuthCtx (RespHeaders ReportsPost)
reportsPostH pid t = do
  _ <- Projects.sessionAndProject pid
  _ <- Projects.updateProjectReportNotif pid t
  addSuccessToast "Report notifications updated successfully" Nothing
  addRespHeaders $ ReportsPost "updated"


newtype ReportsPost = ReportsPost Text


instance ToHtml ReportsPost where
  toHtml (ReportsPost _t) = ""
  toHtmlRaw = toHtml


wrapSingleResponse :: BWConfig -> FreeTierStatus -> Text -> Maybe Text -> (Text, Text, Text) -> ATAuthCtx (RespHeaders ReportsGet)
wrapSingleResponse bw freeTierStatus pageTitle hxRequestM content =
  addRespHeaders $ maybe (ReportsGetSingle $ PageCtx bw{pageTitle, menuItem = Just "Reports", freeTierStatus} content) (const $ ReportsGetSingle' content) hxRequestM


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
  claimed <- Report.claimPreview pid
  view <- case claimed of
    Just token -> LiveReportBuilding pid <$ startLivePreview pid token
    Nothing ->
      Report.getPreview pid >>= \case
        Just (Report.PreviewReady snapshot) -> do
          (dateLabel, emailHtml) <- dropSubject <$> renderSystemEmail Projects.RTWeekly ("/p/" <> pid.toText <> "/reports/live") project sess.user.firstName True snapshot
          pure $ LiveReportReady ("weekly", dateLabel, emailHtml)
        Just Report.PreviewFailed -> pure $ LiveReportFailed pid
        _ -> pure $ LiveReportBuilding pid
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  addRespHeaders
    $ maybe
      (ReportsGetLive $ PageCtx bw{pageTitle = "Reports", menuItem = Just "Reports", freeTierStatus} view)
      (const $ ReportsGetLive' view)
      hxRequestM


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
  | ReportsGetLive (PageCtx LiveReportView)
  | ReportsGetLive' LiveReportView


instance ToHtml ReportsGet where
  toHtml (ReportsGetMain (PageCtx conf (pid, reports, next))) = toHtml $ PageCtx conf $ reportsPage pid reports next
  toHtml (ReportsGetList pid reports next) = toHtml $ reportListItems pid reports next
  toHtml (ReportsGetSingle (PageCtx conf content)) = toHtml $ PageCtx conf $ singleReportPage content
  toHtml (ReportsGetSingle' content) = toHtml $ singleReportPage content
  toHtml (ReportsGetLive (PageCtx conf view)) = toHtml $ PageCtx conf $ liveReportPage view
  toHtml (ReportsGetLive' view) = toHtml $ liveReportPage view
  toHtmlRaw = toHtml


data LiveReportView = LiveReportBuilding Projects.ProjectId | LiveReportFailed Projects.ProjectId | LiveReportReady (Text, Text, Text)


liveReportPage :: LiveReportView -> Html ()
liveReportPage view = div_ ([id_ "live-report-preview", class_ "w-full h-full min-h-0"] <> polling) $ case view of
  LiveReportReady content -> singleReportPage content
  LiveReportBuilding _ -> div_ [class_ "flex h-full flex-col items-center justify-center gap-3 p-6 text-center", role_ "status"] do
    span_ [class_ "loading loading-spinner text-textBrand"] ""
    h3_ [class_ "font-medium text-textStrong"] "Preparing your system report"
    p_ [class_ "text-sm text-textWeak max-w-md"] "Collecting the last seven days across your services, infrastructure, issues, and monitors. This page will update when the report is ready."
  LiveReportFailed pid -> div_ [class_ "flex h-full flex-col items-center justify-center gap-3 p-6 text-center", role_ "status"] do
    h3_ [class_ "font-medium text-textStrong"] "The report could not be prepared"
    p_ [class_ "text-sm text-textWeak max-w-md"] "Some report data could not be collected. We’ll retry in a minute. Your saved reports are still available."
    a_ [href_ $ "/p/" <> pid.toText <> "/reports", class_ "text-sm text-textBrand underline"] "View saved reports"
  where
    polling = case view of
      LiveReportReady{} -> []
      LiveReportBuilding pid -> poll pid "every 2s"
      LiveReportFailed pid -> poll pid "every 60s"
    poll pid trigger = [hxGet_ $ "/p/" <> pid.toText <> "/reports/live", hxTrigger_ trigger, hxTarget_ "this", hxSelect_ "#live-report-preview", hxSwap_ "outerHTML"]


-- | (reportType, dateLabel, emailHtml)
singleReportPage :: (Text, Text, Text) -> Html ()
singleReportPage (reportType, dateLabel, emailHtml) =
  div_ [class_ "w-full flex flex-col h-full"] do
    div_ [class_ "flex w-full justify-between items-center border-b p-4"] do
      h3_ [class_ "text-textStrong font-medium capitalize"] $ toHtml reportType <> " report"
      span_ [class_ "text-sm text-textWeak"] $ toHtml dateLabel
    if T.null emailHtml
      then h3_ [class_ "p-4"] "Report Not Found"
      else iframe_ [term "srcdoc" emailHtml, style_ "width:100%;height:100%;border:none;", term "sandbox" "allow-same-origin allow-top-navigation-by-user-activation"] ""


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
          $ emptyState_ def{icon = Just "empty"} "No reports generated yet" "Scheduled reports will appear here after the first report is generated."
      else do
        div_ [class_ "w-full md:w-1/3 md:border-r border-b md:border-b-0 border-strokeWeak p-4 overflow-x-auto md:overflow-y-auto"]
          $ div_ [class_ "mt-4 flex flex-row md:flex-col gap-4 w-full"] do
            reportCard_ "border-strokeBrand-weak bg-fillBrand-weak/10" ("/p/" <> pid.toText <> "/reports/live") do
              reportCardHead_ "bg-fillBrand-weak" "Weekly report"
                $ span_ [class_ "bg-fillSuccess-strong text-textInverse-strong text-2xs font-bold px-1.5 py-0.5 rounded-full uppercase"] "Live"
              reportCardTitle_ "Last 7 days"
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
