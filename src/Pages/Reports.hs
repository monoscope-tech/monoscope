{-# OPTIONS_GHC -Wno-partial-fields #-}

module Pages.Reports (
  reportsGetH,
  singleReportGetH,
  buildReportJSON,
  buildPerformanceJSON,
  buildAnomalyJSON,
  getPerformanceInsight,
  renderEndpointsTable,
  getPerformanceEmailTemplate,
  getAnomaliesEmailTemplate,
  reportsPostH,
  buildReportJson',
  ReportAnomalyType (..),
  PerformanceReport (..),
  ReportsGet (..),
  ReportsPost (..),
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AEP
import Data.Default (def)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Fields.Types (textFieldTypeToText)
import Models.Apis.Issues qualified as Issues
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps (EndpointPerf, RequestForReport (endpointHash))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.Components.Widget qualified as Widgets
import Relude hiding (ask)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Text.Printf (printf)
import Utils (checkFreeTierExceeded, faSprite_, getDurationNSMS, prettyPrintCount)


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


data ReportAnomalyType
  = ATEndpoint
      { endpointUrlPath :: Text
      , endpointMethod :: Text
      , eventsCount :: Int
      }
  | ATShape
      { endpointUrlPath :: Text
      , endpointMethod :: Text
      , targetHash :: Text
      , newUniqueFields :: [Text]
      , updatedFieldFormats :: [Text]
      , deletedFields :: [Text]
      , eventsCount :: Int
      }
  | ATFormat
      { endpointUrlPath :: Text
      , keyPath :: Text
      , endpointMethod :: Text
      , formatType :: Text
      , formatExamples :: [Text]
      , eventsCount :: Int
      }
  | ATRuntimeException
      { endpointUrlPath :: Text
      }
  | UnknownAnomaly
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON ReportAnomalyType where
  parseJSON = AE.withObject "ReportAnomalyType" $ \o -> do
    anomalyType <- o AE..:? "anomaly_type" :: AEP.Parser (Maybe Text)
    case anomalyType of
      Just _ -> pure UnknownAnomaly
      Nothing -> do
        tag <- o AE..:? "tag" :: AEP.Parser (Maybe Text)
        case tag of
          Just "ATEndpoint" ->
            ATEndpoint
              <$> o
                AE..: "endpointUrlPath"
              <*> o
                AE..: "endpointMethod"
              <*> o
                AE..: "eventsCount"
          Just "ATShape" ->
            ATShape
              <$> o
                AE..: "endpointUrlPath"
              <*> o
                AE..: "endpointMethod"
              <*> o
                AE..: "targetHash"
              <*> o
                AE..: "newUniqueFields"
              <*> o
                AE..: "updatedFieldFormats"
              <*> o
                AE..: "deletedFields"
              <*> o
                AE..: "eventsCount"
          Just "ATFormat" ->
            ATFormat
              <$> o
                AE..: "endpointUrlPath"
              <*> o
                AE..: "keyPath"
              <*> o
                AE..: "endpointMethod"
              <*> o
                AE..: "formatType"
              <*> o
                AE..: "formatExamples"
              <*> o
                AE..: "eventsCount"
          Just "ATRuntimeException" ->
            ATRuntimeException
              <$> o
                AE..: "endpointUrlPath"
          _ -> pure UnknownAnomaly


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
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


buildReportJson' :: Int -> Int -> Double -> Double -> V.Vector (Text, Int, Double, Int, Double) -> V.Vector (Text, Text, Text, Int, Double, Int, Double) -> V.Vector (Text, Int, Int) -> AE.Value
buildReportJson' totalEvents totalErrors eventsChange errorsChange spanTypeStatsDiff' endpointsPerformance slowDbQueries =
  let spanStatsDiff = (\(t, e, chang, dur, durChange) -> AE.object ["spanType" AE..= t, "eventCount" AE..= e, "eventChange" AE..= chang, "averageDuration" AE..= dur, "durationChange" AE..= durChange]) <$> spanTypeStatsDiff'
      perf = (\(u, m, p, d, dc, req, cc) -> AE.object ["host" AE..= u, "urlPath" AE..= p, "method" AE..= m, "averageDuration" AE..= d, "durationDiffPct" AE..= dc, "requestCount" AE..= req, "requestDiffPct" AE..= cc]) <$> V.take 10 endpointsPerformance
      slowDbQueries' = (\(q, d, c) -> AE.object ["query" AE..= q, "averageDuration" AE..= d, "totalEvents" AE..= c]) <$> slowDbQueries
   in AE.object ["endpoints" AE..= perf, "events" AE..= AE.object ["total" AE..= totalEvents, "change" AE..= errorsChange], "errors" AE..= AE.object ["total" AE..= totalErrors, "change" AE..= eventsChange], "spanTypeStats" AE..= spanStatsDiff, "slowDbQueries" AE..= slowDbQueries']


reportsPostH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders ReportsPost)
reportsPostH pid t = do
  _ <- Sessions.sessionAndProject pid
  apiKeys <- dbtToEff $ Projects.updateProjectReportNotif pid t
  addSuccessToast "Report notifications updated Successfully" Nothing
  addRespHeaders $ ReportsPost "updated"


newtype ReportsPost = ReportsPost Text


instance ToHtml ReportsPost where
  toHtml (ReportsPost _t) = ""
  toHtmlRaw = toHtml


singleReportGetH :: Projects.ProjectId -> Reports.ReportId -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
singleReportGetH pid rid hxRequestM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  report <- dbtToEff $ Reports.getReportById rid
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Report"
          , freeTierExceeded = freeTierExceeded
          , config = appCtx.env
          }
  case hxRequestM of
    Just _ -> addRespHeaders $ ReportsGetSingle' (pid, report)
    _ -> addRespHeaders $ ReportsGetSingle $ PageCtx bwconf (pid, report)


reportsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsGetH pid page hxRequest hxBoosted = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  let p = toString (fromMaybe "0" page)
  let pg = fromMaybe 0 (readMaybe p :: Maybe Int)

  reports <- dbtToEff $ Reports.reportHistoryByProject pid pg
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan
  let nextUrl = "/p/" <> show pid.unProjectId <> "/reports?page=" <> show (pg + 1)
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


data ReportsGet
  = ReportsGetMain (PageCtx (Projects.ProjectId, V.Vector Reports.ReportListItem, Text, Bool, Bool))
  | ReportsGetList Projects.ProjectId (V.Vector Reports.ReportListItem) Text
  | ReportsGetSingle (PageCtx (Projects.ProjectId, Maybe Reports.Report))
  | ReportsGetSingle' (Projects.ProjectId, Maybe Reports.Report)


-- this one has no PageCtx wrapping
instance ToHtml ReportsGet where
  toHtml (ReportsGetMain (PageCtx conf (pid, reports, next, daily, weekly))) = toHtml $ PageCtx conf $ reportsPage pid reports next daily weekly
  toHtml (ReportsGetList pid reports next) = toHtml $ reportListItems pid reports next
  toHtml (ReportsGetSingle (PageCtx conf (pid, report))) = toHtml $ PageCtx conf $ singleReportPage pid report
  toHtml (ReportsGetSingle' (pid, report)) = toHtml $ singleReportPage pid report
  toHtmlRaw = toHtml


singleReportPage :: Projects.ProjectId -> Maybe Reports.Report -> Html ()
singleReportPage pid report =
  div_ [class_ "mx-auto w-full flex flex-col overflow-y-scroll h-full"] do
    case report of
      Just report' -> do
        div_ [class_ "flex w-full justify-between items-center border-b p-4"] do
          h3_ [class_ "text-textStrong font-medium capitalize"] $ toHtml report'.reportType <> " report"
          span_ [] $ show $ localDay (zonedTimeToLocalTime report'.createdAt)
        div_ [class_ "space-y-4"] do
          div_ [class_ "mx-auto max-w-[1000px]"] do
            div_ [class_ "px-4 py-3 space-y-8"] do
              let rep_json = AE.decode (AE.encode report'.reportJson) :: Maybe ReportData
              let r = AE.eitherDecode (AE.encode report'.reportJson) :: Either String ReportData
              case r of
                Left err -> do
                  pre_ [class_ "text-textError"] $ toHtml $ "Error parsing report data: " <> T.pack err
                Right _ -> pass
              case rep_json of
                Just v -> do
                  div_ [class_ "flex gap-2 h-48 items-center"] do
                    div_ [class_ "h-full w-1/2 border rounded-lg p-3"] do
                      div_ [class_ "flex w-full items-center justify-between"] do
                        div_ [class_ "flex text-sm items-center gap-3"] do
                          span_ [class_ "text-textWeak"] "Total events"
                          span_ [class_ "font-semibold"] $ toHtml $ prettyPrintCount (fromIntegral v.events.total)
                        span_ [class_ $ "text-xs" <> (if v.events.change < 0 then " text-textError" else " text-textSuccess")] $ show v.events.change <> "% from last period"
                      div_ [class_ "h-[90%]"] do
                        Widget.widget_
                          $ (def :: Widget.Widget)
                            { Widget.wType = WTTimeseries
                            , Widget.unit = Just "rows"
                            , Widget.naked = Just True
                            , Widget.sql =
                                Just
                                  $ [text|SELECT extract(epoch from time_bucket('1 day', timestamp))::integer AS bucket
                                        , kind
                                        , count(*)::float
                                        FROM otel_logs_and_spans
                                        WHERE project_id = '{{project_id}}'
                                        AND timestamp >= NOW() - INTERVAL '7 days'
                                        GROUP BY bucket, kind
                                        ORDER BY bucket |]
                            , Widget.title = Just "Events trend"
                            , Widget.hideLegend = Just True
                            , Widget._projectId = Just pid
                            , Widget.standalone = Just True
                            , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
                            , Widget.allowZoom = Just True
                            , Widget.showMarkArea = Just True
                            , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
                            }
                    div_ [class_ "h-full w-1/2 border rounded-lg p-3"] do
                      div_ [class_ "flex w-full items-center justify-between"] do
                        div_ [class_ "flex text-sm items-center gap-3"] do
                          span_ [class_ "text-textWeak"] "Error trend"
                          span_ [class_ "font-semibold"] $ toHtml $ prettyPrintCount (fromIntegral v.errors.total)
                        span_ [class_ $ "text-xs" <> (if v.errors.change >= 0 then " text-textError" else " text-textSuccess ")] $ show v.errors.change <> "% from last week"
                      div_ [class_ "h-[90%]"] do
                        Widget.widget_
                          $ (def :: Widget.Widget)
                            { Widget.wType = WTTimeseries
                            , Widget.query = Just "status_message == \"ERROR\" | summarize count(*) by bin_auto(timestamp)"
                            , Widget.unit = Just "rows"
                            , Widget.naked = Just True
                            , Widget.title = Just "Errors trend"
                            , Widget.theme = Just "roma"
                            , Widget.hideLegend = Just True
                            , Widget._projectId = Just pid
                            , Widget.standalone = Just True
                            , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
                            , Widget.allowZoom = Just True
                            , Widget.showMarkArea = Just True
                            , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
                            }
                  div_ [class_ "space-y-6"] do
                    div_ [class_ "pt-3 border-t flex justify-between"] do
                      h5_ [class_ "text-sm font-medium"] "Span types overview"
                    let cats = v.spanTypeStats
                    div_ [class_ "flex items-center w-full overflow-x-auto gap-3"] do
                      forM_ cats $ \cat -> do
                        categoryCard cat.spanType cat.eventCount cat.averageDuration cat.durationChange cat.eventChange
                  div_ [class_ "space-y-6 pt-4"] do
                    div_ [class_ "pt-3 border-t flex justify-between"] do
                      h5_ [class_ "text-sm font-medium"] "HTTP endpoints"
                    renderEndpointsTable v.endpoints
                  unless (null v.slowDbQueries) $ do
                    div_ [class_ "space-y-3 pt-4 w-full"] do
                      div_ [class_ "pt-3 border-t flex justify-between"] do
                        h5_ [class_ "text-sm font-medium"] "Slow queries"
                      div_ [class_ "w-full border rounded-lg"] do
                        table_ [class_ "table-auto w-full"] do
                          thead_ [class_ "text-xs text-left text-textStrong font-medium capitalize border-b"] $ tr_ do
                            th_ [class_ "p-2 "] "Query"
                            th_ [class_ "p-2 "] "Avg. duration"
                            th_ [class_ "p-2 "] "Total events"
                          tbody_ [class_ "text-sm"] $ forM_ v.slowDbQueries $ \queryStat -> do
                            tr_ [class_ "p"] do
                              td_ [class_ "p-2 text-textStrong max-w-96 truncate ellipsis"] $ toHtml queryStat.query
                              td_ [class_ "p-2"] $ toHtml $ getDurationNSMS (round queryStat.averageDuration)
                              td_ [class_ "p-2"] $ toHtml $ prettyPrintCount (fromIntegral queryStat.totalEvents)
                Nothing -> pass
      Nothing -> do
        h3_ [] "Report Not Found"


shapeParameterStats_ :: Int -> Int -> Int -> Html ()
shapeParameterStats_ newF deletedF updatedFF = div_ [class_ "inline-block"] do
  div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs"] do
    div_ [class_ "p-2 py-1 bg-fillSuccess-weak text-textSuccess border border-strokeSuccess-strong"] do
      div_ [class_ "text-base"] $ toHtml @String $ show newF
      small_ [class_ "block"] "new fields"
    div_ [class_ " p-2 py-1 bg-fillWeaker text-textStrong border border-strokeMedium"] do
      div_ [class_ "text-base"] $ toHtml @String $ show updatedFF
      small_ [class_ "block"] "updated fields"
    div_ [class_ "p-2  py-1  bg-fillError-weak text-textError border border-strokeError-strong"] do
      div_ [class_ "text-base"] $ toHtml @String $ show deletedF
      small_ [class_ "block"] "deleted fields"


reportsPage :: Projects.ProjectId -> V.Vector Reports.ReportListItem -> Text -> Bool -> Bool -> Html ()
reportsPage pid reports nextUrl daily weekly =
  div_ [class_ "flex flex-row h-full border-t"] do
    when (V.null reports) do
      div_ [class_ "m-auto"] do
        div_ [class_ "flex flex-col items-center justify-center py-16 px-4"] $ do
          div_ [class_ "rounded-full flex items-center justify-center mb-3"] $ do
            faSprite_ "empty" "regular" "h-12 w-12"
          h3_ [class_ "text-xl text-textStrong mb-2"] "No reports generated for this project yet."
    unless (V.null reports) do
      div_ [class_ "w-1/3 border-r border-strokeWeak p-4 overflow-y-auto"] do
        div_ [class_ "mt-4"] do
          reportListItems pid reports nextUrl
      div_ [class_ "w-2/3 overflow-y-auto"] do
        div_ [class_ "flex items-center justify-center h-full", id_ "detailSidebar"] do
          div_ [class_ "text-center"] do
            faSprite_ "clapperboard" "light" "w-36 h-36 mx-auto"
            h3_ [class_ "text-xl font-bold mb-4 text-textStrong"] "View Each Report Details here"
            h3_ [class_ "mt-2 text-lg font-medium text-textStrong"] "But nothing is selected yet"
            p_ [class_ "mt-1  text-textWeak"] "Select a field or similar item on the left"
            p_ [class_ "mt-1  text-textWeak"] "to view more details about it here."


-- div_ [class_ "w-5 bg-gray-200"] ""

reportListItems :: Projects.ProjectId -> V.Vector Reports.ReportListItem -> Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "space-y-4 w-full"] do
    forM_ reports $ \report -> do
      let isWeeklyData = report.reportType == "weekly"
      div_ [class_ "w-full flex flex-col border border-strokeWeak rounded-lg cursor-pointer hover:bg-fillWeaker"] do
        div_ [class_ $ "w-full"] do
          a_
            [ class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"
            , hxGet_ $ "/p/" <> show pid.unProjectId <> "/reports/" <> show report.id.reportId
            , hxTarget_ "#detailSidebar"
            , hxSwap_ "innerHTML"
            ]
            do
              div_ [class_ "flex flex-col grow gap-4"] do
                div_ [class_ "flex items-center w-full justify-between gap-2"] do
                  div_ [class_ $ (if isWeeklyData then "bg-fillBrand-weak" else "bg-fillWeak") <> " text-xs font-medium px-2.5 py-1 rounded-full capitalize"] $ toHtml report.reportType <> " report"
                  faSprite_ "chevron-right" "regular" "w-3 h-3"
                h4_ [class_ "font-medium flex items-center gap-2"] do
                  faSprite_ "calendar" "regular" "w-4 h-4"
                  toHtml $ formatTime defaultTimeLocale "%a, %b %d %Y" (zonedTimeToLocalTime report.createdAt)
    unless (length reports < 20) $ do
      a_ [class_ "w-full cursor-pointer block p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center mb-4", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ nextUrl] "LOAD MORE"


renderEndpointRow :: PerformanceReport -> Html ()
renderEndpointRow endpoint = tr_ [class_ ""] do
  let (pcls, prc) =
        if endpoint.durationDiffPct > 0
          then ("text-textError" :: Text, "+" <> show (durationDiffPct endpoint) <> "%" :: Text)
          else ("text-textSuccess", show (durationDiffPct endpoint) <> "%")
  td_ [class_ "p-2 text-textWeak w-96 flex gap-4 items-center "] do
    span_ [class_ $ "cbadge-sm badge-" <> endpoint.method] $ toHtml $ endpoint.method
    span_ [class_ "flex-shrink-0 text-textStrong"] $ toHtml endpoint.urlPath
  td_ [class_ "p-2"] $ toHtml $ prettyPrintCount endpoint.requestCount
  td_ [class_ $ "p-2 " <> pcls] $ toHtml $ show endpoint.requestDiffPct <> "%"
  td_ [class_ "p-2"] $ toHtml $ getDurationNSMS endpoint.averageDuration
  td_ [class_ $ "p-2 " <> pcls] $ toHtml prc


renderEndpointsTable :: [PerformanceReport] -> Html ()
renderEndpointsTable endpoints = div_ [class_ "w-full border rounded-lg overflow-hidden"] $ table_ [class_ "table-auto w-full"] do
  thead_ [class_ "text-xs text-left text-textStrong font-medium capitalize border-b"] $ tr_ do
    th_ [class_ "p-2"] "Endpoint"
    th_ [class_ "p-2"] "Requests"
    th_ [class_ "p-2"] "Request %"
    th_ [class_ "p-2"] "Avg. latency"
    th_ [class_ "p-2"] "Latency %"
  tbody_ [class_ "text-sm"] $ mapM_ renderEndpointRow endpoints


summaryCard :: Text -> Text -> Text -> Text -> Html ()
summaryCard title value subtitle change = do
  div_ [class_ "rounded-lg flex p-3 flex-col gap-3 border w-max"] $ do
    div_ [class_ "flex flex-col"] $ do
      h3_ [class_ "text-sm font-medium text-textWeak"] $ toHtml title
    div_ [class_ "flex flex-col gap-2"] $ do
      div_ $ do
        div_ [class_ "text-lg font-bold text-textStrong"] $ toHtml value
        p_ [class_ "text-xs text-textWeak"] $ toHtml subtitle
      div_ [class_ "flex items-center gap-2 text-sm font-medium text-chart-1"] $ do
        faSprite_ (if T.isPrefixOf "-" change then "arrow-down" else "arrow-up") "regular" "w-3 h-3"
        span_ [] $ toHtml $ change <> " from last period"


categoryCard :: Text -> Integer -> Double -> Double -> Double -> Html ()
categoryCard category total avDur changeDur changeCount = do
  div_ [class_ "flex text-sm flex-col items-center"] $ do
    div_ [class_ "flex items-center gap-1"] $ do
      faSprite_ (getFaSprite category) "regular" "w-3 h-3"
      h3_ [class_ "text-sm text-textStrong "] $ toHtml category
    span_ [class_ "h-4 w-[2px] bg-fillWeak"] $ pass
    div_ [class_ "flex w-full items-center"] $ do
      div_ [class_ "flex w-24 flex-col items-center", term "data-tippy-content" "Total events"] $ do
        span_ [class_ "w-12 border-t border-t-strokeWeak self-end"] $ pass
        span_ [class_ "h-4 w-[2px] bg-fillWeak"] $ pass
        div_ [class_ "text-sm text-textStrong"] $ toHtml (prettyPrintCount (fromIntegral total))
        div_ [class_ $ "flex items-center gap-1 text-xs mt-1" <> (if changeCount < 0 then " text-textError" else " text-textSuccess")] $ do
          faSprite_ (if changeCount < 0 then "trending-down" else "trending-up") "regular" "w-3 h-3"
          span_ [] $ toHtml $ (show changeCount) <> "%"
      div_ [class_ "flex w-24 flex-col items-center", term "data-tippy-content" "Average duration"] $ do
        span_ [class_ "w-12 border-t border-t-strokeWeak self-start"] $ pass
        span_ [class_ "h-4 w-[2px] bg-fillWeak"] $ pass
        div_ [class_ "text-sm text-textStrong"] $ toHtml (getDurationNSMS $ round avDur)
        div_ [class_ $ "flex items-center gap-1 text-xs mt-1" <> (if changeDur < 0 then " text-textSuccess" else " text-textError")] $ do
          faSprite_ (if changeDur < 0 then "trending-down" else "trending-up") "regular" "w-3 h-3"
          span_ [] $ toHtml $ (show changeDur) <> "%"


getFaSprite :: Text -> Text
getFaSprite category =
  case T.toLower category of
    "http" -> "web"
    "db" -> "database"
    "cache" -> "memory"
    "external" -> "globe"
    "internal" -> "function"
    _ -> "cube"


buildReportJSON :: V.Vector Issues.IssueL -> V.Vector RequestForReport -> V.Vector EndpointPerf -> AE.Value
buildReportJSON anomalies endpoints_perf previous_perf =
  let anomalies_json = buildAnomalyJSON anomalies (length anomalies)
      perf_insight = getPerformanceInsight endpoints_perf previous_perf
      perf_json = buildPerformanceJSON perf_insight
      report_json = case anomalies_json of
        AE.Object va -> case perf_json of
          AE.Object vp -> AE.Object (vp <> va)
          _ -> AE.object []
        _ -> AE.object []
   in report_json


buildPerformanceJSON :: V.Vector PerformanceReport -> AE.Value
buildPerformanceJSON pr = AE.object ["endpoints" AE..= pr]


buildAnomalyJSON :: V.Vector Issues.IssueL -> Int -> AE.Value
buildAnomalyJSON anomalies total = AE.object ["anomalies" AE..= V.catMaybes (V.map buildjson anomalies), "anomaliesCount" AE..= total]
  where
    buildjson :: Issues.IssueL -> Maybe AE.Value
    buildjson issue = case issue.issueType of
      Issues.APIChange ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (apiData :: Issues.APIChangeData) ->
            Just
              $ AE.object
                [ "endpointUrlPath" AE..= apiData.endpointPath
                , "endpointMethod" AE..= apiData.endpointMethod
                , "targetHash" AE..= issue.endpointHash
                , "tag" AE..= Anomalies.ATShape
                , "newUniqueFields" AE..= apiData.newFields
                , "updatedFieldFormats" AE..= apiData.modifiedFields
                , "deletedFields" AE..= apiData.deletedFields
                , "eventsCount" AE..= issue.eventCount
                ]
          _ -> Nothing
      Issues.RuntimeException ->
        case AE.fromJSON (getAeson issue.issueData) of
          AE.Success (errorData :: Issues.RuntimeExceptionData) ->
            Just
              $ AE.object
                [ "endpointUrlPath" AE..= fromMaybe "/" errorData.requestPath
                , "endpointMethod" AE..= fromMaybe "UNKNOWN" errorData.requestMethod
                , "tag" AE..= Anomalies.ATRuntimeException
                , "eventsCount" AE..= issue.eventCount
                ]
          _ -> Nothing
      _ -> Nothing


getAnomaliesEmailTemplate :: V.Vector Issues.IssueL -> V.Vector AE.Value
getAnomaliesEmailTemplate anomalies = buildEmailjson <$> anomalies
  where
    buildEmailjson :: Issues.IssueL -> AE.Value
    buildEmailjson issue =
      let baseObject =
            [ "title" AE..= issue.title
            , "eventsCount" AE..= issue.eventCount
            , "firstSeen" AE..= formatUTC issue.lastSeen
            ]
       in case issue.issueType of
            Issues.APIChange ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (apiData :: Issues.APIChangeData) ->
                  AE.object
                    $ baseObject
                      <> [ "tag" AE..= "ATShape"
                         , "deletedFields" AE..= length apiData.deletedFields
                         , "endpointMethod" AE..= apiData.endpointMethod
                         , "endpointUrlPath" AE..= apiData.endpointPath
                         , "newUniqueFields" AE..= length apiData.newFields
                         , "updatedFields" AE..= length apiData.modifiedFields
                         ]
                _ -> AE.object baseObject
            Issues.RuntimeException ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (errorData :: Issues.RuntimeExceptionData) ->
                  AE.object
                    $ baseObject
                      <> [ "tag" AE..= "ATRuntimeException"
                         , "endpointMethod" AE..= fromMaybe "UNKNOWN" errorData.requestMethod
                         , "endpointUrlPath" AE..= fromMaybe "/" errorData.requestPath
                         ]
                _ -> AE.object baseObject
            _ -> AE.object baseObject


formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"


getPerformanceInsight :: V.Vector RequestDumps.RequestForReport -> V.Vector RequestDumps.EndpointPerf -> V.Vector PerformanceReport
getPerformanceInsight req_dumps previous_p =
  let prMap = Map.fromList [(p.endpointHash, p.averageDuration) | p <- V.toList previous_p]
      pin = V.map (mapFunc prMap) req_dumps
      perfInfo = V.filter (\x -> x.durationDiffPct > 15 || x.durationDiffPct < -15) pin
   in perfInfo


getPerformanceEmailTemplate :: V.Vector RequestDumps.RequestForReport -> V.Vector RequestDumps.EndpointPerf -> V.Vector AE.Value
getPerformanceEmailTemplate pr previous_p =
  ( \p ->
      AE.object
        [ "endpointUrlPath" AE..= p.urlPath
        , "endpointMethod" AE..= p.method
        , "averageLatency" AE..= getMs p.averageDuration
        , "latencyChange" AE..= 0
        ]
  )
    <$> getPerformanceInsight pr previous_p
  where
    getMs :: Integer -> String
    getMs val = msText
      where
        dbo = divideIntegers val 1000000
        msText = printf "%.2fms" dbo


mapFunc :: Map.Map Text Integer -> RequestDumps.RequestForReport -> PerformanceReport
mapFunc prMap rd =
  case Map.lookup rd.endpointHash prMap of
    Just prevDuration ->
      let diff = rd.averageDuration - prevDuration
          diffPct = divideIntegers diff prevDuration * 100
       in PerformanceReport
            { urlPath = rd.urlPath
            , method = rd.method
            , host = ""
            , averageDuration = rd.averageDuration
            , durationDiffPct = diffPct
            , requestCount = 0
            , requestDiffPct = 0
            }
    Nothing ->
      PerformanceReport
        { urlPath = rd.urlPath
        , method = rd.method
        , host = ""
        , averageDuration = rd.averageDuration
        , durationDiffPct = 0
        , requestCount = 0
        , requestDiffPct = 0
        }


divideIntegers :: Integer -> Integer -> Double
divideIntegers a b = fromIntegral a / fromIntegral b
