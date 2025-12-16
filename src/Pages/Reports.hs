{-# OPTIONS_GHC -Wno-partial-fields #-}

module Pages.Reports (
  reportsGetH,
  singleReportGetH,
  renderEndpointsTable,
  getAnomaliesEmailTemplate,
  reportsPostH,
  buildReportJson',
  PerformanceReport (..),
  ReportsGet (..),
  ReportsPost (..),
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.List (foldl)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Issues qualified as Issues
import Models.Apis.Reports qualified as Reports
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Charts.Charts qualified as Charts
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (..))
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask)
import System.Config (AuthContext (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
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
data IssueStat = IssueStat
  { id :: Issues.IssueId
  , title :: Text
  , critical :: Bool
  , severity :: Text
  , issueType :: Issues.IssueType
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
  , issues :: [IssueStat]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


getDataset :: Charts.MetricsData -> Widget.WidgetDataset
getDataset chartEv =
  Widget.WidgetDataset
    { source = AE.toJSON $ V.cons (AE.toJSON <$> chartEv.headers) (AE.toJSON <<$>> chartEv.dataset)
    , rowsPerMin = chartEv.rowsPerMin
    , value = Just chartEv.rowsCount
    , from = chartEv.from
    , to = chartEv.to
    , stats = chartEv.stats
    }


buildReportJson' :: Int -> Int -> Double -> Double -> V.Vector (Text, Int, Double, Int, Double) -> V.Vector (Text, Text, Text, Int, Double, Int, Double) -> V.Vector (Text, Int, Int) -> Charts.MetricsData -> Charts.MetricsData -> V.Vector (Issues.IssueId, Text, Bool, Text, Issues.IssueType) -> AE.Value
buildReportJson' totalEvents totalErrors eventsChange errorsChange spanTypeStatsDiff' endpointsPerformance slowDbQueries chartEv chartErr issues =
  let spanStatsDiff = (\(t, e, chang, dur, durChange) -> AE.object ["spanType" AE..= t, "eventCount" AE..= e, "eventChange" AE..= chang, "averageDuration" AE..= dur, "durationChange" AE..= durChange]) <$> spanTypeStatsDiff'
      perf = (\(u, m, p, d, dc, req, cc) -> AE.object ["host" AE..= u, "urlPath" AE..= p, "method" AE..= m, "averageDuration" AE..= d, "durationDiffPct" AE..= dc, "requestCount" AE..= req, "requestDiffPct" AE..= cc]) <$> V.take 10 endpointsPerformance
      slowDbQueries' = (\(q, d, c) -> AE.object ["query" AE..= q, "averageDuration" AE..= d, "totalEvents" AE..= c]) <$> slowDbQueries
   in AE.object
        [ "endpoints" AE..= perf
        , "events" AE..= AE.object ["total" AE..= totalEvents, "change" AE..= errorsChange]
        , "errors" AE..= AE.object ["total" AE..= totalErrors, "change" AE..= eventsChange]
        , "spanTypeStats" AE..= spanStatsDiff
        , "slowDbQueries" AE..= slowDbQueries'
        , "errorDataset" AE..= (getDataset chartErr)
        , "eventsDataset" AE..= (getDataset chartEv)
        , "issues" AE..= ((\(i, t, c, s, tp) -> IssueStat i t c s tp) <$> issues)
        ]


getAnomaliesEmailTemplate :: V.Vector (Issues.IssueId, Text, Bool, Text, Issues.IssueType) -> V.Vector AE.Value
getAnomaliesEmailTemplate issues =
  V.map (\(i, t, c, s, tp) -> AE.object ["id" AE..= i, "title" AE..= t, "critical" AE..= c, "severity" AE..= s, "issueType" AE..= tp]) issues


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
  let nextUrl = if V.length reports < 20
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


data ReportsGet
  = ReportsGetMain (PageCtx (Projects.ProjectId, V.Vector Reports.ReportListItem, Maybe Text, Bool, Bool))
  | ReportsGetList Projects.ProjectId (V.Vector Reports.ReportListItem) (Maybe Text)
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
                          $ (def :: Widget.Widget){Widget.wType = WTTimeseries, Widget.unit = Just "rows", Widget.naked = Just True, Widget.dataset = Just v.eventsDataset, Widget.title = Just "Events trend", Widget.hideLegend = Just True, Widget._projectId = Just pid, Widget.standalone = Just True, Widget.yAxis = Just (def{showOnlyMaxLabel = Just True}), Widget.allowZoom = Just True, Widget.showMarkArea = Just True, Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})}
                    div_ [class_ "h-full w-1/2 border rounded-lg p-3"] do
                      div_ [class_ "flex w-full items-center justify-between"] do
                        div_ [class_ "flex text-sm items-center gap-3"] do
                          span_ [class_ "text-textWeak"] "Error trend"
                          span_ [class_ "font-semibold"] $ toHtml $ prettyPrintCount (fromIntegral v.errors.total)
                        span_ [class_ $ "text-xs" <> (if v.errors.change >= 0 then " text-textError" else " text-textSuccess ")] $ show v.errors.change <> "% from last week"
                      div_ [class_ "h-[90%]"] do
                        Widget.widget_
                          $ (def :: Widget.Widget){Widget.wType = WTTimeseries, Widget.dataset = Just v.errorDataset, Widget.unit = Just "rows", Widget.naked = Just True, Widget.title = Just "Errors trend", Widget.theme = Just "roma", Widget.hideLegend = Just True, Widget._projectId = Just pid, Widget.standalone = Just True, Widget.yAxis = Just (def{showOnlyMaxLabel = Just True}), Widget.allowZoom = Just True, Widget.showMarkArea = Just True, Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})}
                  div_ [class_ "space-y-6"] do
                    div_ [class_ "pt-3 border-t flex justify-between"] do
                      h5_ [class_ "text-sm font-medium"] "Span types overview"
                    let cats = v.spanTypeStats
                    div_ [class_ "flex items-center w-full overflow-x-auto gap-3"] do
                      forM_ cats $ \cat -> do
                        categoryCard cat.spanType cat.eventCount cat.averageDuration cat.durationChange cat.eventChange

                  div_ [class_ "space-y-6 pt-4"] do
                    div_ [class_ "pt-3 border-t flex  items-center justify-between"] do
                      h5_ [class_ "text-sm font-medium"] "Issues breakdown"
                      div_ [class_ "flex flex-col gap-2"] do
                        div_ [class_ "flex items-center gap-4 "] do
                          div_ [class_ "flex items-center gap-1"] do
                            span_ [class_ "h-3 w-3 rounded bg-fillError-strong"] pass
                            span_ [class_ "text-xs"] "Runtime errors"
                          div_ [class_ "flex items-center gap-1"] do
                            span_ [class_ "h-3 rounded w-3 bg-blue-500"] pass
                            span_ [class_ "text-xs"] "Api changes"
                          div_ [class_ "flex items-center gap-1"] do
                            span_ [class_ "h-3 w-3 rounded bg-yellow-500"] pass
                            span_ [class_ "text-xs"] "Monitor alerts"
                        let totalAnomalies = length v.issues
                            (errTotal, apiTotal, qTotal) = foldl (\(e, a, m) x -> (e + if x.issueType == Issues.RuntimeException then 1 else 0, a + if x.issueType == Issues.APIChange then 1 else 0, m + if x.issueType == Issues.QueryAlert then 1 else 0)) (0, 0, 0) v.issues
                        div_ [class_ "w-full h-3 rounded overflow-x-hidden bg-fillWeak"] do
                          when (totalAnomalies > 0) do
                            div_ [class_ "h-full bg-fillError-strong", style_ $ "width: " <> show (errTotal `div` totalAnomalies * 100) <> "%"] pass
                            div_ [class_ "h-full bg-blue-500", style_ $ "width: " <> show (apiTotal `div` totalAnomalies * 100) <> "%"] pass
                            div_ [class_ "h-full bg-yellow-500", style_ $ "width: " <> show (qTotal `div` totalAnomalies * 100) <> "%"] pass
                    div_ [class_ "flex flex-col gap-2 mt-4"] do
                      when (null v.issues) do
                        span_ [class_ "text-sm font-semibold text-textStrong"] "No new issues found for this period"
                      forM_ v.issues $ \iss -> do
                        div_ [class_ "flex items-center justify-between"] do
                          let titleCls = case iss.issueType of
                                Issues.RuntimeException -> "text-textError"
                                Issues.QueryAlert -> "text-yellow-500"
                                _ -> "text-textBrand-strong"
                          span_ [class_ $ "text-sm font-medium " <> titleCls] $ toHtml iss.title
                  -- span_ [] $ toHtml iss.severity

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


reportsPage :: Projects.ProjectId -> V.Vector Reports.ReportListItem -> Maybe Text -> Bool -> Bool -> Html ()
reportsPage pid reports nextUrl daily weekly =
  div_ [class_ "flex flex-row h-full w-full border-t"] do
    when (V.null reports) do
      div_ [class_ "flex h-full w-full justify-center items-center"] do
        div_ [class_ "flex flex-col items-center justify-center py-16 px-4"] $ do
          div_ [class_ "rounded-full flex items-center justify-center mb-3"] $ do
            faSprite_ "empty" "regular" "h-12 w-12"
          h3_ [class_ "text-xl text-textStrong mb-2"] "No reports generated for this project yet."
    unless (V.null reports) do
      div_ [class_ "w-1/3 border-r border-strokeWeak p-4 overflow-y-auto"] do
        div_ [class_ "mt-4"] do
          reportListItems pid reports nextUrl
      div_ [class_ "w-2/3 overflow-y-auto"] do
        div_ [class_ "flex h-full", id_ "detailSidebar"] do
          let h = V.head reports
          a_
            [ class_ "w-full text-center  cursor-pointer"
            , hxGet_ $ "/p/" <> pid.toText <> "/reports/" <> h.id.toText
            , hxTarget_ "#detailSidebar"
            , hxSwap_ "innerHTML"
            , hxTrigger_ "intersect once"
            ]
            do
              div_ [class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"] do
                span_ [class_ "loading loading-dots text-sm text-textWeak"] pass


-- div_ [class_ "w-5 bg-gray-200"] ""

reportListItems :: Projects.ProjectId -> V.Vector Reports.ReportListItem -> Maybe Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "space-y-4 w-full"] do
    forM_ reports $ \report -> do
      let isWeeklyData = report.reportType == "weekly"
      div_ [class_ "w-full flex flex-col border border-strokeWeak rounded-lg cursor-pointer hover:bg-fillWeaker"] do
        div_ [class_ $ "w-full"] do
          a_
            [ class_ "w-full p-4 flex justify-between hover:bg-fillHover cursor-pointer"
            , hxGet_ $ "/p/" <> pid.toText <> "/reports/" <> report.id.toText
            , hxTarget_ "#detailSidebar"
            , hxSwap_ "innerHTML"
            ]
            do
              div_ [class_ "flex flex-col grow gap-4"] do
                div_ [class_ "flex items-center w-full justify-between gap-2"] do
                  div_ [class_ $ (if isWeeklyData then "bg-fillBrand-weak" else "bg-fillWeak") <> " text-xs font-medium px-2.5 py-1 rounded-full capitalize"] $ toHtml report.reportType <> " report"
                  faSprite_ "chevron-right" "regular" "w-3 h-3"
                h4_ [class_ "font-medium text-sm flex items-center gap-2"] do
                  faSprite_ "calendar" "regular" "w-4 h-4"
                  toHtml $ formatTime defaultTimeLocale "%a, %b %d %Y" (zonedTimeToLocalTime report.createdAt)
    whenJust nextUrl \url ->
      a_ [class_ "w-full cursor-pointer block p-1 text-textBrand bg-fillBrand-weak hover:bg-fillBrand-weak text-center mb-4", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] "LOAD MORE"


renderEndpointRow :: PerformanceReport -> Html ()
renderEndpointRow endpoint = tr_ [class_ ""] do
  td_ [class_ "p-2 text-textWeak w-96 flex gap-4 items-center "] do
    span_ [class_ $ "cbadge-sm badge-" <> endpoint.method] $ toHtml $ endpoint.method
    span_ [class_ "flex-shrink-0 text-textStrong"] $ toHtml endpoint.urlPath
  td_ [class_ "p-2"] $ toHtml $ prettyPrintCount endpoint.requestCount
  td_ [class_ $ "p-2 " <> if endpoint.requestDiffPct < 0 then "text-textError" else "text-textSuccess"] $ toHtml $ show endpoint.requestDiffPct <> "%"
  td_ [class_ "p-2"] $ toHtml $ getDurationNSMS endpoint.averageDuration
  td_ [class_ $ "p-2 " <> if endpoint.durationDiffPct <= 0 then "text-textSuccess" else "text-textError"] $ toHtml $ show (durationDiffPct endpoint) <> "%"


renderEndpointsTable :: [PerformanceReport] -> Html ()
renderEndpointsTable endpoints = div_ [class_ "w-full border rounded-lg overflow-hidden"] $ table_ [class_ "table-auto w-full"] do
  thead_ [class_ "text-xs text-left text-textStrong font-medium capitalize border-b"] $ tr_ do
    th_ [class_ "p-2"] "Endpoint"
    th_ [class_ "p-2"] "Requests"
    th_ [class_ "p-2"] "Request change %"
    th_ [class_ "p-2"] "Avg. latency"
    th_ [class_ "p-2"] "Latency change %"
  tbody_ [class_ "text-sm"] $ mapM_ renderEndpointRow endpoints


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
