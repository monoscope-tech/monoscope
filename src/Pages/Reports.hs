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
  reportEmail,
  ReportAnomalyType (..),
  PerformanceReport (..),
  ReportsGet (..),
  ReportsPost (..),
)
where

import Data.Aeson as Aeson (
  FromJSON,
  KeyValue ((.=)),
  ToJSON,
  Value (Object, String),
  decode,
  encode,
  object,
 )
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid (
  Html,
  ToHtml (..),
  a_,
  checked_,
  class_,
  div_,
  for_,
  h3_,
  h4_,
  h5_,
  href_,
  id_,
  img_,
  input_,
  label_,
  name_,
  p_,
  small_,
  span_,
  src_,
  style_,
  table_,
  tbody_,
  td_,
  th_,
  thead_,
  tr_,
  type_,
  value_,
 )
import Lucid.Htmx (hxGet_, hxPost_, hxSwap_, hxTrigger_)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Fields.Types (textFieldTypeToText)
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps (EndpointPerf, RequestForReport (endpointHash))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig, PageCtx (..), currProject, pageTitle, sessM)
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Text.Printf (printf)


-- import Debug.Trace (traceShow)

data PerformanceReport = PerformanceReport
  { urlPath :: Text
  , method :: Text
  , averageDuration :: Integer
  , durationDiff :: Integer
  , durationDiffType :: Text
  , durationDiffPct :: Integer
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


data ReportData = ReportData
  { endpoints :: [PerformanceReport]
  , anomalies :: [ReportAnomalyType]
  , anomaliesCount :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)


reportsPostH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders ReportsPost)
reportsPostH pid t = do
  _ <- Sessions.sessionAndProject pid
  apiKeys <- dbtToEff $ Projects.updateProjectReportNotif pid t
  addSuccessToast "Report notifications updated Successfully" Nothing
  addRespHeaders $ ReportsPost "updated"


data ReportsPost = ReportsPost Text


instance ToHtml ReportsPost where
  toHtml (ReportsPost _t) = ""
  toHtmlRaw = toHtml


singleReportGetH :: Projects.ProjectId -> Reports.ReportId -> ATAuthCtx (RespHeaders ReportsGet)
singleReportGetH pid rid = do
  (sess, project) <- Sessions.sessionAndProject pid
  report <- dbtToEff $ Reports.getReportById rid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Reports"
          }
  addRespHeaders $ ReportsGetSingle $ PageCtx bwconf (pid, report)


reportsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsGetH pid page hxRequest hxBoosted = do
  (sess, project) <- Sessions.sessionAndProject pid
  let p = toString (fromMaybe "0" page)
  let pg = fromMaybe 0 (readMaybe p :: Maybe Int)

  reports <- dbtToEff $ Reports.reportHistoryByProject pid pg
  let nextUrl = "/p/" <> show pid.unProjectId <> "/reports?page=" <> show (pg + 1)
  case (hxRequest, hxBoosted) of
    (Just "true", Nothing) -> addRespHeaders $ ReportsGetList pid reports nextUrl
    _ -> do
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess.persistentSession
              , currProject = Just project
              , pageTitle = "Reports"
              }
      addRespHeaders $ ReportsGetMain $ PageCtx bwconf (pid, reports, nextUrl, project.dailyNotif, project.weeklyNotif)


data ReportsGet
  = ReportsGetMain (PageCtx (Projects.ProjectId, Vector Reports.ReportListItem, Text, Bool, Bool))
  | ReportsGetList Projects.ProjectId (Vector Reports.ReportListItem) Text
  | ReportsGetSingle (PageCtx (Projects.ProjectId, Maybe Reports.Report))


instance ToHtml ReportsGet where
  toHtml (ReportsGetMain (PageCtx conf (pid, reports, next, daily, weekly))) = toHtml $ PageCtx conf $ reportsPage pid reports next daily weekly
  toHtml (ReportsGetList pid reports next) = toHtml $ reportListItems pid reports next
  toHtml (ReportsGetSingle (PageCtx conf (pid, report))) = toHtml $ PageCtx conf $ singleReportPage pid report
  toHtmlRaw = toHtml


singleReportPage :: Projects.ProjectId -> Maybe Reports.Report -> Html ()
singleReportPage pid report =
  div_ [class_ "mx-auto w-full flex flex-col px-16 pt-10 pb-24  overflow-y-scroll h-full"] do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center font-bold pb-4 border-b"] "Anomaly and Performance Report"
    case report of
      Just report' -> do
        div_ [class_ "mt-4 space-y-4"] do
          div_ [class_ "mx-auto rounded-lg border max-w-[1000px]"] do
            div_ [class_ "bg-gray-100 px-4 py-3 flex justify-between"] do
              h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report'.reportType <> " report"
              span_ [] $ show $ localDay (zonedTimeToLocalTime report'.createdAt)
            div_ [class_ "px-4 py-3 space-y-8"] do
              let rep_json = decode (encode report'.reportJson) :: Maybe ReportData
              case rep_json of
                Just v -> do
                  div_ [class_ "anomalies"] do
                    div_ [class_ "pb-3 border-b flex justify-between"] do
                      h5_ [class_ "font-bold"] "Anomalies"
                      div_ [class_ "flex gap-2"] do
                        span_ [class_ "text-red-500 font-medium"] $ show v.anomaliesCount
                        span_ [] "New anomalies"
                    div_ [class_ "mt-2 space-y-2"] do
                      forM_ v.anomalies $ \anomaly -> do
                        case anomaly of
                          ATEndpoint{endpointUrlPath, endpointMethod, eventsCount} -> do
                            div_ [class_ "space-x-3 border-b pb-1 flex gap-4 items-center justify-between"] do
                              div_ [class_ "flex items-center space-x-3 "] do
                                div_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                                  img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/anomalies/endpoint.svg"]
                                  span_ [] "New Endpoint"
                                small_ [] $ toHtml $ endpointMethod <> " " <> endpointUrlPath <> " "
                              small_ [] $ show eventsCount <> " requests"
                          ATShape{endpointUrlPath, endpointMethod, newUniqueFields, updatedFieldFormats, deletedFields, targetHash, eventsCount} -> do
                            div_ [class_ "border-b pb-1 flex items-center justify-between"] do
                              div_ [class_ "flex items-center space-x-3 "] do
                                div_ [class_ "inline-block font-bold text-blue-700 space-x-2 flex items-center"] do
                                  img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/anomalies/fields.svg"]
                                  span_ [] "New Request Shape"
                                div_ [class_ "flex flex-col"] do
                                  small_ [] $ toHtml $ endpointMethod <> "  " <> endpointUrlPath
                                  small_ [] $ toHtml $ "Signature: " <> targetHash
                                shapeParameterStats_ (length newUniqueFields) (length updatedFieldFormats) (length deletedFields)
                              small_ [] $ show eventsCount <> " requests"
                          ATFormat{endpointUrlPath, endpointMethod, keyPath, formatType, formatExamples, eventsCount} -> do
                            div_ [class_ "space-x-3 border-b pb-1 flex items-center justify-between"] do
                              div_ [class_ "flex items-center gap-2"] do
                                div_ [class_ "inline-block font-bold text-blue-700 space-x-2 shrink-0"] do
                                  img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/anomalies/fields.svg"]
                                  span_ [class_ "inline-block"] "Modified field"
                                small_ [] $ toHtml $ keyPath <> " in " <> endpointMethod <> "  " <> endpointUrlPath
                                div_ [class_ "text-sm"] do
                                  div_ [] do
                                    small_ "current format: "
                                    span_ $ toHtml $ textFieldTypeToText formatType
                                  div_ do
                                    small_ "previous formats: "
                                    span_ "" -- TODO: Should be comma separated list of formats for that field.
                                  div_ do
                                    small_ "examples: "
                                    small_ $ toHtml $ T.intercalate ", " formatExamples
                              small_ [] $ show eventsCount <> " requests"
                          ATRuntimeException _ -> do
                            pass

                  div_ [] do
                    div_ [class_ "pb-3 border-b flex justify-between"] do
                      h5_ [class_ "font-bold"] "Performance"
                      div_ [class_ "flex gap-2"] do
                        span_ [class_ "font-medium"] $ show (length v.endpoints)
                        span_ [] "affected endpoints"
                    renderEndpointsTable v.endpoints
                Nothing -> pass
      Nothing -> do
        h3_ [] "Report Not Found"


shapeParameterStats_ :: Int -> Int -> Int -> Html ()
shapeParameterStats_ newF deletedF updatedFF = div_ [class_ "inline-block"] do
  div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs"] do
    div_ [class_ "p-2 py-1 bg-emerald-100 text-emerald-900 border border-emerald-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show newF
      small_ [class_ "block"] "new fields"
    div_ [class_ " p-2 py-1 bg-slate-100 text-slate-900 border border-slate-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show updatedFF
      small_ [class_ "block"] "updated fields"
    div_ [class_ "p-2  py-1  bg-rose-100 text-rose-900 border border-rose-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show deletedF
      small_ [class_ "block"] "deleted fields"


reportsPage :: Projects.ProjectId -> Vector Reports.ReportListItem -> Text -> Bool -> Bool -> Html ()
reportsPage pid reports nextUrl daily weekly =
  div_ [class_ "mx-auto w-full flex flex-col px-16 pt-10 pb-24  overflow-y-scroll h-full"] do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center font-bold pb-4 border-b"] "Reports History"
    div_ [class_ "mt-4 grid grid-cols-12 gap-4"] do
      --   div_ [class_ "flex flex-col col-span-2 border h-max rounded-lg overflow-hidden mt-16"] do
      --     h5_ [class_ "text-lg font-semibold text-slate-700 pb-1 bg-gray-200 p-2"] "Email notifications"
      --     div_ [class_ "p-2 flex items-center justify-between w-full hover:bg-gray-100"] do
      --       label_ [class_ "inline-flex items-center w-full", Lucid.for_ "e-daily"] "Daily reports"
      --       input_
      --         [ type_ "checkbox"
      --         , id_ "e-daily"
      --         , name_ "daily-reports"
      --         , if daily then checked_ else value_ "off"
      --         , hxPost_ $ "/p/" <> show pid.unProjectId <> "/reports_notif/daily"
      --         , hxTrigger_ "change"
      --         , class_ "checkbox checkbox-success checkbox-sm"
      --         ]
      --     div_ [class_ "p-2 flex items-center justify-between w-full hover:bg-gray-100"] do
      --       label_ [class_ "inline-flex items-center w-full", Lucid.for_ "e-weekly"] "Weekly reports"
      --       input_
      --         [ type_ "checkbox"
      --         , id_ "e-weekly"
      --         , name_ "weekly-reports"
      --         , if weekly then checked_ else value_ "off"
      --         , hxPost_ $ "/p/" <> show pid.unProjectId <> "/reports_notif/weekly"
      --         , hxTrigger_ "change"
      --         , class_ "checkbox checkbox-success checkbox-sm"
      --         ]
      div_ [class_ "col-span-8"] do
        reportListItems pid reports nextUrl


reportListItems :: Projects.ProjectId -> Vector Reports.ReportListItem -> Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "space-y-4"] do
    forM_ reports $ \report -> do
      -- Renders Conditionally...
      -- Only if reportType matches a condition
      -- putStrLn report.reportType

      when (report.reportType == "weekly") $ do
        div_ [class_ "mx-auto rounded-lg border max-w-[1000px]"] do
          a_ [class_ "bg-gray-100 px-4 py-3 flex justify-between", href_ $ "/p/" <> show pid.unProjectId <> "/reports/" <> show report.id.reportId] do
            h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report.reportType <> " report"
            span_ [] $ show $ localDay (zonedTimeToLocalTime report.createdAt)
          div_ [class_ "px-4 py-3 space-y-8"] pass
    if length reports < 20
      then pass
      else a_ [class_ "max-w-[800px] mx-auto cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ nextUrl] "LOAD MORE"


renderEndpointRow :: PerformanceReport -> Html ()
renderEndpointRow endpoint = tr_ do
  let (pcls, prc) =
        if endpoint.durationDiffPct > 0
          then ("text-red-500" :: Text, "+" <> show (durationDiffPct endpoint) <> "%" :: Text)
          else ("text-green-500", show (durationDiffPct endpoint) <> "%")
  let avg_dur_ms = (fromInteger (round $ ((fromInteger endpoint.averageDuration :: Double) / 1000000.0) * 100) :: Double) / 100
  let dur_diff_ms = (fromInteger (round $ ((fromInteger endpoint.durationDiff :: Double) / 1000000.0) * 100) :: Double) / 100
  td_ [class_ "px-6 py-2 border-b text-gray-500 text-sm"] $ toHtml $ method endpoint <> " " <> urlPath endpoint
  td_ [class_ "px-6 py-2 border-b text-gray-500 text-sm"] $ show avg_dur_ms <> "ms"
  td_ [class_ "px-6 py-2 border-b text-gray-500 text-sm"] $ show dur_diff_ms <> "ms"
  td_ [class_ $ "px-6 py-2 border-b " <> pcls] $ toHtml prc


renderEndpointsTable :: [PerformanceReport] -> Html ()
renderEndpointsTable endpoints = table_ [class_ "table-auto w-full"] do
  thead_ [class_ "text-xs text-left text-gray-700 uppercase bg-gray-100"] $ tr_ do
    th_ [class_ "px-6 py-3"] "Endpoint"
    th_ [class_ "px-6 py-3"] "Average latency"
    th_ [class_ "px-6 py-3"] "Change compared to prev."
    th_ [class_ "px-6 py-3"] "latency change %"
  tbody_ $ mapM_ renderEndpointRow endpoints


buildReportJSON :: Vector Anomalies.IssueL -> Vector RequestForReport -> Vector EndpointPerf -> Aeson.Value
buildReportJSON anomalies endpoints_perf previous_perf =
  let anomalies_json = buildAnomalyJSON anomalies (length anomalies)
      perf_insight = getPerformanceInsight endpoints_perf previous_perf
      perf_json = buildPerformanceJSON perf_insight
      report_json = case anomalies_json of
        Aeson.Object va -> case perf_json of
          Aeson.Object vp -> Aeson.Object (vp <> va)
          _ -> Aeson.object []
        _ -> Aeson.object []
   in report_json


buildPerformanceJSON :: V.Vector PerformanceReport -> Aeson.Value
buildPerformanceJSON pr = Aeson.object ["endpoints" .= pr]


buildAnomalyJSON :: Vector Anomalies.IssueL -> Int -> Aeson.Value
buildAnomalyJSON anomalies total = Aeson.object ["anomalies" .= V.map buildjson anomalies, "anomaliesCount" .= total]
  where
    buildjson :: Anomalies.IssueL -> Aeson.Value
    buildjson an = case an.issueData of
      Anomalies.IDNewEndpointIssue e ->
        Aeson.object
          [ "endpointUrlPath" .= e.endpointUrlPath
          , "endpointMethod" .= e.endpointMethod
          , "tag" .= Anomalies.ATEndpoint
          , "eventsCount" .= an.eventsAgg.count
          ]
      Anomalies.IDNewShapeIssue s ->
        Aeson.object
          [ "endpointUrlPath" .= s.endpointUrlPath
          , "endpointMethod" .= s.endpointMethod
          , "targetHash" .= an.targetHash
          , "tag" .= Anomalies.ATShape
          , "newUniqueFields" .= s.newUniqueFields
          , "updatedFieldFormats" .= s.updatedFieldFormats
          , "deletedFields" .= s.deletedFields
          , "eventsCount" .= an.eventsAgg.count
          ]
      Anomalies.IDNewFormatIssue f ->
        Aeson.object
          [ "endpointUrlPath" .= f.endpointUrlPath
          , "endpointMethod" .= f.endpointMethod
          , "keyPath" .= f.fieldKeyPath
          , "tag" .= Anomalies.ATFormat
          , "formatType" .= f.formatType
          , "formatExamples" .= f.examples
          , "eventsCount" .= an.eventsAgg.count
          ]
      _ -> Aeson.object ["anomaly_type" .= String "unknown"]


getAnomaliesEmailTemplate :: Vector Anomalies.IssueL -> Vector Value
getAnomaliesEmailTemplate anomalies = buildEmailjson <$> anomalies
  where
    buildEmailjson :: Anomalies.IssueL -> Value
    buildEmailjson an = case an.issueData of
      Anomalies.IDNewEndpointIssue e ->
        Aeson.object
          [ "tag" .= "ATEndpoint"
          , "title" .= "New Endpoint"
          , "eventsCount" .= an.eventsAgg.count
          , "endpointMethod" .= e.endpointMethod
          , "endpointUrlPath" .= e.endpointUrlPath
          , "firstSeen" .= formatUTC an.eventsAgg.lastSeen
          ]
      Anomalies.IDNewShapeIssue s ->
        Aeson.object
          [ "tag" .= "ATShape"
          , "title" .= "New Request Shape"
          , "eventsCount" .= an.eventsAgg.count
          , "deletedFields" .= length s.deletedFields
          , "endpointMethod" .= s.endpointMethod
          , "endpointUrlPath" .= s.endpointUrlPath
          , "newUniqueFields" .= length s.newUniqueFields
          , "updatedFields" .= length s.updatedFieldFormats
          , "firstSeen" .= formatUTC an.eventsAgg.lastSeen
          ]
      Anomalies.IDNewFormatIssue f ->
        Aeson.object
          [ "tag" .= "ATFormat"
          , "title" .= "Modified Field"
          , "eventsCount" .= an.eventsAgg.count
          , "keyPath" .= f.fieldKeyPath
          , "formatType" .= f.formatType
          , "endpointMethod" .= f.endpointMethod
          , "endpointUrlPath" .= f.endpointUrlPath
          , "formatExamples" .= f.examples
          , "firstSeen" .= formatUTC an.eventsAgg.lastSeen
          ]
      Anomalies.IDNewRuntimeExceptionIssue e ->
        Aeson.object
          [ "tag" .= "ATError"
          , "title" .= e.errorType
          , "eventsCount" .= an.eventsAgg.count
          , "errorMessage" .= e.message
          , "endpointMethod" .= e.requestMethod
          , "endpointUrlPath" .= e.requestPath
          , "firstSeen" .= formatUTC an.eventsAgg.lastSeen
          ]
      _ -> Aeson.object ["message" .= String "unknown"]


formatUTC :: UTCTime -> String
formatUTC = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"


getPerformanceInsight :: V.Vector RequestDumps.RequestForReport -> V.Vector RequestDumps.EndpointPerf -> V.Vector PerformanceReport
getPerformanceInsight req_dumps previous_p =
  let prMap = Map.fromList [(p.endpointHash, p.averageDuration) | p <- V.toList previous_p]
      pin = V.map (mapFunc prMap) req_dumps
      perfInfo = V.filter (\x -> x.durationDiffPct > 15 || x.durationDiffPct < -15) pin
   in perfInfo


getPerformanceEmailTemplate :: V.Vector RequestDumps.RequestForReport -> V.Vector RequestDumps.EndpointPerf -> V.Vector Value
getPerformanceEmailTemplate pr previous_p =
  ( \p ->
      Aeson.object
        [ "endpointUrlPath" .= p.urlPath
        , "endpointMethod" .= p.method
        , "averageLatency" .= getMs p.averageDuration
        , "latencyChange" .= ((if p.durationDiff > 0 then "+" else "") <> getMs p.durationDiff <> " (" <> getDesc p.durationDiffPct <> ")")
        ]
  )
    <$> getPerformanceInsight pr previous_p
  where
    getMs :: Integer -> String
    getMs val = msText
      where
        dbo = divideIntegers val 1000000
        msText = printf "%.2fms" dbo
    getDesc :: Integer -> String
    getDesc x = if x > 0 then show x <> "% slower" else show (x * (-1)) <> "% faster"


mapFunc :: Map.Map Text Integer -> RequestDumps.RequestForReport -> PerformanceReport
mapFunc prMap rd =
  case Map.lookup rd.endpointHash prMap of
    Just prevDuration ->
      let diff = rd.averageDuration - prevDuration
          diffPct = round $ divideIntegers diff prevDuration * 100
          diffType = if diff >= 0 then "up" else "down"
       in PerformanceReport
            { urlPath = rd.urlPath
            , method = rd.method
            , averageDuration = rd.averageDuration
            , durationDiff = diff
            , durationDiffPct = diffPct
            , durationDiffType = diffType
            }
    Nothing ->
      PerformanceReport
        { urlPath = rd.urlPath
        , method = rd.method
        , averageDuration = rd.averageDuration
        , durationDiff = 0
        , durationDiffPct = 0
        , durationDiffType = "up"
        }


divideIntegers :: Integer -> Integer -> Double
divideIntegers a b = fromIntegral a / fromIntegral b


-- createEndpointMap [] mp = mp
-- createEndpointMap (x : xs) mp =
--   case x.anomalyType of
--     Anomalies.ATEndpoint ->
--       let ep_url = fromMaybe "" x.endpointUrlPath
--           method = fromMaybe "" x.endpointMethod
--           endpoint = method <> ep_url
--        in createEndpointMap xs (Map.insert endpoint True mp)
--     _ -> createEndpointMap xs mp

reportEmail :: Projects.ProjectId -> Reports.Report -> Html ()
reportEmail pid report' =
  div_ [style_ "margin-top: 1rem; color: black"] do
    div_ [style_ "margin: 0 auto; border-radius: 0.375rem; border: 1px solid #e5e7eb; max-width: 800px;"] do
      div_ [style_ "background-color: #f3f4f6; padding: 8px 10px;"] do
        h4_ [style_ "font-size: 1.5rem; font-weight: bold; text-transform: capitalize; margin-bottom: 5px"] $ toHtml report'.reportType <> " report"
        p_ [style_ ""] $ show $ localDay (zonedTimeToLocalTime report'.createdAt)
        a_
          [ href_ $ "https://app.apitoolkit.io/p/" <> show pid.unProjectId <> "/reports/" <> show report'.id.reportId
          , style_ "background-color:#3b82f6; margin-top:20px; text-decoration: none; padding: .5em 1em; color: #FCFDFF; display:inline-block; border-radius:.4em; mso-padding-alt:0;text-underline-color:#005959"
          ]
          "View in browser"
      div_ [style_ "padding: 1rem 1rem 2rem; gap: 2rem;"] do
        let rep_json = decode (encode report'.reportJson) :: Maybe ReportData
        case rep_json of
          Just v -> do
            div_ [style_ "margin-bottom: 2rem;"] do
              div_ [style_ "width:100%;border-bottom: 1px solid #e5e7eb; padding-bottom: 0.5rem;"] do
                h5_ [style_ "font-weight: bold; font-size: 18px"] "Anomalies"
                div_ [style_ "display: inline;"] do
                  span_ [style_ "color: #FF0000; font-weight: medium; margin-right: 0.5rem"] do
                    if v.anomaliesCount < 11 then show v.anomaliesCount else "10+"
                  span_ [style_ ""] "New anomalies"
              div_ [style_ "margin-top: 1rem;"] do
                forM_ v.anomalies $ \anomaly -> do
                  case anomaly of
                    ATEndpoint{endpointUrlPath, endpointMethod, eventsCount} -> do
                      div_ [style_ "border-bottom: 1px solid #e5e7eb; margin-bottom: 1rem; padding-bottom: 0.25rem;"] do
                        div_ [style_ "display: inline;"] do
                          span_ [style_ "display: inline; font-weight: bold; color: #3b82f6; margin-right:10px"] "New Endpoint"
                          p_ [style_ ""] $ toHtml $ endpointMethod <> " " <> endpointUrlPath <> " "
                        p_ [style_ ""] $ show eventsCount <> " requests"
                    ATShape{endpointUrlPath, endpointMethod, newUniqueFields, updatedFieldFormats, deletedFields, targetHash, eventsCount} -> do
                      div_ [style_ "border-bottom: 1px solid #e5e7eb; margin-bottom: 1rem; padding-bottom: 0.25rem;"] do
                        div_ [] do
                          span_ [style_ "display: inline; font-weight: bold; color: #3b82f6; margin-right:5px"] "New Request Shape"
                          div_ [style_ "display: flex; flex-direction: column; margin-right:5px"] do
                            small_ [style_ ""] $ toHtml $ endpointMethod <> "  " <> endpointUrlPath
                          -- Assuming shapeParameterStats_ is a custom function for displaying stats.
                          p_ [style_ "display:block"] $ show (length newUniqueFields) <> " new fields"
                          if not (null updatedFieldFormats)
                            then do
                              p_ [style_ "display:block"] $ show (length updatedFieldFormats) <> " updated fields"
                            else pass
                          if not (null updatedFieldFormats)
                            then do
                              p_ [style_ "display:block"] $ show (length deletedFields) <> " deleted fields"
                            else pass
                          p_ [style_ ""] $ show eventsCount <> " requests"
                    ATFormat{endpointUrlPath, endpointMethod, keyPath, formatType, formatExamples, eventsCount} -> do
                      div_ [style_ "border-bottom: 1px solid #e5e7eb;  margin-bottom: 1rem; padding-bottom: 0.25rem; display: flex; gap: 0.75rem; align-items: center; justify-content: space-between;"] do
                        div_ [style_ "display: flex; align-items: center;"] do
                          span_ [style_ "display: inline; font-weight: bold; color: #3b82f6;"] "Modified field"
                          small_ [style_ ""] $ toHtml $ keyPath <> " in " <> endpointMethod <> "  " <> endpointUrlPath
                        div_ [style_ "font-size: 0.875rem;"] do
                          small_ [style_ ""] "current format: "
                          span_ [style_ "display: inline;"] $ toHtml $ textFieldTypeToText formatType
                        -- div_ [style_ ""] do
                        --   small_ [style_ ""] "previous formats: "
                        --   span_ [style_ "display: inline;"] ""  TODO: Should be a comma-separated list of formats for that field.
                        div_ [style_ ""] do
                          small_ [style_ ""] "examples: "
                          small_ [style_ ""] $ toHtml $ T.intercalate ", " formatExamples
                        p_ [style_ ""] $ show eventsCount <> " requests"
                    ATRuntimeException _ -> pass

            div_ [style_ "width: 100%"] do
              div_ [style_ "width:100%; border-bottom: 1px solid #e5e7eb; padding-bottom: 0.5rem; display:inline"] do
                h5_ [style_ "font-weight: bold; font-size:18px; margin-bottom: 1rem"] "Performance"
                renderEmailEndpointsTable v.endpoints
          Nothing -> pass
      a_ [href_ $ "https://app.apitoolkit.io/p/" <> show pid.unProjectId <> "/reports", style_ "width: 100%; text-align: center; color:#3b82f6; margin: 20px; padding-bottom:20px"] "Turn off email alerts"
      div_ [style_ "margin-top: 20px"] pass


renderEmailEndpointsTable :: [PerformanceReport] -> Html ()
renderEmailEndpointsTable endpoints = table_ [style_ "width: 100%; border-collapse: collapse;"] do
  thead_ [style_ "text-align: left; text-transform: uppercase; font-size:12px; background-color: #f3f4f6;"] $ tr_ do
    th_ [style_ "padding: 0.75rem 1.5rem;"] "Endpoint"
    th_ [style_ "padding: 0.75rem 1.5rem;"] "Average latency"
    th_ [style_ "padding: 0.75rem 1.5rem;"] "Change compared to prev."
    th_ [style_ "padding: 0.75rem 1.5rem;"] "Latency change %"
  tbody_ $ mapM_ renderEndpointRow endpoints
