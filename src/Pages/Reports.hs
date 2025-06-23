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

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AEP
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime))
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Fields.Types (textFieldTypeToText)
import Models.Apis.Reports qualified as Reports
import Models.Apis.RequestDumps (EndpointPerf, RequestForReport (endpointHash))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Text.Printf (printf)
import Utils (checkFreeTierExceeded, faSprite_)


data PerformanceReport = PerformanceReport
  { urlPath :: Text
  , method :: Text
  , averageDuration :: Integer
  , durationDiff :: Integer
  , durationDiffType :: Text
  , durationDiffPct :: Integer
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


data ReportData = ReportData
  { endpoints :: [PerformanceReport]
  , anomalies :: [ReportAnomalyType]
  , anomaliesCount :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON)


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
  report <- dbtToEff $ Reports.getReportById rid
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Report"
          , freeTierExceeded = freeTierExceeded
          }
  case hxRequestM of
    Just _ -> addRespHeaders $ ReportsGetSingle' (pid, report)
    _ -> addRespHeaders $ ReportsGetSingle $ PageCtx bwconf (pid, report)


reportsGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders ReportsGet)
reportsGetH pid page hxRequest hxBoosted = do
  (sess, project) <- Sessions.sessionAndProject pid
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
    h3_ [class_ "px-6 text-lg text-textStrong flex place-items-center font-medium pb-4 border-b"] "Anomaly and Performance Report"
    case report of
      Just report' -> do
        div_ [class_ "mt-4 space-y-4"] do
          div_ [class_ "mx-auto rounded-lg border max-w-[1000px]"] do
            div_ [class_ "bg-gray-100 px-4 py-3 flex justify-between"] do
              h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report'.reportType <> " report"
              span_ [] $ show $ localDay (zonedTimeToLocalTime report'.createdAt)
            div_ [class_ "px-4 py-3 space-y-8"] do
              let rep_json = AE.decode (AE.encode report'.reportJson) :: Maybe ReportData
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
                                  img_ [class_ "inline w-4 h-4", src_ "/public/assets/svgs/anomalies/endpoint.svg"]
                                  span_ [] "New Endpoint"
                                small_ [] $ toHtml $ endpointMethod <> " " <> endpointUrlPath <> " "
                              small_ [] $ show eventsCount <> " requests"
                          ATShape{endpointUrlPath, endpointMethod, newUniqueFields, updatedFieldFormats, deletedFields, targetHash, eventsCount} -> do
                            div_ [class_ "border-b pb-1 flex items-center justify-between"] do
                              div_ [class_ "flex items-center space-x-3 "] do
                                div_ [class_ "inline-block font-bold text-blue-700 space-x-2 flex items-center"] do
                                  img_ [class_ "inline w-4 h-4", src_ "/public/assets/svgs/anomalies/fields.svg"]
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
                                  img_ [class_ "inline w-4 h-4", src_ "/public/assets/svgs/anomalies/fields.svg"]
                                  span_ [class_ "inline-block"] "Modified field"
                                small_ [] $ toHtml $ keyPath <> " in " <> endpointMethod <> "  " <> endpointUrlPath
                                div_ [class_ ""] do
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
                          _ -> do
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
    div_ [class_ " p-2 py-1 bg-fillWeaker text-slate-900 border border-slate-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show updatedFF
      small_ [class_ "block"] "updated fields"
    div_ [class_ "p-2  py-1  bg-rose-100 text-rose-900 border border-rose-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show deletedF
      small_ [class_ "block"] "deleted fields"


reportsPage :: Projects.ProjectId -> V.Vector Reports.ReportListItem -> Text -> Bool -> Bool -> Html ()
reportsPage pid reports nextUrl daily weekly =
  div_ [class_ "flex flex-row h-screen bg-white"] do
    div_ [class_ "w-1/3 border-r border-gray-200 p-4 overflow-y-auto"] do
      div_ [class_ "mt-4"] do
        reportListItems pid reports nextUrl

    div_ [class_ "w-2/3 p-4 overflow-y-auto"] do
      div_ [class_ "flex items-center justify-center h-full", id_ "detailSidebar"] do
        div_ [class_ "text-center"] do
          faSprite_ "clapperboard" "light" "w-36 h-36 mx-auto"
          h3_ [class_ "text-xl font-bold mb-4 text-slate-700"] "View Each Report Details here"
          h3_ [class_ "mt-2 text-lg font-medium text-slate-900"] "But nothing is selected yet"
          p_ [class_ "mt-1  text-slate-500"] "Select a field or similar item on the left"
          p_ [class_ "mt-1  text-slate-500"] "to view more details about it here."


-- div_ [class_ "w-5 bg-gray-200"] ""

reportListItems :: Projects.ProjectId -> V.Vector Reports.ReportListItem -> Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "space-y-1 w-full"] do
    forM_ reports $ \report -> do
      let isWeeklyData = report.reportType == "weekly"

      div_ [class_ "w-full flex flex-col"] do
        div_ [class_ $ if isWeeklyData then "w-full bg-gray-100" else "w-11/12 self-end "] do
          a_
            [ class_ "w-full px-4 py-3 flex justify-between hover:bg-gray-200 cursor-pointer"
            , hxGet_ $ "/p/" <> show pid.unProjectId <> "/reports/" <> show report.id.reportId
            , hxTarget_ "#detailSidebar"
            , hxSwap_ "innerHTML"
            ]
            do
              div_ [class_ "flex flex-col grow"] do
                h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report.reportType <> " Report"
                span_ [class_ " text-gray-500"] $ toHtml $ formatTime defaultTimeLocale "%a, %b %d %Y" (zonedTimeToLocalTime report.createdAt)

              div_ [class_ "ml-4 flex items-center"] do
                i_ [class_ "fa fa-arrow-right text-gray-500"] mempty

    when (length reports < 20) $ do
      div_ [class_ "w-full h-16 center-item my-200"] do
        p_ [class_ "text-center text-blue-100"] "The End: No more report to display"

    unless (length reports < 20) $ do
      a_ [class_ "w-full cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center mb-4", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ nextUrl] "LOAD MORE"


renderEndpointRow :: PerformanceReport -> Html ()
renderEndpointRow endpoint = tr_ do
  let (pcls, prc) =
        if endpoint.durationDiffPct > 0
          then ("text-red-500" :: Text, "+" <> show (durationDiffPct endpoint) <> "%" :: Text)
          else ("text-green-500", show (durationDiffPct endpoint) <> "%")
  let avg_dur_ms = (fromInteger (round $ ((fromInteger endpoint.averageDuration :: Double) / 1000000.0) * 100) :: Double) / 100
  let dur_diff_ms = (fromInteger (round $ ((fromInteger endpoint.durationDiff :: Double) / 1000000.0) * 100) :: Double) / 100
  td_ [class_ "px-6 py-2 border-b text-gray-500 "] $ toHtml $ method endpoint <> " " <> urlPath endpoint
  td_ [class_ "px-6 py-2 border-b text-gray-500 "] $ show avg_dur_ms <> "ms"
  td_ [class_ "px-6 py-2 border-b text-gray-500 "] $ show dur_diff_ms <> "ms"
  td_ [class_ $ "px-6 py-2 border-b " <> pcls] $ toHtml prc


renderEndpointsTable :: [PerformanceReport] -> Html ()
renderEndpointsTable endpoints = table_ [class_ "table-auto w-full"] do
  thead_ [class_ "text-xs text-left text-gray-700 uppercase bg-gray-100"] $ tr_ do
    th_ [class_ "px-6 py-3"] "Endpoint"
    th_ [class_ "px-6 py-3"] "Average latency"
    th_ [class_ "px-6 py-3"] "Change compared to prev."
    th_ [class_ "px-6 py-3"] "latency change %"
  tbody_ $ mapM_ renderEndpointRow endpoints


buildReportJSON :: V.Vector Anomalies.IssueL -> V.Vector RequestForReport -> V.Vector EndpointPerf -> AE.Value
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


buildAnomalyJSON :: V.Vector Anomalies.IssueL -> Int -> AE.Value
buildAnomalyJSON anomalies total = AE.object ["anomalies" AE..= V.catMaybes (V.map buildjson anomalies), "anomaliesCount" AE..= total]
  where
    buildjson :: Anomalies.IssueL -> Maybe AE.Value
    buildjson an = case an.issueData of
      Anomalies.IDNewEndpointIssue e ->
        Just
          $ AE.object
            [ "endpointUrlPath" AE..= e.endpointUrlPath
            , "endpointMethod" AE..= e.endpointMethod
            , "tag" AE..= Anomalies.ATEndpoint
            , "eventsCount" AE..= an.eventsAgg.count
            ]
      Anomalies.IDNewShapeIssue s ->
        Just
          $ AE.object
            [ "endpointUrlPath" AE..= s.endpointUrlPath
            , "endpointMethod" AE..= s.endpointMethod
            , "targetHash" AE..= an.targetHash
            , "tag" AE..= Anomalies.ATShape
            , "newUniqueFields" AE..= s.newUniqueFields
            , "updatedFieldFormats" AE..= s.updatedFieldFormats
            , "deletedFields" AE..= s.deletedFields
            , "eventsCount" AE..= an.eventsAgg.count
            ]
      Anomalies.IDNewFormatIssue f ->
        Just
          $ AE.object
            [ "endpointUrlPath" AE..= f.endpointUrlPath
            , "endpointMethod" AE..= f.endpointMethod
            , "keyPath" AE..= f.fieldKeyPath
            , "tag" AE..= Anomalies.ATFormat
            , "formatType" AE..= f.formatType
            , "formatExamples" AE..= f.examples
            , "eventsCount" AE..= an.eventsAgg.count
            ]
      _ -> Nothing


getAnomaliesEmailTemplate :: V.Vector Anomalies.IssueL -> V.Vector AE.Value
getAnomaliesEmailTemplate anomalies = buildEmailjson <$> anomalies
  where
    buildEmailjson :: Anomalies.IssueL -> AE.Value
    buildEmailjson an = case an.issueData of
      Anomalies.IDNewEndpointIssue e ->
        AE.object
          [ "tag" AE..= "ATEndpoint"
          , "title" AE..= "New Endpoint"
          , "eventsCount" AE..= an.eventsAgg.count
          , "endpointMethod" AE..= e.endpointMethod
          , "endpointUrlPath" AE..= e.endpointUrlPath
          , "firstSeen" AE..= formatUTC an.eventsAgg.lastSeen
          ]
      Anomalies.IDNewShapeIssue s ->
        AE.object
          [ "tag" AE..= "ATShape"
          , "title" AE..= "New Request Shape"
          , "eventsCount" AE..= an.eventsAgg.count
          , "deletedFields" AE..= length s.deletedFields
          , "endpointMethod" AE..= s.endpointMethod
          , "endpointUrlPath" AE..= s.endpointUrlPath
          , "newUniqueFields" AE..= length s.newUniqueFields
          , "updatedFields" AE..= length s.updatedFieldFormats
          , "firstSeen" AE..= formatUTC an.eventsAgg.lastSeen
          ]
      Anomalies.IDNewFormatIssue f ->
        AE.object
          [ "tag" AE..= "ATFormat"
          , "title" AE..= "Modified Field"
          , "eventsCount" AE..= an.eventsAgg.count
          , "keyPath" AE..= f.fieldKeyPath
          , "formatType" AE..= f.formatType
          , "endpointMethod" AE..= f.endpointMethod
          , "endpointUrlPath" AE..= f.endpointUrlPath
          , "formatExamples" AE..= f.examples
          , "firstSeen" AE..= formatUTC an.eventsAgg.lastSeen
          ]
      Anomalies.IDNewRuntimeExceptionIssue e ->
        AE.object
          [ "tag" AE..= "ATError"
          , "title" AE..= e.errorType
          , "eventsCount" AE..= an.eventsAgg.count
          , "errorMessage" AE..= e.message
          , "endpointMethod" AE..= e.requestMethod
          , "endpointUrlPath" AE..= e.requestPath
          , "firstSeen" AE..= formatUTC an.eventsAgg.lastSeen
          ]
      _ -> AE.object ["message" AE..= AE.String "unknown"]


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
        , "latencyChange" AE..= ((if p.durationDiff > 0 then "+" else "") <> getMs p.durationDiff <> " (" <> getDesc p.durationDiffPct <> ")")
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
        let rep_json = AE.decode (AE.encode report'.reportJson) :: Maybe ReportData
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
                    _ -> pass

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
