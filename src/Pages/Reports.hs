{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Reports (
  reportsGetH,
  singleReportGetH,
  buildReportJSON,
  buildPerformanceJSON,
  buildAnomalyJSON,
  getPerformanceInsight,
  ReportAnomalyType (..),
  PerformanceReport (..),
) where

import Config
import Data.Default (def)

import Database.PostgreSQL.Entity.DBT (withPool)

import Data.Aeson as Aeson
import Data.Map.Strict qualified as Map
import Data.Vector (Vector)
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Relude

import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.Vector qualified as V
import Models.Apis.Reports qualified as Reports

import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Fields qualified as Field

import Data.Text qualified as T
import Data.UUID.V4 qualified as UUIDV4
import Models.Apis.RequestDumps (EndpointPerf, RequestForReport (endpointHash))

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
  | -- | ATField
    --     { endpointUrlPath :: Text
    --     , endpointMethod :: Text
    --     , fieldKey :: Text
    --     , fieldKeyPath :: Text
    --     , fieldCategory :: Text
    --     , fieldFormat :: Text
    --     , eventsCount :: Int
    --     }
    ATFormat
      { endpointUrlPath :: Text
      , fieldKeyPath :: Text
      , endpointMethod :: Text
      , formatType :: Text
      , formatExamples :: [Text]
      , eventsCount :: Int
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

singleReportGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Reports.ReportId -> DashboardM (Html ())
singleReportGetH sess pid rid = do
  pool <- asks pool
  (project, report) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      report <- Reports.getReportById rid
      pure (project, report)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Reports"
          }
  pure $ bodyWrapper bwconf $ singleReportPage pid report

reportsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
reportsGetH sess pid page hxRequest hxBoosted = do
  print page
  let p = toString (fromMaybe "0" page)
  let pg = fromMaybe 0 (readMaybe p :: Maybe Int)
  pool <- asks pool
  (project, reports) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      reports <- Reports.reportHistoryByProject pid pg
      anomalies <- Anomalies.getReportAnomalies pid "weekly"
      count <- Anomalies.countAnomalies pid "weekly"
      endpoint_rp <- RequestDumps.getRequestDumpForReports pid "weekly"
      previous_p <- RequestDumps.getRequestDumpsForPreviousReportPeriod pid "weekly"
      let rep_json = buildReportJSON anomalies count endpoint_rp previous_p
      currentTime <- liftIO getZonedTime
      reportId <- Reports.ReportId <$> liftIO UUIDV4.nextRandom
      let report =
            Reports.Report
              { id = reportId
              , reportJson = rep_json
              , createdAt = currentTime
              , updatedAt = currentTime
              , projectId = pid
              , reportType = "weekly"
              }
      Reports.addReport report
      pure (project, reports)
  let nextUrl = "/p/" <> show pid.unProjectId <> "/reports?page=" <> show (pg + 1)
  case (hxRequest, hxBoosted) of
    (Just "true", Nothing) -> pure $ do
      reportListItems pid reports nextUrl
    _ -> do
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "API Log Explorer"
              }
      pure $ bodyWrapper bwconf $ reportsPage pid reports nextUrl

singleReportPage :: Projects.ProjectId -> Maybe Reports.Report -> Html ()
singleReportPage pid report =
  div_ [class_ "container mx-auto w-full flex flex-col px-4 pt-10 pb-24"] $ do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center font-bold pb-4 border-b"] "Anomaly and Performance Report"
    case report of
      Just report' -> do
        div_ [class_ "mt-4 space-y-4"] do
          div_ [class_ "mx-auto rounded-lg border max-w-[800px]"] $ do
            div_ [class_ "bg-gray-100 px-4 py-3 flex justify-between"] $ do
              h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report'.reportType <> " report"
              span_ [] $ show $ localDay (zonedTimeToLocalTime report'.createdAt)
            div_ [class_ "px-4 py-3 space-y-8"] $ do
              let rep_json = decode (encode report'.reportJson) :: Maybe ReportData
              case rep_json of
                Just v -> do
                  div_ [class_ "anomalies"] $ do
                    div_ [class_ "pb-3 border-b flex justify-between"] $ do
                      h5_ [class_ "font-bold"] "Anomalies"
                      div_ [class_ "flex gap-2"] do
                        span_ [class_ "text-red-500 font-medium"] $ show v.anomaliesCount
                        span_ [] "New anomalies"
                    div_ [class_ "mt-2 space-y-2"] $ do
                      forM_ v.anomalies $ \anomaly -> do
                        case anomaly of
                          ATEndpoint{endpointUrlPath, endpointMethod, eventsCount} -> do
                            div_ [class_ "space-x-3 border-b pb-1 flex gap-4 items-center justify-between"] do
                              div_ [class_ "flex items-center space-x-3 "] do
                                div_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                                  img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/endpoint.svg"]
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
                          ATFormat{endpointUrlPath, endpointMethod, fieldKeyPath, formatType, formatExamples, eventsCount} -> do
                            div_ [class_ "space-x-3 border-b pb-1 flex items-center justify-between"] do
                              div_ [class_ "flex items-center"] do
                                div_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                                  img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/anomalies/fields.svg"]
                                  span_ [] "Modified field"
                                small_ [] $ toHtml $ fieldKeyPath <> " in " <> endpointMethod <> "  " <> endpointUrlPath
                                div_ [class_ "text-sm"] do
                                  div_ [] do
                                    small_ "current format: "
                                    span_ $ toHtml formatType
                                  div_ do
                                    small_ "previous formats: "
                                    span_ "" -- TODO: Should be comma separated list of formats for that field.
                                  div_ do
                                    small_ "examples: "
                                    small_ $ toHtml $ T.intercalate ", " formatExamples
                              small_ [] $ show eventsCount <> " requests"
                          _ -> pass

                  div_ [] $ do
                    div_ [class_ "pb-3 border-b flex justify-between"] $ do
                      h5_ [class_ "font-bold"] "Performance"
                      div_ [class_ "flex gap-2"] do
                        span_ [class_ "font-medium"] $ show (length v.endpoints)
                        span_ [] "affected endpoints"
                    renderEndpointsTable (v.endpoints)
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

reportsPage :: Projects.ProjectId -> Vector Reports.ReportListItem -> Text -> Html ()
reportsPage pid reports nextUrl =
  div_ [class_ "container mx-auto w-full flex flex-col px-4 pt-10 pb-24"] $ do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center font-bold pb-4 border-b"] "Report History"
    div_ [class_ "mt-4"] $ do
      reportListItems pid reports nextUrl

reportListItems :: Projects.ProjectId -> Vector Reports.ReportListItem -> Text -> Html ()
reportListItems pid reports nextUrl =
  div_ [class_ "space-y-4"] do
    forM_ reports $ \report -> do
      div_ [class_ "mx-auto rounded-lg border max-w-[800px]"] $ do
        a_ [class_ "bg-gray-100 px-4 py-3 flex justify-between", href_ $ "/p/" <> show pid.unProjectId <> "/reports/" <> show report.id.reportId] $ do
          h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report.reportType <> " report"
          span_ [] $ show $ localDay (zonedTimeToLocalTime report.createdAt)
        div_ [class_ "px-4 py-3 space-y-8"] pass
    if length reports < 20
      then pass
      else a_ [class_ "max-w-[800px] mx-auto cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ nextUrl] "LOAD MORE"

renderEndpointRow :: PerformanceReport -> Html ()
renderEndpointRow endpoint = tr_ $ do
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
renderEndpointsTable endpoints = table_ [class_ "table-auto w-full"] $ do
  thead_ [class_ "text-xs text-left text-gray-700 uppercase bg-gray-100"] $ tr_ $ do
    th_ [class_ "px-6 py-3"] "Endpoint"
    th_ [class_ "px-6 py-3"] "Average duration"
    th_ [class_ "px-6 py-3"] "Diff compared to prev."
    th_ [class_ "px-6 py-3"] "Duration diff %"
  tbody_ $ mapM_ renderEndpointRow endpoints

buildReportJSON :: Vector Anomalies.AnomalyVM -> Int -> Vector RequestForReport -> Vector EndpointPerf -> Aeson.Value
buildReportJSON anomalies anm_total endpoints_perf previous_perf =
  let anomalies_json = buildAnomalyJSON anomalies anm_total
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

buildAnomalyJSON :: Vector Anomalies.AnomalyVM -> Int -> Aeson.Value
buildAnomalyJSON anomalies total = Aeson.object ["anomalies" .= V.map buildjson filteredAnom, "anomaliesCount" .= total]
 where
  endMap = createEndpointMap (V.toList anomalies) Map.empty
  filteredAnom = V.filter (filterFunc endMap) anomalies

  filterFunc :: Map Text Bool -> Anomalies.AnomalyVM -> Bool
  filterFunc mp a = case a.anomalyType of
    Anomalies.ATEndpoint -> True
    _ ->
      let ep_url = fromMaybe "" a.endpointUrlPath
          method = fromMaybe "" a.endpointMethod
          endpoint = method <> ep_url
       in fromMaybe False (Map.lookup endpoint mp)

  buildjson :: Anomalies.AnomalyVM -> Aeson.Value
  buildjson an = case an.anomalyType of
    Anomalies.ATEndpoint ->
      Aeson.object
        [ "endpointUrlPath" .= an.endpointUrlPath
        , "endpointMethod" .= an.endpointMethod
        , "tag" .= Anomalies.ATEndpoint
        , "eventsCount" .= an.eventsCount14d
        ]
    Anomalies.ATShape ->
      Aeson.object
        [ "endpointUrlPath" .= an.endpointUrlPath
        , "endpointMethod" .= an.endpointMethod
        , "targetHash" .= an.targetHash
        , "tag" .= Anomalies.ATShape
        , "newUniqueFields" .= an.shapeNewUniqueFields
        , "updatedFieldFormats" .= an.shapeUpdatedFieldFormats
        , "deletedFields" .= an.shapeDeletedFields
        , "eventsCount" .= an.eventsCount14d
        ]
    -- Anomalies.ATField ->
    --   Aeson.object
    --     [ "endpointUrlPath" .= an.endpointUrlPath
    --     , "endpointMethod" .= an.endpointMethod
    --     , "tag" .= Anomalies.ATField
    --     , "key" .= an.fieldKey
    --     , "fieldCategory" .= Field.fieldCategoryEnumToText (fromMaybe Field.FCRequestBody an.fieldCategory)
    --     , "fieldFormat" .= an.fieldFormat
    --     , "eventsCount" .= an.eventsCount14d
    --     ]
    Anomalies.ATFormat ->
      Aeson.object
        [ "endpointUrlPath" .= an.endpointUrlPath
        , "endpointMethod" .= an.endpointMethod
        , "keyPath" .= an.fieldKeyPath
        , "tag" .= Anomalies.ATFormat
        , "formatType" .= an.formatType
        , "formatExamples" .= an.formatExamples
        , "eventsCount" .= an.eventsCount14d
        ]
    _ -> Aeson.object ["anomaly_type" .= String "unknown"]

getPerformanceInsight :: V.Vector RequestDumps.RequestForReport -> V.Vector RequestDumps.EndpointPerf -> V.Vector PerformanceReport
getPerformanceInsight req_dumps previous_p =
  let prMap = Map.fromList [(p.endpointHash, p.averageDuration) | p <- V.toList previous_p]
      pin = V.map (mapFunc prMap) req_dumps
      perfInfo = V.filter (\x -> x.durationDiffPct > 15 || x.durationDiffPct < -15) pin
   in perfInfo

mapFunc :: Map.Map Text Integer -> RequestDumps.RequestForReport -> PerformanceReport
mapFunc prMap rd =
  case Map.lookup (rd.endpointHash) prMap of
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

createEndpointMap :: [Anomalies.AnomalyVM] -> Map Text Bool -> Map Text Bool
createEndpointMap [] mp = mp
createEndpointMap (x : xs) mp =
  case x.anomalyType of
    Anomalies.ATEndpoint ->
      let ep_url = fromMaybe "" x.endpointUrlPath
          method = fromMaybe "" x.endpointMethod
          endpoint = method <> ep_url
       in createEndpointMap xs (Map.insert endpoint True mp)
    _ -> createEndpointMap xs mp