{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pages.Reports (
  reportsGetH,
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
import Data.Time.LocalTime (LocalTime (localDay), ZonedTime (zonedTimeToLocalTime), getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector (Vector)

import Data.Vector qualified as V
import Lucid
import Models.Apis.Reports qualified as Reports
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)

import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Fields qualified as Field
import Models.Projects.Projects qualified as Projects

import Models.Apis.RequestDumps (EndpointPerf, RequestForReport)
import Models.Apis.RequestDumps qualified as RequestDumps

import Relude

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
      , newUniqueFields :: [Text]
      , updatedFieldFormats :: [Text]
      , deletedFields :: [Text]
      , eventsCount :: Int
      }
  | ATField
      { endpointUrlPath :: Text
      , endpointMethod :: Text
      , fieldKey :: Text
      , fieldKeyPath :: Text
      , fieldCategory :: Text
      , fieldFormat :: Text
      , eventsCount :: Int
      }
  | ATFormat
      { endpointUrlPath :: Text
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

reportsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
reportsGetH sess pid = do
  pool <- asks pool
  (project, reports) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      reports <- Reports.reportHistoryByProject pid
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
      -- Reports.addReport report
      pure (project, reports)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Reports"
          }
  pure $ bodyWrapper bwconf $ reportsPage pid reports

reportsPage :: Projects.ProjectId -> Vector Reports.Report -> Html ()
reportsPage pid reports =
  div_ [class_ "container mx-auto w-full flex flex-col px-4 pt-10 pb-24"] $ do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center font-bold pb-4 border-b"] "Report History"
    div_ [class_ "mt-4 space-y-4"] $ do
      forM_ reports $ \report -> do
        div_ [class_ "mx-auto rounded-lg border max-w-[800px]"] $ do
          div_ [class_ "bg-gray-100 px-4 py-3 flex justify-between"] $ do
            h4_ [class_ "text-xl font-medium capitalize"] $ toHtml report.reportType <> " report"
            span_ [] $ show $ localDay (zonedTimeToLocalTime report.createdAt)
          div_ [class_ "px-4 py-3 space-y-8"] $ do
            let end = decode (encode report.reportJson) :: Maybe ReportData
            case end of
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
                          div_ [class_ "space-x-3 border-b pb-1"] do
                            div_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                              img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/endpoint.svg"]
                              span_ [] "New Endpoint"
                            small_ [] $ toHtml $ endpointMethod <> " " <> endpointUrlPath <> " " <> show eventsCount
                        ATShape{endpointUrlPath, endpointMethod} -> do
                          div_ [class_ "space-x-3 border-b pb-1"] do
                            div_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                              img_ [class_ "inline w-4 h-4", src_ "/assets/svgs/anomalies/fields.svg"]
                              span_ [] "New Request Shape"
                            small_ [] $ toHtml $ endpointMethod <> "  " <> endpointUrlPath
                        _ -> pass
                div_ [] $ do
                  div_ [class_ "pb-3 border-b flex justify-between"] $ do
                    h5_ [class_ "font-bold"] "Performance"
                    div_ [class_ "flex gap-2"] do
                      span_ [class_ "font-medium"] $ show (length v.endpoints)
                      span_ [] "affected endpoints"
                  renderEndpointsTable (v.endpoints)
              Nothing -> pass

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
buildAnomalyJSON anomalies total = Aeson.object ["anomalies" .= V.map buildjson anomalies, "anomaliesCount" .= total]
 where
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
        , "tag" .= Anomalies.ATShape
        , "newUniqueFields" .= an.shapeNewUniqueFields
        , "updatedFieldFormats" .= an.shapeUpdatedFieldFormats
        , "deletedFields" .= an.shapeDeletedFields
        , "eventsCount" .= an.eventsCount14d
        ]
    Anomalies.ATField ->
      Aeson.object
        [ "endpointUrlPath" .= an.endpointUrlPath
        , "endpointMethod" .= an.endpointMethod
        , "tag" .= Anomalies.ATField
        , "key" .= an.fieldKey
        , "keyPath" .= an.fieldKeyPath
        , "fieldCategory" .= Field.fieldCategoryEnumToText (fromMaybe Field.FCRequestBody an.fieldCategory)
        , "fieldFormat" .= an.fieldFormat
        , "eventsCount" .= an.eventsCount14d
        ]
    Anomalies.ATFormat ->
      Aeson.object
        [ "endpointUrlPath" .= an.endpointUrlPath
        , "endpointMethod" .= an.endpointMethod
        , "tag" .= Anomalies.ATFormat
        , "formatType" .= an.formatType
        , "formatExamples" .= an.formatExamples
        , "eventsCount" .= an.eventsCount14d
        ]
    Anomalies.ATUnknown -> Aeson.object ["anomaly_type" .= String "unknown"]

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