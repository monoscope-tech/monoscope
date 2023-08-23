{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pages.Reports (reportsGetH) where

import Config
import Data.Default (def)

import Database.PostgreSQL.Entity.DBT (withPool)

import Data.Aeson as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Time.LocalTime (getZonedTime)
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

import Data.HashMap.Lazy qualified as HM
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
      , shapeNewUniqueFields :: [Text]
      , shapeUpdatedFieldFormats :: [Text]
      , shapeDeletedFields :: [Text]
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
      anomalies <- Anomalies.getReportAnomalies pid "daily"
      count <- Anomalies.countAnomalies pid "daily"
      endpoint_rp <- RequestDumps.getRequestDumpForReports pid "daily"
      previous_day <- RequestDumps.getRequestDumpsForPreviousReportPeriod pid "daily"
      let rep_json = buildReportJSON anomalies count endpoint_rp previous_day
      -- let nm = case rep_json of
      --       Object v -> case KeyMap.lookup "endpoints" v of
      --         Just endps -> Just v
      --         _ -> Nothing
      --       _ -> Nothing
      let end = decode (encode rep_json) :: Maybe ReportData
      print end
      currentTime <- liftIO getZonedTime
      reportId <- Reports.ReportId <$> liftIO UUIDV4.nextRandom
      let report =
            Reports.Report
              { id = reportId
              , reportJson = rep_json
              , createdAt = currentTime
              , updatedAt = currentTime
              , projectId = pid
              , reportType = "daily"
              }
      Reports.addReport report
      -- print $ decode (encode rep_json)
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
  div_ [class_ "container mx-auto  px-4 pt-10 pb-24"] $ do
    h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Report History"

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
buildAnomalyJSON anomalies total = Aeson.object ["anomalies" .= V.map buildjson anomalies, "anomalies_count" .= total]
 where
  buildjson :: Anomalies.AnomalyVM -> Aeson.Value
  buildjson an = case an.anomalyType of
    Anomalies.ATEndpoint ->
      Aeson.object
        [ "path_url" .= an.endpointUrlPath
        , "method" .= an.endpointMethod
        , "type" .= Anomalies.ATEndpoint
        , "events_count" .= an.eventsCount14d
        ]
    Anomalies.ATShape ->
      Aeson.object
        [ "path_url" .= an.endpointUrlPath
        , "method" .= an.endpointMethod
        , "type" .= Anomalies.ATShape
        , "unique_field" .= an.shapeNewUniqueFields
        , "update_fields" .= an.shapeUpdatedFieldFormats
        , "deleted_field" .= an.shapeDeletedFields
        , "events_count" .= an.eventsCount14d
        ]
    Anomalies.ATField ->
      Aeson.object
        [ "path_url" .= an.endpointUrlPath
        , "method" .= an.endpointMethod
        , "type" .= Anomalies.ATField
        , "key" .= an.fieldKey
        , "key_path" .= an.fieldKeyPath
        , "field_category" .= Field.fieldCategoryEnumToText (fromMaybe Field.FCRequestBody an.fieldCategory)
        , "field_format" .= an.fieldFormat
        , "events_count" .= an.eventsCount14d
        ]
    Anomalies.ATFormat ->
      Aeson.object
        [ "path_url" .= an.endpointUrlPath
        , "method" .= an.endpointMethod
        , "type" .= Anomalies.ATFormat
        , "format_type" .= an.formatType
        , "format_examples" .= an.formatExamples
        , "events_count" .= an.eventsCount14d
        ]
    Anomalies.ATUnknown -> Aeson.object ["type" .= String "unknown"]

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
          diffPct = (diff `div` prevDuration) * 100
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