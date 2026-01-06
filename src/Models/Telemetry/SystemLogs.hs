module Models.Telemetry.SystemLogs (
  mkSystemLog,
  insertSystemLog,
) where

import Data.Aeson qualified as AE
import Data.Effectful.UUID (UUIDEff)
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static qualified as Eff
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.SummaryGenerator (generateSummary)
import Models.Telemetry.Telemetry (Context (..), OtelLogsAndSpans (..), Severity (..), SeverityLevel (..), bulkInsertOtelLogsAndSpansTF)
import Pkg.DeriveUtils (AesonText (..), UUIDId (..))
import Relude
import System.Config (AuthContext)
import System.Types (DB)


mkSystemLog
  :: Projects.ProjectId
  -> Text -- event name (e.g., "monitor.alert.triggered")
  -> SeverityLevel
  -> Text -- body message (human-readable)
  -> Map Text AE.Value -- attributes
  -> Maybe Int64 -- optional duration in nanoseconds
  -> UTCTime
  -> OtelLogsAndSpans
mkSystemLog (UUIDId pid) eventName sev bodyMsg attrs duration ts =
  let
    (levelText, sevNum) = case sev of
      SLDebug -> ("DEBUG", 5)
      SLInfo -> ("INFO", 9)
      SLWarn -> ("WARN", 13)
      SLError -> ("ERROR", 17)
      SLFatal -> ("FATAL", 21)
    resource = Map.fromList [("service.name", AE.String "SYSTEM")]
    otelLog =
      OtelLogsAndSpans
        { id = "" -- Will be set by bulkInsertOtelLogsAndSpansTF
        , project_id = UUID.toText pid
        , timestamp = ts
        , parent_id = Nothing
        , observed_timestamp = Just ts
        , hashes = V.empty
        , name = Just eventName
        , kind = Just "log"
        , status_code = Nothing
        , status_message = Nothing
        , level = Just levelText
        , severity = Just Severity{severity_text = Just sev, severity_number = sevNum}
        , body = Just $ AesonText $ AE.String bodyMsg
        , duration = duration
        , start_time = ts
        , end_time = Nothing
        , context = Just Context{trace_id = Nothing, span_id = Nothing, trace_state = Nothing, trace_flags = Nothing, is_remote = Nothing}
        , events = Nothing
        , links = Nothing
        , attributes = if Map.null attrs then Nothing else Just (AesonText attrs)
        , resource = Just (AesonText resource)
        , summary = V.empty -- Will be generated
        , date = ts
        , errors = Nothing
        }
   in
    otelLog{summary = generateSummary otelLog}


insertSystemLog
  :: (Concurrent :> es, DB es, Eff.Reader AuthContext :> es, Labeled "timefusion" WithConnection :> es, Log :> es, UUIDEff :> es)
  => OtelLogsAndSpans
  -> Eff es ()
insertSystemLog otelLog = bulkInsertOtelLogsAndSpansTF (V.singleton otelLog)
