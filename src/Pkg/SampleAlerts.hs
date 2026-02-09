module Pkg.SampleAlerts (sampleAlert, sampleReport) where

import Data.Default (def)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Vector qualified as V
import Models.Apis.Issues (IssueType (..))
import Models.Apis.RequestDumps qualified as RD
import Pkg.Mail (NotificationAlerts (..))
import Relude


sampleAlert :: IssueType -> Text -> NotificationAlerts
sampleAlert = \case
  APIChange -> \title -> EndpointAlert ("🧪 TEST: " <> title) (V.singleton "POST /api/users") "test-hash"
  RuntimeException ->
    const
      $ RuntimeErrorAlert
        "test-123"
        def
          { RD.when = UTCTime (fromGregorian 2025 1 1) 0
          , RD.errorType = "🧪 TEST: TypeError"
          , RD.rootErrorType = "TypeError"
          , RD.message = "Sample error message for testing"
          , RD.rootErrorMessage = "Sample error"
          , RD.stackTrace = "at sampleFunction (sample.js:42:15)"
          , RD.hash = Just "test-hash-xyz"
          , RD.technology = Just RD.JsExpress
          , RD.requestMethod = Just "GET"
          , RD.requestPath = Just "/api/test"
          , RD.spanId = Just "test-span-id"
          , RD.traceId = Just "test-trace-id"
          , RD.serviceName = Just "api"
          , RD.stack = Just "at sampleFunction (sample.js:42:15)"
          }
  QueryAlert -> const $ MonitorsAlert "🧪 TEST: High Error Rate" "https://example.com/test"


sampleReport :: Text -> NotificationAlerts
sampleReport title = ReportAlert ("🧪 TEST: " <> title) "2025-01-01" "2025-01-02" 42 1250 (V.singleton ("api", 42, 1250)) "https://example.com" "https://example.com/chart.png" "https://example.com/errors.png"
