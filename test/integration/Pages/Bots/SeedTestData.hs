module Pages.Bots.SeedTestData (seedTelemetryData, cleanupTelemetryData) where

import Data.Aeson qualified as AE
import Data.Pool (withResource)
import Data.Time (UTCTime, addUTCTime)
import Database.PostgreSQL.Simple (Only (..), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pkg.TestUtils (TestResources (..))
import Relude
import Relude.Unsafe qualified as Unsafe

-- | Seed realistic telemetry data for agentic tests
-- Uses frozen time from tests (2025-01-01 00:00:00 UTC)
seedTelemetryData :: TestResources -> IO ()
seedTelemetryData tr = void $ withResource tr.trPool \conn -> do
  let baseTime = Unsafe.read "2025-01-01 00:00:00 UTC" :: UTCTime
      projectId = "00000000-0000-0000-0000-000000000000" :: Text

  -- Insert sample error logs (for "error trend" query)
  -- Last 20 hours of error data
  forM_ [0 .. 20] \i -> do
    let timestamp = addUTCTime (fromIntegral $ -3600 * i) baseTime
        body = AE.object ["message" AE..= ("Sample error message " <> show i :: Text)]

    void $
      execute
        conn
        [sql|
        INSERT INTO otel_logs_and_spans
          (project_id, timestamp, level, severity___severity_text, severity___severity_number,
           body, resource___service___name, attributes___error___type, summary)
        VALUES (?, ?, 'ERROR', 'ERROR', 17, ?, 'api-service', 'RuntimeException',
                ARRAY['error', 'api-service', 'RuntimeException'])
      |]
        (projectId, timestamp, AE.encode body)

  -- Insert sample warning logs (for "service breakdown" query)
  -- Alternate between auth-service and payment-service
  forM_ [0 .. 10] \i -> do
    let timestamp = addUTCTime (fromIntegral $ -1800 * i) baseTime
        serviceName = if even i then "auth-service" else "payment-service" :: Text
        body = AE.object ["message" AE..= ("Warning message " <> show i :: Text)]

    void $
      execute
        conn
        [sql|
        INSERT INTO otel_logs_and_spans
          (project_id, timestamp, level, severity___severity_text, severity___severity_number,
           body, resource___service___name, summary)
        VALUES (?, ?, 'WARN', 'WARN', 13, ?, ?,
                ARRAY['warning', ?::text])
      |]
        (projectId, timestamp, AE.encode body, serviceName, serviceName)

  -- Insert some INFO logs for variety
  forM_ [0 .. 5] \i -> do
    let timestamp = addUTCTime (fromIntegral $ -7200 * i) baseTime
        body = AE.object ["message" AE..= ("Info message " <> show i :: Text)]

    void $
      execute
        conn
        [sql|
        INSERT INTO otel_logs_and_spans
          (project_id, timestamp, level, severity___severity_text, severity___severity_number,
           body, resource___service___name, summary)
        VALUES (?, ?, 'INFO', 'INFO', 9, ?, 'api-service',
                ARRAY['info', 'api-service'])
      |]
        (projectId, timestamp, AE.encode body)


-- | Cleanup test data after tests
cleanupTelemetryData :: TestResources -> IO ()
cleanupTelemetryData tr = void $ withResource tr.trPool \conn ->
  execute
    conn
    [sql|
      DELETE FROM otel_logs_and_spans
      WHERE project_id = '00000000-0000-0000-0000-000000000000'
    |]
    ()
