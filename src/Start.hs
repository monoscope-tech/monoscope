module Start (
  startApp,
)
where

import Configuration.Dotenv qualified as Dotenv
import Control.Exception.Safe qualified as Safe
import OpenTelemetry.Metric (initializeGlobalMeterProvider, shutdownMeterProvider)
import OpenTelemetry.Trace (Tracer, TracerOptions (..), getGlobalTracerProvider, initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider)
import Relude
import System.Server qualified as Server


startApp :: IO ()
startApp = do
  _ <- Safe.try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ())
  withTracer \_tracer ->
    withMeter $ getGlobalTracerProvider >>= Server.runMonoscope
  where
    withTracer :: ((TracerOptions -> Tracer) -> IO c) -> IO c
    withTracer f =
      Safe.bracket
        initializeGlobalTracerProvider
        (\tp -> void $ shutdownTracerProvider tp Nothing)
        (\tracerProvider -> f $ makeTracer tracerProvider "monoscope-server")

    -- Metric instruments live in 'Pkg.Metrics' and pull from the global
    -- meter provider lazily; init here so the global is set before the
    -- first 'histogramRecord'. Reads OTEL_METRICS_EXPORTER / interval env
    -- vars (defaults: otlp, 60s).
    withMeter :: IO c -> IO c
    withMeter action =
      Safe.bracket
        initializeGlobalMeterProvider
        (\mp -> void $ shutdownMeterProvider mp Nothing)
        (const action)
