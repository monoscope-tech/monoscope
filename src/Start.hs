module Start (
  startApp,
)
where

import Control.Exception.Safe qualified as Safe
import OpenTelemetry.Trace (Tracer, TracerOptions (..), TracerProvider, getGlobalTracerProvider, initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider)
import Relude
import System.Server qualified as Server


startApp :: IO ()
startApp = withTracer $ \tracer -> do
  tp <- getGlobalTracerProvider
  Server.runAPItoolkit tp
  pure ()
  where
    withTracer :: ((TracerOptions -> Tracer) -> IO c) -> IO c
    withTracer f =
      Safe.bracket
        initializeGlobalTracerProvider
        shutdownTracerProvider
        ( \tracerProvider -> do
            f $ makeTracer tracerProvider "apitoolkit-server"
        )
