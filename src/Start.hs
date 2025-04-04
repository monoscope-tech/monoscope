module Start (
  startApp,
)
where

import Relude
import System.Server qualified as Server
import Control.Exception.Safe qualified as Safe
import OpenTelemetry.Trace



startApp :: IO ()
startApp = withTracer $ \tracer -> do 
   traceShowM "booooooooooooooooooooooooooo"
   Server.runAPItoolkit
   pure() 
   where 
    withTracer :: ((TracerOptions -> Tracer) -> IO c) -> IO c
    withTracer f =
      Safe.bracket
        initializeGlobalTracerProvider
        shutdownTracerProvider
        (\tracerProvider -> f $ makeTracer tracerProvider "your-app-name-or-subsystem")
