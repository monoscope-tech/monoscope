module Start (
  startApp,
)
where

import Configuration.Dotenv qualified as Dotenv
import Control.Exception.Safe qualified as Safe
import OpenTelemetry.Trace (Tracer, TracerOptions (..), getGlobalTracerProvider, initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider)
import Relude
import System.Server qualified as Server


startApp :: IO ()
startApp = do
  _ <- Safe.try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ())
  withTracer $ \tracer -> do
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
