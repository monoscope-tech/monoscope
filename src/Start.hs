{-# LANGUAGE OverloadedLabels #-}

module Start (
  startApp,
)
where

import Network.GRPC.Common
import Network.GRPC.Common.Compression qualified as Compression
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Server.Run
import Network.GRPC.Server.StreamType
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService
import Relude
import System.Server qualified as Server


-- startApp :: IO ()
-- startApp = Server.runAPItoolkit

startApp :: IO ()
startApp = do
  runServerWithHandlers (serverParams) serverConfig (fromServices $ services)


services
  :: Services
      IO
      ['[LogsService]]
services =
  Service handlers $
    NoMoreServices


handlers :: Methods IO '[LogsService]
handlers =
  Method (mkNonStreaming handleExport) $
    NoMoreMethods


handleExport :: Proto ExportLogsServiceRequest -> IO (Proto ExportLogsServiceResponse)
handleExport req = do
  return $ defMessage & #partialSuccess .~ Nothing
