{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module System.Logging (
  makeLogger,
  runLog,
  logWithTrace,
  logInfo,
  logInfo_,
  logWarn,
  logError,
  logDebug,
  logTrace,
  logAttention,
  logAttention_,
  timeAction,
  LoggingDestination (..),
  getLogLevelFromEnv,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString.Char8 qualified as BS
import Data.Default (Default (..))
import Data.Text qualified as T
import Data.Time.Clock as Time (NominalDiffTime, diffUTCTime)
import Effectful (
  Eff,
  Effect,
  IOE,
  MonadUnliftIO (withRunInIO),
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Log (LogLevel (..), Logger)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Log.Internal.Logger (withLogger)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Trace.Core qualified as Trace
import OpenTelemetry.Trace.Id qualified as TraceId
import Relude hiding (span, traceId)
import System.Envy (ReadShowVar (..), Var)


data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving stock (Generic, Read, Show)
  deriving (Var) via (ReadShowVar LoggingDestination)


instance Default LoggingDestination where
  def = StdOut


-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => Text
  -> Logger
  -> Log.LogLevel
  -> Eff (Log ': es) a
  -> Eff es a
runLog envTxt logger minLogLevel = Log.runLog ("[AT]-" <> envTxt) logger minLogLevel


makeLogger :: IOE :> es => LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger StdOut = LogBulk.withBulkStdOutLogger
makeLogger Json = LogBulk.withBulkJsonStdOutLogger
makeLogger JSONFile = withJSONFileBackend FileBackendConfig{destinationFile = "logs/monoscope.json"}


newtype FileBackendConfig = FileBackendConfig
  { destinationFile :: FilePath
  }
  deriving stock (Eq, Generic, Ord, Show)


withJSONFileBackend
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => FileBackendConfig
  -> (Logger -> Eff es a)
  -> Eff es a
withJSONFileBackend FileBackendConfig{destinationFile} action = withRunInIO $ \unlift -> do
  liftIO $ BS.hPutStrLn stdout $ BS.pack $ "Redirecting logs to " <> destinationFile
  logger <- liftIO $ Log.mkLogger "file-json" $ \msg -> liftIO $ BS.appendFile destinationFile (toStrict $ AE.encode msg <> "\n")
  withLogger logger (unlift . action)


-- | Log with trace context if available
logWithTrace :: (AE.ToJSON a, IOE :> es, Log :> es) => Log.LogLevel -> Text -> a -> Eff es ()
logWithTrace level msg fields = do
  -- Get current context to extract trace information
  ctx <- liftIO Context.getContext
  traceFields <- case Context.lookupSpan ctx of
    Nothing -> pure []
    Just currentSpan -> liftIO $ do
      -- Extract SpanContext from the current span
      spanCtx <- Trace.getSpanContext currentSpan
      let tId = Trace.traceId spanCtx
          sId = Trace.spanId spanCtx
          -- Convert IDs to hex strings
          traceIdHex = decodeUtf8 $ TraceId.traceIdBaseEncodedByteString TraceId.Base16 tId
          spanIdHex = decodeUtf8 $ TraceId.spanIdBaseEncodedByteString TraceId.Base16 sId
      pure
        [ ("trace_id", AE.String traceIdHex)
        , ("span_id", AE.String spanIdHex)
        ]
  -- Convert fields to JSON value and merge with trace fields
  let fieldsValue = AE.toJSON fields
      traceFieldsKeyMap = AEKM.fromList $ map (\(k, v) -> (AEK.fromText k, v)) traceFields
      mergedObject = case fieldsValue of
        AE.Object obj -> AE.Object $ obj <> traceFieldsKeyMap
        _ -> AE.object $ (AEK.fromText "data", fieldsValue) : map (\(k, v) -> (AEK.fromText k, v)) traceFields
  Log.logMessage level msg mergedObject


-- | Convenience functions that automatically include trace context
logInfo :: (AE.ToJSON a, IOE :> es, Log :> es) => Text -> a -> Eff es ()
logInfo = logWithTrace Log.LogInfo


logWarn :: (AE.ToJSON a, IOE :> es, Log :> es) => Text -> a -> Eff es ()
logWarn = logWithTrace Log.LogAttention -- Using LogAttention for warnings


logError :: (AE.ToJSON a, IOE :> es, Log :> es) => Text -> a -> Eff es ()
logError = logWithTrace Log.LogAttention -- Using LogAttention for errors


logDebug :: (AE.ToJSON a, IOE :> es, Log :> es) => Text -> a -> Eff es ()
logDebug = logWithTrace Log.LogTrace -- Using LogTrace for debug


logTrace :: (AE.ToJSON a, IOE :> es, Log :> es) => Text -> a -> Eff es ()
logTrace = logWithTrace Log.LogTrace


logAttention :: (AE.ToJSON a, IOE :> es, Log :> es) => Text -> a -> Eff es ()
logAttention = logWithTrace Log.LogAttention


logInfo_ :: (IOE :> es, Log :> es) => Text -> Eff es ()
logInfo_ msg = logInfo msg ()


logAttention_ :: (IOE :> es, Log :> es) => Text -> Eff es ()
logAttention_ msg = logAttention msg ()


timeAction
  :: forall (es :: [Effect]) (a :: Type)
   . Time :> es
  => Eff es a
  -> Eff es (a, NominalDiffTime)
timeAction action = do
  start <- Time.currentTime
  result <- action
  end <- Time.currentTime
  pure (result, Time.diffUTCTime end start)


-- | Read log level from LOG_LEVEL environment variable
-- Defaults to LogTrace (most verbose) if not set or invalid
getLogLevelFromEnv :: IO LogLevel
getLogLevelFromEnv = do
  maybeLevel <- lookupEnv "LOG_LEVEL"
  pure $ case maybeLevel of
    Just levelStr -> case T.toLower (toText levelStr) of
      "trace" -> LogTrace
      "info" -> LogInfo
      "attention" -> LogAttention
      _ -> LogTrace -- Default to trace if invalid value
    Nothing -> LogTrace -- Default to trace if not set
