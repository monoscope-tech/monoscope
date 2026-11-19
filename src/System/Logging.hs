-- | Monoscope's logging: log-base's 'Log' effect and levels, plus a real warning level, with every
-- message tagged with the calling thread's trace and span (see 'makeLogger').
module System.Logging (
  makeLogger,
  runLog,
  Log,
  Log.logInfo,
  Log.logInfo_,
  logWarn,
  Log.logTrace,
  Log.logTrace_,
  Log.logAttention,
  Log.logAttention_,
  withError,
  timeAction,
  tolerantLogger,
  LoggingDestination (..),
)
where

import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Default (Default (..))
import Data.HashMap.Strict qualified as HM
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
import Log (LogLevel (..), Logger, showLogMessage)
import Log.Internal.Logger (Logger (..), withLogger)
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
  deriving anyclass (Default)
  deriving (Var) via (ReadShowVar LoggingDestination)


-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => Text
  -> Logger
  -> Log.LogLevel
  -> Eff (Log ': es) a
  -> Eff es a
runLog envTxt = Log.runLog ("[AT]-" <> envTxt)


makeLogger :: IOE :> es => LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger dest action = withRunInIO \unlift -> do
  logger <- case dest of
    StdOut -> bulk "stdout-bulk" (putTextLn . showLogMessage Nothing)
    Json -> bulk "stdout-bulk-json" (putLBSLn . jsonLine)
    JSONFile -> putBSLn ("Redirecting logs to " <> encodeUtf8 file) >> Log.mkLogger "file-json" (BSL.appendFile file . (<> "\n") . jsonLine)
  withLogger (tolerantLogger logger) (unlift . action)
  where
    file = "logs/monoscope.json"
    bulk name write = Log.mkBulkLogger name (\msgs -> traverse_ write msgs >> hFlush stdout) pass


-- | One JSON object per line. A message logged by 'logWarn' carries the @warn@ domain, which is
-- written as its level instead: log-base has no warning level, and ingestion reads @attention@
-- as ERROR, which files an issue, while @warn@ is WARN, which does not.
--
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Time (UTCTime (..), fromGregorian)
-- >>> let at d = AE.decode @(Map Text AE.Value) $ jsonLine (Log.LogMessage "c" d (UTCTime (fromGregorian 2026 1 1) 0) Log.LogAttention "m" AE.Null)
-- >>> [(Map.lookup "level" =<< at d, Map.lookup "domain" =<< at d) | d <- [["warn"], ["jobs"]]]
-- [(Just (String "warn"),Just (Array [])),(Just (String "attention"),Just (Array [String "jobs"]))]
jsonLine :: Log.LogMessage -> LByteString
jsonLine m
  | warnDomain `notElem` m.lmDomain = AE.encode m
  | otherwise = AE.encode case AE.toJSON m{Log.lmDomain = filter (/= warnDomain) m.lmDomain} of
      AE.Object o -> AE.Object (AEKM.insert "level" "warn" o)
      v -> v


warnDomain :: Text
warnDomain = "warn"


-- | The process's 'Logger' wrapper. Writes run on the caller's thread, so each message gets that
-- thread's trace and span ids, whichever API logged it; a non-object payload, which log-base files
-- under @__data_array@ and the like, stays under @data@. A write to a logger that has shut down is
-- swallowed: it used to escape through 'supervise', the error-logger thread and Warp's
-- onException, taking the process down.
tolerantLogger :: Logger -> Logger
tolerantLogger l = l{loggerWriteMessage = \m -> void $ Safe.tryAny $ loggerWriteMessage l . shaped m =<< traceIds}
  where
    traceIds = maybe (pure []) (fmap ids . Trace.getSpanContext) . Context.lookupSpan =<< Context.getContext
    ids c = [("trace_id", hex TraceId.traceIdBaseEncodedByteString c.traceId), ("span_id", hex TraceId.spanIdBaseEncodedByteString c.spanId)]
    hex enc = AE.String . decodeUtf8 . enc TraceId.Base16
    shaped m extra = m{Log.lmData = AE.Object $ AEKM.fromList $ extra <> fields m.lmData}
    fields = \case
      AE.Object o -> [(bool k "data" ("__data_" `T.isPrefixOf` AEK.toText k), v) | (k, v) <- AEKM.toList o]
      v -> [("data", v)]


-- | Something to look at that is not a defect: filtered like 'logAttention', but written at level
-- @warn@ (see 'jsonLine'), so it never files an issue.
logWarn :: (AE.ToJSON a, Log.MonadLog m) => Text -> a -> m ()
logWarn msg = Log.localDomain warnDomain . Log.logAttention msg


-- | Attach a rendered exception to a log context under the conventional
-- "error" key, so call sites stop repeating the displayException dance.
withError :: Exception e => e -> HashMap Text Text -> HashMap Text Text
withError e = HM.insert "error" (toText $ displayException e)


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
