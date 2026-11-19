module System.LoggingSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Effectful (runEff)
import Log (LogLevel (..), LogMessage (..), mkLogger)
import Log.Internal.Logger (Logger (..))
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Trace.Core qualified as Trace
import OpenTelemetry.Trace.Id qualified as TraceId
import Pkg.TestUtils
import System.Logging qualified as Logging


spec :: Spec
spec = describe "System.Logging" do
  it "tags a message with the trace and span it was logged in, whichever API logged it" do
    seen <- newIORef []
    logger <- Logging.tolerantLogger <$> mkLogger "capture" (\m -> modifyIORef' seen (m :))
    let write = loggerWriteMessage logger (LogMessage "c" [] frozenTime LogInfo "m" (AE.object ["k" AE..= ("v" :: Text)]))
        ids d = (,) <$> KM.lookup "trace_id" d <*> KM.lookup "span_id" d
        hex enc = AE.String . decodeUtf8 . enc TraceId.Base16
    write
    -- Attached the way 'System.Tracing.runTracing' attaches every span.
    provider <- Trace.createTracerProvider [] Trace.emptyTracerProviderOptions
    ctx <- Context.getContext
    sp <- Trace.createSpan (Trace.makeTracer provider "test" Trace.tracerOptions) ctx "logs" Trace.defaultSpanArguments
    Context.adjustContext (Context.insertSpan sp) >> write >> Context.adjustContext (const ctx)
    expected <- Trace.getSpanContext sp <&> \c -> (hex TraceId.traceIdBaseEncodedByteString c.traceId, hex TraceId.spanIdBaseEncodedByteString c.spanId)
    loggerWaitForWrite logger
    map (\m -> case m.lmData of AE.Object d -> (KM.lookup "k" d, ids d); _ -> (Nothing, Nothing))
      . reverse
      <$> readIORef seen
      `shouldReturn` [(Just "v", Nothing), (Just "v", Just expected)]

  it "keeps a non-object payload under data, and a warning in the warn domain" do
    seen <- newIORef []
    logger <- Logging.tolerantLogger <$> mkLogger "capture" (\m -> modifyIORef' seen (m :))
    runEff $ Logging.runLog "test" logger LogTrace (Logging.logInfo "tuple" ("a" :: Text, 1 :: Int) >> Logging.logWarn "careful" ())
    loggerWaitForWrite logger
    map (\m -> (m.lmDomain, m.lmData))
      . reverse
      <$> readIORef seen
      `shouldReturn` [([], AE.object ["data" AE..= ("a" :: Text, 1 :: Int)]), (["warn"], AE.object ["data" AE..= ([] :: [Int])])]
