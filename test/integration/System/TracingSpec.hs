module System.TracingSpec (spec) where

import BackgroundJobs qualified
import BackgroundJobs.Types qualified as Jobs
import Control.Exception (try)
import Data.Effectful.Wreq qualified as W
import Data.List (lookup)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple qualified as PG
import Effectful (runEff)
import Network.HTTP.Client (defaultManagerSettings)
import Network.HTTP.Types (status200, status400)
import Network.Wai (requestHeaders, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import OpenTelemetry.Propagator (setGlobalTextMapPropagator)
import OpenTelemetry.Propagator.W3CTraceContext (w3cTraceContextPropagator)
import OpenTelemetry.Trace (SpanKind (..), SpanStatus (..))
import OpenTelemetry.Trace.Core (SpanHot (..))
import Pkg.DeriveUtils (pgSpan)
import Pkg.ExtractionWorker qualified as ExtractionWorker
import Pkg.TestUtils
import System.Config (AuthContext (..))
import System.Tracing qualified as Tracing
import System.Types (runBackground)


spec :: Spec
spec = around withTestResources do
  it "outbound HTTP, SQL and job enqueues are Client/Producer spans; in-process spans are Internal" \tr -> do
    -- Prod's initializeGlobalTracerProvider installs this; the test harness never calls it.
    setGlobalTextMapPropagator w3cTraceContextPropagator
    traceparent <- newIORef Nothing
    let app req respond = writeIORef traceparent (lookup "traceparent" (requestHeaders req)) >> respond (responseLBS status200 [] "ok")
        token = "xoxbAbCdEfGhIjKlMnOpQrSt" :: Text
    (_, spans) <- withCapturedSpans \tp -> Warp.testWithApplication (pure app) \port -> do
      mgr <- Tracing.newTracedManager defaultManagerSettings
      void $ runEff $ W.runHTTPWreqVia mgr $ W.get ("http://localhost:" <> show port <> "/services/T0/B0/" <> toString token <> "?key=secret")
      -- A literal ? (jsonb's key-exists operator) must not be formatted, and a bound value must not be recorded.
      withResource tr.trATCtx.pool \c -> do
        let literal = "SELECT 1 AS probe WHERE '{\"a\":1}'::jsonb ? 'a'"; bound = "SELECT ?::text AS probe"
        void (pgSpan c literal (PG.query_ c literal) :: IO [PG.Only Int])
        void (pgSpan c bound (PG.query c bound (PG.Only token)) :: IO [PG.Only Text])
      BackgroundJobs.enqueueJobs tr.trATCtx [Jobs.DailyJob]
      runBackground tr.trLogger tr.trATCtx tp $ Tracing.withSpan_ "in.process" [] pass
    let spanNamed n = maybe (fail $ "no span " <> toString n <> " in " <> show (map (hotName . snd) spans)) pure $ find ((== n) . hotName . snd) spans
    (httpKind, http) <- spanNamed "GET"
    httpKind `shouldBe` Client
    readIORef traceparent >>= (`shouldSatisfy` isJust)
    -- Webhook credentials ride in the path: no URL attribute may keep them.
    let urlAttrs = mapMaybe (textAttr http) ["url.full", "http.url", "url.path", "url.query", "http.target"]
    urlAttrs `shouldSatisfy` (\as -> length as == 5 && not (any (\a -> T.isInfixOf token a || T.isInfixOf "secret" a) as))
    let sqlSpans = [(k, textAttr h "db.system", textAttr h "db.statement") | (k, h) <- spans, maybe False (T.isInfixOf "probe") (textAttr h "db.statement")]
    sqlSpans `shouldMatchList` [(Client, Just "postgresql", Just "SELECT 1 AS probe WHERE '{\"a\":1}'::jsonb ? 'a'"), (Client, Just "postgresql", Just "SELECT ?::text AS probe")]
    (jobKind, job) <- spanNamed "send background_jobs"
    (jobKind, textAttr job "messaging.system") `shouldBe` (Producer, Just "odd_jobs")
    fst <$> spanNamed "in.process" `shouldReturn` Internal

  -- Prod 2026-11-22: error.type was the shown exception, per-request headers and all, so each
  -- failed Twilio/PagerDuty call became its own issue.
  it "a failed outbound request is one low-cardinality error: its status code, not the rendered exchange" \_ -> do
    let app _ respond = respond (responseLBS status400 [("Request-Id", "RQ0123")] "nope")
    (_, spans) <- withCapturedSpans \_ -> Warp.testWithApplication (pure app) \port -> do
      mgr <- Tracing.newTracedManager defaultManagerSettings
      void $ try @SomeException $ runEff $ W.runHTTPWreqVia mgr $ W.get ("http://localhost:" <> show port <> "/x")
    [(textAttr h "error.type", hotStatus h) | (Client, h) <- spans] `shouldBe` [(Just "400", Error "Bad Request from localhost")]

  -- 2026-11-22: per-call helpers were ~60% of prod's 620k self-spans/hour.
  it "AutoInstrument skips NotInstrumented helpers but still wraps the rest" \_ -> do
    (_, spans) <- withCapturedSpans \_ -> do
      cb <- ExtractionWorker.newCircuitBreaker
      now <- getCurrentTime
      void $ ExtractionWorker.shouldAttemptCircuit cb now
      ExtractionWorker.recordCircuitSuccess cb
      void $ ExtractionWorker.recordCircuitFailure cb now
    let names = map (hotName . snd) spans
    when (null names) $ pendingWith "library compiled without the AutoInstrument plugin (test-dev)"
    names `shouldMatchList` ["newCircuitBreaker", "recordCircuitFailure"]
