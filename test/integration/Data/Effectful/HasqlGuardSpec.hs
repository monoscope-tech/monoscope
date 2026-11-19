-- | Regression guard for the 2026-07-06 outage: a write transaction left
-- `idle in transaction` (COMMIT never sent — client/pooler orphaned it) held
-- its locks and wedged `otel_logs_and_spans`, blocking every enrichment writer.
-- `Data.Effectful.Hasql.guardWriteTx` must set `idle_in_transaction_session_timeout`
-- on WRITE transactions (so an orphaned idle txn self-aborts and releases locks)
-- and leave READ transactions untouched.
module Data.Effectful.HasqlGuardSpec (spec) where

import Data.Effectful.Hasql qualified as EHasql
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff)
import Hasql.Interpolate qualified as HI
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import OpenTelemetry.Attributes (Attribute (..), PrimitiveAttribute (..), lookupAttribute)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import OpenTelemetry.Trace (createTracerProvider, emptyTracerProviderOptions, getGlobalTracerProvider, setGlobalTracerProvider)
import OpenTelemetry.Trace.Core (ImmutableSpan (..), SpanHot (..))
import Pkg.DeriveUtils (mkHasqlPool)
import Pkg.TestUtils
import UnliftIO.Async (concurrently_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)


spec :: Spec
spec = around withTestResources do
  -- #803: a span wraps the pool checkout, so a cheap lookup stuck behind a busy pool read as a
  -- slow query. The span keeps its statement name and says how much of it was waiting.
  it "DB spans carry the pool wait, separate from the statement they ran" \tr -> do
    pool <- mkHasqlPool 1 tr.trConnStr
    (processor, ended) <- inMemoryListExporter
    traced <- createTracerProvider [processor] emptyTracerProviderOptions
    let run :: Eff '[EHasql.Hasql, IOE] a -> IO a
        run act = runEff (EHasql.runHasqlPool pool act)
        query :: HI.Sql -> IO ()
        query q = void (run (EHasql.interp q) :: IO [HI.OneColumn Int32])
        statementOf h = case lookupAttribute (hotAttributes h) "db.statement" of Just (AttributeValue (TextAttribute t)) -> t; _ -> ""
        waitOf h = case lookupAttribute (hotAttributes h) "db.client.connection.wait_time" of Just (AttributeValue (DoubleAttribute w)) -> Just w; _ -> Nothing
    bracket getGlobalTracerProvider setGlobalTracerProvider \_ -> do
      setGlobalTracerProvider traced
      -- Both a statement and a session queue behind the one connection for at least 0.8 s.
      concurrently_ (query [HI.sql| SELECT 1 FROM pg_sleep(1.0) |]) $ threadDelay 200_000 >> concurrently_ (query [HI.sql| SELECT 2 AS waited |]) (run $ EHasql.use $ Session.script "SELECT 3")
      void $ OHasql.useStatement pool () (HI.interp False [HI.sql| SELECT 4 AS direct |] :: Statement () [HI.OneColumn Int32])
    hots <- traverse (readIORef . spanHot) =<< readIORef ended
    let spanWith p = maybe (fail "missing span") pure $ find p hots
    waiter <- spanWith (T.isInfixOf "waited" . statementOf)
    session <- spanWith (T.isPrefixOf "hasql.session" . hotName)
    direct <- spanWith (T.isInfixOf "direct" . statementOf)
    -- Named exactly as the library's own useStatement names it, so the two cannot drift.
    hotName waiter `shouldBe` hotName direct
    map waitOf [waiter, session] `shouldSatisfy` all (maybe False (>= 0.5))

  describe "Hasql write-transaction guard (guardWriteTx)"
    $ it "arms idle_in_transaction_session_timeout on writes only, at LOCAL scope"
    $ \tr -> do
      -- A single-connection pool so a session-level baseline persists across uses.
      -- That baseline overrides migration 0104's database-level default, so the
      -- assertions isolate guardWriteTx's SET LOCAL rather than the DB default.
      pool <- mkHasqlPool 1 tr.trConnStr
      let run :: Eff '[EHasql.Hasql, IOE] a -> IO a
          run act = runEff (EHasql.runHasqlPool pool act)
          showTimeout :: Tx.Transaction [Text]
          showTimeout = Tx.statement () $ HI.interp False [HI.sql| SELECT current_setting('idle_in_transaction_session_timeout') |]
      run $ EHasql.use (Session.script "SET idle_in_transaction_session_timeout = '23s'")
      readVal <- run $ EHasql.transaction TxS.ReadCommitted TxS.Read showTimeout
      writeVal <- run $ EHasql.transaction TxS.ReadCommitted TxS.Write showTimeout
      afterVal <- run $ EHasql.transaction TxS.ReadCommitted TxS.Read showTimeout
      readVal `shouldBe` ["23s"] -- reads inherit the session baseline, unguarded
      writeVal `shouldSatisfy` (`elem` [["1min"], ["60s"]]) -- guardWriteTx overrode it for the write txn
      afterVal `shouldBe` ["23s"] -- SET LOCAL scope: baseline restored after commit
