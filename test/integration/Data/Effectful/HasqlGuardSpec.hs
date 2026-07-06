-- | Regression guard for the 2026-07-06 outage: a write transaction left
-- `idle in transaction` (COMMIT never sent — client/pooler orphaned it) held
-- its locks and wedged `otel_logs_and_spans`, blocking every enrichment writer.
-- `Data.Effectful.Hasql.guardWriteTx` must set `idle_in_transaction_session_timeout`
-- on WRITE transactions (so an orphaned idle txn self-aborts and releases locks)
-- and leave READ transactions untouched.
module Data.Effectful.HasqlGuardSpec (spec) where

import Data.Effectful.Hasql qualified as EHasql
import Effectful (Eff, IOE, runEff)
import Hasql.Interpolate qualified as HI
import Hasql.Session qualified as Session
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Pkg.DeriveUtils (mkHasqlPool)
import Pkg.TestUtils (TestResources (..), withTestResources)
import Relude
import Test.Hspec


spec :: Spec
spec = around withTestResources
  $ describe "Hasql write-transaction guard (guardWriteTx)"
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
