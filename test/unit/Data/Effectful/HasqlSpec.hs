module Data.Effectful.HasqlSpec (spec) where

import Data.Effectful.Hasql qualified as EHasql
import Hasql.Errors qualified as HE
import Hasql.Pool qualified as HP
import Relude
import Test.Hspec


-- | Regression for the 2026-06-09 incident: TimeFusion was restarting and
-- libpq returned @FATAL: the database system is starting up@. hasql wraps
-- that as @ConnectionUsageError (OtherConnectionError …)@ — but hasql's own
-- @IsError.isTransient@ classifies the free-form @OtherConnectionError@
-- bucket as NON-transient. The bisector in @bulkInsertOtelLogsAndSpans@
-- then drilled to a single row, logged @POISON_ROW_DROPPED@ and returned 0,
-- making the PubSub/Kafka layer ack a message whose rows never landed in PG
-- or TF — silent log loss.
--
-- Fix: @isTransientUsageError@ now treats every @ConnectionUsageError@ as
-- transient (the server never saw the row, so it can't be a data fault).
-- This regression pins the exact incident shape so the classification can't
-- silently revert.
spec :: Spec
spec = describe "isTransientException" $ do
  let asExc = toException . EHasql.HasqlException

  it "marks ConnectionUsageError (OtherConnectionError …) as transient — POISON_ROW_DROPPED regression" $
    EHasql.isTransientException (asExc (HP.ConnectionUsageError (HE.OtherConnectionError "FATAL: the database system is starting up\n")))
      `shouldBe` True

  it "marks every named ConnectionError constructor as transient (server never saw the row)" $
    for_ allConnectionErrors $ \ce ->
      EHasql.isTransientException (asExc (HP.ConnectionUsageError ce)) `shouldBe` True

  it "marks AcquisitionTimeoutUsageError as transient" $
    EHasql.isTransientException (asExc HP.AcquisitionTimeoutUsageError) `shouldBe` True

  -- Regression for the 2026-06-21 DLQ flood: pgdog / datafusion-postgres reset a
  -- backend connection mid-statement and reply with a server error carrying an
  -- EMPTY SQLSTATE. hasql classes every StatementSessionError as non-transient,
  -- so the dual-write/read-gate retry treated the reset as poison and
  -- dead-lettered writable data. An empty code is never a real PG execution
  -- error (those always carry a 5-char SQLSTATE) → treat as transient.
  it "marks an empty-SQLSTATE ServerError as transient (pgdog/pgwire reset)" $
    EHasql.isTransientException (asExc emptySqlStateError) `shouldBe` True

  it "keeps a real-SQLSTATE ServerError non-transient (genuine poison still routes to DLQ)" $
    EHasql.isTransientException (asExc realSqlStateError) `shouldBe` False

  it "marks an empty-SQLSTATE ScriptSessionError as transient (reset mid-script)" $
    EHasql.isTransientException (asExc emptyScriptError) `shouldBe` True
  where
    allConnectionErrors =
      [ HE.NetworkingConnectionError "ECONNREFUSED"
      , HE.AuthenticationConnectionError "password authentication failed"
      , HE.CompatibilityConnectionError "server_version_num parse error"
      , HE.OtherConnectionError "FATAL: the database system is starting up\n"
      ]
    emptySqlStateError =
      HP.SessionUsageError (HE.StatementSessionError 1 0 "insert into otel_logs_and_spans ..." [] True (HE.ServerStatementError (HE.ServerError "" "Server error" Nothing Nothing Nothing)))
    realSqlStateError =
      HP.SessionUsageError (HE.StatementSessionError 1 0 "insert into otel_logs_and_spans ..." [] True (HE.ServerStatementError (HE.ServerError "23505" "duplicate key value violates unique constraint" Nothing Nothing Nothing)))
    emptyScriptError =
      HP.SessionUsageError (HE.ScriptSessionError "vacuum analyze" (HE.ServerError "" "Server error" Nothing Nothing Nothing))
