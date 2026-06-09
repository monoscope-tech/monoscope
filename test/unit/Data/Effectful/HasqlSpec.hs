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
  where
    allConnectionErrors =
      [ HE.NetworkingConnectionError "ECONNREFUSED"
      , HE.AuthenticationConnectionError "password authentication failed"
      , HE.CompatibilityConnectionError "server_version_num parse error"
      , HE.OtherConnectionError "FATAL: the database system is starting up\n"
      ]
