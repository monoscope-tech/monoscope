-- | Effectful effect for hasql database sessions.
module Data.Effectful.Hasql (
  Hasql (..),
  HasqlException (..),
  isTransientHasqlError,
  isTransientException,
  isDeadlockError,
  isUniqueViolation,
  runHasqlPool,
  use,
  statement,
  transaction,
  labeledSession,
  labeledTransaction,
  interp,
  interpOne,
  interpExecute,
  interpExecute_,
  withHasqlTimefusion,
  withLabeled,
) where

import Control.Exception (throwIO)
import Data.HashMap.Strict qualified as HM
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Labeled (Labeled, labeled)
import Hasql.Errors (IsError (..), ServerError (..), SessionError (..), StatementError (..), toDetailedText)
import Hasql.Interpolate qualified as HI
import Hasql.Pool (UsageError (..))
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import OpenTelemetry.Attributes (Attribute)
import OpenTelemetry.Instrumentation.Hasql (TracedPool)
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Relude


data Hasql :: Effect where
  UseSession :: Session a -> Hasql m (Either UsageError a)
  -- | Run a single Statement so the instrumentation wrapper can attach
  -- db.statement / db.operation.name to the span.
  UseStatement :: params -> Statement params a -> Hasql m (Either UsageError a)
  -- | Run an opaque Session under a caller-chosen span name + extra attributes.
  -- Use for transactions and multi-statement scripts where SQL can't be auto-extracted.
  UseLabeledSession :: Text -> HM.HashMap Text Attribute -> Session a -> Hasql m (Either UsageError a)


type instance DispatchOf Hasql = 'Dynamic


newtype HasqlException = HasqlException UsageError
  deriving stock (Show)


instance Exception HasqlException where
  displayException (HasqlException ue) = case ue of
    AcquisitionTimeoutUsageError -> "Hasql: pool acquisition timeout"
    ConnectionUsageError e -> "Hasql connection error: " <> toString (toDetailedText e)
    SessionUsageError e -> "Hasql session error: " <> toString (toDetailedText e)


-- | True for transient infra failures (acquisition timeout, dropped connection).
-- ConnectionUsageError is *always* transient: the server never saw the row, so
-- it can't be a data/poison fault — hasql's IsError.isTransient returns False
-- for the free-form @OtherConnectionError@ bucket (e.g. "the database system
-- is starting up"), which previously caused the bisector to log
-- POISON_ROW_DROPPED and silently drop logs when TF/PG was restarting.
--
-- >>> isTransientUsageError AcquisitionTimeoutUsageError
-- True
isTransientUsageError :: UsageError -> Bool
isTransientUsageError = \case
  AcquisitionTimeoutUsageError -> True
  ConnectionUsageError _ -> True
  SessionUsageError e -> isTransient e || emptySqlState e
  where
    -- A real PG/TimescaleDB execution error always carries a 5-char SQLSTATE.
    -- An empty code is what pgdog / datafusion-postgres emit when a backend
    -- connection is reset or a statement can't be routed mid-flight — an infra
    -- fault wearing a server-error costume. hasql buckets every
    -- StatementSessionError as non-transient, so without this the dual-write
    -- retry treats the reset as poison and dead-letters writable data (the
    -- 2026-06-21 DLQ flood). Same incident class as ConnectionUsageError above.
    emptySqlState = \case
      StatementSessionError _ _ _ _ _ (ServerStatementError (ServerError code _ _ _ _)) -> code == ""
      ScriptSessionError _ (ServerError code _ _ _ _) -> code == ""
      _ -> False


isTransientHasqlError :: HasqlException -> Bool
isTransientHasqlError (HasqlException ue) = isTransientUsageError ue


-- | Exported so callers stop repeating @maybe False isTransientHasqlError . fromException@.
isTransientException :: SomeException -> Bool
isTransientException = maybe False isTransientHasqlError . fromException


-- | SQLSTATE of a failed statement, if the error carries one — the nested walk
-- shared by the by-code predicates below (Nothing for connection/acquisition faults).
serverErrorCode :: HasqlException -> Maybe Text
serverErrorCode (HasqlException ue) = case ue of
  SessionUsageError (StatementSessionError _ _ _ _ _ (ServerStatementError (ServerError code _ _ _ _))) -> Just code
  SessionUsageError (ScriptSessionError _ (ServerError code _ _ _ _)) -> Just code
  _ -> Nothing


-- | Deadlock detected (SQLSTATE 40P01) — safe to retry the whole transaction.
isDeadlockError :: HasqlException -> Bool
isDeadlockError = (== Just "40P01") . serverErrorCode


-- | Unique-constraint violation (SQLSTATE 23505) — lets a handler turn a losing
-- concurrent insert into a friendly message instead of a raw 500.
isUniqueViolation :: HasqlException -> Bool
isUniqueViolation = (== Just "23505") . serverErrorCode


runHasqlPool :: IOE :> es => TracedPool -> Eff (Hasql ': es) a -> Eff es a
runHasqlPool pool = interpret \_ -> \case
  UseSession s -> OHasql.use pool s
  UseStatement p st -> OHasql.useStatement pool p st
  UseLabeledSession n xs s -> OHasql.useSession pool n xs s


-- | Run a `Session`, throwing `HasqlException` on `UsageError`. Mirrors
-- `Hasql.Pool.use` — opaque sessions get the generic `hasql.session <db>` span.
use :: (Hasql :> es, IOE :> es) => Session a -> Eff es a
use s = send (UseSession s) >>= either (liftIO . throwIO . HasqlException) pure


statement :: (Hasql :> es, IOE :> es) => params -> Statement params a -> Eff es a
statement p st = send (UseStatement p st) >>= either (liftIO . throwIO . HasqlException) pure


-- | Run a `hasql-interpolate` `Sql` value as a prepared statement.
interp :: (HI.DecodeResult a, Hasql :> es, IOE :> es) => HI.Sql -> Eff es a
interp s = statement () (HI.interp True s)


-- | Run a query expecting at most one row.
interpOne :: (HI.DecodeRow a, Hasql :> es, IOE :> es) => HI.Sql -> Eff es (Maybe a)
interpOne s = listToMaybe <$> interp s


-- | Run an INSERT/UPDATE/DELETE and return the number of rows affected.
interpExecute :: (Hasql :> es, IOE :> es) => HI.Sql -> Eff es Int64
interpExecute s = HI.getRowsAffected <$> interp s


-- | Run an INSERT/UPDATE/DELETE, discarding the row count.
interpExecute_ :: (Hasql :> es, IOE :> es) => HI.Sql -> Eff es ()
interpExecute_ s = void $ interpExecute s


transaction :: (Hasql :> es, IOE :> es) => TxS.IsolationLevel -> TxS.Mode -> Tx.Transaction a -> Eff es a
transaction iso mode tx = use (TxS.transaction iso mode tx)


-- | Like 'session' but carries a caller-chosen span name + extra attributes.
labeledSession :: (Hasql :> es, IOE :> es) => Text -> HM.HashMap Text Attribute -> Session a -> Eff es a
labeledSession name attrs s =
  send (UseLabeledSession name attrs s) >>= either (liftIO . throwIO . HasqlException) pure


-- | Like 'transaction' but tags the span with a name describing the unit of work.
labeledTransaction
  :: (Hasql :> es, IOE :> es)
  => Text -> TxS.IsolationLevel -> TxS.Mode -> Tx.Transaction a -> Eff es a
labeledTransaction name iso mode tx = labeledSession name mempty (TxS.transaction iso mode tx)


-- | Generic helper: route an effect through a labelled variant when a flag is on.
withLabeled
  :: forall name e es a
   . (Labeled name e :> es, e :> es)
  => Bool -> Eff (e ': es) a -> Eff es a
withLabeled cond q = if cond then labeled @name @e q else subsume q


withHasqlTimefusion
  :: (Hasql :> es, Labeled "timefusion" Hasql :> es)
  => Bool -> Eff (Hasql ': es) a -> Eff es a
withHasqlTimefusion = withLabeled @"timefusion" @Hasql
