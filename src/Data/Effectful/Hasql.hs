-- | Effectful effect for hasql sessions, sitting alongside the existing
-- `WithConnection` postgresql-simple effect during the migration.
module Data.Effectful.Hasql (
  Hasql (..),
  HasqlException (..),
  isTransientHasqlError,
  runHasqlPool,
  session,
  statement,
  transaction,
  interp,
  withHasqlTimefusion,
  withLabeled,
) where

import Control.Exception (throwIO)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Labeled (Labeled, labeled)
import Hasql.Interpolate qualified as HI
import Hasql.Pool (Pool, UsageError (..), use)
import Hasql.Session (CommandError (..), Session, SessionError (..))
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Relude


data Hasql :: Effect where
  UseSession :: Session a -> Hasql m (Either UsageError a)


type instance DispatchOf Hasql = 'Dynamic


newtype HasqlException = HasqlException UsageError
  deriving stock (Show)


instance Exception HasqlException where
  displayException (HasqlException ue) = case ue of
    AcquisitionTimeoutUsageError -> "Hasql: pool acquisition timeout"
    ConnectionUsageError e -> "Hasql connection error: " <> show e
    SessionUsageError se -> "Hasql session error: " <> show se


-- | True for transient infra failures (acquisition timeout, dropped connection,
-- libpq client error). Server errors / decode errors are programmer bugs, never retry.
-- Exhaustive pattern (no wildcard) so a future hasql-pool upgrade triggers a compile
-- error rather than silently misclassifying new error variants as non-retriable.
isTransientUsageError :: UsageError -> Bool
isTransientUsageError = \case
  AcquisitionTimeoutUsageError -> True
  ConnectionUsageError _ -> True
  SessionUsageError se -> case se of
    QueryError _ _ (ClientError _) -> True
    QueryError _ _ (ResultError _) -> False
    PipelineError (ClientError _) -> True
    PipelineError (ResultError _) -> False


isTransientHasqlError :: HasqlException -> Bool
isTransientHasqlError (HasqlException ue) = isTransientUsageError ue


runHasqlPool :: IOE :> es => Pool -> Eff (Hasql ': es) a -> Eff es a
runHasqlPool pool = interpret \_ -> \case
  UseSession s -> liftIO (use pool s)


-- | Run a `Session`, throwing `HasqlException` on `UsageError`.
session :: (Hasql :> es, IOE :> es) => Session a -> Eff es a
session s = send (UseSession s) >>= either (liftIO . throwIO . HasqlException) pure


statement :: (Hasql :> es, IOE :> es) => params -> Statement params a -> Eff es a
statement p st = session (Session.statement p st)


-- | Run a `hasql-interpolate` `Sql` value as a prepared statement.
interp :: (HI.DecodeResult a, Hasql :> es, IOE :> es) => HI.Sql -> Eff es a
interp s = statement () (HI.interp True s)


transaction :: (Hasql :> es, IOE :> es) => TxS.IsolationLevel -> TxS.Mode -> Tx.Transaction a -> Eff es a
transaction iso mode tx = session (TxS.transaction iso mode tx)


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
