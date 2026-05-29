-- | Mutable test clock for unified time testing.
--
-- Provides a controllable, advanceable clock backed by an 'IORef UTCTime'.
-- In tests, use 'runMutableTime' instead of 'runFrozenTime' to get a clock
-- that can be fast-forwarded with 'advanceTime'.
--
-- The companion migration 0094_app_now_function.sql defines a PostgreSQL
-- @app_now()@ that reads the @app.current_time@ GUC. Pass a test connection
-- through 'syncConnectionTime' before issuing time-sensitive SQL and the
-- triggers / stored procedures will see the same clock as the Haskell
-- effect.
module Pkg.TestClock (
  TestClock (..),
  newTestClock,
  advanceTime,
  setTestTime,
  getTestTime,
  runMutableTime,
  syncConnectionTime,
  runHasqlPoolSynced,
) where

import Data.Effectful.Hasql (Hasql (..))
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PGS
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Time (Time (..))
import GHC.Clock (getMonotonicTime)
import Hasql.Session qualified as Session
import OpenTelemetry.Instrumentation.Hasql (TracedPool)
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Relude


-- | A mutable clock backed by an 'IORef'. Create with 'newTestClock',
-- advance with 'advanceTime', and use 'runMutableTime' to interpret
-- the 'Time' effect.
newtype TestClock = TestClock {unTestClock :: IORef UTCTime}


-- | Create a new test clock starting at the given time.
newTestClock :: UTCTime -> IO TestClock
newTestClock t = TestClock <$> newIORef t


-- | Advance the test clock forward by the given duration.
advanceTime :: TestClock -> NominalDiffTime -> IO ()
advanceTime (TestClock ref) dt = modifyIORef' ref (addUTCTime dt)


-- | Set the test clock to an absolute time.
setTestTime :: TestClock -> UTCTime -> IO ()
setTestTime (TestClock ref) = writeIORef ref


-- | Read the current time from the test clock.
getTestTime :: TestClock -> IO UTCTime
getTestTime (TestClock ref) = readIORef ref


-- | Run the 'Time' effect using a mutable test clock.
-- 'CurrentTime' reads from the IORef (advanceable).
-- 'MonotonicTime' uses the real monotonic clock (unchanged).
runMutableTime :: IOE :> es => TestClock -> Eff (Time ': es) a -> Eff es a
runMutableTime clock = interpret $ \_ -> \case
  CurrentTime -> liftIO $ getTestTime clock
  MonotonicTime -> liftIO getMonotonicTime


-- | Sync a PostgreSQL connection's @app.current_time@ GUC to the test
-- clock's current value. Uses @set_config(_, _, true)@ so the setting is
-- transaction-scoped and resets when the connection returns to the pool.
--
-- Call this on every connection that needs to see the test clock from
-- inside triggers / stored procedures / DDL defaults that go through
-- @app_now()@.
syncConnectionTime :: TestClock -> Connection -> IO ()
syncConnectionTime clock conn = do
  t <- getTestTime clock
  let timeStr = formatTime defaultTimeLocale "%F %T%Q+00" t
  void $ PGS.execute conn "SELECT set_config('app.current_time', ?, true)" (PGS.Only timeStr)


-- | Hasql twin of 'runHasqlPool' that pushes the test clock into the
-- @app.current_time@ GUC before each Session. Use in tests that exercise
-- triggers / stored procedures going through @app_now()@. The setting is
-- session-scoped (@is_local=false@) so it survives the auto-commit boundary
-- between the @SET@ and the user's Session, but every call re-sets it so
-- the pool may freely rotate connections without contaminating tests.
runHasqlPoolSynced :: IOE :> es => TestClock -> TracedPool -> Eff (Hasql ': es) a -> Eff es a
runHasqlPoolSynced clock pool = interpret \_ -> \case
  UseSession s -> do
    t <- liftIO $ getTestTime clock
    let timeStr = formatTime defaultTimeLocale "%F %T%Q+00" t
        setStmt = "SELECT set_config('app.current_time', '" <> encodeUtf8 (toText timeStr) <> "', false)"
    OHasql.use pool (Session.sql setStmt *> s)
