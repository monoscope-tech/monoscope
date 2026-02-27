-- | Mutable test clock for unified time testing.
--
-- Provides a controllable, advanceable clock backed by an 'IORef UTCTime'.
-- In tests, use 'runMutableTime' instead of 'runFrozenTime' to get a clock
-- that can be fast-forwarded with 'advanceTime'.
--
-- This keeps both the Haskell 'Time' effect and PostgreSQL's 'app_now()'
-- function (via GUC sync) aligned to the same controllable clock.
module Pkg.TestClock (
  TestClock (..),
  newTestClock,
  advanceTime,
  setTestTime,
  getTestTime,
  runMutableTime,
  syncConnectionTime,
  runWithTimeSyncedPool,
) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Pool qualified as Pool
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, formatTime)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PGS
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift)
import Effectful.PostgreSQL (WithConnection (..))
import Effectful.Time (Time (..))
import GHC.Clock (getMonotonicTime)
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
runMutableTime :: IOE :> es => TestClock -> Eff (Time : es) a -> Eff es a
runMutableTime clock = interpret $ \_ -> \case
  CurrentTime -> liftIO $ getTestTime clock
  MonotonicTime -> liftIO getMonotonicTime


-- | Sync a PostgreSQL connection's app.current_time GUC to match the test clock.
-- Uses set_config with is_local=true so the setting is transaction-scoped
-- and auto-resets when the connection returns to the pool.
syncConnectionTime :: TestClock -> Connection -> IO ()
syncConnectionTime clock conn = do
  t <- getTestTime clock
  let timeStr = formatTime defaultTimeLocale "%F %T%Q+00" t
  void $ PGS.execute conn "SELECT set_config('app.current_time', ?, true)" (PGS.Only timeStr)


-- | Like 'runWithConnectionPool' but syncs the test clock's time to the
-- PostgreSQL GUC @app.current_time@ on each connection checkout.
-- This ensures that @app_now()@ in triggers and DDL defaults returns
-- the test clock's time rather than the real wall-clock time.
runWithTimeSyncedPool :: (HasCallStack, IOE :> es) => TestClock -> Pool.Pool Connection -> Eff (WithConnection : es) a -> Eff es a
runWithTimeSyncedPool clock pool = interpret $ \env -> \case
  WithConnection f ->
    localSeqUnlift env $ \unlift -> do
      Pool.withResource pool $ \conn -> do
        syncConnectionTime clock conn
        unlift (f conn)
