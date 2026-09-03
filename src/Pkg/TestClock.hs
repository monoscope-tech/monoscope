-- | Mutable test clock for unified time testing.
--
-- Provides a controllable, advanceable clock backed by an 'IORef UTCTime'.
-- In tests, use 'runMutableTime' instead of 'runFrozenTime' to get a clock
-- that can be fast-forwarded with 'advanceTime'.
--
-- The companion migration 0094_app_now_function.sql defines a PostgreSQL
-- @app_now()@ that reads the @app.current_time@ GUC. 'runHasqlPoolSynced' pushes
-- the clock into that GUC before every Session, so triggers and stored
-- procedures see the same clock as the Haskell effect. 'Pkg.TestUtils' wires it
-- for all test DB access, which is why time-sensitive SQL in specs already
-- observes 'advanceTime'.
module Pkg.TestClock (
  TestClock (..),
  newTestClock,
  advanceTime,
  setTestTime,
  getTestTime,
  runMutableTime,
  runHasqlPoolSynced,
) where

import Data.Effectful.Hasql (Hasql (..))
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, defaultTimeLocale, formatTime)
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
-- | Hasql twin of 'runHasqlPool' that pushes the test clock into the
-- @app.current_time@ GUC before each Session. Use in tests that exercise
-- triggers / stored procedures going through @app_now()@. The setting is
-- session-scoped (@is_local=false@) so it survives the auto-commit boundary
-- between the @SET@ and the user's Session, but every call re-sets it so
-- the pool may freely rotate connections without contaminating tests.
runHasqlPoolSynced :: forall es a. IOE :> es => TestClock -> TracedPool -> Eff (Hasql ': es) a -> Eff es a
runHasqlPoolSynced clock pool = interpret \_ -> \case
  UseSession s -> withClock $ \pre -> OHasql.use pool (pre *> s)
  UseStatement p st -> withClock $ \pre -> OHasql.use pool (pre *> Session.statement p st)
  UseLabeledSession n xs s -> withClock $ \pre -> OHasql.useSession pool n xs (pre *> s)
  where
    withClock :: forall r. (Session.Session () -> IO r) -> Eff es r
    withClock run = liftIO $ do
      t <- getTestTime clock
      let timeStr = formatTime defaultTimeLocale "%F %T%Q+00" t
          setStmt = "SELECT set_config('app.current_time', '" <> toText timeStr <> "', false)"
      run (Session.script setStmt)
