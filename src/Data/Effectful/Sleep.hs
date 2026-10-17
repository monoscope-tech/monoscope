-- | Waiting, and the monotonic clock it is measured on, as an effect: tests
-- advance a virtual clock instead of sleeping, so retry budgets cost no wall time.
module Data.Effectful.Sleep (
  Sleep,
  sleepMicros,
  monotonicMicros,
  runSleep,
  runNoSleep,
) where

import Control.Concurrent (threadDelay)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (evalState, get, modify)
import Effectful.TH
import GHC.Clock (getMonotonicTimeNSec)
import Relude hiding (evalState, get, modify)


data Sleep :: Effect where
  SleepMicros :: Int -> Sleep m ()
  MonotonicMicros :: Sleep m Int


type instance DispatchOf Sleep = 'Dynamic


makeEffect ''Sleep


runSleep :: IOE :> es => Eff (Sleep ': es) a -> Eff es a
runSleep = interpret_ \case
  SleepMicros us -> liftIO $ threadDelay us
  MonotonicMicros -> liftIO $ fromIntegral . (`div` 1000) <$> getMonotonicTimeNSec


-- | Sleeps return at once and only advance the clock 'monotonicMicros' reads.
runNoSleep :: Eff (Sleep ': es) a -> Eff es a
runNoSleep = reinterpret_ (evalState (0 :: Int)) \case
  SleepMicros us -> modify (+ us)
  MonotonicMicros -> get
