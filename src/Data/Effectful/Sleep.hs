-- | Waiting as an effect, so retry backoffs cost no wall-clock time in tests.
module Data.Effectful.Sleep (
  Sleep,
  sleepMicros,
  runSleep,
  runNoSleep,
) where

import Control.Concurrent (threadDelay)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Relude


data Sleep :: Effect where
  SleepMicros :: Int -> Sleep m ()


type instance DispatchOf Sleep = 'Dynamic


makeEffect ''Sleep


runSleep :: IOE :> es => Eff (Sleep ': es) a -> Eff es a
runSleep = interpret \_ (SleepMicros us) -> liftIO $ threadDelay us


runNoSleep :: Eff (Sleep ': es) a -> Eff es a
runNoSleep = interpret \_ (SleepMicros _) -> pass
