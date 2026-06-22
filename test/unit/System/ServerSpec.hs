module System.ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race, wait)
import Control.Exception (catch, finally)
import Relude
import System.Server (cancelAllConcurrently)
import Test.Hspec


-- | Regression for the 2026-06-22 "Ctrl-C doesn't shut the app down" bug:
-- async's @waitAnyCancel@ cancels fibers sequentially (@mapM_ cancel@), blocking
-- on each fiber's death before signalling the next, so one slow-to-die fiber
-- (Warp's graceful drain, a wedged librdkafka poll) kept every later fiber —
-- the kafka workers retrying dead-DB writes — alive. 'cancelAllConcurrently'
-- fixes this by cancelling concurrently within a deadline.
spec :: Spec
spec = describe "cancelAllConcurrently" do
  it "cancels later fibers without waiting for an earlier slow-to-die one" do
    fastCancelled <- newIORef False
    fast <- async $ forever (threadDelay 10_000) `finally` writeIORef fastCancelled True
    -- Swallows its first cancel for 2s (models a slow drain). Listed FIRST — the
    -- exact position that used to block sequential cancel and starve `fast`.
    slow <- async $ forever (threadDelay 10_000) `catch` \(_ :: SomeException) -> threadDelay 2_000_000
    canceller <- async $ cancelAllConcurrently 10_000_000 [slow, fast]
    threadDelay 400_000
    readIORef fastCancelled `shouldReturn` True -- would still be False under sequential cancel (blocked on `slow`)
    wait canceller

  it "honours its deadline when a fiber refuses to die" do
    -- Eats every cancel and loops forever, so `cancel`'s wait never returns;
    -- only the deadline can break it. `race` proves we return before 2s.
    stuck <- async $ forever (forever (threadDelay 100_000) `catch` \(_ :: SomeException) -> pass)
    race (threadDelay 2_000_000) (cancelAllConcurrently 500_000 [stuck]) `shouldReturn` Right ()
