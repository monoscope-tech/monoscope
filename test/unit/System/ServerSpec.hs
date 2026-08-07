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
    -- Poll rather than sleep a fixed 400ms: a loaded CI runner can miss that
    -- window and fail a test that is actually passing. Any answer inside 1.5s
    -- still proves the point, because `slow` swallows its cancel for a full 2s —
    -- under sequential cancel `fast` could not have died yet.
    waitUntil 1_500_000 (readIORef fastCancelled) `shouldReturn` True
    wait canceller

  it "honours its deadline when a fiber refuses to die" do
    -- Eats every cancel and loops forever, so `cancel`'s wait never returns;
    -- only the deadline can break it. `race` proves we return before 2s.
    stuck <- async $ forever (forever (threadDelay 100_000) `catch` \(_ :: SomeException) -> pass)
    race (threadDelay 2_000_000) (cancelAllConcurrently 500_000 [stuck]) `shouldReturn` Right ()


-- | Poll @check@ every 10ms until it returns True or @budget@ microseconds
-- elapse. Lets a timing assertion state the deadline it actually cares about
-- instead of guessing how long a busy machine needs.
waitUntil :: Int -> IO Bool -> IO Bool
waitUntil budget check
  | budget <= 0 = check
  | otherwise =
      check >>= \case
        True -> pure True
        False -> threadDelay step >> waitUntil (budget - step) check
  where
    step = 10_000
