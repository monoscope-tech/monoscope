module System.ServerSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race, wait)
import Control.Exception (catch, finally)
import Data.Text qualified as T
import Network.HTTP.Types (status200, status404)
import Network.Wai (Request (..), defaultRequest, responseLBS, responseStatus)
import Network.Wai.Internal (ResponseReceived (..))
import Pkg.DeriveUtils (assetUrl)
import Relude
import System.Server (cancelAllConcurrently, hashedAssetMiddleware)
import Test.Hspec


spec :: Spec
spec = describe "System.Server" do
  hashedAssetSpec
  cancelSpec


-- | Regression for the 2026-08-11 "every page loads unstyled" bug: the hash in the URL
-- was baked in at compile time while the middleware verified against disk at boot, so any
-- asset rebuilt after the last compile (a CSS rebuild, @make fa-add@) failed the check —
-- and the check fails closed, so the page 404'd its own stylesheet. Both sides now read
-- one map, which is what this asserts: whatever a page renders, the middleware accepts.
hashedAssetSpec :: Spec
hashedAssetSpec = describe "hashedAssetMiddleware" do
  -- Returns the path the inner app was handed (Nothing if the middleware answered on its
  -- own) and the status the client would see.
  let req_ path = do
        seen <- newIORef Nothing
        status <- newIORef Nothing
        let inner req respond = writeIORef seen (Just $ T.intercalate "/" req.pathInfo) >> respond (responseLBS status200 [] "")
            capture res = ResponseReceived <$ writeIORef status (Just $ responseStatus res)
        _ <- hashedAssetMiddleware inner defaultRequest{pathInfo = T.splitOn "/" $ T.dropWhile (== '/') path} capture
        (,) <$> readIORef seen <*> readIORef status

  it "serves the URL the pages actually render, off the file it names" do
    req_ (assetUrl "/public/assets/css/tailwind.min.css")
      `shouldReturn` (Just "public/assets/css/tailwind.min.css", Just status200)

  it "404s a URL claiming a build this replica does not have" do
    -- Answering it with these bytes is what poisons a CDN for the whole max-age.
    req_ "/public/assets/css/tailwind.min.0badc0de.css" `shouldReturn` (Nothing, Just status404)

  it "passes an unhashed URL straight through, rather than treating it as a bad hash" do
    req_ "/public/assets/css/tailwind.min.css" `shouldReturn` (Just "public/assets/css/tailwind.min.css", Just status200)


-- | Regression for the 2026-06-22 "Ctrl-C doesn't shut the app down" bug:
-- async's @waitAnyCancel@ cancels fibers sequentially (@mapM_ cancel@), blocking
-- on each fiber's death before signalling the next, so one slow-to-die fiber
-- (Warp's graceful drain, a wedged librdkafka poll) kept every later fiber —
-- the kafka workers retrying dead-DB writes — alive. 'cancelAllConcurrently'
-- fixes this by cancelling concurrently within a deadline.
cancelSpec :: Spec
cancelSpec = describe "cancelAllConcurrently" do
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
