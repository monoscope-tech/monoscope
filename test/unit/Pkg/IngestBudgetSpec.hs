module Pkg.IngestBudgetSpec (spec) where

import Pkg.IngestBudget
import Relude
import System.IO.Error (userError)
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Async (cancel, withAsync)
import UnliftIO.Exception (throwIO, tryAny)


spec :: Spec
spec = describe "shared ingestion byte budget" do
  it "shares capacity across small batches and admits oversized records alone" do
    budget <- newIngestBudget
    entered <- newEmptyMVar
    release <- newEmptyMVar
    let half = fromIntegral (ingestByteLimit `div` 2)
    withAsync (withIngestBytes budget half (putMVar entered () >> takeMVar release)) \holder -> do
      timeout 1_000_000 (takeMVar entered) `shouldReturn` Just ()
      timeout 1_000_000 (withIngestBytes budget half pass) `shouldReturn` Just ()
      timeout 50_000 (withIngestBytes budget (half * 4) pass) `shouldReturn` Nothing
      cancel holder
      timeout 1_000_000 (withIngestBytes budget (half * 4) pass) `shouldReturn` Just ()

  it "restores capacity after cancelling a queued waiter or an active batch, and after exceptions" do
    budget <- newIngestBudget
    entered <- newEmptyMVar
    waiting <- newEmptyMVar
    release <- newEmptyMVar
    let full = fromIntegral ingestByteLimit
    withAsync (withIngestBytes budget full (putMVar entered () >> takeMVar release)) \holder -> do
      timeout 1_000_000 (takeMVar entered) `shouldReturn` Just ()
      withAsync (putMVar waiting () >> withIngestBytes budget full pass) \waiter -> do
        timeout 1_000_000 (takeMVar waiting) `shouldReturn` Just ()
        cancel waiter
      cancel holder
    outcome <- tryAny $ withIngestBytes budget full (throwIO (userError "batch failed") :: IO ())
    outcome `shouldSatisfy` isLeft
    timeout 1_000_000 (withIngestBytes budget full pass) `shouldReturn` Just ()
