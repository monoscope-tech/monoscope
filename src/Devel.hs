{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Devel (dev2) where

-- Devel is useful when doing local web/html development
-- where having fast reloads is important
--

import Data.UUID qualified as UUID
import GHC.Stats (getRTSStats, gcdetails_live_bytes, gc)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.Mem (performMajorGC)

import BackgroundJobs qualified
import Configuration.Dotenv qualified as Dotenv
import Control.Exception.Safe qualified as Safe
import Effectful
import Effectful.Fail (runFailIO)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import OpenTelemetry.Trace (getGlobalTracerProvider)
import Pkg.DeriveUtils (UUIDId (..))
import System.Config qualified as Cfg
import System.Types (runBackground)


dev2 :: IO ()
dev2 = do
  _ <- Safe.try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ())
  ctx <- runEff $ runFailIO Cfg.getAppContext
  tp <- getGlobalTracerProvider
  let pid = UUIDId $ Unsafe.fromJust $ UUID.fromString "be87ebc1-08b9-4293-a390-283460fa6202"
  LogBulk.withBulkStdOutLogger \logger -> do
    let run label action = do
          printMem $ "Before " <> label
          void (runBackground logger ctx tp action) `Safe.catchAny` \e -> putTextLn $ label <> " error: " <> show e
          printMem $ "After " <> label
    run "endpointTemplateDiscovery" $ BackgroundJobs.endpointTemplateDiscovery pid
    run "patternEmbeddingAndMerge" $ BackgroundJobs.patternEmbeddingAndMerge pid
  pass

printMem :: Text -> IO ()
printMem label = do
  performMajorGC
  stats <- getRTSStats
  let liveMB = gcdetails_live_bytes (gc stats) `div` (1024 * 1024)
  putTextLn $ label <> ": " <> show liveMB <> " MB live"

