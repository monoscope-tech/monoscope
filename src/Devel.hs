{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Devel (dev2) where

-- Devel is useful when doing local web/html development
-- where having fast reloads is important
--

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Relude
import Relude.Unsafe qualified as Unsafe

import BackgroundJobs qualified
import Configuration.Dotenv qualified as Dotenv
import Control.Exception.Safe qualified as Safe
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Fail (runFailIO)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils (runTestBackground)
import System.Config qualified as Cfg


dev2 :: IO ()
dev2 = do
  _ <- Safe.try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ())
  ctx <- runEff $ runFailIO Cfg.getAppContext
  -- traceShowM ctx
  now <- getCurrentTime
  -- _ <- runTestBackground ctx $ BackgroundJobs.runHourlyJob now 18
  let pids = V.singleton $ UUIDId $ Unsafe.fromJust $ UUID.fromString "00000000-0000-0000-0000-000000000000"
  _ <- runTestBackground ctx $ BackgroundJobs.generateOtelFacetsBatch pids now

  pass
