{-# OPTIONS_GHC -Wno-error=ambiguous-fields -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-error=unused-imports -Wno-unused-imports -Wno-incomplete-uni-patterns #-}

module Devel (dev2) where

-- Devel is useful when doing local web/html development
-- where having fast reloads is important
--

import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Models.Projects.Projects qualified as Projects
import Network.Wai.Handler.Warp (run)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (get)
import Relude.Unsafe qualified as Unsafe
import Web.Scotty

-- import qualified Data.Text.Lazy.IO as TIO

import Configuration.Dotenv qualified as Dotenv
import Control.Exception (try)
import Data.Pool as Pool (Pool, defaultPoolConfig, newPool)
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple qualified as PG
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Static (addBase, hasPrefix, noDots, staticPolicy, (>->))
import System.Config
import System.Directory (doesFileExist)
import System.Envy (FromEnv (..), ReadShowVar (..), Var (..), decodeEnv, fromVar, toVar)

import BackgroundJobs qualified
import Control.Exception.Safe qualified as Safe
import Data.Time (getCurrentTime)
import Effectful
import Effectful.Fail (runFailIO)
import Models.Projects.Projects (ProjectId (ProjectId))
import Pkg.TestUtils (runTestBackground)
import System.Config qualified as Cfg


dev2 :: IO ()
dev2 = do
  _ <- Safe.try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ())
  ctx <- runEff $ runFailIO Cfg.getAppContext
  -- traceShowM ctx
  now <- getCurrentTime
  -- _ <- runTestBackground ctx $ BackgroundJobs.runHourlyJob now 18
  let pids = ["00000000-0000-0000-0000-000000000000"]
  _ <- runTestBackground ctx $ BackgroundJobs.generateOtelFacetsBatch pids now

  pass
