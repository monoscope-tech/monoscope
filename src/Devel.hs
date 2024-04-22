{-# OPTIONS_GHC -Wno-error=ambiguous-fields -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-error=unused-imports -Wno-unused-imports -Wno-incomplete-uni-patterns #-}

module Devel (dev) where

-- Devel is useful when doing local web/html development
-- where having fast reloads is important
--

import Data.Aeson.QQ (aesonQQ)
import Data.Default (def)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Network.Wai.Handler.Warp (run)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
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
import Relude hiding (get)
import System.Config
import System.Directory (doesFileExist)
import System.Envy (FromEnv (..), ReadShowVar (..), Var (..), decodeEnv, fromVar, toVar)


dev :: IO ()
dev = scotty 8000 $ do
  middleware $ staticPolicy (hasPrefix "assets" >-> addBase "static/public")
  middleware $ staticPolicy (hasPrefix "public" >-> addBase "static")
  get "/" $ do
    _ <- liftIO (try (Dotenv.loadFile Dotenv.defaultConfig) :: IO (Either SomeException ()))
    Right config <- liftIO (decodeEnv :: IO (Either String EnvConfig))
    let createPgConnIO = PG.connectPostgreSQL $ encodeUtf8 config.databaseUrl
    pool <- liftIO $ Pool.newPool $ Pool.defaultPoolConfig createPgConnIO PG.close (60 * 6) 100

    let Just col_id = UUID.fromText "be66540d-88ed-4424-a7c8-93d7dac632bf"
    collectionM <- withPool pool $ Testing.getCollectionById $ Testing.CollectionId col_id
    collection_steps <- withPool pool $ Testing.getCollectionSteps $ Testing.CollectionId col_id

    let bwconf =
          (def :: BWConfig)
            { sessM = Nothing
            , pageTitle = "Testing"
            }
    let collection = (def :: Testing.Collection){Testing.title = "Demo collection", Testing.description = "Description"}
    let pid = Projects.ProjectId UUID.nil
    let stepD =
          (def :: Testing.CollectionStepData)
            { Testing.get = Just "/bla/bla"
            }
    let step =
          (def :: Testing.CollectionStep)
            { Testing.stepData = stepD
            }
    html $ renderText $ bodyWrapper bwconf $ TestCollectionEditor.collectionPage pid collection (V.singleton step)
