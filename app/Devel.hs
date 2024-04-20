module Devel where

-- Devel is useful when doing local web/html development 
-- where having fast reloads is important
--

import Relude hiding (get)
import Relude.Unsafe qualified as Unsafe
import Network.Wai.Handler.Warp (run)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Web.Scotty
import Data.Default (def)
import Models.Tests.Testing qualified as Testing
import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Data.Vector qualified as V
import Data.Aeson.QQ (aesonQQ)
import Lucid
import qualified Data.Text.Lazy.IO as TIO
import System.Directory (doesFileExist)
import Network.HTTP.Types.Status


dev :: IO ()
dev = scotty 8000 $ do
    get "/assets/*splat" $ do
        fileName <- pathParam "splat"
        let filePath = "static/public/assets/" <> toString @Text fileName
        traceShowM filePath
        file filePath
    get "/public/*splat" $ do
        fileName <- pathParam "splat"
        let filePath = "static/public/" <> toString @Text fileName
        traceShowM filePath
        file filePath
    get "/" $ do
        let bwconf =
              (def :: BWConfig)
                { sessM = Nothing 
                , pageTitle = "Testing"
                }
        let collection = (def::Testing.Collection){Testing.title="Demo collection", Testing.description="Description"}
        let pid = Projects.ProjectId UUID.nil
        let step = (def::Testing.CollectionStep){
          Testing.stepData = [aesonQQ|
            {
            "GET": "/test/uri"
            }
            |]
                                                } 
        html $ renderText $ bodyWrapper bwconf $ TestCollectionEditor.collectionPage pid collection (V.singleton  step)

serveStatic :: FilePath -> ActionM ()
serveStatic filePath = do
    fileExists <- liftIO $ doesFileExist filePath
    if fileExists
        then do
            fileContents <- liftIO $ TIO.readFile filePath
            html fileContents
        else status status404 
