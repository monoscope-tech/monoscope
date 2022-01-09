{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Lucid
import Network.Wai (Application)
import Relude
import Servant
  ( Get,
    Handler,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    Server,
    serve,
    Raw,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid
import Servant.Server.StaticFiles
import qualified Pages.Projects.CreateProject as CreateProject

--
-- API Section
--
type API =
  "projects" :> "new" :> Get '[HTML] (Html ())
  :<|> "assets" :> Raw

--
--
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  pure CreateProject.createProject
  :<|> serveDirectoryWebApp "./static/assets"
