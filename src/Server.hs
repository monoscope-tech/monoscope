{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Data.UUID as UUID
import Lucid
import Network.Wai (Application)
import qualified Pages.Endpoints.EndpointDetails as EndpointDetails
import qualified Pages.Endpoints.EndpointList as EndpointList
import qualified Pages.Projects.CreateProject as CreateProject
import Relude
import Servant
  ( Capture,
    Get,
    Handler,
    JSON,
    NoContent (..),
    Post,
    Raw,
    ReqBody,
    Server,
    serve,
    FormUrlEncoded,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid
import Servant.Server.StaticFiles

--
-- API Section
type API 
      = "projects" :> "new" :> Get '[HTML] (Html ())
    :<|> "projects" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (Html ())
    :<|> "endpoints" :> "list" :> Get '[HTML] (Html ())
    :<|> "endpoints" :> Capture "uuid" UUID.UUID :> "details" :> Get '[HTML] (Html ())
    :<|> "assets" :> Raw

--
--
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  CreateProject.createProjectGetH
    :<|> CreateProject.createProjectPostH
    :<|> pure EndpointList.endpointList
    :<|> endpointDetailsH
    :<|> serveDirectoryWebApp "./static/assets"

endpointDetailsH :: UUID.UUID -> Handler (Html ())
endpointDetailsH uuid = pure EndpointDetails.endpointDetails
