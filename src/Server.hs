{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Data.UUID as UUID
import Lucid
import Network.Wai (Application)
import qualified Pages.Endpoints.EndpointDetails as EndpointDetails
import qualified Pages.Projects.ProjectView as ProjectView
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
    ServerT,
    Headers,
    Header,
    serve,
    hoistServer,
    FormUrlEncoded,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid
import Servant.Server.StaticFiles
import  Config   (ctxToHandler, DashboardM, AuthContext, OurHeaders)



--
-- API Section
type API 
      = "projects" :> "new" :> Get '[HTML] (OurHeaders (Html ()))
    :<|> "projects" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (Html ())
    :<|> "endpoints" :> "list" :> Get '[HTML] (Html ())
    :<|> "projects" :> "view" :> Get '[HTML] (Html ())
    :<|> "endpoints" :> Capture "uuid" UUID.UUID :> "details" :> Get '[HTML] (Html ())
    :<|> "assets" :> Raw

--
--
app :: AuthContext -> Application
app ctx = serve api $ hoistServer api (ctxToHandler ctx) server

api :: Proxy API
api = Proxy

server :: ServerT API DashboardM
server =
  CreateProject.createProjectGetH
    :<|> CreateProject.createProjectPostH
    :<|> pure EndpointList.endpointList
    :<|> pure ProjectView.projectView
    :<|> endpointDetailsH
    :<|> serveDirectoryWebApp "./static/assets"

endpointDetailsH :: UUID.UUID -> DashboardM (Html ())
endpointDetailsH uuid = pure EndpointDetails.endpointDetails
