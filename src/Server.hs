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
import qualified Pages.Projects.ListProjects as ListProjects
import qualified Pages.Dashboard as Dashboard
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
import  Config   (ctxToHandler, DashboardM, AuthContext, HeadersTriggerRedirect)
<<<<<<< HEAD


--
-- API Section
type API 
      =  "projects" :> "new" :> Get '[HTML] (Html ()) 
    :<|> "projects" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (HeadersTriggerRedirect (Html ()))
    :<|> "projects" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "dashboard" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID"  Projects.ProjectId :> "endpoints" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Capture "uuid" UUID.UUID :> "details" :> Get '[HTML] (Html ())
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
<<<<<<< HEAD
    :<|> ListProjects.listProjectsGetH
    :<|> Dashboard.dashboardGetH
    :<|> EndpointList.endpointListH
    :<|> EndpointDetails.endpointDetailsH
    :<|> serveDirectoryWebApp "./static/assets"

