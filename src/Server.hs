{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Config (AuthContext, DashboardM, HeadersTriggerRedirect, ctxToHandler)
import Data.UUID as UUID
import Lucid
import qualified Models.Projects.Projects as Projects
import Network.Wai (Application)
import qualified Pages.Dashboard as Dashboard
import qualified Pages.Endpoints.EndpointDetails as EndpointDetails
import qualified Pages.Endpoints.EndpointList as EndpointList
import qualified Pages.Projects.CreateProject as CreateProject
import qualified Pages.Projects.ListProjects as ListProjects
import Relude
import Servant
  ( Capture,
    FormUrlEncoded,
    Get,
    Handler,
    Header,
    Headers,
    JSON,
    NoContent (..),
    Post,
    Raw,
    ReqBody,
    Server,
    ServerT,
    hoistServer,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid
import Servant.Server.StaticFiles

--
-- API Section
type API =
  "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
    :<|> "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (HeadersTriggerRedirect (Html ()))
    :<|> "p" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "dashboard" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Capture "uuid" UUID.UUID :> Get '[HTML] (Html ())
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
    :<|> ListProjects.listProjectsGetH
    :<|> Dashboard.dashboardGetH
    :<|> EndpointList.endpointListH
    :<|> EndpointDetails.endpointDetailsH
    :<|> serveDirectoryWebApp "./static/assets"
