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
    type (:<|>) (..),
    type (:>),

    Verb,
  )
import Servant.HTML.Lucid
import Servant.Server.StaticFiles

import Database.PostgreSQL.Simple (Connection)

import Types (CreateProject)
import Pages.Projects.CreateProject (createProjectHandler, createProjectForm, getProjectHandler)

--
-- API Section
-- Capture "uuid" UUID.UUID :>

-- servant allows creating a new rest verb with the desired status code
type ProjectCreated = Verb 'POST 201

type API =
  "projects" :> "new" :> Get '[HTML] (Html ())
    :<|> "endpoints" :> "details" :> Get '[HTML] (Html ())
    :<|> "endpoints" :> "list" :> Get '[HTML] (Html ())
    :<|> "assets" :> Raw
    <|> "projects" :> "create" :> ReqBody '[FormUrlEncoded] createProjectForm :>  ProjectCreated '[JSON] UUID
    :<|> "projects" :> Capture "uuid" UUID :> Get '[JSON] CreateProject
    <|> "projects" :> "list" :> Get '[JSON] [CreateProject]

--
--
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  pure CreateProject.createProject
    :<|> pure EndpointDetails.endpointDetails
    :<|> pure EndpointList.endpointList
    :<|> serveDirectoryWebApp "./static/assets"
    :<|> createProjectHandler
    :<|> getProjectHandler
    :<|> getAllProjectHandler