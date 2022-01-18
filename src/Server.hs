{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server (app) where

import Data.UUID as UUID
import Lucid
import Network.Wai (Application)
import qualified Pages.Endpoints.EndpointDetails as EndpointDetails
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

import Types (ProjectDummy)
import Pages.Projects.CreateProject (connString, createProjectHandler, getProjectHandler)

--
-- API Section
-- Capture "uuid" UUID.UUID :>

-- servant allows creating a new rest verb with the desired status code
type ProjectCreated = Verb 'POST 201

type API =
  "projects" :> "new" :> Get '[HTML] (Html ())
    :<|> "endpoints" :> "details" :> Get '[HTML] (Html ())
    :<|> "assets" :> Raw
    <|> "projects" :> "create" :> ReqBody '[JSON] ProjectDummy :>  ProjectCreated '[JSON] UUID
    :<|> "projects" :> Capture "uuid" UUID :> Get '[JSON] ProjectDummy
    -- <|> "projects" :> "list" :> Get '[JSON] [ProjectDummy]

--
--
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- added Connection till I figure out how to implement a cleaner single point of connection with the database

server :: Server API
-- server :: Connection -> Server API
server =
  -- server connString =
  pure CreateProject.createProject
    :<|> pure EndpointDetails.endpointDetails
    :<|> serveDirectoryWebApp "./static/assets"
    :<|> (createProjectHandler connString) 
    :<|> (fetchProjectHandler connString) 
  
