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
  )
import Servant.HTML.Lucid
import Servant.Server.StaticFiles

--
-- API Section
-- Capture "uuid" UUID.UUID :>
type API =
  "projects" :> "new" :> Get '[HTML] (Html ())
<<<<<<< Updated upstream
    :<|> "endpoints" :> "list" :> Get '[HTML] (Html ())
    :<|> "endpoints" :> Capture "uuid" UUID.UUID :> "details" :> Get '[HTML] (Html ())
||||||| constructed merge base
    :<|> "endpoints" :> "details" :> Get '[HTML] (Html ())
=======
    :<|> "endpoints" :> Capture "uuid" UUID.UUID :> "details" :> Get '[HTML] (Html ())
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
    :<|> pure EndpointList.endpointList
    :<|> endpointDetailsH
||||||| constructed merge base
    :<|> pure EndpointDetails.endpointDetails
=======
    :<|> endpointDetailsH
>>>>>>> Stashed changes
    :<|> serveDirectoryWebApp "./static/assets"

endpointDetailsH :: UUID.UUID -> Handler (Html ())
endpointDetailsH uuid = pure EndpointDetails.endpointDetails
