{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Server (app) where

import Config (AuthContext, DashboardM, HeadersTriggerRedirect, authHandler, ctxToHandler)
import Data.UUID as UUID
import Lucid
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import Network.Wai (Application, Request)
import qualified Pages.Dashboard as Dashboard
import qualified Pages.Endpoints.EndpointDetails as EndpointDetails
import qualified Pages.Endpoints.EndpointList as EndpointList
import qualified Pages.Projects.CreateProject as CreateProject
import qualified Pages.Projects.ListProjects as ListProjects
import Relude
import Servant
  ( AuthProtect,
    Capture,
    Context (EmptyContext, (:.)),
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
    hoistServerWithContext,
    serve,
    serveWithContext,
    type (:<|>) (..),
    type (:>),
  )
import Servant.HTML.Lucid
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Servant.Server.StaticFiles

--
-- API Section
type ProtectedAPI =
  "p" :> "new" :> Get '[HTML] (Html ()) -- p represents project
    :<|> "p" :> "new" :> ReqBody '[FormUrlEncoded] CreateProject.CreateProjectForm :> Post '[HTML] (HeadersTriggerRedirect (Html ()))
    :<|> "p" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "dashboard" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Get '[HTML] (Html ())
    :<|> "p" :> Capture "projectID" Projects.ProjectId :> "endpoints" :> Capture "uuid" UUID.UUID :> Get '[HTML] (Html ())

type PublicAPI =
  "assets" :> Raw

type API =
  AuthProtect "cookie-auth" :> ProtectedAPI
    :<|> PublicAPI

type instance AuthServerData (AuthProtect "cookie-auth") = Sessions.PersistentSession

--
--
app :: AuthContext -> Application
app ctx = serveWithContext api genAuthServerContext $ hoistServerWithContext api ctxProxy (ctxToHandler ctx) server

api :: Proxy API
api = Proxy

ctxProxy :: Proxy '[AuthHandler Request Sessions.PersistentSession]
ctxProxy = Proxy

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We don't worry about the authentication instrumentation here,
-- that is taken care of by supplying context
protectedServer :: Sessions.PersistentSession -> ServerT ProtectedAPI DashboardM
protectedServer _ =
  CreateProject.createProjectGetH
    :<|> CreateProject.createProjectPostH
    :<|> ListProjects.listProjectsGetH
    :<|> Dashboard.dashboardGetH
    :<|> EndpointList.endpointListH
    :<|> EndpointDetails.endpointDetailsH

publicServer :: ServerT PublicAPI DashboardM
publicServer = serveDirectoryWebApp "./static/assets"

server :: ServerT API DashboardM
server = protectedServer :<|> publicServer

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request Sessions.PersistentSession ': '[])
genAuthServerContext = authHandler :. EmptyContext
