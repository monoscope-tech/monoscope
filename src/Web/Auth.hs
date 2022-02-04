module Web.Auth where

import qualified Crypto.JOSE.Compact
import qualified Crypto.JOSE.JWK
import qualified Crypto.JWT
import Data.Aeson.Lens (key, _String)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Control.Lens as L
import Control.Monad.Trans.Either
import SessionCookies (addCookie, craftSessionCookie)
import Web.Cookie (SetCookie)
import Network.Wai (Application, Request)
import Relude hiding (hoistMaybe)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Config (AuthContext, DashboardM, EnvConfig, HeadersTriggerRedirect, authHandler, ctxToHandler, env, pool)
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import qualified Models.Users.Users as Users
import Database.PostgreSQL.Entity.DBT ( withPool)
import Optics.Operators
import Lucid
import Servant.HTML.Lucid
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
    QueryParam,
    Raw,
    ReqBody,
    ServerError (errHeaders),
    ServerT,
    StdMethod (GET),
    Verb,
    addHeader,
    err301,
    hoistServer,
    hoistServerWithContext,
    noHeader,
    serve,
    serveWithContext,
    throwError,
    type (:<|>) (..),
    type (:>),
  )


-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request Sessions.PersistentSession ': '[])
genAuthServerContext = authHandler :. EmptyContext

logoutH :: DashboardM (Html ())
logoutH = do
  envCfg <- asks env
  let redirectTo = envCfg ^. #auth0Domain <> "/v2/logout?client_id=" <> envCfg ^. #auth0ClientId <> "&returnTo=" <> envCfg ^. #auth0LogoutRedirect
  throwError $ err301 {errHeaders = [("Location", encodeUtf8 @Text @ByteString redirectTo)]}
  pure $ toHtml ""

loginH :: DashboardM (Html ())
loginH = do
  envCfg <- asks env
  newUID <- liftIO UUIDV4.nextRandom
  let state = UUID.toText newUID
  let redirectTo = envCfg ^. #auth0Domain <> "/authorize?response_type=code&client_id=" <> envCfg ^. #auth0ClientId <> "&redirect_uri=" <> envCfg ^. #auth0Callback <> "&state=" <> state <> "&scope=openid profile email"
  throwError $ err301 {errHeaders = [("Location", encodeUtf8 @Text @ByteString redirectTo)]}
  pure $ toHtml ""

-- authCallbackH will accept a request with code and state, and use that code to queery auth- for an auth token, and then for user info
-- it then uses this user info to check if we already have that user in our db. And if we have that user in the db,
-- would simply create a new session. If we dont have the user (by that email), then create the user and still create a session with that user.
authCallbackH ::
  Maybe Text ->
  Maybe Text ->
  DashboardM
    ( Headers
        '[Header "Location" String, Header "Set-Cookie" SetCookie]
        (Html ())
    )
authCallbackH codeM stateM = do
  envCfg <- asks env
  pool <- asks pool

  resp <- runEitherT $ do
    state <- hoistMaybe "invalid state " stateM
    code <- hoistMaybe "invalid code " codeM
    r <-
      liftIO $
        post
          (toString $ envCfg ^. #auth0Domain <> "/oauth/token")
          ( [ "grant_type" := ("authorization_code" :: String),
              "client_id" := envCfg ^. #auth0ClientId,
              "client_secret" := envCfg ^. #auth0Secret,
              "code" := code,
              "redirect_uri" := envCfg ^. #auth0Callback
            ] ::
              [FormParam]
          )
    accessToken <- hoistMaybe "invalid access_token in response" $ r L.^? responseBody . key "access_token" . _String
    let opts = defaults & header "Authorization" L..~ ["Bearer " <> encodeUtf8 @Text @ByteString accessToken]
    r <- liftIO $ getWith opts (toString $ envCfg ^. #auth0Domain <> "/userinfo")
    let email = fromMaybe "" $ r L.^? responseBody . key "email" . _String
    let firstName = fromMaybe "" $ r L.^? responseBody . key "first_name" . _String
    let lastName = fromMaybe "" $ r L.^? responseBody . key "last_name" . _String
    let picture = fromMaybe "" $ r L.^? responseBody . key "picture" . _String

    user <- liftIO $ Users.createUser firstName lastName picture email

    liftIO $
      withPool pool $ do
        userM <- Users.userByEmail email
        userId <- case userM of
          Nothing -> Users.insertUser user >> pure (user ^. #id)
          Just user -> pure $ user ^. #id
        persistentSessId <- liftIO Sessions.newPersistentSessionId
        newSession <- liftIO $ Sessions.newPersistentSession userId persistentSessId
        Sessions.insertSession newSession
        pure persistentSessId

  case resp of
    Left err -> (throwError $ err301 {errHeaders = [("Location", "/login")]}) >> pure (noHeader $ noHeader $ toHtml "")
    Right persistentSessId -> pure $ addHeader "/" $ addCookie (craftSessionCookie persistentSessId False) $ toHtml ""
