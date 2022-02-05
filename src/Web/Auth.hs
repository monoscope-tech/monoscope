module Web.Auth where

import Config (AuthContext, DashboardM, EnvConfig, HeadersTriggerRedirect, authHandler, ctxToHandler, env, pool)
import qualified Control.Lens as L
import Control.Monad.Trans.Either (hoistMaybe, runEitherT)
import qualified Crypto.JOSE.Compact
import qualified Crypto.JOSE.JWK
import qualified Crypto.JWT
import Data.Aeson.Lens (key, _String)
import Data.Pool (Pool)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import Lucid (Html, ToHtml (toHtml))
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import qualified Models.Users.Users as Users
import Network.Wai (Application, Request)
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Optics.Operators ((^.))
import Relude
  ( Applicative (pure),
    Bool (False, True),
    ByteString,
    ConvertUtf8 (encodeUtf8),
    Either (Left, Right),
    Maybe (..),
    Monad ((>>)),
    MonadIO (liftIO),
    Semigroup ((<>)),
    String,
    Text,
    ToString (toString),
    asks,
    fromMaybe,
    putStrLn,
    ($),
    (&),
    (.),
  )
import Servant
  ( Capture,
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
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import SessionCookies (addCookie, craftSessionCookie)
import Web.Cookie (SetCookie)

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Pool Connection -> Context (AuthHandler Request Sessions.PersistentSession ': '[])
genAuthServerContext dbConn = (authHandler dbConn) :. EmptyContext

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
    let firstName = fromMaybe "" $ r L.^? responseBody . key "given_name" . _String
    let lastName = fromMaybe "" $ r L.^? responseBody . key "family_name" . _String
    -- TODO: For users with no profile photos or empty profile photos, use gravatars as their profile photo
    -- https://en.gravatar.com/site/implement/images/
    let picture = fromMaybe "" $ r L.^? responseBody . key "picture" . _String

    liftIO $
      withPool pool $ do
        userM <- Users.userByEmail email
        userId <- case userM of
          Nothing -> do
            user <- liftIO $ Users.createUser firstName lastName picture email
            Users.insertUser user
            pure (user ^. #id)
          Just user -> pure $ user ^. #id
        persistentSessId <- liftIO Sessions.newPersistentSessionId
        newSession <- liftIO $ Sessions.newPersistentSession userId persistentSessId
        Sessions.insertSession newSession
        pure persistentSessId

  case resp of
    Left err -> putStrLn ("unable to process auth callback page " <> err) >> (throwError $ err301 {errHeaders = [("Location", "/login?auth0_callback_failure")]}) >> pure (noHeader $ noHeader $ toHtml "")
    Right persistentSessId -> pure $ addHeader "/" $ addCookie (craftSessionCookie persistentSessId True) $ toHtml ""
