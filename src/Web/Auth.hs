{-# LANGUAGE FlexibleContexts #-}

module Web.Auth where

import Config (AuthContext, DashboardM, EnvConfig, HeadersTriggerRedirect, env, pool)
import Control.Error (note)
import qualified Control.Lens as L
import Control.Monad.Trans.Either (hoistMaybe, runEitherT)
import qualified Crypto.JOSE.Compact
import qualified Crypto.JOSE.JWK
import qualified Crypto.JWT
import Data.Aeson.Lens (key, _String)
import qualified Data.Map.Strict as Map
import Data.Pool (Pool)
import Data.Time.LocalTime (getZonedTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import Lucid (Html, ToHtml (toHtml))
import Lucid.Html5
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Sessions as Sessions
import qualified Models.Users.Users as Users
import Network.Wai (Application, Request (requestHeaders))
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Optics.Operators ((^.))
import Relude
  ( Applicative (pure),
    Bool (False, True),
    ByteString,
    ConvertUtf8 (encodeUtf8),
    Either (Left, Right),
    Maybe (..),
    MaybeT (runMaybeT),
    Monad ((>>)),
    MonadIO (liftIO),
    Semigroup ((<>)),
    String,
    Text,
    ToString (toString),
    asks,
    either,
    error,
    fromMaybe,
    maybe,
    putStrLn,
    traceShowM,
    ($),
    (&),
    (.),
    (<$>),
  )
import Relude.String (show)
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
    err302,
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
import SessionCookies (craftSessionCookie, emptySessionCookie)
import Web.Cookie (SetCookie, parseCookies)
import Prelude (lookup)

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Pool Connection -> Context (AuthHandler Request Sessions.PersistentSession ': '[])
genAuthServerContext dbConn = authHandler dbConn :. EmptyContext

logoutH ::
  DashboardM
    ( Headers
        '[Header "Location" Text, Header "Set-Cookie" SetCookie]
        NoContent
    )
logoutH = do
  envCfg <- asks env
  let redirectTo = envCfg ^. #auth0Domain <> "/v2/logout?client_id=" <> envCfg ^. #auth0ClientId <> "&returnTo=" <> envCfg ^. #auth0LogoutRedirect
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent

loginRedirectH ::
  DashboardM
    ( Headers
        '[Header "Location" Text, Header "Set-Cookie" SetCookie]
        NoContent
    )
loginRedirectH = do
  envCfg <- asks env
  let redirectTo = "/login"
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent

-- loginH
loginH ::
  DashboardM
    ( Headers
        '[Header "Location" Text, Header "Set-Cookie" SetCookie]
        NoContent
    )
loginH = do
  envCfg <- asks env
  state <- liftIO $ UUID.toText <$> UUIDV4.nextRandom
  let redirectTo = envCfg ^. #auth0Domain <> "/authorize?response_type=code&client_id=" <> envCfg ^. #auth0ClientId <> "&redirect_uri=" <> envCfg ^. #auth0Callback <> "&state=" <> state <> "&scope=openid profile email"
  traceShowM redirectTo
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent

-- authCallbackH will accept a request with code and state, and use that code to queery auth- for an auth token, and then for user info
-- it then uses this user info to check if we already have that user in our db. And if we have that user in the db,
-- would simply create a new session. If we dont have the user (by that email), then create the user and still create a session with that user.
authCallbackH ::
  Maybe Text ->
  Maybe Text ->
  DashboardM
    ( Headers
        '[Header "Location" Text, Header "Set-Cookie" SetCookie]
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
        Sessions.insertSession persistentSessId userId (Sessions.SessionData Map.empty)
        pure persistentSessId

  traceShowM "RESP from auth callback"
  traceShowM resp

  case resp of
    Left err -> putStrLn ("unable to process auth callback page " <> err) >> (throwError $ err302 {errHeaders = [("Location", "/login?auth0_callback_failure")]}) >> pure (noHeader $ noHeader $ toHtml "")
    Right persistentSessId -> pure $
      addHeader "/" $
        addHeader (craftSessionCookie persistentSessId True) $ do
          html_ $ do
            head_ $ do
              meta_ [httpEquiv_ "refresh", content_ "1;url=/"]
            body_ $ do
              a_ [href_ "/"] "Continue to APIToolkit"

--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: Pool Connection -> AuthHandler Request Sessions.PersistentSession
authHandler conn = mkAuthHandler handler
  where
    -- instead of just redirecting, we could delete the cookie first?
    throw301 err = traceShowM err >> throwError $ err302 {errHeaders = [("Location", "/to_login")]}

    handler :: Request -> Handler Sessions.PersistentSession
    handler req = either throw301 (lookupAccount conn) $ do
      cookie <- note "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      note "Missing token in cookie" $ lookup "apitoolkit_session" $ parseCookies cookie

-- We need to handle errors for the persistent session better and redirect if there's an error
lookupAccount :: Pool Connection -> ByteString -> Handler Sessions.PersistentSession
lookupAccount conn key = do
  resp <- runEitherT $ do
    pid <- hoistMaybe "unable to convert cookie value to persistent session UUID" (Sessions.PersistentSessionId <$> UUID.fromASCIIBytes key)
    presistentSess <- liftIO $ withPool conn $ Sessions.getPersistentSession pid
    hoistMaybe "lookupAccount: invalid persistentID " presistentSess
  case resp of
    Left err -> do
      throwError $ err302 {errHeaders = [("Location", "/to_login")]}
    Right session -> pure session
