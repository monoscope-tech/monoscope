{-# LANGUAGE FlexibleContexts #-}

module Web.Auth (logoutH, loginRedirectH, loginH, authCallbackH, genAuthServerContext) where

import Colog (LogAction)
import Colog.Core ((<&))
import Config (DashboardM, env, pool)
import Control.Error (note)
import Control.Lens qualified as L
import Data.Aeson.Lens (key, _String)
import Data.Map.Strict qualified as Map
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import Lucid (Html)
import Lucid.Html5
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Network.Wai (Request (requestHeaders))
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Optics.Operators ((^.))
import Pkg.ConvertKit qualified as ConvertKit
import Relude
import Servant (
  Context (EmptyContext, (:.)),
  Handler,
  Header,
  Headers,
  NoContent (..),
  ServerError (errHeaders),
  addHeader,
  err302,
  noHeader,
  throwError,
 )
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import SessionCookies (craftSessionCookie, emptySessionCookie)
import Web.Cookie (SetCookie, parseCookies)
import Prelude (lookup)


-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: LogAction IO String -> Pool Connection -> Context (AuthHandler Request Sessions.PersistentSession ': '[])
genAuthServerContext logger dbConn = authHandler logger dbConn :. EmptyContext


logoutH
  :: DashboardM
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          NoContent
      )
logoutH = do
  envCfg <- asks env
  let redirectTo = envCfg ^. #auth0Domain <> "/v2/logout?client_id=" <> envCfg ^. #auth0ClientId <> "&returnTo=" <> envCfg ^. #auth0LogoutRedirect
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


loginRedirectH
  :: DashboardM
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          NoContent
      )
loginRedirectH = do
  let redirectTo = "/login"
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


-- loginH
loginH
  :: DashboardM
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          NoContent
      )
loginH = do
  envCfg <- asks env
  stateVar <- liftIO $ UUID.toText <$> UUIDV4.nextRandom
  let redirectTo = envCfg ^. #auth0Domain <> "/authorize?response_type=code&client_id=" <> envCfg ^. #auth0ClientId <> "&redirect_uri=" <> envCfg ^. #auth0Callback <> "&state=" <> stateVar <> "&scope=openid profile email"
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


-- authCallbackH will accept a request with code and state, and use that code to queery auth- for an auth token, and then for user info
-- it then uses this user info to check if we already have that user in our db. And if we have that user in the db,
-- would simply create a new session. If we dont have the user (by that email), then create the user and still create a session with that user.
authCallbackH
  :: Maybe Text
  -> Maybe Text -- state variable from auth0
  -> DashboardM
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          (Html ())
      )
authCallbackH codeM _ = do
  envCfg <- asks env
  pool <- asks pool
  resp <- runExceptT do
    code <- hoistEither $ note "invalid code " codeM
    r <-
      liftIO $
        post
          (toString $ envCfg ^. #auth0Domain <> "/oauth/token")
          ( [ "grant_type" := ("authorization_code" :: String)
            , "client_id" := envCfg ^. #auth0ClientId
            , "client_secret" := envCfg ^. #auth0Secret
            , "code" := code
            , "redirect_uri" := envCfg ^. #auth0Callback
            ]
              :: [FormParam]
          )
    accessToken <- hoistEither $ note "invalid access_token in response" $ r L.^? responseBody . key "access_token" . _String
    let opts = defaults & header "Authorization" L..~ ["Bearer " <> encodeUtf8 @Text @ByteString accessToken]
    resp <- liftIO $ getWith opts (toString $ envCfg ^. #auth0Domain <> "/userinfo")
    let email = fromMaybe "" $ resp L.^? responseBody . key "email" . _String
    let firstName = fromMaybe "" $ resp L.^? responseBody . key "given_name" . _String
    let lastName = fromMaybe "" $ resp L.^? responseBody . key "family_name" . _String
    -- TODO: For users with no profile photos or empty profile photos, use gravatars as their profile photo
    -- https://en.gravatar.com/site/implement/images/
    let picture = fromMaybe "" $ resp L.^? responseBody . key "picture" . _String
    (userId, persistentSessId) <- liftIO
      $ withPool
        pool
        do
          userM <- Users.userByEmail email
          userId <- case userM of
            Nothing -> do
              user <- liftIO $ Users.createUser firstName lastName picture email
              Users.insertUser user
              pure user.id
            Just user -> pure user.id
          persistentSessId <- liftIO Sessions.newPersistentSessionId
          Sessions.insertSession persistentSessId userId (Sessions.SessionData Map.empty)
          pure (userId, persistentSessId)
    _ <- liftIO $ ConvertKit.addUser (envCfg ^. #convertkitApiKey) email firstName lastName "" "" ""
    pure persistentSessId

  case resp of
    Left err -> putStrLn ("unable to process auth callback page " <> err) >> (throwError $ err302{errHeaders = [("Location", "/login?auth0_callback_failure")]}) >> pure (noHeader $ noHeader "")
    Right persistentSessId -> pure
      $ addHeader "/"
      $ addHeader
        (craftSessionCookie persistentSessId True)
        do
          html_ do
            head_ do
              meta_ [httpEquiv_ "refresh", content_ "1;url=/"]
            body_ do
              a_ [href_ "/"] "Continue to APIToolkit"


--- | The auth handler wraps a function from Request -> Handler Account.
--- We look for a token in the request headers that we expect to be in the cookie.
--- The token is then passed to our `lookupAccount` function.
authHandler :: LogAction IO String -> Pool Connection -> AuthHandler Request Sessions.PersistentSession
authHandler logger conn = mkAuthHandler handler
  where
    -- instead of just redirecting, we could delete the cookie first?
    throw301 :: Text -> Handler Sessions.PersistentSession
    throw301 err = do
      liftIO (logger <& toString err)
      throwError $ err302{errHeaders = [("Location", "/to_login")]}

    handler :: Request -> Handler Sessions.PersistentSession
    handler req = either throw301 (lookupAccount conn) do
      cookie <- note "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      note "Missing token in cookie" $ lookup "apitoolkit_session" $ parseCookies cookie


-- We need to handle errors for the persistent session better and redirect if there's an error
lookupAccount :: Pool Connection -> ByteString -> Handler Sessions.PersistentSession
lookupAccount conn keyV = do
  resp <- runExceptT do
    pid <- hoistEither $ note @Text "unable to convert cookie value to persistent session UUID" (Sessions.PersistentSessionId <$> UUID.fromASCIIBytes keyV)
    presistentSess <- liftIO $ withPool conn $ Sessions.getPersistentSession pid
    hoistEither $ note "lookupAccount: invalid persistentID " presistentSess
  case resp of
    Left _ -> do
      throwError $ err302{errHeaders = [("Location", "/to_login")]}
    Right session -> pure session
