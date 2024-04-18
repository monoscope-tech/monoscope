{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Web.Auth (
  logoutH,
  loginRedirectH,
  loginH,
  authCallbackH,
  authHandler,
  APItoolkitAuthContext,
) where

import Control.Error (note)
import Control.Lens qualified as L
import Control.Monad.Except qualified as T
import Data.Aeson.Lens (key, _String)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity.DBT (withPool)
import Effectful (
  Eff,
  Effect,
  IOE,
  MonadIO (liftIO),
  runEff,
  type (:>),
 )
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Reader.Static (ask, asks)
import Log (Logger)
import Lucid (Html)
import Lucid.Html5 (
  a_,
  body_,
  content_,
  head_,
  href_,
  html_,
  httpEquiv_,
  meta_,
 )
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Network.HTTP.Types (hCookie)
import Network.Wai (Request (requestHeaders))
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Pkg.ConvertKit qualified as ConvertKit
import Relude (
  Applicative (pure),
  Bool (..),
  ByteString,
  ConvertUtf8 (decodeUtf8, encodeUtf8),
  Either (Left, Right),
  Functor (fmap),
  IO,
  Maybe (..),
  Monad ((>>)),
  Semigroup ((<>)),
  String,
  Text,
  ToString (toString),
  Traversable (mapM),
  Type,
  either,
  fromMaybe,
  hoistEither,
  join,
  maybe,
  putStrLn,
  runExceptT,
  show,
  ($),
  (&),
  (.),
  (<$>),
 )
import Servant (
  Header,
  Headers,
  NoContent (..),
  addHeader,
  noHeader,
 )
import Servant qualified
import Servant.Server (
  Handler,
  ServerError (errBody, errHeaders),
  err302,
  err403,
 )
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import SessionCookies (craftSessionCookie, emptySessionCookie)
import System.Config (
  AuthContext (config, env, pool),
  EnvConfig (
    auth0Callback,
    auth0ClientId,
    auth0Domain,
    auth0LogoutRedirect,
    auth0Secret,
    convertkitApiKey,
    environment
  ),
 )
import System.Logging qualified as Logging
import System.Types (ATBaseCtx)
import Web.Cookie (Cookies, SetCookie, parseCookies)


type APItoolkitAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)


authHandler :: Logger -> AuthContext -> APItoolkitAuthContext
authHandler logger env =
  mkAuthHandler \request -> do
    handler request
      & Logging.runLog (show env.config.environment) logger
      & DB.runDB env.pool
      & effToHandler
  where
    handler :: Request -> Eff '[Log, DB, Error ServerError, IOE] (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
    handler req = do
      let cookies = getCookies req
      mbPersistentSessionId <- handlerToEff $ getSessionId cookies
      let isSidebarClosed = sidebarClosedFromCookie cookies
      mbPersistentSession <- join <$> mapM (dbtToEff . Sessions.getPersistentSession) mbPersistentSessionId
      -- TODO: replace with the user in persistent session?
      mUserInfo <- fetchUser mbPersistentSession
      requestID <- liftIO $ getRequestID req
      (user, sessionId) <- do
        case mUserInfo of
          Nothing -> do
            throwError $ err302{errHeaders = [("Location", "/to_login")]}
          Just (user, userSession) -> do
            pure (user, userSession.id)
      let sessionCookie = Sessions.craftSessionCookie sessionId False
      pure $ Sessions.addCookie sessionCookie (Sessions.Session{persistentSession = mbPersistentSession, ..})


getCookies :: Request -> Cookies
getCookies req =
  maybe [] parseCookies (List.lookup hCookie headers)
  where
    headers = requestHeaders req


getRequestID :: Request -> IO Text
getRequestID req = do
  let headers = requestHeaders req
  case List.lookup "X-Request-ID" headers of
    Nothing -> fmap UUID.toText UUID.nextRandom
    Just requestID -> pure $ decodeUtf8 requestID


sidebarClosedFromCookie :: Cookies -> Bool
sidebarClosedFromCookie cookies = case List.lookup "sidebarClosed" cookies of
  Just "true" -> True
  Just _ -> False
  Nothing -> False


getSessionId :: Cookies -> Handler (Maybe Sessions.PersistentSessionId)
getSessionId cookies =
  case List.lookup "apitoolkit_session" cookies of
    Nothing -> pure Nothing
    Just i ->
      case Sessions.PersistentSessionId <$> UUID.fromASCIIBytes i of
        Nothing -> pure Nothing
        Just sessionId -> pure (Just sessionId)


fetchUser :: (Error ServerError :> es, DB :> es) => Maybe Sessions.PersistentSession -> Eff es (Maybe (Users.User, Sessions.PersistentSession))
fetchUser Nothing = pure Nothing
fetchUser (Just userSession) = do
  user <- lookupUser userSession.userId
  pure (Just (user, userSession))


lookupUser :: (Error ServerError :> es, DB :> es) => Users.UserId -> Eff es Users.User
lookupUser uid = do
  result <- Users.userById uid
  case result of
    Nothing -> throwError (err403{errBody = "Invalid Cookie"})
    (Just user) -> pure user


handlerToEff
  :: forall (es :: [Effect]) (a :: Type)
   . Error ServerError :> es
  => Handler a
  -> Eff es a
handlerToEff handler = do
  v <- unsafeEff_ $ Servant.runHandler handler
  either throwError pure v


effToHandler
  :: forall (a :: Type)
   . ()
  => Eff '[Error ServerError, IOE] a
  -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $ computation
  either T.throwError pure v


logoutH
  :: ATBaseCtx
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          NoContent
      )
logoutH = do
  envCfg <- asks env
  let redirectTo = envCfg.auth0Domain <> "/v2/logout?client_id=" <> envCfg.auth0ClientId <> "&returnTo=" <> envCfg.auth0LogoutRedirect
  pure $ addHeader redirectTo $ addHeader Sessions.emptySessionCookie NoContent


loginRedirectH
  :: ATBaseCtx
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          NoContent
      )
loginRedirectH = do
  let redirectTo = "/login"
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


-- loginH
loginH
  :: ATBaseCtx
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          NoContent
      )
loginH = do
  envCfg <- asks env
  stateVar <- liftIO $ UUID.toText <$> UUIDV4.nextRandom
  let redirectTo = envCfg.auth0Domain <> "/authorize?response_type=code&client_id=" <> envCfg.auth0ClientId <> "&redirect_uri=" <> envCfg.auth0Callback <> "&state=" <> stateVar <> "&scope=openid profile email"
  pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


-- authCallbackH will accept a request with code and state, and use that code to queery auth- for an auth token, and then for user info
-- it then uses this user info to check if we already have that user in our db. And if we have that user in the db,
-- would simply create a new session. If we dont have the user (by that email), then create the user and still create a session with that user.
authCallbackH
  :: Maybe Text
  -> Maybe Text -- state variable from auth0
  -> ATBaseCtx
      ( Headers
          '[Header "Location" Text, Header "Set-Cookie" SetCookie]
          (Html ())
      )
authCallbackH codeM _ = do
  envCfg <- ask @AuthContext
  pool <- asks pool
  resp <- runExceptT do
    code <- hoistEither $ note "invalid code " codeM
    r <-
      liftIO $
        post
          (toString $ envCfg.config.auth0Domain <> "/oauth/token")
          ( [ "grant_type" := ("authorization_code" :: String)
            , "client_id" := envCfg.config.auth0ClientId
            , "client_secret" := envCfg.config.auth0Secret
            , "code" := code
            , "redirect_uri" := envCfg.config.auth0Callback
            ]
              :: [FormParam]
          )
    accessToken <- hoistEither $ note "invalid access_token in response" $ r L.^? responseBody . key "access_token" . _String
    let opts = defaults & header "Authorization" L..~ ["Bearer " <> encodeUtf8 @Text @ByteString accessToken]
    resp <- liftIO $ getWith opts (toString $ envCfg.config.auth0Domain <> "/userinfo")
    let email = fromMaybe "" $ resp L.^? responseBody . key "email" . _String
    let firstName = fromMaybe "" $ resp L.^? responseBody . key "given_name" . _String
    let lastName = fromMaybe "" $ resp L.^? responseBody . key "family_name" . _String
    -- TODO: For users with no profile photos or empty profile photos, use gravatars as their profile photo
    -- https://en.gravatar.com/site/implement/images/
    let picture = fromMaybe "" $ resp L.^? responseBody . key "picture" . _String
    (userId, persistentSessId) <- liftIO $
      withPool
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
    _ <- liftIO $ ConvertKit.addUser envCfg.config.convertkitApiKey email firstName lastName "" "" ""
    pure persistentSessId

  case resp of
    Left err -> putStrLn ("unable to process auth callback page " <> err) >> (throwError $ err302{errHeaders = [("Location", "/login?auth0_callback_failure")]}) >> pure (noHeader $ noHeader "")
    Right persistentSessId -> pure $
      addHeader "/" $
        addHeader
          (craftSessionCookie persistentSessId True)
          do
            html_ do
              head_ do
                meta_ [httpEquiv_ "refresh", content_ "1;url=/"]
              body_ do
                a_ [href_ "/"] "Continue to APIToolkit"
