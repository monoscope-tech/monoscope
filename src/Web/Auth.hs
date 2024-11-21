{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Web.Auth (
  logoutH,
  loginRedirectH,
  loginH,
  authCallbackH,
  sessionByID,
  authHandler,
  APItoolkitAuthContext,
  authorizeUserAndPersist,
) where

import Control.Error (note)
import Control.Lens qualified as L
import Control.Monad.Except qualified as T
import Data.Aeson.Lens (key, _String)
import Data.Effectful.UUID (UUIDEff)
import Data.Effectful.Wreq (HTTP)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Effectful (
  Eff,
  Effect,
  IOE,
  runEff,
  type (:>),
 )
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Reader.Static (ask, asks)
import Effectful.Time (Time)
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
import Models.Users.Sessions (craftSessionCookie, emptySessionCookie)
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Network.HTTP.Types (hCookie)
import Network.Wai (Request (requestHeaders))
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Pkg.ConvertKit qualified as ConvertKit
import Relude hiding (ask, asks)
import Servant (Header, Headers, NoContent (..), addHeader, noHeader)
import Servant qualified
import Servant.Server (Handler, ServerError (errHeaders), err302)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
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
  mkAuthHandler \request ->
    handler request
      & Logging.runLog (show env.config.environment) logger
      & DB.runDB env.pool
      & effToHandler
  where
    handler :: (DB :> es, Error ServerError :> es, IOE :> es) => Request -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
    handler req = do
      let cookies = getCookies req
      mbPersistentSessionId <- handlerToEff $ getSessionId cookies
      let isSidebarClosed = sidebarClosedFromCookie cookies
      requestID <- liftIO $ getRequestID req
      sessionByID mbPersistentSessionId requestID isSidebarClosed


sessionByID :: (DB :> es, Error ServerError :> es) => Maybe Sessions.PersistentSessionId -> Text -> Bool -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
sessionByID mbPersistentSessionId requestID isSidebarClosed = do
  mbPersistentSession <- join <$> mapM Sessions.getPersistentSession mbPersistentSessionId
  let mUser = mbPersistentSession <&> (.user.getUser)
  (user, sessionId, persistentSession) <- case (mUser, mbPersistentSession) of
    (Just user, Just userSession) -> pure (user, userSession.id, userSession)
    _ -> throwError $ err302{errHeaders = [("Location", "/to_login")]}
  let sessionCookie = Sessions.craftSessionCookie sessionId False
  pure $ Sessions.addCookie sessionCookie (Sessions.Session{persistentSession, ..})


getCookies :: Request -> Cookies
getCookies req = maybe [] parseCookies (L.lookup hCookie $ requestHeaders req)


getRequestID :: Request -> IO Text
getRequestID req = do
  let headers = requestHeaders req
  case L.lookup "X-Request-ID" headers of
    Nothing -> fmap UUID.toText UUID.nextRandom
    Just requestID -> pure $ decodeUtf8 requestID


sidebarClosedFromCookie :: Cookies -> Bool
sidebarClosedFromCookie cookies = case L.lookup "sidebarClosed" cookies of
  Just "true" -> True
  Just _ -> False
  Nothing -> False


getSessionId :: Cookies -> Handler (Maybe Sessions.PersistentSessionId)
getSessionId cookies = pure $ Sessions.PersistentSessionId <$> (UUID.fromASCIIBytes =<< L.lookup "apitoolkit_session" cookies)


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
      liftIO
        $ post
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
    lift $ authorizeUserAndPersist (Just envCfg.config.convertkitApiKey) firstName lastName picture email
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


authorizeUserAndPersist :: (DB :> es, UUIDEff :> es, HTTP :> es, Time :> es) => Maybe Text -> Text -> Text -> Text -> Text -> Eff es Sessions.PersistentSessionId
authorizeUserAndPersist convertkitApiKeyM firstName lastName picture email = do
  userM <- Users.userByEmail email
  userId <- case userM of
    Nothing -> do
      user <- Users.createUser firstName lastName picture email
      Users.insertUser user
      pure user.id
    Just user -> pure user.id
  persistentSessId <- Sessions.newPersistentSessionId
  Sessions.insertSession persistentSessId userId (Sessions.SessionData Map.empty)
  _ <- whenJust convertkitApiKeyM \ckKey -> ConvertKit.addUser ckKey email firstName lastName "" "" ""
  pure persistentSessId
