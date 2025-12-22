{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
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
import Data.Effectful.UUID (UUIDEff, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
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
import Effectful.PostgreSQL (WithConnection, runWithConnectionPool)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask, asks)
import Effectful.Time (Time, runTime)
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
import Network.HTTP.Types (hAuthorization, hCookie)
import Network.Wai (Request (rawPathInfo, rawQueryString, requestHeaders))
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Pkg.ConvertKit qualified as ConvertKit
import Relude hiding (ask, asks)
import Servant (Header, Headers, NoContent (..), addHeader, noHeader)
import Servant qualified
import Servant.Server (Handler, ServerError (errHeaders), err302, err401)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Config (
  AuthContext (config, env, pool),
  EnvConfig (
    auth0Callback,
    auth0ClientId,
    auth0Domain,
    auth0LogoutRedirect,
    auth0Secret,
    basicAuthEnabled,
    basicAuthPassword,
    basicAuthUsername,
    convertkitApiKey,
    environment,
    logLevel
  ),
 )
import System.Logging qualified as Logging
import System.Types (ATBaseCtx)
import Utils (escapedQueryPartial)
import Web.Cookie (Cookies, SetCookie, parseCookies)
import "base64" Data.ByteString.Base64 qualified as B64


type APItoolkitAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)


validateBasicAuth :: EnvConfig -> ByteString -> Maybe (Text, Text)
validateBasicAuth config authHeader = do
  let prefix = "Basic "
  let authHeaderText = decodeUtf8 authHeader
  stripped <- T.stripPrefix prefix authHeaderText
  let decoded = B64.decodeBase64Lenient (encodeUtf8 stripped)
  let decodedText = decodeUtf8 decoded
  case T.splitOn ":" decodedText of
    [username, password] ->
      if username == config.basicAuthUsername && password == config.basicAuthPassword
        then Just (username, password)
        else Nothing
    _ -> Nothing


authHandler :: Logger -> AuthContext -> APItoolkitAuthContext
authHandler logger env =
  mkAuthHandler \request ->
    handler request
      & Logging.runLog (show env.config.environment) logger env.config.logLevel
      & runWithConnectionPool env.pool
      & runHTTPWreq
      & runUUID
      & runTime
      & effToHandler
  where
    handler :: (Error ServerError :> es, HTTP :> es, IOE :> es, Time :> es, UUIDEff :> es, WithConnection :> es) => Request -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
    handler req = do
      -- Check if basic auth is enabled and try to authenticate
      if env.config.basicAuthEnabled
        then do
          let authHeader = L.lookup hAuthorization $ requestHeaders req
          case authHeader >>= validateBasicAuth env.config of
            Just (username, _password) -> do
              -- Basic auth successful, create a session for the basic auth user
              let isSidebarClosed = sidebarClosedFromCookie $ getCookies req
              let theme = themeFromCookie $ getCookies req
              requestID <- liftIO $ getRequestID req
              -- Use a fixed email for basic auth users
              sessId <- authorizeUserAndPersist Nothing "Basic" "Auth" "" (username <> "@basic-auth.local")
              sessionByID (Just sessId) requestID isSidebarClosed theme Nothing env.config.basicAuthEnabled
            Nothing -> do
              -- When basic auth is enabled, check if we have a valid cookie session
              -- If not, we should require basic auth instead of redirecting to Auth0
              let cookies = getCookies req
              mbPersistentSessionId <- handlerToEff $ getSessionId cookies
              mbPersistentSession <- join <$> mapM Sessions.getPersistentSession mbPersistentSessionId
              case mbPersistentSession of
                Just _ -> do
                  -- We have a valid cookie session, proceed normally
                  proceedWithCookieAuth req env.config.basicAuthEnabled
                Nothing -> do
                  -- No valid session and basic auth is enabled - return 401
                  throwError $ err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}
        else
          -- Basic auth not enabled, use normal cookie auth
          proceedWithCookieAuth req env.config.basicAuthEnabled

    proceedWithCookieAuth req basicAuthEnabledFlag = do
      let cookies = getCookies req
      mbPersistentSessionId <- handlerToEff $ getSessionId cookies
      let isSidebarClosed = sidebarClosedFromCookie cookies
      let theme = themeFromCookie cookies
      requestID <- liftIO $ getRequestID req
      sessionByID mbPersistentSessionId requestID isSidebarClosed theme (Just $ getRequestUrl req) basicAuthEnabledFlag


sessionByID :: (Error ServerError :> es, HTTP :> es, IOE :> es, Time :> es, UUIDEff :> es, WithConnection :> es) => Maybe Sessions.PersistentSessionId -> Text -> Bool -> Text -> Maybe ByteString -> Bool -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
sessionByID mbPersistentSessionId requestID isSidebarClosed theme url basicAuthEnabled = do
  mbPersistentSession <- join <$> mapM Sessions.getPersistentSession mbPersistentSessionId
  let mUser = mbPersistentSession <&> (.user.getUser)
  (user, sessionId, persistentSession) <- case (mUser, mbPersistentSession) of
    (Just user, Just userSession) -> pure (user, userSession.id, userSession)
    _ -> do
      case url of
        Just u -> do
          if T.isInfixOf "/p/00000000-0000-0000-0000-000000000000" (decodeUtf8 u) || T.isInfixOf "pid=00000000-0000-0000-0000-000000000000" (decodeUtf8 u)
            then do
              sessId <- authorizeUserAndPersist Nothing "Guest" "User" "" "hello@apitoolkit.io"
              mbPess <- join <$> mapM Sessions.getPersistentSession (Just sessId)
              let mU = mbPess <&> (.user.getUser)
              case (mU, mbPess) of
                (Just uu, Just uSess) -> pure (uu, uSess.id, uSess)
                _ ->
                  if basicAuthEnabled
                    then throwError $ err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}
                    else throwError $ err302{errHeaders = [("Location", "/to_login?redirect_to=" <> fromMaybe "" url)]}
            else
              if basicAuthEnabled
                then throwError $ err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}
                else throwError $ err302{errHeaders = [("Location", "/login?redirect_to=" <> fromMaybe "" url)]}
        Nothing ->
          if basicAuthEnabled
            then throwError $ err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}
            else throwError $ err302{errHeaders = [("Location", "/to_login?redirect_to=" <> fromMaybe "" url)]}
  let sessionCookie = Sessions.craftSessionCookie sessionId False
  pure $ Sessions.addCookie sessionCookie (Sessions.Session{persistentSession, ..})


getCookies :: Request -> Cookies
getCookies req = maybe [] parseCookies (L.lookup hCookie $ requestHeaders req)


getRequestUrl :: Request -> ByteString
getRequestUrl req = rawPathInfo req <> rawQueryString req


getRequestID :: Request -> IO Text
getRequestID req = do
  let headers = requestHeaders req
  case L.lookup "X-Request-ID" headers of
    Nothing -> fmap UUID.toText UUID.nextRandom
    Just requestID -> pure $ decodeUtf8 requestID


sidebarClosedFromCookie :: Cookies -> Bool
sidebarClosedFromCookie cookies = case L.lookup "isSidebarClosed" cookies of
  Just "true" -> True
  Just _ -> False
  Nothing -> False


themeFromCookie :: Cookies -> Text
themeFromCookie cookies = case L.lookup "theme" cookies of
  Just "dark" -> "dark"
  Just "light" -> "light"
  Just _ -> "dark"
  Nothing -> "dark"


getSessionId :: Cookies -> Handler (Maybe Sessions.PersistentSessionId)
getSessionId cookies = pure $ Sessions.PersistentSessionId <$> (UUID.fromASCIIBytes =<< L.lookup "monoscope_session" cookies)


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


logoutH :: ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
logoutH = do
  envCfg <- asks env
  let redirectTo = envCfg.auth0Domain <> "/v2/logout?client_id=" <> envCfg.auth0ClientId <> "&returnTo=" <> envCfg.auth0LogoutRedirect
  pure $ addHeader redirectTo $ addHeader Sessions.emptySessionCookie NoContent


loginRedirectH :: Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
loginRedirectH redirectToM = do
  envCfg <- asks env
  -- If basic auth is enabled, return 401 instead of redirecting to /login
  if envCfg.basicAuthEnabled
    then throwError $ err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}
    else do
      let redirectTo = "/login?redirect_to=" <> fromMaybe "/" redirectToM
      pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


-- loginH
loginH
  :: Maybe Text
  -> ATBaseCtx
       ( Headers
           '[Header "Location" Text, Header "Set-Cookie" SetCookie]
           NoContent
       )
loginH redirectToM = do
  envCfg <- asks env
  -- If basic auth is enabled, return 401 instead of redirecting to OAuth
  if envCfg.basicAuthEnabled
    then throwError $ err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}
    else do
      stateVar <- liftIO $ UUID.toText <$> UUIDV4.nextRandom
      let escapedUri = escapedQueryPartial $ envCfg.auth0Callback <> "?redirect_to=" <> fromMaybe "/" redirectToM
      let redirectTo = envCfg.auth0Domain <> "/authorize?response_type=code&client_id=" <> envCfg.auth0ClientId <> "&redirect_uri=" <> escapedUri <> "&state=" <> stateVar <> "&scope=openid profile email"
      pure $ addHeader redirectTo $ addHeader emptySessionCookie NoContent


-- authCallbackH will accept a request with code and state, and use that code to queery auth- for an auth token, and then for user info
-- it then uses this user info to check if we already have that user in our db. And if we have that user in the db,
-- would simply create a new session. If we dont have the user (by that email), then create the user and still create a session with that user.
authCallbackH
  :: Maybe Text
  -> Maybe Text -- state variable from auth0
  -> Maybe Text
  -> ATBaseCtx
       ( Headers
           '[Header "Location" Text, Header "Set-Cookie" SetCookie]
           (Html ())
       )
authCallbackH codeM _ redirectToM = do
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
    let picture = fromMaybe "" $ resp L.^? responseBody . key "picture" . _String
    lift $ authorizeUserAndPersist (Just envCfg.config.convertkitApiKey) firstName lastName picture email
  case resp of
    Left err -> putStrLn ("unable to process auth callback page " <> err) >> (throwError $ err302{errHeaders = [("Location", "/login?auth0_callback_failure")]}) >> pure (noHeader $ noHeader "")
    Right persistentSessId -> pure
      $ addHeader (fromMaybe "/" redirectToM)
      $ addHeader
        (craftSessionCookie persistentSessId True)
        do
          html_ do
            head_ do
              meta_ [httpEquiv_ "refresh", content_ "1;url=/"]
            body_ do
              a_ [href_ $ fromMaybe "/" redirectToM] "Continue to APIToolkit"


authorizeUserAndPersist :: (HTTP :> es, IOE :> es, Time :> es, UUIDEff :> es, WithConnection :> es) => Maybe Text -> Text -> Text -> Text -> Text -> Eff es Sessions.PersistentSessionId
authorizeUserAndPersist convertkitApiKeyM firstName lastName picture email = do
  userM <- Users.userByEmail email
  userId <- case userM of
    Nothing -> do
      user <- Users.createUser firstName lastName picture email
      -- Make basic auth users sudo for admin access
      let userWithSudo =
            if T.isSuffixOf "@basic-auth.local" email
              then user{Users.isSudo = True}
              else user
      Users.insertUser userWithSudo
      pure userWithSudo.id
    Just user -> pure user.id
  persistentSessId <- Sessions.newPersistentSessionId
  Sessions.insertSession persistentSessId userId (Sessions.SessionData Map.empty)
  _ <- whenJust convertkitApiKeyM \ckKey -> ConvertKit.addUser ckKey email firstName lastName "" "" ""
  pure persistentSessId
