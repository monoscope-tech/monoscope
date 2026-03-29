module Web.Auth (
  logoutH,
  loginRedirectH,
  loginH,
  authCallbackH,
  sessionByID,
  authHandler,
  apiKeyAuthHandler,
  ApiKeyAuthContext,
  resolveApiKeyProject,
  APItoolkitAuthContext,
  authorizeUserAndPersist,
  renderError,
  ClientMetadata (..),
  clientMetadataH,
  DeviceCodeResponse (..),
  DeviceTokenResponse (..),
  ProjectInfo (..),
  deviceCodeH,
  deviceTokenH,
  deviceApproveH,
) where

import Control.Error (note)
import Control.Lens qualified as L
import Control.Monad.Except qualified as T
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _String)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Effectful.UUID (UUIDEff, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful (
  Eff,
  Effect,
  IOE,
  runEff,
  type (:>),
 )
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.PostgreSQL (runWithConnectionPool)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static (ask, asks)
import Effectful.Reader.Static qualified
import Effectful.Time (Time, currentTime, runTime)
import Log (Logger)
import Lucid (Html, toHtml)
import Lucid.Html5
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects (craftSessionCookie, emptySessionCookie)
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (Status, hAuthorization, hCookie, statusCode)
import Network.Wai (Request (rawPathInfo, rawQueryString, requestHeaders))
import Network.Wreq (FormParam ((:=)), defaults, getWith, header, post, responseBody)
import Pkg.Mail (addConvertKitUser)
import Relude hiding (ask, asks)
import Servant (Header, Headers, NoContent (..), addHeader)
import Servant qualified
import Servant.Server (Handler, ServerError (..), err302, err401)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import System.Types (ATAuthCtx, ATBaseCtx, DB, RespHeaders, addRespHeaders)
import Utils (escapedQueryPartial)
import Web.Cookie (Cookies, SetCookie, parseCookies)
import "base64" Data.ByteString.Base64 qualified as B64


type APItoolkitAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] Projects.Session)


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
    handler :: (DB es, Error ServerError :> es, HTTP :> es, Time :> es, UUIDEff :> es) => Request -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Projects.Session)
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
              mbPersistentSession <- join <$> mapM Projects.getPersistentSession mbPersistentSessionId
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
      -- Check for Bearer session token (used by CLI device auth flow)
      let mbBearerSessionId = do
            authH <- L.lookup hAuthorization $ requestHeaders req
            token <- T.stripPrefix "Bearer " (decodeUtf8 authH)
            uuid <- UUID.fromText token
            pure $ Projects.PersistentSessionId uuid
      let cookies = getCookies req
      mbCookieSessionId <- handlerToEff $ getSessionId cookies
      let mbPersistentSessionId = mbBearerSessionId <|> mbCookieSessionId
      let isSidebarClosed = sidebarClosedFromCookie cookies
      let theme = themeFromCookie cookies
      requestID <- liftIO $ getRequestID req
      sessionByID mbPersistentSessionId requestID isSidebarClosed theme (Just $ getRequestUrl req) basicAuthEnabledFlag


sessionByID :: (DB es, Error ServerError :> es, HTTP :> es, Time :> es, UUIDEff :> es) => Maybe Projects.PersistentSessionId -> Text -> Bool -> Text -> Maybe ByteString -> Bool -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Projects.Session)
sessionByID mbPersistentSessionId requestID isSidebarClosed theme url basicAuthEnabled = do
  mbPersistentSession <- join <$> mapM Projects.getPersistentSession mbPersistentSessionId
  let mUser = mbPersistentSession <&> (.user.getUser)
  (user, sessionId, persistentSession) <- case (mUser, mbPersistentSession) of
    (Just user, Just userSession) -> pure (user, userSession.id, userSession)
    _ -> do
      case url of
        Just u -> do
          if T.isInfixOf "/p/00000000-0000-0000-0000-000000000000" (decodeUtf8 u) || T.isInfixOf "pid=00000000-0000-0000-0000-000000000000" (decodeUtf8 u)
            then do
              sessId <- authorizeUserAndPersist Nothing "Guest" "User" "" "hello@monoscope.tech"
              mbPess <- join <$> mapM Projects.getPersistentSession (Just sessId)
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
  let sessionCookie = Projects.craftSessionCookie sessionId False
  pure $ Projects.addCookie sessionCookie (Projects.Session{persistentSession, ..})


getCookies :: Request -> Cookies
getCookies req = maybe [] parseCookies (L.lookup hCookie $ requestHeaders req)


getRequestUrl :: Request -> ByteString
getRequestUrl req = rawPathInfo req <> rawQueryString req


getRequestID :: Request -> IO Text
getRequestID req = do
  let headers = requestHeaders req
  case L.lookup "X-Request-ID" headers of
    Nothing -> fmap UUID.toText UUIDV4.nextRandom
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


getSessionId :: Cookies -> Handler (Maybe Projects.PersistentSessionId)
getSessionId cookies = pure $ Projects.PersistentSessionId <$> (UUID.fromASCIIBytes =<< L.lookup "monoscope_session" cookies)


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


type ApiKeyAuthContext = AuthHandler Request Projects.ProjectId


resolveApiKeyProject :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es) => Text -> Eff es (Maybe Projects.ProjectId)
resolveApiKeyProject bearerToken =
  ProjectApiKeys.getProjectIdByApiKey (T.replace "Bearer " "" bearerToken)


apiKeyAuthHandler :: Logger -> AuthContext -> ApiKeyAuthContext
apiKeyAuthHandler logger env = mkAuthHandler \req -> do
  let mbAuth = L.lookup hAuthorization (requestHeaders req) <&> decodeUtf8
  case mbAuth of
    Nothing -> T.throwError err401{errBody = "Missing Authorization header"}
    Just token -> do
      result <-
        resolveApiKeyProject token
          & Logging.runLog (show env.config.environment) logger env.config.logLevel
          & runWithConnectionPool env.pool
          & Effectful.Reader.Static.runReader env
          & runEff
          & liftIO
      case result of
        Nothing -> T.throwError err401{errBody = "Invalid API key"}
        Just pid -> pure pid


logoutH :: ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
logoutH = do
  envCfg <- asks env
  let redirectTo = envCfg.auth0Domain <> "/v2/logout?client_id=" <> envCfg.auth0ClientId <> "&returnTo=" <> envCfg.auth0LogoutRedirect
  pure $ addHeader redirectTo $ addHeader Projects.emptySessionCookie NoContent


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
    Left err -> putStrLn ("unable to process auth callback page " <> err) >> throwError err302{errHeaders = [("Location", "/login?auth0_callback_failure")]}
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


authorizeUserAndPersist :: (DB es, HTTP :> es, Time :> es, UUIDEff :> es) => Maybe Text -> Text -> Text -> Text -> Text -> Eff es Projects.PersistentSessionId
authorizeUserAndPersist convertkitApiKeyM firstName lastName picture email = do
  userM <- Projects.userByEmail email
  userId <- case userM of
    Nothing -> do
      user <- Projects.createUser firstName lastName picture email
      -- Make basic auth users sudo for admin access
      let userWithSudo :: Projects.User
          userWithSudo =
            if T.isSuffixOf "@basic-auth.local" email
              then user{Projects.isSudo = True}
              else user
      Projects.insertUser userWithSudo
      pure userWithSudo.id
    Just user -> pure user.id
  persistentSessId <- Projects.newPersistentSessionId
  Projects.insertSession persistentSessId userId (Projects.SessionData Map.empty)
  _ <- whenJust convertkitApiKeyM \ckKey -> addConvertKitUser ckKey email firstName lastName "" "" ""
  pure persistentSessId


renderError :: forall (es :: [Effect]) (a :: Type). Error ServerError :> es => AuthContext -> Status -> Eff es a
renderError _env status = throwError $ ServerError{errHTTPCode = statusCode status, errBody = "error page: " <> show @LByteString status, errReasonPhrase = "", errHeaders = []}


data ClientMetadata = ClientMetadata
  { projectId :: Projects.ProjectId
  , topicId :: Text
  , pubsubProjectId :: Text
  , pubsubPushServiceAccount :: AE.Value
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ClientMetadata


clientMetadataH :: Maybe Text -> ATBaseCtx ClientMetadata
clientMetadataH Nothing = throwError err401
clientMetadataH (Just authTextB64) = do
  appCtx <- ask @AuthContext
  let authTextE = B64.decodeBase64Untyped (encodeUtf8 $ T.replace "Bearer " "" authTextB64)
  case authTextE of
    Left err -> Logging.logAttention "Auth Error in clientMetadata" (toString err) >> throwError err401
    Right authText -> do
      let decryptedKey = ProjectApiKeys.decryptAPIKey (encodeUtf8 appCtx.config.apiKeyEncryptionSecretKey) authText
      case ProjectApiKeys.ProjectApiKeyId <$> UUID.fromASCIIBytes decryptedKey of
        Nothing -> throwError err401
        Just apiKeyUUID -> do
          (pApiKey, project) <- do
            pApiKeyM <- ProjectApiKeys.getProjectApiKey apiKeyUUID
            case pApiKeyM of
              Nothing -> error "no api key with given id"
              Just pApiKey -> do
                project <- Projects.projectById pApiKey.projectId
                pure (pApiKey, project)
          let serviceAccountJson = case AE.decodeStrict . B64.decodeBase64Lenient . encodeUtf8 $ appCtx.config.monoscopePusherServiceAccountB64 of
                Just val -> val
                Nothing -> error "Failed to decode service account from environment variable"
          pure
            $ ClientMetadata
              { projectId = pApiKey.projectId
              , pubsubProjectId = "past-3"
              , topicId = fromMaybe "" $ listToMaybe appCtx.config.requestPubsubTopics
              , pubsubPushServiceAccount = serviceAccountJson
              }


-- =============================================================================
-- Device Authorization Flow (CLI login)
-- =============================================================================

data DeviceCodeResponse = DeviceCodeResponse
  { deviceCode :: Text
  , userCode :: Text
  , verificationUri :: Text
  , expiresIn :: Int
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] DeviceCodeResponse


data ProjectInfo = ProjectInfo {id :: Text, name :: Text}
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] ProjectInfo


data DeviceTokenResponse = DeviceTokenResponse
  { sessionId :: Maybe Text
  , projects :: Maybe [ProjectInfo]
  , err :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.Rename "err" "error"]] DeviceTokenResponse


genUserCode :: IO Text
genUserCode = do
  let chars = V.fromList ("ABCDEFGHJKLMNPQRSTUVWXYZ23456789" :: [Char])
      len = V.length chars
  (w1, w2, w3, w4) <- UUID.toWords <$> UUIDV4.nextRandom
  let ws = [w1, w2, w3, w4, w1 `xor` w3, w2 `xor` w4]
  pure $ toText [chars V.! (fromIntegral w `mod` len) | w <- ws]


deviceCodeH :: ATBaseCtx DeviceCodeResponse
deviceCodeH = do
  envCfg <- asks env
  deviceCode <- UUID.toText <$> liftIO UUIDV4.nextRandom
  userCode <- liftIO genUserCode
  rowId <- liftIO UUIDV4.nextRandom
  now <- currentTime
  let expiresAt = addUTCTime 300 now
      verificationUri = T.dropWhileEnd (== '/') envCfg.hostUrl <> "/device?code=" <> userCode
  void
    $ PG.execute
      [sql| INSERT INTO users.device_auth_codes (id, device_code, user_code, expires_at) VALUES (?, ?, ?, ?) |]
      (rowId, deviceCode, userCode, expiresAt)
  pure DeviceCodeResponse{expiresIn = 300, ..}


deviceTokenH :: Maybe Text -> ATBaseCtx DeviceTokenResponse
deviceTokenH mCode = do
  let errResp = pure . DeviceTokenResponse Nothing Nothing . Just
  case mCode of
    Nothing -> errResp "missing device_code"
    Just dCode -> do
      rows <-
        PG.query
          [sql| SELECT session_id, expires_at FROM users.device_auth_codes WHERE device_code = ? |]
          (Only dCode)
      now <- currentTime
      case rows of
        [] -> errResp "invalid_device_code"
        ((mbSessId, expiresAt) : _)
          | now > expiresAt -> errResp "expired_token"
          | Nothing <- (mbSessId :: Maybe UUID.UUID) -> errResp "authorization_pending"
          | Just sessUuid <- mbSessId -> do
              mbSess <- Projects.getPersistentSession (Projects.PersistentSessionId sessUuid)
              let projectInfos =
                    mbSess <&> \sess ->
                      [ProjectInfo p.id.toText p.title | p <- V.toList sess.projects.getProjects]
              pure $ DeviceTokenResponse (Just $ UUID.toText sessUuid) projectInfos Nothing


deviceApproveH :: Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
deviceApproveH codeM actionM = do
  sess <- Projects.getSession
  result <- runExceptT $ do
    userCode <- hoistEither $ note "Invalid request: no code provided" codeM
    case actionM of
      Just "approve" -> do
        persistentSessId <- lift Projects.newPersistentSessionId
        lift $ Projects.insertSession persistentSessId sess.user.id (Projects.SessionData Map.empty)
        updated <-
          lift
            $ PG.execute
              [sql| UPDATE users.device_auth_codes SET session_id = ?
                WHERE user_code = ? AND session_id IS NULL AND expires_at > now() |]
              (persistentSessId.getPersistentSessionId, userCode)
        when (updated == 0) $ hoistEither $ Left "Invalid, expired, or already used code."
        pure True
      _ -> pure False
  addRespHeaders $ case result of
    Left err -> approvePageHtml False (Just err)
    Right approved -> approvePageHtml approved Nothing
  where
    approvePageHtml approved errMsg = html_ $ do
      head_ $ do
        title_ "Monoscope - Device Authorization"
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      body_ [style_ "font-family:system-ui;max-width:480px;margin:60px auto;padding:0 20px;text-align:center"] $ do
        h1_ [style_ "font-size:1.5rem"] "Monoscope CLI"
        case errMsg of
          Just err -> p_ [style_ "color:red;font-size:1.2rem"] (toHtml err)
          Nothing | approved -> p_ [style_ "color:green;font-size:1.2rem"] "Device authorized! You can close this tab."
          Nothing -> do
            whenJust codeM \code -> do
              p_ [style_ "font-size:1.2rem"] $ "Confirm CLI login with code: " <> toHtml code
              form_ [method_ "GET", action_ "/device"] $ do
                input_ [type_ "hidden", name_ "code", value_ code]
                input_ [type_ "hidden", name_ "action", value_ "approve"]
                button_ [type_ "submit", style_ "padding:12px 32px;font-size:1rem;cursor:pointer;background:#2563eb;color:white;border:none;border-radius:6px"] "Approve"
