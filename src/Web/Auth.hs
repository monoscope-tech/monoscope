module Web.Auth (
  logoutH,
  loginRedirectH,
  loginH,
  authCallbackH,
  setLanguageH,
  sessionByID,
  AuthChallenge (..),
  challengeFor,
  authHandler,
  apiKeyAuthHandler,
  ApiKeyAuthContext,
  resolveApiKeyProject,
  APItoolkitAuthContext,
  authorizeUserAndPersist,
  demoGuestSessionId,
  demoReaderAllows,
  targetsDemoProject,
  errorPageHtml,
  htmlServerError,
  servantRejection,
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
import Data.Aeson.Lens (key, _Bool, _String)
import Data.Aeson.Types (ToJSON)
import Data.Bits (shiftR, (.&.))
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.UUID (UUIDEff, genUUID, runUUID)
import Data.Effectful.Wreq (HTTP, runHTTPWreq)
import Data.Effectful.Wreq qualified as W
import Data.List qualified as L
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.Quasi qualified as UUIDQ
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import Effectful (
  Eff,
  IOE,
  runEff,
  type (:>),
 )
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Effectful.Log (Log)
import Effectful.Reader.Static (ask, asks)
import Effectful.Reader.Static qualified
import Effectful.Time (Time, currentTime, runTime)
import Hasql.Interpolate qualified as HI
import Log (Logger)
import Lucid (Html, renderBS, toHtml)
import Lucid.Html5
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (craftSessionCookie, emptySessionCookie)
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (RequestHeaders, hAuthorization, hCookie, hUserAgent, methodGet, methodHead, methodPost, parseQueryText, urlEncode)
import Network.Wai (Request (queryString, rawPathInfo, rawQueryString, requestHeaders, requestMethod))
import Network.Wreq (FormParam ((:=)), defaults, header, responseBody)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.AdConversions qualified as AdConversions
import Pkg.Mail (addConvertKitUser)
import Relude hiding (ask, asks)
import Servant (Header, Headers, NoContent (..), addHeader, noHeader)
import Servant qualified
import Servant.Server (Handler, ServerError (..), err302, err401)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Logging
import System.Types (ATAuthCtx, ATBaseCtx, ApiPrincipal (..), DB, RespHeaders, addRespHeaders)
import Utils (escapedQueryPartial, hostPath, nonEmptyT, toUriStr)
import Web.ApiTypes (ApiError (..), refusal)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, sameSiteLax)
import Web.I18n qualified as I18n
import Web.Wire (DeviceCodeResponse (..), DeviceTokenResponse (..), ProjectInfo (..))
import "base64" Data.ByteString.Base64 qualified as B64


type APItoolkitAuthContext = AuthHandler Request (Headers '[Header "Set-Cookie" SetCookie] Projects.Session)


-- | The one 401 that asks a client for basic-auth credentials. Written once so the
-- four places that reject an unauthenticated request under @BASIC_AUTH_ENABLED@
-- cannot drift into serving a redirect (or a different realm) from one of them.
basicAuthChallenge :: ServerError
basicAuthChallenge = err401{errHeaders = [("WWW-Authenticate", "Basic realm=\"Monoscope\"")]}


-- | Drop the @Bearer @ scheme from an @Authorization@ value, if present.
--
-- >>> stripBearer "Bearer abc" == "abc" && stripBearer "abc" == "abc"
-- True
stripBearer :: Text -> Text
stripBearer t = fromMaybe t (T.stripPrefix "Bearer " t)


-- | The configured basic-auth user, if the header carries its credentials.
-- Compared with 'BA.constEq' so a wrong password cannot be narrowed down by
-- timing the response.
validateBasicAuth :: EnvConfig -> ByteString -> Maybe Text
validateBasicAuth config authHeader = do
  stripped <- T.stripPrefix "Basic " (decodeUtf8 authHeader)
  case T.splitOn ":" (decodeUtf8 $ B64.decodeBase64Lenient (encodeUtf8 stripped)) of
    [username, password] -> username <$ guard (eq username config.basicAuthUsername && eq password config.basicAuthPassword)
    _ -> Nothing
  where
    eq a b = BA.constEq (encodeUtf8 a :: ByteString) (encodeUtf8 b :: ByteString)


authHandler :: Logger -> AuthContext -> APItoolkitAuthContext
authHandler logger env =
  mkAuthHandler \request ->
    handler request
      & Logging.runLog (show env.config.environment) logger env.config.logLevel
      & EHasql.runHasqlPool env.hasqlPool
      & runHTTPWreq
      & runUUID
      & runTime
      & runErrorNoCallStack @ServerError
      & runEff
      & liftIO
      >>= either T.throwError pure
  where
    handler :: (DB es, Error ServerError :> es, HTTP :> es, Time :> es, UUIDEff :> es) => Request -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Projects.Session)
    handler req = do
      let headers = requestHeaders req
          cookies = maybe [] parseCookies (L.lookup hCookie headers)
          cookieSessId = Projects.sessionIdFromCookies cookies
          cookieSession = join <$> traverse Projects.getPersistentSession cookieSessId
          scope k = nonEmptyT $ decodeUtf8 <$> join (L.lookup k req.queryString)
          session sessIdM url = do
            requestID <- maybe (UUID.toText <$> genUUID) (pure . decodeUtf8) (L.lookup "X-Request-ID" headers)
            sessionByID sessIdM requestID (L.lookup "isSidebarClosed" cookies == Just "true") (if L.lookup "theme" cookies == Just "light" then "light" else "dark") (I18n.languageFromCookies cookies) (scope "environment") (scope "service_scope") url (challengeFor env.config.basicAuthEnabled headers)
          -- A Bearer session token (CLI device auth flow) takes precedence over the cookie.
          bearerSessId = Projects.PersistentSessionId <$> (L.lookup hAuthorization headers >>= T.stripPrefix "Bearer " . decodeUtf8 >>= UUID.fromText)
          cookieAuth = session (bearerSessId <|> cookieSessId) (Just $ rawPathInfo req <> rawQueryString req)
      if not env.config.basicAuthEnabled
        then cookieAuth
        else case L.lookup hAuthorization headers >>= validateBasicAuth env.config of
          Just username -> do
            -- Browsers attach cached Basic credentials to cross-site form posts, and SameSite
            -- does not cover them, so a page elsewhere could act as this sudo user (approve its
            -- own CLI device code, add members). Refuse state changes from other sites.
            when (crossSiteWrite req) $ throwError Servant.err403{errBody = "Cross-site requests may not change state"}
            let basicEmail = username <> "@basic-auth.local"
            -- Browsers resend `Authorization: Basic` on every request, so minting a
            -- session here unconditionally wrote one users.persistent_sessions row per
            -- page load. That is the same failure documented on 'demoGuestSessionId', which
            -- reached 2,569,021 rows. Reuse the cookie's session when it is live AND belongs
            -- to this same basic-auth identity — a cookie for any other user must not be
            -- adopted just because it exists.
            sessId <-
              cookieSession >>= \case
                Just ps | ps.user.getUser.email == CI.mk basicEmail -> pure ps.id
                _ -> fst <$> authorizeUserAndPersist Nothing True "Basic" "Auth" "" basicEmail
            session (Just sessId) Nothing
          -- Without credentials only a live cookie session gets in; anything else is
          -- challenged for basic auth instead of being redirected to Auth0.
          Nothing -> cookieSession >>= maybe (throwError basicAuthChallenge) (const cookieAuth)


-- | An unsafe-method request a browser marks as coming from another site.
crossSiteWrite :: Request -> Bool
crossSiteWrite req = requestMethod req `notElem` [methodGet, methodHead, "OPTIONS"] && L.lookup "Sec-Fetch-Site" (requestHeaders req) == Just "cross-site"


-- | How to tell an unauthenticated request to authenticate.
--
-- A browser navigation can be redirected. A @fetch@/XHR cannot: it follows the
-- 302 transparently and hands JavaScript a __200 HTML login page__, so every
-- @res.json()@ in the app throws @Unexpected token \'<\', "\<!DOCTYPE "@ with no
-- status to act on. That is one hour-long alert per stale tab, and it ran for
-- 133 days before anyone traced it back to the auth wall.
data AuthChallenge
  = -- | Basic auth is on; ask for credentials whatever the client is.
    ChallengeBasic
  | -- | A top-level navigation: 302 to the login page, as before.
    ChallengeRedirect
  | -- | An htmx swap: 401 + @HX-Redirect@, which htmx navigates on. A 302 here
    -- swaps the login page into whatever fragment asked.
    ChallengeHtmx
  | -- | fetch/XHR: 401 JSON, so the caller sees a status instead of HTML.
    ChallengeJson
  deriving stock (Eq, Show)


-- | Classify a request by how it was issued. @Sec-Fetch-Mode@ is the reliable
-- signal (browsers set it on every request and script cannot forge it); a client
-- too old to send it falls through to the redirect that has always been served.
--
-- >>> challengeFor True [("Sec-Fetch-Mode", "cors")]
-- ChallengeBasic
-- >>> challengeFor False [("Sec-Fetch-Mode", "navigate")]
-- ChallengeRedirect
-- >>> challengeFor False [("HX-Request", "true"), ("Sec-Fetch-Mode", "cors")]
-- ChallengeHtmx
-- >>> challengeFor False [("Sec-Fetch-Mode", "cors")]
-- ChallengeJson
-- >>> challengeFor False [("Accept", "application/json")]
-- ChallengeJson
--
-- No @Sec-Fetch-*@ and no JSON @Accept@ (an old browser, or curl) keeps the old
-- behaviour rather than breaking navigation for it:
--
-- >>> challengeFor False []
-- ChallengeRedirect
challengeFor :: Bool -> RequestHeaders -> AuthChallenge
challengeFor basicAuthEnabled headers
  | basicAuthEnabled = ChallengeBasic
  | isJust (L.lookup "HX-Request" headers) = ChallengeHtmx
  | Just mode <- L.lookup "Sec-Fetch-Mode" headers = if mode == "navigate" then ChallengeRedirect else ChallengeJson
  | maybe False ("application/json" `BS.isInfixOf`) (L.lookup "Accept" headers) = ChallengeJson
  | otherwise = ChallengeRedirect


sessionByID :: (DB es, Error ServerError :> es, Time :> es, UUIDEff :> es) => Maybe Projects.PersistentSessionId -> Text -> Bool -> Text -> I18n.Language -> Maybe Text -> Maybe Text -> Maybe ByteString -> AuthChallenge -> Eff es (Headers '[Header "Set-Cookie" SetCookie] Projects.Session)
sessionByID mbPersistentSessionId requestID isSidebarClosed theme lang environment service url challenge = do
  persistentSession <- maybe guest pure . join =<< traverse Projects.getPersistentSession mbPersistentSessionId
  let user = persistentSession.user.getUser
      sessionId = persistentSession.id
  pure $ Projects.addCookie (Projects.craftSessionCookie sessionId False) (Projects.Session{persistentSession, ..})
  where
    guest = case url of
      Just u
        | targetsDemoProject u -> demoGuestSession >>= maybe (throwError $ unauthenticated "/to_login") pure
        | otherwise -> throwError $ unauthenticated "/login"
      Nothing -> throwError $ unauthenticated "/to_login"
    -- The whole URL is one query *value*, so it needs value-escaping, not
    -- URI-escaping: `escapedQueryPartial` leaves `&` alone, which truncated
    -- `redirect_to=/chart_data?pid=…&since=1H` at the `&` and landed the user on
    -- a URL missing everything after the first parameter.
    loginUrl path = path <> "?redirect_to=" <> decodeUtf8 (urlEncode True (fromMaybe "" url))
    unauthenticated path = case challenge of
      ChallengeBasic -> basicAuthChallenge
      ChallengeRedirect -> err302{errHeaders = [("Location", encodeUtf8 (loginUrl path))]}
      ChallengeHtmx -> err401{errHeaders = [("HX-Redirect", encodeUtf8 (loginUrl path))]}
      ChallengeJson ->
        err401
          { errBody = AE.encode $ AE.object ["error" AE..= ("unauthenticated" :: Text), "login" AE..= loginUrl path]
          , errHeaders = [("Content-Type", "application/json")]
          }


-- | Whether an anonymous request is for the demo project: its path names the demo project, or
-- it names no project and its @pid@ parameter is the demo. Matching the id anywhere in the URL
-- handed the guest session to @/p/<any>/widget?pid=<demo>@ and @/p/new?pid=<demo>@.
--
-- >>> map targetsDemoProject ["/p/00000000-0000-0000-0000-000000000000/log_explorer", "/chart_data?pid=00000000-0000-0000-0000-000000000000&since=1H"]
-- [True,True]
-- >>> map targetsDemoProject ["/p/0a1b2c3d-0000-4000-8000-000000000000/widget?pid=00000000-0000-0000-0000-000000000000", "/p/new?pid=00000000-0000-0000-0000-000000000000", "/x?q=/p/00000000-0000-0000-0000-000000000000", "/p/00000000-0000-0000-0000-0000000000001"]
-- [False,False,False,False]
targetsDemoProject :: ByteString -> Bool
targetsDemoProject url = case filter (not . T.null) (T.splitOn "/" path) of
  "p" : project : _ -> project == demo
  _ -> any (\(k, v) -> k == "pid" && v == Just demo) (parseQueryText $ encodeUtf8 query)
  where
    (path, query) = T.breakOn "?" $ decodeUtf8 url
    demo = Projects.demoProjectId.toText


-- | The one session every anonymous demo visitor shares.
--
-- Fixed, not minted per request. It used to call 'authorizeUserAndPersist', which inserts a
-- fresh session row every time — and because 'System.Types.RespHeaders' carries no
-- @Set-Cookie@, the cookie crafted below never reached the browser, so the next request
-- inserted again. That reached 2,569,021 rows, 91% of @users.persistent_sessions@, before it
-- was noticed. At a fixed id the steady state is one SELECT and no write: what a signed-in
-- request already costs.
--
-- The 'SessionData' map is therefore shared between anonymous visitors. That is what a public
-- demo means, and it is not a regression: a per-request session could never carry anything
-- across requests either.
demoGuestSessionId :: Projects.PersistentSessionId
demoGuestSessionId = Projects.PersistentSessionId [UUIDQ.uuid|d3305e55-1000-4000-8000-000000000001|]


demoGuestSession :: (DB es, Time :> es, UUIDEff :> es) => Eff es (Maybe Projects.PersistentSession)
demoGuestSession =
  Projects.getPersistentSession demoGuestSessionId >>= \case
    Just sess -> pure $ Just sess
    Nothing -> do
      uid <- (.userId) <$> userIdForEmail False "Guest" "User" "" guestEmail
      -- Two concurrent first-requests would both insert; the id is the primary key, so the
      -- loser is a no-op rather than a 500 on a page that is meant to need no account.
      Projects.insertSession demoGuestSessionId uid (Projects.SessionData Map.empty)
      Projects.getPersistentSession demoGuestSessionId
  where
    guestEmail = "hello@monoscope.tech"


type ApiKeyAuthContext = AuthHandler Request ApiPrincipal


-- | What the credential-less demo reader may do: read, search, and speak MCP (whose tool
-- calls are re-checked one by one, see 'Web.Routes.apiV1App'). Every handler sees only the
-- project id, so without this an anonymous caller could mint keys or add members on demo.
demoReaderAllows :: Request -> Bool
demoReaderAllows req = requestMethod req `elem` [methodGet, methodHead] || requestMethod req == methodPost && any (`BS.isSuffixOf` rawPathInfo req) ["/events/query", "/mcp"]


resolveApiKeyProject :: (DB es, Effectful.Reader.Static.Reader AuthContext :> es, IOE :> es, Log :> es) => Text -> Maybe Projects.ProjectId -> Eff es (Maybe Projects.ProjectId)
resolveApiKeyProject bearerToken pidM =
  runMaybeT $ MaybeT (ProjectApiKeys.getProjectIdByApiKey token) <|> do
    sessId <- hoistMaybe $ Projects.PersistentSessionId <$> UUID.fromText token
    pid <- hoistMaybe pidM
    sess <- MaybeT $ Projects.getPersistentSession sessId
    pid <$ guard (V.any ((== pid) . (.id)) sess.projects.getProjects)
  where
    token = stripBearer bearerToken


apiKeyAuthHandler :: Logger -> AuthContext -> ApiKeyAuthContext
apiKeyAuthHandler logger env = mkAuthHandler \req -> do
  let mbAuth = L.lookup hAuthorization (requestHeaders req) <&> decodeUtf8
      mbHeaderPid = Projects.projectIdFromText . decodeUtf8 =<< L.lookup "X-Project-Id" (requestHeaders req)
      runEffs :: Eff '[Log, EHasql.Hasql, Effectful.Reader.Static.Reader AuthContext, IOE] a -> Handler a
      runEffs act =
        liftIO
          $ act
          & Logging.runLog (show env.config.environment) logger env.config.logLevel
          & EHasql.runHasqlPool env.hasqlPool
          & Effectful.Reader.Static.runReader env
          & runEff
  authed <- join <$> traverse (runEffs . (`resolveApiKeyProject` mbHeaderPid)) mbAuth
  -- A member's session token carries that member's role, which every handler below ignores:
  -- a viewer could make themselves admin with PATCH /members/{self}. So a viewer reads only,
  -- and only an admin manages members, keys, teams and the project. API keys are unchanged.
  let principal pid =
        maybe (pure Nothing) (runEffs . (`sessionBearerRole` pid)) mbAuth >>= \case
          Just r | r < ProjectMembers.PEdit -> if demoReaderAllows req then pure $ DemoReader pid else T.throwError $ refusal InsufficientRole
          Just r | r < ProjectMembers.PAdmin, adminRequest req -> T.throwError $ refusal InsufficientRole
          _ -> pure $ Authenticated pid
  case (mbHeaderPid, authed) of
    -- Demo project: world-readable, mirrors the web UI bypass in `sessionByID`.
    (Just pid, _)
      | pid == Projects.demoProjectId, authed == Just pid -> principal pid
      | pid == Projects.demoProjectId -> if demoReaderAllows req then pure $ DemoReader pid else T.throwError $ refusal DemoReadOnly
    (_, Just pid) -> principal pid
    _ -> T.throwError $ refusal $ maybe MissingAuthorization (const InvalidCredentials) mbAuth


-- | The role behind a session-token bearer on @pid@; 'Nothing' for an API key.
sessionBearerRole :: DB es => Text -> Projects.ProjectId -> Eff es (Maybe ProjectMembers.Permissions)
sessionBearerRole bearer pid = runMaybeT do
  sid <- hoistMaybe $ UUID.fromText $ stripBearer bearer
  sess <- MaybeT $ Projects.getPersistentSession $ Projects.PersistentSessionId sid
  fromMaybe (bool ProjectMembers.PView ProjectMembers.PAdmin sess.isSudo) <$> lift (ProjectMembers.getUserPermission pid sess.userId)


-- | Writes to what only an admin may change: members, API keys, teams, project settings.
adminRequest :: Request -> Bool
adminRequest req = requestMethod req `notElem` [methodGet, methodHead] && any (`BS.isInfixOf` rawPathInfo req) ["/api/v1/members", "/api/v1/api_keys", "/api/v1/teams", "/api/v1/project"]


-- | Clearing the cookie alone left the session row valid forever: a copied cookie or a CLI
-- token kept working after logout. The shared demo guest session is never deleted.
logoutH :: Maybe ByteString -> ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
logoutH cookieM = do
  whenJust (Projects.sessionIdFromCookies $ maybe [] parseCookies cookieM) \sid ->
    unless (sid == demoGuestSessionId) $ EHasql.interpExecute_ [HI.sql| DELETE FROM users.persistent_sessions WHERE id = #{sid.getPersistentSessionId} |]
  envCfg <- asks env
  let redirectTo = envCfg.auth0Domain <> "/v2/logout?client_id=" <> envCfg.auth0ClientId <> "&returnTo=" <> envCfg.auth0LogoutRedirect
  pure $ addHeader redirectTo $ addHeader Projects.emptySessionCookie NoContent


-- | A post-login or post-switch destination, kept on this site: a path, never a scheme,
-- @//host@ or @/\\host@ (browsers treat both as another origin).
--
-- >>> map (safeRedirect . Just) ["/p/x/log_explorer?since=1H", "https://evil.example", "//evil.example", "/\\evil.example", "javascript:alert(1)", ""]
-- ["/p/x/log_explorer?since=1H","/","/","/","/","/"]
safeRedirect :: Maybe Text -> Text
safeRedirect = \case
  Just p | Just rest <- T.stripPrefix "/" p, not ("/" `T.isPrefixOf` rest || "\\" `T.isPrefixOf` rest) -> p
  _ -> "/"


-- | Persist the user-selected language as a long-lived cookie, then bounce
-- back to wherever they came from. Accessible without auth (the language
-- switcher in the public auth pages must work pre-login).
setLanguageH :: Text -> Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
setLanguageH langText redirectToM = pure $ addHeader (safeRedirect redirectToM) $ addHeader (I18n.languageSetCookieBS $ I18n.parseLanguage langText) NoContent


loginRedirectH :: Maybe Text -> ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
loginRedirectH redirectToM = do
  envCfg <- asks env
  when envCfg.basicAuthEnabled $ throwError basicAuthChallenge
  pure $ addHeader ("/login?redirect_to=" <> decodeUtf8 (urlEncode True $ encodeUtf8 $ safeRedirect redirectToM)) $ addHeader emptySessionCookie NoContent


loginH
  :: Maybe Text
  -> Maybe Text -- screen_hint: "signup" opens Auth0's sign-up tab (invite emails)
  -> Maybe Text -- login_hint: pre-fills the email field
  -> ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginH redirectToM screenHintM loginHintM = do
  envCfg <- asks env
  when envCfg.basicAuthEnabled $ throwError basicAuthChallenge
  stateVar <- UUID.toText <$> genUUID
  let escapedUri = escapedQueryPartial $ envCfg.auth0Callback <> "?redirect_to=" <> decodeUtf8 (urlEncode True $ encodeUtf8 $ safeRedirect redirectToM)
      hintParam name = maybe "" (\v -> "&" <> name <> "=" <> decodeUtf8 (urlEncode True $ encodeUtf8 v))
      redirectTo = envCfg.auth0Domain <> "/authorize?response_type=code&client_id=" <> envCfg.auth0ClientId <> "&redirect_uri=" <> escapedUri <> "&state=" <> stateVar <> "&scope=openid profile email" <> hintParam "screen_hint" screenHintM <> hintParam "login_hint" loginHintM
  pure $ addHeader redirectTo $ addHeader emptySessionCookie $ addHeader (oauthStateCookie stateVar) NoContent


-- | The browser that starts a login carries its @state@ back to 'authCallbackH'. Unchecked, any
-- valid code was accepted from any browser: a link to the callback with the attacker's own
-- code silently signed the victim into the attacker's account (login CSRF).
oauthStateCookie :: Text -> SetCookie
oauthStateCookie st = defaultSetCookie{setCookieName = "monoscope_oauth_state", setCookieValue = encodeUtf8 st, setCookiePath = Just "/", setCookieHttpOnly = True, setCookieSecure = True, setCookieSameSite = Just sameSiteLax, setCookieMaxAge = Just 600}


-- authCallbackH will accept a request with code and state, and use that code to queery auth- for an auth token, and then for user info
-- it then uses this user info to check if we already have that user in our db. And if we have that user in the db,
-- would simply create a new session. If we dont have the user (by that email), then create the user and still create a session with that user.
authCallbackH
  :: Maybe Text
  -> Maybe Text -- state variable from auth0
  -> Maybe Text
  -> Maybe ByteString -- Cookie
  -> Maybe ByteString -- User-Agent
  -> Maybe ByteString -- CF-IPCountry
  -> ATBaseCtx (Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ()))
authCallbackH codeM stateM redirectToM cookieM userAgentM countryM = do
  envCfg <- ask @AuthContext
  pool <- asks pool
  resp <- runExceptT do
    code <- hoistEither $ note "invalid code " codeM
    let started = L.lookup "monoscope_oauth_state" (maybe [] parseCookies cookieM)
    case (encodeUtf8 @Text @ByteString <$> stateM, started) of
      (Just st, Just expected) | BA.constEq st expected -> pass
      _ -> hoistEither $ Left ("oauth state does not match this browser's login" :: Text)
    r <-
      lift
        $ W.post
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
    resp <- lift $ W.getWith opts (toString $ envCfg.config.auth0Domain <> "/userinfo")
    let field k = fromMaybe "" $ resp L.^? responseBody . key k . _String
    -- Accounts are keyed by email, so an IdP login whose email is unverified could claim
    -- whoever already holds that address (or an invite pre-created for it). It may still
    -- create a fresh account: that reaches nobody else's data.
    when (resp L.^? responseBody . key "email_verified" . _Bool == Just False)
      $ whenJustM (lift $ Projects.userByEmail $ field "email") \_ -> hoistEither $ Left ("unverified email for an existing account" :: Text)
    lift do
      (sessId, signin) <- authorizeUserAndPersist (Just envCfg.config.convertkitApiKey) False (field "given_name") (field "family_name") (field "picture") (field "email")
      (sessId,) <$> case signin of
        Existing _ -> pure Nothing
        Created uid -> AdConversions.recordSignup uid $ catMaybes [(hCookie,) <$> cookieM, (hUserAgent,) <$> userAgentM, ("CF-IPCountry",) <$> countryM]
  case resp of
    Left err -> Logging.logAttention "unable to process auth callback page" err >> throwError err302{errHeaders = [("Location", "/login?auth0_callback_failure")]}
    Right (persistentSessId, signupCookieM) -> pure $ addHeader (safeRedirect redirectToM) $ addHeader (craftSessionCookie persistentSessId True) $ maybe noHeader addHeader signupCookieM do
      html_ do
        head_ $ meta_ [httpEquiv_ "refresh", content_ "1;url=/"]
        body_ $ a_ [href_ $ safeRedirect redirectToM] "Continue to Monoscope"


-- | The user behind an email address, created on first sight.
--
-- Split out of 'authorizeUserAndPersist' so the demo guest can resolve its user without also
-- minting a session row — the whole point of 'demoGuestSession' is that it does not.
userIdForEmail :: (DB es, Time :> es, UUIDEff :> es) => Bool -> Text -> Text -> Text -> Text -> Eff es Signin
userIdForEmail sudo firstName lastName picture email =
  Projects.userByEmail email >>= \case
    Just user -> pure $ Existing user.id
    Nothing -> do
      user <- Projects.createUser firstName lastName picture email
      -- Basic-auth users are admins of their own deployment. Only the basic-auth channel says
      -- so: an IdP sign-up as x@basic-auth.local used to be minted sudo from the email text.
      -- The annotation disambiguates 'isSudo', which 'Projects.PersistentSession' also has.
      let userWithSudo :: Projects.User
          userWithSudo = if sudo then user{Projects.isSudo = True} else user
      Projects.insertUser userWithSudo
      pure $ Created userWithSudo.id


data Signin = Existing {userId :: Projects.UserId} | Created {userId :: Projects.UserId}


authorizeUserAndPersist :: (DB es, HTTP :> es, Time :> es, UUIDEff :> es) => Maybe Text -> Bool -> Text -> Text -> Text -> Text -> Eff es (Projects.PersistentSessionId, Signin)
authorizeUserAndPersist convertkitApiKeyM sudo firstName lastName picture email = do
  signin <- userIdForEmail sudo firstName lastName picture email
  persistentSessId <- Projects.newPersistentSessionId
  Projects.insertSession persistentSessId signin.userId (Projects.SessionData Map.empty)
  _ <- whenJust convertkitApiKeyM \ckKey -> addConvertKitUser ckKey email firstName lastName
  pure (persistentSessId, signin)


htmlServerError :: EnvConfig -> Int -> ServerError
htmlServerError envCfg code = ServerError{errHTTPCode = code, errBody = errorPageHtml envCfg code, errReasonPhrase = "", errHeaders = [("Content-Type", "text/html")]}


-- | Servant's own rejections (no route, unparseable capture/query/body): the 'ApiProblem'
-- envelope under @/api/v1@, the HTML error page elsewhere. Gated on @rawPathInfo@: routing has
-- already consumed @pathInfo@.
servantRejection :: EnvConfig -> ApiError -> Request -> ServerError
servantRejection envCfg e req
  | "/api/v1/" `BS.isPrefixOf` (rawPathInfo req <> "/") = err
  | otherwise = htmlServerError envCfg err.errHTTPCode
  where
    err = refusal e


errorPageHtml :: EnvConfig -> Int -> LByteString
errorPageHtml envCfg code = renderBS $ bodyWrapper def{pageTitle = title <> " — Monoscope", config = envCfg} do
  div_ [class_ "flex flex-col items-center justify-center min-h-[60vh] text-center gap-4 px-4"] do
    div_ [class_ "text-6xl opacity-30"] icon
    h1_ [class_ "text-7xl font-bold text-textWeak"] $ toHtml (show @Text code)
    h2_ [class_ "text-2xl font-semibold"] $ toHtml title
    p_ [class_ "text-textWeak max-w-md"] $ toHtml desc
    div_ [class_ "flex gap-3 mt-4"] do
      button_ [class_ "btn", onclick_ "history.back()"] "Go Back"
      a_ [href_ "/", class_ "btn btn-primary"] "Home"
  where
    (icon, title, desc) :: (Html (), Text, Text) = case code of
      400 -> ("🚫", "Bad Request", "The request could not be understood. Please check and try again.")
      401 -> ("🔒", "Unauthorized", "You need to be logged in to access this page.")
      403 -> ("⛔", "Forbidden", "You don't have permission to access this resource.")
      404 -> ("🔍", "Not Found", "The page you're looking for doesn't exist or has been moved.")
      500 -> ("💥", "Internal Server Error", "Something went wrong on our end. Please try again later.")
      502 -> ("🌐", "Bad Gateway", "We're having trouble connecting. Please try again shortly.")
      503 -> ("🔧", "Service Unavailable", "We're temporarily offline for maintenance. Please try again soon.")
      c -> ("⚠️", "Error " <> show c, "An unexpected error occurred.")


data ClientMetadata = ClientMetadata
  { projectId :: Projects.ProjectId
  , topicId :: Text
  , pubsubProjectId :: Text
  , pubsubPushServiceAccount :: AE.Value
  }
  deriving stock (Generic, Show)
  deriving (ToJSON) via DAE.Snake ClientMetadata


clientMetadataH :: Maybe Text -> ATBaseCtx ClientMetadata
clientMetadataH Nothing = throwError err401
clientMetadataH (Just authTextB64) = do
  appCtx <- ask @AuthContext
  -- Malformed keys are anyone's to send (a Go client posted a wrong-length one ~30 times a day,
  -- each an attention log): a 401, not a page.
  let keyIdM = rightToMaybe (B64.decodeBase64Untyped (encodeUtf8 $ stripBearer authTextB64)) >>= UUID.fromASCIIBytes . ProjectApiKeys.decryptAPIKey (encodeUtf8 appCtx.config.apiKeyEncryptionSecretKey)
  case ProjectApiKeys.ProjectApiKeyId <$> keyIdM of
    Nothing -> throwError err401
    Just apiKeyUUID -> do
      -- The project lookup is the SECOND half of authenticating this key, not
      -- a decoration: an API key outlives the project it belongs to, so
      -- without this a deleted or deactivated project's key still collects
      -- pubsub credentials and keeps ingesting. It was previously fetched
      -- and discarded, which validated nothing.
      pApiKey <-
        ProjectApiKeys.getProjectApiKey apiKeyUUID >>= \case
          Nothing -> Logging.logAttention "clientMetadata: no api key with given id" () >> throwError err401
          Just pApiKey ->
            Projects.activeProjectById pApiKey.projectId >>= \case
              Nothing -> Logging.logInfo "clientMetadata: api key for inactive or deleted project" pApiKey.projectId >> throwError err401
              Just _ -> pure pApiKey
      serviceAccountJson <-
        maybe (Logging.logAttention "clientMetadata: failed to decode service account from env" () >> throwError Servant.err500) pure
          $ AE.decodeStrict (B64.decodeBase64Lenient $ encodeUtf8 appCtx.config.monoscopePusherServiceAccountB64)
      pure
        $ ClientMetadata
          { projectId = pApiKey.projectId
          , pubsubProjectId = "past-3"
          , topicId = fromMaybe "" $ listToMaybe appCtx.config.requestPubsubTopics
          , pubsubPushServiceAccount = serviceAccountJson
          }


-- | Device Authorization Flow (CLI login).
deviceCodeH :: ATBaseCtx DeviceCodeResponse
deviceCodeH = do
  envCfg <- asks env
  deviceCode <- UUID.toText <$> genUUID
  (w1, w2, w3, w4) <- UUID.toWords <$> genUUID
  rowId <- genUUID
  now <- currentTime
  let expiresAt = addUTCTime 300 now
      userCode = deviceUserCode (w1, w2, w3, w4)
      verificationUri = hostPath envCfg.hostUrl ("device?code=" <> userCode)
  void
    $ EHasql.interpExecute
      [HI.sql| INSERT INTO users.device_auth_codes (id, device_code, user_code, expires_at) VALUES (#{rowId}, #{deviceCode}, #{userCode}, #{expiresAt}) |]
  pure DeviceCodeResponse{expiresIn = 300, ..}


-- | Eight independent characters (40 bits) from a v4 UUID's random bits. The old code derived
-- its last two characters from the first four, leaving 2^20 codes that any logged-in user
-- could enumerate to bind someone else's waiting CLI to their own account.
--
-- >>> T.length (deviceUserCode (0, 0, 0, 0))
-- 8
-- >>> deviceUserCode (1, 0x00210000, 3, 0x001f0000)
-- "BAABDAA9"
deviceUserCode :: (Word32, Word32, Word32, Word32) -> Text
deviceUserCode (w1, w2, w3, w4) = toText [chars V.! fromIntegral ((w `shiftR` s) .&. 31) | w <- [w1, w2, w3, w4], s <- [0, 16]]
  where
    chars = V.fromList ("ABCDEFGHJKLMNPQRSTUVWXYZ23456789" :: [Char])


deviceTokenH :: Maybe Text -> ATBaseCtx DeviceTokenResponse
deviceTokenH mCode = do
  let errResp = pure . DeviceTokenResponse Nothing Nothing . Just
  case mCode of
    Nothing -> errResp "missing device_code"
    Just dCode -> do
      rows <-
        EHasql.interp
          [HI.sql| SELECT session_id, expires_at FROM users.device_auth_codes WHERE device_code = #{dCode} |]
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


-- | GET shows the confirmation; only the POST approves. A GET that approved let any
-- link bind an attacker's CLI device code to whoever clicked it (the Lax session
-- cookie rides top-level GETs, not cross-site POSTs).
deviceApproveH :: Bool -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
deviceApproveH approve codeM = do
  sess <- Projects.getSession
  result <- runExceptT $ do
    userCode <- hoistEither $ note "Invalid request: no code provided" codeM
    if approve
      then do
        -- Check before minting: each failed guess used to leave a live, orphaned session row.
        pending <- lift $ EHasql.interpOne @(HI.OneColumn Bool) [HI.sql| SELECT true FROM users.device_auth_codes WHERE user_code = #{userCode} AND session_id IS NULL AND expires_at > now() |]
        when (isNothing pending) $ hoistEither $ Left "Invalid, expired, or already used code."
        persistentSessId <- lift Projects.newPersistentSessionId
        lift $ Projects.insertSession persistentSessId sess.user.id (Projects.SessionData Map.empty)
        let sessUuid = persistentSessId.getPersistentSessionId
        updated <-
          lift
            $ EHasql.interpExecute
              [HI.sql| UPDATE users.device_auth_codes SET session_id = #{sessUuid}
                WHERE user_code = #{userCode} AND session_id IS NULL AND expires_at > now() |]
        when (updated == 0) $ hoistEither $ Left "Invalid, expired, or already used code."
        pure True
      else pure False
  addRespHeaders $ html_ do
    head_ do
      title_ "Monoscope - Device Authorization"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    body_ [style_ "font-family:system-ui;max-width:480px;margin:60px auto;padding:0 20px;text-align:center"] do
      h1_ [style_ "font-size:1.5rem"] "Monoscope CLI"
      case result of
        Left err -> p_ [style_ "color:red;font-size:1.2rem"] (toHtml err)
        Right True -> p_ [style_ "color:green;font-size:1.2rem"] "Device authorized! You can close this tab."
        Right False -> whenJust codeM \code -> do
          p_ [style_ "font-size:1.2rem"] $ "Confirm CLI login with code: " <> toHtml code
          form_ [method_ "POST", action_ $ "/device?code=" <> toUriStr code]
            $ button_ [type_ "submit", style_ "padding:12px 32px;font-size:1rem;cursor:pointer;background:#2563eb;color:white;border:none;border-radius:6px"] "Approve"
