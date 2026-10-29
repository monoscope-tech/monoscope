module Web.AuthSpec (spec) where

import Data.Effectful.Hasql qualified as Hasql
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Hasql.Interpolate qualified as HI
import Lucid qualified
import Models.Projects.Activation qualified as Activation
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (RequestHeaders, hAuthorization, hCookie, status200)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Pkg.AdConversions qualified as AdConversions
import Pkg.TestUtils
import Relude
import Servant.API (Header, Headers, ResponseHeader (..), getResponse, lookupResponseHeader)
import Servant.Server (ServerError (..))
import Servant.Server qualified as Servant
import Servant.Server.Experimental.Auth (unAuthHandler)
import System.Config (AuthContext (hasqlPool))
import System.Config qualified as Config
import Test.Hspec
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie, parseSetCookie, setCookieName, setCookieValue)
import Web.I18n qualified as I18n


spec :: Spec
spec = aroundAll withTestResources do
  -- A fetch/XHR follows a 302 transparently, so redirecting it to the login page
  -- hands JavaScript a 200 HTML document and every `res.json()` in the app throws
  -- `Unexpected token '<', "<!DOCTYPE "`. That was one alert an hour for 133 days,
  -- with no status anywhere to act on.
  describe "unauthenticated challenge" do
    let challengeOf tr chal =
          runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
            $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing Nothing (Just "/chart_data?pid=p&since=1H") chal

    it "answers a data request with 401 JSON, and a navigation with the usual redirect" \tr -> do
      Left json <- challengeOf tr Auth.ChallengeJson
      errHTTPCode json `shouldBe` 401
      L.lookup "Content-Type" json.errHeaders `shouldBe` Just "application/json"

      Left nav <- challengeOf tr Auth.ChallengeRedirect
      errHTTPCode nav `shouldBe` 302
      L.lookup "Location" nav.errHeaders `shouldSatisfy` isJust

    -- htmx navigates on HX-Redirect; a 302 makes it swap the login page into
    -- whichever fragment happened to ask.
    it "answers an htmx swap with HX-Redirect rather than a redirect it would inline" \tr -> do
      Left hx <- challengeOf tr Auth.ChallengeHtmx
      errHTTPCode hx `shouldBe` 401
      L.lookup "HX-Redirect" hx.errHeaders `shouldSatisfy` isJust
      L.lookup "Location" hx.errHeaders `shouldBe` Nothing

    -- The whole original URL is one query *value*. Unescaped, everything after its
    -- first `&` became a parameter of /login and post-login landed on a truncated URL.
    it "escapes the return URL so a query string survives the round trip" \tr -> do
      Left nav <- challengeOf tr Auth.ChallengeRedirect
      let loc = decodeUtf8 @Text $ fromMaybe "" $ L.lookup "Location" nav.errHeaders
      loc `shouldSatisfy` T.isInfixOf "redirect_to=%2Fchart_data%3Fpid%3Dp%26since%3D1H"

  -- The demo project is world-readable, so a cookie-less request is served under a shared
  -- guest identity. That identity used to be minted per request — 'authorizeUserAndPersist'
  -- inserted a session row every time, and since RespHeaders carries no Set-Cookie the browser
  -- never got one back to reuse, so the next request inserted again. Prod reached 2,569,021
  -- guest rows, 91% of users.persistent_sessions, at 324 MB.
  describe "demo guest session" do
    let demoUrl = Just "/p/00000000-0000-0000-0000-000000000000/log_explorer"
        guestOf tr =
          runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
            $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing Nothing demoUrl Auth.ChallengeRedirect
        guestRows tr = do
          rows :: V.Vector Int <-
            runQueryEffect tr
              $ Hasql.interp [HI.sql| SELECT count(*)::bigint FROM users.persistent_sessions ps JOIN users.users u ON u.id = ps.user_id WHERE u.email = 'hello@monoscope.tech' |]
          pure $ V.head rows

    it "serves every anonymous demo request from one session row" \tr -> do
      Right _ <- guestOf tr
      guestRows tr >>= \n -> n `shouldBe` 1

      -- The regression is entirely in the repeats: under the old code each of these minted a
      -- fresh id, which is exactly one more row in the table every time.
      Right _ <- guestOf tr
      Right _ <- guestOf tr
      guestRows tr >>= \n -> n `shouldBe` 1

  -- Same failure class as the demo-guest rows above, on the basic-auth path: browsers
  -- resend `Authorization: Basic` on every request, and the handler minted a session
  -- row each time rather than reusing the one the cookie already names.
  describe "basic auth session reuse" do
    let creds = "dGVzdGVyOnMzY3JldA==" -- base64 of "tester:s3cret"
        basicCtx tr = tr.trATCtx{Config.config = tr.trATCtx.config{Config.basicAuthEnabled = True, Config.basicAuthUsername = "tester", Config.basicAuthPassword = "s3cret"}}
        reqWith hdrs = Wai.defaultRequest{Wai.requestHeaders = hdrs}
        callAuthRequest tr req =
          Servant.runHandler $ unAuthHandler (Auth.authHandler tr.trLogger (basicCtx tr)) req
        callAuth :: TestResources -> RequestHeaders -> IO (Either ServerError (Headers '[Header "Set-Cookie" SetCookie] Projects.Session))
        callAuth tr hdrs = callAuthRequest tr (reqWith hdrs)
        basicRows tr = do
          rows :: V.Vector Int <-
            runQueryEffect tr
              $ Hasql.interp [HI.sql| SELECT count(*)::bigint FROM users.persistent_sessions ps JOIN users.users u ON u.id = ps.user_id WHERE u.email = 'tester@basic-auth.local' |]
          pure $ V.head rows

    it "basicAuth_repeatRequests_reuseOneSessionRow" \tr -> do
      let authHdr = [(hAuthorization, "Basic " <> creds)]
      Right r1 <- callAuth tr authHdr
      basicRows tr >>= \n -> n `shouldBe` 1
      -- Send back the session cookie the way a browser would. Built from the session id
      -- in the response rather than by rendering the Set-Cookie header, so the test needs
      -- no dependency the built test target lacks.
      let sid = (getResponse r1).sessionId
          cookieHdr = [(hCookie, "monoscope_session=" <> UUID.toASCIIBytes sid.getPersistentSessionId)]
      Right _ <- callAuth tr (authHdr <> cookieHdr)
      Right _ <- callAuth tr (authHdr <> cookieHdr)
      -- Under the old code each repeat minted a fresh id: one more row every time.
      basicRows tr >>= \n -> n `shouldBe` 1

    it "uses namespaced URL parameters as the global telemetry scope" \tr -> do
      let request =
            (reqWith [(hAuthorization, "Basic " <> creds)])
              { Wai.queryString = [("service", Just "checkout"), ("service", Just "catalog"), ("service_scope", Just "frontend-proxy"), ("environment", Just "production")]
              }
      Right response <- callAuthRequest tr request
      let session = getResponse response
      session.service `shouldBe` Just "frontend-proxy"
      session.environment `shouldBe` Just "production"

  describe "ad attribution" do
    let metaCfg tr = tr.trATCtx.config{Config.metaCapiAccessToken = Just "capi-token", Config.metaCapiPixelId = Just "123", Config.redditConversionsToken = Just "rdt-token", Config.redditAdAccountId = Just "t2_acct"}
        bodiesTo url = map (decodeUtf8 @Text . snd) . filter (T.isInfixOf url . fst)
        metaBodies = bodiesTo "graph.facebook.com/v21.0/123/events"

    it "carries a landing's click IDs through signup to the account and Meta, once" \tr -> do
      let land path = WT.runSession (WT.request $ WT.setPath Wai.defaultRequest path) (AdConversions.attributionMiddleware \_ respond -> respond $ Wai.responseLBS status200 [] "")
          setCookies resp = [(setCookieName c, setCookieValue c) | ("Set-Cookie", raw) <- WT.simpleHeaders resp, let c = parseSetCookie raw]
      land "/p/x" >>= (`shouldBe` []) . setCookies
      cookies <- setCookies <$> land "/?gclid=G1&utm_source=google&fbclid=F1&rdt_cid=R1&page=2"
      let attrs = AdConversions.attributionFromCookies cookies (Just "UA")
          -- As the auth callback does: only a sign-in that created the account records a signup.
          signIn = do
            Right (_, newM) <- runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider $ Auth.authorizeUserAndPersist Nothing "Ada" "Lovelace" "" "ads@example.com"
            runAsBaseRecordingHTTP tr $ forM newM \uid -> (uid,) <$> AdConversions.recordSignup (metaCfg tr) uid " Ads@Example.com" attrs
      M.keys attrs `shouldBe` ["clicked_at", "fbc", "fbclid", "gclid", "rdt_cid", "user_agent", "utm_source"]
      (reqs, Just (uid, signupCookie)) <- signIn
      setCookieValue signupCookie `shouldBe` encodeUtf8 uid.toText
      [body] <- pure $ metaBodies reqs
      body `shouldContainAll` ["\"signup-" <> uid.toText <> "\"", "CompleteRegistration", "b8a540005b96c041b6b71b7233a370787511dc61fba4500c81cc3ab1a84f93c9", "fb.1." <> attrs M.! "clicked_at" <> "000.F1"]
      [reddit] <- pure $ bodiesTo "ads-api.reddit.com/api/v2.0/conversions/events/t2_acct" reqs
      reddit `shouldContainAll` ["\"SignUp\"", "\"click_id\":\"R1\"", "\"signup-" <> uid.toText <> "\"", "b8a540005b96c041b6b71b7233a370787511dc61fba4500c81cc3ab1a84f93c9"]
      (body <> reddit) `shouldNotSatisfy` T.isInfixOf "ads@example.com"
      runQueryEffect tr (Hasql.interpOne [HI.sql| SELECT attribution->>'gclid' FROM users.users WHERE id = #{uid} |]) >>= (`shouldBe` Just (Just "G1" :: Maybe Text))
      signIn >>= (`shouldBe` ([], Nothing)) . second (fmap fst)
      runAsBaseRecordingHTTP tr (AdConversions.recordSignup tr.trATCtx.config uid "ads@example.com" attrs) >>= (`shouldBe` []) . fst

    it "reports a new project's first ingest once, credited to its creator" \tr -> do
      let activate createdAfter = runAsBaseRecordingHTTP tr $ Activation.recordActivationMilestones Activation.IngestVerified createdAfter [testPid] >>= \ps -> ps <$ AdConversions.sendActivations (metaCfg tr) ps
      activate (Just $ UTCTime (fromGregorian 2100 1 1) 0) >>= (`shouldBe` ([], []))
      (reqs, activated) <- activate Nothing
      activated `shouldBe` [testPid]
      map (T.isInfixOf $ "\"activated-" <> testPid.toText <> "\"") (metaBodies reqs) `shouldBe` [True]
      activate Nothing >>= (`shouldBe` ([], []))

  describe "loginH" do
    -- Invite emails link to /login?screen_hint=signup&login_hint=<email> so
    -- invitees (who have no Auth0 identity yet) land on the sign-up tab with
    -- their email pre-filled instead of a dead-end "Welcome back!" screen.
    it "forwards signup screen_hint and login_hint to the Auth0 authorize URL" \tr -> do
      headers <- toBaseServantResponse tr $ Auth.loginH (Just "/p/some-project") (Just "signup") (Just "invitee@example.com")
      case lookupResponseHeader @"Location" headers of
        Header location -> do
          location `shouldSatisfy` T.isInfixOf "/authorize?"
          location `shouldSatisfy` T.isInfixOf "&screen_hint=signup"
          location `shouldSatisfy` T.isInfixOf "&login_hint=invitee%40example.com"
        _ -> fail "No Location header in loginH response"

    it "omits hint params when not provided" \tr -> do
      headers <- toBaseServantResponse tr $ Auth.loginH (Just "/") Nothing Nothing
      case lookupResponseHeader @"Location" headers of
        Header location -> do
          location `shouldSatisfy` T.isInfixOf "/authorize?"
          location `shouldNotSatisfy` T.isInfixOf "screen_hint"
          location `shouldNotSatisfy` T.isInfixOf "login_hint"
        _ -> fail "No Location header in loginH response"

  -- Regression: GET /device?code=X&action=approve approved, so any link could bind an
  -- attacker's CLI device code to the clicker's account. GET must only render the prompt.
  describe "CLI device approval" do
    it "binds the device code only on POST, never on GET" \tr -> do
      code <- T.take 8 . UUID.toText <$> UUIDV4.nextRandom
      let bound = runQueryEffect tr (Hasql.interp [HI.sql| SELECT session_id IS NOT NULL FROM users.device_auth_codes WHERE user_code = #{code} |])
      runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| INSERT INTO users.device_auth_codes (id, device_code, user_code, expires_at) VALUES (gen_random_uuid(), gen_random_uuid()::text, #{code}, now() + interval '5 minutes') |]
      (_, prompt) <- testServant tr $ Auth.deviceApproveH False (Just code)
      toStrict (Lucid.renderText prompt) `shouldSatisfy` T.isInfixOf "method=\"POST\""
      bound `shouldReturn` V.singleton False
      _ <- testServant tr $ Auth.deviceApproveH True (Just code)
      bound `shouldReturn` V.singleton True
