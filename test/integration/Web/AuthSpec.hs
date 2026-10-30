module Web.AuthSpec (spec) where

import BackgroundJobs qualified
import BackgroundJobs.Types (BgJobs (AdConversion), Conversion (..))
import Data.Effectful.Hasql qualified as Hasql
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime, nominalDay)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Hasql.Interpolate qualified as HI
import Lucid qualified
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (RequestHeaders, hAuthorization, hCookie, status200)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Pages.BodyWrapper qualified as BodyWrapper
import Pkg.AdConversions qualified as AdConversions
import Pkg.TestUtils
import Relude
import Servant.API (Header, Headers, ResponseHeader (..), getHeaders, getResponse, lookupResponseHeader)
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
    let adCtx tr = tr.trATCtx{Config.config = tr.trATCtx.config{Config.auth0Domain = "https://auth0.test", Config.metaCapiAccessToken = Just "capi-token", Config.metaCapiPixelId = Just "123", Config.redditConversionsToken = Just "rdt-token", Config.redditPixelId = Just "t2_px"}}
        -- Dequeue the ad conversion jobs and run them as the worker would, recording what they send.
        sendQueued tr = do
          jobs <- runQueryEffect tr $ Hasql.interp [HI.sql| DELETE FROM background_jobs WHERE payload->>'tag' = 'AdConversion' RETURNING payload |]
          let convs = [c | HI.OneColumn (HI.AsJsonb (AdConversion c)) <- jobs]
          (,convs) . fst <$> runTestBgRecordingHTTP (UTCTime (fromGregorian 2026 10 30) 0) tr (traverse_ (AdConversions.sendConversion $ adCtx tr) convs)
        bodiesTo url = map (decodeUtf8 @Text . snd) . filter (T.isInfixOf url . fst)

    it "carries a consenting landing's click IDs through the auth callback to one signup conversion" \tr -> do
      let land hs path = WT.runSession (WT.request (WT.setPath Wai.defaultRequest path){Wai.requestHeaders = hs}) (AdConversions.attributionMiddleware \_ respond -> respond $ Wai.responseLBS status200 [] "")
          setCookies hs = [parseSetCookie v | ("Set-Cookie", v) <- hs]
          granted = [(hCookie, "ms_consent=granted")]
          -- Fresh ids: the static test supply restarts at ...0001, the template user's id.
          callback cookie = replicateM 4 UUIDV4.nextRandom >>= writeIORef tr.trUUIDRef >> setCookies . getHeaders <$> toBaseServantResponse tr{trATCtx = adCtx tr} (Auth.authCallbackH (Just "code") Nothing Nothing (Just cookie) (Just "UA") Nothing)
      land granted "/p/x" >>= (`shouldBe` []) . setCookies . WT.simpleHeaders
      land [("CF-IPCountry", "DE")] "/?gclid=G1" >>= (`shouldBe` []) . setCookies . WT.simpleHeaders
      [stash] <- setCookies . WT.simpleHeaders <$> land granted "/?gclid=G1&utm_source=google&fbclid=F1&rdt_cid=R1&page=2"
      let cookie = "ms_consent=granted; _fbp=fb.1.1.2; monoscope_attribution=" <> setCookieValue stash
      [_, signup] <- callback cookie
      Just (uid :: Projects.UserId) <- runQueryEffect tr $ Hasql.interpOne [HI.sql| SELECT id FROM users.users WHERE email = 'callback@example.com' |]
      (setCookieName signup, setCookieValue signup) `shouldBe` ("monoscope_signup", encodeUtf8 uid.toText)
      runQueryEffect tr (Hasql.interpOne [HI.sql| SELECT attribution->>'gclid' FROM users.users WHERE id = #{uid} |]) >>= (`shouldBe` Just (Just "G1" :: Maybe Text))
      (reqs, convs) <- sendQueued tr
      let emailSha = "a7872ac5ff5e2cb54645209916a065fd4c5fe5f7196bad0042dc037cc9bd1633"
      convs `shouldBe` [Signup uid]
      [meta] <- pure $ bodiesTo "graph.facebook.com/v21.0/123/events" reqs
      meta `shouldContainAll` ["\"signup-" <> uid.toText <> "\"", "CompleteRegistration", "\"event_source_url\"", "\"client_user_agent\":\"UA\"", "\"fbp\":\"fb.1.1.2\"", "000.F1\"", emailSha]
      [reddit] <- pure $ bodiesTo "ads-api.reddit.com/api/v3/pixels/t2_px/conversion_events" reqs
      reddit `shouldContainAll` ["\"SIGN_UP\"", "\"click_id\":\"R1\"", "\"signup-" <> uid.toText <> "\"", emailSha]
      (meta <> reddit) `shouldNotSatisfy` T.isInfixOf "callback@example.com"
      -- A returning sign-in, or a new one without consent, records and queues nothing.
      callback cookie >>= (`shouldBe` 1) . length
      runQueryEffect tr (AdConversions.recordSignup uid [("CF-IPCountry", "DE")]) >>= (`shouldSatisfy` isNothing)
      sendQueued tr >>= (`shouldBe` ([], []))

    it "loads every ad and analytics tag behind the browser consent check" \tr -> do
      let cfg = tr.trATCtx.config{Config.googleAdsConversionId = Just "AW-1", Config.facebookPixelId1 = Just "fb1", Config.googleTagManagerId = Just "GTM-1", Config.linkedInPartnerId = Just "li1", Config.postHogApiKey = Just "ph1"}
          html = toStrict $ Lucid.renderText $ BodyWrapper.externalHeadScripts_ cfg
      html `shouldNotSatisfy` T.isInfixOf "<noscript"
      html `shouldNotSatisfy` T.isInfixOf "src=\""
      length (filter (T.isPrefixOf "if (window.msConsent)") $ T.splitOn "<script>" html) `shouldBe` 6

    it "reports a recent project's first ingest once from the hourly job, credited to its consenting creator" \tr -> do
      now <- getCurrentTime
      Just (owner :: Projects.UserId) <- runQueryEffect tr $ Hasql.interpOne [HI.sql| SELECT user_id FROM projects.project_members WHERE project_id = #{testPid} ORDER BY created_at, id LIMIT 1 |]
      runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE users.users SET attribution = jsonb_build_object('gclid', 'G2', 'clicked_at', extract(epoch FROM now())::bigint::text) WHERE id = #{owner} |]
      apiKey <- createTestAPIKey tr testPid "ad-activation"
      ingestTrace tr apiKey "ad-activation" (addUTCTime (-600) now)
      let hourly createdAt = do
            runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE projects.projects SET created_at = #{createdAt} WHERE id = #{testPid} |]
            runTestBg now tr $ BackgroundJobs.runHourlyJob now 0
            second (map (T.isInfixOf ("\"activated-" <> testPid.toText <> "\"") . decodeUtf8 . snd)) . swap <$> sendQueued tr
      hourly (addUTCTime (-(31 * nominalDay)) now) >>= (`shouldBe` ([], []))
      hourly now >>= (`shouldBe` ([Activated testPid], [True]))
      hourly now >>= (`shouldBe` ([], []))
      offline <- runQueryEffect tr $ Hasql.interp [HI.sql| SELECT order_id, gclid, right(conversion_time, 5) FROM users.google_ads_offline_conversions WHERE gclid = 'G2' ORDER BY order_id |]
      offline `shouldBe` ([("activated-" <> testPid.toText, "G2", "+0000"), ("signup-" <> owner.toText, "G2", "+0000")] :: [(Text, Text, Text)])

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
      renderT prompt `shouldSatisfy` T.isInfixOf "method=\"POST\""
      bound `shouldReturn` V.singleton False
      _ <- testServant tr $ Auth.deviceApproveH True (Just code)
      bound `shouldReturn` V.singleton True
