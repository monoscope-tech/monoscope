module Web.AuthSpec (spec) where

import BackgroundJobs qualified
import BackgroundJobs.Types (BgJobs (AdConversion), Conversion (..))
import Data.Effectful.Hasql qualified as Hasql
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian, getCurrentTime, nominalDay)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Hasql.Interpolate qualified as HI
import Lucid qualified
import Models.Projects.ProjectMembers qualified as PM
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types (Query, RequestHeaders, hAuthorization, hCacheControl, hCookie, statusCode, status200)
import Network.HTTP.Types.Header (hVary)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WT
import Models.Apis.Issues qualified as Issues
import Pages.AIThreads qualified as AIThreads
import Pages.BodyWrapper qualified as BodyWrapper
import Pages.Home qualified as Home
import Pages.Projects qualified as ProjectsP
import Pages.Settings qualified as Settings
import Pkg.AdConversions qualified as AdConversions
import Pkg.TestUtils
import Servant.API (Header, Headers, ResponseHeader (..), getHeaders, getResponse, lookupResponseHeader)
import Servant.Server (ServerError (..))
import Servant.Server qualified as Servant
import Servant.Server.Experimental.Auth (unAuthHandler)
import System.Config (AuthContext (hasqlPool))
import System.Config qualified as Config
import System.Server qualified as Server
import System.Types (ApiPrincipal (..))
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie, parseSetCookie, setCookieName, setCookieValue)
import Web.FormUrlEncoded (urlDecodeAsForm)
import Web.I18n qualified as I18n


spec :: Spec
spec = around withTestResources do
  it "sign-in: unauthenticated challenges, one demo-guest session, basic-auth reuse and scope, Auth0 hints, device approval, API-key metadata" \tr -> do
    let sessionFor url challenge = runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing Nothing (Just url) challenge
        sessionRows email = dbOne @_ @Int tr [sql| SELECT count(*)::int FROM users.persistent_sessions ps JOIN users.users u ON u.id = ps.user_id WHERE u.email = ? |] (Only (email :: Text))
        base = Servant.runHandler . effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider
    -- A fetch/XHR follows a 302 transparently, so redirecting it to the login page hands
    -- JavaScript a 200 HTML document and every `res.json()` throws: one alert an hour for 133 days.
    Left json <- sessionFor "/chart_data?pid=p&since=1H" Auth.ChallengeJson
    (errHTTPCode json, L.lookup "Content-Type" json.errHeaders) `shouldBe` (401, Just "application/json")
    -- htmx navigates on HX-Redirect; a 302 makes it swap the login page into the asking fragment.
    Left hx <- sessionFor "/chart_data?pid=p&since=1H" Auth.ChallengeHtmx
    (errHTTPCode hx, isJust $ L.lookup "HX-Redirect" hx.errHeaders, L.lookup "Location" hx.errHeaders) `shouldBe` (401, True, Nothing)
    -- The original URL is one query *value*: unescaped, everything after its first `&` was lost.
    Left nav <- sessionFor "/chart_data?pid=p&since=1H" Auth.ChallengeRedirect
    errHTTPCode nav `shouldBe` 302
    decodeUtf8 @Text (fromMaybe "" $ L.lookup "Location" nav.errHeaders) `shouldSatisfy` T.isInfixOf "redirect_to=%2Fchart_data%3Fpid%3Dp%26since%3D1H"

    -- Anonymous demo requests share one guest session; minting one per request reached
    -- 2,569,021 rows (91% of users.persistent_sessions) in prod.
    replicateM_ 3 $ isRight <$> sessionFor "/p/00000000-0000-0000-0000-000000000000/log_explorer" Auth.ChallengeRedirect >>= (`shouldBe` True)
    sessionRows "hello@monoscope.tech" `shouldReturn` 1

    -- Browsers resend `Authorization: Basic` every request; the session cookie must be reused, not re-minted.
    let basicCtx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.basicAuthEnabled = True, Config.basicAuthUsername = "tester", Config.basicAuthPassword = "s3cret"}}
        basic :: RequestHeaders -> Query -> IO (Either ServerError (Headers '[Header "Set-Cookie" SetCookie] Projects.Session))
        basic hdrs qs = Servant.runHandler $ unAuthHandler (Auth.authHandler tr.trLogger basicCtx) Wai.defaultRequest{Wai.requestHeaders = (hAuthorization, "Basic dGVzdGVyOnMzY3JldA==") : hdrs, Wai.queryString = qs}
    Right r1 <- basic [] []
    let cookie = [(hCookie, "monoscope_session=" <> UUID.toASCIIBytes (getResponse r1).sessionId.getPersistentSessionId)]
    replicateM_ 2 $ isRight <$> basic cookie [] >>= (`shouldBe` True)
    sessionRows "tester@basic-auth.local" `shouldReturn` 1
    -- Namespaced URL parameters are the global telemetry scope.
    Right scoped <- basic [] [("service", Just "checkout"), ("service", Just "catalog"), ("service_scope", Just "frontend-proxy"), ("environment", Just "production")]
    ((getResponse scoped).service, (getResponse scoped).environment) `shouldBe` (Just "frontend-proxy", Just "production")

    -- Invite emails link to /login?screen_hint=signup&login_hint=<email> so invitees land on
    -- sign-up with their email filled in; without hints neither parameter is sent.
    let authorizeUrl hint email =
          toBaseServantResponse tr (Auth.loginH (Just "/p/some-project") hint email) >>= \h -> case lookupResponseHeader @"Location" h of
            Header location -> pure location
            _ -> fail "loginH sent no Location"
    authorizeUrl (Just "signup") (Just "invitee@example.com") >>= (`shouldContainAll` ["/authorize?", "&screen_hint=signup", "&login_hint=invitee%40example.com"])
    plain <- authorizeUrl Nothing Nothing
    (T.isInfixOf "/authorize?" plain, T.isInfixOf "screen_hint" plain, T.isInfixOf "login_hint" plain) `shouldBe` (True, False, False)

    -- GET /device?action=approve approved, so any link could bind an attacker's CLI code to the clicker.
    code <- T.take 8 . UUID.toText <$> UUIDV4.nextRandom
    let bound = dbOne @_ @Bool tr [sql| SELECT session_id IS NOT NULL FROM users.device_auth_codes WHERE user_code = ? |] (Only code)
    dbExec tr [sql| INSERT INTO users.device_auth_codes (id, device_code, user_code, expires_at) VALUES (gen_random_uuid(), gen_random_uuid()::text, ?, now() + interval '5 minutes') |] (Only code)
    prompt <- call tr $ Auth.deviceApproveH False (Just code)
    renderT prompt `shouldSatisfy` T.isInfixOf "method=\"POST\""
    bound `shouldReturn` False
    _ <- testServant tr $ Auth.deviceApproveH True (Just code)
    bound `shouldReturn` True

    -- SDK client metadata: a live project's key gets its pubsub config; a bad or wrong-length
    -- (unpadded base64) key, or a key whose project was deleted, is a 401.
    key <- createTestAPIKey tr testPid "metadata"
    Right meta <- base $ Auth.clientMetadataH (Just key)
    (meta.projectId, meta.topicId, meta.pubsubProjectId) `shouldBe` (testPid, "monoscope-prod-default", "past-3")
    victim <- createTestProject tr "deleted"
    deletedKey <- createTestAPIKey tr victim "metadata"
    dbExec tr [sql| UPDATE projects.projects SET deleted_at = now(), active = false WHERE id = ? |] (Only victim)
    for_ [Just "invalid-api-key", Just (T.drop 1 key), Just deletedKey, Nothing] \k -> base (Auth.clientMetadataH k) >>= (`shouldBe` Just 401) . leftToMaybe . first errHTTPCode

  it "roles: demo and viewer bearers only read, editors cannot administer, cross-site basic-auth writes and logged-out sessions are refused" \tr -> do
    let sid = UUID.toASCIIBytes (getResponse tr.trSessAndHeader).sessionId.getPersistentSessionId
        -- Servant runs the auth check at the routed leaf, where pathInfo is already consumed.
        api hs m p = Servant.runHandler $ unAuthHandler (Auth.apiKeyAuthHandler tr.trLogger tr.trATCtx) (WT.setPath Wai.defaultRequest p){Wai.requestMethod = m, Wai.requestHeaders = hs, Wai.pathInfo = []}
        outcome = fmap (bimap errHTTPCode \case Authenticated _ -> rw; Editor _ -> ed; Viewer _ -> vw; DemoReader _ -> ro)
        rw, ed, vw, ro :: Text
        (rw, ed, vw, ro) = ("rw", "ed", "vw", "ro")
        asMember = api [(hAuthorization, "Bearer " <> sid), ("X-Project-Id", encodeUtf8 testPid.toText)]
        demo = api [("X-Project-Id", encodeUtf8 Projects.demoProjectId.toText)]
    -- The demo project's world-readable bypass also took writes, e.g. minting its API keys.
    -- Key ids are listed too: a token is a fixed-IV encryption of its id, so an id is the key.
    traverse outcome [demo "GET" "/api/v1/events", demo "POST" "/api/v1/events/query", demo "POST" "/api/v1/api_keys", demo "GET" "/api/v1/api_keys"] `shouldReturn` ([Right ro, Right ro, Left 401, Left 401] :: [Either Int Text])
    -- A session token ignored the member's role: a viewer could PATCH themselves to admin.
    let probes = [asMember "GET" "/api/v1/members", asMember "PATCH" "/api/v1/members/self", asMember "POST" "/api/v1/api_keys", asMember "POST" "/api/v1/monitors", asMember "GET" "/api/v1/api_keys"]
    for_ [(PM.PView, [Right vw, Left 403, Left 403, Left 403, Left 403] :: [Either Int Text]), (PM.PEdit, [Right ed, Left 403, Left 403, Right ed, Right ed]), (PM.PAdmin, replicate 5 (Right rw))] \(role, want) ->
      setRole tr testPid role >> (traverse outcome probes `shouldReturn` want)

    -- Basic auth rides every request, so a cross-site form could write as the operator.
    let basicCtx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.basicAuthEnabled = True, Config.basicAuthUsername = "tester", Config.basicAuthPassword = "s3cret"}}
        basic m site = fmap (leftToMaybe . first errHTTPCode) . Servant.runHandler $ unAuthHandler (Auth.authHandler tr.trLogger basicCtx) Wai.defaultRequest{Wai.requestMethod = m, Wai.requestHeaders = [(hAuthorization, "Basic dGVzdGVyOnMzY3JldA=="), ("Sec-Fetch-Site", site)]}
    traverse (uncurry basic) ([("POST", "cross-site"), ("GET", "cross-site"), ("POST", "same-origin")] :: [(ByteString, ByteString)]) `shouldReturn` [Just 403, Nothing, Nothing]

    -- Logout only cleared the cookie: the session row, and so a copied cookie or CLI token, lived on.
    let alive = dbOne @_ @Bool tr [sql| SELECT EXISTS (SELECT 1 FROM users.persistent_sessions WHERE id = ?::uuid) |] (Only $ decodeUtf8 @Text sid)
    alive `shouldReturn` True
    void $ toBaseServantResponse tr $ Auth.logoutH (Just $ "monoscope_session=" <> sid)
    alive `shouldReturn` False

  -- demoGuestSeededAdmin: migration 0001 seeds the guest identity as the demo project's admin, so
  -- every anonymous visitor (and its public session id as a bearer) passed PEdit/PAdmin gates.
  it "demoGuestSeededAdmin_cappedAtEdit: the anonymous demo guest edits the demo sandbox but cannot mint keys, invite or change plan" \tr -> do
    Right guest <- runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing Nothing (Just "/p/00000000-0000-0000-0000-000000000000/") Auth.ChallengeRedirect
    let demoPid = Projects.demoProjectId
        asGuest = tr{trSessAndHeader = guest}
    runQueryEffect tr (PM.getSessionPermission demoPid (getResponse guest).user.id) `shouldReturn` Just PM.PEdit
    traverse (httpStatus asGuest) [void $ Settings.apiPostH demoPid (Settings.GenerateAPIKeyForm "guest" Nothing), void $ ProjectsP.manageMembersPostH demoPid Nothing (ProjectsP.ManageMembersForm ["x@example.test"] [PM.PAdmin]), void $ ProjectsP.pricingUpdateH demoPid (ProjectsP.PricingUpdateForm Nothing Nothing Nothing)] `shouldReturn` [403, 403, 403]
    httpStatus asGuest (void $ Home.homeGetH demoPid) `shouldReturn` 200
    -- Chat stays open to the guest: its turn is admitted and runs, read-only.
    call asGuest (AIThreads.startThreadPostHWithTools Nothing demoPid $ fromRightShow $ urlDecodeAsForm "query=What+failed%3F&mode=chat") >>= (`shouldSatisfy` isRight)
    runId <- dbOne tr [sql| SELECT id FROM apis.ai_routine_runs WHERE project_id = ? AND requested_by = ? |] (demoPid, (getResponse guest).user.id)
    run <- runQueryEffect tr (Issues.runContextFor demoPid (Issues.ConversationClaim runId)) >>= maybe (fail "guest run missing") pure
    fmap (.policy) <$> runQueryEffect tr (Issues.runContextAuthority run) `shouldReturn` Just Issues.Observe
    let guestBearer (m, p) = Servant.runHandler $ unAuthHandler (Auth.apiKeyAuthHandler tr.trLogger tr.trATCtx) (WT.setPath Wai.defaultRequest p){Wai.requestMethod = m, Wai.requestHeaders = [(hAuthorization, "Bearer " <> UUID.toASCIIBytes Auth.demoGuestSessionId.getPersistentSessionId), ("X-Project-Id", encodeUtf8 demoPid.toText)]}
    traverse (fmap (bimap errHTTPCode \case Editor _ -> True; _ -> False) . guestBearer) [("GET", "/api/v1/events"), ("POST", "/api/v1/api_keys")] `shouldReturn` ([Right True, Left 403] :: [Either Int Bool])

  -- 2026-11 audit: session pages carried no cache policy, so an edge caching by extension could
  -- store a member's page fetched as /p/<pid>/repositories/services/x.css and serve it to anyone.
  it "privatePages_assetLikePath_carryNoStore: pages default to private, no-store; static assets keep their own policy" \tr -> do
    let cookie = (hCookie, "monoscope_session=" <> UUID.toASCIIBytes (getResponse tr.trSessAndHeader).sessionId.getPersistentSessionId)
        fetch path = WT.runSession (WT.request (WT.setPath Wai.defaultRequest path){Wai.requestHeaders = [cookie]}) (Server.mkServer tr.trLogger tr.trATCtx tr.trTracerProvider)
        policy r = (statusCode r.simpleStatus, L.lookup hCacheControl r.simpleHeaders, L.lookup hVary r.simpleHeaders)
    policy <$> fetch ("/p/" <> encodeUtf8 testPid.toText <> "/repositories/services/x.css") `shouldReturn` (200, Just "private, no-store", Just "Cookie, Authorization")
    (\(_, cache, _) -> cache /= Just "private, no-store") . policy <$> fetch "/public/favicon.ico" `shouldReturn` True

  it "auth0 callback: a refused code exchange sends the user back to login, not a 500 page" \tr -> do
    -- Prod 2026-11-20 (24c4fdfc): Auth0 answered 403 invalid_grant and wreq's exception escaped the handler.
    let ctx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.auth0Domain = "https://auth0-denied.test"}}
    toBaseServantResponse tr{trATCtx = ctx} (Auth.authCallbackH (Just "code") (Just "st-1") Nothing (Just "monoscope_oauth_state=st-1") Nothing Nothing)
      `shouldThrow` \(e :: SomeException) -> "auth0_callback_failure" `T.isInfixOf` show e

  it "ad attribution: a consenting landing becomes one signup conversion, tags wait for consent, a first ingest is reported once" \tr -> do
    let adCtx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.auth0Domain = "https://auth0.test", Config.metaCapiAccessToken = Just "capi-token", Config.metaCapiPixelId = Just "123", Config.redditConversionsToken = Just "rdt-token", Config.redditPixelId = Just "t2_px"}}
        -- Dequeue the ad conversion jobs and run them as the worker would, recording what they send.
        sendQueued = do
          jobs <- runQueryEffect tr $ Hasql.interp [HI.sql| DELETE FROM background_jobs WHERE payload->>'tag' = 'AdConversion' RETURNING payload |]
          let convs = [c | HI.OneColumn (HI.AsJsonb (AdConversion c)) <- jobs]
          (,convs) . fst <$> runTestBgRecordingHTTP (UTCTime (fromGregorian 2026 10 30) 0) tr (traverse_ (AdConversions.sendConversion adCtx) convs)
        bodiesTo url = map (decodeUtf8 @Text . snd) . filter (T.isInfixOf url . fst)
        land hs path = WT.runSession (WT.request (WT.setPath Wai.defaultRequest path){Wai.requestHeaders = hs}) (AdConversions.attributionMiddleware \_ respond -> respond $ Wai.responseLBS status200 [] "")
        setCookies hs = [parseSetCookie v | ("Set-Cookie", v) <- hs]
        granted = [(hCookie, "ms_consent=granted")]
        -- Fresh ids: the static test supply restarts at ...0001, the template user's id.
        callbackWith st cookie = replicateM 4 UUIDV4.nextRandom >>= writeIORef tr.trUUIDRef >> setCookies . getHeaders <$> toBaseServantResponse tr{trATCtx = adCtx} (Auth.authCallbackH (Just "code") st Nothing (Just $ cookie <> "; monoscope_oauth_state=st-1") (Just "UA") Nothing)
        callback = callbackWith (Just "st-1")
    land granted "/p/x" >>= (`shouldBe` []) . setCookies . WT.simpleHeaders
    land [("CF-IPCountry", "DE")] "/?gclid=G1" >>= (`shouldBe` []) . setCookies . WT.simpleHeaders
    [stash] <- setCookies . WT.simpleHeaders <$> land granted "/?gclid=G1&utm_source=google&fbclid=F1&rdt_cid=R1&page=2"
    let cookie = "ms_consent=granted; _fbp=fb.1.1.2; monoscope_attribution=" <> setCookieValue stash
    -- Regression (login CSRF): a callback whose state was not issued to this browser signs no one in.
    for_ [Nothing, Just ("st-other" :: Text)] \st -> (callbackWith st cookie `shouldThrow` anyException)
    -- unverifiedEmail_createsNoAccount: an explicitly unverified sign-up minted the account its real owner later logged into.
    let unverified _ url = pure $ "{\"email\":\"unverified@example.com\",\"email_verified\":false}" <$ guard ("/userinfo" `T.isSuffixOf` toText url)
    toBaseServantResponse tr{trATCtx = adCtx} (withHTTPResponses unverified $ Auth.authCallbackH (Just "code") (Just "st-1") Nothing (Just "monoscope_oauth_state=st-1") Nothing Nothing) `shouldThrow` anyException
    runQueryEffect tr (Projects.userByEmail "unverified@example.com") >>= (`shouldSatisfy` isNothing)
    [_, signup] <- callback cookie
    Just (uid :: Projects.UserId) <- runQueryEffect tr $ Hasql.interpOne [HI.sql| SELECT id FROM users.users WHERE email = 'callback@example.com' |]
    (setCookieName signup, setCookieValue signup) `shouldBe` ("monoscope_signup", encodeUtf8 uid.toText)
    runQueryEffect tr (Hasql.interpOne [HI.sql| SELECT attribution->>'gclid' FROM users.users WHERE id = #{uid} |]) >>= (`shouldBe` Just (Just "G1" :: Maybe Text))
    (reqs, convs) <- sendQueued
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
    sendQueued >>= (`shouldBe` ([], []))

    -- Every ad and analytics tag loads behind the browser consent check.
    let cfg = tr.trATCtx.config{Config.googleAdsConversionId = Just "AW-1", Config.facebookPixelId1 = Just "fb1", Config.googleTagManagerId = Just "GTM-1", Config.linkedInPartnerId = Just "li1", Config.postHogApiKey = Just "ph1"}
        html = toStrict $ Lucid.renderText $ BodyWrapper.externalHeadScripts_ cfg
    html `shouldNotSatisfy` T.isInfixOf "<noscript"
    html `shouldNotSatisfy` T.isInfixOf "src=\""
    length (filter (T.isPrefixOf "if (window.msConsent)") $ T.splitOn "<script>" html) `shouldBe` 6

    -- The hourly job reports a recent project's first ingest once, credited to its consenting creator.
    now <- getCurrentTime
    Just (owner :: Projects.UserId) <- runQueryEffect tr $ Hasql.interpOne [HI.sql| SELECT user_id FROM projects.project_members WHERE project_id = #{testPid} ORDER BY created_at, id LIMIT 1 |]
    runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE users.users SET attribution = jsonb_build_object('gclid', 'G2', 'clicked_at', extract(epoch FROM now())::bigint::text) WHERE id = #{owner} |]
    apiKey <- createTestAPIKey tr testPid "ad-activation"
    ingestTrace tr apiKey "ad-activation" (addUTCTime (-600) now)
    let hourly createdAt = do
          runQueryEffect tr $ Hasql.interpExecute_ [HI.sql| UPDATE projects.projects SET created_at = #{createdAt} WHERE id = #{testPid} |]
          runTestBg now tr $ BackgroundJobs.runHourlyJob now 0
          second (map (T.isInfixOf ("\"activated-" <> testPid.toText <> "\"") . decodeUtf8 . snd)) . swap <$> sendQueued
    hourly (addUTCTime (-(31 * nominalDay)) now) >>= (`shouldBe` ([], []))
    hourly now >>= (`shouldBe` ([Activated testPid], [True]))
    hourly now >>= (`shouldBe` ([], []))
    offline <- runQueryEffect tr $ Hasql.interp [HI.sql| SELECT order_id, gclid, right(conversion_time, 5) FROM users.google_ads_offline_conversions WHERE gclid = 'G2' ORDER BY order_id |]
    offline `shouldBe` ([("activated-" <> testPid.toText, "G2", "+0000"), ("signup-" <> owner.toText, "G2", "+0000")] :: [(Text, Text, Text)])
