module Web.AuthSpec (spec) where

import Data.Effectful.Hasql qualified as Hasql
import Data.List qualified as L
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasql.Interpolate qualified as HI
import Pkg.TestUtils
import Relude
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import Servant.Server (ServerError (..))
import System.Config (AuthContext (hasqlPool))
import Test.Hspec
import Web.Auth qualified as Auth
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
            $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing (Just "/chart_data?pid=p&since=1H") chal

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
            $ Auth.sessionByID Nothing "requestID" False "light" I18n.En Nothing demoUrl Auth.ChallengeRedirect
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
