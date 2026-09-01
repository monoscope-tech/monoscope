module Web.AuthSpec (spec) where

import Data.List qualified as L
import Data.Text qualified as T
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
