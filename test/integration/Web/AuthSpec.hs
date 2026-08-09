module Web.AuthSpec (spec) where

import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Relude
import Servant qualified
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import System.Config (AuthContext (hasqlPool))
import Test.Hspec
import Web.Auth qualified as Auth
import Web.OIDC qualified as OIDC


spec :: Spec
spec = aroundAll withTestResources do
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

  describe "generic OIDC identity linking" do
    it "consumes each authorization state exactly once" \tr -> do
      let nonce = T.replicate 43 "n"
          verifier = T.replicate 43 "v"
      inserted <-
        runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
          $ OIDC.insertPendingAuth "one-time-state" nonce verifier "/p/project"
      inserted `shouldBe` Right True
      firstConsume <-
        runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
          $ OIDC.consumePendingAuth "one-time-state"
      firstConsume `shouldBe` Right (Just $ OIDC.PendingAuth nonce verifier "/p/project")
      secondConsume <-
        runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
          $ OIDC.consumePendingAuth "one-time-state"
      secondConsume `shouldBe` Right Nothing

    it "links one verified local account and reuses the stable issuer/subject identity" \tr -> do
      let localUser :: Projects.User
          localUser = (Servant.getResponse tr.trSessAndHeader).user
          email = CI.original localUser.email
          claims = OIDC.IdentityClaims "stable-subject" (Just email) (Just True) "Test" "User" ""
          settings = oidcSettings "verified_email"
      firstLogin <- OIDC.resolveOrProvisionIdentityIO tr.trPool settings claims Nothing
      fmap (\(user :: Projects.User) -> user.id) firstLogin `shouldBe` Right localUser.id

      let changedClaims = OIDC.IdentityClaims "stable-subject" (Just "changed@example.com") (Just False) "Changed" "Name" ""
      repeatLogin <- OIDC.resolveOrProvisionIdentityIO tr.trPool (oidcSettings "disabled") changedClaims Nothing
      fmap (\(user :: Projects.User) -> user.id) repeatLogin `shouldBe` Right localUser.id

    it "fails closed for unverified email and a second subject for the same issuer/user" \tr -> do
      let localUser :: Projects.User
          localUser = (Servant.getResponse tr.trSessAndHeader).user
          email = CI.original localUser.email
          unverified = OIDC.IdentityClaims "unverified-subject" (Just email) (Just False) "Test" "User" ""
          firstSubject = OIDC.IdentityClaims "first-subject" (Just email) (Just True) "Test" "User" ""
          secondSubject = OIDC.IdentityClaims "second-subject" (Just email) (Just True) "Test" "User" ""
          settings = oidcSettingsFor "https://identity-failclosed.test" "verified_email"
      unverifiedResult <- OIDC.resolveOrProvisionIdentityIO tr.trPool settings unverified Nothing
      unverifiedResult `shouldSatisfy` isIdentityResolutionFailure
      firstLogin <- OIDC.resolveOrProvisionIdentityIO tr.trPool settings firstSubject Nothing
      fmap (\(user :: Projects.User) -> user.id) firstLogin `shouldBe` Right localUser.id
      secondResult <- OIDC.resolveOrProvisionIdentityIO tr.trPool settings secondSubject Nothing
      secondResult `shouldSatisfy` isIdentityResolutionFailure

    it "rejects unverified and missing verification claims during auto-registration" \tr -> do
      let email = "unverified-auto-registration@example.com"
          settings = oidcSettingsForAutoRegistration "https://identity-auto-registration.test" "disabled" True
          claims subject verified = OIDC.IdentityClaims subject (Just email) verified "Test" "User" ""
      candidateResult <-
        runTestEffect tr.trPool tr.trATCtx.hasqlPool tr.trLogger tr.trTracerProvider
          $ Projects.createUser "Test" "User" "" email
      candidate <- case candidateResult of
        Right user -> pure user
        Left _ -> fail "could not construct OIDC auto-registration candidate"
      unverifiedResult <- OIDC.resolveOrProvisionIdentityIO tr.trPool settings (claims "unverified-auto-subject" $ Just False) (Just candidate)
      unverifiedResult `shouldSatisfy` isIdentityResolutionFailure
      missingVerificationResult <- OIDC.resolveOrProvisionIdentityIO tr.trPool settings (claims "missing-verification-auto-subject" Nothing) (Just candidate)
      missingVerificationResult `shouldSatisfy` isIdentityResolutionFailure


isIdentityResolutionFailure :: Either OIDC.Failure Projects.User -> Bool
isIdentityResolutionFailure = \case
  Left OIDC.IdentityResolutionFailure -> True
  _ -> False


oidcSettings :: Text -> OIDC.Settings
oidcSettings = oidcSettingsFor "https://identity.test"


oidcSettingsFor :: Text -> Text -> OIDC.Settings
oidcSettingsFor issuer linkMode = oidcSettingsForAutoRegistration issuer linkMode False


oidcSettingsForAutoRegistration :: Text -> Text -> Bool -> OIDC.Settings
oidcSettingsForAutoRegistration issuer linkMode autoRegister =
  either error id
    $ OIDC.validateSettings
      True
      issuer
      ""
      "monoscope-test"
      "test-secret"
      "https://monoscope.test/auth_callback"
      "https://monoscope.test/"
      "openid profile email"
      "RS256"
      linkMode
      autoRegister
