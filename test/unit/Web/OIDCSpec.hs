module Web.OIDCSpec (spec) where

import Data.Aeson qualified as AE

import Data.Char qualified as Char
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Relude
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Web.OIDC


validSettings :: Settings
validSettings =
  either error id
    $ validateSettings
      True
      "https://identity.example.com"
      ""
      "monoscope"
      "secret"
      "https://monoscope.example.com/auth_callback"
      "https://monoscope.example.com/"
      "openid profile email"
      "RS256"
      "disabled"
      False


validDiscovery :: Discovery
validDiscovery =
  Discovery
    { issuer = "https://identity.example.com"
    , authorizationEndpoint = "https://identity.example.com/authorize"
    , tokenEndpoint = "https://identity.example.com/token"
    , userinfoEndpoint = "https://identity.example.com/userinfo"
    , jwksUri = "https://identity.example.com/jwks"
    , endSessionEndpoint = Just "https://identity.example.com/logout"
    , codeChallengeMethodsSupported = Just ["S256"]
    , tokenEndpointAuthMethodsSupported = Just ["client_secret_basic"]
    , idTokenSigningAlgValuesSupported = ["RS256"]
    }


nowSeconds :: Int64
nowSeconds = 1_700_000_000


validClaims :: IDTokenClaims
validClaims =
  mkClaims
    "https://identity.example.com"
    "subject-1"
    ["monoscope"]
    Nothing
    (nowSeconds + 300)
    Nothing
    nowSeconds
    "expected-nonce"
    (Just "user@example.com")
    (Just True)
    (Just "Test")


mkClaims
  :: Text
  -> Text
  -> [Text]
  -> Maybe Text
  -> Int64
  -> Maybe Int64
  -> Int64
  -> Text
  -> Maybe Text
  -> Maybe Bool
  -> Maybe Text
  -> IDTokenClaims
mkClaims claimIssuer claimSubject claimAudience claimAzp claimExpiry claimNbf claimIat claimNonce claimEmail claimEmailVerified claimGivenName =
  IDTokenClaims
    { issuer = claimIssuer
    , subject = claimSubject
    , audience = claimAudience
    , authorizedParty = claimAzp
    , expiresAt = claimExpiry
    , notBefore = claimNbf
    , issuedAt = claimIat
    , nonce = claimNonce
    , email = claimEmail
    , emailVerified = claimEmailVerified
    , givenName = claimGivenName
    , familyName = Just "User"
    , picture = Nothing
    }


isSuccess :: Either a b -> Bool
isSuccess = isRight


spec :: Spec
spec = do
  describe "validateSettings" do
    it "accepts a complete HTTPS configuration and defaults discovery from issuer" do
      validSettings.discoveryUrl `shouldBe` "https://identity.example.com/.well-known/openid-configuration"
      validSettings.scopes `shouldBe` ["openid", "profile", "email"]
      validSettings.existingUserLinkMode `shouldBe` LinkDisabled

    it "requires HTTPS outside development" do
      isSuccess (validateSettings True "http://identity.example.com" "" "client" "secret" "https://app.example.com/auth_callback" "https://app.example.com/" "openid" "RS256" "disabled" False)
        `shouldBe` False

    it "requires the openid scope" do
      isSuccess (validateSettings True "https://identity.example.com" "" "client" "secret" "https://app.example.com/auth_callback" "https://app.example.com/" "profile email" "RS256" "disabled" False)
        `shouldBe` False

    it "rejects symmetric and unknown signing algorithms" do
      isSuccess (validateSettings True "https://identity.example.com" "" "client" "secret" "https://app.example.com/auth_callback" "https://app.example.com/" "openid" "HS256" "disabled" False)
        `shouldBe` False

    it "rejects issuer query strings and URL userinfo" do
      isSuccess (validateSettings True "https://identity.example.com?tenant=one" "" "client" "secret" "https://app.example.com/auth_callback" "https://app.example.com/" "openid" "RS256" "disabled" False)
        `shouldBe` False
      isSuccess (validateSettings True "https://user@identity.example.com" "" "client" "secret" "https://app.example.com/auth_callback" "https://app.example.com/" "openid" "RS256" "disabled" False)
        `shouldBe` False

  describe "validateDiscovery" do
    it "accepts pinned issuer, PKCE S256, client_secret_basic, and an allowed signing algorithm" do
      validateDiscovery True validSettings validDiscovery `shouldBe` Right validDiscovery

    it "rejects issuer mismatch" do
      validateDiscovery True validSettings validDiscovery{issuer = "https://other.example.com"}
        `shouldSatisfy` (not . isSuccess)

    it "requires advertised PKCE S256" do
      validateDiscovery True validSettings validDiscovery{codeChallengeMethodsSupported = Nothing}
        `shouldSatisfy` (not . isSuccess)
      validateDiscovery True validSettings validDiscovery{codeChallengeMethodsSupported = Just ["plain"]}
        `shouldSatisfy` (not . isSuccess)

    it "rejects unsupported token authentication and signing algorithms" do
      validateDiscovery True validSettings validDiscovery{tokenEndpointAuthMethodsSupported = Just ["client_secret_post"]}
        `shouldSatisfy` (not . isSuccess)
      validateDiscovery True validSettings validDiscovery{idTokenSigningAlgValuesSupported = ["ES256"]}
        `shouldSatisfy` (not . isSuccess)

    it "rejects insecure discovered endpoints in production" do
      validateDiscovery True validSettings validDiscovery{tokenEndpoint = "http://identity.example.com/token"}
        `shouldSatisfy` (not . isSuccess)

  describe "redirect sanitization" do
    it "keeps only local absolute paths" do
      sanitizeRedirectTo (Just "/p/project/dashboards?tab=1") `shouldBe` "/p/project/dashboards?tab=1"
      sanitizeRedirectTo Nothing `shouldBe` "/"

    it "rejects protocol-relative, absolute, backslash, control-character, and overlong redirects" do
      let unsafe :: [Text]
          unsafe =
            [ "//evil.example/path"
            , "https://evil.example/path"
            , "/\\evil"
            , "/safe\r\nLocation: https://evil.example"
            , "/safe\NULevil"
            , "/" <> T.replicate 4097 "x"
            ]
      traverse_ (\value -> sanitizeRedirectTo (Just value) `shouldBe` "/") unsafe

  describe "PKCE and state" do
    it "matches the RFC 7636 S256 test vector" do
      pkceChallenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
        `shouldBe` "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"

    it "generates high-entropy base64url state, nonce, and verifier" do
      (oauthState, nonceValue, verifier) <- generatePKCE
      traverse_ (\value -> T.length value `shouldBe` 43) [oauthState, nonceValue, verifier]
      traverse_ (\value -> value `shouldSatisfy` T.all (\c -> Char.isAlphaNum c || c == '-' || c == '_')) [oauthState, nonceValue, verifier]
      oauthState `shouldSatisfy` (/= nonceValue)
      nonceValue `shouldSatisfy` (/= verifier)

    it "compares state values exactly" do
      statesMatch "same-state" "same-state" `shouldBe` True
      statesMatch "same-state" "different-state" `shouldBe` False

  describe "authorizationURL" do
    it "uses the fixed callback, scopes, nonce, state, and PKCE challenge" do
      let url = authorizationURL validSettings validDiscovery "state-value" "nonce-value" "challenge-value"
      url `shouldSatisfy` T.isPrefixOf "https://identity.example.com/authorize?"
      traverse_
        (\fragment -> url `shouldSatisfy` T.isInfixOf fragment)
        [ "response_type=code"
        , "client_id=monoscope"
        , "redirect_uri=https%3A%2F%2Fmonoscope.example.com%2Fauth_callback"
        , "scope=openid%20profile%20email"
        , "state=state-value"
        , "nonce=nonce-value"
        , "code_challenge=challenge-value"
        , "code_challenge_method=S256"
        ]

  describe "token responses" do
    it "requires an OAuth Bearer token type" do
      let decode body = isSuccess (AE.eitherDecode @TokenResponse $ encodeUtf8 body)
      decode "{\"access_token\":\"access\",\"id_token\":\"id\",\"token_type\":\"Bearer\"}" `shouldBe` True
      decode "{\"access_token\":\"access\",\"id_token\":\"id\",\"token_type\":\"DPoP\"}" `shouldBe` False
      decode "{\"access_token\":\"access\",\"id_token\":\"id\"}" `shouldBe` False

  describe "validateClaims" do
    let now = posixSecondsToUTCTime $ fromIntegral nowSeconds
        validate = validateClaims validSettings now "expected-nonce"
    it "accepts a valid ID token" do
      validate validClaims `shouldBe` Right validClaims

    it "rejects issuer, subject, audience, azp, expiry, time, and nonce failures" do
      let claims claimIssuer claimSubject claimAudience claimAzp claimExpiry claimNbf claimIat claimNonce =
            mkClaims claimIssuer claimSubject claimAudience claimAzp claimExpiry claimNbf claimIat claimNonce (Just "user@example.com") (Just True) (Just "Test")
          invalidClaims :: [IDTokenClaims]
          invalidClaims =
            [ claims "https://other.example.com" "subject-1" ["monoscope"] Nothing (nowSeconds + 300) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "" ["monoscope"] Nothing (nowSeconds + 300) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" (T.replicate 256 "s") ["monoscope"] Nothing (nowSeconds + 300) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["other-client"] Nothing (nowSeconds + 300) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["monoscope", "other-client"] Nothing (nowSeconds + 300) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["monoscope", "other-client"] (Just "other-client") (nowSeconds + 300) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["monoscope"] Nothing (nowSeconds - 61) Nothing nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["monoscope"] Nothing (nowSeconds + 300) (Just $ nowSeconds + 61) nowSeconds "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["monoscope"] Nothing (nowSeconds + 300) Nothing (nowSeconds + 61) "expected-nonce"
            , claims "https://identity.example.com" "subject-1" ["monoscope"] Nothing (nowSeconds + 300) Nothing nowSeconds "wrong-nonce"
            ]
      traverse_ (\invalid -> validate invalid `shouldSatisfy` (not . isSuccess)) invalidClaims

  describe "mergeIdentityClaims" do
    let userInfo = UserInfoClaims "subject-1" (Just "userinfo@example.com") (Just True) (Just "Info") (Just "User") Nothing
    it "requires UserInfo sub to match the signed ID token" do
      mergeIdentityClaims validClaims (UserInfoClaims "other-subject" userInfo.email userInfo.emailVerified userInfo.givenName userInfo.familyName userInfo.picture)
        `shouldSatisfy` (not . isSuccess)

    it "keeps signed ID-token claims ahead of UserInfo" do
      let token = mkClaims validClaims.issuer validClaims.subject validClaims.audience validClaims.authorizedParty validClaims.expiresAt validClaims.notBefore validClaims.issuedAt validClaims.nonce (Just "signed@example.com") (Just False) (Just "Signed")
      merged <- either (error . show) pure $ mergeIdentityClaims token userInfo
      merged.email `shouldBe` Just "signed@example.com"
      merged.emailVerified `shouldBe` Just False
      merged.givenName `shouldBe` "Signed"

    it "does not borrow UserInfo verification for an ID-token email" do
      let token = mkClaims validClaims.issuer validClaims.subject validClaims.audience validClaims.authorizedParty validClaims.expiresAt validClaims.notBefore validClaims.issuedAt validClaims.nonce (Just "signed@example.com") Nothing validClaims.givenName
      merged <- either (error . show) pure $ mergeIdentityClaims token userInfo
      merged.email `shouldBe` Just "signed@example.com"
      merged.emailVerified `shouldBe` Nothing

    it "uses the UserInfo email and verification as a pair when the ID token omits email" do
      let token = mkClaims validClaims.issuer validClaims.subject validClaims.audience validClaims.authorizedParty validClaims.expiresAt validClaims.notBefore validClaims.issuedAt validClaims.nonce Nothing Nothing validClaims.givenName
      merged <- either (error . show) pure $ mergeIdentityClaims token userInfo
      merged.email `shouldBe` Just "userinfo@example.com"
      merged.emailVerified `shouldBe` Just True
