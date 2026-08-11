module Web.OIDC (
  Settings (..),
  Discovery (..),
  LinkMode (..),
  Failure (..),
  PendingAuth (..),
  TokenResponse (..),
  IdentityClaims (..),
  IDTokenClaims (..),
  UserInfoClaims (..),
  validateSettings,
  fetchDiscoveryIO,
  validateDiscovery,
  sanitizeRedirectTo,
  generatePKCE,
  pkceChallenge,
  stateHash,
  statesMatch,
  pendingStateCookie,
  expiredStateCookie,
  appendQueryParameters,
  authorizationURL,
  insertPendingAuth,
  consumePendingAuth,
  exchangeCodeIO,
  fetchUserInfoIO,
  validateIDTokenIO,
  validateClaims,
  mergeIdentityClaims,
  verifiedAutoRegistrationEmail,
  resolveOrProvisionIdentityIO,
) where

import Control.Exception (Exception)
import Control.Lens ((&), (.~), (?~), (^.))
import Control.Monad.Except (throwError)

import Data.Aeson qualified as AE
import Data.Base64.Types qualified as B64
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.Effectful.Hasql qualified as EHasql
import Data.Int (Int64)
import Data.Pool qualified as Pool
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.PostgreSQL.Simple qualified as PG
import Effectful (Eff)
import Hasql.Interpolate qualified as HI
import Jose.Jwa (JwsAlg (..))
import Jose.Jwk (Jwk)
import Jose.Jwt qualified as Jose
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client (managerResponseTimeout, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusIsSuccessful, urlEncode)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Network.Wreq (FormParam ((:=)))
import Network.Wreq qualified as Wreq
import Pkg.DeriveUtils (DB)
import Relude
import UnliftIO.Exception qualified as Safe
import Web.Cookie (SetCookie (..), defaultSetCookie, sameSiteLax)
import "base64" Data.ByteString.Base64.URL qualified as B64URL
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)
import "cryptonite" Crypto.Random (getRandomBytes)


data LinkMode = LinkDisabled | LinkVerifiedEmail
  deriving stock (Eq, Show)


data Settings = Settings
  { issuer :: Text
  , discoveryUrl :: Text
  , clientId :: Text
  , clientSecret :: Text
  , callbackUrl :: Text
  , logoutRedirect :: Text
  , scopes :: [Text]
  , allowedAlgorithms :: [JwsAlg]
  , existingUserLinkMode :: LinkMode
  , autoRegister :: Bool
  }


data Discovery = Discovery
  { issuer :: Text
  , authorizationEndpoint :: Text
  , tokenEndpoint :: Text
  , userinfoEndpoint :: Maybe Text
  , jwksUri :: Text
  , endSessionEndpoint :: Maybe Text
  , codeChallengeMethodsSupported :: Maybe [Text]
  , tokenEndpointAuthMethodsSupported :: Maybe [Text]
  , idTokenSigningAlgValuesSupported :: [Text]
  }
  deriving stock (Eq, Show)


instance AE.FromJSON Discovery where
  parseJSON = AE.withObject "OIDC discovery document" \o ->
    Discovery
      <$> o
      AE..: "issuer"
      <*> o
      AE..: "authorization_endpoint"
      <*> o
      AE..: "token_endpoint"
      <*> o
      AE..:? "userinfo_endpoint"
      <*> o
      AE..: "jwks_uri"
      <*> o
      AE..:? "end_session_endpoint"
      <*> o
      AE..:? "code_challenge_methods_supported"
      <*> o
      AE..:? "token_endpoint_auth_methods_supported"
      <*> o
      AE..: "id_token_signing_alg_values_supported"


data Failure
  = DiscoveryNetworkFailure
  | DiscoveryResponseFailure
  | TokenNetworkFailure
  | TokenResponseFailure
  | UserInfoNetworkFailure
  | UserInfoResponseFailure
  | JwksNetworkFailure
  | JwksResponseFailure
  | TokenHeaderFailure
  | TokenAlgorithmFailure
  | TokenKeyFailure
  | TokenSignatureFailure
  | TokenClaimsFailure
  | UserInfoSubjectFailure
  | IdentityResolutionFailure
  deriving stock (Eq, Show)


data PendingAuth = PendingAuth
  { nonce :: Text
  , codeVerifier :: Text
  , redirectTo :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (HI.DecodeRow)


data TokenResponse = TokenResponse
  { accessToken :: Text
  , idToken :: Text
  }


instance AE.FromJSON TokenResponse where
  parseJSON = AE.withObject "OIDC token response" \o -> do
    accessToken <- o AE..: "access_token"
    idToken <- o AE..: "id_token"
    tokenType :: Text <- o AE..: "token_type"
    unless (T.toCaseFold tokenType == "bearer") $ fail "OIDC token_type must be Bearer"
    pure TokenResponse{..}


newtype Audience = Audience [Text]


instance AE.FromJSON Audience where
  parseJSON (AE.String a) = pure $ Audience [a]
  parseJSON v = Audience <$> AE.parseJSON v


data IDTokenClaims = IDTokenClaims
  { issuer :: Text
  , subject :: Text
  , audience :: [Text]
  , authorizedParty :: Maybe Text
  , expiresAt :: Int64
  , notBefore :: Maybe Int64
  , issuedAt :: Int64
  , nonce :: Text
  , email :: Maybe Text
  , emailVerified :: Maybe Bool
  , givenName :: Maybe Text
  , familyName :: Maybe Text
  , picture :: Maybe Text
  }
  deriving stock (Eq, Show)


instance AE.FromJSON IDTokenClaims where
  parseJSON = AE.withObject "OIDC ID token claims" \o -> do
    Audience audience <- o AE..: "aud"
    IDTokenClaims
      <$> o
      AE..: "iss"
      <*> o
      AE..: "sub"
      <*> pure audience
      <*> o
      AE..:? "azp"
      <*> o
      AE..: "exp"
      <*> o
      AE..:? "nbf"
      <*> o
      AE..: "iat"
      <*> o
      AE..: "nonce"
      <*> o
      AE..:? "email"
      <*> o
      AE..:? "email_verified"
      <*> o
      AE..:? "given_name"
      <*> o
      AE..:? "family_name"
      <*> o
      AE..:? "picture"


data UserInfoClaims = UserInfoClaims
  { subject :: Text
  , email :: Maybe Text
  , emailVerified :: Maybe Bool
  , givenName :: Maybe Text
  , familyName :: Maybe Text
  , picture :: Maybe Text
  }
  deriving stock (Eq, Show)


instance AE.FromJSON UserInfoClaims where
  parseJSON = AE.withObject "OIDC userinfo response" \o ->
    UserInfoClaims
      <$> o
      AE..: "sub"
      <*> o
      AE..:? "email"
      <*> o
      AE..:? "email_verified"
      <*> o
      AE..:? "given_name"
      <*> o
      AE..:? "family_name"
      <*> o
      AE..:? "picture"


data IdentityClaims = IdentityClaims
  { subject :: Text
  , email :: Maybe Text
  , emailVerified :: Maybe Bool
  , givenName :: Text
  , familyName :: Text
  , picture :: Text
  }
  deriving stock (Eq, Show)


parseList :: Text -> [Text]
parseList = filter (not . T.null) . words . T.map (\c -> if c == ',' then ' ' else c)


parseAlgorithm :: Text -> Maybe JwsAlg
parseAlgorithm = \case
  "RS256" -> Just RS256
  "RS384" -> Just RS384
  "RS512" -> Just RS512
  "ES256" -> Just ES256
  "ES384" -> Just ES384
  "ES512" -> Just ES512
  "EdDSA" -> Just EdDSA
  _ -> Nothing


validateSettings
  :: Bool
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Bool
  -> Either Text Settings
validateSettings requireHttps issuer discoveryUrl clientId clientSecret callbackUrl logoutRedirect scopesText algorithmsText linkModeText autoRegister = do
  requireNonEmpty "issuer" issuer
  requireNonEmpty "client_id" clientId
  requireNonEmpty "client_secret" clientSecret
  requireNonEmpty "callback_url" callbackUrl
  requireNonEmpty "logout_redirect" logoutRedirect
  when (T.length issuer > 2048) $ Left "OIDC issuer is too long"
  validateAbsoluteUrl requireHttps "issuer" issuer
  validateIssuer issuer
  let effectiveDiscoveryUrl = if T.null discoveryUrl then T.dropWhileEnd (== '/') issuer <> "/.well-known/openid-configuration" else discoveryUrl
  validateAbsoluteUrl requireHttps "discovery_url" effectiveDiscoveryUrl
  validateAbsoluteUrl requireHttps "callback_url" callbackUrl
  validateAbsoluteUrl requireHttps "logout_redirect" logoutRedirect
  let scopes = parseList scopesText
  unless ("openid" `elem` scopes) $ Left "OIDC_SCOPES must include openid"
  let algorithmNames = parseList algorithmsText
  algorithms <- maybeToRight "OIDC_ALLOWED_ALGORITHMS contains an unsupported or symmetric algorithm" $ traverse parseAlgorithm algorithmNames
  when (null algorithms) $ Left "OIDC_ALLOWED_ALGORITHMS must not be empty"
  existingUserLinkMode <- case T.toLower linkModeText of
    "disabled" -> Right LinkDisabled
    "verified_email" -> Right LinkVerifiedEmail
    _ -> Left "OIDC_EXISTING_USER_LINK_MODE must be disabled or verified_email"
  pure Settings{discoveryUrl = effectiveDiscoveryUrl, allowedAlgorithms = algorithms, ..}
  where
    requireNonEmpty label value = when (T.null $ T.strip value) $ Left ("OIDC " <> label <> " is required")


validateAbsoluteUrl :: Bool -> Text -> Text -> Either Text ()
validateAbsoluteUrl requireHttps label value = case parseURI (toString value) of
  Just URI{uriScheme, uriAuthority = Just URIAuth{uriUserInfo = ""}, uriFragment = ""}
    | uriScheme == "https:" -> Right ()
    | not requireHttps && uriScheme == "http:" -> Right ()
  _ -> Left $ "OIDC " <> label <> " must be an absolute " <> bool "HTTP(S)" "HTTPS" requireHttps <> " URL without a fragment"


validateIssuer :: Text -> Either Text ()
validateIssuer value = case parseURI (toString value) of
  Just URI{uriQuery = "", uriFragment = ""} -> Right ()
  _ -> Left "OIDC issuer must not contain a query or fragment"


validateDiscovery :: Bool -> Settings -> Discovery -> Either Text Discovery
validateDiscovery requireHttps settings discovery = do
  unless (discovery.issuer == settings.issuer) $ Left "OIDC discovery issuer mismatch"
  traverse_
    (uncurry $ validateAbsoluteUrl requireHttps)
    ( [ ("authorization_endpoint", discovery.authorizationEndpoint)
      , ("token_endpoint", discovery.tokenEndpoint)
      , ("jwks_uri", discovery.jwksUri)
      ]
        :: [(Text, Text)]
    )
  traverse_ (validateAbsoluteUrl requireHttps "userinfo_endpoint") discovery.userinfoEndpoint
  traverse_ (validateAbsoluteUrl requireHttps "end_session_endpoint") discovery.endSessionEndpoint
  whenJust discovery.codeChallengeMethodsSupported \methods ->
    unless ("S256" `elem` methods) $ Left "OIDC provider explicitly excludes PKCE S256"
  whenJust discovery.tokenEndpointAuthMethodsSupported \methods ->
    unless ("client_secret_basic" `elem` methods) $ Left "OIDC provider does not advertise client_secret_basic"
  unless
    ( any
        (maybe False (`elem` settings.allowedAlgorithms) . parseAlgorithm)
        discovery.idTokenSigningAlgValuesSupported
    )
    $ Left "OIDC provider does not advertise an allowed ID-token signing algorithm"
  pure discovery


boundedOptions :: Wreq.Options
boundedOptions =
  Wreq.defaults
    & Wreq.manager .~ Left tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro 5_000_000}
    & Wreq.redirects .~ 0
    & Wreq.header "Accept" .~ ["application/json"]


fetchDiscoveryIO :: Bool -> Settings -> IO (Either Failure Discovery)
fetchDiscoveryIO requireHttps settings = do
  result <- Safe.try $ Wreq.getWith boundedOptions (toString settings.discoveryUrl)
  pure case result of
    Left (_ :: SomeException) -> Left DiscoveryNetworkFailure
    Right response
      | not (statusIsSuccessful $ response ^. Wreq.responseStatus) -> Left DiscoveryResponseFailure
      | otherwise ->
          first (const DiscoveryResponseFailure) (AE.eitherDecode (response ^. Wreq.responseBody))
            >>= first (const DiscoveryResponseFailure)
            . validateDiscovery requireHttps settings


sanitizeRedirectTo :: Maybe Text -> Text
sanitizeRedirectTo redirectM = case redirectM of
  Just redirect
    | T.length redirect <= 4096
    , T.isPrefixOf "/" redirect
    , not (T.isPrefixOf "//" redirect)
    , not (T.any (\c -> c == '\\' || c == '\r' || c == '\n' || c == '\0') redirect) ->
        redirect
  _ -> "/"


base64Url :: ByteString -> Text
base64Url = B64.extractBase64 . B64URL.encodeBase64Unpadded


sha256 :: ByteString -> ByteString
sha256 value = BA.convert (hash value :: Digest SHA256)


generatePKCE :: IO (Text, Text, Text)
generatePKCE = do
  oauthState <- base64Url <$> getRandomBytes 32
  nonce <- base64Url <$> getRandomBytes 32
  verifier <- base64Url <$> getRandomBytes 32
  pure (oauthState, nonce, verifier)


pkceChallenge :: Text -> Text
pkceChallenge = base64Url . sha256 . encodeUtf8


stateHash :: Text -> ByteString
stateHash = sha256 . encodeUtf8


statesMatch :: Text -> Text -> Bool
statesMatch left right = BA.constEq (encodeUtf8 left :: ByteString) (encodeUtf8 right :: ByteString)


pendingStateCookie :: Text -> SetCookie
pendingStateCookie oauthState =
  defaultSetCookie
    { setCookieName = "monoscope_oidc_state"
    , setCookieValue = encodeUtf8 oauthState
    , setCookiePath = Just "/auth_callback"
    , setCookieMaxAge = Just 600
    , setCookieHttpOnly = True
    , setCookieSecure = True
    , setCookieSameSite = Just sameSiteLax
    }


expiredStateCookie :: SetCookie
expiredStateCookie = (pendingStateCookie ""){setCookieMaxAge = Just 0}


queryEscape :: Text -> Text
queryEscape = decodeUtf8 . urlEncode True . encodeUtf8


appendQueryParameters :: Text -> [(Text, Text)] -> Text
appendQueryParameters endpoint [] = endpoint
appendQueryParameters endpoint parameters =
  endpoint <> separator <> T.intercalate "&" (fmap renderParameter parameters)
  where
    separator
      | T.isSuffixOf "?" endpoint || T.isSuffixOf "&" endpoint = ""
      | "?" `T.isInfixOf` endpoint = "&"
      | otherwise = "?"
    renderParameter (name, value) = queryEscape name <> "=" <> queryEscape value


authorizationURL :: Settings -> Discovery -> Text -> Text -> Text -> Text
authorizationURL settings discovery oauthState nonce challenge =
  appendQueryParameters
    discovery.authorizationEndpoint
    [ ("response_type", "code")
    , ("client_id", settings.clientId)
    , ("redirect_uri", settings.callbackUrl)
    , ("scope", T.intercalate " " settings.scopes)
    , ("state", oauthState)
    , ("nonce", nonce)
    , ("code_challenge", challenge)
    , ("code_challenge_method", "S256")
    ]
insertPendingAuth :: DB es => Text -> Text -> Text -> Text -> Eff es Bool
insertPendingAuth oauthState nonce verifier redirectTo =
  (== Just (1 :: Int64))
    <$> EHasql.interpOne
      [HI.sql|
        WITH expired AS (
          DELETE FROM users.oidc_pending_auth_requests WHERE expires_at <= now()
        ), inserted AS (
          INSERT INTO users.oidc_pending_auth_requests(state_hash, nonce, code_verifier, redirect_to, expires_at)
          VALUES (#{stateHash oauthState}, #{nonce}, #{verifier}, #{redirectTo}, now() + interval '10 minutes')
          ON CONFLICT (state_hash) DO NOTHING
          RETURNING 1
        )
        SELECT count(*)::bigint FROM inserted
      |]


consumePendingAuth :: DB es => Text -> Eff es (Maybe PendingAuth)
consumePendingAuth oauthState =
  EHasql.interpOne
    [HI.sql|
      WITH consumed AS (
        DELETE FROM users.oidc_pending_auth_requests
        WHERE state_hash = #{stateHash oauthState}
        RETURNING nonce, code_verifier, redirect_to, expires_at
      )
      SELECT nonce, code_verifier, redirect_to
      FROM consumed
      WHERE expires_at > now()
    |]


exchangeCodeIO :: Settings -> Discovery -> Text -> Text -> IO (Either Failure TokenResponse)
exchangeCodeIO settings discovery code verifier = do
  let opts =
        boundedOptions
          & Wreq.auth ?~ Wreq.basicAuth (oauthBasicValue settings.clientId) (oauthBasicValue settings.clientSecret)
          & Wreq.header "Content-Type" .~ ["application/x-www-form-urlencoded"]
      form =
        [ "grant_type" := ("authorization_code" :: Text)
        , "code" := code
        , "redirect_uri" := settings.callbackUrl
        , "code_verifier" := verifier
        ]
          :: [FormParam]
  result <- Safe.try $ Wreq.postWith opts (toString discovery.tokenEndpoint) form
  pure case result of
    Left (_ :: SomeException) -> Left TokenNetworkFailure
    Right response
      | not (statusIsSuccessful $ response ^. Wreq.responseStatus) -> Left TokenResponseFailure
      | otherwise -> first (const TokenResponseFailure) $ AE.eitherDecode (response ^. Wreq.responseBody)
  where
    -- RFC 6749 §2.3.1 applies application/x-www-form-urlencoded encoding to
    -- each client credential before constructing the HTTP Basic value.
    oauthBasicValue = urlEncode True . encodeUtf8


fetchUserInfoIO :: Discovery -> Text -> IO (Either Failure (Maybe UserInfoClaims))
fetchUserInfoIO discovery accessToken = case discovery.userinfoEndpoint of
  Nothing -> pure $ Right Nothing
  Just userinfoEndpoint -> do
    let opts = boundedOptions & Wreq.header "Authorization" .~ ["Bearer " <> encodeUtf8 accessToken]
    result <- Safe.try $ Wreq.getWith opts (toString userinfoEndpoint)
    pure case result of
      Left (_ :: SomeException) -> Left UserInfoNetworkFailure
      Right response
        | not (statusIsSuccessful $ response ^. Wreq.responseStatus) -> Left UserInfoResponseFailure
        | otherwise -> Just <$> first (const UserInfoResponseFailure) (AE.eitherDecode $ response ^. Wreq.responseBody)


data UntrustedHeader = UntrustedHeader
  { algorithm :: Text
  , keyId :: Maybe Text
  }


instance AE.FromJSON UntrustedHeader where
  parseJSON = AE.withObject "JWS header" \o -> UntrustedHeader <$> o AE..: "alg" <*> o AE..:? "kid"


data JwkEntry = JwkEntry
  { keyId :: Maybe Text
  , keyUse :: Maybe Text
  , keyOperations :: Maybe [Text]
  , algorithm :: Maybe Text
  , key :: Jwk
  }


instance AE.FromJSON JwkEntry where
  parseJSON value =
    AE.withObject
      "JWK"
      ( \o ->
          JwkEntry
            <$> o
            AE..:? "kid"
            <*> o
            AE..:? "use"
            <*> o
            AE..:? "key_ops"
            <*> o
            AE..:? "alg"
            <*> AE.parseJSON value
      )
      value


newtype JwkSet = JwkSet [JwkEntry]


instance AE.FromJSON JwkSet where
  parseJSON = AE.withObject "JWK set" \o -> JwkSet <$> o AE..: "keys"


parseJwsAlgorithm :: Settings -> ByteString -> Either Failure (JwsAlg, Maybe Text)
parseJwsAlgorithm settings token = do
  headerPart <- case BS.split 46 token of
    [headerPart, _, _] -> Right headerPart
    _ -> Left TokenHeaderFailure
  headerBytes <- first (const TokenHeaderFailure) $ B64URL.decodeBase64UnpaddedUntyped headerPart
  UntrustedHeader{algorithm, keyId} <- first (const TokenHeaderFailure) $ AE.eitherDecodeStrict headerBytes
  algorithm' <- maybeToRight TokenAlgorithmFailure $ parseAlgorithm algorithm
  unless (algorithm' `elem` settings.allowedAlgorithms) $ Left TokenAlgorithmFailure
  when (maybe False T.null keyId) $ Left TokenKeyFailure
  pure (algorithm', keyId)


renderJwsAlgorithm :: JwsAlg -> Text
renderJwsAlgorithm = \case
  RS256 -> "RS256"
  RS384 -> "RS384"
  RS512 -> "RS512"
  ES256 -> "ES256"
  ES384 -> "ES384"
  ES512 -> "ES512"
  EdDSA -> "EdDSA"
  _ -> ""


fetchJwkSetIO :: Discovery -> IO (Either Failure JwkSet)
fetchJwkSetIO discovery = do
  result <- Safe.try $ Wreq.getWith boundedOptions (toString discovery.jwksUri)
  pure case result of
    Left (_ :: SomeException) -> Left JwksNetworkFailure
    Right response
      | not (statusIsSuccessful $ response ^. Wreq.responseStatus) -> Left JwksResponseFailure
      | otherwise -> first (const JwksResponseFailure) $ AE.eitherDecode (response ^. Wreq.responseBody)


validateIDTokenIO :: Settings -> Discovery -> UTCTime -> Text -> Text -> IO (Either Failure IDTokenClaims)
validateIDTokenIO settings discovery now expectedNonce tokenText = runExceptT do
  let token = encodeUtf8 tokenText
  (algorithm, keyId) <- hoistEither $ parseJwsAlgorithm settings token
  unless (renderJwsAlgorithm algorithm `elem` discovery.idTokenSigningAlgValuesSupported)
    $ throwError TokenAlgorithmFailure
  JwkSet entries <- ExceptT $ fetchJwkSetIO discovery
  let matchingKeys =
        [ entry.key
        | entry <- entries
        , maybe True (\kid -> entry.keyId == Just kid) keyId
        , maybe True (== "sig") entry.keyUse
        , maybe True ("verify" `elem`) entry.keyOperations
        , maybe True (== renderJwsAlgorithm algorithm) entry.algorithm
        ]
  signingKey <- case matchingKeys of
    [key] -> pure key
    _ -> throwError TokenKeyFailure
  decoded <- liftIO (Jose.decode [signingKey] (Just $ Jose.JwsEncoding algorithm) token) >>= hoistEither . first (const TokenSignatureFailure)
  claimsBytes <- case decoded of
    Jose.Jws (_, claims) -> pure claims
    Jose.Unsecured _ -> throwError TokenSignatureFailure
    Jose.Jwe _ -> throwError TokenSignatureFailure
  claims <- hoistEither $ first (const TokenClaimsFailure) $ AE.eitherDecodeStrict claimsBytes
  hoistEither $ validateClaims settings now expectedNonce claims


validateClaims :: Settings -> UTCTime -> Text -> IDTokenClaims -> Either Failure IDTokenClaims
validateClaims settings now expectedNonce claims = do
  let nowSeconds = floor (utcTimeToPOSIXSeconds now) :: Int64
      skew = 60
  unless (claims.issuer == settings.issuer) $ Left TokenClaimsFailure
  when (T.null claims.subject || T.length claims.subject > 255) $ Left TokenClaimsFailure
  when (null claims.audience || settings.clientId `notElem` claims.audience) $ Left TokenClaimsFailure
  when (length claims.audience > 1 && claims.authorizedParty /= Just settings.clientId) $ Left TokenClaimsFailure
  whenJust claims.authorizedParty \azp -> unless (azp == settings.clientId) $ Left TokenClaimsFailure
  when (nowSeconds > claims.expiresAt + skew) $ Left TokenClaimsFailure
  whenJust claims.notBefore \nbf -> when (nbf > nowSeconds + skew) $ Left TokenClaimsFailure
  when (claims.issuedAt > nowSeconds + skew) $ Left TokenClaimsFailure
  unless (statesMatch claims.nonce expectedNonce) $ Left TokenClaimsFailure
  pure claims


mergeIdentityClaims :: IDTokenClaims -> Maybe UserInfoClaims -> Either Failure IdentityClaims
mergeIdentityClaims tokenClaims userInfoM = do
  traverse_ validateUserInfo userInfoM
  -- Treat email and email_verified as one claim pair. Mixing the signed
  -- ID-token email with userinfo's verification bit could mark a different
  -- address as verified.
  let userInfo = fromMaybe emptyUserInfo userInfoM
  let (selectedEmail, selectedEmailVerified) = case tokenClaims.email of
        Just email -> (Just email, tokenClaims.emailVerified)
        Nothing -> (userInfo.email, userInfo.emailVerified)
  pure
    IdentityClaims
      { subject = tokenClaims.subject
      , email = selectedEmail
      , emailVerified = selectedEmailVerified
      , givenName = fromMaybe "" $ tokenClaims.givenName <|> userInfo.givenName
      , familyName = fromMaybe "" $ tokenClaims.familyName <|> userInfo.familyName
      , picture = fromMaybe "" $ tokenClaims.picture <|> userInfo.picture
      }
  where
    validateUserInfo userInfo =
      when (T.null userInfo.subject || userInfo.subject /= tokenClaims.subject) $ Left UserInfoSubjectFailure
    emptyUserInfo = UserInfoClaims tokenClaims.subject Nothing Nothing Nothing Nothing Nothing


verifiedAutoRegistrationEmail :: Settings -> IdentityClaims -> Maybe Text
verifiedAutoRegistrationEmail settings identityClaims
  | settings.autoRegister && identityClaims.emailVerified == Just True = identityClaims.email
  | otherwise = Nothing


data IdentityAbort = IdentityAbort
  deriving stock (Show)


instance Exception IdentityAbort


selectIdentityUser :: PG.Connection -> Text -> Text -> IO (Maybe Projects.User)
selectIdentityUser connection issuer subject =
  exactlyOne
    <$> PG.query
      connection
      "SELECT u.* FROM users.oidc_identities oi JOIN users.users u ON u.id = oi.user_id WHERE oi.issuer = ? AND oi.subject = ? AND u.active = TRUE AND u.deleted_at IS NULL LIMIT 2"
      (issuer, subject)
  where
    exactlyOne = \case
      [user] -> Just user
      _ -> Nothing


resolveOrProvisionIdentityIO
  :: Pool.Pool PG.Connection
  -> Settings
  -> IdentityClaims
  -> Maybe Projects.User
  -> IO (Either Failure Projects.User)
resolveOrProvisionIdentityIO pool settings identityClaims candidateUserM = do
  outcome <- Safe.try $ Pool.withResource pool \connection -> PG.withTransaction connection do
    existing <- selectIdentityUser connection settings.issuer identityClaims.subject
    case existing of
      Just user -> pure user
      Nothing -> do
        linked <- case (settings.existingUserLinkMode, identityClaims.emailVerified, identityClaims.email) of
          (LinkVerifiedEmail, Just True, Just email) -> do
            users :: [Projects.User] <- PG.query connection "SELECT * FROM users.users WHERE lower(email::text) = lower(?) AND active = TRUE AND deleted_at IS NULL" (PG.Only email)
            case users of
              [user] -> linkExistingIdentity connection settings.issuer identityClaims.subject user
              _ -> pure Nothing
          _ -> pure Nothing
        case linked of
          Just user -> pure user
          Nothing ->
            case (verifiedAutoRegistrationEmail settings identityClaims, candidateUserM) of
              (Just verifiedEmail, Just candidateUser)
                | candidateUser.email == CI.mk verifiedEmail -> do
                    created :: [PG.Only Projects.UserId] <- PG.query connection "INSERT INTO users.users (id, created_at, updated_at, deleted_at, active, first_name, last_name, display_image_url, email, phone_number, is_sudo) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT (email) DO NOTHING RETURNING id" candidateUser
                    case created of
                      [PG.Only (_ :: Projects.UserId)] -> do
                        inserted <- insertIdentity connection settings.issuer identityClaims.subject candidateUser
                        if inserted then pure candidateUser else Safe.throwIO IdentityAbort
                      _ -> Safe.throwIO IdentityAbort
              _ -> Safe.throwIO IdentityAbort
  case outcome of
    Right user -> pure $ Right user
    Left (_ :: SomeException) -> do
      recovery <- Safe.try $ Pool.withResource pool \connection -> selectIdentityUser connection settings.issuer identityClaims.subject
      pure case recovery of
        Right (Just canonical) -> Right canonical
        Right Nothing -> Left IdentityResolutionFailure
        Left (_ :: SomeException) -> Left IdentityResolutionFailure
  where
    insertIdentity connection issuer subject user = do
      inserted :: [PG.Only Projects.UserId] <- PG.query connection "INSERT INTO users.oidc_identities (issuer, subject, user_id) VALUES (?, ?, ?) ON CONFLICT DO NOTHING RETURNING user_id" (issuer, subject, user.id)
      pure case inserted of
        [PG.Only (_ :: Projects.UserId)] -> True
        _ -> False

    linkExistingIdentity connection issuer subject user = do
      inserted <- insertIdentity connection issuer subject user
      if inserted then pure (Just user) else selectIdentityUser connection issuer subject
