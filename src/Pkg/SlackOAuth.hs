module Pkg.SlackOAuth (SlackUserGrant (..), SlackTokenLifetime (..), SlackTokenRequest (..), SlackUserCode (..), SlackTokenRefresh (..), SlackOAuthFailure (..), slackEvidenceScopes, exchangeUserToken) where

import Control.Lens ((.~), (^.))
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AE
import Data.Effectful.Wreq qualified as HTTP
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful
import Effectful.Time qualified as Time
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Relude


-- Credentials deliberately have no Show or JSON encoder.
data SlackUserGrant = SlackUserGrant
  { teamId :: Text
  , slackUserId :: Text
  , accessToken :: Text
  , lifetime :: SlackTokenLifetime
  , scopes :: V.Vector Text
  }


data SlackTokenLifetime = NonExpiring | Rotating Text UTCTime


data SlackUserCode = SlackUserCode {code :: Text, redirectUri :: Text, codeVerifier :: Text}


data SlackTokenRefresh = SlackTokenRefresh {teamId :: Text, slackUserId :: Text, scopes :: V.Vector Text, refreshToken :: Text}


data SlackTokenRequest = ExchangeUserCode SlackUserCode | RefreshUserToken SlackTokenRefresh


data SlackOAuthFailure = OAuthRevoked | OAuthRejected | OAuthMalformed
  deriving stock (Eq, Show)


slackEvidenceScopes :: [Text]
slackEvidenceScopes = ["search:read.public", "search:read.private", "search:read.im", "search:read.mpim", "channels:history", "groups:history", "im:history", "mpim:history"]


exchangeUserToken :: (HTTP.HTTP :> es, Time.Time :> es) => Text -> Text -> SlackTokenRequest -> Eff es (Either SlackOAuthFailure SlackUserGrant)
exchangeUserToken clientId clientSecret request = do
  let form :: [FormParam]
      form =
        ["client_id" Wreq.:= clientId, "client_secret" Wreq.:= clientSecret] <> case request of
          ExchangeUserCode (SlackUserCode code redirect verifier) -> ["grant_type" Wreq.:= ("authorization_code" :: Text), "code" Wreq.:= code, "redirect_uri" Wreq.:= redirect, "code_verifier" Wreq.:= verifier]
          RefreshUserToken (SlackTokenRefresh _ _ _ refresh) -> ["grant_type" Wreq.:= ("refresh_token" :: Text), "refresh_token" Wreq.:= refresh]
  response <- HTTP.postWith (HTTP.defaults & Wreq.header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=utf-8"]) "https://slack.com/api/oauth.v2.user.access" form
  now <- Time.currentTime
  pure $ case AE.eitherDecode (response ^. HTTP.responseBody) of
    Left _ -> Left OAuthMalformed
    Right value -> case AE.parseMaybe (AE.withObject "Slack OAuth error" (AE..:? "error")) value of
      Just (Just failure) -> Left $ if failure `elem` (["invalid_refresh_token", "invalid_auth", "token_revoked", "account_inactive"] :: [Text]) then OAuthRevoked else OAuthRejected
      _ -> first (const OAuthMalformed) $ AE.parseEither (parseGrant now) value
  where
    parseGrant :: UTCTime -> AE.Value -> AE.Parser SlackUserGrant
    parseGrant now = AE.withObject "Slack user authorization" \response -> do
      ok <- response AE..: "ok"
      tokenType <- response AE..: "token_type"
      rootUser <- response AE..:? "user_id"
      unless (ok && (tokenType == ("user" :: Text) || tokenType == "Bearer" && isJust rootUser)) $ fail "User authorization required"
      team <- response AE..:? "team"
      user <- response AE..:? "authed_user"
      let required value fallback = maybe (maybe (fail "Missing OAuth subject") pure fallback) (AE..: "id") value
          (expectedTeam, expectedUser, expectedScopes) = case request of
            ExchangeUserCode{} -> (Nothing, Nothing, Nothing)
            RefreshUserToken (SlackTokenRefresh teamId slackUserId scopes _) -> (Just teamId, Just slackUserId, Just scopes)
      teamId <- required team expectedTeam
      slackUserId <- required user (rootUser <|> expectedUser)
      unless (not (T.null teamId) && not (T.null slackUserId) && all (== teamId) expectedTeam && all (== slackUserId) expectedUser && all (== slackUserId) rootUser) $ fail "Slack OAuth identity changed"
      nestedScope <- traverse (AE..:? "scope") user
      rootScope <- response AE..:? "scope"
      let decodeScope = V.fromList . filter (not . T.null) . map T.strip . T.splitOn ","
      for_ ((,) <$> join nestedScope <*> rootScope) $ \(nested, root) ->
        unless (S.fromList (toList $ decodeScope nested) == S.fromList (toList $ decodeScope root)) $ fail "Conflicting OAuth scopes"
      scopes <- maybe (maybe (fail "Missing read scopes") pure expectedScopes) (pure . decodeScope) (join nestedScope <|> rootScope)
      unless (any (T.isPrefixOf "search:read.") scopes && all (`elem` slackEvidenceScopes) scopes && maybe True (\allowed -> all (`elem` allowed) scopes) expectedScopes) $ fail "Read-only search scopes required"
      accessToken <- response AE..: "access_token"
      when (T.null accessToken) $ fail "Missing user token"
      refresh <- response AE..:? "refresh_token"
      expires <- response AE..:? "expires_in"
      lifetime <- case (request, refresh, expires) of
        (ExchangeUserCode{}, Nothing, Nothing) -> pure NonExpiring
        (_, Just token, Just (seconds :: Int)) | not (T.null token) && seconds > 0 -> pure $ Rotating token $ addUTCTime (fromIntegral seconds) now
        _ -> fail "Incomplete token rotation credentials"
      pure SlackUserGrant{teamId, slackUserId, accessToken, lifetime, scopes}
