{-# LANGUAGE OrPatterns #-}

module Pkg.Slack (
  -- * OAuth
  SlackUserGrant (..),
  SlackTokenLifetime (..),
  SlackTokenRequest (..),
  SlackUserCode (..),
  SlackTokenRefresh (..),
  SlackOAuthFailure (..),
  slackEvidenceScopes,
  exchangeUserToken,

  -- * Rate limits
  SlackRateLimited (..),
  SlackHttpFailure,
  slackHttpStatus,
  withRateLimits,

  -- * MCP evidence reads
  SlackRead (..),
  ReadResult (..),
  ReadOutcome (..),
  Failure (..),
  discoverReads,
  readEvidence,
) where

import Control.Exception (ErrorCall (..))
import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Except qualified as Except
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as HTTP
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID (UUID)
import Data.Vector qualified as V
import Effectful
import Effectful.Dispatch.Dynamic (interpose, send)
import Effectful.Exception (throwIO)
import Effectful.Exception qualified as Exception
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types (Status, statusCode, statusIsSuccessful)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types (FormParam)
import Pkg.AgentCatalog (SlackRead (..))
import Pkg.AgentCatalog qualified as Catalog
import Relude
import System.Types (DB)


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


newtype SlackRateLimited = SlackRateLimited UTCTime
  deriving stock (Show)
  deriving anyclass (Exception)


newtype SlackHttpFailure = SlackHttpFailure Status
  deriving stock (Show)
  deriving anyclass (Exception)


slackHttpStatus :: SlackHttpFailure -> Status
slackHttpStatus (SlackHttpFailure status) = status


-- | Cooldowns are shared by workers and survive process restarts. Non-Slack
-- requests retain their original transport options and error handling.
withRateLimits :: (DB es, HTTP.HTTP :> es) => Text -> Eff es a -> Eff es a
withRateLimits workspace = interpose @HTTP.HTTP $ \_ request -> case request of
  HTTP.PostWith options url body -> limited workspace options url $ \opts -> send $ HTTP.PostWith opts url body
  HTTP.GetWith options url -> limited workspace options url $ \opts -> send $ HTTP.GetWith opts url
  (HTTP.Get{}; HTTP.Post{}; HTTP.Put{}; HTTP.Patch{}; HTTP.Delete{}; HTTP.PutWith{}; HTTP.PatchWith{}; HTTP.DeleteWith{}) -> send @HTTP.HTTP $ coerce request


limited :: DB es => Text -> HTTP.Options -> String -> (HTTP.Options -> Eff es (HTTP.Response LByteString)) -> Eff es (HTTP.Response LByteString)
limited workspace options url request = case if url == "https://mcp.slack.com/mcp" then Just "mcp" else T.stripPrefix "https://slack.com/api/" (toText url) of
  Nothing -> request options
  Just path -> do
    let method = T.takeWhile (/= '?') path
    deadline <-
      Hasql.interpOne @(HI.OneColumn UTCTime)
        [HI.sql|SELECT retry_at FROM apis.slack_api_cooldowns
        WHERE team_id = #{workspace} AND method = #{method} AND retry_at > clock_timestamp()|]
    for_ deadline $ \(HI.OneColumn retryAt) -> throwIO $ SlackRateLimited retryAt
    response <- request $ options & Wreq.checkResponse ?~ (\_ _ -> pass)
    if statusCode (response ^. Wreq.responseStatus) == 429
      then do
        let seconds = max 1 $ fromMaybe 60 $ readMaybe @Int $ toString $ decodeUtf8 @Text $ response ^. Wreq.responseHeader "Retry-After"
        saved <-
          Hasql.interpOne @(HI.OneColumn UTCTime)
            [HI.sql|INSERT INTO apis.slack_api_cooldowns (team_id, method, retry_at)
            VALUES (#{workspace}, #{method}, clock_timestamp() + #{seconds} * interval '1 second')
            ON CONFLICT (team_id, method) DO UPDATE
              SET retry_at = GREATEST(slack_api_cooldowns.retry_at, EXCLUDED.retry_at)
            RETURNING retry_at|]
        for_ saved $ \(HI.OneColumn retryAt) -> throwIO $ SlackRateLimited retryAt
        throwIO $ ErrorCall "Saving the Slack rate-limit deadline returned no row"
      else do
        unless (url == "https://mcp.slack.com/mcp" || statusIsSuccessful (response ^. Wreq.responseStatus)) $ throwIO $ SlackHttpFailure $ response ^. Wreq.responseStatus
        pure response


data Failure = ClientNotConfigured | CredentialsRejected | AccessRejected | InvalidProtocol | InvalidCatalog | HttpFailure Int | RpcFailure Int | TransportFailure
  deriving stock (Eq, Show)


data ReadOutcome = ReadSucceeded | ReadRejected
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON)


data ReadResult = ReadResult {content :: [Text], structuredContent :: Maybe AE.Value, outcome :: ReadOutcome}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


-- The constructor and credentials stay inside the initialized-session boundary.
data Session = Session {token :: Text, sessionId :: Maybe ByteString}


data Request a where
  Initialize :: Text -> Request ()
  ListTools :: Maybe Text -> Request ToolsPage
  ReadTool :: SlackRead -> Map Text AE.Value -> Request ReadResult


data RemoteTool = RemoteTool {name :: Text, description :: Maybe Text, inputSchema :: AE.Value, annotations :: Maybe AE.Object}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data ToolsPage = ToolsPage {tools :: [RemoteTool], nextCursor :: Maybe Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


type Client es = (HTTP.HTTP :> es, UUID.UUIDEff :> es)


discoverReads :: Client es => Text -> SlackUserGrant -> Eff es (Either Failure (Map Text (Catalog.Tool SlackRead)))
discoverReads app grant = withSession app grant readCatalog


readEvidence :: Client es => Text -> SlackUserGrant -> Catalog.Tool SlackRead -> Map Text AE.Value -> Eff es (Either Failure ReadResult)
readEvidence app grant tool arguments = withSession app grant \session -> do
  catalog <- readCatalog session
  case Map.lookup tool.name catalog of
    Just current | current.dispatch == tool.dispatch && current.inputSchema == tool.inputSchema -> rpc session $ ReadTool tool.dispatch arguments
    _ -> Except.throwError InvalidCatalog


withSession :: Client es => Text -> SlackUserGrant -> (Session -> ExceptT Failure (Eff es) a) -> Eff es (Either Failure a)
withSession app grant action = runExceptT do
  when (T.null (T.strip app)) $ Except.throwError ClientNotConfigured
  identifier <- lift UUID.genUUID
  response <- rpcPost (Session grant.accessToken Nothing) (rpcBody identifier $ Initialize app)
  let rawId = response ^. Wreq.responseHeader "Mcp-Session-Id"
  sessionId <-
    if BS.null rawId
      then pure Nothing
      else
        if BS.length rawId <= 1024 && BS.all (\c -> c >= 33 && c <= 126) rawId then pure (Just rawId) else Except.throwError InvalidProtocol
  let session = Session grant.accessToken sessionId
  ExceptT $ Exception.bracket (pure session) close \ready -> runExceptT do
    decodeReply identifier (Initialize app) response
    accepted <- rpcPost ready $ AE.object ["jsonrpc" AE..= ("2.0" :: Text), "method" AE..= ("notifications/initialized" :: Text)]
    unless (statusCode (accepted ^. Wreq.responseStatus) == 202 && LBS.null (accepted ^. HTTP.responseBody)) $ Except.throwError InvalidProtocol
    action ready
  where
    close session = for_ session.sessionId \_ -> void $ Exception.try @HttpException $ HTTP.deleteWith (mcpOptions session) endpoint


readCatalog :: Client es => Session -> ExceptT Failure (Eff es) (Map Text (Catalog.Tool SlackRead))
readCatalog session = go S.empty Nothing []
  where
    go seen cursor accumulated = do
      when (S.size seen >= 32) $ Except.throwError InvalidCatalog
      page <- rpc session $ ListTools cursor
      accepted <- Except.liftEither $ first (const InvalidCatalog) $ traverse parseRead page.tools
      let tools = accumulated <> catMaybes accepted
      case page.nextCursor >>= guarded (not . T.null) of
        Just next | next `S.notMember` seen -> go (S.insert next seen) (Just next) tools
        Just _ -> Except.throwError InvalidCatalog
        Nothing -> do
          let catalog = Map.fromList [(tool.name, tool) | tool <- tools]
          unless (Map.size catalog == length tools) $ Except.throwError InvalidCatalog
          pure catalog
    parseRead remote = case Map.lookup remote.name Catalog.slackReads of
      Nothing -> Right Nothing
      Just operation ->
        first toText
          $ AE.parseEither
            ( AE.withObject "MCP input schema" \schema -> do
                kind <- schema AE..: "type"
                unless (kind == ("object" :: Text)) $ fail "Object schema required"
                for_ remote.annotations \annotations -> do
                  readOnly <- annotations AE..:? "readOnlyHint"
                  when (readOnly == Just False) $ fail "Read contract changed"
                pure $ Just $ Catalog.Tool remote.name (fromMaybe "Read Slack evidence as the linked requester." remote.description) remote.inputSchema (Just $ Catalog.slackReadPolicy operation) operation
            )
            remote.inputSchema


rpc :: Client es => Session -> Request a -> ExceptT Failure (Eff es) a
rpc session operation = do
  identifier <- lift UUID.genUUID
  response <- rpcPost session $ rpcBody identifier operation
  decodeReply identifier operation response


rpcBody :: UUID -> Request a -> AE.Value
rpcBody identifier requestValue = AE.object ["jsonrpc" AE..= ("2.0" :: Text), "id" AE..= identifier, "method" AE..= (method :: Text), "params" AE..= parameters]
  where
    (method, parameters) = case requestValue of
      Initialize app -> ("initialize", AE.object ["protocolVersion" AE..= protocol, "capabilities" AE..= AE.object [], "clientInfo" AE..= AE.object ["name" AE..= ("monoscope/" <> app), "version" AE..= ("1" :: Text)]])
      ListTools cursor -> ("tools/list", AE.object ["cursor" AE..= value | value <- maybeToList cursor])
      ReadTool operation arguments -> ("tools/call", AE.object ["name" AE..= Catalog.slackReadName operation, "arguments" AE..= arguments])


rpcPost :: HTTP.HTTP :> es => Session -> AE.Value -> ExceptT Failure (Eff es) (HTTP.Response LByteString)
rpcPost session payload = do
  response <- ExceptT $ first (const TransportFailure) <$> Exception.try @HttpException (HTTP.postWith (mcpOptions session) endpoint payload)
  case statusCode (response ^. Wreq.responseStatus) of
    401 -> Except.throwError CredentialsRejected
    403 -> Except.throwError AccessRejected
    code | code >= 200 && code < 300 -> pure response
    code -> Except.throwError $ HttpFailure code


decodeReply :: Monad m => UUID -> Request a -> HTTP.Response LByteString -> ExceptT Failure m a
decodeReply identifier operation response = Except.liftEither $ do
  when (LBS.length (LBS.take 1_048_577 $ response ^. HTTP.responseBody) > 1_048_576) $ Left InvalidProtocol
  value <- first (const InvalidProtocol) $ AE.eitherDecode $ response ^. HTTP.responseBody
  join
    $ first (const InvalidProtocol)
    $ AE.parseEither
      ( AE.withObject "MCP response" \fields -> do
          version <- fields AE..: "jsonrpc"
          returned <- fields AE..: "id"
          unless (version == ("2.0" :: Text) && returned == identifier) $ fail "Response identity mismatch"
          failure <- fields AE..:? "error"
          case failure of
            Just errorValue | not (KM.member "result" fields) -> Left . RpcFailure <$> errorValue AE..: "code"
            Just _ -> fail "Ambiguous MCP response"
            Nothing -> Right <$> (fields AE..: "result" >>= parseResult operation)
      )
      value
  where
    parseResult :: Request a -> AE.Value -> AE.Parser a
    parseResult requestValue = case requestValue of
      Initialize _ -> AE.withObject "MCP initialization" \value -> do
        version <- value AE..: "protocolVersion"
        server <- value AE..: "serverInfo"
        _ <- server AE..: "name" :: AE.Parser Text
        _ <- server AE..: "version" :: AE.Parser Text
        capabilities <- value AE..: "capabilities"
        _ <- capabilities AE..: "tools" :: AE.Parser AE.Object
        unless (version == protocol) $ fail "Unsupported MCP version"
      ListTools _ -> AE.parseJSON
      ReadTool _ _ -> AE.withObject "MCP read result" \value -> do
        blocks <- value AE..: "content"
        content <-
          traverse
            ( AE.withObject "MCP text evidence" \block -> do
                kind <- block AE..: "type"
                unless (kind == ("text" :: Text)) $ fail "Unsupported evidence content"
                block AE..: "text"
            )
            blocks
        structuredContent <- value AE..:? "structuredContent"
        failed <- value AE..:? "isError" AE..!= False
        pure ReadResult{content, structuredContent, outcome = if failed then ReadRejected else ReadSucceeded}


mcpOptions :: Session -> HTTP.Options
mcpOptions session =
  HTTP.defaults
    & Wreq.header "Authorization"
    .~ [encodeUtf8 $ "Bearer " <> session.token]
      & Wreq.header "Content-Type"
    .~ ["application/json"]
      & Wreq.header "Accept"
    .~ ["application/json, text/event-stream"]
      & Wreq.header "MCP-Protocol-Version"
    .~ [encodeUtf8 protocol]
      & Wreq.header "Mcp-Session-Id"
    .~ maybeToList session.sessionId
      & Wreq.redirects
    .~ 0
      & Wreq.checkResponse
    ?~ (\_ _ -> pass)


endpoint :: String
endpoint = "https://mcp.slack.com/mcp"


protocol :: Text
protocol = "2025-06-18"
