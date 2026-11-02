{-# LANGUAGE GADTs #-}

module Pkg.SlackMCP (SlackRead (..), ReadResult (..), ReadOutcome (..), Failure (..), discoverReads, readEvidence) where

import Control.Lens ((.~), (?~), (^.))
import Control.Monad.Except qualified as Except
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AE
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as HTTP
import Data.Map.Strict qualified as Map
import Data.Set qualified as S
import Data.Text qualified as T
import Data.UUID (UUID)
import Effectful
import Effectful.Exception qualified as Exception
import Network.HTTP.Client (HttpException)
import Network.HTTP.Types (statusCode)
import Network.Wreq qualified as Wreq
import Pkg.AgentCatalog (SlackRead (..))
import Pkg.AgentCatalog qualified as Catalog
import Pkg.SlackOAuth (SlackUserGrant (..))
import Relude


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
  response <- request (Session grant.accessToken Nothing) (body identifier $ Initialize app)
  let rawId = response ^. Wreq.responseHeader "Mcp-Session-Id"
  sessionId <-
    if BS.null rawId
      then pure Nothing
      else
        if BS.length rawId <= 1024 && BS.all (\c -> c >= 33 && c <= 126) rawId then pure (Just rawId) else Except.throwError InvalidProtocol
  let session = Session grant.accessToken sessionId
  ExceptT $ Exception.bracket (pure session) close \ready -> runExceptT do
    decodeReply identifier (Initialize app) response
    accepted <- request ready $ AE.object ["jsonrpc" AE..= ("2.0" :: Text), "method" AE..= ("notifications/initialized" :: Text)]
    unless (statusCode (accepted ^. Wreq.responseStatus) == 202 && LBS.null (accepted ^. HTTP.responseBody)) $ Except.throwError InvalidProtocol
    action ready
  where
    close session = for_ session.sessionId \_ -> void $ Exception.try @HttpException $ HTTP.deleteWith (options session) endpoint


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
  response <- request session $ body identifier operation
  decodeReply identifier operation response


body :: UUID -> Request a -> AE.Value
body identifier requestValue = AE.object ["jsonrpc" AE..= ("2.0" :: Text), "id" AE..= identifier, "method" AE..= (method :: Text), "params" AE..= parameters]
  where
    (method, parameters) = case requestValue of
      Initialize app -> ("initialize", AE.object ["protocolVersion" AE..= protocol, "capabilities" AE..= AE.object [], "clientInfo" AE..= AE.object ["name" AE..= ("monoscope/" <> app), "version" AE..= ("1" :: Text)]])
      ListTools cursor -> ("tools/list", AE.object ["cursor" AE..= value | value <- maybeToList cursor])
      ReadTool operation arguments -> ("tools/call", AE.object ["name" AE..= Catalog.slackReadName operation, "arguments" AE..= arguments])


request :: HTTP.HTTP :> es => Session -> AE.Value -> ExceptT Failure (Eff es) (HTTP.Response LByteString)
request session payload = do
  response <- ExceptT $ first (const TransportFailure) <$> Exception.try @HttpException (HTTP.postWith (options session) endpoint payload)
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


options :: Session -> HTTP.Options
options session =
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
