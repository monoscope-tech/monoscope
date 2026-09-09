{-# LANGUAGE PackageImports #-}

module Pages.Bots.BotTestHelpers (
  -- * Setup Helpers
  setupSlackData,
  setupLinkedSlackData,
  withHTTPResponses,
  withSlackReplyResponses,
  receiveSlackEvent,
  slackRootEvent,
  setupDiscordData,
  setupWhatsappNumber,

  -- * Test Project
  testPid,

  -- * Config Helpers
  getOpenAIKey,
  getOpenAIModel,

  -- * Golden File Helpers
  assertJsonGolden,
  writeGoldenFile,
  readGoldenFile,

  -- * Discord Signature Helpers
  testDiscordPublicKeyHex,
  testDiscordSecretKey,
  signDiscordPayload,

  -- * Response Extraction
  extractSlackBlocks,
  extractResponseText,

  -- * Notification Helpers

  -- (Use runTestBackgroundWithNotifications from Pkg.TestUtils instead)

  -- * JSON Response Helpers
  isValidJsonResponse,
  isEmptyJsonObject,

  -- * Slack Response Helpers
  extractResponseType,
  hasSuccessBlock,
  getBlockType,

  -- * Discord Response Helpers
  getDiscordResponseType,
  hasComponentsV2Flag,
  hasContainerComponent,
  countTextComponents,
  isEmptyResponse,

  -- * WhatsApp Body Detection
  isDashboardCommand,
  isWidgetSelect,
  isDashboardPagination,
  isPrompt,
  hasRequiredTemplateVars,
) where

import Control.Lens (each, filtered, has, lengthOf, to, (.~), (^?))
import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Lens (key, _Array, _Number, _String)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Effectful.Wreq qualified as HTTP
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful (Eff, IOE, type (:>))
import Effectful.Dispatch.Dynamic (interpose, send)
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Integrations qualified as Slack
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Client (RequestBody (..), defaultRequest, requestBody)
import Network.Wreq qualified as Wreq
import Network.Wreq.Types qualified as WreqTypes
import Pages.Bots.BotFixtures (slackCallbackEnvelope)
import Pages.Bots.Slack qualified as SlackEvents
import Pkg.DeriveUtils (UUIDId)
import Pkg.TestUtils
import Relude
import Servant.API.ResponseHeaders (getResponse)
import System.Config (AuthContext (..))
import System.Config qualified as Config
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Test.Hspec (expectationFailure, shouldBe)
import "cryptonite" Crypto.Error qualified as Crypto
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC
import "cryptonite" Crypto.PubKey.Ed25519 qualified as Ed25519


-- * Config Helpers


getOpenAIKey :: TestResources -> Text
getOpenAIKey tr = tr.trATCtx.env.openaiApiKey


getOpenAIModel :: TestResources -> Text
getOpenAIModel tr = tr.trATCtx.env.openaiModel


-- * Setup Helpers


setupSlackData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupSlackData tr pid teamId = void $ runTestBg frozenTime tr $ do
  _ <- Slack.insertAccessToken pid teamId "C_NOTIF_CHANNEL" ("Test Workspace " <> teamId) "x-bot-token" "test-channel" "https://hooks.slack.com/services/test" Nothing
  ProjectMembers.addSlackChannelToEveryoneTeam pid "C_NOTIF_CHANNEL"


-- | A linked command requester; unlinked event tests use setupSlackData instead.
setupLinkedSlackData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupLinkedSlackData tr pid teamId = do
  setupSlackData tr pid teamId
  withResource tr.trPool \conn -> void $ PGS.execute conn [sql|UPDATE apis.slack SET scopes = ARRAY['assistant:write', 'chat:write'] WHERE project_id = ?|] (PGS.Only pid)
  withResource tr.trPool \conn ->
    void
      $ PGS.execute
        conn
        [sql|INSERT INTO apis.slack_identities (team_id, slack_user_id, user_id, project_id)
      VALUES (?, 'U0123ABCDEF', ?, ?)|]
        (teamId, (getResponse tr.trSessAndHeader).user.id, pid)


-- | Override selected HTTP responses while retaining request recording.
withHTTPResponses :: (HTTP.HTTP :> es, IOE :> es) => (HTTP.Options -> String -> IO (Maybe LByteString)) -> Eff es a -> Eff es a
withHTTPResponses fixture = interpose @HTTP.HTTP \_ -> \case
  HTTP.GetWith opts url -> send (HTTP.GetWith opts url) >>= respond opts url
  HTTP.Get url -> send (HTTP.Get url) >>= respond Wreq.defaults url
  HTTP.Post url value -> send (HTTP.Post url value) >>= respond Wreq.defaults url
  HTTP.Put url value -> send (HTTP.Put url value) >>= respond Wreq.defaults url
  HTTP.Patch url value -> send (HTTP.Patch url value) >>= respond Wreq.defaults url
  HTTP.Delete url -> send (HTTP.Delete url) >>= respond Wreq.defaults url
  HTTP.PostWith opts url value -> send (HTTP.PostWith opts url value) >>= respond opts url
  HTTP.PutWith opts url value -> send (HTTP.PutWith opts url value) >>= respond opts url
  HTTP.PatchWith opts url value -> send (HTTP.PatchWith opts url value) >>= respond opts url
  HTTP.DeleteWith opts url -> send (HTTP.DeleteWith opts url) >>= respond Wreq.defaults url
  where
    respond opts url response = do
      body <- liftIO $ fixture opts url
      pure $ maybe response (\value -> response & Wreq.responseBody .~ value) body


-- | Fault only answer posts, allowing asynchronous progress to publish normally.
-- Retain the underlying recorder so assertions inspect actual request payloads.
withSlackReplyResponses :: (HTTP.HTTP :> es, IOE :> es) => IO LByteString -> Eff es a -> Eff es a
withSlackReplyResponses fixture =
  withHTTPResponses (\_ _ -> pure $ Just "{\"ok\":true,\"messages\":[],\"ts\":\"1735689609.000001\"}")
    . interpose @HTTP.HTTP
      ( \_ -> \case
          HTTP.PostWith opts "https://slack.com/api/chat.postMessage" value -> do
            request <- liftIO $ WreqTypes.postPayload value defaultRequest
            body <- case requestBody request of
              RequestBodyLBS bytes -> pure bytes
              RequestBodyBS bytes -> pure $ fromStrict bytes
              _ -> liftIO $ fail "Expected a JSON Slack message body"
            response <- send $ HTTP.PostWith opts "https://slack.com/api/chat.postMessage" value
            if (AE.decode @AE.Value body >>= (^? key "metadata" . key "event_type" . _String)) == Just "monoscope_investigation_reply"
              then liftIO fixture <&> \reply -> response & Wreq.responseBody .~ reply
              else pure response
          operation -> send @HTTP.HTTP $ coerce operation
      )


setupDiscordData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupDiscordData tr pid guildId = void
  $ withResource tr.trPool \conn ->
    PGS.execute conn [sql|INSERT INTO apis.discord (project_id, guild_id) VALUES (?,?) ON CONFLICT (project_id) DO UPDATE SET guild_id = EXCLUDED.guild_id|] (pid, guildId)


setupWhatsappNumber :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupWhatsappNumber tr pid phoneNumber = void
  $ withResource tr.trPool \conn ->
    PGS.execute
      conn
      [sql|UPDATE projects.teams SET phone_numbers = ARRAY[?]
           WHERE project_id = ? AND is_everyone = TRUE AND deleted_at IS NULL|]
      (phoneNumber, pid)


-- * Golden File Helpers


goldenDir :: FilePath
goldenDir = "./tests/golden/bots/"


assertJsonGolden :: FilePath -> AE.Value -> IO ()
assertJsonGolden path actual = do
  updateGolden <- isJust <$> lookupEnv "UPDATE_GOLDEN"
  let fullPath = goldenDir <> path
  exists <- doesFileExist fullPath
  if not exists || updateGolden
    then writeGoldenFile fullPath actual
    else do
      expectedM <- readGoldenFile fullPath
      case expectedM of
        Nothing -> expectationFailure $ "Could not read golden file: " <> fullPath
        Just expected -> actual `shouldBe` expected


writeGoldenFile :: FilePath -> AE.Value -> IO ()
writeGoldenFile path val = do
  let dir = reverse $ dropWhile (/= '/') $ reverse path
  createDirectoryIfMissing True dir
  writeFileLBS path (encodePretty val)


readGoldenFile :: FilePath -> IO (Maybe AE.Value)
readGoldenFile path = do
  exists <- doesFileExist path
  if exists
    then AE.decode <$> readFileLBS path
    else pure Nothing


-- * Discord Signature Helpers


-- Deterministic test keypair from a fixed 32-byte seed

-- | Test secret key derived from deterministic seed (32 bytes of 0x42)
testDiscordSecretKey :: Ed25519.SecretKey
testDiscordSecretKey = case Ed25519.secretKey (BS.replicate 32 0x42) of
  Crypto.CryptoPassed sk -> sk
  Crypto.CryptoFailed err -> error $ "Failed to create test secret key: " <> show err
{-# NOINLINE testDiscordSecretKey #-}


-- | Corresponding public key derived from the secret key
testDiscordPublicKey :: Ed25519.PublicKey
testDiscordPublicKey = Ed25519.toPublic testDiscordSecretKey
{-# NOINLINE testDiscordPublicKey #-}


-- | Hex-encoded public key for use in config/tests
testDiscordPublicKeyHex :: Text
testDiscordPublicKeyHex = decodeUtf8 $ Base16.encode (BA.convert testDiscordPublicKey :: BS.ByteString)
{-# NOINLINE testDiscordPublicKeyHex #-}


-- | Sign a Discord payload with the test secret key
signDiscordPayload :: BS.ByteString -> Text -> (BS.ByteString, BS.ByteString, BS.ByteString)
signDiscordPayload payload timestamp =
  let ts = encodeUtf8 timestamp
      message = ts <> payload
      signature = Ed25519.sign testDiscordSecretKey testDiscordPublicKey message
      sigHex = Base16.encode (BA.convert signature :: BS.ByteString)
   in (payload, sigHex, ts)


-- * Response Extraction


extractSlackBlocks :: AE.Value -> Maybe AE.Value
extractSlackBlocks val = case val of
  AE.Object obj -> AEKM.lookup (AEK.fromText "blocks") obj
  _ -> Nothing


extractResponseText :: AE.Value -> Maybe Text
extractResponseText val = case val of
  AE.Object obj ->
    case AEKM.lookup (AEK.fromText "text") obj of
      Just (AE.String t) -> Just t
      _ ->
        case AEKM.lookup (AEK.fromText "content") obj of
          Just (AE.String t) -> Just t
          _ -> Nothing
  _ -> Nothing


-- * Notification Helpers


-- Note: Use runTestBackgroundWithNotifications from Pkg.TestUtils to capture notifications

-- * JSON Response Helpers


isValidJsonResponse :: AE.Value -> Bool
isValidJsonResponse (AE.Object _) = True
isValidJsonResponse _ = False


isEmptyJsonObject :: AE.Value -> Bool
isEmptyJsonObject (AE.Object obj) = AEKM.null obj
isEmptyJsonObject _ = False


-- * Slack Response Helpers


extractResponseType :: AE.Value -> Maybe Text
extractResponseType val = case val of
  AE.Object obj ->
    case AEKM.lookup "response_type" obj of
      Just (AE.String t) -> Just t
      _ -> Nothing
  _ -> Nothing


hasSuccessBlock :: AE.Value -> Bool
hasSuccessBlock val = case extractSlackBlocks val of
  Just (AE.Array blocks) -> V.any hasSuccessEmoji blocks
  _ -> False
  where
    hasSuccessEmoji block =
      let blockStr = decodeUtf8 $ toStrict $ AE.encode block
       in T.isInfixOf "🟢" blockStr || T.isInfixOf "success" blockStr


getBlockType :: AE.Value -> Maybe Text
getBlockType val = case val of
  AE.Object obj ->
    case AEKM.lookup "type" obj of
      Just (AE.String t) -> Just t
      _ -> Nothing
  _ -> Nothing


-- * Discord Response Helpers


getDiscordResponseType :: AE.Value -> Maybe Int
getDiscordResponseType val = val ^? key "type" . _Number . to round


isEmptyResponse :: AE.Value -> Bool
isEmptyResponse (AE.Object o) = null o
isEmptyResponse _ = False


hasComponentsV2Flag :: AE.Value -> Bool
hasComponentsV2Flag val = val ^? key "data" . key "flags" . _Number . to round == Just (32768 :: Int)


hasContainerComponent :: AE.Value -> Bool
hasContainerComponent val = has (key "data" . key "components" . _Array . each . filtered isContainer) val
  where
    isContainer v = v ^? key "type" . _Number . to round == Just (17 :: Int)


countTextComponents :: AE.Value -> Int
countTextComponents val = lengthOf (key "data" . key "components" . _Array . each . key "components" . _Array . each . filtered isText) val
  where
    isText v = v ^? key "type" . _Number . to round == Just (10 :: Int)


-- * WhatsApp Body Detection


isDashboardCommand :: Text -> Bool
isDashboardCommand = (== "/dashboard")


isWidgetSelect :: Text -> Bool
isWidgetSelect body = "widg___" `T.isPrefixOf` body


isDashboardPagination :: Text -> Bool
isDashboardPagination body = "dashboard___" `T.isPrefixOf` body


isPrompt :: Text -> Bool
isPrompt body =
  not (isDashboardCommand body)
    && not (isWidgetSelect body)
    && not (isDashboardPagination body)
    && not ("dash___" `T.isPrefixOf` body)


hasRequiredTemplateVars :: AE.Value -> Bool
hasRequiredTemplateVars val = case val of
  AE.Object obj -> AEKM.member "1" obj
  _ -> False


receiveSlackEvent :: TestResources -> AE.Value -> IO (UUIDId "slack_event")
receiveSlackEvent tr payload = do
  now <- getTestTime tr.trTestClock
  let timestamp = show @Text (floor (utcTimeToPOSIXSeconds now) :: Integer)
      body = toStrict $ AE.encode payload
      secret = "test-slack-signing-secret" :: ByteString
      signature = "v0=" <> decodeUtf8 (Base16.encode $ BA.convert (HMAC.hmac secret ("v0:" <> encodeUtf8 timestamp <> ":" <> body) :: HMAC.HMAC SHA256))
      cfg = tr.trATCtx.env{Config.slackSigningSecret = decodeUtf8 secret}
  void $ runAsBase tr{trATCtx = tr.trATCtx{Config.env = cfg}} $ SlackEvents.slackEventsPostH body (Just timestamp) (Just signature)
  [PGS.Only receipt] <- withResource tr.trPool \conn ->
    PGS.query conn [sql|SELECT id FROM apis.slack_events WHERE team_id = (?::jsonb)->>'team_id' AND event_id = (?::jsonb)->>'event_id'|] (Aeson payload, Aeson payload)
  pure receipt


slackRootEvent :: Text -> Text -> Text -> Incidents.SlackRootId -> Text -> Text -> AE.Value
slackRootEvent eventId workspace channel rootId timestamp authorApp =
  slackCallbackEnvelope workspace eventId
    $ AE.object
      [ "type" AE..= ("message" :: Text)
      , "subtype" AE..= ("bot_message" :: Text)
      , "bot_id" AE..= ("B_TEST" :: Text)
      , "app_id" AE..= authorApp
      , "text" AE..= ("Monitor alert" :: Text)
      , "channel" AE..= channel
      , "ts" AE..= timestamp
      , "metadata" AE..= AE.object ["event_type" AE..= ("monoscope_incident_root" :: Text), "event_payload" AE..= AE.object ["root_id" AE..= rootId]]
      ]
