{-# LANGUAGE PackageImports #-}

module Pages.Bots.BotTestHelpers (
  -- * Setup Helpers
  setupSlackData,
  setupDiscordData,
  setupWhatsappNumber,

  -- * Test Project
  testPid,

  -- * Config Helpers
  getOpenAIKey,
  hasOpenAIKey,

  -- * Golden File Helpers
  assertJsonGolden,
  writeGoldenFile,
  readGoldenFile,
  shouldMatchGolden,

  -- * Discord Signature Helpers
  testDiscordPublicKeyHex,
  testDiscordSecretKey,
  signDiscordPayload,

  -- * Response Extraction
  extractSlackBlocks,
  extractDiscordComponents,
  extractResponseText,

  -- * Notification Helpers
  captureNotifications,
  assertSlackNotification,
  assertDiscordNotification,
) where

import "cryptonite" Crypto.Error qualified as Crypto
import "cryptonite" Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Apis.Slack qualified as Slack
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import System.Config (AuthContext (..))
import System.Config qualified as Config
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Types (ATBackgroundCtx)
import Test.Hspec (Expectation, expectationFailure, pendingWith, shouldBe, shouldSatisfy)


testPid :: Projects.ProjectId
testPid = UUIDId UUID.nil


-- * Config Helpers


getOpenAIKey :: TestResources -> Text
getOpenAIKey tr = tr.trATCtx.env.openaiApiKey


hasOpenAIKey :: TestResources -> Bool
hasOpenAIKey tr = not $ T.null $ getOpenAIKey tr


-- * Setup Helpers


setupSlackData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupSlackData tr pid teamId = void $ runTestBg tr $ Slack.insertAccessToken pid ("https://hooks.slack.com/test/" <> teamId) teamId "C_NOTIF_CHANNEL"


setupDiscordData :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupDiscordData tr pid guildId = void $ runTestBg tr $ Slack.insertDiscordData pid guildId


setupWhatsappNumber :: TestResources -> Projects.ProjectId -> Text -> IO ()
setupWhatsappNumber tr pid phoneNumber = void
  $ withResource tr.trPool \conn ->
    PGS.execute conn [sql|UPDATE projects.projects SET whatsapp_numbers = ARRAY[?] WHERE id = ?|] (phoneNumber, pid)


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


shouldMatchGolden :: AE.Value -> FilePath -> Expectation
shouldMatchGolden actual path = assertJsonGolden path actual


-- * Discord Signature Helpers
-- Deterministic test keypair from a fixed 32-byte seed


-- | Test secret key derived from deterministic seed (32 bytes of 0x42)
testDiscordSecretKey :: Ed25519.SecretKey
testDiscordSecretKey = case Ed25519.secretKey (BS.replicate 32 0x42) of
  Crypto.CryptoPassed sk -> sk
  Crypto.CryptoFailed _ -> error "Failed to create test secret key"
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


extractDiscordComponents :: AE.Value -> Maybe AE.Value
extractDiscordComponents val = case val of
  AE.Object obj -> AEKM.lookup (AEK.fromText "components") obj
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


captureNotifications :: TestResources -> ATBackgroundCtx a -> IO ([Notification], a)
captureNotifications = runTestBgWithNotifyCapture


assertSlackNotification :: [Notification] -> Text -> Expectation
assertSlackNotification notifications expectedChannelId = do
  let slackNotifs = [n | SlackNotification n <- notifications]
  slackNotifs `shouldSatisfy` any (\n -> n.channelId == expectedChannelId)


assertDiscordNotification :: [Notification] -> Text -> Expectation
assertDiscordNotification notifications expectedChannelId = do
  let discordNotifs = [n | DiscordNotification n <- notifications]
  discordNotifs `shouldSatisfy` any (\n -> n.channelId == expectedChannelId)
