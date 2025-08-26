module Web.ClientMetadataSpec (spec) where

import Data.Aeson.QQ (aesonQQ)
import Data.Base64.Types qualified as B64
import Data.ByteString.Base64 qualified as B64
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Data.Vector qualified as V
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec
import Web.ClientMetadata (ClientMetadata (..))
import System.Config (AuthContext(..), EnvConfig(..))


testId :: Projects.ProjectId
testId = Unsafe.fromJust $ Projects.ProjectId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Get clientMetaData" do
    it "creates and validates API keys correctly" $ \TestResources{..} -> do
      -- Test API key creation and encryption
      apiKey <- createAndSaveApiKey trPool trATCtx
      
      -- Verify the key was saved correctly
      savedKeys <- withPool trPool $ ProjectApiKeys.projectApiKeysByProjectId testId
      length savedKeys `shouldBe` 2
      
      let matchingKey = V.find (\k -> k.keyPrefix == apiKey) savedKeys
      isJust matchingKey `shouldBe` True
      let savedKey = Unsafe.fromJust matchingKey
      savedKey.keyPrefix `shouldBe` apiKey
      savedKey.active `shouldBe` True
      savedKey.projectId `shouldBe` testId

    it "stores API key with correct encryption" $ \TestResources{..} -> do
      -- Create a known UUID for testing
      let testUUID = UUID.nil
      let keyId = ProjectApiKeys.ProjectApiKeyId testUUID
      
      -- Encrypt the key
      let encryptedKey = ProjectApiKeys.encryptAPIKey 
            (encodeUtf8 trATCtx.config.apiKeyEncryptionSecretKey) 
            (encodeUtf8 $ UUID.toText testUUID)
      let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 encryptedKey
      
      -- Save it
      currentTime <- getCurrentTime
      let pApiKey = ProjectApiKeys.ProjectApiKey
            { keyPrefix = encryptedKeyB64
            , active = True
            , title = "Test Key"
            , projectId = testId
            , deletedAt = Nothing
            , createdAt = currentTime
            , updatedAt = currentTime
            , id = keyId
            }
      _ <- withPool trPool $ ProjectApiKeys.insertProjectApiKey pApiKey
      
      -- Verify decryption works
      let decryptedKey = ProjectApiKeys.decryptAPIKey 
            (encodeUtf8 trATCtx.config.apiKeyEncryptionSecretKey) 
            encryptedKey
      UUID.fromASCIIBytes decryptedKey `shouldBe` Just testUUID


createAndSaveApiKey :: Pool Connection -> AuthContext -> IO Text
createAndSaveApiKey pool authCtx = do
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let title = "Test API Key"
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 authCtx.config.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 encryptedKey
  let keyId = ProjectApiKeys.ProjectApiKeyId projectKeyUUID

  currentTime <- liftIO getCurrentTime
  let pApiKey =
        ProjectApiKeys.ProjectApiKey
          { keyPrefix = encryptedKeyB64
          , active = True
          , title = title
          , projectId = testId
          , deletedAt = Nothing
          , createdAt = currentTime
          , updatedAt = currentTime
          , id = keyId
          }
  v <- liftIO $ withPool pool $ ProjectApiKeys.insertProjectApiKey pApiKey
  pure encryptedKeyB64
