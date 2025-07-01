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
import Effectful
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant.Server qualified as ServantS
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (effToServantHandlerTest)
import Test.Hspec
import Web.ClientMetadata (ClientMetadata (..), clientMetadataH)


testId :: Projects.ProjectId
testId = Unsafe.fromJust $ Projects.ProjectId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Get clientMetaData" do
    it "returns client metadata for a valid API key" $ \TestResources{..} -> do
      apiKey <- createAndSaveApiKey trPool trATCtx

      response <-
        clientMetadataH (Just apiKey)
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
      response.projectId `shouldBe` expectedClientMetadata.projectId
      response.topicId `shouldBe` expectedClientMetadata.topicId
      response.pubsubProjectId `shouldBe` expectedClientMetadata.pubsubProjectId

    it "returns 401 for an invalid API key" $ \TestResources{..} -> do
      let invalidApiKey = Just "invalid-api-key"
      response <-
        clientMetadataH invalidApiKey
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler

      response `shouldSatisfy` isLeft
  where
    expectedClientMetadata =
      ClientMetadata
        { projectId = testId
        , topicId = "apitoolkit-prod-default"
        , pubsubProjectId = "past-3"
        , pubsubPushServiceAccount = [aesonQQ|{}|]
        }


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
