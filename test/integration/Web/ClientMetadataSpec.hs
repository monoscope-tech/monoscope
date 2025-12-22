
{-# LANGUAGE PackageImports #-}

module Web.ClientMetadataSpec (spec) where

import Data.Aeson.QQ (aesonQQ)
import Data.Base64.Types qualified as B64
import "base64" Data.ByteString.Base64 qualified as B64
import Data.Pool (Pool, withResource)
import Data.Time (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant.Server qualified as ServantS
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (effToServantHandlerTest)
import Test.Hspec
import Web.ClientMetadata (ClientMetadata (..), clientMetadataH)


testId :: Projects.ProjectId
testId = Unsafe.fromJust $ UUIDId <$> UUID.fromText "00000000-0000-0000-0000-000000000000"


spec :: Spec
spec = aroundAll withTestResources do
  describe "Get clientMetaData" do
    it "returns client metadata for a valid API key" $ \TestResources{..} -> do
      apiKey <- createAndSaveApiKey trPool trATCtx

      response <-
        clientMetadataH (Just apiKey)
          & effToServantHandlerTest trATCtx trLogger trTracerProvider
          & ServantS.runHandler
          <&> fromRightShow
      response.projectId `shouldBe` expectedClientMetadata.projectId
      response.topicId `shouldBe` expectedClientMetadata.topicId
      response.pubsubProjectId `shouldBe` expectedClientMetadata.pubsubProjectId

    it "returns 401 for an invalid API key" $ \TestResources{..} -> do
      let invalidApiKey = Just "invalid-api-key"
      response <-
        clientMetadataH invalidApiKey
          & effToServantHandlerTest trATCtx trLogger trTracerProvider
          & ServantS.runHandler

      response `shouldSatisfy` isLeft
  where
    expectedClientMetadata =
      ClientMetadata
        { projectId = testId
        , topicId = "monoscope-prod-default"
        , pubsubProjectId = "past-3"
        , pubsubPushServiceAccount = [aesonQQ|{}|]
        }


createAndSaveApiKey :: Pool Connection -> AuthContext -> IO Text
createAndSaveApiKey pool authCtx = do
  projectKeyUUID <- UUIDV4.nextRandom
  let title = "Test API Key" :: Text
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 authCtx.config.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.extractBase64 $ B64.encodeBase64 encryptedKey
  let keyId = ProjectApiKeys.ProjectApiKeyId projectKeyUUID

  currentTime <- getCurrentTime
  _ <- withResource pool \conn -> PGS.execute conn [sql|
    INSERT INTO projects.project_api_keys (id, key_prefix, active, title, project_id, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  |] (keyId, encryptedKeyB64, True :: Bool, title, testId, currentTime, currentTime)
  pure encryptedKeyB64
