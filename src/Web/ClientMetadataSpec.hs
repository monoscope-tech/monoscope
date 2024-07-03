module Web.ClientMetadataSpec (spec) where

import Data.Aeson.QQ (aesonQQ)
import Data.ByteString.Base64 qualified as B64
import Data.UUID.V4 qualified as UUIDV4

import Data.Default (def)
import Data.Effectful.UUID (runStaticUUID)
import Data.Effectful.Wreq (runHTTPGolden)
import Data.Time (getCurrentTime)

import Data.Cache (Cache (..), newCache)
import Data.Pool (Pool)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import System.Clock (TimeSpec (TimeSpec))

import Database.PostgreSQL.Entity.DBT (withPool)

import Effectful

import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Time (runTime)
import Log qualified
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions

import Pkg.TmpPg qualified as TmpPg
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant qualified
import Servant.Server qualified as ServantS
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (effToServantHandlerTest)
import Test.Hspec
import Web.Auth qualified as Auth
import Web.ClientMetadata (ClientMetadata (..), clientMetadataH)
import Web.Cookie (SetCookie)


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


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


data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  }


withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = TmpPg.withSetup $ \pool -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  sessAndHeader <- testSessionHeader pool
  let atAuthCtx =
        AuthContext (def @EnvConfig) pool pool projectCache
          $ ( (def :: EnvConfig)
                { apiKeyEncryptionSecretKey = "apitoolkit123456123456apitoolkit"
                , convertkitApiKey = ""
                , convertkitApiSecret = ""
                , requestPubsubTopics = ["apitoolkit-prod-default"]
                }
            )
  f
    TestResources
      { trPool = pool
      , trSessAndHeader = sessAndHeader
      , trProjectCache = projectCache
      , trATCtx = atAuthCtx
      , trLogger = logger
      }


testSessionHeader :: MonadIO m => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session)
testSessionHeader pool = do
  pSessId <-
    Auth.authorizeUserAndPersist Nothing "firstName" "lastName" "https://placehold.it/500x500" "test@apitoolkit.io"
      & (runStaticUUID $ map (UUID.fromWords 0 0 0) [1 .. 10])
      & runHTTPGolden "./golden/"
      & DB.runDB pool
      & runTime
      & runEff
      & liftIO
  Auth.sessionByID (Just pSessId) "requestID" False
    & runErrorNoCallStack @Servant.ServerError
    & DB.runDB pool
    & runEff
    & liftIO
    <&> fromRightShow


createAndSaveApiKey :: Pool Connection -> AuthContext -> IO (Text)
createAndSaveApiKey pool authCtx = do
  projectKeyUUID <- liftIO UUIDV4.nextRandom
  let title = "Test API Key"
  let encryptedKey = ProjectApiKeys.encryptAPIKey (encodeUtf8 authCtx.config.apiKeyEncryptionSecretKey) (encodeUtf8 $ UUID.toText projectKeyUUID)
  let encryptedKeyB64 = B64.encodeBase64 encryptedKey
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
