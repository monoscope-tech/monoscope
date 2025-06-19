module Web.ClientMetadata (ClientMetadata (..), clientMetadataH) where

import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Base64 qualified as B64
import Data.Text qualified as T

import Data.UUID qualified as UUID
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Log qualified
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Relude hiding (ask, asks, max, min)
import Relude.Unsafe ((!!))
import Servant (err401)
import System.Config (
  AuthContext (config),
  EnvConfig (apiKeyEncryptionSecretKey, requestPubsubTopics, apitoolkitPusherServiceAccountB64),
 )
import System.Types (ATBaseCtx)


data ClientMetadata = ClientMetadata
  { projectId :: Projects.ProjectId
  , topicId :: Text
  , pubsubProjectId :: Text
  , pubsubPushServiceAccount :: AE.Value
  }
  deriving stock (Generic, Show)
  deriving
    (ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ClientMetadata


clientMetadataH :: Maybe Text -> ATBaseCtx ClientMetadata
clientMetadataH Nothing = throwError err401
clientMetadataH (Just authTextB64) = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext

  let authTextE = B64.decodeBase64Untyped (encodeUtf8 $ T.replace "Bearer " "" authTextB64)
  case authTextE of
    Left err -> Log.logAttention "Auth Error in clientMetadata" (toString err) >> throwError err401
    Right authText -> do
      let decryptedKey = ProjectApiKeys.decryptAPIKey (encodeUtf8 appCtx.config.apiKeyEncryptionSecretKey) authText
      case ProjectApiKeys.ProjectApiKeyId <$> UUID.fromASCIIBytes decryptedKey of
        Nothing -> throwError err401
        Just apiKeyUUID -> do
          (pApiKey, project) <- dbtToEff do
            pApiKeyM <- ProjectApiKeys.getProjectApiKey apiKeyUUID
            case pApiKeyM of
              Nothing -> error "no api key with given id"
              Just pApiKey -> do
                project <- Projects.projectById pApiKey.projectId
                pure (pApiKey, project)

          let serviceAccountJson = case AE.decodeStrict . B64.decodeBase64Lenient . encodeUtf8 $ appCtx.config.apitoolkitPusherServiceAccountB64 of
                Just val -> val
                Nothing -> error "Failed to decode service account from environment variable"
          
          pure
            $ ClientMetadata
              { projectId = pApiKey.projectId
              , pubsubProjectId = "past-3"
              , topicId = appCtx.config.requestPubsubTopics !! 0 -- apitoolkit-prod-default
              , pubsubPushServiceAccount = serviceAccountJson
              }

