module Web.ClientMetadata (ClientMetadata, clientMetadataH) where

import Colog ((<&))
import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (ToJSON)
import Data.ByteString.Base64 qualified as B64
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Deriving.Aeson qualified as DAE
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Prelude hiding (ask, asks, max, min)
import Relude.Unsafe ((!!))
import Servant (err401)
import System.Config
import System.Types


data ClientMetadata = ClientMetadata
  { projectId :: Projects.ProjectId
  , topicId :: Text
  , pubsubProjectId :: Text
  , pubsubPushServiceAccount :: Value
  }
  deriving stock (Show, Generic)
  deriving
    (ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] ClientMetadata


clientMetadataH :: Maybe Text -> ATBaseCtx ClientMetadata
clientMetadataH Nothing = throwError err401
clientMetadataH (Just authTextB64) = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext

  let authTextE = B64.decodeBase64 (encodeUtf8 $ T.replace "Bearer " "" authTextB64)
  case authTextE of
    Left err -> liftIO (appCtx.logger <& toString err) >> throwError err401
    Right authText -> do
      let decryptedKey = ProjectApiKeys.decryptAPIKey (encodeUtf8 $ appCtx.config.apiKeyEncryptionSecretKey) authText
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

          pure
            $ ClientMetadata
              { projectId = pApiKey.projectId
              , pubsubProjectId = "past-3"
              , topicId = (appCtx.config.requestPubsubTopics) !! 0 -- apitoolkit-prod-default
              , pubsubPushServiceAccount = apitoolkitPusherServiceAccount
              }


apitoolkitPusherServiceAccount :: Value
apitoolkitPusherServiceAccount =
  [aesonQQ|{
  "type": "service_account",
  "project_id": "past-3",
  "private_key_id": "e30a537b86196e7f82d0de17bc968f00ef780a0e",
  "private_key": "-----BEGIN PRIVATE KEY-----\nMIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQCyiI22PkbbS/Kn\nTp+rUue7aqBJnrGwLFiT4elZymhQGPIP9DuBWsb+ZLipFNbUKxly5qObvGxDo4qL\n9vONpuxj/imV3A/MwuOFkUHweLGJJ3exhGxQKAQtLnabe3P9hc+VVCQ7QMY579x1\n2acc2/9EF2V0cLWawEcop5K5PKFL6Ep0OjDT1p0wjVDlbzo/VjOQOynJrxYdPdDW\nWO05XnK3chxSwOK17DDtJwnUaQwMO4TGjM1byosaPH6thKKrHEEzzWYsuZSz07GP\nkHdb9OiTGXb+x2GAmoZfycW7PB9+XMGa3V3pF+MR/KNotNUKl5ZKLiRcCUPGqIKb\nyqBR3qp9AgMBAAECggEAAzEadNH44FN48MbLZ1EMdYxQIKDRWm313hxMLgpxV6U/\n2/XTg9ovST2xviFyPx7PeKoTmLrYpf5BgYPBEkeUCFbzQAuheK6vSfmxLRnAoCFw\nLAtLCG/UC0lpCMSUlp69eapPGi7BRyiOT3O8X3Fgx0MyfCTGX7rk9XQcKmr2ve2k\nlMYSQMRqw7CBDcCxy62kMDUh8YA4slYGBxbg8yr5bK+jtqMufH22RThcHfxvCfaz\nzQjPkZJg3OV1cT4a2Jms77XOQwHkzSjUXOXF+r66QTW/VzxqJdviQ4RfCj1dtooh\nUaLvEWcX86rNaSqJVYmJDqaIsioDTVqoKAxaejDR4QKBgQDX1v3bDREzKvkqQW1M\nN69i1q2qVWpaV6c0WiD4DFeKtK3ODK7XY+NIMKgMoEUrvhnUMrTvXjJSKGv4FQX4\nS5WkRhCnt6RiCTD2rpun5nya2F1oKvKgH53DMSboZ33DtEeOdueMldN9pAd2JEVW\nISHVmVdHsAG9iwe4YkI5njsUpQKBgQDTwI/LEbaWD6S9WBmgKkvg/8OmpPTLhvE0\nv+YWYGOi0q8+Q2YQWC6dXw/r3gG2jyc0GDBMLDfFvTu7PqXUPg4t7FZG+6PXndNE\nTA9+SPYnubbpirCMJxR6e3rCPws0jeLU8tEs8hNgYjZ9K2SuKbmljqAXJfBVywpt\nuKgdBhde+QKBgEyiW+27nQDC4EbdhhYKq233dR/GRgWfm5Fd8ti/YRGxhwhivm1h\nXOBTzkb1XLnkPD14Z3+ZvVooWfVyWXVIOPBjpCQ4ctgQFV9wrXmWv2UnLzr+DLlH\noHcea4cWv7ONvd5aX2ouBDJW3ANZa3jiOKYjVLbyShvnUi1oTcUxZh61AoGBAIsZ\nZJwGH8YFsrc652KCRmQgPzr6A2CrXUnEgXXTjcAI3FtvVN2Jv91a9A+CP+fXs+4R\nsbBoH58CxvkilzaMKS5boPVyGlfqcjzcJhVUAndoFNLna0A178rh0GtHlKnKRuaS\nN3xp7PaUrayd075g5HXv8hQitPT2svwOoEiolrOpAoGAaQ+AVd4p5GY4IQwqroCn\npMSwScJvFkXO/jxw6GnfsJCd9v0D9Q/17a4T3n5i8tKu1hWCqslMRPtKlRj60v/o\nhL7sIeG+as4s0N1VymEaxx3x8DczPaIBQYFPouKAADiI1CTfi8ExFSzA4mlxY3Gi\n3cPxE1MrZwFTGb3PDdBjoQc=\n-----END PRIVATE KEY-----\n",
  "client_email": "apitoolkit-pusher@past-3.iam.gserviceaccount.com",
  "client_id": "108873686795485719221",
  "auth_uri": "https://accounts.google.com/o/oauth2/auth",
  "token_uri": "https://oauth2.googleapis.com/token",
  "auth_provider_x509_cert_url": "https://www.googleapis.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://www.googleapis.com/robot/v1/metadata/x509/apitoolkit-pusher%40past-3.iam.gserviceaccount.com"
}|]
