module Models.Apis.GenerateSwagger (endpointsSwaggerData, Endpoint (..)) where

import Data.Aeson (FromJSON, ToJSON, Value, encode)
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default)
import Data.Default.Instances ()
import Data.Text (Text)
import Data.Time (UTCTime, ZonedTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query, queryOne)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, Only (Only), Query, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Optics.TH (makeFieldLabelsNoPrefix)
import Relude
import Utils (DBField (MkDBField))
import Web.HttpApiData (FromHttpApiData)

-- newtype EndpointId = EndpointId {unEndpointId :: UUID.UUID}
--   deriving stock (Generic, Show)
--   deriving
--     (ToJSON, FromJSON, Eq, Ord, FromField, ToField, FromHttpApiData, Default)
--     via UUID.UUID
--   deriving anyclass (FromRow, ToRow)

data Endpoint = Endpoint
  { urlPath :: Text
  , urlParams :: Value -- Assuming you have a suitable JSON type
  , method :: Text
  , hash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint
  deriving (FromField) via Aeson Endpoint

-- data Shape = Shape
--   { shapeId :: UUID
--   , shapeCreatedAt :: UTCTime
--   , shapeUpdatedAt :: UTCTime
--   , shapeProjectId :: UUID
--   , shapeEndpointHash :: Text
--   , shapeQueryParamsKeypaths :: V.Vector Text
--   , shapeRequestBodyKeypaths :: V.Vector Text
--   , shapeResponseBodyKeypaths :: V.Vector Text
--   , shapeRequestHeadersKeypaths :: V.Vector Text
--   , shapeResponseHeadersKeypaths :: V.Vector Text
--   , shapeFieldHashes :: V.Vector Text
--   , shapeNewUniqueFields :: V.Vector Text
--   , shapeDeletedFields :: V.Vector Text
--   , shapeUpdatedFieldFormats :: V.Vector Text
--   , shapeStatusCode :: Int
--   }

-- data FieldT = FieldT
--   { fieldId :: UUID
--   , fieldCreatedAt :: UTCTime
--   , fieldUpdatedAt :: UTCTime
--   , fieldProjectId :: UUID
--   , fieldEndpointHash :: Text
--   , fieldKey :: Text
--   , fieldFieldType :: FieldType -- Assuming you have a suitable FieldType type
--   , fieldFormat :: Text
--   , fieldFormatOverride :: Text
--   , fieldDescription :: Text
--   , fieldKeyPath :: Text
--   , fieldCategory :: FieldCategory -- Assuming you have a suitable FieldCategory type
--   }

-- data FieldType = Unknown | String | Number | Bool | Object | List | Null
--   deriving stock (Show, Generic, Eq)
--   deriving anyclass (FromField)

-- data FieldCategory = PathParam | QueryParam | RequestHeader | ResponseHeader | RequestBody | ResponseBody
--   deriving stock (Show, Generic, Eq)
--   deriving anyclass (FromField)

endpointsSwaggerData :: Projects.ProjectId -> PgT.DBT IO (Vector Endpoint)
endpointsSwaggerData pid = query Select q (Only pid)
 where
  q =
    [sql|
         SELECT e.url_path, e.url_params, e.method,e.hash
         FROM apis.endpoints e
         WHERE e.project_id = ?
       |]
