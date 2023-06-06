module Models.Apis.GenerateSwagger (endpointsSwaggerData, shapeSwaggerData, fieldsSwaggerData, formatSwaggerData, Format (..), FieldT (..), Shape (..), Endpoint (..)) where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Default.Instances ()
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))

import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (..), query)
import Database.PostgreSQL.Simple (FromRow, Only (Only), ToRow)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Models.Projects.Projects qualified as Projects
import Relude

data Endpoint = Endpoint
  { urlPath :: Text
  , urlParams :: Value
  , method :: Text
  , hash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Endpoint
  deriving (FromField) via Aeson Endpoint

data Shape = Shape
  { queryParamsKeypaths :: V.Vector Text
  , requestBodyKeypaths :: V.Vector Text
  , responseBodyKeypaths :: V.Vector Text
  , requestHeadersKeypaths :: V.Vector Text
  , responseHeadersKeypaths :: V.Vector Text
  , statusCode :: Int
  , endpointHash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape
  deriving (FromField) via Aeson Shape

data FieldT = FieldT
  { key :: Text
  , fieldType :: FieldType
  , fieldTypeOverride :: Maybe Text
  , fieldFormat :: Text
  , description :: Text
  , keyPath :: Text
  , fieldCategory :: FieldCategory
  , fieldHash :: Text
  , fieldEndpointHash :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)

data FieldType
  = UnknownType
  | StringType
  | NumberType
  | BoolType
  | ObjectType
  | ListType
  | NullType
  deriving stock (Show, Generic, Eq)

data FieldCategory
  = PathParamCategory
  | QueryParamCategory
  | RequestHeaderCategory
  | ResponseHeaderCategory
  | RequestBodyCategory
  | ResponseBodyCategory
  deriving stock (Show, Generic, Eq)

instance FromField FieldType where
  fromField field mbData = case mbData of
    Just "unknown" -> pure UnknownType
    Just "string" -> pure StringType
    Just "number" -> pure NumberType
    Just "bool" -> pure BoolType
    Just "object" -> pure ObjectType
    Just "list" -> pure ListType
    Just "null" -> pure NullType
    _ -> pure NullType

instance ToField FieldType where
  toField UnknownType = toField ("unknown" :: Text)
  toField StringType = toField ("string" :: Text)
  toField NumberType = toField ("number" :: Text)
  toField BoolType = toField ("bool" :: Text)
  toField ObjectType = toField ("object" :: Text)
  toField ListType = toField ("list" :: Text)
  toField NullType = toField ("null" :: Text)

instance FromField FieldCategory where
  fromField field mbData = case mbData of
    Just "path_param" -> pure PathParamCategory
    Just "query_param" -> pure QueryParamCategory
    Just "request_header" -> pure RequestHeaderCategory
    Just "response_header" -> pure ResponseHeaderCategory
    Just "request_body" -> pure RequestBodyCategory
    Just "response_body" -> pure ResponseBodyCategory
    _ -> pure QueryParamCategory

instance ToField FieldCategory where
  toField PathParamCategory = toField ("path_param" :: Text)
  toField QueryParamCategory = toField ("query_param" :: Text)
  toField RequestHeaderCategory = toField ("request_header" :: Text)
  toField ResponseHeaderCategory = toField ("response_header" :: Text)
  toField RequestBodyCategory = toField ("request_body" :: Text)
  toField ResponseBodyCategory = toField ("response_body" :: Text)

endpointsSwaggerData :: Projects.ProjectId -> PgT.DBT IO (Vector Endpoint)
endpointsSwaggerData pid = query Select q (Only pid)
 where
  q =
    [sql|
         SELECT e.url_path, e.url_params, e.method,e.hash
         FROM apis.endpoints e
         WHERE e.project_id = ?
       |]

shapeSwaggerData :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector Shape)
shapeSwaggerData pid hashes = query Select q (pid, hashes)
 where
  q =
    [sql|
      SELECT s.query_params_keypaths,
             s.request_body_keypaths, s.response_body_keypaths, s.request_headers_keypaths, 
             s.response_headers_keypaths, s.status_code, s.endpoint_hash
      FROM apis.shapes s
      WHERE s.project_id = ? AND s.endpoint_hash = ANY(?)
    |]

fieldsSwaggerData :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector FieldT)
fieldsSwaggerData pid hashes = query Select q (pid, hashes)
 where
  q =
    [sql|
      SELECT f.key, f.field_type, f.field_type_override, f.format, f.description,
             f.key_path, f.field_category, f.hash field_hash, f.endpoint_hash field_endpoint_hash
      FROM apis.fields f
      WHERE f.project_id = ? AND f.endpoint_hash = ANY(?)
    |]

data Format = Format
  { formatfieldHash :: Text
  , formatfieldFormat :: Text
  , examples :: V.Vector Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromRow, ToRow)

formatSwaggerData :: Projects.ProjectId -> Vector Text -> PgT.DBT IO (Vector Format)
formatSwaggerData pid fieldHashes = query Select q (pid, fieldHashes)
 where
  q =
    [sql|
          SELECT field_hash, field_format, examples
          FROM apis.formats
          WHERE project_id = ? AND  field_hash = ANY(?)
        |]