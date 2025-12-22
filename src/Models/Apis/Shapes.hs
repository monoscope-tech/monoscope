module Models.Apis.Shapes (
  Shape (..),
  ShapeWithFields (..),
  SwShape (..),
  ShapeId,
  bulkInsertShapes,
)
where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.Types (CamelToSnake, Entity, FieldModifiers, GenericEntity, PrimaryKey, Schema, TableName)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Models.Apis.Fields.Types
import Models.Apis.Fields.Types qualified as Fields
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId (..))
import Relude


type ShapeId = UUIDId "shape"


data ShapeWithFields = ShapeWidthFields
  { status :: Int
  , sHash :: Text
  , fieldsMap :: Map FieldCategoryEnum [Fields.Field]
  , reqDescription :: Text
  , resDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)


-- A shape is a deterministic representation of a request-response combination for a given endpoint.
-- We usually expect multiple shapes per endpoint. Eg a shape for a success request-response and another for an error response.
data Shape = Shape
  { id :: ShapeId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , approvedOn :: Maybe UTCTime
  , projectId :: Projects.ProjectId
  , endpointHash :: Text
  , queryParamsKeypaths :: V.Vector Text
  , requestBodyKeypaths :: V.Vector Text
  , responseBodyKeypaths :: V.Vector Text
  , requestHeadersKeypaths :: V.Vector Text
  , responseHeadersKeypaths :: V.Vector Text
  , fieldHashes :: V.Vector Text
  , hash :: Text
  , statusCode :: Int
  , responseDescription :: Text
  , requestDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving (Entity) via (GenericEntity '[Schema "apis", TableName "shapes", PrimaryKey "id", FieldModifiers '[CamelToSnake]] Shape)
  deriving (FromField) via Aeson Shape
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Shape


bulkInsertShapes :: (IOE :> es, WithConnection :> es) => V.Vector Shape -> Eff es ()
bulkInsertShapes shapes = void $ PG.executeMany q $ V.toList rowsToInsert
  where
    q =
      [sql| 
        INSERT INTO apis.shapes
            (project_id, endpoint_hash, query_params_keypaths, request_body_keypaths, response_body_keypaths, request_headers_keypaths, response_headers_keypaths, field_hashes, hash, status_code, request_description, response_description)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING|]
    rowsToInsert =
      V.map
        ( \shape ->
            ( shape.projectId
            , shape.endpointHash
            , shape.queryParamsKeypaths
            , shape.requestBodyKeypaths
            , shape.responseBodyKeypaths
            , shape.requestHeadersKeypaths
            , shape.responseHeadersKeypaths
            , shape.fieldHashes
            , shape.hash
            , fromIntegral shape.statusCode
            , ""
            , ""
            )
        )
        shapes


data SwShape = SwShape
  { swEndpointHash :: Text
  , swQueryParamsKeypaths :: V.Vector Text
  , swRequestBodyKeypaths :: V.Vector Text
  , swResponseBodyKeypaths :: V.Vector Text
  , swRequestHeadersKeypaths :: V.Vector Text
  , swResponseHeadersKeypaths :: V.Vector Text
  , swHash :: Text
  , swStatusCode :: Int
  , swFieldHashes :: V.Vector Text
  , swRequestDescription :: Text
  , swResponseDescription :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default, FromRow, NFData, ToRow)
  deriving anyclass (AE.ToJSON)
  deriving (FromField) via Aeson SwShape
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SwShape
