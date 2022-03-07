module DataSeeding (parseConfigToJson) where

import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Time (ZonedTime)
import Deriving.Aeson qualified as DAE
import Relude

data SeedConfig = SeedConfig
  { from :: ZonedTime,
    to :: ZonedTime,
    intervals :: Text,
    countPerInterval :: Int,
    path :: Text,
    pathParams :: [FieldConfig],
    queryParams :: [FieldConfig],
    requestHeaders :: [FieldConfig],
    responseHeaders :: [FieldConfig],
    requestBody :: [FieldConfig],
    responseBody :: [FieldConfig]
  }
  deriving (Show, Generic)
  deriving
    (AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SeedConfig

data FieldConfig = FieldConfig
  { name :: Text,
    fieldType :: Text,
    typeGenFormat :: Text,
    children :: [FieldConfig]
  }
  deriving (Show, Generic)
  deriving
    (AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FieldConfig

parseConfigToJson :: ByteString -> AE.Value
parseConfigToJson input = do
  [aesonQQ|{}|]
