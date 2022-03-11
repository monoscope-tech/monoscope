{-# LANGUAGE TemplateHaskell #-}

module Models.Apis.Anomalies () where

import Data.Default (Default, def)
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple.FromField (FromField, ResultError (ConversionFailed, UnexpectedNull), fromField, returnError)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField, toField)
import Models.Projects.Projects qualified as Projects
import Models.Users.Users qualified as Users
import Relude
import Web.HttpApiData (FromHttpApiData)

newtype AnomalyId = AnomalyId {unAnomalyId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID

data AnomalyTypes
  = ATUnknown
  | ATField
  | ATEndpoint
  | ATShape
  | ATFormat
  deriving stock (Eq, Generic, Show)

instance Default AnomalyTypes where
  def = ATUnknown

instance ToField AnomalyTypes where
  toField ATUnknown = Escape "unknown"
  toField ATField = Escape "field"
  toField ATEndpoint = Escape "endpoint"
  toField ATShape = Escape "shape"
  toField ATFormat = Escape "format"

parseAnomalyTypes :: (Eq s, IsString s) => s -> Maybe AnomalyTypes
parseAnomalyTypes "unknown" = Just ATUnknown
parseAnomalyTypes "field" = Just ATField
parseAnomalyTypes "endpoint" = Just ATEndpoint
parseAnomalyTypes "shape" = Just ATShape
parseAnomalyTypes "format" = Just ATFormat
parseAnomalyTypes _ = Nothing

instance FromField AnomalyTypes where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseAnomalyTypes bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'anomaly_type' enum, got " <> decodeUtf8 bs <> " instead."

data AnomalyActions
  = AAUnknown
  | AACreated

instance Default AnomalyActions where
  def = AAUnknown

instance ToField AnomalyActions where
  toField AAUnknown = Escape "unknown"
  toField AACreated = Escape "created"

parseAnomalyActions :: (Eq s, IsString s) => s -> Maybe AnomalyActions
parseAnomalyActions "unknown" = Just AAUnknown
parseAnomalyActions "created" = Just AACreated
parseAnomalyActions _ = Nothing

instance FromField AnomalyActions where
  fromField f mdata =
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just bs ->
        case parseAnomalyActions bs of
          Just a -> pure a
          Nothing -> returnError ConversionFailed f $ "Conversion error: Expected 'anomaly_actions' enum, got " <> decodeUtf8 bs <> " instead."

data Anomaly = Anomaly
  { id :: AnomalyId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    projectId :: Projects.ProjectId,
    acknowlegedAt :: Maybe ZonedTime,
    acknowlegedBy :: Maybe Users.UserId,
    anomalyType :: AnomalyTypes,
    action :: AnomalyActions,
    targetId :: UUID.UUID
  }
