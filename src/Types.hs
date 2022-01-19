{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Types
  ( RequestMessage (..),
    RequestDump (..),
    Endpoint (..),
    Field (..),
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import qualified Deriving.Aeson as DAE
import Relude

-- RequestMessage represents a message for a single request pulled from pubsub.
-- >>> show RequestMessage
data RequestMessage = RequestMessage
  { rmTimestamp :: ZonedTime,
    rmProjectId :: UUID.UUID,
    rmHost :: Text,
    rmMethod :: Text,
    rmReferer :: Text,
    rmUrlPath :: Text,
    rmProtoMajor :: Int,
    rmProtoMinor :: Int,
    rmDurationMicroSecs :: Int,
    rmDuration :: Int,
    rmRequestHeaders :: AE.Value,
    rmResponseHeaders :: AE.Value,
    rmRequestBody :: Text,
    rmResponseBody :: Text,
    rmStatusCode :: Int
  }
  deriving (Show, Generic)
  deriving
    (AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.StripPrefix "rm", DAE.CamelToSnake]] RequestMessage

data RequestDump = RequestDump
  { rdCreatedAt :: ZonedTime,
    rdUpdatedAt :: ZonedTime,
    rdId :: UUID.UUID,
    rdProjectId :: UUID.UUID,
    rdHost :: Text,
    rdUrlPath :: Text,
    rdMethod :: Text,
    rdReferer :: Text,
    rdProtoMajor :: Int,
    rdProtoMinor :: Int,
    rdDuration :: CalendarDiffTime,
    rdRequestHeaders :: AE.Value,
    rdResponseHeaders :: AE.Value,
    rdRequestBody :: AE.Value,
    rdResponseBody :: AE.Value,
    rdStatusCode :: Int
  }
  deriving (Show, Generic)
  deriving (ToRow, FromRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "request_dumps", PET.PrimaryKey "id", PET.FieldModifiers '[PET.StripPrefix "rd", PET.CamelToSnake]] RequestDump)

data Endpoint = Endpoint
  { enpCreatedAt :: ZonedTime,
    enpUpdatedAt :: ZonedTime,
    enpID :: UUID.UUID,
    enpProjectId :: UUID.UUID,
    enpUrlPath :: Text,
    enpUrlParams :: AE.Value,
    enpMethod :: Text,
    enpHosts :: Vector Text,
    enpRequestHashes :: Vector Text,
    enpResponseHashes :: Vector Text,
    enpQueryparamHashes :: Vector Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "endpoints", PET.PrimaryKey "id", PET.FieldModifiers '[PET.StripPrefix "enp", PET.CamelToSnake]] Endpoint)

-- data FieldType = Unknown|Keyword|Text|Number|Boolean|DateTime

data Field = Field
  { fdCreatedAt :: ZonedTime,
    fdUpdatedAt :: ZonedTime,
    fdId :: UUID.UUID,
    fdProjectId :: UUID.UUID,
    fdEndpoint :: UUID.UUID,
    fdKey :: Text,
    fdFieldType :: Text,
    fdFieldTypeOverride :: Maybe Text,
    fdFormat :: Text,
    fdFormatOverride :: Maybe Text,
    fdDescription :: Text,
    fdKeyPath :: Vector Text,
    fdKeyPathStr :: Text,
    fdFieldCategory :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "fields", PET.PrimaryKey "id", PET.FieldModifiers '[PET.StripPrefix "fd", PET.CamelToSnake]] Field)

data Format = Format
  { fmCreatedAt :: ZonedTime,
    fmUpdatedAt :: ZonedTime,
    fmId :: UUID.UUID,
    fmFieldId :: UUID.UUID,
    fmFieldType :: Text,
    fmFieldFormat :: Text,
    fmExamples :: Vector Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "formats", PET.PrimaryKey "id", PET.FieldModifiers '[PET.StripPrefix "fm", PET.CamelToSnake]] Format)
