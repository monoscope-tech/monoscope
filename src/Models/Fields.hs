{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# language OverloadedLabels #-}

module Models.Fields
  ( Field (..),
  upsertFields
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import qualified Deriving.Aeson as DAE
import qualified Database.PostgreSQL.Transact as PgT
import Relude
import GHC.Generics (Generic)
import Optics.TH
import Optics.Operators
import qualified Relude.Unsafe as Unsafe


instance Default ZonedTime where
  def = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"

instance Default UUID.UUID where
  def = UUID.nil

instance Default AET.Value where
  def = AET.emptyObject 


data Field = Field
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    id :: UUID.UUID,
    projectId :: UUID.UUID,
    endpoint :: UUID.UUID,
    key :: Text,
    fieldType :: Text,
    fieldTypeOverride :: Maybe Text,
    format :: Text,
    formatOverride :: Maybe Text,
    description :: Text,
    keyPath :: Vector.Vector Text,
    keyPathStr :: Text,
    fieldCategory :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "fields", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] Field)

makeFieldLabelsNoPrefix ''Field

-- | upsertFields
upsertFields :: UUID.UUID -> [(Field, [Text])] -> PgT.DBT IO [Maybe (Only Text)]
upsertFields endpointID fields = options & mapM (PgT.queryOne q)
  where
    q =
      [sql|
        select apis.create_field_and_formats(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)::text;
   |]
    options =
      fields
        & map
          ( \(field, examples) ->
              ( field ^. #projectId,
                endpointID,
               field ^. #key,
               field ^. #fieldType,
               field ^. #fieldTypeOverride,
               field ^. #format,
               field ^. #formatOverride,
               field ^. #description,
               field ^. #keyPath,
               field ^. #keyPathStr,
               field ^. #fieldCategory,
                Vector.fromList examples,
                5 :: Int64 -- max examples size
              )
          )


