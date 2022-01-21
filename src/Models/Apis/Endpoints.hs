{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.Apis.Endpoints
  ( Endpoint (..),
    upsertEndpoints,
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Default.Instances
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe

data Endpoint = Endpoint
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    id :: UUID.UUID,
    projectId :: UUID.UUID,
    urlPath :: Text,
    urlParams :: AE.Value,
    method :: Text,
    hosts :: Vector.Vector Text,
    requestHashes :: Vector.Vector Text,
    responseHashes :: Vector.Vector Text,
    queryparamHashes :: Vector.Vector Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "apis", PET.TableName "endpoints", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] Endpoint)

makeFieldLabelsNoPrefix ''Endpoint

-- |
upsertEndpoints :: Endpoint -> PgT.DBT IO (Maybe (UUID.UUID, Text, Text))
upsertEndpoints endpoint = queryOne Insert q options
  where
    q =
      [sql|  
        INSERT INTO apis.endpoints (project_id, url_path, url_params, method, hosts, request_hashes, response_hashes, queryparam_hashes)
        VALUES(?, ?, ?, ?, ?, ?, ?, ?) 
        ON CONFLICT (project_id, url_path, method) 
        DO 
           UPDATE SET 
            hosts = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.hosts, excluded.hosts) as e order by e),
            request_hashes = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.request_hashes, excluded.request_hashes) as e order by e),
            response_hashes = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.response_hashes, excluded.response_hashes) as e order by e),
            queryparam_hashes = ARRAY(SELECT DISTINCT e from unnest(apis.endpoints.queryparam_hashes, excluded.queryparam_hashes) as e order by e)
        RETURNING id, method, url_path 
      |]
    options =
      ( endpoint ^. #projectId,
        endpoint ^. #urlPath,
        endpoint ^. #urlParams,
        endpoint ^. #method,
        endpoint ^. #hosts,
        endpoint ^. #requestHashes,
        endpoint ^. #responseHashes,
        endpoint ^. #queryparamHashes
      )
