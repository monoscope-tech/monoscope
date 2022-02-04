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

module Models.Users.Users where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Default.Instances
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime, getZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe
import          Data.CaseInsensitive   ( CI )
import qualified Data.CaseInsensitive as CI

newtype UserId = UserId {getUserId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, FromField, ToField, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

data User = User
  { createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    id :: UserId,
    first_name :: Text,
    last_name :: Text,
    display_image_url :: Text,
    email :: CI Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "users", PET.TableName "users", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] User)

createUserId :: IO UserId
createUserId = UserId <$> UUIDV4.nextRandom

createUser :: Text -> Text -> Text -> Text -> IO User
createUser firstName lastName picture email = do
  uid <- createUserId
  now <- getZonedTime
  pure $
    User
      { createdAt = now,
        updatedAt = now,
        deletedAt = Nothing,
        active = True,
        id = uid,
        first_name = firstName,
        last_name = lastName,
        display_image_url = picture,
        email = CI.mk email
      }

makeFieldLabelsNoPrefix ''User

insertUser :: User -> PgT.DBT IO ()
insertUser = insert @User

userByEmail :: Text -> PgT.DBT IO (Maybe User)
userByEmail email = selectOneByField @User [field| email |] (Only email)
