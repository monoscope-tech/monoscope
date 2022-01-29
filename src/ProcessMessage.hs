{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
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

module ProcessMessage
  ( processMessage,
    processRequestMessage,
  )
where

import Colog.Core (LogAction (..), logStringStdout, (<&))
import qualified Config
import Control.Lens ((^?), _Just)
import qualified Control.Lens as L
import Data.Aeson (Object, Value (Array, Bool, Null, Number, Object, String), decodeStrict, eitherDecode, eitherDecodeStrict)
import Data.Aeson as AE
  ( FromJSON,
    Value (Array, Object),
    decodeStrict,
    eitherDecode,
    eitherDecodeStrict,
  )
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson.Types as AET
import qualified Data.ByteString.Base64 as B64
import Data.Fixed (Pico)
import qualified Data.HashMap.Strict as HM
import Data.Pool (Pool, withResource)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base64 as B64T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import Data.Time.Clock as Clock
import Data.Time.LocalTime as Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity (Entity (fields, tableName), insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (Entity (primaryKey), FieldModifiers, GenericEntity, PrimaryKey, TableName)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory), runMigration)
import Database.PostgreSQL.Simple.Migration as Migrations ()
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import Database.PostgreSQL.Simple.TypeInfo.Static ()
import qualified Database.PostgreSQL.Transact as PgT
import Deriving.Aeson as DAE
import GHC.Generics ()
import qualified Models.Apis.Endpoints as Endpoints
import qualified Models.Apis.Fields as Fields
import qualified Models.Apis.Formats as Format
import qualified Models.Apis.RequestDumps as RequestDumps
import qualified Network.Google as Google
import qualified Network.Google.Env as Env
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Resource.PubSub.Projects.Subscriptions.Pull as PubSubPull
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Numeric.Lens ()
import Optics.Operators ((^.))
import Relude
import Relude.Unsafe (fromJust)
import qualified Relude.Unsafe as Unsafe
import qualified RequestMessages
import Servant
  ( Get,
    Handler,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))
import Prelude (read)

processMessage :: LogAction IO String -> Config.EnvConfig -> Pool Connection -> PubSub.ReceivedMessage -> IO (Maybe Text)
processMessage logger envConfig conn msg = do
  let rmMsg = msg ^? PubSub.rmMessage . _Just . PubSub.pmData . _Just
  let decodedMsg = B64.decodeBase64 $ Unsafe.fromJust rmMsg
  let jsonByteStr = fromRight "{}" decodedMsg
  let decodedMsg = eitherDecode (fromStrict jsonByteStr) :: Either String RequestMessages.RequestMessage
  case decodedMsg of
    Left err -> logger <& "error decoding message" <> err
    Right recMsg -> processRequestMessage conn recMsg
  pure $ msg L.^. PubSub.rmAckId

-- | processRequestMessage would take a request message and
-- process it into formats which can be persisited into the database.
-- it processes the request message into the request dump,
-- the endpoint struct and then the request fields.
processRequestMessage :: Pool Connection -> RequestMessages.RequestMessage -> IO ()
processRequestMessage pool requestMsg = do
  recId <- UUIDV4.nextRandom
  timestamp <- Time.getZonedTime
  let dumpAndEndpoint = RequestMessages.requestMsgToDumpAndEndpoint requestMsg timestamp recId
  case dumpAndEndpoint of
    Left err -> putTextLn err
    Right dAndE -> do
      let (reqDump, endpoint, fields) = dAndE
      withPool pool $ do
        RequestDumps.insertRequestDump reqDump
        enpResp <- Endpoints.upsertEndpoints endpoint
        case enpResp of
          Nothing -> error "error"
          Just (enpID, _, _) -> do
            Fields.upsertFields enpID fields
            pure ()
