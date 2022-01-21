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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# language OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module ProcessMessage
  ( processMessage,
    valueToFields,
    processRequestMessage,
    valueToFormatStr,
    valueToFormatNum,
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
import qualified Network.Google as Google
import qualified Network.Google.Env as Env
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Resource.PubSub.Projects.Subscriptions.Pull as PubSubPull
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Numeric.Lens ()
import Relude
import Relude.Unsafe (fromJust)
import qualified Relude.Unsafe as Unsafe
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
import qualified Models.Endpoints as Endpoints
import qualified Models.Fields as Fields
import qualified Models.RequestDumps as RequestDumps
import qualified Models.Formats as Format
import qualified RequestMessages as RequestMessages
import Optics.Operators ((^.))

-- |
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

-- | valueToFields takes an aeson object and converts it into a list of paths to
-- each primitive value in the json and the values.
-- >>> valueToFields exJSON
valueToFields :: AE.Value -> [(Text, AE.Value)]
valueToFields value = snd $ valueToFields' value ("", [])
  where
    valueToFields' :: AE.Value -> (Text, [(Text, AE.Value)]) -> (Text, [(Text, AE.Value)])
    valueToFields' (AE.Object v) akk = HM.toList v & foldl' (\(akkT, akkL) (key, val) -> (akkT, snd $ valueToFields' val (akkT <> "." <> key, akkL))) akk
    valueToFields' (AE.Array v) akk = foldl' (\(akkT, akkL) val -> (akkT, snd $ valueToFields' val (akkT <> ".[]", akkL))) akk v
    valueToFields' v (akk, l) = (akk, (akk, v) : l)

-- |
fieldsToHash :: [(Text, AE.Value)] -> Text
fieldsToHash = foldl' (\akk tp -> akk <> "," <> fst tp) ""

-- |
aeValueToText :: AE.Value -> Text
aeValueToText (AET.String _) = "string"
aeValueToText (AET.Number _) = "number"
aeValueToText AET.Null = "null"
aeValueToText (AET.Bool _) = "bool"
aeValueToText (AET.Object _) = "object"
aeValueToText (AET.Array _) = "array"

-- |
valueToFormat :: AE.Value -> Text
valueToFormat (AET.String val) = valueToFormatStr val
valueToFormat (AET.Number val) = valueToFormatNum val
valueToFormat (AET.Bool _) = "bool"
valueToFormat AET.Null = "null"
valueToFormat (AET.Object _) = "object"
valueToFormat (AET.Array _) = "array"

-- |
valueToFormatStr :: Text -> Text
valueToFormatStr val
  | val =~ ([r|^[0-9]+$|] :: Text) = "integer"
  | val =~ ([r|^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$|] :: Text) = "float"
  | val =~ ([r|^(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$|] :: Text) = "mm/dd/yyyy"
  | val =~ ([r|^(0[1-9]|1[012])[- -.](0[1-9]|[12][0-9]|3[01])[- -.](19|20)\d\d$|] :: Text) = "mm-dd-yyyy"
  | val =~ ([r|^(0[1-9]|1[012])[- ..](0[1-9]|[12][0-9]|3[01])[- ..](19|20)\d\d$|] :: Text) = "mm.dd.yyyy"
  | otherwise = "text"

-- |
valueToFormatNum :: Scientific.Scientific -> Text
valueToFormatNum val
  | Scientific.isFloating val = "float"
  | Scientific.isInteger val = "integer"
  | otherwise = "unknown"

-- |
fieldsToFieldDTO :: Text -> UUID -> (Text, AE.Value) -> (Fields.Field, [Text])
fieldsToFieldDTO fieldCategory projectID (keyPath, val) =
  ( Fields.Field
      { createdAt = read "2019-08-31 05:14:37.537084021 UTC",
        updatedAt = read "2019-08-31 05:14:37.537084021 UTC",
        id = UUID.nil,
        endpoint = UUID.nil,
        projectId = projectID,
        key = snd $ T.breakOnEnd "." keyPath,
        fieldType = aeValueToText val,
        fieldTypeOverride = Just "",
        format = valueToFormat val,
        formatOverride = Just "",
        description = "",
        keyPath = fromList $ T.splitOn "," keyPath,
        keyPathStr = keyPath,
        fieldCategory = fieldCategory
      },
    []
  )

-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a

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

