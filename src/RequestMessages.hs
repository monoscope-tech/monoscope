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

module RequestMessages
  ( RequestMessage (..),
    requestMsgToDumpAndEndpoint,
    valueToFormatStr,
    valueToFields,
  )
where

import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import Data.Time.Clock as Clock
import Data.Time.LocalTime as Time
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import qualified Deriving.Aeson as DAE
import qualified Models.Apis.Endpoints as Endpoints
import qualified Models.Apis.Fields as Fields
import qualified Models.Apis.RequestDumps as RequestDumps
import qualified Models.Apis.Formats as Format
import Optics.Operators
import Optics.TH
import Relude
import Relude.Unsafe as Unsafe
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

-- RequestMessage represents a message for a single request pulled from pubsub.
-- >>> show RequestMessage
data RequestMessage = RequestMessage
  { timestamp :: ZonedTime,
    projectId :: UUID.UUID,
    host :: Text,
    method :: Text,
    referer :: Text,
    urlPath :: Text,
    protoMajor :: Int,
    protoMinor :: Int,
    durationMicroSecs :: Int,
    duration :: Int,
    requestHeaders :: AE.Value,
    responseHeaders :: AE.Value,
    requestBody :: Text,
    responseBody :: Text,
    statusCode :: Int
  }
  deriving (Show, Generic)
  deriving
    (AE.FromJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestMessage

makeFieldLabelsNoPrefix ''RequestMessage

-- |
requestMsgToDumpAndEndpoint :: RequestMessages.RequestMessage -> ZonedTime -> UUID.UUID -> Either Text (RequestDumps.RequestDump, Endpoints.Endpoint, [(Fields.Field, [Text])])
requestMsgToDumpAndEndpoint rM now dumpID = do
  reqBodyB64 <- B64.decodeBase64 $ encodeUtf8 $ rM ^. #requestBody
  reqBody <- eitherStrToText $ AE.eitherDecodeStrict reqBodyB64 :: Either Text AE.Value
  respBodyB64 <- B64.decodeBase64 $ encodeUtf8 $ rM ^. #responseBody
  respBody <- eitherStrToText $ AE.eitherDecodeStrict respBodyB64 :: Either Text AE.Value

  let reqBodyFields = valueToFields reqBody
  let respBodyFields = valueToFields respBody
  let reqBodyFieldHashes = fieldsToHash reqBodyFields
  let respBodyFieldHashes = fieldsToHash respBodyFields
  let reqBodyFieldsDTO = reqBodyFields & map (fieldsToFieldDTO "request_body" (rM ^. #projectId))
  let respBodyFieldsDTO = respBodyFields & map (fieldsToFieldDTO "response_body" (rM ^. #projectId))
  let fieldsDTO = reqBodyFieldsDTO <> respBodyFieldsDTO

  let reqDump =
        RequestDumps.RequestDump
          { createdAt = rM ^. #timestamp,
            updatedAt = now,
            id = dumpID,
            projectId = rM ^. #projectId,
            host = rM ^. #host,
            urlPath = rM ^. #urlPath,
            method = rM ^. #method,
            referer = rM ^. #referer,
            protoMajor = rM ^. #protoMajor,
            protoMinor = rM ^. #protoMinor,
            duration = calendarTimeTime $ secondsToNominalDiffTime $ fromIntegral $ rM ^. #duration,
            statusCode = rM ^. #statusCode,
            requestHeaders = AET.emptyArray,
            responseHeaders = AET.emptyArray,
            requestBody = reqBody,
            responseBody = respBody
          }
  let endpoint =
        Endpoints.Endpoint
          { createdAt = rM ^. #timestamp,
            updatedAt = now,
            id = dumpID,
            projectId = rM ^. #projectId,
            urlPath = rM ^. #urlPath,
            urlParams = AET.emptyObject,
            method = rM ^. #method,
            hosts = [rM ^. #host],
            requestHashes = [reqBodyFieldHashes],
            responseHashes = [respBodyFieldHashes],
            queryparamHashes = []
          }
  pure (reqDump, endpoint, fieldsDTO)

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
fieldsToFieldDTO :: Text -> UUID.UUID -> (Text, AE.Value) -> (Fields.Field, [Text])
fieldsToFieldDTO fieldCategory projectID (keyPath, val) =
  ( Fields.Field
      { createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC",
        updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC",
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
