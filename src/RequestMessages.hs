{-# LANGUAGE TemplateHaskell #-}

module RequestMessages
  ( RequestMessage (..),
    requestMsgToDumpAndEndpoint,
    valueToFormatStr,
    valueToFields,
  )
where

import Data.Aeson qualified as AE
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as AET
import Data.ByteString.Base64 qualified as B64
import Data.HashMap.Strict qualified as HM
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time.Clock as Clock
import Data.Time.LocalTime as Time
import Data.UUID qualified as UUID
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Optics.Operators
import Optics.TH
import Relude
import Relude.Unsafe as Unsafe
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

-- $setup
-- import Relude

-- | RequestMessage represents a message for a single request pulled from pubsub.
data RequestMessage = RequestMessage
  { timestamp :: ZonedTime,
    projectId :: UUID.UUID,
    host :: Text,
    method :: Text,
    referer :: Text,
    urlPath :: Text,
    protoMajor :: Int,
    protoMinor :: Int,
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

requestMsgToDumpAndEndpoint :: RequestMessages.RequestMessage -> ZonedTime -> UUID.UUID -> Either Text (RequestDumps.RequestDump, Endpoints.Endpoint, [(Fields.Field, [Text])])
requestMsgToDumpAndEndpoint rM now dumpID = do
  reqBodyB64 <- B64.decodeBase64 $ encodeUtf8 $ rM ^. #requestBody
  -- NB: At the moment we're discarding the error messages from when we're unable to parse the input
  -- We should log this inputs and maybe input them into the db as is. This is also a potential annomaly for our customers,
  -- And would help us identity what request formats our customers are actually processing, which would help guide our new features.
  -- It could look something like this for a start, but with proper logging and not trace debug.
  -- let reqBodyFields = case reqBodyE of
  --       Left err -> traceShowM err >> []
  --       Right reqBody -> valueToFields reqBody
  let reqBody = fromRight [aesonQQ| {} |] $ AE.eitherDecodeStrict reqBodyB64
  respBodyB64 <- B64.decodeBase64 $ encodeUtf8 $ rM ^. #responseBody
  let respBody = fromRight [aesonQQ| {} |] $ AE.eitherDecodeStrict respBodyB64

  let reqHeaderFields = valueToFields $ rM ^. #requestHeaders
  let respHeaderFields = valueToFields $ rM ^. #responseHeaders
  let reqBodyFields = valueToFields reqBody
  let respBodyFields = valueToFields respBody

  let reqBodyFieldHashes = fieldsToHash reqBodyFields
  let respBodyFieldHashes = fieldsToHash respBodyFields

  let reqHeadersFieldsDTO = reqHeaderFields & map (fieldsToFieldDTO Fields.FCRequestHeader (rM ^. #projectId))
  let respHeadersFieldsDTO = respHeaderFields & map (fieldsToFieldDTO Fields.FCResponseHeader (rM ^. #projectId))
  let reqBodyFieldsDTO = reqBodyFields & map (fieldsToFieldDTO Fields.FCRequestBody (rM ^. #projectId))
  let respBodyFieldsDTO = respBodyFields & map (fieldsToFieldDTO Fields.FCResponseBody (rM ^. #projectId))
  let fieldsDTO = reqHeadersFieldsDTO <> respHeadersFieldsDTO <> reqBodyFieldsDTO <> respBodyFieldsDTO

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
            requestHeaders = rM ^. #requestHeaders,
            responseHeaders = rM ^. #responseHeaders,
            requestBody = reqBody,
            responseBody = respBody
          }
  let endpoint =
        Endpoints.Endpoint
          { createdAt = rM ^. #timestamp,
            updatedAt = now,
            id = Endpoints.EndpointId dumpID,
            projectId = Projects.ProjectId $ rM ^. #projectId,
            urlPath = rM ^. #urlPath,
            urlParams = AET.emptyObject,
            method = rM ^. #method,
            hosts = [rM ^. #host],
            requestHashes = [reqBodyFieldHashes],
            responseHashes = [respBodyFieldHashes],
            queryparamHashes = []
            -- FIXME: Should we have request and response headers? Or should they be part of request and response hashes?
          }
  pure (reqDump, endpoint, fieldsDTO)

-- | valueToFields takes an aeson object and converts it into a list of paths to
-- each primitive value in the json and the values.
--
-- >>> (valueToFields exampleJSON) == exampleJSONFlattened
-- True
valueToFields :: AE.Value -> [(Text, AE.Value)]
valueToFields value = snd $ valueToFields' value ("", [])
  where
    valueToFields' :: AE.Value -> (Text, [(Text, AE.Value)]) -> (Text, [(Text, AE.Value)])
    valueToFields' (AE.Object v) akk = HM.toList v & foldl' (\(akkT, akkL) (key, val) -> (akkT, snd $ valueToFields' val (akkT <> "." <> key, akkL))) akk
    valueToFields' (AE.Array v) akk = foldl' (\(akkT, akkL) val -> (akkT, snd $ valueToFields' val (akkT <> ".[]", akkL))) akk v
    valueToFields' v (akk, l) = (akk, (akk, v) : l)

fieldsToHash :: [(Text, AE.Value)] -> Text
fieldsToHash = foldl' (\akk tp -> akk <> "," <> fst tp) ""

aeValueToFieldType :: AE.Value -> Fields.FieldTypes
aeValueToFieldType (AET.String _) = Fields.FTString
aeValueToFieldType (AET.Number _) = Fields.FTNumber
aeValueToFieldType AET.Null = Fields.FTNull
aeValueToFieldType (AET.Bool _) = Fields.FTBool
aeValueToFieldType (AET.Object _) = Fields.FTObject
aeValueToFieldType (AET.Array _) = Fields.FTList

valueToFormat :: AE.Value -> Text
valueToFormat (AET.String val) = valueToFormatStr val
valueToFormat (AET.Number val) = valueToFormatNum val
valueToFormat (AET.Bool _) = "bool"
valueToFormat AET.Null = "null"
valueToFormat (AET.Object _) = "object"
valueToFormat (AET.Array _) = "array"

-- | valueToFormatStr will take a string and try to find a format which matches that string best.
-- At the moment it takes a text and returns a generic mask that represents the format of that text
-- >>> valueToFormatStr "22/02/2022"
-- "text"
-- >>> valueToFormatStr "20-02-2022"
-- "text"
-- >>> valueToFormatStr "22.02.2022"
-- "text"
-- >>> valueToFormatStr "222"
-- "integer"
valueToFormatStr :: Text -> Text
valueToFormatStr val
  | val =~ ([r|^[0-9]+$|] :: Text) = "integer"
  | val =~ ([r|^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$|] :: Text) = "float"
  | val =~ ([r|^(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$|] :: Text) = "mm/dd/yyyy"
  | val =~ ([r|^(0[1-9]|1[012])[- -.](0[1-9]|[12][0-9]|3[01])[- -.](19|20)\d\d$|] :: Text) = "mm-dd-yyyy"
  | val =~ ([r|^(0[1-9]|1[012])[- ..](0[1-9]|[12][0-9]|3[01])[- ..](19|20)\d\d$|] :: Text) = "mm.dd.yyyy"
  | otherwise = "text"

valueToFormatNum :: Scientific.Scientific -> Text
valueToFormatNum val
  | Scientific.isFloating val = "float"
  | Scientific.isInteger val = "integer"
  | otherwise = "unknown"

fieldsToFieldDTO :: Fields.FieldCategoryEnum -> UUID.UUID -> (Text, AE.Value) -> (Fields.Field, [Text])
fieldsToFieldDTO fieldCategory projectID (keyPath, val) =
  ( Fields.Field
      { createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC",
        updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC",
        id = Fields.FieldId UUID.nil,
        endpoint = Endpoints.EndpointId UUID.nil,
        projectId = Projects.ProjectId projectID,
        key = snd $ T.breakOnEnd "." keyPath,
        fieldType = aeValueToFieldType val,
        fieldTypeOverride = Just "",
        format = valueToFormat val,
        formatOverride = Just "",
        description = "",
        keyPath = fromList $ T.splitOn "." keyPath,
        keyPathStr = keyPath,
        fieldCategory = fieldCategory
      },
    []
  )

---------------------------------------------------------------------------------------
-- Test Stubs

-- | exampleJSON is a test stup used for testing the valueToFields function via doctest
exampleJSON :: AE.Value
exampleJSON =
  [aesonQQ| {
              "menu": {
                "id": "file",
                "value": "File",
                "popup": {
                  "menuitem": [
                    {"value": "New", "onclick": "CreateNewDoc()"},
                    {"value": "Open", "onclick": "OpenDoc()"},
                    {"value": "Close", "onclick": "CloseDoc()"}
                  ]
                }
              }
            }|]

-- | exampleJSONFlattened is the response from when exampleJSON is flattened via the valueToFields function
exampleJSONFlattened :: [(Text, AE.Value)]
exampleJSONFlattened =
  [ (".menu.id", AE.String "file"),
    (".menu.value", AE.String "File"),
    (".menu.popup.menuitem.[].value", AE.String "Close"),
    (".menu.popup.menuitem.[].onclick", AE.String "CloseDoc()"),
    (".menu.popup.menuitem.[].value", AE.String "Open"),
    (".menu.popup.menuitem.[].onclick", AE.String "OpenDoc()"),
    (".menu.popup.menuitem.[].value", AE.String "New"),
    (".menu.popup.menuitem.[].onclick", AE.String "CreateNewDoc()")
  ]
