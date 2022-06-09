{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module RequestMessages
  ( RequestMessage (..),
    requestMsgToDumpAndEndpoint,
    valueToFormatStr,
    valueToFields,
    SDKTypes (..),
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Aeson.Optics
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as AET
import Data.ByteString.Base64 qualified as B64
import Data.HashMap.Strict qualified as HM
import Data.List (groupBy)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time.Clock as Clock
import Data.Time.LocalTime as Time
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Optics.Core
import Optics.Operators
import Optics.TH
import Relude
import Relude.Unsafe as Unsafe hiding (head)
import Text.RawString.QQ
import Text.Regex.TDFA ((=~))

-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson

data SDKTypes = GoGin | GoBuiltIn | PhpLaravel | JsExpress | JavaSpringBoot
  deriving stock (Show, Generic, Read)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] SDKTypes

-- | RequestMessage represents a message for a single request pulled from pubsub.
data RequestMessage = RequestMessage
  { duration :: Int, -- in nanoseconds
    host :: Text,
    method :: Text,
    pathParams :: AE.Value, --- key value map of the params to their values in the original urlpath.
    projectId :: UUID.UUID,
    protoMajor :: Int,
    protoMinor :: Int,
    queryParams :: AE.Value, -- key value map of a key to a list of text values map[string][]string
    rawUrl :: Text, -- raw request uri: path?query combination
    referer :: Text,
    requestBody :: Text,
    requestHeaders :: AE.Value, -- key value map of a key to a list of text values map[string][]string
    responseBody :: Text,
    responseHeaders :: AE.Value, -- key value map of a key to a list of text values map[string][]string
    sdkType :: SDKTypes, -- convension should be <language>-<router library> eg: go-gin, go-builtin, js-express
    statusCode :: Int,
    urlPath :: Text,
    timestamp :: ZonedTime
  }
  deriving stock (Show, Generic)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestMessage

makeFieldLabelsNoPrefix ''RequestMessage

-- >>> redactJSON "menu.id" [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
-- -- >>> redactJSON "menu.[].id" [aesonQQ| {"menu":[{"id":"file", "name":"John"}]} |]
-- redactJSON :: Text -> Value -> Value
-- redactJSON path = redactJSON' (T.splitOn "." path)
--   where
--     redactJSON' :: [Text] -> Value -> Value
--     redactJSON' [] json = json
--     -- redactJSON' ("[]" : xs) json = json & snd (keyPathToOptics (xs, values)) .~ AE.String "[REDACTED]"
--     redactJSON' (x : xs) json = json & snd (keyPathToOptics (xs, key x)) .~ AE.String "[REDACTED]"

--     keyPathToOptics :: ([Text], AffineTraversal' Value Value) -> ([Text], AffineTraversal' Value Value)
--     keyPathToOptics ([], optics) = ([], optics)
--     keyPathToOptics ("[]" : x : xs, optics) = keyPathToOptics (xs, optics % values % key x)
--     keyPathToOptics ("[]" : xs, optics) = keyPathToOptics (xs, optics % values)
--     keyPathToOptics (x : xs, optics) = keyPathToOptics (xs, optics % key x)

exJSON :: AET.Value
exJSON =
  [aesonQQ| {
              "menu": {
                "id": "file",
                "list": ["a", "b"],
                "popup": {
                  "menuitem": [
                    {"value": "v1", "onclick": "oc1"},
                    {"value": "v2", "onclick": "oc2"}
                  ]
                }
              }
            }|]

requestMsgToDumpAndEndpoint :: (Text -> Bool) -> RequestMessages.RequestMessage -> ZonedTime -> UUID.UUID -> Either Text (RequestDumps.RequestDump, Endpoints.Endpoint, [(Fields.Field, [AE.Value])], Shapes.Shape)
requestMsgToDumpAndEndpoint shouldRedact rM now dumpID = do
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

  let pathParamFields = valueToFields shouldRedact $ rM ^. #pathParams
  let queryParamFields = valueToFields shouldRedact $ rM ^. #queryParams
  let reqHeaderFields = valueToFields shouldRedact $ rM ^. #requestHeaders
  let respHeaderFields = valueToFields shouldRedact $ rM ^. #responseHeaders
  let reqBodyFields = valueToFields shouldRedact reqBody
  let respBodyFields = valueToFields shouldRedact respBody

  let queryParamsKeypaths = Vector.fromList $ map fst queryParamFields :: Vector Text
  let requestHeadersKeypaths = Vector.fromList $ map fst reqHeaderFields :: Vector Text
  let responseHeadersKeypaths = Vector.fromList $ map fst respHeaderFields :: Vector Text
  let requestBodyKeypaths = Vector.fromList $ map fst reqBodyFields :: Vector Text
  let responseBodyKeypaths = Vector.fromList $ map fst respBodyFields :: Vector Text

  let pathParamsFieldsDTO = pathParamFields & map (fieldsToFieldDTO Fields.FCPathParam (rM ^. #projectId))
  let queryParamsFieldsDTO = queryParamFields & map (fieldsToFieldDTO Fields.FCQueryParam (rM ^. #projectId))
  let reqHeadersFieldsDTO = reqHeaderFields & map (fieldsToFieldDTO Fields.FCRequestHeader (rM ^. #projectId))
  let respHeadersFieldsDTO = respHeaderFields & map (fieldsToFieldDTO Fields.FCResponseHeader (rM ^. #projectId))
  let reqBodyFieldsDTO = reqBodyFields & map (fieldsToFieldDTO Fields.FCRequestBody (rM ^. #projectId))
  let respBodyFieldsDTO = respBodyFields & map (fieldsToFieldDTO Fields.FCResponseBody (rM ^. #projectId))
  let fieldsDTO =
        pathParamsFieldsDTO
          <> queryParamsFieldsDTO
          <> reqHeadersFieldsDTO
          <> respHeadersFieldsDTO
          <> reqBodyFieldsDTO
          <> respBodyFieldsDTO

  let method = T.toUpper $ rM ^. #method
  let urlPath = normalizeUrlPath (rM ^. #sdkType) (rM ^. #urlPath)

  let shape =
        Shapes.Shape
          { createdAt = rM ^. #timestamp,
            updatedAt = now,
            id = Shapes.ShapeId dumpID,
            projectId = Projects.ProjectId $ rM ^. #projectId,
            endpointId = Endpoints.EndpointId dumpID, -- SHould be overwritten based off what we get from the db when creating endpoints id
            --
            queryParamsKeypaths = queryParamsKeypaths,
            requestHeadersKeypaths = requestHeadersKeypaths,
            responseHeadersKeypaths = responseHeadersKeypaths,
            requestBodyKeypaths = requestBodyKeypaths,
            responseBodyKeypaths = responseBodyKeypaths
          }

  let reqDump =
        RequestDumps.RequestDump
          { createdAt = rM ^. #timestamp,
            updatedAt = now,
            id = dumpID,
            projectId = rM ^. #projectId,
            host = rM ^. #host,
            urlPath = urlPath,
            rawUrl = rM ^. #rawUrl,
            pathParams = rM ^. #pathParams,
            method = method,
            referer = rM ^. #referer,
            protoMajor = rM ^. #protoMajor,
            protoMinor = rM ^. #protoMinor,
            duration = calendarTimeTime $ secondsToNominalDiffTime $ fromIntegral $ rM ^. #duration,
            statusCode = rM ^. #statusCode,
            queryParams = rM ^. #queryParams,
            requestHeaders = rM ^. #requestHeaders,
            responseHeaders = rM ^. #responseHeaders,
            requestBody = reqBody,
            responseBody = respBody,
            --
            queryParamsKeypaths = queryParamsKeypaths,
            requestHeadersKeypaths = requestHeadersKeypaths,
            responseHeadersKeypaths = responseHeadersKeypaths,
            requestBodyKeypaths = requestBodyKeypaths,
            responseBodyKeypaths = responseBodyKeypaths,
            --
            shapeId = Shapes.ShapeId dumpID,
            formatIds = Vector.empty, -- set with values from the db
            fieldIds = Vector.empty
          }
  let endpoint =
        Endpoints.Endpoint
          { createdAt = rM ^. #timestamp,
            updatedAt = now,
            id = Endpoints.EndpointId dumpID,
            projectId = Projects.ProjectId $ rM ^. #projectId,
            urlPath = urlPath,
            urlParams = AET.emptyObject,
            method = method,
            hosts = [rM ^. #host]
          }
  pure (reqDump, endpoint, fieldsDTO, shape)

normalizeUrlPath :: SDKTypes -> Text -> Text
normalizeUrlPath GoGin urlPath = urlPath
normalizeUrlPath GoBuiltIn urlPath = urlPath
normalizeUrlPath PhpLaravel urlPath = urlPath
normalizeUrlPath JsExpress urlPath = urlPath
normalizeUrlPath JavaSpringBoot urlPath = urlPath

-- | valueToFields takes an aeson object and converts it into a list of paths to
-- each primitive value in the json and the values.
--
-- Regular nested text fields:
-- >>> valueToFields [aesonQQ|{"menu":{"id":"text"}}|]
-- [(".menu.id",[String "text"])]
--
-- Integer nested field within an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":[{"int_field":22}]}}|]
-- [(".menu.id.[].int_field",[Number 22.0])]
--
-- Deeper nested field with an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":{"menuitems":[{"key":"value"}]}}}|]
-- [(".menu.id.menuitems.[].key",[String "value"])]
--
-- Flat array value:
-- >>> valueToFields [aesonQQ|{"menu":["abc", "xyz"]}|]
-- [(".menu.[]",[String "abc",String "xyz"])]
--
-- Float values and Null
-- >>> valueToFields [aesonQQ|{"fl":1.234, "nl": null}|]
-- [(".fl",[Number 1.234]),(".nl",[Null])]
--
-- Multiple fields with same key via array:
-- >>> valueToFields [aesonQQ|{"menu":[{"id":"text"},{"id":123}]}|]
-- [(".menu.[].id",[String "text",Number 123.0])]
valueToFields :: (Text -> Bool) -> AE.Value -> [(Text, [AE.Value])]
valueToFields shouldRedact value = dedupFields $ removeBlacklistedFields $ snd $ valueToFields' value ("", [])
  where
    valueToFields' :: AE.Value -> (Text, [(Text, AE.Value)]) -> (Text, [(Text, AE.Value)])
    valueToFields' (AE.Object v) akk = HM.toList v & foldl' (\(akkT, akkL) (key, val) -> (akkT, snd $ valueToFields' val (akkT <> "." <> key, akkL))) akk
    valueToFields' (AE.Array v) akk = foldl' (\(akkT, akkL) val -> (akkT, snd $ valueToFields' val (akkT <> ".[]", akkL))) akk v
    valueToFields' v (akk, l) = (akk, (akk, v) : l)

    -- debupFields would merge all fields in the list of tuples by the first item in the tupple.
    --
    -- >>> dedupFields [(".menu.[]",String "xyz"),(".menu.[]",String "abc")]
    -- [(".menu.[]",[String "abc",String "xyz"])]
    -- >>> dedupFields [(".menu.[]",String "xyz"),(".menu.[]",String "abc"),(".menu.[]",Number 123)]
    -- [(".menu.[]",[Number 123.0,String "abc",String "xyz"])]
    dedupFields :: [(Text, AE.Value)] -> [(Text, [AE.Value])]
    dedupFields fields =
      sortWith fst fields
        & groupBy (\a b -> fst a == fst b)
        & map (foldl' (\(_, xs) (a, b) -> (a, b : xs)) ("", []))

    -- >>> removeBlacklistedFields [(".menu.password",String "xyz"),(".authorization",String "abc")]
    -- [(".menu.password",String "[REDACTED]"),(".authorization",String "[REDACTED]")]
    -- >>> removeBlacklistedFields [(".menu.password",Null),(".regular",String "abc")]
    -- [(".menu.password",String "[REDACTED]"),(".regular",String "abc")]
    removeBlacklistedFields :: [(Text, AE.Value)] -> [(Text, AE.Value)]
    removeBlacklistedFields = map \(key, val) ->
      if or @[]
        [ T.isSuffixOf "password" (T.toLower key),
          T.isSuffixOf "authorization" (T.toLower key),
          T.isSuffixOf "cookie" (T.toLower key),
          shouldRedact key
        ]
        then do
          traceShowM $ "🔥 REDACTED " <> show key
          (key, AE.String "[REDACTED]")
        else do
          traceShowM $ "NOT REDACTED " <> show key
          (key, val)

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

fieldsToFieldDTO :: Fields.FieldCategoryEnum -> UUID.UUID -> (Text, [AE.Value]) -> (Fields.Field, [AE.Value])
fieldsToFieldDTO fieldCategory projectID (keyPath, val) =
  ( Fields.Field
      { createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC",
        updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC",
        id = Fields.FieldId UUID.nil,
        endpointId = Endpoints.EndpointId UUID.nil,
        projectId = Projects.ProjectId projectID,
        key = snd $ T.breakOnEnd "." keyPath,
        -- FIXME: We're discarding the field values of the others, if theer was more than 1 value.
        -- FIXME: We should instead take all the fields into consideration when generating the field types and formats
        fieldType = fromMaybe Fields.FTUnknown $ viaNonEmpty head $ aeValueToFieldType <$> val,
        fieldTypeOverride = Just "",
        format = fromMaybe "" $ viaNonEmpty head $ valueToFormat <$> val,
        formatOverride = Just "",
        description = "",
        keyPath = fromList $ T.splitOn "." keyPath,
        keyPathStr = keyPath,
        fieldCategory = fieldCategory
      },
    val
  )
  where
    aeValueToFieldType :: AE.Value -> Fields.FieldTypes
    aeValueToFieldType (AET.String _) = Fields.FTString
    aeValueToFieldType (AET.Number _) = Fields.FTNumber
    aeValueToFieldType AET.Null = Fields.FTNull
    aeValueToFieldType (AET.Bool _) = Fields.FTBool
    aeValueToFieldType (AET.Object _) = Fields.FTObject
    aeValueToFieldType (AET.Array _) = Fields.FTList
