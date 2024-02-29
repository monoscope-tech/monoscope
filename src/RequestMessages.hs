{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module RequestMessages (
  RequestMessage (..),
  requestMsgToDumpAndEndpoint,
  valueToFormatStr,
  valueToFields,
  redactJSON,
)
where

import Data.Aeson (ToJSON (toJSON), Value)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types qualified as AET
import Data.ByteString.Base64 qualified as B64
import Data.Digest.XXHash
import Data.HashMap.Strict qualified as HM
import Data.List (groupBy)
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time.Clock as Clock
import Data.Time.LocalTime as Time
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Query)
import Deriving.Aeson qualified as DAE
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as Fields (
  insertFieldQueryAndParams,
 )
import Models.Apis.Fields.Types qualified as Fields (
  Field (..),
  FieldCategoryEnum (..),
  FieldId (FieldId),
  FieldTypes (..),
  fieldCategoryEnumToText,
  fieldTypeToText,
 )
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps (SDKTypes (GoOutgoing, JsAxiosOutgoing))
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import NeatInterpolation
import Numeric (showHex)
import Optics.Core
import Optics.TH
import Relude
import Relude.Unsafe as Unsafe hiding (head)
import Text.Regex.TDFA ((=~))
import Utils (DBField ())
import Witch (from)


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


-- | RequestMessage represents a message for a single request pulled from pubsub.
data RequestMessage = RequestMessage
  { duration :: Int -- in nanoseconds
  , host :: Text
  , method :: Text
  , pathParams :: AE.Value --- key value map of the params to their values in the original urlpath.
  , projectId :: UUID.UUID
  , protoMajor :: Int
  , protoMinor :: Int
  , queryParams :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , rawUrl :: Text -- raw request uri: path?query combination
  , referer :: Text
  , requestBody :: Text
  , requestHeaders :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , responseBody :: Text
  , responseHeaders :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , sdkType :: RequestDumps.SDKTypes -- convension should be <language>-<router library> eg: go-gin, go-builtin, js-express
  , statusCode :: Int
  , urlPath :: Text
  , timestamp :: ZonedTime
  , msgId :: Maybe UUID.UUID -- This becomes the request_dump id.
  , parentId :: Maybe UUID.UUID
  , serviceVersion :: Maybe Text -- allow users track deployments and versions (tags, commits, etc)
  , errors :: Maybe [RequestDumps.ATError]
  , tags :: Maybe [Text]
  }
  deriving stock (Show, Generic)
  deriving
    (AE.FromJSON, AE.ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] RequestMessage


makeFieldLabelsNoPrefix ''RequestMessage


-- | Walk the JSON once, redact any fields which are in the list of json paths to be redacted.
-- >>> redactJSON ["menu.id."] [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
--
-- >>> redactJSON ["menu.id"] [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
--
-- >>> redactJSON ["menu.id", "menu.name"] [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "[REDACTED]")]))])
--
-- >>> redactJSON ["menu.[].id", "menu.[].names.[]"] [aesonQQ| {"menu":[{"id":"i1", "names":["John","okon"]}, {"id":"i2"}]} |]
-- Object (fromList [("menu",Array [Object (fromList [("id",String "[REDACTED]"),("names",Array [String "[REDACTED]",String "[REDACTED]"])]),Object (fromList [("id",String "[REDACTED]")])])])
redactJSON :: [Text] -> Value -> Value
redactJSON paths' = redactJSON' (stripPrefixDot paths')
  where
    redactJSON' paths (AET.String value) = if "" `elem` paths then AET.String "[REDACTED]" else AET.String value
    redactJSON' paths (AET.Number value) = if "" `elem` paths then AET.String "[REDACTED]" else AET.Number value
    redactJSON' paths AET.Null = AET.Null
    redactJSON' paths (AET.Bool value) = AET.Bool value
    redactJSON' paths (AET.Object objMap) = AET.Object $ AEK.fromHashMapText $ HM.mapWithKey (\k v -> redactJSON' (mapMaybe (\path -> T.stripPrefix (k <> ".") path <|> T.stripPrefix k path) paths) v) (AEK.toHashMapText objMap)
    redactJSON' paths (AET.Array jsonList) = AET.Array $ Vector.map (redactJSON' (mapMaybe (\path -> T.stripPrefix "[]." path <|> T.stripPrefix "[]" path) paths)) jsonList

    stripPrefixDot = map (\p -> fromMaybe p (T.stripPrefix "." p))


-- requestMsgToDumpAndEndpoint is a very improtant function designed to be run as a pure function
-- which takes in a request and processes it returning an sql query and it's params which can be executed.
-- The advantage of returning these queries and params is that it becomes possible to group together batches of requests
-- from the google cloud pub sub, and to concatenate their queries so that only a single database call is needed for a batch.
-- Also, being a pure function means it's easier to test the request processing logic since we can unit test pure functions easily.
-- We can pass in a request, it's project cache object and inspect the generated sql and params.
requestMsgToDumpAndEndpoint :: Projects.ProjectCache -> RequestMessages.RequestMessage -> UTCTime -> UUID.UUID -> Either Text (Query, [DBField], RequestDumps.RequestDump)
requestMsgToDumpAndEndpoint pjc rM now dumpIDOriginal = do
  -- TODO: User dumpID and msgID to get correct ID
  --
  let dumpID = fromMaybe dumpIDOriginal rM.msgId
  let timestampUTC = zonedTimeToUTC rM.timestamp

  -- TODO: This is a temporary fix to add host in creating endoint hash
  -- These are the projects that we have already created endpoints
  let method = T.toUpper rM.method
  let urlPath = RequestDumps.normalizeUrlPath rM.sdkType rM.statusCode rM.method rM.urlPath
  let !endpointHash = from @String @Text $ showHex (xxHash $ encodeUtf8 $ UUID.toText rM.projectId <> rM.host <> method <> urlPath) ""

  let redactFieldsList = Vector.toList pjc.redactFieldslist <> [".set-cookie", ".password"]
  reqBodyB64 <- B64.decodeBase64 $ encodeUtf8 rM.requestBody
  -- NB: At the moment we're discarding the error messages from when we're unable to parse the input
  -- We should log this inputs and maybe input them into the db as is. This is also a potential annomaly for our customers,
  -- And would help us identity what request formats our customers are actually processing, which would help guide our new features.
  -- It could look something like this for a start, but with proper logging and not trace debug.
  -- let reqBodyFields = case reqBodyE of
  --       Left err -> traceShowM err >> []
  --       Right reqBody -> valueToFields reqBody
  let reqBody = redactJSON redactFieldsList $ fromRight [aesonQQ| {} |] $ AE.eitherDecodeStrict reqBodyB64
  respBodyB64 <- B64.decodeBase64 $ encodeUtf8 rM.responseBody
  let respBody = redactJSON redactFieldsList $ fromRight [aesonQQ| {} |] $ AE.eitherDecodeStrict respBodyB64

  let pathParamFields = valueToFields $ redactJSON redactFieldsList rM.pathParams
      queryParamFields = valueToFields $ redactJSON redactFieldsList rM.queryParams
      reqHeaderFields = valueToFields $ redactJSON redactFieldsList rM.requestHeaders
      respHeaderFields = valueToFields $ redactJSON redactFieldsList rM.responseHeaders
      reqBodyFields = valueToFields reqBody
      respBodyFields = valueToFields respBody
      queryParamsKP = Vector.fromList $ map fst queryParamFields
      requestHeadersKP = Vector.fromList $ map fst reqHeaderFields
      responseHeadersKP = Vector.fromList $ map fst respHeaderFields
      requestBodyKP = Vector.fromList $ map fst reqBodyFields
      responseBodyKP = Vector.fromList $ map fst respBodyFields

  -- We calculate a hash that represents the request.
  -- We skip the request headers from this hash, since the source of the request like browsers might add or skip headers at will,
  -- which would make this process not deterministic anymore, and that's necessary for a hash.
  let combinedKeyPathStr = T.concat $ sort $ Vector.toList $ queryParamsKP <> responseHeadersKP <> requestBodyKP <> responseBodyKP
  let !shapeHash' = from @String @Text $ showHex (xxHash $ encodeUtf8 combinedKeyPathStr) ""
  let shapeHash = endpointHash <> show rM.statusCode <> shapeHash' -- Include the endpoint hash and status code to make the shape hash unique by endpoint and status code.
  let projectId = Projects.ProjectId rM.projectId

  let pathParamsFieldsDTO = pathParamFields <&> fieldsToFieldDTO Fields.FCPathParam projectId endpointHash
      queryParamsFieldsDTO = queryParamFields <&> fieldsToFieldDTO Fields.FCQueryParam projectId endpointHash
      reqHeadersFieldsDTO = reqHeaderFields <&> fieldsToFieldDTO Fields.FCRequestHeader projectId endpointHash
      respHeadersFieldsDTO = respHeaderFields <&> fieldsToFieldDTO Fields.FCResponseHeader projectId endpointHash
      reqBodyFieldsDTO = reqBodyFields <&> fieldsToFieldDTO Fields.FCRequestBody projectId endpointHash
      respBodyFieldsDTO = respBodyFields <&> fieldsToFieldDTO Fields.FCResponseBody projectId endpointHash
      fieldsDTO =
        pathParamsFieldsDTO
          <> queryParamsFieldsDTO
          <> reqHeadersFieldsDTO
          <> respHeadersFieldsDTO
          <> reqBodyFieldsDTO
          <> respBodyFieldsDTO
  let (fields, formats) = unzip fieldsDTO
  let fieldHashes = Vector.fromList $ sort $ map (^. #hash) fields
  let formatHashes = Vector.fromList $ sort $ map (^. #hash) formats

  --- FIXME: why are we not using the actual url params?
  -- Since it foes into the endpoint, maybe it should be the keys and their type? I'm unsure.
  -- At the moment, if an endpoint exists, we don't insert it anymore. But then how do we deal with requests from new hosts?
  let status = rM.statusCode
  let urlParams = AET.emptyObject
  let isOutgoing = isRequestOutgoing rM.sdkType
  let (endpointQ, endpointP)
        | endpointHash `elem` pjc.endpointHashes = ("", []) -- We have the endpoint cache in our db already. Skill adding
        | status == 404 = ("", [])
        | otherwise = Endpoints.upsertEndpointQueryAndParam $ buildEndpoint rM now dumpID projectId method urlPath urlParams endpointHash isOutgoing

  let (shapeQ, shapeP)
        | shapeHash `elem` pjc.shapeHashes = ("", [])
        | status == 404 = ("", [])
        | otherwise =
            do
              -- A shape is a deterministic representation of a request-response combination for a given endpoint.
              -- We usually expect multiple shapes per endpoint. Eg a shape for a success request-response and another for an error response.
              -- Shapes are dependent on the endpoint, statusCode and the unique fields in that shape.
              let shape =
                    Shapes.Shape
                      { id = Shapes.ShapeId dumpID
                      , createdAt = timestampUTC
                      , updatedAt = now
                      , approvedOn = Nothing
                      , projectId = projectId
                      , endpointHash = endpointHash
                      , queryParamsKeypaths = queryParamsKP
                      , requestBodyKeypaths = requestBodyKP
                      , responseBodyKeypaths = responseBodyKP
                      , requestHeadersKeypaths = requestHeadersKP
                      , responseHeadersKeypaths = responseHeadersKP
                      , fieldHashes = fieldHashes
                      , hash = shapeHash
                      , statusCode = rM.statusCode
                      , responseDescription = ""
                      , requestDescription = ""
                      }
              Shapes.insertShapeQueryAndParam shape

  let duration = rM.duration

  let errorsL = fromMaybe empty rM.errors
      errorsJSONB = toJSON errorsL
      tagsV = maybe V.empty V.fromList rM.tags

  -- request dumps are time series dumps representing each requests which we consume from our users.
  -- We use this field via the log explorer for exploring and searching traffic. And at the moment also use it for most time series analytics.
  -- It's likely a good idea to stop relying on it for some of the time series analysis, to allow us easily support request sampling, but still support
  -- relatively accurate analytic counts.
  -- Build the query and params for inserting a request dump into the database.
  -- let (reqDumpQ, reqDumpP) = buildRequestDumpQuery rM dumpID now method urlPath reqBody respBody queryParamsKP requestHeadersKP responseHeadersKP requestBodyKP responseBodyKP endpointHash shapeHash formatHashes fieldHashes
  -- let reqDumpP = RequestDumps.RequestDump rM dumpID now method urlPath reqBody respBody queryParamsKP requestHeadersKP responseHeadersKP requestBodyKP responseBodyKP endpointHash shapeHash formatHashes fieldHashes
  let reqDumpP =
        RequestDumps.RequestDump
          { id = dumpID
          , createdAt = timestampUTC
          , updatedAt = now
          , projectId = rM.projectId
          , host = rM.host
          , urlPath = urlPath
          , rawUrl = rM.rawUrl
          , pathParams = rM.pathParams
          , method = method
          , referer = rM.referer
          , protoMajor = rM.protoMajor
          , protoMinor = rM.protoMinor
          , duration = calendarTimeTime $ secondsToNominalDiffTime $ fromIntegral duration
          , statusCode = rM.statusCode
          , --
            queryParams = rM.queryParams
          , requestBody = reqBody
          , responseBody = respBody
          , requestHeaders = rM.requestHeaders
          , responseHeaders = rM.responseHeaders
          , --
            endpointHash = endpointHash
          , shapeHash = shapeHash
          , formatHashes = formatHashes
          , fieldHashes = fieldHashes
          , durationNs = fromIntegral duration
          , sdkType = rM.sdkType
          , parentId = rM.parentId
          , serviceVersion = rM.serviceVersion
          , errors = errorsJSONB
          , tags = tagsV
          , requestType = RequestDump.getRequestType rM.sdkType
          }

  -- Build all fields and formats, unzip them as separate lists and append them to query and params
  -- We don't border adding them if their shape exists, as we asume that we've already seen such before.
  let (fieldsQ, fieldsP)
        | shapeHash `elem` pjc.shapeHashes = ([], [])
        | status == 404 = ([], [])
        | otherwise = unzip $ map Fields.insertFieldQueryAndParams fields

  -- FIXME:
  -- Instead of having examples as a column under formats, could we be better served by having an examples table?
  -- How do we solve the problem of updating examples for formats if we never insert the format when the shape already exists?
  -- Should we use some randomizer?
  -- The original plan was that we could skip the shape from the input into this function, but then that would mean
  -- also inserting the fields and the shape, when all we want to insert is just the example.
  let (formatsQ, formatsP)
        | shapeHash `elem` pjc.shapeHashes = ([], [])
        | status == 404 = ([], [])
        | otherwise = unzip $ map Formats.insertFormatQueryAndParams formats

  let query = endpointQ <> shapeQ <> mconcat fieldsQ <> mconcat formatsQ
  let params = endpointP <> shapeP <> concat fieldsP <> concat formatsP
  pure (query, params, reqDumpP)


isRequestOutgoing :: SDKTypes -> Bool
isRequestOutgoing sdkType
  | T.isSuffixOf "Outgoing" (show sdkType) = True
  | otherwise = False


buildEndpoint :: RequestMessages.RequestMessage -> UTCTime -> UUID.UUID -> Projects.ProjectId -> Text -> Text -> Value -> Text -> Bool -> Endpoints.Endpoint
buildEndpoint rM now dumpID projectId method urlPath urlParams endpointHash outgoing =
  Endpoints.Endpoint
    { createdAt = zonedTimeToUTC rM.timestamp
    , updatedAt = now
    , id = Endpoints.EndpointId dumpID
    , projectId = projectId
    , urlPath = urlPath
    , urlParams = urlParams
    , method = method
    , host = rM.host
    , hash = endpointHash
    , outgoing = outgoing
    , description = "" :: Text
    }


-- valueToFields takes an aeson object and converts it into a list of paths to
-- each primitive value in the json and the values.
--
-- Regular nested text fields:
-- >>> valueToFields [aesonQQ|{"menu":{"id":"text"}}|]
-- [(".menu.id",[String "text"])]
--
-- Integer nested field within an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":[{"int_field":22}]}}|]
-- [(".menu.id[*].int_field",[Number 22.0])]
--
-- Deeper nested field with an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":{"menuitems":[{"key":"value"}]}}}|]
-- [(".menu.id.menuitems[*].key",[String "value"])]
--
-- Flat array value:
-- >>> valueToFields [aesonQQ|{"menu":["abc", "xyz"]}|]
-- [(".menu[*]",[String "abc",String "xyz"])]
--
-- Float values and Null
-- >>> valueToFields [aesonQQ|{"fl":1.234, "nl": null}|]
-- [(".fl",[Number 1.234]),(".nl",[Null])]
--
-- Multiple fields with same key via array:
-- >>> valueToFields [aesonQQ|{"menu":[{"id":"text"},{"id":123}]}|]
-- [(".menu[*].id",[String "text",Number 123.0])]
--
-- >>> valueToFields [aesonQQ|{"menu":[{"c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd":"text"},{"id":123}]}|]
-- [(".menu[*].id",[Number 123.0]),(".menu[*].{uuid}",[String "text"])]
--
-- FIXME: value To Fields should use the redact fields list to actually redact fields
valueToFields :: AE.Value -> [(Text, [AE.Value])]
valueToFields value = dedupFields $ removeBlacklistedFields $ snd $ valueToFields' value ("", [])
  where
    valueToFields' :: AE.Value -> (Text, [(Text, AE.Value)]) -> (Text, [(Text, AE.Value)])
    valueToFields' (AE.Object v) akk =
      AEK.toHashMapText v
        & HM.toList
        & foldl'
          ( \(akkT, akkL) (k, val) -> (akkT, snd $ valueToFields' val (akkT <> "." <> normalizeKey k, akkL))
          )
          akk
    valueToFields' (AE.Array v) akk = foldl' (\(akkT, akkL) val -> (akkT, snd $ valueToFields' val (akkT <> "[*]", akkL))) akk v
    valueToFields' v (akk, l) = (akk, (akk, v) : l)

    normalizeKey :: Text -> Text
    normalizeKey key = case valueToFormatStr key of
      Just result -> "{" <> result <> "}"
      Nothing -> key


-- debupFields would merge all fields in the list of tuples by the first item in the tupple.
--
-- >>> dedupFields [(".menu[*]",String "xyz"),(".menu[*]",String "abc")]
-- [(".menu[*]",[String "abc",String "xyz"])]
--
-- dedupFields [(".menu.[*]",String "xyz"),(".menu.[*]",String "abc"),(".menu.[*]",Number 123)]
-- [(".menu.[*]",[Number 123.0,String "abc",String "xyz"])]
--
-- dedupFields [(".menu.[*].a",String "xyz"),(".menu.[*].b",String "abc"),(".menu.[*].a",Number 123)]
-- [(".menu.[*].a",[Number 123.0,String "xyz"]),(".menu.[*].b",[String "abc"])]
--
dedupFields :: [(Text, AE.Value)] -> [(Text, [AE.Value])]
dedupFields fields =
  sortWith fst fields
    & groupBy (\a b -> fst a == fst b)
    & map (foldl' (\(_, xs) (a, b) -> (a, b : xs)) ("", []))


-- >>> removeBlacklistedFields [(".menu.password",String "xyz"),(".authorization",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".authorization",String "[REDACTED]")]
--
-- >>> removeBlacklistedFields [(".menu.password",Null),(".regular",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".regular",String "abc")]
removeBlacklistedFields :: [(Text, AE.Value)] -> [(Text, AE.Value)]
removeBlacklistedFields = map \(k, val) ->
  if or @[]
    [ T.isSuffixOf "password" (T.toLower k)
    , T.isSuffixOf "authorization" (T.toLower k)
    , T.isSuffixOf "cookie" (T.toLower k)
    ]
    then (k, AE.String "[REDACTED]")
    else (k, val)


-- >>> valueToFormat (AET.String "22")
-- "integer"
--
-- >>> valueToFormat (AET.String "22.33")
-- "float"
--
-- >>> valueToFormat (AET.String "22/02/2022")
-- "text"
--
valueToFormat :: AE.Value -> Text
valueToFormat (AET.String val) = fromMaybe "text" $ valueToFormatStr val
valueToFormat (AET.Number val) = valueToFormatNum val
valueToFormat (AET.Bool _) = "bool"
valueToFormat AET.Null = "null"
valueToFormat (AET.Object _) = "object"
valueToFormat (AET.Array _) = "array"


-- valueToFormatStr will take a string and try to find a format which matches that string best.
-- At the moment it takes a text and returns a generic mask that represents the format of that text
--
-- >>> valueToFormatStr "22/02/2022"
-- Just "text"
--
-- >>> valueToFormatStr "20-02-2022"
-- Just "text"
--
-- >>> valueToFormatStr "22.02.2022"
-- Nothing
--
-- >>> valueToFormatStr "222"
-- Just "integer"
--
-- >>> valueToFormatStr "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
-- Just "uuid"
--
-- >>> valueToFormatStr "2023-10-14T10:29:38.64522Z"
-- Just "YYYY-MM-DDThh:mm:ss.sTZD"
--
valueToFormatStr :: Text -> Maybe Text
valueToFormatStr val
  | val =~ ([text|^[0-9]+$|] :: Text) = Just "integer"
  | val =~ ([text|^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$|] :: Text) = Just "float"
  | val =~ ([text|^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$|] :: Text) = Just "uuid"
  | val =~ ([text|^(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$|] :: Text) = Just "mm/dd/yyyy"
  | val =~ ([text|^(0[1-9]|1[012])[- -.](0[1-9]|[12][0-9]|3[01])[- -.](19|20)\d\d$|] :: Text) = Just "mm-dd-yyyy"
  | val =~ ([text|^(0[1-9]|1[012])[- ..](0[1-9]|[12][0-9]|3[01])[- ..](19|20)\d\d$|] :: Text) = Just "mm.dd.yyyy"
  | val =~ ([text|^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?(([+-]\d\d:\d\d)|Z)?$|] :: Text) = Just "YYYY-MM-DDThh:mm:ss.sTZD"
  | otherwise = Nothing


-- >>> valueToFormatNum 22.3
-- "float"
-- >>> valueToFormatNum 22
-- "integer"
valueToFormatNum :: Scientific.Scientific -> Text
valueToFormatNum val
  | Scientific.isFloating val = "float"
  | Scientific.isInteger val = "integer"
  | otherwise = "unknown"


-- fieldsToFieldDTO processes a field from apitoolkit clients into a field and format record,
-- which can then be converted into separate sql insert queries.
fieldsToFieldDTO :: Fields.FieldCategoryEnum -> Projects.ProjectId -> Text -> (Text, [AE.Value]) -> (Fields.Field, Formats.Format)
fieldsToFieldDTO fieldCategory projectID endpointHash (keyPath, val) =
  ( Fields.Field
      { createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , id = Fields.FieldId UUID.nil
      , endpointHash = endpointHash
      , projectId = projectID
      , key = snd $ T.breakOnEnd "." keyPath
      , -- FIXME: We're discarding the field values of the others, if theer was more than 1 value.
        -- FIXME: We should instead take all the fields into consideration
        -- FIXME: when generating the field types and formats
        fieldType = fieldType
      , fieldTypeOverride = Nothing
      , format = format
      , formatOverride = Nothing
      , description = ""
      , keyPath = keyPath
      , fieldCategory = fieldCategory
      , hash = fieldHash
      , isEnum = False
      , isRequired = False
      }
  , Formats.Format
      { id = Formats.FormatId UUID.nil
      , createdAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , updatedAt = Unsafe.read "2019-08-31 05:14:37.537084021 UTC"
      , projectId = projectID
      , fieldHash = fieldHash
      , fieldType = fieldType
      , fieldFormat = format
      , -- NOTE: A trailing question, is whether to store examples into a separate table.
        -- It requires some more of a cost benefit analysis.
        examples = Vector.fromList val
      , hash = formatHash
      }
  )
  where
    aeValueToFieldType :: AE.Value -> Fields.FieldTypes
    aeValueToFieldType (AET.String _) = Fields.FTString
    aeValueToFieldType (AET.Number _) = Fields.FTNumber
    aeValueToFieldType AET.Null = Fields.FTNull
    aeValueToFieldType (AET.Bool _) = Fields.FTBool
    aeValueToFieldType (AET.Object _) = Fields.FTObject
    aeValueToFieldType (AET.Array _) = Fields.FTList

    fieldType :: Fields.FieldTypes
    fieldType = fromMaybe Fields.FTUnknown $ viaNonEmpty head $ aeValueToFieldType <$> val

    -- FIXME: We should rethink this value to format logic.
    -- FIXME: Maybe it actually needs machine learning,
    -- FIXME: or maybe it should operate on the entire list, and not just one value.
    format = fromMaybe "" $ viaNonEmpty head $ valueToFormat <$> val

    -- field hash is <hash of the endpoint> + <the hash of <field_category><key_path_str><field_type>> (No space or comma between data)
    preFieldHash = Fields.fieldCategoryEnumToText fieldCategory <> keyPath <> Fields.fieldTypeToText fieldType
    fieldHash' = from @String @Text $ showHex (xxHash $ encodeUtf8 preFieldHash) ""
    fieldHash = endpointHash <> fieldHash'
    formatHash' = from @String @Text $ showHex (xxHash $ encodeUtf8 format) ""
    formatHash = fieldHash <> formatHash'
