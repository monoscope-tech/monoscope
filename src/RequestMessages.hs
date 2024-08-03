{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module RequestMessages (
  RequestMessage (..),
  requestMsgToDumpAndEndpoint,
  valueToFormatStr,
  valueToFields,
  redactJSON,
  toXXHash,
  replaceNullChars,
)
where

import Control.Lens ((.~))
import Control.Monad.ST (ST, runST)
import Data.Aeson (ToJSON (toJSON), Value (Object), (.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.Types (object)
import Data.Aeson.Types qualified as AET
import Data.ByteString.Base64 qualified as B64
import Data.Digest.XXHash (xxHash)
import Data.HashMap.Strict qualified as HM
import Data.HashTable.Class qualified as HTC
import Data.HashTable.ST.Cuckoo qualified as HT
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time.Clock as Clock (UTCTime, secondsToNominalDiffTime)
import Data.Time.LocalTime as Time (ZonedTime, calendarTimeTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple (Query)
import Deriving.Aeson qualified as DAE
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Types qualified as Fields (
  Field (..),
  FieldCategoryEnum (..),
  FieldId (FieldId),
  FieldTypes (..),
  fieldCategoryEnumToText,
 )
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.Wreq
import Numeric (showHex)
import Relude
import Relude.Unsafe as Unsafe (read)
import Text.Regex.TDFA ((=~))
import Utils (DBField ())


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


-- | RequestMessage represents a message for a single request pulled from pubsub.
data RequestMessage = RequestMessage
  { duration :: Int -- in nanoseconds
  , host :: Maybe Text
  , method :: Text
  , pathParams :: AE.Value --- key value map of the params to their values in the original urlpath.
  , projectId :: UUID.UUID
  , protoMajor :: Int
  , protoMinor :: Int
  , queryParams :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , rawUrl :: Text -- raw request uri: path?query combination
  , referer :: Maybe (Either Text [Text])
  , requestBody :: Text
  , requestHeaders :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , responseBody :: Text
  , responseHeaders :: AE.Value -- key value map of a key to a list of text values map[string][]string
  , sdkType :: RequestDumps.SDKTypes -- convension should be <language>-<router library> eg: go-gin, go-builtin, js-express
  , statusCode :: Int
  , urlPath :: Maybe Text -- became Maybe to support express, which sometimes doesn't send urlPath for root.
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


-- Custom ToJSON for Either Text [Text]
instance {-# OVERLAPPING #-} AE.ToJSON (Either Text [Text]) where
  toJSON (Left txt) = AE.toJSON txt
  toJSON (Right texts) = AE.toJSON texts


-- Custom FromJSON for Either Text [Text]
instance {-# OVERLAPPING #-} AE.FromJSON (Either Text [Text]) where
  parseJSON value = case value of
    AE.String txt -> return $ Left txt
    AE.Array texts -> Right <$> AE.parseJSON value -- parses an array of Text
    _ -> fail "Expected either a single string or an array of strings"


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
redactJSON :: V.Vector Text -> Value -> Value
redactJSON paths' = redactJSON' (V.map stripPrefixDot paths')
  where
    redactJSON' !paths value = case value of
      AET.String v -> if "" `V.elem` paths then AET.String "[REDACTED]" else AET.String v
      AET.Number v -> if "" `V.elem` paths then AET.String "[REDACTED]" else AET.Number v
      AET.Null -> AET.Null
      AET.Bool v -> AET.Bool v
      AET.Object objMap -> AET.Object $ AEK.fromHashMapText $ HM.mapWithKey (\k v -> redactJSON' (V.mapMaybe (\path -> T.stripPrefix (k <> ".") path <|> T.stripPrefix k path) paths) v) (AEK.toHashMapText objMap)
      AET.Array jsonList -> AET.Array $ V.map (redactJSON' (V.mapMaybe (\path -> T.stripPrefix "[]." path <|> T.stripPrefix "[]" path) paths)) jsonList

    stripPrefixDot !p = fromMaybe p (T.stripPrefix "." p)


replaceNullChars :: Text -> Text
replaceNullChars = T.replace "\\u0000" ""


leftPad :: Int -> Text -> Text
leftPad len txt = T.justifyRight len '0' (T.take len txt)


toXXHash :: Text -> Text
toXXHash input = leftPad 8 $ fromString $ showHex (xxHash $ encodeUtf8 $ input) ""


processErrors :: Projects.ProjectId -> RequestDumps.SDKTypes -> Text -> Text -> RequestDumps.ATError -> (RequestDumps.ATError, Query, [DBField])
processErrors pid sdkType method urlPath err = (normalizedError, q, params)
  where
    (q, params) = Anomalies.insertErrorQueryAndParams pid normalizedError
    normalizedError =
      err
        { RequestDumps.hash = Just $ fromMaybe (toXXHash (pid.toText <> err.errorType <> err.message <> show sdkType)) err.hash
        , RequestDumps.technology = Just sdkType
        , RequestDumps.requestMethod = Just method
        , RequestDumps.requestPath = Just urlPath
        }


sortVector :: Ord a => V.Vector a -> V.Vector a
sortVector vec = runST $ do
  mvec <- V.thaw vec
  VA.sort mvec
  V.freeze mvec


-- requestMsgToDumpAndEndpoint is a very improtant function designed to be run as a pure function
-- which takes in a request and processes it returning an sql query and it's params which can be executed.
-- The advantage of returning these queries and params is that it becomes possible to group together batches of requests
-- from the google cloud pub sub, and to concatenate their queries so that only a single database call is needed for a batch.
-- Also, being a pure function means it's easier to test the request processing logic since we can unit test pure functions easily.
-- We can pass in a request, it's project cache object and inspect the generated sql and params.
requestMsgToDumpAndEndpoint
  :: Projects.ProjectCache
  -> RequestMessages.RequestMessage
  -> UTCTime
  -> UUID.UUID
  -> Either Text (Maybe RequestDumps.RequestDump, Maybe Endpoints.Endpoint, Maybe Shapes.Shape, V.Vector Fields.Field, V.Vector Formats.Format, V.Vector RequestDumps.ATError)
requestMsgToDumpAndEndpoint pjc rM now dumpIDOriginal = do
  -- TODO: User dumpID and msgID to get correct ID
  let dumpID = fromMaybe dumpIDOriginal rM.msgId
  let timestampUTC = zonedTimeToUTC rM.timestamp

  let method = T.toUpper rM.method
  let urlPath' = RequestDumps.normalizeUrlPath rM.sdkType rM.statusCode rM.method (fromMaybe "/" rM.urlPath)
  let (urlPathDyn, pathParamsDyn, hasDyn) = ensureUrlParams urlPath'
  let (urlPath, pathParams) = if hasDyn then (urlPathDyn, pathParamsDyn) else (urlPath', rM.pathParams)
  let !endpointHash = toXXHash $ UUID.toText rM.projectId <> fromMaybe "" rM.host <> method <> urlPath
  let redactFieldsList = pjc.redactFieldslist V.++ V.fromList [".set-cookie", ".password"]
  let redacted = redactJSON redactFieldsList
  let sanitizeNullChars = encodeUtf8 . replaceNullChars . decodeUtf8
  reqBodyB64 <- B64.decodeBase64Untyped $ encodeUtf8 rM.requestBody
  respBodyB64 <- B64.decodeBase64Untyped $ encodeUtf8 rM.responseBody
  let !reqBody = redacted $ fromRight (AE.object []) $ AE.eitherDecodeStrict $ sanitizeNullChars reqBodyB64
      !respBody = redacted $ fromRight (AE.object []) $ AE.eitherDecodeStrict $ sanitizeNullChars respBodyB64
      !pathParamFields = valueToFields $ redacted rM.pathParams
      !queryParamFields = valueToFields $ redacted rM.queryParams
      !reqHeaderFields = valueToFields $ redacted rM.requestHeaders
      !respHeaderFields = valueToFields $ redacted rM.responseHeaders
      !reqBodyFields = valueToFields reqBody
      !respBodyFields = valueToFields respBody
      !queryParamsKP = V.map fst queryParamFields
      !requestHeadersKP = V.map fst reqHeaderFields
      !responseHeadersKP = V.map fst respHeaderFields
      !requestBodyKP = V.map fst reqBodyFields
      !responseBodyKP = V.map fst respBodyFields

  -- We calculate a hash that represents the request.
  -- We skip the request headers from this hash, since the source of the request like browsers might add or skip headers at will,
  -- which would make this process not deterministic anymore, and that's necessary for a hash.
  let representativeKP = sortVector $ queryParamsKP <> responseHeadersKP <> requestBodyKP <> responseBodyKP
  let !combinedKeyPathStr = T.concat $ V.toList $ representativeKP
  -- Include the endpoint hash and status code to make the shape hash unique by endpoint and status code.
  let !shapeHash = endpointHash <> show rM.statusCode <> toXXHash combinedKeyPathStr
  let projectId = Projects.ProjectId rM.projectId

  let !pathParamsFieldsDTO = V.map (fieldsToFieldDTO Fields.FCPathParam projectId endpointHash) pathParamFields
      !queryParamsFieldsDTO = V.map (fieldsToFieldDTO Fields.FCQueryParam projectId endpointHash) queryParamFields
      !reqHeadersFieldsDTO = V.map (fieldsToFieldDTO Fields.FCRequestHeader projectId endpointHash) reqHeaderFields
      !respHeadersFieldsDTO = V.map (fieldsToFieldDTO Fields.FCResponseHeader projectId endpointHash) respHeaderFields
      !reqBodyFieldsDTO = V.map (fieldsToFieldDTO Fields.FCRequestBody projectId endpointHash) reqBodyFields
      !respBodyFieldsDTO = V.map (fieldsToFieldDTO Fields.FCResponseBody projectId endpointHash) respBodyFields
      !fieldsDTO =
        pathParamsFieldsDTO
          <> queryParamsFieldsDTO
          <> reqHeadersFieldsDTO
          <> respHeadersFieldsDTO
          <> reqBodyFieldsDTO
          <> respBodyFieldsDTO
  let (fields, formats) = V.unzip fieldsDTO
  let !fieldHashes = sortVector $ V.map (.hash) fields
  let !formatHashes = sortVector $ V.map (.hash) formats

  --- FIXME: why are we not using the actual url params?
  -- Since it foes into the endpoint, maybe it should be the keys and their type? I'm unsure.
  -- At the moment, if an endpoint exists, we don't insert it anymore. But then how do we deal with requests from new hosts?
  let urlParams = AET.emptyObject
  let endpoint
        | endpointHash `elem` pjc.endpointHashes = Nothing -- We have the endpoint cache in our db already. Skill adding
        | rM.statusCode == 404 = Nothing
        | otherwise = Just $ buildEndpoint rM now dumpID projectId method urlPath urlParams endpointHash (isRequestOutgoing rM.sdkType)

  let !shape
        | shapeHash `elem` pjc.shapeHashes = Nothing
        | rM.statusCode == 404 = Nothing
        | otherwise =
            -- A shape is a deterministic representation of a request-response combination for a given endpoint.
            -- We usually expect multiple shapes per endpoint. Eg a shape for a success request-response and another for an error response.
            -- Shapes are dependent on the endpoint, statusCode and the unique fields in that shape.
            Just $
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

  -- FIXME: simplify processErrors func
  let !(errorsList, _, _) = V.unzip3 $ V.map (processErrors projectId rM.sdkType rM.method (fromMaybe "" rM.urlPath)) $ V.fromList $ fromMaybe [] rM.errors

  -- request dumps are time series dumps representing each requests which we consume from our users.
  -- We use this field via the log explorer for exploring and searching traffic. And at the moment also use it for most time series analytics.
  -- It's likely a good idea to stop relying on it for some of the time series analysis, to allow us easily support request sampling, but still support
  -- relatively accurate analytic counts.
  -- Build the query and params for inserting a request dump into the database.

  let !reqDumpP =
        RequestDumps.RequestDump
          { id = dumpID
          , createdAt = timestampUTC
          , updatedAt = now
          , projectId = rM.projectId
          , host = fromMaybe "" rM.host
          , urlPath = urlPath
          , rawUrl = rM.rawUrl
          , pathParams = pathParams
          , method = method
          , referer = fromMaybe "" $ rM.referer >>= either Just listToMaybe
          , protoMajor = fromIntegral rM.protoMajor
          , protoMinor = fromIntegral rM.protoMinor
          , duration = calendarTimeTime $ secondsToNominalDiffTime $ fromIntegral rM.duration
          , statusCode = fromIntegral rM.statusCode
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
          , durationNs = fromIntegral rM.duration
          , sdkType = rM.sdkType
          , parentId = rM.parentId
          , serviceVersion = rM.serviceVersion
          , errors = toJSON errorsList
          , tags = maybe V.empty V.fromList rM.tags
          , requestType = RequestDumps.getRequestType rM.sdkType
          }

  -- Build all fields and formats, unzip them as separate lists and append them to query and params
  -- We don't border adding them if their shape exists, as we asume that we've already seen such before.
  let fields'
        -- TODO: Replace this faulty logic with bloom  filter. See comment on formats for more.
        -- \| shapeHash `elem` pjc.shapeHashes = ([], [])
        | rM.statusCode == 404 = V.empty
        | otherwise = fields

  -- FIXME:
  -- Instead of having examples as a column under formats, could we be better served by having an examples table?
  -- How do we solve the problem of updating examples for formats if we never insert the format when the shape already exists?
  -- Should we use some randomizer?
  -- The original plan was that we could skip the shape from the input into this function, but then that would mean
  -- also inserting the fields and the shape, when all we want to insert is just the example.
  let formats'
        -- TODO: Replace this redundancy check with a sort of bit vector or bloom filter that holds all the
        -- existing formats or even just fields in the given project. So we don't insert existing fields
        -- and formats over and over
        -- \| shapeHash `elem` pjc.shapeHashes = ([], [])
        | rM.statusCode == 404 = V.empty
        | otherwise = formats

  Right (Just reqDumpP, endpoint, shape, fields', formats', errorsList)


isRequestOutgoing :: RequestDumps.SDKTypes -> Bool
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
    , host = fromMaybe "" rM.host
    , hash = endpointHash
    , outgoing = outgoing
    , description = "" :: Text
    }


-- valueToFields takes an aeson object and converts it into a vector of paths to
-- each primitive value in the json and the values.
--
-- Regular nested text fields:
-- >>> valueToFields [aesonQQ|{"menu":{"id":"text"}}|]
-- V.fromList [(".menu.id", V.fromList [String "text"])]
--
-- Integer nested field within an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":[{"int_field":22}]}}|]
-- V.fromList [(".menu.id[*].int_field", V.fromList [Number 22.0])]
--
-- Deeper nested field with an array of objects:
-- >>> valueToFields [aesonQQ|{"menu":{"id":{"menuitems":[{"key":"value"}]}}}|]
-- V.fromList [(".menu.id.menuitems[*].key", V.fromList [String "value"])]
--
-- Flat array value:
-- >>> valueToFields [aesonQQ|{"menu":["abc", "xyz"]}|]
-- V.fromList [(".menu[*]", V.fromList [String "abc", String "xyz"])]
--
-- Float values and Null
-- >>> valueToFields [aesonQQ|{"fl":1.234, "nl": null}|]
-- V.fromList [(".fl", V.fromList [Number 1.234]), (".nl", V.fromList [Null])]
--
-- Multiple fields with same key via array:
-- >>> valueToFields [aesonQQ|{"menu":[{"id":"text"},{"id":123}]}|]
-- V.fromList [(".menu[*].id", V.fromList [String "text", Number 123.0])]
--
-- >>> valueToFields [aesonQQ|{"menu":[{"c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd":"text"},{"id":123}]}|]
-- V.fromList [(".menu[*].id", V.fromList [Number 123.0]), (".menu[*].{uuid}", V.fromList [String "text"])]
--
-- FIXME: value To Fields should use the redact fields list to actually redact fields
valueToFields :: AE.Value -> V.Vector (Text, V.Vector AE.Value)
valueToFields value = dedupFields $ removeBlacklistedFields $ snd $ valueToFields' value ("", V.empty)
  where
    valueToFields' :: AE.Value -> (Text, V.Vector (Text, AE.Value)) -> (Text, V.Vector (Text, AE.Value))
    valueToFields' (AE.Object v) (!prefix, !acc) =
      HM.foldlWithKey' folder (prefix, acc) (AEK.toHashMapText v)
      where
        folder (!akkT, !akkL) k val =
          let newPrefix = if T.null akkT then normalizeKey k else akkT <> "." <> normalizeKey k
              (_, newAkkL) = valueToFields' val (newPrefix, akkL)
           in (akkT, newAkkL)
    valueToFields' (AE.Array v) (!prefix, !acc) =
      V.foldl' folder (prefix, acc) v
      where
        folder (!akkT, !akkL) val =
          let (_, newAkkL) = valueToFields' val (akkT <> "[*]", akkL)
           in (akkT, newAkkL)
    valueToFields' v (!prefix, !acc) = (prefix, V.cons (prefix, v) acc)

    normalizeKey :: Text -> Text
    normalizeKey key = case valueToFormatStr key of
      Just result -> "{" <> result <> "}"
      Nothing -> key


-- | Merge all fields in the vector of tuples by the first item in the tuple.
--
-- >>> dedupFields (V.fromList [(".menu[*]", AE.String "xyz"),(".menu[*]", AE.String "abc")])
-- V.fromList [(".menu[*]",V.fromList [AE.String "abc",AE.String "xyz"])]
--
-- >>> dedupFields (V.fromList [(".menu.[*]", AE.String "xyz"),(".menu.[*]", AE.String "abc"),(".menu.[*]", AE.Number 123)])
-- V.fromList [(".menu.[*]",V.fromList [AE.Number 123.0,AE.String "abc",AE.String "xyz"])]
--
-- >>> dedupFields (V.fromList [(".menu.[*].a", AE.String "xyz"),(".menu.[*].b", AE.String "abc"),(".menu.[*].a", AE.Number 123)])
-- V.fromList [(".menu.[*].a",V.fromList [AE.Number 123.0,AE.String "xyz"]),(".menu.[*].b",V.fromList [AE.String "abc"])]
dedupFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, V.Vector AE.Value)
dedupFields fields = runST $ do
  -- Create a mutable hash table
  hashTable <- HT.newSized (V.length fields) :: ST s (HT.HashTable s Text (V.Vector AE.Value))

  -- Populate the hash table
  forM_ fields $ \(k, v) -> do
    existing <- HT.lookup hashTable k
    let newVec = v `V.cons` fromMaybe V.empty existing
    HT.insert hashTable k newVec

  -- Convert the hash table to a list of immutable vectors
  pairs <- HTC.toList hashTable
  V.thaw (V.fromList pairs) >>= V.freeze


-- dedupFields fields = runST $ do
--     hashTable <- HT.new :: ST s (HT.HashTable s Text (V.Vector AE.Value))
--     -- Populate the hash table
--     forM_ fields $ \(k, v) -> do
--         existing <- HT.lookup hashTable k
--         let newVec = v `V.cons` fromMaybe V.empty existing
--         HT.insert hashTable k newVec
--     -- Convert the hash table to an immutable vector of tuples
--     pairs <- HTC.toList hashTable
--     V.thaw (V.fromList pairs) >>= V.freeze

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
--  dedupFields :: [(Text, AE.Value)] -> [(Text, [AE.Value])]
--  dedupFields fields =
--    sortWith fst fields
--      & groupBy (\a b -> fst a == fst b)
--      & map (foldl' (\(_, xs) (a, b) -> (a, b : xs)) ("", []))

-- dedupFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, V.Vector AE.Value)
-- dedupFields input = V.create do
--     let n = V.length input
--     indexMap <- V.thaw $ V.generate n id
--     VA.sortBy  (\i j -> compare (fst $ input V.! i) (fst $ input V.! j)) indexMap
--
--     resultMap <- HM.newSized n
--     V.forM_ indexMap $ \i -> do
--         let (key, value) = input V.! i
--         HM.modify (Just . maybe (V.singleton value) (`V.snoc` value)) key resultMap
--     V.fromList . HM.toList <$> HM.freeze resultMap

-- dedupFields :: [(Text, AE.Value)] -> [(Text, [AE.Value])]
-- dedupFields fields =
--   sortWith fst fields
--     & groupBy (\a b -> fst a == fst b)
--     & map (foldl' (\(_, xs) (a, b) -> (a, b : xs)) ("", []))

-- dedupFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, V.Vector AE.Value)
-- dedupFields fields = force $ V.create do
--   mv <- V.thaw fields -- Create a mutable copy of the input vector
--   VA.sortBy (compare `on` fst) mv -- Sort the mutable vector in-place
--   sorted <- V.freeze mv -- Freeze the sorted vector
--   pure $ V.unfoldr groupAndFold sorted -- Group and fold the sorted vector
--   where
--     groupAndFold v
--       | V.null v = Nothing
--       | otherwise =
--           let (key, value) = V.head v
--               (group, rest) = V.span ((== key) . fst) v
--               values = V.map snd group
--            in Just ((key, values), rest)

-- >>> removeBlacklistedFields [(".menu.password",String "xyz"),(".authorization",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".authorization",String "[REDACTED]")]
--
-- >>> removeBlacklistedFields [(".menu.password",Null),(".regular",String "abc")]
-- [(".menu.password",String "[REDACTED]"),(".regular",String "abc")]
removeBlacklistedFields :: V.Vector (Text, AE.Value) -> V.Vector (Text, AE.Value)
removeBlacklistedFields = V.map \(k, val) ->
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
  | val =~ ([text|^[0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}|] :: Text) = Just "uuid"
  | val =~ ([text|\b[0-9a-fA-F]{24}\b|]) = Just "uuid"
  | val =~ ([text|^(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$|] :: Text) = Just "mm/dd/yyyy"
  | val =~ ([text|^(0[1-9]|1[012])[- -.](0[1-9]|[12][0-9]|3[01])[- -.](19|20)\d\d$|] :: Text) = Just "mm-dd-yyyy"
  | val =~ ([text|^(0[1-9]|1[012])[- ..](0[1-9]|[12][0-9]|3[01])[- ..](19|20)\d\d$|] :: Text) = Just "mm.dd.yyyy"
  | val =~ ([text|^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d+)?(([+-]\d\d:\d\d)|Z)?$|] :: Text) = Just "YYYY-MM-DDThh:mm:ss.sTZD"
  | val =~ ([text|\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b|] :: Text) = Just "ip"
  | otherwise = Nothing


ensureUrlParams :: Text -> (Text, Value, Bool)
ensureUrlParams "" = ("", AE.object [], False)
ensureUrlParams url = (parsedUrl, pathParams, hasDyn)
  where
    (segs, vals) = parseUrlSegments (T.splitOn "/" url) ([], [])
    parsedUrl = T.intercalate "/" segs
    dynSegs = filter (\x -> T.isPrefixOf "{" x) segs
    hasDyn = not $ null dynSegs
    pathParams = buildPathParams dynSegs vals (AE.object [])


parseUrlSegments :: [Text] -> ([Text], [Text]) -> ([Text], [Text])
parseUrlSegments [] parsed = parsed
parseUrlSegments (x : xs) (segs, vals) = case valueToFormatStr x of
  Nothing -> parseUrlSegments xs (segs ++ [x], vals)
  Just v
    | v == "uuid" -> parseUrlSegments xs (addNewSegment segs "uuid", vals ++ [x])
    | v == "mm/dd/yy" || v == "mm-dd-yy" || v == "mm.dd.yyy" -> parseUrlSegments xs (addNewSegment segs "date", vals ++ [x])
    | v == "ip" -> parseUrlSegments xs (addNewSegment segs "ip_address", vals ++ [x])
    | otherwise -> parseUrlSegments xs (addNewSegment segs "number", vals ++ [x])


addNewSegment :: [Text] -> Text -> [Text]
addNewSegment segs seg = newSegs
  where
    catFilter = filter (\x -> T.isPrefixOf ("{" <> seg) x) segs
    pos = length catFilter
    newSeg = if pos > 0 then "{" <> seg <> "_" <> show pos <> "}" else "{" <> seg <> "}"
    newSegs = segs ++ [newSeg]


buildPathParams :: [Text] -> [Text] -> Value -> Value
buildPathParams [] _ acc = acc
buildPathParams _ [] acc = acc
buildPathParams (x : xs) (v : vs) acc = buildPathParams xs vs param
  where
    current = AE.object [AEKey.fromText (T.tail x) AE..= v]
    param = case (acc, current) of
      ((Object a), (Object b)) -> Object $ a <> b
      _ -> acc


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
fieldsToFieldDTO :: Fields.FieldCategoryEnum -> Projects.ProjectId -> Text -> (Text, V.Vector AE.Value) -> (Fields.Field, Formats.Format)
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
        examples = val
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
    fieldType = fromMaybe Fields.FTUnknown $ (V.map aeValueToFieldType val) V.!? 0

    -- field hash is <hash of the endpoint> + <the hash of <field_category><key_path_str><field_type>> (No space or comma between data)
    !fieldHash = endpointHash <> toXXHash (Fields.fieldCategoryEnumToText fieldCategory <> keyPath)
    -- FIXME: We should rethink this value to format logic.
    -- FIXME: Maybe it actually needs machine learning,
    -- FIXME: or maybe it should operate on the entire list, and not just one value.
    format = fromMaybe "" $ (V.map valueToFormat val) V.!? 0
    !formatHash = fieldHash <> toXXHash format
