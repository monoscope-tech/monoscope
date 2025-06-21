{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module RequestMessages (
  RequestMessage (..),
  valueToFormatStr,
  valueToFields,
  redactJSON,
  replaceNullChars,
  fieldsToFieldDTO,
  sortVector,
  ensureUrlParams,
  processErrors,
)
where

import Control.Monad.ST (ST, runST)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.KeyMap qualified as AEK
import Data.Aeson.Types qualified as AET
import Data.HashMap.Strict qualified as HM
import Data.HashTable.Class qualified as HTC
import Data.HashTable.ST.Cuckoo qualified as HT
import Data.Scientific qualified as Scientific
import Data.Text qualified as T
import Data.Time.LocalTime (ZonedTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VA
import Database.PostgreSQL.Simple (Query)
import Deriving.Aeson qualified as DAE
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Fields.Types qualified as Fields (
  Field (..),
  FieldCategoryEnum (..),
  FieldId (FieldId),
  FieldTypes (..),
  fieldCategoryEnumToText,
 )
import Models.Apis.Formats qualified as Formats
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Relude
import Relude.Unsafe as Unsafe (read)
import Text.Regex.TDFA ((=~))
import Utils (DBField (), toXXHash)


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
  deriving stock (Generic, Show)
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
redactJSON :: V.Vector Text -> AE.Value -> AE.Value
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


-- | Process errors with optional HTTP-specific fields
-- If HTTP fields are not provided, they remain as Nothing in the error record
processErrors :: Projects.ProjectId -> Maybe RequestDumps.SDKTypes -> Maybe Text -> Maybe Text -> RequestDumps.ATError -> (RequestDumps.ATError, Query, [DBField])
processErrors pid maybeSdkType maybeMethod maybePath err = (normalizedError, q, params)
  where
    (q, params) = Anomalies.insertErrorQueryAndParams pid normalizedError
    normalizedError =
      err
        { RequestDumps.projectId = Just pid
        , RequestDumps.hash = Just $ fromMaybe defaultHash err.hash
        , RequestDumps.technology = maybeSdkType <|> err.technology
        , RequestDumps.requestMethod = maybeMethod <|> err.requestMethod
        , RequestDumps.requestPath = maybePath <|> err.requestPath
        }
    defaultHash = toXXHash (pid.toText <> err.errorType <> formattedMessage <> maybe "" show maybeSdkType)
    formattedMessage = fromMaybe err.message (valueToFormatStr err.message)


sortVector :: Ord a => V.Vector a -> V.Vector a
sortVector vec = runST $ do
  mvec <- V.thaw vec
  VA.sort mvec
  V.freeze mvec


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
  | val =~ ([text|^[0-9]+$|] :: Text) =
      Just "integer" -- e.g. "12345"
  | val =~ ([text|^[+-]?(\d+(\.\d*)?|\.\d+)$|] :: Text) =
      Just "float" -- e.g. "3.1415"
  | val =~ ([text|^0x[0-9A-Fa-f]+$|] :: Text) =
      Just "hex_integer" -- e.g. "0xDEADBEEF"
  | val =~ ([text|\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\b|] :: Text) =
      Just "uuid" -- e.g. "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
  | val =~ ([text|\b[0-9a-fA-F]{24}\b|] :: Text) =
      Just "mongo_oid" -- e.g. "507f1f77bcf86cd799439011"
  | val =~ ([text|\b[a-fA-F0-9]{32}\b|] :: Text) =
      Just "md5" -- e.g. "d41d8cd98f00b204e9800998ecf8427e"
  | val =~ ([text|\b[a-fA-F0-9]{40}\b|] :: Text) =
      Just "sha1" -- e.g. "da39a3ee5e6b4b0d3255bfef95601890afd80709"
  | val =~ ([text|\b[a-fA-F0-9]{64}\b|] :: Text) =
      Just "sha256" -- e.g. "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  | val =~ ([text|(?<=\s|^)[A-Za-z0-9+/]{20,}={0,2}(?=\s|$)|] :: Text) =
      Just "base64" -- e.g. "SGVsbG8gV29ybGQh"
  | val =~ ([text|\beyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\b|] :: Text) =
      Just "jwt" -- e.g. "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
  | val =~ ([text|\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b|] :: Text) =
      Just "email" -- e.g. "foo.bar@example.com"
  | val =~ ([text|\bhttps?://[^\s/$.?#].[^\s]*\b|] :: Text) =
      Just "url" -- e.g. "https://example.com/path?query=1"
  | val =~ ([text|\b(?:[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?\.)+[A-Za-z]{2,}\b|] :: Text) =
      Just "hostname" -- e.g. "my-server.local"
  | val =~ ([text|\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}\b|] :: Text) =
      Just "ipv4" -- e.g. "192.168.0.1"
  | val =~ ([text|\b([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}\b|] :: Text) =
      Just "ipv6" -- e.g. "fe80::1ff:fe23:4567:890a"
  | val =~ ([text|\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}/([0-9]|[12][0-9]|3[0-2])\b|] :: Text) =
      Just "cidr4" -- e.g. "10.0.0.0/24"
  | val =~ ([text|\b([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}\b|] :: Text) =
      Just "mac" -- e.g. "01:23:45:67:89:ab"
  | val =~ ([text|:\d{1,5}\b|] :: Text) =
      Just "port" -- e.g. ":8080"
  | val =~ ([text|\b[1-5][0-9]{2}\b|] :: Text) =
      Just "http_status" -- e.g. "404"
  | val =~ ([text|(?:/[A-Za-z0-9._-]+)+/?|] :: Text) =
      Just "file_path_unix" -- e.g. "/usr/local/bin/script.sh"
  | val =~ ([text|[A-Za-z]:\\(?:[^\\/:*?"<>|\r\n]+\\)*[^\\/:*?"<>|\r\n]*|] :: Text) =
      Just "file_path_windows" -- e.g. "C:\\Program Files\\App\\app.exe"
  | val =~ ([text|\b(?:4[0-9]{12}(?:[0-9]{3})?|5[1-5][0-9]{14}|3[47][0-9]{13})\b|] :: Text) =
      Just "credit_card" -- e.g. "4111111111111111"
  | val =~ ([text|\b[A-Z]{2}[0-9]{2}[A-Za-z0-9]{4}[0-9]{7}(?:[A-Za-z0-9]?){0,16}\b|] :: Text) =
      Just "iban" -- e.g. "DE89370400440532013000"
  | val =~ ([text|\b\d{3}-\d{2}-\d{4}\b|] :: Text) =
      Just "ssn_us" -- e.g. "123-45-6789"
  | val =~ ([text|\b\+?[0-9]{1,3}[-.\s]?(\([0-9]{1,4}\)|[0-9]{1,4})[-.\s]?[0-9]{1,4}[-.\s]?[0-9]{1,9}\b|] :: Text) =
      Just "phone" -- e.g. "+1-800-555-1234"
  | val =~ ([text|\b(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun),\s\d{1,2}\s(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s\d{4}\s\d{2}:\d{2}:\d{2}\s[+\-]\d{4}\b|] :: Text) =
      Just "rfc2822_date" -- e.g. "Fri, 21 Nov 1997 09:55:06 -0600"
  | val =~ ([text|\b\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+\-]\d{2}:\d{2})?\b|] :: Text) =
      Just "iso8601" -- e.g. "2023-10-14T10:29:38.64522Z"
  | val =~ ([text|\b1[0-9]{12}\b|] :: Text) =
      Just "epoch_ms" -- e.g. "1621255805123"
  | val =~ ([text|\b1[0-9]{9}\b|] :: Text) =
      Just "epoch_s" -- e.g. "1621255805"
  | val =~ ([text|\b\d+(:\d{2}){1,2}(?:\.\d+)?\b|] :: Text) =
      Just "duration" -- e.g. "01:02:03.456"
  | val =~ ([text|^\s*at\s[^\s]+\([^\)]+:\d+\)\b|] :: Text) =
      Just "java_stack" -- e.g. "at com.foo.Bar.method(Bar.java:123)"
  | val =~ ([text|\bpid[:=]?\d+\b|] :: Text) =
      Just "pid" -- e.g. "pid=1234"
  | val =~ ([text|\btid[:=]?\d+\b|] :: Text) =
      Just "tid" -- e.g. "tid:5678"
  | val =~ ([text|\bThread-\d+\b|] :: Text) =
      Just "thread" -- e.g. "Thread-1"
  | val =~ ([text|\bsession_[A-Za-z0-9\-]{8,}\b|] :: Text) =
      Just "session_id" -- e.g. "session_abcdef12"
  | otherwise =
      Nothing


ensureUrlParams :: Text -> (Text, AE.Value, Bool)
ensureUrlParams "" = ("", AE.object [], False)
ensureUrlParams url = (parsedUrl, pathParams, hasDyn)
  where
    (segs, vals) = parseUrlSegments (T.splitOn "/" url) ([], [])
    parsedUrl = T.intercalate "/" segs
    dynSegs = filter (T.isPrefixOf "{") segs
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
    catFilter = filter (T.isPrefixOf ("{" <> seg)) segs
    pos = length catFilter
    newSeg = if pos > 0 then "{" <> seg <> "_" <> show pos <> "}" else "{" <> seg <> "}"
    newSegs = segs ++ [newSeg]


buildPathParams :: [Text] -> [Text] -> AE.Value -> AE.Value
buildPathParams [] _ acc = acc
buildPathParams _ [] acc = acc
buildPathParams (x : xs) (v : vs) acc = buildPathParams xs vs param
  where
    current = AE.object [AEKey.fromText (T.tail x) AE..= v]
    param = case (acc, current) of
      (AE.Object a, AE.Object b) -> AE.Object $ a <> b
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
    fieldType = fromMaybe Fields.FTUnknown $ V.map aeValueToFieldType val V.!? 0

    -- field hash is <hash of the endpoint> + <the hash of <field_category><key_path_str><field_type>> (No space or comma between data)
    !fieldHash = endpointHash <> toXXHash (Fields.fieldCategoryEnumToText fieldCategory <> keyPath)
    -- FIXME: We should rethink this value to format logic.
    -- FIXME: Maybe it actually needs machine learning,
    -- FIXME: or maybe it should operate on the entire list, and not just one value.
    format = fromMaybe "" $ V.map valueToFormat val V.!? 0
    !formatHash = fieldHash <> toXXHash format
