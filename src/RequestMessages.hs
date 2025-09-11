{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- \$setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import qualified Data.Aeson as AE
-- >>> import qualified Data.Vector as V

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
  replaceAllFormats,
  dedupFields,
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
import Relude
import Relude.Unsafe as Unsafe (read)
import Text.RE.Replace (matched)
import Text.RE.TDFA (RE, SearchReplace, ed, re, (*=~/), (=~), (?=~))
import Utils (DBField (), toXXHash)


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as V
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson
-- >>> import Data.Aeson qualified as AE
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes


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
--
-- >>> redactJSON (V.fromList ["menu.id."]) [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
--
-- >>> redactJSON (V.fromList ["menu.id"]) [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "John")]))])
--
-- >>> redactJSON (V.fromList ["menu.id", "menu.name"]) [aesonQQ| {"menu":{"id":"file", "name":"John"}} |]
-- Object (fromList [("menu",Object (fromList [("id",String "[REDACTED]"),("name",String "[REDACTED]")]))])
--
-- >>> redactJSON (V.fromList ["menu.[].id", "menu.[].names.[]"]) [aesonQQ| {"menu":[{"id":"i1", "names":["John","okon"]}, {"id":"i2"}]} |]
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
    defaultHash = toXXHash (pid.toText <> fromMaybe "" err.serviceName <> err.errorType <> replaceAllFormats (err.message <> err.stackTrace) <> maybe "" show maybeSdkType)


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
    normalizeKey key =
      let formatted = replaceAllFormats key
       in if formatted == key
            then fromMaybe key (valueToFormatStr key)
            else formatted


-- | Merge all fields in the vector of tuples by the first item in the tuple.
--
-- >>> dedupFields (V.fromList [(".menu[*]", AE.String "xyz"),(".menu[*]", AE.String "abc")])
-- [(".menu[*]",[String "abc",String "xyz"])]
--
-- >>> dedupFields (V.fromList [(".menu.[*]", AE.String "xyz"),(".menu.[*]", AE.String "abc"),(".menu.[*]", AE.Number 123)])
-- [(".menu.[*]",[Number 123.0,String "abc",String "xyz"])]
--
-- >>> dedupFields (V.fromList [(".menu.[*].a", AE.String "xyz"),(".menu.[*].b", AE.String "abc"),(".menu.[*].a", AE.Number 123)])
-- [(".menu.[*].b",[String "abc"]),(".menu.[*].a",[Number 123.0,String "xyz"])]
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
-- "mm/dd/yyyy"
--
valueToFormat :: AE.Value -> Text
valueToFormat (AET.String val) = case valueToFormatStr val of
  Just fmt -> T.drop 1 $ T.dropEnd 1 fmt -- Remove the curly braces
  Nothing -> "text"
valueToFormat (AET.Number val) = valueToFormatNum val
valueToFormat (AET.Bool _) = "bool"
valueToFormat AET.Null = "null"
valueToFormat (AET.Object _) = "object"
valueToFormat (AET.Array _) = "array"


-- | Common format patterns used by both replaceAllFormats and valueToFormatStr
-- The order matters: more specific patterns should come before more general ones
commonFormatPatterns :: [(RE, Text)]
commonFormatPatterns =
  [ -- UUIDs and hashes (most specific hex patterns first)
    ([re|^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$|], "{uuid}")
  , ([re|^[0-9a-fA-F]{24}$|], "{uuid}") -- Keep as uuid for backward compatibility
  , ([re|^[a-fA-F0-9]{64}$|], "{sha256}")
  , ([re|^[a-fA-F0-9]{40}$|], "{sha1}")
  , ([re|^[a-fA-F0-9]{32}$|], "{md5}")
  , -- Financial (moved up before hex_id to catch credit cards first)
    ([re|^4[0-9]{15}$|], "{credit_card}") -- Visa 16 digits
  , ([re|^4[0-9]{12}$|], "{credit_card}") -- Visa 13 digits
  , ([re|^5[1-5][0-9]{14}$|], "{credit_card}") -- Mastercard
  , ([re|^3[47][0-9]{13}$|], "{credit_card}") -- Amex
  , ([re|^[0-9A-Fa-f]{14,20}$|], "{hex_id}") -- Match hex-like IDs that aren't UUIDs
  -- Authentication & encoding
  , ([re|^eyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+$|], "{jwt}")
  , -- IBAN (moved before base64)
    ([re|^[A-Z]{2}[0-9]{2}[A-Za-z0-9]{4}[0-9]{7}[A-Za-z0-9]{0,16}$|], "{iban}")
  , ([re|^[A-Za-z0-9+/]{20,}={0,2}$|], "{base64}")
  , -- Date patterns (before file paths to avoid conflicts)
    ([re|^(Mon|Tue|Wed|Thu|Fri|Sat|Sun), [0-9]{1,2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} [+\-][0-9]{4}$|], "{rfc2822}")
  , ([re|^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?(Z|[+\-][0-9]{2}:[0-9]{2})?$|], "{YYYY-MM-DDThh:mm:ss.sTZD}") -- ISO 8601
  , ([re|^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{YYYY-MM-DD HH:MM:SS}") -- MySQL datetime
  , ([re|^[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{MM/DD/YYYY HH:MM:SS}") -- US datetime
  , ([re|^[0-9]{2}-[0-9]{2}-[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{MM-DD-YYYY HH:MM:SS}")
  , ([re|^[0-9]{2}\.[0-9]{2}\.[0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{DD.MM.YYYY HH:MM:SS}") -- European datetime
  , ([re|^[0-9]{4}/[0-9]{2}/[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{YYYY/MM/DD HH:MM:SS}") -- Japanese datetime
  , ([re|^(0[1-9]|[12][0-9]|3[01])[/](0[1-9]|1[012])[/](19|20)[0-9][0-9]$|], "{dd/mm/yyyy}") -- European date
  , ([re|^(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[012])-(19|20)[0-9][0-9]$|], "{dd-mm-yyyy}")
  , ([re|^(0[1-9]|[12][0-9]|3[01])\.(0[1-9]|1[012])\.(19|20)[0-9][0-9]$|], "{dd.mm.yyyy}")
  , ([re|^(0[1-9]|1[012])[/](0[1-9]|[12][0-9]|3[01])[/](19|20)[0-9][0-9]$|], "{mm/dd/yyyy}") -- US date
  , ([re|^(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])-(19|20)[0-9][0-9]$|], "{mm-dd-yyyy}")
  , ([re|^(0[1-9]|1[012])\.(0[1-9]|[12][0-9]|3[01])\.(19|20)[0-9][0-9]$|], "{mm.dd.yyyy}")
  , ([re|^[0-9]{4}-[0-9]{2}-[0-9]{2}$|], "{YYYY-MM-DD}") -- ISO date
  , ([re|^[0-9]{4}/[0-9]{2}/[0-9]{2}$|], "{YYYY/MM/DD}") -- Japanese date
  , ([re|^[0-9]{8}$|], "{YYYYMMDD}") -- Compact date
  , ([re|^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) [0-9]{1,2}, [0-9]{4}$|], "{Mon DD, YYYY}") -- Long month
  , ([re|^[0-9]{1,2}-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-[0-9]{4}$|], "{DD-Mon-YYYY}") -- Oracle date
  -- Time patterns
  , ([re|^[0-9]{2}:[0-9]{2}:[0-9]{2}\.[0-9]{3}$|], "{HH:MM:SS.mmm}") -- Time with milliseconds
  , ([re|^[0-9]{2}:[0-9]{2}:[0-9]{2}$|], "{HH:MM:SS}") -- Time only
  , ([re|^[0-9]{1,2}:[0-9]{2} (AM|PM|am|pm)$|], "{H:MM AM/PM}") -- 12-hour time
  -- Personal identifiers
  , ([re|^[0-9]{3}-[0-9]{2}-[0-9]{4}$|], "{ssn}")
  , ([re|^\+1 \([0-9]{3}\) [0-9]{3}-[0-9]{4}$|], "{phone}")
  , -- Network patterns
    ([re|^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)/([0-9]|[12][0-9]|3[0-2])$|], "{cidr}")
  , ([re|^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$|], "{ipv4}")
  , ([re|^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$|], "{email}")
  , ([re|^https?://[^\s]+$|], "{url}")
  , ([re|^([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}$|], "{ipv6}")
  , ([re|^([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}$|], "{mac}")
  , ([re|^:[0-9]{1,5}$|], "{port}")
  , ([re|^[A-Za-z0-9][A-Za-z0-9.-]*\.[A-Za-z]{2,}$|], "{hostname}")
  , -- File paths (after dates to avoid conflicts)
    ([re|^[A-Za-z]:\\\\.*$|], "{file_path}") -- Windows path with double backslash
  , ([re|^[A-Za-z]:\\.*$|], "{file_path}") -- Windows path with single backslash
  , ([re|^/[A-Za-z0-9._/-]+$|], "{file_path}") -- Unix path
  , -- Timestamps
    ([re|^1[0-9]{12}$|], "{epoch_ms}")
  , ([re|^1[0-9]{9}$|], "{epoch_s}")
  , -- Process/thread identifiers
    ([re|^pid[:=]?[0-9]+$|], "{pid}")
  , ([re|^tid[:=]?[0-9]+$|], "{tid}")
  , ([re|^Thread-[0-9]+$|], "{thread}")
  , ([re|^session_[A-Za-z0-9\-]{8,}$|], "{session_id}")
  , -- Numbers (last, as they're most general)
    ([re|^[1-5][0-9]{2}$|], "{http_status}")
  , ([re|^0x[0-9A-Fa-f]+$|], "{hex}")
  , ([re|^[+-]?[0-9]+\.[0-9]+$|], "{float}")
  , ([re|^[0-9]+$|], "{integer}")
  ]


-- | Replaces all format patterns in the input text with their format names
-- This function applies all regex patterns and replaces matches until no more replacements are made
--
-- >>> map replaceAllFormats ["123", "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd", "550e8400-e29b-41d4-a716-446655440000", "507f1f77bcf86cd799439011"]
-- ["{integer}","{uuid}","{uuid}","{uuid}"]
--
-- >>> map replaceAllFormats ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "356a192b7913b04c54574d18c28d46e6395428ab", "5d41402abc4b2a76b9719d911017c592"]
-- ["{sha256}","{sha1}","{md5}"]
--
-- >>> replaceAllFormats "User 123 accessed endpoint c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
-- "User {integer} accessed endpoint {uuid}"
--
-- >>> replaceAllFormats "Error at 192.168.0.1:8080 with status 404"
-- "Error at {ipv4}{port} with status {integer}"
--
-- >>> map replaceAllFormats ["Server on :8080", "localhost:3000", "api.com:443", ":22"]
-- ["Server on {port}","localhost{port}","api.com{port}","{port}"]
--
-- >>> replaceAllFormats "Connected to 10.0.0.1:443 and 192.168.1.100:22"
-- "Connected to {ipv4}{port} and {ipv4}{port}"
--
-- >>> replaceAllFormats "Responses: 200, 301, 404, 500"
-- "Responses: {integer}, {integer}, {integer}, {integer}"
--
-- >>> replaceAllFormats "GET /api/v2/users/123/orders/456 returned 200"
-- "GET /api/v{integer}/users/{integer}/orders/{integer} returned {integer}"
--
-- >>> replaceAllFormats "User 550e8400-e29b-41d4-a716-446655440000 connected from 192.168.1.50:9876"
-- "User {uuid} connected from {ipv4}{port}"
--
-- >>> replaceAllFormats "Hash: a94a8fe5ccb19ba61c4c0873d391e987982fbbd3, Status: 403, Port: :8443"
-- "Hash: {sha1}, Status: {integer}, Port: {port}"
--
-- >>> replaceAllFormats "Processing request 123456 at 2023-10-15:3000"
-- "Processing request {integer} at {integer}-{integer}-{integer}{port}"
--
-- >>> map replaceAllFormats ["Mixed: 192.168.1.1 123 :80 404", "0xDEADBEEF", "Values: 999, 1000, 1001"]
-- ["Mixed: {ipv4} {integer} {port} {integer}","{hex}","Values: {integer}, {integer}, {integer}"]
--
-- >>> map replaceAllFormats ["10.0.0.1", "172.16.0.1", "192.168.1.1", "255.255.255.0"]
-- ["{ipv4}","{ipv4}","{ipv4}","{ipv4}"]
--
-- >>> map replaceAllFormats ["Multiple formats: 123 abc def456789 :9000", "Log entry 404 at 10.0.0.1:8080"]
-- ["Multiple formats: {integer} abc def{integer} {port}","Log entry {integer} at {ipv4}{port}"]
replaceAllFormats :: Text -> Text
replaceAllFormats input = restorePlaceholders $ processPatterns input formatPatternsForReplacement
  where
    -- Process patterns sequentially with (*=~/)
    -- Use special Unicode characters as temporary placeholders to prevent re-matching
    processPatterns :: Text -> [SearchReplace RE Text] -> Text
    processPatterns txt [] = txt
    processPatterns txt (sr : rest) =
      let newTxt = txt *=~/ sr
       in processPatterns newTxt rest

    -- Restore the Unicode placeholders to the final format
    restorePlaceholders :: Text -> Text
    restorePlaceholders txt =
      txt
        *=~/ [ed|〖×UUID×〗///{uuid}|]
        *=~/ [ed|〖×SHA-TWO-FIVE-SIX×〗///{sha256}|]
        *=~/ [ed|〖×SHA-ONE×〗///{sha1}|]
        *=~/ [ed|〖×MD-FIVE×〗///{md5}|]
        *=~/ [ed|〖×IPV-FOUR×〗///{ipv4}|]
        *=~/ [ed|〖×PORT×〗///{port}|]
        *=~/ [ed|〖×HEX×〗///{hex}|]
        *=~/ [ed|〖×INTEGER×〗///{integer}|]

    -- Use a subset of patterns that should be replaced in text
    -- Using Unicode characters with no alphanumeric content that won't match any patterns
    formatPatternsForReplacement :: [SearchReplace RE Text]
    formatPatternsForReplacement =
      [ -- UUIDs and hashes first (most specific)
        [ed|[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}///〖×UUID×〗|]
      , [ed|[a-fA-F0-9]{64}///〖×SHA-TWO-FIVE-SIX×〗|]
      , [ed|[a-fA-F0-9]{40}///〖×SHA-ONE×〗|]
      , [ed|[a-fA-F0-9]{32}///〖×MD-FIVE×〗|]
      , [ed|[0-9a-fA-F]{24}///〖×UUID×〗|]
      , -- Network patterns
        [ed|(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)///〖×IPV-FOUR×〗|]
      , [ed|:[0-9]{1,5}///〖×PORT×〗|]
      , -- Hex numbers (must come before general integers)
        [ed|0x[0-9A-Fa-f]+///〖×HEX×〗|]
      , -- Numbers (including HTTP status codes)
        [ed|[0-9]+///〖×INTEGER×〗|]
      ]


-- valueToFormatStr will take a string and try to find a format which matches that string best.
-- At the moment it takes a text and returns a generic mask that represents the format of that text
--
-- >>> map valueToFormatStr ["22/02/2022", "20-02-2022", "22.02.2022", "2023-10-14", "2023/10/14", "20231014"]
-- [Just "{dd/mm/yyyy}",Just "{dd-mm-yyyy}",Just "{dd.mm.yyyy}",Just "{YYYY-MM-DD}",Just "{YYYY/MM/DD}",Just "{YYYYMMDD}"]
--
-- >>> map valueToFormatStr ["Oct 14, 2023", "14-Oct-2023", "2023-10-14T10:29:38.64522Z", "2023-10-14 10:29:38"]
-- [Just "{Mon DD, YYYY}",Just "{DD-Mon-YYYY}",Just "{YYYY-MM-DDThh:mm:ss.sTZD}",Just "{YYYY-MM-DD HH:MM:SS}"]
--
-- >>> map valueToFormatStr ["15:30:45", "15:30:45.123", "3:30 PM", "Mon, 14 Oct 2023 10:30:00 +0000"]
-- [Just "{HH:MM:SS}",Just "{HH:MM:SS.mmm}",Just "{H:MM AM/PM}",Just "{rfc2822}"]
--
-- >>> map valueToFormatStr ["c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd", "507f1f77bcf86cd799439011", "abc123def456789"]
-- [Just "{uuid}",Just "{uuid}",Nothing]
--
-- >>> map valueToFormatStr ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", "356a192b7913b04c54574d18c28d46e6395428ab", "5d41402abc4b2a76b9719d911017c592"]
-- [Just "{sha256}",Just "{sha1}",Just "{md5}"]
--
-- >>> map valueToFormatStr ["123", "123.456", "-123.456", "0xDEADBEEF", "1634567890", "1634567890123"]
-- [Just "{integer}",Just "{float}",Just "{float}",Just "{hex}",Just "{epoch_s}",Just "{epoch_ms}"]
--
-- >>> map valueToFormatStr ["192.168.1.1", "192.168.1.0/24", "2001:0db8:85a3:0000:0000:8a2e:0370:7334", "00:1B:44:11:3A:B7"]
-- [Just "{ipv4}",Just "{cidr}",Just "{ipv6}",Just "{mac}"]
--
-- >>> map valueToFormatStr [":8080", "user@example.com", "https://api.example.com/v1/users", "api.example.com"]
-- [Just "{port}",Just "{email}",Just "{url}",Just "{hostname}"]
--
-- >>> map valueToFormatStr ["eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c", "dGVzdCBiYXNlNjQgZW5jb2Rpbmc="]
-- [Just "{jwt}",Just "{base64}"]
--
-- >>> map valueToFormatStr ["/usr/local/bin/script.sh", "C:\\Users\\Admin\\Documents", "pid:12345", "tid:67890"]
-- [Just "{file_path}",Just "{file_path}",Just "{pid}",Just "{tid}"]
--
-- >>> map valueToFormatStr ["Thread-42", "session_abc123def456", "4111111111111111", "GB82WEST12345698765432"]
-- [Just "{thread}",Just "{session_id}",Just "{credit_card}",Just "{iban}"]
--
-- >>> map valueToFormatStr ["123-45-6789", "+1 (555) 123-4567", "200", "404", "500"]
-- [Just "{ssn}",Just "{phone}",Just "{http_status}",Just "{http_status}",Just "{http_status}"]
--
-- >>> map valueToFormatStr ["plain text", "", "hello world", "test123test"]
-- [Nothing,Nothing,Nothing,Nothing]
--
-- >>> map valueToFormatStr ["v1"]
--
valueToFormatStr :: Text -> Maybe Text
valueToFormatStr val = checkFormats formatChecks
  where
    checkFormats :: [(RE, Text)] -> Maybe Text
    checkFormats [] = Nothing
    checkFormats ((regex, format) : rest) =
      if matched (val ?=~ regex)
        then Just format
        else checkFormats rest

    -- Use exact match patterns first, then fallback to common patterns
    formatChecks :: [(RE, Text)]
    formatChecks = commonFormatPatterns


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
    | v == "{uuid}" -> parseUrlSegments xs (addNewSegment segs "uuid", vals ++ [x])
    | v
        `elem` [ "{mm/dd/yyyy}"
               , "{mm-dd-yyyy}"
               , "{mm.dd.yyyy}"
               , "{dd/mm/yyyy}"
               , "{dd-mm-yyyy}"
               , "{dd.mm.yyyy}"
               , "{YYYY-MM-DD}"
               , "{YYYY/MM/DD}"
               , "{YYYYMMDD}"
               , "{YYYY-MM-DDThh:mm:ss.sTZD}"
               ] ->
        parseUrlSegments xs (addNewSegment segs "date", vals ++ [x])
    | v `elem` ["{ip}", "{ipv6}"] -> parseUrlSegments xs (addNewSegment segs "ip_address", vals ++ [x])
    | v `elem` ["{integer}", "{float}", "{hex}"] -> parseUrlSegments xs (addNewSegment segs "number", vals ++ [x])
    | otherwise -> parseUrlSegments xs (addNewSegment segs "param", vals ++ [x])


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
