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
import Data.List qualified as L
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
import Text.RE.Replace (Match, capture, matched, replaceAll)
import Text.RE.TDFA (RE, re, (*=~), (?=~))
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
  [ ([re|\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\b|], "{uuid}")
  , ([re|\b[0-9a-fA-F]{24}\b|], "{uuid}") -- Keep as uuid for backward compatibility
  , ([re|\b[a-fA-F0-9]{64}\b|], "{sha256}")
  , ([re|\b[a-fA-F0-9]{40}\b|], "{sha1}")
  , ([re|\b[a-fA-F0-9]{32}\b|], "{md5}")
  , ([re|\b[0-9A-Fa-f]{14,20}\b|], "{hex_id}") -- Match hex-like IDs that aren't UUIDs (common in DB-generated keys)
  , ([re|\b[A-Za-z0-9+/]{20,}={0,2}\b|], "{base64}")
  , ([re|\beyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\b|], "{jwt}")
  , ([re|\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b|], "{email}")
  , ([re|\bhttps?://[^\s/$.?#].[^\s]*\b|], "{url}")
  , ([re|\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}/([0-9]|[12][0-9]|3[0-2])\b|], "{cidr}")
  , ([re|\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}\b|], "{ipv4}") -- Changed to ipv4 for consistency
  , ([re|\b([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4}\b|], "{ipv6}")
  , ([re|\b([0-9A-Fa-f]{2}[:-]){5}[0-9A-Fa-f]{2}\b|], "{mac}")
  , ([re|:\d{1,5}\b|], "{port}")
  , ([re|\b[1-5][0-9]{2}\b|], "{http_status}")
  , ([re|[A-Za-z]:\\[^\\/:*?"<>|\r\n]*|], "{file_path}")
  , ([re|/[A-Za-z0-9._/-]+|], "{file_path}")
  , ([re|\b(4[0-9]{12}([0-9]{3})?|5[1-5][0-9]{14}|3[47][0-9]{13})\b|], "{credit_card}")
  , ([re|\b[A-Z]{2}[0-9]{2}[A-Za-z0-9]{4}[0-9]{7}[A-Za-z0-9]{0,16}\b|], "{iban}")
  , ([re|\b\d{3}-\d{2}-\d{4}\b|], "{ssn}")
  , ([re|\b\+?[0-9]{1,3}[-.\s]?(\([0-9]{1,4}\)|[0-9]{1,4})[-.\s]?[0-9]{1,4}[-.\s]?[0-9]{1,9}\b|], "{phone}")
  , ([re|\b(Mon|Tue|Wed|Thu|Fri|Sat|Sun),\s\d{1,2}\s(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s\d{4}\s\d{2}:\d{2}:\d{2}\s[+\-]\d{4}\b|], "{rfc2822}")
  , ([re|\b\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+\-]\d{2}:\d{2})?\b|], "{YYYY-MM-DDThh:mm:ss.sTZD}") -- ISO 8601
  , ([re|\b\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\b|], "{YYYY-MM-DD HH:MM:SS}") -- MySQL datetime
  , ([re|\b\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}\b|], "{MM/DD/YYYY HH:MM:SS}") -- US datetime
  , ([re|\b\d{2}-\d{2}-\d{4} \d{2}:\d{2}:\d{2}\b|], "{MM-DD-YYYY HH:MM:SS}")
  , ([re|\b\d{2}\.\d{2}\.\d{4} \d{2}:\d{2}:\d{2}\b|], "{DD.MM.YYYY HH:MM:SS}") -- European datetime
  , ([re|\b\d{4}/\d{2}/\d{2} \d{2}:\d{2}:\d{2}\b|], "{YYYY/MM/DD HH:MM:SS}") -- Japanese datetime
  , ([re|\b(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$|], "{mm/dd/yyyy}") -- US date
  , ([re|\b(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])-(19|20)\d\d\b|], "{mm-dd-yyyy}")
  , ([re|\b(0[1-9]|1[012])\.(0[1-9]|[12][0-9]|3[01])\.(19|20)\d\d\b|], "{mm.dd.yyyy}")
  , ([re|\b(0[1-9]|[12][0-9]|3[01])[- /.](0[1-9]|1[012])[- /.](19|20)\d\d\b|], "{dd/mm/yyyy}") -- European date
  , ([re|\b(0[1-9]|[12][0-9]|3[01])-(0[1-9]|1[012])-(19|20)\d\d\b|], "{dd-mm-yyyy}")
  , ([re|\b(0[1-9]|[12][0-9]|3[01])\.(0[1-9]|1[012])\.(19|20)\d\d\b|], "{dd.mm.yyyy}")
  , ([re|\b\d{4}-\d{2}-\d{2}\b|], "{YYYY-MM-DD}") -- ISO date
  , ([re|\b\d{4}/\d{2}/\d{2}\b|], "{YYYY/MM/DD}") -- Japanese date
  , ([re|\b\d{8}\b|], "{YYYYMMDD}") -- Compact date
  , ([re|\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec) \d{1,2}, \d{4}\b|], "{Mon DD, YYYY}") -- Long month
  , ([re|\b\d{1,2}-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\d{4}\b|], "{DD-Mon-YYYY}") -- Oracle date
  , ([re|\b\d{2}:\d{2}:\d{2}\.\d{3}\b|], "{HH:MM:SS.mmm}") -- Time with milliseconds
  , ([re|\b\d{2}:\d{2}:\d{2}\b|], "{HH:MM:SS}") -- Time only
  , ([re|\b\d{1,2}:\d{2} (AM|PM|am|pm)\b|], "{H:MM AM/PM}") -- 12-hour time
  , ([re|\b1[0-9]{12}\b|], "{epoch_ms}")
  , ([re|\b1[0-9]{9}\b|], "{epoch_s}")
  , ([re|\b\d+(:\d{2}){1,2}(\.\d+)?\b|], "{duration}")
  , ([re|^\s*at\s[^\s]+\([^\)]+:\d+\)\b|], "{stack_trace}")
  , ([re|\bpid[:=]?\d+\b|], "{pid}")
  , ([re|\btid[:=]?\d+\b|], "{tid}")
  , ([re|\bThread-\d+\b|], "{thread}")
  , ([re|\bsession_[A-Za-z0-9\-]{8,}\b|], "{session_id}")
  , ([re|\b[A-Za-z0-9][A-Za-z0-9.-]*\.[A-Za-z]{2,}\b|], "{hostname}")
  , ([re|\b0x[0-9A-Fa-f]+\b|], "{hex}")
  , ([re|\b[+-]?\d+\.\d+\b|], "{float}") -- More specific float pattern
  , ([re|\b[0-9]+\b|], "{integer}")
  , ([re|'[^']{1,100}'|], "{quoted_value}") -- Match single-quoted values: 'value'
  , ([re|"[^"]{1,100}"|], "{quoted_value}") -- Match double-quoted values: "value"
  , ([re|`[^`]{1,100}`|], "{quoted_value}") -- Match backtick-quoted values: `value`
  ]


-- | Replaces all format patterns in the input text with their format names
-- This function applies all regex patterns and replaces matches until no more replacements are made
--
-- >>> replaceAllFormats "123"
-- "{integer}"
--
-- >>> replaceAllFormats "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
-- "{uuid}"
--
-- >>> replaceAllFormats "User 123 accessed endpoint c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
-- "User {integer} accessed endpoint {uuid}"
--
-- >>> replaceAllFormats "Error at 192.168.0.1:8080 with status 404"
-- "Error at {integer}.{integer}.{integer}.{integer}:{integer} with status {integer}"
replaceAllFormats :: Text -> Text
replaceAllFormats input =
  let (finalText, replacements) = processAllPatterns input commonFormatPatterns 0 []
   in applyReplacements finalText replacements
  where
    -- Unique placeholder that won't appear in text or match any pattern
    makePlaceholder :: Int -> Text
    makePlaceholder n = "\x00PH" <> T.pack (show n) <> "\x00"

    -- Process all patterns, accumulating placeholder mappings
    processAllPatterns :: Text -> [(RE, Text)] -> Int -> [(Text, Text)] -> (Text, [(Text, Text)])
    processAllPatterns txt [] _ repls = (txt, repls)
    processAllPatterns txt ((regex, replacement) : rest) counter repls =
      let ph = makePlaceholder counter
          newTxt = replaceAll ph (txt *=~ regex)
          hasMatches = newTxt /= txt
          newRepls = if hasMatches then (ph, replacement) : repls else repls
          newCounter = if hasMatches then counter + 1 else counter
       in processAllPatterns newTxt rest newCounter newRepls

    -- Apply all replacements to restore placeholders
    applyReplacements :: Text -> [(Text, Text)] -> Text
    applyReplacements txt [] = txt
    applyReplacements txt ((ph, repl) : rest) =
      applyReplacements (T.replace ph repl txt) rest


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
-- Just "{integer}"
--
-- >>> valueToFormatStr "c73bcdcc-2669-4bf6-81d3-e4ae73fb11fd"
-- Just "{uuid}"
--
-- >>> valueToFormatStr "2023-10-14T10:29:38.64522Z"
-- Just "{YYYY-MM-DDThh:mm:ss.sTZD}"
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

    -- Add patterns that are specific to valueToFormatStr (exact match patterns for path params)
    formatChecks :: [(RE, Text)]
    formatChecks =
      commonFormatPatterns
        ++ [ -- ([re|^[0-9]+$|], "{integer}")
             -- , ([re|^[+-]?(\d+(\.\d*)?|\.\d+)$|], "{float}")
             ([re|^0x[0-9A-Fa-f]+$|], "{hex}")
           ]


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
