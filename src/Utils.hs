{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  eitherStrToText,
  jsonValueToHtmlTree,
  userIsProjectMember,
  GetOrRedirect,
  freeTierDailyMaxEvents,
  JSONHttpApiData (..),
  redirect,
  lookupVecByKey,
  parseTime,
  DBField (..),
  faSprite_,
  lookupVecInt,
  lookupVecText,
  lookupVecIntByKey,
  lookupValueText,
  formatUTC,
  callOpenAIAPI,
  insertIfNotExist,
  parseUTC,
  lookupVecTextByKey,
  getStatusBorderColor,
  systemPrompt,
  faIcon_,
  deleteParam,
  quoteTxt,
  textToBool,
  getMethodBorderColor,
  getSeverityColor,
  getMethodColor,
  getStatusColor,
  unwrapJsonPrimValue,
  listToIndexHashMap,
  b64ToJson,
  lookupMapText,
  getOtelLangVersion,
  lookupMapInt,
  freeTierLimitExceededBanner,
  isDemoAndNotSudo,
  escapedQueryPartial,
  convertToDHMS,
  getSpanStatusColor,
  getKindColor,
  displayTimestamp,
  utcTimeToNanoseconds,
  getDurationNSMS,
  toXXHash,
  getServiceColors,
  getGrpcStatusColor,
  nestedJsonFromDotNotation,
  prettyPrintCount,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Extra.Merge (lodashMerge)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isDigit)
import Data.Digest.XXHash (xxHash)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, ZonedTime, addUTCTime, defaultTimeLocale, parseTimeM, secondsToNominalDiffTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Transact
import Langchain.LLM.Core qualified as LLM
import Langchain.LLM.OpenAI
import Lucid
import Lucid.Hyperscript (__)
import Lucid.Svg qualified as Svg
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Users.Sessions qualified as Session
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Numeric (showHex)
import Pkg.THUtils (hashFile)
import Relude hiding (notElem, show)
import Servant
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import Text.Show


-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True


-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a


type GetOrRedirect = '[WithStatus 200 (Html ()), WithStatus 302 (Headers '[Header "Location" Text] NoContent)]


escapedQueryPartial :: Text -> Text
escapedQueryPartial x = toText $ escapeURIString isUnescapedInURI $ toString x


redirect :: Text -> Headers '[Header "Location" Text] NoContent
redirect destination = addHeader destination NoContent


data DBField = forall a. (Show a, ToField a) => MkDBField a


instance Show DBField where
  show (MkDBField a) = "MkDBField " ++ show a


instance ToField DBField where
  toField (MkDBField a) = toField a


faSprite_ :: Text -> Text -> Text -> Html ()
faSprite_ mIcon faType classes = svg_ [class_ $ "inline-block icon " <> classes] $ Svg.use_ [href_ $ "/public/assets/svgs/fa-sprites/" <> faType <> ".svg?v=" <> fileHash <> "#" <> mIcon]
  where
    fileHash = case faType of
      "regular" -> $(hashFile "/public/assets/svgs/fa-sprites/regular.svg")
      "solid" -> $(hashFile "/public/assets/svgs/fa-sprites/solid.svg")
      _ -> $(hashFile "/public/assets/svgs/fa-sprites/regular.svg")


-- DO NOT USE. Copy the svg into static/public/assets/svgs/fa-sprites/regular.svg or solid.svg
faIcon_ :: Text -> Text -> Text -> Html ()
faIcon_ faIcon faClasses classes = do
  i_ [class_ faClasses, term "data-fa-symbol" faIcon] ""
  svg_ [class_ classes] $ Svg.use_ [href_ $ "#" <> faIcon]


deleteParam :: Text -> Text -> Text
deleteParam key url = if needle == "" then url else T.replace needle "" url
  where
    needle = url =~ reg :: Text
    reg = "&" <> key <> "(=[^&]*)?|^" <> key <> "(=[^&]*)?&?" :: Text


quoteTxt :: Text -> Text
quoteTxt a = "'" <> a <> "'"


textToBool :: Text -> Bool
textToBool a = a == "true"


userIsProjectMember :: Session.PersistentSession -> Projects.ProjectId -> DBT IO Bool
userIsProjectMember sess pid = do
  if sess.isSudo
    then pure True
    else do
      user <- ProjectMembers.selectProjectActiveMember pid sess.userId
      case user of Nothing -> pure False; Just _ -> pure True


getMethodColor :: Text -> Text
getMethodColor "POST" = " cbadge-sm badge-pink "
getMethodColor "PUT" = " cbadge-sm badge-lime "
getMethodColor "DELETE" = " cbadge-sm badge-error "
getMethodColor "PATCH" = " cbadge-sm badge-cyan "
getMethodColor "GET" = " cbadge-sm badge-blue"
getMethodColor _ = " cbadge-sm badge-neutral"


getMethodBorderColor :: Text -> Text
getMethodBorderColor "POST" = "border-pink-500"
getMethodBorderColor "PUT" = "border-lime-500"
getMethodBorderColor "DELETE" = "border-error-500"
getMethodBorderColor "PATCH" = "border-cyan-500"
getMethodBorderColor "GET" = "border-blue-500"
getMethodBorderColor _ = "border-slate-200"


getStatusColor :: Int -> Text
getStatusColor status
  | status < 200 = "cbadge-sm badge-neutral"
  | status >= 200 && status < 300 = "cbadge-sm badge-2xx"
  | status >= 300 && status < 400 = "cbadge-sm badge-3xx"
  | otherwise = "cbadge-sm badge-4xx"


getStatusBorderColor :: Int -> Text
getStatusBorderColor status
  | status < 200 = "border-slate-200"
  | status >= 200 && status < 300 = "border-green-500"
  | status >= 300 && status < 400 = "border-yellow-500"
  | otherwise = "cbadge-sm badge-neutral"


getGrpcStatusColor :: Int -> Text
getGrpcStatusColor status
  | status == 0 = "cbadge-sm badge-2xx" -- OK
  | status >= 1 && status <= 16 = "cbadge-sm badge-4xx" -- Errors (1 to 16 are error codes)
  | otherwise = "text-slate-500 bg-slate-800"


getSeverityColor :: Text -> Text
getSeverityColor "debug" = "text-gray-500 border-gray-500 bg-gray-100"
getSeverityColor "info" = "text-brand border-strokeBrand-strong bg-blue-100"
getSeverityColor "warning" = "text-yellow-700 border-yellow-700 bg-yellow-100"
getSeverityColor "error" = "text-red-500 border-red-700 bg-red-100"
getSeverityColor "critical" = "text-red-700 border-red-700 bg-red-200 font-bold"
getSeverityColor "notice" = "text-green-500 border-green-500 bg-green-100"
getSeverityColor "alert" = "text-orange-600 border-orange-600 bg-orange-100 font-bold"
getSeverityColor _ = "text-black bg-gray-50"


-- >>> replaceNumbers "response_body.0.completed"
-- "response_body[*].completed"
--
replaceNumbers :: Text -> Text
replaceNumbers input = T.replace ".[*]" "[*]" $ T.intercalate "." (map replaceDigitPart parts)
  where
    parts = T.splitOn "." input
    replaceDigitPart :: Text -> Text
    replaceDigitPart part
      | T.all isDigit part = "[*]"
      | otherwise = T.concatMap replaceDigitWithAsterisk part

    replaceDigitWithAsterisk :: Char -> Text
    replaceDigitWithAsterisk ch
      | isDigit ch = "[*]"
      | otherwise = one ch


b64ToJson :: Text -> AE.Value
b64ToJson b64Text =
  fromRight (AE.object []) $ AE.eitherDecodeStrict $ fromRight "{}" $ B64.decodeBase64Untyped $ encodeUtf8 b64Text


jsonValueToHtmlTree :: AE.Value -> Maybe Text -> Html ()
jsonValueToHtmlTree val pathM = do
  div_ [class_ "p-2 rounded-lg bg-fillWeaker border w-full overflow-x-auto json-tree-container"] do
    div_ [class_ "w-full flex items-center gap-4 text-xs mb-2"] do
      button_
        [ class_ "flex hidden items-center gap-1 cursor-pointer"
        , [__|on click
               set container to the closest .json-tree-container to the parentElement of me
               set items to container.querySelectorAll(".log-item-with-children")
               remove .collapsed from items
               add .hidden to me
               remove .hidden from the next <button/>
             end
            |]
        ]
        do
          span_ [class_ "underline"] "Expand all"
          faSprite_ "expand" "regular" "w-2 h-2"
      button_
        [ class_ "flex items-center gap-1 cursor-pointer"
        , [__| on click
               set container to the closest .json-tree-container to the parentElement of me
               set items to container.querySelectorAll(".log-item-with-children")
               add .collapsed to items
               add .hidden to me
               remove .hidden from the previous <button/>
             end
           |]
        ]
        do
          span_ [class_ "underline"] "Collapse all"
          faSprite_ "expand" "regular" "w-2 h-2"

      let json = decodeUtf8 $ AE.encode $ AE.toJSON val
      button_
        [ class_ "flex items-center gap-1 cursor-pointer"
        , term
            "_"
            [text|  on click
                 if 'clipboard' in window.navigator then
                   call navigator.clipboard.writeText(my @data-reqjson)
                   send successToast(value:['Json copied to clipboard']) to <body/>
                 end|]
        , term "data-reqjson" $ json
        ]
        do
          span_ [class_ "underline"] "Copy json"
          faSprite_ "copy" "regular" "w-2 h-2"

      button_
        [ class_ "flex items-center gap-1 cursor-pointer"
        , onclick_ "window.downloadJson(event)"
        , term "data-reqjson" $ json
        ]
        do
          span_ [class_ "underline"] "Download json"
          faSprite_ "download-f" "regular" "w-2 h-2"
    jsonValueToHtmlTree' (fromMaybe "" pathM, "", val)
  where
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (AEKM.toHashMapText v & HM.toList & sort & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (V.iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key, toText $ show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf "[*]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      div_
        [ class_ "relative log-item-field-parent"
        , term "data-field-path" $ replaceNumbers $ if (isJust pathM) then T.replace ".." "." fullFieldPath' else fullFieldPath'
        , term "data-field-value" $ unwrapJsonPrimValue False value
        ]
        $ a_
          [class_ "block hover:bg-fillBrandWeak cursor-pointer pl-6 relative log-item-field-anchor ", [__|install LogItemMenuable|]]
          do
            span_ $ toHtml key
            span_ [class_ "text-blue-800"] ":"
            span_ [class_ "text-blue-800 ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml $ unwrapJsonPrimValue False value

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ (if key == "" then "log-item-with-children" else "log-item-with-children")] do
      a_
        [ class_ "inline-block items-center cursor-pointer"
        , onclick_ "this.parentNode.classList.toggle('collapsed')"
        ]
        do
          faSprite_ "chevron-right" "regular" "log-item-tree-chevron"
          span_ [] $ toHtml $ if key == "" then opening else key <> ": " <> opening
      div_ [class_ "pl-5 children "] do
        span_ [class_ "tree-children-count"] $ toHtml $ show count
        div_ [class_ "tree-children"] child
      span_ [class_ "pl-5 closing-token"] $ toHtml closing


getSpanStatusColor :: Text -> Text
getSpanStatusColor "ERROR" = "cbadge-sm badge-error"
getSpanStatusColor "OK" = "cbadge-sm badge-success"
getSpanStatusColor _ = "cbadge-sm badge-neutral"


-- data SpanKind = SKInternal | SKServer | SKClient | SKProducer | SKConsumer | SKUnspecified

getKindColor :: Text -> Text
getKindColor "INTERNAL" = "badge-info"
getKindColor "SERVER" = "badge-success"
getKindColor "CLIENT" = "badge-warning"
getKindColor "PRODUCER" = "badge-success"
getKindColor "CONSUMER" = "badge-warning"
getKindColor _ = "badge-outline"


unwrapJsonPrimValue :: Bool -> AE.Value -> Text
unwrapJsonPrimValue _ (AE.Bool True) = "true"
unwrapJsonPrimValue _ (AE.Bool False) = "false"
unwrapJsonPrimValue False (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue True (AE.String v) = toText v
unwrapJsonPrimValue _ (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue _ AE.Null = "null"
unwrapJsonPrimValue _ (AE.Object _) = "{..}"
unwrapJsonPrimValue _ (AE.Array items) = "[" <> toText (show (length items)) <> "]"


-- FIXME: delete
lookupMapText :: Text -> HashMap Text AE.Value -> Maybe Text
lookupMapText key hashMap = case HM.lookup key hashMap of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


-- FIXME: delete
lookupMapInt :: Text -> HashMap Text AE.Value -> Int
lookupMapInt key hashMap = case HM.lookup key hashMap of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0


lookupVecText :: V.Vector AE.Value -> Int -> Maybe Text
lookupVecText vec idx = case vec V.!? idx of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


lookupVecInt :: V.Vector AE.Value -> Int -> Int
lookupVecInt vec idx = case vec V.!? idx of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0


lookupVecTextByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Maybe Text
lookupVecTextByKey vec colIdxMap key = HM.lookup key colIdxMap >>= lookupVecText vec


lookupVecIntByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Int
lookupVecIntByKey vec colIdxMap key = (HM.lookup key colIdxMap >>= Just . lookupVecInt vec) & fromMaybe 0


lookupVecByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Maybe AE.Value
lookupVecByKey vec colIdxMap key = HM.lookup key colIdxMap >>= (vec V.!?)


lookupValueText :: AE.Value -> Text -> Maybe Text
lookupValueText (AE.Object obj) key = case AEKM.lookup (AEK.fromText key) obj of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing
lookupValueText _ _ = Nothing


listToIndexHashMap :: Hashable a => [a] -> HM.HashMap a Int
listToIndexHashMap list = HM.fromList [(x, i) | (x, i) <- zip list [0 ..]]


utcTimeToNanoseconds :: UTCTime -> Integer
utcTimeToNanoseconds utcTime =
  let posixTime = utcTimeToPOSIXSeconds utcTime
   in round (posixTime * 1e9)


getDurationNSMS :: Integer -> String
getDurationNSMS duration
  | duration >= 1000000000 = printf "%.1f s" (fromIntegral @_ @Double duration / 1000000000)
  | duration >= 1000000 = printf "%.1f ms" (fromIntegral @_ @Double duration / 1000000)
  | duration >= 1000 = printf "%.1f µs" (fromIntegral @_ @Double duration / 1000)
  | otherwise = printf "%.1f ns" (fromIntegral @_ @Double duration)


getOtelLangVersion :: Text -> Maybe Text
getOtelLangVersion "Golang" = Just "go"
getOtelLangVersion "Python" = Just "python"
getOtelLangVersion "Java" = Just "java"
getOtelLangVersion "Ruby" = Just "ruby"
getOtelLangVersion "Rust" = Just "rust"
getOtelLangVersion "Csharp" = Just "csharp"
getOtelLangVersion "PHP" = Just "php"
getOtelLangVersion "Javascript" = Just "nodejs"
getOtelLangVersion _ = Nothing


displayTimestamp :: Text -> Text
displayTimestamp inputDateString =
  maybe
    T.empty
    (toText . formatTime defaultTimeLocale "%b %d %H:%M:%S")
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (toString inputDateString) :: Maybe UTCTime)


formatUTC :: UTCTime -> Text
formatUTC utcTime =
  toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utcTime


parseUTC :: Text -> Maybe UTCTime
parseUTC utcTime = iso8601ParseM (toString utcTime)


freeTierLimitExceededBanner :: Text -> Html ()
freeTierLimitExceededBanner pid =
  div_ [class_ "flex w-full text-center items-center px-4 gap-4 py-2 bg-red-600 text-white rounded-lg justify-center"] do
    p_ [] "You’ve exceeded your free tier event limit for the past 24 hours. New requests will not be processed until the limit resets."
    a_ [class_ "font-semibold text-red-700 bg-white px-2 py-1 rounded-lg", href_ $ "/p/" <> pid <> "/manage_billing"] "upgrade now"


serviceColors :: V.Vector Text
serviceColors =
  V.fromList
    [ "bg-red-400"
    , "bg-amber-400"
    , "bg-orange-400"
    , "bg-yellow-400"
    , "bg-lime-400"
    , "bg-green-400"
    , "bg-teal-400"
    , "bg-cyan-400"
    , "bg-blue-400"
    , "bg-purple-400"
    , "bg-violet-400"
    , "bg-pink-400"
    , "bg-rose-400"
    , "bg-emerald-400"
    , "bg-fuchsia-400"
    , "bg-indigo-400"
    , "bg-sky-400"
    ]


getServiceColors :: V.Vector Text -> HashMap Text Text
getServiceColors services = go services HM.empty []
  where
    go :: V.Vector Text -> HashMap Text Text -> [Text] -> HashMap Text Text
    go svcs assignedColors usedColors
      | V.null svcs = assignedColors
      | otherwise =
          let service = V.head svcs
              availableColors' = filter (`L.notElem` usedColors) (V.toList serviceColors)
              availableColors = if null availableColors' then V.toList serviceColors else availableColors'
              colorIdx = sum (map ord $ toString $ toXXHash service) `mod` length availableColors
              selectedColor = availableColors L.!! colorIdx
           in go (V.tail svcs) (HM.insert service selectedColor assignedColors) (selectedColor : usedColors)


toXXHash :: Text -> Text
toXXHash input = leftPad 8 $ fromString $ showHex (xxHash $ encodeUtf8 input) ""


leftPad :: Int -> Text -> Text
leftPad len txt = T.justifyRight len '0' (T.take len txt)


-- | Turn a dot‑key + value into a singleton nested Object
build :: [Text] -> AE.Value -> AEKM.KeyMap AE.Value
build [] _ = AEKM.empty
build [x] v = AEKM.singleton (AEK.fromText x) v
build (x : xs) v = AEKM.singleton (AEK.fromText x) (AE.Object (build xs v))


-- | Succinct “dot‑notation → nested JSON”
nestedJsonFromDotNotation :: [(Text, AE.Value)] -> AE.Value
nestedJsonFromDotNotation pairs =
  -- start from empty object, insert each small object with a lodash‑style merge
  foldl'
    ( \acc (k, v) ->
        let nestedObj = AE.Object $ build (T.splitOn "." k) v
         in lodashMerge acc nestedObj
    )
    (AE.object [])
    pairs


convertToDHMS :: NominalDiffTime -> (Int, Int, Int, Int)
convertToDHMS diffTime =
  let totalSeconds = floor diffTime :: Int
      (days, rem1) = (totalSeconds `divMod` 86400)
      (hours, rem2) = rem1 `divMod` 3600
      (minutes, seconds) = rem2 `divMod` 60
   in (7 - days, hours, minutes, seconds)


isDemoAndNotSudo :: Projects.ProjectId -> Bool -> Bool
isDemoAndNotSudo pid isSudo = pid.toText == "00000000-0000-0000-0000-000000000000" && not isSudo


parseTime :: Maybe Text -> Maybe Text -> Maybe Text -> UTCTime -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseTime fromM toM sinceM now = case sinceM of
  Just "1H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just now, Just ("Last Hour", ""))
  Just "24H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just now, Just ("Last 24 Hours", ""))
  Just "7D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 7) now, Just now, Just ("Last 7 Days", ""))
  Just "14D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 14) now, Just now, Just ("Last 14 Days", ""))
  _ -> do
    let f = (iso8601ParseM (toString $ fromMaybe "" fromM) :: Maybe UTCTime)
        t = (iso8601ParseM (toString $ fromMaybe "" toM) :: Maybe UTCTime)
        start = toText . formatTime defaultTimeLocale "%F %T" <$> f
        end = toText . formatTime defaultTimeLocale "%F %T" <$> t
        range = case (start, end) of
          (Just s, Just e) -> Just (s, e)
          _ -> Nothing
     in (f, t, range)


insertIfNotExist :: Eq a => a -> V.Vector a -> V.Vector a
insertIfNotExist x vec
  | x `V.elem` vec = vec
  | otherwise = V.snoc vec x


-- A newtype wrapper that uses the FromJSON instance to implement FromHttpApiData,
-- without requiring quotes for string values in URLs.
newtype JSONHttpApiData a = JSONHttpApiData a


instance AE.FromJSON a => FromHttpApiData (JSONHttpApiData a) where
  parseUrlPiece t =
    -- Try to parse assuming it's already a valid JSON string
    case AE.eitherDecodeStrict' (TE.encodeUtf8 t) of
      Right a -> Right (JSONHttpApiData a)
      -- If parsing fails, try wrapping in quotes and parsing as a string
      Left _ ->
        case AE.eitherDecodeStrict' (TE.encodeUtf8 $ "\"" <> t <> "\"") of
          Right a -> Right (JSONHttpApiData a)
          Left err -> Left (fromString err)


instance AE.ToJSON a => ToHttpApiData (JSONHttpApiData a) where
  toUrlPiece (JSONHttpApiData a) =
    case LBS.toStrict $ AE.encode a of
      -- If the encoded value starts and ends with quotes, remove them
      bs
        | BS.length bs >= 2 && BS.head bs == 34 && BS.last bs == 34 ->
            TE.decodeUtf8 $ BS.init $ BS.tail bs
      -- Otherwise, keep as is
      bs -> TE.decodeUtf8 bs


freeTierDailyMaxEvents :: Integer
freeTierDailyMaxEvents = 1000


-- | Pretty print count numbers, converting large values to K/M/B format
-- | Example: 1534999 becomes 1.5M, 200000 becomes 200K
prettyPrintCount :: Int -> Text
prettyPrintCount n
  | n >= 1_000_000_000 = T.pack (show (n `div` 1_000_000_000)) <> "." <> T.pack (show ((n `mod` 1_000_000_000) `div` 100_000_000)) <> "B"
  | n >= 1_000_000 = T.pack (show (n `div` 1_000_000)) <> "." <> T.pack (show ((n `mod` 1_000_000) `div` 100_000)) <> "M"
  | n >= 1_000 = T.pack (show (n `div` 1_000)) <> "." <> T.pack (show ((n `mod` 1_000) `div` 100)) <> "K"
  | otherwise = T.pack (show n)


callOpenAIAPI :: Text -> Text -> IO (Either Text (Text, Maybe Text))
callOpenAIAPI fullPrompt apiKey = do
  let openAI =
        OpenAI
          { apiKey = apiKey
          , openAIModelName = "gpt-4o-mini"
          , callbacks = []
          , baseUrl = Nothing
          }
  -- Use langchain-hs to generate response
  result <- liftIO $ LLM.generate openAI fullPrompt Nothing
  case result of
    Left err -> pure $ Left $ "LLM Error: " <> T.pack err
    Right response -> do
      -- Parse the response for query and visualization type
      let lines' = T.lines $ T.strip response
          queryLine = fromMaybe "" (viaNonEmpty head lines')

          -- Check if a visualization type is specified
          vizTypeM =
            if length lines' > 1
              then parseVisualizationType (lines' L.!! 1)
              else Nothing

          -- Clean the query by removing any code block markup and language identifiers
          cleanedQuery =
            T.strip
              $ if "```" `T.isPrefixOf` queryLine
                then
                  let withoutFirstLine = maybe "" (T.unlines . toList) $ viaNonEmpty tail (T.lines queryLine)
                      withoutBackticks = T.takeWhile (/= '`') withoutFirstLine
                   in T.strip withoutBackticks
                else queryLine
       in -- Check if the response indicates an invalid query
          if "Please provide a query"
            `T.isInfixOf` cleanedQuery
            || "I need more"
            `T.isInfixOf` cleanedQuery
            || "Could you please"
            `T.isInfixOf` cleanedQuery
            || T.length cleanedQuery
            < 3
            then pure $ Left "INVALID_QUERY_ERROR"
            else pure $ Right (cleanedQuery, vizTypeM)


-- Parse visualization type from the response
parseVisualizationType :: Text -> Maybe Text
parseVisualizationType line = do
  let lowerLine = T.toLower $ T.strip line
      prefixes = ["visualization:", "visualization type:", "viz:", "viz type:", "chart:", "chart type:", "type:"]
      -- Try to match any of the prefixes
      matchedPrefix = find (`T.isPrefixOf` lowerLine) prefixes

  matchedPrefix >>= \prefix -> do
    -- Extract the visualization type after the prefix
    let rawType = T.strip $ T.drop (T.length prefix) lowerLine

    -- Map to known visualization types
    case rawType of
      "bar" -> Just "timeseries"
      "line" -> Just "timeseries_line"
      "logs" -> Just "logs"
      "timeseries" -> Just "timeseries"
      "timeseries_line" -> Just "timeseries_line"
      "bar chart" -> Just "timeseries"
      "line chart" -> Just "timeseries_line"
      "time series" -> Just "timeseries"
      "time series line" -> Just "timeseries_line"
      _ -> Nothing -- Unknown visualization type


systemPrompt :: Text
systemPrompt =
  T.unlines
    [ "You are a helpful assistant that converts natural language queries to KQL (Kusto Query Language) filter expressions."
    , ""
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , ""
    , "Available Operators:"
    , "- Comparison: == != > < >= <="
    , "- Set operations: in !in (e.g., method in (\"GET\", \"POST\"))"
    , "- Text search: has !has (case-insensitive word search)"
    , "- Text collections: has_any has_all (e.g., tags has_any [\"urgent\", \"critical\"])"
    , "- String operations: contains !contains startswith !startswith endswith !endswith"
    , "- Pattern matching: matches =~ (regex, e.g., email matches /.*@company\\.com/)"
    , "- Logical: AND OR (or lowercase and or)"
    , "- Duration values: 100ms 5s 2m 1h (nanoseconds, microseconds, milliseconds, seconds, minutes, hours)"
    , ""
    , "VISUALIZATION TYPES:"
    , "If the query is best visualized as a chart rather than logs, specify the visualization type on a new line after the query:"
    , "- logs: For displaying log entries (default)"
    , "- timeseries (bar): For bar chart time-based visualization"
    , "- timeseries_line (line): For line chart time-based visualization"
    , ""
    , "When to use different visualization types:"
    , "- Use 'logs' for filtering specific log entries or when detailed log information is needed"
    , "- Use 'timeseries' (bar chart) for queries that count occurrences over time, like error counts, status code distribution"
    , "- Use 'timeseries_line' (line chart) for continuous metrics over time, like response times, latency, throughput"
    , ""
    , "IMPORTANT: For chart visualizations (timeseries or timeseries_line), you MUST include a 'summarize' statement in your query."
    , "Chart queries follow standard KQL syntax and typically include:"
    , "1. [filters] | summarize <aggregation> by bin(timestamp, <time>), [optional field]"
    , -- , "2. Optional: | sort by <field> [asc|desc]"
      -- , "3. Optional: | take N"
      ""
    , "The summarize statement can use various aggregation functions like count(), sum(...), avg(...), min(...), max(...), median(...), etc."
    , ""
    , "Examples of chart queries:"
    , "- \"Show errors by hour\": level == \"ERROR\" | summarize count() by bin(timestamp, 1h)"
    , "- \"Graph request counts by kind in 2h blocks\": | summarize count() by bin(timestamp, 2h), kind"
    , "- \"Line chart of p95 durations by method\": | summarize p95(duration) by bin(timestamp, 30m), attributes.http.request.method"
    , ""
    , "Examples:"
    , "- \"show me errors\" -> level == \"ERROR\""
    , "- \"POST requests\" -> attributes.http.request.method == \"POST\""
    , "- \"slow requests\" -> duration > 500ms"
    , "- \"500 errors\" -> attributes.http.response.status_code == \"500\""
    , "- \"GET or POST requests\" -> method in (\"GET\", \"POST\")"
    , "- \"messages containing error\" -> message contains \"error\""
    , "- \"logs with urgent or critical tags\" -> tags has_any [\"urgent\", \"critical\"]"
    , "- \"paths starting with /api\" -> path startswith \"/api\""
    , "- \"emails from company.com\" -> email matches /.*@company\\.com/"
    , "- \"requests taking more than 1 second\" -> duration > 1s"
    , "- \"Show me error count over time\" -> level == \"ERROR\"\nvisualization: timeseries"
    , "- \"Graph of response times\" -> duration > 0\nvisualization: timeseries_line"
    , ""
    , "Return ONLY the KQL filter expression on the first line without any backticks, markdown formatting, or code blocks. If applicable, specify the visualization type on the second line with format 'visualization: [type]'"
    , ""
    , "IMPORTANT: Do not use code blocks or backticks in your response. Return the raw query directly."
    ]
