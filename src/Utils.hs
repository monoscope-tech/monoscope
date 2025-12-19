{-# LANGUAGE GADTs #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  eitherStrToText,
  onpointerdown_,
  jsonValueToHtmlTree,
  freeTierDailyMaxEvents,
  JSONHttpApiData (..),
  parseTime,
  DBField (..),
  faSprite_,
  lookupVecInt,
  lookupVecText,
  lookupVecIntByKey,
  lookupVecBoolByKey,
  lookupValueText,
  formatUTC,
  insertIfNotExist,
  lookupVecTextByKey,
  deleteParam,
  getSeverityColor,
  unwrapJsonPrimValue,
  listToIndexHashMap,
  b64ToJson,
  getOtelLangVersion,
  freeTierLimitExceededBanner,
  checkFreeTierExceeded,
  isDemoAndNotSudo,
  escapedQueryPartial,
  displayTimestamp,
  utcTimeToNanoseconds,
  getDurationNSMS,
  toUriStr,
  toXXHash,
  getServiceColors,
  getGrpcStatusColor,
  nestedJsonFromDotNotation,
  prettyPrintCount,
  extractMessageFromLog,
  -- Fill color helpers
  statusFillColor,
  statusFillColorText,
  methodFillColor,
  levelFillColor,
  changeTypeFillColor,
)
where

import Data.Aeson as AE
import Data.Aeson.Extra.Merge (lodashMerge)
import Data.Aeson.Key (fromText)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Digest.XXHash (xxHash)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Time (ZonedTime, addUTCTime, defaultTimeLocale, parseTimeM, secondsToNominalDiffTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Transact
import Lucid
import Lucid.Hyperscript (__)
import Lucid.Svg qualified as Svg
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.HTTP.Types (urlEncode)
import Network.URI (escapeURIString, isUnescapedInURI)
import Numeric (showHex)
import Pkg.THUtils (hashFile)
import Relude hiding (notElem, show)
import Servant
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import Text.Show
import "base64" Data.ByteString.Base64 qualified as B64


-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True


-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a


escapedQueryPartial :: Text -> Text
escapedQueryPartial x = toText $ escapeURIString isUnescapedInURI $ toString x


data DBField = forall a. (Show a, ToField a) => MkDBField a


instance Show DBField where
  show (MkDBField a) = "MkDBField " ++ show a


instance ToField DBField where
  toField (MkDBField a) = toField a


onpointerdown_ :: Text -> Attribute
onpointerdown_ = term "onpointerdown"


faSprite_ :: Text -> Text -> Text -> Html ()
faSprite_ mIcon faType classes = svg_ [class_ $ "inline-block icon " <> classes] $ Svg.use_ [href_ $ "/public/assets/svgs/fa-sprites/" <> faType <> ".svg?v=" <> fileHash <> "#" <> mIcon]
  where
    fileHash = case faType of
      "regular" -> $(hashFile "/public/assets/svgs/fa-sprites/regular.svg")
      "solid" -> $(hashFile "/public/assets/svgs/fa-sprites/solid.svg")
      _ -> $(hashFile "/public/assets/svgs/fa-sprites/regular.svg")


deleteParam :: Text -> Text -> Text
deleteParam key url
  | needle == "" = url
  | otherwise = fixQueryStart $ T.replace needle "" url
  where
    needle = url =~ reg :: Text
    -- Handle &key= and ?key= patterns
    reg = "&" <> key <> "(=[^&]*)?|\\?" <> key <> "(=[^&]*)?" :: Text
    -- If result has path&remaining (no ?), fix to path?remaining
    fixQueryStart t = case T.breakOn "&" t of
      (before, after) | not (T.null after) && not ("?" `T.isInfixOf` before) -> before <> "?" <> T.drop 1 after
      _ -> t


getGrpcStatusColor :: Int -> Text
getGrpcStatusColor status
  | status == 0 = "cbadge-sm badge-2xx" -- OK
  | status >= 1 && status <= 16 = "cbadge-sm badge-4xx" -- Errors (1 to 16 are error codes)
  | otherwise = "text-textWeak bg-fillStrong"


getSeverityColor :: Text -> Text
getSeverityColor "debug" = "text-textWeak border-strokeWeak bg-fillWeak"
getSeverityColor "info" = "text-textBrand border-strokeBrand-strong bg-fillInformation-weak"
getSeverityColor "warning" = "text-textWarning border-strokeWarning-strong bg-fillWarning-weak"
getSeverityColor "error" = "text-textError border-strokeError-strong bg-fillError-weak"
getSeverityColor "critical" = "text-textError border-strokeError-strong bg-fillError-strong font-bold"
getSeverityColor "notice" = "text-textSuccess border-strokeSuccess-strong bg-fillSuccess-weak"
getSeverityColor "alert" = "text-textWarning border-strokeWarning-strong bg-fillWarning-strong font-bold"
getSeverityColor _ = "text-textStrong bg-fillWeaker"


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
  div_ [class_ "p-2 rounded-lg bg-fillWeaker border w-full json-tree-container"] do
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
        , term "data-reqjson" json
        ]
        do
          span_ [class_ "underline"] "Copy json"
          faSprite_ "copy" "regular" "w-2 h-2"

      button_
        [ class_ "flex items-center gap-1 cursor-pointer"
        , onclick_ "window.downloadJson(event)"
        , term "data-reqjson" json
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
        [ class_ "log-item-field-parent block dropdown"
        , term "data-field-path" $ replaceNumbers $ if isJust pathM then T.replace ".." "." fullFieldPath' else fullFieldPath'
        , term "data-field-value" $ unwrapJsonPrimValue False value
        ]
        do
          a_
            [tabindex_ "0", role_ "button", class_ "block hover:bg-fillBrandWeak cursor-pointer pl-6 log-item-field-anchor"]
            do
              span_ $ toHtml key
              span_ [class_ "text-textBrand"] ":"
              span_ [class_ "text-textBrand ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml $ unwrapJsonPrimValue False value

          ul_ [tabindex_ "-1", id_ "log-item-context-menu", class_ "log-item-context-menu dropdown-content z-50 menu p-2 shadow bg-base-100 rounded-box w-52", tabindex_ "0"] do
            li_
              $ a_
                [ class_ "flex gap-2 items-center"
                , [__|
                    init set fp to (closest @data-field-path) then
                        set cols to params().cols or '' then 
                        set colsX to cols.split(',') then
                        if colsX contains fp 
                          set innerHTML of first <span/> in me to 'Remove column'
                        end
                    on click
                        set fp to (closest @data-field-path)
                        call #resultTable.toggleColumnOnTable(fp) then
                        set cols to params().cols or '' then
                        set colsX to cols.split(',')
                        if colsX contains fp
                          set innerHTML of first <span/> in me to 'Remove column'
                        else
                          set innerHTML of first <span/> in me to 'Add as table column'
                        end
                  |]
                ]
                do
                  faSprite_ "table-column" "regular" "w-4 h-4 text-iconNeutral"
                  span_ [] "Add as table column"
            li_
              $ a_
                [ class_ "flex gap-2 items-center"
                , [__|on click if 'clipboard' in window.navigator then
                      call navigator.clipboard.writeText((previous <.log-item-field-value/>)'s innerText)
                      send successToast(value:['Value has been added to the Clipboard']) to <body/>
                      halt
                    end|]
                ]
                do
                  faSprite_ "copy" "regular" "w-4 h-4 text-iconNeutral"
                  span_ [] "Copy field value"
            li_ $ a_ [class_ "flex gap-2 items-center", onpointerdown_ "filterByField(event, 'Eq')"] do
              faSprite_ "filter-enhanced" "regular" "w-4 h-4 text-iconNeutral"
              span_ [] "Filter by field"
            li_ $ a_ [class_ "flex gap-2 items-center", onpointerdown_ "filterByField(event, 'NotEq')"] do
              faSprite_ "not-equal" "regular" "w-4 h-4 text-iconNeutral"
              span_ [] "Exclude field"
            li_
              $ a_
                [ class_ "flex gap-2 items-center"
                , [__|
                    init call window.updateGroupByButtonText(event, me) end
                    on refreshItem call window.updateGroupByButtonText(event, me) end
      
                    on click
                      call document.querySelector('query-builder').toggleGroupByField(closest @data-field-path) then
                      trigger refreshItem on me
                    end
                |]
                ]
                do
                  faSprite_ "copy" "regular" "w-4 h-4 text-iconNeutral"
                  span_ [] "Group by field"

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ $ "log-item-with-children" <> if count == 0 then " collapsed" else ""] do
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


unwrapJsonPrimValue :: Bool -> AE.Value -> Text
unwrapJsonPrimValue _ (AE.Bool True) = "true"
unwrapJsonPrimValue _ (AE.Bool False) = "false"
unwrapJsonPrimValue False (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue True (AE.String v) = toText v
unwrapJsonPrimValue _ (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue _ AE.Null = "null"
unwrapJsonPrimValue _ (AE.Object _) = "{..}"
unwrapJsonPrimValue _ (AE.Array items) = "[" <> toText (show (length items)) <> "]"


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


lookupVecBoolByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Bool
lookupVecBoolByKey vec colIdxMap key =
  fromMaybe False $ HM.lookup key colIdxMap >>= \i ->
    vec V.!? i >>= \case AE.Bool b -> Just b; _ -> Nothing


lookupVecIntByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Int
lookupVecIntByKey vec colIdxMap key = fromMaybe 0 $ lookupVecInt vec <$> HM.lookup key colIdxMap


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
    (toText . formatTime defaultTimeLocale "%b %d %H:%M")
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (toString inputDateString) :: Maybe UTCTime)


formatUTC :: UTCTime -> Text
formatUTC utcTime =
  toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utcTime


freeTierLimitExceededBanner :: Text -> Html ()
freeTierLimitExceededBanner pid =
  div_ [class_ "flex w-full text-center items-center px-4 gap-4 py-1 bg-fillError-weak text-textError rounded-lg justify-center"] do
    strong_ "Daily cap reached."
    " Your free-tier events have run out; unlock more capacity. "
    a_ [class_ "underline underline-offset-2 link", href_ $ "/p/" <> pid <> "/manage_billing"] "See pricing"


checkFreeTierExceeded :: Projects.ProjectId -> Text -> DBT IO Bool
checkFreeTierExceeded pid paymentPlan =
  if paymentPlan == "Free"
    then (> fromIntegral freeTierDailyMaxEvents) <$> RequestDumps.getLast24hTotalRequest pid
    else pure False


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
nestedJsonFromDotNotation =
  -- start from empty object, insert each small object with a lodash‑style merge
  foldl'
    ( \acc (k, v) ->
        let nestedObj = AE.Object $ build (T.splitOn "." k) v
         in lodashMerge acc nestedObj
    )
    (AE.object [])


isDemoAndNotSudo :: Projects.ProjectId -> Bool -> Bool
isDemoAndNotSudo pid isSudo = pid.toText == "00000000-0000-0000-0000-000000000000" && not isSudo


toUriStr :: Text -> Text
toUriStr s = decodeUtf8 $ urlEncode True (encodeUtf8 s)


parseTime :: Maybe Text -> Maybe Text -> Maybe Text -> UTCTime -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseTime fromM toM sinceM now = case sinceM of
  Just "5M" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 300) now, Just now, Just ("Last 5 min", ""))
  Just "15M" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 900) now, Just now, Just ("Last 15 min", ""))
  Just "30M" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 1800) now, Just now, Just ("Last 30 min", ""))
  Just "1H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just now, Just ("Last Hour", ""))
  Just "3H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 3) now, Just now, Just ("Last 3 Hours", ""))
  Just "6H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 6) now, Just now, Just ("Last 6 Hours", ""))
  Just "12H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 12) now, Just now, Just ("Last 12 Hours", ""))
  Just "24H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just now, Just ("Last 24 Hours", ""))
  Just "3D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 3) now, Just now, Just ("Last 3 Days", ""))
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
    case AE.eitherDecodeStrict' (encodeUtf8 t) of
      Right a -> Right (JSONHttpApiData a)
      -- If parsing fails, try wrapping in quotes and parsing as a string
      Left _ ->
        case AE.eitherDecodeStrict' (encodeUtf8 $ "\"" <> t <> "\"") of
          Right a -> Right (JSONHttpApiData a)
          Left err -> Left (fromString err)


instance AE.ToJSON a => ToHttpApiData (JSONHttpApiData a) where
  toUrlPiece (JSONHttpApiData a) =
    case toStrict $ AE.encode a of
      -- If the encoded value starts and ends with quotes, remove them
      bs
        | BS.length bs >= 2 && BS.head bs == 34 && BS.last bs == 34 ->
            decodeUtf8 $ BS.init $ BS.tail bs
      -- Otherwise, keep as is
      bs -> decodeUtf8 bs


freeTierDailyMaxEvents :: Integer
freeTierDailyMaxEvents = 1000


-- | Pretty print count numbers, converting large values to K/M/B format
-- | Example: 1534999 becomes 1.5M, 200000 becomes 200K
prettyPrintCount :: Int -> Text
prettyPrintCount n
  | n >= 1_000_000_000 = T.show (n `div` 1_000_000_000) <> "." <> T.show ((n `mod` 1_000_000_000) `div` 100_000_000) <> "B"
  | n >= 1_000_000 = T.show (n `div` 1_000_000) <> "." <> T.show ((n `mod` 1_000_000) `div` 100_000) <> "M"
  | n >= 1_000 = T.show (n `div` 1_000) <> "." <> T.show ((n `mod` 1_000) `div` 100) <> "K"
  | otherwise = T.show n


messageKeys :: [T.Text]
messageKeys =
  [ "msg"
  , "message"
  , "@message"
  , "@m"
  , "short_message"
  , "full_message"
  , "raw_message"
  , "original_message"
  , "event"
  , "text"
  , "data"
  , "body"
  , "log.message"
  , "log"
  , "logmessage"
  , "log_message"
  , "Message"
  , "MSG"
  , "Msg"
  , "m"
  , "@msg"
  , "@text"
  , "@event"
  , "content"
  , "description"
  , "detail"
  , "details"
  , "info"
  , "information"
  , "payload"
  , "value"
  , "record"
  , "entry"
  , "log.entry"
  , "log.text"
  , "log.event"
  , "log.body"
  , "log_entry"
  , "log_text"
  , "log_event"
  , "log_body"
  , "logMessage"
  , "logMsg"
  , "logText"
  , "logEvent"
  , "log.msg"
  , "@log"
  , "@content"
  , "@data"
  , "@body"
  , "@payload"
  , "@detail"
  , "@details"
  , "@description"
  , "@info"
  , "event_message"
  , "eventMessage"
  , "event.message"
  , "event_msg"
  , "eventMsg"
  , "event.msg"
  , "message_text"
  , "messageText"
  , "message.text"
  , "msg_text"
  , "msgText"
  , "msg.text"
  , "text_message"
  , "textMessage"
  , "text.message"
  , "note"
  , "notes"
  , "comment"
  , "comments"
  , "statement"
  , "line"
  , "logline"
  , "log_line"
  , "log.line"
  , "@line"
  , "@logline"
  , "output"
  , "log_output"
  , "log.output"
  , "@output"
  , "string"
  , "str"
  , "s"
  , "txt"
  , "message_body"
  , "messageBody"
  , "message.body"
  , "msg_body"
  , "msgBody"
  , "msg.body"
  , "_message"
  , "_msg"
  , "__message"
  , "__msg"
  ]


extractMessageFromLog :: Value -> Maybe T.Text
extractMessageFromLog (AE.Object obj) =
  listToMaybe [v | key <- messageKeys, Just v <- [extractValue key obj]]
  where
    extractValue :: T.Text -> Object -> Maybe T.Text
    extractValue key km = case Data.Aeson.KeyMap.lookup (fromText key) km of
      Just (AE.String s) -> Just s
      Just val -> Just (toText $ show val)
      Nothing -> Nothing
extractMessageFromLog _ = Nothing


-- | Get fill color class for HTTP status codes
statusFillColor :: Int -> Text
statusFillColor code
  | code >= 500 = "bg-fillError-strong"
  | code >= 400 = "bg-fillWarning-strong"
  | code >= 300 = "bg-fillBrand-strong"
  | code >= 200 = "bg-fillSuccess-strong"
  | otherwise = "bg-fillStrong"


-- | Get fill color from status code text (first char: "2xx", "3xx", etc.)
statusFillColorText :: Text -> Text
statusFillColorText val = case T.take 1 val of
  "5" -> "bg-fillError-strong"
  "4" -> "bg-fillWarning-strong"
  "3" -> "bg-fillBrand-strong"
  "2" -> "bg-fillSuccess-strong"
  _ -> "bg-fillStrong"


-- | Get fill color class for HTTP methods
methodFillColor :: Text -> Text
methodFillColor method = case T.toUpper method of
  "GET" -> "bg-fillBrand-strong"
  "POST" -> "bg-fillSuccess-strong"
  "PUT" -> "bg-fillWarning-strong"
  "DELETE" -> "bg-fillError-strong"
  "PATCH" -> "bg-fillBrand-strong"
  _ -> "bg-fillBrand-strong"


-- | Get fill color class for log levels
levelFillColor :: Text -> Text
levelFillColor level = case T.toLower level of
  v | "error" `T.isInfixOf` v || "fatal" `T.isInfixOf` v -> "bg-fillError-strong"
  v | "warn" `T.isInfixOf` v -> "bg-fillWarning-strong"
  v | "info" `T.isInfixOf` v -> "bg-fillBrand-strong"
  v | "debug" `T.isInfixOf` v || "trace" `T.isInfixOf` v -> "bg-fillStrong"
  _ -> "bg-fillWeak"


-- | Get outline color classes for change types (added/modified/removed)
changeTypeFillColor :: Text -> Text
changeTypeFillColor changeType = case T.toLower changeType of
  "added" -> "text-fillSuccess-strong border-strokeSuccess-strong bg-fillSuccess-weak"
  "modified" -> "text-fillInformation-strong border-strokeInformation-strong bg-fillInformation-weak"
  "removed" -> "text-fillError-strong border-strokeError-strong bg-fillError-weak"
  _ -> "text-textWeak border-strokeWeak bg-fillWeak"
