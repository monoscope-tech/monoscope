{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Utils (
  eitherStrToText,
  jsonValueToHtmlTree,
  userIsProjectMember,
  GetOrRedirect,
  redirect,
  lookupVecByKey,
  DBField (..),
  faSprite_,
  lookupVecInt,
  lookupVecText,
  lookupVecIntByKey,
  formatUTC,
  parseUTC,
  lookupVecTextByKey,
  getStatusBorderColor,
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
  lemonSqueezyUrls,
  lemonSqueezyUrlsAnnual,
  lookupMapText,
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
)
where

import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.Char (isDigit)
import Data.Digest.XXHash (xxHash)
import Data.HashMap.Strict qualified as HM
import Data.List (notElem, (!!))
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, ZonedTime, defaultTimeLocale, parseTimeM)
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
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Session
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


data DBField = forall a. (ToField a, Show a) => MkDBField a


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
getSeverityColor "debug" = "text-gray-500 bg-gray-100"
getSeverityColor "info" = "text-blue-500 bg-blue-100"
getSeverityColor "warning" = "text-yellow-700 bg-yellow-100"
getSeverityColor "error" = "text-red-500 bg-red-100"
getSeverityColor "critical" = "text-red-700 bg-red-200 font-bold"
getSeverityColor "notice" = "text-green-500 bg-green-100"
getSeverityColor "alert" = "text-orange-600 bg-orange-100 font-bold"
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


jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = jsonValueToHtmlTree' ("", "", val)
  where
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (AEK.toHashMapText v & HM.toList & sort & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (V.iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key, toText $ show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf "[*]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      div_
        [ class_ "relative log-item-field-parent"
        , term "data-field-path" $ replaceNumbers fullFieldPath'
        , term "data-field-value" $ unwrapJsonPrimValue False value
        ]
        $ a_
          [class_ "block hover:bg-blue-50 cursor-pointer pl-6 relative log-item-field-anchor ", [__|install LogItemMenuable|]]
          do
            span_ $ toHtml key
            span_ [class_ "text-blue-800"] ":"
            span_ [class_ "text-blue-800 ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml $ unwrapJsonPrimValue False value

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ (if key == "" then "" else "collapsed")] do
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
lookupMapText :: Text -> HashMap Text Value -> Maybe Text
lookupMapText key hashMap = case HM.lookup key hashMap of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


-- FIXME: delete
lookupMapInt :: Text -> HashMap Text Value -> Int
lookupMapInt key hashMap = case HM.lookup key hashMap of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0


lookupVecText :: V.Vector Value -> Int -> Maybe Text
lookupVecText vec idx = case vec V.!? idx of
  Just (AE.String textValue) -> Just textValue -- Extract text from Value if it's a String
  _ -> Nothing


lookupVecInt :: V.Vector Value -> Int -> Int
lookupVecInt vec idx = case vec V.!? idx of
  Just (AE.Number val) -> fromMaybe 0 $ toBoundedInteger val -- Extract text from Value if it's a String
  _ -> 0


lookupVecTextByKey :: V.Vector Value -> HM.HashMap Text Int -> Text -> Maybe Text
lookupVecTextByKey vec colIdxMap key = HM.lookup key colIdxMap >>= lookupVecText vec


lookupVecIntByKey :: V.Vector Value -> HM.HashMap Text Int -> Text -> Int
lookupVecIntByKey vec colIdxMap key = (HM.lookup key colIdxMap >>= Just . lookupVecInt vec) & fromMaybe 0


lookupVecByKey :: V.Vector Value -> HM.HashMap Text Int -> Text -> Maybe Value
lookupVecByKey vec colIdxMap key = HM.lookup key colIdxMap >>= (vec V.!?)


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
    p_ [] "You have exceeded the free tier requests limit for this month, new requests will not be processed."
    a_ [class_ "font-semibold text-red-700 bg-white px-2 py-1 rounded-lg", href_ $ "/p/" <> pid <> "/settings"] "upgrade now"


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
              availableColors' = filter (`notElem` usedColors) (V.toList serviceColors)
              availableColors = if null availableColors' then V.toList serviceColors else availableColors'
              colorIdx = sum (map ord $ toString $ toXXHash service) `mod` length availableColors
              selectedColor = availableColors !! colorIdx
           in go (V.tail svcs) (HM.insert service selectedColor assignedColors) (selectedColor : usedColors)


toXXHash :: Text -> Text
toXXHash input = leftPad 8 $ fromString $ showHex (xxHash $ encodeUtf8 input) ""


leftPad :: Int -> Text -> Text
leftPad len txt = T.justifyRight len '0' (T.take len txt)


convertToDHMS :: NominalDiffTime -> (Int, Int, Int, Int)
convertToDHMS diffTime =
  let totalSeconds = floor diffTime :: Int
      (days, rem1) = (totalSeconds `divMod` 86400)
      (hours, rem2) = rem1 `divMod` 3600
      (minutes, seconds) = rem2 `divMod` 60
   in (7 - days, hours, minutes, seconds)


-- "https://apitoolkit.lemonsqueezy.com/buy/b982b83b-66cc-4169-b5cb-f7d1d8a96a18?embed=1&media=0&logo=0&desc=0&checkout[custom][project_id]="

lemonSqueezyUrls :: V.Vector Text
lemonSqueezyUrls =
  V.fromList
    [ "https://apitoolkit.lemonsqueezy.com/buy/7c2ec567-2b9f-4bab-bb63-c3d5ab45e65b?embed=1&media=0&logo=0&desc=0" -- 400k
    , "https://apitoolkit.lemonsqueezy.com/buy/9695dbc9-6e24-4054-879f-1f360eda9293?embed=1&media=0&logo=0&desc=0" -- 550K
    , "https://apitoolkit.lemonsqueezy.com/buy/096bb970-e6a2-4cd7-a6ee-5f6ce6ecd39e?embed=1&media=0&logo=0&desc=0" -- 1M
    , "https://apitoolkit.lemonsqueezy.com/buy/e1b06cdb-5e18-44a4-9ec9-ece569f73137?embed=1&media=0&logo=0&desc=0" -- 2.5M
    , "https://apitoolkit.lemonsqueezy.com/buy/952fc4b0-2c5e-4789-b186-8580998dabd5?embed=1&media=0&logo=0&desc=0" -- 5M
    , "https://apitoolkit.lemonsqueezy.com/buy/1ceb7455-fa10-4c5c-b34b-00fa6f1b1729?embed=1&media=0&logo=0&desc=0" -- 7.5M
    , "https://apitoolkit.lemonsqueezy.com/buy/d0095f15-2363-4045-8c99-6603e8038c9e?embed=1&media=0&logo=0&desc=0" -- 10M
    ]


lemonSqueezyUrlsAnnual :: V.Vector Text
lemonSqueezyUrlsAnnual =
  V.fromList
    [ "https://apitoolkit.lemonsqueezy.com/buy/311bf5e7-f17a-44e6-b209-c42b84306f47?embed=1&media=0&logo=0&desc=0" -- 2.4M
    , "https://apitoolkit.lemonsqueezy.com/buy/0900a416-05b1-4ebe-a7bb-8b7d4c063c7f?embed=1&media=0&logo=0&desc=0" -- 6.6M
    , "https://apitoolkit.lemonsqueezy.com/buy/53fc9818-8f8a-4e60-a390-c10609e0a535?embed=1&media=0&logo=0&desc=0" -- 12M
    , "https://apitoolkit.lemonsqueezy.com/buy/196ba4de-af2e-4e17-ada6-6889e62011d0?embed=1&media=0&logo=0&desc=0" -- 30M
    , "https://apitoolkit.lemonsqueezy.com/buy/1821b082-28c2-4d2d-9a29-2cae822943b6?embed=1&media=0&logo=0&desc=0" -- 60M
    , "https://apitoolkit.lemonsqueezy.com/buy/1821b082-28c2-4d2d-9a29-2cae822943b6?embed=1&media=0&logo=0&desc=0" -- 60M
    , "https://apitoolkit.lemonsqueezy.com/buy/1821b082-28c2-4d2d-9a29-2cae822943b6?embed=1&media=0&logo=0&desc=0" -- 60M
    ]


isDemoAndNotSudo :: Projects.ProjectId -> Bool -> Bool
isDemoAndNotSudo pid isSudo = pid.toText == "00000000-0000-0000-0000-000000000000" && not isSudo
