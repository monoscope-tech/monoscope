module Utils (
  eitherStrToText,
  onpointerdown_,
  jsonValueToHtmlTree,
  freeTierDailyMaxEvents,
  JSONHttpApiData (..),
  parseTime,
  DBField (..),
  faSprite_,
  LoadingSize (..),
  LoadingType (..),
  loadingIndicator_,
  loadingIndicatorWith_,
  htmxIndicator_,
  htmxIndicatorWith_,
  htmxOverlayIndicator_,
  lookupVecInt,
  lookupVecText,
  lookupVecIntByKey,
  lookupVecBoolByKey,
  lookupValueText,
  formatUTC,
  insertIfNotExist,
  getAlertStatusColor,
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
  -- Hex color mapping for ECharts server-side rendering
  themeColorsHex,
  getSeriesColorHex,
  nestedJsonFromDotNotation,
  prettyPrintCount,
  formatWithCommas,
  extractMessageFromLog,
  -- Fill color helpers
  statusFillColor,
  statusFillColorText,
  methodFillColor,
  levelFillColor,
  changeTypeFillColor,
  replaceAllFormats,
)
where

import Data.Aeson as AE
import Data.Aeson.Extra.Merge (lodashMerge)
import Data.Aeson.Key (fromText)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.KeyMap qualified as AEKM
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Digest.XXHash (xxHash)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time (ZonedTime, addUTCTime, defaultTimeLocale, parseTimeM, secondsToNominalDiffTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Only (Only, fromOnly))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Effectful (Eff, IOE)
import Effectful qualified
import Effectful.PostgreSQL (WithConnection)
import Effectful.PostgreSQL qualified as PG
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Hyperscript (__)
import Lucid.Svg qualified as Svg
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.HTTP.Types (urlEncode)
import Network.URI (escapeURIString, isUnescapedInURI)
import Numeric (showHex)
import Pkg.DeriveUtils (hashFile)
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


faSprite_ :: Monad m => Text -> Text -> Text -> HtmlT m ()
faSprite_ mIcon faType classes = svg_ [class_ $ "inline-block icon " <> classes] $ Svg.use_ [href_ $ "/public/assets/svgs/fa-sprites/" <> faType <> ".svg?v=" <> fileHash <> "#" <> mIcon]
  where
    fileHash = case faType of
      "regular" -> $(hashFile "/public/assets/svgs/fa-sprites/regular.svg")
      "solid" -> $(hashFile "/public/assets/svgs/fa-sprites/solid.svg")
      _ -> $(hashFile "/public/assets/svgs/fa-sprites/regular.svg")


-- | Type-safe loading indicator size
data LoadingSize = LdXS | LdSM | LdMD | LdLG deriving (Eq, Show)


-- | Type-safe loading indicator type
data LoadingType = LdDots | LdSpinner | LdRing deriving (Eq, Show)


loadingSizeClass :: LoadingSize -> Text
loadingSizeClass = \case LdXS -> "xs"; LdSM -> "sm"; LdMD -> "md"; LdLG -> "lg"


loadingTypeClass :: LoadingType -> Text
loadingTypeClass = \case LdDots -> "dots"; LdSpinner -> "spinner"; LdRing -> "ring"


-- | Accessible loading indicator with screen reader support
-- Tailwind safelist: class_ "loading loading-dots loading-spinner loading-ring loading-xs loading-sm loading-md loading-lg"
loadingIndicator_ :: Monad m => LoadingSize -> LoadingType -> HtmlT m ()
loadingIndicator_ size typ = loadingIndicatorWith_ size typ ""


-- | Loading indicator with extra classes for custom styling
loadingIndicatorWith_ :: Monad m => LoadingSize -> LoadingType -> Text -> HtmlT m ()
loadingIndicatorWith_ size typ extraClasses = span_ [class_ $ "loading loading-" <> loadingTypeClass typ <> " loading-" <> loadingSizeClass size <> if T.null extraClasses then "" else " " <> extraClasses, role_ "status", Aria.label_ "Loading"] ""


htmxIndicator_ :: Monad m => Text -> LoadingSize -> HtmlT m ()
htmxIndicator_ elId size = htmxIndicatorWith_ elId size ""


htmxIndicatorWith_ :: Monad m => Text -> LoadingSize -> Text -> HtmlT m ()
htmxIndicatorWith_ elId size extraCls =
  span_ [id_ elId, class_ $ "htmx-indicator loading loading-dots loading-" <> loadingSizeClass size <> bool "" (" " <> extraCls) (not $ T.null extraCls), role_ "status", Aria.label_ "Loading"] ""


htmxOverlayIndicator_ :: Monad m => Text -> HtmlT m ()
htmxOverlayIndicator_ elId = span_ [id_ elId, class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots z-10 top-10", role_ "status", Aria.label_ "Loading"] ""


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
  div_ [class_ "p-2 rounded-lg bg-fillWeaker border w-full json-tree-container monospace text-sm leading-6"] do
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
            li_ $ a_ [class_ "flex gap-2 items-center", onpointerdown_ "viewFieldPatterns(event.target.closest('[data-field-path]').dataset.fieldPath)"] do
              faSprite_ "chart-bar" "regular" "w-4 h-4 text-iconNeutral"
              span_ [] "View patterns"

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
unwrapJsonPrimValue stripped = \case
  AE.Bool True -> "true"
  AE.Bool False -> "false"
  AE.String v -> if stripped then toText v else "\"" <> toText v <> "\""
  AE.Number v -> toText @String $ show v
  AE.Null -> "null"
  AE.Object _ -> "{..}"
  AE.Array items -> "[" <> toText (show (length items)) <> "]"


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
  fromMaybe False $ HM.lookup key colIdxMap >>= ((vec V.!?) >=> \case AE.Bool b -> Just b; _ -> Nothing)


lookupVecIntByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Int
lookupVecIntByKey vec colIdxMap key = maybe 0 (lookupVecInt vec) (HM.lookup key colIdxMap)


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


checkFreeTierExceeded :: (IOE Effectful.:> es, WithConnection Effectful.:> es) => Projects.ProjectId -> Text -> Eff es Bool
checkFreeTierExceeded pid paymentPlan =
  if paymentPlan == "Free"
    then do
      count <- maybe (0 :: Int) fromOnly . listToMaybe <$> PG.query [sql| SELECT count(*)::INT FROM otel_logs_and_spans WHERE project_id=? AND timestamp > NOW() - interval '1 day'|] (Only pid)
      pure $ count > fromInteger freeTierDailyMaxEvents
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
getServiceColors = V.foldl' assign HM.empty
  where
    assign colors service =
      let colorIdx = fromIntegral (xxHash (encodeUtf8 service)) `mod` V.length serviceColors
       in HM.insert service (serviceColors V.! colorIdx) colors


-- | Theme colors (hex) for ECharts - matches colorMapping.ts THEME_COLORS
themeColorsHex :: V.Vector Text
themeColorsHex =
  V.fromList
    [ "#1A74A8"
    , "#91cc75"
    , "#fac858"
    , "#ee6666"
    , "#73c0de"
    , "#3ba272"
    , "#fc8452"
    , "#9a60b4"
    , "#c71585"
    , "#37a2da"
    , "#32c5e9"
    , "#20b2aa"
    , "#228b22"
    , "#ff8c00"
    , "#ff6347"
    , "#dc143c"
    , "#8b008b"
    , "#4b0082"
    , "#6a5acd"
    , "#4169e1"
    ]


nullishNames, percentileNames :: HS.HashSet Text
nullishNames = HS.fromList ["null", "undefined", "unknown"]
percentileNames = HS.fromList ["median", "max", "min", "q1", "q3"]


-- | Get hex color for chart series - auto-detects type (status code, percentile, or generic)
getSeriesColorHex :: Text -> Text
getSeriesColorHex name
  | T.null name = themeColorsHex V.! 0
  | HS.member (T.toLower name) nullishNames = "#9ca3af"
  | isStatusCode name = statusCodeColorHex (fromMaybe 0 $ readMaybe $ toString name)
  | isPercentile name = percentileColorHex name
  | otherwise = themeColorsHex V.! hashTextToIndex name
  where
    isStatusCode t = T.length t == 3 && T.all isDigit t && T.head t `elem` ['2' .. '5']
    isPercentile t = T.isPrefixOf "p" (T.toLower t) || HS.member (T.toLower t) percentileNames
    hashTextToIndex t = fromIntegral (xxHash (encodeUtf8 t)) `mod` V.length themeColorsHex
    statusCodeColorHex code
      | code >= 200 && code < 300 = "#1A74A8"
      | code >= 300 && code < 400 = "#73c0de"
      | code >= 400 && code < 500 = "#fac858"
      | code >= 500 = "#ee6666"
      | otherwise = "#9d96f5"
    percentileColorHex p = case T.toLower p of
      "p50" -> "#91cc75"
      "median" -> "#91cc75"
      "min" -> "#91cc75"
      "p75" -> "#3ba272"
      "q1" -> "#3ba272"
      "p90" -> "#fac858"
      "p95" -> "#fc8452"
      "q3" -> "#fc8452"
      "p99" -> "#dc2626"
      "p100" -> "#991b1b"
      "max" -> "#991b1b"
      _ -> themeColorsHex V.! hashTextToIndex p


toXXHash :: Text -> Text
toXXHash = T.justifyRight 8 '0' . T.take 8 . fromString . flip showHex "" . xxHash . encodeUtf8


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


-- | Format a Double with thousand separators
-- | Example: 1000 -> "1,000", 1000.55 -> "1,000.55", 0.5 -> "0.5"
formatWithCommas :: Double -> Text
formatWithCommas d =
  let (intPart, fracPart) = properFraction d :: (Int, Double)
      intFormatted = fmt (commaizeF intPart)
   in if fracPart == 0 then intFormatted else intFormatted <> toText (dropWhile (/= '.') (show d))


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


getAlertStatusColor :: Text -> Text
getAlertStatusColor status = case status of
  "Alerting" -> "badge-error"
  "Warning" -> "badge-warning"
  _ -> "badge-success"


-- | Single-pass scanner that replaces variable tokens (IDs, numbers, dates, etc.) with type placeholders.
-- Used for log pattern grouping — normalizes log messages so structurally identical messages cluster together.
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
-- "Processing request {integer} at {YYYY-MM-DD}{port}"
--
-- >>> map replaceAllFormats ["Mixed: 192.168.1.1 123 :80 404", "0xDEADBEEF", "Values: 999, 1000, 1001"]
-- ["Mixed: {ipv4} {integer} {port} {integer}","{hex}","Values: {integer}, {integer}, {integer}"]
--
-- >>> map replaceAllFormats ["10.0.0.1", "172.16.0.1", "192.168.1.1", "255.255.255.0"]
-- ["{ipv4}","{ipv4}","{ipv4}","{ipv4}"]
--
-- >>> map replaceAllFormats ["Multiple formats: 123 abc def456789 :9000", "Log entry 404 at 10.0.0.1:8080"]
-- ["Multiple formats: {integer} abc def{integer} {port}","Log entry {integer} at {ipv4}{port}"]
--
-- >>> replaceAllFormats "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.signature sent to user@example.com"
-- "{jwt} sent to {email}"
--
-- >>> replaceAllFormats "2023-10-14 10:29:38 ERROR: Connection refused"
-- "{YYYY-MM-DD HH:MM:SS} ERROR: Connection refused"
--
-- >>> replaceAllFormats "[2023-10-14T10:29:38.123Z] INFO: Started"
-- "[{YYYY-MM-DDThh:mm:ss.sTZD}] INFO: Started"
--
-- >>> replaceAllFormats "123-45-6789 is a SSN"
-- "{ssn} is a SSN"
--
-- >>> replaceAllFormats "{\"status_code\":200,\"count\":42}"
-- "{\"status_code\":{integer},\"count\":{integer}}"
--
-- >>> replaceAllFormats "Oct 14, 2023 - User 12345 logged in"
-- "{Mon DD, YYYY} - User {integer} logged in"
--
-- >>> replaceAllFormats "Error on 14-Oct-2023 at 10:30"
-- "Error on {DD-Mon-YYYY} at {integer}{port}"
--
-- >>> replaceAllFormats "Jan 1, 2024 and Feb 28, 2025"
-- "{Mon DD, YYYY} and {Mon DD, YYYY}"
replaceAllFormats :: Text -> Text
replaceAllFormats !input = toText . TLB.toLazyText $ go Nothing (replacePrePass input)
  where
    -- Pre-pass: replace emails, JWTs, and text dates before the main scan
    replacePrePass :: Text -> Text
    replacePrePass = replaceJWTs . replaceEmails . replaceTextDates

    -- Replace text-based date formats: "Oct 14, 2023" and "14-Oct-2023"
    replaceTextDates :: Text -> Text
    replaceTextDates !txt = foldl' (flip replaceMonth) txt
      ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]

    replaceMonth :: Text -> Text -> Text
    replaceMonth mon = go'
      where
        go' !t = case T.breakOn mon t of
          (before, after)
            | T.null after -> t
            | not (monthBoundaryBefore before) || not (monthBoundaryAfter (T.drop 3 after)) ->
                before <> mon <> go' (T.drop 3 after)
            | otherwise ->
                let afterMon = T.drop 3 after
                 in case matchMonDY afterMon of
                      Just rest -> before <> "{Mon DD, YYYY}" <> go' rest
                      Nothing -> case matchDMonY before afterMon of
                        Just (prefix, rest) -> prefix <> "{DD-Mon-YYYY}" <> go' rest
                        Nothing -> before <> mon <> go' afterMon

    monthBoundaryBefore t = T.null t || not (isAlpha (T.last t))
    monthBoundaryAfter t = T.null t || not (isAlpha (T.head t))

    -- Match " DD[,] YYYY" after a month name
    matchMonDY :: Text -> Maybe Text
    matchMonDY !rest = do
      r1 <- T.stripPrefix " " rest
      let (day, r2) = T.span isDigit r1
      guard $ T.length day >= 1 && T.length day <= 2
      let r3 = fromMaybe r2 (T.stripPrefix "," r2)
      r4 <- T.stripPrefix " " r3
      let (year, r5) = T.span isDigit r4
      guard $ T.length year == 4
      guard $ T.null r5 || not (isAlphaNum (T.head r5))
      pure r5

    -- Match "DD-" before and "-YYYY" after a month name
    matchDMonY :: Text -> Text -> Maybe (Text, Text)
    matchDMonY before afterMon = do
      r1 <- T.stripPrefix "-" afterMon
      let (year, rest) = T.span isDigit r1
      guard $ T.length year == 4
      guard $ T.null rest || not (isAlphaNum (T.head rest))
      b <- T.stripSuffix "-" before
      guard $ not (T.null b)
      let (dayRev, prefRev) = T.span isDigit (T.reverse b)
      guard $ T.length dayRev >= 1 && T.length dayRev <= 2
      let prefix = T.reverse prefRev
      guard $ T.null prefix || not (isAlphaNum (T.last prefix))
      pure (prefix, rest)

    replaceEmails :: Text -> Text
    replaceEmails !txt =
      let (before, after) = T.breakOn "@" txt
       in if T.null after
            then txt
            else
              let (localRev, prefixRev) = T.span isLocalChar (T.reverse before)
               in if not (T.null localRev)
                    then case tryEmailDomain (T.drop 1 after) of
                      Just rest -> T.reverse prefixRev <> "{email}" <> replaceEmails rest
                      Nothing -> before <> "@" <> replaceEmails (T.drop 1 after)
                    else before <> "@" <> replaceEmails (T.drop 1 after)

    isLocalChar :: Char -> Bool
    isLocalChar c = isAlphaNum c || c `elem` ("._%+-" :: [Char])

    tryEmailDomain :: Text -> Maybe Text
    tryEmailDomain !afterAt = do
      let isDomainChar c = isAlphaNum c || c == '.' || c == '-'
          (domain, rest) = T.span isDomainChar afterAt
          parts = T.splitOn "." domain
      guard $ length parts >= 2
      let tld = viaNonEmpty last parts
      guard $ maybe False (\t -> T.length t >= 2 && T.all isAlpha t) tld
      pure rest

    replaceJWTs :: Text -> Text
    replaceJWTs !txt =
      let (before, after) = T.breakOn "eyJ" txt
       in if T.null after
            then txt
            else
              let rest3 = T.drop 3 after
                  (_seg1, r1) = T.span isBase64Url rest3
               in if not (T.null r1) && T.head r1 == '.'
                    then
                      let (seg2, r2) = T.span isBase64Url (T.drop 1 r1)
                       in if not (T.null seg2) && not (T.null r2) && T.head r2 == '.'
                            then
                              let (seg3, r3) = T.span isBase64Url (T.drop 1 r2)
                               in if not (T.null seg3)
                                    then before <> "{jwt}" <> replaceJWTs r3
                                    else before <> "eyJ" <> replaceJWTs rest3
                            else before <> "eyJ" <> replaceJWTs rest3
                    else before <> "eyJ" <> replaceJWTs rest3

    isBase64Url :: Char -> Bool
    isBase64Url c = isAlphaNum c || c == '_' || c == '-'

    -- Main single-pass scanner. Triggers on digits and ':'
    go :: Maybe Char -> Text -> TLB.Builder
    go !prev !txt = case T.break trigger txt of
      (safe, rest)
        | T.null rest -> TLB.fromText safe
        | otherwise ->
            let lastCh = if T.null safe then prev else Just (T.last safe)
                ch = T.head rest
             in if ch == ':'
                  then TLB.fromText safe <> dispatchColon lastCh (T.drop 1 rest)
                  else -- Digit trigger: check for hex-alpha suffix to peel
                    case peelHexSuffix safe of
                      (safePre, hexPart)
                        | not (T.null hexPart) ->
                            let combined = hexPart <> rest
                                lastPre = if T.null safePre then prev else Just (T.last safePre)
                             in TLB.fromText safePre <> scanHexDigit lastPre combined
                      _ -> TLB.fromText safe <> scanDigit lastCh rest

    trigger :: Char -> Bool
    trigger c = isDigit c || c == ':'

    -- Peel trailing hex-alpha chars (a-f, A-F) from safe text
    peelHexSuffix :: Text -> (Text, Text)
    peelHexSuffix !t =
      let rev = T.reverse t
          (hexRev, preRev) = T.span isHexAlpha rev
       in (T.reverse preRev, T.reverse hexRev)

    -- Colon dispatch: context-aware port detection
    dispatchColon :: Maybe Char -> Text -> TLB.Builder
    dispatchColon !lastCh !rest
      | not (T.null rest) && isDigit (T.head rest) && isPortContext lastCh =
          let (digits, after) = T.span isDigit rest
           in if T.length digits <= 5
                then "{port}" <> go (Just '}') after
                else TLB.singleton ':' <> go (Just ':') rest
      | otherwise = TLB.singleton ':' <> go (Just ':') rest

    isPortContext :: Maybe Char -> Bool
    isPortContext Nothing = True -- start of line or after placeholder
    isPortContext (Just c) = isAlphaNum c || c `elem` ("})]-. " :: [Char])

    -- Hex+digit sequence starting with hex alpha (from suffix peeling)
    scanHexDigit :: Maybe Char -> Text -> TLB.Builder
    scanHexDigit !prev !txt =
      let (fullHex, rest) = T.span isHexDigit' txt
          len = T.length fullHex
          atBoundary = T.null rest || not (isHexDigit' (T.head rest))
       in case () of
            _
              | len >= 8 && not (T.null rest) && T.head rest == '-' ->
                  case tryUUID txt of
                    Just r -> "{uuid}" <> go (Just '}') r
                    Nothing -> recognizeHex prev len atBoundary fullHex rest txt
              | otherwise -> recognizeHex prev len atBoundary fullHex rest txt

    recognizeHex :: Maybe Char -> Int -> Bool -> Text -> Text -> Text -> TLB.Builder
    recognizeHex !prev !len !atBoundary !fullHex !rest !txt = case len of
      64 | atBoundary -> "{sha256}" <> go (Just '}') rest
      40 | atBoundary -> "{sha1}" <> go (Just '}') rest
      32 | atBoundary -> "{md5}" <> go (Just '}') rest
      24 | atBoundary -> "{uuid}" <> go (Just '}') rest
      _ ->
        -- Fallback: emit one char at a time so digits within get replaced
        TLB.singleton (T.head txt) <> go (Just (T.head txt)) (T.drop 1 txt)

    -- Starts with digit
    scanDigit :: Maybe Char -> Text -> TLB.Builder
    scanDigit !prev !txt
      -- 0x hex literal
      | T.length txt >= 2 && T.head txt == '0' && T.index txt 1 == 'x' =
          let (hexRun, rest) = T.span isHexDigit' (T.drop 2 txt)
           in if T.null hexRun then "{integer}" <> go (Just '}') (T.drop 1 txt) else "{hex}" <> go (Just '}') rest
      | otherwise =
          let (digits, afterDigits) = T.span isDigit txt
              dLen = T.length digits
           in -- Timestamp: 4 digits + '-' → try YYYY-MM-DD...
              if dLen == 4 && not (T.null afterDigits) && T.head afterDigits == '-'
                then case tryTimestamp digits afterDigits of
                  Just (placeholder, rest) -> placeholder <> go (Just '}') rest
                  Nothing -> case tryUUID txt of
                    Just rest -> "{uuid}" <> go (Just '}') rest
                    Nothing -> trySSNOrFallback prev digits afterDigits txt
                -- Time: 1-2 digits + ':' → try HH:MM:SS
                else
                  if dLen <= 2 && dLen >= 1 && not (T.null afterDigits) && T.head afterDigits == ':'
                    then case tryTimeOnly digits (T.drop 1 afterDigits) of
                      Just (placeholder, rest) -> placeholder <> go (Just '}') rest
                      Nothing -> "{integer}" <> go (Just '}') afterDigits
                    -- UUID check: up to 8 hex digits + '-'
                    else
                      if dLen <= 8 && not (T.null afterDigits) && T.head afterDigits == '-'
                        then case tryUUID txt of
                          Just rest -> "{uuid}" <> go (Just '}') rest
                          Nothing -> trySSNOrFallback prev digits afterDigits txt
                        else classifyDigit prev txt digits afterDigits

    trySSNOrFallback :: Maybe Char -> Text -> Text -> Text -> TLB.Builder
    trySSNOrFallback !prev !digits !afterDigits !txt
      -- SSN: 3 digits + '-' → NNN-NN-NNNN
      | T.length digits == 3 && not (T.null afterDigits) && T.head afterDigits == '-' =
          case trySSN (T.drop 1 afterDigits) of
            Just rest -> "{ssn}" <> go (Just '}') rest
            Nothing -> classifyDigit prev txt digits afterDigits
      | otherwise = classifyDigit prev txt digits afterDigits

    classifyDigit :: Maybe Char -> Text -> Text -> Text -> TLB.Builder
    classifyDigit !_prev !txt !digits !afterDigits
      | T.null afterDigits = "{integer}" <> go (Just '}') afterDigits
      | T.head afterDigits == '.' = case tryIPv4 digits afterDigits of
          Just rest -> "{ipv4}" <> go (Just '}') rest
          Nothing ->
            let (decimals, rest) = T.span isDigit (T.drop 1 afterDigits)
             in if not (T.null decimals)
                  then "{float}" <> go (Just '}') rest
                  else "{integer}" <> TLB.singleton '.' <> go (Just '.') (T.drop 1 afterDigits)
      | isHexAlpha (T.head afterDigits) = classifyHexContinuation txt digits afterDigits
      | otherwise = "{integer}" <> go (Just '}') afterDigits

    classifyHexContinuation :: Text -> Text -> Text -> TLB.Builder
    classifyHexContinuation !txt !digits !afterDigits =
      let (fullHex, rest) = T.span isHexDigit' txt
          len = T.length fullHex
          atBoundary = T.null rest || not (isHexDigit' (T.head rest))
          fallback = "{integer}" <> go (Just '}') afterDigits
       in if
            | len == 64 && atBoundary -> "{sha256}" <> go (Just '}') rest
            | len == 40 && atBoundary -> "{sha1}" <> go (Just '}') rest
            | len == 32 && atBoundary -> "{md5}" <> go (Just '}') rest
            | len == 24 && atBoundary -> "{uuid}" <> go (Just '}') rest
            | len == 8 && not (T.null rest) && T.head rest == '-' ->
                maybe fallback (\r -> "{uuid}" <> go (Just '}') r) (tryUUID txt)
            | otherwise -> fallback

    -- Timestamp: YYYY-MM-DD, optionally followed by ' HH:MM:SS' or 'THH:MM:SS[.sss][Z/+TZ]'
    tryTimestamp :: Text -> Text -> Maybe (TLB.Builder, Text)
    tryTimestamp !yyyy !afterYear = do
      -- Already have 4 digits (yyyy) and afterYear starts with '-'
      r1 <- T.stripPrefix "-" afterYear
      let (mm, r2) = T.span isDigit r1
      guard $ T.length mm == 2
      r3 <- T.stripPrefix "-" r2
      let (dd, r4) = T.span isDigit r3
      guard $ T.length dd == 2
      -- Got YYYY-MM-DD. Check for time part
      case T.uncons r4 of
        Just (' ', r5) ->
          -- Space separator: YYYY-MM-DD HH:MM:SS
          case tryTimePart r5 of
            Just (_, rest) -> Just ("{YYYY-MM-DD HH:MM:SS}", rest)
            Nothing -> Just ("{YYYY-MM-DD}", r4)
        Just ('T', r5) ->
          -- ISO 8601: YYYY-MM-DDThh:mm:ss[.sss][Z/±TZ]
          case tryTimePart r5 of
            Just (_, rest) -> Just ("{YYYY-MM-DDThh:mm:ss.sTZD}", skipTZD rest)
            Nothing -> Just ("{YYYY-MM-DD}", r4)
        _ -> Just ("{YYYY-MM-DD}", r4)

    -- Parse HH:MM:SS[.sss] — returns (placeholder, rest)
    tryTimePart :: Text -> Maybe (TLB.Builder, Text)
    tryTimePart !txt = do
      let (hh, r1) = T.span isDigit txt
      guard $ T.length hh == 2
      r2 <- T.stripPrefix ":" r1
      let (mm, r3) = T.span isDigit r2
      guard $ T.length mm == 2
      r4 <- T.stripPrefix ":" r3
      let (ss, r5) = T.span isDigit r4
      guard $ T.length ss == 2
      -- Optional fractional seconds
      let rest = case T.uncons r5 of
            Just ('.', r6) -> let (_, r7) = T.span isDigit r6 in r7
            _ -> r5
      Just ("{HH:MM:SS}", rest)

    -- Skip timezone suffix: Z, +HH:MM, -HH:MM, +HHMM, -HHMM
    skipTZD :: Text -> Text
    skipTZD !txt = case T.uncons txt of
      Just ('Z', rest) -> rest
      Just (c, rest)
        | c == '+' || c == '-' ->
            let (d1, r1) = T.span isDigit rest
             in if T.length d1 >= 2
                  then case T.uncons r1 of
                    Just (':', r2) -> let (d2, r3) = T.span isDigit r2 in if T.length d2 == 2 then r3 else r1
                    _ -> r1
                  else txt
      _ -> txt

    -- Standalone time: HH:MM:SS[.mmm] — digits already consumed, rest starts after ':'
    tryTimeOnly :: Text -> Text -> Maybe (TLB.Builder, Text)
    tryTimeOnly !hh !afterColon = do
      let (mm, r1) = T.span isDigit afterColon
      guard $ T.length mm == 2
      r2 <- T.stripPrefix ":" r1
      let (ss, r3) = T.span isDigit r2
      guard $ T.length ss == 2
      let (placeholder, rest) = case T.uncons r3 of
            Just ('.', r4) ->
              let (frac, r5) = T.span isDigit r4
               in if not (T.null frac) then ("{HH:MM:SS.mmm}", r5) else ("{HH:MM:SS}", r3)
            _ -> ("{HH:MM:SS}", r3)
      Just (placeholder, rest)

    -- SSN: NN-NNNN (already consumed NNN and '-', rest starts after '-')
    trySSN :: Text -> Maybe Text
    trySSN !txt = do
      let (d2, r1) = T.span isDigit txt
      guard $ T.length d2 == 2
      r2 <- T.stripPrefix "-" r1
      let (d3, rest) = T.span isDigit r2
      guard $ T.length d3 == 4
      pure rest

    -- UUID: exactly 8-4-4-4-12 hex chars with dashes
    tryUUID :: Text -> Maybe Text
    tryUUID !txt = do
      (_, r1) <- consumeHexSeg 8 txt
      r1' <- T.stripPrefix "-" r1
      (_, r2) <- consumeHexSeg 4 r1'
      r2' <- T.stripPrefix "-" r2
      (_, r3) <- consumeHexSeg 4 r2'
      r3' <- T.stripPrefix "-" r3
      (_, r4) <- consumeHexSeg 4 r3'
      r4' <- T.stripPrefix "-" r4
      (_, r5) <- consumeHexSeg 12 r4'
      guard $ T.null r5 || not (isHexDigit' (T.head r5))
      pure r5

    consumeHexSeg :: Int -> Text -> Maybe (Text, Text)
    consumeHexSeg n t =
      let seg = T.take n t
       in if T.length seg == n && T.all isHexDigit' seg then Just (seg, T.drop n t) else Nothing

    -- IPv4: N.N.N.N where N is 0-255
    tryIPv4 :: Text -> Text -> Maybe Text
    tryIPv4 !firstOctet !afterFirst = do
      guard $ isOctet firstOctet
      r1 <- T.stripPrefix "." afterFirst
      let (o2, r2) = T.span isDigit r1
      guard $ not (T.null o2) && isOctet o2
      r2' <- T.stripPrefix "." r2
      let (o3, r3) = T.span isDigit r2'
      guard $ not (T.null o3) && isOctet o3
      r3' <- T.stripPrefix "." r3
      let (o4, rest) = T.span isDigit r3'
      guard $ not (T.null o4) && isOctet o4
      pure rest

    isOctet :: Text -> Bool
    isOctet t =
      let len = T.length t
       in len >= 1 && len <= 3 && case readMaybe (toString t) :: Maybe Int of
            Just n -> n >= 0 && n <= 255
            Nothing -> False

    isHexAlpha :: Char -> Bool
    isHexAlpha c = (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    isHexDigit' :: Char -> Bool
    isHexDigit' c = isDigit c || isHexAlpha c
