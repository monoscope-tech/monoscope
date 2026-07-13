module Utils (
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
  lookupVecIntByKey,
  lookupVecBoolByKey,
  lookupValueText,
  formatUTC,
  formatUTCMicros,
  insertIfNotExist,
  getAlertStatusColor,
  lookupVecTextByKey,
  deleteParam,
  unwrapJsonPrimValue,
  listToIndexHashMap,
  b64ToJson,
  FreeTierStatus (..),
  freeTierUsageBanner,
  checkFreeTierStatus,
  checkFreeTierExceeded,
  isDemoAndNotSudo,
  escapedQueryPartial,
  displayTimestamp,
  utcTimeToNanoseconds,
  getDurationNSMS,
  toUriStr,
  toXXHash,
  getServiceColors,
  serviceFillColor,
  -- Hex color mapping for ECharts server-side rendering
  themeColorsHex,
  getSeriesColorHex,
  nestedJsonFromDotNotation,
  prettyPrintCount,
  formatWithCommas,
  sanitizeBackendError,
  extractMessageFromLog,
  -- Fill color helpers
  statusFillColorText,
  methodFillColor,
  levelFillColor,
  replaceAllFormats,
  truncateHour,
  prettyTimeShort,
  formatOffset,
  navTabAttrs,
  popoverTrigger_,
  popoverPanel_,
  fieldMenuPanel_,
  drawerLoadAttrs_,
  renderMarkdown,
  jsonToMap,
  fieldContextMenuItems_,
  fieldMenuActions,
  FieldAction (..),
  FieldMenuCtx (..),
  renderSummaryTags,
  renderSummaryElements,
  summaryForDetailView,
  calculateCycleStartDate,
  -- NUL-byte scrubbing for PG `jsonb` ingest paths.
  scrubNulText,
  scrubNulValue,
)
where

import Data.Aeson as AE
import Data.Aeson.Extra.Merge (lodashMerge)
import Data.Aeson.Key (fromText)
import Data.Aeson.Key qualified as AEK
import Data.Aeson.KeyMap (lookup)
import Data.Aeson.KeyMap qualified as AEKM
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Default (Default (..))
import Data.Digest.XXHash (xxHash)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as TLB
import Data.Time (ZonedTime, addUTCTime, defaultTimeLocale, parseTimeM, secondsToNominalDiffTime)
import Data.Time.Calendar (fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime (..), diffUTCTime, secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (timeOfDayToTime, timeToTimeOfDay)
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Effectful (Eff, IOE, type (:>))
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxBoost_, hxGet_, hxSelect_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Lucid.Svg qualified as Svg
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.HTTP.Types (urlEncode)
import Network.URI (escapeURIString, isUnescapedInURI)
import Numeric (showHex)
import Pkg.DeriveUtils (hashFile)
import Relude hiding (notElem, show)
import Servant hiding ((:>))
import Text.MMark qualified as MMark
import Text.Printf (printf)
import Text.Regex.TDFA ((=~))
import Text.Show
import Text.Slugify (slugify)
import "base64" Data.ByteString.Base64 qualified as B64


-- Added only for satisfying the tests
instance Eq ZonedTime where
  (==) _ _ = True


escapedQueryPartial :: Text -> Text
escapedQueryPartial x = toText $ escapeURIString isUnescapedInURI $ toString x


data DBField = forall a. (Show a, ToField a) => MkDBField a


instance Show DBField where
  show (MkDBField a) = "MkDBField " ++ show a


instance ToField DBField where
  toField (MkDBField a) = toField a


onpointerdown_ :: Text -> Attribute
onpointerdown_ = term "onpointerdown"


-- | Field-label context for the shared context menu. 'StaticField' bakes the
-- field key (bold) and an optional display value into labels (inline log-item
-- tree, facet menu). 'DynamicField' renders @.ctx-key@/@.ctx-val@ placeholders
-- filled at click time by the LogItemMenuable behavior (cloned-template menu,
-- where the field isn't known until a pill is clicked).
data FieldMenuCtx = StaticField Text (Maybe Text) | DynamicField


-- | Actions in the shared field context menu, rendered in the order given;
-- 'FDivider' draws a separator. Every action reads the field from the nearest
-- @data-field-path@/@data-field-value@ ancestor, so callers must set those on an
-- enclosing element (or, for the cloned template, on the clicked pill).
data FieldAction
  = FCopyValue
  | FCopyKeyValue
  | FCopyField
  | FFilter
  | FExclude
  | FReplaceFilter
  | FGroupBy
  | FViewPatterns
  | FAddColumn
  | FDivider


-- | The full field-menu action list (Datadog order), shared by the inline
-- log-item tree and the cloned-template pill menu so the two can't drift.
fieldMenuActions :: [FieldAction]
fieldMenuActions =
  [ FCopyValue
  , FCopyKeyValue
  , FDivider
  , FFilter
  , FExclude
  , FReplaceFilter
  , FGroupBy
  , FViewPatterns
  , FDivider
  , FAddColumn
  ]


-- | Datadog-style shared context menu for log/trace/facet field actions. Single
-- source of truth for the log-item, cloned-template, and facet menus.
fieldContextMenuItems_ :: Monad m => FieldMenuCtx -> [FieldAction] -> HtmlT m ()
fieldContextMenuItems_ ctx = traverse_ \case
  FCopyValue ->
    menuItem_
      "copy"
      "Copy value"
      valTip
      [__|on click if 'clipboard' in window.navigator then
            call navigator.clipboard.writeText((closest @data-field-value))
            send successToast(value:['Copied to Clipboard']) to <body/>
            halt
          end|]
  FCopyKeyValue ->
    menuItem_
      "copy"
      "Copy key == value"
      kvTip
      [__|on click if 'clipboard' in window.navigator then
            call navigator.clipboard.writeText((closest @data-field-path) + ' == ' + (closest @data-field-value))
            send successToast(value:['Copied to Clipboard']) to <body/>
            halt
          end|]
  FCopyField ->
    menuItem_
      "copy"
      "Copy field"
      keyTip
      [__|on click if 'clipboard' in window.navigator then
            call navigator.clipboard.writeText((closest @data-field-path))
            send successToast(value:['Copied to Clipboard']) to <body/>
            halt
          end|]
  FFilter -> menuItem_ "filter" ("Filter by " <> keyVal_) kvTip $ onpointerdown_ "filterByField(event, 'Eq')"
  FExclude -> menuItem_ "filter-circle-xmark" ("Exclude " <> keyVal_) kvTip $ onpointerdown_ "filterByField(event, 'NotEq')"
  FReplaceFilter -> menuItem_ "arrow-rotate-left" ("Replace filter with " <> keyVal_) kvTip $ onpointerdown_ "filterByField(event, 'Replace')"
  FGroupBy ->
    menuItem_
      "layer-group"
      (span_ [class_ "gb-verb"] "Group by " <> key_)
      keyTip
      [__|
        init if document.querySelector('query-builder') call window.updateGroupByButtonText(event, me) end
        on refreshItem if document.querySelector('query-builder') call window.updateGroupByButtonText(event, me) end

        on click
          set qb to document.querySelector('query-builder')
          if qb
            call qb.toggleGroupByField(closest @data-field-path)
            trigger refreshItem on me
          end
        end
      |]
  FViewPatterns -> menuItem_ "chart-simple" ("Show patterns for " <> key_) keyTip $ onpointerdown_ "viewFieldPatterns(event.target.closest('[data-field-path]').dataset.fieldPath)"
  FAddColumn ->
    -- Label reflects current state; refreshed when the popover opens (by which point the
    -- <log-list> element has upgraded — refreshing on `init` raced its upgrade and threw).
    -- Guard on the method too, so a stray early `toggle` can't call a not-yet-upgraded element.
    menuItem_
      "table-columns"
      (span_ [class_ "col-verb"] "Add column for " <> key_)
      keyTip
      [__|
        on toggle from closest <[popover]/>
            if #resultTable and #resultTable.isColumnOnTable
              if #resultTable.isColumnOnTable(closest @data-field-path)
                set innerHTML of first <.col-verb/> in me to 'Remove column for '
              else
                set innerHTML of first <.col-verb/> in me to 'Add column for '
              end
            end
        on click
            if #resultTable and #resultTable.toggleColumnOnTable
              if #resultTable.toggleColumnOnTable(closest @data-field-path)
                set innerHTML of first <.col-verb/> in me to 'Remove column for '
              else
                set innerHTML of first <.col-verb/> in me to 'Add column for '
              end
            end
      |]
  FDivider -> li_ [class_ "pointer-events-none"] $ div_ [class_ "border-t border-strokeWeak my-1 -mx-2"] ""
  where
    -- Full (untruncated) key/value for tooltips; the label shows a shortened form.
    (keyTxt, valTxtM) = case ctx of StaticField k v -> (k, v); DynamicField -> ("", Nothing)
    valTip = fromMaybe "" valTxtM
    keyTip = keyTxt
    kvTip = keyTxt <> maybe "" (" == " <>) valTxtM
    key_ = b_ [class_ "ctx-key font-semibold text-textStrong"] $ case ctx of
      StaticField k _ -> toHtml k
      DynamicField -> "field"
    val_ = span_ [class_ "ctx-val"] $ case ctx of
      StaticField _ (Just v) -> toHtml (truncateMiddle 52 v)
      _ -> "value"
    keyVal_ = key_ <> span_ [class_ "text-textWeak"] " == " <> val_
    -- w-full so the item fills the fixed-width menu; the label then shrinks
    -- (min-w-0) and truncates instead of widening the menu / scrolling it.
    -- @tip@ (full key/value) becomes a native `title` tooltip so the truncated text
    -- is recoverable on hover.
    menuItem_ icon labelHtml tip action =
      li_ $ a_ ([class_ "flex gap-2 items-center flex-nowrap w-full", action] <> [term "title" tip | not (T.null tip)]) do
        faSprite_ icon "regular" "w-4 h-4 text-iconNeutral shrink-0"
        span_ [class_ "truncate min-w-0"] labelHtml


-- | Middle-truncate a display string to at most @n@ chars (keeps head + tail),
-- e.g. @k6NOL5dPP3Yzy9ZD…pI/B@ — for showing long values in menu labels.
truncateMiddle :: Int -> Text -> Text
truncateMiddle n t
  | T.length t <= n = t
  | otherwise = T.take (n - 6) t <> "…" <> T.takeEnd 5 t


-- | Render a Font Awesome icon from the sprite sheet at
-- static/public/assets/svgs/fa-sprites/{regular,solid}.svg. The first arg is
-- the symbol id (e.g. "bucket"). When adding a new icon, you must add its
-- <symbol> to the corresponding sprite file — it will not render otherwise.
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


-- | @scope@ must be unique per call site on a page: the popover ids/anchor-names
-- are derived from it, and the log-item detail renders several trees whose paths
-- overlap (e.g. the raw-data tab re-renders resource/attributes). Duplicate ids
-- would make native popovertarget resolve to the first match and break the menu.
jsonValueToHtmlTree :: Text -> AE.Value -> Maybe Text -> Html ()
jsonValueToHtmlTree scope val pathM = do
  div_ [class_ "p-2 rounded-lg bg-fillWeaker border w-full json-tree-container monospace text-sm leading-6"] do
    div_ [class_ "w-full flex items-center gap-4 text-xs mb-2"] do
      -- One toggle button: collapse state (checked = collapsed) is bulk-written to every
      -- node's .tree-toggle; the label reflects the next action. Imperative by nature (CSS
      -- can't write another element's checked state), so hyperscript is the right tool.
      when hasChildren $ button_
        [ class_ "flex items-center gap-1 cursor-pointer"
        , [__|on click
               set me.collapsed to not me.collapsed
               for it in <.tree-toggle/> in closest .json-tree-container set the checked of it to me.collapsed end
               if me.collapsed put 'Expand all' into the first <span/> in me
               else put 'Collapse all' into the first <span/> in me
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
            [text|on click
                 call navigator.clipboard.writeText(my @data-reqjson)
                 send successToast(value:['Json copied to clipboard']) to <body/>|]
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
    hasChildren = case val of AE.Object o -> not (AEKM.null o); AE.Array a -> not (V.null a); _ -> False
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (AEKM.toHashMapText v & HM.toList & sort & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (V.iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key, toText $ show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf "[*]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      let fieldPopId = "log-field-" <> scope <> "-" <> slugify fullFieldPath'
      let dfPath = replaceNumbers $ if isJust pathM then T.replace ".." "." fullFieldPath' else fullFieldPath'
          dfVal = unwrapJsonPrimValue False value
      div_
        [ class_ "log-item-field-parent block"
        , term "data-field-path" dfPath
        , term "data-field-value" dfVal
        ]
        do
          button_
            ([type_ "button", class_ "block w-full text-left hover:bg-fillBrandWeak cursor-pointer pl-6 log-item-field-anchor"] <> popoverTrigger_ fieldPopId)
            do
              unless (T.null key) do
                span_ $ toHtml key
                span_ [class_ "text-textBrand"] ":"
              span_ [class_ "text-textBrand ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml dfVal

          ul_ ([class_ "dropdown log-item-context-menu menu p-2 shadow-lg bg-bgRaised rounded-box border border-strokeWeak w-96 max-w-[92vw]"] <> fieldMenuPanel_ fieldPopId)
            $ fieldContextMenuItems_ (StaticField dfPath (Just dfVal)) fieldMenuActions

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    -- Collapse state is a hidden checkbox toggled by the label (no JS): CSS collapses the
    -- node via `.log-item-with-children:has(> label > .tree-toggle:checked)`. Checked = collapsed.
    renderParentType opening closing key count child = div_ [class_ "log-item-with-children"] do
      label_ [class_ "inline-block items-center cursor-pointer"] do
        input_ $ [type_ "checkbox", class_ "tree-toggle sr-only"] <> [checked_ | count == 0]
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


-- | Positional vector lookup: decode the i-th element via FromJSON.
lookupVec :: AE.FromJSON a => V.Vector AE.Value -> Int -> Maybe a
lookupVec vec idx = vec V.!? idx >>= AET.parseMaybe AE.parseJSON


-- | Key-indexed vector lookup via a column-index map.
lookupVecBy :: AE.FromJSON a => V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Maybe a
lookupVecBy vec colIdxMap key = HM.lookup key colIdxMap >>= lookupVec vec


lookupVecTextByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Maybe Text
lookupVecTextByKey = lookupVecBy


lookupVecBoolByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Bool
lookupVecBoolByKey v m k = fromMaybe False (lookupVecBy v m k)


lookupVecIntByKey :: V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Int
lookupVecIntByKey v m k = fromMaybe 0 (lookupVecBy v m k)


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
  | duration >= 60000000000 = printf "%.1f m" (fromIntegral @_ @Double duration / 60000000000)
  | duration >= 1000000000 = printf "%.1f s" (fromIntegral @_ @Double duration / 1000000000)
  | duration >= 1000000 = printf "%.1f ms" (fromIntegral @_ @Double duration / 1000000)
  | duration >= 1000 = printf "%.1f µs" (fromIntegral @_ @Double duration / 1000)
  | otherwise = printf "%.1f ns" (fromIntegral @_ @Double duration)


displayTimestamp :: Text -> Text
displayTimestamp inputDateString =
  maybe
    T.empty
    (toText . formatTime defaultTimeLocale "%b %d %H:%M")
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (toString inputDateString) :: Maybe UTCTime)


formatUTC :: UTCTime -> Text
formatUTC utcTime =
  toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" utcTime


-- | ISO-8601 with fixed 6-digit (microsecond) fractional seconds.
formatUTCMicros :: UTCTime -> Text
formatUTCMicros = toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ"


data FreeTierStatus = NotFreeTier | FreeTierOk | FreeTierWarning Int Int | FreeTierExceeded Int Int
  deriving stock (Eq, Generic, Show)


instance Default FreeTierStatus where def = NotFreeTier


freeTierUsageBanner :: Text -> FreeTierStatus -> Html ()
freeTierUsageBanner pid = \case
  FreeTierWarning used limit ->
    div_ [class_ "flex w-full text-center items-center px-4 gap-4 py-1 bg-fillWarning-weak text-textWarning rounded-lg justify-center"] do
      strong_ $ "You\x2019ve used " <> toHtml (formatWithCommas $ fromIntegral used) <> " of " <> toHtml (formatWithCommas $ fromIntegral limit) <> " daily events."
      a_ [class_ "underline underline-offset-2 link font-medium", href_ $ "/p/" <> pid <> "/manage_billing"] "Compare plans \x2192"
  FreeTierExceeded _ _ ->
    div_ [class_ "flex w-full text-center items-center px-4 gap-4 py-1 bg-fillError-weak text-textError rounded-lg justify-center"] do
      strong_ "Daily cap reached \x2014 new events are being dropped."
      a_ [class_ "underline underline-offset-2 link font-medium", href_ $ "/p/" <> pid <> "/manage_billing"] "Upgrade to keep sending events \x2192"
  _ -> pass


checkFreeTierStatus :: (Hasql.Hasql :> es, IOE :> es, Time :> es) => Projects.ProjectId -> Text -> Eff es FreeTierStatus
checkFreeTierStatus pid paymentPlan =
  if Projects.isFreeTier paymentPlan
    then do
      now <- Time.currentTime
      count <- fromMaybe (0 :: Int) <$> Hasql.interpOne [HI.sql| SELECT count(*)::BIGINT FROM otel_logs_and_spans WHERE project_id=#{pid.toText} AND timestamp > #{now}::timestamptz - interval '1 day'|]
      let limit = fromInteger freeTierDailyMaxEvents
      pure $ if count >= limit then FreeTierExceeded count limit else if count >= (limit * 80) `div` 100 then FreeTierWarning count limit else FreeTierOk
    else pure NotFreeTier


checkFreeTierExceeded :: (Hasql.Hasql :> es, IOE :> es, Time :> es) => Projects.ProjectId -> Text -> Eff es Bool
checkFreeTierExceeded pid pp = isExceeded <$> checkFreeTierStatus pid pp
  where
    isExceeded (FreeTierExceeded _ _) = True
    isExceeded _ = False


serviceColors :: V.Vector Text
serviceColors =
  V.fromList
    -- Ordered for maximum hue separation: any 3 consecutive colors are visually distinct
    [ "bg-blue-400" -- blue
    , "bg-red-400" -- red
    , "bg-green-400" -- green
    , "bg-amber-400" -- amber
    , "bg-purple-400" -- purple
    , "bg-teal-400" -- teal
    , "bg-orange-400" -- orange
    , "bg-sky-400" -- sky
    , "bg-rose-400" -- rose
    , "bg-lime-400" -- lime
    , "bg-indigo-400" -- indigo
    , "bg-yellow-400" -- yellow
    , "bg-pink-400" -- pink
    , "bg-emerald-400" -- emerald
    , "bg-violet-400" -- violet
    , "bg-cyan-400" -- cyan
    , "bg-fuchsia-400" -- fuchsia
    ]


getServiceColors :: V.Vector Text -> HashMap Text Text
getServiceColors = V.foldl' (\m s -> HM.insert s (serviceFillColor s) m) HM.empty


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
  | T.toLower name == "unset" = "#7c8db5"
  | HS.member (T.toLower name) nullishNames = "#9ca3af"
  | isStatusCode name = statusCodeColorHex (fromMaybe 0 $ readMaybe $ toString name)
  | isPercentile name = percentileColorHex name
  | otherwise = themeColorsHex V.! hashTextToIndex name
  where
    isStatusCode t = T.length t == 3 && T.all isDigit t && T.head t `elem` ("2345" :: String)
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
freeTierDailyMaxEvents = 10000


-- | Pretty print count numbers, converting large values to K/M/B format
-- | Example: 1534999 becomes 1.5M, 200000 becomes 200K
prettyPrintCount :: Int -> Text
prettyPrintCount n
  | n >= 1_000_000_000 = T.show (n `div` 1_000_000_000) <> "." <> T.show ((n `mod` 1_000_000_000) `div` 100_000_000) <> "B"
  | n >= 1_000_000 = T.show (n `div` 1_000_000) <> "." <> T.show ((n `mod` 1_000_000) `div` 100_000) <> "M"
  | n >= 1_000 = T.show (n `div` 1_000) <> "." <> T.show ((n `mod` 1_000) `div` 100) <> "K"
  | otherwise = T.show n


-- | Map a raw backend (Postgres / TimeFusion) error message to a short, safe
-- user-facing label. Raw detail stays in logs/spans, never the UI. Covers both
-- Postgres phrasing and TimeFusion's DataFusion-wrapped forms since prod reads
-- can hit either backend.
--
-- >>> sanitizeBackendError "This feature is not implemented: Unsupported SQL type jsonpath"
-- "This query isn't supported on the current data source"
-- >>> sanitizeBackendError "FATAL:  the database system is starting up"
-- "Database unavailable"
-- >>> sanitizeBackendError "ERROR: column \"foo\" does not exist"
-- "Column not found"
-- >>> sanitizeBackendError "Schema error: No field named bar"
-- "Column not found"
-- >>> sanitizeBackendError "some entirely novel failure"
-- "Query execution failed"
sanitizeBackendError :: Text -> Text
sanitizeBackendError raw
  | colNotFound = "Column not found"
  | tableNotFound = "Table not found"
  | "canceling statement due to" `T.isInfixOf` msg = "Query timed out"
  | "connection" `T.isInfixOf` msg && ("refused" `T.isInfixOf` msg || "closed" `T.isInfixOf` msg) = "Database unavailable"
  | "starting up" `T.isInfixOf` msg = "Database unavailable"
  | "not implemented" `T.isInfixOf` msg = "This query isn't supported on the current data source"
  | otherwise = "Query execution failed"
  where
    msg = T.toLower raw
    colNotFound =
      ("does not exist" `T.isInfixOf` msg && "column" `T.isInfixOf` msg)
        || "no field named"
        `T.isInfixOf` msg
        || "unknown column"
        `T.isInfixOf` msg
    tableNotFound =
      ("does not exist" `T.isInfixOf` msg && "relation" `T.isInfixOf` msg)
        || "unknown table"
        `T.isInfixOf` msg
        || ("table" `T.isInfixOf` msg && "not found" `T.isInfixOf` msg)


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


-- | Get service color class by hashing the service name (matches getServiceColors logic)
serviceFillColor :: Text -> Text
serviceFillColor name = serviceColors V.! (fromIntegral (xxHash (encodeUtf8 name)) `mod` V.length serviceColors)


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
--
-- >>> replaceAllFormats "1-Jan-2024 then 31-Dec-2025"
-- "{DD-Mon-YYYY} then {DD-Mon-YYYY}"
--
-- >>> replaceAllFormats "Event on 5-Mar-2023 at noon"
-- "Event on {DD-Mon-YYYY} at noon"
--
-- >>> replaceAllFormats "notJan 1, 2024"
-- "notJan {integer}, {integer}"
--
-- >>> replaceAllFormats "14-Oct-2023 and Oct 14, 2023"
-- "{DD-Mon-YYYY} and {Mon DD, YYYY}"
--
-- >>> replaceAllFormats "Sep 3, 2024 then 7-Sep-2024 end"
-- "{Mon DD, YYYY} then {DD-Mon-YYYY} end"
replaceAllFormats :: Text -> Text
replaceAllFormats !input = toText . TLB.toLazyText $ go Nothing (replacePrePass input)
  where
    -- Pre-pass: replace emails, JWTs, and text dates before the main scan
    replacePrePass :: Text -> Text
    replacePrePass = replaceJWTs . replaceEmails . replaceTextDates

    -- Replace text-based date formats: "Oct 14, 2023" and "14-Oct-2023" in a single pass.
    -- Uses a deferred "DD-" buffer so DD-Mon-YYYY can be detected without backtracking.
    monthSet :: HS.HashSet Text
    monthSet = HS.fromList ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

    replaceTextDates :: Text -> Text
    replaceTextDates !txt = toText . TLB.toLazyText $ scanMons Nothing Nothing txt

    -- prev: last emitted char (for boundary checks)
    -- deferred: a "DD-" prefix held back in case the next token is a month (Nothing or Just (prefixBeforeDD, ddDash))
    scanMons :: Maybe Char -> Maybe (TLB.Builder, Text) -> Text -> TLB.Builder
    scanMons !prev !deferred !t
      -- End of input: flush deferred and remaining text
      | T.null t = maybe mempty (\(pb, dd) -> pb <> TLB.fromText dd) deferred
      | T.length t < 3 = maybe mempty (\(pb, dd) -> pb <> TLB.fromText dd) deferred <> TLB.fromText t
      -- Check for month abbreviation
      | let mon = T.take 3 t
      , HS.member mon monthSet
      , maybe True (not . isAlpha) prev
      , monthBoundaryAfter (T.drop 3 t) =
          let afterMon = T.drop 3 t
           in case matchMonDY afterMon of
                Just rest ->
                  maybe mempty (\(pb, dd) -> pb <> TLB.fromText dd) deferred
                    <> TLB.fromText "{Mon DD, YYYY}"
                    <> scanMons (Just '}') Nothing rest
                Nothing -> case deferred of
                  Just (prefBefore, _ddDash)
                    | Just rest <- matchDMonYFwd afterMon ->
                        prefBefore <> TLB.fromText "{DD-Mon-YYYY}" <> scanMons (Just '}') Nothing rest
                  _ ->
                    maybe mempty (\(pb, dd) -> pb <> TLB.fromText dd) deferred
                      <> TLB.singleton (T.head t)
                      <> scanMons (Just (T.head t)) Nothing (T.drop 1 t)
      -- Detect "D-" or "DD-" that might precede a month: defer it
      | isNothing deferred
      , isDigit (T.head t)
      , let (ds, r) = T.span isDigit t
      , T.length ds <= 2
      , not (T.null r)
      , T.head r == '-'
      , let afterDash = T.drop 1 r
      , T.length afterDash >= 3
      , HS.member (T.take 3 afterDash) monthSet
      , maybe True (not . isAlphaNum) prev =
          scanMons (Just '-') (Just (mempty, ds <> "-")) afterDash
      -- Default: emit current char
      | otherwise =
          maybe mempty (\(pb, dd) -> pb <> TLB.fromText dd) deferred
            <> TLB.singleton (T.head t)
            <> scanMons (Just (T.head t)) Nothing (T.drop 1 t)

    -- Check "-YYYY" after month name for DD-Mon-YYYY pattern
    matchDMonYFwd :: Text -> Maybe Text
    matchDMonYFwd afterMon = do
      r1 <- T.stripPrefix "-" afterMon
      let (year, rest) = T.span isDigit r1
      guard $ T.length year == 4
      guard $ T.null rest || not (isAlphaNum (T.head rest))
      pure rest

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
    isOctet t = let len = T.length t in len >= 1 && len <= 3 && T.all isDigit t && T.foldl' (\acc c -> acc * 10 + (fromEnum c - 48)) 0 t <= (255 :: Int)

    isHexAlpha :: Char -> Bool
    isHexAlpha c = (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

    isHexDigit' :: Char -> Bool
    isHexDigit' c = isDigit c || isHexAlpha c


truncateHour :: UTCTime -> UTCTime
truncateHour (UTCTime day dt) = UTCTime day (secondsToDiffTime $ floor dt `div` 3600 * 3600)


prettyTimeShort :: UTCTime -> UTCTime -> Text
prettyTimeShort now t =
  let s = max 0 $ floor (diffUTCTime now t) :: Int
   in if
        | s < 60 -> T.show s <> " sec ago"
        | s < 3600 -> T.show (s `div` 60) <> " min ago"
        | s < 86400 -> let h = s `div` 3600 in T.show h <> bool " hrs" " hr" (h == 1) <> " ago"
        | s < 604800 -> let d = s `div` 86400 in T.show d <> bool " days" " day" (d == 1) <> " ago"
        | otherwise -> let w = s `div` 604800 in T.show w <> bool " wks" " wk" (w == 1) <> " ago"


-- | Forward-progress offset between two epoch-ms timestamps: @"+0s"@, @"+5s"@, @"+1m 23s"@, @"+2h 5m"@.
-- For timeline rails where the leading @+@ marks "elapsed since the base event", not "ago".
formatOffset :: Integer -> Integer -> Text
formatOffset base ts
  | s < 60 = "+" <> T.show s <> "s"
  | s < 3600 = "+" <> T.show (s `div` 60) <> "m " <> T.show (s `mod` 60) <> "s"
  | otherwise = "+" <> T.show (s `div` 3600) <> "h " <> T.show ((s `mod` 3600) `div` 60) <> "m"
  where
    s = max 0 $ (ts - base) `div` 1000


-- Reusable htmx attrs for tab-style nav links (preload + morph swap)
navTabAttrs :: [Attribute]
navTabAttrs =
  [ hxBoost_ "true"
  , hxTarget_ "#main-content"
  , hxSelect_ "#main-content"
  , term "hx-select-oob" "#main-sidenav:morph,#main-navbar:morph"
  , hxSwap_ "morph"
  , [__|on click set my.preloadState to 'DONE'|]
  ]


-- CSS anchor-positioned popover attrs (DaisyUI popover-target pattern), replacing focus-based `.dropdown`.
-- popoverTrigger_ goes on the trigger <button>; popoverPanel_ on the panel — both share the same id.
-- Neither carries a class_; the caller composes these with its own class_ (which must include the
-- DaisyUI `dropdown` base class that supplies position-area anchoring).
popoverTrigger_ :: Text -> [Attribute]
popoverTrigger_ pid = [term "popovertarget" pid, style_ $ "anchor-name:--anchor-" <> pid]


popoverPanel_ :: Text -> [Attribute]
popoverPanel_ pid = [id_ pid, term "popover" "auto", style_ $ "position-try:flip-block; position-anchor:--anchor-" <> pid]


-- | Panel attrs for the shared field context menu. Adds a bubbling click handler that
-- closes the popover after an item runs, and `halt`s so the click can't bubble to an
-- enclosing @<label for>@ (e.g. the facet-section collapse header) and toggle it.
fieldMenuPanel_ :: Text -> [Attribute]
fieldMenuPanel_ pid = popoverPanel_ pid <> [[__|on click call me.hidePopover() then halt|]]


-- | HTMX attrs that open the global data drawer and load @url@ into it.
drawerLoadAttrs_ :: Text -> [Attribute]
drawerLoadAttrs_ url =
  [ hxGet_ url
  , hxTarget_ "#global-data-drawer-content"
  , hxSwap_ "innerHTML"
  , term "hx-on::after-swap" "window.evalScriptsFromContent(htmx.find('#global-data-drawer-content'))"
  , term "_" "on pointerdown or click set #global-data-drawer.checked to true"
  ]


renderMarkdown :: Text -> Html ()
renderMarkdown md = case MMark.parse "" md of
  Left _ -> toHtml md
  Right doc -> toHtmlRaw $ MMark.render doc


jsonToMap :: AE.Value -> Maybe (Map Text AE.Value)
jsonToMap (AE.Object o) = Just $ AEKM.toMapText o
jsonToMap _ = Nothing


-- | Style mapping matching web-components/src/log-list-utils.ts STYLE_MAPPINGS
summaryStyleClass :: Text -> Text
summaryStyleClass = \case
  "badge-error" -> "badge badge-sm badge-error"
  "badge-warning" -> "badge badge-sm badge-warning"
  "badge-info" -> "badge badge-sm badge-info"
  "badge-success" -> "badge badge-sm badge-success"
  "badge-neutral" -> "badge badge-sm badge-neutral"
  "error-strong" -> "badge badge-sm badge-error"
  "error-weak" -> "badge badge-sm opacity-70 badge-error"
  "warning-strong" -> "badge badge-sm badge-warning"
  "info-strong" -> "badge badge-sm badge-info"
  "success-strong" -> "badge badge-sm badge-success"
  "neutral" -> "badge badge-sm badge-neutral"
  s | "right-badge-" `T.isPrefixOf` s -> "badge badge-sm ml-auto " <> T.drop 6 s
  s | "text-" `T.isPrefixOf` s -> s <> " text-xs"
  _ -> "badge badge-sm badge-neutral"


-- | Map a summary `field` (left side of `;style⇒value`) to the column path used
-- by the filter-menu (LogItemMenuable). Return Nothing for synthetic/derived
-- fields that don't map to a filterable column.
summaryFilterPath :: Text -> Maybe Text
summaryFilterPath = \case
  "method" -> Just "attributes.http.request.method"
  "status_code" -> Just "attributes.http.response.status_code"
  "status" -> Just "status_code"
  "route" -> Just "attributes.http.route"
  "url" -> Just "attributes.url.path"
  "service" -> Just "resource.service.name"
  "kind" -> Just "kind"
  "duration" -> Just "duration"
  "trace_id" -> Just "context.trace_id"
  "span_id" -> Just "context.span_id"
  "severity_text" -> Just "severity.severity_text"
  -- B3: accept the dotted OTel-style form the schema endpoint advertises so
  -- @severity.text == "ERROR"@ and @severity_text == "ERROR"@ both resolve
  -- to the actual column. Previously only the underscore form mapped.
  "severity.text" -> Just "severity.severity_text"
  "severity.severity_text" -> Just "severity.severity_text"
  "level" -> Just "severity.severity_text"
  "name" -> Just "name"
  "db.system" -> Just "attributes.db.system.name"
  "db.statement" -> Just "attributes.db.statement"
  "db.query.text" -> Just "attributes.db.query.text"
  "rpc.method" -> Just "attributes.rpc.method"
  "rpc.service" -> Just "attributes.rpc.service"
  "span_name" -> Just "name"
  "session" -> Just "attributes.session.id"
  "user email" -> Just "attributes.user.email"
  "user name" -> Just "attributes.user.full_name"
  _ -> Nothing


-- | Fields whose values can be long (URLs, routes, queries) — truncate to keep
-- the header scannable and surface the full value via tooltip.
summaryFieldIsLong :: Text -> Bool
summaryFieldIsLong f = f `elem` ["url", "route", "resource", "target", "db.statement", "db.query.text", "span_name"]


-- | Parse a `field;style⇒value` segment. Returns Nothing for malformed input.
parseSummaryEl :: Text -> Maybe (Text, Text, Text)
parseSummaryEl seg = do
  let (prefix, rest) = T.breakOn "⇒" seg
  value <- T.stripPrefix "⇒" rest
  let (field, styleSep) = T.breakOn ";" prefix
  style <- T.stripPrefix ";" styleSep
  pure (field, style, value)


-- | Render a single `field;style⇒value` element. When the field maps to a
-- known filter path, wraps the pill so clicking opens the LogItemMenuable menu
-- with the path/value pre-filled (same contract as Pages.LogExplorer.LogItem.spanBadge).
renderSummaryElement :: Text -> Html ()
renderSummaryElement seg = case parseSummaryEl seg of
  Just (field, style, value) ->
    let
      -- `badge` centers its content; for long values we want left-anchored truncation so `/foo/...` reads naturally.
      truncCls = if summaryFieldIsLong field then " max-w-sm truncate whitespace-nowrap !justify-start" else ""
      pillCls = summaryStyleClass style <> truncCls
      tooltip = field <> ": " <> value
     in
      case summaryFilterPath field of
        Just path ->
          div_
            [ class_ "relative min-w-0"
            , term "data-field-path" path
            , term "data-field-value" ("\"" <> value <> "\"")
            ]
            $ button_
              [ class_ $ "cursor-pointer " <> pillCls
              , term "data-tippy-content" tooltip
              , term "_" "install LogItemMenuable"
              ]
              (ansiToHtml value)
        Nothing ->
          span_ [class_ pillCls, title_ tooltip] (ansiToHtml value)
  Nothing -> span_ [class_ "text-xs text-textWeak"] $ ansiToHtml seg


-- | Render text containing ANSI SGR escape sequences (@\\ESC[...m@) as styled
-- Lucid HTML, mirroring the @ansi_up.ansi_to_html@ path that
-- @web-components/src/log-list-utils.ts@ uses for log-list summary values.
-- Hot-paths plain text (no ESC) through 'toHtml'. Unknown SGR codes are dropped.
--
-- >>> import Lucid (renderText)
-- >>> renderText (ansiToHtml "plain")
-- "plain"
-- >>> renderText (ansiToHtml "\ESC[31mboom\ESC[0m")
-- "<span class=\"text-textError\">boom</span>"
-- >>> renderText (ansiToHtml "a\ESC[1;32mOK\ESC[0mb")
-- "a<span class=\"font-bold text-textSuccess\">OK</span>b"
-- >>> renderText (ansiToHtml "\ESC[99mx")
-- "x"
-- >>> renderText (ansiToHtml "\ESC[33mwarn")
-- "<span class=\"text-textWarning\">warn</span>"
ansiToHtml :: Text -> Html ()
ansiToHtml t
  | not (T.isInfixOf "\ESC[" t) = toHtml t
  | otherwise = go t ""
  where
    go input cls
      | T.null input = pass
      | otherwise =
          let (plain, rest) = T.breakOn "\ESC[" input
              emitPlain =
                unless (T.null plain)
                  $ if T.null cls then toHtml plain else span_ [class_ cls] (toHtml plain)
           in case T.stripPrefix "\ESC[" rest of
                Nothing -> emitPlain
                Just rest' ->
                  let (codes, after) = T.breakOn "m" rest'
                   in case T.stripPrefix "m" after of
                        Nothing -> emitPlain
                        Just tl -> emitPlain >> go tl (updateCls cls codes)
    updateCls cur codes = stepCodes (mapMaybe (readMaybe @Int . toString) (T.splitOn ";" codes)) cur
    -- Skip 256-color (38/48;5;n) and truecolor (38/48;2;r;g;b) payloads so their
    -- numeric args don't leak back into sgrClass as spurious codes.
    stepCodes [] c = c
    stepCodes (0 : rs) _ = stepCodes rs ""
    stepCodes (38 : 5 : _ : rs) c = stepCodes rs c
    stepCodes (48 : 5 : _ : rs) c = stepCodes rs c
    stepCodes (38 : 2 : _ : _ : _ : rs) c = stepCodes rs c
    stepCodes (48 : 2 : _ : _ : _ : rs) c = stepCodes rs c
    stepCodes (n : rs) c = stepCodes rs $ case sgrClass n of
      Just new -> if T.null c then new else c <> " " <> new
      Nothing -> c
    sgrClass :: Int -> Maybe Text
    sgrClass = \case
      1 -> Just "font-bold"
      2 -> Just "text-textWeak"
      30 -> Just "text-textStrong"
      31 -> Just "text-textError"
      32 -> Just "text-textSuccess"
      33 -> Just "text-textWarning"
      34 -> Just "text-textBrand"
      35 -> Just "text-fuchsia-500"
      36 -> Just "text-cyan-500"
      37 -> Just "text-textStrong"
      90 -> Just "text-textWeak"
      91 -> Just "text-textError"
      92 -> Just "text-textSuccess"
      93 -> Just "text-textWarning"
      94 -> Just "text-textBrand"
      95 -> Just "text-fuchsia-400"
      96 -> Just "text-cyan-400"
      97 -> Just "text-textStrong"
      _ -> Nothing


-- | Render a space-separated `field;style⇒value` string from the DB summary
-- column. Whitespace inside values is lost by design (matches list-row contract).
renderSummaryTags :: Text -> Html ()
renderSummaryTags txt =
  span_ [class_ "inline-flex flex-wrap items-center gap-1"]
    $ forM_ (words txt) renderSummaryElement


-- | Render a pre-split summary vector (from Telemetry.generateSummary) as pills.
renderSummaryElements :: V.Vector Text -> Html ()
renderSummaryElements els =
  div_ [class_ "flex flex-wrap items-center gap-1.5 min-w-0"]
    $ V.forM_ els renderSummaryElement


-- | Adapt a summary vector for the log-item detail header:
--   1. drop list-row noise (raw JSON dumps of attributes/resource, redundant protocol);
--   2. strip `right-badge-` style (ml-auto right-alignment is meaningless in a wrapping header);
--   3. dedupe by (field, value) so `status ERROR` doesn't appear twice (left-side + right-side).
summaryForDetailView :: V.Vector Text -> V.Vector Text
summaryForDetailView = dedupe . V.mapMaybe step
  where
    skip el = any (`T.isPrefixOf` el) ["attributes;text-textWeak⇒", "resource;text-textWeak⇒", "protocol;"]
    step el
      | skip el = Nothing
      | otherwise = Just $ case parseSummaryEl el of
          Just (field, style, value) ->
            let style' = fromMaybe style (T.stripPrefix "right-badge-" style)
             in field <> ";" <> style' <> "⇒" <> value
          Nothing -> el
    key el = maybe ("", el) (\(f, _, v) -> (f, v)) (parseSummaryEl el)
    dedupe xs = V.fromList $ reverse $ snd $ V.foldl' step' (mempty, []) xs
      where
        step' (seen, acc) x
          | S.member (key x) seen = (seen, acc)
          | otherwise = (S.insert (key x) seen, x : acc)


-- | Given the subscription start date and current time, returns the UTC
-- timestamp of the start of the current billing cycle. The cycle renews on
-- the same day-of-month as the original start date.
calculateCycleStartDate :: UTCTime -> UTCTime -> UTCTime
calculateCycleStartDate start current =
  let (_startYear, _startMonth, startDay) = toGregorian $ utctDay start
      (currentYear, currentMonth, currentDay) = toGregorian $ utctDay current
      timeOfDay = timeToTimeOfDay $ utctDayTime start
      cycleStartDay
        | currentDay > startDay = fromGregorian currentYear currentMonth startDay
        | currentMonth == 1 = fromGregorian (currentYear - 1) 12 startDay
        | otherwise = fromGregorian currentYear (currentMonth - 1) startDay
   in UTCTime cycleStartDay (timeOfDayToTime timeOfDay)


-- | Strip NUL bytes from a 'Text'. Postgres' @jsonb@ rejects NUL in text
-- contexts (SQLSTATE @22P05@); span attributes occasionally carry one (k8s
-- events, base64-flagged fields). The guard makes the common case (no NUL) a
-- single linear scan with no allocation.
scrubNulText :: T.Text -> T.Text
scrubNulText t = if T.any (== '\NUL') t then T.filter (/= '\NUL') t else t


-- | Recursive 'scrubNulText' over an 'AE.Value' — strings, array elements,
-- and object keys+values. Untouched objects keep their underlying 'KeyMap'.
scrubNulValue :: AE.Value -> AE.Value
scrubNulValue = \case
  AE.String t -> AE.String (scrubNulText t)
  AE.Array xs -> AE.Array (fmap scrubNulValue xs)
  AE.Object o
    | Relude.any (T.any (== '\NUL') . AEK.toText) (AEKM.keys o) ->
        AE.Object
          $ AEKM.fromList
            [(AEK.fromText (scrubNulText (AEK.toText k)), scrubNulValue v) | (k, v) <- AEKM.toList o]
    | otherwise -> AE.Object (fmap scrubNulValue o)
  v -> v
