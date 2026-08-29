module Pkg.Components.TimePicker (
  parseTimeRange,
  timepicker_,
  refreshButton_,
  timeHiddenInputs_,
  TimePicker (..),
  TimeWindow (..),
  mkTimeWindow,
  windowUrl,
) where

import Data.Aeson qualified as AE
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Deriving.Aeson.Stock qualified as DAE
import Language.Haskell.TH.Syntax qualified as THS
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (termRaw)
import Lucid.Hyperscript (__)
import NeatInterpolation (text)
import Pkg.DeriveUtils (assetUrl)
import Relude hiding (some)
import Text.Megaparsec (Parsec, parse, some)
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (faSprite_, nonEmptyT, popoverPanel_, popoverTrigger_, timeScopedUrl)


-- $setup
-- >>> import Relude.Unsafe qualified as Unsafe


type Parser = Parsec Void Text


data TimePicker = TimePicker
  { since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  }
  deriving (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake TimePicker


-- | Test parseSince with different time units
-- >>> parseSince (Unsafe.read "2024-10-31 12:00:00 UTC") "2H"
-- (Just 2024-10-31 10:00:00 UTC,Just 2024-10-31 12:00:00 UTC,Just ("2H",""))
--
-- >>> parseSince (Unsafe.read "2024-10-31 12:00:00 UTC") "30M"
-- (Just 2024-10-31 11:30:00 UTC,Just 2024-10-31 12:00:00 UTC,Just ("30M",""))
--
-- >>> parseSince (Unsafe.read "2024-10-31 12:00:00 UTC") "7D"
-- (Just 2024-10-24 12:00:00 UTC,Just 2024-10-31 12:00:00 UTC,Just ("7D",""))
--
-- >>> parseSince (Unsafe.read "2024-10-31 12:00:00 UTC") "1h"
-- (Just 2024-10-31 11:00:00 UTC,Just 2024-10-31 12:00:00 UTC,Just ("1H",""))
parseSince :: UTCTime -> Text -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseSince now since =
  either (const (Nothing, Nothing, Nothing)) buildResult (parse timeParser "" since)
  where
    buildResult (num, secs) =
      ( Just $ addUTCTime (negate . secondsToNominalDiffTime $ fromIntegral (num * secs)) now
      , Just now
      , Just (T.toUpper since, "")
      )

    -- unknown units resolve to 0 seconds, i.e. a zero-width range rather than an unbounded one
    timeParser :: Parser (Int, Int)
    timeParser = (,) <$> decimal <*> (space *> (unitSecs . toText <$> some letterChar))
    unitSecs u = fromMaybe 0 $ lookup (T.toUpper u) [("S", 1), ("M", 60), ("H", 3600), ("D", 86400)]


-- | The one place the default time range is decided. Every layer (server SQL,
-- the picker label, the frontend) either forwards an explicit user pick through
-- here or defers to this — nobody else names a default.
defaultSince :: Text
defaultSince = "1H"


-- | Parse time range from TimePicker
-- Converts user input (since/from/to) into start and end times. Empty strings
-- are treated as absent, and any range that resolves to nothing falls back to
-- 'defaultSince' — so this can never emit an unbounded scan regardless of what
-- the client sends.
--
-- Test with since value (uses current time as end)
-- >>> parseTimeRange (Unsafe.read "2024-10-31 12:00:00 UTC") (TimePicker (Just "2H") Nothing Nothing)
-- (Just 2024-10-31 10:00:00 UTC,Just 2024-10-31 12:00:00 UTC,Just ("2H",""))
--
-- Test with from/to values
-- >>> parseTimeRange (Unsafe.read "2024-10-31 12:00:00 UTC") (TimePicker Nothing (Just "2024-10-31T08:00:00Z") (Just "2024-10-31T10:00:00Z"))
-- (Just 2024-10-31 08:00:00 UTC,Just 2024-10-31 10:00:00 UTC,Just ("2024-10-31 08:00:00","2024-10-31 10:00:00"))
--
-- Empty since/from/to must fall back to the default range, never an unbounded scan
-- >>> parseTimeRange (Unsafe.read "2024-10-31 12:00:00 UTC") (TimePicker (Just "") (Just "") (Just ""))
-- (Just 2024-10-31 11:00:00 UTC,Just 2024-10-31 12:00:00 UTC,Just ("1H",""))
parseTimeRange :: UTCTime -> TimePicker -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseTimeRange now tp = case (nonEmptyT tp.since, nonEmptyT tp.from, nonEmptyT tp.to) of
  (Just s, _, _) -> parseSince now s
  (_, Nothing, Nothing) -> parseSince now defaultSince
  (_, fromM, toM) -> case (parseUTCTime fromM, parseUTCTime toM) of
    (Nothing, Nothing) -> parseSince now defaultSince
    (f, t) -> (f, t, liftA2 (,) (fmtTime f) (fmtTime t))
  where
    parseUTCTime :: Maybe Text -> Maybe UTCTime
    parseUTCTime = iso8601ParseM . toString . fromMaybe ""
    fmtTime = fmap (toText . formatTime defaultTimeLocale "%F %T")


-----------------------------------------------------------------------------------------------------
-- Timepicker component. To be used at call site
-----------------------------------------------------------------------------------------------------
timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("5M", "Last 5 mins")
  , ("15M", "Last 15 mins")
  , ("30M", "Last 30 mins")
  , ("1H", "Last hour")
  , ("3H", "Last 3 hours")
  , ("6H", "Last 6 hours")
  , ("12H", "Last 12 hours")
  , ("24H", "Last 24 hours")
  , ("3D", "Last 3 days")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]


timepicker_ :: Maybe Text -> Maybe (Text, Text) -> Maybe Text -> Html ()
timepicker_ submitForm currentRange targetIdM = do
  let targetPr = fromMaybe "n" targetIdM
      isLive = maybe True (T.null . snd) currentRange
      displayRange = maybe "Last hour" (\(start, end) -> if T.null end then fromMaybe start (lookup start timePickerItems) else start <> " – " <> end) currentRange
      -- with a form we submit it; without one the caller-supplied fallback reloads/updates params
      submitVia noForm = maybe noForm (\fm -> [text|htmx.trigger("#${fm}", "submit")|]) submitForm
  -- read/written by window.updateTimePicker + window.getTimeRange (main.ts)
  input_ [type_ "hidden", id_ $ targetPr <> "-custom_range_input"]
  button_
    [ term "popovertarget" (targetPr <> "-timepicker-popover")
    , style_ $ "anchor-name:--" <> targetPr <> "-timepicker-anchor"
    , term "popovertargetaction" "toggle"
    , onclick_ "event.stopPropagation()"
    , class_ "flex min-h-9 items-center gap-2 max-md:gap-1.5 px-3 max-md:px-2 border border-strokeWeak rounded-lg shadow-xs text-sm text-textWeak cursor-pointer"
    , data_ "live-range" $ bool "false" "true" isLive
    ]
    do
      when isLive $ span_ [class_ "rounded bg-fillSuccess-strong px-1.5 py-0.5 text-xs font-semibold leading-none text-textInverse-strong", data_ "live-badge" ""] "LIVE"
      faSprite_ "calendar" "regular" "h-4 w-4 text-iconNeutral max-md:hidden"
      let attrs = maybe [] (\(s, e) -> [data_ "start" s, data_ "end" e]) currentRange
      span_ (attrs ++ [class_ "inline-block leading-none whitespace-nowrap", id_ $ targetPr <> "-currentRange"]) $ toHtml displayRange
      span_ [id_ $ targetPr <> "-offsetIndicator", class_ "text-xs text-textWeak max-md:hidden"] "UTC+00"
      faSprite_ "chevron-down" "regular" "h-3 w-3"

  div_ [class_ "relative w-max"] do
    div_
      [ class_ "border dropdown dropdown-end menu w-96 max-md:w-[calc(100vw-1rem)] rounded-box bg-bgRaised shadow-lg"
      , term "popover" "manual"
      , id_ $ targetPr <> "-timepicker-popover"
      , style_ $ "position-anchor:--" <> targetPr <> "-timepicker-anchor"
      ]
      do
        div_ [class_ "absolute top-0 left-0 z-50 hidden", id_ $ targetPr <> "-timepickerSidebar", [__| on click halt|]] $ div_ [id_ $ targetPr <> "-startTime", class_ "hidden"] ""
        ul_ [] do
          li_ [class_ "menu-title"] "Select Time Range"
          let action = submitVia "window.setQueryParamAndReload('since', my @data-value)"
          forM_ timePickerItems \(val, title) ->
            li_ $ button_
              [ class_ "flex items-center justify-between hover:bg-fillWeak rounded-lg px-3 py-2 w-full text-left"
              , data_ "value" val
              , data_ "label" title
              , termRaw "_" [text|on click call window.updateTimePicker({since: @data-value}, {targetPr: '${targetPr}', label: @data-label}) then $action then call #${targetPr}-timepicker-popover.hidePopover()|]
              ]
              do
                span_ [class_ "text-sm"] $ toHtml title
                span_ [class_ "text-xs text-textWeak"] $ toHtml val
          li_ $ button_
            [ class_ "w-full text-left"
            , term "_" [text| on click toggle .hidden on #$targetPr-timepickerSidebar |]
            ]
            do
              faSprite_ "calendar" "regular" "h-4 w-4 mr-2 text-iconNeutral"
              span_ "Custom date range"

        -- updateTimePicker already set the params; the formless case just reloads
        let submitAction = submitVia "window.setParams({}, true)"
            -- Self-hosted: easepick injects this into the picker's shadow root, so a
            -- jsdelivr blip left the date picker unstyled on an otherwise-working page.
            easepickCss = assetUrl "/public/assets/css/thirdparty/easepick.min.css"
        script_
          [text|
      (function() {
        const fmt = (d) => new Date(d).toLocaleString();
        const el = (suffix) => document.getElementById("$targetPr-" + suffix);
        const hideSidebar = () => el('timepickerSidebar').classList.add('hidden');
        // main.js and easepick are deferred, so poll until they land
        function initTimeDisplay() {
          if (typeof getUTCOffset === 'undefined') { setTimeout(initTimeDisplay, 50); return; }
          const offsetEl = el('offsetIndicator');
          if (offsetEl) offsetEl.innerText = getUTCOffset();
          const range = el('currentRange');
          if (!range) return;
          const { start, end } = range.dataset;
          if (start && end) range.innerText = `$${fmt(start)} - $${fmt(end)}`;
        }
        function initEasepick() {
          if (typeof easepick === 'undefined') { setTimeout(initEasepick, 100); return; }
          if (window["$targetPr-picker"]) return;
          window["$targetPr-picker"] = new easepick.create({
            element: '#$targetPr-startTime',
            css: ['${easepickCss}'],
            inline: true,
            plugins: ['RangePlugin', 'TimePlugin'],
            autoApply: false,
            documentClick: (e) => {
              if (e.target.classList.contains('easepick-wrapper')) return;
              hideSidebar();
              return true;
            },
            setup(picker) {
              picker.on("clear", hideSidebar);
              picker.on('select', ({ detail: { start, end } }) => {
                if (start.getTime() >= end.getTime()) end = new Date();
                window.updateTimePicker({from: start.toISOString(), to: end.toISOString()}, {targetPr: "$targetPr"});
                ${submitAction};
                el('timepicker-popover').hidePopover();
              });
            },
          });
        }
        initTimeDisplay();
        initEasepick();
      })()
    |]


-- | A resolved time range plus the query params it came from, so a page can query the store
-- and rebuild its own links from one value. Absent params default to the last 15 minutes.
data TimeWindow = TimeWindow
  { fromTime :: UTCTime
  , toTime :: UTCTime
  , currentRange :: Maybe (Text, Text)
  , fromQuery :: Maybe Text
  , toQuery :: Maybe Text
  , sinceQuery :: Maybe Text
  }


defaultWindow :: Text
defaultWindow = "5M"


mkTimeWindow :: UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> TimeWindow
mkTimeWindow now fromQuery toQuery sinceParam =
  let sinceQuery = nonEmptyT sinceParam <|> (defaultWindow <$ guard (all (isNothing . nonEmptyT) [fromQuery, toQuery]))
      (fromM, toM, currentRange) = parseTimeRange now $ TimePicker sinceQuery fromQuery toQuery
      (defaultFrom, _, _) = parseSince now defaultWindow
   in TimeWindow
        { fromTime = fromMaybe (fromMaybe now defaultFrom) fromM
        , toTime = fromMaybe now toM
        , currentRange
        , fromQuery
        , toQuery
        , sinceQuery
        }


-- | A URL under @base@ that carries this window's params forward, plus any extras.
windowUrl :: Text -> [(Text, Text)] -> TimeWindow -> Text
windowUrl base extras window = timeScopedUrl base extras window.fromQuery window.toQuery window.sinceQuery


timeHiddenInputs_ :: Maybe Text -> Maybe Text -> Maybe Text -> Html ()
timeHiddenInputs_ fromM toM sinceM = forM_ ([("from", fromM), ("to", toM), ("since", sinceM)] :: [(Text, Maybe Text)]) \(name, valueM) ->
  whenJust (nonEmptyT valueM) \value -> input_ [type_ "hidden", name_ name, value_ value]


refreshOptions :: [(Text, Text, Text)]
refreshOptions =
  [ ("Paused", "Pause live updates", "0")
  , ("15s", "15 seconds", "15000")
  , ("30s", "30 seconds", "30000")
  , ("1m", "1 minute", "60000")
  , ("5m", "5 minutes", "300000")
  , ("15m", "15 minutes", "900000")
  , ("30m", "30 minutes", "1800000")
  , ("1h", "1 hour", "3600000")
  , ("2h", "2 hours", "7200000")
  , ("1d", "1 day", "86400000")
  ]


-- | Datadog-style time transport shared by Explorer, Metrics, dashboards, and infrastructure.
-- A relative range starts live at 15 seconds. Stepping backward converts it to an absolute
-- range; stepping forward is disabled while live and resumes once the range is historical.
refreshButton_ :: Html ()
refreshButton_ =
  div_
    [ class_ "join min-h-9"
    , data_ "time-transport" ""
    , [__|on load call window.initTimeTransport(me)|]
    ]
    do
      transportBtn "Previous time window" "" [onclick_ "window.shiftTimeRange(-1, this.closest('[data-time-transport]'))"]
        $ faSprite_ "chevron-left" "regular" "h-3.5 w-3.5 text-iconNeutral"
      transportBtn "Pause live updates" "" [data_ "live-toggle" "", onclick_ "window.toggleLiveRefresh(this.closest('[data-time-transport]'))"] do
        span_ [data_ "pause-icon" ""] $ faSprite_ "pause" "solid" "h-3.5 w-3.5 text-iconBrand"
        span_ [data_ "play-icon" "", class_ "hidden"] $ faSprite_ "play" "solid" "h-3.5 w-3.5 text-iconNeutral"
      transportBtn "Next time window" " disabled:-ms-px disabled:bg-bgSunken disabled:text-textDisabled" [data_ "next-window" "", onclick_ "window.shiftTimeRange(1, this.closest('[data-time-transport]'))"]
        $ faSprite_ "chevron-right" "regular" "h-3.5 w-3.5 text-iconNeutral"
      transportBtn "Live update interval" "" (popoverTrigger_ "auto-refresh-pop")
        $ faSprite_ "chevron-down" "regular" "h-3 w-3 text-iconNeutral"
      ul_ ([class_ "dropdown dropdown-end menu p-2 shadow-lg bg-bgRaised rounded-box border border-strokeWeak mt-2 min-w-44"] <> popoverPanel_ "auto-refresh-pop") do
        li_ [class_ "menu-title"] "Live update interval"
        forM_ refreshOptions \(label, title, ms) ->
          li_
            $ button_
              [ type_ "button"
              , data_ "value" ms
              , data_ "tippy-content" title
              , onclick_ "window.setTimeRefreshInterval(this.closest('[data-time-transport]'), Number(this.dataset.value)); this.closest('[popover]').hidePopover()"
              ]
            $ toHtml label
  where
    transportBtn label extraClass attrs =
      button_
        $ [ type_ "button"
          , class_ $ "btn btn-sm join-item min-h-9 border-strokeWeak bg-bgBase px-2 shadow-xs" <> extraClass
          , Aria.label_ label
          , data_ "tippy-content" label
          ]
        <> attrs
