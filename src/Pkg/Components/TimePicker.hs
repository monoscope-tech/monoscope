module Pkg.Components.TimePicker (
  parseTimeRange,
  timepicker_,
  refreshButton_,
  TimePicker (..),
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
import Utils (faSprite_, nonEmptyT, popoverPanel_, popoverTrigger_)


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
      -- with a form we submit it; without one the caller-supplied fallback reloads/updates params
      submitVia noForm = maybe noForm (\fm -> [text|htmx.trigger("#${fm}", "submit")|]) submitForm
  -- read/written by window.updateTimePicker + window.getTimeRange (main.ts)
  input_ [type_ "hidden", id_ $ targetPr <> "-custom_range_input"]
  button_
    [ term "popovertarget" (targetPr <> "-timepicker-popover")
    , style_ $ "anchor-name:--" <> targetPr <> "-timepicker-anchor"
    , term "popovertargetaction" "toggle"
    , onclick_ "event.stopPropagation()"
    , class_ "flex items-center gap-2 max-md:gap-1.5 py-2 max-md:py-1.5 px-3 max-md:px-2 border border-strokeWeak rounded-lg shadow-xs text-sm text-textWeak cursor-pointer"
    ]
    do
      faSprite_ "calendar" "regular" "h-4 w-4 text-iconNeutral"
      let attrs = maybe [] (\(s, e) -> [data_ "start" s, data_ "end" e]) currentRange
      span_ (attrs ++ [class_ "inline-block leading-none", id_ $ targetPr <> "-currentRange"]) $ toHtml (maybe defaultSince (\(s, e) -> s <> if T.null e then "" else " - " <> e) currentRange)
      span_ [id_ $ targetPr <> "-offsetIndicator", class_ "text-2xs text-textWeak max-md:hidden"] "UTC+00"
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
              onClickHandler =
                [text|on click call window.updateTimePicker({since: @data-value}, {targetPr: '${targetPr}', label: @data-value}) then $action then call #${targetPr}-timepicker-popover.hidePopover()|]
          forM_ timePickerItems \(val, title) ->
            li_ $ button_
              [ class_ "flex items-center justify-between hover:bg-fillWeak rounded-lg px-3 py-2 w-full text-left"
              , data_ "value" val
              , termRaw "_" onClickHandler
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


refreshOptions :: [(Text, Text, Text)]
refreshOptions =
  [ ("paused", "Paused", "0")
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


-- | Refresh button with auto-refresh dropdown. Driven entirely by the global
-- @setRefreshInterval@ / @update-query@ event system, so it takes no arguments.
refreshButton_ :: Html ()
refreshButton_ = do
  div_ [class_ "join"] do
    label_
      [ class_ "cursor-pointer px-3 max-md:px-2 flex items-center border border-strokeWeak rounded-l-lg shadow-xs leading-none join-item"
      , data_ "tippy-content" "Refresh"
      , Aria.label_ "Refresh"
      , [__| on click trigger 'update-query' on document then
          add .animate-spin to the first <svg/> in me then wait 1 seconds then
          remove .animate-spin from the first <svg/> in me |]
      ]
      $ faSprite_ "arrows-rotate" "regular" "w-3.5 h-3.5 text-iconNeutral"
    div_ [class_ "leading-none join-item border-y border-r border-strokeWeak rounded-r-lg shadow-xs group/rf"] do
      button_ ([type_ "button", class_ "cursor-pointer py-2 px-3 max-md:px-2 flex gap-1.5 max-md:gap-1 items-center leading-none text-sm", data_ "tippy-content" "Auto-refresh interval"] <> popoverTrigger_ "auto-refresh-pop") do
        span_ [class_ "auto-refresh-span text-textWeak max-md:hidden", Aria.label_ "Auto-refresh interval"] "Paused"
        faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral"

      ul_ ([class_ "dropdown dropdown-end menu p-2 shadow-lg bg-bgRaised rounded-box border border-strokeWeak mt-2 min-w-40"] <> popoverPanel_ "auto-refresh-pop") do
        li_ [class_ "menu-title"] "Auto-refresh"
        forM_ refreshOptions \(label, title, ms) ->
          li_
            $ a_
              [ data_ "value" ms
              , data_ "tippy-content" title
              , [__| on click
                  set .auto-refresh-span.innerText to my.textContent then
                  send setRefreshInterval(interval: parseInt(@data-value)) to window then
                  call (closest <[popover]/>).hidePopover()
              |]
              ]
            $ toHtml label
