module Pkg.Components.TimePicker (
  defaultSince,
  parseTimeRange,
  LiveDataMode (..),
  liveDataControls_,
  timeHiddenInputs_,
  TimePicker (..),
  rangePairs,
  rangeJson,
  rangeQuery,
  TimeWindow (..),
  mkTimeWindow,
  windowUrl,
  cacheTtl,
) where

import Data.Aeson qualified as AE
import Data.List (lookup)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, secondsToNominalDiffTime)
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
import System.Clock (TimeSpec (TimeSpec))
import Text.Megaparsec (Parsec, parse, some)
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (faSprite_, formatUTC, nonEmptyT, popoverPanel_, popoverTrigger_, sinceWindows, timeScopedUrl, toUriStr)


-- $setup
-- >>> import Relude.Unsafe qualified as Unsafe
-- >>> import Data.Time (UTCTime (UTCTime), fromGregorian)
-- >>> let epoch = UTCTime (fromGregorian 2026 1 1) 0


type Parser = Parsec Void Text


data TimePicker = TimePicker
  { since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  }
  deriving (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake TimePicker


-- | The selected range as wire params, in the order the picker reads them, with
-- blanks dropped. Pages hand the same three values to seed scripts, @hx-vals@ and
-- Explorer links, so all three shapes below come from here.
--
-- >>> rangePairs (TimePicker (Just "24H") Nothing (Just ""))
-- [("since","24H")]
rangePairs :: TimePicker -> [(Text, Text)]
rangePairs tp = [(k, v) | (k, Just v) <- [("since", tp.since), ("from", tp.from), ("to", tp.to)], not (T.null v)]


-- | 'rangePairs' as a JSON object. @<@ is escaped for callers that embed it in an
-- inline @<script>@, where a literal @<@ can close the element.
--
-- >>> rangeJson (TimePicker Nothing (Just "2026-01-01T00:00:00Z") (Just ""))
-- "{\"from\":\"2026-01-01T00:00:00Z\"}"
rangeJson :: TimePicker -> Text
rangeJson = T.replace "<" "\\u003c" . decodeUtf8 . AE.encode . Map.fromList . rangePairs


-- | 'rangePairs' as query params, each already prefixed with @&@.
--
-- >>> rangeQuery (TimePicker (Just "24H") Nothing (Just "2026-01-01T00:00:00Z"))
-- "&since=24H&to=2026-01-01T00%3A00%3A00Z"
rangeQuery :: TimePicker -> Text
rangeQuery = foldMap (\(k, v) -> "&" <> k <> "=" <> toUriStr v) . rangePairs


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


-- | The default range for query surfaces (Explorer, dashboards): every layer
-- (server SQL, the picker label, the frontend) either forwards an explicit user
-- pick through here or defers to this. 'mkTimeWindow' pages use 'defaultWindow'
-- instead — those are the only two defaults in the app.
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
-- (Just 2024-10-31 08:00:00 UTC,Just 2024-10-31 10:00:00 UTC,Just ("2024-10-31T08:00:00Z","2024-10-31T10:00:00Z"))
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
    -- Browser Date parsing must retain the UTC instant in every viewer timezone.
    fmtTime = fmap formatUTC


-----------------------------------------------------------------------------------------------------
-- Timepicker component. To be used at call site
-----------------------------------------------------------------------------------------------------

-- | Derived from 'Utils.sinceWindows' so the dropdown and the labels
-- 'Utils.parseTime' resolves are the same strings by construction.
timePickerItems :: [(Text, Text)]
timePickerItems = map (second snd) sinceWindows


timepickerRangeOnly_ :: Maybe Text -> Maybe (Text, Text) -> Maybe Text -> Html ()
timepickerRangeOnly_ = timepickerWithStatus_ False


data LiveDataMode = RefreshOnly | RowStreaming Bool


-- | One source of truth for the header's time range, transport, and live-data settings.
-- 'RefreshOnly' omits row streaming for surfaces such as dashboards. 'RowStreaming False'
-- keeps that setting visible but unavailable for Explorer views that cannot stream rows.
liveDataControls_ :: Maybe Text -> Maybe (Text, Text) -> Maybe Text -> LiveDataMode -> Html ()
liveDataControls_ submitForm currentRange targetIdM liveDataMode =
  div_
    [ class_ "flex items-center gap-4 max-lg:gap-2"
    , data_ "live-data" ""
    , data_ "live-mode" $ case liveDataMode of RefreshOnly -> "refresh-only"; RowStreaming _ -> "row-streaming"
    , data_ "state" "paused"
    , [__|on change if event.target.matches('[data-row-stream-toggle]') call window.syncTimeTransports() end
          on "live-data-state-change" call window.syncTimeTransports()|]
    ]
    do
      div_ [class_ "flex items-center gap-2", data_ "header-time" ""] do
        timepickerRangeOnly_ submitForm currentRange targetIdM
        refreshButtonWithPopover_ popoverId (maybe "Refresh interval" (const "Live data settings") streamingSupport) liveDataPanel
  where
    targetPr = fromMaybe "n" targetIdM
    popoverId = targetPr <> "-live-data-pop"
    liveDataPanel =
      div_
        ( popoverPanel_ popoverId
            <> [ class_ "dropdown dropdown-end mt-2 w-72 rounded-xl border border-strokeWeak bg-bgRaised p-2 text-sm shadow-lg"
               , role_ "dialog"
               , Aria.label_ $ maybe "Refresh interval" (const "Live data controls") streamingSupport
               ]
        )
        $ do
          case streamingSupport of
            Nothing -> do
              div_ [class_ "px-2 pb-2 pt-1"] do
                div_ [class_ "font-semibold text-textStrong"] "Refresh interval"
                p_ [class_ "mt-0.5 text-xs text-textWeak"] "Choose how often charts, counts, and the query refresh."
              div_ [class_ "grid grid-cols-2 gap-1", role_ "group", Aria.label_ "Refresh interval", data_ "refresh-interval-menu" ""]
                $ forM_ refreshOptions refreshIntervalOption_
            Just supportsStreaming -> do
              div_ [class_ "px-2 pb-2 pt-1"] do
                div_ [class_ "font-semibold text-textStrong"] "Live updates"
                p_ [class_ "mt-0.5 text-xs text-textWeak"] "Choose how this view receives new data."
              label_ [class_ $ "flex min-h-11 items-center gap-3 rounded-lg px-2 py-2 hover:bg-fillWeak" <> bool " opacity-60 cursor-not-allowed" " cursor-pointer" supportsStreaming] do
                div_ [class_ "min-w-0 flex-1"] do
                  div_ [class_ "font-medium text-textStrong"] "Stream new events"
                  span_
                    [ class_ "text-xs text-textWeak"
                    , data_ "row-stream-status" ""
                    , term "hx-live:text" "closest('[data-live-data]').q('[data-row-stream-toggle]').disabled ? 'Unavailable in this view' : closest('[data-live-data]').q('[data-row-stream-toggle]').checked ? 'On' : 'Paused'"
                    ]
                    $ if supportsStreaming then "Paused" else "Unavailable in this view"
                input_ $ [type_ "checkbox", id_ "streamLiveData", class_ "toggle toggle-sm", term "aria-label" "Stream new events", data_ "row-stream-toggle" ""] <> [disabled_ "" | not supportsStreaming]
              div_ [class_ "space-y-2 px-2 py-2"] do
                label_ [class_ "block", Lucid.for_ $ targetPr <> "-live-refresh-interval"] do
                  div_ [class_ "font-medium text-textStrong"] "Refresh results"
                  span_ [class_ "text-xs text-textWeak"] "Re-run charts, counts, and the query."
                refreshIntervalSelect_ targetPr
          span_
            [ class_ "sr-only"
            , data_ "live-data-announcer" ""
            , role_ "status"
            , Aria.live_ "polite"
            , Aria.atomic_ "true"
            , term "hx-live:text" "closest('[data-live-data]').data.state == 'historical' ? 'Historical' : closest('[data-live-data]').data.state == 'live' ? 'Live data' : closest('[data-live-data]').data.state == 'refresh-paused' ? 'Refresh paused' : closest('[data-live-data]').data.state == 'stream-paused' ? 'Event stream paused' : 'Paused'"
            ]
            $ maybe "Live updates" (bool "Live updates" "Event stream paused") streamingSupport
    streamingSupport = case liveDataMode of
      RefreshOnly -> Nothing
      RowStreaming supportsStreaming -> Just supportsStreaming


timepickerWithStatus_ :: Bool -> Maybe Text -> Maybe (Text, Text) -> Maybe Text -> Html ()
timepickerWithStatus_ showLiveStatus submitForm currentRange targetIdM = do
  let targetPr = fromMaybe "n" targetIdM
      isLive = maybe True (T.null . snd) currentRange
      displayRange = maybe "Last hour" (\(start, end) -> if T.null end then fromMaybe start (lookup start timePickerItems) else start <> " – " <> end) currentRange
      -- with a form we submit it; without one the page's widgets refetch in place
      submitVia noForm = maybe noForm (\fm -> [text|htmx.trigger("#${fm}", "submit")|]) submitForm
  -- read/written by window.updateTimePicker + window.getTimeRange (main.ts)
  input_ [type_ "hidden", id_ $ targetPr <> "-custom_range_input"]
  button_
    [ term "popovertarget" (targetPr <> "-timepicker-popover")
    , style_ $ "anchor-name:--" <> targetPr <> "-timepicker-anchor"
    , term "popovertargetaction" "toggle"
    , onclick_ "event.stopPropagation()"
    , class_ "group/range flex min-w-0 max-w-full h-8 max-md:min-h-11 items-center gap-2 max-md:gap-1.5 px-3 max-md:px-2 border border-strokeWeak bg-bgRaised rounded-lg shadow-xs text-sm text-textWeak cursor-pointer hover:border-strokeStrong hover:bg-fillWeak focus-visible:border-strokeFocus"
    , data_ "live-range" $ bool "false" "true" isLive
    , data_ "state" $ bool "historical" "live" isLive
    ]
    do
      when showLiveStatus
        $ span_
          [ class_ "rounded bg-fillWeak px-1.5 py-0.5 text-xs font-semibold leading-none text-textWeak group-data-[state=live]/range:bg-fillSuccess-strong group-data-[state=live]/range:text-textInverse-strong group-data-[state=paused]/range:bg-fillWarning-strong group-data-[state=paused]/range:text-textInverse-strong"
          , data_ "live-badge" ""
          , term "hx-live:text" "closest('[data-live-range]').data.state == 'historical' ? 'HISTORICAL' : closest('[data-live-range]').data.state == 'live' ? 'LIVE' : 'PAUSED'"
          ]
        $ bool "HISTORICAL" "LIVE" isLive
      faSprite_ "calendar" "regular" "h-4 w-4 text-iconNeutral max-md:hidden"
      let attrs = maybe [] (\(s, e) -> [data_ "start" s, data_ "end" e]) currentRange
      span_ (attrs ++ [class_ "inline-block min-w-0 leading-snug text-left whitespace-normal md:whitespace-nowrap", id_ $ targetPr <> "-currentRange"]) $ toHtml displayRange
      span_ [id_ $ targetPr <> "-offsetIndicator", class_ "text-xs text-textWeak max-md:hidden"] "UTC+0"
      faSprite_ "chevron-down" "regular" "h-3 w-3"

  div_ [class_ "contents", data_ "time-picker-root" ""] do
    div_
      [ class_ "time-range-popover border dropdown dropdown-end menu w-96 rounded-box bg-bgRaised shadow-lg"
      , term "popover" "manual"
      , id_ $ targetPr <> "-timepicker-popover"
      , style_ $ "position-anchor:--" <> targetPr <> "-timepicker-anchor"
      ]
      do
        div_ [class_ "time-range-custom hidden", id_ $ targetPr <> "-timepickerSidebar"] do
          button_
            [ type_ "button"
            , class_ "flex items-center gap-2 px-3 py-2 min-h-11 text-sm text-textBrand"
            , term "_" [text|on click add .hidden to #$targetPr-timepickerSidebar then call #$targetPr-customRangeTrigger.focus()|]
            ]
            do
              faSprite_ "arrow-left" "regular" "h-4 w-4"
              "Preset ranges"
          div_ [id_ $ targetPr <> "-startTime", class_ "hidden"] ""
        ul_ [] do
          li_ [class_ "menu-title"] "Select Time Range"
          let action = submitVia "window.dispatchQueryUpdate()"
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
            , id_ $ targetPr <> "-customRangeTrigger"
            , term "_" [text| on click remove .hidden from #$targetPr-timepickerSidebar then call #$targetPr-timepickerSidebar.querySelector('button').focus() |]
            ]
            do
              faSprite_ "calendar" "regular" "h-4 w-4 mr-2 text-iconNeutral"
              span_ "Custom date range"
          li_ [class_ "menu-title md:hidden"] "Live and navigation"
          li_ [class_ "md:hidden"] $ div_ [class_ "grid grid-cols-3 gap-1"] do
            button_
              [ type_ "button"
              , class_ "inline-flex min-h-11 items-center justify-center gap-1 rounded-lg hover:bg-fillWeak"
              , Aria.label_ "Previous time window"
              , [__|on click call window.shiftTimeRange(-1, me.closest('[data-time-picker-root]').parentElement.querySelector('[data-time-transport]'))|]
              ]
              $ faSprite_ "chevron-left" "regular" "h-4 w-4 text-iconNeutral"
            button_
              [ type_ "button"
              , class_ "inline-flex min-h-11 items-center justify-center gap-1 rounded-lg px-2 hover:bg-fillWeak"
              , data_ "mobile-live-toggle" ""
              , term "hx-live:aria-label" "(closest('[data-time-picker-root]').parentElement.q('[data-time-transport]').data.state == 'live' ? 'Pause' : 'Resume') + ' live data'"
              , [__|on click call window.toggleLiveData(me.closest('[data-live-data]'), me.closest('[data-time-picker-root]').parentElement.querySelector('[data-time-transport]'))|]
              ]
              do
                faSprite_ "pause" "solid" "h-3.5 w-3.5 text-iconNeutral"
                span_ [term "hx-live:text" "closest('[data-time-picker-root]').parentElement.q('[data-time-transport]').data.state == 'live' ? 'Pause' : 'Resume'"] "Pause"
            button_
              [ type_ "button"
              , class_ "inline-flex min-h-11 items-center justify-center gap-1 rounded-lg hover:bg-fillWeak disabled:text-textDisabled"
              , Aria.label_ "Next time window"
              , data_ "mobile-next-window" ""
              , term "hx-live:disabled" "closest('[data-time-picker-root]').parentElement.q('[data-time-transport]').data.live == 'true'"
              , [__|on click call window.shiftTimeRange(1, me.closest('[data-time-picker-root]').parentElement.querySelector('[data-time-transport]'))|]
              ]
              $ faSprite_ "chevron-right" "regular" "h-4 w-4 text-iconNeutral"
          li_ [class_ "menu-title md:hidden"] "Refresh interval"
          li_ [class_ "md:hidden"] $ div_ [class_ "space-y-1", role_ "group", Aria.label_ "Refresh interval"] $ forM_ refreshOptions refreshIntervalOption_

        let submitAction = submitVia "window.dispatchQueryUpdate()"
            -- Self-hosted: easepick injects this into the picker's shadow root, so a
            -- jsdelivr blip left the date picker unstyled on an otherwise-working page.
            easepickCss = assetUrl "/public/assets/css/thirdparty/easepick.min.css"
        script_
          [text|
      (function() {
        const el = (suffix) => document.getElementById("$targetPr-" + suffix);
        const hideSidebar = () => el('timepickerSidebar').classList.add('hidden');
        function initTimeDisplay() {
          const zoneEl = el('offsetIndicator');
          if (zoneEl && window.getUTCOffset) zoneEl.innerText = window.getUTCOffset();
          const range = el('currentRange');
          if (!range) return;
          const { start, end } = range.dataset;
          if (start && end && window.formatTimeRange) range.innerText = window.formatTimeRange(start, end);
        }
        window.addEventListener('monoscope:time-format-ready', initTimeDisplay, {once: true});
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


-- | The default range for 'mkTimeWindow' surfaces (infrastructure, RUM), which
-- refresh live and so open on a tighter window than 'defaultSince'.
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
-- | How long a read over this window may be served from cache.
--
-- A cached answer drifts from the truth at a rate set by the window it covers: five minutes of
-- staleness is 0.3% of a 24-hour count and invisible, but it is the whole of a five-minute one.
-- So the budget is a fraction of the window rather than one number for every view.
--
-- The floor matters more than the ceiling. A cache whose entries expire before the page that
-- fills them has finished building is worse than no cache — it pays the write and never serves
-- a read. RUM at 24h took 8-22s to assemble and the infrastructure pages 32-60s, against a flat
-- 15s expiry, so not one of those reads was ever reused. The ceiling matches the sibling
-- endpoint and host stats caches.
--
-- >>> cacheTtl (mkTimeWindow epoch Nothing Nothing (Just "24H"))
-- TimeSpec {sec = 300, nsec = 0}
-- >>> cacheTtl (mkTimeWindow epoch Nothing Nothing (Just "1H"))
-- TimeSpec {sec = 30, nsec = 0}
-- >>> cacheTtl (mkTimeWindow epoch Nothing Nothing (Just "5M"))
-- TimeSpec {sec = 30, nsec = 0}
cacheTtl :: TimeWindow -> TimeSpec
cacheTtl window = TimeSpec (max 30 $ min 300 $ round (diffUTCTime window.toTime window.fromTime) `div` 120) 0


windowUrl :: Text -> [(Text, Text)] -> TimeWindow -> Text
windowUrl base extras window = timeScopedUrl base extras window.fromQuery window.toQuery window.sinceQuery


timeHiddenInputs_ :: Maybe Text -> Maybe Text -> Maybe Text -> Html ()
timeHiddenInputs_ fromM toM sinceM = forM_ ([("from", fromM), ("to", toM), ("since", sinceM)] :: [(Text, Maybe Text)]) \(name, valueM) ->
  whenJust (nonEmptyT valueM) \value -> input_ [type_ "hidden", name_ name, value_ value]


refreshOptions :: [(Text, Text, Text)]
refreshOptions =
  [ ("Off", "Turn off automatic refresh", "0")
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


refreshIntervalOption_ :: (Text, Text, Text) -> Html ()
refreshIntervalOption_ (label, title, ms) =
  button_
    [ type_ "button"
    , class_ "group/refresh-option flex min-h-11 w-full items-center justify-between rounded-lg px-3 py-2 text-start text-sm hover:bg-fillWeak focus-visible:outline-2 focus-visible:outline-offset-1 data-[selected=true]:bg-fillWeak data-[selected=true]:font-semibold"
    , Aria.label_ title
    , data_ "value" ms
    , data_ "refresh-option" ""
    , term "aria-pressed" "false"
    , term "hx-live:aria-pressed" "closest('[data-live-data]').q('[data-time-transport]').data.interval == data.value"
    , term "hx-live:data-selected" "closest('[data-live-data]').q('[data-time-transport]').data.interval == data.value"
    , [__|on click call window.setTimeRefreshInterval(me.closest('[data-live-data]').q('[data-time-transport]'), Number(my.dataset.value)) then call me.closest('[popover]').hidePopover()|]
    ]
    do
      span_ $ toHtml label
      span_ [class_ "opacity-0 group-data-[selected=true]/refresh-option:opacity-100", Aria.hidden_ "true"] $ faSprite_ "check" "regular" "h-3 w-3 text-iconBrand"


-- | Compact cadence picker for richer live-data panels. Keeping it beside the
-- shared refresh options prevents Explorer from silently losing intervals that
-- remain available on dashboards and other telemetry pages.
refreshIntervalSelect_ :: Text -> Html ()
refreshIntervalSelect_ targetPr =
  select_
    [ class_ "select select-sm h-8 w-full bg-bgBase"
    , id_ $ targetPr <> "-live-refresh-interval"
    , data_ "refresh-select" ""
    , Aria.label_ "Refresh results interval"
    , term "hx-live" "this.value = closest('[data-time-transport]').data.interval"
    , [__|on change call window.setTimeRefreshInterval(me.closest('[data-time-transport]'), Number(my.value))|]
    ]
    $ forM_ refreshOptions \(_, title, ms) -> option_ [value_ ms] $ toHtml $ bool title "Off" (ms == "0")


-- | The transport and its settings are one control group. Explorer supplies its broader
-- live-data panel here so row streaming does not appear as a competing top-level control.
refreshButtonWithPopover_ :: Text -> Text -> Html () -> Html ()
refreshButtonWithPopover_ popoverId popoverLabel popoverPanel =
  div_
    [ class_ "group/transport flex h-8 items-center gap-0 rounded-lg border border-strokeWeak bg-bgRaised p-0.5 shadow-xs hover:border-strokeStrong focus-within:border-strokeFocus max-md:hidden"
    , data_ "time-transport" ""
    , [__|on load if window.initTimeTransport call window.initTimeTransport(me) end
          on "monoscope:time-transport-ready" from window call window.initTimeTransport(me)
          on htmx:beforeCleanupElement if window.destroyTimeTransport call window.destroyTimeTransport(me) end|]
    ]
    do
      transportBtn "Previous time window" "" [[__|on click call window.shiftTimeRange(-1, me.closest('[data-time-transport]'))|]]
        $ faSprite_ "chevron-left" "regular" "h-3.5 w-3.5 text-iconNeutral opacity-80"
      transportDivider
      transportBtn "Pause live updates" " min-w-24 px-3" [data_ "live-toggle" "", term "hx-live:aria-label" "closest('[data-time-transport]').data.state == 'live' ? (closest('[data-live-data]').count ? 'Pause live data' : 'Pause live updates') : closest('[data-time-transport]').data.state == 'historical' ? 'Return to live' : (closest('[data-live-data]').count ? 'Resume live data' : 'Resume live updates')", term "hx-live:aria-pressed" "closest('[data-time-transport]').data.state == 'live'", [__|on click call window.toggleLiveData(me.closest('[data-live-data]'), me.closest('[data-time-transport]'))|]] do
        span_ [data_ "pause-icon" "", class_ "hidden items-center justify-center align-middle leading-none group-data-[state=live]/transport:inline-flex"] $ faSprite_ "pause" "solid" "h-3.5 w-3.5 text-iconBrand"
        span_ [data_ "play-icon" "", class_ "inline-flex items-center justify-center align-middle leading-none translate-x-px group-data-[state=live]/transport:hidden"] $ faSprite_ "play" "solid" "h-3.5 w-3.5 text-iconNeutral"
        span_ [data_ "refresh-label" "", class_ "hidden lg:inline text-xs font-medium leading-none", term "hx-live:text" "closest('[data-time-transport]').data.state == 'live' ? 'Pause' : closest('[data-time-transport]').data.state == 'historical' ? 'Historical' : 'Resume'"] "Pause"
      transportDivider
      transportBtn "Next time window" " disabled:bg-transparent disabled:text-textDisabled" [data_ "next-window" "", term "hx-live:disabled" "closest('[data-time-transport]').data.live == 'true'", [__|on click call window.shiftTimeRange(1, me.closest('[data-time-transport]'))|]]
        $ faSprite_ "chevron-right" "regular" "h-3.5 w-3.5 text-iconNeutral opacity-80"
      transportDivider
      transportBtn popoverLabel "" (data_ "live-data-trigger" "" : term "hx-live:aria-label" "closest('[data-live-data]').count ? 'Live data: ' + closest('[data-live-data]').data.state.replace('-', ' ') : 'Live update interval'" : popoverTrigger_ popoverId)
        $ faSprite_ "chevron-down" "regular" "h-3 w-3 text-iconNeutral"
      popoverPanel
  where
    transportDivider = span_ [class_ "h-4 w-px shrink-0 bg-strokeWeak opacity-60", Aria.hidden_ "true"] ""
    transportBtn label extraClass attrs =
      button_
        $ [ type_ "button"
          , class_ $ "inline-flex h-7 min-w-7 items-center justify-center gap-1.5 rounded-md bg-transparent px-1.5 leading-none text-textWeak hover:bg-fillWeak focus-visible:outline-2 focus-visible:outline-offset-1 disabled:cursor-not-allowed disabled:opacity-50" <> extraClass
          , Aria.label_ label
          , data_ "tippy-content" label
          ]
        <> attrs
