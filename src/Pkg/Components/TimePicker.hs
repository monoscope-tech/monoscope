module Pkg.Components.TimePicker (
  parseTimeRange,
  timepicker_,
  refreshButton_,
  TimePicker (..),
) where

import Data.Aeson qualified as AE
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Deriving.Aeson.Stock qualified as DAE
import Language.Haskell.TH.Syntax qualified as THS
import Lucid
import Lucid.Base (termRaw)
import Lucid.Hyperscript (__)
import NeatInterpolation (text)
import PyF (fmt)
import Relude hiding (some)
import Text.Megaparsec (Parsec, parse, some)
import Text.Megaparsec.Char (letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (faSprite_)


type Parser = Parsec Void Text


data TimePicker = TimePicker
  { since :: Maybe Text
  , from :: Maybe Text
  , to :: Maybe Text
  }
  deriving (Generic, Show, THS.Lift)
  deriving anyclass (NFData)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake TimePicker


-- Mapping of time units to seconds
unitToSeconds :: String -> Maybe (Int, Text)
unitToSeconds unit =
  L.lookup
    unit
    [ ("S", (1, "Seconds"))
    , ("M", (60, "Minutes"))
    , ("H", (3600, "Hours"))
    , ("D", (86400, "Days"))
    ]


---- >>> parseTimeRange (Unsafe.read "2024-10-31 12:00:00 UTC") "2H"
-- (Just 2024-10-31 10:00:00 UTC,Just 2024-10-31 12:00:00 UTC,Just "Last 2 Hours")
--
parseSince :: UTCTime -> Text -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseSince now since =
  either (const (Nothing, Nothing, Nothing)) buildResult (parse timeParser "" since)
  where
    buildResult (num, unit) = (Just start, Just now, Just ("Last " <> show num <> " " <> label, ""))
      where
        (secs, label) = fromMaybe (0, "") (unitToSeconds unit)
        start = addUTCTime (negate . secondsToNominalDiffTime . fromIntegral $ num * secs) now

    timeParser :: Parser (Int, String)
    timeParser = (,) <$> decimal <*> (space *> some letterChar)


parseTimeRange :: UTCTime -> TimePicker -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseTimeRange now (TimePicker Nothing Nothing Nothing) = parseSince now "1H"
parseTimeRange now (TimePicker (Just "") Nothing Nothing) = parseSince now "1H"
parseTimeRange now (TimePicker Nothing fromM toM) = parseFromAndTo now fromM toM
parseTimeRange now (TimePicker (Just "") fromM toM) = parseFromAndTo now fromM toM
parseTimeRange now (TimePicker sinceM _ _) = parseSince now (maybeToMonoid sinceM)


parseFromAndTo :: UTCTime -> Maybe Text -> Maybe Text -> (Maybe UTCTime, Maybe UTCTime, Maybe (Text, Text))
parseFromAndTo now fromM toM =
  (parseUTCTime fromM, parseUTCTime toM, formatRange fromM toM)
  where
    parseUTCTime = iso8601ParseM . toString . fromMaybe ""
    formatRange f t = liftA2 (,) (formatTime' f) (formatTime' t)

    formatTime' :: Maybe Text -> Maybe Text
    formatTime' = fmap (toText . formatTime defaultTimeLocale "%F %T") . parseUTCTime


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


timepicker_ :: Maybe Text -> Maybe (Text, Text) -> Html ()
timepicker_ submitForm currentRange = do
  fieldset_ [class_ "border border-strokeWeak p-0 inline-block rounded-lg overflow-hidden dash-variable text-sm shadow-xs -mt-1.5"] do
    legend_ [id_ "offsetIndicator", class_ "px-1 ml-2 text-xs text-textWeak"] "UTC+00"
    input_ [type_ "hidden", id_ "since_input"]
    input_ [type_ "hidden", id_ "custom_range_input"]

    -- Trigger button using DaisyUI popover approach
    button_
      [ term "popovertarget" "timepicker-popover"
      , term "style" "anchor-name:--timepicker-anchor"
      , class_ "flex items-center gap-2 relative pt-0 p-2 text-textWeak cursor-pointer"
      ]
      do
        faSprite_ "calendar" "regular" "h-4 w-4 text-iconNeutral "
        let attrs = maybe [] (\(s, e) -> [term "data-start" s, term "data-end" e]) currentRange
        span_ (attrs ++ [class_ "inline-block leading-none", id_ "currentRange"]) $ toHtml (maybe "Last Hour" (\(s, e) -> s <> if T.null e then "" else "-" <> e) currentRange)
        faSprite_ "chevron-down" "regular" "h-3 w-3"

  -- DaisyUI popover contentd
  div_ [class_ "relative w-max bg-red-500"] do
    div_
      [ class_ "dropdown dropdown-end menu w-96 rounded-box bg-bgOverlay shadow-lg"
      , term "popover" ""
      , id_ "timepicker-popover"
      , term "style" "position-anchor:--timepicker-anchor"
      ]
      do
        div_ [class_ "absolute top-0 left-0 z-50 hidden", id_ "timepickerSidebar", [__| on click halt|]] $ div_ [id_ "startTime", class_ "hidden"] ""
        ul_ [] do
          li_ [class_ "menu-title"] "Select Time Range"
          let action =
                maybe
                  "window.setQueryParamAndReload('since', my @data-value)"
                  (\fm -> [fmt|htmx.trigger("#{fm}", "submit")|])
                  submitForm
              onClickHandler =
                [text|on click set #custom_range_input's value to my @data-value
                    then set #currentRange's innerText to my @data-title
                    then call window.setParams({since:@data-value, from:'', to:''})
                    then ${action} |]
              timePickerLink val title =
                li_ $ a_
                  [ class_ "flex items-center justify-between hover:bg-base-200 rounded-lg px-3 py-2"
                  , term "data-value" val
                  , term "data-title" title
                  , termRaw "_" onClickHandler
                  ]
                  do
                    span_ [class_ "text-sm"] $ toHtml title
                    span_ [class_ "text-xs text-base-content/60"] $ toHtml val
          mapM_ (uncurry timePickerLink) timePickerItems
          li_ $ a_
            [ [__| on click toggle .hidden on #timepickerSidebar |]
            ]
            do
              faSprite_ "calendar" "regular" "h-4 w-4 mr-2 text-iconNeutral"
              span_ "Custom date range"

        -- Custom date range picker (hidden by default)
        let submitAction =
              maybe
                "window.setParams({from: formatDate(start), to: formatDate(end), since: ''}, true);"
                (\fm -> [text|window.setParams({from: formatDate(start), to: formatDate(end), since: ''}); htmx.trigger("#${fm}", "submit");|])
                submitForm
        script_
          [text|
      (function() {
        var formatDateLocal = (date) => new Date(date).toLocaleString();
        document.addEventListener('DOMContentLoaded', ()=> {
          const offsetStr = getUTCOffset(); 
          document.getElementById('offsetIndicator').innerText = offsetStr;
          const range = document.getElementById("currentRange");
          const start = range.dataset.start;
          const end = range.dataset.end;
          if(start && end) {
              range.innerText = `$${formatDateLocal(start)} - $${formatDateLocal(end)}`
            }
        })
        window.picker = new easepick.create({
          element: '#startTime',
          css: ['https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.css'],
          inline: true,
          plugins: ['RangePlugin', 'TimePlugin'],
          autoApply: false,
          documentClick: (e) => {
            if(e.target.classList.contains('easepick-wrapper')) {
              return;
            }
            document.querySelector('#timepickerSidebar').classList.add('hidden');
            return true;
          },
          setup(picker) {
            picker.on("clear", () => {
              document.querySelector('#timepickerSidebar').classList.add('hidden');
            });
            
            picker.on('select', ({ detail: { start, end } }) => {
              startMs = start.getTime();
              endMs = end.getTime();
              if (startMs >= endMs) {
                end = new Date();
                }
              const formatDate = (date) => date.toISOString();
              document.getElementById('custom_range_input').value = `$${start}/$${end}`;
              document.getElementById('currentRange').innerText = `$${formatDateLocal(start)} - $${formatDateLocal(end)}`;
              ${submitAction}
            });
          },
        });
      })()
    |]


-- | Common refresh options used throughout the application
refreshOptions :: [(Text, Text, Text)]
refreshOptions =
  [ ("off", "Off", "0")
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


-- | Refresh button with auto-refresh dropdown
-- No parameters needed as it's now fully standardized to work with the global event system
refreshButton_ :: Html ()
refreshButton_ = do
  -- The join button group for refresh
  div_ [class_ "join"] do
    -- Immediate refresh button
    label_
      [ class_ "cursor-pointer px-3 flex items-center border border-strokeWeak shadow-xs leading-none join-item"
      , data_ "tippy-content" "Refresh"
      , [__| on click trigger 'update-query' on window then
          add .animate-spin to the first <svg/> in me then wait 1 seconds then
          remove .animate-spin from the first <svg/> in me |]
      ]
      $ faSprite_ "arrows-rotate" "regular" "w-3 h-3 text-iconNeutral"

    -- Auto-refresh dropdown
    div_ [class_ "dropdown dropdown-end leading-none join-item border-y border-r border-strokeWeak shadow-xs group/rf"] do
      div_ [class_ "cursor-pointer py-2 px-3 flex gap-2 items-center leading-none", tabindex_ "0", data_ "tippy-content" "Refresh frequency"] do
        span_ [class_ "auto-refresh-span"] "Off"
        faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral "

      -- Dropdown menu
      ul_ [class_ "dropdown-content menu p-2 shadow-lg bg-base-100 rounded-box z-[1] mt-2", tabindex_ "0"] do
        forM_ refreshOptions \(label, title, ms) ->
          li_
            $ a_
              [ data_ "value" ms
              , data_ "tippy-content" title
              , [__| on click 
                  set .auto-refresh-span.innerText to my.textContent then
                  send setRefreshInterval(interval: parseInt(@data-value)) to window then
                  focus(document.body)
              |]
              ]
            $ toHtml label
