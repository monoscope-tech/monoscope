{-# LANGUAGE ViewPatterns #-}

module Pkg.Components.TimePicker (parseTimeRange, timepicker_, TimePickerP (..)) where

import Data.List qualified as L
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, formatTime, secondsToNominalDiffTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
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


data TimePickerP = TimePickerP
  { sinceM :: Maybe Text
  , fromM :: Maybe Text
  , toM :: Maybe Text
  }
  deriving (Generic)


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
parseSince :: UTCTime -> Text -> (Maybe UTCTime, Maybe UTCTime, Maybe Text)
parseSince now since =
  either (const (Nothing, Nothing, Nothing)) buildResult (parse timeParser "" since)
  where
    buildResult (num, unit) = (Just start, Just now, Just $ "Last " <> show num <> " " <> label)
      where
        (secs, label) = fromMaybe (0, "") (unitToSeconds unit)
        start = addUTCTime (negate . secondsToNominalDiffTime . fromIntegral $ num * secs) now

    timeParser :: Parser (Int, String)
    timeParser = (,) <$> decimal <*> (space *> some letterChar)


parseTimeRange :: UTCTime -> TimePickerP -> (Maybe UTCTime, Maybe UTCTime, Maybe Text)
parseTimeRange now (TimePickerP Nothing fromM toM) = parseFromAndTo now fromM toM
parseTimeRange now (TimePickerP (Just "") fromM toM) = parseFromAndTo now fromM toM
parseTimeRange now (TimePickerP sinceM _ _) = parseSince now (maybeToMonoid sinceM)


parseFromAndTo :: UTCTime -> Maybe Text -> Maybe Text -> (Maybe UTCTime, Maybe UTCTime, Maybe Text)
parseFromAndTo now fromM toM =
  (parseUTCTime fromM, parseUTCTime toM, formatRange fromM toM)
  where
    parseUTCTime = iso8601ParseM . toString . fromMaybe ""
    formatRange f t = liftA2 (\start end -> start <> "-" <> end) (formatTime' f) (formatTime' t)

    formatTime' :: Maybe Text -> Maybe Text
    formatTime' = fmap (toText . formatTime defaultTimeLocale "%F %T") . parseUTCTime


-----------------------------------------------------------------------------------------------------
-- Timepicker component. To be used at call site
-----------------------------------------------------------------------------------------------------
timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour")
  , ("24H", "Last 24 Hours")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]


timepicker_ :: Maybe Text -> Maybe Text -> Html ()
timepicker_ submitForm currentRange = div_ [class_ "relative"] do
  input_ [type_ "hidden", id_ "since_input"]
  input_ [type_ "hidden", id_ "custom_range_input"]
  a_
    [ class_ "relative p-2.5 bg-slate-100 border border-slate-300 text-base flex items-center gap-1.5 rounded-xl cursor-pointer"
    , [__| on click toggle .hidden on #timepickerBox |]
    ]
    do
      faSprite_ "calendar" "regular" "h-5 w-5"
      span_ [class_ "inline-block leading-none", id_ "currentRange"] $ toHtml (fromMaybe "Last 24 Hours" currentRange)
      faSprite_ "chevron-down" "regular" "h-3 w-3"
  div_ [id_ "timepickerBox", class_ "hidden absolute right-0 z-10 mt-1 rounded-md flex"] do
    div_ [class_ "relative hidden", id_ "timepickerSidebar"] $ div_ [id_ "startTime", class_ "hidden"] ""
    div_
      [class_ "inline-block w-84 overflow-auto bg-base-100 py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:"]
      do
        let linkClassBase = "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200"
            action =
              maybe
                "window.setQueryParamAndReload('since', my @data-value)"
                (\fm -> [fmt|htmx.trigger("#{fm}", "submit")|])
                submitForm
            onClickHandler =
              [text|on click set #custom_range_input's value to my @data-value
                       then toggle .hidden on #timepickerBox
                       then set #currentRange's innerText to my @data-title
                       then call window.setParams({since:@data-value, from:'', to:''}) 
                       then ${action} |]
            timePickerLink val title =
              a_
                [ class_ (linkClassBase <> " text-nowrap")
                , term "data-value" val
                , term "data-title" title
                , termRaw "_" onClickHandler
                ]
                $ toHtml title
        mapM_ (uncurry timePickerLink) timePickerItems
        a_
          [ class_ linkClassBase
          , [__| on click toggle .hidden on #timepickerSidebar |]
          ]
          "Custom date range"
    let submitAction =
          maybe
            "window.setParams({from: formatDate(start), to: formatDate(end), since: ''}, true);"
            (\fm -> [text|window.setParams({from: formatDate(start), to: formatDate(end), since: ''}); htmx.trigger("#${fm}", "submit");|])
            submitForm
    script_
      [text|
      window.picker = new easepick.create({
        element: '#startTime',
        css: ['https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.css'],
        inline: true,
        plugins: ['RangePlugin', 'TimePlugin'],
        autoApply: false,
        setup(picker) {
          picker.on('select', ({ detail: { start, end } }) => {
            const formatDate = (date) => date.toISOString();
            document.getElementById('custom_range_input').value = `$${start}/$${end}`;
            document.getElementById('timepickerBox')?.classList.toggle('hidden');
            document.getElementById('currentRange').innerText = `$${formatDate(start)} - $${formatDate(end)}`;
            ${submitAction}
          });
        },
      });
    |]
