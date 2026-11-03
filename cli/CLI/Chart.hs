-- | Terminal chart rendering: braille line plots (via granite), block bar
-- charts, sparklines and stat tiles.
--
-- Everything here is pure @[Text]@ in, @[Text]@ out (one entry per terminal
-- row) so charts compose — 'CLI.Dashboard' lays several of them side by side
-- into a grid — and so they can be asserted on in tests without a TTY. Colour
-- is opt-in via 'colorful' so a piped/redirected stdout stays clean.
module CLI.Chart (
  -- * Series
  Series (..),
  seriesFrom,
  seriesFromMetrics,
  isTimeColumn,

  -- * Options
  ChartOpts (..),
  defaultChartOpts,

  -- * Renderers
  renderTimeseries,
  renderBars,
  renderStat,
  sparkline,

  -- * Formatting helpers (shared with the widget renderers)
  formatValue,
  formatTimeLabel,
  seriesColor,
  colorize,
  dim,
  bold,
  visibleWidth,
  stripAnsi,
  padTo,
  ellipsize,
) where

import Relude

import Data.Ord (clamp)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import Granite qualified as G
import Pages.Charts.Types (MetricsData (..))
import System.Console.ANSI qualified as ANSI


-- | One named line/bar in a chart. @points@ are @(x, y)@ with @x@ in the
-- caller's units (epoch milliseconds for timeseries, the category index for
-- bars) and @Nothing@ for a gap the renderer must not bridge.
data Series = Series
  { name :: Text
  , points :: [(Double, Maybe Double)]
  }
  deriving stock (Show)


data ChartOpts = ChartOpts
  { width :: Int
  -- ^ Total columns the chart may occupy, axis labels included.
  , height :: Int
  -- ^ Plot rows, excluding the x-axis line, x labels and legend.
  , colorful :: Bool
  , unit :: Text
  -- ^ Appended to y-axis and legend values ("ms", "req/s", "%", …).
  , timeAxis :: Bool
  -- ^ Format x labels as clock times rather than plain numbers.
  , showLegend :: Bool
  , yMax :: Maybe Double
  -- ^ Pin the top of the y-axis (used when a widget declares a threshold).
  }
  deriving stock (Show)


defaultChartOpts :: ChartOpts
defaultChartOpts =
  ChartOpts
    { width = 80
    , height = 12
    , colorful = True
    , unit = ""
    , timeAxis = True
    , showLegend = True
    , yMax = Nothing
    }


-- | Split a result grid into one 'Series' per non-timestamp column. A leading
-- timestamp column (present whenever the query binned by time) becomes the x
-- axis; without one, points fall back to their row index so a plain
-- @summarize@ still charts.
--
-- >>> map (.name) (seriesFrom ["timestamp", "hits", "errs"] [[Just 1000, Just 5, Just 1]])
-- ["hits","errs"]
-- >>> (.points) <$> viaNonEmpty head (seriesFrom ["hits"] [[Just 5], [Just 7]])
-- Just [(0.0,Just 5.0),(1.0,Just 7.0)]
seriesFrom :: [Text] -> [[Maybe Double]] -> [Series]
seriesFrom headers grid = [Series h [(x, join (row !!? i)) | (x, row) <- rows] | (i, h) <- valueCols]
  where
    hasTime = maybe False isTimeColumn (viaNonEmpty head headers)
    valueCols = [(i, h) | (i, h) <- zip [0 ..] headers, not (hasTime && i == 0)]
    rows =
      [ (if hasTime then fromMaybe (fromIntegral n) (join (row !!? 0)) else fromIntegral n, row)
      | (n :: Int, row) <- zip [0 ..] grid
      ]


seriesFromMetrics :: MetricsData -> [Series]
seriesFromMetrics md = seriesFrom (V.toList md.headers) (V.toList (V.toList <$> md.dataset))


-- | Does this column hold the x-axis timestamps? The one place that knows the
-- names the server can emit for a time bin.
--
-- >>> map isTimeColumn ["timestamp", "Created_At", "service"]
-- [True,True,False]
isTimeColumn :: Text -> Bool
isTimeColumn h = T.toLower h `elem` ["timestamp", "created_at", "time", "bucket", "_timestamp"]


-- | Full timeseries chart: y-axis ticks, braille plot area, x-axis time
-- labels and, for two or more series, a colour legend.
renderTimeseries :: ChartOpts -> [Series] -> [Text]
renderTimeseries opts allSeries
  | null ys = [dim opts.colorful $ T.center opts.width ' ' "no data in range"]
  | otherwise = map (if opts.colorful then id else stripAnsi) $ filter (not . T.null) $ lines $ G.lineGraph [(s.name, [(x, y) | (x, Just y) <- s.points]) | s <- series] plot
  where
    series = take (length palette) allSeries
    (xs, ys) = unzip [(x, y) | s <- series, (x, Just y) <- s.points]
    (xLo, xHi) = rangeOf xs
    fmtX = if opts.timeAxis then formatTimeLabel (xHi - xLo) else formatValue ""
    -- Anchoring at zero is what makes two stacked widgets comparable at a
    -- glance; a floating baseline exaggerates noise into a cliff.
    yLo = min 0 (foldl' min 0 ys)
    yHi = max (yLo + 1e-9) (fromMaybe (foldl' max yLo ys) opts.yMax)
    -- Round the axis top up to a 1/2/2.5/5 step so evenly spaced ticks land on round values.
    raw = (yHi - yLo) / fromIntegral (max 2 (opts.height `div` 3))
    mag = 10 ** fromIntegral (floor (logBase 10 raw) :: Int)
    step = mag * fromMaybe 10 (find (>= raw / mag) [1, 2, 2.5, 5])
    ticks = ceiling ((yHi - yLo) / step - 1e-9) :: Int
    yTop = yLo + step * fromIntegral ticks
    gutter = 1 + foldl' max 1 [T.length (formatValue opts.unit (yLo + step * fromIntegral i)) | i <- [0 .. ticks]]
    plot =
      G.defPlot
        { -- The last x label starts one column before the final tick, overhanging the plot.
          G.widthChars = max 8 (opts.width - gutter - T.length (fmtX xHi))
        , G.heightChars = opts.height
        , G.leftMargin = gutter
        , G.titleMargin = 0
        , G.xBounds = if xHi > xLo then (Just xLo, Just xHi) else (Nothing, Nothing)
        , G.yBounds = (Just yLo, Just yTop)
        , G.yNumTicks = ticks + 1
        , G.legendPos = if opts.showLegend && length series > 1 then G.LegendBottom else G.LegendNone
        , G.colorPalette = [G.Cyan, G.Yellow, G.Magenta, G.Green, G.Blue, G.White, G.Red]
        , G.xFormatter = \_ _ -> fmtX
        , G.yFormatter = \_ _ -> formatValue opts.unit
        }


-- | Horizontal bars for categorical results (@… | summarize count() by service@).
-- Labels are left-aligned in their own gutter, values right-aligned after the
-- bar, so the eye can scan any of the three columns independently.
renderBars :: ChartOpts -> [(Text, Double)] -> [Text]
renderBars opts rows
  | null rows = [dim opts.colorful "no data in range"]
  | otherwise = [line i l v | (i, (l, v)) <- zip [0 ..] rows]
  where
    hi = foldl' max 1e-9 (map snd rows)
    labelW = min 28 (foldl' max 1 (map (T.length . fst) rows))
    valTexts = [formatValue opts.unit v | (_, v) <- rows]
    valW = foldl' max 1 (map T.length valTexts)
    barW = max 4 (opts.width - labelW - valW - 3)
    line i l v =
      T.justifyLeft labelW ' ' (ellipsize labelW l)
        <> " "
        <> colorize opts.colorful (seriesColor i) (bar barW (v / hi))
        <> " "
        <> T.justifyRight valW ' ' (formatValue opts.unit v)
    -- Eighth-block characters let a bar end mid-cell instead of snapping to whole columns.
    bar w f =
      let (full, rest) = (round (clamp (0, 1) f * fromIntegral (w * 8)) :: Int) `divMod` 8
       in T.replicate full "█" <> (if rest == 0 then "" else one (chr (0x2590 - rest)))


-- | Single-value tile: the number, its unit, and an inline sparkline of how it
-- got there. This is the terminal analogue of the dashboard's stat widget.
renderStat :: ChartOpts -> Text -> Maybe Double -> [Series] -> [Text]
renderStat opts label valM series =
  [ bold opts.colorful (maybe "—" (formatValue opts.unit) valM)
  , dim opts.colorful (ellipsize opts.width label)
  ]
    <> [colorize opts.colorful (seriesColor 0) spark | not (T.null spark)]
  where
    spark = sparkline opts.width [y | s <- take 1 series, (_, Just y) <- s.points]


-- | Compact single-row plot. Downsamples to at most @w@ buckets by averaging,
-- so a 2000-point series still reads correctly at 40 columns. A flat series
-- draws a centred line.
sparkline :: Int -> [Double] -> Text
sparkline w ys
  | null ys || w < 1 = ""
  | otherwise = toText [T.index " ▁▂▃▄▅▆▇█" (level y) | y <- buckets]
  where
    n = length ys
    buckets
      | n <= w = ys
      | otherwise = [let c = take (max 1 (n `div` w)) (drop (i * n `div` w) ys) in sum c / fromIntegral (length c) | i <- [0 .. w - 1]]
    (lo, hi) = rangeOf buckets
    level y = if hi - lo < 1e-12 then 4 else clamp (0, 8) (round ((y - lo) / (hi - lo) * 8))


-- Formatting -----------------------------------------------------------------

-- | Terminal-friendly value formatting: SI suffixes above 1000, duration units
-- when the widget declares one, at most one decimal so columns stay aligned.
--
-- >>> map (formatValue "") [0, 12, 1234, 2500000]
-- ["0","12","1.2k","2.5M"]
-- >>> formatValue "ms" 1500
-- "1.5s"
-- >>> formatValue "%" 99.42
-- "99.4%"
formatValue :: Text -> Double -> Text
formatValue u v = case u of
  "ms" | abs v >= 1000 -> num (v / 1000) <> "s"
  "ns" | abs v >= 1e6 -> num (v / 1e6) <> "ms"
  "%" -> num v <> "%"
  _ -> siNum v <> u
  where
    num x = let r = fromIntegral (round (x * 10) :: Integer) / 10 in if r == fromIntegral (round r :: Integer) then show (round r :: Integer) else show r
    siNum x
      | abs x >= 1e9 = num (x / 1e9) <> "G"
      | abs x >= 1e6 = num (x / 1e6) <> "M"
      | abs x >= 1000 = num (x / 1000) <> "k"
      | otherwise = num x


-- | Clock label for an epoch-millisecond x value, at a precision that suits the
-- visible window: seconds under 10 minutes, date once the window passes 2 days.
formatTimeLabel :: Double -> Double -> Text
formatTimeLabel spanMs ms = toText (formatTime defaultTimeLocale fmt (posixSecondsToUTCTime (realToFrac (ms / 1000))))
  where
    fmt
      | spanMs <= 600_000 = "%H:%M:%S"
      | spanMs <= 172_800_000 = "%H:%M"
      | otherwise = "%m-%d %H:%M"


seriesColor :: Int -> ANSI.Color
seriesColor i = fromMaybe ANSI.Cyan (palette !!? (i `mod` length palette))


-- | Categorical palette. Ordered so the first few series stay distinguishable
-- for the most common forms of colour-blindness, and so none of them collide
-- with the red we reserve for errors elsewhere in the CLI.
palette :: [ANSI.Color]
palette = [ANSI.Cyan, ANSI.Yellow, ANSI.Magenta, ANSI.Green, ANSI.Blue, ANSI.White, ANSI.Red]


colorize :: Bool -> ANSI.Color -> Text -> Text
colorize False _ t = t
colorize True c t = toText (ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]) <> t <> toText (ANSI.setSGRCode [ANSI.Reset])


dim :: Bool -> Text -> Text
dim False t = t
dim True t = toText (ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.FaintIntensity]) <> t <> toText (ANSI.setSGRCode [ANSI.Reset])


bold :: Bool -> Text -> Text
bold False t = t
bold True t = toText (ANSI.setSGRCode [ANSI.SetConsoleIntensity ANSI.BoldIntensity]) <> t <> toText (ANSI.setSGRCode [ANSI.Reset])


-- Small numeric helpers ------------------------------------------------------

rangeOf :: [Double] -> (Double, Double)
rangeOf [] = (0, 1)
rangeOf (x : xs) = (foldl' min x xs, foldl' max x xs)


ellipsize :: Int -> Text -> Text
ellipsize n t = if T.length t <= n then t else T.take (max 0 (n - 1)) t <> "…"


-- | Width the terminal will actually give a string: ANSI escapes are zero-width
-- but 'T.length' counts them, so every box-drawing calculation must go through
-- this instead.
--
-- >>> visibleWidth (colorize True ANSI.Red "abc")
-- 3
visibleWidth :: Text -> Int
visibleWidth = T.length . stripAnsi


-- | Drop every ANSI escape sequence. Needed wherever styled text has to become
-- data again — logfmt output, width arithmetic.
stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC[" t of
  (before, "") -> before
  (before, rest) -> before <> stripAnsi (T.drop 1 (T.dropWhile (/= 'm') rest))


-- | Pad (or truncate) to an exact visible width, ANSI-safe.
padTo :: Int -> Text -> Text
padTo n t
  | w <= n = t <> T.replicate (n - w) " "
  | T.length t == w = ellipsize n t
  -- Coloured and too wide: drop the styling rather than slice mid-escape.
  | otherwise = ellipsize n (stripAnsi t)
  where
    w = visibleWidth t
