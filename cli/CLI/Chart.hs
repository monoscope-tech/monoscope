-- | Terminal chart rendering: braille line/area plots, block bar charts,
-- sparklines and stat tiles.
--
-- Everything here is pure @[Text]@ in, @[Text]@ out (one entry per terminal
-- row) so charts compose — 'CLI.Dashboard' lays several of them side by side
-- into a grid — and so they can be asserted on in tests without a TTY.
--
-- Resolution comes from Unicode braille: one character cell carries a 2×4
-- pixel grid, so a 60×12 chart is really a 120×48 plot. Colour is opt-in via
-- 'colorful' so a piped/redirected stdout stays clean.
module CLI.Chart (
  -- * Series
  Series (..),
  seriesFromMetrics,

  -- * Options
  ChartOpts (..),
  defaultChartOpts,
  ChartStyle (..),

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
  padTo,
  ellipsize,
) where

import Relude

import Data.Bits (bit, (.|.))
import Data.List (zipWith3)
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
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


data ChartStyle = Line | Area | Bars
  deriving stock (Eq, Show)


data ChartOpts = ChartOpts
  { width :: Int
  -- ^ Total columns the chart may occupy, axis labels included.
  , height :: Int
  -- ^ Plot rows, excluding the x-axis line, x labels and legend.
  , colorful :: Bool
  , style :: ChartStyle
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
    , style = Line
    , unit = ""
    , timeAxis = True
    , showLegend = True
    , yMax = Nothing
    }


-- | Split the server's @MetricsData@ into one 'Series' per non-timestamp
-- column. The first header is the bin timestamp (epoch milliseconds) whenever
-- the query used @by bin(...)@; without it points fall back to their row index
-- so a plain @summarize@ still charts.
seriesFromMetrics :: MetricsData -> [Series]
seriesFromMetrics md
  | null valueCols = []
  | otherwise = [Series h [(x, row V.!? i >>= id) | (x, row) <- rows] | (i, h) <- valueCols]
  where
    headers = V.toList md.headers
    hasTime = maybe False isTimeHeader (viaNonEmpty head headers)
    valueCols = [(i, h) | (i, h) <- zip [0 ..] headers, not (hasTime && i == 0)]
    rows =
      [ (if hasTime then fromMaybe (fromIntegral n) (join (row V.!? 0)) else fromIntegral n, row)
      | (n :: Int, row) <- zip [0 ..] (V.toList md.dataset)
      ]


isTimeHeader :: Text -> Bool
isTimeHeader h = T.toLower h `elem` ["timestamp", "created_at", "time", "bucket", "_timestamp"]


-- | Full timeseries chart: y-axis with nice ticks, braille plot area, x-axis
-- time labels and a colour legend.
renderTimeseries :: ChartOpts -> [Series] -> [Text]
renderTimeseries opts allSeries
  | null pts = [dim opts.colorful $ T.center opts.width ' ' "no data in range"]
  | otherwise = body <> [axisLine, xLabels] <> legend
  where
    series = take (length palette) allSeries
    pts = [(x, y) | s <- series, (x, Just y) <- s.points]
    (xLo, xHi) = rangeOf (map fst pts)
    dataHi = fromMaybe (snd (rangeOf (map snd pts))) opts.yMax
    -- Anchoring at zero is what makes two stacked widgets comparable at a
    -- glance; a floating baseline exaggerates noise into a cliff.
    yLo = min 0 (fst (rangeOf (map snd pts)))
    ticks = niceTicks yLo dataHi (max 2 (opts.height `div` 3))
    -- Stretch the axis to the topmost tick so the labelled rows land on evenly
    -- spaced lines instead of drifting with the data's exact maximum.
    yHi = foldl' max dataHi ticks
    labels = [formatValue opts.unit t | t <- ticks]
    gutter = foldl' max 1 (map T.length labels)
    plotW = max 8 (opts.width - gutter - 2)
    canvas = foldl' (drawSeries opts.style plotW opts.height (xLo, xHi) (yLo, yHi)) mempty (zip [0 ..] series)
    body = zipWith3 row [0 ..] (rowLabels ticks) (canvasRows opts.colorful plotW opts.height canvas)
    row _ lbl cells = T.justifyRight gutter ' ' lbl <> dim opts.colorful " │" <> cells
    -- One label per tick, placed on the row the tick's value lands on.
    rowLabels ts =
      [ maybe "" (formatValue opts.unit) (find (\t -> tickRow t == r) ts)
      | r <- [0 .. opts.height - 1]
      ]
      where
        tickRow t = clamp 0 (opts.height - 1) $ opts.height - 1 - round (frac yLo yHi t * fromIntegral (opts.height - 1))
    axisLine = T.replicate gutter " " <> dim opts.colorful (" └" <> T.replicate plotW "─")
    xLabels = T.replicate (gutter + 2) " " <> dim opts.colorful (spread plotW (map fmtX [xLo, (xLo + xHi) / 2, xHi]))
    fmtX = if opts.timeAxis then formatTimeLabel (xHi - xLo) else formatValue ""
    legend
      | not opts.showLegend || length series < 2 = []
      | otherwise =
          [ T.replicate (gutter + 2) " "
              <> T.intercalate "  " [colorize opts.colorful (seriesColor i) "●" <> " " <> s.name | (i, s) <- zip [0 ..] series]
          ]


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


-- | Fractional-width bar built from the eighth-block characters, so a bar can
-- end mid-cell instead of snapping to whole columns.
bar :: Int -> Double -> Text
bar w f = T.replicate full "█" <> partial
  where
    eighths = round (clampD 0 1 f * fromIntegral (w * 8)) :: Int
    (full, rest) = eighths `divMod` 8
    partial = if rest == 0 then "" else one (chr (0x2590 - rest))


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
-- so a 2000-point series still reads correctly at 40 columns.
sparkline :: Int -> [Double] -> Text
sparkline w ys
  | null ys || w < 1 = ""
  | otherwise = T.pack [T.index blocks (level y) | y <- buckets]
  where
    blocks = " ▁▂▃▄▅▆▇█"
    buckets = map avg (chunkInto w ys)
    (lo, hi) = rangeOf buckets
    level y = clamp 0 8 $ round (frac lo hi y * 8)


-- Canvas ---------------------------------------------------------------------

-- | Sparse braille canvas keyed by @row * width + col@; each cell holds the
-- set dot bits plus the series that last wrote to it (which decides colour).
type Canvas = IntMap (Word8, Int)


-- | Braille dot bit for a sub-cell, following the U+28xx layout (dots 1-3 and
-- 4-6 down the two columns, dots 7-8 on the bottom row).
dotBit :: Int -> Int -> Word8
dotBit dx dy
  | dy == 3 = if dx == 0 then 0x40 else 0x80
  | otherwise = bit (dy + 3 * dx)


plot :: Int -> Int -> Int -> Int -> Int -> Canvas -> Canvas
plot w h px py sIdx
  | px < 0 || py < 0 || px >= w * 2 || py >= h * 4 = id
  | otherwise = IntMap.insertWith merge ((py `div` 4) * w + px `div` 2) (dotBit (px `mod` 2) (py `mod` 4), sIdx)
  where
    merge (newBits, s) (oldBits, _) = (newBits .|. oldBits, s)


drawSeries :: ChartStyle -> Int -> Int -> (Double, Double) -> (Double, Double) -> Canvas -> (Int, Series) -> Canvas
drawSeries style w h (xLo, xHi) (yLo, yHi) c0 (sIdx, s) = case style of
  Bars -> foldl' column c0 pixels
  Area -> foldl' column c0 pixels
  Line -> foldl' segment c0 (zip pixels (drop 1 pixels))
  where
    pixels = [(toPx xLo xHi (w * 2) x, (h * 4 - 1) - toPx yLo yHi (h * 4) y) | (x, Just y) <- s.points]
    column c (px, py) = foldl' (\acc y -> plot w h px y sIdx acc) c [py .. h * 4 - 1]
    -- Gaps are already absent from `pixels`, so a segment can span a hole. That
    -- is deliberate for Line: a dropped bin shouldn't split the line visually
    -- when the series is otherwise continuous.
    segment c ((x0, y0), (x1, y1)) = foldl' (\acc (x, y) -> plot w h x y sIdx acc) c (bresenham x0 y0 x1 y1)


toPx :: Double -> Double -> Int -> Double -> Int
toPx lo hi n v = clamp 0 (n - 1) $ round (frac lo hi v * fromIntegral (n - 1))


bresenham :: Int -> Int -> Int -> Int -> [(Int, Int)]
bresenham x0 y0 x1 y1 = go x0 y0 (dx + dy)
  where
    dx = abs (x1 - x0)
    dy = negate (abs (y1 - y0))
    sx = if x0 < x1 then 1 else -1
    sy = if y0 < y1 then 1 else -1
    go x y err
      | x == x1 && y == y1 = [(x, y)]
      | otherwise =
          (x, y)
            : let (x', err') = if 2 * err >= dy && x /= x1 then (x + sx, err + dy) else (x, err)
                  (y', err'') = if 2 * err <= dx && y /= y1 then (y + sy, err' + dx) else (y, err')
               in go x' y' err''


canvasRows :: Bool -> Int -> Int -> Canvas -> [Text]
canvasRows color w h canvas = [T.concat [cell (r * w + c) | c <- [0 .. w - 1]] | r <- [0 .. h - 1]]
  where
    cell k = case IntMap.lookup k canvas of
      Nothing -> " "
      Just (bits, s) -> colorize color (seriesColor s) (one (chr (0x2800 + fromIntegral bits)))


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
formatTimeLabel spanMs ms = toText (formatTime defaultTimeLocale fmt (msToUTC ms))
  where
    fmt
      | spanMs <= 600_000 = "%H:%M:%S"
      | spanMs <= 172_800_000 = "%H:%M"
      | otherwise = "%m-%d %H:%M"


msToUTC :: Double -> UTCTime
msToUTC ms = posixSecondsToUTCTime (realToFrac (ms / 1000))


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

-- | Where @v@ sits in @[lo, hi]@, as 0..1. A degenerate range maps to the
-- middle rather than blowing up, so a flat series draws a centred line.
frac :: Double -> Double -> Double -> Double
frac lo hi v
  | hi - lo < 1e-12 = 0.5
  | otherwise = clampD 0 1 ((v - lo) / (hi - lo))


rangeOf :: [Double] -> (Double, Double)
rangeOf [] = (0, 1)
rangeOf (x : xs) = (foldl' min x xs, foldl' max x xs)


-- | Human-friendly axis ticks: round the step to 1/2/5 × a power of ten and
-- walk from the first multiple at or above @lo@.
--
-- >>> niceTicks 0 100 4
-- [0.0,25.0,50.0,75.0,100.0]
-- >>> niceTicks 0 7 3
-- [0.0,2.0,4.0,6.0]
niceTicks :: Double -> Double -> Int -> [Double]
niceTicks lo hi n
  | hi - lo < 1e-12 = [lo]
  | otherwise = takeWhile (<= hi + step / 2) [start + fromIntegral i * step | i <- [0 :: Int ..]]
  where
    raw = (hi - lo) / fromIntegral (max 1 n)
    mag = 10 ** fromIntegral (floor (logBase 10 raw) :: Int)
    step = mag * fromMaybe 10 (find (>= raw / mag) [1, 2, 2.5, 5])
    start = fromIntegral (ceiling (lo / step) :: Integer) * step


-- | Place labels across @w@ columns: first flush left, last flush right, the
-- rest evenly spaced. Overlapping labels are dropped rather than truncated.
spread :: Int -> [Text] -> Text
spread w = foldl' place (T.replicate w " ") . zip [0 :: Int ..] . \ls -> zip (positions (length ls)) ls
  where
    positions k = [round (fromIntegral i / fromIntegral (max 1 (k - 1)) * fromIntegral (w - 1) :: Double) | i <- [0 .. k - 1]]
    place acc (_, (p, l)) =
      let at = clamp 0 (max 0 (w - T.length l)) (p - if p >= w - 1 then T.length l - 1 else 0)
       in if T.any (/= ' ') (T.take (T.length l + 1) (T.drop at acc))
            then acc
            else T.take at acc <> l <> T.drop (at + T.length l) acc


chunkInto :: Int -> [a] -> [[a]]
chunkInto n xs
  | len <= n = map one xs
  | otherwise = [slice i | i <- [0 .. n - 1]]
  where
    len = length xs
    slice i = take (max 1 (len `div` n)) (drop (i * len `div` n) xs)


avg :: [Double] -> Double
avg [] = 0
avg xs = sum xs / fromIntegral (length xs)


ellipsize :: Int -> Text -> Text
ellipsize n t = if T.length t <= n then t else T.take (max 0 (n - 1)) t <> "…"


clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi


clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi


-- | Width the terminal will actually give a string: ANSI escapes are zero-width
-- but 'T.length' counts them, so every box-drawing calculation must go through
-- this instead.
--
-- >>> visibleWidth (colorize True ANSI.Red "abc")
-- 3
visibleWidth :: Text -> Int
visibleWidth = T.length . stripAnsi


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
