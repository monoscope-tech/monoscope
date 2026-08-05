-- | Renders a Monoscope dashboard into the terminal.
--
-- The server resolves every widget's query up front (@GET
-- \/api\/v1\/dashboards\/{id}\/data@) and hands back the same 12-column grid the
-- browser lays out. We map that grid onto the terminal's width so the terminal
-- dashboard has the same shape as the web one — widgets that sit side by side
-- in the browser sit side by side here.
module CLI.Dashboard (
  DashboardData (..),
  RenderedWidget (..),
  renderDashboard,
  renderWidget,
) where

import Relude

import CLI.Chart
import Data.Aeson qualified as AE
import Data.Text qualified as T
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAES


-- | Wire shape of @GET /api/v1/dashboards/{id}/data@ (@Web.ApiTypes.DashboardData@).
data DashboardData = DashboardData
  { title :: Text
  , tab :: Maybe Text
  , tabs :: [Text]
  , since :: Maybe Text
  , widgets :: [RenderedWidget]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAES.Snake DashboardData


data RenderedWidget = RenderedWidget
  { id :: Maybe Text
  , title :: Maybe Text
  , subtitle :: Maybe Text
  , wType :: Text
  , unit :: Maybe Text
  , layout :: Maybe Layout
  , value :: Maybe Double
  , headers :: [Text]
  , rows :: [[Maybe Double]]
  , textRows :: [[Text]]
  , error :: Maybe Text
  , columns :: [Text]
  -- ^ Column titles for table widgets, which name their columns in the schema
  -- rather than taking them from the query's result headers.
  , children :: [RenderedWidget]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake], DAE.OmitNothingFields] RenderedWidget


data Layout = Layout {x, y, w, h :: Maybe Int}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAES.Snake Layout


-- | The browser grid is 12 columns wide; keeping the same divisor is what makes
-- a half-width widget stay half-width in the terminal.
gridColumns :: Int
gridColumns = 12


-- | Terminal rows per grid unit. Gridstack's cell height is ~60px against a
-- ~20px line box, so three rows per unit keeps a 4-unit chart roughly as tall,
-- relative to its width, as it looks on screen.
rowsPerUnit :: Int
rowsPerUnit = 3


renderDashboard :: Bool -> Int -> DashboardData -> [Text]
renderDashboard color termW d = header <> renderGrid color termW d.widgets
  where
    header =
      [ bold color d.title <> maybe "" (\t -> dim color "  ▸ " <> t) d.tab
      , dim color $ T.intercalate "   " $ catMaybes [("range: " <>) <$> d.since, tabHint]
      , ""
      ]
    tabHint = case d.tabs of
      (_ : _ : _) -> Just $ "tabs: " <> T.intercalate ", " d.tabs
      _ -> Nothing


-- | Lay a set of widgets out as bands of the 12-column grid. Called
-- recursively for group widgets, which nest their own sub-grid.
renderGrid :: Bool -> Int -> [RenderedWidget] -> [Text]
renderGrid color w = concatMap (renderBand color w) . bands


-- | Group widgets into rows of the grid. Widgets are laid out in @y@ order and
-- a new band starts whenever adding the next widget would overflow 12 columns,
-- which reproduces gridstack's wrapping without needing its full solver.
bands :: [RenderedWidget] -> [[RenderedWidget]]
bands = go 0 [] . sortOn (\w -> (gridY w, gridX w))
  where
    go _ acc [] = [reverse acc | not (null acc)]
    go used acc (w : ws)
      | not (null acc) && used + gridW w > gridColumns = reverse acc : go (gridW w) [w] ws
      | otherwise = go (used + gridW w) (w : acc) ws


gridX, gridY, gridW, gridH :: RenderedWidget -> Int
gridX w = fromMaybe 0 (w.layout >>= (.x))
gridY w = fromMaybe 0 (w.layout >>= (.y))
gridW w = max 1 (min gridColumns (fromMaybe gridColumns (w.layout >>= (.w))))
gridH w = max 1 (fromMaybe 3 (w.layout >>= (.h)))


-- | Render one grid row: every widget gets a share of the width proportional to
-- its grid span, all boxes are padded to the band's tallest, and they're
-- stitched together column by column.
renderBand :: Bool -> Int -> [RenderedWidget] -> [Text]
renderBand color termW ws = stitch boxes <> [""]
  where
    widths = shareWidth (termW - (length ws - 1)) (map gridW ws)
    -- Ask each widget for its content at the height its grid span implies, then
    -- size the band to whatever the tallest actually produced: a chart fills its
    -- span, but a group shrinks to the sub-grid it contains.
    contents = [(bw, w, renderWidget color (max 1 (bw - 4)) (gridH w * rowsPerUnit) w) | (bw, w) <- zip widths ws]
    boxH = foldl' max 3 [length c | (_, _, c) <- contents]
    boxes = [box color bw boxH w c | (bw, w, c) <- contents]
    stitch [] = []
    stitch bs = [T.intercalate " " (map (fromMaybe "" . (!!? r)) bs) | r <- [0 .. foldl' max 0 (map length bs) - 1]]


-- | Split @total@ columns across widgets in proportion to their grid spans,
-- giving the remainder to the last one so a band always ends flush right.
--
-- >>> shareWidth 100 [6, 6]
-- [50,50]
-- >>> shareWidth 94 [4, 4, 4]
-- [31,31,32]
-- >>> sum (shareWidth 77 [3, 9]) == 77
-- True
shareWidth :: Int -> [Int] -> [Int]
shareWidth total spans = case reverse [max 8 (total * sp `div` max 1 (sum spans)) | sp <- spans] of
  [] -> []
  (_ : rest) -> reverse (max 8 (total - sum rest) : rest)


-- | Frame pre-rendered content in a fixed-size box with a title bar.
box :: Bool -> Int -> Int -> RenderedWidget -> [Text] -> [Text]
box color w h widget content = [top] <> map side body <> [bottom]
  where
    inner = max 1 (w - 4)
    title = padTo inner $ bold color (ellipsize inner (fromMaybe widget.wType widget.title))
    top = dim color "╭ " <> title <> dim color " ╮"
    bottom = dim color ("╰" <> T.replicate (max 2 (w - 2)) "─" <> "╯")
    side t = dim color "│ " <> padTo inner t <> dim color " │"
    body = take h (content <> repeat "")


-- | Draw a widget's body at a given size. Exposed on its own so
-- @monoscope dashboards render --widget ID@ can print one widget full-width.
renderWidget :: Bool -> Int -> Int -> RenderedWidget -> [Text]
renderWidget color w h widget = case widget.error of
  Just e -> [colorize color (seriesColor 6) ("query failed: " <> ellipsize (w - 14) e)]
  Nothing -> case widget.wType of
    "stat" -> renderStat opts (fromMaybe "" widget.subtitle) widget.value series
    "timeseries_stat" -> renderStat opts (fromMaybe "" widget.subtitle) (statValue widget) series
    "table" -> textTable color w h widget
    "logs" -> textTable color w h widget
    "traces" -> textTable color w h widget
    "top_list" -> renderBars opts (categorical widget)
    "pie_chart" -> renderBars opts (categorical widget)
    "distribution" -> renderBars opts (categorical widget)
    "group" -> renderGrid color w widget.children
    -- Everything with no terminal-native form (geomaps, flamegraphs, service
    -- maps, …) still shows its numbers rather than an empty box.
    _ -> if null series then textTable color w h widget else renderTimeseries opts series
  where
    opts =
      defaultChartOpts
        { width = w
        , height = max 3 (h - 2)
        , colorful = color
        , unit = fromMaybe "" widget.unit
        , showLegend = h > 6
        }
    series = widgetSeries widget


-- | Series for a widget, from the numeric grid the server returned. Mirrors
-- 'CLI.Chart.seriesFromMetrics' but reads the flattened wire rows.
widgetSeries :: RenderedWidget -> [Series]
widgetSeries widget
  | null valueCols = []
  | otherwise = [Series hdr [(x, join (row !!? i)) | (x, row) <- xs] | (i, hdr) <- valueCols]
  where
    hasTime = maybe False isTimeCol (viaNonEmpty head widget.headers)
    valueCols = [(i, hdr) | (i, hdr) <- zip [0 ..] widget.headers, not (hasTime && i == 0)]
    xs = [(if hasTime then fromMaybe (fromIntegral n) (join (row !!? 0)) else fromIntegral n, row) | (n :: Int, row) <- zip [0 ..] widget.rows]


isTimeCol :: Text -> Bool
isTimeCol h = T.toLower h `elem` ["timestamp", "created_at", "time", "bucket"]


-- | Latest non-null point of the first series — what a timeseries-backed stat
-- widget shows as its headline number.
statValue :: RenderedWidget -> Maybe Double
statValue widget = widget.value <|> (viaNonEmpty last . mapMaybe snd . (.points) =<< viaNonEmpty head (widgetSeries widget))


-- | @(label, value)@ pairs for the bar-style widgets: the first text column
-- labels the bar, the first numeric column sizes it.
categorical :: RenderedWidget -> [(Text, Double)]
categorical widget = case widget.textRows of
  [] -> [(maybe "" show (join (row !!? 0)), fromMaybe 0 (join (row !!? 1))) | row <- widget.rows]
  rows -> [(fromMaybe "" (r !!? 0), fromMaybe 0 (readMaybe . toString =<< r !!? 1)) | r <- rows]


-- | Fallback renderer for row-shaped widgets: a plain column layout sized to
-- the box, no borders (the box already has them).
textTable :: Bool -> Int -> Int -> RenderedWidget -> [Text]
textTable color w h widget
  | null rows = [dim color "no data in range"]
  | otherwise = dim color (line heads) : map line (take (h - 1) rows)
  where
    rows = if null widget.textRows then [[maybe "" (formatValue "") c | c <- r] | r <- widget.rows] else widget.textRows
    heads = if null widget.headers then widget.columns else widget.headers
    cols = max 1 (foldl' max (length heads) (map length (take 1 rows)))
    colW = max 6 ((w - (cols - 1)) `div` cols)
    line cells = T.intercalate " " [padTo colW (ellipsize colW c) | c <- take cols (cells <> repeat "")]
