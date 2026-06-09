-- | Sentry-style pretty-printed tables for the CLI.
--
-- Renders a Unicode box-drawing table with per-cell colors (severity →
-- red/yellow/blue/gray, brand identifiers in cyan, muted footers in dim).
-- ANSI is suppressed under JSON/YAML mode so a piped stdout never sees
-- escape codes.
--
-- Callers don't pick column widths — the renderer measures the data and
-- truncates the rightmost wide column with @…@ to fit the terminal. If the
-- terminal width can't be determined, we default to 120 cols (enough for
-- most laptop terminals, narrow enough to avoid run-on lines in tmux).
module CLI.Table (
  -- * Cell-level styling
  CellStyle (..),
  Severity (..),
  Align (..),
  Cell,
  plain,
  level,
  numeric,
  muted,
  brand,
  success,
  failure,

  -- * Rendering
  renderRichTable,
  renderRichTableWith,
  RichTableOpts (..),
  defaultRichTableOpts,

  -- * Pagination cue
  paginationFooter,

  -- * Severity parser
  parseSeverity,
) where

import Relude
import Relude qualified

import CLI.Core (OutputMode (..), getOutputMode, isJsonOutput)
import Data.Char (toUpper)
import Data.Text qualified as T
import Data.Text.IO qualified
import Effectful
import System.Console.ANSI qualified as ANSI


-- | What a cell means semantically. The renderer maps this to SGR codes;
-- callers don't pick colors directly so the palette stays consistent.
data CellStyle
  = Plain
  | Level Severity
  | Numeric
  | Muted
  | Brand
  | Success
  | Failure
  deriving stock (Eq, Show)


-- | Telemetry severity. Maps to the OTel canonical strings (TRACE/DEBUG/INFO/
-- WARN/ERROR/FATAL) for display and to vivid SGR colors for emphasis.
data Severity = SevTrace | SevDebug | SevInfo | SevWarn | SevError | SevFatal
  deriving stock (Eq, Show)


-- | Column alignment for the per-column header + data formatting pass.
data Align = AlignLeft | AlignRight | AlignCenter
  deriving stock (Eq, Show)


-- | One styled cell. The renderer measures via the bare text and wraps the
-- output in SGR escapes when the table mode allows it.
type Cell = (CellStyle, Text)


plain, numeric, muted, brand, success, failure :: Text -> Cell
plain t = (Plain, t)
numeric t = (Numeric, t)
muted t = (Muted, t)
brand t = (Brand, t)
success t = (Success, t)
failure t = (Failure, t)


-- | Wrap a severity-typed cell. Lower-cased input is tolerated.
level :: Severity -> Text -> Cell
level s t = (Level s, t)


-- | Renderer knobs. Most callers only override 'alignments' to right-align
-- numeric columns and 'maxWidth' on layouts that benefit from a hard cap.
data RichTableOpts = RichTableOpts
  { alignments :: [Align]
  -- ^ Per-column alignment. Shorter than the column count → remaining
  -- columns default to 'AlignLeft'.
  , maxWidth :: Maybe Int
  -- ^ Hard cap on total width. When 'Nothing' we query the terminal.
  , wrapRightmost :: Bool
  -- ^ When True, the last column absorbs the slack and overflowing rows are
  -- truncated with @…@. Default True — this is what makes long titles in
  -- @issues list@ readable instead of forcing a horizontal scroll.
  }
  deriving stock (Show)


defaultRichTableOpts :: RichTableOpts
defaultRichTableOpts = RichTableOpts {alignments = [], maxWidth = Nothing, wrapRightmost = True}


-- | The vanilla renderer: defaults for alignment + width. Use this for most
-- list commands.
renderRichTable :: IOE :> es => [Text] -> [[Cell]] -> Eff es ()
renderRichTable = renderRichTableWith defaultRichTableOpts


-- | Render with explicit knobs.
--
-- - In JSON/YAML mode this is a no-op (rendering goes through 'renderJSON'
--   on the same payload at the call site). The renderer never emits
--   anything in non-table mode so a buggy caller can't leak ANSI.
-- - In table mode but a non-TTY stdout (e.g. @--table | cat@), ANSI is
--   still emitted because the user explicitly asked for the table view.
renderRichTableWith :: IOE :> es => RichTableOpts -> [Text] -> [[Cell]] -> Eff es ()
renderRichTableWith opts headers rows = do
  mode <- getOutputMode
  case mode of
    OutputJSON -> pass
    OutputYAML -> pass
    OutputTable -> do
      ansi <- not <$> isJsonOutput
      termW <- liftIO termWidth
      let cap = fromMaybe termW opts.maxWidth
          aligns = zipWith const (opts.alignments <> repeat AlignLeft) headers
          (widths, normalized) = layout opts.wrapRightmost cap headers rows
      liftIO $ Data.Text.IO.putStrLn (topBorder widths)
      liftIO $ Data.Text.IO.putStrLn (renderHeaderRow ansi aligns widths headers)
      liftIO $ Data.Text.IO.putStrLn (midBorder widths)
      forM_ normalized $ \r ->
        liftIO $ Data.Text.IO.putStrLn (renderRow ansi aligns widths r)
      liftIO $ Data.Text.IO.putStrLn (bottomBorder widths)


-- | One line of muted text, intended for "… N more — use --limit / --cursor"
-- hints below a list. Skipped in JSON mode so a piped consumer sees only
-- payload, no chrome.
paginationFooter :: IOE :> es => Text -> Eff es ()
paginationFooter msg = do
  mode <- getOutputMode
  when (mode == OutputTable) $ liftIO $ do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
    Data.Text.IO.putStrLn msg
    ANSI.setSGR [ANSI.Reset]


-- Layout

-- | Decide per-column widths and (possibly) truncate the rightmost column to
-- fit the terminal cap. The header counts toward the natural width so single
-- short rows don't squeeze the title to nothing.
layout :: Bool -> Int -> [Text] -> [[Cell]] -> ([Int], [[Cell]])
layout wrapRight cap headers rows =
  let cellText = snd
      n = length headers
      natural =
        [ foldr max (T.length h) [T.length (cellText c) | r <- rows, Just c <- [r !!? i]]
        | (i, h) <- zip [0 ..] headers
        ]
      -- Account for chrome: │ data │ data │ → 3 + (3 * n-1) = 3n separators
      chromeW = 3 * n + 1
      total = sum natural + chromeW
   in if not wrapRight || total <= cap || null natural
        then (natural, rows)
        else
          let slack = total - cap
              widthLast = max 8 (fromMaybe 0 (viaNonEmpty last natural) - slack)
              widths = fromMaybe [] (viaNonEmpty init natural) <> [widthLast]
              truncRow r = case nonEmpty r of
                Nothing -> r
                Just ne ->
                  let (sty, t) = Relude.last ne
                      rest = fromMaybe [] (viaNonEmpty init r)
                   in rest <> [(sty, truncateCell widthLast t)]
           in (widths, map truncRow rows)


truncateCell :: Int -> Text -> Text
truncateCell w t
  | T.length t <= w = t
  | w <= 1 = T.take w t
  | otherwise = T.take (w - 1) t <> "…"


-- Relude already exports `!!?`; use it directly above.


-- Borders

topBorder, midBorder, bottomBorder :: [Int] -> Text
topBorder ws = "╭" <> T.intercalate "┬" [T.replicate (w + 2) "─" | w <- ws] <> "╮"
midBorder ws = "├" <> T.intercalate "┼" [T.replicate (w + 2) "─" | w <- ws] <> "┤"
bottomBorder ws = "╰" <> T.intercalate "┴" [T.replicate (w + 2) "─" | w <- ws] <> "╯"


-- Rows

renderHeaderRow :: Bool -> [Align] -> [Int] -> [Text] -> Text
renderHeaderRow ansi aligns widths headers =
  let cells = zip3 aligns widths (map T.toUpper headers)
      formatted = [pad a w t | (a, w, t) <- cells]
      joined = T.intercalate (sepStyled ansi) (map (boldStyled ansi) formatted)
   in "│ " <> joined <> " │"


renderRow :: Bool -> [Align] -> [Int] -> [Cell] -> Text
renderRow ansi aligns widths row =
  let triples = zip3 aligns widths row
      formatted = [styled ansi sty (pad a w t) | (a, w, (sty, t)) <- triples]
   in "│ " <> T.intercalate (sepStyled ansi) formatted <> " │"


sepStyled :: Bool -> Text
sepStyled _ansi = " │ "


pad :: Align -> Int -> Text -> Text
pad a w t =
  let n = T.length t
      gap = max 0 (w - n)
   in case a of
        AlignLeft -> t <> T.replicate gap " "
        AlignRight -> T.replicate gap " " <> t
        AlignCenter ->
          let lpad = gap `div` 2
              rpad = gap - lpad
           in T.replicate lpad " " <> t <> T.replicate rpad " "


-- Styling

boldStyled :: Bool -> Text -> Text
boldStyled True t = sgr ANSI.BoldIntensity Nothing <> t <> reset
  where
    sgr i _ = T.pack (ANSI.setSGRCode [ANSI.SetConsoleIntensity i])
boldStyled False t = t


reset :: Text
reset = T.pack (ANSI.setSGRCode [ANSI.Reset])


styled :: Bool -> CellStyle -> Text -> Text
styled False _ t = t
styled True s t = wrap (sgrFor s) t
  where
    wrap codes x
      | null codes = x
      | otherwise = T.pack (ANSI.setSGRCode codes) <> x <> reset


sgrFor :: CellStyle -> [ANSI.SGR]
sgrFor = \case
  Plain -> []
  Numeric -> [] -- numeric cells already right-aligned; no color
  Muted -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
  Brand -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]
  Success -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Green]
  Failure -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  Level SevFatal -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  Level SevError -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  Level SevWarn -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]
  Level SevInfo -> [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  Level SevDebug -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
  Level SevTrace -> [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]


-- Helpers

-- | Best-effort terminal width. Falls back to 120 cols when stdout isn't a
-- TTY or when the query fails (e.g. inside tmux without a recent xterm).
termWidth :: IO Int
termWidth = do
  szM <- ANSI.getTerminalSize
  pure $ case szM of
    Just (_, w) | w > 20 -> w
    _ -> 120


-- | Parse a severity tag tolerantly. Returns 'Nothing' for an unrecognised
-- value so callers can fall back to 'Plain' instead of mislabelling.
parseSeverity :: Text -> Maybe Severity
parseSeverity (map toUpper . toString . T.strip -> s) = case s of
  "TRACE" -> Just SevTrace
  "DEBUG" -> Just SevDebug
  "INFO" -> Just SevInfo
  "INFORMATION" -> Just SevInfo
  "WARN" -> Just SevWarn
  "WARNING" -> Just SevWarn
  "ERROR" -> Just SevError
  "ERR" -> Just SevError
  "FATAL" -> Just SevFatal
  "CRITICAL" -> Just SevFatal
  "CRIT" -> Just SevFatal
  _ -> Nothing
