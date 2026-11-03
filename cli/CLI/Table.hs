-- | Pretty-printed Unicode-box tables for the CLI.
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

  -- * Rendering
  renderRichTableWith,
  RichTableOpts (..),
  defaultRichTableOpts,

  -- * Pagination cue
  paginationFooter,

  -- * Severity parser
  parseSeverity,

  -- * Terminal size
  termWidth,
) where

import Relude

import CLI.Core (OutputMode (..), getOutputMode, isJsonOutput)
import Data.Char (toUpper)
import Data.Text qualified as T
import Effectful
import System.Console.ANSI qualified as ANSI
import System.IO (hIsTerminalDevice)
import Text.Layout.Table (LineStyle (SingleLine), center, column, columnHeaderTableS, expand, expandUntil, fullH, headerColumn, left, noAlign, right, rowsG, simpleTableStyleSpec, singleCutMark, tableLines, unicodeTableStyleFromSpec, withRoundCorners)
import Text.Layout.Table.Cell.Formatted qualified as F


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


plain, numeric, muted, brand :: Text -> Cell
plain t = (Plain, t)
numeric t = (Numeric, t)
muted t = (Muted, t)
brand t = (Brand, t)


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
  -- ^ Hard cap on total width. When 'Nothing' we query the terminal. The
  -- widest column (usually a title) is truncated with @…@ to fit.
  }
  deriving stock (Show)


defaultRichTableOpts :: RichTableOpts
defaultRichTableOpts = RichTableOpts{alignments = [], maxWidth = Nothing}


-- | Render with explicit knobs.
--
-- - In JSON/YAML mode this is a no-op (rendering goes through 'renderJSON'
--   on the same payload at the call site). The renderer never emits
--   anything in non-table mode so a buggy caller can't leak ANSI.
-- - In table mode but a non-TTY stdout (e.g. @--table | cat@), ANSI is
--   still emitted because the user explicitly asked for the table view.
renderRichTableWith :: IOE :> es => RichTableOpts -> [Text] -> [[Cell]] -> Eff es ()
renderRichTableWith opts headers rows = whenM ((== OutputTable) <$> getOutputMode) do
  ansi <- not <$> isJsonOutput
  cap <- maybe (liftIO termWidth) pure opts.maxWidth
  let natural = [foldl' max (T.length h) [T.length (snd c) | Just c <- map (!!? i) rows] | (i, h) <- zip [0 ..] headers]
      widest = maybe 0 fst (viaNonEmpty head (sortWith (Down . snd) (zip [0 :: Int ..] natural)))
      -- The widest column absorbs the overflow (│ x │ y │ costs 3n + 1 columns of chrome).
      fitW = max 8 (cap - 3 * length headers - 1 - sum natural + fromMaybe 0 (natural !!? widest))
      positions = [case a of AlignLeft -> left; AlignRight -> right; AlignCenter -> center | a <- zipWith const (opts.alignments <> repeat AlignLeft) headers]
      cut = singleCutMark "…"
      spec i p = column (if i == widest then expandUntil fitW else expand) p noAlign cut
      sgr codes t = (if ansi && not (null codes) then \x -> F.formatted (ANSI.setSGRCode codes) x (ANSI.setSGRCode [ANSI.Reset]) else id) (F.plain t)
      style = withRoundCorners (unicodeTableStyleFromSpec (simpleTableStyleSpec SingleLine SingleLine))
      header = fullH [headerColumn p (Just cut) | p <- positions] [sgr [ANSI.SetConsoleIntensity ANSI.BoldIntensity] (T.toUpper h) | h <- headers]
      table = columnHeaderTableS (zipWith spec [0 ..] positions) style header [rowsG [[sgr (sgrFor sty) t | (sty, t) <- r] | r <- rows]]
  liftIO $ mapM_ (putStrLn . toString) (tableLines table)


-- | One line of muted text, intended for "… N more — use --limit / --cursor"
-- hints below a list. Skipped in JSON mode so a piped consumer sees only
-- payload, no chrome.
paginationFooter :: IOE :> es => Text -> Eff es ()
paginationFooter msg = do
  mode <- getOutputMode
  when (mode == OutputTable) $ liftIO $ do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
    putTextLn msg
    ANSI.setSGR [ANSI.Reset]


-- Styling

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
  -- getTerminalSize reads stdin, which throws EOF under a pipe or a CI runner.
  szM <- ifM (hIsTerminalDevice stdout) ANSI.getTerminalSize (pure Nothing)
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
