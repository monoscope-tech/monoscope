-- | Log and span rendering for the terminal.
--
-- The boxed table is right for aggregate results but wrong for a log stream:
-- during an incident you scan a column of timestamps and messages, and box
-- borders every row fight that. So the default here is one event per line —
-- time, severity, service, message, then the identifiers you'd grep for —
-- which is also what @tail -f@-shaped usage needs.
module CLI.LogView (
  LogFormat (..),
  parseLogFormat,
  EventRow (..),
  eventRows,
  renderEventLine,
  renderLogfmt,
  renderWaterfall,
) where

import Relude

import CLI.Chart (colorize, dim, ellipsize, formatValue, padTo)
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Pkg.CLIFormat (extractRows, renderSummaryItems)
import System.Console.ANSI qualified as ANSI


-- | How @events\/logs\/traces search@ and @tail@ print rows on a terminal.
-- JSON and YAML are handled a level up by the global output mode; this only
-- picks between the human-facing shapes.
data LogFormat
  = -- | One event per line: time, severity, service, message. The default.
    FmtLine
  | -- | The bordered column table, for when the columns matter more than the flow.
    FmtTable
  | -- | @key=value@ pairs — greppable, and what most log shippers expect.
    FmtLogfmt
  deriving stock (Eq, Show)


-- | >>> map parseLogFormat ["line", "table", "logfmt", "nope"]
-- [Right FmtLine,Right FmtTable,Right FmtLogfmt,Left "--format must be one of: line, table, logfmt; got 'nope'"]
parseLogFormat :: Text -> Either Text LogFormat
parseLogFormat t = case T.toLower (T.strip t) of
  "line" -> Right FmtLine
  "table" -> Right FmtTable
  "logfmt" -> Right FmtLogfmt
  other -> Left $ "--format must be one of: line, table, logfmt; got '" <> other <> "'"


-- | One event, resolved from the server's column-indexed rows into the fields a
-- log line needs. Anything the query didn't select is 'Nothing' and simply
-- doesn't appear on the line.
data EventRow = EventRow
  { eventId :: Maybe Text
  , timestamp :: Maybe Text
  , service :: Maybe Text
  , spanName :: Maybe Text
  , kind :: Maybe Text
  , durationNs :: Maybe Double
  , traceId :: Maybe Text
  , parentId :: Maybe Text
  , startNs :: Maybe Double
  , summary :: Maybe Text
  , isError :: Bool
  , extras :: [(Text, Text)]
  -- ^ Every other selected column, in the order the server sent them, so a
  -- @project@-ed query still shows what it asked for.
  }
  deriving stock (Eq, Show)


-- | Pull the rows out of an @\/api\/v1\/events@ envelope. Columns are located
-- by name via @colIdxMap@ rather than position, so a change to the default
-- projection can't silently shift every field one column to the left.
eventRows :: AE.Value -> [EventRow]
eventRows = \case
  AE.Object obj ->
    let idx = colIdxMap (KM.lookup "colIdxMap" obj)
        at name row = Map.lookup name idx >>= \i -> guarded (not . T.null) =<< (row !!? i)
        known = ["id", "timestamp", "trace_id", "span_name", "duration", "service", "kind", "summary", "errors", "parent_id", "start_time_ns", "latency_breakdown"]
        others = [(n, i) | (n, i) <- Map.toList idx, n `notElem` known]
     in [ EventRow
        { eventId = at "id" row
        , timestamp = at "timestamp" row
        , service = at "service" row
        , spanName = at "span_name" row
        , kind = at "kind" row
        , durationNs = readMaybe . toString =<< at "duration" row
        , traceId = at "trace_id" row
        , parentId = at "parent_id" row
        , startNs = readMaybe . toString =<< at "start_time_ns" row
        , summary = at "summary" row
        , isError = at "errors" row `elem` [Just "true", Just "t", Just "True"]
        , extras = [(n, v) | (n, i) <- others, Just v <- [guarded (not . T.null) =<< (row !!? i)]]
        }
        | row <- extractRows (KM.lookup "logsData" obj)
        ]
  _ -> []


colIdxMap :: Maybe AE.Value -> Map Text Int
colIdxMap = \case
  Just (AE.Object obj) -> Map.fromList [(AK.toText k, floor n) | (k, AE.Number n) <- KM.toList obj]
  _ -> mempty


-- | A single event as one terminal line. Columns are fixed-width up to the
-- message so the eye can run straight down the timestamps and services; the
-- message and trailing identifiers take whatever is left.
renderEventLine :: Bool -> Int -> EventRow -> Text
renderEventLine color width r =
  T.stripEnd
    $ T.intercalate
      " "
      [ dim color (padTo 12 (clockTime r.timestamp))
      , levelTag color r
      , colorize color ANSI.Cyan (padTo 18 (ellipsize 18 (fromMaybe "-" r.service)))
      , dim color (padTo 7 (maybe "" (formatValue "ms" . (/ 1e6)) r.durationNs))
      , ellipsize msgWidth message
      ]
      <> trailer
  where
    msgWidth = max 20 (width - 12 - 5 - 18 - 7 - 5 - T.length (stripStyle trailer))
    message = maybe (fromMaybe "" r.spanName) renderSummary r.summary
    trailer = mconcat [dim color ("  " <> k <> "=" <> v) | (k, v) <- take 4 r.extras] <> maybe "" (\t -> dim color ("  trace=" <> T.take 12 t)) r.traceId
    stripStyle = T.filter (/= '\ESC')


-- | The @summary@ column is the platform's own display form for an event
-- (@field;style⇒value@ items); reusing it keeps the CLI's message text
-- identical to the log explorer's.
renderSummary :: Text -> Text
renderSummary cell = case AE.eitherDecode @[Text] (encodeUtf8 cell) of
  Right items -> renderSummaryItems items
  Left _ -> cell


-- | Severity badge. Spans have no severity of their own, so an error span shows
-- @ERR@ and everything else shows its kind — the shape and position stay
-- constant either way, which is what makes the column scannable.
levelTag :: Bool -> EventRow -> Text
levelTag color r
  | r.isError = colorize color ANSI.Red (padTo 5 "ERR")
  | otherwise = case T.toUpper (fromMaybe "" r.kind) of
      "LOG" -> dim color (padTo 5 "LOG")
      k | T.null k -> padTo 5 ""
      k -> dim color (padTo 5 (T.take 5 k))


-- | @key=value@ form. Values containing spaces are quoted; nothing else is
-- escaped, matching the logfmt convention.
--
-- >>> renderLogfmt (EventRow (Just "e1") (Just "2026-08-06T01:02:03Z") (Just "api") Nothing (Just "log") Nothing Nothing (Just "boom now") True [])
-- "ts=2026-08-06T01:02:03Z level=error service=api kind=log msg=\"boom now\" id=e1"
renderLogfmt :: EventRow -> Text
renderLogfmt r =
  T.unwords
    $ [k <> "=" <> quote v | (k, Just v) <- pairs]
    <> [k <> "=" <> quote v | (k, v) <- r.extras]
  where
    pairs =
      [ ("ts", r.timestamp)
      , ("level", Just (if r.isError then "error" else "info"))
      , ("service", r.service)
      , ("kind", r.kind)
      , ("span", r.spanName)
      , ("dur_ms", show . (/ 1e6) <$> r.durationNs)
      , ("msg", renderSummary <$> r.summary)
      , ("trace", r.traceId)
      , ("id", r.eventId)
      ]
    quote v = if T.any (== ' ') v then "\"" <> v <> "\"" else v


-- | Trace waterfall: one row per span, indented by depth, with a bar showing
-- where the span sat inside the trace's total span. This is the view that
-- answers "what actually took the time", which a flat list of spans can't.
--
-- Spans whose parent isn't in the result set are treated as roots, so a
-- partially fetched trace still renders instead of vanishing.
renderWaterfall :: Bool -> Int -> [EventRow] -> [Text]
renderWaterfall color width rows
  | null rows = ["no spans in this trace"]
  | otherwise = header : concatMap (walk 0) roots
  where
    ids = Set.fromList (mapMaybe (.eventId) rows)
    kids p = sortOn (.startNs) [r | r <- rows, r.parentId == Just p]
    roots = sortOn (.startNs) [r | r <- rows, maybe True (`Set.notMember` ids) r.parentId]
    t0 = foldl' min (1 / 0) (mapMaybe (.startNs) rows)
    t1 = foldl' max 0 [s + fromMaybe 0 r.durationNs | r <- rows, Just s <- [r.startNs]]
    total = max 1 (t1 - t0)
    labelW = min 44 (max 20 (width - barW - 12))
    barW = max 10 (width `div` 3)
    header = dim color (padTo labelW "span" <> " " <> padTo barW ("0 → " <> formatValue "ms" (total / 1e6)) <> " duration")
    walk depth r =
      line depth r : concatMap (walk (depth + 1)) (maybe [] kids r.eventId)
    line depth r =
      padTo labelW (T.replicate depth " " <> ellipsize (labelW - depth) (label r))
        <> " "
        <> padTo barW (barFor r)
        <> " "
        <> dim color (maybe "" (formatValue "ms" . (/ 1e6)) r.durationNs)
    label r = fromMaybe "-" (r.spanName <|> r.service)
    barFor r = case (r.startNs, r.durationNs) of
      (Just s, Just d) ->
        let lead = floor ((s - t0) / total * fromIntegral barW)
            len = max 1 (round (d / total * fromIntegral barW))
         in T.replicate lead " " <> colorize color (if r.isError then ANSI.Red else ANSI.Cyan) (T.replicate (min (barW - lead) len) "█")
      _ -> ""


-- | @HH:MM:SS.mmm@ from an ISO-8601 timestamp. Kept as text slicing rather than
-- a parse+format round trip: the server already emits UTC in a fixed layout,
-- and an unparseable value should still show something rather than blank out
-- the whole column.
--
-- >>> clockTime (Just "2026-08-06T01:02:03.456789Z")
-- "01:02:03.456"
-- >>> clockTime Nothing
-- "-"
clockTime :: Maybe Text -> Text
clockTime Nothing = "-"
clockTime (Just t) = case T.splitOn "T" t of
  [_, rest] -> T.take 12 (T.takeWhile (/= 'Z') rest)
  _ -> T.take 12 t
