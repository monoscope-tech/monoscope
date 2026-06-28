-- | Minimal parser for the Prometheus text exposition format (version 0.0.4),
-- the body returned by a @/metrics@ endpoint. We only need to recover, per
-- sample line, the metric name, its labels, the scalar value and an optional
-- millisecond timestamp, plus the @# TYPE@/@# HELP@ metadata that precedes a
-- family. Histogram/summary component series (@_bucket@/@_sum@/@_count@) are
-- returned as ordinary scalar samples — the scraper ingests each as its own
-- series, which is enough to make them queryable.
module Pkg.Prometheus (
  SampleType (..),
  Sample (..),
  parsePrometheus,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Relude


data SampleType = Counter | Gauge | Histogram | Summary | Untyped
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | One scraped sample (a single time series point).
data Sample = Sample
  { name :: Text
  , labels :: [(Text, Text)]
  , value :: Double
  , timestampMs :: Maybe Int64
  , sampleType :: SampleType
  , help :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)


-- | Per-family metadata accumulated from @# TYPE@/@# HELP@ lines.
data Meta = Meta {mType :: SampleType, mHelp :: Text}


-- | Parse a full exposition document into samples. Malformed sample lines are
-- skipped (best-effort scraping shouldn't abort a whole batch over one bad row).
--
-- >>> map (\s -> (s.name, s.labels, s.value)) $ parsePrometheus "# TYPE http_requests_total counter\nhttp_requests_total{method=\"post\",code=\"200\"} 1027 1395066363000\n"
-- [("http_requests_total",[("method","post"),("code","200")],1027.0)]
--
-- >>> map (\s -> (s.name, s.value, s.timestampMs)) $ parsePrometheus "metric_without_timestamp_and_labels 12.47\n"
-- [("metric_without_timestamp_and_labels",12.47,Nothing)]
--
-- >>> map (\s -> (s.value)) $ parsePrometheus "weird{problem=\"x\"} +Inf -3982045\nneg -Inf\nnan NaN\n"
-- [Infinity,-Infinity,NaN]
--
-- >>> map (\s -> s.sampleType) $ parsePrometheus "# TYPE x gauge\nx 1\ny 2\n"
-- [Gauge,Untyped]
--
-- OpenMetrics counters declare TYPE on the family name but expose @_total@ samples:
-- >>> map (\s -> s.sampleType) $ parsePrometheus "# TYPE http_requests counter\nhttp_requests_total 5\n"
-- [Counter]
--
-- >>> map (\s -> (s.name, s.labels)) $ parsePrometheus "rpc{quantile=\"0.5\",path=\"/a,b\"} 4773\n"
-- [("rpc",[("quantile","0.5"),("path","/a,b")])]
parsePrometheus :: Text -> [Sample]
parsePrometheus = go Map.empty . lines
  where
    go _ [] = []
    go meta (rawLine : rest) =
      let line = T.strip rawLine
       in if
            | T.null line -> go meta rest
            | "# TYPE " `T.isPrefixOf` line || "#TYPE " `T.isPrefixOf` line ->
                go (recordType meta (T.drop 1 line)) rest
            | "# HELP " `T.isPrefixOf` line || "#HELP " `T.isPrefixOf` line ->
                go (recordHelp meta (T.drop 1 line)) rest
            | "#" `T.isPrefixOf` line -> go meta rest
            | otherwise -> case parseSample meta line of
                Just s -> s : go meta rest
                Nothing -> go meta rest

    -- "TYPE <family> <type>"
    recordType meta body = case words body of
      (_ : fam : ty : _) -> Map.insertWith mergeType fam (Meta (toType ty) "") meta
      _ -> meta
    recordHelp meta body = case words body of
      ws@(_ : fam : _) -> Map.insertWith mergeHelp fam (Meta Untyped (unwords (drop 2 ws))) meta
      _ -> meta
    mergeType new old = old{mType = new.mType}
    mergeHelp new old = old{mHelp = new.mHelp}

    toType = \case
      "counter" -> Counter
      "gauge" -> Gauge
      "histogram" -> Histogram
      "summary" -> Summary
      _ -> Untyped


-- | Parse one sample line: @name[{labels}] value [timestamp]@.
parseSample :: Map.Map Text Meta -> Text -> Maybe Sample
parseSample meta line = do
  let (name, afterName) = T.span isNameChar line
  guard $ not $ T.null name
  (labels, afterLabels) <- case T.uncons (T.stripStart afterName) of
    Just ('{', _) -> do
      let stripped = T.stripStart afterName
      (inside, rest) <- splitBrace (T.drop 1 stripped)
      pure (parseLabels inside, rest)
    _ -> pure ([], afterName)
  case words (T.strip afterLabels) of
    (valTok : tsToks) -> do
      val <- parseValue valTok
      let ts = listToMaybe tsToks >>= parseTs
          Meta{mType, mHelp} = resolveMeta meta name
      pure Sample{name, labels, value = val, timestampMs = ts, sampleType = mType, help = mHelp}
    [] -> Nothing
  where
    isNameChar c = c == '_' || c == ':' || isAsciiLower c || isAsciiUpper c || isDigit c


-- | Look up family metadata, stripping the suffixes that distinguish a sample name
-- from its @# TYPE@ family name so the sample inherits the declared type. Covers
-- histogram/summary component series (@_bucket@/@_sum@/@_count@) and the OpenMetrics
-- counter convention where @# TYPE http_requests counter@ is exposed as
-- @http_requests_total@ (@_total@) — without this a counter is misread as a gauge.
-- Classic exposition (@# TYPE x_total counter@) still matches via the unstripped name.
resolveMeta :: Map.Map Text Meta -> Text -> Meta
resolveMeta meta name = fromMaybe (Meta Untyped "") $ asum (map (`Map.lookup` meta) candidates)
  where
    candidates = name : mapMaybe (`T.stripSuffix` name) ["_bucket", "_sum", "_count", "_total"]


-- | Split at the matching @}@, returning (inside, after). Honours quoted
-- strings so a @}@ inside a label value doesn't end the block early.
splitBrace :: Text -> Maybe (Text, Text)
splitBrace = go False "" . toString
  where
    go _ acc [] = Just (toText (reverse acc), "")
    go inQ acc (c : cs)
      | c == '"' = go (not inQ) (c : acc) cs
      | c == '\\', inQ, (n : ns) <- cs = go inQ (n : c : acc) ns
      | c == '}', not inQ = Just (toText (reverse acc), toText cs)
      | otherwise = go inQ (c : acc) cs


-- | Parse the comma-separated @name="value"@ pairs inside @{...}@.
parseLabels :: Text -> [(Text, Text)]
parseLabels = go . T.stripStart
  where
    go t
      | T.null t = []
      | otherwise =
          let (k, afterK) = T.span isLabelNameChar t
              afterEq = T.stripStart $ T.drop 1 $ T.stripStart afterK -- drop '='
           in case T.uncons afterEq of
                Just ('"', body) ->
                  let (v, rest) = takeQuoted body
                      rest' = T.stripStart $ T.dropWhile (== ',') $ T.stripStart rest
                   in if T.null k then go rest' else (k, v) : go rest'
                _ -> []
    isLabelNameChar c = c == '_' || isAsciiLower c || isAsciiUpper c || isDigit c
    -- consume up to the closing unescaped quote, unescaping \\ \" \n
    takeQuoted = go' ""
      where
        go' acc t = case T.uncons t of
          Nothing -> (toText (reverse acc), "")
          Just ('"', rest) -> (toText (reverse acc), rest)
          Just ('\\', rest) -> case T.uncons rest of
            Just ('n', r) -> go' ('\n' : acc) r
            Just (c, r) -> go' (c : acc) r
            Nothing -> (toText (reverse acc), "")
          Just (c, rest) -> go' (c : acc) rest


-- | Go-style float literal, including @+Inf@/@-Inf@/@NaN@.
parseValue :: Text -> Maybe Double
parseValue (T.strip -> t) = case t of
  "+Inf" -> Just (1 / 0)
  "Inf" -> Just (1 / 0)
  "-Inf" -> Just (-(1 / 0))
  "NaN" -> Just (0 / 0)
  "Nan" -> Just (0 / 0)
  _ -> readMaybe (toString (T.dropWhile (== '+') t))


parseTs :: Text -> Maybe Int64
parseTs t = readMaybe (toString t) <|> (truncate <$> (readMaybe (toString t) :: Maybe Double))
