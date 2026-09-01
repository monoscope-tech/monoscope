module Pages.LogExplorer.Log (
  apiLogH,
  logExplorerDataH,
  logExplorerSchemaH,
  logExplorerValidateH,
  QueryValidation (..),
  logExplorerFacetsH,
  logPatternsH,
  logSessionsH,
  alertFormH,
  apiLogExpandH,
  aiSearchH,
  queryEvents,
  LogsGet (..),
  LogResult (..),
  PatternsView (..),
  SessionsView (..),
  ApiLogsPageData (..),
  virtualTable,
  curateCols,
  logQueryBox_,
  TraceTreeEntry (..),
  buildTraceTree,
  synthesizeOrphanHeaders,
  colsFitRows,
  fmtPct1,
  -- Sidebar facet definitions — exported for high-level tests.
  Facet (..),
  FacetGroup (..),
  facetDefs,
  renderFacets,
)
where

import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.Effectful.Hasql (Hasql)
import Data.Effectful.Hasql qualified as Hasql
import Data.Foldable.WithIndex (iforM_)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Labeled (Labeled)
import Effectful.Log qualified as ELog
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Numeric (showFFloat)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, pageActions, pageTitle)
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..), enrichSchemaWithFacets, logQueryBox_)
import Pkg.Components.TimePicker qualified as Components
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (WTTimeseries, WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (PageCursor (..), PageDirection (..), defaultQueryLimit, pSource, parseQueryToAST, toQText)
import Pkg.Parser.Expr qualified as ParserExpr
import Pkg.Parser.Stats (QueryError (..), Section (TakeCommand), parseQueryDiagnosed)
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetSummary (..), FacetValue (..))
import Relude hiding (ask)
import Relude.Extra.Foldable1 (maximum1, minimum1)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Text.Casing (fromAny, toKebab)
import Text.Megaparsec (parseMaybe)
import Utils (FieldAction (..), FieldMenuCtx (..), LoadingSize (..), LoadingType (..), checkFreeTierStatus, explorerNavTabs_, faSprite_, fieldContextMenuItems_, fieldMenuPanel_, getDurationNSMS, getServiceColors, htmxOverlayIndicator_, levelFillColor, listToIndexHashMap, loadingIndicator_, lookupVecTextByKey, methodFillColor, popoverTrigger_, prettyPrintCount, sanitizeBackendError, serviceFillColor, statusFillColorText, toUriStr)

import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Models.Apis.Monitors (MonitorAlertConfig (..))
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as ManageMembers
import Pages.Components (FieldCfg (..), FieldSize (..), facetOption_, facetRail_, facetSection_, formField_, localTimeFmt_, resizer_)
import Pages.Monitors qualified as AlertUI
import Pkg.AI qualified as AI

import BackgroundJobs qualified
import Data.Map.Strict qualified as Map
import Data.OpenApi (ToSchema (..))
import Data.Pool (withResource)
import Data.Scientific (toBoundedInteger)
import Data.Set qualified as S
import Deriving.Aeson qualified as DAE
import Deriving.Aeson.Stock qualified as DAE
import OddJobs.Job (createJob)
import Pkg.DeriveUtils (CamelSchema (..), SnakeSchema (..))
import System.Logging qualified as Log
import System.Tracing (Tracing, withSpan_)
import Text.Slugify (slugify)
import UnliftIO.Exception (tryAny)


data TraceTreeEntry = TraceTreeEntry
  { traceId :: Text
  , startTime :: Int64
  , duration :: Int64
  , traceStartTime :: Maybe Text
  , root :: Text
  , children :: Map.Map Text [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.ToJSON) via DAE.Snake TraceTreeEntry
  deriving (ToSchema) via SnakeSchema TraceTreeEntry


data SpanInfo = SpanInfo {spanId :: Text, parentId :: Maybe Text, traceIdVal :: Text, startNs :: Int64, dur :: Int64, timestamp :: Maybe Text, isQueryResult :: Bool, rowIdx :: Int}


-- | Build trace tree from flat rows. Query-result spans (index < queryResultCount)
-- become roots; other spans (fetched via selectChildSpansAndLogs) nest beneath them.
--
-- >>> import Relude
-- >>> import Data.Vector qualified as V
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.HashMap.Strict qualified as HM
-- >>> import "monoscope" Pages.LogExplorer.Log qualified as LL
-- >>> let colIdx = HM.fromList [("id",0),("trace_id",1),("parent_id",2),("start_time_ns",3),("duration",4),("latency_breakdown",5),("kind",6),("errors",7),("timestamp",8)]
-- >>> let row1 = V.fromList [AE.String "s1", AE.String "t1", AE.Null, AE.Number 100, AE.Number 1000, AE.String "lb1", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let row2 = V.fromList [AE.String "s2", AE.String "t1", AE.String "lb1", AE.Number 200, AE.Number 500, AE.String "lb2", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:01Z"]
-- >>> let vecs = V.fromList [row1, row2]
-- >>> let (_, result) = LL.buildTraceTree colIdx 1 vecs
-- >>> length result
-- 1
-- >>> fmap (.root) (viaNonEmpty head result)
-- Just "lb1"
--
-- Non-query-result spans (orphans) whose parent is not in result set are discarded (not duplicated across roots):
--
-- >>> let colIdx2 = HM.fromList [("id",0),("trace_id",1),("parent_id",2),("start_time_ns",3),("duration",4),("latency_breakdown",5),("kind",6),("errors",7),("timestamp",8)]
-- >>> let qr = V.fromList [AE.String "s1", AE.String "t1", AE.Null, AE.Number 100, AE.Number 1000, AE.String "rt", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let ch = V.fromList [AE.String "s2", AE.String "t1", AE.String "rt", AE.Number 200, AE.Number 500, AE.String "ch1", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:01Z"]
-- >>> let orph = V.fromList [AE.String "s3", AE.String "t1", AE.String "missing", AE.Number 300, AE.Number 100, AE.String "orp", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:02Z"]
-- >>> let (_, r2) = LL.buildTraceTree colIdx2 1 (V.fromList [qr, ch, orph])
-- >>> length r2
-- 1
-- >>> fmap (.children) (viaNonEmpty head r2)
-- Just (fromList [("rt",["ch1"])])
--
-- Deeply nested spans (>5 levels) preserve full hierarchy:
--
-- >>> let colIdx = HM.fromList [("id",0),("trace_id",1),("parent_id",2),("start_time_ns",3),("duration",4),("latency_breakdown",5),("kind",6),("errors",7),("timestamp",8)]
-- >>> let mkSpan lb par ns = V.fromList [AE.String lb, AE.String "t1", maybe AE.Null AE.String par, AE.Number ns, AE.Number 100, AE.String lb, AE.String "span", AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let rows = V.fromList [mkSpan "L0" Nothing 100, mkSpan "L1" (Just "L0") 200, mkSpan "L2" (Just "L1") 300, mkSpan "L3" (Just "L2") 400, mkSpan "L4" (Just "L3") 500, mkSpan "L5" (Just "L4") 600, mkSpan "L6" (Just "L5") 700]
-- >>> import Data.Map.Strict qualified as Map
-- >>> let (_, r) = LL.buildTraceTree colIdx 1 rows
-- >>> length r
-- 1
-- >>> Map.size . (.children) <$> viaNonEmpty head r
-- Just 6
-- >>> Map.lookup "L4" . (.children) =<< viaNonEmpty head r
-- Just ["L5"]
--
-- Mixed logs (kind=log) and spans in same trace; all entries use parent_id:
--
-- >>> let colIdx = HM.fromList [("id",0),("trace_id",1),("parent_id",2),("start_time_ns",3),("duration",4),("latency_breakdown",5),("kind",6),("errors",7),("timestamp",8)]
-- >>> let rootSpan = V.fromList [AE.String "s1", AE.String "t1", AE.Null, AE.Number 100, AE.Number 1000, AE.String "root-span", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let childSpan = V.fromList [AE.String "s2", AE.String "t1", AE.String "root-span", AE.Number 200, AE.Number 500, AE.String "child-span", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:01Z"]
-- >>> let logEntry = V.fromList [AE.String "log1", AE.String "t1", AE.String "child-span", AE.Number 250, AE.Number 0, AE.String "child-span", AE.String "log", AE.Null, AE.String "2025-01-01T00:00:02Z"]
-- >>> let (_, r) = LL.buildTraceTree colIdx 1 (V.fromList [rootSpan, childSpan, logEntry])
-- >>> length r
-- 1
-- >>> Map.lookup "root-span" . (.children) =<< viaNonEmpty head r
-- Just ["child-span"]
-- >>> Map.lookup "child-span" . (.children) =<< viaNonEmpty head r
-- Just ["log1"]
--
-- Clock skew: child raw start (50) is before parent (100). Adjusted child start
-- shifts to 100 and the trace start window matches the parent's start:
--
-- >>> let colIdxS = HM.fromList [("id",0),("trace_id",1),("parent_id",2),("start_time_ns",3),("duration",4),("latency_breakdown",5),("kind",6),("errors",7),("timestamp",8)]
-- >>> let parent = V.fromList [AE.String "p", AE.String "t1", AE.Null, AE.Number 100, AE.Number 1000, AE.String "p", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let skewed = V.fromList [AE.String "c", AE.String "t1", AE.String "p", AE.Number 50, AE.Number 200, AE.String "c", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:01Z"]
-- >>> let (adj, rs) = LL.buildTraceTree colIdxS 1 (V.fromList [parent, skewed])
-- >>> fmap (.startTime) (viaNonEmpty head rs)
-- Just 100
-- >>> (adj V.! 1) V.!? 3
-- Just (Number 100.0)
--
-- A late point event does not stretch the axis. The span runs 100..1100; a log at 50000
-- belongs to the trace but has no extent, and letting it set the window shrank every span
-- in the latency column to a sliver at the left edge:
--
-- >>> let mk i par ns d k = V.fromList [AE.String i, AE.String "t1", maybe AE.Null AE.String par, AE.Number ns, AE.Number d, AE.String i, AE.String k, AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let (_, rw) = LL.buildTraceTree colIdxS 1 (V.fromList [mk "p" Nothing 100 1000 "span", mk "l" (Just "p") 50000 0 "log"])
-- >>> fmap (\e -> (e.startTime, e.duration)) (viaNonEmpty head rw)
-- Just (100,1000)
--
-- With nothing but point events there is no span to size the window by, so it keeps the
-- full extent rather than collapsing to zero:
--
-- >>> let (_, rp) = LL.buildTraceTree colIdxS 1 (V.fromList [mk "l1" Nothing 100 0 "log", mk "l2" (Just "l1") 900 0 "log"])
-- >>> fmap (.duration) (viaNonEmpty head rp)
-- Just 800
--
-- A cyclic parent chain terminates. @parent_id@ is instrumentation-supplied, so a broken SDK
-- can emit @a -> b -> a@; walking that without a visited set spins the request thread forever,
-- holding its database connection. Each span is visited once and the back-edge is pruned, so
-- the adjacency that reaches the client is acyclic rather than merely survivable:
--
-- >>> let (_, rc) = LL.buildTraceTree colIdxS 1 (V.fromList [mk "a" (Just "b") 100 10 "span", mk "b" (Just "a") 200 10 "span"])
-- >>> Map.lookup "a" . (.children) =<< viaNonEmpty head rc
-- Just ["b"]
-- >>> Map.lookup "b" . (.children) =<< viaNonEmpty head rc
-- Nothing
--
-- A span that is its own parent terminates too:
--
-- >>> let (_, rs') = LL.buildTraceTree colIdxS 1 (V.fromList [mk "r" Nothing 100 10 "span", mk "s" (Just "s") 200 10 "span"])
-- >>> length rs'
-- 1
buildTraceTree :: HM.HashMap Text Int -> Int -> V.Vector (V.Vector AE.Value) -> (V.Vector (V.Vector AE.Value), [TraceTreeEntry])
buildTraceTree colIdxMap queryResultCount rows
  -- An aggregate result (`| summarize …`) is one or two columns wide while the
  -- column map still describes the log table, so writing a span's adjusted
  -- start/duration back at those indices threw `index out of bounds`. Such rows
  -- carry no spans to nest: hand them back untouched.
  | not (colsFitRows colIdxMap rows) = (rows, [])
  | otherwise = (adjustedRows, sortWith (Down . (.startTime)) entries)
  where
    lookupIdx = flip HM.lookup colIdxMap
    txt k row = mfilter (not . T.null) $ lookupVecTextByKey row colIdxMap k
    int64 k row = (lookupIdx k >>= (row V.!?)) >>= \case AE.Number n -> toBoundedInteger n :: Maybe Int64; _ -> Nothing

    mkSpanInfo :: Int -> V.Vector AE.Value -> SpanInfo
    mkSpanInfo idx row =
      let isLog = txt "kind" row == Just "log"
          rawId = fromMaybe ("gen-" <> show idx) $ txt "id" row
          sid = if isLog then rawId else fromMaybe rawId (txt "latency_breakdown" row)
          pid = txt "parent_id" row
          tid = fromMaybe ("gen-trace-" <> show idx) $ txt "trace_id" row
          sns = fromMaybe 0 $ int64 "start_time_ns" row
          d = if isLog then 0 else fromMaybe 0 (int64 "duration" row)
       in SpanInfo sid pid tid sns d (txt "timestamp" row) (idx < queryResultCount) idx

    spanInfos = V.imap mkSpanInfo rows

    grouped :: Map.Map Text [SpanInfo]
    grouped = Map.fromListWith (<>) [(si.traceIdVal, [si]) | si <- V.toList spanInfos]

    -- Per-trace results: (entry, [(rowIdx, adjStart, adjDur)])
    traceResults = concatMap buildTraceEntries (Map.elems grouped)
    entries = map fst traceResults
    adjustments = concatMap snd traceResults

    -- Apply adjustments to row vectors at start_time_ns / duration columns.
    stIdxM = lookupIdx "start_time_ns"
    durIdxM = lookupIdx "duration"
    adjMap :: Map.Map Int (Int64, Int64)
    adjMap = Map.fromList [(i, (s, d)) | (i, s, d) <- adjustments]
    adjustedRows = V.imap applyAdj rows
      where
        applyAdj i row = maybe row adjustRow (Map.lookup i adjMap)
          where
            adjustRow (s, d) =
              let upd =
                    catMaybes
                      [ (,AE.Number (fromIntegral s)) <$> stIdxM
                      , (,AE.Number (fromIntegral d)) <$> durIdxM
                      ]
               in if null upd then row else row V.// upd

    buildTraceEntries :: [SpanInfo] -> [(TraceTreeEntry, [(Int, Int64, Int64)])]
    buildTraceEntries spans =
      let spanMap = Map.fromList $ map (\s -> (s.spanId, s)) spans
          childrenMap :: Map.Map Text [Text]
          childrenMap = Map.fromListWith (<>) [(pid, [s.spanId]) | s <- spans, Just pid <- [s.parentId], Map.member pid spanMap]
          sortedChildrenMap = Map.map (sortWith \x -> maybe 0 (.startNs) (Map.lookup x spanMap)) childrenMap
          parentIsQR s = maybe False (.isQueryResult) (s.parentId >>= flip Map.lookup spanMap)
          roots = filter (\s -> s.isQueryResult && not (parentIsQR s)) spans
          traceStartTime = viaNonEmpty minimum1 $ mapMaybe (.timestamp) spans
          tid = maybe "" (.traceIdVal) (viaNonEmpty head spans)
       in map (buildEntry tid sortedChildrenMap spanMap traceStartTime) roots

    -- Clock-skew correction: shift a child forward if it starts before its parent's
    -- adjusted start, clamping duration to the parent's window. Mirrors Pages.Telemetry.buildSpanTree.
    buildEntry :: Text -> Map.Map Text [Text] -> Map.Map Text SpanInfo -> Maybe Text -> SpanInfo -> (TraceTreeEntry, [(Int, Int64, Int64)])
    buildEntry tid fullChildrenMap spanMap tst root' =
      let rootEnd = root'.startNs + root'.dur
          -- Same back-edge pruning as `go`, for the one edge it cannot see: a root listed as
          -- its own child (a span whose parent_id is its own span id).
          rootKids = filter (/= root'.spanId) $ Map.findWithDefault [] root'.spanId fullChildrenMap
          initAcc = Map.fromList [(root'.spanId, rootKids) | not (null rootKids)]
          rootAdj = (root'.rowIdx, root'.startNs, root'.dur)
          -- `seen` makes the walk terminate on a cyclic parent chain. `parent_id` comes from
          -- instrumentation, so `a -> b -> a` (or a self-parent) is reachable from any broken
          -- SDK, and without this the recursion never returns: the request thread spins on a
          -- core holding its database connection until the process is restarted.
          go _ _ [] st = st
          go pStart pEnd (x : xs) st@(minS, maxE, adjs, treeAcc, seen)
            | S.member x seen = go pStart pEnd xs st
            | otherwise = case Map.lookup x spanMap of
                Nothing -> go pStart pEnd xs st
                Just si ->
                  let cStart = si.startNs
                      delta = pStart - cStart
                      (adjStart, adjDur) =
                        if delta > 0
                          then (cStart + delta, max 0 (min si.dur (pEnd - cStart - delta)))
                          else (cStart, si.dur)
                      adjEnd = adjStart + adjDur
                      seen' = S.insert x seen
                      -- Drop back-edges rather than merely declining to follow them, so the
                      -- adjacency map that goes over the wire is acyclic for every client, not
                      -- just for the one traversal here. A child already placed elsewhere in
                      -- this trace keeps its first position.
                      kids = filter (`S.notMember` seen') $ Map.findWithDefault [] x fullChildrenMap
                      treeAcc' = if null kids then treeAcc else Map.insert x kids treeAcc
                      st' = (min minS adjStart, max maxE adjEnd, (si.rowIdx, adjStart, adjDur) : adjs, treeAcc', seen')
                      st'' = go adjStart adjEnd kids st'
                   in go pStart pEnd xs st''
          (fullStart, fullEnd, adjustments', subtreeChildren, _) =
            go root'.startNs rootEnd rootKids (root'.startNs, rootEnd, [rootAdj], initAcc, one root'.spanId)
          -- Only rows that lasted set the window. A point event carries a time but no
          -- extent, and one log emitted seconds later under the same trace used to define
          -- the axis for every span in it: a 71ms trace whose tail log lands at +3.4s
          -- became a 3452ms axis, crushing every real span into the first 2% of the
          -- latency column. Logs still render — the client clamps them into the window —
          -- they just no longer stretch it. A trace of nothing but point events keeps the
          -- full extent, since there is no span to size it by.
          (minStart, maxEnd) = case nonEmpty [(s, s + d) | (_, s, d) <- adjustments', d > 0] of
            Just lasting -> (minimum1 $ fmap fst lasting, maximum1 $ fmap snd lasting)
            Nothing -> (fullStart, fullEnd)
       in (TraceTreeEntry tid minStart (maxEnd - minStart) tst root'.spanId subtreeChildren, adjustments')


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


-- | Whether every column index addresses a real slot in the result rows.
-- False for anything that isn't the log-table shape — an aggregate projection,
-- most obviously — and the trace-tree machinery must then keep its hands off.
--
-- >>> colsFitRows (HM.fromList [("id",0),("duration",7)]) (V.singleton (V.replicate 12 AE.Null))
-- True
-- >>> colsFitRows (HM.fromList [("id",0),("duration",7)]) (V.singleton (V.singleton AE.Null))
-- False
-- >>> colsFitRows (HM.fromList [("id",0)]) V.empty
-- False
colsFitRows :: HM.HashMap Text Int -> V.Vector (V.Vector AE.Value) -> Bool
colsFitRows colIdxMap rows = maybe False (\w -> all (< w) (HM.elems colIdxMap)) (V.length <$> rows V.!? 0)


-- | Detect query-result spans whose parent_id is missing from the result and
-- ≥2 of them share that same parent_id. Emit one synthetic row per group whose
-- latency_breakdown = parent_id so 'buildTraceTree' nests the orphans under
-- it, matching the trace-breakdown waterfall's visual contract.
synthesizeOrphanHeaders :: HM.HashMap Text Int -> V.Vector (V.Vector AE.Value) -> V.Vector (V.Vector AE.Value)
synthesizeOrphanHeaders colIdxMap rows
  | not (colsFitRows colIdxMap rows) = V.empty
  | otherwise = V.fromList [synthRow t p ks | ((t, p), ks) <- Map.toList groups, length ks >= 2]
  where
    lookupIdx = flip HM.lookup colIdxMap
    colCount = maybe 0 V.length (rows V.!? 0)
    textAt k r = mfilter (not . T.null) $ lookupVecTextByKey r colIdxMap k
    numAt k r = lookupIdx k >>= (r V.!?) >>= \case AE.Number n -> Just (round n :: Integer); _ -> Nothing
    presentIds = S.fromList $ V.toList $ V.mapMaybe (textAt "latency_breakdown") rows
    -- Combined orphan-detect + key extraction: trace_id + parent_id where the
    -- parent_id is non-empty and not present as any row's span_id.
    keyOf r = do
      p <- textAt "parent_id" r
      guard (not (S.member p presentIds))
      t <- textAt "trace_id" r
      pure (t, p)
    groups = Map.fromListWith (<>) [(k, [r]) | r <- V.toList rows, Just k <- [keyOf r]]
    firstText k = fromMaybe "" . asum . map (textAt k)
    synthRow tid pid ks =
      let spans' = mapMaybe (\r -> (,) <$> numAt "start_time_ns" r <*> numAt "duration" r) ks
          startNs = maybe 0 minimum1 $ nonEmpty $ map fst spans'
          endNs = foldr (max . uncurry (+)) startNs spans'
          label = "Upstream span missing \x2014 " <> T.take 8 pid
          -- 'text-textWeak' style matches renderer's WEAK_TEXT_STYLES lookup; italic +
          -- dashed border are applied in log-list.ts, keyed off the synthetic-* id.
          fields :: [(Text, AE.Value)]
          fields =
            [ ("id", AE.String ("synthetic-" <> pid))
            , ("timestamp", AE.String (firstText "timestamp" ks))
            , ("trace_id", AE.String tid)
            , ("span_name", AE.String label)
            , ("duration", AE.Number (fromIntegral (max 0 (endNs - startNs))))
            , ("service", AE.String (firstText "service" ks))
            , ("parent_id", AE.Null)
            , ("start_time_ns", AE.Number (fromIntegral startNs))
            , ("errors", AE.Bool False)
            , ("summary", AE.toJSON (["span_name;text-textWeak\x21d2" <> label] :: [Text]))
            , ("latency_breakdown", AE.String pid)
            , ("kind", AE.String "span")
            ]
       in V.replicate colCount AE.Null V.// [(i, v) | (k, v) <- fields, Just i <- [lookupIdx k]]


rowCountDisplay_ :: Text -> Text -> Text -> Html ()
rowCountDisplay_ suffix countText suffixText =
  div_ [Aria.live_ "polite", Aria.atomic_ "true"] do
    span_ [class_ "text-textStrong", id_ $ "row-count-display" <> dashSuffix] $ toHtml countText
    span_ [class_ "text-textStrong", id_ $ "row-count-suffix" <> dashSuffix] $ toHtml suffixText
  where
    dashSuffix = if T.null suffix then "" else "-" <> suffix


-- | Visual grouping for the sidebar. Each group renders one collapsible section.
data FacetGroup = FGCommon | FGHTTP | FGSeverity | FGResource | FGUserSession | FGDatabase | FGErrors
  deriving stock (Bounded, Enum, Eq, Ord, Show)


facetGroupLabel :: FacetGroup -> Text
facetGroupLabel = \case
  FGCommon -> "Common Filters"
  FGHTTP -> "HTTP"
  FGSeverity -> "Severity"
  FGResource -> "Resource"
  FGUserSession -> "User & Session"
  FGDatabase -> "Database"
  FGErrors -> "Errors & Exceptions"


facetGroupParam :: FacetGroup -> Text
facetGroupParam = toText . toKebab . fromAny . drop 2 . toString . show


parseFacetGroup :: Text -> Maybe FacetGroup
parseFacetGroup value = find ((== value) . facetGroupParam) universe


-- | A facet entry the sidebar can render. @path@ is the canonical KQL field
-- name (the 'FacetData' lookup key and what a user types). Invariant
-- ('prop_facetsAreFast'): @path@ must be a flat-column reference (in
-- 'Pkg.Parser.Expr.flattenedOtelAttributes' or 'topLevelOtelColumns') so
-- click-to-filter compiles to a direct column scan, not a jsonb_path fallback.
data Facet = Facet
  { path :: Text
  , label :: Text
  , group :: FacetGroup
  , color :: Text -> Text
  }


-- | The full facet list. Source order is preserved within each group.
--
-- >>> import qualified Pkg.Parser.Expr as PE
-- >>> import qualified Data.Set as S
-- >>> all (\f -> S.member f.path PE.flattenedOtelAttributes || S.member f.path PE.topLevelOtelColumns) facetDefs
-- True
facetDefs :: [Facet]
facetDefs =
  let nc = const "" -- no fill color
   in -- Common
      [ Facet "resource.service.name" "Service" FGCommon serviceFillColor
      , Facet "name" "Operation Name" FGCommon nc
      , Facet "level" "Log Level" FGCommon levelFillColor
      , Facet "status_code" "Status Code" FGCommon statusFillColorText
      , Facet "kind" "Kind" FGCommon nc
      , Facet "attributes.http.request.method" "HTTP Method" FGCommon methodFillColor
      , Facet "attributes.http.response.status_code" "HTTP Status" FGCommon statusFillColorText
      , Facet "attributes.db.operation.name" "DB Operation" FGCommon nc
      , -- HTTP
        Facet "attributes.http.request.method_original" "Original Method" FGHTTP methodFillColor
      , Facet "attributes.http.request.resend_count" "Resend Count" FGHTTP nc
      , Facet "attributes.http.request.body.size" "Request Body Size" FGHTTP nc
      , Facet "attributes.url.path" "URL Path" FGHTTP nc
      , Facet "attributes.url.scheme" "URL Scheme" FGHTTP nc
      , Facet "attributes.url.full" "Full URL" FGHTTP nc
      , Facet "attributes.url.fragment" "URL Fragment" FGHTTP nc
      , Facet "attributes.url.query" "URL Query" FGHTTP nc
      , Facet "attributes.user_agent.original" "User Agent" FGHTTP nc
      , -- Severity
        Facet "severity.severity_text" "Severity Text" FGSeverity levelFillColor
      , Facet "severity.severity_number" "Severity Number" FGSeverity nc
      , Facet "status_message" "Status Message" FGSeverity nc
      , -- Resource
        Facet "resource.service.version" "Service Version" FGResource nc
      , Facet "resource.service.instance.id" "Service Instance ID" FGResource nc
      , Facet "resource.service.namespace" "Service Namespace" FGResource nc
      , Facet "resource.telemetry.sdk.language" "SDK Language" FGResource nc
      , Facet "resource.telemetry.sdk.name" "SDK Name" FGResource nc
      , Facet "resource.telemetry.sdk.version" "SDK Version" FGResource nc
      , -- Database
        Facet "attributes.db.system.name" "Database System" FGDatabase nc
      , Facet "attributes.db.collection.name" "Collection Name" FGDatabase nc
      , Facet "attributes.db.namespace" "Database Namespace" FGDatabase nc
      , Facet "attributes.db.operation.batch.size" "Batch Size" FGDatabase nc
      , -- Errors & Exceptions
        Facet "attributes.exception.type" "Exception Type" FGErrors (const "bg-fillError-strong")
      , Facet "attributes.exception.message" "Exception Message" FGErrors nc
      ]
        -- User & Session, derived from 'Telemetry.identityFields' so a field the log-item
        -- detail panel shows is a field you can facet on — the two lists had already drifted
        -- apart once. Restricted to promoted columns because a facet counts distinct values
        -- across the range, which needs a column and not a probe into the Variant blob; the
        -- tenant keys stay filterable through a pill's menu, they just aren't faceted.
        <> [ Facet path label FGUserSession nc
           | (k, label) <- Telemetry.identityFields
           , let path = "attributes." <> k
           , S.member path ParserExpr.flattenedOtelAttributes
           ]


-- | 'facetDefs' bucketed by 'FacetGroup', built once at module load time.
facetsByGroup :: Map.Map FacetGroup [Facet]
facetsByGroup = Map.fromListWith (flip (<>)) [(f.group, [f]) | f <- facetDefs]


-- | Render facet data for Log Explorer sidebar in a compact format.
-- The facet counts are scaled in the upstream summary based on the selected time range.
renderFacets :: FacetSummary -> Html ()
renderFacets facetSummary =
  forM_ (universe :: [FacetGroup]) \facetGroup -> renderFacetGroup (facetGroup == FGCommon) facetGroup facetSummary


-- | A group shell is cheap enough for the initial sidebar. Only Common Filters
-- includes its fields immediately; every closed group fetches and replaces
-- its shell with the server-rendered Lucid fragment on first open.
renderFacetGroup :: Bool -> FacetGroup -> FacetSummary -> Html ()
renderFacetGroup loaded facetGroup facetSummary =
  facetSection_ loaded "facet-section-group" attrs (facetGroupTitle_ facetGroup) body
  where
    attrs =
      if loaded
        then []
        else
          [ hxGet_ url
          , hxTrigger_ "toggle[target.open] once"
          , hxTarget_ "this"
          , hxSwap_ "outerHTML"
          , hxIndicator_ "find .facet-group-loader"
          ]
    body
      | loaded = div_ [class_ "facets-container"] $ renderFacetFields facetGroup facetSummary
      | otherwise = div_ [class_ "facet-group-loader htmx-indicator flex h-8 items-center justify-center"] $ loadingIndicator_ LdXS LdSpinner
    url = "/p/" <> facetSummary.projectId <> "/log_explorer/facets?group=" <> facetGroupParam facetGroup


facetGroupTitle_ :: FacetGroup -> Html ()
facetGroupTitle_ = span_ [class_ "font-medium text-sm"] . toHtml . facetGroupLabel


renderFacetFields :: FacetGroup -> FacetSummary -> Html ()
renderFacetFields facetGroup facetSummary = do
  let (FacetData facetMap) = facetSummary.facetJson
      facets = Map.findWithDefault [] facetGroup facetsByGroup
  -- Checkbox↔query sync lives in web-components/src/main.ts (syncFacetCheckboxes),
  -- wired to `update-query` + `htmx:after:swap` so swapped-in facets re-sync.
  iforM_ facets \idx facet -> do
    let values = HM.lookupDefault [] facet.path facetMap
        open = facetGroup == FGCommon && idx < 5 && not (null values)
        (visibleValues, hiddenValues) = splitAt 5 values
        hiddenCount = length hiddenValues
    facetSection_
      open
      ""
      []
      ( div_ [class_ "flex items-center justify-between gap-2"] do
          span_ [class_ "truncate text-sm font-normal", term "data-tippy-content" facet.path] $ toHtml facet.label
          -- Bubble-halt so the ⋮ popover (and its menu items) don't toggle the <details>.
          div_ [class_ "inline-block", [__|on click halt the event's bubbling|]] do
            button_ ([type_ "button", class_ "cursor-pointer rounded p-1 hover:bg-fillWeak", Aria.label_ "Facet options"] <> popoverTrigger_ (slugify facet.path))
              $ faSprite_ "ellipsis-vertical" "regular" "w-3 h-3"
            ul_ ([class_ "dropdown menu p-2 shadow-sm bg-bgRaised rounded-box w-96 border border-strokeWeak z-50", term "data-field-path" facet.path] <> fieldMenuPanel_ (slugify facet.path))
              $ fieldContextMenuItems_ (StaticField facet.path Nothing) [FCopyField, FDivider, FGroupBy, FViewPatterns, FDivider, FAddColumn]
      )
      $ div_ [class_ "facet-values pl-5 pr-1 mb-1 space-y-1"] do
        if null values
          then div_ [class_ "facet-empty px-1 py-1 text-xs italic text-textWeak"] "no values in window"
          else forM_ visibleValues (renderFacetValue facet)
        when (hiddenCount > 0)
          $ div_ [class_ "facet-tail"]
          $ button_
            [ type_ "button"
            , class_ "facet-more text-textBrand text-xs px-1 py-0.5 cursor-pointer hover:underline"
            , hxGet_ $ "/p/" <> facetSummary.projectId <> "/log_explorer/facets?field=" <> toUriStr facet.path
            , hxTarget_ "closest .facet-tail"
            , hxSwap_ "outerHTML"
            ]
          $ toHtml
          $ "+ More ("
          <> prettyPrintCount hiddenCount
          <> ")"


renderFacetValue :: Facet -> FacetValue -> Html ()
renderFacetValue f (FacetValue val count) =
  facetOption_
    "facet-item max-md:min-h-9"
    []
    ( do
        input_
          [ type_ "checkbox"
          , class_ "checkbox checkbox-xs max-md:checkbox-sm"
          , -- Via queryEditorCall, not the element directly: Monaco is loaded lazily, so on a fresh
            -- page load <query-editor> is still un-upgraded and `.toggleSubQuery` doesn't exist yet.
            [__|on click js(me) window.queryEditorCall('toggleSubQuery', me.dataset.field + ' == "' + me.dataset.value + '"') end|]
          , Aria.label_ (f.path <> " equals " <> val)
          , term "data-tippy-content" (f.path <> " == \"" <> val <> "\"")
          , term "data-field" f.path
          , term "data-value" val
          ]
        let colorClass = f.color val
        unless (T.null colorClass) $ span_ [class_ $ colorClass <> " shrink-0 w-0.5 h-3 rounded-sm"] ""
        span_ [class_ "facet-value truncate text-xs", term "data-tippy-content" val] $ toHtml val
    )
    (span_ [class_ "facet-count shrink-0 text-xs tabular-nums text-textWeak"] $ toHtml $ prettyPrintCount count)


renderFacetTail :: Text -> FacetSummary -> Html ()
renderFacetTail field facetSummary =
  whenJust (L.find ((== field) . (.path)) facetDefs) \facet -> do
    let (FacetData facetMap) = facetSummary.facetJson
        values = drop 5 $ HM.lookupDefault [] field facetMap
        count = prettyPrintCount $ length values
    details_ [class_ "facet-tail group", open_ ""] do
      summary_ [class_ "list-none cursor-pointer text-textBrand text-xs px-1 py-0.5 hover:underline focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong"] do
        span_ [class_ "group-open:hidden"] $ toHtml $ "+ More (" <> count <> ")"
        span_ [class_ "hidden group-open:inline"] $ toHtml $ "− Less (" <> count <> ")"
      div_ [class_ "space-y-1"] $ forM_ values (renderFacetValue facet)


-- | Core result builder shared by apiLogH and queryEvents. When @withChildren@
-- is False, only matched rows are returned — no descendants, no synthesised
-- orphan headers (trace-tree concerns the UI wants but the API/CLI usually doesn't).
-- The time window is re-derived from the returned page's own first/last timestamps,
-- so only @sinceM@ (the relative-range token) is needed here.
buildLogResult :: (DB es, Labeled "timefusion" Hasql :> es, Time.Time :> es) => Bool -> Bool -> Projects.ProjectId -> UTCTime -> Maybe Text -> [Text] -> [Text] -> (V.Vector (V.Vector AE.Value), [Text], Int) -> Eff es LogResult
buildLogResult useTf withChildren pid now sinceM addCols removeCols (requestVecs, colNames, resultCount') = do
  let colIdxMap = listToIndexHashMap colNames
      colOf k v = lookupVecTextByKey v colIdxMap k
      reqLastCreatedAtM = colOf "timestamp" =<< (requestVecs V.!? (V.length requestVecs - 1))
      reqFirstCreatedAtM = colOf "timestamp" =<< (requestVecs V.!? 0)
      alreadyLoadedIds = V.mapMaybe (colOf "id") requestVecs
      (fromDD, toDD, _) = Components.parseTimeRange now (Components.TimePicker sinceM reqLastCreatedAtM reqFirstCreatedAtM)
  childSpansList <-
    if not withChildren || V.length requestVecs > 100
      then pure [] -- Skip expensive child span fetch for large result sets; traces load lazily on detail view
      else do
        let traceIds = V.fromList $ take 50 $ nubOrd $ V.toList $ V.mapMaybe (mfilter (not . T.null) . colOf "trace_id") requestVecs
            -- latency_breakdown is aliased from context___span_id (see Pkg.Parser).
            seedSpanIds = V.mapMaybe (colOf "latency_breakdown") requestVecs
        LogQueries.selectChildSpansAndLogs useTf pid addCols traceIds seedSpanIds (fromDD, toDD) alreadyLoadedIds
  let synthRows = if withChildren then synthesizeOrphanHeaders colIdxMap requestVecs else V.empty
      requestVecsAug = synthRows <> requestVecs
      rawLogsData = requestVecsAug <> V.fromList childSpansList
      cols = nubOrd $ curateCols addCols removeCols colNames
      -- Keyed on the service, which is what the name has always claimed. Keying on
      -- `span_name` made this a per-*operation* palette: two spans in one service got two
      -- colours and the same operation in two services got one, so the latency column's
      -- colour carried no service signal at all — and every row whose operation missed the
      -- palette fell back to grey. Same hash as the trace waterfall and the service map, so
      -- a service looks the same wherever it appears.
      colors = getServiceColors $ V.mapMaybe (colOf "service") rawLogsData
      queryResultCount = V.length requestVecsAug
      (logsData, traces) = buildTraceTree colIdxMap queryResultCount rawLogsData
  pure
    LogResult
      { logsData
      , cols
      , colIdxMap
      , cursor = reqLastCreatedAtM
      , nextUrl = ""
      , resetLogsUrl = ""
      , recentUrl = ""
      , serviceColors = colors
      , queryResultCount
      , count = resultCount'
      , -- Compare only real fetched rows against selectLogTable's overflow sentinel
        -- (resultCount' = limit+1); counting synthesized orphan headers would inflate
        -- the page past the sentinel and stall load-more.
        hasMore = V.length requestVecs < resultCount'
      , traces
      , error = Nothing
      }


-- | Standalone query function for the v1 API events endpoint. Returns a
-- JSON-shaped 400 (@{"error": {code, message, field?, suggestion?, details?}}@)
-- for parse/query errors instead of raw Hasql/SQL.
queryEvents :: (DB es, ELog.Log :> es, Effectful.Reader.Static.Reader AuthContext :> es, Error Servant.ServerError :> es, Labeled "timefusion" Hasql :> es, Time.Time :> es, Tracing :> es) => Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Bool -> Maybe Bool -> Eff es LogResult
queryEvents pid queryM sinceM fromM toM sourceM limitM withChildrenM includeAttributesM = do
  now <- Time.currentTime
  let queryInput = fromMaybe "" queryM
  queryAST <- case parseQueryToAST queryInput of
    Left err -> throwError $ kqlError400 "invalid_query" ("Invalid query: " <> err) Nothing Nothing Nothing
    Right ast -> pure ast
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
      -- Apply the API `limit` as the query row-limit (when KQL has none) so selectLogTable
      -- returns exactly the page; a post-hoc `V.take` would miscount hasMore since
      -- trace-tree rows include synth headers + descendants.
      hasKqlLimit = any (\case TakeCommand{} -> True; _ -> False) queryAST
      queryAST' = if hasKqlLimit then queryAST else queryAST <> [TakeCommand (min defaultQueryLimit (fromMaybe 100 limitM))]
  enableTfReads <- (.env.enableTimefusionReads) <$> Effectful.Reader.Static.ask @AuthContext
  result <- LogQueries.selectLogTable enableTfReads pid queryAST' (toQText queryAST') Nothing (fromD, toD) ["attributes" | fromMaybe False includeAttributesM] (parseMaybe pSource =<< sourceM) Nothing Nothing
  case result of
    Left err -> throwError $ translateQueryError err
    -- Default to exact-match (no trace expansion); UI passes True via apiLogH.
    Right r -> buildLogResult enableTfReads (fromMaybe False withChildrenM) pid now sinceM [] [] r


-- | Translate the raw exception string from 'LogQueries.selectLogTable' into a
-- structured 400: a column-not-exist error becomes @unknown_field@ with a fix
-- suggestion; everything else is a generic @query_failed@.
translateQueryError :: Text -> Servant.ServerError
translateQueryError raw =
  let
    -- Pluck the first line so the user doesn't see raw Haskell record syntax.
    firstLine = T.strip $ T.takeWhile (/= '\n') raw
    summary
      | T.null firstLine = "Query execution failed"
      | T.length firstLine > 240 = T.take 237 firstLine <> "…"
      | otherwise = firstLine
   in
    maybe
      (kqlError400 "query_failed" summary Nothing Nothing (Just raw))
      ( \col ->
          kqlError400
            "unknown_field"
            ("Unknown field \"" <> col <> "\"")
            (Just col)
            (Just $ "wrap as 'body has \"" <> col <> "\"' for full-text, or use 'field == value' for equality")
            (Just raw)
      )
      (extractMissingColumn raw)


-- | Build a 400 with a JSON-shaped body the CLI's 'renderAPIError' decodes.
-- The @details@ slot carries the raw SQL/Hasql text; only included when set.
kqlError400 :: Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Servant.ServerError
kqlError400 code msg fieldM suggestionM detailsM =
  Servant.err400
    { Servant.errBody = AE.encode $ AE.object ["error" AE..= errBody]
    , Servant.errHeaders = [("Content-Type", "application/json")]
    }
  where
    errBody =
      AE.object
        $ catMaybes
          [ Just ("code" AE..= code)
          , Just ("message" AE..= msg)
          , ("field" AE..=) <$> fieldM
          , ("suggestion" AE..=) <$> suggestionM
          , ("details" AE..=) <$> detailsM
          ]


-- | Pull a missing column name from the underlying SQL error. Handles both
-- Postgres (@column "X" does not exist@) and TimeFusion (@No field named X@)
-- shapes; 'Nothing' if neither matches, so unrelated errors aren't mislabeled.
extractMissingColumn :: Text -> Maybe Text
extractMissingColumn t = tfMatch <|> pgMatch
  where
    lower = T.toLower t
    -- Match case-insensitively but re-slice from the original-case text so the
    -- column name keeps its casing.
    after needle =
      let (pre, hit) = T.breakOn needle lower
       in T.drop (T.length pre + T.length needle) t <$ guard (not (T.null hit))
    tfMatch = after "no field named " >>= guarded (not . T.null) . T.strip . T.takeWhile (`notElem` ['.', ' ', '\n', '"', '`'])
    pgMatch = do
      guard $ "does not exist" `T.isInfixOf` lower
      rest <- after "column "
      guarded (not . T.null) $ maybe (T.takeWhile (`notElem` [' ', ',']) rest) (T.takeWhile (/= '"')) (T.stripPrefix "\"" rest)


-- | Log Explorer page shell. Renders chrome only (query box, facets, widgets,
-- session header). Log rows are fetched separately by the log-list web component
-- from 'logExplorerDataH' and the sibling patterns/sessions/query-library/alert-form endpoints.
apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM' cols' sinceM fromM toM sourceM targetSpansM targetEventM showTraceM vizTypeM alertM pTargetM = do
  let source = fromMaybe "spans" sourceM
  (sess, project, bw) <- mkPageCtx pid
  let queryInput = maybeToMonoid queryM'
      parseError msg = addTriggerEvent "showParseError" (AE.toJSON msg) >> addErrorToast "Error Parsing Query" (Just msg) $> ([], Just msg)
  (queryAST, parseErrorMsg) <- case parseQueryToAST queryInput of
    Left err -> parseError err
    Right ast
      | not (T.null (T.strip queryInput)) && null ast -> parseError "Invalid query syntax"
      | otherwise -> pure (ast, Nothing)

  -- Fire-and-forget: mark onboarding done + log this query into history.
  recordExploration pid sess.persistentSession.userId project.onboardingStepsCompleted queryAST

  now <- Time.currentTime
  let (_, _, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  authCtx <- Effectful.Reader.Static.ask @AuthContext

  -- An alert ID pre-fills the query box and selects the alert's viz type.
  alertDM <- lookupAlert alertM
  let effectiveVizType = vizTypeM <|> ((.visualizationType) <$> alertDM)

  -- Non-common facets and the Query Library lazy-load through their own HTMX endpoints.
  freeTierStatusE <- tryAny $ checkFreeTierStatus pid project.paymentPlan

  -- The initial HTMX facet request used to enqueue this job. Common facets now render
  -- with the page, so preserve the missing-summary recovery without restoring that
  -- client-side round trip.
  when (isNothing bw.facetSummaryM) $ enqueueFacetsJob authCtx pid now

  whenLeft_ (void freeTierStatusE) (Log.logAttention "Log explorer freeTierStatus failed" . show @Text)

  let freeTierStatus = fromRight def freeTierStatusE

  -- Preload the data fetch before the log-list web component boots. Point it at the endpoint
  -- matching the active viz — otherwise the sessions/patterns page fires a wasted
  -- logs query that its transport never consumes (and which contends with the
  -- real aggregate fetch).
  let dataEndpoint = case effectiveVizType of
        Just "sessions" -> LogQueries.Sessions
        Just "patterns" -> LogQueries.Patterns
        _ -> LogQueries.Data
      preloadUrl = T.replace "\"" "%22" $ LogQueries.logExplorerUrlPath pid dataEndpoint queryM' cols' Nothing sinceM fromM toM Nothing sourceM False

  let stampPng base = do
        url <- Widget.widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid base sinceM fromM toM
        pure $ if T.null url then base else base{Widget.pngUrl = Just url}
  chartWidget <- stampPng (logChartWidget pid)
  latencyWidget <- stampPng (logLatencyWidget pid)

  let bwconf =
        bw
          { pageTitle = "Explorer"
          , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/api-log-explorer/"
          , freeTierStatus = freeTierStatus
          , headContent = Nothing
          , pageActions = Just $ logExplorerActions_ currentRange
          , navTabs = Just $ explorerNavTabs_ pid "Events"
          , needsTagify = False
          }

  let page =
        ApiLogsPageData
          { pid
          , resultCount = 0
          , currentRange
          , query = queryM'
          , source
          , targetSpans = targetSpansM
          , targetEvent = targetEventM
          , showTrace = showTraceM
          , vizType = effectiveVizType
          , alert = alertDM
          , targetPattern = pTargetM
          , chartWidget
          , latencyWidget
          , queryResultCount = 0
          , parseError = parseErrorMsg
          , preloadUrl
          , facetSummary = bw.facetSummaryM
          }
  addRespHeaders $ LogPage $ PageCtx bwconf page


-- | Fire-and-forget on page load: mark the @explored_logs@ onboarding step done
-- and record the query in the user's history.
recordExploration :: Projects.ProjectId -> Projects.UserId -> V.Vector Text -> [Section] -> ATAuthCtx ()
recordExploration pid uid stepsDone queryAST = do
  unless (V.elem "explored_logs" stepsDone)
    $ void
    $ Hasql.interpExecute [HI.sql| UPDATE projects.projects SET onboarding_steps_completed = array_append(onboarding_steps_completed, 'explored_logs') WHERE id = #{pid} AND NOT ('explored_logs' = ANY(onboarding_steps_completed)) |]
  Projects.queryLibInsert Projects.QLTHistory pid uid (toQText queryAST) queryAST Nothing


-- | Log Explorer header controls: live-stream toggle, time picker, refresh.
logExplorerActions_ :: Maybe (Text, Text) -> Html ()
logExplorerActions_ currentRange = div_ [class_ "flex gap-2 max-md:gap-1 items-center"] do
  -- sr-only, not hidden: `display:none` drops the checkbox out of the tab order, so the
  -- label could advertise role=switch while being unreachable by keyboard. The ring on
  -- the label is what makes that focus visible, since the input itself has no box.
  label_ [class_ "cursor-pointer border border-strokeWeak rounded-lg flex shadow-xs has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-strokeBrand-strong", role_ "switch", Aria.label_ "Stream live data", [__|on change from #streamLiveData set @aria-checked to #streamLiveData.checked|], term "aria-checked" "false"] do
    input_ [type_ "checkbox", id_ "streamLiveData", class_ "sr-only"]
    span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-2 items-center", data_ "tippy-content" "Pause live stream"] $ faSprite_ "pause" "solid" "h-4 w-4 text-iconNeutral"
    span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex py-1 px-2 items-center", data_ "tippy-content" "Stream live data"] $ faSprite_ "play" "regular" "h-4 w-4 text-iconNeutral"
  Components.timepicker_ (Just "log_explorer_form") currentRange Nothing
  Components.refreshButton_


-- | Shared prologue for the log-data endpoints: auth-gate the request, grab the
-- app config + clock, and resolve the time range once.
-- The sticky deployment-environment selection travels with the session (it is read from
-- the @env@ cookie at auth time), so every data endpoint that already resolves the session
-- gets it here rather than declaring a query parameter it would have to be handed on every
-- link in the app.
logDataEnv :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (AuthContext, UTCTime, Maybe UTCTime, Maybe UTCTime, Maybe Text)
logDataEnv pid sinceM fromM toM = do
  (sess, _) <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  pure (authCtx, now, fromD, toD, sess.environment)


-- | Log-row data endpoint. The log-list web component fetches this; the shell
-- (apiLogH) renders only chrome. Returns the trace-tree-expanded 'LogResult'.
logExplorerDataH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe PageDirection -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogResult)
logExplorerDataH pid queryM' cols' cursorM' directionM sinceM fromM toM sourceM targetSpansM = withSpan_ "log-explorer.data" [] do
  (authCtx, now, fromD, toD, envM) <- logDataEnv pid sinceM fromM toM
  -- `cols` is a delta over server defaults: bare tokens add columns, `-`-prefixed tokens hide defaults.
  let (removeToks, addCols) = L.partition ("-" `T.isPrefixOf`) $ filter (not . T.null) $ T.splitOn "," (fromMaybe "" cols')
      removeCols = map (T.drop 1) removeToks
      cursor = PageCursor (fromMaybe PageOlder directionM) <$> cursorM'
      emptyTable = (V.empty, ["timestamp", "summary", "duration"], 0)
  -- Carry a sanitized failure message alongside the (empty) table so the client
  -- can show an error state instead of a misleading "no events" list.
  (errM, tableData) <- case parseQueryToAST (maybeToMonoid queryM') of
    Left err -> Log.logInfo "Log explorer data: rejected invalid KQL query" err $> (Just err, emptyTable)
    Right queryAST -> do
      resultE <-
        LogQueries.selectLogTable authCtx.env.enableTimefusionReads pid queryAST (toQText queryAST) cursor (fromD, toD) addCols (parseMaybe pSource =<< sourceM) targetSpansM envM
      case resultE of
        Left err -> Log.logAttention "log-explorer.data query failed" (AE.object ["project_id" AE..= pid.toText, "source" AE..= fromMaybe "spans" sourceM, "error" AE..= err]) $> (Just (sanitizeBackendError err), emptyTable)
        Right t -> pure (Nothing, t)
  -- UI always wants the trace-tree context; the API/CLI defaults off.
  lr <- buildLogResult authCtx.env.enableTimefusionReads True pid now sinceM addCols removeCols tableData
  let lastFM = lr.cursor >>= (iso8601ParseM . toString) <&> toText . iso8601Show . addUTCTime (-0.001)
  addRespHeaders
    (lr :: LogResult)
      { error = errM
      , nextUrl = LogQueries.logExplorerUrlPath pid LogQueries.Data queryM' cols' lastFM sinceM fromM toM (Just "loadmore") sourceM False
      , resetLogsUrl = LogQueries.logExplorerUrlPath pid LogQueries.Data queryM' cols' Nothing Nothing Nothing Nothing Nothing sourceM False
      , recentUrl = LogQueries.logExplorerUrlPath pid LogQueries.Data queryM' cols' Nothing sinceM fromM toM (Just "loadmore") sourceM True
      }


-- | Lazy facet fragments for collapsed groups and per-field overflow values.
-- Common facets render in the initial page from the summary already read by
-- 'mkPageCtx'; this endpoint keeps the rest off the critical rendering path.
logExplorerFacetsH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
logExplorerFacetsH pid fieldM groupM = do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  facetSummary <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" now now
  when (isNothing facetSummary) $ enqueueFacetsJob authCtx pid now
  addRespHeaders
    $ maybe
      (div_ [class_ "px-1 py-4 text-xs italic text-textWeak"] "Filters are still being built for this project.")
      ( \summary -> case (fieldM, groupM >>= parseFacetGroup) of
          (Just field, _) -> renderFacetTail field summary
          (_, Just facetGroup) -> renderFacetGroup True facetGroup summary
          _ -> renderFacets summary
      )
      facetSummary


-- | Structured verdict for the query editor. It underlines @column@..@width@ and
-- prints @message@, so the squiggle comes from the same parser that gates
-- execution instead of a regex approximation of the grammar.
data QueryValidation = QueryValidation
  { valid :: Bool
  , message :: Maybe Text
  , column :: Maybe Int
  , width :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving (ToSchema) via CamelSchema QueryValidation
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] QueryValidation


-- | Validate a KQL query without running it.
logExplorerValidateH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders QueryValidation)
logExplorerValidateH pid queryM sourceM = do
  _ <- Projects.sessionAndProject pid
  -- Same source the query will run under, so the squiggle and the execution agree about
  -- which table's columns exist. Without it a metrics query is marked invalid on a page
  -- that runs it happily.
  addRespHeaders
    $ either
      (\e -> QueryValidation{valid = False, message = Just e.message, column = Just e.column, width = Just e.width})
      (const QueryValidation{valid = True, message = Nothing, column = Nothing, width = Nothing})
      (parseQueryDiagnosed (parseMaybe pSource =<< sourceM) (maybeToMonoid queryM))


-- | Enriched span schema for the query editor, served from a dedicated endpoint
-- so the ~365KB payload isn't inlined into (and re-encoded on) every page render.
-- The client caches it in a window promise for the session. Facet enrichment is
-- from in-memory summary state (cheap); the time range is ignored by getFacetSummary.
logExplorerSchemaH :: Projects.ProjectId -> ATAuthCtx (RespHeaders AE.Value)
logExplorerSchemaH pid = do
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime
  facetsM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" now now
  -- Derived, not the hand-coded schema: the query editor validates field names
  -- against this response, so it has to advertise everything the parser accepts
  -- (live columns + aliases) or working queries get "Unknown field" squiggles.
  let schema = Schema.deriveSchema ParserExpr.flattenedOtelAttributes
  addRespHeaders $ maybe (AE.toJSON schema) (enrichSchemaWithFacets schema . (.facetJson)) facetsM


-- | Patterns visualization data endpoint (aggregate log patterns as JSON).
logPatternsH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders PatternsView)
logPatternsH pid queryM' sinceM fromM toM sourceM pTargetM skipM = do
  (authCtx, now, fromD, toD, envM) <- logDataEnv pid sinceM fromM toM
  -- Start (epoch seconds) of the earliest of the 24 hourly volume slots, so the
  -- client can map bar i to the clock hour @baseHourEpoch + i*3600@ (see buildHourlyBuckets).
  let baseHourEpoch = (floor (utcTimeToPOSIXSeconds now) `div` 3600 - 23) * 3600 :: Int
  case parseQueryToAST (maybeToMonoid queryM') of
    Left err -> Log.logInfo "Log explorer patterns: rejected invalid KQL query" err >> addRespHeaders (PatternsView 0 V.empty False 0)
    Right queryAST -> do
      (total, rows) <- LogQueries.fetchLogPatterns authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) (parseMaybe pSource =<< sourceM) pTargetM envM (fromMaybe 0 skipM)
      addRespHeaders $ PatternsView total (V.fromList rows) (fromMaybe 0 skipM == 0) baseHourEpoch


-- | Sessions visualization data endpoint (aggregate sessions as JSON).
logSessionsH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> ATAuthCtx (RespHeaders SessionsView)
logSessionsH pid queryM' sinceM fromM toM skipM sortByM = do
  (authCtx, _, fromD, toD, _) <- logDataEnv pid sinceM fromM toM
  case parseQueryToAST (maybeToMonoid queryM') of
    Left err -> Log.logInfo "Log explorer sessions: rejected invalid KQL query" err >> addRespHeaders (SessionsView 0 V.empty Nothing)
    Right queryAST -> do
      let skip = fromMaybe 0 skipM
      (summ, total, rows) <- LogQueries.fetchSessions authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) sortByM skip
      -- Summary only rides the first page; later load-more pages don't need it.
      addRespHeaders $ SessionsView total (V.fromList rows) (guard (skip == 0) $> summ)


-- | Lazily-loaded alert configuration form (HTMX partial). Kept off the shell's
-- hot path — the shell no longer forks a teams query or renders this every load.
alertFormH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
alertFormH pid alertM = do
  (_, project) <- Projects.sessionAndProject pid
  alertDM <- lookupAlert alertM
  teams <- V.fromList <$> ManageMembers.getTeams pid
  addRespHeaders $ alertConfigurationForm_ project alertDM teams


-- | Queue facet generation for a project whose summary hasn't been built yet.
enqueueFacetsJob :: AuthContext -> Projects.ProjectId -> UTCTime -> ATAuthCtx ()
enqueueFacetsJob authCtx pid now =
  liftIO $ withResource authCtx.jobsPool \conn ->
    void $ createJob conn "background_jobs" $ BackgroundJobs.GenerateOtelFacetsBatch (V.singleton pid) now


-- | Resolve an @?alert=<uuid>@ query param to its monitor, if it parses and exists.
lookupAlert :: DB es => Maybe Text -> Eff es (Maybe Monitors.QueryMonitor)
lookupAlert = maybe (pure Nothing) (Monitors.queryMonitorById . Monitors.QueryMonitorId) . (>>= UUID.fromText)


-- Widget definitions for log explorer charts
logChartWidget :: Projects.ProjectId -> Widget.Widget
logChartWidget pid =
  (def :: Widget.Widget)
    { Widget.id = Just "log-explorer-all-traces"
    , Widget.wType = WTTimeseries
    , Widget.query = Just "summarize count(*) by bin_auto(timestamp), coalesce(status_code, level)"
    , Widget.unit = Just "rows"
    , Widget.title = Just "All traces"
    , Widget.legendPosition = Just "top-right"
    , Widget.legendSize = Just "xs"
    , Widget._projectId = Just pid
    , Widget.standalone = Just True
    , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
    , Widget.allowZoom = Just True
    , Widget.showMarkArea = Just True
    , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
    }


logLatencyWidget :: Projects.ProjectId -> Widget.Widget
logLatencyWidget pid =
  (def :: Widget.Widget)
    { Widget.wType = WTTimeseriesLine
    , Widget.standalone = Just True
    , Widget.title = Just "Latency percentiles"
    , Widget.hideSubtitle = Just True
    , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
    , Widget.summarizeBy = Just Widget.SBMax
    , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
    , Widget.query = Just "duration != null | summarize percentiles(duration, 50, 75, 90, 95) by bin_auto(timestamp)"
    , Widget.unit = Just "ns"
    , Widget.legendPosition = Just "top-right"
    , Widget.legendSize = Just "xs"
    , Widget._projectId = Just pid
    }


-- | One-decimal percent formatter.
--
-- >>> import "monoscope" Pages.LogExplorer.Log qualified as LL
-- >>> LL.fmtPct1 0
-- "0.0%"
-- >>> LL.fmtPct1 5.24
-- "5.2%"
-- >>> LL.fmtPct1 100
-- "100.0%"
-- >>> LL.fmtPct1 (-1.25)
-- "-1.2%"
fmtPct1 :: Double -> Text
fmtPct1 x = toText (showFFloat (Just 1) x "") <> "%"


-- | Shimmer placeholder mirroring 'sessionsHeader_' (6-KPI grid + over-time bar
-- card) so the summary region keeps its height during the sessions-viz swap.
-- NB: the #page-summary-region wrapper classes are repeated verbatim at every
-- use site (here, 'chartSummarySkeleton_', the two headers, 'apiLogsPage') —
-- Tailwind's scanner and locality both want them written on the element.
sessionsSummarySkeleton_ :: Html ()
sessionsSummarySkeleton_ =
  div_ [class_ "mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full flex flex-col gap-2", role_ "status", Aria.label_ "Loading session summary"] do
    div_ [class_ "grid grid-cols-6 max-md:grid-cols-3 gap-2"]
      $ replicateM_ 6
      $ div_ [class_ "surface-raised rounded-2xl px-3 py-2 flex flex-col gap-1"] do
        div_ [class_ "h-3 w-16 rounded skeleton-shimmer"] ""
        div_ [class_ "h-6 w-20 rounded skeleton-shimmer"] ""
        div_ [class_ "h-3 w-14 rounded skeleton-shimmer"] ""
    div_ [class_ "surface-raised rounded-2xl px-3 py-2"] do
      div_ [class_ "h-3 w-28 rounded skeleton-shimmer mb-2"] ""
      div_ [class_ "h-12 w-full rounded skeleton-shimmer"] ""


-- | Shimmer placeholder mirroring the chart+latency widget strip (the non-sessions
-- summary region) so its height is preserved during the swap back from sessions.
chartSummarySkeleton_ :: Html ()
chartSummarySkeleton_ =
  div_ [class_ "timeline flex flex-row gap-4 mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full min-h-36 max-md:min-h-28 aspect-[10/1] max-md:aspect-auto max-md:flex-col", role_ "status", Aria.label_ "Loading chart"] do
    div_ [class_ "flex-[3] min-w-0 rounded-2xl skeleton-shimmer"] ""
    div_ [class_ "flex-1 min-w-0 max-md:hidden rounded-2xl skeleton-shimmer"] ""


-- | KPI card shared by the sessions/patterns summary headers.
kpiCard_ :: (Text, Text, Maybe Text) -> Html ()
kpiCard_ (label, value, subM) = div_ [class_ "surface-raised rounded-2xl px-3 py-2 flex flex-col gap-0.5 min-w-0"] do
  span_ [class_ "text-xs text-textWeak truncate"] $ toHtml label
  strong_ [class_ "text-textStrong text-xl font-bold tabular-nums leading-tight truncate"] $ toHtml value
  whenJust subM (span_ [class_ "text-xs text-textWeak tabular-nums truncate"] . toHtml)


-- | Percent-of-tallest-bar normalizer for the over-time charts (0 when empty).
barNorm :: [Int] -> [Int] -> Int -> Double
barNorm clean err = \n -> if maxBar <= 0 then 0 else fromIntegral n / fromIntegral maxBar * 100
  where
    maxBar = foldl' max 0 (zipWith (+) clean err)


-- | The stacked clean/errored rects inside one over-time bar.
summaryBarRects_ :: (Int -> Double) -> Int -> Int -> Html ()
summaryBarRects_ norm c e = do
  let bar cls n = when (n > 0) $ div_ [class_ cls, style_ $ "height:" <> T.show (max 4 (norm n)) <> "%"] ""
  bar "w-full rounded-sm bg-fillBrand-strong/70 group-hover/bar:bg-fillBrand-strong transition-colors" c
  bar "w-full rounded-sm bg-fillError-strong" e


-- | Over-time chart card shared by the sessions/patterns headers. Bars carry a
-- @data-bi@ bucket index and a @data-count@ base tooltip; @bucketStartEpoch@ and
-- @bucketWidthSec@ ride on the container. Axis labels are server-rendered
-- @<local-time>@ elements (the element reformats into the viewer's zone);
-- @window.formatSummaryChart@ only fills the per-bar time-range tooltips.
-- @axisM@ is the populated window's (start, end) epoch range.
summaryChartCard_ :: Text -> Maybe Text -> Int -> Int -> Maybe (Int, Int) -> [Html ()] -> Html ()
summaryChartCard_ title noteM bucketStartEpoch bucketWidthSec axisM barEls =
  div_ [class_ "surface-raised rounded-2xl px-3 py-2"] do
    div_ [class_ "flex items-center justify-between mb-1"] do
      span_ [class_ "text-xs text-textWeak"] $ toHtml title
      div_ [class_ "flex gap-3 text-xs text-textWeak"] do
        let swatch cls label = span_ [class_ "flex items-center gap-1"] (span_ [class_ $ "inline-block w-2 h-2 rounded-sm " <> cls] "" >> label)
        swatch "bg-fillBrand-strong/70" "Clean"
        swatch "bg-fillError-strong" "Errored"
    if null barEls
      then div_ [class_ "h-12 flex items-center justify-center text-xs text-textWeak"] "No data in range"
      else do
        div_ ([class_ "flex items-end gap-[2px] h-12", data_ "summary-chart" "", data_ "bucket-start" (T.show bucketStartEpoch), data_ "bucket-width" (T.show bucketWidthSec)] <> maybe [] (\n -> [data_ "note" n]) noteM) $ sequence_ barEls
        whenJust axisM \(axisStart, axisEnd) -> do
          let fmt = if axisEnd - axisStart >= 86400 then "MMM d, HH:mm" else "HH:mm"
              lt = localTimeFmt_ fmt . posixSecondsToUTCTime . fromIntegral
          div_ [class_ "flex justify-between text-2xs text-textWeak mt-1 tabular-nums"] (lt axisStart >> lt axisEnd)


-- | Summary header for the patterns viz — parity with 'sessionsHeader_'. KPIs and
-- the volume chart are derived from the returned page (the per-pattern volume
-- buckets are summed element-wise, split clean/error), so no extra scan is needed.
-- The buckets are the hourly series behind the ~volume column, clamped to the
-- picker but laid out in a fixed 24-slot frame; we trim the all-zero head/tail
-- so the chart shows only the populated window. @baseHourEpoch@ is the start
-- (epoch seconds) of hourly slot 0, so bar @i@ covers @baseHourEpoch + i*3600@.
-- Bars are non-interactive: unlike sessions we don't carry a filter action.
patternsHeader_ :: V.Vector LogQueries.PatternRow -> Int -> Int -> Html ()
patternsHeader_ rowsV totalPatterns baseHourEpoch = do
  let rows = V.toList rowsV
      inRange p = sum p.volume
      shown = length rows
      totalEvents = sum (map inRange rows) :: Int
      errRows = filter (.isError) rows
      errPatterns = length errRows
      errShare = if shown == 0 then 0 else 100 * (fromIntegral errPatterns :: Double) / fromIntegral shown
      services = length $ ordNub $ mapMaybe (.service) rows
      topShare = case rows of
        (p : _) | totalEvents > 0 -> 100 * (fromIntegral (inRange p) :: Double) / fromIntegral totalEvents
        _ -> 0
      nBuckets = foldl' max 0 (map (length . (.volume)) rows)
      pad xs = take nBuckets (xs <> repeat 0)
      sumBk ps = foldl' (zipWith (+)) (replicate nBuckets 0) (map (pad . (.volume)) ps)
      -- Trim leading/trailing empty hourly slots to the populated window so a
      -- short range doesn't render as one bar lost in 23 empty ones.
      rawClean = sumBk (filter (not . (.isError)) rows)
      rawErr = sumBk errRows
      active = [i | (i, t) <- zip [0 :: Int ..] (zipWith (+) rawClean rawErr), t > 0]
      window = viaNonEmpty (\a -> (minimum1 a, maximum1 a)) active
      slice xs = maybe [] (\(lo, hi) -> take (hi - lo + 1) $ drop lo xs) window
      cleanBk = slice rawClean
      errBk = slice rawErr
      -- Bars keep their original hour index so the client maps bar i to the
      -- clock hour @baseHourEpoch + i*3600@.
      bars = zip3 (maybe [] (uncurry enumFromTo) window) cleanBk errBk
      norm = barNorm cleanBk errBk
      barEls =
        [ div_
            [ class_ "flex-1 h-full flex flex-col-reverse gap-[1px] min-w-[2px] group/bar"
            , data_ "bi" (T.show i)
            , data_ "count" (prettyPrintCount (c + e) <> " events \xb7 " <> prettyPrintCount e <> " errored")
            ]
            (summaryBarRects_ norm c e)
        | (i, c, e) <- bars
        ]

      kpis :: [(Text, Text, Maybe Text)]
      kpis =
        [ ("Patterns", prettyPrintCount totalPatterns, Just $ prettyPrintCount shown <> " shown")
        , ("Events", prettyPrintCount totalEvents, Just "in range")
        , ("Error patterns", prettyPrintCount errPatterns, Just $ fmtPct1 errShare <> " of shown")
        , ("Services", prettyPrintCount services, Nothing)
        , ("Noisiest", fmtPct1 topShare, Just "of events")
        ]

  div_ [class_ "mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full flex flex-col gap-2"] do
    div_ [class_ "grid grid-cols-5 max-md:grid-cols-3 gap-2"] $ forM_ kpis kpiCard_
    -- Patterns volume is hourly (from the rollup), so a range under an hour
    -- resolves to a single full-width bar. Rather than hide the trend, the note
    -- rides on the container and the client appends it to the lone bar's tooltip
    -- so a hover explains why it's one bar (see formatSummaryChart).
    summaryChartCard_ "Volume over time" (Just "bucketed hourly \x2014 widen the range for a fuller trend") baseHourEpoch 3600 ((\(lo, hi) -> (baseHourEpoch + lo * 3600, baseHourEpoch + (hi + 1) * 3600)) <$> window) barEls


sessionsHeader_ :: LogQueries.SessionSummary -> Html ()
sessionsHeader_ summ = do
  let total = fromIntegral summ.totalSessions :: Int
      errored = fromIntegral summ.erroredSessions :: Int
      errRate = if total == 0 then 0 else 100 * (fromIntegral errored :: Double) / fromIntegral total
      medDur = toText $ getDurationNSMS (fromIntegral summ.medianDurationNs)
      p95Dur = toText $ getDurationNSMS (fromIntegral summ.p95DurationNs)
      totalEvt = fromIntegral summ.totalEvents :: Int
      bars = zip3 [0 :: Int ..] summ.clean summ.errored
      norm = barNorm summ.clean summ.errored
      bucketFrom i = summ.bucketStartEpoch + i * summ.bucketWidthSec
      axisM = guard (not (null bars)) $> (summ.bucketStartEpoch, summ.bucketStartEpoch + length bars * summ.bucketWidthSec)
      -- The onclick filters the table to the bucket; window.__sessionsBucketFilter
      -- is defined once in queryEditorInitializationCode so this header stays
      -- script-free and can be injected via innerHTML. Axis labels + time-range
      -- tooltips are filled client-side by window.formatSummaryChart from the
      -- data-bucket-start/width on the container (see summaryChartCard_).
      barEls =
        [ button_
            [ class_ "flex-1 h-full flex flex-col-reverse gap-[1px] min-w-[2px] cursor-pointer group/bar"
            , type_ "button"
            , data_ "bi" (T.show i)
            , data_ "count" (prettyPrintCount (c + e) <> " sessions \xb7 " <> prettyPrintCount e <> " errored")
            , onclick_ $ "window.__sessionsBucketFilter(" <> T.show (bucketFrom i) <> "," <> T.show (bucketFrom i + summ.bucketWidthSec) <> ")"
            ]
            (summaryBarRects_ norm c e)
        | (i, c, e) <- bars
        ]

      kpis :: [(Text, Text, Maybe Text)]
      kpis =
        [ ("Sessions", prettyPrintCount total, Just $ prettyPrintCount (max 0 (total - errored)) <> " clean")
        , ("Errored", fmtPct1 errRate, Just $ prettyPrintCount errored <> " sessions")
        , ("Median duration", medDur, Just $ "p95 " <> p95Dur)
        , ("Median events", prettyPrintCount $ fromIntegral summ.medianEvents, Just $ prettyPrintCount totalEvt <> " total")
        , ("Users", prettyPrintCount $ fromIntegral summ.uniqueUsers, Nothing)
        , ("Services", prettyPrintCount $ fromIntegral summ.uniqueServices, Nothing)
        ]

  div_ [class_ "mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full flex flex-col gap-2"] do
    div_ [class_ "grid grid-cols-6 max-md:grid-cols-3 gap-2"] $ forM_ kpis kpiCard_
    summaryChartCard_ "Sessions over time" Nothing summ.bucketStartEpoch summ.bucketWidthSec axisM barEls


newtype LogsGet = LogPage (PageCtx ApiLogsPageData)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogPage _) = AE.object ["error" AE..= True]


-- | JSON payload for the patterns visualization endpoint. The 'Bool' flags the
-- first page (skip=0); when set, the rendered summary header rides along as
-- @summaryHtml@ so the client injects #page-summary-region without a second scan
-- (mirrors 'SessionsView').
data PatternsView = PatternsView Int (V.Vector LogQueries.PatternRow) Bool Int


instance AE.ToJSON PatternsView where
  toJSON (PatternsView totalPatterns patterns isFirstPage baseHourEpoch) =
    aggregateEnvelope rows cols allCols total
      $ ["totalPatterns" AE..= totalPatterns]
      <> ["summaryHtml" AE..= Lucid.renderText (patternsHeader_ patterns totalPatterns baseHourEpoch) | isFirstPage]
    where
      -- No level ("status") column: it's null for span-based patterns (a dead
      -- all-"-" column), error patterns already carry an inline badge, and the
      -- sidebar Log Level facet covers filtering. Level stays in allCols so
      -- colIdxMap indices (esp. is_error) and the error derivation are intact.
      cols = ["id", "pattern_count", "volume", "service", "summary"] :: [Text]
      -- pattern_hash carries the comma-joined pat: tag hashes (see PatternRow.hashes);
      -- the client feeds it to the inline-expand endpoint as the tag-match key.
      allCols = ["id", "pattern_count", "volume", "level", "service", "summary", "merged_count", "is_error", "pattern_hash"] :: [Text]
      patternToSummary pat
        | "\x1E" `T.isInfixOf` pat = AE.toJSON (T.splitOn "\x1E" pat)
        | otherwise = AE.toJSON (splitSummaryElements pat)
      -- Group words into summary elements: each word containing ⇒ starts a new
      -- element; subsequent plain words are part of the preceding element's value.
      splitSummaryElements :: Text -> [Text]
      splitSummaryElements = map unwords . L.groupBy (\_ w -> not ("⇒" `T.isInfixOf` w)) . words
      rowOf p = AE.Array $ V.fromList [AE.Null, AE.toJSON p.count, AE.toJSON p.volume, AE.toJSON p.level, AE.toJSON p.service, patternToSummary p.logPattern, AE.toJSON p.mergedCount, AE.toJSON p.isError, AE.toJSON (T.intercalate "," p.hashes)]
      rows = V.map rowOf patterns
      total = V.foldl' (\acc p -> acc + p.count) 0 patterns


-- | JSON payload for the sessions visualization endpoint. Reuses the logs
-- column layout so the same rendering code applies; session-specific info is
-- packed into the summary column as badge elements. The optional
-- 'LogQueries.SessionSummary' is present only on the first page (skip=0); its
-- rendered header HTML rides along as @summaryHtml@ so the client can inject
-- #page-summary-region without a second scan/request.
data SessionsView = SessionsView Int (V.Vector LogQueries.SessionRow) (Maybe LogQueries.SessionSummary)


instance AE.ToJSON SessionsView where
  toJSON (SessionsView totalSessions sessions summaryM) =
    aggregateEnvelope rows cols allCols total
      $ ["totalSessions" AE..= totalSessions]
      <> maybe [] (\summ -> ["summaryHtml" AE..= Lucid.renderText (sessionsHeader_ summ)]) summaryM
    where
      cols = ["id", "timestamp", "service", "summary", "latency_breakdown"] :: [Text]
      allCols = ["id", "timestamp", "trace_id", "span_name", "duration", "service", "parent_id", "start_time_ns", "errors", "summary", "latency_breakdown", "kind", "event_count"] :: [Text]
      -- `key;style⇒value` badge contract (see renderSessionSummary/parseSummaryElement in log-list.ts).
      tag key style val = key <> ";" <> style <> "\x21d2" <> val
      field key = tag key ""
      -- Stack-trace errors: keep only the first line, capped so one huge exception can't blow out the row.
      clipError = (\t -> if T.length t > 120 then T.take 119 t <> "\x2026" else t) . fromMaybe "" . viaNonEmpty head . lines . T.strip
      -- Positions match allCols; unused slots (span_name/parent_id/kind/latency_breakdown/id) are inert placeholders.
      rowOf s =
        AE.Array
          . V.fromList
          $ [ AE.Null
            , AE.toJSON s.firstSeen
            , AE.toJSON s.sessionId -- trace_id, used as expand key
            , AE.String ""
            , AE.toJSON s.durationNs
            , AE.toJSON (unwords $ V.toList s.services)
            , AE.String ""
            , AE.Number 0
            , AE.toJSON s.errorCount
            , AE.toJSON summaryParts
            , AE.Null
            , AE.String ""
            , AE.toJSON s.traceCount -- event_count, drives the [+N] children badge
            ]
        where
          -- Full session id (not truncated): the client feeds it into /replay_session/{id}, a Servant UUID capture.
          -- The tag is what renders the Replay action, so it's emitted only for
          -- sessions that actually have a recording.
          summaryParts =
            catMaybes
              [ tag "session" "right-neutral" s.sessionId <$ guard s.hasReplay
              , Just $ field "user" (LogQueries.sessionUserDisplay s.userEmail s.userName s.userId)
              , field "url" <$> mfilter (not . T.null) s.landingUrl
              , field "device" <$> mfilter (not . T.null) s.userAgent
              , field "events" (show s.eventCount) <$ guard (s.eventCount > 0)
              , field "errors" (show s.errorCount) <$ guard (s.errorCount > 0)
              , field "error" . clipError <$> mfilter (not . T.null) s.firstError
              , Just $ field "duration" (toText $ getDurationNSMS (fromIntegral s.durationNs))
              ]
      rows = V.map rowOf sessions
      total = V.foldl' (\acc s -> acc + s.eventCount) 0 sessions


-- | Shared JSON envelope for aggregate visualizations (patterns, sessions).
aggregateEnvelope :: V.Vector AE.Value -> [Text] -> [Text] -> Int64 -> [AET.Pair] -> AE.Value
aggregateEnvelope rows cols allCols total extra =
  AE.object
    $ [ "logsData" AE..= rows
      , "cols" AE..= cols
      , "colIdxMap" AE..= HM.fromList (zip allCols [0 :: Int ..])
      , "count" AE..= total
      , "hasMore" AE..= (V.length rows >= LogQueries.aggregatePageSize)
      , "queryResultCount" AE..= V.length rows
      , "serviceColors" AE..= AE.object []
      , "traces" AE..= ([] :: [AE.Value])
      ]
    <> extra


-- | Render context for the Log Explorer page shell — chrome only; rows/aggregates
-- are fetched separately (see 'logExplorerDataH' and the patterns/sessions endpoints).
data ApiLogsPageData = ApiLogsPageData
  { pid :: Projects.ProjectId
  , resultCount :: Int
  , currentRange :: Maybe (Text, Text)
  , query :: Maybe Text
  , source :: Text
  , targetSpans :: Maybe Text
  , targetEvent :: Maybe Text
  , showTrace :: Maybe Text
  , vizType :: Maybe Text
  , alert :: Maybe Monitors.QueryMonitor
  , targetPattern :: Maybe Text
  , chartWidget :: Widget.Widget
  , latencyWidget :: Widget.Widget
  , queryResultCount :: Int
  , parseError :: Maybe Text
  , preloadUrl :: Text
  , facetSummary :: Maybe FacetSummary
  }


data LogResult = LogResult
  { logsData :: V.Vector (V.Vector AE.Value)
  , cols :: [Text]
  , colIdxMap :: HM.HashMap Text Int
  , cursor :: Maybe Text
  , nextUrl, resetLogsUrl, recentUrl :: Text
  , serviceColors :: HM.HashMap Text Text
  , queryResultCount, count :: Int
  , hasMore :: Bool
  , traces :: [TraceTreeEntry]
  , error :: Maybe Text
  -- ^ Sanitized backend-failure message. When set, the web client renders an
  -- error state (inline on first load, toast on refresh) instead of the
  -- misleading empty "no events" list. Raw detail stays in the OTEL span + log.
  }
  deriving stock (Generic)
  deriving (ToSchema) via CamelSchema LogResult
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] LogResult


virtualTable :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Html ()
virtualTable pid initialFetchUrl modeM = do
  termRaw
    "log-list"
    ( [ id_ "resultTable"
      , class_ "w-full shrink-1 flex flex-col h-full min-w-0 rr-block"
      , term "windowTarget" "logList"
      , term "projectId" pid.toText
      ]
        <> [term "initialFetchUrl" u | u <- maybeToList initialFetchUrl]
        <> [term "mode" m | m <- maybeToList modeM]
    )
    ("" :: Text)


-- | Inner div that lazily HTMX-loads @url@ into @#target@ once @trigger@ fires.
-- Shared by the details and alert-form side panels; @extra@ carries per-panel bits.
-- Deliberately innerHTML, not morph. Measured: the swap is ~7ms of a ~200ms click, so
-- morph optimises 4% of the interaction — and idiomorph mutates nodes in place, so
-- hyperscript never processes `install FieldMenuDelegate` on morphed-in content and the
-- field context menu silently stops opening. Revisit only with an explicit
-- `_hyperscript.processNode` on htmx:after:swap (see the LogItemMenuable behavior, which
-- already has to do this by hand).
lazyLoad_ :: Text -> Text -> Text -> [Attribute] -> Html ()
lazyLoad_ target url trigger extra =
  div_ ([hxGet_ url, hxTarget_ ("#" <> target), hxSwap_ "innerHTML", hxTrigger_ trigger, term "hx-sync" "this:replace"] <> extra) pass


traceLoadingSkeleton_ :: Html ()
traceLoadingSkeleton_ =
  div_ [class_ "w-full h-full p-2 flex flex-col gap-4", role_ "status", Aria.live_ "polite"] do
    -- Match the loaded trace's header so opening it never shifts the waterfall.
    div_ [class_ "flex flex-wrap justify-between items-center gap-y-1"] do
      div_ [class_ "flex items-center gap-3"] do
        div_ [class_ "h-4 w-12 rounded skeleton-shimmer"] ""
        div_ [class_ "h-5 w-32 rounded skeleton-shimmer"] ""
      div_ [class_ "flex items-center gap-2"] do
        div_ [class_ "h-8 w-56 rounded-lg skeleton-shimmer"] ""
        div_ [class_ "h-7 w-7 rounded skeleton-shimmer"] ""
    div_ [class_ "flex flex-col gap-2 w-full mt-5"] do
      div_ [class_ "flex flex-wrap justify-between gap-y-1 mb-2"] do
        div_ [class_ "flex items-center gap-5"] do
          div_ [class_ "h-4 w-16 rounded skeleton-shimmer"] ""
          div_ [class_ "h-4 w-14 rounded skeleton-shimmer"] ""
          div_ [class_ "h-4 w-14 rounded skeleton-shimmer"] ""
        div_ [class_ "flex items-center gap-3"] do
          div_ [class_ "h-4 w-16 rounded skeleton-shimmer"] ""
          div_ [class_ "h-4 w-16 rounded skeleton-shimmer"] ""
          div_ [class_ "h-4 w-20 rounded skeleton-shimmer"] ""
      div_ [class_ "h-9 w-full rounded-lg border border-strokeWeak bg-fillWeaker flex items-center px-3"] do
        div_ [class_ "h-3 w-32 rounded skeleton-shimmer"] ""
      div_ [class_ "border border-strokeWeak rounded-2xl min-h-[230px] overflow-hidden"] do
        div_ [class_ "flex h-8 border-b border-strokeWeak items-end text-xs"] do
          div_ [class_ "shrink-0 px-2 pb-1 text-textWeak font-medium", style_ "width:35%"] "Service / Span"
          div_ [class_ "grow flex justify-between px-2 pb-1"]
            $ forM_ ([1 .. 7] :: [Int])
            $ \_ -> div_ [class_ "h-3 w-8 rounded skeleton-shimmer"] ""
        div_ [class_ "py-1"]
          $ forM_
            ( [ ("w-28", "ml-0 w-full")
              , ("w-24", "ml-0 w-[78%]")
              , ("w-36", "ml-[70%] w-[21%]")
              , ("w-40", "ml-[72%] w-[4%]")
              , ("w-32", "ml-[72%] w-[2%]")
              , ("w-48", "ml-[73%] w-[8%]")
              , ("w-36", "ml-[73%] w-[3%]")
              , ("w-40", "ml-[73%] w-[2%]")
              , ("w-28", "ml-[74%] w-[5%]")
              ]
                :: [(Text, Text)]
            )
          $ \(labelW, barPos) ->
            div_ [class_ "flex items-center h-7"] do
              div_ [class_ "shrink-0 flex items-center gap-2 px-3", style_ "width:35%"] do
                div_ [class_ "w-2.5 h-2.5 rounded-full skeleton-shimmer"] ""
                div_ [class_ $ "h-3 " <> labelW <> " rounded skeleton-shimmer"] ""
              div_ [class_ "relative grow h-full"]
                $ div_ [class_ $ "h-3 mt-2 rounded-sm skeleton-shimmer " <> barPos] ""
    span_ [class_ "sr-only"] "Loading trace…"
    div_ [id_ "trace-load-error", class_ "hidden mt-4 flex items-center gap-3 text-sm text-textWeak"] do
      "Trace details could not be loaded."
      button_
        [ class_ "text-textBrand font-medium cursor-pointer hover:underline"
        , Aria.label_ "Retry loading trace"
        , [__|on click
        add .hidden to #trace-load-error
        call htmx.ajax('GET', #trace_expanded_view's @data-trace-url, {target: #trace_expanded_view, swap: 'innerHTML'})
      end|]
        ]
        "Retry"


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  -- #main-content is the HTMX swap target; <head> is not included in a boosted
  -- response, so the preload must live here for in-app navigation too. A trace
  -- deep link does not display the log table, so do not contend with its fetch.
  -- Deliberately a script, not <link rel=preload as=fetch>: the log-list fetches
  -- with Accept: application/json + credentials, which won't match the preload
  -- cache's request and would double-fetch.
  when (isNothing page.showTrace)
    $ script_
    $ "window.logDataPromise = fetch(\""
    <> page.preloadUrl
    <> "\", {headers: {Accept: \"application/json\"}, credentials: \"include\"}).then(r => r.json());"
  sectionWrapper_ do
    template_ [id_ "loader-tmp"] $ loadingIndicator_ LdMD LdDots
    template_ [id_ "trace-loading-skeleton"] traceLoadingSkeleton_
    div_ [class_ "fixed z-[9999] hidden right-0 w-max h-max border border-strokeWeak rounded top-32 bg-bgBase shadow-2xl", id_ "sessionPlayerWrapper"] do
      termRaw "session-replay" [id_ "sessionReplay", class_ "shrink-1 flex flex-col", term "projectId" page.pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)
    queryControlsSection
    facetsAndLogListSection
  where
    -- NB: the query-editor init code (incl. the ~365KB span schema JSON) is emitted
    -- by logQueryBox_ inside queryControlsSection. Do not re-emit it here — a second
    -- copy doubled the page payload and re-ran the facet-enrich + JSON encode.

    pidTxt = page.pid.toText
    countText = prettyPrintCount page.queryResultCount
    suffixText = if page.queryResultCount >= page.resultCount then " rows" else "+ rows"

    -- Show/hide-filters label; @attrs@ must not carry a class_ (Lucid concatenates
    -- duplicate class attributes with no separator).
    filtersLabel_ :: [Attribute] -> Html () -> Html ()
    filtersLabel_ attrs trailing = label_ ([class_ "gap-1 flex items-center min-h-6 cursor-pointer text-textWeak rounded has-[:focus-visible]:ring-2 has-[:focus-visible]:ring-strokeBrand-strong"] <> attrs) do
      faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
      span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
      span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
      "filters"
      trailing

    -- data-fullscreen=details|trace drives layout via tailwind.css; single-valued so
    -- "at most one fullscreen mode" holds by construction. Several elements (detail
    -- panel, trace overlay, close handlers) `send toggleFullscreen(mode: …) to
    -- #apiLogsPage`, but this element is the only receiver — so the handler lives on it.
    sectionWrapper_ =
      section_
        [ class_ "mx-auto pt-2 max-md:px-2 px-4 gap-3.5 max-md:gap-2 w-full flex flex-col h-full overflow-y-hidden overflow-x-hidden pb-2 group/pg"
        , id_ "apiLogsPage"
        , [__|on toggleFullscreen(mode, active)
                default active to (my @data-fullscreen is not mode)
                if active
                  set my @data-fullscreen to mode
                  call updateUrlState('fullscreen', mode)
                otherwise if my @data-fullscreen is mode
                  remove @data-fullscreen from me
                  call updateUrlState('fullscreen', '', 'delete')
                end
                send resize to window
              end
              init
                set fs to params().fullscreen
                if fs is 'details' or fs is 'trace' send toggleFullscreen(mode: fs, active: true) to me end
              end|]
        ]

    -- Query box, mobile filter toggle, and the chart/session summary strip.
    queryControlsSection = div_ [class_ "w-full", id_ "log_explorer_controls"] do
      logQueryBox_
        LogQueryBoxConfig
          { pid = page.pid
          , currentRange = page.currentRange
          , source = Just page.source
          , targetSpan = page.targetSpans
          , query = page.query
          , vizType = page.vizType
          , updateUrl = True
          , targetWidgetPreview = Nothing
          , alert = isJust page.alert
          , patternSelected = page.targetPattern
          , mobileExtra = Just do
              filtersLabel_ [Lucid.for_ "toggle-filters"] pass
              span_ [class_ "text-strokeWeak text-xs", Aria.hidden_ "true"] "·"
              rowCountDisplay_ "mobile" countText suffixText
          , parseError = page.parseError
          }

      -- For the sessions and patterns viz the header (summary) is derived in the
      -- same fetch as the rows and injected client-side from the data response
      -- (see log-list.ts) — so render the skeleton here and let the data fetch
      -- fill it, rather than blocking the shell on a summary query. Other viz
      -- types render the chart+latency widgets. #page-summary-region is the swap target.
      div_ [id_ "page-summary-region"]
        $ if page.vizType == Just "sessions" || page.vizType == Just "patterns"
          then sessionsSummarySkeleton_
          else div_ [class_ "timeline flex flex-row gap-4 mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full min-h-36 max-md:min-h-28 aspect-[10/1] max-md:aspect-auto max-md:flex-col"] do
            Widget.widget_ page.chartWidget
            div_ [class_ "flex-1 min-w-0 max-md:hidden"] $ Widget.widget_ page.latencyWidget

      -- Skeletons cloned by swapSessionsRegionIfNeeded during the (multi-second)
      -- viz-tab swap, mirroring each incoming region's height so layout doesn't jump.
      template_ [id_ "sessions-summary-skeleton"] sessionsSummarySkeleton_
      template_ [id_ "chart-summary-skeleton"] chartSummarySkeleton_

    -- Three-pane layout: facets sidebar, logs/viz/trace list, and the
    -- alert-form / log-details side panels (all resizable via `resizer_`).
    facetsAndLogListSection = div_ [class_ "flex max-md:flex-col h-full overflow-y-hidden max-md:overflow-y-auto", id_ "facets_and_loglist"] do
      facetsPanel
      div_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden max-md:hidden mr-3.5", id_ "resizer-facets_width-wrapper"] $ resizer_ "facets-container" "facets_width" True
      logsListPanel
      div_ [class_ "hidden group-has-[#create-alert-toggle:checked]/pg:block max-md:hidden ml-3.5"] $ resizer_ "alert_container" "alert_width" False
      alertPanel
      div_ [class_ $ "transition-opacity duration-200 hidden max-md:hidden ml-3.5 " <> if isJust page.targetEvent then "group-has-[#viz-logs:checked]/pg:block group-has-[#viz-sessions:checked]/pg:block" else "", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False
      detailsPanel

    -- No `contain:layout` here: it makes this a containing block for fixed/anchored
    -- descendants, which clips the facet action popover (top layer) to the sidebar.
    facetsPanel =
      facetRail_
        (Just "facets-container")
        "w-68 will-change-[width] text-sm text-textWeak shrink-0 h-full overflow-y-scroll max-md:w-full max-md:shrink max-md:max-h-[55vh] max-md:border-b max-md:border-strokeWeak group-has-[.toggle-filters:checked]/pg:max-w-0 group-has-[.toggle-filters:checked]/pg:overflow-hidden max-md:group-has-[.toggle-filters:checked]/pg:max-h-0"
        "Search filters"
        Nothing
        $ div_ [id_ "facets-list"]
        $ maybe
          (div_ [class_ "px-1 py-4 text-xs italic text-textWeak"] "Filters are still being built for this project.")
          renderFacets
          page.facetSummary

    logsListPanel = div_ [class_ "grow will-change-[width] contain-[layout_style] relative flex flex-col shrink-1 min-w-0 w-full h-full ", id_ "logs_list_container"] do
      rowCountHeader
      vizWidget
      traceOverlay
      div_ [class_ "flex-1 min-h-0 h-full flex flex-col"]
        $ div_ [class_ "flex-1 min-h-0 hidden h-full group-has-[#viz-logs:checked]/pg:block group-has-[#viz-patterns:checked]/pg:block group-has-[#viz-sessions:checked]/pg:block"]
        $ virtualTable page.pid Nothing page.vizType

    -- Filters toggle and row count, shown above the viz widget / trace / virtual table.
    -- max-md:hidden: on phones this row moves into the toolbar via mobileExtra, and two
    -- copies would also give the screen reader two aria-live regions for one count.
    rowCountHeader = div_ [class_ "max-md:hidden flex gap-2 py-1 text-sm z-10 w-max bg-bgBase -mb-6 group-has-[#viz-patterns:checked]/pg:mb-0"] do
      filtersLabel_ []
        $ input_
          [ type_ "checkbox"
          , class_ "toggle-filters sr-only"
          , id_ "toggle-filters"
          , [__|
              init
                if window.innerWidth < 768 set my.checked to true
                else set my.checked to (localStorage.getItem('toggle-filter-checked') is 'true')
                end
                wait 300ms
                js document.getElementById('filterElement')?.refreshLayout?.() end
              on change
                call localStorage.setItem('toggle-filter-checked', my.checked)
                wait 200ms
                js document.getElementById('filterElement')?.refreshLayout?.() end
            |]
          ]
      span_ [class_ "text-strokeWeak", Aria.hidden_ "true"] "|"
      rowCountDisplay_ "" countText suffixText

    -- Shows when not in logs view (skip for patterns mode which uses log-list)
    vizWidget = div_ [class_ "flex-1 min-h-0 h-full group-has-[#viz-logs:checked]/pg:hidden group-has-[#viz-patterns:checked]/pg:hidden group-has-[#viz-sessions:checked]/pg:hidden"] do
      let widgetVals =
            decodeUtf8
              $ AE.encode
              $ AE.object
                [ "id" AE..= ("visualization-widget" :: Text)
                , "type" AE..= fromMaybe "timeseries" page.vizType
                , "title" AE..= ("Visualization" :: Text)
                , "standalone" AE..= True
                , "allow_zoom" AE..= True
                , "_project_id" AE..= page.pid.toText
                , "_center_title" AE..= True
                , "layout" AE..= AE.object ["w" AE..= (6 :: Int), "h" AE..= (4 :: Int)]
                ]
      div_
        [ id_ "visualization-widget-container"
        , class_ " w-full"
        , style_ "aspect-ratio: 4 / 2;"
        , hxPost_ ("/p/" <> page.pid.toText <> "/widget")
        , hxTrigger_ "intersect once, update-widget"
        , hxTarget_ "this"
        , hxSwap_ "innerHTML"
        , hxVals_ widgetVals
        , hxExt_ "json-enc,forward-page-params"
        , term "hx-sync" "this:replace"
        ]
        ""

    -- Both shared-link and in-app paths enter through loadTrace, so they get the
    -- same immediate skeleton, request, and retry state.
    traceOverlay =
      div_
        [ class_ $ "absolute top-0 right-0 w-full h-full overflow-scroll c-scroll z-50 bg-bgBase transition-all duration-100 " <> if isJust page.showTrace then "" else "hidden"
        , id_ "trace_expanded_view"
        , term "aria-busy" "true"
        , term
            "_"
            [text|on closeTraceView
                    add .hidden to me then send toggleFullscreen(mode: 'trace', active: false) to #apiLogsPage
                    call updateUrlState('showTrace', '', 'delete')
                  end
                  on htmx:after:swap[#apiLogsPage's @data-fullscreen is 'details'] from me
                    send toggleFullscreen(mode: 'trace', active: true) to #apiLogsPage
                  end
                  on htmx:response:error from me
                    remove .hidden from #trace-load-error
                  end
                  on loadTrace(url)
                    set my @data-trace-url to url
                    put #trace-loading-skeleton.innerHTML into me then remove .hidden from me
                    send toggleFullscreen(mode: 'trace', active: true) to #apiLogsPage
                    call htmx.ajax('GET', url, {target: me, swap: 'innerHTML'})
                    then call window.evalScriptsFromContent(me)
                  end
                  on openTraceFullscreen(traceId, timestamp) from window
                    call updateUrlState('showTrace', traceId + '/?timestamp=' + timestamp)
                    send loadTrace(url: '/p/${pidTxt}/traces/' + traceId + '/?timestamp=' + encodeURIComponent(timestamp)) to me|]
        ]
        do
          traceLoadingSkeleton_
          whenJust page.showTrace \trIdAndTimestamp -> do
            let url = "/p/" <> page.pid.toText <> "/traces/" <> trIdAndTimestamp
            div_ [term "data-trace-url" url, [__|init send loadTrace(url: my @data-trace-url) to #trace_expanded_view|]] pass

    -- Lazily loaded (HTMX) the first time this container is revealed, so the
    -- shell never renders it or forks a teams query per load.
    alertPanel = div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll hidden group-has-[#create-alert-toggle:checked]/pg:block w-[500px] max-md:w-full max-md:fixed max-md:inset-0 max-md:z-50 max-md:max-w-full", id_ "alert_container"] do
      let aurl = "/p/" <> page.pid.toText <> "/log_explorer/alert_form" <> maybe "" (\a -> "?alert=" <> a.id.toText) page.alert
      -- Container is display:none until checked, so IntersectionObserver can't drive
      -- the load — fire off the toggle's change instead (and at init if deep-linked open).
      lazyLoad_
        "alert_container"
        aurl
        "loadAlertForm once"
        [ [__|init if #create-alert-toggle.checked then trigger loadAlertForm on me end
              on change[#create-alert-toggle.checked] from #create-alert-toggle trigger loadAlertForm on me|]
        ]

    detailsPanel =
      div_
        [ class_ "details-panel grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0 max-w-0 overflow-hidden group-has-[#viz-logs:checked]/pg:max-w-full group-has-[#viz-logs:checked]/pg:overflow-y-auto group-has-[#viz-sessions:checked]/pg:max-w-full group-has-[#viz-sessions:checked]/pg:overflow-y-auto max-md:hidden max-md:[&.details-open]:block! max-md:[&.details-open]:fixed max-md:[&.details-open]:inset-0 max-md:[&.details-open]:z-40 max-md:[&.details-open]:w-full max-md:[&.details-open]:max-w-full max-md:[&.details-open]:bg-bgBase"
        , id_ "log_details_container"
        , -- Detail loads are last-click-wins. htmx's default sync strategy is "queue first",
          -- which drops a click made while another detail request is in flight: the new row
          -- never loads and the overlay indicator (added on click) is never cleared, so the
          -- panel sits on a frozen three-dot loader until a page reload. "replace" aborts the
          -- in-flight request and issues the new one instead.
          term "hx-sync" "this:replace"
        , term "data-has-target" (if isJust page.targetEvent then "1" else "0")
        , [__|on checkMobileOpen[window.innerWidth < 768] add .details-open to me
        init
          if my @data-has-target is '1'
            send checkMobileOpen to me
            set queryWidth to params().details_width
            set storedWidth to localStorage.getItem('resizer-details_width')
            if queryWidth set my *width to queryWidth + 'px'
            else if storedWidth and not storedWidth.endsWith('px') set my *width to storedWidth + 'px'
            else if storedWidth set my *width to storedWidth
            else set my *width to '30%'
            end
          end
        end
        on htmx:after:swap send checkMobileOpen to me end
        on keydown[key=='Escape' and not (the event's target matches <input, textarea, select, [contenteditable]/>) and no <[popover]:popover-open/> and no <dialog[open]/>] from window
          -- `the first <…/> exists`, not a bare `<…/>`: a query literal is a lazy query object
          -- that stays truthy at zero matches, so a bare `if <sel/>` never falls through.
          if the first <#trace_details_container.open/> exists send closeDetailPanel to #trace_details_container
          otherwise if #trace_expanded_view does not match .hidden send closeTraceView to #trace_expanded_view
          otherwise send closeDetailPanel to me end
        end
        on closeDetailPanel
          add .hidden to #trace_expanded_view
          send toggleFullscreen(mode: 'details', active: false) to #apiLogsPage
          remove .details-open from me
          set my *width to '0px'
          set the *width of #logs_list_container to '100%'
          remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
          add .hidden .opacity-0 .pointer-events-none to #resizer-details_width-wrapper
          call updateUrlState(['details_width', 'target_event', 'showTrace'], '', 'delete')
        end|]
        ]
        do
          htmxOverlayIndicator_ "details_indicator"
          whenJust page.targetEvent \te -> do
            let url = "/p/" <> page.pid.toText <> "/log_explorer/" <> te
            lazyLoad_ "log_details_container" url "intersect once" [hxIndicator_ "#details_indicator"]


-- | Inline-expand endpoint for the Sessions and Patterns visualizations. Returns
-- up to @limitN@ example events for a session (@kind=session@) or pattern
-- (@kind=pattern@), plus a @hasMore@ flag for pagination.
apiLogExpandH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AE.Value)
apiLogExpandH pid kindM keyM skipM queryM sinceM fromM toM = do
  (authCtx, _, fromD, toD, _) <- logDataEnv pid sinceM fromM toM
  let key = maybeToMonoid keyM
  when (T.null key) $ throwError Servant.err400{Servant.errBody = "Missing key"}
  -- Sessions render a trace tree (hence the child-span fetch and larger page);
  -- patterns just show flat examples.
  (expandKind, limitN) <- case kindM of
    Just "session" -> pure (LogQueries.ExpandSession key, 100 :: Int)
    Just "pattern" -> pure (LogQueries.ExpandPattern key, 20)
    _ -> throwError Servant.err400{Servant.errBody = "kind must be session or pattern"}

  queryAST <-
    either (\err -> throwError Servant.err400{Servant.errBody = encodeUtf8 $ "Invalid query: " <> err}) pure (parseQueryToAST (maybeToMonoid queryM))

  (rows, cols) <- LogQueries.fetchEventExamples authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) expandKind (fromMaybe 0 skipM) (limitN + 1)

  let hasMore = V.length rows > limitN
      shown = V.take limitN rows
      colIdxMap = listToIndexHashMap cols
      colOf k v = lookupVecTextByKey v colIdxMap k
      alreadyLoadedIds = V.mapMaybe (colOf "id") shown
      traceIds = V.fromList $ take 100 $ nubOrd $ mapMaybe (mfilter (not . T.null) . colOf "trace_id") $ V.toList shown
      seedSpanIds = V.mapMaybe (colOf "latency_breakdown") shown
  childSpansList <- case expandKind of
    LogQueries.ExpandSession _ -> LogQueries.selectChildSpansAndLogs authCtx.env.enableTimefusionReads pid [] traceIds seedSpanIds (fromD, toD) alreadyLoadedIds
    LogQueries.ExpandPattern _ -> pure []
  let rawLogsData = shown <> V.fromList childSpansList
      (logsData, traces) = buildTraceTree colIdxMap (V.length shown) rawLogsData

  addRespHeaders
    $ AE.object
      [ "cols" AE..= curateCols [] [] cols
      , "rows" AE..= logsData
      , "hasMore" AE..= hasMore
      , "colIdxMap" AE..= colIdxMap
      , "traces" AE..= traces
      , "queryResultCount" AE..= V.length shown
      ]


aiSearchH :: Projects.ProjectId -> AE.Value -> ATAuthCtx (RespHeaders AE.Value)
aiSearchH pid requestBody = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let envCfg = authCtx.env
      parsed = AET.parseMaybe (AE.withObject "request" \o -> liftA2 (,) (o AE..: "input") (o AE..:? "timezone")) requestBody

  (inputText, timezoneM) <-
    parsed `whenNothing` do
      addErrorToast "Invalid AI search input" Nothing
      throwError Servant.err400{Servant.errBody = "Invalid input format"}

  when (T.null (T.strip inputText)) do
    addErrorToast "Please enter a search query" Nothing
    throwError Servant.err400{Servant.errBody = "Empty input"}

  -- Fetch precomputed facets for context (last 24 hours)
  facetSummaryM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" (addUTCTime (-86400) now) now
  let config = (AI.defaultAgenticConfig pid){AI.facetContext = facetSummaryM, AI.timezone = timezoneM, AI.maxIterations = 2, AI.useTimefusion = envCfg.enableTimefusionReads}
  result <- AI.runAgenticQuery config inputText envCfg.openaiModel envCfg.openaiApiKey
  case result of
    Left errMsg -> do
      addErrorToast "AI search failed" (Just errMsg)
      throwError Servant.err502{Servant.errBody = encodeUtf8 errMsg}
    Right resp ->
      addRespHeaders
        $ AE.object
          [ "query" AE..= resp.query
          , "visualization_type" AE..= resp.visualization
          , "commentary" AE..= resp.explanation
          , "time_range" AE..= resp.timeRange
          ]


-- | Visible columns = server defaults plus the URL's deltas (@addCols@ show extra, @removeCols@
-- hide defaults), with id first, timestamp second, latency_breakdown last. Deltas keep the
-- shareable URL small and forward-compatible — new default columns still show up on old links.
--
-- >>> curateCols [] [] ["id","timestamp","resource.service.name","duration","body"]
-- ["id","timestamp","resource.service.name"]
--
-- >>> curateCols [] ["resource.service.name"] ["id","timestamp","resource.service.name"]
-- ["id","timestamp"]
--
-- >>> curateCols ["duration"] [] ["id","timestamp","resource.service.name","duration"]
-- ["id","timestamp","resource.service.name","duration"]
curateCols :: [Text] -> [Text] -> [Text] -> [Text]
curateCols addCols removeCols = sortOn rank . filter keep
  where
    keep c = c == "id" || (c `notElem` removeCols && (c `notElem` hiddenByDefault || c `elem` addCols))
    rank :: Text -> Int
    rank = \case
      "id" -> 0
      "timestamp" -> 1
      "latency_breakdown" -> 3
      _ -> 2 -- sortOn is stable, so ties keep their incoming order
    hiddenByDefault =
      [ "trace_id"
      , "severity_text"
      , "parent_id"
      , "errors"
      , "http_attributes"
      , "db_attributes"
      , "rpc_attributes"
      , "start_time_ns"
      , "kind"
      , "span_name"
      , "status"
      , "start_time"
      , "end_time"
      , "duration"
      , "body"
      ]


-- | Render alert configuration form for creating log-based alerts
alertConfigurationForm_ :: Projects.Project -> Maybe Monitors.QueryMonitor -> V.Vector ManageMembers.Team -> Html ()
alertConfigurationForm_ project alertM teams = do
  let pid = project.id
  div_ [class_ "surface-raised h-full flex flex-col group/alt"] do
    div_ [class_ "flex items-center justify-between px-4 py-2.5"] do
      div_ [class_ "flex items-center gap-2.5"] do
        div_ [class_ "w-8 h-8 rounded-full bg-fillBrand-weak flex items-center justify-center shrink-0"]
          $ faSprite_ "bell" "regular" "w-4 h-4 text-iconBrand"
        div_ [] do
          h3_ [class_ "text-base font-semibold text-textStrong"] "Create monitor"
          p_ [class_ "text-xs text-textWeak hidden sm:block"] "Get notified when your query matches specific conditions"
      label_
        [Lucid.for_ "create-alert-toggle", class_ "p-1 rounded-lg hover:bg-fillWeak transition-colors"]
        $ faSprite_ "xmark" "regular" "w-3 h-3 text-iconNeutral"

    div_ [class_ "p-4 pt-3 flex-1 overflow-y-auto c-scroll"] do
      form_
        [ id_ "alert-form"
        , hxPost_ $ "/p/" <> pid.toText <> "/monitors/alerts"
        , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, source: params().source || 'spans', vizType: getVizType(), teams: window.getTagValues('#alert-form-teams')}"
        , hxSwap_ "none"
        , class_ "flex flex-col gap-3"
        , [__|on htmx:after:request[detail.ctx.response.status < 400] set my value to '' then call me.reset()|]
        ]
        do
          input_ [type_ "hidden", name_ "alertId", value_ $ maybe "" ((.id.toText)) alertM]
          formField_ FieldSm def{value = maybe "" (\x -> x.alertConfig.title) alertM, placeholder = "e.g. High error rate on checkout API"} "Name" "title" True Nothing

          let defaultFrequency = maybe 5 (.checkIntervalMins) alertM
              conditionType = if maybe True (\x -> x.alertThreshold > 0 && isJust x.warningThreshold) alertM then Just "threshold_exceeded" else Just "has_matches"
          AlertUI.monitorScheduleSection_ project.paymentPlan defaultFrequency 5 conditionType Nothing

          AlertUI.thresholdsSection_ Nothing (fmap (.alertThreshold) alertM) ((.warningThreshold) =<< alertM) (maybe False (.triggerLessThan) alertM) ((.alertRecoveryThreshold) =<< alertM) ((.warningRecoveryThreshold) =<< alertM)

          let selectedTeamIds = maybe V.empty (.teams) alertM
          AlertUI.notificationSettingsSection_ ((.alertConfig.severity) <$> alertM) ((.alertConfig.subject) <$> alertM) ((.alertConfig.message) <$> alertM) (maybe True (.alertConfig.emailAll) alertM) teams selectedTeamIds "alert-form" alertM

          div_ [class_ "flex items-center justify-end gap-2 pt-4 pb-20 mt-4 border-t border-strokeWeak"] do
            label_ [Lucid.for_ "create-alert-toggle", class_ "btn btn-sm"] "Cancel"
            button_
              [type_ "submit", class_ "btn btn-primary btn-sm"]
              (faSprite_ "plus" "regular" "w-3.5 h-3.5" >> if isJust alertM then "Update monitor" else "Create monitor")
