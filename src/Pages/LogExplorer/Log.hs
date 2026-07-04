module Pages.LogExplorer.Log (
  apiLogH,
  logExplorerDataH,
  logPatternsH,
  logSessionsH,
  saveQueryH,
  deleteQueryH,
  alertFormH,
  apiLogExpandH,
  aiSearchH,
  queryEvents,
  LogsGet (..),
  LogResult (..),
  QueryLibraryView (..),
  SaveQueryForm (..),
  PatternsView (..),
  SessionsView (..),
  ApiLogsPageData (..),
  virtualTable,
  curateCols,
  logQueryBox_,
  TraceTreeEntry (..),
  buildTraceTree,
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
import Data.Effectful.Hasql qualified as Hasql
import Data.Foldable.WithIndex (iforM_)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
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
import NeatInterpolation (text)
import Numeric (showFFloat)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs, pageActions, pageTitle)
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..), logQueryBox_, queryEditorInitializationCode, queryLibrary_)
import Pkg.Components.TimePicker qualified as Components
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (WTTimeseries, WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (defaultQueryLimit, pSource, parseQueryToAST, toQText)
import Pkg.Parser.Stats (Section (TakeCommand))
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetSummary (..), FacetValue (..))
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils (FieldAction (..), FieldMenuCtx (..), LoadingSize (..), LoadingType (..), checkFreeTierStatus, faSprite_, fieldContextMenuItems_, getDurationNSMS, getServiceColors, htmxOverlayIndicator_, levelFillColor, listToIndexHashMap, loadingIndicator_, lookupVecTextByKey, methodFillColor, popoverPanel_, popoverTrigger_, prettyPrintCount, serviceFillColor, statusFillColorText)

import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Models.Apis.Monitors (MonitorAlertConfig (..))
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as ManageMembers
import Pages.Components (FieldCfg (..), FieldSize (..), formField_, resizer_)
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
import Effectful.Ki qualified as Ki
import OddJobs.Job (createJob)
import Pkg.DeriveUtils (CamelSchema (..), SnakeSchema (..))
import System.Logging qualified as Log
import System.Tracing (Tracing, forkWithCtx)
import Text.Slugify (slugify)
import UnliftIO.Exception (tryAny)
import Web.FormUrlEncoded (FromForm)


data TraceTreeEntry = TraceTreeEntry
  { traceId :: Text
  , startTime :: Int64
  , duration :: Int64
  , traceStartTime :: Maybe Text
  , root :: Text
  , children :: Map.Map Text [Text]
  }
  deriving stock (Generic, Show)
  deriving (AE.ToJSON) via DAE.Snake TraceTreeEntry
  deriving (ToSchema) via SnakeSchema TraceTreeEntry


-- Internal type for span info extraction
data SpanInfo = SpanInfo {spanId :: Text, parentId :: Maybe Text, traceIdVal :: Text, startNs :: Int64, dur :: Int64, timestamp :: Maybe Text, isQueryResult :: Bool, rowIdx :: Int}


-- | Build trace tree from flat rows. Query-result spans (index < queryResultCount)
-- become roots. Non-query-result spans (fetched via selectChildSpansAndLogs) form
-- the tree beneath them.
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
buildTraceTree :: HM.HashMap Text Int -> Int -> V.Vector (V.Vector AE.Value) -> (V.Vector (V.Vector AE.Value), [TraceTreeEntry])
buildTraceTree colIdxMap queryResultCount rows = (adjustedRows, sortWith (Down . (.startTime)) entries)
  where
    lookupIdx = flip HM.lookup colIdxMap
    valText v = (v V.!?) >=> \case AE.String t | not (T.null t) -> Just t; _ -> Nothing
    valInt64 v = (v V.!?) >=> \case AE.Number n -> toBoundedInteger n :: Maybe Int64; _ -> Nothing

    mkSpanInfo :: Int -> V.Vector AE.Value -> SpanInfo
    mkSpanInfo idx row =
      let isLog = maybe False (\i -> valText row i == Just "log") (lookupIdx "kind")
          rawId = fromMaybe ("gen-" <> show idx) $ lookupIdx "id" >>= valText row
          rawLb = lookupIdx "latency_breakdown" >>= valText row
          sid = if isLog then rawId else fromMaybe rawId rawLb
          pid = lookupIdx "parent_id" >>= valText row
          tid = fromMaybe ("gen-trace-" <> show idx) $ lookupIdx "trace_id" >>= valText row
          sns = fromMaybe 0 $ lookupIdx "start_time_ns" >>= valInt64 row
          d = if isLog then 0 else fromMaybe 0 (lookupIdx "duration" >>= valInt64 row)
          ts = lookupIdx "timestamp" >>= valText row
       in SpanInfo sid pid tid sns d ts (idx < queryResultCount) idx

    spanInfos = V.imap mkSpanInfo rows

    grouped :: Map.Map Text [SpanInfo]
    grouped = Map.fromListWith (<>) [(si.traceIdVal, [si]) | si <- V.toList spanInfos]

    -- Per-trace results: (entry, [(rowIdx, adjStart, adjDur)])
    traceResults = concatMap buildTraceEntries (Map.elems grouped)
    entries = map fst traceResults
    adjustments = concatMap snd traceResults

    -- Apply adjustments to row vectors at start_time_ns / duration columns.
    stIdxM = HM.lookup "start_time_ns" colIdxMap
    durIdxM = HM.lookup "duration" colIdxMap
    adjMap :: Map.Map Int (Int64, Int64)
    adjMap = Map.fromList [(i, (s, d)) | (i, s, d) <- adjustments]
    adjustedRows = V.imap applyAdj rows
      where
        applyAdj i row = case Map.lookup i adjMap of
          Nothing -> row
          Just (s, d) ->
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
          traceStartTime = viaNonEmpty head $ sort $ mapMaybe (.timestamp) spans
          tid = maybe "" (.traceIdVal) (viaNonEmpty head spans)
       in map (buildEntry tid sortedChildrenMap spanMap traceStartTime) roots

    -- Walk subtree applying clock-skew correction: if a child starts before its
    -- parent's adjusted start, shift it forward and clamp its duration to fit
    -- within the parent's window. Mirrors Pages.Telemetry.buildSpanTree.
    buildEntry :: Text -> Map.Map Text [Text] -> Map.Map Text SpanInfo -> Maybe Text -> SpanInfo -> (TraceTreeEntry, [(Int, Int64, Int64)])
    buildEntry tid fullChildrenMap spanMap tst root' =
      let rootEnd = root'.startNs + root'.dur
          rootKids = fromMaybe [] (Map.lookup root'.spanId fullChildrenMap)
          initAcc = Map.fromList [(root'.spanId, rootKids) | not (null rootKids)]
          rootAdj = (root'.rowIdx, root'.startNs, root'.dur)
          go _ _ [] st = st
          go pStart pEnd (x : xs) (minS, maxE, adjs, treeAcc) = case Map.lookup x spanMap of
            Nothing -> go pStart pEnd xs (minS, maxE, adjs, treeAcc)
            Just si ->
              let cStart = si.startNs
                  delta = pStart - cStart
                  (adjStart, adjDur) =
                    if delta > 0
                      then (cStart + delta, max 0 (min si.dur (pEnd - cStart - delta)))
                      else (cStart, si.dur)
                  adjEnd = adjStart + adjDur
                  kids = fromMaybe [] (Map.lookup x fullChildrenMap)
                  treeAcc' = if null kids then treeAcc else Map.insert x kids treeAcc
                  st' = (min minS adjStart, max maxE adjEnd, (si.rowIdx, adjStart, adjDur) : adjs, treeAcc')
                  -- recurse depth-first into this subtree, then continue with siblings
                  st'' = go adjStart adjEnd kids st'
               in go pStart pEnd xs st''
          (minStart, maxEnd, adjustments', subtreeChildren) =
            go root'.startNs rootEnd rootKids (root'.startNs, rootEnd, [rootAdj], initAcc)
       in (TraceTreeEntry tid minStart (maxEnd - minStart) tst root'.spanId subtreeChildren, adjustments')


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


-- | Detect query-result spans whose parent_id is missing from the result and
-- ≥2 of them share that same parent_id. Emit one synthetic row per group whose
-- latency_breakdown = parent_id so 'buildTraceTree' nests the orphans under
-- it, matching the trace-breakdown waterfall's visual contract.
synthesizeOrphanHeaders :: HM.HashMap Text Int -> V.Vector (V.Vector AE.Value) -> V.Vector (V.Vector AE.Value)
synthesizeOrphanHeaders colIdxMap rows = V.fromList [synthRow t p ks | ((t, p), ks) <- Map.toList groups, length ks >= 2]
  where
    lookupIdx = flip HM.lookup colIdxMap
    colCount = maybe 0 V.length (rows V.!? 0)
    textAt k r = lookupIdx k >>= (r V.!?) >>= \case AE.String t | not (T.null t) -> Just t; _ -> Nothing
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
          startNs = foldr (min . fst) 0 spans'
          endNs = foldr (max . uncurry (+)) startNs spans'
          label = "Upstream span missing \x2014 " <> T.take 8 pid
          -- Children count already shows on the tree chevron — no duplicate
          -- here. Single 'text-textWeak' style so the renderer's
          -- WEAK_TEXT_STYLES lookup matches; the italic + dashed border live
          -- on the row in log-list.ts, keyed off the synthetic-* id.
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


-- | A facet entry the sidebar can render.
--
-- @path@ is the canonical KQL field name — the lookup key in 'FacetData' AND
-- the field a user types in KQL. Invariant ('prop_facetsAreFast'): @path@ must
-- be a flat-column reference (in 'Pkg.Parser.Expr.flattenedOtelAttributes' or
-- 'Pkg.Parser.Expr.topLevelOtelColumns') so click-to-filter compiles to a
-- direct column scan, not a jsonb_path fallback.
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
      , -- User & Session
        Facet "attributes.session.id" "Session ID" FGUserSession nc
      , Facet "attributes.user.id" "User ID" FGUserSession nc
      , Facet "attributes.user.email" "User Email" FGUserSession nc
      , Facet "attributes.user.name" "Username" FGUserSession nc
      , Facet "attributes.user.full_name" "Full Name" FGUserSession nc
      , -- Database
        Facet "attributes.db.system.name" "Database System" FGDatabase nc
      , Facet "attributes.db.collection.name" "Collection Name" FGDatabase nc
      , Facet "attributes.db.namespace" "Database Namespace" FGDatabase nc
      , Facet "attributes.db.operation.batch.size" "Batch Size" FGDatabase nc
      , -- Errors & Exceptions
        Facet "attributes.exception.type" "Exception Type" FGErrors (const "bg-fillError-strong")
      , Facet "attributes.exception.message" "Exception Message" FGErrors nc
      ]


-- | 'facetDefs' bucketed by 'FacetGroup', built once at module load time.
facetsByGroup :: Map.Map FacetGroup [Facet]
facetsByGroup = Map.fromListWith (flip (<>)) [(f.group, [f]) | f <- facetDefs]


-- | Render facet data for Log Explorer sidebar in a compact format.
-- The facet counts are scaled in the upstream summary based on the selected time range.
renderFacets :: FacetSummary -> Html ()
renderFacets facetSummary = do
  let (FacetData facetMap) = facetSummary.facetJson
  -- Checkbox↔query sync lives in web-components/src/main.ts (syncFacetCheckboxes),
  -- wired to `update-query` + `htmx:afterSettle` so swapped-in facets re-sync.
  forM_ (universe :: [FacetGroup]) \g ->
    renderFacetSection (facetGroupLabel g) (Map.findWithDefault [] g facetsByGroup) facetMap (g /= FGCommon)
  where
    collapsible_ :: [Attribute] -> Text -> Bool -> [Attribute] -> Html () -> Html () -> Html ()
    collapsible_ wrapAttrs toggleId open labelAttrs header body =
      -- No `contain` here: it would become the containing block for the fixed-position
      -- field-action popover (top layer), trapping it inside this section's overflow clip.
      div_ (wrapAttrs <> [class_ " block "]) do
        input_ $ [type_ "checkbox", class_ "hidden peer", id_ toggleId] ++ [checked_ | open]
        label_
          ( labelAttrs
              <> [ class_ " cursor-pointer peer-checked:[&_.chev]:rotate-0 "
                 , role_ "button"
                 , Lucid.for_ toggleId
                 , Aria.expanded_ (T.toLower $ show open)
                 , [__|live set @aria-expanded to (the previous <input/>'s checked)|]
                 ]
          )
          header
        div_ [class_ "h-0 overflow-hidden peer-checked:h-auto transition-[height] ease-out duration-200"] body

    chev_ :: Text -> Html ()
    chev_ size = faSprite_ "chevron-down" "regular" $ "chev shrink-0 transition-transform -rotate-90 " <> size

    renderFacetSection :: Text -> [Facet] -> HM.HashMap Text [FacetValue] -> Bool -> Html ()
    renderFacetSection sectionName fs facetMap collapsed =
      collapsible_
        [class_ "facet-section-group"]
        ("toggle-" <> slugify sectionName)
        (not collapsed)
        [class_ "p-2 bg-fillWeak rounded-lg flex gap-2 items-center"]
        (chev_ "w-3 h-3" >> span_ [class_ "font-medium text-sm"] (toHtml sectionName))
        $ div_ [class_ "facets-container mt-1"]
        $ iforM_ fs \idx f -> do
          let values = HM.lookupDefault [] f.path facetMap
              open = f.group == FGCommon && idx < 5 && not (null values)
              (visibleValues, hiddenValues) = splitAt 5 values
              hiddenCount = length hiddenValues
              renderFacetValue (FacetValue val count) =
                label_ [class_ "facet-item flex items-center justify-between py-0.5 max-md:py-1.5 px-1 hover:bg-fillWeak rounded cursor-pointer will-change-[background-color]"] do
                  div_ [class_ "flex items-center gap-2 min-w-0 flex-1"] do
                    input_
                      [ type_ "checkbox"
                      , class_ "checkbox checkbox-xs max-md:checkbox-sm"
                      , [__|on click toggleSubQuery(@data-field + ' == "' + @data-value + '"') on #filterElement|]
                      , term "data-tippy-content" (f.path <> " == \"" <> val <> "\"")
                      , term "data-field" f.path
                      , term "data-value" val
                      ]
                    let colorClass = f.color val
                    unless (T.null colorClass) $ span_ [class_ $ colorClass <> " shrink-0 w-0.5 h-3 rounded-sm"] ""
                    span_ [class_ "facet-value truncate text-xs", term "data-tippy-content" val] (toHtml val)
                  span_ [class_ "facet-count text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ prettyPrintCount count
          collapsible_
            [class_ "facet-section border-t border-strokeWeak"]
            ("facet-toggle-" <> f.path)
            open
            [class_ "flex items-center justify-between hover:bg-fillWeak rounded"]
            do
              div_
                [class_ "p-2 flex items-center gap-2 flex-1"]
                (chev_ "w-2.5 h-2.5" >> span_ [class_ "text-sm", term "data-tippy-content" f.path] (toHtml f.label))
              div_ [class_ "inline-block"] do
                button_ ([type_ "button", class_ "cursor-pointer p-2 hover:bg-fillWeak rounded", Aria.label_ "Facet options"] <> popoverTrigger_ (slugify f.path))
                  $ faSprite_ "ellipsis-vertical" "regular" "w-3 h-3"
                -- Opens below the ⋮, extending right over the log list (top-layer popover),
                -- so long field names aren't truncated inside the narrow facets sidebar.
                ul_ ([class_ "dropdown menu p-2 shadow-sm bg-bgRaised rounded-box w-96 border border-strokeWeak z-50", term "data-field-path" f.path] <> popoverPanel_ (slugify f.path))
                  $ fieldContextMenuItems_ (StaticField f.path Nothing) [FCopyField, FDivider, FGroupBy, FViewPatterns, FDivider, FAddColumn]
            $ div_ [class_ "facet-values pl-7 pr-2 mb-1 space-y-1"] do
              if null values
                then div_ [class_ "facet-empty px-1 py-1 text-xs italic text-textWeak"] "no values in window"
                else forM_ visibleValues renderFacetValue
              when (hiddenCount > 0) do
                input_ [type_ "checkbox", class_ "hidden peer/more", id_ $ "more-" <> f.path]
                label_ [class_ "text-textBrand text-xs px-1 py-0.5 cursor-pointer hover:underline", Lucid.for_ $ "more-" <> f.path] do
                  span_ [class_ "peer-checked/more:hidden"] $ toHtml $ "+ More (" <> prettyPrintCount hiddenCount <> ")"
                  span_ [class_ "hidden peer-checked/more:inline"] $ toHtml $ "- Less (" <> prettyPrintCount hiddenCount <> ")"
                div_ [class_ "hidden peer-checked/more:block space-y-1"] $ forM_ hiddenValues renderFacetValue


keepNonEmpty :: Maybe Text -> Maybe Text
keepNonEmpty Nothing = Nothing
keepNonEmpty (Just "") = Nothing
keepNonEmpty (Just a) = Just a


-- | Core result builder shared by apiLogH and queryEvents.
-- When @withChildren@ is False, only rows matching the predicate are returned —
-- no descendants of matches, no synthesised orphan headers (both are
-- trace-tree concerns the UI wants but the API/CLI usually doesn't).
buildLogResult :: (DB es, Time.Time :> es) => Bool -> Projects.ProjectId -> UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> [Text] -> (V.Vector (V.Vector AE.Value), [Text], Int) -> Eff es LogResult
buildLogResult withChildren pid now sinceM fromM toM addCols removeCols (requestVecs, colNames, resultCount') = do
  let colIdxMap = listToIndexHashMap colNames
      reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
      reqFirstCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? 0)
      alreadyLoadedIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "id") requestVecs
      (fromDD, toDD, _) = Components.parseTimeRange now (Components.TimePicker sinceM reqLastCreatedAtM reqFirstCreatedAtM)
  childSpansList <-
    if not withChildren || V.length requestVecs > 100
      then pure [] -- Skip expensive child span fetch for large result sets; traces load lazily on detail view
      else do
        let allTraceIds = V.filter (not . T.null) $ V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
            traceIds = V.fromList $ take 50 $ nubOrd $ V.toList allTraceIds
            -- latency_breakdown is aliased from context___span_id (see Pkg.Parser).
            seedSpanIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "latency_breakdown") requestVecs
        LogQueries.selectChildSpansAndLogs pid addCols traceIds seedSpanIds (fromDD, toDD) alreadyLoadedIds
  let synthRows = if withChildren then synthesizeOrphanHeaders colIdxMap requestVecs else V.empty
      requestVecsAug = synthRows <> requestVecs
      rawLogsData = requestVecsAug <> V.fromList childSpansList
      cols = nubOrd $ curateCols addCols removeCols colNames
      colors = getServiceColors $ V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") rawLogsData
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
      , -- Compare ONLY the real fetched rows (requestVecs) against selectLogTable's
        -- overflow sentinel (resultCount' = limit+1 when more pages exist). Counting
        -- synthesized orphan-header rows here inflates the page to limit+synth ≥ sentinel,
        -- flipping hasMore false on any page with an orphan group and stalling load-more.
        hasMore = V.length requestVecs < resultCount'
      , traces
      }


-- | Standalone query function for the v1 API events endpoint. Returns a
-- JSON-shaped 400 (@{"error": {code, message, field?, suggestion?, details?}}@)
-- for parse/query errors so the CLI can render a one-liner instead of raw
-- Hasql/SQL; propagates DB errors instead of silently returning empty results.
queryEvents :: (DB es, ELog.Log :> es, Error Servant.ServerError :> es, Time.Time :> es, Tracing :> es) => Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Bool -> Eff es LogResult
queryEvents pid queryM sinceM fromM toM sourceM limitM withChildrenM = do
  now <- Time.currentTime
  let queryInput = fromMaybe "" queryM
  queryAST <- case parseQueryToAST queryInput of
    Left err -> throwError $ kqlError400 "invalid_query" ("Invalid query: " <> err) Nothing Nothing Nothing
    Right ast -> pure ast
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
      -- Apply the API `limit` (default 100, capped at defaultQueryLimit) as the
      -- query row-limit when the KQL has no explicit `| limit`, so selectLogTable
      -- returns exactly the page and hasMore/cursor stay consistent. A post-hoc
      -- `V.take` can't: the trace-tree rows include synth headers + descendants,
      -- which miscount hasMore and drop matched roots past the cut.
      hasKqlLimit = any (\case TakeCommand{} -> True; _ -> False) queryAST
      queryAST' = if hasKqlLimit then queryAST else queryAST <> [TakeCommand (min defaultQueryLimit (fromMaybe 100 limitM))]
  result <- LogQueries.selectLogTable pid queryAST' (toQText queryAST') Nothing (fromD, toD) [] (parseMaybe pSource =<< sourceM) Nothing
  case result of
    Left err -> throwError $ translateQueryError err
    -- Default to exact-match (no trace expansion); UI passes True via apiLogH.
    Right r -> buildLogResult (fromMaybe False withChildrenM) pid now sinceM fromM toM [] [] r


-- | Translate the raw exception string from 'LogQueries.selectLogTable' into a
-- structured 400. A column-not-exist (SQLSTATE 42703) becomes an @unknown_field@
-- error with the missing column extracted so the CLI can suggest a fix;
-- everything else is a generic @query_failed@ with the raw text under @details@.
translateQueryError :: Text -> Servant.ServerError
translateQueryError raw =
  let
    -- The Hasql @show@ output usually contains the bare PG error somewhere;
    -- pluck the first reasonable summary line so the user doesn't see a
    -- paragraph of Haskell record syntax.
    firstLine = T.strip $ T.takeWhile (/= '\n') raw
    summary
      | T.null firstLine = "Query execution failed"
      | T.length firstLine > 240 = T.take 237 firstLine <> "…"
      | otherwise = firstLine
   in
    case extractMissingColumn raw of
      Just col ->
        kqlError400
          "unknown_field"
          ("Unknown field \"" <> col <> "\"")
          (Just col)
          (Just $ "wrap as 'body has \"" <> col <> "\"' for full-text, or use 'field == value' for equality")
          (Just raw)
      Nothing ->
        kqlError400
          "query_failed"
          summary
          Nothing
          Nothing
          (Just raw)


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


-- | Pull a missing column name from the underlying SQL error. Handles both:
--
-- - Postgres: @column "X" does not exist@ (SQLSTATE 42703)
-- - TimeFusion: @Schema error: No field named X@
--
-- Returns 'Nothing' when neither shape matches so we don't mislabel
-- unrelated errors (timeouts, network, etc.).
extractMissingColumn :: Text -> Maybe Text
extractMissingColumn t = tfMatch <|> pgMatch
  where
    tfMatch =
      let after = snd (T.breakOn "no field named " (T.toLower t))
       in if T.null after
            then Nothing
            else
              -- Use the original-case text from the same offset so the
              -- returned column name keeps its casing.
              let idx = T.length t - T.length after
                  rest = T.drop (idx + T.length "no field named ") t
                  col = T.strip $ T.takeWhile (\c -> c /= '.' && c /= ' ' && c /= '\n' && c /= '"' && c /= '`') rest
               in if T.null col then Nothing else Just col
    pgMatch =
      let after = snd (T.breakOn "column " (T.toLower t))
       in if T.null after || not ("does not exist" `T.isInfixOf` T.toLower t)
            then Nothing
            else
              let idx = T.length t - T.length after
                  rest = T.drop (idx + T.length "column ") t
                  col = case T.stripPrefix "\"" rest of
                    Just q -> T.takeWhile (/= '"') q
                    Nothing -> T.takeWhile (\c -> c /= ' ' && c /= ',') rest
               in if T.null col then Nothing else Just col


-- | Log Explorer page shell. Renders chrome only (query box, facets, widgets,
-- session header). Log rows are fetched separately by the log-list web
-- component from 'logExplorerDataH' (and the sibling patterns/sessions
-- endpoints), so this handler no longer touches the row-fetch, query-library
-- mutation, or alert-form paths — each of those is now its own endpoint.
apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM' cols' sinceM fromM toM sourceM targetSpansM targetEventM showTraceM vizTypeM alertM pTargetM = do
  let source = fromMaybe "spans" sourceM
  (sess, project, bw) <- mkPageCtx pid
  let queryInput = maybeToMonoid queryM'
  let parseError msg = addTriggerEvent "showParseError" (AE.toJSON msg) >> addErrorToast "Error Parsing Query" (Just msg) $> ([], Just msg)
  (queryAST, parseErrorMsg) <- case parseQueryToAST queryInput of
    Left err -> parseError err
    Right ast
      | not (T.null (T.strip queryInput)) && null ast -> parseError "Invalid query syntax"
      | otherwise -> pure (ast, Nothing)

  -- Fire-and-forget: mark onboarding done + log this query into history.
  recordExploration pid sess.persistentSession.userId project.onboardingStepsCompleted queryAST

  now <- Time.currentTime
  let (fromD, toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  authCtx <- Effectful.Reader.Static.ask @AuthContext

  -- An alert ID pre-fills the query box and selects the alert's viz type.
  alertDM <- case alertM >>= UUID.fromText of
    Just alertId -> Monitors.queryMonitorById (Monitors.QueryMonitorId alertId)
    Nothing -> pure Nothing
  let effectiveVizType = vizTypeM <|> ((.visualizationType) <$> alertDM)

  -- Only shell-side queries: query library, facets, free-tier, and — for the
  -- sessions viz — the session-level summary that renders in the page header.
  (queryLibE, facetSummaryE, freeTierStatusE, sessionSummary) <- Ki.scoped \scope -> do
    let aw = Ki.atomically . Ki.await
    t1 <- forkWithCtx scope $ tryAny $ Projects.queryLibHistoryForUser pid sess.persistentSession.userId
    t2 <- forkWithCtx scope $ tryAny $ SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)
    t3 <- forkWithCtx scope $ tryAny $ checkFreeTierStatus pid project.paymentPlan
    t4 <- forkWithCtx scope $ case effectiveVizType of
      Just "sessions" -> Just . first (show @Text) <$> tryAny (LogQueries.fetchSessionSummary authCtx.env.enableTimefusionReads pid queryAST (fromD, toD))
      _ -> pure Nothing
    (,,,) <$> aw t1 <*> aw t2 <*> aw t3 <*> aw t4

  let logErr label res = whenLeft_ (void res) (Log.logAttention ("Log explorer " <> label <> " failed") . show @Text)
  logErr "queryLib" queryLibE
  logErr "facets" facetSummaryE
  logErr "freeTierStatus" freeTierStatusE
  whenJust sessionSummary \s -> whenLeft_ s (Log.logAttention "fetchSessionSummary failed")

  let queryLib = fromRight [] queryLibE
      facetSummary = join $ rightToMaybe facetSummaryE
      freeTierStatus = fromRight def freeTierStatusE
      (queryLibRecent, queryLibSaved) = bimap V.fromList V.fromList $ L.partition (\x -> Projects.QLTHistory == x.queryType) queryLib

  -- Queue facet generation if no precomputed facets exist (new projects)
  when (isNothing facetSummary)
    $ liftIO
    $ withResource authCtx.jobsPool \conn ->
      void $ createJob conn "background_jobs" $ BackgroundJobs.GenerateOtelFacetsBatch (V.singleton pid) now

  let preloadUrl = T.replace "\"" "%22" $ LogQueries.logExplorerUrlPath pid queryM' cols' Nothing sinceM fromM toM Nothing sourceM False
      headContent = Just $ script_ [text|window.logDataPromise = fetch("$preloadUrl", {headers: {Accept: "application/json"}, credentials: "include"}).then(r => r.json());|]

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
          , headContent = headContent
          , pageActions = Just $ logExplorerActions_ currentRange
          , navTabs = Just $ logExplorerNavTabs_ pid
          }

  let page =
        ApiLogsPageData
          { pid
          , resultCount = 0
          , currentRange
          , query = queryM'
          , source
          , targetSpans = targetSpansM
          , queryLibRecent
          , queryLibSaved
          , targetEvent = targetEventM
          , showTrace = showTraceM
          , facets = facetSummary
          , vizType = effectiveVizType
          , alert = alertDM
          , sessionSummary
          , targetPattern = pTargetM
          , chartWidget
          , latencyWidget
          , queryResultCount = 0
          , parseError = parseErrorMsg
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
  label_ [class_ "cursor-pointer border border-strokeWeak rounded-lg flex shadow-xs", role_ "switch", Aria.label_ "Stream live data", [__|on change from #streamLiveData if #streamLiveData.checked set @aria-checked to 'true' else set @aria-checked to 'false'|], term "aria-checked" "false"] do
    input_ [type_ "checkbox", id_ "streamLiveData", class_ "hidden"]
    span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-2 items-center", data_ "tippy-content" "Pause live stream"] $ faSprite_ "pause" "solid" "h-4 w-4 text-iconNeutral"
    span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex py-1 px-2 items-center", data_ "tippy-content" "Stream live data"] $ faSprite_ "play" "regular" "h-4 w-4 text-iconNeutral"
  Components.timepicker_ (Just "log_explorer_form") currentRange Nothing
  Components.refreshButton_


-- | Log Explorer nav tabs (Events / Metrics).
logExplorerNavTabs_ :: Projects.ProjectId -> Html ()
logExplorerNavTabs_ pid = div_ [class_ "tabs tabs-box tabs-outline items-center", role_ "tablist"] do
  a_ ([href_ $ "/p/" <> pid.toText <> "/log_explorer", role_ "tab", class_ "tab h-auto! tab-active text-textStrong", term "aria-current" "page", term "aria-selected" "true"] <> navTabAttrs) "Events"
  a_ ([href_ $ "/p/" <> pid.toText <> "/metrics", role_ "tab", class_ "tab h-auto! ", term "aria-selected" "false"] <> navTabAttrs) "Metrics"


-- | Shared prologue for the log-data endpoints: auth-gate the request, grab the
-- app config + clock, and resolve the time range once.
logDataEnv :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (AuthContext, UTCTime, Maybe UTCTime, Maybe UTCTime)
logDataEnv pid sinceM fromM toM = do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  pure (authCtx, now, fromD, toD)


-- | Log-row data endpoint. The log-list web component fetches this; the shell
-- (apiLogH) renders only chrome. Returns the trace-tree-expanded 'LogResult'.
logExplorerDataH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogResult)
logExplorerDataH pid queryM' cols' cursorM' sinceM fromM toM sourceM targetSpansM = do
  (authCtx, now, fromD, toD) <- logDataEnv pid sinceM fromM toM
  -- `cols` is a delta over server defaults: bare tokens add columns, `-`-prefixed tokens hide defaults.
  let (removeToks, addCols) = L.partition ("-" `T.isPrefixOf`) $ filter (not . T.null) $ T.splitOn "," (fromMaybe "" cols')
      removeCols = map (T.drop 1) removeToks
      emptyTable = (V.empty, ["timestamp", "summary", "duration"], 0)
  tableData <- case parseQueryToAST (maybeToMonoid queryM') of
    Left err -> Log.logInfo "Log explorer data: rejected invalid KQL query" err $> emptyTable
    Right queryAST -> do
      resultE <-
        Hasql.withHasqlTimefusion authCtx.env.enableTimefusionReads
          $ LogQueries.selectLogTable pid queryAST (toQText queryAST) cursorM' (fromD, toD) addCols (parseMaybe pSource =<< sourceM) targetSpansM
      case resultE of
        Left err -> Log.logAttention "Log explorer data query failed" (show @Text err) $> emptyTable
        Right t -> pure t
  -- UI always wants the trace-tree context; the API/CLI defaults off.
  lr <- buildLogResult True pid now sinceM fromM toM addCols removeCols tableData
  let lastFM = lr.cursor >>= textToUTC <&> toText . iso8601Show . addUTCTime (-0.001)
  addRespHeaders
    (lr :: LogResult)
      { nextUrl = LogQueries.logExplorerUrlPath pid queryM' cols' lastFM sinceM fromM toM (Just "loadmore") sourceM False
      , resetLogsUrl = LogQueries.logExplorerUrlPath pid queryM' cols' Nothing Nothing Nothing Nothing Nothing sourceM False
      , recentUrl = LogQueries.logExplorerUrlPath pid queryM' cols' Nothing sinceM fromM toM (Just "loadmore") sourceM True
      }


-- | Patterns visualization data endpoint (aggregate log patterns as JSON).
logPatternsH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ATAuthCtx (RespHeaders PatternsView)
logPatternsH pid queryM' sinceM fromM toM sourceM pTargetM skipM = do
  (authCtx, _, fromD, toD) <- logDataEnv pid sinceM fromM toM
  case parseQueryToAST (maybeToMonoid queryM') of
    Left err -> Log.logInfo "Log explorer patterns: rejected invalid KQL query" err >> addRespHeaders (PatternsView 0 V.empty)
    Right queryAST -> do
      (total, rows) <- LogQueries.fetchLogPatterns authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) (parseMaybe pSource =<< sourceM) pTargetM (fromMaybe 0 skipM)
      addRespHeaders $ PatternsView total (V.fromList rows)


-- | Sessions visualization data endpoint (aggregate sessions as JSON).
logSessionsH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> ATAuthCtx (RespHeaders SessionsView)
logSessionsH pid queryM' sinceM fromM toM skipM sortByM = do
  (authCtx, _, fromD, toD) <- logDataEnv pid sinceM fromM toM
  case parseQueryToAST (maybeToMonoid queryM') of
    Left err -> Log.logInfo "Log explorer sessions: rejected invalid KQL query" err >> addRespHeaders (SessionsView 0 V.empty)
    Right queryAST -> do
      (total, rows) <- LogQueries.fetchSessions authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) sortByM (fromMaybe 0 skipM)
      addRespHeaders $ SessionsView total (V.fromList rows)


-- | Form body for 'saveQueryH' (HTMX serializes the modal's inputs + hx-vals here).
data SaveQueryForm = SaveQueryForm {query :: Maybe Text, queryLibId :: Maybe Text, queryTitle :: Maybe Text}
  deriving stock (Generic)
  deriving anyclass (FromForm)


-- | Save (create or rename) a query-library item, returning the refreshed
-- popover fragment. Split out of the log-fetch GET so a mutation isn't
-- smuggled through a read path.
saveQueryH :: Projects.ProjectId -> SaveQueryForm -> ATAuthCtx (RespHeaders QueryLibraryView)
saveQueryH pid form = do
  (sess, _) <- Projects.sessionAndProject pid
  let uid = sess.persistentSession.userId
      queryAST = fromRight [] $ parseQueryToAST (maybeToMonoid form.query)
  if (isJust . keepNonEmpty) form.queryLibId && (isJust . keepNonEmpty) form.queryTitle
    then Projects.queryLibTitleEdit pid uid (maybeToMonoid form.queryLibId) (maybeToMonoid form.queryTitle) >> addSuccessToast "Edited Query title successfully" Nothing
    else Projects.queryLibInsert Projects.QLTSaved pid uid (toQText queryAST) queryAST form.queryTitle >> addSuccessToast "Saved to Query Library successfully" Nothing
  addTriggerEvent "closeModal" ""
  queryLibraryFragment pid uid


-- | Delete a query-library item, returning the refreshed popover fragment.
deleteQueryH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders QueryLibraryView)
deleteQueryH pid qId = do
  (sess, _) <- Projects.sessionAndProject pid
  let uid = sess.persistentSession.userId
  Projects.queryLibItemDelete pid uid qId
  addSuccessToast "Deleted from Query Library successfully" Nothing
  queryLibraryFragment pid uid


-- | Re-read the caller's query library and wrap it as the popover fragment.
queryLibraryFragment :: Projects.ProjectId -> Projects.UserId -> ATAuthCtx (RespHeaders QueryLibraryView)
queryLibraryFragment pid uid = do
  queryLib <- Projects.queryLibHistoryForUser pid uid
  let (recent, saved) = bimap V.fromList V.fromList $ L.partition (\x -> Projects.QLTHistory == x.queryType) queryLib
  addRespHeaders $ QueryLibraryView pid saved recent


-- | Lazily-loaded alert configuration form (HTMX partial). Kept off the shell's
-- hot path — the shell no longer forks a teams query or renders this every load.
alertFormH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
alertFormH pid alertM = do
  (_, project) <- Projects.sessionAndProject pid
  alertDM <- case alertM >>= UUID.fromText of
    Just alertId -> Monitors.queryMonitorById (Monitors.QueryMonitorId alertId)
    Nothing -> pure Nothing
  teams <- V.fromList <$> ManageMembers.getTeams pid
  addRespHeaders $ alertConfigurationForm_ project alertDM teams


textToUTC :: Text -> Maybe UTCTime
textToUTC = iso8601ParseM . toString


-- Widget definitions for log explorer charts
logChartWidget :: Projects.ProjectId -> Widget.Widget
logChartWidget pid =
  (def :: Widget.Widget)
    { Widget.wType = WTTimeseries
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


-- | Rendered when the sessions summary query fails. Surfaces the failure
-- inline rather than silently falling back to the generic log/span widgets
-- (which would reintroduce the unit-of-analysis mismatch).
sessionsHeaderError_ :: Text -> Html ()
sessionsHeaderError_ err =
  div_ [class_ "mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden surface-raised rounded-2xl px-3 py-2 flex items-start gap-2 text-sm"] do
    faSprite_ "triangle-exclamation" "regular" "h-4 w-4 mt-0.5 text-iconError shrink-0"
    div_ [class_ "min-w-0"] do
      div_ [class_ "text-textStrong font-medium"] "Session summary unavailable"
      div_ [class_ "text-textWeak text-xs truncate"] $ toHtml err


sessionsHeader_ :: LogQueries.SessionSummary -> Html ()
sessionsHeader_ summ = do
  let total = fromIntegral summ.totalSessions :: Int
      errored = fromIntegral summ.erroredSessions :: Int
      clean = max 0 (total - errored)
      errRate = if total == 0 then 0 else 100 * (fromIntegral errored :: Double) / fromIntegral total
      medDur = toText $ getDurationNSMS (fromIntegral summ.medianDurationNs)
      p95Dur = toText $ getDurationNSMS (fromIntegral summ.p95DurationNs)
      totalEvt = fromIntegral summ.totalEvents :: Int
      bars = zip3 [0 :: Int ..] summ.clean summ.errored
      maxBar = foldl' max 0 $ zipWith (+) summ.clean summ.errored
      norm n = if maxBar <= 0 then 0 else (fromIntegral n / fromIntegral maxBar :: Double) * 100
      bucketFrom i = summ.bucketStartEpoch + i * summ.bucketWidthSec
      kpi :: Text -> Text -> Maybe Text -> Html ()
      kpi label value subM = div_ [class_ "surface-raised rounded-2xl px-3 py-2 flex flex-col gap-0.5 min-w-0"] do
        span_ [class_ "text-xs text-textWeak truncate"] $ toHtml label
        strong_ [class_ "text-textStrong text-xl font-bold tabular-nums leading-tight truncate"] $ toHtml value
        whenJust subM (span_ [class_ "text-xs text-textWeak tabular-nums truncate"] . toHtml)
  div_ [class_ "mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full flex flex-col gap-2"] do
    div_ [class_ "grid grid-cols-6 max-md:grid-cols-3 gap-2"] do
      kpi "Sessions" (prettyPrintCount total) (Just $ prettyPrintCount clean <> " clean")
      kpi "Errored" (fmtPct1 errRate) (Just $ prettyPrintCount errored <> " sessions")
      kpi "Median duration" medDur (Just $ "p95 " <> p95Dur)
      kpi "Median events" (prettyPrintCount $ fromIntegral summ.medianEvents) (Just $ prettyPrintCount totalEvt <> " total")
      kpi "Users" (prettyPrintCount $ fromIntegral summ.uniqueUsers) Nothing
      kpi "Services" (prettyPrintCount $ fromIntegral summ.uniqueServices) Nothing
    div_ [class_ "surface-raised rounded-2xl px-3 py-2"] do
      div_ [class_ "flex items-center justify-between mb-1"] do
        span_ [class_ "text-xs text-textWeak"] "Sessions over time"
        div_ [class_ "flex gap-3 text-xs text-textWeak"] do
          span_ [class_ "flex items-center gap-1"] do
            span_ [class_ "inline-block w-2 h-2 rounded-sm bg-fillBrand-strong/70"] ""
            "Clean"
          span_ [class_ "flex items-center gap-1"] do
            span_ [class_ "inline-block w-2 h-2 rounded-sm bg-fillError-strong"] ""
            "Errored"
      if total == 0 || null bars
        then div_ [class_ "h-12 flex items-center justify-center text-xs text-textWeak"] "No sessions in range"
        else div_ [class_ "flex items-end gap-[2px] h-12"] do
          forM_ bars \(i, c, e) -> do
            let cPct = norm c
                ePct = norm e
                tip = prettyPrintCount (c + e) <> " sessions · " <> prettyPrintCount e <> " errored"
                js =
                  "window.__sessionsBucketFilter("
                    <> T.show (bucketFrom i)
                    <> ","
                    <> T.show (bucketFrom i + summ.bucketWidthSec)
                    <> ")"
            button_
              [ class_ "flex-1 h-full flex flex-col-reverse gap-[1px] min-w-[2px] cursor-pointer group/bar"
              , type_ "button"
              , data_ "tippy-content" tip
              , onclick_ js
              ]
              do
                -- Match the legend swatch opacity (/70) so clean bars are
                -- visibly readable on the dark surface — /40 was nearly
                -- invisible, making clean traffic look absent.
                when (c > 0)
                  $ div_ [class_ "w-full rounded-sm bg-fillBrand-strong/70 group-hover/bar:bg-fillBrand-strong transition-colors", style_ $ "height:" <> T.show (max 4 cPct) <> "%"] ""
                when (e > 0)
                  $ div_ [class_ "w-full rounded-sm bg-fillError-strong", style_ $ "height:" <> T.show (max 4 ePct) <> "%"] ""
      -- Reassigned on every render so bug fixes aren't masked by a stale
      -- definition left behind from a prior HTMX swap. Dispatches the same
      -- update-query event log-list uses for chart-zoom so the table refetches.
      script_
        [text|
          window.__sessionsBucketFilter = function(fromEpoch, toEpoch) {
            if (!Number.isFinite(fromEpoch) || !Number.isFinite(toEpoch) || fromEpoch <= 0) return;
            const from = new Date(fromEpoch * 1000).toISOString();
            const to = new Date(toEpoch * 1000).toISOString();
            if (window.updateTimePicker) {
              window.updateTimePicker({ from, to }, { skipSetParams: true });
            } else {
              console.warn('[sessions-header] updateTimePicker missing; picker UI will not reflect filter');
            }
            const p = new URLSearchParams(window.location.search);
            p.set('from', from); p.set('to', to); p.delete('since');
            const url = window.location.pathname + '?' + p.toString() + window.location.hash;
            window.history.replaceState({}, '', url);
            document.dispatchEvent(new CustomEvent('update-query', { bubbles: true, detail: { source: 'sessions-header-bar', timeRange: from + ' \u2192 ' + to } }));
          };
        |]


data LogsGet
  = LogPage (PageCtx ApiLogsPageData)
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-textError"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogsGetError _) = AE.object ["error" AE..= True, "message" AE..= ("Something went wrong" :: Text)]
  toJSON (LogsGetErrorSimple msg) = AE.object ["error" AE..= True, "message" AE..= msg]
  toJSON _ = AE.object ["error" AE..= True]


-- | HTMX fragment for the query-library popover, returned by the save/delete
-- mutation endpoints (hxSelect extracts #queryLibraryContent from it).
data QueryLibraryView = QueryLibraryView Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)


instance ToHtml QueryLibraryView where
  toHtml (QueryLibraryView pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtmlRaw = toHtml


-- | JSON payload for the patterns visualization endpoint.
data PatternsView = PatternsView Int (V.Vector LogQueries.PatternRow)


instance AE.ToJSON PatternsView where
  toJSON (PatternsView totalPatterns patterns) =
    let patternToSummary pat
          | "\x1E" `T.isInfixOf` pat = AE.toJSON (T.splitOn "\x1E" pat)
          | otherwise = AE.toJSON (splitSummaryElements pat)
        -- Group words into summary elements: each word containing ⇒ starts a new
        -- element; subsequent plain words are part of the preceding element's value.
        splitSummaryElements :: Text -> [Text]
        splitSummaryElements = map (unwords . reverse) . reverse . foldl' go [] . words
          where
            go [] w = [[w]]
            go acc w | "⇒" `T.isInfixOf` w = [w] : acc
            go (cur : rest) w = (w : cur) : rest
        cols = ["id", "pattern_count", "volume", "level", "service", "summary"] :: [Text]
        allCols = cols ++ ["merged_count", "is_error"] :: [Text]
        rows = V.map (\p -> AE.Array $ V.fromList [AE.Null, AE.toJSON p.count, AE.toJSON p.volume, AE.toJSON p.level, AE.toJSON p.service, patternToSummary p.logPattern, AE.toJSON p.mergedCount, AE.toJSON p.isError]) patterns
        total = V.foldl' (\acc p -> acc + p.count) 0 patterns
     in aggregateEnvelope rows cols allCols total ["totalPatterns" AE..= totalPatterns]


-- | JSON payload for the sessions visualization endpoint.
--
-- Sessions use the exact same column layout as logs so the same rendering code
-- is reused. Session-specific info is packed into the summary column as badge
-- elements. Column indices must match the logs colIdxMap exactly.
data SessionsView = SessionsView Int (V.Vector LogQueries.SessionRow)


instance AE.ToJSON SessionsView where
  toJSON (SessionsView totalSessions sessions) =
    let
      -- Display columns — identical to logs
      cols = ["id", "timestamp", "service", "summary", "latency_breakdown"] :: [Text]
      -- Full colIdxMap — same indices as logs so groupSpans/tree logic works
      allCols = ["id", "timestamp", "trace_id", "span_name", "duration", "service", "parent_id", "start_time_ns", "errors", "summary", "latency_breakdown", "kind", "event_count"] :: [Text]
      fmtDuration ns
        | ns >= 60_000_000_000 = show (ns `div` 60_000_000_000) <> "m"
        | ns >= 1_000_000_000 = show (ns `div` 1_000_000_000) <> "s"
        | ns >= 1_000_000 = show (ns `div` 1_000_000) <> "ms"
        | otherwise = show ns <> "ns"
      rowOf s =
        let user = LogQueries.sessionUserDisplay s.userEmail s.userName s.userId
            svcList = V.toList s.services
            svc = if null svcList then "" else T.intercalate " " svcList
            -- Error messages can be stack traces. Take just the first line
            -- and cap length so one huge exception doesn't blow out the row.
            clipError t =
              let firstLine = fromMaybe t . viaNonEmpty head . lines . T.strip $ t
               in if T.length firstLine > 120 then T.take 119 firstLine <> "\x2026" else firstLine
            -- `session;right-neutral` puts the session id in the right-aligned
            -- badge group, which is the branch that renders the ▶ play button
            -- (see createSessionButton in log-list.ts). Without the right-
            -- prefix the session id just renders as inert text.
            -- Contract: summary parts are `field;style⇒value`. Empty style
            -- still needs the semicolon so parseSummaryElement classifies
            -- them as `formatted` — without it, renderSessionSummary drops
            -- the whole row's fields as plain content.
            summaryParts =
              -- Full session id: the client feeds this into /replay_session/{id},
              -- which Servant captures as a UUID. Truncating here would round-trip
              -- to a 400 from Servant's route parser before the handler runs.
              [ "session;right-neutral\x21d2" <> s.sessionId
              , "user;\x21d2" <> user
              ]
                ++ ["url;\x21d2" <> u | Just u <- [s.landingUrl], not (T.null u)]
                ++ ["device;\x21d2" <> ua | Just ua <- [s.userAgent], not (T.null ua)]
                ++ ["events;\x21d2" <> show s.eventCount | s.eventCount > 0]
                ++ ["errors;\x21d2" <> show s.errorCount | s.errorCount > 0]
                ++ ["error;\x21d2" <> clipError e | Just e <- [s.firstError], not (T.null e)]
                ++ ["duration;\x21d2" <> toText (fmtDuration s.durationNs)]
         in AE.Array
              $ V.fromList
                [ AE.Null -- id (0)
                , AE.toJSON s.firstSeen -- timestamp (1)
                , AE.toJSON s.sessionId -- trace_id (2) — used as expand key
                , AE.String "" -- span_name (3)
                , AE.toJSON s.durationNs -- duration (4)
                , AE.toJSON svc -- service (5)
                , AE.String "" -- parent_id (6)
                , AE.Number 0 -- start_time_ns (7)
                , AE.toJSON s.errorCount -- errors (8)
                , AE.toJSON summaryParts -- summary (9)
                , AE.Null -- latency_breakdown (10)
                , AE.String "" -- kind (11)
                , AE.toJSON s.traceCount -- event_count (12) — used for [+N] children count
                ]
      rows = V.map rowOf sessions
      total = V.foldl' (\acc s -> acc + s.eventCount) 0 sessions
     in
      aggregateEnvelope rows cols allCols total ["totalSessions" AE..= totalSessions]


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
    ++ extra


-- | Render context for the Log Explorer page shell. Rows/aggregates are fetched
-- separately by the log-list web component (see 'logExplorerDataH' and the
-- patterns/sessions endpoints), so this carries only what the chrome renders.
data ApiLogsPageData = ApiLogsPageData
  { pid :: Projects.ProjectId
  , resultCount :: Int
  , currentRange :: Maybe (Text, Text)
  , query :: Maybe Text
  , source :: Text
  , targetSpans :: Maybe Text
  , queryLibRecent :: V.Vector Projects.QueryLibItem
  , queryLibSaved :: V.Vector Projects.QueryLibItem
  , targetEvent :: Maybe Text
  , showTrace :: Maybe Text
  , facets :: Maybe FacetSummary
  , vizType :: Maybe Text
  , alert :: Maybe Monitors.QueryMonitor
  , sessionSummary :: Maybe (Either Text LogQueries.SessionSummary)
  , targetPattern :: Maybe Text
  , chartWidget :: Widget.Widget
  , latencyWidget :: Widget.Widget
  , queryResultCount :: Int
  , parseError :: Maybe Text
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
  }
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] LogResult
  deriving (ToSchema) via CamelSchema LogResult


virtualTable :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Html ()
virtualTable pid initialFetchUrl modeM = do
  termRaw
    "log-list"
    ( [ id_ "resultTable"
      , class_ "w-full shrink-1 flex flex-col h-full min-w-0 rr-block"
      , term "windowTarget" "logList"
      , term "projectId" pid.toText
      ]
        <> [term "initialFetchUrl" (fromMaybe "" initialFetchUrl) | isJust initialFetchUrl]
        <> [term "mode" m | m <- maybeToList modeM]
    )
    ("" :: Text)


-- | Inner div that lazily HTMX-loads @url@ into @#target@ once @trigger@ fires.
-- Shared by the trace, details, and alert-form side panels — each is a hidden
-- container whose body is fetched on first reveal; @extra@ carries the per-panel
-- bits (loading indicator, or the hyperscript that fires the trigger).
lazyLoad_ :: Text -> Text -> Text -> [Attribute] -> Html ()
lazyLoad_ target url trigger extra =
  div_ ([hxGet_ url, hxTarget_ ("#" <> target), hxSwap_ "innerHTML", hxTrigger_ trigger, term "hx-sync" "this:replace"] <> extra) pass


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  sectionWrapper_ do
    template_ [id_ "loader-tmp"] $ loadingIndicator_ LdMD LdDots
    div_ [class_ "fixed z-[9999] hidden right-0 w-max h-max border rounded top-32 bg-bgBase shadow-lg", id_ "sessionPlayerWrapper"] do
      termRaw "session-replay" [id_ "sessionReplay", class_ "shrink-1 flex flex-col", term "projectId" page.pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)
    shareLogModal
    queryControlsSection
    facetsAndLogListSection
  queryEditorInitializationCode page.queryLibRecent page.queryLibSaved page.vizType ((.facetJson) <$> page.facets)
  where
    countText = prettyPrintCount page.queryResultCount
    suffixText = if page.queryResultCount >= page.resultCount then " rows" else "+ rows"
    showTrace = isJust page.showTrace
    pidT = page.pid.toText

    -- data-fullscreen=details|trace drives layout via tailwind.css; single-valued so
    -- "at most one fullscreen mode" holds by construction.
    sectionWrapper_ =
      section_
        [ class_ "mx-auto pt-2 max-md:px-2 px-4 gap-3.5 max-md:gap-2 w-full flex flex-col h-full overflow-y-hidden overflow-x-hidden pb-2 group/pg"
        , id_ "apiLogsPage"
        , [__|on toggleFullscreen(mode, active)
              default active to (my @data-fullscreen is not mode) then
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

    shareLogModal =
      div_
        [ class_ "fixed hidden right-0 top-0 justify-end left-0 bottom-0 w-full bg-black bg-opacity-5 z-50"
        , [__|on click remove .show-log-modal from #expand-log-modal|]
        , id_ "expand-log-modal"
        ]
        do
          div_ [class_ "relative ml-auto w-full"] do
            div_ [class_ "flex justify-end  w-full p-4 "]
              $ button_ [class_ "cursor-pointer", Aria.label_ "Close log details", [__|on click add .hidden to #expand-log-modal|]]
              $ faSprite_ "xmark" "regular" "h-8"
            form_
              [ hxPost_ $ "/p/" <> page.pid.toText <> "/share/"
              , hxSwap_ "innerHTML"
              , hxTarget_ "#copy_share_link"
              , id_ "share_log_form"
              ]
              do
                input_ [type_ "hidden", value_ "1 hour", name_ "expiresIn", id_ "expire_input"]
                input_ [type_ "hidden", value_ "", name_ "reqId", id_ "req_id_input"]
                input_ [type_ "hidden", value_ "", name_ "reqCreatedAt", id_ "req_created_at_input"]

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
          , queryLibRecent = page.queryLibRecent
          , queryLibSaved = page.queryLibSaved
          , updateUrl = True
          , targetWidgetPreview = Nothing
          , alert = isJust page.alert
          , patternSelected = page.targetPattern
          , mobileExtra = Just do
              label_ [class_ "gap-1 flex items-center cursor-pointer text-textWeak", Lucid.for_ "toggle-filters"] do
                faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
                span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
                span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
                "filters"
              span_ [class_ "text-strokeWeak text-xs"] "·"
              rowCountDisplay_ "mobile" countText suffixText
          , parseError = page.parseError
          , facetData = (.facetJson) <$> page.facets
          }

      -- Sessions viz renders a session-level header. A Left means the summary
      -- query failed — surface it visibly instead of silently reverting to the
      -- generic span/log widgets, which would reintroduce the unit-of-analysis
      -- mismatch this view exists to fix. Wrapped in #page-summary-region so the
      -- viz-tab change handler can swap this fragment when crossing the sessions
      -- boundary (sessions uses a different region than other viz types).
      div_ [id_ "page-summary-region"] $ case page.sessionSummary of
        Just (Right summ) -> sessionsHeader_ summ
        Just (Left err) -> sessionsHeaderError_ err
        Nothing ->
          div_ [class_ "timeline flex flex-row gap-4 mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full min-h-36 max-md:min-h-28 aspect-[10/1] max-md:aspect-auto max-md:flex-col"] do
            Widget.widget_ page.chartWidget
            div_ [class_ "flex-1 min-w-0 max-md:hidden"] $ Widget.widget_ page.latencyWidget

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
      div_ [class_ "w-68 will-change-[width] text-sm text-textWeak shrink-0 flex flex-col h-full overflow-y-scroll gap-2 max-md:w-full max-md:shrink max-md:max-h-48 max-md:border-b max-md:border-strokeWeak group-has-[.toggle-filters:checked]/pg:max-w-0 group-has-[.toggle-filters:checked]/pg:overflow-hidden max-md:group-has-[.toggle-filters:checked]/pg:max-h-0", id_ "facets-container"] do
        div_ [class_ "sticky top-0 z-10 bg-bgBase relative mb-2"] do
          span_ [class_ "absolute inset-y-0 left-3 flex items-center", Aria.hidden_ "true"]
            $ faSprite_ "magnifying-glass" "regular" "w-4 h-4 text-iconNeutral"
          input_
            [ placeholder_ "Search filters..."
            , class_ "rounded-lg pl-10 pr-3 py-1.5 border border-strokeStrong w-full"
            , term "data-filterParent" "facets-container"
            , [__| on keyup
                    if the event's key is 'Escape'
                      set my value to '' then trigger keyup
                    else
                      show <div.facet-section-group/> in #{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()
                      show <div.facet-section/> in #{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()
                      show <div.facet-value/> in #{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()
                  |]
            ]
        whenJust page.facets renderFacets

    logsListPanel = div_ [class_ "grow will-change-[width] contain-[layout_style] relative flex flex-col shrink-1 min-w-0 w-full h-full ", id_ "logs_list_container"] do
      rowCountHeader
      vizWidget
      traceOverlay
      div_ [class_ "flex-1 min-h-0 h-full flex flex-col"]
        $ div_ [class_ "flex-1 min-h-0 hidden h-full group-has-[#viz-logs:checked]/pg:block group-has-[#viz-patterns:checked]/pg:block group-has-[#viz-sessions:checked]/pg:block"]
        $ virtualTable page.pid Nothing Nothing

    -- Filters toggle and row count, shown above the viz widget / trace / virtual table.
    rowCountHeader = div_ [class_ "flex gap-2 py-1 text-sm z-10 w-max bg-bgBase -mb-6 group-has-[#viz-patterns:checked]/pg:mb-0"] do
      label_ [class_ "gap-1 flex items-center cursor-pointer text-textWeak"] do
        faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
        span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
        span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
        "filters"
        input_
          [ type_ "checkbox"
          , class_ "toggle-filters hidden"
          , id_ "toggle-filters"
          , [__|
              init
                if window.innerWidth < 768 set my.checked to true
                else set my.checked to (localStorage.getItem('toggle-filter-checked') is 'true')
                end
                wait 300ms
                if #filterElement call #filterElement.refreshLayout() end
              on change
                call localStorage.setItem('toggle-filter-checked', my.checked)
                wait 200ms
                if #filterElement call #filterElement.refreshLayout() end
            |]
          ]
      span_ [class_ "text-strokeWeak "] "|"
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

    -- The openTraceFullscreen event is dispatched by the hover "View trace"
    -- button on virtual-list rows (log-list.ts).
    traceOverlay =
      div_
        [ class_ $ "absolute top-0 right-0  w-full h-full overflow-scroll c-scroll z-50 bg-bgBase transition-all duration-100 " <> if showTrace then "" else "hidden"
        , id_ "trace_expanded_view"
        , term
            "_"
            [text|on closeTraceView
                    add .hidden to me
                    send toggleFullscreen(mode: 'trace', active: false) to #apiLogsPage
                    call updateUrlState('showTrace', '', 'delete')
                  end
                  on htmx:afterSwap[#apiLogsPage's @data-fullscreen is 'details'] from me
                    send toggleFullscreen(mode: 'trace', active: true) to #apiLogsPage
                  end
                  on openTraceFullscreen(traceId, timestamp) from window
                    put '' into me
                    remove .hidden from me
                    send toggleFullscreen(mode: 'trace', active: true) to #apiLogsPage
                    call updateUrlState('showTrace', traceId + '/?timestamp=' + timestamp)
                    call htmx.ajax('GET', '/p/$pidT/traces/' + traceId + '/?timestamp=' + encodeURIComponent(timestamp), {target: me, swap: 'innerHTML'})
                    then call window.evalScriptsFromContent(me)|]
        ]
        do
          whenJust page.showTrace \trIdAndTimestamp -> do
            let url = "/p/" <> page.pid.toText <> "/traces/" <> trIdAndTimestamp
            loadingIndicator_ LdMD LdDots
            lazyLoad_ "trace_expanded_view" url "intersect once" []

    -- Lazily loaded (HTMX) the first time this container is revealed, so the
    -- shell never renders it or forks a teams query per load.
    alertPanel = div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll hidden group-has-[#create-alert-toggle:checked]/pg:block w-[500px] max-md:w-full max-md:fixed max-md:inset-0 max-md:z-50 max-md:max-w-full", id_ "alert_container"] do
      let aurl = "/p/" <> page.pid.toText <> "/log_explorer/alert_form" <> maybe "" (\a -> "?alert=" <> a.id.toText) page.alert
      -- The container is display:none until #create-alert-toggle is checked, so an
      -- IntersectionObserver ("intersect") can't reliably drive the load. Fire off the
      -- toggle's change instead (and at init for a deep-linked, already-open panel).
      lazyLoad_
        "alert_container"
        aurl
        "loadAlertForm once"
        [ [__|init if #create-alert-toggle.checked then trigger loadAlertForm on me end
              on change[#create-alert-toggle.checked] from #create-alert-toggle trigger loadAlertForm on me|]
        ]

    detailsPanel =
      div_
        [ class_ "grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0 max-w-0 overflow-hidden group-has-[#viz-logs:checked]/pg:max-w-full group-has-[#viz-logs:checked]/pg:overflow-y-auto group-has-[#viz-sessions:checked]/pg:max-w-full group-has-[#viz-sessions:checked]/pg:overflow-y-auto max-md:hidden max-md:[&.details-open]:block! max-md:[&.details-open]:fixed max-md:[&.details-open]:inset-0 max-md:[&.details-open]:z-40 max-md:[&.details-open]:w-full max-md:[&.details-open]:max-w-full max-md:[&.details-open]:bg-bgBase"
        , id_ "log_details_container"
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
        on htmx:afterSwap send checkMobileOpen to me end
        on keydown[key=='Escape' and not (the event's target matches <input, textarea, select, [contenteditable]/>) and no <[popover]:popover-open/> and no <dialog[open]/>] from window
          if #trace_expanded_view does not match .hidden send closeTraceView to #trace_expanded_view
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


-- | Inline-expand endpoint for the Sessions and Patterns visualizations.
-- Returns up to @limitN@ example events that belong to a given session
-- (@kind=session@) or that match a given pattern template (@kind=pattern@),
-- plus a @hasMore@ flag for pagination.
apiLogExpandH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AE.Value)
apiLogExpandH pid kindM keyM skipM queryM sinceM fromM toM = do
  _ <- Projects.sessionAndProject pid
  when (T.null $ maybeToMonoid keyM) $ throwError Servant.err400{Servant.errBody = "Missing key"}
  expandKind <- case kindM of
    Just "session" -> pure $ LogQueries.ExpandSession (maybeToMonoid keyM)
    Just "pattern" -> pure $ LogQueries.ExpandPattern (maybeToMonoid keyM)
    _ -> throwError Servant.err400{Servant.errBody = "kind must be session or pattern"}
  let isSession = case expandKind of LogQueries.ExpandSession _ -> True; LogQueries.ExpandPattern _ -> False
      limitN = (if isSession then 100 else 20) :: Int
  queryAST <- case parseQueryToAST (maybeToMonoid queryM) of
    Left err -> throwError Servant.err400{Servant.errBody = encodeUtf8 $ "Invalid query: " <> err}
    Right ast -> pure ast
  now <- Time.currentTime
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  (rows, cols) <- LogQueries.fetchEventExamples authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) expandKind (fromMaybe 0 skipM) (limitN + 1)
  let hasMore = V.length rows > limitN
      shown = if hasMore then V.take limitN rows else rows
      colIdxMap = listToIndexHashMap cols
      alreadyLoadedIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "id") shown
  -- Only fetch child spans for sessions (trace tree view); patterns just show flat examples
  let traceIds = V.fromList $ take 100 $ nubOrd $ mapMaybe (mfilter (not . T.null) . (\v -> lookupVecTextByKey v colIdxMap "trace_id")) $ V.toList shown
      seedSpanIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "latency_breakdown") shown
  childSpansList <- bool (pure []) (LogQueries.selectChildSpansAndLogs pid [] traceIds seedSpanIds (fromD, toD) alreadyLoadedIds) isSession
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
  (inputText, timezoneM) <- case parsed of
    Nothing -> do
      addErrorToast "Invalid AI search input" Nothing
      throwError Servant.err400{Servant.errBody = "Invalid input format"}
    Just v -> pure v
  when (T.null (T.strip inputText)) $ do
    addErrorToast "Please enter a search query" Nothing
    throwError Servant.err400{Servant.errBody = "Empty input"}
  -- Fetch precomputed facets for context (last 24 hours)
  facetSummaryM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" (addUTCTime (-86400) now) now
  let config = (AI.defaultAgenticConfig pid){AI.facetContext = facetSummaryM, AI.timezone = timezoneM, AI.maxIterations = 2}
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


-- | Compute the visible columns from server defaults plus the user's URL-encoded deltas:
-- @addCols@ are extra columns to show (bare tokens in the `cols` param), @removeCols@ are
-- default columns to hide (@-@-prefixed tokens). Deltas (rather than an explicit column
-- list) keep the shareable URL small and forward-compatible — new default columns still
-- appear for old links, and no transient client state can collapse the table. @id@ is
-- always kept as the row key.
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
curateCols addCols removeCols cols = sortBy sortAccordingly visible
  where
    defaultSummaryPaths =
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
    -- Show a fetched column when it's the id key, or it's not explicitly removed and either
    -- isn't hidden-by-default or was explicitly added. `removeCols` wins over `addCols` for the
    -- (client-never-produced) both-present case.
    visible = flip filter cols \c ->
      c == "id" || (c `notElem` removeCols && (c `notElem` defaultSummaryPaths || c `elem` addCols))

    sortAccordingly :: Text -> Text -> Ordering
    sortAccordingly a b
      | a == "id" = LT
      | b == "id" = GT
      | a == "timestamp" && b /= "id" = LT
      | b == "timestamp" && a /= "id" = GT
      | a == "latency_breakdown" = GT
      | b == "latency_breakdown" = LT
      | otherwise = comparing (`L.elemIndex` visible) a b


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
      button_
        [ type_ "button"
        , class_ "p-1 rounded-lg hover:bg-fillWeak transition-colors"
        , [__|on click set #create-alert-toggle.checked to false|]
        ]
        $ faSprite_ "xmark" "regular" "w-3 h-3 text-iconNeutral"

    div_ [class_ "p-4 pt-3 flex-1 overflow-y-auto c-scroll"] do
      form_
        [ id_ "alert-form"
        , hxPost_ $ "/p/" <> pid.toText <> "/monitors/alerts"
        , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, source: params().source || 'spans', vizType: getVizType(), teams: window.getTagValues('#alert-form-teams')}"
        , hxSwap_ "none"
        , class_ "flex flex-col gap-3"
        , [__|on htmx:afterRequest[detail.successful] set my value to '' then call me.reset()|]
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
            button_
              [ type_ "button"
              , class_ "btn btn-outline btn-sm"
              , [__|on click set #create-alert-toggle.checked to false|]
              ]
              "Cancel"
            button_
              [ type_ "submit"
              , class_ "btn btn-primary btn-sm"
              ]
              do
                faSprite_ "plus" "regular" "w-3.5 h-3.5"
                if isJust alertM then "Update monitor" else "Create monitor"
