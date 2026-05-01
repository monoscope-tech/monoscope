module Pages.LogExplorer.Log (
  apiLogH,
  apiLogExpandH,
  aiSearchH,
  queryEvents,
  LogsGet (..),
  LogResult (..),
  ApiLogsPageData (..),
  virtualTable,
  curateCols,
  logQueryBox_,
  TraceTreeEntry (..),
  buildTraceTree,
  fmtPct1,
)
where

import Control.Error (hush)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
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
import Models.Apis.Fields (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.Fields qualified as Fields
import Models.Apis.LogQueries qualified as LogQueries
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Numeric (showFFloat)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), currProject, navTabAttrs, pageActions, pageTitle, sessM)
import Pkg.Components.LogQueryBox (LogQueryBoxConfig (..), logQueryBox_, queryEditorInitializationCode, queryLibrary_)
import Pkg.Components.TimePicker qualified as Components
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (WTTimeseries, WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (pSource, parseQueryToAST, toQText)
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils (FreeTierStatus (..), LoadingSize (..), LoadingType (..), checkFreeTierStatus, faSprite_, formatUTC, getDurationNSMS, getServiceColors, htmxOverlayIndicator_, levelFillColor, listToIndexHashMap, loadingIndicator_, lookupVecTextByKey, methodFillColor, prettyPrintCount, serviceFillColor, statusFillColorText)

import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Models.Apis.Monitors (MonitorAlertConfig (..))
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as ManageMembers
import Pages.Components (FieldCfg (..), FieldSize (..), formField_, resizer_)
import Pages.Monitors qualified as AlertUI
import Pkg.AI qualified as AI

import BackgroundJobs qualified
import Control.Lens ((.~), (?~))
import Data.Map.Strict qualified as Map
import Data.OpenApi (NamedSchema (..), OpenApiItems (..), OpenApiType (..), Referenced (..), ToSchema (..))
import Data.OpenApi qualified as OA
import Data.Pool (withResource)
import Data.Scientific (toBoundedInteger)
import Data.Set qualified as S
import Deriving.Aeson qualified as DAE
import Effectful.Ki qualified as Ki
import OddJobs.Job (createJob)
import Pkg.DeriveUtils (SnakeSchema (..))
import System.Logging qualified as Log
import UnliftIO.Exception (tryAny)


data TraceTreeEntry = TraceTreeEntry
  { traceId :: Text
  , startTime :: Int64
  , duration :: Int64
  , traceStartTime :: Maybe Text
  , root :: Text
  , children :: Map.Map Text [Text]
  }
  deriving stock (Generic, Show)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] TraceTreeEntry
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
                    [ (\j -> (j, AE.Number (fromIntegral s))) <$> stIdxM
                    , (\j -> (j, AE.Number (fromIntegral d))) <$> durIdxM
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


-- | Render facet data for Log Explorer sidebar in a compact format
-- | The facet counts are already scaled in the Fields.getFacetSummary function based on the selected time range
-- | Facets are normally generated for a 24-hour period, but will be proportionally adjusted for the user's time selection
renderFacets :: FacetSummary -> Html ()
renderFacets facetSummary = do
  let (FacetData facetMap) = facetSummary.facetJson

      -- Color functions for different facet types (using shared Utils)
      statusColorFn = statusFillColorText
      methodColorFn = methodFillColor
      levelColorFn = levelFillColor

      -- Group facet fields by category
      -- Define mapping of field keys to display names and color functions

      -- Root level facets (displayed at the top)
      rootFacets :: [(Text, Text, Text -> Text)]
      rootFacets =
        [ ("level", "Log Level", levelColorFn)
        ,
          ( "status_code"
          , "Status Code"
          , \val -> case T.toUpper val of
              "OK" -> "bg-fillSuccess-strong"
              "ERROR" -> "bg-fillError-strong"
              "UNSET" -> "bg-fillWeak"
              _ -> "bg-fillStrong"
          )
        , ("resource___service___name", "Service", serviceFillColor)
        , ("name", "Operation Name", const "")
        , ("kind", "Kind", const "")
        , ("resource___service___version", "Service Version", const "")
        , ("attributes___http___request___method", "HTTP Method", methodColorFn)
        , ("attributes___http___response___status_code", "HTTP Status", statusColorFn)
        , ("attributes___error___type", "Error Type", const "bg-fillError-strong")
        ]

      -- Grouped facets for better organization
      facetGroups :: [(Text, [(Text, Text, Text -> Text)])]
      facetGroups =
        [
          ( "HTTP"
          ,
            [ ("attributes___http___request___method_original", "Original Method", methodColorFn)
            , ("attributes___http___request___resend_count", "Resend Count", const "")
            , ("attributes___http___request___body___size", "Request Body Size", const "")
            , ("attributes___url___path", "URL Path", const "")
            , ("attributes___url___scheme", "URL Scheme", const "")
            , ("attributes___url___full", "Full URL", const "")
            , ("attributes___url___fragment", "URL Fragment", const "")
            , ("attributes___url___query", "URL Query", const "")
            , ("attributes___user_agent___original", "User Agent", const "")
            ]
          )
        ,
          ( "Severity"
          ,
            [ ("severity___severity_text", "Severity Text", levelColorFn)
            , ("severity___severity_number", "Severity Number", const "")
            , ("status_message", "Status Message", const "")
            ]
          )
        ,
          ( "Resource"
          ,
            [ ("resource___service___instance___id", "Service Instance ID", const "")
            , ("resource___service___namespace", "Service Namespace", const "")
            , ("resource___telemetry___sdk___language", "SDK Language", const "")
            , ("resource___telemetry___sdk___name", "SDK Name", const "")
            , ("resource___telemetry___sdk___version", "SDK Version", const "")
            ]
          )
        ,
          ( "Network"
          ,
            [ ("attributes___network___protocol___name", "Protocol Name", const "")
            , ("attributes___network___protocol___version", "Protocol Version", const "")
            , ("attributes___network___transport", "Transport", const "")
            , ("attributes___network___type", "Network Type", const "")
            , ("attributes___client___address", "Client Address", const "")
            , ("attributes___server___address", "Server Address", const "")
            ]
          )
        ,
          ( "User & Session"
          ,
            [ ("attributes___user___id", "User ID", const "")
            , ("attributes___user___email", "User Email", const "")
            , ("attributes___user___name", "Username", const "")
            , ("attributes___user___full_name", "Full Name", const "")
            , ("attributes___session___id", "Session ID", const "")
            , ("attributes___session___previous___id", "Previous Session", const "")
            ]
          )
        ,
          ( "Database"
          ,
            [ ("attributes___db___system___name", "Database System", const "")
            , ("attributes___db___collection___name", "Collection Name", const "")
            , ("attributes___db___namespace", "Database Namespace", const "")
            , ("attributes___db___operation___name", "DB Operation", const "")
            , ("attributes___db___response___status_code", "DB Status Code", const "")
            , ("attributes___db___operation___batch___size", "Batch Size", const "")
            ]
          )
        ,
          ( "Errors & Exceptions"
          ,
            [ ("attributes___error___type", "Error Type", const "bg-fillError-strong")
            , ("attributes___exception___type", "Exception Type", const "bg-fillError-strong")
            , ("attributes___exception___message", "Exception Message", const "")
            ]
          )
        ]

  -- Add JS for filtering with checkbox sync
  script_
    [text|
    function filterByFacet(field, value) {
      const queryFragment = field + ' == "' + value + '"';
      document.getElementById("filterElement").toggleSubQuery(queryFragment);
    }
    
    // Function to update facet checkboxes based on query content
    function syncFacetCheckboxes() {
      const filterEl = document.getElementById("filterElement");
      if (!filterEl) return;
      
      // Need to get the query directly from the monaco editor
      const query = filterEl.editor ? filterEl.editor.getValue() : "";
      
      // Batch DOM updates using requestAnimationFrame
      requestAnimationFrame(() => {
        const checkboxes = document.querySelectorAll('input[type="checkbox"][data-field][data-value]');
        checkboxes.forEach(cb => {
          const field = cb.getAttribute('data-field');
          const value = cb.getAttribute('data-value');
          const fragment = field + ' == "' + value + '"';
          cb.checked = query.includes(fragment);
        });
      });
    }
    
    // Initialize and set up event listeners
    document.addEventListener('DOMContentLoaded', () => {
      setTimeout(syncFacetCheckboxes, 100);
      
      // Listen for query changes via the custom event
      window.addEventListener('update-query', syncFacetCheckboxes);
    });
  |]

  renderFacetSection "Common Filters" rootFacets facetMap False

  forM_ facetGroups $ \(groupName, facetDisplays) -> renderFacetSection groupName facetDisplays facetMap True
  where
    renderFacetSection :: Text -> [(Text, Text, Text -> Text)] -> HM.HashMap Text [FacetValue] -> Bool -> Html ()
    renderFacetSection sectionName facetDisplays facetMap collapsed = do
      -- Use div with group for section
      div_ [class_ "facet-section-group group/section block contain-[layout_style]"] do
        input_ $ [type_ "checkbox", class_ "hidden peer", id_ $ "toggle-" <> T.replace " " "-" sectionName] ++ [checked_ | not collapsed]
        -- Section header - use label to toggle checkbox
        label_ [class_ "p-2 bg-fillWeak rounded-lg cursor-pointer flex gap-2 items-center peer-checked:[&>svg]:rotate-0", role_ "button", Lucid.for_ $ "toggle-" <> T.replace " " "-" sectionName, Aria.expanded_ (if collapsed then "false" else "true"), [__|on change from previous <input/> if the checked of previous <input/> set @aria-expanded to 'true' else set @aria-expanded to 'false'|]] do
          faSprite_ "chevron-down" "regular" "w-3 h-3 transition-transform -rotate-90"
          span_ [class_ "font-medium text-sm"] (toHtml sectionName)

        -- Facets container
        div_ [class_ "facets-container mt-1 max-h-0 overflow-hidden peer-checked:max-h-[2000px] transition-[max-height] duration-300"] do
          forM_ (zip [0 ..] facetDisplays) \(idx, (key, displayName, colorFn)) ->
            whenJust (HM.lookup key facetMap) \values -> do
              let shouldBeExpanded = sectionName == "Common Filters" && idx < 5
              label_ [class_ "facet-section border-t border-strokeWeak group/facet block contain-[layout_style]"] do
                input_ $ [type_ "checkbox", class_ "hidden", id_ $ "facet-toggle-" <> key] ++ [checked_ | shouldBeExpanded]
                -- Facet header with actions
                div_ [class_ "flex items-center justify-between hover:bg-fillWeak rounded"] do
                  div_ [class_ "p-2 flex items-center gap-2 cursor-pointer flex-1"] do
                    faSprite_ "chevron-down" "regular" "w-2.5 h-2.5 transition-transform group-has-[:checked]/facet:rotate-0 -rotate-90"
                    span_ [class_ "text-sm", term "data-tippy-content" (T.replace "___" "." key)] (toHtml displayName)

                  -- Dropdown menu for actions
                  div_ [class_ "dropdown dropdown-end contain-[layout_style]", onclick_ "event.stopPropagation()"] do
                    a_ [tabindex_ "0", class_ "cursor-pointer p-2 hover:bg-fillWeak rounded", Aria.label_ "Facet options", role_ "button"] do
                      faSprite_ "ellipsis-vertical" "regular" "w-3 h-3"
                    ul_ [tabindex_ "0", class_ "dropdown-content z-10 menu p-2 shadow-sm bg-bgRaised rounded-box w-52"] do
                      li_
                        $ a_
                          [ term "data-field" (T.replace "___" "." key)
                          , class_ "flex gap-2 items-center"
                          , [__|
                             init 
                                set cols to params().cols or '' then 
                                set colsX to cols.split(',') then
                                if colsX contains @data-field
                                  set innerHTML of first <span/> in me to 'Remove column'
                                end
                              on click
                                call #resultTable.toggleColumnOnTable(@data-field) then
                                set cols to params().cols or '' then
                                set colsX to cols.split(',')
                                if colsX contains @data-field
                                  set innerHTML of first <span/> in me to 'Remove column'
                                else
                                  set innerHTML of first <span/> in me to 'Add as table column'
                                end
                              |]
                          ]
                          do
                            faSprite_ "table-column" "regular" "w-4 h-4 text-iconNeutral"
                            span_ [] "Add as table column"
                      li_
                        $ a_
                          [ term "data-field" (T.replace "___" "." key)
                          , term "data-key" key
                          , class_ "flex gap-2 items-center"
                          , [__|
                              init call window.updateGroupByButtonText(event, me) end
                              on refreshItem call window.updateGroupByButtonText(event, me) end

                              on click
                                call document.querySelector('query-builder').toggleGroupByField(@data-field) then
                                trigger refreshItem on me
                              end
                          |]
                          ]
                          do
                            faSprite_ "group-by" "regular" "w-4 h-4 text-iconNeutral"
                            span_ [] "Group by"
                      li_ $ a_ [class_ "flex gap-2 items-center", onclick_ $ "viewFieldPatterns('" <> T.replace "___" "." key <> "')"] do
                        faSprite_ "chart-bar" "regular" "w-4 h-4 text-iconNeutral"
                        span_ [] "View patterns"

                -- Render facet values (uses group-has-checked to show/hide)
                div_ [class_ "facet-values pl-7 pr-2 mb-1 space-y-1 max-h-0 overflow-hidden group-has-[:checked]/facet:max-h-[1000px] transition-[max-height] duration-200"] do
                  let valuesWithIndices = zip [0 ..] values
                      (visibleValues, hiddenValues) = splitAt 5 valuesWithIndices
                      hiddenCount = length hiddenValues
                      -- Helper function to render a facet value item
                      renderFacetValue (FacetValue val count) =
                        label_ [class_ "facet-item flex items-center justify-between py-0.5 max-md:py-1.5 px-1 hover:bg-fillWeak rounded cursor-pointer will-change-[background-color]"] do
                          div_ [class_ "flex items-center gap-2 min-w-0 flex-1"] do
                            input_
                              [ type_ "checkbox"
                              , class_ "checkbox checkbox-xs max-md:checkbox-sm"
                              , name_ key
                              , onclick_ $ "filterByFacet('" <> T.replace "___" "." key <> "', '" <> val <> "')"
                              , term "data-tippy-content" (T.replace "___" "." key <> " == \"" <> val <> "\"")
                              , term "data-field" (T.replace "___" "." key)
                              , term "data-value" val
                              ]

                            let colorClass = colorFn val
                            unless (T.null colorClass) $ span_ [class_ $ colorClass <> " shrink-0 w-0.5 h-3 rounded-sm"] ""
                            span_ [class_ "facet-value truncate text-xs", term "data-tippy-content" val] (toHtml val)

                          span_ [class_ "facet-count text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ prettyPrintCount count
                  -- Render visible values
                  forM_ visibleValues \(_, value) -> renderFacetValue value

                  -- Show more/less toggle for hidden values
                  when (hiddenCount > 0) do
                    let moreId = "more-" <> key
                    input_ [type_ "checkbox", class_ "hidden peer/more", id_ moreId]
                    label_ [class_ "text-textBrand text-xs px-1 py-0.5 cursor-pointer hover:underline", Lucid.for_ moreId] do
                      span_ [class_ "peer-checked/more:hidden"] $ toHtml $ "+ More (" <> prettyPrintCount hiddenCount <> ")"
                      span_ [class_ "hidden peer-checked/more:inline"] $ toHtml $ "- Less (" <> prettyPrintCount hiddenCount <> ")"

                    div_ [class_ "hidden peer-checked/more:block space-y-1"] $ forM_ hiddenValues \(_, value) -> renderFacetValue value


keepNonEmpty :: Maybe Text -> Maybe Text
keepNonEmpty Nothing = Nothing
keepNonEmpty (Just "") = Nothing
keepNonEmpty (Just a) = Just a


-- | Core result builder shared by apiLogH and queryEvents.
buildLogResult :: (DB es, Time.Time :> es) => Projects.ProjectId -> UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> [Text] -> (V.Vector (V.Vector AE.Value), [Text], Int) -> Eff es LogResult
buildLogResult pid now sinceM fromM toM summaryCols (requestVecs, colNames, resultCount') = do
  let colIdxMap = listToIndexHashMap colNames
      reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
      reqFirstCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? 0)
      alreadyLoadedIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "id") requestVecs
      (fromDD, toDD, _) = Components.parseTimeRange now (Components.TimePicker sinceM reqLastCreatedAtM reqFirstCreatedAtM)
  childSpansList <-
    if V.length requestVecs > 100
      then pure [] -- Skip expensive child span fetch for large result sets; traces load lazily on detail view
      else do
        let allTraceIds = V.filter (not . T.null) $ V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
            traceIds = V.fromList $ take 50 $ nubOrd $ V.toList allTraceIds
        LogQueries.selectChildSpansAndLogs pid summaryCols traceIds (fromDD, toDD) alreadyLoadedIds
  let synthRows = synthesizeOrphanHeaders colIdxMap requestVecs
      requestVecsAug = synthRows <> requestVecs
      rawLogsData = requestVecsAug <> V.fromList childSpansList
      cols = nubOrd $ curateCols summaryCols colNames
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
      , hasMore = queryResultCount < resultCount'
      , traces
      }


-- | Standalone query function for the v1 API events endpoint.
-- Returns 400 for parse errors, propagates DB errors instead of silently returning empty results.
queryEvents :: (DB es, ELog.Log :> es, Error Servant.ServerError :> es, Time.Time :> es) => Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Eff es LogResult
queryEvents pid queryM sinceM fromM toM sourceM limitM = do
  now <- Time.currentTime
  let queryInput = fromMaybe "" queryM
  queryAST <- case parseQueryToAST queryInput of
    Left err -> throwError Servant.err400{Servant.errBody = encodeUtf8 $ "Invalid query: " <> err}
    Right ast -> pure ast
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  result <- LogQueries.selectLogTable pid queryAST (toQText queryAST) Nothing (fromD, toD) [] (parseMaybe pSource =<< sourceM) Nothing
  case result of
    Left err -> throwError Servant.err400{Servant.errBody = encodeUtf8 $ "Query failed: " <> err}
    Right r -> do
      lr <- buildLogResult pid now sinceM fromM toM [] r
      let limited = V.take (fromMaybe 100 limitM) lr.logsData
          qrc = V.length limited
      pure lr{logsData = limited, queryResultCount = qrc, hasMore = qrc < lr.count}


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM' cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM queryLibItemTitle queryLibItemID detailWM targetEventM showTraceM hxRequestM hxBoostedM jsonM vizTypeM alertM skipM pTargetM sortByM = do
  (sess, project) <- Projects.sessionAndProject pid
  let source = fromMaybe "spans" sourceM
  let summaryCols = T.splitOn "," (fromMaybe "" cols')
  let queryInput = maybeToMonoid queryM'
  let parseError msg = addTriggerEvent "showParseError" (AE.toJSON msg) >> addErrorToast "Error Parsing Query" (Just msg) $> ([], Just msg)
  (queryAST, parseErrorMsg) <- case parseQueryToAST queryInput of
    Left err -> parseError err
    Right ast
      | not (T.null (T.strip queryInput)) && null ast -> parseError "Invalid query syntax"
      | otherwise -> pure (ast, Nothing)
  let hadParseError = isJust parseErrorMsg
  let queryText = toQText queryAST
      isJsonReq = jsonM == Just "true"

  -- Fire-and-forget: onboarding + query history (skip on JSON fast path since HTML path already does it)
  unless isJsonReq do
    unless (V.elem "explored_logs" project.onboardingStepsCompleted)
      $ void
      $ Hasql.interpExecute [HI.sql| UPDATE projects.projects SET onboarding_steps_completed = array_append(onboarding_steps_completed, 'explored_logs') WHERE id = #{pid} AND NOT ('explored_logs' = ANY(onboarding_steps_completed)) |]
    unless (isJust queryLibItemTitle) $ Projects.queryLibInsert Projects.QLTHistory pid sess.persistentSession.userId queryText queryAST Nothing

  when (layoutM == Just "SaveQuery") do
    if (isJust . keepNonEmpty) queryLibItemID && (isJust . keepNonEmpty) queryLibItemTitle
      then do
        Projects.queryLibTitleEdit pid sess.persistentSession.userId (maybeToMonoid queryLibItemID) (maybeToMonoid queryLibItemTitle)
        addSuccessToast "Edited Query title successfully" Nothing
      else do
        Projects.queryLibInsert Projects.QLTSaved pid sess.persistentSession.userId queryText queryAST queryLibItemTitle
        addSuccessToast "Saved to Query Library successfully" Nothing
    addTriggerEvent "closeModal" ""

  when (layoutM == Just "DeleteQuery") do
    Projects.queryLibItemDelete pid sess.persistentSession.userId (maybeToMonoid queryLibItemID)
    addSuccessToast "Deleted from Query Library successfully" Nothing

  now <- Time.currentTime
  let (fromD, toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  authCtx <- Effectful.Reader.Static.ask @AuthContext

  -- If an alert ID is provided, fetch the alert and pre-fill the query box
  alertDM <- case alertM of
    Nothing -> return Nothing
    Just alertIdText -> case UUID.fromText alertIdText of
      Just alertId -> Monitors.queryMonitorById (Monitors.QueryMonitorId alertId)
      Nothing -> return Nothing

  -- Use alert's visualization type if no vizType specified and alert is loaded
  let effectiveVizType = vizTypeM <|> ((.visualizationType) <$> alertDM)

  -- Skip table load on initial page load unless it's a JSON request
  let shouldSkipLoad = hadParseError || isNothing layoutM && isNothing hxRequestM && jsonM /= Just "true"
      fetchLogs =
        Hasql.withHasqlTimefusion authCtx.env.enableTimefusionReads
          $ LogQueries.selectLogTable pid queryAST queryText cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- JSON fast path: skip side queries (facets, teams, queryLib, patterns)
  let isJsonFastPath = jsonM == Just "true" && layoutM /= Just "SaveQuery"

  let buildLogResult' tableData = do
        lr <- buildLogResult pid now sinceM fromM toM summaryCols tableData
        let lastFM = lr.cursor >>= textToUTC <&> toText . iso8601Show . addUTCTime (-0.001)
        pure
          (lr :: LogResult)
            { nextUrl = LogQueries.logExplorerUrlPath pid queryM' cols' lastFM sinceM fromM toM (Just "loadmore") sourceM False
            , resetLogsUrl = LogQueries.logExplorerUrlPath pid queryM' cols' Nothing Nothing Nothing Nothing Nothing sourceM False
            , recentUrl = LogQueries.logExplorerUrlPath pid queryM' cols' Nothing sinceM fromM toM (Just "loadmore") sourceM True
            }

  let fetchOrSkip = if shouldSkipLoad then pure $ Right (V.empty, ["timestamp", "summary", "duration"], 0) else fetchLogs

  if isJsonFastPath
    then case effectiveVizType of
      Just "patterns" -> do
        (totalPatterns, patternRows) <- LogQueries.fetchLogPatterns authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) (parseMaybe pSource =<< sourceM) pTargetM (fromMaybe 0 skipM)
        addRespHeaders $ LogsPatternJson totalPatterns (V.fromList patternRows)
      Just "sessions" -> do
        (totalSessions, sessionRows) <- LogQueries.fetchSessions authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) sortByM (fromMaybe 0 skipM)
        addRespHeaders $ LogsSessionsJson totalSessions (V.fromList sessionRows)
      _ -> do
        tableAsVecE <- fetchOrSkip
        whenLeft_ (void tableAsVecE) (Log.logAttention "Log explorer JSON query failed" . show @Text)
        case hush tableAsVecE of
          Just tableResult -> buildLogResult' tableResult >>= addRespHeaders . LogsGetJson
          Nothing -> buildLogResult' (V.empty, ["timestamp", "summary", "duration"], 0) >>= addRespHeaders . LogsGetJson
    else do
      -- Full HTML path: parallelize independent DB queries
      (tableAsVecE, queryLibE, facetSummaryE, freeTierStatusE, teamsE, aggregateE) <- Ki.scoped \scope -> do
        let aw = Ki.atomically . Ki.await
        t1 <- Ki.fork scope fetchOrSkip
        t2 <- Ki.fork scope $ tryAny $ Projects.queryLibHistoryForUser pid sess.persistentSession.userId
        t3 <- Ki.fork scope $ tryAny $ Fields.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)
        t4 <- Ki.fork scope $ tryAny $ checkFreeTierStatus pid project.paymentPlan
        t5 <- Ki.fork scope $ tryAny $ V.fromList <$> ManageMembers.getTeams pid
        -- Patterns and sessions are mutually exclusive; a single fork suffices.
        -- The summary is tried independently so its failure doesn't drop the
        -- sessions table with it.
        t6 <- Ki.fork scope $ tryAny $ case effectiveVizType of
          Just "patterns" -> (,Nothing,Nothing) . Just . second V.fromList <$> LogQueries.fetchLogPatterns authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) (parseMaybe pSource =<< sourceM) pTargetM (fromMaybe 0 skipM)
          Just "sessions" -> do
            rows <- second V.fromList <$> LogQueries.fetchSessions authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) sortByM (fromMaybe 0 skipM)
            summE <- tryAny $ LogQueries.fetchSessionSummary authCtx.env.enableTimefusionReads pid queryAST (fromD, toD)
            whenLeft_ summE (Log.logAttention "fetchSessionSummary failed" . show @Text)
            pure (Nothing, Just rows, Just $ first (show @Text) summE)
          _ -> pure (Nothing, Nothing, Nothing)
        (,,,,,) <$> aw t1 <*> aw t2 <*> aw t3 <*> aw t4 <*> aw t5 <*> aw t6

      -- Log errors from non-critical queries but continue with defaults
      let logErr label res = whenLeft_ (void res) (Log.logAttention ("Log explorer " <> label <> " failed") . show @Text)
      logErr "queryLib" queryLibE
      logErr "facets" facetSummaryE
      logErr "freeTierStatus" freeTierStatusE
      logErr "teams" teamsE
      logErr "aggregate" aggregateE

      let tableAsVecM = hush tableAsVecE
          queryLib = fromRight [] queryLibE
          facetSummary = join $ rightToMaybe facetSummaryE
          freeTierStatus = fromRight def freeTierStatusE
          teams = fromRight V.empty teamsE
          (patterns, sessions, sessionSummary) = fromRight (Nothing, Nothing, Nothing) aggregateE
          (queryLibRecent, queryLibSaved) = bimap V.fromList V.fromList $ L.partition (\x -> Projects.QLTHistory == x.queryType) queryLib

      -- Queue facet generation if no precomputed facets exist (new projects)
      when (isNothing facetSummary)
        $ liftIO
        $ withResource authCtx.jobsPool \conn ->
          void $ createJob conn "background_jobs" $ BackgroundJobs.GenerateOtelFacetsBatch (V.singleton pid) now

      -- Build preload URL using the same function that builds the JSON URLs
      let preloadUrl = T.replace "\"" "%22" $ LogQueries.logExplorerUrlPath pid queryM' cols' (formatUTC <$> cursorM') sinceM fromM toM Nothing sourceM False
          headContent = Just $ do
            script_ [text|window.logDataPromise = fetch("$preloadUrl", {headers: {Accept: "application/json"}, credentials: "include"}).then(r => r.json());|]

      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = Just project
              , pageTitle = "Explorer"
              , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/api-log-explorer/"
              , freeTierStatus = freeTierStatus
              , config = authCtx.config
              , headContent = headContent
              , pageActions = Just $ div_ [class_ "flex gap-2 max-md:gap-1 items-center"] do
                  label_ [class_ "cursor-pointer border border-strokeWeak rounded-lg flex shadow-xs", role_ "switch", Aria.label_ "Stream live data", [__|on change from #streamLiveData if #streamLiveData.checked set @aria-checked to 'true' else set @aria-checked to 'false'|], term "aria-checked" "false"] do
                    input_ [type_ "checkbox", id_ "streamLiveData", class_ "hidden"]
                    span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-2 items-center", data_ "tippy-content" "Pause live stream"] $ faSprite_ "pause" "solid" "h-4 w-4 text-iconNeutral"
                    span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex py-1 px-2 items-center", data_ "tippy-content" "Stream live data"] $ faSprite_ "play" "regular" "h-4 w-4 text-iconNeutral"
                  Components.timepicker_ (Just "log_explorer_form") currentRange Nothing
                  Components.refreshButton_
              , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline items-center", role_ "tablist"] do
                  a_
                    ([href_ $ "/p/" <> pid.toText <> "/log_explorer", role_ "tab", class_ "tab h-auto! tab-active text-textStrong", term "aria-current" "page", term "aria-selected" "true"] <> navTabAttrs)
                    "Events"
                  a_ ([href_ $ "/p/" <> pid.toText <> "/metrics", role_ "tab", class_ "tab h-auto! ", term "aria-selected" "false"] <> navTabAttrs) "Metrics"
              }

      case tableAsVecM of
        Just tableResult -> do
          r <- buildLogResult' tableResult
          let patternsToSkip = fromMaybe 0 skipM + maybe 0 (V.length . snd) patterns
              sessionsToSkip = fromMaybe 0 skipM + maybe 0 (V.length . snd) sessions

          -- Build widgets with PNG URLs
          let baseChartWidget = logChartWidget pid
              baseLatencyWidget = logLatencyWidget pid
          chartPngUrl <- Widget.widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid baseChartWidget sinceM fromM toM
          latencyPngUrl <- Widget.widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid baseLatencyWidget sinceM fromM toM
          let chartWidget = if T.null chartPngUrl then baseChartWidget else baseChartWidget{Widget.pngUrl = Just chartPngUrl}
              latencyWidget = if T.null latencyPngUrl then baseLatencyWidget else baseLatencyWidget{Widget.pngUrl = Just latencyPngUrl}

          let page =
                ApiLogsPageData
                  { pid
                  , resultCount = r.count
                  , requestVecs = r.logsData
                  , cols = r.cols
                  , colIdxMap = r.colIdxMap
                  , nextLogsURL = r.nextUrl
                  , resetLogsURL = r.resetLogsUrl
                  , recentLogsURL = r.recentUrl
                  , currentRange
                  , exceededFreeTier = case freeTierStatus of FreeTierExceeded _ _ -> True; _ -> False
                  , query = queryM'
                  , cursor = r.cursor
                  , isTestLog = Nothing
                  , emptyStateUrl = Nothing
                  , source
                  , targetSpans = targetSpansM
                  , serviceColors = r.serviceColors
                  , daysCountDown = Nothing
                  , queryLibRecent
                  , queryLibSaved
                  , fromD
                  , toD
                  , detailsWidth = detailWM
                  , targetEvent = targetEventM
                  , showTrace = showTraceM
                  , facets = facetSummary
                  , vizType = effectiveVizType
                  , alert = alertDM
                  , patterns = patterns
                  , patternsToSkip
                  , sessions = sessions
                  , sessionsToSkip
                  , sessionSummary
                  , targetPattern = pTargetM
                  , project = project
                  , teams
                  , chartWidget
                  , latencyWidget
                  , queryResultCount = r.queryResultCount
                  , parseError = parseErrorMsg
                  }

          addRespHeaders $ case (layoutM, hxRequestM, jsonM, effectiveVizType) of
            (_, _, Just "true", Just "patterns") -> let (tp, ps) = fromMaybe (0, V.empty) patterns in LogsPatternJson tp ps
            (_, _, Just "true", Just "sessions") -> let (ts, ss) = fromMaybe (0, V.empty) sessions in LogsSessionsJson ts ss
            (Just "SaveQuery", _, _, _) -> LogsQueryLibrary pid queryLibSaved queryLibRecent
            (Just "resultTable", Just "true", _, _) -> LogsGetJson r
            (Just "all", Just "true", _, _) -> LogsGetJson r
            (_, _, Just "true", _) -> LogsGetJson r
            _ -> LogPage $ PageCtx bwconf page
        Nothing -> do
          case (layoutM, hxRequestM, hxBoostedM, jsonM) of
            (_, _, _, Just "true") -> do
              addErrorToast "Something went wrong" Nothing
              addRespHeaders $ LogsGetErrorSimple "Failed to fetch logs data"
            _ -> do
              addErrorToast "Something went wrong" Nothing
              addRespHeaders $ LogsGetError $ PageCtx bwconf "Something went wrong"


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
  | LogsGetJson LogResult
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)
  | LogsPatternJson Int (V.Vector LogQueries.PatternRow)
  | LogsSessionsJson Int (V.Vector LogQueries.SessionRow)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-textError"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtml x@LogsGetJson{} = span_ [] . toHtml $ (decodeUtf8 $ AE.encode x :: Text)
  toHtml x@LogsPatternJson{} = span_ [] . toHtml $ (decodeUtf8 $ AE.encode x :: Text)
  toHtml x@LogsSessionsJson{} = span_ [] . toHtml $ (decodeUtf8 $ AE.encode x :: Text)
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogsGetJson r) = AE.toJSON r
  toJSON (LogsPatternJson totalPatterns patterns) =
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
        allCols = cols ++ ["merged_count"] :: [Text]
        rows = V.map (\p -> AE.Array $ V.fromList [AE.Null, AE.toJSON p.count, AE.toJSON p.volume, AE.toJSON p.level, AE.toJSON p.service, patternToSummary p.logPattern, AE.toJSON p.mergedCount]) patterns
        total = V.foldl' (\acc p -> acc + p.count) 0 patterns
     in aggregateEnvelope rows cols allCols total ["totalPatterns" AE..= totalPatterns]
  -- Sessions use the exact same column layout as logs so the same rendering code is reused.
  -- Session-specific info is packed into the summary column as badge elements.
  -- Column indices must match the logs colIdxMap exactly.
  toJSON (LogsSessionsJson totalSessions sessions) =
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
  toJSON (LogsGetError _) = AE.object ["error" AE..= True, "message" AE..= ("Something went wrong" :: Text)]
  toJSON (LogsGetErrorSimple msg) = AE.object ["error" AE..= True, "message" AE..= msg]
  toJSON _ = AE.object ["error" AE..= True]


-- This component has been moved to Pkg.Components.LogQueryBox

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


data ApiLogsPageData = ApiLogsPageData
  { pid :: Projects.ProjectId
  , resultCount :: Int
  , requestVecs :: V.Vector (V.Vector AE.Value)
  , cols :: [Text]
  , colIdxMap :: HM.HashMap Text Int
  , nextLogsURL :: Text
  , resetLogsURL :: Text
  , recentLogsURL :: Text
  , currentRange :: Maybe (Text, Text)
  , exceededFreeTier :: Bool
  , query :: Maybe Text
  , cursor :: Maybe Text
  , isTestLog :: Maybe Bool
  , emptyStateUrl :: Maybe Text
  , source :: Text
  , targetSpans :: Maybe Text
  , serviceColors :: HM.HashMap Text Text
  , daysCountDown :: Maybe Text
  , queryLibRecent :: V.Vector Projects.QueryLibItem
  , queryLibSaved :: V.Vector Projects.QueryLibItem
  , fromD :: Maybe UTCTime
  , toD :: Maybe UTCTime
  , detailsWidth :: Maybe Text
  , targetEvent :: Maybe Text
  , showTrace :: Maybe Text
  , facets :: Maybe FacetSummary
  , vizType :: Maybe Text
  , alert :: Maybe Monitors.QueryMonitor
  , patterns :: Maybe (Int, V.Vector LogQueries.PatternRow)
  , patternsToSkip :: Int
  , sessions :: Maybe (Int, V.Vector LogQueries.SessionRow)
  , sessionsToSkip :: Int
  , sessionSummary :: Maybe (Either Text LogQueries.SessionSummary)
  , targetPattern :: Maybe Text
  , project :: Projects.Project
  , teams :: V.Vector ManageMembers.Team
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


instance ToSchema LogResult where
  declareNamedSchema _ = do
    traceRef <- OA.declareSchemaRef (Proxy @TraceTreeEntry)
    let prop t = Inline $ mempty & OA.type_ ?~ t
        propDesc t d = Inline $ mempty & OA.type_ ?~ t & OA.description ?~ d
        arrOf ref = Inline $ mempty & OA.type_ ?~ OpenApiArray & OA.items ?~ OpenApiItemsObject ref
    pure
      $ NamedSchema (Just "LogResult")
      $ mempty
      & OA.type_
      ?~ OpenApiObject
        & OA.properties
      .~ fromList
        [ ("logsData", propDesc OpenApiArray "Array of row arrays, each row is an array of values")
        , ("cols", arrOf (Inline $ mempty & OA.type_ ?~ OpenApiString))
        , ("colIdxMap", propDesc OpenApiObject "Column name to index mapping")
        , ("cursor", prop OpenApiString)
        , ("nextUrl", prop OpenApiString)
        , ("resetLogsUrl", prop OpenApiString)
        , ("recentUrl", prop OpenApiString)
        , ("serviceColors", propDesc OpenApiObject "Service name to color hex mapping")
        , ("count", prop OpenApiInteger)
        , ("queryResultCount", prop OpenApiInteger)
        , ("hasMore", prop OpenApiBoolean)
        , ("traces", arrOf traceRef)
        ]


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


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 max-md:px-2 px-4 gap-3.5 max-md:gap-2 w-full flex flex-col h-full overflow-y-hidden overflow-x-hidden pb-2 group/pg", id_ "apiLogsPage"] do
    template_ [id_ "loader-tmp"] $ loadingIndicator_ LdMD LdDots

    div_ [class_ "fixed z-[9999] hidden right-0 w-max h-max border rounded top-32 bg-bgBase shadow-lg", id_ "sessionPlayerWrapper"] do
      termRaw "session-replay" [id_ "sessionReplay", class_ "shrink-1 flex flex-col", term "projectId" page.pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)
    div_
      [ style_ "z-index:26"
      , class_ "fixed hidden right-0 top-0 justify-end left-0 bottom-0 w-full bg-black bg-opacity-5"
      , [__|on click remove .show-log-modal from #expand-log-modal|]
      , id_ "expand-log-modal"
      ]
      do
        div_ [class_ "relative ml-auto w-full", style_ ""] do
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
    let countText = prettyPrintCount page.queryResultCount
        suffixText = if page.queryResultCount >= page.resultCount then " rows" else "+ rows"
    div_ [class_ "w-full"] do
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
              label_ [class_ "gap-1 flex items-center cursor-pointer text-textWeak"] do
                faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
                span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
                span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
                "filters"
                input_ [type_ "checkbox", class_ "toggle-filters hidden", id_ "toggle-filters-mobile", onchange_ "document.getElementById('toggle-filters').checked = this.checked; document.getElementById('toggle-filters').dispatchEvent(new Event('change'));"]
                script_ "if(window.innerWidth<768) document.getElementById('toggle-filters-mobile').checked=true;"
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
    div_ [class_ "flex max-md:flex-col h-full overflow-y-hidden max-md:overflow-y-auto", id_ "facets_and_loglist"] do
      -- FACETS
      div_ [class_ "w-68 will-change-[width] contain-[layout_style] text-sm text-textWeak shrink-0 flex flex-col h-full overflow-y-scroll gap-2 max-md:w-full max-md:shrink max-md:max-h-48 max-md:border-b max-md:border-strokeWeak group-has-[.toggle-filters:checked]/pg:max-w-0 group-has-[.toggle-filters:checked]/pg:overflow-hidden max-md:group-has-[.toggle-filters:checked]/pg:max-h-0", id_ "facets-container"] do
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

      div_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden max-md:hidden mr-3.5"] $ resizer_ "facets-container" "facets_width" True

      let dW = fromMaybe "100%" page.detailsWidth
          showTrace = isJust page.showTrace
      div_ [class_ "grow will-change-[width] contain-[layout_style] relative flex flex-col shrink-1 min-w-0 w-full h-full ", style_ $ "xwidth: " <> dW, id_ "logs_list_container"] do
        -- Filters and row count header
        div_ [class_ "flex gap-2 py-1 text-sm z-10 w-max bg-bgBase -mb-6 group-has-[#viz-patterns:checked]/pg:mb-0"] do
          label_ [class_ "gap-1 flex items-center cursor-pointer text-textWeak"] do
            faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
            span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
            span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
            "filters"
            input_ [type_ "checkbox", class_ "toggle-filters hidden", id_ "toggle-filters", onchange_ "localStorage.setItem('toggle-filter-checked', this.checked); var m=document.getElementById('toggle-filters-mobile'); if(m) m.checked=this.checked; setTimeout(() => { const editor = document.getElementById('filterElement'); if (editor && editor.refreshLayout) editor.refreshLayout(); }, 200);"]
            script_
              [text|
              if (window.innerWidth < 768) document.getElementById('toggle-filters').checked = true;
              else document.getElementById('toggle-filters').checked = localStorage.getItem('toggle-filter-checked') === 'true';
              // Ensure editor layout is correct on initial load
              setTimeout(() => {
                const editor = document.getElementById('filterElement');
                if (editor && editor.refreshLayout) {
                  editor.refreshLayout();
                }
              }, 300);
            |]
          span_ [class_ "text-strokeWeak "] "|"
          rowCountDisplay_ "" countText suffixText

        -- Visualization widget that shows when not in logs view (skip for patterns mode which uses log-list)
        div_ [class_ "flex-1 min-h-0 h-full group-has-[#viz-logs:checked]/pg:hidden group-has-[#viz-patterns:checked]/pg:hidden group-has-[#viz-sessions:checked]/pg:hidden"] do
          let pid = page.pid.toText
          let vizType = maybe "\"timeseries\"" show page.vizType
          script_
            [text| var widgetJSON = {
                  "id": "visualization-widget",
                  "type": ${vizType}, 
                  "title": "Visualization",
                  "standalone": true,
                  "allow_zoom": true,
                  "_project_id": "$pid",
                  "_center_title": true, 
                  "layout": {"w": 6, "h": 4}
                };
                |]
          div_
            [ id_ "visualization-widget-container"
            , class_ " w-full"
            , style_ "aspect-ratio: 4 / 2;"
            , hxPost_ ("/p/" <> page.pid.toText <> "/widget")
            , hxTrigger_ "intersect once, update-widget"
            , hxTarget_ "this"
            , hxSwap_ "innerHTML"
            , hxVals_ "js:{...widgetJSON}"
            , hxExt_ "json-enc,forward-page-params"
            , term "hx-sync" "this:replace"
            ]
            ""

        -- Trace view container
        div_ [class_ $ "absolute top-0 right-0  w-full h-full overflow-scroll c-scroll z-50 bg-bgBase transition-all duration-100 " <> if showTrace then "" else "hidden", id_ "trace_expanded_view"] do
          whenJust page.showTrace \trIdAndTimestamp -> do
            let url = "/p/" <> page.pid.toText <> "/traces/" <> trIdAndTimestamp
            loadingIndicator_ LdMD LdDots
            div_ [hxGet_ url, hxTarget_ "#trace_expanded_view", hxSwap_ "innerHtml", hxTrigger_ "intersect one", term "hx-sync" "this:replace"] pass

        -- Logs view section (also within the scrollable container)
        div_ [class_ "flex-1 min-h-0 h-full flex flex-col"] do
          -- Virtual table for logs
          div_ [class_ "flex-1 min-h-0 hidden h-full group-has-[#viz-logs:checked]/pg:block group-has-[#viz-patterns:checked]/pg:block group-has-[#viz-sessions:checked]/pg:block"] $ virtualTable page.pid Nothing Nothing

      -- Alert configuration panel on the right
      div_ [class_ "hidden group-has-[#create-alert-toggle:checked]/pg:block max-md:hidden ml-3.5"] $ resizer_ "alert_container" "alert_width" False

      div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll hidden group-has-[#create-alert-toggle:checked]/pg:block w-[500px] max-md:w-full max-md:fixed max-md:inset-0 max-md:z-50 max-md:max-w-full", id_ "alert_container"] do
        alertConfigurationForm_ page.project page.alert page.teams

      div_ [class_ $ "transition-opacity duration-200 hidden max-md:hidden ml-3.5 " <> if isJust page.targetEvent then "group-has-[#viz-logs:checked]/pg:block group-has-[#viz-sessions:checked]/pg:block" else "", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False

      div_ [class_ "grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0 max-w-0 overflow-hidden group-has-[#viz-logs:checked]/pg:max-w-full group-has-[#viz-logs:checked]/pg:overflow-y-auto group-has-[#viz-sessions:checked]/pg:max-w-full group-has-[#viz-sessions:checked]/pg:overflow-y-auto max-md:hidden max-md:[&.details-open]:block! max-md:[&.details-open]:fixed max-md:[&.details-open]:inset-0 max-md:[&.details-open]:z-40 max-md:[&.details-open]:w-full max-md:[&.details-open]:max-w-full max-md:[&.details-open]:bg-bgBase", id_ "log_details_container", term "hx-on::after-swap" "if(window.innerWidth<768)this.classList.add('details-open')"] do
        htmxOverlayIndicator_ "details_indicator"
        whenJust page.targetEvent \te -> do
          script_
            [text|
            document.addEventListener('DOMContentLoaded', function() {
              const detailsContainer = document.getElementById('log_details_container');
              if (detailsContainer) {
                if (window.innerWidth < 768) detailsContainer.classList.add('details-open');
                const queryWidth = new URLSearchParams(window.location.search).get('details_width');
                const storedWidth = localStorage.getItem('resizer-details_width');

                if (queryWidth) detailsContainer.style.width = queryWidth + 'px';
                else if (storedWidth && !storedWidth.endsWith('px')) detailsContainer.style.width = storedWidth + 'px';
                else if (storedWidth) detailsContainer.style.width = storedWidth;
                else detailsContainer.style.width = '30%';
              }
              });
          |]
          let url = "/p/" <> page.pid.toText <> "/log_explorer/" <> te
          div_ [hxGet_ url, hxTarget_ "#log_details_container", hxSwap_ "innerHtml", hxTrigger_ "intersect one", hxIndicator_ "#details_indicator", term "hx-sync" "this:replace"] pass

  queryEditorInitializationCode page.queryLibRecent page.queryLibSaved page.vizType ((.facetJson) <$> page.facets)


-- | Inline-expand endpoint for the Sessions and Patterns visualizations.
-- Returns up to @limitN@ example events that belong to a given session
-- (@kind=session@) or that match a given pattern template (@kind=pattern@),
-- plus a @hasMore@ flag for pagination.
apiLogExpandH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AE.Value)
apiLogExpandH pid kindM keyM skipM queryM sinceM fromM toM = do
  _ <- Projects.sessionAndProject pid
  let skip = fromMaybe 0 skipM
      key = fromMaybe "" keyM
      isSessionKind = kindM == Just "session"
      limitN = (if isSessionKind then 100 else 20) :: Int
      fetchLimit = limitN + 1
  when (T.null key) $ throwError Servant.err400{Servant.errBody = "Missing key"}
  queryAST <- case parseQueryToAST (fromMaybe "" queryM) of
    Left err -> throwError Servant.err400{Servant.errBody = encodeUtf8 $ "Invalid query: " <> err}
    Right ast -> pure ast
  now <- Time.currentTime
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  expandKind <- case kindM of
    Just "session" -> pure $ LogQueries.ExpandSession key
    Just "pattern" -> pure $ LogQueries.ExpandPattern key
    _ -> throwError Servant.err400{Servant.errBody = "kind must be session or pattern"}
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  (rows, cols) <- LogQueries.fetchEventExamples authCtx.env.enableTimefusionReads pid queryAST (fromD, toD) expandKind skip fetchLimit
  let hasMore = V.length rows > limitN
      shown = if hasMore then V.take limitN rows else rows
      colIdxMap = listToIndexHashMap cols
      isSession = case expandKind of LogQueries.ExpandSession _ -> True; _ -> False
      alreadyLoadedIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "id") shown
  -- Only fetch child spans for sessions (trace tree view); patterns just show flat examples
  childSpansList <-
    if not isSession
      then pure []
      else do
        let allTraceIds = V.filter (not . T.null) $ V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") shown
            traceIds = V.fromList $ take 100 $ nubOrd $ V.toList allTraceIds
        LogQueries.selectChildSpansAndLogs pid [] traceIds (fromD, toD) alreadyLoadedIds
  let rawLogsData = shown <> V.fromList childSpansList
      queryResultCount = V.length shown
      (logsData, traces) = buildTraceTree colIdxMap queryResultCount rawLogsData
      displayCols = curateCols [] cols
  addRespHeaders
    $ AE.object
      [ "cols" AE..= displayCols
      , "rows" AE..= logsData
      , "hasMore" AE..= hasMore
      , "colIdxMap" AE..= colIdxMap
      , "traces" AE..= traces
      , "queryResultCount" AE..= queryResultCount
      ]


aiSearchH :: Projects.ProjectId -> AE.Value -> ATAuthCtx (RespHeaders AE.Value)
aiSearchH pid requestBody = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  now <- Time.currentTime
  let envCfg = authCtx.env

  let parsed = AET.parseMaybe (AE.withObject "request" \o -> liftA2 (,) (o AE..: "input") (o AE..:? "timezone")) requestBody
  case parsed of
    Nothing -> do
      addErrorToast "Invalid AI search input" Nothing
      throwError Servant.err400{Servant.errBody = "Invalid input format"}
    Just (inputText, timezoneM) ->
      if T.null (T.strip inputText)
        then do
          addErrorToast "Please enter a search query" Nothing
          throwError Servant.err400{Servant.errBody = "Empty input"}
        else do
          -- Fetch precomputed facets for context (last 24 hours)
          let dayAgo = addUTCTime (-86400) now
          facetSummaryM <- Fields.getFacetSummary pid "otel_logs_and_spans" dayAgo now
          let config = (AI.defaultAgenticConfig pid){AI.facetContext = facetSummaryM, AI.timezone = timezoneM, AI.maxIterations = 2}
          result <- AI.runAgenticQuery config inputText envCfg.openaiModel envCfg.openaiApiKey

          case result of
            Left errMsg -> do
              addErrorToast "AI search failed" (Just errMsg)
              throwError Servant.err502{Servant.errBody = encodeUtf8 errMsg}
            Right resp -> do
              addRespHeaders
                $ AE.object
                  [ "query" AE..= resp.query
                  , "visualization_type" AE..= resp.visualization
                  , "commentary" AE..= resp.explanation
                  , "time_range" AE..= resp.timeRange
                  ]


curateCols :: [Text] -> [Text] -> [Text]
curateCols summaryCols cols = sortBy sortAccordingly filteredCols
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
    filteredCols = filter (\c -> c `notElem` defaultSummaryPaths || c `elem` summaryCols) cols

    sortAccordingly :: Text -> Text -> Ordering
    sortAccordingly a b
      | a == "id" = LT
      | b == "id" = GT
      | a == "timestamp" && b /= "id" = LT
      | b == "timestamp" && a /= "id" = GT
      | a == "latency_breakdown" = GT
      | b == "latency_breakdown" = LT
      | otherwise = comparing (`L.elemIndex` filteredCols) a b


-- | Render alert configuration form for creating log-based alerts
alertConfigurationForm_ :: Projects.Project -> Maybe Monitors.QueryMonitor -> V.Vector ManageMembers.Team -> Html ()
alertConfigurationForm_ project alertM teams = do
  let pid = project.id
  div_ [class_ "surface-raised h-full flex flex-col group/alt"] do
    -- Header section (more compact)
    div_ [class_ "flex items-center justify-between px-4 py-2.5"] do
      div_ [class_ "flex items-center gap-2.5"] do
        div_ [class_ "w-8 h-8 rounded-full bg-fillBrand-weak flex items-center justify-center shrink-0"]
          $ faSprite_ "bell" "regular" "w-4 h-4 text-iconBrand"
        div_ [] do
          h3_ [class_ "text-base font-semibold text-textStrong"] "Create monitor"
          p_ [class_ "text-xs text-textWeak hidden sm:block"] "Get notified when your query matches specific conditions"
      -- Close button only
      button_
        [ type_ "button"
        , class_ "p-1 rounded-lg hover:bg-fillWeak transition-colors"
        , [__|on click set #create-alert-toggle.checked to false|]
        ]
        $ faSprite_ "xmark" "regular" "w-3 h-3 text-iconNeutral"

    -- Form content wrapper with scrolling
    div_ [class_ "p-4 pt-3 flex-1 overflow-y-auto c-scroll"] do
      form_
        [ id_ "alert-form"
        , hxPost_ $ "/p/" <> pid.toText <> "/monitors/alerts"
        , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, source: params().source || 'spans', vizType: getVizType(), teams: window.getTagValues('#alert-form-teams')}"
        , hxSwap_ "none"
        , class_ "flex flex-col gap-3"
        , [__|on htmx:afterRequest
             if event.detail.successful
               set my value to ''
               call me.reset()
             end
          |]
        ]
        do
          input_ [type_ "hidden", name_ "alertId", value_ $ maybe "" ((.id.toText)) alertM]
          formField_ FieldSm def{value = maybe "" (\x -> x.alertConfig.title) alertM, placeholder = "e.g. High error rate on checkout API"} "Name" "title" True Nothing

          -- Monitor Schedule section (shared component)
          let defaultFrequency = maybe 5 (.checkIntervalMins) alertM
              conditionType = if maybe True (\x -> x.alertThreshold > 0 && isJust x.warningThreshold) alertM then Just "threshold_exceeded" else Just "has_matches"
          AlertUI.monitorScheduleSection_ project.paymentPlan defaultFrequency 5 conditionType Nothing

          -- Thresholds section (shared component)
          AlertUI.thresholdsSection_ Nothing (fmap (.alertThreshold) alertM) ((.warningThreshold) =<< alertM) (maybe False (.triggerLessThan) alertM) ((.alertRecoveryThreshold) =<< alertM) ((.warningRecoveryThreshold) =<< alertM)

          -- Notification Settings section (shared component)
          let selectedTeamIds = maybe V.empty (.teams) alertM
          AlertUI.notificationSettingsSection_ ((.alertConfig.severity) <$> alertM) ((.alertConfig.subject) <$> alertM) ((.alertConfig.message) <$> alertM) (maybe True (.alertConfig.emailAll) alertM) teams selectedTeamIds "alert-form" alertM

          -- Action buttons with proper spacing
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
