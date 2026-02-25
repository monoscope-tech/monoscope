module Pages.LogExplorer.Log (
  apiLogH,
  aiSearchH,
  LogsGet (..),
  LogResult (..),
  ApiLogsPageData (..),
  virtualTable,
  curateCols,
  logQueryBox_,
  TraceTreeEntry (..),
  buildTraceTree,
)
where

import Control.Error (hush)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime)
import Data.Vector qualified as V
import Effectful.Error.Static (throwError)
import Effectful.Labeled (labeled)
import Effectful.PostgreSQL qualified as PG
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Fields (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.Fields qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), currProject, pageActions, pageTitle, sessM)
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
import Utils (LoadingSize (..), LoadingType (..), checkFreeTierExceeded, faSprite_, formatUTC, getServiceColors, htmxOverlayIndicator_, levelFillColor, listToIndexHashMap, loadingIndicator_, lookupVecTextByKey, methodFillColor, prettyPrintCount, statusFillColorText)

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
import Data.Pool (withResource)
import Data.Scientific (toBoundedInteger)
import Deriving.Aeson qualified as DAE
import Effectful.Ki qualified as Ki
import OddJobs.Job (createJob)
import Pages.Bots.Utils qualified as BotUtils


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


-- Internal type for span info extraction
data SpanInfo = SpanInfo {spanId :: Text, parentId :: Maybe Text, traceIdVal :: Text, startNs :: Int64, dur :: Int64, timestamp :: Maybe Text, isQueryResult :: Bool}


-- | Build trace tree from flat rows. Query-result spans (index < queryResultCount)
-- become roots. Non-query-result spans (fetched via selectChildSpansAndLogs) form
-- the tree beneath them.
--
-- >>> import Relude
-- >>> import Data.Vector qualified as V
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.HashMap.Strict qualified as HM
-- >>> let colIdx = HM.fromList [("id",0),("trace_id",1),("parent_id",2),("start_time_ns",3),("duration",4),("latency_breakdown",5),("kind",6),("errors",7),("timestamp",8)]
-- >>> let row1 = V.fromList [AE.String "s1", AE.String "t1", AE.Null, AE.Number 100, AE.Number 1000, AE.String "lb1", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:00Z"]
-- >>> let row2 = V.fromList [AE.String "s2", AE.String "t1", AE.String "lb1", AE.Number 200, AE.Number 500, AE.String "lb2", AE.String "span", AE.Null, AE.String "2025-01-01T00:00:01Z"]
-- >>> let vecs = V.fromList [row1, row2]
-- >>> let result = buildTraceTree colIdx 1 vecs
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
-- >>> let r2 = buildTraceTree colIdx2 1 (V.fromList [qr, ch, orph])
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
-- >>> let r = buildTraceTree colIdx 1 rows
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
-- >>> let r = buildTraceTree colIdx 1 (V.fromList [rootSpan, childSpan, logEntry])
-- >>> length r
-- 1
-- >>> Map.lookup "root-span" . (.children) =<< viaNonEmpty head r
-- Just ["child-span"]
-- >>> Map.lookup "child-span" . (.children) =<< viaNonEmpty head r
-- Just ["log1"]
buildTraceTree :: HM.HashMap Text Int -> Int -> V.Vector (V.Vector AE.Value) -> [TraceTreeEntry]
buildTraceTree colIdxMap queryResultCount rows = sortWith (Down . (.startTime)) entries
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
       in SpanInfo sid pid tid sns d ts (idx < queryResultCount)

    spanInfos = V.imap mkSpanInfo rows

    grouped :: Map.Map Text [SpanInfo]
    grouped = Map.fromListWith (<>) [(si.traceIdVal, [si]) | si <- V.toList spanInfos]

    entries = concatMap buildTraceEntries (Map.elems grouped)

    buildTraceEntries :: [SpanInfo] -> [TraceTreeEntry]
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

    buildEntry :: Text -> Map.Map Text [Text] -> Map.Map Text SpanInfo -> Maybe Text -> SpanInfo -> TraceTreeEntry
    buildEntry tid fullChildrenMap spanMap tst root' =
      let go [] minS maxE acc = (minS, maxE, acc)
          go (x : xs) minS maxE acc = case Map.lookup x spanMap of
            Nothing -> go xs minS maxE acc
            Just si ->
              let ce = si.startNs + si.dur
                  kids = fromMaybe [] (Map.lookup x fullChildrenMap)
                  newAcc = if null kids then acc else Map.insert x kids acc
               in go (kids ++ xs) (min minS si.startNs) (max maxE ce) newAcc
          rootKids = fromMaybe [] (Map.lookup root'.spanId fullChildrenMap)
          initAcc = Map.fromList [(root'.spanId, rootKids) | not (null rootKids)]
          (minStart, maxEnd, subtreeChildren) = go rootKids root'.startNs (root'.startNs + root'.dur) initAcc
       in TraceTreeEntry tid minStart (maxEnd - minStart) tst root'.spanId subtreeChildren


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


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
        , ("resource___service___name", "Service", const "")
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
        label_ [class_ "p-2 bg-fillWeak rounded-lg cursor-pointer flex gap-2 items-center peer-checked:[&>svg]:rotate-0", Lucid.for_ $ "toggle-" <> T.replace " " "-" sectionName] do
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
                    a_ [tabindex_ "0", class_ "cursor-pointer p-2 hover:bg-fillWeak rounded"] do
                      faSprite_ "ellipsis-vertical" "regular" "w-3 h-3"
                    ul_ [tabindex_ "0", class_ "dropdown-content z-10 menu p-2 shadow bg-base-100 rounded-box w-52"] do
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
                        label_ [class_ "facet-item flex items-center justify-between py-0.5 px-1 hover:bg-fillWeak rounded cursor-pointer will-change-[background-color]"] do
                          div_ [class_ "flex items-center gap-2 min-w-0 flex-1"] do
                            input_
                              [ type_ "checkbox"
                              , class_ "checkbox checkbox-xs"
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


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM' cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM queryLibItemTitle queryLibItemID detailWM targetEventM showTraceM hxRequestM hxBoostedM jsonM vizTypeM alertM skipM pTargetM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let source = fromMaybe "spans" sourceM
  let summaryCols = T.splitOn "," (fromMaybe "" cols')
  let queryInput = maybeToMonoid queryM'
  (queryAST, hadParseError) <- case parseQueryToAST queryInput of
    Left err -> addErrorToast "Error Parsing Query" (Just err) >> pure ([], True)
    Right ast
      | not (T.null (T.strip queryInput)) && null ast -> addErrorToast "Error Parsing Query" (Just "Invalid query syntax") >> pure ([], True)
      | otherwise -> pure (ast, False)
  let queryText = toQText queryAST

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
  let shouldSkipLoad = hadParseError || isNothing layoutM && isNothing hxRequestM && jsonM /= Just "true" || effectiveVizType == Just "patterns"
      fetchLogs =
        if authCtx.env.enableTimefusionReads
          then labeled @"timefusion" @PG.WithConnection $ RequestDumps.selectLogTable pid queryAST queryText cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM
          else RequestDumps.selectLogTable pid queryAST queryText cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- JSON fast path: skip side queries (facets, teams, queryLib, patterns)
  let isJsonFastPath =
        jsonM == Just "true"
          && effectiveVizType /= Just "patterns"
          && layoutM /= Just "SaveQuery"

  let buildLogResult (requestVecs, colNames, resultCount') = do
        let colIdxMap = listToIndexHashMap colNames
            reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
            reqFirstCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? 0)
            traceIds = V.filter (not . T.null) $ V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
            alreadyLoadedIds = V.mapMaybe (\v -> lookupVecTextByKey v colIdxMap "id") requestVecs
            (fromDD, toDD, _) = Components.parseTimeRange now (Components.TimePicker sinceM reqLastCreatedAtM reqFirstCreatedAtM)
        childSpansList <- RequestDumps.selectChildSpansAndLogs pid summaryCols traceIds (fromDD, toDD) alreadyLoadedIds
        let finalVecs = requestVecs <> V.fromList childSpansList
            curatedColNames = nubOrd $ curateCols summaryCols colNames
            lastFM = reqLastCreatedAtM >>= textToUTC >>= Just . toText . iso8601Show . addUTCTime (-0.001)
            nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' lastFM sinceM fromM toM (Just "loadmore") sourceM False
            resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' Nothing Nothing Nothing Nothing Nothing sourceM False
            recentLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' Nothing sinceM fromM toM (Just "loadmore") sourceM True
            colors = getServiceColors $ V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") finalVecs
            queryResultCount = V.length requestVecs
            traces = buildTraceTree colIdxMap queryResultCount finalVecs
        pure
          LogResult
            { finalVecs
            , curatedColNames
            , colIdxMap
            , cursor = reqLastCreatedAtM
            , nextLogsURL
            , resetLogsURL
            , recentLogsURL
            , serviceColors = colors
            , queryResultCount
            , resultCount = resultCount'
            , traces
            }

  let fetchOrSkip = if shouldSkipLoad then pure $ Right (V.empty, ["timestamp", "summary", "duration"], 0) else fetchLogs

  if isJsonFastPath
    then do
      tableAsVecE <- fetchOrSkip
      case hush tableAsVecE of
        Just tableResult -> buildLogResult tableResult >>= addRespHeaders . LogsGetJson
        Nothing -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ LogsGetErrorSimple "Failed to fetch logs data"
    else do
      -- Full HTML path: parallelize independent DB queries
      (tableAsVecE, queryLib, facetSummary, freeTierExceeded, teams, patterns) <- Ki.scoped \scope -> do
        let aw = Ki.atomically . Ki.await
        t1 <- Ki.fork scope fetchOrSkip
        t2 <- Ki.fork scope $ Projects.queryLibHistoryForUser pid sess.persistentSession.userId
        t3 <- Ki.fork scope $ Fields.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)
        t4 <- Ki.fork scope $ checkFreeTierExceeded pid project.paymentPlan
        t5 <- Ki.fork scope $ V.fromList <$> ManageMembers.getTeams pid
        t6 <- Ki.fork scope $ case effectiveVizType of
          Just "patterns" -> Just . V.fromList <$> RequestDumps.fetchLogPatterns pid queryAST (fromD, toD) (parseMaybe pSource =<< sourceM) pTargetM (fromMaybe 0 skipM)
          _ -> pure Nothing
        (,,,,,) <$> aw t1 <*> aw t2 <*> aw t3 <*> aw t4 <*> aw t5 <*> aw t6

      -- FIXME: we're silently ignoring parse errors and the likes.
      let tableAsVecM = hush tableAsVecE
          (queryLibRecent, queryLibSaved) = bimap V.fromList V.fromList $ L.partition (\x -> Projects.QLTHistory == x.queryType) queryLib

      -- Queue facet generation if no precomputed facets exist (new projects)
      when (isNothing facetSummary)
        $ liftIO
        $ withResource authCtx.jobsPool \conn ->
          void $ createJob conn "background_jobs" $ BackgroundJobs.GenerateOtelFacetsBatch (V.singleton pid) now

      -- Build preload URL using the same function that builds the JSON URLs
      let preloadUrl = RequestDumps.requestDumpLogUrlPath pid queryM' cols' (formatUTC <$> cursorM') sinceM fromM toM Nothing sourceM False
          -- Also preload the chart data request
          chartDataUrl = "/chart_data?pid=" <> pid.toText <> "&query=summarize+count%28*%29+by+bin_auto%28timestamp%29%2C+status_code"
          headContent = Just $ do
            script_ [text|window.logDataPromise = fetch("$preloadUrl", {headers: {Accept: "application/json"}, credentials: "include"}).then(r => r.json());|]
            link_ [rel_ "preload", href_ chartDataUrl, term "as" "fetch"]

      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = Just project
              , pageTitle = "Explorer"
              , docsLink = Just "https://monoscope.tech/docs/dashboard/dashboard-pages/api-log-explorer/"
              , freeTierExceeded = freeTierExceeded
              , config = authCtx.config
              , headContent = headContent
              , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
                  label_ [class_ "cursor-pointer border border-strokeWeak rounded-lg flex shadow-xs"] do
                    input_ [type_ "checkbox", id_ "streamLiveData", class_ "hidden"]
                    span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-3 items-center", data_ "tippy-content" "pause live data stream"] $ faSprite_ "pause" "solid" "h-4 w-4 text-iconNeutral"
                    span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex  py-1 px-3 items-center", data_ "tippy-content" "stream live data"] $ faSprite_ "play" "regular" "h-4 w-4 text-iconNeutral"
                  Components.timepicker_ (Just "log_explorer_form") currentRange Nothing
                  Components.refreshButton_
              , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-outline items-center"] do
                  a_
                    [href_ $ "/p/" <> pid.toText <> "/log_explorer", role_ "tab", class_ "tab h-auto! tab-active text-textStrong"]
                    "Events"
                  a_ [href_ $ "/p/" <> pid.toText <> "/metrics", role_ "tab", class_ "tab h-auto! "] "Metrics"
              }

      case tableAsVecM of
        Just tableResult -> do
          r <- buildLogResult tableResult
          let patternsToSkip = fromMaybe 0 skipM + maybe 0 V.length patterns

          -- Build widgets with PNG URLs
          let baseChartWidget = logChartWidget pid
              baseLatencyWidget = logLatencyWidget pid
          chartPngUrl <- BotUtils.widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid baseChartWidget sinceM fromM toM
          latencyPngUrl <- BotUtils.widgetPngUrl authCtx.env.apiKeyEncryptionSecretKey authCtx.env.hostUrl pid baseLatencyWidget sinceM fromM toM
          let chartWidget = if T.null chartPngUrl then baseChartWidget else baseChartWidget{Widget.pngUrl = Just chartPngUrl}
              latencyWidget = if T.null latencyPngUrl then baseLatencyWidget else baseLatencyWidget{Widget.pngUrl = Just latencyPngUrl}

          let page =
                ApiLogsPageData
                  { pid
                  , resultCount = r.resultCount
                  , requestVecs = r.finalVecs
                  , cols = r.curatedColNames
                  , colIdxMap = r.colIdxMap
                  , nextLogsURL = r.nextLogsURL
                  , resetLogsURL = r.resetLogsURL
                  , recentLogsURL = r.recentLogsURL
                  , currentRange
                  , exceededFreeTier = freeTierExceeded
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
                  , targetPattern = pTargetM
                  , project = project
                  , teams
                  , chartWidget
                  , latencyWidget
                  , queryResultCount = r.queryResultCount
                  }

          addRespHeaders $ case (layoutM, hxRequestM, jsonM, effectiveVizType) of
            (_, _, Just "true", Just "patterns") -> LogsPatternJson (fromMaybe V.empty patterns)
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
    , Widget.query = Just "summarize count(*) by bin_auto(timestamp), status_code"
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


data LogsGet
  = LogPage (PageCtx ApiLogsPageData)
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text
  | LogsGetJson LogResult
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)
  | LogsPatternJson (V.Vector RequestDumps.PatternRow)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-textError"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtml x@LogsGetJson{} = span_ [] . toHtml $ (decodeUtf8 $ AE.encode x :: Text)
  toHtml x@LogsPatternJson{} = span_ [] . toHtml $ (decodeUtf8 $ AE.encode x :: Text)
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogsGetJson r) = AE.object ["logsData" AE..= r.finalVecs, "serviceColors" AE..= r.serviceColors, "nextUrl" AE..= r.nextLogsURL, "resetLogsUrl" AE..= r.resetLogsURL, "recentUrl" AE..= r.recentLogsURL, "cols" AE..= r.curatedColNames, "colIdxMap" AE..= r.colIdxMap, "count" AE..= r.resultCount, "traces" AE..= r.traces, "hasMore" AE..= (r.queryResultCount < r.resultCount), "queryResultCount" AE..= r.queryResultCount]
  toJSON (LogsPatternJson patterns) =
    let total = V.foldl' (\acc p -> acc + p.count) 0 patterns
        -- Row layout: [id, pattern_count, volume, level, service, summary] — must match colIdxMap
        -- On-the-fly patterns use chr(30) separator; Drain patterns are space-separated
        patternToSummary pat
          | "\x1E" `T.isInfixOf` pat = AE.toJSON (T.splitOn "\x1E" pat)
          | otherwise = AE.toJSON (T.words pat)
        rows = V.map (\p -> AE.Array $ V.fromList [AE.Null, AE.toJSON p.count, AE.toJSON p.volume, AE.toJSON p.level, AE.toJSON p.service, patternToSummary p.logPattern]) patterns
    in AE.object
          [ "logsData" AE..= rows
          , "cols" AE..= (["id", "pattern_count", "volume", "level", "service", "summary"] :: [Text])
          , "colIdxMap" AE..= HM.fromList [("id" :: Text, 0 :: Int), ("pattern_count", 1), ("volume", 2), ("level", 3), ("service", 4), ("summary", 5)]
          , "count" AE..= total
          , "hasMore" AE..= (V.length patterns > 14)
          , "queryResultCount" AE..= V.length patterns
          , "serviceColors" AE..= AE.object []
          , "traces" AE..= ([] :: [AE.Value])
          ]
  toJSON (LogsGetError _) = AE.object ["error" AE..= True, "message" AE..= ("Something went wrong" :: Text)]
  toJSON (LogsGetErrorSimple msg) = AE.object ["error" AE..= True, "message" AE..= msg]
  toJSON _ = AE.object ["error" AE..= True]


-- This component has been moved to Pkg.Components.LogQueryBox

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
  , patterns :: Maybe (V.Vector RequestDumps.PatternRow)
  , patternsToSkip :: Int
  , targetPattern :: Maybe Text
  , project :: Projects.Project
  , teams :: V.Vector ManageMembers.Team
  , chartWidget :: Widget.Widget
  , latencyWidget :: Widget.Widget
  , queryResultCount :: Int
  }


data LogResult = LogResult
  { finalVecs :: V.Vector (V.Vector AE.Value)
  , curatedColNames :: [Text]
  , colIdxMap :: HM.HashMap Text Int
  , cursor :: Maybe Text
  , nextLogsURL, resetLogsURL, recentLogsURL :: Text
  , serviceColors :: HM.HashMap Text Text
  , queryResultCount, resultCount :: Int
  , traces :: [TraceTreeEntry]
  }


virtualTable :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Html ()
virtualTable pid initialFetchUrl modeM = do
  termRaw
    "log-list"
    ( [ id_ "resultTable"
      , class_ "w-full divide-y shrink-1 flex flex-col h-full min-w-0 rr-block"
      , term "windowTarget" "logList"
      , term "projectId" pid.toText
      ]
        <> [term "initialFetchUrl" (fromMaybe "" initialFetchUrl) | isJust initialFetchUrl]
        <> [term "mode" m | m <- maybeToList modeM]
    )
    ("" :: Text)


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 px-4 gap-3.5 w-full flex flex-col h-full overflow-y-hidden overflow-x-hidden pb-2 group/pg", id_ "apiLogsPage"] do
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
            $ button_ [class_ "cursor-pointer", [__|on click add .hidden to #expand-log-modal|]]
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
          }

      div_ [class_ "timeline flex flex-row gap-4 mt-3 group-has-[.no-chart:checked]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full min-h-36 ", style_ "aspect-ratio: 10 / 1;"] do
        Widget.widget_ page.chartWidget
        Widget.widget_ page.latencyWidget
    div_ [class_ "flex h-full gap-3.5 overflow-y-hidden", id_ "facets_and_loglist"] do
        -- FACETS
        div_ [class_ "w-68 will-change-[width] contain-[layout_style] text-sm text-textWeak shrink-0 flex flex-col h-full overflow-y-scroll gap-2 group-has-[.toggle-filters:checked]/pg:max-w-0 group-has-[.toggle-filters:checked]/pg:overflow-hidden ", id_ "facets-container"] do
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

        div_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] $ resizer_ "facets-container" "facets_width" True

        let dW = fromMaybe "100%" page.detailsWidth
            showTrace = isJust page.showTrace
        div_ [class_ "grow will-change-[width] contain-[layout_style] relative flex flex-col shrink-1 min-w-0 w-full h-full ", style_ $ "xwidth: " <> dW, id_ "logs_list_container"] do
          -- Filters and row count header
          let isPatterns = page.vizType == Just "patterns"
          div_ [class_ $ "flex gap-2  pt-1 text-sm z-10 w-max bg-bgBase" <> bool " -mb-6" "" isPatterns] do
            label_ [class_ "gap-1 flex items-center cursor-pointer text-textWeak"] do
              faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
              span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
              span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
              "filters"
              input_ [type_ "checkbox", class_ "toggle-filters hidden", id_ "toggle-filters", onchange_ "localStorage.setItem('toggle-filter-checked', this.checked); setTimeout(() => { const editor = document.getElementById('filterElement'); if (editor && editor.refreshLayout) editor.refreshLayout(); }, 200);"]
              script_
                [text|
              document.getElementById('toggle-filters').checked = localStorage.getItem('toggle-filter-checked') === 'true';
              // Ensure editor layout is correct on initial load
              setTimeout(() => {
                const editor = document.getElementById('filterElement');
                if (editor && editor.refreshLayout) {
                  editor.refreshLayout();
                }
              }, 300);
            |]
            span_ [class_ "text-strokeWeak "] "|"
            div_ [class_ ""] do
              span_ [class_ "text-textStrong", id_ "row-count-display"] $ toHtml $ prettyPrintCount page.queryResultCount
              span_ [class_ "text-textStrong", id_ "row-count-suffix"] $ toHtml $ bool (" of " <> prettyPrintCount page.resultCount <> " rows") " rows" (page.queryResultCount >= page.resultCount)

          -- Visualization widget that shows when not in logs view (skip for patterns mode which uses log-list)
          div_ [class_ $ "flex-1 min-h-0 h-full group-has-[#viz-logs:checked]/pg:hidden" <> bool "" " hidden" isPatterns] do
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
            div_ [class_ "flex-1 min-h-0 hidden h-full group-has-[#viz-logs:checked]/pg:block group-has-[#viz-patterns:checked]/pg:block"] $ virtualTable page.pid Nothing page.vizType

        -- Alert configuration panel on the right
        div_ [class_ "hidden group-has-[#create-alert-toggle:checked]/pg:block"] $ resizer_ "alert_container" "alert_width" False

        div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll hidden group-has-[#create-alert-toggle:checked]/pg:block", id_ "alert_container", style_ "width: 500px;"] do
          alertConfigurationForm_ page.project page.alert page.teams

        div_ [class_ $ "transition-opacity duration-200 hidden group-has-[#viz-logs:checked]/pg:block " <> if isJust page.targetEvent then "" else "opacity-0 pointer-events-none hidden", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False

        div_ [class_ "grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0 max-w-0 overflow-hidden group-has-[#viz-logs:checked]/pg:max-w-full group-has-[#viz-logs:checked]/pg:overflow-y-auto", id_ "log_details_container"] do
          htmxOverlayIndicator_ "details_indicator"
          whenJust page.targetEvent \te -> do
            script_
              [text|
            document.addEventListener('DOMContentLoaded', function() {
              const detailsContainer = document.getElementById('log_details_container');
              if (detailsContainer) {
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

  queryEditorInitializationCode page.queryLibRecent page.queryLibSaved page.vizType


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
          let config = (AI.defaultAgenticConfig pid){AI.facetContext = facetSummaryM, AI.timezone = timezoneM}
          result <- AI.runAgenticQuery config inputText envCfg.openaiApiKey

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
      isByos = project.paymentPlan == "Bring your own storage"
  div_ [class_ "surface-raised h-full flex flex-col group/alt"] do
    -- Header section (more compact)
    div_ [class_ "flex items-center justify-between px-4 py-2.5"] do
      div_ [class_ "flex items-center gap-2.5"] do
        div_ [class_ "w-8 h-8 rounded-full bg-fillBrand-weak flex items-center justify-center shrink-0"]
          $ faSprite_ "bell" "regular" "w-4 h-4 text-iconBrand"
        div_ [] do
          h3_ [class_ "text-base font-semibold text-textStrong"] "Create Alert"
          p_ [class_ "text-xs text-textWeak hidden sm:block"] "Monitor logs and get notified on thresholds"
      -- Close button only
      button_
        [ type_ "button"
        , class_ "p-1 rounded-lg hover:bg-fillWeak transition-colors"
        , [__|on click set #create-alert-toggle.checked to false|]
        ]
        $ faSprite_ "xmark" "regular" "w-3 h-3 text-textWeak"

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
          formField_ FieldSm def{value = maybe "" (\x -> x.alertConfig.title) alertM, placeholder = "e.g. High error rate on checkout API"} "Alert name" "title" True Nothing

          -- Monitor Schedule section (shared component)
          let defaultFrequency = maybe 5 (.checkIntervalMins) alertM
              conditionType = if maybe True (\x -> x.alertThreshold > 0 && isJust x.warningThreshold) alertM then Just "threshold_exceeded" else Just "has_matches"
          AlertUI.monitorScheduleSection_ isByos defaultFrequency 5 conditionType Nothing

          -- Thresholds section (shared component)
          AlertUI.thresholdsSection_ Nothing (fmap (.alertThreshold) alertM) ((.warningThreshold) =<< alertM) (maybe False (.triggerLessThan) alertM) ((.alertRecoveryThreshold) =<< alertM) ((.warningRecoveryThreshold) =<< alertM)

          -- Notification Settings section (shared component)
          let selectedTeamIds = maybe V.empty (.teams) alertM
          AlertUI.notificationSettingsSection_ ((.alertConfig.severity) <$> alertM) ((.alertConfig.subject) <$> alertM) ((.alertConfig.message) <$> alertM) (maybe True (.alertConfig.emailAll) alertM) teams selectedTeamIds "alert-form"

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
                if isJust alertM then "Update alert" else "Create Alert"



