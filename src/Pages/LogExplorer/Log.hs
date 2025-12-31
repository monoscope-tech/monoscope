module Pages.LogExplorer.Log (
  apiLogH,
  aiSearchH,
  LogsGet (..),
  ApiLogsPageData (..),
  virtualTable,
  curateCols,
  logQueryBox_,
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
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
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
import Utils (checkFreeTierExceeded, faSprite_, formatUTC, getServiceColors, levelFillColor, listToIndexHashMap, lookupVecIntByKey, lookupVecTextByKey, methodFillColor, onpointerdown_, prettyPrintCount, statusFillColorText)

import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID qualified as UUID
import Models.Apis.Monitors (MonitorAlertConfig (..))
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectMembers qualified as ManageMembers
import Pages.Components (resizer_)
import Pkg.AI (callOpenAIAPI, systemPrompt)
import Pkg.AI qualified as AI


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


-- | Render facet data for Log Explorer sidebar in a compact format
-- | The facet counts are already scaled in the Facets.getFacetSummary function based on the selected time range
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

                          span_ [class_ "facet-count text-xs text-textWeak shrink-0"] $ toHtml $ prettyPrintCount count
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
  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
  queryAST <- parseQuery $ maybeToMonoid queryM'
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
  let shouldSkipLoad = isNothing layoutM && isNothing hxRequestM && jsonM /= Just "true" || effectiveVizType == Just "patterns"
      fetchLogs =
        if authCtx.env.enableTimefusionReads
          then labeled @"timefusion" @PG.WithConnection $ RequestDumps.selectLogTable pid queryAST queryText cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM
          else RequestDumps.selectLogTable pid queryAST queryText cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  tableAsVecE <-
    if shouldSkipLoad
      then pure $ Right (V.empty, ["timestamp", "summary", "duration"], 0)
      else fetchLogs

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE

  (queryLibRecent, queryLibSaved) <- bimap V.fromList V.fromList . L.partition (\x -> Projects.QLTHistory == x.queryType) <$> Projects.queryLibHistoryForUser pid sess.persistentSession.userId

  -- Get facet summary for the time range specified
  facetSummary <- Facets.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)

  freeTierExceeded <- checkFreeTierExceeded pid project.paymentPlan

  -- Fetch teams
  teams <- V.fromList <$> ManageMembers.getTeams pid

  patterns <- case effectiveVizType of
    Just "patterns" -> do
      patternsResult <- V.fromList <$> RequestDumps.fetchLogPatterns pid queryAST (fromD, toD) (parseMaybe pSource =<< sourceM) pTargetM (fromMaybe 0 skipM)
      return $ Just patternsResult
    _ -> return Nothing

  -- Build preload URL using the same function that builds the JSON URLs
  let preloadUrl = RequestDumps.requestDumpLogUrlPath pid queryM' cols' (formatUTC <$> cursorM') sinceM fromM toM Nothing sourceM False
      -- Also preload the chart data request
      chartDataUrl = "/chart_data?pid=" <> pid.toText <> "&query=summarize+count%28*%29+by+bin_auto%28timestamp%29%2C+status_code"
      headContent = Just $ do
        link_ [rel_ "preload", href_ preloadUrl, term "as" "fetch"]
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
    Just tableAsVec -> do
      let (requestVecs, colNames, resultCount) = tableAsVec
          curatedColNames = nubOrd $ curateCols summaryCols colNames
          colIdxMap = listToIndexHashMap colNames
          reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
          reqFirstCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? 0)
          traceIds = V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
          -- Extract IDs of spans that already have a parent_id (these are already child spans in our results)
          alreadyLoadedChildSpanIds =
            V.mapMaybe
              ( \v -> case lookupVecTextByKey v colIdxMap "parent_id" of
                  Just parentId | not (T.null parentId) -> lookupVecTextByKey v colIdxMap "id"
                  Nothing -> do
                    case lookupVecIntByKey v colIdxMap "duration" of -- exlude logs since they have not parent/child relationships
                      0 -> lookupVecTextByKey v colIdxMap "id"
                      _ -> Nothing
                  _ -> Nothing
              )
              requestVecs
          (fromDD, toDD, _) = Components.parseTimeRange now (Components.TimePicker sinceM reqLastCreatedAtM reqFirstCreatedAtM)

      childSpansList <- case queryM' of
        Nothing -> RequestDumps.selectChildSpansAndLogs pid summaryCols traceIds (fromDD, toDD) alreadyLoadedChildSpanIds
        _ -> pure []
      let
        childSpans = V.fromList childSpansList
        finalVecs = requestVecs <> childSpans
        lastFM = reqLastCreatedAtM >>= textToUTC >>= Just . toText . iso8601Show . addUTCTime (-0.001)
        nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' lastFM sinceM fromM toM (Just "loadmore") sourceM False
        resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' Nothing Nothing Nothing Nothing Nothing sourceM False
        recentLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' Nothing sinceM fromM toM (Just "loadmore") sourceM True

        serviceNames = V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") finalVecs
        colors = getServiceColors (V.catMaybes serviceNames)
        patternsToSkip = fromMaybe 0 skipM + maybe 0 V.length patterns
        page =
          ApiLogsPageData
            { pid
            , resultCount
            , requestVecs = finalVecs
            , cols = curatedColNames
            , colIdxMap
            , nextLogsURL
            , resetLogsURL
            , recentLogsURL
            , currentRange
            , exceededFreeTier = freeTierExceeded
            , query = queryM'
            , cursor = reqLastCreatedAtM
            , isTestLog = Nothing
            , emptyStateUrl = Nothing
            , source
            , targetSpans = targetSpansM
            , serviceColors = colors
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
            }

      let jsonResponse = LogsGetJson finalVecs colors nextLogsURL resetLogsURL recentLogsURL curatedColNames colIdxMap resultCount
      addRespHeaders $ case (layoutM, hxRequestM, jsonM, effectiveVizType) of
        (_, Just "true", _, Just "patterns") -> LogsPatternList pid (fromMaybe V.empty patterns) patternsToSkip pTargetM
        (Just "SaveQuery", _, _, _) -> LogsQueryLibrary pid queryLibSaved queryLibRecent
        (Just "resultTable", Just "true", _, _) -> jsonResponse
        (Just "all", Just "true", _, _) -> jsonResponse
        (_, _, Just "true", _) -> jsonResponse
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


data LogsGet
  = LogPage (PageCtx ApiLogsPageData)
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text
  | LogsGetJson (V.Vector (V.Vector AE.Value)) (HM.HashMap Text Text) Text Text Text [Text] (HM.HashMap Text Int) Int
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)
  | LogsPatternList Projects.ProjectId (V.Vector (Text, Int)) Int (Maybe Text)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-textError"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsPatternList pid patterns skip tg) = toHtml $ patternList patterns pid skip (skip > 0) tg
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtml (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap count) =
    span_ [] $ toHtml (decodeUtf8 $ AE.encode $ AE.toJSON (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap count) :: Text)
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap count) = AE.object ["logsData" AE..= vecs, "serviceColors" AE..= colors, "nextUrl" AE..= nextLogsURL, "resetLogsUrl" AE..= resetLogsURL, "recentUrl" AE..= recentLogsURL, "cols" AE..= cols, "colIdxMap" AE..= colIdxMap, "count" AE..= count]
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
  , facets :: Maybe Models.Apis.Fields.Types.FacetSummary
  , vizType :: Maybe Text
  , alert :: Maybe Monitors.QueryMonitor
  , patterns :: Maybe (V.Vector (Text, Int))
  , patternsToSkip :: Int
  , targetPattern :: Maybe Text
  , project :: Projects.Project
  , teams :: V.Vector ManageMembers.Team
  }


virtualTable :: Projects.ProjectId -> Maybe Text -> Html ()
virtualTable pid initialFetchUrl = do
  termRaw
    "log-list"
    ( [ id_ "resultTable"
      , class_ "w-full divide-y shrink-1 flex flex-col h-full min-w-0 rr-block"
      , term "windowTarget" "logList"
      , term "projectId" pid.toText
      ]
        <> [term "initialFetchUrl" (fromMaybe "" initialFetchUrl) | isJust initialFetchUrl]
    )
    ("" :: Text)


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 px-4 gap-3.5 w-full flex flex-col h-full overflow-y-hidden overflow-x-hidden pb-2 group/pg", id_ "apiLogsPage"] do
    template_ [id_ "loader-tmp"] $ span_ [class_ "loading loading-dots loading-md"] ""

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
        let patternTarget = fromMaybe "log_pattern" page.targetPattern
            nm = fromMaybe "Log" $ viaNonEmpty head $ T.splitOn "_" patternTarget
        let (tp, query, title) = case page.vizType of
              Just "patterns" -> (WTTimeseriesLine, patternTarget <> " != null | summarize count(*) by bin_auto(timestamp), " <> patternTarget, nm <> " patterns")
              _ -> (WTTimeseries, "summarize count(*) by bin_auto(timestamp), status_code", "All traces")
        Widget.widget_ $ (def :: Widget.Widget){Widget.wType = tp, Widget.query = Just query, Widget.unit = Just "rows", Widget.title = Just title, Widget.hideLegend = Just True, Widget._projectId = Just page.pid, Widget.standalone = Just True, Widget.yAxis = Just (def{showOnlyMaxLabel = Just True}), Widget.allowZoom = Just True, Widget.showMarkArea = Just True, Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})}
        unless (page.vizType == Just "patterns")
          $ Widget.widget_
          $ (def :: Widget.Widget)
            { Widget.wType = WTTimeseriesLine
            , Widget.standalone = Just True
            , Widget.title = Just "Latency percentiles"
            , Widget.hideSubtitle = Just True
            , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
            , Widget.summarizeBy = Just Widget.SBMax
            , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
            , Widget.query = Just "duration != null | summarize percentiles(duration) by bin_auto(timestamp)"
            , Widget.unit = Just "ns"
            , Widget.hideLegend = Just True
            , Widget._projectId = Just page.pid
            }
    whenJust page.patterns \patternsData ->
      div_ [class_ "overflow-y-auto max-h-96 border border-strokeWeak rounded mt-3"] do
        patternList patternsData page.pid page.patternsToSkip False page.targetPattern
    unless (page.vizType == Just "patterns")
      $ div_ [class_ "flex h-full gap-3.5 overflow-y-hidden", id_ "facets_and_loglist"] do
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
          div_ [class_ "flex gap-2  pt-1 text-sm -mb-6 z-10 w-max bg-bgBase"] do
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
            div_ [class_ ""] $ span_ [class_ "text-textStrong", id_ "row-count-display"] (toHtml $ prettyPrintCount page.resultCount) >> span_ [class_ "text-textStrong"] (toHtml " rows")

          -- Visualization widget that shows when not in logs view
          div_ [class_ "flex-1 min-h-0 h-full group-has-[#viz-logs:checked]/pg:hidden"] do
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
              , hxExt_ "json-enc"
              , term "hx-sync" "this:replace"
              ]
              ""

          -- Trace view container
          div_ [class_ $ "absolute top-0 right-0  w-full h-full overflow-scroll c-scroll z-50 bg-bgBase transition-all duration-100 " <> if showTrace then "" else "hidden", id_ "trace_expanded_view"] do
            whenJust page.showTrace \trIdAndTimestamp -> do
              let url = "/p/" <> page.pid.toText <> "/traces/" <> trIdAndTimestamp
              span_ [class_ "loading loading-dots loading-md"] ""
              div_ [hxGet_ url, hxTarget_ "#trace_expanded_view", hxSwap_ "innerHtml", hxTrigger_ "intersect one", term "hx-sync" "this:replace"] pass

          -- Logs view section (also within the scrollable container)
          div_ [class_ "flex-1 min-h-0 h-full flex flex-col"] do
            -- Virtual table for logs
            div_ [class_ "flex-1 min-h-0 hidden h-full group-has-[#viz-logs:checked]/pg:block"] $ virtualTable page.pid Nothing

        -- Alert configuration panel on the right
        div_ [class_ "hidden group-has-[#create-alert-toggle:checked]/pg:block"] $ resizer_ "alert_container" "alert_width" False

        div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll hidden group-has-[#create-alert-toggle:checked]/pg:block", id_ "alert_container", style_ "width: 500px;"] do
          alertConfigurationForm_ page.project page.alert page.teams

        div_ [class_ $ "transition-opacity duration-200 hidden group-has-[#viz-logs:checked]/pg:block " <> if isJust page.targetEvent then "" else "opacity-0 pointer-events-none hidden", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False

        div_ [class_ "grow-0 relative shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0 max-w-0 overflow-hidden group-has-[#viz-logs:checked]/pg:max-w-full group-has-[#viz-logs:checked]/pg:overflow-y-auto", id_ "log_details_container"] do
          span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
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
aiSearchH _pid requestBody = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  let envCfg = authCtx.env

  let inputTextM = AET.parseMaybe (AE.withObject "request" (AE..: "input")) requestBody
  case inputTextM of
    Nothing -> do
      addErrorToast "Invalid AI search input" Nothing
      throwError Servant.err400{Servant.errBody = "Invalid input format"}
    Just inputText ->
      if T.null (T.strip inputText)
        then do
          addErrorToast "Please enter a search query" Nothing
          throwError Servant.err400{Servant.errBody = "Empty input"}
        else do
          let fullPrompt = systemPrompt <> "\n\nUser query: " <> inputText
          result <- liftIO $ callOpenAIAPI fullPrompt envCfg.openaiApiKey

          case result of
            Left errMsg -> do
              addErrorToast "AI search failed" (Just errMsg)
              throwError Servant.err502{Servant.errBody = encodeUtf8 errMsg}
            Right rs -> do
              let resp = AI.getAskLLMResponse rs
              case resp of
                Left err -> do
                  addErrorToast "AI search failed" (Just err)
                  throwError Servant.err502{Servant.errBody = encodeUtf8 err}
                Right AI.ChatLLMResponse{..} -> do
                  addRespHeaders
                    $ AE.object
                      [ "query" AE..= query
                      , "visualization_type" AE..= visualization
                      ]


curateCols :: [Text] -> [Text] -> [Text]
curateCols summaryCols cols = sortBy sortAccordingly filteredCols
  where
    defaultSummaryPaths =
      [ "trace_id"
      , "severity_text"
      , "parent_span_id"
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
  div_ [class_ "bg-fillWeaker h-full flex flex-col group/alt"] do
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
        $ faSprite_ "xmark" "regular" "w-4 h-4 text-textWeak"

    -- Form content wrapper with scrolling
    div_ [class_ "p-4 pt-3 flex-1 overflow-y-auto c-scroll"] do
      form_
        [ id_ "alert-form"
        , hxPost_ $ "/p/" <> pid.toText <> "/monitors/alerts"
        , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, source: params().source || 'spans', vizType: getVizType(), teams: getSelectedTeams()}"
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
          -- Alert name field (more compact)
          input_ [type_ "hidden", name_ "alertId", value_ $ maybe "" ((.id.toText)) alertM]
          fieldset_ [class_ "fieldset"] do
            label_ [class_ "label text-xs font-medium text-textStrong mb-1"] "Alert name"
            input_
              [ type_ "text"
              , name_ "title"
              , value_ $ maybe "" (\x -> x.alertConfig.title) alertM
              , placeholder_ "e.g. High error rate on checkout API"
              , class_ "input input-sm w-full"
              , required_ ""
              ]

          -- Monitor Schedule (collapsible)
          div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
            label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
              div_ [class_ "flex items-center gap-2"] do
                faSprite_ "clock" "regular" "w-4 h-4 text-iconNeutral"
                span_ [class_ "text-sm font-medium text-textStrong"] "Monitor Schedule"
              input_ [type_ "checkbox", class_ "hidden peer", checked_]
              faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"

            div_ [class_ "gap-3 p-3 pt-0 peer-has-[:checked]:flex hidden"] do
              let timeOpts = [(1, "minute"), (2, "2 minutes"), (5, "5 minutes"), (10, "10 minutes"), (15, "15 minutes"), (30, "30 minutes"), (60, "hour"), (360, "6 hours"), (720, "12 hours"), (1440, "day")]
                  defaultInterval = maybe 5 (.checkIntervalMins) alertM
                  mkOpt (m, l) =
                    let isDisabled = not isByos && m < 5
                        attrs =
                          [value_ (show m <> "m")]
                            <> [disabled_ "" | isDisabled]
                            <> [selected_ "" | m == defaultInterval]
                            <> [term "data-tippy-content" "Upgrade to a higher plan to access this frequency" | isDisabled]
                     in option_ attrs ("every " <> l)

              -- Frequency
              fieldset_ [class_ "fieldset flex-1"] do
                label_ [class_ "label text-xs"] "Execute the query"
                select_ [name_ "frequency", class_ "select select-sm"] $ forM_ timeOpts mkOpt

              -- Time window
              fieldset_ [class_ "fieldset flex-1"] do
                label_ [class_ "label text-xs"] "Include rows from"
                select_
                  [ name_ "timeWindow"
                  , class_ "select select-sm"
                  , id_ "timeWindow"
                  , [__|on change
                       set qb to document.querySelector('query-builder')
                       if qb exists then
                         call qb.updateBinInQuery('timestamp', my.value)
                       end
                     |]
                  ]
                  do
                    forM_
                      (zip timeOpts [False, False, True, False, False, False, False, False, False, False])
                      \((m, l), sel) -> option_ (value_ (show m <> "m") : [selected_ "" | sel]) ("the last " <> l)

              -- Condition type
              fieldset_ [class_ "fieldset flex-1"] do
                label_ [class_ "label text-xs"] "Notify me when"
                select_
                  [ name_ "conditionType"
                  , class_ "select select-sm"
                  , id_ "condType"
                  , [__|on change 
                     if my value == 'threshold_exceeded' then remove .hidden from #thresholds
                     else add .hidden to #thresholds end
                     |]
                  ]
                  do
                    -- option_ [value_ "matches_changed"] "the query's results change"
                    option_ ([value_ "threshold_exceeded"] <> [selected_ "" | maybe True (\x -> x.alertThreshold > 0 && isJust x.warningThreshold) alertM]) "threshold is exceeded"
                    option_ ([value_ "has_matches"] <> [selected_ "" | maybe False (\x -> x.alertThreshold == 0 && isNothing x.warningThreshold) alertM]) "the query has any results"

          -- Thresholds (collapsible, only visible when threshold_exceeded is selected)
          div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden", id_ "thresholds"] do
            label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
              div_ [class_ "flex items-center gap-2"] do
                faSprite_ "chart-line" "regular" "w-4 h-4 text-iconNeutral"
                span_ [class_ "text-sm font-medium text-textStrong"] "Thresholds"
              input_ [type_ "checkbox", class_ "hidden peer", checked_]
              faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"

            div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
              div_ [class_ "flex flex-row gap-3"] do
                let thresholdInput name color label req vM = fieldset_ [class_ "fieldset flex-1"] do
                      _ <- label_ [class_ "label flex items-center gap-1.5 text-xs mb-1"] do
                        _ <- div_ [class_ $ "w-1.5 h-1.5 rounded-full " <> color] ""
                        _ <- span_ [class_ "font-medium"] label
                        when req $ span_ [class_ "text-textWeak"] "*"
                      div_ [class_ "relative"] do
                        input_
                          $ [ type_ "number"
                            , name_ name
                            , id_ name
                            , class_ "input input-sm pr-14"
                            , [__|on input 
                                   set chart to #visualization-widget
                                   if chart exists
                                     call chart.applyThresholds({
                                       alert: parseFloat(#alertThreshold.value),
                                       warning: parseFloat(#warningThreshold.value)
                                     })
                                   end|]
                            ]
                          ++ [required_ "" | req]
                          ++ [value_ (maybe "" show vM) | isJust vM]
                        span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] "events"

                thresholdInput "alertThreshold" "bg-fillError-strong" "Alert threshold" True (fmap (.alertThreshold) alertM)
                thresholdInput "warningThreshold" "bg-fillWarning-strong" "Warning threshold" False ((.warningThreshold) =<< alertM)

                fieldset_ [class_ "fieldset flex-1"] do
                  label_ [class_ "label text-xs font-medium mb-1"] "Trigger condition"
                  select_ [name_ "direction", class_ "select select-sm"] do
                    option_ (value_ "above" : [selected_ "" | maybe True (not . (.triggerLessThan)) alertM]) "Above threshold"
                    option_ (value_ "below" : [selected_ "" | maybe False (.triggerLessThan) alertM]) "Below threshold"

              -- Info banner (more compact)
              div_ [class_ "flex items-start gap-2 p-2.5 bg-bgAlternate rounded-lg mt-3 hidden"] do
                faSprite_ "lightbulb" "regular" "w-3.5 h-3.5 text-iconBrand mt-0.5 shrink-0"
                div_ [class_ "flex-1 text-xs"] do
                  p_ [class_ "text-textStrong font-medium"] "Preview thresholds on chart"
                  p_ [class_ "text-textWeak mt-0.5"] "Colored lines show threshold values"

          -- Notification settings (collapsible)
          div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden"] do
            label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
              div_ [class_ "flex items-center gap-2"] do
                faSprite_ "envelope" "regular" "w-4 h-4 text-iconNeutral"
                span_ [class_ "text-sm font-medium text-textStrong"] "Notification Settings"
              input_ [type_ "checkbox", class_ "hidden peer"]
              faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"

            div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
              -- Severity and Subject row
              let defaultSeverity = maybe "Error" (.alertConfig.severity) alertM
              div_ [class_ "flex items-center w-full gap-2 mb-3"] do
                fieldset_ [class_ "fieldset"] do
                  label_ [class_ "label text-xs font-medium mb-1"] "Severity"
                  select_ [class_ "select select-sm w-28", name_ "severity"] do
                    option_ [selected_ "" | defaultSeverity == "Info"] "Info"
                    option_ [selected_ "" | defaultSeverity == "Error"] "Error"
                    option_ [selected_ "" | defaultSeverity == "Warning"] "Warning"
                    option_ [selected_ "" | defaultSeverity == "Critical"] "Critical"

                fieldset_ [class_ "fieldset w-full"] do
                  label_ [class_ "label text-xs font-medium mb-1"] "Subject"
                  input_
                    [ placeholder_ "e.g. Alert triggered for high error rate"
                    , class_ "input input-sm w-full"
                    , name_ "subject"
                    , value_ (maybe "Alert triggered" (\x -> x.alertConfig.subject) alertM)
                    ]

              -- Message field
              fieldset_ [class_ "fieldset w-full mb-3"] do
                label_ [class_ "label text-xs font-medium mb-1"] "Message"
                textarea_
                  [ placeholder_ "Alert message details"
                  , class_ "textarea textarea-sm p-2 rounded-lg w-full"
                  , name_ "message"
                  , rows_ "3"
                  ]
                  $ toHtml
                  $ maybe "The alert threshold has been exceeded. Check the APItoolkit dashboard for details." (.alertConfig.message) alertM

              -- Recovery Thresholds section with improved spacing
              div_ [class_ "border-t border-strokeWeak pt-4 mt-4"] do
                div_ [class_ "mb-3"] do
                  h4_ [class_ "font-medium text-sm text-textStrong mb-1"] "Recovery Thresholds"
                  p_ [class_ "text-xs text-textWeak"] "Continue notifications until monitor recovers"

                div_ [class_ "space-y-3"] do
                  -- Renotify option
                  div_ [class_ "flex items-center"] do
                    label_ [class_ "flex items-center gap-2 text-xs"] do
                      input_ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ "notifyAfterCheck"]
                      span_ [] "Renotify every"
                    select_ [class_ "select select-sm w-28 ml-2", name_ "notifyAfter", id_ "notifyAfterInterval"]
                      $ zipWithM_ (\v t -> option_ (value_ v : [selected_ "" | v == "30m"]) (toHtml t)) ["10m", "20m", "30m", "1h", "6h", "24h"] ["10 mins", "20 mins", "30 mins", "1 hour", "6 hours", "24 hours"]

                  -- Stop after option
                  div_ [class_ "flex items-center"] do
                    label_ [class_ "flex items-center gap-2 text-xs"] do
                      input_
                        [ type_ "checkbox"
                        , class_ "checkbox checkbox-sm"
                        , name_ "stopAfterCheck"
                        , [__|on change if my.checked then remove .hidden from #stopAfterInput else add .hidden to #stopAfterInput end|]
                        ]
                      span_ [] "Stop after"
                    div_ [class_ "flex items-center gap-1.5 ml-2 hidden", id_ "stopAfterInput"] do
                      input_ [type_ "number", class_ "input input-sm w-16", value_ "5", name_ "stopAfter", min_ "1", max_ "100"]
                      span_ [class_ "text-xs text-textWeak"] "occurrences"
              -- Teams
              div_ [class_ "border-t border-strokeWeak pt-4 mt-4"] do
                div_ [class_ "flex flex-col gap-1"] do
                  span_ [class_ "text text-sm"] "Teams"
                  span_ [class_ "text-xs text-textWeak"] "Add teams to notify (if no team is added, project level notification channels will be used)"
                textarea_ [class_ "input max-h-max w-full mt-2 resize-none", name_ "teams"] ""
              let teamList = decodeUtf8 $ AE.encode $ (\x -> AE.object ["name" AE..= x.handle, "value" AE..= x.id]) <$> teams
                  -- alertConfig.teams is a list of team IDs
                  -- teams is a list of TeamVM with id and name
                  -- map all team IDs to names
                  teamName tId teamVMs =
                    case V.find (\t -> t.id == tId) teamVMs of
                      Just t -> t.handle
                      Nothing -> "Unknown Team"

                  existingTeams = decodeUtf8 $ AE.encode $ case alertM of
                    Nothing -> []
                    Just am -> (\tId -> AE.object ["name" AE..= teamName tId teams, "value" AE..= tId]) <$> am.teams
              script_
                [text|
                window.addEventListener('DOMContentLoaded', () => {
                      const tagify = createTagify('#alert-form textarea[name="teams"]', {
                      tagTextProp: 'name',
                      whitelist: $teamList,
                    });
                  tagify.addTags($existingTeams);
                })
                const getSelectedTeams = () => {
                    return tagify.value.map(item => item.value);
                }
              |]

              -- Recipients checkbox with better spacing
              div_ [class_ "flex items-center gap-2 mt-4 pt-3 border-t border-strokeWeak"] do
                label_ [class_ "label cursor-pointer flex items-center gap-2"] do
                  input_ $ [type_ "checkbox", class_ "checkbox checkbox-sm", name_ "recipientEmailAll", value_ "true"] ++ [checked_ | maybe True (.alertConfig.emailAll) alertM]
                  span_ [class_ "text-sm"] "Send to all team members"

                span_ [class_ "tooltip", term "data-tip" "Configure specific recipients in alert settings after creation"]
                  $ faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-iconNeutral"

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


patternList :: V.Vector (Text, Int) -> Projects.ProjectId -> Int -> Bool -> Maybe Text -> Html ()
patternList patterns pid skip onlyRows targetPattern = do
  let total = V.foldl' (\acc (_, c) -> acc + c) 0 patterns
      url = "/p/" <> pid.toText <> "/log_explorer?viz_type=patterns&pattern_skip=" <> show skip <> "&pattern_target=" <> fromMaybe "log_pattern" targetPattern
  if onlyRows
    then do
      forM_ patterns $ \p -> renderPattern p total pid
      when (V.length patterns > 14)
        $ loadMore url
    else
      if V.null patterns
        then div_ [class_ "p-6 bg-bgBase rounded-lg text-center", id_ "pattern-list"] $ do
          faSprite_ "magnifying-glass" "regular" "w-6 h-6 mx-auto text-iconNeutral"
          h3_ [class_ "mt-3 text-sm font-medium text-textStrong"] "No patterns found"
          p_ [class_ "mt-1 text-xs text-textWeak"] "Try expanding the time range or adjust your query to surface patterns."
          div_ [class_ "mt-3"]
            $ button_
              [ class_ "btn btn-sm btn-ghost"
              , onpointerdown_ "dispatchEvent(new Event('update-query'))"
              ]
              "Refresh"
        else do
          table_ [class_ "min-w-full table table-sm rounded-2xl text-sm", id_ "pattern-list"] $ do
            thead_ [class_ "sticky top-0 bg-bgBase z-10 text-xs"]
              $ tr_
              $ do
                th_ [class_ "px-4 py-2 text-left"] ""
                th_ [class_ "px-4 py-2 text-left"] "Count"
                th_ [class_ "px-4 py-2 text-left"] "%"
                th_ [class_ "px-4 py-2 text-left"] "Pattern"
            tbody_ [class_ "divide-y divide-border"] do
              forM_ patterns $ \p -> renderPattern p total pid
              when (V.length patterns > 14)
                $ loadMore url

  script_
    [text|
       ['submit', 'add-query', 'update-query'].forEach((ev) =>
         window.addEventListener(ev, () => {
          const p = new URLSearchParams(window.location.search);
          const pathName = window.location.pathname;
          const url = window.location.origin + pathName + "?" + p.toString();
          htmx.ajax('GET', url, { target: '#pattern-list', swap: 'outerHTML', indicator: '#details_indicator' });
      })
    );
    |]


loadMore :: Text -> Html ()
loadMore url =
  tr_
    [ class_ ""
    , hxTrigger_ "click, intersect once"
    , hxSwap_ "outerHTML"
    , hxGet_ url
    , hxIndicator_ "#rowsIndicator"
    ]
    do
      td_ ""
      td_ ""
      td_ ""
      td_
        [class_ "col-span-4 w-full relative w-full cursor-pointer flex items-center p-1 text-textBrand"]
        do
          "Load more"
          span_ [id_ "rowsIndicator", class_ "ml-2 htmx-indicator loading loading-dots loading-md"] ""


renderPattern :: (Text, Int) -> Int -> Projects.ProjectId -> Html ()
renderPattern (template, count) total pid =
  tr_ [class_ "hover:bg-fillWeaker"] $ do
    td_ [class_ "flex items-center justify-center w-32 h-12"]
      $ Widget.widget_
      $ (def :: Widget.Widget)
        { Widget.wType = WTTimeseries
        , Widget.query = Just "kind == \"log\" | summarize count() by bin_auto(timestamp)"
        , Widget.naked = Just True
        , Widget.xAxis = Just (def{showAxisLabel = Just False})
        , Widget.yAxis = Just (def{showAxisLabel = Just False})
        , Widget.hideLegend = Just True
        , Widget._projectId = Just pid
        , Widget.standalone = Just True
        , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
        }
    td_ [class_ "px-4 py-2 monospace"] (toHtml (show count))
    td_ [class_ "px-4 py-2"]
      $ toHtml (take 4 (show (fromIntegral count / fromIntegral total * 100 :: Double)) ++ "%")
    td_ [class_ "px-4 py-2 max-w-xl truncate whitespace-nowrap"]
      $ code_
        [class_ "bg-muted/50 px-2 py-1 rounded monospace text-foreground"]
        (toHtml template)
