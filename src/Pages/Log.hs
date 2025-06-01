module Pages.Log (
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
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static qualified
import Effectful.Time qualified as Time

-- import Fmt (commaizeF, fmt) -- Using prettyPrintCount instead
import Langchain.LLM.Core qualified as LLM
import Langchain.LLM.OpenAI (OpenAI (..))
import Log qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), currProject, pageActions, pageTitle, sessM)
import Pages.Dashboards (visTypes)
import Pkg.Components qualified as Components
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (WTStat, WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (pSource, parseQueryToAST, toQText)
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils (displayTimestamp, faSprite_, formatUTC, freeTierLimitExceededBanner, getServiceColors, listToIndexHashMap, lookupVecTextByKey, prettyPrintCount)


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

      -- Color functions for different facet types
      statusColorFn val = case T.take 1 val of
        "2" -> "bg-green-500"
        "3" -> "bg-blue-500"
        "4" -> "bg-yellow-500"
        "5" -> "bg-red-600"
        _ -> "bg-gray-500"

      methodColorFn val = case val of
        "GET" -> "bg-[#067cff]"
        "POST" -> "bg-green-500"
        "PUT" -> "bg-amber-500"
        "DELETE" -> "bg-red-500"
        _ -> "bg-purple-500"

      levelColorFn val = case val of
        "ERROR" -> "bg-red-500"
        "WARN" -> "bg-yellow-500"
        "INFO" -> "bg-blue-500"
        "DEBUG" -> "bg-gray-500"
        _ -> "bg-gray-400"

      -- Group facet fields by category
      -- Define mapping of field keys to display names and color functions

      -- Root level facets (displayed at the top)
      rootFacets :: [(Text, Text, (Text -> Text))]
      rootFacets =
        [ ("level", "Log Level", levelColorFn)
        , ("kind", "Kind", const "")
        , ("name", "Operation Name", const "")
        , ("status_code", "Status Code", statusColorFn)
        , ("resource___service___name", "Service", const "")
        , ("resource___service___version", "Service Version", const "")
        , ("attributes___http___request___method", "HTTP Method", methodColorFn)
        , ("attributes___http___response___status_code", "HTTP Status", statusColorFn)
        , ("attributes___error___type", "Error Type", const "bg-red-500")
        ]

      -- Grouped facets for better organization
      facetGroups :: [(Text, [(Text, Text, (Text -> Text))])]
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
            [ ("attributes___error___type", "Error Type", const "bg-red-500")
            , ("attributes___exception___type", "Exception Type", const "bg-red-500")
            , ("attributes___exception___message", "Exception Message", const "")
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
          ( "Severity"
          ,
            [ ("severity___severity_text", "Severity Text", levelColorFn)
            , ("severity___severity_number", "Severity Number", const "")
            , ("status_message", "Status Message", const "")
            ]
          )
        ]

  -- Add JS for filtering
  script_
    [text|
    function filterByFacet(field, value) {
      document.getElementById("filterElement").handleAddQuery(field + ' == "' + value + '"');
    }
  |]

  script_
    [text|
    // Add a utility to handle removing group by statements
    if (!document.getElementById("filterElement").handleRemoveGroupBy) {
      document.getElementById("filterElement").handleRemoveGroupBy = function(field) {
        const currentQuery = this.value || "";
        const regex = new RegExp("group by " + field, 'gi');
        const newQuery = currentQuery.replace(regex, "").trim();
        this.value = newQuery;
        this.dispatchEvent(new Event('input'));
        htmx.trigger('#log_explorer_form', 'update-query');
      };
    }
  |]

  renderFacetSection "Common Filters" rootFacets facetMap False

  forM_ facetGroups $ \(groupName, facetDisplays) -> renderFacetSection groupName facetDisplays facetMap True
  where
    renderFacetSection :: Text -> [(Text, Text, (Text -> Text))] -> HM.HashMap Text [FacetValue] -> Bool -> Html ()
    renderFacetSection sectionName facetDisplays facetMap collapsed = div_ [class_ "facet-section-group"] do
      label_ [class_ "p-3 bg-fillWeak rounded-lg cursor-pointer flex gap-3 items-center peer"] do
        input_ $ [class_ "hidden peer", type_ "checkbox", name_ $ "section-" <> sectionName] ++ [checked_ | collapsed]
        span_ [class_ "peer-checked:-rotate-90 transition-transform duration-150 flex"] $ faSprite_ "chevron-down" "regular" "w-3 h-3"
        toHtml sectionName
      div_ [class_ "xmax-h-auto divide-y peer-has-checked:max-h-0 peer-has-checked:overflow-hidden transition-[max-height] duration-150"] $ forM_ facetDisplays \(key, displayName, colorFn) ->
        whenJust (HM.lookup key facetMap) \values ->
          div_ [class_ "facet-section flex flex-col transition-all duration-150 rounded-lg group"] do
            div_ [class_ "p-3 flex justify-between items-center cursor-pointer peer"] do
              label_ [class_ "gap-3 flex items-center cursor-pointer"] do
                -- Check if this facet's checkboxes should be unchecked by default (first 4 in Common Filters)
                let commonFilterKeys = ["level", "kind", "name", "status_code"]
                    shouldBeUnchecked = sectionName == "Common Filters" && key `elem` commonFilterKeys
                input_ $ [class_ "hidden peer", type_ "checkbox", name_ $ "group-" <> key] ++ [checked_ | not shouldBeUnchecked]
                span_ [class_ "peer-checked:-rotate-90 transition-transform duration-150 flex"] $ faSprite_ "chevron-down" "regular" "w-3 h-3"
                let dotKey = T.replace "___" "." key
                span_ [term "data-tippy-content" dotKey] (toHtml displayName)
              -- Add vertical ellipsis dropdown menu
              div_ [class_ "dropdown dropdown-end"] do
                label_ [tabindex_ "0", class_ "cursor-pointer"] do
                  faSprite_ "ellipsis-vertical" "regular" "w-3 h-3"
                ul_ [tabindex_ "0", class_ "dropdown-content z-10 menu p-2 shadow bg-base-100 rounded-box w-52"] do
                  li_
                    $ a_
                      [ term "data-field" (T.replace "___" "." key)
                      , term "data-key" key
                      , [__|
                      init
                        set query to window.getQueryFromEditor()
                        if query && query.toLowerCase().includes('group by ' + @data-field.toLowerCase())
                          set my innerHTML to 'Remove group by'
                        end
                      on click
                        set query to window.getQueryFromEditor()
                        if query && query.toLowerCase().includes('group by ' + @data-field.toLowerCase())
                          call document.getElementById('filterElement').handleRemoveGroupBy(@data-field)
                        else
                          call document.getElementById('filterElement').handleAddQuery('group by ' + @data-field)
                        end
                    |]
                      ]
                      "Group by"
                  li_
                    $ a_
                      [ term "data-key" key
                      , [__|
                      init
                        set cols to (params().cols ?? "").split(",").filter(x => x != "")
                        if cols.includes(@data-key)
                          set my innerHTML to 'Remove table column'
                        end
                      on click
                        set cols to (params().cols ?? "").split(",").filter(x => x != "")
                        if cols.includes(@data-key)
                          set newCols to [...new Set(cols.filter(x => x != @data-key))].join(",")
                        else
                          set newCols to [...new Set([...cols, @data-key])].join(",")
                        end
                        call htmx.trigger('#log_explorer_form', 'submit', {cols: newCols})
                      |]
                      ]
                      "Add table column"

            div_ [class_ "pl-5 xmax-h-auto peer-has-checked:max-h-0 peer-has-checked:overflow-hidden transition-[max-height] duration-150"] do
              -- Prepare value lists and add toggle when needed
              let valuesWithIndices = zip [0 ..] values
                  (visibleValues, hiddenValues) = splitAt 5 valuesWithIndices
                  hiddenCount = length hiddenValues

                  -- Helper function to render a facet value item
                  renderFacetValue (FacetValue val count) =
                    div_ [class_ "facet-item flex justify-between items-center hover:bg-fillWeak transition-colors duration-150 rounded-md p-1 last:pb-3"] do
                      label_ [class_ "flex gap-3 items-center cursor-pointer flex-1 min-w-0 overflow-hidden"] do
                        input_
                          [ type_ "checkbox"
                          , class_ "checkbox checkbox-sm"
                          , name_ key
                          , onclick_ $ "filterByFacet('" <> T.replace "___" "." key <> "', '" <> val <> "')"
                          , term "data-tippy-content" (T.replace "___" "." key <> " == \"" <> val <> "\"")
                          ]

                        span_ [class_ "facet-value truncate", term "data-tippy-content" val] do
                          let colorClass = colorFn val
                          when (not $ T.null colorClass) $ span_ [class_ $ colorClass <> " shrink-0 w-1 h-5 rounded-sm mr-1.5"] " "
                          toHtml val
                      span_ [class_ "facet-count text-textWeak ml-1"] $ toHtml $ prettyPrintCount count

              div_ [class_ "values-container"] do
                forM_ visibleValues \(_, value) -> renderFacetValue value
                when (hiddenCount > 0) do
                  input_ [type_ "checkbox", class_ "hidden peer/more", id_ $ "more-toggle-" <> key]
                  div_ [class_ "hidden peer-checked/more:block"]
                    $ forM_ hiddenValues \(_, value) -> renderFacetValue value

                  label_ [class_ "text-textBrand px-3 py-3 cursor-pointer hover:underline peer-checked/more:hidden flex items-center gap-1", Lucid.for_ $ "more-toggle-" <> key] do
                    toHtml $ "+ More (" <> prettyPrintCount hiddenCount <> ")"

                  label_ [class_ "text-textBrand px-3 py-3 cursor-pointer hover:underline hidden peer-checked/more:flex items-center gap-1", Lucid.for_ $ "more-toggle-" <> key] do
                    toHtml $ "- Less (" <> prettyPrintCount hiddenCount <> ")"


resizer_ :: Text -> Text -> Bool -> Html ()
resizer_ targetId urlParam increasingDirection =
  div_
    [ class_ "group relative shrink-0 h-full flex items-center justify-center border-l hover:border-strokeBrand-strong cursor-ew-resize overflow-visible select-none"
    , term "data-resize-target" targetId
    , term "data-resize-direction" (if increasingDirection then "increase" else "decrease")
    , term "data-url-param" urlParam
    , [__|
        on mousedown
          add .select-none to body then
          set :startX to event.clientX then
          set :target to #{@data-resize-target} then
          set :startWidth to the :target's offsetWidth then
          set :urlParam to @data-url-param then
          set :isRightPanel to (@data-resize-direction == 'decrease')
        end
        
        on mousemove from window
          if :startX is not null
            set deltaX to (event.clientX - :startX) then
            if :isRightPanel
            then set newWidth to :startWidth - deltaX
            else set newWidth to :startWidth + deltaX end
            if newWidth < 0 set newWidth to 0 end
            set the :target's *width to newWidth + 'px'
          end
        end
        
        on mouseup from window
          if :startX is not null
            set finalWidth to the :target's offsetWidth then
            remove .select-none from body then

            call updateUrlState(:urlParam, finalWidth) then
            call localStorage.setItem('resizer-'+:urlParam, finalWidth + 'px') then
            set :startX to null
          end
        end
      |]
    ]
    do
      div_
        [ id_ "resizer"
        , class_ $ "absolute left-1/2 top-1/2 -translate-x-1/2 leading-none py-1 -translate-y-1/2 bg-bgRaised rounded-sm border border-strokeBrand-weak group-hover:border-strokeBrand-strong text-iconNeutral group-hover:text-iconBrand"
        ]
        $ faSprite_ "grip-dots-vertical" "regular" "w-4 h-5"


keepNonEmpty :: Maybe Text -> Maybe Text
keepNonEmpty Nothing = Nothing
keepNonEmpty (Just "") = Nothing
keepNonEmpty (Just a) = Just a


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM' cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM queryLibItemTitle queryLibItemID detailWM targetEventM showTraceM hxRequestM hxBoostedM jsonM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let source = "spans" -- fromMaybe "spans" sourceM
  let summaryCols = T.splitOn "," (fromMaybe "" cols')
  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
  queryAST <- parseQuery $ maybeToMonoid queryM'
  let queryText = toQText queryAST

  -- Debug logging for query parsing in apiLogH
  Log.logTrace
    "apiLogH Query Processing"
    ( AE.object
        [ "original_query_input" AE..= fromMaybe "" queryM'
        , "parsed_query_ast" AE..= show queryAST
        , "reconstructed_query_text" AE..= queryText
        , "project_id" AE..= show pid
        , "source" AE..= source
        , "target_spans" AE..= fromMaybe "" targetSpansM
        ]
    )

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
  tableAsVecE <- RequestDumps.selectLogTable pid queryAST cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE

  (queryLibRecent, queryLibSaved) <- V.partition (\x -> Projects.QLTHistory == (x.queryType)) <$> Projects.queryLibHistoryForUser pid sess.persistentSession.userId

  -- Get facet summary for the time range specified
  facetSummary <- Facets.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)

  freeTierExceeded <-
    dbtToEff
      $ if project.paymentPlan == "Free"
        then (> 1000) <$> RequestDumps.getLastSevenDaysTotalRequest pid
        else pure False

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Explorer"
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/openapi-docs/"
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              label_ [class_ "cursor-pointer border border-strokeStrong rounded-lg flex shadow-sm"] do
                input_ [type_ "checkbox", id_ "streamLiveData", class_ "hidden"]
                span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-3 items-center", data_ "tippy-content" "pause live data stream"] $ faSprite_ "pause" "solid" "h-4 w-4"
                span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex  py-1 px-3 items-center", data_ "tippy-content" "stream live data"] $ faSprite_ "play" "regular" "h-4 w-4"
              Components.timepicker_ (Just "log_explorer_form") currentRange
              Components.refreshButton_
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-md p-0 tabs-outline items-center border"] do
              a_
                [onclick_ "window.setQueryParamAndReload('source', 'spans')", role_ "tab", class_ $ "tab h-auto! " <> if source == "spans" then "tab-active text-textStrong " else ""]
                "Events"
                -- a_ [onclick_ "window.setQueryParamAndReload('source', 'metrics')", role_ "tab", class_ $ "tab py-1.5 h-auto! " <> if source == "metrics" then "tab-active" else ""] "Metrics"
          }

  case tableAsVecM of
    Just tableAsVec -> do
      let (requestVecs, colNames, resultCount) = tableAsVec
          curatedColNames = nubOrd $ curateCols summaryCols colNames
          colIdxMap = listToIndexHashMap colNames
          reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
          reqFirstCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? 0)
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source False
          recentLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' reqFirstCreatedAtM sinceM fromM toM (Just "loadmore") source True

          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' Nothing Nothing Nothing Nothing Nothing source False
          -- additionalReqsVec <-
          --   if (null traceIDs)
          --     then pure []
          --     else do
          --       -- rs <- RequestDumps.selectChildSpansAndLogs pid summaryCols (V.filter (/= "") traceIDs) (fromD, toD)
          --       pure []
          -- let finalVecs = requestVecs <> additionalReqsVec
          serviceNames = V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") requestVecs
          colors = getServiceColors (V.catMaybes serviceNames)
      let page =
            ApiLogsPageData
              { pid
              , resultCount
              , requestVecs = requestVecs
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
              }
      case (layoutM, hxRequestM, hxBoostedM, jsonM) of
        (Just "SaveQuery", _, _, _) -> addRespHeaders $ LogsQueryLibrary pid queryLibSaved queryLibRecent
        (Just "resultTable", Just "true", _, _) -> addRespHeaders $ LogsGetResultTable page False
        (Just "virtualTable", _, _, _) -> addRespHeaders $ LogsGetVirtuaTable page
        (Just "all", Just "true", _, Nothing) -> addRespHeaders $ LogsGetResultTable page True
        (_, _, _, Just _) -> addRespHeaders $ LogsGetJson requestVecs colors nextLogsURL resetLogsURL recentLogsURL
        _ -> addRespHeaders $ LogPage $ PageCtx bwconf page
    Nothing -> do
      case (layoutM, hxRequestM, hxBoostedM) of
        (Just "loadmore", Just "true", _) -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ LogsGetErrorSimple ""
        (Just "resultTable", Just "true", _) -> addRespHeaders $ LogsGetErrorSimple "Something went wrong"
        (Just "all", Just "true", _) -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ LogsGetErrorSimple ""
        _ -> addRespHeaders $ LogsGetError $ PageCtx bwconf "Something went wrong"


data LogsGet
  = LogPage (PageCtx ApiLogsPageData)
  | LogsGetResultTable ApiLogsPageData Bool
  | LogsGetVirtuaTable ApiLogsPageData
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text
  | LogsGetJson (V.Vector (V.Vector AE.Value)) (HM.HashMap Text Text) Text Text Text
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetResultTable page bol) = toHtml $ virtualTableTrigger page
  toHtml (LogsGetVirtuaTable page) = toHtml $ virtualTable page
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-red-500"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtml (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL) = span_ [] $ show $ AE.object ["logsData" AE..= vecs, "serviceColors" AE..= colors, "nextUrl" AE..= nextLogsURL, "resetLogsUrl" AE..= resetLogsURL, "recentUrl" AE..= recentLogsURL]
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL) = AE.object ["logsData" AE..= vecs, "serviceColors" AE..= colors, "nextUrl" AE..= nextLogsURL, "resetLogsUrl" AE..= resetLogsURL, "recentUrl" AE..= recentLogsURL]
  toJSON _ = AE.object []


logQueryBox_ :: Projects.ProjectId -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
logQueryBox_ pid currentRange source targetSpan query queryLibRecent queryLibSaved = do
  Components.modal_ "saveQueryMdl" "" $ form_
    [ class_ "flex flex-col p-3 gap-3"
    , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer?layout=SaveQuery"
    , hxVals_ "js:{query:window.getQueryFromEditor()}"
    , hxTarget_ "#queryLibraryParentEl"
    , hxSwap_ "outerHTML"
    , hxSelect_ "#queryLibraryParentEl"
    , hxPushUrl_ "false"
    ]
    do
      strong_ "Please input a title for your query"
      input_ [type_ "hidden", value_ "", name_ "queryLibId", id_ "queryLibId"]
      input_ [class_ "input input-md", placeholder_ "query title", name_ "queryTitle"]
      button_ [type_ "submit", class_ "btn cursor-pointer bg-linear-to-b from-[#067cff] to-[#0850c5] text-white"] "Save"
  form_
    [ hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
    , -- , hxPushUrl_ "true"
      hxTrigger_ "update-query from:#filterElement, submit, update-query from:window"
    , hxVals_ "js:{...{layout:'resultTable', ...params()}}"
    , hxTarget_ "#resultTableInner"
    , hxSwap_ "outerHTML"
    , id_ "log_explorer_form"
    , hxIndicator_ "#run-query-indicator"
    , [__| on keydown if event.key is 'Enter' halt |]
    , class_ "flex flex-col gap-1"
    ]
    do
      div_ [class_ "flex flex-col gap-2 items-stretch justify-center group/fltr"] do
        div_ [class_ "p-1 flex-1 flex flex-col gap-2  bg-fillWeaker rounded-lg border border-strokeWeak group-has-[.ai-search:checked]/fltr:border-2 group-has-[.ai-search:checked]/fltr:border-iconBrand group-has-[.ai-search:checked]/fltr:shadow-xs shadow-strokeBrand-weak"] do
          input_
            [ class_ "hidden ai-search"
            , type_ "checkbox"
            , id_ "ai-search-chkbox"
            , [__|on change if me.checked then call #ai-search-input.focus() end
                  on keydown[key=='Space' and shiftKey] from document set #ai-search-chkbox.checked to true
                  |]
            ]
          script_
            [text|
            document.addEventListener('keydown', function(e) {
              if (e.key === "?" && !e.ctrlKey && !e.metaKey && !e.altKey && (e.target.tagName !== 'INPUT') && (e.target.tagName !== 'TEXTAREA') && (e.target.contentEditable !== 'true')) {
                e.preventDefault();
                e.stopPropagation();
                document.getElementById("ai-search-chkbox").checked = true;
                document.getElementById("ai-search-input").focus()
                document.getElementById("ai-search-input").value=""
              }
            });
            |]
          div_ [class_ "w-full gap-2 items-center px-2 hidden group-has-[.ai-search:checked]/fltr:flex"] do
            faSprite_ "sparkles" "regular" "h-4 w-4 inline-block text-iconBrand"
            input_
              [ class_ "border-0 w-full flex-1 p-2 outline-none peer"
              , placeholder_ "Ask. Eg: Logs with errors. Hit Enter to submit"
              , id_ "ai-search-input"
              , autofocus_
              , required_ "required"
              , name_ "input"
              , hxPost_ $ "/p/" <> pid.toText <> "/log_explorer/ai_search"
              , hxTrigger_ "input[this.value.trim().length > 0] changed delay:500ms"
              , hxSwap_ "none"
              , hxExt_ "json-enc"
              , term "hx-validate" "false"
              , hxIndicator_ "#ai-search-loader"
              , [__|on keydown[key=='Escape'] set #ai-search-chkbox.checked to false
                   on keydown[key=='Enter'] 
                     if this.value.trim().length > 0 
                       then halt then trigger htmx:trigger 
                     end
                   on htmx:afterRequest 
                     if event.detail.successful 
                       then 
                         call JSON.parse(event.detail.xhr.responseText) set result to it
                         if result.query
                           then 
                             call #filterElement.handleAddQuery(result.query, true)
                         end
                     else
                       if event.detail.xhr.responseText and event.detail.xhr.responseText.includes('INVALID_QUERY_ERROR')
                         then
                           send errorToast(value:['Could not understand your query. Please try rephrasing it or use a more specific request.']) to <body/>
                       end
                     end|]
              ]
            span_ [class_ "htmx-indicator", id_ "ai-search-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin"
            a_
              [ class_ "px-3 py-0.5 inline-flex gap-2 items-center cursor-pointer border text-textDisabled shadow-strokeBrand-weak hover:border-strokeBrand-weak rounded-sm peer-valid:border-strokeBrand-strong peer-valid:text-textBrand peer-valid:shadow-md"
              , onclick_ "htmx.trigger('#ai-search-input', 'htmx:trigger')"
              ]
              do
                faSprite_ "arrow-right" "regular" "h-4 w-4"
                "Submit"
            label_ [Lucid.for_ "ai-search-chkbox", class_ "cursor-pointer p-1", data_ "tippy-content" "Collapse APItoolkit AI without losing your query"] $ faSprite_ "arrows-minimize" "regular" "h-4 w-4 inline-block text-iconBrand"

          div_ [class_ "flex flex-1 gap-2 justify-between items-stretch"] do
            queryLibrary_ pid queryLibSaved queryLibRecent
            div_ [id_ "queryBuilder", class_ "flex-1 flex items-center"] $ termRaw "query-editor" [id_ "filterElement", class_ "w-full h-full flex items-center", term "default-value" (fromMaybe "" query)] ("" :: Text)
            div_ [class_ "gap-[2px] flex items-center"] do
              span_ "in"
              -- TODO: trigger update-query instead
              select_
                [ class_ "ml-1 select select-sm w-full max-w-xs h-full bg-transparent border-strokeStrong"
                , name_ "target-spans"
                , id_ "spans-toggle"
                , onchange_ "htmx.trigger('#log_explorer_form', 'submit')"
                ]
                do
                  let target = fromMaybe "all-spans" targetSpan
                  option_ (value_ "all-spans" : ([selected_ "true" | target == "all-spans"])) "All spans"
                  option_ (value_ "root-spans" : ([selected_ "true" | target == "root-spans"])) "Trace Root Spans"
                  option_ (value_ "service-entry-spans" : ([selected_ "true" | target == "service-entry-spans"])) "Service Entry Spans"
            div_ [class_ "dropdown dropdown-hover dropdown-bottom dropdown-end"] do
              div_ [class_ "rounded-lg px-3 py-2 text-slate-700 inline-flex items-center border border-strokeStrong h-full", tabindex_ "0", role_ "button"] $ faSprite_ "floppy-disk" "regular" "h-5 w-5"
              ul_ [tabindex_ "0", class_ "dropdown-content border menu bg-base-100 rounded-box z-1 w-60 p-2 shadow-lg h-full"] do
                li_ $ label_ [Lucid.for_ "saveQueryMdl"] "Save query to Query Library"
            -- li_ $ a_ [] "Save query as an Alerts"
            -- li_ $ a_ [] "Save result to a dashboard"
            button_
              [type_ "submit", class_ "leading-none rounded-lg px-3 py-2 cursor-pointer !h-auto btn btn-primary"]
              do
                span_ [id_ "run-query-indicator", class_ "refresh-indicator htmx-indicator query-indicator loading loading-dots loading-sm"] ""
                faSprite_ "magnifying-glass" "regular" "h-4 w-4 inline-block"
      div_ [class_ "flex items-between justify-between"] do
        div_ [class_ "flex items-center gap-2"] do
          visualizationTabs_
          span_ [class_ "text-textDisabled mx-2 text-xs"] "|"
          -- Query Builder for GROUP BY, AGG, SORT, LIMIT
          termRaw "query-builder" [term "query-editor-selector" "#filterElement"] ("" :: Text)

        div_ [class_ "", id_ "resultTableInner"] pass

        div_ [class_ "flex justify-end  gap-2 "] do
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded-sm toggle-chart"] >> span_ "timeline"


queryLibrary_ :: Projects.ProjectId -> V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
queryLibrary_ pid queryLibSaved queryLibRecent = div_ [class_ "dropdown dropdown-bottom dropdown-start", id_ "queryLibraryParentEl"] do
  div_ [class_ "cursor-pointer relative  text-textWeak rounded-lg border border-strokeStrong h-full flex gap-2 items-center px-2 mb-2", tabindex_ "0", role_ "button"]
    $ (toHtml "Presets" >> faSprite_ "chevron-down" "regular" "w-3 h-3")
  div_ [class_ "dropdown-content z-20"] $ div_ [class_ "tabs tabs-box tabs-md tabs-outline items-center bg-fillWeak p-0 h-full", role_ "tablist", id_ "queryLibraryTabListEl"] do
    tabPanel_ "Saved" (queryLibraryContent_ "Saved" queryLibSaved)
    tabPanel_ "Recent" (queryLibraryContent_ "Recent" queryLibRecent)
  where
    tabPanel_ :: Text -> Html () -> Html ()
    tabPanel_ label content = do
      input_ $ [type_ "radio", name_ "querylib", role_ "tab", class_ "tab", Aria.label_ label] <> [checked_ | label == "Saved"]
      div_ [role_ "tabpanel", class_ "tab-content bg-bgBase shadow-lg rounded-box h-full max-h-[60dvh] w-[40vw] space-y-2 overflow-y-scroll"] content

    queryLibraryContent_ :: Text -> V.Vector Projects.QueryLibItem -> Html ()
    queryLibraryContent_ label items = do
      searchBar_ label
      div_ [class_ $ "border divide-y rounded-xl p-3 dataLibContent" <> label] $ V.forM_ items queryLibItem_

    searchBar_ :: Text -> Html ()
    searchBar_ label = div_ [class_ "flex gap-2 sticky top-0 p-3 bg-bgBase z-20"] do
      label_ [class_ "input input-md flex items-center gap-2 flex-1"] do
        faSprite_ "magnifying-glass" "regular" "h-4 w-4 opacity-70"
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , term "data-filterParent" $ "dataLibContent" <> label
          , [__|on keyup
                     if the event's key is 'Escape' set my value to '' then trigger keyup
                     else show <.group/> in .{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()|]
          ]
      when (label == "Saved") do
        label_ [class_ "tabs tabs-md tabs-box tabs-outline bg-slate-200 text-slate-50 shrink items-center", role_ "tablist"] do
          input_ [class_ "hidden", type_ "checkbox", id_ "queryLibraryGroup"]
          div_ [role_ "tab", class_ "tab h-full bg-slate-50 group-has-[#queryLibraryGroup:checked]/pg:bg-transparent", term "data-tippy-content" "My Queries"] $ faSprite_ "user" "solid" "w-5 h-5"
          div_ [role_ "tab", class_ "tab h-full group-has-[#queryLibraryGroup:checked]/pg:bg-slate-50", term "data-tippy-content" "All team Queries"] $ faSprite_ "users" "solid" "w-5 h-5"


queryLibItem_ :: Projects.QueryLibItem -> Html ()
queryLibItem_ qli =
  div_ [class_ $ "clear p-3 space-y-2 hover:bg-fillWeaker cursor-pointer group " <> if qli.byMe then "" else "hidden group-has-[#queryLibraryGroup:checked]/pg:block"] do
    div_ [class_ "inline-flex gap-2 float-right"] do
      div_ [class_ "flex opacity-0 transition-opacity duration-300 group-hover:opacity-100 gap-2"] do
        a_
          [ class_ "tooltip"
          , term "data-tip" "run query"
          , term "data-query" $ qli.queryText
          , [__| on click call #filterElement.handleAddQuery({detail: JSON.parse(@data-query)})|]
          ]
          $ faSprite_ "play" "regular" "h-4 w-4"
        a_ [class_ "tooltip", term "data-tip" "copy query to clipboard", [__|install Copy(content: (next <.queryText/> ))|]] $ faSprite_ "copy" "regular" "h-4 w-4"
        when qli.byMe $ a_ [class_ "tooltip", term "data-tip" "edit query title", [__|on click set #queryLibId.value to @data-queryId then set #saveQueryMdl.checked to true|], term "data-queryId" qli.id.toText] $ faSprite_ "pen-to-square" "regular" "h-4 w-4"
        when qli.byMe
          $ a_
            [ class_ "tooltip"
            , term "data-tip" "delete query"
            , hxGet_ $ "/p/" <> qli.projectId.toText <> "/log_explorer?layout=DeleteQuery&queryLibId=" <> qli.id.toText
            , hxVals_ "js:{query:window.getQueryFromEditor()}"
            , hxTarget_ "#queryLibraryTabListEl"
            , hxSwap_ "outerHTML"
            , hxSelect_ "#queryLibraryTabListEl"
            , hxPushUrl_ "false"
            ]
          $ faSprite_ "trash-can" "regular" "h-4 w-4"
      label_ [class_ ""] do
        input_ [class_ "hidden", type_ "checkbox"]
        span_ [class_ ""] $ faSprite_ "ellipsis-vertical" "regular" "h-4 w-4"
        ul_ [class_ "hidden peer-checked:block z-30"] do
          li_ "Send query to alert"
          li_ "Send query to a dashboard"
    strong_ $ whenJust qli.title \title -> (toHtml title)
    pre_
      $ code_ [class_ "language-js bg-transparent! queryText whitespace-pre-wrap break-words"]
      $ toHtml qli.queryText
    div_ [class_ "gap-3 flex"] $ time_ [datetime_ "", term "data-tippy-content" "created on"] (toHtml $ displayTimestamp $ formatUTC qli.createdAt) >> when qli.byMe " by me"


data ApiLogsPageData = ApiLogsPageData
  { pid :: Projects.ProjectId
  , resultCount :: Int
  , requestVecs :: V.Vector (V.Vector AE.Value)
  , cols :: [Text]
  , colIdxMap :: HM.HashMap Text Int
  , nextLogsURL :: Text
  , resetLogsURL :: Text
  , recentLogsURL :: Text
  , currentRange :: Maybe Text
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
  }


virtualTableTrigger :: ApiLogsPageData -> Html ()
virtualTableTrigger page = do
  div_ [id_ "resultTableInner"] do
    let vecs = decodeUtf8 $ AE.encode page.requestVecs
        cols = decodeUtf8 $ AE.encode page.cols
        colIdxMap = decodeUtf8 $ AE.encode page.colIdxMap
        serviceColors = decodeUtf8 $ AE.encode page.serviceColors
        nextLogsURL = decodeUtf8 $ AE.encode page.nextLogsURL
        recentFetchUrl = decodeUtf8 $ AE.encode page.recentLogsURL
    script_
      [text|
        if(window.logListTable) {
           window.logListTable.updateTableData($vecs, $cols, $colIdxMap, $serviceColors, $nextLogsURL, $recentFetchUrl)
          }
    |]


virtualTable :: ApiLogsPageData -> Html ()
virtualTable page = do
  termRaw
    "log-list"
    [ id_ "resultTable"
    , class_ "w-full divide-y shrink-1 flex flex-col h-full min-w-0"
    ]
    ("" :: Text)
  let logs = decodeUtf8 $ AE.encode page.requestVecs
      cols = decodeUtf8 $ AE.encode page.cols
      colIdxMap = decodeUtf8 $ AE.encode page.colIdxMap
      serviceColors = decodeUtf8 $ AE.encode page.serviceColors
      nextfetchurl = page.nextLogsURL
      recentFetchUrl = page.recentLogsURL
      resetLogsURL = page.resetLogsURL
      projectid = page.pid.toText
  script_
    [text|
      window.virtualListData = {
       requestVecs: $logs,
       cols: $cols,
       colIdxMap: $colIdxMap,
       serviceColors: $serviceColors,
       nextFetchUrl: `$nextfetchurl`,
       recentFetchUrl: `$recentFetchUrl`,
       resetLogsUrl: `$resetLogsURL`,
       projectId: "$projectid"
      }
   |]


visualizationTabs_ :: Html ()
visualizationTabs_ =
  div_ [class_ "tabs tabs-box tabs-outline tabs-xs bg-gray-100 p-1 rounded-lg", id_ "visualizationTabs", role_ "tablist"] do
    forM_ visTypes $ \(icon, label, vizType, emoji) -> label_ [class_ "tab !shadow-none !border-strokeWeak flex gap-1"] do
      input_ $ [type_ "radio", name_ "visualization", id_ $ "viz-" <> vizType, value_ vizType] <> [checked_ | vizType == "logs"]
      span_ [class_ "text-iconNeutral leading-none"] $ toHtml emoji -- faSprite_ icon "regular" "w-3 h-2"
      span_ [] $ toHtml label


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-y-hidden pb-2 group/pg", id_ "apiLogsPage"] do
    template_ [id_ "loader-tmp"] $ span_ [class_ "loading loading-dots loading-md"] ""
    when page.exceededFreeTier $ freeTierLimitExceededBanner page.pid.toText
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
    div_ [] do
      logQueryBox_ page.pid page.currentRange page.source page.targetSpans page.query page.queryLibRecent page.queryLibSaved

      div_ [class_ "timeline flex flex-row gap-4 mt-3 group-has-[#viz-logs:not(:checked)]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full", style_ "aspect-ratio: 10 / 1;"] do
        Widget.widget_ $ (def :: Widget.Widget){Widget.query = Just "timechart count(*)", Widget.unit = Just "rows", Widget.title = Just "All traces", Widget.hideLegend = Just True, Widget._projectId = Just page.pid, Widget.standalone = Just True, Widget.yAxis = Just (def{showOnlyMaxLabel = Just True}), Widget.allowZoom = Just True, Widget.showMarkArea = Just True, Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})}

        Widget.widget_
          $ (def :: Widget.Widget)
            { Widget.wType = WTTimeseriesLine
            , Widget.standalone = Just True
            , Widget.title = Just "Latency percentiles (ms)"
            , Widget.hideSubtitle = Just True
            , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
            , Widget.summarizeBy = Just Widget.SBMax
            , Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})
            , Widget.sql =
                Just
                  [text| SELECT timeB, COALESCE(value, 0)::float AS value, quantile
                              FROM ( SELECT extract(epoch from time_bucket('1h', timestamp))::integer AS timeB,
                                      ARRAY[
                                        COALESCE((approx_percentile(0.50, percentile_agg(duration)) / 1000000.0), 0)::float,
                                        COALESCE((approx_percentile(0.75, percentile_agg(duration)) / 1000000.0), 0)::float,
                                        COALESCE((approx_percentile(0.90, percentile_agg(duration)) / 1000000.0), 0)::float,
                                        COALESCE((approx_percentile(0.95, percentile_agg(duration)) / 1000000.0), 0)::float
                                      ] AS values,
                                      ARRAY['p50', 'p75', 'p90', 'p95'] AS quantiles
                                FROM otel_logs_and_spans
                                WHERE project_id='{{project_id}}'
                                  {{time_filter}} {{query_ast_filters}}
                                  AND duration IS NOT NULL
                                GROUP BY timeB
                                HAVING COUNT(*) > 0
                              ) s,
                            LATERAL unnest(s.values, s.quantiles) AS u(value, quantile)
                            WHERE value IS NOT NULL;
                        |]
            , Widget.unit = Just "ms"
            , Widget.hideLegend = Just True
            , Widget._projectId = Just page.pid
            }

    div_ [class_ "flex h-full gap-3.5 overflow-y-hidden"] do
      -- FACETS
      div_ [class_ "w-68 text-sm shrink-0 flex flex-col h-full overflow-y-scroll gap-2 group-has-[.toggle-filters:checked]/pg:max-w-0 group-has-[.toggle-filters:checked]/pg:overflow-hidden ", id_ "facets-container"] do
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
      div_ [class_ "grow relative flex flex-col shrink-1 min-w-0 w-full h-full", style_ $ "xwidth: " <> dW, id_ "logs_list_container"] do
        div_ [class_ "flex gap-2  pt-1 text-sm -mb-6 z-10 w-max"] do
          label_ [class_ "gap-1 flex items-center cursor-pointer"] do
            faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 "
            span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
            span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
            "filters"
            input_ [type_ "checkbox", class_ "toggle-filters hidden", id_ "toggle-filters", onchange_ "localStorage.setItem('toggle-filter-checked', this.checked)"]
            script_ "document.getElementById('toggle-filters').checked = localStorage.getItem('toggle-filter-checked') === 'true';"
          span_ [class_ "text-slate-200"] "|"
          div_ [class_ ""] $ span_ [class_ "text-slate-950"] (toHtml $ prettyPrintCount page.resultCount) >> span_ [class_ "text-slate-600"] (toHtml (" rows found"))
        div_ [class_ $ "absolute top-0 right-0  w-full h-full overflow-scroll c-scroll z-50 bg-white transition-all duration-100 " <> if showTrace then "" else "hidden", id_ "trace_expanded_view"] do
          whenJust page.showTrace \trId -> do
            let url = "/p/" <> page.pid.toText <> "/traces/" <> trId
            span_ [class_ "loading loading-dots loading-md"] ""
            div_ [hxGet_ url, hxTarget_ "#trace_expanded_view", hxSwap_ "innerHtml", hxTrigger_ "intersect one"] pass

        -- Visualization container with conditional display based on radio selection
        div_ [class_ "hidden group-has-[#viz-logs:checked]/pg:block h-full"] do
          virtualTable page

        div_ [class_ "group-has-[#viz-logs:checked]/pg:hidden h-full"] do
          let pid = page.pid.toText
          script_
            [class_ "hidden"]
            [text| var widgetJSON = { 
                  "id": "visualization-widget",
                  "type": "timeseries_line", 
                  "title": "Visualization",
                  "standalone": true,
                  "allow_zoom": true,
                  "_project_id": "$pid",
                  "_center_title": true, 
                  "layout": {"w": 6, "h": 4}
                };
                
                document.addEventListener('DOMContentLoaded', function() {
                  const updateWidget = () => {
                    const visType = document.querySelector('input[name="visualization"]:checked').value;
                    if (visType) {widgetJSON.type = visType}
                    document.getElementById('visualization-widget-container').dispatchEvent(new Event('update-widget'));
                  };
                  
                  // Update visualization when tabs change
                  document.querySelectorAll('input[name="visualization"]').forEach(radio => {
                    radio.addEventListener('change', updateWidget);
                  });
                  
                  updateWidget();
                });
                |]
          div_
            [ id_ "visualization-widget-container"
            , class_ "h-full w-full"
            , hxPost_ "/widget"
            , hxTrigger_ "intersect once, update-widget"
            , hxTarget_ "this"
            , hxSwap_ "innerHTML"
            , hxVals_ "js:{...widgetJSON}"
            , hxExt_ "json-enc"
            ]
            ""

      resizer_ "log_details_container" "details_width" False

      div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0", id_ "log_details_container"] do
        span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
        whenJust page.targetEvent \te -> do
          script_
            [text|
            const detailsContainer = document.getElementById('log_details_container');
            if (detailsContainer) {
              const queryWidth = new URLSearchParams(window.location.search).get('details_width');
              const storedWidth = localStorage.getItem('resizer-details_width');
              
              if (queryWidth) detailsContainer.style.width = queryWidth + 'px';
              else if (storedWidth && !storedWidth.endsWith('px')) detailsContainer.style.width = storedWidth + 'px';
              else if (storedWidth) detailsContainer.style.width = storedWidth;
              else detailsContainer.style.width = '30%';
            }
          |]
          let url = "/p/" <> page.pid.toText <> "/log_explorer/" <> te
          div_ [hxGet_ url, hxTarget_ "#log_details_container", hxSwap_ "innerHtml", hxTrigger_ "intersect one", hxIndicator_ "#details_indicator"] pass

  jsonTreeAuxillaryCode page.pid page.query
  queryEditorInitializationCode page.queryLibRecent page.queryLibSaved


aiSearchH :: Projects.ProjectId -> AE.Value -> ATAuthCtx (RespHeaders AE.Value)
aiSearchH _pid requestBody = do
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
          result <- callOpenAIAPI inputText
          case result of
            Left errMsg -> do
              addErrorToast "AI search failed" (Just errMsg)
              throwError Servant.err502{Servant.errBody = encodeUtf8 errMsg}
            Right kqlQuery -> addRespHeaders $ AE.object ["query" AE..= kqlQuery]
  where
    callOpenAIAPI :: Text -> ATAuthCtx (Either Text Text)
    callOpenAIAPI input = do
      authCtx <- Effectful.Reader.Static.ask @AuthContext
      let config = authCtx.env
      let openAI =
            OpenAI
              { apiKey = config.openaiApiKey
              , openAIModelName = "gpt-4o-mini"
              , callbacks = []
              , baseUrl = Nothing
              }
      let fullPrompt = systemPrompt <> "\n\nUser query: " <> input

      -- Use langchain-hs to generate response
      result <- liftIO $ LLM.generate openAI fullPrompt Nothing
      case result of
        Left err -> pure $ Left $ "LLM Error: " <> T.pack err
        Right response ->
          -- Clean up the response to extract just the KQL query
          let cleanedResponse = T.strip $ T.takeWhile (/= '\n') response
           in -- Check if the response indicates an invalid query
              if "Please provide a query"
                `T.isInfixOf` cleanedResponse
                || "I need more"
                  `T.isInfixOf` cleanedResponse
                || "Could you please"
                  `T.isInfixOf` cleanedResponse
                || T.length cleanedResponse
                  < 3
                then pure $ Left "INVALID_QUERY_ERROR"
                else pure $ Right cleanedResponse

    systemPrompt :: Text
    systemPrompt =
      T.unlines
        [ "You are a helpful assistant that converts natural language queries to KQL (Kusto Query Language) filter expressions."
        , ""
        , Schema.generateSchemaForAI Schema.telemetrySchema
        , ""
        , "Available Operators:"
        , "- Comparison: == != > < >= <="
        , "- Set operations: in !in (e.g., method in (\"GET\", \"POST\"))"
        , "- Text search: has !has (case-insensitive word search)"
        , "- Text collections: has_any has_all (e.g., tags has_any [\"urgent\", \"critical\"])"
        , "- String operations: contains !contains startswith !startswith endswith !endswith"
        , "- Pattern matching: matches =~ (regex, e.g., email matches /.*@company\\.com/)"
        , "- Logical: AND OR (or lowercase and or)"
        , "- Duration values: 100ms 5s 2m 1h (nanoseconds, microseconds, milliseconds, seconds, minutes, hours)"
        , ""
        , "Examples:"
        , "- \"show me errors\" -> level == \"ERROR\""
        , "- \"POST requests\" -> attributes.http.request.method == \"POST\""
        , "- \"slow requests\" -> duration > 500ms"
        , "- \"500 errors\" -> attributes.http.response.status_code == \"500\""
        , "- \"GET or POST requests\" -> method in (\"GET\", \"POST\")"
        , "- \"messages containing error\" -> message contains \"error\""
        , "- \"logs with urgent or critical tags\" -> tags has_any [\"urgent\", \"critical\"]"
        , "- \"paths starting with /api\" -> path startswith \"/api\""
        , "- \"emails from company.com\" -> email matches /.*@company\\.com/"
        , "- \"requests taking more than 1 second\" -> duration > 1s"
        , ""
        , "Return only the KQL filter expression, no explanations."
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


-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Maybe Text -> Html ()
jsonTreeAuxillaryCode pid query = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu  origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-bgBase ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-hidden z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1", role_ "none"] do
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{...{layout:'resultTable',cols:toggleColumnToSummary(event), ...params()}}"
          , hxTarget_ "#resultTableInner"
          , hxSwap_ "outerHTML"
          , -- , hxIndicator_ "#query-indicator"
            [__|init set fp to (closest @data-field-path) then
                  if params().cols.split(",").includes(fp) then set my innerHTML to 'Remove column' end|]
          ]
          "Add field as Column"
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , [__|on click if 'clipboard' in window.navigator then
                    call navigator.clipboard.writeText((previous <.log-item-field-value/>)'s innerText)
                    send successToast(value:['Value has been added to the Clipboard']) to <body/>
                    halt
                  end|]
          ]
          "Copy field value"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , onclick_ "filterByField(event, 'Eq')"
          ]
          "Filter by field"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , onclick_ "filterByField(event, 'NotEq')"
          ]
          "Exclude field"

  script_
    [text|
    function filterByField(event, operation) {
        const pathsToRemap = [
          ["request_headers", "attributes.http.request.header"],
          ["response_headers", "attributes.http.response.header"],
          ["response_body", "body.response_body"],
          ["request_body", "body.request_body"],
          ["method", "attributes.http.request.method"],
          ["query_params", "attributes.http.request.query_params"],
          ["path_params", "attributes.http.request.path_params"],
          ["host", "attributes.net.host.name"],
          ["urlPath", "attributes.http.route"],
          ["raw_url", "attributes.http.target"],
          ["status_code", "attributes.http.response.status_code"],
        ]
        let { fieldPath: path, fieldValue: value } = event.target.closest('[data-field-path]').dataset;

        pathsToRemap.forEach(([from, to]) => {
          if (path.startsWith(from)) {
            path = path.replace(from, to)
          }
        })

        const operator = operation === 'Eq' ? '==' : operation === 'NotEq' ? '!=' : '==';
        document.getElementById("filterElement").handleAddQuery(path + ' ' + operator + ' ' + value);
    }

    var toggleColumnToSummary = (e)=>{
      const cols = (params().cols??"").split(",").filter(x=>x!="");
      const subject = e.target.closest('.log-item-field-parent').dataset.fieldPath;
      if (cols.includes(subject)) {
        return [...new Set(cols.filter(x=>x!=subject))].join(",");
      }
      cols.push(subject)
      return [... new Set (cols)].join (",")
    }


    var removeNamedColumnToSummary = (namedCol) => {
      const cols = (params().cols ?? '').split(',').filter((x) => x != '')
      return [...new Set(cols.filter((x) => namedCol.toLowerCase() != x.replaceAll('.', '•').replaceAll('[', '❲').replaceAll(']', '❳').toLowerCase()))].join(',')
    }

|]


queryEditorInitializationCode :: V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
queryEditorInitializationCode queryLibRecent queryLibSaved = do
  let queryLibData = queryLibRecent <> queryLibSaved
      queryLibDataJson = decodeUtf8 $ AE.encode queryLibData
      schemaJson = decodeUtf8 $ AE.encode Schema.telemetrySchemaJson
      popularQueriesJson = decodeUtf8 $ AE.encode Schema.popularOtelQueriesJson
  script_
    [text|
    setTimeout(() => {
      const editor = document.getElementById('filterElement');
      if (editor) {
        if (editor.setQueryLibrary) {
          const queryLibraryData = $queryLibDataJson;
          editor.setQueryLibrary(queryLibraryData);
        }
        
        if (window.schemaManager && window.schemaManager.setSchemaData) {
          const schemaData = $schemaJson;
          window.schemaManager.setSchemaData('spans', schemaData);
        }
        
        if (editor.setPopularSearches) {
          const popularQueries = $popularQueriesJson;
          editor.setPopularSearches(popularQueries);
        }
      }
    }, 100);
    |]
