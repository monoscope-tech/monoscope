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
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
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
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (pSource, parseQueryToAST, toQText)
import Relude hiding (ask)
import Servant qualified
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils (checkFreeTierExceeded, faSprite_, getServiceColors, listToIndexHashMap, lookupVecTextByKey, onpointerdown_, prettyPrintCount)

import Pkg.AI (callOpenAIAPI, systemPrompt)


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
        "2" -> "bg-fillSuccess-strong"
        "3" -> "bg-fillBrand-strong"
        "4" -> "bg-fillWarning-strong"
        "5" -> "bg-fillError-strong"
        _ -> "bg-fillStrong"

      methodColorFn val = case val of
        "GET" -> "bg-fillBrand-strong"
        "POST" -> "bg-fillSuccess-strong"
        "PUT" -> "bg-fillWarning-strong"
        "DELETE" -> "bg-fillError-strong"
        _ -> "bg-fillBrand-strong"

      levelColorFn val = case val of
        "ERROR" -> "bg-fillError-strong"
        "WARN" -> "bg-fillWarning-strong"
        "INFO" -> "bg-fillBrand-strong"
        "DEBUG" -> "bg-fillStrong"
        _ -> "bg-fillWeak"

      -- Group facet fields by category
      -- Define mapping of field keys to display names and color functions

      -- Root level facets (displayed at the top)
      rootFacets :: [(Text, Text, Text -> Text)]
      rootFacets =
        [ ("level", "Log Level", levelColorFn)
        , ("kind", "Kind", const "")
        , ("name", "Operation Name", const "")
        , ("status_code", "Status Code", statusColorFn)
        , ("resource___service___name", "Service", const "")
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
      
      document.querySelectorAll('input[type="checkbox"][data-field][data-value]').forEach(cb => {
        const field = cb.getAttribute('data-field');
        const value = cb.getAttribute('data-value');
        const fragment = field + ' == "' + value + '"';
        cb.checked = query.includes(fragment);
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
                      [ term "data-key" key
                      , term "data-field" (T.replace "___" "." key)
                      , class_ "flex gap-2 items-center"
                      , hxGet_ $ "/p/" <> facetSummary.projectId <> "/log_explorer"
                      , hxPushUrl_ "true"
                      , hxVals_ "js:{...{...params(), layout:'resultTable',cols:toggleColumnToSummary(event)}}"
                      , hxTarget_ "#resultTableInner"
                      , hxSwap_ "outerHTML"
                      , [__|init set fp to @data-field then
                                if params().cols.split(",").includes(fp) then 
                                  set my.querySelector('span').innerHTML to "Remove column"
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
                          , term "data-field" (T.replace "___" "." key)
                          , term "data-value" val
                          ]

                        span_ [class_ "facet-value truncate", term "data-tippy-content" val] do
                          let colorClass = colorFn val
                          unless (T.null colorClass) $ span_ [class_ $ colorClass <> " shrink-0 w-1 h-5 rounded-sm mr-1.5"] " "
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
        [ id_ $ "resizer-" <> urlParam
        , class_ "absolute left-1/2 top-1/2 z-50 -translate-x-1/2 leading-none py-1 -translate-y-1/2 bg-bgBase rounded-sm border border-strokeBrand-weak group-hover:border-strokeBrand-strong text-iconNeutral group-hover:text-iconBrand"
        ]
        $ faSprite_ "grip-dots-vertical" "regular" "w-4 h-5"


keepNonEmpty :: Maybe Text -> Maybe Text
keepNonEmpty Nothing = Nothing
keepNonEmpty (Just "") = Nothing
keepNonEmpty (Just a) = Just a


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM' cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM queryLibItemTitle queryLibItemID detailWM targetEventM showTraceM hxRequestM hxBoostedM jsonM vizTypeM = do
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
  tableAsVecE <- RequestDumps.selectLogTable pid queryAST queryText cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE

  (queryLibRecent, queryLibSaved) <- V.partition (\x -> Projects.QLTHistory == x.queryType) <$> Projects.queryLibHistoryForUser pid sess.persistentSession.userId

  -- Get facet summary for the time range specified
  facetSummary <- Facets.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)

  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Explorer"
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/api-log-explorer/"
          , freeTierExceeded = freeTierExceeded
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              label_ [class_ "cursor-pointer border border-strokeWeak rounded-lg flex shadow-xs"] do
                input_ [type_ "checkbox", id_ "streamLiveData", class_ "hidden"]
                span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-3 items-center", data_ "tippy-content" "pause live data stream"] $ faSprite_ "pause" "solid" "h-4 w-4 text-iconNeutral"
                span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex  py-1 px-3 items-center", data_ "tippy-content" "stream live data"] $ faSprite_ "play" "regular" "h-4 w-4 text-iconNeutral"
              Components.timepicker_ (Just "log_explorer_form") currentRange
              Components.refreshButton_
          , navTabs = Just $ div_ [class_ "tabs tabs-box tabs-md p-0 tabs-outline items-center border"] do
              a_
                [href_ $ "/p/" <> pid.toText <> "/log_explorer", role_ "tab", class_ $ "tab h-auto! tab-active text-textStrong"]
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
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source False
          recentLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' reqFirstCreatedAtM sinceM fromM toM (Just "loadmore") source True

          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM' cols' Nothing Nothing Nothing Nothing Nothing source False
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
              , vizType = vizTypeM
              }
      case (layoutM, hxRequestM, hxBoostedM, jsonM) of
        (Just "SaveQuery", _, _, _) -> addRespHeaders $ LogsQueryLibrary pid queryLibSaved queryLibRecent
        (Just "resultTable", Just "true", _, _) -> addRespHeaders $ LogsGetResultTable page False
        (Just "virtualTable", _, _, _) -> addRespHeaders $ LogsGetVirtuaTable page
        (Just "all", Just "true", _, Nothing) -> addRespHeaders $ LogsGetResultTable page True
        (_, _, _, Just _) -> addRespHeaders $ LogsGetJson requestVecs colors nextLogsURL resetLogsURL recentLogsURL curatedColNames colIdxMap
        _ -> addRespHeaders $ LogPage $ PageCtx bwconf page
    Nothing -> do
      case (layoutM, hxRequestM, hxBoostedM, jsonM) of
        (Just "loadmore", Just "true", _, _) -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ LogsGetErrorSimple ""
        (Just "resultTable", Just "true", _, _) -> addRespHeaders $ LogsGetErrorSimple "Something went wrong"
        (Just "all", Just "true", _, _) -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ LogsGetErrorSimple ""
        (_, _, _, Just _) -> addRespHeaders $ LogsGetErrorSimple "Failed to fetch logs data"
        _ -> addRespHeaders $ LogsGetError $ PageCtx bwconf "Something went wrong"


data LogsGet
  = LogPage (PageCtx ApiLogsPageData)
  | LogsGetResultTable ApiLogsPageData Bool
  | LogsGetVirtuaTable ApiLogsPageData
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text
  | LogsGetJson (V.Vector (V.Vector AE.Value)) (HM.HashMap Text Text) Text Text Text [Text] (HM.HashMap Text Int)
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetResultTable page bol) = toHtml $ virtualTableTrigger page
  toHtml (LogsGetVirtuaTable page) = toHtml $ virtualTable page
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-textError"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtml (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap) = span_ [] $ show $ AE.object ["logsData" AE..= vecs, "serviceColors" AE..= colors, "nextUrl" AE..= nextLogsURL, "resetLogsUrl" AE..= resetLogsURL, "recentUrl" AE..= recentLogsURL]
  toHtmlRaw = toHtml


instance AE.ToJSON LogsGet where
  toJSON (LogsGetJson vecs colors nextLogsURL resetLogsURL recentLogsURL cols colIdxMap) = AE.object ["logsData" AE..= vecs, "serviceColors" AE..= colors, "nextUrl" AE..= nextLogsURL, "resetLogsUrl" AE..= resetLogsURL, "recentUrl" AE..= recentLogsURL, "cols" AE..= cols, "colIdxMap" AE..= colIdxMap]
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
           setTimeout(()=> {
           tippy('[data-tippy-content]').forEach(t => t.destroy());
           tippy('[data-tippy-content]');
           },100)
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


-- visualizationTabs_ has been moved to Pkg.Components.LogQueryBox

apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-y-hidden overflow-x-hidden pb-2 group/pg", id_ "apiLogsPage"] do
    template_ [id_ "loader-tmp"] $ span_ [class_ "loading loading-dots loading-md"] ""
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
          }

      div_ [class_ "timeline flex flex-row gap-4 mt-3 group-has-[#viz-logs:not(:checked)]/pg:hidden group-has-[.toggle-chart:checked]/pg:hidden w-full min-h-36 ", style_ "aspect-ratio: 10 / 1;"] do
        Widget.widget_ $ (def :: Widget.Widget){Widget.query = Just "summarize count(*) by bin_auto(timestamp), status_code", Widget.unit = Just "rows", Widget.title = Just "All traces", Widget.hideLegend = Just True, Widget._projectId = Just page.pid, Widget.standalone = Just True, Widget.yAxis = Just (def{showOnlyMaxLabel = Just True}), Widget.allowZoom = Just True, Widget.showMarkArea = Just True, Widget.layout = Just (def{Widget.w = Just 6, Widget.h = Just 4})}

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
                  [text| SELECT timeB, quantile,COALESCE(value, 0)::float AS value
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
                            WHERE value IS NOT NULL;|]
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
        -- Common scrollable container for both views
        div_ [class_ "flex flex-col h-full overflow-y-auto"] do
          -- Alert configuration section (visible in charts view when alert toggle is checked)
          div_ [class_ "shrink-0 group-has-[#viz-logs:checked]/pg:hidden"] do
            alertConfigurationForm_ page.pid

          -- Visualization widget that shows when not in logs view
          div_ [class_ "flex-1 min-h-0 group-has-[#viz-logs:checked]/pg:hidden"] do
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

        -- Filters and row count header
        div_ [class_ "flex gap-2  pt-1 text-sm -mb-6 z-10 w-max bg-bgBase"] do
          label_ [class_ "gap-1 flex items-center cursor-pointer"] do
            faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 text-iconNeutral"
            span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
            span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
            "filters"
            input_ [type_ "checkbox", class_ "toggle-filters hidden", id_ "toggle-filters", onchange_ "localStorage.setItem('toggle-filter-checked', this.checked); setTimeout(() => { const editor = document.getElementById('filterElement'); if (editor && editor.refreshLayout) editor.refreshLayout(); }, 200);"]
            script_
              $ [text|
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
          div_ [class_ ""] $ span_ [class_ "text-textStrong"] (toHtml $ prettyPrintCount page.resultCount) >> span_ [class_ "text-textStrong"] (toHtml " rows")

        -- Trace view container
        div_ [class_ $ "absolute top-0 right-0  w-full h-full overflow-scroll c-scroll z-50 bg-bgBase transition-all duration-100 " <> if showTrace then "" else "hidden", id_ "trace_expanded_view"] do
          whenJust page.showTrace \trId -> do
            let url = "/p/" <> page.pid.toText <> "/traces/" <> trId
            span_ [class_ "loading loading-dots loading-md"] ""
            div_ [hxGet_ url, hxTarget_ "#trace_expanded_view", hxSwap_ "innerHtml", hxTrigger_ "intersect one", term "hx-sync" "this:replace"] pass

          -- Logs view section (also within the scrollable container)
          div_ [class_ "flex-1 min-h-0 group-has-[#viz-logs:not(:checked)]/pg:hidden flex flex-col"] do
            -- Alert configuration for logs view (only shows when alert toggle is checked)
            div_ [class_ "shrink-0"] do
              alertConfigurationForm_ page.pid

            -- Virtual table for logs
            div_ [class_ "flex-1 min-h-0"] do
              virtualTable page

      div_ [class_ $ "transition-opacity duration-200 " <> if isJust page.targetEvent then "" else "opacity-0 pointer-events-none hidden", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False

      div_ [class_ "grow-0 shrink-0 overflow-y-auto overflow-x-hidden h-full c-scroll w-0", id_ "log_details_container"] do
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

  jsonTreeAuxillaryCode page.pid page.query
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
            Right (kqlQuery, vizType) ->
              addRespHeaders
                $ AE.object
                  [ "query" AE..= kqlQuery
                  , "visualization_type" AE..= vizType
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
    ul_ [id_ "log-item-context-menu", class_ "log-item-context-menu origin-top-right absolute left-0 dropdown-content z-10 menu p-2 shadow bg-base-100 rounded-box w-52", tabindex_ "0"] do
      li_
        $ a_
          [ class_ "flex gap-2 items-center"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{...{...params(), layout:'resultTable',cols:toggleColumnToSummary(event)}}"
          , hxTarget_ "#resultTableInner"
          , hxSwap_ "outerHTML"
          , [__|init set fp to (closest @data-field-path) then
              if params().cols.split(",").includes(fp) then 
                set my.querySelector('span').innerHTML to "Remove column"
              end|]
          ]
          do
            faSprite_ "table-column" "regular" "w-4 h-4 text-iconNeutral"
            span_ [] "Add as table column"
      li_
        $ a_
          [ class_ "flex gap-2 items-center"
          , [__|on click if 'clipboard' in window.navigator then
                call navigator.clipboard.writeText((previous <.log-item-field-value/>)'s innerText)
                send successToast(value:['Value has been added to the Clipboard']) to <body/>
                halt
              end|]
          ]
          do
            faSprite_ "copy" "regular" "w-4 h-4 text-iconNeutral"
            span_ [] "Copy field value"
      li_ $ a_ [class_ "flex gap-2 items-center", onpointerdown_ "filterByField(event, 'Eq')"] do
        faSprite_ "filter-enhanced" "regular" "w-4 h-4 text-iconNeutral"
        span_ [] "Filter by field"
      li_ $ a_ [class_ "flex gap-2 items-center", onpointerdown_ "filterByField(event, 'NotEq')"] do
        faSprite_ "not-equal" "regular" "w-4 h-4 text-iconNeutral"
        span_ [] "Exclude field"
      li_
        $ a_
          [ class_ "flex gap-2 items-center"
          , [__|
              init call window.updateGroupByButtonText(event, me) end
              on refreshItem call window.updateGroupByButtonText(event, me) end

              on click
                call document.querySelector('query-builder').toggleGroupByField(closest @data-field-path) then
                trigger refreshItem on me
              end
          |]
          ]
          do
            faSprite_ "copy" "regular" "w-4 h-4 text-iconNeutral"
            span_ [] "Group by field"

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
      const cols = (params().cols||"").split(",").filter(x=>x!="");
      const subject = (e.target.closest('[data-field-path]')?.dataset.fieldPath || e.target.closest('[data-field]').dataset.field);
      const finalCols =  subject ? 
        [...new Set(cols.includes(subject) ? 
          cols.filter(x=>x!=subject) : 
          [...cols, subject])].join(",") : 
        cols.join(",");
      return finalCols;
    }


    var removeNamedColumnToSummary = (namedCol) => {
      const cols = (params().cols ?? '').split(',').filter((x) => x != '')
      return [...new Set(cols.filter((x) => namedCol.toLowerCase() != x.replaceAll('.', '•').replaceAll('[', '❲').replaceAll(']', '❳').toLowerCase()))].join(',')
    }

|]


-- | Render alert configuration form for creating log-based alerts
alertConfigurationForm_ :: Projects.ProjectId -> Html ()
alertConfigurationForm_ pid = do
  div_ [class_ "alert-configuration bg-fillWeaker rounded-2xl p-4 mb-3 border border-strokeWeak hidden group-has-[#create-alert-toggle:checked]/pg:block shrink-0 group/alt"] do
    -- Header section (more compact)
    div_ [class_ "flex items-center justify-between mb-3"] do
      div_ [class_ "flex items-center gap-2.5"] do
        div_ [class_ "w-8 h-8 rounded-full bg-fillBrand-weak flex items-center justify-center shrink-0"]
          $ faSprite_ "bell" "regular" "w-4 h-4 text-iconBrand"
        div_ [] do
          h3_ [class_ "text-base font-semibold text-textStrong"] "Create Alert"
          p_ [class_ "text-xs text-textWeak hidden sm:block"] "Monitor logs and get notified on thresholds"
      button_
        [ type_ "button"
        , class_ "p-1 rounded-lg hover:bg-fillWeak transition-colors"
        , [__|on click set #create-alert-toggle.checked to false|]
        ]
        $ faSprite_ "xmark" "regular" "w-4 h-4 text-textWeak"

    form_
      [ id_ "alert-form"
      , hxPost_ $ "/p/" <> pid.toText <> "/alerts"
      , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, source: params().source || 'spans'}"
      , class_ "flex flex-col gap-3"
      ]
      do
        -- Alert name field (more compact)
        fieldset_ [class_ "fieldset"] do
          label_ [class_ "label text-xs font-medium text-textStrong mb-1"] "Alert name"
          input_
            [ type_ "text"
            , name_ "title"
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
                mkOpt (m, l) = option_ [value_ (show m <> "m")] ("every " <> l)

            -- Frequency
            fieldset_ [class_ "fieldset flex-1"] do
              label_ [class_ "label text-xs"] "Execute the query"
              select_ [name_ "frequency", class_ "select select-sm"] $ forM_ timeOpts mkOpt

            -- Time window
            fieldset_ [class_ "fieldset flex-1"] do
              label_ [class_ "label text-xs"] "Include rows from"
              select_ [name_ "timeWindow", class_ "select select-sm"] do
                forM_
                  (zip timeOpts [False, False, True, False, False, False, False, False, False, False])
                  \((m, l), sel) -> option_ ([value_ (show m <> "m")] ++ [selected_ "" | sel]) ("the last " <> l)

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
                  option_ [value_ "has_matches", selected_ ""] "the query has any results"
                  option_ [value_ "matches_changed"] "the query's results change"
                  option_ [value_ "threshold_exceeded"] "threshold is exceeded"

        -- Thresholds (collapsible, only visible when threshold_exceeded is selected)
        div_ [class_ "bg-bgBase rounded-xl border border-strokeWeak overflow-hidden hidden", id_ "thresholds"] do
          label_ [class_ "flex items-center justify-between p-3 cursor-pointer hover:bg-fillWeak transition-colors peer"] do
            div_ [class_ "flex items-center gap-2"] do
              faSprite_ "chart-line" "regular" "w-4 h-4 text-iconNeutral"
              span_ [class_ "text-sm font-medium text-textStrong"] "Thresholds"
            input_ [type_ "checkbox", class_ "hidden peer", checked_]
            faSprite_ "chevron-down" "regular" "w-3 h-3 text-iconNeutral peer-checked:rotate-180 transition-transform"

          div_ [class_ "p-3 pt-0 peer-has-[:checked]:block hidden"] do
            div_ [class_ "flex flex-row gap-3"] do
              let thresholdInput name color label req = fieldset_ [class_ "fieldset flex-1"] do
                    label_ [class_ "label flex items-center gap-1.5 text-xs mb-1"] do
                      div_ [class_ $ "w-1.5 h-1.5 rounded-full " <> color] ""
                      span_ [class_ "font-medium"] label
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
                      span_ [class_ "absolute right-2 top-1/2 -translate-y-1/2 text-xs text-textWeak"] "events"

              thresholdInput "alertThreshold" "bg-fillError-strong" "Alert threshold" True
              thresholdInput "warningThreshold" "bg-fillWarning-strong" "Warning threshold" False

              fieldset_ [class_ "fieldset flex-1"] do
                label_ [class_ "label text-xs font-medium mb-1"] "Trigger condition"
                select_ [name_ "direction", class_ "select select-sm"] do
                  option_ [value_ "above", selected_ ""] "Above threshold"
                  option_ [value_ "below"] "Below threshold"

            -- Info banner (more compact)
            div_ [class_ "flex items-start gap-2 p-2.5 bg-bgAlternate rounded-lg mt-3"] do
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
            div_ [class_ "flex items-center w-full gap-2 mb-3"] do
              fieldset_ [class_ "fieldset"] do
                label_ [class_ "label text-xs font-medium mb-1"] "Severity"
                select_ [class_ "select select-sm w-28", name_ "severity"] do
                  option_ [] "Info"
                  option_ [selected_ ""] "Error"
                  option_ [] "Warning"
                  option_ [] "Critical"

              fieldset_ [class_ "fieldset w-full"] do
                label_ [class_ "label text-xs font-medium mb-1"] "Subject"
                input_
                  [ placeholder_ "e.g. Alert triggered for high error rate"
                  , class_ "input input-sm w-full"
                  , name_ "subject"
                  , value_ "Alert triggered"
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
                "The alert threshold has been exceeded. Check the APItoolkit dashboard for details."

            -- Recipients checkbox
            div_ [class_ "flex items-center gap-2"] do
              label_ [class_ "label cursor-pointer flex items-center gap-2"] do
                input_ [type_ "checkbox", class_ "checkbox checkbox-xs", name_ "recipientEmailAll", value_ "true", checked_]
                span_ [class_ "text-xs"] "Send to all team members"

              span_ [class_ "tooltip", term "data-tip" "Configure specific recipients in alert settings after creation"]
                $ faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-iconNeutral"

        -- Action buttons (more compact)
        div_ [class_ "flex items-center justify-end gap-2 pt-1"] do
          button_
            [ type_ "button"
            , class_ "btn btn-outline btn-xs"
            , [__|on click set #create-alert-toggle.checked to false|]
            ]
            "Cancel"
          button_
            [ type_ "submit"
            , class_ "btn btn-primary btn-xs"
            ]
            do
              faSprite_ "plus" "regular" "w-3.5 h-3.5"
              "Create Alert"
