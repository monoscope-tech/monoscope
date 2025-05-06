module Pages.Log (
  apiLogH,
  apiLogJson,
  LogsGet (..),
  ApiLogsPageData (..),
  virtualTable,
  curateCols,
  logQueryBox_,
)
where

import Control.Error (hush)
import Data.Aeson qualified as AE
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Fields.Facets qualified as Facets
import Models.Apis.Fields.Types (FacetData (..), FacetSummary (..), FacetValue (..))
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), currProject, pageActions, pageTitle, sessM)
import Pkg.Components qualified as Components
import Pkg.Components.Widget (WidgetAxis (..), WidgetType (WTTimeseriesLine))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser (pSource, parseQueryToAST, toQText)
import Relude hiding (ask)
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


-- | Render facet data for Log Explorer sidebar in a compact format
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

      -- Define display info for different facet types
      facetDisplays :: [(Text, Text, (Text -> Text))]
      facetDisplays =
        [ ("level", "Log Level", levelColorFn)
        , ("kind", "Span Kind", const "")
        , ("name", "Operation name", const "")
        , ("attributes___http___response___status_code", "Status Code", statusColorFn)
        , ("attributes___http___request___method", "HTTP Method", methodColorFn)
        , ("resource___service___name", "Service", const "")
        , ("attributes___error___type", "Error Type", const "bg-red-500")
        ]

      levelColorFn val = case val of
        "ERROR" -> "bg-red-500"
        "WARN" -> "bg-yellow-500"
        "INFO" -> "bg-blue-500"
        "DEBUG" -> "bg-gray-500"
        _ -> "bg-gray-400"

  -- Add JS for filtering
  script_
    [text|
    function filterByFacet(field, value) {
      document.getElementById("filterElement").handleAddQuery({
        "tag": "Eq",
        "contents": [field, value]
      });
    }
  |]

  -- Render each facet group
  forM_ facetDisplays $ \(key, displayName, colorFn) -> do
    whenJust (HM.lookup key facetMap) $ \values -> do
      when (not $ null values) $ do
        div_ [class_ "facet-section flex flex-col gap-1.5 py-3 transition-all duration-200 hover:bg-fillWeaker rounded-lg group"] do
          div_
            [ class_ "flex justify-between items-center text-slate-950 pb-2 cursor-pointer"
            , [__|on click toggle .collapsed on me.parentElement|]
            ]
            $ span_ [class_ "facet-title"] (toHtml displayName)
              >> faSprite_ "chevron-down" "regular" "w-3 h-3 transition-transform duration-200 group-[.collapsed]:rotate-180"

          -- Wrap facet values in a collapsible container with animation
          div_ [class_ "facet-content overflow-hidden transition-all duration-300 ease-in-out max-h-96 opacity-100 transition-opacity duration-150 group-[.collapsed]:max-h-0 group-[.collapsed]:opacity-0 group-[.collapsed]:py-0"] do
            -- Render each facet value
            forM_ values \(FacetValue val count) -> do
              div_ [class_ "facet-item flex justify-between items-center py-1 hover:bg-fillWeak transition-colors duration-150 rounded-md px-1"] do
                label_ [class_ "flex gap-1.5 items-center text-slate-950 cursor-pointer flex-1"] $ do
                  input_
                    [ type_ "checkbox"
                    , class_ "checkbox checkbox-sm"
                    , -- Convert key format for filter (from db___ format to proper dot notation)
                      onclick_ $ "filterByFacet('" <> T.replace "___" "." key <> "', '" <> val <> "')"
                    ]
                  let colorClass = colorFn val
                  when (not $ T.null colorClass) $
                    span_ [class_ $ colorClass <> " shrink-0 w-1 h-5 rounded-sm"] " "
                  span_ [class_ "facet-value truncate max-w-[80%]", term "data-tippy-content" val] $ toHtml val
                span_ [class_ "facet-count text-slate-500 shrink-0 ml-1"] $ toHtml $ show count


keepNonEmpty :: Maybe Text -> Maybe Text
keepNonEmpty Nothing = Nothing
keepNonEmpty (Just "") = Nothing
keepNonEmpty (Just a) = Just a


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM queryASTM cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM queryLibItemTitle queryLibItemID detailWM targetEventM showTraceM hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let source = fromMaybe "spans" sourceM
  let summaryCols = T.splitOn "," (fromMaybe "" cols')
  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
  queryAST <-
    maybe
      (parseQuery $ maybeToMonoid queryM)
      (either (const $ parseQuery $ maybeToMonoid queryM) pure . AE.eitherDecode . encodeUtf8)
      queryASTM
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
  tableAsVecE <- RequestDumps.selectLogTable pid queryAST cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE

  (queryLibRecent, queryLibSaved) <- V.partition (\x -> Projects.QLTHistory == (x.queryType)) <$> Projects.queryLibHistoryForUser pid sess.persistentSession.userId

  facetSummary <- Facets.getFacetSummary pid "otel_logs_and_spans" (fromMaybe (addUTCTime (-86400) now) fromD) (fromMaybe now toD)

  freeTierExceeded <-
    dbtToEff $
      if project.paymentPlan == "Free"
        then (> 10000) <$> RequestDumps.getLastSevenDaysTotalRequest pid
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
  let (days, hours, minutes, _seconds) = convertToDHMS $ diffUTCTime now project.createdAt
      daysLeft =
        if days >= 0 && project.paymentPlan /= "Free"
          then Just $ show days <> " days, " <> show hours <> " hours, " <> show minutes <> " minutes"
          else Nothing
  case tableAsVecM of
    Just tableAsVec -> do
      let (requestVecs, colNames, resultCount) = tableAsVec
          curatedColNames = nubOrd $ curateCols summaryCols colNames
          colIdxMap = listToIndexHashMap colNames
          reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
          traceIDs = V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source queryASTM
          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing Nothing source Nothing
      additionalReqsVec <-
        if (null traceIDs)
          then pure []
          else do
            rs <- RequestDumps.selectChildSpansAndLogs pid summaryCols $ V.filter (/= "") traceIDs
            pure rs
      let finalVecs = requestVecs <> additionalReqsVec
          serviceNames = V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") finalVecs
          colors = getServiceColors (V.catMaybes serviceNames)
      let page =
            ApiLogsPageData
              { pid
              , resultCount
              , requestVecs = finalVecs
              , cols = curatedColNames
              , colIdxMap
              , nextLogsURL
              , resetLogsURL
              , currentRange
              , exceededFreeTier = freeTierExceeded
              , query = queryM
              , cursor = reqLastCreatedAtM
              , isTestLog = Nothing
              , emptyStateUrl = Nothing
              , source
              , targetSpans = targetSpansM
              , serviceColors = colors
              , daysCountDown = daysLeft
              , queryAST = decodeUtf8 $ AE.encode queryAST
              , queryLibRecent
              , queryLibSaved
              , fromD
              , toD
              , detailsWidth = detailWM
              , targetEvent = targetEventM
              , showTrace = showTraceM
              , facets = facetSummary
              }
      case (layoutM, hxRequestM, hxBoostedM) of
        (Just "SaveQuery", _, _) -> addRespHeaders $ LogsQueryLibrary pid queryLibSaved queryLibRecent
        (Just "resultTable", Just "true", _) -> addRespHeaders $ LogsGetResultTable page False
        (Just "virtualTable", _, _) -> addRespHeaders $ LogsGetVirtuaTable page
        (Just "all", Just "true", _) -> addRespHeaders $ LogsGetResultTable page True
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
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetResultTable page bol) = toHtml $ virtualTableTrigger page
  toHtml (LogsGetVirtuaTable page) = toHtml $ virtualTable page
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-red-500"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtmlRaw = toHtml


apiLogJson :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AE.Value)
apiLogJson pid queryM queryASTM cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let source = fromMaybe "spans" sourceM
  let summaryCols = T.splitOn "," (fromMaybe "" cols')
  let parseQuery q = either (\err -> addErrorToast "Error Parsing Query" (Just err) >> pure []) pure (parseQueryToAST q)
  queryAST <-
    maybe
      (parseQuery $ maybeToMonoid queryM)
      (either (const $ parseQuery $ maybeToMonoid queryM) pure . AE.eitherDecode . encodeUtf8)
      queryASTM

  now <- Time.currentTime
  let (fromD, toD, _) = Components.parseTimeRange now (Components.TimePicker sinceM fromM toM)
  tableAsVecE <- RequestDumps.selectLogTable pid queryAST cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE
  case tableAsVecM of
    Just tableAsVec -> do
      let (requestVecs, colNames, _) = tableAsVec
          colIdxMap = listToIndexHashMap colNames
          reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source queryASTM
          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing Nothing source Nothing
          traceIDs = V.catMaybes $ V.map (\v -> lookupVecTextByKey v colIdxMap "trace_id") requestVecs
      additionalReqsVec <-
        if (null traceIDs)
          then pure []
          else do
            rs <- RequestDumps.selectChildSpansAndLogs pid summaryCols $ V.filter (/= "") traceIDs
            pure rs
      let finalVecs = requestVecs <> additionalReqsVec
          serviceNames = V.map (\v -> lookupVecTextByKey v colIdxMap "span_name") finalVecs
          colors = getServiceColors (V.catMaybes serviceNames)

      addRespHeaders $
        AE.object
          [ "logsData" AE..= finalVecs
          , "serviceColors" AE..= colors
          , "nextUrl" AE..= nextLogsURL
          , "resetLogsUrl" AE..= resetLogsURL
          ]
    Nothing -> do
      addRespHeaders $ AE.object ["error" AE..= "Something went wrong"]


logQueryBox_ :: Projects.ProjectId -> Maybe Text -> Text -> Maybe Text -> Text -> V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
logQueryBox_ pid currentRange source targetSpan queryAST queryLibRecent queryLibSaved = do
  Components.modal_ "saveQueryMdl" "" $ form_
    [ class_ "flex flex-col p-3 gap-3"
    , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer?layout=SaveQuery"
    , hxVals_ "js:{queryAST:window.getQueryFromEditor()}"
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
    , hxPushUrl_ "true"
    , hxTrigger_ "add-query from:#filterElement, update-query from:#filterElement, submit, update-query from:window"
    , hxVals_ "js:{queryAST:window.getQueryFromEditor(), since: params().since, from: params().from, to:params().to, cols:params().cols, layout:'all', source: params().source}"
    , hxTarget_ "#resultTableInner"
    , hxSwap_ "outerHTML"
    , id_ "log_explorer_form"
    , hxIndicator_ "#run-query-indicator"
    , [__| on keydown if event.key is 'Enter' halt |]
    , class_ "flex flex-col gap-1"
    ]
    do
      div_ [class_ "flex gap-2 items-stretch justify-center"] do
        queryLibrary_ pid queryLibSaved queryLibRecent
        div_ [class_ "p-1 pl-3 flex-1 flex gap-2  bg-fillWeaker rounded-lg border border-strokeWeak justify-between items-stretch"] do
          div_ [id_ "queryEditor", class_ "h-14 hidden overflow-hidden  bg-fillWeak flex-1 flex items-center"] pass
          div_ [id_ "queryBuilder", class_ "flex-1 flex items-center"] $ termRaw "filter-element" [id_ "filterElement", class_ "w-full h-full flex items-center", termRaw "ast" queryAST] ("" :: Text)
          div_ [class_ "gap-[2px] flex items-center"] do
            span_ "in"
            select_
              [ class_ "ml-1 select select-sm w-full max-w-[150px]"
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
            div_ [class_ "rounded-lg px-3 py-2 text-slate-700 inline-flex items-center border border-strokeStrong", tabindex_ "0", role_ "button"] $ faSprite_ "floppy-disk" "regular" "h-5 w-5"
            ul_ [tabindex_ "0", class_ "dropdown-content border menu bg-base-100 rounded-box z-1 w-60 p-2 shadow-lg"] do
              li_ $ label_ [Lucid.for_ "saveQueryMdl"] "Save query to Query Library"
          -- li_ $ a_ [] "Save query as an Alerts"
          -- li_ $ a_ [] "Save result to a dashboard"
          button_
            [type_ "submit", class_ "leading-none rounded-lg px-3 py-2 cursor-pointer btn btn-primary"]
            do
              span_ [id_ "run-query-indicator", class_ "refresh-indicator htmx-indicator query-indicator loading loading-dots loading-sm"] ""
              faSprite_ "magnifying-glass" "regular" "h-4 w-4 inline-block"
      div_ [class_ "flex items-between justify-between"] do
        div_ [class_ "", id_ "resultTableInner"] pass

        -- termRaw "filter-element" [id_ "filterElement", class_ "w-full h-full flex items-center", termRaw "ast" queryAST, termRaw "mode" "command"] ("" :: Text)
        div_ [class_ "flex justify-end  gap-2 "] do
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded-sm toggle-chart"] >> span_ "charts"
          fieldset_ [class_ "fieldset"] $ label_ [class_ "label"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded-sm", id_ "toggleQueryEditor", onclick_ "toggleQueryBuilder()"] >> span_ "query editor"


queryLibrary_ :: Projects.ProjectId -> V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
queryLibrary_ pid queryLibSaved queryLibRecent = div_ [class_ "dropdown dropdown-hover dropdown-bottom dropdown-start", id_ "queryLibraryParentEl"] do
  div_ [class_ "cursor-pointer relative  bg-fillWeak  text-textWeak rounded-lg border border-strokeWeaker h-full flex gap-2 items-center px-2", tabindex_ "0", role_ "button"] $
    (toHtml "Presets" >> faSprite_ "chevron-down" "regular" "w-3 h-3")
  div_ [class_ "dropdown-content z-20"] $ div_ [class_ "tabs tabs-box tabs-md tabs-outline items-center bg-fillWeak p-0 h-full", role_ "tablist", id_ "queryLibraryTabListEl"] do
    tabPanel_ "Saved" (queryLibraryContent_ "Saved" queryLibSaved)
    tabPanel_ "Recent" (queryLibraryContent_ "Recent" queryLibRecent)
  where
    tabPanel_ :: Text -> Html () -> Html ()
    tabPanel_ label content = do
      input_ $ [type_ "radio", name_ "querylib", role_ "tab", class_ "tab", Aria.label_ label] <> [checked_ | label == "Saved"]
      div_ [role_ "tabpanel", class_ "tab-content bg-base-100 shadow-lg rounded-box h-[70vh] w-[40vw] space-y-2 overflow-y-scroll"] content

    queryLibraryContent_ :: Text -> V.Vector Projects.QueryLibItem -> Html ()
    queryLibraryContent_ label items = do
      searchBar_ label
      div_ [class_ $ "border divide-y rounded-xl p-3 dataLibContent" <> label] $ V.forM_ items queryLibItem_

    searchBar_ :: Text -> Html ()
    searchBar_ label = div_ [class_ "flex gap-2 sticky top-0 p-3 bg-base-100 z-20"] do
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
          , term "data-queryAST" $ decodeUtf8 $ AE.encode qli.queryAst
          , [__| on click call #filterElement.handleAddQuery({detail: JSON.parse(@data-queryAST)})|]
          ]
          $ faSprite_ "play" "regular" "h-4 w-4"
        a_ [class_ "tooltip", term "data-tip" "copy query to clipboard", [__|install Copy(content: (next <.queryText/> ))|]] $ faSprite_ "copy" "regular" "h-4 w-4"
        when qli.byMe $ a_ [class_ "tooltip", term "data-tip" "edit query title", [__|on click set #queryLibId.value to @data-queryId then set #saveQueryMdl.checked to true|], term "data-queryId" qli.id.toText] $ faSprite_ "pen-to-square" "regular" "h-4 w-4"
        when qli.byMe
          $ a_
            [ class_ "tooltip"
            , term "data-tip" "delete query"
            , hxGet_ $ "/p/" <> qli.projectId.toText <> "/log_explorer?layout=DeleteQuery&queryLibId=" <> qli.id.toText
            , hxVals_ "js:{queryAST:window.getQueryFromEditor()}"
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
    pre_ $
      code_ [class_ "language-js bg-transparent! queryText whitespace-pre-wrap break-words"] $
        toHtml qli.queryText
    div_ [class_ "gap-3 flex"] $ time_ [datetime_ "", term "data-tippy-content" "created on"] (toHtml $ displayTimestamp $ formatUTC qli.createdAt) >> when qli.byMe " by me"


data ApiLogsPageData = ApiLogsPageData
  { pid :: Projects.ProjectId
  , resultCount :: Int
  , requestVecs :: V.Vector (V.Vector AE.Value)
  , cols :: [Text]
  , colIdxMap :: HM.HashMap Text Int
  , nextLogsURL :: Text
  , resetLogsURL :: Text
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
  , queryAST :: Text
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

    script_
      [text|
        if(window.logListTable) {
           window.logListTable.updateTableData($vecs, $cols, $colIdxMap, $serviceColors, $nextLogsURL)
          }
    |]


virtualTable :: ApiLogsPageData -> Html ()
virtualTable page = do
  termRaw
    "log-list"
    [ id_ "resultTable"
    , class_ "w-full divide-y shrink-1 flex flex-col h-full min-w-0  overflow-x-hidden"
    ]
    ("" :: Text)
  let logs = decodeUtf8 $ AE.encode page.requestVecs
      cols = decodeUtf8 $ AE.encode page.cols
      colIdxMap = decodeUtf8 $ AE.encode page.colIdxMap
      serviceColors = decodeUtf8 $ AE.encode page.serviceColors
      nextfetchurl = page.nextLogsURL
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
       resetLogsUrl: `$resetLogsURL`,
       projectId: "$projectid"
      }
   |]


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-hidden pb-2 group/pg", id_ "apiLogsPage"] do
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
          div_ [class_ "flex justify-end  w-full p-4 "] $
            button_ [[__|on click add .hidden to #expand-log-modal|]] $
              faSprite_ "xmark" "regular" "h-8"
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
      logQueryBox_ page.pid page.currentRange page.source page.targetSpans page.queryAST page.queryLibRecent page.queryLibSaved

      div_ [class_ "flex flex-row gap-4 mt-3 group-has-[.toggle-chart:checked]/pg:hidden w-full", style_ "aspect-ratio: 10 / 1;"] do
        Widget.widget_ $ (def :: Widget.Widget){Widget.query = Just "timechart count(*)", Widget.unit = Just "rows", Widget.title = Just "All traces", Widget.hideLegend = Just True, Widget._projectId = Just page.pid, Widget.standalone = Just True, Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})}

        Widget.widget_ $
          (def :: Widget.Widget)
            { Widget.wType = WTTimeseriesLine
            , Widget.standalone = Just True
            , Widget.title = Just "Latency percentiles (ms)"
            , Widget.hideSubtitle = Just True
            , Widget.yAxis = Just (def{showOnlyMaxLabel = Just True})
            , Widget.summarizeBy = Just Widget.SBMax
            , Widget.sql =
                Just
                  [text|
                        SELECT timeB, value, quantile
                              FROM ( SELECT extract(epoch from time_bucket('1h', timestamp))::integer AS timeB,
                                      ARRAY[
                                        (approx_percentile(0.50, percentile_agg(duration)) / 1000000.0)::float,
                                        (approx_percentile(0.75, percentile_agg(duration)) / 1000000.0)::float,
                                        (approx_percentile(0.90, percentile_agg(duration)) / 1000000.0)::float,
                                        (approx_percentile(0.95, percentile_agg(duration)) / 1000000.0)::float
                                      ] AS values,
                                      ARRAY['p50', 'p75', 'p90', 'p95'] AS quantiles
                                FROM otel_logs_and_spans
                                WHERE project_id='{{project_id}}'
                                  {{time_filter}} {{query_ast_filters}}
                                GROUP BY timeB
                              ) s,
                            LATERAL unnest(s.values, s.quantiles) AS u(value, quantile);
                        |]
            , Widget.unit = Just "ms"
            , Widget.hideLegend = Just True
            , Widget._projectId = Just page.pid
            }

    div_ [class_ "flex h-full gap-3.5 overflow-hidden"] do
      -- FACETS
      div_ [class_ "w-1/6 text-sm shrink-0 flex flex-col gap-2 p-2 transition-all duration-500 ease-out opacity-100 delay-[0ms] group-has-[.toggle-filters:checked]/pg:duration-300 group-has-[.toggle-filters:checked]/pg:opacity-0 group-has-[.toggle-filters:checked]/pg:w-0 group-has-[.toggle-filters:checked]/pg:p-0 group-has-[.toggle-filters:checked]/pg:overflow-hidden"] do
        input_
          [ placeholder_ "Search facets..."
          , class_ "rounded-lg px-3 py-1 border border-strokeStrong"
          , term "data-filterParent" "facets-container"
          , [__| on keyup 
                if the event's key is 'Escape' 
                  set my value to '' 
                  trigger keyup 
                else 
                  show <div.facet-section/> in #{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()
                  show <div.facet-item/> in #{@data-filterParent} when its textContent.toLowerCase() contains my value.toLowerCase()
              |]
          ]
        div_ [class_ "divide-y gap-3 overflow-y-scroll h-full", id_ "facets-container"] do
          whenJust page.facets renderFacets

      div_ [class_ "grow flex-1 h-full space-y-1.5 overflow-hidden"] do
        div_ [class_ "flex w-full relative h-full", id_ "logs_section_container"] do
          let dW = fromMaybe "100%" page.detailsWidth
              showTrace = isJust page.showTrace
          div_ [class_ "relative flex flex-col shrink-1 min-w-0 w-full h-full", style_ $ "width: " <> dW, id_ "logs_list_container"] do
            div_ [class_ "flex gap-2  pt-1 text-sm -mb-6 z-10 w-max"] do
              label_ [class_ "gap-1 flex items-center cursor-pointer"] do
                faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 "
                span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
                span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
                "filters"
                input_ [type_ "checkbox", class_ "toggle-filters hidden", checked_]
              span_ [class_ "text-slate-200"] "|"
              div_ [class_ ""] $ span_ [class_ "text-slate-950"] (toHtml @Text $ fmt $ commaizeF page.resultCount) >> span_ [class_ "text-slate-600"] (toHtml (" rows found"))
            div_ [class_ $ "absolute top-0 right-0  w-full h-full overflow-scroll c-scroll z-50 bg-white transition-all duration-100 " <> if showTrace then "" else "hidden", id_ "trace_expanded_view"] do
              whenJust page.showTrace \trId -> do
                let url = "/p/" <> page.pid.toText <> "/traces/" <> trId
                span_ [class_ "loading loading-dots loading-md"] ""
                div_ [hxGet_ url, hxTarget_ "#trace_expanded_view", hxSwap_ "innerHtml", hxTrigger_ "intersect one"] pass
            virtualTable page

          div_ [onmousedown_ "mouseDown(event)", class_ "relative shrink-0 h-full flex items-center justify-center border-l  hover:border-strokeBrand-strong cursor-ew-resize overflow-visible"] do
            div_
              [ onmousedown_ "mouseDown(event)"
              , class_ "absolute inset-y-0 left-0 w-4 -ml-3 cursor-ew-resize h-full z-10"] ""
            div_
              [ onmousedown_ "mouseDown(event)"
              , id_ "resizer"
              , class_ $ "absolute left-1/2 top-1/2 z-50 -translate-x-1/2 leading-none py-1 -translate-y-1/2 bg-slate-50 rounded-sm border border-strokeBrand-weak text-iconNeutral " <> if isJust page.detailsWidth then "" else "hidden"
              ] $ faSprite_ "grip-dots-vertical" "regular" "w-5 h-5"

          div_ [class_ "overflow-y-auto grow-1 overflow-x-hidden h-full c-scroll transition-all duration-100", style_ "width:0px", id_ "log_details_container"] do
            span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
            whenJust page.targetEvent \te -> do
              let url = "/p/" <> page.pid.toText <> "/log_explorer/" <> te
              div_ [hxGet_ url, hxTarget_ "#log_details_container", hxSwap_ "innerHtml", hxTrigger_ "intersect one", hxIndicator_ "#details_indicator"] pass

          script_
            [text|
          function updateUrlState(key, value, action='set') {
            const params = new URLSearchParams(window.location.search)
            if(action === 'delete') {
              params.delete(key)
            }else {
             params.set(key, value)
            }
            window.history.replaceState({}, '', `$${window.location.pathname}?$${params}`)
          }
          var logsList = null
          const logDetails = document.querySelector('#log_details_container')
          const container = document.querySelector('#logs_section_container')
          const logsListC = document.querySelector('#logs_list_container')          
          const containerWidth = Number(window.getComputedStyle(container).width.replace('px',''))

          let mouseState = {x: 0}
          let resizeStart = false
          let target = ""


          function mouseDown(event) {
              resizeStart = true
              container.style.userSelect = "none";
              mouseState = {x: event.clientX  }
              target = event.target.id
          }

          function handleMouseup(event) {
            resizeStart = false
            container.style.userSelect = "auto";
            target = ""
          }

          function handleMouseMove(event) {
            if(!resizeStart) return
            if(!logsList) {
            
            }
            const diff = event.clientX  - mouseState.x
            mouseState = {x: event.clientX}
            const edW = Number(logDetails.style.width.replace('px',''))
            let ldW = Number(logsListC.style.width.replace('px',''))
            if(isNaN(ldW)) {
                ldW = Number(window.getComputedStyle(logsListC).width.replace('px',''))
            }
            //logDetails.style.width = (edW - diff) + 'px'
            logsListC.style.width = (ldW + diff) + 'px'
            updateUrlState('details_width', logsListC.style.width)
          }
          window.addEventListener ('mousemove', handleMouseMove)
          window.addEventListener ('mouseup', handleMouseup)
          |]

  jsonTreeAuxillaryCode page.pid page.queryAST


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
      | a == "rest" = GT
      | b == "rest" = LT
      | otherwise = comparing (`L.elemIndex` filteredCols) a b


-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Text -> Html ()
jsonTreeAuxillaryCode pid queryAST = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu  origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-bgBase ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-hidden z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1", role_ "none"] do
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{queryAST:params().queryAST,cols:toggleColumnToSummary(event),layout:'resultTable', since: params().since, from: params().from, to:params().to, source: params().source}"
          , hxTarget_ "#resultTableInner"
          , hxSwap_ "outerHTML"
          , -- , hxIndicator_ "#query-indicator"
            [__|init set fp to (closest @data-field-path) then
                  if params().cols.split(",").includes(fp) then set my innerHTML to 'Remove field from summary' end|]
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

        document.getElementById("filterElement").handleAddQuery({
            "tag": operation,
            "contents": [path, JSON.parse(value)]
        });
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

    // TODO: Delete
    function toggleQueryBuilder() {
        ["queryBuilder", "queryEditor"].forEach(id => document.getElementById(id).classList.toggle("hidden"));
        window.editor ??= CodeMirror(document.getElementById('queryEditor'), {
            value: params().queryAST || window.queryBuilderValue || '',
            mode: "javascript",
            theme: "elegant",
            lineNumbers: true,
        });
        setTimeout(() => {
            const editorVisible = !document.getElementById("queryEditor").classList.contains("hidden");
            editorVisible
                ? window.editor.setValue(window.queryBuilderValue)
                : document.querySelector('#filterElement')?.setBuilderValue(window.editor.getValue());
        }, 10);
    }
|]
