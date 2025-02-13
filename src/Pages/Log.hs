module Pages.Log (
  apiLogH,
  apiLogJson,
  LogsGet (..),
  ApiLogsPageData (..),
  resultTable_,
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
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), currProject, pageActions, pageTitle, sessM)
import Pages.Components (emptyState_)
import Pages.Components qualified as Components
import Pages.Telemetry.Spans qualified as Spans
import Pkg.Components qualified as Components
import Pkg.Parser (pSource, parseQueryToAST, toQText)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Types
import Text.Megaparsec (parseMaybe)
import Utils


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


keepNonEmpty :: Maybe Text -> Maybe Text
keepNonEmpty Nothing = Nothing
keepNonEmpty (Just "") = Nothing
keepNonEmpty (Just a) = Just a


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM queryASTM cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM queryLibItemTitle queryLibItemID hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let source = fromMaybe "requests" sourceM
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

  freeTierExceeded <-
    dbtToEff
      $ if project.paymentPlan == "Free"
        then (> 5000) <$> RequestDumps.getLastSevenDaysTotalRequest pid
        else pure False

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Explorer"
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/openapi-docs/"
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              label_ [class_ "cursor-pointer border border-strokeStrong rounded-lg flex shadow"] do
                input_ [type_ "checkbox", id_ "streamLiveData", class_ "hidden"]
                span_ [class_ "group-has-[#streamLiveData:checked]/pg:flex hidden py-1 px-3 items-center", term "data-tippy-content" "pause live data stream"] $ faSprite_ "pause" "solid" "h-4 w-4"
                span_ [class_ "group-has-[#streamLiveData:checked]/pg:hidden flex  py-1 px-3 items-center", term "data-tippy-content" "stream live data"] $ faSprite_ "play" "regular" "h-4 w-4"
              Components.timepicker_ (Just "log_explorer_form") currentRange
              a_ [class_ "cursor-pointer py-1 px-3 border border-strokeStrong rounded-lg shadow", [__|on click htmx.trigger('#log_explorer_form', 'submit') |], term "data-tippy-content" "refresh"] $ faSprite_ "arrows-rotate" "regular" "h-4 w-4"
          , navTabs = Just $ div_ [class_ "tabs tabs-boxed tabs-md p-0 tabs-outline items-center  bg-fillWeak  text-textWeak border"] do
              a_ [onclick_ "window.setQueryParamAndReload('source', 'requests')", role_ "tab", class_ $ "tab py-1 !h-auto " <> if source == "requests" then "tab-active  text-textStrong border border-strokeStrong " else ""] "Requests"
              a_ [onclick_ "window.setQueryParamAndReload('source', 'logs')", role_ "tab", class_ $ "tab py-1 !h-auto " <> if source == "logs" then "tab-active  text-textStrong border border-strokeStrong " else ""] "Logs"
              a_ [onclick_ "window.setQueryParamAndReload('source', 'spans')", role_ "tab", class_ $ "tab py-1 !h-auto " <> if source == "spans" then "tab-active  text-textStrong border border-strokeStrong " else ""] "Traces"
              -- a_ [onclick_ "window.setQueryParamAndReload('source', 'metrics')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if source == "metrics" then "tab-active" else ""] "Metrics"
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
          reqLastCreatedAtM =
            if source == "requests"
              then (\r -> lookupVecTextByKey r colIdxMap "created_at") =<< (requestVecs V.!? (V.length requestVecs - 1))
              else (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
          childSpanIds =
            if source == "spans"
              then V.map (\v -> lookupVecTextByKey v colIdxMap "latency_breakdown") requestVecs
              else []
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source queryASTM
          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing Nothing source Nothing
      childSpans <- Telemetry.getChildSpans pid (V.catMaybes childSpanIds)
      let page =
            ApiLogsPageData
              { pid
              , resultCount
              , requestVecs
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
              , childSpans = childSpans
              , daysCountDown = daysLeft
              , queryAST = decodeUtf8 $ AE.encode queryAST
              , queryLibRecent
              , queryLibSaved
              }
      case (layoutM, hxRequestM, hxBoostedM) of
        (Just "SaveQuery", _, _) -> addRespHeaders $ LogsQueryLibrary pid queryLibSaved queryLibRecent
        (Just "loadmore", Just "true", _) -> addRespHeaders $ LogsGetRows pid requestVecs curatedColNames colIdxMap nextLogsURL source childSpans
        (Just "resultTable", Just "true", _) -> addRespHeaders $ LogsGetResultTable page False
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
  | -- TODO: Make the field below a named record
    LogsGetRows Projects.ProjectId (V.Vector (V.Vector AE.Value)) [Text] (HM.HashMap Text Int) Text Text (V.Vector Telemetry.SpanRecord)
  | LogsGetResultTable ApiLogsPageData Bool
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text
  | LogsQueryLibrary Projects.ProjectId (V.Vector Projects.QueryLibItem) (V.Vector Projects.QueryLibItem)


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetRows pid requestVecs cols colIdxMap nextLogsURL source chSpns) = toHtml $ logItemRows_ pid requestVecs cols colIdxMap nextLogsURL source chSpns
  toHtml (LogsGetResultTable page bol) = toHtml $ virtualTable page
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-red-500"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtml (LogsQueryLibrary pid queryLibSaved queryLibRecent) = toHtml $ queryLibrary_ pid queryLibSaved queryLibRecent
  toHtmlRaw = toHtml


apiLogJson :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders AE.Value)
apiLogJson pid queryM queryASTM cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let source = fromMaybe "requests" sourceM
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
          reqLastCreatedAtM =
            if source == "requests"
              then (\r -> lookupVecTextByKey r colIdxMap "created_at") =<< (requestVecs V.!? (V.length requestVecs - 1))
              else (\r -> lookupVecTextByKey r colIdxMap "timestamp") =<< (requestVecs V.!? (V.length requestVecs - 1))
          childSpanIds =
            if source == "spans"
              then V.map (\v -> lookupVecTextByKey v colIdxMap "latency_breakdown") requestVecs
              else []
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source queryASTM
          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing Nothing source Nothing
      childSpans <- Telemetry.getChildSpans pid (V.catMaybes childSpanIds)
      let colors = getServiceColors $ (.spanName) <$> childSpans
          childSps =
            ( map
                ( \sp ->
                    let name = AE.String sp.spanName
                        parentId = AE.String $ fromMaybe "" sp.parentSpanId
                        duration = AE.Number $ fromIntegral sp.spanDurationNs
                        color = AE.String $ fromMaybe "bg-black" $ HM.lookup sp.spanName colors
                        spandId = AE.String $ sp.spanId
                     in AE.toJSON ([name, parentId, duration, color, spandId] :: [AE.Value])
                )
                $ V.toList childSpans
            )

      addRespHeaders $ AE.object ["logsData" AE..= requestVecs, "childSpans" AE..= childSps, "nextUrl" AE..= nextLogsURL, "resetLogsUrl" AE..= resetLogsURL]
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
      input_ [class_ "input input-bordered input-md", placeholder_ "query title", name_ "queryTitle"]
      button_ [type_ "submit", class_ "btn cursor-pointer bg-gradient-to-b from-[#067cff] to-[#0850c5] text-white"] "Save"
  form_
    [ hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
    , hxPushUrl_ "true"
    , hxTrigger_ "add-query from:#filterElement, update-query from:#filterElement"
    , hxVals_ "js:{queryAST:window.getQueryFromEditor(), since: params().since, from: params().from, to:params().to, cols:params().cols, layout:'all', source: params().source}"
    , hxTarget_ "#resultTable"
    , hxSwap_ "outerHTML"
    , id_ "log_explorer_form"
    , hxIndicator_ "#run-query-indicator"
    , [__| on keydown if event.key is 'Enter' halt |]
    ]
    do
      div_ [class_ "flex gap-2 items-stretch justify-center"] do
        queryLibrary_ pid queryLibSaved queryLibRecent
        div_ [class_ "p-1 pl-3 flex-1 flex gap-2  bg-fillWeaker rounded-lg border border-strokeWeak justify-between items-stretch"] do
          div_ [id_ "queryEditor", class_ "h-14 hidden overflow-hidden  bg-fillWeak flex-1 flex items-center"] pass
          div_ [id_ "queryBuilder", class_ "flex-1 flex items-center"] $ termRaw "filter-element" [id_ "filterElement", class_ "w-full h-full flex items-center", termRaw "ast" queryAST] ("" :: Text)
          when (source == "spans") do
            let target = fromMaybe "all-spans" targetSpan
            div_ [class_ "gap-[2px] flex items-center"] do
              span_ "in"
              select_
                [ class_ "ml-1 select select-sm select-bordered w-full max-w-[150px]"
                , name_ "target-spans"
                , id_ "spans-toggle"
                , onchange_ "htmx.trigger('#log_explorer_form', 'submit')"
                ]
                do
                  option_ (value_ "all-spans" : ([selected_ "true" | target == "all-spans"])) "All spans"
                  option_ (value_ "root-spans" : ([selected_ "true" | target == "root-spans"])) "Trace Root Spans"
                  option_ (value_ "service-entry-spans" : ([selected_ "true" | target == "service-entry-spans"])) "Service Entry Spans"
          div_ [class_ "dropdown dropdown-hover dropdown-bottom dropdown-end"] do
            div_ [class_ "rounded-lg px-3 py-2 text-slate-700 inline-flex items-center border border-strokeStrong", tabindex_ "0", role_ "button"] $ faSprite_ "floppy-disk" "regular" "h-5 w-5"
            ul_ [tabindex_ "0", class_ "dropdown-content border menu bg-base-100 rounded-box z-[1] w-60 p-2 shadow-lg"] do
              li_ $ label_ [Lucid.for_ "saveQueryMdl"] "Save query to Query Library"
          -- li_ $ a_ [] "Save query as an Alerts"
          -- li_ $ a_ [] "Save result to a dashboard"
          button_
            [type_ "submit", class_ "leading-none rounded-lg px-3 py-2 cursor-pointer btn-primary"]
            do
              span_ [id_ "run-query-indicator", class_ "refresh-indicator htmx-indicator query-indicator loading loading-dots loading-sm"] ""
              faSprite_ "magnifying-glass" "regular" "h-4 w-4 inline-block"
      div_ [class_ "flex items-between justify-between"] do
        -- termRaw "filter-element" [id_ "filterElement", class_ "w-full h-full flex items-center", termRaw "ast" queryAST, termRaw "mode" "command"] ("" :: Text)
        div_ [class_ "flex justify-end  gap-2 "] do
          div_ [class_ "py-1 flex flex-row justify-end"] $ label_ [class_ "flex items-center cursor-pointer space-x-2 p-1"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded toggle-chart"] >> span_ "charts"
          div_ [class_ "form-control w-max"] $ label_ [class_ "label flex items-center cursor-pointer w-max space-x-2"] do
            input_ [type_ "checkbox", class_ "checkbox checkbox-sm rounded", id_ "toggleQueryEditor", onclick_ "toggleQueryBuilder()"] >> span_ "query editor"


queryLibrary_ :: Projects.ProjectId -> V.Vector Projects.QueryLibItem -> V.Vector Projects.QueryLibItem -> Html ()
queryLibrary_ pid queryLibSaved queryLibRecent = div_ [class_ "dropdown dropdown-hover dropdown-bottom dropdown-start", id_ "queryLibraryParentEl"] do
  div_ [class_ "cursor-pointer relative  bg-fillWeak  text-textWeak rounded-lg border border-strokeWeaker h-full flex gap-2 items-center px-2", tabindex_ "0", role_ "button"]
    $ (toHtml "Presets" >> faSprite_ "chevron-down" "regular" "w-3 h-3")
  div_ [class_ "dropdown-content z-20"] $ div_ [class_ "tabs tabs-boxed tabs-md tabs-outline items-center bg-fillWeak p-0 h-full", role_ "tablist", id_ "queryLibraryTabListEl"] do
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
      label_ [class_ "input input-md input-bordered flex items-center gap-2 flex-1"] do
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
        label_ [class_ "tabs tabs-md tabs-boxed tabs-outline bg-slate-200 text-slate-50 shrink items-center", role_ "tablist"] do
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
    pre_
      $ code_ [class_ "language-js !bg-transparent queryText whitespace-pre-wrap break-words"]
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
  , currentRange :: Maybe Text
  , exceededFreeTier :: Bool
  , query :: Maybe Text
  , cursor :: Maybe Text
  , isTestLog :: Maybe Bool
  , emptyStateUrl :: Maybe Text
  , source :: Text
  , targetSpans :: Maybe Text
  , childSpans :: V.Vector Telemetry.SpanRecord
  , daysCountDown :: Maybe Text
  , queryAST :: Text
  , queryLibRecent :: V.Vector Projects.QueryLibItem
  , queryLibSaved :: V.Vector Projects.QueryLibItem
  }


virtualTable :: ApiLogsPageData -> Html ()
virtualTable page = do
  let colors = getServiceColors $ (.spanName) <$> page.childSpans
      childSps =
        ( map
            ( \sp ->
                let name = AE.String sp.spanName
                    parentId = AE.String $ fromMaybe "" sp.parentSpanId
                    duration = AE.Number $ fromIntegral sp.spanDurationNs
                    color = AE.String $ fromMaybe "bg-black" $ HM.lookup sp.spanName colors
                 in AE.toJSON ([name, parentId, duration, color] :: [AE.Value])
            )
            $ V.toList page.childSpans
        )
  termRaw
    "log-list"
    [ id_ "resultTable"
    , class_ "w-full divide-y flex flex-col h-full overflow-y-hidden overflow-x-auto"
    , term "data-results" (decodeUtf8 $ AE.encode page.requestVecs)
    , term "data-columns" (decodeUtf8 $ AE.encode page.cols)
    , term "data-colIdxMap" (decodeUtf8 $ AE.encode page.colIdxMap)
    , term "data-childSpans" (decodeUtf8 $ AE.encode childSps)
    , term "data-nextfetchurl" page.nextLogsURL
    , term "data-projectid" page.pid.toText
    ]
    ("" :: Text)


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  script_ [type_ "module", src_ "/public/assets/explorer-list.js"] ("" :: Text)
  section_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-hidden pb-12  group/pg", id_ "apiLogsPage"] do
    div_
      [ style_ "z-index:26"
      , class_ "fixed hidden right-0 top-0 justify-end left-0 bottom-0 w-full bg-black bg-opacity-5"
      , [__|on click remove .show-log-modal from #expand-log-modal|]
      , id_ "expand-log-modal"
      ]
      do
        div_ [class_ "relative ml-auto w-full", style_ ""] do
          div_ [class_ "flex justify-end  w-full p-4 "]
            $ button_ [[__|on click add .hidden to #expand-log-modal|]]
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
      logQueryBox_ page.pid page.currentRange page.source page.targetSpans page.queryAST page.queryLibRecent page.queryLibSaved

      div_ [class_ "flex flex-row gap-4 mt-3 group-has-[.toggle-chart:checked]/pg:hidden"] do
        renderChart page.pid "reqsChartsECP" "All requests" (Just $ fmt (commaizeF page.resultCount)) Nothing page.source ""
        unless (page.source == "logs") $ renderChart page.pid "reqsChartsLatP" "Latency" Nothing Nothing page.source ", chart_type:'LineCT', group_by:'GBDurationPercentile'"

    div_ [class_ "flex gap-3.5 overflow-hidden"] do
      div_ [class_ "w-1/5 shrink-0 flex flex-col gap-2 p-2 hidden  group-has-[.toggle-filters:checked]/pg:hidden "] do
        input_ [placeholder_ "Search filter", class_ "rounded-lg shadow px-3 py-1 border border-strokeStrong"]
        div_ [class_ "divide-y gap-3"] do
          div_ [class_ "flex flex-col gap-1.5 py-3"] do
            div_ [class_ "flex justify-between items-center text-slate-950 pb-2"] $ span_ "Status" >> faSprite_ "chevron-down" "regular" "w-3 h-3"
            div_ [class_ "flex justify-between items-center"] do
              div_ [class_ "flex gap-1.5 items-center text-slate-950"] $ input_ [type_ "checkbox", class_ "checkbox "] >> span_ [class_ "bg-green-500 shrink-0 w-1 h-5 rounded"] " " >> span_ [] "200"
              span_ "19,833"
            div_ [class_ "flex justify-between items-center"] do
              div_ [class_ "flex gap-1.5 items-center  text-slate-950"] $ input_ [type_ "checkbox", class_ "checkbox "] >> span_ [class_ "bg-red-600 shrink-0 w-1 h-5 rounded"] " " >> span_ [] "200"
              span_ "121"
          div_ [class_ "flex flex-col gap-1.5 py-3"] do
            div_ [class_ "flex justify-between items-center text-slate-950 pb-2"] $ span_ "Methods" >> faSprite_ "chevron-down" "regular" "w-3 h-3"
            div_ [class_ "flex justify-between"] do
              div_ [class_ "flex gap-1.5 items-center  text-slate-950"] $ input_ [type_ "checkbox", class_ "checkbox "] >> span_ [class_ "bg-[#067cff] shrink-0 w-1 h-5 rounded"] " " >> span_ [] "GET"
              span_ "8,675"
            div_ [class_ "flex justify-between"] do
              div_ [class_ "flex gap-1.5 items-center  text-slate-950"] $ input_ [type_ "checkbox", class_ "checkbox "] >> span_ [class_ "text-green-500 shrink-0 w-1 h-5 rounded"] " " >> span_ [] "POST"
              span_ "4,459"

      div_ [class_ "grow flex-1 space-y-1.5 overflow-hidden"] do
        -- div_ [class_ "flex gap-2  pt-1"] do
        --   label_ [class_ "gap-1 flex items-center cursor-pointer"] do
        --     faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 "
        --     span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
        --     span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
        --     "filters"
        --     input_ [type_ "checkbox", class_ "toggle-filters hidden", checked_]
        --   span_ [class_ "text-slate-200"] "|"
        -- div_ [class_ "divide-y flex flex-col  overflow-hidden"] $ resultTableAndMeta_ page
        div_ [class_ "flex items-start h-full", id_ "logs_section_container"] do
          div_ [class_ "relative flex items-start w-full h-full", id_ "logs_list_container"] do
            div_ [class_ "absolute top-0 right-0 hidden w-full h-full overflow-scroll c-scroll z-50 bg-white transition-all duration-100", id_ "trace_expanded_view"] pass
            virtualTable page

          div_ [onmousedown_ "mouseDown(event)", class_ "relative shrink-0 h-full flex items-center justify-center w-1 bg-fillWeak  cursor-ew-resize overflow-visible"] do
            div_ [onmousedown_ "mouseDown(event)", id_ "resizer", class_ "absolute left-1/2 top-1/2 z-[999] -translate-x-1/2  px-1 py-1 -translate-y-1/2 w-max bg-slate-50 rounded border border-strokeBrand-weak grid grid-cols-2 gap-1"] do
              div_ [class_ "bg-iconNeutral h-[3px] w-[3px] rounded-full"] ""
              div_ [class_ "bg-iconNeutral h-[3px] w-[3px] rounded-full"] ""
              div_ [class_ "bg-iconNeutral h-[3px] w-[3px] rounded-full"] ""
              div_ [class_ "bg-iconNeutral h-[3px] w-[3px] rounded-full"] ""
              div_ [class_ "bg-iconNeutral h-[3px] w-[3px] rounded-full"] ""
              div_ [class_ "bg-iconNeutral h-[3px] w-[3px] rounded-full"] ""

          div_ [class_ "relative shrink-0 flex flex-col overflow-y-auto overflow-x-hidden h-full c-scroll transition-all duration-100", style_ "width:0px", id_ "log_details_container"] do
            span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""

          script_
            [text|
          function updateUrlState(key, value) {
            const params = new URLSearchParams(window.location.search)
            params.set(key, value)
            window.history.replaceState({}, '', `$${window.location.pathname}?$${params}`)
          }
          var logsList = null
          const logDetails = document.querySelector('#log_details_container')
          const container = document.querySelector('#logs_section_container')
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
             logsList = document.querySelector('#logs_list_container')
            }
            const diff = event.clientX  - mouseState.x
            mouseState = {x: event.clientX}
            const edW = Number(logDetails.style.width.replace('px',''))
            logDetails.style.width = (edW - diff) + 'px'
            updateUrlState('details_width', logDetails.style.width)
          }
          window.addEventListener ('mousemove', handleMouseMove)
          window.addEventListener ('mouseup', handleMouseup)
          |]

  jsonTreeAuxillaryCode page.pid page.queryAST
  -- drawerWithURLContent_ : Used when you expand a log item
  -- using the drawer as a global is a workaround since to separate the logs scope from other content and improve scroll performance.
  Components.drawerWithURLContent_ "global-data-drawer" Nothing ""
  -- the loader is used and displayed while loading the content for the global drawer
  template_ [id_ "loader-tmp"] $ span_ [class_ "loading loading-dots loading-md"] ""


-- TODO: centralize to have a single chart rendering component
renderChart :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Maybe Text -> Text -> Text -> Html ()
renderChart pid chartId chartTitle primaryUnitM rateM source extraHxVals = do
  -- let chartAspectRatio "logs" = "aspect-[12/1]"
  --     chartAspectRatio _ = "aspect-[3/1]"
  div_ [class_ "flex-1 space-y-1.5 overflow-x-hidden flex-grow"] do
    div_ [class_ "leading-none flex justify-between items-center"] do
      div_ [class_ "inline-flex gap-3 items-center text-sm text-textStrong"] do
        span_ $ toHtml chartTitle
        whenJust primaryUnitM $ span_ [class_ "bg-fillWeak border border-strokeWeak px-2 py-1 rounded-2xl text-xs "] . toHtml
        whenJust rateM $ span_ [class_ "text-slate-300"] . toHtml
      label_ [class_ "rounded-full border border-slate-300 p-2 inline-flex cursor-pointer"] $ faSprite_ "up-right-and-down-left-from-center" "regular" "w-3 h-3"
    div_
      [ class_ $ "rounded-2xl border border-slate-200 log-chart p-3 "
      , style_ "height:115px"
      , hxGet_ $ "/charts_html?id=" <> chartId <> "&show_legend=false&pid=" <> pid.toText
      , hxTrigger_ "intersect, submit from:#log_explorer_form, add-query from:#filterElement, update-query from:#filterElement"
      , hxVals_ $ "js:{queryAST:window.getQueryFromEditor('" <> chartId <> "'), since: params().since, from: params().from, to:params().to, cols:params().cols, layout:'all', source: params().source" <> extraHxVals <> "}"
      , hxSwap_ "innerHTML"
      ]
      ""


resultTableAndMeta_ :: ApiLogsPageData -> Html ()
resultTableAndMeta_ page =
  div_ [class_ "relative overflow-y-scroll overflow-x-hidden w-full pb-16", id_ "resultTableScroller"] do
    resultTable_ page True
    script_ [text|document.getElementById("resultTableScroller").scrollTop = document.querySelector("#resultTableScroller tr").offsetHeight;|]


resultTable_ :: ApiLogsPageData -> Bool -> Html ()
resultTable_ page mainLog = table_
  [ class_ "w-full  table-auto ctable table-pin-rows table-pin-cols overflow-x-hidden"
  , style_ "height:1px; --rounded-box:0"
  , id_ "resultTable"
  , term "data-source" page.source
  ]
  do
    -- height:1px fixes the cell minimum heights somehow.
    let isLogEventB = isLogEvent page.cols
    when (null page.requestVecs && (isNothing page.query || not mainLog)) do
      whenJust page.isTestLog $ \query -> do
        emptyState_ "Waiting for Test run events..." "You're currently not running any tests yet." page.emptyStateUrl "Go to test editor"
      unless (isJust page.isTestLog) do
        if mainLog
          then
            let subText = "You're currently not sending any data to APItoolkit from your backends yet."
                url = Just $ "/p/" <> page.pid.toText <> "/integration_guides"
             in emptyState_ "Waiting for  events" subText url "Read the setup guide"
          else section_ [class_ "w-max mx-auto"] $ p_ "This request has no outgoing requests yet."
    unless (null page.requestVecs) do
      thead_ $ tr_ [class_ "text-slate-700 border-b font-medium border-y"] $ forM_ page.cols $ logTableHeading_ page.pid isLogEventB
      tbody_ [id_ "log-item-table-body", class_ "w-full log-item-table-body [content-visibility:auto]"] do
        script_
          [text|
          window.latestLogsURLQueryValsFn = function(){
              const datetime = document.querySelector('#log-item-table-body time')?.getAttribute('datetime');
              const updatedTo = datetime
                  ? new Date(new Date(datetime).getTime() + 1).toISOString()
                  : params().to;
              return {from:params().from, to:updatedTo};
          }|]
        tr_
          $ td_ [colspan_ $ show $ length page.cols]
          $ a_
            [ class_ "cursor-pointer inline-flex justify-center py-1 px-56 ml-36 blue-800 bg-blue-100 hover:bg-blue-200 gap-3 items-center"
            , hxTrigger_ "click, every 5s [document.getElementById('streamLiveData').checked]"
            , hxVals_ "js:{queryAST:window.getQueryFromEditor(), since: params().since, cols:params().cols, layout:'all', source: params().source, ...window.latestLogsURLQueryValsFn()}"
            , hxSwap_ "afterend settle:500ms"
            , hxGet_ $ "/p/" <> page.pid.toText <> "/log_explorer?layout=loadmore"
            , hxPushUrl_ "false"
            , hxTarget_ "closest tr"
            , -- using hyperscript instead of hxIndicator_ so the loader isnt distracting by showing up every 5 second and only when clicked
              [__| on click remove .hidden from #loadNewIndicator on htmx:afterRequest add .hidden to #loadNewIndicator |]
            ]
            (span_ [class_ "inline-block"] "check for newer results" >> span_ [id_ "loadNewIndicator", class_ "hidden loading loading-dots loading-sm inline-block pl-3"] "")

        logItemRows_ page.pid (V.take 2 page.requestVecs) page.cols page.colIdxMap page.nextLogsURL page.source page.childSpans


curateCols :: [Text] -> [Text] -> [Text]
curateCols summaryCols cols = sortBy sortAccordingly filteredCols
  where
    defaultSummaryPaths =
      [ "errors_count"
      , "host"
      , "status_code"
      , "method"
      , "url_path"
      , "request_type"
      , "trace_id"
      , "severity_text"
      , "kind"
      , "span_name"
      , "status"
      , "start_time"
      , "end_time"
      , "duration"
      , "body"
      ]
    isLogEventB = isLogEvent cols
    filteredCols = filter (\c -> not isLogEventB || (c `notElem` defaultSummaryPaths || c `elem` summaryCols)) cols

    sortAccordingly :: Text -> Text -> Ordering
    sortAccordingly a b
      | a == "id" = LT
      | b == "id" = GT
      | a == "created_at" && b /= "id" = LT
      | b == "created_at" && a /= "id" = GT
      | a == "rest" = GT
      | b == "rest" = LT
      | otherwise = comparing (`L.elemIndex` filteredCols) a b


logItemRows_ :: Projects.ProjectId -> V.Vector (V.Vector AE.Value) -> [Text] -> HM.HashMap Text Int -> Text -> Text -> V.Vector Telemetry.SpanRecord -> Html ()
logItemRows_ pid requests curatedCols colIdxMap nextLogsURL source chSpns = do
  forM_ requests \reqVec -> do
    let (logItemPath, _reqId) = fromMaybe ("", "") $ requestDumpLogItemUrlPath pid reqVec colIdxMap
    let (_, errCount, errClass) = errorClass True reqVec colIdxMap
    tr_ [class_ "log-row cursor-pointer overflow-hidden", [__|on click toggle .hidden on next <tr/> then toggle .expanded-log on me|]]
      $ forM_ curatedCols \c -> td_ [class_ "pl-3"] $ logItemCol_ source pid reqVec colIdxMap c chSpns
    tr_ [class_ "hidden"] do
      -- used for when a row is expanded.
      td_ [class_ "pl-4"] $ a_ [class_ $ "inline-block h-full " <> errClass, term "data-tippy-content" $ show errCount <> " errors attached to this request"] ""
      td_ [colspan_ $ show $ length curatedCols - 1] $ div_ [hxGet_ $ logItemPath <> "?source=" <> source, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""
  when (V.length requests > 199)
    $ tr_
    $ td_ [colspan_ $ show $ length curatedCols]
    $ a_
      [ class_ "cursor-pointer inline-flex justify-center py-1 px-56 ml-36 blue-800 bg-blue-100 hover:bg-blue-200 gap-3 items-center"
      , hxTrigger_ "click, intersect once"
      , hxSwap_ "outerHTML"
      , hxGet_ nextLogsURL
      , hxTarget_ "closest tr"
      , hxPushUrl_ "false"
      ]
      (span_ [class_ "inline-block"] "LOAD MORE " >> span_ [class_ "loading loading-dots loading-sm inline-block pl-3"] "")


errorClass :: Bool -> V.Vector AE.Value -> HM.HashMap Text Int -> (Int, Int, Text)
errorClass expandedSection reqVec colIdxMap =
  let errCount = lookupVecIntByKey reqVec colIdxMap "errors_count"
      status = lookupVecIntByKey reqVec colIdxMap "status_code"
      errClass =
        if
          | errCount > 0 -> " w-1 bg-red-500 "
          | status >= 400 -> " w-1 bg-warning "
          | expandedSection -> " w-1 bg-blue-200 "
          | otherwise -> " w-1 bg-blue-200 status-indicator "
   in ( status
      , errCount
      , errClass
      )


barSeverityClass :: V.Vector AE.Value -> HM.HashMap Text Int -> Text
barSeverityClass reqVec colIdxMap =
  let severity = fromMaybe "INFO" $ lookupVecTextByKey reqVec colIdxMap "severity"
      cls = case severity of
        "ERROR" -> "bg-red-500"
        "WARNING" -> "bg-warning"
        "INFO" -> "bg-blue-200"
        "DEBUG" -> "bg-gray-300"
        "FATAL" -> "bg-purple-500"
        _ -> "bg-blue-200"
   in cls


logTableHeading_ :: Projects.ProjectId -> Bool -> Text -> Html ()
logTableHeading_ pid True "id" = td_ [class_ "p-0 m-0 whitespace-nowrap w-3"] ""
logTableHeading_ pid True "status_code" = logTableHeadingWrapper_ pid "status_code" Nothing $ toHtml @Text "status"
logTableHeading_ pid True "created_at" = logTableHeadingWrapper_ pid "created_at" (Just "w-[16ch]") $ toHtml @Text "timestamp" >> small_ " (UTC)"
logTableHeading_ pid True "timestamp" = logTableHeadingWrapper_ pid "timestamp" (Just "w-[16ch]") $ toHtml @Text "timestamp" >> small_ " (UTC)"
logTableHeading_ pid True "latency_breakdown" = logTableHeadingWrapper_ pid "latency_breakdown" (Just "w-[16ch]") $ toHtml @Text "latency_breakdown"
logTableHeading_ pid True "service" = logTableHeadingWrapper_ pid "service" (Just "w-[20ch]") $ toHtml @Text "service"
logTableHeading_ pid True "rest" = logTableHeadingWrapper_ pid "rest" Nothing $ toHtml @Text "summary"
logTableHeading_ pid isLogEventB col = logTableHeadingWrapper_ pid col Nothing $ toHtml $ Unsafe.last $ T.splitOn "•" col


logTableHeadingWrapper_ :: Projects.ProjectId -> Text -> Maybe Text -> Html () -> Html ()
logTableHeadingWrapper_ pid title classes child = td_
  [ class_ $ "cursor-pointer p-0 m-0 whitespace-nowrap " <> maybeToMonoid classes
  ]
  do
    span_ [class_ "text-slate-200"] "|"
    div_
      [class_ "dropdown pl-2", term "data-tippy-content" title]
      do
        div_ [tabindex_ "0", role_ "button", class_ "py-1"] do
          child
          span_ [class_ "ml-1 p-0.5 border border-slate-200 rounded inline-flex"] $ faSprite_ "chevron-down" "regular" "w-3 h-3"
        ul_ [tabindex_ "0", class_ "dropdown-content z-[1] menu p-2 shadow bg-bgBase rounded-box min-w-[15rem]"] do
          li_ [class_ "underline underline-offset-2"] $ toHtml title
          li_
            $ a_
              [ hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
              , hxPushUrl_ "true"
              , hxVals_ $ "js:{queryAST:params().queryAST,cols:removeNamedColumnToSummary('" <> title <> "'),layout:'resultTable'}"
              , hxTarget_ "#resultTable"
              , hxSwap_ "outerHTML"
              ]
              "Hide column"


isLogEvent :: [Text] -> Bool
isLogEvent cols = all @[] (`elem` cols) ["id", "created_at"] || all @[] (`elem` cols) ["id", "timestamp"]


renderBadge :: Text -> Text -> Text -> Html ()
renderBadge className content tip = span_ [class_ className, term "data-tippy-content" tip] $ toHtml content


renderLogBadge :: Text -> V.Vector AE.Value -> HM.HashMap Text Int -> Text -> Html ()
renderLogBadge key reqVec colIdxMap className = renderBadge (className <> " cbadge ") (fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap key) key


renderMethod :: V.Vector AE.Value -> HM.HashMap Text Int -> Html ()
renderMethod reqVec colIdxMap =
  let method = fromMaybe "/" $ lookupVecTextByKey reqVec colIdxMap "method"
   in renderBadge ("min-w-[4rem] cbadge " <> maybe "badge-ghost" getMethodColor (lookupVecTextByKey reqVec colIdxMap "method")) method "method"


renderTimestamp :: Text -> V.Vector AE.Value -> HM.HashMap Text Int -> Html ()
renderTimestamp key reqVec colIdxMap =
  time_ [class_ "monospace whitespace-nowrap text-slate-600 ", term "data-tippy-content" "timestamp", datetime_ timestamp] $ toHtml (displayTimestamp timestamp)
  where
    timestamp = maybeToMonoid $ lookupVecTextByKey reqVec colIdxMap key


renderStatusCode :: V.Vector AE.Value -> HM.HashMap Text Int -> Html ()
renderStatusCode reqVec colIdxMap =
  renderBadge (getStatusColor $ lookupVecIntByKey reqVec colIdxMap "status_code") (show @Text $ lookupVecIntByKey reqVec colIdxMap "status_code") "status"


renderIconWithTippy :: Text -> Text -> Html () -> Html ()
renderIconWithTippy iconClass tip = a_ [class_ $ "shrink-0 inline-flex " <> iconClass, term "data-tippy-content" tip]


logItemCol_ :: Text -> Projects.ProjectId -> V.Vector AE.Value -> HM.HashMap Text Int -> Text -> V.Vector Telemetry.SpanRecord -> Html ()
logItemCol_ source pid reqVec colIdxMap "id" chSpns = do
  let (status, errCount, errClass) = errorClass False reqVec colIdxMap
  let severityClass = barSeverityClass reqVec colIdxMap
  div_ [class_ "grid grid-cols-3 items-center max-w-12 min-w-10"] do
    span_ [class_ "col-span-1 h-5 rounded flex"] $ renderIconWithTippy (if source == "logs" then severityClass else errClass) (show errCount <> " errors attached; status " <> show status) " "
    faSprite_ "chevron-right" "solid" "h-3 col-span-1 text-gray-500 chevron log-chevron "
logItemCol_ _ _ reqVec colIdxMap "created_at" _ = renderTimestamp "created_at" reqVec colIdxMap
logItemCol_ _ _ reqVec colIdxMap "timestamp" _ = renderTimestamp "timestamp" reqVec colIdxMap
logItemCol_ _ _ reqVec colIdxMap "status_code" _ = renderStatusCode reqVec colIdxMap
logItemCol_ _ _ reqVec colIdxMap "method" _ = renderMethod reqVec colIdxMap
logItemCol_ _ _ reqVec colIdxMap "severity_text" _ = renderLogBadge "severity_text" reqVec colIdxMap (getSeverityColor $ T.toLower $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "severity_text")
logItemCol_ _ _ reqVec colIdxMap "duration" _ = renderBadge "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak" (toText (getDurationNSMS $ toInteger $ lookupVecIntByKey reqVec colIdxMap "duration")) "duration"
logItemCol_ _ _ reqVec colIdxMap "span_name" _ = renderLogBadge "span_name" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
logItemCol_ _ _ reqVec colIdxMap "service" _ = renderLogBadge "service" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
logItemCol_ _ pid reqVec colIdxMap "latency_breakdown" childSpans =
  let spanId = lookupVecTextByKey reqVec colIdxMap "latency_breakdown"
   in Spans.spanLatencyBreakdown $ V.filter (\s -> s.parentSpanId == spanId) childSpans
logItemCol_ _ _ reqVec colIdxMap "body" _ = renderLogBadge "body" reqVec colIdxMap "space-x-2 whitespace-nowrap"
logItemCol_ _ _ reqVec colIdxMap "kind" _ = renderBadge "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak" (fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "kind") "kind"
logItemCol_ _ _ reqVec colIdxMap "status" _ = renderLogBadge "status" reqVec colIdxMap (getSpanStatusColor $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "status")
logItemCol_ source pid reqVec colIdxMap "rest" _ = div_ [class_ "space-x-2 whitespace-nowrap max-w-8xl overflow-hidden "] do
  let key = "rest"
  case source of
    "logs" -> forM_ ["severity_text", "body"] \v -> logItemCol_ source pid reqVec colIdxMap v []
    "spans" -> forM_ ["status", "kind", "duration", "span_name"] \v -> logItemCol_ source pid reqVec colIdxMap v []
    _ -> do
      if lookupVecTextByKey reqVec colIdxMap "request_type" == Just "Incoming"
        then renderIconWithTippy "text-slate-500" "Incoming Request" (faSprite_ "arrow-down-left" "solid" "h-3")
        else renderIconWithTippy "text-blue-700" "Outgoing Request" (faSprite_ "arrow-up-right" "solid" "h-3")
      logItemCol_ source pid reqVec colIdxMap "status_code" []
      logItemCol_ source pid reqVec colIdxMap "method" []
      renderLogBadge "url_path" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
      logItemCol_ source pid reqVec colIdxMap "duration" []
      renderLogBadge "host" reqVec colIdxMap "cbadge-sm badge-neutral border  border-strokeWeak  bg-fillWeak"
      span_ $ toHtml $ maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)
logItemCol_ _ _ reqVec colIdxMap key _ = renderBadge "space-nowrap overflow-x-hidden max-w-lg" (maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)) key


requestDumpLogItemUrlPath :: Projects.ProjectId -> V.Vector AE.Value -> HM.HashMap Text Int -> Maybe (Text, Text)
requestDumpLogItemUrlPath pid rd colIdxMap = do
  rdId <- lookupVecTextByKey rd colIdxMap "id"
  rdCreatedAt <- lookupVecTextByKey rd colIdxMap "created_at" <|> lookupVecTextByKey rd colIdxMap "timestamp"
  pure ("/p/" <> pid.toText <> "/log_explorer/" <> rdId <> "/" <> rdCreatedAt, rdId)


-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Text -> Html ()
jsonTreeAuxillaryCode pid queryAST = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu  origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-bgBase ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1", role_ "none"] do
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{queryAST:params().queryAST,cols:toggleColumnToSummary(event),layout:'resultTable', since: params().since, from: params().from, to:params().to, source: params().source}"
          , hxTarget_ "#resultTable"
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
        const { fieldPath: path, fieldValue: value } = event.target.closest('[data-field-path]').dataset;
        document.getElementById("filterElement").handleAddQuery({detail:{
            "tag": operation,
            "contents": [path, JSON.parse(value)]
        }});
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
