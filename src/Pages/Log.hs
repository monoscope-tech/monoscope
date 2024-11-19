module Pages.Log (
  apiLogH,
  LogsGet (..),
  ApiLogsPageData (..),
  resultTable_,
  curateCols,
  logQueryBox_,
)
where

import Control.Error (hush)
import Data.Aeson (Value)
import Data.Aeson qualified as AE
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.List (elemIndex)
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, zonedTimeToUTC)
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt (commaizeF, fmt)
import Lucid
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
import Pages.Traces.Spans qualified as Spans
import Pkg.Components qualified as Components
import Pkg.Parser (pSource, parseQueryToAST)
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


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders LogsGet)
apiLogH pid queryM queryASTM cols' cursorM' sinceM fromM toM layoutM sourceM targetSpansM hxRequestM hxBoostedM = do
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
  let (fromD, toD, currentRange) = Components.parseTimeRange now (Components.TimePickerP sinceM fromM toM)
  tableAsVecE <- RequestDumps.selectLogTable pid queryAST cursorM' (fromD, toD) summaryCols (parseMaybe pSource =<< sourceM) targetSpansM

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE

  freeTierExceeded <-
    dbtToEff
      $ if project.paymentPlan == "Free"
        then (> 5000) <$> RequestDumps.getLastSevenDaysTotalRequest pid
        else pure False

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Explorer"
          , pageActions = Just $ Components.timepicker_ (Just "log_explorer_form") currentRange
          , navTabs = Just $ div_ [class_ "tabs tabs-boxed tabs-md tabs-outline items-center bg-slate-200 text-slate-700"] do
              a_ [onclick_ "window.setQueryParamAndReload('source', 'requests')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if source == "requests" then "tab-active" else ""] "Requests"
              a_ [onclick_ "window.setQueryParamAndReload('source', 'logs')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if source == "logs" then "tab-active" else ""] "Logs"
              a_ [onclick_ "window.setQueryParamAndReload('source', 'spans')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if source == "spans" then "tab-active" else ""] "Traces"
              -- a_ [onclick_ "window.setQueryParamAndReload('source', 'metrics')", role_ "tab", class_ $ "tab py-1.5 !h-auto " <> if source == "metrics" then "tab-active" else ""] "Metrics"
          }
  currTime <- liftIO getCurrentTime
  let createdUTc = zonedTimeToUTC project.createdAt
      (days, hours, minutes, _seconds) = convertToDHMS $ diffUTCTime currTime createdUTc
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
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore") source
          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing Nothing source
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
              }
      case (layoutM, hxRequestM, hxBoostedM) of
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
    LogsGetRows Projects.ProjectId (V.Vector (V.Vector Value)) [Text] (HM.HashMap Text Int) Text Text (V.Vector Telemetry.SpanRecord)
  | LogsGetResultTable ApiLogsPageData Bool
  | LogsGetError (PageCtx Text)
  | LogsGetErrorSimple Text


instance ToHtml LogsGet where
  toHtml (LogPage (PageCtx conf pa_dat)) = toHtml $ PageCtx conf $ apiLogsPage pa_dat
  toHtml (LogsGetRows pid requestVecs cols colIdxMap nextLogsURL source chSpns) = toHtml $ logItemRows_ pid requestVecs cols colIdxMap nextLogsURL source chSpns
  toHtml (LogsGetResultTable page bol) = toHtml $ resultTable_ page bol
  toHtml (LogsGetErrorSimple err) = span_ [class_ "text-red-500"] $ toHtml err
  toHtml (LogsGetError (PageCtx conf err)) = toHtml $ PageCtx conf err
  toHtmlRaw = toHtml


logQueryBox_ :: Projects.ProjectId -> Maybe Text -> Text -> Maybe Text -> Text -> Html ()
logQueryBox_ pid currentRange source targetSpan queryAST = do
  form_
    [ hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
    , hxPushUrl_ "true"
    , hxVals_ "js:{queryAST:window.getQueryFromEditor(), since: params().since, from: params().from, to:params().to, cols:params().cols, layout:'all', source: params().source}"
    , termRaw "hx-on::before-request" ""
    , hxTarget_ "#resultTable"
    , hxSwap_ "outerHTML"
    , id_ "log_explorer_form"
    , hxIndicator_ "#run-query-indicator"
    , [__| on keydown if event.key is 'Enter' halt |]
    ]
    do
      div_ [class_ "flex gap-2 items-stretch justify-center"] do
        div_ [class_ "cursor-pointer relative bg-slate-100 rounded-2xl border border-slate-200 inline-flex justify-center"] do
          div_ [class_ "flex gap-2 justify-center items-center px-2 text-slate-600 "] $ "Saved queries" >> faSprite_ "chevron-down" "regular" "w-3 h-3"
        div_ [class_ "p-1 pl-3 flex-1 flex gap-2 bg-slate-100 rounded-2xl border border-slate-200 justify-between items-stretch"] do
          div_ [id_ "queryEditor", class_ "h-14 hidden overflow-hidden bg-gray-200 flex-1 flex items-center"] pass
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
          button_ [class_ "rounded-xl p-3 bg-slate-200 text-slate-700 inline-flex items-center"] $ faSprite_ "floppy-disk" "regular" "h-5 w-5"
          button_
            [type_ "submit", class_ "leading-none rounded-xl p-3 cursor-pointer bg-gradient-to-b from-[#067cff] to-[#0850c5] text-white"]
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


data ApiLogsPageData = ApiLogsPageData
  { pid :: Projects.ProjectId
  , resultCount :: Int
  , requestVecs :: V.Vector (V.Vector Value)
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
  }


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto pt-2 px-6 gap-3.5 w-full flex flex-col h-full overflow-hidden pb-12  group/pg", id_ "apiLogsPage"] do
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
      logQueryBox_ page.pid page.currentRange page.source page.targetSpans page.queryAST

      div_ [class_ "flex flex-row gap-4 mt-3"] do
        renderChart page.pid "reqsChartsECP" "All requests" (Just $ fmt (commaizeF page.resultCount)) Nothing page.source ""
        unless (page.source == "logs") $ renderChart page.pid "reqsChartsErrP" "Errors" Nothing Nothing page.source ", theme:'roma'"
        unless (page.source == "logs") $ renderChart page.pid "reqsChartsLatP" "Latency" Nothing Nothing page.source ", chart_type:'LineCT', group_by:'GBDurationPercentile'"

    div_ [class_ "flex gap-3.5 overflow-hidden"] do
      div_ [class_ "card-round w-1/5 shrink-0 flex flex-col gap-2 p-2  group-has-[.toggle-filters:checked]/pg:hidden "] do
        input_ [placeholder_ "Search filter", class_ "rounded-2xl bg-slate-25 px-4 py-2 border border-slate-300 "]
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
            div_ [class_ "flex justify-between items-center text-slate-950 pb-2"] $ span_ "MMethos" >> faSprite_ "chevron-down" "regular" "w-3 h-3"
            div_ [class_ "flex justify-between"] do
              div_ [class_ "flex gap-1.5 items-center  text-slate-950"] $ input_ [type_ "checkbox", class_ "checkbox "] >> span_ [class_ "bg-[#067cff] shrink-0 w-1 h-5 rounded"] " " >> span_ [] "GET"
              span_ "8,675"
            div_ [class_ "flex justify-between"] do
              div_ [class_ "flex gap-1.5 items-center  text-slate-950"] $ input_ [type_ "checkbox", class_ "checkbox "] >> span_ [class_ "text-green-500 shrink-0 w-1 h-5 rounded"] " " >> span_ [] "POST"
              span_ "4,459"

      div_ [class_ "grow flex-1 space-y-1.5 overflow-hidden"] do
        div_ [class_ "flex gap-2  pt-1"] do
          label_ [class_ "gap-1 flex items-center cursor-pointer"] do
            faSprite_ "side-chevron-left-in-box" "regular" "w-4 h-4 group-has-[.toggle-filters:checked]/pg:rotate-180 "
            span_ [class_ "hidden group-has-[.toggle-filters:checked]/pg:block"] "Show"
            span_ [class_ "group-has-[.toggle-filters:checked]/pg:hidden"] "Hide"
            "filters"
            input_ [type_ "checkbox", class_ "toggle-filters hidden", checked_]
          span_ [class_ "text-slate-200"] "|"
          div_ [class_ ""] $ span_ [class_ "text-slate-950"] (toHtml @Text $ fmt $ commaizeF page.resultCount) >> span_ (toHtml (" " <> page.source <> " found"))
        div_ [class_ "card-round divide-y flex flex-col h-full overflow-hidden"] $ resultTableAndMeta_ page
  jsonTreeAuxillaryCode page.pid page.queryAST
  -- drawerWithURLContent_ : Used when you expand a log item
  -- using the drawer as a global is a workaround since to separate the logs scope from other content and improve scroll performance.
  Components.drawerWithURLContent_ "global-data-drawer" Nothing ""
  -- the loader is used and displayed while loading the content for the global drawer
  template_ [id_ "loader-tmp"] $ span_ [class_ "loading loading-dots loading-md"] ""


renderChart :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Maybe Text -> Text -> Text -> Html ()
renderChart pid chartId chartTitle primaryUnitM rateM source extraHxVals = do
  let chartAspectRatio "logs" = "aspect-[12/1]"
      chartAspectRatio _ = "aspect-[3/1]"
  div_ [class_ "flex-1 space-y-1.5"] do
    div_ [class_ "leading-none flex justify-between items-center"] do
      div_ [class_ "inline-flex gap-3 items-center"] do
        span_ $ toHtml chartTitle
        whenJust primaryUnitM \primaryUnit -> span_ [class_ "bg-slate-200 px-2 py-1 rounded-3xl"] (toHtml primaryUnit)
        whenJust rateM \rate -> span_ [class_ "text-slate-300"] (toHtml rate)
      label_ [class_ "rounded-full border border-slate-300 p-2 inline-flex cursor-pointer"] $ faSprite_ "up-right-and-down-left-from-center" "regular" "w-3 h-3"
    div_
      [ id_ chartId
      , class_ $ "rounded-2xl border border-slate-200 log-chart p-3 group-has-[.toggle-chart:checked]/pg:hidden " <> (chartAspectRatio source)
      , hxGet_ $ "/charts_html?id=" <> chartId <> "&show_legend=false&pid=" <> pid.toText
      , hxTrigger_ "intersect, htmx:beforeRequest from:#log_explorer_form"
      , hxVals_ $ "js:{query_raw:window.getQueryFromEditor('" <> chartId <> "'), since: params().since, from: params().from, to:params().to, cols:params().cols, layout:'all', source: params().source" <> extraHxVals <> "}"
      , hxSwap_ "innerHTML"
      ]
      ""


resultTableAndMeta_ :: ApiLogsPageData -> Html ()
resultTableAndMeta_ page =
  div_ [class_ "relative overflow-y-scroll overflow-x-hidden h-full w-full pb-16"]
    $ resultTable_ page True


resultTable_ :: ApiLogsPageData -> Bool -> Html ()
resultTable_ page mainLog = table_
  [ class_ "w-full  table-auto ctable table-pin-rows table-pin-cols overflow-x-hidden [contain:strict] [content-visibility:auto]"
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
      thead_ $ tr_ [class_ "text-slate-700 border-b font-medium"] $ forM_ page.cols $ logTableHeading_ page.pid isLogEventB
      tbody_ [id_ "w-full log-item-table-body [content-visibility:auto]"] $ logItemRows_ page.pid page.requestVecs page.cols page.colIdxMap page.nextLogsURL page.source page.childSpans


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
      | otherwise = comparing (`elemIndex` filteredCols) a b


logItemRows_ :: Projects.ProjectId -> V.Vector (V.Vector Value) -> [Text] -> HM.HashMap Text Int -> Text -> Text -> V.Vector Telemetry.SpanRecord -> Html ()
logItemRows_ pid requests curatedCols colIdxMap nextLogsURL source chSpns = do
  forM_ requests \reqVec -> do
    let (logItemPath, _reqId) = fromMaybe ("", "") $ requestDumpLogItemUrlPath pid reqVec colIdxMap
    let (_, errCount, errClass) = errorClass True reqVec colIdxMap
    tr_ [class_ "cursor-pointer overflow-hidden", [__|on click toggle .hidden on next <tr/> then toggle .expanded-log on me|]]
      $ forM_ curatedCols \c -> td_ [class_ "pl-3"] $ logItemCol_ source pid reqVec colIdxMap c chSpns
    tr_ [class_ "hidden"] do
      -- used for when a row is expanded.
      td_ [class_ "pl-4"] $ a_ [class_ $ "inline-block h-full " <> errClass, term "data-tippy-content" $ show errCount <> " errors attached to this request"] ""
      td_ [colspan_ $ show $ length curatedCols - 1] $ div_ [hxGet_ $ logItemPath <> "?source=" <> source, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""
  when (Vector.length requests > 199)
    $ tr_
    $ td_ [colspan_ $ show $ length curatedCols]
    $ a_
      [ class_ "cursor-pointer inline-flex justify-center py-1 px-56 ml-36 blue-800 bg-blue-100 hover:bg-blue-200 gap-3 items-center"
      , hxTrigger_ "click, intersect once"
      , hxSwap_ "outerHTML"
      , hxGet_ nextLogsURL
      , hxTarget_ "closest tr"
      ]
      (span_ [class_ "inline-block"] "LOAD MORE " >> span_ [class_ "loading loading-dots loading-sm inline-block pl-3"] "")


errorClass :: Bool -> V.Vector Value -> HM.HashMap Text Int -> (Int, Int, Text)
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


barSeverityClass :: V.Vector Value -> HM.HashMap Text Int -> Text
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
        ul_ [tabindex_ "0", class_ "dropdown-content z-[1] menu p-2 shadow bg-slate-25 rounded-box min-w-[15rem]"] do
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


renderLogBadge :: Text -> V.Vector Value -> HM.HashMap Text Int -> Text -> Html ()
renderLogBadge key reqVec colIdxMap className = renderBadge (className <> " cbadge ") (fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap key) key


renderMethod :: V.Vector Value -> HM.HashMap Text Int -> Html ()
renderMethod reqVec colIdxMap =
  let method = fromMaybe "/" $ lookupVecTextByKey reqVec colIdxMap "method"
   in renderBadge ("min-w-[4rem] cbadge " <> maybe "badge-ghost" getMethodColor (lookupVecTextByKey reqVec colIdxMap "method")) method "method"


renderTimestamp :: Text -> V.Vector Value -> HM.HashMap Text Int -> Html ()
renderTimestamp key reqVec colIdxMap =
  renderBadge "monospace whitespace-nowrap text-slate-600 " (displayTimestamp $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap key) "timestamp"


renderStatusCode :: V.Vector Value -> HM.HashMap Text Int -> Html ()
renderStatusCode reqVec colIdxMap =
  renderBadge (getStatusColor $ lookupVecIntByKey reqVec colIdxMap "status_code") (show @Text $ lookupVecIntByKey reqVec colIdxMap "status_code") "status"


renderIconWithTippy :: Text -> Text -> Html () -> Html ()
renderIconWithTippy iconClass tip content =
  a_ [class_ $ "shrink-0 inline-flex " <> iconClass, term "data-tippy-content" tip] content


logItemCol_ :: Text -> Projects.ProjectId -> V.Vector Value -> HM.HashMap Text Int -> Text -> V.Vector Telemetry.SpanRecord -> Html ()
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
logItemCol_ _ _ reqVec colIdxMap "duration" _ = renderBadge "cbadge-sm badge-neutral" (toText (getDurationNSMS $ toInteger $ lookupVecIntByKey reqVec colIdxMap "duration")) "duration"
logItemCol_ _ _ reqVec colIdxMap "span_name" _ = renderLogBadge "span_name" reqVec colIdxMap "cbadge-sm badge-neutral"
logItemCol_ _ _ reqVec colIdxMap "service" _ = renderLogBadge "service" reqVec colIdxMap "cbadge-sm badge-neutral"
logItemCol_ _ pid reqVec colIdxMap "latency_breakdown" childSpans =
  let spanId = lookupVecTextByKey reqVec colIdxMap "latency_breakdown"
   in Spans.spanLatencyBreakdown $ V.filter (\s -> s.parentSpanId == spanId) childSpans
logItemCol_ _ _ reqVec colIdxMap "body" _ = renderLogBadge "body" reqVec colIdxMap "space-x-2 whitespace-nowrap"
logItemCol_ _ _ reqVec colIdxMap "kind" _ = renderBadge "cbadge-sm badge-neutral" (fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "kind") "kind"
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
      renderLogBadge "url_path" reqVec colIdxMap "cbadge-sm badge-neutral"
      logItemCol_ source pid reqVec colIdxMap "duration" []
      renderLogBadge "host" reqVec colIdxMap "cbadge-sm badge-neutral"
      p_ $ toHtml $ maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)
logItemCol_ _ _ reqVec colIdxMap key _ = renderBadge "space-nowrap overflow-x-hidden max-w-lg" (maybe "" (unwrapJsonPrimValue True) (lookupVecByKey reqVec colIdxMap key)) key


requestDumpLogItemUrlPath :: Projects.ProjectId -> V.Vector Value -> HM.HashMap Text Int -> Maybe (Text, Text)
requestDumpLogItemUrlPath pid rd colIdxMap = do
  rdId <- lookupVecTextByKey rd colIdxMap "id"
  rdCreatedAt <- lookupVecTextByKey rd colIdxMap "created_at" <|> lookupVecTextByKey rd colIdxMap "timestamp"
  pure ("/p/" <> pid.toText <> "/log_explorer/" <> rdId <> "/" <> rdCreatedAt, rdId)


-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Text -> Html ()
jsonTreeAuxillaryCode pid queryAST = do
  template_ [id_ "log-item-context-menu-tmpl "] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu  origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-slate-25 ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] do
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
          , onclick_ "filterByField(event, '==')"
          ]
          "Filter by field"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1  hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , onclick_ "filterByField(event, '!=')"
          ]
          "Exclude field"

  script_
    [text|
    function filterByField(event, operation) {
        const { fieldPath: path, fieldValue: value } = event.target.closest('[data-field-path]').dataset,
              editorVal = window.queryBuilderValue ?? window.editor?.getValue() ?? '',
              separator = editorVal && !editorVal.toLowerCase().endsWith('and') && !editorVal.toLowerCase().endsWith('or') ? ' AND ' : editorVal ? ' ' : '',
              newVal = editorVal + separator + `$${path.replace(/\[\*\]\.(\d+)\./g, '[*].')} $${operation} $${value}`;
        document.querySelector('#filterElement')?.setBuilderValue?.(newVal);
        window.editor?.setValue?.(newVal);
        htmx.trigger("#log_explorer_form", "submit");
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

    var execd = false
    document.addEventListener('DOMContentLoaded', function(){
      window.setQueryBuilderFromParams()
    })

|]
