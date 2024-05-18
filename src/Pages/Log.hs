module Pages.Log (
  apiLogH,
)
where

import Control.Error (hush)
import Data.Aeson (Value)
import Data.Containers.ListUtils (nubOrd)
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.List (elemIndex)
import Data.Text qualified as T
import Data.Time (
  UTCTime,
  addUTCTime,
  getCurrentTime,
  secondsToNominalDiffTime,
 )
import Data.Time.Format (
  defaultTimeLocale,
  formatTime,
  parseTimeM,
 )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Fmt (commaizeF, fmt)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pages.Monitors.Alerts qualified as Alerts
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Types
import Utils
import Witch (from)


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


apiLogH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
apiLogH pid queryM cols' cursorM' sinceM fromM toM layoutM hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid

  let summaryCols = T.splitOn "," (fromMaybe "" cols')
  let query = fromMaybe "" queryM
  now <- liftIO getCurrentTime
  let (fromD, toD, currentRange) = case sinceM of
        Just "1H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime 3600) now, Just now, Just "Last Hour")
        Just "24H" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24) now, Just now, Just "Last 24 Hours")
        Just "7D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 7) now, Just now, Just "Last 7 Days")
        Just "14D" -> (Just $ addUTCTime (negate $ secondsToNominalDiffTime $ 3600 * 24 * 14) now, Just now, Just "Last 14 Days")
        _ -> do
          let f = (iso8601ParseM (from @Text $ fromMaybe "" fromM) :: Maybe UTCTime)
          let t = (iso8601ParseM (from @Text $ fromMaybe "" toM) :: Maybe UTCTime)
          let start = toText . formatTime defaultTimeLocale "%F %T" <$> f
          let end = toText . formatTime defaultTimeLocale "%F %T" <$> t
          let range = case (start, end) of
                (Just s, Just e) -> Just (s <> "-" <> e)
                _ -> Nothing
          (f, t, range)

  tableAsVecE <- dbtToEff $ RequestDumps.selectLogTable pid query cursorM' (fromD, toD) summaryCols

  -- FIXME: we're silently ignoring parse errors and the likes.
  let tableAsVecM = hush tableAsVecE

  freeTierExceeded <-
    dbtToEff
      $ if project.paymentPlan == "Free"
        then do
          totalRequest <- RequestDumps.getTotalRequestForCurrentMonth pid
          return $ totalRequest > 20000
        else do
          return False
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "API Log Explorer"
          }

  case tableAsVecM of
    Just tableAsVec -> do
      let (requestVecs, colNames, requestsCount) = tableAsVec
          curatedColNames = nubOrd $ curateCols summaryCols colNames
          colIdxMap = listToIndexHashMap colNames
          reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "created_at") =<< (requestVecs V.!? (V.length requestVecs - 1))
          nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' reqLastCreatedAtM sinceM fromM toM (Just "loadmore")
          resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing Nothing Nothing Nothing Nothing
          page =
            ApiLogsPageData
              { pid
              , resultCount = requestsCount
              , requestVecs
              , cols = curatedColNames
              , colIdxMap
              , nextLogsURL
              , resetLogsURL
              , currentRange
              , exceededFreeTier = freeTierExceeded
              , query = queryM
              }

      case (layoutM, hxRequestM, hxBoostedM) of
        (Just "loadmore", Just "true", _) -> addRespHeaders $ logItemRows_ pid requestVecs curatedColNames colIdxMap nextLogsURL
        (Just "resultTable", Just "true", _) -> addRespHeaders $ resultTable_ page False
        (Just "all", Just "true", _) -> addRespHeaders $ resultTable_ page True
        _ -> do
          addRespHeaders $ bodyWrapper bwconf $ apiLogsPage page
    Nothing -> do
      case (layoutM, hxRequestM, hxBoostedM) of
        (Just "loadmore", Just "true", _) -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ ""
        (Just "resultTable", Just "true", _) -> do
          addRespHeaders $ span_ [class_ "text-red-500"] "Something went wrong"
        (Just "all", Just "true", _) -> do
          addErrorToast "Something went wrong" Nothing
          addRespHeaders $ ""
        _ -> do
          addRespHeaders $ bodyWrapper bwconf $ h4_ [] "Something went wrong"


timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour")
  , ("24H", "Last 24 Hours")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]


logQueryBox_ :: Projects.ProjectId -> Maybe Text -> Html ()
logQueryBox_ pid currentRange =
  form_
    [ class_ "card-round w-full text-sm"
    , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
    , hxPushUrl_ "true"
    , hxVals_ "js:{query:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols, layout:'all'}"
    , termRaw "hx-on::before-request" ""
    , hxTarget_ "#resultTable"
    , hxSwap_ "outerHTML"
    , id_ "log_explorer_form"
    , hxIndicator_ "#run-query-indicator"
    ]
    do
      nav_ [class_ "flex flex-row p-2 content-end justify-between items-baseline border-slate-100"] do
        a_ [class_ "inline-block"] "Query"
        div_ [class_ "flex gap-10 items-center"] do
          div_ [class_ "relative p-1 "] do
            div_ [class_ "relative"] do
              input_ [type_ "hidden", id_ "since_input"]
              input_ [type_ "hidden", id_ "custom_range_input"]
              a_
                [ class_ "relative btn btn-sm btn-outline"
                , [__| on click toggle .hidden on #timepickerBox|]
                ]
                do
                  mIcon_ "clock" "h-4 w-4"
                  span_ [class_ "inline-block", id_ "currentRange"] $ toHtml (fromMaybe "Last 14 Days" currentRange)
                  faSprite_ "chevron-down" "regular" "h-3 w-3 inline-block"
              div_ [id_ "timepickerBox", class_ "hidden absolute z-10 mt-1  rounded-md flex"] do
                div_ [class_ "inline-block w-84 overflow-auto bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"] do
                  timePickerItems
                    & mapM_ \(val, title) ->
                      a_
                        [ class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 "
                        , term "data-value" val
                        , term "data-title" title
                        , [__| on click set #custom_range_input's value to my @data-value then log my @data-value 
                                   then toggle .hidden on #timepickerBox 
                                   then set #currentRange's innerText to my @data-title
                                   then htmx.trigger("#log_explorer_form", "submit")
                         |]
                        ]
                        $ toHtml title
                  a_ [class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ", [__| on click toggle .hidden on #timepickerSidebar |]] "Custom date range"
                div_ [class_ "inline-block relative hidden", id_ "timepickerSidebar"] do
                  div_ [id_ "startTime", class_ "hidden"] ""
          div_ $ div_ [class_ "form-control"] $ label_ [class_ "label cursor-pointer space-x-2"] do
            input_ [type_ "checkbox", class_ "toggle", id_ "toggleQueryEditor", onclick_ "toggleQueryBuilder()"]
            span_ [class_ "label-text"] "Use editor"

          button_
            [type_ "submit", class_ "btn btn-sm btn-success"]
            do
              span_ [id_ "run-query-indicator", class_ "refresh-indicator htmx-indicator query-indicator loading loading-dots loading-md"] ""
              faSprite_ "sparkles" "regular" "h-3 w-3 inline-block"
              span_ "Run query"
      div_ do
        div_ [class_ "bg-gray-200"] do
          div_ [id_ "queryEditor", class_ "h-14 hidden overflow-hidden bg-gray-200"] pass
        div_ [id_ "queryBuilder", class_ "mb-4"] do
          termRaw "filter-element" [id_ "filterElement"] ("" :: Text)


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
  }


apiLogsPage :: ApiLogsPageData -> Html ()
apiLogsPage page = do
  section_ [class_ "mx-auto px-10 py-2 gap-2 w-full flex flex-col h-[98%] overflow-hidden ", id_ "apiLogsPage"] do
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
    script_
      []
      [text|
    function getQueryFromEditor(){
     const toggler = document.getElementById("toggleQueryEditor")
     if(toggler.checked) {
          return window.editor.getValue();
      }else {
          return window.queryBuilderValue || "";
      }
    }

    function getTimeRange () {
      const rangeInput = document.getElementById("custom_range_input")
      const range = rangeInput.value.split("/")
      if (range.length == 2)  {
         return {from: range[0], to: range[1], since: ''}
      }
      if (range[0]!=''){
        return {since: range[0], from: '', to: ''}
       }
       if (params().since==''){
        return {since: '14D', from: params().from, to: params().to}
      }
       return {since: params().since, from: params().from, to: params().to}
    }
      |]
    logQueryBox_ page.pid page.currentRange

    div_ [class_ "card-round w-full  grow divide-y flex flex-col text-sm h-full overflow-hidden"] do
      div_ [class_ "flex-1 "] do
        div_ [class_ "pl-3 py-1 flex flex-row justify-between"] do
          a_ [class_ "cursor-pointer inline-block pr-3 space-x-2 bg-blue-50 hover:bg-blue-100 blue-800 p-1 rounded-md", [__|on click toggle .hidden on #reqsChartParent|]] do
            faSprite_ "chart-bar" "regular" "h-3 w-3 inline-block"
            span_ [] "toggle chart"
          a_
            [ class_ "cursor-pointer flex gap-2 items-center pr-3"
            , hxGet_ page.resetLogsURL
            , hxTarget_ "#log-item-table-body"
            , hxSwap_ "innerHTML scroll:#log-item-table-body:top"
            , hxIndicator_ "#refresh-indicator"
            ]
            do
              span_ [id_ "refresh-indicator", class_ "refresh-indicator htmx-indicator query-indicator loading loading-dots loading-md"] ""
              faSprite_ "arrows-rotate" "regular" "h-3 w-3 inline-block"
              span_ [] "refresh"
        div_
          [ id_ "reqsChartsECP"
          , class_ "px-5"
          , style_ "height:150px"
          , hxGet_ $ "/charts_html?id=reqsChartsEC&show_legend=true&pid=" <> page.pid.toText
          , hxTrigger_ "intersect,  htmx:beforeRequest from:#log_explorer_form"
          , hxVals_ "js:{query_raw:getQueryFromEditor(), since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to, cols:params().cols, layout:'all'}"
          , hxSwap_ "innerHTML"
          ]
          ""
      resultTableAndMeta_ page
      jsonTreeAuxillaryCode page.pid


resultTableAndMeta_ :: ApiLogsPageData -> Html ()
resultTableAndMeta_ page = do
  section_ [class_ " w-full h-full overflow-hidden"] $ section_ [class_ " w-full tabs tabs-bordered items-start overflow-hidden h-full place-content-start", role_ "tablist"] do
    input_ [type_ "radio", name_ "logExplorerMain", role_ "tab", class_ "tab", checked_, Aria.label_ $ "Query results (" <> fmt (commaizeF page.resultCount) <> ")"]
    div_ [class_ "relative overflow-y-scroll overflow-x-hidden h-full w-full tab-content", role_ "tabpanel"] do
      resultTable_ page True
      div_ [style_ "width:2000px"] pass

    input_ [type_ "radio", name_ "logExplorerMain", role_ "tab", class_ "tab", Aria.label_ "Alerts"]
    div_ [class_ "relative overflow-y-scroll h-full tab-content", role_ "tabpanel"] do
      div_ [hxGet_ $ "/p/" <> page.pid.toText <> "/alerts", hxTrigger_ "intersect", hxSwap_ "innerHTML", id_ "alertsListContainer"] ""

    input_ [type_ "radio", name_ "logExplorerMain", role_ "tab", class_ "tab", Aria.label_ "Save as Alert"]
    div_ [class_ "relative overflow-y-scroll overflow-x-hidden h-full tab-content p-3", role_ "tabpanel"] do
      Alerts.editAlert_ page.pid Nothing
      div_ [style_ "width:2000px"] pass


resultTable_ :: ApiLogsPageData -> Bool -> Html ()
resultTable_ page mainLog = table_ [class_ "w-full table table-sm table-pin-rows table-pin-cols overflow-x-hidden", style_ "height:1px", id_ "resultTable"] do
  -- height:1px fixes the cell minimum heights somehow.
  let isLogEventB = isLogEvent page.cols
  when (null page.requestVecs && isNothing page.query) $ do
    if mainLog
      then do
        section_ [class_ "w-max  mx-auto my-16 p-5 sm:py-14 sm:px-24 items-center flex gap-16"] do
          div_ [] $ faSprite_ "empty-set" "solid" "h-24 w-24"
          div_ [class_ "flex flex-col gap-2"] do
            h2_ [class_ "text-2xl font-bold"] "Waiting for events..."
            p_ "You're currently not sending any data to APItoolkit from your backends yet."
            a_ [href_ $ "/p/" <> page.pid.toText <> "/integration_guides", class_ "w-max btn btn-indigo -ml-1 text-md"] "Read the setup guide"
      else do
        section_ [class_ "w-max mx-auto"] do
          p_ "This request has no outgoing requests yet."
  unless (null page.requestVecs) $ do
    thead_ $ tr_ $ forM_ page.cols $ logTableHeading_ page.pid isLogEventB
    tbody_ [id_ "w-full log-item-table-body"] do
      logItemRows_ page.pid page.requestVecs page.cols page.colIdxMap page.nextLogsURL


curateCols :: [Text] -> [Text] -> [Text]
curateCols summaryCols cols = sortBy sortAccordingly filteredCols
  where
    defaultSummaryPaths = ["errors_count", "host", "status_code", "method", "url_path", "request_type"]
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


logItemRows_ :: Projects.ProjectId -> V.Vector (V.Vector Value) -> [Text] -> HM.HashMap Text Int -> Text -> Html ()
logItemRows_ pid requests curatedCols colIdxMap nextLogsURL = do
  forM_ requests \reqVec -> do
    let logItemPath = requestDumpLogItemUrlPath pid reqVec colIdxMap
    let (_, errCount, errClass) = errorClass True reqVec colIdxMap
    tr_ [class_ "cursor-pointer ", [__|on click toggle .hidden on next <tr/> then toggle .expanded-log on me|]]
      $ forM_ curatedCols (td_ . logItemCol_ pid reqVec colIdxMap)
    tr_ [class_ "hidden"] $ do
      -- used for when a row is expanded.
      td_ $ a_ [class_ $ "inline-block h-full " <> errClass, term "data-tippy-content" $ show errCount <> " errors attached to this request"] ""
      td_ [colspan_ $ show $ length curatedCols - 1] $ div_ [hxGet_ $ fromMaybe "" logItemPath, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""
  when (Vector.length requests > 199)
    $ tr_
    $ td_ [colspan_ $ show $ length curatedCols]
    $ a_
      [ class_ "cursor-pointer inline-flex justify-center py-1 px-56 ml-36 blue-800 bg-blue-100 hover:bg-blue-200 text-center "
      , hxTrigger_ "click"
      , hxSwap_ "outerHTML"
      , hxGet_ nextLogsURL
      , hxTarget_ "closest tr"
      , hxIndicator_ "next .htmx-indicator"
      ]
      (span_ [class_ "inline-block"] "LOAD MORE " >> span_ [class_ "htmx-indicator loading loading-dots loading-lg inline-block pl-3"] "")


errorClass :: Bool -> V.Vector Value -> HM.HashMap Text Int -> (Int, Int, Text)
errorClass expandedSection reqVec colIdxMap =
  let errCount = lookupVecIntByKey reqVec colIdxMap "errors_count"
      status = lookupVecIntByKey reqVec colIdxMap "status_code"
      errClass =
        if
          | errCount > 0 -> " w-1 bg-red-500 "
          | status >= 400 -> " w-1 bg-warning "
          | expandedSection -> " w-1 bg-blue-200 "
          | otherwise -> " w-1 bg-transparent status-indicator "
   in ( status
      , errCount
      , errClass
      )


logTableHeading_ :: Projects.ProjectId -> Bool -> Text -> Html ()
logTableHeading_ pid True "id" = logTableHeadingWrapper_ pid "_" $ toHtml @Text ""
logTableHeading_ pid True "status_code" = logTableHeadingWrapper_ pid "status_code" $ toHtml @Text "status"
logTableHeading_ pid True "created_at" = logTableHeadingWrapper_ pid "created_at" $ toHtml @Text "timestamp"
logTableHeading_ pid isLogEventB col = logTableHeadingWrapper_ pid col $ toHtml $ Unsafe.last $ T.splitOn "•" col


logTableHeadingWrapper_ :: Projects.ProjectId -> Text -> Html () -> Html ()
logTableHeadingWrapper_ pid title child = td_
  [ class_ "bg-base-200 cursor-pointer p-0 m-0 "
  ]
  $ div_
    [class_ "dropdown", term "data-tippy-content" title]
    do
      div_ [tabindex_ "0", role_ "button", class_ "py-2 px-3 block"] child
      ul_ [tabindex_ "0", class_ "dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box min-w-[15rem]"] do
        li_ [class_ "underline underline-offset-2"] $ toHtml title
        li_
          $ a_
            [ hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
            , hxPushUrl_ "true"
            , hxVals_ $ "js:{query:params().query,cols:removeNamedColumnToSummary('" <> title <> "'),layout:'resultTable'}"
            , hxTarget_ "#resultTable"
            , hxSwap_ "outerHTML"
            ]
            "Hide column"


isLogEvent :: [Text] -> Bool
isLogEvent cols = all @[] (`elem` cols) ["id", "created_at"]


displayTimestamp :: Text -> Text
displayTimestamp inputDateString =
  maybe
    T.empty
    (toText . formatTime defaultTimeLocale "%b %d %H:%M:%S")
    (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (toString inputDateString) :: Maybe UTCTime)


logItemCol_ :: Projects.ProjectId -> V.Vector Value -> HM.HashMap Text Int -> Text -> Html ()
logItemCol_ pid reqVec colIdxMap "id" = do
  let (status, errCount, errClass) = errorClass False reqVec colIdxMap
  let logItemPath = requestDumpLogItemUrlPath pid reqVec colIdxMap
  div_ [class_ "grid grid-cols-3 gap-4 items-center max-w-8"] do
    a_ [class_ $ "col-span-1 shrink-0 inline-block h-full w-1 " <> errClass, term "data-tippy-content" $ show errCount <> " errors attached to this request; status " <> show status] " "
    button_
      [ class_ "col-span-1"
      , hxGet_ (fromMaybe "" logItemPath <> "/detailed")
      , hxTarget_ "#expand-log-modal"
      , [__|on click remove .hidden from #expand-log-modal|]
      ]
      $ faSprite_ "link" "solid" "h-3 w-3 text-blue-500"
    faSprite_ "chevron-right" "solid" "h-3 w-3 col-span-1 ml-1 text-gray-500 chevron log-chevron "
logItemCol_ _ reqVec colIdxMap "created_at" = span_ [class_ "font-mono whitespace-nowrap ", term "data-tippy-content" "timestamp"] $ toHtml $ displayTimestamp $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "created_at"
logItemCol_ _ reqVec colIdxMap "status_code" = span_ [class_ $ "badge " <> getStatusColor (lookupVecIntByKey reqVec colIdxMap "status_code"), term "data-tippy-content" "status"] $ toHtml $ show @Text $ lookupVecIntByKey reqVec colIdxMap "status_code"
logItemCol_ _ reqVec colIdxMap "method" = span_ [class_ $ "min-w-[4rem] badge " <> maybe "badge-ghost" getMethodColor (lookupVecTextByKey reqVec colIdxMap "method"), term "data-tippy-content" "method"] $ toHtml $ fromMaybe "/" $ lookupVecTextByKey reqVec colIdxMap "method"
logItemCol_ pid reqVec colIdxMap key@"rest" = div_ [class_ "space-x-2 whitespace-nowrap max-w-8xl overflow-x-hidden "] do
  if lookupVecTextByKey reqVec colIdxMap "request_type" == Just "Incoming"
    then span_ [class_ "text-center w-3 inline-flex ", term "data-tippy-content" "Incoming Request"] $ faSprite_ "arrow-down-left" "solid" "h-3 w-3 text-gray-400"
    else span_ [class_ "text-center w-3 inline-flex ", term "data-tippy-content" "Outgoing Request"] $ faSprite_ "arrow-up-right" "solid" "h-3 w-3 text-red-800"
  logItemCol_ pid reqVec colIdxMap "status_code"
  logItemCol_ pid reqVec colIdxMap "method"
  span_ [class_ "badge badge-ghost ", term "data-tippy-content" "URL Path"] $ toHtml $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "url_path"
  span_ [class_ "badge badge-ghost ", term "data-tippy-content" "Host"] $ toHtml $ fromMaybe "" $ lookupVecTextByKey reqVec colIdxMap "host"
  span_ [] $ toHtml $ maybe "" unwrapJsonPrimValue (lookupVecByKey reqVec colIdxMap key)
logItemCol_ _ reqVec colIdxMap key =
  div_ [class_ "xwhitespace-nowrap xoverflow-x-hidden max-w-lg ", term "data-tippy-content" key]
    $ toHtml
    $ maybe "" unwrapJsonPrimValue (lookupVecByKey reqVec colIdxMap key)


requestDumpLogItemUrlPath :: Projects.ProjectId -> V.Vector Value -> HM.HashMap Text Int -> Maybe Text
requestDumpLogItemUrlPath pid rd colIdxMap = do
  rdId <- lookupVecTextByKey rd colIdxMap "id"
  rdCreatedAt <- lookupVecTextByKey rd colIdxMap "created_at"
  pure $ "/p/" <> pid.toText <> "/log_explorer/" <> rdId <> "/" <> rdCreatedAt


-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Html ()
jsonTreeAuxillaryCode pid = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu text-sm origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1", role_ "none"] do
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-0"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{query:params().query,cols:toggleColumnToSummary(event),layout:'resultTable', since: getTimeRange().since, from: getTimeRange().from, to:getTimeRange().to}"
          , hxTarget_ "#resultTable"
          , hxSwap_ "outerHTML"
          , -- , hxIndicator_ "#query-indicator"
            [__|init set fp to (closest @data-field-path) then 
                  if isFieldInSummary(fp) then set my innerHTML to 'Remove field from summary' end|]
          ]
          "Add field to Summary"
        a_
          [ class_ "cursor-pointer text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-1"
          , [__|on click 
                  if 'clipboard' in window.navigator then 
                    call navigator.clipboard.writeText((previous <.log-item-field-value/>)'s innerText)
                    send successToast(value:['Value has been added to the Clipboard']) to <body/>
                    halt
                  end|]
          ]
          "Copy field value"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-2"
          , onclick_ "filterByField(event, '==')"
          ]
          "Filter by field"
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-3"
          , onclick_ "filterByField(event, '!=')"
          ]
          "Exclude field"

  script_
    [text|
      const picker = new easepick.create({
        element: '#startTime',
        css: [
          'https://cdn.jsdelivr.net/npm/@easepick/bundle@1.2.0/dist/index.css',
        ],
        inline: true,
        plugins: ['RangePlugin', 'TimePlugin'],
        autoApply: false,
        setup(picker) {
          picker.on('select', (e) => {
            const start = JSON.stringify(e.detail.start).slice(1, -1);
            const end = JSON.stringify(e.detail.end).slice(1, -1);
            const rangeInput = document.getElementById("custom_range_input")
            rangeInput.value = start + "/" + end
            document.getElementById("timepickerBox").classList.toggle("hidden")
            document.getElementById("currentRange").innerText = start.split("T")[0] + " - " + end.split("T")[0]
            htmx.trigger("#log_explorer_form", "submit")
          });
        },
      });
      window.picker = picker; 
        document.getElementById("log_explorer_form").addEventListener("keydown", function (e) {
            if (e.key === "Enter") {
                e.preventDefault();
            }
        });

      function buildCurlRequest(event) {
          const data = JSON.parse(event.currentTarget.dataset.reqjson);
          const request_headers = data.request_headers;
          const request_body = data.request_body;
          const url = "https://" + data.host + data.raw_url + " \\\n";
          const method = data.method;
          let curlCommand = 'curl -X "' + method + '" ' + url + " ";
          let curlHeaders = "";
          if (typeof request_headers === "object") {
            try {
              curlHeaders = Object.entries(request_headers)
                .map(([key, value]) => '-H "' + key + " " + value + '" \\\n')
                .join("");
            } catch (error) {}
          }
          if (curlHeaders != "") curlCommand += curlHeaders;
          let reqBody = "";
          if (method.toLowerCase() != "get") {
            try {
              reqBody = " -d '" + JSON.stringify(request_body) + "' \\\n";
            } catch (error) {
              reqBody = "-data-raw " + '"' + request_body + '"  \\\n';
            }
          }
          if (reqBody !== " -d ''") {
            curlCommand += reqBody;
          }
          navigator.clipboard.writeText(curlCommand)
            .then(() => {
              const event = new CustomEvent("successToast", {
                  detail: { value: ["Curl command copied"] },
                  bubbles: true,
                  composed: true,
                });
              document.querySelector("body").dispatchEvent(event);
            })
          
      }

    function downloadJson(event) {
         event.stopPropagation()
         const json = event.currentTarget.dataset.reqjson
         var blob = new Blob([json], { type: "application/json" });
         var a = document.createElement("a");
         a.href = URL.createObjectURL(blob);
         a.download = "request-data-" + (new Date().toString()) + ".json";
         a.textContent = "";
         document.body.appendChild(a);
         a.click();
         document.body.removeChild(a); 
       }

    function filterByField(event, operation) {
       const target = event.target.parentNode.parentNode.parentNode
       const path = target.getAttribute('data-field-path');
       const value = target.getAttribute('data-field-value');
       const regex = /\[\*\]\.(\d+)\./g;
       const replacedPath = path.replace(regex, '[*].');
       const filter = replacedPath + ' ' + operation + ' ' + value
       let editorVal = '' 
       if(window.queryBuilderValue) {
        editorVal = window.queryBuilderValue
        }else if(window.editor) {
           editorVal = window.editor.getValue()
        }
       let newVal = ''
       if (editorVal.toLowerCase().endsWith("and") || editorVal.toLowerCase().endsWith("or")) {
           newVal = editorVal + " " + filter
       }else {
        if (editorVal.length == 0) {
            newVal = filter
          }else {
            newVal = editorVal + " AND " + filter
          }
       }

       if (newVal != "") {
          const filterComp = document.querySelector('#filterElement')
          if(filterComp) {
               filterComp.setBuilderValue(newVal)
            }
          if(window.editor) {
             window.editor.setValue(newVal)
            }
          htmx.trigger("#log_explorer_form", "submit")
        }

    }

    var params = () => new Proxy(new URLSearchParams(window.location.search), {
      get: (searchParams, prop) => searchParams.get(prop)??"",
    });

    var toggleColumnToSummary = (e)=>{
      const cols = (params().cols??"").split(",").filter(x=>x!="");
      const subject = e.target.closest('.log-item-field-parent').dataset.fieldPath; 
      if (cols.includes(subject)) {
        return [...new Set(cols.filter(x=>x!=subject))].join(",");
      } 
      cols.push(subject)
      return [... new Set (cols)].join (",")
    }

    var removeNamedColumnToSummary = (namedCol)=>{
      console.log(params())
      const cols = (params().cols??"").split(",").filter(x=>x!="");
      const subject = namedCol; 

      cols.forEach(x=>console.log(subject,x.replaceAll(".", "•").replaceAll("[", "❲").replaceAll("]", "❳") ))

      return [...new Set(cols.filter(
            x=>subject.toLowerCase() != x.replaceAll(".", "•")
                           .replaceAll("[", "❲")
                           .replaceAll("]", "❳").toLowerCase()
                                    ))].join(",");
    }

    var isFieldInSummary = field => params().cols.split(",").includes(field);
    


    function toggleQueryBuilder() {
     document.getElementById("queryBuilder").classList.toggle("hidden")
     document.getElementById("queryEditor").classList.toggle("hidden")
     if(!window.editor) {
        var codeMirrorEditor = CodeMirror(document.getElementById('queryEditor'), {
          value: params().query || window.queryBuilderValue || '',
          mode:  "javascript",
          theme: "elegant",
          lineNumbers: true,
        });
        window.editor = codeMirrorEditor
     }

     if(!document.getElementById("queryEditor").className.includes("hidden")) {
          setTimeout(() => {
            window.editor.setValue(window.queryBuilderValue)
          },10)
      }else {
          const filterComp = document.querySelector('#filterElement')
          if(filterComp) {
            setTimeout(()=> {
             filterComp.setBuilderValue(window.editor.getValue())
            },10)
          }
      }
    }

    var execd = false
    document.addEventListener('DOMContentLoaded', function(){
      window.setQueryBuilderFromParams()
    })



function updateMarkAreas(chartId, warningVal, incidentVal) {
  warningVal = parseInt(warningVal, 10);
  incidentVal = parseInt(incidentVal, 10)
  var myChart = echarts.getInstanceByDom(document.getElementById(chartId));

  // Retrieve the current chart options
  var options = myChart.getOption();

  // Iterate over each series to update markAreas
  options.series.forEach((seriesItem) => {
      // Reset markArea data for clean update
      seriesItem.markArea = {label:{show:false}, data: []};

      // Define markArea for Warning if warningVal is not null
      if (warningVal !== null && warningVal!=NaN) {
          seriesItem.markArea.data.push([{
              name: 'Warning',
              yAxis: warningVal,
              itemStyle: {
                  color: 'rgba(255, 212, 0, 0.4)'
              }
          }, {
              yAxis: incidentVal
          }]);
      }

      // Define markArea for Incident
      seriesItem.markArea.data.push([{
          name: 'Incident',
          yAxis: incidentVal,
          itemStyle: {
              color: 'rgba(255, 173, 177, 0.5)'
          }
      }, {
          yAxis: 'max'
      }]);
  });

  // Apply the updated options back to the chart
  myChart.setOption({series: options.series}, false);
}
    |]
