module Pages.Log (apiLog, apiLogItem, logItemRows, expandAPIlogItem, expandAPIlogItem') where

import Config
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.ByteString.Lazy qualified as BS
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (ZonedTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.UUID qualified as UUID
import Data.Vector (Vector, iforM_, (!?))
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (Insert), execute, withPool)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Fmt
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Lucid.Svg (d_, fill_, path_, use_, viewBox_)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pkg.Components (loader)
import Relude

import System.Clock
import Utils


-- $setup
-- >>> import Relude
-- >>> import Data.Vector qualified as Vector
-- >>> import Data.Aeson.QQ (aesonQQ)
-- >>> import Data.Aeson


apiLog :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> DashboardM (Html ())
apiLog sess pid queryM cols' fromM hxRequestM hxBoostedM = do
  let cols = T.splitOn "," (fromMaybe "" cols')
  let query = fromMaybe "" queryM
  let fromM' = case fromM of
        Nothing -> Nothing
        Just "" -> Nothing
        Just a -> Just a

  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, requests) <- liftIO
        $ withPool pool
        $ do
          project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
          requests <- RequestDumps.selectRequestDumpByProject pid query fromM'
          pure (project, requests)

      reqChartTxt <- liftIO $ withPool pool $ RequestDumps.throughputBy pid Nothing Nothing Nothing Nothing Nothing (3 * 60) Nothing queryM (Nothing, Nothing)
      let reqLastCreatedAtM = (^. #createdAt) <$> viaNonEmpty last (Vector.toList requests) -- FIXME: unoptimal implementation, converting from vector to list for last
      let fromTempM = toText . formatTime defaultTimeLocale "%F %T" <$> reqLastCreatedAtM
      let nextLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' fromTempM

      let resultCount = maybe 0 (^. #fullCount) (requests !? 0)
      case (hxRequestM, hxBoostedM) of
        (Just "true", Nothing) -> pure $ do
          span_ [id_ "result-count", hxSwapOob_ "outerHTML"] $ show resultCount
          reqChart reqChartTxt True
          logItemRows pid requests cols nextLogsURL
        _ -> do
          let bwconf =
                (def :: BWConfig)
                  { sessM = Just sess
                  , currProject = project
                  , pageTitle = "API Log Explorer"
                  }
          let resetLogsURL = RequestDumps.requestDumpLogUrlPath pid queryM cols' Nothing
          pure $ bodyWrapper bwconf $ apiLogsPage pid resultCount requests cols reqChartTxt nextLogsURL resetLogsURL


apiLogItem :: Sessions.PersistentSession -> Projects.ProjectId -> UUID.UUID -> ZonedTime -> DashboardM (Html ())
apiLogItem sess pid rdId createdAt = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      startTime <- liftIO $ getTime Monotonic
      logItemM <- liftIO $ withPool pool $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      afterProccessing <- liftIO $ getTime Monotonic
      let content = case logItemM of
            Just req -> apiLogItemView req (RequestDumps.requestDumpLogItemUrlPath pid req)
            Nothing -> div_ "invalid log request ID"
      endTime <- liftIO $ getTime Monotonic
      liftIO $ putStrLn $ fmtLn $ " APILOG pipeline microsecs: queryDuration " +| (toNanoSecs (diffTimeSpec startTime afterProccessing)) `div` 1000 |+ " -> processingDuration " +| toNanoSecs (diffTimeSpec afterProccessing endTime) `div` 1000 |+ " -> TotalDuration " +| toNanoSecs (diffTimeSpec startTime endTime) `div` 1000 |+ ""
      pure content


expandAPIlogItem :: Sessions.PersistentSession -> Projects.ProjectId -> UUID.UUID -> ZonedTime -> DashboardM (Html ())
expandAPIlogItem sess pid rdId createdAt = do
  pool <- asks pool
  startTime <- liftIO $ getTime Monotonic
  logItemM <- liftIO $ withPool pool $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
  afterProccessing <- liftIO $ getTime Monotonic
  let content = case logItemM of
        Just req -> expandAPIlogItem' req True
        Nothing -> div_ [class_ "h-full flex flex-col justify-center items-center"] do
          p_ [] "Request not found"
  pure content


expandAPIlogItem' :: RequestDumps.RequestDumpLogItem -> Bool -> Html ()
expandAPIlogItem' req modal = do
  div_ [class_ "flex flex-col w-full pb-[100px]"] $ do
    div_ [class_ "w-full flex flex-col gap-2 gap-4"] do
      let methodColor = getMethodBgColor req.method
      let statusColor = getStatusColor req.statusCode
      div_ [class_ "flex gap-4 items-center"] do
        div_ [class_ $ "text-white font-semibold px-2 py-1 rounded min-w-[70px] text-center " <> methodColor] $ toHtml req.method
        div_ [class_ $ "text-lg font-bold px-2 " <> statusColor] $ show req.statusCode
        div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] $ do
          mIcon_ "calender" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" (req.createdAt)
      if modal
        then do
          div_
            [ class_ "flex gap-2 px-4 items-center border border-dashed h-[50px]"
            , id_ "copy_share_link"
            ]
            do
              div_ [class_ "relative", style_ "width:150px", onblur_ "document.getElementById('expire_container').classList.add('hidden')"] $ do
                button_
                  [ onclick_ "toggleExpireOptions(event)"
                  , id_ "toggle_expires_btn"
                  , class_ "w-full flex gap-2 text-gray-600 justify_between items-center cursor-pointer px-2 py-1 border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200"
                  ]
                  $ do
                    p_ [style_ "width: calc(100% - 25px)", class_ "text-sm truncate ..."] "Expires in: 1 hour"
                    img_ [src_ "/assets/svgs/select_chevron.svg", style_ "height:15px; width:15px"]
                div_ [id_ "expire_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] $ do
                  ["1 hour", "8 hours", "1 day"] & mapM_ \sw -> do
                    button_
                      [ onclick_ "expireChanged(event)"
                      , term "data-expire-value" sw
                      , class_ "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
                      ]
                      $ toHtml sw
              button_
                [ class_ "flex flex-col gap-1 bg-blue-500 px-2 py-1 rounded text-white"
                , term "data-req-id" (show req.id)
                , onclick_ "getShareLink(event)"
                ]
                "Get share link"
        else pass

    -- url, endpoint, latency, request size, repsonse size
    div_ [class_ "flex flex-col mt-4 p-4 justify-between w-full rounded-xl border"] do
      div_ [class_ "text-lg mb-2"] do
        span_ [class_ "text-gray-500 font-semibold"] "Endpoint: "
        span_ [] $ toHtml req.urlPath
      div_ [class_ "text-lg"] do
        span_ [class_ "text-gray-500 font-semibold"] "URL: "
        span_ [] $ toHtml req.rawUrl
      div_ [class_ "flex gap-2 mt-4"] do
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] $ do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "clock" "h-4 w-4 text-gray-400"
            span_ [class_ "text-md font-bold"] $ show (req.durationNs `div` 1000) <> " ms"
          p_ [class_ "text-gray-500"] "Latency"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] $ do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "upload" "h-4 w-4 text-gray-400"
            let reqSize = BS.length $ AE.encode req.requestBody
            span_ [class_ "text-md font-bold"] $ show (reqSize - 2) <> " bytes"
          p_ [class_ "text-gray-500"] "Request size"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] $ do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "download4" "h-4 w-4 text-gray-400"
            let respSize = BS.length $ AE.encode req.responseBody
            span_ [class_ "text-md font-bold"] $ show (respSize - 2) <> " bytes"
          p_ [class_ "text-gray-500"] "Response size"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] $ do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "projects" "h-5 w-5 text-gray-400"
            span_ [class_ "text-md font-bold"] $ show req.sdkType
          p_ [class_ "text-gray-500"] "Framework"
    -- request details
    div_ [class_ "border rounded-lg mt-8", id_ "request_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Request Details"
      ul_ [class_ "px-4 flex gap-10 border-b text-gray-500"] do
        li_ [] do
          button_
            [ class_ "sdk_tab sdk_tab_active"
            , onclick_ "changeTab('req_body','request_detail_container')"
            , id_ "req_body"
            ]
            "Body"
        li_ [] do
          button_
            [ class_ "sdk_tab"
            , onclick_ "changeTab('req_headers', 'request_detail_container')"
            , id_ "req_headers"
            ]
            "Headers"
        li_ [] do
          button_
            [ class_ "sdk_tab"
            , onclick_ "changeTab('query_params', 'request_detail_container')"
            , id_ "query_params"
            ]
            "Query Params"
        li_ [] do
          button_
            [ class_ "sdk_tab"
            , onclick_ "changeTab('path_params', 'request_detail_container')"
            , id_ "path_params"
            ]
            "Path Params"
      div_ [class_ "bg-gray-50 m-4  p-2 rounded-lg border sdk_tab_content sdk_tab_content_active", id_ "req_body_json"] do
        jsonValueToHtmlTree req.requestBody
      div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg hidden border sdk_tab_content", id_ "req_headers_json"] do
        jsonValueToHtmlTree req.requestHeaders
      div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg hidden border sdk_tab_content", id_ "query_params_json"] do
        jsonValueToHtmlTree req.queryParams
      div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg hidden border sdk_tab_content", id_ "path_params_json"] do
        jsonValueToHtmlTree req.pathParams

    -- response details
    div_ [class_ "border rounded-lg mt-8", id_ "reponse_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Response Details"
      ul_ [class_ "px-4 flex gap-10 border-b text-gray-500"] do
        li_ [] do
          button_
            [ class_ "sdk_tab sdk_tab_active"
            , onclick_ "changeTab('res_body', 'reponse_detail_container')"
            , id_ "res_body"
            ]
            "Body"
        li_ [] do
          button_
            [ class_ "sdk_tab"
            , onclick_ "changeTab('res_headers', 'reponse_detail_container')"
            , id_ "res_headers"
            ]
            "Headers"
      div_ [class_ "bg-gray-50 m-4  p-2 rounded-lg border sdk_tab_content sdk_tab_content_active", id_ "res_body_json"] do
        jsonValueToHtmlTree req.responseBody
      div_ [class_ "bg-gray-50 m-4 p-2 hidden rounded-lg border sdk_tab_content", id_ "res_headers_json"] do
        jsonValueToHtmlTree req.responseHeaders


apiLogsPage :: Projects.ProjectId -> Int -> Vector RequestDumps.RequestDumpLogItem -> [Text] -> Text -> Text -> Text -> Html ()
apiLogsPage pid resultCount requests cols reqChartTxt nextLogsURL resetLogsURL = do
  section_ [class_ "mx-auto px-10 py-2 gap-2 flex flex-col h-[98%] overflow-hidden "] $ do
    div_
      [ style_ "z-index:26; width: min(90vw, 800px)"
      , class_ "fixed hidden right-0 bg-white overflow-y-scroll h-[calc(100%-60px)] border-l border-l-2 shadow"
      , id_ "expand-log-modal"
      ]
      $ do
        div_ [class_ "relative ml-auto w-full", style_ ""] do
          div_ [class_ "flex justify-end  w-full p-4 "] do
            button_ [[__|on click add .hidden to #expand-log-modal|]] do
              img_ [class_ "h-8", src_ "/assets/svgs/close.svg"]
          let postP = "/p/" <> pid.toText <> "/share/"
          form_
            [ hxPost_ postP
            , hxSwap_ "innerHTML"
            , hxTarget_ "#copy_share_link"
            , id_ "share_log_form"
            ]
            do
              input_ [type_ "hidden", value_ "1 hour", name_ "expiresIn", id_ "expire_input"]
              input_ [type_ "hidden", value_ "", name_ "reqId", id_ "req_id_input"]
          div_ [id_ "log-modal-content-loader", class_ "bg-white rounded-lg shadow p-4 absolute top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2"] do
            loader
          div_ [class_ "px-2", id_ "log-modal-content"] pass
    form_
      [ class_ "card-round w-full text-sm"
      , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
      , hxPushUrl_ "true"
      , hxVals_ "js:{query:getQueryFromEditor(), cols:params().cols}"
      , hxTarget_ "#log-item-table-body"
      , id_ "log_explorer_form"
      , hxIndicator_ "#query-indicator"
      ]
      $ do
        nav_ [class_ "flex flex-row p-2 content-end justify-between items-baseline border-slate-100"] $ do
          a_ [class_ "inline-block"] "Query"
          button_ [type_ "submit", class_ "cursor-pointer inline-block space-x-1 bg-blue-100 hover:bg-blue-200 blue-800 py-1 px-2 rounded-lg"] do
            img_ [src_ "/assets/svgs/sparkles.svg", class_ "w-3 h-3 inline-block"]
            span_ "Run query"
        div_ $ do
          div_ [id_ "queryEditor", class_ "h-14"] ""

    div_ [class_ "card-round w-full grow divide-y flex flex-col text-sm h-full overflow-y-hidden overflow-x-hidden"] $ do
      div_ [class_ "pl-3 py-1 space-x-5 flex flex-row justify-between"] $ do
        a_ [class_ "cursor-pointer inline-block pr-3 space-x-2 bg-blue-50 hover:bg-blue-100 blue-800 p-1 rounded-md", [__|on click toggle .hidden on #reqsChartParent|]] $ do
          img_ [src_ "/assets/svgs/cube-transparent.svg", class_ "w-4 inline-block"]
          span_ [] "toggle chart"
      reqChart reqChartTxt False
      div_ [class_ "pl-3 py-2 space-x-5 flex flex-row justify-between"] $ do
        div_ [class_ "inline-block space-x-3"] $ do
          strong_ "Query results"
          span_ [class_ "space-x-1"] $ do
            span_ [id_ "result-count"] $ show resultCount
            span_ " log entries"
        a_
          [ class_ "cursor-pointer inline-block pr-3 space-x-2"
          , hxGet_ resetLogsURL
          , hxTarget_ "#log-item-table-body"
          , hxSwap_ "innerHTML scroll:#log-item-table-body:top"
          , hxIndicator_ "#query-indicator"
          ]
          $ do
            svg_ [class_ "w-4 h-4 icon text-slate-500 inline-block"] $ use_ [href_ "/assets/svgs/sprite/sprite.svg#refresh"]
            span_ [] "refresh"

      jsonTreeAuxillaryCode pid
      div_ [class_ "table-fixed grow w-full min-w-full h-full divide-y flex flex-col monospace overflow-hidden"] $ do
        div_ [class_ "text-xs bg-gray-100 gray-400"] $ do
          div_ [class_ "flex flex-row text-left space-x-4"] $ do
            span_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-8"] ""
            span_ [class_ "font-normal inline-block py-1.5 p-1 px-2 w-36"] "TIMESTAMP"
            span_ [class_ "font-normal inline-block py-1.5 p-1 px-2 grow"] "SUMMARY"
        div_ [class_ "htmx-indicator query-indicator", id_ "query-indicator"] $ do
          loader
        div_ [class_ "grow overflow-y-scroll h-full whitespace-nowrap text-sm divide-y overflow-x-hidden", id_ "log-item-table-body"] $ do
          logItemRows pid requests cols nextLogsURL
  script_
    [text|
function changeTab(tabId, parent) {
  const p = document.getElementById(parent);
  const tabLinks = p.querySelectorAll('.sdk_tab');
  tabLinks.forEach(link => link.classList.remove('sdk_tab_active'));
  const clickedTabLink = document.getElementById(tabId);
  clickedTabLink.classList.add('sdk_tab_active')
  const tabContents = p.querySelectorAll('.sdk_tab_content');
  tabContents.forEach(content => {
    content.classList.add("hidden")
    content.classList.remove ("sdk_tab_content_active")
  });
  const tabContent = document.getElementById(tabId + '_json');
  tabContent.classList.remove("hidden")
  setTimeout(()=>{tabContent.classList.add("sdk_tab_content_active")},10)
}

function toggleExpireOptions (event) {
    event.preventDefault()
    event.stopPropagation()
    const container = document.querySelector('#expire_container')
    if(container) {
     container.classList.toggle('hidden')
    }
}

function getShareLink(event) {
  event.target.innerText = "Generating..."
  const reqId = event.target.getAttribute ("data-req-id")
  document.querySelector('#req_id_input').value = reqId
  htmx.trigger('#share_log_form','submit')
}

function expireChanged(event) {
    event.preventDefault()
    event.stopPropagation()
    const current = document.querySelector('#toggle_expires_btn')
    if(current && current.firstChild) {
       current.firstChild.innerText = "Expires in: " + event.target.getAttribute("data-expire-value")
       document.querySelector("#expire_input").value = event.target.getAttribute("data-expire-value")
    }
}
  |]
reqChart :: Text -> Bool -> Html ()
reqChart reqChartTxt hxOob = do
  div_ [id_ "reqsChartParent", class_ "p-5", hxSwapOob_ $ show hxOob] $ do
    div_ [id_ "reqsChartsEC", class_ "", style_ "height:100px"] ""
    script_
      [text| throughputEChart("reqsChartsEC", $reqChartTxt, [], true) |]


logItemRows :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> [Text] -> Text -> Html ()
logItemRows pid requests cols nextLogsURL = do
  requests & traverse_ \req -> do
    let logItemPath = RequestDumps.requestDumpLogItemUrlPath pid req
    div_
      [ class_ "flex flex-row border-l-4 border-l-transparent divide-x space-x-4 hover:bg-blue-50 cursor-pointer"
      , term "data-log-item-path" logItemPath
      , term
          "_"
          [text|
            install LogItemExpandable
        |]
      ]
      $ do
        div_ [class_ "flex-none inline-block p-1 px-2 w-8 flex justify-center align-middle"] $ do
          img_ [src_ "/assets/svgs/cheveron-right.svg", class_ "w-1.5 log-chevron"]
        div_ [class_ "flex-none inline-block p-1 px-2 w-36 overflow-hidden"] $ toHtml @String $ formatTime defaultTimeLocale "%F %T" (req ^. #createdAt)
        div_ [class_ "inline-block p-1 px-2 grow"] $ do
          let reqJSON = AE.toJSON req
          let colValues = concatMap (\col -> findValueByKeyInJSON (T.splitOn "." col) reqJSON) cols
          -- FIXME: probably inefficient implementation and should be optimized
          zip cols colValues
            & traverse_
              \(col, colValue) ->
                div_ [class_ "relative inline-block log-item-field-parent", term "data-field-path" col] $ do
                  a_
                    [ class_ "cursor-pointer mx-1 inline-block bg-blue-100 hover:bg-blue-200 blue-900 px-3 rounded-xl monospace log-item-field-anchor log-item-field-value"
                    , term "data-field-path" col
                    , [__|install LogItemMenuable|]
                    ]
                    $ toHtml colValue
          let cls = "mx-1 inline-block slate-900 px-3 rounded-xl monospace" :: Text
          let method_cls = cls <> getMethodBgColor (req ^. #method)
          span_ [class_ method_cls] $ toHtml $ req ^. #method
          span_ [class_ $ cls <> " bg-gray-100"] $ toHtml $ req ^. #urlPath
          let status_cls = if req ^. #statusCode > 400 then cls <> " bg-red-100" else cls <> " bg-green-100"
          span_ [class_ status_cls] $ show $ req ^. #statusCode
          span_ [class_ $ cls <> " bg-gray-50"] $ toHtml $ req ^. #rawUrl
          let reqBody = decodeUtf8 $ AE.encode $ req ^. #requestBody
          let respBody = decodeUtf8 $ AE.encode $ req ^. #responseBody
          let reqHeaders = decodeUtf8 $ AE.encode $ req ^. #requestHeaders
          let respHeaders = decodeUtf8 $ AE.encode $ req ^. #responseHeaders
          p_ [class_ "inline-block"] $ toHtml $ T.take 300 [text| request_body=$reqBody response_body=$respBody request_headers=$reqHeaders response_headers=$respHeaders|]

    div_ [class_ "hidden w-full flex px-2 py-8 justify-center item-loading"] do
      loader
  when (Vector.length requests > 199) $ a_
    [ class_ "cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center"
    , hxTrigger_ "click"
    , hxSwap_ "outerHTML"
    , hxGet_ nextLogsURL
    ]
    do
      div_ [class_ "htmx-indicator query-indicator"] do
        loader
      "LOAD MORE"


apiLogItemView :: RequestDumps.RequestDumpLogItem -> Text -> Html ()
apiLogItemView req expandItemPath = do
  div_ [class_ "log-item-info border-l-blue-200 border-l-4"]
    $ div_ [class_ "pl-4 py-1 ", colspan_ "3"]
    $ do
      button_
        [ class_ "px-2 rounded text-white bg-blue-500 text-sm font-semibold"
        , term "data-log-item-path" (expandItemPath <> "/detailed")
        , [__|on click remove .hidden from #expand-log-modal then
                remove .hidden from #log-modal-content-loader
                fetch `${@data-log-item-path}` as html then put it into #log-modal-content
                add .hidden to #log-modal-content-loader
                end
          |]
        ]
        "expand"
      jsonValueToHtmlTree $ AE.toJSON req


-- | jsonValueToHtmlTree takes an aeson json object and renders it as a collapsible html tree, with hyperscript for interactivity.
jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = jsonValueToHtmlTree' ("", "", val)
  where
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (AEK.toHashMapText v & HM.toList & sort & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key <> "." <> "[]", show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf ".[]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      div_
        [ class_ "relative log-item-field-parent"
        , term "data-field-path" fullFieldPath'
        ]
        $ a_ [class_ "block hover:bg-blue-50 cursor-pointer pl-6 relative log-item-field-anchor ", [__|install LogItemMenuable|]]
        $ do
          span_ $ toHtml key
          span_ [class_ "text-blue-800"] ":"
          span_ [class_ "text-blue-800 ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml $ unwrapJsonPrimValue value

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ (if key == "" then "" else "collapsed")] $ do
      a_
        [ class_ "inline-block cursor-pointer"
        , onclick_ "this.parentNode.classList.toggle('collapsed')"
        ]
        $ do
          span_ [class_ "log-item-tree-chevron "] "▾"
          span_ [] $ toHtml $ if key == "" then opening else key <> ": " <> opening
      div_ [class_ "pl-5 children "] $ do
        span_ [class_ "tree-children-count"] $ show count
        div_ [class_ "tree-children"] child
      span_ [class_ "pl-5 closing-token"] $ toHtml closing


-- >>> findValueByKeyInJSON ["key1", "key2"] [aesonQQ|{"kx":0, "key1":{"key2":"k2val"}}|]
-- ["\"k2val\""]
-- >>> findValueByKeyInJSON ["key1", "[]", "key2"] [aesonQQ|{"kx":0, "key1":[{"key2":"k2val"}]}|]
-- ["\"k2val\""]
findValueByKeyInJSON :: [Text] -> AE.Value -> [Text]
findValueByKeyInJSON (x : path) (AE.Object obj) = concatMap (\(_, v) -> findValueByKeyInJSON path v) (AEK.toHashMapText obj & HM.toList & filter (\(k, _) -> k == x))
findValueByKeyInJSON ("[]" : path) (AE.Array vals) = concatMap (findValueByKeyInJSON path) (Vector.toList vals)
findValueByKeyInJSON [] value = [unwrapJsonPrimValue value]
findValueByKeyInJSON _ _ = ["_"]


-- findValueByKeyInJSON _ _ = error "findValueByKeyInJSON: case should be unreachable"

-- TODO:
jsonTreeAuxillaryCode :: Projects.ProjectId -> Html ()
jsonTreeAuxillaryCode pid = do
  template_ [id_ "log-item-context-menu-tmpl"] $ do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu text-sm origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] $ do
      div_ [class_ "py-1", role_ "none"] $ do
        a_
          [ class_ "cursor-pointer text-gray-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-gray-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-0"
          , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer"
          , hxPushUrl_ "true"
          , hxVals_ "js:{query:params().query,cols:toggleColumnToSummary(event)}"
          , hxSwap_ "innerHTML scroll:#log-item-table-body:top"
          , hxTarget_ "#log-item-table-body"
          , hxIndicator_ "#query-indicator"
          , [__|init
                  set fp to (closest @data-field-path)
                  if isFieldInSummary(fp) then set my innerHTML to 'Remove field from summary' end|]
          ]
          "Add field to Summary"
        a_
          [ class_ "cursor-pointer text-gray-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-gray-900"
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
          [ class_ "cursor-pointer w-full text-left text-gray-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-gray-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-2"
          , [__|on click 
                    set filter_path to (previous .log-item-field-value) @data-field-path
                    set filter_value to (previous  <.log-item-field-value/>)'s innerText
                    if window.editor.getValue().includes(filter_path) and window.editor.getValue().includes(filter_value)
                       exit
                    end
                    if window.editor.getValue().toLowerCase().endsWith("and") or window.editor.getValue().toLowerCase().endsWith("or") then
                       window.editor.setValue(window.editor.getValue() + " " + filter_path + "=" + filter_value)
                      else 
                        if window.editor.getValue().length == 0 then
                           window.editor.setValue (filter_path + "=" + filter_value)
                        else
                          window.editor.setValue (window.editor.getValue() + " AND " + filter_path + "=" + filter_value)
                        end
                    end
                    if window.editor.getValue() != "" then
                      htmx.trigger("#log_explorer_form", "submit")
                    end
                  end|]
          ]
          "Filter by field"

  script_
    [type_ "text/hyperscript"]
    [text|
      behavior LogItemMenuable
        on click
          if I match <.with-context-menu/> then
            remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu />
          else
            remove <.log-item-context-menu /> then remove .with-context-menu from <.with-context-menu /> then
            get #log-item-context-menu-tmpl.innerHTML then put it after me then add .with-context-menu to me then 
            _hyperscript.processNode(document.querySelector('.log-item-context-menu'))
            htmx.process(document.querySelector('.log-item-context-menu'))
          end
          halt
        end
      end

      behavior LogItemExpandable
        on click 
          if I match <.expanded-log/> then 
            remove next <.log-item-info/> then 
            remove .expanded-log from me
          else
            add .expanded-log to me
            remove .hidden from next <.item-loading />
            fetch `$${@data-log-item-path}` as html then put it after me then
             add .hidden to next <.item-loading />
            _hyperscript.processNode(next <.log-item-info />) then
          end 
      end
    |]

  script_ [src_ "/assets/js/monaco/vs/loader.js", defer_ "true"] ("" :: Text)
  script_
    [text|
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
      const new_cols = [... new Set (cols)].join (",")
      return new_cols
    }
    var isFieldInSummary = field => params().cols.split(",").includes(field);
    var getQueryFromEditor = () => window.editor.getValue();

    var execd = false
    document.addEventListener('DOMContentLoaded', function(){
      // Configuration for the monaco editor which the query editor is built on.
      require.config({ paths: { vs: '/assets/js/monaco/vs' } });
			require(['vs/editor/editor.main'], function () {
        monaco.editor.defineTheme('apitoolkit', {
          base: 'vs',
          inherit: true,
          rules: [{ background: 'EDF9FA' }],
          colors: {
            'editor.foreground': '#000000',
            'editor.background': '#f5f5f5',
            'editorGutter.background': '#e8e8e8',
          }
        });
        monaco.editor.setTheme('apitoolkit');
				window.editor = monaco.editor.create(document.getElementById('queryEditor'), {
          value: params().query,
					language:'hcl',
          minimap:{enabled:false}
				});
			});
      // Monaco code suggestions https://github.com/microsoft/monaco-editor/issues/1850
    })
    |]

  style_
    [text|
    .tree-children {
      display: block;
    }
    .tree-children-count { display: none; }
    .collapsed .tree-children {
      display: none !important; 
    }
    .collapsed .tree-children-count {display: inline !important;}
    .collapsed .children {display: inline-block; padding-left:0}
    .collapsed .closing-token {padding-left:0}
  |]


unwrapJsonPrimValue :: AE.Value -> Text
unwrapJsonPrimValue (AE.Bool True) = "true"
unwrapJsonPrimValue (AE.Bool False) = "true"
unwrapJsonPrimValue (AE.String v) = "\"" <> toText v <> "\""
unwrapJsonPrimValue (AE.Number v) = toText @String $ show v
unwrapJsonPrimValue AE.Null = "null"
unwrapJsonPrimValue (AE.Object _) = "{..}"
unwrapJsonPrimValue (AE.Array items) = "[" <> show (length items) <> "]"

-- unwrapJsonPrimValue (AE.Object _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only. got object" -- should never be reached
-- unwrapJsonPrimValue (AE.Array _) = error "Impossible. unwrapJsonPrimValue should be for primitive types only. got array" -- should never be reached
