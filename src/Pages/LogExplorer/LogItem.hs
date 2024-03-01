module Pages.LogExplorer.LogItem where

import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.ByteString.Lazy qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format
import Data.UUID qualified as UUID
import Data.Vector (iforM_)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Pages.Components qualified as Components
import Pages.NonMember
import PyF
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import System.Config
import System.Types
import Utils


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> ATAuthCtx (Html ())
expandAPIlogItemH pid rdId createdAt = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession

  logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
  let content = case logItemM of
        Just req -> expandAPIlogItem' pid req True
        Nothing -> div_ [class_ "h-full flex flex-col justify-center items-center"] do
          p_ [] "Request not found"
  pure content


expandAPIlogItem' :: Projects.ProjectId -> RequestDumps.RequestDumpLogItem -> Bool -> Html ()
expandAPIlogItem' pid req modal = do
  div_ [class_ "flex flex-col w-full gap-4 pb-[100px]"] do
    div_ [class_ "w-full flex flex-col gap-2 gap-4"] do
      let methodColor = getMethodColor req.method
      let statusColor = getStatusColor req.statusCode
      div_ [class_ "flex gap-4 items-center"] do
        div_ [class_ $ "font-semibold px-2 py-1 rounded min-w-[70px] text-center h-full " <> methodColor] $ toHtml req.method
        div_ [class_ $ "text-lg font-bold px-2 " <> statusColor] $ show req.statusCode
        div_ [class_ "flex border border-gray-200 m-1 rounded-xl p-2"] do
          mIcon_ "calender" "h-4 mr-2 w-4"
          span_ [class_ "text-xs"] $ toHtml $ formatTime defaultTimeLocale "%b %d, %Y %R" req.createdAt
      when modal do
        div_
          [ class_ "flex gap-2 px-4 items-center border border-dashed h-[50px]"
          , id_ "copy_share_link"
          ]
          do
            div_ [class_ "relative", style_ "width:150px", onblur_ "document.getElementById('expire_container').classList.add('hidden')"] do
              button_
                [ onclick_ "toggleExpireOptions(event)"
                , id_ "toggle_expires_btn"
                , class_ "w-full flex gap-2 text-slate-600 justify_between items-center cursor-pointer px-2 py-1 border rounded focus:ring-2 focus:ring-blue-200 active:ring-2 active:ring-blue-200"
                ]
                do
                  p_ [style_ "width: calc(100% - 25px)", class_ "text-sm truncate ..."] "Expires in: 1 hour"
                  faIcon_ "fa-chevron-down" "fa-light fa-chevron-down" "h-3 w-3"
              div_ [id_ "expire_container", class_ "absolute hidden bg-white border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] do
                forM_ (["1 hour", "8 hours", "1 day"] :: [Text]) \sw -> do
                  button_
                    [ onclick_ "expireChanged(event)"
                    , term "data-expire-value" sw
                    , class_ "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
                    ]
                    $ toHtml sw
            button_
              [ class_ "flex flex-col gap-1 bg-blue-500 px-2 py-1 rounded text-white"
              , term "data-req-id" (show req.id)
              , [__|on click set #req_id_input.value to my @data-req-id then call #share_log_form.requestSubmit() |]
              ]
              "Get link"

    -- url, endpoint, latency, request size, repsonse size
    div_ [class_ "flex flex-col mt-4 p-4 justify-between w-full rounded-xl border"] do
      div_ [class_ "text-lg mb-2"] do
        span_ [class_ "text-slate-500 font-semibold"] "Endpoint: "
        span_ [] $ toHtml req.urlPath
      div_ [class_ "text-lg"] do
        span_ [class_ "text-slate-500 font-semibold"] "URL: "
        span_ [] $ toHtml req.rawUrl
      div_ [class_ "flex gap-2 mt-4"] do
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "clock" "h-4 w-4 text-slate-400"
            span_ [class_ "text-md font-bold"] $ show (req.durationNs `div` 1000) <> " ms"
          p_ [class_ "text-slate-500"] "Latency"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "upload" "h-4 w-4 text-slate-400"
            let reqSize = BS.length $ AE.encode req.requestBody
            span_ [class_ "text-md font-bold"] $ show (reqSize - 2) <> " bytes"
          p_ [class_ "text-slate-500"] "Request size"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "download4" "h-4 w-4 text-slate-400"
            let respSize = BS.length $ AE.encode req.responseBody
            span_ [class_ "text-md font-bold"] $ show (respSize - 2) <> " bytes"
          p_ [class_ "text-slate-500"] "Response size"
        div_ [class_ "flex flex-col gap-1 px-4 min-w-[120px] py-3 border border-dashed border-gray-400 m-1 rounded"] do
          div_ [class_ "flex gap-1 items-center"] do
            mIcon_ "projects" "h-5 w-5 text-slate-400"
            span_ [class_ "text-md font-bold"] $ show req.sdkType
          p_ [class_ "text-slate-500"] "Framework"
    -- errors
    when (req.errorsCount > 0) $ div_ [class_ "border rounded-lg mt-8"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 gap-4 items-center"] do
        p_ [class_ "font-bold"] "Errors"
        p_ [class_ "text-sm text-red-500 font-bold"] $ show req.errorsCount
      div_ [class_ "px-4 flex gap-10 border-b text-gray-500"] do
        jsonValueToHtmlTree req.errors

    -- outgoing request details
    div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
      p_ [class_ "font-bold"] "Outgoing requests"
    div_ [class_ "grow overflow-y-auto py-2 px-1 max-h-[500px] whitespace-nowrap text-sm divide-y overflow-x-hidden"] do
      let escapedQueryPartial = toText $ escapeURIString isUnescapedInURI $ toString $ [fmt|parent_id=="{UUID.toText req.id}"|]
      let events_url = "/p/" <> pid.toText <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial
      div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""

    -- request details
    div_ [class_ "border rounded-lg mt-8", id_ "request_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Request Details"

      div_ [class_ "tabs tabs-bordered", role_ "tablist"] do
        input_ [type_ "radio", name_ "req-details-tab", role_ "tab", Aria.label_ "Body", class_ "tab w-max", checked_]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4  p-2 rounded-lg border break-all", id_ "req_body_json"]
          $ jsonValueToHtmlTree req.requestBody

        input_ [type_ "radio", name_ "req-details-tab", role_ "tab", Aria.label_ "Headers", class_ "tab"]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border break-all", id_ "req_headers_json"]
          $ jsonValueToHtmlTree req.requestHeaders

        input_ [type_ "radio", name_ "req-details-tab", role_ "tab", Aria.label_ "Query Params", class_ "tab break-keep"]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border", id_ "query_params_json"]
          $ jsonValueToHtmlTree req.queryParams

        input_ [type_ "radio", name_ "req-details-tab", role_ "tab", Aria.label_ "Path Params", class_ "tab break-keep"]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border", id_ "path_params_json"]
          $ jsonValueToHtmlTree req.pathParams

    -- response details
    div_ [class_ "border rounded-lg mt-8", id_ "reponse_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Response Details"

      div_ [class_ "tabs tabs-bordered", role_ "tablist"] do
        input_ [type_ "radio", name_ "resp-details-tab", role_ "tab", Aria.label_ "Body", class_ "tab", checked_]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4  p-2 rounded-lg border", id_ "res_body_json"]
          $ jsonValueToHtmlTree req.responseBody

        input_ [type_ "radio", name_ "resp-details-tab", role_ "tab", Aria.label_ "Headers", class_ "tab"]
        div_ [class_ "tab-content", role_ "tabpanel"]
          $ div_ [class_ "bg-gray-50 m-4 p-2 rounded-lg border", id_ "res_headers_json"]
          $ jsonValueToHtmlTree req.responseHeaders
  script_
    [text|
function toggleExpireOptions (event) {
    event.preventDefault()
    event.stopPropagation()
    const container = document.querySelector('#expire_container')
    if(container) {
     container.classList.toggle('hidden')
    }
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


apiLogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> ATAuthCtx (Html ())
apiLogItemH pid rdId createdAt = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      let content = case logItemM of
            Just req -> apiLogItemView req (RequestDumps.requestDumpLogItemUrlPath pid req)
            Nothing -> div_ "invalid log request ID"
      pure content


apiLogItemView :: RequestDumps.RequestDumpLogItem -> Text -> Html ()
apiLogItemView req expandItemPath = do
  div_ [class_ "flex items-center gap-2"] do
    Components.drawerWithURLContent_
      ("expand-log-drawer-" <> UUID.toText req.id)
      (expandItemPath <> "/detailed")
      $ span_ [class_ "btn btn-sm btn-outline"] ("Expand" >> faIcon_ "fa-expand" "fa-regular fa-expand" "h-3 w-3")
    let reqJson = decodeUtf8 $ AE.encode $ AE.toJSON req
    button_
      [ class_ "btn btn-sm btn-outline"
      , term "data-reqJson" reqJson
      , [__|on click if 'clipboard' in window.navigator then
                  call navigator.clipboard.writeText(my @data-reqJson)
                  send successToast(value:['Request json has been copied to clipboard']) to <body/>
                end|]
      ]
      $ span_ [] "Copy"
      >> faIcon_ "fa-copy" "fa-regular fa-copy" "h-3 w-3"
    button_
      [ class_ "btn btn-sm btn-outline"
      , onclick_ "downloadJson(event)"
      , term "data-reqJson" reqJson
      ]
      $ span_ [] "Download"
      >> faIcon_ "fa-arrow-down-to-line" "fa-regular fa-arrow-down-to-line" "h-3 w-3"
  jsonValueToHtmlTree $ selectiveToJson req


-- Function to selectively convert RequestDumpLogItem to JSON
selectiveToJson :: RequestDumps.RequestDumpLogItem -> AE.Value
selectiveToJson req =
  AE.object
    $ concat
      [ ["created_at" .= req.createdAt]
      , ["duration_ns" .= req.durationNs]
      , ["errors" .= req.errors]
      , ["host" .= req.host]
      , ["method" .= req.method]
      , ["parent_id" .= req.parentId]
      , ["path_params" .= req.pathParams]
      , ["query_params" .= req.queryParams]
      , ["raw_url" .= req.rawUrl]
      , ["referer" .= req.referer]
      , ["request_body" .= req.requestBody]
      , ["request_headers" .= req.requestHeaders]
      , ["request_type" .= req.requestType]
      , ["response_body" .= req.responseBody]
      , ["response_headers" .= req.responseHeaders]
      , ["sdk_type" .= req.sdkType]
      , ["service_version" .= req.serviceVersion]
      , ["status_code" .= req.statusCode]
      , ["tags" .= req.tags]
      , ["url_path" .= req.urlPath]
      ]


-- | jsonValueToHtmlTree takes an aeson json object and renders it as a collapsible html tree, with hyperscript for interactivity.
jsonValueToHtmlTree :: AE.Value -> Html ()
jsonValueToHtmlTree val = jsonValueToHtmlTree' ("", "", val)
  where
    jsonValueToHtmlTree' :: (Text, Text, AE.Value) -> Html ()
    jsonValueToHtmlTree' (path, key, AE.Object v) = renderParentType "{" "}" key (length v) (AEK.toHashMapText v & HM.toList & sort & mapM_ (\(kk, vv) -> jsonValueToHtmlTree' (path <> "." <> key, kk, vv)))
    jsonValueToHtmlTree' (path, key, AE.Array v) = renderParentType "[" "]" key (length v) (iforM_ v \i item -> jsonValueToHtmlTree' (path <> "." <> key <> "[*]", show i, item))
    jsonValueToHtmlTree' (path, key, value) = do
      let fullFieldPath = if T.isSuffixOf "[*]" path then path else path <> "." <> key
      let fullFieldPath' = fromMaybe fullFieldPath $ T.stripPrefix ".." fullFieldPath
      div_
        [ class_ "relative log-item-field-parent"
        , term "data-field-path" fullFieldPath'
        , term "data-field-value" $ unwrapJsonPrimValue value
        ]
        $ a_
          [class_ "block hover:bg-blue-50 cursor-pointer pl-6 relative log-item-field-anchor ", [__|install LogItemMenuable|]]
          do
            span_ $ toHtml key
            span_ [class_ "text-blue-800"] ":"
            span_ [class_ "text-blue-800 ml-2.5 log-item-field-value", term "data-field-path" fullFieldPath'] $ toHtml $ unwrapJsonPrimValue value

    renderParentType :: Text -> Text -> Text -> Int -> Html () -> Html ()
    renderParentType opening closing key count child = div_ [class_ (if key == "" then "" else "collapsed")] do
      a_
        [ class_ "inline-block cursor-pointer"
        , onclick_ "this.parentNode.classList.toggle('collapsed')"
        ]
        do
          span_ [class_ "log-item-tree-chevron "] "▾"
          span_ [] $ toHtml $ if key == "" then opening else key <> ": " <> opening
      div_ [class_ "pl-5 children "] do
        span_ [class_ "tree-children-count"] $ show count
        div_ [class_ "tree-children"] child
      span_ [class_ "pl-5 closing-token"] $ toHtml closing
