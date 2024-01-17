module Pages.LogExplorer.LogItem where

import Lucid
import Lucid.Htmx
import Models.Projects.Projects qualified as Projects
import Config
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.ByteString.Lazy qualified as BS
import Data.Default (def)
import Data.Digest.XXHash
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (
  UTCTime,
  ZonedTime,
  addUTCTime,
  getCurrentTime,
  secondsToNominalDiffTime,
  utc,
  utcToZonedTime,
 )
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.UUID qualified as UUID
import Data.Vector (Vector, iforM_)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Base
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Lucid.Svg (use_)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Numeric (showHex)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Pages.NonMember
import Pkg.Components (loader)
import Relude

import Data.Time.Clock (UTCTime)
import Data.Time.Format
import System.Clock
import Utils
import Witch (from)



expandAPIlogItemH :: Sessions.PersistentSession -> Projects.ProjectId -> UUID.UUID -> ZonedTime -> DashboardM (Html ())
expandAPIlogItemH sess pid rdId createdAt = do
  pool <- asks pool
  startTime <- liftIO $ getTime Monotonic
  (logItemM, childRequests) <- liftIO $ withPool pool do
    logItem <- RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
    childRequets <- RequestDumps.selectRequestDumpByProjectAndParentId pid rdId
    pure (logItem, childRequets)
  let content = case logItemM of
        Just req -> expandAPIlogItem' pid req True childRequests
        Nothing -> div_ [class_ "h-full flex flex-col justify-center items-center"] do
          p_ [] "Request not found"
  pure content


expandAPIlogItem' :: Projects.ProjectId -> RequestDumps.RequestDumpLogItem -> Bool -> Vector RequestDumps.RequestDumpLogItem -> Html ()
expandAPIlogItem' pid req modal outgoingRequests = do
  div_ [class_ "flex flex-col w-full pb-[100px]"] do
    div_ [class_ "w-full flex flex-col gap-2 gap-4"] do
      let methodColor = getMethodColor req.method
      let statusColor = getStatusColor req.statusCode
      div_ [class_ "flex gap-4 items-center"] do
        div_ [class_ $ "font-semibold px-2 py-1 rounded min-w-[70px] text-center " <> methodColor] $ toHtml req.method
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
              , onclick_ "getShareLink(event)"
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
    unless (null outgoingRequests) $ div_ [class_ "border rounded-lg mt-8"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Outgoing requests"
      div_ [class_ "grow overflow-y-auto py-2 px-1 max-h-[500px] whitespace-nowrap text-sm divide-y overflow-x-hidden"] do
        logItemRows pid outgoingRequests [] ""

    -- request details
    div_ [class_ "border rounded-lg mt-8", id_ "request_detail_container"] do
      div_ [class_ "flex w-full bg-gray-100 px-4 py-2 flex-col gap-2"] do
        p_ [class_ "font-bold"] "Request Details"
      ul_ [class_ "px-4 flex gap-10 border-b text-slate-500"] do
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
      ul_ [class_ "px-4 flex gap-10 border-b text-slate-500"] do
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

      def LogItemExpandable(me)
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


apiLogItemH :: Sessions.PersistentSession -> Projects.ProjectId -> UUID.UUID -> ZonedTime -> DashboardM (Html ())
apiLogItemH sess pid rdId createdAt = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      logItemM <- liftIO $ withPool pool $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      let content = case logItemM of
            Just req -> apiLogItemView req (RequestDumps.requestDumpLogItemUrlPath pid req)
            Nothing -> div_ "invalid log request ID"
      pure content


apiLogItemView :: RequestDumps.RequestDumpLogItem -> Text -> Html ()
apiLogItemView req expandItemPath = do
  let errorClass = if req.errorsCount > 0 then "border-l-red-200" else "border-l-blue-200"
  div_ [class_ $ "log-item-info border-l-4 " <> errorClass]
    $ div_ [class_ "pl-4 py-1 ", colspan_ "3"] do
      div_ [class_ "flex items-center gap-2"] do
        button_
          [ class_ "px-4 rounded text-gray-600 border py-1 expand-button"
          , term "data-log-item-path" (expandItemPath <> "/detailed")
          , [__|on click remove .hidden from #expand-log-modal then
                   remove .hidden from #log-modal-content-loader
                   fetch `${@data-log-item-path}` as html then put it into #log-modal-content
                   add .hidden to #log-modal-content-loader
                   _hyperscript.processNode(document.querySelector('#log-modal-content'))
                   htmx.process(document.querySelector('#log-modal-content'))
                   end
             |]
          ]
          "Expand"
        let reqJson = decodeUtf8 $ AE.encode $ AE.toJSON req
        button_
          [ class_ "px-4 rounded flex items-center gap-1 text-gray-600 border py-1"
          , term "data-reqJson" reqJson
          , [__|on click if 'clipboard' in window.navigator then
                          call navigator.clipboard.writeText(my @data-reqJson)
                          send successToast(value:['Request json has been copied to clipboard']) to <body/>
                        end|]
          ]
          do
            span_ [] "Copy"
            faIcon_ "fa-copy" "fa-regular fa-copy" "h-4 w-4"
        button_
          [ class_ "px-4 flex items-center gap-1 rounded text-gray-600 border py-1"
          , onclick_ "downloadJson(event)"
          , term "data-reqJson" reqJson
          ]
          do
            span_ [] "Download"
            faIcon_ "fa-download" "fa-regular fa-download" "h-4 w-4"
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
