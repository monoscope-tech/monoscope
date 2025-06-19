module Pages.LogExplorer.LogItem (expandAPIlogItemH, expandAPIlogItem', ApiItemDetailed (..)) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Lazy qualified as BS
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Pages.Components (dateTime, statBox_)
import Pages.Telemetry.Spans qualified as Spans
import Pages.Telemetry.Utils (atMapText, getRequestDetails)
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getDurationNSMS, getMethodBorderColor, getMethodColor, getSeverityColor, getStatusBorderColor, getStatusColor, jsonValueToHtmlTree, onpointerdown_)


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ApiItemDetailed)
expandAPIlogItemH pid rdId createdAt sourceM = do
  _ <- Sessions.sessionAndProject pid
  let source = fromMaybe "requets" sourceM
  case source of
    "logs" -> do
      logItem <- Telemetry.logRecordByProjectAndId pid createdAt rdId
      addRespHeaders $ case logItem of
        Just lg -> LogItemExpanded pid lg
        Nothing -> ItemDetailedNotFound "Log not found"
    _ -> do
      spanItem <- Telemetry.spanRecordByProjectAndId pid createdAt rdId
      case spanItem of
        Just spn -> do
          aptSpan <- case getRequestDetails spn.attributes of
            Just ("HTTP", _, _, _) -> do
              let trIdM = spn.context >>= (.trace_id)
              if spn.name /= Just "apitoolkit-http-span"
                then do
                  case trIdM of
                    Just trId -> do
                      Telemetry.spanRecordByName pid trId "apitoolkit-http-span"
                    _ -> pure Nothing
                else pure Nothing
            _ -> pure Nothing
          addRespHeaders $ SpanItemExpanded pid spn aptSpan
        Nothing -> addRespHeaders $ ItemDetailedNotFound "Span not found"


expandAPIlogItem' :: Projects.ProjectId -> RequestDumps.RequestDumpLogItem -> Bool -> Html ()
expandAPIlogItem' pid req modal = do
  div_ [class_ "relative flex flex-col w-full px-4 gap-4 pb-[100px]"] do
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
    div_ [class_ "flex justify-between items-center", id_ "copy_share_link"] pass
    div_ [class_ "w-full flex flex-col gap-4"] do
      let methodColor = getMethodColor req.method
          statusColor = getStatusColor req.statusCode
          borderColor = getMethodBorderColor req.method
          stBorder = getStatusBorderColor req.statusCode

      div_ [class_ "flex  justify-between items-center gap-4"] do
        div_ [class_ "flex items-center gap-4"] do
          span_ [class_ $ "flex items-center rounded-lg px-2 py-1 border font-medium gap-2 " <> borderColor <> " " <> methodColor] $ toHtml req.method
          span_ [class_ $ "flex items-center rounded-lg px-2 py-1 border font-medium gap-2 " <> stBorder <> " " <> statusColor] $ toHtml $ show req.statusCode

        div_ [class_ "flex items-center gap-2"] do
          dateTime (zonedTimeToUTC req.createdAt) Nothing
          button_
            [ class_ "ml-4 p-0 -mt-1 cursor-pointer"
            , [__|on click add .hidden to #trace_expanded_view 
            then put '0px' into  #log_details_container.style.width 
            then put '100%' into #logs_list_container.style.width 
            then add .hidden to #resizer-details_width
            then call updateUrlState('details_width', '', 'delete')
            then call updateUrlState('target_event', '0px', 'delete')
            |]
            ]
            do
              faSprite_ "xmark" "regular" "w-3 h-3 text-textBrand"
    -- url, endpoint, latency, request size, repsonse size
    let path = toText $ escapeURIString isUnescapedInURI $ "url_path==\"" <> toString req.urlPath <> "\""
        query = toText $ escapeURIString isUnescapedInURI $ "raw_url==\"" <> toString req.rawUrl <> "\""
        rawUrl = "/p/" <> pid.toText <> "/log_explorer?query=" <> query
        urlPath = "/p/" <> pid.toText <> "/log_explorer?query=" <> path
    div_ [class_ "flex flex-col mt-4 justify-between w-full"] do
      div_ [class_ "text-base mb-2 flex gap-6 items-center"] do
        span_ [class_ "text-slate-500 font-medium w-16"] "Endpoint"
        div_ [class_ "flex gap-1 items-center"] do
          span_ [class_ "text-slate-800 text-sm truncate ellipsis urlPath", term "data-tippy" req.urlPath] $ toHtml req.urlPath
          div_ [[__| install Copy(content:.urlPath )|]] do
            faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-fillWeaker rounded-full p-2 text-slate-500"
          a_ [href_ urlPath] do
            faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 btn-primary rounded-full"
      div_ [class_ "text-base flex items-center gap-6"] do
        span_ [class_ "text-slate-500 font-medium w-16"] "URL"
        div_ [class_ "flex gap-1 items-center"] do
          span_ [class_ "text-slate-800 text-sm truncate ellipsis", term "data-tippy" req.rawUrl] $ toHtml req.rawUrl
          div_ [[__| install Copy(content:.urlPath )|]] do
            faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-fillWeaker rounded-full p-2 text-slate-500"
          a_ [href_ rawUrl] do
            faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 btn-primary rounded-full"
      div_ [class_ "flex gap-2 mt-4"] do
        statBox_ Nothing (Just ("clock", "regular", "text-brand")) "Latency" "Latency" (toText $ getDurationNSMS req.durationNs) Nothing Nothing
        let reqSize = BS.length $ AE.encode req.requestBody
        statBox_ Nothing (Just ("upload", "regular", "text-brand")) "Request size" "Total request body size in bytes" (show (reqSize - 2)) Nothing Nothing
        let respSize = BS.length $ AE.encode req.responseBody
        statBox_ Nothing (Just ("download", "regular", "text-brand")) "Response size" "Total response body size in bytes" (show (respSize - 2)) Nothing Nothing
        statBox_ Nothing (Just ("stack", "regular", "text-brand")) "Framework" "Framework used to handle this the request" (show req.sdkType) Nothing Nothing

    -- errors
    when (req.errorsCount > 0) $ div_ [class_ "mt-4"] do
      div_ [class_ "flex w-full text-slate-950 font-medium gap-2 items-center"] do
        p_ "Errors"
        p_ [class_ " text-red-500 font-bold"] $ show req.errorsCount
      div_ [class_ "p-4 rounded-lg border border-slate-200 text-gray-500"] do
        jsonValueToHtmlTree req.errors Nothing

    div_ [id_ "http-content-container", class_ "flex flex-col gap-3"] do
      let json = selectiveReqToJson req
      div_ [class_ "flex items-center gap-2"] do
        button_
          [ class_ "flex items-center gap-1 text-sm text-textBrand cursor-pointer"
          , onpointerdown_ "window.buildCurlRequest(event)"
          , term "data-reqjson" $ decodeUtf8 $ AE.encode json
          ]
          do
            span_ [class_ "underline"] "Copy request as curl"
            faSprite_ "copy" "regular" "w-2 h-2"
        let createdAt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" req.createdAt
        button_
          [ class_ "flex items-center gap-2 text-textBrand text-sm underline cursor-pointer"
          , hxPost_ $ "/p/" <> pid.toText <> "/share/" <> UUID.toText req.id <> "/" <> createdAt <> "?event_type=request"
          , hxSwap_ "innerHTML"
          , hxTarget_ "#copy_share_link"
          ]
          do
            "Get shareable link"
            faSprite_ "link-simple" "regular" "w-3 h-3"

      div_ [class_ "bg-fillWeak w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"] $ do
        div_ [class_ "justify-start items-start flex text-sm"] $ do
          button_ [onpointerdown_ "navigatable(this, '#raw_content', '#http-content-container', 't-tab-box-active')", class_ "cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak t-tab-box-active"] "Raw Details"
          button_ [onpointerdown_ "navigatable(this, '#req_content', '#http-content-container', 't-tab-box-active')", class_ "cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Req Body"
          button_ [onpointerdown_ "navigatable(this, '#res_content', '#http-content-container', 't-tab-box-active')", class_ "cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Res Body"
          button_ [onpointerdown_ "navigatable(this, '#hed_content', '#http-content-container', 't-tab-box-active')", class_ "cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Headers"
          button_ [onpointerdown_ "navigatable(this, '#par_content', '#http-content-container', 't-tab-box-active')", class_ "cursor-pointer a-tab px-3 py-1 rounded-lg text-textWeak"] "Params"
      div_ [] do
        div_ [id_ "raw_content", class_ "a-tab-content"] do
          jsonValueToHtmlTree json Nothing
        div_ [id_ "req_content", class_ "hidden a-tab-content"] do
          jsonValueToHtmlTree req.requestBody Nothing
        div_ [id_ "res_content", class_ "hidden a-tab-content"] do
          jsonValueToHtmlTree req.responseBody Nothing
        div_ [id_ "hed_content", class_ "hidden a-tab-content"] do
          jsonValueToHtmlTree (AE.object ["request_headers" AE..= req.requestHeaders, "response_headers" AE..= req.responseHeaders]) Nothing
        div_ [id_ "par_content", class_ "hidden a-tab-content"] do
          jsonValueToHtmlTree (AE.object ["query_params" AE..= req.queryParams, "path_params" AE..= req.pathParams]) Nothing


-- outgoing request details
-- div_ [class_ "flex w-full flex-col gap-1"] do
--   p_ [class_ "font-medium text-slate-950 mb-2"] "Outgoing requests"
--   div_ [class_ "grow rounded-lg border border-slate-200 overflow-y-auto py-2 px-1 h-[150px] whitespace-nowrap  divide-y overflow-x-hidden"] do
--     let createdAt = toText $ formatTime defaultTimeLocale "%FT%T%6QZ" req.createdAt
--         escapedQueryPartial = toText $ escapeURIString isUnescapedInURI $ toString $ "parent_id==\"" <> UUID.toText req.id <> "\" AND " <> "created_at>=\"" <> createdAt <> "\""
--         events_url = "/p/" <> pid.toText <> "/log_explorer?layout=virtualTable&query=" <> escapedQueryPartial
--     div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""

data ApiItemDetailed
  = SpanItemExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans)
  | LogItemExpanded Projects.ProjectId Telemetry.OtelLogsAndSpans
  | ItemDetailedNotFound Text


instance ToHtml ApiItemDetailed where
  toHtml (SpanItemExpanded pid spn aptSpan) = toHtml $ Spans.expandedSpanItem pid spn aptSpan Nothing Nothing
  toHtml (LogItemExpanded pid req) = toHtml $ apiLogItemView pid req
  toHtml (ItemDetailedNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


apiLogItemView :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Html ()
apiLogItemView pid lg = do
  div_ [class_ "w-full flex flex-col gap-2 px-2 pb-2 relative"] $ do
    div_ [class_ "flex justify-between items-center", id_ "copy_share_link"] pass
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
    div_ [class_ "flex flex-col gap-4 bg-gray-50 py-2  px-2"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "whitespace-nowrap font-semibold text-textStrong"] "Trace Log"
        div_ [class_ "flex gap-4 items-center"] $ do
          dateTime lg.timestamp Nothing
          div_ [class_ "flex gap-2 items-center"] do
            button_
              [ class_ "cursor-pointer"
              , [__|on click add .hidden to #trace_expanded_view 
            then put '0px' into  #log_details_container.style.width 
            then put '100%' into #logs_list_container.style.width 
            then add .hidden to #resizer-details_width
            then remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
            then call updateUrlState('details_width', '', 'delete')
            then call updateUrlState('target_event', '0px', 'delete')
            then call updateUrlState('showTrace', '', 'delete')
            |]
              ]
              do
                faSprite_ "xmark" "regular" "w-3 h-3 text-textBrand"
    div_ [class_ "flex flex-col gap-4"] do
      div_ [class_ "flex items-center gap-4"] do
        let svTxt = maybe "UNSET" (\x -> maybe "UNSET" show x.severity_text) lg.severity
            cls = getSeverityColor svTxt
        span_ [class_ $ "rounded-lg border cbadge-sm text-sm px-2 py-1 shrink-0 " <> cls] $ toHtml $ T.toUpper svTxt
        h4_ [class_ "text-slate-800 font-medium"] $ toHtml $ case lg.body of
          Just (AE.String x) -> x
          _ -> toStrict $ encodeToLazyText lg.body

      div_ [class_ "flex gap-2 flex-wrap"] $ do
        spanBadge (fromMaybe "" $ atMapText "service.name" lg.resource) "Service"
        spanBadge ("Spand ID: " <> maybe "" (\z -> fromMaybe "" z.span_id) lg.context) "Span ID"
        spanBadge ("Trace ID: " <> maybe "" (\z -> fromMaybe "" z.trace_id) lg.context) "Trace ID"

      div_ [class_ "flex gap-2 items-center text-textBrand font-medium text-xs"] do
        whenJust lg.context $ \ctx -> do
          whenJust ctx.trace_id $ \trId -> do
            let tracePath = "/p/" <> pid.toText <> "/traces/" <> trId <> "/"
            button_
              [ class_ "flex items-end gap-1 cursor-pointer"
              , term
                  "_"
                  [text|on click remove .hidden from #trace_expanded_view
                            then call updateUrlState('showTrace', "$trId")
                            then set #trace_expanded_view.innerHTML to #loader-tmp.innerHTML
                            then fetch $tracePath
                            then set #trace_expanded_view.innerHTML to it
                            then htmx.process(#trace_expanded_view)
                            then _hyperscript.processNode(#trace_expanded_view) then window.evalScriptsFromContent(#trace_expanded_view)|]
              ]
              do
                "View parent trace"
                faSprite_ "cross-hair" "regular" "w-4 h-4"
        let lg_id = UUID.toText lg.id
        let createdAt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" lg.timestamp
        button_
          [ class_ "flex items-center gap-2 cursor-pointer"
          , hxPost_ $ "/p/" <> pid.toText <> "/share/" <> lg_id <> "/" <> createdAt <> "?event_type=log"
          , hxSwap_ "innerHTML"
          , hxTarget_ "#copy_share_link"
          ]
          do
            "Generate shareable link"
            faSprite_ "link-simple" "regular" "w-3 h-3"

      div_ [class_ "w-full mt-4", id_ "log-tabs-container"] do
        div_ [class_ "flex", [__|on click halt|]] $ do
          button_ [class_ "cursor-pointer a-tab border-b-2 border-b-slate-200 px-4 py-1.5 t-tab-active", onpointerdown_ "navigatable(this, '#att-content', '#log-tabs-container', 't-tab-active')"] "Attributes"
          button_ [class_ "cursor-pointer a-tab border-b-2 border-b-slate-200 px-4 py-1.5 ", onpointerdown_ "navigatable(this, '#meta-content', '#log-tabs-container', 't-tab-active')"] "Process"
          div_ [class_ "w-full border-b-2 border-b-slate-200"] pass

        div_ [class_ "grid my-4 text-slate-600 font"] $ do
          div_ [class_ "a-tab-content", id_ "att-content"] $ do
            jsonValueToHtmlTree (maybe (AE.object []) (AE.Object . KEM.fromMapText) lg.attributes) Nothing
          div_ [class_ "hidden a-tab-content", id_ "meta-content"] $ do
            jsonValueToHtmlTree (maybe (AE.object []) (AE.Object . KEM.fromMapText) lg.resource) Nothing


-- div_ [class_ "px-2 flex flex-col w-full items-center gap-2"] do
--   span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
--   jsonValueToHtmlTree req

spanBadge :: Text -> Text -> Html ()
spanBadge val key = do
  div_
    [ class_ "flex gap-2 items-center text-textStrong bg-fillWeak border border-strokeWeak text-xs rounded-lg whitespace-nowrap px-2 py-1"
    , term "data-tippy-content" key
    ]
    $ do
      span_ [] $ toHtml val


-- Function to selectively convert RequestDumpLogItem to JSON
selectiveReqToJson :: RequestDumps.RequestDumpLogItem -> AE.Value
selectiveReqToJson req =
  AE.object
    $ concat @[]
      [ ["created_at" AE..= req.createdAt]
      , ["duration_ns" AE..= req.durationNs]
      , ["errors" AE..= req.errors]
      , ["host" AE..= req.host]
      , ["method" AE..= req.method]
      , ["parent_id" AE..= req.parentId]
      , ["path_params" AE..= req.pathParams]
      , ["query_params" AE..= req.queryParams]
      , ["raw_url" AE..= req.rawUrl]
      , ["referer" AE..= req.referer]
      , ["request_body" AE..= req.requestBody]
      , ["request_headers" AE..= req.requestHeaders]
      , ["request_type" AE..= req.requestType]
      , ["response_body" AE..= req.responseBody]
      , ["response_headers" AE..= req.responseHeaders]
      , ["sdk_type" AE..= req.sdkType]
      , ["service_version" AE..= req.serviceVersion]
      , ["status_code" AE..= req.statusCode]
      , ["tags" AE..= req.tags]
      , ["url_path" AE..= req.urlPath]
      ]
