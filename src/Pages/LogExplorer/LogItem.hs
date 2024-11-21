module Pages.LogExplorer.LogItem (expandAPIlogItemH, expandAPIlogItem', apiLogItemH, ApiLogItem (..), ApiItemDetailed (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString.Lazy qualified as BS
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Data.UUID qualified as UUID
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Network.URI (escapeURIString, isUnescapedInURI)
import Pages.Components (statBox_)
import Pages.Traces.Spans qualified as Spans
import PyF (fmt)
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getDurationNSMS, getMethodBorderColor, getMethodColor, getStatusBorderColor, getStatusColor, jsonValueToHtmlTree, toXXHash)


expandAPIlogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ApiItemDetailed)
expandAPIlogItemH pid rdId createdAt sourceM = do
  _ <- Sessions.sessionAndProject pid
  let source = fromMaybe "requets" sourceM
  case source of
    "spans" -> do
      spanItem <- Telemetry.spanRecordByProjectAndId pid createdAt rdId
      addRespHeaders $ case spanItem of
        Just spn -> SpanItemExpanded pid spn
        Nothing -> ItemDetailedNotFound "Span not found"
    _ -> do
      logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      addRespHeaders $ case logItemM of
        Just req -> LogItemExpanded pid req True
        Nothing -> ItemDetailedNotFound "Request not found"


expandAPIlogItem' :: Projects.ProjectId -> RequestDumps.RequestDumpLogItem -> Bool -> Html ()
expandAPIlogItem' pid req modal = do
  div_ [class_ "flex flex-col w-full gap-4 pb-[100px]"] do
    div_ [class_ "w-full flex flex-col gap-4"] do
      let methodColor = getMethodColor req.method
          statusColor = getStatusColor req.statusCode
          borderColor = getMethodBorderColor req.method
          stBorder = getStatusBorderColor req.statusCode

      div_ [class_ "flex  justify-between items-center gap-4"] do
        div_ [class_ "flex items-center gap-4"] do
          span_ [class_ $ "flex items-center rounded-lg px-2 py-2 border font-medium gap-2 " <> borderColor <> " " <> methodColor] $ toHtml req.method
          span_ [class_ $ "flex items-center rounded-lg px-2 py-2 border font-medium gap-2 " <> stBorder <> " " <> statusColor] $ toHtml $ show req.statusCode
          span_ [class_ "flex items-center rounded-lg px-2 py-1 text-sm font-medium gap-2 border border-slate-300 bg-slate-100 text-slate-600"] do
            faSprite_ "calendar" "regular" "w-4 h-4 fill-none"
            toHtml $ formatTime defaultTimeLocale "%b. %d, %Y %I:%M:%S %p" req.createdAt

        div_ [class_ "flex items-center"] do
          when modal do
            div_
              [ class_ "flex gap-2 px-4 items-center"
              , id_ "copy_share_link"
              ]
              do
                p_ [class_ "text-slate-500 font-medium"] "Expires in: "
                div_ [class_ "relative w-max flex"] do
                  button_
                    [ [__|on click toggle .hidden on #expire_container|]
                    , id_ "toggle_expires_btn"
                    , class_ "btn px-0 flex w-[100px] nowrap justify-center gap-2 text-slate-600 font-medium items-center cursor-pointer border border-slate-300 bg-slate-100"
                    ]
                    do
                      span_ [] "1 hour"
                      faSprite_ "chevron-down" "regular" "h-3 w-3"
                  div_ [id_ "expire_container", class_ "absolute hidden bg-base-100 border shadow w-full overflow-y-auto", style_ "top:100%; max-height: 300px; z-index:9"] do
                    forM_ (["1 hour", "8 hours", "1 day"] :: [Text]) \sw -> do
                      button_
                        [ [__|on click set #toggle_expires_btn.firstElementChild.innerText to event.target's @data-expire-value
                                            then set #expire_input.value to event.target's @data-expire-value
                                            then add .hidden to #expire_container
                                            |]
                        , term "data-expire-value" sw
                        , class_ "p-2 w-full text-left truncate ... hover:bg-blue-100 hover:text-black"
                        ]
                        $ toHtml sw
                button_
                  [ class_ "btn blue-gr-btn"
                  , term "data-req-id" (show req.id)
                  , term "data-req-created-at" (toText $ formatTime defaultTimeLocale "%FT%T%6QZ" req.createdAt)
                  , [__|on click set #req_id_input.value to my @data-req-id
                              then set #req_created_at_input.value to my @data-req-created-at
                              then call #share_log_form.requestSubmit() |]
                  ]
                  "Get link"
    -- url, endpoint, latency, request size, repsonse size
    let endpointHash = toXXHash $ pid.toText <> req.host <> req.method <> req.urlPath
    let endpointURl = "/p/" <> pid.toText <> "/log_explorer/endpoint/" <> endpointHash
    div_ [class_ "flex flex-col mt-4 justify-between w-full"] do
      div_ [class_ "text-base mb-2 flex gap-6 items-center"] do
        span_ [class_ "text-slate-500 font-medium w-16"] "Endpoint"
        div_ [class_ "flex gap-1 items-center"] do
          span_ [class_ "text-slate-800 text-sm truncate ellipsis urlPath", term "data-tippy" req.urlPath] $ toHtml req.urlPath
          div_ [[__| install Copy(content:.urlPath )|]] do
            faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-slate-100 rounded-full p-2 text-slate-500"
          a_ [href_ endpointURl] do
            faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 blue-gr-btn rounded-full"
      div_ [class_ "text-base flex items-center gap-6"] do
        span_ [class_ "text-slate-500 font-medium w-16"] "URL"
        div_ [class_ "flex gap-1 items-center"] do
          span_ [class_ "text-slate-800 text-sm truncate ellipsis", term "data-tippy" req.rawUrl] $ toHtml req.rawUrl
          div_ [[__| install Copy(content:.urlPath )|]] do
            faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-slate-100 rounded-full p-2 text-slate-500"
          a_ [href_ endpointURl] do
            faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 blue-gr-btn rounded-full"
      div_ [class_ "flex gap-2 mt-4"] do
        statBox_ Nothing (Just ("clock", "regular", "text-blue-500")) "Latency" "Latency" (toText $ getDurationNSMS req.durationNs) Nothing Nothing
        let reqSize = BS.length $ AE.encode req.requestBody
        statBox_ Nothing (Just ("upload", "regular", "text-blue-500")) "Request size" "Total request body size in bytes" (show (reqSize - 2)) Nothing Nothing
        let respSize = BS.length $ AE.encode req.responseBody
        statBox_ Nothing (Just ("download", "regular", "text-blue-500")) "Response size" "Total response body size in bytes" (show (respSize - 2)) Nothing Nothing
        statBox_ Nothing (Just ("stack", "regular", "text-blue-500")) "Framework" "Framework used to handle this the request" (show req.sdkType) Nothing Nothing

    -- errors
    when (req.errorsCount > 0) $ div_ [class_ "mt-4"] do
      div_ [class_ "flex w-full text-slate-950 font-medium gap-2 items-center"] do
        p_ "Errors"
        p_ [class_ " text-red-500 font-bold"] $ show req.errorsCount
      div_ [class_ "p-4 rounded-3xl border border-slate-200 text-gray-500"] do
        jsonValueToHtmlTree req.errors

    -- outgoing request details
    div_ [class_ "flex w-full flex-col gap-1"] do
      p_ [class_ "font-medium text-slate-950 mb-2"] "Outgoing requests"
      div_ [class_ "grow rounded-3xl border border-slate-200 overflow-y-auto py-2 px-1 max-h-[500px] whitespace-nowrap  divide-y overflow-x-hidden"] do
        let createdAt = toText $ formatTime defaultTimeLocale "%FT%T%6QZ" req.createdAt
        let escapedQueryPartial = toText $ escapeURIString isUnescapedInURI $ toString [fmt|parent_id=="{UUID.toText req.id}" AND created_at<="{createdAt}"|]
        let events_url = "/p/" <> pid.toText <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial
        div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""

    -- request details
    div_ [class_ "mt-8", id_ "req-tabs-container"] do
      p_ [class_ "text-slate-950 font-medium mb-2"] "Request Details"
      div_ [class_ "rounded-3xl border border-slate-200", role_ "tablist"] do
        div_ [class_ "flex w-full text-slate-500"] do
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max t-tab-active", onclick_ "navigatable(this, '#req_body_json', '#req-tabs-container', 't-tab-active')"] $ "Body"
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max", onclick_ "navigatable(this, '#req_headers_json', '#req-tabs-container', 't-tab-active')"] $ "Headers"
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max", onclick_ "navigatable(this, '#query_params_json', '#req-tabs-container', 't-tab-active')"] $ "Query"
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max", onclick_ "navigatable(this, '#path_params_json', '#req-tabs-container', 't-tab-active')"] $ "Path Params"
          button_ [class_ "border-b border-b-slate-200 w-full"] pass

        div_ [class_ "a-tab-content m-4  rounded-xl p-2 border border-slate-200", id_ "req_body_json"]
          $ jsonValueToHtmlTree req.requestBody

        div_ [class_ "a-tab-content m-4 hidden rounded-xl p-2 border border-slate-200 break-all", id_ "req_headers_json"]
          $ jsonValueToHtmlTree req.requestHeaders

        div_ [class_ "a-tab-content m-4 hidden rounded-xl p-2 border border-slate-200", id_ "query_params_json"]
          $ jsonValueToHtmlTree req.queryParams

        div_ [class_ "a-tab-content m-4 hidden rounded-xl p-2 border border-slate-200", id_ "path_params_json"]
          $ jsonValueToHtmlTree req.pathParams

    -- response details
    div_ [class_ "mt-8", id_ "res-tabs-container"] do
      p_ [class_ "text-slate-950 font-medium mb-2"] "Request Details"
      div_ [class_ "rounded-3xl border border-slate-200", role_ "tablist"] do
        div_ [class_ "flex w-full text-slate-500"] do
          button_ [class_ "a-tab px-3 border-b border-b-slate-200 py-2 w-max t-tab-active", onclick_ "navigatable(this, '#res_body_json', '#res-tabs-container', 't-tab-active')"] $ "Body"
          button_ [class_ "a-tab px-3 border-b border-b-slate-200 py-2 w-max", role_ "tab", onclick_ "navigatable(this, '#res_headers_json', '#res-tabs-container', 't-tab-active')"] $ "Headers"
          button_ [class_ "border-b border-b-slate-200 w-full"] pass

        div_ [class_ "a-tab-content m-4 rounded-xl p-2 border border-slate-200", id_ "res_body_json"]
          $ jsonValueToHtmlTree req.responseBody

        div_ [class_ "a-tab-content m-4 hidden rounded-xl p-2 border border-slate-200", id_ "res_headers_json"]
          $ jsonValueToHtmlTree req.responseHeaders


apiLogItemH :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Maybe Text -> ATAuthCtx (RespHeaders ApiLogItem)
apiLogItemH pid rdId createdAt sourceM = do
  let source = fromMaybe "requests" sourceM
  _ <- Sessions.sessionAndProject pid
  logItem <- case sourceM of
    Just "logs" -> do
      logItem <- Telemetry.logRecordByProjectAndId pid createdAt rdId
      pure $ AE.toJSON <$> logItem
    Just "spans" -> do
      spanItem <- Telemetry.spanRecordByProjectAndId pid createdAt rdId
      pure $ selectiveSpanToJson <$> spanItem
    _ -> do
      logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      pure $ selectiveReqToJson <$> logItemM
  addRespHeaders $ case logItem of
    Just req -> ApiLogItem pid rdId req (requestDumpLogItemUrlPath pid rdId createdAt) source
    Nothing -> ApiLogItemNotFound $ "Invalid " <> source <> " ID"


requestDumpLogItemUrlPath :: Projects.ProjectId -> UUID.UUID -> UTCTime -> Text
requestDumpLogItemUrlPath pid rdId timestamp = "/p/" <> pid.toText <> "/log_explorer/" <> UUID.toText rdId <> "/" <> fromString (formatShow iso8601Format timestamp)


data ApiLogItem
  = ApiLogItem Projects.ProjectId UUID.UUID AE.Value Text Text
  | ApiLogItemNotFound Text


instance ToHtml ApiLogItem where
  toHtml (ApiLogItem pid logId req expandItemPath source) = toHtml $ apiLogItemView pid logId req expandItemPath source
  toHtml (ApiLogItemNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


data ApiItemDetailed
  = LogItemExpanded Projects.ProjectId RequestDumps.RequestDumpLogItem Bool
  | SpanItemExpanded Projects.ProjectId Telemetry.SpanRecord
  | ItemDetailedNotFound Text


instance ToHtml ApiItemDetailed where
  toHtml (LogItemExpanded pid log_item is_modal) = toHtml $ expandAPIlogItem' pid log_item is_modal
  toHtml (SpanItemExpanded pid span_item) = toHtml $ Spans.expandedSpanItem pid span_item Nothing Nothing
  toHtml (ItemDetailedNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


apiLogItemView :: Projects.ProjectId -> UUID.UUID -> AE.Value -> Text -> Text -> Html ()
apiLogItemView pid logId req expandItemPath source = do
  let trId = case req of
        AE.Object o -> case KEM.lookup "trace_id" o of
          Just (AE.String trid) -> Just trid
          _ -> Nothing
        _ -> Nothing
  let logItemPathDetailed = if source == "spans" then "/p/" <> pid.toText <> "/traces/" <> fromMaybe "" trId else expandItemPath <> "/detailed?source=" <> source
  div_ [class_ "flex items-center gap-2"] do
    when (source /= "logs")
      $ label_
        [ class_ "btn btn-sm bg-base-100"
        , Lucid.for_ "global-data-drawer"
        , term "_"
            $ [text|on mousedown or click fetch $logItemPathDetailed
                  then set #global-data-drawer-content.innerHTML to #loader-tmp.innerHTML
                  then set #global-data-drawer.checked to true
                  then set #global-data-drawer-content.innerHTML to it
                  then htmx.process(#global-data-drawer-content) then _hyperscript.processNode(#global-data-drawer-content) then window.evalScriptsFromContent(#global-data-drawer-content)|]
        ]
        ("Expand" >> faSprite_ "expand" "regular" "h-3 w-3")

    let reqJson = decodeUtf8 $ AE.encode req
    when (source /= "logs" && source /= "spans")
      $ button_
        [ class_ "btn btn-sm bg-base-100"
        , term "data-reqJson" reqJson
        , onclick_ "window.buildCurlRequest(event)"
        ]
        (span_ [] "Copy as curl" >> faSprite_ "copy" "regular" "h-3 w-3")

    button_
      [ class_ "btn btn-sm bg-base-100"
      , onclick_ "window.downloadJson(event)"
      , term "data-reqJson" reqJson
      ]
      (span_ [] "Download" >> faSprite_ "arrow-down-to-line" "regular" "h-3 w-3")
  jsonValueToHtmlTree req


-- Function to selectively convert RequestDumpLogItem to JSON
selectiveReqToJson :: RequestDumps.RequestDumpLogItem -> AE.Value
selectiveReqToJson req =
  AE.object
    $ concat @[]
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


selectiveSpanToJson :: Telemetry.SpanRecord -> AE.Value
selectiveSpanToJson sp =
  AE.object
    $ concat @[]
      [ ["timestamp" .= sp.timestamp]
      , ["span_id" .= sp.spanId]
      , ["span_name" .= sp.spanName]
      , ["kind" .= sp.kind]
      , ["links" .= sp.links]
      , ["trace_id" .= sp.traceId]
      , ["start_time" .= sp.startTime]
      , ["status" .= sp.status]
      , ["status_message" .= sp.statusMessage]
      , ["parent_span_id" .= sp.parentSpanId]
      , ["trace_state" .= sp.traceState]
      , ["intrumentation_scope" .= sp.instrumentationScope]
      , ["attributes" .= sp.attributes]
      , ["resource" .= sp.resource]
      ]


-- | jsonValueToHtmlTree takes an aeson json object and renders it as a collapsible html tree, with hyperscript for interactivity.
