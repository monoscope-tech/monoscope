module Pages.LogExplorer.LogItem (expandAPIlogItemH, expandAPIlogItem', ApiItemDetailed (..)) where

import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BS
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Pages.Components (dateTime, statBox_)
import Pages.Telemetry.Spans qualified as Spans
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
    "logs" -> do
      logItem <- Telemetry.logRecordByProjectAndId pid createdAt rdId
      addRespHeaders $ case logItem of
        Just req -> LogItemExpanded pid (AE.toJSON req)
        Nothing -> ItemDetailedNotFound "Log not found"
    _ -> do
      logItemM <- dbtToEff $ RequestDumps.selectRequestDumpByProjectAndId pid createdAt rdId
      addRespHeaders $ case logItemM of
        Just req -> RequestItemExpanded pid req True
        Nothing -> ItemDetailedNotFound "Request not found"


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
          let createdAt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" req.createdAt
          button_
            [ class_ "flex items-center gap-2 text-textBrand text-sm"
            , hxPost_ $ "/p/" <> pid.toText <> "/share/" <> UUID.toText req.id <> "/" <> createdAt <> "?event_type=request"
            , hxSwap_ "innerHTML"
            , hxTarget_ "#copy_share_link"
            ]
            do
              "Get share link"
              faSprite_ "link-simple" "regular" "w-3 h-3"
          button_ [class_ "ml-4 p-0 -mt-1", [__|on click add .hidden to #trace_expanded_view then put '0px' into  #log_details_container.style.width|]] do
            faSprite_ "xmark" "regular" "w-3 h-3 text-textBrand"
    -- url, endpoint, latency, request size, repsonse size
    let endpointHash = toXXHash $ pid.toText <> req.host <> req.method <> req.urlPath
    let endpointURl = "/p/" <> pid.toText <> "/log_explorer/endpoint/" <> endpointHash
    div_ [class_ "flex flex-col mt-4 justify-between w-full"] do
      div_ [class_ "text-base mb-2 flex gap-6 items-center"] do
        span_ [class_ "text-slate-500 font-medium w-16"] "Endpoint"
        div_ [class_ "flex gap-1 items-center"] do
          span_ [class_ "text-slate-800 text-sm truncate ellipsis urlPath", term "data-tippy" req.urlPath] $ toHtml req.urlPath
          div_ [[__| install Copy(content:.urlPath )|]] do
            faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-fillWeaker rounded-full p-2 text-slate-500"
          a_ [href_ endpointURl] do
            faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 btn-primary rounded-full"
      div_ [class_ "text-base flex items-center gap-6"] do
        span_ [class_ "text-slate-500 font-medium w-16"] "URL"
        div_ [class_ "flex gap-1 items-center"] do
          span_ [class_ "text-slate-800 text-sm truncate ellipsis", term "data-tippy" req.rawUrl] $ toHtml req.rawUrl
          div_ [[__| install Copy(content:.urlPath )|]] do
            faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-fillWeaker rounded-full p-2 text-slate-500"
          a_ [href_ endpointURl] do
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
      div_ [class_ "p-4 rounded-3xl border border-slate-200 text-gray-500"] do
        jsonValueToHtmlTree req.errors

    -- outgoing request details
    -- div_ [class_ "flex w-full flex-col gap-1"] do
    --   p_ [class_ "font-medium text-slate-950 mb-2"] "Outgoing requests"
    --   div_ [class_ "grow rounded-3xl border border-slate-200 overflow-y-auto py-2 px-1 max-h-[500px] whitespace-nowrap  divide-y overflow-x-hidden"] do
    --     let createdAt = toText $ formatTime defaultTimeLocale "%FT%T%6QZ" req.createdAt
    --     let escapedQueryPartial = toText $ escapeURIString isUnescapedInURI $ toString [fmt|parent_id=="{UUID.toText req.id}" AND created_at<="{createdAt}"|]
    --         parentId = UUID.toText req.id
    --         escapedQueryAST = [text|%5B%7B"tag"%3A"Search"%2C"contents"%3A%7B"tag"%3A"And"%2C"contents"%3A%5B%7B"tag"%3A"Eq"%2C"contents"%3A%5B"parent_id"%2C"$parentId"%5D%7D%2C%7B"tag"%3A"LTEq"%2C"contents"%3A%5B"created_at"%2C"$createdAt"%5D%7D%5D%7D%7D%5D|]
    --         events_url = "/p/" <> pid.toText <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial <> "&queryAST=" <> escapedQueryAST
    --     div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""

    -- request details
    div_ [class_ "mt-8", id_ "req-tabs-container"] do
      p_ [class_ "text-slate-950 font-medium mb-2"] "Request Details"
      div_ [class_ "rounded-3xl border border-slate-200", role_ "tablist"] do
        div_ [class_ "flex w-full text-slate-500"] do
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max t-tab-active", onclick_ "navigatable(this, '#req_body_json', '#req-tabs-container', 't-tab-active')"] "Body"
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max", onclick_ "navigatable(this, '#req_headers_json', '#req-tabs-container', 't-tab-active')"] "Headers"
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max", onclick_ "navigatable(this, '#query_params_json', '#req-tabs-container', 't-tab-active')"] "Query"
          button_ [class_ "a-tab whitespace-nowrap px-3 py-2 border-b border-b-slate-200 w-max", onclick_ "navigatable(this, '#path_params_json', '#req-tabs-container', 't-tab-active')"] "Path Params"
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
          button_ [class_ "a-tab px-3 border-b border-b-slate-200 py-2 w-max t-tab-active", onclick_ "navigatable(this, '#res_body_json', '#res-tabs-container', 't-tab-active')"] "Body"
          button_ [class_ "a-tab px-3 border-b border-b-slate-200 py-2 w-max", role_ "tab", onclick_ "navigatable(this, '#res_headers_json', '#res-tabs-container', 't-tab-active')"] "Headers"
          button_ [class_ "border-b border-b-slate-200 w-full"] pass

        div_ [class_ "a-tab-content m-4 rounded-xl p-2 border border-slate-200", id_ "res_body_json"]
          $ jsonValueToHtmlTree req.responseBody

        div_ [class_ "a-tab-content m-4 hidden rounded-xl p-2 border border-slate-200", id_ "res_headers_json"]
          $ jsonValueToHtmlTree req.responseHeaders


data ApiItemDetailed
  = RequestItemExpanded Projects.ProjectId RequestDumps.RequestDumpLogItem Bool
  | SpanItemExpanded Projects.ProjectId Telemetry.SpanRecord
  | LogItemExpanded Projects.ProjectId AE.Value
  | ItemDetailedNotFound Text


instance ToHtml ApiItemDetailed where
  toHtml (RequestItemExpanded pid log_item is_modal) = toHtml $ expandAPIlogItem' pid log_item is_modal
  toHtml (SpanItemExpanded pid span_item) = toHtml $ Spans.expandedSpanItem pid span_item Nothing Nothing
  toHtml (LogItemExpanded pid req) = toHtml $ apiLogItemView pid req
  toHtml (ItemDetailedNotFound message) = div_ [] $ toHtml message
  toHtmlRaw = toHtml


apiLogItemView :: Projects.ProjectId -> AE.Value -> Html ()
apiLogItemView pid req = do
  div_ [class_ "px-2 flex flex-col w-full items-center gap-2"] do
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
    jsonValueToHtmlTree req
