module Pages.Telemetry.Spans (expandedSpanItem, spanLatencyBreakdown, spanGetH) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KEM
import Data.ByteString.Base64 qualified as B64
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (formatTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (Context (..), SpanRecord (..), convertSpanToRequestMessage)
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Components (dateTime)
import Pages.Telemetry.Utils (getErrorDetails, getRequestDetails, getServiceName, getSpanErrors)
import Relude
import RequestMessages (RequestMessage (..))
import System.Types
import Utils


spanGetH :: Projects.ProjectId -> Text -> Text -> ATAuthCtx (RespHeaders (Html ()))
spanGetH pid trId spanId = do
  spanRecord <- Telemetry.spanRecordById pid trId spanId
  case spanRecord of
    Just sp -> do
      aptSpan <- case getRequestDetails sp.attributes of
        Just ("HTTP", _, _, _) -> do
          if sp.name /= Just "apitoolkit-http-span"
            then do
              spn <- Telemetry.spanRecordByName pid trId "apitoolkit-http-span"
              pure spn
            else pure Nothing
        _ -> pure Nothing
      addRespHeaders $ expandedSpanItem pid sp aptSpan Nothing Nothing
    Nothing -> do
      addRespHeaders $ h1_ [] "Span not found"


expandedSpanItem :: Projects.ProjectId -> Telemetry.OtelLogsAndSpans -> Maybe Telemetry.OtelLogsAndSpans -> Maybe Text -> Maybe Text -> Html ()
expandedSpanItem pid sp aptSp leftM rightM = do
  let reqDetails = getRequestDetails sp.attributes
  div_ [class_ "w-full px-2 pb-2 relative pb-[50px]"] $ do
    div_ [class_ "flex justify-between items-center", id_ "copy_share_link"] pass
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "loading-span-list"] ""
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
    div_ [class_ "flex flex-col gap-4 bg-gray-50 py-2  px-2"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "whitespace-nowrap font-semibold text-textStrong"] "Trace Span"
        div_ [class_ "flex gap-4 items-center"] $ do
          dateTime sp.start_time Nothing
          div_ [class_ "flex gap-2 items-center"] do
            button_
              [ [__|on click add .hidden to #trace_expanded_view 
            then put '0px' into  #log_details_container.style.width 
            then put '100%' into #logs_list_container.style.width 
            then add .hidden to #resizer
            then remove .bg-fillBrand-strong from <.item-row.bg-fillBrand-strong/>
            then call updateUrlState('details_width', '', 'delete')
            then call updateUrlState('target_event', '0px', 'delete')
            then call updateUrlState('showTrace', "true", 'delete')
            |]
              ]
              do
                faSprite_ "xmark" "regular" "w-3 h-3 text-textBrand"

      div_ [class_ "flex items-center gap-4 text-sm font-medium text-slate-950"] $ do
        case reqDetails of
          Just req -> do
            div_ [class_ "flex flex-wrap items-center gap-2"] do
              whenJust reqDetails $ \case
                ("HTTP", method, path, status) -> do
                  let methodClass = getMethodColor method
                      borderColor = getMethodBorderColor method
                      extraClass = getStatusColor status
                      stBorder = getStatusBorderColor status
                  span_ [class_ $ "px-2 py-1 rounded-lg text-sm border " <> borderColor <> " " <> methodClass] $ toHtml method
                  span_ [class_ $ "px-2 py-1 rounded-lg text-sm border " <> stBorder <> " " <> extraClass] $ toHtml $ T.take 3 $ show status
                  div_ [class_ "flex items-center"] do
                    span_ [class_ "shrink-1 px-2 py-1.5 max-w-96 truncate mr-2 urlPath"] $ toHtml path
                    div_ [[__| install Copy(content:.urlPath )|]] do
                      faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-fillWeaker rounded-full p-2 text-slate-500"
                (scheme, method, path, status) -> do
                  -- span_ [class_ " font-medium border rounded-sm px-2 py-1.5"] $ toHtml scheme
                  div_ [class_ "flex flex-wrap items-center"] do
                    span_ [class_ "flex gap-2 items-center text-textStrong bg-fillWeak border border-strokeWeak rounded-lg whitespace-nowrap px-2 py-1"] $ toHtml method
                    span_ [class_ "px-2 py-1.5 max-w-96"] $ toHtml path
                    let extraClass = getGrpcStatusColor status
                    when (scheme /= "DB") $ span_ [class_ $ " px-2 py-1.5 border-l " <> extraClass] $ toHtml $ show status
          Nothing -> do
            h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml $ fromMaybe "" sp.name

      div_ [class_ "flex gap-2 flex-wrap"] $ do
        spanBadge (toText $ getDurationNSMS $ maybe 0 fromIntegral sp.duration) "Span duration"
        spanBadge (getServiceName sp.resource) "Service"
        spanBadge ("Spand ID: " <> maybe "" (\c -> fromMaybe "" c.span_id) sp.context) "Span ID"
        spanBadge (fromMaybe "" sp.kind) "Span Kind"
      -- spanBadge (maybe "" show sp.status) "Span Status"

      div_ [class_ "flex gap-2 items-center text-textBrand font-medium text-xs"] do
        whenJust reqDetails $ \case
          ("HTTP", _, _, _) -> do
            let json = decodeUtf8 $ AE.encode $ convertSpanToRequestMessage sp "" >>= (\req -> Just $ selectiveReqToJson req)
            button_
              [ class_ "flex items-center gap-1"
              , onclick_ "window.buildCurlRequest(event)"
              , term "data-reqjson" json
              ]
              do
                "Copy request as curl"
                faSprite_ "copy" "regular" "w-3 h-3"
          _ -> pass
        whenJust sp.context $ \ctx -> do
          whenJust ctx.trace_id $ \trId -> do
            let tracePath = "/p/" <> pid.toText <> "/traces/" <> trId <> "/"
            button_
              [ class_ "flex items-end gap-1"
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
        let sp_id = UUID.toText sp.id
        let createdAt = toText $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" $ sp.timestamp
        button_
          [ class_ "flex items-center gap-2"
          , hxPost_ $ "/p/" <> pid.toText <> "/share/" <> sp_id <> "/" <> createdAt <> "?event_type=span"
          , hxSwap_ "innerHTML"
          , hxTarget_ "#copy_share_link"
          ]
          do
            "Generate shareable link"
            faSprite_ "link-simple" "regular" "w-3 h-3"

    div_ [class_ "w-full mt-8", id_ "span-tabs-container"] do
      let spanErrors = getSpanErrors $ fromMaybe AE.Null sp.events
          isHttp = case reqDetails of
            Just ("HTTP", _, _, _) -> True
            _ -> False
      div_ [class_ "flex", [__|on click halt|]] $ do
        when isHttp $ button_ [class_ "a-tab cursor-pointer  border-b-2 border-b-slate-200 px-4 py-1.5 t-tab-active", onclick_ "navigatable(this, '#request-content', '#span-tabs-container', 't-tab-active','.http')"] "Request"
        button_ [class_ $ "a-tab cursor-pointer border-b-2 border-b-slate-200 px-4 py-1.5 " <> if isHttp then "" else "t-tab-active", onclick_ "navigatable(this, '#att-content', '#span-tabs-container', 't-tab-active','.http')"] "Attributes"
        button_ [class_ "a-tab cursor-pointer border-b-2 border-b-slate-200 px-4 py-1.5 ", onclick_ "navigatable(this, '#meta-content', '#span-tabs-container', 't-tab-active', '.http')"] "Process"
        unless (null spanErrors) $ do
          button_ [class_ "a-tab cursor-pointer border-b-2 border-b-slate-200 flex items-center gap-1 nowrap px-4 py-1.5 ", onclick_ "navigatable(this, '#errors-content', '#span-tabs-container', 't-tab-active', '.http')"] do
            "Errors"
            div_ [class_ "badge badge-error badge-sm"] $ show $ length spanErrors
        button_ [class_ "a-tab cursor-pointer border-b-2 border-b-slate-200 flex items-center gap-1 px-4 py-1.5 ", onclick_ "navigatable(this, '#logs-content', '#span-tabs-container', 't-tab-active','.http')"] $ do
          "Logs"
          div_ [class_ "badge badge-ghost badge-sm"] $ show $ numberOfEvents $ fromMaybe AE.Null sp.events
        button_ [class_ "a-tab cursor-pointer border-b-2 whitespace-nowrap border-b-slate-200 px-4 py-1.5", onclick_ "navigatable(this, '#m-raw-content', '#span-tabs-container', 't-tab-active','.http')"] "Raw data"
        div_ [class_ "w-full border-b-2 border-b-slate-200"] pass

      div_ [class_ "grid my-4 text-slate-600 font"] $ do
        div_ [class_ "hidden a-tab-content", id_ "m-raw-content"] $ do
          jsonValueToHtmlTree (selectiveOtelLogsJson sp) Nothing
        div_ [class_ $ "a-tab-content " <> if isHttp then "hidden" else "", id_ "att-content"] $ do
          jsonValueToHtmlTree (fromMaybe (AE.object []) (fmap AE.Object $ fmap KEM.fromMapText sp.attributes)) (Just "attributes")
        div_ [class_ "hidden a-tab-content", id_ "meta-content"] $ do
          jsonValueToHtmlTree (fromMaybe (AE.object []) (fmap AE.Object $ fmap KEM.fromMapText sp.resource)) (Just "resource")
        div_ [class_ "hidden a-tab-content", id_ "errors-content"] $ do
          renderErrors spanErrors
        div_ [class_ "hidden a-tab-content", id_ "logs-content"] $ do
          jsonValueToHtmlTree (AE.toJSON sp.events) Nothing

        whenJust reqDetails $ \case
          ("HTTP", method, path, status) -> do
            let cSp = fromMaybe sp aptSp
            let httpJsonM = convertSpanToRequestMessage cSp ""
            case httpJsonM of
              Just httpJson -> do
                div_ [class_ "a-tab-content nested-tab", id_ "request-content"] do
                  div_ [id_ "http-content-container", class_ "flex flex-col gap-3 mt-2"] do
                    div_ [class_ "bg-fillWeak w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"] $ do
                      div_ [class_ "justify-start items-start flex text-sm"] $ do
                        button_ [onclick_ "navigatable(this, '#req_content', '#http-content-container', 't-tab-box-active')", class_ "http a-tab px-3 py-1 rounded-lg text-textWeak t-tab-box-active"] "Req Body"
                        button_ [onclick_ "navigatable(this, '#res_content', '#http-content-container', 't-tab-box-active')", class_ "http a-tab px-3 py-1 rounded-lg text-textWeak"] "Res Body"
                        button_ [onclick_ "navigatable(this, '#hed_content', '#http-content-container', 't-tab-box-active')", class_ "http a-tab px-3 py-1 rounded-lg text-textWeak"] "Headers"
                        button_ [onclick_ "navigatable(this, '#par_content', '#http-content-container', 't-tab-box-active')", class_ "http a-tab px-3 py-1 rounded-lg text-textWeak"] "Params"
                        button_ [onclick_ "navigatable(this, '#raw_content', '#http-content-container', 't-tab-box-active')", class_ "http a-tab px-3 py-1 rounded-lg text-textWeak"] "Request Details"
                    div_ [] do
                      div_ [id_ "raw_content", class_ "hidden a-tab-content http"] do
                        jsonValueToHtmlTree (selectiveReqToJson httpJson) Nothing
                      div_ [id_ "req_content", class_ "a-tab-content http"] do
                        let b = case cSp.body of
                              Just (AE.Object bb) -> case KEM.lookup "request_body" bb of
                                Just a -> a
                                _ -> AE.object []
                              _ -> AE.object []
                        jsonValueToHtmlTree b $ Just "body.request_body"
                      div_ [id_ "res_content", class_ "hidden a-tab-content http"] do
                        let b = case cSp.body of
                              Just (AE.Object bb) -> case KEM.lookup "response_body" bb of
                                Just a -> a
                                _ -> AE.object []
                              _ -> AE.object []
                        jsonValueToHtmlTree b $ Just "body.response_body"
                      div_ [id_ "hed_content", class_ "hidden a-tab-content http"] do
                        jsonValueToHtmlTree (AE.object ["request_headers" AE..= httpJson.requestHeaders, "response_headers" AE..= httpJson.responseHeaders]) Nothing
                      div_ [id_ "par_content", class_ "hidden a-tab-content http"] do
                        jsonValueToHtmlTree (AE.object ["query_params" AE..= httpJson.queryParams, "path_params" AE..= httpJson.pathParams]) Nothing
              Nothing -> pass
          _ -> pass


renderErrors :: [AE.Value] -> Html ()
renderErrors errs = div_ [class_ "flex flex-col gap-1"] $ do
  forM_ errs $ \err -> do
    let (tye, message, stacktrace) = getErrorDetails err
    div_ [class_ "flex flex-col rounded-lg border overflow-hidden"] $ do
      div_ [class_ "bg-red-100 text-red-600 px-4 py-2 flex gap-2 items-center"] do
        span_ [class_ "font-bold"] $ toHtml (tye <> ":")
        span_ [] $ toHtml message
      div_ [] do
        p_ [class_ "whitespace-nowrap px-4 py-2"] $ toHtml stacktrace


numberOfEvents :: AE.Value -> Int
numberOfEvents (AE.Array obj) = length obj
numberOfEvents _ = 0


spanLatencyBreakdown :: V.Vector Telemetry.SpanRecord -> Html ()
spanLatencyBreakdown spans = do
  let colors = getServiceColors $ (.spanName) <$> spans
  let totalDuration = sum $ (.spanDurationNs) <$> spans
  div_ [class_ "flex h-6 w-[150px] "] $ do
    forM_ (zip [0 ..] (V.toList spans)) \(i, sp) -> do
      -- use percentage of total duration to determine width of bar
      let wdth = (fromIntegral sp.spanDurationNs / fromIntegral totalDuration) * 150
      let color = fromMaybe "bg-black" $ HM.lookup sp.spanName colors
      let roundr = if i == length spans - 1 then "rounded-r " else ""
          roundl = if i == 0 then "rounded-l " else ""
      div_
        [ class_ $ "h-full overflow-hidden  " <> roundl <> roundr <> color
        , style_ $ "width:" <> show wdth <> "px;"
        , term "data-tippy-content" $ "Span name: " <> sp.spanName <> " Duration: " <> toText (getDurationNSMS sp.spanDurationNs)
        , title_ $ "Span name: " <> sp.spanName <> " Duration: " <> toText (getDurationNSMS sp.spanDurationNs)
        ]
        do
          div_ [class_ "h-full w-full"] ""


spanBadge :: Text -> Text -> Html ()
spanBadge val key = do
  div_
    [ class_ "flex gap-2 items-center text-textStrong bg-fillWeak border border-strokeWeak text-xs rounded-lg whitespace-nowrap px-2 py-1"
    , term "data-tippy-content" key
    ]
    $ do
      span_ [] $ toHtml val


selectiveReqToJson :: RequestMessage -> AE.Value
selectiveReqToJson req =
  AE.object $
    concat @[]
      [ ["created_at" AE..= req.timestamp]
      , ["errors" AE..= fromMaybe [] req.errors]
      , ["host" AE..= req.host]
      , ["method" AE..= req.method]
      , ["parent_id" AE..= req.parentId]
      , ["path_params" AE..= req.pathParams]
      , ["query_params" AE..= req.queryParams]
      , ["raw_url" AE..= req.rawUrl]
      , ["referer" AE..= req.referer]
      , ["request_body" AE..= b64ToJson req.requestBody]
      , ["request_headers" AE..= req.requestHeaders]
      , ["response_body" AE..= b64ToJson req.responseBody]
      , ["response_headers" AE..= req.responseHeaders]
      , ["service_version" AE..= req.serviceVersion]
      , ["status_code" AE..= req.statusCode]
      , ["tags" AE..= req.tags]
      , ["url_path" AE..= req.urlPath]
      ]


selectiveOtelLogsJson :: Telemetry.OtelLogsAndSpans -> AE.Value
selectiveOtelLogsJson sp =
  AE.object $
    concat @[]
      [ ["start_time" AE..= sp.start_time]
      , ["end_time" AE..= sp.end_time]
      , ["resource" AE..= sp.resource]
      , ["attributes" AE..= sp.attributes]
      , maybe [] (\d -> ["name" AE..= d]) sp.name
      , maybe [] (\d -> ["status" AE..= d]) sp.status_code
      , maybe [] (\d -> ["kind" AE..= d]) sp.kind
      , maybe [] (\d -> ["body" AE..= d]) sp.body
      , maybe [] (\d -> ["duration" AE..= (getDurationNSMS $ fromIntegral d)]) sp.duration
      , ["parent_id" AE..= sp.parent_id]
      , maybe [] (\d -> ["context" AE..= d]) sp.context
      , maybe [] (\d -> ["severity" AE..= d]) sp.severity
      , maybe [] (\d -> ["events" AE..= d]) sp.events
      , maybe [] (\d -> ["link" AE..= d]) sp.links
      ]
