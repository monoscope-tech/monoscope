module Pages.Telemetry.Spans (expandedSpanItem, spanLatencyBreakdown, spanGetH) where

import Data.Aeson qualified as AE
import Data.Effectful.UUID qualified as UUID
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (formatTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanRecord (..), convertSpanToRequestMessage)
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
      addRespHeaders $ expandedSpanItem pid sp Nothing Nothing
    Nothing -> do
      addRespHeaders $ h1_ [] "Span not found"


expandedSpanItem :: Projects.ProjectId -> Telemetry.SpanRecord -> Maybe Text -> Maybe Text -> Html ()
expandedSpanItem pid sp leftM rightM = do
  let reqDetails = getRequestDetails sp
  div_ [class_ "w-full px-2 pb-2 relative"] $ do
    div_ [class_ "flex justify-between items-center", id_ "copy_share_link"] pass
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "loading-span-list"] ""
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "details_indicator"] ""
    div_ [class_ "flex flex-col gap-4 bg-gray-50 py-2  px-2"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          -- div_ [class_ "flex relative flex-col items-center justify-center"] do
          --   span_
          --     [ class_ "cursor-pointer absolute -top-[18px] h-max text-textWeak"
          --     , hxGet_ $ "/p/" <> pid.toText <> "/traces/" <> sp.traceId <> "/?span_id="
          --     , hxSwap_ "innerHTML"
          --     , hxTarget_ "#trace_span_container"
          --     , hxTrigger_ "click"
          --     ]
          --     $ faSprite_ "p-chevron-up" "regular" "w-5 h-5"
          --   span_
          --     [ class_ "cursor-pointer absolute w-max -bottom-[18px] h-max text-textWeak"
          --     , hxGet_ $ "/p/" <> pid.toText <> "/traces/" <> sp.traceId <> "/?span_id="
          --     , hxSwap_ "innerHTML"
          --     , hxTarget_ "#trace_span_container"
          --     , hxTrigger_ "click"
          --     ]
          --     $ faSprite_ "p-chevron-down" "regular" "w-5 h-5"
          h3_ [class_ "whitespace-nowrap font-semibold text-textStrong"] "Trace Span"
        div_ [class_ "flex gap-4 items-center"] $ do
          dateTime sp.startTime Nothing
          div_ [class_ "flex gap-2 items-center"] do
            button_ [[__|on click add .hidden to #trace_expanded_view then put '0px' into  #log_details_container.style.width|]] do
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
                    a_ [href_ "", class_ "ml-1"] do
                      faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 btn-primary rounded-full"
                (scheme, method, path, status) -> do
                  -- span_ [class_ " font-medium border rounded px-2 py-1.5"] $ toHtml scheme
                  div_ [class_ "flex border rounded overflow-hidden"] do
                    span_ [class_ " px-2 py-1.5 max-w-44 truncate bg-gray-200 border-r"] $ toHtml method
                    span_ [class_ " px-2 py-1.5 max-w-96 truncate"] $ toHtml path
                    let extraClass = getGrpcStatusColor status
                    span_ [class_ $ " px-2 py-1.5 border-l " <> extraClass] $ toHtml $ show status
          Nothing -> do
            h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml sp.spanName

      div_ [class_ "flex gap-2 flex-wrap"] $ do
        spanBadge (toText $ getDurationNSMS sp.spanDurationNs) "Span duration"
        spanBadge (getServiceName sp) "Service"
        spanBadge ("Spand ID: " <> sp.spanId) "Span ID"
        spanBadge (maybe "" show sp.kind) "Span Kind"
        spanBadge (maybe "" show sp.status) "Span Status"

      div_ [class_ "flex gap-2 items-center text-textBrand font-medium text-xs"] do
        -- button_ [class_ "flex items-center gap-2"] do
        --   "Copy requests as curl"
        --   faSprite_ "copy" "regular" "w-3 h-3"
        let tracePath = "/p/" <> pid.toText <> "/traces/" <> sp.traceId <> "/"
        button_
          [ class_ "flex items-end gap-1"
          , term
              "_"
              [text|on click remove .hidden from #trace_expanded_view
                    then set #trace_expanded_view.innerHTML to #loader-tmp.innerHTML
                    then fetch $tracePath
                    then set #trace_expanded_view.innerHTML to it
                    then htmx.process(#trace_expanded_view)
                    then _hyperscript.processNode(#trace_expanded_view) then window.evalScriptsFromContent(#trace_expanded_view)|]
          ]
          do
            "View parent trace"
            faSprite_ "cross-hair" "regular" "w-4 h-4"
        let sp_id = UUID.toText sp.uSpanId
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

      whenJust reqDetails $ \case
        ("HTTP", method, path, status) -> do
          let httpJson = convertSpanToRequestMessage sp ""
          div_ [id_ "http-content-container", class_ "flex flex-col gap-3"] do
            div_ [class_ "bg-fillWeak w-max rounded-lg border border-strokeWeak justify-start items-start inline-flex"] $ do
              div_ [class_ "justify-start items-start flex text-sm"] $ do
                button_ [onclick_ "navigatable(this, '#raw_content', '#http-content-container', 't-tab-box-active')", class_ "a-tab px-3 py-1 rounded-lg text-textWeak t-tab-box-active"] "Raw Details"
                button_ [onclick_ "navigatable(this, '#req_content', '#http-content-container', 't-tab-box-active')", class_ "a-tab px-3 py-1 rounded-lg text-textWeak"] "Req Body"
                button_ [onclick_ "navigatable(this, '#res_content', '#http-content-container', 't-tab-box-active')", class_ "a-tab px-3 py-1 rounded-lg text-textWeak"] "Res Body"
                button_ [onclick_ "navigatable(this, '#hed_content', '#http-content-container', 't-tab-box-active')", class_ "a-tab px-3 py-1 rounded-lg text-textWeak"] "Headers"
                button_ [onclick_ "navigatable(this, '#par_content', '#http-content-container', 't-tab-box-active')", class_ "a-tab px-3 py-1 rounded-lg text-textWeak"] "Params"
            div_ [] do
              div_ [id_ "raw_content", class_ "a-tab-content"] do
                jsonValueToHtmlTree $ selectiveReqToJson httpJson
              div_ [id_ "req_content", class_ "hidden a-tab-content"] do
                jsonValueToHtmlTree $ AE.toJSON httpJson.requestBody
              div_ [id_ "res_content", class_ "hidden a-tab-content"] do
                jsonValueToHtmlTree $ AE.toJSON httpJson.responseBody
              div_ [id_ "hed_content", class_ "hidden a-tab-content"] do
                jsonValueToHtmlTree $ AE.object ["request_headers" AE..= httpJson.requestHeaders, "response_headers" AE..= httpJson.responseHeaders]
              div_ [id_ "par_content", class_ "hidden a-tab-content"] do
                jsonValueToHtmlTree $ AE.object ["query_params" AE..= httpJson.queryParams, "path_params" AE..= httpJson.pathParams]
        _ -> pass

    div_ [class_ "w-full mt-8", id_ "span-tabs-container"] do
      let spanErrors = getSpanErrors sp
      div_ [class_ "flex", [__|on click halt|]] $ do
        button_ [class_ "a-tab border-b-2 border-b-slate-200 px-4 py-1.5 t-tab-active", onclick_ "navigatable(this, '#att-content', '#span-tabs-container', 't-tab-active')"] "Attributes"
        button_ [class_ "a-tab border-b-2 border-b-slate-200 px-4 py-1.5 ", onclick_ "navigatable(this, '#meta-content', '#span-tabs-container', 't-tab-active')"] "Process"
        unless (null spanErrors) $ do
          button_ [class_ "a-tab border-b-2 border-b-slate-200 flex items-center gap-1 nowrap px-4 py-1.5 ", onclick_ "navigatable(this, '#errors-content', '#span-tabs-container', 't-tab-active')"] do
            "Errors"
            div_ [class_ "badge badge-error badge-sm"] $ show $ length spanErrors
        button_ [class_ "a-tab border-b-2 border-b-slate-200 flex items-center gap-1 px-4 py-1.5 ", onclick_ "navigatable(this, '#logs-content', '#span-tabs-container', 't-tab-active')"] $ do
          "Logs"
          div_ [class_ "badge badge-ghost badge-sm"] $ show $ numberOfEvents sp.events
        div_ [class_ "w-full border-b-2 border-b-slate-200"] pass

      div_ [class_ "grid my-4 text-slate-600 font"] $ do
        div_ [class_ "a-tab-content", id_ "att-content"] $ do
          jsonValueToHtmlTree sp.attributes
        div_ [class_ "hidden a-tab-content", id_ "meta-content"] $ do
          jsonValueToHtmlTree sp.resource
        div_ [class_ "hidden a-tab-content", id_ "errors-content"] $ do
          renderErrors spanErrors
        div_ [class_ "hidden a-tab-content", id_ "logs-content"] $ do
          jsonValueToHtmlTree $ AE.toJSON sp.events


renderErrors :: [AE.Value] -> Html ()
renderErrors errs = div_ [class_ "flex flex-col gap-1"] $ do
  forM_ errs $ \err -> do
    let (tye, message, stacktrace) = getErrorDetails err
    div_ [class_ "flex flex-col rounded-lg border overflow-hidden"] $ do
      div_ [class_ "bg-red-100 text-red-600 px-4 py-2 flex gap-2 items-center"] do
        span_ [class_ "font-bold"] $ toHtml (tye <> ":")
        span_ [] $ toHtml message
      div_ [] do
        p_ [class_ "whitespace-prewrap px-4 py-2"] $ toHtml stacktrace


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
      , ["request_body" AE..= req.requestBody]
      , ["request_headers" AE..= req.requestHeaders]
      , ["response_body" AE..= req.responseBody]
      , ["response_headers" AE..= req.responseHeaders]
      , ["service_version" AE..= req.serviceVersion]
      , ["status_code" AE..= req.statusCode]
      , ["tags" AE..= req.tags]
      , ["url_path" AE..= req.urlPath]
      ]
