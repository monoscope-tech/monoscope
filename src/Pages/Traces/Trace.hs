module Pages.Traces.Trace (traceH, TraceDetailsGet (..)) where

import Data.Aeson ((.=))
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.HashMap.Internal.Strict qualified as HM
import Data.Text qualified as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Components (dateTime)
import Pages.Traces.Spans qualified as Spans
import Pages.Traces.Utils
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getDurationNSMS, getGrpcStatusColor, getServiceColors, getStatusColor, utcTimeToNanoseconds)


traceH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId spanIdM nav = do
  if isJust nav
    then do
      spanRecords <- Telemetry.getSpandRecordsByTraceId pid trId
      let sid = fromMaybe "" spanIdM
          targetSpan = fromMaybe (V.head spanRecords) (V.find (\x -> x.spanId == sid) spanRecords)
          targetIndex = fromMaybe 0 (V.findIndex (\x -> x.spanId == sid) spanRecords)
          prevSpan =
            if targetIndex > 0
              then Just (spanRecords V.! (targetIndex - 1))
              else Nothing
          nextSpan =
            if targetIndex < V.length spanRecords - 1
              then Just (spanRecords V.! (targetIndex + 1))
              else Nothing
      addRespHeaders $ SpanDetails pid targetSpan (prevSpan >>= \s -> Just s.spanId) (nextSpan >>= \s -> Just s.spanId)
    else do
      traceItemM <- Telemetry.getTraceDetails pid trId
      case traceItemM of
        Just traceItem -> do
          spanRecords <- Telemetry.getSpandRecordsByTraceId pid trId
          let span_id = fromMaybe "" $ if isJust spanIdM then spanIdM else Just ""
          let pageProps = PageProps pid traceItem span_id spanRecords
          addRespHeaders $ TraceDetails pageProps
        Nothing -> addRespHeaders $ TraceDetailsNotFound "Trace not found"


data PageProps = PageProps
  { pid :: Projects.ProjectId
  , traceItem :: Telemetry.Trace
  , span_id :: Text
  , spanRecords :: V.Vector Telemetry.SpanRecord
  }


data TraceDetailsGet
  = TraceDetails PageProps
  | SpanDetails Projects.ProjectId Telemetry.SpanRecord (Maybe Text) (Maybe Text)
  | TraceDetailsNotFound Text


data ServiceData = ServiceData {name :: Text, duration :: Integer}


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails p) = toHtml $ tracePage p
  toHtml (SpanDetails pid s left right) = toHtml $ Spans.expandedSpanItem pid s left right
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


tracePage :: PageProps -> Html ()
tracePage p = do
  let pid = p.pid
      traceItem = p.traceItem
      sId = p.span_id
      tSp = fromMaybe (V.head p.spanRecords) (V.find (\s -> s.spanId == sId) p.spanRecords)
      serviceData = V.toList $ getServiceData <$> p.spanRecords
      serviceNames = V.fromList $ ordNub $ (.name) <$> serviceData
      serviceColors = getServiceColors serviceNames
  div_ [class_ "w-full h-full pt-2", id_ "trace_span_container"] $ do
    div_ [class_ "flex flex-col w-full gap-4 h-full pb-4"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "whitespace-nowrap text-lg font-medium text-slate-950"] "Trace"
          div_ [class_ "flex items-center border border-slate-200 rounded-lg"] do
            span_ [class_ "text-sm text-slate-950 font-medium border-r border-r-slate-200 px-2 py-1.5"] "Trace ID"
            span_ [class_ "text-slate-600 text-sm font-medium px-2 py-1.5"] $ toHtml traceItem.traceId
            faSprite_ "copy" "regular" "w-3 h-3 mr-2 text-slate-500"
          div_ [class_ "flex items-center gap-1"] do
            button_
              [ class_ "cursor-pointer h-8 w-8 flex items-center justify-center rounded-full bg-slate-100 border border-slate-200 text-slate-500"
              , hxGet_ $ "/p/" <> pid.toText <> "/traces/" <> traceItem.traceId <> "/?span_id=" <> tSp.spanId <> "&nav=true"
              , hxSwap_ "innerHTML"
              , hxTarget_ "#trace_span_container"
              , hxTrigger_ "click"
              ]
              $ faSprite_ "chevron-left" "regular" "w-4 h-4"
            button_
              [ class_ "cursor-pointer h-8 w-8 flex items-center justify-center rounded-full bg-slate-100 border border-slate-200 text-slate-500"
              , hxGet_ $ "/p/" <> pid.toText <> "/traces/" <> traceItem.traceId <> "/?span_id=" <> tSp.spanId <> "&nav=true"
              , hxSwap_ "innerHTML"
              , hxTarget_ "#trace_span_container"
              , hxTrigger_ "click"
              ]
              $ faSprite_ "chevron-right" "regular" "w-4 h-4"
        dateTime traceItem.traceStartTime

      div_ [class_ "flex gap-1 w-full mt-5"] $ do
        div_ [role_ "tablist", class_ "w-full", id_ "trace-tabs"] $ do
          div_ [class_ "flex justify-between mb-2"] do
            div_ [class_ "flex items-center gap-2 text-slate-500 font-medium"] do
              button_ [class_ "a-tab text-sm px-3 py-1.5 border-b-2 border-b-transparent t-tab-active", onclick_ "navigatable(this, '#flame_graph', '#trace-tabs', 't-tab-active')"] "Flame Graph"
              button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5", onclick_ "navigatable(this, '#water_fall', '#trace-tabs', 't-tab-active')"] "Waterfall"
              button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5", onclick_ "navigatable(this, '#span_list', '#trace-tabs', 't-tab-active')"] "Spans List"
            div_ [class_ "flex items-center gap-2"] do
              stBox "Spans" (show $ length p.spanRecords)
              stBox "Errors" "0"
              stBox "Total duration" (toText $ getDurationNSMS traceItem.traceDurationNs)
          div_ [role_ "tabpanel", class_ "a-tab-content w-full bg-white", id_ "flame_graph"] do
            div_ [class_ "flex gap-2 w-full pt-2"] do
              div_
                [ class_ "w-[65%] group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                , id_ "flame-graph-container"
                ]
                do
                  div_ [class_ "w-full sticky top-0 border-b border-b-gray-300 h-6 text-xs relative", id_ "time-container"] pass
                  div_ [class_ "w-full overflow-x-hidden h-56 c-scroll relative", id_ $ "a" <> traceItem.traceId] pass
                  div_ [class_ "h-full top-0  absolute z-50 hidden", id_ "time-bar-indicator"] do
                    div_ [class_ "relative h-full"] do
                      div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ "line-time"] "2 ms"
                      div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-gray-200"] pass

              div_ [class_ "border rounded-lg w-[35%] overflow-x-hidden"] do
                h3_ [class_ "w-full flex p-3 font-medium justify-between items-center text-sm border-b"] do
                  span_ [] "Services"
                  span_ [] "Exec Time %"
                div_ [class_ "w-full h-[200px] overflow-x-hidden  text-gray-600 overflow-y-auto c-scroll", id_ $ "services-" <> traceItem.traceId] do
                  forM_ serviceNames $ \s -> do
                    let spans = filter (\x -> x.name == s) serviceData
                        duration = sum $ (.duration) <$> spans
                        allDur = sum $ (.duration) <$> serviceData
                        percent = show $ (fromIntegral duration / fromIntegral allDur) * 100
                        color = getServiceColor s serviceColors
                    div_ [class_ "flex items-center justify-between px-2 py-1"] $ do
                      div_ [class_ "flex gap-1 items-center"] $ do
                        div_ [class_ $ "w-3 h-3 rounded " <> color] pass
                        span_ [class_ ""] $ toHtml s
                      div_ [class_ "flex gap-1 items-center"] $ do
                        span_ [class_ "text-xs max-w-52 truncate"] $ toHtml $ T.take 4 percent <> "%"
                        div_ [class_ "w-[100px] h-3 bg-gray-200 rounded overflow-hidden"] $
                          div_ [class_ $ "h-full pl-2 text-xs font-medium " <> color, style_ $ "width:" <> percent <> "%"] pass

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "water_fall"] do
            div_ [class_ "border w-full rounded-xl min-h-[230px] max-h-[330px] overflow-auto overflow-x-hidden "] do
              renderSpanListTable serviceNames serviceColors p.spanRecords

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "span_list"] do
            div_ [class_ "border w-full rounded-2xl min-h-[230px] max-h-[330px] overflow-auto overflow-x-hidden "] do
              renderSpanListTable serviceNames serviceColors p.spanRecords

      div_ [class_ "my-5 py-2 rounded-lg border"] do
        div_ [class_ "flex flex-col gap-4", id_ $ "span-" <> traceItem.traceId] do
          Spans.expandedSpanItem pid tSp Nothing Nothing
  let spanJson = decodeUtf8 $ AE.encode $ p.spanRecords <&> getSpanJson
  let colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k .= v | (k, v) <- HM.toList serviceColors]
  let trId = traceItem.traceId
  script_ [text|flameGraphChart($spanJson, "a$trId", $colorsJson);|]


getSpanJson :: Telemetry.SpanRecord -> AE.Value
getSpanJson sp =
  AE.object
    [ "span_id" .= sp.spanId
    , "name" .= sp.spanName
    , "value" .= sp.spanDurationNs
    , "start" .= utcTimeToNanoseconds sp.startTime
    , "parent_id" .= sp.parentSpanId
    , "service_name" .= getServiceName sp
    ]


renderSpanRecordRow :: V.Vector Telemetry.SpanRecord -> HashMap Text Text -> Text -> Html ()
renderSpanRecordRow spanRecords colors service = do
  let totalDuration = sum $ (.spanDurationNs) <$> spanRecords
  let filterRecords = V.filter (\x -> getServiceName x == service) spanRecords
  let listLen = V.length filterRecords
  let duration = sum $ (.spanDurationNs) <$> filterRecords
  tr_
    [ class_ "bg-white w-full overflow-x-hidden p-2 cursor-pointer font-medium hover:bg-gray-100 border-b-2 last:border-b-0"
    , [__|on click toggle .hidden on next <tr/> then toggle .rotate-90 on the first <svg/> in the first <td/> in me|]
    ]
    do
      td_ [class_ "ml-1 px-2 py-1 w-[600px] text-slate-950 truncate flex items-center gap-1 font-medium"] do
        div_ [class_ "w-1 bg-blue-200 h-4"] pass
        faSprite_ "chevron-right" "regular" "h-3 w-3 mr-2 text-gray-500"
        div_ [class_ $ "w-3 h-3 rounded " <> getServiceColor service colors] pass
        span_ [] $ toHtml service
      td_ [class_ "px-2 py-1 max-w-48 text-slate-500 truncate pl-4"] $ toHtml $ show listLen
      td_ [class_ "px-2 py-1 max-w-48 text-slate-500 truncate pl-4"] $ toHtml $ getDurationNSMS $ duration `div` toInteger listLen
      td_ [class_ "px-2 py-1 max-w-48 text-slate-500 truncate pl-4"] $ toHtml $ getDurationNSMS duration
      td_ [class_ "px-2 py-1 max-w-48 text-slate-500 truncate pl-4"] $ toHtml $ show (duration * 100 `div` totalDuration) <> "%"
  tr_ [class_ "hidden p-0 m-0", [__|on click halt|]] do
    td_ [colspan_ "5", class_ "pl-[13px] overflow-x-hidden"] do
      spanTable filterRecords


renderSpanListTable :: V.Vector Text -> HashMap Text Text -> V.Vector Telemetry.SpanRecord -> Html ()
renderSpanListTable services colors records =
  table_ [class_ "w-full table table-sm overflow-x-hidden"] $ do
    thead_ [class_ "border-b"] $ do
      tr_ [class_ "p-2 border-b font-normal"] $ do
        th_ "Resource"
        th_ "Spans"
        th_ "Avg. Duration"
        th_ "Exec. Time"
        th_ "%Exec. Time"
    tbody_ [class_ "space-y-0"] $
      mapM_ (renderSpanRecordRow records colors) services


spanTable :: V.Vector Telemetry.SpanRecord -> Html ()
spanTable records =
  table_ [class_ "table table-sm w-full"] do
    thead_ $
      tr_ [class_ "p-2 border-b font-normal"] $ do
        td_ "Time"
        td_ "Span Name"
        td_ "Event Type"
        td_ "Span Kind"
        td_ "Status"
        td_ "Exec. Time"
    tbody_ do
      forM_ records $ \spanRecord -> do
        let pidText = UUID.toText spanRecord.projectId
            spanid = maybe "" UUID.toText spanRecord.uSpandId
            tme = fromString (formatShow iso8601Format spanRecord.timestamp)
            (reqType, _, _, status_code) = fromMaybe ("", "", "", 0) $ getRequestDetails spanRecord
        tr_
          [ hxGet_ $ "/p/" <> pidText <> "/log_explorer/" <> spanid <> "/" <> tme <> "/detailed?source=spans"
          , hxTarget_ $ "#span-" <> spanRecord.traceId
          , hxSwap_ "innerHTML"
          , id_ $ "sp-list-" <> spanRecord.spanId
          ]
          $ do
            td_ $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" spanRecord.timestamp
            td_ $ toHtml spanRecord.spanName
            td_ $ toHtml reqType
            td_ $ toHtml $ T.drop 2 $ maybe "" show spanRecord.kind
            let xcls = getStatusColor status_code
                gcls = getGrpcStatusColor status_code
                fcls = if reqType == "HTTP" then xcls else gcls
            td_ do
              span_ [class_ fcls] $ toHtml $ show status_code
            td_ do
              span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


getServiceData :: Telemetry.SpanRecord -> ServiceData
getServiceData sp = ServiceData{name = getServiceName sp, duration = sp.spanDurationNs}


stBox :: Text -> Text -> Html ()
stBox title value =
  div_ [class_ "flex items-end px-2 gap-2 border-r  last:border-r-0"] do
    span_ [class_ "text-slate-950 font-medium"] $ toHtml value
    span_ [class_ "font-medium text-slate-500 text-sm"] $ toHtml title
