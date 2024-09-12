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
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.Traces.Spans qualified as Spans
import Pages.Traces.Utils
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getDurationNSMS, getGrpcStatusColor, getServiceColors, getStatusColor, utcTimeToNanoseconds)


traceH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId spanIdM = do
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
  | TraceDetailsNotFound Text


data ServiceData = ServiceData {name :: Text, duration :: Integer}


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails p) = toHtml $ tracePage p
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


tracePage :: PageProps -> Html ()
tracePage p = do
  let pid = p.pid
      traceItem = p.traceItem
      sId = p.span_id
      serviceData = V.toList $ getServiceData <$> p.spanRecords
      serviceNames = V.fromList $ ordNub $ (.name) <$> serviceData
      reqDetails = getRequestDetails (V.head p.spanRecords)
      serviceColors = getServiceColors serviceNames
  div_ [class_ "w-full h-full"] $ do
    div_ [class_ "flex flex-col w-full gap-4 h-full pb-4"] $ do
      div_ [class_ "flex items-center gap-4"] $ do
        h3_ [class_ "whitespace-nowrap text-xl font-bold pr-4 border-r border-r-2"] "Trace"
        div_ [class_ "flex items-center gap-4"] $ do
          h4_ [class_ "text-xl font-medium"] $ toHtml $ if not (null serviceNames) then V.head serviceNames else "Unknown Service"
          faSprite_ "arrow-right" "regular" "w-4 h-4 font-bold"
          h4_ [class_ "text-xl font-medium"] $ toHtml $ if not (null p.spanRecords) then (V.head p.spanRecords).spanName else "Unknown Span"
        div_ [class_ "flex items-end border rounded"] do
          span_ [class_ "text-sm text-gray-500 font-medium border-r px-2 py-1"] "Trace ID"
          span_ [class_ "text-sm px-2 py-1"] $ toHtml traceItem.traceId
      div_ [class_ "flex gap-4 items-center justify-between text-gray-600"] $ do
        div_ [class_ "flex gap-4 items-center"] do
          div_ [class_ "font-medium flex shrink-0 items-center rounded gap-1 border px-2 py-1.5 text-gray-600"] do
            faSprite_ "clock" "regular" "w-3 h-3"
            span_ [class_ "text-sm font-medium"] $ toHtml $ getDurationNSMS traceItem.traceDurationNs
          div_ [class_ "flex items-center gap-4"] do
            whenJust reqDetails $ \case
              ("HTTP", method, path, status) -> do
                span_ [class_ "text-sm font-medium border rounded px-2 py-1.5"] "HTTP"
                div_ [class_ "flex border rounded overflow-hidden"] do
                  span_ [class_ "text-sm px-2 py-1.5 border-r bg-gray-200"] $ toHtml method
                  span_ [class_ "text-sm px-2 py-1.5 max-w-96 truncate"] $ toHtml path
                  let extraClass = getStatusColor status
                  span_ [class_ $ "text-sm px-2 py-1.5 " <> extraClass] $ toHtml $ T.take 3 $ show status
              (scheme, method, path, status) -> do
                span_ [class_ "text-sm font-medium border rounded px-2 py-1.5"] $ toHtml scheme
                div_ [class_ "flex border rounded overflow-hidden"] do
                  span_ [class_ "text-sm px-2 py-1.5 max-w-44 truncate bg-gray-200 border-r"] $ toHtml method
                  span_ [class_ "text-sm px-2 py-1.5 max-w-96 truncate"] $ toHtml path
                  span_ [class_ "text-sm px-2 py-1.5 border-l"] $ toHtml $ show status

        span_ [class_ "text-sm"] $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" traceItem.traceStartTime

      div_ [class_ "flex gap-1 w-full mt-5"] $ do
        div_ [class_ "w-full"] do
          div_ [role_ "tablist", class_ "tabs tabs-bordered bg-white"] $ do
            input_ [type_ "radio", name_ "my_tabs_2", role_ "tab", class_ "tab after:pb-2", term "aria-label" "Flame Graph", checked_]
            div_ [role_ "tabpanel", class_ "tab-content w-full bg-white"] do
              div_ [class_ "flex gap-2 w-full pt-2"] do
                div_
                  [ class_ "w-[65%] group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                  , id_ "flame-graph-container"
                  ]
                  do
                    div_ [class_ "w-full sticky top-0 border-b border-b-gray-300 h-6 text-xs relative", id_ $ "time-container"] pass
                    div_ [class_ "w-full overflow-x-hidden h-48 c-scroll relative", id_ $ "a" <> traceItem.traceId] pass
                    div_ [class_ "h-full top-0  absolute z-50 hidden", id_ "time-bar-indicator"] do
                      div_ [class_ "relative h-full"] do
                        div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ "line-time"] "2 ms"
                        div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-gray-200"] pass

                div_ [class_ "border rounded-lg w-[35%] overflow-x-hidden"] do
                  h3_ [class_ "w-full flex p-2 font-medium justify-between items-center border-b"] do
                    span_ [] "Services"
                    span_ [] "Exec Time %"
                  div_ [class_ "w-full h-[200px] overflow-x-hidden text-sm text-gray-600 overflow-y-auto c-scroll", id_ $ "services-" <> traceItem.traceId] do
                    forM_ serviceNames $ \s -> do
                      let spans = filter (\x -> x.name == s) serviceData
                          duration = sum $ (.duration) <$> spans
                          allDur = sum $ (.duration) <$> serviceData
                          percent = show $ (fromIntegral duration / fromIntegral allDur) * 100
                          color = getServiceColor s serviceColors
                      div_ [class_ "flex items-center justify-between px-2 py-1"] $ do
                        div_ [class_ "flex gap-1 items-center"] $ do
                          div_ [class_ "w-3 h-3 rounded", style_ $ "background-color:" <> color] pass
                          span_ [class_ ""] $ toHtml s
                        div_ [class_ "flex gap-1 items-center"] $ do
                          span_ [class_ "text-xs max-w-52 truncate"] $ toHtml $ T.take 4 percent <> "%"
                          div_ [class_ "w-[100px] h-3 bg-gray-200 rounded overflow-hidden"] $
                            div_ [class_ "h-full pl-2 text-xs font-medium", style_ $ "width:" <> percent <> "%; background-color:" <> color] pass

            input_ [type_ "radio", name_ "my_tabs_2", role_ "tab", class_ "tab after:pb-2", term "aria-label" "Span List"]
            div_ [role_ "tabpanel", class_ "tab-content pt-2"] do
              div_ [class_ "border w-full rounded-lg min-h-[230px] max-h-[330px] overflow-auto overflow-x-hidden "] do
                renderSpanListTable serviceNames serviceColors p.spanRecords

      div_ [class_ "my-5 py-2 rounded-lg border"] do
        div_ [class_ "flex flex-col gap-4 px-4", id_ $ "span-" <> traceItem.traceId] do
          let tSp = fromMaybe (V.head p.spanRecords) (V.find (\s -> s.spanId == sId) p.spanRecords)
          Spans.expandedSpanItem pid tSp
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
      td_ [class_ "px-2 py-1 w-[600px] truncate flex items-center gap-1"] do
        faSprite_ "chevron-right" "regular" "h-3 w-3 mr-2 text-gray-500"
        div_ [class_ "w-3 h-3 rounded", style_ $ "background-color:" <> getServiceColor service colors] pass
        span_ [] $ toHtml service
      td_ [class_ "px-2 py-1 max-w-48 truncate pl-4"] $ toHtml $ show listLen
      td_ [class_ "px-2 py-1 max-w-48 truncate pl-4"] $ toHtml $ getDurationNSMS $ duration `div` toInteger listLen
      td_ [class_ "px-2 py-1 max-w-48 truncate pl-4"] $ toHtml $ getDurationNSMS duration
      td_ [class_ "px-2 py-1 max-w-48 truncate pl-4"] $ toHtml $ show (duration * 100 `div` totalDuration) <> "%"
  tr_ [class_ "hidden p-0 m-0", [__|on click halt|]] do
    td_ [colspan_ "5", class_ "pl-[13px] overflow-x-hidden"] do
      spanTable filterRecords


renderSpanListTable :: V.Vector Text -> HashMap Text Text -> V.Vector Telemetry.SpanRecord -> Html ()
renderSpanListTable services colors records =
  table_ [class_ "w-full table table-sm overflow-x-hidden"] $ do
    thead_ [class_ "border-b bg-gray-100"] $ do
      tr_ [class_ "p-2 border-b font-normal bg-gray-100"] $ do
        th_ "Resource"
        th_ "Spans"
        th_ "Avg. Duration"
        th_ "Exec. Time"
        th_ "%Exec. Time"
    tbody_ [class_ "space-y-0"] $
      mapM_ (renderSpanRecordRow records colors) services


spanTable :: V.Vector Telemetry.SpanRecord -> Html ()
spanTable records =
  div_ [class_ "border-l-2 flex flex-col pb-2 gap-1"] do
    div_
      [ class_ "bg-white pl-2 w-full text-xs font-medium  bg-gray-50 pb-1 text-gray-500 overflow-x-hidden items-center flex gap-3 cursor-pointer flex-nowrap  border-b border-b-gray-50"
      ]
      $ do
        span_ [class_ "px-2 w-[200px] truncate"] "Time"
        span_ [class_ "px-2 w-[400px] truncate"] "Span Name"
        span_ [class_ "px-2 w-28 truncate"] "Event Type"
        span_ [class_ "px-2 w-28 truncate"] "Span Kind"
        span_ [class_ "px-1 w-16 text-center "] "Status"
        span_ [class_ "px-2 w-28 truncate"] "Exec. Time"
    forM_ records $ \spanRecord -> do
      let pidText = UUID.toText spanRecord.projectId
          spanid = maybe "" UUID.toText spanRecord.uSpandId
          tme = fromString (formatShow iso8601Format spanRecord.timestamp)
          (reqType, _, _, status_code) = fromMaybe ("", "", "", 0) $ getRequestDetails spanRecord

      div_
        [ class_ "bg-white pl-2 w-full overflow-x-hidden text-gray-700 items-center flex gap-3 cursor-pointer text-sm flex-nowrap hover:bg-gray-100 border-b border-b-gray-50 last:border-b-0"
        , hxGet_ $ "/p/" <> pidText <> "/log_explorer/" <> spanid <> "/" <> tme <> "/detailed?source=spans"
        , hxTarget_ $ "#span-" <> spanRecord.traceId
        , hxSwap_ "innerHTML"
        , id_ $ "sp-list-" <> spanRecord.spanId
        ]
        $ do
          span_ [class_ "px-2 py-1 w-[200px] truncate"] $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" spanRecord.timestamp
          span_ [class_ "px-2 py-1 w-[400px] truncate"] $ toHtml spanRecord.spanName
          span_ [class_ "px-2 py-1 w-28 truncate"] $ toHtml reqType
          span_ [class_ "px-2 py-1 w-28 truncate"] $ toHtml $ T.drop 2 $ maybe "" show spanRecord.kind
          let xcls = getStatusColor status_code
              gcls = getGrpcStatusColor status_code
              fcls = if reqType == "HTTP" then xcls else gcls
          span_ [class_ $ "p-1 w-16 text-center " <> fcls] $ toHtml $ show status_code
          span_ [class_ "px-2 py-1 w-28 truncate"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


getServiceData :: Telemetry.SpanRecord -> ServiceData
getServiceData sp = ServiceData{name = getServiceName sp, duration = sp.spanDurationNs}
