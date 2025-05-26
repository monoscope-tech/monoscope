module Pages.Telemetry.Trace (traceH, TraceDetailsGet (..)) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.HashMap.Internal.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx (hxGet_, hxIndicator_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanStatus (SSError))
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Components (dateTime)
import Pages.Telemetry.Spans qualified as Spans
import Pages.Telemetry.Utils
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_, getDurationNSMS, getServiceColors, utcTimeToNanoseconds)


traceH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders TraceDetailsGet)
traceH pid trId spanIdM nav = do
  if isJust nav
    then do
      spanRecords' <- Telemetry.getSpandRecordsByTraceId pid trId
      let spanRecords = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> spanRecords'
      let sid = fromMaybe "" spanIdM
          targetSpan = fromMaybe (V.head spanRecords') (V.find (\x -> maybe False (\s -> s.span_id == Just sid) x.context) spanRecords')
          targetIndex = fromMaybe 0 (V.findIndex (\x -> maybe False (\s -> s.span_id == Just sid) x.context) spanRecords')
          prevSpan =
            if targetIndex > 0
              then Just (spanRecords V.! (targetIndex - 1))
              else Nothing
          nextSpan =
            if targetIndex < V.length spanRecords - 1
              then Just (spanRecords V.! (targetIndex + 1))
              else Nothing
      addRespHeaders $ SpanDetails pid targetSpan Nothing (prevSpan >>= \s -> Just s.spanId) (nextSpan >>= \s -> Just s.spanId)
    else do
      traceItemM <- Telemetry.getTraceDetails pid trId
      case traceItemM of
        Just traceItem -> do
          spanRecords' <- Telemetry.getSpandRecordsByTraceId pid trId
          let spanRecords = V.catMaybes $ Telemetry.convertOtelLogsAndSpansToSpanRecord <$> spanRecords'
              pageProps = PageProps pid traceItem spanRecords
          addRespHeaders $ TraceDetails pageProps
        Nothing -> addRespHeaders $ TraceDetailsNotFound "Trace not found"


data PageProps = PageProps
  { pid :: Projects.ProjectId
  , traceItem :: Telemetry.Trace
  , spanRecords :: V.Vector Telemetry.SpanRecord
  }


data TraceDetailsGet
  = TraceDetails PageProps
  | SpanDetails Projects.ProjectId Telemetry.OtelLogsAndSpans (Maybe Telemetry.OtelLogsAndSpans) (Maybe Text) (Maybe Text)
  | TraceDetailsNotFound Text


data ServiceData = ServiceData {name :: Text, duration :: Integer}


instance ToHtml TraceDetailsGet where
  toHtml (TraceDetails p) = toHtml $ tracePage p
  toHtml (SpanDetails pid s aptSpn left right) = toHtml $ Spans.expandedSpanItem pid s aptSpn left right
  toHtml (TraceDetailsNotFound msg) = toHtml msg
  toHtmlRaw = toHtml


tracePage :: PageProps -> Html ()
tracePage p = do
  let pid = p.pid
      traceItem = p.traceItem
      serviceData = V.toList $ getServiceData <$> p.spanRecords
      serviceNames = V.fromList $ ordNub $ (.name) <$> serviceData
      serviceColors = getServiceColors serviceNames
      rootSpans = buildSpanTree p.spanRecords
  div_ [class_ "w-full p-2", id_ "trace_span_container"] $ do
    div_ [class_ "flex flex-col w-full gap-4 pb-4"] $ do
      div_ [class_ "flex justify-between items-center"] do
        div_ [class_ "flex items-center gap-4"] $ do
          h3_ [class_ "whitespace-nowrap  font-semibold text-textStrong"] "Trace Breakdown"
        div_ [class_ "flex items-center gap-2"] $ do
          dateTime traceItem.traceStartTime (Just traceItem.traceEndTime)
          button_ [class_ "p-0 m-0", [__| on click add .hidden to #trace_expanded_view then call updateUrlState('showTrace', '', 'delete')|]] do
            faSprite_ "side-chevron-left-in-box" "regular" "w-5 h-5 text-textBrand rotate-180"

      div_ [class_ "flex gap-1 w-full mt-5"] $ do
        div_ [role_ "tablist", class_ "w-full flex flex-col gap-2", id_ "trace-tabs"] $ do
          div_ [class_ "flex flex-col gap-2"] do
            div_ [class_ "flex justify-between mb-2"] do
              div_ [class_ "flex items-center gap-2 text-slate-500 font-medium"] do
                button_ [class_ "a-tab text-sm px-3 py-1.5 border-b-2 border-b-transparent t-tab-active", onclick_ "navigatable(this, '#flame_graph', '#trace-tabs', 't-tab-active')"] "Flame Graph"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5", onclick_ "navigatable(this, '#water_fall', '#trace-tabs', 't-tab-active')"] "Waterfall"
                button_ [class_ "a-tab text-sm px-3 border-b-2 border-b-transparent py-1.5", onclick_ "navigatable(this, '#span_list', '#trace-tabs', 't-tab-active')"] "Spans List"
              div_ [class_ "flex items-center gap-2"] do
                stBox (show $ length p.spanRecords) Nothing
                stBox (show $ length $ V.filter (\s -> s.status == Just SSError) p.spanRecords) $ Just (faSprite_ "alert-triangle" "regular" "w-3 h-3 text-red-500")
                stBox (toText $ getDurationNSMS traceItem.traceDurationNs) $ Just (faSprite_ "clock" "regular" "w-3 h-3 text-textWeak")
            div_ [class_ "flex gap-2 w-full items-center"] do
              div_ [class_ "flex items-center gap-2 w-full rounded-xl px-3 grow-1 h-12 border border-slate-200 bg-fillWeaker"] do
                faSprite_ "magnifying-glass" "regular" "w-4 h-4 text-slate-500"
                input_
                  [ class_ "w-full py text-slate-950 bg-transparent hover:outline-hidden focus:outline-hidden"
                  , type_ "text"
                  , placeholder_ "Search"
                  , id_ "search-input"
                  , [__| on input show .span-filterble in #trace_span_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
                  ]
                let spanIds = decodeUtf8 $ AE.encode $ (.spanId) <$> p.spanRecords
                div_ [class_ "flex items-center gap-1", id_ "currentSpanIndex", term "data-span" "0"] do
                  button_
                    [ class_ "h-7 w-7 flex items-center justify-center bg-fillWeaker rounded-full font-bold border border-slate-200 text-slate-950  cursor-pointer"
                    , onclick_ [text|navigateSpans($spanIds, "prev")|]
                    ]
                    do
                      faSprite_ "chevron-up" "regular" "w-4 h-4"
                  button_
                    [ class_ "h-7 w-7 flex items-center justify-center rounded-full bg-fillWeaker font-bold border border-slate-200 text-slate-950 cursor-pointer"
                    , onclick_ [text|navigateSpans($spanIds, "next")|]
                    ]
                    do
                      faSprite_ "chevron-down" "regular" "h-4 w-4"
              button_ [class_ "btn border border-slate-200 bg-fillWeaker h-12"] "Reset Zoom"
          div_ [role_ "tabpanel", class_ "a-tab-content w-full", id_ "flame_graph"] do
            div_ [class_ "flex gap-2 w-full pt-2"] do
              div_
                [ class_ "w-[65%] group px-2 pt-4 border relative flex flex-col rounded-lg overflow-hidden"
                , id_ "flame-graph-container"
                ]
                do
                  div_ [class_ "w-full sticky top-0 border-b border-b-gray-300 h-6 text-xs relative", id_ "time-container"] pass
                  div_ [class_ "w-full overflow-x-hidden min-h-56 h-full relative", id_ $ "a" <> traceItem.traceId] pass
                  div_ [class_ "h-full top-0  absolute z-50 hidden", id_ "time-bar-indicator"] do
                    div_ [class_ "relative h-full"] do
                      div_ [class_ "text-xs top-[-18px] absolute -translate-x-1/2 whitespace-nowrap", id_ "line-time"] "2 ms"
                      div_ [class_ "h-[calc(100%-24px)] mt-[24px] w-[1px] bg-gray-200"] pass

              div_ [class_ "border rounded-lg w-[35%] overflow-x-hidden"] do
                h3_ [class_ "w-full flex p-3 font-medium justify-between items-center text-sm border-b"] do
                  span_ [] "Services"
                  span_ [] "Exec Time %"
                div_ [class_ "w-full overflow-x-hidden  text-gray-600", id_ $ "services-" <> traceItem.traceId] do
                  forM_ serviceNames $ \s -> do
                    let spans = filter (\x -> x.name == s) serviceData
                        duration = sum $ (.duration) <$> spans
                        allDur = sum $ (.duration) <$> serviceData
                        percent = show $ (fromIntegral duration / fromIntegral allDur) * 100
                        color = getServiceColor s serviceColors
                    div_ [class_ "flex items-center justify-between px-2 py-1"] $ do
                      div_ [class_ "flex gap-1 items-center"] $ do
                        div_ [class_ $ "w-3 h-3 rounded-sm " <> color] pass
                        span_ [class_ ""] $ toHtml s
                      div_ [class_ "flex gap-1 items-center"] $ do
                        span_ [class_ "text-xs max-w-52 truncate"] $ toHtml $ T.take 4 percent <> "%"
                        div_ [class_ "w-[100px] h-3 bg-gray-200 rounded-sm overflow-hidden"]
                          $ div_ [class_ $ "h-full pl-2 text-xs font-medium " <> color, style_ $ "width:" <> percent <> "%"] pass

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "water_fall"] do
            div_ [class_ "border border-slate-200 flex w-full rounded-2xl min-h-[230px]  overflow-y-auto overflow-x-hidden "] do
              div_ [class_ "w-full border-r overflow-x-hidden"] do
                div_ [class_ "border-b h-10 border-b-slate-200"] pass
                waterFallTree pid rootSpans traceItem.traceId serviceColors
              div_ [class_ "shrink-0 px-2"] do
                div_
                  [ class_ "w-[550px] sticky top-0 border-b border-b-slate-200 h-10 text-xs relative"
                  , id_ "waterfall-time-container"
                  ]
                  pass
                div_ [class_ "w-[550px] overflow-x-hidden py-2 relative flex flex-col gap-2", id_ $ "waterfall-" <> traceItem.traceId] pass

          div_ [role_ "tabpanel", class_ "a-tab-content pt-2 hidden", id_ "span_list"] do
            div_ [class_ "border border-slate-200 w-full rounded-2xl min-h-[230px] overflow-x-hidden "] do
              renderSpanListTable serviceNames serviceColors p.spanRecords

  let spanJson = decodeUtf8 $ AE.encode $ p.spanRecords <&> getSpanJson
  let waterFallJson = decodeUtf8 $ AE.encode rootSpans

  let colorsJson = decodeUtf8 $ AE.encode $ AE.object [AEKey.fromText k AE..= v | (k, v) <- HM.toList serviceColors]
  let trId = traceItem.traceId
  script_
    [text|
      function navigateSpans(spans, direction) {
         const container = document.querySelector('#currentSpanIndex')
         const currentSpan = Number(container.dataset.span)
         if (direction == 'next' && currentSpan >= spans.length) {
           return
         }
         if (direction == 'prev' && currentSpan <= 0) {
           return
         }
         const spandInd = direction == 'next' ? currentSpan + 1 : currentSpan - 1
         const span = spans[spandInd]
         container.dataset.span = spandInd
         htmx.trigger('#trigger-span-' + span, 'click')
      }
  |]
  script_ [text|flameGraphChart($spanJson, "a$trId", $colorsJson);|]
  script_ [text|waterFallGraphChart($waterFallJson, "waterfall-$trId", $colorsJson);|]


getSpanJson :: Telemetry.SpanRecord -> AE.Value
getSpanJson sp =
  AE.object
    [ "span_id" AE..= sp.spanId
    , "name" AE..= sp.spanName
    , "value" AE..= sp.spanDurationNs
    , "start" AE..= start
    , "parent_id" AE..= sp.parentSpanId
    , "service_name" AE..= getServiceName sp.resource
    , "has_errors" AE..= spanHasErrors sp
    ]
  where
    start = utcTimeToNanoseconds sp.startTime


renderSpanRecordRow :: V.Vector Telemetry.SpanRecord -> HashMap Text Text -> Text -> Html ()
renderSpanRecordRow spanRecords colors service = do
  let totalDuration = sum $ (.spanDurationNs) <$> spanRecords
  let filterRecords = V.filter (\x -> getServiceName x.resource == service) spanRecords
  let listLen = V.length filterRecords
  let duration = sum $ (.spanDurationNs) <$> filterRecords
  tr_
    [ class_ "w-full overflow-x-hidden p-2 cursor-pointer font-medium hover:bg-gray-100 border-b-2 last:border-b-0"
    , [__|on click toggle .hidden on next <tr/> then toggle .rotate-90 on the first <svg/> in the first <td/> in me|]
    ]
    do
      td_ [class_ "ml-1 px-2 py-1 w-[600px] text-slate-950 truncate flex items-center gap-1 font-medium"] do
        div_ [class_ "w-1 bg-blue-200 h-4"] pass
        faSprite_ "chevron-right" "regular" "h-3 w-3 mr-2 text-gray-500"
        div_ [class_ $ "w-3 h-3 rounded-sm " <> getServiceColor service colors] pass
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
    tbody_ [class_ "space-y-0"]
      $ mapM_ (renderSpanRecordRow records colors) services


spanTable :: V.Vector Telemetry.SpanRecord -> Html ()
spanTable records =
  div_ [class_ "rounded-xl my-2 mx-3 border border-slate-200"] do
    table_ [class_ "table w-full"] do
      thead_ [class_ "border-b border-slate-200"]
        $ tr_ [class_ "p-2 border-b font-medium"]
        $ do
          td_ "Time"
          td_ "Span name"
          td_ "Event type"
          td_ "Span kind"
          td_ "Exec. time"
      tbody_ do
        forM_ records $ \spanRecord -> do
          let pidText = UUID.toText spanRecord.projectId
              spanid = UUID.toText spanRecord.uSpanId
              tme = fromString (formatShow iso8601Format spanRecord.timestamp)
              (reqType, _, _, _) = fromMaybe ("", "", "", 0) $ getRequestDetails spanRecord.attributes
          tr_
            [ hxGet_ $ "/p/" <> pidText <> "/log_explorer/" <> spanid <> "/" <> tme <> "/detailed?source=spans"
            , hxTarget_ "#log_details_container"
            , hxSwap_ "innerHTML"
            , id_ $ "sp-list-" <> spanRecord.spanId
            , class_ "span-filterble font-medium"
            , hxIndicator_ "#loading-span-list"
            ]
            $ do
              td_ $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" spanRecord.timestamp
              td_ $ toHtml spanRecord.spanName
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml reqType
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ T.drop 2 $ maybe "" show spanRecord.kind
              td_ do
                span_ [class_ "cbadge-sm badge-neutral"] $ toHtml $ getDurationNSMS spanRecord.spanDurationNs


getServiceData :: Telemetry.SpanRecord -> ServiceData
getServiceData sp = ServiceData{name = getServiceName sp.resource, duration = sp.spanDurationNs}


stBox :: Text -> Maybe (Html ()) -> Html ()
stBox value iconM =
  div_ [class_ "flex items-center px-2 gap-2 border-r last:border-r-0"] do
    whenJust iconM $ id
    span_ [class_ "text-textStrong text-sm"] $ toHtml value
    if isNothing iconM then span_ [class_ "font-medium text-textWeak text-xs"] "Spans" else pass


data SpanMin = SpanMin
  { parentSpanId :: Maybe Text
  , spanId :: Text
  , spanName :: Text
  , spanDurationNs :: Integer
  , uSpanId :: UUID.UUID
  , hasErrors :: Bool
  , serviceName :: Text
  , startTime :: Integer
  , endTime :: Maybe Integer
  , timestamp :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data SpanTree = SpanTree
  { spanRecord :: SpanMin
  , children :: [SpanTree]
  }
  deriving (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


buildSpanMap :: V.Vector Telemetry.SpanRecord -> Map (Maybe Text) [Telemetry.SpanRecord]
buildSpanMap = V.foldr (\sp m -> Map.insertWith (++) sp.parentSpanId [sp] m) Map.empty


buildTree :: Map (Maybe Text) [Telemetry.SpanRecord] -> Maybe Text -> [SpanTree]
buildTree spanMap parentId =
  case Map.lookup parentId spanMap of
    Nothing -> []
    Just spans ->
      [ SpanTree
          SpanMin
            { parentSpanId = sp.parentSpanId
            , spanId = sp.spanId
            , uSpanId = sp.uSpanId
            , spanName = sp.spanName
            , spanDurationNs = sp.spanDurationNs
            , serviceName = getServiceName sp.resource
            , startTime = utcTimeToNanoseconds sp.startTime
            , endTime = utcTimeToNanoseconds <$> sp.endTime
            , hasErrors = spanHasErrors sp
            , timestamp = sp.timestamp
            }
          (buildTree spanMap (Just sp.spanId))
      | sp <- spans
      ]


buildSpanTree :: V.Vector Telemetry.SpanRecord -> [SpanTree]
buildSpanTree spans =
  let spanMap = buildSpanMap spans
   in buildTree spanMap Nothing


waterFallTree :: Projects.ProjectId -> [SpanTree] -> Text -> HashMap Text Text -> Html ()
waterFallTree pid records trId scols = do
  div_ [class_ "pl-2 py-2 flex flex-col gap-2"] do
    forM_ (zip [0 ..] records) \(i, c) -> do
      buildTree_ pid c trId 0 scols True


buildTree_ :: Projects.ProjectId -> SpanTree -> Text -> Int -> HashMap Text Text -> Bool -> Html ()
buildTree_ pid sp trId level scol isLasChild = do
  let hasChildren = not $ null sp.children
      serviceCol = getServiceColor sp.spanRecord.serviceName scol
  -- let str = "on click toggle .hidden on the next .children_container then toggle .collapsed on me then toggle .hidden on  #waterfall-child-" <> sp.spanRecord.spanId
  div_ [class_ "flex items-start w-full relative span-filterble"] do
    when (level /= 0) $ div_ [class_ "w-4 shrink-0 ml-2 h-[1px] mt-2 bg-slate-200"] pass
    unless (level == 0) $ div_ [class_ "absolute -top-3 left-2 border-l h-5 border-l-slate-200"] pass
    unless isLasChild $ div_ [class_ "absolute top-1 left-2 border-l h-full border-l-slate-200"] pass
    div_ [class_ "flex flex-col w-full grow-1 shrink-1 border-slate-200 relative"] do
      when hasChildren $ div_ [class_ "absolute top-1 left-2 border-l h-2 border-l-slate-200"] pass
      let tme = fromString (formatShow iso8601Format sp.spanRecord.timestamp)
          spanId = UUID.toText sp.spanRecord.uSpanId
      div_
        [ class_ "w-full cursor-pointer flex justify-between max-w-full items-center h-5 collapsed"
        , hxGet_ $ "/p/" <> pid.toText <> "/log_explorer/" <> spanId <> "/" <> tme <> "/detailed?source=spans"
        , hxTarget_ "#log_details_container"
        , hxSwap_ "innerHTML"
        , hxIndicator_ "#loading-span-list"
        , id_ $ "trigger-span-" <> sp.spanRecord.spanId
        ]
        do
          div_ [class_ "flex items-center w-[95%] gap-1 border-blue-300 rounded-lg overflow-x-hidden waterfall-item", [__|on click remove .border from .waterfall-item then add .border to me|]] do
            when hasChildren $ do
              div_ [class_ "border border-slate-200 w-7 flex justify-between gap-1 items-center rounded-sm p-0.5"] do
                faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 font-bold text-slate-950 waterfall-item-tree-chevron"
                span_ [class_ "text-xs"] $ toHtml $ show (length sp.children)
            span_ [class_ "font-medium text-slate-950 "] $ toHtml sp.spanRecord.serviceName
            faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 text-slate-950"
            span_ [class_ "text-slate-500 text-sm whitespace-nowrap"] $ toHtml sp.spanRecord.spanName
          span_ [class_ $ "w-1 rounded-sm h-5 shrink-0 " <> serviceCol] ""
      when hasChildren $ do
        div_ [class_ "flex flex-col children_container gap-2 mt-2", id_ $ "waterfall-tree-" <> sp.spanRecord.spanId] do
          forM_ (zip [0 ..] sp.children) \(i, c) -> do
            buildTree_ pid c trId (level + 1) scol (i == length sp.children - 1)
