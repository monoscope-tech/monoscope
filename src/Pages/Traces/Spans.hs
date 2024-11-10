module Pages.Traces.Spans (expandedSpanItem, spanLatencyBreakdown, spanGetH) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanRecord (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Components (dateTime)
import Pages.Traces.Utils (getRequestDetails, getServiceName)
import Relude
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
  div_ [class_ "w-full pb-2 relative"] $ do
    span_ [class_ "htmx-indicator query-indicator absolute loading left-1/2 -translate-x-1/2 loading-dots absoute z-10 top-10", id_ "loading-span-list"] ""
    div_ [class_ "flex flex-col gap-1 bg-gray-50 py-2  px-4"] $ do
      div_ [class_ "flex flex-col w-full gap-4 h-full pb-4"] $ do
        div_ [class_ "flex justify-between items-center"] do
          div_ [class_ "flex items-center gap-4"] $ do
            h3_ [class_ "whitespace-nowrap text-lg font-medium text-slate-950"] "Span"
            div_ [class_ "flex items-center border border-slate-200 rounded-lg"] do
              span_ [class_ "text-sm text-slate-950 font-medium border-r border-r-slate-200 px-2 py-1.5"] "Span ID"
              span_ [class_ "text-slate-600 text-sm font-medium px-2 py-1.5 span_id"] $ toHtml sp.spanId
              div_ [[__|install Copy(content: .span_id )|], class_ "mr-2"] do
                faSprite_ "copy" "regular" "w-3 h-3 text-slate-500"
            div_ [class_ "flex items-center gap-1"] do
              whenJust leftM $ \l -> do
                button_
                  [ class_ "cursor-pointer h-8 w-8 flex items-center justify-center rounded-full bg-slate-100 border border-slate-200 text-slate-500"
                  , hxGet_ $ "/p/" <> pid.toText <> "/traces/" <> sp.traceId <> "/?span_id=" <> l <> "&nav=true"
                  , hxSwap_ "innerHTML"
                  , hxTarget_ "#trace_span_container"
                  , hxTrigger_ "click"
                  ]
                  $ faSprite_ "chevron-left" "regular" "w-4 h-4"
              whenJust rightM $ \r -> do
                button_
                  [ class_ "cursor-pointer h-8 w-8 flex items-center justify-center rounded-full bg-slate-100 border border-slate-200 text-slate-500"
                  , hxGet_ $ "/p/" <> pid.toText <> "/traces/" <> sp.traceId <> "/?span_id=" <> r <> "&nav=true"
                  , hxSwap_ "innerHTML"
                  , hxTarget_ "#trace_span_container"
                  , hxTrigger_ "click"
                  ]
                  $ faSprite_ "chevron-right" "regular" "w-4 h-4"
          dateTime sp.startTime

      div_ [class_ "flex items-center gap-4 text-sm font-medium text-slate-950"] $ do
        h4_ [class_ "text-xl "] $ toHtml $ getServiceName sp
        faSprite_ "chevron-right" "regular" "w-4 h-4 font-bold text-slate"
        h4_ [class_ "text-xl max-w-96 truncate"] $ toHtml sp.spanName

      div_ [class_ "flex gap-4 items-center justify-between text-slate-600 text-sm mt-3"] $ do
        div_ [class_ "flex gap-4 items-center"] do
          div_ [class_ "font-medium flex shrink-0 items-center font-medium bg-slate-100 rounded-lg gap-1 border border-slate-300 px-2 py-1.5"] do
            faSprite_ "clock" "regular" "w-4 h-4"
            span_ [class_ " font-medium"] $ toHtml $ getDurationNSMS sp.spanDurationNs
          div_ [class_ "flex items-center gap-4"] do
            whenJust reqDetails $ \case
              ("HTTP", method, path, status) -> do
                div_ [class_ "flex items-center gap-1 font-medium border border-slate-300 font-medium rounded-lg bg-slate-100 px-2 py-1.5"] do
                  faSprite_ "web" "regular" "w-4 h-4"
                  span_ [class_ ""] "HTTP"
                let methodClass = getMethodColor method
                    borderColor = getMethodBorderColor method
                    extraClass = getStatusColor status
                    stBorder = getStatusBorderColor status
                span_ [class_ $ "p-2 rounded-lg border " <> borderColor <> " " <> methodClass] $ toHtml method
                span_ [class_ $ "p-2 rounded-lg border " <> stBorder <> " " <> extraClass] $ toHtml $ T.take 3 $ show status
                div_ [class_ "flex items-center"] do
                  span_ [class_ " px-2 py-1.5 max-w-96 truncate mr-2 urlPath"] $ toHtml path
                  div_ [[__| install Copy(content:.urlPath )|]] do
                    faSprite_ "copy" "regular" "h-8 w-8 border border-slate-300 bg-slate-100 rounded-full p-2 text-slate-500"
                  a_ [href_ "", class_ "ml-1"] do
                    faSprite_ "arrow-up-right" "regular" "h-8 w-8 p-2 blue-gr-btn rounded-full"
              (scheme, method, path, status) -> do
                span_ [class_ " font-medium border rounded px-2 py-1.5"] $ toHtml scheme
                div_ [class_ "flex border rounded overflow-hidden"] do
                  span_ [class_ " px-2 py-1.5 max-w-44 truncate bg-gray-200 border-r"] $ toHtml method
                  span_ [class_ " px-2 py-1.5 max-w-96 truncate"] $ toHtml path
                  let extraClass = getGrpcStatusColor status
                  span_ [class_ $ " px-2 py-1.5 border-l " <> extraClass] $ toHtml $ show status
    div_ [class_ "w-full mt-8", id_ "span-tabs-container"] do
      div_ [class_ "flex", [__|on click halt|]] $ do
        button_ [class_ "a-tab border-b-2 border-b-slate-200 px-4 py-1.5 t-tab-active", onclick_ "navigatable(this, '#att-content', '#span-tabs-container', 't-tab-active')"] "Attributes"
        button_ [class_ "a-tab border-b-2 border-b-slate-200 px-4 py-1.5 ", onclick_ "navigatable(this, '#meta-content', '#span-tabs-container', 't-tab-active')"] "Process"
        button_ [class_ "a-tab border-b-2 border-b-slate-200 flex items-center gap-1 px-4 py-1.5 ", onclick_ "navigatable(this, '#logs-content', '#span-tabs-container', 't-tab-active')"] $ do
          "Logs"
          div_ [class_ "badge badge-ghost badge-sm"] $ show $ numberOfEvents sp.events
        div_ [class_ "w-full border-b-2 border-b-slate-200"] pass

      div_ [class_ "grid mt-4 px-4 text-slate-600 font"] $ do
        div_ [class_ "a-tab-content", id_ "att-content"] $ do
          div_ [class_ "font-medium mb-1"] "Tags"
          div_ [class_ "rounded-lg border border-slate-200 p-4"] $ do
            jsonValueToHtmlTree sp.attributes
        div_ [class_ "hidden a-tab-content", id_ "meta-content"] $ do
          div_ [class_ "font-medium mb-1"] "Metadata"
          div_ [class_ "rounded-lg border border-slate-200 p-4"] $ do
            jsonValueToHtmlTree sp.resource
        div_ [class_ "hidden a-tab-content", id_ "logs-content"] $ do
          div_ [class_ "font-medium mb-1"] "Logs"
          div_ [class_ "rounded-lg border border-slate-200 p-4"] $ do
            jsonValueToHtmlTree sp.events


tagItem :: Text -> Text -> Text -> Html ()
tagItem key val cls =
  div_ [class_ "flex items-center gap-2 rounded-md bg-gray-100 px-3 py-1 "] $ do
    div_ [class_ "h-1 w-1 rounded-full bg-slate-700 shrink-0"] ""
    div_ [class_ "flex items-center"] do
      span_ [] $ toHtml key
      span_ [] " = "
      span_ [] $ toHtml val


displaySpanJson :: AE.Value -> Html ()
displaySpanJson (AE.Object obj) = mapM_ displaySpanList (KM.toList obj)
displaySpanJson _ = pass


displaySpanList :: (KM.Key, AE.Value) -> Html ()
displaySpanList (key, AE.String v) = tagItem (Key.toText key) v "text-orange-600"
displaySpanList (key, AE.Number v) = tagItem (Key.toText key) (show v) "text-blue-600"
displaySpanList (key, AE.Bool v) = tagItem (Key.toText key) (show v) "text-blue-600"
displaySpanList (key, v) = tagItem (Key.toText key) (show v) "text-orange-600"


displayLogsSection :: AE.Value -> Html ()
displayLogsSection (AE.Array obj) = V.mapM_ displayEventItem obj
displayLogsSection _ = pass


displayEventItem :: AE.Value -> Html ()
displayEventItem (AE.Object obj) = do
  div_ [class_ "w-full", [__| on click halt|]] do
    div_ [class_ "flex items-center justify-between w-full cursor-pointer px-2 py-1 bg-gray-100", [__|on click toggle .hidden on the next <div/>|]] $ do
      let evnt = KM.lookup "event_name" obj
      case evnt of
        Just (AE.String v) -> do
          div_ [] $ toHtml $ "event = " <> toText v
          div_ [] pass
        _ -> div_ [] "event = "
    div_ [class_ "expand-log max-h-96 hidden overflow-y-auto w-full py-8"] do
      jsonValueToHtmlTree (AE.Object obj)
displayEventItem _ = pass


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
