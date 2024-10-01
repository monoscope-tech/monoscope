module Pages.Traces.Spans (expandedSpanItem, spanLatencyBreakdown) where

import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanRecord (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import NeatInterpolation (text)
import Pages.Traces.Utils (getRequestDetails, getServiceColor, getServiceName)
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils


expandedSpanItem :: Projects.ProjectId -> Telemetry.SpanRecord -> Html ()
expandedSpanItem pid sp = do
  let reqDetails = getRequestDetails sp
  script_
    [type_ "text/hyperscript"]
    [text|
      behavior Navigatable(content)
         on click remove .tab-active from .span-tab
            then add .tab-active to me
            then add .hidden to .span-tab-content
            then remove .hidden from content
      end
    |]
  div_ [class_ "w-full pb-2"] $ do
    div_ [class_ "flex flex-col gap-2 bg-gray-50 py-2"] $ do
      div_ [class_ "flex items-center gap-4"] $ do
        h3_ [class_ "whitespace-nowrap text-xl font-bold pr-4 border-r border-r-2"] "Span"
        div_ [class_ "flex items-center gap-4"] $ do
          h4_ [class_ "text-xl font-medium"] $ toHtml $ getServiceName sp
          faSprite_ "caret-up" "solid" "w-5 h-5 rotate-90 font-bold"
          h4_ [class_ "text-xl font-medium max-w-96 truncate"] $ toHtml sp.spanName
      div_ [class_ "flex gap-4 items-center"] do
        div_ [class_ "flex items-end border rounded"] do
          span_ [class_ "text-sm text-gray-500 font-medium border-r px-2 py-1"] "Span ID"
          span_ [class_ "text-sm px-2 py-1 text-gray-600"] $ toHtml sp.spanId
        div_ [class_ "flex items-end border rounded"] do
          span_ [class_ "text-sm text-gray-500 font-medium border-r px-2 py-1"] "Trace ID"
          span_ [class_ "text-sm px-2 py-1 text-gray-600"] $ toHtml sp.traceId
      div_ [class_ "flex gap-4 items-center justify-between text-gray-600  mt-3"] $ do
        div_ [class_ "flex gap-4 items-center"] do
          div_ [class_ "font-medium flex shrink-0 items-center rounded gap-1 border px-2 py-1.5 text-gray-600"] do
            faSprite_ "clock" "regular" "w-3 h-3"
            span_ [class_ "text-sm font-medium"] $ toHtml $ getDurationNSMS sp.spanDurationNs
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
                  let extraClass = getGrpcStatusColor status
                  span_ [class_ $ "text-sm px-2 py-1.5 border-l " <> extraClass] $ toHtml $ show status
        span_ [class_ "text-sm"] $ toHtml $ formatTime defaultTimeLocale "%b %d %Y %H:%M:%S%Q" sp.timestamp

    div_ [class_ "tabs tabs-boxed tabs-outline items-center border tabs-sm w-max mt-8", [__|on click halt|]] $ do
      a_ [class_ "tab span-tab tab-active", [__| install Navigatable(content: .attributes-content)|]] "Attributes"
      a_ [class_ "tab span-tab", [__| install Navigatable(content: .process-content)|]] $ do
        "Process"
      a_ [class_ "tab span-tab", [__| install Navigatable(content: .logs-content)|]] $ do
        "Logs"
        div_ [class_ "badge badge-ghost badge-sm"] $ show $ numberOfEvents sp.events

    div_ [class_ "grid mt-4"] $ do
      div_ [class_ "grid gap-3 span-tab-content attributes-content"] $ do
        div_ [class_ "font-semibold"] "Tags"
        div_ [class_ "flex gap-3 flex-wrap"] $ do
          displaySpanJson sp.attributes
      div_ [class_ "grid gap-3 span-tab-content hidden process-content"] $ do
        div_ [class_ "font-semibold"] "Metadata"
        div_ [class_ "flex gap-3 flex-wrap"] $ do
          displaySpanJson sp.resource
      div_ [class_ "grid gap-3 span-tab-content hidden logs-content"] $ do
        div_ [class_ "font-semibold"] "Logs"
        div_ [class_ "flex flex-col gap-1 w-full"] do
          displayLogsSection sp.events


tagItem :: Text -> Text -> Text -> Html ()
tagItem key val cls =
  div_ [class_ "flex items-center gap-2 rounded-md bg-gray-100 px-3 py-1 text-sm"] $ do
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
  div_ [class_ "flex h-6 w-[150px]"] $ do
    V.forM_ spans $ \sp -> do
      -- use percentage of total duration to determine width of bar
      let wdth = (fromIntegral sp.spanDurationNs / fromIntegral totalDuration) * 150
      let color = fromMaybe "#000000" $ HM.lookup sp.spanName colors
      div_
        [ class_ $ "h-full overflow-hidden tooltip " <> color
        , style_ $ "width:" <> show wdth <> "px;"
        , term "data-tip" $ "Span name: " <> sp.spanName <> " Duration: " <> toText (getDurationNSMS sp.spanDurationNs)
        , title_ $ "Span name: " <> sp.spanName <> " Duration: " <> toText (getDurationNSMS sp.spanDurationNs)
        ]
        do
          div_ [class_ "h-full w-full"] ""
