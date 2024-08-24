module Pages.Traces.Spans (expandedSpanItem) where

import Lucid
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanRecord (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude
import Data.Aeson qualified as AE
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Aeson.Key as Key


expandedSpanItem :: Projects.ProjectId -> Telemetry.SpanRecord -> Html ()
expandedSpanItem pid sp = do
  div_ [class_ "w-full"] $ do
    div_ [class_ "flex flex-col space-y-1.5 p-6 bg-gray-50 px-6 py-4"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "grid gap-1"] $ do
          h3_ [class_ "whitespace-nowrap text-2xl font-semibold leading-none tracking-tight"] $ toHtml sp.spanName
          p_ [class_ "text-sm text-muted-foreground"] $ "Span ID: " <> toHtml sp.spanId
          p_ [class_ "text-sm text-muted-foreground"] $ "Trace ID: " <> toHtml sp.traceId
        div_ [class_ "flex items-center gap-2"] $ do
          div_ [class_ "text-sm text-muted-foreground"] $
            time_ [datetime_ "2023-06-23T15:34:12Z"] "Jun 23, 2023 3:34 PM"
          div_ [class_ "text-lg font-medium"] $
            span_ [data_ "id" "11"] "250ms"

    div_ [class_ "grid gap-6 p-6"] $ do
      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Tags"
        div_ [class_ "flex gap-3 flex-wrap"] $ do
          displaySpanJson sp.attributes

      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Logs"
        div_ [class_ "flex flex-col gap-1 w-full"] do
          displayLogsSection sp.events
      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Metadata"
        div_ [class_ "flex gap-3 flex-wrap"] $ do
          displaySpanJson sp.resource


tagItem :: Text -> Text -> Text -> Html ()
tagItem key val cls =
  div_ [class_ "flex items-center gap-2 rounded-md bg-gray-100 px-3 py-1 text-sm"] $ do
    div_ [class_ "h-1 w-1 rounded-full bg-slate-700 shrink-0"] ""
    div_ [class_ "flex items-center"] do
      span_ [] $ toHtml key
      span_ [] " = "
      span_ [] $ toHtml val


displaySpanJson :: AE.Value -> Html ()
displaySpanJson (AE.Object obj) = do
  mapM_ displaySpanList (KM.toList obj)
displaySpanJson _ = pass


displaySpanList :: (KM.Key, AE.Value) -> Html ()
displaySpanList (key, AE.String v) = do
  tagItem (Key.toText key) v "text-orange-600"
displaySpanList (key, AE.Number v) = do
  tagItem (Key.toText key) (show v) "text-blue-600"
displaySpanList (key, AE.Bool v) = do
  tagItem (Key.toText key) (show v) "text-blue-600"
displaySpanList (key, v) = do
  tagItem (Key.toText key) (show v) "text-orange-600"


displayLogsSection :: AE.Value -> Html ()
displayLogsSection (AE.Array obj) = do
  V.mapM_ displayEventItem obj
displayLogsSection _ = pass


displayEventItem :: AE.Value -> Html ()
displayEventItem (AE.Object obj) = do
  div_ [class_ "w-full", [__| on click halt|]] do
    div_ [class_ "flex items-center justify-between w-full px-2 py-1 bg-gray-100", [__|on click toggle .hidden on the next <div/>|]] $ do
      let evnt = KM.lookup "event_name" obj
      case evnt of
        Just (AE.String v) -> do
          div_ [] $ toHtml $ "event = " <> toText v
          div_ [] pass
        _ -> div_ [] "event = "
    div_ [class_ "expand-log max-h-96 hidden overflow-y-auto bg-red-500 w-full py-8"] $ pass
displayEventItem _ = pass
