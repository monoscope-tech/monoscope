module Pages.Traces.Spans (expandedSpanItem) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Effectful.UUID (UUID)
import Data.HashMap.Strict qualified as HM
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanRecord (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude
import Utils


expandedSpanItem :: Projects.ProjectId -> Telemetry.SpanRecord -> Html ()
expandedSpanItem pid sp = do
  div_ [class_ "w-full max-w-4xl"] $ do
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
        div_ [class_ "flex gap-2 flex-wrap"] $ do
          displaySpanJson sp.attributes

      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Metadata"
        div_ [class_ "flex gap-2 flex-wrap"] $ do
          displaySpanJson sp.resource

      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Logs"
        div_ [class_ "grid gap-3"] $ do
          div_ [class_ "grid grid-cols-[auto_1fr] items-start gap-3 rounded-md bg-gray-50 p-3"] $ do
            div_ [class_ "rounded-full bg-slate-700 px-2 py-1 text-xs text-primary-foreground"] "Info"
            div_ [data_ "id" "33"] $ do
              div_ [class_ "font-medium"] "Request received"
              div_ [class_ "text-muted-foreground"] "Received GET request for /api/users"
          div_ [class_ "grid grid-cols-[auto_1fr] items-start gap-3 rounded-md bg-gray-50 p-3"] $ do
            div_ [class_ "rounded-full bg-accent px-2 py-1 text-xs text-accent-foreground"] "Debug"
            div_ [data_ "id" "38"] $ do
              div_ [class_ "font-medium"] "Querying database"
              div_ [class_ "text-muted-foreground"] "Executing SELECT * FROM users query"
          div_ [class_ "grid grid-cols-[auto_1fr] items-start gap-3 rounded-md bg-gray-50 p-3"] $ do
            div_ [class_ "rounded-full bg-success px-2 py-1 text-xs text-success-foreground"] "Success"
            div_ [data_ "id" "43"] $ do
              div_ [class_ "font-medium"] "Response sent"
              div_ [class_ "text-muted-foreground"] "Sent 200 OK response with user data"


tagItem :: Text -> Html ()
tagItem tag =
  div_ [class_ "flex items-center gap-2 rounded-md bg-gray-100 px-3 py-1 text-sm"] $ do
    div_ [class_ "h-1 w-1 rounded-full bg-slate-700 shrink-0"] ""
    div_ [data_ "id" "60"] $ toHtml tag


displaySpanJson :: Value -> Html ()
displaySpanJson (Object obj) = do
  mapM_ displaySpanList (KM.toList obj)
displaySpanJson _ = pass


displaySpanList :: (KM.Key, Value) -> Html ()
displaySpanList (key, String v) = do
  tagItem $ Key.toText key <> "=" <> v
displaySpanList (key, Number val) = do
  tagItem $ Key.toText key <> "=" <> show val
displaySpanList (key, Bool val) = do
  tagItem $ Key.toText key <> "=" <> show val
displaySpanList (key, val) = do
  tagItem $ Key.toText key <> "=" <> show val
