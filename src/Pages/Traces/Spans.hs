module Pages.Traces.Spans (expandedSpanItem) where

import Data.Effectful.UUID (UUID)
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry (SpanRecord (..))
import Models.Telemetry.Telemetry qualified as Telemetry
import Relude
import Utils


expandedSpanItem :: Projects.ProjectId -> Telemetry.SpanRecord -> Html ()
expandedSpanItem pid spn = do
  div_ [class_ "w-full max-w-4xl"] $ do
    div_ [class_ "flex flex-col space-y-1.5 p-6 bg-gray-50 px-6 py-4"] $ do
      div_ [class_ "flex items-center justify-between"] $ do
        div_ [class_ "grid gap-1"] $ do
          h3_ [class_ "whitespace-nowrap text-2xl font-semibold leading-none tracking-tight"] $ toHtml spn.spanName
          p_ [class_ "text-sm text-muted-foreground"] $ "Span ID: " <> toHtml spn.spanId
          p_ [class_ "text-sm text-muted-foreground"] $ "Trace ID: " <> toHtml spn.traceId
        div_ [class_ "flex items-center gap-2"] $ do
          div_ [class_ "text-sm text-muted-foreground"]
            $ time_ [datetime_ "2023-06-23T15:34:12Z"] "Jun 23, 2023 3:34 PM"
          div_ [class_ "text-lg font-medium"]
            $ span_ [data_ "id" "11"] "250ms"

    div_ [class_ "grid gap-6 p-6"] $ do
      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Tags"
        div_ [class_ "grid grid-cols-2 gap-3 sm:grid-cols-3 md:grid-cols-4"] $ do
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "18"] "http.method=GET"
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "21"] "http.url=/api/users"
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "24"] "http.status_code=200"
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "27"] "service=api"

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

      div_ [class_ "grid gap-3"] $ do
        div_ [class_ "font-semibold"] "Metadata"
        div_ [class_ "grid grid-cols-2 gap-3 sm:grid-cols-3 md:grid-cols-4"] $ do
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "51"] "http.method=GET"
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "54"] "http.url=/api/users"
          div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
            div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
            div_ [data_ "id" "57"] "http.status_code=200"
          tagItem "service=api"


tagItem :: Text -> Html ()
tagItem tag =
  div_ [class_ "flex items-center gap-2 rounded-md bg-gray-50 px-3 py-1 text-sm"] $ do
    div_ [class_ "h-2 w-2 rounded-full bg-slate-700"] ""
    div_ [data_ "id" "60"] $ toHtml tag
