module Pages.Components (statBox, drawerWithURLContent_) where

import Data.Text qualified as T
import Fmt
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Projects.Projects (ProjectId)
import Relude
import Utils


statBox :: Maybe ProjectId -> Text -> Text -> Int -> Maybe Int -> Html ()
statBox pid title helpInfo val bckupValM = do
  let tl = getTargetPage title
  let pidT = case pid of
        Just p -> p.toText
        Nothing -> ""
  if not (T.null tl)
    then do
      a_ [href_ $ "/p/" <> pidT <> tl, class_ "col-span-1 card-round p-5 flex flex-row content-between justify-between"] do
        div_ do
          div_ [class_ "inline-block flex flex-row content-between"] do
            strong_ [class_ "font-bold text-2xl"] $ toHtml @Text $ fmt (commaizeF val)
            maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
          span_ $ toHtml title
        span_ [class_ "inline-block", term "data-tippy-content" helpInfo] $ mIcon_ "info" "w-4 h-4"
    else do
      div_ [class_ "col-span-1 card-round p-5 flex flex-row content-between justify-between"] do
        div_ do
          div_ [class_ "inline-block flex flex-row content-between"] do
            strong_ [class_ "font-bold text-2xl"] $ toHtml @Text $ fmt (commaizeF val)
            maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
          span_ $ toHtml title
        span_ [class_ "inline-block", term "data-tippy-content" helpInfo] $ mIcon_ "info" "w-4 h-4"


getTargetPage :: Text -> Text
getTargetPage "Requests" = "/log_explorer"
getTargetPage "Anomalies" = "/anomalies"
getTargetPage "Endpoints" = "/endpoints"
getTargetPage _ = ""


drawerWithURLContent_ :: Text -> Text -> Html () -> Html ()
drawerWithURLContent_ drawerId url trigger = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  input_ [id_ drawerId, type_ "checkbox", class_ "drawer-toggle", [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup |]]
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block"] $ trigger
  div_ [class_ "drawer-side fixed top-0 left-0 w-full h-full flex z-[10000] ", style_ "position:fixed;width:100%;display:flex"] do
    label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "w-full drawer-overlay grow flex-1"] ""
    div_ [style_ "width: min(90vw, 850px) ", class_ "bg-white min-h-full p-5 clear-both"] do
      label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "float-right p-3 rounded-full hover:bg-gray-100 text-xl"] "x"
      div_ [class_ "bg-white", hxGet_ url, hxTrigger_ "intersect once", hxSwap_ "innerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""
