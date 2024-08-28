module Pages.Components (statBox, drawerWithURLContent_) where

import Data.Text qualified as T
import Fmt (commaizeF, fmt, (+|))
import Lucid (
  Html,
  Term (term),
  ToHtml (toHtml),
  a_,
  class_,
  div_,
  for_,
  href_,
  id_,
  input_,
  label_,
  small_,
  span_,
  strong_,
  style_,
  type_,
 )
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects (ProjectId)
import Relude (
  Int,
  Maybe (..),
  Semigroup ((<>)),
  Text,
  maybe,
  not,
  ($),
 )
import Utils (faSprite_)


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
        span_ [class_ "inline-block tooltip", term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"
    else do
      div_ [class_ "col-span-1 card-round p-5 flex flex-row content-between justify-between"] do
        div_ do
          div_ [class_ "inline-block flex flex-row content-between"] do
            strong_ [class_ "font-bold text-2xl"] $ toHtml @Text $ fmt (commaizeF val)
            maybe "" (\bVal -> small_ $ toHtml @Text $ fmt ("/" +| commaizeF bVal)) bckupValM
          span_ $ toHtml title
        span_ [class_ "inline-block tooltip", term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"


getTargetPage :: Text -> Text
getTargetPage "Requests" = "/log_explorer"
getTargetPage "Anomalies" = "/anomalies"
getTargetPage "Endpoints" = "/endpoints"
getTargetPage _ = ""


drawerWithURLContent_ :: Text -> Text -> Html () -> Html ()
drawerWithURLContent_ drawerId url trigger = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  input_ [id_ drawerId, type_ "checkbox", class_ "drawer-toggle", [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup |]]
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block"] trigger
  div_ [class_ "drawer-side fixed top-0 left-0 w-full h-full flex z-[10000] overflow-y-scroll ", style_ "position:fixed;width:100%;display:flex"] do
    label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "w-full drawer-overlay grow flex-1"] ""
    div_ [style_ "width: min(90vw, 850px) ", class_ "bg-base-100 min-h-full p-4 clear-both overflow-y-scroll"] do
      label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "float-right p-3 rounded-full hover:bg-gray-100 text-xl"] "x"
      div_ [class_ "bg-base-100", hxGet_ url, hxTrigger_ "intersect once", hxSwap_ "innerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""
