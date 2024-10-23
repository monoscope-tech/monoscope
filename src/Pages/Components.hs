module Pages.Components (statBox, drawerWithURLContent_, statBox_) where

import Data.Text qualified as T
import Fmt (commaizeF, fmt, (+|))
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects (ProjectId)
import Relude
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


statBox_ :: Maybe ProjectId -> Maybe (Text, Text, Text) -> Text -> Text -> Text -> Maybe Int -> Html ()
statBox_ pid iconM title helpInfo val bckupValM = do
  -- let tl = getTargetPage title
  -- let pidT = case pid of
  --       Just p -> p.toText
  --       Nothing -> ""
  div_ [class_ "bg-[#F1F5F9] rounded-3xl flex flex-col gap-3 p-5 border border-[#E2E8F0]"] do
    whenJust iconM $ \(icon, kind, color) -> do
      div_ [class_ "flex items-center justify-center h-10 w-10 bg-white rounded-[12px]"] do
        faSprite_ icon kind $ "w-4 h-4 " <> color
    div_ [class_ "flex flex-col gap-.5"] do
      span_ [class_ "font-bold text-4xl text-gray-800"] $ toHtml val
      div_ [class_ "flex gap-1 items-center text-sm text-gray-500"] do
        p_ [] $ toHtml title
        span_ [class_ "inline-block tooltip", term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 h-4"


getTargetPage :: Text -> Text
getTargetPage "Requests" = "/log_explorer"
getTargetPage "Anomalies" = "/anomalies"
getTargetPage "Endpoints" = "/endpoints"
getTargetPage _ = ""


drawerWithURLContent_ :: Text -> Maybe Text -> Html () -> Html ()
drawerWithURLContent_ drawerId urlM trigger = div_ [class_ "drawer drawer-end inline-block w-auto"] do
  input_ [id_ drawerId, type_ "checkbox", class_ "drawer-toggle", [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup |]]
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block"] trigger
  div_ [class_ "drawer-side fixed top-0 left-0 w-full h-full flex z-[10000] overflow-y-scroll ", style_ "position:fixed;width:100%;display:flex"] do
    label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "w-full drawer-overlay grow flex-1"] ""
    div_ [style_ "width: min(90vw, 1200px)", class_ "bg-base-100 h-full clear-both overflow-y-scroll"] do
      label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "float-right p-3 rounded-full hover:bg-gray-100 text-xl"] "x"
      div_
        ( [id_ $ drawerId <> "-content", class_ "bg-base-100 h-full overflow-y-auto p-4 flex flex-col", hxSwap_ "innerHTML"]
            <> maybe [] (\url -> [hxGet_ url, hxTrigger_ "intersect once"]) urlM
        )
        $ span_ [class_ "loading loading-dots loading-md"] ""
