module Pages.Components (statBox, drawerWithURLContent_, statBox_, emptyState_, dateTime) where

import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
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


statBox_ :: Maybe ProjectId -> Maybe (Text, Text, Text) -> Text -> Text -> Text -> Maybe Int -> Maybe Text -> Html ()
statBox_ pid iconM title helpInfo val bckupValM valClsM = do
  -- let tl = getTargetPage title
  -- let pidT = case pid of
  --       Just p -> p.toText
  --       Nothing -> ""
  div_ [class_ "bg-slate-100 rounded-3xl flex flex-col gap-3 p-5 border border-slate-200"] do
    whenJust iconM $ \(icon, kind, color) -> do
      div_ [class_ "flex items-center justify-center h-10 w-10 bg-slate-50 rounded-xl"] do
        faSprite_ icon kind $ "w-4 h-4 " <> color
    div_ [class_ "flex flex-col gap-1"] do
      let fsiz = if isJust iconM then "text-2xl " else "text-4xl "
      span_ [class_ $ "font-bold  " <> fsiz <> fromMaybe "text-gray-800" valClsM] $ toHtml val
      div_ [class_ "flex gap-2 items-center text-sm text-gray-500"] do
        p_ [] $ toHtml title
        span_ [term "data-tip" helpInfo] $ faSprite_ "circle-info" "regular" "w-4 mt-[-2px]"


emptyState_ :: Text -> Text -> Maybe Text -> Text -> Html ()
emptyState_ title subTxt url btnText = do
  section_ [class_ "w-max mx-auto my-8 text-center p-5 sm:py-14 sm:px-24 flex flex-col gap-4"] do
    div_ [] $ faSprite_ "empty" "regular" "h-24 w-24 stroke-blue-500 fill-blue-500"
    div_ [class_ "flex flex-col gap-2"] do
      h2_ [class_ "text-xl text-slate-800 font-bold"] $ toHtml title
      p_ [class_ "text-sm font-medium text-gray-500"] $ toHtml subTxt
      a_ [href_ $ fromMaybe "" url, class_ "btn text-sm w-max mx-auto bg-brand text-white"] $ toHtml btnText


getTargetPage :: Text -> Text
getTargetPage "Requests" = "/log_explorer"
getTargetPage "Anomalies" = "/anomalies"
getTargetPage "Endpoints" = "/endpoints"
getTargetPage _ = ""


drawerWithURLContent_ :: Text -> Maybe Text -> Html () -> Html ()
drawerWithURLContent_ drawerId urlM trigger = div_ [class_ "drawer drawer-end inline-block w-auto h-"] do
  input_ [id_ drawerId, type_ "checkbox", class_ "drawer-toggle", [__|on keyup if the event's key is 'Escape' set my.checked to false trigger keyup |]]
  label_ [Lucid.for_ drawerId, class_ "drawer-button inline-block"] trigger
  div_ [class_ "drawer-side fixed top-0 left-0 w-full h-full flex z-[10000] overflow-y-scroll ", style_ "position:fixed;width:100%;display:flex"] do
    label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "w-full drawer-overlay grow flex-1"] ""
    div_ [style_ "width: min(90vw, 1200px)", class_ "bg-slate-50 h-full overflow-y-scroll"] do
      label_ [Lucid.for_ drawerId, Aria.label_ "close modal", class_ "float-right mt-5 h-10 w-10 flex items-center justify-center rounded-full bg-slate-200"] $ faSprite_ "xmark" "solid" "w-4 h-4"
      div_
        ( [id_ $ drawerId <> "-content", class_ "bg-slate-50 p-4 h-full flex flex-col", hxSwap_ "innerHTML"]
            <> maybe [] (\url -> [hxGet_ url, hxTrigger_ "intersect once"]) urlM
        )
        $ span_ [class_ "loading loading-dots loading-md"] ""


dateTime :: UTCTime -> Html ()
dateTime t = do
  span_ [class_ "flex items-center rounded-lg px-2 py-1.5 font-medium gap-2 border border-slate-300 bg-slate-100 text-slate-600"] do
    faSprite_ "calendar" "regular" "w-5 h-5 fill-none"
    toHtml $ formatTime defaultTimeLocale "%b. %d, %Y %I:%M:%S %p" t
