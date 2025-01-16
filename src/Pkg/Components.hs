module Pkg.Components (
  loader,
  navBar,
  bashCommand,
  codeExample,
  modal_,
  dropDownMenu_,
  codeEmphasis,
  withEmphasisedText,
  TabFilter (..),
  TabFilterOpt (..),
  module Pkg.Components.ItemsList,
  module Pkg.Components.TimePicker,
)
where

import Lucid
import Lucid.Base
import Lucid.Hyperscript
import Lucid.Svg (d_, fill_, path_, viewBox_)
import Pkg.Components.ItemsList
import Pkg.Components.Modals (dropDownMenu_, modal_)
import Pkg.Components.TimePicker
import Relude
import Utils


loader :: Html ()
loader =
  div_ [role_ "status"] do
    svg_ [class_ "w-8 h-8 mr-2 text-gray-200 animate-spin fill-blue-600", viewBox_ "0 0 100 101", fill_ "none", xmlns_ "http://www.w3.org/2000/svg"] do
      path_ [d_ "M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z", fill_ "currentColor"]
      path_ [d_ "M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z", fill_ "currentFill"]
    span_ [class_ "sr-only"] "Loading..."


navBar :: Html ()
navBar = do
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full w-full px-6 py-4 bg-base-100 flex flex-row justify-between"] do
    div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"] do
      a_ [href_ "https://apitoolkit.io", class_ "flex items-center text-gray-500 hover:text-gray-700"] do
        img_
          [ class_ "h-12 sd-hidden"
          , src_ "/public/assets/svgs/logo.svg"
          ]
        img_
          [ class_ "h-12 w-10 hidden sd-show"
          , src_ "/public/assets/svgs/logo_mini.svg"
          ]


bashCommand :: Text -> Html ()
bashCommand command = do
  div_ [class_ "w-full"] do
    div_ [class_ "w-full rounded-lg bg-slate-100 px-4 py-2 text-slate-700 flex gap-2 items-start"] do
      span_ [class_ "text-gray-400"] "$"
      span_ $ toHtml command
      button_
        [ termRaw "data-command" command
        , [__|
            on click
              if 'clipboard' in window.navigator then
                call navigator.clipboard.writeText(my @data-command)
                send successToast(value:['Command copied to clipboard']) to <body/>
              end
      |]
        ]
        do
          faSprite_ "copy" "solid" "h-4 w-4 text-gray-500"


codeExample :: Text -> Html ()
codeExample code = do
  div_ [class_ "relative overflow-hidden flex bg-slate-100 border border-weak rounded-xl"] do
    div_ [class_ "relative w-full flex flex-col"] do
      div_ [class_ "flex-none border-b border-weak flex justify-between items-center gap-4"] do
        div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
          div_ [class_ "w-2.5 h-2.5 bg-red-500 rounded-full"] ""
          div_ [class_ "w-2.5 h-2.5 bg-yellow-500 rounded-full"] ""
          div_ [class_ "w-2.5 h-2.5 bg-green-500 rounded-full"] ""
        button_
          [ class_ "text-gray-500 font-bold mr-6"
          , term "data-code" code
          , [__|
              on click
                if 'clipboard' in window.navigator then
                  call navigator.clipboard.writeText(my @data-code)
                  send successToast(value:['Copied']) to <body/>
                end
           |]
          ]
          $ faSprite_ "copy" "solid" "h-4 w-4 inline-block"
      div_ [class_ "relative flex-auto flex flex-col bg-slate-100"] do
        pre_ [class_ "flex leading-snug"] do
          code_ [class_ "flex-auto relative block text-strong py-4 px-4 overflow-auto hljs atom-one-light"] $ toHtml code


codeEmphasis :: Text -> Html ()
codeEmphasis code = span_ [class_ "text-red-500"] $ toHtml code


withEmphasisedText :: [(Text, Bool)] -> Html ()
withEmphasisedText [] = mempty
withEmphasisedText ((text, True) : xs) = do
  codeEmphasis $ " " <> text <> " "
  withEmphasisedText xs
withEmphasisedText ((text, False) : xs) = do
  toHtml text
  withEmphasisedText xs


--------------------------------------------------------------------
-- DaisyUI bordered tabs.
------------------------------------------------------------------
data TabFilter = TabFilter
  { current :: Text
  , currentURL :: Text
  , options :: [TabFilterOpt]
  }


data TabFilterOpt = TabFilterOpt
  { name :: Text
  , count :: Maybe Int
  }


instance ToHtml TabFilter where
  toHtmlRaw = toHtml
  toHtml tf = div_ [class_ "tabs tabs-boxed tabs-outline p-0 bg-weak text-weak border items-center border"] do
    let uri = deleteParam "filter" tf.currentURL
    forM_ tf.options \opt ->
      a_
        [ href_ $ uri <> "&filter=" <> escapedQueryPartial opt.name
        , role_ "tab"
        , class_ $ "tab " <> if opt.name == tf.current then "tab-active text-strong stroke-strong" else ""
        ]
        do
          span_ $ toHtml opt.name
          whenJust opt.count $ span_ [class_ "absolute top-[1px] -right-[5px] text-white text-xs font-medium rounded-full px-1 bg-red-500"] . show

-- , navTabs = Just $ div_ [class_ "tabs tabs-boxed tabs-md p-0 tabs-outline items-center bg-weak text-weak border"] do
--     a_ [onclick_ "window.setQueryParamAndReload('source', 'requests')", role_ "tab", class_ $ "tab py-1 !h-auto " <> if source == "requests" then "tab-active text-strong stroke-strong " else ""] "Requests"
--     a_ [onclick_ "window.setQueryParamAndReload('source', 'logs')", role_ "tab", class_ $ "tab py-1 !h-auto " <> if source == "logs" then "tab-active text-strong stroke-strong " else ""] "Logs"
--     a_ [onclick_ "window.setQueryParamAndReload('source', 'spans')", role_ "tab", class_ $ "tab py-1 !h-auto " <> if source == "spans" then "tab-active text-strong stroke-strong " else ""] "Traces"
