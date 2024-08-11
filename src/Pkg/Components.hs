module Pkg.Components (
  loader,
  navBar,
  bashCommand,
  codeExample,
  modal_,
  dropDownMenu_,
  timepicker_,
  codeEmphasis,
  withEmphasisedText,
  TabFilter (..),
  TabFilterOpt (..),
)
where

import Data.Text
import Lucid
import Lucid.Base
import Lucid.Hyperscript
import Lucid.Svg (d_, fill_, path_, viewBox_)
import Pkg.Components.Modals (dropDownMenu_, modal_)
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
  nav_ [id_ "main-navbar", class_ "fixed z-20 top-0 w-full w-full px-6 py-4 border-b bg-base-100 flex flex-row justify-between"] do
    div_ [class_ "flex justify-between items-center gap-4 w-[1000px] mx-auto"] do
      a_ [href_ "https://apitoolkit.io", class_ "flex items-center text-gray-500 hover:text-gray-700"] do
        img_
          [ class_ "h-12 sd-hidden"
          , src_ "/assets/svgs/logo.svg"
          ]
        img_
          [ class_ "h-12 w-10 hidden sd-show"
          , src_ "/assets/svgs/logo_mini.svg"
          ]


bashCommand :: Text -> Html ()
bashCommand command = do
  div_ [class_ "w-full"] do
    div_ [class_ "w-full rounded-lg bg-slate-800 px-4 py-2 text-gray-300 flex gap-2 items-start"] do
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
  div_ [class_ "relative overflow-hidden flex bg-slate-800 sm:rounded-xl"] do
    div_ [class_ "relative w-full flex flex-col"] do
      div_ [class_ "flex-none border-b border-slate-500/30 flex justify-between items-center gap-4"] do
        div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
          div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
        button_
          [ class_ "text-gray-500 text-sm font-bold mr-6"
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
      div_ [class_ "relative flex-auto flex flex-col"] do
        pre_ [class_ "flex leading-snug"] do
          code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs atom-one-dark"] $ toHtml code


codeEmphasis :: Text -> Html ()
codeEmphasis code = span_ [class_ "text-red-500"] $ toHtml $ code


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
  toHtmlRaw a = toHtml a
  toHtml tf = div_ [class_ "tabs tabs-boxed border"] do
    let uri = deleteParam "filter" tf.currentURL
    forM_ tf.options \opt ->
      a_
        [ href_ $ uri <> "&filter=" <> escapedQueryPartial opt.name
        , role_ "tab"
        , class_ $ "tab " <> if opt.name == tf.current then "tab-active" else ""
        ]
        do
          span_ $ toHtml opt.name
          whenJust opt.count \countV -> span_ [class_ "absolute top-[1px] -right-[5px] text-white text-xs font-medium rounded-full px-1 bg-red-500"] $ show countV


-----------
--
--

timePickerItems :: [(Text, Text)]
timePickerItems =
  [ ("1H", "Last Hour")
  , ("24H", "Last 24 Hours")
  , ("7D", "Last 7 days")
  , ("14D", "Last 14 days")
  ]


timepicker_ :: Maybe Text -> Html ()
timepicker_ currentRange = div_ [class_ "relative"] do
  input_ [type_ "hidden", id_ "since_input"]
  input_ [type_ "hidden", id_ "custom_range_input"]
  a_
    [ class_ "relative btn btn-sm btn-outline"
    , [__| on click toggle .hidden on #timepickerBox|]
    ]
    do
      faSprite_ "clock" "regular" "h-4 w-4"
      span_ [class_ "inline-block", id_ "currentRange"] $ toHtml (fromMaybe "Last 14 Days" currentRange)
      faSprite_ "chevron-down" "regular" "h-3 w-3 inline-block"
  div_ [id_ "timepickerBox", class_ "hidden absolute z-10 mt-1  rounded-md flex"] do
    div_ [class_ "inline-block w-84 overflow-auto bg-base-100 py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"] do
      timePickerItems
        & mapM_ \(val, title) ->
          a_
            [ class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 "
            , term "data-value" val
            , term "data-title" title
            , [__| on click set #custom_range_input's value to my @data-value then log my @data-value
                                   then toggle .hidden on #timepickerBox
                                   then set #currentRange's innerText to my @data-title
                                   then htmx.trigger("#log_explorer_form", "submit")
                         |]
            ]
            $ toHtml title
      a_ [class_ "block text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-200 ", [__| on click toggle .hidden on #timepickerSidebar |]] "Custom date range"
    div_ [class_ "inline-block relative hidden", id_ "timepickerSidebar"] do
      div_ [id_ "startTime", class_ "hidden"] ""
