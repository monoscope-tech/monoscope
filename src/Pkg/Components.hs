module Pkg.Components (
  navBar,
  modal_,
  dropDownMenu_,
  TabFilter (..),
  TabFilterOpt (..),
  module Pkg.Components.ItemsList,
  module Pkg.Components.TimePicker,
)
where

import Lucid
import Pkg.Components.ItemsList
import Pkg.Components.Modals (dropDownMenu_, modal_)
import Pkg.Components.TimePicker
import Relude
import Utils


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
  toHtml tf = div_ [class_ "tabs tabs-box tabs-outline p-0 bg-fillWeak text-textWeak border items-center border"] do
    let uri = deleteParam "filter" tf.currentURL
    forM_ tf.options \opt ->
      a_
        [ href_ $ uri <> "&filter=" <> escapedQueryPartial opt.name
        , role_ "tab"
        , class_ $ "tab " <> if opt.name == tf.current then "tab-active text-textStrong border border-strokeStrong" else ""
        ]
        do
          span_ $ toHtml opt.name
          whenJust opt.count $ span_ [class_ "absolute top-[1px] -right-[5px] text-white text-xs font-medium rounded-full px-1 bg-red-500"] . show
