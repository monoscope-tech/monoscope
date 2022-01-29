{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Projects.ProjectView (projectView) where

import Data.UUID as UUID
import Lucid
import Lucid.HTMX
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant
import Servant.HTML.Lucid
import Text.RawString.QQ

projectView :: Html ()
projectView = bodyWrapper "Project View" $ do
  div_ [class_ "container"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5"] "Projects"
    div_ [class_ "px-10 border border-gray-200 py-5  bg-white rounded-3xl"] $ do
      div_ [class_ "flex justify-between "] $ do
        h3_ [class_ "text-slate-700 text-base"] "Delivery Hero"
        div_ [class_ "flex flex-row"] $ do
          img_ [src_ "/assets/svgs/calender.svg", class_ "h-4 mx-2  w-4"]
        h4_ [class_ "text-sm text-gray-500"] " Jun 17"
      p_ [class_ "text-gray-500 my-5 mt-4 font-light text-sm"] "We need to develop several options (Inbox template, Chat template, tasks template, Projects template) of cool user interface design templates - to carefully work out the smallest details."
      div_ [class_ "flex flex-row mt-5"] $ do
        img_ [class_ " mx-1 h-5 w-5", src_ "/assets/svgs/avatar_man.svg"]
