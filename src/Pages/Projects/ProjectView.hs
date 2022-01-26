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
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5" ] "Projects"
  
