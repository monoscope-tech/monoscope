{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Projects.CreateProject (createProject) where

import Lucid
import Lucid.HTMX
import Relude
import Servant.HTML.Lucid
import Text.RawString.QQ
import Pages.BodyWrapper (bodyWrapper)

createProject :: Html ()
createProject = bodyWrapper "Create Project" $ do
  p_ [class_ "text-[#3F434A] text-xl font-normal" ] "Create Project"
  div_ [class_ "p-10 bg-[#FFFFFF] h-40 w-40 outline outline-offset-2 outline-blue-500"] $ do 
    p_ [class_ "" ] "Check"
  p_ [class_ "" ] "Check"
  
  

  
