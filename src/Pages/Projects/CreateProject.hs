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
  p_ $ b_ "bad"
  p_ $ i_ "iik"
  p_ $ a_ [] "stringsx"
