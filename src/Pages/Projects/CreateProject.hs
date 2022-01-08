{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Pages.Projects.CreateProject (createProject) where

import Relude
import Lucid
import Servant.HTML.Lucid

createProject :: Html ()
createProject = do
  p_ $ b_ "bad"
  p_ $ i_ "iik"
  p_ $ a_ [] "stringsx"
