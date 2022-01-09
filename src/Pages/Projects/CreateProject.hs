{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Pages.Projects.CreateProject (createProject) where

import Lucid
import Relude
import Servant.HTML.Lucid

createProject :: Html ()
createProject = bodyWrapper "Create Project" $ do
  p_ $ b_ "bad"
  p_ $ i_ "iik"
  p_ $ a_ [] "stringsx"


bodyWrapper :: Text -> Html () -> Html ()
bodyWrapper title child = do
  doctypehtml_ $ do
    head_ $ do
      title_ $ toHtml title 
      link_ [rel_ "stylesheet", type_ "text/css", href_ "/assets/css/tailwind.min.css"]
    body_ $ do
      aside_ $ do
        div_ $ do
          img_ [src_ "//placehold.it/200x200"]
        div_ $ do
          a_ [] $ do
            div_ $ do
              strong_ "Flip productivity"
              span_ "Development"
            div_ $ do
              img_ [src_ "//placehold.it/100x100"]
              img_ [src_ "//placehold.it/100x100"]
                
      section_ $ do
        nav_ $ do
          img_ [src_ "//placehold.it/50x100"]
      child
