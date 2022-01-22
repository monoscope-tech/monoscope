{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Endpoints.EndpointDetails (endpointDetails) where

import Data.UUID as UUID
import Lucid
import Lucid.HTMX
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant
import Servant.HTML.Lucid
import Text.RawString.QQ

endpointDetails :: Html ()
endpointDetails = bodyWrapper "Endpoint Details" $ do
  section_ [class_ "flex flex-row"] $ do
      div_ [class_ "flex flex-row justify-between"]$ do
        div_ [class_ "flex flex-row"] $ do
          h3_ [class_ "text-xl text-slate-700"] "GET /users/acc_details"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ " h-4 w-4 m-2"]
        div_ [class_ "flex flex-row"] $ do 
          a_ [href_ ""] $ do
            button_ [class_ "bg-white rounded-lg h-10 mt-3 m-2"] $ do
              img_ [src_ "/assets/svgs/filter.svg", class_ " h-6 w-6 m-2"] 
          a_ [href_ ""] $ do
            button_ [class_ "bg-blue-700 flex h-11 flex-row m-2 px-3 rounded-xl py-2"] $ do
              h3_ [class_ "text-white text-sm text-bold mx-2 mt-1"] "Download JSON"
              div_ [class_ "bg-blue-900 p-1 rounded-lg ml-2"] $ do
                img_ [src_ "/assets/svgs/whitedown.svg", class_ "text-white h-2 w-2 m-1"] 
      div_ [class_ "abolute w-20 border border-solid border-gray-400 right-0 top-0 h-screen"] $ do
          div_ [class_ "p-4"] $ do
            img_ [src_ ""]
