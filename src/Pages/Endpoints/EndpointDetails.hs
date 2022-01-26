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
  div_ [class_ "container"] $ do
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
            h3_ [class_ "text-white text-sm text-bold mx-2 mt-1"] "Download Swagger"
            div_ [class_ "bg-blue-900 p-1 rounded-lg ml-2"] $ do
              img_ [src_ "/assets/svgs/whitedown.svg", class_ "text-white h-2 w-2 m-1"] 
    div_ [class_ "flex justify-between mt-5"] $ do
      div_ [class_ "flex flex-row"] $ do
        img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-1 w-4" ]
        span_ [class_ "text-lg text-slate-700"] "Request"
      div_ [class_ "flex flex-row mt-2"] $ do
        img_ [src_ "/assets/svgs/leftarrow.svg", class_ " m-2" ]
        span_ [src_ " mx-4"] "1/1"
        img_ [src_ "/assets/svgs/rightarrow.svg", class_ " m-2" ]
    div_ [class_ "bg-white rounded-xl p-5"] $ do 
        div_ [class_ "flex flex-row "] $ do
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-4 mr-3 mt-4 w-4" ]
          div_ [class_ "bg-gray-100 px-10 rounded-xl w-full p-4 text-sm text-slate-600"] "REQUEST HEADERS"
        div_ [class_ "border flex mb-5 flex-row border-gray-100 ml-20 px-5 p-3 rounded-xl mt-2 "] $ do
          input_ [type_ "checkbox" ,class_ " mr-12 m-1" ] 
          span_ [class_ "grow text-sm text-slate-600"] "Authorization"
          span_ [class_ "text-sm text-slate-500 mx-12"] "[] obj"
          img_ [src_ "/assets/svgs/alert-red.svg", class_ " mx-10 "]
          img_ [src_ "/assets/svgs/dots-vertical.svg", class_ "mx-5"]

        

    