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
            h3_ [class_ "text-white text-sm text-bold mx-2 mt-1"] "Download JSON"
            div_ [class_ "bg-blue-900 p-1 rounded-lg ml-2"] $ do
              img_ [src_ "/assets/svgs/whitedown.svg", class_ "text-white h-2 w-2 m-1"] 
    div_ [class_ "bg-amber-100 mt-3 relative rounded-xl w-full border border-yellow-400 p-8 flex "] $ do
      div_ [class_ "mx-2"] $ do
        img_ [src_ "/assets/svgs/warning.svg", class_ "mt-3"]
      div_ [class_ " mx-6 border-r border-yellow-400 p-5"] $ do
        h5_ [class_ "text-black text-neutral-800 mb-4 -mt-8 text-base"] "Added Fields (3)"
        p_ [href_ "", class_ "text-sm underline underline-offset-1"] "access_code"
        p_ [href_ "", class_ "text-sm underline underline-offset-1"] "date_of_birth"
        p_ [href_ "", class_ "text-sm underline underline-offset-1"] "atm_passcodes"
      div_ [class_ " mx-6 border-r border-yellow-400 p-5"] $ do
        h5_ [class_ "text-black text-neutral-800 mb-4 -mt-8 text-base"] "Deleted Fields (2)"
        p_ [href_ "", class_ "text-sm text-red-400 underline underline-offset-1"] "access_code"
        p_ [href_ "", class_ "text-sm text-red-400 underline underline-offset-1"] "date_of_birth"
      div_ [class_ " mx-6 p-5"] $ do
        h5_ [class_ "text-black text-neutral-800 mb-4 -mt-8 text-base"] "Changed Field (1)"
        div_ [class_ "flex" ]$ do
          p_ [href_ "", class_ "text-sm underline underline-offset-1 mr-4"] "access_code"
          img_ [src_ "/assets/svgs/cheveron-right.svg", class_ " mr-4"] 
          p_ [href_ "", class_ "text-sm text-red-400 underline underline-offset-1 "] "access_code"
      a_ [class_ " mx-4 mt-5 absolute m-5 bottom-0 right-0  text-sm cursor-pointer text-slate-600" ] "Resolve"
    