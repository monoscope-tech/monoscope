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
  div_ [class_ "flex flex-row justify-between"]$ do
    div_ [class_ "flex flex-row"] $ do
      h3_ [class_ "text-xl text-slate-700"] "GET /users/acc_details"
      img_ [src_ "/assets/svgs/down_chevron.svg"]
    div_ [class_ "flex flex-row"] $ do
      button_ [class_ "bg-white "] $ img_ [src_ "/assets/svgs/filter.svg"]
      a_ [href_ "#", class_ "flex flex-row"] $ do
        span_ "Download Swagger"
        img_ [src_ "/assets/svgs/down_chevron.svg", class_ "p-2 bg-navy"]
