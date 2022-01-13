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
import Servant.HTML.Lucid
import Text.RawString.QQ

endpointDetails :: Html ()
endpointDetails = bodyWrapper "Endpoint Details" $ do
  div_ $ do
    div_ $ do
      span_ "GET /users/acc_details"
      img_ [src_ "/assets/svgs/down_caret.svg"]
    div_ $ do
      button_ [class_ "bg-white "] $ img_ [src_ "settings.svg"]
      a_ [href_ "#", class_ ""] $ do
        span_ "Download Swagger"
        img_ [src_ "/assets/svgs/down_chevron.svg", class_ "p-2 bg-navy"]
