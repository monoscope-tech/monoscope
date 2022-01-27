{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.Endpoints.EndpointList (endpointList) where

import Data.UUID as UUID
import Lucid
import Lucid.HTMX
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant.HTML.Lucid
import Text.RawString.QQ

import Models.Apis.Endpoints
import Data.Vector (Vector)
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics ()
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne,
                                       query_)

endPointHandler :: DBT IO (Vector Endpoint) 
endPointHandler = do
  maybeEndpoint <- query_ Select (_select @Endpoint)
  return maybeEndpoint

endpointList :: Maybe Endpoint -> Html ()
endpointList endPointHandler = bodyWrapper "Endpoint List" $ do
  div_ [class_ "container mx-auto px-4"] $ do
    div_ [class_ "flex justify-between"] $ do
      h3_ [class_ "text-xl text-slate-700"] "w-bank Endpoints"
      div_ [class_ "flex flex-row"] $ do
        button_ [class_ "bg-white rounded-xl py-2 px-4 m-3 h-10 flex flex-row"] $ do
          img_ [src_ "/assets/svgs/download.svg", class_ "h-4 w-6"]
          span_ [class_ "text-sm"] "Export" 
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
        button_ [class_ "bg-blue-700 h-10  px-2 rounded-xl py-1 mt-3 "] $ do
          img_ [src_ "/assets/svgs/white-plus.svg", class_ "text-white h-4 w-6 text-bold"]

    -- search       
    div_ [class_ "card rounded-lg bg-white border-solid border-2 border-light-blue-500 p-5 overflow-x-auto"] $ do
      div_ [class_ "w-full flex flex-row m-3"] $ do 
        div_ [class_ "flex rounded-xl bg-white py-2 px-3 flex-row w-3/4 border-solid border border-gray-200 h-10"] $ do 
          img_ [src_ "/assets/svgs/search.svg", class_ "h-5 w-auto"]
          input_ [type_ "text", class_ "w-full h-full p-2 text-sm text-gray-400 font-normal focus:outline-none", placeholder_ "Search endpoints..."]
          img_ [src_ "/assets/svgs/filter.svg", class_ "h-5 w-auto self-end"]
        button_ [class_ "bg-blue-700/20 place-content-center py-2 px-4 w-28 mx-3 flex flex-row rounded-xl h-10"] $ do
          img_ [src_ "/assets/svgs/merge.svg", class_ "h-4 w-4 mt-1 "]
          span_ [class_ "text-blue-600"] "Merge"
        button_ [class_ "bg-transparent place-content-center py-2 px-4 w-28 mx-3 flex flex-row border-solid border border-gray-200 rounded-xl h-10"] $ do
          span_ [class_ "text-sm text-slate-600 mr-1"] "Actions"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 "]

      --table head
      table_ [class_ "table-auto w-full  mt-6"] $ do
        thead_ $ do
          tr_ [class_ "border-b border-b-slate-300 p-10ç "]$ do
            th_ [class_ "text-left "] $ do
              input_ [type_ "checkbox"]
            th_ [class_ "text-left text-sm text-gray-400 font-normal "] "ENDPOINTS"
            th_ [class_ "text-left"] ""
            th_ [class_ "text-left text-sm text-gray-400 font-normal"] "AVG RPM"
            th_ [class_ "text-left text-sm text-gray-400 font-normal"] "MEAN STATUS"
            th_ [class_ "text-left text-sm text-gray-400 font-normal"] "AVG LATENCY"
            th_ [class_ "text-left text-sm text-gray-400 font-normal"] "FLAG"
        tbody_ $ do
          tr_ [class_ "border-b border-b-slate-300"] $ do
            td_ [class_ "text-left pr-4 "] $ do
              input_ [type_ "checkbox"]
            td_ [class_ "flex flex-row"] $ do 
              div_ [class_ "w-25 rounded-lg text-center text-white bg-green-400 p-2 m-2"] "POST"
            td_ [class_ " text-base text-slate-500 font-normal"] (show endPointHandler)
            td_ [class_ " text-base text-slate-500 font-normal"] "api/students/tests/scores"
            td_ [class_ " text-sm text-gray-400 font-normal"] "4500"
            td_ [class_ " text-sm text-slate-500 font-normal"] "200"
            td_ [class_ " text-sm text-gray-400 font-normal"] "400ms"
            td_ [class_ "grid justify-items-end font-medium text-gray-400"] $ do 
              div_ [class_ "flex flex-row"] $ do
                img_ [src_ "/assets/svgs/alert-red.svg"]
                img_ [src_ "/assets/svgs/dots-vertical.svg"]

          tr_ [class_ "border-b border-b-slate-300"] $do
            td_ [class_ "text-left pr-4 "] $ do
              input_ [type_ "checkbox"]
            td_ [class_ "flex flex-row"] $ do 
              div_ [class_ "w-25 rounded-lg text-center text-white bg-orange-400 p-2 m-2"] "PATCH"
            td_ [class_ " text-base text-slate-500 font-normal"] "api/students/tests/scores"
            td_ [class_ " text-sm text-gray-400 font-normal"] "4500"
            td_ [class_ " text-sm text-slate-500 font-normal"] "200"
            td_ [class_ " text-sm text-gray-400 font-normal"] "400ms"
            td_ [class_ "grid justify-items-end font-medium text-gray-400"] $ do 
              div_ [class_ "flex flex-row"] $ do
                img_ [src_ "/assets/svgs/alert-red.svg"]
                img_ [src_ "/assets/svgs/dots-vertical.svg"]

      --table footer
      div_ [class_ "flex flex-row mt-5 justify-between"] $ do 
        div_ [class_ "flex flex-row"] $ do
          button_ [class_ "bg-transparent place-content-center py-2 px-3 mx-3 flex flex-row border-solid border border-gray-200 rounded-xl h-10"] $ do
            span_ [class_ "text-sm text-slate-500 mr-1"] "10"
            img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
          span_ [src_ "text-gray-200 mt-6 font-light text-base"] "Showing 1 - 10 of 100"
        div_ [class_ "flex flex-row"] $ do
          button_ [class_ "bg-gray-100 h-10 w-10 mx-1 px-2 flex flex-row rounded-xl py-3 place-content-center"] $ do
            img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "-mr-1"]
            img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "-ml-1"]
          button_ [class_ "bg-gray-100 h-10 w-10 mx-1 px-2 rounded-xl py-1 place-content-center"] $ do
            img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "ml-1"]
          button_ [class_ "bg-blue-700 h-10 w-10 mx-1 px-2 rounded-xl py-1"] $ do
            span_ [class_ "text-white text-bold"] "1"
          button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
            span_ [class_ "text-slate-700 text-bold"] "2"
          button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
            span_ [class_ "text-slate-700 text-bold"] "3"
          button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
            span_ [class_ "text-slate-700 text-bold"] "..."
          button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
            span_ [class_ "text-slate-700 text-bold"] "5"
          button_ [class_ "bg-[#304FFD]/20 place-content-center h-10 mx-1 rounded-xl py-1 w-10"] $ do 
            img_ [src_ "/assets/svgs/arrowright1.svg", class_ "ml-3"]
          button_ [class_ "bg-blue-700/20 place-content-center h-10 mx-1 flex flex-row rounded-xl py-3 w-10"] $ do 
            img_ [src_ "/assets/svgs/arrowright1.svg", class_ "-mr-1"]
            img_ [src_ "/assets/svgs/arrowright1.svg", class_ "-ml-1"]



            



        

      
  
        
