{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Pages.Projects.CreateProject (createProject) where

import Lucid
import Lucid.HTMX
import Relude
import Servant.HTML.Lucid
import Text.RawString.QQ
import Pages.BodyWrapper (bodyWrapper)

createProject :: Html ()
createProject = bodyWrapper "Create Project" $ do
  p_ [class_ "text-slate-700 text-2xl font-medium mb-5" ] "Create Project"
  div_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl"] $ do
    p_ [class_ "text-gray-500 mx-2 font-light text-base"] "Title"
    input_ [class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 " ]
    p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-base"] "Description"
    textarea_ [class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ", rows_ "4", placeholder_ "Description"  ] "Description"  --Once i remove the "Description", it shows a "typecheck(-Wdeferred-type-errors)" error
    p_ [class_ "text-gray-500 mt-5 mx-2 font-light text-sm"] "Invite a project member"
    div_ [class_ "flex flex-row space-x-3"] $ do
      input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "anthony@gmail.com"  ]
      select_ [class_ "w-1/3 h-10 px-5  my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
        option_ [class_ "text-gray-500"] "Can Edit"
        option_ [class_ "text-gray-500"] "Can View"
      img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
    div_ [class_ "flex flex-row space-x-3"] $ do
      input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "smile@yahoo.com"  ]
      select_ [class_ "w-1/3 h-10 px-5 my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
        option_ [class_ "text-gray-500"] "Can Edit"
        option_ [class_ "text-gray-500"] "Can View"
      img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
    div_ [class_ "flex flex-row space-x-3"] $ do
      input_ [class_ "w-2/3 h-10 px-5 my-2 w-full text-sm bg-white text-slate-700 border-solid border border-gray-200 rounded-2xl border-0 ", placeholder_ "seniormanVic@gmail.com"  ]
      select_ [class_ "w-1/3 h-10 px-5 my-2 w-full text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl border-0"] $ do
        option_ [class_ "text-gray-500"] "Can Edit"
        option_ [class_ "text-gray-500"] "Can View"
      img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]   
    div_ [class_ "flex flex-row cursor-pointer mt-2"] $ do
      img_ [src_ "/assets/svgs/blue-plus.svg", class_ "mx-2"]   
      span_ [class_ "text-blue-700 font-medium text-base "] "Add member"
    a_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer" ] "Next step"


  
  

  
