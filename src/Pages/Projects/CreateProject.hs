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
  p_ [class_ "text-[#3F434A] text-[28px] font-medium mb-5" ] "Create Project"
  div_ [class_ "relative px-10 border border-[#E8E9EB] py-10 h-[650px] bg-[#FFFFFF] w-[535px] rounded-[30px]"] $ do
    p_ [class_ "text-[#8A9099] mx-2 font-light text-[14px]"] "Title"
    input_ [class_ "h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#000000] border-solid border border-[#E8E9EB] rounded-2xl border-0 " ]
    p_ [class_ "text-[#8A9099] mt-5 mx-2 font-light text-[14px]"] "Description"
    textarea_ [class_ " py-2 px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#000000] border-solid border border-[#E8E9EB] rounded-2xl border-0 ", rows_ "4", placeholder_ "Description"  ] "Description"  --Once i remove the "Description", it shows a "typecheck(-Wdeferred-type-errors)" error
    p_ [class_ "text-[#8A9099] mt-5 mx-2 font-light text-[14px]"] "Invite a project member"
    div_ [class_ "flex flex-row"] $ do
      input_ [class_ "w-2/3 h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#3F434A] border-solid border border-[#E8E9EB] rounded-2xl border-0 ", placeholder_ "anthony@gmail.com"  ]
      select_ [class_ "w-1/3 h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#707273] border-solid border border-[#E8E9EB] rounded-2xl border-0"] $ do
        option_ [class_ "text-[#8A9099]"] "Can Edit"
        option_ [class_ "text-[#8A9099]"] "Can View"
      img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
    div_ [class_ "flex flex-row"] $ do
      input_ [class_ "w-2/3 h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#3F434A] border-solid border border-[#E8E9EB] rounded-2xl border-0 ", placeholder_ "smile@yahoo.com"  ]
      select_ [class_ "w-1/3 h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#707273] border-solid border border-[#E8E9EB] rounded-2xl border-0"] $ do
        option_ [class_ "text-[#8A9099]"] "Can Edit"
        option_ [class_ "text-[#8A9099]"] "Can View"
      img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]
    div_ [class_ "flex flex-row"] $ do
      input_ [class_ "w-2/3 h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#3F434A] border-solid border border-[#E8E9EB] rounded-2xl border-0 ", placeholder_ "seniormanVic@gmail.com"  ]
      select_ [class_ "w-1/3 h-[40px] px-5 m-2 w-full text-sm bg-[#FFFFFF] text-[#707273] border-solid border border-[#E8E9EB] rounded-2xl border-0"] $ do
        option_ [class_ "text-[#8A9099]"] "Can Edit"
        option_ [class_ "text-[#8A9099]"] "Can View"
      img_ [src_ "/assets/svgs/delete.svg", class_ "cursor-pointer"]   
    div_ [class_ "flex flex-row cursor-pointer mt-2"] $ do
      img_ [src_ "/assets/svgs/blue-plus.svg", class_ "mx-2"]   
      span_ [class_ "text-[#304FFD] font-medium font-[14px] "] "Add member"
    a_ [class_ "py-2 px-5 bg-[#304FFD] absolute m-4 bottom-0 right-0 text-[white] text-[15px] rounded-[15px] cursor-pointer" ] "Next step"


  
  

  
