{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}




module Pages.Projects.CreateProject (
  CreateProjectForm,
  createProjectGetH,
  createProjectPostH) where

import Lucid
import Lucid.HTMX
import Relude
import Servant.HTML.Lucid
import Text.RawString.QQ
import Pages.BodyWrapper (bodyWrapper)
import Control.Concurrent (forkIO)
import Servant
  ( Handler,
  )
import Web.FormUrlEncoded (FromForm) 
import Data.Valor (Valor, Valid, check1, failIf, validateM)
import qualified Data.Valor as Valor
import qualified Data.Text as T
import Data.Default
import qualified Models.Projects.Projects as Projects
import qualified Data.Vector as Vector


data CreateProjectForm = CreateProjectForm 
  { title :: Text,
    description :: Text
  } deriving (Eq, Show, Generic)
    deriving anyclass (FromForm, Default)

data CreateProjectFormError = CreateProjectFormError
  { titleE :: Maybe [String],
    descriptionE :: Maybe [String]
  } deriving (Eq, Show, Generic)
    deriving anyclass (Default)

createProjectFormToModel :: CreateProjectForm -> Projects.CreateProject 
createProjectFormToModel CreateProjectForm{..} = Projects.CreateProject{..}

createProjectFormV :: Monad m => Valor CreateProjectForm m CreateProjectFormError 
createProjectFormV = CreateProjectFormError 
    <$> check1 title (failIf ["name can't be empty"] T.null)
    <*> check1 description Valor.pass 


createProjectGetH :: Handler (Html ())
createProjectGetH  = do
  pure $ bodyWrapper "Create Project" $ createProjectBody (def @CreateProjectForm) (def @CreateProjectFormError) 


createProjectPostH :: CreateProjectForm -> Handler (Html ())
createProjectPostH createP = do
  validationRes <- validateM createProjectFormV createP  

  traceShowM validationRes
  traceShowM createP

  case validationRes of 
    Left cp -> do
      -- Projects.insertProject (createProjectFormToModel cp)
      pure $ createProjectBody (Valor.unValid cp) (def @CreateProjectFormError) 
    Right cpe -> pure $ createProjectBody createP cpe 

createProjectBody :: CreateProjectForm -> CreateProjectFormError -> Html ()
createProjectBody  cp cpe = do 
  section_ [id_ "main-content"] $ do
    h2_ [class_ "text-slate-700 text-2xl font-medium mb-5" ] "Create Project"
    form_ [class_ "relative px-10 border border-gray-200 py-10  bg-white w-1/2 rounded-3xl", hxPost_ "/projects/new", hxTarget_ "#main-content"] $ do
      div_ $ do
        label_ [class_ "text-gray-500 mx-2 font-light text-base"] "Title"
        input_ [class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ",
               type_ "text",
                 id_ "title" ,
                name_ "title" 
               ]
      div_ $ do
        label_ [class_ "text-gray-500 mt-5 mx-2 font-light text-base"] "Description"
        textarea_ [
          class_ " py-2 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl border-0 ", 
          rows_ "4", placeholder_ "Description",
          id_ "description",
          name_ "description" 
          ] ""
      div_ $ do
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
      button_ [class_ "py-2 px-5 bg-blue-700 absolute m-5 bottom-0 right-0 text-[white] text-sm rounded-xl cursor-pointer", type_ "submit"] "Next step"

