{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pages.Projects.ListProjects
  ( listProjectsGetH,
  )
where

import Config
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import qualified Models.Projects.Projects as Projects
import qualified Models.Users.Users as Users
import Optics.Operators
import Pages.BodyWrapper (bodyWrapper)
import Relude
import Servant
  ( Handler,
    addHeader,
    noHeader,
  )
import Servant.HTML.Lucid

listProjectsGetH :: DashboardM (Html ())
listProjectsGetH = do
  pool <- asks pool
  projects <- liftIO $ withPool pool $ Projects.selectProjectsForUser (Users.UserId UUID.nil)
  pure $ listProjectsBody projects

listProjectsBody :: Vector.Vector Projects.Project -> Html ()
listProjectsBody projects = do
  bodyWrapper "Projects" $
    section_ [id_ "main-content"] $ do
      div_ [class_ "flex justify-between mb-6"] $ do
        h2_ [class_ "text-slate-700 text-2xl font-medium"] "Projects"
        a_ [class_ "btn-indigo", href_ "/p/new"] "Create Project"
      section_ [] $ do
        div_ [class_ "bg-white shadow overflow-hidden sm:rounded-md"] $ do
          ul_ [role_ "list", class_ "divide-y divide-gray-200"] $ do
            projects
              & mapM_
                ( \project -> do
                    li_ $ do
                      a_ [href_ ("/p/" <> (UUID.toText $ Projects.unProjectId $ project ^. #id) <> "/dashboard"), class_ "block hover:bg-gray-50"] $ do
                        div_ [class_ "px-4 py-4 flex items-center sm:px-6"] $ do
                          div_ [class_ "min-w-0 flex-1 sm:flex sm:items-center sm:justify-between"] $ do
                            div_ [class_ "truncate"] $ do
                              div_ [class_ "text-sm"] $ do
                                p_ [class_ "block font-medium text-indigo-600 truncate py-2"] $ toHtml $ project ^. #title
                                p_ [class_ "block flex-shrink-0 font-normal text-gray-500"] $ toHtml $ project ^. #description
                              div_ [class_ "mt-2 flex"] $ do
                                div_ [class_ "flex items-center text-sm text-gray-500"] $ do
                                  small_ $ do
                                    span_ "Created on "
                                    time_ [datetime_ "2020-01-07"] "January 7, 2020"
                            div_ [class_ "mt-4 flex-shrink-0 sm:mt-0 sm:ml-5"] $ do
                              div_ [class_ "flex overflow-hidden -space-x-1"] $ do
                                img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-white", src_ "https://images.unsplash.com/photo-1506794778202-cad84cf45f1d?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80", alt_ "Dries Vincent"]
                                img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-white", src_ "https://images.unsplash.com/photo-1517841905240-472988babdf9?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80", alt_ "Lindsay Walton"]
                          div_ [class_ "ml-5 flex-shrink-0 text-gray-400"] $ do
                            img_ [src_ "/assets/svgs/right_chevron.svg"]
                )
