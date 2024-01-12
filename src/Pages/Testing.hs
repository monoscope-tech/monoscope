module Pages.Testing (testingGetH) where

import Config
import Data.Default (def)
import Lucid
import Lucid.Hyperscript
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.List ((!!))
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, withPool)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pages.NonMember
import Pkg.Components (loader)
import Servant (Headers, addHeader)
import Servant.Htmx (HXRedirect, HXTrigger)
import Utils

import Web.FormUrlEncoded (FromForm)


testingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
testingGetH sess pid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      project <- liftIO $
        withPool
          pool
          do
            Projects.selectProjectForUser (Sessions.userId sess, pid)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "About project"
              }
      pure $ bodyWrapper bwconf $ testingPage pid


testingPage :: Projects.ProjectId -> Html ()
testingPage pid = do
  div_ [class_ "w-full"] do
    modal
    div_ [class_ "w-full mt-4 max-w-6xl mx-auto"] do
      div_ [class_ "flex justify-between border-b py-2 items-center"] do
        h1_ [class_ "text-3xl font-bold"] "Test Collections"
        button_
          [ class_ "text-white rounded bg-blue-500 px-4 py-2 flex items-center gap-2"
          , [__|on click remove .hidden from #col-modal|]
          ]
          do
            faIcon_ "fa-plus" "fa-light fa-plus" "h-6 w-6"
            "Collection"
      div_ [class_ "w-full grid grid-cols-2 gap-4 mt-8"] do
        collectionCard
        collectionCard
        collectionCard


collectionCard :: Html ()
collectionCard = do
  div_ [class_ "rounded-xl border  p-4  flex flex-col gap-5 text-gray-700 h-full shadow"] $ do
    a_ [href_ ("/p/testing/" <> "test_id")] $ do
      div_ [class_ "flex flex-col gap-5"] $ do
        div_ [class_ "flex items-center justify-between"] $ do
          div_ [class_ "flex flex-col gap-1"] $ do
            span_ [class_ "text-sm font-medium"] "Created at"
            span_ [class_ "text-xs text-gray-500"] "2023/10/22, 09:00"
          div_ [class_ "flex flex-col gap-1"] $ do
            span_ [class_ "text-sm font-medium"] "Last modified"
            span_ [class_ "text-xs text-gray-500"] "2023/10/22, 09:00"
        div_ [class_ "flex flex-col w-full gap-2"] $ do
          h3_ [class_ "font-semibold tracking-tight text-xl"] "Testing user profile"
          p_ [class_ "text-sm text-gray-500 break-words"] "user profile for making sure everything works"
          div_ [class_ "flex gap-2 items-center text-xs rounded py-1"] $ do
            span_ [class_ "font-bold"] "Last run"
            span_ [class_ "text-gray-500"] "2023/10/22, 09:00"
    div_ [class_ "text-sm flex items-center justify-between"] $ do
      div_ [class_ "flex gap-5 items-center"] $ do
        div_ [class_ "flex gap-2  rounded bg-gray-100 px-2 py-1"] $ do
          span_ "Steps"
          span_ [class_ "text-blue-500 font-medium"] "12"
        div_ [class_ "flex gap-2 rounded bg-gray-100 px-2 py-1"] $ do
          span_ "Passed"
          span_ [class_ "text-green-500 font-medium"] "10"
        div_ [class_ "flex gap-2 rounded bg-gray-100 px-2 py-1"] $ do
          span_ "Failed"
          span_ [class_ "text-red-500 font-medium"] "2"
      button_ [[__|on click remove .hidden from #col-modal|]] do
        faIcon_ "fa-edit" "fa-light fa-edit" "h-6 w-6"


data EditCol = EditCol {title :: Text, description :: Text}


modal :: Html ()
modal = do
  div_
    [ class_ "fixed inset-0 z-50 w-screen hidden overflow-y-auto bg-gray-300 bg-opacity-50"
    , id_ "col-modal"
    , [__|on click add .hidden to me|]
    ]
    $ do
      div_ [class_ "flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0"] $ do
        div_ [class_ "relative transform overflow-hidden rounded-xl border shadow bg-white text-left transition-all my-8 w-full max-w-2xl", [__|on click halt|]] $ do
          form_ [class_ "bg-white pb-4"] $ do
            h3_ [class_ "text-2xl w-full px-6 py-4 border-b font-semibold leading-6 text-gray-700", id_ "modal-title"] "New Collection"
            div_ [class_ "px-6 mt-4 items-start flex flex-col gap-5 text-gray-600"] $ do
              input_ [type_ "hidden", id_ "collection_id", name_ "col_id"]
              div_ [class_ "flex flex-col gap-1 w-full"] $ do
                label_ [Lucid.for_ "title", class_ "text-sm font-medium leading-none"] "Title"
                input_
                  [ type_ "text"
                  , name_ "title"
                  , id_ "title"
                  , class_ "flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                  , placeholder_ "Test Profile edit"
                  ]
              div_ [class_ "flex flex-col gap-1 w-full"] $ do
                label_ [Lucid.for_ "desc", class_ "text-sm font-medium leading-none"] "Description"
                textarea_
                  [ type_ "text"
                  , name_ "desc"
                  , id_ "desc"
                  , class_ "flex h-16 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                  , placeholder_ "Test Profile edit"
                  ]
                  ""
          div_ [class_ "px-4 py-3 sm:flex sm:flex-row-reverse sm:px-6 border-t mt-4"] $ do
            button_
              [ type_ "button"
              , class_ "inline-flex w-full justify-center rounded-md bg-blue-500 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-blue-600 sm:ml-3 sm:w-[100px]"
              ]
              "Save"
            button_
              [ type_ "button"
              , [__|on click add .hidden to #col-modal|]
              , class_ "mt-3 inline-flex w-full justify-center rounded-md bg-white px-3 py-2 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 sm:mt-0 sm:w-[100px]"
              ]
              "Cancel"
