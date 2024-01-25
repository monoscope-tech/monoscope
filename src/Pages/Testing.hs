module Pages.Testing (
  testingGetH,
  testingPostH,
  testingPutH,
  collectionGetH,
  TestCollectionForm (..),
  collectionStepPostH,
  collectionStepPutH,
) where

import Config
import Control.Exception
import Data.Default (def)
import Foreign.C.String
import Foreign.C.Types
import NeatInterpolation (text)
import System.IO.Error

import Lucid
import Lucid.Hyperscript
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Database.PostgreSQL.Entity.DBT (withPool)
import Pages.NonMember
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import Utils

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Lucid.Base
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_)
import Models.Apis.Testing qualified as Testing
import Models.Users.Sessions qualified as Session
import Web.FormUrlEncoded (FromForm)


data TestCollectionForm = TestCollectionForm
  { collection_id :: Text
  , title :: Text
  , description :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, FromJSON)


-- data CollectionStep = CollectionStep
--   {
--     title :: Text,

--   }
--   deriving stock (Show, Generic)
--   deriving anyclass (FromJSON, ToJSON)

data ScheduleForm = ScheduleForm
  { schedule :: Maybe Text
  , isScheduled :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, FromJSON)


testingPutH :: Session.PersistentSession -> Projects.ProjectId -> Testing.CollectionId -> Text -> Value -> DashboardM (Html ())
testingPutH sess pid cid action steps = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      case action of
        "update_config" -> do
          _ <- withPool pool $ Testing.updateCollectionConfig cid steps
          pure ""
        "update_schedule" -> do
          let sch = decode (encode steps) :: Maybe ScheduleForm
          case sch of
            Just s -> do
              _ <- withPool pool $ Testing.updateSchedule cid s.schedule s.isScheduled
              pure ""
            Nothing -> do
              pure ""
        _ -> do
          pure ""


testingPostH :: Sessions.PersistentSession -> Projects.ProjectId -> TestCollectionForm -> DashboardM (Headers '[HXTrigger] (Html ()))
testingPostH sess pid collection = do
  pool <- asks pool
  project <- liftIO $
    withPool
      pool
      do
        Projects.selectProjectForUser (Sessions.userId sess, pid)
  if collection.collection_id == ""
    then do
      currentTime <- liftIO getZonedTime
      colId <- Testing.CollectionId <$> liftIO UUIDV4.nextRandom
      let coll =
            Testing.Collection
              { id = colId
              , createdAt = currentTime
              , projectId = pid
              , updatedAt = currentTime
              , lastRun = Nothing
              , title = collection.title
              , description = collection.description
              , config = Aeson.object []
              , schedule = Nothing
              , isScheduled = False
              }
      _ <- withPool pool do Testing.addCollection coll
      cols <- withPool pool do Testing.getCollections pid
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "", "successToast": ["Collection added Successfully"]}|]
      pure $ addHeader hxTriggerData $ testingPage pid cols
    else do
      _ <- withPool pool do Testing.updateCollection pid collection.collection_id collection.title collection.description
      cols <- withPool pool do Testing.getCollections pid
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "", "successToast": ["Collection updated Successfully"]}|]
      pure $ addHeader hxTriggerData $ testingPage pid cols


testingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
testingGetH sess pid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, colls) <- liftIO $
        withPool
          pool
          do
            project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
            colls <- Testing.getCollections pid
            pure (project, colls)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Testing"
              }
      pure $ bodyWrapper bwconf $ testingPage pid colls


testingPage :: Projects.ProjectId -> V.Vector Testing.CollectionListItem -> Html ()
testingPage pid colls = do
  div_ [class_ "w-full", id_ "main"] do
    modal pid
    div_ [class_ "w-full mt-4 max-w-7xl mx-auto"] do
      div_ [class_ "flex justify-between border-b py-2 items-center"] do
        h1_ [class_ "text-3xl font-bold"] "Test Collections"
        button_
          [ class_ "text-white rounded bg-blue-500 px-4 py-2 flex items-center gap-2"
          , [__|on click remove .hidden from #col-modal then set #collection_id's value to ""|]
          ]
          do
            faIcon_ "fa-plus" "fa-light fa-plus" "h-6 w-6"
            "Collection"
      div_ [class_ "w-full grid grid-cols-2 gap-8 mt-8"] do
        forM_ colls \c -> do
          collectionCard pid c


collectionCard :: Projects.ProjectId -> Testing.CollectionListItem -> Html ()
collectionCard pid col = do
  div_ [class_ "rounded-xl border p-4 flex flex-col gap-5 text-gray-700 h-full shadow hover:shadow-lg"] $ do
    a_ [href_ ("/p/" <> pid.toText <> "/testing/" <> col.id.toText)] $ do
      div_ [class_ "flex flex-col gap-5"] $ do
        div_ [class_ "flex items-center justify-between"] $ do
          div_ [class_ "flex flex-col gap-1"] $ do
            span_ [class_ "text-sm font-medium"] "Created at"
            span_ [class_ "text-xs text-gray-500"] $ toHtml $ T.take 19 $ toText $ show col.createdAt
          div_ [class_ "flex flex-col gap-1"] $ do
            span_ [class_ "text-sm font-medium"] "Last modified"
            span_ [class_ "text-xs text-gray-500"] $ toHtml $ T.take 19 $ toText $ show col.updatedAt
        div_ [class_ "flex flex-col w-full gap-2"] $ do
          h3_ [class_ "font-semibold tracking-tight text-xl"] $ toHtml col.title
          p_ [class_ "text-sm text-gray-500 break-words max-w-4xl"] $ toHtml col.description
          div_ [class_ "flex justify-between items-center"] do
            div_ [class_ "flex gap-2 items-center text-xs rounded py-1"] $ do
              span_ [class_ "font-bold"] "Last run"
              span_ [class_ "text-gray-500"] $ toHtml $ maybe "-" (T.take 19 . toText . show) col.lastRun
            div_ [class_ "flex gap-2 items-center text-xs rounded py-1"] $ do
              span_ [class_ "font-bold"] "Schedule"
              span_ [class_ "text-gray-500"] $ toHtml $ maybe "-" (T.take 19 . toText . show) col.lastRun
    div_ [class_ "text-sm flex items-center justify-between"] $ do
      div_ [class_ "flex gap-5 items-center"] $ do
        div_ [class_ "flex gap-2  rounded bg-gray-100 px-2 py-1"] $ do
          span_ "Steps"
          span_ [class_ "text-blue-500 font-medium"] $ show col.stepsCount
        div_ [class_ "flex gap-2 rounded bg-gray-100 px-2 py-1"] $ do
          span_ "Passed"
          span_ [class_ "text-green-500 font-medium"] "-"
        div_ [class_ "flex gap-2 rounded bg-gray-100 px-2 py-1"] $ do
          span_ "Failed"
          span_ [class_ "text-red-500 font-medium"] "-"
      button_
        [ term "data-id" col.id.toText
        , term "data-title" col.title
        , term "data-desc" col.description
        , [__|on click remove .hidden from #col-modal 
               then set #collection_id's value to my @data-id
               then set #title's value to my @data-title 
               then set #desc's value to my @data-desc
               |]
        ]
        do
          faIcon_ "fa-edit" "fa-light fa-edit" "h-6 w-6"


modal :: Projects.ProjectId -> Html ()
modal pid = do
  div_
    [ class_ "fixed inset-0 z-50 w-screen hidden overflow-y-auto bg-gray-300 bg-opacity-50"
    , id_ "col-modal"
    , [__|on click add .hidden to me|]
    ]
    $ do
      div_ [class_ "flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0"] $ do
        div_ [class_ "relative transform overflow-hidden rounded-xl border shadow bg-white text-left transition-all my-8 w-full max-w-2xl", onclick_ "noPropagation(event)"] do
          form_
            [ hxPost_ $ "/p/" <> pid.toText <> "/testing"
            , class_ "w-full"
            , hxTarget_ "#main"
            , hxSwap_ "outerHTML"
            ]
            $ do
              div_ [class_ "bg-white pb-4"] $ do
                h3_ [class_ "text-2xl w-full px-6 py-4 border-b font-semibold leading-6 text-gray-700", id_ "modal-title"] "New Collection"
                div_ [class_ "px-6 mt-4 items-start flex flex-col gap-5 text-gray-700"] $ do
                  input_ [type_ "hidden", id_ "collection_id", name_ "collection_id"]
                  div_ [class_ "flex flex-col gap-1 w-full"] $ do
                    label_ [Lucid.for_ "title", class_ "text-sm font-semibold leading-none"] "Title"
                    input_
                      [ type_ "text"
                      , name_ "title"
                      , id_ "title"
                      , class_ "flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                      , placeholder_ "Test Profile edit"
                      ]
                  div_ [class_ "flex flex-col gap-1 w-full"] $ do
                    label_ [Lucid.for_ "desc", class_ "text-sm font-semibold leading-none"] "Description"
                    textarea_
                      [ type_ "text"
                      , name_ "description"
                      , id_ "desc"
                      , class_ "flex h-16 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                      , placeholder_ "Test Profile edit"
                      ]
                      ""
              div_ [class_ "px-4 py-3 sm:flex sm:flex-row-reverse sm:px-6 border-t mt-4"] $ do
                button_
                  [ type_ "submit"
                  , class_ "inline-flex w-full justify-center rounded-md bg-blue-500 px-3 py-2 text-sm font-semibold text-white shadow-sm hover:bg-blue-600 sm:ml-3 sm:w-[100px]"
                  ]
                  "Save"
                button_
                  [ type_ "button"
                  , [__|on click add .hidden to #col-modal|]
                  , class_ "mt-3 inline-flex w-full justify-center rounded-md bg-white px-3 py-2 text-sm font-semibold text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 hover:bg-gray-50 sm:mt-0 sm:w-[100px]"
                  ]
                  "Cancel"
      script_
        [text|
          function noPropagation(event) {
      event.stopPropagation();
    }
      |]


collectionStepPostH :: Sessions.PersistentSession -> Projects.ProjectId -> Testing.CollectionId -> Value -> DashboardM (Html ())
collectionStepPostH sess pid cid step_val = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      currentTime <- liftIO getZonedTime
      step_id <- Testing.CollectionStepId <$> liftIO UUIDV4.nextRandom
      let newStep =
            Testing.CollectionStep
              { id = step_id
              , createdAt = currentTime
              , updatedAt = currentTime
              , lastRun = Nothing
              , projectId = pid
              , collectionId = cid
              , stepData = step_val
              }
      _ <- withPool pool $ Testing.addCollectionStep newStep
      pure ""


collectionStepPutH :: Sessions.PersistentSession -> Projects.ProjectId -> Testing.CollectionStepId -> Value -> DashboardM (Html ())
collectionStepPutH sess pid csid value = do
  pool <- asks pool
  _ <- withPool pool $ Testing.updateCollectionStep csid value
  pure ""


-- Import the foreign function from the Rust library
foreign import ccall unsafe "haskell_binding" haskellBinding :: Int


collectionGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Testing.CollectionId -> DashboardM (Html ())
collectionGetH sess pid col_id = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      collection <- withPool pool $ Testing.getCollectionById col_id
      project <- withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)
      collection_steps <- withPool pool $ Testing.getCollectionSteps col_id
      -- _ <- case collection of
      --   Just c -> do
      --     let col_json = decodeUtf8 $ encode c
      --     v <- liftIO $ withCString col_json $ \c_str -> do
      --       traceShowM c_str
      --       -- let bs = haskellBinding
      --       -- traceShowM bs
      --       pure ""
      --     pure ("" :: String)
      --   Nothing -> do
      --     pure ""
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Testing"
              }
      pure $ bodyWrapper bwconf $ collectionPage pid collection collection_steps


collectionPage :: Projects.ProjectId -> Maybe Testing.Collection -> V.Vector Testing.CollectionStep -> Html ()
collectionPage pid col steps = do
  div_ [] do
    case col of
      Just c -> do
        let col_json = decodeUtf8 $ encode c
        let steps_json = decodeUtf8 $ encode steps
        div_ [id_ "test-data", term "data-collection" col_json, term "data-steps" steps_json] do
          termRaw "test-editor" [id_ "testEditorElement"] ("" :: Text)
      Nothing -> do
        h1_ [] "Not found"
  script_ [type_ "module", src_ "/assets/testeditor.js"] ("" :: Text)
  script_ [src_ "https://unpkg.com/js-yaml/dist/js-yaml.min.js", crossorigin_ "true"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.js"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/mode/yaml/yaml.js"] ("" :: Text)
  style_
    [text|
        .CodeMirror {
            height: 100%;
        }
    |]

-- runTestH :: Sessions.PersistentSession -> Projects.ProjectId -> Testing.CollectionId -> DashboardM (Html ())
-- runTestH sess pid col_id = do
--   pool <- asks pool
--   collection <- withPool pool $ Testing.getCollectionById col_id
--   _ <- case collection of
--     Just c -> do
--       let col_json = decodeUtf8 $ encode c
--       v <- liftIO $ withCString col_json $ \c_str -> do
--         traceShowM c_str
--         bs <- haskellBinding c_str
--         traceShowM bs
--         pure bs
--       pure ("" :: String)
--     Nothing -> do
--       pure ""
--   pure $ div_ [] "s"