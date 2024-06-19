module Pages.Monitors.Testing (
  testingGetH,
  testingPostH,
  TestCollectionForm (..),
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Text qualified as T
import Data.Time (getZonedTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxBoost_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.Components.ItemsList qualified as ItemsList
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Utils
import Web.FormUrlEncoded (FromForm)


data TestCollectionForm = TestCollectionForm
  { collection_id :: Text
  , title :: Text
  , description :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


data ScheduleForm = ScheduleForm
  { schedule :: Maybe Text
  , isScheduled :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, AE.FromJSON)


testingPostH :: Projects.ProjectId -> TestCollectionForm -> ATAuthCtx (RespHeaders (Html ()))
testingPostH pid collection = do
  (_, project) <- Sessions.sessionAndProject pid
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
              , config = AE.object []
              , schedule = "1 day"
              , isScheduled = False
              , collectionSteps = Testing.CollectionSteps V.empty
              }
      _ <- dbtToEff $ Testing.addCollection coll
      cols <- dbtToEff $ Testing.getCollections pid Testing.Active
      addSuccessToast "Collection added Successfully" Nothing
      testingGetH pid Nothing
    else do
      -- _ <- dbtToEff $ Testing.updateCollection pid collection.collection_id collection.title collection.description
      cols <- dbtToEff $ Testing.getCollections pid Testing.Active
      addSuccessToast "Collection updated Successfully" Nothing
      testingGetH pid Nothing


testingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
testingGetH pid maybeTab = do
  let tabStatus = case maybeTab of
        Just "Active" -> Testing.Active
        Just "Inactive" -> Testing.Inactive
        _ -> Testing.Active -- Default to Active if tab is unrecognized
  currTime <- Time.currentTime
  (sess, project) <- Sessions.sessionAndProject pid
  colls <- dbtToEff $ Testing.getCollections pid tabStatus
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = ""
          , currentURL = "/p/" <> pid.toText <> "/testing"
          , currTime
          , nextFetchUrl = Nothing
          , tabsFilter = Nothing
          , heading = Nothing
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No Multistep Test/Monitor yet."
                  , description = "You're can create one to start monitoring your services."
                  , actionText = "Create Monitor"
                  , destination = "/p/" <> listCfg.projectId.toText <> "/integration_guides#outgoing-request-monitoring"
                  }
          , elemID = "anomalyListForm"
          }
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "API Tests (Beta)"
          }
  addRespHeaders $ bodyWrapper bwconf $ testingPage listCfg pid maybeTab colls


testingPage :: ItemsList.ItemsListCfg -> Projects.ProjectId -> Maybe Text -> V.Vector Testing.CollectionListItem -> Html ()
testingPage listCfg pid maybeTab testItems = do
  div_ [class_ "w-full", id_ "main"] do
    modal pid
    div_ [class_ "w-full mx-auto px-16 pt-5 pb-24 overflow-y-scroll h-full"] $ do
      h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center"] do
        span_ [] "Multistep API monitors/tests (Beta)"
        button_
          [ class_ "w-max btn btn-indigo text-md"
          , [__|on click remove .hidden from #col-modal then set #collection_id's value to ""|]
          ]
          $ (faSprite_ "plus" "regular" "h-6 w-6" >> "Collection")
      div_ [class_ "py-2 px-2 space-x-6 border-b border-slate-20 mt-6 mb-8 text-sm font-light", hxBoost_ "true"] do
        a_ [class_ $ "inline-block py-2 " <> if maybeTab == Just "Active" then "font-bold text-black" else "", href_ ("/p/" <> pid.toText <> "/testing/?tab=Active")] "Active"
        a_ [class_ $ "inline-block py-2 " <> if maybeTab == Just "Inactive" then "font-bold text-black" else "", href_ ("/p/" <> pid.toText <> "/testing/?tab=Inactive")] "InActive"
      ItemsList.itemsList_ listCfg testItems \_ -> collectionCard pid


collectionCard :: Projects.ProjectId -> Testing.CollectionListItem -> Html ()
collectionCard pid col = div_ [class_ "flex py-4 gap-8 items-center"] do
  div_ [class_ "h-4 flex space-x-3 w-8 "] do
    a_ [class_ "w-2 h-full"] ""
    input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input", type_ "checkbox", name_ "listItemId", value_ col.id.toText]

  div_ [class_ "space-y-3 grow"] do
    div_ [class_ ""] do
      a_ [href_ $ "/p/" <> pid.toText <> "/testing/" <> col.id.toText, class_ "inline-block font-bold text-blue-700 space-x-2"] $ toHtml col.title
      div_ [class_ "mt-5"] do
        div_ [class_ "flex items-center gap-2"] do
          span_ [class_ "text-xs text-gray-500 font-bold"] "Created"
          span_ [class_ "inline-block text-gray-800 text-xs"] $ toHtml $ T.take 19 $ show @Text col.createdAt
        div_ [class_ "flex items-center gap-2"] do
          span_ [class_ "text-xs text-gray-500 font-bold"] "Last run"
          span_ [class_ "text-gray-500"] $ toHtml $ maybe "-" (T.take 19 . show @Text) col.lastRun
  div_ [class_ "flex items-center justify-center "] do
    div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs w-96"] do
      div_ [class_ "p-2 bg-slate-100 text-slate-900 border border-slate-300"] do
        div_ [class_ "text-base"] $ show col.stepsCount
        small_ [class_ "block"] "Steps"
      div_ [class_ " p-2 bg-emerald-100 text-emerald-900 border border-emerald-300"] do
        div_ [class_ "text-base"] "-"
        small_ [class_ "block"] "Passed"
      div_ [class_ "p-2  bg-rose-100 text-rose-900 border border-rose-300"] do
        div_ [class_ "text-base"] "-"
        small_ [class_ "block"] "Failed"


modal :: Projects.ProjectId -> Html ()
modal pid = do
  div_
    [ class_ "fixed inset-0 z-50 w-screen hidden overflow-y-auto bg-gray-300 bg-opacity-50"
    , id_ "col-modal"
    , [__|on click add .hidden to me|]
    ]
    $ do
      div_ [class_ "flex min-h-full items-end justify-center p-4 text-center sm:items-center sm:p-0"] $ do
        div_ [class_ "relative transform overflow-hidden rounded-xl border shadow bg-white text-left transition-all my-8 w-full max-w-2xl", onclick_ "event.stopPropagation()"] do
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
                      , placeholder_ "Test Collection Title"
                      ]
                  div_ [class_ "flex flex-col gap-1 w-full"] $ do
                    label_ [Lucid.for_ "desc", class_ "text-sm font-semibold leading-none"] "Description"
                    textarea_
                      [ type_ "text"
                      , name_ "description"
                      , id_ "desc"
                      , class_ "flex h-16 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm transition-colors placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                      , placeholder_ "Description"
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
