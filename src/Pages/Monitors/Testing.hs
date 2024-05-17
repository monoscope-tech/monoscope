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
import Lucid (
  Html,
  Term (term),
  ToHtml (toHtml),
  a_,
  button_,
  class_,
  div_,
  for_,
  form_,
  h1_,
  h3_,
  href_,
  id_,
  input_,
  label_,
  name_,
  onclick_,
  placeholder_,
  script_,
  small_,
  span_,
  textarea_,
  type_,
 )
import Lucid.Htmx (hxBoost_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
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


data ParamInput = ParamInput
  { ackd :: Bool
  , archived :: Bool
  }
testingPostH :: Projects.ProjectId -> TestCollectionForm -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
testingPostH pid collection acknM archM = do
  (_, project) <- Sessions.sessionAndProject pid
  let ackd = textToBool <$> acknM
  let archived = textToBool <$> archM
  let paramInput =
        ParamInput
          { ackd = fromMaybe False ackd
          , archived = fromMaybe False archived
          }
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
              , collectionSteps = AE.Array []
              }
      _ <- dbtToEff $ Testing.addCollection coll
      cols <- dbtToEff $ Testing.getCollections pid
      addSuccessToast "Collection added Successfully" Nothing
      addRespHeaders $ testingPage paramInput pid cols
    else do
      -- _ <- dbtToEff $ Testing.updateCollection pid collection.collection_id collection.title collection.description
      cols <- dbtToEff $ Testing.getCollections pid
      addSuccessToast "Collection updated Successfully" Nothing
      addRespHeaders $ testingPage paramInput pid cols


testingGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
testingGetH pid acknM archM = do
  (sess, project) <- Sessions.sessionAndProject pid
  colls <- dbtToEff $ Testing.getCollections pid
  let ackd = textToBool <$> acknM
  let archived = textToBool <$> archM
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Testing"
          }
  let paramInput =
        ParamInput
          { ackd = fromMaybe False ackd
          , archived = fromMaybe False archived
          }
  addRespHeaders $ bodyWrapper bwconf $ testingPage paramInput pid colls


testingPage :: ParamInput -> Projects.ProjectId -> V.Vector Testing.CollectionListItem -> Html ()
testingPage paramInput pid colls = do
  div_ [class_ "w-full", id_ "main"] do
    modal pid
    div_ [class_ "w-full mt-4 max-w-7xl mx-auto"] do
      div_ [class_ "flex justify-between border-b py-2 items-center"] do
        h1_ [class_ "text-3xl font-bold"] "Test Collections"
        button_
          [ class_ "text-white rounded bg-blue-500 px-4 py-2 flex items-center gap-2"
          , [__|on click remove .hidden from #col-modal then set #collection_id's value to ""|]
          ]
          $ (faIcon_ "fa-plus" "fa-light fa-plus" "h-6 w-6" >> "Collection")
      div_ [class_ "py-2 px-2 space-x-6 border-b border-slate-20 mt-6 mb-8 text-sm font-light", hxBoost_ "true"] do
        let uri = "/p/" <> pid.toText <> "/testing/"
        a_ [class_ $ "inline-block py-2 " <> if not paramInput.ackd && not paramInput.archived then " font-bold text-black " else "", href_ $ uri] "Active"
        a_ [class_ $ "inline-block  py-2 " <> if paramInput.ackd && not paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&ackd=true"] "InActive"
        a_ [class_ $ "inline-block  py-2 " <> if paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&archived=true"] "Deactive"
      div_ [class_ "px-8 py-4"] do
        div_ [class_ "w-full card-round"] $ collectionCardList pid colls


collectionCardList :: Projects.ProjectId -> V.Vector Testing.CollectionListItem -> Html ()
collectionCardList pid colls = form_ [class_ "col-span-5 bg-white divide-y "] $ do
  div_ [class_ "flex py-3 gap-8 items-center  bg-gray-50"] do
    div_ [class_ "h-4 flex space-x-3 w-8"] do
      a_ [class_ " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", type_ "checkbox"]
    div_ [class_ " grow flex flex-row gap-2"] do
      button_ [class_ "btn btn-sm btn-outline border-black hover:shadow-2xl"] "✓ acknowlege"
    div_ [class_ "flex justify-center font-base w-60 content-between gap-14"] do
      span_ "Created at"
      div_ [class_ " space-x-2 font-base text-sm"] $ do
        a_ [class_ "cursor-pointer"] "24h"
        a_ [class_ "cursor-pointer font-bold text-base"] "14d"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "Last modified"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "Last run"
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "Schedule"
  div_ [class_ "w-full flex flex-row p-3"] $ do
    div_ [class_ "relative flex w-full bg-white py-2 px-3 border-solid border border-gray-200 h-10"] $ do
      faIcon_ "fa-magnifying-glass" "fa-light fa-magnifying-glass" "h-5 w-5"
      input_
        [ type_ "text"
        , [__| on input show .endpoint_item in #endpoints_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
        , class_ "dataTable-search w-full h-full p-2 text-gray-500 font-normal focus:outline-none"
        , placeholder_ "Search endpoints..."
        ]
  forM_ colls \c -> do
    collectionCard pid c


collectionCard :: Projects.ProjectId -> Testing.CollectionListItem -> Html ()
collectionCard pid col = do
  div_ [class_ "flex py-4 gap-8 items-center endpoint_item"] do
    div_ [class_ "h-4 flex space-x-3 w-8 "] do
      a_ [class_ " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input", type_ "checkbox"]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] $ do
          span_ $ toHtml col.title
        small_ [class_ "inline-block text-gray-800"] $ toHtml $ T.take 19 $ show @Text col.createdAt
        div_ [class_ "flex items-center gap-2 mt-5"] do
          a_ [href_ ("/p/" <> pid.toText <> "/testing/" <> col.id.toText)] $ do
            span_ [class_ "text-xs text-gray-500"] $ toHtml $ T.take 19 $ show @Text col.updatedAt
            span_ [class_ "text-gray-500"] $ toHtml $ maybe "-" (T.take 19 . show @Text) col.lastRun
    div_ [class_ "flex items-center justify-center "] do
      div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs w-96"] do
        div_ [class_ "p-2 bg-emerald-100 text-emerald-900 border border-emerald-300"] do
          div_ [class_ "text-base"] $ show col.stepsCount
          small_ [class_ "block"] "Steps"
        div_ [class_ " p-2 bg-slate-100 text-slate-900 border border-slate-300"] do
          div_ [class_ "text-base"] "-"
          small_ [class_ "block"] "Passed"
        div_ [class_ "p-2  bg-rose-100 text-rose-900 border border-rose-300"] do
          div_ [class_ "text-base"] "-"
          small_ [class_ "block"] "Failed"
    div_ [class_ "w-36 flex items-center justify-center"] do
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
        $ faSprite_ "pen-to-square" "regular" "h-6 w-6"


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
      script_
        [text|
          function noPropagation(event) {
            event.stopPropagation();
        }
      |]
