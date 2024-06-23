module Pages.Monitors.Testing (
  testingGetH,
  testingPostH,
  CollectionListItemVM(..),
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUIDV4
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxExt_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxVals_)
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Data.Vector qualified as V
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pkg.Components qualified as Components
import Pkg.Components.ItemsList qualified as ItemsList
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Utils


testingPostH :: Projects.ProjectId -> TestCollectionEditor.CollectionStepUpdateForm 
             -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage CollectionListItemVM)))
testingPostH pid colF = do
  (_, project) <- Sessions.sessionAndProject pid
  currentTime <- Time.currentTime
  colId <- Testing.CollectionId <$> liftIO UUIDV4.nextRandom
  let coll =
        Testing.Collection
          { id = colId
          , createdAt = currentTime
          , projectId = pid
          , updatedAt = currentTime
          , lastRun = Nothing
          , title = fromMaybe "" colF.title
          , description = fromMaybe "" colF.description
          , config = AE.object []
          , schedule = ((fromMaybe "" colF.scheduleNumber) <> " " <> fromMaybe "" colF.scheduleNumberUnit)
          , isScheduled = colF.scheduled == Just "on"
          , collectionSteps = Testing.CollectionSteps (colF.stepsData)
          }
  _ <- dbtToEff $ Testing.addCollection coll
  addSuccessToast "Collection added Successfully" Nothing
  testingGetH pid Nothing


testingGetH :: Projects.ProjectId -> Maybe Text 
            -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage CollectionListItemVM)))
testingGetH pid filterTM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (currentFilterTab, tabStatus) = case filterTM of
        Just "Active" -> ("Active", Testing.Active)
        Just "Inactive" -> ("Inactive", Testing.Inactive)
        _ -> ("Active", Testing.Active)
  currTime <- Time.currentTime
  colls <- dbtToEff $ Testing.getCollections pid tabStatus
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = Nothing
          , currentURL = "/p/" <> pid.toText <> "/testing?"
          , currTime
          , nextFetchUrl = Nothing
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing}
          , tabsFilter =
              Just $
                ItemsList.TabFilter
                  { current = currentFilterTab
                  , options =
                      [ ItemsList.TabFilterOpt{name = "Active", count = Nothing}
                      , ItemsList.TabFilterOpt{name = "Inactive", count = Nothing}
                      ]
                  }
          , bulkActions =
              [ ItemsList.BulkAction{icon = Just "check", title = "deactivate", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowlege"}
              ]
          , heading =
              Just $
                ItemsList.Heading
                  { pageTitle = "Multistep API monitors/tests (Beta)"
                  , rightComponent =
                      Just
                        $ Components.modal_ "test-settings-modal" (span_ [class_ "btn btn-sm btn-primary space-x-2"] $ Utils.faSprite_ "plus" "regular" "h-4" >> "new tests")
                        $ form_
                          [ hxPost_ $ "/p/" <> pid.toText <> "/testing"
                          , class_ "w-full"
                          , hxTarget_ "#itemsListPage"
                          , hxSelect_ "#itemsListPage"
                          , hxVals_ "js:{stepsData: []}"
                          , hxExt_ "json-enc"
                          , hxSwap_ "outerHTML"
                          ]
                        $ TestCollectionEditor.testSettingsModalContent_ False (def :: Testing.Collection)
                  , subSection = Nothing
                  }
          , zeroState =
              Just $
                ItemsList.ZeroState
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
  addRespHeaders $ PageCtx bwconf (ItemsList.ItemsPage listCfg $ V.map (CollectionListItemVM pid) colls)

data CollectionListItemVM = CollectionListItemVM Projects.ProjectId Testing.CollectionListItem

instance ToHtml CollectionListItemVM where 
  toHtml (CollectionListItemVM pid he) = toHtmlRaw $ collectionCard pid he
  toHtmlRaw = toHtml

collectionCard :: Projects.ProjectId -> Testing.CollectionListItem -> Html ()
collectionCard pid col = div_ [class_ "flex py-4 gap-8 items-center itemsListItem"] do
  div_ [class_ "h-4 flex space-x-3 w-8 "] do
    a_ [class_ "w-2 h-full"] ""
    input_ [term "aria-label" "Select Issue", class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox  checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "listItemId", value_ col.id.toText]

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
