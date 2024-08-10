module Pages.Monitors.Testing (
  testingGetH,
  testingPostH,
  CollectionListItemVM (..),
  collectionDashboard,
)
where

import Control.Error.Util (hush)
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.List.Extra (nubOrd)
import Data.Time (UTCTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxExt_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxVals_)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (statBox)
import Pages.Log (ApiLogsPageData (isTestLog))
import Pages.Log qualified as Log
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pkg.Components qualified as Components
import Pkg.Components.ItemsList qualified as ItemsList
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.Time.Pretty (prettyTimeAuto)
import Utils


testingPostH
  :: Projects.ProjectId
  -> TestCollectionEditor.CollectionStepUpdateForm
  -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage CollectionListItemVM)))
testingPostH pid colF = do
  (_, project) <- Sessions.sessionAndProject pid
  currentTime <- Time.currentTime
  colId <- Testing.CollectionId <$> liftIO UUIDV4.nextRandom
  let scheduleText = fromMaybe "1" colF.scheduleNumber <> " " <> fromMaybe "Days" colF.scheduleNumberUnit
  let scheduleText' = if project.paymentPlan == "Free" then "1 day" else scheduleText

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
          , schedule = scheduleText'
          , isScheduled = True
          , collectionSteps = Testing.CollectionSteps colF.stepsData
          , lastRunResponse = Nothing
          , lastRunPassed = 0
          , lastRunFailed = 0
          }
  _ <- dbtToEff $ Testing.addCollection coll
  if project.paymentPlan == "Free" && isJust colF.scheduleNumberUnit && colF.scheduleNumberUnit /= Just "Days"
    then addErrorToast "You are using the free plan. You can only schedule collections to run once a day." Nothing
    else addSuccessToast "Collection added Successfully" Nothing
  testingGetH pid Nothing


testingGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage CollectionListItemVM)))
testingGetH pid filterTM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (currentFilterTab, tabStatus) = case filterTM of
        Just "Active" -> ("Active", Testing.Active)
        Just "Inactive" -> ("Inactive", Testing.Inactive)
        _ -> ("Active", Testing.Active)
  let currentURL = "/p/" <> pid.toText <> "/testing?"
  currTime <- Time.currentTime
  colls <- dbtToEff $ Testing.getCollections pid tabStatus
  inactiveColsCount <- dbtToEff $ Testing.inactiveCollectionsCount pid
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = Nothing
          , currentURL
          , currTime
          , nextFetchUrl = Nothing
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing}
          , heading = Nothing
          , bulkActions =
              [ ItemsList.BulkAction{icon = Just "check", title = "deactivate", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowlege"}
              ]
          , zeroState =
              Just $
                ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No Multistep Test/Monitor yet."
                  , description = "You're can create one to start monitoring your services."
                  , actionText = "Create Monitor"
                  , destination = Left "test-settings-modal"
                  }
          , elemID = "anomalyListForm"
          }
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Multistep API Tests (Beta)"
          , pageActions =
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
          , navTabs =
              Just $
                toHtml $
                  Components.TabFilter
                    { current = currentFilterTab
                    , currentURL
                    , options =
                        [ Components.TabFilterOpt{name = "Active", count = Nothing}
                        , Components.TabFilterOpt{name = "Inactive", count = Just inactiveColsCount}
                        ]
                    }
          }
  addRespHeaders $ PageCtx bwconf (ItemsList.ItemsPage listCfg $ V.map (\col -> CollectionListItemVM pid col currTime) colls)


data CollectionListItemVM = CollectionListItemVM Projects.ProjectId Testing.CollectionListItem UTCTime


instance ToHtml CollectionListItemVM where
  toHtml (CollectionListItemVM pid he tme) = toHtmlRaw $ collectionCard pid he tme
  toHtmlRaw = toHtml


collectionCard :: Projects.ProjectId -> Testing.CollectionListItem -> UTCTime -> Html ()
collectionCard pid col currTime = do
  div_ [class_ "flex flex-col gap-1"] do
    div_ [class_ "flex py-4 gap-8 items-center itemsListItem"] do
      div_ [class_ "h-4 flex space-x-3 w-8 "] do
        a_ [class_ "w-2 h-full"] ""
        input_
          [ term "aria-label" "Select Issue"
          , class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox  checkbox-md checked:checkbox-primary"
          , type_ "checkbox"
          , name_ "listItemId"
          , value_ col.id.toText
          ]
      div_ [class_ "space-y-3 grow"] do
        div_ [class_ "flex gap-10 items-center"] do
          a_
            [ href_ $ "/p/" <> pid.toText <> "/testing/" <> col.id.toText <> "/overview"
            , class_ "text-xl font-medium text-blue-700"
            ]
            $ toHtml col.title
          a_ [href_ $ "/p/" <> pid.toText <> "/testing/" <> col.id.toText] do
            faSprite_ "pen-to-square" "regular" "h-5 w-5 -mt-2"

        div_ [class_ "mt-2 flex gap-2 items-center"] do
          span_ [class_ "inline-block space-x-1"] do
            faSprite_ "clock" "regular" "w-3 h-3"
          span_
            [ class_ "decoration-black underline ml-1"
            , term "data-tippy-content" $ "created at: " <> show col.createdAt
            ]
            $ toHtml
            $ prettyTimeAuto currTime col.createdAt
          span_ "|"
          span_
            [class_ "decoration-black underline", term "data-tippy-content" $ "last run: " <> show col.lastRun]
            do
              case col.lastRun of
                Just t -> toHtml $ prettyTimeAuto currTime t
                Nothing -> "-"

      div_ [class_ "flex items-center justify-center "] do
        div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs w-96"] do
          div_ [class_ "p-2 bg-slate-100 text-slate-900 border border-slate-300"] do
            div_ [class_ "text-base"] $ show col.stepsCount
            small_ [class_ "block"] "Steps"
          div_ [class_ " p-2 bg-emerald-100 text-emerald-900 border border-emerald-300"] do
            div_ [class_ "text-base"] $ show col.passed
            small_ [class_ "block"] "Passed"
          div_ [class_ "p-2  bg-rose-100 text-rose-900 border border-rose-300 mr-4"] do
            div_ [class_ "text-base"] $ show col.failed
            small_ [class_ "block"] "Failed"


pageTabs :: Text -> Html ()
pageTabs url = do
  div_ [class_ "tabs tabs-boxed border"] do
    a_ [href_ $ url <> "/overview", role_ "tab", class_ "tab tab-active"] "Overview"
    a_ [href_ $ url, role_ "tab", class_ "tab"] "Test editor"


collectionDashboard :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
collectionDashboard pid cid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let query = "sdk_type == \"TestkitOutgoing\" and request_headers.X-Testkit-Collection-ID == \"" <> cid.toText <> "\""
  tableAsVecE <- RequestDumps.selectLogTable pid query Nothing (Nothing, Nothing) [""] Nothing
  collectionM <- dbtToEff $ Testing.getCollectionById cid
  let tableAsVecM = hush tableAsVecE
  let url = "/p/" <> pid.toText <> "/testing/" <> cid.toText

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "API Tests (Beta)"
          , navTabs = Just $ pageTabs url
          }
  case collectionM of
    Just col -> do
      let (Testing.CollectionSteps steps) = col.collectionSteps
          stepsCount = V.length steps
      addRespHeaders $ PageCtx bwconf $ dashboardPage pid cid stepsCount col.lastRunPassed col.lastRunFailed col.schedule tableAsVecM
    Nothing -> addRespHeaders $ PageCtx bwconf $ "Something went wrong"


dashboardPage :: Projects.ProjectId -> Testing.CollectionId -> Int -> Int -> Int -> Text -> Maybe (V.Vector (V.Vector AE.Value), [Text], Int) -> Html ()
dashboardPage pid cid steps passed failed schedule reqsVecM =
  section_ [class_ "p-8  mx-auto px-16 w-full flex flex-col space-y-12 pb-24  h-full"] do
    div_ [class_ "relative p-1 flex gap-10 items-start"] do
      dStats pid steps passed failed schedule
    div_ [class_ "card-round p-4 h-auto overflow-y-scroll"] do
      h6_ [class_ "font-medium"] "Test run logs"
      div_ [class_ "overflow-x-hidden"] do
        case reqsVecM of
          Just reqVec -> do
            let (requestVecs, colNames, requestsCount) = reqVec
                query = Just "sdk_type=\"TestkitOutgoing\""
                colIdxMap = listToIndexHashMap colNames
                reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "created_at") =<< (requestVecs V.!? (V.length requestVecs - 1))
                curatedColNames = nubOrd $ Log.curateCols [""] colNames
                nextLogsURL = RequestDumps.requestDumpLogUrlPath pid query Nothing reqLastCreatedAtM Nothing Nothing Nothing (Just "loadmore")
                resetLogsURL = RequestDumps.requestDumpLogUrlPath pid query Nothing Nothing Nothing Nothing Nothing Nothing
                page =
                  Log.ApiLogsPageData
                    { pid
                    , resultCount = requestsCount
                    , requestVecs
                    , cols = curatedColNames
                    , colIdxMap
                    , nextLogsURL
                    , resetLogsURL
                    , currentRange = Nothing
                    , exceededFreeTier = False
                    , query
                    , cursor = Nothing
                    , isTestLog = Just True
                    , emptyStateUrl = Just $ "/p/" <> pid.toText <> "/testing/" <> cid.toText
                    }
            Log.resultTable_ page False
          _ -> pass


dStats :: Projects.ProjectId -> Int -> Int -> Int -> Text -> Html ()
dStats pid steps passed failed freq = do
  section_ [class_ "space-y-3 shrink-0 w-1/2"] do
    div_ [class_ "reqResSubSection space-y-5"] do
      div_ [class_ "grid grid-cols-4 gap-5"] do
        statBox (Just pid) "Steps" "Total number of steps" steps Nothing
        statBox (Just pid) "Passed" "Total number of steps passed in the last test run" passed Nothing
        statBox (Just pid) "Failed" "Total number of steps failed in the last test run" failed Nothing
        div_ [class_ "col-span-1 card-round p-5 flex flex-row content-between justify-between"] do
          div_ do
            div_ [class_ "inline-block flex flex-row content-between"] do
              strong_ [class_ "font-bold text-2xl"] $ toHtml freq
            span_ $ "Frequency"
          span_ [class_ "inline-block tooltip", term "data-tip" ("Collection test runs every: " <> freq)] $ faSprite_ "circle-info" "regular" "w-4 h-4"
