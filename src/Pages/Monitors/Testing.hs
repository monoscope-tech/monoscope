module Pages.Monitors.Testing (
  testingGetH,
  CollectionListItemVM (..),
  collectionDashboard,
)
where

import Control.Error.Util (hush)
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.List (nubBy)
import Data.List.Extra (nubOrd)
import Data.Time (UTCTime)
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt.Internal.Core (fmt)
import Fmt.Internal.Numeric (commaizeF)
import Lucid
import Lucid.Htmx (hxExt_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxVals_)
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Network.URI (URIAuth (uriRegName), parseURI, uriAuthority)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (statBox, statBox_)
import Pages.Log (ApiLogsPageData (isTestLog))
import Pages.Log qualified as Log
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pkg.Components qualified as Components
import Pkg.Components.ItemsList qualified as ItemsList
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.ParserCombinators.ReadPrec (step)
import Text.Time.Pretty (prettyTimeAuto)
import Utils


testingGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage CollectionListItemVM)))
testingGetH pid filterTM timeFilter = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (currentFilterTab, tabStatus) = case filterTM of
        Just "Active" -> ("Active", Testing.Active)
        Just "Inactive" -> ("Inactive", Testing.Inactive)
        _ -> ("Active", Testing.Active)
  let currentURL = "/p/" <> pid.toText <> "/monitors?"
  currTime <- Time.currentTime
  colls <- dbtToEff $ Testing.getCollections pid tabStatus
  inactiveColsCount <- dbtToEff $ Testing.inactiveCollectionsCount pid
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = Nothing
          , currentURL
          , currTime
          , filter = timeFilter
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
          , pageActions = Just $ a_ [href_ $ "/p/" <> pid.toText <> "/monitors/collection", class_ "btn btn-sm btn-outline space-x-2"] $ Utils.faSprite_ "plus" "regular" "h-4" >> "new tests"
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
  div_ [class_ "border-b flex p-4 gap-4"] do
    div_ [class_ "mt-2 shrink-0"] do
      input_
        [ term "aria-label" "Select Issue"
        , class_ "endpoint_anomaly_input bulkactionItemCheckbox checkbox  checkbox-md checked:checkbox-primary"
        , type_ "checkbox"
        , name_ "listItemId"
        , value_ col.id.toText
        ]
    div_ [class_ "w-full flex flex-col gap-2 shrink-1"] do
      div_ [class_ "flex gap-10 items-center"] do
        a_ [href_ $ "/p/" <> pid.toText <> "/monitors/" <> col.id.toText <> "/overview", class_ "font-medium text-gray-800 text-xl"] $ toHtml col.title
        div_ [class_ "flex gap-2 items-center text-sm"] do
          forM_ col.tags $ \tag -> do
            span_ [class_ "badge bg-sky-100 text-sky-600"] $ toHtml tag
      div_ [class_ "w-full flex"] do
        div_ [class_ "flex flex-col gap-6 w-1/3"] do
          div_ [class_ "flex gap-2 items-center w-full"] do
            let hosts = extractHostnames (V.toList col.urls)
            forM_ hosts $ \url -> do
              span_ [class_ "badge badge-ghost"] $ toHtml url
          div_ [class_ "flex gap-4 w-full items-center"] do
            if col.failed > 0
              then span_ [class_ "badge bg-red-100 text-red-700"] "Failing"
              else span_ [class_ "badge bg-green-100 text-green-700"] "Passing"
            div_ [class_ "flex items-center shrink-0 gap-1"] do
              faSprite_ "clock" "regular" "h-4 w-4"
              span_ [class_ "shrink-0 text-sm"] $ toHtml $ "every " <> col.schedule
        div_ [class_ "w-2/3 flex justify-between gap-10 items-center"] do
          div_ [class_ "flex gap-6 items-center"] do
            div_ [class_ "flex gap-1.5 items-center"] do
              faSprite_ "calendar" "regular" "h-6 w-6 fill-none"
              div_ [class_ "flex flex-col"] do
                span_ [class_ "text-gray-500"] "Started"
                span_ [class_ "text-sm font-medium text-gray-800"] $ toHtml $ prettyTimeAuto currTime col.createdAt
            div_ [class_ "flex gap-1.5 items-center"] do
              faSprite_ "play" "regular" "h-6 w-6 fill-none"
              div_ [class_ "flex flex-col"] do
                span_ [class_ "text-gray-500"] "Last run"
                span_ [class_ "text-sm font-medium text-gray-800"] do
                  case col.lastRun of
                    Just t -> toHtml $ prettyTimeAuto currTime t
                    Nothing -> "-"
          stepsBox_ col.stepsCount col.passed col.failed
          div_ [class_ "flex gap-2 px-4 py-2 items-center rounded-3xl"] do
            a_ [href_ $ "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> col.id.toText] do
              faSprite_ "pen-to-square" "regular" "h-5 w-5 -mt-2"


stepsBox_ :: Int -> Int -> Int -> Html ()
stepsBox_ total passed failed = do
  div_ [class_ "flex gap-2 px-6 py-2 items-center border rounded-3xl"] do
    div_ [class_ "p-2 text-center"] do
      div_ [class_ "text-gray-800 text-base font-medium"] $ show total
      small_ [class_ "block"] "Steps"
    div_ [class_ "p-2 text-center"] do
      div_ [class_ "font-medium text-green-700"] $ show passed
      small_ [class_ "block"] "Passed"
    div_ [class_ "p-2 text-center"] do
      div_ [class_ "font-medium text-red-700"] $ show failed
      small_ [class_ "block"] "Failed"


pageTabs :: Text -> Text -> Html ()
pageTabs url ov = do
  div_ [class_ "tabs tabs-boxed tabs-outline items-center border"] do
    a_ [href_ ov, role_ "tab", class_ "tab tab-active"] "Overview"
    a_ [href_ url, role_ "tab", class_ "tab"] "Test editor"


collectionDashboard :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
collectionDashboard pid cid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let query = "sdk_type == \"TestkitOutgoing\" and request_headers.X-Testkit-Collection-ID == \"" <> cid.toText <> "\""
  tableAsVecE <- RequestDumps.selectLogTable pid query Nothing (Nothing, Nothing) [""] Nothing Nothing
  collectionM <- dbtToEff $ Testing.getCollectionById cid
  let tableAsVecM = hush tableAsVecE
  let url = "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> cid.toText
  let overviewUrl = "/p/" <> pid.toText <> "/monitors/" <> cid.toText <> "/overview"

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "API Tests (Beta)"
          , navTabs = Just $ pageTabs url overviewUrl
          }
  case collectionM of
    Just col -> do
      addRespHeaders $ PageCtx bwconf $ dashboardPage pid col tableAsVecM
    Nothing -> addRespHeaders $ PageCtx bwconf "Something went wrong"


dashboardPage :: Projects.ProjectId -> Testing.Collection -> Maybe (V.Vector (V.Vector AE.Value), [Text], Int) -> Html ()
dashboardPage pid col reqsVecM = do
  let passed = col.lastRunPassed
      failed = col.lastRunFailed
      schedule = col.schedule
      (Testing.CollectionSteps steps) = col.collectionSteps
      stepsCount = V.length steps
      urls = catMaybes $ join $ V.toList $ (\x -> [x.get, x.post, x.patch, x.delete, x.put] :: [Maybe Text]) <$> steps
      hostnames = extractHostnames urls
  section_ [class_ "pt-2 mx-auto px-14 w-full flex flex-col gap-4 pb-24 h-full"] do
    div_ [class_ "flex justify-between items-center"] do
      div_ [class_ "flex flex-col gap-2"] do
        div_ [class_ "flex gap-2 items-center text-sm"] do
          forM_ col.tags $ \tag -> do
            span_ [class_ "badge bg-sky-100 text-sky-600"] $ toHtml tag
        div_ [] do
          forM_ hostnames $ \host -> do
            span_ [class_ "badge badge-ghost"] $ toHtml host

      stepsBox_ stepsCount passed failed
    div_ [class_ "relative p-1 flex gap-10 items-start"] do
      dStats pid stepsCount passed failed schedule
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
                nextLogsURL = RequestDumps.requestDumpLogUrlPath pid query reqLastCreatedAtM Nothing Nothing Nothing Nothing (Just "loadmore") "requests"
                resetLogsURL = RequestDumps.requestDumpLogUrlPath pid query Nothing Nothing Nothing Nothing Nothing Nothing "requests"
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
                    , emptyStateUrl = Just $ "/p/" <> pid.toText <> "/monitors/" <> col.id.toText
                    , source = "requests"
                    , targetSpans = Nothing
                    , childSpans = []
                    , daysCountDown = Nothing
                    }
            Log.resultTable_ page False
          _ -> pass


dStats :: Projects.ProjectId -> Int -> Int -> Int -> Text -> Html ()
dStats pid steps passed failed freq = do
  section_ [class_ "space-y-3 shrink-0 w-1/2"] do
    div_ [class_ "reqResSubSection space-y-5"] do
      div_ [class_ "flex gap-4"] do
        statBox_ (Just pid) Nothing "Steps" "Total number of steps" (fmt (commaizeF steps)) Nothing
        statBox_ (Just pid) Nothing "Passed" "Total number of steps passed in the last test run" (fmt (commaizeF passed)) Nothing
        statBox_ (Just pid) Nothing "Failed" "Total number of steps failed in the last test run" (fmt (commaizeF failed)) Nothing
        statBox_ (Just pid) Nothing "Frequency" "Frequency of collection test runs" freq Nothing


extractHostname :: Text -> Maybe Text
extractHostname txt =
  case parseURI (toString txt) of
    Just uri -> case uriAuthority uri of
      Just auth -> Just $ toText (uriRegName auth)
      Nothing -> Nothing
    Nothing -> Nothing


extractHostnames :: [Text] -> [Text]
extractHostnames urs = sortNub (mapMaybe extractHostname urs)
