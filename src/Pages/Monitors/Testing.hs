module Pages.Monitors.Testing (
  collectionDashboard,
  MonitorType (..),
  UnifiedMonitorItem (..),
  unifiedMonitorsGetH,
  unifiedMonitorOverviewH,
  statusBadge_,
)
where

import Control.Error.Util (hush)
import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.HashMap.Lazy qualified as HM
import Data.List.Extra (nubOrd)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Fmt.Internal.Core (fmt)
import Fmt.Internal.Numeric (commaizeF)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Network.URI (URIAuth (uriRegName), parseURI, uriAuthority)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (emptyState_, statBox_)
import Pages.LogExplorer.Log (ApiLogsPageData (isTestLog))
import Pages.LogExplorer.Log qualified as Log
import Pages.Monitors.TestCollectionEditor (castToStepResult)
import Pkg.Components.ItemsList (TabFilter (..), TabFilterOpt (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget (Widget (..))
import Pkg.Components.Widget qualified as Widget
import Pkg.Parser
import PyF qualified
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierExceeded, faSprite_, listToIndexHashMap, lookupVecTextByKey)



stepsBox_ :: Int -> Int -> Int -> Html ()
stepsBox_ total passed failed = do
  div_ [class_ "flex gap-2 px-6 py-2 items-center border rounded-3xl"] do
    div_ [class_ "p-2 text-center"] do
      div_ [class_ "text-textStrong text-lg text-base font-medium"] $ show total
      small_ [class_ "block"] "Steps"
    div_ [class_ "p-2 text-center"] do
      div_ [class_ "font-medium  text-lg text-textSuccess"] $ show passed
      small_ [class_ "block"] "Passed"
    div_ [class_ "p-2 text-center"] do
      div_ [class_ "font-medium text-lg text-textError"] $ show failed
      small_ [class_ "block"] "Failed"


pageTabs :: Text -> Text -> Html ()
pageTabs url ov = do
  div_ [class_ "tabs tabs-box tabs-outline items-center border p-0  bg-fillWeak  text-textWeak"] do
    a_ [href_ ov, role_ "tab", class_ "tab tab-active border border-strokeStrong  text-textStrong"] "Overview"
    a_ [href_ url, role_ "tab", class_ "tab"] "Test editor"


-- TODO: can't the log list page endpoint be reused for this info on this page? atleast we shouldnt need to query log table, and rather htmx load the correct log table
collectionDashboard :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
collectionDashboard pid cid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let queryTextRaw = [PyF.fmt|"sdk_type == \"TestkitOutgoing\" and request_headers.X-Testkit-Collection-ID == \"{cid.toText}\""]|]
  queryAST <- case parseQueryToAST queryTextRaw of
    Left err -> addErrorToast "Error Parsing Query " (Just err) >> pure []
    Right ast -> pure ast
  tableAsVecE <- RequestDumps.selectLogTable pid queryAST queryTextRaw Nothing (Nothing, Nothing) [""] Nothing Nothing
  collectionM <- dbtToEff $ Testing.getCollectionById cid
  let tableAsVecM = hush tableAsVecE
  let url = "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> cid.toText
  let overviewUrl = "/p/" <> pid.toText <> "/monitors/" <> cid.toText <> "/overview"

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "API Tests (Beta)"
          , prePageTitle = Just "Monitors & Alerts"
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
  section_ [class_ "pt-2 mx-auto px-14 w-full flex flex-col gap-4 h-full"] do
    div_ [class_ "flex justify-between items-center"] do
      div_ [class_ "flex flex-col gap-2"] do
        div_ [class_ "flex gap-2 items-center text-sm"] do
          forM_ col.tags $ \tag -> do
            span_ [class_ "badge badge-blue"] $ toHtml tag
        div_ [] do
          forM_ hostnames $ \host -> do
            span_ [class_ "badge badge-ghost"] $ toHtml host
      stepsBox_ stepsCount passed failed
    div_ [class_ "relative p-1 flex gap-10 items-start"] do
      dStats pid stepsCount passed failed schedule
    div_ [role_ "tablist", class_ "w-full rounded-3xl border", id_ "t-tabs-container"] do
      div_ [class_ "w-full flex"] do
        button_
          [ class_ "cursor-pointer t-tab px-5 pt-2 pb-1.5 text-sm text-textWeak border-b t-tab-active a-tab"
          , role_ "tab"
          , term "aria-label" "Overview"
          , onclick_ "navigatable(this, '#results-t', '#t-tabs-container', 't-tab-active')"
          ]
          "Results"
        button_
          [ class_ "cursor-pointer t-tab px-5 pt-2 pb-1.5 text-sm text-textWeak border-b a-tab"
          , role_ "tab"
          , term "aria-label" "Logs"
          , onclick_ "navigatable(this, '#logs-t', '#t-tabs-container', 't-tab-active')"
          ]
          "Logs"
        div_ [class_ "w-full border-b"] pass
      div_ [role_ "tabpanel", class_ "h-[65vh] overflow-y-auto a-tab-content", id_ "results-t"] do
        let result = col.lastRunResponse >>= castToStepResult
        let Testing.CollectionSteps stepsD = col.collectionSteps
        testResultDiagram_ pid col.id stepsD result
      div_ [class_ "hidden h-[65vh] overflow-y-auto a-tab-content", id_ "logs-t"] do
        div_ [class_ "overflow-x-hidden h-full"] do
          case reqsVecM of
            Just reqVec -> do
              let (requestVecs, colNames, requestsCount) = reqVec
                  query = Just "sdk_type==\"TestkitOutgoing\""
                  colIdxMap = listToIndexHashMap colNames
                  reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "created_at") =<< (requestVecs V.!? (V.length requestVecs - 1))
                  curatedColNames = nubOrd $ Log.curateCols [""] colNames
                  nextLogsURL = RequestDumps.requestDumpLogUrlPath pid query reqLastCreatedAtM Nothing Nothing Nothing Nothing (Just "loadmore") "requests" False
                  resetLogsURL = RequestDumps.requestDumpLogUrlPath pid query Nothing Nothing Nothing Nothing Nothing Nothing "requests" False
                  page =
                    Log.ApiLogsPageData
                      { pid
                      , resultCount = requestsCount
                      , requestVecs
                      , cols = curatedColNames
                      , colIdxMap
                      , nextLogsURL
                      , resetLogsURL
                      , recentLogsURL = ""
                      , currentRange = Nothing
                      , exceededFreeTier = False
                      , query
                      , cursor = Nothing
                      , isTestLog = Just True
                      , emptyStateUrl = Just $ "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> col.id.toText
                      , source = "requests"
                      , targetSpans = Nothing
                      , daysCountDown = Nothing
                      , queryLibRecent = V.empty
                      , queryLibSaved = V.empty
                      , serviceColors = HM.empty
                      , fromD = Nothing
                      , toD = Nothing
                      , detailsWidth = Nothing
                      , targetEvent = Nothing
                      , showTrace = Nothing
                      , facets = Nothing
                      , vizType = Nothing
                      }
              Log.virtualTable page
            _ -> pass


testResultDiagram_ :: Projects.ProjectId -> Testing.CollectionId -> V.Vector Testing.CollectionStepData -> Maybe (V.Vector Testing.StepResult) -> Html ()
testResultDiagram_ pid cid steps result = do
  when (isNothing result) $ do
    let subTxt = "You are currently not running this test collection yet."
        url = Just $ "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> cid.toText
    emptyState_ "Waiting for test run events" subTxt url "Go to test editor"
  whenJust result $ \res -> do
    div_ [class_ "p-6 flex flex-col gap-8"] do
      forM_ (V.indexed steps) $ \(i, st) -> do
        let stepResult = res V.!? i
        renderStepIll_ st stepResult i


renderStepIll_ :: Testing.CollectionStepData -> Maybe Testing.StepResult -> Int -> Html ()
renderStepIll_ st stepResult ind = do
  whenJust stepResult $ \stepRes -> do
    let assertionRes = (\(i, r) -> getAssertionResult r (V.fromList stepRes.assertResults V.!? i)) <$> V.indexed (fromMaybe [] st.asserts)
        totalPass = V.length $ V.filter fst assertionRes
    div_ [] do
      div_ [class_ "flex items-center gap-2 cursor-pointer", [__|on click toggle .hidden on the next .step-body|]] do
        faSprite_ "chevron-up" "regular" "h-5 w-5 border rounded-sm p-1"
        span_ [class_ "text-textStrong text-sm font-medium"] $ "Step " <> show (ind + 1)
        span_ [class_ "badge badge-success"] $ show totalPass <> "/" <> show (V.length assertionRes) <> " Passed"
        span_ [class_ "text-textWeak text-sm flex items-center gap-1"] do
          faSprite_ "chevron-right" "regular" "h-3 w-3"
          toHtml $ maybeToMonoid st.title
      div_ [class_ "step-body ml-2"] do
        div_ [class_ "border-l h-8"] pass
        div_ [class_ "flex"] do
          div_ [class_ "border-t w-8"] pass
          div_ [] do
            div_ [class_ "flex gap-1 -mt-2 cursor-pointer", [__|on click toggle .hidden on the next .assert-body|]] do
              faSprite_ "chevron-up" "regular" "h-5 w-5 border rounded-sm p-1"
              span_ [class_ "font-medium text-sm text-textStrong"] "Assertions"
            div_ [class_ "border-l pt-4 ml-2 flex flex-col gap-3 assert-body"] do
              forM_ assertionRes $ \(success, resultText) -> do
                div_ [class_ "flex gap-3 items-center"] do
                  span_
                    [ class_ "w-10 relative border-t after:content-[''] after:top-[-2px] after:absolute after:h-1 after:w-1 after:rounded-full after:bg-fillWeak after:right-[-2px] after:z-10"
                    ]
                    pass
                  if success
                    then span_ [class_ "badge badge-success"] "Passed"
                    else span_ [class_ "badge badge-error"] "Failed"
                  span_ [class_ "text-sm"] $ toHtml resultText


getAssertionResult :: Map Text AE.Value -> Maybe Testing.AssertResult -> (Bool, Text)
getAssertionResult st stepResult =
  case Map.toList st of
    [(key, val)] ->
      let v = case val of
            AE.String s -> s
            _ -> show val
          resultText = convertAssertText key (toText v)
          success = maybe False (\x -> (x.ok == Just True) && isNothing x.err) stepResult
       in (success, resultText)
    _ -> (False, "Something went wrong")


convertAssertText :: Text -> Text -> Text
convertAssertText key val =
  case key of
    "ok" ->
      let (left, right, op) = splitOnAny ["==", "!=", ">", "<", ">=", "<="] val
          (cat, t) = getAssertCategory left
       in cat <> " " <> t <> " " <> operationText (fromMaybe "" op) <> " " <> right
    _ -> key <> ": " <> val


getAssertCategory :: Text -> (Text, Text)
getAssertCategory key
  | T.isPrefixOf "$.resp.status" key = ("Status code", T.replace "$.resp.status" "" key)
  | T.isPrefixOf "$.resp.headers" key = ("Header", T.replace "$.resp.headers." "" key)
  | T.isPrefixOf "$.resp.json" key = ("Json path", T.replace ".resp.json" "" key)
  | otherwise = ("Body", key)


operationText :: Text -> Text
operationText "==" = "equals"
operationText "!=" = "does not equal"
operationText ">" = "is greater than"
operationText "<" = "is less than"
operationText ">=" = "is greater than or equal to"
operationText "<=" = "is less than or equal to"
operationText op = op


splitOnAny :: [Text] -> Text -> (Text, Text, Maybe Text)
splitOnAny delimiters val =
  case findDelimiter delimiters val of
    Just delimiter ->
      let parts = T.splitOn delimiter val
       in case parts of
            [left, right] -> (T.strip left, T.strip right, Just delimiter)
            _ -> (val, "", Nothing) -- Fallback in case splitting fails
    Nothing -> (val, "", Nothing)


findDelimiter :: [Text] -> Text -> Maybe Text
findDelimiter [] _ = Nothing
findDelimiter (d : ds) val
  | d `T.isInfixOf` val = Just d
  | otherwise = findDelimiter ds val


dStats :: Projects.ProjectId -> Int -> Int -> Int -> Text -> Html ()
dStats pid steps passed failed freq = do
  section_ [class_ "space-y-3 shrink-0 w-1/2"] do
    div_ [class_ "flex gap-2"] do
      statBox_ (Just pid) Nothing "Steps" "Total number of steps" (fmt (commaizeF steps)) Nothing Nothing
      statBox_ (Just pid) Nothing "Passed" "Total number of steps passed in the last test run" (fmt (commaizeF passed)) Nothing Nothing
      statBox_ (Just pid) Nothing "Failed" "Total number of steps failed in the last test run" (fmt (commaizeF failed)) Nothing Nothing
      statBox_ (Just pid) Nothing "Frequency" "Frequency of collection test runs" freq Nothing Nothing


extractHostname :: Text -> Maybe Text
extractHostname txt =
  case parseURI (toString txt) of
    Just uri -> case uriAuthority uri of
      Just auth -> Just $ toText (uriRegName auth)
      Nothing -> Nothing
    Nothing -> Nothing


extractHostnames :: [Text] -> [Text]
extractHostnames urs = sortNub (mapMaybe extractHostname urs)


-- | Types for unified monitor view
data MonitorType = MTAlert | MTMultiStep
  deriving (Eq, Show)


data UnifiedMonitorItem = UnifiedMonitorItem
  { monitorType :: MonitorType
  , monitorId :: Text
  , projectId :: Text
  , title :: Text
  , status :: Text -- "Passing", "Failing", "Active", "Inactive"
  , schedule :: Text
  , lastRun :: Maybe UTCTime
  , createdAt :: UTCTime
  , tags :: [Text]
  , hosts :: [Text]
  , details :: UnifiedMonitorDetails
  }


data UnifiedMonitorDetails
  = AlertDetails
      { query :: Text
      , alertThreshold :: Int
      , warningThreshold :: Maybe Int
      , triggerDirection :: Text -- "above" or "below"
      , lastEvaluated :: Maybe UTCTime
      , alertLastTriggered :: Maybe UTCTime
      }
  | MultiStepDetails
      { stepsCount :: Int
      , passed :: Int
      , failed :: Int
      , urls :: V.Vector Text
      }


-- | Unified handler for monitors endpoint showing both alerts and multi-step monitors
unifiedMonitorsGetH
  :: Projects.ProjectId
  -> Maybe Text -- filter
  -> Maybe Text -- since
  -> ATAuthCtx (RespHeaders (PageCtx (ItemsList.ItemsPage UnifiedMonitorItem)))
unifiedMonitorsGetH pid filterTM sinceM = do
  (sess, project) <- Sessions.sessionAndProject pid
  currTime <- Time.currentTime

  -- Parse filter
  let filterType = fromMaybe "All" filterTM

  -- Fetch multi-step monitors (collections)
  collections <- case filterType of
    "Multi-step" -> dbtToEff $ Testing.getCollections pid Testing.Active
    "All" -> dbtToEff $ Testing.getCollections pid Testing.Active
    "Inactive" -> dbtToEff $ Testing.getCollections pid Testing.Inactive
    _ -> pure V.empty

  -- Fetch alerts (query monitors)
  allAlerts <- dbtToEff $ Monitors.queryMonitorsAll pid
  let activeAlerts = V.filter (isNothing . (.deactivatedAt)) allAlerts
      inactiveAlerts = V.filter (isJust . (.deactivatedAt)) allAlerts

  alerts <- case filterType of
    "Alerts" -> pure activeAlerts
    "All" -> pure activeAlerts
    "Inactive" -> pure inactiveAlerts
    _ -> pure V.empty

  -- Convert to unified items using the generic converter
  let collectionItems = V.map (toUnifiedMonitorItem pid currTime . Right) collections
      alertItems = V.map (toUnifiedMonitorItem pid currTime . Left) alerts
      allItems = collectionItems <> alertItems

  -- Count inactive items for tab
  inactiveColsCount <- dbtToEff $ Testing.inactiveCollectionsCount pid
  let inactiveAlertsCount = V.length inactiveAlerts
      totalInactive = inactiveColsCount + inactiveAlertsCount

  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  let currentURL = "/p/" <> pid.toText <> "/monitors?"
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , sort = Nothing
          , currentURL
          , currTime
          , filter = sinceM
          , nextFetchUrl = Nothing
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing}
          , heading = Nothing
          , bulkActions = []
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No monitors configured yet"
                  , description = "Create alerts or multi-step monitors to start monitoring your services"
                  , actionText = "Create Monitor"
                  , destination =
                      Left
                        $ fromLazy
                        $ Lucid.renderText
                          ( div_ [class_ "flex gap-2"] do
                              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle", class_ "btn btn-sm btn-primary"] "Create Alert"
                              a_ [href_ $ "/p/" <> pid.toText <> "/monitors/collection", class_ "btn btn-sm btn-outline"] "Create Multi-step Monitor"
                          )
                  }
          , elemID = "monitorsListForm"
          }

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Monitors"
          , menuItem = Just "Monitors & Alerts"
          , docsLink = Just "https://apitoolkit.io/docs/monitors/"
          , freeTierExceeded = freeTierExceeded
          , pageActions = Just $ div_ [class_ "flex gap-2"] do
              div_ [class_ "dropdown dropdown-end"] do
                label_ [tabindex_ "0", class_ "btn btn-sm btn-primary gap-2"] do
                  faSprite_ "plus" "regular" "h-4"
                  "Create Monitor"
                  faSprite_ "chevron-down" "regular" "h-3"
                ul_ [tabindex_ "0", class_ "dropdown-content z-10 menu p-2 shadow bg-base-100 rounded-box w-52"] do
                  li_ $ a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer#create-alert-toggle"] do
                    faSprite_ "bell" "regular" "h-4 w-4"
                    "Log Query Alert"
                  li_ $ a_ [href_ $ "/p/" <> pid.toText <> "/monitors/collection"] do
                    faSprite_ "list-check" "regular" "h-4 w-4"
                    "Multi-step Monitor"
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = filterType
                  , currentURL
                  , options =
                      [ TabFilterOpt{name = "All", count = Nothing}
                      , TabFilterOpt{name = "Alerts", count = Just $ V.length activeAlerts}
                      , TabFilterOpt{name = "Multi-step", count = Just $ V.length collections}
                      , TabFilterOpt{name = "Inactive", count = Just totalInactive}
                      ]
                  }
          }

  addRespHeaders $ PageCtx bwconf (ItemsList.ItemsPage listCfg allItems)


-- | Convert any monitor to unified item using a more generic approach
toUnifiedMonitorItem :: Projects.ProjectId -> UTCTime -> Either Monitors.QueryMonitor Testing.CollectionListItem -> UnifiedMonitorItem
toUnifiedMonitorItem pid currTime = either (alertToUnifiedItem pid) (collectionToUnifiedItem pid currTime)
  where
    -- \| Convert collection to unified monitor item
    collectionToUnifiedItem _ _ col =
      let hosts = extractHostnames (V.toList col.urls)
       in UnifiedMonitorItem
            { monitorType = MTMultiStep
            , monitorId = col.id.toText
            , projectId = pid.toText
            , title = col.title
            , status = if col.failed > 0 then "Failing" else "Passing"
            , schedule = "every " <> col.schedule
            , lastRun = col.lastRun
            , createdAt = col.createdAt
            , tags = V.toList col.tags
            , hosts = hosts
            , details =
                MultiStepDetails
                  { stepsCount = col.stepsCount
                  , passed = col.passed
                  , failed = col.failed
                  , urls = col.urls
                  }
            }

    -- \| Convert alert to unified monitor item
    alertToUnifiedItem _ alert =
      let isActive = isNothing alert.deactivatedAt
          config = alert.alertConfig
       in UnifiedMonitorItem
            { monitorType = MTAlert
            , monitorId = alert.id.toText
            , projectId = pid.toText
            , title = config.title
            , status = if isActive then "Active" else "Inactive"
            , schedule = "every " <> show alert.checkIntervalMins <> " min"
            , lastRun = Just alert.lastEvaluated
            , createdAt = alert.createdAt
            , tags = []
            , hosts = []
            , details =
                AlertDetails
                  { query = alert.logQuery
                  , alertThreshold = alert.alertThreshold
                  , warningThreshold = alert.warningThreshold
                  , triggerDirection = if alert.triggerLessThan then "below" else "above"
                  , lastEvaluated = Just alert.lastEvaluated
                  , alertLastTriggered = alert.alertLastTriggered
                  }
            }


instance ToHtml UnifiedMonitorItem where
  toHtml item = toHtmlRaw $ unifiedMonitorCard item
  toHtmlRaw = toHtml


-- | Render unified monitor card
unifiedMonitorCard :: UnifiedMonitorItem -> Html ()
unifiedMonitorCard item = do
  div_ [class_ "border-b flex p-4 gap-4 itemsListItem hover:bg-fillWeak transition-colors group/card"] do
    -- Monitor type indicator
    div_ [class_ "mt-2 shrink-0"] do
      div_ [class_ $ "w-10 h-10 rounded-lg flex items-center justify-center " <> typeColorClass] do
        faSprite_ typeIcon "regular" "h-5 w-5"

    div_ [class_ "w-full flex flex-col gap-2 shrink-1"] do
      -- Title and tags row
      div_ [class_ "flex gap-10 items-center"] do
        a_ [href_ detailsUrl, class_ "font-medium text-textStrong text-base hover:text-textBrand transition-colors"] $ toHtml item.title
        div_ [class_ "flex gap-1 items-center text-sm"] do
          -- Monitor type badge
          span_ [class_ "badge badge-sm badge-ghost"] $ toHtml typeLabel
          -- Tags
          forM_ item.tags $ \tag -> do
            span_ [class_ "badge badge-sm badge-blue"] $ toHtml tag

      -- Details row
      div_ [class_ "w-full flex"] do
        div_ [class_ "flex flex-col gap-6 w-1/3"] do
          -- Hosts or query preview
          div_ [class_ "flex gap-2 items-center w-full"] do
            case item.details of
              MultiStepDetails{} -> do
                forM_ item.hosts $ \host -> do
                  span_ [class_ "badge badge-ghost"] $ toHtml host
              AlertDetails{query} -> do
                span_ [class_ "text-sm text-textWeak font-mono truncate", term "data-tippy-content" query] $ toHtml $ T.take 50 query

          -- Status and schedule
          div_ [class_ "flex gap-4 w-full items-center"] do
            statusBadge item.status
            div_ [class_ "flex items-center shrink-0 gap-1"] do
              faSprite_ "clock" "regular" "h-4 w-4"
              span_ [class_ "shrink-0 text-sm"] $ toHtml item.schedule

        div_ [class_ "w-2/3 flex justify-between gap-10 items-center"] do
          div_ [class_ "flex gap-6 items-center"] do
            -- Created date
            div_ [class_ "flex gap-1.5 items-center"] do
              faSprite_ "calendar" "regular" "h-6 w-6 fill-none"
              div_ [class_ "flex flex-col"] do
                span_ [class_ "text-textWeak text-xs"] "Created"
                span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml $ prettyTimeAuto item.createdAt item.createdAt

            -- Last run
            div_ [class_ "flex gap-1.5 items-center"] do
              faSprite_ "play" "regular" "h-6 w-6 fill-none text-iconNeutral"
              div_ [class_ "flex flex-col"] do
                span_ [class_ "text-textWeak text-xs"] "Last run"
                span_ [class_ "text-sm font-medium text-textStrong"] do
                  case item.lastRun of
                    Just t -> toHtml $ prettyTimeAuto item.createdAt t
                    Nothing -> "Never"

          -- Type-specific details
          case item.details of
            MultiStepDetails{stepsCount, passed, failed} ->
              stepsBox_ stepsCount passed failed
            AlertDetails{alertThreshold, warningThreshold, triggerDirection} ->
              thresholdBox_ alertThreshold warningThreshold triggerDirection

          -- Actions
          div_ [class_ "flex gap-2 px-4 py-2 items-center rounded-3xl opacity-0 group-hover/card:opacity-100 transition-opacity"] do
            a_ [href_ editUrl, term "data-tippy-content" "Edit", class_ "hover:text-textBrand transition-colors"] do
              faSprite_ "pen-to-square" "regular" "h-5 w-5"
            button_
              [ type_ "button"
              , term "data-tippy-content" $ if item.status `elem` ["Active", "Passing"] then "Deactivate" else "Activate"
              , class_ "hover:text-textBrand transition-colors"
              , hxPost_ toggleUrl
              , hxTarget_ "closest .itemsListItem"
              , hxSwap_ "outerHTML"
              ]
              do
                faSprite_ (if item.status `elem` ["Active", "Passing"] then "pause" else "play") "regular" "h-5 w-5"
  where
    (typeIcon, typeLabel, typeColorClass) = case item.monitorType of
      MTAlert -> ("bell", "Alert", "bg-fillWarning-weak text-iconWarning")
      MTMultiStep -> ("list-check", "Multi-step", "bg-fillBrand-weak text-iconBrand")

    -- Use unified overview route for both types
    detailsUrl = "/p/" <> item.projectId <> "/monitors/" <> item.monitorId <> "/overview"

    editUrl = case item.monitorType of
      MTAlert -> "/p/" <> item.projectId <> "/alerts/" <> item.monitorId
      MTMultiStep -> "/p/" <> item.projectId <> "/monitors/collection?col_id=" <> item.monitorId

    toggleUrl = case item.monitorType of
      MTAlert -> "/p/" <> item.projectId <> "/alerts/" <> item.monitorId <> "/toggle_active"
      MTMultiStep -> "/p/" <> item.projectId <> "/monitors/" <> item.monitorId <> "/toggle_active"


-- | Shared status badge component used across monitors
statusBadge :: Text -> Html ()
statusBadge = statusBadge_ False


-- | Status badge component with size option
statusBadge_ :: Bool -> Text -> Html ()
statusBadge_ isLarge status = do
  let (badgeClass, icon) = case status of
        "Passing" -> ("badge-success", Just "check")
        "Failing" -> ("badge-error", Just "xmark")
        "Active" -> ("badge-success", Nothing)
        "Inactive" -> ("badge-ghost", Nothing)
        _ -> ("badge-ghost", Nothing)
      sizeClass = if isLarge then "" else "badge-sm"
  span_ [class_ $ "badge " <> sizeClass <> " " <> badgeClass <> " gap-1"] do
    whenJust icon $ \i -> faSprite_ i "regular" "h-3 w-3"
    toHtml status


-- | Threshold box for alerts
thresholdBox_ :: Int -> Maybe Int -> Text -> Html ()
thresholdBox_ alert warning direction = do
  div_ [class_ "flex gap-2 px-4 py-2 items-center border rounded-3xl"] do
    div_ [class_ "flex items-center gap-3"] do
      -- Direction indicator
      div_ [class_ "flex items-center gap-1"] do
        faSprite_ (if direction == "above" then "arrow-up" else "arrow-down") "regular" "h-4 w-4"
        span_ [class_ "text-xs text-textWeak"] $ toHtml direction
      -- Alert threshold
      div_ [class_ "text-center"] do
        div_ [class_ "text-textError font-medium"] $ show alert
        small_ [class_ "block text-xs"] "Alert"
      -- Warning threshold
      whenJust warning $ \w -> do
        div_ [class_ "text-center"] do
          div_ [class_ "text-textWarning font-medium"] $ show w
          small_ [class_ "block text-xs"] "Warning"


-- | Unified monitor overview handler that works for both alerts and collections
unifiedMonitorOverviewH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
unifiedMonitorOverviewH pid monitorId = do
  (sess, project) <- Sessions.sessionAndProject pid
  currTime <- Time.currentTime
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  -- Try to find as alert first
  alertM <- case UUID.fromText monitorId of
    Just uuid -> do
      alerts <- dbtToEff $ Monitors.queryMonitorsById (V.singleton $ Monitors.QueryMonitorId uuid)
      pure $ alerts V.!? 0
    Nothing -> pure Nothing

  -- Try as collection if not alert
  collectionM <- case Testing.CollectionId <$> UUID.fromText monitorId of
    Just cid -> dbtToEff $ Testing.getCollectionById cid
    Nothing -> pure Nothing

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Monitor Overview"
          , prePageTitle = Just "Monitors & Alerts"
          , menuItem = Just "Monitors & Alerts"
          , docsLink = Just "https://apitoolkit.io/docs/monitors/"
          , freeTierExceeded = freeTierExceeded
          }

  case (alertM, collectionM) of
    (Just alert, _) -> do
      let bwconf' = bwconf{navTabs = Just $ monitorOverviewTabs pid monitorId "alert"}
      addRespHeaders $ PageCtx bwconf' $ unifiedOverviewPage pid (Left alert) currTime
    (_, Just collection) -> do
      -- Fetch logs for collection
      let queryTextRaw = [PyF.fmt|"sdk_type == \"TestkitOutgoing\" and request_headers.X-Testkit-Collection-ID == \"{monitorId}\""|]
      queryAST <- case parseQueryToAST queryTextRaw of
        Left err -> addErrorToast "Error Parsing Query " (Just err) >> pure []
        Right ast -> pure ast
      tableAsVecE <- RequestDumps.selectLogTable pid queryAST queryTextRaw Nothing (Nothing, Nothing) [""] Nothing Nothing
      let tableAsVecM = hush tableAsVecE
      let bwconf' = bwconf{navTabs = Just $ monitorOverviewTabs pid monitorId "collection"}
      addRespHeaders $ PageCtx bwconf' $ unifiedOverviewPage pid (Right (collection, tableAsVecM)) currTime
    _ -> addRespHeaders $ PageCtx bwconf $ div_ [class_ "p-6 text-center"] "Monitor not found"


-- | Unified overview tabs
monitorOverviewTabs :: Projects.ProjectId -> Text -> Text -> Html ()
monitorOverviewTabs pid monitorId monitorType = do
  let overviewUrl = "/p/" <> pid.toText <> "/monitors/" <> monitorId <> "/overview"
      editUrl = case monitorType of
        "alert" -> "/p/" <> pid.toText <> "/alerts/" <> monitorId
        _ -> "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> monitorId
  div_ [class_ "tabs tabs-box tabs-outline items-center border p-0 bg-fillWeak text-textWeak"] do
    a_ [href_ overviewUrl, role_ "tab", class_ "tab tab-active border border-strokeStrong text-textStrong"] "Overview"
    a_ [href_ editUrl, role_ "tab", class_ "tab"] "Configuration"


-- | Unified overview page that handles both monitor types
unifiedOverviewPage :: Projects.ProjectId -> Either Monitors.QueryMonitorEvaled (Testing.Collection, Maybe (V.Vector (V.Vector AE.Value), [Text], Int)) -> UTCTime -> Html ()
unifiedOverviewPage pid monitorData currTime = do
  section_ [class_ "pt-2 mx-auto px-14 w-full flex flex-col gap-4 h-full"] do
    -- Header section
    div_ [class_ "flex justify-between items-center"] do
      case monitorData of
        Left alert -> monitorHeader (alert.alertConfig.title) (isJust alert.deactivatedAt) ("Severity: " <> alert.alertConfig.severity)
        Right (col, _) -> monitorHeader col.title False ("Schedule: every " <> col.schedule)

      -- Action buttons
      div_ [class_ "flex gap-2"] do
        case monitorData of
          Left alert -> do
            button_
              [ class_ "btn btn-sm btn-outline"
              , hxPost_ $ "/p/" <> pid.toText <> "/alerts/" <> alert.id.toText <> "/toggle_active"
              , hxTarget_ "body"
              , hxSwap_ "innerHTML"
              ]
              $ if isJust alert.deactivatedAt then "Activate" else "Deactivate"
            a_ [href_ $ "/p/" <> pid.toText <> "/alerts/" <> alert.id.toText, class_ "btn btn-sm btn-primary"] do
              faSprite_ "pen-to-square" "regular" "h-4 w-4"
              "Edit Alert"
          Right (col, _) -> do
            a_ [href_ $ "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> col.id.toText, class_ "btn btn-sm btn-primary"] do
              faSprite_ "pen-to-square" "regular" "h-4 w-4"
              "Edit Monitor"

    -- Stats section
    div_ [class_ "relative p-1 flex gap-10 items-start"] do
      case monitorData of
        Left alert -> alertStats_ pid alert currTime
        Right (col, _) -> 
          let (Testing.CollectionSteps steps) = col.collectionSteps
           in dStats pid (V.length steps) col.lastRunPassed col.lastRunFailed col.schedule

    -- Content tabs
    tabbedSection_ "monitor-tabs" $ case monitorData of
      Left alert ->
        [ ("Query & Visualization", alertQueryTab_ pid alert)
        , ("Execution History", monitorHistoryTab_ "Alert execution history")
        , ("Notifications", alertNotificationsTab_ alert)
        ]
      Right (col, logsM) ->
        [ ("Results", testResultsTab_ pid col)
        , ("Logs", logsTab_ pid col logsM)
        , ("Execution History", monitorHistoryTab_ "Test run history")
        ]
  where
    monitorHeader title isInactive subtitle = do
      div_ [class_ "flex flex-col gap-2"] do
        _ <- h1_ [class_ "text-2xl font-semibold text-textStrong"] $ toHtml title
        div_ [class_ "flex gap-2 items-center text-sm"] do
          statusBadge $ if isInactive then "Inactive" else "Active"
          span_ [class_ "text-textWeak"] "•"
          span_ [class_ "text-textWeak"] $ toHtml subtitle


-- | Reusable tabbed section component
tabbedSection_ :: Text -> [(Text, Html ())] -> Html ()
tabbedSection_ containerId tabs = do
  div_ [role_ "tablist", class_ "w-full rounded-3xl border", id_ containerId] do
    div_ [class_ "w-full flex"] do
      forM_ (zip [0 ..] tabs) $ \(idx, (label, _)) -> do
        let tabId = containerId <> "-tab-" <> show idx
        button_
          [ class_ $ "cursor-pointer tab-btn px-5 pt-2 pb-1.5 text-sm text-textWeak border-b" <> if idx == 0 then " tab-active" else ""
          , role_ "tab"
          , term "aria-label" label
          , onclick_ $ "navigateTab(this, '#" <> tabId <> "', '#" <> containerId <> "')"
          ]
          $ toHtml label
      div_ [class_ "w-full border-b"] pass

    forM_ (zip [0 ..] tabs) $ \(idx, (_, content)) -> do
      let tabId = containerId <> "-tab-" <> show idx
      div_ [role_ "tabpanel", class_ $ "h-[65vh] overflow-y-auto tab-content" <> if idx /= 0 then " hidden" else "", id_ tabId] content

  -- Add navigation script once
  script_
    []
    "function navigateTab(tab, contentId, containerId) { \
    \const container = document.querySelector(containerId); \
    \container.querySelectorAll('.tab-active').forEach(t => t.classList.remove('tab-active')); \
    \tab.classList.add('tab-active'); \
    \container.querySelectorAll('.tab-content').forEach(c => c.classList.add('hidden')); \
    \document.querySelector(contentId).classList.remove('hidden'); \
    \}"


-- | Shared history tab
monitorHistoryTab_ :: Text -> Html ()
monitorHistoryTab_ description = do
  div_ [class_ "p-6"] do
    h3_ [class_ "text-lg font-medium text-textStrong mb-4"] "Execution History"
    div_ [class_ "text-center text-textWeak py-12"] do
      faSprite_ "clock-rotate-left" "regular" "h-12 w-12 mb-3 text-iconNeutral"
      p_ [] "Execution history coming soon"
      p_ [class_ "text-sm mt-2"] $ toHtml description


-- | Test results tab for collections
testResultsTab_ :: Projects.ProjectId -> Testing.Collection -> Html ()
testResultsTab_ pid col = do
  let result = col.lastRunResponse >>= castToStepResult
      (Testing.CollectionSteps stepsD) = col.collectionSteps
  testResultDiagram_ pid col.id stepsD result


-- | Logs tab for collections
logsTab_ :: Projects.ProjectId -> Testing.Collection -> Maybe (V.Vector (V.Vector AE.Value), [Text], Int) -> Html ()
logsTab_ pid col reqsVecM = do
  div_ [class_ "overflow-x-hidden h-full"] do
    case reqsVecM of
      Just reqVec -> do
        let (requestVecs, colNames, requestsCount) = reqVec
            query = Just "sdk_type==\"TestkitOutgoing\""
            colIdxMap = listToIndexHashMap colNames
            reqLastCreatedAtM = (\r -> lookupVecTextByKey r colIdxMap "created_at") =<< (requestVecs V.!? (V.length requestVecs - 1))
            curatedColNames = nubOrd $ Log.curateCols [""] colNames
            nextLogsURL = RequestDumps.requestDumpLogUrlPath pid query reqLastCreatedAtM Nothing Nothing Nothing Nothing (Just "loadmore") "requests" False
            resetLogsURL = RequestDumps.requestDumpLogUrlPath pid query Nothing Nothing Nothing Nothing Nothing Nothing "requests" False
            page =
              Log.ApiLogsPageData
                { pid
                , resultCount = requestsCount
                , requestVecs
                , cols = curatedColNames
                , colIdxMap
                , nextLogsURL
                , resetLogsURL
                , recentLogsURL = ""
                , currentRange = Nothing
                , exceededFreeTier = False
                , query
                , cursor = Nothing
                , isTestLog = Just True
                , emptyStateUrl = Just $ "/p/" <> pid.toText <> "/monitors/collection?col_id=" <> col.id.toText
                , source = "requests"
                , targetSpans = Nothing
                , daysCountDown = Nothing
                , queryLibRecent = V.empty
                , queryLibSaved = V.empty
                , serviceColors = HM.empty
                , fromD = Nothing
                , toD = Nothing
                , detailsWidth = Nothing
                , targetEvent = Nothing
                , showTrace = Nothing
                , facets = Nothing
                , vizType = Nothing
                }
        Log.virtualTable page
      _ -> pass


-- | Alert statistics boxes (copied from Alerts module for consolidation)
alertStats_ :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> UTCTime -> Html ()
alertStats_ pid alert currTime = do
  section_ [class_ "space-y-3 shrink-0 w-full"] do
    div_ [class_ "flex gap-2"] do
      statBox_
        (Just pid)
        Nothing
        "Check Interval"
        "How often the alert query is evaluated"
        (show alert.checkIntervalMins <> " min")
        Nothing
        Nothing

      statBox_
        (Just pid)
        Nothing
        "Alert Threshold"
        ("Trigger alert when value is " <> direction)
        (fmt (commaizeF alert.alertThreshold))
        Nothing
        Nothing

      whenJust alert.warningThreshold $ \warning ->
        statBox_
          (Just pid)
          Nothing
          "Warning Threshold"
          ("Trigger warning when value is " <> direction)
          (fmt (commaizeF warning))
          Nothing
          Nothing

      statBox_
        (Just pid)
        Nothing
        "Last Evaluated"
        "When the alert was last checked"
        (toText $ prettyTimeAuto currTime alert.lastEvaluated)
        Nothing
        Nothing

      statBox_
        (Just pid)
        Nothing
        "Last Triggered"
        "When the alert was last triggered"
        (maybe "Never" (toText . prettyTimeAuto currTime) alert.alertLastTriggered)
        Nothing
        Nothing
  where
    direction = if alert.triggerLessThan then "below" else "above"


-- | Alert query tab content (copied from Alerts module)
alertQueryTab_ :: Projects.ProjectId -> Monitors.QueryMonitorEvaled -> Html ()
alertQueryTab_ pid alert = do
  div_ [class_ "p-6"] do
    -- Query display
    div_ [class_ "mb-6"] do
      h3_ [class_ "text-lg font-medium text-textStrong mb-3"] "Alert Query"
      div_ [class_ "bg-bgAlternate rounded-lg p-4"] do
        pre_ [class_ "text-sm font-mono text-textWeak overflow-x-auto"] $ toHtml alert.logQuery

    -- Visualization
    div_ [] do
      h3_ [class_ "text-lg font-medium text-textStrong mb-3"] "Query Visualization"
      div_ [class_ "bg-bgBase rounded-lg border p-4", style_ "aspect-ratio: 4 / 2;"] do
        Widget.widget_
          $ (def :: Widget)
            { Widget.query = Just alert.logQuery
            , Widget.title = Just "Alert Query Results"
            , Widget.standalone = Just True
            , Widget._projectId = Just pid
            , Widget.layout = Just (def{Widget.w = Just 12, Widget.h = Just 6})
            -- Note: Thresholds would need to be displayed as markLines in the widget rendering
            }


-- | Alert notifications tab content (copied from Alerts module)
alertNotificationsTab_ :: Monitors.QueryMonitorEvaled -> Html ()
alertNotificationsTab_ alert = do
  div_ [class_ "p-6"] do
    h3_ [class_ "text-lg font-medium text-textStrong mb-4"] "Notification Settings"

    -- Email recipients
    div_ [class_ "mb-6"] do
      h4_ [class_ "text-base font-medium text-textStrong mb-2"] "Email Recipients"
      if alert.alertConfig.emailAll
        then div_ [class_ "text-sm text-textWeak"] "All team members will receive notifications"
        else
          if null alert.alertConfig.emails
            then div_ [class_ "text-sm text-textWeak"] "No email recipients configured"
            else div_ [class_ "flex flex-wrap gap-2"] do
              forM_ alert.alertConfig.emails $ \email ->
                span_ [class_ "badge badge-ghost"] $ toHtml (CI.original email)

    -- Slack channels
    div_ [class_ "mb-6"] do
      h4_ [class_ "text-base font-medium text-textStrong mb-2"] "Slack Channels"
      if null alert.alertConfig.slackChannels
        then div_ [class_ "text-sm text-textWeak"] "No Slack channels configured"
        else div_ [class_ "flex flex-wrap gap-2"] do
          forM_ alert.alertConfig.slackChannels $ \channel ->
            span_ [class_ "badge badge-ghost"] $ toHtml channel

    -- Message template
    div_ [] do
      h4_ [class_ "text-base font-medium text-textStrong mb-2"] "Notification Template"
      div_ [class_ "bg-bgAlternate rounded-lg p-4 space-y-2"] do
        div_ [] do
          span_ [class_ "text-sm font-medium text-textStrong"] "Subject: "
          span_ [class_ "text-sm text-textWeak"] $ toHtml alert.alertConfig.subject
        div_ [] do
          span_ [class_ "text-sm font-medium text-textStrong"] "Message: "
          p_ [class_ "text-sm text-textWeak mt-1"] $ toHtml alert.alertConfig.message
