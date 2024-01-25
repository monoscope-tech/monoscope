module Pages.Anomalies.AnomalyList (
  anomalyListGetH,
  anomalyDetailsGetH,
  anomalyBulkActionsPostH,
  acknowlegeAnomalyGetH,
  unAcknowlegeAnomalyGetH,
  archiveAnomalyGetH,
  unArchiveAnomalyGetH,
  anomalyListSlider,
  AnomalyBulkForm,
  anomalyAcknowlegeButton,
  anomalyArchiveButton,
) where

import Config
import Data.Aeson (encode)
import Data.Aeson.QQ (aesonQQ)
import Network.URI (escapeURIString, isUnreserved, isUnescapedInURI)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text (replace)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, getCurrentTime, zonedTimeToUTC)
import Data.Tuple.Extra (fst3)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, withPool)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Apis.Anomalies (AnomalyVM)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Fields.Types
import Pages.Components qualified as Components
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Apis.Shapes (getShapeFields)
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Charts.Charts (QueryBy)
import Pages.Charts.Charts qualified as Charts
import Pages.Endpoints.EndpointComponents qualified as EndpointComponents
import Pages.NonMember
import Pkg.Components (loader)
import Relude
import Relude.Unsafe qualified as Unsafe
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import Text.Time.Pretty (prettyTimeAuto)
import Utils
import Web.FormUrlEncoded (FromForm)
import Lucid.Aria qualified as Aria


newtype AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


acknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
acknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      let q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=? |]
      r <- liftIO $ withPool pool $ execute Update q (sess.userId, aid)
      pure $ anomalyAcknowlegeButton pid aid True


unAcknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
unAcknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      let q = [sql| update apis.anomalies set acknowleged_by=null, acknowleged_at=null where id=? |]
      _ <- liftIO $ withPool pool $ execute Update q (Only aid)
      pure $ anomalyAcknowlegeButton pid aid False


archiveAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
archiveAnomalyGetH sess pid aid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      let q = [sql| update apis.anomalies set archived_at=NOW() where id=? |]
      _ <- liftIO $ withPool pool $ execute Update q (Only aid)
      pure $ anomalyArchiveButton pid aid True


unArchiveAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
unArchiveAnomalyGetH sess pid aid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      let q = [sql| update apis.anomalies set archived_at=null where id=? |]
      _ <- liftIO $ withPool pool $ execute Update q (Only aid)
      pure $ anomalyArchiveButton pid aid False


-- When given a list of anomalyIDs and an action, said action would be applied to the anomalyIDs.
-- Then a notification should be triggered, as well as an action to reload the anomaly List.
anomalyBulkActionsPostH :: Sessions.PersistentSession -> Projects.ProjectId -> Text -> AnomalyBulkForm -> DashboardM (Headers '[HXTrigger] (Html ()))
anomalyBulkActionsPostH sess pid action items = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"refreshMain": "", "errorToast": [#{action <> " anomalies not successfull."}]}|]
      pure $ addHeader hxTriggerData ""
    else do
      v <- case action of
        "acknowlege" -> liftIO $ withPool pool $ execute Update [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=ANY(?::uuid[]) |] (sess.userId, Vector.fromList items.anomalyId)
        "archive" -> liftIO $ withPool pool $ execute Update [sql| update apis.anomalies set archived_at=NOW() where id=ANY(?::uuid[]) |] (Only $ Vector.fromList items.anomalyId)
        _ -> error $ "unhandled anomaly bulk action state " <> action
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"refreshMain": "", "successToast": [#{action <> "d anomalies Successfully"}]}|]
      pure $ addHeader hxTriggerData ""


data ParamInput = ParamInput
  { currentURL :: Text
  , ackd :: Bool
  , archived :: Bool
  , sort :: Text
  }


anomalyListGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Endpoints.EndpointId -> Maybe Text -> Maybe Text -> DashboardM (Html ())
anomalyListGetH sess pid layoutM ackdM archivedM sortM page loadM endpointM hxRequestM hxBoostedM = do
  let ackd = textToBool <$> ackdM
  let archived = textToBool <$> archivedM
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      let fetchLimit = 61
      let limit = maybe (Just fetchLimit) (\x -> if x == "slider" then Just 51 else Just fetchLimit) layoutM
      let pageInt = case page of
            Just p -> if limit == Just 51 then 0 else Unsafe.read (toString p)
            Nothing -> 0
      (project, anomalies) <- liftIO $
        withPool
          pool
          do
            project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
            anomalies <- case layoutM of
              Just _ -> Anomalies.selectAnomalies pid endpointM ackd archived sortM limit (pageInt * fetchLimit)
              Nothing -> Anomalies.selectAnomalies pid Nothing ackd archived sortM limit (pageInt * fetchLimit)
            pure (project, anomalies)
      currTime <- liftIO getCurrentTime
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Changes & Errors"
              }
      let currentURL = "/p/" <> pid.toText <> "/anomalies?layout=" <> fromMaybe "false" layoutM <> "&ackd=" <> fromMaybe "false" ackdM <> "&archived=" <> fromMaybe "false" archivedM
      let nextFetchUrl = maybe (Just $ currentURL <> "&load_more=true&page=" <> show (pageInt + 1)) (\x -> if x == "slider" then Nothing else Just $ currentURL <> "&load_more=true&page=" <> show (pageInt + 1)) layoutM

      let paramInput =
            ParamInput
              { currentURL = currentURL
              , ackd = fromMaybe False ackd
              , archived = fromMaybe False archived
              , sort = fromMaybe "" sortM
              }

      let elementBelowTabs =
            div_ [class_ "grid grid-cols-5", hxGet_ paramInput.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] $
              anomalyList paramInput pid currTime anomalies nextFetchUrl
      let anom = case nextFetchUrl of
            Just url -> do
              mapM_ (renderAnomaly False currTime) anomalies
              if length anomalies > fetchLimit - 1
                then a_ [class_ "cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] do
                  div_ [class_ "htmx-indicator query-indicator"] do
                    loader
                  "LOAD MORE"
                else ""
            Nothing -> mapM_ (renderAnomaly False currTime) anomalies

      case (layoutM, hxRequestM, hxBoostedM, loadM) of
        (Just "slider", Just "true", _, _) -> pure $ anomalyListSlider currTime pid endpointM (Just anomalies)
        (_, _, _, Just "true") -> pure anom
        (_, Just "true", Just "false", _) -> pure elementBelowTabs
        (_, Just "true", Nothing, _) -> pure elementBelowTabs
        _ -> pure $ bodyWrapper bwconf $ anomalyListPage paramInput pid currTime anomalies nextFetchUrl


anomalyListPage :: ParamInput -> Projects.ProjectId -> UTCTime -> Vector Anomalies.AnomalyVM -> Maybe Text -> Html ()
anomalyListPage paramInput pid currTime anomalies nextFetchUrl = div_ [class_ "w-full mx-auto  px-16 pt-10 pb-24"] do
  div_
    [ style_ "z-index:26"
    , class_ "fixed hidden right-0 top-0 justify-end left-0 bottom-0 w-full bg-black bg-opacity-5"
    , id_ "expand-an-modal"
    , [__|on click add .hidden to #expand-an-modal|]
    ]
    do
      div_ [class_ "h-full bg-white border-l ml-auto shadow pt-8 overflow-y-scroll", style_ "width:min(90vw, 800px);transition: all 1s", [__|on click halt|]] do
        div_ [class_ "relative w-full", style_ ""] do
          div_ [class_ "flex justify-end  w-full p-4 "] do
            button_ [class_ "bg-gray-200 rounded-full p-2 text-gray-500 hover:bg-gray-300 hover:text-gray-700", [__|on click add .hidden to #expand-an-modal|]] do
              faIcon_ "fa-close" "fa-regular fa-close" "h-4 w-4"
          div_ [id_ "an-modal-content-loader", class_ "bg-white rounded z-50 border p-4 absolute top-[40vh] left-1/2 -translate-x-1/2 -translate-y-1/2"] do
            loader
          div_ [class_ "px-2", id_ "an-modal-content"] pass
  h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Changes & Errors"
  div_ [class_ "py-2 px-2 space-x-6 border-b border-slate-20 mt-6 mb-8 text-sm font-light", hxBoost_ "true"] do
    let uri = deleteParam "archived" $ deleteParam "ackd" paramInput.currentURL
    a_ [class_ $ "inline-block py-2 " <> if not paramInput.ackd && not paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&ackd=false&archived=false"] "Inbox"
    a_ [class_ $ "inline-block  py-2 " <> if paramInput.ackd && not paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&ackd=true&archived=false"] "Acknowleged"
    a_ [class_ $ "inline-block  py-2 " <> if paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&archived=true"] "Archived"
  div_ [class_ "grid grid-cols-5 card-round", id_ "anomalyListBelowTab", hxGet_ paramInput.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] $ anomalyList paramInput pid currTime anomalies nextFetchUrl


anomalyList :: ParamInput -> Projects.ProjectId -> UTCTime -> Vector Anomalies.AnomalyVM -> Maybe Text -> Html ()
anomalyList paramInput pid currTime anomalies nextFetchUrl = form_ [class_ "col-span-5 bg-white divide-y ", id_ "anomalyListForm"] do
  let bulkActionBase = "/p/" <> pid.toText <> "/anomalies/bulk_actions"
  let currentURL' = deleteParam "sort" paramInput.currentURL
  let sortMenu =
        [ ("First Seen", "First time the issue occured", "first_seen")
        , ("Last Seen", "Last time the issue occured", "last_seen")
        , ("Events", "Number of events", "events")
        ]
          :: [(Text, Text, Text)]
  let currentSortTitle = maybe "First Seen" fst3 $ find (\(_, _, identifier) -> identifier == paramInput.sort) sortMenu
  div_
    [class_ "flex py-3 gap-8 items-center  bg-gray-50"]
    do
      div_ [class_ "h-4 flex space-x-3 w-8"] do
        a_ [class_ " w-2 h-full"] ""
        input_ [term "aria-label" "Select Issue", type_ "checkbox"]
      div_ [class_ " grow flex flex-row gap-2"] do
        button_ [class_ "btn-sm bg-transparent border-black hover:shadow-2xl", hxPost_ $ bulkActionBase <> "/acknowlege", hxSwap_ "none"] "✓ acknowlege"
        button_ [class_ "btn-sm bg-transparent space-x-1 border-black hover:shadow-2xl", hxPost_ $ bulkActionBase <> "/archive", hxSwap_ "none"] do
          faSprite_ "inbox-full" "solid" "h-4 w-4 inline-block"
          span_ "archive"
      div_ [class_ "relative inline-block"] do
        a_ [class_ "btn-sm bg-transparent border-black hover:shadow-2xl space-x-2", [__|on click toggle .hidden on #sortMenuDiv |]] do
          mIcon_ "sort" "h-4 w-4"
          span_ $ toHtml currentSortTitle
        div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "p-1 hidden text-sm border border-black-30 absolute right-0 z-10 mt-2 w-72 origin-top-right rounded-md bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none", tabindex_ "-1"] do
          sortMenu & mapM_ \(title, desc, identifier) -> do
            let isActive = paramInput.sort == identifier || (paramInput.sort == "" && identifier == "first_seen")
            a_
              [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
              , href_ $ currentURL' <> "&sort=" <> identifier
              , hxIndicator_ "#sortLoader"
              ]
              do
                div_ [class_ "flex flex-col items-center justify-center px-3"] do
                  if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
                div_ [class_ "grow space-y-1"] do
                  span_ [class_ "block text-lg"] $ toHtml title
                  span_ [class_ "block "] $ toHtml desc

      div_ [class_ "flex justify-center font-base w-60 content-between gap-14"] do
        span_ "GRAPH"
        div_ [class_ " space-x-2 font-base text-sm"] do
          a_ [class_ "cursor-pointer"] "24h"
          a_ [class_ "cursor-pointer font-bold text-base"] "14d"
      div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "EVENTS"
      div_ [class_ "p-12 fixed rounded-lg shadow bg-white top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 htmx-indicator query-indicator", id_ "sortLoader"] do
        loader

  when (null anomalies) $ div_ [class_ "flex text-center justify-center items-center h-32"] do
    strong_ "No anomalies yet"
  mapM_ (renderAnomaly False currTime) anomalies
  case nextFetchUrl of
    Just url ->
      if length anomalies > 60
        then a_ [class_ "cursor-pointer block p-1 blue-800 bg-blue-100 hover:bg-blue-200 text-center", hxTrigger_ "click", hxSwap_ "outerHTML", hxGet_ url] do
          div_ [class_ "htmx-indicator query-indicator"] do
            loader
          "LOAD MORE"
        else ""
    Nothing -> ""


anomalyListSlider :: UTCTime -> Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe (Vector Anomalies.AnomalyVM) -> Html ()
anomalyListSlider _ _ _ (Just []) = ""
anomalyListSlider _ pid eid Nothing = do
  div_ [hxGet_ $ "/p/" <> pid.toText <> "/anomalies?layout=slider" <> maybe "" (\x -> "&endpoint=" <> x.toText) eid, hxSwap_ "outerHTML", hxTrigger_ "load"] do
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        faIconWithAnchor_ "fa-chevron-down" "fa-light fa-chevron-down" "h-4 mr-3 mt-1 w-4" "toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)"
        span_ [class_ "text-lg text-slate-700"] "Ongoing Anomalies and Monitors"
      div_ [class_ "flex flex-row mt-2"] ""
anomalyListSlider currTime _ _ (Just anomalies) = do
  let anomalyIds = replace "\"" "'" $ show $ fmap (Anomalies.anomalyIdText . (^. #id)) anomalies
  let totalAnomaliesTxt = toText $ if length anomalies > 50 then ("50+" :: Text) else show (length anomalies)
  div_ do
    script_ [text| var rem = (x,y)=>((x%y)==0?1:(x%y)); |]
    script_
      [type_ "text/hyperscript"]
      [text| 
         init set $$currentAnomaly to 0 then
              set $$anomalyIds to $anomalyIds

          def setAnomalySliderPag()
            set #anomalySliderPagination.innerHTML to ($$currentAnomaly+1)+'/$totalAnomaliesTxt '
          end
         |]
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        faIconWithAnchor_ "fa-chevron-down" "fa-light fa-chevron-down" "h-4 mr-3 mt-1 w-4" "toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)"
        span_ [class_ "text-lg text-slate-700"] "Ongoing Anomalies and Monitors"
      div_ [class_ "flex items-center gap-2 mt-2"] do
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (Math.max(0, $currentAnomaly-1) % $anomalyIds.length) end then 
                          set $currentAnomaly to it then
                          show #{$anomalyIds[$currentAnomaly]} then 
                          setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-left" "regular" "h-4 w-4"
        span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (($currentAnomaly+1) % $anomalyIds.length) end then 
                          set $currentAnomaly to it then
                          show #{$anomalyIds[$currentAnomaly]} then
                          setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-right" "regular" "h-4 w-4"

    div_
      [ class_ "parent-slider"
      , [__|init setAnomalySliderPag() then show #{$anomalyIds[$currentAnomaly]} |]
      ]
      $ mapM_ (renderAnomaly True currTime) anomalies


anomalyTimeline :: ZonedTime -> Maybe ZonedTime -> Html ()
anomalyTimeline createdAt acknowlegedAt = small_ [class_ "inline-block  px-8 py-6 space-x-2"] $ case acknowlegedAt of
  Nothing -> do
    span_ [class_ "bg-red-200 text-red-900 inline-block px-3 rounded-lg"] "ONGOING"
    time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" createdAt
    span_ [class_ "inline-block"] "-"
    span_ "present"
  Just ackTime -> do
    span_ [class_ "bg-green-200 text-green-900 inline-block px-3 rounded-lg"] "ACKNOWLEGED"
    time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" createdAt
    span_ [class_ "inline-block"] "-"
    time_ [class_ "inline-block"] $ toHtml @String $ formatTime defaultTimeLocale "%F %R" ackTime


shapeParameterStats_ :: Int -> Int -> Int -> Html ()
shapeParameterStats_ newF deletedF updatedFF = div_ [class_ "inline-block"] do
  div_ [class_ "grid grid-cols-3 gap-2 text-center text-xs w-96"] do
    div_ [class_ "p-2 bg-emerald-100 text-emerald-900 border border-emerald-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show newF
      small_ [class_ "block"] "new fields"
    div_ [class_ " p-2 bg-slate-100 text-slate-900 border border-slate-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show updatedFF
      small_ [class_ "block"] "updated fields"
    div_ [class_ "p-2  bg-rose-100 text-rose-900 border border-rose-300"] do
      div_ [class_ "text-base"] $ toHtml @String $ show deletedF
      small_ [class_ "block"] "deleted fields"


-- anomalyAccentColor isAcknowleged isArchived
anomalyAccentColor :: Bool -> Bool -> Text
anomalyAccentColor _ True = "bg-slate-400"
anomalyAccentColor True False = "bg-green-200"
anomalyAccentColor False False = "bg-red-800"


anomalyItem :: Bool -> UTCTime -> Anomalies.AnomalyVM -> Text -> Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
anomalyItem hideByDefault currTime anomaly icon title subTitle content = do
  let anomalyId = Anomalies.anomalyIdText anomaly.id
  div_ [class_ $ "flex py-4 gap-8 " <> if hideByDefault then "card-round bg-white px-5" else "", style_ (if hideByDefault then "display:none" else ""), id_ anomalyId] do
    div_ [class_ $ "h-4 flex self-start space-x-3 w-8 " <> if hideByDefault then "hidden" else ""] do
      a_ [class_ $ anomalyAccentColor (isJust anomaly.acknowlegedAt) (isJust anomaly.archivedAt) <> " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", type_ "checkbox", name_ "anomalyId", value_ anomalyId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [href_ $ "/p/" <> anomaly.projectId.toText <> "/anomaly/" <> anomaly.targetHash, class_ "inline-block font-bold text-blue-700 space-x-2"] do
          img_ [src_ icon, class_ "inline w-4 h-4"]
          span_ $ toHtml title
        small_ [class_ "inline-block text-gray-800"] $ fromMaybe (toHtml @String "") subTitle
      div_ [class_ "flex flex-row gap-8"] do
        div_ do
          div_ [class_ "text-xs decoration-dotted underline-offset-2 space-x-4 "] do
            span_ [class_ "bg-red-50 p-1"] "ongoing"
            span_ [class_ "inline-block space-x-1"] do
              mIcon_ "clock" "w-3 h-3"
              span_
                [ class_ "decoration-black underline ml-1"
                , term "data-tippy-content" $ "first seen: " <> show anomaly.createdAt
                ]
                $ toHtml
                $ prettyTimeAuto currTime
                $ zonedTimeToUTC anomaly.createdAt
          -- span_ "|"
          -- span_ [class_ "decoration-black underline", term "data-tippy-content" $ "last seen: " <> show anomaly.lastSeen] $ toHtml $ prettyTimeAuto currTime $ zonedTimeToUTC anomaly.lastSeen
          div_ [class_ "flex items-center gap-2 mt-5"] do
            anomalyArchiveButton anomaly.projectId anomaly.id (isJust anomaly.archivedAt)
            anomalyAcknowlegeButton anomaly.projectId anomaly.id (isJust anomaly.acknowlegedAt)
            let modalEndpoint = "/p/" <> anomaly.projectId.toText <> "/anomaly/" <> anomaly.targetHash <> "?modal=True"
            Components.drawerWithURLContent_ "expand-log-drawer" modalEndpoint $ span_ [class_ "inline-block xchild-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100"] (mIcon_ "enlarge" "w-3 h-3")
        fromMaybe (toHtml @String "") content
    let chartQuery = Just $ anomaly2ChartQuery anomaly.anomalyType anomaly.targetHash
    div_ [class_ "flex items-center justify-center "] $ div_ [class_ "w-60 h-16 px-3"] $ Charts.throughput anomaly.projectId anomaly.targetHash chartQuery Nothing 14 Nothing False (Nothing, Nothing) Nothing
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ show anomaly.eventsCount14d


anomalyDetailsGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Text -> Maybe Text -> DashboardM (Html ())
anomalyDetailsGetH sess pid targetHash hxBoostedM = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, anomaly) <- liftIO $
        withPool
          pool
          do
            project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
            anomaly <- Anomalies.getAnomalyVM pid targetHash
            pure (project, anomaly)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Anomaly Details"
              }
      case anomaly of
        Just an -> do
          let chartQuery = Just $ anomaly2ChartQuery an.anomalyType an.targetHash
          currTime <- liftIO getCurrentTime
          case an.anomalyType of
            Anomalies.ATEndpoint -> do
              -- for endpoint anomalies
              shapes <- liftIO $ withPool pool $ Shapes.shapesByEndpointHash pid targetHash
              fields <- liftIO $ withPool pool $ Fields.selectFields pid targetHash
              let shapesWithFieldsMap = Vector.map (`getShapeFields` fields) shapes
              case hxBoostedM of
                Just _ -> pure $ anomalyDetailsPage an (Just shapesWithFieldsMap) Nothing Nothing chartQuery currTime True
                Nothing -> do
                  pure $ bodyWrapper bwconf $ div_ [class_ "w-full px-32"] do
                    h1_ [class_ "my-10 py-2 border-b w-full text-lg font-semibold"] "Anomaly Details"
                    anomalyDetailsPage an (Just shapesWithFieldsMap) Nothing Nothing chartQuery currTime False
            Anomalies.ATShape -> do
              newF <- liftIO $ withPool pool $ Fields.selectFieldsByHashes pid an.shapeNewUniqueFields
              let newFM = groupFieldsByCategory newF
              updF <- liftIO $ withPool pool $ Fields.selectFieldsByHashes pid an.shapeUpdatedFieldFormats
              let updfM = groupFieldsByCategory updF
              delF <- liftIO $ withPool pool $ Fields.selectFieldsByHashes pid an.shapeDeletedFields
              let delFM = groupFieldsByCategory delF
              let anFields = (newFM, updfM, delFM)
              case hxBoostedM of
                Just _ -> pure $ anomalyDetailsPage an Nothing (Just anFields) Nothing chartQuery currTime True
                Nothing -> pure $ bodyWrapper bwconf $ div_ [class_ "w-full px-32"] do
                  h1_ [class_ "my-10 py-2 border-b w-full text-lg font-semibold"] "Anomaly Details"
                  anomalyDetailsPage an Nothing (Just anFields) Nothing chartQuery currTime False
            _ -> do
              anFormats <- liftIO $ withPool pool $ Fields.getFieldsByEndpointKeyPathAndCategory pid (maybe "" (\x -> UUID.toText x.unEndpointId) an.endpointId) (fromMaybe "" an.fieldKeyPath) (fromMaybe FCRequestBody an.fieldCategory)
              case hxBoostedM of
                Just _ -> do
                  pure $ anomalyDetailsPage an Nothing Nothing (Just anFormats) chartQuery currTime True
                Nothing -> pure $ bodyWrapper bwconf $ div_ [class_ "w-full px-32"] do
                  h1_ [class_ "my-10 py-2 border-b w-full text-lg font-semibold"] "Anomaly Details"
                  anomalyDetailsPage an Nothing Nothing (Just anFormats) chartQuery currTime False
        Nothing -> pure $ bodyWrapper bwconf $ h4_ [] "ANOMALY NOT FOUND"


anomalyDetailsPage :: AnomalyVM -> Maybe (Vector Shapes.ShapeWithFields) -> Maybe (Map FieldCategoryEnum [Field], Map FieldCategoryEnum [Field], Map FieldCategoryEnum [Field]) -> Maybe (Vector Text) -> Maybe QueryBy -> UTCTime -> Bool -> Html ()
anomalyDetailsPage anomaly shapesWithFieldsMap fields prvFormatsM chartQuery currTime modal = do
  div_ [class_ "w-full "] do
    div_ [class_ "w-full"] do
      div_ [class_ "flex items-center justify-between gap-2 flex-wrap"] do
        case anomaly.anomalyType of
          Anomalies.ATEndpoint -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                faSprite_ "arrow-up-arrow-down" "solid" "inline w-6 h-6 -mt-1"
                span_ [class_ "text-2xl"] "New Endpoint"
              div_ [class_ "flex items-center gap-3"] do
                let methodColor = Utils.getMethodColor (fromMaybe "" anomaly.endpointMethod)
                div_ [class_ $ "px-4 py-1 text-sm rounded-lg font-semibold " <> methodColor] $ toHtml $ fromMaybe "" anomaly.endpointMethod
                span_ [] $ toHtml $ fromMaybe "" anomaly.endpointUrlPath
          Anomalies.ATShape -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                img_ [src_ "/assets/svgs/anomalies/fields.svg", class_ "inline w-6 h-6 -mt-1"]
                span_ [class_ "text-2xl"] "New Request Shape"
              div_ [class_ "flex items-center gap-3"] do
                let methodColor = Utils.getMethodColor (fromMaybe "" anomaly.endpointMethod)
                p_ [class_ "italic"] "in"
                div_ [class_ $ "px-4 py-1 text-sm rounded-lg font-semibold " <> methodColor] $ toHtml $ fromMaybe "" anomaly.endpointMethod
                span_ [] $ toHtml $ fromMaybe "" anomaly.endpointUrlPath
              div_ [class_ "mt-4"] do
                shapeParameterStats_ (length anomaly.shapeNewUniqueFields) (length anomaly.shapeDeletedFields) (length anomaly.shapeUpdatedFieldFormats)
          Anomalies.ATFormat -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                img_ [src_ "/assets/svgs/anomalies/fields.svg", class_ "inline w-6 h-6 -mt-1"]
                span_ [class_ "text-2xl"] "Modified field"
              div_ [class_ "flex items-center gap-3"] do
                let methodColor = Utils.getMethodColor (fromMaybe "" anomaly.endpointMethod)
                p_ [class_ "italic"] "in"
                div_ [class_ $ "px-4 py-1 text-sm rounded-lg font-semibold " <> methodColor] $ toHtml $ fromMaybe "" anomaly.endpointMethod
                span_ [] $ toHtml $ fromMaybe "" anomaly.endpointUrlPath
          _ -> pass
        div_ [class_ "flex items-center gap-8 shrink-0 text-gray-600"] do
          div_ [class_ "flex items-center gap-6 -mt-4"] do
            div_ [class_ "flex flex-col gap-2"] do
              h4_ [class_ "font-semibold"] "Events"
              span_ [class_ "inline-block space-x-1"] $ show anomaly.eventsCount14d
            div_ [class_ "flex flex-col gap-2"] do
              h4_ [class_ "font-semibold"] "First seen"
              span_ [class_ "inline-block space-x-1"] do
                mIcon_ "clock" "w-3 h-3"
                span_
                  [ class_ "decoration-black underline ml-1"
                  , term "data-tippy-content" $ "first seen: " <> show anomaly.createdAt
                  ]
                  $ toHtml
                  $ prettyTimeAuto currTime
                  $ zonedTimeToUTC anomaly.createdAt
            div_ [class_ "flex flex-col gap-2"] do
              h4_ [class_ "font-semibold"] "Last seen"
              span_ [class_ "decoration-black underline", term "data-tippy-content" $ "last seen: " <> show anomaly.lastSeen] $ toHtml $ prettyTimeAuto currTime $ zonedTimeToUTC anomaly.lastSeen
          div_ [class_ "w-[200px] h-[80px] mt-4 shrink-0"] do
            Charts.throughput anomaly.projectId anomaly.targetHash chartQuery Nothing 14 Nothing False (Nothing, Nothing) Nothing
    div_ [class_ "w-full flex items-center gap-4 mt-4 overflow-y-auto "] do
      if modal
        then do
          a_ [href_ $ "/p/" <> anomaly.projectId.toText <> "/anomaly/" <> anomaly.targetHash, term "data-tippy-content" "Go to page"] do
            "Anomaly Page" >> mIcon_ "enlarge" "w-3 h-3"
        else do
          anomalyArchiveButton anomaly.projectId anomaly.id (isJust anomaly.archivedAt)
          anomalyAcknowlegeButton anomaly.projectId anomaly.id (isJust anomaly.acknowlegedAt)

    div_ [class_ "mt-6 space-y-4"] do
      div_ [class_ "tabs tabs-bordered", role_ "tablist"] do
        input_ [type_ "radio", name_ "anomaly-events-tabs", role_ "tab", class_ "tab", Aria.label_ "Overview", checked_]
        div_ [role_ "tabpanel", class_ "tab-content w-full bg-white rounded-lg overflow-y-auto h-full", id_ "overview_content"] do
          case anomaly.anomalyType of
            Anomalies.ATEndpoint -> endpointOverview shapesWithFieldsMap
            Anomalies.ATShape -> requestShapeOverview fields
            Anomalies.ATFormat -> anomalyFormatOverview anomaly (fromMaybe [] prvFormatsM)
            _ -> ""

        input_ [type_ "radio", name_ "anomaly-events-tabs", role_ "tab", class_ "tab", Aria.label_ "Events"]
        div_ [role_ "tabpanel", class_ "tab-content grow overflow-y-auto h-full whitespace-nowrap text-sm divide-y overflow-x-hidden ", id_ "events_content"] do
          let anomalyQueryPartial = buildQueryForAnomaly anomaly.anomalyType anomaly.targetHash
          let escapedQueryPartial = toText $ escapeURIString isUnescapedInURI $ toString anomalyQueryPartial
          let events_url = "/p/" <> (UUID.toText $ Projects.unProjectId $ anomaly.projectId) <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial
          div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""


buildQueryForAnomaly :: Anomalies.AnomalyTypes -> Text -> Text 
buildQueryForAnomaly Anomalies.ATEndpoint hash = "endpoint_hash==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATShape hash = "shape_hash==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATFormat hash = "format_hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATField hash = "field[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATUnknown hash = "" 


endpointOverview :: Maybe (Vector Shapes.ShapeWithFields) -> Html ()
endpointOverview shapesWithFieldsMap =
  div_ [] do
    case shapesWithFieldsMap of
      Just s -> do
        reqResSection "Request" True (Vector.toList s)
        reqResSection "Response" False (Vector.toList s)
      Nothing -> pass


requestShapeOverview :: Maybe (Map FieldCategoryEnum [Field], Map FieldCategoryEnum [Field], Map FieldCategoryEnum [Field]) -> Html ()
requestShapeOverview fieldChanges = do
  div_ [class_ "flex flex-col gap-6"] do
    case fieldChanges of
      Just f ->
        div_ [class_ "flex flex-col gap-6"] do
          let (fs, sn, th) = case f of
                (xx, y, z) -> (xx, y, y)
          div_ [class_ "flex flex-col"] do
            h3_ [class_ "text-green-500 py-1  w-fit font-semibold border-b border-b-green-500 mb-2"] "New Unique Fields"
            div_ [class_ "px-2"] do
              p_ [class_ "hidden last:block"] "No new unique fields"
              subSubSection "Request Path Params" (Map.lookup Fields.FCPathParam fs)
              subSubSection "Request Query Params" (Map.lookup Fields.FCQueryParam fs)
              subSubSection "Request Headers" (Map.lookup Fields.FCRequestHeader fs)
              subSubSection "Request Body" (Map.lookup Fields.FCRequestBody fs)
              subSubSection "Response Headers" (Map.lookup Fields.FCResponseHeader fs)
              subSubSection "Response Body" (Map.lookup Fields.FCResponseBody fs)
          div_ [class_ "flex flex-col"] do
            h3_ [class_ "text-gray-500 py-1 w-fit font-semibold border-b border-b-gray-500 mb-2"] "Updated Fields"
            div_ [class_ "px-2"] do
              p_ [class_ "hidden last:block"] "No updated fields"
              subSubSection "Request Path Params" (Map.lookup Fields.FCPathParam sn)
              subSubSection "Request Query Params" (Map.lookup Fields.FCQueryParam sn)
              subSubSection "Request Headers" (Map.lookup Fields.FCRequestHeader sn)
              subSubSection "Request Body" (Map.lookup Fields.FCRequestBody sn)
              subSubSection "Response Headers" (Map.lookup Fields.FCResponseHeader sn)
              subSubSection "Response Body" (Map.lookup Fields.FCResponseBody sn)
          div_ [class_ "flex flex-col"] do
            h3_ [class_ "text-red-500 w-fit py-1 font-semibold border-b border-b-red-500 mb-2"] "Deleted Fields"
            div_ [class_ "px-2"] do
              p_ [class_ "hidden last:block"] "No deleted fields"
              subSubSection "Request Path Params" (Map.lookup Fields.FCPathParam th)
              subSubSection "Request Query Params" (Map.lookup Fields.FCQueryParam th)
              subSubSection "Request Headers" (Map.lookup Fields.FCRequestHeader th)
              subSubSection "Request Body" (Map.lookup Fields.FCRequestBody th)
              subSubSection "Response Headers" (Map.lookup Fields.FCResponseHeader th)
              subSubSection "Response Body" (Map.lookup Fields.FCResponseBody th)
      Nothing -> pass
anomalyFormatOverview :: AnomalyVM -> Vector Text -> Html ()
anomalyFormatOverview an prevFormats =
  section_ [class_ "space-y-10"] do
    div_ [class_ "flex items-center gap-6"] do
      div_ do
        h6_ [class_ "text-sm text-slate-800"] "FIELD NAME"
        h3_ [class_ "text-base text-slate-800"] $ toHtml $ fromMaybe "" an.fieldKey
      div_ do
        h6_ [class_ "text-sm text-slate-800 "] "FIELD PATH"
        h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml $ fromMaybe "" an.fieldKeyPath
      div_ do
        h6_ [class_ "text-sm text-slate-800"] "FIELD CATEGORY"
        h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldCategoryToDisplay $ fromMaybe FCRequestBody an.fieldCategory
    div_ [class_ "flex items-center gap-6"] do
      div_ do
        h5_ [class_ "text-sm text-slate-800"] "NEW FIELD FORMAT"
        h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml $ fieldTypeToText $ fromMaybe FTString an.formatType
      div_ do
        h5_ [class_ "text-sm text-slate-800"] "PREVIOUS FIELD FORMATS"
        ul_ [class_ "list-disc"] do
          prevFormats & mapM_ \f -> do
            li_ [class_ "ml-10 text-slate-800 text-sm"] $ toHtml f
    div_ do
      h6_ [class_ "text-slate-600 mt-4 text-sm"] "EXAMPLE VALUES"
      ul_ [class_ "list-disc"] do
        an.formatExamples & mapM_ \exs -> do
          forM_ exs \ex -> do
            li_ [class_ "ml-10 text-slate-800 text-sm"] $ toHtml ex


-- div_ [class_ "flex flex-col gap-4 mt-6 text-gray-600"] do
--   div_ [class_ "flex gap-2"] do
--     span_ [class_ "font-semibold"] "Field format:"
--     span_ [] $ toHtml $ fieldTypeToText $ fromMaybe FTString an.formatType
--   div_ [class_ "flex gap-2"] do
--     span_ [class_ "font-semibold"] "Field Category:"
--     span_ [] $ toHtml $ fieldCategoryEnumToText $ fromMaybe FCRequestBody an.fieldCategory
--   div_ [class_ "flex gap-2"] do
--     span_ [class_ "font-semibold"] "Field Key Path:"
--     span_ [] $ toHtml $ fromMaybe "" an.fieldKeyPath
--   div_ [class_ "flex gap-2"] do
--     span_ [class_ "font-semibold"] "Field Key:"
--     span_ [] $ toHtml $ fromMaybe "" an.fieldKey
--   div_ [class_ "flex gap-2 items-center"] do
--     span_ [class_ "font-semibold"] "Examples:"
--     span_ $ toHtml $ maybe "" (T.intercalate ", " . Vector.toList) an.formatExamples

anomaly2ChartQuery :: Anomalies.AnomalyTypes -> Text -> Charts.QueryBy
anomaly2ChartQuery Anomalies.ATEndpoint = Charts.QBEndpointHash
anomaly2ChartQuery Anomalies.ATShape = Charts.QBShapeHash
anomaly2ChartQuery Anomalies.ATFormat = Charts.QBFormatHash
anomaly2ChartQuery Anomalies.ATUnknown = error "Should not convert unknown anomaly to chart"
anomaly2ChartQuery Anomalies.ATField = error "Should not see field anomaly to chart in anomaly UI. ATField gets hidden under shape"


anomalyDisplayConfig :: Anomalies.AnomalyVM -> (Text, Text)
anomalyDisplayConfig anomaly = case anomaly.anomalyType of
  Anomalies.ATField -> ("New Field Found", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATShape -> ("New Request Shape", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATEndpoint -> ("New Endpoint", "/assets/svgs/endpoint.svg")
  Anomalies.ATFormat -> ("Modified field", "/assets/svgs/anomalies/fields.svg")
  Anomalies.ATUnknown -> ("Unknown anomaly", "/assets/svgs/anomalies/fields.svg")


renderAnomaly :: Bool -> UTCTime -> Anomalies.AnomalyVM -> Html ()
renderAnomaly hideByDefault currTime anomaly = do
  let (anomalyTitle, icon) = anomalyDisplayConfig anomaly
  case anomaly.anomalyType of
    Anomalies.ATEndpoint -> do
      let endpointTitle = fromMaybe "" anomaly.endpointMethod <> "  " <> fromMaybe "" anomaly.endpointUrlPath
      anomalyItem hideByDefault currTime anomaly icon anomalyTitle (Just $ toHtml endpointTitle) Nothing
    Anomalies.ATShape -> do
      let endpointTitle = fromMaybe "" anomaly.endpointMethod <> "  " <> fromMaybe "" anomaly.endpointUrlPath
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml anomaly.targetHash
            span_ [] "in"
            span_ [] $ toHtml endpointTitle
      let shapeContent = shapeParameterStats_ (length anomaly.shapeNewUniqueFields) (length anomaly.shapeDeletedFields) (length anomaly.shapeUpdatedFieldFormats)
      anomalyItem hideByDefault currTime anomaly icon anomalyTitle (Just subTitle) (Just shapeContent)
    Anomalies.ATFormat -> do
      let endpointTitle = toHtml $ fromMaybe "" anomaly.endpointMethod <> "  " <> fromMaybe "" anomaly.endpointUrlPath
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml $ fromMaybe "" anomaly.fieldKeyPath
            span_ [] "in"
            span_ [] $ toHtml endpointTitle
      let formatContent = div_ [class_ "block"] do
            div_ [class_ "text-sm"] do
              div_ do
                small_ "current format: "
                span_ $ maybe "" show anomaly.formatType
              div_ do
                small_ "previous formats: "
                span_ "" -- TODO: Should be comma separated list of formats for that field.
              div_ do
                small_ "examples: "
                small_ $ toHtml $ maybe "" (T.intercalate ", " . Vector.toList) anomaly.formatExamples
      anomalyItem hideByDefault currTime anomaly icon anomalyTitle (Just subTitle) (Just formatContent)
    Anomalies.ATField -> error "Anomalies.ATField anomaly should never show up in practice "
    Anomalies.ATUnknown -> error "Anomalies.ATField anomaly should never show up in practice "


anomalyAcknowlegeButton :: Projects.ProjectId -> Anomalies.AnomalyId -> Bool -> Html ()
anomalyAcknowlegeButton pid aid acked = do
  let acknowlegeAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Anomalies.anomalyIdText aid <> if acked then "/unacknowlege" else "/acknowlege"
  a_
    [ class_ $
        "inline-block child-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100 "
          <> (if acked then "bg-green-100 text-green-900" else "")
    , term "data-tippy-content" "acknowlege anomaly"
    , hxGet_ acknowlegeAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    if acked then "✓ Acknowleged" else "✓ Acknowlege"


anomalyArchiveButton :: Projects.ProjectId -> Anomalies.AnomalyId -> Bool -> Html ()
anomalyArchiveButton pid aid archived = do
  let archiveAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Anomalies.anomalyIdText aid <> if archived then "/unarchive" else "/archive"
  a_
    [ class_ $
        "inline-block xchild-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100 "
          <> (if archived then " bg-green-100 text-green-900" else "")
    , term "data-tippy-content" $ if archived then "unarchive" else "archive"
    , hxGet_ archiveAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    $ faSprite_ "inbox-full" "solid" "h-4 w-4"


reqResSection :: Text -> Bool -> [Shapes.ShapeWithFields] -> Html ()
reqResSection title isRequest shapesWithFieldsMap =
  section_ [class_ "space-y-3"] do
    div_ [class_ "flex justify-between mt-5"] do
      div_ [class_ "flex flex-row"] do
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]] $
          faSprite_ "chevron-down" "light" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-slate-800"] $ toHtml title

    div_ [class_ "bg-white border border-gray-100 rounded-xl py-5 px-5 space-y-6 reqResSubSection"] $
      forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap) $
        \(index, s) -> do
          let sh = if index == 1 then title <> "_fields" else title <> "_fields hidden"
          div_ [class_ sh, id_ $ title <> "_" <> show index] do
            if isRequest
              then do
                subSubSection (title <> " Path Params") (Map.lookup Fields.FCPathParam s.fieldsMap)
                subSubSection (title <> " Query Params") (Map.lookup Fields.FCQueryParam s.fieldsMap)
                subSubSection (title <> " Headers") (Map.lookup Fields.FCRequestHeader s.fieldsMap)
                subSubSection (title <> " Body") (Map.lookup Fields.FCRequestBody s.fieldsMap)
              else do
                subSubSection (title <> " Headers") (Map.lookup Fields.FCResponseHeader s.fieldsMap)
                subSubSection (title <> " Body") (Map.lookup Fields.FCResponseBody s.fieldsMap)


-- | subSubSection ..
subSubSection :: Text -> Maybe [Fields.Field] -> Html ()
subSubSection title fieldsM =
  case fieldsM of
    Nothing -> ""
    Just fields -> do
      div_ [class_ "space-y-1 mb-4"] do
        div_ [class_ "flex flex-row items-center"] do
          faIconWithAnchor_ "fa-chevron-down" "fa-light fa-chevron-down" "h-6 mr-3 w-6 p-1 cursor-pointer" "toggle .neg-rotate-90 on me then toggle .hidden on (next .subSectionContent)"
          div_ [class_ "px-4 rounded-xl w-full font-bold text-sm text-slate-900"] $ toHtml title
        div_ [class_ "space-y-1 subSectionContent"] do
          fieldsToNormalized fields & mapM_ \(key, fieldM) -> do
            let segments = T.splitOn "." key
            let depth = length segments
            let depthPadding = "margin-left:" <> show (20 + (depth * 20)) <> "px"
            let displayKey = last ("" :| segments)
            case fieldM of
              Nothing -> do
                a_
                  [ class_ "flex flex-row items-center"
                  , style_ depthPadding
                  , [__| on click toggle .neg-rotate-90 on <.chevron/> in me then collapseUntil((me), (my @data-depth))  |]
                  ]
                  do
                    faSprite_ "chevron-down" "light" "h-6 w-6 mr-1 chevron cursor-pointer p-1"
                    div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full"] do
                      span_ [class_ "text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 inline-flex items-center ml-4"] do
                        if "[*]" `T.isSuffixOf` key
                          then EndpointComponents.fieldTypeToDisplay Fields.FTList
                          else EndpointComponents.fieldTypeToDisplay Fields.FTObject
              Just field -> do
                a_
                  [ class_ "flex flex-row cursor-pointer"
                  , style_ depthPadding
                  , term "data-depth" $ show depth
                  ]
                  do
                    faSprite_ "chevron-down" "light" "h-4 mr-3 mt-4 w-4 invisible"
                    div_ [class_ "border-b flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full items-center"] do
                      span_ [class_ "grow text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                      span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ EndpointComponents.fieldTypeToDisplay field.fieldType
