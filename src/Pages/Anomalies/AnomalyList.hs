module Pages.Anomalies.AnomalyList (
  anomalyListGetH,
  anomalyDetailsGetH,
  anomalyBulkActionsPostH,
  escapedQueryPartial,
  acknowlegeAnomalyGetH,
  unAcknowlegeAnomalyGetH,
  archiveAnomalyGetH,
  unArchiveAnomalyGetH,
  anomalyListSlider,
  AnomalyBulkForm (..),
  AnomalyListGet (..),
  anomalyAcknowlegeButton,
  anomalyArchiveButton,
  AnomalyAction (..),
  AnomalyDetails,
  IssueVM (..),
)
where

import BackgroundJobs qualified
import Data.Default (def)
import Data.Map qualified as Map
import Data.Pool (withResource)
import Data.Text (replace)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (termRaw)
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Fields.Types (
  Field (fieldType),
  fieldTypeToText,
  fieldsToNormalized,
  groupFieldsByCategory,
 )
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes (getShapeFields)
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users (User (id))
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components qualified as Components
import Pages.Endpoints.EndpointComponents qualified as EndpointComponents
import Pkg.Components qualified as Components
import Pkg.Components.ItemsList qualified as ItemsList
import PyF (fmt)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (pool))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, addSuccessToast)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (escapedQueryPartial, faSprite_, getMethodColor)
import Web.FormUrlEncoded (FromForm)


newtype AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


acknowlegeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyAction)
acknowlegeAnomalyGetH pid aid hostM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let host = fromMaybe "" hostM
  appCtx <- ask @AuthContext
  let text_id = V.fromList [UUID.toText aid.unAnomalyId]
  v <- dbtToEff $ Anomalies.acknowledgeAnomalies sess.user.id text_id
  _ <- dbtToEff $ Anomalies.acknowlegeCascade sess.user.id v
  _ <- liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.GenSwagger pid sess.user.id host
  addRespHeaders $ Acknowlege pid aid True


unAcknowlegeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unAcknowlegeAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set acknowleged_by=null, acknowleged_at=null where id=? |]
  let qI = [sql| update apis.issues set acknowleged_by=null, acknowleged_at=null where id=? |]
  _ <- dbtToEff $ execute Update qI (Only aid)
  _ <- dbtToEff $ execute Update q (Only aid)
  addRespHeaders $ Acknowlege pid aid False


archiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
archiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=NOW() where id=? |]
  let qI = [sql| update apis.issues set archived_at=NOW() where id=? |]
  _ <- dbtToEff $ execute Update qI (Only aid)
  _ <- dbtToEff $ execute Update q (Only aid)
  addRespHeaders $ Archive pid aid True


unArchiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unArchiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=null where id=? |]
  let qI = [sql| update apis.issues set archived_at=null where id=? |]
  _ <- dbtToEff $ execute Update qI (Only aid)
  _ <- dbtToEff $ execute Update q (Only aid)
  addRespHeaders $ Archive pid aid False


data AnomalyAction
  = Acknowlege Projects.ProjectId Anomalies.AnomalyId Bool
  | Archive Projects.ProjectId Anomalies.AnomalyId Bool
  | Bulk


instance ToHtml AnomalyAction where
  toHtml (Acknowlege pid aid is_ack) = toHtml $ anomalyAcknowlegeButton pid aid is_ack ""
  toHtml (Archive pid aid is_arch) = toHtml $ anomalyArchiveButton pid aid is_arch
  toHtml Bulk = ""
  toHtmlRaw = toHtml


-- When given a list of anomalyIDs and an action, said action would be applied to the anomalyIDs.
-- Then a notification should be triggered, as well as an action to reload the anomaly List.
anomalyBulkActionsPostH :: Projects.ProjectId -> Text -> AnomalyBulkForm -> ATAuthCtx (RespHeaders AnomalyAction)
anomalyBulkActionsPostH pid action items = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  _ <- case action of
    "acknowlege" -> do
      v <- dbtToEff $ Anomalies.acknowledgeAnomalies sess.user.id (V.fromList items.anomalyId)
      _ <- dbtToEff $ Anomalies.acknowlegeCascade sess.user.id v
      hosts <- dbtToEff $ Endpoints.getEndpointsByAnomalyTargetHash pid v
      forM_ hosts \h -> do
        _ <- liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" do BackgroundJobs.GenSwagger pid sess.user.id h.host
        pass
    "archive" -> do
      _ <- dbtToEff $ execute Update [sql| update apis.anomalies set archived_at=NOW() where id=ANY(?::uuid[]) |] (Only $ V.fromList items.anomalyId)
      pass
    _ -> error $ "unhandled anomaly bulk action state " <> action
  addSuccessToast (action <> "d items Successfully") Nothing
  addRespHeaders Bulk


anomalyListGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Endpoints.EndpointId
  -> Maybe Text
  -> Maybe Text
  -> ATAuthCtx (RespHeaders AnomalyListGet)
anomalyListGetH pid layoutM filterTM sortM pageM loadM endpointM hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Inbox" -> (False, False, "Inbox")
        Just "Acknowleged" -> (True, False, "Acknowleged")
        Just "Archived" -> (False, True, "Archived")
        _ -> (False, False, "Inbox")

  let fLimit = 10
  let pageInt = maybe 0 (Unsafe.read . toString) pageM
  issues <- dbtToEff $ Anomalies.selectIssues pid endpointM (Just ackd) (Just archived) sortM (Just fLimit) (pageInt * fLimit)
  currTime <- liftIO getCurrentTime
  let currentURL = mconcat ["/p/", pid.toText, "/anomalies?layout=", fromMaybe "false" layoutM, "&ackd=", show ackd, "&archived=", show archived]
      nextFetchUrl = case layoutM of
        Just "slider" -> Nothing
        _ -> Just $ currentURL <> "&load_more=true&page=" <> show (pageInt + 1)
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , nextFetchUrl
          , sort = Just $ ItemsList.SortCfg{current = fromMaybe "events" sortM}
          , filter = Nothing
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing} -- FIXME: search actual db
          , heading = Nothing
          , bulkActions =
              [ ItemsList.BulkAction{icon = Just "check", title = "acknowlege", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowlege"}
              , ItemsList.BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
              ]
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No Issues Or Errors."
                  , description = "Start monitoring errors that happened during a request."
                  , actionText = "Error reporting guide"
                  , destination = Right $ "/p/" <> listCfg.projectId.toText <> "/integration_guides#errors-monitoring"
                  }
          , elemID = "anomalyListForm"
          , ..
          }
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Issues: Changes, Alerts & Errors"
          , menuItem = Just "Changes & Errors"
          , navTabs =
              Just
                $ toHtml
                $ Components.TabFilter
                  { current = currentFilterTab
                  , currentURL
                  , options =
                      [ Components.TabFilterOpt{name = "Inbox", count = Nothing}
                      , Components.TabFilterOpt{name = "Acknowleged", count = Nothing}
                      , Components.TabFilterOpt{name = "Archived", count = Nothing}
                      ]
                  }
          }
      issuesVM = V.map (IssueVM False currTime) issues
  addRespHeaders $ case (layoutM, hxRequestM, hxBoostedM, loadM) of
    (Just "slider", Just "true", _, _) -> ALSlider currTime pid endpointM (Just $ V.map (IssueVM True currTime) issues)
    (_, _, _, Just "true") -> ALItemsRows $ ItemsList.ItemsRows nextFetchUrl issuesVM
    _ -> ALItemsPage $ PageCtx bwconf (ItemsList.ItemsPage listCfg issuesVM)


data AnomalyListGet
  = ALItemsPage (PageCtx (ItemsList.ItemsPage IssueVM))
  | ALItemsRows (ItemsList.ItemsRows IssueVM)
  | ALSlider UTCTime Projects.ProjectId (Maybe Endpoints.EndpointId) (Maybe (V.Vector IssueVM))


instance ToHtml AnomalyListGet where
  toHtml (ALSlider utcTime pid eid issue) = toHtmlRaw $ anomalyListSlider utcTime pid eid issue
  toHtml (ALItemsPage pg) = toHtml pg
  toHtml (ALItemsRows rows) = toHtml rows
  toHtmlRaw = toHtml


anomalyListSlider :: UTCTime -> Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe (V.Vector IssueVM) -> Html ()
anomalyListSlider _ _ _ (Just []) = ""
anomalyListSlider _ pid eid Nothing = do
  div_ [hxGet_ $ "/p/" <> pid.toText <> "/anomalies?layout=slider" <> maybe "" (\x -> "&endpoint=" <> x.toText) eid, hxSwap_ "outerHTML", hxTrigger_ "load"] do
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-slate-700"] "Ongoing Issues and Monitors"
      div_ [class_ "flex flex-row mt-2"] ""
anomalyListSlider currTime _ _ (Just issues) = do
  let anomalyIds = replace "\"" "'" $ show $ fmap (Anomalies.anomalyIdText . (\(IssueVM _ _ issue) -> issue.id)) issues
  let totalAnomaliesTxt = toText $ if length issues > 10 then ("10+" :: Text) else show (length issues)
  div_ do
    script_ [text| var rem = (x,y)=>((x%y)==0?1:(x%y)); |]
    script_
      [type_ "text/hyperscript"]
      [text| init set $$currentAnomaly to 0 then set $$anomalyIds to $anomalyIds
          def setAnomalySliderPag()
            set #anomalySliderPagination.innerHTML to ($$currentAnomaly+1)+'/$totalAnomaliesTxt '
          end |]
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-slate-700"] "Ongoing Issues and Monitors"
      div_ [class_ "flex items-center gap-2 mt-2"] do
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (Math.max(0, $currentAnomaly-1) % $anomalyIds.length) end then
                          set $currentAnomaly to it then show #{$anomalyIds[$currentAnomaly]} then setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-left" "regular" "h-4 w-4"
        span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                js($currentAnomaly, $anomalyIds) return (($currentAnomaly+1) % $anomalyIds.length) end then
                set $currentAnomaly to it then show #{$anomalyIds[$currentAnomaly]} then setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-right" "regular" "h-4 w-4"

    div_
      [ class_ "parent-slider"
      , [__|init setAnomalySliderPag() then show #{$anomalyIds[$currentAnomaly]} |]
      ]
      $ mapM_ toHtml issues


shapeParameterStats_ :: Int -> Int -> Int -> Html ()
shapeParameterStats_ newF deletedF updatedFF = div_ [class_ "stats stats-vertical lg:stats-horizontal shadow"] do
  div_ [class_ "stat p-3 px-4"] $ div_ [class_ "text-2xl font-medium text-green-800"] (toHtml @String $ show newF) >> small_ [class_ "stat-title"] "new fields"
  div_ [class_ "stat p-3 px-4"] $ div_ [class_ "text-2xl font-medium text-slate-800"] (toHtml @String $ show updatedFF) >> small_ [class_ "stat-title"] "updated"
  div_ [class_ "stat p-3 px-4"] $ div_ [class_ "text-2xl font-medium text-red-800"] (toHtml @String $ show deletedF) >> small_ [class_ "stat-title"] "deleted"


-- anomalyAccentColor isAcknowleged isArchived
anomalyAccentColor :: Bool -> Bool -> Text
anomalyAccentColor _ True = "bg-slate-400"
anomalyAccentColor True False = "bg-green-200"
anomalyAccentColor False False = "bg-red-800"


issueItem :: Bool -> UTCTime -> Anomalies.IssueL -> Text -> Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
issueItem hideByDefault currTime issue icon title subTitle content = do
  let issueId = Anomalies.anomalyIdText issue.id
  div_ [class_ $ "flex py-4 gap-8 items-center itemsListItem " <> if hideByDefault then "card-round px-5" else "", style_ (if hideByDefault then "display:none" else ""), id_ issueId] do
    div_ [class_ $ "h-4 flex space-x-3 w-8 items-center justify-center " <> if hideByDefault then "hidden" else ""] do
      a_ [class_ $ anomalyAccentColor (isJust issue.acknowlegedAt) (isJust issue.archivedAt) <> " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", class_ "bulkactionItemCheckbox  checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "issueId", value_ issueId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        let modalEndpoint = "/p/" <> issue.projectId.toText <> "/anomalies/by_hash/" <> issue.targetHash <> "?modal=True"
        a_ [hxGet_ modalEndpoint, hxTarget_ ("#expand-log-drawer-" <> issue.targetHash), hxTrigger_ "click", class_ "inline-block font-bold text-blue-700 space-x-2 cursor-pointer", termRaw "preload" "mouseover"]
          $ do
            img_ [src_ icon, class_ "inline w-4 h-4"]
          >> span_ (toHtml title)
        small_ [class_ "inline-block text-gray-800"] $ fromMaybe (toHtml @String "") subTitle
      div_ [class_ "flex flex-row gap-8"] do
        div_ do
          div_ [class_ "text-xs decoration-dotted underline-offset-2 space-x-4 "] do
            span_ [class_ "bg-red-50 p-1"] "ongoing"
            span_ [class_ "inline-block space-x-1"] do
              faSprite_ "clock" "solid" "w-3 h-3"
              span_
                [ class_ "decoration-black underline ml-1"
                , term "data-tippy-content" $ "first seen: " <> show issue.createdAt
                ]
                $ toHtml
                $ prettyTimeAuto currTime
                $ zonedTimeToUTC issue.createdAt
              span_ "|"
              span_ [class_ "decoration-black underline", term "data-tippy-content" $ "last seen: " <> show issue.eventsAgg.lastSeen] $ toHtml $ prettyTimeAuto currTime issue.eventsAgg.lastSeen
          div_ [class_ "flex items-center gap-2 mt-5"] do
            anomalyArchiveButton issue.projectId issue.id (isJust issue.archivedAt)
            anomalyAcknowlegeButton issue.projectId issue.id (isJust issue.acknowlegedAt) ""
            let modalEndpoint = "/p/" <> issue.projectId.toText <> "/anomalies/by_hash/" <> issue.targetHash <> "?modal=True"
            Components.drawerWithURLContent_ ("expand-log-drawer-" <> issue.targetHash) (Just modalEndpoint) $ span_ [class_ "flex items-center justify-center cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100"] (faSprite_ "expand" "regular" "w-3 h-3")
        fromMaybe (toHtml @String "") content
    let issueQueryPartial = buildQueryForAnomaly issue.anomalyType issue.targetHash
    div_ [class_ "flex items-center justify-center "]
      $ div_
        [ class_ "w-60 h-16 px-3"
        , hxGet_ $ "/charts_html?pid=" <> issue.projectId.toText <> "&since=14D&query_raw=" <> escapedQueryPartial [fmt|{issueQueryPartial} | timechart [1d]|]
        , hxTrigger_ "intersect once"
        , hxSwap_ "innerHTML"
        ]
        ""
    div_ [class_ "w-36 flex items-center justify-center"]
      $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"]
      $ show issue.eventsAgg.count


-- anomalyDetailsGetH: either in modal or as a standalone page
anomalyDetailsGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyDetails)
anomalyDetailsGetH pid targetHash hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  issueM <- dbtToEff $ Anomalies.selectIssueByHash pid targetHash
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Anomaly Details"
          }
  case issueM of
    Nothing -> addRespHeaders $ AnomalyDetailsNoFound $ PageCtx bwconf ()
    Just issue -> do
      currTime <- liftIO getCurrentTime
      case issue.issueData of
        Anomalies.IDNewEndpointIssue issueD -> do
          -- for endpoint anomalies
          shapes <- dbtToEff $ Shapes.shapesByEndpointHash pid targetHash
          fields <- dbtToEff $ Fields.selectFields pid targetHash
          let shapesWithFieldsMap = V.map (`getShapeFields` fields) shapes
          case hxBoostedM of
            Just _ -> addRespHeaders $ AnomalyDetailsBoosted (issue, Just shapesWithFieldsMap, Nothing, Nothing, currTime, True)
            Nothing -> addRespHeaders $ AnomalyDetailsMain $ PageCtx bwconf (issue, Just shapesWithFieldsMap, Nothing, Nothing, currTime, False)
        Anomalies.IDNewShapeIssue issueD -> do
          newF <- dbtToEff $ Fields.selectFieldsByHashes pid issueD.newUniqueFields
          updF <- dbtToEff $ Fields.selectFieldsByHashes pid (T.take 16 <$> issueD.updatedFieldFormats)
          delF <- dbtToEff $ Fields.selectFieldsByHashes pid issueD.deletedFields
          let anFields = (groupFieldsByCategory newF, groupFieldsByCategory updF, groupFieldsByCategory delF)
          case hxBoostedM of
            Just _ -> addRespHeaders $ AnomalyDetailsBoosted (issue, Nothing, Just anFields, Nothing, currTime, True)
            Nothing -> addRespHeaders $ AnomalyDetailsMain $ PageCtx bwconf (issue, Nothing, Just anFields, Nothing, currTime, False)
        Anomalies.IDNewFormatIssue issueD -> do
          anFormats <-
            dbtToEff
              $ Fields.getFieldsByEndpointKeyPathAndCategory pid issueD.endpointId.toText issueD.fieldKeyPath issueD.fieldCategory
          case hxBoostedM of
            Just _ -> addRespHeaders $ AnomalyDetailsBoosted (issue, Nothing, Nothing, Just anFormats, currTime, True)
            Nothing -> addRespHeaders $ AnomalyDetailsMain $ PageCtx bwconf (issue, Nothing, Nothing, Just anFormats, currTime, False)
        Anomalies.IDNewRuntimeExceptionIssue issueD -> do
          case hxBoostedM of
            Just _ -> addRespHeaders $ AnomalyDetailsBoosted (issue, Nothing, Nothing, Nothing, currTime, True)
            Nothing -> addRespHeaders $ AnomalyDetailsMain $ PageCtx bwconf (issue, Nothing, Nothing, Nothing, currTime, False)
        _ -> addRespHeaders $ AnomalyDetailsNoFound $ PageCtx bwconf ()


data AnomalyDetails
  = AnomalyDetailsMain (PageCtx (Anomalies.IssueL, Maybe (V.Vector Shapes.ShapeWithFields), Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]), Maybe (V.Vector Text), UTCTime, Bool))
  | AnomalyDetailsBoosted (Anomalies.IssueL, Maybe (V.Vector Shapes.ShapeWithFields), Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]), Maybe (V.Vector Text), UTCTime, Bool)
  | AnomalyDetailsNoFound (PageCtx ())


instance ToHtml AnomalyDetails where
  toHtml (AnomalyDetailsMain (PageCtx bwconf (issue, shapesWithFieldsMap, fields, prvFormatsM, currTime, modal))) = toHtml $ PageCtx bwconf $ anomalyDetailsPageM issue shapesWithFieldsMap fields prvFormatsM currTime modal
  toHtml (AnomalyDetailsBoosted (issue, shapesWithFieldsMap, fields, prvFormatsM, currTime, modal)) = toHtml $ anomalyDetailsPage issue shapesWithFieldsMap fields prvFormatsM currTime modal
  toHtml (AnomalyDetailsNoFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf notFoundPage
  toHtmlRaw = toHtml


notFoundPage :: Html ()
notFoundPage = do
  h4_ [] "ANOMALY NOT FOUND"


anomalyDetailsPageM :: Anomalies.IssueL -> Maybe (V.Vector Shapes.ShapeWithFields) -> Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]) -> Maybe (V.Vector Text) -> UTCTime -> Bool -> Html ()
anomalyDetailsPageM issue shapesWithFieldsMap fields prvFormatsM currTime modal = do
  div_ [class_ "w-full px-32 overflow-y-scroll h-full"] do
    h1_ [class_ "my-10 py-2 border-b w-full text-lg font-semibold"] "Anomaly Details"
    anomalyDetailsPage issue shapesWithFieldsMap fields prvFormatsM currTime modal


anomalyDetailsPage :: Anomalies.IssueL -> Maybe (V.Vector Shapes.ShapeWithFields) -> Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]) -> Maybe (V.Vector Text) -> UTCTime -> Bool -> Html ()
anomalyDetailsPage issue shapesWithFieldsMap fields prvFormatsM currTime modal = do
  let anomalyQueryPartial = buildQueryForAnomaly issue.anomalyType issue.targetHash
  div_ [class_ "w-full "] do
    div_ [class_ "w-full"] do
      div_ [class_ "flex items-center justify-between gap-2 flex-wrap"] do
        case issue.issueData of
          Anomalies.IDNewEndpointIssue issueD -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                faSprite_ "arrow-up-arrow-down" "solid" "inline w-6 h-6 -mt-1"
                span_ [class_ "text-2xl"] "New Endpoint"
              div_ [class_ "flex items-center gap-3"] do
                let methodColor = Utils.getMethodColor issueD.endpointMethod
                div_ [class_ $ "px-4 py-1 text-sm rounded-lg font-semibold " <> methodColor] $ toHtml issueD.endpointMethod
                span_ [] $ toHtml issueD.endpointUrlPath
          Anomalies.IDNewShapeIssue issueD -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                img_ [src_ "/public/assets/svgs/anomalies/fields.svg", class_ "inline w-6 h-6 -mt-1"]
                span_ [class_ "text-2xl"] "New Request Shape"
              div_ [class_ "flex items-center gap-3"] do
                let methodColor = Utils.getMethodColor issueD.endpointMethod
                p_ [class_ "italic"] "in"
                div_ [class_ $ "px-4 py-1 text-sm rounded-lg font-semibold " <> methodColor] $ toHtml issueD.endpointMethod
                span_ [] $ toHtml issueD.endpointUrlPath
              div_ [class_ "mt-4"] do
                shapeParameterStats_ (length issueD.newUniqueFields) (length issueD.deletedFields) (length issueD.updatedFieldFormats)
          Anomalies.IDNewFormatIssue issueD -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                img_ [src_ "/public/assets/svgs/anomalies/fields.svg", class_ "inline w-6 h-6 -mt-1"]
                span_ [class_ "text-2xl"] "Modified field"
              div_ [class_ "flex items-center gap-3"] do
                let methodColor = Utils.getMethodColor issueD.endpointMethod
                p_ [class_ "italic"] "in"
                div_ [class_ $ "px-4 py-1 text-sm rounded-lg font-semibold " <> methodColor] $ toHtml issueD.endpointMethod
                span_ [] $ toHtml issueD.endpointUrlPath
          Anomalies.IDNewRuntimeExceptionIssue issueD -> do
            div_ [class_ "flex flex-col gap-4 shrink-0"] do
              a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
                img_ [src_ "/public/assets/svgs/anomalies/fields.svg", class_ "inline w-6 h-6 -mt-1"]
                span_ [class_ "text-2xl"] $ toHtml issueD.errorType
              p_ $ toHtml issueD.message
          _ -> pass
        div_ [class_ "flex items-center gap-8 shrink-0 text-gray-600"] do
          div_ [class_ "flex items-center gap-6 -mt-4"] do
            div_ [class_ "flex flex-col gap-2"] do
              h4_ [class_ "font-semibold"] "Events"
              span_ [class_ "inline-block space-x-1"] $ show issue.eventsAgg.count
            div_ [class_ "flex flex-col gap-2"] do
              h4_ [class_ "font-semibold"] "First seen"
              span_ [class_ "inline-block space-x-1"] do
                faSprite_ "clock" "regular" "w-3 h-3"
                span_
                  [ class_ "decoration-black underline ml-1"
                  , term "data-tippy-content" $ "first seen: " <> show issue.createdAt
                  ]
                  $ toHtml
                  $ prettyTimeAuto currTime
                  $ zonedTimeToUTC issue.createdAt
            div_ [class_ "flex flex-col gap-2"] do
              h4_ [class_ "font-semibold"] "Last seen"
              span_ [class_ "decoration-black underline", term "data-tippy-content" $ "last seen: " <> show issue.eventsAgg.lastSeen] $ toHtml $ prettyTimeAuto currTime issue.eventsAgg.lastSeen
          div_
            [ id_ "reqsChartsEC"
            , class_ "w-[200px] h-[80px] mt-4 shrink-0"
            , style_ "height:100px"
            , hxGet_ $ "/charts_html?pid=" <> issue.projectId.toText <> "&since=14D&query_raw=" <> escapedQueryPartial [fmt|{anomalyQueryPartial} | timechart [1d]|]
            , hxTrigger_ "intersect"
            , hxSwap_ "innerHTML"
            ]
            ""
    div_ [class_ "w-full flex items-center gap-4 mt-4 overflow-y-auto "] do
      if modal
        then do
          a_ [href_ $ "/p/" <> issue.projectId.toText <> "/anomalies/by_hash/" <> issue.targetHash, term "data-tippy-content" "Go to page", class_ "btn btn-sm btn-outline "] do
            "Expand Page" >> faSprite_ "expand" "regular" "w-3 h-3"
        else do
          anomalyArchiveButton issue.projectId issue.id (isJust issue.archivedAt)
          anomalyAcknowlegeButton issue.projectId issue.id (isJust issue.acknowlegedAt) ""

    div_ [class_ "mt-6 space-y-4"] do
      div_ [class_ "tabs tabs-bordered", role_ "tablist"] do
        input_ [type_ "radio", name_ $ "anomaly-events-tabs-" <> issue.targetHash, role_ "tab", class_ "tab", Aria.label_ "Overview", checked_]
        div_ [role_ "tabpanel", class_ "tab-content w-full bg-base-100 rounded-lg overflow-x-hidden", id_ "overview_content"] do
          case issue.issueData of
            Anomalies.IDNewEndpointIssue _ -> endpointOverview shapesWithFieldsMap
            Anomalies.IDNewShapeIssue _ -> requestShapeOverview fields
            Anomalies.IDNewFormatIssue issueD -> anomalyFormatOverview issueD (fromMaybe [] prvFormatsM)
            _ -> ""

        input_ [type_ "radio", name_ $ "anomaly-events-tabs-" <> issue.targetHash, role_ "tab", class_ "tab", Aria.label_ "Events"]
        div_ [role_ "tabpanel", class_ "tab-content grow whitespace-nowrap text-sm divide-y overflow-x-hidden ", id_ "events_content"] do
          let events_url = "/p/" <> UUID.toText (Projects.unProjectId issue.projectId) <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial anomalyQueryPartial
          div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""


buildQueryForAnomaly :: Anomalies.AnomalyTypes -> Text -> Text
buildQueryForAnomaly Anomalies.ATEndpoint hash = "endpoint_hash==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATShape hash = "shape_hash==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATFormat hash = "format_hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATField hash = "field[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATRuntimeException hash = "errors[*].hash==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATUnknown hash = ""


endpointOverview :: Maybe (V.Vector Shapes.ShapeWithFields) -> Html ()
endpointOverview shapesWithFieldsMap =
  div_ [] do
    whenJust shapesWithFieldsMap \s -> do
      reqResSection "Request" True (V.toList s)
      reqResSection "Response" False (V.toList s)


requestShapeOverview :: Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]) -> Html ()
requestShapeOverview fieldChanges = div_ [class_ "flex flex-col gap-6"] do
  whenJust fieldChanges \(fs, sn, th) -> do
    shapeOSection_ "New Unique Fields" "text-green-800" fs
    shapeOSection_ "Updated Fields" "text-slate-800" sn
    shapeOSection_ "Deleted Fields" "text-red-800" th
  where
    shapeOSection_ :: Text -> Text -> Map Fields.FieldCategoryEnum [Field] -> Html ()
    shapeOSection_ title color fields = div_ [class_ "flex flex-col"] do
      h3_ [class_ $ color <> " py-1 w-fit font-semibold border-b border-b-" <> color <> "-500 mb-2"] (toHtml title)
      div_ [class_ "px-2"] do
        p_ [class_ "hidden last:block"] ("No " <> toHtml title)
        subSubSection "Request Path Params" (Map.lookup Fields.FCPathParam fields)
        subSubSection "Request Query Params" (Map.lookup Fields.FCQueryParam fields)
        subSubSection "Request Headers" (Map.lookup Fields.FCRequestHeader fields)
        subSubSection "Request Body" (Map.lookup Fields.FCRequestBody fields)
        subSubSection "Response Headers" (Map.lookup Fields.FCResponseHeader fields)
        subSubSection "Response Body" (Map.lookup Fields.FCResponseBody fields)


anomalyFormatOverview :: Anomalies.NewFormatIssue -> V.Vector Text -> Html ()
anomalyFormatOverview formatData prevFormats =
  section_ [class_ "space-y-10"] do
    div_ [class_ "flex items-center gap-6"] do
      -- div_ do
      --   h6_ [class_ "text-sm text-slate-800"] "FIELD NAME"
      --   h3_ [class_ "text-base text-slate-800"] $ toHtml $ formatData.fieldKey
      div_ do
        h6_ [class_ "text-sm text-slate-800 "] "FIELD PATH"
        h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml formatData.fieldKeyPath
    -- div_ do
    --   h6_ [class_ "text-sm text-slate-800"] "FIELD CATEGORY"
    --   h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldCategoryToDisplay $ fromMaybe FCRequestBody an.fieldCategory
    div_ [class_ "flex items-center gap-6"] do
      div_ do
        h5_ [class_ "text-sm text-slate-800"] "NEW FIELD FORMAT"
        h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml $ fieldTypeToText formatData.formatType
      div_ do
        h5_ [class_ "text-sm text-slate-800"] "PREVIOUS FIELD FORMATS"
        ul_ [class_ "list-disc"] do
          prevFormats & mapM_ \f -> do
            li_ [class_ "ml-10 text-slate-800 text-sm"] $ toHtml f
    div_ do
      h6_ [class_ "text-slate-600 mt-4 text-sm"] "EXAMPLE VALUES"
      ul_ [class_ "list-disc"] do
        formatData.examples & mapM_ \exs -> do
          forM_ exs \ex -> do
            li_ [class_ "ml-10 text-slate-800 text-sm"] $ toHtml ex


issueDisplayConfig :: Anomalies.IssueL -> (Text, Text)
issueDisplayConfig issue = case issue.issueData of
  Anomalies.IDNewFieldIssue _ -> ("New Field Found", "/public/assets/svgs/anomalies/fields.svg")
  Anomalies.IDNewShapeIssue _ -> ("New Request Shape", "/public/assets/svgs/anomalies/fields.svg")
  Anomalies.IDNewEndpointIssue _ -> ("New Endpoint", "/public/assets/svgs/anomalies/endpoint.svg")
  Anomalies.IDNewFormatIssue _ -> ("Modified field", "/public/assets/svgs/anomalies/fields.svg")
  Anomalies.IDNewRuntimeExceptionIssue err -> (err.errorType, "/public/assets/svgs/anomalies/fields.svg")
  Anomalies.IDEmpty -> ("Unknown anomaly", "/public/assets/svgs/anomalies/fields.svg")


data IssueVM = IssueVM Bool UTCTime Anomalies.IssueL
  deriving stock (Show)


instance ToHtml IssueVM where
  {-# INLINE toHtml #-}
  toHtml (IssueVM hideByDefault currTime issue) = toHtmlRaw $ renderIssue hideByDefault currTime issue
  toHtmlRaw = toHtml


renderIssue :: Bool -> UTCTime -> Anomalies.IssueL -> Html ()
renderIssue hideByDefault currTime issue = do
  let (issueTitle, icon) = issueDisplayConfig issue
  case issue.issueData of
    Anomalies.IDNewEndpointIssue issueD -> do
      let endpointTitle = issueD.endpointMethod <> "  " <> issueD.endpointUrlPath
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml endpointTitle
            span_ [] "from"
            span_ [] $ toHtml issueD.host
      issueItem hideByDefault currTime issue icon issueTitle (Just subTitle) Nothing
    Anomalies.IDNewShapeIssue issueD -> do
      let endpointTitle = issueD.endpointMethod <> "  " <> issueD.endpointUrlPath
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml issue.targetHash
            span_ [] "in"
            span_ [] $ toHtml endpointTitle
      let shapeContent = shapeParameterStats_ (length issueD.newUniqueFields) (length issueD.deletedFields) (length issueD.updatedFieldFormats)
      issueItem hideByDefault currTime issue icon issueTitle (Just subTitle) (Just shapeContent)
    Anomalies.IDNewFormatIssue issueD -> do
      let endpointTitle = toHtml $ issueD.endpointMethod <> "  " <> issueD.endpointUrlPath
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml issueD.fieldKeyPath
            span_ [] "in"
            span_ [] $ toHtml endpointTitle
      let formatContent = div_ [class_ "block"] do
            div_ [class_ "text-sm"] do
              div_ do
                small_ "current format: "
                span_ $ toHtml issueD.formatType.toText
              div_ do
                small_ "previous formats: "
                span_ "" -- TODO: Should be comma separated list of formats for that field.
              div_ do
                small_ "examples: "
                small_ $ toHtml $ maybe "" (T.intercalate ", " . V.toList) issueD.examples
      issueItem hideByDefault currTime issue icon issueTitle (Just subTitle) (Just formatContent)
    Anomalies.IDNewRuntimeExceptionIssue issueD -> do
      let subTitle = span_ [class_ "space-x-2"] do
            a_ [class_ "cursor-pointer"] $ toHtml @Text $ issueD.rootErrorType
      let body = div_ [class_ "block"] $ p_ [] $ toHtml issueD.message
      issueItem hideByDefault currTime issue icon issueTitle (Just subTitle) (Just body)
    _ -> error "Anomalies.ATField issue should never show up in practice "


anomalyAcknowlegeButton :: Projects.ProjectId -> Anomalies.AnomalyId -> Bool -> Text -> Html ()
anomalyAcknowlegeButton pid aid acked host = do
  let acknowlegeAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Anomalies.anomalyIdText aid <> if acked then "/unacknowlege" else "/acknowlege?host=" <> host
  a_
    [ class_
        $ "inline-block child-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100 "
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
    [ class_
        $ "inline-block xchild-hover cursor-pointer py-2 px-3 rounded border border-gray-200 text-xs hover:shadow shadow-blue-100 "
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
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]]
          $ faSprite_ "chevron-down" "light" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-slate-800"] $ toHtml title

    div_ [class_ "bg-base-100 border border-gray-100 rounded-xl py-5 px-5 space-y-6 reqResSubSection"]
      $ forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap)
      $ \(index, s) -> do
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
subSubSection title fieldsM = whenJust fieldsM \fields -> do
  div_ [class_ "space-y-1 mb-4"] do
    div_ [class_ "flex flex-row items-center"] do
      a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .subSectionContent)|]] $ faSprite_ "chevron-down" "regular" "h-6 mr-3 w-6 p-1 cursor-pointer"
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
            a_ [class_ "flex flex-row cursor-pointer", style_ depthPadding, term "data-depth" $ show depth] do
              faSprite_ "chevron-down" "light" "h-4 mr-3 mt-4 w-4 invisible"
              div_ [class_ "border-b flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full items-center"] do
                span_ [class_ "grow text-sm text-slate-800 inline-flex items-center"] $ toHtml displayKey
                span_ [class_ "text-sm text-slate-600 mx-12 inline-flex items-center"] $ EndpointComponents.fieldTypeToDisplay field.fieldType
