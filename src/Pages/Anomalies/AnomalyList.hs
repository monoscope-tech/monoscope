module Pages.Anomalies.AnomalyList (
  anomalyListGetH,
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
import Data.Default (def)
import Data.Text (replace)
import Data.Text qualified as T
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, getCurrentTime, zonedTimeToUTC)
import Data.Tuple.Extra (fst3)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, withPool)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.RequestDumps (RequestDump (RequestDump))
import Models.Apis.RequestDumps qualified as RequestDump
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Charts.Charts qualified as Charts
import Relude
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import Text.Time.Pretty (prettyTimeAuto)
import Utils
import Web.FormUrlEncoded (FromForm)

data AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

acknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
acknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  let q = [sql| update apis.anomalies set acknowleged_by=?, acknowleged_at=NOW() where id=? |]
  r <- liftIO $ withPool pool $ execute Update q (sess.userId, aid)
  pure $ anomalyAcknowlegeButton pid aid True

unAcknowlegeAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
unAcknowlegeAnomalyGetH sess pid aid = do
  pool <- asks pool
  let q = [sql| update apis.anomalies set acknowleged_by=null, acknowleged_at=null where id=? |]
  _ <- liftIO $ withPool pool $ execute Update q (Only aid)
  pure $ anomalyAcknowlegeButton pid aid False

archiveAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
archiveAnomalyGetH sess pid aid = do
  pool <- asks pool
  let q = [sql| update apis.anomalies set archived_at=NOW() where id=? |]
  _ <- liftIO $ withPool pool $ execute Update q (Only aid)
  pure $ anomalyArchiveButton pid aid True

unArchiveAnomalyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Anomalies.AnomalyId -> DashboardM (Html ())
unArchiveAnomalyGetH sess pid aid = do
  pool <- asks pool
  let q = [sql| update apis.anomalies set archived_at=null where id=? |]
  _ <- liftIO $ withPool pool $ execute Update q (Only aid)
  pure $ anomalyArchiveButton pid aid False

-- When given a list of anomalyIDs and an action, said action would be applied to the anomalyIDs.
-- Then a notification should be triggered, as well as an action to reload the anomaly List.
anomalyBulkActionsPostH :: Sessions.PersistentSession -> Projects.ProjectId -> Text -> AnomalyBulkForm -> DashboardM (Headers '[HXTrigger] (Html ()))
anomalyBulkActionsPostH sess pid action items = do
  pool <- asks pool
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

anomalyListGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Endpoints.EndpointId -> Maybe Text -> Maybe Text -> DashboardM (Html ())
anomalyListGetH sess pid layoutM ackdM archivedM sortM endpointM hxRequestM hxBoostedM = do
  let ackd = textToBool <$> ackdM
  let archived = textToBool <$> archivedM
  pool <- asks pool
  let limit = (\x -> if x == "slider" then Just 51 else Nothing) =<< layoutM
  (project, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      anomalies <- Anomalies.selectAnomalies pid Nothing ackd archived sortM limit
      pure (project, anomalies)
  currTime <- liftIO getCurrentTime
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Changes & Errors"
          }
  let currentURL = "/p/" <> pid.toText <> "/anomalies?layout=" <> fromMaybe "false" layoutM <> "&ackd=" <> fromMaybe "false" ackdM <> "&archived=" <> fromMaybe "false" archivedM
  let paramInput =
        ParamInput
          { currentURL = currentURL
          , ackd = fromMaybe False ackd
          , archived = fromMaybe False archived
          , sort = fromMaybe "" sortM
          }

  let elementBelowTabs =
        div_ [class_ "grid grid-cols-5", hxGet_ paramInput.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] $
          anomalyList paramInput pid currTime anomalies

  case (layoutM, hxRequestM, hxBoostedM) of
    (Just "slider", Just "true", _) -> pure $ anomalyListSlider currTime pid endpointM (Just anomalies)
    (_, Just "true", Just "false") -> pure elementBelowTabs
    (_, Just "true", Nothing) -> pure elementBelowTabs
    _ -> pure $ bodyWrapper bwconf $ anomalyListPage paramInput pid currTime anomalies

anomalyListPage :: ParamInput -> Projects.ProjectId -> UTCTime -> Vector Anomalies.AnomalyVM -> Html ()
anomalyListPage paramInput pid currTime anomalies = div_ [class_ "w-full mx-auto  px-16 pt-10 pb-24"] $ do
  h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Changes & Errors"
  div_ [class_ "py-2 px-2 space-x-6 border-b border-slate-20 mt-6 mb-8 text-sm font-light", hxBoost_ "true"] do
    let uri = deleteParam "archived" $ deleteParam "ackd" paramInput.currentURL
    a_ [class_ $ "inline-block py-2 " <> if not paramInput.ackd && not paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&ackd=false&archived=false"] "Inbox"
    a_ [class_ $ "inline-block  py-2 " <> if paramInput.ackd && not paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&ackd=true&archived=false"] "Acknowleged"
    a_ [class_ $ "inline-block  py-2 " <> if paramInput.archived then " font-bold text-black " else "", href_ $ uri <> "&archived=true"] "Archived"
  div_ [class_ "grid grid-cols-5 card-round", id_ "anomalyListBelowTab", hxGet_ paramInput.currentURL, hxSwap_ "outerHTML", hxTrigger_ "refreshMain"] $ anomalyList paramInput pid currTime anomalies

anomalyList :: ParamInput -> Projects.ProjectId -> UTCTime -> Vector Anomalies.AnomalyVM -> Html ()
anomalyList paramInput pid currTime anomalies = form_ [class_ "col-span-5 bg-white divide-y ", id_ "anomalyListForm"] $ do
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
          img_ [src_ "/assets/svgs/anomalies/archive.svg", class_ "h-4 w-4 inline-block"]
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
              ]
              do
                div_ [class_ "flex flex-col items-center justify-center px-3"] do
                  if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
                div_ [class_ "grow space-y-1"] do
                  span_ [class_ "block text-lg"] $ toHtml title
                  span_ [class_ "block "] $ toHtml desc

      div_ [class_ "flex justify-center font-base w-60 content-between gap-14"] do
        span_ "GRAPH"
        div_ [class_ " space-x-2 font-base text-sm"] $ do
          a_ [class_ "cursor-pointer"] "24h"
          a_ [class_ "cursor-pointer font-bold text-base"] "14d"
      div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base"] "EVENTS"

  when (null anomalies) $ div_ [class_ "flex text-center justify-center items-center h-32"] $ do
    strong_ "No anomalies yet"
  mapM_ (renderAnomaly False currTime) anomalies

anomalyListSlider :: UTCTime -> Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe (Vector Anomalies.AnomalyVM) -> Html ()
anomalyListSlider _ _ _ (Just []) = ""
anomalyListSlider _ pid eid Nothing = do
  div_ [hxGet_ $ "/p/" <> pid.toText <> "/anomalies?layout=slider" <> maybe "" (\x -> "&endpoint=" <> x.toText) eid, hxSwap_ "outerHTML", hxTrigger_ "load"] $ do
    div_ [class_ "flex justify-between mt-5 pb-2"] $ do
      div_ [class_ "flex flex-row"] $ do
        img_
          [ src_ "/assets/svgs/cheveron-down.svg"
          , class_ "h-4 mr-3 mt-1 w-4"
          , [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]
          ]
        span_ [class_ "text-lg text-slate-700"] "Ongoing Anomalies and Monitors"
      div_ [class_ "flex flex-row mt-2"] ""
anomalyListSlider currTime _ _ (Just anomalies) = do
  let anomalyIds = replace "\"" "'" $ show $ fmap (Anomalies.anomalyIdText . (^. #id)) anomalies
  let totalAnomaliesTxt = toText $ if length anomalies > 50 then "50+" else show (length anomalies)
  div_ $ do
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
    div_ [class_ "flex justify-between mt-5 pb-2"] $ do
      div_ [class_ "flex flex-row"] $ do
        img_
          [ src_ "/assets/svgs/cheveron-down.svg"
          , class_ "h-4 mr-3 mt-1 w-4"
          , [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]
          ]
        span_ [class_ "text-lg text-slate-700"] "Ongoing Anomalies and Monitors"
      div_ [class_ "flex flex-row mt-2"] $ do
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (Math.max(0, $currentAnomaly-1) % $anomalyIds.length) end then 
                          set $currentAnomaly to it then
                          show #{$anomalyIds[$currentAnomaly]} then 
                          setAnomalySliderPag()|]
          ]
          $ img_ [src_ "/assets/svgs/leftarrow.svg", class_ " m-2"]
        span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (($currentAnomaly+1) % $anomalyIds.length) end then 
                          set $currentAnomaly to it then
                          show #{$anomalyIds[$currentAnomaly]} then
                          setAnomalySliderPag()|]
          ]
          $ img_ [src_ "/assets/svgs/rightarrow.svg", class_ " m-2"]

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
  let anomalyId = Anomalies.anomalyIdText (anomaly.id)
  div_ [class_ $ "flex py-4 gap-8 " <> if hideByDefault then "card-round bg-white px-5" else "", style_ (if hideByDefault then "display:none" else ""), id_ anomalyId] do
    div_ [class_ $ "h-4 flex self-start space-x-3 w-8 " <> if hideByDefault then "hidden" else ""] do
      a_ [class_ $ anomalyAccentColor (isJust anomaly.acknowlegedAt) (isJust anomaly.archivedAt) <> " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", type_ "checkbox", name_ "anomalyId", value_ anomalyId]
    div_ [class_ "space-y-3 grow"] do
      div_ [class_ "space-x-3"] do
        a_ [class_ "inline-block font-bold text-blue-700 space-x-2"] do
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
              span_ "|"
              span_ [class_ "decoration-black underline", term "data-tippy-content" $ "last seen: " <> show anomaly.lastSeen] $ toHtml $ prettyTimeAuto currTime $ zonedTimeToUTC anomaly.lastSeen
          div_ [class_ "flex items-center gap-2 mt-5"] do
            anomalyArchiveButton anomaly.projectId anomaly.id (isJust anomaly.archivedAt)
            anomalyAcknowlegeButton anomaly.projectId anomaly.id (isJust anomaly.acknowlegedAt)
        fromMaybe (toHtml @String "") content
    let chartQuery = Just $ anomaly2ChartQuery anomaly.anomalyType anomaly.targetHash
    div_ [class_ "flex items-center justify-center "] $ div_ [class_ "w-60 h-16 px-3"] $ Charts.throughput anomaly.projectId anomaly.targetHash chartQuery Nothing 14 Nothing False (Nothing, Nothing) Nothing
    div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Anomaly in the last 14days"] $ show $ anomaly.eventsCount14d

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
    $ img_ [src_ "/assets/svgs/anomalies/archive.svg", class_ "h-4 w-4"]
