module Pages.Dashboards (dashboardGetH, DashboardGet (..), dashboardsGetH, DashboardsGet (..), dashboardsPostH, DashboardForm (..)) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Generics.Labels ()
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity qualified as DBT
import Database.PostgreSQL.Entity.Types qualified as DBT
import Database.PostgreSQL.Simple (Only (Only))
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxPost_)
import Lucid.Hyperscript (__)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import NeatInterpolation
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Pkg.Components qualified as Components
import Pkg.Components.Widget qualified as Widget
import Relude
import Servant (NoContent (..), err404, errBody)
import System.Types
import Utils (faIcon_, faSprite_)
import Utils qualified
import Web.FormUrlEncoded (FromForm)


data DashboardGet = DashboardGet Projects.ProjectId Dashboards.Dashboard


instance ToHtml DashboardGet where
  toHtml (DashboardGet pid dash) = toHtml $ dashboardPage_ pid dash
  toHtmlRaw = toHtml


dashboardPage_ :: Projects.ProjectId -> Dashboards.Dashboard -> Html ()
dashboardPage_ pid dash = do
  Components.modal_ "pageTitleModalId" "" $ form_
    [class_ "flex flex-col p-3 gap-3"]
    do
      label_ [class_ "form-control w-full max-w-xs"] do
        div_ [class_ "label"] $ span_ [class_ "label-text"] "Change Dashboard Title"
        input_ [class_ "input input-bordered w-full max-w-xs", placeholder_ "Insert new title", value_ $ maybeToMonoid dash.title]

  div_ [class_ "mx-auto pt-2 pb-6 px-6 gap-3.5 w-full flex flex-col h-full overflow-y-scroll pb-12 group/pg", id_ "dashboardPage"] do
    div_ "" -- variables selector area
    div_ [class_ "grid-stack"] $ forM_ dash.widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
    script_ "GridStack.init()"


loadDashboardFromVM :: Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM dashVM = case dashVM.schema of
  Just schema_ -> pure schema_
  Nothing -> find (\d -> d.file == dashVM.baseTemplate) dashboardTemplates


dashboardGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId fileM fromDStr toDStr sinceStr = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let (_fromD, _toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)

  dashVM <-
    (dbtToEff $ DBT.selectById @Dashboards.DashboardVM (Only dashId)) >>= \case
      Just v -> pure v
      Nothing -> throwError $ err404{errBody = ("Dashboard with ID not found. ID:" <> encodeUtf8 dashId.toText)}

  dash <- case fileM of
    Just file -> Dashboards.readDashboardEndpoint file
    Nothing -> maybe (throwError $ err404) pure (loadDashboardFromVM dashVM)
  dash' <- forOf (#widgets . traverse) dash \widget ->
    if (widget.eager == Just True)
      then do
        metricsD <- Charts.queryMetrics (Just pid) widget.query Nothing sinceStr fromDStr toDStr Nothing
        pure $
          widget
            & #dataset
              ?~ Widget.WidgetDataset
                { source = AE.toJSON $ V.cons (AE.toJSON <$> metricsD.headers) (AE.toJSON <<$>> metricsD.dataset)
                , rowsPerMin = Just metricsD.rowsPerMin
                , value = Just $ fromIntegral metricsD.rowsCount
                , from = metricsD.from
                , to = metricsD.to
                }
      else pure widget

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , prePageTitle = Just "Dashboards"
          , pageTitle = maybeToMonoid dash'.title
          , pageTitleModalId = Just "dashbordTitleMdl"
          , pageActions = Just $ Components.timepicker_ Nothing currentRange
          }
  addRespHeaders $ PageCtx bwconf $ DashboardGet pid dash'


--------------------------------------------------------------------
-- Dashboard List
--

data DashboardsGet = DashboardsGet
  { dashboards :: V.Vector Dashboards.DashboardVM
  , projectId :: Projects.ProjectId
  }
  deriving (Show, Generic)


instance ToHtml DashboardsGet where
  toHtml dash = toHtml $ dashboardsGet_ dash
  toHtmlRaw = toHtml


renderDashboardListItem :: Bool -> Text -> (Text, Text) -> Text -> Text -> Html ()
renderDashboardListItem checked tmplClass (icon, iconClasses) content value = label_
  [ class_
      [text| cursor-pointer group hover:bg-fillWeaker hover:border rounded-lg flex p-1.5 gap-2 items-center
      group-has-[.${tmplClass}:checked]/md:bg-fillWeaker group-has-[.${tmplClass}:checked]/md:border |]
  ]
  do
    input_ ([class_ $ "hidden " <> tmplClass, type_ "radio", name_ "file", value_ value] <> [checked_ | checked])
    span_ [class_ "p-1 px-2 bg-slate-200 rounded-md"] $ faIcon_ icon iconClasses "icon w-4 h-4"
    span_ [class_ "grow"] $ toHtml content
    span_ [class_ "px-2 p-1 hidden group-hover:block group-has-[.${tmplClass}:checked]/md:block"] $ faSprite_ "chevron-right" "regular" "w-4 h-4"


dashboardsGet_ :: DashboardsGet -> Html ()
dashboardsGet_ dg = do
  Components.modal_ "newDashboardMdl" "" $ form_
    [ class_ "grid grid-cols-7 bg-bgBase overflow-hidden h-full gap-4 group/md"
    , hxPost_ ""
    ]
    do
      div_ [class_ "col-span-2 space-y-4"] do
        strong_ "Create dashboard"
        label_ [class_ "input input-sm input-bordered flex items-center "] do
          faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
          input_ [type_ "text", class_ "grow pl-2", placeholder_ "Search"]
          kbd_ [class_ "kbd kbd-sm"] "/"
        div_ [class_ "space-y-1"] do
          renderDashboardListItem True "tmplRadio0" ("cards-blank", "fa-cards-blank fa-regular") "Blank dashboard" ""
          iforM_ dashboardTemplates \idx dashTmpl -> do
            let tmplItemClass = "tmplRadio" <> (show $ idx + 1)
            renderDashboardListItem False tmplItemClass ("docker", "fa-docker fa-brands") (maybeToMonoid dashTmpl.title) (maybeToMonoid dashTmpl.file)

      div_ [class_ "col-span-5 px-3 py-5 divide-y h-full overflow-y-scroll "] do
        div_ [class_ "flex gap-3 pb-5"] do
          div_ [class_ "p-2 bg-fillWeaker rounded-lg"] $ faIcon_ "cards-blank" "regular" "w-8 h-8"
          div_ [class_ "flex-1"] do
            strong_ [class_ "text-xl"] "Custom Dashboard"
            p_ [class_ "text-sm"] "Get started from a blank slate"
          div_ [class_ "flex items-center justify-center shrink"] $ button_ [class_ "leading-none rounded-xl p-3 cursor-pointer bg-gradient-to-b from-[#067cff] to-[#0850c5] text-white", type_ "submit"] "Select dashboard"
        div_ [class_ "pt-5"] do
          div_ [class_ "bg-[#1e9cff] px-5 py-8 rounded-xl"] $ img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full"]
  div_ [id_ "itemsListPage", class_ "mx-auto px-6 pt-4 gap-8 w-full flex flex-col h-full overflow-hidden pb-12  group/pg"] do
    div_ [class_ "flex"] do
      label_ [class_ "input input-md input-bordered flex-1 flex bg-fillWeaker border-slate-200 shadow-none overflow-hidden items-center gap-2"] do
        faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
        input_ [type_ "text", class_ "grow", placeholder_ "Search", [__|on input show .itemsListItem in #itemsListPage when its textContent.toLowerCase() contains my value.toLowerCase()|]]
    -- button_
    div_ [class_ "grid grid-cols-2 gap-5"] do
      forM_ dg.dashboards \dashVM -> do
        let dash = loadDashboardFromVM dashVM
        a_ [class_ "rounded-xl border border-slate-200 gap-3.5 p-4 bg-fillWeaker flex", href_ ("/p/" <> dg.projectId.toText <> "/dashboards/" <> dashVM.id.toText)] do
          div_ [class_ "flex-1 space-y-2"] do
            div_ [class_ "flex items-center gap-2"] do
              strong_ [class_ "font-medium"] (toHtml dashVM.title)
              span_ [class_ "leading-none", term "data-tippy-content" "This dashboard is currently your homepage."] do
                when (isJust dashVM.homepageSince) $ (faSprite_ "house" "regular" "w-4 h-4")
            div_ [class_ "gap-2 flex items-center"] do
              time_ [class_ "mr-2 text-slate-400", datetime_ $ Utils.formatUTC dashVM.createdAt] $ toHtml $ formatTime defaultTimeLocale "%eth %b %Y" dashVM.createdAt
              forM_ dashVM.tags (a_ [class_ "cbadge-sm badge-neutral cbadge bg-slate-200"] . toHtml @Text)
          div_ [class_ "flex items-center justify-center gap-3"] do
            button_ [class_ "rounded-full border border-slate-300 p-2 leading-none text-gray-700"] $
              if isJust dashVM.starredSince
                then (faSprite_ "star" "solid" "w-5 h-5")
                else (faSprite_ "star" "regular" "w-5 h-5")
            div_ [class_ "space-x-2"] $ faSprite_ "chart-area" "regular" "w-5 h-5" >> (span_ . toHtml $ maybe "0" (show . length . (.widgets)) dash <> " charts")


dashboardsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx DashboardsGet))
dashboardsGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  dashboards <- dbtToEff $ DBT.selectManyByField @Dashboards.DashboardVM [DBT.field| project_id |] pid
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Dashboards"
          , pageActions = Just $ (label_ [Lucid.for_ "newDashboardMdl", class_ "leading-none rounded-xl p-3 cursor-pointer bg-gradient-to-b from-[#067cff] to-[#0850c5] text-white"] "New Dashboard")
          }
  addRespHeaders $
    PageCtx bwconf $
      DashboardsGet{dashboards, projectId = pid}


data DashboardForm = DashboardForm
  { file :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


dashboardsPostH :: Projects.ProjectId -> DashboardForm -> ATAuthCtx (RespHeaders NoContent)
dashboardsPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  did <- Dashboards.DashboardId <$> UUID.genUUID
  let dashM = find (\dashboard -> dashboard.file == Just form.file) dashboardTemplates
  let redirectURI = "/p/" <> pid.toText <> "/dashboards/" <> (did.toText)
  dbtToEff $
    DBT.insert @Dashboards.DashboardVM $
      Dashboards.DashboardVM
        { id = did
        , projectId = pid
        , createdAt = now
        , updatedAt = now
        , createdBy = sess.user.id
        , baseTemplate = if form.file == "" then Nothing else Just form.file
        , schema = Nothing
        , starredSince = Nothing
        , homepageSince = Nothing
        , tags = V.fromList $ fromMaybe [] $ dashM >>= (.tags)
        , title = fromMaybe [] $ dashM >>= (.title)
        }
  redirectCS redirectURI
  addRespHeaders NoContent


-- -- Template Haskell splice to generate the list of dashboards by reading the dashboards folder in filesystem
dashboardTemplates :: [Dashboards.Dashboard]
dashboardTemplates = $(Dashboards.readDashboardsFromDirectory "static/public/dashboards")
