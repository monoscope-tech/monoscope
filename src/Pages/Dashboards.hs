module Pages.Dashboards (dashboardGetH, entrypointGetH, DashboardGet (..), dashboardsGetH, DashboardsGet (..), dashboardsPostH, DashboardForm (..)) where

import Control.Lens
import Data.Aeson qualified as AE
import Data.Default
import Data.Effectful.UUID qualified as UUID
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity qualified as DBT
import Database.PostgreSQL.Entity.DBT qualified as DBT
import Database.PostgreSQL.Entity.Types qualified as DBT
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Lucid
import Lucid.Htmx (hxPost_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper
import Pages.Charts.Charts qualified as Charts
import Pkg.Components qualified as Components
import Pkg.Components.Widget qualified as Widget
import Relude
import Servant (NoContent (..), err404, errBody)
import Servant.API (Header)
import Servant.API.ResponseHeaders (Headers, addHeader)
import System.Types
import Text.Regex.TDFA ((=~))
import Utils (faSprite_)
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

  section_ [class_ "pb-12 h-full"] $ div_ [class_ "mx-auto pt-5 pb-6 px-6 gap-3.5 w-full flex flex-col h-full overflow-y-scroll pb-12 group/pg", id_ "dashboardPage"] do
    div_ "" -- variables selector area
    div_ [class_ "grid-stack  -m-2 mb-20"] $ forM_ dash.widgets (\w -> toHtml (w{Widget._projectId = Just pid}))
    script_
      [text| 
      GridStack.init({
        acceptWidgets: true, 
        cellHeight: '5rem',
        margin:'0.5rem',
        marginTop: 0,
        marginLeft: '0.5rem',
        marginRight: '0.5rem',
        marginBottom: '2rem',
        styleInHead: true
      })
      var nestedGrids = document.querySelectorAll('.nested-grid');
      nestedGrids.forEach(function(nestedEl) {
        GridStack.init({
          acceptWidgets: true,
          cellHeight: '4.9rem',
          marginTop: '0px',
          margin:'0.5rem',
          marginLeft: '0.5rem',
          marginRight: '0.5rem',
          marginBottom: '1rem',
          staticGrid: false, // or true if you want to disable drag/resize in the inner grid
          // You might also want to set a specific handle to avoid conflict with the outer grid
          // handle: '.nested-grid-item-handle'
        }, nestedEl);
      });
      |]


loadDashboardFromVM :: Dashboards.DashboardVM -> Maybe Dashboards.Dashboard
loadDashboardFromVM dashVM = case dashVM.schema of
  Just schema_ -> pure schema_
  Nothing -> find (\d -> d.file == dashVM.baseTemplate) dashboardTemplates


-- | Replace all occurrences of {key} in the input text using the provided mapping.
replacePlaceholders :: M.Map T.Text T.Text -> T.Text -> T.Text
replacePlaceholders mappng input =
  let regex = "\\{([^}]+)\\}" :: T.Text
      go txt =
        case T.unpack txt =~ T.unpack regex :: (String, String, String, [String]) of
          (before, match, after, [key])
            | not (null match) ->
                let replacement = M.findWithDefault (T.pack match) (T.pack key) mappng
                 in T.pack before <> replacement <> go (T.pack after)
          _ -> txt
   in go input


dashboardGetH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx DashboardGet))
dashboardGetH pid dashId fileM fromDStr toDStr sinceStr = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let (fromD, toD, currentRange) = Components.parseTimeRange now (Components.TimePicker sinceStr fromDStr toDStr)

  dashVM <-
    (dbtToEff $ DBT.selectById @Dashboards.DashboardVM (Only dashId)) >>= \case
      Just v -> pure v
      Nothing -> throwError $ err404{errBody = ("Dashboard with ID not found. ID:" <> encodeUtf8 dashId.toText)}

  dash <- case fileM of
    Just file -> Dashboards.readDashboardEndpoint file
    Nothing -> maybe (throwError err404) pure (loadDashboardFromVM dashVM)

  -- Process a single widget recursively.
  let processWidget :: Widget.Widget -> ATAuthCtx Widget.Widget
      processWidget widgetBase = do
        let fromStr = maybeToMonoid $ Utils.formatUTC <$> fromD
            toStr = maybeToMonoid $ Utils.formatUTC <$> toD
        let mappng =
              M.fromList
                [ ("project_id", pid.toText)
                , ("from", fromStr)
                , ("to", toStr)
                , ("time_filter", "(timestamp >= '" <> fromStr <> "' AND timestamp <= '" <> toStr <> "')")
                , ("time_filter_sql_created_at", "(created_at >= '" <> fromStr <> "' AND created_at <= '" <> toStr <> "')")
                ]
            widget =
              widgetBase
                & #sql . _Just %~ replacePlaceholders mappng
                & #query . _Just %~ replacePlaceholders mappng
        widget' <-
          if (widget.eager == Just True || widget.wType `elem` [Widget.WTAnomalies])
            then do
              case widget.wType of
                Widget.WTAnomalies -> do
                  issues <- dbtToEff $ Anomalies.selectIssues pid Nothing (Just False) (Just False) Nothing (Nothing) (0)
                  let issuesVM = V.map (AnomalyList.IssueVM False now "24h") issues
                  pure $
                    widget
                      & #html
                        ?~ ( renderText do
                              div_ [class_ "space-y-4"] do
                                forM_ issuesVM (\x -> div_ [class_ "border border-strokeWeak rounded-2xl overflow-hidden"] $ toHtml x)
                           )
                Widget.WTStat -> do
                  stat <- Charts.queryFloat (Just pid) widget.query Nothing widget.sql sinceStr fromDStr toDStr Nothing
                  pure $
                    widget
                      & #dataset
                        ?~ def
                          { Widget.source = AE.Null
                          , Widget.value = stat.dataFloat
                          }
                _ -> do
                  metricsD <-
                    Charts.queryMetrics (Just pid) widget.query Nothing widget.sql sinceStr fromDStr toDStr Nothing
                  pure $
                    widget
                      & #dataset
                        ?~ Widget.WidgetDataset
                          { source =
                              AE.toJSON $
                                V.cons
                                  (AE.toJSON <$> metricsD.headers)
                                  (AE.toJSON <<$>> metricsD.dataset)
                          , rowsPerMin = metricsD.rowsPerMin
                          , value = Just metricsD.rowsCount
                          , from = metricsD.from
                          , to = metricsD.to
                          }
            else pure widget
        -- Recursively process child widgets, if any.
        case widget'.children of
          Nothing -> pure widget'
          Just childWidgets -> do
            newChildren <- traverse processWidget childWidgets
            pure $ widget' & #children .~ Just newChildren

  -- Traverse the top-level widgets in the dashboard.
  dash' <- forOf (#widgets . traverse) dash processWidget

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


renderDashboardListItem :: Bool -> Text -> Text -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Html ()
renderDashboardListItem checked tmplClass title value description icon prview = label_
  [ class_
      [text| cursor-pointer group/it border border-transparent hover:bg-fillWeaker hover:border-strokeWeak rounded-lg flex p-1.5 gap-2 items-center
      group-has-[input:checked]/it:bg-fillWeaker group-has-[input:checked]/it:border-strokeWeak dashboardListItem|]
  , term "data-title" title
  , term "data-description" $ maybeToMonoid description
  , term "data-icon" $ fromMaybe "square-dashed" icon
  , term "data-preview" $ fromMaybe "/public/assets/svgs/screens/dashboard_blank.svg" prview
  , [__| on mouseover set #dItemPreview.src to my @data-preview 
              then set #dItemTitle.innerText to my @data-title
              then set #dItemDescription.innerText to my @data-description
              then set #dItemIcon.innerHTML to `<svg class='w-8 h-8'><use href='/public/assets/svgs/fa-sprites/regular.svg#${my @data-icon}'></use></svg>` 
          on mouseout
              put (<.dashboardListItem:has(input:checked)/>) into checkedLabel
              set #dItemPreview.src to checkedLabel's @data-preview
              then set #dItemTitle.innerText to checkedLabel's @data-title
              then set #dItemDescription.innerText to checkedLabel's @data-description
              then set #dItemIcon.innerHTML to `<svg class='w-8 h-8'><use href='/public/assets/svgs/fa-sprites/regular.svg#${checkedLabel's @data-icon}'></use></svg>`
              |]
  ]
  do
    input_ ([class_ $ "hidden " <> tmplClass, type_ "radio", name_ "file", value_ value] <> [checked_ | checked])
    span_ [class_ "p-1 px-2 bg-fillWeak rounded-md"] $ faSprite_ (fromMaybe "square-dashed" icon) "regular" "w-4 h-4"
    span_ [class_ "grow"] $ toHtml title
    span_ [class_ "px-2 p-1 invisible group-has-[input:checked]/it:visible"] $ faSprite_ "chevron-right" "regular" "w-4 h-4"


dashboardsGet_ :: DashboardsGet -> Html ()
dashboardsGet_ dg = do
  Components.modal_ "newDashboardMdl" "" $ form_
    [ class_ "grid grid-cols-7 overflow-hidden h-full gap-4 group/md"
    , hxPost_ ""
    ]
    do
      div_ [class_ "col-span-2 space-y-4"] do
        strong_ "Create dashboard"
        label_ [class_ "input input-sm input-bordered flex items-center "] do
          faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
          input_
            [ type_ "text"
            , class_ "grow pl-2"
            , placeholder_ "Search"
            , [__|
              on keyup
                if the event's key is 'Escape' set my value to '' then trigger keyup
                else show <.dashboardListItem/> in #dashListItemParent when its textContent.toLowerCase() contains my value.toLowerCase() |]
            ]
          kbd_ [class_ "kbd kbd-sm"] "/"
        div_ [class_ "space-y-1", id_ "dashListItemParent"] do
          renderDashboardListItem True "tmplRadio0" "Blank dashboard" "" (Just "Get started from a blank slate") (Just "cards-blank") Nothing
          iforM_ dashboardTemplates \idx dashTmpl -> do
            let tmplItemClass = "tmplRadio" <> (show $ idx + 1)
            renderDashboardListItem False tmplItemClass (maybeToMonoid dashTmpl.title) (maybeToMonoid dashTmpl.file) dashTmpl.description dashTmpl.icon dashTmpl.preview

      div_ [class_ "col-span-5 px-3 py-5 divide-y h-full overflow-y-scroll "] do
        div_ [class_ "flex gap-3 pb-5"] do
          div_ $ div_ [class_ "p-2 bg-fillWeaker rounded-lg ", id_ "dItemIcon"] $ faSprite_ "cards-blank" "regular" "w-8 h-8"
          div_ [class_ "flex-1"] do
            strong_ [class_ "text-xl", id_ "dItemTitle"] "Custom Dashboard"
            p_ [class_ "text-sm line-clamp-2 min-h-10", id_ "dItemDescription"] "Get started from a blank slate"
          div_ [class_ "flex items-center justify-center shrink"] $ button_ [class_ "leading-none rounded-lg p-3 cursor-pointer bg-fillBrand-strong shadow text-white", type_ "submit"] "Select template"
        div_ [class_ "pt-5"] $
          div_ [class_ "bg-[#1e9cff] px-5 py-8 rounded-xl aspect-square w-full flex items-center"] $
            img_ [src_ "/public/assets/svgs/screens/dashboard_blank.svg", class_ "w-full", id_ "dItemPreview"]

  div_ [id_ "itemsListPage", class_ "mx-auto px-6 pt-4 gap-8 w-full flex flex-col h-full overflow-hidden pb-12  group/pg"] do
    div_ [class_ "flex"] do
      label_ [class_ "input input-md input-bordered flex-1 flex bg-fillWeaker border-slate-200 shadow-none overflow-hidden items-center gap-2"] do
        faSprite_ "magnifying-glass" "regular" "w-4 h-4 opacity-70"
        input_
          [ type_ "text"
          , class_ "grow"
          , placeholder_ "Search"
          , [__|
              on keyup
                if the event's key is 'Escape' set my value to '' then trigger keyup
                else show <.itemListItem/> in #itemsListPage when its textContent.toLowerCase() contains my value.toLowerCase() |]
          ]
    -- button_
    div_ [class_ "grid grid-cols-2 gap-5"] do
      forM_ dg.dashboards \dashVM -> do
        let dash = loadDashboardFromVM dashVM
        div_ [class_ "rounded-xl border border-strokeWeak gap-3.5 p-4 bg-fillWeaker flex itemListItem"] do
          div_ [class_ "flex-1 space-y-2"] do
            div_ [class_ "flex items-center gap-2"] do
              a_ [class_ "hover:underline underline-offset-2", href_ ("/p/" <> dg.projectId.toText <> "/dashboards/" <> dashVM.id.toText)] $ strong_ [class_ "font-medium"] (toHtml dashVM.title)
              span_ [class_ "leading-none", term "data-tippy-content" "This dashboard is currently your homepage."] do
                when (isJust dashVM.homepageSince) $ (faSprite_ "house" "regular" "w-4 h-4")
            div_ [class_ "gap-2 flex items-center"] do
              time_ [class_ "mr-2 text-slate-400", term "data-tippy-content" "Date of dashboard creation", datetime_ $ Utils.formatUTC dashVM.createdAt] $ toHtml $ formatTime defaultTimeLocale "%eth %b %Y" dashVM.createdAt
              forM_ dashVM.tags (span_ [class_ "cbadge-sm badge-neutral cbadge bg-fillWeak"] . toHtml @Text)
          div_ [class_ "flex items-end justify-center gap-5"] do
            button_ [class_ "leading-none", term "data-tippy-content" "click star this dashboard"] $
              if isJust dashVM.starredSince
                then (faSprite_ "star" "solid" "w-5 h-5")
                else (faSprite_ "star" "regular" "w-5 h-5")
            let widgetCount = maybe "0" (show . length . (.widgets)) dash
            div_ [class_ "flex items-end gap-2", term "data-tippy-content" $ "There are " <> widgetCount <> " charts/widgets in this dashboard"] $ faSprite_ "chart-area" "regular" "w-5 h-5 text-iconNeutral" >> (span_ [class_ "leading-none"] . toHtml $ widgetCount)


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
          , pageActions = Just $ (label_ [Lucid.for_ "newDashboardMdl", class_ "leading-none rounded-xl shadow p-3 cursor-pointer bg-fillBrand-strong text-white"] "New Dashboard")
          }
  addRespHeaders
    $ PageCtx bwconf
    $ DashboardsGet{dashboards, projectId = pid}


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
  dbtToEff
    $ DBT.insert @Dashboards.DashboardVM
    $ Dashboards.DashboardVM
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


entrypointGetH
  :: Projects.ProjectId
  -> ATAuthCtx (Headers '[Header "Location" Text] NoContent)
entrypointGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  now <- Time.currentTime
  let mkPath p d = "/p/" <> pid.toText <> p <> d
      q = [sql|select id::text from projects.dashboards where project_id=? and (homepage_since is not null or base_template='_overview.yaml')|]
      newDashboard = do
        did <- Dashboards.DashboardId <$> UUID.genUUID
        dbtToEff $
          DBT.insert @Dashboards.DashboardVM
            Dashboards.DashboardVM
              { id = did
              , projectId = pid
              , createdAt = now
              , updatedAt = now
              , createdBy = sess.user.id
              , baseTemplate = Just "_overview.yaml"
              , schema = Nothing
              , starredSince = Nothing
              , homepageSince = Nothing
              , tags = V.fromList ["overview", "http", "logs", "traces", "events"]
              , title = "Overview"
              }
        pure did.toText
  redirectTo <-
    if project.paymentPlan == "ONBOARDING"
      then pure $ mkPath "/onboarding" ""
      else (mkPath "/dashboards/") <$> (maybe newDashboard pure =<< dbtToEff (DBT.queryOne DBT.Select q (Only pid)))
  pure $ addHeader redirectTo NoContent
