{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pages.Projects.ListProjects (
  listProjectsGetH,
  ListProjectsGet (..),
)
where

import Data.Default (def)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask)
import Fmt
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.Widget (Widget (..), WidgetType (..), widget_)
import Relude hiding (ask, asks)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types
import Utils (faSprite_)


listProjectsGetH :: ATAuthCtx (RespHeaders ListProjectsGet)
listProjectsGetH = do
  (sess, project) <- Sessions.sessionAndProject (Projects.ProjectId UUID.nil)
  appCtx <- ask @AuthContext
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , pageTitle = "Projects"
          , hideNavbar = True
          , pageActions = Nothing
          , enableBrowserMonitoring = appCtx.env.enableBrowserMonitoring
          }

  projects <- dbtToEff $ Projects.selectProjectsForUser sess.persistentSession.userId
  let demoProject =
        (def :: Projects.Project')
          { Projects.title = project.title
          , Projects.description = project.description
          , Projects.createdAt = project.createdAt
          }

  addRespHeaders $ ListProjectsGet $ PageCtx bwconf (projects, demoProject)


newtype ListProjectsGet = ListProjectsGet {unwrap :: PageCtx (V.Vector Projects.Project', Projects.Project')}
  deriving stock (Show)


instance ToHtml ListProjectsGet where
  toHtml (ListProjectsGet (PageCtx bwconf (projects, demoProject))) = toHtml $ PageCtx bwconf $ listProjectsBody bwconf.sessM projects demoProject
  toHtmlRaw = toHtml


listProjectsBody :: Maybe Sessions.Session -> V.Vector Projects.Project' -> Projects.Project' -> Html ()
listProjectsBody sessM projects demoProject = do
  -- Custom succinct navbar
  nav_ [class_ "fixed top-0 left-0 right-0 bg-bgBase border-b border-strokeWeak z-50"] do
    div_ [class_ "flex items-center justify-between px-6 py-3"] do
      -- Logo
      a_ [href_ "/", class_ "flex items-center"] do
        img_ [class_ "h-6 dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
        img_ [class_ "h-6 hidden dark:block", src_ "/public/assets/svgs/logo_white.svg"]
      -- User actions
      div_ [class_ "flex items-center gap-3"] do
        -- Dark mode toggle
        label_ [class_ "swap swap-rotate"] $ do
          input_
            ( [ type_ "checkbox"
              , class_ "theme-controller"
              , id_ "dark-mode-toggle-navbar"
              , onclick_ "toggleDarkMode()"
              ]
                <> [checked_ | maybe True (\s -> s.theme /= "light") sessM]
            )
          -- Sun icon (shown in light mode)
          span_ [class_ "swap-off"] $ faSprite_ "sun-bright" "regular" "h-5 w-5"
          -- Moon icon (shown in dark mode)
          span_ [class_ "swap-on"] $ faSprite_ "moon-stars" "regular" "h-5 w-5"
        a_ [class_ "btn btn-ghost btn-sm", href_ "https://apitoolkit.io/docs/", target_ "_blank"] "Docs"
        a_ [class_ "btn btn-ghost btn-sm text-textError", href_ "/logout"] "Logout"

  section_ [id_ "main-content", class_ "mx-auto p-6 pb-36 pt-20 overflow-y-auto h-full"] do
    -- Header
    div_ [class_ "flex justify-between items-center mb-8"] do
      h2_ [class_ "text-textStrong text-3xl font-semibold"] "Projects"
      a_ [class_ "btn btn-primary btn-sm", href_ "/p/new"] do
        faSprite_ "plus" "regular" "h-4 w-4 mr-2"
        "New Project"

    -- User Projects Grid
    when (not $ V.null projects) do
      div_ [class_ "mb-12"] do
        h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Your Projects"
        div_ [class_ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"] do
          mapM_ projectCard_ $ V.toList projects

    -- Demo Project Section
    div_ [] do
      h3_ [class_ "text-textWeak text-lg font-medium mb-4"] "Demo Project"
      div_ [class_ "grid grid-cols-1 max-w-md"] do
        projectCard_ demoProject


projectCard_ :: Projects.Project' -> Html ()
projectCard_ project = do
  div_ [class_ "bg-base-100 border border-strokeWeak rounded-xl shadow-sm hover:shadow-md transition-shadow duration-200 overflow-hidden group"] do
    a_ [href_ ("/p/" <> project.id.toText), class_ "block"] do
      -- Card Header
      div_ [class_ "p-5 pb-3"] do
        div_ [class_ "flex justify-between items-start mb-3"] do
          div_ [class_ "flex-1 min-w-0"] do
            h4_ [class_ "text-textStrong font-semibold text-lg truncate group-hover:text-textBrand transition-colors"] $ toHtml project.title
            p_ [class_ "text-textWeak text-sm mt-1 line-clamp-2"] $ toHtml project.description
          faSprite_ "arrow-right" "regular" "h-4 w-4 text-textWeak opacity-0 group-hover:opacity-100 transition-opacity ml-2 mt-1"

        -- Metadata
        div_ [class_ "flex items-center justify-between text-sm text-textWeak"] do
          div_ [class_ "flex items-center gap-1"] do
            faSprite_ "calendar" "regular" "h-3.5 w-3.5"
            time_ [datetime_ $ fmt $ dateDashF project.createdAt] $ toHtml @Text $ fmt $ dateDashF project.createdAt

          -- User Avatars
          when (not $ V.null project.usersDisplayImages) do
            div_ [class_ "flex -space-x-2"] do
              project.usersDisplayImages & V.toList & take 3 & mapM_ \imgSrc ->
                img_ [class_ "inline-block h-6 w-6 rounded-full ring-2 ring-base-100", src_ imgSrc, alt_ "User avatar"]
              when (V.length project.usersDisplayImages > 3) do
                span_ [class_ "flex items-center justify-center h-6 w-6 rounded-full bg-fillWeak text-textWeak text-xs ring-2 ring-base-100"] do
                  toHtml ("+" <> show (V.length project.usersDisplayImages - 3))

      -- Event Chart Widget
      div_ [class_ "border-t border-strokeWeak bg-fillWeaker/30 px-4 pt-8 h-36"] do
        widget_
          $ (def :: Widget)
            { wType = WTTimeseriesLine
            , id = Just project.id.toText
            , title = Nothing
            , subtitle = Nothing
            , hideSubtitle = Just True
            , query = Nothing
            , -- , query = Just "summarize count(*) by bin(timestamp, 1h)"
              _projectId = Just project.id
            , naked = Just True
            , -- , showTooltip = Just True
              -- , layout = Just (def{w = Just 12, h = Just 2})
              hideLegend = Just True
            , standalone = Just True
            }
