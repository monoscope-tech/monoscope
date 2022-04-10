module Pages.Log (apiLog) where

import Config
import Data.Default (def)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Optics.Core ((^.))
import Pages.BodyWrapper (BWConfig, bodyWrapper, currProject, pageTitle, sessM)
import Relude
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)

apiLog :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
apiLog sess pid = do
  pool <- asks pool
  (project, requests) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      requests <- RequestDumps.selectRequestDumpByProject pid
      pure (project, requests)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Logs"
          }
  pure $ bodyWrapper bwconf $ apiLogsPage pid requests

apiLogsPage :: Projects.ProjectId -> Vector RequestDumps.RequestDumpLogItem -> Html ()
apiLogsPage pid requests =
  section_ [class_ "container mx-auto  px-4 py-10"] $ do
    div_ [class_ "flex justify-between mb-5"] $ do
      h3_ [class_ "place-items-center"] "ApiToolKit"
      div_ [class_ "flex flex-row"] $ do
        img_ [src_ "/public/assets/svgs/funnel.svg", class_ "h-4 mt-4 mx-3 w-auto"]
        div_ [class_ "flex flex-row px-5  my-2  text-sm bg-white text-zinc-500 border-solid border border-gray-200 rounded-2xl"] $ do
          input_ [type_ "date", class_ " h-10 "]
          span_ [class_ "mx-4 mt-2 text-base text-zinc-500"] "--"
          input_ [type_ "date", class_ " h-10 "]
    div_ [class_ "card rounded-lg mb-5 bg-white border-solid border-2 border-light-blue-500 p-5 overflow-x-auto"] "Charts"

    div_ [class_ "card rounded-lg bg-white border-solid border-2 border-light-blue-500 p-5 overflow-x-auto"] $ do
      div_ [class_ "w-full flex flex-row justify-between m-3"] $ do
        div_ [class_ "flex rounded-xl bg-white py-2 px-3 flex-row w-3/4 border-solid border border-gray-200 h-10"] $ do
          img_ [src_ "/assets/svgs/search.svg", class_ "h-5 w-auto"]
          input_ [type_ "text", class_ " w-full h-full p-2 text-sm text-gray-400 font-normal focus:outline-none", placeholder_ "Search logs"]
          img_ [src_ "/assets/svgs/filter.svg", class_ "h-5 w-auto self-end"]
        button_ [class_ "bg-transparent place-content-center py-2 px-4 w-28 mx-3 flex flex-row border-solid border border-gray-200 rounded-xl h-10"] $ do
          span_ [class_ "text-sm text-slate-600 mr-1"] "Actions"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 "]
      -- table head

      table_ [class_ "table-auto w-full  mt-6"] $ do
        thead_ $ do
          tr_ [class_ "border-b border-b-gray-300 py-5 font-semibold "] $ do
            th_ [class_ "text-left text-sm text-slate-700 "] "DATE"
            th_ [class_ "text-left text-sm text-slate-700 "] "HOST"
            th_ [class_ "text-left text-sm text-slate-700 "] "SERVICE"
        tbody_ $ do
          requests & mapM_ \req -> do
            tr_ [class_ "border-b border-b-gray-300 py-8 font-medium"] $ do
              td_ [class_ " text-sm inconsolata text-slate-700 font-normal"]$ toHtml @String $ formatTime defaultTimeLocale "%F %R" (req ^. #createdAt)
              td_ [class_ " text-sm inconsolata text-slate-700 font-normal"] $ toHtml $ req ^. #host
              td_ [class_ " inconsolata text-base text-slate-700"] $ toHtml $ req ^. #urlPath
              
