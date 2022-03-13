module Pages.Anomalies.AnomalyList (anomalyListGetH) where

import Config
import Data.Vector
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Models.Apis.Anomalies qualified as Anomalies
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (bodyWrapper)
import Relude

anomalyListGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
anomalyListGetH sess pid = do
  pool <- asks pool
  (project, anomalies) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      anomalies <- Anomalies.selectAnomalies pid
      pure (project, anomalies)
  pure $ bodyWrapper (Just sess) project "Anomalies" $ anomalyList anomalies

anomalyList :: Vector Anomalies.AnomalyVM -> Html ()
anomalyList anomalies = do
  div_ [class_ "container mx-auto  px-4 pt-10 pb-24 h-full overflow-y-scroll"] $ do
    div_ [class_ "flex justify-between"] $ do
      h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Anomalies"
      div_ [class_ "flex flex-row"] $ do
        button_ [class_ "bg-white rounded-xl py-2 px-4 m-3 h-10 flex flex-row"] $ do
          img_ [src_ "/assets/svgs/download.svg", class_ "h-4 w-6"]
          span_ [class_ "text-sm"] "Export"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
        button_ [class_ "bg-blue-700 h-10  px-2 rounded-xl py-1 mt-3 "] $ do
          img_ [src_ "/assets/svgs/white-plus.svg", class_ "text-white h-4 w-6 text-bold"]
