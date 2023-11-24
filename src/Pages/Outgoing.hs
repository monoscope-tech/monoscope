module Pages.Outgoing (outgoingGetH) where

import Config
import Data.Default (def)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper
import Pages.Charts.Charts (QueryBy (QBHost))
import Pages.Charts.Charts qualified as Charts
import Pages.NonMember
import Relude
import Utils


outgoingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
outgoingGetH sess pid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, hostsEvents) <- liftIO $ withPool pool do
        project <- Projects.projectById pid
        hostsAndEvents <- Endpoints.dependenciesAndEventsCount pid
        pure (project, hostsAndEvents)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Dependencies"
              }
      pure $ bodyWrapper bwconf $ outgoingPage pid hostsEvents


outgoingPage :: Projects.ProjectId -> V.Vector Endpoints.HostEvents -> Html ()
outgoingPage pid hostsEvents = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24"] $ do
  h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Dependencies"
  div_ [class_ "mt-8 space-y-4 py-8 "] do
    div_ [class_ "flex px-8 w-full justify-between"] do
      h3_ [class_ "font-bold"] "Hosts"
      h3_ [class_ "font-bold"] "Events"
    div_ [class_ "flex flex-col"] do
      forM_ hostsEvents $ \host -> do
        div_ [class_ "flex border border-t-transparent items-center"] do
          a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "flex  w-full justify-between items-center p-8 hover:bg-gray-50"] $ do
            span_ [class_ "p-2", href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host] $ toHtml host.host
          -- div_ [class_ "w-[200px] h-[80px] mt-4 shrink-0"] do
          -- Charts.throughput pid host.host (Just (QBHost host.host)) Nothing 14 Nothing False (Nothing, Nothing) Nothing
          div_ [class_ "shrink-0 flex items-center gap-10 p-8"] do
            a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%20%3D%20" <> "\"" <> host.host <> "\"", class_ "p-2 shrink-0 pl-8 text-blue-500 hover:text-slate-600"] "View logs"
            span_ [] $ show host.eventCount
    when (null hostsEvents) $ div_ [class_ "flex flex-col text-center justify-center items-center h-32"] $ do
      strong_ "No dependencies yet."
      p_ "All dependencies' host names and number of events will be shown here."
