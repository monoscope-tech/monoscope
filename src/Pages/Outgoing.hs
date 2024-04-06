{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pages.Outgoing (outgoingGetH) where

import Data.Default (def)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT
import Effectful.PostgreSQL.Transact.Effect
import Lucid
import Lucid.Hyperscript
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Pages.BodyWrapper
import Pages.Charts.Charts (QueryBy (QBHost))
import Pages.Charts.Charts qualified as Charts
import Pages.NonMember
import Pages.Onboarding qualified as Onboarding
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import System.Types
import Utils
import Effectful.Reader.Static (ask, asks)
import Pages.BodyWrapper
import Pages.NonMember (userNotMemeberPage)  -- Fix the function name here
import Lucid.Htmx
import Lucid.Hyperscript.QuasiQuoter
import Models.Apis.Anomalies qualified as Anomalies
import Fmt (commaizeF, fixedF, fmt, (+|), (|+))
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.Charts.Charts (QueryBy (QBHost))
import System.Config
import System.Types
import Pages.Charts.Charts qualified as Charts

-- Define the type for the authentication context
-- type ATAuthCtx :: Type -> Type
-- type ATAuthCtx =
--   Eff
--     '[ Effectful.Reader.Static.Reader (Headers '[Header "Set-Cookie" SetCookie] Sessions.Session)
--      , Effectful.Reader.Static.Reader AuthContext
--      , DB
--      , Time
--      , Log
--      , Error ServerError
--      , IOE
--      ]

data OutgoingParamInput = OutgoingParamInput
  { sortField :: Text
  , search :: Text
  , activeTab :: Text
  }

outgoingGetH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
outgoingGetH pid sortM searchM activeTabM = do
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then pure $ userNotMemeberPage sess 
    else do
      (project, hostsEvents) <- dbtToEff do
        project <- Projects.projectById pid
        hostsAndEvents <- Endpoints.dependenciesAndEventsCount pid
        pure (project, hostsAndEvents)
      let bwconf =
            def
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Dependencies"
              }
      let paramInput =
            OutgoingParamInput
              { sortField = fromMaybe "" sortM
              , search = fromMaybe "" searchM
              , activeTab = fromMaybe "Active" activeTabM
              }
      pure $ bodyWrapper bwconf $ outgoingPage pid paramInput hostsEvents

sortOptions :: [(Text, Text, Text)]
sortOptions =
  [ ("Host Name", "Sort by Host Name", "host")
  , ("Event Count", "Sort by Event Count", "event_count")
  ]

tabs :: [(Text, Text, Text)]
tabs =
  [ ("Active", "Active Outbound Integrations", "active")
  ,("Inbox","Inbox Outbound Integerations", "inbox")
  , ("Archived", "Archived Outbound Integrations", "archived")
  ]

-- outgoingPage :: Projects.ProjectId -> OutgoingParamInput -> V.Vector Endpoints.HostEvents -> Html ()
-- outgoingPage pid paramInput hostsEvents = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
--   h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center"] "Outbound Integrations"


--   div_ [class_ "col-span-4 py-2 space-x-4 border-b border-slate-20 mt-6 mb-8 text-sm font-light"] $ do
--       forM_ tabs $ \(title, desc, identifier) -> do
--         let isActive = activeTab paramInput == identifier
--         a_
--           [ class_ $ "inline-block py-2 " <> if isActive then " font-bold text-black " else ""
--           , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> sortField paramInput <> "&activeTab=" <> identifier
--           ]
--           $ toHtml title
--   div_ [class_ "grid grid-cols-4 card-round", id_ "outgoingListBelowTab"] $ do
--     div_ [class_ "col-span-4 py-2 space-x-4 border-b border-slate-20 mt-6 mb-8 text-sm font-light"] $ do
--       forM_ sortOptions $ \(title, desc, identifier) -> do
--         let isActive = sortField paramInput == identifier
--         a_
--           [ class_ $ "inline-block py-2 " <> if isActive then " font-bold text-black " else ""
--           , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> identifier <> "&activeTab=" <> activeTab paramInput
--           ]
--           $ toHtml title
      

--   div_ [class_ "w-full flex flex-row p-3"] $ do
--     input_
--       [ type_ "text"
--       , placeholder_ "Search"
--       , class_ "dataTable-search w-full h-10 p-2 text-sm text-gray-400 font-normal focus:outline-none"
--       , value_ $ search paramInput
--       , oninput_ $ "showOutgoingItems('#outgoingListBelowTab', 'outgoing_item', '" <> search paramInput <> "')"
--       ]
--   div_ [class_ "col-span-4 bg-white divide-y"] $ do
--     table_ [class_ "w-full table-fixed border-collapse border border-gray-200"] $ do
--       thead_ $ tr_ [class_ "bg-gray-100"] $ do
--         th_ [class_ "px-4 py-2 text-left"] "Host"
--         th_ [class_ "px-4 py-2 text-left"] "Events"
--         th_ [class_ "px-4 py-2 text-center"] "Graph"
--         th_ [class_ "px-4 py-2 text-left"] "View Log"
--       tbody_ $ forM_ hostsEvents $ \host -> do
--         tr_ [class_ "border-b border-gray-200 outgoing_item"] $ do
--           td_ [class_ "px-4 py-2"] $ a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "text-blue-500 hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
--           td_ [class_ "px-4 py-2"] $ toHtml (show host.eventCount)
--           td_ [class_ "px-4 py-2 flex item-center justify-center"] $ do
--             div_ [class_ "mt-4 w-60 h-16 px-3"] $ Charts.throughput pid host.host (Just (QBHost host.host)) Nothing 14 Nothing False (Nothing, Nothing) Nothing
--           td_ [class_ "px-4 py-2 "] $ do
--              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-blue-500 hover:text-slate-600"] $ "View logs"  
--   when (null hostsEvents) $ div_ [class_ "flex flex-col text-center justify-center items-center h-32"] $ do
--     strong_ "No dependencies yet."
--     p_ "All dependencies' host names and number of events will be shown here."
outgoingPage :: Projects.ProjectId -> OutgoingParamInput -> V.Vector Endpoints.HostEvents -> Html ()
outgoingPage pid paramInput hostsEvents = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
  h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center"] "Outbound Integrations"

  div_ [class_ "col-span-4 py-2 space-x-4 border-b border-slate-20 mt-6 mb-8 text-sm font-light"] $ do
    forM_ tabs $ \(title, desc, identifier) -> do
      let isActive = activeTab paramInput == identifier
      a_
        [ class_ $ "inline-block py-2 " <> if isActive then " font-bold text-black " else ""
        , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> sortField paramInput <> "&activeTab=" <> identifier
        ]
        $ toHtml title

  div_ [class_ "grid grid-cols-4 card-round bg-gray-50 ", id_ "outgoingListBelowTab"] $ do
    -- Labels for each column`
    div_ [class_ "col-span-4 py-2 space-x-4 border-b border-slate-20 mt-6 mb-8 text-sm font-light flex justify-between items-center px-2"] $ do
      div_ [class_ "w-1/4"] "Host"
      div_ [class_ "w-1/4"] "Event"
      div_ [class_ "w-1/4"] "Graph"
      div_ [class_ "w-1/4"] "View Log"

    -- Search bar with adjusted spacing
    div_ [class_ "w-full p-3 bg-white border-b border-slate-20"] $ do  
      div_ [class_ "flex w-full bg-white py-2 px-3 flex-row border-solid border border-gray-200 h-10 mb-4 bg-white "] $ do
        faIcon_ "fa-magnifying-glass" "fa-light fa-magnifying-glass" "h-5 w-auto mt-auto mb-auto"
        input_
          [ type_ "text"
          -- , [__| on input show .endpoint_item in #endpoints_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
          , class_ "dataTable-search w-full h-full p-2 ml-2 text-sm text-gray-400 font-normal focus:outline-none"
          , placeholder_ "Search endpoints..."
          ]
        faIcon_ "fa-line-height" "fa-regular fa-line-height" "h-5 w-auto mt-auto mb-auto"   
      
    -- Data rows
    div_ [class_ "col-span-4 bg-white divide-y"] $ do
      forM_ hostsEvents $ \host -> do
        div_ [class_ "border-b border-gray-200 outgoing_item flex justify-between px-4 py-2 align-center mt-8"] $ do
          div_ [class_ "w-1/4"] $ a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "text-blue-500 hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
          div_ [class_ "w-1/4"] $ toHtml (show host.eventCount)
          div_ [class_ "w-1/4 flex item-center justify-center"] $ do
            div_ [class_ "flex items-center justify-center "] $ div_ [class_ "w-60 h-16 "] $ Charts.throughput pid host.host (Just (QBHost host.host)) Nothing 14 Nothing   False (Nothing, Nothing) Nothing
          div_ [class_ "w-1/4"] $ do
            a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-blue-500 hover:text-slate-600"] $ "View logs"
  
  when (null hostsEvents) $ div_ [class_ "flex flex-col text-center justify-center items-center h-32"] $ do
    strong_ "No dependencies yet."
    p_ "All dependencies' host names and number of events will be shown here."
