{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pages.Outgoing (outgoingGetH) where

import Data.Default (def)
import Data.Text (Text)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Tuple.Extra (fst3)
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
import PyF qualified



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
    [ ("First Seen", "First time the issue occured", "first_seen")
      ,("Last Seen", "Last time the issue occured", "last_seen")
      ,("Events", "Number of events", "events")
    ]
-- tabs :: [(Text, Text, Text)]
-- tabs =
--   [ ("Active", "Active Outbound Integrations", "active")
--   ,("Inbox","Inbox Outbound Integerations", "inbox")
--   , ("Archived", "Archived Outbound Integrations", "archived")
--   ]

outgoingPage :: Projects.ProjectId -> OutgoingParamInput -> V.Vector Endpoints.HostEvents -> Html ()
outgoingPage pid paramInput hostsEvents =  do 
  div_ [class_ "w-full mx-auto px-16 pt-10 pb-24 overflow-y-scroll h-full"] $ do
    h3_ [class_ "text-xl text-slate-700 flex gap-1 place-items-center mb-10"] "Outbound Integrations"

    -- div_ [class_ "col-span-4 py-2 space-x-4  border-b border-slate-20 mt-6 mb-8 text-sm font-light"] $ do
    --       forM_ tabs $ \(title, desc, identifier) -> do
    --         let isActive = activeTab paramInput == identifier
    --         a_
    --           [ class_ $ "inline-block py-2 " <> if isActive then " font-bold text-black " else ""
    --           , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> sortField paramInput <> "&activeTab=" <> identifier
    --           ]
    --           $ toHtml title
        
          
    div_ [class_ "grid grid-cols-4 card-round", id_ "outgoingListBelowTab"] $ do
      -- Labels for each column`
      div_ [class_ "col-span-4 bg-white divide-y"]  $ do
        div_ [class_ "col-span-4 py-2 space-x-4 border-b border-slate-20 pt-4 text-sm font-light flex justify-between items-center px-2 bg-gray-50 font-base "] $ do
         div_ [class_ ""] $ do 
          div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base font-medium text-black"] "HOST"
         div_ [class_ "flex item-center"] $ do 
          div_ [class_ "relative inline-block"] $ do
            let currentSortTitle = maybe "First Seen" fst3 $ find (\(_, _, identifier) -> identifier == paramInput.sortField) sortOptions
            a_ [class_ "btn-sm bg-transparent border-black hover:shadow-2xl space-x-2", [__|on click toggle .hidden on #sortMenuDiv |]] do
              mIcon_ "sort" "h-4 w-4"
              span_ $ toHtml currentSortTitle
            div_ [id_ "sortMenuDiv", hxBoost_ "true", class_ "p-1 hidden text-sm border border-black-30 absolute right-0 z-10 mt-2 w-72 origin-top-right rounded-md bg-white shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none", tabindex_ "-1"] do
              sortOptions & mapM_ \(title, desc, identifier) -> do
                let isActive = sortField paramInput == identifier
                a_
                  [ class_ $ "block flex flex-row px-3 py-2 hover:bg-blue-50 rounded-md cursor-pointer " <> (if isActive then " text-blue-800 " else "")
                  , href_ $ "/p/" <> pid.toText <> "/outgoing?sort=" <> identifier
                  ]
                  do
                    div_ [class_ "flex flex-col items-center justify-center px-3"] do
                      if isActive then mIcon_ "checkmark4" "w-4 h-5" else mIcon_ "" "w-4 h-5"
                    div_ [class_ "grow space-y-1"] do
                      span_ [class_ "block text-lg"] $ toHtml title
                      span_ [class_ "block "] $ toHtml desc
          div_ [class_ "flex justify-center font-base w-60 content-between gap-14 font-medium text-black"] do
            span_ "GRAPH"
          div_ [class_ "w-36 flex items-center justify-center"] $ span_ [class_ "font-base font-medium text-black"] "EVENTS"
      div_ [class_ "w-full bg-white border-b border-slate-20"] $ do  
        div_ [class_ "w-full flex flex-row p-3"] $ do
            div_ [class_ "relative flex w-full bg-white py-2 px-3 border-solid border border-gray-200 h-10"] $ do
              faIcon_ "fa-magnifying-glass" "fa-light fa-magnifying-glass" "h-5 w-5"
              input_
                [ type_ "text"
                , [__| on input show .endpoint_item in #endpoints_container when its textContent.toLowerCase() contains my value.toLowerCase() |]
                , class_ "dataTable-search w-full h-full p-2 text-gray-500 font-normal focus:outline-none"
                , placeholder_ "Search endpoints..."
                ]   
        
      -- Data rows
      div_ [class_ "col-span-4 bg-white divide-y"] $ do
        forM_ hostsEvents $ \host -> do
          div_ [class_ "border-b border-gray-200 outgoing_item flex justify-between px-4 py-2 align-center mt-8"] $ do
            div_ [class_ "flex item-center justify-center"] $ a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "text-blue-500 hover:text-slate-600"] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
            div_ [class_ "flex item-center justify-center"] $ do
              a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "text-blue-500 hover:text-slate-600"] $ "View logs"
            div_ [class_ "flex item-center justify-center mb-4"] $ do
              div_
                [ class_ "w-56 h-12 px-3"
                , hxGet_ $  "/charts_html?pid=" <> pid.toText <> "&since=14D&query_raw=" <> AnomalyList.escapedQueryPartial [PyF.fmt|host=="{host.host}" | timechart [1d]|]
                , hxTrigger_ "intersect once"
                , hxSwap_ "innerHTML"
                ]
               ""
            div_ [class_ "w-36 flex item-center justify-center"] $ toHtml (show host.eventCount)
    
      when (null hostsEvents) $ div_ [class_ "flex flex-col text-center justify-center items-center h-32"] $ do
        strong_ "No dependencies yet."
        p_ "All dependencies' host names and number of events will be shown here."
