module Pages.Endpoints.EndpointList (endpointListH) where

import Config
import Data.Default (def)
import Data.Text (toLower)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.DBT
  ( withPool,
  )
import Fmt (commaizeF, fixedF, fmt, (+|), (|+))
import Lucid
import Lucid.Htmx
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Optics.Operators
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

endpointListH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
endpointListH sess pid = do
  pool <- asks pool
  (project, endpointStats) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      endpointStats <- Endpoints.endpointRequestStatsByProject pid
      pure (project, endpointStats)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Endpoints"
          }
  pure $ bodyWrapper bwconf $ endpointList endpointStats

endpointList :: Vector Endpoints.EndpointRequestStats -> Html ()
endpointList enps = do
  div_ [class_ "container mx-auto  px-4 pt-10 pb-24 h-full overflow-y-scroll"] $ do
    div_ [class_ "flex justify-between"] $ do
      h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Endpoints"
      div_ [class_ "flex flex-row"] $ do
        button_ [class_ "bg-white rounded-xl py-2 px-4 m-3 h-10 flex flex-row"] $ do
          img_ [src_ "/assets/svgs/download.svg", class_ "h-4 w-6"]
          span_ [class_ "text-sm"] "Export"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
        button_ [class_ "bg-blue-700 h-10  px-2 rounded-xl py-1 mt-3 "] $ do
          img_ [src_ "/assets/svgs/white-plus.svg", class_ "text-white h-4 w-6 text-bold"]
    -- search
    div_ [class_ "card-round p-5 overflow-x-auto"] $ do
      div_ [class_ "w-full flex flex-row m-3"] $ do
        div_ [class_ "flex rounded-xl bg-white py-2 px-3 flex-row w-3/4 border-solid border border-gray-200 h-10"] $ do
          img_ [src_ "/assets/svgs/search.svg", class_ "h-5 w-auto"]
          input_ [type_ "text", class_ "dataTable-search w-full h-full p-2 text-sm text-gray-400 font-normal focus:outline-none", placeholder_ "Search endpoints..."]
          img_ [src_ "/assets/svgs/filter.svg", class_ "h-5 w-auto self-end"]
        button_ [class_ "bg-blue-700/20 place-content-center py-2 px-4 w-28 mx-3 flex flex-row rounded-xl h-10"] $ do
          img_ [src_ "/assets/svgs/merge.svg", class_ "h-4 w-4 mt-1 "]
          span_ [class_ "text-blue-600"] "Merge"
        button_ [class_ "bg-transparent place-content-center py-2 px-4 w-28 mx-3 flex flex-row border-solid border border-gray-200 rounded-xl h-10"] $ do
          span_ [class_ "text-sm text-slate-600 mr-1"] "Actions"
          img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 "]
      -- table head

      table_ [class_ "table-auto w-full  mt-6", id_ "apitab", hxBoost_ "true"] $ do
        thead_ $ do
          tr_ [class_ "border-b border-b-slate-50 text-sm text-gray-400 font-normal "] $ do
            th_ [class_ "text-left "] $ do
              input_ [type_ "checkbox"]
            th_ [style_ "width:7rem; min-width:7rem; max-width:7rem;"] ""
            th_ [class_ ""] "ENDPOINTS"
            th_ [class_ "text-center"] "REQUESTS"
            th_ [class_ "text-right"] "TOTAL TIME"
            th_ [class_ "text-right"] "P50 LATENCY"
            th_ [class_ "text-right"] "P99 LATENCY"
            th_ [class_ "text-center"] ""
        tbody_ $ do
          enps & mapM_ \enp -> do
            tr_ [class_ "border-b border-b-slate-50 py-2"] $ do
              td_ [class_ "text-left pr-4 "] $ input_ [type_ "checkbox"]
              td_ [class_ "text-right"] $ do
                a_ [href_ ("/p/" <> Projects.projectIdText (enp ^. #projectId) <> "/endpoints/" <> Endpoints.endpointIdText (enp ^. #endpointId))] $ do
                  span_ [class_ $ "endpoint endpoint-" <> toLower (enp ^. #method)] $ toHtml $ enp ^. #method
              td_ [class_ ""] $ do
                a_ [href_ ("/p/" <> Projects.projectIdText (enp ^. #projectId) <> "/endpoints/" <> Endpoints.endpointIdText (enp ^. #endpointId))] $ do
                  span_ [class_ " inconsolata text-base text-slate-700"] $ toHtml $ enp ^. #urlPath
              td_ [class_ " text-sm text-gray-400 font-normal text-center"] $ do
                span_ $ toHtml @String $ fmt $ commaizeF (enp ^. #totalRequests)
              td_ [class_ " inconsolata text-base text-slate-700 text-right space-x-2"] $ do
                span_ $ toHtml @String $ fmt $ fixedF 1 (enp ^. #totalTime) -- convert from milliseconds
                div_ [class_ "w-10 inline-block", term "data-tippy-content" "enpoint total time vs project total"] $ meter__ $ ((enp ^. #totalTime) / (enp ^. #totalTimeProj)) * 100
              td_ [class_ " text-sm text-gray-400 font-normal text-right space-x-2"] $ do
                span_ $ toHtml @Text $ fmt $ fixedF 2 (enp ^. #p50) |+ " ms"
                div_ [class_ "w-10 inline-block", term "data-tippy-content" "p50 vs max"] $ meter__ $ ((enp ^. #p50) / (enp ^. #max)) * 100
              td_ [class_ " text-sm text-gray-400 font-normal text-right space-x-2"] $ do
                span_ $ toHtml @Text $ fmt $ fixedF 2 (enp ^. #p99) |+ " ms"
                div_ [class_ "w-10 inline-block", term "data-tippy-content" "p99 vs max"] $ meter__ $ ((enp ^. #p99) / (enp ^. #max)) * 100
              td_ [class_ "text-right font-medium text-gray-400 "] $ do
                div_ [class_ "flex flex-row w-full justify-end gap-6"] $ do
                  when (enp ^. #ongoingAnomalies > 0) $ do
                    img_ [class_ "px-3", term "data-tippy-content" "ongoing anomaly", src_ "/assets/svgs/alert-red.svg"]
                  img_ [class_ "px-3", src_ "/assets/svgs/dots-vertical.svg"]

meter__ :: Double -> Html ()
meter__ prcntg = div_ [class_ "shadow w-full bg-slate-200 h-2.5 "] $ do
  div_ [class_ "shadow bg-blue-500 h-full", style_ $ fmt $ "width: " +| fixedF 2 prcntg |+ "%"] ""

-------------------------------------------------------------------------------------------
-- TODO: This section holds the navbar for pagination.
-- TODO: We should uncomment it when we have bandwidth to implement endpoint pagination
-------------------------------------------------------------------------------------------
-- table footer
-- README: Hiding the pagination logic because while pagination is important,
-- TODO: SHow this pagination when we're ready to actually implement the pagination logic.
-- we might not quickly have companies who have enough endpoints that they need pagination. So it's better to focus on important topics.
-- div_ [class_ "flex flex-row mt-5 justify-between hidden"] $ do
--   div_ [class_ "flex flex-row"] $ do
--     button_ [class_ "bg-transparent place-content-center py-2 px-3 mx-3 flex flex-row border-solid border border-gray-200 rounded-xl h-10"] $ do
--       span_ [class_ "text-sm text-slate-500 mr-1"] "10"
--       img_ [src_ "/assets/svgs/cheveron-down.svg", class_ "h-3 w-3 mt-1 mx-1"]
--     span_ [src_ "text-gray-200 mt-6 font-light text-base"] "Showing 1 - 10 of 100"
--   div_ [class_ "flex flex-row"] $ do
--     button_ [class_ "bg-gray-100 h-10 w-10 mx-1 px-2 flex flex-row rounded-xl py-3 place-content-center"] $ do
--       img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "-mr-1"]
--       img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "-ml-1"]
--     button_ [class_ "bg-gray-100 h-10 w-10 mx-1 px-2 rounded-xl py-1 place-content-center"] $ do
--       img_ [src_ "/assets/svgs/arrowleft1.svg", class_ "ml-1"]
--     button_ [class_ "bg-blue-700 h-10 w-10 mx-1 px-2 rounded-xl py-1"] $ do
--       span_ [class_ "text-white text-bold"] "1"
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "2"
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "3"
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "..."
--     button_ [class_ "bg-transparent h-10 w-10 mx-1 px-2 rounded-xl py-1 hover:bg-gray-100 "] $ do
--       span_ [class_ "text-slate-700 text-bold"] "5"
--     button_ [class_ "bg-[#304FFD]/20 place-content-center h-10 mx-1 rounded-xl py-1 w-10"] $ do
--       img_ [src_ "/assets/svgs/arrowright1.svg", class_ "ml-3"]
--     button_ [class_ "bg-blue-700/20 place-content-center h-10 mx-1 flex flex-row rounded-xl py-3 w-10"] $ do
--       img_ [src_ "/assets/svgs/arrowright1.svg", class_ "-mr-1"]
--       img_ [src_ "/assets/svgs/arrowright1.svg", class_ "-ml-1"]
