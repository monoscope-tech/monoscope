module Pages.Monitors.TestCollectionEditor (collectionGetH, collectionPage) where

import Data.Default (def)
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext)
import System.Types (ATAuthCtx)
import Utils qualified
import Utils
import Lucid.Aria qualified as Aria


collectionGetH :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (Html ())
collectionGetH pid col_id = do
  appConf <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  collectionM <- dbtToEff $ Testing.getCollectionById col_id
  project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  collection_steps <- dbtToEff $ Testing.getCollectionSteps col_id
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Testing"
          }
  pure $ bodyWrapper bwconf $ collectionPage pid (Unsafe.fromJust collectionM) collection_steps


collectionPage :: Projects.ProjectId -> Testing.Collection -> V.Vector Testing.CollectionStep -> Html ()
collectionPage pid col steps = do
  -- let col_json = decodeUtf8 $ AE.encode col
  -- let steps_json = decodeUtf8 $ AE.encode steps
  section_ [id_ "test-data", class_ "grid grid-cols-2 h-full divide-x divide-gray-200"] do
    div_ [class_ "col-span-1 h-full divide-y"] do
      div_ [class_ "flex items-center justify-between"] do
        div_ [class_ " pb-5 p-5 space-y-2"] do
          h2_ [class_ "text-base font-semibold leading-6 text-gray-900 flex items-end"] do
            toHtml col.title
            small_ [class_ "inline-block ml-2 truncate text-sm text-gray-500"] "created  2024/01/23"
          p_ [class_ "text-sm"] $ toHtml col.description
        div_ [] do
          span_ [class_ "badge badge-success"] "Active"
          a_ [class_ "p-3"] $ Utils.faSprite_ "ellipsis-vertical" "light" "h-5"
      div_ [class_ "p-4 flex justify-between items-center"] do
        h4_ [class_ "font-semibold text-2xl font-medium "] "Steps"
        div_ [class_ "space-x-4 flex items-center"] do
          button_ [class_ "btn btn-sm btn-success"] do
            span_ "Run all"
            faIcon_ "fa-play" "fa-solid fa-play" "w-3 h-3"
          label_ [class_ "relative inline-flex items-center cursor-pointer space-x-2"] do 
            input_ [type_ "checkbox", class_ "toggle"]
            span_ [class_ "text-sm"] "Code"

        
      div_ [class_ "p-4 space-y-4"] do

        div_ [class_ "divide-y divide-slate-200 rounded-lg border border-slate-200 group/item"] do
          div_ [class_ "flex flex-row items-center bg-gray-50 divide-x divide-slate-200"] do
            div_ [class_ "h-full shrink bg-gray-50 p-3"] $ faIcon_ "fa-grip-dots-vertical" "fa-solid fa-grip-dots-vertical" " h-4 w-4"
            div_ [class_ "flex-1 flex flex-row items-center gap-4 bg-white px-5 py-3"] do 
              span_ "1"
              a_ $ faSprite_ "chevron-right" "solid" "h-4 w-3"
              div_ [class_ "w-full space-y-1 relative"] do
                div_ [class_ "absolute right-0 flex items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex"] do
                  button_ [class_ ""] "View results"
                  button_ [class_ "text-blue-600"] $ faIcon_ "fa-play" "fa-play fa-solid" "w-2 h-3"
                  button_ [class_ "text-red-700"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-2 h-3"
                h4_ [class_ "text-lg"] "Add Chair Object"
                div_ [class_ "flex text-sm"] do 
                  span_ [class_ "rounded-l-lg bg-slate-300 px-3 py-1 font-semibold"] "POST"
                  span_ [class_ "bg-slate-100 px-3 py-1"] "https://api.apitoolkit.io/admin/product/new"
                  span_ [class_ "rounded-r-lg bg-green-600 px-3 py-1 font-bold text-white"] "200 OK"
          div_ [class_ "space-y-2 divide-y xp-3 hidden"] do 
            div_ [class_ "mt-2 flex w-full gap-2"] ""


        div_ [class_ "divide-y divide-slate-200 rounded-lg border border-slate-200 group/item"] do
          div_ [class_ "flex flex-row items-center bg-gray-50 divide-x divide-slate-200"] do
            div_ [class_ "h-full shrink bg-gray-50 p-3"] $ faIcon_ "fa-grip-dots-vertical" "fa-solid fa-grip-dots-vertical" " h-4 w-4"
            div_ [class_ "flex-1 flex flex-row items-center gap-4 bg-white px-5 py-3"] do 
              span_ "2"
              a_ $ faSprite_ "chevron-right" "solid" "h-4 w-3"
              div_ [class_ "w-full space-y-1 relative"] do
                div_ [class_ "absolute right-0 flex items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex"] do
                  button_ [class_ ""] "View results"
                  button_ [class_ "text-blue-600"] $ faIcon_ "fa-play" "fa-play fa-solid" "w-2 h-3"
                  button_ [class_ "text-red-700"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-2 h-3"
                input_ [class_ "text-lg w-full", value_ "Add Chair Object"]
                div_ [class_ "flex text-sm"] do 
                  span_ [class_ "rounded-l-lg bg-slate-300 px-3 py-1 font-semibold"] "POST"
                  span_ [class_ "bg-slate-100 px-3 py-1"] "https://api.apitoolkit.io/admin/product/new"
                  span_ [class_ "rounded-r-lg bg-green-600 px-3 py-1 font-bold text-white"] "200 OK"
          div_ [class_ "space-y-3 pt-4 p-3 "] do 
            div_ [class_ "relative flex flex-row gap-1 "] do 
              label_ [Lucid.for_ "actions-list-input", class_ "w-28  shrink text-sm font-medium form-control "] do
                div_ [class_ "label"] $ span_ [class_ "label-text-alt"] "Method"
                input_ [list_ "actions-list", id_ "actions-list-input", class_ "input input-sm input-bordered w-full", placeholder_ "method"] 
                datalist_ [id_ "actions-list"] do 
                  option_ [value_ "GET", selected_ "selected"] ""
                  option_ [value_ "POST"] ""
              label_ [Lucid.for_ "action-data", class_ "flex-1 text-sm font-medium form-control w-full"] do 
                div_ [class_ "label"] $ span_ [class_ "label-text-alt"] "URI"
                input_ [type_ "text", id_ "action-data", class_ "input input-sm input-bordered w-full", placeholder_ "Request URI"] 
            div_ [role_ "tablist", class_ "tabs tabs-bordered pt-4"] do
              input_ [type_ "radio", name_ "httpOptions", role_ "tab", class_ "tab", Aria.label_ "Params", checked_] 
              div_ [role_ "tabpanel", class_ "tab-content px-2 py-4"] do
                div_ [class_ "flex flex-row items-center gap-2"] do
                  input_ [class_ "shrink input input-xs input-bordered w-1/3", placeholder_ "Param Key"]
                  input_ [class_ "flex-1 input input-xs input-bordered w-full", placeholder_ "Param Value"]
                  div_ [class_ "shrink flex flex-row gap-1"] do
                    a_ [] $ faIcon_ "fa-pen-to-square" "fa-pen-to-square fa-solid" "w-3 h-3"
                    a_ [class_ "text-red-700 cursor-pointer"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-3 h-3"

              input_ [type_ "radio",  name_ "httpOptions", role_ "tab", class_ "tab", Aria.label_ "Headers"] 
              div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 "] "headers"

              input_ [type_ "radio", name_ "httpOptions", role_ "tab", class_ "tab", Aria.label_ "Body"] 
              div_ [role_ "tabpanel", class_ "tab-content px-2 py-4"] do
                div_ "jkfdsjklfd"
            div_ [class_ ""] do 
              h5_ [class_ "label-text p-1 mb-2"] "Assertions"
              div_ [class_ "text-sm space-y-2 px-2"] do
                -- div_ [class_ "flex flex-row items-center gap-2 pl-2 hover:bg-slate-50"] do
                --   span_ "✅"
                --   span_ [class_ "shrink"] "number:"
                --   span_ [class_ "flex-1 w-full"] "$.field.child"
                --   div_ [class_ "shrink flex flex-row gap-1"] do
                --     a_ [] $ faIcon_ "fa-pen-to-square" "fa-pen-to-square fa-solid" "w-3 h-3"
                --     a_ [class_ "text-red-700 cursor-pointer"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-3 h-3"
                -- div_ [class_ "flex flex-row items-center gap-2 pl-2 hover:bg-slate-50"] do
                --   span_ "❌"
                --   span_ [class_ "shrink" ] "ok:"
                --   span_ [class_ "flex-1 w-full"] "$.field.child == $.next.child[0].sub"
                --   div_ [class_ "shrink flex flex-row gap-1"] do
                --     a_ [] $ faIcon_ "fa-pen-to-square" "fa-pen-to-square fa-solid" "w-3 h-3"
                --     a_ [class_ "text-red-700 cursor-pointer"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-3 h-3"

                div_ [class_ "flex flex-row items-center gap-2"] do
                  input_ [type_ "checkbox", class_ "hidden " ]
                  input_ [list_ "assertions-list", class_ "shrink input input-xs input-bordered w-1/4", placeholder_ "Param Key"]
                  datalist_ [id_ "assertions-list"] do
                    option_ [value_ "number", selected_ "selected"] ""
                  input_ [class_ "flex-1 input input-xs input-bordered w-full", placeholder_ "Param Value"]
                  div_ [class_ "shrink flex flex-row gap-1"] do
                    label_ [class_ "cursor-pointer"] $ faIcon_ "fa-pen-to-square" "fa-pen-to-square fa-solid" "w-3 h-3"
                    a_ [class_ "text-red-700 cursor-pointer"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-3 h-3"





    div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
      div_ [class_ "flex flex-col justify-center items-center h-full text-slate-400 text-xl space-y-4"] do
        div_ [] $ Utils.faIcon_ "fa-objects-column" "fa-objects-column fa-solid" "w-16 h-16"
        p_ [class_ "text-slate-500"] "Run a test to view the results here. "



-- script_ [type_ "module", src_ "/assets/testeditor.js"] ("" :: Text)
-- script_ [src_ "/assets/js/thirdparty/jsyaml.min.js", crossorigin_ "true"] ("" :: Text)
-- script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.js"] ("" :: Text)
-- script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/mode/yaml/yaml.js"] ("" :: Text)
-- style_
--   [text|
--       .CodeMirror {
--           height: 100%;
--       }
--   |]
