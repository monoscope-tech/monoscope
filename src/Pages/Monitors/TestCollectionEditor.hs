module Pages.Monitors.TestCollectionEditor (collectionGetH, CollectionStepUpdateForm (..), collectionPage, collectionStepsUpdateH) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base
import Lucid.Hyperscript
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import PyF
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext)
import System.Types (ATAuthCtx)
import Utils
import Web.FormUrlEncoded (FromForm)


data CollectionStepUpdateForm = CollectionStepUpdateForm
  {
    stepsData :: Text 
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


collectionStepsUpdateH :: Projects.ProjectId -> Testing.CollectionId -> CollectionStepUpdateForm -> ATAuthCtx (Html ())
collectionStepsUpdateH pid colId stepsForm = do
  pure $ toHtml ""


collectionGetH :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (Html ())
collectionGetH pid colId = do
  appConf <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  collectionM <- dbtToEff $ Testing.getCollectionById colId
  project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  collection_steps <- dbtToEff $ Testing.getCollectionSteps colId
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
  editorExtraElements
  form_ [id_ "stepsForm", class_ "grid grid-cols-2 h-full divide-x divide-gray-200 group/colForm"] do
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
          button_ [class_ "btn btn-sm btn-success ", type_ "submit"] do
            span_ "Run all"
            faIcon_ "fa-play" "fa-solid fa-play" "w-3 h-3"
          button_ [class_ "btn btn-sm btn-warning "] do
            span_ "Save"
            faIcon_ "fa-save" "fa-solid fa-save" "w-3 h-3"
          label_ [class_ "relative inline-flex items-center cursor-pointer space-x-2"] do
            input_ [type_ "checkbox", class_ "toggle editorMode", onchange_ "buildAndSetEditor()"]
            span_ [class_ "text-sm"] "Code"
      div_ [class_ ""] do
        div_ [id_ "steps-codeEditor", class_ "hidden group-has-[.editorMode:checked]/colForm:block"] ""
        div_ [class_ "group-has-[.editorMode:checked]/colForm:hidden p-4 space-y-4 "] do
          V.iforM_ steps collectionStep_
          button_ [class_ "btn btn-outline btn-neutral btn-sm items-center"] do
            faSprite_ "plus" "sharp-regular" "w-4 h-4"
            span_ "Add Another Step"
    div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
      div_ [class_ "flex flex-col justify-center items-center h-full text-slate-400 text-xl space-y-4"] do
        div_ [] $ Utils.faIcon_ "fa-objects-column" "fa-objects-column fa-solid" "w-16 h-16"
        p_ [class_ "text-slate-500"] "Run a test to view the results here. "


collectionStep_ :: Int -> Testing.CollectionStep -> Html ()
collectionStep_ idx step = do
  let (sdMethod, sdUri) = fromMaybe ("", "") $ Testing.stepDataMethod step.stepData
  div_ [class_ "divide-y divide-slate-200 rounded-lg border border-slate-200 group/item"] do
    input_ [type_ "checkbox", id_ "stepState", class_ "hidden stepState"]
    div_ [class_ "flex flex-row items-center bg-gray-50 divide-x divide-slate-200"] do
      div_ [class_ "h-full shrink bg-gray-50 p-3"] $ faIcon_ "fa-grip-dots-vertical" "fa-solid fa-grip-dots-vertical" " h-4 w-4"
      div_ [class_ "flex-1 flex flex-row items-center gap-1 bg-white pr-5 py-3"] do
        label_ [Lucid.for_ "stepState", class_ "p-3 cursor-pointer"] $ toHtml $ show (idx + 1)
        label_ [Lucid.for_ "stepState", class_ "p-3 cursor-pointer"] do
          faSprite_ "chevron-right" "solid" "h-4 w-3 group-has-[.stepState:checked]/item:rotate-90"
        div_ [class_ "w-full space-y-1 relative"] do
          div_ [class_ "absolute right-0 flex items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex"] do
            button_ [class_ ""] "View results"
            button_ [class_ "text-blue-600"] $ faIcon_ "fa-play" "fa-play fa-solid" "w-2 h-3"
            button_ [class_ "text-red-700"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-2 h-3"
          input_ [class_ "text-lg w-full", value_ $ fromMaybe "" step.stepData.title, name_ $ [fmt|[{show idx}][title]|]]
          div_ [class_ "flex text-sm group-has-[.stepState:checked]/item:hidden"] do
            span_ [class_ "rounded-l-lg bg-slate-300 px-3 py-1 font-semibold"] $ toHtml sdMethod
            span_ [class_ "bg-slate-100 px-3 py-1"] $ toHtml sdUri
            span_ [class_ "rounded-r-lg bg-green-600 px-3 py-1 font-bold text-white"] "200 OK"
    div_ [class_ "space-y-3 pt-4 p-3 hidden group-has-[.stepState:checked]/item:block"] do
      div_ [class_ "relative flex flex-row gap-2 items-center px-2"] do
        label_ [Lucid.for_ "actions-list-input", class_ "w-28  shrink text-sm font-medium form-control "] do
          input_
            [ list_ "actions-list"
            , id_ "actions-list-input"
            , class_ "input input-sm input-bordered w-full"
            , placeholder_ "method"
            , value_ sdMethod
            , termRaw "__" [fmt|on change trottle:500ms put me.value into #ection-data-{show idx}'s @name |]
            ]
        label_ [Lucid.for_ "action-data", class_ "flex-1 text-sm font-medium form-control w-full "] do
          div_ [class_ "flex flex-row items-center gap-1"] do
            input_ [type_ "text", id_ [fmt|action-data-{show idx}|], class_ "input input-sm input-bordered w-full", placeholder_ "Request URI", name_ [fmt|[{show idx}][{sdMethod}]|]]
      div_ [role_ "tablist", class_ "tabs tabs-bordered pt-4"] do
        input_ [type_ "radio", name_ "httpOptions", role_ "tab", class_ "tab", Aria.label_ "Params", checked_]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 space-y-2"] do
          whenJust step.stepData.params $ \mp -> forM_ (Map.toList mp) \(paramK, paramV) ->
            paramRow False (paramK) (paramV) ([fmt|[{show idx}][params][{paramK}]|]) (paramV)
          paramRow False ("") ("") "" ("")

        input_ [type_ "radio", name_ "httpOptions", role_ "tab", class_ "tab", Aria.label_ "Headers"]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 space-y-2"] do
          whenJust step.stepData.headers $ \mp -> forM_ (Map.toList mp) \(headerK, headerV) ->
            paramRow False (headerK) (headerK) ([fmt|[{show idx}][headers][{headerK}]|]) (headerV)
          paramRow False ("") ("") "" ("")

        input_ [type_ "radio", name_ "httpOptions", role_ "tab", class_ "tab", Aria.label_ "Body"]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4"] do
          div_ "jkfdsjklfd"
      div_ [class_ ""] do
        h5_ [class_ "label-text p-1 mb-2"] "Assertions"
        div_ [class_ "text-sm space-y-2 px-2"] do
          whenJust step.stepData.asserts $ \vmp -> V.forM_ vmp \mp -> forM_ (Map.toList mp) \(assertK, assertV) ->
            paramRow True (assertK) (valueToText assertV) ([fmt|[{show idx}][headers][{valueToText assertV}]|]) (valueToText assertV)
          paramRow True ("") ("") "" ("")


valueToText :: AE.Value -> Text
valueToText val = TL.toStrict $ TLE.decodeUtf8 $ AE.encode val


paramRow :: Bool -> Text -> Text -> Text -> Text -> Html ()
paramRow isAssertion keyName keyVal valName valVal = div_ [class_ "flex flex-row items-center gap-2 paramRow"] do
  when isAssertion $ span_ [class_ "shrink"] $ "✅"
  input_ [class_ "shrink input input-xs input-bordered w-1/3", placeholder_ "Key", name_ keyName, value_ keyVal]
  input_ [class_ "flex-1 input input-xs input-bordered w-full", placeholder_ "Value", name_ valName, value_ valVal]
  div_ [class_ "shrink flex flex-row gap-1 items-center"] do
    a_ [[__|on click put #paramRowTmpl.innerHTML after the closest parent <div.paramRow/>  then _hyperscript.processNode(.paramRow)|]] $ faIcon_ "fa-plus" "fa-plus fa-solid" "w-3 h-3"
    a_ [class_ "text-red-700 cursor-pointer", [__|on click remove the closest parent <div.paramRow/> then _hyperscript.processNode(.paramRow)|]] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-3 h-3"


editorExtraElements :: Html ()
editorExtraElements = do
  datalist_ [id_ "assertions-list"] do
    option_ [value_ "number", selected_ "selected"] ""
  datalist_ [id_ "actions-list"] do
    option_ [value_ "GET"] ""
    option_ [value_ "POST"] ""
    option_ [value_ "PUT"] ""
    option_ [value_ "UPDATE"] ""
    option_ [value_ "PATCH"] ""
    option_ [value_ "DELETE"] ""
  template_ [id_ "paramRowTmpl"] $ paramRow False "" "" "" ""
  -- script_ [type_ "module", src_ "/assets/testeditor.js"] ("" :: Text)
  script_ [src_ "/assets/js/thirdparty/jsyaml.min.js", crossorigin_ "true"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/codemirror.min.js"] ("" :: Text)
  script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.63.0/mode/yaml/yaml.js"] ("" :: Text)
  style_
    [text|
        .CodeMirror {
            height: 100%;
        }
    |]
  script_ [text|
    function buildAndSetEditor(){
      stepFormToObject()
    }
    function stepObjectToForm(){

    }
    function stepFormToObject(){
      const stepCodeEditor = document.getElementById("steps-codeEditor")
      const form = document.getElementById("stepsForm");
      const formData = new FormData(form);
      const collectionSteps = [];

      for (const [name, value] of formData.entries()) {
          console.log(name, value)
          const [_, stepIndex, field] = name.match(/\[(\d+)\]\[(\w+)\]/) || [];
          if (stepIndex && field) {
            collectionSteps[stepIndex] = collectionSteps[stepIndex] || {};
            collectionSteps[stepIndex][field] = value;
          }
      }

      console.log(collectionSteps);
      stepCodeEditor.innerHTML = JSON.stringify(collectionSteps)
    }
  |]
