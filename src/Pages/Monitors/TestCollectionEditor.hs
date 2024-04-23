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
  { stepsData :: Text
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
  form_ [id_ "stepsForm", class_ "grid grid-cols-2 h-full divide-x divide-gray-200 group/colForm", termRaw "data-defaultKeyPrefix" "[kPrefix]"] do
    div_ [class_ "col-span-1 h-full divide-y flex flex-col"] do
      div_ [class_ "shrink flex items-center justify-between"] do
        div_ [class_ " pb-5 p-5 space-y-2"] do
          h2_ [class_ "text-base font-semibold leading-6 text-gray-900 flex items-end"] do
            toHtml col.title
            small_ [class_ "inline-block ml-2 truncate text-sm text-gray-500"] "created  2024/01/23"
          p_ [class_ "text-sm"] $ toHtml col.description
        div_ [] do
          span_ [class_ "badge badge-success"] "Active"
          a_ [class_ "p-3"] $ Utils.faSprite_ "ellipsis-vertical" "light" "h-5"
      div_ [class_ "shrink p-4 flex justify-between items-center"] do
        h4_ [class_ "font-semibold text-2xl font-medium "] "Steps"
        div_ [class_ "space-x-4 flex items-center"] do
          button_ [class_ "btn btn-sm btn-success ", type_ "submit"] do
            span_ "Run all"
            faIcon_ "fa-play" "fa-solid fa-play" "w-3 h-3"
          button_ [class_ "btn btn-sm btn-warning "] do
            span_ "Save"
            faIcon_ "fa-save" "fa-solid fa-save" "w-3 h-3"
          label_ [class_ "relative inline-flex items-center cursor-pointer space-x-2"] do
            input_ [type_ "checkbox", class_ "toggle editorMode", onchange_ "buildAndSetEditor(event)"]
            span_ [class_ "text-sm"] "Code"
      div_ [class_ "h-full flex-1"] do
        div_ [id_ "steps-codeEditor", class_ "h-full max-h-screen hidden group-has-[.editorMode:checked]/colForm:block"] ""
        div_ [class_ "group-has-[.editorMode:checked]/colForm:hidden"] do
          div_ [class_ " p-4 space-y-4 collectionSteps", id_ "collectionStepsContainer"] do
            V.iforM_ steps \idx step -> collectionStep_ (Just $ idx) step
          div_ [class_ "p-4 pt-2"] $ a_ [class_ "btn btn-outline btn-neutral btn-sm items-center cursor-pointer", 
                 [__| on click set :stepTmpl to #collectionStepTmpl.innerHTML.replaceAll('[idx]', #collectionStepsContainer.length)
                          then append :stepTmpl to #collectionStepsContainer 
                          then _hyperscript.processNode(#stepsForm)|]] do
            faSprite_ "plus" "sharp-regular" "w-4 h-4"
            span_ "Add Another Step"
    div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
      div_ [class_ "flex flex-col justify-center items-center h-full text-slate-400 text-xl space-y-4"] do
        div_ [] $ Utils.faIcon_ "fa-objects-column" "fa-objects-column fa-solid" "w-16 h-16"
        p_ [class_ "text-slate-500"] "Run a test to view the results here. "


collectionStep_ :: Maybe Int -> Testing.CollectionStep -> Html ()
collectionStep_ idxM step = do
  let idx = maybe "[idx]" show idxM
  let (sdMethod, sdUri) = fromMaybe ("", "") $ Testing.stepDataMethod step.stepData
  div_ [class_ "divide-y divide-slate-200 rounded-lg border border-slate-200 group/item"] do
    input_ [type_ "checkbox", id_ [fmt|stepState-{idx}|], class_ "hidden stepState"]
    div_ [class_ "flex flex-row items-center bg-gray-50 divide-x divide-slate-200"] do
      div_ [class_ "h-full shrink bg-gray-50 p-3"] $ faIcon_ "fa-grip-dots-vertical" "fa-solid fa-grip-dots-vertical" " h-4 w-4"
      div_ [class_ "flex-1 flex flex-row items-center gap-1 bg-white pr-5 py-3"] do
        label_ [Lucid.for_ [fmt|stepState-{idx}|], class_ "p-3 cursor-pointer"] $ toHtml $ maybe "" (\x->show $ x + 1) idxM
        label_ [Lucid.for_ [fmt|stepState-{idx}|], class_ "p-3 cursor-pointer"] do
          faSprite_ "chevron-right" "solid" "h-4 w-3 group-has-[.stepState:checked]/item:rotate-90"
        div_ [class_ "w-full space-y-1 relative"] do
          div_ [class_ "absolute right-0 flex items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex"] do
            button_ [class_ ""] "View results"
            button_ [class_ "text-blue-600"] $ faIcon_ "fa-play" "fa-play fa-solid" "w-2 h-3"
            button_ [class_ "text-red-700"] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-2 h-3"
          input_ [class_ "text-lg w-full", value_ $ fromMaybe "" step.stepData.title, name_ $ [fmt|[{idx}][title]|], id_ [fmt|title-{idx}|]]
          div_ [class_ "flex text-sm group-has-[.stepState:checked]/item:hidden"] do
            span_ [class_ "rounded-l-lg bg-slate-300 px-3 py-1 font-semibold"] $ toHtml sdMethod
            span_ [class_ "bg-slate-100 px-3 py-1"] $ toHtml sdUri
            span_ [class_ "rounded-r-lg bg-green-600 px-3 py-1 font-bold text-white"] "200 OK"
    div_ [class_ "space-y-3 pt-4 p-3 hidden group-has-[.stepState:checked]/item:block"] do
      div_ [class_ "relative flex flex-row gap-2 items-center px-2"] do
        label_ [Lucid.for_ $ "actions-list-input-" <>idx , class_ "w-28  shrink text-sm font-medium form-control "] do
          input_
            [ list_ "actions-list"
            , id_ $ "actions-list-input-"<>idx
            , class_ "input input-sm input-bordered w-full"
            , placeholder_ "method"
            , value_ sdMethod
            , termRaw "__" [fmt|on change trottle:500ms put me.value into #actions-data-{idx}'s @name |]
            ]
        label_ [Lucid.for_ "actions-data", class_ "flex-1 text-sm font-medium form-control w-full "] do
          div_ [class_ "flex flex-row items-center gap-1"] do
            input_
              [ type_ "text"
              , id_ [fmt|actions-data-{idx}|]
              , class_ "input input-sm input-bordered w-full"
              , placeholder_ "Request URI"
              , name_ [fmt|[{idx}][{sdMethod}]|]
              ]
      div_ [role_ "tablist", class_ "tabs tabs-bordered pt-4"] do
        input_ [type_ "radio", name_ "_httpOptions", role_ "tab", class_ "tab", Aria.label_ "Params", checked_]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 space-y-2", id_ [fmt|[{idx}][params]|]] do
          whenJust step.stepData.params $ \mp -> forM_ (Map.toList mp) \(paramK, paramV) ->
            paramRowKV [fmt|[{idx}][params]|] paramK paramV
          paramRowKV [fmt|[{idx}][params]|] "" ""

        input_ [type_ "radio", name_ "_httpOptions", role_ "tab", class_ "tab", Aria.label_ "Headers"]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 space-y-2", id_ [fmt|[{idx}][headers]|]] do
          whenJust step.stepData.headers $ \mp -> forM_ (Map.toList mp) \(headerK, headerV) ->
            paramRowKV [fmt|[{idx}][headers]|] headerK headerV
          paramRowKV ([fmt|[{idx}][headers]|]) "" ""

        input_ [type_ "radio", name_ "_httpOptions", role_ "tab", class_ "tab", Aria.label_ "Body"]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4"] do
          div_ "jkfdsjklfd"
      div_ [class_ ""] do
        h5_ [class_ "label-text p-1 mb-2"] "Assertions"
        div_ [class_ "text-sm space-y-2 px-2 [&_.assertIndicator]:inline-block",  id_ [fmt|[{idx}][asserts]|]] do
          whenJust step.stepData.asserts $ \vmp -> V.iforM_ vmp \vIdx mp -> forM_ (Map.toList mp) \(assertK, assertV) ->
            paramRowKV ([fmt|[{idx}][asserts][{show vIdx}]|]) "" ""
          let assertsLen = show $ maybe 0 length step.stepData.asserts
          paramRowKV ([fmt|[{idx}][asserts][{assertsLen}]|]) "" ""


valueToText :: AE.Value -> Text
valueToText val = TL.toStrict $ TLE.decodeUtf8 $ AE.encode val


paramRowKV :: Text -> Text -> Text -> Html ()
paramRowKV keyPrefix keyV valV = div_ [class_ "flex flex-row items-center gap-2 paramRow"] do
  span_ [class_ "shrink hidden assertIndicator"] $ "✅"
  input_ [class_ "shrink input input-xs input-bordered w-1/3", placeholder_ "Key", value_ keyV, termRaw "data-keyPrefix" keyPrefix
    , [__|on change set :kPrefix to `${my @data-keyPrefix}[${me.value}]` then set (next <input/>)'s @name to :kPrefix  |]
    ]
  input_ [class_ "flex-1 input input-xs input-bordered w-full", placeholder_ "Value"
    , name_ $ keyPrefix <> "[" <> keyV <> "]", value_ valV]
  div_ [class_ "shrink flex flex-row gap-1 items-center"] do
    a_ [[__|on click set :paramTmpl to #paramRowTmpl.innerHTML.replaceAll(#stepsForm's @data-defaultKeyPrefix, @data-keyPrefix) 
                then put :paramTmpl after the closest parent <div.paramRow/>  
                then _hyperscript.processNode(#stepsForm)|]
      ,  termRaw "data-keyPrefix" keyPrefix] $ faIcon_ "fa-plus" "fa-plus fa-solid" "w-3 h-3"
    a_ [class_ "text-red-700 cursor-pointer", [__|on click remove the closest parent <div.paramRow/> |]] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-3 h-3"




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
  template_ [id_ "paramRowTmpl"] $ paramRowKV "[kPrefix]" "" ""
  template_ [id_ "paramRowTmplFull"] $ paramRowKV "[kPrefix]" "[kKey]" "[kVal]"
  template_ [id_ "collectionStepTmpl"] $ collectionStep_ Nothing (def::Testing.CollectionStep)
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
  script_
    [raw|
    function buildAndSetEditor(event){
     if (event.target.checked) {
        console.log("not checked")
        stepFormToObject()
      } else{
        console.log("window editor value", window.editor.getValue())
        const collectionSteps = jsyaml.load(window.editor.getValue());
        console.log("checked", collectionSteps)
        populateForm(collectionSteps, 'stepsForm')
      }
    }

    function stepFormToObject(){
      const form = document.getElementById("stepsForm");
      const formData = new FormData(form);
      const collectionSteps = [];
      for (const [name, value] of formData.entries()) {
        const parts = name.split(/[\[\]]+/).filter(part => part !== '');
        let currentStep = collectionSteps;
        for (let i = 0; i < parts.length; i++) {
            const part = parts[i];
            const isNumeric = !isNaN(parseInt(parts[i + 1]));
            if (isNumeric && parseInt(parts[i + 1]) >= 0) {
                currentStep[part] = currentStep[part] || [];
                let index = parseInt(parts[++i]); // Move index one step as we know it's an array index
                currentStep[part][index] = currentStep[part][index] || {};
                currentStep = currentStep[part][index];
            } else if (parts[i + 1]) {
                currentStep[part] = currentStep[part] || {};
                currentStep = currentStep[part];
            } else {
                currentStep[part] = value;
            }
        }
      }
      const yamlData = jsyaml.dump(collectionSteps, { indent: 2 });
      console.log(yamlData);
      window.editor.setValue(yamlData)
    }


function populateForm(collectionSteps, formId) {
    const form = document.getElementById(formId);
    const paramRowTmplFull = document.getElementById('paramRowTmplFull').innerHTML;
    const paramRowTmpl = document.getElementById('paramRowTmpl').innerHTML;

    const getTemplate = (kPrefix, key, value) => paramRowTmplFull
        .replace(/\[kPrefix\]\[\[kKey\]\]/g, `${kPrefix}[${key}]`)
        .replace(/\[kPrefix\]/g, kPrefix)
        .replace(/\[kKey\]/g, key)
        .replace(/\[kVal\]/g, value);

    const updateInnerContent = (values, kPrefix) => {
        return Object.entries(values).reduce((content, [key, value]) => 
            content + getTemplate(kPrefix, key, value), '');
    };

    const handleParams = (kPrefix, values) => {
        let innerContent = updateInnerContent(values, kPrefix);
        innerContent += paramRowTmpl.replace('[kPrefix]', kPrefix);
        document.getElementById(kPrefix).innerHTML = innerContent;
    };

    const handleAsserts = (idx, valuesList) => {
        let innerContent = valuesList.reduce((content, values, idxV) => {
            let kPrefix = `[${idx}][asserts][${idxV}]`;
            return content + updateInnerContent(values, kPrefix);
        }, '');

        innerContent += paramRowTmpl.replace('[kPrefix]', `[${idx}][asserts][${valuesList.length}]`);
        document.getElementById(`[${idx}][asserts]`).innerHTML = innerContent;
    };

    const setMethod = (idx, method, val) => {
        form.querySelector(`#actions-list-input-${idx}`).value = method;
        form.querySelector(`#actions-data-${idx}`).value = val;
    };

    function setFields(idx, data) {
        if (data && typeof data === 'object' && !(data instanceof File)) {
            Object.entries(data).forEach(([key, value]) => {
                switch (key) {
                    case 'title':
                        form.querySelector(`#title-${idx}`).value = value;
                        break;
                    case 'params':
                    case 'headers':
                        handleParams(`[${idx}][${key}]`, value);
                        break;
                    case 'asserts':
                        handleAsserts(idx, value);
                        break;
                    default:
                        if (['GET', 'POST', 'PATCH', 'PUT', 'UPDATE', 'DELETE', 'OPTION'].includes(key.toUpperCase())) {
                            setMethod(idx, key, value);
                        }
                        break;
                }
            });
        }
    }

    collectionSteps.forEach((element, idx) => {
        setFields(idx, element);
    });
}


  |]
  script_
    [text|
      document.addEventListener('DOMContentLoaded', function(){
        require.config({ paths: { vs: '/assets/js/monaco/vs' } });
        require.config({ paths: { 'vs': 'https://unpkg.com/monaco-editor/min/vs' } });
		  	require(['vs/editor/editor.main'], function () {
        monaco.editor.defineTheme('nightOwl', {
           base: 'vs-dark',
           inherit: true,
           rules: [
             { token: 'comment', foreground: '#6A9955' },
             { token: 'keyword', foreground: '#C586C0' },
             { token: 'number', foreground: '#B5CEA8' },
             { token: 'string', foreground: '#CE9178' },
             { token: 'operator', foreground: '#D4D4D4' },
             { token: 'identifier', foreground: '#D4D4D4' },
             { token: 'type', foreground: '#4EC9B0' },
             { token: 'delimiter', foreground: '#D4D4D4' },
             { token: 'punctuation', foreground: '#D4D4D4' },
             { token: 'namespace', foreground: '#9CDCFE' },
             { token: 'function', foreground: '#DCDCAA' },
             { token: 'class', foreground: '#4EC9B0' },
             { token: 'variable', foreground: '#D4D4D4' }
           ],
           colors: {
             'editor.foreground': '#D4D4D4',
             'editor.background': '#011627',
             'editor.selectionBackground': '#2D3643',
             'editor.lineHighlightBackground': '#202B33',
             'editorCursor.foreground': '#D4D4D4',
             'editorWhitespace.foreground': '#404040'
           }
        });
       window.monacoEditor = monaco.editor
       // const val = document.querySelector('#swaggerData').value
       // let json = JSON.parse(val)
       // const yamlData = jsyaml.dump(json,{indent:2})
		   window.editor = monaco.editor.create(document.getElementById('steps-codeEditor'), {
            // value: yamlData,
		  			language:'yaml',
            minimap:{enabled:false},
            automaticLayout : true,
            fontSize: 14,
            lineHeight: 20,
            lineNumbersMinChars: 3,
            theme: 'nightOwl'
		  		});
		   });
      })

    |]
