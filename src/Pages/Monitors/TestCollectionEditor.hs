module Pages.Monitors.TestCollectionEditor (collectionGetH, CollectionStepUpdateForm (..), collectionRunTestsH, collectionPage, collectionStepsUpdateH) where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Either.Extra
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Foreign.C.String (peekCString, withCString)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base
import Lucid.Htmx
import Lucid.Hyperscript
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import PyF
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import RustInterop (run_testkit)
import System.Config (AuthContext)
import System.Types (ATAuthCtx)
import Utils
import Data.Aeson.Encode.Pretty


data CollectionStepUpdateForm = CollectionStepUpdateForm
  { stepsData :: V.Vector Testing.CollectionStepData
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via (DAE.CustomJSON) '[DAE.OmitNothingFields] CollectionStepUpdateForm


collectionStepsUpdateH :: Projects.ProjectId -> Testing.CollectionId -> CollectionStepUpdateForm -> ATAuthCtx (Html ())
collectionStepsUpdateH pid colId stepsForm = do
  traceShowM stepsForm
  _ <- dbtToEff $ Testing.updateCollectionSteps colId stepsForm.stepsData
  -- TODO: toast
  pure $ toHtml ""


callRunTestkit :: String -> IO String
callRunTestkit hsString = withCString hsString $ \cstr -> do
  resultCString <- run_testkit (cstr)
  peekCString (resultCString)


collectionRunTestsH :: Projects.ProjectId -> Testing.CollectionId -> Maybe Int -> CollectionStepUpdateForm -> ATAuthCtx (Html ())
collectionRunTestsH pid colId runIdxM stepsForm = do
  traceShowM stepsForm
  tkResp <- liftIO $ callRunTestkit $ decodeUtf8 $ AE.encode $ stepsForm.stepsData
  let stepResults = fromRight' $ AE.eitherDecodeStrictText (toText tkResp) :: V.Vector Testing.StepResult
  traceShowM "RESP RS"
  traceShowM tkResp
  traceShowM stepResults
  pure $ do
    script_
      []
      [fmt|window.collectionResults = {tkResp}
    window.renderCollection()|]
    V.forM_ stepResults collectionStepResult_


collectionGetH :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (Html ())
collectionGetH pid colId = do
  appConf <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  collectionM <- dbtToEff $ Testing.getCollectionById colId
  traceShowM collectionM
  project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Testing"
          }
  pure $ bodyWrapper bwconf $ collectionPage pid (Unsafe.fromJust collectionM)


collectionPage :: Projects.ProjectId -> Testing.Collection -> Html ()
collectionPage pid col = do
  let collectionStepsJSON = AE.encode col.collectionSteps
  script_ [] [fmt|window.collectionSteps = {collectionStepsJSON};|]
  editorExtraElements
  section_ [class_ "h-full overflow-y-hidden"] do
    form_
      [ id_ "stepsForm"
      , class_ "grid grid-cols-2 h-full divide-x divide-gray-200 group/colForm overflow-y-hidden"
      , termRaw "data-defaultKeyPrefix" "[kPrefix]"
      , hxPost_ ""
      , hxSwap_ "none"
      , hxParams_ "stepsData"
      , hxExt_ "json-enc"
      , hxVals_ "js:{stepsData: window.collectionSteps}"
      ]
      do
        div_ [class_ "col-span-1 h-full divide-y flex flex-col overflow-y-hidden"] do
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
              button_
                [ class_ "btn btn-sm btn-success "
                , hxPatch_ ""
                , hxParams_ "stepsData"
                , hxExt_ "json-enc"
                , hxVals_ "js:{stepsData: window.collectionSteps}"
                , hxTarget_ "#step-results-parent"
                , hxSwap_ "innerHTML"
                ]
                do
                  span_ "Run all" >> faSprite_ "play" "solid" "w-3 h-3"
              button_ [class_ "btn btn-sm btn-warning ", type_ "submit"] do
                span_ "Save" >> faSprite_ "floppy-disk" "solid" "w-3 h-3"
              label_ [class_ "relative inline-flex items-center cursor-pointer space-x-2"] do
                input_ [type_ "checkbox", class_ "toggle editorMode", onchange_ "buildAndSetEditor(event)"]
                span_ [class_ "text-sm"] "Code"
          div_ [class_ "h-full flex-1 overflow-y-hidden"] do
            div_ [id_ "steps-codeEditor", class_ "h-full max-h-screen hidden group-has-[.editorMode:checked]/colForm:block"] ""
            div_ [class_ "h-full overflow-y-scroll group-has-[.editorMode:checked]/colForm:hidden"] do
              div_
                [ class_ " p-4 space-y-4 collectionSteps"
                , id_ "collectionStepsContainer"
                ]
                ""
              div_ [class_ "p-4 pt-2"] $ a_
                [ class_ "btn btn-outline btn-neutral btn-sm items-center cursor-pointer"
                , [__| on click set :stepTmpl to #collectionStepTmpl.innerHTML.replaceAll('[idx]', #collectionStepsContainer.childNodes.length)
                            then put :stepTmpl at the end of #collectionStepsContainer
                            then _hyperscript.processNode(#stepsForm)
                            |]
                ]
                do
                  faSprite_ "plus" "sharp-regular" "w-4 h-4"
                  span_ "Add Another Step"
        div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
          div_ [class_ "max-h-full overflow-y-scroll", id_ "step-results-parent"] ""

          div_ [class_ "flex flex-col justify-center items-center h-full text-slate-400 text-xl space-y-4"] do
            div_ [] $ Utils.faIcon_ "fa-objects-column" "fa-objects-column fa-solid" "w-16 h-16"
            p_ [class_ "text-slate-500"] "Run a test to view the results here. "


collectionStepResult_ :: Testing.StepResult -> Html ()
collectionStepResult_ stepResult = div_ [role_ "tablist", class_ "tabs tabs-lifted"] do
  input_ [type_ "radio", name_ "step-result-tabs", role_ "tab", class_ "tab", Aria.label_ "Response Log", checked_]
  div_ [role_ "tabpanel", class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6"] $
    toHtmlRaw $
      textToHTML stepResult.stepLog

  input_ [type_ "radio", name_ "step-result-tabs", role_ "tab", class_ "tab", Aria.label_ "Response Headers"]
  div_ [role_ "tabpanel", class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6 "] $
    table_ [class_ "table table-xs"] do
      thead_ [] do
        tr_ [] do
          th_ [] "Name"
          th_ [] "Value"
      tbody_ $ forM_ (M.toList stepResult.request.resp.headers) $ \(k, v) -> tr_ [] do
        td_ [] $ toHtml k
        td_ [] $ toHtml $ T.intercalate "," v

  input_ [type_ "radio", name_ "step-result-tabs", role_ "tab", class_ "tab", Aria.label_ "Response Body"]
  div_ [role_ "tabpanel", class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6"] do
    pre_ [class_ "flex text-sm leading-snug w-full max-h-[50rem] overflow-y-scroll"] $ 
      code_ [class_ "h-full hljs language-json atom-one-dark w-full rounded"] $ 
        toHtmlRaw $ encodePretty stepResult.request.resp.json


textToHTML :: Text -> Text
textToHTML txt = T.intercalate (toText "<br>") (T.split (== '\n') txt)


collectionStep_ :: Html ()
collectionStep_ = do
  div_ [class_ "rounded-lg overflow-hidden border border-slate-200 group/item collectionStep"] do
    input_ [type_ "checkbox", id_ "stepState-${idx}", class_ "hidden stepState"]
    div_ [class_ "flex flex-row items-center bg-gray-50 "] do
      div_ [class_ "h-full shrink bg-gray-50 p-3 hidden border-r border-r-slate-200"] $ faIcon_ "fa-grip-dots-vertical" "fa-solid fa-grip-dots-vertical" " h-4 w-4"
      div_ [class_ "flex-1 flex flex-row items-center gap-1 bg-white pr-5 py-3"] do
        label_ [Lucid.for_ "stepState-${idx}", class_ "p-3 cursor-pointer text-xs text-slate-700"] "${idx+1}"
        label_ [Lucid.for_ "stepState-${idx}", class_ "p-3 cursor-pointer"] do
          faSprite_ "chevron-right" "solid" "h-4 w-3 group-has-[.stepState:checked]/item:rotate-90"
        div_ [class_ "w-full space-y-1 relative"] do
          div_ [class_ "absolute right-0 flex items-center gap-3 text-xs text-gray-600 hidden group-hover/item:flex"] do
            button_ [class_ ""] "View results"
            button_ [class_ "text-blue-600"] $ faIcon_ "fa-play" "fa-play fa-solid" "w-2 h-3"
            a_ [class_ "text-red-700", [__|on click remove the closest parent <.collectionStep/> |]] $ faIcon_ "fa-xmark" "fa-xmark fa-solid" "w-2 h-3"
          input_ [class_ "text-lg w-full", placeholder_ "Untitled", value_ "${stepData.title}", name_ "[${idx}][title]", id_ "title-${idx}"]
          div_ [class_ "relative flex flex-row gap-2 items-center"] do
            label_ [Lucid.for_ $ "actions-list-input-${idx}", class_ "w-28  shrink text-sm font-medium form-control "] do
              input_
                [ list_ "actions-list"
                , id_ $ "actions-list-input-${idx}"
                , class_ "input input-sm input-bordered w-full"
                , placeholder_ "method"
                , value_ "${methodAndUrl(stepData).method}"
                , termRaw "_" "on change throttled at 500ms put \\`[${idx}][\\${me.value}]\\` into #actions-data-${idx}'s @name"
                ]
            label_ [Lucid.for_ "actions-data", class_ "flex-1 text-sm font-medium form-control w-full "] do
              div_ [class_ "flex flex-row items-center gap-1"] do
                input_
                  [ type_ "text"
                  , id_ "actions-data-${idx}"
                  , class_ "input input-sm input-bordered w-full"
                  , placeholder_ "Request URI"
                  , name_ "[${idx}][${methodAndUrl(stepData).method}]"
                  , value_ "${methodAndUrl(stepData).url}"
                  ]
    div_ [class_ "border-t border-t-slate-200 space-y-3 p-3 hidden group-has-[.stepState:checked]/item:block"] do
      div_ [role_ "tablist", class_ "tabs tabs-bordered pt-1"] do
        input_ [type_ "radio", name_ "_httpOptions-${idx}", role_ "tab", class_ "tab", Aria.label_ "Params", checked_]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 space-y-2", id_ "[{idx}][params]"] do
          toHtmlRaw "${stepData.params && Object.entries(stepData.params).map(([key, value])=>litParam(key, value, null, `[${idx}][params]`))}"
          toHtmlRaw "${(!stepData.params || Object.entries(stepData.params).length==0)? litParam('', '', null, `[${idx}][params]`):'' }"

        input_ [type_ "radio", name_ "_httpOptions-${idx}", role_ "tab", class_ "tab", Aria.label_ "Headers"]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4 space-y-2", id_ "[{idx}][headers]"] do
          toHtmlRaw "${stepData.headers && Object.entries(stepData.headers).map(([key, value])=>litParam(key, value, null, `[${idx}][headers]`))}"
          toHtmlRaw "${(!stepData.headers || Object.entries(stepData.headers).length==0)? litParam('', '', null, `[${idx}][headers]`):'' }"

        input_ [type_ "radio", name_ "_httpOptions-${idx}", role_ "tab", class_ "tab", Aria.label_ "Body"]
        div_ [role_ "tabpanel", class_ "tab-content px-2 py-4"] do
          select_ [class_ "peer select select-sm select-bordered", termRaw "data-chosen" "json", onchange_ "this.dataset.chosen = this.value;"] do
            option_ [selected_ "selected"] "json"
            option_ [] "raw"
          div_ [class_ "hidden peer-data-[chosen=json]:block"] $ textarea_ [class_ "w-full border border-slate-200", name_ "[${idx}][json]"] "${stepData.json}"
          div_ [class_ "hidden peer-data-[chosen=raw]:block"] $ textarea_ [class_ "w-full border border-slate-200", name_ "[${idx}][raw]"] $ "${stepData.raw}"

      div_ [class_ ""] do
        h5_ [class_ "label-text p-1 mb-2"] "Assertions"
        div_ [class_ "text-sm space-y-2 px-2 [&_.assertIndicator]:inline-block paramRows", id_ "[{idx}][asserts]"] do
          toHtmlRaw "${stepData.asserts && stepData.asserts.map((assert, aidx)=>Object.entries(assert).map(([key, value])=>litParam(key, value, aidx, `[${idx}][asserts][${aidx}]`)))}"
          toHtmlRaw "${(!stepData.asserts || Object.entries(stepData.asserts[0]).length==0)? litParam('', '', null, `[${idx}][asserts][0]`):'' }"
      div_ [class_ ""] do
        h5_ [class_ "label-text p-1 mb-2"] "Exports"
        div_ [class_ "text-sm space-y-2 px-2 paramRows", id_ "[{idx}][exports]"] do
          toHtmlRaw "${stepData.exports && Object.entries(stepData.exports).map(([key, value])=>litParam(key, value, null, `[${idx}][exports]`))}"
          toHtmlRaw "${(!stepData.exports || Object.entries(stepData.exports).length==0)? litParam('', '', null, `[$idx][exports]`):'' }"


paramRowKV :: Html ()
paramRowKV = div_ [class_ "flex flex-row items-center gap-2 paramRow"] do
  span_ [class_ "shrink hidden assertIndicator"] $ "✅"
  input_
    [ class_ "shrink input input-xs input-bordered w-1/3"
    , placeholder_ "Key"
    , value_ "${key}"
    , termRaw "data-keyPrefix" "$keyPrefix"
    , [__|on change set :kPrefix to \`\${my @data-keyPrefix}[\${me.value}]\` then set (next <input/>)'s @name to :kPrefix  |]
    ]
  input_
    [ class_ "flex-1 input input-xs input-bordered w-full"
    , placeholder_ "Value"
    , name_ "${keyPrefix}[${key}]"
    , value_ "${value}"
    ]
  div_ [class_ "shrink flex flex-row gap-1 items-center"] do
    a_
      [ termRaw
          "_"
          [raw|on click set :nextIndex to (the closest parent <div.paramRows/>).childNodes.length 
                then set :paramTmpl to 
                        #{tmplForAddBtn}.innerHTML.replaceAll(#stepsForm's @data-defaultKeyPrefix, @data-keyPrefix).replaceAll('[aidx]', \`[\${{:nextIndex}}]\`)
                then put :paramTmpl after the closest parent <div.paramRow/>  
                then _hyperscript.processNode(#stepsForm) 
                |]
      , termRaw "data-keyPrefix" ""
      ]
      $ faSprite_ "plus" "solid" "w-3 h-3"
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
  script_ [type_ "module", src_ "/assets/steps-editor.js"] ("" :: Text)
  template_ [id_ "paramRowTmpl"] $ paramRowKV
  template_ [id_ "paramRowTmplAssert"] $ paramRowKV
  template_ [id_ "paramRowTmplFull"] $ paramRowKV
  template_ [id_ "paramRowTmplFullAssert"] $ paramRowKV
  script_ [id_ "collectionStepTmpl", type_ "module"] do
    toHtmlRaw "import {html, render} from '/assets/deps/lit/lit-html.js';"

    toHtmlRaw
      [raw|
function methodAndUrl(obj) {
    const validMethods = ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS", "TRACE", "CONNECT"];
    const validEntries = Object.entries(obj).filter(([method, url]) => validMethods.includes(method) && url !== null);
    const found = validEntries.length > 0 ? validEntries[0] : null;
    return found ? { method: found[0], url: found[1] } : {method:"", url:""};
}
    |]

    toHtmlRaw "const litParam = (key, value, aidx, keyPrefix) => html`"
    paramRowKV
    toHtmlRaw "`;"

    toHtmlRaw "const litCollections = ()=>window.collectionSteps.map((stepData, idx) =>html`"
    collectionStep_
    toHtmlRaw "`);"
    toHtmlRaw "window.renderCollection = () => render(litCollections(), document.getElementById('collectionStepsContainer'));"
    toHtmlRaw "window.renderCollection();"

  script_ [src_ "/assets/js/thirdparty/jsyaml.min.js", crossorigin_ "true"] ("" :: Text)
  script_
    [raw|
  function buildAndSetEditor(event){
   if (event.target.checked) {
      const collectionSteps = stepFormToObject();
      const yamlData = jsyaml.dump(collectionSteps, { indent: 2 });
      window.editor.setValue(yamlData)
    } else{
      window.collectionSteps = jsyaml.load(window.editor.getValue());
      window.renderCollection();
    }
  }

  function stepFormToObject(){
    const formData = new FormData(document.getElementById("stepsForm"));
    const collectionSteps = [];

    for (const [name, value] of formData.entries()) {
      if (!name.startsWith("[")) continue;
      const path = name.split(/[\[\]]+/).filter(part => part !== '')
        .map(part => isNaN(part) ? part : parseInt(part));  // Convert array indexes to integers
      if (path.length > 1 && value && value!='' && value!='null' ) {
        _.set(collectionSteps, path, value);  // Use Lodash's set function
      } 
    }
    return collectionSteps;
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
