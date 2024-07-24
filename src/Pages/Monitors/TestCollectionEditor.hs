module Pages.Monitors.TestCollectionEditor (
  collectionGetH,
  CollectionStepUpdateForm (..),
  collectionRunTestsH,
  collectionPage,
  collectionStepsUpdateH,
  testSettingsModalContent_,
  CollectionGet,
  CollectionRunTest,
  CollectionMut (..),
) where

import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Default (def)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Log qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (
  hxExt_,
  hxIndicator_,
  hxParams_,
  hxPatch_,
  hxPost_,
  hxSwap_,
  hxTarget_,
  hxVals_,
 )
import Models.Projects.Projects qualified as Projects
import Models.Tests.TestToDump qualified as TestToDump
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.LogExplorer.LogItem (jsonValueToHtmlTree)
import Pkg.Components.Modals qualified as Components
import PyF (fmt)
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (faSprite_)


data CollectionStepUpdateForm = CollectionStepUpdateForm
  { stepsData :: V.Vector Testing.CollectionStepData
  , title :: Maybe Text
  , description :: Maybe Text
  , scheduled :: Maybe Text
  , scheduleNumber :: Maybe Text
  , scheduleNumberUnit :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via (DAE.CustomJSON) '[DAE.OmitNothingFields] CollectionStepUpdateForm


collectionStepsUpdateH :: Projects.ProjectId -> Testing.CollectionId -> CollectionStepUpdateForm -> ATAuthCtx (RespHeaders (CollectionMut))
collectionStepsUpdateH pid colId colF = do
  let isScheduled = colF.scheduled == Just "on"
  _ <- dbtToEff $ Testing.updateCollection pid colId (fromMaybe "" colF.title) (fromMaybe "" colF.description) isScheduled ((fromMaybe "" colF.scheduleNumber) <> " " <> fromMaybe "" colF.scheduleNumberUnit) colF.stepsData
  addSuccessToast "Collection's steps updated successfully" Nothing
  addRespHeaders $ CollectionMut


collectionRunTestsH :: Projects.ProjectId -> Testing.CollectionId -> Maybe Int -> CollectionStepUpdateForm -> ATAuthCtx (RespHeaders (CollectionRunTest))
collectionRunTestsH pid colId runIdxM stepsForm = do
  stepResultsE <- TestToDump.runTestAndLog pid stepsForm.stepsData
  case stepResultsE of
    Right stepResults -> do
      let tkRespJson = decodeUtf8 @Text $ AE.encode stepResults
      addSuccessToast "Collection completed execution" Nothing
      addRespHeaders $ CollectionRunTest stepResults tkRespJson
    Left e -> do
      Log.logAttention "Collection failed execution" e
      addErrorToast "Collection failed execution" (Just $ show e)
      addRespHeaders $ RunTestError


collectionGetH :: Projects.ProjectId -> Testing.CollectionId -> ATAuthCtx (RespHeaders (CollectionGet))
collectionGetH pid colId = do
  (sess, project) <- Sessions.sessionAndProject pid
  collectionM <- dbtToEff $ Testing.getCollectionById colId
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession
          , currProject = Just project
          , pageTitle = "Testing"
          }
  case collectionM of
    Nothing -> addRespHeaders $ CollectionNotFound $ PageCtx bwconf ()
    Just col -> addRespHeaders $ CollectionGet $ PageCtx bwconf (pid, col)


data CollectionGet
  = CollectionGet (PageCtx (Projects.ProjectId, Testing.Collection))
  | CollectionNotFound (PageCtx ())


instance ToHtml CollectionGet where
  toHtml (CollectionGet (PageCtx bwconf (pid, col))) = toHtml $ PageCtx bwconf $ collectionPage pid col
  toHtml (CollectionNotFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf $ collectionNotFoundPage
  toHtmlRaw = toHtml


data CollectionMut = CollectionMut


instance ToHtml CollectionMut where
  toHtml (CollectionMut) = ""
  toHtmlRaw = toHtml


data CollectionRunTest
  = CollectionRunTest (V.Vector Testing.StepResult) Text
  | RunTestError


instance ToHtml CollectionRunTest where
  toHtml (CollectionRunTest results tkjson) = toHtml $ do
    script_
      [fmt|
        window.collectionResults = {tkjson}; 
        window.updateCollectionResults({tkjson});
    |]
    V.iforM_ results (\i r -> collectionStepResult_ i r)
  toHtml RunTestError = ""
  toHtmlRaw = toHtml


collectionNotFoundPage :: Html ()
collectionNotFoundPage = div_ [class_ "w-full h-full flex items-center justify-center"] $ do
  h4_ [] "Collection not found"


testSettingsModalContent_ :: Bool -> Testing.Collection -> Html ()
testSettingsModalContent_ isUpdate col = div_ [class_ "space-y-5 w-96"] do
  div_ $ h3_ if isUpdate then "Update Test settings" else "New Text/Monitor"
  label_ [class_ "form-control w-full"] do
    div_ [class_ "label"] $ span_ [class_ "label-text"] "Test title"
    input_ [type_ "text", placeholder_ "Type here", class_ "input input-bordered w-full ", name_ "title", value_ col.title]
  label_ [class_ "form-control w-full"] do
    div_ [class_ "label"] $ span_ [class_ "label-text"] "Test Description"
    textarea_ [type_ "text", placeholder_ "Type here", class_ "textarea textarea-bordered w-full h-24", name_ "description"] $ toHtml col.description
  div_ [class_ "space-y-2 group/schedule"] do
    div_ [class_ "form-control w-full"] $ label_ [class_ "label cursor-pointer"] do
      span_ [class_ "label-text"] "Schedule"
      input_ $ [type_ "checkbox", class_ "toggle", name_ "scheduled"] <> [checked_ | col.isScheduled]
    label_ [class_ "form-control w-full hidden group-has-[.toggle:checked]/schedule:block"] do
      div_ [class_ "label"] $ span_ [class_ "label-text"] "run test every:"
      div_ [class_ "join"] do
        let (scheduleNumber, scheduleNumberUnit) = case (words col.schedule) of
              [num, unit] -> (num, unit)
              _ -> ("1", "day")
        input_ [class_ "input input-bordered join-item", type_ "number", name_ "scheduleNumber", value_ $ scheduleNumber]
        select_ [class_ "select select-bordered join-item", name_ "scheduleNumberUnit"] do
          option_ [selected_ "" | scheduleNumberUnit == "minutes"] "Minutes"
          option_ [selected_ "" | scheduleNumberUnit == "hours"] "Hours"
          option_ [selected_ "" | scheduleNumberUnit == "days"] "Days"
  div_ $ button_ [class_ "btn btn-bordered btn-primary", type_ "submit"] $ if isUpdate then "Update" else "Create Test"


collectionPage :: Projects.ProjectId -> Testing.Collection -> Html ()
collectionPage pid col = do
  let collectionStepsJSON = AE.encode col.collectionSteps
  script_ [] [fmt|window.collectionSteps = {collectionStepsJSON};|]
  editorExtraElements
  section_ [class_ "h-full overflow-y-hidden"] do
    form_
      [ id_ "stepsForm"
      , class_ "grid grid-cols-2 h-full divide-x divide-gray-200 group/colform overflow-y-hidden"
      , hxPost_ ""
      , hxSwap_ "none"
      , hxExt_ "json-enc"
      , hxVals_ "js:{stepsData: document.getElementById('stepsEditor').collectionSteps}"
      ]
      do
        div_ [class_ "col-span-1 h-full divide-y flex flex-col overflow-y-hidden"] do
          div_ [class_ "shrink flex items-center justify-between"] do
            div_ [class_ " pb-5 p-5 space-y-2"] do
              h2_ [class_ "text-base font-semibold leading-6 text-gray-900 flex items-end"] do
                toHtml col.title
                small_ [class_ "inline-block ml-2 truncate text-sm text-gray-500"] "created  2024/01/23"
              p_ [class_ "text-sm"] $ toHtml col.description
            div_ [class_ ""] do
              span_ [class_ "badge badge-success"] "Active"
              div_ [class_ "inline-block"] $ Components.modal_ "test-settings-modal" (span_ [class_ "p-3"] $ Utils.faSprite_ "sliders" "regular" "h-4") $ testSettingsModalContent_ True col
          div_ [class_ "shrink p-4 flex justify-between items-center"] do
            div_ [class_ "flex items-center space-x-4"] do
              h4_ [class_ "font-semibold text-2xl font-medium "] "Steps"
              a_ [href_ "https://apitoolkit.io/docs/dashboard/dashboard-pages/api-tests/", target_ "_blank", class_ "text-sm flex items-center gap-1 text-blue-500"] do
                faSprite_ "link-simple" "regular" "w-4 h-4" >> "Docs"
            div_ [class_ "space-x-4 flex items-center"] do
              button_
                [ class_ "btn btn-sm btn-success"
                , hxPatch_ ""
                , hxParams_ "stepsData"
                , hxExt_ "json-enc"
                , hxVals_ "js:{stepsData: document.getElementById('stepsEditor').collectionSteps}"
                , hxTarget_ "#step-results-parent"
                , hxSwap_ "innerHTML"
                , hxIndicator_ "#step-results-indicator"
                ]
                (span_ "Run all" >> faSprite_ "play" "solid" "w-3 h-3")
              button_ [class_ "btn btn-sm btn-warning ", type_ "submit"] (span_ "Save" >> faSprite_ "floppy-disk" "solid" "w-3 h-3")
              label_ [class_ "relative inline-flex items-center cursor-pointer space-x-2"] do
                input_ [type_ "checkbox", class_ "toggle editormode"] >> span_ [class_ "text-sm"] "Code"
          div_ [class_ "h-full flex-1 overflow-y-hidden"] $ termRaw "steps-editor" [id_ "stepsEditor"] ""

        div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
          div_ [class_ "max-h-full h-full overflow-y-auto space-y-4 relative", id_ "step-results-parent"] do
            div_ [id_ "step-results-indicator", class_ "steps-indicator flex flex-col justify-center items-center h-full text-slate-400 text-xl space-y-4"] do
              div_ [class_ "w-full flex flex-col gap-2 items-center empty-state"] do
                Utils.faSprite_ "objects-column" "solid" "w-16 h-16"
                p_ [class_ "text-slate-500"] "Run tests to view the results here."
              div_ [class_ "hidden loading-indicator flex justify-center"] do
                span_ [class_ "loading loading-dots loading-lg"] ""

    script_ [type_ "module", src_ "/assets/steps-editor.js"] ("" :: Text)


collectionStepResult_ :: Int -> Testing.StepResult -> Html ()
collectionStepResult_ idx stepResult = section_ [class_ "p-1"] do
  when (idx == 1) $ div_ [id_ "step-results-indicator", class_ "absolute top-1/2 z-10 left-1/2 -translate-x-1/2 rounded-sm -translate-y-1/2 steps-indicator text-slate-400"] do
    div_ [class_ "hidden loading-indicator flex justify-center bg-white rounded-sm shadow-sm p-4"] do
      span_ [class_ "loading loading-dots loading-lg"] ""
  div_ [class_ "p-2 bg-base-200 font-bold"] do
    toHtml $ (show $ idx + 1) <> " " <> fromMaybe "" stepResult.stepName
  div_ [role_ "tablist", class_ "tabs tabs-lifted"] do
    input_ [type_ "radio", name_ $ "step-result-tabs-" <> show idx, role_ "tab", class_ "tab", Aria.label_ "Response Log", checked_]
    div_ [role_ "tabpanel", class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6"]
      $ toHtmlRaw
      $ textToHTML stepResult.stepLog

    input_ [type_ "radio", name_ $ "step-result-tabs-" <> show idx, role_ "tab", class_ "tab", Aria.label_ "Response Headers"]
    div_ [role_ "tabpanel", class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6 "]
      $ table_ [class_ "table table-xs"] do
        thead_ [] $ tr_ [] $ th_ [] "Name" >> th_ [] "Value"
        tbody_ $ forM_ (M.toList stepResult.request.resp.headers) $ \(k, v) -> tr_ [] do
          td_ [] $ toHtml k
          td_ [] $ toHtml $ T.intercalate "," v

    input_ [type_ "radio", name_ $ "step-result-tabs-" <> show idx, role_ "tab", class_ "tab", Aria.label_ "Response Body"]
    div_ [role_ "tabpanel", class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6"] do
      jsonValueToHtmlTree stepResult.request.resp.json
    jsonTreeAuxillaryCode idx
    let stepIndex = show idx
    script_
      [text|
        function addToAssertions(event) {
            const step = $stepIndex
            const target = event.target.parentNode.parentNode.parentNode
            const path = target.getAttribute('data-field-path');
            const value = target.getAttribute('data-field-value');
            const assertion = "$.resp.json." + path + ' == ' + value;
            console.log(assertion)
            window.updateStepAssertions(assertion, step - 1);
        }
    |]


jsonTreeAuxillaryCode :: Int -> Html ()
jsonTreeAuxillaryCode idx = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu text-sm origin-top-right absolute left-0 mt-2 w-56 rounded-md shadow-md shadow-slate-300 bg-white ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-none z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1", role_ "none"] do
        button_
          [ class_ "cursor-pointer w-full text-left text-slate-700 block px-4 py-1 text-sm hover:bg-gray-100 hover:text-slate-900"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-2"
          , onclick_ "addToAssertions(event)"
          , type_ "button"
          ]
          "Add to assertions"


textToHTML :: Text -> Text
textToHTML txt = T.intercalate (toText "<br>") (T.split (== '\n') txt)


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
  datalist_ [id_ "assertsDataList"] do
    option_ [value_ "ok"] ""
    option_ [value_ "array"] ""
    option_ [value_ "empty"] ""
    option_ [value_ "string"] ""
    option_ [value_ "number"] ""
    option_ [value_ "boolean"] ""
    option_ [value_ "null"] ""
    option_ [value_ "exists"] ""
    option_ [value_ "date"] ""
    option_ [value_ "notEmpty"] ""
  script_ [src_ "/assets/js/thirdparty/jsyaml.min.js", crossorigin_ "true"] ("" :: Text)
