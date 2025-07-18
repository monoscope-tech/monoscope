module Pages.Monitors.TestCollectionEditor (
  collectionGetH,
  collectionRunTestsH,
  collectionStepVariablesUpdateH,
  collectionPage,
  collectionStepsUpdateH,
  castToStepResult,
  collectionStepVariablesDeleteH,
  CollectionVariableForm (..),
  CollectionGet,
  CollectionRunTest,
  CollectionMut (..),
)
where

import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Time qualified as Time
import Log qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (
  hxDelete_,
  hxExt_,
  hxIndicator_,
  hxPost_,
  hxSwap_,
  hxTarget_,
  hxTrigger_,
  hxVals_,
 )
import Lucid.Hyperscript (__)
import Models.Projects.Projects qualified as Projects
import Models.Tests.TestToDump qualified as TestToDump
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.ItemsList qualified as Components
import PyF (fmt)
import Relude hiding (ask)
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, redirectCS)
import Utils (checkFreeTierExceeded, faSprite_, getStatusColor, insertIfNotExist, jsonValueToHtmlTree)


data CollectionVariableForm = CollectionVariableForm
  { variableName :: Text
  , variableValue :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] CollectionVariableForm


collectionStepsUpdateH :: Projects.ProjectId -> Testing.CollectionStepUpdateForm -> Maybe Text -> ATAuthCtx (RespHeaders CollectionMut)
collectionStepsUpdateH pid colF onboardingM = do
  (_, project) <- Sessions.sessionAndProject pid
  let (isValid, errMessage) = validateCollectionForm colF project.paymentPlan
      steps = project.onboardingStepsCompleted
      newStepsComp = insertIfNotExist "CreateMonitor" steps
  if not isValid
    then do
      forM_ errMessage (`addErrorToast` Nothing)
      addRespHeaders CollectionMutError
    else do
      let colIdM = colF.collectionId
      case colIdM of
        Just colId -> do
          _ <- dbtToEff $ Testing.updateCollection pid colId colF
          case onboardingM of
            Just _ -> do
              redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=NotifChannel"
              _ <- dbtToEff $ Projects.updateOnboardingStepsCompleted pid newStepsComp
              addRespHeaders CollectionMutSuccess
            Nothing -> do
              addSuccessToast "Collection's steps updated successfully" Nothing
              addRespHeaders CollectionMutSuccess
        Nothing -> do
          currentTime <- Time.currentTime
          colId <- Testing.CollectionId <$> liftIO UUIDV4.nextRandom
          let scheduleText = fromMaybe "1" colF.scheduleNumber <> " " <> fromMaybe "days" colF.scheduleNumberUnit
          let scheduleText' = if project.paymentPlan == "Free" then "1 days" else scheduleText

          let coll =
                Testing.Collection
                  { id = colId
                  , createdAt = currentTime
                  , projectId = pid
                  , updatedAt = currentTime
                  , lastRun = Nothing
                  , title = fromMaybe "[TITLE]" colF.title
                  , description = fromMaybe "" colF.description
                  , config = AE.object []
                  , schedule = scheduleText'
                  , isScheduled = True
                  , collectionSteps = Testing.CollectionSteps colF.stepsData
                  , lastRunResponse = Nothing
                  , lastRunPassed = 0
                  , lastRunFailed = 0
                  , tags = V.empty
                  , collectionVariables = Testing.CollectionVariables V.empty
                  , alertSeverity = "Info"
                  , alertMessage = ""
                  , alertSubject = ""
                  , notifyAfter = "6hours"
                  , notifyAfterCheck = False
                  , stopAfter = "0"
                  , stopAfterCheck = False
                  }
          _ <- dbtToEff $ Testing.addCollection coll
          case onboardingM of
            Just _ -> do
              _ <- dbtToEff $ Projects.updateOnboardingStepsCompleted pid newStepsComp
              redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=NotifChannel"
              addRespHeaders CollectionMutSuccess
            Nothing -> do
              addSuccessToast "Collection saved successfully" Nothing
              redirectCS $ "/p/" <> pid.toText <> "/monitors/collection/?col_id=" <> colId.toText
              addRespHeaders CollectionMutSuccess


collectionStepVariablesUpdateH :: Projects.ProjectId -> Testing.CollectionId -> CollectionVariableForm -> ATAuthCtx (RespHeaders (Html ()))
collectionStepVariablesUpdateH pid colId colF = do
  (_, project) <- Sessions.sessionAndProject pid
  coll <- dbtToEff $ Testing.getCollectionById colId
  case coll of
    Nothing -> do
      addErrorToast "Collection not found" Nothing
      addRespHeaders ""
    Just col -> do
      let Testing.CollectionVariables vars = col.collectionVariables
          vs = V.filter (\v -> v.variableName /= colF.variableName) vars
      let colVar = Testing.CollectionVariablesItem{variableName = colF.variableName, variableValue = colF.variableValue}
      let updatedVars = vs <> [colVar]
      _ <- dbtToEff $ Testing.updateCollectionVariables pid colId (Testing.CollectionVariables updatedVars)
      c <- dbtToEff $ Testing.getCollectionById colId
      addSuccessToast "Collection's variables updated successfully" Nothing
      addRespHeaders $ variablesDialog pid c


collectionStepVariablesDeleteH :: Projects.ProjectId -> Testing.CollectionId -> Text -> ATAuthCtx (RespHeaders (Html ()))
collectionStepVariablesDeleteH pid colId varName = do
  coll <- dbtToEff $ Testing.getCollectionById colId
  case coll of
    Nothing -> do
      addErrorToast "Collection not found" Nothing
      addRespHeaders ""
    Just col -> do
      let Testing.CollectionVariables vars = col.collectionVariables
          vs = V.filter (\v -> v.variableName /= varName) vars
      _ <- dbtToEff $ Testing.updateCollectionVariables pid colId (Testing.CollectionVariables vs)
      c <- dbtToEff $ Testing.getCollectionById colId
      addSuccessToast "Collection's variables updated successfully" Nothing
      addRespHeaders $ variablesDialog pid c


collectionRunTestsH :: Projects.ProjectId -> Testing.CollectionId -> Maybe Int -> Testing.CollectionStepUpdateForm -> ATAuthCtx (RespHeaders CollectionRunTest)
collectionRunTestsH pid colId runIdxM stepsForm = do
  col <- dbtToEff $ Testing.getCollectionById colId
  let Testing.CollectionVariables vars = case col of
        Just c -> c.collectionVariables
        Nothing -> Testing.CollectionVariables V.empty
  resE <- TestToDump.runCollectionTest stepsForm.stepsData vars colId
  _ <- TestToDump.logTest pid colId stepsForm.stepsData resE
  case resE of
    Right stepResults -> do
      let tkRespJson = decodeUtf8 @Text $ AE.encode stepResults
          (passed, failed) = Testing.getCollectionRunStatus stepResults
          response = AE.toJSON stepResults
      _ <- dbtToEff $ Testing.updateCollectionLastRun colId (Just response) passed failed
      addSuccessToast "Collection completed execution" Nothing
      addRespHeaders $ CollectionRunTest stepResults tkRespJson
    Left e -> do
      Log.logAttention "Collection failed execution" e
      addErrorToast "Collection failed execution" (Just $ show e)
      addRespHeaders RunTestError


castToStepResult :: AE.Value -> Maybe (V.Vector Testing.StepResult)
castToStepResult v = case AE.eitherDecodeStrictText (decodeUtf8 $ AE.encode v) of
  Right v' -> Just v'
  Left e -> Nothing


pageTabs :: Text -> Maybe Text -> Html ()
pageTabs url ov = do
  div_ [class_ "tabs tabs-box tabs-outline items-center p-0  bg-fillWeak  text-textWeak border"] do
    whenJust ov $ \v -> do
      a_ [href_ v, role_ "tab", class_ "tab"] "Overview"
    a_ [href_ url, role_ "tab", class_ "tab tab-active  text-textStrong border border-strokeStrong"] "Test editor"


collectionGetH :: Projects.ProjectId -> Maybe Testing.CollectionId -> ATAuthCtx (RespHeaders CollectionGet)
collectionGetH pid colIdM = do
  (sess, project) <- Sessions.sessionAndProject pid
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  let editorUrl = "/p/" <> pid.toText <> "/monitors/collection/" <> maybe "" (\c -> "?col_id=" <> c.toText) colIdM
  let overviewUrl = (\c -> Just $ "/p/" <> pid.toText <> "/monitors/" <> c.toText <> "/overview") =<< colIdM
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , docsLink = Just "https://apitoolkit.io/docs/dashboard/dashboard-pages/api-tests/"
          , pageTitle = "Testing"
          , prePageTitle = Just "Monitors & Alerts"
          , freeTierExceeded = freeTierExceeded
          , navTabs = Just $ pageTabs editorUrl overviewUrl
          , pageActions = Just $ div_ [class_ "inline-flex gap-2"] do
              button_ [class_ "h-8 rounded-lg btn-primary text-sm text-white px-3 flex items-center", onclick_ "htmx.trigger('#stepsForm','submit')"] do
                span_ [id_ "save-indicator", class_ "refresh-indicator px-6 htmx-indicator query-indicator loading loading-dots loading-md"] ""
                span_ [class_ "flex items-center gap-2"] do
                  faSprite_ "save" "regular" "w-4 h-4 stroke-white"
                  span_ [class_ "font-semibold"] "Save changes"
          }
  case colIdM of
    Nothing -> do
      addRespHeaders $ CollectionGet $ PageCtx bwconf (pid, Nothing, Nothing, decodeUtf8 $ AE.encode $ AE.Array [])
    Just colId -> do
      collectionM <- dbtToEff $ Testing.getCollectionById colId
      case collectionM of
        Nothing -> addRespHeaders $ CollectionNotFound $ PageCtx bwconf ()
        Just col -> do
          let runRes = col.lastRunResponse >>= castToStepResult
              respJs = decodeUtf8 $ AE.encode $ maybe [] (\v -> (\x -> AE.object ["assert_results" AE..= x.assertResults, "step_error" AE..= x.stepError]) <$> v) runRes
          addRespHeaders $ CollectionGet $ PageCtx bwconf (pid, Just col, runRes, respJs)


data CollectionGet
  = CollectionGet (PageCtx (Projects.ProjectId, Maybe Testing.Collection, Maybe (V.Vector Testing.StepResult), String))
  | CollectionNotFound (PageCtx ())


instance ToHtml CollectionGet where
  toHtml (CollectionGet (PageCtx bwconf (pid, col, cl_rn, respJsn))) = toHtml $ PageCtx bwconf $ collectionPage pid col cl_rn respJsn
  toHtml (CollectionNotFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf collectionNotFoundPage
  toHtmlRaw = toHtml


data CollectionMut = CollectionMutSuccess | CollectionMutError


instance ToHtml CollectionMut where
  toHtml CollectionMutSuccess = ""
  toHtml CollectionMutError = ""
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
    V.iforM_ results collectionStepResult_
  toHtml RunTestError = ""
  toHtmlRaw = toHtml


collectionNotFoundPage :: Html ()
collectionNotFoundPage = div_ [class_ "w-full h-full flex items-center justify-center"] $ do
  h4_ [] "Collection not found"


-- Timeline steps

timelineSteps :: Projects.ProjectId -> Maybe Testing.Collection -> Components.TimelineSteps
timelineSteps pid col =
  Components.TimelineSteps
    [ Components.TimelineStep "Define API test" $ defineTestSteps_ col
    , Components.TimelineStep "Name and tag your test" $ nameOfTest_ (maybe "" (.title) col) (maybe V.empty (.tags) col)
    , Components.TimelineStep "Set Alert Message and Recovery Threshold" $ configureNotificationMessage_ col
    ]
    col


configureNotificationMessage_ :: Maybe Testing.Collection -> Html ()
configureNotificationMessage_ colM = do
  let (severity, subject, message, naf, saf, nfc, sfc) = case colM of
        Just col -> (col.alertSeverity, col.alertSubject, col.alertMessage, col.notifyAfter, col.stopAfter, col.notifyAfterCheck, col.stopAfterCheck)
        Nothing -> ("Info", "Error: Error subject", "Alert Message", "10 minutes", "0", False, False)
  div_ [class_ "space-y-4 bg-fillWeaker p-4 rounded-2xl"] do
    div_ [class_ "p-4 bg-fillWeaker rounded-xl"] do
      div_ [class_ "flex items-center w-full gap-2"] do
        fieldset_ [class_ "fieldset"] do
          label_ [class_ "label"] $ span_ [class_ "label-text font-medium"] "Severity"
          select_ [class_ "select select-sm shadow-none w-28", name_ "alertSeverity"] do
            option_ [selected_ "" | severity == "Info"] "Info"
            option_ [selected_ "" | severity == "Warning"] "Warning"
            option_ [selected_ "" | severity == "Error"] "Error"
            option_ [selected_ "" | severity == "Critical"] "Critical"
        fieldset_ [class_ "fieldset w-full"] do
          label_ [class_ "label font-medium"] "Subject"
          input_ [placeholder_ "Error: Error subject", class_ "input shadow-none input-sm w-full", name_ "alertSubject", value_ subject]
      fieldset_ [class_ "fieldset w-full my-3"] do
        label_ [class_ "label"] "Message"
        textarea_
          [placeholder_ "Alert Message", class_ "textarea  shadow-none p-2 rounded-2xl textarea-xs w-full", name_ "alertMessage", value_ message]
          $ toHtml message
      div_ [class_ "space-y-2 py-4"] do
        h3_ [class_ "text-textWeak font-medium"] "Recovery Thresholds"
        p_ [class_ "text-sm font-medium"] "Send notifications for alert status periodically as long as the monitor has not recovered"
        div_ [class_ "flex items-center gap-2 pt-4"] do
          input_ $ [class_ "checkbox checkbox-sm", type_ "checkbox", name_ "notifyAfterCheck"] ++ [checked_ | nfc]
          span_ "If this monitor is not acknowleged or resoved, notify renotify every"
          select_ [class_ "select select-xs shadow-none", name_ "notifyAfter"]
            $ mapM_ (\v -> option_ [selected_ "" | v == naf] $ toHtml v) ["10 mins", "20 mins", "30 mins", "1 hour", "6 hours", "24 hours"]
        div_ [class_ "flex items-center gap-2"] do
          input_ $ [class_ "checkbox checkbox-sm", type_ "checkbox", name_ "stopAfterCheck"] ++ [checked_ | sfc]
          span_ "Stop renotifying after "
          input_ [type_ "number", class_ "input input-xs shadow-none w-20", value_ saf, name_ "stopAfter"]
          span_ "occurences."


nameOfTest_ :: Text -> V.Vector Text -> Html ()
nameOfTest_ name tags = do
  let tgs = decodeUtf8 $ AE.encode $ V.toList tags
  div_ [class_ "fieldset w-full p-4 bg-fillWeaker rounded-2xl"] do
    div_ [class_ "flex flex-col rounded-xl p-4 bg-fillWeaker"] do
      label_ [class_ "label text-textWeak text-sm font-semibold"] "Name"
      input_ [placeholder_ "Give your test a name", id_ "test_title", class_ "input input-sm mb-2 shadow-none w-full", name_ "title", value_ name]
      label_ [class_ "label text-textWeak text-sm font-semibold"] "Tags"
      input_ [placeholder_ "Add tags", id_ "tags_input", class_ "rounded-lg shadow-none w-full", value_ tgs]
      script_
        [text|
     document.addEventListener('DOMContentLoaded', function() {
      var inputElem = document.querySelector('#tags_input')
      var tagify = new Tagify(inputElem)
      tagify.addTags($tgs);
      window.tagify = tagify
    })
  |]


defineTestSteps_ :: Maybe Testing.Collection -> Html ()
defineTestSteps_ colM = do
  div_ [class_ "flex flex-col  notif bg-fillInformation-weak bg-opacity-60 rounded-xl relative"] do
    div_ [class_ "rounded-full absolute shadow-xs bg-bgRaised flex justify-center items-center h-5 w-5 top-1.5 right-1.5 mb-0"] do
      a_ [[__|on click remove the closest parent <.notif/>|]] $ faSprite_ "xmark" "regular" "w-2 -mt-[2px] text-textBrand"
    div_ [class_ "flex items-center gap-4 py-4 px-8"] do
      faSprite_ "circle-info" "regular" "w-5 h-5 fill-none stroke-strokeBrand-strong"
      div_ [class_ "text-sm font-medium text-textWeak"] do
        p_ [] "Link multiple steps by creating variables from the request response data."
  div_ [class_ "overflow-y-hidden flex-1 "] $ termRaw "assertion-builder" [id_ ""] ""
  div_ [class_ "overflow-y-hidden flex-1 "] $ termRaw "steps-editor" [id_ "stepsEditor"] ""


collectionPage :: Projects.ProjectId -> Maybe Testing.Collection -> Maybe (V.Vector Testing.StepResult) -> String -> Html ()
collectionPage pid colM col_rn respJson = do
  let collectionStepsJSON = AE.encode $ maybe (Testing.CollectionSteps []) (.collectionSteps) colM
  let (scheduled, scheduleNumber, scheduleNumberUnit) =
        maybe
          (True, "1", "minutes")
          ( \col -> case words col.schedule of
              [num, unit] -> (col.isScheduled, num, unit)
              _ -> (True, "1", "minutes")
          )
          colM
  script_ [fmt| window.collectionSteps = {collectionStepsJSON};|]
  editorExtraElements
  section_ [class_ "h-full overflow-y-hidden"] do
    form_
      [ id_ "stepsForm"
      , class_ "grid grid-cols-3 h-[100%] overflow-y-scroll pb-14 group/colform overflow-y-hidden"
      , hxPost_ $ "/p/" <> pid.toText <> "/monitors/collection/"
      , hxSwap_ "none"
      , hxExt_ "json-enc"
      , hxVals_ "js:{stepsData: saveStepData(), tags: getTags()}"
      , hxIndicator_ "#save-indicator"
      ]
      do
        div_ [class_ "col-span-2 px-8 pt-5 pb-12"] do
          div_ [class_ "flex items-centers justify-between mb-4"] do
            div_ [class_ "flex items-center gap-2"] do
              span_ [class_ "text-textStrong font-medium whitespace-nowrap"] "Run the test every"
              input_ [class_ "ml-3 input input-sm shadow-none w-12 text-center", type_ "number", value_ scheduleNumber, name_ "scheduleNumber"]
              select_ [class_ "select select-sm shadow-none", name_ "scheduleNumberUnit"] do
                option_ (value_ "minutes" : [selected_ "" | scheduleNumberUnit == "minutes"]) "minutes"
                option_ (value_ "hours" : [selected_ "" | scheduleNumberUnit == "hours"]) "hours"
                option_ (value_ "days" : [selected_ "" | scheduleNumberUnit == "days"]) "days"
            div_ [class_ "flex items-center gap-2"] do
              span_ [class_ "text-sm text-textWeak font-medium"] "Status"
              let extrcls = if scheduled then "bg-fillSuccess-weak text-textSuccess" else "bg-fillWeak text-textWeak"
              select_ [class_ $ "select select-sm rounded-lg shadow-none " <> extrcls, name_ "scheduled"] do
                option_ (value_ "on" : [selected_ "" | scheduled]) "Active"
                option_ (value_ "off" : [selected_ "" | not scheduled]) "Inactive"

          -- label_ [class_ "relative inline-flex items-center cursor-pointer space-x-2"] do
          --   input_ ([checked_ | scheduled] ++ [type_ "checkbox", class_ "toggle", name_ "scheduled"]) >> span_ [class_ "text-sm"] "Schedule test"

          toHtml $ timelineSteps pid colM
          whenJust colM $ \col -> do
            input_ [type_ "hidden", name_ "collectionId", value_ col.id.toText]

        div_ [class_ "fixed bg-transparent right-10 w-[25%] h-[80%] top-1/2 -translate-y-1/2", id_ "v-tabs-container"] do
          div_ [role_ "tablist", class_ "w-full h-full"] do
            div_ [class_ "w-full flex rounded-t-2xl border"] do
              button_
                [ class_ "cursor-pointer a-tab px-4 py-3 text-sm text-textWeak border-b-2 t-tab-active"
                , role_ "tab"
                , type_ "button"
                , onclick_ "navigatable(this, '#vars-t', '#v-tabs-container', 't-tab-active')"
                ]
                "Variables"
              button_
                [ class_ "cursor-pointer a-tab px-4 py-3 text-sm whitespace-nowrap text-textWeak border-b-2"
                , role_ "tab"
                , type_ "button"
                , onclick_ "navigatable(this, '#step-results-parent', '#v-tabs-container', 't-tab-active')"
                ]
                "Result Log"
              div_ [class_ "w-full border-b-2"] pass
            div_ [role_ "tabpanel", class_ "h-[calc(100%-30px)] max-h-[calc(100%-30px)] rounded-b-2xl border border-t-0 a-tab-content", id_ "vars-t"] do
              variablesDialog pid colM
            div_ [role_ "tabpanel", class_ "hidden relative space-y-4 h-[calc(100%-30px)] max-h-[calc(100%-30px)] a-tab-content rounded-b-2xl border border-t-0 overflow-y-scroll", id_ "step-results-parent"] do
              case col_rn of
                Just res -> do
                  V.iforM_ res collectionStepResult_
                Nothing -> do
                  div_ [id_ "step-results-indicator", class_ "steps-indicator flex flex-col justify-center items-center h-full text-textWeak text-xl space-y-4"] do
                    div_ [class_ "w-full flex flex-col gap-2 items-center empty-state"] do
                      Utils.faSprite_ "objects-column" "solid" "w-16 h-16"
                      p_ [class_ "text-textWeak"] "Run tests to view the results here."
                    div_ [class_ "hidden loading-indicator flex justify-center"] do
                      span_ [class_ "loading loading-dots loading-lg"] ""

        -- input_ [type_ "radio", name_ "side-tabs", role_ "tab", class_ "tab", term "aria-label" "Resources"]
        -- div_ [role_ "tabpanel", class_ "tab-content max-h-full h-full overflow-y-auto space-y-4 relative"] "Hello world"
        jsonTreeAuxillaryCode

    let res = toText respJson
    script_
      [text|
        window.collectionResults = $res;
        document.addEventListener('DOMContentLoaded', function(){{
             window.updateCollectionResults($res);
        }})
      |]


variablesDialog :: Projects.ProjectId -> Maybe Testing.Collection -> Html ()
variablesDialog pid colM = do
  div_ [class_ "w-full text-center pt-4", id_ "test-variables-content"] do
    whenJust colM $ \col -> do
      let Testing.CollectionVariables vars = col.collectionVariables
      div_ [class_ "w-full flex flex-col gap-2"] do
        forM_ vars $ \var -> do
          div_ [class_ "flex items-center w-full px-4 gap-2 text-sm relative"] do
            div_ [class_ "input text-left truncate ellipsis input-sm w-full bg-transparent"] $ toHtml var.variableName
            span_ [] "="
            div_ [class_ "input text-left truncate ellipsis input-sm w-full bg-transparent"] $ toHtml var.variableValue
            div_
              [ class_ "acursor-pointer h-5 w-5 flex justify-center items-center rounded-full bg-bgRaised shadow-sm border"
              , hxDelete_ $ "/p/" <> pid.toText <> "/monitors/" <> col.id.toText <> "/variables/" <> var.variableName
              , hxTarget_ "#test-variables-content"
              , hxSwap_ "outerHTML"
              ]
              do
                faSprite_ "trash" "regular" "w-3 h-3 stroke-strokeError-strong"
      let varsJson = decodeUtf8 $ AE.encode $ V.toList vars
      script_
        [text|
          window.testVariables  = $varsJson;
      |]
    when (isNothing colM) $ do
      div_ [class_ "w-full pt-24 text-center"] do
        h4_ [class_ "text-lg font-medium"] "Save New Test Collection To Create Local Variables"
        p_ [class_ "text-textWeak"] "Save your new test collection before you can create local variables to be used in your test steps."
    whenJust colM $ \col -> do
      let Testing.CollectionVariables vars = col.collectionVariables
      when (V.null vars) $ do
        div_ [class_ "w-full pt-24 text-center"] do
          h4_ [class_ "text-lg font-medium"] "Create Local Variables"
          p_ [class_ "text-textWeak"] "Create local variables to be used in your test steps."
      label_ [Lucid.for_ "my_modal_7", class_ "flex items-center mx-4 mt-4 gap-2"] do
        faSprite_ "plus" "solid" "w-4 h-4"
        span_ [class_ "underline  text-textWeak font-medium"] "Add new variable"
      input_ [type_ "checkbox", id_ "my_modal_7", class_ "modal-toggle"]
      div_ [class_ "modal", role_ "dialog"] $ do
        div_
          [ class_ "modal-box"
          , [__|on click halt|]
          ]
          $ do
            h3_ [class_ "text-lg font-bold text-left"] "New Variable"
            div_
              [ class_ "modal-action"
              , hxPost_ $ "/p/" <> pid.toText <> "/monitors/" <> col.id.toText <> "/variables"
              , hxTarget_ "#test-variables-content"
              , hxSwap_ "outerHTML"
              , id_ "test-variables"
              , hxTrigger_ "click from:#var-save"
              ]
              do
                fieldset_ [class_ "fieldset w-full"] do
                  label_ [class_ "label"] $ span_ [class_ "label-text"] "Name"
                  input_ [type_ "text", placeholder_ "Variable name", class_ "input input-sm w-full ", name_ "variableName", value_ ""]
                  label_ [class_ "label"] $ span_ [class_ "label-text"] "Value"
                  input_ [type_ "text", placeholder_ "Value", class_ "input input-sm w-full ", name_ "variableValue", value_ ""]
            div_ [class_ "modal-action"] do
              button_ [class_ "btn btn-sm btn-success", id_ "var-save", [__|on click halt then htmx.trigger('#test-variables','click')|]] "Save"
        label_ [class_ "modal-backdrop", Lucid.for_ "my_modal_7"] "Close"


collectionStepResult_ :: Int -> Testing.StepResult -> Html ()
collectionStepResult_ idx stepResult = section_ [class_ "p-1"] do
  when (idx == 0) $ div_ [id_ "step-results-indicator", class_ "absolute top-1/2 z-10 left-1/2 -translate-x-1/2 rounded-xs -translate-y-1/2 steps-indicator text-textWeak"] do
    div_ [class_ "hidden loading-indicator flex justify-center bg-base-100 rounded-xs shadow-xs p-4"] do
      span_ [class_ "loading loading-dots loading-lg"] ""
  div_ [class_ "p-2 bg-base-200 font-bold"] do
    toHtml $ show (idx + 1) <> " " <> fromMaybe "" stepResult.stepName
    p_ [class_ $ "block badge badge-sm " <> getStatusColor stepResult.request.resp.status, term "data-tippy-content" "status"] $ show stepResult.request.resp.status
  div_ [role_ "tablist", class_ "tabs tabs-lift"] do
    input_ [type_ "radio", name_ $ "step-result-tabs-" <> show idx, role_ "tab", class_ "tab", Aria.label_ "Response Log", checked_]
    div_ [role_ "tabpanel", class_ "tab-content bg-base-100 border-base-300 rounded-box p-6"]
      $ toHtmlRaw
      $ textToHTML stepResult.stepLog

    input_ [type_ "radio", name_ $ "step-result-tabs-" <> show idx, role_ "tab", class_ "tab", Aria.label_ "Response Headers"]
    div_ [role_ "tabpanel", class_ "tab-content bg-base-100 border-base-300 rounded-box p-6 "]
      $ table_ [class_ "table table-xs"] do
        thead_ [] $ tr_ [] $ th_ [] "Name" >> th_ [] "Value"
        tbody_ do
          whenJust stepResult.request.resp.headers $ \headers -> do
            forM_ (Map.toList headers) $ \(k, v) -> tr_ [] do
              td_ [] $ toHtml k
              td_ [] $ toHtml $ T.intercalate "," v

    input_ [type_ "radio", name_ $ "step-result-tabs-" <> show idx, role_ "tab", class_ "tab", Aria.label_ "Response Body"]
    div_ [role_ "tabpanel", id_ $ "res-container-" <> show idx, class_ "tab-content bg-base-100 bg-base-100 border-base-300 rounded-box p-6", term "data-step" (show idx)] do
      jsonValueToHtmlTree stepResult.request.resp.json Nothing
      p_ [] $ toHtml stepResult.request.resp.raw


jsonTreeAuxillaryCode :: Html ()
jsonTreeAuxillaryCode = do
  template_ [id_ "log-item-context-menu-tmpl"] do
    div_ [id_ "log-item-context-menu", class_ "log-item-context-menu  origin-top-right absolute left-0 mt-2 rounded-md shadow-md shadow-slate-300 bg-base-100 ring-1 ring-black ring-opacity-5 divide-y divide-gray-100 focus:outline-hidden z-10", role_ "menu", tabindex_ "-1"] do
      div_ [class_ "py-1 w-max", role_ "none"] do
        button_
          [ class_ "cursor-pointer w-full text-left text-textStrong block px-4 py-1  hover:bg-fillWeak hover:text-textStrong"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-1"
          , onclick_ "addToAssertions(event, 'ok', '==')"
          , type_ "button"
          ]
          "Add an equals to assertion"
        button_
          [ class_ "cursor-pointer w-full text-left text-textStrong block px-4 py-1  hover:bg-fillWeak hover:text-textStrong"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-2"
          , onclick_ "addToAssertions(event, 'ok', '!=')"
          , type_ "button"
          ]
          "Add a not equals assertion"
        button_
          [ class_ "cursor-pointer w-full text-left text-textStrong block px-4 py-1  hover:bg-fillWeak hover:text-textStrong"
          , role_ "menuitem"
          , tabindex_ "-1"
          , onclick_ "addToAssertions(event, 'ok', '>')"
          , type_ "button"
          ]
          "Add a greater than assertions"
        button_
          [ class_ "cursor-pointer w-full text-left text-textStrong block px-4 py-1  hover:bg-fillWeak hover:text-textStrong"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-4"
          , onclick_ "addToAssertions(event, 'string')"
          , type_ "button"
          ]
          "Add an is string assertions"

        button_
          [ class_ "cursor-pointer w-full text-left text-textStrong block px-4 py-1  hover:bg-fillWeak hover:text-textStrong"
          , role_ "menuitem"
          , tabindex_ "-1"
          , id_ "menu-item-4"
          , onclick_ "addToAssertions(event, 'number')"
          , type_ "button"
          ]
          "Add an is number assertions"


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


validateCollectionForm :: Testing.CollectionStepUpdateForm -> Text -> (Bool, [Text])
validateCollectionForm colF paymentPlan =
  let
    isValidScheduleForPlan = not (paymentPlan == "Free" && isJust colF.scheduleNumberUnit && colF.scheduleNumberUnit /= Just "days")
    isTitleValid = isJust colF.title && colF.title /= Just ""
    isScheduleNumberValid =
      case colF.scheduleNumber of
        Just sn -> case readMaybe (toString sn) :: Maybe Int of
          Just n -> n > 0
          Nothing -> False
        Nothing -> True
    errorMessages =
      ["Title should not be empty" | not isTitleValid]
        ++ ["Schedule number should not be less than one" | not isScheduleNumberValid]
        ++ ["You're on the free plan, you can't schedule a test to run more than once a day" | not isValidScheduleForPlan]
    isValid = isTitleValid && isScheduleNumberValid && isValidScheduleForPlan
   in
    (isValid, errorMessages)
