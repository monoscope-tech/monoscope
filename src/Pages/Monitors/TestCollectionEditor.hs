module Pages.Monitors.TestCollectionEditor (collectionGetH, CollectionStepUpdateForm (..), collectionRunTestsH, collectionPage, collectionStepsUpdateH) where

import Data.Aeson qualified as AE
import Data.Aeson.Encode.Pretty
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
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import PyF
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import RustInterop (run_testkit)
import System.Config (AuthContext)
import System.Types (ATAuthCtx)
import Utils


data CollectionStepUpdateForm = CollectionStepUpdateForm
  { stepsData :: V.Vector Testing.CollectionStepData
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON, AE.ToJSON) via (DAE.CustomJSON) '[DAE.OmitNothingFields] CollectionStepUpdateForm


collectionStepsUpdateH :: Projects.ProjectId -> Testing.CollectionId -> CollectionStepUpdateForm -> ATAuthCtx (Html ())
collectionStepsUpdateH pid colId stepsForm = do
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
                input_ [type_ "checkbox", class_ "toggle editorMode"]
                span_ [class_ "text-sm"] "Code"
          div_ [class_ "h-full flex-1 overflow-y-hidden"] $ termRaw "steps-editor" [id_ "stepsEditor"] ""

        div_ [class_ "col-span-1 h-full border-r border-gray-200"] do
          div_ [class_ "max-h-full overflow-y-scroll", id_ "step-results-parent"] ""

          div_ [class_ "flex flex-col justify-center items-center h-full text-slate-400 text-xl space-y-4"] do
            div_ [] $ Utils.faIcon_ "fa-objects-column" "fa-objects-column fa-solid" "w-16 h-16"
            p_ [class_ "text-slate-500"] "Run a test to view the results here. "
    script_ [type_ "module", src_ "/assets/steps-editor.js"] ("" :: Text)


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
        toHtmlRaw $
          encodePretty stepResult.request.resp.json


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
  script_ [src_ "/assets/js/thirdparty/jsyaml.min.js", crossorigin_ "true"] ("" :: Text)
