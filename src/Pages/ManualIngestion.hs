module Pages.ManualIngestion (RequestMessageForm (..), manualIngestGetH, manualIngestPostH) where

import Colog.Core ((<&))
import Config
import Data.Aeson.QQ
import Data.Text.Encoding.Base64 qualified as B64
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.HTMX
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (bodyWrapper)
import ProcessMessage qualified
import Relude
import RequestMessages qualified
import Text.RawString.QQ (r)
import Web.FormUrlEncoded (FromForm)

data RequestMessageForm = RequestMessageForm
  { timestamp :: ZonedTime,
    host :: Text,
    method :: Text,
    referer :: Text,
    urlPath :: Text,
    protoMajor :: Int,
    protoMinor :: Int,
    duration :: Int,
    requestHeaders :: Text,
    responseHeaders :: Text,
    requestBody :: Text,
    responseBody :: Text,
    statusCode :: Int
  }
  deriving (Show, Generic)
  deriving anyclass (FromForm)

reqMsgFormToReqMsg :: UUID.UUID -> RequestMessageForm -> RequestMessages.RequestMessage
reqMsgFormToReqMsg pid RequestMessageForm {..} =
  RequestMessages.RequestMessage
    { projectId = pid,
      requestHeaders = [aesonQQ|{}|],
      responseHeaders = [aesonQQ|{}|],
      requestBody = B64.encodeBase64 requestBody,
      responseBody = B64.encodeBase64 responseBody,
      ..
    }

-- TODO:
-- - [x] Parse the time to the ZonedTime from string or default to current time.
-- - [x] use one of duration or durationMicroSecs and derive one from the other, or even get read of 1 of them from the expected input. We can always calculate one from the other
-- - [x] Base64 encode requst and response Bodies before processing
-- - [x] Process and persist the request as usual
-- - implement support for processing and persisting the request and response headers
-- - include a datalist of http methods
-- - include a data list of http status codes for the status codes
-- - include a data list of all endpoints in the service in the endpoints field
-- - timestamp should default to the current data in the browser form except preloaded
-- - preload the form wiht the data from the last submitted and persisted request dump from the database.
-- - document how to access this page from within the github readme.
-- - [FUTURE] If we ever have a super admin, it should be possible to access this page directly from there for any given company.
manualIngestPostH :: Sessions.PersistentSession -> Projects.ProjectId -> RequestMessageForm -> DashboardM (Html ())
manualIngestPostH sess pid reqM = do
  traceShowM reqM
  logger <- asks logger
  liftIO $ logger <& show reqM
  pool <- asks pool
  project <-
    liftIO $
      withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)

  liftIO $ ProcessMessage.processRequestMessage logger pool $ reqMsgFormToReqMsg (Projects.unProjectId pid) reqM

  pure manualIngestPage

manualIngestGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
manualIngestGetH sess pid = do
  pool <- asks pool
  project <-
    liftIO $
      withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)

  pure $ bodyWrapper (Just sess) project "Manual Ingest" manualIngestPage

manualIngestPage :: Html ()
manualIngestPage = do
  section_ [id_ "mainContent"] $ do
    script_
      [ src_ "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/9.7.2/jsoneditor.min.js",
        integrity_ "sha512-9T9AIzkTI9pg694MCTReaZ0vOimxuTKXA15Gin+AZ4eycmg85iEXGX811BAjyY+NOcDCdlA9k2u9SqVAyNqFkQ==",
        crossorigin_ "anonymous"
      ]
      ""
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/9.7.2/jsoneditor.min.css"]
    section_ [class_ "container mx-auto  px-4 py-10"] $ do
      div_ [class_ "flex justify-between mb-6"] $ do
        h2_ [class_ "text-slate-700 text-2xl font-medium"] "API Keys"
      form_
        [ class_ "relative space-y-10 px-10 border border-gray-200 py-10  bg-white w-3/4 rounded-3xl",
          hxTarget_ "#mainContent",
          hxSwap_ "outerHTML",
          hxPost_ "",
          hxVals_
            [r|js: requestBody:reqBodyEditor.getText(), 
                       responseBody: respBodyEditor.getText(),
                       timestamp: (new Date(document.getElementById('timestamp').value)).toISOString()|]
        ]
        $ do
          inputDatetime "timestamp"
          inputText "" "host"
          inputText "" "method"
          inputText "" "referer"
          inputText "" "urlPath"
          inputInt "" "protoMajor"
          inputInt "" "protoMinor"
          inputInt "duration in nanoseconds (1ms -> 1,000,000 ns)" "duration"
          inputText "" "requestHeaders"
          inputText "" "responseHeaders"
          inputTextArea "requestBody"
          inputTextArea "responseBody"
          inputInt "" "statusCode"
          div_ $ do
            button_ [class_ "btn-sm btn-indigo", type_ "submit"] "Submit"
    script_
      []
      [r|
        // create the editor
        var opt = {mode:"code", modes: ["code","tree"]}
        var reqBodyEditor = new JSONEditor(document.getElementById("requestBody"), opt)
        var respBodyEditor = new JSONEditor(document.getElementById("responseBody"), opt)

        // set json
        var initialJson = {
            "Array": [1, 2, 3],
            "Boolean": true,
            "Null": null,
            "Number": 123,
            "Object": {"a": "b", "c": "d"},
            "String": "Hello World"
        }
        reqBodyEditor.set(initialJson)
        respBodyEditor.set(initialJson)
    |]

inputText :: Text -> Text -> Html ()
inputText title name = do
  div_ $ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  ",
        type_ "text",
        id_ name,
        name_ name
      ]

inputTextArea :: Text -> Html ()
inputTextArea name = do
  div_ $ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml name
    div_
      [ class_ "w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl",
        id_ name,
        name_ name
      ]
      ""

inputDatetime :: Text -> Html ()
inputDatetime name = do
  div_ $ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml name
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  ",
        type_ "datetime-local",
        id_ name,
        name_ name,
        pattern_ "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}"
      ]

inputInt :: Text -> Text -> Html ()
inputInt title name = do
  div_ $ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  ",
        type_ "number",
        id_ name,
        name_ name
      ]
