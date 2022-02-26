module Pages.ManualIngestion (RequestMessageForm (..), manualIngestGetH, manualIngestPostH) where

import Colog.Core ((<&))
import Config
import Data.Aeson (Value, eitherDecodeStrict)
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
import Utils
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

reqMsgFormToReqMsg :: UUID.UUID -> RequestMessageForm -> Either Text RequestMessages.RequestMessage
reqMsgFormToReqMsg pid RequestMessageForm {..} = do
  reqHeaders <- eitherStrToText $ eitherDecodeStrict (encodeUtf8 @Text @ByteString requestHeaders) :: Either Text Value
  respHeaders <- eitherStrToText $ eitherDecodeStrict (encodeUtf8 @Text @ByteString responseHeaders) :: Either Text Value
  Right
    RequestMessages.RequestMessage
      { projectId = pid,
        requestHeaders = reqHeaders,
        responseHeaders = respHeaders,
        requestBody = B64.encodeBase64 requestBody,
        responseBody = B64.encodeBase64 responseBody,
        ..
      }

-- TODO:
-- - [x] Parse the time to the ZonedTime from string or default to current time.
-- - [x] use one of duration or durationMicroSecs and derive one from the other, or even get read of 1 of them from the expected input. We can always calculate one from the other
-- - [x] Base64 encode requst and response Bodies before processing
-- - [x] Process and persist the request as usual
-- - [~] implement support for processing and persisting the request and response headers
-- - - Partial support until we support header hashes in both the db and the codebase
-- - implement support for process request params. Figure out how we should accept the request params from the clients: raw path, path, map of params separately (vs processing string and building map server side)
-- - [x] include a datalist of http methods
-- - include a data list of http status codes for the status codes
-- - include a data list of all endpoints in the service in the endpoints field
-- - timestamp should default to the current data in the browser form except preloaded
-- - preload the form wiht the data from the last submitted and persisted request dump from the database.
-- - document how to access this page from within the github readme.
-- - [FUTURE] If we ever have a super admin, it should be possible to access this page directly from there for any given company.
manualIngestPostH :: Sessions.PersistentSession -> Projects.ProjectId -> RequestMessageForm -> DashboardM (Html ())
manualIngestPostH sess pid reqMF = do
  logger <- asks logger
  liftIO $ logger <& "manualIngestPost RequestMessageForm <> " <> show reqMF
  pool <- asks pool
  project <-
    liftIO $
      withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)
  case reqMsgFormToReqMsg (Projects.unProjectId pid) reqMF of
    Left err -> liftIO $ logger <& "error parsing manualIngestPost req Message; " <> show err
    Right reqM -> liftIO $ ProcessMessage.processRequestMessage logger pool reqM

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
                   requestHeaders: reqHeadersEditor.getText(),
                   responseHeaders: respHeadersEditor.getText(),
                   timestamp: (new Date(document.getElementById('timestamp').value)).toISOString()|]
        ]
        $ do
          inputDatetime "timestamp"
          inputText "" "host"
          inputTextDatalist "" "method" ["GET", "POST", "PUT", "DELETE", "OPTION", "HEAD"]
          inputText "" "referer"
          inputText "" "urlPath"
          inputInt "" "protoMajor"
          inputInt "" "protoMinor"
          inputInt "duration in nanoseconds (1ms -> 1,000,000 ns)" "duration"
          inputTextArea "requestHeaders as a key value json pair" "requestHeaders"
          inputTextArea "responseHeaders as a key value json pair" "responseHeaders"
          inputTextArea "" "requestBody"
          inputTextArea "" "responseBody"
          inputInt "" "statusCode"
          div_ $ do
            button_ [class_ "btn-sm btn-indigo", type_ "submit"] "Submit"
    script_
      []
      [r|
        // create the editor
        var opt = {mode:"code", modes: ["code","tree"]}
        var reqHeadersEditor = new JSONEditor(document.getElementById("requestHeaders"), opt)
        var respHeadersEditor = new JSONEditor(document.getElementById("responseHeaders"), opt)
        var reqBodyEditor = new JSONEditor(document.getElementById("requestBody"), opt)
        var respBodyEditor = new JSONEditor(document.getElementById("responseBody"), opt)

        var initialJson = {
            "Content-Type": ["application/json"],
        }
        reqHeadersEditor.set(initialJSON)
        respHeadersEditor.set(initialJSON)

        // set json
        var initialJsonB = {
            "Array": [1, 2, 3],
            "Boolean": true,
            "Null": null,
            "Number": 123,
            "Object": {"a": "b", "c": "d"},
            "String": "Hello World"
        }
        reqBodyEditor.set(initialJsonB)
        respBodyEditor.set(initialJsonB)
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

inputTextDatalist :: Text -> Text -> [Text] -> Html ()
inputTextDatalist title name datalist = do
  div_ $ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  ",
        type_ "text",
        id_ name,
        name_ name,
        list_ $ name <> "-list"
      ]
    datalist_ [id_ $ name <> "-list"] $ do
      datalist & mapM_ (\it -> option_ [value_ it] $ toHtml it)

inputTextArea :: Text -> Text -> Html ()
inputTextArea title name = do
  div_ $ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
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

httpStatusCodes :: [(Text, Text)]
httpStatusCodes =
  [ ("100", "Continue"),
    ("101", "Switching Protocols"),
    ("103", "Early Hints"),
    ("200", "OK"),
    ("201", "Created"),
    ("202", "Accepted"),
    ("203", "Non-Authoritative Information"),
    ("204", "No Content"),
    ("205", "Reset Content"),
    ("206", "Partial Content"),
    ("300", "Multiple Choices"),
    ("301", "Moved Permanently"),
    ("302", "Found"),
    ("303", "See Other"),
    ("304", "Not Modified"),
    ("307", "Temporary Redirect"),
    ("308", "Permanent Redirect"),
    ("400", "Bad Request"),
    ("401", "Unauthorized"),
    ("402", "Payment Required"),
    ("403", "Forbidden"),
    ("404", "Not Found"),
    ("405", "Method Not Allowed"),
    ("406", "Not Acceptable"),
    ("407", "Proxy Authentication Required"),
    ("408", "Request Timeout"),
    ("409", "Conflict"),
    ("410", "Gone"),
    ("411", "Length Required"),
    ("412", "Precondition Failed"),
    ("413", "Payload Too Large"),
    ("414", "URI Too Long"),
    ("415", "Unsupported Media Type"),
    ("416", "Range Not Satisfiable"),
    ("417", "Expectation Failed"),
    ("418", "I'm a teapot"),
    ("422", "Unprocessable Entity"),
    ("425", "Too Early"),
    ("426", "Upgrade Required"),
    ("428", "Precondition Required"),
    ("429", "Too Many Requests"),
    ("431", "Request Header Fields Too Large"),
    ("451", "Unavailable For Legal Reasons"),
    ("500", "Internal Server Error"),
    ("501", "Not Implemented"),
    ("502", "Bad Gateway"),
    ("503", "Service Unavailable"),
    ("504", "Gateway Timeout"),
    ("505", "HTTP Version Not Supported"),
    ("506", "Variant Also Negotiates"),
    ("507", "Insufficient Storage"),
    ("508", "Loop Detected"),
    ("510", "Not Extended"),
    ("511", "Network Authentication Required")
  ]
