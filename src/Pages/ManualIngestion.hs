module Pages.ManualIngestion (RequestMessageForm (..), manualIngestGetH, manualIngestPostH) where

import Data.Default (def)
import Data.Time (ZonedTime)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid (
  Html,
  ToHtml (toHtml),
  button_,
  class_,
  crossorigin_,
  datalist_,
  div_,
  form_,
  h2_,
  href_,
  id_,
  input_,
  integrity_,
  label_,
  lang_,
  link_,
  list_,
  name_,
  option_,
  pattern_,
  rel_,
  script_,
  section_,
  src_,
  type_,
  value_,
 )
import Lucid.Htmx (hxPost_, hxSwap_, hxTarget_, hxVals_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.NonMember (userNotMemeberPage)
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import System.Types (ATAuthCtx)
import System.Config (AuthContext)
import Utils ( userIsProjectMember)
import Web.FormUrlEncoded (FromForm)
import NeatInterpolation (text)


data RequestMessageForm = RequestMessageForm
  { timestamp :: ZonedTime
  , host :: Text
  , method :: Text
  , referer :: Text
  , urlPath :: Text
  , pathParams :: Text
  , protoMajor :: Int
  , protoMinor :: Int
  , duration :: Int
  , requestHeaders :: Text
  , responseHeaders :: Text
  , queryParams :: Text
  , requestBody :: Text
  , responseBody :: Text
  , statusCode :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)



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
-- - [x] document how to access this page from within the github readme.
-- - [FUTURE] If we ever have a super admin, it should be possible to access this page directly from there for any given company.
manualIngestPostH :: Projects.ProjectId -> RequestMessageForm -> ATAuthCtx (Html ())
manualIngestPostH pid reqMF = do
  pure ""


-- FIXME: commented out because processMessage is in a different monad from manualIngest, and this method is not used much either
-- -- TODO: temporary, to work with current logic
-- appCtx <- ask @AuthContext
-- let env = appCtx.config
-- sess' <- Sessions.getSession
-- let sess = Unsafe.fromJust sess'.persistentSession
-- let currUserId = sess.userId

-- logger <- asks logger
-- isMember <- dbtToEff $ userIsProjectMember sess pid
-- if not isMember
--   then do
--     pure $ userNotMemeberPage sess
--   else do
--     projectCache <- asks projectCache
--     project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)
--     case reqMsgFormToReqMsg (Projects.unProjectId pid) reqMF of
--       Left err -> liftIO $ logger <& "error parsing manualIngestPost req Message; " <> show err
--       Right reqM -> void $ liftIO $ ProcessMessage.processMessages' logger env appCtx.pool [(Just "", reqM)] projectCache

--     pure manualIngestPage

manualIngestGetH :: Projects.ProjectId -> ATAuthCtx (Html ())
manualIngestGetH pid = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)

      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "ManualIngest"
              }
      pure $ bodyWrapper bwconf manualIngestPage


manualIngestPage :: Html ()
manualIngestPage = do
  section_ [id_ "mainContent", class_ "h-full overflow-scroll"] do
    script_
      [ src_ "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/9.7.2/jsoneditor.min.js"
      , integrity_ "sha512-9T9AIzkTI9pg694MCTReaZ0vOimxuTKXA15Gin+AZ4eycmg85iEXGX811BAjyY+NOcDCdlA9k2u9SqVAyNqFkQ=="
      , crossorigin_ "anonymous"
      ]
      ("" :: Text)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/9.7.2/jsoneditor.min.css"]
    section_ [class_ "container mx-auto  px-4 py-10"] do
      div_ [class_ "flex justify-between mb-6"] do
        h2_ [class_ "text-slate-700 text-2xl font-medium"] "API Keys"
      form_
        [ class_ "relative space-y-10 px-10 border border-gray-200 py-10  bg-white w-3/4 rounded-3xl"
        , hxTarget_ "#mainContent"
        , hxSwap_ "outerHTML"
        , hxPost_ ""
        , hxVals_
            [text|js: requestBody:reqBodyEditor.getText(), 
                   responseBody: respBodyEditor.getText(),
                   queryParams: queryParamsEditor.getText(),
                   pathParams: pathParamsEditor.getText(),
                   requestHeaders: reqHeadersEditor.getText(),
                   responseHeaders: respHeadersEditor.getText(),
                   timestamp: (new Date(document.getElementById('timestamp').value)).toISOString()|]
        ]
        do
          inputDatetime "timestamp"
          inputText "" "host"
          inputTextDatalist "" "method" ["GET", "POST", "PUT", "DELETE", "OPTION", "HEAD"]
          inputText "" "referer"
          inputText "" "urlPath"
          inputInt "" "protoMajor" 2
          inputInt "" "protoMinor" 2
          inputInt "duration in nanoseconds (1ms -> 1,000,000 ns)" "duration" 56000000
          inputTextArea "requestHeaders as a key value json pair" "requestHeaders"
          inputTextArea "responseHeaders as a key value json pair" "responseHeaders"
          inputTextArea "" "pathParams"
          inputTextArea "" "queryParams"
          inputTextArea "" "requestBody"
          inputTextArea "" "responseBody"
          inputInt "" "statusCode" 200
          div_ do
            button_ [class_ "btn-sm btn-indigo", type_ "submit"] "Submit"
    script_ @Text
      [text|
        // create the editor
        var opt = {mode:"code", modes: ["code","tree"]}
        var reqHeadersEditor = new JSONEditor(document.getElementById("requestHeaders"), opt)
        var respHeadersEditor = new JSONEditor(document.getElementById("responseHeaders"), opt)
        var reqBodyEditor = new JSONEditor(document.getElementById("requestBody"), opt)
        var respBodyEditor = new JSONEditor(document.getElementById("responseBody"), opt)
        var queryParamsEditor = new JSONEditor(document.getElementById("queryParams"), opt)
        var pathParamsEditor = new JSONEditor(document.getElementById("pathParams"), opt)

        var initialJson = {
            "Content-Type": ["application/json"],
        }
        reqHeadersEditor.set(initialJson)
        respHeadersEditor.set(initialJson)

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

        var pathInitialJsonB = {"abc":"xyz"}
        pathParamsEditor.set(pathInitialJsonB)
        queryParamsEditor.set(pathInitialJsonB)
    |]


inputText :: Text -> Text -> Html ()
inputText title name = do
  div_ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  "
      , type_ "text"
      , id_ name
      , name_ name
      ]


inputTextDatalist :: Text -> Text -> [Text] -> Html ()
inputTextDatalist title name datalist = do
  div_ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  "
      , type_ "text"
      , id_ name
      , name_ name
      , list_ $ name <> "-list"
      ]
    datalist_ [id_ $ name <> "-list"] do
      datalist & mapM_ (\it -> option_ [value_ it] $ toHtml it)


inputTextArea :: Text -> Text -> Html ()
inputTextArea title name = do
  div_ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    div_
      [ class_ "w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl"
      , id_ name
      , name_ name
      ]
      ""


inputDatetime :: Text -> Html ()
inputDatetime name = do
  div_ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml name
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  "
      , type_ "datetime-local"
      , id_ name
      , name_ name
      , pattern_ "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}"
      ]


inputInt :: Text -> Text -> Int -> Html ()
inputInt title name value = do
  div_ do
    label_ [class_ "text-gray-400 mx-2  text-sm"] $ toHtml $ title <> " [" <> name <> "]"
    input_
      [ class_ "h-10 px-5 my-2 w-full text-sm bg-white text-black border-solid border border-gray-200 rounded-2xl  "
      , type_ "number"
      , id_ name
      , name_ name
      , value_ $ show value
      , lang_ "en-150"
      ]
