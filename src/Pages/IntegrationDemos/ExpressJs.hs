module Pages.IntegrationDemos.ExpressJs (expressGuide) where

import Data.Text
import Data.Text qualified as T
import Lucid
import Pkg.Components
import Relude


expressGuide :: Text -> Html ()
expressGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Install the APIToolkit express SDK using npm/bun/pnpm"
      bashCommand "npm install apitoolkit-express"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your server"
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APIToolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APIToolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To achieve this, wrap your axios instance with APIToolkit's observeAxios function."
      codeExample $ outgoingRequest apikey


initCode :: Text -> Text
initCode apiKey =
  T.unlines
    [ "import express from 'express';"
    , "import { APIToolkit } from 'apitoolkit-express';"
    , ""
    , "const app = express();"
    , "const port = 3000;"
    , ""
    , "app.use(express.json());"
    , "app.use(express.urlencoded({ extended: true }));"
    , ""
    , "const apitoolkitClient = APIToolkit.NewClient({ apiKey: '" <> apiKey <> "' });"
    , "app.use(apitoolkitClient.expressMiddleware);"
    , ""
    , "app.get('/', (req, res) => {"
    , "  res.json({message:'Hello World!'})"
    , "});"
    , ""
    , "app.listen(port, () => {"
    , "   console.log(`Example app listening on port ${port}`);"
    , "});"
    ]


configOptions :: Text
configOptions =
  T.unlines
    [ "{"
    , "  // List of request and response headers to be redacted"
    , "  redactHeaders: [\"Authorization\"],"
    , "  // Json path list of response body fields to be redacted"
    , "  redactResponseBody: [\"$.user.email\", \"$.user.age\"],"
    , "  // Json path list of request body fields to be redacted"
    , "  redactRequestBody: [\"$.password\", \"$.credit_card\", \"$.ccv\"],"
    , "  // Set debug to true to help in troubleshooting issues"
    , "  debug: true"
    , "}"
    ]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  T.unlines
    [ "import { APIToolkit, ReportError } from \"apitoolkit-express\";"
    , "import express from \"express\";"
    , "import axios from \"axios\";"
    , ""
    , "const app = express();"
    , "const port = 3000;"
    , ""
    , "const apitoolkitClient = APIToolkit.NewClient({ apiKey: \"" <> apiKey <> "\" });"
    , "app.use(apitoolkitClient.expressMiddleware);"
    , ""
    , "app.get(\"/\", (req, res) => {"
    , "  try {"
    , "    let inf = 1/0;"
    , "    res.send(\"The impossible number is: \" + inf);"
    , "  } catch (error) {"
    , "    // Manually report errors to APIToolkit"
    , "    ReportError(error);"
    , "    res.send(\"Something went wrong\");"
    , "  }"
    , "});"
    , "// Automatically report unhandled errors"
    , "// Error handler must be before any other error middleware and after all controllers"
    , "app.use(apitoolkitClient.errorHandler);"
    , ""
    , "app.listen(port, () => {"
    , "   console.log(`Example app listening on port ${port}`);"
    , "});"
    ]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  T.unlines
    [ "import express from 'express';"
    , "import axios from 'axios';"
    , "import { observeAxios } from 'apitoolkit-express';"
    , ""
    , "const app = express();"
    , "const port = 3000;"
    , "const apitoolkitClient = APIToolkit.NewClient({ apiKey: '" <> apiKey <> "' });"
    , "app.use(apitoolkitClient.expressMiddleware);"
    , ""
    , "app.get('/', async (req, res) => {"
    , "    const response = await observeAxios(axios).get(\"https://jsonplaceholder.typicode.com/posts/1\");"
    , "    res.send(response.data);"
    , "});"
    , ""
    , "app.listen(port, () => {"
    , "   console.log(`Example app listening on port ${port}`);"
    , "});"
    ]
