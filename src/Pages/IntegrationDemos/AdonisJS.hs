module Pages.IntegrationDemos.AdonisJS (adonisGuide) where

import Data.Text qualified as T
import Lucid
import Pkg.Components
import Relude


adonisGuide :: Text -> Html ()
adonisGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Install the APItoolkit express SDK using npm/bun/pnpm"
      bashCommand "npm install apitoolkit-adonis"

    div_ [class_ "w-full flex flex-col gap-2"] do
      p_ [class_ "text-gray-600 font-medium"] "Configure APItoolkit for your adonis project"
      bashCommand "node ace configure apitoolkit-adonis"

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Configuration"
      p_ [class_ "text-gray-600 font-medium"] "Add your API key to the conf/apitoolkit.ts default export"
      codeExample $ confCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "Initialize the apitoolkit adonis middleware by adding it to your"
        codeEmphasis " kernel.ts/js "
        "file"
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside API Key to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "AsyncLocalStorage"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The apitoolkit adonis package relies on asyncLocalStorage for observing outgoing requests and error reporting"
      h4_ [] do
        "To enable asyncLocalStorage in your adonis project, add this to your"
        codeEmphasis " config.ts "
        "file"
      codeExample configOptions2
    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To achieve this, wrap your axios instance with APItoolkit's observeAxios function."
      codeExample $ outgoingRequest apikey


initCode :: Text -> Text
initCode apiKey =
  unlines
    [ "Server.middleware.register(["
    , "    () => import('@ioc:Adonis/Core/BodyParser'),"
    , "    () => import(\"@ioc:APIToolkit\")"
    , "])"
    ]


confCode :: Text -> Text
confCode apiKey =
  unlines
    [ "export const apitoolkitConfig = {"
    , "    apiKey: \"" <> apiKey <> "\","
    ]


configOptions :: Text
configOptions =
  unlines
    [ "export const apitoolkitConfig = {"
    , "    apiKey: \"YOUR_API_KEY\","
    , "    redactHeaders: [\"Content-Type\", \"Authorization\", \"Cookies\"], // Specified headers will be redacted"
    , "    redactRequestBody: [\"$.credit-card.cvv\", \"$.credit-card.name\"], // Specified request bodies fields will be redacted"
    , "    redactResponseBody: [\"$.message.error\"] // Specified response body fields will be redacted"
    , "}"
    ]


configOptions2 :: Text
configOptions2 =
  unlines
    [ "export const http: ServerConfig = {"
    , "  useAsyncLocalStorage: true"
    , "  // other configs"
    , "}"
    ]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  unlines
    [ "import Route from '@ioc:Adonis/Core/Route'"
    , "import { reportError } from \"apitoolkit-adonis\";"
    , ""
    , "Route.get('/observer', async () => {"
    , "  try {"
    , "    throw (\"Error occured\")"
    , "  } catch (error) {"
    , "    reportError(error)"
    , "  }"
    , "  return { hello: 'world' }"
    , "})"
    ]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  unlines
    [ "import Route from '@ioc:Adonis/Core/Route'"
    , "import { observeAxios } from \"apitoolkit-adonis\""
    , "import axios from \"axios\""
    , ""
    , "Route.get('/observer', async () => {"
    , "    const response = await observeAxios(axios).get(`${baseURL}/user_list/active`);"
    , "    return response.data;"
    , "})"
    ]
