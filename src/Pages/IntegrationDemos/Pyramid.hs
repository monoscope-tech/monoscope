module Pages.IntegrationDemos.Pyramid (pyramidGuide) where

import Data.Text
import Data.Text qualified as T
import Lucid
import Pkg.Components
import Relude


pyramidGuide :: Text -> Html ()
pyramidGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium flex items-center gap-1"] do
        "Install the APItoolkit pyramid SDK using "
        span_ [class_ "text-red-500"] "pip:"
      bashCommand "pip install apitoolkit-pyramid"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your pyramid server. Add your APIToolkit API key APITOOLKIT_KEY to your development.ini or production.ini files or in your settings:"
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production. Aside reporting errors manually, we also automatically report unrecovered exceptions that occured during a request."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] do
        "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them. To monitor outgoing http requests from your pyramid application import"
        span_ [class_ "text-red-500"] " observe_request "
        "from the apitoolkit_pyramid package and use it to observe reqeust like so..."
      codeExample $ outgoingRequest apikey


initCode :: Text -> Text
initCode apiKey =
  T.unlines
    [ "from wsgiref.simple_server import make_server"
    , "from pyramid.config import Configurator"
    , "from pyramid.response import Response"
    , "from pyramid.view import view_config"
    , ""
    , "@view_config("
    , "    route_name='home'"
    , ")"
    , "def home(request):"
    , "    return Response('Welcome!')"
    , ""
    , "if __name__ == '__main__':"
    , "    setting = {\"APITOOLKIT_KEY\": \"" <> apiKey <> "\"}"
    , "    with Configurator(settings=setting) as config:"
    , "        # add aptoolkit tween"
    , "        config.add_tween(\"apitoolkit_pyramid.APIToolkit\")"
    , "        config.add_route('home', '/')"
    , "        config.scan()"
    , "        app = config.make_wsgi_app()"
    , "    server = make_server('0.0.0.0', 6543, app)"
    , "    server.serve_forever()"
    ]


configOptions :: Text
configOptions =
  T.unlines
    [ "setting = {"
    , "   # YOU API KEY"
    , "   \"APITOOLKIT_KEY\": \"<YOUR_API_KEY>\","
    , "   # List of request and response headers to redact"
    , "   \"APITOOLKIT_REDACT_HEADERS\": [\"Authorization\"],"
    , "   # List of field jsonpaths to redact in request body"
    , "   \"APITOOLKIT_REDACT_REQ_BODY\":[\"$.user.password\", \"$.user.email\"],"
    , "   # List of field jsonpaths to redact in response body"
    , "   \"APITOOLKIT_REDACT_RES_BODY\":[\"$.profile.account_number\", \"$.profile.cvv\"],"
    , "   # Your applications version"
    , "   \"APITOOLKIT_SERVICE_VERSION\": \" 1.0.0 \","
    , "   # Tags to help track different deployments"
    , "   \"APITOOLKIT_TAGS\": [],"
    , "   # Set to True to enable debug mode"
    , "   \"APITOOLKIT_DEBUG\": False,"
    , "   # When set, only route in whitelist are monitored"
    , "   \"APITOOLKIT_ROUTES_WHITELIST\": [\"/user/{name}\"],"
    , "   }"
    ]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  T.unlines
    [ "from pyramid.response import Response"
    , "from pyramid.view import view_config"
    , "from apitoolkit_pyramid import observe_request, report_error"
    , ""
    , "@view_config(route_name='home')"
    , "def home(request):"
    , "  try:"
    , "    val = 1/0"
    , "    return Response(val)"
    , "  except Exception as e:"
    , "    # Report error to apitoolkit"
    , "    report_error(request, e)"
    , "    return Response(\"something went wrong\")"
    , ""
    , "if __name__ == '__main__':"
    , "    setting = {\"APITOOLKIT_KEY\": \"" <> apiKey <> "\"}"
    , "    with Configurator(settings=setting) as config:"
    , "        config.add_tween(\"apitoolkit_pyramid.APIToolkit\")"
    , "        config.add_route('home', '/user/{name}')"
    , "        config.scan()"
    , "        app = config.make_wsgi_app()"
    , "    server = make_server('0.0.0.0', 6543, app)"
    , "    server.serve_forever()"
    ]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  T.unlines
    [ "from pyramid.response import Response"
    , "from pyramid.view import view_config"
    , "from apitoolkit_pyramid import observe_request"
    , ""
    , "@view_config(route_name='home')"
    , "def home(request):"
    , "    resp = observe_request(request).get("
    , "        \"https://jsonplaceholder.typicode.com/todos/2\")"
    , "    return Response(resp.read())"
    , ""
    , "if __name__ == '__main__':"
    , "    setting = {\"APITOOLKIT_KEY\": \"" <> apiKey <> "\"}"
    , "    with Configurator(settings=setting) as config:"
    , "        config.add_tween(\"apitoolkit_pyramid.APIToolkit\")"
    , "        config.add_route('home', '/user/{name}')"
    , "        config.scan()"
    , "        app = config.make_wsgi_app()"
    , "    server = make_server('0.0.0.0', 6543, app)"
    , "    server.serve_forever()"
    ]
