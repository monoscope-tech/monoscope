module Pages.IntegrationDemos.Slim (slimGuide) where

import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


slimGuide :: Text -> Html ()
slimGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Run the following command to install the Slim package:"
      bashCommand "composer require apitoolkit/apitoolkit-slim"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] "Create a new instance of the APIToolkitMiddleware class and register the middleware in your Slim app."
      codeExample $ initCode apikey

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc. All environment variables"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APIToolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APIToolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing requests are also associated with the incoming request that triggered them. To achieve this, use the observeGuzzle method of the APIToolkitSlim class."
      codeExample $ outgoingRequest apikey


initCode :: Text -> Text
initCode apikey =
  [text|
php use Slim\Factory\AppFactory; 
use APIToolkit\APIToolkitMiddleware;

require DIR . '/vendor/autoload.php';

$$app = AppFactory::create();

$$apitoolkitMiddleware = new APIToolkitMiddleware("$apikey");

$$app->add($$apitoolkitMiddleware);

$$app->get('/', function ($$request, $$response) { 
    $$response->getBody()->write('Hello, World!'); 
    return $$response;
});

$$app->run();
|]


configOptions :: Text
configOptions =
  [text|
$$redactHeaders = 
$$redactRequestBody = ["$.user.password"]
$$redactResponseBody = ["$.user.token"]
$$debug = false
$$serviceVersion = "1.0.1"
$$tags = ["Prod", "Eu"]
$$apitoolkitMiddleware = new APIToolkitMiddleware("<API_KEY>", $$redactHeaders, $$redactRequestBody, $$redactResponseBody, $$debug, $$serviceVersion, $$tags);
|]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  [text|
use Slim\Factory\AppFactory;
use APIToolkit\APIToolkitMiddleware;
use APIToolkit\APIToolkitSlim;

require __DIR__ . '/vendor/autoload.php';

$$app = AppFactory::create();

$$apitoolkitMiddleware = new APIToolkitMiddleware("$apiKey");

$$app->add($$apitoolkitMiddleware);

$$app->get('/', function (Request $$request, Response $$response) {
    try {
        throw new Exception("Custom user error");
        return $$response;
    } catch (Exception $$e) {
        // Report the error to APIToolkit
        APIToolkitSlim::reportError($$e, $$request);
        $$response->getBody()->write($$e->getMessage());
        return $$response;
    }
});

$$app->run();
|]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
use Slim\Factory\AppFactory;
use APIToolkit\APIToolkitMiddleware;
use APIToolkit\APIToolkitSlim;

require __DIR__ . '/vendor/autoload.php';

$$app = AppFactory::create();

$$apitoolkitMiddleware = new APIToolkitMiddleware("$apiKey");

$$app->add($$apitoolkitMiddleware);

$$app->get('/', function (Request $$request, Response $$response) {
    $$guzzleClient = APIToolkitSlim::observeGuzzle($$request);
    $$responseFromGuzzle = $$guzzleClient->request('GET', 'https://api.example.com/resource');
    $$response->getBody()->write($$responseFromGuzzle->getBody()->getContents());
    return $$response;
});

$$app->run();

|]
