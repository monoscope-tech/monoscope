module Pages.IntegrationDemos.Php (phpGuide) where

import Data.Text qualified as T
import Lucid
import NeatInterpolation (text)
import Pkg.Components
import Relude


phpGuide :: Text -> Html ()
phpGuide apikey = do
  section_ [class_ "flex flex-col gap-4 lang-guide hidden", id_ "php_main"] do
    span_ [class_ "text-2xl font-semibold text-strong"] "Integrate PHP SDK"
    -- div_ [class_ "flex items-center gap-2"] do
    --   mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      span_ [class_ "text-strong font-semibold"] "Select framework"
      div_ [class_ "flex items-center gap-2"] do
        mapM_ (frameworkItem "php") frameworks
      div_ [class_ "w-full border-b"] pass
      phpGuideTemplate apikey "Laravel" laravelCode "php artisan serve"
      phpGuideTemplate apikey "Slim" slimCode "php -S localhost:8000"


frameworks :: [Text]
frameworks = ["Laravel", "Slim"]


phpGuideTemplate :: Text -> Text -> Text -> Text -> Html ()
phpGuideTemplate apikey framework initCode runCommand = do
  section_ [class_ $ "flex flex-col gap-10 php-guide " <> if framework == "Laravel" then "" else "hidden", id_ framework] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] $ toHtml $ "Run the command below to install the APItoolkit " <> framework <> " sdk"
      bashCommand $ "composer require open-telemetry/sdk open-telemetry/exporter-otlp open-telemetry/opentelemetry-auto-psr18 apitoolkit/apitoolkit-" <> T.toLower framework
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [] "To setup open telemetry First install the open telemetry php extension:"
      bashCommand "pecl install opentelemetry"
      p_ [] "Then add it to your php.ini file"
      codeExample
        [text|
[opentelemetry]
extension=opentelemetry.so|]
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey
      p_ [class_ "text-gray-600 font-medium"] "Then run the command below to start your server with opentelemetry instrumentation"
      bashCommand runCommand
    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] $ toHtml $ "Configure & Initialize APItoolkit " <> framework <> " sdk"
      p_ [class_ "text-gray-600 font-medium"] do
        "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request"
        when (framework == "Laravel") do
          " by adding the following code snippet to your "
          codeEmphasis "Kernel.php"
          " file"
      codeExample initCode
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      when (framework == "Laravel") do
        p_ [class_ "text-gray-600 font-medium"] "The SDK reads the following environment variables to configure the SDK"
        codeExample laravelConfig
      when (framework == "Slim") do
        p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
        codeExample slimConfig


otelConfig :: Text -> Text
otelConfig apikey =
  [text|
export OTEL_PHP_AUTOLOAD_ENABLED=true
export OTEL_SERVICE_NAME=your-service-name
export OTEL_TRACES_EXPORTER=otlp
export OTEL_EXPORTER_OTLP_PROTOCOL=http/protobuf
export OTEL_EXPORTER_OTLP_ENDPOINT=http://otelcol.apitoolkit.io:4318
export OTEL_RESOURCE_ATTRIBUTES="at-project-key=apikey"
  |]


laravelCode :: Text
laravelCode =
  [text|
<?php

namespace App\Http;

use Illuminate\Foundation\Http\Kernel as HttpKernel;

class Kernel extends HttpKernel
{
  protected $$middlewareGroups = [
    'api' => [
      // Other middleware here...
      \APIToolkit\Http\Middleware\APIToolkit::class, // Initialize the APItoolkit client
    ],
  ];
}
|]


laravelConfig :: Text
laravelConfig =
  [text|
APITOOLKIT_CAPTURE_REQUEST_BODY = True
APITOOLKIT_CAPTURE_RESPONSE_BODY = True
APITOOLKIT_SERVICE_NAME = "your-service-name" #	A defined string name of your application
APITOOLKIT_DEBUG = False	# Set to true to enable debug mode.
APITOOLKIT_TAGS = ["prod","eu"]	# A list of defined tags for your services (used for grouping and filtering data on the dashboard).
APITOOLKIT_SERVICE_VERSION = "2.1.2"	# A defined string version of your application (used for further debugging on the dashboard).
APITOOLKIT_REDACT_HEADERS = ["Authorization"]	# A list of HTTP header keys to redact.
APITOOLKIT_REDACT_REQUEST_BODY = ["$.password", "$.email"]	# A list of JSONPaths from the request body to redact.
APITOOLKIT_REDACT_RESPONSE_BODY = ["$.email"]	# A list of JSONPaths from the response body to redact.
APITOOLKIT_CAPTURE_REQUEST_BODY	= True # Set to true to capture the request body.
APITOOLKIT_CAPTURE_RESPONSE_BODY = True	# Set to true to capture the response body.

|]


slimCode :: Text
slimCode =
  [text|
use Slim\Factory\AppFactory;
use APIToolkit\APIToolkitMiddleware;

require __DIR__ . '/vendor/autoload.php';

$$app = AppFactory::create();

// Initialize the APItoolkit client
$$apitoolkitMiddleware = new APIToolkitMiddleware([
  'debug' => false,
  'tags' => ["environment: production", "region: us-east-1"],
  'captureRequestBody' => true,
  'serviceVersion' => "v2.0",
]);

$$app->add($$apitoolkitMiddleware);
// END Initialize the APItoolkit client

$$app->get('/', function ($$request, $$response) {
  $$response->getBody()->write('Hello, World!');
  return $$response;
});

$$app->run();
|]


slimConfig :: Text
slimConfig =
  [text|
$$apitoolkitMiddleware = new APIToolkitMiddleware([
    'debug' => false, // Enable or disable debug mode. Set to true for additional logging.
    'tags' => ["environment: production", "region: us-east-1"], // Tags to help with grouping and filtering data on the dashboard.
    'serviceVersion' => "v2.0", // Version of your application. Useful for debugging different versions.
    'redactHeaders' => ["Authorization", "X-API-Key"], // List of HTTP header keys to redact from captured data for privacy.
    'serviceName' => "your-service-name", // Name of your application.
    'redactRequestBody' => ["$.password", "$.creditCard.number"], // List of JSONPaths in the request body to redact for privacy.
    'redactResponseBody' => ["$.user.token", "$.transaction.details"], // List of JSONPaths in the response body to redact for privacy.
    'captureRequestBody' => true, // Whether to capture the request body. Default is false.
    'captureResponseBody' => true,  // Whether to capture the response body. Default is false.
]);

|]
