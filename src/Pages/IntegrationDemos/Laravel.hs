module Pages.IntegrationDemos.Laravel (laravelGuide) where

import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


laravelGuide :: Text -> Html ()
laravelGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Run the following command to install the Laravel package:"
      bashCommand "composer require apitoolkit/apitoolkit-laravel"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "SET API KEY"
      p_ [class_ "text-gray-600 font-medium"] do
        "Set the APITOOLKIT_KEY environment variable to your API key in you"
        codeEmphasis " .env "
        "file, should look like this:"
      codeExample $ apiKeyCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "Register the middleware in the"
        codeEmphasis " app/Http/Kernel.php "
        "file under the correct middleware group eg api, or at the root"
      codeExample initCode

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc. All environment variables"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APIToolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample errorReportingCode

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APIToolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them, you can also monitor request in a background job or outside request context. To achieve this, use the observeGuzzle method of the APIToolkitLaravel class."
      codeExample outgoingRequest


apiKeyCode :: Text -> Text
apiKeyCode apiKey =
  [text|
APITOOLKIT_KEY=$apiKey
|]


initCode :: Text
initCode =
  [text|
<?php

namespace App\Http;

use Illuminate\Foundation\Http\Kernel as HttpKernel;

class Kernel extends HttpKernel
{
    ...
    /**
     * The application's route middleware groups.
     *
     * @var array
     */
    protected $$middlewareGroups = [
        ...
        'api' => [
            ...
            \APIToolkit\Http\Middleware\APIToolkit::class,
            ...
        ],
    ];
    ...
}
|]


configOptions :: Text
configOptions =
  [text|
   APITOOLKIT_TAGS # array A list of tags for your services. 
   APITOOLKIT_SERVICE_VERSION=1.0.0 #string The version of your application. 
   APITOOLKIT_REDACT_HEADERS=["Authorization"] # array A list of headers to be redacted. 
   APITOOLKIT_REDACT_REQUEST_BODY=["$.user.password"] # array A list of request body fields (jsonpaths) to be redacted. 
   APITOOLKIT_REDACT_RESPONSE_BODY=["$.user.token"] # array A list of response body fields (jsonpaths) to be redacted. 
   APITOOLKIT_DEBUG=false  #boolean Set to true to enable debug.
|]


errorReportingCode :: Text
errorReportingCode =
  [text|
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;
use Illuminate\Foundation\Exceptions\Handler as ExceptionHandler;
use APIToolkit\APIToolkitLaravel;
use Throwable;

class Handler extends ExceptionHandler
{
    /**
     * Register the exception handling callbacks for the application.
     * For automatic error reporting
     */
    public function register(): void
    {
        $$this->reportable(function (Throwable $$e) {
            // Report the error to APIToolkit
            $$request = request();
            APIToolkitLaravel::reportError($$e, $$request);
        });
    }
}

// You can also report errors manually like so. 

Route::get('/user', function (Request $$request) {
    try {
        throw new Exception("Custom user error");
        return response()->json(["hello" => "world"]);
    } catch (Exception $$e) {
        // Report the error to APIToolkit
        APIToolkitLaravel::reportError($$e, $$request);
        return response()->json(["error" => $$e->getMessage()]);
    }
});
|]


outgoingRequest :: Text
outgoingRequest =
  [text|
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;
use APIToolkit\APIToolkitLaravel;

Route::get('/user', function (Request $$request) {
    $$options = [
        "pathPattern" => "/repos/{owner}/{repo}", # For observing Requests with Path Params
        "redactHeaders" => ["Server"], # headers redaction
        "redactRequestBody" => ["$.password"],
        "redactResponseBody" => ["$.password"]
    ];
    $$guzzleClient = APIToolkitLaravel::observeGuzzle($$request, $$options);
    $$responseFromGuzzle = $$guzzleClient->request('GET', 'https://api.github.com/repos/guzzle/guzzle?foobar=123');
    $$response = $$responseFromGuzzle->getBody()->getContents();

    return $$response;
})
|]
