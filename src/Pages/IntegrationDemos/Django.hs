module Pages.IntegrationDemos.Django (djangoGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


djangoGuide :: Text -> Html ()
djangoGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Run the following command to install the django package:"
      bashCommand "pip install apitoolkit-django"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "SET API KEY"
      p_ [class_ "text-gray-600 font-medium"] do
        "Add"
        codeEmphasis " APITOOLKIT_KEY "
        "and set it to your API KEY in your django"
        codeEmphasis " settings.py "
        "file:"
      codeExample $ apiKeyCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Initialize SDK"
      p_ [class_ "text-gray-600 font-medium"] do
        "To initialize the django package simple Tadd apitoolkit_django middleware into the"
        codeEmphasis " settings.py "
        "middleware list:"
      codeExample initCode

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] do
        "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc. All in your django"
        codeEmphasis " settings.py "
        "file"
      codeExample configOptions

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APItoolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample $ errorReportingCode apikey

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them."
      codeExample $ outgoingRequest apikey


apiKeyCode :: Text -> Text
apiKeyCode apiKey =
  [text|
APITOOLKIT_KEY=$apiKey
|]


initCode :: Text
initCode =
  [text|
MIDDLEWARE = [
    ...,
    'apitoolkit_django.APIToolkit',
    ...,
]
|]


configOptions :: Text
configOptions =
  [text|
# A list of headers to be redacted.
APITOOLKIT_REDACT_HEADERS = ["Authorization", "Cookie","Content-Length", "Content-Type"] 
# A list of request body fields (jsonpaths) to be redacted
APITOOLKIT_REDACT_REQ_BODY = ["$.password", "$.credit_card"]
# A list of response body fields (jsonpaths) to be redacted. 
APITOOLKIT_REDACT_RES_BODY = ["$.credentials", "$.social_security_number"]
# Set to true to enable debug mode
APITOOLKIT_DEBUG = False
# Tags
APITOOLKIT_TAGS = ["PROD", "EU"]
# Service version
APITTOLKIT_SERVICE_VERSION = "2.0.0"
|]


errorReportingCode :: Text -> Text
errorReportingCode apiKey =
  [text|
from django.http import JsonResponse
from apitoolkit_django import  report_error

def hello_world(request, name):
    try:
        v = 1/0
        return JsonResponse({"hello": "world " + v})
    except Exception as e:
        report_error(request, e)
        return JsonResponse({"Error": "Something went wrong"})
|]


outgoingRequest :: Text -> Text
outgoingRequest apiKey =
  [text|
from django.http import JsonResponse
from apitoolkit_django import observe_request

def hello_world(request, name):
    resp = observe_request(request).get(
        "https://jsonplaceholder.typicode.com/todos/2")
    resp.read()
    return JsonResponse({"data": resp.read()})
|]
