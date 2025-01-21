module Pages.IntegrationDemos.Python (pythonGuide) where

import Data.Text qualified as T
import Lucid
import NeatInterpolation (text)
import Pkg.Components
import Relude


pythonGuide :: Text -> Html ()
pythonGuide apiKey =
  section_ [class_ "flex flex-col gap-4 lang-guide hidden", id_ "py_main"] do
    span_ [class_ "text-2xl font-semibold text-strong"] "Integrate Golang SDK"
    -- div_ [class_ "flex items-center gap-2"] do
    --   mapM_ featureItem features
    div_ [class_ "flex flex-col gap-2"] do
      span_ [class_ "text-strong font-semibold"] "Select framework"
      div_ [class_ "flex items-center gap-2"] do
        mapM_ (frameworkItem "python") frameworks
    div_ [class_ "w-full border-b"] pass
    pythonTemplate apiKey "Django" "opentelemetry-instrument python3 manage.py runserver --noreload" djangCode djangoConfig
    pythonTemplate apiKey "FastApi" "opentelemetry-instrument uvicorn main:app --reload" fastapi fastapiConfig
    pythonTemplate apiKey "Flask" "opentelemetry-instrument flask run --app app" flask flaskConfig
    pythonTemplate apiKey "Pyramid" "opentelemetry-instrument pserve development.ini" pyramid pyramidConfig


frameworks :: [Text]
frameworks = ["Django", "FastApi", "Flask", "Pyramid"]


pythonTemplate :: Text -> Text -> Text -> Text -> Text -> Html ()
pythonTemplate apikey framework runCommand initCode configOptions = do
  section_ [class_ $ "flex flex-col gap-10 python-guide " <> if framework == "Django" then "" else "hidden", id_ framework] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Installation"
      p_ [class_ "text-gray-600 font-medium"] $ toHtml $ "Run the command below to install the APItoolkit " <> framework <> " sdk"
      bashCommand $ "pip install apitoolkit-" <> T.toLower framework <> " opentelemetry-distro opentelemetry-exporter-otlp"
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Open Telemetry SDK Configuration"
      p_ [class_ "text-gray-600 font-medium"] "Set the neccessary environment variables to configure the SDK"
      codeExample $ otelConfig apikey framework
      p_ [class_ "text-gray-600 font-medium"] "Then run the command below to start your server with opentelemetry instrumentation"
      bashCommand runCommand
    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] $ toHtml $ "Configure & Initialize APItoolkit " <> framework <> " sdk"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming http request, errors and outgoing request."
      codeExample initCode
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts the following optional fields to allow you to customize the sdk. Redacting sensitive fields, debug mode etc"
      codeExample configOptions


otelConfig :: Text -> Text -> Text
otelConfig apikey framework =
  let djangExtr = if framework == "Django" then "\nexport DJANGO_SETTINGS_MODULE=mysite.settings" else ""
   in [text|
export OTEL_EXPORTER_OTLP_ENDPOINT="http://otelcol.apitoolkit.io:4317"
export OTEL_SERVICE_NAME="my-service" # Specifies the name of the service.
export OTEL_RESOURCE_ATTRIBUTES="at-project-key=$apikey" # Adds your API KEY to the resource.
export OTEL_EXPORTER_OTLP_PROTOCOL="grpc" # Specifies the protocol to use for the OpenTelemetry exporter.$djangExtr
  |]


djangCode :: Text
djangCode =
  [text|
from pathlib import Path
from dotenv import load_dotenv
import os

load_dotenv()

# Build paths inside the project like this: BASE_DIR / 'subdir'.
BASE_DIR = Path(__file__).resolve().parent.parent

# Add the APItoolkit configuration options
APITOOLKIT_CAPTURE_REQUEST_BODY = True
APITOOLKIT_CAPTURE_RESPONSE_BODY = True

MIDDLEWARE = [
  'apitoolkit_django.APIToolkit',  # Initialize APItoolkit
  # Add other middleware as needed
]
|]


djangoConfig :: Text
djangoConfig =
  [text|
from pathlib import Path
from dotenv import load_dotenv
import os

load_dotenv()

# Build paths inside the project like this: BASE_DIR / 'subdir'.
BASE_DIR = Path(__file__).resolve().parent.parent

# All APItoolkit configuration options
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


fastapi :: Text
fastapi =
  [text|
from fastapi import FastAPI
from apitoolkit_fastapi import APIToolkit

app = FastAPI()

# Initialize APItoolkit
apitoolkit = APIToolkit()
app.middleware("http")(apitoolkit.middleware)
# END Initialize APItoolkit

@app.get("/")
def read_root():
  return {"Hello": "World"}
|]


fastapiConfig :: Text
fastapiConfig =
  [text|
from fastapi import FastAPI
from apitoolkit_fastapi import APIToolkit

app = FastAPI()
$apitoolkitConfig
|]


flask :: Text
flask =
  [text|
from flask import Flask
from apitoolkit_flask import APIToolkit

app = Flask(__name__)

# Initialize APItoolkit
apitoolkit = APIToolkit()

@app.before_request
def before_request():
  apitoolkit.beforeRequest()

@app.after_request
def after_request(response):
  apitoolkit.afterRequest(response)
  return response
# END Initialize APItoolkit

@app.route('/hello', methods=['GET', 'POST'])
def sample_route():
  return {"Hello": "World"}

app.run(debug=True)
|]


flaskConfig :: Text
flaskConfig =
  [text|
from flask import Flask
from apitoolkit_flask import APIToolkit

app = Flask(__name__)

$apitoolkitConfig
|]


apitoolkitConfig :: Text
apitoolkitConfig =
  [text|
# Initialize APItoolkit
apitoolkit = APIToolkit(
service_name="your-service-name" #	A defined string name of your application (used for further debugging on the dashboard).
debug=False #	Set to true to enable debug mode.
tags=["prod", "eu"] # A list of defined tags for your services (used for grouping and filtering data on the dashboard).
service_version="2.2" #	A defined string version of your application (used for further debugging on the dashboard).
redact_headers=["Authorization", "X-API-KEY"]	# A list of HTTP header keys to redact.
redact_resquest_body=["$.password"]	# A list of JSONPaths from the request body to redact.
redact_response_body=["$.email"]	# A list of JSONPaths from the response body to redact.
capture_request_body=True	# Set to true to capture the request body.
capture_response_body=True	# Set to true to capture the response body.
)
|]


pyramid :: Text
pyramid =
  [text|
from wsgiref.simple_server import make_server
from pyramid.config import Configurator
from pyramid.response import Response
from pyramid.view import view_config

@view_config(route_name='home')
def home(request):
  return Response('Welcome!')

if __name__ == '__main__':
  settings = {
    "APITOOLKIT_SERVICE_NAME": "YOUR_SERVICE_NAME",
  }
  with Configurator(settings=settings) as config:
    # Initialize APItoolkit
    config.add_tween("apitoolkit_pyramid.APIToolkit")
    # END Initialize APItoolkit
    config.add_route('home', '/')
    config.scan()
    app = config.make_wsgi_app()
  server = make_server('0.0.0.0', 6543, app)
  server.serve_forever()
|]


pyramidConfig :: Text
pyramidConfig =
  [text|
settings = {
  "APITOOLKIT_SERVICE_NAME": "your-service-anem" # A defined string name of your application
  "APITOOLKIT_DEBUG": False # Set to true to enable debug mode.
  "APITOOLKIT_TAGS": ["eu", "prod"]	# A list of defined tags for your services (used for grouping and filtering data on the dashboard).
  "APITOOLKIT_SERVICE_VERSION": "2.2.1"	# A defined string version of your application (used for further debugging on the dashboard).
  "APITOOLKIT_REDACT_HEADERS": ["Authorizatoin", "X-API-KEY"]	# A list of HTTP header keys to redact.
  "APITOOLKIT_REDACT_REQUEST_BODY": ["$.password"]	# A list of JSONPaths from the request body to redact.
  "APITOOLKIT_REDACT_RESPONSE_BODY": ["$.email"]	# A list of JSONPaths from the response body to redact.
  "APITOOLKIT_CAPTURE_REQUEST_BODY": True	# Set to true to capture the request body.
  "APITOOLKIT_CAPTURE_RESPONSE_BODY": True	# Set to true to capture the response body.
  }
|]
