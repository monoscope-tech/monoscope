module Pages.IntegrationDemos.Flask (flaskGuide) where

import Data.Text
import Lucid
import NeatInterpolation
import Pkg.Components
import Relude


flaskGuide :: Text -> Html ()
flaskGuide apikey = do
  section_ [class_ "flex flex-col gap-10"] do
    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-2xl font-semibold"] "Install"
      p_ [class_ "text-gray-600 font-medium"] "Run the following command to install the flask package:"
      bashCommand "pip install apitoolkit-flask"

    div_ [class_ "w-full flex flex-col gap-2", id_ "requests-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Configure & Initialize The SDK"
      p_ [class_ "text-gray-600 font-medium"] "Configure and initialize the SDK to start monitoring incoming request to your server"
      codeExample
        $ [text|
from flask import Flask
from apitoolkit_flask import APIToolkit

app = Flask(__name__)

apitoolkit = APIToolkit(api_key="$apikey")

@app.before_request
def before_request():
    apitoolkit.beforeRequest()

@app.after_request
def after_request(response):
    apitoolkit.afterRequest(response)
    return response

@app.route('/hello', methods=['GET', 'POST'])
def sample_route(subject):
    return {"Hello": "World"}

app.run(debug=True)
      |]

    div_ [class_ "w-full flex flex-col gap-2"] do
      h3_ [class_ "text-xl font-medium"] "Configuration Options"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "The SDK accepts other options alongside apikey to allow you to customize the sdk. Redacting sensitive fields, debug mode etc."
      codeExample
        $ [text|
from apitoolkit_flask import APIToolkit

# A list of fields to redact from response body
redact_res = ["$.api_key", "$.password"]
# A list of fields to redact from request body
redact_req = ["$.credit-card.cvv", "$.credit-card.name"]
# A list of fields to redact from request and response headers
redact_headers = ["Authorization", "Cookie"]
# Tags for the current service 
tags = ["prod", "eu"]
# Current version of your service
service_version = "1.0.0"
# Set to true to enabel debug mode 
debug = False 

# Pass them as arguments to the APIToolkit class construtor.
apitoolkit = APIToolkit(api_key="", debug=debug,redact_response_body=redact_res, redact_request_body=redact_req,redact_headers=redact_headers)
|]

    div_ [class_ "w-full flex flex-col gap-2", id_ "errors-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Error Reporting"
      p_ [class_ "text-gray-600 font-medium max-w-5xl"] "APIToolkit allows you to report errors alongside the request that caused them which allows you to easily reproduce and fix issues in production."
      codeExample
        $ [text|
from flask import Flask, request
from apitoolkit_flask import report_error

apitoolkit = APIToolkit(api_key="$apikey")

@app.route('/sample/', methods=['GET', 'POST'])
async def sample_route(subject):
    try:
        resp = 1/0
        return resp
    except Exception as e:
        # Report the error to APIToolkit
        report_error(request, e)
        return "Something went wrong"
      |]

    div_ [class_ "w-full flex flex-col gap-2", id_ "outgoing-request-monitoring"] do
      h3_ [class_ "text-2xl font-semibold"] "Outgoing Request Monitoring"
      p_ [class_ "text-gray-600 max-w-5xl"] "APItoolkit also allows you to monitor your outgoing request (i.e the api calls your make from your server). Monitored outgoing are also associated with the incoming request that triggered them."
      codeExample
        $ [text|
from flask import Flask, request
from apitoolkit_flask import observe_request

apitoolkit = APIToolkit(api_key="$apikey")

@app.route('/sample/', methods=['GET', 'POST'])
async def sample_route(subject):
    # Observe the request and send it to APItoolkit's servers
    resp = observe_request(request).get("https://jsonplaceholder.typicode.com/todos/2")
    return resp.read()
      |]
