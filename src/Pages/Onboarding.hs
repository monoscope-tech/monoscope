{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Onboarding (onboardingGetH, integrateApiToolkit, tabs, contentHeader) where

import Data.Default (def)
import Data.Vector qualified as V
import Lucid
import Lucid.Htmx (hxGet_, hxSwap_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.RequestDumps qualified as RequestDumps
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projectjs
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.Components
import Relude hiding (ask)
import System.Types (ATAuthCtx)
import Utils (
  faIcon_,
  faSprite_,
 )


onboardingGetH :: Projects.ProjectId -> Maybe Bool -> Maybe Bool -> Maybe Text -> ATAuthCtx (Html ())
onboardingGetH pid polling redirected current_tab = do
  (sess, project) <- Sessions.sessionAndProject pid
  apiKeys <- dbtToEff $ ProjectApiKeys.projectApiKeysByProjectId pid
  requestDumps <- dbtToEff $ RequestDumps.countRequestDumpByProject pid
  let hasRequest = requestDumps > 0
  let apikey = if V.null apiKeys then "<APIKEY>" else (V.head apiKeys).keyPrefix
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess.persistentSession 
          , currProject = Just project
          , pageTitle = "Get Started"
          , hasIntegrated = Just hasRequest
          }
  case polling of
    Just _ -> pure $ onboardingPage pid apikey hasRequest (isJust project.questions) (fromMaybe False redirected) (fromMaybe "express" current_tab)
    Nothing -> pure $ bodyWrapper bwconf $ onboardingPage pid apikey hasRequest (isJust project.questions) (fromMaybe False redirected) "express"


onboardingPage :: Projects.ProjectId -> Text -> Bool -> Bool -> Bool -> Text -> Html ()
onboardingPage pid apikey hasRequest ans redi ctb = do
  div_
    [ class_ "relative h-full overflow-y-scroll  w-full"
    , hxGet_ $ "/p/" <> pid.toText <> "/onboarding?polling=True"
    , hxTrigger_ "load delay:30s"
    , hxVals_ "js:{current_tab:getCurrentTab()}"
    , hxSwap_ "outerHTML"
    ]
    $ do
      when redi $ div_ [class_ "w-full text-center py-2 bg-yellow-500"] "You have to integrate APItoolkit in your app before you can start using the platform"
      div_ [class_ "flex flex-col h-full w-full gap-10"] $ do
        div_ [class_ "w-full flex justify-center"] $ do
          div_ [class_ "flex flex-col w-[1000px] mt-4 rounded-2xl"] $ do
            div_ [class_ "w-full px-8 py-4 flex justify-between border-b border-b-2"] $ do
              h4_ [class_ "font-bold text-2xl text-blue-500"] "Onboarding checklist"
              let p
                    | hasRequest = "100%"
                    | otherwise = "66%"
              span_ [class_ "text-slate-500 text-lg font-bold"] $ p <> " completed"
            ul_ [class_ "grid grid-cols-3 items-start px-3 py-4 border rounded-xl mt-8"] do
              li_ [class_ "flex h-full mx-4 py-2 gap-6 text-green border-r"] do
                faSprite_ "circle-check" "sharp-regular" "h-6 w-6 mt-2 text-green-700"
                button_ [class_ "flex flex-col cursor-default"] do
                  p_ [class_ "font-semibold text-blue-500"] "Create an account"
                  span_ [class_ "text-slate-500"] "This is completed when you sign up."
              li_ [class_ "flex flex-col  mx-4 h-full py-2 gap-6 text-green border-r"] do
                div_ [class_ "flex w-full gap-6"] do
                  faSprite_ "circle-check" "sharp-regular" "shrink-0 h-6 w-6 mt-2 text-green-700"
                  button_
                    [ class_ "flex justify-between text-left w-full items-center"
                    ]
                    do
                      a_ [class_ "flex flex-col", href_ $ "/p/" <> pid.toText <> "/apis"] do
                        p_ [class_ "font-semibold text-blue-500 underline"] "Generate an API key"
                        span_ [class_ "text-slate-500"] "The API key is auto generated for you and will be used to authenticate requests."

              li_ [class_ "mx-4 py-2 h-full"] do
                div_ [class_ "flex w-full gap-6"] do
                  let style = if hasRequest then "text-green-700" else "text-gray-400 "
                  let style2 = if hasRequest then "text-blue-500" else "text-gray-400 "
                  faSprite_ "circle-check" "sharp-regular" $ "shrink-0 h-6 mt-2 w-6 " <> style
                  button_ [class_ "flex justify-between text-left w-full items-center cursor-default", [__|on click toggle .hidden on #SDKs|]] do
                    div_ [class_ "flex flex-col"] do
                      p_ [class_ $ "font-semibold " <> style2] "Integrate APItoolkit to your app"
                      span_ [class_ $ "text-slate-500"] "Integrate APItoolkit using any of our SDKs to start monitoring requests."
                when (not hasRequest) $ div_ [class_ " inline-block space-x-2 text-red-800 pt-2"] do
                  faIcon_ "fa-spinner" "fa-sharp fa-light fa-spinner " "fa-spin h-6 w-6 inline-block "
                  span_ "Waiting to recieve data from your server."

        div_ [class_ "text- center"] do
          div_ [class_ "flex flex-col w-full py-4 items-center gap-4"]
            $ if hasRequest
              then completedBanner pid
              else div_ [class_ "w-[1000px] min-w-0"] $ integrateApiToolkit apikey ctb

        div_ [class_ "w-full flex justify-center pb-16 mt-10"] $ do
          div_ [class_ "flex flex-col w-[1000px] rounded-2xl overflow-hidden border grid grid-cols-2 border-b "] $ do
            a_ [class_ "flex flex-col gap-2 py-8 border-r px-8 hover:bg-blue-100 ", href_ "https://www.apitoolkit.io/docs", target_ "_BLANK"] do
              faSprite_ "file-lines" "thin" "h-8 w-8"
              h3_ [class_ "font-bold text-lg"] "Documentation"
              p_ [class_ "text-slate-700"] "Check out our documentation to learn more about using APItoolkit."
              span_ [href_ "https://www.apitoolkit.io/docs", class_ "text-blue-500 flex items-center gap-2"] do
                faSprite_ "link-simple" "sharp-regular" "h-8 w-8 text-blue-500"
                "Read the docs"
            a_ [class_ "block px-8 py-16 flex items-center gap-6 border-l hover:bg-blue-100 ", href_ "https://calendar.app.google/EvPzCoVsLh5gqkAo8", target_ "_BLANK"] do
              faSprite_ "circle-play" "light" "text-blue-500 h-14 w-14"
              div_ [class_ "flex flex-col"] do
                span_ [class_ "font-bold text-lg text-blue-700 space-x-3"] do
                  span_ "Need Help?"
                  span_ "Or a Demo?"
                span_ [class_ "text-slate-500"] "Schedule a brief call with an Engineer."


integrateApiToolkit :: Text -> Text -> Html ()
integrateApiToolkit apikey current_tab =
  div_ [class_ "w-full mx-auto rounded-lg border mb-10 overflow-hidden"] do
    div_ [class_ "w-full p-8 bg-gray-50"] do
      div_ [class_ "flex w-full justify-center gap-4 items-center mb-2"] do
        span_ [class_ "text-blue-500 pr-4 border-r border-r-2 border-r-blue-500 font-bold"] "Next Up"
        h3_ [class_ "font-bold text-2xl"] "Integrate APItoolkit"
      div_ [class_ "pb-2 flex gap-10 w-full mt-8"] do
        div_ [class_ "font-bold text-center w-[35%]"] $ do
          tabs current_tab
        div_ [class_ "flex flex-col w-[65%] shrink-0 max-w-[65%]"] do
          tabContentExpress apikey current_tab
          tabContentGin apikey current_tab
          tabContentLaravel apikey current_tab
          tabContentFlask apikey current_tab
          tabContentFastAPI apikey current_tab
          tabContentDjango apikey current_tab
          tabContentSymfony apikey current_tab
          tabContentDotNet apikey current_tab
          tabContentFastify apikey current_tab
          tabContentEcho apikey current_tab
          tabContentGorilla apikey current_tab
          tabContentPhoenix apikey current_tab
          tabContentAdonis apikey current_tab
    script_
      [text|
var getCurrentTab = () => {
  const tab= document.querySelector(".sdk_tab_active")
  if(tab) {
    return tab.id
    }
  return 'express'
}
hljs.highlightAll();
 |]


completedBanner :: Projectjs.ProjectId -> Html ()
completedBanner pid =
  div_ [class_ "w-[1000px] bg-slate-200 mx-auto rounded-lg border shadow mb-10"] do
    div_ [class_ "w-full px-8 py-16 bg-slate-100  rounded"] do
      div_ [class_ "pb-2 flex items-center flex-col gap-4 text-blue-500 font-medium"] do
        faSprite_ "circle-check" "sharp-regular" "h-24 w-24 text-green-700"
        p_ [class_ "max-w-md text-center"] "Onboarding completed!"
        a_ [href_ $ "/p/" <> pid.toText <> "/", class_ "btn btn-primary"] "Go to the dashboard"


tabContentExpress :: Text -> Text -> Html ()
tabContentExpress apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "express" then "" else "hidden"), id_ "express_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Express Js"
        p_ [class_ "w-full py-1"] "Here is how to quickly integrate APItoolkit into your Express.js application"
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        bashCommand $ "npm i apitoolkit-express"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
      codeExample
        $ [text|
import express from 'express';
import { APIToolkit } from 'apitoolkit-express';

const app = express();
const port = 3000;

app.use(express.json());
app.use(express.urlencoded({ extended: true }));

const apitoolkitClient = APIToolkit.NewClient({ apiKey: "$apikey" });
app.use(apitoolkitClient.expressMiddleware);

app.get('/', (req, res) => {
  res.json({message:'Hello World!'})
});

app.listen(port, () => {
   console.log(`Example app listening on port $${port}`);
});
      |]
      guideFooterLink "https://apitoolkit.io/docs/sdks/nodejs/expressjs/" "Express JS"


tabContentGin :: Text -> Text -> Html ()
tabContentGin apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "gin" then "" else "hidden"), id_ "gin_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Golang Gin"
        p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Golang Gin application."
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        bashCommand $ "go get github.com/apitoolkit/apitoolkit-go"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
      codeExample
        $ [text|
package main

import (
	context

	apitoolkit "github.com/apitoolkit/apitoolkit-go"
	github.com/gin-gonic/gin"
)

func main() {
	apitoolkitCfg := apitoolkit.Config{
		APIKey: "$apikey",
	}

	// Initialize the client using your apitoolkit generated apikey
	apitoolkitClient, _ := apitoolkit.NewClient(context.Background(), apitoolkitCfg)
	router := gin.New()

	// Register with the corresponding middleware of your choice. For Gin router, we use the GinMiddleware method.
	router.Use(apitoolkitClient.GinMiddleware)

	// Register your handlers and run the gin server as usual.
	router.POST("/:slug/test", func(c *gin.Context) {
		c.JSON(200, gin.H{"message": "Hello world"})
	})
	router.Run(":8080")
}
|]
      guideFooterLink "https://apitoolkit.io/docs/sdks/golang/gin/" "Golang Gin"


tabContentLaravel :: Text -> Text -> Html ()
tabContentLaravel apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "laravel" then "" else "hidden"), id_ "laravel_content"] $ do
    div_ [class_ "relative flex flex-col gap-2"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Laravel PHP"
        p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Laravel application."
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        bashCommand "composer require apitoolkit/apitoolkit-php"
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1 mt-4"] "Integrate"
        p_ [class_ "w-full py-1"] do
          "First, set the APITOOLKIT_KEY environment variable to your"
          span_ [class_ "text-red-500"] " .env "
          "file."
        codeExample $ "APITOOLKIT_KEY=" <> apikey
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] do
        "Next, register the middleware in your"
        span_ [class_ "text-red-500"] " app/Http/Kernel.php "
        "file."
      codeExample
        $ [text|
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
      guideFooterLink "https://apitoolkit.io/docs/sdks/php/laravel/" "Laravel"


tabContentSymfony :: Text -> Text -> Html ()
tabContentSymfony apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "symfony" then "" else "hidden"), id_ "symfony_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Symfony PHP"
        p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Symfony PHP application."
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        bashCommand $ "composer require apitoolkit/apitoolkit-symfony"
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1 mt-4"] "Integrate"
        p_ [class_ "w-full py-1"] do
          "First, set the APITOOLKIT_KEY environment variable to your"
          span_ [class_ "text-red-500"] " .env "
          "file."
        codeExample $ "APITOOLKIT_KEY=" <> apikey
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] do
        "Next, register the middleware in your"
        span_ [class_ "text-red-500"] " service.yaml "
        "file"
      codeExample
        $ [text|
services:
  APIToolkit\EventSubscriber\APIToolkitService:
    arguments:
      $$apiKey: '%env(APITOOLKIT_KEY)%'
    # Optional:  if you want to cache login result add this cache poll instance via setter injection
    calls:
      - setCachePool: ['@PutYourCachePoolServiceHere']
    tags:
      - { name: 'kernel.event_subscriber' }
|]
      guideFooterLink "https://apitoolkit.io/docs/sdks/php/symfony/" "PHP Symfony"


tabContentDotNet :: Text -> Text -> Html ()
tabContentDotNet apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "net" then "" else "hidden"), id_ "net_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] ".NET"
        p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your .NET application."
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        bashCommand $ "dotnet add package ApiToolkit.Net"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
      codeExample
        $ [text|
using ApiToolkit.Net;

var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

var config = new Config
{
    ApiKey = "$apikey"
};
var client = await APIToolkit.NewClientAsync(config);

app.Use(async (context, next) =>
{
    var apiToolkit = new APIToolkit(next, client);
    await apiToolkit.InvokeAsync(context);
});

app.MapGet("/", async (context) =>
{
    await context.Response.WriteAsync("Hello World!");
});

app.Run();
|]
      guideFooterLink "https://apitoolkit.io/docs/sdks/dotnet/dotnetcore/" ".NET"


tabContentFastify :: Text -> Text -> Html ()
tabContentFastify apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "fastify" then "" else "hidden"), id_ "fastify_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Fastify JS"
          p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Fastify.js application."
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          bashCommand $ "npm install apitoolkit-fastify"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
        codeExample
          $ [text|
import APIToolkit from 'apitoolkit-fastify';
import Fastify from 'fastify';
const fastify = Fastify();

// Create and initialize an instance of the APIToolkit
const apittoolkitClient = APIToolkit.NewClient({
  apiKey: "$apikey",
  fastify,
});
apitoolkitClient.init();

//Rest of your app
fastify.get('/hello', function (request, reply) {
  reply.send({ hello: 'world' });
});

fastify.listen({ port: 3000 }, function (err, address) {
  if (err) {
    fastify.log.error(err);
    process.exit(1);
  }
});
|]
        guideFooterLink "https://apitoolkit.io/docs/sdks/nodejs/fastify/" "Fastify JS"


tabContentFlask :: Text -> Text -> Html ()
tabContentFlask apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "flask" then "" else "hidden"), id_ "flask_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Flask"
          p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Flask application."
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          bashCommand $ "pip install apitoolkit-flask"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
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
        guideFooterLink "https://apitoolkit.io/docs/sdks/python/flask/" "Flask"


tabContentFastAPI :: Text -> Text -> Html ()
tabContentFastAPI apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "fastapi" then "" else "hidden"), id_ "fastapi_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "FastAPI"
          p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your FastAPI application."
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          bashCommand $ "pip install apitoolkit-fastapi"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
        codeExample
          $ [text|
from fastapi import FastAPI
from apitoolkit_fastapi import APIToolkit

app = FastAPI()

redact_res = ["$.api_key", "$.password"]
# A list of fields to redact from request body
redact_req = ["$.credit-card.cvv", "$.credit-card.name"]
# A list of fields to redact from request and response headers
redact_headers = ["Authorization", "Cookie"]

# Initialize apitoolkit
apitoolkit = APIToolkit(
    api_key="$apikey", debug=True,redact_response_body=redact_res,
    redact_request_body=redact_req,redact_headers=redact_headers
)

app.middleware('http')(apitoolkit.middleware)

@app.get("/")
def read_root():
    return {"Hello": "World"}           
|]
        guideFooterLink "https://apitoolkit.io/docs/sdks/python/fastapi/" "FastAPI"


tabContentDjango :: Text -> Text -> Html ()
tabContentDjango apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "django" then "" else "hidden"), id_ "django_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Django"
          p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Django application."
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          bashCommand $ "pip install apitoolkit-django"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
        codeExample
          $ [text|
APITOOLKIT_KEY = "$apikey"
MIDDLEWARE = [
    ...,
    'apitoolkit_django.APIToolkit',
    ...,
]

APITOOLKIT_REDACT_HEADERS = ["Authorization", "Cookie","Content-Length", "Content-Type"]
APITOOLKIT_REDACT_REQ_BODY = ["$.password", "$.credit_card"]
APITOOLKIT_REDACT_RES_BODY = ["$.credentials", "$.social_security_number"]

|]
        guideFooterLink "https://apitoolkit.io/docs/sdks/python/django/" "Django"


tabContentEcho :: Text -> Text -> Html ()
tabContentEcho apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "echo" then "" else "hidden"), id_ "echo_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "go get github.com/apitoolkit/apitoolkit-go"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app by adding APITOOLKIT_KEY and APIToolkit to the settings middleware list"
        codeExample
          $ [text|
package main

import (
    "context"
    "github.com/labstack/echo/v4"
    apitoolkit "github.com/apitoolkit/apitoolkit-go"
)

func main() {
    ctx := context.Background()

    // Initialize the client using your apitoolkit.io generated apikey
    apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: "$apikey"})
    if err != nil {
        panic(err)
    }

    e := echo.New()

    // Register with the corresponding middleware of your choice.
    // Assuming apitoolkit provides an EchoMiddleware function for the echo framework.
    e.Use(apitoolkitClient.EchoMiddleware)

    e.POST("/:slug/test", func(c echo.Context) error {
        return c.String(http.StatusOK, "ok")
    })

    e.Start(":8080")
}|]
        guideFooterLink "https://apitoolkit.io/docs/sdks/golang/echo/" "Golang Echo"


tabContentGorilla :: Text -> Text -> Html ()
tabContentGorilla apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "gorilla" then "" else "hidden"), id_ "gorilla_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Golang Gorilla Mux"
          p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Golang Gorilla Mux application."
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          bashCommand $ "go get github.com/apitoolkit/apitoolkit-go"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate"
        codeExample
          $ [text|
package main

import (
    "context"
    "net/http"
    "github.com/gorilla/mux"
    apitoolkit "github.com/apitoolkit/apitoolkit-go"
)

func main() {
    ctx := context.Background()

    // Initialize the client using your generated apikey
    apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: "$apikey"})
    if err != nil {
        panic(err)
    }

    r := mux.NewRouter()
    // Register middleware
    r.Use(apitoolkitClient.GorillaMuxMiddleware)
    r.HandleFunc("/{slug}/test",func(w http.ResponseWriter, r *http.Request) {
        w.WriteHeader(http.StatusOK)
        w.Write([]byte("ok"))
    })

    http.ListenAndServe(":8080", r)
}
|]
        guideFooterLink "https://apitoolkit.io/docs/sdks/golang/gorillamux/" "Gorilla Mux"


tabContentPhoenix :: Text -> Text -> Html ()
tabContentPhoenix apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "phoenix" then "" else "hidden"), id_ "phoenix_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Elixir Phoenix"
          p_ [class_ "w-full py-1"] "Here is how to integrate APItoolkit into your Elixir application. "
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          codeExample $ "{:apitoolkit_phoenix, \"~> 0.1.1\"}"
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Integrate"
        codeExample
          $ [text|
defmodule HelloWeb.Router do
  use HelloWeb, :router
  use Plug.ErrorHandler
  import ApitoolkitPhoenix

  pipeline :api do
    plug :accepts, ["json"]
    # Other plugs
    plug ApitoolkitPhoenix,
      config: %{
        api_key: "$apikey",
        redact_headers: ["accept-language", "cookie", "x-csrf-token"] # list of headers to be redacted
        redact_request_body: [".user.password", ".user.credit_card"] # list of json paths to redact from request body
        redact_response_body: [".users[*].email"] # list of json paths to redact from response body

      }
  end
end |]
        guideFooterLink "https://apitoolkit.io/docs/sdks/elixir/phoenix/" "Elixir Phoenix"


tabContentAdonis :: Text -> Text -> Html ()
tabContentAdonis apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "adonis" then "" else "hidden"), id_ "adonis_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Adonis JS"
          p_ [class_ "w-full py-1"] "Here's how to quickly integrate APItoolkit into your Adonis.js application."
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          bashCommand $ "npm install apitoolkit-adonis"
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Integrate"
          p_ [class_ ""] "First configure the package"
          bashCommand $ "node ace configure apitoolkit-adonis"
          p_ [class_ "mt-4"] do
            "Next, set API key in a"
            span_ [class_ "text-red-500"] " /conf/apitoolkit.ts "
            "file."
          codeExample
            [text|
export const apitoolkitConfig = {
apiKey: "$apikey",
};
          |]

          p_ [class_ "mt-4"] do
            "Then, add"
            span_ [class_ "text-red-500"] " @ioc:APIToolkit "
            "to your global middlewares in the"
            span_ [class_ "text-red-500"] " start/kernel.ts "
            "file."
        codeExample
          $ [text|
Server.middleware.register([
  () => import(\"@ioc:Adonis/Core/BodyParser\"),
  () => import(\"@ioc:APIToolkit\"),
]);
|]
        guideFooterLink "https://apitoolkit.io/docs/sdks/nodejs/adonisjs/" "Adonis JS"


tabs :: Text -> Html ()
tabs current_tab =
  ul_ [class_ "grid grid-cols-3 font-medium w-full gap-4"] $ do
    script_
      [type_ "text/hyperscript"]
      [text|
      behavior Navigatable(content)
         on click remove .sdk_tab_active from .sdk_tab 
            then add .sdk_tab_active to me 
            then add .hidden to .tab-content 
            then remove .hidden from content
      end
    |]
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ $ if current_tab == "express" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #express_content) |]
        , id_ "express"
        ]
        do
          img_ [src_ "/assets/framework-logos/express-logo.png", alt_ "Express Js", class_ "w-full"]
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ $ if current_tab == "adonis" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #adonis_content) |]
        , id_ "adonis"
        ]
        do
          img_ [src_ "/assets/framework-logos/adonis-logo.png", alt_ "adonis", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "gin" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #gin_content) |]
        , id_ "gin"
        ]
        do
          img_ [src_ "/assets/framework-logos/gin-logo.png", alt_ "Gin", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "laravel" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #laravel_content) |]
        , id_ "laravel"
        ]
        do
          img_ [src_ "/assets/framework-logos/laravel-logo.png", alt_ "", class_ "w-full"]

    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "flask" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #flask_content) |]
        , id_ "flask"
        ]
        do
          img_ [src_ "/assets/framework-logos/flask-logo.png", alt_ "", class_ "w-full"]

    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "fastapi" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #fastapi_content) |]
        , id_ "fastapi"
        ]
        do
          img_ [src_ "/assets/framework-logos/fastapi-logo.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "django" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #django_content) |]
        , id_ "django"
        ]
        do
          img_ [src_ "/assets/framework-logos/django-logo.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "gorilla" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #gorilla_content) |]
        , id_ "gorilla"
        ]
        do
          img_ [src_ "/assets/framework-logos/mux-logo.png", alt_ "", class_ "w-full"]

    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "symfony" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #symfony_content) |]
        , id_ "symfony"
        ]
        do
          img_ [src_ "/assets/framework-logos/symfony-logo.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "net" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #net_content) |]
        , id_ "net"
        ]
        do
          img_ [src_ "/assets/framework-logos/net-logo.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "echo" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #phoenix_content) |]
        , id_ "phoenix"
        ]
        do
          img_ [src_ "/assets/framework-logos/phoenix.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "fastify" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #fastify_content) |]
        , id_ "fastify"
        ]
        do
          img_ [src_ "/assets/framework-logos/fastify-logo.png", alt_ "", class_ "w-full"]


contentHeader :: Text -> Html ()
contentHeader target =
  div_ [class_ "flex-none border-b border-slate-500/30 flex justify-between items-center gap-4"] do
    div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
      div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
      div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
      div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
    button_
      [ class_ "text-gray-500 text-sm font-bold mr-6"
      , term "data-target" target
      , [__|
          on click
            if 'clipboard' in window.navigator then
              call document.getElementById(@data-target)
              call navigator.clipboard.writeText(it's innerText)
              send successToast(value:['Copied']) to <body/>
            end
       |]
      ]
      do
        faIcon_ "fa-copy" "fa-solid fa-copy" "h-4 w-4 inline-block"


guideFooterLink :: Text -> Text -> Html ()
guideFooterLink url name = do
  p_ [class_ "mt-6"] do
    "⏭️: Read the"
    a_ [href_ url, class_ "text-blue-500 underline"] $ toHtml $ " " <> name <> " SDK guide "
    "to learn more."
