module Pages.Outgoing (outgoingGetH) where

import Data.Default (def)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Lucid
import Lucid.Hyperscript
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Pages.BodyWrapper
import Pages.Charts.Charts (QueryBy (QBHost))
import Pages.Charts.Charts qualified as Charts
import Pages.NonMember
import Pages.Onboarding qualified as Onboarding
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import System.Config
import System.Types
import Utils

outgoingGetH :: Projects.ProjectId -> ATAuthCtx (Html ())
outgoingGetH pid = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, hostsEvents) <- dbtToEff do
        project <- Projects.projectById pid
        hostsAndEvents <- Endpoints.dependenciesAndEventsCount pid
        pure (project, hostsAndEvents)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess,
                currProject = project,
                pageTitle = "Dependencies"
              }
      pure $ bodyWrapper bwconf $ outgoingPage pid hostsEvents

outgoingPage :: Projects.ProjectId -> V.Vector Endpoints.HostEvents -> Html ()
outgoingPage pid hostsEvents = div_ [class_ "w-full mx-auto px-16 pt-10 pb-24  overflow-y-scroll h-full"] $ do
  h3_ [class_ "text-xl text-slate-700 flex place-items-center"] "Outbound Integrations"
  div_ [class_ "mt-8 mx-auto space-y-4 max-w-[1000px]"] do
    div_ [class_ "flex px-8 w-full justify-between"] do
      h3_ [class_ "font-bold"] "Host"
      h3_ [class_ "font-bold"] "Events"
    div_ [class_ "flex flex-col"] do
      forM_ hostsEvents $ \host -> do
        div_ [class_ "flex border border-t-transparent items-center"] do
          a_ [href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host, class_ "flex  w-full justify-between items-center p-8"] $ do
            span_ [class_ "p-2", href_ $ "/p/" <> pid.toText <> "/endpoints?host=" <> host.host] $ toHtml (T.replace "http://" "" $ T.replace "https://" "" host.host)
          -- div_ [class_ "w-[200px] h-[80px] mt-4 shrink-0"] do
          -- Charts.throughput pid host.host (Just (QBHost host.host)) Nothing 14 Nothing False (Nothing, Nothing) Nothing
          div_ [class_ "shrink-0 flex items-center gap-10 p-8"] do
            a_ [href_ $ "/p/" <> pid.toText <> "/log_explorer?query=host%3D%3D" <> "\"" <> host.host <> "\"", class_ "p-2 shrink-0 pl-8 text-blue-500 hover:text-slate-600"]
              $ "View logs"
              >> faIcon_ "fa-arrow-up-right" "fa-solid fa-arrow-up-right" "h-3 w-3"
            span_ [] $ show host.eventCount
    when (null hostsEvents) $ div_ [class_ "flex flex-col text-center justify-center items-center"] $ do
      strong_ [class_ "text-3xl mb-1"] "No dependencies yet."
      p_ [class_ "text-lg mb-2"] "Start monitoring your outbound integrations"
      monitorOutgoingRequestDemos

monitorOutgoingRequestDemos :: Html ()
monitorOutgoingRequestDemos =
  div_ [class_ "w-full mx-auto text-left rounded-lg border mb-10 overflow-hidden"] do
    div_ [class_ "w-full p-8 bg-gray-50"] do
      div_ [class_ "pb-2 flex gap-10 w-full mt-8"] do
        div_ [class_ "font-bold text-center w-[35%]"] $ do
          tabs "express"
        div_ [class_ "flex flex-col w-[65%] shrink-0 max-w-[65%]"] do
          outgoingContentAxios "" "express"
          outgoingContentAdonis "" "express"
          outgoingGuzzleLaravel "" "express"
          outgoingContentDjango "" "express"
          outgoingContentFlask "" "express"
          outgoingContentFastAPI "" "express"
          outgoingContentFastify "" "express"
          outgoingContentGin "" "express"
      div_ [class_ "font-medium text-slate-700 mt-8 space-y-2 text-xl"] do
        p_ [class_ "space-x-3"] do
          a_ [class_ "block link underline text-slate-900 underline-offset-4", href_ "https://apitoolkit.io/docs/get-started/quickstarts", target_ "BLANK"] "View Integration Quickstarts &  documentation on our Knowlege base"
    script_
      [text|
      hljs.highlightAll();
    |]

outgoingContentAxios :: Text -> Text -> Html ()
outgoingContentAxios apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "express" then "" else "hidden"), id_ "express_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "npm install apitoolkit-express"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "express_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs atom-one-dark", id_ "express_code"]
                  $ toHtml
                  $ "import express from \"express\";\n"
                  <> "import { APIToolkit, observeAxios } from \"apitoolkit-express\";\n"
                  <> "import axios from \"axios\";\n"
                  <> "const app = express();\n\n"
                  <> "const apitoolkitClient = APIToolkit.NewClient({\n"
                  <> "  apiKey: \""
                  <> apikey
                  <> "\",\n"
                  <> "});\n\n"
                  <> "app.use(apitoolkitClient.expressMiddleware);\n\n"
                  <> "app.get(\"/\", async (req, res) => {\n"
                  <> "  const response = await observeAxios(axios).get(\n"
                  <> "    \"https://jsonplaceholder.typicode.com/posts/1\"\n"
                  <> "  );\n"
                  <> "  res.json(response.data);\n"
                  <> "});\n\n"
                  <> "app.listen(3000, () => {\n"
                  <> "  console.log(`Example app listening on port ${port}`);\n"
                  <> "});\n"

outgoingGuzzleLaravel :: Text -> Text -> Html ()
outgoingGuzzleLaravel apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "laravel" then "" else "hidden"), id_ "laravel_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "composer require apitoolkit/apitoolkit-laravel"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "laravel_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs atom-one-dark", id_ "laravel_code"]
                  $ toHtml
                  $ "use Illuminate\\Http\\Request;\n"
                  <> "use Illuminate\\Support\\Facades\\Route;\n"
                  <> "use APIToolkit\\APIToolkitLaravel;\n\n"
                  <> "Route::get('/user', function (Request $request) {\n"
                  <> "    $options = [\n"
                  <> "        \"pathPattern\" => \"/repos/{owner}/{repo}\", # For observing Requests with Path Params\n"
                  <> "        \"redactHeaders\" => [\"Server\"], # headers redaction\n"
                  <> "        \"redactRequestBody\" => [\"$.password\"],\n"
                  <> "        \"redactResponseBody\" => [\"$.password\"]\n"
                  <> "    ];\n"
                  <> "    $guzzleClient = APIToolkitLaravel::observeGuzzle($request, $options);\n"
                  <> "    $responseFromGuzzle = $guzzleClient->request('GET', 'https://api.github.com/repos/guzzle/guzzle?foobar=123');\n"
                  <> "    $response = $responseFromGuzzle->getBody()->getContents();\n\n"
                  <> "    return $response;\n"
                  <> "});\n"

outgoingContentAdonis :: Text -> Text -> Html ()
outgoingContentAdonis apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "adonis" then "" else "hidden"), id_ "adonis_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "npm install apitoolkit-adonis"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "adonis_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-js atom-one-dark", id_ "adonis_code"]
                  $ "import Route from \"@ioc:Adonis/Core/Route\";\n"
                  <> "import { observeAxios } from \"apitoolkit-adonis\";\n"
                  <> "import axios from \"axios\";\n\n"
                  <> "const redactHeadersList = [\"Content-Type\", \"Authorization\"];\n"
                  <> "const redactRequestBodyList = [\"$.body.user.name\"];\n\n"
                  <> "Route.get(\"/observer\", async () => {\n"
                  <> "  const response = await observeAxios(\n"
                  <> "    axios,\n"
                  <> "    \"/users/{user_id}\",\n"
                  <> "    redactHeadersList,\n"
                  <> "    redactRequestBodyList\n"
                  <> "  ).get(`${baseURL}/users/11`);\n\n"
                  <> "  return response.data;\n"
                  <> "});\n"

outgoingContentDjango :: Text -> Text -> Html ()
outgoingContentDjango apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "django" then "" else "hidden"), id_ "django_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-django"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "express_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark", id_ "django_code"]
                  $ "from django.http import JsonResponse\n"
                  <> "from apitoolkit_django import observe_request, report_error\n\n"
                  <> "def hello_world(request, name):\n"
                  <> "    resp = observe_request(request).get(\n"
                  <> "        \"https://jsonplaceholder.typicode.com/todos/2\")\n"
                  <> "    resp.read()\n"
                  <> "    return JsonResponse({\"data\": resp.read()})\n"

outgoingContentFlask :: Text -> Text -> Html ()
outgoingContentFlask apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "flask" then "" else "hidden"), id_ "flask_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-flask"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "flask_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark", id_ "flask_code"]
                  $ "from flask import Flask, request\n"
                  <> "from apitoolkit_flask import observe_request\n\n"
                  <> "app = Flask(__name__)\n\n"
                  <> "@app.route('/sample/', methods=['GET', 'POST'])\n"
                  <> "async def sample_route(subject):\n"
                  <> "    # Observe the request and send it to the APIToolkit server\n"
                  <> "    resp = observe_request(request).get(\"https://jsonplaceholder.typicode.com/todos/2\")\n"
                  <> "    return resp.read()\n"

outgoingContentFastAPI :: Text -> Text -> Html ()
outgoingContentFastAPI apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "fastapi" then "" else "hidden"), id_ "fastapi_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-fastapi"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "fastapi_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark", id_ "fastapi_code"]
                  $ "from fastapi import FastAPI, Request\n"
                  <> "from apitoolkit_fastapi import observe_request, report_error\n\n"
                  <> "app = FastAPI()\n\n"
                  <> "@app.get('/sample/{subject}')\n"
                  <> "async def sample_route(subject: str, request: Request):\n"
                  <> "    # Observe the request and send it to the APIToolkit server\n"
                  <> "    resp = observe_request(request).get(\"https://jsonplaceholder.typicode.com/todos/2\")\n"
                  <> "    return resp.read()"

outgoingContentFastify :: Text -> Text -> Html ()
outgoingContentFastify apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "fastify" then "" else "hidden"), id_ "fastify_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "npm install apitoolkit-fastify"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "fastify_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark", id_ "fastify_code"]
                  $ "import APIToolkit, { observeAxios } from \"apitoolkit-fastify\";\n"
                  <> "import axios from \"axios\";\n"
                  <> "import Fastify from \"fastify\";\n\n"
                  <> "const fastify = Fastify();\n\n"
                  <> "const apitoolkitClient = APIToolkit.NewClient({\n"
                  <> "  apiKey: \"<Your_API_KEY>\",\n"
                  <> "  fastify,\n"
                  <> "});\n"
                  <> "apitoolkitClient.init();\n\n"
                  <> "fastify.get(\"/\", async (request, reply) => {\n"
                  <> "  const res = await observeAxios(axios, \"/todos/{todo_id}\").get(\n"
                  <> "    \"https://jsonplaceholder.typicode.com/todos/1\"\n"
                  <> "  );\n"
                  <> "  return reply.send(res.data);\n"
                  <> "});"

outgoingContentGin :: Text -> Text -> Html ()
outgoingContentGin apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col " <> (if current_tab == "gin" then "" else "hidden"), id_ "gin_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "go get github.com/apitoolkit/apitoolkit-go"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[28rem] max-h-[0vh]] sm:rounded-xl lg:h-[28rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            Onboarding.contentHeader "gin_code"
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto language-go hljs atom-one-dark", id_ "gin_code"]
                  $ "package main\n\n"
                  <> "import (\n"
                  <> "    \"context\"\n"
                  <> "    \"net/http\"\n\n"
                  <> "    apitoolkit \"github.com/apitoolkit/apitoolkit-go\"\n"
                  <> "    \"github.com/gin-gonic/gin\"\n"
                  <> ")\n\n"
                  <> "func main() {\n\n"
                  <> "    apitoolkitClient, err := apitoolkit.NewClient(context.Background(), apitoolkit.Config{APIKey: \"\"})\n"
                  <> "    if err != nil {\n"
                  <> "        panic(err)\n"
                  <> "    }\n\n"
                  <> "    router := gin.New()\n\n"
                  <> "    router.Use(apitoolkitClient.GinMiddleware)\n\n"
                  <> "    router.POST(\"/:slug/test\", func(c *gin.Context) {\n"
                  <> "        // Create a new HTTP client\n"
                  <> "        HTTPClient := http.DefaultClient\n\n"
                  <> "        // Replace the transport with the custom roundtripper\n"
                  <> "        HTTPClient.Transport = apitoolkitClient.WrapRoundTripper(\n"
                  <> "            c.Request().Context(),\n"
                  <> "            HTTPClient.Transport,\n"
                  <> "            WithRedactHeaders([]string{}),\n"
                  <> "        )\n\n"
                  <> "        // Make an outgoing HTTP request using the modified HTTPClient\n"
                  <> "        _, _ = HTTPClient.Get(\"https://jsonplaceholder.typicode.com/posts/1\")\n\n"
                  <> "        // Respond to the request\n"
                  <> "        c.JSON(http.StatusOK, gin.H{\n"
                  <> "            \"message\": \"pong\",\n"
                  <> "        })\n"
                  <> "    })\n"
                  <> "}"

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
        [ class_ $ if current_tab == "express" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #express_content) |],
          id_ "express"
        ]
        do
          img_ [src_ "/assets/framework-logos/express-logo.png", alt_ "Express Js", class_ "w-full"]
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ $ if current_tab == "adonis" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #adonis_content) |],
          id_ "adonis"
        ]
        do
          img_ [src_ "/assets/framework-logos/adonis-logo.png", alt_ "adonis", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "gin" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #gin_content) |],
          id_ "gin"
        ]
        do
          img_ [src_ "/assets/framework-logos/gin-logo.png", alt_ "Gin", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "laravel" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #laravel_content) |],
          id_ "laravel"
        ]
        do
          img_ [src_ "/assets/framework-logos/laravel-logo.png", alt_ "", class_ "w-full"]

    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "flask" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #flask_content) |],
          id_ "flask"
        ]
        do
          img_ [src_ "/assets/framework-logos/flask-logo.png", alt_ "", class_ "w-full"]

    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "fastapi" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #fastapi_content) |],
          id_ "fastapi"
        ]
        do
          img_ [src_ "/assets/framework-logos/fastapi-logo.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "django" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #django_content) |],
          id_ "django"
        ]
        do
          img_ [src_ "/assets/framework-logos/django-logo.png", alt_ "", class_ "w-full"]
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "fastify" then "sdk_tab sdk_tab_active" else "sdk_tab",
          [__| install Navigatable(content: #fastify_content) |],
          id_ "fastify"
        ]
        do
          img_ [src_ "/assets/framework-logos/fastify-logo.png", alt_ "", class_ "w-full"]
