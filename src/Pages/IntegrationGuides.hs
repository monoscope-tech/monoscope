module Pages.IntegrationGuides (getH, IntegrationsGet (..)) where

import Data.Default (Default (def))
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid
import Lucid.Hyperscript (__)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper (
  BWConfig (currProject, pageTitle, sessM),
  PageCtx (..),
 )
import Pages.IntegrationDemos.AdonisJS (adonisGuide)
import Pages.IntegrationDemos.Django (djangoGuide)
import Pages.IntegrationDemos.DotNet (dotNetGuide)
import Pages.IntegrationDemos.Echo (echoGuide)
import Pages.IntegrationDemos.FastApi (fastApiGuide)
import Pages.IntegrationDemos.Flask (flaskGuide)
import Pages.IntegrationDemos.Gin (ginGuide)
import Pages.IntegrationDemos.GoNative (goNativeGuide)
import Pages.IntegrationDemos.GorillaMux (gorillaGuide)
import Pages.IntegrationDemos.Javascript (expressGuide, fastifyGuide)
import Pages.IntegrationDemos.Laravel (laravelGuide)
import Pages.IntegrationDemos.NestJs (nestGuide)
import Pages.IntegrationDemos.Phoenix (phoenixGuide)
import Pages.IntegrationDemos.Pyramid (pyramidGuide)
import Pages.IntegrationDemos.Slim (slimGuide)
import Pages.IntegrationDemos.Springboot (springGuide)
import Pages.IntegrationDemos.Symfony (symfonyGuide)
import Relude hiding (ask)
import System.Types
import Utils (faSprite_)


getH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders IntegrationsGet)
getH pid sdkM errReportM reqMonM = do
  (sess, project) <- Sessions.sessionAndProject pid
  apiKey <- dbtToEff $ ProjectApiKeys.projectApiKeysByProjectId pid
  let key = if V.length apiKey > 0 then let defKey = V.head apiKey in defKey.keyPrefix else "<API_KEY>"
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Integrations"
          }
  addRespHeaders $ IntegrationsGet $ PageCtx bwconf (pid, fromMaybe "express" sdkM, key, errReportM, reqMonM)


newtype IntegrationsGet = IntegrationsGet (PageCtx (Projects.ProjectId, Text, Text, Maybe Text, Maybe Text))


instance ToHtml IntegrationsGet where
  toHtml (IntegrationsGet (PageCtx bwconf (pid, sdk, apiKey, errReportM, reqMonM))) = toHtml $ PageCtx bwconf $ integrationsPage pid sdk apiKey errReportM reqMonM
  toHtmlRaw = toHtml


integrationsPage :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Maybe Text -> Html ()
integrationsPage pid sdk apiKey errReportM reqMonM = do
  let baseUrl = "/p/" <> pid.toText <> "/integration_guides?" <> maybe "" ("error_reporting=" <>) errReportM <> maybe "" ("&outgoing=" <>) reqMonM
  main_ [class_ "w-full h-full overflow-y-scroll scroll-smooth", id_ "main"] do
    div_ [class_ "flex flex-col gap-6 border-b  m-8 pb-4 sticky top-[-40px] bg-base-100 z-50"] do
      div_ [class_ "flex justify-between items-center"] do
        h3_ [class_ "text-3xl font-medium capitalize"] $ toHtml $ "Configure " <> sdk <> " SDK"
        div_ [class_ "flex items-center gap-4"] do
          a_ [href_ "https://apitoolkit.io/docs/get-started/quickstarts/", target_ "_BLANK", class_ "rounded-lg border px-4 py-2"] "See Full Documentation"
      div_ [class_ "flex items-center gap-4"] do
        div_ [class_ "relative"] do
          button_ [class_ "border flex items-center justify-between border-blue-500 w-36 rounded-lg px-2 py-1.5  font-medium", [__|on click toggle .hidden on #sdk_list|]] do
            span_ [class_ "b"] $ toHtml $ getTitle sdk
            span_ [] do
              faSprite_ "chevron-down" "regular" "h-3 w-3"
          div_ [class_ "hidden w-full flex flex-col left-0 absolute shadow top-8 bg-base-100  rounded", id_ "sdk_list"] do
            -- .NET
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=dotnet"] ".NET"
            -- Elixir
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=phoenix"] "Elixir Phoenix"
            -- Go
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=echo"] "Go Echo"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=gin"] "Go Gin"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=gorilla"] "Go Gorilla Mux"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=gonative"] "Go Native"
            -- Java
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=springboot"] "Java Springboot"
            -- JavaScript
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=adonis"] "Adonis Js"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=express"] "ExpressJs"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=fastify"] "Fastify Js"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=nest"] "Nest Js"
            -- PHP
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=laravel"] "Laravel"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=slim"] "PHP Slim"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=symfony"] "PHP Symfony"
            -- Python
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=django"] "Django"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=flask"] "Flask"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=fastapi"] "Python FastAPI"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=pyramid"] "Python Pyramid"

        button_
          [ class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium  hover:bg-gray-100"
          , [__|on click go to the top of #requests-monitoring|]
          ]
          "Request monitoring"
        button_
          [ class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium  hover:bg-gray-100"
          , [__|on click go to the top of #errors-monitoring|]
          ]
          "Error Reporting"
        button_
          [ class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium  hover:bg-gray-100"
          , [__|on click go to the top of #outgoing-request-monitoring|]
          ]
          "Outgoing request monitoring"
    div_ [class_ "px-8 mb-10"] do
      case sdk of
        "gin" -> ginGuide apiKey
        "pyramid" -> pyramidGuide apiKey
        "dotnet" -> dotNetGuide apiKey
        "adonis" -> adonisGuide apiKey
        "laravel" -> laravelGuide apiKey
        "django" -> djangoGuide apiKey
        "phoenix" -> phoenixGuide apiKey
        "flask" -> flaskGuide apiKey
        "echo" -> echoGuide apiKey
        "symfony" -> symfonyGuide apiKey
        "fastapi" -> fastApiGuide apiKey
        "fastify" -> fastifyGuide apiKey
        "slim" -> slimGuide apiKey
        "nest" -> nestGuide apiKey
        "gorilla" -> gorillaGuide apiKey
        "gonative" -> goNativeGuide apiKey
        "springboot" -> springGuide apiKey
        _ -> expressGuide apiKey
    script_
      [text|
hljs.highlightAll();
 |]


getTitle :: Text -> Text
getTitle "gin" = "GO Gin"
getTitle "pyramid" = "Python Pyramid"
getTitle "dotnet" = "CSharp .NET"
getTitle "adonis" = "Adonis Js"
getTitle "laravel" = "PHP Laravel"
getTitle "django" = "Python Django"
getTitle "phoenix" = "Elixir Phoenix"
getTitle "flask" = "Python Flask"
getTitle "echo" = "Go Echo"
getTitle "symfony" = "PHP Symfony"
getTitle "fastapi" = "Python FastAPI"
getTitle "fastify" = "Fastify JS"
getTitle "slim" = "PHP Slim"
getTitle "nest" = "Nest Js"
getTitle "gorilla" = "Go Gorilla Mux"
getTitle "gonative" = "Go Native"
getTitle "springboot" = "Java Springboot"
getTitle _ = "Express Js"
