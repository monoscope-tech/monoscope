module Pages.IntegrationGuides (getH) where

import Data.Default
import Data.Text
import Data.Vector qualified as V
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
  ( Html,
    ToHtml (toHtml),
    a_,
    button_,
    class_,
    div_,
    h3_,
    href_,
    id_,
    main_,
    script_,
    span_,
    target_,
  )
import Lucid.Hyperscript
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Pages.BodyWrapper
  ( BWConfig (currProject, pageTitle, sessM),
    bodyWrapper,
  )
import Pages.IntegrationDemos.AdonisJS
import Pages.IntegrationDemos.Django
import Pages.IntegrationDemos.DotNet
import Pages.IntegrationDemos.Echo
import Pages.IntegrationDemos.ExpressJs
import Pages.IntegrationDemos.FastApi
import Pages.IntegrationDemos.Flask
import Pages.IntegrationDemos.Gin
import Pages.IntegrationDemos.Laravel
import Pages.IntegrationDemos.Phoenix
import Pages.IntegrationDemos.Pyramid
import Pages.IntegrationDemos.Symfony
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext)
import System.Types (ATAuthCtx)
import Utils

getH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (Html ())
getH pid sdkM errReportM reqMonM = do
  appCtx <- ask @AuthContext
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession
  (apiKey, project) <- dbtToEff $ do
    apiKey <- ProjectApiKeys.projectApiKeysByProjectId pid
    project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
    let key = if V.length apiKey > 0 then let defKey = V.head apiKey in defKey.keyPrefix else "<API_KEY>"
    pure (key, project)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Integrations"
          }
  pure $ bodyWrapper bwconf $ integrationsPage pid (fromMaybe "express" sdkM) apiKey errReportM reqMonM

integrationsPage :: Projects.ProjectId -> Text -> Text -> Maybe Text -> Maybe Text -> Html ()
integrationsPage pid sdk apiKey errReportM reqMonM = do
  let baseUrl = "/p/" <> pid.toText <> "/integration_guides?" <> maybe "" ("error_reporting=" <>) errReportM <> maybe "" (\v -> "&outgoing=" <> v) reqMonM
  main_ [class_ "w-full h-full overflow-y-scroll scroll-smooth", id_ "main"] do
    div_ [class_ "flex flex-col gap-6 border-b  m-8 pb-4 sticky top-[-40px] bg-white z-50"] do
      div_ [class_ "flex justify-between items-center"] do
        h3_ [class_ "text-3xl font-medium capitalize"] $ toHtml $ "Configure " <> sdk <> " SDK"
        div_ [class_ "flex items-center gap-4"] do
          a_ [href_ "https://apitoolkit.io/docs/get-started/quickstarts/", target_ "_BLANK", class_ "rounded-lg border px-4 py-2"] "See Full Documentation"
      div_ [class_ "flex items-center gap-4"] do
        div_ [class_ "relative"] do
          button_ [class_ "border flex items-center justify-between border-blue-500 w-36 rounded-lg px-2 py-1.5 text-sm font-medium", [__|on click toggle .hidden on #sdk_list|]] do
            span_ [class_ "b"] $ toHtml $ getTitle sdk
            span_ [] do
              faIcon_ "fa fa-solid fa-angle-down" "fa fa-solid fa-angle-down" "h-3 w-3"
          div_ [class_ "hidden w-full flex flex-col left-0 absolute shadow top-8 bg-white text-sm rounded", id_ "sdk_list"] do
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=express"] "ExpressJs"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=gin"] "Go Gin"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=pyramid"] "Python Pyramid"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=dotnet"] ".NET"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=laravel"] "Laravel"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=django"] "Django"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=adonis"] "Adonis Js"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=flask"] "Flask"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=echo"] "Go Echo"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=phoenix"] "Elixir Phoenix"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=symfony"] "PHP Symfony"
            a_ [class_ "px-2 py-1 hover:bg-gray-200", href_ $ baseUrl <> "sdk=fastapi"] "Python FastAPI"
        button_
          [ class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium text-sm hover:bg-gray-100",
            [__|on click go to the top of #requests-monitoring|]
          ]
          "Request monitoring"
        button_
          [ class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium text-sm hover:bg-gray-100",
            [__|on click go to the top of #errors-monitoring|]
          ]
          "Error Reporting"
        button_
          [ class_ "rounded-lg flex items-center gap-2 border px-4 py-1.5 font-medium text-sm hover:bg-gray-100",
            [__|on click go to the top of #outgoing-request-monitoring|]
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
getTitle _ = "Express Js"
