{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Onboarding (onboardingGetH) where

import Config
import Data.Default (def)
import Data.Text qualified as T
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx (hxGet_, hxPost_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projectjs
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.NonMember
import Relude
import Utils (
  faIcon_,
  redirect,
  userIsProjectMember,
 )


onboardingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Bool -> Maybe Bool -> Maybe Text -> DashboardM (Html ())
onboardingGetH sess pid polling redirected current_tab = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, apikey, hasRequest) <- liftIO
        $ withPool
          pool
          do
            project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
            apiKeys <- ProjectApiKeys.projectApiKeysByProjectId pid
            requestDumps <- RequestDumps.countRequestDumpByProject pid
            let apikey = if V.null apiKeys then "<APIKEY>" else (V.head apiKeys).keyPrefix
            pure (project, apikey, requestDumps > 0)
      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Get started"
              , hasIntegrated = Just hasRequest
              }
      let ans = case project of
            Nothing -> False
            Just p -> case p.questions of
              Just v -> True
              _ -> False
      case polling of
        Just _ -> pure $ onboardingPage pid apikey hasRequest ans (fromMaybe False redirected) (fromMaybe "express" current_tab)
        Nothing -> pure $ bodyWrapper bwconf $ onboardingPage pid apikey hasRequest ans (fromMaybe False redirected) "express"


onboardingPage :: Projects.ProjectId -> Text -> Bool -> Bool -> Bool -> Text -> Html ()
onboardingPage pid apikey hasRequest ans redi ctb = do
  div_
    [ class_ "relative h-full"
    , hxGet_ $ "/p/" <> pid.toText <> "/onboarding?polling=True"
    , hxTrigger_ "load delay:30s"
    , hxVals_ "js:{current_tab:getCurrentTab()}"
    , hxSwap_ "outerHTML"
    ]
    $ do
      when redi $ div_ [class_ "w-full text-center py-2 bg-yellow-500"] "You have to integrate APIToolkit in your app before you can start using the platform"
      div_ [class_ "flex flex-col h-full w-full gap-16"] $ do
        div_ [class_ "text- center"] do
          div_ [class_ "flex flex-col w-full mt-10 py-4 items-center gap-4"] $ do
            h3_ [class_ "text-4xl font-bold"] "Ready, Set, Integrate!"
            div_ [class_ "flex flex-col text-center gap-1 mb-4"] do
              div_ [class_ " text-lg  max-w-prose"] do
                p_ "To get the most from APIToolkit, integrate it into your project (Even just on your local or development machine). You can integrate an outgoing HTTP request client, or an entire server with incoming requests. "
              p_ [class_ " text-lg"] "Finish up, then dash to your dashboard! 🚀"
            if hasRequest then completedBanner pid else integrateApiToolkit apikey ctb
        div_ [class_ "w-full flex justify-center"] $ do
          div_ [class_ "flex flex-col w-[800px] rounded-2xl border border-2"] $ do
            div_ [class_ "w-full px-8 py-4 flex justify-between border-b border-b-2"] $ do
              h4_ [class_ "font-bold text-lg"] "Onboarding checklist"
              let p
                    | hasRequest = "100%"
                    | otherwise = "66%"
              span_ [class_ "text-slate-500"] $ p <> " completed"
            ul_ [class_ "px-3 py-4"] do
              li_ [class_ "flex items-center mx-4 py-4 border-b gap-6 text-green"] do
                faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" "h-6 w-6 text-green-700"
                button_ [class_ "flex flex-col"] do
                  p_ [class_ "font-semibold"] "Create an account"
                  span_ [class_ "text-slate-500"] "This is completed when you sign up"
              li_ [class_ "flex flex-col items-center mx-4 py-4 border-b gap-6 text-green"] do
                div_ [class_ "flex w-full items-center gap-6"] do
                  faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" "h-6 w-6 text-green-700"
                  button_
                    [ class_ "flex justify-between text-left w-full items-center"
                    , [__|on click toggle .hidden on #addAPIKey|]
                    ]
                    do
                      div_ [class_ "flex flex-col"] do
                        p_ [class_ "font-semibold"] "Generate an API key"
                        span_ [class_ "text-slate-500"] "The API key is used to authenticate requests, Auto generated."
                      faIcon_ "fa-chevron-down" "fa-regular fa-chevron-down" "h-6 w-6"
                div_ [class_ "bg-slate-100 hidden w-full py-16 px-24", id_ "addAPIKey"] do
                  p_ [class_ "text-green-500 text-center"] $ toHtml apikey

              li_ [class_ "mx-4 py-4 border-b"] do
                div_ [class_ "flex w-full items-center  gap-6"] do
                  let style = if hasRequest then "text-green-700" else "text-gray-400 "
                  faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" $ "h-6 w-6 " <> style
                  button_ [class_ "flex justify-between text-left w-full items-center", [__|on click toggle .hidden on #SDKs|]] do
                    div_ [class_ "flex flex-col"] do
                      p_ [class_ "font-semibold"] "Integrate APIToolkit to your app"
                      span_ [class_ "text-slate-500"] "Integrate apitoolkit using any of our SDKs to start sending request."
                    faIcon_ "fa-chevron-down" "fa-regular fa-chevron-down" "h-6 w-6"
                div_ [class_ "w-full bg-slate-100 mt-8", id_ "SDKs"] do
                  if hasRequest
                    then do
                      p_ [class_ "text-green-500 text-center py-16 text-center"]
                        $ span_ "Apitoolkit has been integrated into your app"
                    else do
                      div_ [class_ "font-medium text-lg text-center border-b border-slate-200 py-16 space-y-2"] $ do
                        a_ [class_ "block link underline text-slate-900 underline-offset-4", href_ "https://apitoolkit.io/docs/quickstarts/", target_ "BLANK"] "View Integration Quickstarts &  documentation at our Knowledge base."
                        span_ [class_ "block text-slate-900  space-x-2"] do
                          span_ "Need more help?"
                          a_ [class_ "link underline underline-offset-4", href_ "https://calendar.app.google/EvPzCoVsLh5gqkAo8", target_ "BLANK"] "Schedule a call with an Engineer."
                        div_ [class_ " inline-block space-x-3 text-red-800 pt-5"] do
                          faIcon_ "fa-spinner" "fa-sharp fa-light fa-spinner " "fa-spin h-6 w-6 inline-block "
                          span_ "Waiting to recieve data from your server."

        div_ [class_ "w-full flex justify-center pb-16 mt-16"] $ do
          div_ [class_ "flex flex-col w-[800px] rounded-2xl border border-2 grid grid-cols-2 border-b "] $ do
            a_ [class_ "flex flex-col gap-2 py-8 border-r px-8 hover:bg-blue-100 ", href_ "https://www.apitoolkit.io/docs", target_ "_BLANK"] do
              faIcon_ "fa-file-lines" "fa-thin fa-file-lines" "h-8 w-8"
              h3_ [class_ "font-bold text-lg"] "Documentation"
              p_ [class_ "text-slate-700"] "Check out our documentation to learn more about using APIToolkit."
              span_ [href_ "https://www.apitoolkit.io/docs", class_ "text-blue-500 flex items-center gap-2"] do
                faIcon_ "fa-link-simple" "fa-sharp fa-regular fa-link-simple" "h-8 w-8 text-blue-500"
                "Read the docs"
            a_ [class_ "block px-8 py-16 flex items-center gap-6 border-l hover:bg-blue-100 ", href_ "https://calendar.app.google/EvPzCoVsLh5gqkAo8", target_ "_BLANK"] do
              faIcon_ "fa-circle-play" "fa-light fa-circle-play" "text-blue-500 h-14 w-14"
              div_ [class_ "flex flex-col"] do
                span_ [class_ "font-bold text-lg text-blue-700 space-x-3"] do
                  span_ "Need Help?"
                  span_ "Or a Demo?"
                span_ [class_ "text-slate-500"] "Schedule a brief call with an Engineer."
  script_
    [text|

var getCurrentTab = () => {
  const tab= document.querySelector(".sdk_tab_active")
  if(tab) {
    console.log(tab)
    return tab.id
    }
  return 'express'
}
hljs.highlightAll();
  |]


generateApikey :: Projects.ProjectId -> Html ()
generateApikey pid =
  div_ [class_ "w-[800px] bg-slate-200 mx-auto rounded-lg border-8 border-white shadow-lg mb-10"] do
    div_ [class_ "w-full p-8"] do
      div_ [class_ "flex w-full justify-center gap-4 items-center mb-10"] do
        span_ [class_ "text-blue-500 pr-4 border-r border-r-2 border-r-blue-500 text-2xl"] "Next Up"
        h3_ [class_ "font-bold text-2xl"] "Generate API Key"
      div_ [id_ "main-content2"] do
        form_
          [ hxPost_ $ "/p/" <> pid.toText <> "/apis"
          , class_ "flex items-end justify-center mx-8  pt-4 px-4 pb-20 text-center sm:block sm:p-0"
          , hxTarget_ "#main-content2"
          ]
          do
            div_ [class_ "bg-white rounded-lg px-4 pt-5 pb-4 text-left"] do
              div_ [class_ "sm:flex sm:items-start"] do
                div_ [class_ "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left grow"] do
                  h3_ [class_ "text-lg font-medium text-slate-900", id_ "modal-title"] "Enter API key title"
                  div_ [class_ "mt-2 space-y-2"] do
                    p_ [class_ "text-sm text-slate-500"] do
                      "Please input a title for your API Key. You can find all API keys "
                      a_ [href_ $ "/p/" <> pid.toText <> "/apis", class_ "text-blue-500"] "here"
                    div_ do
                      input_ [class_ "input-txt px-4 py-2  border w-full", type_ "text", placeholder_ "API Key Title", name_ "title", autofocus_]
                      input_ [hidden_ "true", name_ "from", value_ "onboarding"]
              div_ [class_ "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse"] do
                button_ [type_ "submit", class_ "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-blue-600 text-base font-medium text-white hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 sm:ml-3 sm:w-auto sm:text-sm"] "Submit"


integrateApiToolkit :: Text -> Text -> Html ()
integrateApiToolkit apikey current_tab =
  div_ [class_ "w-[800px] bg-slate-200 mx-auto rounded-lg border-8 border-white shadow-lg mb-10"] do
    div_ [class_ "w-full p-8 bg-slate-100  rounded"] do
      div_ [class_ "flex w-full justify-center gap-4 items-center mb-2"] do
        span_ [class_ "text-blue-500 pr-4 border-r border-r-2 border-r-blue-500 font-bold"] "Next Up"
        h3_ [class_ "font-bold text-2xl"] "Integrate APIToolkit"
      div_ [class_ "pb-2 mt-5"] do
        div_ [class_ "font-bold text-center text-white border-b border-slate-200"] $ do
          tabs current_tab
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
      div_ [class_ "font-medium text-slate-700 mt-8 space-y-2 text-xl"] do
        p_ [class_ "space-x-3"] do
          span_ [class_ ""] "Having trouble integrating APIToolkit?"
          a_ [href_ "https://calendar.app.google/EvPzCoVsLh5gqkAo8", target_ "_BLANK", class_ "text-blue-500"] "Contact support"
        a_ [class_ "block link underline text-slate-900 underline-offset-4", href_ "https://apitoolkit.io/docs/quickstarts/", target_ "BLANK"] "View Integration Quickstarts &  documentation on our Knowlege base"


completedBanner :: Projectjs.ProjectId -> Html ()
completedBanner pid =
  div_ [class_ "w-[800px] bg-slate-200 mx-auto rounded-lg border-8 border-white shadow-lg mb-10"] do
    div_ [class_ "w-full px-8 py-12 bg-slate-100  rounded"] do
      div_ [class_ "flex w-full justify-center gap-4 items-center mb-2"] do
        span_ [class_ "text-blue-500 pr-4 border-r border-r-2 border-r-blue-500 text-2xl"] "Done"
        h3_ [class_ "font-bold text-2xl"] "Onboarding Completed"
      div_ [class_ "pb-2 flex items-center mt-8 flex-col gap-4 text-blue-500 font-medium"] do
        a_ [href_ $ "/p/" <> pid.toText <> "/"] "Go to the dashboard"
        faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" "h-24 w-24 text-green-700"


tabContentExpress :: Text -> Text -> Html ()
tabContentExpress apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "express" then "" else "hidden"), id_ "express_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6 space-x-3"] do
        strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
        a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-express", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-express"
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "npm i apitoolkit-express"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
            pre_ [class_ "flex min-h-full text-lg leading-snug"] do
              code_ [class_ "h-full hljs language-javascript atom-one-dark"]
                $ toHtml
                $ "import express from 'express';\n"
                <> "import APIToolkit from 'apitoolkit-express';\n"
                <> "\n"
                <> "const app = express();\n"
                <> "const port = 3000;\n"
                <> "\n"
                <> "const apitoolkitClient = await APIToolkit.NewClient({ apiKey: '"
                <> apikey
                <> "' });\n"
                <> "app.use(apitoolkitClient.expressMiddleware);\n"
                <> "\n"
                <> "app.get('/', (req, res) => {\n"
                <> "   res.send('Hello World!');\n"
                <> "});\n"
                <> "\n"
                <> "app.listen(port, () => {\n"
                <> "   console.log(`Example app listening on port ${port}`);\n"
                <> "});"


tabContentGin :: Text -> Text -> Html ()
tabContentGin apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "gin" then "" else "hidden"), id_ "gin_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6 space-x-3"] do
        strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
        a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-go", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-go"
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "go get github.com/apitoolkit/apitoolkit-go"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
            pre_ [class_ "flex min-h-full text-lg leading-snug"] do
              code_
                [class_ "h-full hljs language-go atom-one-dark"]
                $ toHtml
                $ "package main\n"
                <> "\n"
                <> "import (\n"
                <> "    // Import the apitoolkit golang sdk\n"
                <> "    apitoolkit \"github.com/apitoolkit/apitoolkit-go\"\n"
                <> "    context\n"
                <> "    \"github.com/gin-gonic/gin\"\n"
                <> ")\n"
                <> "\n"
                <> "func main() {\n"
                <> "    ctx := context.Background()\n"
                <> "\n"
                <> "    // Initialize the client using your apitoolkit.io generated apikey\n"
                <> "    apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: \""
                <> apikey
                <> "\"})\n"
                <> "    if err != nil {\n"
                <> "        // Handle the error\n"
                <> "        panic(err)\n"
                <> "    }\n"
                <> "\n"
                <> "    router := gin.New()\n"
                <> "\n"
                <> "    // Register with the corresponding middleware of your choice. For Gin router, we use the GinMiddleware method.\n"
                <> "    router.Use(apitoolkitClient.GinMiddleware)\n"
                <> "\n"
                <> "    // Register your handlers as usual and run the gin server as usual.\n"
                <> "    router.POST(\":/slug/test\", func(c *gin.Context) { c.String(200, \"ok\") })\n"
                <> "\n"
                <> "    router.Run(\":8080\")\n"
                <> "}"


tabContentLaravel :: Text -> Text -> Html ()
tabContentLaravel apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "laravel" then "" else "hidden"), id_ "laravel_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6 space-x-3"] do
        strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
        a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-laravel", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-laravel"
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "composer require apitoolkit/apitoolkit-php"
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1 mt-4"] "Set up APITOOLKIT_KEY env variable"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] $ toHtml $ "APITOOLKIT_KEY=" <> apikey
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Update into your app/Http/Kernel.php"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
            pre_ [class_ "flex min-h-full text-lg leading-snug"] do
              code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-php atom-one-dark"]
                $ "<?php\n"
                <> "\n"
                <> "namespace App\\Http;\\n"
                <> "\n"
                <> "use Illuminate\\Foundation\\Http\\Kernel as HttpKernel;\n"
                <> "\n"
                <> "class Kernel extends HttpKernel\n"
                <> "    {\n"
                <> "    // ...\n"
                <> "    /**\n"
                <> "     * The application's route middleware groups.\n"
                <> "     *\n"
                <> "     * @var array\n"
                <> "     */\n"
                <> "    protected $middlewareGroups = [\n"
                <> "        // ...\n"
                <> "        'api' => [\n"
                <> "            // ...\n"
                <> "            \\APIToolkit\\Http\\Middleware\\APIToolkit::class,\n"
                <> "            // ...\n"
                <> "        ],\n"
                <> "    ];\n"
                <> "    // ...\n"
                <> "}"


tabContentSymfony :: Text -> Text -> Html ()
tabContentSymfony apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "symfony" then "" else "hidden"), id_ "symfony_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6 space-x-3"] do
        strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
        a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-symfony", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-symfony"
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "composer require apitoolkit/apitoolkit-symfony"
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1 mt-4"] "Set up APITOOLKIT_KEY env variable"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] $ toHtml $ "APITOOLKIT_KEY=" <> apikey
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0  h-full flex-auto flex flex-col"] do
            pre_ [class_ "flex min-h-full text-lg leading-snug"] do
              code_
                [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-php atom-one-dark"]
                $ "services:\n"
                <> "    APIToolkit\\EventSubscriber\\APIToolkitService:\n"
                <> "        arguments:\n"
                <> "            $apiKey: '%env(APITOOLKIT_KEY)%'\n"
                <> "        # Optional: if you want to cache login result add this cache pool instance via setter injection\n"
                <> "        calls:\n"
                <> "            - setCachePool: ['@PutYourCachePoolServiceHere']\n"
                <> "        tags:\n"
                <> "            - { name: 'kernel.event_subscriber' }"


tabContentDotNet :: Text -> Text -> Html ()
tabContentDotNet apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "net" then "" else "hidden"), id_ "net_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6 space-x-3"] do
        strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
        a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-dotnet", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-dotnet"
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "dotnet add package ApiToolkit.Net"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
            pre_ [class_ "flex min-h-full text-lg leading-snug"] do
              code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-csharp atom-one-dark"]
                $ toHtml
                $ "var config = new Config\n"
                <> "{\n"
                <> "    Debug = true, // Set debug flags to false in production\n"
                <> "    ApiKey = \""
                <> apikey
                <> "\"\n"
                <> "};\n"
                <> "var client = await APIToolkit.NewClientAsync(config);\n"
                <> "// Register the middleware to use the initialized client\n"
                <> "app.Use(async (context, next) =>\n"
                <> "    var apiToolkit = new APIToolkit(next, client);\n"
                <> "    await apiToolkit.InvokeAsync(context);\n"
                <> ");"


tabContentFastify :: Text -> Text -> Html ()
tabContentFastify apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "fastify" then "" else "hidden"), id_ "fastify_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6 space-x-3"] do
          strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
          a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-fastify", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-fastify"
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "npm install apitoolkit-fastify"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-javascript atom-one-dark"]
                  $ toHtml
                  $ "import APIToolkit from 'apitoolkit-fastify';\n"
                  <> "import Fastify from 'fastify';\n"
                  <> "const fastify = Fastify();\n"
                  <> "// Create and initialize an instance of the APIToolkit\n"
                  <> "const apittoolkitClient = await APIToolkit.NewClient({\n"
                  <> "  apiKey: \""
                  <> apikey
                  <> "\",\n"
                  <> "  fastify: fastify\n"
                  <> "});\n"
                  <> "apitoolkitClient.init();\n"
                  <> "// Rest of your app\n"
                  <> "fastify.get('/hello', (request, reply) => {\n"
                  <> " reply.send({hello:'world'})\n"
                  <> "});\n"
                  <> "\n"
                  <> "fastify.listen({port: 3000}, function(err, address) {\n"
                  <> "   if(err) {\n"
                  <> "     fastify.log.error(err);\n"
                  <> "     process.exit(1);\n"
                  <> "   }\n"
                  <> "});"


tabContentFlask :: Text -> Text -> Html ()
tabContentFlask apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "flask" then "" else "hidden"), id_ "flask_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6 space-x-3"] do
          strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
          a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-flask", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-flask"
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-flask"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark"]
                  $ toHtml
                  $ "from flask import Flask\n"
                  <> "from apitoolkit_flask import APIToolkit\n"
                  <> "\n"
                  <> "app = Flask(__name__)\n"
                  <> "\n"
                  <> "apitoolkit = APIToolkit(api_key=\""
                  <> apikey
                  <> "\", debug=True)\n"
                  <> "\n"
                  <> "@app.before_request\n"
                  <> "def before_request():\n"
                  <> "    apitoolkit.beforeRequest()\n"
                  <> "\n"
                  <> "@app.after_request\n"
                  <> "def after_request(response):\n"
                  <> "    apitoolkit.afterRequest(response)\n"
                  <> "    return response\n"
                  <> "\n"
                  <> "@app.route('/hello', methods=['GET', 'POST'])\n"
                  <> "def sample_route(subject):\n"
                  <> "    return {\"Hello\": \"World\"}\n"
                  <> "\n"
                  <> "app.run(debug=True)\n"


tabContentFastAPI :: Text -> Text -> Html ()
tabContentFastAPI apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "fastapi" then "" else "hidden"), id_ "fastapi_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6 space-x-3"] do
          strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
          a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-fastapi", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-fastapi"
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-fastapi"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] $ do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] $ do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark"]
                  $ toHtml
                  $ "from fastapi import FastAPI\n"
                  <> "from apitoolkit_fastapi import APIToolkit\n"
                  <> "\n"
                  <> "app = FastAPI()\n"
                  <> "\n"
                  <> "# A list of fields to redact from response body\n"
                  <> "redact_res = [\"$.api_key\", \"$.password\"]\n"
                  <> "# A list of fields to redact from request body\n"
                  <> "redact_req = [\"$.credit-card.cvv\", \"$.credit-card.name\"]\n"
                  <> "# A list of fields to redact from request and response headers\n"
                  <> "redact_headers = [\"Authorization\", \"Cookie\"]\n"
                  <> "\n"
                  <> "# Initialize apitoolkit\n"
                  <> "apitoolkit = APIToolkit(\n"
                  <> "    api_key=\""
                  <> apikey
                  <> "\", debug=True, redact_response_body=redact_res,\n"
                  <> "    redact_request_body=redact_req, redact_headers=redact_headers\n"
                  <> ")\n"
                  <> "\n"
                  <> "app.middleware('http')(apitoolkit.middleware)\n"
                  <> "\n"
                  <> "@app.get(\"/\")\n"
                  <> "def read_root():\n"
                  <> "    return {\"Hello\": \"World\"}\n"


tabContentDjango :: Text -> Text -> Html ()
tabContentDjango apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "django" then "" else "hidden"), id_ "django_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6 space-x-3"] do
          strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
          a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-django", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-django"

        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-django"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app by adding APITOOLKIT_KEY and APIToolkit to the settings middleware list"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[0vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-python atom-one-dark"]
                  $ toHtml
                  $ "APITOOLKIT_KEY = \""
                  <> apikey
                  <> "\"\n"
                  <> "\n"
                  <> "MIDDLEWARE = [\n"
                  <> "    ...,\n"
                  <> "    'apitoolkit-django.APIToolkit',\n"
                  <> "    ...,\n"
                  <> "]\n"
                  <> "\n"
                  <> "APITOOLKIT_REDACT_HEADERS = [\"Authorization\", \"Cookie\",\"Content-Length\", \"Content-Type\"] # optional\n"
                  <> "APITOOLKIT_REDACT_REQ_BODY = [\"$.password\", \"$.credit_card\"] # optional\n"
                  <> "APITOOLKIT_REDACT_RES_BODY = [\"$.credentials\", \"$.social_security_number\"] # optional\n"


tabContentEcho :: Text -> Text -> Html ()
tabContentEcho apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "echo" then "" else "hidden"), id_ "echo_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6 space-x-3"] do
          strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
          a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-go", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-django"

        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "go get github.com/apitoolkit/apitoolkit-go"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app by adding APITOOLKIT_KEY and APIToolkit to the settings middleware list"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[0vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-go atom-one-dark"]
                  $ toHtml
                  $ "package main\n"
                  <> "\n"
                  <> "import (\n"
                  <> "    context\n"
                  <> "    \"net/http\"\n"
                  <> "\n"
                  <> "    apitoolkit \"github.com/apitoolkit/apitoolkit-go\"\n"
                  <> "    \"github.com/labstack/echo/v4\"\n"
                  <> ")\n"
                  <> "\n"
                  <> "func main() {\n"
                  <> "    ctx := context.Background()\n"
                  <> "\n"
                  <> "    // Initialize the client using your apitoolkit.io generated apikey\n"
                  <> "    apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: \""
                  <> apikey
                  <> "\"})\n"
                  <> "    if err != nil {\n"
                  <> "        panic(err)\n"
                  <> "    }\n"
                  <> "\n"
                  <> "    e := echo.New()\n"
                  <> "\n"
                  <> "    // Register with the corresponding middleware of your choice.\n"
                  <> "    // Assuming apitoolkit provides an EchoMiddleware function for the echo framework.\n"
                  <> "    e.Use(apitoolkitClient.EchoMiddleware)\n"
                  <> "\n"
                  <> "    e.POST(\":/slug/test\", func(c echo.Context) error {\n"
                  <> "        return c.String(http.StatusOK, \"ok\")\n"
                  <> "    })\n"
                  <> "\n"
                  <> "    e.Start(\":8080\")\n"
                  <> "}"


tabContentGorilla :: Text -> Text -> Html ()
tabContentGorilla apikey current_tab =
  div_ [class_ $ "tab-content flex flex-col m-8 " <> (if current_tab == "gorilla" then "" else "hidden"), id_ "gorilla_content"]
    $ do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6 space-x-3"] do
          strong_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Repo:"
          a_ [class_ "link underline text-lg", href_ "https://github.com/apitoolkit/apitoolkit-go", target_ "BLANK"] "github.com/apitoolkit/apitoolkit-go"

        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "go get github.com/apitoolkit/apitoolkit-echo"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app by adding APITOOLKIT_KEY and APIToolkit to the settings middleware list"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[0vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 h-full flex-auto flex flex-col"] do
              pre_ [class_ "flex min-h-full text-lg leading-snug"] do
                code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto hljs language-go atom-one-dark"]
                  $ toHtml
                  $ "package main\n"
                  <> "import (\n"
                  <> "    \"context\"\n"
                  <> "    \"net/http\"\n"
                  <> "    \"github.com/gorilla/mux\"\n"
                  <> "    apitoolkit \"github.com/apitoolkit/apitoolkit-go\"\n"
                  <> ")\n"
                  <> "func main() {\n"
                  <> "    ctx := context.Background()\n"
                  <> "    // Initialize the client using your generated API key\n"
                  <> "    apitoolkitClient, err := apitoolkit.NewClient(ctx, apitoolkit.Config{APIKey: \""
                  <> apikey
                  <> "\"})\n"
                  <> "    if err != nil {\n"
                  <> "        panic(err)\n"
                  <> "    }\n"
                  <> "    r := mux.NewRouter()\n"
                  <> "    // Register middleware\n"
                  <> "    r.Use(apitoolkitClient.GorillaMuxMiddleware)\n"
                  <> "    r.HandleFunc(\"/{slug}/test\", func(w http.ResponseWriter, r *http.Request) {\n"
                  <> "        w.WriteHeader(http.StatusOK)\n"
                  <> "        w.Write([]byte(\"ok\"))\n"
                  <> "    })\n"
                  <> "    // Start the HTTP server on port 8080\n"
                  <> "    http.ListenAndServe(\":8080\", r)\n"
                  <> "}"


tabs :: Text -> Html ()
tabs current_tab =
  ul_ [class_ "flex flex-nowrap overflow-x-auto gap-6 font-medium"] $ do
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
        "Express Js"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "gin" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #gin_content) |]
        , id_ "gin"
        ]
        "Go Gin"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "laravel" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #laravel_content) |]
        , id_ "laravel"
        ]
        "Laravel"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "flask" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #flask_content) |]
        , id_ "flask"
        ]
        "Flask"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "fastapi" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #fastapi_content) |]
        , id_ "fastapi"
        ]
        "FastAPI"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "django" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #django_content) |]
        , id_ "django"
        ]
        "Django"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "gorilla" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #gorilla_content) |]
        , id_ "gorilla"
        ]
        "Go Gorilla"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "symfony" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #symfony_content) |]
        , id_ "symfony"
        ]
        "Symfony"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "net" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #net_content) |]
        , id_ "net"
        ]
        "C# .NET"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "gin" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #echo_content) |]
        , id_ "echo"
        ]
        "Go Echo"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ $ if current_tab == "fastify" then "sdk_tab sdk_tab_active" else "sdk_tab"
        , [__| install Navigatable(content: #fastify_content) |]
        , id_ "fastify"
        ]
        "Fastify Js"
