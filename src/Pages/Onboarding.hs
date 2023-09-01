{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Onboarding (onboardingGetH) where

import Config
import Data.Default (def)

import Database.PostgreSQL.Entity.DBT (withPool)

import Lucid
import Lucid.Hyperscript

import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import NeatInterpolation

import Models.Users.Sessions qualified as Sessions

import Lucid.Htmx (hxPost_, hxTarget_)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

onboardingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
onboardingGetH sess pid = do
  pool <- asks pool
  (project, hasApikeys, hasRequest) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      apiKeys <- ProjectApiKeys.countProjectApiKeysByProjectId pid
      requestDumps <- RequestDumps.countRequestDumpByProject pid
      pure (project, apiKeys > 0, requestDumps > 0)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Get started"
          }
  pure $ bodyWrapper bwconf $ onboardingPage pid hasApikeys hasRequest

onboardingPage :: Projects.ProjectId -> Bool -> Bool -> Html ()
onboardingPage pid hasApikey hasRequest = do
  div_ [class_ "relative h-full"] $ do
    div_ [class_ "flex flex-col h-full w-full gap-16"] $ do
      div_ [class_ "flex flex-col w-full mt-10 py-4 items-center gap-4"] $ do
        h3_ [class_ "text-slate-900 text-4xl font-bold"] "Complete the onboarding checklist"
        div_ [class_ "flex flex-col text-center gap-1"] do
          p_ [class_ "text-slate-700 text-[16px]"] "Complete the onboarding checklist below to fully set up APIToolkit."
          p_ [class_ "text-slate-700 text-[16px]"] "Once completed, you can dismiss getting started from the menu"
      div_ [class_ "w-full flex justify-center"] $ do
        div_ [class_ "flex flex-col w-[800px] rounded-2xl border border-2"] $ do
          div_ [class_ "w-full px-8 py-4 flex justify-between border-b border-b-2"] $ do
            h4_ [class_ "font-bold text-lg"] "Onboarding checklist"
            span_ [class_ "text-slate-500"] "25% completed"
          ul_ [class_ "px-3 py-4"] do
            li_ [class_ "flex items-center mx-4 py-4 border-b gap-6"] do
              img_ [src_ "/assets/svgs/check_complete.svg", class_ "h-6 w-6"]
              button_ [class_ "flex flex-col"] do
                p_ [class_ "font-semibold"] "Create an account"
                span_ [class_ "text-slate-500"] "This is completed when you sign up"
            li_ [class_ "flex flex-col items-center mx-4 py-4 border-b gap-6 text-green"] do
              div_ [class_ "flex w-full items-center gap-6"] do
                let url = if hasApikey then "/assets/svgs/check_complete.svg" else "/assets/svgs/check.svg"
                img_ [src_ url, class_ "h-6 w-6"]
                button_
                  [ class_ "flex justify-between text-left w-full items-center"
                  , [__|on click toggle .hidden on #addAPIKey|]
                  ]
                  do
                    div_ [class_ "flex flex-col"] do
                      p_ [class_ "font-semibold"] "Generate an API key"
                      span_ [class_ "text-slate-500"] "The API key is used to authenticate requests"
                    img_ [src_ "/assets/svgs/down_chevron.svg", class_ "h-6 w-6"]
              div_ [class_ "bg-gray-100 hidden w-full py-16 px-24", id_ "addAPIKey"] do
                if hasApikey
                  then do
                    p_ [class_ "text-green-500 text-center"] "You have generated an API key"
                  else do
                    div_ [id_ "main-content"] do
                      form_
                        [ hxPost_ $ "/p/" <> pid.toText <> "/apis"
                        , class_ "flex items-end justify-center  pt-4 px-4 pb-20 text-center sm:block sm:p-0"
                        , hxTarget_ "#main-content"
                        ]
                        $ do
                          div_ [class_ "bg-white rounded-lg px-4 pt-5 pb-4 text-left"] $ do
                            div_ [class_ "sm:flex sm:items-start"] $ do
                              div_ [class_ "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left grow"] $ do
                                h3_ [class_ "text-lg font-medium text-gray-900", id_ "modal-title"] "Enter API key title"
                                div_ [class_ "mt-2 space-y-2"] $ do
                                  p_ [class_ "text-sm text-gray-500"] do
                                    "Please input a title for your API Key. You can find all API keys "
                                    a_ [href_ $ "/p/" <> pid.toText <> "/apis", class_ "text-blue-500"] "here"
                                  div_ $ do
                                    input_ [class_ "input-txt px-4 py-2  border w-full", type_ "text", placeholder_ "API Key Title", name_ "title", autofocus_]
                                    input_ [hidden_ "true", name_ "from", value_ "onboarding"]
                            div_ [class_ "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse"] $ do
                              button_ [type_ "submit", class_ "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-blue-600 text-base font-medium text-white hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 sm:ml-3 sm:w-auto sm:text-sm"] "Submit"

            li_ [class_ "mx-4 py-4 border-b"] do
              div_ [class_ "flex w-full items-center  gap-6"] do
                let url = if hasRequest then "/assets/svgs/check_complete.svg" else "/assets/svgs/check.svg"
                img_ [src_ url, class_ "h-6 w-6"]
                button_ [class_ "flex justify-between text-left w-full items-center", [__|on click toggle .hidden on #SDKs|]] do
                  div_ [class_ "flex flex-col"] do
                    p_ [class_ "font-semibold"] "Integrate APIToolkit to your app"
                    span_ [class_ "text-slate-500"] "Integrate apitoolkit using any of our SDKs to start sending request."
                  img_ [src_ "/assets/svgs/down_chevron.svg", class_ "h-6 w-6"]
              div_ [class_ "hidden w-full bg-gray-100 mt-8", id_ "SDKs"] do
                if hasRequest
                  then do
                    p_ [class_ "text-green-500 text-center py-16 text-center"] "Apitoolkit has been integrated into your app"
                  else do
                    div_ [class_ "pb-8"] do
                      div_ [class_ "font-bold text-center text-white border-b border-gray-200"] $ do
                        tabs
                      tabContentExpress
                      tabContentGin
                      tabContentLaravel
                      tabContentSymfony
                      tabContentDotNet
                      tabContentFastify

            -- <divclass="relative bg-slate-50 bg-no-repeat bg-cover  rounded-lg p-4 md:p-8 shadow-2xl">

            li_ [class_ "flex items-center mx-4 py-4 border-b gap-6"] do
              div_ [class_ "flex w-full items-center  gap-6"] do
                img_ [src_ "/assets/svgs/check.svg", class_ "h-6 w-6"]
                button_ [class_ "flex justify-between text-left w-full items-center"] do
                  div_ [class_ "flex flex-col"] do
                    p_ [class_ "font-semibold"] "Invite team members"
                    span_ [class_ "text-slate-500"] "Inivte other users to collaborate on your project"
                  img_ [src_ "/assets/svgs/down_chevron.svg", class_ "h-6 w-6"]
      div_ [class_ "w-full flex justify-center pb-16 mt-16"] $ do
        div_ [class_ "flex flex-col w-[800px] rounded-2xl border border-2"] $ do
          div_ [class_ "grid grid-cols-2 border-b px-8"] do
            div_ [class_ "flex flex-col gap-2 py-8 border-r"] do
              img_ [src_ "/assets/svgs/docs.svg", class_ "h-8 w-8"]
              h3_ [class_ "font-bold text-lg"] "Documentation"
              p_ [class_ "text-slate-700"] "Check out our documentation to learn more about using APIToolkit."
              a_ [href_ "https://www.apitoolkit.io/docs", class_ "text-blue-500"] "Read the docs"
            div_ [class_ "flex flex-col gap-2 py-4 border-l"] pass
          div_ [class_ "px-8 py-16 flex items-center gap-6"] do
            img_ [src_ "/assets/svgs/play.svg", class_ "h-14 w-14"]
            div_ [] do
              h3_ [class_ "font-bold text-lg text-blue-500"] "Watch Demo"
              p_ [class_ "text-slate-500"] "Watch co-founder Antony briefly explain what apitoolkit is and how to use it to stay on top of your APIs."
  script_
    [text|

function changeTab(tabId) {
  const tabLinks = document.querySelectorAll('.sdk_tab');
  tabLinks.forEach(link => link.classList.remove('sdk_tab_active'));
  const clickedTabLink = document.getElementById(tabId);
  clickedTabLink.classList.add('sdk_tab_active')
  const tabContents = document.querySelectorAll('.tab-content');
  tabContents.forEach(content => content.classList.add("hidden"));

  // Display the content of the clicked tab
  console.log(tabId + "_content")
  const tabContent = document.getElementById(tabId + '_content');
  console.log(tabContent)
  tabContent.classList.remove("hidden")

}
  |]

tabContentExpress :: Html ()
tabContentExpress =
  div_ [class_ "tab-content flex flex-col m-8", id_ "express_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "npm i apitoolkit-express"
      h4_ [class_ "text-gray-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] dark:bg-slate-900/70 dark:backdrop-blur dark:ring-1 dark:ring-inset dark:ring-white/10"] $ do
        div_ [class_ "relative w-full flex flex-col"] $ do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] $ do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] $ do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] $ do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] $ do
              div_ [class_ "w-full relative flex-auto"] $ do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] $ do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] $ do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] $ do
                    span_ [class_ "hljs-keyword"] "import" >> " " >> span_ [class_ "hljs-title"] "express" >> " " >> span_ [class_ "hljs-keyword"] "from" >> " " >> span_ [class_ "hljs-string"] "'express';" >> "\n"
                    span_ [class_ "hljs-keyword"] "import" >> " " >> span_ [class_ "hljs-title"] "APIToolkit" >> " " >> span_ [class_ "hljs-keyword"] "from" >> " " >> span_ [class_ "hljs-string"] "'apitoolkit-express';" >> "\n\n"
                    span_ [class_ "hljs-keyword"] "const" >> " " >> span_ [class_ "hljs-title"] "app" >> " = " >> span_ [class_ "hljs-title"] "express" >> "();\n"
                    span_ [class_ "hljs-keyword"] "const" >> " " >> span_ [class_ "hljs-title"] "port" >> " = " >> span_ [class_ "hljs-number"] "3000;" >> "\n\n"
                    span_ [class_ "hljs-keyword"] "const" >> " " >> span_ [class_ "hljs-title"] "apitoolkitClient" >> " = " >> span_ [class_ "hljs-keyword"] "await" >> " " >> span_ [class_ "hljs-title"] "APIToolkit.NewClient" >> "({ " >> span_ [class_ "hljs-attr"] "apiKey" >> ": " >> span_ [class_ "hljs-string"] "'<API-KEY>'" >> " });" >> "\n"
                    "app." >> span_ [class_ "hljs-title"] "use" >> "(" >> span_ [class_ "hljs-title"] "apitoolkitClient.expressMiddleware" >> ");" >> "\n\n"
                    "app." >> span_ [class_ "hljs-title"] "get" >> "(" >> span_ [class_ "hljs-string"] "'/'" >> ", " >> span_ [class_ "hljs-function"] "(" >> span_ [class_ "hljs-params"] "req, res" >> ") =>" >> " {" >> "\n"
                    "   res." >> span_ [class_ "hljs-title"] "send" >> "(" >> span_ [class_ "hljs-string"] "'Hello World!'" >> ");" >> "\n"
                    "});" >> "\n\n"
                    "app." >> span_ [class_ "hljs-title"] "listen" >> "(" >> span_ [class_ "hljs-title"] "port" >> ", " >> span_ [class_ "hljs-function"] "()" >> " => {" >> "\n"
                    "   console." >> span_ [class_ "hljs-title"] "log" >> "(" >> span_ [class_ "hljs-string"] "`Example app listening on port ${port}`" >> ");" >> "\n"
                    "});"

                    "    "

tabContentGin :: Html ()
tabContentGin =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "gin_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "go get github.com/apitoolkit/apitoolkit-go"
      h4_ [class_ "text-gray-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] dark:bg-slate-900/70 dark:backdrop-blur dark:ring-1 dark:ring-inset dark:ring-white/10"] $ do
        div_ [class_ "relative w-full flex flex-col"] $ do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] $ do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] $ do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] $ do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] $ do
              div_ [class_ "w-full relative flex-auto"] $ do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] $ do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] $ do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] $ do
                    span_ [class_ "hljs-keyword"] "package" >> " " >> span_ [class_ "hljs-title"] "main" >> "\n\n"
                    span_ [class_ "hljs-keyword"] "import" >> " (" >> "\n"
                    span_ [class_ "hljs-comment"] "// Import the apitoolkit golang sdk" >> "\n"
                    span_ [class_ ""] "apitoolkit" >> " " >> span_ [class_ "hljs-string"] "\"github.com/apitoolkit/apitoolkit-go\"" >> "\n"
                    span_ [class_ "hljs-string"] "\"context\"" >> "\n"
                    span_ [class_ "hljs-string"] "\"github.com/gin-gonic/gin\"" >> "\n"
                    span_ [class_ "hljs-keyword"] ")" >> "\n\n"
                    span_ [class_ "hljs-keyword"] "func" >> " " >> span_ [class_ "hljs-title"] "main" >> "()" >> " {" >> "\n"
                    span_ [class_ "hljs-variable"] "ctx" >> " := " >> span_ [class_ "hljs-title"] "context.Background" >> "()" >> "\n\n"
                    span_ [class_ "hljs-comment"] "// Initialize the client using your apitoolkit.io generated apikey" >> "\n"
                    span_ [class_ "hljs-variable"] "apitoolkitClient, err" >> " := " >> span_ [class_ "hljs-title"] "apitoolkit.NewClient" >> "(ctx, apitoolkit.Config{APIKey: " >> span_ [class_ "hljs-string"] "\"<APIKEY>\"" >> "})" >> "\n"
                    span_ [class_ "hljs-keyword"] "if" >> " err != " >> span_ [class_ "hljs-literal"] "nil" >> " {" >> "\n"
                    span_ [class_ "hljs-comment"] "    // Handle the error" >> "\n"
                    span_ [class_ "hljs-built_in"] "    panic" >> "(err)" >> "\n"
                    span_ [class_ ""] "}" >> "\n\n"
                    span_ [class_ "hljs-variable"] "router" >> " := " >> span_ [class_ "hljs-title"] "gin.New" >> "()" >> "\n\n"
                    span_ [class_ "hljs-comment"] "// Register with the corresponding middleware of your choice. For Gin router, we use the GinMiddleware method." >> "\n"
                    "router." >> span_ [class_ "hljs-title"] "Use" >> "(apitoolkitClient.GinMiddleware)" >> "\n\n"
                    span_ [class_ "hljs-comment"] "// Register your handlers as usual and run the gin server as usual." >> "\n"
                    "router." >> span_ [class_ "hljs-title"] "POST" >> "(" >> span_ [class_ "hljs-string"] "\"/:slug/test\"" >> ", " >> span_ [class_ "hljs-keyword"] "func" >> "(c *gin.Context)" >> " {" >> " c.String" >> "(" >> span_ [class_ "hljs-number"] "200" >> ", " >> span_ [class_ "hljs-string"] "\"ok\"" >> ")" >> " })" >> "\n"
                    span_ [class_ "hljs-comment"] "// Rest of your app..." >> "\n"

tabContentLaravel :: Html ()
tabContentLaravel =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "laravel_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "composer require apitoolkit/apitoolkit-php"
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1 mt-4"] "Set up APITOOLKIT_KEY env variable"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "APITOOLKIT_KEY=<YOUR_API_KEY>"
      h4_ [class_ "text-gray-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] dark:bg-slate-900/70 dark:backdrop-blur dark:ring-1 dark:ring-inset dark:ring-white/10"] $ do
        div_ [class_ "relative w-full flex flex-col"] $ do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] $ do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] $ do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] $ do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] $ do
              div_ [class_ "w-full relative flex-auto"] $ do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] $ do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] $ do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] $ do
                    span_ [class_ "hljs-meta"] "<?php" >> "\n\n"
                    span_ [class_ "hljs-keyword"] "namespace" >> " " >> span_ [class_ "hljs-title"] "App" >> "\\" >> span_ [class_ "hljs-title"] "Http" >> ";" >> "\n\n"
                    span_ [class_ "hljs-keyword"] "use" >> " " >> span_ [class_ "hljs-title"] "Illuminate" >> "\\" >> span_ [class_ "hljs-title"] "Foundation" >> "\\" >> span_ [class_ "hljs-title"] "Http" >> "\\" >> span_ [class_ "hljs-title"] "Kernel" >> " " >> span_ [class_ "hljs-keyword"] "as" >> " " >> span_ [class_ "hljs-title"] "HttpKernel" >> ";" >> "\n\n"
                    span_ [class_ "hljs-class"] "class" >> " " >> span_ [class_ "hljs-title"] "Kernel" >> " " >> span_ [class_ "hljs-keyword"] "extends" >> " " >> span_ [class_ "hljs-title"] "HttpKernel" >> "\n" >> "    {\n"
                    span_ [class_ "hljs-comment"] "    // ..." >> "\n\n"
                    span_ [class_ "hljs-comment"] "    /**\n" >> span_ [class_ "hljs-comment"] "     * The application's route middleware groups.\n" >> span_ [class_ "hljs-comment"] "     *\n" >> span_ [class_ "hljs-comment"] "     * " >> span_ [class_ "hljs-comment"] "@var array" >> "\n" >> span_ [class_ "hljs-comment"] "     */" >> "\n"
                    span_ [class_ "hljs-keyword"] "    protected" >> " " >> span_ [class_ "hljs-variable"] "$middlewareGroups" >> " = [\n"
                    span_ [class_ "hljs-comment"] "        // ..." >> "\n\n"
                    span_ [class_ "hljs-string"] "        'api'" >> " => [\n"
                    span_ [class_ "hljs-comment"] "            // ..." >> "\n"
                    span_ [class_ "hljs-title"] "            \\APIToolkit\\Http\\Middleware\\APIToolkit" >> "::" >> span_ [class_ "hljs-variable language_"] "class" >> "," >> "\n"
                    span_ [class_ "hljs-comment"] "           // ..." >> "\n"
                    span_ [class_ ""] "          ]," >> "\n"
                    span_ [class_ ""] "    ];" >> "\n\n"
                    span_ [class_ "hljs-comment"] "    // ..." >> "\n" >> "}"

tabContentSymfony :: Html ()
tabContentSymfony =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "symfony_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "composer require apitoolkit/apitoolkit-symfony"
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1 mt-4"] "Set up APITOOLKIT_KEY env variable"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "APITOOLKIT_KEY=<YOUR_API_KEY>"
      h4_ [class_ "text-gray-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] dark:bg-slate-900/70 dark:backdrop-blur dark:ring-1 dark:ring-inset dark:ring-white/10"] $ do
        div_ [class_ "relative w-full flex flex-col"] $ do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] $ do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] $ do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] $ do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] $ do
              div_ [class_ "w-full relative flex-auto"] $ do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] $ do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] $ do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] $ do
                    span_ [class_ "hljs-attr"] "services:" >> "\n"
                    span_ [class_ "hljs-string"] "    APIToolkit\\EventSubscriber\\APIToolkitService:" >> "\n"
                    span_ [class_ "hljs-attr"] "        arguments:" >> "\n"
                    span_ [class_ "hljs-string"] "            $apiKey:" >> " " >> span_ [class_ "hljs-string"] "'%env(APITOOLKIT_KEY)%'" >> "\n"
                    span_ [class_ "hljs-comment"] "     # Optional:  if you want to cache login result add this cache pool instance via setter injection" >> "\n"
                    span_ [class_ "hljs-attr"] "        calls:" >> "\n"
                    span_ [class_ "hljs-bullet"] "            -" >> " " >> span_ [class_ "hljs-attr"] "setCachePool:" >> " [" >> span_ [class_ "hljs-string"] "'@PutYourCachePoolServiceHere'" >> "]" >> "\n"
                    span_ [class_ "hljs-attr"] "        tags:" >> "\n"
                    span_ [class_ "hljs-bullet"] "            -" >> " { " >> span_ [class_ "hljs-attr"] "name:" >> " " >> span_ [class_ "hljs-string"] "'kernel.event_subscriber'" >> " }"

tabContentDotNet :: Html ()
tabContentDotNet =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "net_content"] $ do
    div_ [class_ "relative"] $ do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-gray-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "dotnet add package ApiToolkit.Net"
      h4_ [class_ "text-gray-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] dark:bg-slate-900/70 dark:backdrop-blur dark:ring-1 dark:ring-inset dark:ring-white/10"] $ do
        div_ [class_ "relative w-full flex flex-col"] $ do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] $ do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] $ do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] $ do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] $ do
              div_ [class_ "w-full relative flex-auto"] $ do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] $ do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] $ do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
                    span_ [class_ "hljs-keyword"] "var"
                      >> " "
                      >> span_ [class_ "hljs-variable"] "config"
                      >> " = "
                      >> span_ [class_ "hljs-keyword"] "new"
                      >> " Config"
                      >> "\n"
                      >> "{\n"
                      >> span_ [class_ "hljs-attr"] "    Debug"
                      >> " = "
                      >> span_ [class_ "hljs-literal"] "true"
                      >> ","
                      >> " "
                      >> span_ [class_ "hljs-comment"] "// Set debug flags to false in production"
                      >> "\n"
                      >> span_ [class_ "hljs-attr"] "    ApiKey"
                      >> " = "
                      >> span_ [class_ "hljs-string"] "\"<Your_APIKey>\""
                      >> "\n"
                      >> span_ [class_ "hljs-meta"] "};"
                      >> "\n"
                    span_ [class_ "hljs-keyword"] "var" >> " " >> span_ [class_ "hljs-variable"] "client" >> " = " >> span_ [class_ "hljs-keyword"] "await" >> " " >> span_ [class_ "hljs-title"] "APIToolkit.NewClientAsync" >> "(config);" >> "\n"
                    span_ [class_ "hljs-comment"] "// Register the middleware to use the initialized client" >> "\n"
                    "app."
                      >> span_ [class_ "hljs-title"] "Use"
                      >> "(async (context, next) =>"
                      >> "\n"
                      >> span_ [class_ "hljs-keyword"] "    var"
                      >> " "
                      >> span_ [class_ "hljs-variable"] "apiToolkit"
                      >> " = "
                      >> span_ [class_ "hljs-keyword"] "new"
                      >> " "
                      >> span_ [class_ "hljs-title"] "APIToolkit"
                      >> "(next, client);"
                      >> "\n"
                      >> span_ [class_ "hljs-keyword"] "    await"
                      >> " "
                      >> span_ [class_ "hljs-variable"] "apiToolkit.InvokeAsync"
                      >> "(context);"
                      >> "\n"
                    span_ [class_ ""] ");"

tabContentFastify :: Html ()
tabContentFastify =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "fastify_content"] $
    do
      div_ [class_ "relative"] $ do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-gray-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-gray-200 px-4 py-2 rounded text-lg"] "npm install apitoolkit-fastify"
        h4_ [class_ "text-gray-900 font-medium text-lg my-2"] "Integrate into your app"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] dark:bg-slate-900/70 dark:backdrop-blur dark:ring-1 dark:ring-inset dark:ring-white/10"] $ do
          div_ [class_ "relative w-full flex flex-col"] $ do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] $ do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] $ do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 flex-auto flex flex-col"] $ do
              div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] $ do
                div_ [class_ "w-full relative flex-auto"] $ do
                  pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] $ do
                    div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] $ do
                      "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24"
                    code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
                      span_ [class_ "hljs-keyword"] "import" >> span_ [class_ "hljs-title"] " APIToolkit " >> span_ [class_ "hljs-keyword"] "from" >> span_ [class_ "hljs-string"] " 'apitoolkit-fastify'" >> ";"
                      "\n" >> span_ [class_ "hljs-keyword"] "import" >> span_ [class_ "hljs-title"] " Fastify " >> span_ [class_ "hljs-keyword"] "from" >> span_ [class_ "hljs-string"] " 'fastify'" >> ";"
                      "\n" >> span_ [class_ "hljs-keyword"] "const" >> span_ [class_ "hljs-title"] " fastify " >> span_ [class_ "hljs-keyword"] "=" >> span_ [class_ "hljs-title"] " Fastify" >> span_ [class_ ""] "();\n"
                      span_ [class_ "hljs-comment"] "// Create and initialize an instance of the APIToolkit\n"
                      span_ [class_ "hljs-keyword"] "const" >> span_ [class_ "hljs-title"] " apittoolkitClient " >> span_ [class_ "hljs-keyword"] "= " >> span_ [class_ "hljs-keyword"] "await"
                      span_ [class_ "hljs-title"] " APIToolkit.NewClient" >> "({\n"
                      span_ [class_ "hljs-attr"] "  apiKey" >> ": " >> span_ [class_ "hljs-string"] "<YOUR_API_KEY>" >> ",\n"
                      span_ [class_ "hljs-attr"] "  fastify" >> ": fastify\n" >> "});\n"
                      span_ [class_ "hljs-title function_"] "apitoolkitClient.init" >> "();\n"
                      span_ [class_ "hljs-comment"] "// Rest of your app \n"
                      span_ [class_ "hljs-title"] "fastify.get" >> "(" >> span_ [class_ "hljs-string"] "'/hello'" >> ", " >> span_ [class_ "hljs-params"] "(request, reply)" >> span_ [class_ "hljs-keyword"] " => " >> "{\n"
                      " reply." >> span_ [class_ "hljs-title"] "send" >> "({" >> span_ [class_ "hljs-attr"] "hello" >> ":" >> span_ [class_ "hljs-string"] "'world'" >> "})\n"
                      span_ [] "});\n\n"
                      "fastify." >> span_ [class_ "hljs-title"] "listen" >> "({" >> span_ [class_ "hljs-attr"] "port" >> ": " >> span_ [class_ "hljs-number"] "3000" >> "}, " >> span_ [class_ "hljs-keyword"] "function" >> span_ [class_ "hljs-params"] "(err, address) {\n"
                      span_ [class_ "hljs-keyword"] "   if" >> span_ [class_ "hljs-params"] "(err) {\n"
                      "     fastify." >> span_ [class_ "hljs-title"] "log.error" >> "(err); \n"
                      "     process." >> span_ [class_ "hljs-title"] "exit" >> "(1);\n"
                      span_ [class_ ""] "   }\n"
                      span_ [class_ ""] "});"

tabs :: Html ()
tabs =
  ul_ [class_ "flex flex-nowrap overflow-x-auto justify-center gap-2"] $ do
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ "sdk_tab sdk_tab_active"
        , onclick_ "changeTab('express')"
        , id_ "express"
        ]
        "Express Js"
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('gin')"
        , id_ "gin"
        ]
        "Go Gin"
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('laravel')"
        , id_ "laravel"
        ]
        "Laravel"
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('symfony')"
        , id_ "symfony"
        ]
        "Symfony"
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('net')"
        , id_ "net"
        ]
        "C# .NET"
    li_ [class_ "shrink-0"] $ do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('fastify')"
        , id_ "fastify"
        ]
        "Fastify Js"

--   li_ [] "Generate API key"
--   li_ [] "Integrate API toolkit into your app"
