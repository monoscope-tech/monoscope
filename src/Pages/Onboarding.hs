{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Pages.Onboarding (onboardingGetH) where

import Config
import Data.Default (def)
import Data.Text qualified as T
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx (hxPost_, hxTarget_)
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


onboardingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
onboardingGetH sess pid = do
  pool <- asks pool
  isMember <- liftIO $ withPool pool $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      (project, hasApikeys, hasRequest) <- liftIO
        $ withPool pool
        do
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
      let ans = case project of
            Nothing -> False
            Just p -> case p.questions of
              Just v -> True
              _ -> False

      pure $ bodyWrapper bwconf $ onboardingPage pid hasApikeys hasRequest ans


onboardingPage :: Projects.ProjectId -> Bool -> Bool -> Bool -> Html ()
onboardingPage pid hasApikey hasRequest ans = do
  div_ [class_ "relative h-full"] do
    div_ [class_ "flex flex-col h-full w-full gap-16"] do
      div_ [class_ "flex flex-col w-full mt-10 py-4 items-center gap-4"] do
        h3_ [class_ "text-slate-900 text-4xl font-bold"] "Complete the onboarding checklist"
        div_ [class_ "flex flex-col text-center gap-1 mb-4"] do
          p_ [class_ "text-slate-700 text-[16px]"] "Complete the onboarding checklist below to fully set up APIToolkit."
          p_ [class_ "text-slate-700 text-[16px]"] "Once completed, you can go to the dashboard to start using the platform"
        if not hasApikey
          then do
            generateApikey pid
          else ""
        if hasApikey && not hasRequest
          then do
            integrateApiToolkit
          else ""
        if hasApikey && hasRequest then completedBanner pid else ""
      div_ [class_ "w-full flex justify-center"] do
        div_ [class_ "flex flex-col w-[800px] rounded-2xl border border-2"] do
          div_ [class_ "w-full px-8 py-4 flex justify-between border-b border-b-2"] do
            h4_ [class_ "font-bold text-lg"] "Onboarding checklist"
            let p
                  | hasApikey = if hasRequest then "100%" else "66%"
                  | hasRequest = if hasApikey then "100%" else "66%"
                  | otherwise = "33%"
            span_ [class_ "text-slate-500"] $ p <> " completed"
          ul_ [class_ "px-3 py-4"] do
            li_ [class_ "flex items-center mx-4 py-4 border-b gap-6 text-green"] do
              faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" "h-6 w-6 text-green-700"
              button_ [class_ "flex flex-col"] do
                p_ [class_ "font-semibold"] "Create an account"
                span_ [class_ "text-slate-500"] "This is completed when you sign up"
            li_ [class_ "flex flex-col items-center mx-4 py-4 border-b gap-6 text-green"] do
              div_ [class_ "flex w-full items-center gap-6"] do
                let style = if hasApikey then "text-green-700" else "text-gray-300"
                faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" $ "h-6 w-6 " <> style
                button_
                  [ class_ "flex justify-between text-left w-full items-center"
                  , [__|on click toggle .hidden on #addAPIKey|]
                  ]
                  do
                    div_ [class_ "flex flex-col"] do
                      p_ [class_ "font-semibold"] "Generate an API key"
                      span_ [class_ "text-slate-500"] "The API key is used to authenticate requests"
                    faIcon_ "fa-chevron-down" "fa-regular fa-chevron-down" "h-6 w-6"
              div_ [class_ "bg-slate-100 hidden w-full py-16 px-24", id_ "addAPIKey"] do
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

            li_ [class_ "mx-4 py-4 border-b"] do
              div_ [class_ "flex w-full items-center  gap-6"] do
                let style = if hasRequest then "text-green-700" else "text-gray-300"
                faIcon_ "fa-circle-check" "fa-sharp fa-regular fa-circle-check" $ "h-6 w-6 " <> style
                button_ [class_ "flex justify-between text-left w-full items-center", [__|on click toggle .hidden on #SDKs|]] do
                  div_ [class_ "flex flex-col"] do
                    p_ [class_ "font-semibold"] "Integrate APIToolkit to your app"
                    span_ [class_ "text-slate-500"] "Integrate apitoolkit using any of our SDKs to start sending request."
                  faIcon_ "fa-chevron-down" "fa-regular fa-chevron-down" "h-6 w-6"
              div_ [class_ "hidden w-full bg-slate-100 mt-8", id_ "SDKs"] do
                if hasRequest
                  then do
                    p_ [class_ "text-green-500 text-center py-16 text-center"] "Apitoolkit has been integrated into your app"
                  else do
                    div_ [class_ "pb-8"] do
                      div_ [class_ "font-bold text-center text-white border-b border-slate-200"] do
                        p_ [class_ "text-red-500 text-center py-16 text-center"] "Apitoolkit has not been integrated into your app"
      --   tabs
      -- tabContentExpress
      -- tabContentGin
      -- tabContentLaravel
      -- tabContentSymfony
      -- tabContentDotNet
      -- tabContentFastify

      div_ [class_ "w-full flex justify-center pb-16 mt-16"] do
        div_ [class_ "flex flex-col w-[800px] rounded-2xl border border-2"] do
          div_ [class_ "grid grid-cols-2 border-b px-8"] do
            div_ [class_ "flex flex-col gap-2 py-8 border-r"] do
              faIcon_ "fa-file-lines" "fa-thin fa-file-lines" "h-8 w-8"
              h3_ [class_ "font-bold text-lg"] "Documentation"
              p_ [class_ "text-slate-700"] "Check out our documentation to learn more about using APIToolkit."
              a_ [href_ "https://www.apitoolkit.io/docs", class_ "text-blue-500 flex items-center gap-2"] do
                faIcon_ "fa-link-simple" "fa-sharp fa-regular fa-link-simple" "h-8 w-8 text-blue-500"
                "Read the docs"
            -- div_ [class_ "flex flex-col gap-2 py-4 border-l"] pass
            div_ [class_ "px-8 py-16 flex items-center gap-6 border-l"] do
              faIcon_ "fa-circle-play" "fa-light fa-circle-play" "text-blue-500"
              a_ [href_ "https://calendly.com/tonyalaribe/30min", class_ "flex flex-col"] do
                span_ [class_ "font-bold text-lg text-blue-500"] "Get Demo"
                span_ [class_ "text-slate-500"] "Schedule a brief call with co-founder Antony to provide a concise overview of apitoolkit and guide him on its effective utilization for API management."
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


integrateApiToolkit :: Html ()
integrateApiToolkit =
  div_ [class_ "w-[800px] bg-slate-200 mx-auto rounded-lg border-8 border-white shadow-lg mb-10"] do
    div_ [class_ "w-full p-8 bg-slate-100  rounded"] do
      div_ [class_ "flex w-full justify-center gap-4 items-center mb-2"] do
        span_ [class_ "text-blue-500 pr-4 border-r border-r-2 border-r-blue-500 text-2xl"] "Next Up"
        h3_ [class_ "font-bold text-2xl"] "Integrate APIToolkit"
      div_ [class_ "pb-2"] do
        div_ [class_ "font-bold text-center text-white border-b border-slate-200"] do
          tabs
        tabContentExpress
        tabContentGin
        tabContentLaravel
        tabContentFlask
        tabContentFastAPI
        tabContentDjango
        tabContentSymfony
        tabContentDotNet
        tabContentFastify


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


tabContentExpress :: Html ()
tabContentExpress =
  div_ [class_ "tab-content flex flex-col m-8", id_ "express_content"] do
    div_ [class_ "relative"] do
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
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
              div_ [class_ "w-full relative flex-auto"] do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
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
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "gin_content"] do
    div_ [class_ "relative"] do
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
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
              div_ [class_ "w-full relative flex-auto"] do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
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
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "laravel_content"] do
    div_ [class_ "relative"] do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "composer require apitoolkit/apitoolkit-php"
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1 mt-4"] "Set up APITOOLKIT_KEY env variable"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "APITOOLKIT_KEY=<YOUR_API_KEY>"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
              div_ [class_ "w-full relative flex-auto"] do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24\n25\n26\n27\n28\n29\n30\n"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
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
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "symfony_content"] do
    div_ [class_ "relative"] do
      div_ [class_ "mb-6"] do
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "composer require apitoolkit/apitoolkit-symfony"
        h3_ [class_ "text-slate-900 font-medium text-lg mb-1 mt-4"] "Set up APITOOLKIT_KEY env variable"
        p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "APITOOLKIT_KEY=<YOUR_API_KEY>"
      h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app"
      div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
        div_ [class_ "relative w-full flex flex-col"] do
          div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
            div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
              div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
              div_ [class_ "w-full relative flex-auto"] do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                    "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11"
                  code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
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
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "net_content"] do
    div_ [class_ "relative"] do
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
          div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
            div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
              div_ [class_ "w-full relative flex-auto"] do
                pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                  div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
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
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "fastify_content"]
    do
      div_ [class_ "relative"] do
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
            div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
              div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
                div_ [class_ "w-full relative flex-auto"] do
                  pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                    div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
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
tabContentFlask :: Html ()
tabContentFlask =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "flask_content"]
    do
      div_ [class_ "relative"] do
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
            div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
              div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
                div_ [class_ "w-full relative flex-auto"] do
                  pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                    div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                      "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24"
                    code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
                      span_ [class_ "hljs-keyword"] "from" >> " flask " >> span_ [class_ "hljs-keyword"] "import " >> "Flask"
                      br_ []
                      span_ [class_ "hljs-keyword"] "from" >> " apitoolkit " >> span_ [class_ "hljs-keyword"] "import " >> "APIToolkit"
                      br_ []
                      span_ [class_ ""] "app = Flask(__name__)"
                      br_ []
                      br_ []
                      span_ [class_ ""] "apitoolkit = APIToolkit(api_key=" >> span_ [class_ "hljs-string"] "<API_KEY>" >> ", debug=" >> span_ [class_ "hljs-keyword"] "True" >> ")"
                      br_ []
                      br_ []
                      span_ [class_ "hljs-deco"] "@app.before_request"
                      br_ []
                      span_ [class_ "hljs-keyword"] "def" >> span_ [class_ "hljs-title"] " before_request" >> "():"
                      br_ []
                      "    apitoolkit." >> span_ [class_ "hljs-title"] "beforeRequest" >> "()"
                      br_ []
                      br_ []
                      span_ [class_ "hljs-deco"] "@app.after_request"
                      br_ []
                      span_ [class_ "hljs-keyword"] "def" >> span_ [class_ "hljs-title"] " after_request" >> span_ [class_ "hljs-params"] "(response):"
                      br_ []
                      "    apitoolkit." >> span_ [class_ "hljs-title"] "afterRequest" >> "(response)\n"
                      span_ [class_ "hljs-keyword"] "         return " >> "response"
                      br_ []
                      br_ []
                      span_ [class_ "hljs-deco"] "@app.route(/hello/<name>, methods=[GET,  POST])"
                      br_ []
                      span_ [class_ "hljs-keyword"] "def " >> span_ [class_ "hljs-title"] "sample_route" >> "(name):"
                      br_ []
                      span_ [class_ "hljs-keyword"] "         return " >> "{" >> span_ [class_ "hljs-string"] "\"Hello\"" >> ": " >> span_ [class_ "hljs-string"] "\"Hello \"" >> " + name }"
                      br_ []
                      br_ []
                      span_ [] "app.run(debug=" >> span_ [class_ "hljs-keyword"] "True" >> ")"


tabContentFastAPI :: Html ()
tabContentFastAPI =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "fastapi_content"]
    do
      div_ [class_ "relative"] do
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
            div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
              div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
                div_ [class_ "w-full relative flex-auto"] do
                  pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                    div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                      "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n18\n19\n20\n21\n22\n23\n24"
                    code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
                      span_ [class_ "hljs-keyword"] "from" >> " fastapi " >> span_ [class_ "hljs-keyword"] "import " >> "FastAPI"
                      br_ []
                      span_ [class_ "hljs-keyword"] "from" >> " apitoolkit " >> span_ [class_ "hljs-keyword"] "import " >> "APIToolkit"
                      br_ []
                      br_ []
                      span_ [class_ ""] "app = " >> span_ [class_ "hljs-title"] "FastAPI" >> "()"
                      br_ []
                      br_ []
                      span_ [class_ ""] "apitoolkit = " >> span_ [class_ "hljs-title"] "APIToolkit" >> "()"
                      br_ []
                      br_ []
                      span_ [class_ "hljs-deco"] "@app.on_event('startup')"
                      br_ []
                      span_ [class_ "hljs-keyword"] "async def" >> span_ [class_ "hljs-title"] " startup_event" >> "():"
                      br_ []
                      span_ [class_ "hljs-keyword"] "    await" >> " apitoolkit." >> span_ [class_ "hljs-title"] "initialize" >> "("
                      br_ []
                      span_ [class_ "hljs-string"] "     <API_KEY>" >> ", debug=" >> span_ [class_ "hljs-keyword"] "True"
                      br_ []
                      span_ [] "    )"
                      br_ []
                      br_ []
                      "apitoolkit." >> span_ [class_ "hljs-title"] "middleware" >> "(" >> span_ [class_ "hljs-string"] "'http'" >> ")(apitoolkit.middleware)\n"
                      br_ []
                      span_ [class_ "hljs-deco"] "@app.get(/)"
                      br_ []
                      span_ [class_ "hljs-keyword"] "def " >> span_ [class_ "hljs-title"] "read_root" >> "():"
                      br_ []
                      span_ [class_ "hljs-keyword"] "     return " >> "{" >> span_ [class_ "hljs-string"] "\"Hello\"" >> ": " >> span_ [class_ "hljs-string"] "\"Hello world\"}"
                      br_ []


tabContentDjango :: Html ()
tabContentDjango =
  div_ [class_ "tab-content flex flex-col m-8 hidden", id_ "django_content"]
    do
      div_ [class_ "relative"] do
        div_ [class_ "mb-6"] do
          h3_ [class_ "text-slate-900 font-medium text-lg mb-1"] "Installation"
          p_ [class_ "w-full bg-slate-200 px-4 py-2 rounded-xl text-lg"] "pip install apitoolkit-django"
        h4_ [class_ "text-slate-900 font-medium text-lg my-2"] "Integrate into your app by adding APITOOLKIT_KEY and APIToolkit to the settings middleware list"
        div_ [class_ "relative overflow-hidden  flex bg-slate-800 h-[31.625rem] max-h-[60vh]] sm:rounded-xl lg:h-[34.6875rem] "] do
          div_ [class_ "relative w-full flex flex-col"] do
            div_ [class_ "flex-none border-b border-slate-500/30 flex items-center gap-4"] do
              div_ [class_ "flex items-center h-8 space-x-1.5 px-3"] do
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
                div_ [class_ "w-2.5 h-2.5 bg-slate-600 rounded-full"] ""
            div_ [class_ "relative min-h-0 flex-auto flex flex-col"] do
              div_ [class_ "w-full flex-auto flex min-h-0 overflow-auto"] do
                div_ [class_ "w-full relative flex-auto"] do
                  pre_ [class_ "flex min-h-full text-lg leading-snug", id_ "testkit-eg"] do
                    div_ [class_ "hidden md:block text-slate-600 flex-none py-4 pr-4 text-right select-none", style_ "width:50px"] do
                      "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15"
                    code_ [class_ "flex-auto relative block text-slate-50 py-4 px-4 overflow-auto"] do
                      span_ [class_ "hljs-keyword"] "APITOOLKIT_KEY" >> " = " >> span_ [class_ "hljs-string"] "<YOUR_API_KEY> \n"
                      br_ []
                      span_ [class_ "hljs-keyword"] "MIDDLEWARE" >> " = [\n"
                      span_ [] "   ...\n"
                      span_ [class_ "hljs-string"] "    'apitoolkit.APIToolkit'" >> ",\n"
                      span_ [] "   ...\n"
                      span_ [] "]"


tabs :: Html ()
tabs =
  ul_ [class_ "flex flex-nowrap overflow-x-auto gap-6"] do
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab sdk_tab_active"
        , onclick_ "changeTab('express')"
        , id_ "express"
        ]
        "Express Js"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('gin')"
        , id_ "gin"
        ]
        "Go Gin"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('laravel')"
        , id_ "laravel"
        ]
        "Laravel"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('flask')"
        , id_ "flask"
        ]
        "Flask"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('fastapi')"
        , id_ "fastapi"
        ]
        "FastAPI"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('django')"
        , id_ "django"
        ]
        "Django"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('symfony')"
        , id_ "symfony"
        ]
        "Symfony"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('net')"
        , id_ "net"
        ]
        "C# .NET"
    li_ [class_ "shrink-0"] do
      button_
        [ class_ "sdk_tab"
        , onclick_ "changeTab('fastify')"
        , id_ "fastify"
        ]
        "Fastify Js"
