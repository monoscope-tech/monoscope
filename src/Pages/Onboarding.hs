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
                  then pass
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
              div_ [class_ "bg-gradient-to-r from-green-400 to-blue-500 hidden w-full pb-16 px-24  mt-8", id_ "SDKs"] do
                div_ [class_ "bg-white rounded-lg px-4 pt-5 pb-4 text-left"] $ pass
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

--   li_ [] "Generate API key"
--   li_ [] "Integrate API toolkit into your app"
