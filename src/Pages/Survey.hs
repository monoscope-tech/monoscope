module Pages.Survey (surveyGetH) where

import Config
import Data.Default (def)
import Data.Text qualified as T
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Lucid.Htmx (hxPost_, hxTarget_)
import Lucid.Hyperscript
import Lucid.Svg (d_, fill_, path_, stroke_, stroke_linecap_, stroke_linejoin_, stroke_width_, viewBox_)
import Lucid.Svg qualified as A
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

surveyGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
surveyGetH sess pid = do
  pool <- asks pool
  project <- liftIO $
    withPool pool $ do
      Projects.selectProjectForUser (Sessions.userId sess, pid)
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Project about"
          }
  pure $ bodyWrapper bwconf $ aboutPage pid

aboutPage :: Projects.ProjectId -> Html ()
aboutPage pid = do
  div_
    [ style_ "z-index:99999"
    , class_ "fixed pt-16 justify-center z-50 w-full p-4 bg-white overflow-y-auto inset-0 h-full max-h-full text-lg"
    , id_ "surveyDialog"
    ]
    $ do
      div_ [class_ "relative mx-auto pb-24", style_ "width: min(90vw, 900px)"] do
        div_ [class_ "flex justify-between mb-8"] $ do
          progressSteps
        div_ [class_ "bg-white rounded-lg shadow w-full"] do
          div_ [class_ "flex items-start justify-between p-6 space-x-2 w-full  border-b rounded-t"] $ do
            form_
              [ hxPost_ $ "/p/" <> pid.toText <> "/survey"
              , hxTarget_ "#main-content"
              , id_ "main-content"
              , class_ "w-full"
              , [__|on closeModal from body add .hidden to #surveyDialog then call me.reset()|]
              ]
              $ do
                div_ [class_ "p-6 flex flex-col gap-8 overflow-y-auto", style_ "width:100%"] $ do
                  h3_ [class_ "text-navy-900 text-lg"] "Help us give you the best experience by completing the following"
                  div_ [class_ "flex flex-col gap-2"] do
                    span_ [class_ "font-semibold text-xl"] "What API/Web frameworks do you plan to integrate?"
                    div_ [id_ "stack", name_ "stack", required_ "required", class_ "px-2 py-2"] $ do
                      div_ [class_ "columns-3 space-y-2"] do
                        forM_ stackOptions $ \(value, label, img) ->
                          div_ [class_ "column border rounded-lg"] do
                            label_ [class_ "block px-6 py-8 hover:bg-slate-100 p-2"] $ do
                              input_ [class_ "mr-3", type_ "checkbox", id_ value, name_ "stack", value_ value]
                              toHtml label
                      div_ [class_ "flex flex-col gap-2"] $ do
                        label_ [class_ "font-medium mt-2"] "Other (specify)"
                        input_ [type_ "text", name_ "stack", class_ "px-2 py-1 bg-slate-50 border border-gray-300 text-gray-900 focus:outline-none rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full"]

                  div_ [class_ "flex flex-col gap-2"] do
                    label_ [class_ "font-bold"] "What APIToolkit features are you most interested in?"
                    div_ do
                      forM_ functionalityOptions $ \(value, label) -> do
                        label_ [class_ "block hover:bg-slate-100 p-2"] $ do
                          input_ [class_ "mr-3", type_ "checkbox", id_ value, name_ "functionality", value_ value]
                          toHtml label
                  div_ [class_ "flex flex-col gap-2"] do
                    label_ [class_ "font-bold"] "Where would you prefer your data to be processed?"
                    div_ [class_ "columns-3"] do
                      forM_ dataLocationOptions $ \(value, label) -> do
                        label_ [class_ "block hover:bg-slate-100 p-2"] $ do
                          input_ [class_ "mr-3", type_ "radio", id_ value, name_ "dataLocation", value_ value, required_ "required"]
                          toHtml label
                  div_ [class_ "flex flex-col gap-2"] do
                    label_ [class_ "font-bold"] "How did you find APIToolkit?"
                    div_ [class_ "columns-2"] do
                      forM_ foundUsFromOptions $ \(value, label) -> do
                        label_ [class_ "block hover:bg-slate-100 p-2"] $ do
                          input_ [class_ "mr-3", type_ "radio", id_ value, name_ "foundUsFrom", value_ value, required_ "required"]
                          toHtml label
                  div_ [class_ "flex flex-col gap-2 w-full"] do
                    label_ [class_ "font-bold"] "Phone number"
                    div_ [class_ "w-full"] do
                      input_ [class_ "px-2 py-1 bg-slate-50 border border-gray-300 text-gray-900 focus:outline-none rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full", type_ "text", name_ "phoneNumber"]

                div_ [class_ "flex w-full justify-end items-center px-6 space-x-2 mt-8"] do
                  button_ [type_ "sumbit", class_ "btn-lg btn-indigo text-xl px-4"] "Proceed"

stackOptions :: [(T.Text, T.Text, T.Text)]
stackOptions =
  [ ("expressjs", "JS - Express.js", "/assets/gradient.webp")
  , ("nest", "JS - Nest Js", "/assets/gradient.webp")
  , ("next", "JS - Next Js", "/assets/gradient.webp")
  , ("koa", "JS - Koa", "/assets/gradient.webp")
  , ("sailsjs", "JS - Sailsjs", "/assets/gradient.webp")
  , ("adonisjs", "JS - Adonisjs", "/assets/gradient.webp")
  , ("fastify", "Js - Fastify", "/assets/gradient.webp")
  , ("django", "Python - Django", "/assets/gradient.webp")
  , ("go-native", "Golang - Native", "/assets/gradient.webp")
  , ("gorilla-mux", "Golang - Gorilla Mux", "/assets/gradient.webp")
  , ("gin", "Golang - Gin", "/assets/gradient.webp")
  , ("fiber", "Golang - Fiber", "/assets/gradient.webp")
  , ("beego", "Golang - Beego", "/assets/gradient.webp")
  , ("laravel", "PHP - Laravel", "/assets/gradient.webp")
  , ("lumen", "PHP - Lumen", "/assets/gradient.webp")
  , ("symfony", "PHP - Symfony", "/assets/gradient.webp")
  , ("cakePHP", "PHP - CakePHP", "/assets/gradient.webp")
  , ("cakeigniter", "PHP - Codeigniter", "/assets/gradient.webp")
  , ("flask", "Python - Flask", "/assets/gradient.webp")
  , ("fastapi", "Python - FastAPI", "/assets/gradient.webp")
  , ("springboot", "Java - Spring Boot", "/assets/gradient.webp")
  , ("rails", "Ruby - Ruby on Rails", "/assets/gradient.webp")
  , ("phoenix", "Elixir - Phoenix", "/assets/gradient.webp")
  , (".net", "C# - ASP.NET", "/assets/gradient.webp")
  , ("ihp-hs", "Haskell - IHP", "/assets/gradient.webp")
  , ("actix", "Rust - Actix", "/assets/gradient.webp")
  , ("rocket", "Rust - Rocket", "/assets/gradient.webp")
  , ("scala-play", "Scala - Play", "/assets/gradient.webp")
  ]

functionalityOptions :: [(T.Text, T.Text)]
functionalityOptions =
  [ ("monitoring", "API Monitoring")
  , ("log_explorer", "Log Explorer")
  , ("documentation", "Automatic API Documentation")
  , ("anomaly_detection", "Anomaly Detection")
  , ("testing", "Testing")
  ]

dataLocationOptions :: [(T.Text, T.Text)]
dataLocationOptions =
  [ ("asia", "Asia")
  , ("eu", "EU")
  , ("us", "US")
  ]

foundUsFromOptions :: [(T.Text, T.Text)]
foundUsFromOptions =
  [ ("twitter", "Twitter")
  , ("google", "Google")
  , ("linkedin", "LinkedIn")
  , ("reddit", "Reddit")
  , ("other", "Other")
  ]

progressSteps :: Html ()
progressSteps = do
  ol_ [class_ "flex items-center w-full text-sm font-medium text-center text-gray-500 dark:text-gray-400 sm:text-base"] $ do
    li_ [class_ "flex md:w-full items-center text-blue-600 dark:text-blue-500 sm:after:content-[''] after:w-1/2 after:h-1 after:border-b after:border-blue-600 after:border-1 after:hidden sm:after:inline-block after:mx-6 xl:after:mx-10"] $ do
      span_ [class_ "flex items-center"] $ do
        svg_ [class_ "w-3.5 h-3.5 sm:w-4 sm:h-4 mr-2.5", style_ "aria-hidden: true", xmlns_ "http://www.w3.org/2000/svg", fill_ "currentColor", viewBox_ "0 0 20 20"] $ do
          path_ [d_ "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 8.207-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L9 10.586l3.293-3.293a1 1 0 0 1 1.414 1.414Z"]
        "Create Account"
    li_ [class_ "flex md:w-full items-center after:content-[''] after:w-1/2 after:h-1 after:border-b after:border-gray-200 after:border-1 after:hidden sm:after:inline-block after:mx-6 xl:after:mx-10 dark:after:border-gray-700"] $ do
      span_ [class_ "flex items-center"] $ do
        svg_ [class_ "w-3.5 h-3.5 sm:w-4 sm:h-4 mr-2.5", style_ "aria-hidden: true", xmlns_ "http://www.w3.org/2000/svg", fill_ "currentColor", viewBox_ "0 0 20 20"] $ do
          path_ [d_ "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 8.207-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L9 10.586l3.293-3.293a1 1 0 0 1 1.414 1.414Z"]
        "Your Stack"
    li_ [class_ "flex items-center"] $ do
      span_ [class_ "mr-2"] ""
      "Integrate"