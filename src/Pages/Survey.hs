module Pages.Survey (surveyGetH, surveyPutH, SurveyForm) where

import Config
import Data.Default (def)
import Data.Text qualified as T
import Lucid
import Lucid.Htmx (hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript
import Lucid.Svg (d_, fill_, path_, viewBox_)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users qualified as Users
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import Data.List ((!!))
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute, withPool)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Pkg.Components (loader)
import Servant (Headers, addHeader)
import Servant.Htmx (HXRedirect, HXTrigger)
import Web.FormUrlEncoded (FromForm)

data SurveyForm = SurveyForm
  { stack :: [Text]
  , functionality :: [Text]
  , dataLocation :: Text
  , foundUsFrom :: Text
  , phoneNumber :: Maybe Text
  , fullName :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, FromJSON)

instance ToJSON SurveyForm where
  toJSON surveyForm =
    object
      [ "stack" .= filter (not . T.null) surveyForm.stack
      , "functionality" .= filter (not . T.null) surveyForm.functionality
      , "dataLocation" .= dataLocation surveyForm
      , "foundUsFrom" .= foundUsFrom surveyForm
      ]

surveyPutH :: Sessions.PersistentSession -> Projects.ProjectId -> SurveyForm -> DashboardM (Headers '[HXTrigger, HXRedirect] (Html ()))
surveyPutH sess pid survey = do
  pool <- asks pool
  env <- asks env
  let nameArr = T.splitOn " " (fullName survey)
  if length nameArr < 2
    then do
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","errorToast": ["Invalid full name format"]}|]
      pure $ addHeader hxTriggerData $ addHeader "" ""
    else do
      let jsonBytes = encode survey
      let firstName = nameArr !! 0
      let lastName = nameArr !! 1
      let phoneNumber = survey.phoneNumber
      res <- liftIO $ withPool pool $ execute Update [sql| update projects.projects set questions= ? where id=? |] (jsonBytes, pid)
      u <- liftIO $ withPool pool $ execute Update [sql| update users.users set first_name= ?, last_name=?, phone_number=? where id=? |] (firstName, lastName, phoneNumber, sess.userId)
      traceShowM u
      let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","successToast": ["Thanks for taking the survey"]}|]
      pure $ addHeader hxTriggerData $ addHeader ("/p/" <> show pid.unProjectId <> "/onboarding") ""

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
          , pageTitle = "About project"
          }
  let user = sess.user.getUser
  let full_name = user.firstName <> " " <> user.lastName
  let phoneNumber = fromMaybe "" user.phoneNumber
  pure $ bodyWrapper bwconf $ aboutPage pid full_name phoneNumber

aboutPage :: Projects.ProjectId -> Text -> Text -> Html ()
aboutPage pid full_name phoneNumber = do
  div_
    [ style_ "z-index:26"
    , class_ "fixed pt-16 justify-center z-50 w-full p-4 bg-white overflow-y-auto inset-0 h-full max-h-full text-lg"
    , id_ "surveyDialog"
    ]
    $ do
      div_ [class_ "relative mx-auto pb-24", style_ "width: min(90vw, 1000px)"] do
        div_ [class_ "flex justify-between mb-8"] $ do
          progressSteps
        div_ [class_ "bg-white rounded-lg shadow w-full"] do
          div_ [class_ "flex items-start justify-between p-6 space-x-2 w-full  border-b rounded-t"] $ do
            form_
              [ hxPost_ $ "/p/" <> pid.toText <> "/survey"
              , hxSwap_ "none"
              , class_ "w-full"
              ]
              $ do
                div_ [class_ "p-6 flex flex-col gap-8 overflow-y-auto", style_ "width:100%"] $ do
                  h3_ [class_ "text-navy-900 text-lg"] "Help us give you the best experience by completing the following"
                  div_ [class_ "flex flex-col gap-2 mt-8"] $ do
                    label_ [class_ "font-medium mt-2"] "Full name"
                    input_ [type_ "text", name_ "fullName", required_ "required", value_ full_name, class_ "px-2 py-1 bg-slate-50 border border-gray-300 text-gray-900 focus:outline-none rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full"]
                  div_ [class_ "flex flex-col gap-2"] do
                    span_ [class_ "font-semibold text-xl"] "What API/Web frameworks do you plan to integrate?"
                    div_ [id_ "stack", name_ "stack", required_ "required", class_ "px-2 py-2"] $ do
                      div_ [class_ "grid grid-cols-5 gap-6 space-y-2"] do
                        forM_ stackOptions $ \(value, label, img) -> do
                          let bg = "url('/assets/framework-logos/" <> img <> "')"
                          div_
                            [ class_ $ "column border rounded-lg text-[14px] bg-center bg-contain bg-no-repeat " <> bg
                            , style_ $ "background-image: " <> bg
                            ]
                            do
                              label_ [class_ "block group p-2 h-[80px] hover:bg-slate-100"] $ do
                                input_ [class_ "mr-3", type_ "checkbox", id_ value, name_ "stack", value_ value]
                                span_ [class_ "hidden group-hover:inline"] $ toHtml label
                      div_ [class_ "flex flex-col gap-2 mt-8"] $ do
                        label_ [class_ "font-medium mt-2"] "Other (specify)"
                        input_ [type_ "text", name_ "stack", class_ "px-2 py-1 bg-slate-50 border border-gray-300 text-gray-900 focus:outline-none rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full"]

                  div_ [class_ "flex flex-col gap-2"] do
                    label_ [class_ "font-bold"] "What APIToolkit features are you most interested in?"
                    div_ [class_ "columns-3"] do
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
                    div_ [class_ "columns-3"] do
                      forM_ foundUsFromOptions $ \(value, label) -> do
                        label_ [class_ "block hover:bg-slate-100 p-2"] $ do
                          input_ [class_ "mr-3", type_ "radio", id_ value, name_ "foundUsFrom", value_ value, required_ "required"]
                          toHtml label
                  div_ [class_ "flex flex-col gap-2 w-full"] do
                    label_ [class_ "font-bold"] "What's your phone number?"
                    div_ [class_ "w-full"] do
                      input_ [value_ phoneNumber, class_ "px-2 py-1 bg-slate-50 border border-gray-300 text-gray-900 focus:outline-none rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full", type_ "text", name_ "phoneNumber"]

                div_ [class_ "flex w-full justify-end items-center px-6 space-x-2 mt-8"] do
                  button_ [type_ "sumbit", class_ "btn-lg btn-indigo text-xl px-4"] "Proceed"
                  div_ [class_ "htmx-indicator query-indicator"] do
                    loader

stackOptions :: [(T.Text, T.Text, T.Text)]
stackOptions =
  [ ("expressjs", "JS - Express.js", "express-logo.png")
  , ("nest", "JS - Nest Js", "nestjs-logo.png")
  , ("next", "JS - Next Js", "nextjs-logo.webp")
  , ("koa", "JS - Koa", "koa-logo.png")
  , ("sailsjs", "JS - Sailsjs", "sails-logo.png")
  , ("adonisjs", "JS - Adonisjs", "adonis-logo.png")
  , ("fastify", "Js - Fastify", "fastify-logo.png")
  , ("django", "Python - Django", "django-logo.png")
  , ("go-native", "Golang - Native", "go-logo.png")
  , ("gorilla-mux", "Golang - Gorilla Mux", "mux-logo.png")
  , ("gin", "Golang - Gin", "gin-logo.png")
  , ("fiber", "Golang - Fiber", "fiber-logo.png")
  , ("laravel", "PHP - Laravel", "laravel-logo.png")
  , ("lumen", "PHP - Lumen", "lumen-logo.webp")
  , ("symfony", "PHP - Symfony", "symfony-logo.png")
  , ("cakePHP", "PHP - CakePHP", "cake-logo.jpg")
  , ("codeigniter", "PHP - Codeigniter", "igniter-logo.png")
  , ("flask", "Python - Flask", "flask-logo.png")
  , ("fastapi", "Python - FastAPI", "fastapi-logo.png")
  , ("springboot", "Java - Spring Boot", "spring-logo.png")
  , ("rails", "Ruby - Ruby on Rails", "rails-logo.png")
  , ("phoenix", "Elixir - Phoenix", "phoenix-logo.webp")
  , (".net", "C# - ASP.NET", "net-logo.png")
  , ("ihp-hs", "Haskell - IHP", "ihp-logo.svg")
  , ("actix", "Rust - Actix", "actix-logo.png")
  , ("rocket", "Rust - Rocket", "rocket-logo.webp")
  , ("scala-play", "Scala - Play", "play-logo.png")
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
        "About project"
    li_ [class_ "flex items-center"] $ do
      span_ [class_ "mr-2"] ""
      "Integrate"