{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.Onboarding.Onboarding (
  onboardingGetH,
  onboardingInfoPost,
  onboardingConfPost,
  phoneEmailPostH,
  pricingPage,
  checkIntegrationGet,
  onboardingStepSkipped,
  proxyLandingH,
  DiscordForm (..),
  NotifChannelForm (..),
  OnboardingInfoForm (..),
  OnboardingConfForm (..),
) where

import Control.Lens qualified as L
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Wreq qualified as W (get, responseBody)
import Data.Text qualified as T
import Data.Tuple.Extra (thd3)
import Data.Vector qualified as V (Vector, fromList, head, length, toList)
import Database.PostgreSQL.Entity.DBT (execute, queryOne)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types (Query (Query))
import Database.PostgreSQL.Transact (DBT)
import Effectful.Error.Static (throwError)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Slack (getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects (NotificationChannel (NEmail, NPhone))
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components
import PyF (fmt)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import Servant (err401, errBody)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, redirectCS)
import Utils (faSprite_, getOtelLangVersion, insertIfNotExist, lookupValueText, onpointerdown_)
import Web.FormUrlEncoded


-- 'Info', 'Survey', 'CreateMonitor','NotifChannel','Integration', 'Pricing', 'Complete'
onboardingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
onboardingGetH pid onboardingStepM = do
  (sess, project) <- Sessions.sessionAndProject pid
  appContx <- ask @AuthContext
  let bodyConfig =
        (def :: BWConfig)
          { currProject = Nothing
          }
      questions = fromMaybe (AE.Object []) project.questions
      onboardingStep = fromMaybe "Info" onboardingStepM
  case onboardingStep of
    "Complete" -> do
      addRespHeaders $ PageCtx bodyConfig $ onboardingCompleteBody pid
    "Survey" -> do
      let host = fromMaybe "" $ lookupValueText questions "location"
          func = case questions of
            (AE.Object q) ->
              let fun = case KM.lookup "functionality" q of
                    Just (AE.Array f) ->
                      ( \case
                          AE.String s -> s
                          _ -> ""
                      )
                        <$> V.toList f
                    _ -> []
               in fun
            _ -> []
      addRespHeaders $ PageCtx bodyConfig $ onboardingConfigBody pid host func
    "CreateMonitor" -> do
      colsM <- dbtToEff $ Testing.getCollectionByTitle pid "HEALTH CHECK."
      addRespHeaders $ PageCtx bodyConfig $ createMonitorPage pid colsM
    "NotifChannel" -> do
      slack <- getProjectSlackData pid
      discord <- getDiscordDataByProjectId pid
      let phone = fromMaybe "" project.notifyPhoneNumber
          emails = project.notifyEmails
          hasDiscord = isJust discord
          hasSlack = isJust slack
      addRespHeaders $ PageCtx bodyConfig $ notifChannels appContx pid phone emails hasDiscord hasSlack
    "Integration" -> do
      apiKey <- dbtToEff $ ProjectApiKeys.projectApiKeysByProjectId pid
      let key = if V.length apiKey > 0 then let defKey = V.head apiKey in defKey.keyPrefix else "<API_KEY>"
      addRespHeaders $ PageCtx bodyConfig $ integrationsPage pid key
    "Pricing" -> do
      let lemonUrl = appContx.config.lemonSqueezyUrl <> "&checkout[custom][project_id]=" <> pid.toText
          critical = appContx.config.lemonSqueezyCriticalUrl <> "&checkout[custom][project_id]=" <> pid.toText
          paymentPlan = project.paymentPlan
      addRespHeaders $ PageCtx bodyConfig $ pricingPage pid lemonUrl critical paymentPlan
    _ -> do
      let firstName = sess.user.firstName
          lastName = sess.user.lastName
          (companyName, companySize, foundUsfrom) = (fromMaybe "" $ lookupValueText questions "companyName", fromMaybe "" $ lookupValueText questions "companySize", fromMaybe "" $ lookupValueText questions "foundUsFrom")
      addRespHeaders $ PageCtx bodyConfig $ onboardingInfoBody pid firstName lastName companyName companySize foundUsfrom


-- addRespHeaders $ PageCtx bodyConfig $ div_ [class_ "container"] $ "Hello world"

data OnboardingInfoForm = OnboardingInfoForm
  { firstName :: Text
  , lastName :: Text
  , companyName :: Text
  , companySize :: Text
  , whereDidYouHearAboutUs :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm)


data OnboardingConfForm = OnboardingConForm
  { location :: Text
  , functionality :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype DiscordForm = DiscordForm {url :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data NotifChannelForm = NotifChannelForm
  { phoneNumber :: Text
  , emails :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm)


onboardingStepSkipped :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
onboardingStepSkipped pid stepM = do
  (sess, project) <- Sessions.sessionAndProject pid
  case stepM of
    Just step -> do
      let stepsCompleted = project.onboardingStepsCompleted
          newCompleted = insertIfNotExist step stepsCompleted
      _ <- dbtToEff $ execute [sql| update projects.projects set onboarding_steps_completed=? where id=? |] (newCompleted, pid)

      redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=" <> getNextStep step
      addRespHeaders ""
    _ -> do
      redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Info"
      addRespHeaders ""


getNextStep :: Text -> Text
getNextStep "CreateMonitor" = "NotifChannel"
getNextStep "Integration" = "Pricing"
getNextStep _ = "Info"


phoneEmailPostH :: Projects.ProjectId -> NotifChannelForm -> ATAuthCtx (RespHeaders (Html ()))
phoneEmailPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let phone = form.phoneNumber
      emails = form.emails
      notifs = if phone /= "" then map (.toText) (V.toList project.notificationsChannel) <> [show NPhone] else map (.toText) (V.toList project.notificationsChannel)
      notifs' = if emails /= [] then notifs <> [show NEmail] else notifs
      notifsTxt = ordNub notifs'
      stepsCompleted = project.onboardingStepsCompleted
      newCompleted = insertIfNotExist "NotifChannel" stepsCompleted
      q = [sql| update projects.projects set notifications_channel=?::notification_channel_enum[], notify_phone_number=?, notify_emails=?::text[],onboarding_steps_completed=? where id=? |]
  projectMembers <- dbtToEff $ Projects.usersByProjectId pid
  let emails' = (\u -> CI.original u.email) <$> projectMembers
  _ <- dbtToEff $ execute q (V.fromList notifsTxt, phone, V.fromList emails, newCompleted, pid)
  addRespHeaders $ inviteTeamMemberModal pid (V.fromList $ ordNub $ emails <> V.toList emails')


checkIntegrationGet :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
checkIntegrationGet pid languageM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let stepsCompleted = project.onboardingStepsCompleted
      newCompleted = insertIfNotExist "Integration" stepsCompleted
      extrQ = case languageM of
        Just lg ->
          let l = fromMaybe "" (getOtelLangVersion lg)
           in "and resource ->> 'telemetry.sdk.language' = '" <> l <> "'"
        _ -> ""
      q = [text|SELECT span_id, span_name FROM telemetry.spans WHERE project_id = ? $extrQ|]
  v <- dbtToEff (queryOne (Query $ encodeUtf8 q) (Only pid) :: (DBT IO (Maybe (Text, Text))))
  if isJust v
    then do
      _ <- dbtToEff $ execute [sql|update projects.projects set onboarding_steps_completed=? where id=?|] (newCompleted, pid)
      case languageM of
        Just lg -> addRespHeaders verifiedCheck
        _ -> do
          redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Pricing"
          addRespHeaders ""
    else throwError (err401{errBody = "No events found yet"})


verifiedCheck :: Html ()
verifiedCheck = div_ [class_ "flex items-center gap-2 text-textSuccess"] do
  span_ "verified"
  faSprite_ "circle-check" "regular" "h-4 w-4"


onboardingInfoPost :: Projects.ProjectId -> OnboardingInfoForm -> ATAuthCtx (RespHeaders (Html ()))
onboardingInfoPost pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let firstName = form.firstName
      lastName = form.lastName
  let infoJson =
        KM.fromList
          [ ("companyName", AE.toJSON form.companyName)
          , ("companySize", AE.toJSON form.companySize)
          , ("foundUsFrom", AE.toJSON form.whereDidYouHearAboutUs)
          ]
      questions = case project.questions of
        Just (AE.Object o) -> AE.Object $ infoJson <> o
        _ -> AE.Object infoJson
      jsonBytes = AE.encode questions
      stepsCompleted = project.onboardingStepsCompleted
      newCompleted = insertIfNotExist "Info" stepsCompleted
  _ <- dbtToEff $ execute [sql| update projects.projects set title=?,questions=?,onboarding_steps_completed=? where id=? |] (form.companyName, jsonBytes, newCompleted, pid)
  _ <- dbtToEff $ execute [sql| update users.users set first_name= ?, last_name=? where id=? |] (firstName, lastName, sess.user.id)
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Survey"
  addRespHeaders ""


onboardingConfPost :: Projects.ProjectId -> OnboardingConfForm -> ATAuthCtx (RespHeaders (Html ()))
onboardingConfPost pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let infoJson =
        KM.fromList
          [ ("functionality", AE.toJSON form.functionality)
          , ("location", AE.toJSON form.location)
          ]
      questions = case project.questions of
        Just (AE.Object o) -> AE.Object $ infoJson <> o
        _ -> AE.Object infoJson
      jsonBytes = AE.encode questions
      stepsCompleted = project.onboardingStepsCompleted
      newCompleted = insertIfNotExist "Survey" stepsCompleted
  _ <- dbtToEff $ execute [sql| update projects.projects set questions=?, onboarding_steps_completed=? where id=? |] (jsonBytes, newCompleted, pid)
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
  addRespHeaders ""


onboardingCompleteBody :: Projects.ProjectId -> Html ()
onboardingCompleteBody pid = do
  div_ [class_ "w-[550px] h-full flex items-center mx-auto relative"] $ do
    canvas_ [id_ "drawing_canvas", class_ "absolute top-0 left-0  w-full"] pass
    div_ [class_ "flex-col gap-4 flex w-full p-14 my-auto border border-weak rounded-2xl"] $ do
      div_ [class_ "p-3 bg-fillSuccess-weak rounded-full w-max border-strokeSuccess-weak gap-2 inline-flex"]
        $ faSprite_ "circle-check" "regular" "h-8 w-8 text-iconSuccess"
      div_ [class_ "flex flex-col gap-2"] do
        h3_ [class_ " text-textStrong font-semibold text-2xl"] "Onboarding completed!"
        p_ [class_ " text-textWeak text-sm"] "You're all set! You can now start using exploring the apitoolkit dashboard by clicking the button below."
      a_ [class_ "btn-primary py-2 font-semibold rounded-lg text-center mt-1", href_ $ "/p/" <> pid.toText <> "/"] "Go to your dashboard"
  script_ [src_ "/public/assets/js/confetti.js"] ("" :: Text)


pricingPage :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
pricingPage pid lemon critical paymentPlan = do
  div_ [class_ "w-[1100px] mx-auto mt-[70px] mb-10 mx-auto"] $ do
    div_ [class_ "flex-col gap-6 flex w-full"] $ do
      div_ [class_ "w-1/2"] $ do
        stepIndicator 6 "Please pick a plan" $ "/p/" <> pid.toText <> "/onboarding?step=Integration"
      paymentPlanPicker pid lemon critical paymentPlan
      div_ [class_ "flex flex-col gap-2 w-full"] do
        span_ [class_ " text-textStrong text-2xl font-semibold mt-20"] "FAQ"
        div_ [class_ "flex flex-col mt-4 w-full"] do
          faQ "What is an event?" "An event is any of span, log, or metric that you send to APItoolkit."
          faQ "How do you handle security and sensitive data?" "We employ encryption and authentication measures to ensure the security of your data during transmission and storage. All our SDKs also support redacting data. You can simply specify the JSONPath to the fields that you don't want the SDKs to forward to APItoolkit, and those sensitive fields will be stripped out/redacted before the data even leaves your servers and replaced with the text \"CLIENT REDACTED\" on our end. We will never see anything you don't want us to see."
          faQ "What makes us better than others?" "Aside the observerbility features like traces, logs, metrics etc. APItoolkit takes it a step further by monitoring request payloads for both incoming and outgoing requests, automatic error reportings like sentry and payload changes detections which gives engineering teams with all the information the need to seamlessly debug and fix issues in their servers."


-- Group is a tuple of (Group Name, List of languages in that group)
integrationGroups :: [(Text, [(Text, Text, [(Text, Text, Text)])])]
integrationGroups =
  [
    ( "Applications"
    ,
      [
        ( "js"
        , "Javascript"
        ,
          [ ("ExpressJS", "express-icon.png", "nodejs/expressjs")
          , ("AdonisJS", "adonis-icon.svg", "nodejs/adonisjs")
          , ("Fastify", "fastify-icon.png", "nodejs/fastifyjs")
          , ("NestJS", "nest-icon.png", "nodejs/nestjs")
          , ("NextJS", "next-icon.svg", "nodejs/nextjs")
          ]
        )
      ,
        ( "go"
        , "Golang"
        ,
          [ ("Chi", "chi-logo.svg", "golang/chi")
          , ("Echo", "echo-logo.png", "golang/echo")
          , ("Fiber", "fiber-logo.svg", "golang/fiber")
          , ("Gin", "gin-logo.png", "golang/gin")
          , ("Gorilla Mux", "mux-logo.png", "golang/gorillamux")
          , ("Native", "go-logo.svg", "golang/native")
          ]
        )
      ,
        ( "py"
        , "Python"
        ,
          [ ("Django", "django-icon.png", "python/django")
          , ("FastAPI", "fastapi-icon.png", "python/fastapi")
          , ("Flask", "flask-icon.png", "python/flask")
          , ("Pyramid", "pyramid-icon.png", "python/pyramid")
          ]
        )
      ,
        ( "elixir"
        , "Elixir"
        , [("Phoenix", "phoenix-logo.png", "elixir/phoenix")]
        )
      ,
        ( "php"
        , "PHP"
        ,
          [ ("Laravel", "laravel-icon.png", "php/laravel")
          , ("Slim", "slim-icon.png", "php/slim")
          , ("Symfony", "symfony-icon.png", "php/symfony")
          ]
        )
      ,
        ( "java"
        , "Java"
        , [("Spring Boot", "springboot-logo.svg", "java/springboot")]
        )
      ,
        ( "cs"
        , "C#"
        , [(".Net Core", "netcore-logo.png", "dotnet/dotnetcore")]
        )
      ]
    )
  ,
    ( "Infrastructure"
    ,
      [
        ( "linux"
        , "Linux"
        , [("Linux", "linux.svg", "infrastructure/linux")]
        )
      ,
        ( "docker"
        , "Docker"
        , [("Docker", "docker.svg", "infrastructure/docker")]
        )
      ,
        ( "kubernetes"
        , "Kubernetes"
        , [("Kubernetes", "kubernetes.svg", "infrastructure/kubernetes")]
        )
      ,
        ( "kafka"
        , "Kafka"
        , [("Kafka", "kafka.svg", "infrastructure/kafka")]
        )
      ]
    )
  ,
    ( "Databases"
    ,
      [
        ( "postgresql"
        , "PostgreSQL"
        , [("PostgreSQL", "postgresql.svg", "databases/postgres")]
        )
      ,
        ( "mongodb"
        , "MongoDB"
        , [("MongoDB", "mongodb.svg", "databases/mongodb")]
        )
      ,
        ( "mysql"
        , "MySQL"
        , [("MySQL", "mysql.svg", "databases/mysql")]
        )
      ]
    )
  ]


-- IMPORtANT: DO NOT DELETE. Needed for the tailwindcss to generate classes.
-- [class_ "group-has-[#check-js:checked]/pg:block group-has-[#check-go:checked]/pg:block group-has-[#check-elixir:checked]/pg:block group-has-[#check-py:checked]/pg:block group-has-[#check-java:checked]/pg:block"]
-- [class_ "group-has-[#check-php:checked]/pg:block group-has-[#check-cs:checked]/pg:block"]
-- [class_ "group-has-[#check-linux:checked]/pg:block group-has-[#check-docker:checked]/pg:block group-has-[#check-kubernetes:checked]/pg:block group-has-[#check-kafka:checked]/pg:block"]
-- [class_ "group-has-[#check-postgresql:checked]/pg:block group-has-[#check-mongodb:checked]/pg:block group-has-[#check-mysql:checked]/pg:block"]
-- [class_ "text-left pb-12 prose prose-slate dark:prose-invert prose-headings:font-medium prose-a:text-secondary prose-a:underline prose-a:underline-offset-4 prose-img:w-full prose-img:rounded-md prose-img:drop-shadow-md prose-img:border prose-img:border-base-200 prose-p:leading-relaxed prose-headings:scroll-mt-40 prose-pre:p-0 before:prose-li:bg-secondary before:prose-li:text-secondary prose-strong:font-medium prose-secondary w-full"]

integrationsPage :: Projects.ProjectId -> Text -> Html ()
integrationsPage pid apikey =
  div_ [class_ "w-full flex h-screen overflow-hidden group/pg"] do
    div_ [class_ "w-1/2 bg-bgRaised h-full flex flex-col"] do
      div_ [class_ "pt-[156px] px-12 flex-shrink-0"]
        $ div_ [class_ "max-w-[550px]"]
        $ stepIndicator 5 "Instrument your apps or servers"
        $ "/p/"
          <> pid.toText
          <> "/onboarding?step=NotifChannel"
      div_ [class_ "flex-col w-full gap-4 flex mt-4 px-12 overflow-y-auto flex-grow"] do
        p_ [class_ "text-textStrong"] do
          "Send Logs, Metrics or Traces. Select an item below for instructions. "
          br_ []
          "Click proceed when you're done integrating your applications."

        div_ [class_ "my-6 p-4 bg-fillWeak border border-strokeWeak rounded-xl"] do
          div_ [class_ "mb-2 text-textStrong font-semibold"] "Your API Key"
          div_ [class_ "flex items-center gap-2"] do
            div_ [class_ "flex-1 font-mono bg-bgBase p-3 border border-strokeWeak rounded-lg overflow-x-auto", id_ "api-key-display"] $ toHtml apikey
            button_
              [ class_ "px-4 py-2 bg-fillBrand-strong rounded-xl text-textInverse-strong flex items-center gap-1 hover:bg-fillBrand-strong/90 cursor-pointer"
              , type_ "button"
              , onpointerdown_ "navigator.clipboard.writeText(document.getElementById('api-key-display').textContent); this.innerHTML = '<span>Copied!</span><svg class=\"h-4 w-4\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M5 13l4 4L19 7\"></path></svg>';"
              ]
              do
                span_ "Copy"
                faSprite_ "copy" "regular" "h-4 w-4"

        -- Display integration groups
        forM_ integrationGroups \(groupName, langsList) -> div_ [class_ "mb-6"] do
          div_ [class_ "text-textStrong font-semibold text-xl mb-2"] $ toHtml groupName
          div_ [class_ "grid grid-cols-2 gap-2"]
            $ forM_ langsList \(lang, langName, _) ->
              languageItem pid langName lang

        div_ [class_ "flex items-center gap-4 py-8"] do
          button_ [class_ "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center", hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check", hxSwap_ "none", hxIndicator_ "#loadingIndicator"] "Confirm & Proceed"
          a_
            [ class_ "px-4 py-3 flex items-center underline text-textBrand text-xl cursor-pointer"
            , type_ "button"
            , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/skip?step=Integration"
            ]
            "Skip"

    div_ [class_ "w-1/2 h-full overflow-hidden border-l border-weak"] do
      div_ [class_ "h-full flex flex-col"] do
        div_ [class_ "w-full h-full overflow-y-auto rounded-2xl blue-gradient-box bg-bgBase"] do
          -- Welcome content shown when no integration is selected
          div_ [class_ "p-12 text-center group-has-[.checkbox:checked]/pg:hidden flex items-center justify-center h-full"] do
            div_ [class_ "flex flex-col w-full items-center gap-8"] do
              -- Icon/graphic
              div_ [class_ "p-6 bg-fillWeak rounded-full"] do
                faSprite_ "brackets-curly" "regular" "h-16 w-16 text-textBrand"

              -- Welcome text
              h2_ [class_ "text-3xl text-textStrong"] "👈 Select your stack on the left to begin"
              p_ [class_ "text-lg text-textWeak max-w-md"] do
                "You can also check out our youtube videos for more interactive walkthorughs."

                -- YouTube video embeds
                div_ [class_ "grid grid-cols-2 gap-4 w-full"] do
                  -- Video 1
                  div_ [class_ "relative overflow-hidden rounded-lg border border-weak", style_ "padding-bottom: 56.25%;"] do
                    iframe_
                      [ class_ "absolute top-0 left-0 w-full h-full"
                      , src_ "https://www.youtube.com/embed/Q-tGuIkDmyk?si=BIHn2vN1m9gDs_9v"
                      , title_ "YouTube video player"
                      , term "frameborder" "0"
                      , term "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                      , term "referrerpolicy" "strict-origin-when-cross-origin"
                      , term "allowfullscreen" ""
                      ]
                      ""

                  -- Video 2 (replace VIDEO_ID_2 with actual ID)
                  div_ [class_ "relative overflow-hidden rounded-lg border border-weak", style_ "padding-bottom: 56.25%;"] do
                    iframe_
                      [ class_ "absolute top-0 left-0 w-full h-full"
                      , src_ "https://www.youtube.com/embed/OALS4ckfOdI"
                      , title_ "YouTube video player"
                      , term "frameborder" "0"
                      , term "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                      , term "referrerpolicy" "strict-origin-when-cross-origin"
                      , term "allowfullscreen" ""
                      ]
                      ""

                div_ [class_ "text-center mt-3"]
                  $ a_ [href_ "https://www.youtube.com/@apitoolkit", target_ "_blank", class_ "text-textBrand hover:underline text-sm font-medium"] do
                    "Watch more tutorials →"

          -- Display guides for all integration options
          forM_ integrationGroups \(_, integrations) -> do
            forM_ integrations \(lang, langName, frameworks) ->
              div_ [class_ $ "p-4 lang-guide hidden group-has-[#check-" <> lang <> ":checked]/pg:block", id_ $ lang <> "_main"] do
                div_ [class_ "px-8 sticky top-0 z-10 bg-bgBase py-2"]
                  $ div_ [class_ "inline-block tabs tabs-box tabs-outline p-0 bg-bgBase text-textWeak border ", role_ "tablist"]
                  $ forM_ (zip [0 ..] frameworks) \(idx, (fwName, fwIcon, fwPath)) ->
                    label_ [class_ "tab gap-2 items-center", Lucid.for_ $ "fw-tab-" <> lang <> "-" <> show idx] do
                      input_
                        $ [ type_ "radio"
                          , name_ $ "tab-" <> lang
                          , id_ $ "fw-tab-" <> lang <> "-" <> show idx
                          , class_ "hidden"
                          , Aria.label_ fwName
                          , hxGet_ $ "/proxy/docs/sdks/" <> fwPath
                          , hxTarget_ $ "#fw-content-" <> lang
                          , hxTrigger_ "change"
                          , hxSwap_ "innerHTML"
                          , hxSelect_ "#mainArticle"
                          , hxIndicator_ $ "#fw-indicator-" <> lang
                          ]
                          <> [checked_ | idx == 0]
                      unless (T.null fwIcon) $ img_ [class_ "h-5 w-5", src_ $ "https://apitoolkit.io/assets/img/framework-logos/" <> fwIcon]
                      span_ $ toHtml fwName

                div_ [class_ "relative p-8"] do
                  div_ [id_ $ "fw-indicator-" <> lang, class_ "htmx-indicator flex justify-center py-5"]
                    $ span_ [class_ "loading loading-dots loading-md"] ""
                  div_
                    [ id_ $ "fw-content-" <> lang
                    , hxGet_ $ "/proxy/docs/sdks/" <> thd3 (frameworks Unsafe.!! 0)
                    , hxTrigger_ "load"
                    , hxSwap_ "innerHTML"
                    , hxSelect_ "#mainArticle"
                    , class_ "prose-a:!text-textBrand prose-a:!underline"
                    ]
                    ""


languageItem :: Projects.ProjectId -> Text -> Text -> Html ()
languageItem pid lang ext = do
  label_
    [ class_ "group/li cols-span-1 h-12 px-3 py-2 bg-transparent rounded-xl border border-strokeWeak justify-start items-center gap-3 inline-flex cursor-pointer"
    ]
    do
      input_
        [ type_ "checkbox"
        , class_ "checkbox shrink-0"
        , id_ $ "check-" <> ext
        , value_ ext
        , onchange_ $ "if(this.checked) { document.getElementById('" <> ext <> "_main').scrollIntoView({behavior: 'smooth'}); }"
        ]
      div_ [class_ "flex w-full items-center justify-between"] do
        div_ [class_ "flex items-center gap-2 text-sm font-semibold"] do
          img_ [class_ "h-5 w-5", src_ $ "/public/assets/svgs/" <> ext <> ".svg"]
          span_ $ toHtml lang
        div_ [class_ "hidden group-has-[.checkbox:checked]/li:block text-sm toggle-target", id_ $ "integration-check-container" <> T.replace "#" "" lang] do
          div_
            [ class_ "flex items-center gap-2 "
            , hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check?language=" <> T.replace "#" "sharp" lang
            , hxSwap_ "innerHTML"
            , hxTarget_ $ "#integration-check-" <> ext
            , hxTrigger_ "load delay:5s"
            ]
            do
              span_ [class_ " text-textStrong"] "waiting for events"
              faSprite_ "spinner" "regular" "h-4 w-4 animate-spin"


-- Helper function to render connection status button
connectionStatusButton :: Bool -> Text -> Html ()
connectionStatusButton isConnected connectUrl
  | isConnected = button_ [class_ "text-textSuccess font-semibold"] "Connected"
  | otherwise =
      a_
        [ target_ "_blank"
        , class_ "border px-3 h-8 flex items-center shadow-xs border-[var(--brand-color)] rounded-lg text-textBrand font-semibold"
        , href_ connectUrl
        ]
        "Connect"


-- Helper function to render integration card
integrationCard :: Text -> Text -> Bool -> Text -> Html ()
integrationCard serviceName iconPath isConnected connectUrl = do
  div_ [class_ "px-3 py-2 rounded-xl border border-strokeWeak bg-fillWeak justify-between items-center flex"] $ do
    div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
      img_ [src_ iconPath]
      span_ [class_ "text-center text-textStrong text-xl"] $ toHtml serviceName
    connectionStatusButton isConnected connectUrl


-- Helper function to render form field with label
formField :: Text -> Text -> Text -> Text -> Text -> Html ()
formField labelText inputType inputName inputId inputValue = do
  div_ [class_ "flex flex-col gap-2"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ "text-textStrong lowercase first-letter:uppercase"] $ toHtml labelText
    if inputType == "textarea"
      then textarea_ [class_ "textarea w-full rounded-lg border border-strokeStrong", type_ "text", name_ inputName, id_ inputId] ""
      else input_ [class_ "input w-full h-12", type_ inputType, name_ inputName, id_ inputId, value_ inputValue]


notifChannels :: AuthContext -> Projects.ProjectId -> Text -> V.Vector Text -> Bool -> Bool -> Html ()
notifChannels appCtx pid phone emails hasDiscord hasSlack = do
  let slackRedirectUri = appCtx.env.slackRedirectUri
      discordUri = appCtx.env.discordRedirectUri
      slackUrl = "https://slack.com/oauth/v2/authorize?client_id=" <> appCtx.config.slackClientId <> "&scope=chat:write,commands,incoming-webhook,files:write,app_mentions:read,channels:history,groups:history,im:history,mpim:history&user_scope=" <> "&redirect_uri=" <> slackRedirectUri <> pid.toText <> "?onboarding=true"
      discordUrl = "https://discord.com/oauth2/authorize?response_type=code&client_id=" <> appCtx.config.discordClientId <> "&permissions=277025392640&integration_type=0&scope=bot+applications.commands" <> "&state=" <> pid.toText <> "__onboarding" <> "&redirect_uri=" <> discordUri

  div_ [class_ "w-[550px] mx-auto mt-[156px] mb-10"] $ do
    div_ [id_ "inviteModalContainer"] pass
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 4 "How should we notify you about issues?" $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
      div_ [class_ "flex-col w-full gap-8 flex mt-4"] $ do
        div_ [class_ "w-full flex flex-col gap-8"] $ do
          div_ [class_ "w-full gap-2 grid grid-cols-2"] $ do
            integrationCard "Slack" "/public/assets/svgs/slack.svg" hasSlack slackUrl
            integrationCard "Discord" "/public/assets/svgs/discord.svg" hasDiscord discordUrl
          form_
            [ class_ "flex flex-col gap-8"
            , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/phone-emails"
            , hxExt_ "json-enc"
            , hxVals_ "js:{phoneNumber: document.getElementById('phone').value , emails: getTags()}"
            , hxTarget_ "#inviteModalContainer"
            , hxSwap_ "innerHTML"
            , hxIndicator_ "#loadingIndicator"
            ]
            $ do
              formField "Notify phone number" "text" "phoneNumber" "phone" phone
              formField "Notify the following email address" "textarea" "emails" "emails_input" ""
              div_ [class_ "items-center gap-4 flex"] $ do
                button_ [class_ "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center"] "Proceed"
      let tgs = decodeUtf8 $ AE.encode $ V.toList emails
      script_
        [text|
     document.addEventListener('DOMContentLoaded', function() {
      var inputElem = document.querySelector('#emails_input')
      var tagify = new Tagify(inputElem)
      tagify.addTags($tgs);
      window.tagify = tagify
    })

    function appendMember() {
      const email = document.querySelector('#add-member-input').value
      if(email.length < 1) return
      const node = document.querySelector("#member-template").cloneNode(true)
      node.removeAttribute('id')
      node.querySelector('input').value = email
      node.querySelector('input').setAttribute('name', 'emails')
      node.querySelector('span').textContent = email
      node.classList.remove('hidden')
      document.querySelector('#members-container').appendChild(node)
       _hyperscript.processNode(node)
       document.querySelector('#add-member-input').value = ''
    }
  |]


createMonitorPage :: Projects.ProjectId -> Maybe Testing.Collection -> Html ()
createMonitorPage pid colM = do
  div_ [class_ "w-[550px] mx-auto mt-[156px] mb-10"] $ do
    let collectionStepsJSON = AE.encode $ maybe (Testing.CollectionSteps []) (.collectionSteps) colM
    script_ [fmt| window.collectionSteps = {collectionStepsJSON};|]
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 3 "Let's create your first endpoint monitor" $ "/p/" <> pid.toText <> "/onboarding?step=Survey"
      form_
        [ class_ "flex-col w-full gap-8 flex"
        , hxPost_ $ "/p/" <> pid.toText <> "/monitors/collection?onboarding=true"
        , hxExt_ "json-enc"
        , hxVals_ "js:{stepsData: saveStepData()}"
        , hxIndicator_ "#loadingIndicator"
        ]
        $ do
          input_ [class_ "input w-full h-12", type_ "hidden", name_ "title", value_ "HEALTH CHECK."]
          whenJust colM $ \col -> do
            input_ [type_ "hidden", name_ "collectionId", value_ col.id.toText]
          div_ [class_ "w-full"] $ termRaw "assertion-builder" [id_ ""] ""
          div_ [class_ "w-full"] $ termRaw "steps-editor" [id_ "stepsEditor", term "isOnboarding" "true"] ""
          div_ [class_ "items-center gap-4 flex"] $ do
            button_ [class_ "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center", type_ "submit"] "Proceed"
            button_
              [ class_ "px-4 py-3 flex items-center underline text-textBrand text-xl cursor-pointer"
              , type_ "button"
              , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/skip?step=CreateMonitor"
              ]
              "Skip"


onboardingInfoBody :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Html ()
onboardingInfoBody pid firstName lastName cName cSize fUsFrm = do
  div_ [class_ "w-[550px] mx-auto mt-[156px]"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 1 "Tell us a little bit about you" ""
      form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/info", hxIndicator_ "#loadingIndicator"] $ do
        div_ [class_ "flex-col w-full gap-4 mt-4 flex"] $ do
          mapM_ createInputField [("first Name" :: Text, firstName), ("last Name", lastName), ("company Name", cName)]
          createSelectField cSize "company Size" [("1 - 4", "1 to 4"), ("5 - 10", "5 to 10"), ("11 - 25", "11 to 25"), ("26+", "26 and above")]
          createSelectField fUsFrm "where Did You Hear About Us" [("google", "Google"), ("twitter", "Twitter"), ("linkedin", "LinkedIn"), ("friend", "Friend"), ("other", "Other")]
        div_ [class_ "items-center gap-1 flex"] $ do
          button_ [class_ "btn-primary px-6 py-4 text-xl rounded-lg cursor-pointer flex items-center"] "Proceed"


onboardingConfigBody :: Projects.ProjectId -> Text -> [Text] -> Html ()
onboardingConfigBody pid loca func = do
  div_ [class_ "w-[550px] mx-auto mt-[156px]"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 2 "Let's configure your project" $ "/p/" <> pid.toText <> "/onboarding?step=Info"
      form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/survey", hxIndicator_ "#loadingIndicator"] $ do
        div_ [class_ "flex-col w-full gap-14 mt-4 flex"] $ do
          div_ [class_ "flex-col gap-2 flex"] $ do
            div_ [class_ "items-center gap-[2px] flex"] $ do
              span_ [class_ " text-textStrong"] "Where should your project be hosted?"
              span_ [class_ " text-textWeak"] "*"
            div_ [class_ "pt-2 flex-col gap-4 flex text-sm  text-textStrong"] $ do
              forM_ locations $ createBinaryField "radio" "location" [loca]
          div_ [class_ "flex-col gap-2 flex"] $ do
            div_ [class_ "items-center flex gap-[2px]"] $ do
              span_ [class_ " text-textStrong"] "Which APItoolkit features will you be using?"
              span_ [class_ " text-textWeak"] "*"
            div_ [class_ "pt-2 flex-col gap-4 flex"] $ do
              forM_ functionalities $ createBinaryField "checkbox" "functionality" func
        div_ [class_ "items-center gap-1 flex"] $ do
          button_ [class_ "btn-primary px-6 py-4 text-xl rounded-lg cursor-pointer flex items-center"] "Proceed"


inviteTeamMemberModal :: Projects.ProjectId -> V.Vector Text -> Html ()
inviteTeamMemberModal pid emails = do
  div_ [id_ "invite-modal-container"] $ do
    input_ [type_ "checkbox", id_ "inviteModal", class_ "modal-toggle", checked_]
    div_ [class_ "modal p-8", role_ "dialog"] do
      universalIndicator
      div_ [class_ "modal-box bg-bgRaised flex flex-col gap-4"] $ do
        div_ [class_ "p-3 bg-fillSuccess-weak rounded-full w-max border-strokeSuccess-weak gap-2 inline-flex"]
          $ faSprite_ "circle-check" "regular" "h-6 w-6 text-iconSuccess"
        span_ [class_ " text-textStrong text-2xl font-semibold"] "We've sent you a test notification"
        div_ [class_ "text-textWeak"] "No notification? Close this modal and verify emails and channels."
        div_ [class_ "h-1 w-full  bg-fillWeak"] pass
        div_ [class_ "flex-col gap-4 flex"] $ do
          div_ [class_ "flex-col gap-5 flex"] $ do
            div_ [class_ "w-full text-textWeak"] "The users below will be added to your project as team members"
            div_ [class_ "w-full gap-4 flex flex-col"] $ do
              div_ [class_ "w-full gap-2 flex items-center"] $ do
                div_ [class_ "flex-col gap-1 inline-flex w-full"]
                  $ div_ [class_ "flex flex-col gap-1 w-full"]
                  $ do
                    input_ [class_ "input input-sm w-full", placeholder_ "email@example.com", type_ "email", id_ "add-member-input"]
                button_ [class_ "btn-primary rounded-xl px-4 py-2 justify-center items-center flex text-white text-sm cursor-pointer", onpointerdown_ "appendMember()"] "invite"
              div_ [class_ "w-full"] $ do
                div_ [class_ "w-full  text-textStrong text-sm font-semibold"] "Members"
                div_ [class_ "w-full border-t border-weak"] $ do
                  form_
                    [ class_ "flex-col flex"
                    , id_ "members-container"
                    , hxPost_ $ "/p/" <> pid.toText <> "/manage_members?onboarding=true"
                    , hxIndicator_ "#loadingIndicator"
                    ]
                    $ do
                      inviteMemberItem "hidden"
                      forM_ emails $ \email -> do
                        inviteMemberItem email
        div_ [class_ "modal-action w-full flex items-center justify-start gap-4 mt-2"] do
          button_ [class_ "btn-primary px-8 py-2 text-lg rounded-xl cursor-pointer flex items-center", type_ "button", onpointerdown_ "htmx.trigger('#members-container', 'submit')"] "Proceed"
          label_ [class_ "text-textBrand underline cursor-pointer", Lucid.for_ "inviteModal"] "Back"


functionalities :: [(Text, Text)]
functionalities =
  [ ("logs", "Logs, Metrics and Traces")
  , ("analytics", "API Analytics and Payload Monitoring")
  , ("healthchecks", "Website/API Healthchecks and Status Pages")
  , ("changeTracking", "API (Breaking) Change Tracking")
  , ("customDashboards", "Custom Dashboards")
  ]
locations :: [(Text, Text)]
locations =
  [ ("usa", "USA")
  , ("eu", "EU")
  , ("asia", "Asia")
  ]


inviteMemberItem :: Text -> Html ()
inviteMemberItem email = do
  let hide = email == "hidden"
  div_ (class_ ("flex  py-1 w-full justify-between items-center border-b border-strokeWeak " <> if hide then "hidden" else "") : [id_ "member-template" | hide]) do
    div_ [class_ "pr-6 py-1  w-full justify-start items-center inline-flex"] do
      input_ ([type_ "hidden", value_ email] ++ [name_ "emails" | not hide])
      span_ [class_ "text-textStrong text-sm font-normal"]
        $ toHtml email
    select_ [name_ "permissions", class_ "select select-xs"] do
      option_ [class_ "text-textWeak", value_ "admin"] "Admin"
      option_ [class_ "text-textWeak", value_ "edit"] "Can Edit"
      option_ [class_ "text-textWeak", value_ "view"] "Can View"
    button_
      [ [__| on click remove the closest parent <div/> then halt |]
      , class_ "text-textBrand ml-4 text-sm underline"
      , type_ "button"
      ]
      "remove"


createInputField :: (Text, Text) -> Html ()
createInputField (labelText, value) = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ " text-textStrong lowercase first-letter:uppercase"] (toHtml labelText)
      span_ [class_ " text-textWeak"] "*"
    input_ [class_ "input w-full h-12", type_ "text", name_ $ T.replace " " "" labelText, required_ "required", value_ value]


createSelectField :: Text -> Text -> V.Vector (Text, Text) -> Html ()
createSelectField val labelText options = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ " text-textStrong lowercase first-letter:uppercase"] $ toHtml labelText
      span_ [class_ " text-textWeak"] "*"
    select_ [class_ "select w-full h-12", name_ $ T.replace " " "" labelText, required_ "required"] do
      option_ [value_ ""] ""
      forM_ options $ \(key, value) -> option_ (value_ key : [selected_ val | val == key]) $ toHtml value


createBinaryField :: Text -> Text -> [Text] -> (Text, Text) -> Html ()
createBinaryField kind name selectedValues (value, label) = do
  div_ [class_ " items-center gap-3 inline-flex"] $ do
    let checked = value `elem` selectedValues
    input_ $ [class_ "w-6 h-6 rounded-sm", type_ kind, name_ name, value_ value, id_ value] <> [required_ "required" | kind == "radio"] <> [checked_ | checked]
    label_ [class_ " text-textStrong text-sm", Lucid.for_ value] $ toHtml label


stepIndicator :: Int -> Text -> Text -> Html ()
stepIndicator step title prevUrl = do
  universalIndicator
  div_ [class_ "flex-col gap-4 flex w-full"] $ do
    a_ [href_ "/", class_ "absolute top-10 left-10 py-2 pr-2 bg-bgBase rounded-xs"] do
      img_ [class_ "h-7", src_ "/public/assets/svgs/logo.svg"]
    div_ [class_ "flex-col gap-2 flex w-full"] $ do
      div_ [class_ " text-textStrong text-base font-semibold"] $ "Step " <> show step <> " of 6"
      div_ [class_ "grid grid-cols-6 w-full gap-1"] $ do
        forM_ [1 .. 6] $ \i -> div_ [class_ $ "h-2 w-full rounded-sm " <> if step >= i then "btn-primary rounded-sm" else " bg-fillWeak shadow-sm border border-strokeWeak"] pass
      when (step > 1) $ do
        a_ [class_ "flex items-center gap-3 flex text-textBrand w-full mt-2", href_ prevUrl] $ do
          faSprite_ "arrow-left" "regular" "h-4 w-4"
          span_ [] "Back"
    span_ [class_ " text-textStrong text-4xl mt-4"] $ toHtml title


faQ :: Text -> Text -> Html ()
faQ question answer =
  div_ [class_ "w-full py-4 border-t border-weak flex flex-col"] $ do
    button_ [class_ " text-textStrong font-semibold flex w-full justify-between items-center", [__|on click toggle .hidden on the next <div/>|]] do
      span_ [] $ toHtml question
      faSprite_ "chevron-down" "regular" "h-4 w-4  text-textWeak"
    div_ [class_ " text-textWeak font-medium w-full hidden"] $ toHtml answer


universalIndicator :: Html ()
universalIndicator =
  div_ [class_ "fixed  htmx-indicator top-0 left-0 right-0 bottom-0 flex items-center justify-center z-9999", id_ "loadingIndicator"] do
    span_ [class_ "loading loading-dots loading-lg"] ""


-- | Proxy handler for fetching documentation from apitoolkit.io
-- This bypasses CORS restrictions by fetching the content server-side
proxyLandingH :: [Text] -> ATAuthCtx (RespHeaders Text)
proxyLandingH path = do
  let baseUrl = "https://apitoolkit.io/"
      fullUrl = baseUrl <> T.intercalate "/" path

  response <- W.get (toString fullUrl)

  let content = fromMaybe "" $ response L.^? W.responseBody
      textContent = decodeUtf8 content
  addRespHeaders textContent
