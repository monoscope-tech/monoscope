{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pages.Onboarding.Onboarding (
  onboardingGetH,
  onboardingInfoPost,
  onboardingConfPost,
  discorPostH,
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
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Tuple.Extra (thd3)
import Data.Vector as V (Vector, fromList, head, length, toList)
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
import Models.Apis.Slack (getProjectSlackData)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing qualified as Testing
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation (text)
import Network.HTTP.Types (status200)
import Network.Wreq (get, responseBody)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components
import Pkg.Mail (sendDiscordNotif)
import PyF (fmt)
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import Servant (err401, err500, errBody)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, redirectCS)
import Utils (faSprite_, getOtelLangVersion, insertIfNotExist, lookupValueText)
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
                      ( \x -> case x of
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
      let phone = fromMaybe "" project.notifyPhoneNumber
          emails = project.notifyEmails
          hasDiscord = isJust project.discordUrl
          hasSlack = isJust slack
      addRespHeaders $ PageCtx bodyConfig $ notifChannels pid appContx.config.slackRedirectUri phone emails hasDiscord hasSlack
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


data DiscordForm = DiscordForm {url :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data NotifChannelForm = NotifChannelForm
  { phoneNumber :: Text
  , emails :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm)


discorPostH :: Projects.ProjectId -> DiscordForm -> ATAuthCtx (RespHeaders (Html ()))
discorPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let notifs = ordNub $ (map (.toText) (V.toList project.notificationsChannel) <> [(Projects.NDiscord).toText])
  sendDiscordNotif form.url "APItoolkit connected successfully"
  let stepsCompleted = project.onboardingStepsCompleted
      newCompleted = insertIfNotExist "NotifChannel" stepsCompleted
      q = [sql| update projects.projects set onboarding_steps_completed=? where id=? |]
  _ <- dbtToEff do Projects.updateNotificationsChannel pid notifs (Just form.url)
  _ <- dbtToEff $ execute q (newCompleted, pid)
  addRespHeaders $ button_ [class_ "text-green-500 font-semibold"] "Connected"


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
      notifs = if phone /= "" then map (.toText) (V.toList project.notificationsChannel) <> [(Projects.NPhone).toText] else map (.toText) (V.toList project.notificationsChannel)
      notifs' = if emails /= [] then notifs <> [(Projects.NEmail).toText] else notifs
      notifsTxt = ordNub $ notifs'
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
verifiedCheck = div_ [class_ "flex items-center gap-2 text-green-500"] do
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
      div_ [class_ "p-3 bg-[#0acc91]/5 rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"]
        $ faSprite_ "circle-check" "regular" "h-8 w-8 text-green-500"
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


langs :: [(Text, Text, [(Text, Text, Text)])]
langs =
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


-- IMPORtANT: DO NOT DELETE. Needed for the tailwindcss to generate classes.
-- [class_ "group-has-[#check-js:checked]/pg:block group-has-[#check-go:checked]/pg:block group-has-[#check-elixir:checked]/pg:block group-has-[#check-py:checked]/pg:block group-has-[#check-java:checked]/pg:block"]
-- [class_ "group-has-[#check-php:checked]/pg:block group-has-[#check-cs:checked]/pg:block"]
-- [class_ "text-left pb-12 prose prose-slate dark:prose-invert prose-headings:font-medium prose-a:text-secondary prose-a:underline prose-a:underline-offset-4 prose-img:w-full prose-img:rounded-md prose-img:drop-shadow-md prose-img:border prose-img:border-base-200 prose-p:leading-relaxed prose-headings:scroll-mt-40 prose-pre:p-0 before:prose-li:bg-secondary before:prose-li:text-secondary prose-strong:font-medium prose-secondary w-full"]

integrationsPage :: Projects.ProjectId -> Text -> Html ()
integrationsPage pid apikey =
  div_ [class_ "w-full flex h-full group/pg"] do
    div_ [class_ "w-1/2 bg-white pt-[156px] h-full px-12 border-r border-weak"] do
      div_ [class_ " bg-white ml-auto"] do
        div_ [class_ "flex-col gap-4 flex w-full"] do
          div_ [class_ "max-w-[550px]"] $ stepIndicator 5 "Instrument your apps or servers" $ "/p/" <> pid.toText <> "/onboarding?step=NotifChannel"
          div_ [class_ "flex-col w-full gap-8 flex mt-4"] do
            p_ [class_ " text-textStrong"] do
              "Send Logs, Metrics or Traces. Select an item below for instructions. "
              br_ []
              "Click proceed when you're done integrating your applications."
            div_ [class_ "grid grid-cols-2 gap-2"] $ forM_ langs \(lang, langName, _) -> languageItem pid langName lang
            div_ [class_ "flex items-center gap-4"] do
              button_ [class_ "btn btn-primary cursor-pointer", hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check", hxSwap_ "none", hxIndicator_ "#loadingIndicator"] "Confirm & Proceed"
              a_
                [ class_ "px-2 h-14 flex items-center underline text-brand text-xl font-semibold"
                , type_ "button"
                , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/skip?step=Integration"
                ]
                "Skip"
    div_ [class_ "w-1/2 flex items-center px-12"] do
      div_ [class_ "rounded-2xl w-full blue-gradient-box bg-bgBase flex flex-col justify-between items-center h-[90vh]"] do
        div_ [class_ "w-full h-full overflow-y-auto"]
          $ forM_ langs \(lang, langName, frameworks) ->
            div_ [class_ $ "p-4 lang-guide hidden group-has-[#check-" <> lang <> ":checked]/pg:block", id_ $ lang <> "_main"] do
              div_ [class_ "px-8 sticky  top-0 z-10"]
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
                        <> [checked_ | (idx == 0)]
                    unless (T.null fwIcon) $ img_ [class_ "h-5 w-5", src_ $ "https://apitoolkit.io/assets/img/framework-logos/" <> fwIcon]
                    span_ $ toHtml fwName
              br_ []
              div_ [class_ "relative p-8"] do
                div_ [id_ $ "fw-indicator-" <> lang, class_ "htmx-indicator flex justify-center py-5"]
                  $ span_ [class_ "loading loading-dots loading-md"] ""
                div_
                  [ id_ $ "fw-content-" <> lang
                  , hxGet_ $ "/proxy/docs/sdks/" <> thd3 (frameworks Unsafe.!! 0)
                  , hxTrigger_ "load"
                  , hxSwap_ "innerHTML"
                  , hxSelect_ "#mainArticle"
                  , class_ ""
                  ]
                  ""


languageItem :: Projects.ProjectId -> Text -> Text -> Html ()
languageItem pid lang ext = do
  label_
    [ class_ "group/li cols-span-1 h-12 px-3 py-2 bg-[#00157f]/0 rounded-xl border border-[#001066]/10 justify-start items-center gap-3 inline-flex cursor-pointer"
    ]
    do
      input_ [type_ "checkbox", class_ "checkbox shrink-0", id_ $ "check-" <> ext, value_ ext, 
              onchange_ $ "if(this.checked) { document.getElementById('" <> ext <> "_main').scrollIntoView({behavior: 'smooth'}); }"]
      div_ [class_ "flex w-full items-center justify-between"] do
        div_ [class_ "flex items-center gap-2 text-sm font-semibold"] do
          img_ [class_ "h-5 w-5", src_ $ "/public/assets/svgs/" <> ext <> ".svg"]
          span_ $ toHtml lang
        div_ [class_ "hidden group-has-[.checkbox:checked]/li:block text-sm toggle-target", id_ $ "integration-check-container" <> T.replace "#" "" lang] do
          div_
            [ class_ "flex items-center gap-2 "
            , hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check?language=" <> (T.replace "#" "sharp" lang)
            , hxSwap_ "innerHTML"
            , hxTarget_ $ "#integration-check-" <> ext
            , hxTrigger_ "load delay:5s"
            ]
            do
              span_ [class_ " text-textStrong"] "waiting for events"
              faSprite_ "spinner" "regular" "h-4 w-4 animate-spin"


notifChannels :: Projects.ProjectId -> Text -> Text -> Vector Text -> Bool -> Bool -> Html ()
notifChannels pid slackRedirectUri phone emails hasDiscord hasSlack = do
  div_ [class_ "w-[550px] mx-auto mt-[156px] mb-10"] $ do
    div_ [id_ "inviteModalContainer"] pass
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 4 "How should we notify you about issues?" $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
      div_ [class_ "flex-col w-full gap-8 flex mt-4"] $ do
        div_ [class_ "w-full flex flex-col gap-8"] $ do
          div_ [class_ "w-full gap-2 grid grid-cols-2"] $ do
            div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10  bg-fillWeak justify-between items-center flex"] $ do
              div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
                img_ [src_ "/public/assets/svgs/slack.svg"]
                span_ [class_ "text-center text-black text-xl font-semibold"] "Slack"
              let
                -- slackDev = "https://slack.com/oauth/v2/authorize?client_id=6187126212950.6193763110659&scope=chat:write,incoming-webhook&user_scope="
                slackPro = "https://slack.com/oauth/v2/authorize?client_id=6211090672305.6200958370180&scope=chat:write,incoming-webhook&user_scope="
              if hasSlack
                then button_ [class_ "text-green-500 font-semibold"] "Connected"
                else a_
                  [ target_ "_blank"
                  , class_ "border px-3 h-8 flex items-center shadow-xs border-[var(--brand-color)] rounded-lg text-brand font-semibold"
                  , href_ $ slackPro <> "&redirect_uri=" <> slackRedirectUri <> pid.toText <> "?onboarding=true"
                  ]
                  do
                    "Connect"
            div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10  bg-fillWeak justify-between items-center flex"] $ do
              div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
                img_ [src_ "/public/assets/svgs/discord.svg"]
                div_ [class_ "text-center text-black text-xl font-semibold"] "Discord"
              if hasDiscord
                then button_ [class_ "text-green-500 font-semibold"] "Connected"
                else discordModal pid
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
              div_ [class_ "flex flex-col gap-2"] do
                div_ [class_ "flex w-full items-center gap-1"] $ do
                  span_ [class_ " text-textStrong lowercase first-letter:uppercase"] "Notify phone number"
                input_ [class_ "input w-full h-12", type_ "text", name_ "phoneNumber", id_ "phone", value_ phone]
              div_ [class_ "flex flex-col gap-2"] do
                div_ [class_ "flex w-full items-center gap-1"] $ do
                  span_ [class_ " text-textStrong lowercase first-letter:uppercase"] "Notify the following email address"
                textarea_ [class_ "w-full rounded-lg border border-strokeStrong", type_ "text", name_ "emails", id_ "emails_input"] ""
              div_ [class_ "items-center gap-4 flex"] $ do
                button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg cursor-pointer"] "Proceed"
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
            button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg cursor-pointer", type_ "submit"] "Proceed"
            button_
              [ class_ "px-6 h-14 flex items-center border border-[var(--brand-color)] text-brand text-xl font-semibold rounded-lg cursor-pointer"
              , onclick_ "window.addCollectionStep()"
              , type_ "button"
              ]
              "Add a step"
            button_
              [ class_ "px-2 h-14 flex items-center underline text-brand text-xl font-semibold cursor-pointer"
              , type_ "button"
              , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/skip?step=CreateMonitor"
              ]
              "Skip"


discordModal :: Projects.ProjectId -> Html ()
discordModal pid = do
  div_ [id_ "discord-modal"] $ do
    label_ [Lucid.for_ "my_modal_6", class_ "border px-3 h-8 flex items-center shadow-xs border-[var(--brand-color)] rounded-lg text-brand font-semibold"] "Connect"
    input_ [type_ "checkbox", id_ "my_modal_6", class_ "modal-toggle"]
    div_ [class_ "modal", role_ "dialog"] do
      div_ [class_ "modal-box"] $ do
        div_
          [ hxPost_ $ "/p/" <> pid.toText <> "/onboarding/discord"
          , hxTarget_ "#discord-modal"
          , hxSwap_ "innerHTML"
          , id_ "dscrd"
          , hxVals_ "js:{url: document.getElementById('discord-url').value}"
          ]
          do
            div_ [[__| on click halt|]] do
              h3_ [class_ "text-lg font-bold"] "Enter discord webhook URL"
              p_ [class_ "py-4 flex items-center gap-1"] do
                "Enter your preferred channels' webhook url. You can find it in the channel settings."
              input_ [type_ "text", class_ "input w-full h-12", id_ "discord-url", name_ "url", required_ "required"]
            div_ [class_ "modal-action"] do
              button_ [class_ "btn btn-primary rounded-lg btn-sm", type_ "button", onclick_ "htmx.trigger('#dscrd','submit')"] "Submit"
      label_ [class_ "modal-backdrop", Lucid.for_ "my_modal_6"] "Close"


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
          button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg cursor-pointer"] "Proceed"


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
          button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg cursor-pointer"] "Proceed"


inviteTeamMemberModal :: Projects.ProjectId -> Vector Text -> Html ()
inviteTeamMemberModal pid emails = do
  div_ [id_ "invite-modal-container"] $ do
    input_ [type_ "checkbox", id_ "inviteModal", class_ "modal-toggle", checked_]
    div_ [class_ "modal p-8", role_ "dialog"] do
      universalIndicator
      div_ [class_ "modal-box flex flex-col gap-4"] $ do
        div_ [class_ "p-3 bg-[#0acc91]/5 rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"]
          $ faSprite_ "circle-check" "regular" "h-6 w-6 text-green-500"
        span_ [class_ " text-textStrong text-2xl font-semibold"] "Test notifications sent"
        div_ [class_ "text-[#000833]/60"] "No notification? Close this modal and verify emails and channels."
        div_ [class_ "h-1 w-full  bg-fillWeak"] pass
        div_ [class_ "flex-col gap-4 flex"] $ do
          div_ [class_ "flex-col gap-5 flex"] $ do
            div_ [class_ "w-full text-[#000833]/60"] "The users below will be added to your project as team members"
            div_ [class_ "w-full gap-4 flex flex-col"] $ do
              div_ [class_ "w-full gap-2 flex items-center"] $ do
                div_ [class_ "flex-col gap-1 inline-flex w-full"]
                  $ div_ [class_ "flex flex-col gap-1 w-full"]
                  $ do
                    input_ [class_ "input input-sm w-full", placeholder_ "email@example.com", type_ "email", id_ "add-member-input"]
                button_ [class_ "btn-primary rounded-lg  px-3 h-8 justify-center items-center flex text-white text-sm font-semibold", onclick_ "appendMember()"] "invite"
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
        div_ [class_ "modal-action w-full flex items-center justify-start gap-2 mt-2"] do
          button_ [class_ "btn btn-primary font-semibold rounded-lg cursor-pointer", type_ "button", onclick_ "htmx.trigger('#members-container', 'submit')"] "Proceed"
          label_ [class_ "text-brand font-semibold underline", Lucid.for_ "inviteModal"] "Close"


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
  div_ (class_ ("flex  py-1 w-full justify-between items-center border-b border-[#001066]/10 " <> if hide then "hidden" else "") : [id_ "member-template" | hide]) do
    div_ [class_ "pr-6 py-1  w-full justify-start items-center inline-flex"] do
      input_ ([type_ "hidden", value_ email] ++ [name_ "emails" | not hide])
      span_ [class_ "text-[#000626]/90 text-sm font-normal"]
        $ toHtml email
    select_ [name_ "permissions", class_ "select select-xs"] do
      option_ [class_ "text-gray-500", value_ "admin"] "Admin"
      option_ [class_ "text-gray-500", value_ "edit"] "Can Edit"
      option_ [class_ "text-gray-500", value_ "view"] "Can View"
    button_
      [ [__| on click remove the closest parent <div/> then halt |]
      , class_ "text-brand ml-4 font-semibold text-sm underline"
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


createSelectField :: Text -> Text -> Vector (Text, Text) -> Html ()
createSelectField val labelText options = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ " text-textStrong lowercase first-letter:uppercase"] $ toHtml labelText
      span_ [class_ " text-textWeak"] "*"
    select_ [class_ "select w-full h-12", name_ $ T.replace " " "" labelText, required_ "required"] do
      option_ [value_ ""] ""
      forM_ options $ \(key, value) -> option_ ([value_ key] ++ [selected_ val | val == key]) $ toHtml value


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
    a_ [href_ $ "/", class_ "absolute top-10 left-10"] do
      img_ [class_ "h-7", src_ "/public/assets/svgs/logo.svg"]
    div_ [class_ "flex-col gap-2 flex w-full"] $ do
      div_ [class_ " text-textStrong text-base font-semibold"] $ "Step " <> show step <> " of 6"
      div_ [class_ "grid grid-cols-6 w-full gap-1"] $ do
        forM_ [1 .. 6] $ \i -> div_ [class_ $ "h-2 w-full rounded-sm " <> if step >= i then "btn-primary rounded-sm" else " bg-fillWeak shadow-[inset_0px_1px_4px_0px_rgba(0,0,0,0.08)] border border-[#001066]/10"] pass
      when (step > 1) $ do
        a_ [class_ "flex items-center gap-3 flex text-brand w-full mt-2", href_ prevUrl] $ do
          faSprite_ "arrow-left" "regular" "h-4 w-4"
          span_ [class_ "font-semibold"] "Back"
    span_ [class_ " text-textStrong text-4xl font-semibold mt-4"] $ toHtml title


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
  traceShowM "proxyLandingH "
  traceShowM path
  let baseUrl = "https://apitoolkit.io/"
      fullUrl = baseUrl <> T.intercalate "/" path

  response <- W.get (toString fullUrl)

  let content = fromMaybe "" $ response L.^? responseBody
      textContent = TE.decodeUtf8 $ BL.toStrict content
  addRespHeaders textContent
