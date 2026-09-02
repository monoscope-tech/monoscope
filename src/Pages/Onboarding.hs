module Pages.Onboarding (
  onboardingGetH,
  onboardingInfoPostH,
  onboardingConfPostH,
  phoneEmailPostH,
  checkIntegrationGet,
  onboardingStepSkipped,
  dismissChecklistH,
  proxyLandingH,
  NotifChannelForm (..),
  OnboardingInfoForm (..),
  OnboardingConfForm (..),
  OnboardingGet (..),
  OnboardingStepData (..),
  OnboardingInfoPost (..),
  OnboardingConfPost (..),
  OnboardingPhoneEmailsPost (..),
) where

import Control.Lens qualified as L
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (key, _Array, _String)
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W (get, responseBody)
import Data.Text qualified as T
import Data.Tuple.Extra (thd3)
import Data.Vector qualified as V (Vector, fromList, toList)
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static (ask)
import Effectful.State.Static.Local qualified as State
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Integrations (getDiscordDataByProjectId, getProjectSlackData)
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Components
import Pkg.DeriveUtils (assetUrl)
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, HXRedirectDest, RespHeaders, TriggerEvents, XWidgetJSON, addErrorToast, addRespHeaders, redirectCS)
import Utils (LoadingSize (..), LoadingType (..), faSprite_, insertIfNotExist, loadingIndicator_, lookupValueText, onpointerdown_)
import Web.FormUrlEncoded


-- 'Info', 'Survey', 'CreateMonitor','NotifChannel','Integration', 'Pricing', 'Complete'
onboardingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders OnboardingGet)
onboardingGetH pid onboardingStepM = do
  (sess, project, bw) <- mkPageCtx pid
  appCtx <- ask @AuthContext
  let questions = fromMaybe (AE.Object []) project.questions
      onboardingStep = fromMaybe "Info" onboardingStepM
  stepData <- case onboardingStep of
    "Complete" -> pure $ CompleteStep pid
    "Survey" -> pure $ SurveyStep pid (fromMaybe "" $ lookupValueText questions "location") (questions L.^.. key "functionality" . _Array . traverse . _String)
    "NotifChannel" -> do
      hasSlack <- isJust <$> getProjectSlackData pid
      hasDiscord <- isJust <$> getDiscordDataByProjectId pid
      everyoneTeamM <- ProjectMembers.getEveryoneTeam pid
      let phone = fromMaybe "" $ everyoneTeamM >>= viaNonEmpty head . V.toList . (.phone_numbers)
          slackUrl = "https://slack.com/oauth/v2/authorize?client_id=" <> appCtx.config.slackClientId <> "&scope=chat:write,commands,incoming-webhook,files:write,app_mentions:read,channels:read,groups:read,channels:history,groups:history,im:history,mpim:history,chat:write.public&user_scope=&redirect_uri=" <> appCtx.env.slackRedirectUri <> "&state=" <> pid.toText <> "__onboarding"
          discordUrl = "https://discord.com/oauth2/authorize?response_type=code&client_id=" <> appCtx.config.discordClientId <> "&permissions=277025392640&integration_type=0&scope=bot+applications.commands&state=" <> pid.toText <> "__onboarding&redirect_uri=" <> appCtx.env.discordRedirectUri
      pure $ NotifChannelStep pid slackUrl discordUrl phone (maybe mempty (.notify_emails) everyoneTeamM) hasSlack hasDiscord
    "Integration" -> IntegrationStep pid . maybe "<API_KEY>" (.keyPrefix) . listToMaybe <$> ProjectApiKeys.projectApiKeysByProjectId pid
    "Pricing" ->
      let checkout u = u <> "&checkout[custom][project_id]=" <> pid.toText
       in pure $ PricingStep pid (checkout appCtx.config.lemonSqueezyUrl) (checkout appCtx.config.lemonSqueezyCriticalUrl) project.paymentPlan appCtx.config.enableFreetier appCtx.config.basicAuthEnabled
    _ ->
      let q = fromMaybe "" . lookupValueText questions
       in pure $ InfoStep pid sess.user.firstName sess.user.lastName (q "companyName") (q "companySize") (q "foundUsFrom")
  addRespHeaders $ OnboardingGet $ PageCtx bw{currProject = Nothing} stepData


data OnboardingInfoForm = OnboardingInfoForm
  { firstName :: Text
  , lastName :: Text
  , companyName :: Text
  , companySize :: Text
  , whereDidYouHearAboutUs :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm)


data OnboardingConfForm = OnboardingConfForm
  { location :: Text
  , functionality :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data NotifChannelForm = NotifChannelForm
  { phoneNumber :: Text
  , emails :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, FromForm)


data OnboardingStepData
  = InfoStep
      { stepPid :: Projects.ProjectId
      , firstName :: Text
      , lastName :: Text
      , companyName :: Text
      , companySize :: Text
      , foundUsFrom :: Text
      }
  | SurveyStep
      { stepPid :: Projects.ProjectId
      , location :: Text
      , functionality :: [Text]
      }
  | NotifChannelStep
      { stepPid :: Projects.ProjectId
      , slackUrl :: Text
      , discordUrl :: Text
      , phoneNumber :: Text
      , emails :: V.Vector Text
      , hasSlack :: Bool
      , hasDiscord :: Bool
      }
  | IntegrationStep
      { stepPid :: Projects.ProjectId
      , apiKey :: Text
      }
  | PricingStep
      { stepPid :: Projects.ProjectId
      , lemonUrl :: Text
      , criticalUrl :: Text
      , paymentPlan :: Text
      , enableFreetier :: Bool
      , basicAuthEnabled :: Bool
      }
  | CompleteStep
      { stepPid :: Projects.ProjectId
      }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


newtype OnboardingGet = OnboardingGet (PageCtx OnboardingStepData)
  deriving stock (Generic, Show)


instance ToHtml OnboardingGet where
  toHtml (OnboardingGet (PageCtx bwconf stepData)) = toHtml $ PageCtx bwconf $ renderStep stepData
    where
      renderStep = \case
        CompleteStep{..} -> onboardingCompleteBody stepPid
        SurveyStep{..} -> onboardingConfigBody stepPid location functionality
        NotifChannelStep{..} -> notifChannelsWithUrls slackUrl discordUrl stepPid phoneNumber emails hasDiscord hasSlack
        IntegrationStep{..} -> integrationsPage stepPid apiKey
        PricingStep{..} -> pricingPage stepPid lemonUrl criticalUrl paymentPlan enableFreetier basicAuthEnabled Projects.NoBillingProvider
        InfoStep{..} -> onboardingInfoBody stepPid firstName lastName companyName companySize foundUsFrom
  toHtmlRaw = toHtml


newtype OnboardingInfoPost = OnboardingInfoPost ()
  deriving stock (Generic, Show)


instance ToHtml OnboardingInfoPost where
  toHtml _ = ""
  toHtmlRaw = toHtml


newtype OnboardingConfPost = OnboardingConfPost ()
  deriving stock (Generic, Show)


instance ToHtml OnboardingConfPost where
  toHtml _ = ""
  toHtmlRaw = toHtml


data OnboardingPhoneEmailsPost = OnboardingPhoneEmailsPost
  { projectId :: Projects.ProjectId
  , emails :: V.Vector Text
  , enableFreetier :: Bool
  }
  deriving stock (Generic, Show)


instance ToHtml OnboardingPhoneEmailsPost where
  toHtml (OnboardingPhoneEmailsPost pid emails enableFreetier) = toHtmlRaw $ inviteTeamMemberModal pid emails enableFreetier
  toHtmlRaw = toHtml


onboardingStepSkipped :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
onboardingStepSkipped pid stepM = do
  (_, project) <- Projects.sessionAndProject pid
  whenJust stepM $ markStepCompleted pid project.onboardingStepsCompleted
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=" <> bool "Info" "Pricing" (stepM == Just "Integration")
  addRespHeaders ""


dismissChecklistH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
dismissChecklistH pid = do
  (_, project) <- Projects.sessionAndProject pid
  markStepCompleted pid project.onboardingStepsCompleted "checklist_dismissed"
  addRespHeaders ""


markStepCompleted :: (Hasql.Hasql :> es, IOE :> es) => Projects.ProjectId -> V.Vector Text -> Text -> Eff es ()
markStepCompleted pid stepsCompleted step =
  Hasql.interpExecute_ [HI.sql| update projects.projects set onboarding_steps_completed=#{insertIfNotExist step stepsCompleted} where id=#{pid} |]


phoneEmailPostH :: Projects.ProjectId -> NotifChannelForm -> ATAuthCtx (RespHeaders OnboardingPhoneEmailsPost)
phoneEmailPostH pid form = do
  (_, project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  memberEmails <- map (CI.original . (.email)) <$> Projects.usersByProjectId pid
  markStepCompleted pid project.onboardingStepsCompleted "NotifChannel"
  ProjectMembers.setEveryoneTeamEmails pid (V.fromList form.emails)
  unless (T.null form.phoneNumber) $ ProjectMembers.setEveryoneTeamPhones pid (V.fromList [form.phoneNumber])
  addRespHeaders $ OnboardingPhoneEmailsPost pid (V.fromList $ ordNub $ form.emails <> memberEmails) appCtx.config.enableFreetier


checkIntegrationGet :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
checkIntegrationGet pid languageM = do
  (_, project) <- Projects.sessionAndProject pid
  v :: Maybe Text <- Hasql.interpOne [HI.sql|SELECT context___span_id FROM otel_logs_and_spans WHERE project_id = #{pid.toText} LIMIT 1|]
  case v of
    Nothing -> addErrorToast "No events found yet" Nothing >> addRespHeaders ""
    Just _ -> do
      markStepCompleted pid project.onboardingStepsCompleted "Integration"
      if isJust languageM
        then addRespHeaders $ div_ [class_ "flex items-center gap-2 text-textSuccess"] do
          span_ "verified"
          faSprite_ "circle-check" "regular" "h-4 w-4"
        else redirectCS ("/p/" <> pid.toText <> "/onboarding?step=Pricing") >> addRespHeaders ""


onboardingInfoPostH :: Projects.ProjectId -> OnboardingInfoForm -> ATAuthCtx (RespHeaders OnboardingInfoPost)
onboardingInfoPostH pid form = do
  (sess, project) <- Projects.sessionAndProject pid
  let jsonBytes =
        mergeQuestions
          project.questions
          [ ("companyName", AE.toJSON form.companyName)
          , ("companySize", AE.toJSON form.companySize)
          , ("foundUsFrom", AE.toJSON form.whereDidYouHearAboutUs)
          ]
      userId = sess.user.id
  Hasql.interpExecute_ [HI.sql| update projects.projects set title=#{form.companyName},questions=#{jsonBytes},onboarding_steps_completed=#{insertIfNotExist "Info" project.onboardingStepsCompleted} where id=#{pid} |]
  Hasql.interpExecute_ [HI.sql| update users.users set first_name=#{form.firstName}, last_name=#{form.lastName} where id=#{userId} |]
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=Survey"
  addRespHeaders $ OnboardingInfoPost ()


-- | Merge new answers into a project's existing questions blob; new answers win on conflict.
mergeQuestions :: Maybe AE.Value -> [(AE.Key, AE.Value)] -> HI.AsJsonb AE.Value
mergeQuestions old kvs = HI.AsJsonb $ AE.Object $ KM.fromList kvs <> foldMap (\case AE.Object o -> o; _ -> mempty) old


onboardingConfPostH :: Projects.ProjectId -> OnboardingConfForm -> ATAuthCtx (RespHeaders OnboardingConfPost)
onboardingConfPostH pid form = do
  (_, project) <- Projects.sessionAndProject pid
  let jsonBytes = mergeQuestions project.questions [("functionality", AE.toJSON form.functionality), ("location", AE.toJSON form.location)]
  Hasql.interpExecute_ [HI.sql| update projects.projects set questions=#{jsonBytes}, onboarding_steps_completed=#{insertIfNotExist "Survey" project.onboardingStepsCompleted} where id=#{pid} |]
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=NotifChannel"
  addRespHeaders $ OnboardingConfPost ()


onboardingCompleteBody :: Projects.ProjectId -> Html ()
onboardingCompleteBody pid = do
  div_ [class_ "w-full max-w-xl h-full flex items-center mx-auto relative px-4 md:px-0"] $ do
    canvas_ [id_ "drawing_canvas", class_ "absolute top-0 left-0  w-full"] pass
    div_ [class_ "flex-col gap-4 flex w-full p-14 my-auto border border-weak rounded-2xl"] $ do
      iconBadgeWith_ "p-3" "h-8 w-8" "rounded-full" SuccessBadge "circle-check"
      div_ [class_ "flex flex-col gap-2"] do
        h3_ [class_ " text-textStrong  text-2xl"] "Onboarding completed!"
        p_ [class_ " text-textWeak text-sm"] "You're all set! You can now start exploring the monoscope dashboard by clicking the button below."
      a_ [class_ "btn-primary py-2  rounded-lg text-center mt-1", href_ $ "/p/" <> pid.toText <> "/"] "Go to your dashboard"
  script_ [src_ "/public/assets/js/confetti.js"] ("" :: Text)


pricingPage :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Projects.BillingProvider -> Html ()
pricingPage pid lemon critical paymentPlan freeTierEnabled basicAuthEnabled provider = do
  div_ [class_ "w-full max-w-[1100px] mx-auto mt-[70px] mb-10 px-4 md:px-0"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      div_ [class_ "w-full md:w-1/2"] $ do
        stepIndicator 5 "Please pick a plan" $ "/p/" <> pid.toText <> "/onboarding?step=Integration"
      paymentPlanPicker pid lemon critical paymentPlan freeTierEnabled basicAuthEnabled True provider
      div_ [class_ "flex flex-col gap-2 w-full pb-20"] do
        span_ [class_ "text-textStrong text-2xl mt-20"] "FAQ"
        div_ [class_ "join join-vertical w-full mt-4"] do
          faQ "What is an event?" "An event is any of span, log, or metric that you send to Monoscope."
          faQ "How do you handle security and sensitive data?" "We employ encryption and authentication measures to ensure the security of your data during transmission and storage. All our SDKs also support redacting data. You can simply specify the JSONPath to the fields that you don't want the SDKs to forward to Monoscope, and those sensitive fields will be stripped out/redacted before the data even leaves your servers and replaced with the text \"CLIENT REDACTED\" on our end. We will never see anything you don't want us to see."
          faQ "What makes us better than others?" "Aside from the standard observability features like traces, logs, and metrics, Monoscope takes it a step further by monitoring request payloads for both incoming and outgoing requests, automatic error reporting, and payload-change detection — giving engineering teams all the information they need to seamlessly debug and fix issues in their servers."


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


-- IMPORTANT: DO NOT DELETE. Needed for the tailwindcss to generate classes.
-- [class_ "group-has-[#check-js:checked]/pg:block group-has-[#check-go:checked]/pg:block group-has-[#check-elixir:checked]/pg:block group-has-[#check-py:checked]/pg:block group-has-[#check-java:checked]/pg:block"]
-- [class_ "group-has-[#check-php:checked]/pg:block group-has-[#check-cs:checked]/pg:block"]
-- [class_ "group-has-[#check-linux:checked]/pg:block group-has-[#check-docker:checked]/pg:block group-has-[#check-kubernetes:checked]/pg:block group-has-[#check-kafka:checked]/pg:block"]
-- [class_ "group-has-[#check-postgresql:checked]/pg:block group-has-[#check-mongodb:checked]/pg:block group-has-[#check-mysql:checked]/pg:block"]
-- [class_ "text-left pb-12 prose prose-slate dark:prose-invert prose-headings:font-medium prose-a:text-secondary prose-a:underline prose-a:underline-offset-4 prose-img:w-full prose-img:rounded-md prose-img:drop-shadow-md prose-img:border prose-img:border-base-200 prose-p:leading-relaxed prose-headings:scroll-mt-40 prose-pre:p-0 before:prose-li:bg-secondary before:prose-li:text-secondary prose-strong:font-medium prose-secondary w-full"]

integrationsPage :: Projects.ProjectId -> Text -> Html ()
integrationsPage pid apikey =
  div_ [class_ "w-full flex flex-col md:flex-row min-h-screen md:h-screen md:overflow-hidden group/pg"] do
    div_ [class_ "w-full md:w-1/2 bg-bgRaised md:h-full flex flex-col"] do
      div_ [class_ "pt-14 md:pt-[156px] px-4 md:px-12 flex-shrink-0"]
        $ div_ [class_ "max-w-xl"]
        $ stepIndicator 4 "Instrument your apps or servers"
        $ "/p/"
        <> pid.toText
        <> "/onboarding?step=NotifChannel"
      div_ [class_ "flex-col w-full gap-3 md:gap-4 flex mt-3 md:mt-4 px-4 md:px-12 overflow-y-auto flex-grow pb-20 md:pb-4"] do
        p_ [class_ "text-textWeak leading-relaxed text-sm md:text-base"] do
          "Select your stack below for integration instructions."

        div_ [class_ "mt-2 md:mt-6 mb-0 p-3 md:p-4 bg-fillWeak border border-strokeWeak rounded-xl"] do
          div_ [class_ "mb-1.5 md:mb-2 text-textWeak text-sm"] "Your API Key"
          div_ [class_ "flex items-center gap-2"] do
            div_ [class_ "flex-1 monospace bg-bgBase p-3 border border-strokeWeak rounded-lg overflow-x-auto", id_ "api-key-display"] $ toHtml apikey
            button_
              [ class_ "px-4 py-2 bg-fillBrand-strong rounded-xl text-textInverse-strong flex items-center gap-1 hover:bg-fillBrand-strong/90 cursor-pointer"
              , type_ "button"
              , [__|on pointerdown call navigator.clipboard.writeText(#api-key-display's innerText) then put 'Copied!' into me|]
              ]
              do
                span_ "Copy"
                faSprite_ "copy" "regular" "h-4 w-4"

        div_ [class_ "mb-4 px-4 bg-gradient-to-r from-fillInformation-weak to-transparent border border-strokeInformation-weak rounded-lg"] do
          p_ [class_ "text-sm text-textStrong"] do
            "Want to test quickly? "
            label_
              [ class_ "text-textBrand hover:text-textStrong underline font-medium cursor-pointer"
              , Lucid.for_ "telemetrygen-modal"
              ]
              "Use telemetrygen"
            " to send sample data in seconds"

        forM_ integrationGroups \(groupName, langsList) -> div_ [class_ "mb-4 md:mb-6"] do
          div_ [class_ "text-textWeak text-lg md:text-xl mb-2"] $ toHtml groupName
          div_ [class_ "grid grid-cols-2 gap-2"]
            $ forM_ langsList \(lang, langName, _) ->
              languageItem pid langName lang

        integrationCta_ pid "max-md:hidden flex items-center gap-4 py-8" "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center" "px-4 py-3 flex items-center underline text-textBrand text-xl cursor-pointer"
      integrationCta_ pid "md:hidden fixed bottom-0 left-0 right-0 bg-bgRaised border-t border-strokeWeak pl-4 pr-18 py-3 flex items-center gap-3 z-30" "btn-primary px-6 py-2.5 text-base rounded-xl cursor-pointer flex-1 flex items-center justify-center" "px-3 py-2.5 flex items-center underline text-textBrand text-base cursor-pointer"

    div_ [class_ "w-full md:w-1/2 md:h-full overflow-hidden md:border-l border-weak", id_ "docs-panel"] do
      div_ [class_ "md:hidden sticky top-0 z-20 bg-bgBase border-b border-strokeWeak p-3 hidden", id_ "docs-panel-back"] do
        button_
          [ class_ "flex items-center gap-2 text-textBrand cursor-pointer"
          , type_ "button"
          , [__|on click remove .open from #docs-panel|]
          ]
          do
            faSprite_ "arrow-left" "regular" "h-4 w-4"
            span_ "Back to selection"
      div_ [class_ "md:h-full flex flex-col"] do
        div_ [class_ "w-full md:h-full overflow-y-auto rounded-2xl blue-gradient-box bg-bgBase"] do
          div_ [class_ "max-md:hidden p-12 text-center group-has-[.checkbox:checked]/pg:hidden flex items-center justify-center h-full"] do
            div_ [class_ "flex flex-col w-full items-center gap-8"] do
              div_ [class_ "p-6 bg-fillWeak rounded-full"] do
                faSprite_ "brackets-curly" "regular" "h-16 w-16 text-iconBrand"

              h2_ [class_ "text-3xl text-textStrong"] "👈 Select your stack on the left to begin"
              p_ [class_ "text-lg text-textWeak max-w-md leading-relaxed"] do
                "You can also check out our youtube videos for more interactive walkthroughs."

                div_ [class_ "grid grid-cols-2 gap-4 w-full"]
                  $ forM_ ["Q-tGuIkDmyk?si=BIHn2vN1m9gDs_9v", "OALS4ckfOdI"] \vid ->
                    div_ [class_ "relative overflow-hidden rounded-lg border border-weak", style_ "padding-bottom: 56.25%;"]
                      $ iframe_
                        [ class_ "absolute top-0 left-0 w-full h-full"
                        , src_ $ "https://www.youtube.com/embed/" <> vid
                        , title_ "YouTube video player"
                        , term "frameborder" "0"
                        , term "allow" "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
                        , term "referrerpolicy" "strict-origin-when-cross-origin"
                        , term "allowfullscreen" ""
                        ]
                        ""

                div_ [class_ "text-center mt-3"]
                  $ a_ [href_ "https://www.youtube.com/@monoscope", target_ "_blank", class_ "text-textBrand hover:underline text-sm font-medium"] do
                    "Watch more tutorials →"

          forM_ integrationGroups \(_, integrations) -> do
            forM_ integrations \(lang, _, frameworks) ->
              div_ [class_ $ "p-4 lang-guide hidden group-has-[#check-" <> lang <> ":checked]/pg:block", id_ $ lang <> "_main"] do
                div_ [class_ "px-2 md:px-8 sticky top-0 z-10 bg-bgBase py-2"]
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
                      unless (T.null fwIcon) $ img_ [class_ "h-5 w-5", src_ $ "https://monoscope.tech/assets/img/framework-logos/" <> fwIcon]
                      span_ $ toHtml fwName

                div_ [class_ "relative p-2 md:p-8"] do
                  div_ [id_ $ "fw-indicator-" <> lang, class_ "htmx-indicator flex justify-center py-5"]
                    $ loadingIndicator_ LdMD LdDots
                  div_
                    [ id_ $ "fw-content-" <> lang
                    , hxGet_ $ "/proxy/docs/sdks/" <> foldMap thd3 (listToMaybe frameworks)
                    , hxTrigger_ "load"
                    , hxSwap_ "innerHTML"
                    , hxSelect_ "#mainArticle"
                    , class_ "prose-a:!text-textBrand prose-a:!underline"
                    ]
                    ""

    modalWith_ "telemetrygen-modal" def{boxClass = "max-w-2xl", hideClose = True} Nothing do
      h3_ [class_ "text-lg font-bold text-textStrong flex items-center gap-2 mb-4"] do
        faSprite_ "flask-vial" "regular" "h-5 w-5"
        span_ "Quick Test with the CLI"

      p_ [class_ "text-textWeak mb-6 leading-relaxed"] "Use the Monoscope CLI to send test traces and verify your setup is working. No extra tools needed."

      div_ [class_ "space-y-4"] do
        div_ [class_ "p-4 bg-fillWeak rounded-lg"] do
          div_ [class_ "text-textStrong font-medium mb-2 flex items-center gap-2"] do
            span_ [class_ "inline-flex items-center justify-center w-6 h-6 rounded-full bg-fillBrand-weak text-textBrand text-sm font-bold"] "1"
            span_ "Install & authenticate (if you haven't)"
          div_
            [class_ "bg-bgBase p-3 rounded monospace text-sm overflow-x-auto border border-strokeWeak"]
            "curl https://monoscope.tech/install.sh | sh && monoscope auth login"

        div_ [class_ "p-4 bg-fillWeak rounded-lg"] do
          div_ [class_ "text-textStrong font-medium mb-2 flex items-center gap-2"] do
            span_ [class_ "inline-flex items-center justify-center w-6 h-6 rounded-full bg-fillBrand-weak text-textBrand text-sm font-bold"] "2"
            span_ "Send test traces"
          div_ [class_ "relative"] do
            pre_ [class_ "bg-bgBase p-3 rounded monospace text-sm overflow-x-auto border border-strokeWeak", id_ "telemetrygen-cmd"]
              $ code_
              $ toHtml
                "monoscope telemetrygen --kind=trace --count=10"
            button_
              [ class_ "absolute top-2 right-2 px-3 py-1 text-xs bg-fillBrand-strong rounded text-textInverse-strong flex items-center gap-1 hover:bg-fillBrand-strong/90"
              , type_ "button"
              , [__|on click call navigator.clipboard.writeText(#telemetrygen-cmd's innerText) then put 'Copied!' into me|]
              ]
              do
                span_ "Copy"
                faSprite_ "copy" "regular" "h-3 w-3"

      div_ [class_ "mt-6 p-4 bg-fillSuccess-weak border border-strokeSuccess-weak rounded-lg flex items-start gap-3"] do
        faSprite_ "circle-check" "regular" "h-5 w-5 text-iconSuccess flex-shrink-0 mt-0.5"
        div_ do
          p_ [class_ "text-textSuccess text-sm font-medium leading-relaxed"] "What happens next?"
          p_ [class_ "text-textSuccess text-sm mt-1 leading-relaxed"] "After running the command, traces will appear in your dashboard within seconds. You can then proceed with the full SDK integration."

      div_ [class_ "modal-action"] $ label_ [Lucid.for_ "telemetrygen-modal", class_ "btn"] "Close"

    -- Mobile docs-panel overlay + the AI menu injected into proxied docs content
    style_
      $ unlines
        [ "@media(max-width:767px){#docs-panel{display:none}#docs-panel.open{display:flex;flex-direction:column;position:fixed;inset:0;z-index:50;background:var(--color-bgBase);overflow-y:auto}#docs-panel.open #docs-panel-back{display:block}}"
        , ".ai-menu-wrap{position:relative}"
        , ".ai-menu{position:absolute;right:0;top:100%;z-index:50;min-width:260px;margin-top:6px;background:var(--color-bgRaised);border:1px solid var(--color-strokeWeak);border-radius:12px;box-shadow:0 8px 24px rgba(0,0,0,0.12);padding:6px;opacity:0;transform:scale(0.95) translateY(-4px);pointer-events:none;transition:opacity 150ms ease,transform 150ms ease}"
        , ".ai-menu.open{opacity:1;transform:scale(1) translateY(0);pointer-events:auto}"
        , ".ai-menu-item{display:flex;align-items:center;gap:10px;padding:8px 12px;border-radius:8px;cursor:pointer;font-size:0.8125rem;color:var(--color-text);text-decoration:none;border:none;background:none;width:100%;text-align:left;transition:background 100ms ease}"
        , ".ai-menu-item:hover{background:var(--color-fillWeak)}"
        , ".ai-menu-item.disabled{opacity:0.4;pointer-events:none}"
        , ".ai-menu-item svg{width:16px;height:16px;flex-shrink:0}"
        , ".ai-menu-item .ai-menu-label{flex:1}"
        , ".ai-menu-item .ai-menu-sub{font-size:0.6875rem;color:var(--color-textWeak)}"
        , ".ai-menu-sep{height:1px;background:var(--color-strokeWeak);margin:4px 8px}"
        , ".ai-menu-item .ai-menu-check{display:none;color:oklch(0.65 0.2 145)}"
        , ".ai-menu-item.copied .ai-menu-check{display:block}"
        , ".ai-menu-item.copied svg:first-child{display:none}"
        ]

    link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-dark.min.css")]
    forM_ ["highlight", "javascript", "python", "go", "java", "csharp", "php", "elixir", "bash", "shell", "yaml", "json"] \f ->
      script_ [src_ (assetUrl $ "/public/assets/deps/highlightjs/" <> f <> ".min.js")] ("" :: Text)

    script_
      [text|
        hljs.highlightAll();

        function initAIMenu(container) {
          var trigger = container.querySelector('#ai-menu-trigger');
          var menu = container.querySelector('#ai-menu');
          if (!trigger || !menu) return;
          var mcpItems = menu.querySelectorAll('[data-action="copy-mcp"],[data-action="connect-cursor"],[data-action="connect-vscode"]');
          mcpItems.forEach(function(el) { el.classList.add('disabled'); });
          var pageUrl = window.location.href;
          var title = container.querySelector('h1');
          var pageTitle = title ? title.textContent.replace(/#/g, '').trim() : document.title;
          var prompt = 'Read this Monoscope docs page and help me understand it: ' + pageUrl;
          var chatgpt = menu.querySelector('[data-action="open-chatgpt"]');
          if (chatgpt) chatgpt.href = 'https://chatgpt.com/?hints=search&q=' + encodeURIComponent(prompt);
          var claude = menu.querySelector('[data-action="open-claude"]');
          if (claude) claude.href = 'https://claude.ai/new?q=' + encodeURIComponent(prompt);
          var perplexity = menu.querySelector('[data-action="open-perplexity"]');
          if (perplexity) perplexity.href = 'https://www.perplexity.ai/search?q=' + encodeURIComponent(prompt);
          trigger.onclick = function(e) { e.stopPropagation(); menu.classList.toggle('open'); };
          document.addEventListener('click', function(e) {
            if (!menu.contains(e.target) && e.target !== trigger) menu.classList.remove('open');
          });
          document.addEventListener('keydown', function(e) { if (e.key === 'Escape') menu.classList.remove('open'); });
          function pageMarkdown() {
            var clone = container.cloneNode(true);
            clone.querySelectorAll('.ai-menu-wrap, .code-header').forEach(function(el) { el.remove(); });
            return '# ' + pageTitle + '\n\nSource: ' + pageUrl + '\n\n' + clone.innerText;
          }
          var copyPage = menu.querySelector('[data-action="copy-page"]');
          if (copyPage) copyPage.onclick = function() {
            navigator.clipboard.writeText(pageMarkdown()).then(function() {
              copyPage.classList.add('copied');
              setTimeout(function() { copyPage.classList.remove('copied'); }, 2000);
            });
          };
          var viewMd = menu.querySelector('[data-action="view-markdown"]');
          if (viewMd) viewMd.onclick = function() {
            window.open(URL.createObjectURL(new Blob([pageMarkdown()], { type: 'text/plain' })), '_blank');
          };
        }

        // Re-highlight after HTMX swaps content
        document.body.addEventListener('htmx:after:swap', function(event) {
          // Only highlight content in the integration documentation area
          const target = event.detail.target;
          if (target && (target.id.startsWith('fw-content-') || target.classList.contains('lang-guide'))) {
            target.querySelectorAll('pre code').forEach((block) => {
              hljs.highlightElement(block);
            });
            initAIMenu(target);
          }
        });
      |]


-- | "Confirm & Proceed" + "Skip" pair; classes differ between the desktop inline and mobile sticky bars.
integrationCta_ :: Projects.ProjectId -> Text -> Text -> Text -> Html ()
integrationCta_ pid wrapCls btnCls skipCls = div_ [class_ wrapCls] do
  button_ [class_ btnCls, hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check", hxSwap_ "none", hxIndicator_ "#loadingIndicator"] "Confirm & Proceed"
  a_ [class_ skipCls, type_ "button", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/skip?step=Integration"] "Skip"


languageItem :: Projects.ProjectId -> Text -> Text -> Html ()
languageItem pid langName lang = do
  label_
    [ class_ "group/li cols-span-1 h-12 px-3 py-2 bg-transparent rounded-xl border border-strokeWeak justify-start items-center gap-3 inline-flex cursor-pointer"
    ]
    do
      input_
        [ type_ "checkbox"
        , class_ "checkbox shrink-0"
        , id_ $ "check-" <> lang
        , value_ lang
        , onchange_ $ "if(this.checked){var dp=document.getElementById('docs-panel');if(window.innerWidth<768){dp.classList.add('open');requestAnimationFrame(function(){dp.scrollTop=0});}else{requestAnimationFrame(function(){document.getElementById('" <> lang <> "_main').scrollIntoView({behavior:'smooth'})});}}"
        ]
      div_ [class_ "flex w-full items-center justify-between overflow-hidden"] do
        div_ [class_ "flex items-center gap-2 text-sm min-w-0"] do
          img_ [class_ "h-5 w-5", src_ $ "/public/assets/svgs/" <> lang <> ".svg"]
          span_ $ toHtml langName
        div_ [class_ "hidden group-has-[.checkbox:checked]/li:block text-sm toggle-target", id_ $ "integration-check-container" <> T.replace "#" "" langName] do
          div_
            [ class_ "flex items-center gap-1 shrink-0"
            , hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check?language=" <> T.replace "#" "sharp" langName
            , hxSwap_ "innerHTML"
            , hxTarget_ $ "#integration-check-" <> lang
            , hxTrigger_ "load delay:5s"
            ]
            do
              span_ [class_ "text-textStrong hidden md:inline text-xs"] "waiting"
              faSprite_ "spinner" "regular" "h-4 w-4 animate-spin shrink-0"


integrationCard :: Text -> Text -> Bool -> Text -> Html ()
integrationCard serviceName iconPath isConnected connectUrl = do
  div_ [class_ "px-3 py-2 rounded-xl border border-strokeWeak bg-fillWeak justify-between items-center flex"] $ do
    div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
      img_ [src_ iconPath]
      span_ [class_ "text-center text-textStrong text-xl"] $ toHtml serviceName
    if isConnected
      then button_ [class_ "text-textSuccess "] "Connected"
      else a_ [target_ "_blank", class_ "border px-3 h-8 flex items-center shadow-xs border-[var(--brand-color)] rounded-lg text-textBrand ", href_ connectUrl] "Connect"


onboardingStepWrapper_ :: Int -> Text -> Text -> Html () -> Html ()
onboardingStepWrapper_ step title prevUrl content =
  div_ [class_ "w-full max-w-xl mx-auto mt-20 md:mt-[156px] px-4 md:px-0"]
    $ div_ [class_ "flex-col gap-4 flex w-full"] do
      stepIndicator step title prevUrl
      content


notifChannelsWithUrls :: Text -> Text -> Projects.ProjectId -> Text -> V.Vector Text -> Bool -> Bool -> Html ()
notifChannelsWithUrls slackUrl discordUrl pid phone emails hasDiscord hasSlack = do
  div_ [class_ "w-full max-w-xl mx-auto mt-20 md:mt-[156px] mb-10 px-4 md:px-0"] $ do
    div_ [id_ "inviteModalContainer"] pass
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 3 "How should we notify you about issues?" $ "/p/" <> pid.toText <> "/onboarding?step=Survey"
      div_ [class_ "flex-col w-full gap-8 flex mt-4"] $ do
        div_ [class_ "w-full flex flex-col gap-8"] $ do
          div_ [class_ "w-full gap-2 grid grid-cols-1 md:grid-cols-2"] $ do
            integrationCard "Slack" "/public/assets/svgs/slack.svg" hasSlack slackUrl
            integrationCard "Discord" "/public/assets/svgs/discord.svg" hasDiscord discordUrl
          form_
            [ class_ "flex flex-col gap-8"
            , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/phone-emails"
            , hxExt_ "json-enc"
            , hxVals_ "js:{phoneNumber: document.getElementById('phoneNumber').value , emails: window.getTagValues('#emails')}"
            , hxTarget_ "#inviteModalContainer"
            , hxSwap_ "innerHTML"
            , hxIndicator_ "#loadingIndicator"
            ]
            $ do
              formField_ FieldMd def{inputType = "tel", value = phone} "Notify phone number" "phoneNumber" False Nothing
              formField_ FieldMd def "Notify the following email address" "emails" False $ Just $ tagInput_ "emails" "" [data_ "tagify-initial" $ decodeUtf8 $ AE.encode emails]
              div_ [class_ "items-center gap-4 flex"] $ do
                button_ [class_ "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center"] "Proceed"
      script_
        """
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
        """


onboardingInfoBody :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Html ()
onboardingInfoBody pid firstName lastName cName cSize fUsFrm = do
  onboardingStepWrapper_ 1 "Tell us a little bit about you" ""
    $ form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/info", hxIndicator_ "#loadingIndicator"]
    $ do
      div_ [class_ "flex-col w-full gap-4 mt-4 flex"] $ do
        forM_ ([("First Name", "firstName", firstName), ("Last Name", "lastName", lastName), ("Company Name", "companyName", cName)] :: [(Text, Text, Text)]) \(label, name, val) ->
          formField_ FieldMd def{value = val} label name True Nothing
        let createSelectField selected label name opts = formSelectField_ FieldMd label name True $ options_ (Just selected) (("", "") : opts)
        createSelectField cSize "Company Size" "companySize" [("1 - 4", "1 to 4"), ("5 - 10", "5 to 10"), ("11 - 25", "11 to 25"), ("26+", "26 and above")]
        createSelectField fUsFrm "How Did You Hear About Us" "whereDidYouHearAboutUs" [("google", "Google"), ("twitter", "Twitter"), ("linkedin", "LinkedIn"), ("friend", "Friend"), ("other", "Other")]
      div_ [class_ "items-center gap-1 flex"] $ do
        button_ [class_ "btn-primary px-6 py-4 text-xl rounded-lg cursor-pointer flex items-center"] "Proceed"


onboardingConfigBody :: Projects.ProjectId -> Text -> [Text] -> Html ()
onboardingConfigBody pid loca func = do
  onboardingStepWrapper_ 2 "Let's configure your project" ("/p/" <> pid.toText <> "/onboarding?step=Info")
    $ form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/survey", hxIndicator_ "#loadingIndicator"]
    $ do
      div_ [class_ "flex-col w-full gap-14 mt-4 flex"] $ do
        div_ [class_ "flex-col gap-2 flex"] $ do
          div_ [class_ "items-center gap-[2px] flex"] $ do
            span_ [class_ " text-textStrong"] "Where should your project be hosted?"
            span_ [class_ " text-textWeak"] "*"
          div_ [class_ "pt-2 flex-col gap-4 flex text-sm  text-textStrong"] $ do
            forM_ locations $ createBinaryField "radio" "location" [loca]
        div_ [class_ "flex-col gap-2 flex"] $ do
          div_ [class_ "items-center flex gap-[2px]"] $ do
            span_ [class_ " text-textStrong"] "Which Monoscope features will you be using?"
            span_ [class_ " text-textWeak"] "*"
          div_ [class_ "pt-2 flex-col gap-4 flex"] $ do
            forM_ functionalities $ createBinaryField "checkbox" "functionality" func
      div_ [class_ "items-center gap-1 flex"] $ do
        button_ [class_ "btn-primary px-6 py-4 text-xl rounded-lg cursor-pointer flex items-center"] "Proceed"


inviteTeamMemberModal :: Projects.ProjectId -> V.Vector Text -> Bool -> Html ()
inviteTeamMemberModal pid emails enableFreetier = do
  div_ [id_ "invite-modal-container"] do
    modalWith_ "inviteModal" def{autoOpen = True, boxClass = "bg-bgRaised max-w-md", wrapperClass = "p-4 md:p-8"} Nothing do
      div_ [class_ "flex items-center gap-3"] do
        iconBadge_ SuccessBadge "circle-check"
        span_ [class_ "text-textStrong text-xl"] "Test notification sent"
      div_ [class_ "text-textWeak text-sm"] "No notification? Close this modal and verify your emails and channels."
      details_ [class_ "w-full border-t border-strokeWeak pt-3"] $ do
        summary_ [class_ "text-textBrand text-sm cursor-pointer"] "Invite team members"
        div_ [class_ "flex-col gap-3 flex pt-3"] $ do
          when enableFreetier $ div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 flex items-start gap-2"] do
            faSprite_ "circle-info" "regular" "w-4 h-4 text-textInformation flex-shrink-0 mt-0.5"
            p_ [class_ "text-sm text-textWeak"] "Free plan members will be invited but disabled until you upgrade."
          div_ [class_ "w-full gap-2 flex items-center"] $ do
            input_ [class_ "input input-sm w-full", placeholder_ "email@example.com", type_ "email", id_ "add-member-input"]
            button_ [class_ "btn-primary rounded-xl px-4 py-2 text-white text-sm cursor-pointer whitespace-nowrap", onpointerdown_ "appendMember()"] "Invite"
          form_
            [ class_ "flex-col flex"
            , id_ "members-container"
            , hxPost_ $ "/p/" <> pid.toText <> "/manage_members?onboarding=true"
            , hxIndicator_ "#loadingIndicator"
            ]
            $ do
              inviteMemberItem Nothing
              forM_ emails (inviteMemberItem . Just)
      div_ [class_ "modal-action w-full flex items-center justify-start gap-4"] do
        button_ [class_ "btn-primary px-8 py-2 text-lg rounded-xl cursor-pointer flex items-center", type_ "submit", term "form" "members-container"] "Proceed"
        label_ [Lucid.for_ "inviteModal", class_ "text-textWeak text-sm underline cursor-pointer"] "Close"


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


-- | 'Nothing' renders the hidden @#member-template@ that appendMember() clones.
inviteMemberItem :: Maybe Text -> Html ()
inviteMemberItem emailM = do
  let (email, isTemplate) = (fromMaybe "" emailM, isNothing emailM)
  div_ (class_ ("flex  py-1 w-full justify-between items-center border-b border-strokeWeak " <> bool "" "hidden" isTemplate) : [id_ "member-template" | isTemplate]) do
    div_ [class_ "pr-6 py-1  w-full justify-start items-center inline-flex"] do
      input_ ([type_ "hidden", value_ email] <> [name_ "emails" | not isTemplate])
      span_ [class_ "text-textStrong text-sm font-normal"] $ toHtml email
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


createBinaryField :: Text -> Text -> [Text] -> (Text, Text) -> Html ()
createBinaryField kind name selectedValues (value, label) = do
  div_ [class_ " items-center gap-3 inline-flex"] $ do
    input_ $ [class_ "w-6 h-6 rounded-sm", type_ kind, name_ name, value_ value, id_ value] <> [required_ "required" | kind == "radio"] <> [checked_ | value `elem` selectedValues]
    label_ [class_ " text-textStrong text-sm", Lucid.for_ value] $ toHtml label


stepIndicator :: Int -> Text -> Text -> Html ()
stepIndicator step title prevUrl = do
  div_ [class_ "fixed  htmx-indicator top-0 left-0 right-0 bottom-0 flex items-center justify-center z-9999", id_ "loadingIndicator"] $ loadingIndicator_ LdLG LdDots
  div_ [class_ "flex-col gap-2 md:gap-4 flex w-full"] $ do
    a_ [href_ "/", class_ "absolute top-4 left-4 md:top-10 md:left-10 py-2 pr-2 rounded-xs"] do
      img_ [class_ "h-7 dark:hidden", src_ "/public/assets/svgs/logo_black.svg"]
      img_ [class_ "h-7 hidden dark:block", src_ "/public/assets/svgs/logo_white.svg"]
    div_ [class_ "flex-col gap-1.5 md:gap-2 flex w-full"] $ do
      headerRow_ [] do
        div_ [class_ "text-textStrong text-sm md:text-base"] $ "Step " <> show step <> " of 5"
        when (step > 1)
          $ a_ [class_ "flex items-center gap-1.5 text-textBrand text-sm md:hidden", href_ prevUrl] do
            faSprite_ "arrow-left" "regular" "h-3.5 w-3.5"
            span_ [] "Back"
      div_ [class_ "grid grid-cols-5 w-full gap-1"] $ do
        forM_ [1 .. 5] $ \i -> div_ [class_ $ "h-1.5 md:h-2 w-full rounded-sm " <> if step >= i then "btn-primary rounded-sm" else " bg-fillWeak shadow-sm border border-strokeWeak"] pass
      when (step > 1)
        $ a_ [class_ "max-md:hidden flex items-center gap-3 text-textBrand w-full mt-2", href_ prevUrl] do
          faSprite_ "arrow-left" "regular" "h-4 w-4"
          span_ [] "Back"
    span_ [class_ "text-textStrong text-xl md:text-4xl mt-1 md:mt-4"] $ toHtml title


faQ :: Text -> Text -> Html ()
faQ question answer =
  details_ [class_ "collapse collapse-arrow join-item border border-strokeWeak"] do
    summary_ [class_ "collapse-title text-textStrong font-medium"] $ toHtml question
    div_ [class_ "collapse-content text-textWeak leading-relaxed"] $ p_ $ toHtml answer


-- | Proxy handler for fetching documentation from monoscope.tech
-- This bypasses CORS restrictions by fetching the content server-side
proxyLandingH :: (HTTP :> es, State.State HXRedirectDest :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es) => [Text] -> Eff es (RespHeaders Text)
proxyLandingH path = do
  response <- W.get $ toString $ "https://monoscope.tech/" <> T.intercalate "/" path
  addRespHeaders $ T.replace "href=\"/" "href=\"https://monoscope.tech/" $ decodeUtf8 $ fromMaybe "" $ response L.^? W.responseBody
