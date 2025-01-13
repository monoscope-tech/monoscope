module Pages.Onboarding.Onboarding (onboardingGetH, onboardingInfoPost, onboardingConfPost, OnboardingInfoForm (..), OnboardingConfForm (..)) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Text qualified as T
import Data.Vector as V (Vector)
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Gogol.PubSub (CreateSnapshotRequest (labels))
import Lucid
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Models.Projects.Projects (OnboardingStep (..))
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components qualified as Components
import Pkg.Components qualified as Components
import Relude
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, redirectCS)
import Utils (faSprite_, redirect)
import Web.FormUrlEncoded


-- 'Info', 'Survey', 'CreateMonitor','NotifChannel','Integration', 'Pricing', 'Complete'
onboardingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
onboardingGetH pid onboardingStep = do
  (sess, project) <- Sessions.sessionAndProject pid
  let firstName = sess.user.firstName
      lastName = sess.user.lastName
      -- onboardingStep = project.onboardingStep
      bodyConfig =
        (def :: BWConfig)
          { currProject = Nothing
          }
      page = case onboardingStep of
        Just "Survey" -> onboardingConfigBody pid
        Just "CreateMonitor" -> createMonitorPage pid
        Just "NotifChannel" -> notifChannels pid
        _ -> onboardingInfoBody pid firstName lastName
  addRespHeaders $ PageCtx bodyConfig page


-- addRespHeaders $ PageCtx bodyConfig $ div_ [class_ "container"] $ "Hello world"

data OnboardingInfoForm
  = OnboardingInfoForm
  { firstName :: Text
  , lastName :: Text
  , companyName :: Text
  , companySize :: Text
  , whereDidYouHearAboutUs :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, AE.FromJSON, AE.ToJSON)


data OnboardingConfForm = OnboardingConForm
  { location :: Text
  , functionality :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


onboardingInfoPost :: Projects.ProjectId -> OnboardingInfoForm -> ATAuthCtx (RespHeaders (Html ()))
onboardingInfoPost pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let firstName = form.firstName
      lastName = form.lastName
  let infoJson =
        KM.fromList
          [ ("companyName", AE.toJSON form.companyName)
          , ("companySize", AE.toJSON form.companySize)
          , ("foundUsfrom", AE.toJSON form.whereDidYouHearAboutUs)
          ]
      questions = case project.questions of
        Just (AE.Object o) -> AE.Object $ o <> infoJson
        _ -> AE.Object infoJson
      jsonBytes = AE.encode questions
  res <- dbtToEff $ execute Update [sql| update projects.projects set title=?, questions= ? where id=? |] (form.companyName, jsonBytes, pid)
  u <- dbtToEff $ execute Update [sql| update users.users set first_name= ?, last_name=? where id=? |] (firstName, lastName, sess.user.id)
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
        Just (AE.Object o) -> AE.Object $ o <> infoJson
        _ -> AE.Object infoJson
      jsonBytes = AE.encode questions
  res <- dbtToEff $ execute Update [sql| update projects.projects set  questions= ? where id=? |] (jsonBytes, pid)
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
  addRespHeaders ""


notifChannels :: Projects.ProjectId -> Html ()
notifChannels pid = do
  div_ [class_ "w-[550px] mx-auto mt-[156px] mb-10"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 4 "How should we notify you about issues?" $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
      form_ [class_ "flex-col w-full gap-8 flex mt-4"] $ do
        div_ [class_ "w-full flex flex-col gap-8"] $ do
          div_ [class_ "w-full gap-2 grid grid-cols-2"] $ do
            div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10 bg-weak justify-between items-center flex"] $ do
              div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
                img_ [src_ "/public/assets/svgs/slack.svg"]
                div_ [class_ "text-center text-black text-xl font-semibold"] "Slack"
              div_ [class_ "px-1 justify-center items-center gap-2 flex"] $
                button_ [class_ "border px-3 h-8 flex items-center shadow-sm border-[var(--brand-color)] rounded-lg text-brand font-semibold"] "Connect"
            div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10 bg-weak justify-between items-center flex"] $ do
              div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
                img_ [src_ "/public/assets/svgs/discord.svg"]
                div_ [class_ "text-center text-black text-xl font-semibold"] "Discord"
              div_ [class_ "px-1 justify-center items-center gap-2 flex"] $
                button_ [class_ "border px-3 h-8 flex items-center shadow-sm border-[var(--brand-color)] rounded-lg text-brand font-semibold"] "Connect"
          -- div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10 bg-weak justify-between items-center flex"] $ do
          --   div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
          --     img_ [src_ "/public/assets/svgs/teams.svg"]
          --     div_ [class_ "text-center text-black text-xl font-semibold"] "Teams"
          --   div_ [class_ "px-1 justify-center items-center gap-2 flex"] $
          --     button_ [class_ "border px-3 h-8 flex items-center shadow-sm border-[var(--brand-color)] rounded-lg text-brand font-semibold"] "Connect"
          -- div_ [class_ "h-12 px-3 py-2 bg-[#00157f]/0 rounded-xl justify-between items-center flex"] $ do
          --   div_ [class_ "w-[63.02px] h-4 relative  overflow-hidden"] ""
          --   div_ [class_ "bg-white/0 rounded-lg justify-center items-center flex"] $ do
          --     div_ [class_ "px-3 rounded-lg justify-center items-center flex"] $ do
          --       div_ [class_ "px-1 justify-center items-center gap-2 flex"] $
          --         div_ [class_ "text-center text-[#067a57] text-sm font-semibold"] "Connected"
          div_ [class_ "flex flex-col gap-2"] do
            div_ [class_ "flex w-full items-center gap-1"] $ do
              span_ [class_ "text-strong lowercase first-letter:uppercase"] $ "Notify the following email address"
            input_ [class_ "input w-full h-12", type_ "text", name_ "phoeNumber", required_ "required", value_ ""]
          div_ [class_ "flex flex-col gap-2"] do
            div_ [class_ "flex w-full items-center gap-1"] $ do
              span_ [class_ "text-strong lowercase first-letter:uppercase"] $ "Notify the following email address"
            textarea_ [class_ "w-full rounded-lg stroke-strong", type_ "text", name_ $ "emails", required_ "required", id_ "emails_input"] ""
          div_ [class_ "items-center gap-4 flex"] $ do
            button_ [class_ "px-6 h-14 flex items-center bg-brand text-white text-xl font-semibold rounded-lg"] "Proceed"
      script_
        [text|
     document.addEventListener('DOMContentLoaded', function() {
      var inputElem = document.querySelector('#emails_input')
      var tagify = new Tagify(inputElem)
      window.tagify = tagify
    })
  |]


createMonitorPage :: Projects.ProjectId -> Html ()
createMonitorPage pid = do
  div_ [class_ "w-[550px] mx-auto mt-[156px] mb-10"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 3 "Let's create your first endpoint monitor" $ "/p/" <> pid.toText <> "/onboarding?step=Survey"
      form_
        [ class_ "flex-col w-full gap-8 flex"
        , hxPost_ $ "/p/" <> pid.toText <> "/monitors/collection?onboarding=true"
        , hxExt_ "json-enc"
        , hxVals_ "js:{stepsData: saveStepData()}"
        ]
        $ do
          input_ [class_ "input w-full h-12", type_ "hidden", name_ "title", value_ "HEALTHCHECK"]
          div_ [class_ "w-full"] $ termRaw "assertion-builder" [id_ ""] ""
          div_ [class_ "w-full"] $ termRaw "steps-editor" [id_ "stepsEditor", term "isOnboarding" "true"] ""
          div_ [class_ "items-center gap-4 flex"] $ do
            button_ [class_ "px-6 h-14 flex items-center bg-brand text-white text-xl font-semibold rounded-lg", type_ "submit"] "Proceed"
            button_
              [ class_ "px-6 h-14 flex items-center border border-[var(--brand-color)] text-brand text-xl font-semibold rounded-lg"
              , onclick_ "window.addCollectionStep()"
              , type_ "button"
              ]
              "Add a step"
            a_
              [ class_ "px-2 h-14 flex items-center underline text-brand text-xl font-semibold"
              , type_ "button"
              , href_ $ "/p/" <> pid.toText <> "/onboarding?step=NotifChannel"
              ]
              "Skip"


onboardingInfoBody :: Projects.ProjectId -> Text -> Text -> Html ()
onboardingInfoBody pid firstName lastName = do
  div_ [class_ "w-[448px] mx-auto mt-[156px]"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 1 "Tell us a little bit about you" ""
      form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/info"] $ do
        div_ [class_ "flex-col w-full gap-4 mt-4 flex"] $ do
          mapM_ createInputField [("first Name" :: Text, firstName), ("last Name", lastName), ("company Name", "")]
          createSelectField "company Size" [("1 - 4", "1 to 5"), ("5 - 10", "5 to 10"), ("10 - 25", "10 to 25"), ("25+", "25 and above")]
          createSelectField "where Did You Hear About Us" [("google", "Google"), ("twitter", "Twitter"), ("linkedin", "LinkedIn"), ("friend", "Friend"), ("other", "Other")]
        div_ [class_ "items-center gap-1 flex"] $ do
          button_ [class_ "px-6 h-14 flex items-center bg-brand text-white text-xl font-semibold rounded-lg"] "Proceed"


onboardingConfigBody :: Projects.ProjectId -> Html ()
onboardingConfigBody pid = do
  div_ [class_ "w-[448px] mx-auto mt-[156px]"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 2 "Let's configure your project" $ "/p/" <> pid.toText <> "/onboarding?step=Info"
      form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/survey"] $ do
        div_ [class_ "flex-col w-full gap-14 mt-4 flex"] $ do
          div_ [class_ "flex-col gap-2 flex"] $ do
            div_ [class_ "items-center gap-[2px] flex"] $ do
              span_ [class_ "text-strong"] "Where should your project be hosted?"
              span_ [class_ "text-weak"] "*"
            div_ [class_ "pt-2 flex-col gap-4 flex text-sm text-strong"] $ do
              forM_ locations $ createBinaryField "radio" "location"
          div_ [class_ "flex-col gap-2 flex"] $ do
            div_ [class_ "items-center flex gap-[2px]"] $ do
              span_ [class_ "text-strong"] "Which APItoolkit features will you be using?"
              span_ [class_ "text-weak"] "*"
            div_ [class_ "pt-2 flex-col gap-4 flex"] $ do
              forM_ functionalities $ createBinaryField "checkbox" "functionality"
        div_ [class_ "items-center gap-1 flex"] $ do
          button_ [class_ "px-6 h-14 flex items-center bg-brand text-white text-xl font-semibold rounded-lg"] "Proceed"


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


createInputField :: (Text, Text) -> Html ()
createInputField (labelText, value) = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ "text-strong lowercase first-letter:uppercase"] (toHtml labelText)
      span_ [class_ "text-weak"] "*"
    input_ [class_ "input w-full h-12", type_ "text", name_ $ T.replace " " "" labelText, required_ "required", value_ value]


createSelectField :: Text -> Vector (Text, Text) -> Html ()
createSelectField labelText options = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ "text-strong lowercase first-letter:uppercase"] $ toHtml labelText
      span_ [class_ "text-weak"] "*"
    select_ [class_ "select w-full h-12", name_ $ T.replace " " "" labelText, required_ "required"] do
      option_ [value_ ""] ""
      forM_ options $ \(key, value) -> option_ [value_ key] $ toHtml value


createBinaryField :: Text -> Text -> (Text, Text) -> Html ()
createBinaryField kind name (value, label) = do
  div_ [class_ " items-center gap-3 inline-flex"] $ do
    input_ $ [class_ "w-6 h-6 rounded", type_ kind, name_ name, value_ value, id_ value] <> [required_ "required" | kind == "radio"]
    label_ [class_ "text-strong text-sm", Lucid.for_ value] $ toHtml label


stepIndicator :: Int -> Text -> Text -> Html ()
stepIndicator step title prevUrl =
  div_ [class_ "flex-col gap-4 flex w-full"] $ do
    div_ [class_ "flex-col gap-2 flex w-full"] $ do
      div_ [class_ "text-strong text-base font-semibold"] $ "Step " <> show step <> " of 6"
      div_ [class_ "grid grid-cols-6 w-full gap-1"] $ do
        forM_ [1 .. 6] $ \i -> div_ [class_ $ if step >= i then "bg-brand rounded" else "bg-weak shadow-[inset_0px_1px_4px_0px_rgba(0,0,0,0.08)] border border-[#001066]/10" <> " h-2 w-full rounded"] pass
      when (step > 1) $ do
        a_ [class_ "flex items-center gap-3 flex text-brand w-full mt-2", href_ prevUrl] $ do
          faSprite_ "arrow-left" "regular" "h-4 w-4"
          span_ [class_ "font-semibold"] "Back"
    span_ [class_ "text-strong text-4xl font-semibold mt-4"] $ toHtml title
