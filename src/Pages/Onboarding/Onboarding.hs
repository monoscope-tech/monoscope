{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use lambda-case" #-}
module Pages.Onboarding.Onboarding (
  onboardingGetH,
  onboardingInfoPost,
  onboardingConfPost,
  discorPostH,
  phoneEmailPostH,
  checkIntegrationGet,
  DiscordForm (..),
  NotifChannelForm (..),
  OnboardingInfoForm (..),
  OnboardingConfForm (..),
) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Default (def)
import Data.Text qualified as T
import Data.Vector as V (Vector, fromList, toList)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select, Update), execute, query, queryOne)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx
import Models.Projects.Projects (OnboardingStep (..))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))

import Database.PostgreSQL.Transact (DBT)
import Lucid.Hyperscript (__)
import Models.Tests.Testing qualified as Testing
import Pages.Components qualified as Components
import Pages.IntegrationDemos.Golang (golangGuide)
import Pages.IntegrationDemos.Javascript (javascriptGuide)
import Pkg.Components qualified as Components
import Pkg.Mail (sendDiscordNotif)
import PyF (fmt)
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders, redirectCS)
import Utils (faSprite_, lookupValueText, redirect)
import Web.FormUrlEncoded


-- 'Info', 'Survey', 'CreateMonitor','NotifChannel','Integration', 'Pricing', 'Complete'
onboardingGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
onboardingGetH pid onboardingStep = do
  (sess, project) <- Sessions.sessionAndProject pid
  appContx <- ask @AuthContext
  let bodyConfig =
        (def :: BWConfig)
          { currProject = Nothing
          }
      questions = fromMaybe (AE.Object []) project.questions
  case onboardingStep of
    Just "Survey" -> do
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
    Just "CreateMonitor" -> do
      colsM <- dbtToEff $ Testing.getCollectionByTitle pid "HEALTH CHECK."
      addRespHeaders $ PageCtx bodyConfig $ createMonitorPage pid colsM
    Just "NotifChannel" -> do
      let phone = fromMaybe "" project.notifyPhoneNumber
          emails = project.notifyEmails
      addRespHeaders $ PageCtx bodyConfig $ notifChannels pid appContx.config.slackRedirectUri phone emails
    Just "Integration" -> do
      addRespHeaders $ PageCtx bodyConfig $ integrationsPage pid
    _ -> do
      let firstName = sess.user.firstName
          lastName = sess.user.lastName
          (companyName, companySize, foundUsfrom) = (fromMaybe "" $ lookupValueText questions "companyName", fromMaybe "" $ lookupValueText questions "companySize", fromMaybe "" $ lookupValueText questions "foundUsFrom")
      addRespHeaders $ PageCtx bodyConfig $ onboardingInfoBody pid firstName lastName companyName companySize foundUsfrom


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


data DiscordForm = DiscordForm {url :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


data NotifChannelForm = NotifChannelForm
  { phoneNumber :: Text
  , emails :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, AE.FromJSON, AE.ToJSON)


discorPostH :: Projects.ProjectId -> DiscordForm -> ATAuthCtx (RespHeaders (Html ()))
discorPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let notifs = ordNub $ Projects.parseNotifChannel <$> (V.toList project.notificationsChannel <> [Projects.NDiscord])
  sendDiscordNotif form.url "APItoolkit connected successfully"
  _ <- dbtToEff do Projects.updateNotificationsChannel pid notifs (Just form.url)
  addRespHeaders $ button_ [class_ "text-green-500 font-semibold"] "Connected"


phoneEmailPostH :: Projects.ProjectId -> NotifChannelForm -> ATAuthCtx (RespHeaders (Html ()))
phoneEmailPostH pid form = do
  (sess, project) <- Sessions.sessionAndProject pid
  let phone = form.phoneNumber
      emails = form.emails
      notifs = if phone /= "" then V.toList project.notificationsChannel <> [Projects.NPhone] else V.toList project.notificationsChannel
      notifs' = if emails /= [] then notifs <> [Projects.NEmail] else notifs
      notifsTxt = ordNub $ Projects.parseNotifChannel <$> notifs'
      q = [sql| update projects.projects set notifications_channel=?::notification_channel_enum[], notify_phone_number=?, notify_emails=?::text[] where id=? |]
  _ <- dbtToEff $ execute Update q (V.fromList notifsTxt, phone, V.fromList emails, pid)
  addRespHeaders $ inviteTeamMemberModal pid (V.fromList emails)


checkIntegrationGet :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
checkIntegrationGet pid language = do
  let q = [sql| SELECT * FROM telemetry.spans WHERE project_id = ? and resource ->> 'telemetry.sdk.language' = ?|]
  v <- dbtToEff (queryOne Select q (pid, language) :: (DBT IO (Maybe Telemetry.SpanRecord)))
  case v of
    Nothing -> addRespHeaders $ div_ [class_ "flex items-center gap-2 text-green-500"] do
      span_ "verified"
      faSprite_ "circle-check" "regular" "h-4 w-4"
    _ -> addRespHeaders $ integrationCheck pid language


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
        Just (AE.Object o) -> AE.Object $ infoJson <> o
        _ -> AE.Object infoJson
      jsonBytes = AE.encode questions
  res <- dbtToEff $ execute Update [sql| update projects.projects set  questions= ? where id=? |] (jsonBytes, pid)
  redirectCS $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
  addRespHeaders ""


integrationsPage :: Projects.ProjectId -> Html ()
integrationsPage pid =
  div_ [class_ "w-[1200px] flex justify-between mx-auto"] $ do
    div_ [class_ "w-[448px] mt-[156px] mb-10"] $ do
      div_ [class_ "flex-col gap-4 flex w-full"] $ do
        stepIndicator 5 "Instrument your apps or servers" $ "/p/" <> pid.toText <> "/onboarding?step=NotifChannel"
        div_ [class_ "flex-col w-full gap-8 flex mt-4"] do
          p_ [class_ "text-strong"] "Send Logs, Metrics or Traces. Click proceed when you’re done integrating your applications. learn more"
          div_ [class_ "flex flex-col gap-4 "] $ do
            div_ [class_ "flex flex-col gap-2"] do
              input_ [class_ "w-full input input-sm"]
              div_ [class_ "w-full"] $ do
                div_ [class_ "flex items-center justify-between"] $ do
                  tagItem "All" False
                  tagItem "Backend" True
                  tagItem "Frontend" False
                  tagItem "Database" False
                  tagItem "Infra" False
                  tagItem "Others" False
            div_ [class_ "flex flex-col gap-2"] do
              languageItem pid "Javascript" "js"
              languageItem pid "Golang" "go"
              languageItem pid "Python" "py"
              languageItem pid "PHP" "php"
              languageItem pid "C#" "cs"
          div_ [class_ "flex items-center gap-4"] do
            button_ [class_ "btn btn-primary"] "Confirm & Proceed"
            button_ [class_ "font-semibold text-brand underline"] "Skip"
    div_ [class_ "w-[700px]"] do
      div_ [class_ "fixed top-1/2 -translate-y-1/2 w-[min(48vw,800px)] border rounded-2xl border-weak flex justify-between items-center h-[90vh]"] do
        div_ [class_ "w-full h-full overflow-y-auto p-6"] do
          javascriptGuide "hello"
          golangGuide "hello"
    script_
      [text|
      function toggleCheckbox(event) {
        event.stopPropagation();
        event.target.nextSibling.lastChild.classList.toggle('hidden');
      }
    |]


languageItem :: Projects.ProjectId -> Text -> Text -> Html ()
languageItem pid lang ext = do
  let ext_main = ext <> "_main"
  let clck = [text|on click add .hidden to <.lang-guide/> then remove .hidden from #$ext_main|]
  button_
    [ class_ "h-12 px-3 py-2 bg-[#00157f]/0 rounded-xl border border-[#001066]/10 justify-start items-center gap-3 inline-flex"
    , term "_" clck
    ]
    do
      input_ [type_ "checkbox", class_ "checkbox shrink-0", onclick_ "toggleCheckbox(event)"]
      div_ [class_ "flex w-full items-center justify-between"] do
        div_ [class_ "flex items-center gap-2"] do
          img_ [class_ "h-5 w-5", src_ $ "/public/assets/svgs/" <> ext <> ".svg"]
          span_ [class_ "text-sm font-semibold text-strong"] $ toHtml lang
        div_ [class_ "hidden text-sm toggle-target", id_ $ "integration-check-container" <> T.replace "#" "" lang] do
          integrationCheck pid lang


integrationCheck :: Projects.ProjectId -> Text -> Html ()
integrationCheck pid language = do
  div_
    [ class_ "flex items-center gap-2 "
    , hxGet_ $ "/p/" <> pid.toText <> "/onboarding/integration-check/" <> language
    , hxSwap_ "innerHTML"
    , hxTarget_ $ "#integration-check-container" <> (T.replace "#" "" language)
    , hxTrigger_ "load delay:5s"
    ]
    do
      span_ [class_ "text-strong"] "waiting for events"
      faSprite_ "spinner" "regular" "h-4 w-4 animate-spin"


tagItem :: Text -> Bool -> Html ()
tagItem label isActive =
  div_ [class_ "px-3 py-1 rounded-2xl  border-[#001066]/10 bg-weak"] $
    span_ [class_ "text-sm"] (toHtml label)


notifChannels :: Projects.ProjectId -> Text -> Text -> Vector Text -> Html ()
notifChannels pid slackRedirectUri phone emails = do
  div_ [class_ "w-[550px] mx-auto mt-[156px] mb-10"] $ do
    div_ [id_ "inviteModalContainer"] pass
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 4 "How should we notify you about issues?" $ "/p/" <> pid.toText <> "/onboarding?step=CreateMonitor"
      div_ [class_ "flex-col w-full gap-8 flex mt-4"] $ do
        div_ [class_ "w-full flex flex-col gap-8"] $ do
          div_ [class_ "w-full gap-2 grid grid-cols-2"] $ do
            div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10 bg-weak justify-between items-center flex"] $ do
              div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
                img_ [src_ "/public/assets/svgs/slack.svg"]
                span_ [class_ "text-center text-black text-xl font-semibold"] "Slack"
              let
                -- slackDev = "https://slack.com/oauth/v2/authorize?client_id=6187126212950.6193763110659&scope=chat:write,incoming-webhook&user_scope="
                slackPro = "https://slack.com/oauth/v2/authorize?client_id=6211090672305.6200958370180&scope=chat:write,incoming-webhook&user_scope="

              a_
                [ target_ "_blank"
                , class_ "border px-3 h-8 flex items-center shadow-sm border-[var(--brand-color)] rounded-lg text-brand font-semibold"
                , href_ $ slackPro <> "&redirect_uri=" <> slackRedirectUri <> pid.toText <> "?onboarding=true"
                ]
                do
                  "Connect"
            div_ [class_ "px-3 py-2 rounded-xl border border-[#001066]/10 bg-weak justify-between items-center flex"] $ do
              div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
                img_ [src_ "/public/assets/svgs/discord.svg"]
                div_ [class_ "text-center text-black text-xl font-semibold"] "Discord"
              discordModal pid
          form_
            [ class_ "flex flex-col gap-8"
            , hxPost_ $ "/p/" <> pid.toText <> "/onboarding/phone-emails"
            , hxExt_ "json-enc"
            , hxVals_ "js:{phoneNumber: document.getElementById('phone').value , emails: getTags()}"
            , hxTarget_ "#inviteModalContainer"
            , hxSwap_ "innerHTML"
            ]
            $ do
              div_ [class_ "flex flex-col gap-2"] do
                div_ [class_ "flex w-full items-center gap-1"] $ do
                  span_ [class_ "text-strong lowercase first-letter:uppercase"] "Notify phone number"
                input_ [class_ "input w-full h-12", type_ "text", name_ "phoneNumber", id_ "phone", value_ phone]
              div_ [class_ "flex flex-col gap-2"] do
                div_ [class_ "flex w-full items-center gap-1"] $ do
                  span_ [class_ "text-strong lowercase first-letter:uppercase"] "Notify the following email address"
                textarea_ [class_ "w-full rounded-lg stroke-strong", type_ "text", name_ "emails", id_ "emails_input"] ""
              div_ [class_ "items-center gap-4 flex"] $ do
                button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg"] "Proceed"
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
        ]
        $ do
          input_ [class_ "input w-full h-12", type_ "hidden", name_ "title", value_ "HEALTH CHECK."]
          whenJust colM $ \col -> do
            input_ [type_ "hidden", name_ "collectionId", value_ col.id.toText]
          div_ [class_ "w-full"] $ termRaw "assertion-builder" [id_ ""] ""
          div_ [class_ "w-full"] $ termRaw "steps-editor" [id_ "stepsEditor", term "isOnboarding" "true"] ""
          div_ [class_ "items-center gap-4 flex"] $ do
            button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg", type_ "submit"] "Proceed"
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


discordModal :: Projects.ProjectId -> Html ()
discordModal pid = do
  div_ [id_ "discord-modal"] $ do
    label_ [Lucid.for_ "my_modal_6", class_ "border px-3 h-8 flex items-center shadow-sm border-[var(--brand-color)] rounded-lg text-brand font-semibold"] "Connect"
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
              p_ [class_ "py-4"] "This modal works with a hidden checkbox!"
              input_ [type_ "text", class_ "input w-full h-12", id_ "discord-url", name_ "url", required_ "required"]
            div_ [class_ "modal-action"] do
              button_ [class_ "btn btn-primary rounded-lg btn-sm", type_ "button", onclick_ "htmx.trigger('#dscrd','submit')"] "Submit"
      label_ [class_ "modal-backdrop", Lucid.for_ "my_modal_6"] "Close"


onboardingInfoBody :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Html ()
onboardingInfoBody pid firstName lastName cName cSize fUsFrm = do
  div_ [class_ "w-[550px] mx-auto mt-[156px]"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 1 "Tell us a little bit about you" ""
      form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/info"] $ do
        div_ [class_ "flex-col w-full gap-4 mt-4 flex"] $ do
          mapM_ createInputField [("first Name" :: Text, firstName), ("last Name", lastName), ("company Name", cName)]
          createSelectField cSize "company Size" [("1 - 4", "1 to 4"), ("5 - 10", "5 to 10"), ("11 - 25", "11 to 25"), ("26+", "26 and above")]
          createSelectField fUsFrm "where Did You Hear About Us" [("google", "Google"), ("twitter", "Twitter"), ("linkedin", "LinkedIn"), ("friend", "Friend"), ("other", "Other")]
        div_ [class_ "items-center gap-1 flex"] $ do
          button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg"] "Proceed"


onboardingConfigBody :: Projects.ProjectId -> Text -> [Text] -> Html ()
onboardingConfigBody pid loca func = do
  div_ [class_ "w-[550px] mx-auto mt-[156px]"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      stepIndicator 2 "Let's configure your project" $ "/p/" <> pid.toText <> "/onboarding?step=Info"
      form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ "/p/" <> pid.toText <> "/onboarding/survey"] $ do
        div_ [class_ "flex-col w-full gap-14 mt-4 flex"] $ do
          div_ [class_ "flex-col gap-2 flex"] $ do
            div_ [class_ "items-center gap-[2px] flex"] $ do
              span_ [class_ "text-strong"] "Where should your project be hosted?"
              span_ [class_ "text-weak"] "*"
            div_ [class_ "pt-2 flex-col gap-4 flex text-sm text-strong"] $ do
              forM_ locations $ createBinaryField "radio" "location" [loca]
          div_ [class_ "flex-col gap-2 flex"] $ do
            div_ [class_ "items-center flex gap-[2px]"] $ do
              span_ [class_ "text-strong"] "Which APItoolkit features will you be using?"
              span_ [class_ "text-weak"] "*"
            div_ [class_ "pt-2 flex-col gap-4 flex"] $ do
              forM_ functionalities $ createBinaryField "checkbox" "functionality" func
        div_ [class_ "items-center gap-1 flex"] $ do
          button_ [class_ "px-6 h-14 flex items-center btn-primary text-xl font-semibold rounded-lg"] "Proceed"


inviteTeamMemberModal :: Projects.ProjectId -> Vector Text -> Html ()
inviteTeamMemberModal pid emails = do
  div_ [id_ "invite-modal-container"] $ do
    input_ [type_ "checkbox", id_ "inviteModal", class_ "modal-toggle", checked_]
    div_ [class_ "modal p-8", role_ "dialog"] do
      div_ [class_ "modal-box flex flex-col gap-4"] $ do
        div_ [class_ "p-3 bg-[#0acc91]/5 rounded-full w-max border-[#067a57]/20 gap-2 inline-flex"] $
          faSprite_ "circle-check" "regular" "h-6 w-6 text-green-500"
        span_ [class_ "text-strong text-2xl font-semibold"] "Test notifications sent"
        div_ [class_ "text-[#000833]/60"] "No notification? Close this modal and verify emails and channels."
        div_ [class_ "h-1 w-full bg-weak"] pass
        div_ [class_ "flex-col gap-4 flex"] $ do
          div_ [class_ "flex-col gap-5 flex"] $ do
            div_ [class_ "w-full text-[#000833]/60"] "The users below will be added to your project as team members"
            div_ [class_ "w-full gap-4 flex flex-col"] $ do
              div_ [class_ "w-full gap-2 flex items-center"] $ do
                div_ [class_ "flex-col gap-1 inline-flex w-full"] $
                  div_ [class_ "flex flex-col gap-1 w-full"] $ do
                    input_ [class_ "input input-sm w-full", placeholder_ "email@example.com", type_ "email", id_ "add-member-input"]
                button_ [class_ "btn-primary rounded-lg  px-3 h-8 justify-center items-center flex text-white text-sm font-semibold", onclick_ "appendMember()"] "invite"
              div_ [class_ "w-full"] $ do
                div_ [class_ "w-full text-strong text-sm font-semibold"] "Members"
                div_ [class_ "w-full border-t border-weak"] $ do
                  form_ [class_ "flex-col flex", id_ "members-container", hxPost_ $ "/p/" <> pid.toText <> "/manage_members?onboarding=true"] $ do
                    inviteMemberItem "hidden"
                    forM_ emails $ \email -> do
                      inviteMemberItem email
        div_ [class_ "modal-action w-full flex items-center justify-start gap-2 mt-2"] do
          button_ [class_ "btn btn-primary font-semibold rounded-lg", type_ "button", onclick_ "htmx.trigger('#members-container', 'submit')"] "Proceed"
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
      span_ [class_ "text-[#000626]/90 text-sm font-normal"] $
        toHtml email
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
      span_ [class_ "text-strong lowercase first-letter:uppercase"] (toHtml labelText)
      span_ [class_ "text-weak"] "*"
    input_ [class_ "input w-full h-12", type_ "text", name_ $ T.replace " " "" labelText, required_ "required", value_ value]


createSelectField :: Text -> Text -> Vector (Text, Text) -> Html ()
createSelectField val labelText options = do
  div_ [class_ "flex flex-col gap-1 w-full"] $ do
    div_ [class_ "flex w-full items-center gap-1"] $ do
      span_ [class_ "text-strong lowercase first-letter:uppercase"] $ toHtml labelText
      span_ [class_ "text-weak"] "*"
    select_ [class_ "select w-full h-12", name_ $ T.replace " " "" labelText, required_ "required"] do
      option_ [value_ ""] ""
      forM_ options $ \(key, value) -> option_ ([value_ key] ++ [selected_ val | val == key]) $ toHtml value


createBinaryField :: Text -> Text -> [Text] -> (Text, Text) -> Html ()
createBinaryField kind name selectedValues (value, label) = do
  div_ [class_ " items-center gap-3 inline-flex"] $ do
    let checked = value `elem` selectedValues
    input_ $ [class_ "w-6 h-6 rounded", type_ kind, name_ name, value_ value, id_ value] <> [required_ "required" | kind == "radio"] <> [checked_ | checked]
    label_ [class_ "text-strong text-sm", Lucid.for_ value] $ toHtml label


stepIndicator :: Int -> Text -> Text -> Html ()
stepIndicator step title prevUrl =
  div_ [class_ "flex-col gap-4 flex w-full"] $ do
    div_ [class_ "flex-col gap-2 flex w-full"] $ do
      div_ [class_ "text-strong text-base font-semibold"] $ "Step " <> show step <> " of 6"
      div_ [class_ "grid grid-cols-6 w-full gap-1"] $ do
        forM_ [1 .. 6] $ \i -> div_ [class_ $ if step >= i then "btn-primary rounded" else "bg-weak shadow-[inset_0px_1px_4px_0px_rgba(0,0,0,0.08)] border border-[#001066]/10" <> " h-2 w-full rounded"] pass
      when (step > 1) $ do
        a_ [class_ "flex items-center gap-3 flex text-brand w-full mt-2", href_ prevUrl] $ do
          faSprite_ "arrow-left" "regular" "h-4 w-4"
          span_ [class_ "font-semibold"] "Back"
    span_ [class_ "text-strong text-4xl font-semibold mt-4"] $ toHtml title
