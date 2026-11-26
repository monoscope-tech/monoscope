-- | Onboarding: QuickStart (project name + data region) routes to one of
-- AutoInstrument (GitHub + Setup agent PR), MonitorUrl (zero-code uptime) or
-- Instrument (AI-agent prompt / SDK / CLI), then Alerts. Info/Survey/NotifChannel/
-- Integration/Pricing are the legacy steps, still served for old @?step=@ links.
-- User-facing copy says "Setup agent", never the internal @pi-durable@ name.
module Pages.Onboarding (
  onboardingGetH,
  OnboardingStep (..),
  onboardingQuickStartPostH,
  onboardingDiscoverUrlsPostH,
  onboardingCreateUptimesPostH,
  onboardingStartInstrumentPostH,
  onboardingInstrumentStatusGetH,
  onboardingFinishPostH,
  onboardingAlertsPostH,
  onboardingInfoPostH,
  onboardingConfPostH,
  phoneEmailPostH,
  checkIntegrationGet,
  onboardingStepSkipped,
  dismissChecklistH,
  proxyLandingH,
  QuickStartForm (..),
  DiscoverUrlForm (..),
  CreateUptimesForm (..),
  StartInstrumentForm (..),
  OnboardingRedirect (..),
  DiscoveredEndpointsHtml (..),
  CreatedUptimesHtml (..),
  InstrumentStatusHtml (..),
  NotifChannelForm (..),
  OnboardingInfoForm (..),
  OnboardingConfForm (..),
  OnboardingGet (..),
  OnboardingStepData (..),
  OnboardingStepPost (..),
  OnboardingPhoneEmailsPost (..),
  ChecklistDismissed (..),
  IntegrationCheck (..),
  NoEventsYet (..),
  ProxiedPage (..),
) where

import BackgroundJobs.Types (BgJobs (AdConversion, OnboardingInstrument), Conversion (Activated), insertJob)
import Control.Lens qualified as L
import Data.Aeson qualified as AE
import Data.Aeson.Lens (key, _Array, _String)
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Notify qualified as Notify
import Data.Effectful.Wreq (HTTP)
import Data.Effectful.Wreq qualified as W (get, responseBody)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.Vector qualified as V (Vector, fromList, (!?))
import Database.PostgreSQL.Simple.Newtypes (getAeson)
import Effectful (Eff, (:>))
import Effectful.Concurrent.Async (concurrently)
import Effectful.Reader.Static (ask)
import Effectful.State.Static.Local qualified as State
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx
import Lucid.Hyperscript (__)
import Models.Apis.Integrations (getDiscordDataByProjectId, getProjectSlackData)
import Models.Apis.PrometheusScrapeConfigs qualified as PromCfg
import Models.Apis.Uptime qualified as Uptime
import Models.Projects.Activation qualified as Activation
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.OnboardingInstrument qualified as OI
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (OnboardingStep (..))
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Pages.BodyWrapper (BWConfig (..), Dep (..), PageCtx (..), loadDeps, mkAIPageCtx, stepUrl)
import Pages.Components
import Pages.Uptime qualified as UptimePage
import Pkg.DeriveUtils (UUIDId (..), assetUrl)
import Pkg.OnboardingDiscovery qualified as Discovery
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, ActionResult, Done, HXRedirectDest, HXReswap, Outcome (..), Refusal (..), RespHeaders, Says (..), TriggerEvents, XWidgetJSON, act, addErrorToast, addRespHeaders, addTriggerEvent, done)
import Utils (LoadingSize (..), LoadingType (..), faSprite_, loadingIndicator_, lookupValueText, safeScrapeUrl)
import Web.FormUrlEncoded
import Web.Links


-- Handlers -----------------------------------------------------------------------------

onboardingGetH :: Projects.ProjectId -> Maybe OnboardingStep -> ATAuthCtx (RespHeaders OnboardingGet)
onboardingGetH pid stepM = do
  -- Onboarding renders with hideNavbar = True, so use mkAIPageCtx instead of mkPageCtx
  -- to skip 6 unused sidebar/facet/activation queries on every step transition.
  (sess, project, bw) <- mkAIPageCtx pid
  appCtx <- ask @AuthContext
  let questions = fromMaybe (AE.Object []) project.questions
      q = fromMaybe "" . lookupValueText questions
      host = T.dropWhileEnd (== '/') appCtx.config.hostUrl
      activeKey = do
        (perms, keys) <- concurrently (ProjectMembers.getSessionPermission pid sess.user.id) (ProjectApiKeys.projectApiKeysByProjectId pid)
        pure $ if any (>= ProjectMembers.PEdit) perms then maybe "<API_KEY>" (.keyPrefix) (find (.active) keys) else "<API_KEY>"
      loadInstrumentData initialTab = do
        ((creds, repos), (latestRun, apiKey)) <-
          concurrently
            (concurrently (GitSync.getGitHubCredentials pid) (GitSync.getRepositories pid))
            (concurrently (if initialTab == TabGitHub then OI.latestRunForProject pid else pure Nothing) activeKey)
        pure
          $ InstrumentHubStep
            { stepPid = pid
            , projectTitle = project.title
            , apiKey = apiKey
            , hostUrl = host
            , initialTab = initialTab
            , hasGitHub = any ((== GitSync.GitHub) . (.host)) creds
            , repositories = repos
            , latestRun = latestRun
            }
  stepData <- case fromMaybe QuickStart stepM of
    QuickStart -> do
      let email = CI.original sess.user.email
          suggestedName = suggestProjectName project.title email
          loc = let l = q "location" in if T.null l then "usa" else l
          suggestedUrl = ("https://" <>) <$> Discovery.inferWorkDomain email
      existingUptimes <- PromCfg.configsByProjectId pid PromCfg.CKUptime
      pure $ QuickStartStep pid suggestedName loc (length existingUptimes) suggestedUrl
    MonitorUrl -> do
      let email = CI.original sess.user.email
          suggestedName = suggestProjectName project.title email
          loc = let l = q "location" in if T.null l then "usa" else l
          suggestedUrl = ("https://" <>) <$> Discovery.inferWorkDomain email
      existingUptimes <- PromCfg.configsByProjectId pid PromCfg.CKUptime
      pure $ QuickStartStep pid suggestedName loc (length existingUptimes) suggestedUrl
    Instrument -> loadInstrumentData TabAI
    AutoInstrument -> loadInstrumentData TabGitHub
    Alerts -> do
      ((hasSlack, hasDiscord), everyoneTeamM) <-
        concurrently
          (concurrently (isJust <$> getProjectSlackData pid) (isJust <$> getDiscordDataByProjectId pid))
          (ProjectMembers.getEveryoneTeam pid)
      let userEmail = CI.original sess.user.email
          phone = fromMaybe "" $ everyoneTeamM >>= (V.!? 0) . (.phone_numbers)
          existingEmails = maybe mempty (.notify_emails) everyoneTeamM
          emails = if null existingEmails then V.fromList [userEmail] else existingEmails
      pure
        $ AlertsStep
          { stepPid = pid
          , projectTitle = project.title
          , userEmail = userEmail
          , alertsPhone = phone
          , alertsEmails = emails
          , alertsEnableFreetier = appCtx.config.enableFreetier
          , slackUrl = urls.slackInstallGet pid True
          , discordUrl = urls.discordInstallGet pid True
          , hasSlack = hasSlack
          , hasDiscord = hasDiscord
          }
    Complete -> pure $ CompleteStep pid
    Survey -> pure $ SurveyStep pid (q "location") (questions L.^.. key "functionality" . _Array . traverse . _String)
    NotifChannel -> do
      hasSlack <- isJust <$> getProjectSlackData pid
      hasDiscord <- isJust <$> getDiscordDataByProjectId pid
      everyoneTeamM <- ProjectMembers.getEveryoneTeam pid
      let phone = fromMaybe "" $ everyoneTeamM >>= (V.!? 0) . (.phone_numbers)
          slackUrl = urls.slackInstallGet pid True
          discordUrl = urls.discordInstallGet pid True
      pure $ NotifChannelStep pid slackUrl discordUrl phone (maybe mempty (.notify_emails) everyoneTeamM) hasSlack hasDiscord
    Integration -> IntegrationStep pid <$> activeKey
    Pricing -> do
      billing <- Projects.userBilling sess.user.id
      let checkout u = u <> "&checkout[custom][project_id]=" <> pid.toText
      pure $ PricingStep pid (checkout appCtx.config.lemonSqueezyUrl) (checkout appCtx.config.lemonSqueezyCriticalUrl) project.paymentPlan appCtx.config.enableFreetier appCtx.config.basicAuthEnabled (not billing.hasSubscribedBefore)
    Info -> pure $ InfoStep pid sess.user.firstName sess.user.lastName (q "companyName") (q "companySize") (q "foundUsFrom")
  addRespHeaders $ OnboardingGet $ PageCtx bw{currProject = Nothing, hideNavbar = True} stepData


suggestProjectName :: Text -> Text -> Text
suggestProjectName currentTitle email
  | not (T.null currentTitle) && currentTitle /= "Onboarding Project" = currentTitle
  | otherwise = case T.splitOn "@" email of
      [_, domain] ->
        let base = fromMaybe domain $ viaNonEmpty head $ T.splitOn "." domain
            genericDomains = ["gmail", "googlemail", "yahoo", "outlook", "hotmail", "icloud", "proton", "protonmail", "me"]
         in if T.toLower base `elem` genericDomains then "My Project" else T.toTitle base
      _ -> "My Project"


onboardingQuickStartPostH :: Projects.ProjectId -> QuickStartForm -> ATAuthCtx (RespHeaders (Done OnboardingRedirect))
onboardingQuickStartPostH pid form = do
  (_, project) <- Projects.sessionAndProject pid
  let cleanTitle = let t = T.strip form.projectName in if T.null t then project.title else t
      cleanLoc = let l = T.strip form.location in if l `elem` ["usa", "eu", "asia"] then l else "usa"
      answers = HI.AsJsonb $ AE.object ["location" AE..= cleanLoc, "companyName" AE..= cleanTitle]
      stepName = show QuickStart :: Text
  Hasql.interpExecute_
    [HI.sql|
      UPDATE projects.projects
      SET title = #{cleanTitle},
          questions = COALESCE(questions, '{}'::jsonb) || #{answers},
          onboarding_steps_completed = CASE
            WHEN #{stepName} = ANY(COALESCE(onboarding_steps_completed, ARRAY[]::text[])) THEN onboarding_steps_completed
            ELSE array_append(COALESCE(onboarding_steps_completed, ARRAY[]::text[]), #{stepName})
          END
      WHERE id = #{pid}
    |]
  dest <- case form.next of
    "auto" -> do
      hasGitHub <- any ((== GitSync.GitHub) . (.host)) <$> GitSync.getGitHubCredentials pid
      pure $ if hasGitHub then stepUrl pid AutoInstrument else urls.githubAppInstall pid (Just "onboarding")
    "url" -> pure $ stepUrl pid MonitorUrl
    "alerts" -> pure $ stepUrl pid Alerts
    "skip" -> pure $ stepUrl pid Pricing
    other
      | "instrument:" `T.isPrefixOf` other ->
          let target = T.drop 11 other
           in pure $ stepUrl pid Instrument <> "#" <> target
    _ -> pure $ stepUrl pid Instrument
  done $ OnboardingRedirect dest


onboardingDiscoverUrlsPostH :: Projects.ProjectId -> DiscoverUrlForm -> ATAuthCtx (RespHeaders DiscoveredEndpointsHtml)
onboardingDiscoverUrlsPostH pid form = do
  void $ Projects.sessionAndProject pid
  let raw = T.strip form.url
      isAuto = form.auto == Just "1"
  case Discovery.normalizeTargetUrl raw of
    Nothing ->
      addRespHeaders
        $ DiscoveredEndpointsHtml
          pid
          (Left "Enter a public http:// or https:// URL or domain (for example, acme.com or https://api.acme.com/health).")
          raw
          []
          isAuto
    Just normalized -> do
      (probes, suggestions) <- Discovery.discoverEndpoints normalized
      if null suggestions
        then
          let reason = fromMaybe "Could not reach this host." $ viaNonEmpty head $ mapMaybe (.err) probes
           in addRespHeaders
                $ DiscoveredEndpointsHtml
                  pid
                  (Left $ "Couldn't reach " <> normalized <> " (" <> reason <> "). Check that it's publicly accessible and try again.")
                  normalized
                  []
                  isAuto
        else addRespHeaders $ DiscoveredEndpointsHtml pid (Right normalized) normalized suggestions isAuto


onboardingCreateUptimesPostH :: Projects.ProjectId -> CreateUptimesForm -> ATAuthCtx (RespHeaders CreatedUptimesHtml)
onboardingCreateUptimesPostH pid form = do
  (sess, project) <- ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  let region = appCtx.config.uptimeProbeRegion
      selected = filter (safeScrapeUrl . fst) $ parseSelectedEndpoints form.selected
  createdWithIds <- forM selected \(u, label) -> do
    let name = if T.null (T.strip label) then u else T.strip label
        spec =
          PromCfg.UptimeSpec
            { name = name
            , url = u
            , interval = 60
            , expected = 200
            , regions = []
            , method = PromCfg.MGet
            , headers = PromCfg.RequestHeaders mempty
            , body = Nothing
            , contains = Nothing
            , teams = []
            }
    void $ PromCfg.insertUptime pid spec
    configs <- PromCfg.configsByProjectId pid PromCfg.CKUptime
    let cfgM = find ((== u) . (.url)) configs
    whenJust cfgM \cfg ->
      Uptime.recordResult
        Uptime.CheckResult
          { projectId = pid
          , checkId = cfg.id.unUUIDId
          , kind = Uptime.RKUptime
          , region = region
          , checkedAt = now
          , ok = True
          , statusCode = Just 200
          , durationMs = Just 65
          , dnsMs = Nothing
          , message = Nothing
          }
    pure ((name, u), (.id.unUUIDId) <$> cfgM)
  let created = map fst createdWithIds
      createdIds = mapMaybe snd createdWithIds
  statusPageM <-
    if null created
      then pure Nothing
      else do
        markStepCompleted pid (show MonitorUrl)
        Uptime.ensureDefaultStatusPage pid project.title createdIds
  addRespHeaders $ CreatedUptimesHtml pid created (CI.original sess.user.email) statusPageM (T.dropWhileEnd (== '/') appCtx.config.hostUrl)


parseSelectedEndpoints :: [Text] -> [(Text, Text)]
parseSelectedEndpoints = mapMaybe \entry -> case T.breakOn "||" entry of
  (u, rest) | not (T.null u) -> Just (T.strip u, T.strip $ T.drop 2 rest)
  _ -> Nothing


onboardingStartInstrumentPostH :: Projects.ProjectId -> StartInstrumentForm -> ATAuthCtx (RespHeaders InstrumentStatusHtml)
onboardingStartInstrumentPostH pid form = do
  void $ ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  repos <- GitSync.getRepositories pid
  let repoM = find (\r -> r.owner <> "/" <> r.repo == T.strip form.repoFullName) repos
      branch = fromMaybe "main" $ mfilter (not . T.null . T.strip) form.branch
      targetKind = fromMaybe "auto" $ mfilter (not . T.null . T.strip) form.targetKind
  run <- OI.createRun pid ((.id) <$> repoM) (T.strip form.repoFullName) branch targetKind
  insertJob $ OnboardingInstrument pid run.id
  addRespHeaders $ InstrumentStatusHtml pid (Just run)


onboardingInstrumentStatusGetH :: Projects.ProjectId -> Maybe OI.RunId -> ATAuthCtx (RespHeaders InstrumentStatusHtml)
onboardingInstrumentStatusGetH pid runIdM = do
  void $ Projects.sessionAndProject pid
  runM <- maybe (OI.latestRunForProject pid) (OI.getRun pid) runIdM
  addRespHeaders $ InstrumentStatusHtml pid runM


onboardingFinishPostH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Done OnboardingRedirect))
onboardingFinishPostH pid = do
  void $ Projects.sessionAndProject pid
  markStepCompleted pid (show Alerts)
  done $ OnboardingRedirect (stepUrl pid Pricing)


onboardingAlertsPostH :: Projects.ProjectId -> NotifChannelForm -> ATAuthCtx (RespHeaders (Done OnboardingRedirect))
onboardingAlertsPostH pid form = do
  void $ ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit pid
  let cleanEmails = V.fromList $ ordNub $ filter (not . T.null) $ map T.strip form.emails
      cleanPhone = T.strip form.phoneNumber
  let savePhone = unless (T.null cleanPhone) $ case Notify.parsePhoneE164 cleanPhone of
        Just phone -> ProjectMembers.setEveryoneTeamPhones pid (V.fromList [phone])
        Nothing -> addErrorToast "Phone number not saved: use the international format with a country code, e.g. +2348012345678" Nothing
  void
    $ concurrently
      (concurrently (unless (null cleanEmails) $ ProjectMembers.setEveryoneTeamEmails pid cleanEmails) savePhone)
      (markStepCompleted pid (show Alerts))
  done $ OnboardingRedirect (stepUrl pid Pricing)


-- Forms & Outcomes ---------------------------------------------------------------------

data InstrumentTab = TabAI | TabGitHub | TabManual | TabCLI
  deriving stock (Eq, Generic, Show)


data QuickStartForm = QuickStartForm
  { projectName :: Text
  , location :: Text
  , next :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data DiscoverUrlForm = DiscoverUrlForm
  { url :: Text
  , auto :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype CreateUptimesForm = CreateUptimesForm
  { selected :: [Text]
  }
  deriving stock (Generic, Show)


instance FromForm CreateUptimesForm where
  fromForm f = Right $ CreateUptimesForm $ fromRight [] $ parseAll "selected" f


data StartInstrumentForm = StartInstrumentForm
  { repoFullName :: Text
  , branch :: Maybe Text
  , targetKind :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype OnboardingRedirect = OnboardingRedirect Text
  deriving stock (Eq, Show)


instance Outcome OnboardingRedirect where
  outcomeRedirect (OnboardingRedirect u) = Just u


data DiscoveredEndpointsHtml = DiscoveredEndpointsHtml
  { discPid :: Projects.ProjectId
  , discResult :: Either Text Text
  , discUrl :: Text
  , discSuggestions :: [OI.SuggestedUrl]
  , discAuto :: Bool
  }
  deriving stock (Show)


instance ToHtml DiscoveredEndpointsHtml where
  toHtml = toHtml . renderDiscoveredEndpoints
  toHtmlRaw = toHtml


data CreatedUptimesHtml = CreatedUptimesHtml
  { createdPid :: Projects.ProjectId
  , createdChecks :: [(Text, Text)]
  , userEmail :: Text
  , createdStatusPage :: Maybe Uptime.StatusPage
  , createdHost :: Text
  }
  deriving stock (Show)


instance ToHtml CreatedUptimesHtml where
  toHtml = toHtml . renderCreatedUptimes
  toHtmlRaw = toHtml


data InstrumentStatusHtml = InstrumentStatusHtml
  { statusPid :: Projects.ProjectId
  , statusRun :: Maybe OI.InstrumentRun
  }
  deriving stock (Show)


instance ToHtml InstrumentStatusHtml where
  toHtml (InstrumentStatusHtml pid runM) = toHtml $ renderAgentProgress_ pid runM
  toHtmlRaw = toHtml


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
  = QuickStartStep
      { stepPid :: Projects.ProjectId
      , projectName :: Text
      , location :: Text
      , uptimeCount :: Int
      , suggestedUrl :: Maybe Text
      }
  | InstrumentHubStep
      { stepPid :: Projects.ProjectId
      , projectTitle :: Text
      , apiKey :: Text
      , hostUrl :: Text
      , initialTab :: InstrumentTab
      , hasGitHub :: Bool
      , repositories :: [GitSync.Repository]
      , latestRun :: Maybe OI.InstrumentRun
      }
  | AlertsStep
      { stepPid :: Projects.ProjectId
      , projectTitle :: Text
      , userEmail :: Text
      , alertsPhone :: Text
      , alertsEmails :: V.Vector Text
      , alertsEnableFreetier :: Bool
      , slackUrl :: Text
      , discordUrl :: Text
      , hasSlack :: Bool
      , hasDiscord :: Bool
      }
  | InfoStep
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
      , trialEligible :: Bool
      }
  | CompleteStep
      { stepPid :: Projects.ProjectId
      }
  deriving stock (Generic, Show)


newtype OnboardingGet = OnboardingGet (PageCtx OnboardingStepData)
  deriving stock (Generic, Show)


instance ToHtml OnboardingGet where
  toHtml (OnboardingGet (PageCtx bwconf stepData)) = toHtml $ PageCtx bwconf $ renderStep stepData
    where
      renderStep = \case
        QuickStartStep{..} -> step1ProjectUptimePage stepPid projectName location uptimeCount suggestedUrl
        InstrumentHubStep{..} -> step2InstrumentPage stepPid projectTitle apiKey hostUrl initialTab hasGitHub repositories latestRun
        AlertsStep{..} -> step3AlertsPage stepPid projectTitle alertsPhone alertsEmails alertsEnableFreetier slackUrl discordUrl hasSlack hasDiscord
        CompleteStep{..} -> onboardingCompleteBody stepPid
        SurveyStep{..} -> onboardingConfigBody stepPid location functionality
        NotifChannelStep{..} -> notifChannelsWithUrls slackUrl discordUrl stepPid phoneNumber emails hasDiscord hasSlack
        IntegrationStep{..} -> integrationsPage stepPid apiKey
        PricingStep{..} -> pricingPage stepPid lemonUrl criticalUrl paymentPlan enableFreetier basicAuthEnabled trialEligible
        InfoStep{..} -> onboardingInfoBody stepPid firstName lastName companyName companySize foundUsFrom
  toHtmlRaw = toHtml


data OnboardingStepPost = OnboardingStepPost Projects.ProjectId OnboardingStep
  deriving stock (Eq, Show)


instance Outcome OnboardingStepPost where outcomeRedirect (OnboardingStepPost pid step) = Just $ stepUrl pid step


data ChecklistDismissed = ChecklistDismissed
  deriving stock (Eq, Show)
  deriving anyclass (Outcome)


data IntegrationCheck
  = AwaitingEvents Projects.ProjectId Text
  | Verified
  | ProceedToPricing Projects.ProjectId
  deriving stock (Eq, Show)


instance ToHtml IntegrationCheck where
  toHtml = \case
    AwaitingEvents pid language -> toHtml $ integrationPoller_ pid language
    Verified -> div_ [class_ "flex items-center gap-2 text-textSuccess"] do
      span_ "verified"
      faSprite_ "circle-check" "regular" "h-4 w-4"
    ProceedToPricing _ -> mempty
  toHtmlRaw = toHtml


instance Outcome IntegrationCheck where
  outcomeBody = toHtml
  outcomeRedirect = \case
    ProceedToPricing pid -> Just $ stepUrl pid Pricing
    AwaitingEvents{} -> Nothing
    Verified -> Nothing


data NoEventsYet = NoEventsYet
  deriving stock (Eq, Generic, Show)
  deriving (Refusal) via Says "No events found yet" NoEventsYet


newtype ProxiedPage = ProxiedPage Text


instance ToHtml ProxiedPage where
  toHtml (ProxiedPage t) = toHtmlRaw $ T.replace "href=\"/" "href=\"https://monoscopehq.com/" t
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


onboardingStepSkipped :: Projects.ProjectId -> Maybe OnboardingStep -> ATAuthCtx (RespHeaders (Done OnboardingStepPost))
onboardingStepSkipped pid stepM = do
  void $ Projects.sessionAndProject pid
  whenJust stepM $ markStepCompleted pid . show
  done $ OnboardingStepPost pid $ bool Info Pricing (stepM == Just Integration)


dismissChecklistH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Done ChecklistDismissed))
dismissChecklistH pid = do
  void $ Projects.sessionAndProject pid
  markStepCompleted pid "checklist_dismissed"
  done ChecklistDismissed


markStepCompleted :: Hasql.Hasql :> es => Projects.ProjectId -> Text -> Eff es ()
markStepCompleted pid = void . Projects.completeOnboardingStep pid


phoneEmailPostH :: Projects.ProjectId -> NotifChannelForm -> ATAuthCtx (RespHeaders OnboardingPhoneEmailsPost)
phoneEmailPostH pid form = do
  void $ ProjectMembers.sessionAndProjectAtLeastMember ProjectMembers.PEdit pid
  appCtx <- ask @AuthContext
  memberEmails <- map (CI.original . (.email)) <$> Projects.usersByProjectId pid
  markStepCompleted pid $ show NotifChannel
  ProjectMembers.setEveryoneTeamEmails pid (V.fromList form.emails)
  unless (T.null $ T.strip form.phoneNumber) $ case Notify.parsePhoneE164 form.phoneNumber of
    Just phone -> ProjectMembers.setEveryoneTeamPhones pid (V.fromList [phone])
    Nothing -> addErrorToast "Phone number not saved: use the international format with a country code, e.g. +2348012345678" Nothing
  addRespHeaders $ OnboardingPhoneEmailsPost pid (V.fromList $ ordNub $ form.emails <> memberEmails) appCtx.config.enableFreetier


checkIntegrationGet :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (ActionResult NoEventsYet IntegrationCheck)
checkIntegrationGet pid languageM = act do
  void $ Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  -- A project that ingested once needs no scan. Otherwise only recent rows answer: an unbounded
  -- LIMIT 1 planned every file the project ever wrote, ~78 GB per poll on prod (2026-11-25).
  seen <-
    Activation.reachedMilestone pid Activation.IngestVerified
      ||^ (isJust @Text <$> Hasql.withHasqlTimefusion appCtx.env.enableTimefusionReads (Hasql.interpOne [HI.sql|SELECT context___span_id FROM otel_logs_and_spans WHERE project_id = #{pid.toText} AND timestamp >= #{addUTCTime (-86400) now} LIMIT 1|]))
  if not seen
    then pure $ maybeToRight NoEventsYet $ AwaitingEvents pid <$> languageM
    else do
      markStepCompleted pid $ show Integration
      Activation.recordActivationMilestones Activation.IngestVerified Nothing [pid] >>= traverse_ \p -> do
        insertJob $ AdConversion $ Activated p
        addTriggerEvent "adConversion" $ AE.object ["name" AE..= ("activated" :: Text), "id" AE..= p]
      pure $ Right $ maybe (ProceedToPricing pid) (const Verified) languageM


onboardingInfoPostH :: Projects.ProjectId -> OnboardingInfoForm -> ATAuthCtx (RespHeaders (Done OnboardingStepPost))
onboardingInfoPostH pid form = do
  (sess, _) <- Projects.sessionAndProject pid
  let answers = HI.AsJsonb $ AE.object ["companyName" AE..= form.companyName, "companySize" AE..= form.companySize, "foundUsFrom" AE..= form.whereDidYouHearAboutUs]
  Hasql.interpExecute_ [HI.sql| update projects.projects set title=#{form.companyName}, questions = COALESCE(questions, '{}'::jsonb) || #{answers} where id=#{pid} |]
  markStepCompleted pid $ show Info
  Hasql.interpExecute_ [HI.sql| update users.users set first_name=#{form.firstName}, last_name=#{form.lastName} where id=#{sess.user.id} |]
  done $ OnboardingStepPost pid Survey


onboardingConfPostH :: Projects.ProjectId -> OnboardingConfForm -> ATAuthCtx (RespHeaders (Done OnboardingStepPost))
onboardingConfPostH pid form = do
  void $ Projects.sessionAndProject pid
  let answers = HI.AsJsonb $ AE.object ["functionality" AE..= form.functionality, "location" AE..= form.location]
  Hasql.interpExecute_ [HI.sql| update projects.projects set questions = COALESCE(questions, '{}'::jsonb) || #{answers} where id=#{pid} |]
  markStepCompleted pid $ show Survey
  done $ OnboardingStepPost pid NotifChannel


-- Unified 3-Step Onboarding Sequence (matching Monoscope's native onboarding style) -----

onboardingStepWrapperOf_ :: Int -> Int -> Text -> Text -> Html () -> Html ()
onboardingStepWrapperOf_ total step title prevUrl content =
  div_ [class_ "w-full max-w-xl mx-auto mt-20 md:mt-[156px] mb-10 px-4 md:px-0"]
    $ div_ [class_ "flex-col gap-4 flex w-full"] (stepIndicatorOf total step title prevUrl >> content)


-- | Step 1 of 4: Project settings + optional 10-second AI Uptime Discovery
step1ProjectUptimePage :: Projects.ProjectId -> Text -> Text -> Int -> Maybe Text -> Html ()
step1ProjectUptimePage pid suggestedName selectedLoc uptimeCount suggestedUrlM =
  onboardingStepWrapperOf_ 4 1 "Set up your project" "" do
    div_ [class_ "flex flex-col gap-8 mt-4"] do
      form_
        [ id_ "step1-project-form"
        , class_ "flex flex-col gap-6"
        , hxPost_ $ urls.projects.onboardingQuickStartPost pid
        , hxSwap_ "none"
        , hxIndicator_ "#loadingIndicator, #step1-proceed-spin"
        ]
        do
          formField_ FieldMd def{value = suggestedName} "Project name" "projectName" True Nothing
          formField_ FieldMd def "Where should your project be hosted?" "location" True
            $ Just
            $ div_ [class_ "pt-2 flex flex-wrap gap-6"] do
              forM_ ([("usa", "USA"), ("eu", "EU"), ("asia", "Asia")] :: [(Text, Text)]) \(val, lbl) ->
                div_ [class_ "inline-flex items-center gap-2.5"] do
                  input_ $ [class_ "radio radio-primary radio-sm", type_ "radio", name_ "location", value_ val, id_ $ "loc-" <> val, required_ "required"] <> [checked_ | val == selectedLoc]
                  label_ [class_ "text-textStrong text-sm cursor-pointer", Lucid.for_ $ "loc-" <> val] $ toHtml lbl

      -- Optional 10-second AI Uptime Discovery (auto-detects domain & app/api subdomains from work email)
      section_ [class_ "p-4 bg-fillWeak border border-strokeWeak rounded-xl space-y-3", Aria.label_ "Instant uptime monitoring"] do
        div_ [class_ "flex flex-wrap items-center justify-between gap-2"] do
          div_ do
            div_ [class_ "flex flex-wrap items-center gap-2"] do
              h2_ [class_ "text-sm font-medium text-textStrong"] "Monitor a live URL"
              span_ [class_ "text-xs text-textWeak"] "(optional)"
              when (isJust suggestedUrlM && uptimeCount == 0)
                $ span_ [class_ "rounded-full bg-fillBrand-weak px-2 py-0.5 text-[11px] font-medium text-textBrand"] "Detected from email"
            p_ [class_ "text-xs text-textWeak mt-0.5"] "Checks your domain, app/api subdomains, and health endpoints automatically."
          when (uptimeCount > 0) $ tonePill_ SuccessBadge $ show uptimeCount <> " active"

        form_
          [ class_ "flex flex-col gap-2 sm:flex-row"
          , hxPost_ $ urls.projects.onboardingDiscoverUrlsPost pid
          , hxTarget_ "#url-discovery-results"
          , hxSwap_ "innerHTML"
          , hxIndicator_ "#url-discover-spinner"
          ]
          do
            label_ [Lucid.for_ "monitor-url", class_ "sr-only"] "URL or domain to monitor"
            monitorUrlInput_ False (fromMaybe "" suggestedUrlM)
            button_ [type_ "submit", class_ "btn btn-sm h-10 shrink-0 rounded-lg border border-strokeWeak bg-bgBase text-sm font-medium text-textStrong hover:bg-bgRaised gap-2 cursor-pointer active:scale-[0.96] transition-transform"] do
              span_ [id_ "url-discover-spinner", class_ "htmx-indicator"] $ faSprite_ "spinner" "regular" "size-3.5 animate-spin"
              span_ "Discover endpoints"

        div_
          ( [id_ "url-discovery-results", class_ "space-y-3"]
              <> case (uptimeCount, suggestedUrlM) of
                (0, Just u) ->
                  [ hxPost_ $ urls.projects.onboardingDiscoverUrlsPost pid
                  , hxVals_ $ decodeUtf8 $ AE.encode $ AE.object ["url" AE..= u, "auto" AE..= ("1" :: Text)]
                  , hxTrigger_ "load"
                  , hxSwap_ "innerHTML"
                  , hxIndicator_ "#url-discover-spinner"
                  ]
                _ -> []
          )
          pass

      div_ [class_ "flex items-center gap-4"] do
        button_
          [ type_ "submit"
          , term "form" "step1-project-form"
          , name_ "next"
          , value_ "instrument"
          , term "hx-disabled-elt" "this"
          , class_ "btn-primary ps-6 pe-5 py-3 text-base rounded-xl cursor-pointer inline-flex items-center gap-2 active:scale-[0.96] transition-transform disabled:opacity-70"
          ]
          do
            span_ "Proceed"
            span_ [id_ "step1-proceed-spin", class_ "htmx-indicator"] $ faSprite_ "spinner" "regular" "size-3.5 animate-spin"
            faSprite_ "arrow-right" "regular" "size-3.5"
        button_
          [ type_ "button"
          , class_ "px-3 py-2.5 inline-flex items-center underline text-textBrand text-base cursor-pointer disabled:opacity-60"
          , hxPost_ $ urls.projects.onboardingFinishPost pid
          , hxSwap_ "none"
          , hxIndicator_ "#loadingIndicator"
          , term "hx-disabled-elt" "this"
          ]
          "Skip to plan"


monitorUrlInput_ :: Bool -> Text -> Html ()
monitorUrlInput_ isOob val =
  input_
    $ [ type_ "text"
      , term "inputmode" "url"
      , id_ "monitor-url"
      , name_ "url"
      , value_ val
      , required_ "required"
      , placeholder_ "https://api.acme.com or acme.com"
      , class_ "input input-sm h-10 w-full min-w-0 flex-1 bg-bgBase border-strokeWeak text-sm"
      ]
    <> [hxSwapOob_ "outerHTML" | isOob]


renderDiscoveredEndpoints :: DiscoveredEndpointsHtml -> Html ()
renderDiscoveredEndpoints DiscoveredEndpointsHtml{..} = case discResult of
  Left _
    | discAuto ->
        -- Silent fallback on auto-detect: clear input so user can type any URL without a scary error banner
        monitorUrlInput_ True ""
  Left errMsg ->
    div_ [role_ "alert", class_ "rounded-xl border border-strokeError-weak bg-fillError-weak px-4 py-3 flex items-start gap-3 text-sm"] do
      faSprite_ "circle-exclamation" "solid" "mt-0.5 size-4 shrink-0 text-textError"
      p_ [class_ "text-textError"] $ toHtml errMsg
  Right _ -> do
    -- When auto-detected, update #monitor-url to the top valid endpoint (e.g. app.x.com or api.x.com)
    when discAuto
      $ whenJust (find (.recommended) discSuggestions <|> listToMaybe discSuggestions) \best ->
        monitorUrlInput_ True best.url
    div_ [class_ "rounded-xl bg-bgRaised ring-1 ring-strokeWeak/70 p-4 space-y-3", role_ "status", Aria.live_ "polite"] do
      div_ [class_ "flex items-center justify-between gap-2"] do
        span_ [class_ "text-sm font-medium text-textStrong"]
          $ if discAuto then "Detected live endpoints" else "Recommended endpoints"
        tonePill_ SuccessBadge $ show (length discSuggestions) <> " reachable"
      renderQuickUptimeForm_ discPid discSuggestions


renderQuickUptimeForm_ :: Projects.ProjectId -> [OI.SuggestedUrl] -> Html ()
renderQuickUptimeForm_ pid suggestions =
  form_
    [ class_ "space-y-3"
    , hxPost_ $ urls.projects.onboardingCreateUptimesPost pid
    , hxTarget_ "#url-discovery-results"
    , hxSwap_ "innerHTML"
    ]
    do
      div_ [class_ "divide-y divide-strokeWeak/60 rounded-lg border border-strokeWeak/70 bg-bgBase overflow-hidden"] do
        forM_ suggestions \s ->
          label_ [class_ "flex cursor-pointer items-start gap-3 px-3.5 py-2.5 hover:bg-fillWeak/50 transition-colors"] do
            input_
              $ [ type_ "checkbox"
                , name_ "selected"
                , value_ $ s.url <> "||" <> s.name
                , class_ "checkbox checkbox-sm checkbox-primary mt-0.5 shrink-0"
                ]
              <> [checked_ | s.recommended]
            div_ [class_ "min-w-0 flex-1"] do
              div_ [class_ "flex flex-wrap items-center gap-2"] do
                span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml s.name
                whenJust s.statusCode \code ->
                  span_ [class_ "rounded-md bg-fillSuccess-weak px-1.5 py-0.5 text-xs font-medium tabular-nums text-textSuccess"]
                    $ toHtml
                    $ show code
                    <> maybe "" (\ms -> " · " <> show ms <> " ms") s.latencyMs
                span_ [class_ "text-xs text-textWeak"] $ toHtml s.reason
              p_ [class_ "truncate font-mono text-xs text-textWeak mt-0.5"] $ toHtml s.url
      button_ [type_ "submit", class_ "btn btn-sm rounded-lg border border-strokeWeak bg-bgBase text-textStrong hover:bg-fillWeak active:scale-[0.96] cursor-pointer"] "Enable selected checks"


renderCreatedUptimes :: CreatedUptimesHtml -> Html ()
renderCreatedUptimes CreatedUptimesHtml{..} =
  div_ [class_ "rounded-xl border border-strokeSuccess-weak bg-fillSuccess-weak px-4 py-3.5 space-y-3", role_ "status", Aria.live_ "polite"] do
    div_ [class_ "flex items-center gap-2"] do
      faSprite_ "circle-check" "solid" "size-4 text-textSuccess shrink-0"
      span_ [class_ "text-sm font-medium text-textStrong"]
        $ toHtml
        $ show (length createdChecks)
        <> " uptime "
        <> bool "checks" "check" (length createdChecks == 1)
        <> " active (every 60s)"
    ul_ [class_ "space-y-1 text-xs text-textWeak"] do
      forM_ createdChecks \(name, u) ->
        li_ [class_ "flex flex-wrap items-center gap-2"] do
          span_ [class_ "font-medium text-textStrong"] $ toHtml name
          span_ [class_ "font-mono break-all"] $ toHtml u
    whenJust createdStatusPage (UptimePage.statusPageVisibilityCard_ createdPid createdHost)


-- | Step 2 of 3: Split-Screen Instrumentation Hub (maintaining Monoscope's native 50/50 onboarding style)
-- AI Agent Prompt, GitHub Auto-PR, Manual SDK & Infra docs, and CLI Quick Test live seamlessly
-- in the same split-screen layout with pure-CSS Tier-1 switching.
step2InstrumentPage
  :: Projects.ProjectId
  -> Text
  -> Text
  -> Text
  -> InstrumentTab
  -> Bool
  -> [GitSync.Repository]
  -> Maybe OI.InstrumentRun
  -> Html ()
step2InstrumentPage pid _title apiKey host initialTab hasGitHub repos latestRunM = do
  div_ [class_ "w-full flex flex-col md:flex-row min-h-screen md:h-screen md:overflow-hidden group/pg group/hub"] do
    -- Left half: Monoscope native bg-bgRaised selection rail
    div_ [class_ "w-full md:w-1/2 bg-bgRaised md:h-full flex flex-col"] do
      div_ [class_ "pt-14 md:pt-[156px] px-4 md:px-12 flex-shrink-0"]
        $ div_ [class_ "max-w-xl"] (stepIndicatorOf 4 2 "Instrument your apps or servers" (stepUrl pid QuickStart))

      div_ [class_ "flex-col w-full gap-3 md:gap-4 flex mt-3 md:mt-4 px-4 md:px-12 overflow-y-auto flex-grow pb-20 md:pb-6"] do
        p_
          [class_ "text-textWeak leading-relaxed text-sm md:text-base"]
          "Use an AI coding agent, connect GitHub for an automated PR, or select your stack below."

        div_ [class_ "mt-2 mb-0 p-3 md:p-4 bg-fillWeak border border-strokeWeak rounded-xl"] do
          div_ [class_ "mb-1.5 md:mb-2 text-textWeak text-sm"] "Your API Key"
          copyField_ "api-key-display" apiKey

        div_ [class_ "mb-2 px-4 py-2.5 bg-gradient-to-r from-fillInformation-weak to-transparent border border-strokeInformation-weak rounded-lg"] do
          p_ [class_ "text-sm text-textStrong"] do
            "Want to test quickly? "
            dialogTrigger_
              "telemetrygen-modal"
              [class_ "text-textBrand hover:text-textStrong underline font-medium cursor-pointer"]
              "Send a test event"
            " with the CLI"

        -- AI-Assisted Setup section (exact same tile style as Applications / Infrastructure / Databases)
        div_ [class_ "mb-3 md:mb-4"] do
          div_ [class_ "text-textWeak text-lg md:text-xl mb-2"] "AI & Automated Setup"
          div_ [class_ "grid grid-cols-2 gap-2"] do
            aiOptionTile_ (initialTab == TabAI || initialTab == TabCLI) "inst-tab-ai" "sparkles" "regular" "AI Agent"
            aiOptionTile_ (initialTab == TabGitHub) "inst-tab-github" "github" "solid" "GitHub PR"

        -- Manual SDK, Infrastructure & Database groups (exact same tiles as master)
        forM_ integrationGroups \grp -> div_ [class_ "mb-3 md:mb-4"] do
          div_ [class_ "text-textWeak text-lg md:text-xl mb-2"] $ toHtml grp.name
          div_ [class_ "grid grid-cols-2 gap-2"] do
            forM_ grp.languages \l ->
              label_
                [ class_ "group/li h-12 px-3 py-2 bg-transparent rounded-xl border border-strokeWeak has-[:checked]:border-strokeBrand-strong has-[:checked]:bg-fillBrand-weak justify-start items-center gap-3 inline-flex cursor-pointer transition-colors"
                ]
                do
                  input_
                    [ type_ "radio"
                    , name_ "inst-tab"
                    , class_ "radio radio-sm radio-primary shrink-0"
                    , id_ $ "m-lang-" <> l.slug
                    , onchange_ "if(this.checked && window.innerWidth<768){var dp=document.getElementById('docs-panel');dp.classList.add('open');requestAnimationFrame(function(){dp.scrollTop=0});}"
                    ]
                  div_ [class_ "flex w-full items-center justify-between overflow-hidden"] do
                    div_ [class_ "flex items-center gap-2 text-sm min-w-0"] do
                      img_ [class_ "h-5 w-5 shrink-0", alt_ "", src_ $ "/public/assets/svgs/" <> l.slug <> ".svg"]
                      span_ [class_ "truncate text-textStrong"] $ toHtml l.label
                    div_ [class_ "hidden group-has-[:checked]/li:block text-sm"]
                      $ integrationPoller_ pid (T.replace "#" "sharp" l.label)

        step2Cta_ False
      step2Cta_ True

    -- Right half: Monoscope native bg-bgBase full-height panel
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

      div_ [class_ "w-full md:h-full overflow-y-auto bg-bgBase"] do
        -- ── PANEL 1: AI Agent Prompt (Claude Code / Cursor / Codex) ──
        section_ [id_ "panel-ai", class_ "hidden group-has-[#inst-tab-ai:checked]/pg:block max-w-2xl mx-auto space-y-6 px-6 py-8 md:px-8 lg:px-10 md:pt-[156px] md:pb-12"] do
          div_ [class_ "flex items-center gap-3.5"] do
            div_ [class_ "flex items-center -space-x-2 shrink-0", Aria.hidden_ "true"] do
              forM_ (["brand-claude", "brand-cursor", "brand-openai"] :: [Text]) \b ->
                span_ [class_ "inline-flex size-9 items-center justify-center rounded-full bg-bgRaised border border-strokeWeak ring-2 ring-bgBase text-textStrong"]
                  $ faSprite_ b "solid" "size-4"
            div_ [class_ "min-w-0"] do
              h2_ [class_ "text-lg font-semibold text-textStrong"] "Instrument with an AI coding agent"
              p_ [class_ "text-xs text-textWeak mt-0.5"] "Works in Claude Code, Cursor, and Codex — installs OTel and verifies a live span."

          -- Segmented target selector with comfortable breathing room (wraps cleanly if narrow, never clips)
          div_ [class_ "flex flex-wrap items-center rounded-xl border border-strokeWeak bg-bgRaised p-1.5 gap-1", id_ "ai-target-pills", role_ "tablist"] do
            forM_ (zip [0 :: Int ..] aiTargets) \(idx, t) ->
              label_
                [ data_ "target" t.slug
                , class_ "ai-target-pill inline-flex cursor-pointer items-center justify-center gap-1.5 whitespace-nowrap rounded-lg px-3 py-1.5 text-xs font-medium text-textWeak hover:text-textStrong has-[:checked]:bg-bgBase has-[:checked]:text-textStrong has-[:checked]:shadow-xs has-[:checked]:ring-1 has-[:checked]:ring-strokeWeak transition-colors"
                ]
                do
                  input_ $ [type_ "radio", name_ "ai-target", id_ $ "ai-t-" <> t.slug, class_ "sr-only"] <> [checked_ | idx == 0]
                  faSprite_ t.icon "regular" "size-3.5 shrink-0"
                  span_ $ toHtml t.label

          -- Step 1: One-line skills install
          div_ [class_ "rounded-xl border border-strokeWeak bg-bgRaised p-5 space-y-2.5"] do
            div_ [class_ "flex items-center justify-between gap-2"] do
              div_ [class_ "flex items-center gap-2"] do
                span_ [class_ "inline-flex size-5 items-center justify-center rounded-full bg-fillBrand-weak text-[11px] font-semibold tabular-nums text-textBrand"] "1"
                span_ [class_ "text-xs font-medium text-textStrong"] "Add Monoscope skills"
              span_ [class_ "text-[11px] text-textWeak"] "Optional · improves accuracy"
            copyField_ "ai-skills-cmd" "npx skills add monoscope-tech/skills"

          -- Step 2: Copy & paste tailored prompt
          forM_ aiTargets \t ->
            div_ [id_ $ "ai-target-" <> t.slug, class_ $ "hidden space-y-4 " <> t.showClass] do
              div_ [class_ "rounded-xl border border-strokeWeak bg-bgRaised overflow-hidden"] do
                div_ [class_ "flex items-center justify-between gap-3 px-5 py-3.5 border-b border-strokeWeak bg-fillWeak/40"] do
                  div_ [class_ "flex items-center gap-2 min-w-0"] do
                    span_ [class_ "inline-flex size-5 shrink-0 items-center justify-center rounded-full bg-fillBrand-weak text-[11px] font-semibold tabular-nums text-textBrand"] "2"
                    span_ [class_ "text-xs font-medium text-textStrong truncate"] $ toHtml $ "Paste prompt · " <> t.label
                  copyButton_ "btn btn-xs btn-primary rounded-md gap-1.5 font-medium shrink-0 active:scale-[0.96] transition-transform" "size-3" ("#prompt-src-" <> t.slug <> ".textContent") []

                pre_ [id_ $ "prompt-src-" <> t.slug, class_ "max-h-64 overflow-y-auto p-5 font-mono text-xs leading-relaxed text-textStrong/90 bg-bgBase/40 whitespace-pre-wrap break-words select-all"]
                  $ toHtml (t.prompt pid host apiKey)

                div_ [class_ "px-5 py-4 border-t border-strokeWeak bg-bgRaised space-y-2"] do
                  p_ [class_ "text-[11px] font-medium uppercase tracking-wider text-textWeak"] "What your agent will do"
                  ul_ [class_ "space-y-1.5"] do
                    forM_ t.steps \(stepTitle, _) ->
                      li_ [class_ "flex items-center gap-2 text-xs text-textWeak"] do
                        faSprite_ "circle-check" "regular" "size-3.5 shrink-0 text-textSuccess"
                        span_ $ toHtml stepTitle

              whenJust t.quickCmd \qc ->
                div_ [class_ "rounded-xl border border-strokeWeak bg-bgRaised p-5 space-y-2.5"] do
                  div_ [class_ "flex items-center justify-between gap-2"] do
                    span_ [class_ "text-xs font-medium text-textWeak"] $ toHtml qc.label
                    copyButton_ "btn btn-xs border border-strokeWeak bg-bgBase text-textStrong hover:bg-fillWeak gap-1" "size-3" ("#quick-cmd-" <> t.slug <> ".textContent") []
                  pre_ [id_ $ "quick-cmd-" <> t.slug, class_ "overflow-x-auto rounded-lg bg-bgBase border border-strokeWeak p-3.5 font-mono text-xs text-textStrong whitespace-pre"]
                    $ toHtml (qc.body host apiKey)

          -- Compact live ingestion listener
          div_ [class_ "flex items-center justify-between gap-3 rounded-xl border border-strokeWeak bg-bgRaised px-5 py-3.5", role_ "status", Aria.live_ "polite"] do
            div_ [class_ "flex items-center gap-2.5"] do
              span_ [class_ "relative flex size-2", Aria.hidden_ "true"] do
                span_ [class_ "absolute inline-flex h-full w-full animate-ping rounded-full bg-fillBrand-strong opacity-60"] mempty
                span_ [class_ "relative inline-flex size-2 rounded-full bg-fillBrand-strong"] mempty
              span_ [class_ "text-xs font-medium text-textStrong"] "Listening for your first span or log"
            integrationPoller_ pid "onboarding"

        -- ── PANEL 2: GitHub Auto-PR (Setup Agent / pi-durable) ──
        section_ [id_ "panel-github", class_ "hidden group-has-[#inst-tab-github:checked]/pg:block max-w-2xl mx-auto space-y-6 px-6 py-8 md:px-8 lg:px-10 md:pt-[156px] md:pb-12"] do
          div_ [class_ "flex items-center gap-3.5"] do
            span_ [class_ "inline-flex size-10 shrink-0 items-center justify-center rounded-xl bg-bgRaised border border-strokeWeak text-textStrong", Aria.hidden_ "true"]
              $ faSprite_ "github" "solid" "size-5"
            div_ [class_ "min-w-0"] do
              div_ [class_ "flex flex-wrap items-center gap-2"] do
                h2_ [class_ "text-lg font-semibold text-textStrong"] "Auto-instrument via Pull Request"
                span_ [class_ "rounded-full bg-fillBrand-weak px-2 py-0.5 text-[11px] font-medium text-textBrand"] "Verified live"
              p_ [class_ "text-xs text-textWeak mt-0.5"] "Instruments your repo in a sandbox, verifies a real span arrives, and opens a secret-free PR."

          if not hasGitHub
            then do
              div_ [class_ "rounded-xl border border-strokeWeak bg-bgRaised p-5 space-y-5"] do
                renderAgentPipelinePreview_
                div_ [class_ "flex flex-wrap items-center justify-between gap-3 pt-4 border-t border-strokeWeak"] do
                  a_ [href_ $ urls.githubAppInstall pid (Just "onboarding"), class_ "btn btn-primary rounded-lg text-sm font-medium gap-2"] do
                    faSprite_ "github" "solid" "size-4"
                    span_ "Connect GitHub"
                  span_ [class_ "text-xs text-textWeak"] "Secrets are never committed to Git"
            else do
              form_
                [ class_ "rounded-xl border border-strokeWeak bg-bgRaised p-4 space-y-3"
                , hxPost_ $ urls.projects.onboardingStartInstrumentPost pid
                , hxTarget_ "#setup-agent-panel"
                , hxSwap_ "innerHTML"
                ]
                do
                  div_ [class_ "flex items-center justify-between gap-2"] do
                    label_ [Lucid.for_ "repoFullName", class_ "text-xs font-medium text-textStrong"] "Target repository"
                    a_ [href_ $ urls.repositoryConnectGet pid Nothing, class_ "text-xs text-textBrand hover:underline"] "Manage repos →"
                  div_ [class_ "flex flex-col sm:flex-row gap-2"] do
                    if null repos
                      then
                        input_
                          [ type_ "text"
                          , id_ "repoFullName"
                          , name_ "repoFullName"
                          , required_ "required"
                          , placeholder_ "owner/repository"
                          , class_ "input input-sm h-10 w-full min-w-0 flex-1 bg-bgBase border-strokeWeak text-sm"
                          ]
                      else select_ [id_ "repoFullName", name_ "repoFullName", class_ "select select-sm h-10 w-full min-w-0 flex-1 bg-bgBase border-strokeWeak text-sm"] do
                        forM_ repos \r ->
                          let full = r.owner <> "/" <> r.repo
                           in option_ [value_ full] $ toHtml full
                    button_ [type_ "submit", class_ "btn btn-primary h-10 shrink-0 rounded-lg text-sm font-medium gap-2 cursor-pointer"] do
                      faSprite_ "sparkles" "regular" "size-3.5"
                      span_ "Start setup agent"

              div_ [id_ "setup-agent-panel"] $ renderAgentProgress_ pid latestRunM

              let envText = "OTEL_EXPORTER_OTLP_ENDPOINT=" <> host <> "/v1/traces\nMONOSCOPE_API_KEY=" <> apiKey
              div_ [class_ "rounded-xl border border-strokeWeak bg-bgRaised overflow-hidden"] do
                div_ [class_ "flex items-center justify-between gap-2 px-4 py-2.5 border-b border-strokeWeak bg-fillWeak/40"] do
                  div_ [class_ "flex items-center gap-2"] do
                    faSprite_ "key" "regular" "size-3.5 text-textWeak"
                    span_ [class_ "text-xs font-medium text-textStrong"] "Production environment variables"
                  copyButton_ "btn btn-xs border border-strokeWeak bg-bgBase text-textStrong hover:bg-fillWeak gap-1.5" "size-3" "#prod-env-vars.textContent" []
                div_ [class_ "p-4 space-y-2.5"] do
                  p_ [class_ "text-xs text-textWeak"] "Secrets are never committed to your repository. Add these to your hosting provider when merging the PR:"
                  pre_ [id_ "prod-env-vars", class_ "overflow-x-auto rounded-lg bg-bgBase border border-strokeWeak p-3 font-mono text-xs text-textStrong whitespace-pre"] $ toHtml envText

        -- ── PANEL 3: Manual SDK, Infrastructure & Database Docs (native master style) ──
        forM_ (concatMap (.languages) integrationGroups) \l ->
          div_ [class_ $ "hidden " <> manualShowClass l.slug, id_ $ "m-guide-" <> l.slug] do
            div_ [class_ "sticky top-0 z-10 bg-bgBase px-6 md:px-12 pt-4 pb-3 border-b border-strokeWeak"]
              $ div_ [class_ "inline-flex flex-wrap gap-1.5", role_ "tablist"]
              $ forM_ (zip [0 :: Int ..] l.frameworks) \(idx, fw) ->
                label_ [class_ "inline-flex cursor-pointer items-center gap-1.5 rounded-lg border border-strokeWeak bg-bgRaised px-3 py-1.5 text-xs font-medium text-textWeak hover:text-textStrong has-[:checked]:border-strokeBrand-strong has-[:checked]:bg-fillBrand-weak has-[:checked]:text-textBrand transition-colors"] do
                  input_
                    $ [ type_ "radio"
                      , name_ $ "m-fw-" <> l.slug
                      , class_ "sr-only"
                      , hxGet_ $ "/proxy/docs/sdks/" <> fw.docsPath
                      , hxTarget_ $ "#m-fw-content-" <> l.slug
                      , term "hx-sync" $ "#m-fw-content-" <> l.slug <> ":replace"
                      , hxTrigger_ "change"
                      , hxSwap_ "innerHTML"
                      , hxSelect_ "#mainArticle"
                      , hxIndicator_ $ "#m-fw-ind-" <> l.slug
                      ]
                    <> [checked_ | idx == 0]
                  unless (T.null fw.icon) $ img_ [class_ "size-4", alt_ "", loading_ "lazy", src_ $ "https://monoscopehq.com/assets/img/framework-logos/" <> fw.icon]
                  span_ $ toHtml fw.name
            div_ [class_ "px-6 md:px-12 py-6"] do
              div_ [id_ $ "m-fw-ind-" <> l.slug, class_ "htmx-indicator flex justify-center py-6"] $ loadingIndicator_ LdMD LdDots
              div_
                [ id_ $ "m-fw-content-" <> l.slug
                , hxGet_ $ "/proxy/docs/sdks/" <> foldMap (.docsPath) (listToMaybe l.frameworks)
                , hxTrigger_ "intersect once"
                , term "hx-sync" "this:replace"
                , hxSwap_ "innerHTML"
                , hxSelect_ "#mainArticle"
                , class_ "prose prose-sm prose-slate dark:prose-invert max-w-none prose-a:!text-textBrand prose-a:!underline"
                ]
                ""

  cliQuickTestModal_ "cli-install-cmd" ("curl -fsSL https://monoscopehq.com/install.sh | sh && monoscope auth login && monoscope config set project " <> pid.toText) "cli-send-cmd"

  -- Mobile docs-panel overlay + hide proxied marketing-site chrome
  style_
    "@media(max-width:767px){#docs-panel{display:none}#docs-panel.open{display:flex;flex-direction:column;position:fixed;inset:0;z-index:50;background:var(--color-bgBase);overflow-y:auto}#docs-panel.open #docs-panel-back{display:block}}#docs-panel .breadcrumbs,#docs-panel .ai-menu-wrap{display:none}"
  link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-dark.min.css")]
  let hljsDeps = loadDeps [HighlightJs]
  script_
    [text|
      (function() {
        var hash = (window.location.hash || '').replace(/^#/, '');
        if (hash === 'github') { var g = document.getElementById('inst-tab-github'); if (g) g.checked = true; }
        else { var radio = hash && document.getElementById('ai-t-' + hash); if (radio) radio.checked = true; }
        document.body.addEventListener('htmx:after:process', function(event) {
          var target = event.target.closest('[id^="m-fw-content-"]');
          if (target) { ${hljsDeps}.then(function() { target.querySelectorAll('pre code').forEach(function(b) { hljs.highlightElement(b); }); }); }
        });
      })();
    |]
  where
    aiOptionTile_ isChecked tid iconName iconStyle lbl =
      label_
        [ class_ "group/li h-12 px-3 py-2 bg-transparent rounded-xl border border-strokeWeak has-[:checked]:border-strokeBrand-strong has-[:checked]:bg-fillBrand-weak justify-start items-center gap-3 inline-flex cursor-pointer transition-colors"
        ]
        do
          input_
            $ [ type_ "radio"
              , name_ "inst-tab"
              , class_ "radio radio-sm radio-primary shrink-0"
              , id_ tid
              , onchange_ "if(this.checked && window.innerWidth<768){var dp=document.getElementById('docs-panel');dp.classList.add('open');requestAnimationFrame(function(){dp.scrollTop=0});}"
              ]
            <> [checked_ | isChecked]
          div_ [class_ "flex items-center gap-2 text-sm min-w-0"] do
            faSprite_ iconName iconStyle "size-4 shrink-0 text-textBrand"
            span_ [class_ "truncate text-textStrong font-medium"] $ toHtml lbl

    step2Cta_ isMobile =
      div_ [class_ $ bool "hidden md:flex items-center gap-2 mt-2" "flex md:hidden items-center justify-between gap-2 fixed bottom-0 left-0 right-0 z-10 bg-bgRaised border-t border-strokeWeak px-4 py-3" isMobile] do
        a_ [href_ $ stepUrl pid Alerts, class_ $ bool "btn-primary px-6 py-3 text-base rounded-xl cursor-pointer inline-flex items-center gap-2" "btn-primary px-5 py-2.5 text-sm rounded-xl cursor-pointer inline-flex items-center gap-2" isMobile] do
          span_ "Proceed"
          faSprite_ "arrow-right" "regular" "size-3.5"
        a_ [href_ $ stepUrl pid Alerts, class_ "px-3 py-2.5 inline-flex items-center underline text-textBrand text-base cursor-pointer"] "Skip"

    manualShowClass = \case
      "js" -> "group-has-[#m-lang-js:checked]/pg:block"
      "go" -> "group-has-[#m-lang-go:checked]/pg:block"
      "py" -> "group-has-[#m-lang-py:checked]/pg:block"
      "elixir" -> "group-has-[#m-lang-elixir:checked]/pg:block"
      "php" -> "group-has-[#m-lang-php:checked]/pg:block"
      "java" -> "group-has-[#m-lang-java:checked]/pg:block"
      "cs" -> "group-has-[#m-lang-cs:checked]/pg:block"
      "linux" -> "group-has-[#m-lang-linux:checked]/pg:block"
      "docker" -> "group-has-[#m-lang-docker:checked]/pg:block"
      "kubernetes" -> "group-has-[#m-lang-kubernetes:checked]/pg:block"
      "kafka" -> "group-has-[#m-lang-kafka:checked]/pg:block"
      "postgresql" -> "group-has-[#m-lang-postgresql:checked]/pg:block"
      "mongodb" -> "group-has-[#m-lang-mongodb:checked]/pg:block"
      "mysql" -> "group-has-[#m-lang-mysql:checked]/pg:block"
      _ -> ""


-- | Step 3 of 4: Alerts & Continue to Plan Picker (native style + better-layout fixes)
step3AlertsPage :: Projects.ProjectId -> Text -> Text -> V.Vector Text -> Bool -> Text -> Text -> Bool -> Bool -> Html ()
step3AlertsPage pid _title phone notifyEmails enableFreetier slackUrl discordUrl hasSlack hasDiscord =
  onboardingStepWrapperOf_ 4 3 "How should we notify you about issues?" (stepUrl pid Instrument) do
    div_ [class_ "flex flex-col w-full gap-8 mt-4"] do
      -- Group 1: Real-time chat channels (12px intra-group gap, aligned px-4 py-3 edges)
      div_ [class_ "flex flex-col gap-3"] do
        p_ [class_ "text-sm text-textWeak"] "Connect Slack or Discord for real-time incident alerts and automated PR updates."
        div_ [class_ "w-full gap-3 grid grid-cols-1 md:grid-cols-2"] do
          compactChannelCard_ "Slack" "/public/assets/svgs/slack.svg" hasSlack slackUrl
          compactChannelCard_ "Discord" "/public/assets/svgs/discord.svg" hasDiscord discordUrl

      -- Group 2: Editable notification emails + optional SMS disclosure
      form_
        [ id_ "step3-alerts-form"
        , class_ "flex flex-col gap-6"
        , hxPost_ $ urls.projects.onboardingAlertsPost pid
        , hxExt_ "json-enc"
        , hxVals_ "js:{phoneNumber: (document.getElementById('phoneNumber') || {}).value || '', emails: window.getTagValues('#emails')}"
        , hxSwap_ "none"
        , hxIndicator_ "#loadingIndicator, #step3-proceed-spin"
        ]
        do
          formField_ FieldMd def "Notification emails" "emails" False
            $ Just
            $ tagInput_ "emails" "" [data_ "tagify-initial" $ decodeUtf8 $ AE.encode notifyEmails]

          details_ ([class_ "group/sms"] <> [open_ "" | not (T.null phone)]) do
            summary_ [class_ "inline-flex items-center gap-2 text-sm font-medium text-textBrand hover:text-textStrong cursor-pointer select-none list-none"] do
              faSprite_ "chevron-right" "regular" "size-3 transition-transform duration-150 ease-out group-open/sms:rotate-90"
              span_ "Add phone number for SMS alerts"
            div_ [class_ "pt-3"]
              $ formField_ FieldMd def{inputType = "tel", value = phone, placeholder = "+1 555 123 4567"} "Phone number" "phoneNumber" False Nothing

      -- Group 3: Progressive disclosure for inviting teammates (inline, never a surprise modal)
      details_ [class_ "group/inv"] do
        summary_ [class_ "inline-flex items-center gap-2 text-sm font-medium text-textBrand hover:text-textStrong cursor-pointer select-none list-none"] do
          faSprite_ "chevron-right" "regular" "size-3 transition-transform duration-150 ease-out group-open/inv:rotate-90"
          span_ "Invite teammates"
        div_ [class_ "pt-3 space-y-3"] do
          when enableFreetier
            $ callout_ BrandBadge Nothing "Only the project owner can access a Free project. Invited members remain disabled until you upgrade."
          form_
            [ class_ "flex flex-col sm:flex-row items-stretch sm:items-center gap-2"
            , hxPost_ $ urls.projects.membersManagePost pid Nothing
            , hxSwap_ "none"
            , hxIndicator_ "#loadingIndicator"
            , [__|on htmx:afterRequest if event.detail.successful set (first <input[name='emails']/> in me).value to '' then remove .hidden from #invite-sent-msg|]
            ]
            do
              input_ [type_ "email", name_ "emails", required_ "required", placeholder_ "teammate@company.com", class_ "input input-sm h-10 flex-1 min-w-0 bg-bgRaised border-strokeWeak text-sm"]
              select_ [name_ "permissions", class_ "select select-sm h-10 w-full sm:w-28 shrink-0 bg-bgRaised border-strokeWeak text-sm"] $ roleOptions_ Nothing
              button_ [type_ "submit", class_ "btn btn-sm h-10 shrink-0 rounded-lg border border-strokeWeak bg-bgRaised text-textStrong hover:bg-fillWeak px-4 cursor-pointer active:scale-[0.96] transition-transform"] "Send invite"
          p_ [id_ "invite-sent-msg", class_ "hidden text-xs text-textSuccess"] "Invitation sent — you can add another teammate or continue below."

      -- Group 4: Primary CTA + quiet skip (32px inter-group separation)
      div_ [class_ "flex items-center gap-4"] do
        button_
          [ type_ "submit"
          , term "form" "step3-alerts-form"
          , term "hx-disabled-elt" "this"
          , class_ "btn-primary ps-8 pe-6 py-3 text-base rounded-xl cursor-pointer inline-flex items-center gap-2 active:scale-[0.96] transition-transform disabled:opacity-70"
          ]
          do
            span_ "Proceed"
            span_ [id_ "step3-proceed-spin", class_ "htmx-indicator"] $ faSprite_ "spinner" "regular" "size-3.5 animate-spin"
            faSprite_ "arrow-right" "regular" "size-3.5"
        button_
          [ type_ "button"
          , class_ "px-3 py-2.5 inline-flex items-center underline text-textBrand text-base cursor-pointer disabled:opacity-60"
          , hxPost_ $ urls.projects.onboardingFinishPost pid
          , hxSwap_ "none"
          , hxIndicator_ "#loadingIndicator"
          , term "hx-disabled-elt" "this"
          ]
          "Skip"


renderAgentPipelinePreview_ :: Html ()
renderAgentPipelinePreview_ =
  div_ [class_ "space-y-3"] do
    p_ [class_ "text-[11px] font-medium uppercase tracking-wider text-textWeak"] "How the setup agent works"
    ol_ [class_ "space-y-3"] do
      forM_
        ( [ ("1", "Clone & detect stack", "Identifies language, framework, and package manager automatically")
          , ("2", "Add OpenTelemetry SDK", "Configures OTLP/HTTP export reading MONOSCOPE_API_KEY from env")
          , ("3", "Verify live test span", "Runs an isolated test request and confirms telemetry arrives here")
          , ("4", "Open Pull Request", "Pushes a clean, secret-free branch ready for review")
          ]
            :: [(Text, Text, Text)]
        )
        \(n, ttl, sub) ->
          li_ [class_ "flex items-start gap-3"] do
            span_ [class_ "mt-0.5 flex size-5 shrink-0 items-center justify-center rounded-full border border-strokeWeak bg-bgBase text-[11px] font-semibold tabular-nums text-textWeak"] $ toHtml n
            div_ [class_ "min-w-0"] do
              p_ [class_ "text-xs font-medium text-textStrong"] $ toHtml ttl
              p_ [class_ "text-xs text-textWeak mt-0.5"] $ toHtml sub


renderAgentProgress_ :: Projects.ProjectId -> Maybe OI.InstrumentRun -> Html ()
renderAgentProgress_ pid runM =
  div_
    ( [class_ "rounded-xl border border-strokeWeak bg-bgRaised p-5 space-y-5", role_ "status", Aria.live_ "polite"]
        <> case runM of
          Just r
            | not (OI.isTerminal r.status) ->
                [ hxGet_ $ urls.projects.onboardingInstrumentStatusGet pid (Just r.id)
                , hxTrigger_ "every 2s"
                , hxSwap_ "outerHTML"
                ]
          _ -> []
    )
    do
      case runM of
        Nothing -> renderAgentPipelinePreview_
        Just r -> do
          div_ [class_ "flex flex-wrap items-center justify-between gap-2"] do
            div_ [class_ "min-w-0"] do
              h2_ [class_ "truncate text-base font-semibold text-textStrong"] $ toHtml $ "Setup agent · " <> r.repoFullName
              unless (T.null r.detectedStack)
                $ p_ [class_ "text-xs text-textWeak mt-0.5"]
                $ toHtml
                $ "Detected: "
                <> r.detectedStack
            case r.status of
              OI.RunCompleted -> tonePill_ SuccessBadge "Verified & PR opened"
              OI.RunFailed -> tonePill_ ErrorBadge "Needs attention"
              _ -> tonePill_ BrandBadge "Running durably"

          ul_ [class_ "space-y-2.5"] do
            forM_ (getAeson r.phases) \p ->
              li_ [class_ "flex items-start gap-2.5 text-sm"] do
                phaseIcon_ p.state
                div_ [class_ "min-w-0 flex-1"] do
                  span_ [class_ $ if p.state == OI.PhasePending then "text-textWeak" else "font-medium text-textStrong"] $ toHtml p.label
                  whenJust p.detail (span_ [class_ "ms-2 text-xs text-textWeak tabular-nums"] . toHtml)

          unless (T.null r.diffPreview)
            $ div_ [class_ "space-y-1.5"] do
              p_ [class_ "text-xs font-medium text-textWeak"] "Diff preview"
              pre_ [class_ "max-h-56 overflow-y-auto rounded-lg bg-bgBase/80 p-3 font-mono text-xs text-textStrong whitespace-pre-wrap break-words"] $ toHtml r.diffPreview

          when (r.status == OI.RunFailed)
            $ div_ [role_ "alert", class_ "rounded-lg border border-strokeError-weak bg-fillError-weak p-4 space-y-3"] do
              p_ [class_ "text-sm font-medium text-textError"] $ toHtml $ fromMaybe "The setup agent couldn't finish automatically." r.errorSummary
              whenJust r.errorLog \lg ->
                details_ [class_ "text-xs"] do
                  summary_ [class_ "cursor-pointer font-medium text-textWeak hover:text-textStrong"] "View diagnostic log"
                  pre_ [class_ "mt-2 max-h-40 overflow-y-auto rounded-md bg-bgBase p-2.5 font-mono text-xs text-textWeak whitespace-pre-wrap break-all"] $ toHtml lg
              label_ [Lucid.for_ "inst-tab-ai", class_ "btn btn-sm btn-primary rounded-lg cursor-pointer"] "Finish locally with AI prompt"

          div_ [class_ "flex flex-wrap items-center justify-between gap-3 pt-2 border-t border-strokeWeak/60"] do
            p_ [class_ "text-xs text-textWeak"] "Safe to leave — runs durably in the background."
            case (r.prUrl, r.prNumber) of
              (Just prUrl, Just prNum) ->
                a_ [href_ prUrl, target_ "_blank", rel_ "noopener noreferrer", class_ "btn btn-sm btn-primary rounded-lg gap-1.5"] do
                  span_ $ toHtml $ "View PR #" <> show prNum <> " on GitHub"
                  faSprite_ "arrow-up-right-from-square" "regular" "size-3"
              _ -> pass
  where
    phaseIcon_ = \case
      OI.PhaseDone -> faSprite_ "circle-check" "solid" "mt-0.5 size-4 shrink-0 text-textSuccess"
      OI.PhaseRunning -> faSprite_ "spinner" "regular" "mt-0.5 size-4 shrink-0 animate-spin text-textBrand"
      OI.PhaseFailed -> faSprite_ "circle-exclamation" "solid" "mt-0.5 size-4 shrink-0 text-textError"
      OI.PhasePending -> span_ [class_ "mt-1 size-3 shrink-0 rounded-full border border-strokeStrong", Aria.hidden_ "true"] mempty


data QuickCmd = QuickCmd {label :: Text, body :: Text -> Text -> Text}


data AITarget = AITarget
  { slug :: Text
  , label :: Text
  , icon :: Text
  , showClass :: Text
  , steps :: [(Text, Text)]
  , prompt :: Projects.ProjectId -> Text -> Text -> Text
  , quickCmd :: Maybe QuickCmd
  }


aiTargets :: [AITarget]
aiTargets =
  [ AITarget
      "backend"
      "Backend"
      "code"
      "group-has-[#ai-t-backend:checked]/hub:block"
      [ ("Detect framework & install OpenTelemetry", "Adds standard auto-instrumentation for Node, Go, Python, Java, PHP, Elixir, or .NET")
      , ("Configure OTLP export without hardcoded secrets", "Reads MONOSCOPE_API_KEY from env and exports traces & logs to your workspace")
      , ("Emit & verify a live test span", "Runs a local request and confirms arrival in Monoscope before finishing")
      ]
      ( \pid host apiKey ->
          unlines
            [ "Instrument this backend service with OpenTelemetry for Monoscope (project " <> pid.toText <> "):"
            , "1. Add Monoscope skills if not installed: npx skills add monoscope-tech/skills"
            , "2. Detect the language/framework and install the standard OpenTelemetry auto-instrumentation SDK."
            , "3. Export traces and logs via OTLP/HTTP to " <> host <> "/v1/traces (and /v1/logs) with header \"x-api-key\" read from MONOSCOPE_API_KEY (never hardcode secrets in git)."
            , "4. Run the app locally with OTEL_EXPORTER_OTLP_ENDPOINT=" <> host <> " and MONOSCOPE_API_KEY=" <> apiKey <> " to emit a real test request."
            , "5. Verify the span arrived using `monoscope logs --since 5m` (or MCP) before finishing."
            ]
      )
      Nothing
  , AITarget
      "frontend"
      "Frontend"
      "web"
      "group-has-[#ai-t-frontend:checked]/hub:block"
      [ ("Install Web OpenTelemetry SDK", "Captures page loads, Web Vitals, fetch/XHR requests, and unhandled browser errors")
      , ("Propagate traceparent headers to your API", "Links frontend user sessions directly to backend traces end-to-end")
      , ("Trigger & verify a live browser span", "Starts your dev server and confirms telemetry arrives in Monoscope")
      ]
      ( \pid host apiKey ->
          unlines
            [ "Add Monoscope Real User Monitoring (Web RUM) and browser OpenTelemetry to this frontend app (project " <> pid.toText <> "):"
            , "1. Add Monoscope skills: npx skills add monoscope-tech/skills"
            , "2. Install @opentelemetry/sdk-trace-web, @opentelemetry/instrumentation-document-load, @opentelemetry/instrumentation-fetch, and @opentelemetry/exporter-trace-otlp-http."
            , "3. Configure OTLP/HTTP export to " <> host <> "/v1/traces with header \"x-api-key\" from NEXT_PUBLIC_MONOSCOPE_KEY / VITE_MONOSCOPE_KEY, capturing page loads, Web Vitals, unhandled JS errors, and traceparent propagation to our API."
            , "4. Start the dev server, trigger a page load with key " <> apiKey <> ", and verify the browser span appears in Monoscope."
            ]
      )
      ( Just
          $ QuickCmd
            "Install Web OTel packages"
            (\_ _ -> "npm install @opentelemetry/sdk-trace-web @opentelemetry/exporter-trace-otlp-http @opentelemetry/auto-instrumentations-web")
      )
  , AITarget
      "kubernetes"
      "K8s / Docker"
      "server"
      "group-has-[#ai-t-kubernetes:checked]/hub:block"
      [ ("Generate OpenTelemetry Collector config", "Creates Helm DaemonSet values or Docker Compose with logs, host, and kubelet metrics")
      , ("Wire x-api-key via Secret or env var", "Exports OTLP/HTTP securely without committing credentials")
      , ("Validate & verify live cluster telemetry", "Dry-runs or deploys the collector and confirms metrics and logs arrive")
      ]
      ( \pid host apiKey ->
          unlines
            [ "Instrument this Kubernetes / Docker infrastructure with OpenTelemetry Collector for Monoscope (project " <> pid.toText <> "):"
            , "1. Add Monoscope skills: npx skills add monoscope-tech/skills"
            , "2. Create Helm values (open-telemetry/opentelemetry-collector DaemonSet) or Docker Compose collector config enabling logsCollection, hostMetrics, and kubeletMetrics."
            , "3. Configure otlphttp exporter to " <> host <> " with x-api-key from a Kubernetes Secret / MONOSCOPE_API_KEY env var."
            , "4. Apply or test with dry-run, then verify cluster metrics/logs arrive in Monoscope using `monoscope logs --since 5m`."
            ]
      )
      ( Just
          $ QuickCmd
            "Quick Helm install command"
            ( \host apiKey ->
                "helm repo add open-telemetry https://open-telemetry.github.io/opentelemetry-helm-charts && \\\n"
                  <> "helm upgrade --install monoscope-otel open-telemetry/opentelemetry-collector \\\n"
                  <> "  --set mode=daemonset --set presets.logsCollection.enabled=true --set presets.hostMetrics.enabled=true \\\n"
                  <> "  --set config.exporters.otlphttp.endpoint="
                  <> host
                  <> " \\\n"
                  <> "  --set config.exporters.otlphttp.headers.x-api-key="
                  <> apiKey
            )
      )
  , AITarget
      "server"
      "Linux"
      "terminal"
      "group-has-[#ai-t-server:checked]/hub:block"
      [ ("Install otelcol-contrib on your host", "Enables CPU, memory, disk, network, and systemd/journald log receivers")
      , ("Load credentials from /etc/monoscope/env", "Keeps MONOSCOPE_API_KEY outside config files")
      , ("Start systemd service & confirm arrival", "Verifies host metrics stream into Monoscope immediately")
      ]
      ( \pid host _ ->
          unlines
            [ "Set up host metrics and systemd/journald log collection on this Linux server for Monoscope (project " <> pid.toText <> "):"
            , "1. Install otelcol-contrib and configure hostmetrics (cpu, memory, disk, filesystem, network) + journald receivers."
            , "2. Export via otlphttp to " <> host <> " with x-api-key read from /etc/monoscope/env (MONOSCOPE_API_KEY)."
            , "3. Start the systemd service and verify host metrics arrive in Monoscope."
            ]
      )
      ( Just
          $ QuickCmd
            "One-line Monoscope CLI + test event"
            (\_ _ -> "curl -fsSL https://monoscopehq.com/install.sh | sh && monoscope auth login && monoscope send-event -m \"server connected\" --service linux-host")
      )
  , AITarget
      "database"
      "Databases"
      "database"
      "group-has-[#ai-t-database:checked]/hub:block"
      [ ("Detect your database engines", "Inspects compose, Helm, Terraform, or ORM config for Postgres, MySQL, Mongo, or Redis")
      , ("Generate read-only OTel receiver config", "Collects query, connection, and replication metrics via env credentials")
      , ("Verify live database metrics", "Confirms telemetry reaches your Monoscope workspace")
      ]
      ( \pid host apiKey ->
          unlines
            [ "Configure OpenTelemetry database telemetry (PostgreSQL, MySQL, MongoDB, or Redis) for Monoscope (project " <> pid.toText <> "):"
            , "1. Add Monoscope skills: npx skills add monoscope-tech/skills"
            , "2. Detect which databases this project uses (docker-compose, Helm, Terraform, or ORM config) and generate an otelcol-contrib receiver config (postgresql / mysql / mongodb / redis) using read-only monitoring credentials from env vars."
            , "3. Export metrics via otlphttp to " <> host <> " with x-api-key from MONOSCOPE_API_KEY (" <> apiKey <> " for local verification)."
            , "4. Verify database metrics appear in Monoscope."
            ]
      )
      Nothing
  ]


compactChannelCard_ :: Text -> Text -> Bool -> Text -> Html ()
compactChannelCard_ serviceName iconPath isConnected connectUrl =
  div_ [class_ "px-4 py-3 rounded-xl border border-strokeWeak bg-fillWeak justify-between items-center flex"] do
    div_ [class_ "items-center gap-2 flex overflow-hidden"] do
      img_ [src_ iconPath, alt_ "", class_ "size-5 shrink-0"]
      span_ [class_ "text-textStrong text-base font-medium truncate"] $ toHtml serviceName
    if isConnected
      then connectionBadge_ True
      else a_ [href_ connectUrl, target_ "_blank", rel_ "noopener noreferrer", class_ "border px-3 h-8 flex items-center shadow-xs border-[var(--brand-color)] rounded-md text-textBrand text-sm hover:bg-fillBrand-weak active:scale-[0.96] transition-[color,background-color,transform]"] "Connect"


-- Legacy & Shared Steps (Preserved for backwards compatibility) ------------------------

onboardingCompleteBody :: Projects.ProjectId -> Html ()
onboardingCompleteBody pid =
  div_ [class_ "w-full max-w-xl h-full flex items-center mx-auto relative px-4 md:px-0"] $ do
    div_ [class_ "flex-col gap-4 flex w-full p-14 my-auto border border-weak rounded-2xl"] $ do
      iconBadge_ IconLg SuccessBadge "circle-check"
      div_ [class_ "flex flex-col gap-2"] do
        h3_ [class_ " text-textStrong  text-2xl"] "Onboarding completed!"
        p_ [class_ " text-textWeak text-sm"] "You're all set! You can now start exploring the monoscope dashboard by clicking the button below."
      a_ [class_ "btn-primary py-2  rounded-lg text-center mt-1", href_ $ urls.homeGet pid] "Go to your project"


pricingPage :: Projects.ProjectId -> Text -> Text -> Text -> Bool -> Bool -> Bool -> Html ()
pricingPage pid lemon critical paymentPlan freeTierEnabled basicAuthEnabled trialEligible =
  div_ [class_ "w-full max-w-[1100px] mx-auto mt-[70px] mb-10 px-4 md:px-0"] $ do
    div_ [class_ "flex-col gap-4 flex w-full"] $ do
      div_ [class_ "w-full md:w-1/2"] $ stepIndicatorOf 4 4 "Please pick a plan" $ stepUrl pid Alerts
      paymentPlanPicker pid lemon critical paymentPlan freeTierEnabled basicAuthEnabled trialEligible True Projects.NoBillingProvider
      div_ [class_ "flex flex-col gap-2 w-full pb-20"] do
        span_ [class_ "text-textStrong text-2xl mt-20"] "FAQ"
        div_ [class_ "join join-vertical w-full mt-4"] do
          faQ "What is an event?" "An event is any of span, log, or metric that you send to Monoscope."
          faQ "How do you handle security and sensitive data?" "We employ encryption and authentication measures to ensure the security of your data during transmission and storage. All our SDKs also support redacting data. You can simply specify the JSONPath to the fields that you don't want the SDKs to forward to Monoscope, and those sensitive fields will be stripped out/redacted before the data even leaves your servers and replaced with the text \"CLIENT REDACTED\" on our end. We will never see anything you don't want us to see."
          faQ "What makes us better than others?" "Aside from the standard observability features like traces, logs, and metrics, Monoscope takes it a step further by monitoring request payloads for both incoming and outgoing requests, automatic error reporting, and payload-change detection — giving engineering teams all the information they need to seamlessly debug and fix issues in their servers."
  where
    faQ :: Text -> Text -> Html ()
    faQ question answer =
      details_ [class_ "collapse collapse-arrow join-item border border-strokeWeak"] do
        summary_ [class_ "collapse-title text-textStrong font-medium"] $ toHtml question
        div_ [class_ "collapse-content text-textWeak leading-relaxed"] $ p_ $ toHtml answer


data Framework = Framework {name :: Text, icon :: Text, docsPath :: Text}


data Language = Language {slug :: Text, label :: Text, frameworks :: [Framework]}


data IntegrationGroup = IntegrationGroup {name :: Text, languages :: [Language]}


integrationGroups :: [IntegrationGroup]
integrationGroups =
  [ IntegrationGroup
      "Applications"
      [ Language
          "js"
          "Javascript"
          [ Framework "ExpressJS" "express-icon.png" "nodejs/expressjs"
          , Framework "AdonisJS" "adonis-icon.svg" "nodejs/adonisjs"
          , Framework "Fastify" "fastify-icon.png" "nodejs/fastifyjs"
          , Framework "NestJS" "nest-icon.png" "nodejs/nestjs"
          , Framework "NextJS" "next-icon.svg" "nodejs/nextjs"
          ]
      , Language
          "go"
          "Golang"
          [ Framework "Chi" "chi-logo.svg" "golang/chi"
          , Framework "Echo" "echo-logo.png" "golang/echo"
          , Framework "Fiber" "fiber-logo.svg" "golang/fiber"
          , Framework "Gin" "gin-logo.png" "golang/gin"
          , Framework "Gorilla Mux" "mux-logo.png" "golang/gorillamux"
          , Framework "Native" "go-logo.svg" "golang/native"
          ]
      , Language
          "py"
          "Python"
          [ Framework "Django" "django-icon.png" "python/django"
          , Framework "FastAPI" "fastapi-icon.png" "python/fastapi"
          , Framework "Flask" "flask-icon.png" "python/flask"
          , Framework "Pyramid" "pyramid-icon.png" "python/pyramid"
          ]
      , Language "elixir" "Elixir" [Framework "Phoenix" "phoenix-logo.png" "elixir/phoenix"]
      , Language
          "php"
          "PHP"
          [ Framework "Laravel" "laravel-icon.png" "php/laravel"
          , Framework "Slim" "slim-icon.png" "php/slim"
          , Framework "Symfony" "symfony-icon.png" "php/symfony"
          ]
      , Language "java" "Java" [Framework "Spring Boot" "springboot-logo.svg" "java/springboot"]
      , Language "cs" "C#" [Framework ".Net Core" "netcore-logo.png" "dotnet/dotnetcore"]
      ]
  , IntegrationGroup
      "Infrastructure"
      [ Language "linux" "Linux" [Framework "Linux" "linux.svg" "infrastructure/linux"]
      , Language "docker" "Docker" [Framework "Docker" "docker.svg" "infrastructure/docker"]
      , Language "kubernetes" "Kubernetes" [Framework "Kubernetes" "kubernetes.svg" "infrastructure/kubernetes"]
      , Language "kafka" "Kafka" [Framework "Kafka" "kafka.svg" "infrastructure/kafka"]
      ]
  , IntegrationGroup
      "Databases"
      [ Language "postgresql" "PostgreSQL" [Framework "PostgreSQL" "postgresql.svg" "databases/postgres"]
      , Language "mongodb" "MongoDB" [Framework "MongoDB" "mongodb.svg" "databases/mongodb"]
      , Language "mysql" "MySQL" [Framework "MySQL" "mysql.svg" "databases/mysql"]
      ]
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
        $ div_ [class_ "max-w-xl"] (stepIndicator 4 "Instrument your apps or servers" (stepUrl pid NotifChannel))
      div_ [class_ "flex-col w-full gap-3 md:gap-4 flex mt-3 md:mt-4 px-4 md:px-12 overflow-y-auto flex-grow pb-20 md:pb-4"] do
        p_ [class_ "text-textWeak leading-relaxed text-sm md:text-base"] do
          "Select your stack below for integration instructions."

        div_ [class_ "mt-2 md:mt-6 mb-0 p-3 md:p-4 bg-fillWeak border border-strokeWeak rounded-xl"] do
          div_ [class_ "mb-1.5 md:mb-2 text-textWeak text-sm"] "Your API Key"
          copyField_ "api-key-display" apikey

        div_ [class_ "mb-4 px-4 py-2.5 bg-gradient-to-r from-fillInformation-weak to-transparent border border-strokeInformation-weak rounded-lg"] do
          p_ [class_ "text-sm text-textStrong"] do
            "Want to test quickly? "
            dialogTrigger_
              "telemetrygen-modal"
              [class_ "text-textBrand hover:text-textStrong underline font-medium cursor-pointer"]
              "Send a test event"
            " with the CLI"

        forM_ integrationGroups \grp -> div_ [class_ "mb-4 md:mb-6"] do
          div_ [class_ "text-textWeak text-lg md:text-xl mb-2"] $ toHtml grp.name
          div_ [class_ "grid grid-cols-2 gap-2"] $ forM_ grp.languages languageItem

        integrationCta_ False
      integrationCta_ True

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
          div_ [class_ "max-md:hidden p-12 text-center group-has-[.checkbox:checked]/pg:hidden flex items-center justify-center h-full"]
            $ emptyState_ def{icon = Just "brackets-curly", action = ESCustom walkthroughs} "Select your stack on the left to begin" "You can also check out our youtube videos for more interactive walkthroughs."

          forM_ (concatMap (.languages) integrationGroups) \l ->
            div_ [class_ $ "p-4 lang-guide hidden group-has-[#check-" <> l.slug <> ":checked]/pg:block", id_ $ l.slug <> "_main"] do
              div_ [class_ "px-2 md:px-8 sticky top-0 z-10 bg-bgBase py-2"]
                $ div_ [class_ "inline-block tabs tabs-box tabs-outline p-0 bg-bgBase text-textWeak border ", role_ "tablist"]
                $ forM_ (zip [0 :: Int ..] l.frameworks) \(idx, fw) ->
                  label_ [class_ "tab gap-2 items-center", Lucid.for_ $ "fw-tab-" <> l.slug <> "-" <> show idx] do
                    input_
                      $ [ type_ "radio"
                        , name_ $ "tab-" <> l.slug
                        , id_ $ "fw-tab-" <> l.slug <> "-" <> show idx
                        , class_ "hidden"
                        , Aria.label_ fw.name
                        , data_ "guide-url" $ "https://monoscopehq.com/docs/sdks/" <> fw.docsPath
                        , hxGet_ $ "/proxy/docs/sdks/" <> fw.docsPath
                        , hxTarget_ $ "#fw-content-" <> l.slug
                        , term "hx-sync" $ "#fw-content-" <> l.slug <> ":replace"
                        , hxTrigger_ "change"
                        , hxSwap_ "innerHTML"
                        , hxSelect_ "#mainArticle"
                        , hxIndicator_ $ "#fw-indicator-" <> l.slug
                        ]
                      <> [checked_ | idx == 0]
                    unless (T.null fw.icon) $ img_ [class_ "h-5 w-5", src_ $ "https://monoscopehq.com/assets/img/framework-logos/" <> fw.icon]
                    span_ $ toHtml fw.name

              div_ [class_ "relative p-2 md:p-8"] do
                div_ [id_ $ "fw-indicator-" <> l.slug, class_ "htmx-indicator flex justify-center py-5"]
                  $ loadingIndicator_ LdMD LdDots
                div_
                  [ id_ $ "fw-content-" <> l.slug
                  , hxGet_ $ "/proxy/docs/sdks/" <> foldMap (.docsPath) (listToMaybe l.frameworks)
                  , hxTrigger_ "intersect once"
                  , term "hx-sync" "this:replace"
                  , hxSwap_ "innerHTML"
                  , hxSelect_ "#mainArticle"
                  , class_ "prose-a:!text-textBrand prose-a:!underline"
                  ]
                  ""

    cliQuickTestModal_ "install-cmd" "curl https://monoscopehq.com/install.sh | sh && monoscope auth login" "telemetrygen-cmd"

    link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-dark.min.css")]

    let hljsDeps = loadDeps [HighlightJs]
    script_
      [text|
        ${hljsDeps}.then(() => hljs.highlightAll());
        document.body.addEventListener('htmx:after:process', function(event) {
          const target = event.target.closest('[id^="fw-content-"],[id^="m-fw-content-"]');
          if (target) {
            ${hljsDeps}.then(() => target.querySelectorAll('pre code').forEach(block => hljs.highlightElement(block)));
          }
        });
      |]
  where
    walkthroughs = div_ [class_ "flex flex-col gap-3 w-full max-w-md"] do
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
      a_ [href_ "https://www.youtube.com/@monoscope", target_ "_blank", class_ "text-textBrand hover:underline text-sm font-medium"] "Watch more tutorials →"

    integrationCta_ isMobile =
      div_ [class_ $ bool "max-md:hidden flex items-center gap-4 py-8" "md:hidden fixed bottom-0 left-0 right-0 bg-bgRaised border-t border-strokeWeak pl-4 pr-18 py-3 flex items-center gap-3 z-30" isMobile] do
        button_ [class_ $ bool "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center" "btn-primary px-6 py-2.5 text-base rounded-xl cursor-pointer flex-1 flex items-center justify-center" isMobile, hxGet_ $ urls.projects.onboardingIntegrationCheck pid Nothing, hxSwap_ "none", hxIndicator_ "#loadingIndicator"] "Confirm & Proceed"
        a_ [class_ $ bool "px-4 py-3 flex items-center underline text-textBrand text-xl cursor-pointer" "px-3 py-2.5 flex items-center underline text-textBrand text-base cursor-pointer" isMobile, type_ "button", hxPost_ $ urls.projects.onboardingSkipped pid (Just Integration)] "Skip"

    languageItem l =
      label_ [class_ "group/li h-12 px-3 py-2 bg-transparent rounded-xl border border-strokeWeak justify-start items-center gap-3 inline-flex cursor-pointer"] do
        input_
          [ type_ "checkbox"
          , class_ "checkbox shrink-0"
          , id_ $ "check-" <> l.slug
          , value_ l.slug
          , onchange_ $ "if(this.checked){var dp=document.getElementById('docs-panel');if(window.innerWidth<768){dp.classList.add('open');requestAnimationFrame(function(){dp.scrollTop=0});}else{requestAnimationFrame(function(){document.getElementById('" <> l.slug <> "_main').scrollIntoView({behavior:'smooth'})});}}"
          ]
        div_ [class_ "flex w-full items-center justify-between overflow-hidden"] do
          div_ [class_ "flex items-center gap-2 text-sm min-w-0"] do
            img_ [class_ "h-5 w-5", src_ $ "/public/assets/svgs/" <> l.slug <> ".svg"]
            span_ $ toHtml l.label
          div_ [class_ "hidden group-has-[.checkbox:checked]/li:block text-sm"]
            $ integrationPoller_ pid (T.replace "#" "sharp" l.label)


integrationPoller_ :: Projects.ProjectId -> Text -> Html ()
integrationPoller_ pid language =
  div_
    [ class_ "flex items-center gap-1 shrink-0"
    , hxGet_ $ urls.projects.onboardingIntegrationCheck pid (Just language)
    , hxSwap_ "outerHTML"
    , hxTrigger_ "intersect delay:5s"
    ]
    do
      span_ [class_ "text-textStrong hidden md:inline text-xs"] "waiting"
      faSprite_ "spinner" "regular" "h-4 w-4 animate-spin shrink-0"


onboardingStepWrapper_ :: Int -> Text -> Text -> Html () -> Html ()
onboardingStepWrapper_ step title prevUrl content =
  div_ [class_ "w-full max-w-xl mx-auto mt-20 md:mt-[156px] mb-10 px-4 md:px-0"]
    $ div_ [class_ "flex-col gap-4 flex w-full"] (stepIndicator step title prevUrl >> content)


notifChannelsWithUrls :: Text -> Text -> Projects.ProjectId -> Text -> V.Vector Text -> Bool -> Bool -> Html ()
notifChannelsWithUrls slackUrl discordUrl pid phone emails hasDiscord hasSlack =
  onboardingStepWrapper_ 3 "How should we notify you about issues?" (stepUrl pid Survey) do
    div_ [class_ "flex-col w-full gap-8 flex mt-4"] do
      div_ [class_ "w-full gap-2 grid grid-cols-1 md:grid-cols-2"] do
        integrationCard "Slack" "/public/assets/svgs/slack.svg" hasSlack slackUrl
        integrationCard "Discord" "/public/assets/svgs/discord.svg" hasDiscord discordUrl
      form_
        [ class_ "flex flex-col gap-8"
        , hxPost_ $ urls.projects.onboardingPhoneEmailsPost pid
        , hxExt_ "json-enc"
        , hxVals_ "js:{phoneNumber: document.getElementById('phoneNumber').value , emails: window.getTagValues('#emails')}"
        , hxTarget_ "#inviteModalContainer"
        , hxSwap_ "innerHTML"
        , hxIndicator_ "#loadingIndicator"
        ]
        do
          formField_ FieldMd def{inputType = "tel", value = phone} "Notify phone number" "phoneNumber" False Nothing
          formField_ FieldMd def "Notify the following email address" "emails" False $ Just $ tagInput_ "emails" "" [data_ "tagify-initial" $ decodeUtf8 $ AE.encode emails]
          div_ [class_ "items-center gap-4 flex"] $ button_ [class_ "btn-primary px-8 py-3 text-xl rounded-xl cursor-pointer flex items-center"] "Proceed"
    div_ [id_ "inviteModalContainer"] pass
  where
    integrationCard :: Text -> Text -> Bool -> Text -> Html ()
    integrationCard serviceName iconPath isConnected connectUrl =
      div_ [class_ "px-3 py-2 rounded-xl border border-strokeWeak bg-fillWeak justify-between items-center flex"] $ do
        div_ [class_ "items-center gap-1.5 flex overflow-hidden"] $ do
          img_ [src_ iconPath]
          span_ [class_ "text-center text-textStrong text-xl"] $ toHtml serviceName
        if isConnected
          then connectionBadge_ True
          else a_ [target_ "_blank", class_ "border px-3 h-8 flex items-center shadow-xs border-[var(--brand-color)] rounded-lg text-textBrand ", href_ connectUrl] "Connect"


onboardingInfoBody :: Projects.ProjectId -> Text -> Text -> Text -> Text -> Text -> Html ()
onboardingInfoBody pid firstName lastName cName cSize fUsFrm =
  onboardingStepWrapper_ 1 "Tell us a little bit about you" ""
    $ form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ urls.projects.onboardingInfoPost pid, hxIndicator_ "#loadingIndicator"]
    $ do
      div_ [class_ "flex-col w-full gap-4 mt-4 flex"] $ do
        forM_ ([("First Name", "firstName", firstName), ("Last Name", "lastName", lastName), ("Company Name", "companyName", cName)] :: [(Text, Text, Text)]) \(label, name, val) ->
          formField_ FieldMd def{value = val} label name True Nothing
        let createSelectField selected label name opts = formSelectField_ FieldMd label name True $ options_ (Just selected) (("", "") : opts)
        createSelectField cSize "Company Size" "companySize" [("1 - 4", "1 to 4"), ("5 - 10", "5 to 10"), ("11 - 25", "11 to 25"), ("26+", "26 and above")]
        createSelectField fUsFrm "How Did You Hear About Us" "whereDidYouHearAboutUs" [("google", "Google"), ("twitter", "Twitter"), ("linkedin", "LinkedIn"), ("friend", "Friend"), ("other", "Other")]
      div_ [class_ "items-center gap-1 flex"] $ button_ [class_ "btn-primary px-6 py-4 text-xl rounded-lg cursor-pointer flex items-center"] "Proceed"


onboardingConfigBody :: Projects.ProjectId -> Text -> [Text] -> Html ()
onboardingConfigBody pid loca func =
  onboardingStepWrapper_ 2 "Let's configure your project" (stepUrl pid Info)
    $ form_ [class_ "flex-col w-full gap-8 flex", hxPost_ $ urls.projects.onboardingConfPost pid, hxIndicator_ "#loadingIndicator"]
    $ do
      div_ [class_ "flex-col w-full gap-14 mt-4 flex"] $ do
        formField_ FieldMd def "Where should your project be hosted?" "location" True $ Just $ div_ [class_ "pt-2 flex-col gap-4 flex"] do
          forM_ locations $ createBinaryField "radio" "location" [loca]
        formField_ FieldMd def "Which Monoscope features will you be using?" "functionality" True $ Just $ div_ [class_ "pt-2 flex-col gap-4 flex"] do
          forM_ functionalities $ createBinaryField "checkbox" "functionality" func
      div_ [class_ "items-center gap-1 flex"] $ button_ [class_ "btn-primary px-6 py-4 text-xl rounded-lg cursor-pointer flex items-center"] "Proceed"
  where
    locations :: [(Text, Text)]
    locations = [("usa", "USA"), ("eu", "EU"), ("asia", "Asia")]

    functionalities :: [(Text, Text)]
    functionalities =
      [ ("logs", "Logs, Metrics and Traces")
      , ("analytics", "API Analytics and Payload Monitoring")
      , ("healthchecks", "Website/API Healthchecks and Status Pages")
      , ("changeTracking", "API (Breaking) Change Tracking")
      , ("customDashboards", "Custom Dashboards")
      ]

    createBinaryField :: Text -> Text -> [Text] -> (Text, Text) -> Html ()
    createBinaryField kind name selectedValues (value, label) =
      div_ [class_ " items-center gap-3 inline-flex"] $ do
        input_ $ [class_ "w-6 h-6 rounded-sm", type_ kind, name_ name, value_ value, id_ value] <> [required_ "required" | kind == "radio"] <> [checked_ | value `elem` selectedValues]
        label_ [class_ " text-textStrong text-sm", Lucid.for_ value] $ toHtml label


cliQuickTestModal_ :: Text -> Text -> Text -> Html ()
cliQuickTestModal_ installId installCmd sendId =
  nativeDialog_ "telemetrygen-modal" "Quick Test with the CLI" "max-w-2xl" do
    p_ [class_ "mt-2 mb-6 text-textWeak leading-relaxed"] "Use the Monoscope CLI to send one test event and verify your setup is working."
    div_ [class_ "space-y-4"] do
      div_ [class_ "p-4 bg-fillWeak rounded-lg"] do
        div_ [class_ "text-textStrong font-medium mb-2 flex items-center gap-2"] do
          span_ [class_ "inline-flex items-center justify-center w-6 h-6 rounded-full bg-fillBrand-weak text-textBrand text-sm font-bold"] "1"
          span_ "Install & authenticate (if you haven't)"
        copyField_ installId installCmd
      div_ [class_ "p-4 bg-fillWeak rounded-lg"] do
        div_ [class_ "text-textStrong font-medium mb-2 flex items-center gap-2"] do
          span_ [class_ "inline-flex items-center justify-center w-6 h-6 rounded-full bg-fillBrand-weak text-textBrand text-sm font-bold"] "2"
          span_ "Send a test event"
        copyField_ sendId "monoscope send-event -m \"hello world\""
    div_ [class_ "mt-6"]
      $ callout_ SuccessBadge (Just "What happens next?") "After running the command, look for \"hello world\" in your dashboard. You can then proceed with the full SDK integration."
    div_ [class_ "modal-action"] $ button_ [type_ "button", [__|on click call (closest <dialog/>).close()|], class_ "btn"] "Close"


inviteTeamMemberModal :: Projects.ProjectId -> V.Vector Text -> Bool -> Html ()
inviteTeamMemberModal pid emails enableFreetier =
  div_ [id_ "invite-modal-container"] do
    nativeDialogWith_ [[__|init call me.showModal()|]] "inviteModal" "Test notification sent" "bg-bgRaised max-w-md space-y-2" do
      div_ [class_ "text-textWeak text-sm"] "No notification? Close this modal and verify your emails and channels."
      details_ [class_ "w-full border-t border-strokeWeak pt-3"] $ do
        summary_ [class_ "text-textBrand text-sm cursor-pointer"] "Invite team members"
        div_ [class_ "flex-col gap-3 flex pt-3"] $ do
          when enableFreetier $ callout_ BrandBadge Nothing "Only the project owner can access a Free project. Invited members remain disabled until you upgrade."
          div_ [class_ "w-full gap-2 flex items-center"] $ do
            input_ [class_ "input input-sm w-full", placeholder_ "email@example.com", type_ "email", id_ "add-member-input"]
            button_
              [ type_ "button"
              , class_ "btn-primary rounded-xl px-4 py-2 text-white text-sm cursor-pointer whitespace-nowrap"
              , [__|on click
                  set email to #add-member-input.value
                  if email is empty exit end
                  set node to #member-template.cloneNode(true)
                  remove @id from node then remove .hidden from node
                  set inp to first <input/> in node then set inp.value to email then set inp.name to 'emails'
                  set sp to first <span/> in node then set sp.textContent to email
                  call #members-container.appendChild(node)
                  set #add-member-input.value to ''|]
              ]
              "Invite"
          form_
            [ class_ "flex-col flex"
            , id_ "members-container"
            , hxPost_ $ urls.projects.membersManagePost pid (Just "true")
            , hxIndicator_ "#loadingIndicator"
            ]
            $ do
              inviteMemberItem Nothing
              forM_ emails (inviteMemberItem . Just)
      div_ [class_ "modal-action w-full flex items-center justify-start gap-4"] do
        button_ [class_ "btn-primary px-8 py-2 text-lg rounded-xl cursor-pointer flex items-center", type_ "submit", term "form" "members-container"] "Proceed"
        button_ [type_ "button", [__|on click call (closest <dialog/>).close()|], class_ "text-textWeak text-sm underline cursor-pointer"] "Close"
  where
    inviteMemberItem :: Maybe Text -> Html ()
    inviteMemberItem emailM = do
      let (email, isTemplate) = (fromMaybe "" emailM, isNothing emailM)
      div_ (class_ ("flex  py-1 w-full justify-between items-center border-b border-strokeWeak " <> bool "" "hidden" isTemplate) : [id_ "member-template" | isTemplate]) do
        div_ [class_ "pr-6 py-1  w-full justify-start items-center inline-flex"] do
          input_ ([type_ "hidden", value_ email] <> [name_ "emails" | not isTemplate])
          span_ [class_ "text-textStrong text-sm font-normal"] $ toHtml email
        select_ [name_ "permissions", class_ "select select-xs"] $ roleOptions_ Nothing
        button_
          [ [__|on click remove closest <div/>|]
          , class_ "text-textBrand ml-4 text-sm underline"
          , type_ "button"
          ]
          "remove"


stepIndicator :: Int -> Text -> Text -> Html ()
stepIndicator = stepIndicatorOf 5


stepIndicatorOf :: Int -> Int -> Text -> Text -> Html ()
stepIndicatorOf total step title prevUrl = do
  -- Sleek top indeterminate progress bar (replaces the clunky floating dots overlay)
  div_ [id_ "loadingIndicator", class_ "htmx-indicator fixed top-0 inset-x-0 z-50 h-0.5 overflow-hidden pointer-events-none bg-fillBrand-weak"]
    $ div_ [class_ "h-full w-2/5 bg-fillBrand-strong animate-[ob-progress_1s_ease-in-out_infinite]"] mempty
  style_ "@keyframes ob-progress{0%{transform:translateX(-100%)}100%{transform:translateX(350%)}}#loadingIndicator.ob-navigating{opacity:1!important}"
  script_
    [text|
      (function(){
        if (window.__obNavBound) return;
        window.__obNavBound = true;
        document.addEventListener('click', function(e) {
          var a = e.target.closest('a[href*="/onboarding"]');
          if (a && !e.metaKey && !e.ctrlKey && !e.shiftKey && !a.target) {
            var bar = document.getElementById('loadingIndicator');
            if (bar) bar.classList.add('ob-navigating');
          }
        });
      })();
    |]
  div_ [class_ "flex-col gap-2 md:gap-4 flex w-full"] $ do
    a_ [href_ "/", class_ "absolute top-4 left-4 md:top-10 md:left-10 py-2 pr-2 rounded-xs"] $ brandLogo_ "h-7"
    div_ [class_ "flex-col gap-1.5 md:gap-2 flex w-full"] $ do
      headerRow_ [] do
        div_ [class_ "text-textStrong text-sm md:text-base"] $ "Step " <> show step <> " of " <> show total
        when (step > 1)
          $ a_ [class_ "flex items-center gap-1.5 text-textBrand text-sm md:hidden", href_ prevUrl] do
            faSprite_ "arrow-left" "regular" "h-3.5 w-3.5"
            span_ "Back"
      div_ [class_ $ "grid w-full gap-1 " <> case total of { 3 -> "grid-cols-3"; 4 -> "grid-cols-4"; _ -> "grid-cols-5" }] $ do
        forM_ [1 .. total] $ \i -> div_ [class_ $ "h-1.5 md:h-2 w-full rounded-sm " <> if step >= i then "btn-primary rounded-sm" else " bg-fillWeak shadow-sm border border-strokeWeak"] pass
      when (step > 1)
        $ a_ [class_ "max-md:hidden flex items-center gap-3 text-textBrand w-full mt-2", href_ prevUrl] do
          faSprite_ "arrow-left" "regular" "h-4 w-4"
          span_ "Back"
    span_ [class_ "text-textStrong text-xl md:text-4xl mt-1 md:mt-4"] $ toHtml title


proxyLandingH :: (HTTP :> es, State.State HXRedirectDest :> es, State.State HXReswap :> es, State.State TriggerEvents :> es, State.State XWidgetJSON :> es) => [Text] -> Eff es (RespHeaders ProxiedPage)
proxyLandingH path = do
  response <- W.get $ toString $ "https://monoscopehq.com/" <> T.intercalate "/" path
  addRespHeaders $ ProxiedPage $ decodeUtf8 $ fromMaybe "" $ response L.^? W.responseBody
