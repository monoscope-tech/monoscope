module Pages.GitSync (
  gitWebhookPostH,
  gitSyncSettingsGetH,
  gitSyncSettingsPostH,
  gitSyncSettingsDeleteH,
  GitSyncForm (..),
  RepoSelectForm (..),
  queueGitSyncPush,
  -- GitHub App handlers
  githubAppInstallH,
  githubAppCallbackH,
  githubAppReposH,
  githubAppSelectRepoH,
) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Effectful.Wreq qualified as W
import Data.Pool (withResource)
import Data.Text qualified as T
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxDelete_, hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper, withSettingsPage)
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), confirmModal_, connectionBadge_, formField_, formSelectField_, headerRow_, iconBadgeLg_, iconBadge_, primaryButton_, sectionLabel_, settingsH2_, settingsSection_)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Relude hiding (ask)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders)
import Utils (LoadingSize (..), faSprite_, htmxIndicator_, renderMarkdown)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (parseUrlPiece)


data GitSyncForm = GitSyncForm
  { host :: Maybe Git.GitHost
  -- ^ Absent on the GitHub App path, which cannot be any other host.
  , apiBase :: Maybe Text
  -- ^ The origin of a self-hosted install, as typed. Normalised by 'Git.mkGitConn' before it
  -- is stored, so what lands in the column is already an API base.
  , owner :: Text
  , repo :: Text
  , branch :: Text
  , accessToken :: Text
  , webhookSecret :: Maybe Text
  , pathPrefix :: Maybe Text -- Optional folder prefix (e.g., "monoscope" -> monoscope/dashboards/)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


statusResp :: Text -> AE.Value
statusResp s = AE.object ["status" AE..= s]


errResp :: Text -> AE.Value
errResp msg = AE.object ["status" AE..= ("error" :: Text), "message" AE..= msg]


-- | An inbound push from any host.
--
-- The order below is the security-relevant part and is deliberate:
--
--   1. parse /only/ the repository name — enough to find the row that holds the secret;
--   2. check the row's host matches the route the delivery arrived on;
--   3. verify the signature;
--   4. only then decide it is a push and queue a job.
--
-- Doing (4) before (3) is how a forged body gets to enqueue work. Nothing here acts on the
-- payload beyond its name until 'Git.verifyWebhook' has returned.
gitWebhookPostH :: Git.GitHost -> Git.WebhookReq -> ATBaseCtx AE.Value
gitWebhookPostH host req = case Git.parseWebhookRepo host req.body of
  Nothing -> errResp "missing repository" <$ Log.logAttention "Git webhook without a repository name" (Git.hostSlug host)
  Just fullName -> do
    let (ownerName, repoName) = Git.splitFullName fullName
    GitSync.getGitSyncByRepo host ownerName repoName >>= \case
      Nothing -> statusResp "ignored" <$ Log.logTrace "Git webhook for untracked repo" (Git.hostSlug host, fullName)
      Just sync -> case Git.verifyWebhook host sync.webhookSecret req of
        Left err -> errResp err <$ Log.logAttention "Git webhook signature validation failed" (Git.hostSlug host, fullName, err)
        Right () -> do
          -- Only reachable for a GitHub row created before secrets were required; every new
          -- connection stores one, so this is a migration artefact worth seeing in the log.
          when (isNothing sync.webhookSecret) $ Log.logWarn "Git webhook accepted without a secret to verify against" (Git.hostSlug host, fullName)
          if Git.isPushEvent host req.event
            then do
              ctx <- ask @Config.AuthContext
              liftIO $ withResource ctx.jobsPool \conn ->
                void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo sync.projectId
              statusResp "ok" <$ Log.logTrace "Triggered git sync from webhook" (sync.projectId, Git.hostSlug host, fullName)
            else AE.object ["status" AE..= ("ignored" :: Text), "event" AE..= req.event] <$ Log.logTrace "Ignoring non-push git event" req.event


gitSyncSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsGetH pid = do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  withSettingsPage pid "Integrations" \_ -> pure $ settingsSection_ do
    settingsH2_ "GitHub Sync"
    div_ [id_ "git-sync-content"] $ gitSyncSettingsView ctx.config ctx.env.hostUrl pid syncM
    -- The same installation powers both, and someone who just connected here should not have
    -- to discover the other half of it from the settings index.
    p_ [class_ "pt-4 text-xs text-textWeak"] do
      "This is the repository monoscope keeps its own dashboard and monitor YAML in. To read the source behind a stack frame, link your service repositories under "
      a_ [href_ ("/p/" <> pid.toText <> "/settings/code-mappings"), class_ "text-textBrand underline"] "Source Code"
      " — the same installation covers it."


-- | Connect or update the config-sync repository.
--
-- The connection is built before anything is written: 'Git.mkGitConn' is what rejects a Gitea
-- row with no server URL, a Bitbucket row with one, an @http://@ origin, and a URL carrying
-- credentials. Storing first and discovering that on the next background job would leave a
-- row that looks connected and never syncs.
gitSyncSettingsPostH :: Projects.ProjectId -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsPostH pid form = do
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      host = fromMaybe Git.GitHub form.host
  existingM <- GitSync.getGitHubSync pid
  -- The host/origin pair is vetted on its own: an update that keeps the stored token has no
  -- token to build a connection with, and 'Git.validateOrigin' is the half of the check that
  -- never needed one. The flag is checked here as well as in the form, because the form is
  -- not the only way to reach this handler.
  let vetted
        | host `notElem` offeredHosts ctx.config = Left $ Git.hostLabel host <> " connections are not enabled on this deployment."
        | otherwise = Git.validateOrigin host form.apiBase
  case vetted of
    Left err -> do
      addErrorToast ("Could not connect to " <> Git.hostLabel host) (Just err)
      addRespHeaders $ gitSyncSettingsView ctx.config ctx.env.hostUrl pid existingM
    Right _ -> do
      -- Detecting the default branch needs a token, so a form that kept the stored one falls
      -- back to "main" rather than asking the host with a credential it does not have.
      branch <- case (T.null form.branch, Git.mkGitConn host form.apiBase form.accessToken) of
        (False, _) -> pure form.branch
        (True, Right conn) -> W.runHTTPWreq $ Git.defaultBranchOf conn (Git.RepoRef form.owner form.repo "HEAD")
        -- No usable token, so nothing to ask the host with; "main" is what the field would
        -- have defaulted to anyway.
        (True, Left _) -> pure "main"
      let apiBase = rightToMaybe . Git.normalizeOrigin =<< mfilter (not . T.null . T.strip) form.apiBase
      syncM <- case existingM of
        Nothing -> GitSync.insertGitHubSync encKey pid host apiBase form.owner form.repo branch form.accessToken form.webhookSecret (fromMaybe "" form.pathPrefix)
        Just existing
          | T.null form.accessToken -> GitSync.updateGitHubSyncKeepToken existing.id form.owner form.repo branch True
          | otherwise -> GitSync.updateGitHubSync encKey existing.id form.owner form.repo branch form.accessToken True
      Log.logTrace (bool "Created git sync config" "Updated git sync config" (isJust existingM)) (pid, Git.hostSlug host, form.owner, form.repo)
      addRespHeaders $ gitSyncSettingsView ctx.config ctx.env.hostUrl pid syncM


gitSyncSettingsDeleteH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsDeleteH pid = do
  ctx <- ask @Config.AuthContext
  whenJustM (GitSync.getGitHubSync pid) \existing -> do
    _ <- GitSync.deleteGitHubSync existing.id
    Log.logTrace "Deleted GitHub sync config" pid
  addRespHeaders $ gitSyncSettingsView ctx.config ctx.env.hostUrl pid Nothing


-- | Which hosts this deployment will connect to.
--
-- GitHub is always offered. The other three are behind @ENABLE_NON_GITHUB_GIT_HOSTS@ until
-- their request paths have been exercised against the real vendors — they are covered by
-- doctests and fixtures, which is not the same as having worked once. Returns 'Either' so the
-- handler can reuse it as a validation step.
offeredHosts :: Config.EnvConfig -> [Git.GitHost]
offeredHosts cfg = if cfg.enableNonGithubGitHosts then universe else [Git.GitHub]


gitSyncSettingsView :: Config.EnvConfig -> Text -> Projects.ProjectId -> Maybe GitSync.GitHubSync -> Html ()
gitSyncSettingsView cfg hostUrl pid syncM =
  div_ [class_ "space-y-6"] $ maybe (notConnectedView cfg actionUrl) (\sync -> connectedView sync actionUrl (webhookUrlFor hostUrl sync.host)) syncM
  where
    actionUrl = "/p/" <> pid.toText <> "/settings/git-sync"


-- | Where a host should send its pushes.
--
-- GitHub keeps the original path so hooks configured before this change keep working; every
-- other host gets the per-host route, which is what tells the handler whose signature scheme
-- to check.
webhookUrlFor :: Text -> Git.GitHost -> Text
webhookUrlFor hostUrl = \case
  Git.GitHub -> hostUrl <> "webhook/github"
  h -> hostUrl <> "webhook/git/" <> Git.hostSlug h


-- | What a host calls the credential you have to paste, and where to make one. Wrong-sounding
-- instructions are worse than none: a GitLab user told to "create a Personal Access Token with
-- Contents read/write" will look for a setting that does not exist.
hostTokenHelp :: Git.GitHost -> (Text, Text)
hostTokenHelp = \case
  Git.GitHub -> ("Personal access token", "Fine-grained token with Contents: Read and write on the repository.")
  Git.GitLab -> ("Project or personal access token", "Scope: api (or read_api plus write_repository).")
  Git.Gitea -> ("Access token", "Settings → Applications → Generate token, with repository read and write.")
  Git.Bitbucket -> ("Repository or workspace access token", "Scopes: repository, repository:write.")


notConnectedView :: Config.EnvConfig -> Text -> Html ()
notConnectedView cfg actionUrl = do
  -- GitHub App (primary)
  div_ [class_ "space-y-3"] do
    p_ [class_ "text-sm text-textWeak"] "Install the GitHub App to sync dashboards with your repository. Webhooks are configured automatically."
    a_ [href_ (actionUrl <> "/install"), class_ "btn btn-sm btn-primary gap-2"] do
      faSprite_ "github" "regular" "w-3.5 h-3.5"
      "Install GitHub App"

  -- Token connection: the only option on GitLab, Gitea and Bitbucket, and still available on
  -- GitHub for anyone who cannot install an App.
  div_ [class_ "pt-6 border-t border-strokeWeak"] do
    details_ [class_ "group/host"] do
      summary_ [class_ "text-xs font-medium text-textWeak cursor-pointer list-none flex items-center gap-1.5 hover:text-textStrong"] do
        faSprite_ "chevron-right" "solid" "w-3 h-3 transition-transform group-open/host:rotate-90"
        toHtml $ "Or connect " <> T.intercalate ", " (map Git.hostLabel (offeredHosts cfg)) <> " with a token"
      form_ [class_ "pt-4 space-y-3", hxPost_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content", hxIndicator_ "#indicator"] do
        div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
          -- Picking a host rewrites the token label and shows the server-URL field only for
          -- the hosts that can have one, so nobody is asked for a Bitbucket server address.
          formSelectField_ FieldSm "Git host" "host" True $ forM_ (offeredHosts cfg) \h ->
            let (tokenLabel, tokenHelp) = hostTokenHelp h
             in option_
                  ( [value_ (Git.hostSlug h), term "data-token-label" tokenLabel, term "data-token-help" tokenHelp, term "data-origin" (originMode h)]
                      <> [selected_ "" | h == Git.GitHub]
                  )
                  $ toHtml (Git.hostLabel h)
          formField_
            FieldSm
            def
              { placeholder = "https://gitlab.example.com"
              , extraAttrs =
                  [ [__| on load or change from #host
                                     set mode to #host.selectedOptions[0].dataset.origin
                                     then if mode is 'no' then add .hidden to closest <fieldset/> else remove .hidden from closest <fieldset/> end
                                     then set my required to (mode is 'required') |]
                  ]
              }
            "Server URL"
            "apiBase"
            False
            Nothing
          formField_ FieldSm def{placeholder = "acme-corp"} "Repository owner" "owner" True Nothing
          formField_ FieldSm def{placeholder = "observability-config"} "Repository name" "repo" True Nothing
          formField_ FieldSm def{placeholder = "leave blank to detect"} "Branch" "branch" False Nothing
          formField_
            FieldSm
            def
              { inputType = "password"
              , placeholder = "paste token"
              , extraAttrs = [[__| on load or change from #host put #host.selectedOptions[0].dataset.tokenLabel into the previous <label/> |]]
              }
            "Access token"
            "accessToken"
            True
            Nothing
        div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
          formField_ FieldSm def{placeholder = "monoscope"} "Folder in repo" "pathPrefix" False Nothing
          -- Required in the UI for every new connection: a webhook we cannot verify is a
          -- webhook anyone can forge into triggering a sync.
          formField_ FieldSm def{inputType = "password", placeholder = "shared secret for the webhook"} "Webhook secret" "webhookSecret" True Nothing
        p_ [class_ "text-xs text-textWeak", id_ "token-help", [__| on load or change from #host put #host.selectedOptions[0].dataset.tokenHelp into me |]] ""
        p_ [class_ "text-xs text-textWeak"] do
          "Dashboards are stored in "
          code_ [class_ "text-textBrand"] "dashboards/"
        button_ [class_ "btn btn-sm btn-outline gap-1", type_ "submit"] do
          "Connect with token"
          htmxIndicator_ "indicator" LdXS


connectedView :: GitSync.GitHubSync -> Text -> Text -> Html ()
connectedView sync actionUrl webhookUrl = do
  let isViaApp = isJust sync.installationId
  headerRow_ [] do
    div_ [class_ "flex items-center gap-2 text-sm"] do
      faSprite_ "circle-check" "solid" "w-3.5 h-3.5 text-iconSuccess"
      span_ [class_ "font-medium text-textStrong"] $ toHtml $ sync.owner <> "/" <> sync.repo
      span_ [class_ "text-textWeak"] $ toHtml $ "(" <> Git.hostLabel sync.host <> ", " <> sync.branch <> ", " <> (if isViaApp then "App" else "token") <> ")"
    connectionBadge_ "Connected"

  -- Repository settings
  form_ [class_ "space-y-4", hxPost_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content", hxIndicator_ "#indicator"] do
    -- An App installation already knows which repositories it reaches, so changing one is
    -- picking from that list — not retyping an owner and a name and finding out later that
    -- one of them was wrong. A PAT reaches whatever it was scoped to and we cannot enumerate
    -- it, so those keep the text boxes.
    if isViaApp
      then div_ [class_ "flex items-end gap-3"] do
        formField_ FieldSm def{value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
        a_ [href_ (actionUrl <> "/repos"), class_ "btn btn-sm btn-outline gap-1.5 shrink-0"] do
          faSprite_ "code-branch" "regular" "w-3 h-3"
          "Change repository"
        input_ [type_ "hidden", name_ "owner", value_ sync.owner]
        input_ [type_ "hidden", name_ "repo", value_ sync.repo]
        input_ [type_ "hidden", name_ "accessToken", value_ ""]
      else div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
        formField_ FieldSm def{value = sync.owner, placeholder = "acme-corp"} "Repository Owner" "owner" True Nothing
        formField_ FieldSm def{value = sync.repo, placeholder = "observability-config"} "Repository Name" "repo" True Nothing
        formField_ FieldSm def{value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
        formField_ FieldSm def{inputType = "password", placeholder = "Leave empty to keep current"} "Access Token" "accessToken" False Nothing
    formField_ FieldSm def{value = sync.pathPrefix, placeholder = "monoscope"} "Folder in repo" "pathPrefix" False Nothing
    p_ [class_ "text-xs text-textWeak"] do
      "Dashboards stored in "
      code_ [class_ "text-textBrand"] $ toHtml $ if T.null sync.pathPrefix then "dashboards/" else sync.pathPrefix <> "/dashboards/"

    -- Webhook URL
    div_ [class_ "pt-4 border-t border-strokeWeak space-y-2"] do
      headerRow_ [] do
        sectionLabel_ "Webhook URL"
        button_
          [ type_ "button"
          , class_ "btn btn-xs btn-ghost gap-1"
          , term "data-url" webhookUrl
          , [__| on click call navigator.clipboard.writeText(my @data-url)
                   then put 'Copied!' into the first <span/> in me
                   then wait 2s then put 'Copy' into the first <span/> in me |]
          ]
          do
            faSprite_ "copy" "regular" "w-3 h-3"
            span_ "Copy"
      div_ [class_ "bg-fillWeak rounded-lg px-3 py-1.5 font-mono text-xs text-textWeak break-all"] $ toHtml webhookUrl
      unless isViaApp $ p_ [class_ "text-xs text-textWeak"] "Add this to your repository for automatic syncing."

    -- Actions
    div_ [class_ "flex items-center justify-between pt-2"] do
      button_ [class_ "btn btn-sm btn-primary gap-1", type_ "submit"] do
        "Save"
        htmxIndicator_ "indicator" LdXS
      label_ [class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak", Lucid.for_ "disconnect-modal"] do
        faSprite_ "link-slash" "regular" "w-3 h-3"
        span_ "Disconnect"

  confirmModal_ "disconnect-modal" "Disconnect GitHub?" "This will stop syncing dashboards with your repository. Dashboards will remain unchanged." [hxDelete_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] "Disconnect"

  unless isViaApp $ div_ [class_ "pt-6 border-t border-strokeWeak"] do
    div_ [class_ "prose prose-sm max-w-none"] $ renderMarkdown $ setupInstructions sync.host webhookUrl


-- | Setup notes for a token connection, in the host's own vocabulary — the GitHub-only
-- walkthrough this replaced sent GitLab, Gitea and Bitbucket users looking for settings that
-- do not exist on their host.
setupInstructions :: Git.GitHost -> Text -> Text
setupInstructions host webhookUrl =
  let label = Git.hostLabel host
      (tokenLabel, tokenHelp) = hostTokenHelp host
   in [text|
## Setup Instructions

### 1. Create a $label $tokenLabel

$tokenHelp

### 2. Repository Structure

Create a `dashboards/` folder in your repository with YAML files:

```
your-repo/
├── dashboards/
│   ├── api-overview.yaml
│   ├── error-tracking.yaml
│   └── performance.yaml
└── README.md
```

### 3. Dashboard YAML Format

Each dashboard file should have this structure:

```yaml
title: API Overview
description: Monitor API health and performance
tags:
  - api
  - monitoring
widgets:
  - type: chart
    title: Request Count
    query: "| summarize count() by bin(timestamp, 1h)"
```

### 4. Set Up Webhook (Optional)

For automatic syncing when you push changes, add a webhook in your repository settings:

1. Set the payload URL to: `${webhookUrl}`
2. Set the content type to: `application/json`
3. Send push events only, signed with the webhook secret you pasted above

*Without a webhook, syncing happens on a schedule or can be triggered manually.*
|]


-- | Queue a git sync push for a dashboard if git sync is configured
queueGitSyncPush :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx ()
queueGitSyncPush pid dashboardId = do
  ctx <- ask @Config.AuthContext
  whenJustM (GitSync.getGitHubSync pid) \sync -> when sync.syncEnabled do
    liftIO $ withResource ctx.jobsPool \conn ->
      void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushDashboard pid (unUUIDId dashboardId)
    Log.logTrace "Queued git sync push for dashboard" (pid, dashboardId)


-- | Form for selecting a repo from GitHub App installation
data RepoSelectForm = RepoSelectForm
  { repoFullName :: Text -- "owner/repo"
  , branch :: Text
  , pathPrefix :: Maybe Text
  , installationId :: Int64 -- GitHub App installation ID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Meta-refresh redirect (no JS, escapes url properly) with a fallback link for no-refresh clients.
redirectPage :: Text -> Text -> Html ()
redirectPage msg url = div_ [class_ "p-8 text-center"] do
  meta_ [httpEquiv_ "refresh", content_ ("0;url=" <> url)]
  p_ [class_ "text-textWeak mb-4"] $ toHtml msg
  a_ [href_ url, class_ "text-textBrand underline"] "Continue"


-- | Store the grant an installation is, keyed by the account it covers. Called on every path
-- an installation can arrive by, because it is what makes source-code reading work without
-- also configuring dashboard sync. Best-effort: failing to name the account must not lose the
-- installation the user just completed, so it logs and moves on.
recordInstallation :: Projects.ProjectId -> Int64 -> ATAuthCtx ()
recordInstallation pid instId = do
  ctx <- ask @Config.AuthContext
  W.runHTTPWreq (GitSync.getInstallationAccount ctx.config.githubAppId ctx.config.githubAppPrivateKey instId) >>= \case
    Left err -> Log.logAttention "Could not record GitHub credential: installation account lookup failed" (pid, instId, err)
    Right account -> void $ GitSync.upsertGitHubCredential (encodeUtf8 ctx.config.apiKeyEncryptionSecretKey) pid Git.GitHub Nothing account (Just instId) Nothing


-- | Redirect to GitHub App installation page. @to@ names where the callback should land —
-- the same installation grants dashboard sync and source reading, so the flow has to come
-- back to whichever of the two the user started from.
githubAppInstallH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
githubAppInstallH pid toM = do
  ctx <- ask @Config.AuthContext
  addRespHeaders
    $ redirectPage "Redirecting to GitHub..."
    $ "https://github.com/apps/"
    <> ctx.config.githubAppName
    <> "/installations/new?state="
    <> pid.toText
    <> maybe "" (":" <>) (toM >>= guarded (not . T.null))


-- | Which half of the installation the user started from: the same grant powers dashboard sync
-- and source-code reading, and the callback has to land back where the flow began.
data InstallDest = InstallSync | InstallCode
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON)


-- | The project and landing page a callback's @state@ names, as written by 'githubAppInstallH'.
--
-- >>> snd <$> parseInstallState "0e26-4a1b:code"
-- Just InstallCode
-- >>> snd <$> parseInstallState "0e26-4a1b"
-- Just InstallSync
-- >>> fst <$> parseInstallState "0e26-4a1b:code"
-- Just "0e26-4a1b"
--
-- Anything else is not a state we wrote, and a project id guessed out of it would connect a
-- repository to the wrong project:
--
-- >>> parseInstallState ""
-- Nothing
parseInstallState :: Text -> Maybe (Text, InstallDest)
parseInstallState s = case T.breakOn ":" s of
  ("", _) -> Nothing
  (pid, dest) -> Just (pid, bool InstallSync InstallCode (T.drop 1 dest == "code"))


-- | Where an install lands once GitHub hands control back.
installReturnUrl :: Projects.ProjectId -> Int64 -> InstallDest -> Text
installReturnUrl pid instId = \case
  InstallCode -> "/p/" <> pid.toText <> "/settings/code-mappings"
  InstallSync -> "/p/" <> pid.toText <> "/settings/git-sync/repos?installationId=" <> show instId


-- | Handle callback from GitHub after App installation
githubAppCallbackH :: Maybe Int64 -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
githubAppCallbackH instIdM _setupAction stateM = do
  ctx <- ask @Config.AuthContext
  sess <- Projects.getSession
  let bwconf = (def :: BWConfig){sessM = Just sess, pageTitle = "GitHub Sync", config = ctx.config}
      parsed = do
        (pidTxt, dest) <- stateM >>= parseInstallState
        (,dest) <$> rightToMaybe (parseUrlPiece pidTxt)
  case (instIdM, parsed) of
    (Just instId, Just (pid, dest)) -> do
      existingM <- GitSync.getGitHubSync pid
      recordInstallation pid instId
      Log.logInfo (bool "GitHub App installed" "GitHub App already configured, updating installation" (isJust existingM)) (pid, instId, dest)
      addRespHeaders $ bodyWrapper bwconf $ redirectPage "GitHub App installed! Redirecting..." $ installReturnUrl pid instId dest
    (Just instId, Nothing) -> do
      -- No state param - user installed directly from GitHub, show project selector
      Log.logInfo "GitHub callback without state, showing project selector" instId
      projects <- Projects.selectProjectsForUser sess.persistentSession.userId
      addRespHeaders $ bodyWrapper bwconf $ projectSelectorView instId projects
    _ -> do
      Log.logAttention "Invalid GitHub callback" (instIdM, stateM)
      addRespHeaders $ bodyWrapper bwconf $ div_ [class_ "p-8 text-center text-textError"] "Invalid callback. Please try again."


-- | View for selecting a project when state is missing from callback
projectSelectorView :: Int64 -> [Projects.ProjectListItem] -> Html ()
projectSelectorView instId projects = div_ [class_ "min-h-screen bg-bgBase flex items-center justify-center p-8"] do
  div_ [class_ "surface-raised rounded-2xl p-6 max-w-md w-full space-y-4"] do
    div_ [class_ "flex items-center gap-3 mb-4"] do
      iconBadgeLg_ SuccessBadge "circle-check"
      div_ do
        h3_ [class_ "text-lg font-semibold text-textStrong"] "GitHub App Installed!"
        p_ [class_ "text-sm text-textWeak"] "Select a project to connect"
    if null projects
      then p_ [class_ "text-textWeak text-center py-4"] "No projects found. Create a project first."
      else div_ [class_ "space-y-2"] $ forM_ projects \proj ->
        a_ [href_ ("/p/" <> proj.id.toText <> "/settings/git-sync/repos?installationId=" <> show instId), class_ "flex items-center gap-3 p-3 rounded-lg border border-strokeWeak hover:border-strokeBrand-strong cursor-pointer block"] do
          iconBadge_ NeutralBadge "folder"
          span_ [class_ "font-medium text-textStrong"] $ toHtml proj.title


-- | List repositories from GitHub App installation (full page with BodyWrapper)
githubAppReposH :: Projects.ProjectId -> Maybe Int64 -> ATAuthCtx (RespHeaders (Html ()))
githubAppReposH pid instIdParam = withSettingsPage pid "Integrations" \_ -> do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  let instIdM = instIdParam <|> (syncM >>= (.installationId))
      errBox err = div_ [class_ "text-textError p-4"] $ toHtml err
  content <- case instIdM of
    Nothing -> pure $ errBox ("No GitHub App installation found" :: Text)
    Just instId -> do
      -- Idempotent, and this is the first point at which grant and project are known together
      -- when the callback carried no project and the user picked one here.
      recordInstallation pid instId
      W.runHTTPWreq $ either errBox (repoSelectionView pid instId) <$> runExceptT do
        tok <- ExceptT $ first ("Failed to get token: " <>) <$> GitSync.getInstallationToken ctx.config.githubAppId ctx.config.githubAppPrivateKey instId
        conn <- hoistEither $ Git.mkGitConn Git.GitHub Nothing tok.token
        ExceptT $ first ("Failed to list repos: " <>) <$> Git.listRepos conn
  pure $ div_ [class_ "w-full h-full overflow-y-auto"] $ section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "GitHub Sync"
      p_ [class_ "text-textWeak text-sm mt-1"] "Select a repository to sync dashboards with."
    div_ [id_ "git-sync-content", class_ "surface-raised rounded-2xl p-4"] content


-- | View for selecting a repository
repoSelectionView :: Projects.ProjectId -> Int64 -> [Git.GitRepo] -> Html ()
repoSelectionView pid instId repos = div_ [class_ "space-y-4"] do
  h3_ [class_ "text-lg font-medium text-textStrong"] "Select Repository"
  p_ [class_ "text-sm text-textWeak"] "Choose which repository to sync dashboards with."
  form_ [class_ "space-y-4", hxPost_ ("/p/" <> pid.toText <> "/settings/git-sync/select"), hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] do
    input_ [type_ "hidden", name_ "installationId", value_ (show instId)]
    repoFilter_ (length repos)
    -- Each repo carries its own default branch, so choosing one fills the branch field in
    -- rather than leaving "main" to be wrong on every repo that renamed its trunk.
    div_ [class_ "space-y-2 max-h-80 overflow-y-auto c-scroll", id_ "repo-list"] $ forM_ (zip [0 :: Int ..] repos) \(idx, repo) ->
      label_ [class_ "repo-row flex items-center gap-3 p-3 rounded-lg border border-strokeWeak hover:border-strokeBrand-strong cursor-pointer has-[:checked]:border-strokeBrand-strong has-[:checked]:bg-fillBrand-weak", term "data-name" (T.toLower repo.fullName)] do
        input_
          $ [ type_ "radio"
            , name_ "repoFullName"
            , value_ repo.fullName
            , class_ "radio radio-sm"
            , required_ ""
            , term "data-branch" repo.defaultBranch
            , [__| on change set #branch.value to my @data-branch |]
            ]
          <> [checked_ | idx == 0]
        span_ [class_ "font-medium text-textStrong truncate"] $ toHtml repo.fullName
        when repo.private $ span_ [class_ "shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak"] "private"
    -- The list is exactly what the installation was granted, so a repository that is not in it
    -- is a narrower grant rather than a missing row — and this is the only page that widens it.
    a_
      [ href_ (GitSync.installationSettingsUrl instId)
      , target_ "_blank"
      , rel_ "noopener"
      , class_ "text-xs text-textBrand underline inline-flex items-center gap-1"
      ]
      do
        "Repository missing? Add it to the installation on GitHub"
        faSprite_ "arrow-up-right-from-square" "regular" "w-2.5 h-2.5"
    div_ [class_ "grid grid-cols-2 gap-4"] do
      formField_ FieldSm def{value = maybe "main" (.defaultBranch) (viaNonEmpty head repos), placeholder = "main"} "Branch" "branch" False Nothing
      formField_ FieldSm def{placeholder = "monoscope"} "Folder in repo (optional)" "pathPrefix" False Nothing
    primaryButton_ [type_ "submit"] "Connect Repository"


-- | Type-to-filter over a repo list. Only rendered once the list is long enough that scanning
-- it is the slower option — an account with four repos does not need a search box.
repoFilter_ :: Int -> Html ()
repoFilter_ n = when (n > 8) $ label_ [class_ "input input-sm w-full flex items-center gap-2"] do
  faSprite_ "magnifying-glass" "regular" "w-3.5 h-3.5 text-iconNeutral shrink-0"
  input_
    [ type_ "search"
    , class_ "grow"
    , placeholder_ ("Filter " <> show n <> " repositories")
    , Aria.label_ "Filter repositories"
    , [__| on input
             for row in <.repo-row/>
               if row@data-name contains my value.toLowerCase() then remove .hidden from row else add .hidden to row end
             end |]
    ]


-- | Handle repo selection from GitHub App
githubAppSelectRepoH :: Projects.ProjectId -> RepoSelectForm -> ATAuthCtx (RespHeaders (Html ()))
githubAppSelectRepoH pid form = do
  ctx <- ask @Config.AuthContext
  let (ownerVal, repoVal) = Git.splitFullName form.repoFullName
      prefix = fromMaybe "" form.pathPrefix
  result <-
    GitSync.getGitHubSync pid >>= \case
      Nothing -> GitSync.insertGitHubAppSync pid form.installationId ownerVal repoVal form.branch prefix
      Just existing -> GitSync.updateGitHubSyncRepo existing.id ownerVal repoVal form.branch prefix
  recordInstallation pid form.installationId
  Log.logInfo "GitHub App repo selected" (pid, form.repoFullName)
  liftIO $ withResource ctx.jobsPool \conn ->
    void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushAllDashboards pid
  addRespHeaders $ gitSyncSettingsView ctx.config ctx.env.hostUrl pid result


-- | Whether this host's server-URL field is required, optional, or meaningless — read by the
-- hyperscript on the field so the form asks only for what the host can accept.
originMode :: Git.GitHost -> Text
originMode h = case Git.hostOriginRule h of
  Git.OriginOptional _ _ -> "optional"
  Git.OriginRequired _ -> "required"
  Git.OriginRejected _ -> "no"
