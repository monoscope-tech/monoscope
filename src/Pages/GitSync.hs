{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Pages.GitSync (
  githubWebhookPostH,
  GitHubWebhookPayload (..),
  GitHubRepo (..),
  GitHubOwner (..),
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
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.Default (def)
import Data.Effectful.Wreq qualified as W
import Data.Pool (withResource)
import Data.Text qualified as T
import Deriving.Aeson.Stock qualified as DAES
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxDelete_, hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Models.Projects.Dashboards qualified as Dashboards
import Pkg.DeriveUtils (UUIDId (..))
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pkg.GitHub qualified as GitHub
import Relude hiding (ask)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Text.MMark qualified as MMark
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (parseUrlPiece)
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


renderMarkdown :: Text -> Html ()
renderMarkdown md = case MMark.parse "" md of
  Left _ -> toHtml md
  Right doc -> toHtmlRaw $ MMark.render doc


data GitHubWebhookPayload = GitHubWebhookPayload
  { ref :: Maybe Text
  , repository :: Maybe GitHubRepo
  , pusher :: Maybe AE.Value
  , commits :: Maybe [AE.Value]
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake GitHubWebhookPayload


data GitHubRepo = GitHubRepo
  { fullName :: Text
  , name :: Text
  , owner :: GitHubOwner
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake GitHubRepo


data GitHubOwner = GitHubOwner
  { login :: Text
  , name :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAES.Snake GitHubOwner


data GitSyncForm = GitSyncForm
  { owner :: Text
  , repo :: Text
  , branch :: Text
  , accessToken :: Text
  , pathPrefix :: Maybe Text -- Optional folder prefix (e.g., "monoscope" -> monoscope/dashboards/)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


githubWebhookPostH :: Maybe Text -> Maybe Text -> ByteString -> ATBaseCtx AE.Value
githubWebhookPostH signatureM eventTypeM rawBody = do
  case AE.eitherDecodeStrict rawBody of
    Left err -> do
      Log.logAttention "GitHub webhook invalid JSON payload" err
      pure $ AE.object ["status" AE..= ("error" :: Text), "message" AE..= ("invalid JSON: " <> toText err)]
    Right payload -> handlePayload signatureM eventTypeM rawBody payload


handlePayload :: Maybe Text -> Maybe Text -> ByteString -> GitHubWebhookPayload -> ATBaseCtx AE.Value
handlePayload signatureM eventTypeM rawBody payload = do
  ctx <- ask @Config.AuthContext
  case payload.repository of
    Nothing -> do
      Log.logAttention "GitHub webhook missing repository" ()
      pure $ AE.object ["status" AE..= ("error" :: Text), "message" AE..= ("missing repository" :: Text)]
    Just repo -> do
      let ownerName = repo.owner.login
          repoName = repo.name
      syncM <- GitSync.getGitHubSyncByRepo ownerName repoName
      case syncM of
        Nothing -> do
          Log.logInfo "GitHub webhook for untracked repo" (ownerName, repoName)
          pure $ AE.object ["status" AE..= ("ignored" :: Text)]
        Just sync -> case validateWebhookSignature sync.webhookSecret signatureM rawBody of
          Left err -> do
            Log.logAttention "GitHub webhook signature validation failed" (ownerName, repoName, err)
            pure $ AE.object ["status" AE..= ("error" :: Text), "message" AE..= err]
          Right () -> do
            when (isNothing sync.webhookSecret) $ Log.logWarn "GitHub webhook accepted without secret validation" (ownerName, repoName)
            case eventTypeM of
              Just "push" -> do
                liftIO $ withResource ctx.jobsPool \conn ->
                  void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo sync.projectId
                Log.logInfo "Triggered git sync from webhook" (sync.projectId, ownerName, repoName)
                pure $ AE.object ["status" AE..= ("ok" :: Text)]
              Just event -> do
                Log.logInfo "Ignoring GitHub event type" event
                pure $ AE.object ["status" AE..= ("ignored" :: Text), "event" AE..= event]
              Nothing -> do
                Log.logInfo "GitHub webhook missing event type" ()
                pure $ AE.object ["status" AE..= ("error" :: Text)]


validateWebhookSignature :: Maybe Text -> Maybe Text -> ByteString -> Either Text ()
validateWebhookSignature Nothing _ _ = Right () -- No secret configured, skip validation (logged at call site for visibility)
validateWebhookSignature _ Nothing _ = Left "signature required but not provided"
validateWebhookSignature (Just secret) (Just sig) body =
  let expectedSig = "sha256=" <> decodeUtf8 (B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) body :: HMAC.HMAC SHA256))
   in if BA.constEq (encodeUtf8 sig :: ByteString) (encodeUtf8 expectedSig :: ByteString) then Right () else Left "invalid signature"


gitSyncSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsGetH pid = do
  (sess, project) <- Sessions.sessionAndProject pid
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "GitHub Sync", isSettingsPage = True, config = ctx.config}
  addRespHeaders $ bodyWrapper bwconf $ gitSyncSettingsPage ctx.env.hostUrl pid syncM


gitSyncSettingsPostH :: Projects.ProjectId -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsPostH pid form = do
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  -- Auto-detect branch if not specified or empty
  branch <-
    if T.null form.branch
      then GitSync.detectDefaultBranch form.accessToken form.owner form.repo
      else pure form.branch
  existingM <- GitSync.getGitHubSync pid
  syncM <- case existingM of
    Nothing -> do
      result <- GitSync.insertGitHubSync encKey pid form.owner form.repo branch form.accessToken Nothing (fromMaybe "" form.pathPrefix)
      Log.logInfo "Created GitHub sync config" (pid, form.owner, form.repo)
      pure result
    Just existing -> do
      result <-
        if T.null form.accessToken
          then GitSync.updateGitHubSyncKeepToken existing.id form.owner form.repo branch True
          else GitSync.updateGitHubSync encKey existing.id form.owner form.repo branch form.accessToken True
      Log.logInfo "Updated GitHub sync config" (pid, form.owner, form.repo)
      pure result
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid syncM


gitSyncSettingsDeleteH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsDeleteH pid = do
  ctx <- ask @Config.AuthContext
  existingM <- GitSync.getGitHubSync pid
  case existingM of
    Nothing -> pass
    Just existing -> do
      _ <- GitSync.deleteGitHubSync existing.id
      Log.logInfo "Deleted GitHub sync config" pid
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid Nothing


gitSyncSettingsPage :: Text -> Projects.ProjectId -> Maybe GitSync.GitHubSync -> Html ()
gitSyncSettingsPage hostUrl pid syncM = div_ [class_ "w-full h-full overflow-y-auto"] do
  section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "GitHub Sync"
      p_ [class_ "text-textWeak text-sm mt-1"] "Sync dashboards with a GitHub repository. Changes pushed to the repo will be synced automatically."
    div_ [id_ "git-sync-content"] $ gitSyncSettingsView hostUrl pid syncM


gitSyncSettingsView :: Text -> Projects.ProjectId -> Maybe GitSync.GitHubSync -> Html ()
gitSyncSettingsView hostUrl pid syncM = do
  let webhookUrl = hostUrl <> "webhook/github"
      actionUrl = "/p/" <> pid.toText <> "/settings/git-sync"
      installUrl = "/p/" <> pid.toText <> "/settings/git-sync/install"
      isViaApp = maybe False (isJust . (.installationId)) syncM
  div_ [class_ "space-y-6"] do
    case syncM of
      Nothing -> notConnectedView pid actionUrl installUrl
      Just sync -> connectedView hostUrl pid sync actionUrl webhookUrl isViaApp


-- | View when not connected - shows GitHub App as primary with PAT fallback
notConnectedView :: Projects.ProjectId -> Text -> Text -> Html ()
notConnectedView pid actionUrl installUrl = do
  -- GitHub App install card (primary option)
  div_ [class_ "surface-raised rounded-2xl p-6 space-y-4"] do
    div_ [class_ "flex items-center gap-3 mb-4"] do
      div_ [class_ "p-3 rounded-full bg-fillBrand-weak"] $ faSprite_ "github" "regular" "h-6 w-6 text-textBrand"
      div_ do
        h3_ [class_ "text-lg font-semibold text-textStrong"] "Connect to GitHub"
        p_ [class_ "text-sm text-textWeak"] "Sync dashboards with your repository automatically"
    p_ [class_ "text-sm text-textWeak"] "Install our GitHub App to connect your repository. Webhooks are configured automatically for instant syncing."
    a_ [href_ installUrl, class_ "btn btn-primary w-full gap-2"] do
      faSprite_ "github" "regular" "w-4 h-4"
      "Install GitHub App"

  -- PAT fallback (collapsible)
  div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
    div_ [class_ "collapse collapse-arrow"] do
      input_ [type_ "checkbox", class_ "peer"]
      div_ [class_ "collapse-title flex items-center gap-2 text-sm font-medium text-textWeak peer-checked:text-textStrong"] do
        faSprite_ "key" "regular" "w-4 h-4"
        "Or connect with Personal Access Token"
      div_ [class_ "collapse-content"] do
        form_ [class_ "pt-4 space-y-4", hxPost_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content", hxIndicator_ "#indicator"] do
          div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] do
            gitField "Repository Owner" "owner" True "" False "acme-corp"
            gitField "Repository Name" "repo" True "" False "observability-config"
            gitField "Branch" "branch" True "main" False "main"
            gitField "Access Token" "accessToken" True "" True "ghp_..."
          gitField "Path Prefix" "pathPrefix" False "" False "monoscope"
          p_ [class_ "text-xs text-textWeak"] do
            "Personal access token with Contents read/write permission. "
            "Path prefix is optional — dashboards are stored in "
            code_ [class_ "text-textBrand"] "dashboards/"
          div_ [class_ "flex justify-end"] do
            button_ [class_ "btn btn-sm btn-outline gap-1", type_ "submit"] do
              "Connect with PAT"
              span_ [class_ "htmx-indicator loading loading-dots loading-xs", id_ "indicator"] ""


-- | View when connected - shows repo info and webhook URL
connectedView :: Text -> Projects.ProjectId -> GitSync.GitHubSync -> Text -> Text -> Bool -> Html ()
connectedView hostUrl pid sync actionUrl webhookUrl isViaApp = do
  -- Connection status card
  div_ [class_ "surface-raised rounded-2xl p-4"] do
    div_ [class_ "flex items-center justify-between"] do
      div_ [class_ "flex items-center gap-3"] do
        div_ [class_ "p-2 rounded-full bg-fillSuccess-weak"] $ faSprite_ "circle-check" "regular" "h-4 w-4 text-textSuccess"
        div_ do
          h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml $ sync.owner <> "/" <> sync.repo
          p_ [class_ "text-xs text-textWeak"] do
            toHtml $ sync.branch <> " branch"
            span_ [class_ "mx-1"] "•"
            if isViaApp then "via GitHub App" else "via Personal Access Token"
      connectionStatus True

  -- Repository settings card (for updates)
  form_ [class_ "space-y-6", hxPost_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content", hxIndicator_ "#indicator"] do
    div_ [class_ "surface-raised rounded-2xl p-4 space-y-4"] do
      label_ [class_ "text-sm font-medium text-textStrong block"] "Repository Settings"
      div_ [class_ "grid grid-cols-1 gap-4 md:grid-cols-2"] do
        gitField "Repository Owner" "owner" True sync.owner False "acme-corp"
        gitField "Repository Name" "repo" True sync.repo False "observability-config"
        gitField "Branch" "branch" True sync.branch False "main"
        unless isViaApp $ gitField "Access Token" "accessToken" False "" True "Leave empty to keep current"
      gitField "Path Prefix" "pathPrefix" False sync.pathPrefix False "monoscope"
      p_ [class_ "text-xs text-textWeak"] do
        "Dashboards are stored in "
        code_ [class_ "text-textBrand"] $ toHtml $ if T.null sync.pathPrefix then "dashboards/" else sync.pathPrefix <> "/dashboards/"

    -- Webhook URL card
    div_ [class_ "surface-raised rounded-2xl p-4 space-y-2"] do
      div_ [class_ "flex items-center justify-between"] do
        div_ do
          label_ [class_ "text-sm font-medium text-textStrong block"] "Webhook URL"
          p_ [class_ "text-xs text-textWeak"] $ if isViaApp then "Configured automatically by GitHub App" else "Add this to your repository for automatic syncing"
        button_ [type_ "button", class_ "btn btn-sm btn-ghost gap-1", onclick_ ("navigator.clipboard.writeText('" <> webhookUrl <> "'); this.querySelector('span').textContent='Copied!'; setTimeout(() => this.querySelector('span').textContent='Copy', 2000)")] do
          faSprite_ "copy" "regular" "w-3 h-3"
          span_ "Copy"
      div_ [class_ "bg-fillWeak rounded-lg px-3 py-2 font-mono text-sm text-textWeak break-all"] $ toHtml webhookUrl

    -- Actions
    div_ [class_ "flex items-center justify-between"] do
      button_ [class_ "btn btn-sm btn-outline gap-1", type_ "submit"] do
        "Update Settings"
        span_ [class_ "htmx-indicator loading loading-dots loading-xs", id_ "indicator"] ""
      label_ [class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak", Lucid.for_ "disconnect-modal"] do
        faSprite_ "link-slash" "regular" "w-3 h-3"
        span_ "Disconnect"

  -- Disconnect modal
  input_ [type_ "checkbox", id_ "disconnect-modal", class_ "modal-toggle"]
  div_ [class_ "modal", role_ "dialog"] do
    div_ [class_ "modal-box p-6"] do
      div_ [class_ "flex items-start gap-3 mb-4"] do
        div_ [class_ "p-2 bg-fillError-weak rounded-full"] $ faSprite_ "triangle-alert" "regular" "h-5 w-5 text-textError"
        div_ do
          h3_ [class_ "text-lg font-semibold text-textStrong"] "Disconnect GitHub?"
          p_ [class_ "text-sm text-textWeak mt-1"] "This will stop syncing dashboards with your repository. Dashboards will remain unchanged."
      div_ [class_ "flex justify-end gap-2 mt-6"] do
        label_ [class_ "btn btn-sm btn-ghost", Lucid.for_ "disconnect-modal"] "Cancel"
        button_ [class_ "btn btn-sm bg-fillError-strong text-white hover:opacity-90", hxDelete_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] "Disconnect"
    label_ [class_ "modal-backdrop", Lucid.for_ "disconnect-modal"] ""

  -- Setup instructions for PAT users
  unless isViaApp $ div_ [class_ "surface-raised rounded-2xl p-4"] do
    div_ [class_ "prose prose-sm max-w-none"] $ renderMarkdown $ setupInstructions webhookUrl


gitField :: Text -> Text -> Bool -> Text -> Bool -> Text -> Html ()
gitField lbl name required defVal isPass placeholder =
  div_ [class_ "space-y-1.5"] do
    label_ [class_ "flex items-center gap-1 text-xs font-medium text-textWeak"] do
      toHtml lbl
      when required $ span_ [class_ "text-textError"] "*"
    input_ ([class_ "input input-bordered input-sm w-full", value_ defVal, name_ name, type_ $ if isPass then "password" else "text", placeholder_ placeholder] <> [required_ "true" | required])


connectionStatus :: Bool -> Html ()
connectionStatus connected = span_ [class_ $ "inline-flex items-center gap-1.5 rounded-full px-2.5 py-1 text-xs font-medium " <> if connected then "bg-fillSuccess-weak text-textSuccess" else "bg-fillWeak text-textWeak"] do
  faSprite_ (if connected then "circle-check" else "circle-info") "regular" "w-3 h-3"
  if connected then "Connected" else "Not connected"


setupInstructions :: Text -> Text
setupInstructions webhookUrl =
  [text|
## Setup Instructions

### 1. Create a GitHub Personal Access Token

Go to [GitHub Fine-grained tokens](https://github.com/settings/tokens?type=beta) and:

1. Click **Generate new token**
2. Set an expiration (or no expiration for long-term use)
3. Under **Repository access**, select the specific repository
4. Under **Permissions → Repository permissions**, set **Contents** to **Read and write**
5. Click **Generate token** and copy it

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

For automatic syncing when you push changes:

1. Go to your repository → **Settings** → **Webhooks** → **Add webhook**
2. Set **Payload URL** to: `${webhookUrl}`
3. Set **Content type** to: `application/json`
4. Select **Just the push event**
5. Click **Add webhook**

*Without a webhook, syncing happens on a schedule or can be triggered manually.*
|]


-- | Queue a git sync push for a dashboard if git sync is configured
queueGitSyncPush :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx ()
queueGitSyncPush pid dashboardId = do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  case syncM of
    Nothing -> pass
    Just sync | not sync.syncEnabled -> pass
    Just _ -> do
      liftIO $ withResource ctx.jobsPool \conn ->
        void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushDashboard pid (unUUIDId dashboardId)
      Log.logInfo "Queued git sync push for dashboard" (pid, dashboardId)


-- | Form for selecting a repo from GitHub App installation
data RepoSelectForm = RepoSelectForm
  { repoFullName :: Text -- "owner/repo"
  , branch :: Text
  , pathPrefix :: Maybe Text
  , installationId :: Int64 -- GitHub App installation ID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Redirect to GitHub App installation page
githubAppInstallH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
githubAppInstallH pid = do
  ctx <- ask @Config.AuthContext
  -- Store project ID in state parameter for callback
  let stateParam = pid.toText
      installUrl = "https://github.com/apps/" <> ctx.config.githubAppName <> "/installations/new?state=" <> stateParam
  -- Return a redirect page
  addRespHeaders $ div_ [class_ "p-8 text-center"] do
    p_ [class_ "text-textWeak mb-4"] "Redirecting to GitHub..."
    script_ $ "window.location.href = '" <> installUrl <> "';"


-- | Handle callback from GitHub after App installation
githubAppCallbackH :: Maybe Int64 -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
githubAppCallbackH instIdM _setupAction stateM = do
  ctx <- ask @Config.AuthContext
  sess <- Sessions.getSession
  let bwconf = (def :: BWConfig){sessM = Just sess, pageTitle = "GitHub Sync", config = ctx.config}
  case (instIdM, stateM >>= rightToMaybe . parseUrlPiece) of
    (Just instId, Just pid) -> do
      -- Check if sync already exists for this project
      existingM <- GitSync.getGitHubSync pid
      case existingM of
        Just _ -> Log.logInfo "GitHub App already configured, updating installation" (pid, instId)
        Nothing -> Log.logInfo "GitHub App installed, awaiting repo selection" (pid, instId)
      -- Redirect to repo selection page
      let reposUrl = "/p/" <> pid.toText <> "/settings/git-sync/repos?installationId=" <> show instId
      addRespHeaders $ bodyWrapper bwconf $ div_ [class_ "p-8 text-center"] do
        p_ [class_ "text-textWeak mb-4"] "GitHub App installed! Redirecting..."
        script_ $ "window.location.href = '" <> reposUrl <> "';"
    (Just instId, Nothing) -> do
      -- No state param - user installed directly from GitHub, show project selector
      Log.logInfo "GitHub callback without state, showing project selector" instId
      projects <- Projects.selectProjectsForUser sess.persistentSession.userId
      addRespHeaders $ bodyWrapper bwconf $ projectSelectorView instId projects
    _ -> do
      Log.logAttention "Invalid GitHub callback" (instIdM, stateM)
      addRespHeaders $ bodyWrapper bwconf $ div_ [class_ "p-8 text-center text-textError"] "Invalid callback. Please try again."


-- | View for selecting a project when state is missing from callback
projectSelectorView :: Int64 -> [Projects.Project'] -> Html ()
projectSelectorView instId projects = div_ [class_ "min-h-screen bg-bgBase flex items-center justify-center p-8"] do
  div_ [class_ "surface-raised rounded-2xl p-6 max-w-md w-full space-y-4"] do
    div_ [class_ "flex items-center gap-3 mb-4"] do
      div_ [class_ "p-3 rounded-full bg-fillSuccess-weak"] $ faSprite_ "circle-check" "regular" "h-6 w-6 text-textSuccess"
      div_ do
        h3_ [class_ "text-lg font-semibold text-textStrong"] "GitHub App Installed!"
        p_ [class_ "text-sm text-textWeak"] "Select a project to connect"
    if null projects
      then p_ [class_ "text-textWeak text-center py-4"] "No projects found. Create a project first."
      else div_ [class_ "space-y-2"] $ forM_ projects \proj ->
        a_ [href_ ("/p/" <> proj.id.toText <> "/settings/git-sync/repos?installationId=" <> show instId), class_ "flex items-center gap-3 p-3 rounded-lg border border-borderWeak hover:border-borderBrand cursor-pointer block"] do
          div_ [class_ "p-2 rounded-full bg-fillWeak"] $ faSprite_ "folder" "regular" "h-4 w-4 text-textWeak"
          span_ [class_ "font-medium text-textStrong"] $ toHtml proj.title


-- | List repositories from GitHub App installation (full page with BodyWrapper)
githubAppReposH :: Projects.ProjectId -> Maybe Int64 -> ATAuthCtx (RespHeaders (Html ()))
githubAppReposH pid instIdParam = do
  (sess, project) <- Sessions.sessionAndProject pid
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  let instIdM = instIdParam <|> (syncM >>= (.installationId))
  content <- case instIdM of
    Nothing -> pure $ div_ [class_ "text-textError p-4"] "No GitHub App installation found"
    Just instId -> W.runHTTPWreq do
      tokenResult <- GitHub.getInstallationToken ctx.config.githubAppId ctx.config.githubAppPrivateKey instId
      case tokenResult of
        Left err -> pure $ div_ [class_ "text-textError p-4"] $ toHtml $ "Failed to get token: " <> err
        Right tok -> do
          reposResult <- GitHub.listInstallationRepos tok.token
          case reposResult of
            Left err -> pure $ div_ [class_ "text-textError p-4"] $ toHtml $ "Failed to list repos: " <> err
            Right repos -> pure $ repoSelectionView pid instId repos
  let bwconf = (def :: BWConfig){sessM = Just sess, currProject = Just project, pageTitle = "GitHub Sync", isSettingsPage = True, config = ctx.config}
  addRespHeaders $ bodyWrapper bwconf $ repoSelectionPage content


-- | Page structure for repo selection
repoSelectionPage :: Html () -> Html ()
repoSelectionPage content = div_ [class_ "w-full h-full overflow-y-auto"] do
  section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "GitHub Sync"
      p_ [class_ "text-textWeak text-sm mt-1"] "Select a repository to sync dashboards with."
    div_ [id_ "git-sync-content", class_ "surface-raised rounded-2xl p-4"] content


-- | View for selecting a repository
repoSelectionView :: Projects.ProjectId -> Int64 -> [GitHub.GitHubRepo] -> Html ()
repoSelectionView pid instId repos = div_ [class_ "space-y-4"] do
  h3_ [class_ "text-lg font-medium text-textStrong"] "Select Repository"
  p_ [class_ "text-sm text-textWeak"] "Choose which repository to sync dashboards with:"
  form_ [class_ "space-y-4", hxPost_ ("/p/" <> pid.toText <> "/settings/git-sync/select"), hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] do
    input_ [type_ "hidden", name_ "installationId", value_ (show instId)]
    div_ [class_ "space-y-2"] $ forM_ (zip [0 :: Int ..] repos) \(idx, repo) -> label_ [class_ "flex items-center gap-3 p-3 rounded-lg border border-borderWeak hover:border-borderBrand cursor-pointer"] do
      input_ $ [type_ "radio", name_ "repoFullName", value_ repo.fullName, class_ "radio radio-sm", required_ ""] <> [checked_ | idx == 0]
      div_ do
        span_ [class_ "font-medium text-textStrong"] $ toHtml repo.fullName
        when repo.private $ span_ [class_ "ml-2 text-xs text-textWeak"] "(private)"
    div_ [class_ "grid grid-cols-2 gap-4"] do
      div_ [class_ "space-y-1.5"] do
        label_ [class_ "text-xs font-medium text-textWeak"] "Branch"
        input_ [class_ "input input-bordered input-sm w-full", name_ "branch", value_ "main", placeholder_ "main"]
      div_ [class_ "space-y-1.5"] do
        label_ [class_ "text-xs font-medium text-textWeak"] "Path Prefix (optional)"
        input_ [class_ "input input-bordered input-sm w-full", name_ "pathPrefix", placeholder_ "monoscope"]
    button_ [type_ "submit", class_ "btn btn-primary btn-sm"] "Connect Repository"


-- | Handle repo selection from GitHub App
githubAppSelectRepoH :: Projects.ProjectId -> RepoSelectForm -> ATAuthCtx (RespHeaders (Html ()))
githubAppSelectRepoH pid form = do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  -- Parse owner/repo from fullName
  let (ownerVal, repoVal) = case T.splitOn "/" form.repoFullName of
        [o, r] -> (o, r)
        _ -> (form.repoFullName, "")
      prefix = fromMaybe "" form.pathPrefix
  -- Insert or update the sync config
  result <- case syncM of
    Nothing -> GitSync.insertGitHubAppSync pid form.installationId ownerVal repoVal form.branch prefix
    Just existing -> GitSync.updateGitHubSyncRepo existing.id ownerVal repoVal form.branch prefix
  Log.logInfo "GitHub App repo selected" (pid, form.repoFullName)
  -- Push all existing dashboards to the repo
  liftIO $ withResource ctx.jobsPool \conn ->
    void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushAllDashboards pid
  -- Return updated settings view
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid result
