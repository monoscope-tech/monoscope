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
import Lucid.Hyperscript (__)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper, withSettingsPage)
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), confirmModal_, connectionBadge_, formField_, headerRow_, iconBadgeLg_, iconBadge_, primaryButton_, sectionLabel_, settingsH2_, settingsSection_)
import Pkg.DeriveUtils (UUIDId (..))
import Relude hiding (ask)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Utils (LoadingSize (..), faSprite_, htmxIndicator_, renderMarkdown)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (parseUrlPiece)
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


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


statusResp :: Text -> AE.Value
statusResp s = AE.object ["status" AE..= s]


errResp :: Text -> AE.Value
errResp msg = AE.object ["status" AE..= ("error" :: Text), "message" AE..= msg]


githubWebhookPostH :: Maybe Text -> Maybe Text -> ByteString -> ATBaseCtx AE.Value
githubWebhookPostH signatureM eventTypeM rawBody = case AE.eitherDecodeStrict @GitHubWebhookPayload rawBody of
  Left err -> errResp ("invalid JSON: " <> toText err) <$ Log.logAttention "GitHub webhook invalid JSON payload" err
  Right payload -> case payload.repository of
    Nothing -> errResp "missing repository" <$ Log.logAttention "GitHub webhook missing repository" ()
    Just repo -> do
      let (ownerName, repoName) = (repo.owner.login, repo.name)
      syncM <- GitSync.getGitHubSyncByRepo ownerName repoName
      case syncM of
        Nothing -> statusResp "ignored" <$ Log.logTrace "GitHub webhook for untracked repo" (ownerName, repoName)
        Just sync -> case validateWebhookSignature sync.webhookSecret signatureM rawBody of
          Left err -> errResp err <$ Log.logAttention "GitHub webhook signature validation failed" (ownerName, repoName, err)
          Right () -> do
            when (isNothing sync.webhookSecret) $ Log.logWarn "GitHub webhook accepted without secret validation" (ownerName, repoName)
            case eventTypeM of
              Just "push" -> do
                ctx <- ask @Config.AuthContext
                liftIO $ withResource ctx.jobsPool \conn ->
                  void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncFromRepo sync.projectId
                statusResp "ok" <$ Log.logTrace "Triggered git sync from webhook" (sync.projectId, ownerName, repoName)
              Just event -> AE.object ["status" AE..= ("ignored" :: Text), "event" AE..= event] <$ Log.logTrace "Ignoring GitHub event type" event
              Nothing -> statusResp "error" <$ Log.logInfo "GitHub webhook missing event type" ()


validateWebhookSignature :: Maybe Text -> Maybe Text -> ByteString -> Either Text ()
validateWebhookSignature Nothing _ _ = Right () -- No secret configured, skip validation (logged at call site for visibility)
validateWebhookSignature _ Nothing _ = Left "signature required but not provided"
validateWebhookSignature (Just secret) (Just sig) body =
  let expectedSig = "sha256=" <> decodeUtf8 (B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 secret :: ByteString) body :: HMAC.HMAC SHA256))
   in unless (BA.constEq (encodeUtf8 sig :: ByteString) (encodeUtf8 expectedSig :: ByteString)) $ Left "invalid signature"


gitSyncSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsGetH pid = do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  withSettingsPage pid "Integrations" \_ -> pure $ settingsSection_ do
    settingsH2_ "GitHub Sync"
    div_ [id_ "git-sync-content"] $ gitSyncSettingsView ctx.env.hostUrl pid syncM


gitSyncSettingsPostH :: Projects.ProjectId -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsPostH pid form = do
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  branch <- if T.null form.branch then GitSync.detectDefaultBranch form.accessToken form.owner form.repo else pure form.branch
  existingM <- GitSync.getGitHubSync pid
  syncM <- case existingM of
    Nothing -> GitSync.insertGitHubSync encKey pid form.owner form.repo branch form.accessToken Nothing (fromMaybe "" form.pathPrefix)
    Just existing
      | T.null form.accessToken -> GitSync.updateGitHubSyncKeepToken existing.id form.owner form.repo branch True
      | otherwise -> GitSync.updateGitHubSync encKey existing.id form.owner form.repo branch form.accessToken True
  Log.logTrace (maybe "Created GitHub sync config" (const "Updated GitHub sync config") existingM) (pid, form.owner, form.repo)
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid syncM


gitSyncSettingsDeleteH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsDeleteH pid = do
  ctx <- ask @Config.AuthContext
  whenJustM (GitSync.getGitHubSync pid) \existing -> do
    _ <- GitSync.deleteGitHubSync existing.id
    Log.logTrace "Deleted GitHub sync config" pid
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid Nothing


gitSyncSettingsView :: Text -> Projects.ProjectId -> Maybe GitSync.GitHubSync -> Html ()
gitSyncSettingsView hostUrl pid syncM =
  div_ [class_ "space-y-6"] $ maybe (notConnectedView actionUrl) (\sync -> connectedView sync actionUrl (hostUrl <> "webhook/github")) syncM
  where
    actionUrl = "/p/" <> pid.toText <> "/settings/git-sync"


notConnectedView :: Text -> Html ()
notConnectedView actionUrl = do
  -- GitHub App (primary)
  div_ [class_ "space-y-3"] do
    p_ [class_ "text-sm text-textWeak"] "Install the GitHub App to sync dashboards with your repository. Webhooks are configured automatically."
    a_ [href_ (actionUrl <> "/install"), class_ "btn btn-sm btn-primary gap-2"] do
      faSprite_ "github" "regular" "w-3.5 h-3.5"
      "Install GitHub App"

  -- PAT fallback
  div_ [class_ "pt-6 border-t border-strokeWeak"] do
    details_ [class_ "group"] do
      summary_ [class_ "text-xs font-medium text-textWeak cursor-pointer list-none flex items-center gap-1.5 hover:text-textStrong"] do
        faSprite_ "chevron-right" "solid" "w-3 h-3 transition-transform group-open:rotate-90"
        "Or connect with Personal Access Token"
      form_ [class_ "pt-4 space-y-3", hxPost_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content", hxIndicator_ "#indicator"] do
        div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
          formField_ FieldSm def{placeholder = "acme-corp"} "Repository Owner" "owner" True Nothing
          formField_ FieldSm def{placeholder = "observability-config"} "Repository Name" "repo" True Nothing
          formField_ FieldSm def{value = "main", placeholder = "main"} "Branch" "branch" True Nothing
          formField_ FieldSm def{inputType = "password", placeholder = "ghp_..."} "Access Token" "accessToken" True Nothing
        formField_ FieldSm def{placeholder = "monoscope"} "Path Prefix" "pathPrefix" False Nothing
        p_ [class_ "text-xs text-textWeak"] do
          "Token needs Contents read/write permission. Dashboards stored in "
          code_ [class_ "text-textBrand"] "dashboards/"
        button_ [class_ "btn btn-sm btn-outline gap-1", type_ "submit"] do
          "Connect with PAT"
          htmxIndicator_ "indicator" LdXS


connectedView :: GitSync.GitHubSync -> Text -> Text -> Html ()
connectedView sync actionUrl webhookUrl = do
  let isViaApp = isJust sync.installationId
  headerRow_ [] do
    div_ [class_ "flex items-center gap-2 text-sm"] do
      faSprite_ "circle-check" "solid" "w-3.5 h-3.5 text-iconSuccess"
      span_ [class_ "font-medium text-textStrong"] $ toHtml $ sync.owner <> "/" <> sync.repo
      span_ [class_ "text-textWeak"] $ toHtml $ "(" <> sync.branch <> ", " <> (if isViaApp then "GitHub App" else "PAT") <> ")"
    connectionBadge_ "Connected"

  -- Repository settings
  form_ [class_ "space-y-4", hxPost_ actionUrl, hxSwap_ "innerHTML", hxTarget_ "#git-sync-content", hxIndicator_ "#indicator"] do
    div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
      formField_ FieldSm def{value = sync.owner, placeholder = "acme-corp"} "Repository Owner" "owner" True Nothing
      formField_ FieldSm def{value = sync.repo, placeholder = "observability-config"} "Repository Name" "repo" True Nothing
      formField_ FieldSm def{value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
      unless isViaApp $ formField_ FieldSm def{inputType = "password", placeholder = "Leave empty to keep current"} "Access Token" "accessToken" False Nothing
    formField_ FieldSm def{value = sync.pathPrefix, placeholder = "monoscope"} "Path Prefix" "pathPrefix" False Nothing
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

  -- Setup instructions for PAT users
  unless isViaApp $ div_ [class_ "pt-6 border-t border-strokeWeak"] do
    div_ [class_ "prose prose-sm max-w-none"] $ renderMarkdown $ setupInstructions webhookUrl


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


-- | Redirect to GitHub App installation page
githubAppInstallH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
githubAppInstallH pid = do
  ctx <- ask @Config.AuthContext
  addRespHeaders $ redirectPage "Redirecting to GitHub..." $ "https://github.com/apps/" <> ctx.config.githubAppName <> "/installations/new?state=" <> pid.toText


-- | Handle callback from GitHub after App installation
githubAppCallbackH :: Maybe Int64 -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
githubAppCallbackH instIdM _setupAction stateM = do
  ctx <- ask @Config.AuthContext
  sess <- Projects.getSession
  let bwconf = (def :: BWConfig){sessM = Just sess, pageTitle = "GitHub Sync", config = ctx.config}
  case (instIdM, stateM >>= rightToMaybe . parseUrlPiece) of
    (Just instId, Just pid) -> do
      existingM <- GitSync.getGitHubSync pid
      Log.logInfo (maybe "GitHub App installed, awaiting repo selection" (const "GitHub App already configured, updating installation") existingM) (pid, instId)
      addRespHeaders $ bodyWrapper bwconf $ redirectPage "GitHub App installed! Redirecting..." $ "/p/" <> pid.toText <> "/settings/git-sync/repos?installationId=" <> show instId
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
        a_ [href_ ("/p/" <> proj.id.toText <> "/settings/git-sync/repos?installationId=" <> show instId), class_ "flex items-center gap-3 p-3 rounded-lg border border-borderWeak hover:border-borderBrand cursor-pointer block"] do
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
    Just instId ->
      W.runHTTPWreq $ either errBox (repoSelectionView pid instId) <$> runExceptT do
        tok <- ExceptT $ first ("Failed to get token: " <>) <$> GitSync.getInstallationToken ctx.config.githubAppId ctx.config.githubAppPrivateKey instId
        ExceptT $ first ("Failed to list repos: " <>) <$> GitSync.listInstallationRepos tok.token
  pure $ div_ [class_ "w-full h-full overflow-y-auto"] $ section_ [class_ "p-8 max-w-2xl mx-auto space-y-6"] do
    div_ [class_ "mb-2"] do
      h2_ [class_ "text-textStrong text-xl font-semibold"] "GitHub Sync"
      p_ [class_ "text-textWeak text-sm mt-1"] "Select a repository to sync dashboards with."
    div_ [id_ "git-sync-content", class_ "surface-raised rounded-2xl p-4"] content


-- | View for selecting a repository
repoSelectionView :: Projects.ProjectId -> Int64 -> [GitSync.GitHubRepo] -> Html ()
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
      formField_ FieldSm def{value = "main", placeholder = "main"} "Branch" "branch" False Nothing
      formField_ FieldSm def{placeholder = "monoscope"} "Path Prefix (optional)" "pathPrefix" False Nothing
    primaryButton_ [type_ "submit"] "Connect Repository"


-- | Handle repo selection from GitHub App
githubAppSelectRepoH :: Projects.ProjectId -> RepoSelectForm -> ATAuthCtx (RespHeaders (Html ()))
githubAppSelectRepoH pid form = do
  ctx <- ask @Config.AuthContext
  let (ownerVal, repoVal) = second (T.drop 1) $ T.breakOn "/" form.repoFullName
      prefix = fromMaybe "" form.pathPrefix
  result <-
    GitSync.getGitHubSync pid >>= \case
      Nothing -> GitSync.insertGitHubAppSync pid form.installationId ownerVal repoVal form.branch prefix
      Just existing -> GitSync.updateGitHubSyncRepo existing.id ownerVal repoVal form.branch prefix
  Log.logInfo "GitHub App repo selected" (pid, form.repoFullName)
  liftIO $ withResource ctx.jobsPool \conn ->
    void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushAllDashboards pid
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid result
