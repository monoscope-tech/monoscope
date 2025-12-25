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
  queueGitSyncPush,
) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEK
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Deriving.Aeson.Stock qualified as DAES
import Effectful.Reader.Static (ask)
import Lucid
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Relude hiding (ask)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addRespHeaders)
import Text.MMark qualified as MMark
import Web.FormUrlEncoded (FromForm)
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
   in if sig == expectedSig then Right () else Left "invalid signature"


gitSyncSettingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsGetH pid = do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid syncM


gitSyncSettingsPostH :: Projects.ProjectId -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsPostH pid form = do
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
  existingM <- GitSync.getGitHubSync pid
  syncM <- case existingM of
    Nothing -> do
      result <- GitSync.insertGitHubSync encKey pid form.owner form.repo form.branch form.accessToken Nothing
      Log.logInfo "Created GitHub sync config" (pid, form.owner, form.repo)
      pure result
    Just existing -> do
      result <-
        if T.null form.accessToken
          then GitSync.updateGitHubSyncKeepToken existing.id form.owner form.repo form.branch True
          else GitSync.updateGitHubSync encKey existing.id form.owner form.repo form.branch form.accessToken True
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


gitSyncSettingsView :: Text -> Projects.ProjectId -> Maybe GitSync.GitHubSync -> Html ()
gitSyncSettingsView hostUrl pid syncM = do
  let webhookUrl = hostUrl <> "webhook/github"
  div_ [class_ "space-y-6"] do
    -- Main settings card
    div_ [class_ "card bg-base-100 shadow-lg"] do
      div_ [class_ "card-body"] do
        h2_ [class_ "card-title"] "GitHub Sync"
        p_ [class_ "text-sm opacity-70"] "Sync dashboards with a GitHub repository. Changes pushed to the repo will be synced automatically."

        case syncM of
          Nothing -> do
            form_ [class_ "space-y-4 mt-4", method_ "POST", action_ ("/p/" <> pid.toText <> "/settings/git-sync")] do
              div_ [class_ "form-control"] do
                label_ [class_ "label"] $ span_ [class_ "label-text"] "Repository Owner"
                input_ [class_ "input input-bordered", type_ "text", name_ "owner", placeholder_ "acme-corp", required_ ""]
              div_ [class_ "form-control"] do
                label_ [class_ "label"] $ span_ [class_ "label-text"] "Repository Name"
                input_ [class_ "input input-bordered", type_ "text", name_ "repo", placeholder_ "observability-config", required_ ""]
              div_ [class_ "form-control"] do
                label_ [class_ "label"] $ span_ [class_ "label-text"] "Branch"
                input_ [class_ "input input-bordered", type_ "text", name_ "branch", value_ "main", required_ ""]
              div_ [class_ "form-control"] do
                label_ [class_ "label"] $ span_ [class_ "label-text"] "Access Token"
                input_ [class_ "input input-bordered", type_ "password", name_ "accessToken", placeholder_ "ghp_...", required_ ""]
                label_ [class_ "label"] $ span_ [class_ "label-text-alt opacity-70"] "Personal access token with Contents read/write permission"
              div_ [class_ "card-actions justify-end mt-2"] do
                button_ [class_ "btn btn-primary", type_ "submit"] "Connect Repository"
          Just sync -> do
            div_ [class_ "space-y-4 mt-4"] do
              div_ [class_ "flex items-center gap-2"] do
                span_ [class_ "badge badge-success"] "Connected"
                span_ [class_ "font-mono"] $ toHtml $ sync.owner <> "/" <> sync.repo
                span_ [class_ "text-sm opacity-70"] $ toHtml $ "(" <> sync.branch <> ")"

              -- Webhook URL with copy button
              div_ [class_ "bg-base-200 p-4 rounded-lg"] do
                div_ [class_ "flex items-center justify-between"] do
                  div_ do
                    span_ [class_ "text-sm font-semibold"] "Webhook URL"
                    div_ [class_ "font-mono text-sm mt-1", id_ "webhook-url"] $ toHtml webhookUrl
                  button_ [class_ "btn btn-sm btn-ghost", onclick_ ("navigator.clipboard.writeText('" <> webhookUrl <> "'); this.innerHTML='Copied!'; setTimeout(() => this.innerHTML='Copy', 2000)")] "Copy"

              form_ [class_ "space-y-4", method_ "POST", action_ ("/p/" <> pid.toText <> "/settings/git-sync")] do
                div_ [class_ "form-control"] do
                  label_ [class_ "label"] $ span_ [class_ "label-text"] "Repository Owner"
                  input_ [class_ "input input-bordered", type_ "text", name_ "owner", value_ sync.owner, required_ ""]
                div_ [class_ "form-control"] do
                  label_ [class_ "label"] $ span_ [class_ "label-text"] "Repository Name"
                  input_ [class_ "input input-bordered", type_ "text", name_ "repo", value_ sync.repo, required_ ""]
                div_ [class_ "form-control"] do
                  label_ [class_ "label"] $ span_ [class_ "label-text"] "Branch"
                  input_ [class_ "input input-bordered", type_ "text", name_ "branch", value_ sync.branch, required_ ""]
                div_ [class_ "form-control"] do
                  label_ [class_ "label"] $ span_ [class_ "label-text"] "Access Token"
                  input_ [class_ "input input-bordered", type_ "password", name_ "accessToken", placeholder_ "Leave empty to keep current", value_ ""]
                div_ [class_ "card-actions justify-between"] do
                  button_ [class_ "btn btn-error btn-outline", type_ "button", onclick_ ("fetch('/p/" <> pid.toText <> "/settings/git-sync', {method: 'DELETE'}).then(() => location.reload())")] "Disconnect"
                  button_ [class_ "btn btn-primary", type_ "submit"] "Update"

    -- Setup instructions card
    div_ [class_ "card bg-base-100 shadow-lg"] do
      div_ [class_ "card-body prose prose-sm max-w-none"] do
        renderMarkdown $ setupInstructions webhookUrl


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
queueGitSyncPush :: Projects.ProjectId -> UUID.UUID -> Maybe Text -> ATAuthCtx ()
queueGitSyncPush pid dashboardId mFilePath = do
  ctx <- ask @Config.AuthContext
  syncM <- GitSync.getGitHubSync pid
  case syncM of
    Nothing -> pass
    Just sync | not sync.syncEnabled -> pass
    Just _ -> do
      let filePath = fromMaybe ("dashboards/" <> UUID.toText dashboardId <> ".yaml") mFilePath
      liftIO $ withResource ctx.jobsPool \conn ->
        void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushDashboard pid dashboardId filePath
      Log.logInfo "Queued git sync push for dashboard" (pid, dashboardId)
