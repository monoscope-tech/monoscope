module Pages.GitSync (
  gitWebhookPostH,
  GitSyncSettings (..),
  GitSyncFailure (..),
  GitSyncRemoval (..),
  repositoryDashboardGetH,
  repositoryDashboardPostH,
  RepositoryDashboardGet (..),
  RepositoryDashboardForm (..),
  gitSyncSettingsPostH,
  gitSyncSettingsDeleteH,
  gitSyncSettingsUpdateH,
  gitSyncRepositoryDeleteH,
  gitSyncRepositoryRetryH,
  gitSyncRepositoryPauseH,
  GitSyncForm (..),
  RepoSelectForm (..),
  queueGitSyncPush,
  DashboardRepositoryGet (..),
  DashboardRepositoryForm (..),
  dashboardRepositoryGetH,
  dashboardRepositoryPostH,
  -- GitHub App handlers
  githubAppInstallH,
  githubAppCallbackH,
  githubAppReposH,
  githubAppSelectRepoH,
  GitHubConnect (..),
  ConnectFailure (..),
  SelectRejection (..),
  SyncPanels (..),
) where

import BackgroundJobs qualified
import Control.Monad.Trans.Except (withExceptT)
import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Htmx (hxIndicator_, hxPost_, hxSelect_, hxSwap_, hxTarget_)
import Models.Projects.DashboardTemplates (getDashboardTemplates, loadDashboardFromVM)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import Network.HTTP.Types (urlEncode)
import OpenTelemetry.Attributes qualified as Otel
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx)
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), colorChip_, connectionBadge_, copyButton_, copyField_, disclosure_, formField_, formSelectField_, headerRow_, sectionLabel_)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Pkg.Metrics qualified as Metrics
import Relude hiding (ask)
import Servant (ServerError (..), err401, err403, err404, err503)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBaseCtx, ActionResult, Done, Outcome (..), Refusal (..), Rendered (..), RespHeaders, act, addErrorToast, addRespHeaders, addSuccessToast, done, redirectCS)
import Utils (LoadingSize (..), faSprite_, hostPath, htmxIndicator_, renderMarkdown)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (parseUrlPiece)
import Web.Links (CookieProtectedRoutes (..), ProjectsRoutes' (..), urls)


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


-- | An inbound push from any host. The order is security-relevant: parse /only/ the repository
-- name, find its row, verify the signature, and only then queue a job. Queueing before
-- 'Git.verifyWebhook' returns is how a forged body gets to enqueue work.
gitWebhookPostH :: Git.GitHost -> Git.WebhookReq -> ATBaseCtx AE.Value
gitWebhookPostH Git.GitHub req | req.event == Just "pull_request" = do
  ctx <- ask @Config.AuthContext
  let secret = ctx.config.githubAppWebhookSecret
  if T.null secret
    then throwError err503{errBody = "GitHub App webhook secret is not configured"}
    else case Git.verifyWebhook Git.GitHub (Just secret) req of
      Left _ -> throwError err401{errBody = "Invalid GitHub signature"}
      Right () -> case ImpactReviews.decodeEvent req.body of
        Left err -> AE.object ["status" AE..= ("ignored" :: Text)] <$ Log.logInfo "Ignoring GitHub PR event" err
        Right event -> ImpactReviews.receiveEvent event $> AE.object ["status" AE..= ("ok" :: Text)]
gitWebhookPostH host req = case Git.parseWebhookRepo host req.body of
  Nothing -> errResp "missing repository" <$ Log.logAttention "Git webhook without a repository name" (Git.hostSlug host)
  Just fullName -> do
    let (ownerName, repoName) = Git.splitFullName fullName
    syncs <- GitSync.getGitSyncsByRepo host ownerName repoName
    outcomes <- forM syncs \sync -> case Git.verifyWebhook host sync.webhookSecret req of
      Left err -> do
        Metrics.bump Metrics.gitWebhookRejections [("host", Otel.toAttribute $ Git.hostSlug host), ("reason", Otel.toAttribute $ rejectionReason err)]
        Log.logAttention "Git webhook signature validation failed" (Git.hostSlug host, fullName, Git.webhookRejectionMessage err)
        pure $ Left err
      Right () -> do
        when (isNothing sync.webhookSecret) $ Log.logWarn "Git webhook accepted without a secret to verify against" (Git.hostSlug host, fullName)
        when (Git.isPushEvent host req.event && Git.webhookTargetsBranch host sync.branch req.body) do
          ctx <- ask @Config.AuthContext
          whenJust (Git.parseWebhookRevision host req.body) $ void . GitSync.setAnnouncedRevision sync.id
          BackgroundJobs.enqueueJobs ctx [BackgroundJobs.GitSyncRepository sync.projectId sync.id]
          Log.logTrace "Triggered git sync from webhook" (sync.projectId, Git.hostSlug host, fullName)
        pure $ Right ()
    pure $ case partitionEithers outcomes of
      (_, _ : _) -> statusResp $ bool "ignored" "ok" (Git.isPushEvent host req.event)
      (err : _, []) -> errResp $ Git.webhookRejectionMessage err
      ([], []) -> statusResp "ignored"
  where
    statusResp :: Text -> AE.Value
    statusResp s = AE.object ["status" AE..= s]
    errResp :: Text -> AE.Value
    errResp msg = AE.object ["status" AE..= ("error" :: Text), "message" AE..= msg]
    -- A metric dimension, so a handful of labels rather than one per rejection.
    rejectionReason :: Git.WebhookRejection -> Text
    rejectionReason = \case
      Git.SignatureMissing -> "missing"
      Git.WebhookIdMissing -> "missing"
      Git.TimestampMissing -> "missing"
      Git.MalformedSecret -> "malformed_secret"
      Git.SignatureMismatch -> "invalid"


-- | Connect or update the config-sync repository. The connection is vetted before anything is
-- written — storing first and discovering a bad origin on the next background job would leave a
-- row that looks connected and never syncs.
gitSyncSettingsPostH :: Projects.ProjectId -> GitSyncForm -> ATAuthCtx (ActionResult Git.GitConnError GitSyncSettings)
gitSyncSettingsPostH pid submitted = do
  requireGitWrite pid
  syncs <- GitSync.getGitSyncs pid
  let form = if fromMaybe Git.GitHub submitted.host == Git.GitHub then submitted{owner = T.toLower submitted.owner, repo = T.toLower submitted.repo} else submitted
      origin = rightToMaybe . Git.normalizeOrigin =<< mfilter (not . T.null . T.strip) form.apiBase
      matched = find (\s -> s.host == fromMaybe Git.GitHub form.host && s.apiBase == origin && s.owner == form.owner && s.repo == form.repo) syncs
      legacy = if T.null form.accessToken then case syncs of [sync] -> Just sync; _ -> Nothing else Nothing
  saveGitSyncH pid (matched <|> legacy) form


gitSyncSettingsUpdateH :: Projects.ProjectId -> GitSync.GitHubSyncId -> GitSyncForm -> ATAuthCtx (ActionResult Git.GitConnError GitSyncSettings)
gitSyncSettingsUpdateH pid sid form = do
  requireGitWrite pid
  sync <- GitSync.getGitSyncById pid sid >>= maybe (throwError err404) pure
  saveGitSyncH pid (Just sync) form{owner = sync.owner, repo = sync.repo, host = Just sync.host, apiBase = sync.apiBase}


-- | Why a connection was not saved: the form cannot describe one, so no host was asked.
instance Refusal Git.GitConnError where
  refusalMessage = Git.gitConnErrorMessage


saveGitSyncH :: Projects.ProjectId -> Maybe GitSync.GitHubSync -> GitSyncForm -> ATAuthCtx (ActionResult Git.GitConnError GitSyncSettings)
saveGitSyncH pid existingM form = act do
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      host = fromMaybe Git.GitHub form.host
      validation = case existingM of
        Nothing -> void $ Git.mkGitConn host form.apiBase form.accessToken
        Just _ -> bimap Git.BadEndpoint (const ()) $ Git.validateOrigin host form.apiBase
  -- The host/origin pair is vetted on its own: an update that keeps the stored token has no
  -- token to build a connection with, and 'Git.validateOrigin' is the half of the check that
  -- never needed one.
  forM validation \() -> do
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
      Nothing -> GitSync.insertGitHubSync encKey pid host apiBase form.owner form.repo branch (GitSync.PersonalToken form.accessToken) form.webhookSecret (fromMaybe "" form.pathPrefix)
      -- An empty token box means "keep the stored one", not "clear it".
      Just existing -> GitSync.updateGitHubSync encKey existing.id form.owner form.repo branch (guarded (not . T.null) form.accessToken) form.pathPrefix
    whenJust syncM \sync -> do
      when (maybe True (not . (.syncEnabled)) existingM) $ BackgroundJobs.enqueueJobs ctx [BackgroundJobs.GitSyncRepository pid sync.id]
      unless (T.null form.accessToken)
        $ void
        $ GitSync.upsertGitHubCredential encKey pid sync.host sync.apiBase sync.owner Nothing (Just form.accessToken)
    Log.logTrace (bool "Created git sync config" "Updated git sync config" (isJust existingM)) (pid, Git.hostSlug host, form.owner, form.repo)
    pure $ GitSyncSettings ctx.env.hostUrl pid syncM Nothing


-- | What disconnecting answered with. 'ChooseRepository' sends a project with several
-- connections to the list, since the id-less route cannot tell which one was meant.
data GitSyncRemoval = Disconnected GitSync.GitHubSyncId | Kept GitSyncSettings | ChooseRepository Projects.ProjectId


instance ToHtml GitSyncRemoval where
  toHtml = \case
    Disconnected sid -> div_ [id_ ("git-sync-" <> sid.toText), class_ "rounded-lg bg-fillWeak p-4 text-sm text-textWeak"] "Dashboard sync disconnected. Dashboards are retained as local dashboards."
    Kept settings -> toHtml settings
    ChooseRepository _ -> mempty
  toHtmlRaw = toHtml


instance Outcome GitSyncRemoval where
  outcomeBody = toHtml
  outcomeRedirect = \case
    ChooseRepository pid -> Just $ urls.repositoriesGet pid Nothing Nothing
    Disconnected _ -> Nothing
    Kept _ -> Nothing


gitSyncSettingsDeleteH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Done GitSyncRemoval))
gitSyncSettingsDeleteH pid = do
  requireGitWrite pid
  GitSync.getGitSyncs pid >>= \case
    [sync] -> gitSyncRepositoryDeleteH pid sync.id
    [] -> do
      ctx <- ask @Config.AuthContext
      done $ Kept $ GitSyncSettings ctx.env.hostUrl pid Nothing Nothing
    _ -> addErrorToast "Choose a repository to disconnect" Nothing >> done (ChooseRepository pid)


gitSyncRepositoryDeleteH :: Projects.ProjectId -> GitSync.GitHubSyncId -> ATAuthCtx (RespHeaders (Done GitSyncRemoval))
gitSyncRepositoryDeleteH pid sid = do
  requireGitWrite pid
  sync <- GitSync.getGitSyncById pid sid >>= maybe (throwError err404) pure
  ctx <- ask @Config.AuthContext
  GitSync.deleteGitHubSync sid >>= \case
    Left GitSync.ConnectionMissing -> throwError err404
    Left GitSync.SyncRunning -> done $ Kept $ GitSyncSettings ctx.env.hostUrl pid (Just sync) (Just SyncRunning)
    Right () -> done $ Disconnected sid


gitSyncRepositoryRetryH :: Projects.ProjectId -> GitSync.GitHubSyncId -> ATAuthCtx (RespHeaders (Done GitSyncSettings))
gitSyncRepositoryRetryH pid sid = do
  requireGitWrite pid
  sync <- GitSync.getGitSyncById pid sid >>= maybe (throwError err404) pure
  ctx <- ask @Config.AuthContext
  job <-
    if not sync.syncEnabled
      then pure $ Left SyncPaused
      else case maybe GitSync.ImportDashboards (.operation) sync.lastError of
        GitSync.ImportDashboards -> pure $ Right $ BackgroundJobs.GitSyncRepository pid sid
        GitSync.ExportDashboards -> pure $ Right $ BackgroundJobs.GitSyncPushRepository pid sid
        GitSync.ExportDashboard did -> do
          dashboard <- Dashboards.getDashboardByProjectId pid did
          pure $ if maybe False ((== Just sid) . (.gitSyncId)) dashboard then Right (BackgroundJobs.GitSyncPushDashboard pid did.unUUIDId) else Left DashboardUnavailable
  whenRight_ job \retry -> do
    BackgroundJobs.enqueueJobs ctx [retry]
    addSuccessToast "Retry queued" (Just "The error stays visible until this operation succeeds.")
  done $ GitSyncSettings ctx.env.hostUrl pid (Just sync) (leftToMaybe job)


gitSyncRepositoryPauseH :: Projects.ProjectId -> GitSync.GitHubSyncId -> ATAuthCtx (RespHeaders (Done GitSyncSettings))
gitSyncRepositoryPauseH pid sid = do
  requireGitWrite pid
  sync <- GitSync.pauseGitSync pid sid >>= maybe (throwError err404) pure
  ctx <- ask @Config.AuthContext
  done $ GitSyncSettings ctx.env.hostUrl pid (Just sync) Nothing


requireGitWrite :: Projects.ProjectId -> ATAuthCtx ()
requireGitWrite = void . ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit


-- | Why an action left a connection as it was; shown inside its panel.
data GitSyncFailure = SyncRunning | SyncPaused | DashboardUnavailable
  deriving stock (Eq, Show)


-- | One connection's settings panel, the morph target of every action on it.
data GitSyncSettings = GitSyncSettings
  { hostUrl :: Text
  , projectId :: Projects.ProjectId
  , sync :: Maybe GitSync.GitHubSync
  , failure :: Maybe GitSyncFailure
  }
  deriving (Outcome) via Rendered GitSyncSettings


instance ToHtml GitSyncSettings where
  toHtml = toHtml . gitSyncSettingsView
  toHtmlRaw = toHtml


gitSyncSettingsView :: GitSyncSettings -> Html ()
gitSyncSettingsView GitSyncSettings{hostUrl, projectId = pid, sync = syncM, failure} =
  div_ [id_ targetId, class_ "space-y-6"] do
    whenJust failure $ p_ [role_ "alert", class_ "text-sm text-textWarning"] . \case
      SyncRunning -> "A dashboard sync is running. Try disconnecting again when it finishes."
      SyncPaused -> "Sync is paused. Enable sync before retrying."
      DashboardUnavailable -> "This dashboard no longer belongs to this repository."
    maybe notConnectedView connectedView syncM
  where
    actionUrl = maybe (urls.gitSyncSettingsPost pid) (urls.gitSyncRepositoryUpdate pid . (.id)) syncM
    targetId = "git-sync-" <> maybe "new" (.id.toText) syncM
    notConnectedView :: Html ()
    notConnectedView = do
      div_ [class_ "space-y-3"] do
        p_ [class_ "text-sm text-textWeak"] "Install the GitHub App to sync dashboards with your repository. Webhooks are configured automatically."
        a_ [href_ (urls.githubAppInstall pid Nothing), class_ "btn btn-sm btn-primary gap-2"] do
          faSprite_ "github" "regular" "w-3.5 h-3.5"
          "Install GitHub App"

      -- Token connection: the only option on GitLab, Gitea and Bitbucket, and still available on
      -- GitHub for anyone who cannot install an App.
      div_ [class_ "pt-6 border-t border-strokeWeak"] do
        disclosure_ [] "text-xs font-medium text-textWeak hover:text-textStrong" (toHtml $ "Or connect " <> T.intercalate ", " (map Git.hostLabel universe) <> " with a token")
          $ form_ [class_ "pt-4 space-y-3", hxPost_ actionUrl, hxSwap_ "outerMorph", hxTarget_ ("#" <> targetId), hxIndicator_ ("#" <> targetId <> "-indicator")] do
            div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
              -- Picking a host rewrites the token label and shows the server-URL field only for
              -- the hosts that can have one, so nobody is asked for a Bitbucket server address.
              formSelectField_ FieldSm "Git host" "host" True $ forM_ (universe @Git.GitHost) \h ->
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
                      [ term "hx-live:required" "host.selectedOptions[0].dataset.origin == 'required'"
                      , term "hx-live" "closest('fieldset').class.hidden = host.selectedOptions[0].dataset.origin == 'no'"
                      ]
                  }
                "Server URL"
                "apiBase"
                False
                Nothing
              formField_ FieldSm def{placeholder = "acme-corp"} "Repository owner" "owner" True Nothing
              formField_ FieldSm def{placeholder = "observability-config"} "Repository name" "repo" True Nothing
              formField_ FieldSm def{inputId = Just (targetId <> "-branch"), placeholder = "leave blank to detect"} "Branch" "branch" False Nothing
              formField_
                FieldSm
                def
                  { inputType = "password"
                  , placeholder = "paste token"
                  , extraAttrs = [term "hx-live" "closest('fieldset').q('label').textContent = host.selectedOptions[0].dataset.tokenLabel"]
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
            p_ [class_ "text-xs text-textWeak", id_ "token-help", term "hx-live:text" "host.selectedOptions[0].dataset.tokenHelp"] ""
            p_ [class_ "text-xs text-textWeak"] do
              "Dashboards are stored in "
              code_ [class_ "text-textBrand"] "dashboards/"
            button_ [class_ "btn btn-sm gap-1", type_ "submit"] do
              "Connect with token"
              htmxIndicator_ (targetId <> "-indicator") LdXS

    connectedView :: GitSync.GitHubSync -> Html ()
    connectedView sync = do
      let isViaApp = isJust sync.installationId
          -- GitHub keeps the original path so hooks configured before this change keep working; every
          -- other host gets the per-host route, which tells the handler whose signature scheme to check.
          webhookUrl = hostUrl <> bool ("webhook/git/" <> Git.hostSlug sync.host) "webhook/github" (sync.host == Git.GitHub)
      headerRow_ [class_ "flex-wrap gap-3"] do
        div_ [class_ "min-w-0 flex-1 space-y-1"] do
          p_ [class_ "flex items-start gap-2 text-sm font-medium text-textStrong"] do
            faSprite_ "code-branch" "regular" "mt-0.5 w-3.5 h-3.5 shrink-0 text-iconNeutral"
            span_ [class_ "break-all"] $ toHtml $ sync.owner <> "/" <> sync.repo
          p_ [class_ "text-xs text-textWeak break-words"] $ toHtml $ Git.hostLabel sync.host <> " · " <> sync.branch <> " · " <> bool "Token account" "GitHub App" isViaApp
        if not sync.syncEnabled
          then colorChip_ NeutralBadge "circle-pause" "Paused"
          else if isJust sync.lastError then colorChip_ ErrorBadge "circle-exclamation" "Sync failed" else connectionBadge_ True
      unless sync.syncEnabled $ p_ [class_ "text-sm text-textWeak"] "Sync is paused. Existing dashboards and monitors remain available."
      whenJust sync.lastError \err -> div_ [class_ "space-y-2"] do
        p_ [role_ "status", class_ "text-sm text-textError break-words"] $ toHtml err.message
        case err.operation of
          GitSync.ImportDashboards -> p_ [class_ "text-xs text-textWeak"] "Some resource definitions could not be imported. Other changes still apply."
          GitSync.ExportDashboards -> p_ [class_ "text-xs text-textWeak"] "Configuration export failed. Retry to update this repository’s pull request."
          GitSync.ExportDashboard did -> a_ [href_ (urls.dashboardRepositoryGet pid did), class_ "inline-flex min-h-6 items-center text-sm text-textBrand underline underline-offset-2"] "View the dashboard that failed to export"
        when sync.syncEnabled $ button_ [type_ "button", term "hx-disable" "this", hxPost_ (urls.gitSyncRepositoryRetry pid sync.id), hxTarget_ ("#" <> targetId), hxSwap_ "outerMorph", hxIndicator_ ("#" <> targetId <> "-retry-indicator"), class_ "btn btn-sm btn-ghost gap-2"] do
          toHtml @Text $ case err.operation of GitSync.ImportDashboards -> "Retry import"; GitSync.ExportDashboard _ -> "Retry export"; GitSync.ExportDashboards -> "Retry export"
          htmxIndicator_ (targetId <> "-retry-indicator") LdXS

      form_ [class_ "space-y-4", hxPost_ actionUrl, hxSwap_ "outerMorph", hxTarget_ ("#" <> targetId), hxIndicator_ ("#" <> targetId <> "-indicator")] do
        -- An App installation knows which repositories it reaches, so changing one is picking from
        -- that list; a PAT's scope cannot be enumerated, so those keep the text boxes.
        if isViaApp
          then div_ [class_ "flex flex-wrap items-end gap-3"] do
            formField_ FieldSm def{inputId = Just (targetId <> "-branch"), value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
            input_ [type_ "hidden", name_ "owner", value_ sync.owner]
            input_ [type_ "hidden", name_ "repo", value_ sync.repo]
            input_ [type_ "hidden", name_ "accessToken", value_ ""]
          else div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
            input_ [type_ "hidden", name_ "owner", value_ sync.owner]
            input_ [type_ "hidden", name_ "repo", value_ sync.repo]
            formField_ FieldSm def{inputId = Just (targetId <> "-branch"), value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
            formField_ FieldSm def{inputId = Just (targetId <> "-accessToken"), inputType = "password", placeholder = "Leave empty to keep current"} "Access Token" "accessToken" False Nothing
        formField_ FieldSm def{inputId = Just (targetId <> "-pathPrefix"), value = sync.pathPrefix, placeholder = "monoscope"} "Folder in repo" "pathPrefix" False Nothing
        p_ [class_ "text-xs text-textWeak"] do
          "Definitions stored in dashboards/ and monitors/ under "
          code_ [class_ "text-textBrand"] $ toHtml $ if T.null sync.pathPrefix then "repository root" else sync.pathPrefix <> "/"

        div_ [class_ "pt-4 border-t border-strokeWeak space-y-2"] do
          sectionLabel_ "Webhook URL"
          copyField_ (targetId <> "-webhook-url") webhookUrl
          unless isViaApp $ p_ [class_ "text-xs text-textWeak"] "Add this to your repository for automatic syncing."

        div_ [class_ "flex flex-wrap items-center gap-2 pt-2"] do
          button_ [class_ "btn btn-sm btn-primary gap-1", type_ "submit"] do
            if sync.syncEnabled then "Save" else "Resume sync"
            htmxIndicator_ (targetId <> "-indicator") LdXS
          when sync.syncEnabled $ button_ [type_ "button", hxPost_ (urls.gitSyncRepositoryPause pid sync.id), hxTarget_ ("#" <> targetId), hxSwap_ "outerMorph", hxIndicator_ ("#" <> targetId <> "-pause-indicator"), class_ "btn btn-sm btn-ghost gap-1"] do
            "Pause sync"
            htmxIndicator_ (targetId <> "-pause-indicator") LdXS
      unless isViaApp $ details_ [class_ "pt-6 border-t border-strokeWeak"] do
        summary_ [class_ "cursor-pointer text-sm font-medium text-textStrong"] "Webhook and dashboard file setup"
        div_ [class_ "prose prose-sm max-w-none pt-4"] $ renderMarkdown $ setupInstructions sync.host webhookUrl

    -- Setup notes for a token connection, in the host's own vocabulary — the GitHub-only
    -- walkthrough this replaced sent GitLab, Gitea and Bitbucket users looking for settings that
    -- do not exist on their host.
    setupInstructions :: Git.GitHost -> Text -> Text
    setupInstructions host webhookUrl =
      let label = Git.hostLabel host
       in [text|
    ## Dashboard files

    Store YAML files in the configured `dashboards/` directory. For example:

    ```yaml
    title: API Overview
    widgets:
      - type: chart
        title: Request Count
        query: "| summarize count() by bin(timestamp, 1h)"
    ```

    ## Automatic sync

    In your $label repository settings, add a push webhook:

    1. Set its URL to `${webhookUrl}`.
    2. Set the content type to `application/json`.
    3. Use the webhook secret for this connection and send push events only.

    Monoscope imports the repository when you enable sync. The webhook keeps later
    commits in sync. Assign local dashboards to this repository to push their edits.
    |]

    -- Whether this host's server-URL field is required, optional, or meaningless — read by the
    -- hx-live expression on the field so the form asks only for what the host can accept.
    originMode :: Git.GitHost -> Text
    originMode h = case Git.hostOriginRule h of
      Git.OriginOptional _ _ -> "optional"
      Git.OriginRequired _ -> "required"
      Git.OriginRejected _ -> "no"

    -- What a host calls the credential you have to paste, and where to make one. Wrong-sounding
    -- instructions are worse than none: a GitLab user told to "create a Personal Access Token with
    -- Contents read/write" will look for a setting that does not exist.
    hostTokenHelp :: Git.GitHost -> (Text, Text)
    hostTokenHelp = \case
      Git.GitHub -> ("Personal access token", "Fine-grained token with Contents: Read and write on the repository.")
      Git.GitLab -> ("Project or personal access token", "Scope: api (or read_api plus write_repository).")
      Git.Gitea -> ("Access token", "Settings → Applications → Generate token, with repository read and write.")
      Git.Bitbucket -> ("Repository or workspace access token", "Scopes: repository, repository:write.")


-- | Queue a git sync push for a dashboard if git sync is configured
queueGitSyncPush :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx ()
queueGitSyncPush pid dashboardId = do
  ctx <- ask @Config.AuthContext
  dashboard <- Dashboards.getDashboardByProjectId pid dashboardId
  syncM <- maybe (pure Nothing) (GitSync.getGitSyncById pid) (dashboard >>= (.gitSyncId))
  whenJust syncM \sync -> when sync.syncEnabled do
    BackgroundJobs.enqueueJobs ctx [BackgroundJobs.GitSyncPushDashboard pid (unUUIDId dashboardId)]
    Log.logTrace "Queued git sync push for dashboard" (pid, dashboardId)


data RepositoryDashboardGet = RepositoryDashboardGet
  { repository :: GitSync.Repository
  , sync :: Maybe GitSync.GitHubSync
  , accounts :: [GitSync.GitHubCredential]
  , form :: RepositoryDashboardForm
  , hostUrl :: Text
  , permission :: Maybe ProjectMembers.Permissions
  , setupError :: Maybe Text
  }
  deriving stock (Show)


data RepositoryDashboardForm = RepositoryDashboardForm {credentialId :: Maybe GitSync.GitHubCredentialId, branch :: Text, pathPrefix :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


repositoryDashboardPage :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (PageCtx RepositoryDashboardGet)
repositoryDashboardPage pid rid = do
  (session, _, bw) <- mkPageCtx pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  accounts <- filter (\c -> (c.host, c.apiBase) == (repository.host, repository.apiBase)) <$> GitSync.getGitHubCredentials pid
  sync <- find (\s -> (s.host, s.apiBase, s.owner, s.repo) == (repository.host, repository.apiBase, repository.owner, repository.repo)) <$> GitSync.getGitSyncs pid
  ctx <- ask @Config.AuthContext
  repos <- maybe (pure Nothing) (liftIO . Cache.lookup ctx.repoListCache) repository.credentialId
  let branch = maybe "" (.defaultBranch) $ find ((== repository.owner <> "/" <> repository.repo) . (.fullName)) (fold repos)
  permission <- ProjectMembers.getUserPermission pid session.user.id
  pure $ PageCtx bw{prePageTitle = Just "Repositories", pageTitle = repository.owner <> "/" <> repository.repo, pageTitleUrl = Just $ urls.repositoryGet pid rid, pageTitleSuffix = Just "Configuration sync", menuItem = Just "Repositories"} RepositoryDashboardGet{repository, sync, accounts, form = RepositoryDashboardForm repository.credentialId branch Nothing, hostUrl = ctx.config.hostUrl, permission, setupError = Nothing}


repositoryDashboardGetH :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (RespHeaders (PageCtx RepositoryDashboardGet))
repositoryDashboardGetH pid rid = addRespHeaders =<< repositoryDashboardPage pid rid


repositoryDashboardPostH :: Projects.ProjectId -> GitSync.RepositoryId -> RepositoryDashboardForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryDashboardGet))
repositoryDashboardPostH pid rid form = do
  requireGitWrite pid
  page <- repositoryDashboardPage pid rid
  case page.content.sync of
    Just _ -> addRespHeaders page
    Nothing -> do
      ctx <- ask @Config.AuthContext
      result <- runExceptT do
        cid <- hoistEither $ maybeToRight "Choose an account for this repository." form.credentialId
        account <- lift (GitSync.getGitHubCredential (encodeUtf8 ctx.config.apiKeyEncryptionSecretKey) pid cid) >>= hoistEither . maybeToRight "This account is unavailable. Choose another account."
        unless ((account.host, account.apiBase) == (page.content.repository.host, page.content.repository.apiBase)) $ hoistEither $ Left "Choose an account on this repository's Git host."
        credentials <- hoistEither $ maybeToRight "Reconnect this account to enable configuration sync." $ GitSync.credentialCreds account
        when (isJust account.installationId && any T.null [ctx.config.githubAppWebhookSecret, ctx.config.githubAppId, ctx.config.githubAppPrivateKey])
          $ hoistEither
          $ Left "An administrator must configure the GitHub App and webhook before configuration sync can run."
        branch <-
          if T.null (T.strip form.branch)
            then do
              token <- ExceptT $ GitSync.githubToken ctx.config.githubAppId ctx.config.githubAppPrivateKey credentials
              conn <- hoistEither $ first Git.gitConnErrorMessage $ GitSync.credentialConn account token
              ExceptT $ first (const "Could not detect the default branch. Enter a branch or check this account’s repository access.") <$> Git.fetchDefaultBranch conn (Git.RepoRef page.content.repository.owner page.content.repository.repo "HEAD")
            else pure $ T.strip form.branch
        secret <-
          if isJust account.installationId
            then pure $ guarded (not . T.null) ctx.config.githubAppWebhookSecret
            else Just . show <$> lift UUID.genUUID
        lift $ GitSync.enableRepositoryDashboardSync pid rid cid branch (T.dropAround (== '/') $ T.strip $ fromMaybe "" form.pathPrefix) secret
      case result of
        Left err -> addRespHeaders page{content = page.content{setupError = Just err, form}}
        Right sync -> do
          whenJust sync \connection -> BackgroundJobs.enqueueJobs ctx [BackgroundJobs.GitSyncRepository pid connection.id]
          fresh <- repositoryDashboardPage pid rid
          addRespHeaders $ if isJust fresh.content.sync then fresh else fresh{content = fresh.content{setupError = Just "This account is no longer available. Choose another account and try again.", form}}


instance ToHtml RepositoryDashboardGet where
  toHtml page = section_ [class_ "w-full min-w-0 space-y-6 p-4 sm:p-6"] do
    let repository = page.repository
        dashboardsUrl = urls.repositoryDashboardPost repository.projectId repository.id
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Configuration sync"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Keep dashboard and monitor definitions alongside code. Monoscope edits open or update one configuration PR; merged target-branch changes update Monoscope."
    div_ [id_ "repository-dashboard-content", class_ "space-y-4 rounded-xl border border-strokeWeak bg-bgBase p-4 sm:p-5"] do
      whenJust page.setupError $ p_ [role_ "alert", class_ "text-sm text-textError"] . toHtml
      if not (maybe False (>= ProjectMembers.PEdit) page.permission)
        then do
          p_ [class_ "text-sm text-textWeak"] "A project editor can configure configuration sync for this repository."
          case page.sync of
            Nothing -> p_ [class_ "text-sm text-textWeak"] "Configuration sync is not configured."
            Just sync -> div_ [class_ "space-y-2 rounded-xl border border-strokeWeak p-4"] do
              p_ [class_ "text-sm text-textStrong"] $ if sync.syncEnabled then "Sync enabled" else "Sync paused"
              p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ sync.branch <> " · " <> GitSync.getDashboardsPath sync
              whenJust sync.lastError $ p_ [role_ "status", class_ "text-sm text-textError break-words"] . toHtml . (.message)
        else case page.sync of
          Just sync -> do
            toHtml $ GitSyncSettings page.hostUrl repository.projectId (Just sync) Nothing
            when (isNothing sync.installationId) $ whenJust sync.webhookSecret \secret -> div_ [class_ "space-y-2 border-t border-strokeWeak pt-4"] do
              formField_ FieldSm def{inputId = Just "repository-webhook-secret", inputType = "password", value = secret, extraAttrs = [readonly_ "true"]} "Webhook secret" "webhookSecret" False Nothing
              toHtml $ copyButton_ "btn btn-xs btn-ghost gap-1" "w-3 h-3" "my @data-secret" [term "data-secret" secret]
              p_ [class_ "text-xs text-textWeak"] "Use this secret when adding the push webhook to your Git host."
          Nothing | null page.accounts -> do
            p_ [class_ "text-sm text-textWeak"] "Connect an account with access to this repository to enable configuration sync."
            a_ [href_ (urls.repositoryConnectGet repository.projectId Nothing), class_ "btn btn-sm btn-primary"] "Connect an account"
          Nothing -> form_ [action_ dashboardsUrl, method_ "post", hxPost_ dashboardsUrl, hxTarget_ "#repository-dashboard-content", hxSelect_ "#repository-dashboard-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-dashboard-indicator", class_ "space-y-4"] do
            formSelectField_ FieldSm "Repository account" "credentialId" True do
              option_ ([value_ "", disabled_ "disabled"] <> [selected_ "selected" | isNothing page.form.credentialId]) "Choose an account"
              forM_ page.accounts \account -> option_ ([value_ account.id.toText] <> [selected_ "selected" | Just account.id == page.form.credentialId]) $ toHtml account.account
            div_ [class_ "grid gap-3 sm:grid-cols-2"] do
              formField_ FieldSm def{value = page.form.branch, placeholder = "Leave blank to detect"} "Branch" "branch" False Nothing
              formField_ FieldSm def{value = fromMaybe "" page.form.pathPrefix, placeholder = "monoscope"} "Folder in repo" "pathPrefix" False Nothing
            p_ [class_ "text-xs text-textWeak"] "This folder contains dashboards/ and monitors/. Move existing resources into the repository folder to include them."
            button_ [type_ "submit", class_ "btn btn-sm btn-primary gap-2"] do
              "Enable configuration sync"
              htmxIndicator_ "repository-dashboard-indicator" LdXS
  toHtmlRaw = toHtml


-- | Repository ownership for one dashboard, including assignment conflicts.
data DashboardRepositoryGet = DashboardRepositoryGet
  { dashboard :: Dashboards.DashboardVM
  , repositories :: [GitSync.GitHubSync]
  , assignmentError :: Maybe GitSync.DashboardRepositoryError
  , permission :: Maybe ProjectMembers.Permissions
  }
  deriving stock (Show)


newtype DashboardRepositoryForm = DashboardRepositoryForm {repositoryId :: GitSync.GitHubSyncId}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


instance ToHtml DashboardRepositoryGet where
  toHtml page = section_ [class_ "mx-auto max-w-3xl space-y-6 px-4 py-6 sm:px-8 sm:py-8"] do
    let dash = page.dashboard
        pid = dash.projectId
    a_ [href_ (urls.dashboardsGet pid dash.id Nothing Nothing Nothing Nothing []), class_ "inline-flex items-center gap-2 text-sm text-textBrand hover:underline"] do
      faSprite_ "arrow-left" "regular" "h-3 w-3"
      "Back to dashboard"
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Dashboard repository"
      p_ [class_ "text-sm text-textWeak break-words"] $ toHtml dash.title
    div_ [id_ "dashboard-repository-content", class_ "space-y-4"] do
      whenJust page.assignmentError \err -> p_ [role_ "alert", class_ "rounded-lg bg-fillError-weak p-3 text-sm text-textError"] $ toHtml @Text case err of
        GitSync.DashboardMissing -> "This dashboard is no longer available."
        GitSync.RepositoryMissing -> "That repository is no longer connected to this project. Choose another repository."
        GitSync.OwnedByRepository _ -> "This dashboard already belongs to another repository. Disconnect its dashboard sync before assigning it elsewhere."
        GitSync.FileOwnedByDashboard _ -> "Another dashboard already uses this file in the repository. Rename this dashboard or choose another folder before syncing."
      case dash.gitSyncId >>= \sid -> find ((== sid) . (.id)) page.repositories of
        Just repository -> div_ [class_ "rounded-xl border border-strokeWeak p-4 sm:p-5 space-y-3"] do
          colorChip_ SuccessBadge "circle-check" "Repository assigned"
          p_ [class_ "font-medium text-sm text-textStrong break-all"] $ toHtml $ repository.owner <> "/" <> repository.repo
          p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ fromMaybe (Git.hostLabel repository.host) repository.apiBase <> " · " <> repository.branch
          whenJust dash.filePath $ p_ [class_ "font-mono text-xs text-textWeak break-all"] . toHtml . (GitSync.getDashboardsPath repository <>)
          whenJust repository.lastError $ p_ [role_ "status", class_ "text-sm text-textError break-words"] . toHtml . (.message)
          if repository.syncEnabled
            then p_ [class_ "text-sm text-textWeak"] $ if isNothing dash.fileSha then "Waiting for the first push. Dashboard edits will sync to this repository." else "Dashboard edits sync to this repository."
            else p_ [class_ "text-sm text-textWarning"] "Sync is paused. Enable dashboard sync in Repositories to send edits."
          p_ [class_ "text-xs text-textWeak"] "Disconnect dashboard sync in Repositories to retain a local copy."
          a_ [href_ (urls.repositoriesGet pid Nothing Nothing <> "#git-sync-" <> repository.id.toText), class_ "btn btn-sm btn-ghost"] "View in Repositories"
        Nothing
          | not (maybe False (>= ProjectMembers.PEdit) page.permission) ->
              p_ [class_ "text-sm text-textWeak"] "A project editor can assign this dashboard to a repository."
        Nothing | null page.repositories -> do
          p_ [class_ "text-sm text-textWeak"] "Connect a repository to version dashboards alongside your team's code."
          a_ [href_ (urls.repositoriesGet pid Nothing Nothing), class_ "btn btn-sm btn-primary"] "Connect a repository"
        Nothing -> form_
          [ hxPost_ (urls.dashboardRepositoryPost pid dash.id)
          , hxTarget_ "#dashboard-repository-content"
          , hxSelect_ "#dashboard-repository-content"
          , hxSwap_ "outerMorph"
          , hxIndicator_ "#dashboard-repository-indicator"
          , class_ "rounded-xl border border-strokeWeak p-4 sm:p-5 space-y-4"
          ]
          do
            p_ [class_ "text-sm text-textWeak"] "This dashboard is local. Choose the repository that should own its files. Each repository keeps its own dashboards and sync status."
            formSelectField_ FieldSm "Repository" "repositoryId" True do
              option_ [value_ "", disabled_ "disabled", selected_ "selected"] "Choose a repository"
              forM_ page.repositories \repository ->
                option_ [value_ repository.id.toText]
                  $ toHtml
                  $ repository.owner
                  <> "/"
                  <> repository.repo
                  <> " · "
                  <> fromMaybe (Git.hostLabel repository.host) repository.apiBase
                  <> " · "
                  <> repository.branch
                  <> bool " · paused" "" repository.syncEnabled
            button_ [type_ "submit", class_ "btn btn-sm btn-primary gap-2"] do
              "Sync dashboard"
              htmxIndicator_ "dashboard-repository-indicator" LdXS
  toHtmlRaw = toHtml


dashboardRepositoryGetH :: Projects.ProjectId -> Dashboards.DashboardId -> ATAuthCtx (RespHeaders (PageCtx DashboardRepositoryGet))
dashboardRepositoryGetH pid did = dashboardRepositoryPageH pid did Nothing


dashboardRepositoryPageH :: Projects.ProjectId -> Dashboards.DashboardId -> Maybe GitSync.DashboardRepositoryError -> ATAuthCtx (RespHeaders (PageCtx DashboardRepositoryGet))
dashboardRepositoryPageH pid did assignmentError = do
  (session, _, bw) <- mkPageCtx pid
  dashboard <- Dashboards.getDashboardByProjectId pid did >>= maybe (throwError err404) pure
  repositories <- GitSync.getGitSyncs pid
  permission <- ProjectMembers.getUserPermission pid session.user.id
  addRespHeaders $ PageCtx bw{pageTitle = "Dashboard repository"} DashboardRepositoryGet{dashboard, repositories, assignmentError, permission}


dashboardRepositoryPostH :: Projects.ProjectId -> Dashboards.DashboardId -> DashboardRepositoryForm -> ATAuthCtx (RespHeaders (PageCtx DashboardRepositoryGet))
dashboardRepositoryPostH pid did form = do
  requireGitWrite pid
  dashboard <- Dashboards.getDashboardByProjectId pid did >>= maybe (throwError err404) pure
  assignmentError <-
    GitSync.assignDashboardRepository pid did form.repositoryId >>= \case
      Left err -> pure $ Just err
      Right () -> do
        when (isNothing dashboard.schema) do
          cfg <- (.config) <$> ask @Config.AuthContext
          templates <- getDashboardTemplates cfg
          void $ Dashboards.updateSchema did (fromMaybe def $ loadDashboardFromVM templates dashboard) Nothing
        queueGitSyncPush pid did
        pure Nothing
  dashboardRepositoryPageH pid did assignmentError


-- | Form for selecting repos from a GitHub App installation.
data RepoSelectForm = RepoSelectForm
  { repoFullName :: [Text] -- owner/repo selections
  , branch :: Text
  , pathPrefix :: Maybe Text
  , installationId :: Int64 -- GitHub App installation ID
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Where connecting a GitHub account goes next: on to GitHub or back into the project, or a
-- failure with a way back to the project (or to the project list when the attempt names none).
data GitHubConnect = Continue Text Text | ConnectFailed (Maybe Projects.ProjectId) ConnectFailure


data ConnectFailure = AppNotConfigured | AttemptMissing | AuthorizationFailed GitSync.InstallationError | ConnectionIncomplete
  deriving stock (Eq, Show)


instance ToHtml GitHubConnect where
  toHtml = \case
    -- Meta-refresh redirect (no JS, escapes url properly) with a fallback link for no-refresh clients.
    Continue msg url -> div_ [class_ "p-8 text-center"] do
      meta_ [httpEquiv_ "refresh", content_ ("0;url=" <> url)]
      p_ [class_ "text-textWeak mb-4"] $ toHtml msg
      a_ [href_ url, class_ "text-textBrand underline"] "Continue"
    ConnectFailed pidM failure -> div_ [class_ "mx-auto max-w-xl space-y-4 p-6"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Connect GitHub"
      p_ [role_ "alert", class_ "text-sm text-textError"] $ toHtml @Text case failure of
        AppNotConfigured -> "An administrator must configure GitHub user authorization before connecting an account."
        AttemptMissing -> "This connection request is missing, expired, or already used. Return to Repositories and connect the account again."
        AuthorizationFailed err -> GitSync.installationErrorMessage err
        ConnectionIncomplete -> "GitHub did not complete the connection. Return to Repositories and try again."
      a_ [href_ (maybe urls.projects.listGet (\pid -> urls.repositoryConnectGet pid Nothing) pidM), class_ "btn btn-sm btn-ghost"] $ if isJust pidM then "Back to repository connections" else "Back to projects"
  toHtmlRaw = toHtml


-- | Begin installation with an opaque state bound to the signed-in browser session.
githubAppInstallH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx GitHubConnect))
githubAppInstallH pid toM = do
  requireGitWrite pid
  ctx <- ask @Config.AuthContext
  session <- Projects.getSession
  (_, _, bw) <- mkPageCtx pid
  addRespHeaders
    . PageCtx bw{pageTitle = "Connect GitHub"}
    =<< if any T.null [ctx.config.githubClientId, ctx.config.githubClientSecret]
      then pure $ ConnectFailed (Just pid) AppNotConfigured
      else do
        nonce <- GitSync.createInstallationAttempt session.persistentSession.id (GitSync.InstallationAttempt pid (if toM == Just "code" then GitSync.InstallCode else GitSync.InstallSync) GitSync.InstallApp)
        pure $ Continue "Continue to GitHub to choose the account and repositories." $ "https://github.com/apps/" <> ctx.config.githubAppName <> "/installations/new?state=" <> UUID.toText nonce


githubAppCallbackH :: Maybe Int64 -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx GitHubConnect))
githubAppCallbackH instIdM _setupAction stateM codeM = do
  ctx <- ask @Config.AuthContext
  session <- Projects.getSession
  attemptM <- maybe (pure Nothing) (GitSync.consumeInstallationAttempt session.persistentSession.id) (stateM >>= rightToMaybe . parseUrlPiece)
  let callback = hostPath ctx.config.hostUrl $ urls.githubAppCallback Nothing Nothing Nothing Nothing
      escaped = decodeUtf8 . urlEncode True . encodeUtf8
  content <- case attemptM of
    Nothing -> pure $ ConnectFailed Nothing AttemptMissing
    Just attempt -> do
      requireGitWrite attempt.projectId
      let installation = case attempt.step of GitSync.InstallApp -> instIdM; GitSync.AuthorizeApp iid -> Just iid
      case (installation, codeM) of
        (Just iid, Just code) ->
          GitSync.authorizeGitHubInstallation (encodeUtf8 ctx.config.apiKeyEncryptionSecretKey) attempt.projectId ctx.config.githubAppId ctx.config.githubClientId ctx.config.githubClientSecret code callback iid <&> \case
            Left err -> ConnectFailed (Just attempt.projectId) (AuthorizationFailed err)
            Right () -> Continue "GitHub account verified. Choose the repositories to connect." $ urls.repositoryConnectGet attempt.projectId Nothing
        (Just iid, Nothing) | GitSync.InstallApp <- attempt.step -> do
          nonce <- GitSync.createInstallationAttempt session.persistentSession.id attempt{GitSync.step = GitSync.AuthorizeApp iid}
          pure $ Continue "Authorize your GitHub account to verify this installation." $ "https://github.com/login/oauth/authorize?client_id=" <> escaped ctx.config.githubClientId <> "&redirect_uri=" <> escaped callback <> "&state=" <> UUID.toText nonce
        _ -> pure $ ConnectFailed (Just attempt.projectId) ConnectionIncomplete
  addRespHeaders $ PageCtx (def :: BWConfig){sessM = Just session, pageTitle = "Connect GitHub", config = ctx.config} content


-- | List repositories from GitHub App installation (full page with BodyWrapper)
githubAppReposH :: Projects.ProjectId -> Maybe Int64 -> ATAuthCtx (RespHeaders (Html ()))
githubAppReposH pid instIdParam = do
  requireGitWrite pid
  accounts <- GitSync.getGitHubCredentials pid
  forM_ instIdParam $ \iid -> unless (any (\account -> account.host == Git.GitHub && isNothing account.apiBase && account.installationId == Just iid) accounts) $ throwError err403
  redirectCS $ urls.repositoryConnectGet pid Nothing
  addRespHeaders mempty


-- | Why selected repositories were not connected.
data SelectRejection = NoRepositorySelected | RepositoryListFailed Text | RepositoryUnavailable Text
  deriving stock (Eq, Show)


instance Refusal SelectRejection where
  refusalMessage = \case
    NoRepositorySelected -> "Select at least one repository"
    RepositoryListFailed _ -> "Could not connect repositories"
    RepositoryUnavailable _ -> "Could not connect repositories"
  refusalDetail = \case
    NoRepositorySelected -> Nothing
    RepositoryListFailed err -> Just err
    RepositoryUnavailable name -> Just $ "Repository access is unavailable: " <> name


newtype SyncPanels = SyncPanels [GitSyncSettings]
  deriving (Outcome) via Rendered SyncPanels


instance ToHtml SyncPanels where
  toHtml (SyncPanels panels) = div_ [class_ "space-y-6"] $ traverse_ toHtml panels
  toHtmlRaw = toHtml


-- | Handle repo selection from GitHub App
githubAppSelectRepoH :: Projects.ProjectId -> RepoSelectForm -> ATAuthCtx (ActionResult SelectRejection SyncPanels)
githubAppSelectRepoH pid form = act do
  requireGitWrite pid
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      selections = ordNub form.repoFullName
      creds = GitSync.AppInstallation form.installationId
  accounts <- GitSync.getGitHubCredentials pid
  account <- maybe (throwError err403) pure $ find (\c -> c.host == Git.GitHub && isNothing c.apiBase && c.installationId == Just form.installationId) accounts
  cached <- liftIO $ Cache.lookup ctx.repoListCache account.id
  branches <- runExceptT do
    repos <- case cached of
      Just available -> pure available
      Nothing -> withExceptT RepositoryListFailed do
        token <- ExceptT $ GitSync.githubToken ctx.config.githubAppId ctx.config.githubAppPrivateKey creds
        conn <- hoistEither $ first Git.gitConnErrorMessage $ Git.mkGitConn Git.GitHub Nothing token
        available <- ExceptT $ Git.listRepos conn
        lift $ liftIO $ Cache.insert ctx.repoListCache account.id available
        pure available
    selected <- forM selections \name -> do
      repo <- hoistEither $ maybeToRight (RepositoryUnavailable name) $ find ((== name) . (.fullName)) repos
      pure (name, if T.null (T.strip form.branch) then repo.defaultBranch else T.strip form.branch)
    hoistEither $ maybeToRight NoRepositorySelected $ nonEmpty selected
  forM branches \selected -> do
    syncs <- GitSync.getGitSyncs pid
    results <-
      catMaybes <$> forM (toList selected) \(name, branch) -> do
        let (ownerVal, repoVal) = Git.splitFullName name
        case find (\s -> s.host == Git.GitHub && isNothing s.apiBase && s.owner == ownerVal && s.repo == repoVal) syncs of
          Nothing -> GitSync.insertGitHubSync encKey pid Git.GitHub Nothing ownerVal repoVal branch creds (guarded (not . T.null) ctx.config.githubAppWebhookSecret) (fromMaybe "" form.pathPrefix)
          Just existing -> pure $ Just existing
    BackgroundJobs.enqueueJobs ctx $ map (BackgroundJobs.GitSyncRepository pid . (.id)) results
    pure $ SyncPanels [GitSyncSettings ctx.env.hostUrl pid (Just sync) Nothing | sync <- results]
