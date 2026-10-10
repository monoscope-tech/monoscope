module Pages.GitSync (
  gitWebhookPostH,
  gitSyncSettingsView,
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
) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.UUID qualified as UUID
import Data.Effectful.Wreq qualified as W
import Data.Pool (withResource)
import Data.Text qualified as T
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxDelete_, hxIndicator_, hxPost_, hxSelect_, hxSwap_, hxTarget_)
import Models.Projects.DashboardTemplates (getDashboardTemplates, loadDashboardFromVM)
import Models.Projects.Dashboards qualified as Dashboards
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import OpenTelemetry.Attributes qualified as Otel
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper, mkPageCtx, withSettingsPage)
import Pages.Components (BadgeColor (..), EmptyStateCfg (..), EmptyStateSize (..), FieldCfg (..), FieldSize (..), colorChip_, confirmModal_, connectionBadge_, copyButton_, emptyState_, filterInputAttr_, formField_, formSelectField_, headerRow_, iconBadgeLg_, iconBadge_, installationSettingsLink_, primaryButton_, sectionLabel_, settingsH2_, settingsSection_)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Pkg.Metrics qualified as Metrics
import Relude hiding (ask)
import Servant (ServerError (..), err401, err403, err404, err503)
import System.Config qualified as Config
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ATBaseCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
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
        Log.logAttention "Git webhook signature validation failed" (Git.hostSlug host, fullName, err)
        pure $ Left err
      Right () -> do
        when (isNothing sync.webhookSecret) $ Log.logWarn "Git webhook accepted without a secret to verify against" (Git.hostSlug host, fullName)
        when (Git.isPushEvent host req.event) do
          ctx <- ask @Config.AuthContext
          whenJust (Git.parseWebhookRevision host req.body) $ void . GitSync.setAnnouncedRevision sync.id
          liftIO $ withResource ctx.jobsPool \conn ->
            void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncRepository sync.projectId sync.id
          Log.logTrace "Triggered git sync from webhook" (sync.projectId, Git.hostSlug host, fullName)
        pure $ Right ()
    pure $ case partitionEithers outcomes of
      (_, _ : _) -> statusResp $ bool "ignored" "ok" (Git.isPushEvent host req.event)
      (err : _, []) -> errResp err
      ([], []) -> statusResp "ignored"
  where
    statusResp :: Text -> AE.Value
    statusResp s = AE.object ["status" AE..= s]
    errResp :: Text -> AE.Value
    errResp msg = AE.object ["status" AE..= ("error" :: Text), "message" AE..= msg]
    -- Collapse a verification failure into one of a handful of labels: this becomes a metric
    -- dimension, and 'Git.verifyWebhook' can only fail in these ways.
    rejectionReason :: Text -> Text
    rejectionReason err
      | "not provided" `T.isInfixOf` err = "missing"
      | "base64" `T.isInfixOf` err = "malformed_secret"
      | otherwise = "invalid"


-- | Connect or update the config-sync repository. The connection is vetted before anything is
-- written — storing first and discovering a bad origin on the next background job would leave a
-- row that looks connected and never syncs.
gitSyncSettingsPostH :: Projects.ProjectId -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsPostH pid form = do
  requireGitWrite pid
  syncs <- GitSync.getGitSyncs pid
  let origin = rightToMaybe . Git.normalizeOrigin =<< mfilter (not . T.null . T.strip) form.apiBase
      matched = find (\s -> s.host == fromMaybe Git.GitHub form.host && s.apiBase == origin && s.owner == form.owner && s.repo == form.repo) syncs
      legacy = if T.null form.accessToken then case syncs of [sync] -> Just sync; _ -> Nothing else Nothing
  saveGitSyncH pid (matched <|> legacy) form


gitSyncSettingsUpdateH :: Projects.ProjectId -> GitSync.GitHubSyncId -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsUpdateH pid sid form = do
  requireGitWrite pid
  sync <- GitSync.getGitSyncById pid sid >>= maybe (throwError err404) pure
  saveGitSyncH pid (Just sync) form{owner = sync.owner, repo = sync.repo, host = Just sync.host, apiBase = sync.apiBase}


saveGitSyncH :: Projects.ProjectId -> Maybe GitSync.GitHubSync -> GitSyncForm -> ATAuthCtx (RespHeaders (Html ()))
saveGitSyncH pid existingM form = do
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      host = fromMaybe Git.GitHub form.host
  -- The host/origin pair is vetted on its own: an update that keeps the stored token has no
  -- token to build a connection with, and 'Git.validateOrigin' is the half of the check that
  -- never needed one.
  case Git.validateOrigin host form.apiBase of
    Left err -> do
      addErrorToast ("Could not connect to " <> Git.hostLabel host) (Just err)
      addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid existingM
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
        Nothing -> GitSync.insertGitHubSync encKey pid host apiBase form.owner form.repo branch (GitSync.PersonalToken form.accessToken) form.webhookSecret (fromMaybe "" form.pathPrefix)
        -- An empty token box means "keep the stored one", not "clear it".
        Just existing -> GitSync.updateGitHubSync encKey existing.id form.owner form.repo branch (guarded (not . T.null) form.accessToken) form.pathPrefix
      whenJust syncM \sync -> do
        when (maybe False (not . (.syncEnabled)) existingM) $ liftIO $ withResource ctx.jobsPool \conn ->
          void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncRepository pid sync.id
        unless (T.null form.accessToken)
          $ void
          $ GitSync.upsertGitHubCredential encKey pid sync.host sync.apiBase sync.owner Nothing (Just form.accessToken)
      Log.logTrace (bool "Created git sync config" "Updated git sync config" (isJust existingM)) (pid, Git.hostSlug host, form.owner, form.repo)
      addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid syncM


gitSyncSettingsDeleteH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncSettingsDeleteH pid = do
  requireGitWrite pid
  ctx <- ask @Config.AuthContext
  whenJustM (GitSync.getGitHubSync pid) \existing -> do
    _ <- GitSync.deleteGitHubSync existing.id
    Log.logTrace "Deleted GitHub sync config" pid
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid Nothing


gitSyncRepositoryDeleteH :: Projects.ProjectId -> GitSync.GitHubSyncId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncRepositoryDeleteH pid sid = do
  requireGitWrite pid
  _ <- GitSync.getGitSyncById pid sid >>= maybe (throwError err404) pure
  void $ GitSync.deleteGitHubSync sid
  addRespHeaders $ div_ [id_ ("git-sync-" <> sid.toText), class_ "rounded-lg bg-fillWeak p-4 text-sm text-textWeak"] "Dashboard sync disconnected. Dashboards are retained as local dashboards."


gitSyncRepositoryRetryH :: Projects.ProjectId -> GitSync.GitHubSyncId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncRepositoryRetryH pid sid = do
  requireGitWrite pid
  sync <- GitSync.getGitSyncById pid sid >>= maybe (throwError err404) pure
  ctx <- ask @Config.AuthContext
  if sync.syncEnabled
    then do
      liftIO $ withResource ctx.jobsPool \conn -> void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncRepository pid sid
      addSuccessToast "Import queued" (Just "The current error stays visible until the dashboard import succeeds.")
    else addErrorToast "Sync is paused" (Just "Enable sync before retrying.")
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid (Just sync)


gitSyncRepositoryPauseH :: Projects.ProjectId -> GitSync.GitHubSyncId -> ATAuthCtx (RespHeaders (Html ()))
gitSyncRepositoryPauseH pid sid = do
  requireGitWrite pid
  sync <- GitSync.pauseGitSync pid sid >>= maybe (throwError err404) pure
  ctx <- ask @Config.AuthContext
  addRespHeaders $ gitSyncSettingsView ctx.env.hostUrl pid (Just sync)


requireGitWrite :: Projects.ProjectId -> ATAuthCtx ()
requireGitWrite pid = do
  (session, _) <- Projects.sessionAndProject pid
  permission <- ProjectMembers.getUserPermission pid session.user.id
  unless (maybe False (>= ProjectMembers.PEdit) permission) $ throwError err403


gitSyncSettingsView :: Text -> Projects.ProjectId -> Maybe GitSync.GitHubSync -> Html ()
gitSyncSettingsView hostUrl pid syncM =
  div_ [id_ targetId, class_ "space-y-6"] $ maybe notConnectedView (\sync -> connectedView sync (webhookUrlFor sync.host)) syncM
  where
    baseUrl = "/p/" <> pid.toText <> "/settings/git-sync"
    actionUrl = baseUrl <> maybe "" (\s -> "/" <> s.id.toText) syncM
    targetId = "git-sync-" <> maybe "new" (.id.toText) syncM
    -- GitHub keeps the original path so hooks configured before this change keep working; every
    -- other host gets the per-host route, which tells the handler whose signature scheme to check.
    webhookUrlFor :: Git.GitHost -> Text
    webhookUrlFor = \case
      Git.GitHub -> hostUrl <> "webhook/github"
      h -> hostUrl <> "webhook/git/" <> Git.hostSlug h
    notConnectedView :: Html ()
    notConnectedView = do
      -- GitHub App (primary)
      div_ [class_ "space-y-3"] do
        p_ [class_ "text-sm text-textWeak"] "Install the GitHub App to sync dashboards with your repository. Webhooks are configured automatically."
        a_ [href_ (baseUrl <> "/install"), class_ "btn btn-sm btn-primary gap-2"] do
          faSprite_ "github" "regular" "w-3.5 h-3.5"
          "Install GitHub App"

      -- Token connection: the only option on GitLab, Gitea and Bitbucket, and still available on
      -- GitHub for anyone who cannot install an App.
      div_ [class_ "pt-6 border-t border-strokeWeak"] do
        details_ [class_ "group/host"] do
          summary_ [class_ "text-xs font-medium text-textWeak cursor-pointer list-none flex items-center gap-1.5 hover:text-textStrong"] do
            faSprite_ "chevron-right" "solid" "w-3 h-3 transition-transform group-open/host:rotate-90"
            toHtml $ "Or connect " <> T.intercalate ", " (map Git.hostLabel universe) <> " with a token"
          form_ [class_ "pt-4 space-y-3", hxPost_ actionUrl, hxSwap_ "outerMorph", hxTarget_ ("#" <> targetId), hxIndicator_ ("#" <> targetId <> "-indicator")] do
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
              formField_ FieldSm def{id = Just (targetId <> "-branch"), placeholder = "leave blank to detect"} "Branch" "branch" False Nothing
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

    connectedView :: GitSync.GitHubSync -> Text -> Html ()
    connectedView sync webhookUrl = do
      let isViaApp = isJust sync.installationId
      headerRow_ [class_ "flex-wrap gap-3"] do
        div_ [class_ "min-w-0 flex-1 space-y-1"] do
          p_ [class_ "flex items-start gap-2 text-sm font-medium text-textStrong"] do
            faSprite_ "code-branch" "regular" "mt-0.5 w-3.5 h-3.5 shrink-0 text-iconNeutral"
            span_ [class_ "break-all"] $ toHtml $ sync.owner <> "/" <> sync.repo
          p_ [class_ "text-xs text-textWeak break-words"] $ toHtml $ Git.hostLabel sync.host <> " · " <> sync.branch <> " · " <> (if isViaApp then "GitHub App" else "Token account")
        if not sync.syncEnabled
          then connectionBadge_ "Paused"
          else if isJust sync.lastError then colorChip_ "text-textError bg-fillError-weak" "circle-exclamation" "Sync failed" else connectionBadge_ "Connected"
      unless sync.syncEnabled $ p_ [class_ "text-sm text-textWeak"] "Sync is paused. Existing dashboards remain available."
      whenJust sync.lastError \err -> div_ [class_ "space-y-2"] do
        p_ [role_ "status", class_ "text-sm text-textError break-words"] $ toHtml err
        when sync.syncEnabled $ button_ [type_ "button", hxPost_ (actionUrl <> "/retry"), hxTarget_ ("#" <> targetId), hxSwap_ "outerMorph", hxIndicator_ ("#" <> targetId <> "-retry-indicator"), class_ "btn btn-sm btn-ghost gap-2"] do
          "Retry import"
          htmxIndicator_ (targetId <> "-retry-indicator") LdXS

      -- Repository settings
      form_ [class_ "space-y-4", hxPost_ actionUrl, hxSwap_ "outerMorph", hxTarget_ ("#" <> targetId), hxIndicator_ ("#" <> targetId <> "-indicator")] do
        -- An App installation knows which repositories it reaches, so changing one is picking from
        -- that list; a PAT's scope cannot be enumerated, so those keep the text boxes.
        if isViaApp
          then div_ [class_ "flex flex-wrap items-end gap-3"] do
            formField_ FieldSm def{id = Just (targetId <> "-branch"), value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
            a_ [href_ ("/p/" <> pid.toText <> "/repositories/connect"), class_ "btn btn-sm gap-1.5 shrink-0"] do
              faSprite_ "code-branch" "regular" "w-3 h-3"
              "Add repository"
            input_ [type_ "hidden", name_ "owner", value_ sync.owner]
            input_ [type_ "hidden", name_ "repo", value_ sync.repo]
            input_ [type_ "hidden", name_ "accessToken", value_ ""]
          else div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2"] do
            input_ [type_ "hidden", name_ "owner", value_ sync.owner]
            input_ [type_ "hidden", name_ "repo", value_ sync.repo]
            formField_ FieldSm def{id = Just (targetId <> "-branch"), value = sync.branch, placeholder = "main"} "Branch" "branch" True Nothing
            formField_ FieldSm def{id = Just (targetId <> "-accessToken"), inputType = "password", placeholder = "Leave empty to keep current"} "Access Token" "accessToken" False Nothing
        formField_ FieldSm def{id = Just (targetId <> "-pathPrefix"), value = sync.pathPrefix, placeholder = "monoscope"} "Folder in repo" "pathPrefix" False Nothing
        p_ [class_ "text-xs text-textWeak"] do
          "Dashboards stored in "
          code_ [class_ "text-textBrand"] $ toHtml $ if T.null sync.pathPrefix then "dashboards/" else sync.pathPrefix <> "/dashboards/"

        -- Webhook URL
        div_ [class_ "pt-4 border-t border-strokeWeak space-y-2"] do
          headerRow_ [] do
            sectionLabel_ "Webhook URL"
            copyButton_ "btn btn-xs btn-ghost gap-1" "w-3 h-3" "my @data-url" [term "data-url" webhookUrl]
          div_ [class_ "bg-fillWeak rounded-lg px-3 py-1.5 font-mono text-xs text-textWeak break-all"] $ toHtml webhookUrl
          unless isViaApp $ p_ [class_ "text-xs text-textWeak"] "Add this to your repository for automatic syncing."

        -- Actions
        div_ [class_ "flex flex-wrap items-center gap-2 pt-2"] do
          button_ [class_ "btn btn-sm btn-primary gap-1", type_ "submit"] do
            if sync.syncEnabled then "Save" else "Resume sync"
            htmxIndicator_ (targetId <> "-indicator") LdXS
          when sync.syncEnabled $ button_ [type_ "button", hxPost_ (actionUrl <> "/pause"), hxTarget_ ("#" <> targetId), hxSwap_ "outerMorph", hxIndicator_ ("#" <> targetId <> "-pause-indicator"), class_ "btn btn-sm btn-ghost gap-1"] do
            "Pause sync"
            htmxIndicator_ (targetId <> "-pause-indicator") LdXS
          label_ [class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak", Lucid.for_ ("disconnect-modal-" <> sync.id.toText)] do
            faSprite_ "link-slash" "regular" "w-3 h-3"
            span_ "Disconnect"

      confirmModal_ ("disconnect-modal-" <> sync.id.toText) "Disconnect dashboard sync?" "This stops syncing this repository. Its dashboards will remain as local dashboards; source access and PR reviews are unchanged." [hxDelete_ actionUrl, hxSwap_ "outerMorph", hxTarget_ ("#" <> targetId)] "Disconnect"

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
    liftIO $ withResource ctx.jobsPool \conn ->
      void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncPushDashboard pid (unUUIDId dashboardId)
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
  pure $ PageCtx bw{pageTitle = "Dashboard sync"} RepositoryDashboardGet{repository, sync, accounts, form = RepositoryDashboardForm repository.credentialId branch Nothing, hostUrl = ctx.config.hostUrl, permission, setupError = Nothing}


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
        credentials <- hoistEither $ maybeToRight "Reconnect this account to enable dashboard sync." $ GitSync.credentialCreds account
        when (isJust account.installationId && any T.null [ctx.config.githubAppWebhookSecret, ctx.config.githubAppId, ctx.config.githubAppPrivateKey])
          $ hoistEither
          $ Left "An administrator must configure the GitHub App and webhook before dashboard sync can run."
        branch <-
          if T.null (T.strip form.branch)
            then do
              token <- ExceptT $ GitSync.githubToken ctx.config.githubAppId ctx.config.githubAppPrivateKey credentials
              conn <- hoistEither $ GitSync.credentialConn account token
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
          whenJust sync \connection -> liftIO $ withResource ctx.jobsPool \conn ->
            void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncRepository pid connection.id
          fresh <- repositoryDashboardPage pid rid
          addRespHeaders $ if isJust fresh.content.sync then fresh else fresh{content = fresh.content{setupError = Just "This account is no longer available. Choose another account and try again.", form}}


instance ToHtml RepositoryDashboardGet where
  toHtml page = section_ [class_ "mx-auto max-w-4xl space-y-6 px-4 py-6 sm:px-8 sm:py-8"] do
    let repository = page.repository
        base = "/p/" <> repository.projectId.toText <> "/repositories/" <> repository.id.toText
    a_ [href_ base, class_ "text-sm text-textBrand hover:underline"] $ toHtml $ repository.owner <> "/" <> repository.repo
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Dashboard sync"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Keep this team's dashboards alongside its code. Each repository has its own files, branch, and sync status."
    div_ [id_ "repository-dashboard-content", class_ "space-y-4"] do
      whenJust page.setupError $ p_ [role_ "alert", class_ "text-sm text-textError"] . toHtml
      if not (maybe False (>= ProjectMembers.PEdit) page.permission)
        then do
          p_ [class_ "text-sm text-textWeak"] "A project editor can configure dashboard sync for this repository."
          case page.sync of
            Nothing -> p_ [class_ "text-sm text-textWeak"] "Dashboard sync is not configured."
            Just sync -> div_ [class_ "space-y-2 rounded-xl border border-strokeWeak p-4"] do
              p_ [class_ "text-sm text-textStrong"] $ if sync.syncEnabled then "Sync enabled" else "Sync paused"
              p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ sync.branch <> " · " <> GitSync.getDashboardsPath sync
              whenJust sync.lastError $ p_ [role_ "status", class_ "text-sm text-textError break-words"] . toHtml
        else case page.sync of
          Just sync -> do
            toHtml $ gitSyncSettingsView page.hostUrl repository.projectId (Just sync)
            when (isNothing sync.installationId) $ whenJust sync.webhookSecret \secret -> div_ [class_ "space-y-2 border-t border-strokeWeak pt-4"] do
              formField_ FieldSm def{id = Just "repository-webhook-secret", inputType = "password", value = secret, extraAttrs = [readonly_ "true"]} "Webhook secret" "webhookSecret" False Nothing
              toHtml $ copyButton_ "btn btn-xs btn-ghost gap-1" "w-3 h-3" "my @data-secret" [term "data-secret" secret]
              p_ [class_ "text-xs text-textWeak"] "Use this secret when adding the push webhook to your Git host."
          Nothing | null page.accounts -> do
            p_ [class_ "text-sm text-textWeak"] "Connect an account with access to this repository to enable dashboard sync."
            a_ [href_ ("/p/" <> repository.projectId.toText <> "/repositories/connect"), class_ "btn btn-sm btn-primary"] "Connect an account"
          Nothing -> form_ [action_ (base <> "/dashboards"), method_ "post", hxPost_ (base <> "/dashboards"), hxTarget_ "#repository-dashboard-content", hxSelect_ "#repository-dashboard-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-dashboard-indicator", class_ "space-y-4"] do
            formSelectField_ FieldSm "Repository account" "credentialId" True do
              option_ ([value_ "", disabled_ "disabled"] <> [selected_ "selected" | isNothing page.form.credentialId]) "Choose an account"
              forM_ page.accounts \account -> option_ ([value_ account.id.toText] <> [selected_ "selected" | Just account.id == page.form.credentialId]) $ toHtml account.account
            div_ [class_ "grid gap-3 sm:grid-cols-2"] do
              formField_ FieldSm def{value = page.form.branch, placeholder = "Leave blank to detect"} "Branch" "branch" False Nothing
              formField_ FieldSm def{value = fromMaybe "" page.form.pathPrefix, placeholder = "monoscope"} "Folder in repo" "pathPrefix" False Nothing
            p_ [class_ "text-xs text-textWeak"] "Dashboard files use the dashboards/ directory inside this folder. Existing local dashboards stay local until you assign them to this repository."
            button_ [type_ "submit", class_ "btn btn-sm btn-primary gap-2"] do
              "Enable dashboard sync"
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
        base = "/p/" <> dash.projectId.toText
    a_ [href_ (base <> "/dashboards/" <> dash.id.toText), class_ "inline-flex items-center gap-2 text-sm text-textBrand hover:underline"] do
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
          connectionBadge_ "Repository assigned"
          p_ [class_ "font-medium text-sm text-textStrong break-all"] $ toHtml $ repository.owner <> "/" <> repository.repo
          p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ fromMaybe (Git.hostLabel repository.host) repository.apiBase <> " · " <> repository.branch
          whenJust dash.filePath $ p_ [class_ "font-mono text-xs text-textWeak break-all"] . toHtml . (GitSync.getDashboardsPath repository <>)
          whenJust repository.lastError $ p_ [role_ "status", class_ "text-sm text-textError break-words"] . toHtml
          if repository.syncEnabled
            then p_ [class_ "text-sm text-textWeak"] $ if isNothing dash.fileSha then "Waiting for the first push. Dashboard edits will sync to this repository." else "Dashboard edits sync to this repository."
            else p_ [class_ "text-sm text-textWarning"] "Sync is paused. Enable dashboard sync in Repositories to send edits."
          p_ [class_ "text-xs text-textWeak"] "Disconnect dashboard sync in Repositories to retain a local copy."
          a_ [href_ (base <> "/repositories?tab=configuration#git-sync-" <> repository.id.toText), class_ "btn btn-sm btn-ghost"] "View in Repositories"
        Nothing
          | not (maybe False (>= ProjectMembers.PEdit) page.permission) ->
              p_ [class_ "text-sm text-textWeak"] "A project editor can assign this dashboard to a repository."
        Nothing | null page.repositories -> do
          p_ [class_ "text-sm text-textWeak"] "Connect a repository to version dashboards alongside your team's code."
          a_ [href_ (base <> "/repositories?tab=configuration"), class_ "btn btn-sm btn-primary"] "Connect a repository"
        Nothing -> form_
          [ hxPost_ (base <> "/dashboards/" <> dash.id.toText <> "/repository")
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
                  $ repository.owner <> "/" <> repository.repo <> " · " <> fromMaybe (Git.hostLabel repository.host) repository.apiBase <> " · " <> repository.branch <> bool " · paused" "" repository.syncEnabled
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


-- | Meta-refresh redirect (no JS, escapes url properly) with a fallback link for no-refresh clients.
redirectPage :: Text -> Text -> Html ()
redirectPage msg url = div_ [class_ "p-8 text-center"] do
  meta_ [httpEquiv_ "refresh", content_ ("0;url=" <> url)]
  p_ [class_ "text-textWeak mb-4"] $ toHtml msg
  a_ [href_ url, class_ "text-textBrand underline"] "Continue"


-- | Store the grant an installation is, keyed by the account it covers — this is what makes
-- source-code reading work without also configuring dashboard sync. Best-effort: failing to name
-- the account must not lose the installation the user just completed.
recordInstallation :: Projects.ProjectId -> Int64 -> ATAuthCtx ()
recordInstallation pid instId = do
  requireGitWrite pid
  ctx <- ask @Config.AuthContext
  W.runHTTPWreq (GitSync.getInstallationAccount ctx.config.githubAppId ctx.config.githubAppPrivateKey instId) >>= \case
    Left err -> Log.logAttention "Could not record GitHub credential: installation account lookup failed" (pid, instId, err)
    Right account -> void $ GitSync.upsertGitHubCredential (encodeUtf8 ctx.config.apiKeyEncryptionSecretKey) pid Git.GitHub Nothing account (Just instId) Nothing


-- | Redirect to GitHub App installation page. @to@ names where the callback should land —
-- the same installation grants dashboard sync and source reading, so the flow has to come
-- back to whichever of the two the user started from.
githubAppInstallH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
githubAppInstallH pid toM = do
  requireGitWrite pid
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
  deriving stock (Generic, Show)
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
  where
    -- Where an install lands once GitHub hands control back.
    installReturnUrl :: Projects.ProjectId -> Int64 -> InstallDest -> Text
    installReturnUrl pid instId = \case
      InstallCode -> "/p/" <> pid.toText <> "/repositories/connect"
      InstallSync -> "/p/" <> pid.toText <> "/settings/git-sync/repos?installationId=" <> show instId

    -- View for selecting a project when state is missing from callback
    projectSelectorView :: Int64 -> [Projects.ProjectListItem] -> Html ()
    projectSelectorView instId projects = div_ [class_ "min-h-screen bg-bgBase flex items-center justify-center p-8"] do
      div_ [class_ "surface-raised rounded-2xl p-6 max-w-md w-full space-y-4"] do
        div_ [class_ "flex items-center gap-3 mb-4"] do
          iconBadgeLg_ SuccessBadge "circle-check"
          div_ do
            h3_ [class_ "text-lg font-semibold text-textStrong"] "GitHub App Installed!"
            p_ [class_ "text-sm text-textWeak"] "Select a project to connect"
        if null projects
          then emptyState_ def{size = ESCompact, icon = Just "folder"} "No projects found" "Create a project first, then come back to connect it."
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
      W.runHTTPWreq $ either errBox (repoSelectionView instId) <$> runExceptT do
        tok <- ExceptT $ first ("Failed to get token: " <>) <$> GitSync.getInstallationToken ctx.config.githubAppId ctx.config.githubAppPrivateKey instId
        conn <- hoistEither $ Git.mkGitConn Git.GitHub Nothing tok.token
        ExceptT $ first ("Failed to list repos: " <>) <$> Git.listRepos conn
  pure $ settingsSection_ do
    settingsH2_ "GitHub Sync"
    p_ [class_ "text-textWeak text-sm -mt-4"] "Select repositories for dashboard sync. You can connect more later."
    div_ [id_ "git-sync-content", class_ "surface-raised rounded-2xl p-4"] content
  where
    -- View for selecting a repository
    repoSelectionView :: Int64 -> [Git.GitRepo] -> Html ()
    repoSelectionView instId repos = div_ [class_ "space-y-4"] do
      h3_ [class_ "text-lg font-medium text-textStrong"] "Select repositories"
      p_ [class_ "text-sm text-textWeak"] "Choose repositories to sync dashboards with. Each keeps its own files and sync status."
      form_ [class_ "space-y-4", hxPost_ ("/p/" <> pid.toText <> "/settings/git-sync/select"), hxSwap_ "innerHTML", hxTarget_ "#git-sync-content"] do
        input_ [type_ "hidden", name_ "installationId", value_ (show instId)]
        repoFilter_ (length repos)
        div_ [class_ "space-y-2 max-h-80 overflow-y-auto c-scroll", id_ "repo-list"] $ forM_ repos \repo ->
          label_ [class_ "repo-row flex items-center gap-3 p-3 rounded-lg border border-strokeWeak hover:border-strokeBrand-strong cursor-pointer has-[:checked]:border-strokeBrand-strong has-[:checked]:bg-fillBrand-weak", term "data-filter" (T.toLower repo.fullName)] do
            input_
              [ type_ "checkbox"
              , name_ "repoFullName"
              , value_ repo.fullName
              , class_ "checkbox checkbox-sm"
              ]
            span_ [class_ "font-medium text-textStrong truncate"] $ toHtml repo.fullName
            when repo.private $ span_ [class_ "shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak"] "private"
        installationSettingsLink_ (GitSync.installationSettingsUrl instId)
        div_ [class_ "grid gap-4 sm:grid-cols-2"] do
          formField_ FieldSm def{placeholder = "Each repository’s default branch"} "Branch override (optional)" "branch" False Nothing
          formField_ FieldSm def{placeholder = "monoscope"} "Folder in repo (optional)" "pathPrefix" False Nothing
        primaryButton_ [type_ "submit"] "Connect selected repositories"

    -- Type-to-filter over a repo list. Only rendered once the list is long enough that scanning
    -- it is the slower option — an account with four repos does not need a search box.
    repoFilter_ :: Int -> Html ()
    repoFilter_ n = when (n > 8) $ label_ [class_ "input input-sm w-full flex items-center gap-2"] do
      faSprite_ "magnifying-glass" "regular" "w-3.5 h-3.5 text-iconNeutral shrink-0"
      input_
        [ type_ "search"
        , class_ "grow"
        , placeholder_ ("Filter " <> show n <> " repositories")
        , Aria.label_ "Filter repositories"
        , filterInputAttr_ ".repo-row"
        ]


-- | Handle repo selection from GitHub App
githubAppSelectRepoH :: Projects.ProjectId -> RepoSelectForm -> ATAuthCtx (RespHeaders (Html ()))
githubAppSelectRepoH pid form = do
  requireGitWrite pid
  ctx <- ask @Config.AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      selections = ordNub form.repoFullName
      creds = GitSync.AppInstallation form.installationId
  branches <-
    if not (T.null $ T.strip form.branch)
      then pure $ Right [(name, form.branch) | name <- selections]
      else W.runHTTPWreq $ runExceptT do
        token <- ExceptT $ GitSync.githubToken ctx.config.githubAppId ctx.config.githubAppPrivateKey creds
        conn <- hoistEither $ Git.mkGitConn Git.GitHub Nothing token
        repos <- ExceptT $ Git.listRepos conn
        forM selections \name -> do
          repo <- hoistEither $ maybeToRight ("Repository access is unavailable: " <> name) $ find ((== name) . (.fullName)) repos
          pure (name, repo.defaultBranch)
  case branches of
    Left err -> addErrorToast "Could not connect repositories" (Just err) >> addRespHeaders (p_ [class_ "text-sm text-textError"] $ toHtml err)
    Right [] -> addErrorToast "Select at least one repository" Nothing >> addRespHeaders (p_ [class_ "text-sm text-textWeak"] "Select at least one repository and try again.")
    Right selected -> do
      syncs <- GitSync.getGitSyncs pid
      results <- fmap catMaybes $ forM selected \(name, branch) -> do
        let (ownerVal, repoVal) = Git.splitFullName name
        case find (\s -> s.host == Git.GitHub && isNothing s.apiBase && s.owner == ownerVal && s.repo == repoVal) syncs of
          Nothing -> GitSync.insertGitHubSync encKey pid Git.GitHub Nothing ownerVal repoVal branch creds (guarded (not . T.null) ctx.config.githubAppWebhookSecret) (fromMaybe "" form.pathPrefix)
          Just existing -> pure $ Just existing
      recordInstallation pid form.installationId
      liftIO $ withResource ctx.jobsPool \conn -> forM_ results \sync ->
        void $ createJob conn "background_jobs" $ BackgroundJobs.GitSyncRepository pid sync.id
      addRespHeaders $ div_ [class_ "space-y-6"] $ forM_ results $ gitSyncSettingsView ctx.env.hostUrl pid . Just
