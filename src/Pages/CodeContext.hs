{-# LANGUAGE OrPatterns #-}

-- | The source behind one stack frame.
--
-- Its own module rather than a function in "Pages.LogExplorer.LogItem", because the model it
-- needs ("Models.Projects.CodeContext") reaches GitHub through @Models.Projects.GitSync@,
-- which reaches @Pkg.Components.Widget@, which imports @LogItem@ — a cycle. The renderer in
-- "Pages.Components" only ever builds the URL, so nothing on the rendering side has to know
-- this module exists.
module Pages.CodeContext (memoryGetH, memoryPostH, MemoryForm (..), WikiGet (..), repositoryAnalyzePostH, repositoryResumePostH, repositoryFolderPostH, repositoryConflictPostH, FolderForm (..), ConflictForm (..), repositoryTokenGetH, repositoryTokenPostH, RepositoryTokenGet (..), RepositoryTokenForm (..), codeContextH, repositoriesGetH, serviceRepositoriesGetH, repositoryGetH, repositoryDeleteH, repositorySourceGetH, repositorySourcePostH, repositorySourceDeleteH, repositoryReviewsPostH, repositorySourceEnabledPostH, SourceContextForm (..), repositoryConnectGetH, repositoryConnectPostH, RepositoryConnectGet (..), RepositoryConnectForm (..), RepositoryGet (..), ReviewReadiness (..), RepositoryTab (..), codeMappingsGetH, codeMappingsEditorGetH, codeMappingsPostH, codeMappingsDeleteH, CodeMappingForm (..), impactReviewSettingsPostH, impactReviewRetryH) where

import BackgroundJobs.Types qualified as Jobs
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUIDEff
import Data.Effectful.Wreq qualified as W
import Data.List (lookup)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Yaml qualified as Yaml
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (throwError)
import Effectful.Reader.Static qualified
import Hasql.Interpolate qualified as HI
import Lucid hiding (for_)
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxDelete_, hxGet_, hxInclude_, hxIndicator_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as Memory
import Models.Projects.Projects qualified as Projects
import Models.Projects.RepositorySync qualified as RepositorySync
import Network.HTTP.Types.URI (urlEncode)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper, mkPageCtx)
import Pages.Components (BadgeColor (..), FieldCfg (..), FieldSize (..), colorChip_, disclosure_, filterInputAttr_, formField_, formSelectField_, installationSettingsLink_, options_)
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Pkg.ImpactReview qualified as ImpactReview
import Relude
import Servant (err403, err404, getResponse)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, redirectCS)
import Utils (LoadingSize (..), faSprite_, htmxIndicator_, nonEmptyT, renderMarkdown)
import Web.Auth (demoGuestSessionId)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData (..))


-- | Source around one stack frame, read from the repository linked to the project.
--
-- Every outcome renders as a line of prose inside the frame's panel rather than as an error
-- response: this is a progressive enhancement on a stack trace that already reads fine
-- without it, and a project that has never configured a code mapping must not be shown a
-- failure it did not cause. The reasons are distinguished, though — "no mapping covers this
-- path" and "that line is past the end of the file" have different fixes, and collapsing
-- them into silence sends the reader to configure something already configured.
codeContextH :: Projects.ProjectId -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
codeContextH pid fileM lineM svcM revM = do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  case (nonEmptyT fileM, lineM) of
    (Just path, Just n) ->
      W.runHTTPWreq (CodeContext.fetchSnippet authCtx.codeBlobCache authCtx.config pid svcM (nonEmptyT revM) path n)
        >>= addRespHeaders
        . either reason_ snippet_
    _ -> addRespHeaders $ note_ "This frame has no file and line to look up." Nothing
  where
    -- An unmapped frame is the one failure the reader can act on from here, so it is the one
    -- that gets a control — carrying the path, so the form it opens is already filled in.
    reason_ err = note_ (CodeContext.snippetErrorMessage err) case err of
      CodeContext.NoMapping path -> Just path
      (CodeContext.CredentialGone; CodeContext.ReadFailed{}; CodeContext.LineOutOfRange{}) -> Nothing
    note_ :: Text -> Maybe Text -> Html ()
    note_ msg fixPathM = div_ [class_ "pl-5 py-1 text-2xs text-textWeak italic flex items-center gap-2"] do
      toHtml msg
      whenJust fixPathM \path ->
        a_
          [ href_ ("/p/" <> pid.toText <> "/repositories?sample=" <> decodeUtf8 (urlEncode True $ encodeUtf8 path))
          , class_ "not-italic text-textBrand underline shrink-0"
          ]
          "Choose a repository"
    -- No empty-body arm: 'fetchSnippet' returns a 'Left' for a line past the end of the
    -- file, so a 'Snippet' that reaches here has source in it.
    snippet_ :: CodeContext.Snippet -> Html ()
    snippet_ s =
      div_ [class_ "mt-1 rounded-md border border-strokeWeak overflow-hidden font-mono text-2xs leading-relaxed"]
        $ forM_ (zip [s.startLine ..] s.body) \(n, src) ->
          -- The failing line is marked by a background AND a gutter caret, never by
          -- colour alone: it is the one line in this panel a reader must not miss.
          div_ [class_ $ "flex " <> bool "" "bg-fillError-weak" (n == s.focusLine)] do
            span_ [class_ "shrink-0 w-12 px-2 text-right tabular-nums text-textWeak select-none border-r border-strokeWeak"] $ toHtml @Text (show n)
            span_ [class_ $ "shrink-0 w-3 text-center " <> bool "text-transparent" "text-textError" (n == s.focusLine)] $ toHtml @Text "›"
            span_ [class_ "px-2 whitespace-pre overflow-x-auto c-scroll"] $ toHtml src


-- | Mapping form. Only @repo@ is required, and it arrives from a picker rather than a text
-- box — the account's repositories are a list we already hold, so typing one is an
-- opportunity to typo, not a choice.
--
-- @samplePath@ is a line pasted out of a real stack trace. When it is given, the two path
-- fields are derived from it against the repository's file list instead of being typed; see
-- 'CodeContext.deriveMapping'.
data CodeMappingForm = CodeMappingForm
  { repo :: Maybe Text
  , ref :: Maybe Text
  , service :: Maybe Text
  , samplePath :: Maybe Text
  , pathPrefix :: Maybe Text
  , sourceRoot :: Maybe Text
  , credentialId :: Maybe GitSync.GitHubCredentialId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | The mapping editor. @sample@ carries a stack-frame path in from the frame that had no
-- mapping, so arriving from an unmapped trace lands on a form already holding the example it
-- needs.
codeMappingsGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsGetH pid = repositoriesGetH pid (Just Overview)


data MemoryForm = MemoryForm {pageId :: Memory.PageId, version :: Word32, title :: Text, body :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


memoryGetH :: Projects.ProjectId -> Maybe Memory.PageId -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
memoryGetH pid page = memoryPageH pid page Nothing


memoryPostH :: Projects.ProjectId -> MemoryForm -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
memoryPostH pid form = do
  requireReviewWrite pid
  session <- Projects.getSession
  case Memory.Document <$> Memory.mkMemoryText form.title <*> Memory.mkMemoryText form.body of
    Left message -> memoryPageH pid (Just form.pageId) $ Just (form, message)
    Right document -> do
      saved <- Memory.writePage pid form.pageId form.version document $ Memory.Member session.user.id
      case saved of
        Nothing -> memoryPageH pid (Just form.pageId) $ Just (form, "This page changed while you were editing. Your draft is below. Open the latest revision before saving again.")
        Just _ -> do
          addSuccessToast "Wiki page saved" Nothing
          memoryGetH pid $ Just form.pageId


data WikiGet = WikiGet
  { projectId :: Projects.ProjectId
  , canEdit :: Bool
  , pages :: [Memory.Page]
  , history :: [Memory.Page]
  , form :: MemoryForm
  , message :: Maybe Text
  }
  deriving stock (Generic, Show)


memoryPageH :: Projects.ProjectId -> Maybe Memory.PageId -> Maybe (MemoryForm, Text) -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
memoryPageH pid selected draft = do
  (session, _, bw) <- mkPageCtx pid
  when (session.persistentSession.id == demoGuestSessionId) $ throwError err404
  canEdit <- maybe False (>= ProjectMembers.PEdit) <$> ProjectMembers.getUserPermission pid session.user.id
  pages <- Memory.latestPages pid
  history <- maybe (pure []) (Memory.pageHistory pid) selected
  let current = listToMaybe history
  when (isJust selected && isNothing current && isNothing draft) $ throwError err404
  fresh <- maybe (UUIDId <$> UUIDEff.genUUID) pure selected
  let edit = maybe (MemoryForm fresh 0 "" "") (\page -> MemoryForm page.id (fromIntegral page.version) (Memory.memoryText page.document.title) (Memory.memoryText page.document.body)) current
  addRespHeaders $ PageCtx bw{pageTitle = "Repositories"} $ WikiGet pid canEdit pages history (maybe edit fst draft) (snd <$> draft)


instance ToHtml WikiGet where
  toHtml WikiGet{projectId = pid, canEdit, pages, history, form, message} = toHtmlRaw $ repositoryLayout_ pid Wiki Nothing canEdit True do
    let current = listToMaybe history
        base = "/p/" <> pid.toText <> "/memories"
        pageLink page = base <> "?page=" <> page.id.toText
        authorName = \case Memory.Member _ -> "Project member"; Memory.Agent _ -> "Agent"
    div_ [class_ "grid gap-6 md:grid-cols-[15rem_minmax(0,1fr)]"] do
      nav_ [Aria.label_ "Wiki pages", class_ "space-y-3", term "preload" "mouseover"] do
        when canEdit $ a_ (class_ "btn btn-sm btn-ghost" : repositoryNavAttrs base) "New page"
        when (null pages) $ p_ [class_ "text-sm text-textWeak"] "No wiki pages yet. Record service context, investigation notes, or runbooks here."
        when (length pages == Memory.pageLimit) $ p_ [class_ "text-sm text-textWeak"] $ toHtml @Text $ "Showing the " <> show Memory.pageLimit <> " most recent pages. Older pages remain available through their links."
        ul_ [class_ "space-y-1"] $ for_ pages \page -> li_ do
          a_ (class_ "block rounded-md px-2 py-2 text-sm text-textBrand hover:bg-fillWeak break-words" : repositoryNavAttrs (pageLink page)) $ toHtml $ Memory.memoryText page.document.title
      section_ [class_ "min-w-0 space-y-5"] do
        h2_ [class_ "text-lg font-semibold text-textStrong"] $ toHtml $ maybe "New wiki page" (Memory.memoryText . (.document.title)) current
        p_ [class_ "text-sm text-textWeak"] "Shared project notes and runbooks. Check dated conclusions against current source and telemetry."
        for_ current \page -> do
          p_ [class_ "text-xs text-textWeak"] $ toHtml @Text $ authorName page.author <> " · Revision " <> show page.version <> " · " <> show page.updatedAt
          div_ [class_ "whitespace-pre-wrap break-words text-sm text-textStrong"] $ toHtml $ Memory.memoryText page.document.body
        when canEdit $ details_ ([class_ "border-t border-strokeWeak pt-4"] <> [open_ "" | isNothing current || isJust message]) do
          summary_ [class_ "cursor-pointer text-sm font-medium text-textStrong"] $ if isNothing current then "Write a page" else "Edit this page"
          form_ [method_ "post", action_ base, hxPost_ base, hxTarget_ "#repository-content", hxSelect_ "#repository-content", hxSwap_ "outerHTML", class_ "mt-4 space-y-4"] do
            input_ [type_ "hidden", name_ "pageId", value_ form.pageId.toText]
            input_ [type_ "hidden", name_ "version", value_ $ show form.version]
            for_ message $ p_ [id_ "memory-error", role_ "alert", class_ "text-sm text-textError"] . toHtml
            label_ [term "for" "memory-title", class_ "block space-y-1 text-sm font-medium text-textStrong"] do
              span_ "Title (required)"
              input_ ([id_ "memory-title", name_ "title", required_ "", maxlength_ "120", value_ form.title, class_ "input input-bordered w-full"] <> [Aria.describedby_ "memory-error" | isJust message])
            label_ [term "for" "memory-body", class_ "block space-y-1 text-sm font-medium text-textStrong"] do
              span_ "Page content (required)"
              textarea_ ([id_ "memory-body", name_ "body", required_ "", maxlength_ "16000", rows_ "12", class_ "textarea textarea-bordered w-full text-sm font-normal"] <> [Aria.describedby_ "memory-error" | isJust message]) $ toHtml form.body
            button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save page"
        unless (null history) $ details_ [class_ "border-t border-strokeWeak pt-4"] do
          summary_ [class_ "cursor-pointer text-sm font-medium text-textStrong"] "Revision history"
          when (length history == Memory.pageLimit) $ p_ [class_ "mt-3 text-sm text-textWeak"] $ toHtml @Text $ "Showing the " <> show Memory.pageLimit <> " most recent revisions. Older revisions remain stored."
          div_ [class_ "mt-3 divide-y divide-strokeWeak"] $ for_ history \page -> details_ [class_ "py-3"] do
            summary_ [class_ "cursor-pointer text-sm text-textWeak"] $ toHtml @Text $ "Revision " <> show page.version <> " · " <> authorName page.author <> " · " <> show page.updatedAt
            h3_ [class_ "mt-3 text-sm font-semibold text-textStrong"] $ toHtml $ Memory.memoryText page.document.title
            div_ [class_ "mt-2 whitespace-pre-wrap break-words text-sm text-textStrong"] $ toHtml $ Memory.memoryText page.document.body


  toHtmlRaw = toHtml


repositoryLayout_ :: Projects.ProjectId -> RepositoryTab -> Maybe Text -> Bool -> Bool -> Html () -> Html ()
repositoryLayout_ pid tab serviceM canEdit canReadWiki content =
  let base = "/p/" <> pid.toText <> "/repositories"
   in div_ [class_ "h-full w-full overflow-y-auto"] do
        section_ [class_ "mx-auto max-w-6xl space-y-6 px-4 py-6 sm:px-8 sm:py-8"] do
          header_ [class_ "flex flex-col items-start gap-4 sm:flex-row sm:justify-between"] do
            div_ [class_ "min-w-0 w-full flex-1 space-y-1"] do
              h1_ [class_ "text-xl sm:text-2xl font-semibold tracking-tight text-textStrong break-words"] $ toHtml $ maybe "Repositories" ("Repositories for " <>) serviceM
              p_ [class_ "text-sm text-textWeak max-w-2xl"] $ toHtml @Text $ maybe "Manage source context, pull request reviews, and configuration sync for each repository." (const "Source mappings that apply to this service, including mappings for all services.") serviceM
              when (isJust serviceM) $ a_ [href_ base, class_ "inline-flex items-center gap-1 py-1 text-sm text-textBrand hover:underline"] "All repositories"
            when canEdit $ a_ [href_ (base <> "/connect"), class_ "btn btn-sm btn-ghost gap-2"] do
              faSprite_ "plus" "regular" "h-3.5 w-3.5"
              "Add repositories"
          when (isNothing serviceM)
            $ nav_ [id_ "repository-nav", Aria.label_ "Repository views", class_ "flex flex-wrap gap-1 border-b border-strokeWeak pb-2", term "preload" "mouseover"]
            $ forM_ ([(Overview, "Repositories", ""), (PullRequests, "Pull requests", "?tab=reviews"), (Wiki, "Project wiki", "?tab=wiki")] :: [(RepositoryTab, Text, Text)]) \(value, label, query) ->
              when (value /= Wiki || canReadWiki)
                $ a_
                  ( [class_ $ "rounded-lg px-3 py-2 text-sm font-medium transition-colors " <> bool "text-textWeak hover:bg-fillWeak hover:text-textStrong" "bg-fillBrand-weak text-textBrand" (tab == value)]
                      <> repositoryNavAttrs (if value == Wiki then "/p/" <> pid.toText <> "/memories" else base <> query)
                      <> [term "aria-current" "page" | tab == value]
                  )
                $ toHtml @Text label
          section_ [id_ "repository-content", class_ "min-w-0"] content


repositoryNavAttrs :: Text -> [Attribute]
repositoryNavAttrs url =
  [ href_ url
  , hxGet_ url
  , hxTarget_ "#repository-content"
  , hxSelect_ "#repository-content"
  , term "hx-select-oob" "#repository-nav:outerMorph"
  , hxSwap_ "outerMorph"
  , hxPushUrl_ "true"
  , [__|on click set my.preloadState to 'DONE'|]
  ]


data RepositoryTab = Overview | PullRequests | Wiki
  deriving stock (Eq, Show)


instance FromHttpApiData RepositoryTab where
  parseUrlPiece value = maybeToRight "Choose repositories, reviews or wiki" $ lookup value [("overview", Overview), ("reviews", PullRequests), ("wiki", Wiki), ("configuration", Overview)]


repositoriesGetH :: Projects.ProjectId -> Maybe RepositoryTab -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
repositoriesGetH pid tabM sampleM = case fromMaybe Overview tabM of
  Overview -> repositoriesPageH pid $ OverviewPage sampleM Nothing
  PullRequests -> repositoriesPageH pid ReviewsPage
  Wiki -> fmap toHtml <$> memoryGetH pid Nothing


serviceRepositoriesGetH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (Html ()))
serviceRepositoriesGetH pid service = repositoriesPageH pid (OverviewPage Nothing $ Just service)


data RepositoryView = OverviewPage (Maybe Text) (Maybe Text) | ReviewsPage


repositoriesPageH :: Projects.ProjectId -> RepositoryView -> ATAuthCtx (RespHeaders (Html ()))
repositoriesPageH pid selection = do
  (session, _, bw) <- mkPageCtx pid
  canEdit <- maybe False (>= ProjectMembers.PEdit) <$> ProjectMembers.getUserPermission pid session.user.id
  let (tab, serviceM) = case selection of
        OverviewPage _ service -> (Overview, service)
        ReviewsPage -> (PullRequests, Nothing)
      base = "/p/" <> pid.toText <> "/repositories"
      canReadWiki = session.persistentSession.id /= demoGuestSessionId
  content <- case selection of
    OverviewPage sampleM _ -> do
      mappings <- CodeContext.getCodeMappings pid
      syncs <- GitSync.getGitSyncs pid
      settings <- ImpactReviews.repositorySettings pid
      credentials <- GitSync.getGitHubCredentials pid
      repositories <- GitSync.getRepositories pid
      conflicts <- Hasql.interp @[HI.OneColumn GitSync.GitHubSyncId] [HI.sql| SELECT DISTINCT sync_id FROM projects.repository_resources WHERE project_id = #{pid} AND conflicted |]
      let applicable = filter (\m -> maybe True (\service -> isNothing m.service || m.service == Just service) serviceM) mappings
          visible = filter (\repository -> isNothing serviceM || any (mappingInRepository credentials repository) applicable) repositories
      pure $ overview_ canEdit serviceM sampleM base applicable syncs settings credentials visible [sid | HI.OneColumn sid <- conflicts]
    ReviewsPage -> do
      cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
      runs <- ImpactReviews.latestRuns pid
      pure $ reviewHistory_ canEdit cfg runs
  addRespHeaders $ bodyWrapper bw{pageTitle = "Repositories"} $ repositoryLayout_ pid tab serviceM canEdit canReadWiki content
  where
    overview_ :: Bool -> Maybe Text -> Maybe Text -> Text -> [CodeContext.CodeMapping] -> [GitSync.GitHubSync] -> [ImpactReviews.ReviewSettings] -> [GitSync.GitHubCredential] -> [GitSync.Repository] -> [GitSync.GitHubSyncId] -> Html ()
    overview_ canEdit serviceM sampleM base mappings syncs settings credentials repositories conflicts = do
      let linked = [((c.host, c.apiBase, m.owner, m.repo), m) | m <- mappings, c <- credentials, c.id == m.credentialId]
      if null repositories
        then div_ [class_ "rounded-xl border border-strokeWeak p-6 sm:p-8 space-y-3"] do
          h2_ [class_ "font-semibold text-textStrong"] $ toHtml @Text $ maybe "Connect the repositories behind your services" (const "No repositories linked to this service") serviceM
          p_ [class_ "text-sm text-textWeak max-w-xl"]
            $ toHtml @Text
            $ if canEdit
              then maybe "Link services to their code for source context and production impact reviews. You can also connect repositories that only contain dashboards." (const "Choose a repository, then link this service in Configure source context.") serviceM
              else "Ask a project editor to link services to their source repositories."
          when canEdit $ a_ [href_ (base <> maybe "/connect" (const "") serviceM), class_ "btn btn-sm btn-primary"] $ toHtml @Text $ maybe "Connect repositories" (const "Choose a repository") serviceM
        else div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] $ forM_ repositories \repository -> do
          let host = repository.host
              origin = repository.apiBase
              owner = repository.owner
              repo = repository.repo
              key = (host, origin, owner, repo)
              services = sort . ordNub $ mapMaybe (\(repoKey, m) -> guard (repoKey == key) *> m.service) linked
              reviewM = find (\s -> host == Git.GitHub && isNothing origin && s.owner == T.toLower owner && s.repo == T.toLower repo) settings
              sync = find (\s -> (s.host, s.apiBase, s.owner, s.repo) == key) syncs
          article_ [class_ "grid gap-4 p-4 sm:p-5 lg:grid-cols-[minmax(0,2fr)_repeat(3,minmax(0,1fr))]"] do
            div_ [class_ "min-w-0 space-y-2"] do
              a_ [href_ (base <> "/" <> repository.id.toText <> foldMap (\sample -> "/source?sample=" <> decodeUtf8 (urlEncode True $ encodeUtf8 sample)) sampleM), class_ "font-medium text-sm text-textBrand break-all hover:underline"] $ toHtml (owner <> "/" <> repo)
              p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ fromMaybe (Git.hostLabel host) origin
              p_ [class_ "text-xs text-textWeak break-words"] $ toHtml $ if any (\(repoKey, m) -> repoKey == key && isNothing m.service) linked then "All services" else if null services then "No services linked" else T.intercalate ", " services
            div_ [class_ "space-y-2"] do
              p_ [class_ "text-xs font-medium text-textWeak"] "Source context"
              if repository.sourceEnabled && any ((== key) . fst) linked
                then colorChip_ SuccessBadge "circle-check" "Active"
                else colorChip_ NeutralBadge "circle-info" $ if repository.sourceEnabled then "Needs setup" else "Off"
            div_ [class_ "space-y-2"] do
              p_ [class_ "text-xs font-medium text-textWeak"] "Production impact reviews"
              case reviewM of
                Just s | s.enabled && not (null services) -> colorChip_ SuccessBadge "circle-check" "Enabled"
                Just s | s.enabled -> colorChip_ WarningBadge "circle-info" "Link a service"
                Just _ -> colorChip_ NeutralBadge "circle-info" "Off"
                Nothing -> colorChip_ NeutralBadge "circle-info" "Not configured"
            div_ [class_ "space-y-2"] do
              p_ [class_ "text-xs font-medium text-textWeak"] "Configuration sync"
              case sync of
                Just s | s.id `elem` conflicts -> colorChip_ WarningBadge "circle-exclamation" "Conflicts need input"
                Just s | s.exportPaused -> colorChip_ WarningBadge "circle-info" "Export paused"
                Just s | isJust s.lastExportHash -> colorChip_ WarningBadge "code-branch" "Changes pending"
                Just s | Just err <- s.lastError -> colorChip_ ErrorBadge "circle-exclamation" "Sync failed" >> p_ [class_ "text-xs text-textError break-words"] (toHtml err.message)
                Just s | not s.syncEnabled -> colorChip_ NeutralBadge "circle-info" "Paused"
                Just s | isJust s.announcedRevision -> colorChip_ WarningBadge "circle-info" "Changes pending"
                Just s | isJust s.lastRevision -> colorChip_ SuccessBadge "circle-check" "Synced"
                Just _ -> colorChip_ NeutralBadge "circle-info" "Awaiting first sync"
                Nothing -> colorChip_ NeutralBadge "circle-info" "Not configured"

    reviewHistory_ :: Bool -> EnvConfig -> [ImpactReviews.ReviewRun] -> Html ()
    reviewHistory_ canEdit cfg runs = div_ [class_ "space-y-4"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Production impact reviews"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Advisory pull request reviews connect code changes to this project's production telemetry. Open a repository to configure reviews and evidence sharing."
      when (null runs) $ div_ [class_ "rounded-xl border border-strokeWeak p-5 space-y-2"] do
        p_ [class_ "text-sm font-medium text-textStrong"] "No pull requests reviewed yet"
        p_ [class_ "text-sm text-textWeak"] "Link a GitHub repository and its services to receive reviews when ready pull requests are opened or updated."
      forM_ runs \run -> details_ [class_ "rounded-lg border border-strokeWeak p-4"] do
        summary_ [class_ "cursor-pointer flex flex-wrap items-center gap-2 text-sm text-textStrong"] do
          span_ [class_ "font-medium break-all"] $ toHtml (run.owner <> "/" <> run.repo <> " #" <> show run.number)
          colorChip_ (case run.state of ImpactReviews.Completed -> SuccessBadge; ImpactReviews.Incomplete -> WarningBadge; ImpactReviews.Queued -> NeutralBadge; ImpactReviews.Reviewing -> BrandBadge; ImpactReviews.Superseded -> NeutralBadge) "code-branch" (show run.state)
          code_ [class_ "text-xs text-textWeak"] $ toHtml $ T.take 8 run.revision
        whenJust run.error $ p_ [class_ "mt-2 text-sm text-textError break-words"] . toHtml
        whenJust run.result \value -> case AE.fromJSON value of
          AE.Error _ -> p_ [class_ "mt-2 text-sm text-textWeak"] "The saved review could not be displayed. Rerun the review."
          AE.Success result -> div_ [class_ "prose prose-sm mt-3 max-w-none"] $ renderMarkdown (ImpactReview.renderReview cfg.hostUrl run True result)
        when (canEdit && run.revision == run.latestRevision && run.state /= ImpactReviews.Reviewing) $ button_ [class_ "btn btn-sm btn-ghost mt-3", hxPost_ ("/p/" <> pid.toText <> "/settings/pr-reviews/" <> run.id.toText <> "/rerun"), hxTarget_ "#repository-content", hxSelect_ "#repository-content", hxSwap_ "outerMorph"] "Rerun review"


data ReviewReadiness
  = ReviewReady ImpactReviews.ReviewSettings
  | ReviewDisabled
  | ReviewNeedsService
  | ReviewNeedsApp
  | ReviewNeedsServer
  | ReviewUnsupportedHost
  deriving stock (Show)


data RepositoryGet = RepositoryGet
  { repository :: GitSync.Repository
  , mappings :: [CodeContext.CodeMapping]
  , dashboardSync :: Maybe GitSync.GitHubSync
  , reviewReadiness :: ReviewReadiness
  , reviewSettings :: Maybe ImpactReviews.ReviewSettings
  , resources :: [RepositorySync.Resource]
  , conflictVersions :: [(UUIDId "repository_resource", Maybe RepositorySync.Definition)]
  , folderCandidates :: [RepositorySync.FolderCandidate]
  , analysis :: Maybe GitSync.RepositoryAnalysis
  , permission :: Maybe ProjectMembers.Permissions
  }
  deriving stock (Show)


repositoryGetH :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositoryGetH pid rid = do
  (session, _, bw) <- mkPageCtx pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  credentials <- GitSync.getGitHubCredentials pid
  allMappings <- CodeContext.getCodeMappings pid
  syncs <- GitSync.getGitSyncs pid
  settings <- ImpactReviews.repositorySettings pid
  cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
  permission <- ProjectMembers.getUserPermission pid session.user.id
  let mappings = filter (mappingInRepository credentials repository) allMappings
      dashboardSync = find (\s -> (s.host, s.apiBase, s.owner, s.repo) == (repository.host, repository.apiBase, repository.owner, repository.repo)) syncs
      reviewSettings = find (\s -> s.owner == T.toLower repository.owner && s.repo == T.toLower repository.repo) settings
      reviewReadiness
        | repository.host /= Git.GitHub || isJust repository.apiBase = ReviewUnsupportedHost
        | any T.null [cfg.githubAppWebhookSecret, cfg.githubAppId, cfg.githubAppPrivateKey] = ReviewNeedsServer
        | not (any (isJust . (.service)) mappings) = ReviewNeedsService
        | Just s <- reviewSettings = if s.enabled then ReviewReady s else ReviewDisabled
        | otherwise = ReviewNeedsApp
  resources <- maybe (pure []) (RepositorySync.resources pid . (.id)) dashboardSync
  conflictVersions <- forM (filter (.conflicted) resources) $ \resource -> (resource.id,) <$> RepositorySync.localDefinition resource
  folderCandidates <- RepositorySync.folderCandidates pid
  analysis <- GitSync.getRepositoryAnalysis pid rid
  addRespHeaders $ PageCtx bw{pageTitle = repository.owner <> "/" <> repository.repo} RepositoryGet{repository, mappings, dashboardSync, reviewReadiness, reviewSettings, resources, conflictVersions, folderCandidates, analysis, permission}


newtype ConflictForm = ConflictForm {useGit :: Bool}
  deriving stock (Generic)
  deriving anyclass (FromForm)


data FolderForm = FolderForm {kind :: RepositorySync.ResourceKind, localId :: UUID.UUID}
  deriving stock (Generic)
  deriving anyclass (FromForm)


repositoryAnalyzePostH :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositoryAnalyzePostH pid rid = do
  requireReviewWrite pid
  _ <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  ctx <- Effectful.Reader.Static.ask @AuthContext
  liftIO $ withResource ctx.jobsPool \conn -> void $ createJob conn "background_jobs" $ Jobs.AnalyzeRepository pid rid
  addSuccessToast "Analysis queued" (Just "Existing settings and disabled capabilities are preserved.")
  repositoryGetH pid rid


repositoryResumePostH :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositoryResumePostH pid rid = do
  requireReviewWrite pid
  page <- repositoryGetH pid rid
  for_ (getResponse page).content.dashboardSync \sync -> do
    result <- withRepositoryPullRequest pid sync Git.reopenConfigurationPullRequest
    case result of
      Left err -> addErrorToast "Could not resume export" (Just err)
      Right () -> do
        Hasql.interpExecute_ [HI.sql| UPDATE projects.git_sync SET export_paused = false, last_export_hash = NULL WHERE project_id = #{pid} AND id = #{sync.id} |]
        ctx <- Effectful.Reader.Static.ask @AuthContext
        liftIO $ withResource ctx.jobsPool \conn -> void $ createJob conn "background_jobs" $ Jobs.GitSyncRepository pid sync.id
  repositoryGetH pid rid


repositoryFolderPostH :: Projects.ProjectId -> GitSync.RepositoryId -> FolderForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositoryFolderPostH pid rid form = do
  requireReviewWrite pid
  page <- repositoryGetH pid rid
  let content = (getResponse page).content
  candidate <- RepositorySync.folderCandidates pid >>= maybe (throwError err404) pure . find (\item -> item.kind == form.kind && item.localId == form.localId)
  sync <- maybe (throwError err404) pure content.dashboardSync
  let kind = T.toLower $ show @Text candidate.kind
      directory = if candidate.kind == RepositorySync.Dashboard then "dashboards/" else "monitors/"
      path = (if T.null sync.pathPrefix then "" else sync.pathPrefix <> "/") <> directory <> kind <> "-" <> show candidate.localId <> ".yaml"
  assignment <- if candidate.kind == RepositorySync.Dashboard then GitSync.assignDashboardRepository pid (UUIDId candidate.localId) sync.id else pure $ Right ()
  case assignment of
    Left _ -> addErrorToast "Could not move dashboard" (Just "Its repository assignment changed. Refresh and try again.")
    Right () -> do
      RepositorySync.registerResource pid sync.id candidate.kind candidate.localId path
      ctx <- Effectful.Reader.Static.ask @AuthContext
      liftIO $ withResource ctx.jobsPool \conn -> void $ createJob conn "background_jobs" $ Jobs.GitSyncRepository pid sync.id
  repositoryGetH pid rid


repositoryConflictPostH :: Projects.ProjectId -> GitSync.RepositoryId -> UUIDId "repository_resource" -> ConflictForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositoryConflictPostH pid rid resourceId form = do
  requireReviewWrite pid
  page <- repositoryGetH pid rid
  resource <- maybe (throwError err404) pure $ find (\item -> item.id == resourceId && item.conflicted) (getResponse page).content.resources
  if form.useGit
    then do
      let incoming = traverse (first toText . AET.parseEither AE.parseJSON) resource.remoteDefinition
      case incoming of
        Left err -> addErrorToast "Could not apply Git version" (Just err)
        Right definition -> do
          observed <- RepositorySync.resourceSnapshot resource
          whenLeftM_ (RepositorySync.importDefinition resource observed definition) (addErrorToast "Could not apply Git version" . Just)
    else Hasql.interpExecute_ [HI.sql| UPDATE projects.repository_resources SET merged_definition = remote_definition, local_baseline = remote_definition, remote_definition = NULL, conflicted = false WHERE id = #{resource.id} AND project_id = #{pid} AND conflicted AND remote_definition IS NOT DISTINCT FROM #{resource.remoteDefinition}::jsonb |]
  ctx <- Effectful.Reader.Static.ask @AuthContext
  liftIO $ withResource ctx.jobsPool \conn -> void $ createJob conn "background_jobs" $ Jobs.GitSyncRepository pid resource.syncId
  repositoryGetH pid rid


withRepositoryPullRequest :: Projects.ProjectId -> GitSync.GitHubSync -> (forall es. W.HTTP :> es => Git.GitConn -> Git.RepoRef -> Int -> Eff es (Either Text ())) -> ATAuthCtx (Either Text ())
withRepositoryPullRequest pid observed action = case observed.pullRequestNumber of
  Nothing -> pure $ Right ()
  Just number -> do
    ctx <- Effectful.Reader.Static.ask @AuthContext
    runExceptT do
      sync <- ExceptT $ maybeToRight "This sync connection is unavailable." <$> GitSync.getGitSyncByIdDecrypted (encodeUtf8 ctx.config.apiKeyEncryptionSecretKey) pid observed.id GitSync.ExportDashboards
      credentials <- hoistEither $ maybeToRight "Reconnect the repository account." $ GitSync.syncCreds sync
      token <- ExceptT $ GitSync.githubToken ctx.config.githubAppId ctx.config.githubAppPrivateKey credentials
      conn <- hoistEither $ GitSync.syncConn sync token
      ExceptT $ action conn (GitSync.syncRepoRef sync) (fromIntegral number)


repositoryDeleteH :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (RespHeaders (Html ()))
repositoryDeleteH pid rid = do
  requireReviewWrite pid
  page <- repositoryGetH pid rid
  closed <-
    maybe
      (pure $ Right ())
      ( \sync -> withRepositoryPullRequest pid sync $ \conn repo number -> runExceptT do
          pr <- ExceptT $ Git.getConfigurationPullRequest conn repo number
          when (pr.state == "open") $ ExceptT $ Git.closeConfigurationPullRequest conn repo number
      )
      (getResponse page).content.dashboardSync
  case closed of
    Left err -> addRespHeaders $ p_ [role_ "alert", class_ "text-sm text-textError"] $ toHtml $ "Could not close the configuration PR. The repository is still connected. " <> err
    Right () ->
      GitSync.removeRepository pid rid >>= \case
        Left GitSync.ConnectionMissing -> throwError err404
        Left GitSync.SyncRunning -> addRespHeaders $ p_ [role_ "alert", class_ "text-sm text-textWarning"] "A sync is running. Try removing the repository again when it finishes."
        Right () -> do
          addSuccessToast "Repository removed" (Just "Dashboards, monitors, and review history are retained.")
          redirectCS $ "/p/" <> pid.toText <> "/repositories"
          addRespHeaders mempty


instance ToHtml RepositoryGet where
  toHtml page = section_ [id_ "repository-detail", class_ "mx-auto max-w-4xl space-y-8 px-4 py-6 sm:px-8 sm:py-8"] do
    let repository = page.repository
        base = "/p/" <> repository.projectId.toText <> "/repositories"
        canEdit = maybe False (>= ProjectMembers.PEdit) page.permission
    a_ [href_ base, class_ "inline-flex items-center gap-2 text-sm text-textBrand hover:underline"] do
      faSprite_ "arrow-left" "regular" "h-3 w-3"
      "All repositories"
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong break-all"] $ toHtml $ repository.owner <> "/" <> repository.repo
      p_ [class_ "text-sm text-textWeak break-all"] $ toHtml $ fromMaybe (Git.hostLabel repository.host) repository.apiBase
    whenJust (page.analysis >>= (.error)) $ \err -> p_ [role_ "status", class_ "text-sm text-textError"] $ toHtml $ "Setup needs attention: " <> err
    whenJust (page.analysis >>= (.suggestions)) $ \suggestions -> when (null page.mappings) $ div_ [class_ "rounded-xl border border-strokeWeak p-4 space-y-2"] do
      p_ [class_ "text-sm font-medium text-textStrong"] "Service mapping needs input"
      p_ [class_ "text-sm text-textWeak break-words"] $ toHtml $ if null suggestions.services then "No service names have been observed yet. Link a service when its telemetry arrives." else "Observed services: " <> T.intercalate ", " (take 12 suggestions.services)
    when canEdit $ form_ [method_ "post", action_ (base <> "/" <> repository.id.toText <> "/analyze"), hxPost_ (base <> "/" <> repository.id.toText <> "/analyze"), hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph"] $ button_ [type_ "submit", class_ "btn btn-sm btn-ghost"] "Analyze repository"
    section_ [class_ "space-y-3"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Services and source context"
      colorChip_ NeutralBadge "circle-info" $ if repository.sourceEnabled then "Source context enabled" else "Source context disabled"
      when canEdit $ form_ [method_ "post", action_ (base <> "/" <> repository.id.toText <> "/source/enabled"), hxPost_ (base <> "/" <> repository.id.toText <> "/source/enabled"), hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph"] do
        input_ [type_ "hidden", name_ "enabled", value_ (if repository.sourceEnabled then "false" else "true")]
        button_ [type_ "submit", class_ "btn btn-sm btn-ghost"] $ if repository.sourceEnabled then "Disable source context" else "Enable source context"

      if null page.mappings
        then p_ [class_ "text-sm text-textWeak"] "No source paths are linked. Link a service to read the failing code directly from its stack trace."
        else div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] $ forM_ page.mappings \mapping -> div_ [class_ "p-4 space-y-2"] do
          p_ [class_ "text-sm font-medium text-textStrong break-words"] $ toHtml $ fromMaybe "All services" mapping.service
          p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ scopeLabel mapping
          code_ [class_ "text-xs text-textWeak break-all"] $ toHtml mapping.ref
      a_ [href_ (base <> "/" <> repository.id.toText <> "/source"), class_ "btn btn-sm btn-ghost"] $ if canEdit then "Configure source context" else "View source context"
    section_ [class_ "space-y-3"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Production impact reviews"
      case page.reviewReadiness of
        ReviewReady settings -> do
          colorChip_ SuccessBadge "circle-check" "Ready for pull requests"
          p_ [class_ "text-sm text-textWeak"] $ if settings.includeEvidence then "Reviews share production aggregates and links in GitHub." else "Reviews share links to production evidence in GitHub."
        ReviewDisabled -> colorChip_ NeutralBadge "circle-info" "Reviews disabled"
        ReviewNeedsService -> p_ [class_ "text-sm text-textWeak"] "Link a named service so reviews can find the production telemetry for this repository."
        ReviewNeedsApp -> p_ [class_ "text-sm text-textWeak"] "Link source context through the GitHub App to receive automatic pull request reviews. A token connection can still provide source context and configuration sync."
        ReviewNeedsServer -> p_ [class_ "text-sm text-textWeak"] "An administrator must configure the GitHub App and webhook before automatic reviews can run."
        ReviewUnsupportedHost -> p_ [class_ "text-sm text-textWeak"] "Automatic pull request reviews currently support GitHub.com. Source context and configuration sync remain available for this host."
      when (canEdit && repository.host == Git.GitHub && isNothing repository.apiBase) $ form_ [method_ "post", action_ (base <> "/" <> repository.id.toText <> "/source/reviews"), hxPost_ (base <> "/" <> repository.id.toText <> "/source/reviews"), hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph", class_ "flex flex-wrap items-end gap-3"] do
        input_ [type_ "hidden", name_ "owner", value_ repository.owner]
        input_ [type_ "hidden", name_ "repo", value_ repository.repo]
        label_ [class_ "space-y-1 text-sm text-textWeak"] do
          span_ [class_ "block"] "PR reviews"
          select_ [name_ "enabled", class_ "select select-sm"] $ forM_ ([(True, "Enabled"), (False, "Disabled")] :: [(Bool, Text)]) \(value, label) -> option_ ([value_ (if value then "true" else "false")] <> [selected_ "selected" | value == maybe False (.enabled) page.reviewSettings]) $ toHtml @Text label
        label_ [class_ "space-y-1 text-sm text-textWeak"] do
          span_ [class_ "block"] "Production evidence in GitHub"
          select_ [name_ "includeEvidence", class_ "select select-sm"] $ forM_ ([(False, "Links only"), (True, "Aggregates and links")] :: [(Bool, Text)]) \(value, label) -> option_ ([value_ (if value then "true" else "false")] <> [selected_ "selected" | value == maybe False (.includeEvidence) page.reviewSettings]) $ toHtml @Text label
        button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save review settings"
      div_ [class_ "flex flex-wrap gap-2"] do
        a_ [href_ (base <> "?tab=reviews"), class_ "btn btn-sm btn-ghost"] "Review history"
    section_ [class_ "space-y-3"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Dashboards and monitors"
      case page.dashboardSync of
        Nothing -> p_ [class_ "text-sm text-textWeak"] "Configuration sync is not configured. This repository remains connected for your team."
        Just sync -> do
          whenJust sync.lastError $ p_ [role_ "status", class_ "text-sm text-textError break-words"] . toHtml . (.message)
          p_ [class_ "text-sm text-textWeak break-all"] $ toHtml $ sync.branch <> " · " <> GitSync.getDashboardsPath sync
          colorChip_ NeutralBadge "code-branch" $ if not sync.syncEnabled then "Disabled" else if sync.exportPaused then "Export paused" else if any (.conflicted) page.resources then "Conflicts need input" else if isJust sync.lastExportHash then "Changes proposed" else if isJust sync.lastRevision then "Synced" else "Preparing folder"
          whenJust sync.pullRequestNumber $ \number -> a_ [href_ ("https://github.com/" <> sync.owner <> "/" <> sync.repo <> "/pull/" <> show number), target_ "_blank", rel_ "noopener noreferrer", class_ "text-sm text-textBrand hover:underline"] $ toHtml $ "Configuration PR #" <> show @Text number
          p_ [class_ "text-sm text-textWeak"] "Monoscope edits are active immediately. Git changes apply after merge into the target branch. Dashboards and monitor definitions share this folder."
          when (canEdit && sync.exportPaused) $ form_ [method_ "post", action_ (base <> "/" <> repository.id.toText <> "/resume"), hxPost_ (base <> "/" <> repository.id.toText <> "/resume"), hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph"] $ button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Resume export"
          unless (null page.resources) $ ul_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] $ forM_ page.resources \resource -> li_ [class_ "flex flex-wrap items-center justify-between gap-3 p-3"] do
            code_ [class_ "text-xs text-textStrong break-all"] $ toHtml resource.filePath
            when resource.conflicted do
              span_ [class_ "text-sm text-textWarning"] "Both versions changed"
              details_ [class_ "w-full min-w-0 space-y-3"] do
                summary_ [class_ "cursor-pointer text-sm text-textBrand"] "Compare versions"
                div_ [class_ "grid min-w-0 gap-4 md:grid-cols-2"] $ for_ ([("Active in Monoscope", Yaml.encode . AE.toJSON <$> join (lookup resource.id page.conflictVersions)), ("Merged Git version", Yaml.encode <$> resource.remoteDefinition)] :: [(Text, Maybe ByteString)]) \(label, version) -> div_ [class_ "min-w-0 space-y-2"] do
                  h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml label
                  pre_ [class_ "max-h-80 overflow-auto rounded-lg bg-fillWeak p-3 text-xs text-textWeak"] $ toHtml @Text $ maybe "Deleted" decodeUtf8 version
              when canEdit $ form_ [method_ "post", action_ (base <> "/" <> repository.id.toText <> "/resources/" <> resource.id.toText <> "/conflict"), hxPost_ (base <> "/" <> repository.id.toText <> "/resources/" <> resource.id.toText <> "/conflict"), hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph", class_ "flex gap-2"] do
                button_ [type_ "submit", name_ "useGit", value_ "false", class_ "btn btn-sm btn-ghost"] "Keep Monoscope"
                button_ [type_ "submit", name_ "useGit", value_ "true", class_ "btn btn-sm btn-ghost"] "Use Git"
          when canEdit $ details_ [class_ "space-y-3"] do
            summary_ [class_ "cursor-pointer text-sm text-textBrand"] "Move existing resources into this folder"
            for_ ([RepositorySync.Dashboard, RepositorySync.Query, RepositorySync.Uptime, RepositorySync.Cron] :: [RepositorySync.ResourceKind]) \kind -> do
              let candidates = filter ((== kind) . (.kind)) page.folderCandidates
              unless (null candidates) $ form_ [method_ "post", action_ (base <> "/" <> repository.id.toText <> "/folder"), hxPost_ (base <> "/" <> repository.id.toText <> "/folder"), hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph", class_ "flex flex-wrap items-end gap-2"] do
                input_ [type_ "hidden", name_ "kind", value_ $ T.toLower $ show kind]
                formSelectField_ FieldSm (show kind) "localId" True $ for_ candidates \candidate -> option_ [value_ $ show candidate.localId] $ toHtml candidate.title
                button_ [type_ "submit", class_ "btn btn-sm btn-ghost"] "Move into folder"
      a_ [href_ (base <> "/" <> repository.id.toText <> "/dashboards"), class_ "btn btn-sm btn-ghost"] $ if canEdit then "Configure sync" else "View sync"
    when canEdit $ section_ [class_ "space-y-3 border-t border-strokeWeak pt-6"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Remove connection"
      p_ [class_ "max-w-2xl text-sm text-textWeak"] "Stop using this repository for source context, pull request reviews, and configuration sync. Dashboards and monitors stay local and review history is retained. Shared repository accounts remain connected."
      button_ [type_ "button", onclick_ "document.getElementById('remove-repository-dialog').showModal()", class_ "btn btn-sm btn-ghost text-textError hover:bg-fillError-weak"] "Remove repository"
      term "dialog" [id_ "remove-repository-dialog", Aria.labelledby_ "remove-repository-title", Aria.describedby_ "remove-repository-description", class_ "modal overscroll-contain"] do
        div_ [class_ "modal-box space-y-4 p-6"] do
          h2_ [id_ "remove-repository-title", class_ "text-lg font-semibold text-textStrong break-all"] $ toHtml $ "Remove " <> repository.owner <> "/" <> repository.repo <> "?"
          p_ [id_ "remove-repository-description", class_ "text-sm text-textWeak"] "Source context, automatic reviews, and configuration sync will stop for this repository. Dashboards, monitors, and review history are retained. You can connect the repository again later."
          div_ [id_ "remove-repository-feedback"] ""
          div_ [class_ "flex flex-wrap justify-end gap-2"] do
            form_ [method_ "dialog"] $ button_ [class_ "btn btn-sm btn-ghost", autofocus_] "Cancel"
            button_ [type_ "button", hxDelete_ (base <> "/" <> repository.id.toText), hxTarget_ "#remove-repository-feedback", hxSwap_ "innerHTML", term "hx-disable" "this", hxIndicator_ "#remove-repository-indicator", class_ "btn btn-sm bg-fillError-strong text-textInverse-strong hover:opacity-90 gap-2"] do
              "Remove repository"
              htmxIndicator_ "remove-repository-indicator" LdXS
        form_ [method_ "dialog", class_ "modal-backdrop"] $ button_ [Aria.label_ "Cancel removal"] "Close"
  toHtmlRaw = toHtml


mappingInRepository :: [GitSync.GitHubCredential] -> GitSync.Repository -> CodeContext.CodeMapping -> Bool
mappingInRepository credentials repository mapping =
  (mapping.owner, mapping.repo)
    == (repository.owner, repository.repo)
    && any (\c -> c.id == mapping.credentialId && (c.host, c.apiBase) == (repository.host, repository.apiBase)) credentials


repositorySourceGetH :: Projects.ProjectId -> GitSync.RepositoryId -> Maybe GitSync.GitHubCredentialId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
repositorySourceGetH pid rid cid sample = do
  (_, _, bw) <- mkPageCtx pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  content <- codeMappingsContent pid sample cid (Just repository)
  addRespHeaders $ bodyWrapper bw{pageTitle = "Source context"} $ section_ [class_ "mx-auto max-w-4xl space-y-6 px-4 py-6 sm:px-8 sm:py-8"] do
    a_ [href_ ("/p/" <> pid.toText <> "/repositories/" <> rid.toText), class_ "text-sm text-textBrand hover:underline"] $ toHtml $ repository.owner <> "/" <> repository.repo
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Source context"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Link this repository to the services it builds. Stack traces and pull request reviews use these mappings to find the right code and production telemetry."
    div_ [id_ "code-mappings-content"] content


repositorySourcePostH :: Projects.ProjectId -> GitSync.RepositoryId -> CodeMappingForm -> ATAuthCtx (RespHeaders (Html ()))
repositorySourcePostH pid rid form = do
  requireReviewWrite pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  cid <- maybe (throwError err403) pure (form.credentialId <|> repository.credentialId)
  account <- codeContextCredential pid (Just cid) >>= maybe (throwError err403) pure
  unless ((account.host, account.apiBase) == (repository.host, repository.apiBase)) $ throwError err403
  saveCodeMapping pid form{repo = Just (repository.owner <> "/" <> repository.repo), credentialId = Just account.id}
  repositorySourceGetH pid rid (Just account.id) (nonEmptyT form.samplePath)


newtype SourceContextForm = SourceContextForm {enabled :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


repositorySourceEnabledPostH :: Projects.ProjectId -> GitSync.RepositoryId -> SourceContextForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositorySourceEnabledPostH pid rid form = do
  requireReviewWrite pid
  _ <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  Hasql.interpExecute_ [HI.sql| UPDATE projects.repositories SET source_enabled = #{form.enabled} WHERE project_id = #{pid} AND id = #{rid} |]
  repositoryGetH pid rid


repositoryReviewsPostH :: Projects.ProjectId -> GitSync.RepositoryId -> ImpactReviews.ReviewSettings -> ATAuthCtx (RespHeaders (Html ()))
repositoryReviewsPostH pid rid settings = do
  requireReviewWrite pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  unless (repository.host == Git.GitHub && isNothing repository.apiBase) $ throwError err403
  ImpactReviews.updateSettings pid settings{ImpactReviews.owner = T.toLower repository.owner, ImpactReviews.repo = T.toLower repository.repo}
  addSuccessToast "Review settings saved" Nothing
  page <- repositoryGetH pid rid
  addRespHeaders $ toHtml $ getResponse page


repositorySourceDeleteH :: Projects.ProjectId -> GitSync.RepositoryId -> CodeContext.CodeMappingId -> ATAuthCtx (RespHeaders (Html ()))
repositorySourceDeleteH pid rid mid = do
  requireReviewWrite pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  credentials <- GitSync.getGitHubCredentials pid
  mappings <- CodeContext.getCodeMappings pid
  unless (any (\m -> m.id == mid && mappingInRepository credentials repository m) mappings) $ throwError err404
  CodeContext.deleteCodeMapping pid mid
  repositorySourceGetH pid rid Nothing Nothing


-- | The panel, rebuilt from scratch after every change. Listing the account's repositories is
-- a GitHub call, so it happens once here rather than per render inside the view.
--
-- Without a linked account there is nothing to map onto, so the page says so and offers the
-- one control that fixes it rather than a form whose every submission would be discarded.
codeMappingsEditorGetH :: Projects.ProjectId -> Maybe GitSync.GitHubCredentialId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsEditorGetH pid credentialM sampleM = do
  _ <- Projects.sessionAndProject pid
  addRespHeaders =<< codeMappingsContent pid sampleM credentialM Nothing


codeMappingsContent :: Projects.ProjectId -> Maybe Text -> Maybe GitSync.GitHubCredentialId -> Maybe GitSync.Repository -> ATAuthCtx (Html ())
codeMappingsContent pid sampleM credentialM repositoryM = do
  session <- Projects.getSession
  canEdit <- maybe False (>= ProjectMembers.PEdit) <$> ProjectMembers.getUserPermission pid session.user.id
  allAccounts <- if canEdit then codeContextCredentials pid else GitSync.getGitHubCredentials pid
  let accounts = filter (\c -> maybe True (\r -> (r.host, r.apiBase) == (c.host, c.apiBase)) repositoryM) allAccounts
      requested = credentialM <|> (repositoryM >>= (.credentialId))
  selected <- if canEdit then codeContextCredential pid requested else pure Nothing
  let credM = selected >>= \c -> guard (any ((== c.id) . (.id)) accounts && (isNothing repositoryM || isJust requested)) $> c
  mappings <- filter (\m -> maybe True (\r -> mappingInRepository accounts r m) repositoryM) <$> CodeContext.getCodeMappings pid
  repoResult <- maybe (pure $ Right []) (credentialRepos repositoryM) credM
  reviewSettings <- filter (\settings -> maybe True (\r -> r.host == Git.GitHub && isNothing r.apiBase && (settings.owner, settings.repo) == (T.toLower r.owner, T.toLower r.repo)) repositoryM) <$> ImpactReviews.repositorySettings pid
  authConfig <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
  pure $ div_ [class_ "space-y-6"] do
    unless canEdit $ p_ [class_ "text-sm text-textWeak"] "A project editor can configure source context and pull request reviews."
    unless (null mappings) $ div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] $ forM_ mappings (mappingRow_ canEdit)
    when (canEdit && not (null accounts) && isNothing repositoryM)
      $ form_
        ( [ action_ editorUrl
          , method_ "get"
          , hxGet_ editorUrl
          , hxTrigger_ "change"
          , hxTarget_ "#code-mappings-content"
          , hxIndicator_ "#code-account-indicator"
          , term "hx-disable" "#code-mappings-content input, #code-mappings-content select, #code-mappings-content button"
          , class_ "space-y-2"
          ]
            <> scopedSwap
        )
      $ do
        input_ [type_ "hidden", name_ "sample", value_ sample]
        formSelectField_ FieldSm "Repository account" "credentialId" True do
          option_ ([value_ "", disabled_ "disabled"] <> [selected_ "selected" | isNothing credM]) "Choose an account"
          forM_ accounts \account ->
            option_
              ([value_ account.id.toText] <> [selected_ "selected" | Just account.id == ((.id) <$> credM)])
              $ toHtml
              $ account.account
              <> " · "
              <> fromMaybe (Git.hostLabel account.host) account.apiBase
        htmxIndicator_ "code-account-indicator" LdXS
    when canEdit $ case credM of
      Nothing | null accounts -> do
        p_ [class_ "text-sm text-textWeak max-w-2xl"] "Connect an account to link your services to source code and production impact reviews. The same connection can sync dashboards."
        a_ [href_ ("/p/" <> pid.toText <> "/repositories/connect"), class_ "btn btn-sm btn-primary gap-2"] do
          faSprite_ "code-branch" "regular" "w-3.5 h-3.5"
          "Connect an account"
      Nothing -> p_ [class_ "text-sm text-textWeak"] "Choose an account to link another repository. Existing service mappings keep their own account."
      Just cred -> do
        p_ [class_ "text-sm text-textWeak"] "Link services and source paths. The same service links are used by pull request reviews."
        case repoResult of
          Left _ -> div_ [role_ "alert", class_ "space-y-2"] do
            p_ [class_ "text-sm text-textError"] "Could not load repositories from this account. Check its access or retry; you can still enter a repository name below."
            button_ ([type_ "button", hxGet_ editorUrl, hxInclude_ "#code-mappings-content form[action]", hxTarget_ "#code-mappings-content", hxIndicator_ "#code-account-indicator", term "hx-disable" "#code-mappings-content input, #code-mappings-content select, #code-mappings-content button", class_ "btn btn-sm btn-ghost"] <> scopedSwap) "Retry"
          Right _ -> pass
        addMappingForm_ (fromRight [] repoResult) cred
    when (isNothing repositoryM) $ section_ [class_ "pt-6 space-y-3 border-t border-strokeWeak"] do
      h3_ [class_ "text-sm font-semibold text-textStrong"] "Production impact reviews"
      p_ [class_ "text-xs text-textWeak"] "Linked GitHub repositories receive advisory PR comments based on this project's telemetry. Ready PRs are reviewed when opened or updated."
      when (T.null authConfig.githubAppWebhookSecret) $ p_ [class_ "text-xs text-textError"] "Reviews need a GitHub App webhook secret. Ask your Monoscope administrator to configure it."
      when (null reviewSettings) $ p_ [class_ "text-xs text-textWeak"] "Link a repository using a GitHub App installation to enable automatic reviews."
      forM_ reviewSettings \settings ->
        if canEdit
          then form_ ([class_ "flex flex-wrap items-end gap-3 rounded-lg border border-strokeWeak p-3", hxPost_ (maybe ("/p/" <> pid.toText <> "/settings/pr-reviews") ((<> "/reviews") . sourceUrl) repositoryM), hxTarget_ "#code-mappings-content"] <> scopedSwap) $ do
            input_ [type_ "hidden", name_ "owner", value_ settings.owner]
            input_ [type_ "hidden", name_ "repo", value_ settings.repo]
            span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml (settings.owner <> "/" <> settings.repo)
            label_ [class_ "text-xs text-textWeak space-y-1"] do
              span_ [class_ "block"] "PR reviews"
              select_ [name_ "enabled", class_ "select select-sm"] $ options_ (Just $ bool "false" "true" settings.enabled) [("true", "Enabled"), ("false", "Disabled")]
            label_ [class_ "text-xs text-textWeak space-y-1"] do
              span_ [class_ "block"] "Production evidence in GitHub"
              select_ [name_ "includeEvidence", class_ "select select-sm"] $ options_ (Just $ bool "false" "true" settings.includeEvidence) [("true", "Aggregates and links"), ("false", "Links only")]
            button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save review settings"
          else div_ [class_ "space-y-1 rounded-lg border border-strokeWeak p-3"] do
            p_ [class_ "text-sm font-medium text-textStrong break-all"] $ toHtml $ settings.owner <> "/" <> settings.repo
            colorChip_ NeutralBadge "code-branch" $ if settings.enabled then "Reviews enabled" else "Reviews disabled"
            p_ [class_ "text-xs text-textWeak"] $ if settings.includeEvidence then "Production evidence: aggregates and links" else "Production evidence: links only"
  where
    sample = fromMaybe "" sampleM
    sourceUrl r = "/p/" <> pid.toText <> "/repositories/" <> r.id.toText <> "/source"
    editorUrl = maybe ("/p/" <> pid.toText <> "/settings/code-mappings/editor") sourceUrl repositoryM
    postUrl = maybe ("/p/" <> pid.toText <> "/settings/code-mappings") sourceUrl repositoryM
    scopedSwap = [attr | isJust repositoryM, attr <- [hxSelect_ "#code-mappings-content", hxSwap_ "outerMorph"]]

    -- One linked repository, read as a sentence rather than as five columns of path fragments.
    mappingRow_ :: Bool -> CodeContext.CodeMapping -> Html ()
    mappingRow_ canEdit cm = div_ [class_ "grid grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-x-3 gap-y-2 px-3 py-3 text-sm"] do
      faSprite_ "github" "regular" "w-3.5 h-3.5 shrink-0 text-iconNeutral"
      span_ [class_ "min-w-0 font-medium text-textStrong break-all"] $ toHtml (cm.owner <> "/" <> cm.repo)
      when canEdit
        $ button_
          ( [ class_ "ml-auto shrink-0 btn btn-xs btn-ghost text-textError gap-1"
            , hxDelete_ (postUrl <> "/" <> cm.id.toText)
            , hxTarget_ "#code-mappings-content"
            , Aria.label_ ("Unlink " <> cm.owner <> "/" <> cm.repo)
            ]
              <> scopedSwap
          )
        $ do
          faSprite_ "link-slash" "regular" "w-3 h-3"
          "Unlink"
      div_ [class_ "col-start-2 col-span-2 flex flex-wrap items-center gap-2 text-2xs text-textWeak"] do
        span_ [class_ "rounded-sm border border-strokeWeak px-1 font-mono break-all"] $ toHtml cm.ref
        span_ [class_ "break-words"] $ toHtml (scopeLabel cm)
        whenJust cm.service $ span_ [class_ "rounded-sm border border-strokeWeak px-1 break-all"] . toHtml

    -- Add a repository. One picker and, when the paths are not already repo-relative, one
    -- pasted stack-trace line — everything else is derived or defaulted, and the raw fields stay
    -- available under Advanced for the cases the derivation cannot reach.
    addMappingForm_ :: [Git.GitRepo] -> GitSync.GitHubCredential -> Html ()
    addMappingForm_ repos credential =
      form_
        ( [ class_ "pt-4 space-y-3 border-t border-strokeWeak"
          , hxPost_ postUrl
          , hxTarget_ "#code-mappings-content"
          , hxIndicator_ "#code-mapping-indicator"
          ]
            <> scopedSwap
        )
        $ do
          input_ [type_ "hidden", name_ "credentialId", value_ credential.id.toText]
          div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-3"] do
            -- Each option carries its own default branch, so the Branch field below can fill
            -- itself in and a repo whose trunk is not called "main" needs no correction.
            case repositoryM of
              Just repository -> input_ [type_ "hidden", name_ "repo", value_ (repository.owner <> "/" <> repository.repo)]
              Nothing | null repos -> formField_ FieldSm def{inputId = Just "code-mapping-repo", placeholder = "checkout-service"} "Repository" "repo" True Nothing
              Nothing -> formField_ FieldSm def{inputId = Just "code-mapping-repo"} "Repository" "repo" True
                $ Just
                $ select_ [id_ "code-mapping-repo", name_ "repo", required_ "true", class_ "select select-sm w-full cursor-pointer"]
                $ forM_ repos \r ->
                  option_ [value_ r.fullName, term "data-branch" r.defaultBranch] $ toHtml r.fullName
            formField_
              FieldSm
              def
                { value = maybe "main" (.defaultBranch) (case repositoryM of Nothing -> listToMaybe repos; Just repository -> find ((== repository.owner <> "/" <> repository.repo) . (.fullName)) repos)
                , placeholder = "main"
                , extraAttrs = [[__| on load or change from #code-mapping-repo set my value to #code-mapping-repo.selectedOptions[0].dataset.branch |] | not (null repos), isNothing repositoryM]
                }
              "Branch"
              "ref"
              False
              Nothing
            formField_ FieldSm def{placeholder = "any service"} "Service" "service" False Nothing
          -- The picker can only offer what the installation was granted, so a missing repository
          -- is a narrower grant rather than a bug in the list, and this is where it is widened.
          when (isNothing repositoryM) $ whenJust credential.installationId $ installationSettingsLink_ . GitSync.installationSettingsUrl
          formField_
            FieldSm
            def{value = sample, placeholder = "/srv/app/services/checkout.py"}
            "A file path from one of your stack traces"
            "samplePath"
            False
            Nothing
          p_ [class_ "text-xs text-textWeak"] "Paste one line's path and we work out how it lines up with the repository. Leave it blank if your traces already print repo-relative paths."
          disclosure_ [] "text-xs font-medium text-textWeak hover:text-textStrong" "Advanced: set the paths myself"
            $ div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2 pt-3"] do
              formField_ FieldSm def{inputId = Just "code-mapping-path-prefix", placeholder = "/srv/app/"} "Strip from frame path" "pathPrefix" False Nothing
              formField_ FieldSm def{placeholder = "src"} "Directory inside the repo" "sourceRoot" False Nothing
          div_ [class_ "flex items-center gap-2"] do
            button_ [class_ "btn btn-sm btn-primary", type_ "submit"] $ if isJust repositoryM then "Link service" else "Link repository"
            htmxIndicator_ "code-mapping-indicator" LdXS
          p_ [class_ "text-2xs text-textWeak"] "Spans that report their commit sha are read at that revision; the branch above is the fallback for those that don't."


-- | Cache successful provider listings; an unavailable account is distinct from an empty one.
credentialRepos :: Maybe GitSync.Repository -> GitSync.GitHubCredential -> ATAuthCtx (Either Text [Git.GitRepo])
credentialRepos repositoryM cred = do
  ctx <- Effectful.Reader.Static.ask @AuthContext
  cached <- liftIO $ Cache.lookup ctx.repoListCache cred.id
  let known = case repositoryM of
        Nothing -> cached
        Just repository -> pure <$> find ((== repository.owner <> "/" <> repository.repo) . (.fullName)) (fold cached)
  case known of
    Just repos -> pure $ Right repos
    Nothing ->
      ( runExceptT do
          conn <- repoConn ctx.config cred
          case repositoryM of
            Nothing -> ExceptT $ maybe (Git.listTokenRepos conn) (const $ Git.listRepos conn) cred.installationId
            Just repository -> pure <$> ExceptT (Git.fetchRepository conn $ Git.RepoRef repository.owner repository.repo "HEAD")
      )
        >>= \case
          Left err -> Left err <$ Log.logWarn "Could not list repositories" (cred.projectId, Git.hostSlug cred.host, err)
          Right repos -> do
            when (isNothing repositoryM) $ liftIO $ Cache.insert ctx.repoListCache cred.id repos
            pure $ Right repos


newtype RepositoryConnectForm = RepositoryConnectForm
  { repoFullName :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data RepositoryConnectGet = RepositoryConnectGet
  { projectId :: Projects.ProjectId
  , accounts :: [GitSync.GitHubCredential]
  , credential :: Maybe GitSync.GitHubCredential
  , repositories :: Either Text [Git.GitRepo]
  , selected :: [Text]
  , connectionError :: Maybe Text
  }
  deriving stock (Show)


repositoryConnectGetH :: Projects.ProjectId -> Maybe GitSync.GitHubCredentialId -> ATAuthCtx (RespHeaders (PageCtx RepositoryConnectGet))
repositoryConnectGetH pid cid = do
  requireReviewWrite pid
  addRespHeaders =<< repositoryConnectPage pid cid


repositoryConnectPage :: Projects.ProjectId -> Maybe GitSync.GitHubCredentialId -> ATAuthCtx (PageCtx RepositoryConnectGet)
repositoryConnectPage projectId cid = do
  (_, _, bw) <- mkPageCtx projectId
  accounts <- codeContextCredentials projectId
  selectedAccount <- codeContextCredential projectId cid
  repositories <- maybe (pure $ Right []) (credentialRepos Nothing) selectedAccount
  let credential = find (\account -> Just account.id == ((.id) <$> selectedAccount)) accounts
  pure $ PageCtx bw{pageTitle = "Connect repositories"} RepositoryConnectGet{projectId, accounts, credential, repositories, selected = [], connectionError = Nothing}


repositoryConnectPostH :: Projects.ProjectId -> Maybe GitSync.GitHubCredentialId -> RepositoryConnectForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryConnectGet))
repositoryConnectPostH pid cid form = do
  requireReviewWrite pid
  page <- repositoryConnectPage pid cid
  let selected = ordNub form.repoFullName
  result <- runExceptT do
    account <- hoistEither $ maybeToRight "Choose an account connected to this project." page.content.credential
    available <- hoistEither page.content.repositories
    when (null selected) $ hoistEither $ Left "Select at least one repository."
    repositories <- forM selected \name ->
      hoistEither $ maybeToRight ("Repository access is unavailable: " <> name <> ". Check account access and try again.") $ find ((== name) . (.fullName)) available
    existing <- lift $ GitSync.getRepositories pid
    forM_ repositories \repository -> do
      connected <- ExceptT $ maybeToRight "This account is no longer connected. Choose another account and try again." <$> GitSync.connectRepository pid account.id repository
      unless (any ((== connected.id) . (.id)) existing) $ lift do
        ctx <- Effectful.Reader.Static.ask @AuthContext
        liftIO $ withResource ctx.jobsPool \jobsConn -> void $ createJob jobsConn "background_jobs" $ Jobs.AnalyzeRepository pid connected.id
  case result of
    Left err -> addRespHeaders page{content = page.content{selected, connectionError = guard (isRight page.content.repositories) $> err}}
    Right () -> do
      addSuccessToast "Repositories connected" (Just "We’re analyzing their services and configuration. Reliable matches are enabled automatically.")
      redirectCS $ "/p/" <> pid.toText <> "/repositories"
      addRespHeaders page


instance ToHtml RepositoryConnectGet where
  toHtml page = section_ [class_ "mx-auto max-w-4xl space-y-6 px-4 py-6 sm:px-8 sm:py-8"] do
    let base = "/p/" <> page.projectId.toText <> "/repositories"
        connectionUrl = base <> "/connect" <> foldMap (\c -> "?credentialId=" <> c.id.toText) page.credential
    a_ [href_ base, class_ "text-sm text-textBrand hover:underline"] "All repositories"
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Connect repositories"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Choose repositories to add. Monoscope analyzes their services and enables source context, PR reviews, and dashboard and monitor sync where setup is clear. This can post reviews and open configuration PRs. You can disable each capability at any time."
    div_ [id_ "repository-connect-content", class_ "space-y-5"] do
      unless (null page.accounts) $ form_
        [action_ (base <> "/connect"), method_ "get", hxGet_ (base <> "/connect"), hxTrigger_ "change", hxPushUrl_ "true", hxTarget_ "#repository-connect-content", hxSelect_ "#repository-connect-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-account-indicator", class_ "space-y-2"]
        do
          formSelectField_ FieldSm "Repository account" "credentialId" True do
            option_ ([value_ "", disabled_ "disabled"] <> [selected_ "selected" | isNothing page.credential]) "Choose an account"
            forM_ page.accounts \account -> option_ ([value_ account.id.toText] <> [selected_ "selected" | Just account.id == ((.id) <$> page.credential)]) $ toHtml $ account.account <> " · " <> fromMaybe (Git.hostLabel account.host) account.apiBase
          htmxIndicator_ "repository-account-indicator" LdXS
      whenJust page.connectionError $ p_ [role_ "alert", class_ "text-sm text-textError break-words"] . toHtml
      case page.credential of
        Nothing -> p_ [class_ "text-sm text-textWeak"] $ if null page.accounts then "Connect an account to choose its repositories." else "Choose an account to see the repositories it can access."
        Just account -> do
          case page.repositories of
            Left _ -> div_ [role_ "alert", class_ "space-y-3 rounded-lg border border-strokeError-weak bg-fillError-weak p-4"] do
              p_ [class_ "text-sm text-textError"] "Could not load repositories. Retry, or ask an administrator to check this connection."
              a_ [id_ "repository-connect-retry", href_ connectionUrl, hxGet_ connectionUrl, hxTarget_ "#repository-connect-content", hxSelect_ "#repository-connect-content", hxSwap_ "outerMorph", class_ "btn btn-sm btn-ghost"] "Retry"
            Right [] -> p_ [role_ "status", class_ "text-sm text-textWeak"] "No repositories are available to this account. Grant repository access or connect another account."
            Right repositories -> form_ [action_ connectionUrl, method_ "post", hxPost_ connectionUrl, hxTarget_ "#repository-connect-content", hxSelect_ "#repository-connect-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-connect-indicator", class_ "space-y-4"] do
              when (length repositories > 8) $ label_ [class_ "input input-sm w-full"] do
                faSprite_ "magnifying-glass" "regular" "h-3.5 w-3.5 text-iconNeutral"
                input_ [type_ "search", Aria.label_ "Filter repositories", placeholder_ "Filter repositories", class_ "grow", filterInputAttr_ ".repository-choice"]
              div_ [class_ "max-h-96 overflow-y-auto c-scroll rounded-xl border border-strokeWeak divide-y divide-strokeWeak"] $ forM_ repositories \repository ->
                label_ [class_ "repository-choice flex min-h-11 cursor-pointer items-center gap-3 p-3 hover:bg-fillWeak has-[:checked]:bg-fillBrand-weak", term "data-filter" (T.toLower repository.fullName)] do
                  input_ ([type_ "checkbox", name_ "repoFullName", value_ repository.fullName, class_ "checkbox checkbox-sm shrink-0"] <> [checked_ | repository.fullName `elem` page.selected])
                  span_ [class_ "min-w-0 flex-1 text-sm text-textStrong break-all"] $ toHtml repository.fullName
                  when repository.private $ span_ [class_ "text-xs text-textWeak shrink-0"] "Private"
              button_ [type_ "submit", class_ "btn btn-sm btn-primary gap-2"] do
                "Connect selected repositories"
                htmxIndicator_ "repository-connect-indicator" LdXS
          whenJust account.installationId $ installationSettingsLink_ . GitSync.installationSettingsUrl
      a_ [href_ ("/p/" <> page.projectId.toText <> "/settings/git-sync/install?to=code"), class_ "btn btn-sm btn-ghost"] "Connect GitHub account"
      p_ [class_ "text-xs text-textWeak"] do
        "Using another Git host or a repository token? "
        a_ [href_ (base <> "/connect/token"), class_ "text-textBrand underline underline-offset-2"] "Connect with a token"
  toHtmlRaw = toHtml


data RepositoryTokenForm = RepositoryTokenForm
  { host :: Git.GitHost
  , apiBase :: Maybe Text
  , repoFullName :: Text
  , accessToken :: Text
  , replaceToken :: Maybe Bool
  }
  deriving stock (Generic)
  deriving anyclass (FromForm)


data RepositoryTokenGet = RepositoryTokenGet
  { projectId :: Projects.ProjectId
  , host :: Git.GitHost
  , apiBase :: Maybe Text
  , repoFullName :: Text
  , connectionError :: Maybe Text
  }
  deriving stock (Show)


repositoryTokenPage :: Projects.ProjectId -> ATAuthCtx (PageCtx RepositoryTokenGet)
repositoryTokenPage pid = do
  requireReviewWrite pid
  (_, _, bw) <- mkPageCtx pid
  pure $ PageCtx bw{pageTitle = "Connect with a token"} $ RepositoryTokenGet pid Git.GitHub Nothing "" Nothing


repositoryTokenGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (PageCtx RepositoryTokenGet))
repositoryTokenGetH pid = addRespHeaders =<< repositoryTokenPage pid


repositoryTokenPostH :: Projects.ProjectId -> RepositoryTokenForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryTokenGet))
repositoryTokenPostH pid form = do
  page <- repositoryTokenPage pid
  ctx <- Effectful.Reader.Static.ask @AuthContext
  let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      fullName = T.strip form.repoFullName
      token = T.strip form.accessToken
      safePage = page{content = (page.content :: RepositoryTokenGet){host = form.host, apiBase = form.apiBase, repoFullName = fullName}}
  result <- runExceptT do
    conn <- hoistEither $ Git.mkGitConn form.host form.apiBase token
    let (owner, name) = Git.splitFullName fullName
    when (T.null owner || T.null name) $ hoistEither $ Left "Enter the full repository name, such as team/checkout."
    repository <- ExceptT $ first (const "Could not access this repository. Check its name, server URL, and token permissions.") <$> Git.fetchRepository conn (Git.RepoRef owner name "HEAD")
    let (accountOwner, _) = Git.splitFullName repository.fullName
        accountName = if form.host == Git.GitHub then T.toLower accountOwner else accountOwner
        origin = rightToMaybe . Git.normalizeOrigin =<< mfilter (not . T.null . T.strip) form.apiBase
    accounts <- lift $ GitSync.getGitHubCredentials pid
    let observed = find (\c -> (c.host, c.apiBase, c.account) == (form.host, origin, accountName)) accounts
    whenJust observed \existing -> do
      when (isJust existing.installationId) $ hoistEither $ Left "This owner is connected through the GitHub App. Choose its existing account or grant the App access to this repository."
      plain <- lift $ GitSync.getGitHubCredential encKey pid existing.id
      when (form.replaceToken /= Just True && (plain >>= (.accessToken)) /= Just token)
        $ hoistEither
        $ Left "This owner already has a saved token. Use that connection, or select Replace saved token to update it for all repositories using this account."
    account <- ExceptT $ maybeToRight "This account changed while connecting. Reload and check its saved connection before trying again." <$> GitSync.saveTokenCredential encKey pid form.host origin accountName token observed
    existing <- lift $ GitSync.getRepositories pid
    connected <- ExceptT $ maybeToRight "This account is no longer available. Try connecting again." <$> GitSync.connectRepository pid account.id repository
    unless (any ((== connected.id) . (.id)) existing) $ lift $ liftIO $ withResource ctx.jobsPool \jobsConn -> void $ createJob jobsConn "background_jobs" $ Jobs.AnalyzeRepository pid connected.id
    lift $ liftIO $ Cache.delete ctx.repoListCache account.id
    pure connected
  case result of
    Left err -> addRespHeaders safePage{content = (safePage.content :: RepositoryTokenGet){connectionError = Just err}}
    Right repository -> do
      addSuccessToast "Repository connected" (Just "We’re analyzing its services and configuration. Reliable matches are enabled automatically.")
      redirectCS $ "/p/" <> pid.toText <> "/repositories/" <> repository.id.toText
      addRespHeaders safePage


instance ToHtml RepositoryTokenGet where
  toHtml page = section_ [class_ "mx-auto max-w-2xl space-y-6 px-4 py-6 sm:px-8 sm:py-8"] do
    let base = "/p/" <> page.projectId.toText <> "/repositories/connect"
    a_ [href_ base, class_ "text-sm text-textBrand hover:underline"] "Back to repository connections"
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Connect with a token"
      p_ [class_ "text-sm text-textWeak"] "Monoscope analyzes this repository and configures source context and configuration sync where supported. Automatic PR reviews require the GitHub App. You can disable each capability later."
    div_ [id_ "repository-token-content", class_ "space-y-4"] do
      whenJust page.connectionError $ p_ [role_ "alert", class_ "text-sm text-textError break-words"] . toHtml
      form_ [action_ (base <> "/token"), method_ "post", hxPost_ (base <> "/token"), hxTarget_ "#repository-token-content", hxSelect_ "#repository-token-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-token-indicator", class_ "space-y-4"] do
        formSelectField_ FieldSm "Git host" "host" True $ forM_ (universe @Git.GitHost) \host ->
          option_ ([value_ (Git.hostSlug host)] <> [selected_ "selected" | host == page.host]) $ toHtml $ Git.hostLabel host
        formField_ FieldSm def{value = fromMaybe "" page.apiBase, placeholder = "https://git.example.com"} "Server URL (for self-hosted Git)" "apiBase" False Nothing
        formField_ FieldSm def{value = page.repoFullName, placeholder = "team/checkout"} "Full repository name" "repoFullName" True Nothing
        formField_ FieldSm def{inputType = "password", extraAttrs = [autocomplete_ "new-password"]} "Access token" "accessToken" True Nothing
        p_ [class_ "text-xs text-textWeak"] "The token needs read access to this repository. Dashboard sync will also need write access if you enable it later."
        details_ [class_ "space-y-3"] do
          summary_ [class_ "cursor-pointer text-sm text-textWeak"] "Replacing an existing connection?"
          label_ [class_ "flex items-start gap-3 text-sm text-textStrong"] do
            input_ [type_ "checkbox", name_ "replaceToken", value_ "true", class_ "checkbox checkbox-sm shrink-0"]
            "Replace saved token"
          p_ [class_ "text-xs text-textWeak"] "This updates the source-access token for every repository using this owner’s account on this Git host."
        button_ [type_ "submit", class_ "btn btn-sm btn-primary gap-2"] do
          "Connect repository"
          htmxIndicator_ "repository-token-indicator" LdXS
  toHtmlRaw = toHtml


-- | Adopt existing dashboard grants when source access has not been configured yet.
codeContextCredentials :: Projects.ProjectId -> ATAuthCtx [GitSync.GitHubCredential]
codeContextCredentials pid =
  GitSync.getGitHubCredentials pid >>= \case
    [] -> do
      encKey <- encodeUtf8 @Text . (.apiKeyEncryptionSecretKey) . (.config) <$> Effectful.Reader.Static.ask @AuthContext
      syncs <- GitSync.getGitSyncsDecrypted encKey pid
      forM_ syncs \sync -> GitSync.upsertGitHubCredential encKey pid sync.host sync.apiBase sync.owner sync.installationId sync.accessToken
      GitSync.getGitHubCredentials pid
    accounts -> pure accounts


-- | Only a single account can be chosen implicitly; posted IDs remain project-scoped.
codeContextCredential :: Projects.ProjectId -> Maybe GitSync.GitHubCredentialId -> ATAuthCtx (Maybe GitSync.GitHubCredential)
codeContextCredential pid requested = do
  accounts <- codeContextCredentials pid
  let selected = requested <|> case accounts of [account] -> Just account.id; _ -> Nothing
  encKey <- encodeUtf8 @Text . (.apiKeyEncryptionSecretKey) . (.config) <$> Effectful.Reader.Static.ask @AuthContext
  maybe (pure Nothing) (GitSync.getGitHubCredential encKey pid) selected


-- | A connection to the credential's host, minted per call because installation tokens expire.
repoConn :: (IOE :> es, W.HTTP :> es) => EnvConfig -> GitSync.GitHubCredential -> ExceptT Text (Eff es) Git.GitConn
repoConn cfg cred = do
  creds <- hoistEither $ maybeToRight "That credential has neither an installation nor a token." $ GitSync.credentialCreds cred
  token <- ExceptT $ GitSync.githubToken cfg.githubAppId cfg.githubAppPrivateKey creds
  hoistEither $ GitSync.credentialConn cred token


-- | Save a mapping. The two path fields are derived from the sample frame path when one is
-- given and they were left blank, which is the path most users take — nobody knows offhand
-- that their container mounts the app at @\/srv\/app@.
codeMappingsPostH :: Projects.ProjectId -> CodeMappingForm -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsPostH pid form = do
  saveCodeMapping pid form
  addRespHeaders =<< codeMappingsContent pid (nonEmptyT form.samplePath) form.credentialId Nothing


saveCodeMapping :: Projects.ProjectId -> CodeMappingForm -> ATAuthCtx ()
saveCodeMapping pid form = do
  requireReviewWrite pid
  credM <- codeContextCredential pid form.credentialId
  let sample = nonEmptyT form.samplePath
  case (credM, nonEmptyT form.repo) of
    (Just cred, Just repo) -> do
      let ref = fromMaybe "main" $ nonEmptyT form.ref
          (owner, name) = case Git.splitFullName repo of ("", shortName) -> (cred.account, shortName); fullName -> fullName
          repoRef = GitSync.RepoRef owner name ref
          typed = (fromMaybe "" form.pathPrefix, fromMaybe "" form.sourceRoot)
      derived <- case sample of
        Just s | typed == ("", "") -> deriveFromRepo cred repoRef s
        _ -> pure $ Right typed
      case derived of
        Left err -> addErrorToast "Could not work out the mapping" (Just err)
        Right (prefix, root) -> do
          let svc = nonEmptyT form.service
          CodeContext.insertCodeMapping pid cred.id repoRef svc prefix root
          addSuccessToast (repo <> " linked") Nothing
    (Nothing, _) -> addErrorToast "Choose a repository account" (Just "Select an account connected to this project, or connect another account.")
    (_, Nothing) -> addErrorToast "Pick a repository" Nothing
  where
    -- Work the two path fields out of one real frame path by matching it against the repo's
    -- file list. The file that matched is dropped: what the user checks is the mapping the row
    -- then spells out, not a path they would have to compare by eye.
    deriveFromRepo :: GitSync.GitHubCredential -> GitSync.RepoRef -> Text -> ATAuthCtx (Either Text (Text, Text))
    deriveFromRepo cred repoRef sample = do
      cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
      W.runHTTPWreq $ runExceptT do
        conn <- repoConn cfg cred
        -- Whole tree, not a prefix: the point is to find where the frame path lands, and we do not
        -- yet know which directory that is. Paths only, so Bitbucket's missing blob shas cost nothing.
        (_, entries) <- ExceptT $ first (("Could not read " <> repoRef.repo <> ": ") <>) <$> Git.fetchTree conn repoRef ""
        hoistEither
          $ maybeToRight ("No file in " <> repoRef.repo <> "@" <> repoRef.ref <> " lines up with " <> sample <> ". Check the branch, or fill the paths in under Advanced.")
          $ (\(prefix, root, _) -> (prefix, root))
          <$> CodeContext.deriveMapping (map (.path) entries) sample


codeMappingsDeleteH :: Projects.ProjectId -> CodeContext.CodeMappingId -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsDeleteH pid mid = do
  requireReviewWrite pid
  CodeContext.deleteCodeMapping pid mid
  addRespHeaders =<< codeMappingsContent pid Nothing Nothing Nothing


-- | What a mapping covers, in the terms the reader has: frame paths, not columns.
--
-- >>> import Data.Default (def)
-- >>> scopeLabel def
-- "all frames \8594 repo root"
-- >>> scopeLabel def{CodeContext.pathPrefix = "/srv/app/", CodeContext.sourceRoot = "src"}
-- "frames under /srv/app/ \8594 src/"
scopeLabel :: CodeContext.CodeMapping -> Text
scopeLabel cm = from <> " → " <> to
  where
    from = if T.null cm.pathPrefix then "all frames" else "frames under " <> cm.pathPrefix
    to = if T.null cm.sourceRoot then "repo root" else T.dropWhileEnd (== '/') cm.sourceRoot <> "/"


impactReviewSettingsPostH :: Projects.ProjectId -> ImpactReviews.ReviewSettings -> ATAuthCtx (RespHeaders (Html ()))
impactReviewSettingsPostH pid settings = do
  requireReviewWrite pid
  ImpactReviews.updateSettings pid settings{ImpactReviews.owner = T.toLower settings.owner, ImpactReviews.repo = T.toLower settings.repo}
  addSuccessToast "Review settings saved" Nothing
  addRespHeaders =<< codeMappingsContent pid Nothing Nothing Nothing


impactReviewRetryH :: Projects.ProjectId -> ImpactReviews.ReviewId -> ATAuthCtx (RespHeaders (Html ()))
impactReviewRetryH pid rid = do
  requireReviewWrite pid
  ImpactReviews.retryRun pid rid
  repositoriesGetH pid (Just PullRequests) Nothing


requireReviewWrite :: Projects.ProjectId -> ATAuthCtx ()
requireReviewWrite = void . ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit
