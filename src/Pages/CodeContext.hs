{-# LANGUAGE OrPatterns #-}

-- | The source behind one stack frame.
--
-- Its own module rather than a function in "Pages.LogExplorer.LogItem", because the model it
-- needs ("Models.Projects.CodeContext") reaches GitHub through @Models.Projects.GitSync@,
-- which reaches @Pkg.Components.Widget@, which imports @LogItem@ — a cycle. The renderer in
-- "Pages.Components" only ever builds the URL, so nothing on the rendering side has to know
-- this module exists.
module Pages.CodeContext (wikiGetH, memoryGetH, memoryPostH, MemoryForm (..), WikiGet (..), repositoryAnalyzePostH, repositoryResumePostH, repositoryFolderPostH, repositoryConflictPostH, FolderForm (..), ConflictForm (..), repositoryTokenGetH, repositoryTokenPostH, RepositoryTokenGet (..), RepositoryTokenForm (..), codeContextH, FrameSource (..), repositoriesGetH, RepositoriesGet (..), serviceRepositoriesGetH, repositoryGetH, repositoryDeleteH, RepositoryRemoval (..), repositorySourceGetH, RepositorySourceGet, repositorySourcePostH, repositorySourceDeleteH, repositoryReviewsPostH, ReviewChange (..), repositorySourceEnabledPostH, SourceContextForm (..), repositoryConnectGetH, repositoryConnectPostH, RepositoryConnectGet (..), RepositoryConnectForm (..), RepositoryGet (..), ReviewReadiness (..), RepositoryTab (..), codeMappingsEditorGetH, codeMappingsPostH, codeMappingsDeleteH, CodeMappingForm (..), MappingsPanel, MappingRejection (..), impactReviewSettingsPostH, impactReviewRetryH, issueFixGetH, issueFixEvidenceH, issueFixMediaH, issueFixPackageH, rangedMedia) where

import BackgroundJobs.Types qualified as Jobs
import Commonmark qualified as Markdown
import Commonmark.Blocks qualified as MarkdownBlocks
import Commonmark.Extensions (HasWikilinks (..), TitlePosition (..), gfmExtensions, wikilinksSpec)
import Commonmark.Tag qualified as MarkdownTag
import Commonmark.TokParsers (symbol)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Algorithm.Diff qualified as Diff
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.Default (def)
import Data.Effectful.AgentArtifacts qualified as Artifacts
import Data.Effectful.CodeReview qualified as Agent
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUIDEff
import Data.Effectful.Wreq qualified as W
import Data.Foldable1 qualified as F1
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Pool (withResource)
import Data.Scientific (FPFormat (Fixed), formatScientific)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Effectful (Eff, IOE, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static qualified
import Hasql.Interpolate qualified as HI
import Lucid hiding (for_)
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxDelete_, hxGet_, hxInclude_, hxIndicator_, hxPost_, hxPushUrl_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Issues qualified as Issues
import Models.Projects.AgentUsage qualified as Usage
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.ImpactReviews qualified as ImpactReviews
import Models.Projects.IssueFixes qualified as IssueFixes
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.ProjectMemory qualified as Memory
import Models.Projects.Projects qualified as Projects
import Models.Projects.RepositorySync qualified as RepositorySync
import Network.HTTP.Types qualified as H
import Network.HTTP.Types.Header qualified as Header
import Network.Wai qualified as Wai
import Numeric (showFFloat)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), bodyWrapper, mkPageCtx)
import Pages.Components (BadgeColor (..), EmptyStateAction (..), EmptyStateCfg (..), FieldCfg (..), FieldSize (..), PanelCfg (..), PanelStyle (..), callout_, colorChip_, dialogTrigger_, disclosure_, emptyState_, filterInputAttr_, formField_, formSelectField_, installationSettingsLink_, localTimeFmt_, nativeDialog_, options_, panel_, settingsH2_)
import Pkg.Components.Table qualified as Table
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.Git qualified as Git
import Pkg.ImpactReview qualified as ImpactReview
import Relude
import Servant (Header, Headers, ServerError (errHeaders), addHeader, err403, err404, err503, getResponse)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ActionResult, DB, Done, Outcome (..), Refusal (..), Rendered (..), RespHeaders, Toast (..), ToastLevel (..), act, addErrorToast, addRespHeaders, addSuccessToast, done, redirectCS)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Parsec qualified as Parsec
import Text.Slugify (slugify)
import Utils (LoadingSize (..), TabStrip (..), countNoun, faSprite_, htmxIndicator_, navTabStrip_, nonEmptyT, renderMarkdown)
import Web.ApiTypes (readOnlyMessage)
import Web.Auth (demoGuestSessionId)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (toUrlPiece)
import Web.Links (CookieProtectedRoutes (..), IssuesRoutes' (..), RepositoryTab (..), urls)


issueFixForViewer :: (DB es, Error ServerError :> es) => Projects.ProjectId -> IssueFixes.FixId -> Projects.Session -> Eff es (Agent.FixResult, Agent.Explanation)
issueFixForViewer pid fid session = do
  let missing = err404{errHeaders = [(Header.hCacheControl, "private, no-store")]}
  when (session.persistentSession.id == demoGuestSessionId) $ throwError missing
  maybe (throwError missing) pure =<< IssueFixes.mediaFor pid fid (IssueFixes.Member session.user.id)


issueFixGetH :: Projects.ProjectId -> IssueFixes.FixId -> ATAuthCtx (Headers '[Header "Cache-Control" Text] (Html ()))
issueFixGetH pid fid = do
  session <- Projects.getSession
  (result, explanation) <- issueFixForViewer pid fid session
  (_, _, bw) <- mkPageCtx pid
  totals <- Usage.usageTotals Usage.Fixes pid
  void $ issueFixForViewer pid fid session
  pure $ addHeader @"Cache-Control" ("private, no-store" :: Text) $ bodyWrapper bw{pageTitle = "Issue fix"} $ div_ [class_ "h-full overflow-y-auto"] $ section_ [class_ "mx-auto max-w-5xl space-y-6 px-4 py-6 sm:px-8"] do
    a_ [href_ (urls.repositoriesGet pid (Just Fixes) Nothing), class_ "text-sm text-textBrand hover:underline"] "All issue fixes"
    issueFixEvidence_ pid fid result explanation
    section_ [class_ "space-y-2 text-sm text-textWeak"] do
      h2_ [class_ "font-medium text-textStrong"] "Recorded usage"
      for_ (find ((== Just fid) . (.runId)) totals) \total -> do
        p_ $ toHtml $ usageText total.usage
        p_ $ toHtml $ resourceText total.resources


issueFixEvidenceH :: Projects.ProjectId -> IssueFixes.FixId -> ATAuthCtx (Html ())
issueFixEvidenceH pid fid = do
  session <- Projects.getSession
  when (session.persistentSession.id == demoGuestSessionId) $ throwError err404
  media <- IssueFixes.mediaFor pid fid (IssueFixes.Member session.user.id)
  pure $ maybe (p_ [role_ "status", class_ "text-sm text-textWeak"] "No explanation is available for this fix.") (uncurry $ issueFixEvidence_ pid fid) media


issueFixEvidence_ :: Projects.ProjectId -> IssueFixes.FixId -> Agent.FixResult -> Agent.Explanation -> Html ()
issueFixEvidence_ pid fid result explanation =
  let media = urls.issueFixMedia pid fid
   in section_ [Aria.label_ "Verified fix evidence", class_ "space-y-6"] do
        header_ [class_ "space-y-2"] do
          h2_ [class_ "text-xl font-semibold text-textStrong break-words"] $ toHtml result.summary
          p_ [class_ "text-sm text-textWeak"] do
            toHtml @Text "Verified candidate "
            code_ $ toHtml $ T.take 12 $ IssueFixes.revisionText result.head
          p_ [class_ "text-sm text-textWeak"] $ toHtml @Text $ "Inferred cause: " <> result.rootCause
        video_ [term "controls" "", term "preload" "metadata", term "playsinline" "", term "aria-label" "Bug reproduction and verified fix", term "poster" (media Artifacts.Before), width_ (show explanation.width), height_ (show explanation.height), class_ "aspect-video h-auto w-full rounded-lg border border-strokeWeak"] do
          source_ [src_ (media Artifacts.Video), type_ "video/mp4"]
          track_ [src_ (media Artifacts.Captions), term "kind" "captions", term "srclang" "en", label_ "English captions", term "default" ""]
          p_ $ a_ [href_ (media Artifacts.Video), term "download" "explanation.mp4"] "Download the explanation video"
        p_ [class_ "text-sm text-textWeak"] "The video uses retained verification evidence. The cause is an agent inference."
        figure_ [class_ "space-y-2"] do
          img_ [src_ (media Artifacts.Diagram), alt_ "Diagram of the reproduced failure, inferred cause, change, and passing regression", class_ "w-full rounded-lg border border-strokeWeak"]
          figcaption_ [class_ "text-sm text-textWeak"] $ a_ [href_ (media Artifacts.Captions), class_ "text-textBrand hover:underline"] "Read the video captions"
        section_ [class_ "space-y-2"] do
          h3_ [class_ "text-base font-semibold text-textStrong"] "Pull request package"
          p_ [class_ "text-sm text-textWeak"] "A short description, video, diagram, captions, and verification metadata. Prepared for review; not published."
          a_ [href_ (urls.issueFixPackage pid fid), term "download" "publication.tar", class_ "btn btn-sm btn-outline"] "Download PR package"


issueFixPackageH :: Projects.ProjectId -> IssueFixes.FixId -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> ATAuthCtx Wai.ResponseReceived
issueFixPackageH pid fid request send = do
  session <- Projects.getSession
  (result, explanation) <- issueFixForViewer pid fid session
  let headers = [(Header.hCacheControl, "private, no-store"), (Header.hVary, "Cookie, Authorization"), ("X-Content-Type-Options", "nosniff")]
  if Wai.requestMethod request `notElem` [H.methodGet, H.methodHead]
    then liftIO $ send $ Wai.responseLBS H.status405 ((Header.hAllow, "GET, HEAD") : headers) ""
    else do
      origin <- (.config.hostUrl) <$> Effectful.Reader.Static.ask @AuthContext
      archive <- either (const $ throwError err503{errHeaders = [(Header.hCacheControl, "private, no-store")]}) (pure . Artifacts.archiveBytes) =<< Artifacts.packageFixMedia result explanation origin
      void $ issueFixForViewer pid fid session
      liftIO $ send $ Wai.responseLBS H.status200 (headers <> [(Header.hContentType, "application/x-tar"), (Header.hContentDisposition, "attachment; filename=publication.tar"), (Header.hContentLength, encodeUtf8 $ show $ LBS.length archive)]) $ if Wai.requestMethod request == H.methodHead then "" else archive


issueFixMediaH :: Projects.ProjectId -> IssueFixes.FixId -> Artifacts.Asset -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> ATAuthCtx Wai.ResponseReceived
issueFixMediaH pid fid asset request send = do
  session <- Projects.getSession
  (_, explanation) <- issueFixForViewer pid fid session
  if Wai.requestMethod request `notElem` [H.methodGet, H.methodHead]
    then liftIO $ send $ Wai.responseLBS H.status405 [(Header.hAllow, "GET, HEAD"), (Header.hCacheControl, "private, no-store")] ""
    else do
      bytes <- either (const $ throwError err503{errHeaders = [(Header.hCacheControl, "private, no-store")]}) (pure . Artifacts.verifiedBytes) =<< Artifacts.readFixMedia explanation asset
      void $ issueFixForViewer pid fid session
      liftIO $ send $ rangedMedia request [(Header.hContentType, Artifacts.assetMime asset), (Header.hCacheControl, "private, no-store"), (Header.hVary, "Cookie, Authorization")] bytes


-- | Media bytes with single-range support, which Safari needs to play video.
rangedMedia :: Wai.Request -> [Header.Header] -> LBS.ByteString -> Wai.Response
rangedMedia request extra bytes = case selected of
  Nothing -> Wai.responseLBS H.status416 ((Header.hContentRange, "bytes */" <> encodeUtf8 (show size)) : headers) ""
  Just (status, (startByte, lastByte)) ->
    let byteCount = lastByte - startByte + 1
        responseHeaders = headers <> [(Header.hContentLength, encodeUtf8 $ show byteCount)] <> [(Header.hContentRange, encodeUtf8 $ "bytes " <> show startByte <> "-" <> show lastByte <> "/" <> show size) | status == H.status206]
        body = if Wai.requestMethod request == H.methodHead then "" else LBS.take (fromIntegral byteCount) $ LBS.drop (fromIntegral startByte) bytes
     in Wai.responseLBS status responseHeaders body
  where
    size = fromIntegral $ LBS.length bytes :: Integer
    headers = extra <> [(Header.hAcceptRanges, "bytes"), ("X-Content-Type-Options", "nosniff"), ("Content-Security-Policy", "default-src 'none'; sandbox")]
    selected = case lookup Header.hRange $ Wai.requestHeaders request of
      Just value
        | Wai.requestMethod request == H.methodGet
        , Just [range] <- Header.parseByteRanges value ->
            (H.status206,) <$> case range of
              Header.ByteRangeFrom startByte | startByte < size -> Just (startByte, size - 1)
              Header.ByteRangeFromTo startByte lastByte | startByte < size && startByte <= lastByte -> Just (startByte, min lastByte $ size - 1)
              Header.ByteRangeSuffix count | count > 0 -> Just (max 0 $ size - count, size - 1)
              _ -> Nothing
      _ -> Just (H.status200, (0, size - 1))


-- | Source around one stack frame, read from the repository linked to the project.
--
-- Every outcome renders as a line of prose inside the frame's panel rather than as an error
-- response: this is a progressive enhancement on a stack trace that already reads fine
-- without it, and a project that has never configured a code mapping must not be shown a
-- failure it did not cause. The reasons are distinguished, though — "no mapping covers this
-- path" and "that line is past the end of the file" have different fixes, and collapsing
-- them into silence sends the reader to configure something already configured.
codeContextH :: Projects.ProjectId -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders FrameSource)
codeContextH pid fileM lineM svcM revM = do
  _ <- Projects.sessionAndProject pid
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  addRespHeaders =<< case (nonEmptyT fileM, lineM) of
    (Just path, Just n) -> either (SnippetUnavailable pid) SnippetFound <$> W.runHTTPWreq (CodeContext.fetchSnippet authCtx.codeBlobCache authCtx.config pid svcM (nonEmptyT revM) path n)
    _ -> pure NoFrameLine


data FrameSource = NoFrameLine | SnippetUnavailable Projects.ProjectId CodeContext.SnippetError | SnippetFound CodeContext.Snippet
  deriving stock (Eq, Show)


instance ToHtml FrameSource where
  toHtmlRaw = toHtml
  toHtml = \case
    NoFrameLine -> note_ "This frame has no file and line to look up." Nothing
    -- An unmapped frame is the one failure the reader can act on from here, so it is the one
    -- that gets a control — carrying the path, so the form it opens is already filled in.
    SnippetUnavailable pid err -> note_ (CodeContext.snippetErrorMessage err) case err of
      CodeContext.NoMapping path -> Just $ urls.repositoriesGet pid Nothing (Just path)
      (CodeContext.CredentialGone; CodeContext.ReadFailed{}; CodeContext.ConnectionInvalid{}; CodeContext.LineOutOfRange{}) -> Nothing
    -- No empty-body arm: 'fetchSnippet' returns a 'Left' for a line past the end of the
    -- file, so a 'Snippet' that reaches here has source in it.
    SnippetFound s ->
      div_ [class_ "mt-1 rounded-md border border-strokeWeak overflow-hidden font-mono text-2xs leading-relaxed"]
        $ forM_ (zip [s.startLine ..] s.body) \(n, src) ->
          -- The failing line is marked by a background AND a gutter caret, never by
          -- colour alone: it is the one line in this panel a reader must not miss.
          div_ [class_ $ "flex " <> bool "" "bg-fillError-weak" (n == s.focusLine)] do
            span_ [class_ "shrink-0 w-12 px-2 text-right tabular-nums text-textWeak select-none border-r border-strokeWeak"] $ toHtml @Text (show n)
            span_ [class_ $ "shrink-0 w-3 text-center " <> bool "text-transparent" "text-textError" (n == s.focusLine)] $ toHtml @Text "›"
            span_ [class_ "px-2 whitespace-pre overflow-x-auto c-scroll"] $ toHtml src
    where
      note_ :: Monad m => Text -> Maybe Text -> HtmlT m ()
      note_ msg fixUrlM = div_ [class_ "pl-5 py-1 text-2xs text-textWeak italic flex items-center gap-2"] do
        toHtml msg
        whenJust fixUrlM \url -> a_ [href_ url, class_ "not-italic text-textBrand underline shrink-0"] "Choose a repository"


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
data MemoryForm = MemoryForm {pageId :: Memory.PageId, version :: Word32, path :: Maybe Text, title :: Text, body :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | @NewPage@ carries the path to prefill; @Landing@ is the wiki index.
data WikiSelection = Landing | NewPage Text | ById Memory.PageId | ByPath Memory.PagePath


data WikiTree = WikiTree (Maybe Memory.Page) (Map Text WikiTree)


-- | Markdown rendered for the wiki, where @[[path|label]]@ links to another page.
data WikiDoc


instance Markdown.Rangeable (Markdown.Html WikiDoc) where
  ranged _ = id


-- | Valid targets become @wiki:@ hrefs that the renderer resolves to the project wiki; others stay plain text.
instance {-# OVERLAPPING #-} HasWikilinks (Markdown.Html WikiDoc) where
  wikilink target label = either (const label) (\path -> Markdown.addAttribute ("class", "wikilink") $ Markdown.link ("wiki:" <> Memory.pagePathText path) "" label) $ Memory.mkPagePath target


wikiGetH :: Projects.ProjectId -> [Text] -> Maybe Memory.PagePath -> Maybe Text -> Maybe Text -> Bool -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
wikiGetH pid segments after search folder edit = do
  when (any (T.any (== '/')) segments) $ throwError err404
  selection <- if null segments then pure $ maybe Landing (NewPage . (\f -> bool (f <> "/") "" $ T.null f) . T.dropAround (== '/')) folder else either (const $ throwError err404) (pure . ByPath) $ Memory.mkPagePath $ T.intercalate "/" segments
  memoryPageH pid selection after (nonEmptyT $ T.strip <$> search) edit Nothing


memoryGetH :: Projects.ProjectId -> Maybe Memory.PageId -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
memoryGetH pid page = memoryPageH pid (maybe Landing ById page) Nothing Nothing False Nothing


wikiPageUrl :: Projects.ProjectId -> Memory.PagePath -> Bool -> Text
wikiPageUrl pid path = urls.projectWikiGet pid (T.splitOn "/" $ Memory.pagePathText path) Nothing Nothing Nothing


memoryPostH :: Projects.ProjectId -> MemoryForm -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
memoryPostH pid form = do
  requireReviewWrite pid
  session <- Projects.getSession
  -- A blank path, or one ending in a folder, takes its page name from the title.
  let named path = bool path (path <> slugify form.title) $ T.null path || "/" `T.isSuffixOf` path
  case (,) <$> traverse (Memory.mkPagePath . named . T.strip) form.path <*> (Memory.Document <$> Memory.mkMemoryText form.title <*> Memory.mkMemoryText form.body) of
    Left message -> redraft message
    Right (path, document) ->
      Memory.writePage pid form.pageId form.version path document (Memory.Member session.user.id) >>= \case
        Left Memory.StaleRevision -> redraft "This page changed while you were editing. Your draft is below. Open the latest revision before saving again."
        Left Memory.PathTaken -> redraft "Another page already uses this path. Your draft is below. Choose a different path."
        Right page -> do
          addSuccessToast "Wiki page saved" Nothing
          redirectCS $ wikiPageUrl pid page.path False
          memoryGetH pid $ Just form.pageId
  where
    redraft message = memoryPageH pid (ById form.pageId) Nothing Nothing False (Just (form, message))


data WikiGet = WikiGet
  { projectId :: Projects.ProjectId
  , canEdit :: Bool
  , editing :: Bool
  , pages :: [Memory.Page]
  , search :: Maybe Text
  , backlinks :: [Memory.Page]
  , history :: [Memory.Page]
  , form :: MemoryForm
  , message :: Maybe Text
  }
  deriving stock (Generic, Show)


memoryPageH :: Projects.ProjectId -> WikiSelection -> Maybe Memory.PagePath -> Maybe Text -> Bool -> Maybe (MemoryForm, Text) -> ATAuthCtx (RespHeaders (PageCtx WikiGet))
memoryPageH pid selection after search editRequested draft = do
  (session, _, bw) <- mkPageCtx pid
  when (session.persistentSession.id == demoGuestSessionId) $ throwError err404
  canEdit <- maybe False (>= ProjectMembers.PEdit) <$> ProjectMembers.getUserPermission pid session.user.id
  -- @newPath@ is the path a new page starts at; an editor following a link to a missing page gets one there.
  (selected, newPath) <- case selection of
    Landing -> pure (Nothing, Nothing)
    NewPage path -> pure (Nothing, Just path)
    ById page -> pure (Just page, Nothing)
    ByPath path ->
      Memory.pageAtPath pid path >>= \case
        Just page -> pure (Just page, Nothing)
        Nothing | canEdit -> pure (Nothing, Just $ Memory.pagePathText path)
        Nothing -> throwError err404
  pages <- Memory.pageWindow pid (maybe Memory.AllPages Memory.Matching search) after
  history <- maybe (pure []) (Memory.pageHistory pid) selected
  let current = listToMaybe history
  when (isJust selected && isNothing current && isNothing draft) $ throwError err404
  backlinks <- maybe (pure []) (\page -> Memory.pageWindow pid (Memory.LinkingTo page.path) Nothing) current
  fresh <- maybe (UUIDId <$> UUIDEff.genUUID) pure selected
  let edit = maybe (MemoryForm fresh 0 (newPath <|> Just "") "" "") (\page -> MemoryForm page.id (fromIntegral page.version) (Just $ Memory.pagePathText page.path) (Memory.memoryText page.document.title) (Memory.memoryText page.document.body)) current
  let editing = canEdit && (editRequested || isJust draft || isJust newPath)
      actions = div_ [class_ "flex items-center gap-2"] case (editing, current) of
        (True, _) -> do
          span_ [id_ "wiki-dirty", hidden_ "", class_ "text-xs text-textWeak", [__|on input from #wiki-form remove @hidden from me|]] "Unsaved changes"
          for_ current \page -> a_ [href_ $ wikiPageUrl pid page.path False, class_ "btn btn-ghost btn-sm", [__|on click if #wiki-dirty.hidden is false and not confirm('Discard unsaved changes?') halt the event|]] "Cancel"
          button_ [type_ "submit", term "form" "wiki-form", title_ "Save (\x2318S)", class_ "btn btn-primary btn-sm"] "Save page"
        (False, Just page) -> a_ (class_ "btn btn-primary btn-sm gap-1.5" : repositoryNavAttrs (wikiPageUrl pid page.path True)) $ faSprite_ "pen-to-square" "regular" "h-3.5 w-3.5" >> "Edit"
        (False, Nothing) -> a_ (class_ "btn btn-primary btn-sm gap-1.5" : repositoryNavAttrs (urls.projectWikiGet pid [] Nothing Nothing (Just "") False)) $ faSprite_ "plus" "regular" "h-3.5 w-3.5" >> "New page"
  addRespHeaders $ PageCtx (repositoryBWConfig bw pid Wiki Nothing canEdit True){pageActions = actions <$ guard canEdit} $ WikiGet pid canEdit editing pages search backlinks history (maybe edit fst draft) (snd <$> draft)


instance ToHtml WikiGet where
  toHtml WikiGet{projectId = pid, canEdit, editing, pages, search, backlinks, history, form, message} = toHtmlRaw $ repositoryLayout_ Nothing do
    let current = listToMaybe history
        base = urls.projectWikiGet pid [] Nothing Nothing Nothing False
        newPageUrl folder = urls.projectWikiGet pid [] Nothing Nothing (Just folder) False
        pageLink page = wikiPageUrl pid page.path False
        visible = take Memory.pageLimit pages
        displayed = visible <> [page | page <- maybeToList current, all ((/= page.id) . (.id)) visible]
        title page = toHtml $ Memory.memoryText page.document.title
        author = \case
          Memory.Member _ -> "Project member"
          Memory.Agent _ -> "Agent"
          Memory.Conversation _ -> "AI conversation"
          Memory.Run run -> a_ [href_ $ urls.aiRoutineRunGet pid run Nothing, class_ "underline underline-offset-2"] "Agent run"
        edited page = span_ [class_ "tabular-nums"] do
          toHtml @Text $ "Revision " <> show page.version <> " · "
          author page.author
          " · "
          localTimeFmt_ "MMM d, yyyy HH:mm" page.updatedAt
        baseSyntax = wikilinksSpec TitleAfterPipe <> gfmExtensions <> Markdown.defaultSyntaxSpec
        -- Embedded HTML is text; only Markdown constructs may create markup.
        syntax =
          baseSyntax
            { Markdown.syntaxBlockSpecs = filter ((/= (MarkdownBlocks.rawHtmlSpec @Identity @(Markdown.Html WikiDoc) @(Markdown.Html WikiDoc)).blockType) . (.blockType)) baseSyntax.syntaxBlockSpecs
            , Markdown.syntaxInlineParsers = (Markdown.str . Markdown.untokenize <$> Parsec.try ((:) <$> symbol '<' <*> MarkdownTag.htmlTag)) : baseSyntax.syntaxInlineParsers
            }
        pageBody body = div_ [class_ "prose max-w-none break-words text-textStrong [&_pre]:overflow-x-auto"] $ case runIdentity $ Markdown.commonmarkWith syntax "" body of
          Left _ -> toHtml body
          Right (parsed :: Markdown.Html WikiDoc) -> toHtmlRaw $ sanitizeBalance $ T.replace "href=\"wiki:" ("href=\"" <> base <> "/") $ toStrict $ Markdown.renderHtml parsed
        revisionLines page = ("Title: " <> Memory.memoryText page.document.title) : "" : lines (Memory.memoryText page.document.body)
        revisionDiff old new = pre_ [class_ "overflow-x-auto rounded-lg bg-fillWeak py-2 text-xs leading-relaxed"] $ for_ (Diff.getGroupedDiff (revisionLines old) (revisionLines new)) \case
          Diff.First removed -> diffLines "bg-fillError-weak text-textError" "- " removed
          Diff.Second added -> diffLines "bg-fillSuccess-weak text-textSuccess" "+ " added
          Diff.Both same _ -> diffLines "text-textWeak" "  " $ if length same > 5 then take 2 same <> ["\x22EF " <> show (length same - 4) <> " unchanged lines"] <> drop (length same - 2) same else same
          where
            diffLines cls sigil = traverse_ \line -> span_ [class_ $ "block whitespace-pre-wrap break-words px-2 " <> cls] $ toHtml @Text $ sigil <> line
        newPageIn folder = a_ ([Aria.label_ $ "New page in " <> folder, title_ "New page", class_ "btn btn-ghost btn-xs btn-square shrink-0 opacity-0 group-hover/row:opacity-100 focus-visible:opacity-100"] <> repositoryNavAttrs (newPageUrl folder)) $ faSprite_ "plus" "regular" "h-3 w-3"
    div_ [class_ "grid min-h-full md:grid-cols-[16rem_minmax(0,1fr)]"] do
      nav_ [Aria.label_ "Wiki pages", class_ "min-w-0 space-y-3 border-strokeWeak pb-6 md:sticky md:top-0 md:max-h-[calc(100dvh-8rem)] md:self-start md:overflow-y-auto md:border-r md:pr-3", term "preload" "mouseover"] do
        div_ [class_ "flex items-center justify-between gap-2 ps-2"] do
          span_ [class_ "text-xs font-semibold uppercase tracking-wider text-textWeak"] "Pages"
          when canEdit $ a_ ([Aria.label_ "New page", title_ "New page", class_ "btn btn-ghost btn-sm btn-square"] <> repositoryNavAttrs (newPageUrl "")) $ faSprite_ "plus" "regular" "h-3.5 w-3.5"
        form_ [method_ "get", action_ base, role_ "search", class_ "relative"] do
          faSprite_ "magnifying-glass" "regular" "pointer-events-none absolute start-2.5 top-1/2 z-10 h-3.5 w-3.5 -translate-y-1/2 text-iconNeutral"
          input_ [type_ "search", id_ "wiki-search", name_ "q", value_ $ fromMaybe "" search, placeholder_ "Search pages", Aria.label_ "Search wiki pages", class_ "input input-sm w-full ps-8", hxGet_ base, hxTrigger_ "input changed delay:300ms, search", hxTarget_ "#repository-content", hxSelect_ "#repository-content", hxSwap_ "outerMorph", hxPushUrl_ "true"]
        when (null pages) $ p_ [class_ "px-2 text-sm text-textWeak"] $ maybe "No pages yet." (\q -> toHtml $ "No pages match \x201C" <> q <> "\x201D.") search
        let insert page [] (WikiTree _ children) = WikiTree (Just page) children
            insert page (segment : rest) (WikiTree item children) = WikiTree item $ Map.alter (Just . insert page rest . fromMaybe (WikiTree Nothing mempty)) segment children
            tree = foldr (\page -> insert page $ T.splitOn "/" $ Memory.pagePathText page.path) (WikiTree Nothing mempty) displayed
            isCurrent item = Just item.id == ((.id) <$> current)
            link item = a_ ([class_ $ "flex min-w-0 items-center gap-2 rounded-md px-2 py-1.5 text-sm focus-visible:outline-2 focus-visible:outline-strokeFocus " <> bool "text-textWeak hover:bg-fillWeak hover:text-textStrong" "bg-fillBrand-weak font-medium text-textStrong" (isCurrent item), title_ $ Memory.pagePathText item.path] <> [term "aria-current" "page" | isCurrent item] <> repositoryNavAttrs (pageLink item)) do
              faSprite_ "file-lines" "regular" "h-3.5 w-3.5 shrink-0"
              span_ [class_ "truncate"] $ title item
            renderTree prefix (WikiTree _ children) = ul_ [class_ "space-y-px"] $ for_ (Map.toAscList children) \(segment, branch@(WikiTree page descendants)) -> li_ do
              let folder = T.intercalate "/" (prefix <> [segment])
                  holdsCurrent = maybe True (\item -> Memory.pagePathText item.path == folder || (folder <> "/") `T.isPrefixOf` Memory.pagePathText item.path) current
              if Map.null descendants
                then for_ page link
                else details_ ([class_ "group/folder"] <> [open_ "" | holdsCurrent]) do
                  summary_ [class_ "group/row flex cursor-pointer list-none items-center gap-1 rounded-md py-1 ps-1 pe-1 text-sm text-textStrong hover:bg-fillWeak [&::-webkit-details-marker]:hidden"] do
                    faSprite_ "chevron-right" "regular" "h-3 w-3 shrink-0 text-iconNeutral transition-transform group-open/folder:rotate-90"
                    faSprite_ "folder" "regular" "h-3.5 w-3.5 shrink-0 text-iconNeutral"
                    span_ [class_ "min-w-0 flex-1 truncate"] $ toHtml segment
                    when canEdit $ newPageIn folder
                  div_ [class_ "ms-3 border-s border-strokeWeak ps-1"] do
                    for_ page link
                    renderTree (prefix <> [segment]) branch
        case search of
          Nothing -> renderTree [] tree
          Just _ -> ul_ [class_ "space-y-px"] $ for_ visible \page -> li_ $ a_ (class_ "block rounded-md px-2 py-1.5 hover:bg-fillWeak" : repositoryNavAttrs (pageLink page)) do
            span_ [class_ "block truncate text-sm text-textStrong"] $ title page
            span_ [class_ "block truncate font-mono text-xs text-textWeak"] $ toHtml $ Memory.pagePathText page.path
        when (length pages > Memory.pageLimit) $ for_ (viaNonEmpty last visible) \page ->
          a_ (class_ "block px-2 py-1.5 text-sm text-textBrand hover:underline" : repositoryNavAttrs (urls.projectWikiGet pid [] (Just page.path) search Nothing False)) "More pages"
      article_ [class_ "min-w-0 pb-16 md:px-10"] do
        header_ [class_ "sticky top-0 z-10 flex min-h-12 flex-wrap items-center gap-x-3 gap-y-1 border-b border-strokeWeak bg-bgBase py-2"] do
          nav_ [Aria.label_ "Wiki breadcrumb", class_ "flex min-w-0 flex-1 items-center gap-1 text-sm text-textWeak"] do
            a_ (class_ "hover:text-textStrong" : repositoryNavAttrs base) "Wiki"
            for_ (maybe (bool [] ["New page"] editing) (T.splitOn "/" . Memory.pagePathText . (.path)) current) \segment -> do
              faSprite_ "chevron-right" "regular" "h-2.5 w-2.5 shrink-0"
              span_ [class_ "truncate last:text-textStrong"] $ toHtml segment
          unless editing $ for_ current $ p_ [class_ "text-xs text-textWeak"] . edited
        div_ [class_ "mx-auto max-w-3xl"] do
          unless editing $ case current of
            Just page -> div_ [class_ "space-y-6 pt-8"] do
              h2_ [class_ "text-3xl font-bold tracking-tight text-balance text-textStrong"] $ title page
              pageBody $ Memory.memoryText page.document.body
            Nothing -> div_ [class_ "space-y-4 pt-16"] do
              h2_ [class_ "text-2xl font-semibold tracking-tight text-textStrong"] "Project wiki"
              p_ [class_ "max-w-prose text-sm text-textWeak"] "Shared notes and runbooks for this project. Check dated conclusions against current source and telemetry."
              unless (null pages) $ section_ [Aria.labelledby_ "wiki-recent", class_ "space-y-1 pt-4"] do
                h3_ [id_ "wiki-recent", class_ "text-xs font-semibold uppercase tracking-wider text-textWeak"] "Recently updated"
                ul_ [class_ "divide-y divide-strokeWeak"] $ for_ (take 8 $ sortOn (Down . (.updatedAt)) visible) \page -> li_ $ a_ (class_ "flex items-baseline justify-between gap-4 py-2 hover:text-textBrand" : repositoryNavAttrs (pageLink page)) do
                  span_ [class_ "truncate text-sm text-textStrong"] $ title page
                  span_ [class_ "shrink-0 text-xs text-textWeak tabular-nums"] $ localTimeFmt_ "MMM d, HH:mm" page.updatedAt
          when editing $ div_ [class_ "pt-8"] do
            when (isNothing current) $ h2_ [class_ "sr-only"] "New page"
            form_ [id_ "wiki-form", method_ "post", action_ (urls.projectMemoryPost pid), hxPost_ (urls.projectMemoryPost pid), hxTarget_ "#repository-content", hxSelect_ "#repository-content", hxSwap_ "outerHTML", class_ "space-y-3", [__|on keydown[(metaKey or ctrlKey) and key is 's'] from window halt the event then call me.requestSubmit()|]] do
              input_ [type_ "hidden", name_ "pageId", value_ form.pageId.toText]
              input_ [type_ "hidden", name_ "version", value_ $ show form.version]
              for_ message $ p_ [id_ "memory-error", role_ "alert", class_ "rounded-lg bg-fillError-weak px-3 py-2 text-sm text-textError"] . toHtml
              term "wiki-editor" [class_ "block space-y-3", term "data-mode" "source"] do
                input_ ([id_ "memory-title", name_ "title", required_ "", maxlength_ "120", value_ form.title, placeholder_ "Untitled", Aria.label_ "Title", class_ "w-full rounded-md bg-transparent text-3xl font-bold tracking-tight text-textStrong placeholder:text-textWeak focus-visible:outline-none"] <> [Aria.describedby_ "memory-error" | isJust message])
                div_ [class_ "flex items-center gap-1.5 text-xs text-textWeak"] do
                  faSprite_ "folder" "regular" "h-3 w-3 shrink-0"
                  input_ ([id_ "memory-path", name_ "path", maxlength_ "512", placeholder_ "folder/page \x2014 blank uses the title", value_ $ fromMaybe "" form.path, Aria.label_ "Page path", class_ "min-w-0 flex-1 rounded bg-transparent px-1 py-0.5 font-mono text-xs text-textWeak placeholder:text-textWeak hover:bg-fillWeak focus-visible:bg-fillWeak focus-visible:outline-2 focus-visible:outline-strokeFocus"] <> [Aria.describedby_ "memory-error" | isJust message])
                  div_ [hidden_ "", term "data-editor-controls" ""]
                    $ select_ [term "data-editor-mode" "", Aria.label_ "Editing mode", class_ "select select-ghost select-xs w-auto text-textWeak"] do
                      option_ [value_ "rich"] "Rich text"
                      option_ [value_ "source"] "Markdown"
                label_ [term "for" "memory-body", id_ "memory-body-label", class_ "sr-only"] "Page content"
                textarea_ ([id_ "memory-body", name_ "body", required_ "", maxlength_ "16000", rows_ "18", class_ "textarea w-full font-mono text-sm leading-relaxed"] <> [Aria.describedby_ "memory-error" | isJust message]) $ toHtml form.body
                div_ [term "data-rich-editor" "", hidden_ "", class_ "-ms-12"] pass
                p_ [term "data-editor-status" "", role_ "status", class_ "text-xs text-textWeak"] "Markdown \x00B7 / for blocks \x00B7 [[path]] or [[path|label]] links a page \x00B7 \x2318S saves"
          unless (null backlinks) $ section_ [Aria.labelledby_ "wiki-backlinks", class_ "mt-12 space-y-2 border-t border-strokeWeak pt-4"] do
            h3_ [id_ "wiki-backlinks", class_ "flex items-center gap-1.5 text-xs font-semibold uppercase tracking-wider text-textWeak"] $ faSprite_ "link" "regular" "h-3 w-3" >> "Linked from"
            ul_ [class_ "flex flex-wrap gap-2"] $ for_ backlinks \page -> li_ $ a_ (class_ "inline-flex items-center gap-1.5 rounded-md bg-fillWeak px-2 py-1 text-sm text-textStrong hover:text-textBrand" : repositoryNavAttrs (pageLink page)) $ faSprite_ "file-lines" "regular" "h-3 w-3" >> title page
          unless (null history) $ details_ [class_ "mt-6 border-t border-strokeWeak pt-4"] do
            summary_ [class_ "flex cursor-pointer items-center gap-1.5 text-xs font-semibold uppercase tracking-wider text-textWeak hover:text-textStrong"] $ faSprite_ "clock-rotate-left" "regular" "h-3 w-3" >> "Revision history"
            when (length history == Memory.pageLimit) $ p_ [class_ "mt-3 text-sm text-textWeak"] $ toHtml @Text $ "Showing the " <> show Memory.pageLimit <> " most recent revisions. Older revisions remain stored."
            div_ [class_ "mt-3 divide-y divide-strokeWeak"] $ for_ (zip history $ map Just (drop 1 history) <> [Nothing]) \(page, previous) -> details_ [class_ "py-3"] do
              summary_ [class_ "cursor-pointer text-sm text-textWeak"] $ edited page
              for_ previous \old -> div_ [class_ "mt-3 space-y-1"] do
                p_ [class_ "text-xs font-medium text-textWeak"] $ toHtml @Text $ "Changes from revision " <> show old.version
                revisionDiff old page
              h3_ [class_ "mt-3 text-sm font-semibold text-textStrong"] $ title page
              div_ [class_ "mt-2"] $ pageBody $ Memory.memoryText page.document.body


  toHtmlRaw = toHtml


repositoryBWConfig :: BWConfig -> Projects.ProjectId -> RepositoryTab -> Maybe Text -> Bool -> Bool -> BWConfig
repositoryBWConfig bw pid tab serviceM canEdit canReadWiki =
  let repositories t = urls.repositoriesGet pid t Nothing
   in bw
        { pageTitle = "Repositories"
        , menuItem = Just "Repositories"
        , pageTitleSuffix = serviceM
        , pageActions =
            a_ [href_ (urls.repositoryConnectGet pid Nothing), class_ "btn btn-sm btn-primary gap-2"] (faSprite_ "plus" "regular" "h-3.5 w-3.5" >> "Add repositories") <$ guard canEdit
        , navTabs =
            Just
              $ navTabStrip_
                NavLinks
                "Repository views"
                ""
                [(toHtml label, url, tab == value) | (value, label, url) <- [(Overview, "Repositories" :: Text, repositories Nothing), (PullRequests, "Pull requests", repositories $ Just PullRequests), (Fixes, "Issue fixes", repositories $ Just Fixes), (Usage, "Usage", repositories $ Just Usage), (Wiki, "Project wiki", urls.projectWikiGet pid [] Nothing Nothing Nothing False)], value /= Wiki || canReadWiki]
        }


repositoryLayout_ :: Maybe Text -> Html () -> Html ()
repositoryLayout_ serviceM content =
  div_ [class_ "h-full w-full overflow-y-auto"] do
    when (isJust serviceM) $ p_ [class_ "px-4 pt-4 text-sm text-textWeak"] "Source mappings that apply to this service, including mappings for all services."
    section_ [id_ "repository-content", class_ "min-w-0 p-4"] content


repositoryNavAttrs :: Text -> [Attribute]
repositoryNavAttrs url =
  [ href_ url
  , hxGet_ url
  , hxTarget_ "#repository-content"
  , hxSelect_ "#repository-content"
  , term "hx-select-oob" "#main-navbar:outerMorph"
  , hxSwap_ "outerMorph"
  , hxPushUrl_ "true"
  , [__|on click set my.preloadState to 'DONE'|]
  ]


repositoriesGetH :: Projects.ProjectId -> Maybe RepositoryTab -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx RepositoriesGet))
repositoriesGetH pid tabM sampleM = case fromMaybe Overview tabM of
  Overview -> repositoriesPageH pid $ OverviewPage sampleM Nothing
  PullRequests -> repositoriesPageH pid ReviewsPage
  Fixes -> repositoriesPageH pid FixesPage
  Usage -> repositoriesPageH pid UsagePage
  Wiki -> fmap (\(PageCtx conf wiki) -> PageCtx conf $ RepositoryWiki wiki) <$> memoryGetH pid Nothing


-- | A repositories tab: a rendered view under the service it is scoped to, or the wiki.
data RepositoriesGet = RepositoryContent (Maybe Text) (Html ()) | RepositoryWiki WikiGet


instance ToHtml RepositoriesGet where
  toHtmlRaw = toHtml
  toHtml = \case
    RepositoryContent serviceM content -> toHtml $ repositoryLayout_ serviceM content
    RepositoryWiki wiki -> toHtml wiki


serviceRepositoriesGetH :: Projects.ProjectId -> Text -> ATAuthCtx (RespHeaders (PageCtx RepositoriesGet))
serviceRepositoriesGetH pid service = repositoriesPageH pid (OverviewPage Nothing $ Just service)


data RepositoryView = OverviewPage (Maybe Text) (Maybe Text) | ReviewsPage | FixesPage | UsagePage


repositoriesPageH :: Projects.ProjectId -> RepositoryView -> ATAuthCtx (RespHeaders (PageCtx RepositoriesGet))
repositoriesPageH pid selection = do
  (session, _, bw) <- mkPageCtx pid
  canEdit <- maybe False (>= ProjectMembers.PEdit) <$> ProjectMembers.getUserPermission pid session.user.id
  let (tab, serviceM) = case selection of
        OverviewPage _ service -> (Overview, service)
        ReviewsPage -> (PullRequests, Nothing)
        FixesPage -> (Fixes, Nothing)
        UsagePage -> (Usage, Nothing)
      canReadWiki = session.persistentSession.id /= demoGuestSessionId
  content <- case selection of
    OverviewPage sampleM _ -> do
      mappings <- CodeContext.getCodeMappings pid
      syncs <- GitSync.getGitSyncs pid
      settings <- ImpactReviews.repositorySettings pid
      credentials <- GitSync.getGitHubCredentials pid
      repositories <- GitSync.getRepositories pid
      conflicts <- map HI.getOneColumn <$> Hasql.interp @[HI.OneColumn GitSync.GitHubSyncId] [HI.sql| SELECT DISTINCT sync_id FROM projects.repository_resources WHERE project_id = #{pid} AND conflicted |]
      let applicable = filter (\m -> maybe True (\service -> isNothing m.service || m.service == Just service) serviceM) mappings
          visible = filter (\repository -> isNothing serviceM || any (mappingInRepository credentials repository) applicable) repositories
      pure do
        let linked = [((c.host, c.apiBase, m.owner, m.repo), m) | m <- applicable, c <- credentials, c.id == m.credentialId]
            repositoryKey repository = (repository.host, repository.apiBase, repository.owner, repository.repo)
            servicesFor repository = sort . ordNub $ mapMaybe (\(key, m) -> guard (key == repositoryKey repository) *> m.service) linked
            repositoryUrl repository = maybe (urls.repositoryGet pid repository.id) (urls.repositorySourceGet pid repository.id Nothing . Just) sampleM
        toHtml
          $ Table.Table
            { Table.config = def{Table.elemID = "repositoriesTable", Table.renderAsTable = True}
            , Table.columns =
                [ Table.col
                    "Repository"
                    ( \repository -> div_ [class_ "min-w-0 space-y-1"] do
                        a_ [href_ $ repositoryUrl repository, class_ "font-medium text-textStrong hover:text-textBrand hover:underline break-all"] $ toHtml (repository.owner <> "/" <> repository.repo)
                        p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ fromMaybe (Git.hostLabel repository.host) repository.apiBase
                    )
                , Table.col "Services" (\repository -> span_ [class_ "text-xs text-textWeak break-words"] $ toHtml $ if any (\(key, m) -> key == repositoryKey repository && isNothing m.service) linked then "All services" else case servicesFor repository of [] -> "No services linked"; services -> T.intercalate ", " services)
                , Table.col
                    "Source context"
                    ( \repository ->
                        if repository.sourceEnabled && any ((== repositoryKey repository) . fst) linked
                          then colorChip_ SuccessBadge "circle-check" "Active"
                          else colorChip_ NeutralBadge "circle-info" $ if repository.sourceEnabled then "Needs setup" else "Off"
                    )
                , Table.col
                    "Production impact reviews"
                    ( \repository -> case find (\s -> repository.host == Git.GitHub && isNothing repository.apiBase && s.owner == T.toLower repository.owner && s.repo == T.toLower repository.repo) settings of
                        Just _ | null (servicesFor repository) -> colorChip_ WarningBadge "circle-info" "Link a service"
                        Just s -> colorChip_ (triggerBadge s.trigger) "circle-check" $ triggerLabel s.trigger
                        Nothing -> colorChip_ NeutralBadge "circle-info" "Not configured"
                    )
                , Table.col
                    "Configuration sync"
                    ( \repository -> case find (\s -> (s.host, s.apiBase, s.owner, s.repo) == repositoryKey repository) syncs of
                        Just s | s.id `elem` conflicts -> colorChip_ WarningBadge "circle-exclamation" "Conflicts need input"
                        Just s | s.exportPaused -> colorChip_ WarningBadge "circle-info" "Export paused"
                        Just s | isJust s.lastExportHash -> colorChip_ WarningBadge "code-branch" "Changes pending"
                        Just s | Just err <- s.lastError -> colorChip_ ErrorBadge "circle-exclamation" "Sync failed" >> p_ [class_ "text-xs text-textError break-words"] (toHtml err.message)
                        Just s | not s.syncEnabled -> colorChip_ NeutralBadge "circle-info" "Paused"
                        Just s | isJust s.announcedRevision -> colorChip_ WarningBadge "circle-info" "Changes pending"
                        Just s | isJust s.lastRevision -> colorChip_ SuccessBadge "circle-check" "Synced"
                        Just _ -> colorChip_ NeutralBadge "circle-info" "Awaiting first sync"
                        Nothing -> colorChip_ NeutralBadge "circle-info" "Not configured"
                    )
                , Table.col "" (const $ span_ [Aria.hidden_ "true", class_ "flex justify-end text-iconNeutral"] $ faSprite_ "chevron-right" "regular" "h-3.5 w-3.5")
                ]
            , Table.rows = V.fromList visible
            , Table.features =
                def
                  { Table.search = Just Table.ClientSide
                  , Table.searchPlaceholder = Just "Search repositories"
                  , Table.rowLink = Just repositoryUrl
                  , Table.zeroState =
                      Just
                        Table.ZeroState
                          { Table.icon = "code-branch"
                          , Table.title = maybe "Connect the repositories behind your services" (const "No repositories linked to this service") serviceM
                          , Table.description = if canEdit then maybe "Link services to their code for source context and production impact reviews. You can also connect repositories that only contain dashboards." (const "Choose a repository, then link this service in Configure source context.") serviceM else "Ask a project editor to link services to their source repositories."
                          , Table.action = Table.ESCustom $ when canEdit $ a_ [href_ (maybe (urls.repositoryConnectGet pid Nothing) (const $ urls.repositoriesGet pid Nothing Nothing) serviceM), class_ "btn btn-sm btn-primary"] $ toHtml @Text $ maybe "Connect repositories" (const "Choose a repository") serviceM
                          }
                  }
            }
    ReviewsPage -> do
      cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
      runs <- ImpactReviews.latestRuns pid
      usage <- ImpactReviews.usageTotals pid
      checks <- ImpactReviews.latestChecks pid
      pure $ div_ [class_ "space-y-4"] do
        p_ [class_ "text-sm text-textWeak max-w-2xl"] "Pull request reviews connect code changes to production telemetry. Configure reviews and evidence sharing in each repository."
        -- Accounting, not review state: kept one click away so the list leads.
        for_ (find (isNothing . (.runId)) usage) \total -> disclosure_ [] "text-xs text-textWeak hover:text-textStrong" "Model and sandbox usage for this project" $ div_ [class_ "mt-2 space-y-1 ps-5 text-xs text-textWeak"] do
          p_ $ toHtml $ "All time: " <> usageText total.usage
          p_ $ toHtml $ resourceText total.resources
          p_ "Model estimates are not customer charges. Compute, storage and video costs are not included. Refresh to see new usage."
        toHtml
          $ Table.Table
            { Table.config = def{Table.elemID = "pullRequestsTable", Table.renderAsTable = True}
            , Table.columns =
                [ Table.col "Repository" (\run -> span_ [class_ "font-medium text-textStrong break-all"] $ toHtml (run.owner <> "/" <> run.repo))
                , Table.col
                    "Pull request"
                    ( \run -> a_ [href_ $ "https://github.com/" <> run.owner <> "/" <> run.repo <> "/pull/" <> show run.number, target_ "_blank", rel_ "noopener noreferrer", class_ "inline-flex items-center gap-2 text-textBrand hover:underline whitespace-nowrap tabular-nums", Aria.label_ $ "Open pull request #" <> show run.number <> " in " <> run.owner <> "/" <> run.repo] do
                        toHtml @Text $ "#" <> show run.number
                        faSprite_ "arrow-up-right-from-square" "regular" "h-3 w-3"
                    )
                , Table.col
                    "Status"
                    ( \run -> case run.state of
                        ImpactReviews.Superseded -> span_ [class_ "inline-flex items-center gap-1.5 text-xs text-textWeak"] $ faSprite_ "clock-rotate-left" "regular" "h-3 w-3" >> "Superseded"
                        ImpactReviews.Completed -> colorChip_ SuccessBadge "circle-check" "Completed"
                        ImpactReviews.Incomplete -> colorChip_ WarningBadge "circle-exclamation" "Incomplete"
                        ImpactReviews.Queued -> colorChip_ NeutralBadge "clock" "Queued"
                        ImpactReviews.Reviewing -> colorChip_ BrandBadge "arrows-rotate" "Reviewing"
                    )
                , Table.col
                    "Result"
                    ( \run -> case AE.fromJSON <$> run.result of
                        Just (AE.Success (result :: ImpactReview.ReviewResult)) ->
                          let (toneM, icon, label) = case result.verdict of
                                ImpactReview.WorthChecking -> (Just WarningBadge, "triangle-exclamation", countNoun (length result.findings) "finding" <> " to check")
                                ImpactReview.CoverageUnknown -> (Just NeutralBadge, "circle-question", "Coverage unknown")
                                ImpactReview.NoFinding -> (Nothing, "circle-check", "No finding")
                              plain = span_ [class_ "inline-flex items-center gap-1.5 text-xs text-textWeak"] $ faSprite_ icon "regular" "h-3 w-3" >> toHtml label
                           in -- Only a current review's open question earns a chip; a superseded one's verdict is history.
                              maybe plain (\tone -> colorChip_ tone icon label) (toneM <* guard (run.state /= ImpactReviews.Superseded))
                        _ -> span_ [class_ "text-xs text-textWeak"] "\x2014"
                    )
                , Table.col "Revision" (\run -> code_ [class_ "text-xs text-textWeak", title_ run.revision] $ toHtml $ T.take 8 run.revision)
                , Table.col "Reviewed" (\run -> span_ [class_ "whitespace-nowrap text-xs text-textWeak tabular-nums"] $ localTimeFmt_ "MMM d, HH:mm" run.createdAt)
                , Table.col "" (\run -> dialogTrigger_ ("review-" <> run.id.toText) [class_ "btn btn-sm btn-ghost btn-square -me-2", Aria.label_ $ "View review for " <> run.owner <> "/" <> run.repo <> " #" <> show run.number] $ faSprite_ "chevron-right" "regular" "h-3.5 w-3.5")
                ]
            , Table.rows = V.fromList runs
            , Table.features =
                def
                  { Table.search = Just Table.ClientSide
                  , Table.searchPlaceholder = Just "Search pull requests"
                  , Table.rowAttrs = Just \run -> [class_ "cursor-pointer", data_ "review" ("review-" <> run.id.toText), [__|on click if not event.target.closest('a, button') call document.getElementById(@data-review).showModal() end|]]
                  , Table.zeroState =
                      Just
                        Table.ZeroState
                          { Table.icon = "code-branch"
                          , Table.title = "No pull requests reviewed yet"
                          , Table.description = "Link a GitHub repository and its services to review pull requests with production telemetry."
                          , Table.action = Table.ESCustom $ a_ [href_ $ urls.repositoriesGet pid Nothing Nothing, class_ "btn btn-sm btn-primary"] "Configure repositories"
                          }
                  }
            }
        forM_ runs \run -> nativeDialog_ ("review-" <> run.id.toText) (run.owner <> "/" <> run.repo <> " #" <> show run.number) "w-full max-w-4xl space-y-5 p-6" do
          div_ [class_ "flex flex-wrap items-center gap-3 text-xs text-textWeak -mt-3"] do
            toHtml @Text $ "Review " <> show run.state
            code_ [title_ run.revision] $ toHtml $ T.take 8 run.revision
          for_ (find ((== Just run.id) . (.runId)) usage) \total -> do
            p_ [class_ "mt-2 text-sm text-textWeak"] $ toHtml $ usageText total.usage
            p_ [class_ "text-sm text-textWeak"] $ toHtml $ resourceText total.resources
          let executions = [receipt | execution <- checks, execution.runId == run.id, let Aeson receipt = execution.receipt]
          unless (null executions) $ div_ [class_ "mt-3 space-y-2 text-sm"] do
            p_ [class_ "font-medium text-textStrong"] "Local regression checks"
            p_ [class_ "text-xs text-textWeak"] "Recorded execution results do not establish that a check covers the reported bug. Compute cost is unpriced."
            for_ (NE.groupAllWith (.checkId) executions) \comparison@(representative :| _) -> div_ [class_ "space-y-1"] do
              p_ [class_ "text-xs text-textWeak"] $ toHtml @Text $ "Check " <> T.take 8 representative.checkId.toText
              ul_ [class_ "space-y-1 text-textWeak"] $ for_ comparison \receipt -> do
                let status = case receipt.status of
                      Agent.Passed -> "passed"
                      Agent.Failed -> "assertion failed"
                      Agent.Blocked -> "setup blocked"
                      Agent.Invalidated -> "source changed"
                      Agent.TimedOut -> "timed out"
                      Agent.OutputLimit -> "output limit reached"
                      Agent.Cancelled -> "cancelled"
                      Agent.CleanupFailed -> "cleanup failed"
                li_ [class_ "flex flex-wrap items-baseline gap-2"] do
                  toHtml @Text $ (case receipt.side of Agent.Base -> "Base"; Agent.Head -> "Head") <> ": " <> status
                  code_ [class_ "text-xs"] $ toHtml $ T.take 8 receipt.revision
                  span_ [class_ "tabular-nums"] $ toHtml @Text $ show receipt.durationMs <> " ms · " <> show receipt.cpus <> " vCPUs · " <> show receipt.memoryMiB <> " MiB"
          whenJust run.error $ p_ [class_ "text-sm text-textError break-words"] . toHtml
          case run.result of
            Nothing -> p_ [class_ "text-sm text-textWeak"] $ toHtml @Text case run.state of
              ImpactReviews.Queued -> "This review is queued."
              ImpactReviews.Reviewing -> "This review is in progress."
              ImpactReviews.Superseded -> "A newer revision replaced this review."
              ImpactReviews.Completed -> "No review result is available."
              ImpactReviews.Incomplete -> "No review result is available."
            Just value -> case AE.fromJSON value of
              AE.Error _ -> p_ [class_ "text-sm text-textWeak"] "The saved review could not be displayed. Rerun the review."
              AE.Success result -> do
                div_ [class_ "prose prose-sm max-w-none break-words [&_pre]:overflow-x-auto"] $ renderMarkdown (ImpactReview.renderReview cfg.hostUrl run ImpactReview.MonoscopeView result)
                for_ (nonEmpty =<< result.knowledge) \notes -> details_ [class_ "mt-3 text-sm text-textWeak"] do
                  summary_ [class_ "cursor-pointer font-medium"] "Service knowledge from this review"
                  p_ [class_ "mt-2 text-xs"] "Agent conclusions from the recorded evidence window. Recheck against current code and telemetry."
                  ul_ [class_ "mt-2 space-y-2"] $ for_ notes \note -> li_ do
                    span_ [class_ "font-medium"] $ toHtml note.service
                    toHtml @Text $ ": " <> note.summary
          div_ [class_ "flex flex-wrap justify-end gap-2 border-t border-strokeWeak pt-4"] do
            form_ [method_ "dialog"] $ button_ [class_ "btn btn-sm btn-ghost", autofocus_] "Close"
            when (canEdit && run.revision == run.latestRevision && run.state /= ImpactReviews.Reviewing) $ button_ [type_ "button", class_ "btn btn-sm btn-primary", hxPost_ (urls.impactReviewRetry pid run.id), hxTarget_ "#repository-content", hxSelect_ "#repository-content", hxSwap_ "outerMorph", [__|on click call (closest <dialog/>).close()|]] "Rerun review"
    FixesPage -> do
      runs <- if session.persistentSession.id == demoGuestSessionId then pure [] else IssueFixes.latestMediaForUser pid session.user.id
      pure $ section_ [class_ "space-y-4"] do
        h2_ [class_ "text-base font-semibold text-textStrong"] "Issue fixes"
        p_ [class_ "text-sm text-textWeak"] "Completed fixes and their verification evidence. Available to project administrators."
        when (null runs) $ emptyState_ def{icon = Just "code", action = ESLink ((urls.issues pid).listGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []) "Open issues"} "No verified fixes yet" "A fix lands here once Monoscope has checked it against production. Start one with Plan a fix on an issue."
        ul_ [class_ "divide-y divide-strokeWeak"] $ for_ runs \run -> li_ [class_ "py-3"] $ a_ [href_ (urls.issueFixGet pid run.runId), class_ "text-sm text-textBrand hover:underline"] $ toHtml run.summary
    UsagePage -> usage_ <$> ImpactReviews.customerUsage session.user.id
  addRespHeaders $ PageCtx (repositoryBWConfig bw pid tab serviceM canEdit canReadWiki) $ RepositoryContent serviceM content
  where
    usage_ :: [ImpactReviews.ProjectUsage] -> Html ()
    usage_ rows = div_ [class_ "space-y-4"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Agent usage by customer and project"
      p_ [class_ "text-sm text-textWeak"] "All-time review and issue-fix subtotals include only projects where you are an active administrator. Customer references use recorded billing snapshots. Older model receipts are marked when attributed on receipt."
      p_ [class_ "text-xs text-textWeak"] "Model estimates are not customer charges. Resource measurements have no prices. Only regression executions and retained explanations are measured; other resource usage is not yet metered."
      when (null rows) $ emptyState_ def{icon = Just "gauge"} "No agent usage yet" "Pull request reviews and issue fixes in projects you administer are metered here as they run."
      for_ (NE.groupBy ((==) `on` (\row -> maybeToRight row.projectId ((row.billingProvider,) <$> row.customerId))) rows) \projects@(row :| _) ->
        section_ [class_ "rounded-lg border border-strokeWeak p-4 space-y-3"] do
          h3_ [class_ "text-sm font-medium text-textStrong break-all"] $ case row.customerId of
            Nothing -> "No billing customer linked"
            Just cid -> do
              toHtml @Text $ case row.billingProvider of Projects.StripeProvider -> "Stripe customer "; Projects.LemonSqueezyProvider -> "Lemon Squeezy customer "; Projects.NoBillingProvider -> "Billing customer "
              code_ $ toHtml cid
          p_ [class_ "text-sm text-textWeak"] $ toHtml $ usageText $ F1.foldMap1 (.usage) projects
          p_ [class_ "text-sm text-textWeak"] $ toHtml $ resourceText $ F1.foldMap1 (.resources) projects
          ul_ [class_ "space-y-2"] $ for_ (NE.groupBy ((==) `on` (.projectId)) projects) \projectRows@(project :| _) -> li_ [class_ "flex flex-wrap items-baseline justify-between gap-2 text-sm"] do
            a_ [href_ (urls.repositoriesGet project.projectId (Just PullRequests) Nothing), class_ "text-textBrand hover:underline break-all"] $ toHtml project.title
            div_ [class_ "space-y-1 text-textWeak tabular-nums"] do
              p_ $ toHtml $ usageText $ F1.foldMap1 (.usage) projectRows
              p_ $ toHtml $ resourceText $ F1.foldMap1 (.resources) projectRows


resourceText :: Usage.ResourceUsage -> Text
resourceText total =
  "Sandbox allocation: "
    <> show total.checks
    <> " recorded regression executions · "
    <> show total.allocations
    <> " agent VM allocations · "
    <> seconds total.vcpuMs
    <> " vCPU-s · "
    <> seconds total.memoryMiBMs
    <> " MiB-s · "
    <> whole total.logBytes
    <> " log bytes"
    <> (if total.explanations == 0 then "" else " · retained explanations: " <> show total.explanations <> " · rendering: " <> seconds total.renderMs <> " s · media: " <> whole total.mediaBytes <> " bytes")
    <> (if total.cleanupFailures == 0 then "" else " · agent VM cleanup unconfirmed: " <> show total.cleanupFailures)
    <> (if total.unreportedAllocations == 0 then "" else " · attempts without an agent allocation receipt: " <> show total.unreportedAllocations)
    <> " · resource cost unavailable"
  where
    seconds value = toText $ showFFloat (Just 3) (realToFrac value / 1000 :: Double) ""
    whole = toText . formatScientific Fixed (Just 0)


usageText :: ImpactReviews.UsageCost -> Text
usageText total =
  ( if total.calls == 0
      then "No model usage reported"
      else
        (if total.unpricedCalls == total.calls then "Cost unavailable" else if total.costNanousd > 0 && total.costNanousd < 1000 then "<$0.000001 estimated" else "$" <> toText (showFFloat (Just 6) (realToFrac total.costNanousd / 1e9 :: Double) "") <> " estimated")
          <> " · calls: "
          <> show total.calls
          <> (if total.failedCalls == 0 then "" else " · " <> show total.failedCalls <> " failed")
          <> (if total.unpricedCalls == 0 then "" else " · " <> show total.unpricedCalls <> " unpriced (excluded from estimate)")
          <> (if total.lateAttributedCalls == 0 then "" else " · " <> show total.lateAttributedCalls <> " attributed on receipt (no attempt snapshot)")
  )
    <> (if total.unreportedCalls == 0 then "" else " · calls without usage: " <> show total.unreportedCalls <> " (cost unknown)")
    <> (if total.unreportedAttempts == 0 then "" else " · attempts without model receipts: " <> show total.unreportedAttempts <> " (cost unknown)")


triggerLabel :: ImpactReviews.ReviewTrigger -> Text
triggerLabel = \case
  ImpactReviews.ReadyPullRequests -> "Ready pull requests"
  ImpactReviews.AllPullRequests -> "Drafts and ready pull requests"
  ImpactReviews.WhenAsked -> "Only when asked"


-- | One repository's review settings as form fields; a repository without saved settings shows the defaults.
reviewSettingsFields_ :: Monad m => Text -> Text -> Maybe ImpactReviews.ReviewSettings -> HtmlT m ()
reviewSettingsFields_ owner repo settings = do
  input_ [type_ "hidden", name_ "owner", value_ owner]
  input_ [type_ "hidden", name_ "repo", value_ repo]
  let trigger = maybe ImpactReviews.ReadyPullRequests (.trigger) settings
      evidence = bool "false" "true" $ any (.includeEvidence) settings
  formSelectField_ FieldSm "Review pull requests" "trigger" False $ options_ (Just $ toUrlPiece trigger) [(toUrlPiece t, triggerLabel t) | t <- universe]
  formSelectField_ FieldSm "Production evidence in GitHub" "includeEvidence" False $ options_ (Just evidence) [("false", "Links only"), ("true", "Aggregates and links")]


-- | Asks-only reviews are quiet until someone comments, so they read as neutral, not active.
triggerBadge :: ImpactReviews.ReviewTrigger -> BadgeColor
triggerBadge = \case ImpactReviews.WhenAsked -> NeutralBadge; (ImpactReviews.ReadyPullRequests; ImpactReviews.AllPullRequests) -> SuccessBadge


askHint :: Text
askHint = "Comment @monoscope review on any pull request to ask for a review, or @monoscope prove it to also record proof."


data ReviewReadiness
  = ReviewReady ImpactReviews.ReviewSettings
  | ReviewNeedsRoutine
  | ReviewNeedsService
  | ReviewNeedsApp
  | ReviewNeedsServer
  | ReviewNeedsAgent
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
  routine <- Issues.getEventRoutine pid Issues.EventPullRequest
  cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
  permission <- ProjectMembers.getUserPermission pid session.user.id
  let mappings = filter (mappingInRepository credentials repository) allMappings
      dashboardSync = find (\s -> (s.host, s.apiBase, s.owner, s.repo) == (repository.host, repository.apiBase, repository.owner, repository.repo)) syncs
      reviewSettings = find (\s -> s.owner == T.toLower repository.owner && s.repo == T.toLower repository.repo) settings
      reviewReadiness
        | repository.host /= Git.GitHub || isJust repository.apiBase = ReviewUnsupportedHost
        | any T.null ([cfg.githubAppWebhookSecret, cfg.githubAppId, cfg.githubAppPrivateKey] :: [Text]) = ReviewNeedsServer
        | not (any (isJust . (.service)) mappings) = ReviewNeedsService
        | Just s <- reviewSettings = if isNothing cfg.impactReviewAgent then ReviewNeedsAgent else maybe ReviewNeedsRoutine (const $ ReviewReady s) routine
        | otherwise = ReviewNeedsApp
  resources <- maybe (pure []) (RepositorySync.resources pid . (.id)) dashboardSync
  conflictVersions <- forM (filter (.conflicted) resources) $ \resource -> (resource.id,) <$> RepositorySync.localDefinition resource
  folderCandidates <- RepositorySync.folderCandidates pid
  analysis <- GitSync.getRepositoryAnalysis pid rid
  addRespHeaders $ PageCtx bw{prePageTitle = Just "Repositories", pageTitle = repository.owner <> "/" <> repository.repo, menuItem = Just "Repositories"} RepositoryGet{repository, mappings, dashboardSync, reviewReadiness, reviewSettings, resources, conflictVersions, folderCandidates, analysis, permission}


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
  assignment <- if candidate.kind == RepositorySync.Dashboard then void <$> GitSync.assignDashboardRepository pid (UUIDId candidate.localId) sync.id else pure $ Right ()
  case assignment of
    Left GitSync.BuiltInReadOnly -> addErrorToast "Could not move dashboard" (Just readOnlyMessage)
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
      conn <- hoistEither $ first Git.gitConnErrorMessage $ GitSync.syncConn sync token
      ExceptT $ action conn (GitSync.syncRepoRef sync) (fromIntegral number)


repositoryDeleteH :: Projects.ProjectId -> GitSync.RepositoryId -> ATAuthCtx (RespHeaders (Done RepositoryRemoval))
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
  done =<< case closed of
    Left err -> pure $ PullRequestNotClosed err
    Right () ->
      GitSync.removeRepository pid rid >>= \case
        Left GitSync.ConnectionMissing -> throwError err404
        Left GitSync.SyncRunning -> pure RemovalBlockedBySync
        Right () -> pure $ RepositoryRemoved pid


-- | A blocked removal answers 200 with its reason, rendered inside the still-open confirmation dialog.
data RepositoryRemoval = RepositoryRemoved Projects.ProjectId | RemovalBlockedBySync | PullRequestNotClosed Text
  deriving stock (Eq, Show)


instance Outcome RepositoryRemoval where
  outcomeBody = \case
    RepositoryRemoved _ -> mempty
    RemovalBlockedBySync -> p_ [role_ "alert", class_ "text-sm text-textWarning"] "A sync is running. Try removing the repository again when it finishes."
    PullRequestNotClosed err -> p_ [role_ "alert", class_ "text-sm text-textError"] $ toHtml $ "Could not close the configuration PR. The repository is still connected. " <> err
  outcomeRedirect = \case
    RepositoryRemoved pid -> Just $ urls.repositoriesGet pid Nothing Nothing
    (RemovalBlockedBySync; PullRequestNotClosed _) -> Nothing
  outcomeToast = \case
    RepositoryRemoved _ -> Just $ Toast SuccessToast "Repository removed" (Just "Dashboards, monitors, and review history are retained.") Nothing
    (RemovalBlockedBySync; PullRequestNotClosed _) -> Nothing


instance ToHtml RepositoryGet where
  toHtml page = section_ [id_ "repository-detail", class_ "@container/repository w-full min-w-0 space-y-5 p-4 sm:p-6"] do
    let repository = page.repository
        pid = repository.projectId
        rid = repository.id
        canEdit = maybe False (>= ProjectMembers.PEdit) page.permission
        post url attrs = form_ ([method_ "post", action_ url, hxPost_ url, hxTarget_ "#repository-detail", hxSelect_ "#repository-detail", hxSwap_ "outerMorph"] <> attrs)
        syncLink = a_ [href_ (urls.repositoryDashboardGet pid rid), class_ "btn btn-sm btn-outline"] $ if canEdit then "Configure sync" else "View sync"
    header_ [class_ "flex flex-wrap items-center gap-x-6 gap-y-3"] do
      div_ [class_ "min-w-0 space-y-2 break-words"] do
        settingsH2_ $ repository.owner <> "/" <> repository.repo
        p_ [class_ "text-sm text-textWeak break-all"] $ toHtml $ fromMaybe (Git.hostLabel repository.host) repository.apiBase
      when canEdit $ post (urls.repositoryAnalyzePost pid rid) [] $ button_ [type_ "submit", class_ "btn btn-sm btn-outline gap-2"] do
        faSprite_ "arrows-rotate" "regular" "h-3.5 w-3.5"
        "Analyze repository"
      when canEdit $ p_ [class_ "w-full max-w-[65ch] text-xs text-textWeak"] "Analysis matches this repository to services in your telemetry. The first run can link those services and turn on dashboard sync; existing settings are kept."
    whenJust (page.analysis >>= (.error)) $ \err -> p_ [role_ "status", class_ "text-sm text-textError"] $ toHtml $ "Setup needs attention: " <> err
    whenJust (page.analysis >>= (.suggestions)) $ \suggestions -> when (null page.mappings) $ div_ [class_ "rounded-xl border border-strokeWeak p-4 space-y-2"] do
      p_ [class_ "text-sm font-medium text-textStrong"] "Service mapping needs input"
      p_ [class_ "text-sm text-textWeak break-words"] $ toHtml $ if null suggestions.services then "No service names have been observed yet. Link a service when its telemetry arrives." else "Observed services: " <> T.intercalate ", " (take 12 suggestions.services)
    div_ [class_ "grid min-w-0 items-start gap-5 @min-[64rem]/repository:grid-cols-2"] do
      section_ [class_ "min-w-0 @min-[64rem]/repository:col-start-1 @min-[64rem]/repository:row-start-1", Aria.label_ "Services and source context"]
        $ panel_ Raised def{icon = Just "code-branch"} "Services and source context"
        $ div_ [class_ "space-y-4"] do
          bool (colorChip_ NeutralBadge "circle-info" "Disabled") (colorChip_ SuccessBadge "circle-check" "Enabled") repository.sourceEnabled
          if null page.mappings
            then p_ [class_ "text-sm text-textWeak"] "No source paths are linked. Link a service to read the failing code directly from its stack trace."
            else div_ [class_ "divide-y divide-strokeWeak rounded-lg bg-fillWeak"] $ forM_ page.mappings \mapping -> div_ [class_ "flex flex-wrap items-start justify-between gap-3 p-3"] do
              div_ [class_ "min-w-0 space-y-1"] do
                p_ [class_ "text-sm font-medium text-textStrong break-words"] $ toHtml $ fromMaybe "All services" mapping.service
                p_ [class_ "text-xs text-textWeak break-all"] $ toHtml $ scopeLabel mapping
              code_ [class_ "rounded bg-bgBase px-2 py-1 text-xs text-textWeak break-all"] $ toHtml mapping.ref
          div_ [class_ "flex flex-wrap items-center gap-2"] do
            a_ [href_ (urls.repositorySourceGet pid rid Nothing Nothing), class_ "btn btn-sm btn-outline"] $ if canEdit then "Configure source context" else "View source context"
            when canEdit $ post (urls.repositorySourceEnabledPost pid rid) [] do
              input_ [type_ "hidden", name_ "enabled", value_ (if repository.sourceEnabled then "false" else "true")]
              button_ [type_ "submit", class_ "btn btn-sm btn-ghost"] $ if repository.sourceEnabled then "Disable source context" else "Enable source context"
      section_ [class_ "min-w-0 @min-[64rem]/repository:col-start-2 @min-[64rem]/repository:row-start-1 @min-[64rem]/repository:row-span-2", Aria.label_ "Production impact reviews"]
        $ panel_ Raised def{icon = Just "code-pull-request"} "Production impact reviews"
        $ div_ [class_ "space-y-4"] do
          let setupRequired = (,True) . callout_ WarningBadge (Just "Setup required")
              (readiness, blocked) = case page.reviewReadiness of
                ReviewReady settings -> (,False) do
                  colorChip_ (triggerBadge settings.trigger) "circle-check" $ triggerLabel settings.trigger
                  p_ [class_ "text-sm text-textWeak"] $ toHtml askHint
                ReviewNeedsAgent -> setupRequired "Automatic reviews are unavailable. Ask your Monoscope administrator to finish review setup."
                ReviewNeedsRoutine -> setupRequired "Activate or resume the Production impact review routine. Repository settings choose which pull requests it reviews."
                ReviewNeedsService -> setupRequired "Link a named service so reviews can find the production telemetry for this repository."
                ReviewNeedsApp -> setupRequired "Link source context through the GitHub App to receive automatic pull request reviews. A token connection can still provide source context and configuration sync."
                ReviewNeedsServer -> setupRequired "An administrator must configure the GitHub App and webhook before automatic reviews can run."
                ReviewUnsupportedHost -> (callout_ NeutralBadge (Just "Unavailable") "Automatic pull request reviews currently support GitHub.com. Source context and configuration sync remain available for this host.", True)
          readiness
          when (canEdit && repository.host == Git.GitHub && isNothing repository.apiBase) $ post (urls.repositoryReviewsPost pid rid) [class_ "grid items-end gap-3 sm:grid-cols-2"] do
            reviewSettingsFields_ repository.owner repository.repo page.reviewSettings
            button_ [type_ "submit", class_ $ "btn btn-sm mb-1 justify-self-start sm:col-span-2 " <> bool "btn-primary" "btn-outline" blocked] "Save review settings"
            when blocked $ p_ [class_ "text-xs text-textWeak sm:col-span-2"] "Saved settings take effect once setup is complete."
          div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-2"] do
            a_ [href_ (urls.repositoriesGet pid (Just PullRequests) Nothing), class_ "btn btn-sm btn-outline"] "Review history"
            a_ [href_ (urls.aiRoutinesGet pid <> "#routine-production-impact-review"), class_ "text-sm text-textBrand hover:underline"] "Manage review routine and runs"
      section_ [class_ "min-w-0 @min-[64rem]/repository:col-start-1 @min-[64rem]/repository:row-start-2", Aria.label_ "Dashboards and monitors"]
        $ panel_ Raised def{icon = Just "dashboard"} "Dashboards and monitors"
        $ div_ [class_ "space-y-4"] do
          case page.dashboardSync of
            Nothing -> div_ [class_ "space-y-3"] do
              p_ [class_ "text-sm text-textWeak"] "Keep dashboards and monitors as files in this repository. Monoscope proposes changes as pull requests, and merged changes apply automatically."
              syncLink
            Just sync -> do
              whenJust sync.lastError \err -> div_ [role_ "status"] $ callout_ ErrorBadge (Just $ case err.operation of GitSync.ImportDashboards -> "Import from the repository failed"; GitSync.ExportDashboard _ -> "Export to the repository failed"; GitSync.ExportDashboards -> "Export to the repository failed") $ span_ [class_ "break-words"] $ toHtml err.message
              p_ [class_ "text-sm text-textWeak break-all"] $ toHtml $ sync.branch <> " · " <> GitSync.getDashboardsPath sync
              colorChip_ NeutralBadge "code-branch" $ if not sync.syncEnabled then "Disabled" else if sync.exportPaused then "Export paused" else if any (.conflicted) page.resources then "Conflicts need input" else if isJust sync.lastExportHash then "Changes proposed" else if isJust sync.lastRevision then "Synced" else "Preparing folder"
              whenJust sync.pullRequestNumber $ \number -> a_ [href_ ("https://github.com/" <> sync.owner <> "/" <> sync.repo <> "/pull/" <> show number), target_ "_blank", rel_ "noopener noreferrer", class_ "text-sm text-textBrand hover:underline"] $ toHtml $ "Configuration PR #" <> show @Text number
              p_ [class_ "text-sm text-textWeak"] "Monoscope edits are active immediately. Git changes apply after merge into the target branch. Dashboards and monitor definitions share this folder."
              syncLink
              when (canEdit && sync.exportPaused) $ post (urls.repositoryResumePost pid rid) [] $ button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Resume export"
              when canEdit $ details_ [class_ "space-y-3"] do
                summary_ [class_ "cursor-pointer text-sm text-textBrand"] "Move existing resources into this folder"
                for_ ([RepositorySync.Dashboard, RepositorySync.Query, RepositorySync.Uptime, RepositorySync.Cron] :: [RepositorySync.ResourceKind]) \kind -> do
                  let candidates = filter ((== kind) . (.kind)) page.folderCandidates
                  unless (null candidates) $ post (urls.repositoryFolderPost pid rid) [class_ "flex flex-wrap items-end gap-2"] do
                    input_ [type_ "hidden", name_ "kind", value_ $ T.toLower $ show kind]
                    formSelectField_ FieldSm (show kind) "localId" True $ for_ candidates \candidate -> option_ [value_ $ show candidate.localId] $ toHtml candidate.title
                    button_ [type_ "submit", class_ "btn btn-sm btn-ghost"] "Move into folder"
    unless (null page.resources)
      $ panel_ Raised def{icon = Just "dashboard"} "Synced resources"
      $ ul_ [class_ "divide-y divide-strokeWeak"]
      $ forM_ page.resources \resource -> li_ [class_ "flex flex-wrap items-center justify-between gap-3 p-3"] do
        code_ [class_ "text-xs text-textStrong break-all"] $ toHtml resource.filePath
        when resource.conflicted do
          span_ [class_ "text-sm text-textWarning"] "Both versions changed"
          details_ [class_ "w-full min-w-0 space-y-3"] do
            summary_ [class_ "cursor-pointer text-sm text-textBrand"] "Compare versions"
            div_ [class_ "grid min-w-0 gap-4 md:grid-cols-2"] $ for_ ([("Active in Monoscope", Yaml.encode . AE.toJSON <$> join (lookup resource.id page.conflictVersions)), ("Merged Git version", Yaml.encode <$> resource.remoteDefinition)] :: [(Text, Maybe ByteString)]) \(label, version) -> div_ [class_ "min-w-0 space-y-2"] do
              h3_ [class_ "text-sm font-medium text-textStrong"] $ toHtml label
              pre_ [class_ "max-h-80 overflow-auto rounded-lg bg-fillWeak p-3 text-xs text-textWeak"] $ toHtml @Text $ maybe "Deleted" decodeUtf8 version
          when canEdit $ post (urls.repositoryConflictPost pid rid resource.id) [class_ "flex gap-2"] do
            button_ [type_ "submit", name_ "useGit", value_ "false", class_ "btn btn-sm btn-ghost"] "Keep Monoscope"
            button_ [type_ "submit", name_ "useGit", value_ "true", class_ "btn btn-sm btn-ghost"] "Use Git"
    when canEdit $ panel_ Raised def{icon = Just "link-slash"} "Remove connection" $ do
      div_ [class_ "space-y-3"] do
        p_ [class_ "max-w-[65ch] text-sm leading-relaxed text-textWeak"] "Stop using this repository for source context, pull request reviews, and configuration sync. Dashboards and monitors stay local and review history is retained. Shared repository accounts remain connected."
        dialogTrigger_ "remove-repository-dialog" [class_ "btn btn-sm btn-outline text-textError hover:bg-fillError-weak"] "Remove repository"
      nativeDialog_ "remove-repository-dialog" ("Remove " <> repository.owner <> "/" <> repository.repo <> "?") "space-y-4 p-6" do
        p_ [class_ "text-sm text-textWeak"] "Source context, automatic reviews, and configuration sync will stop for this repository. Dashboards, monitors, and review history are retained. You can connect the repository again later."
        div_ [id_ "remove-repository-feedback"] ""
        div_ [class_ "flex flex-wrap justify-end gap-2"] do
          form_ [method_ "dialog"] $ button_ [class_ "btn btn-sm btn-ghost", autofocus_] "Cancel"
          button_ [type_ "button", hxDelete_ (urls.repositoryDelete pid rid), hxTarget_ "#remove-repository-feedback", hxSwap_ "innerHTML", term "hx-disable" "this", hxIndicator_ "#remove-repository-indicator", class_ "btn btn-sm bg-fillError-strong text-textInverse-strong hover:opacity-90 gap-2"] do
            "Remove repository"
            htmxIndicator_ "remove-repository-indicator" LdXS
  toHtmlRaw = toHtml


mappingInRepository :: [GitSync.GitHubCredential] -> GitSync.Repository -> CodeContext.CodeMapping -> Bool
mappingInRepository credentials repository mapping =
  (mapping.owner, mapping.repo)
    == (repository.owner, repository.repo)
    && any (\c -> c.id == mapping.credentialId && (c.host, c.apiBase) == (repository.host, repository.apiBase)) credentials


repositorySourceGetH :: Projects.ProjectId -> GitSync.RepositoryId -> Maybe GitSync.GitHubCredentialId -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx RepositorySourceGet))
repositorySourceGetH pid rid cid sample = do
  (_, _, bw) <- mkPageCtx pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  content <- codeMappingsContent pid sample cid (Just repository)
  addRespHeaders $ PageCtx bw{prePageTitle = Just "Repositories", pageTitle = repository.owner <> "/" <> repository.repo, pageTitleUrl = Just $ urls.repositoryGet pid rid, pageTitleSuffix = Just "Source context", menuItem = Just "Repositories"} $ RepositorySourceGet $ MappingsPanel content


newtype RepositorySourceGet = RepositorySourceGet MappingsPanel


instance ToHtml RepositorySourceGet where
  toHtmlRaw = toHtml
  toHtml (RepositorySourceGet panel) = section_ [class_ "w-full min-w-0 space-y-6 p-4 sm:p-6"] do
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Source context"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Link this repository to the services it builds. Stack traces and pull request reviews use these mappings to find the right code and production telemetry."
    div_ [id_ "code-mappings-content"] $ toHtml panel


repositorySourcePostH :: Projects.ProjectId -> GitSync.RepositoryId -> CodeMappingForm -> ATAuthCtx (ActionResult MappingRejection MappingsPanel)
repositorySourcePostH pid rid form = act do
  requireReviewWrite pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  cid <- maybe (throwError err403) pure (form.credentialId <|> repository.credentialId)
  account <- codeContextCredential pid (Just cid) >>= maybe (throwError err403) pure
  unless ((account.host, account.apiBase) == (repository.host, repository.apiBase)) $ throwError err403
  saved <- saveCodeMapping pid form{repo = Just (repository.owner <> "/" <> repository.repo), credentialId = Just account.id}
  forM saved \() -> MappingsPanel . toHtml . getResponse <$> repositorySourceGetH pid rid (Just account.id) (nonEmptyT form.samplePath)


newtype SourceContextForm = SourceContextForm {enabled :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


repositorySourceEnabledPostH :: Projects.ProjectId -> GitSync.RepositoryId -> SourceContextForm -> ATAuthCtx (RespHeaders (PageCtx RepositoryGet))
repositorySourceEnabledPostH pid rid form = do
  requireReviewWrite pid
  _ <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  Hasql.interpExecute_ [HI.sql| UPDATE projects.repositories SET source_enabled = #{form.enabled} WHERE project_id = #{pid} AND id = #{rid} |]
  repositoryGetH pid rid


repositoryReviewsPostH :: Projects.ProjectId -> GitSync.RepositoryId -> ImpactReviews.ReviewSettings -> ATAuthCtx (RespHeaders (Done (ReviewChange (PageCtx RepositoryGet))))
repositoryReviewsPostH pid rid settings = do
  requireReviewWrite pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  unless (repository.host == Git.GitHub && isNothing repository.apiBase) $ throwError err403
  ImpactReviews.updateSettings pid settings{ImpactReviews.owner = T.toLower repository.owner, ImpactReviews.repo = T.toLower repository.repo}
  done . SettingsSaved . getResponse =<< repositoryGetH pid rid


-- | The view re-rendered after a review change.
data ReviewChange a = SettingsSaved a | RerunQueued a
  deriving stock (Foldable, Show)


instance ToHtml a => Outcome (ReviewChange a) where
  outcomeBody = foldMap toHtml
  outcomeToast = \case
    SettingsSaved _ -> Just "Review settings saved"
    RerunQueued _ -> Nothing


repositorySourceDeleteH :: Projects.ProjectId -> GitSync.RepositoryId -> CodeContext.CodeMappingId -> ATAuthCtx (RespHeaders (Done MappingsPanel))
repositorySourceDeleteH pid rid mid = do
  requireReviewWrite pid
  repository <- GitSync.getRepository pid rid >>= maybe (throwError err404) pure
  credentials <- GitSync.getGitHubCredentials pid
  mappings <- CodeContext.getCodeMappings pid
  unless (any (\m -> m.id == mid && mappingInRepository credentials repository m) mappings) $ throwError err404
  CodeContext.deleteCodeMapping pid mid
  done . MappingsPanel . toHtml . getResponse =<< repositorySourceGetH pid rid Nothing Nothing


-- | The panel, rebuilt from scratch after every change. Listing the account's repositories is
-- a GitHub call, so it happens once here rather than per render inside the view.
--
-- Without a linked account there is nothing to map onto, so the page says so and offers the
-- one control that fixes it rather than a form whose every submission would be discarded.
codeMappingsEditorGetH :: Projects.ProjectId -> Maybe GitSync.GitHubCredentialId -> Maybe Text -> ATAuthCtx (RespHeaders MappingsPanel)
codeMappingsEditorGetH pid credentialM sampleM = do
  _ <- Projects.sessionAndProject pid
  addRespHeaders . MappingsPanel =<< codeMappingsContent pid sampleM credentialM Nothing


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
    unless (null mappings) $ section_ [class_ "space-y-3 rounded-xl border border-strokeWeak p-4 sm:p-5"] do
      h2_ [class_ "text-base font-semibold text-textStrong"] "Linked services"
      div_ [class_ "divide-y divide-strokeWeak rounded-lg bg-fillWeak"] $ forM_ mappings (mappingRow_ canEdit)
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
        a_ [href_ (urls.repositoryConnectGet pid Nothing), class_ "btn btn-sm btn-primary gap-2"] do
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
      p_ [class_ "text-xs text-textWeak"] $ toHtml $ "The Production impact review routine reviews pull requests for linked repositories. " <> askHint
      a_ [href_ (urls.aiRoutinesGet pid <> "#routine-production-impact-review"), class_ "text-sm text-textBrand hover:underline"] "Manage review routine and runs"
      when (T.null authConfig.githubAppWebhookSecret) $ p_ [class_ "text-xs text-textError"] "Reviews need a GitHub App webhook secret. Ask your Monoscope administrator to configure it."
      when (null reviewSettings) $ p_ [class_ "text-xs text-textWeak"] "Link a repository using a GitHub App installation to enable automatic reviews."
      forM_ reviewSettings \settings ->
        if canEdit
          then form_ ([class_ "flex flex-wrap items-end gap-3 rounded-lg border border-strokeWeak p-3", hxPost_ (maybe (urls.impactReviewSettingsPost pid) (urls.repositoryReviewsPost pid . (.id)) repositoryM), hxTarget_ "#code-mappings-content"] <> scopedSwap) $ do
            span_ [class_ "text-sm font-medium text-textStrong"] $ toHtml (settings.owner <> "/" <> settings.repo)
            reviewSettingsFields_ settings.owner settings.repo (Just settings)
            button_ [type_ "submit", class_ "btn btn-sm btn-primary"] "Save review settings"
          else div_ [class_ "space-y-1 rounded-lg border border-strokeWeak p-3"] do
            p_ [class_ "text-sm font-medium text-textStrong break-all"] $ toHtml $ settings.owner <> "/" <> settings.repo
            colorChip_ NeutralBadge "code-branch" $ triggerLabel settings.trigger
            p_ [class_ "text-xs text-textWeak"] $ if settings.includeEvidence then "Production evidence: aggregates and links" else "Production evidence: links only"
  where
    sample = fromMaybe "" sampleM
    editorUrl = maybe (urls.codeMappingsEditor pid Nothing Nothing) (\r -> urls.repositorySourceGet pid r.id Nothing Nothing) repositoryM
    postUrl = maybe (urls.codeMappingsPost pid) (urls.repositorySourcePost pid . (.id)) repositoryM
    scopedSwap = [attr | isJust repositoryM, attr <- [hxSelect_ "#code-mappings-content", hxSwap_ "outerMorph"]]

    -- One linked repository, read as a sentence rather than as five columns of path fragments.
    mappingRow_ :: Bool -> CodeContext.CodeMapping -> Html ()
    mappingRow_ canEdit cm = div_ [class_ "grid grid-cols-[auto_minmax(0,1fr)_auto] items-center gap-x-3 gap-y-2 px-3 py-3 text-sm"] do
      faSprite_ "github" "regular" "w-3.5 h-3.5 shrink-0 text-iconNeutral"
      span_ [class_ "min-w-0 font-medium text-textStrong break-all"] $ toHtml (cm.owner <> "/" <> cm.repo)
      when canEdit
        $ button_
          ( [ class_ "ml-auto shrink-0 btn btn-xs btn-ghost text-textError gap-1"
            , hxDelete_ (maybe (urls.codeMappingsDelete pid cm.id) (\r -> urls.repositorySourceDelete pid r.id cm.id) repositoryM)
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
        ( [ class_ "space-y-4 rounded-xl border border-strokeWeak bg-bgBase p-4 sm:p-5"
          , hxPost_ postUrl
          , hxTarget_ "#code-mappings-content"
          , hxIndicator_ "#code-mapping-indicator"
          ]
            <> scopedSwap
        )
        $ do
          h2_ [class_ "text-base font-semibold text-textStrong"] "Link a service"
          input_ [type_ "hidden", name_ "credentialId", value_ credential.id.toText]
          div_ [class_ "grid grid-cols-1 gap-4 sm:grid-cols-2"] do
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
  pure $ PageCtx bw{prePageTitle = Just "Repositories", pageTitle = "Connect repositories", menuItem = Just "Repositories"} RepositoryConnectGet{projectId, accounts, credential, repositories, selected = [], connectionError = Nothing}


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
      redirectCS $ urls.repositoriesGet pid Nothing Nothing
      addRespHeaders page


instance ToHtml RepositoryConnectGet where
  toHtml page = section_ [class_ "w-full min-w-0 space-y-6 p-4 sm:p-6"] do
    let connectionUrl = urls.repositoryConnectGet page.projectId ((.id) <$> page.credential)
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Connect repositories"
      p_ [class_ "text-sm text-textWeak max-w-2xl"] "Choose repositories to add. Monoscope analyzes their services and enables source context, PR reviews, and dashboard and monitor sync where setup is clear. This can post reviews and open configuration PRs. You can disable each capability at any time."
    div_ [id_ "repository-connect-content", class_ "space-y-5 rounded-xl border border-strokeWeak bg-bgBase p-4 sm:p-5"] do
      unless (null page.accounts) $ form_
        [action_ (urls.repositoryConnectGet page.projectId Nothing), method_ "get", hxGet_ (urls.repositoryConnectGet page.projectId Nothing), hxTrigger_ "change", hxPushUrl_ "true", hxTarget_ "#repository-connect-content", hxSelect_ "#repository-connect-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-account-indicator", class_ "space-y-2"]
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
      a_ [href_ (urls.githubAppInstall page.projectId (Just "code")), class_ "btn btn-sm btn-outline"] "Connect GitHub account"
      p_ [class_ "border-t border-strokeWeak pt-4 text-xs text-textWeak"] do
        "Using another Git host or a repository token? "
        a_ [href_ (urls.repositoryTokenGet page.projectId), class_ "text-textBrand underline underline-offset-2"] "Connect with a token"
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
  pure $ PageCtx bw{prePageTitle = Just "Repositories", pageTitle = "Connect repositories", pageTitleUrl = Just $ urls.repositoryConnectGet pid Nothing, pageTitleSuffix = Just "Token", menuItem = Just "Repositories"} $ RepositoryTokenGet pid Git.GitHub Nothing "" Nothing


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
    conn <- hoistEither $ first Git.gitConnErrorMessage $ Git.mkGitConn form.host form.apiBase token
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
      redirectCS $ urls.repositoryGet pid repository.id
      addRespHeaders safePage


instance ToHtml RepositoryTokenGet where
  toHtml page = section_ [class_ "w-full min-w-0 space-y-6 p-4 sm:p-6"] do
    header_ [class_ "space-y-2"] do
      h1_ [class_ "text-xl font-semibold text-textStrong"] "Connect with a token"
      p_ [class_ "text-sm text-textWeak"] "Monoscope analyzes this repository and configures source context and configuration sync where supported. Automatic PR reviews require the GitHub App. You can disable each capability later."
    div_ [id_ "repository-token-content", class_ "space-y-4 rounded-xl border border-strokeWeak bg-bgBase p-4 sm:p-5"] do
      whenJust page.connectionError $ p_ [role_ "alert", class_ "text-sm text-textError break-words"] . toHtml
      form_ [action_ (urls.repositoryTokenPost page.projectId), method_ "post", hxPost_ (urls.repositoryTokenPost page.projectId), hxTarget_ "#repository-token-content", hxSelect_ "#repository-token-content", hxSwap_ "outerMorph", hxIndicator_ "#repository-token-indicator", class_ "space-y-4"] do
        formSelectField_ FieldSm "Git host" "host" True $ forM_ (universe @Git.GitHost) \host ->
          option_ ([value_ (Git.hostSlug host)] <> [selected_ "selected" | host == page.host]) $ toHtml $ Git.hostLabel host
        formField_ FieldSm def{value = fromMaybe "" page.apiBase, placeholder = "https://git.example.com"} "Server URL (for self-hosted Git)" "apiBase" False Nothing
        formField_ FieldSm def{value = page.repoFullName, placeholder = "team/checkout"} "Full repository name" "repoFullName" True Nothing
        formField_ FieldSm def{inputType = "password", extraAttrs = [autocomplete_ "new-password"]} "Access token" "accessToken" True Nothing
        p_ [class_ "text-xs text-textWeak"] "The token needs read access to this repository. Dashboard sync will also need write access if you enable it later."
        details_ [class_ "space-y-3 rounded-lg bg-fillWeak p-3"] do
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
  hoistEither $ first Git.gitConnErrorMessage $ GitSync.credentialConn cred token


-- | Save a mapping. The two path fields are derived from the sample frame path when one is
-- given and they were left blank, which is the path most users take — nobody knows offhand
-- that their container mounts the app at @\/srv\/app@.
codeMappingsPostH :: Projects.ProjectId -> CodeMappingForm -> ATAuthCtx (ActionResult MappingRejection MappingsPanel)
codeMappingsPostH pid form = act do
  saved <- saveCodeMapping pid form
  forM saved \() -> MappingsPanel <$> codeMappingsContent pid (nonEmptyT form.samplePath) form.credentialId Nothing


-- | The mappings panel or page as re-rendered after a mapping is saved.
newtype MappingsPanel = MappingsPanel (Html ())
  deriving newtype (ToHtml)
  deriving (Outcome) via Rendered MappingsPanel


-- | Why a code mapping was not saved.
data MappingRejection = NoAccount | NoRepository | RepositoryUnreadable Text Text | NoFileMatches Text Text Text
  deriving stock (Eq, Show)


instance Refusal MappingRejection where
  refusalMessage = \case
    NoAccount -> "Choose a repository account"
    NoRepository -> "Pick a repository"
    RepositoryUnreadable{} -> "Could not work out the mapping"
    NoFileMatches{} -> "Could not work out the mapping"
  refusalDetail = \case
    NoAccount -> Just "Select an account connected to this project, or connect another account."
    NoRepository -> Nothing
    RepositoryUnreadable repo err -> Just $ "Could not read " <> repo <> ": " <> err
    NoFileMatches repo ref sample -> Just $ "No file in " <> repo <> "@" <> ref <> " lines up with " <> sample <> ". Check the branch, or fill the paths in under Advanced."


saveCodeMapping :: Projects.ProjectId -> CodeMappingForm -> ATAuthCtx (Either MappingRejection ())
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
      forM derived \(prefix, root) -> do
        CodeContext.insertCodeMapping pid cred.id repoRef (nonEmptyT form.service) prefix root
        addSuccessToast (repo <> " linked") Nothing
    (Nothing, _) -> pure $ Left NoAccount
    (_, Nothing) -> pure $ Left NoRepository
  where
    -- Work the two path fields out of one real frame path by matching it against the repo's
    -- file list. The file that matched is dropped: what the user checks is the mapping the row
    -- then spells out, not a path they would have to compare by eye.
    deriveFromRepo :: GitSync.GitHubCredential -> GitSync.RepoRef -> Text -> ATAuthCtx (Either MappingRejection (Text, Text))
    deriveFromRepo cred repoRef sample = do
      cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
      W.runHTTPWreq $ runExceptT do
        let unreadable :: Either Text b -> Either MappingRejection b
            unreadable = first (RepositoryUnreadable repoRef.repo)
        conn <- ExceptT $ unreadable <$> runExceptT (repoConn cfg cred)
        -- Whole tree, not a prefix: the point is to find where the frame path lands, and we do not
        -- yet know which directory that is. Paths only, so Bitbucket's missing blob shas cost nothing.
        (_, entries) <- ExceptT $ unreadable <$> Git.fetchTree conn repoRef ""
        hoistEither
          $ maybeToRight (NoFileMatches repoRef.repo repoRef.ref sample)
          $ (\(prefix, root, _) -> (prefix, root))
          <$> CodeContext.deriveMapping (map (.path) entries) sample


codeMappingsDeleteH :: Projects.ProjectId -> CodeContext.CodeMappingId -> ATAuthCtx (RespHeaders (Done MappingsPanel))
codeMappingsDeleteH pid mid = do
  requireReviewWrite pid
  CodeContext.deleteCodeMapping pid mid
  done . MappingsPanel =<< codeMappingsContent pid Nothing Nothing Nothing


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


impactReviewSettingsPostH :: Projects.ProjectId -> ImpactReviews.ReviewSettings -> ATAuthCtx (RespHeaders (Done (ReviewChange MappingsPanel)))
impactReviewSettingsPostH pid settings = do
  requireReviewWrite pid
  ImpactReviews.updateSettings pid settings{ImpactReviews.owner = T.toLower settings.owner, ImpactReviews.repo = T.toLower settings.repo}
  done . SettingsSaved . MappingsPanel =<< codeMappingsContent pid Nothing Nothing Nothing


impactReviewRetryH :: Projects.ProjectId -> ImpactReviews.ReviewId -> ATAuthCtx (RespHeaders (Done (ReviewChange (PageCtx RepositoriesGet))))
impactReviewRetryH pid rid = do
  requireReviewWrite pid
  ImpactReviews.retryRun pid rid
  done . RerunQueued . getResponse =<< repositoriesPageH pid ReviewsPage


requireReviewWrite :: Projects.ProjectId -> ATAuthCtx ()
requireReviewWrite = void . ProjectMembers.sessionAndProjectAtLeast ProjectMembers.PEdit
