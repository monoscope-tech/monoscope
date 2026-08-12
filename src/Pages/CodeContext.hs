-- | The source behind one stack frame.
--
-- Its own module rather than a function in "Pages.LogExplorer.LogItem", because the model it
-- needs ("Models.Projects.CodeContext") reaches GitHub through @Models.Projects.GitSync@,
-- which reaches @Pkg.Components.Widget@, which imports @LogItem@ — a cycle. The renderer in
-- "Pages.Components" only ever builds the URL, so nothing on the rendering side has to know
-- this module exists.
module Pages.CodeContext (codeContextH, codeMappingsGetH, codeMappingsPostH, codeMappingsDeleteH, CodeMappingForm (..)) where

import Data.Default (def)
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Effectful (Eff, IOE, (:>))
import Effectful.Reader.Static qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxDelete_, hxIndicator_, hxPost_, hxSwap_, hxTarget_)
import Lucid.Hyperscript (__)
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (withSettingsPage)
import Pages.Components (FieldCfg (..), FieldSize (..), formField_, formSelectField_, settingsH2_, settingsSection_)
import Pkg.Git qualified as Git
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Utils (LoadingSize (..), faSprite_, htmxIndicator_)
import Web.FormUrlEncoded (FromForm)


-- | Absent and blank mean the same thing here: form fields and span attributes both arrive
-- as @""@ rather than missing.
nonBlank :: Maybe Text -> Maybe Text
nonBlank = (>>= guarded (not . T.null))


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
  case (nonBlank fileM, lineM) of
    (Just path, Just n) ->
      W.runHTTPWreq (CodeContext.fetchSnippet authCtx.config pid svcM (nonBlank revM) path n)
        >>= addRespHeaders
        . either reason_ snippet_
    _ -> addRespHeaders $ note_ "This frame has no file and line to look up." Nothing
  where
    -- An unmapped frame is the one failure the reader can act on from here, so it is the one
    -- that gets a control — carrying the path, so the form it opens is already filled in.
    reason_ :: CodeContext.SnippetError -> Html ()
    reason_ err = note_ (CodeContext.snippetErrorMessage err) case err of
      CodeContext.NoMapping path -> Just path
      CodeContext.CredentialGone -> Nothing
      CodeContext.ReadFailed{} -> Nothing
      CodeContext.LineOutOfRange{} -> Nothing
    note_ :: Text -> Maybe Text -> Html ()
    note_ msg fixPathM = div_ [class_ "pl-5 py-1 text-2xs text-textWeak italic flex items-center gap-2"] do
      toHtml msg
      whenJust fixPathM \path ->
        a_
          [ href_ ("/p/" <> pid.toText <> "/settings/code-mappings?sample=" <> path)
          , class_ "not-italic text-textBrand underline shrink-0"
          ]
          "Link a repository"
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
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | The mapping editor. @sample@ carries a stack-frame path in from the frame that had no
-- mapping, so arriving from an unmapped trace lands on a form already holding the example it
-- needs.
codeMappingsGetH :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsGetH pid sampleM = withSettingsPage pid "Integrations" \_ -> do
  view <- codeMappingsContent pid sampleM
  pure $ settingsSection_ do
    settingsH2_ "Source Code"
    div_ [id_ "code-mappings-content"] view


-- | The panel, rebuilt from scratch after every change. Listing the account's repositories is
-- a GitHub call, so it happens once here rather than per render inside the view.
codeMappingsContent :: Projects.ProjectId -> Maybe Text -> ATAuthCtx (Html ())
codeMappingsContent pid sample = do
  credM <- codeContextCredential pid
  mappings <- CodeContext.getCodeMappings pid
  repos <- maybe (pure []) (credentialRepos pid) credM
  pure $ codeMappingsView pid credM repos mappings (fromMaybe "" sample)


-- | The account this project can read source from.
--
-- A project that connected GitHub for dashboard sync has already granted an installation, and
-- that installation reaches every repo in the account — so adopt it rather than asking for a
-- second authorisation to the same place. Adding an account beyond that one is installing the
-- App there, which is the same flow the git-sync settings already run.
codeContextCredential :: Projects.ProjectId -> ATAuthCtx (Maybe GitSync.GitHubCredential)
codeContextCredential pid =
  GitSync.getGitHubCredentials pid >>= \case
    cred : _ -> pure $ Just cred
    [] ->
      GitSync.getGitHubSync pid >>= \case
        Just sync | isJust sync.installationId || isJust sync.accessToken -> do
          -- The sync row holds the PAT already encrypted; hand the credential the plaintext
          -- so it is stored under this table's own encryption rather than double-wrapped.
          encKey <- encodeUtf8 @Text . (.apiKeyEncryptionSecretKey) . (.config) <$> Effectful.Reader.Static.ask @AuthContext
          plain <- GitSync.getGitHubSyncDecrypted encKey pid
          GitSync.upsertGitHubCredential encKey pid sync.host sync.apiBase sync.owner sync.installationId (plain >>= (.accessToken))
        _ -> pure Nothing


-- | The repositories a credential reaches, for the picker.
--
-- Every host can answer this, but not always usefully: a repository-scoped GitLab or Bitbucket
-- token legitimately reaches exactly one repository, and a token with no listing scope reaches
-- none it can enumerate. An empty list is therefore a valid answer rather than a failure, and
-- the form keeps manual entry available for it.
credentialRepos :: Projects.ProjectId -> GitSync.GitHubCredential -> ATAuthCtx [Git.GitRepo]
credentialRepos pid cred = do
  cfg <- (.config) <$> Effectful.Reader.Static.ask @AuthContext
  result <- W.runHTTPWreq $ runExceptT $ repoConn cfg cred >>= ExceptT . Git.listRepos
  either (\err -> [] <$ Log.logWarn "Could not list repositories for code mappings" (pid, Git.hostSlug cred.host, err)) pure result


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
  credM <- codeContextCredential pid
  let sample = nonBlank form.samplePath
  case (credM, nonBlank form.repo) of
    (Just cred, Just repo) -> do
      let ref = fromMaybe "main" $ nonBlank form.ref
          repoRef = GitSync.RepoRef cred.account repo ref
          typed = (fromMaybe "" form.pathPrefix, fromMaybe "" form.sourceRoot)
      derived <- case sample of
        Just s | typed == ("", "") -> deriveFromRepo cred repoRef s
        _ -> pure $ Right typed
      case derived of
        Left err -> addErrorToast "Could not work out the mapping" (Just err)
        Right (prefix, root) -> do
          let svc = nonBlank form.service
          -- The row is keyed on (service, path prefix), so a second repository added with the
          -- same scope silently takes the first one's place. Saying so beats leaving someone
          -- to work out why the repo they linked five minutes ago stopped resolving.
          replaced <- find (\cm -> cm.service == svc && cm.pathPrefix == prefix && cm.repo /= repo) <$> CodeContext.getCodeMappings pid
          CodeContext.insertCodeMapping pid cred.id repoRef svc prefix root
          addSuccessToast (repo <> " linked") $ replaced <&> \old -> "Replaced " <> old.repo <> ", which covered the same frames. Give each repository its own frame path to keep both."
    (Nothing, _) -> addErrorToast "Connect GitHub first" Nothing
    (_, Nothing) -> addErrorToast "Pick a repository" Nothing
  addRespHeaders =<< codeMappingsContent pid sample


-- | Work the two path fields out of one real frame path by matching it against the repo's
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
    (prefix, root, _) <-
      hoistEither
        $ maybeToRight ("No file in " <> repoRef.repo <> "@" <> repoRef.ref <> " lines up with " <> sample <> ". Check the branch, or fill the paths in under Advanced.")
        $ CodeContext.deriveMapping (map (.path) entries) sample
    pure (prefix, root)


codeMappingsDeleteH :: Projects.ProjectId -> CodeContext.CodeMappingId -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsDeleteH pid mid = do
  CodeContext.deleteCodeMapping pid mid
  addRespHeaders =<< codeMappingsContent pid Nothing


-- | Without a linked account there is nothing to map onto, so the page says so and offers the
-- one control that fixes it rather than a form whose every submission would be discarded.
codeMappingsView :: Projects.ProjectId -> Maybe GitSync.GitHubCredential -> [Git.GitRepo] -> [CodeContext.CodeMapping] -> Text -> Html ()
codeMappingsView pid credM repos mappings sample = div_ [class_ "space-y-6"] case credM of
  Nothing -> do
    p_ [class_ "text-sm text-textWeak"] "Stack traces show the source around the failing line, read straight from your repositories. Install the GitHub App to turn it on — the same installation also powers dashboard sync, so this is the only authorisation either needs."
    a_ [href_ ("/p/" <> pid.toText <> "/settings/git-sync/install?to=code"), class_ "btn btn-sm btn-primary gap-2"] do
      faSprite_ "github" "regular" "w-3.5 h-3.5"
      "Connect GitHub"
  Just cred -> do
    p_ [class_ "text-sm text-textWeak"] do
      "Reading source from "
      code_ [class_ "text-textBrand"] $ toHtml cred.account
      ". Link each repository your services are built from; frames outside a linked repository stay plain text."
    if null mappings
      then p_ [class_ "text-sm text-textWeak italic"] "No repositories linked yet."
      else div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] $ forM_ mappings (mappingRow_ pid)
    addMappingForm_ pid repos sample cred.installationId


-- | One linked repository, read as a sentence rather than as five columns of path fragments.
mappingRow_ :: Projects.ProjectId -> CodeContext.CodeMapping -> Html ()
mappingRow_ pid cm = div_ [class_ "flex items-center gap-3 px-3 py-2 text-sm"] do
  faSprite_ "github" "regular" "w-3.5 h-3.5 shrink-0 text-iconNeutral"
  span_ [class_ "font-medium text-textStrong truncate"] $ toHtml (cm.owner <> "/" <> cm.repo)
  span_ [class_ "shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak font-mono"] $ toHtml cm.ref
  span_ [class_ "text-2xs text-textWeak truncate"] $ toHtml (scopeLabel cm)
  whenJust cm.service $ span_ [class_ "shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak"] . toHtml
  button_
    [ class_ "ml-auto shrink-0 btn btn-xs btn-ghost text-textError gap-1"
    , hxDelete_ ("/p/" <> pid.toText <> "/settings/code-mappings/" <> cm.id.toText)
    , hxSwap_ "innerHTML"
    , hxTarget_ "#code-mappings-content"
    , Aria.label_ ("Unlink " <> cm.owner <> "/" <> cm.repo)
    ]
    do
      faSprite_ "link-slash" "regular" "w-3 h-3"
      "Unlink"


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


-- | Add a repository. One picker and, when the paths are not already repo-relative, one
-- pasted stack-trace line — everything else is derived or defaulted, and the raw fields stay
-- available under Advanced for the cases the derivation cannot reach.
addMappingForm_ :: Projects.ProjectId -> [Git.GitRepo] -> Text -> Maybe Int64 -> Html ()
addMappingForm_ pid repos sample instIdM =
  form_
    [ class_ "pt-4 space-y-3 border-t border-strokeWeak"
    , hxPost_ ("/p/" <> pid.toText <> "/settings/code-mappings")
    , hxSwap_ "innerHTML"
    , hxTarget_ "#code-mappings-content"
    , hxIndicator_ "#code-mapping-indicator"
    ]
    do
      div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-3"] do
        -- Each option carries its own default branch, so the Branch field below can fill
        -- itself in and a repo whose trunk is not called "main" needs no correction.
        if null repos
          then formField_ FieldSm def{placeholder = "checkout-service"} "Repository" "repo" True Nothing
          else formSelectField_ FieldSm "Repository" "repo" True $ forM_ repos \r ->
            option_ [value_ r.name, term "data-branch" r.defaultBranch] $ toHtml r.fullName
        formField_
          FieldSm
          def
            { value = maybe "main" (.defaultBranch) (viaNonEmpty head repos)
            , placeholder = "main"
            , extraAttrs = [[__| on load or change from #repo set my value to #repo.selectedOptions[0].dataset.branch |] | not (null repos)]
            }
          "Branch"
          "ref"
          False
          Nothing
        formField_ FieldSm def{placeholder = "any service"} "Only for service" "service" False Nothing
      -- The picker can only offer what the installation was granted, so a missing repository
      -- is a narrower grant rather than a bug in the list, and this is where it is widened.
      whenJust instIdM \instId ->
        a_
          [ href_ (GitSync.installationSettingsUrl instId)
          , target_ "_blank"
          , rel_ "noopener"
          , class_ "text-xs text-textBrand underline inline-flex items-center gap-1"
          ]
          do
            "Repository missing? Add it to the installation on GitHub"
            faSprite_ "arrow-up-right-from-square" "regular" "w-2.5 h-2.5"
      formField_
        FieldSm
        def{value = sample, placeholder = "/srv/app/services/checkout.py"}
        "A file path from one of your stack traces"
        "samplePath"
        False
        Nothing
      p_ [class_ "text-xs text-textWeak"] "Paste one line's path and we work out how it lines up with the repository. Leave it blank if your traces already print repo-relative paths."
      details_ [class_ "group"] do
        summary_ [class_ "text-xs font-medium text-textWeak cursor-pointer list-none flex items-center gap-1.5 hover:text-textStrong"] do
          faSprite_ "chevron-right" "solid" "w-3 h-3 transition-transform group-open:rotate-90"
          "Advanced: set the paths myself"
        div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-2 pt-3"] do
          formField_ FieldSm def{placeholder = "/srv/app/"} "Strip from frame path" "pathPrefix" False Nothing
          formField_ FieldSm def{placeholder = "src"} "Directory inside the repo" "sourceRoot" False Nothing
      div_ [class_ "flex items-center gap-2"] do
        button_ [class_ "btn btn-sm btn-primary", type_ "submit"] "Link repository"
        htmxIndicator_ "code-mapping-indicator" LdXS
      p_ [class_ "text-2xs text-textWeak"] "Spans that report their commit sha are read at that revision; the branch above is the fallback for those that don't."
