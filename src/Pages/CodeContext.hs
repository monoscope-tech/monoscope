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
import Effectful.Reader.Static qualified
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxDelete_, hxPost_, hxSwap_, hxTarget_)
import Models.Projects.CodeContext qualified as CodeContext
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (withSettingsPage)
import Pages.Components (FieldCfg (..), FieldSize (..), formField_, settingsH2_, settingsSection_)
import Relude
import System.Config (AuthContext (..), EnvConfig (..))
import System.Types (ATAuthCtx, RespHeaders, addRespHeaders)
import Utils (faSprite_)
import Web.FormUrlEncoded (FromForm)


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
  case (fileM >>= guarded (not . T.null), lineM) of
    (Just path, Just n) ->
      W.runHTTPWreq (CodeContext.fetchSnippet authCtx.config pid svcM (revM >>= guarded (not . T.null)) path n)
        >>= addRespHeaders
        . either note_ snippet_
    _ -> addRespHeaders $ note_ "This frame has no file and line to look up."
  where
    note_ :: Text -> Html ()
    note_ msg = div_ [class_ "pl-5 py-1 text-2xs text-textWeak italic"] $ toHtml msg
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


-- | Mapping form. @repo@ is the only required field: the account comes from the credential,
-- and both path fields default to @""@ (repo root, no prefix to strip), which is the right
-- mapping for a service whose frames are already repo-relative.
data CodeMappingForm = CodeMappingForm
  { repo :: Maybe Text
  , ref :: Maybe Text
  , service :: Maybe Text
  , pathPrefix :: Maybe Text
  , sourceRoot :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


codeMappingsGetH :: Projects.ProjectId -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsGetH pid = withSettingsPage pid "Integrations" \_ -> do
  creds <- codeContextCredentials pid
  mappings <- CodeContext.getCodeMappings pid
  pure $ settingsSection_ do
    settingsH2_ "Source Code"
    div_ [id_ "code-mappings-content"] $ codeMappingsView pid creds mappings


-- | The accounts this project can read source from.
--
-- A project that connected GitHub for dashboard sync has already granted an installation, and
-- that installation reaches every repo in the account — so adopt it rather than asking for a
-- second authorisation to the same place. Adding an account beyond that one is installing the
-- App there, which is the same flow the git-sync settings already run.
codeContextCredentials :: Projects.ProjectId -> ATAuthCtx [GitSync.GitHubCredential]
codeContextCredentials pid = do
  authCtx <- Effectful.Reader.Static.ask @AuthContext
  existing <- GitSync.getGitHubCredentials pid
  if null existing
    then
      GitSync.getGitHubSync pid >>= \case
        Just sync | isJust sync.installationId || isJust sync.accessToken -> do
          -- The sync row holds the PAT already encrypted; hand the credential the plaintext
          -- so it is stored under this table's own encryption rather than double-wrapped.
          let encKey = encodeUtf8 @Text authCtx.config.apiKeyEncryptionSecretKey
          plain <- GitSync.getGitHubSyncDecrypted encKey pid
          maybeToList <$> GitSync.upsertGitHubCredential encKey pid sync.owner sync.installationId (plain >>= (.accessToken))
        _ -> pure []
    else pure existing


codeMappingsPostH :: Projects.ProjectId -> CodeMappingForm -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsPostH pid form = do
  creds <- codeContextCredentials pid
  -- One credential today, so it needs no picker; the account is still stored per mapping, so
  -- a second account is a form field rather than a migration.
  whenJust ((,) <$> viaNonEmpty head creds <*> (form.repo >>= guarded (not . T.null))) \(cred, repo) ->
    CodeContext.insertCodeMapping
      pid
      cred.id
      (GitSync.RepoRef cred.account repo (fromMaybe "main" $ form.ref >>= guarded (not . T.null)))
      (form.service >>= guarded (not . T.null))
      (fromMaybe "" form.pathPrefix)
      (fromMaybe "" form.sourceRoot)
  addRespHeaders . codeMappingsView pid creds =<< CodeContext.getCodeMappings pid


codeMappingsDeleteH :: Projects.ProjectId -> CodeContext.CodeMappingId -> ATAuthCtx (RespHeaders (Html ()))
codeMappingsDeleteH pid mid = do
  CodeContext.deleteCodeMapping pid mid
  creds <- codeContextCredentials pid
  addRespHeaders . codeMappingsView pid creds =<< CodeContext.getCodeMappings pid


-- | The mapping editor. Without a linked repository there is nothing to map onto, so the
-- page says so and points at the one control that fixes it rather than offering a form whose
-- every submission would be discarded.
codeMappingsView :: Projects.ProjectId -> [GitSync.GitHubCredential] -> [CodeContext.CodeMapping] -> Html ()
codeMappingsView pid creds mappings = div_ [class_ "space-y-6"] case viaNonEmpty head creds of
  Nothing -> do
    p_ [class_ "text-sm text-textWeak"] "Connect GitHub first — source snippets are read from your repositories through that installation."
    a_ [href_ ("/p/" <> pid.toText <> "/settings/git-sync"), class_ "btn btn-sm btn-primary gap-2"] do
      faSprite_ "github" "regular" "w-3.5 h-3.5"
      "Connect GitHub"
  Just cred -> do
    p_ [class_ "text-sm text-textWeak"] do
      "Stack frames show the source around the failing line, read from the repositories of "
      code_ [class_ "text-textBrand"] $ toHtml cred.account
      ". A mapping says which repository a stack-trace path belongs to, and how its path lines up with the repo's."
    p_ [class_ "text-2xs text-textWeak"] "Spans that report their commit sha are read at that revision; the branch below is the fallback for those that don't."
    if null mappings
      then p_ [class_ "text-sm text-textWeak italic"] "No mappings yet. Frames will show as plain text until one matches."
      else div_ [class_ "divide-y divide-strokeWeak rounded-xl border border-strokeWeak"] $ forM_ mappings \cm ->
        div_ [class_ "flex items-center gap-3 px-3 py-2 text-sm"] do
          span_ [class_ "font-mono text-xs text-textStrong truncate"] $ toHtml (dashIfBlank cm.pathPrefix)
          faSprite_ "arrow-right" "regular" "w-3 h-3 shrink-0 text-iconNeutral"
          span_ [class_ "font-mono text-xs text-textStrong truncate"] $ toHtml (cm.owner <> "/" <> cm.repo <> "@" <> cm.ref <> "/" <> dashIfBlank cm.sourceRoot)
          whenJust cm.service $ span_ [class_ "shrink-0 rounded-sm border border-strokeWeak px-1 text-2xs text-textWeak"] . toHtml
          button_
            [ class_ "ml-auto shrink-0 btn btn-xs btn-ghost text-textError"
            , hxDelete_ ("/p/" <> pid.toText <> "/settings/code-mappings/" <> cm.id.toText)
            , hxSwap_ "innerHTML"
            , hxTarget_ "#code-mappings-content"
            , Aria.label_ "Remove mapping"
            ]
            "Remove"
    form_ [class_ "pt-4 space-y-3 border-t border-strokeWeak", hxPost_ ("/p/" <> pid.toText <> "/settings/code-mappings"), hxSwap_ "innerHTML", hxTarget_ "#code-mappings-content"] do
      div_ [class_ "grid grid-cols-1 gap-3 md:grid-cols-5"] do
        formField_ FieldSm def{placeholder = "checkout-service"} "Repository" "repo" True Nothing
        formField_ FieldSm def{placeholder = "main"} "Branch" "ref" False Nothing
        formField_ FieldSm def{placeholder = "/srv/app/"} "Strip from frame path" "pathPrefix" False Nothing
        formField_ FieldSm def{placeholder = "src"} "Repo directory" "sourceRoot" False Nothing
        formField_ FieldSm def{placeholder = "any service"} "Only for service" "service" False Nothing
      button_ [class_ "btn btn-sm btn-outline", type_ "submit"] "Add mapping"
  where
    dashIfBlank t = if T.null t then "(repo root)" else t
