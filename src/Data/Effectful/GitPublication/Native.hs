module Data.Effectful.GitPublication.Native (publish, Stage (..), Failure (..), Authorization (..), MediaUrls (..), Receipt (..), Visibility (..), PullState (..), Attachment (..), attachmentFile, attachmentReference, attachmentUrl, mediaMarker, publicationMarker, publicationBody, normalizeBody) where

import Control.Monad (foldM)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson qualified as AE
import Data.Char (isAlphaNum, isAscii, isHexDigit)
import Data.Effectful.AgentWorkspace (captureProcess, gitCredentials, gitEnvironment, sync)
import Data.Effectful.AgentWorkspace qualified as AgentIO
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), artifactCheckout, readExplanationMedia, readPrivateArtifact, withVerifiedArtifacts)
import Data.Effectful.CodeReview.Types (ArtifactRequest (..), Explanation (..), FixResult (..), Revision, revisionText, validExplanation)
import Data.Effectful.CodeReview.Types qualified as Agent
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Network.HTTP.Types.URI (urlEncode)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Pkg.Git (PullState (..), Visibility (..))
import Pkg.Git qualified as Git
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesPathExist)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (isAbsolute, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files qualified as Posix
import System.Process.Typed (ExitCode (..), nullStream, proc, setEnv, setStdin, setWorkingDir)
import UnliftIO.Exception (tryAny)
import UnliftIO.Timeout qualified as Timeout


data Stage = Prepare | Discover | Branch | Create | AttachVideo | AttachDiagram | Complete
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" Stage


data Failure = InvalidRequest | IdentityChanged | AccessRevoked | PathChanged | NativeUnavailable | SourceChanged | VisibilityChanged | PullRequestChanged | UploadUncertain | StateChanged | GitHubFailure | PushFailed | NotConfigured | ProtocolFailure | DeadlineExceeded | InvalidReceipt
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Authorization = Authorization
  { projectId :: UUIDId "project"
  , runId :: UUIDId "issue_fix"
  , head :: Revision
  , source :: Text
  , mediaSha256 :: Text
  , branch :: Text
  , targetBranch :: Text
  , repo :: Text
  , visibility :: Visibility
  , description :: Text
  , stage :: Stage
  }
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data MediaUrls = MediaUrls {video :: Text, diagram :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Receipt = Receipt {number :: Word64, url :: Text, head :: Revision, source :: Text, mediaSha256 :: Text, assets :: MediaUrls, draft :: Bool, state :: PullState}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Journal constructors retain their existing JSON names.
data Attachment = Video | Diagram
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Publication uses the rendered PNG for the diagram slot; the retained diagram is SVG.
-- >>> map attachmentFile [Video, Diagram]
-- ["explanation.mp4","cause.png"]
attachmentFile :: Attachment -> Text
attachmentFile = Agent.assetFile . \case Video -> Agent.Video; Diagram -> Agent.Cause


attachmentLink :: Attachment -> Text -> Text
attachmentLink = \case Video -> id; Diagram -> ("![Reproduction, inferred cause, and verified fix](" <>) . (<> ")")


-- | Local references retain the upload CLI's existing Markdown contract.
-- >>> map attachmentReference [Video, Diagram]
-- ["![](explanation.mp4)","![Reproduction, inferred cause, and verified fix](cause.png)"]
attachmentReference :: Attachment -> Text
attachmentReference asset = case asset of Video -> "![](" <> attachmentFile asset <> ")"; Diagram -> attachmentLink asset (attachmentFile asset)


-- | Only GitHub's credential-free, query-free attachment UUID URLs are accepted.
-- >>> attachmentUrl "https://github.com/user-attachments/assets/00000000-0000-4000-8000-000000000001"
-- True
-- >>> let root = "https://github.com/user-attachments/assets/" :: Text
-- >>> attachmentUrl (root <> "AABBCCDD-0000-4000-8000-000000000001")
-- True
-- >>> any attachmentUrl ["http://github.com/user-attachments/assets/00000000-0000-4000-8000-000000000001", root <> "00000000-0000-4000-8000-000000000001?download=1", root <> "00000000-0000-4000-8000-000000000001#fragment", "https://token@github.com/user-attachments/assets/00000000-0000-4000-8000-000000000001", "https://github.com:443/user-attachments/assets/00000000-0000-4000-8000-000000000001", root <> "not-a-uuid"]
-- False
attachmentUrl :: Text -> Bool
attachmentUrl url = maybe False (isJust . UUID.fromText) $ T.stripPrefix "https://github.com/user-attachments/assets/" url


mediaMarker :: Map Text Text -> Attachment -> Text
mediaMarker files asset = "<!-- monoscope-fix-media:" <> (case asset of Video -> "video"; Diagram -> "diagram") <> ":" <> fromMaybe "" (Map.lookup (attachmentFile asset) files) <> " -->"


publicationMarker :: FixResult -> Explanation -> Text
publicationMarker result explanation = "<!-- monoscope-fix project=" <> result.projectId.toText <> " run=" <> result.runId.toText <> " candidate=" <> result.candidateId.toText <> " source=" <> explanation.source <> " -->"


-- | The upload body keeps the existing media markers, ordering, Markdown and final newline.
-- >>> let files = Map.fromList [("explanation.mp4", "video-digest"), ("cause.png", "diagram-digest")] :: Map Text Text
-- >>> publicationBody "description" files (Map.fromList [(Video, "video-url"), (Diagram, "diagram-url")]) Nothing
-- "description\n\n<!-- monoscope-fix-media:video:video-digest -->\nvideo-url\n\n<!-- monoscope-fix-media:diagram:diagram-digest -->\n![Reproduction, inferred cause, and verified fix](diagram-url)\n"
-- >>> publicationBody "description" files Map.empty (Just Diagram)
-- "description\n\n_Video attachment pending._\n\n<!-- monoscope-fix-media:diagram:diagram-digest -->\n![Reproduction, inferred cause, and verified fix](cause.png)\n"
publicationBody :: Text -> Map Text Text -> Map Attachment Text -> Maybe Attachment -> Text
publicationBody description files urls uploading =
  description
    <> "\n\n"
    <> T.intercalate
      "\n\n"
      [ case Map.lookup asset urls of
          Just url -> mediaMarker files asset <> "\n" <> attachmentLink asset url
          Nothing | uploading == Just asset -> mediaMarker files asset <> "\n" <> attachmentReference asset
          _ -> "_" <> (case asset of Video -> "Video"; Diagram -> "Diagram") <> " attachment pending._"
      | asset <- [Video, Diagram]
      ]
    <> "\n"


normalizeBody :: Text -> Text
normalizeBody = T.strip . T.replace "\r\n" "\n"


-- $setup
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Relude


data PublicationState = Prepared | Draft Word64 | Uploading Word64 Attachment | Finished Word64
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Journal = Journal {identity :: Authorization, state :: PublicationState}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Backend returns a receipt, never the application's PublishedFix capability.
publish :: ArtifactStore -> FilePath -> Text -> ArtifactRequest -> Explanation -> Authorization -> (Stage -> IO (Either Failure ())) -> IO (Either Failure Receipt)
publish store executable token artifacts explanation context authorize = do
  outcome <- tryAny $ Timeout.timeout (480 * 1000000) $ runExceptT $ do
    ensure NativeUnavailable $ isAbsolute executable && validToken token
    permit Prepare
    ensure IdentityChanged $ validExplanation result explanation && context.projectId == result.projectId && context.runId == result.runId && context.head == result.head && context.source == explanation.source && artifacts.repository == "https://github.com/" <> context.repo <> ".git" && context.stage == Prepare
    root <- liftIO $ canonicalizePath store.usageDir
    let run = root </> "fixes" </> toString result.runId.toText
    liftIO (canonicalizePath run) >>= ensure PathChanged . (== run)
    directory <-
      foldM
        ( \parent part -> do
            let child = parent </> part
            liftIO $ createDirectoryIfMissing False child
            liftIO (canonicalizePath child) >>= ensure PathChanged . (== child)
            liftIO $ Posix.setFileMode child 0o700 >> sync parent
            pure child
        )
        run
        ["publications-native", toString result.candidateId.toText, toString explanation.source]
    ExceptT $ withFileLock (directory </> ".lock") Exclusive $ const $ runExceptT $ do
      let statePath = directory </> "state.json"
          save :: PublicationState -> ExceptT Failure IO ()
          save progress = liftIO $ AgentIO.publish statePath $ Journal context progress
      exists <- liftIO $ doesPathExist statePath
      journal <- if exists then decode StateChanged =<< liftIO (readPrivateArtifact directory "state.json" 65536) else pure $ Journal context Prepared
      ensure IdentityChanged $ journal.identity == context
      save journal.state
      ExceptT $ withSystemTempDirectory "monoscope-native-publication" \temporary -> runExceptT do
        environment <- liftIO $ filter ((== "PATH") . fst) <$> getEnvironment
        gitEnv <- liftIO $ gitEnvironment "https" []
        let command :: Failure -> FilePath -> [(String, String)] -> FilePath -> [String] -> ExceptT Failure IO ByteString
            command failure cwd env name args = do
              answer <- liftIO $ tryAny $ Timeout.timeout (90 * 1000000) $ captureProcess 1048576 (setWorkingDir cwd $ setEnv env $ setStdin nullStream $ proc name args)
              case answer of
                Right (Just (ExitSuccess, out, _)) -> pure out
                _ -> throwE failure
            ghEnv = environment <> [("GH_CONFIG_DIR", temporary </> "gh"), ("GH_TOKEN", toString token), ("GH_HOST", "github.com"), ("GH_PROMPT_DISABLED", "1"), ("GH_NO_UPDATE_NOTIFIER", "1")]
            gh = command GitHubFailure temporary ghEnv executable
            encoded :: Text -> String
            encoded = toString . decodeUtf8 @Text . urlEncode True . encodeUtf8
            endpoint = "/repos/" <> toString context.repo
            api :: AE.FromJSON a => Text -> String -> Maybe AE.Value -> ExceptT Failure IO a
            api method path payload = do
              for_ payload $ liftIO . writeFileLBS (temporary </> "request.json") . AE.encode
              decode GitHubFailure =<< gh (["api", "--hostname", "github.com", "--method", toString method, path] <> [arg | isJust payload, arg <- ["--input", temporary </> "request.json"]])
            -- Pagination flags belong to gh, not the endpoint's query string.
            pages :: AE.FromJSON a => String -> ExceptT Failure IO a
            pages path = decode GitHubFailure =<< gh ["api", "--hostname", "github.com", "--method", "GET", path, "--paginate", "--slurp"]
            refs name = do
              matches <- concat <$> (pages (endpoint <> "/git/matching-refs/heads/" <> encoded name) :: ExceptT Failure IO [[Ref]])
              pure $ (.object.sha) <$> find ((== "refs/heads/" <> name) . (.ref)) matches
            access = do
              metadata <- api "GET" endpoint Nothing
              ensure VisibilityChanged $ (metadata :: Git.RepositoryAccess).visibility == context.visibility
              ensure AccessRevoked metadata.permissions.push
              ensure IdentityChanged $ T.toCaseFold metadata.fullName == T.toCaseFold context.repo
            guardRemote stage = do
              permit stage
              access
              refs context.targetBranch >>= ensure SourceChanged . (== Just (revisionText result.base))
              when (stage /= Branch) $ refs context.branch >>= ensure SourceChanged . (== Just (revisionText result.head))
            checked :: Git.PublicationPullRequest -> ExceptT Failure IO Git.PublicationPullRequest
            checked pr = do
              ensure PullRequestChanged $ pr.number > 0 && pr.head.sha == revisionText result.head && pr.head.ref == context.branch && pr.base.ref == context.targetBranch && all ((== T.toCaseFold context.repo) . T.toCaseFold) [pr.head.repo.fullName, pr.base.repo.fullName] && T.toCaseFold pr.htmlUrl == T.toCaseFold ("https://github.com/" <> context.repo <> "/pull/" <> show pr.number) && marker `elem` lines (normalizeBody $ fromMaybe "" pr.body)
              pure pr
            fetch number = api "GET" (endpoint <> "/pulls/" <> show number) Nothing >>= checked
            marker = publicationMarker result explanation
            observed :: Git.PublicationPullRequest -> ExceptT Failure IO (Map Attachment Text)
            observed pr =
              Map.fromList . catMaybes <$> forM [Video, Diagram] \asset -> do
                let rest = [drop (index + 1) rows | let rows = lines (normalizeBody $ fromMaybe "" pr.body), (index, line) <- zip [0 ..] rows, line == mediaMarker explanation.files asset]
                ensure PullRequestChanged $ length rest <= 1
                pure $ do
                  following <- viaNonEmpty Relude.head rest
                  line <- find (not . T.null . T.strip) following
                  url <- if asset == Video then Just line else T.stripPrefix "![Reproduction, inferred cause, and verified fix](" line >>= T.stripSuffix ")"
                  guard $ attachmentUrl url
                  pure (asset, url)
            body = publicationBody context.description explanation.files
        void $ command InvalidRequest temporary gitEnv "git" ["check-ref-format", "refs/heads/" <> toString context.targetBranch]
        help <- decodeUtf8 @Text <$> gh ["pr", "edit", "--help"]
        ensure NativeUnavailable $ "--attach" `elem` words help
        for_ [Video, Diagram] \asset -> do
          bytes <- ExceptT $ first (const SourceChanged) <$> readExplanationMedia store.usageDir explanation (attachmentFile asset)
          liftIO $ writeFileBS (temporary </> toString (attachmentFile asset)) bytes
        -- The verifier owns and cleans up the exact bundle-derived checkout.
        verified <- liftIO $ withVerifiedArtifacts store artifacts \verifiedArtifacts -> runExceptT do
          let checkout = artifactCheckout verifiedArtifacts
          permit Discover
          access
          matches <- concat <$> (pages (endpoint <> "/pulls?state=all&head=" <> encoded (T.takeWhile (/= '/') context.repo <> ":" <> context.branch) <> "&per_page=100") :: ExceptT Failure IO [[PullNumber]])
          existing <- case matches of [] -> pure Nothing; [pr] -> Just <$> fetch pr.number; _ -> throwE PullRequestChanged
          ensure StateChanged $ case journal.state of Prepared -> True; Draft n -> fmap (.number) existing == Just n; Uploading n _ -> fmap (.number) existing == Just n; Finished n -> fmap (.number) existing == Just n
          pr <- case existing of
            Just found -> pure found
            Nothing -> do
              guardRemote Branch
              previous <- refs context.branch
              ensure SourceChanged $ isNothing previous || previous == Just (revisionText result.head)
              when (isNothing previous) do
                let pushEnv = gitEnv <> gitCredentials token
                void $ command PushFailed checkout pushEnv "git" ["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false", "push", "--porcelain", "--force-with-lease=refs/heads/" <> toString context.branch <> ":", toString artifacts.repository, toString (revisionText result.head) <> ":refs/heads/" <> toString context.branch]
              guardRemote Create
              created <- api "POST" (endpoint <> "/pulls") (Just $ AE.object ["title" AE..= result.summary, "head" AE..= context.branch, "base" AE..= context.targetBranch, "draft" AE..= True, "body" AE..= body Map.empty Nothing]) >>= checked
              save $ Draft created.number
              pure created
          (final, progress) <-
            foldM
              ( \(current, progress) asset -> do
                  remote <- fetch current.number
                  urls <- observed remote
                  if Map.member asset urls
                    then do
                      let next = case progress of Uploading n pending | pending == asset -> Draft n; _ -> progress
                      save next
                      pure (remote, next)
                    else do
                      case progress of Finished _ -> throwE StateChanged; Uploading _ _ -> throwE UploadUncertain; _ -> pass
                      ensure PullRequestChanged $ remote.state == Open && remote.draft && normalizeBody (fromMaybe "" remote.body) == normalizeBody (body urls Nothing)
                      guardRemote $ if asset == Video then AttachVideo else AttachDiagram
                      save $ Uploading remote.number asset
                      liftIO $ writeFileBS (temporary </> "description.md") $ encodeUtf8 $ body urls (Just asset)
                      void $ gh ["pr", "edit", show remote.number, "--repo", toString context.repo, "--body-file", temporary </> "description.md", "--attach", temporary </> toString (attachmentFile asset)]
                      after <- fetch remote.number
                      observed after >>= ensure UploadUncertain . Map.member asset
                      save $ Draft after.number
                      pure (after, Draft after.number)
              )
              (pr, journal.state)
              [Video, Diagram]
          case progress of Finished _ -> permit Complete >> access; _ | final.state == Closed -> permit Complete >> access; _ -> guardRemote Complete
          urls <- observed final
          ensure UploadUncertain $ Map.size urls == 2
          ensure PullRequestChanged $ final.state == Open || final.merged
          video <- maybe (throwE UploadUncertain) pure $ Map.lookup Video urls
          diagram <- maybe (throwE UploadUncertain) pure $ Map.lookup Diagram urls
          ensure InvalidReceipt $ normalizeBody (fromMaybe "" final.body) == normalizeBody (body urls Nothing) && video /= diagram
          save $ Finished final.number
          pure $ Receipt final.number final.htmlUrl result.head explanation.source context.mediaSha256 (MediaUrls video diagram) final.draft final.state
        hoistEither $ fromRight (Left SourceChanged) verified
  pure $ case outcome of Left _ -> Left GitHubFailure; Right Nothing -> Left DeadlineExceeded; Right (Just result') -> result'
  where
    result = artifacts.result
    permit stage = ExceptT $ authorize stage
    ensure failure = flip unless $ throwE failure
    validToken value = T.length value == 40 && T.all isHexDigit value || any (\prefix -> maybe False (\rest -> not (T.null rest) && T.all (\c -> isAscii c && (isAlphaNum c || c == '_')) rest) $ T.stripPrefix prefix value) ["ghp_", "gho_", "github_pat_"]


newtype PullNumber = PullNumber {number :: Word64}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


newtype Object = Object {sha :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Ref = Ref {ref :: Text, object :: Object}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


decode :: AE.FromJSON a => Failure -> ByteString -> ExceptT Failure IO a
decode failure = either (const $ throwE failure) pure . AE.eitherDecodeStrict
