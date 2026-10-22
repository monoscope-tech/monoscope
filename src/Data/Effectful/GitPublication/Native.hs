module Data.Effectful.GitPublication.Native (publish) where

import Conduit (runConduit, (.|))
import Control.Monad (foldM)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import "base64" Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isAscii, isHexDigit)
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), artifactCheckout, readPrivateArtifact, withVerifiedArtifacts)
import Data.Effectful.CodeReview.Explanation (readExplanationMedia)
import Data.Effectful.CodeReview.Types (ArtifactRequest (..), Explanation (..), FixResult (..), revisionText, validExplanation)
import Data.Effectful.GitPublication.Types
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Network.HTTP.Types.URI (urlEncode)
import Pkg.Git qualified as Git
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesPathExist, renameFile)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (isAbsolute, (</>))
import System.IO.Temp (withSystemTempDirectory, withTempFile)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Unistd (fileSynchronise)
import System.Process.Typed (ExitCode (..), createPipe, getStderr, getStdout, nullStream, proc, setEnv, setStderr, setStdin, setStdout, setWorkingDir, waitExitCode, withProcessTerm)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (bracket, tryAny)
import UnliftIO.Timeout qualified as Timeout


data Asset = Video | Diagram
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data PublicationState = Prepared | Draft Word64 | Uploading Word64 Asset | Finished Word64
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
          save progress = liftIO $ do
            withTempFile directory ".state" \path handle -> do
              LBS.hPut handle $ AE.encode $ Journal context progress
              hFlush handle
              Posix.setFileMode path 0o600
              sync path
              renameFile path statePath
              sync directory
      exists <- liftIO $ doesPathExist statePath
      journal <- if exists then decode StateChanged =<< liftIO (readPrivateArtifact directory "state.json" 65536) else pure $ Journal context Prepared
      ensure IdentityChanged $ journal.identity == context
      save journal.state
      ExceptT $ withSystemTempDirectory "monoscope-native-publication" \temporary -> runExceptT do
        environment <- liftIO $ filter ((== "PATH") . fst) <$> getEnvironment
        let command :: Failure -> FilePath -> [(String, String)] -> FilePath -> [String] -> ExceptT Failure IO ByteString
            command failure cwd env name args = do
              answer <- liftIO $ Timeout.timeout (90 * 1000000) $ withProcessTerm (setWorkingDir cwd $ setEnv env $ setStdin nullStream $ setStdout createPipe $ setStderr createPipe $ proc name args) \process -> do
                let capture handle = toStrict <$> runConduit (CC.sourceHandle handle .| CC.takeE (1048577 :: Int) .| CC.sinkLazy)
                (out, err) <- concurrently (capture $ getStdout process) (capture $ getStderr process)
                status <- waitExitCode process
                pure (status, out, err)
              case answer of
                Just (ExitSuccess, out, err) | BS.length out <= 1048576 && BS.length err <= 1048576 -> pure out
                _ -> throwE failure
            ghEnv = environment <> [("GH_CONFIG_DIR", temporary </> "gh"), ("GH_TOKEN", toString token), ("GH_HOST", "github.com"), ("GH_PROMPT_DISABLED", "1"), ("GH_NO_UPDATE_NOTIFIER", "1")]
            gh = command GitHubFailure temporary ghEnv executable
            gitEnv = environment <> [("LC_ALL", "C"), ("GIT_CONFIG_NOSYSTEM", "1"), ("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_TERMINAL_PROMPT", "0"), ("GIT_ALLOW_PROTOCOL", "https")]
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
              ensure PullRequestChanged $ pr.number > 0 && pr.head.sha == revisionText result.head && pr.head.ref == context.branch && pr.base.ref == context.targetBranch && all ((== T.toCaseFold context.repo) . T.toCaseFold) [pr.head.repo.fullName, pr.base.repo.fullName] && T.toCaseFold pr.htmlUrl == T.toCaseFold ("https://github.com/" <> context.repo <> "/pull/" <> show pr.number) && marker `elem` lines (normalized $ fromMaybe "" pr.body)
              pure pr
            fetch number = api "GET" (endpoint <> "/pulls/" <> show number) Nothing >>= checked
            marker = "<!-- monoscope-fix project=" <> result.projectId.toText <> " run=" <> result.runId.toText <> " candidate=" <> result.candidateId.toText <> " source=" <> explanation.source <> " -->"
            mediaMarker asset = "<!-- monoscope-fix-media:" <> key asset <> ":" <> fromMaybe "" (Map.lookup (file asset) explanation.files) <> " -->"
            observed :: Git.PublicationPullRequest -> ExceptT Failure IO (Map Asset Text)
            observed pr =
              Map.fromList . catMaybes <$> forM [Video, Diagram] \asset -> do
                let rest = [drop (index + 1) rows | let rows = lines (normalized $ fromMaybe "" pr.body), (index, line) <- zip [0 ..] rows, line == mediaMarker asset]
                ensure PullRequestChanged $ length rest <= 1
                pure $ do
                  following <- viaNonEmpty Relude.head rest
                  line <- find (not . T.null . T.strip) following
                  url <- if asset == Video then Just line else T.stripPrefix "![Reproduction, inferred cause, and verified fix](" line >>= T.stripSuffix ")"
                  guard $ assetUrl url
                  pure (asset, url)
            body :: Map Asset Text -> Maybe Asset -> Text
            body urls uploading =
              context.description
                <> "\n\n"
                <> T.intercalate
                  "\n\n"
                  [ case Map.lookup asset urls of
                      Just url -> mediaMarker asset <> "\n" <> if asset == Video then url else "![Reproduction, inferred cause, and verified fix](" <> url <> ")"
                      Nothing | uploading == Just asset -> mediaMarker asset <> "\n" <> reference asset
                      _ -> "_" <> (if asset == Video then "Video" else "Diagram") <> " attachment pending._"
                  | asset <- [Video, Diagram]
                  ]
                <> "\n"
        void $ command InvalidRequest temporary gitEnv "git" ["check-ref-format", "refs/heads/" <> toString context.targetBranch]
        help <- decodeUtf8 @Text <$> gh ["pr", "edit", "--help"]
        ensure NativeUnavailable $ "--attach" `elem` words help
        for_ [Video, Diagram] \asset -> do
          bytes <- ExceptT $ first (const SourceChanged) <$> readExplanationMedia store.usageDir explanation (file asset)
          liftIO $ writeFileBS (temporary </> toString (file asset)) bytes
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
                let settings :: [(Text, Text)]
                    settings = [("core.hooksPath", "/dev/null"), ("core.fsmonitor", "false"), ("credential.helper", ""), ("http.followRedirects", "false"), ("http.https://github.com/.extraheader", "AUTHORIZATION: basic " <> extractBase64 (Base64.encodeBase64 $ encodeUtf8 $ "x-access-token:" <> token))]
                    pushEnv = gitEnv <> [("GIT_CONFIG_COUNT", show $ length settings)] <> concat [[("GIT_CONFIG_KEY_" <> show index, toString k), ("GIT_CONFIG_VALUE_" <> show index, toString v)] | (index, (k, v)) <- zip [0 :: Int ..] settings]
                void $ command PushFailed checkout pushEnv "git" ["push", "--porcelain", "--force-with-lease=refs/heads/" <> toString context.branch <> ":", toString artifacts.repository, toString (revisionText result.head) <> ":refs/heads/" <> toString context.branch]
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
                      ensure PullRequestChanged $ remote.state == Open && remote.draft && normalized (fromMaybe "" remote.body) == normalized (body urls Nothing)
                      guardRemote $ if asset == Video then AttachVideo else AttachDiagram
                      save $ Uploading remote.number asset
                      liftIO $ writeFileBS (temporary </> "description.md") $ encodeUtf8 $ body urls (Just asset)
                      void $ gh ["pr", "edit", show remote.number, "--repo", toString context.repo, "--body-file", temporary </> "description.md", "--attach", temporary </> toString (file asset)]
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
          ensure InvalidReceipt $ normalized (fromMaybe "" final.body) == normalized (body urls Nothing) && video /= diagram
          save $ Finished final.number
          pure $ Receipt final.number final.htmlUrl result.head explanation.source context.mediaSha256 (MediaUrls video diagram) final.draft final.state
        hoistEither $ fromRight (Left SourceChanged) verified
  pure $ case outcome of Left _ -> Left GitHubFailure; Right Nothing -> Left DeadlineExceeded; Right (Just result') -> result'
  where
    result = artifacts.result
    permit stage = ExceptT $ authorize stage
    ensure failure = flip unless $ throwE failure
    normalized = T.strip . T.replace "\r\n" "\n"
    key = \case Video -> "video"; Diagram -> "diagram"
    file = \case Video -> "explanation.mp4"; Diagram -> "cause.png"
    reference = \case Video -> "![](explanation.mp4)"; Diagram -> "![Reproduction, inferred cause, and verified fix](cause.png)"
    validToken value = T.length value == 40 && T.all isHexDigit value || any (\prefix -> maybe False (\rest -> not (T.null rest) && T.all (\c -> isAscii c && (isAlphaNum c || c == '_')) rest) $ T.stripPrefix prefix value) ["ghp_", "gho_", "github_pat_"]
    assetUrl url = maybe False (isJust . UUID.fromText) $ T.stripPrefix "https://github.com/user-attachments/assets/" url


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


sync :: FilePath -> IO ()
sync path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.cloexec = True}) Posix.closeFd fileSynchronise
