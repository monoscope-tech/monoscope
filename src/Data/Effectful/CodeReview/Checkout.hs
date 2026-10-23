module Data.Effectful.CodeReview.Checkout (Source (..), Prepared (..), Envelope (..), withCheckout) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Effectful.AgentWorkspace (captureProcess, gitCredentials, gitProcess, mkRepository, repositoryText, pattern GitHub, pattern Local)
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), artifactCheckout, peerEvidence, sha256, withVerifiedArtifacts)
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Types (ArtifactRequest (..), FixResult (..), Request (..), Revision, TestArtifact, mkRevision, revisionText)
import Data.List (lookup)
import Data.List.Extra (nubOrd)
import Data.Text qualified as T
import Deriving.Aeson qualified as DAE
import GHC.Clock (getMonotonicTimeNSec)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files qualified as Posix
import System.Process.Typed (ExitCode (..), createPipe, getStdout, readProcessStdout_, setStdout, waitExitCode, withProcessTerm)
import UnliftIO.Exception (throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout


data Source where
  Repository :: Request run -> Source
  VerifiedCandidate :: ArtifactRequest -> Source


data CacheStatus = Hit | Miss | Verified
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" CacheStatus


-- | Host-bound fields the agent wraps around its decision, encoded as @{"artifact": …}@ or @{"result": …}@.
data Envelope = Artifact TestArtifact | Result FixResult
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.SumObjectWithSingleField, DAE.ConstructorTagModifier DAE.CamelToSnake] Envelope


-- | @guest@ names the host skill directory of a host-owned VM.
data Prepared = Prepared {directory :: FilePath, repositorySkills :: FilePath, rootInstructions :: Text, projectId :: UUIDId "project", repository :: Text, base :: Revision, head :: Revision, cacheStatus :: CacheStatus, durationMs :: Word64, policy :: Policy.Policy, guest :: Maybe FilePath, envelope :: Maybe Envelope}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


-- Keep the private checkout alive until the child and all of its VMs have exited.

-- | The action also receives the recorded evidence to place read-only in the guest (empty for source checkouts).
-- Only preparation failures become @Left@; the action's own exceptions propagate.
withCheckout :: ArtifactStore -> Source -> (Prepared -> Map Text ByteString -> IO a) -> IO (Either Text a)
withCheckout store source action = do
  outcome <- tryAny $ case source of
    Repository request -> withSystemTempDirectory "monoscope-agent-checkout" \temporary -> do
      started <- getMonotonicTimeNSec
      prepared <- Timeout.timeout (120 * 1000000) $ prepare temporary request
      traverse
        (\(directory, baseRevision, headRevision, cacheStatus) -> deliver started directory request.projectId request.repository baseRevision headRevision cacheStatus mempty)
        prepared
    VerifiedCandidate artifacts -> do
      started <- getMonotonicTimeNSec
      verified <- withVerifiedArtifacts store artifacts \verifiedArtifacts -> do
        let directory = artifactCheckout verifiedArtifacts
            result = artifacts.result
        git directory ["checkout", "--quiet", "--detach", toString $ revisionText result.head] []
        deliver started directory result.projectId artifacts.repository result.base result.head Verified $ peerEvidence verifiedArtifacts
      either (const invalid) (pure . Just) verified
  case outcome of
    Right (Just completed) -> Right <$> either throwIO pure completed
    _ -> pure $ Left "Repository checkout could not be prepared"
  where
    elapsed started = max 1 . (`div` 1000000) . (+ 999999) . subtract started <$> getMonotonicTimeNSec
    deliver started directory project repository baseRevision headRevision status evidence = withSystemTempDirectory "monoscope-agent-metadata" \temporary -> do
      metadata <- canonicalizePath temporary
      let roots = [".agents/skills", ".pi/skills"]
          instructionNames = ["AGENTS.override.md", "AGENTS.md", "CLAUDE.md"]
      for_ roots $ createDirectoryIfMissing True . (metadata </>) . toString
      entries <- T.splitOn "\0" . decodeUtf8 <$> gitBytes directory (["ls-tree", "-rz", toString $ revisionText headRevision, "--"] <> map toString (roots <> instructionNames)) []
      let files =
            mapMaybe
              ( \entry -> case T.breakOn "\t" entry of
                  (header, path) | [mode, "blob", blob] <- words header, mode `elem` ["100644", "100755"] -> Just (T.drop 1 path, blob)
                  _ -> Nothing
              )
              entries
          content blob = gitBytes directory ["cat-file", "blob", toString blob] []
          instruction blob = do
            command <- gitCommand directory ["cat-file", "blob", toString blob] []
            withProcessTerm (setStdout createPipe command) \process -> do
              bytes <- BS.hGet (getStdout process) 32768
              -- Leaving the bracket with git still writing races typed-process's waiter (ECHILD).
              let drain = BS.hGetSome (getStdout process) 65536 >>= \chunk -> unless (BS.null chunk) drain
              drain
              waitExitCode process >>= \exit -> unless (exit == ExitSuccess) invalid
              pure $ decodeUtf8With lenientDecode bytes
      for_ files \(path, blob) -> when (any (\root -> (root <> "/") `T.isPrefixOf` path) roots && (".md" `T.isSuffixOf` path || takeFileName (toString path) `elem` [".gitignore", ".ignore", ".fdignore"])) do
        let destination = metadata </> toString path
        createDirectoryIfMissing True $ takeDirectory destination
        writeFileBS destination =<< content blob
      instructions <- maybe (pure "") instruction $ asum $ map (`lookup` files) instructionNames
      duration <- elapsed started
      tryAny $ flip action evidence $ Prepared directory metadata instructions project repository baseRevision headRevision status duration (Policy.policy Policy.Advisory (revisionText baseRevision) (revisionText headRevision) instructions) Nothing Nothing
    prepare :: FilePath -> Request run -> IO (FilePath, Revision, Revision, CacheStatus)
    prepare temporary request = do
      baseRevision <- either (const invalid) pure $ mkRevision request.base
      headRevision <- either (const invalid) pure $ mkRevision request.head
      origin <- either (const invalid) pure $ mkRepository request.repository
      for_ (case origin of Local path -> Just path; GitHub _ -> Nothing) canonical
      let protocol = case origin of Local _ -> "file"; GitHub _ -> "https"
          sourcePath = case origin of Local path -> path; GitHub _ -> toString $ repositoryText origin
          credentials = ("GIT_ALLOW_PROTOCOL", protocol) : gitCredentials request.gitToken
      root <- canonicalizePath store.cacheDir
      let project = root </> toString request.projectId.toText
          mirror = project </> toString (sha256 $ encodeUtf8 request.repository)
          checkout = temporary </> "checkout"
      for_ [project, mirror] \directory -> do
        createDirectoryIfMissing True directory
        canonical directory
        Posix.setFileMode directory 0o700
      status <- withFileLock (mirror <> ".native.lock") Exclusive \_ -> do
        let cached args = git temporary (["--git-dir", mirror] <> args) credentials
        cached ["init", "--bare", "--quiet"]
        available <- tryAny $ traverse_ (\revision -> cached ["cat-file", "-e", toString (revisionText revision) <> "^{commit}"]) [baseRevision, headRevision]
        -- A cache hit is not proof that this request still has source access.
        cached ["ls-remote", "--quiet", "--", sourcePath, "HEAD"]
        cached $ ["fetch", "--quiet", "--no-tags", "--", sourcePath] <> ["+" <> revision <> ":refs/heads/monoscope/" <> revision | revision <- nubOrd $ map (toString . revisionText) [baseRevision, headRevision]]
        git temporary ["clone", "--quiet", "--no-local", "--no-checkout", "--", mirror, checkout] []
        pure $ if isRight available then Hit else Miss
      git checkout ["remote", "set-url", "origin", toString request.repository] []
      git checkout ["checkout", "--quiet", "--detach", toString $ revisionText headRevision] []
      pure (checkout, baseRevision, headRevision, status)
    canonical directory = canonicalizePath directory >>= \resolved -> unless (resolved == directory) invalid
    git directory args extra = void . readProcessStdout_ =<< gitCommand directory args extra
    gitBytes directory args extra = do
      command <- gitCommand directory args extra
      (exit, bytes, _) <- captureProcess 1048576 command
      unless (exit == ExitSuccess) invalid
      pure bytes
    gitCommand directory args extra = gitProcess (fromMaybe "file" $ lookup "GIT_ALLOW_PROTOCOL" extra) directory args (filter ((/= "GIT_ALLOW_PROTOCOL") . fst) extra)
    invalid = throwIO $ userError "Invalid repository checkout"
