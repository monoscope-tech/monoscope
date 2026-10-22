module Data.Effectful.CodeReview.Checkout (Source (..), Prepared (..), withCheckout) where

import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Char (isAlphaNum, isAscii)
import Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), ReviewEvidence, artifactCheckout, reviewEvidence, sha256, withVerifiedArtifacts)
import Data.Effectful.CodeReview.Policy qualified as Policy
import Data.Effectful.CodeReview.Types (ArtifactRequest (..), FixResult (..), Request (..), Revision, mkRevision, revisionText)
import Data.List (lookup)
import Data.List.Extra (nubOrd)
import Data.Text qualified as T
import GHC.Clock (getMonotonicTimeNSec)
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Relude
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (isAbsolute, takeDirectory, takeFileName, (</>))
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files qualified as Posix
import System.Process.Typed (ExitCode (..), createPipe, getStdout, nullStream, proc, readProcessStdout_, setEnv, setStderr, setStdout, setWorkingDir, waitExitCode, withProcessTerm)
import UnliftIO.Exception (throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout


data Source where
  Repository :: Request run -> Source
  VerifiedCandidate :: ArtifactRequest -> Source


data CacheStatus = Hit | Miss | Verified
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" CacheStatus


data CheckoutEvidence = SourceOnly | VerifiedEvidence ReviewEvidence
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data Prepared = Prepared {directory :: FilePath, repositorySkills :: FilePath, rootInstructions :: Text, projectId :: UUIDId "project", repository :: Text, base :: Revision, head :: Revision, cacheStatus :: CacheStatus, durationMs :: Word64, evidence :: CheckoutEvidence, policy :: Policy.Policy}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data Origin = GitHub Text | Local FilePath


-- Keep the private checkout alive until the child and all of its VMs have exited.
withCheckout :: ArtifactStore -> Source -> (Prepared -> IO a) -> IO (Either Text a)
withCheckout store source action = do
  outcome <- tryAny $ case source of
    Repository request -> withSystemTempDirectory "monoscope-agent-checkout" \temporary -> do
      started <- getMonotonicTimeNSec
      prepared <- Timeout.timeout (120 * 1000000) $ prepare temporary request
      traverse
        (\(directory, baseRevision, headRevision, cacheStatus) -> deliver started directory request.projectId request.repository baseRevision headRevision cacheStatus SourceOnly)
        prepared
    VerifiedCandidate artifacts -> do
      started <- getMonotonicTimeNSec
      verified <- withVerifiedArtifacts store artifacts \verifiedArtifacts -> do
        let directory = artifactCheckout verifiedArtifacts
            result = artifacts.result
        git directory ["checkout", "--quiet", "--detach", toString $ revisionText result.head] []
        deliver started directory result.projectId artifacts.repository result.base result.head Verified $ VerifiedEvidence $ reviewEvidence verifiedArtifacts
      either (const invalid) (pure . Just) verified
  pure $ case outcome of Right (Just prepared) -> Right prepared; _ -> Left "Repository checkout could not be prepared"
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
              when (BS.length bytes < 32768) $ waitExitCode process >>= \exit -> unless (exit == ExitSuccess) invalid
              pure $ decodeUtf8With lenientDecode bytes
      for_ files \(path, blob) -> when (any (\root -> (root <> "/") `T.isPrefixOf` path) roots && (".md" `T.isSuffixOf` path || takeFileName (toString path) `elem` [".gitignore", ".ignore", ".fdignore"])) do
        let destination = metadata </> toString path
        createDirectoryIfMissing True $ takeDirectory destination
        writeFileBS destination =<< content blob
      instructions <- maybe (pure "") instruction $ asum $ map (`lookup` files) instructionNames
      duration <- elapsed started
      action $ Prepared directory metadata instructions project repository baseRevision headRevision status duration evidence (Policy.policy Policy.Advisory (revisionText baseRevision) (revisionText headRevision) instructions)
    prepare :: FilePath -> Request run -> IO (FilePath, Revision, Revision, CacheStatus)
    prepare temporary request = do
      baseRevision <- either (const invalid) pure $ mkRevision request.base
      headRevision <- either (const invalid) pure $ mkRevision request.head
      origin <- parseOrigin request.repository
      let protocol = case origin of Local _ -> "file"; GitHub _ -> "https"
          sourcePath = case origin of Local path -> path; GitHub url -> toString url
          settings :: [(Text, Text)]
          settings = [("credential.helper", ""), ("http.followRedirects", "false")] <> [("http.https://github.com/.extraheader", "AUTHORIZATION: basic " <> extractBase64 (Base64.encodeBase64 $ encodeUtf8 $ "x-access-token:" <> request.gitToken)) | not $ T.null request.gitToken]
          credentials = [("GIT_CONFIG_COUNT", show $ length settings), ("GIT_ALLOW_PROTOCOL", protocol)] <> concat [[("GIT_CONFIG_KEY_" <> show index, toString key), ("GIT_CONFIG_VALUE_" <> show index, toString value)] | (index, (key, value)) <- zip [0 :: Int ..] settings]
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
    parseOrigin value
      | isAbsolute $ toString value = canonical (toString value) $> Local (toString value)
      | otherwise = case T.stripPrefix "https://github.com/" value >>= T.stripSuffix ".git" of
          Just slug | [owner, name] <- T.splitOn "/" slug, all segment [owner, name] -> pure $ GitHub value
          _ -> invalid
      where
        segment part = not (T.null part) && part `notElem` [".", ".."] && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.'])) part
    canonical directory = canonicalizePath directory >>= \resolved -> unless (resolved == directory) invalid
    git directory args extra = void . readProcessStdout_ =<< gitCommand directory args extra
    gitBytes directory args extra = do
      command <- gitCommand directory args extra
      withProcessTerm (setStdout createPipe command) \process -> do
        bytes <- BS.hGet (getStdout process) (1048576 + 1)
        when (BS.length bytes > 1048576) invalid
        exit <- waitExitCode process
        unless (exit == ExitSuccess) invalid
        pure bytes
    gitCommand directory args extra = do
      environment <- filter ((== "PATH") . fst) <$> getEnvironment
      pure (setWorkingDir directory $ setStderr nullStream $ setEnv (environment <> [("LC_ALL", "C"), ("GIT_CONFIG_NOSYSTEM", "1"), ("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_TERMINAL_PROMPT", "0")] <> (if null extra then [("GIT_ALLOW_PROTOCOL", "file")] else extra)) $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false"] <> args)
    invalid = throwIO $ userError "Invalid repository checkout"
