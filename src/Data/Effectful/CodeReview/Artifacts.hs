module Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), verifyArtifacts, withVerifiedArtifacts, readPrivateArtifact, sha256) where

import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Char (isHexDigit)
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Pkg.DeriveUtils (UUIDId, WrappedEnumSC (..))
import Relude
import System.Directory (canonicalizePath)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Process.Typed (nullStream, proc, readProcessStdout_, setEnv, setStderr, setWorkingDir)
import UnliftIO.Exception (bracket, bracketOnError, throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


data ArtifactStore = ArtifactStore {usageDir :: FilePath, cacheDir :: FilePath}


data ReportStatus = Completed
  deriving stock (Generic, Read, Show)
  deriving (AE.FromJSON) via WrappedEnumSC 'Nothing "" ReportStatus


data Placement = Injected | Retained
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON) via WrappedEnumSC 'Nothing "" Placement


data Resources = Resources {cpus :: Word16, memoryMiB :: Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Execution = Execution
  { id :: UUIDId "check_execution"
  , side :: RevisionSide
  , revision :: Revision
  , status :: CheckStatus
  , exitCode :: Maybe Int
  , configuredResources :: Resources
  , durationMs :: Scientific
  , logBytes :: Word32
  , logSha256 :: Text
  , sourceBefore :: Text
  , sourceAfter :: Text
  , testFilesSha256 :: Text
  , testPatchDisposition :: Placement
  }
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Report = Report
  { id :: UUIDId "regression_check"
  , projectId :: UUIDId "project"
  , runId :: UUIDId "issue_fix"
  , attemptId :: UUIDId "agent_attempt"
  , imageBuildId :: Text
  , workspace :: Maybe WorkspaceFingerprint
  , scriptSha256 :: Text
  , testPatchSha256 :: Text
  , regressionSha256 :: Text
  , failureMessage :: Text
  , status :: ReportStatus
  , results :: (Execution, Execution)
  }
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


verifyArtifacts :: ArtifactStore -> ArtifactRequest -> IO (Either Text ())
verifyArtifacts store request = withVerifiedArtifacts store request $ const pass


withVerifiedArtifacts :: ArtifactStore -> ArtifactRequest -> (FilePath -> IO a) -> IO (Either Text a)
withVerifiedArtifacts store request action = do
  outcome <- tryAny $ withSystemTempDirectory "monoscope-native-artifacts" \directory -> do
    verified <- Timeout.timeout (180 * 1000000) $ verify directory
    traverse action verified
  pure $ case outcome of Right (Just verified) -> Right verified; _ -> Left "Retained fix artifacts do not match the verified candidate"
  where
    result = request.result
    baseline = request.baseline
    candidate = request.candidate
    ensure condition = unless condition $ throwIO $ userError "Invalid retained fix evidence"
    digest value = T.length value == 64 && T.all (\c -> isHexDigit c && not (c >= 'A' && c <= 'F')) value
    canonical directory = canonicalizePath directory >>= ensure . (== directory)
    decode :: forall value. AE.FromJSON value => BS.ByteString -> IO value
    decode bytes = either (const $ throwIO $ userError "Invalid retained JSON") pure $ AE.eitherDecodeStrict bytes
    placement = \case Just InjectedTest{} -> Just Injected; Just RetainedTest{} -> Just Retained; _ -> Nothing
    regressionHash = \case Just (InjectedTest value) -> Just value; Just (RetainedTest value) -> Just value; _ -> Nothing
    verify directory = do
      ensure $ not (T.null request.repository) && not (T.null $ T.strip result.rootCause) && T.length result.rootCause <= 1200 && not (T.null $ T.strip result.summary) && T.length result.summary <= 600
      ensure $ baseline.side == Base && baseline.status == Failed && maybe False (\code -> code > 0 && code `notElem` [126, 127]) baseline.exitCode
      ensure $ candidate.side == Head && candidate.status == Passed && candidate.exitCode == Just 0 && placement candidate.regression == Just Retained
      ensure $ baseline.revision == revisionText result.base && candidate.revision == revisionText result.head && result.base /= result.head && baseline.id /= candidate.id
      ensure $ (baseline.imageBuildId, baseline.workspace, baseline.scriptSha256, baseline.cpus, baseline.memoryMiB) == (candidate.imageBuildId, candidate.workspace, candidate.scriptSha256, candidate.cpus, candidate.memoryMiB)
      ensure $ isJust $ UUID.fromText baseline.imageBuildId
      root <- canonicalizePath store.usageDir
      let checks = root </> "fixes" </> toString result.runId.toText </> "checks"
          captured = checks </> "candidates" </> toString result.candidateId.toText
          execution = checks </> toString result.checkId.toText
          expectedMetadata = case AE.toJSON result of AE.Object fields -> AE.Object $ foldr KM.delete fields ["checkId", "regressionSha256", "summary"]; value -> value
      metadata <- decode =<< readPrivateArtifact captured "candidate.json" 65536
      ensure $ metadata == expectedMetadata
      patch <- readPrivateArtifact captured "fix.patch" 1048576
      bundle <- readPrivateArtifact captured "candidate.bundle" (64 * 1048576)
      ensure $ sha256 patch == result.patchSha256 && sha256 bundle == result.bundleSha256
      report <- decode @Report =<< readPrivateArtifact execution "report.json" 65536
      ensure $ (report.id, report.projectId, report.runId, report.attemptId) == (result.checkId, result.projectId, result.runId, result.attemptId)
      ensure $ (report.imageBuildId, report.workspace, report.scriptSha256, report.regressionSha256) == (baseline.imageBuildId, baseline.workspace, baseline.scriptSha256, result.regressionSha256)
      case report.status of Completed -> pass
      script <- readPrivateArtifact execution "script.sh" 262144
      testPatch <- readPrivateArtifact execution "test.patch" 1048576
      let frozen = toStrict $ Encoding.encodingToLazyByteString $ Encoding.pairs ("script" AE..= (decodeUtf8 script :: Text) <> "testPatch" AE..= (decodeUtf8 testPatch :: Text) <> "failureMessage" AE..= report.failureMessage)
      ensure $ sha256 script == baseline.scriptSha256 && sha256 testPatch == report.testPatchSha256 && not (T.null report.failureMessage) && sha256 frozen == result.regressionSha256
      let (baseRow, headRow) = report.results
      ensure $ digest baseRow.testFilesSha256 && baseRow.testFilesSha256 == headRow.testFilesSha256
      for_ [(baseRow, baseline), (headRow, candidate)] \((row, receipt) :: (Execution, CheckReceipt)) -> do
        ensure $ receipt.attemptId == result.attemptId && receipt.checkId == result.checkId && regressionHash receipt.regression == Just result.regressionSha256
        ensure $ receipt.cpus > 0 && receipt.memoryMiB > 0 && receipt.logBytes <= 1048576
        ensure $ (row.id, row.side, revisionText row.revision, row.status, row.exitCode, row.logBytes, row.logSha256) == (receipt.id, receipt.side, receipt.revision, receipt.status, receipt.exitCode, receipt.logBytes, receipt.logSha256)
        ensure $ row.durationMs >= 0 && ceiling @Scientific @Integer row.durationMs == toInteger receipt.durationMs
        ensure $ (row.configuredResources.cpus, row.configuredResources.memoryMiB) == (receipt.cpus, receipt.memoryMiB)
        ensure $ digest row.sourceBefore && row.sourceBefore == row.sourceAfter && Just row.testPatchDisposition == placement receipt.regression
        output <- readPrivateArtifact execution (if receipt.side == Base then "base.log" else "head.log") 1048576
        ensure $ BS.length output == fromIntegral receipt.logBytes && sha256 output == receipt.logSha256 && (receipt.side /= Base || encodeUtf8 report.failureMessage `BS.isInfixOf` output)
      let (header, boundary) = BS.breakSubstring "\n\n" bundle
          headerLines = lines (decodeUtf8 header :: Text)
          signature = viaNonEmpty Relude.head headerLines
          prerequisites = filter (T.isPrefixOf "-") $ drop 1 headerLines
          refs = filter (\line -> not (T.isPrefixOf "-" line || T.isPrefixOf "@" line)) $ drop 1 headerLines
          capabilities = filter (T.isPrefixOf "@") $ drop 1 headerLines
      ensure $ not (BS.null boundary) && BS.length header > 0 && BS.length header < 65536 && (signature == Just "# v2 git bundle" || signature == Just "# v3 git bundle")
      ensure $ map (T.takeWhile (/= ' ') . T.drop 1) prerequisites == [revisionText result.base] && refs == [revisionText result.head <> " refs/heads/monoscope-candidate/" <> result.candidateId.toText]
      ensure $ all (\capability -> signature == Just "# v3 git bundle" && capability == "@object-format=" <> bool "sha256" "sha1" (T.length (revisionText result.head) == 40)) capabilities
      cache <- canonicalizePath store.cacheDir
      let mirror = cache </> toString result.projectId.toText </> toString (sha256 $ encodeUtf8 request.repository)
      canonical mirror
      environment <- filter ((== "PATH") . fst) <$> getEnvironment
      let checkout = directory </> "checkout"
          git cwd args = T.strip . decodeUtf8 <$> readProcessStdout_ (setStderr nullStream $ setEnv (environment <> [("LC_ALL", "C"), ("GIT_CONFIG_NOSYSTEM", "1"), ("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_ALLOW_PROTOCOL", "file")]) $ setWorkingDir cwd $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false"] <> args)
      -- Git transport avoids copying a mirror's object files while another worker fetches.
      void $ git directory ["clone", "--quiet", "--no-local", "--no-checkout", "--", mirror, checkout]
      writeFileBS (directory </> "candidate.bundle") bundle
      writeFileBS (directory </> "fix.patch") patch
      void $ git checkout ["bundle", "verify", directory </> "candidate.bundle"]
      void $ git checkout ["bundle", "unbundle", directory </> "candidate.bundle"]
      git checkout ["rev-list", "--parents", "-n", "1", toString $ revisionText result.head] >>= ensure . (== revisionText result.head <> " " <> revisionText result.base)
      void $ git checkout ["checkout", "--quiet", "--detach", toString $ revisionText result.base]
      void $ git checkout ["apply", "--index", directory </> "fix.patch"]
      tree <- git checkout ["write-tree"]
      git checkout ["rev-parse", toString $ revisionText result.head <> "^{tree}"] >>= ensure . (== tree)
      pure checkout


sha256 :: ByteString -> Text
sha256 bytes = show (hash bytes :: Digest SHA256)


readPrivateArtifact :: FilePath -> FilePath -> Int -> IO ByteString
readPrivateArtifact directory name limit = do
  canonicalizePath directory >>= ensure . (== directory)
  bracket
    ( bracketOnError (Posix.openFd (directory </> name) Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.nonBlock = True, Posix.cloexec = True}) Posix.closeFd \fd -> do
        status <- Posix.getFdStatus fd
        ensure $ Posix.isRegularFile status && Posix.fileSize status <= fromIntegral limit
        (,Posix.fileSize status) <$> Posix.fdToHandle fd
    )
    (hClose . fst)
    \(handle, size) -> do
      bytes <- BS.hGet handle (limit + 1)
      ensure $ toInteger (BS.length bytes) == toInteger size
      pure bytes
  where
    ensure condition = unless condition $ throwIO $ userError "Invalid private artifact"
