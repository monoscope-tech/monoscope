{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoOverloadedLists #-}

module Data.Effectful.AgentWorkspace (Workspace, Options (..), PruneOptions (..), PruneReport, pruneBudgetMet, pruneWorkspaces, Operation (..), Prepared, Leased (..), Request, mkRequest, Baseline, leaseWorkspace, leasedBaseline, RegressionOptions (..), RegressionRequest, mkRegressionRequest, RegressionResult (..), RegressionReceipt, RegressionReport (..), ReportState (..), CheckExecution (..), ExecutionKey (..), Measurement (..), pinnedRevisionText, Resources (..), Placement (..), RegressionError (..), verifyRegression, regressionLogs, regressionErrorText, runWorkspace, withWorkspace, runRegression, runWorkspaceReal, runWorkspaceTest, RPC, withSandbox, withSandboxRelease, Command (..), Sandbox (..), Isolation (..), GuestCommand (..), Copy (..), LoggedCommand (..), Overflow (..), Execution (..), LogExit (..), Image (..), ImageId (..), SandboxFailure (..), agentCommand, guestGit, checkoutGuest, readBounded, publish, publishBytes, sync, gitProcess, gitEnvironment, gitCredentials, captureProcess, Repository, pattern Local, pattern GitHub, mkRepository, repositoryText, withOwner, Revision, mkRevision, revisionText, RevisionSide (..), CheckStatus (..), Regression (..), CheckReceipt (..), WorkspaceFingerprint, mkWorkspaceFingerprint, workspaceFingerprintText, digestFingerprint, AllocationReceipt, AllocationOutcome (..), allocationId, allocationAttempt, agentResources, hostAllocation, TestArtifact (..)) where

import Conduit (runConduit, (.|))
import Control.Exception qualified as Exception
import Control.Monad (foldM)
import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH qualified as ATH
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isAscii, isHexDigit)
import Data.Conduit.Combinators qualified as CC
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Deriving.Aeson qualified as DAE
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client.TLS qualified as TLS
import Network.Minio qualified as Minio
import Network.URI (URI (..), URIAuth (..), parseURI)
import Pkg.Deriving (UUIDId (..), WrappedEnumSC (..), decodeEnumSC)
import Relude hiding (Identity, identity, withFile)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, removePathForcibly, renameDirectory, renameFile)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hClose, withBinaryFile)
import System.IO.Error (isDoesNotExistError, tryIOError, userError)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory, withTempDirectory, withTempFile)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Process (getParentProcessID)
import System.Posix.Unistd (fileSynchronise)
import System.Process.Typed (ExitCode (..), ProcessConfig, createPipe, getStderr, getStdin, getStdout, nullStream, proc, readProcessStdout_, setEnv, setStderr, setStdin, setStdout, setWorkingDir, waitExitCode, withProcessTerm)
import UnliftIO.Async (concurrently, withAsync)
import UnliftIO.Concurrent (myThreadId, threadDelay)
import UnliftIO.Exception (bracket, bracketOnError, catchAny, evaluate, onException, throwIO, throwTo, tryAny)
import UnliftIO.Timeout qualified as Timeout
import "base64" Data.ByteString.Base64 qualified as Base64
import "cryptonite" Crypto.Hash (Context, Digest, SHA256, hash, hashFinalize, hashInit, hashUpdate, hashlazy)


data AllocationOutcome = Released | ReleaseFailed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" AllocationOutcome


data AllocationData = AllocationData {id :: UUIDId "agent_allocation", attemptId :: UUIDId "agent_attempt", durationMs :: Word64, cpus :: Word16, memoryMiB :: Word32, outcome :: AllocationOutcome}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | The private wire product preserves positive resources through JSON round-trips.
-- >>> let receipt cpus = AE.object ["id" AE..= ("00000000-0000-4000-8000-000000000001" :: Text), "attemptId" AE..= ("00000000-0000-4000-8000-000000000002" :: Text), "durationMs" AE..= (1 :: Int), "cpus" AE..= (cpus :: Int), "memoryMiB" AE..= (1024 :: Int), "outcome" AE..= ("released" :: Text)]
-- >>> let decoded = AE.fromJSON (receipt 2) :: AE.Result AllocationReceipt
-- >>> case decoded of AE.Success _ -> True; _ -> False
-- True
-- >>> (decoded >>= AE.fromJSON . AE.toJSON) == decoded
-- True
-- >>> case (AE.fromJSON (receipt 0) :: AE.Result AllocationReceipt) of AE.Error _ -> True; _ -> False
-- True
newtype AllocationReceipt = AllocationReceipt AllocationData
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON AllocationReceipt where
  parseJSON value = do
    receipt <- AE.parseJSON value
    unless (receipt.cpus > 0 && receipt.memoryMiB > 0) $ fail "Allocation resources must be positive"
    pure $ AllocationReceipt receipt


allocationId :: AllocationReceipt -> UUIDId "agent_allocation"
allocationId (AllocationReceipt receipt) = receipt.id


allocationAttempt :: AllocationReceipt -> UUIDId "agent_attempt"
allocationAttempt (AllocationReceipt receipt) = receipt.attemptId


-- | Every agent VM uses this shape, so host-measured receipts cannot report other resources.
agentResources :: (Word16, Word32)
agentResources = (2, 1024)


hostAllocation :: UUIDId "agent_allocation" -> UUIDId "agent_attempt" -> Word64 -> AllocationOutcome -> AllocationReceipt
hostAllocation identifier attempt durationMs = AllocationReceipt . uncurry (AllocationData identifier attempt durationMs) agentResources


data RevisionSide = Base | Head
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" RevisionSide


data CheckStatus = Passed | Failed | Blocked | Invalidated | TimedOut | OutputLimit | Cancelled | CleanupFailed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" CheckStatus


-- The digest binds command, test patch, and expected assertion; constructors record placement.
data Regression = ScriptCheck {sha256 :: Text} | InjectedTest {sha256 :: Text} | RetainedTest {sha256 :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- Execution metadata comes from the host verifier, without scripts or log bodies.
data CheckReceipt = CheckReceipt
  { id :: UUIDId "check_execution"
  , attemptId :: UUIDId "agent_attempt"
  , checkId :: UUIDId "regression_check"
  , side :: RevisionSide
  , revision :: Text
  , imageBuildId :: Text
  , scriptSha256 :: Text
  , regression :: Maybe Regression
  , status :: CheckStatus
  , exitCode :: Maybe Int
  , durationMs :: Word64
  , cpus :: Word16
  , memoryMiB :: Word32
  , logBytes :: Word32
  , logSha256 :: Text
  , workspace :: Maybe WorkspaceFingerprint
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Identifies the prepared disk; absence in a receipt means a pristine toolchain image.
-- >>> let fingerprint = mkWorkspaceFingerprint (T.replicate 64 "a")
-- >>> fmap (const True) fingerprint
-- Right True
-- >>> (fingerprint >>= first toText . AE.eitherDecode . AE.encode) == fingerprint
-- True
-- >>> mkWorkspaceFingerprint "invalid"
-- Left "Invalid workspace fingerprint"
newtype WorkspaceFingerprint = WorkspaceFingerprint Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


mkWorkspaceFingerprint :: Text -> Either Text WorkspaceFingerprint
mkWorkspaceFingerprint value
  | T.length value == 64 && T.all (\c -> c `elem` (['0' .. '9'] <> ['a' .. 'f'])) value = Right $ WorkspaceFingerprint value
  | otherwise = Left "Invalid workspace fingerprint"


workspaceFingerprintText :: WorkspaceFingerprint -> Text
workspaceFingerprintText (WorkspaceFingerprint value) = value


digestFingerprint :: Digest SHA256 -> WorkspaceFingerprint
digestFingerprint = WorkspaceFingerprint . show


instance AE.FromJSON WorkspaceFingerprint where
  parseJSON = AE.withText "Workspace fingerprint" $ either (fail . toString) pure . mkWorkspaceFingerprint


-- $setup
-- >>> import Data.Text qualified as T
-- >>> import Data.Aeson qualified as AE
-- >>> import Relude


newtype Revision = Revision Text
  deriving stock (Eq, Show)
  deriving newtype (AE.ToJSON)


-- | Both the valid round-trip and rejection belong to this boundary.
--
-- >>> fmap revisionText (mkRevision (T.replicate 40 "A")) == Right (T.replicate 40 "a")
-- True
-- >>> mkRevision "branch-name"
-- Left "InvalidRevision"
mkRevision :: Text -> Either Text Revision
mkRevision text = if T.length text `elem` [40, 64] && T.all isHexDigit text then Right $ Revision $ T.toLower text else Left "InvalidRevision"


revisionText :: Revision -> Text
revisionText (Revision text) = text


instance AE.FromJSON Revision where
  parseJSON = AE.withText "Revision" $ either (fail . toString) pure . mkRevision


data TestArtifact = TestArtifact {script :: Text, testPatch :: Text, failureMessage :: Text}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


type ProjectId = UUIDId "project"


newtype ImageId = ImageId UUID
  deriving stock (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON)


-- The native boundary accepts only already-normalized revisions.
newtype PinnedRevision = PinnedRevision Revision
  deriving stock (Eq)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON PinnedRevision where
  parseJSON = AE.withText "Pinned revision" $ either (fail . toString) pure . mkPinnedRevision


mkPinnedRevision :: Text -> Either Text PinnedRevision
mkPinnedRevision value = case mkRevision value of
  Right revision | revisionText revision == value -> Right $ PinnedRevision revision
  _ -> Left "Invalid revision"


pinnedRevisionText :: PinnedRevision -> Text
pinnedRevisionText (PinnedRevision revision) = revisionText revision


data Repository = LocalRepository FilePath | GitHubRepository Text
  deriving stock (Eq)


pattern Local :: FilePath -> Repository
pattern Local path <- LocalRepository path


pattern GitHub :: Text -> Repository
pattern GitHub slug <- GitHubRepository slug


{-# COMPLETE Local, GitHub #-}


instance AE.ToJSON Repository where
  toJSON = AE.toJSON . repositoryText


repositoryText :: Repository -> Text
repositoryText = \case Local path -> toText path; GitHub slug -> "https://github.com/" <> slug <> ".git"


instance AE.FromJSON Repository where
  parseJSON = AE.withText "Credential-free repository" $ either (fail . toString) pure . mkRepository


-- | Keep local and GitHub origins credential-free and preserve their wire spelling.
-- >>> let origins = ["/tmp/repository", "https://github.com/org/repo.git"]
-- >>> traverse (fmap repositoryText . mkRepository) origins == Right origins
-- True
-- >>> all (isLeft . mkRepository) ["https://user:secret@github.com/org/repo.git", "https://github.com/../repo.git", "relative/path"]
-- True
-- >>> all (\origin -> either (const False) (\repo -> AE.fromJSON (AE.toJSON repo) == AE.Success repo) (mkRepository origin)) origins
-- True
mkRepository :: Text -> Either Text Repository
mkRepository value
  | isAbsolute $ toString value = Right $ LocalRepository $ toString value
  | Just slug <- T.stripPrefix "https://github.com/" value >>= T.stripSuffix ".git"
  , [owner, name] <- T.splitOn "/" slug
  , all (\part -> not (T.null part) && part `notElem` [".", ".."] && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.'])) part) [owner, name] =
      Right $ GitHubRepository slug
  | otherwise = Left "Invalid repository"


type Fingerprint = WorkspaceFingerprint


mkFingerprint :: Text -> Maybe Fingerprint
mkFingerprint = rightToMaybe . mkWorkspaceFingerprint


data Request = Request {projectId :: ProjectId, repository :: Repository, base :: PinnedRevision, expectedImageBuildId :: Maybe ImageId, expectedCheckpoint :: Maybe Fingerprint, ownerPid :: Maybe Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


mkRequest :: UUIDId "project" -> Text -> Text -> Maybe (UUIDId "guest_image") -> Either Text Request
mkRequest project repository base image = Request project <$> mkRepository repository <*> mkPinnedRevision base <*> pure (ImageId . (.unUUIDId) <$> image) <*> pure Nothing <*> pure Nothing


data Options = Options {cacheDir :: FilePath, image :: Text, bridge :: FilePath, script :: FilePath, dependencyArchive :: Maybe FilePath}


data Operation = Lookup | Prepare FilePath | Archive FilePath | Restore FilePath


data PruneOptions = PruneOptions {cacheDir :: FilePath, remote :: FilePath}


data PruneRequest = PruneRequest {projectId :: ProjectId, maxBytes :: Word64, ownerPid :: Maybe Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data RetainReason = Pinned | ArchiveUnavailable | InvalidEntry
  deriving stock (Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" RetainReason


data PruneAction = Evicted Fingerprint | Kept Fingerprint RetainReason | EvictionFailed Fingerprint
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data PruneReport = PruneReport {projectId :: ProjectId, beforeBytes :: Word64, afterBytes :: Word64, leaseBytes :: Word64, targetBytes :: Word64, entries :: Seq.Seq PruneAction, unrecognized :: [FilePath], resourceCostStatus :: ResourceCost}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


pruneBudgetMet :: PruneReport -> Bool
pruneBudgetMet report = report.afterBytes <= report.targetBytes


data PruneOutcome = PruneStarted | PruneRunning PruneReport | PruneFinished PruneReport Double
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data PruneRecord = PruneRecord {id :: UUIDId "workspace_prune", projectId :: ProjectId, startedAt :: UTCTime, outcome :: PruneOutcome}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data CacheEntry = CacheEntry {key :: Fingerprint, manifest :: Manifest, lastUsed :: UTCTime, status :: Posix.FileStatus}


data Remote = Remote {endpoint :: Text, bucket :: Text, region :: Text, accessKey :: Text, secretKey :: Text, maxBytes :: Word64}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data TransferDirection = Upload | Download
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" TransferDirection


data TransferOutcome = TransferStarted | TransferCompleted FileFingerprint Double | TransferFailed Double
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data TransferRecord = TransferRecord {id :: UUIDId "workspace_transfer", identity :: Identity, direction :: TransferDirection, startedAt :: UTCTime, outcome :: TransferOutcome, resourceCostStatus :: ResourceCost}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data FileFingerprint = FileFingerprint {sha256 :: Fingerprint, bytes :: Word64}
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] FileFingerprint


-- Declaration order preserves the existing JavaScript cache key encoding.
data Identity = Identity {projectId :: ProjectId, repository :: Repository, base :: PinnedRevision, imageBuildId :: ImageId, preparationSha256 :: Fingerprint, dependency :: Maybe FileFingerprint}
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] Identity


data ResourceCost = Unpriced
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" ResourceCost


data Manifest = Manifest {version :: Word8, identity :: Identity, assetDir :: FilePath, createdAt :: UTCTime, sha256 :: Fingerprint, durationMs :: Double, bytes :: Word64, cpus :: Word16, memoryMiB :: Word32, resourceCostStatus :: ResourceCost}
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] Manifest


instance AE.FromJSON Manifest where
  parseJSON value = do
    manifest <- AE.genericParseJSON AE.defaultOptions value
    unless (manifest.version == 1 && manifest.bytes > 0 && manifest.durationMs >= 0 && not (isInfinite manifest.durationMs || isNaN manifest.durationMs) && manifest.cpus == 2 && manifest.memoryMiB == 1024 && manifest.resourceCostStatus == Unpriced) $ fail "Invalid checkpoint manifest"
    pure manifest


data Prepared = Prepared {manifest :: Manifest, path :: FilePath}
  deriving stock (Generic)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] Prepared


data Leased = Leased {prepared :: Prepared, resumePath :: FilePath}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data Image = Image {assetDir :: FilePath, buildId :: ImageId}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Isolation = OfflineCow
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" Isolation


data Sandbox = Sandbox {imagePath :: FilePath, cpus :: Word16, memoryMiB :: Word32, isolation :: Isolation, mounts :: Map Text FilePath}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data GuestCommand = GuestCommand {argv :: NonEmpty Text, cwd :: Maybe Text, quiet :: Bool}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data Copy = Copy {source :: FilePath, destination :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


-- Verifier logs close their VM at the limit; agent sessions discard the excess and keep running.
data Overflow = CloseSandbox | DiscardOutput
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" Overflow


data LoggedCommand = LoggedCommand {command :: GuestCommand, path :: FilePath, limitBytes :: Word32, overflow :: Overflow}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


-- Runs as the unprivileged review account with a reset environment and fixed UTF-8 locale.
agentCommand :: [Text] -> NonEmpty Text
agentCommand command = "/usr/bin/setpriv" :| ["--reuid=1000", "--regid=1000", "--clear-groups", "--no-new-privs", "--reset-env", "--", "/usr/bin/env", "LANG=C.UTF-8", "LC_ALL=C.UTF-8"] <> command


guestGit :: [Text] -> NonEmpty Text
guestGit args = "/usr/bin/git" :| (["-c", "safe.directory=/workspace", "-c", "core.hooksPath=/dev/null", "-C", "/workspace"] <> args)


-- | Resumed workspaces retain build outputs while fetching the pinned revision's objects.
checkoutGuest :: (NonEmpty Text -> IO Text) -> Bool -> Text -> IO ()
checkoutGuest run resumed revision = do
  if resumed
    then do
      for_ ["/workspace", "/checkout/.git"] $ \path -> void $ run $ "/usr/bin/git" :| ["config", "--system", "--add", "safe.directory", path]
      void $ run $ guestGit ["fetch", "--quiet", "--no-tags", "--", "/checkout", revision]
    else void $ run $ "/bin/cp" :| ["-R", "/checkout/.", "/workspace"]
  void $ run $ guestGit ["checkout", "--quiet", "--detach", revision]


data Command result where
  Resolve :: Text -> Command Image
  Create :: Sandbox -> Command ()
  Execute :: GuestCommand -> Command Execution
  CopyFile :: Copy -> Command ()
  Checkpoint :: FilePath -> Command CheckpointData
  InspectCheckpoint :: FilePath -> Command CheckpointData
  Resume :: (Sandbox, FilePath) -> Command ()
  GetIdentity :: () -> Command (UUIDId "agent_allocation")
  ExecuteLogged :: LoggedCommand -> Command LogExit
  Close :: () -> Command ()


data Execution = Execution {exitCode :: Int, stdout :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data CheckpointData = CheckpointData {createdAt :: UTCTime, guestAssetBuildId :: ImageId}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data RegressionOptions = RegressionOptions {artifactDir :: FilePath, checkout :: FilePath, image :: Text, bridge :: FilePath}


data Artifact = Artifact {script :: Text, testPatch :: Maybe Text, failureMessage :: Text}
  deriving stock (Generic)


instance AE.FromJSON Artifact where
  parseJSON value = do
    artifact <- AE.genericParseJSON AE.defaultOptions value
    let bounded limit text = not (T.null $ T.strip text) && T.length text <= limit
    unless (bounded 65536 artifact.script && bounded 200 artifact.failureMessage && all (bounded 262144) artifact.testPatch) $ fail "Invalid regression artifact"
    pure artifact


data Snapshot = Snapshot {path :: FilePath, sha256 :: Fingerprint}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Baseline = Pristine | PreparedCheckpoint Snapshot
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Verifiers replay against the same immutable checkpoint the leased agent VM resumed from.
leasedBaseline :: Maybe Leased -> Baseline
leasedBaseline = maybe Pristine \leased -> PreparedCheckpoint $ Snapshot leased.prepared.path leased.prepared.manifest.sha256


data Revisions = BaseOnly PinnedRevision | HeadOnly PinnedRevision | Compare PinnedRevision PinnedRevision


instance AE.FromJSON Revisions where
  parseJSON value = AE.parseJSON value >>= either (fail . toString) pure . mkRevisions


mkRevisions :: [(RevisionSide, PinnedRevision)] -> Either Text Revisions
mkRevisions = \case
  [(Base, baseline)] -> Right $ BaseOnly baseline
  [(Head, candidate)] -> Right $ HeadOnly candidate
  [(Base, baseline), (Head, candidate)] -> Right $ Compare baseline candidate
  _ -> Left "Expected pinned baseline, candidate, or ordered baseline/candidate comparison"


data RegressionRequest = RegressionRequest {projectId :: ProjectId, runId :: UUIDId "run", attemptId :: UUIDId "agent_attempt", sessionId :: UUIDId "session", base :: PinnedRevision, imageBuildId :: ImageId, workspace :: Baseline, artifact :: Artifact, revisions :: Revisions, ownerPid :: Maybe Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


mkRegressionRequest :: UUIDId "project" -> UUIDId run -> UUIDId "agent_attempt" -> UUID -> Text -> ImageId -> Baseline -> AE.Value -> [(Text, Text)] -> Either Text RegressionRequest
mkRegressionRequest project run attempt session base image workspace artifact revisions = do
  frozen <- case AE.fromJSON artifact of AE.Success value -> Right value; AE.Error message -> Left $ toText message
  comparison <- mkRevisions =<< traverse (\(side, revision) -> (,) <$> maybeToRight "Invalid revision side" (decodeEnumSC @"" $ toString side) <*> mkPinnedRevision revision) revisions
  RegressionRequest project (UUIDId run.unUUIDId) attempt (UUIDId session) <$> mkPinnedRevision base <*> pure image <*> pure workspace <*> pure frozen <*> pure comparison <*> pure Nothing


data ReportState = Running | Completed | Incomplete
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" ReportState


data Placement = Injected | Retained
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" Placement


data LogExit = Exited Int | LogLimit
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON)


data Resources = Resources {cpus :: Word16, memoryMiB :: Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data ExecutionKey = ExecutionKey {id :: UUIDId "check_execution", side :: RevisionSide, revision :: PinnedRevision}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON ExecutionKey where
  parseJSON = AE.withObject "Execution key" \fields -> ExecutionKey <$> fields AE..: "id" <*> fields AE..: "side" <*> (PinnedRevision <$> fields AE..: "revision")


data Measurement duration = Measurement {status :: CheckStatus, exitCode :: Maybe Int, configuredResources :: Resources, resourceCostStatus :: ResourceCost, durationMs :: duration, logBytes :: Word32, logSha256 :: Fingerprint, tail :: Text, logPath :: Maybe Text, vmId :: Maybe Text, reason :: Maybe Text, sourceBefore :: Maybe Fingerprint, sourceAfter :: Maybe Fingerprint, testFilesSha256 :: Maybe Fingerprint, testPatchDisposition :: Maybe Placement}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data CheckExecution duration = Pending ExecutionKey | Measured ExecutionKey (Measurement duration)
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON duration => AE.FromJSON (CheckExecution duration) where
  parseJSON value =
    AE.withObject
      "Check execution"
      ( \fields ->
          if KM.member "tag" fields
            then AE.genericParseJSON AE.defaultOptions value
            else
              if KM.lookup "status" fields == Just (AE.String "starting")
                then Pending <$> AE.parseJSON value
                else Measured <$> AE.parseJSON value <*> AE.parseJSON (AE.Object $ fields <> KM.fromList [("resourceCostStatus", AE.String "unpriced"), ("tail", AE.String "")])
      )
      value


data RegressionReport duration = RegressionReport {id :: UUIDId "regression_check", projectId :: ProjectId, runId :: UUIDId "run", attemptId :: UUIDId "agent_attempt", imageBuildId :: ImageId, workspace :: Maybe Fingerprint, createdAt :: Maybe UTCTime, scriptSha256 :: Fingerprint, testPatchSha256 :: Maybe Fingerprint, regressionSha256 :: Fingerprint, failureMessage :: Text, status :: ReportState, results :: [CheckExecution duration]}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


type RegressionReceipt = CheckReceipt


data RegressionResult = RegressionResult {report :: RegressionReport Double, receipts :: [RegressionReceipt], isError :: Bool}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data RegressionError = ComparisonBudgetExhausted | RegressionUnavailable
  deriving stock (Show)
  deriving anyclass (Exception)


regressionErrorText :: RegressionError -> Text
regressionErrorText = \case
  ComparisonBudgetExhausted -> "Local check comparison budget is exhausted"
  RegressionUnavailable -> "Native regression could not produce retained evidence"


data SandboxFailure = SandboxCleanupFailed | SandboxOwnerLost
  deriving stock (Show)
  deriving anyclass (Exception)


data Workspace :: Effect where
  PruneWorkspaces :: PruneOptions -> FilePath -> Workspace m (Either Text PruneReport)
  RunWorkspace :: Options -> Operation -> FilePath -> Workspace m (Either Text (Maybe Prepared))
  WithWorkspace :: Options -> FilePath -> (Maybe Leased -> m a) -> Workspace m (Either Text a)
  RunRegression :: RegressionOptions -> FilePath -> Workspace m (Either RegressionError RegressionResult)


type instance DispatchOf Workspace = 'Dynamic


makeEffect ''Workspace


instance AE.ToJSON (Command result) where
  toJSON = $(ATH.mkToJSON AE.defaultOptions ''Command)
  toEncoding = $(ATH.mkToEncoding AE.defaultOptions ''Command)


runWorkspaceTest :: (Options -> Operation -> FilePath -> Either Text (Maybe Prepared)) -> (RegressionOptions -> FilePath -> Either RegressionError RegressionResult) -> (PruneOptions -> FilePath -> Either Text PruneReport) -> Eff (Workspace ': es) a -> Eff es a
runWorkspaceTest respond respondRegression respondPrune = interpret $ \env -> \case
  PruneWorkspaces options requestFile -> pure $ respondPrune options requestFile
  RunWorkspace options operation requestFile -> pure $ respond options operation requestFile
  WithWorkspace options requestFile action -> case respond options Lookup requestFile of
    Left message -> pure $ Left message
    Right prepared -> localSeqUnlift env $ \unlift -> Right <$> unlift (action $ fmap (\value -> Leased value (value.path <> ".resume")) prepared)
  RunRegression options requestFile -> pure $ respondRegression options requestFile


runWorkspaceReal :: IOE :> es => Eff (Workspace ': es) a -> Eff es a
runWorkspaceReal = interpret $ \env -> \case
  PruneWorkspaces options requestFile -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) do
      request <- decode @PruneRequest =<< readBounded requestFile 65536
      withOwner request.ownerPid $ prune options request
    pure $ case outcome of Right (Just result) -> Right result; _ -> Left "Checkpoint pruning could not complete"
  RunWorkspace options operation requestFile -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) do
      request <- decode =<< readBounded requestFile 65536
      withOwner request.ownerPid $ case operation of
        Lookup -> do
          root <- canonicalizePath options.cacheDir
          withProjectLock root request.projectId $ lookupWorkspace options operation request
        _ -> lookupWorkspace options operation request
    pure $ case outcome of Right (Just prepared) -> Right prepared; _ -> Left "Workspace preparation or integrity verification failed"
  WithWorkspace options requestFile action -> localSeqUnliftIO env $ \unlift -> liftIO do
    outcome <- tryAny do
      request <- decode =<< readBounded requestFile 65536
      withOwner request.ownerPid $ leaseWorkspace options request (tryAny . unlift . action)
    case outcome of
      Left _ -> pure $ Left "Workspace preparation or integrity verification failed"
      Right result -> Right <$> either throwIO pure result
  RunRegression options requestFile -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) do
      request <- decode =<< readBounded requestFile (2 * 1048576)
      withOwner request.ownerPid $ verifyRegression options request
    pure $ case outcome of
      Right (Just result) -> Right result
      Left failure -> Left $ fromMaybe RegressionUnavailable $ fromException failure
      _ -> Left RegressionUnavailable


prune :: PruneOptions -> PruneRequest -> IO PruneReport
prune options request = do
  started <- getMonotonicTimeNSec
  now <- getCurrentTime
  root <- canonicalizePath options.cacheDir
  withProjectLock root request.projectId do
    remote <- readRemote options.remote
    connection <- connectStorage remote
    let project = root </> projectText request.projectId
        parent = project </> "workspaces"
        leases = project </> "leases"
        directory key = parent </> toString (digestText key)
        measure = do
          present <- doesDirectoryExist leases
          when present $ canonical leases
          output <- decodeUtf8 @Text <$> readProcessStdout_ (proc "du" $ ["-sk", parent] <> [leases | present])
          sizes <- forM (lines output) \line -> do
            size <- maybe invalid pure $ viaNonEmpty head (words line) >>= readMaybe @Word64 . toString
            ensure $ size <= maxBound `div` 1024
            pure $ size * 1024
          case sizes of
            [cached] -> pure (cached, 0)
            [cached, leased] -> do
              ensure $ toInteger cached + toInteger leased <= toInteger (maxBound :: Word64)
              pure (cached + leased, leased)
            _ -> invalid
        readEntry key = do
          let entry = directory key
          canonical entry
          Posix.getSymbolicLinkStatus entry >>= ensure . Posix.isDirectory
          names <- listDirectory entry
          ensure $ all (`elem` ["manifest.json", "baseline.qcow2", "last-used.json"]) names
          manifest <- decode @Manifest =<< readBounded (entry </> "manifest.json") 65536
          ensure $ manifest.identity.projectId == request.projectId && identityKey manifest.identity == key
          stamp <- tryIOError $ readBounded (entry </> "last-used.json") 65536
          lastUsed <- case stamp of Left failure | isDoesNotExistError failure -> pure manifest.createdAt; Left failure -> throwIO failure; Right bytes -> decode bytes
          status <- Posix.getSymbolicLinkStatus $ entry </> "baseline.qcow2"
          ensure $ Posix.isRegularFile status
          pure $ CacheEntry key manifest lastUsed status
        verifyRemote entry = do
          let manifest = entry.manifest
              prefix = "agent-workspaces/v1/" <> toText (projectText request.projectId) <> "/" <> digestText entry.key <> "/" <> digestText manifest.sha256
          ensure $ manifest.bytes <= remote.maxBytes
          encoded <- storage connection do
            response <- Minio.getObject remote.bucket (prefix <> ".json") Minio.defaultGetObjectOptions
            runConduit $ response.gorObjectStream .| CC.takeE 65537 .| CC.sinkLazy
          ensure $ LBS.length encoded <= 65536
          archived <- decode @Manifest $ toStrict encoded
          ensure $ archived.identity == manifest.identity && archived.sha256 == manifest.sha256 && archived.bytes == manifest.bytes
          (context, size) <- storage connection do
            response <- Minio.getObject remote.bucket (prefix <> ".qcow2") Minio.defaultGetObjectOptions
            liftIO $ ensure $ response.gorObjectInfo.oiSize == fromIntegral manifest.bytes
            runConduit $ response.gorObjectStream .| CC.takeE (fromIntegral manifest.bytes + 1) .| CC.foldl (\(!context, !count) chunk -> (hashUpdate context chunk, count + fromIntegral (BS.length chunk))) (hashInit :: Context SHA256, 0 :: Word64)
          ensure $ size == manifest.bytes && digestFingerprint (hashFinalize context) == manifest.sha256
    let journals = project </> "prunes"
    createDirectoryIfMissing True journals
    canonical journals
    Posix.setFileMode journals 0o700
    identifier <- UUIDId <$> UUID4.nextRandom
    let record = PruneRecord identifier request.projectId now PruneStarted
        UUIDId uid = identifier
        save outcome = publish (journals </> toString (UUID.toText uid) <> ".json") (record :: PruneRecord){outcome}
    save PruneStarted
    names <- listDirectory parent
    let keys = mapMaybe (mkFingerprint . toText) names
    checked <- forM keys $ \key -> (key,) <$> tryAny (readEntry key)
    (before, leased) <- measure
    let initial = PruneReport request.projectId before before leased request.maxBytes (Seq.fromList [Kept key InvalidEntry | (key, Left _) <- checked]) [name | name <- names, name /= ".native.lock", isNothing $ mkFingerprint $ toText name] Unpriced
    save $ PruneRunning initial
    final <-
      foldM
        ( \report entry ->
            if report.afterBytes <= request.maxBytes
              then pure report
              else do
                action <-
                  if Posix.linkCount entry.status > 1
                    then pure $ Kept entry.key Pinned
                    else do
                      diskFingerprint <- tryAny $ fingerprint $ directory entry.key </> "baseline.qcow2"
                      case diskFingerprint of
                        Left _ -> pure $ Kept entry.key InvalidEntry
                        Right measured | measured /= FileFingerprint entry.manifest.sha256 entry.manifest.bytes -> pure $ Kept entry.key InvalidEntry
                        Right _ -> do
                          proof <- tryAny $ verifyRemote entry
                          case proof of
                            Left _ -> pure $ Kept entry.key ArchiveUnavailable
                            Right () -> do
                              removed <- tryAny $ withTempDirectory parent ".evict-" \temporary -> do
                                let retired = temporary </> "entry"
                                renameDirectory (directory entry.key) retired
                                sync parent
                                removePathForcibly retired
                              pure $ either (const $ EvictionFailed entry.key) (const $ Evicted entry.key) removed
                (after, pinned) <- measure
                let updated = report{afterBytes = after, leaseBytes = pinned, entries = report.entries Seq.|> action}
                save $ PruneRunning updated
                pure updated
        )
        initial
        (sortOn (.lastUsed) $ rights $ map snd checked)
    ended <- getMonotonicTimeNSec
    save $ PruneFinished final (fromIntegral (ended - started) / 1000000)
    pure final


-- | Retained host logs paired with the guest paths the report advertises for them.
regressionLogs :: RegressionOptions -> RegressionResult -> [(FilePath, Text)]
regressionLogs options result =
  [ (options.artifactDir </> toString (UUID.toText check) </> (if key.side == Base then "base.log" else "head.log"), path)
  | let UUIDId check = result.report.id
  , Measured key measured <- result.report.results
  , Just path <- [measured.logPath]
  ]


verifyRegression :: RegressionOptions -> RegressionRequest -> IO RegressionResult
verifyRegression options request = do
  let artifact = request.artifact
      revisions = case request.revisions of BaseOnly baseline -> (Base, baseline) :| []; HeadOnly candidate -> (Head, candidate) :| []; Compare baseline candidate -> (Base, baseline) :| [(Head, candidate)]
      identifierText (UUIDId value) = toString $ UUID.toText value
      privateWrite path bytes = writeFileBS path bytes >> Posix.setFileMode path 0o600
  ensure $ isAbsolute options.checkout && isAbsolute options.artifactDir
  checkout <- canonicalizePath options.checkout
  let hostGit args = readProcessStdout_ =<< gitProcess "file" checkout args []
  headRevision <- T.strip . decodeUtf8 <$> hostGit ["rev-parse", "HEAD"]
  for_ revisions \(side, revision) -> ensure $ pinnedRevisionText revision == (if side == Base then pinnedRevisionText request.base else headRevision)
  root <- canonicalizePath options.artifactDir
  createDirectoryIfMissing True root
  canonical root
  Posix.setFileMode root 0o700
  let counter = takeDirectory root </> ".regression-" <> identifierText request.sessionId <> ".count"
  withFileLock (counter <> ".lock") Exclusive \_ -> do
    previous <- tryIOError $ readBounded counter 16
    count <- case previous of Left failure | isDoesNotExistError failure -> pure 0; Left failure -> throwIO failure; Right bytes -> decode @Word8 bytes
    unless (count < 3) $ throwIO ComparisonBudgetExhausted
    publish counter $ count + 1
    checkId <- UUIDId <$> UUID4.nextRandom
    created <- getCurrentTime
    let directory = root </> identifierText checkId
        workspaceHash = case request.workspace of Pristine -> Nothing; PreparedCheckpoint snapshot -> Just snapshot.sha256
        template = RegressionReport checkId request.projectId request.runId request.attemptId request.imageBuildId workspaceHash (Just created) (sha $ encodeUtf8 artifact.script) (sha . encodeUtf8 <$> artifact.testPatch) (sha $ toStrict $ Encoding.encodingToLazyByteString $ AE.genericToEncoding AE.defaultOptions artifact) artifact.failureMessage Running []
    createDirectory directory
    Posix.setFileMode directory 0o700
    sync root
    privateWrite (directory </> "script.sh") $ encodeUtf8 artifact.script
    for_ artifact.testPatch $ privateWrite (directory </> "test.patch") . encodeUtf8
    traverse_ sync $ [directory </> "script.sh"] <> [directory </> "test.patch" | isJust artifact.testPatch]
    executions <- newIORef []
    receipts <- newIORef []
    let persist reportState = readIORef executions >>= \results -> publish (directory </> "report.json") template{status = reportState, results}
        currentKey = \case Pending key -> key; Measured key _ -> key
        update key measured = do
          modifyIORef' executions $ map \execution -> if (currentKey execution).id == key.id then Measured key measured else execution
          persist Running
    persist Running
    flip onException (persist Incomplete) do
      for_ revisions \(side, revision) ->
        whenM (all ((/= CleanupFailed) . (.status)) <$> readIORef receipts) do
          key <- (\identifier -> ExecutionKey (UUIDId identifier) side revision) <$> UUID4.nextRandom
          modifyIORef' executions (<> [Pending key])
          persist Running
          started <- getMonotonicTimeNSec
          observation <- newIORef $ Measurement Blocked Nothing (Resources 2 1024) Unpriced 0 0 (sha "") "" Nothing Nothing (Just "Check did not complete with usable execution evidence") Nothing Nothing Nothing Nothing
          let logPath = directory </> (if side == Base then "base.log" else "head.log")
              change f = modifyIORef' observation f >> readIORef observation >>= update key
              checked :: RPC -> NonEmpty Text -> IO Text
              checked rpc argv = do
                result <- rpc @Execution $ Execute $ GuestCommand argv (Just "/") False
                unless (result.exitCode == 0) do
                  change $ \measured -> measured{reason = Just "Verifier preparation failed"}
                  invalid
                pure result.stdout
              copy :: RPC -> FilePath -> Text -> IO ()
              copy rpc path destination = rpc @() $ CopyFile $ Copy path destination
          privateWrite logPath ""
          let released = \case
                Released -> pass
                ReleaseFailed -> publish counter (3 :: Word8) >> change (\measured -> measured{status = CleanupFailed})
          outcome <- tryAny $ Timeout.timeout (120 * 1000000) $ withSystemTempDirectory "monoscope-verifier-snapshot" $ \private -> withSandboxRelease options.bridge released \rpc -> do
            image <- rpc @Image $ Resolve options.image
            ensure $ image.buildId == request.imageBuildId
            let sandbox = Sandbox image.assetDir 2 1024 OfflineCow $ one ("/checkout", checkout)
            case request.workspace of
              Pristine -> rpc @() $ Create sandbox
              PreparedCheckpoint snapshot -> do
                let mutable = private </> "resume.qcow2"
                copySnapshot snapshot mutable
                checkpoint <- rpc @CheckpointData $ InspectCheckpoint mutable
                ensure $ checkpoint.guestAssetBuildId == request.imageBuildId
                rpc @() $ Resume (sandbox, mutable)
            vm <- rpc $ GetIdentity ()
            change $ \measured -> measured{vmId = Just vm.toText}
            when (case request.workspace of Pristine -> True; _ -> False) $ void $ checked rpc $ "/bin/mkdir" :| ["-p", "/workspace"]
            checkoutGuest (checked rpc) (case request.workspace of Pristine -> False; _ -> True) (pinnedRevisionText revision)
            for_ artifact.testPatch \_ -> do
              copy rpc (directory </> "test.patch") "/run/monoscope-test.patch"
              stats <- checked rpc $ guestGit ["apply", "--numstat", "-z", "/run/monoscope-test.patch"]
              paths <- traverse (\entry -> case T.splitOn "\t" entry of _ : _ : path | not (null path) -> pure $ T.intercalate "\t" path; _ -> invalid) $ filter (not . T.null) $ T.splitOn "\0" stats
              ensure $ not (null paths) && length paths <= 100
              privateWrite (directory </> "test-paths") $ encodeUtf8 $ T.intercalate "\0" paths <> "\0"
              copy rpc (directory </> "test-paths") "/run/monoscope-test-paths"
              applicable <- rpc @Execution $ Execute $ GuestCommand (guestGit ["apply", "--index", "--check", "/run/monoscope-test.patch"]) (Just "/") False
              placement <-
                if applicable.exitCode == 0
                  then checked rpc (guestGit ["apply", "--index", "/run/monoscope-test.patch"]) $> Injected
                  else do
                    retained <- rpc @Execution $ Execute $ GuestCommand (guestGit ["apply", "--index", "--reverse", "--check", "/run/monoscope-test.patch"]) (Just "/") False
                    unless (retained.exitCode == 0) do
                      change $ \measured -> measured{reason = Just "Frozen test patch neither applies nor matches this revision"}
                      invalid
                    pure Retained
              change $ \measured -> measured{testPatchDisposition = Just placement}
            void $ checked rpc $ "/bin/bash" :| ["--noprofile", "--norc", "-c", "git -c safe.directory=/workspace -C /workspace ls-files -z > /run/monoscope-baseline-paths"]
            copy rpc (directory </> "script.sh") "/run/monoscope-regression.sh"
            void $ checked rpc $ "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]
            privateWrite (directory </> "fingerprint.sh") $ encodeUtf8 fingerprintScript
            copy rpc (directory </> "fingerprint.sh") "/run/monoscope-fingerprint.sh"
            let sourceHash paths = do
                  result <- rpc @Execution $ Execute $ GuestCommand ("/bin/bash" :| ["--noprofile", "--norc", "/run/monoscope-fingerprint.sh", paths]) (Just "/workspace") False
                  ensure $ result.exitCode == 0
                  maybe invalid pure $ viaNonEmpty head (words result.stdout) >>= mkFingerprint
            before <- sourceHash "/run/monoscope-baseline-paths"
            change $ \measured -> measured{sourceBefore = Just before}
            for_ artifact.testPatch \_ -> do
              frozen <- sourceHash "/run/monoscope-test-paths"
              change $ \measured -> measured{testFilesSha256 = Just frozen}
              prior <- readIORef executions
              let firstFrozen = viaNonEmpty head prior >>= \case Measured _ measured -> measured.testFilesSha256; _ -> Nothing
              when (side == Head && firstFrozen /= Just frozen) do
                change $ \measured -> measured{reason = Just "Candidate changed the frozen regression files"}
                invalid
            result <- rpc @LogExit $ ExecuteLogged $ LoggedCommand (GuestCommand (agentCommand ["/bin/bash", "--noprofile", "--norc", "/run/monoscope-regression.sh"]) (Just "/workspace") False) logPath 1048576 CloseSandbox
            case result of
              LogLimit -> change $ \measured -> measured{status = OutputLimit, reason = Just "Check output limit"}
              Exited exit -> do
                change $ \measured -> measured{exitCode = Just exit}
                after <- sourceHash "/run/monoscope-baseline-paths"
                logBytes <- readBounded logPath 1048576
                let (status, reason)
                      | before /= after = (Invalidated, Just "Tracked source changed during the check")
                      | exit `elem` [126, 127] = (Blocked, Just "Check command or dependency unavailable")
                      | exit == 0 = (Passed, Nothing)
                      | encodeUtf8 artifact.failureMessage `BS.isInfixOf` logBytes = (Failed, Nothing)
                      | otherwise = (Blocked, Just $ "Failure did not emit the expected case-sensitive literal " <> decodeUtf8 (AE.encode artifact.failureMessage) <> ". Inspect the full log and correct the check or assertion text; setup/dependency errors are not reproduced bugs.")
                change $ \measured -> measured{status, reason, exitCode = Just exit, sourceAfter = Just after}
          case outcome of
            Right Nothing -> change $ \measured -> if measured.status == CleanupFailed then measured else measured{status = TimedOut}
            Left _ -> pass
            Right (Just ()) -> pass
          log <- readBounded logPath 1048576
          sync logPath
          ended <- getMonotonicTimeNSec
          change $ \measured -> measured{durationMs = fromIntegral (ended - started) / 1000000, logBytes = fromIntegral $ BS.length log, logSha256 = sha log, tail = decodeUtf8With lenientDecode $ BS.drop (max 0 $ BS.length log - 4096) log, logPath = Just $ "/run/monoscope-review-output/check-" <> toText (identifierText checkId) <> "-" <> (if side == Base then "base" else "head") <> ".log"}
          measured <- readIORef observation
          let kind = case artifact.testPatch of Nothing -> Just $ ScriptCheck (digestText template.regressionSha256); Just _ -> case measured.testPatchDisposition of Just Injected -> Just $ InjectedTest (digestText template.regressionSha256); Just Retained -> Just $ RetainedTest (digestText template.regressionSha256); Nothing -> Nothing
              receipt = CheckReceipt key.id request.attemptId checkId side (pinnedRevisionText revision) (case request.imageBuildId of ImageId image -> UUID.toText image) (digestText template.scriptSha256) kind measured.status measured.exitCode (ceiling measured.durationMs) 2 1024 measured.logBytes (digestText measured.logSha256) workspaceHash
          publish (takeDirectory root </> toString receipt.id.toText <> ".json") $ AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= receipt]
          modifyIORef' receipts (<> [receipt])
      results <- readIORef executions
      saved <- readIORef receipts
      let report = template{status = if length results == length revisions then Completed else Incomplete, results}
      publish (directory </> "report.json") report
      pure $ RegressionResult report saved $ any (\receipt -> receipt.status `notElem` [Passed, Failed]) saved


lookupWorkspace :: Options -> Operation -> Request -> IO (Maybe Prepared)
lookupWorkspace options operation request = do
  scriptBytes <- readBounded options.script (4 * 65536)
  scriptText <- either (const invalid) pure $ decodeUtf8' scriptBytes
  ensure $ T.length scriptText <= 65536 && not (T.null $ T.strip scriptText)
  dependency <- traverse fingerprint options.dependencyArchive
  withSandbox options.bridge \rpc -> do
    image <- rpc @Image $ Resolve options.image
    for_ request.expectedImageBuildId $ ensure . (== image.buildId)
    assetDir <- canonicalizePath image.assetDir
    ensure $ assetDir == image.assetDir
    let repoText = repositoryText request.repository
        baseText = pinnedRevisionText request.base
        identity = Identity request.projectId request.repository request.base image.buildId (sha scriptBytes) dependency
        key = identityKey identity
    root <- canonicalizePath options.cacheDir
    let parent = root </> projectText request.projectId </> "workspaces"
        directory = parent </> toString (workspaceFingerprintText key)
        load = loadWorkspace identity image directory (rpc . InspectCheckpoint)
        locked = withProjectLock root request.projectId
        transfer direction remoteFile action = do
          remote <- readRemote remoteFile
          let records = root </> projectText request.projectId </> "transfers"
          createDirectoryIfMissing True records
          canonical records
          Posix.setFileMode records 0o700
          identifier <- UUIDId <$> UUID4.nextRandom
          now <- getCurrentTime
          started <- getMonotonicTimeNSec
          let record = TransferRecord identifier identity direction now TransferStarted Unpriced
              UUIDId transferId = identifier
              save outcome = publish (records </> toString (UUID.toText transferId) <> ".json") (record :: TransferRecord){outcome}
              elapsed = (\ended -> fromIntegral (ended - started) / 1000000) <$> getMonotonicTimeNSec
              prefix = "agent-workspaces/v1/" <> toText (projectText request.projectId) <> "/" <> workspaceFingerprintText key <> "/"
          save TransferStarted
          connection' <- connectStorage remote
          value <- action remote prefix connection' `onException` (elapsed >>= save . TransferFailed)
          elapsed >>= save . TransferCompleted (FileFingerprint value.manifest.sha256 value.manifest.bytes)
          pure $ Just value
        diskKey prefix manifest = prefix <> workspaceFingerprintText manifest.sha256 <> ".qcow2"
        manifestKey prefix = (prefix <>) . maybe "manifest.json" ((<> ".json") . workspaceFingerprintText)
    case operation of
      Lookup -> load
      Archive remoteFile -> locked $ transfer Upload remoteFile \remote prefix connection -> do
        prepared <- maybe invalid pure =<< load
        ensure $ prepared.manifest.bytes <= remote.maxBytes
        withFile prepared.path \handle bytes -> do
          ensure $ bytes == prepared.manifest.bytes
          void $ storage connection $ Minio.putObject remote.bucket (diskKey prefix prepared.manifest) (CC.sourceHandle handle) (Just $ fromIntegral bytes) Minio.defaultPutObjectOptions
        uploaded <- storage connection $ Minio.statObject remote.bucket (diskKey prefix prepared.manifest) Minio.defaultGetObjectOptions
        ensure $ uploaded.oiSize == fromIntegral prepared.manifest.bytes
        let manifestBytes = AE.encode prepared.manifest
        for_ [manifestKey prefix $ Just prepared.manifest.sha256, manifestKey prefix Nothing] \object ->
          void $ storage connection $ Minio.putObject remote.bucket object (CC.sourceLazy manifestBytes) (Just $ LBS.length manifestBytes) Minio.defaultPutObjectOptions
        pure prepared
      Restore remoteFile -> locked do
        saved <- load
        case saved of
          Just _ -> pure saved
          Nothing -> transfer Download remoteFile \remote prefix connection -> withTempDirectory parent ".restore-" \temporary -> do
            encoded <- storage connection do
              response <- Minio.getObject remote.bucket (manifestKey prefix request.expectedCheckpoint) Minio.defaultGetObjectOptions
              runConduit $ response.gorObjectStream .| CC.takeE 65537 .| CC.sinkLazy
            ensure $ LBS.length encoded <= 65536
            archived <- decode @Manifest $ toStrict encoded
            ensure $ archived.identity == identity && archived.bytes <= remote.maxBytes
            for_ request.expectedCheckpoint $ ensure . (== archived.sha256)
            let disk = temporary </> "baseline.qcow2"
            withBinaryFile disk WriteMode \handle -> storage connection do
              response <- Minio.getObject remote.bucket (diskKey prefix archived) Minio.defaultGetObjectOptions
              liftIO $ ensure $ response.gorObjectInfo.oiSize == fromIntegral archived.bytes
              runConduit $ response.gorObjectStream .| CC.takeE (fromIntegral archived.bytes + 1) .| CC.sinkHandle handle
            Posix.setFileMode disk 0o600
            measured <- fingerprint disk
            ensure $ measured == FileFingerprint archived.sha256 archived.bytes
            writeFileLBS (temporary </> "manifest.json") $ AE.encode (archived :: Manifest){assetDir = image.assetDir}
            Posix.setFileMode (temporary </> "manifest.json") 0o600
            restored <- maybe invalid pure =<< loadWorkspace identity image temporary (rpc . InspectCheckpoint)
            traverse_ sync [disk, temporary </> "manifest.json", temporary]
            renameDirectory temporary directory
            sync parent
            pure (restored :: Prepared){path = directory </> "baseline.qcow2"}
      Prepare checkout -> do
        ensure $ isAbsolute checkout
        cleanCheckout <- canonicalizePath checkout
        locked do
          saved <- load
          case saved of
            Just _ -> pure saved
            Nothing -> withTempDirectory parent ".prepare-" \temporary -> withSystemTempDirectory "monoscope-native-baseline" \source -> do
              let clean = source </> "checkout"
                  git args = readProcessStdout_ =<< gitProcess "file" source args []
                  execute cwd quiet argv = do
                    result <- rpc @Execution $ Execute $ GuestCommand argv cwd quiet
                    ensure $ result.exitCode == 0
                    pure $ T.strip result.stdout
                  gitGuest = execute (Just "/workspace") False . guestGit
                  parseHash value = maybe invalid pure $ viaNonEmpty head (words value) >>= mkFingerprint
                  copy hostPath destination = rpc @() $ CopyFile $ Copy hostPath destination
              void $ git ["init", "--quiet", clean]
              void $ git ["-C", clean, "fetch", "--quiet", "--no-tags", "--", cleanCheckout, toString baseText]
              void $ git ["-C", clean, "checkout", "--quiet", "--detach", toString baseText]
              void $ git ["-C", clean, "remote", "add", "origin", toString repoText]
              started <- getMonotonicTimeNSec
              rpc @() $ Create $ Sandbox image.assetDir 2 1024 OfflineCow (one ("/baseline", clean))
              for_ ["/bin/mkdir" :| ["-p", "/workspace"], "/bin/cp" :| ["-R", "/baseline/.", "/workspace"], "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]] $ void . execute Nothing False
              tracked <- git ["-C", clean, "ls-files", "-z"]
              writeFileLBS (source </> "tracked") tracked
              copy (source </> "tracked") "/run/monoscope-baseline-paths"
              writeFileBS (source </> "fingerprint.sh") $ encodeUtf8 fingerprintScript
              copy (source </> "fingerprint.sh") "/run/monoscope-fingerprint.sh"
              let sourceHash = execute (Just "/workspace") False ("/bin/bash" :| ["--noprofile", "--norc", "/run/monoscope-fingerprint.sh"]) >>= parseHash
              before <- sourceHash
              for_ dependency \expected -> do
                archive <- maybe invalid pure options.dependencyArchive
                copy archive "/run/monoscope-dependencies.tar"
                measured <- execute (Just "/workspace") False $ "/usr/bin/sha256sum" :| ["/run/monoscope-dependencies.tar"]
                actual <- parseHash measured
                ensure $ actual == expected.sha256
              writeFileBS (source </> "prepare.sh") scriptBytes
              copy (source </> "prepare.sh") "/run/monoscope-prepare.sh"
              void $ execute (Just "/workspace") True $ agentCommand ["/bin/bash", "--noprofile", "--norc", "/run/monoscope-prepare.sh"]
              revision <- gitGuest ["rev-parse", "HEAD"]
              after <- sourceHash
              ensure $ revision == baseText && before == after
              checkpoint <- rpc @CheckpointData $ Checkpoint $ temporary </> "baseline.qcow2"
              ensure $ checkpoint.guestAssetBuildId == image.buildId
              measured <- fingerprint $ temporary </> "baseline.qcow2"
              ended <- getMonotonicTimeNSec
              let manifest = Manifest 1 identity image.assetDir checkpoint.createdAt measured.sha256 (fromIntegral (ended - started) / 1000000) measured.bytes 2 1024 Unpriced
              writeFileLBS (temporary </> "manifest.json") $ AE.encode manifest
              Posix.setFileMode (temporary </> "manifest.json") 0o600
              traverse_ sync [temporary </> "baseline.qcow2", temporary </> "manifest.json", temporary]
              renameDirectory temporary directory
              sync parent
              load
  where
    loadWorkspace :: Identity -> Image -> FilePath -> (FilePath -> IO CheckpointData) -> IO (Maybe Prepared)
    loadWorkspace identity image directory inspect = do
      loaded <- tryIOError $ readBounded (directory </> "manifest.json") 65536
      case loaded of
        Left failure | isDoesNotExistError failure -> pure Nothing
        Left failure -> throwIO failure
        Right bytes -> do
          manifest <- decode @Manifest bytes
          for_ request.expectedCheckpoint $ ensure . (== manifest.sha256)
          ensure $ manifest.identity == identity && manifest.assetDir == image.assetDir
          let path = directory </> "baseline.qcow2"
          canonical directory
          measured <- fingerprint path
          ensure $ measured.sha256 == manifest.sha256 && measured.bytes == manifest.bytes
          checkpoint <- inspect path
          ensure $ checkpoint.guestAssetBuildId == identity.imageBuildId
          publish (directory </> "last-used.json") =<< getCurrentTime
          pure $ Just $ Prepared manifest path


-- | Holds a private copy of the matching prepared checkpoint while the action runs; @Nothing@ on a cache miss.
leaseWorkspace :: Options -> Request -> (Maybe Leased -> IO a) -> IO a
leaseWorkspace options request action = do
  root <- canonicalizePath options.cacheDir
  bracket
    ( withProjectLock root request.projectId do
        prepared <- lookupWorkspace options Lookup request
        forM prepared \value -> do
          let parent = root </> projectText request.projectId </> "leases"
          createDirectoryIfMissing True parent
          canonical parent
          Posix.setFileMode parent 0o700
          bracketOnError (createTempDirectory parent ".lease-") removePathForcibly \directory -> do
            let immutable = directory </> "baseline.qcow2"
                mutable = directory </> "resume.qcow2"
            Posix.createLink value.path immutable
            copySnapshot (Snapshot immutable value.manifest.sha256) mutable
            pure $ Leased value{path = immutable} mutable
    )
    (\prepared -> withProjectLock root request.projectId $ for_ prepared $ removePathForcibly . takeDirectory . (.resumePath))
    action


projectText :: ProjectId -> FilePath
projectText project = toString project.toText


digestText :: Fingerprint -> Text
digestText = workspaceFingerprintText


identityKey :: Identity -> Fingerprint
identityKey = sha . toStrict . AE.encode


withProjectLock :: FilePath -> ProjectId -> IO a -> IO a
withProjectLock root projectId action = do
  createDirectoryIfMissing True root
  canonical root
  let project = root </> projectText projectId
      workspaces = project </> "workspaces"
  for_ [project, workspaces] \path -> do
    createDirectoryIfMissing True path
    canonical path
    Posix.setFileMode path 0o700
    sync path
  withFileLock (workspaces </> ".native.lock") Exclusive $ const action


readRemote :: FilePath -> IO Remote
readRemote path = do
  remote <- decode @Remote =<< readBounded path 65536
  case parseURI $ toString remote.endpoint of
    Just uri | uri.uriScheme `elem` ["http:", "https:"], Just authority <- uri.uriAuthority, null authority.uriUserInfo, not (null authority.uriRegName), uri.uriPath `elem` ["", "/"], null uri.uriQuery, null uri.uriFragment -> pass
    _ -> invalid
  ensure $ not (any (T.null . T.strip) [remote.bucket, remote.region, remote.accessKey, remote.secretKey]) && remote.maxBytes > 0 && remote.maxBytes < fromIntegral (maxBound :: Int64)
  pure remote


connectStorage :: Remote -> IO Minio.MinioConn
connectStorage remote = do
  let connection = Minio.setCreds (Minio.CredentialValue (fromString $ toString remote.accessKey) (fromString $ toString remote.secretKey) Nothing) $ Minio.setRegion remote.region $ fromString $ toString remote.endpoint
  Minio.mkMinioConn connection =<< TLS.getGlobalManager


storage :: Minio.MinioConn -> Minio.Minio a -> IO a
storage connection action = either (const invalid) pure =<< Minio.runMinioWith connection action


copySnapshot :: Snapshot -> FilePath -> IO ()
copySnapshot snapshot destination = do
  canonical snapshot.path
  withFile snapshot.path $ \source _ -> withBinaryFile destination WriteMode $ \target -> LBS.hPut target =<< LBS.hGetContents source
  Posix.setFileMode destination 0o600
  copied <- fingerprint destination
  ensure $ copied.sha256 == snapshot.sha256


fingerprint :: FilePath -> IO FileFingerprint
fingerprint path = withFile path \handle size -> do
  ensure $ size > 0
  bytes <- LBS.hGetContents handle
  digest <- evaluate $ digestFingerprint (hashlazy bytes)
  pure $ FileFingerprint digest size


readBounded :: FilePath -> Word64 -> IO ByteString
readBounded path limit = withFile path \handle size -> do
  ensure $ size <= limit
  bytes <- BS.hGet handle $ fromIntegral limit + 1
  ensure $ fromIntegral (BS.length bytes) == size
  pure bytes


withFile :: FilePath -> (Handle -> Word64 -> IO a) -> IO a
withFile path action =
  bracket
    ( bracketOnError (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.nonBlock = True, Posix.cloexec = True}) Posix.closeFd \fd -> do
        status <- Posix.getFdStatus fd
        ensure $ Posix.isRegularFile status && Posix.fileSize status >= 0
        (,fromIntegral $ Posix.fileSize status) <$> Posix.fdToHandle fd
    )
    (hClose . fst)
    (uncurry action)


canonical :: FilePath -> IO ()
canonical path = canonicalizePath path >>= ensure . (== path)


ensure :: Bool -> IO ()
ensure condition = unless condition invalid


invalid :: IO a
invalid = throwIO $ userError "Invalid prepared workspace"


decode :: AE.FromJSON a => ByteString -> IO a
decode = either (const invalid) pure . AE.eitherDecodeStrict


sha :: ByteString -> Fingerprint
sha = digestFingerprint . hash


fingerprintScript :: Text
fingerprintScript =
  """
  set -euo pipefail
  while IFS= read -r -d '' file; do
    printf '%s\\0' "$file"
    if [ -e "./$file" ] || [ -L "./$file" ]; then
      /usr/bin/stat -c '%f' -- "./$file"
      if [ -L "./$file" ]; then /usr/bin/readlink -z -- "./$file"; else /usr/bin/sha256sum -- "./$file"; fi
    else printf 'missing\\0'; fi
  done < "${1:-/run/monoscope-baseline-paths}" | /usr/bin/sha256sum
  """


type RPC = forall value. AE.FromJSON value => Command value -> IO value


withSandbox :: FilePath -> (RPC -> IO a) -> IO a
withSandbox bridge = withSandboxRelease bridge (const pass)


-- | Cleanup is observed before restoring the action's exception, including cancellation.
withSandboxRelease :: FilePath -> (AllocationOutcome -> IO ()) -> (RPC -> IO a) -> IO a
withSandboxRelease bridge released action = do
  environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "XDG_CACHE_HOME"] || "GONDOLIN_" `isPrefixOf` key) <$> getEnvironment
  withProcessTerm (setEnv environment $ setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc bridge []) \process -> Exception.mask \restore -> do
    pending <- newIORef False
    let receive = Exception.mask \restoreRead -> do
          bytes <- restoreRead $ B8.hGetLine $ getStdout process
          writeIORef pending False
          ensure $ BS.length bytes <= 1048576
          pure bytes
        rpc :: forall value. AE.FromJSON value => Command value -> IO value
        rpc command = Exception.mask \restoreCall -> do
          writeIORef pending True
          LBS.hPut (getStdin process) (AE.encode command <> "\n")
          hFlush $ getStdin process
          bytes <- restoreCall receive
          either (const invalid) (either (const invalid) pure) $ AE.eitherDecodeStrict @(Either Text value) bytes
    result <- Exception.try @SomeException $ restore $ action rpc
    closed <- Exception.try @SomeException $ restore $ Timeout.timeout (2 * 1000000) do
      whenM (readIORef pending) $ void receive
      rpc $ Close ()
    let release = case closed of Right (Just ()) -> Released; _ -> ReleaseFailed
    released release
    case result of
      Left failure -> Exception.throwIO failure
      Right value -> case closed of
        Right (Just ()) -> pure value
        Left failure | isJust (Exception.fromException failure :: Maybe Exception.SomeAsyncException) -> Exception.throwIO failure
        _ -> throwIO SandboxCleanupFailed


withOwner :: Maybe Word32 -> IO a -> IO a
withOwner supplied action = do
  parent <- fromIntegral <$> getParentProcessID
  let owner = fromMaybe parent supplied
      present = getParentProcessID >>= \current -> unless (current == fromIntegral owner) $ throwIO SandboxOwnerLost
  present
  thread <- myThreadId
  withAsync (catchAny (forever $ threadDelay 250000 >> present) $ throwTo thread) $ const action


sync :: FilePath -> IO ()
sync path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.cloexec = True}) Posix.closeFd fileSynchronise


publishBytes :: FilePath -> LBS.ByteString -> IO ()
publishBytes path bytes = withTempFile (takeDirectory path) ".native-" \temporary handle -> do
  LBS.hPut handle bytes
  hFlush handle
  Posix.setFileMode temporary 0o600
  sync temporary
  renameFile temporary path
  sync $ takeDirectory path


publish :: AE.ToJSON value => FilePath -> value -> IO ()
publish path value = publishBytes path (AE.encode value <> "\n")


-- | Only PATH and explicit transport credentials enter host Git commands.
gitProcess :: String -> FilePath -> [String] -> [(String, String)] -> IO (ProcessConfig () () ())
gitProcess protocol directory args extra = do
  environment <- gitEnvironment protocol extra
  pure $ setWorkingDir directory $ setStderr nullStream $ setEnv environment $ proc "git" $ ["-c", "core.hooksPath=/dev/null", "-c", "core.fsmonitor=false"] <> args


gitEnvironment :: String -> [(String, String)] -> IO [(String, String)]
gitEnvironment protocol extra = do
  environment <- filter ((== "PATH") . fst) <$> getEnvironment
  pure $ environment <> [("LC_ALL", "C"), ("GIT_CONFIG_NOSYSTEM", "1"), ("GIT_CONFIG_GLOBAL", "/dev/null"), ("GIT_TERMINAL_PROMPT", "0"), ("GIT_ALLOW_PROTOCOL", protocol)] <> extra


-- | Capture both pipes concurrently; the extra byte distinguishes overflow from exact-limit output.
captureProcess :: Int -> ProcessConfig input output errorOutput -> IO (ExitCode, ByteString, ByteString)
captureProcess limit command = withProcessTerm (setStdout createPipe $ setStderr createPipe command) \process -> do
  let capture handle = do
        bytes <- toStrict <$> runConduit (CC.sourceHandle handle .| CC.takeE (limit + 1) .| CC.sinkLazy)
        ensure $ BS.length bytes <= limit
        pure bytes
  (out, err) <- concurrently (capture $ getStdout process) (capture $ getStderr process)
  (,,) <$> waitExitCode process <*> pure out <*> pure err


gitCredentials :: Text -> [(String, String)]
gitCredentials token =
  let settings = [("credential.helper", ""), ("http.followRedirects", "false")] <> [("http.https://github.com/.extraheader", "AUTHORIZATION: basic " <> extractBase64 (Base64.encodeBase64 $ encodeUtf8 $ "x-access-token:" <> token)) | not $ T.null token]
   in ("GIT_CONFIG_COUNT", show $ length settings) : concat [[("GIT_CONFIG_KEY_" <> show index, toString key), ("GIT_CONFIG_VALUE_" <> show index, toString value)] | (index, (key, value)) <- zip [0 :: Int ..] settings]
