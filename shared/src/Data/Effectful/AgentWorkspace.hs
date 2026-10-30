module Data.Effectful.AgentWorkspace (Workspace, Options (..), PruneOptions (..), PruneReport, pruneBudgetMet, pruneWorkspaces, MaintenanceReport, maintenanceBudgetMet, maintainWorkspaces, Operation (..), Prepared, Leased, TestFiles, TestPatchError (..), testPatchErrorText, renderTestPatch, RegressionOptions (..), RegressionResult, RegressionError (..), regressionErrorText, runWorkspace, withWorkspace, runRegression, runWorkspaceReal, runWorkspaceTest) where

import Conduit (runConduit, (.|))
import Control.Monad (foldM)
import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isAlphaNum, isAscii, isControl)
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
import GHC.TypeLits (Symbol)
import Network.HTTP.Client.TLS qualified as TLS
import Network.Minio qualified as Minio
import Network.URI (URI (..), URIAuth (..), parseURI)
import Pkg.Deriving (WrappedEnumSC (..))
import Pkg.GitCommand qualified as HostGit
import Relude hiding (Identity, identity, withFile)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, doesDirectoryExist, listDirectory, removePathForcibly, renameDirectory, renameFile)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock, withTryFileLock)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hClose, withBinaryFile)
import System.IO.Error (isDoesNotExistError, tryIOError, userError)
import System.IO.Temp (createTempDirectory, withSystemTempDirectory, withTempDirectory, withTempFile)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Process (getParentProcessID)
import System.Posix.Unistd (fileSynchronise)
import System.Process.Typed (ExitCode (..), byteStringInput, createPipe, getStdin, getStdout, nullStream, proc, readProcessStdout_, setEnv, setStderr, setStdin, setStdout, waitExitCode, withProcessTerm)
import UnliftIO.Async (withAsync)
import UnliftIO.Concurrent (myThreadId, threadDelay)
import UnliftIO.Exception (bracket, bracketOnError, catchAny, evaluate, onException, throwIO, throwTo, tryAny)
import UnliftIO.Timeout qualified as Timeout
import "cryptonite" Crypto.Hash (Context, Digest, SHA256, hash, hashFinalize, hashInit, hashUpdate, hashlazy)


newtype ProjectId = ProjectId UUID
  deriving stock (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON)


newtype ImageId = ImageId UUID
  deriving stock (Eq, Generic, Show)
  deriving newtype (AE.FromJSON, AE.ToJSON)


newtype Revision = Revision Text
  deriving stock (Eq)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON Revision where
  parseJSON = AE.withText "Pinned revision" $ \value ->
    if T.length value `elem` [40, 64] && T.all (\c -> c `elem` (['0' .. '9'] <> ['a' .. 'f'])) value then pure $ Revision value else fail "Invalid revision"


newtype Repository = Repository Text
  deriving stock (Eq)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON Repository where
  parseJSON = AE.withText "Credential-free repository" $ \value -> if validRepository value then pure $ Repository value else fail "Invalid repository"
    where
      validRepository :: Text -> Bool
      validRepository value
        | isAbsolute $ toString value = True
        | otherwise = case T.stripPrefix "https://github.com/" value >>= T.stripSuffix ".git" of
            Just slug | [owner, name] <- T.splitOn "/" slug -> all (\part -> not (T.null part) && part `notElem` [".", ".."] && T.all (\c -> isAscii c && (isAlphaNum c || c `elem` ['-', '_', '.'])) part) [owner, name]
            _ -> False


newtype Fingerprint = Fingerprint Text
  deriving stock (Eq)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON Fingerprint where
  parseJSON = AE.withText "SHA-256" $ maybe (fail "Invalid SHA-256") pure . mkFingerprint


mkFingerprint :: Text -> Maybe Fingerprint
mkFingerprint value = if T.length value == 64 && T.all (\c -> c `elem` (['0' .. '9'] <> ['a' .. 'f'])) value then Just $ Fingerprint value else Nothing


data Request = Request {projectId :: ProjectId, repository :: Repository, base :: Revision, expectedImageBuildId :: Maybe ImageId, expectedCheckpoint :: Maybe Fingerprint, ownerPid :: Maybe Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Options = Options {cacheDir :: FilePath, image :: Text, bridge :: FilePath, script :: FilePath, dependencyArchive :: Maybe FilePath}


data Operation = Lookup | Prepare FilePath | Archive FilePath | Restore FilePath


data PruneOptions = PruneOptions {cacheDir :: FilePath, remote :: FilePath}


data MaintenanceOptions = MaintenanceOptions {cacheDir :: FilePath, remote :: FilePath, maxBytes :: Word64}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data MaintenanceReport = MaintenanceReport {beforeBytes :: Word64, afterBytes :: Word64, targetBytes :: Word64, projects :: [PruneReport], failures :: [ProjectId], unrecognized :: [FilePath], recoveries :: [StageRecovery]}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


maintenanceBudgetMet :: MaintenanceReport -> Bool
maintenanceBudgetMet report = report.afterBytes <= report.targetBytes && null report.failures


data ArchivePolicy = RetainUnarchived | ArchiveIdle


data StageKind = DownloadStage | EvictionStage
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Reclamation copy = PartialDownload | ArchivedDownload copy | ArchivedEviction copy
  deriving stock (Foldable, Functor, Generic, Traversable)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data StagePhase = StageDownload | StageReclaiming (Reclamation FileFingerprint) | StageClosed StageKind | StageRecovered (Reclamation FileFingerprint)
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data StageRecord = StageRecord {id :: Identifier "workspace_stage", identity :: Identity, phase :: StagePhase, startedAt :: UTCTime, updatedAt :: UTCTime, resourceCostStatus :: ResourceCost}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Stage = Stage {record :: StageRecord, path :: FilePath, journal :: FilePath}


data VerifiedRemoteCopy = VerifiedRemoteCopy {identity :: Identity, snapshot :: FileFingerprint}


data StageOperation = DownloadOperation Identity | EvictionOperation VerifiedRemoteCopy


data RecoveryOutcome = StageRemoved (Reclamation FileFingerprint) | StageRetained RetainReason | StageRecoveryFailed
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data StageRecovery = StageRecovery {projectId :: ProjectId, id :: Identifier "workspace_stage", outcome :: RecoveryOutcome}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


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


data PruneRecord = PruneRecord {id :: Identifier "workspace_prune", projectId :: ProjectId, startedAt :: UTCTime, outcome :: PruneOutcome}
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


data TransferRecord = TransferRecord {id :: Identifier "workspace_transfer", identity :: Identity, direction :: TransferDirection, startedAt :: UTCTime, outcome :: TransferOutcome, resourceCostStatus :: ResourceCost}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data FileFingerprint = FileFingerprint {sha256 :: Fingerprint, bytes :: Word64}
  deriving stock (Eq, Generic)
  deriving anyclass (AE.FromJSON)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.OmitNothingFields] FileFingerprint


-- Declaration order preserves the existing JavaScript cache key encoding.
data Identity = Identity {projectId :: ProjectId, repository :: Repository, base :: Revision, imageBuildId :: ImageId, preparationSha256 :: Fingerprint, dependency :: Maybe FileFingerprint}
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


data LoggedCommand = LoggedCommand {command :: GuestCommand, path :: FilePath, limitBytes :: Word32}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data Command = Resolve Text | Create Sandbox | Execute GuestCommand | CopyFile Copy | Checkpoint FilePath | InspectCheckpoint FilePath | Resume (Sandbox, FilePath) | GetIdentity () | ExecuteLogged LoggedCommand | Close ()
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data Execution = Execution {exitCode :: Int, stdout :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data CheckpointData = CheckpointData {createdAt :: UTCTime, guestAssetBuildId :: ImageId}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


newtype Identifier (kind :: Symbol) = Identifier UUID
  deriving stock (Eq, Generic)
  deriving newtype (AE.FromJSON, AE.ToJSON)


data RegressionOptions = RegressionOptions {artifactDir :: FilePath, checkout :: FilePath, image :: Text, bridge :: FilePath}


data Artifact = Artifact {script :: Text, testPatch :: Maybe Text, failureMessage :: Text}
  deriving stock (Generic)


data TestFile = TestFile {path :: Text, content :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


instance AE.FromJSON TestFile where
  parseJSON = AE.withObject "Regression file" \fields -> do
    file <- TestFile <$> fields AE..: "path" <*> fields AE..: "content"
    let parts = T.splitOn "/" file.path
    unless (not (T.null file.path) && T.length file.path <= 4096 && not (isAbsolute $ toString file.path) && all (\part -> T.toCaseFold part `notElem` ["", ".", "..", ".git"]) parts && not (T.any (\c -> isControl c || c == '\\') file.path)) $ fail "Test files require ordinary repository-relative paths"
    unless (BS.length (encodeUtf8 file.content) <= 262144) $ fail "Test file exceeds its byte limit"
    pure file


-- $setup
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.ByteString.Lazy qualified as LBS
-- >>> import Data.Text qualified as T
-- >>> import Relude


-- | Complete, bounded file contents with safe, unique repository-relative names.
-- >>> let files :: [(Text, Text)] -> LBS.ByteString; files entries = AE.encode [AE.object ["path" AE..= path, "content" AE..= content] | (path, content) <- entries]
-- >>> let original = AE.eitherDecode @TestFiles (files [("test/a.sh", "*** End Patch\n")])
-- >>> fmap AE.toJSON (original >>= AE.eitherDecode @TestFiles . AE.encode) == fmap AE.toJSON original
-- True
-- >>> map (isLeft . AE.eitherDecode @TestFiles . files) [[], [("../x", "")], [(".git/config", "")], [("a/../b", "")], [("a", ""), ("a", "")]]
-- [True,True,True,True,True]
-- >>> isLeft $ AE.eitherDecode @TestFiles (files [("a", T.replicate 131073 "λ")])
-- True
-- >>> isLeft $ AE.eitherDecode @TestFiles (files [("a", T.replicate 131073 "x"), ("b", T.replicate 131073 "x")])
-- True
-- >>> isLeft $ AE.eitherDecode @TestFiles (files [(show n, "") | n <- [1 :: Int .. 101]])
-- True
newtype TestFiles = TestFiles (NonEmpty TestFile)
  deriving newtype (AE.ToJSON)


instance AE.FromJSON TestFiles where
  parseJSON value = do
    files <- AE.parseJSON @(NonEmpty TestFile) value
    unless (length files <= 100 && length (ordNub $ map (.path) $ toList files) == length files && sum (fmap (BS.length . encodeUtf8 . (.content)) files) <= 262144) $ fail "Supply unique test files within the file and byte limits"
    pure $ TestFiles files


data ArtifactInput = ExistingArtifact Artifact | FileArtifact Text TestFiles Text


instance AE.FromJSON ArtifactInput where
  parseJSON = AE.withObject "Frozen regression" \fields -> do
    script <- fields AE..: "script"
    failure <- fields AE..: "failureMessage"
    let bounded limit text = not (T.null $ T.strip text) && T.length text <= limit
    unless (bounded 65536 script && bounded 200 failure) $ fail "Invalid regression artifact"
    case KM.lookup "testFiles" fields of
      Just value -> do
        when (KM.member "testPatch" fields) $ fail "Supply test files or a test patch, not both"
        FileArtifact script <$> AE.parseJSON value <*> pure failure
      Nothing -> do
        patch <- fields AE..:? "testPatch"
        unless (all (bounded 262144) patch) $ fail "Invalid regression artifact"
        pure $ ExistingArtifact $ Artifact script patch failure


data Snapshot = Snapshot {path :: FilePath, sha256 :: Fingerprint}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Baseline = Pristine | PreparedCheckpoint Snapshot
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Side = Base | Head
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.FromJSON, AE.ToJSON) via WrappedEnumSC 'Nothing "" Side


data Revisions = BaseOnly Revision | HeadOnly Revision | Compare Revision Revision


instance AE.FromJSON Revisions where
  parseJSON value =
    AE.parseJSON @[(Side, Revision)] value >>= \case
      [(Base, baseline)] -> pure $ BaseOnly baseline
      [(Head, candidate)] -> pure $ HeadOnly candidate
      [(Base, baseline), (Head, candidate)] -> pure $ Compare baseline candidate
      _ -> fail "Expected pinned baseline, candidate, or ordered baseline/candidate comparison"


data RegressionRequest artifact = RegressionRequest {projectId :: ProjectId, runId :: Identifier "run", attemptId :: Identifier "attempt", sessionId :: Identifier "session", base :: Revision, imageBuildId :: ImageId, workspace :: Baseline, artifact :: artifact, revisions :: Revisions, ownerPid :: Maybe Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data ReportState = Running | Completed | Incomplete
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" ReportState


data CheckState = Passed | Failed | Blocked | Invalidated | TimedOut | OutputLimit | CleanupFailed
  deriving stock (Eq, Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" CheckState


data Placement = Injected | Retained
  deriving stock (Generic, Read, Show)
  deriving (AE.ToJSON) via WrappedEnumSC 'Nothing "" Placement


data LogExit = Exited Int | LogLimit
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Resources = Resources {cpus :: Word16, memoryMiB :: Word32}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data ExecutionKey = ExecutionKey {id :: Identifier "execution", side :: Side, revision :: Revision}


data Measurement = Measurement {status :: CheckState, exitCode :: Maybe Int, configuredResources :: Resources, resourceCostStatus :: ResourceCost, durationMs :: Double, logBytes :: Word32, logSha256 :: Fingerprint, tail :: Text, logPath :: Maybe Text, vmId :: Maybe Text, reason :: Maybe Text, sourceBefore :: Maybe Fingerprint, sourceAfter :: Maybe Fingerprint, testFilesSha256 :: Maybe Fingerprint, testPatchDisposition :: Maybe Placement}


data CheckExecution = Pending ExecutionKey | Measured ExecutionKey Measurement


instance AE.ToJSON CheckExecution where
  toJSON execution = AE.object $ case execution of
    Pending key -> fields key <> ["status" AE..= ("starting" :: Text), "exitCode" AE..= AE.Null]
    Measured key value ->
      fields key
        <> [ "status" AE..= value.status
           , "exitCode" AE..= value.exitCode
           , "configuredResources" AE..= value.configuredResources
           , "resourceCostStatus" AE..= value.resourceCostStatus
           , "durationMs" AE..= value.durationMs
           , "logBytes" AE..= value.logBytes
           , "logSha256" AE..= value.logSha256
           , "tail" AE..= value.tail
           , "logPath" AE..= value.logPath
           , "vmId" AE..= value.vmId
           , "reason" AE..= value.reason
           , "sourceBefore" AE..= value.sourceBefore
           , "sourceAfter" AE..= value.sourceAfter
           , "testFilesSha256" AE..= value.testFilesSha256
           , "testPatchDisposition" AE..= value.testPatchDisposition
           ]
    where
      fields key = ["id" AE..= key.id, "side" AE..= key.side, "revision" AE..= key.revision]


data RegressionReport = RegressionReport {id :: Identifier "check", projectId :: ProjectId, runId :: Identifier "run", attemptId :: Identifier "attempt", imageBuildId :: ImageId, workspace :: Maybe Fingerprint, createdAt :: UTCTime, scriptSha256 :: Fingerprint, testPatchSha256 :: Maybe Fingerprint, regressionSha256 :: Fingerprint, failureMessage :: Text, status :: ReportState, results :: [CheckExecution]}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data RegressionKind = ScriptCheck {sha256 :: Fingerprint} | InjectedTest {sha256 :: Fingerprint} | RetainedTest {sha256 :: Fingerprint}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data RegressionReceipt = RegressionReceipt {id :: Identifier "execution", attemptId :: Identifier "attempt", checkId :: Identifier "check", side :: Side, revision :: Revision, imageBuildId :: ImageId, workspace :: Maybe Fingerprint, scriptSha256 :: Fingerprint, regression :: Maybe RegressionKind, status :: CheckState, exitCode :: Maybe Int, durationMs :: Word64, cpus :: Word16, memoryMiB :: Word32, logBytes :: Word32, logSha256 :: Fingerprint}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data RegressionResult = RegressionResult {report :: RegressionReport, receipts :: [RegressionReceipt], isError :: Bool}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


data RegressionError = ComparisonBudgetExhausted | RegressionUnavailable | TestFilesRejected TestPatchError
  deriving stock (Show)
  deriving anyclass (Exception)


regressionErrorText :: RegressionError -> Text
regressionErrorText = \case
  ComparisonBudgetExhausted -> "Local check comparison budget is exhausted"
  RegressionUnavailable -> "Native regression could not produce retained evidence"
  TestFilesRejected failure -> testPatchErrorText failure


data TestPatchError = InvalidTestTarget | TestPatchTooLarge | TestPatchUnavailable
  deriving stock (Eq, Show)


testPatchErrorText :: TestPatchError -> Text
testPatchErrorText = \case
  InvalidTestTarget -> "Supply only new or changed regular test files; unchanged files, symlinks and directories cannot be frozen through testFiles"
  TestPatchTooLarge -> "Generated regression patch exceeds its 256 KiB byte limit"
  TestPatchUnavailable -> "Git could not generate the regression patch"


renderTestPatch :: FilePath -> Text -> TestFiles -> IO (Either TestPatchError Text)
renderTestPatch checkout base (TestFiles files) = do
  environment <- getEnvironment
  outcome <- tryAny $ withSystemTempDirectory "monoscope-test-index" \temporary -> runExceptT do
    let reject :: TestPatchError -> ExceptT TestPatchError IO value
        reject = hoistEither . Left
        git directory input args = ExceptT $ withProcessTerm (setStdin (byteStringInput $ toLazy input) $ setStdout createPipe $ HostGit.command environment HostGit.Local directory $ ["--literal-pathspecs", "-c", "core.ignoreCase=false"] <> args) \process -> do
          bytes <- BS.hGet (getStdout process) 262145
          if BS.length bytes > 262144
            then pure $ Left TestPatchTooLarge
            else do
              status <- waitExitCode process
              pure $ if status == ExitSuccess then Right bytes else Left TestPatchUnavailable
        bare = temporary </> "index.git"
    -- This private bare index never executes source or exposes alternates to a guest.
    void $ git temporary "" ["clone", "--bare", "--shared", "--", checkout, bare]
    void $ git bare "" ["read-tree", toString base]
    for_ files \file -> do
      entry <- git bare "" ["ls-files", "--stage", "-z", "--", toString file.path]
      (mode, previous) <- case filter (not . T.null) $ T.splitOn "\0" $ decodeUtf8 @Text entry of
        [] -> pure ("100644", Nothing)
        [row] -> case T.splitOn "\t" row of
          [metadata, name] | name == file.path -> case words metadata of
            [mode, blob, "0"] | mode `elem` ["100644", "100755"] -> pure (toString mode, Just blob)
            _ -> reject InvalidTestTarget
          _ -> reject InvalidTestTarget
        _ -> reject InvalidTestTarget
      blob <- T.strip . decodeUtf8 @Text <$> git bare (encodeUtf8 file.content) ["hash-object", "-w", "--stdin"]
      when (previous == Just blob) $ reject InvalidTestTarget
      void $ git bare "" ["update-index", "--add", "--cacheinfo", mode, toString blob, toString file.path]
    bytes <- git bare "" ["diff", "--cached", "--binary", "--no-ext-diff", "--no-textconv", "--no-color", "--no-relative", "--src-prefix=a/", "--dst-prefix=b/", toString base, "--"]
    hoistEither $ if BS.null bytes then Left InvalidTestTarget else first (const InvalidTestTarget) $ decodeUtf8' bytes
  pure $ join $ first (const TestPatchUnavailable) outcome


data SandboxFailure = SandboxCleanupFailed | SandboxOwnerLost
  deriving stock (Show)
  deriving anyclass (Exception)


data Workspace :: Effect where
  MaintainWorkspaces :: FilePath -> Workspace m (Either Text MaintenanceReport)
  PruneWorkspaces :: PruneOptions -> FilePath -> Workspace m (Either Text PruneReport)
  RunWorkspace :: Options -> Operation -> FilePath -> Workspace m (Either Text (Maybe Prepared))
  WithWorkspace :: Options -> FilePath -> (Maybe Leased -> m a) -> Workspace m (Either Text a)
  RunRegression :: RegressionOptions -> FilePath -> Workspace m (Either RegressionError RegressionResult)


type instance DispatchOf Workspace = 'Dynamic


makeEffect ''Workspace


runWorkspaceTest :: (Options -> Operation -> FilePath -> Either Text (Maybe Prepared)) -> (RegressionOptions -> FilePath -> Either RegressionError RegressionResult) -> (PruneOptions -> FilePath -> Either Text PruneReport) -> (FilePath -> Either Text MaintenanceReport) -> Eff (Workspace ': es) a -> Eff es a
runWorkspaceTest respond respondRegression respondPrune respondMaintenance = interpret $ \env -> \case
  MaintainWorkspaces config -> pure $ respondMaintenance config
  PruneWorkspaces options requestFile -> pure $ respondPrune options requestFile
  RunWorkspace options operation requestFile -> pure $ respond options operation requestFile
  WithWorkspace options requestFile action -> case respond options Lookup requestFile of
    Left message -> pure $ Left message
    Right prepared -> localSeqUnlift env $ \unlift -> Right <$> unlift (action $ fmap (\value -> Leased value (value.path <> ".resume")) prepared)
  RunRegression options requestFile -> pure $ respondRegression options requestFile


runWorkspaceReal :: IOE :> es => Eff (Workspace ': es) a -> Eff es a
runWorkspaceReal = interpret $ \env -> \case
  MaintainWorkspaces config -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) $ maintain =<< decode @MaintenanceOptions =<< readBounded config 65536
    pure $ case outcome of Right (Just report) -> Right report; _ -> Left "Workspace maintenance could not complete"
  PruneWorkspaces options requestFile -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) do
      request <- decode @PruneRequest =<< readBounded requestFile 65536
      withOwner request.ownerPid $ prune RetainUnarchived options request
    pure $ case outcome of Right (Just result) -> Right result; _ -> Left "Checkpoint pruning could not complete"
  RunWorkspace options operation requestFile -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) do
      request <- decode =<< readBounded requestFile 65536
      withOwner request.ownerPid $ case operation of
        Lookup -> do
          root <- canonicalizePath options.cacheDir
          withProjectLock root request.projectId $ workspace options operation request
        _ -> workspace options operation request
    pure $ case outcome of Right (Just prepared) -> Right prepared; _ -> Left "Workspace preparation or integrity verification failed"
  WithWorkspace options requestFile action -> localSeqUnliftIO env $ \unlift -> liftIO do
    outcome <- tryAny do
      request <- decode =<< readBounded requestFile 65536
      withOwner request.ownerPid do
        root <- canonicalizePath options.cacheDir
        bracket
          ( withProjectLock root request.projectId do
              prepared <- workspace options Lookup request
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
          (tryAny . unlift . action)
    case outcome of
      Left _ -> pure $ Left "Workspace preparation or integrity verification failed"
      Right result -> Right <$> either throwIO pure result
  RunRegression options requestFile -> liftIO do
    outcome <- tryAny $ Timeout.timeout (420 * 1000000) do
      request <- decode =<< readBounded requestFile (2 * 1048576)
      withOwner request.ownerPid do
        artifact <- case request.artifact of
          ExistingArtifact value -> pure value
          FileArtifact script files failure -> do
            let Revision base = request.base
            patch <- renderTestPatch options.checkout base files >>= either (throwIO . TestFilesRejected) pure
            pure $ Artifact script (Just patch) failure
        verifyRegression options request{artifact}
    pure $ case outcome of
      Right (Just result) -> Right result
      Left failure -> Left $ fromMaybe RegressionUnavailable $ fromException failure
      _ -> Left RegressionUnavailable
  where
    maintain :: MaintenanceOptions -> IO MaintenanceReport
    maintain options = do
      createDirectoryIfMissing True options.cacheDir
      root <- canonicalizePath options.cacheDir
      canonical root
      withFileLock (root </> ".maintenance.lock") Exclusive \_ -> do
        names <- listDirectory root
        let project name = do
              uuid <- UUID.fromText $ toText name
              guard $ projectText (ProjectId uuid) == name
              pure $ ProjectId uuid
        recognized <- filterM (doesDirectoryExist . (</> "workspaces") . (root </>) . projectText) $ mapMaybe project names
        before <- allocated root
        recovery <- forM recognized \pid -> do
          outcome <- tryAny $ recoverStages root options.remote pid
          pure (pid, outcome)
        usedAfterRecovery <- allocated root
        initial <- if usedAfterRecovery <= options.maxBytes then pure [] else forM recognized \pid -> (pid,) <$> allocated (root </> projectText pid)
        (reports, failed, _) <-
          foldM
            ( \accumulator@(reports, failed, used) (pid, _) ->
                if used <= options.maxBytes
                  then pure accumulator
                  else do
                    let parent = root </> projectText pid
                    outcome <- tryAny do
                      canonical parent
                      Posix.getSymbolicLinkStatus parent >>= ensure . Posix.isDirectory
                      leases <- doesDirectoryExist (parent </> "leases")
                      bytes <- measureAllocation $ (parent </> "workspaces") :| [parent </> "leases" | leases]
                      let target = fromInteger $ max 0 (toInteger bytes - max 0 (toInteger used - toInteger options.maxBytes))
                      prune ArchiveIdle (PruneOptions root options.remote) (PruneRequest pid target Nothing)
                    after <- allocated root
                    pure $ case outcome of
                      Right report -> (report : reports, failed, after)
                      Left _ -> (reports, pid : failed, after)
            )
            ([], [pid | (pid, Left _) <- recovery] <> [action.projectId | (_, Right actions) <- recovery, action <- actions, StageRecoveryFailed <- [action.outcome]], usedAfterRecovery)
            (sortOn (negate . toInteger . snd) initial)
        after <- allocated root
        pure $ MaintenanceReport before after options.maxBytes (reverse reports) (reverse failed) [name | name <- names, name /= ".maintenance.lock", maybe True (`notElem` recognized) $ project name] (concat [actions | (_, Right actions) <- recovery])
      where
        allocated path = measureAllocation $ one path

    stageKind :: StagePhase -> StageKind
    stageKind = \case
      StageDownload -> DownloadStage
      StageReclaiming proof -> kind proof
      StageClosed value -> value
      StageRecovered proof -> kind proof
      where
        kind = \case PartialDownload -> DownloadStage; ArchivedDownload _ -> DownloadStage; ArchivedEviction _ -> EvictionStage

    stagePrefix :: StageKind -> FilePath
    stagePrefix = \case DownloadStage -> ".restore-"; EvictionStage -> ".evict-"

    stagePaths :: FilePath -> StageRecord -> Stage
    stagePaths root record =
      let Identifier identifier = record.id
          name = toString $ UUID.toText identifier
          project = root </> projectText record.identity.projectId
       in Stage record (project </> "workspaces" </> (stagePrefix (stageKind record.phase) <> name)) (project </> "stages" </> (name <> ".json"))

    withStage :: FilePath -> StageOperation -> (FilePath -> IO a) -> IO a
    withStage root operation action = do
      identifier <- Identifier <$> UUID4.nextRandom
      now <- getCurrentTime
      let (identity, phase) = case operation of
            DownloadOperation value -> (value, StageDownload)
            EvictionOperation verified -> (verified.identity, StageReclaiming $ ArchivedEviction verified.snapshot)
          staged = stagePaths root $ StageRecord identifier identity phase now now Unpriced
          records = takeDirectory staged.journal
      createDirectoryIfMissing True records
      canonical records
      Posix.setFileMode records 0o700
      sync $ takeDirectory records
      bracket
        (do publish staged.journal staged.record; createDirectory staged.path; Posix.setFileMode staged.path 0o700; sync $ takeDirectory staged.path; pure staged)
        ( \value -> case operation of
            DownloadOperation _ -> do
              removePathForcibly value.path
              saveStage value $ StageClosed DownloadStage
            EvictionOperation verified -> reclaimStage value $ ArchivedEviction verified
        )
        (action . (.path))

    saveStage :: Stage -> StagePhase -> IO ()
    saveStage staged phase = do
      now <- getCurrentTime
      publish staged.journal staged.record{phase = phase, updatedAt = now}

    reclaimStage :: Stage -> Reclamation VerifiedRemoteCopy -> IO ()
    reclaimStage staged proof = do
      for_ proof $ ensure . (== staged.record.identity) . (.identity)
      let saved = fmap (.snapshot) proof
      saveStage staged $ StageReclaiming saved
      removePathForcibly staged.path
      sync $ takeDirectory staged.path
      saveStage staged $ StageRecovered saved

    recoverStages :: FilePath -> FilePath -> ProjectId -> IO [StageRecovery]
    recoverStages root remoteFile pid = do
      let parent = root </> projectText pid </> "workspaces"
          records = root </> projectText pid </> "stages"
      present <- doesDirectoryExist records
      if not present
        then pure []
        else do
          canonical parent
          canonical records
          result <- withTryFileLock (parent </> ".native.lock") Exclusive \_ -> do
            names <- listDirectory parent
            forM (mapMaybe parseName names) \(kind, identifier) -> do
              let Identifier uid = identifier
                  journal = records </> toString (UUID.toText uid) <> ".json"
              attempted <- tryAny do
                record <- decode @StageRecord =<< readBounded journal 65536
                ensure $ record.id == identifier && record.identity.projectId == pid && stageKind record.phase == kind
                let staged = stagePaths root record
                canonical staged.path
                Posix.getSymbolicLinkStatus staged.path >>= ensure . Posix.isDirectory
                let content = if kind == EvictionStage then staged.path </> "entry" else staged.path
                entries <- listDirectory staged.path
                when (kind == EvictionStage) $ ensure $ all (== "entry") entries
                files <-
                  if kind == EvictionStage && null entries
                    then pure []
                    else do
                      canonical content
                      listDirectory content
                ensure $ all (`elem` ["baseline.qcow2", "manifest.json", "last-used.json"]) files
                for_ files \file -> do
                  status <- Posix.getSymbolicLinkStatus $ content </> file
                  ensure $ Posix.isRegularFile status && Posix.linkCount status == 1
                selected <- case record.phase of
                  StageClosed _ -> invalid
                  StageRecovered _ -> invalid
                  StageReclaiming proof -> pure proof
                  StageDownload ->
                    if "manifest.json" `elem` files
                      then do
                        manifest <- decode @Manifest =<< readBounded (content </> "manifest.json") 65536
                        ensure $ manifest.identity == record.identity
                        pure $ ArchivedDownload $ manifestFingerprint manifest
                      else pure PartialDownload
                case selected of
                  PartialDownload -> ensure $ "manifest.json" `notElem` files
                  ArchivedDownload _ -> pass
                  ArchivedEviction _ -> pass
                for_ selected \expected -> do
                  when ("baseline.qcow2" `elem` files) $ fingerprint (content </> "baseline.qcow2") >>= ensure . (== expected)
                  when ("manifest.json" `elem` files) do
                    manifest <- decode @Manifest =<< readBounded (content </> "manifest.json") 65536
                    ensure $ manifest.identity == record.identity && manifestFingerprint manifest == expected
                archived <- tryAny $ traverse (\expected -> do remote <- readRemote remoteFile; connection <- connectStorage remote; verifyRemote remote connection record.identity expected) selected
                case archived of
                  Left _ -> pure $ StageRetained ArchiveUnavailable
                  Right verified -> do
                    reclaimed <- tryAny $ reclaimStage staged verified
                    pure $ either (const StageRecoveryFailed) (const $ StageRemoved selected) reclaimed
              pure $ StageRecovery pid identifier $ either (const $ StageRetained InvalidEntry) Relude.id attempted
          pure $ fromMaybe [] result
      where
        parseName name =
          viaNonEmpty
            head
            [(kind, Identifier identifier) | kind <- [DownloadStage, EvictionStage], Just text <- [T.stripPrefix (toText $ stagePrefix kind) $ toText name], Just identifier <- [UUID.fromText text], UUID.toText identifier == text]

    allocatedSizes :: NonEmpty FilePath -> IO [Word64]
    allocatedSizes paths = do
      output <- decodeUtf8 @Text <$> readProcessStdout_ (proc "du" $ "-sk" : toList paths)
      forM (lines output) \line -> do
        size <- maybe invalid pure $ viaNonEmpty head (words line) >>= readMaybe @Word64 . toString
        ensure $ size <= maxBound `div` 1024
        pure $ size * 1024

    measureAllocation :: NonEmpty FilePath -> IO Word64
    measureAllocation paths = do
      bytes <- sum . map toInteger <$> allocatedSizes paths
      ensure $ bytes <= toInteger (maxBound :: Word64)
      pure $ fromInteger bytes

    verifyRemote :: Remote -> Minio.MinioConn -> Identity -> FileFingerprint -> IO VerifiedRemoteCopy
    verifyRemote remote connection identity expected = do
      let prefix = "agent-workspaces/v1/" <> toText (projectText identity.projectId) <> "/" <> digestText (identityKey identity) <> "/" <> digestText expected.sha256
      ensure $ expected.bytes <= remote.maxBytes
      encoded <- storage connection do
        response <- Minio.getObject remote.bucket (prefix <> ".json") Minio.defaultGetObjectOptions
        runConduit $ response.gorObjectStream .| CC.takeE 65537 .| CC.sinkLazy
      ensure $ LBS.length encoded <= 65536
      archived <- decode @Manifest $ toStrict encoded
      ensure $ archived.identity == identity && manifestFingerprint archived == expected
      (context, size) <- storage connection do
        response <- Minio.getObject remote.bucket (prefix <> ".qcow2") Minio.defaultGetObjectOptions
        liftIO $ ensure $ response.gorObjectInfo.oiSize == fromIntegral expected.bytes
        runConduit $ response.gorObjectStream .| CC.takeE (fromIntegral expected.bytes + 1) .| CC.foldl (\(!context, !count) chunk -> (hashUpdate context chunk, count + fromIntegral (BS.length chunk))) (hashInit :: Context SHA256, 0 :: Word64)
      ensure $ size == expected.bytes && Fingerprint (show $ hashFinalize context) == expected.sha256
      pure $ VerifiedRemoteCopy identity expected

    manifestFingerprint :: Manifest -> FileFingerprint
    manifestFingerprint manifest = FileFingerprint manifest.sha256 manifest.bytes

    prune :: ArchivePolicy -> PruneOptions -> PruneRequest -> IO PruneReport
    prune archivePolicy options request = do
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
              sizes <- allocatedSizes $ parent :| [leases | present]
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
        let journals = project </> "prunes"
        createDirectoryIfMissing True journals
        canonical journals
        Posix.setFileMode journals 0o700
        identifier <- Identifier <$> UUID4.nextRandom
        let record = PruneRecord identifier request.projectId now PruneStarted
            Identifier uid = identifier
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
                              proof <- tryAny do
                                archived <- tryAny $ verifyRemote remote connection entry.manifest.identity (manifestFingerprint entry.manifest)
                                case (archived, archivePolicy) of
                                  (Right verified, _) -> pure verified
                                  (Left failure, RetainUnarchived) -> throwIO failure
                                  (Left _, ArchiveIdle) -> do
                                    void $ transfer root entry.manifest.identity Upload options.remote \remote' prefix connection' -> uploadCheckpoint remote' prefix connection' $ Prepared{manifest = entry.manifest, path = directory entry.key </> "baseline.qcow2"}
                                    verifyRemote remote connection entry.manifest.identity (manifestFingerprint entry.manifest)
                              case proof of
                                Left _ -> pure $ Kept entry.key ArchiveUnavailable
                                Right verified -> do
                                  removed <- tryAny $ withStage root (EvictionOperation verified) \temporary -> do
                                    let retired = temporary </> "entry"
                                    renameDirectory (directory entry.key) retired
                                    sync parent
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

    verifyRegression :: RegressionOptions -> RegressionRequest Artifact -> IO RegressionResult
    verifyRegression options request = do
      let artifact = request.artifact
          revisions = case request.revisions of BaseOnly baseline -> (Base, baseline) :| []; HeadOnly candidate -> (Head, candidate) :| []; Compare baseline candidate -> (Base, baseline) :| [(Head, candidate)]
          revisionText (Revision value) = value
          identifierText (Identifier value) = toString $ UUID.toText value
          privateWrite path bytes = writeFileBS path bytes >> Posix.setFileMode path 0o600
      ensure $ isAbsolute options.checkout && isAbsolute options.artifactDir
      checkout <- canonicalizePath options.checkout
      environment <- getEnvironment
      let hostGit args = readProcessStdout_ $ HostGit.command environment HostGit.Local checkout args
      headRevision <- T.strip . decodeUtf8 <$> hostGit ["rev-parse", "HEAD"]
      for_ revisions \(side, revision) -> ensure $ revisionText revision == (if side == Base then revisionText request.base else headRevision)
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
        checkId <- Identifier <$> UUID4.nextRandom
        created <- getCurrentTime
        let directory = root </> identifierText checkId
            workspaceHash = case request.workspace of Pristine -> Nothing; PreparedCheckpoint snapshot -> Just snapshot.sha256
            template = RegressionReport checkId request.projectId request.runId request.attemptId request.imageBuildId workspaceHash created (sha $ encodeUtf8 artifact.script) (sha . encodeUtf8 <$> artifact.testPatch) (sha $ toStrict $ Encoding.encodingToLazyByteString $ AE.genericToEncoding AE.defaultOptions artifact) artifact.failureMessage Running []
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
          for_ revisions \(side, revision) -> do
            released <- all ((/= CleanupFailed) . (.status)) <$> readIORef receipts
            when released do
              key <- (\identifier -> ExecutionKey (Identifier identifier) side revision) <$> UUID4.nextRandom
              modifyIORef' executions (<> [Pending key])
              persist Running
              started <- getMonotonicTimeNSec
              observation <- newIORef $ Measurement Blocked Nothing (Resources 2 1024) Unpriced 0 0 (sha "") "" Nothing Nothing (Just "Check did not complete with usable execution evidence") Nothing Nothing Nothing Nothing
              let logPath = directory </> (if side == Base then "base.log" else "head.log")
                  change f = modifyIORef' observation f >> readIORef observation >>= update key
                  checked :: RPC -> NonEmpty Text -> IO Text
                  checked = checkedWith "Verifier preparation failed"
                  checkedWith :: Text -> RPC -> NonEmpty Text -> IO Text
                  checkedWith message rpc argv = do
                    result <- rpc @Execution $ Execute $ GuestCommand argv (Just "/") False
                    unless (result.exitCode == 0) do
                      change $ \measured -> measured{reason = Just message}
                      invalid
                    pure result.stdout
                  copy :: RPC -> FilePath -> Text -> IO ()
                  copy rpc path destination = rpc @() $ CopyFile $ Copy path destination
              privateWrite logPath ""
              outcome <- tryAny $ Timeout.timeout (120 * 1000000) $ withSystemTempDirectory "monoscope-verifier-snapshot" $ \private -> withSandbox options.bridge \rpc -> do
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
                vm <- rpc @Text $ GetIdentity ()
                change $ \measured -> measured{vmId = Just vm}
                let revisionArgs = ["-c", "safe.directory=/workspace", "-c", "core.hooksPath=/dev/null", "-C", "/workspace"]
                    guestGit args = "/usr/bin/git" :| (revisionArgs <> args)
                case request.workspace of
                  Pristine -> for_ [("/bin/mkdir" :: Text) :| ["-p", "/workspace"], "/bin/cp" :| ["-R", "/checkout/.", "/workspace"]] $ void . checked rpc
                  PreparedCheckpoint _ -> do
                    for_ ["/workspace", "/checkout/.git"] $ \path -> void $ checked rpc $ "/usr/bin/git" :| ["config", "--system", "--add", "safe.directory", path]
                    void $ checked rpc $ guestGit ["fetch", "--quiet", "--no-tags", "--", "/checkout", revisionText revision]
                void $ checked rpc $ guestGit ["checkout", "--quiet", "--detach", revisionText revision]
                for_ artifact.testPatch \_ -> do
                  copy rpc (directory </> "test.patch") "/run/monoscope-test.patch"
                  stats <- checkedWith "Could not inspect the retained Git diff. Supply a unified Git patch with diff --git, ---/+++ and @@ headers; apply_patch markers are unsupported. No repository assertion ran." rpc $ guestGit ["apply", "--recount", "--numstat", "-z", "/run/monoscope-test.patch"]
                  paths <- traverse (\entry -> case T.splitOn "\t" entry of _ : _ : path | not (null path) -> pure $ T.intercalate "\t" path; _ -> invalid) $ filter (not . T.null) $ T.splitOn "\0" stats
                  ensure $ not (null paths) && length paths <= 100
                  privateWrite (directory </> "test-paths") $ encodeUtf8 $ T.intercalate "\0" paths <> "\0"
                  copy rpc (directory </> "test-paths") "/run/monoscope-test-paths"
                  applicable <- rpc @Execution $ Execute $ GuestCommand (guestGit ["apply", "--recount", "--index", "--check", "/run/monoscope-test.patch"]) (Just "/") False
                  placement <-
                    if applicable.exitCode == 0
                      then checked rpc (guestGit ["apply", "--recount", "--index", "/run/monoscope-test.patch"]) $> Injected
                      else do
                        retained <- rpc @Execution $ Execute $ GuestCommand (guestGit ["apply", "--recount", "--index", "--reverse", "--check", "/run/monoscope-test.patch"]) (Just "/") False
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
                result <- rpc @LogExit $ ExecuteLogged $ LoggedCommand (GuestCommand ("/usr/bin/setpriv" :| ["--reuid=1000", "--regid=1000", "--clear-groups", "--no-new-privs", "--reset-env", "--", "/usr/bin/env", "LANG=C.UTF-8", "LC_ALL=C.UTF-8", "/bin/bash", "--noprofile", "--norc", "/run/monoscope-regression.sh"]) (Just "/workspace") False) logPath 1048576
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
                Right Nothing -> change $ \measured -> measured{status = TimedOut}
                Left failure | Just SandboxCleanupFailed <- fromException failure -> do
                  publish counter (3 :: Word8)
                  change $ \measured -> measured{status = CleanupFailed}
                _ -> pass
              log <- readBounded logPath 1048576
              sync logPath
              ended <- getMonotonicTimeNSec
              change $ \measured -> measured{durationMs = fromIntegral (ended - started) / 1000000, logBytes = fromIntegral $ BS.length log, logSha256 = sha log, tail = decodeUtf8With lenientDecode $ BS.drop (max 0 $ BS.length log - 4096) log, logPath = Just $ "/run/monoscope-review-output/check-" <> toText (identifierText checkId) <> "-" <> (if side == Base then "base" else "head") <> ".log"}
              measured <- readIORef observation
              let kind = case artifact.testPatch of Nothing -> Just $ ScriptCheck template.regressionSha256; Just _ -> case measured.testPatchDisposition of Just Injected -> Just $ InjectedTest template.regressionSha256; Just Retained -> Just $ RetainedTest template.regressionSha256; Nothing -> Nothing
                  receipt = RegressionReceipt key.id request.attemptId checkId side revision request.imageBuildId workspaceHash template.scriptSha256 kind measured.status measured.exitCode (ceiling measured.durationMs) 2 1024 measured.logBytes measured.logSha256
              publish (takeDirectory root </> identifierText receipt.id <> ".json") $ AE.object ["tag" AE..= ("Check" :: Text), "contents" AE..= receipt]
              modifyIORef' receipts (<> [receipt])
          results <- readIORef executions
          saved <- readIORef receipts
          let report = template{status = if length results == length revisions then Completed else Incomplete, results}
          publish (directory </> "report.json") report
          pure $ RegressionResult report saved $ any (\receipt -> receipt.status `notElem` [Passed, Failed]) saved

    transfer :: FilePath -> Identity -> TransferDirection -> FilePath -> (Remote -> Text -> Minio.MinioConn -> IO Prepared) -> IO (Maybe Prepared)
    transfer root identity direction remoteFile action = do
      remote <- readRemote remoteFile
      let records = root </> projectText identity.projectId </> "transfers"
      createDirectoryIfMissing True records
      canonical records
      Posix.setFileMode records 0o700
      identifier <- Identifier <$> UUID4.nextRandom
      now <- getCurrentTime
      started <- getMonotonicTimeNSec
      let record = TransferRecord identifier identity direction now TransferStarted Unpriced
          Identifier transferId = identifier
          save outcome = publish (records </> toString (UUID.toText transferId) <> ".json") (record :: TransferRecord){outcome}
          elapsed = (\ended -> fromIntegral (ended - started) / 1000000) <$> getMonotonicTimeNSec
          prefix = "agent-workspaces/v1/" <> toText (projectText identity.projectId) <> "/" <> digestText (identityKey identity) <> "/"
      save TransferStarted
      connection' <- connectStorage remote
      value <- action remote prefix connection' `onException` (elapsed >>= save . TransferFailed)
      elapsed >>= save . TransferCompleted (FileFingerprint value.manifest.sha256 value.manifest.bytes)
      pure $ Just value

    diskKey :: Text -> Manifest -> Text
    diskKey prefix manifest = prefix <> (case manifest.sha256 of Fingerprint value -> value) <> ".qcow2"

    manifestKey :: Text -> Maybe Fingerprint -> Text
    manifestKey prefix = (prefix <>) . maybe "manifest.json" (\(Fingerprint digest) -> digest <> ".json")

    uploadCheckpoint :: Remote -> Text -> Minio.MinioConn -> Prepared -> IO Prepared
    uploadCheckpoint remote prefix connection prepared = do
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

    workspace :: Options -> Operation -> Request -> IO (Maybe Prepared)
    workspace options operation request = do
      scriptBytes <- readBounded options.script (4 * 65536)
      scriptText <- either (const invalid) pure $ decodeUtf8' scriptBytes
      ensure $ T.length scriptText <= 65536 && not (T.null $ T.strip scriptText)
      dependency <- traverse fingerprint options.dependencyArchive
      environment <- getEnvironment
      withSandbox options.bridge \rpc -> do
        image <- rpc @Image $ Resolve options.image
        for_ request.expectedImageBuildId $ ensure . (== image.buildId)
        assetDir <- canonicalizePath image.assetDir
        ensure $ assetDir == image.assetDir
        let Repository repoText = request.repository
            Revision baseText = request.base
            identity = Identity request.projectId request.repository request.base image.buildId (sha scriptBytes) dependency
            key = identityKey identity
        root <- canonicalizePath options.cacheDir
        let parent = root </> projectText request.projectId </> "workspaces"
            directory = parent </> toString (case key of Fingerprint value -> value)
            load = loadWorkspace identity image directory (rpc . InspectCheckpoint)
            locked = withProjectLock root request.projectId
        case operation of
          Lookup -> load
          Archive remoteFile -> locked $ transfer root identity Upload remoteFile \remote prefix connection -> do
            prepared <- maybe invalid pure =<< load
            uploadCheckpoint remote prefix connection prepared
          Restore remoteFile -> locked do
            saved <- load
            case saved of
              Just _ -> pure saved
              Nothing -> transfer root identity Download remoteFile \remote prefix connection -> withStage root (DownloadOperation identity) \temporary -> do
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
                      git args = readProcessStdout_ $ HostGit.command environment HostGit.Local source args
                      execute cwd quiet argv = do
                        result <- rpc @Execution $ Execute $ GuestCommand argv cwd quiet
                        ensure $ result.exitCode == 0
                        pure $ T.strip result.stdout
                      guestGit args = execute (Just "/workspace") False $ "/usr/bin/git" :| (["-c", "safe.directory=/workspace", "-c", "core.hooksPath=/dev/null", "-C", "/workspace"] <> args)
                      parseHash value = maybe invalid pure $ viaNonEmpty head (words value) >>= mkFingerprint
                      copy hostPath destination = rpc @() $ CopyFile $ Copy hostPath destination
                  void $ git ["init", "--quiet", clean]
                  void $ git ["-C", clean, "fetch", "--quiet", "--no-tags", "--", cleanCheckout, toString baseText]
                  void $ git ["-C", clean, "checkout", "--quiet", "--detach", toString baseText]
                  void $ git ["-C", clean, "remote", "add", "origin", toString repoText]
                  started <- getMonotonicTimeNSec
                  rpc @() $ Create $ Sandbox image.assetDir 2 1024 OfflineCow (one ("/baseline", clean))
                  for_ [("/bin/mkdir" :: Text) :| ["-p", "/workspace"], "/bin/cp" :| ["-R", "/baseline/.", "/workspace"], "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]] $ void . execute Nothing False
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
                  void $ execute (Just "/workspace") True $ "/usr/bin/setpriv" :| ["--reuid=1000", "--regid=1000", "--clear-groups", "--no-new-privs", "--reset-env", "--", "/usr/bin/env", "LANG=C.UTF-8", "LC_ALL=C.UTF-8", "/bin/bash", "--noprofile", "--norc", "/run/monoscope-prepare.sh"]
                  revision <- guestGit ["rev-parse", "HEAD"]
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


projectText :: ProjectId -> FilePath
projectText (ProjectId identifier) = toString $ UUID.toText identifier


digestText :: Fingerprint -> Text
digestText (Fingerprint value) = value


identityKey :: Identity -> Fingerprint
identityKey = sha . toStrict . Encoding.encodingToLazyByteString . AE.genericToEncoding AE.defaultOptions{AE.omitNothingFields = True}


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
  digest <- evaluate (show (hashlazy bytes :: Digest SHA256) :: Text)
  pure $ FileFingerprint (Fingerprint digest) size


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


sync :: FilePath -> IO ()
sync path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.cloexec = True}) Posix.closeFd fileSynchronise


sha :: ByteString -> Fingerprint
sha bytes = Fingerprint $ show (hash bytes :: Digest SHA256)


fingerprintScript :: Text
fingerprintScript =
  """
  set -euo pipefail
  while IFS= read -r -d '' file; do
    if [ -d "./$file" ] && [ ! -L "./$file" ]; then
      find -P "./$file" -print0 | LC_ALL=C sort -z
    else printf '%s\\0' "./$file"; fi
  done < "${1:-/run/monoscope-baseline-paths}" |
  while IFS= read -r -d '' file; do
    file=${file#./}
    printf '%s\\0' "$file"
    if [ -e "./$file" ] || [ -L "./$file" ]; then
      /usr/bin/stat -c '%f' -- "./$file"
      if [ -L "./$file" ]; then /usr/bin/readlink -z -- "./$file"; elif [ -d "./$file" ]; then printf 'directory\\0'; else /usr/bin/sha256sum -- "./$file"; fi
    else printf 'missing\\0'; fi
  done | /usr/bin/sha256sum
  """


type RPC = forall value. AE.FromJSON value => Command -> IO value


withSandbox :: FilePath -> (RPC -> IO a) -> IO a
withSandbox bridge action = do
  environment <- filter (\(key, _) -> key `elem` ["PATH", "HOME", "XDG_CACHE_HOME"] || "GONDOLIN_" `isPrefixOf` key) <$> getEnvironment
  withProcessTerm (setEnv environment $ setStdin createPipe $ setStdout createPipe $ setStderr nullStream $ proc bridge []) \process -> do
    let rpc :: forall value. AE.FromJSON value => Command -> IO value
        rpc command = do
          LBS.hPut (getStdin process) (AE.encode command <> "\n")
          hFlush $ getStdin process
          bytes <- B8.hGetLine $ getStdout process
          ensure $ BS.length bytes <= 1048576
          either (const invalid) (either (const invalid) pure) $ AE.eitherDecodeStrict @(Either Text value) bytes
        close = do
          closed <- Timeout.timeout (2 * 1000000) $ tryAny $ rpc @() $ Close ()
          case closed of Just (Right ()) -> pass; _ -> throwIO SandboxCleanupFailed
    result <- action rpc
    close
    pure result


publish :: AE.ToJSON value => FilePath -> value -> IO ()
publish path value = withTempFile (takeDirectory path) ".native-" \temporary handle -> do
  LBS.hPut handle $ AE.encode value <> "\n"
  hFlush handle
  sync temporary
  renameFile temporary path
  sync $ takeDirectory path


withOwner :: Maybe Word32 -> IO a -> IO a
withOwner supplied action = do
  parent <- fromIntegral <$> getParentProcessID
  let owner = fromMaybe parent supplied
      present = getParentProcessID >>= \current -> unless (current == fromIntegral owner) $ throwIO SandboxOwnerLost
  present
  thread <- myThreadId
  withAsync (catchAny (forever $ threadDelay 250000 >> present) $ throwTo thread) $ const action
