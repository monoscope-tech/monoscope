{-# LANGUAGE OrPatterns #-}

module Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), CandidateCause, CandidatePatch, mkCandidatePatch, CapturedCandidate, candidateMatchesPatch, candidateStep, candidateIdentity, candidateRevision, FixSummary, candidateResult, captureCandidate, CandidateSnapshot, candidateSnapshot, restoreCandidate, verifyReproduction, verifyComparison, retainAdmission, VerifiedArtifacts, artifactCheckout, peerPreparation, verifyArtifacts, withVerifiedArtifacts, readPrivateArtifact, writePrivateArtifact, sha256) where

import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isHexDigit)
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..))
import Pkg.GitCommand qualified as HostGit
import Relude
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, listDirectory, renameFile)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory, withTempFile)
import System.Posix.Files qualified as Posix
import System.Posix.IO qualified as Posix
import System.Posix.Unistd (fileSynchronise)
import System.Process.Typed (readProcessStdout_)
import UnliftIO.Exception (bracket, bracketOnError, throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


data ArtifactStore = ArtifactStore {usageDir :: FilePath, cacheDir :: FilePath}


data VerifiedArtifacts = VerifiedArtifacts FilePath ArtifactRequest ReviewEvidence


data ReviewEvidence = ReviewEvidence TestArtifact ByteString ByteString


artifactCheckout :: VerifiedArtifacts -> FilePath
artifactCheckout (VerifiedArtifacts directory _ _) = directory


peerPreparation :: VerifiedArtifacts -> Map Text ByteString
peerPreparation (VerifiedArtifacts _ request (ReviewEvidence regression baseLog headLog)) =
  Map.fromList
    [ ("/run/monoscope-fix-review.json", toStrict $ AE.encode $ AE.object ["result" AE..= request.result, "regression" AE..= regression, "baseline" AE..= request.baseline, "candidate" AE..= request.candidate])
    , ("/run/monoscope-fix-review-base.log", baseLog)
    , ("/run/monoscope-fix-review-head.log", headLog)
    ]


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


data Report rows = Report
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
  , results :: rows
  }
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


verifyArtifacts :: ArtifactStore -> ArtifactRequest -> IO (Either Text ())
verifyArtifacts store request = withVerifiedArtifacts store request $ const pass


withVerifiedArtifacts :: ArtifactStore -> ArtifactRequest -> (VerifiedArtifacts -> IO a) -> IO (Either Text a)
withVerifiedArtifacts store request action = do
  outcome <- tryAny $ withSystemTempDirectory "monoscope-native-artifacts" \directory -> do
    verified <- Timeout.timeout (180 * 1000000) do
      let result = request.result
          metadata = case AE.toJSON result of AE.Object fields -> AE.Object $ foldr KM.delete fields ["checkId", "regressionSha256", "summary"]; value -> value
      unless (not (T.null $ T.strip result.summary) && T.length result.summary <= 600 && result.checkId == request.candidate.checkId && (case request.candidate.regression of Just (RetainedTest value) -> value == result.regressionSha256; (Nothing; Just ScriptCheck{}; Just InjectedTest{}) -> False)) $ fail "Invalid retained fix result"
      captured <- either fail pure $ AET.parseEither AE.parseJSON metadata
      (checkout, evidence) <- verifyEvidence store request.repository captured request.baseline request.candidate directory
      pure $ VerifiedArtifacts checkout request evidence
    traverse action verified
  pure $ case outcome of Right (Just verified) -> Right verified; _ -> Left "Retained fix artifacts do not match the verified candidate"


verifyComparison :: ArtifactStore -> Text -> CapturedCandidate -> CheckReceipt -> CheckReceipt -> IO (Either Text ())
verifyComparison store repository (CapturedCandidate captured) baseline candidate = do
  outcome <- tryAny $ withSystemTempDirectory "monoscope-native-comparison" $ Timeout.timeout (180 * 1000000) . void . verifyEvidence store repository captured baseline candidate
  pure $ case outcome of Right (Just ()) -> Right (); _ -> Left "Retained comparison does not match its native execution"


verifyEvidence :: ArtifactStore -> Text -> CandidateManifest -> CheckReceipt -> CheckReceipt -> FilePath -> IO (FilePath, ReviewEvidence)
verifyEvidence store repository result baseline candidate directory = do
  ensure $ baseline.side == Base && baseline.status == Failed && maybe False (\code -> code > 0 && code `notElem` [126, 127]) baseline.exitCode
  ensure $ candidate.side == Head && candidate.status == Passed && candidate.exitCode == Just 0 && placement candidate.regression == Just Retained
  ensure $ baseline.revision == revisionText result.base && candidate.revision == revisionText result.head && result.base /= result.head && baseline.id /= candidate.id
  ensure $ (baseline.imageBuildId, baseline.workspace, baseline.scriptSha256, baseline.cpus, baseline.memoryMiB) == (candidate.imageBuildId, candidate.workspace, candidate.scriptSha256, candidate.cpus, candidate.memoryMiB)
  ensure $ isJust $ UUID.fromText baseline.imageBuildId
  (report, artifact, execution) <- readEvidence @(Execution, Execution) store (result.projectId, result.runId, result.attemptId) candidate
  let (baseRow, headRow) = report.results
  requireEvidence $ baseRow.testFilesSha256 == headRow.testFilesSha256
  (baseLog, headLog) <- (,) <$> verifiedLog execution report (baseRow, baseline) <*> verifiedLog execution report (headRow, candidate)
  checkout <- verifyCandidate store repository result directory
  pure (checkout, ReviewEvidence artifact baseLog headLog)
  where
    ensure = requireEvidence
    placement = regressionPlacement


readEvidence :: AE.FromJSON rows => ArtifactStore -> (UUIDId "project", UUIDId "issue_fix", UUIDId "agent_attempt") -> CheckReceipt -> IO (Report rows, TestArtifact, FilePath)
readEvidence store (project, run, attempt) receipt = do
  root <- canonicalizePath store.usageDir
  let directory = root </> "fixes" </> toString run.toText </> "checks" </> toString receipt.checkId.toText
  report <- readPrivateArtifact directory "report.json" 65536 >>= either fail pure . AE.eitherDecodeStrict
  requireEvidence $ (report.id, report.projectId, report.runId, report.attemptId) == (receipt.checkId, project, run, attempt)
  requireEvidence $ (report.imageBuildId, report.workspace, report.scriptSha256) == (receipt.imageBuildId, receipt.workspace, receipt.scriptSha256)
  requireEvidence $ regressionHash receipt.regression == Just report.regressionSha256
  case report.status of Completed -> pass
  script <- readPrivateArtifact directory "script.sh" 262144
  patch <- readPrivateArtifact directory "test.patch" 1048576
  let artifact = TestArtifact (decodeUtf8 script) (decodeUtf8 patch) report.failureMessage
  requireEvidence $ sha256 script == receipt.scriptSha256 && sha256 patch == report.testPatchSha256 && not (T.null report.failureMessage) && testArtifactHash artifact == report.regressionSha256
  pure (report, artifact, directory)


verifiedLog :: FilePath -> Report rows -> (Execution, CheckReceipt) -> IO ByteString
verifiedLog directory report (row, receipt) = do
  requireEvidence $ receipt.attemptId == report.attemptId && receipt.checkId == report.id && regressionHash receipt.regression == Just report.regressionSha256
  requireEvidence $ receipt.cpus > 0 && receipt.memoryMiB > 0 && receipt.logBytes <= 1048576
  requireEvidence $ (row.id, row.side, revisionText row.revision, row.status, row.exitCode, row.logBytes, row.logSha256) == (receipt.id, receipt.side, receipt.revision, receipt.status, receipt.exitCode, receipt.logBytes, receipt.logSha256)
  requireEvidence $ row.durationMs >= 0 && ceiling @Scientific @Integer row.durationMs == toInteger receipt.durationMs
  requireEvidence $ (row.configuredResources.cpus, row.configuredResources.memoryMiB) == (receipt.cpus, receipt.memoryMiB)
  requireEvidence $ digest row.sourceBefore && row.sourceBefore == row.sourceAfter && digest row.testFilesSha256 && Just row.testPatchDisposition == regressionPlacement receipt.regression
  output <- readPrivateArtifact directory (if receipt.side == Base then "base.log" else "head.log") 1048576
  requireEvidence $ BS.length output == fromIntegral receipt.logBytes && sha256 output == receipt.logSha256 && (receipt.side /= Base || encodeUtf8 report.failureMessage `BS.isInfixOf` output)
  pure output
  where
    digest value = T.length value == 64 && T.all (\c -> isHexDigit c && not (c >= 'A' && c <= 'F')) value


regressionPlacement :: Maybe Regression -> Maybe Placement
regressionPlacement = \case Just InjectedTest{} -> Just Injected; Just RetainedTest{} -> Just Retained; (Nothing; Just ScriptCheck{}) -> Nothing


regressionHash :: Maybe Regression -> Maybe Text
regressionHash = \case Just (InjectedTest value) -> Just value; Just (RetainedTest value) -> Just value; (Nothing; Just ScriptCheck{}) -> Nothing


requireEvidence :: Bool -> IO ()
requireEvidence condition = unless condition $ throwIO $ userError "Invalid retained fix evidence"


verifyReproduction :: ArtifactStore -> FixRequest -> TestArtifact -> CheckReceipt -> IO (Either Text ())
verifyReproduction store job artifact receipt = do
  outcome <- tryAny do
    let request = job.request
    requireEvidence $ receipt.side == Base && receipt.status == Failed && receipt.revision == request.base && receipt.imageBuildId == job.imageBuildId.toText && maybe False (\code -> code > 0 && code `notElem` [126, 127]) receipt.exitCode
    (report, retained, directory) <- readEvidence @(NonEmpty Execution) store (request.projectId, request.runId, request.attemptId) receipt
    requireEvidence $ retained == artifact
    case report.results of row :| [] -> void $ verifiedLog directory report (row, receipt); _ -> fail "Reproduction requires one base execution"
    root <- canonicalizePath store.usageDir
    let admissions = root </> "fixes" </> toString request.runId.toText </> "checks" </> "admissions"
    entries <- listDirectory admissions
    matched <- forM [entry | entry <- entries, isJust $ UUID.fromString entry] \entry -> do
      metadata <- readPrivateArtifact (admissions </> entry) "admission.json" 65536 >>= either fail pure . AE.eitherDecodeStrict
      rationale <- either fail pure $ AET.parseEither (AE.withObject "Retained admission" (AE..: "rationale")) metadata
      pure $ not (T.null $ T.strip rationale) && metadata == admissionMetadata job artifact rationale
    requireEvidence $ or matched
  pure $ first (const "Retained reproduction does not match its admitted execution") outcome


verifyCandidate :: ArtifactStore -> Text -> CandidateManifest -> FilePath -> IO FilePath
verifyCandidate store repository result directory = do
  ensure $ not (T.null repository) && not (T.null $ T.strip result.rootCause) && T.length result.rootCause <= 1200 && result.base /= result.head
  root <- canonicalizePath store.usageDir
  let captured = root </> "fixes" </> toString result.runId.toText </> "checks" </> "candidates" </> toString result.candidateId.toText
  metadata <- readPrivateArtifact captured "candidate.json" 65536 >>= either fail pure . AE.eitherDecodeStrict
  ensure $ metadata == AE.toJSON result
  patch <- readPrivateArtifact captured "fix.patch" 1048576
  bundle <- readPrivateArtifact captured "candidate.bundle" bundleLimit
  ensure $ sha256 patch == result.patchSha256 && sha256 bundle == result.bundleSha256
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
  let mirror = cache </> toString result.projectId.toText </> toString (sha256 $ encodeUtf8 repository)
  canonical mirror
  environment <- getEnvironment
  let checkout = directory </> "checkout"
      git cwd args = T.strip . decodeUtf8 <$> readProcessStdout_ (HostGit.command environment HostGit.Local cwd args)
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
  where
    ensure condition = unless condition $ throwIO $ userError "Invalid captured candidate"
    canonical path = canonicalizePath path >>= ensure . (== path)


-- $setup
-- >>> import Relude
-- >>> import Data.Aeson qualified as AE
-- >>> import Data.Text qualified as T


-- | Candidate input preserves its byte budget and bounded visual explanation.
-- >>> let accepted patch cause = case AE.fromJSON @CandidatePatch (AE.object ["patch" AE..= (patch :: Text), "rootCause" AE..= (cause :: Text)]) of AE.Success _ -> True; _ -> False
-- >>> map (uncurry accepted) [("diff", "cause"), ("", "cause"), ("diff", " "), ("diff", "bad\DEL")]
-- [True,False,False,False]
-- >>> accepted (T.replicate 524289 "é") "cause"
-- False
-- >>> accepted "diff" (T.replicate 241 "x")
-- False
-- >>> accepted "diff" "valid\nexplanation"
-- True
data CandidatePatch = CandidatePatch {patch :: Text, rootCause :: CandidateCause}


newtype CandidateCause = CandidateCause {text :: Text}


instance AE.FromJSON CandidateCause where
  parseJSON = parseVisual 240 CandidateCause


instance AE.FromJSON CandidatePatch where
  parseJSON = AE.withObject "CandidatePatch" \fields -> do
    patch <- fields AE..: "patch"
    cause <- fields AE..: "rootCause"
    either (fail . toString) pure $ mkCandidatePatch patch cause


mkCandidatePatch :: Text -> CandidateCause -> Either Text CandidatePatch
mkCandidatePatch patch cause
  | BS.length (encodeUtf8 patch) <= 1048576 && not (T.null patch) = Right $ CandidatePatch patch cause
  | otherwise = Left "Candidate patch must be nonempty and within its 1 MiB limit"


data CandidateManifest = CandidateManifest {projectId :: UUIDId "project", runId :: UUIDId "issue_fix", issueId :: UUIDId "issue", attemptId :: UUIDId "agent_attempt", candidateId :: UUIDId "fix_candidate", base :: Revision, head :: Revision, patchSha256 :: Text, bundleSha256 :: Text, rootCause :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


newtype CapturedCandidate = CapturedCandidate CandidateManifest
  deriving newtype (AE.ToJSON)


newtype CandidateSnapshot = CandidateSnapshot CandidateManifest
  deriving newtype (AE.FromJSON, AE.ToJSON)


candidateSnapshot :: CapturedCandidate -> CandidateSnapshot
candidateSnapshot (CapturedCandidate candidate) = CandidateSnapshot candidate


restoreCandidate :: ArtifactStore -> FixRequest -> FilePath -> CandidateSnapshot -> IO (Either Text CapturedCandidate)
restoreCandidate store job checkout (CandidateSnapshot captured) = do
  outcome <- tryAny $ withSystemTempDirectory "monoscope-native-candidate" \directory -> Timeout.timeout (180 * 1000000) do
    let request = job.request
    unless ((captured.projectId, captured.runId, captured.issueId, captured.attemptId, revisionText captured.base) == (request.projectId, request.runId, job.issueId, request.attemptId, request.base)) $ fail "Candidate belongs to another fix request"
    verified <- verifyCandidate store request.repository captured directory
    environment <- getEnvironment
    let git = readProcessStdout_ . HostGit.command environment HostGit.Local checkout
    void $ git ["fetch", "--quiet", "--no-tags", "--", verified, toString $ revisionText captured.head]
    void $ git ["update-ref", "refs/heads/monoscope-candidate/" <> toString captured.candidateId.toText, toString $ revisionText captured.head]
    void $ git ["checkout", "--quiet", "--detach", toString $ revisionText captured.head]
    pure $ CapturedCandidate captured
  pure $ case outcome of Right (Just restored) -> Right restored; _ -> Left "Retained candidate does not match this fix request"


candidateMatchesPatch :: CapturedCandidate -> UUIDId "fix_candidate" -> Text -> Bool
candidateMatchesPatch (CapturedCandidate candidate) identifier patch = candidate.candidateId == identifier && sha256 (encodeUtf8 patch) == candidate.patchSha256


candidateStep :: CapturedCandidate -> Step
candidateStep (CapturedCandidate candidate) = ProposeCandidate $ Candidate candidate.head candidate.rootCause


candidateIdentity :: CapturedCandidate -> UUIDId "fix_candidate"
candidateIdentity (CapturedCandidate candidate) = candidate.candidateId


candidateRevision :: CapturedCandidate -> Revision
candidateRevision (CapturedCandidate candidate) = candidate.head


-- | Summaries preserve meaningful, bounded visual copy.
-- >>> let accepts text = case AE.fromJSON @FixSummary (AE.String text) of AE.Success _ -> True; _ -> False
-- >>> map accepts ["Preserve retry tokens", " ", T.replicate 181 "x", "bad\DEL"]
-- [True,False,False,False]
-- >>> accepts (T.replicate 180 "x")
-- True
newtype FixSummary = FixSummary {text :: Text}


instance AE.FromJSON FixSummary where
  parseJSON = parseVisual 180 FixSummary


parseVisual :: Int -> (Text -> value) -> AE.Value -> AET.Parser value
parseVisual limit wrap = AE.withText "Visual explanation" \text ->
  if not (T.null $ T.strip text) && T.length text <= limit && T.all (\c -> c >= ' ' || c `elem` ['\t', '\n']) text && not (T.any (== '\DEL') text)
    then pure $ wrap text
    else fail "Visual explanation must be nonblank, bounded and free of control characters"


candidateResult :: CapturedCandidate -> CheckReceipt -> FixSummary -> Either Text FixResult
candidateResult (CapturedCandidate candidate) receipt summary = case receipt.regression of
  Just (RetainedTest fingerprint)
    | receipt.side == Head && receipt.status == Passed && receipt.revision == revisionText candidate.head ->
        Right $ FixResult candidate.projectId candidate.runId candidate.issueId candidate.attemptId candidate.candidateId candidate.base candidate.head candidate.patchSha256 candidate.bundleSha256 candidate.rootCause receipt.checkId fingerprint summary.text
    | otherwise -> Left "Candidate has no passing retained regression"
  (Nothing; Just ScriptCheck{}; Just InjectedTest{}) -> Left "Candidate has no passing retained regression"


captureCandidate :: FilePath -> FixRequest -> FilePath -> CandidatePatch -> IO (Either Text CapturedCandidate)
captureCandidate artifacts job checkout input = do
  outcome <- tryAny $ Timeout.timeout (120 * 1000000) do
    environment <- getEnvironment
    let request = job.request
        git args = T.strip . decodeUtf8 <$> readProcessStdout_ (HostGit.command environment HostGit.Local checkout args)
        ensure condition = unless condition $ throwIO $ userError "Invalid captured candidate"
    base <- either (const $ throwIO $ userError "Invalid candidate base") pure $ mkRevision request.base
    identifier <- UUIDId <$> UUID4.nextRandom
    let collection = artifacts </> "candidates"
        directory = collection </> toString identifier.toText
        patchFile = directory </> "fix.patch"
        bundle = directory </> "candidate.bundle"
        reference = "refs/heads/monoscope-candidate/" <> toString identifier.toText
        publish = writePrivateArtifact directory
    createDirectoryIfMissing False collection
    Posix.setFileMode collection 0o700
    sync artifacts
    createDirectory directory
    Posix.setFileMode directory 0o700
    sync collection
    publish "fix.patch" $ fromStrict $ encodeUtf8 input.patch
    void $ git ["reset", "--hard", toString request.base]
    void $ git ["clean", "-fdx"]
    void $ git ["apply", "--index", patchFile]
    tree <- git ["write-tree"]
    revision <- git ["-c", "user.name=Monoscope", "-c", "user.email=agent@monoscope.invalid", "commit-tree", toString tree, "-p", toString request.base, "-m", "Fix reported issue and retain its regression"] >>= either (const $ throwIO $ userError "Invalid candidate revision") pure . mkRevision
    void $ git ["checkout", "--quiet", "--detach", toString $ revisionText revision]
    void $ git ["update-ref", reference, toString $ revisionText revision]
    void $ git ["bundle", "create", bundle, reference, "^" <> toString request.base]
    Posix.setFileMode bundle 0o600
    sync bundle
    bytes <- readPrivateArtifact directory "candidate.bundle" bundleLimit
    ensure $ not (BS.null bytes)
    let captured = CandidateManifest request.projectId request.runId job.issueId request.attemptId identifier base revision (sha256 $ encodeUtf8 input.patch) (sha256 bytes) input.rootCause.text
    publish "candidate.json" $ AE.encode captured <> "\n"
    pure $ CapturedCandidate captured
  pure $ case outcome of Right (Just captured) -> Right captured; _ -> Left "Candidate could not be captured by the host"


retainAdmission :: FilePath -> FixRequest -> TestArtifact -> Text -> IO ()
retainAdmission artifacts job artifact rationale = do
  let collection = artifacts </> "admissions"
  createDirectoryIfMissing False collection
  Posix.setFileMode collection 0o700
  sync artifacts
  identifier <- UUID4.nextRandom
  let directory = collection </> show identifier
  createDirectory directory
  Posix.setFileMode directory 0o700
  sync collection
  writePrivateArtifact directory "admission.json" $ AE.encode $ admissionMetadata job artifact rationale


admissionMetadata :: FixRequest -> TestArtifact -> Text -> AE.Value
admissionMetadata job artifact rationale = AE.object ["projectId" AE..= job.request.projectId, "runId" AE..= job.request.runId, "issueId" AE..= job.issueId, "attemptId" AE..= job.request.attemptId, "base" AE..= job.request.base, "regressionSha256" AE..= testArtifactHash artifact, "rationale" AE..= rationale]


testArtifactHash :: TestArtifact -> Text
testArtifactHash artifact = sha256 $ toStrict $ Encoding.encodingToLazyByteString $ Encoding.pairs ("script" AE..= artifact.script <> "testPatch" AE..= artifact.testPatch <> "failureMessage" AE..= artifact.failureMessage)


writePrivateArtifact :: FilePath -> FilePath -> LBS.ByteString -> IO ()
writePrivateArtifact directory name bytes = withTempFile directory ".artifact-" \temporary handle -> do
  LBS.hPut handle bytes
  hFlush handle
  sync temporary
  renameFile temporary $ directory </> name
  sync directory


sync :: FilePath -> IO ()
sync path = bracket (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags{Posix.nofollow = True, Posix.cloexec = True}) Posix.closeFd fileSynchronise


bundleLimit :: Int
bundleLimit = 64 * 1048576


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
