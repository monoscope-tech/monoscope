module Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), CandidateCause, CandidatePatch, mkCandidatePatch, CapturedCandidate, candidateMatchesPatch, candidateStep, candidateIdentity, candidateRevision, FixSummary, candidateResult, captureCandidate, retainAdmission, VerifiedArtifacts, PeerPreparation, artifactCheckout, peerPreparation, verifyArtifacts, withVerifiedArtifacts, readPrivateArtifact, writePrivateArtifact, sha256) where

import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types qualified as AET
import Data.Base64.Types (extractBase64)
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
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, renameFile)
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
import "base64" Data.ByteString.Base64 qualified as Base64
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


data ArtifactStore = ArtifactStore {usageDir :: FilePath, cacheDir :: FilePath}


data VerifiedArtifacts = VerifiedArtifacts FilePath ArtifactRequest ReviewEvidence


data ReviewEvidence = ReviewEvidence TestArtifact ByteString ByteString


data PeerPreparation = PeerPreparation FixResult (Map Text ByteString)


instance AE.ToJSON PeerPreparation where
  toJSON (PeerPreparation result files) = AE.object ["result" AE..= result, "files" AE..= Map.map (extractBase64 . Base64.encodeBase64) files]


artifactCheckout :: VerifiedArtifacts -> FilePath
artifactCheckout (VerifiedArtifacts directory _ _) = directory


peerPreparation :: VerifiedArtifacts -> PeerPreparation
peerPreparation (VerifiedArtifacts _ request (ReviewEvidence regression baseLog headLog)) =
  PeerPreparation request.result
    $ Map.fromList
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


withVerifiedArtifacts :: ArtifactStore -> ArtifactRequest -> (VerifiedArtifacts -> IO a) -> IO (Either Text a)
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
      bundle <- readPrivateArtifact captured "candidate.bundle" bundleLimit
      ensure $ sha256 patch == result.patchSha256 && sha256 bundle == result.bundleSha256
      report <- decode @Report =<< readPrivateArtifact execution "report.json" 65536
      ensure $ (report.id, report.projectId, report.runId, report.attemptId) == (result.checkId, result.projectId, result.runId, result.attemptId)
      ensure $ (report.imageBuildId, report.workspace, report.scriptSha256, report.regressionSha256) == (baseline.imageBuildId, baseline.workspace, baseline.scriptSha256, result.regressionSha256)
      case report.status of Completed -> pass
      script <- readPrivateArtifact execution "script.sh" 262144
      testPatch <- readPrivateArtifact execution "test.patch" 1048576
      ensure $ sha256 script == baseline.scriptSha256 && sha256 testPatch == report.testPatchSha256 && not (T.null report.failureMessage) && testArtifactHash (TestArtifact (decodeUtf8 script) (decodeUtf8 testPatch) report.failureMessage) == result.regressionSha256
      let (baseRow, headRow) = report.results
      ensure $ digest baseRow.testFilesSha256 && baseRow.testFilesSha256 == headRow.testFilesSha256
      let verifiedLog ((row, receipt) :: (Execution, CheckReceipt)) = do
            ensure $ receipt.attemptId == result.attemptId && receipt.checkId == result.checkId && regressionHash receipt.regression == Just result.regressionSha256
            ensure $ receipt.cpus > 0 && receipt.memoryMiB > 0 && receipt.logBytes <= 1048576
            ensure $ (row.id, row.side, revisionText row.revision, row.status, row.exitCode, row.logBytes, row.logSha256) == (receipt.id, receipt.side, receipt.revision, receipt.status, receipt.exitCode, receipt.logBytes, receipt.logSha256)
            ensure $ row.durationMs >= 0 && ceiling @Scientific @Integer row.durationMs == toInteger receipt.durationMs
            ensure $ (row.configuredResources.cpus, row.configuredResources.memoryMiB) == (receipt.cpus, receipt.memoryMiB)
            ensure $ digest row.sourceBefore && row.sourceBefore == row.sourceAfter && Just row.testPatchDisposition == placement receipt.regression
            output <- readPrivateArtifact execution (if receipt.side == Base then "base.log" else "head.log") 1048576
            ensure $ BS.length output == fromIntegral receipt.logBytes && sha256 output == receipt.logSha256 && (receipt.side /= Base || encodeUtf8 report.failureMessage `BS.isInfixOf` output)
            pure output
      (baseLog, headLog) <- (,) <$> verifiedLog (baseRow, baseline) <*> verifiedLog (headRow, candidate)
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
      pure $ VerifiedArtifacts checkout request $ ReviewEvidence (TestArtifact (decodeUtf8 script) (decodeUtf8 testPatch) report.failureMessage) baseLog headLog


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
  deriving stock (Generic)


newtype CandidateCause = CandidateCause {text :: Text}


instance AE.FromJSON CandidateCause where
  parseJSON = parseVisual 240 CandidateCause


instance AE.FromJSON CandidatePatch where
  parseJSON value = do
    input <- AE.genericParseJSON @CandidatePatch AE.defaultOptions value
    either (fail . toString) pure $ mkCandidatePatch input.patch input.rootCause


mkCandidatePatch :: Text -> CandidateCause -> Either Text CandidatePatch
mkCandidatePatch patch cause
  | BS.length (encodeUtf8 patch) <= 1048576 && not (T.null patch) = Right $ CandidatePatch patch cause
  | otherwise = Left "Candidate patch must be nonempty and within its 1 MiB limit"


data CapturedCandidate = CapturedCandidate {projectId :: UUIDId "project", runId :: UUIDId "issue_fix", issueId :: UUIDId "issue", attemptId :: UUIDId "agent_attempt", candidateId :: UUIDId "fix_candidate", base :: Revision, head :: Revision, patchSha256 :: Text, bundleSha256 :: Text, rootCause :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


candidateMatchesPatch :: CapturedCandidate -> UUIDId "fix_candidate" -> Text -> Bool
candidateMatchesPatch candidate identifier patch = candidate.candidateId == identifier && sha256 (encodeUtf8 patch) == candidate.patchSha256


candidateStep :: CapturedCandidate -> Step
candidateStep candidate = ProposeCandidate $ Candidate candidate.head candidate.rootCause


candidateIdentity :: CapturedCandidate -> UUIDId "fix_candidate"
candidateIdentity = (.candidateId)


candidateRevision :: CapturedCandidate -> Revision
candidateRevision = (.head)


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
candidateResult candidate receipt summary = case receipt.regression of
  Just (RetainedTest fingerprint)
    | receipt.side == Head && receipt.status == Passed && receipt.revision == revisionText candidate.head ->
        Right $ FixResult candidate.projectId candidate.runId candidate.issueId candidate.attemptId candidate.candidateId candidate.base candidate.head candidate.patchSha256 candidate.bundleSha256 candidate.rootCause receipt.checkId fingerprint summary.text
  _ -> Left "Candidate has no passing retained regression"


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
    let captured = CapturedCandidate request.projectId request.runId job.issueId request.attemptId identifier base revision (sha256 $ encodeUtf8 input.patch) (sha256 bytes) input.rootCause.text
    publish "candidate.json" $ AE.encode captured <> "\n"
    pure captured
  pure $ case outcome of Right (Just captured) -> Right captured; _ -> Left "Candidate could not be captured by the host"


retainAdmission :: FilePath -> FixRequest -> TestArtifact -> Text -> IO ()
retainAdmission artifacts job artifact rationale = do
  let collection = artifacts </> "admissions"
      request = job.request
  createDirectoryIfMissing False collection
  Posix.setFileMode collection 0o700
  sync artifacts
  identifier <- UUID4.nextRandom
  let directory = collection </> show identifier
  createDirectory directory
  Posix.setFileMode directory 0o700
  sync collection
  writePrivateArtifact directory "admission.json" $ AE.encode $ AE.object ["projectId" AE..= request.projectId, "runId" AE..= request.runId, "issueId" AE..= job.issueId, "attemptId" AE..= request.attemptId, "base" AE..= request.base, "regressionSha256" AE..= testArtifactHash artifact, "rationale" AE..= rationale]


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
