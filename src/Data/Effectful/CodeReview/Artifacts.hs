module Data.Effectful.CodeReview.Artifacts (ArtifactStore (..), CandidatePatch, CapturedCandidate (..), captureCandidate, VerifiedArtifacts, artifactCheckout, peerEvidence, verifyArtifacts, withVerifiedArtifacts, readPrivateArtifact, sha256, renderExplanation, readExplanationMedia) where

import Data.Aeson qualified as AE
import Data.Aeson.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Data.Char (isControl, isDigit)
import Data.Effectful.AgentWorkspace (CheckExecution (..), ExecutionKey (..), ImageId (..), Measurement (..), Placement (..), RegressionReport (..), ReportState (..), Resources (..), captureProcess, gitProcess, pinnedRevisionText, publishBytes, readBounded, sync, workspaceFingerprintText)
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.FileEmbed (embedFile)
import Data.Generics.Product.Subtype (upcast)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import GHC.Clock (getMonotonicTimeNSec)
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, removePathForcibly, renameDirectory)
import System.Environment (getEnvironment)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((</>))
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Posix.Files qualified as Posix
import System.Process.Typed (ExitCode (..), nullStream, proc, readProcessStdout_, setEnv, setStdin, setWorkingDir)
import UnliftIO.Exception (throwIO, tryAny)
import UnliftIO.Timeout qualified as Timeout
import "cryptonite" Crypto.Hash (Digest, SHA256, hash)


data ArtifactStore = ArtifactStore {usageDir :: FilePath, cacheDir :: FilePath}


data VerifiedArtifacts = VerifiedArtifacts FilePath ArtifactRequest ReviewEvidence


data ReviewEvidence = ReviewEvidence TestArtifact ByteString ByteString


artifactCheckout :: VerifiedArtifacts -> FilePath
artifactCheckout (VerifiedArtifacts directory _ _) = directory


-- | Recorded evidence for the independent reviewer, keyed by its read-only guest path.
peerEvidence :: VerifiedArtifacts -> Map Text ByteString
peerEvidence (VerifiedArtifacts _ request (ReviewEvidence regression baseLog headLog)) =
  Map.fromList
    [ ("/run/monoscope-fix-review.json", toStrict $ AE.encode $ AE.object ["result" AE..= request.result, "regression" AE..= regression, "baseline" AE..= request.baseline, "candidate" AE..= request.candidate])
    , ("/run/monoscope-fix-review-base.log", baseLog)
    , ("/run/monoscope-fix-review-head.log", headLog)
    ]


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
          expectedMetadata = AE.toJSON (upcast @CapturedCandidate result)
      metadata <- decode =<< readPrivateArtifact captured "candidate.json" 65536
      ensure $ metadata == expectedMetadata
      patch <- readPrivateArtifact captured "fix.patch" 1048576
      bundle <- readPrivateArtifact captured "candidate.bundle" bundleLimit
      ensure $ sha256 patch == result.patchSha256 && sha256 bundle == result.bundleSha256
      report <- decode @(RegressionReport Scientific) =<< readPrivateArtifact execution "report.json" 65536
      ensure $ (report.id, report.projectId, report.runId.unUUIDId, report.attemptId) == (result.checkId, result.projectId, result.runId.unUUIDId, result.attemptId)
      ensure $ (case report.imageBuildId of { ImageId image -> UUID.toText image }, report.workspace, workspaceFingerprintText report.scriptSha256, workspaceFingerprintText report.regressionSha256) == (baseline.imageBuildId, baseline.workspace, baseline.scriptSha256, result.regressionSha256)
      script <- readPrivateArtifact execution "script.sh" 262144
      testPatch <- readPrivateArtifact execution "test.patch" 1048576
      let frozen = toStrict $ Encoding.encodingToLazyByteString $ Encoding.pairs ("script" AE..= (decodeUtf8 script :: Text) <> "testPatch" AE..= (decodeUtf8 testPatch :: Text) <> "failureMessage" AE..= report.failureMessage)
      ensure $ sha256 script == baseline.scriptSha256 && Just (sha256 testPatch) == fmap workspaceFingerprintText report.testPatchSha256 && not (T.null report.failureMessage) && sha256 frozen == result.regressionSha256
      (baseKey, baseRow, headKey, headRow) <- case report.results of
        [Measured baseKey baseRow, Measured headKey headRow] | report.status == Completed -> pure (baseKey, baseRow, headKey, headRow)
        _ -> throwIO $ userError "Incomplete retained comparison"
      ensure $ isJust baseRow.testFilesSha256 && baseRow.testFilesSha256 == headRow.testFilesSha256
      let verifiedLog ((key, row, receipt) :: (ExecutionKey, Measurement Scientific, CheckReceipt)) = do
            ensure $ receipt.attemptId == result.attemptId && receipt.checkId == result.checkId && regressionHash receipt.regression == Just result.regressionSha256
            ensure $ receipt.cpus > 0 && receipt.memoryMiB > 0 && receipt.logBytes <= 1048576
            ensure $ (key.id, key.side, pinnedRevisionText key.revision, row.status, row.exitCode, row.logBytes, workspaceFingerprintText row.logSha256) == (receipt.id, receipt.side, receipt.revision, receipt.status, receipt.exitCode, receipt.logBytes, receipt.logSha256)
            ensure $ row.durationMs >= 0 && ceiling @Scientific @Integer row.durationMs == toInteger receipt.durationMs
            ensure $ (row.configuredResources.cpus, row.configuredResources.memoryMiB) == (receipt.cpus, receipt.memoryMiB)
            ensure $ isJust row.sourceBefore && row.sourceBefore == row.sourceAfter && row.testPatchDisposition == placement receipt.regression && isJust row.testPatchDisposition
            output <- readPrivateArtifact execution (if receipt.side == Base then "base.log" else "head.log") 1048576
            ensure $ BS.length output == fromIntegral receipt.logBytes && sha256 output == receipt.logSha256 && (receipt.side /= Base || encodeUtf8 report.failureMessage `BS.isInfixOf` output)
            pure output
      (baseLog, headLog) <- (,) <$> verifiedLog (baseKey, baseRow, baseline) <*> verifiedLog (headKey, headRow, candidate)
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
      let checkout = directory </> "checkout"
          git cwd args = T.strip . decodeUtf8 <$> (readProcessStdout_ =<< gitProcess "file" cwd args [])
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
data CandidatePatch = CandidatePatch {patch :: Text, rootCause :: Text}
  deriving stock (Generic)


instance AE.FromJSON CandidatePatch where
  parseJSON value = do
    input <- AE.genericParseJSON AE.defaultOptions value
    unless (BS.length (encodeUtf8 input.patch) <= 1048576 && not (T.null input.patch) && not (T.null $ T.strip input.rootCause) && T.length input.rootCause <= 240 && T.all (\c -> c >= ' ' || c `elem` ['\t', '\n']) input.rootCause && not (T.any (== '\DEL') input.rootCause)) $ fail "Invalid candidate patch or root cause"
    pure input


data CapturedCandidate = CapturedCandidate {projectId :: UUIDId "project", runId :: UUIDId "issue_fix", issueId :: UUIDId "issue", attemptId :: UUIDId "agent_attempt", candidateId :: UUIDId "fix_candidate", base :: Revision, head :: Revision, patchSha256 :: Text, bundleSha256 :: Text, rootCause :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


captureCandidate :: FilePath -> FixRequest -> FilePath -> CandidatePatch -> IO (Either Text CapturedCandidate)
captureCandidate artifacts job checkout input = do
  outcome <- tryAny $ Timeout.timeout (120 * 1000000) do
    let request = job.request
        git args = T.strip . decodeUtf8 <$> (readProcessStdout_ =<< gitProcess "file" checkout args [])
        ensure condition = unless condition $ throwIO $ userError "Invalid captured candidate"
    base <- either (const $ throwIO $ userError "Invalid candidate base") pure $ mkRevision request.base
    identifier <- UUIDId <$> UUID4.nextRandom
    let collection = artifacts </> "candidates"
        directory = collection </> toString identifier.toText
        patchFile = directory </> "fix.patch"
        bundle = directory </> "candidate.bundle"
        reference = "refs/heads/monoscope-candidate/" <> toString identifier.toText
        publish name = publishBytes (directory </> name)
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
    let captured = CapturedCandidate request.projectId request.runId job.issueId request.attemptId identifier base revision (sha256 $ encodeUtf8 input.patch) (sha256 bytes) input.rootCause
    publish "candidate.json" $ AE.encode captured <> "\n"
    pure captured
  pure $ case outcome of Right (Just captured) -> Right captured; _ -> Left "Candidate could not be captured by the host"


bundleLimit :: Int
bundleLimit = 64 * 1048576


sha256 :: ByteString -> Text
sha256 bytes = show (hash bytes :: Digest SHA256)


readPrivateArtifact :: FilePath -> FilePath -> Int -> IO ByteString
readPrivateArtifact directory name limit = do
  canonicalizePath directory >>= ensure . (== directory)
  readBounded (directory </> name) $ fromIntegral limit
  where
    ensure condition = unless condition $ throwIO $ userError "Invalid private artifact"


data Stream = Stream {codec_name :: Text, width :: Word16, height :: Word16}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


newtype Format = Format {duration :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Probe = Probe {streams :: [Stream], format :: Format}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Scene = Scene {name :: Text, title :: Text, heading :: Text, detail :: Text, note :: Text, accent :: Text}


renderExplanation :: ArtifactStore -> ArtifactRequest -> IO (Either Text Explanation)
renderExplanation store request = withinBudget $ do
  regression <- either (const invalidExplanation) pure =<< withVerifiedArtifacts store request (\(VerifiedArtifacts _ _ (ReviewEvidence frozen _ _)) -> pure frozen)
  root <- canonicalizePath store.usageDir
  let result = request.result
      checks = root </> "fixes" </> toString result.runId.toText </> "checks"
      captured = checks </> "candidates" </> toString result.candidateId.toText
  let cause = result.rootCause
      change = result.summary
      before = regression.failureMessage
      bounded limit value = not (T.null $ T.strip value) && T.length value <= limit && not (T.any (\c -> isControl c && c /= '\n' && c /= '\t') value)
  ensureExplanation $ bounded 240 cause && bounded 180 change && bounded 360 before
  let renderer = sha256 $ $(embedFile "src/Data/Effectful/CodeReview/Artifacts.hs") <> $(embedFile "src/Data/Effectful/CodeReview/Types.hs")
      source = sha256 $ toStrict $ AE.encode $ AE.object ["version" AE..= (1 :: Int), "rendererSha256" AE..= renderer, "result" AE..= result, "executions" AE..= (request.baseline.id, request.candidate.id, request.baseline.imageBuildId), "presentation" AE..= (cause, change, before)]
      parent = captured </> "explanations"
      directory = parent </> toString source
  createDirectoryIfMissing True parent
  canonicalizePath parent >>= ensureExplanation . (== parent)
  Posix.setFileMode parent 0o700
  withFileLock (parent </> ".native-render.lock") Exclusive \_ -> do
    cached <- tryAny do
      manifest <- decodeExplanation @Explanation =<< readPrivateArtifact directory "manifest.json" 65536
      ensureExplanation $ manifest.source == source && manifest.rendererSha256 == renderer && validExplanation result manifest
      sizes <- forM (Map.toList manifest.files) \(name, digest) -> do
        bytes <- readPrivateArtifact directory (toString name) (32 * 1048576)
        ensureExplanation $ sha256 bytes == digest
        pure (name, fromIntegral $ BS.length bytes :: Word64)
      ensureExplanation $ sum (map snd sizes) == manifest.mediaBytes && lookup "explanation.mp4" sizes == Just manifest.videoBytes
      pure manifest
    case cached of
      Right manifest -> pure manifest
      Left _ -> withTempDirectory parent ".native-render-" \temporary -> do
        started <- getMonotonicTimeNSec
        environment <- filter ((== "PATH") . fst) <$> getEnvironment
        let command program args = do
              (exit, out, err) <- captureProcess 1048576 (setStdin nullStream $ setEnv (environment <> [("LC_ALL", "C")]) $ setWorkingDir temporary $ proc program args)
              ensureExplanation $ exit == ExitSuccess
              pure (decodeUtf8 out, decodeUtf8 err)
            write :: FilePath -> Text -> IO ()
            write name value = writeFileBS (temporary </> name) $ encodeUtf8 value
            wrap width = T.intercalate "\n" . concatMap splitLine . T.splitOn "\n"
              where
                splitLine text
                  | T.length text <= width = [text]
                  | otherwise =
                      let (prefix, suffix) = T.splitAt width text
                          (wholeWords, lastWord) = T.breakOnEnd " " prefix
                          (part, rest) = if T.null wholeWords then (prefix, suffix) else (T.dropEnd 1 wholeWords, lastWord <> suffix)
                       in part : splitLine rest
            quoted = decodeUtf8 . AE.encode @Text
            graph body = "digraph G { graph [bgcolor=\"#0f172a\", pad=\"0.65\", nodesep=\"0.5\", ranksep=\"0.5\"]; node [fontname=\"Helvetica\", fontsize=26, fontcolor=\"#f8fafc\", color=\"#334155\", style=\"rounded,filled\", fillcolor=\"#172338\", margin=\"0.3,0.22\", shape=box]; edge [color=\"#94a3b8\", arrowsize=0.9]; " <> body <> " }"
            short = T.take 10 . revisionText
            scenes =
              [ Scene "before" "01  /  REPRODUCE" "The regression fails on base" before ("Recorded assertion · exit " <> maybe "unknown" show request.baseline.exitCode <> " · " <> short result.base) "#fb7185"
              , Scene "cause" "02  /  ROOT CAUSE" "Why the behavior failed" cause "Agent explanation · inference from the investigated code" "#a5b4fc"
              , Scene "change" "03  /  THE FIX" "The candidate keeps the regression" change "Same test · same image · fresh verifier VMs" "#a5b4fc"
              , Scene "after" "04  /  VERIFY" "The unchanged test passes on candidate" "Verified exit code: 0." ("Recorded exit status · exit 0 · " <> short result.head <> "\nCounterfactual: the same test still fails on base") "#34d399"
              ]
        write "diagram.dot" $ graph $ "rankdir=LR; base [label=" <> quoted ("Base fails\n" <> short result.base) <> ", color=\"#fb7185\"]; cause [label=" <> quoted ("Inferred cause\n" <> wrap 30 cause) <> "]; change [label=" <> quoted ("Change\n" <> wrap 30 change) <> "]; fixed [label=" <> quoted ("Same test passes\n" <> short result.head) <> ", color=\"#34d399\"]; base -> cause -> change -> fixed;"
        void $ command "dot" ["-Tsvg", "diagram.dot", "-o", "diagram.svg"]
        for_ scenes \scene ->
          if scene.name == "cause"
            then void $ command "dot" ["-Tpng", "diagram.dot", "-o", "cause.png"]
            else do
              write (toString scene.name <> ".dot") $ graph $ "a [label=" <> quoted scene.title <> ", shape=plain, fontsize=20, fontcolor=" <> quoted scene.accent <> "]; b [label=" <> quoted scene.heading <> ", shape=plain, fontsize=32]; c [label=" <> quoted (wrap 60 scene.detail) <> ", fontname=" <> quoted (if scene.name == "before" || scene.name == "after" then "Courier" else "Helvetica") <> ", color=" <> quoted scene.accent <> "]; d [label=" <> quoted scene.note <> ", shape=plain, fontsize=18, fontcolor=\"#cbd5e1\"]; e [label=" <> quoted ("Monoscope · retained verification evidence · " <> T.take 8 result.checkId.toText) <> ", shape=plain, fontsize=14, fontcolor=\"#94a3b8\"]; a -> b -> c -> d -> e [style=invis];"
              void $ command "dot" ["-Tpng", toString scene.name <> ".dot", "-o", toString scene.name <> ".png"]
        let stamp :: Int -> Text
            stamp seconds = "00:00:" <> T.justifyRight 2 '0' (show seconds) <> ".000"
            escaped = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;" . T.intercalate "\n" . filter (not . T.null . T.strip) . lines
        write "captions.vtt" $ "WEBVTT\n\n" <> T.intercalate "\n" [stamp (index * 9) <> " --> " <> stamp ((index + 1) * 9) <> "\n" <> scene.heading <> "\n" <> escaped scene.detail <> "\n" | (index, scene) <- zip [0 ..] scenes]
        let inputs = concatMap (\scene -> ["-loop", "1", "-t", "9", "-i", toString scene.name <> ".png"]) scenes
            filters = T.intercalate ";" ["[" <> show index <> ":v]scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2:color=0x0f172a,setsar=1,fps=24[v" <> show index <> "]" | index <- [0 .. 3 :: Int]] <> ";[v0][v1][v2][v3]concat=n=4:v=1:a=0[out]"
        void $ command "ffmpeg" $ ["-hide_banner", "-loglevel", "error"] <> inputs <> ["-filter_complex", toString filters, "-map", "[out]", "-an", "-c:v", "libx264", "-preset", "veryfast", "-crf", "24", "-pix_fmt", "yuv420p", "-movflags", "+faststart", "-t", "36", "explanation.mp4"]
        probe <- decodeExplanation @Probe . encodeUtf8 . fst =<< command "ffprobe" ["-v", "error", "-show_streams", "-show_format", "-of", "json", "explanation.mp4"]
        ensureExplanation $ case probe.streams of [stream] -> (stream.codec_name, stream.width, stream.height) == ("h264", 1280, 720) && readMaybe @Double (toString probe.format.duration) == Just 36; _ -> False
        ffmpeg <- maybe invalidExplanation pure . viaNonEmpty Relude.head . lines . fst =<< command "ffmpeg" ["-version"]
        graphviz <- T.strip . snd <$> command "dot" ["-V"]
        files <- forM mediaFiles \name -> do
          bytes <- readPrivateArtifact temporary (toString name) (32 * 1048576)
          pure (name, (sha256 bytes, fromIntegral $ BS.length bytes :: Word64))
        elapsed <- subtract started <$> getMonotonicTimeNSec
        videoBytes <- maybe invalidExplanation (pure . snd) $ lookup "explanation.mp4" files
        let manifest = Explanation{version = 1, source, rendererSha256 = renderer, projectId = result.projectId, runId = result.runId, issueId = result.issueId, candidateId = result.candidateId, head = result.head, checkId = result.checkId, regressionSha256 = result.regressionSha256, durationMs = 36000, width = 1280, height = 720, renderMs = max 1 $ (elapsed + 999999) `div` 1000000, mediaBytes = sum $ map (snd . snd) files, videoBytes, resourceCostStatus = Unpriced, tools = Map.fromList [("ffmpeg", ffmpeg), ("graphviz", graphviz)], files = Map.fromList $ map (second fst) files}
        writeFileLBS (temporary </> "manifest.json") $ AE.encode manifest
        for_ ("manifest.json" : map toString mediaFiles) $ sync . (temporary </>)
        sync temporary
        removePathForcibly directory
        renameDirectory temporary directory
        sync parent
        pure manifest


readExplanationMedia :: FilePath -> Explanation -> Text -> IO (Either Text ByteString)
readExplanationMedia usageDir explanation asset = withinBudget do
  ensureExplanation $ asset `elem` mediaFiles && T.length explanation.source == 64 && T.all (\c -> isDigit c || c >= 'a' && c <= 'f') explanation.source
  root <- canonicalizePath usageDir
  let directory = root </> "fixes" </> toString explanation.runId.toText </> "checks" </> "candidates" </> toString explanation.candidateId.toText </> "explanations" </> toString explanation.source
  retained <- decodeExplanation @Explanation =<< readPrivateArtifact directory "manifest.json" 65536
  ensureExplanation $ retained == explanation
  bytes <- readPrivateArtifact directory (toString asset) (32 * 1048576)
  ensureExplanation $ Map.lookup asset explanation.files == Just (sha256 bytes)
  pure bytes


withinBudget :: IO a -> IO (Either Text a)
withinBudget action = do
  outcome <- tryAny $ Timeout.timeout (180 * 1000000) action
  pure $ case outcome of Right (Just result) -> Right result; _ -> Left "Verified explanation media could not be prepared"


decodeExplanation :: AE.FromJSON a => ByteString -> IO a
decodeExplanation = either (const invalidExplanation) pure . AE.eitherDecodeStrict


ensureExplanation :: Bool -> IO ()
ensureExplanation condition = unless condition invalidExplanation


invalidExplanation :: IO a
invalidExplanation = throwIO $ userError "Invalid retained explanation"
