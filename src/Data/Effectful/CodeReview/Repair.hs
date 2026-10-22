module Data.Effectful.CodeReview.Repair (Stage (..), Repair (..), reproduce, propose, verify, submission) where

import Control.Monad.Trans.Except (throwE)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.Effectful.AgentWorkspace (agentCommand, publish)
import Data.Effectful.CodeReview.Artifacts (CapturedCandidate (..), captureCandidate, sha256)
import Data.Effectful.CodeReview.Guest qualified as Guest
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))


-- | Source edits unlock only after a host-admitted test fails on base; submission needs a verified, unchanged candidate.
data Stage
  = Unreproduced
  | Reproduced TestArtifact
  | Proposed TestArtifact CapturedCandidate
  | Verified TestArtifact CapturedCandidate CheckReceipt


-- | What a fix session may do: replay a frozen test on revisions (recording its checks), ask the host to admit a test,
-- and persist phases, which throws when the host rejects one.
data Repair = Repair
  { session :: Guest.Session
  , job :: FixRequest
  , checkout :: FilePath
  , artifacts :: FilePath
  , admit :: TestArtifact -> IO (Either Text Text)
  , phase :: Step -> IO ()
  , replay :: TestArtifact -> [(Text, Text)] -> IO (Either Text (AE.Value, [CheckReceipt]))
  , stage :: IORef Stage
  }


reproduce :: Repair -> AE.Value -> IO (Either Text AE.Value)
reproduce repair arguments =
  readIORef repair.stage >>= \case
    Unreproduced -> runExceptT do
      (script, failureMessage, inline, file) <- hoistEither $ first toText $ AET.parseEither (AE.withObject "Regression" \o -> (,,,) <$> o AE..: "script" <*> o AE..: "failureMessage" <*> o AE..:? "testPatch" <*> o AE..:? "testPatchFile") arguments
      testPatch <- case (inline, file) of
        (Just text, Nothing) -> pure text
        (Nothing, Just path) -> do
          (exit, bytes, _) <- liftIO $ repair.session.output (agentCommand ["/bin/sh", "-c", "exec /usr/bin/head -c 262145 -- \"$1\" 2>/dev/null", "monoscope-patch", resolve path]) (262144 + 2)
          when (exit /= Just 0) $ throwE "Regression patch file could not be read"
          when (BS.length bytes > 262144) $ throwE "Regression patch file exceeds its 256 KiB limit"
          pure $ decodeUtf8 bytes
        _ -> throwE "Supply exactly one of testPatch or testPatchFile"
      when (T.null testPatch || T.length testPatch > 262144) $ throwE "Regression patch must contain 1 to 262144 characters"
      let artifact = TestArtifact script testPatch failureMessage
      rationale <- ExceptT $ repair.admit artifact
      when (T.null $ T.strip rationale) $ throwE "Host admission requires a recorded rationale"
      admission <- liftIO $ ((repair.artifacts </> "admissions") </>) . UUID.toString <$> UUID4.nextRandom
      liftIO $ createDirectoryIfMissing True admission >> publish (admission </> "admission.json") (AE.object ["projectId" AE..= repair.job.request.projectId, "runId" AE..= repair.job.request.runId, "issueId" AE..= repair.job.issueId, "attemptId" AE..= repair.job.request.attemptId, "base" AE..= repair.job.request.base, "regressionSha256" AE..= sha256 (toStrict $ AE.encode $ AE.object ["script" AE..= script, "testPatch" AE..= testPatch, "failureMessage" AE..= failureMessage]), "rationale" AE..= rationale])
      (report, receipts) <- ExceptT $ repair.replay artifact [("base", repair.job.request.base)]
      case receipts of
        [baseline] | baseline.status == Failed -> do
          liftIO do
            repair.session.place "/run/monoscope-admitted-test.patch" $ encodeUtf8 testPatch
            unless (retained baseline) $ repair.session.root $ "/usr/bin/git" :| ["-c", "core.hooksPath=/dev/null", "apply", "--index", "/run/monoscope-admitted-test.patch"]
            repair.session.root $ "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]
            writeIORef repair.stage $ Reproduced artifact
          pure $ AE.String $ "Regression reproduced and frozen. Source editing is now available.\n" <> decodeUtf8 (AE.encode report)
        _ -> throwE "The admitted regression did not fail on base; source edits remain locked"
    _ -> pure $ Left "A regression is already frozen for this run"


propose :: Repair -> Text -> IO (Either Text AE.Value)
propose repair rootCause =
  readIORef repair.stage
    >>= maybe
      (pure $ Left "Reproduce the issue before proposing a fix")
      ( \artifact -> runExceptT do
          liftIO $ writeIORef repair.stage $ Reproduced artifact
          patch <- ExceptT $ capture repair
          input <- hoistEither $ maybeToRight "Invalid candidate patch or root cause" $ AET.parseMaybe AE.parseJSON $ AE.object ["patch" AE..= patch, "rootCause" AE..= rootCause]
          candidate <- ExceptT $ captureCandidate repair.artifacts repair.job repair.checkout input
          liftIO do
            repair.phase $ ProposeCandidate $ Candidate candidate.head rootCause
            writeIORef repair.stage $ Proposed artifact candidate
          pure $ AE.toJSON candidate
      )
      . frozen


verify :: Repair -> IO (Either Text AE.Value)
verify repair =
  readIORef repair.stage >>= \case
    current | Just (artifact, candidate) <- proposed current -> runExceptT do
      liftIO $ writeIORef repair.stage $ Proposed artifact candidate
      unchanged repair candidate "Workspace changed; propose and verify a new candidate"
      (report, receipts) <- ExceptT $ repair.replay artifact [("base", repair.job.request.base), ("head", revisionText candidate.head)]
      let rendered = decodeUtf8 $ AE.encode report
      case receipts of
        [baseline, verified]
          | baseline.status == Failed
          , verified.status == Passed
          , retained verified -> do
              liftIO $ writeIORef repair.stage $ Verified artifact candidate verified
              pure $ AE.String $ "Frozen regression fails on base and passes on the candidate with its test retained.\n" <> rendered
        _ -> throwE $ "Candidate verification did not establish a failing base and passing retained test.\n" <> rendered
    _ -> pure $ Left "Capture a candidate after reproduction first"


-- | Completes the agent's summary into the verified result the host validates.
submission :: Repair -> AE.Value -> IO (Either Text AE.Value)
submission repair draft =
  readIORef repair.stage >>= \case
    Verified _ candidate check | Just (RetainedTest digest) <- check.regression -> runExceptT do
      summary <- hoistEither $ first toText $ AET.parseEither (AE.withObject "Fix submission" (AE..: "summary")) draft
      unchanged repair candidate "Workspace changed after verification; propose and verify a new candidate"
      pure $ AE.toJSON $ FixResult candidate.projectId candidate.runId candidate.issueId candidate.attemptId candidate.candidateId candidate.base candidate.head candidate.patchSha256 candidate.bundleSha256 candidate.rootCause check.checkId digest summary
    _ -> pure $ Left "Capture and verify a source fix before submission"


frozen :: Stage -> Maybe TestArtifact
frozen = \case Unreproduced -> Nothing; Reproduced artifact -> Just artifact; Proposed artifact _ -> Just artifact; Verified artifact _ _ -> Just artifact


proposed :: Stage -> Maybe (TestArtifact, CapturedCandidate)
proposed = \case Proposed artifact candidate -> Just (artifact, candidate); Verified artifact candidate _ -> Just (artifact, candidate); _ -> Nothing


retained :: CheckReceipt -> Bool
retained receipt = case receipt.regression of Just (RetainedTest _) -> True; _ -> False


unchanged :: Repair -> CapturedCandidate -> Text -> ExceptT Text IO ()
unchanged repair candidate message = ExceptT (capture repair) >>= \patch -> unless (sha256 (encodeUtf8 patch) == candidate.patchSha256) $ throwE message


-- | The agent's staged changes against the pinned base, as a binary Git patch of at most 1 MiB.
capture :: Repair -> IO (Either Text Text)
capture repair = do
  (added, _, _) <- repair.session.output (agentCommand ["/usr/bin/git", "add", "-A"]) 65536
  (exit, bytes, truncated) <- repair.session.output (agentCommand ["/bin/sh", "-c", "exec /usr/bin/git diff --cached --binary --no-ext-diff --no-textconv --no-color --no-relative --src-prefix=a/ --dst-prefix=b/ \"$1\" -- 2>/dev/null", "monoscope-capture", repair.job.request.base]) (1048576 + 1)
  pure
    if
      | added /= Just 0 || exit /= Just 0 -> Left "Candidate patch could not be captured"
      | truncated -> Left "Candidate patch exceeds its 1 MiB limit"
      | otherwise -> Right $ decodeUtf8 bytes


-- | Guest paths resolve like the agent's file tools: home-relative, then workspace-relative.
resolve :: Text -> Text
resolve path
  | Just rest <- T.stripPrefix "~" path, T.null rest || "/" `T.isPrefixOf` rest = "/home/review-agent" <> rest
  | "/" `T.isPrefixOf` path = path
  | otherwise = "/workspace/" <> path
