module Data.Effectful.CodeReview.Repair (Progress, Repair (..), reproduce, propose, verify, submission) where

import Control.Monad.Trans.Except (throwE)
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.ByteString qualified as BS
import Data.Effectful.AgentWorkspace (LogExit (..), agentCommand, publish)
import Data.Effectful.CodeReview.Artifacts (CapturedCandidate (..), captureCandidate, sha256)
import Data.Effectful.AgentWorkspace.Guest qualified as Guest
import Data.Effectful.CodeReview.Types hiding (sha256)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import Pkg.DeriveUtils (UUIDId)
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))


-- | Source edits unlock only after a host-admitted test fails on base (a progress exists); submission needs a
-- proposal whose comparison verified it, recorded as its check and retained-test digest.
data Progress = Progress {artifact :: TestArtifact, proposal :: Maybe Proposal}


data Proposal = Proposal {candidate :: CapturedCandidate, verified :: Maybe (UUIDId "regression_check", Text)}


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
  , stage :: IORef (Maybe Progress)
  }


reproduce :: Repair -> AE.Value -> IO (Either Text AE.Value)
reproduce repair arguments =
  readIORef repair.stage >>= \case
    Nothing -> runExceptT do
      (script, failureMessage, inline, file) <- hoistEither $ first toText $ AET.parseEither (AE.withObject "Regression" \o -> (,,,) <$> o AE..: "script" <*> o AE..: "failureMessage" <*> o AE..:? "testPatch" <*> o AE..:? "testPatchFile") arguments
      testPatch <- case (inline, file) of
        (Just text, Nothing) -> pure text
        (Nothing, Just path) -> do
          (exit, bytes, _) <- liftIO $ repair.session.output (agentCommand ["/bin/sh", "-c", "exec /usr/bin/head -c 262145 -- \"$1\" 2>/dev/null", "monoscope-patch", resolve path]) (262144 + 2)
          when (exit /= Exited 0) $ throwE "Regression patch file could not be read"
          when (BS.length bytes > 262144) $ throwE "Regression patch file exceeds its 256 KiB limit"
          pure $ decodeUtf8 bytes
        _ -> throwE "Supply exactly one of testPatch or testPatchFile"
      when (T.null testPatch || T.length testPatch > 262144) $ throwE "Regression patch must contain 1 to 262144 characters"
      let artifact = TestArtifact script testPatch failureMessage
      rationale <- ExceptT $ repair.admit artifact
      when (T.null $ T.strip rationale) $ throwE "Host admission requires a recorded rationale"
      admission <- liftIO $ ((repair.artifacts </> "admissions") </>) . UUID.toString <$> UUID4.nextRandom
      liftIO $ createDirectoryIfMissing True admission >> publish (admission </> "admission.json") (AE.object ["projectId" AE..= repair.job.request.projectId, "runId" AE..= repair.job.request.runId, "issueId" AE..= repair.job.issueId, "attemptId" AE..= repair.job.request.attemptId, "base" AE..= repair.job.request.base, "regressionSha256" AE..= sha256 (toStrict $ AE.encode artifact), "rationale" AE..= rationale])
      (report, receipts) <- ExceptT $ repair.replay artifact [("base", repair.job.request.base)]
      case receipts of
        [baseline] | baseline.status == Failed -> do
          liftIO do
            repair.session.place "/run/monoscope-admitted-test.patch" $ encodeUtf8 testPatch
            unless (retained baseline) $ repair.session.root $ "/usr/bin/git" :| ["-c", "core.hooksPath=/dev/null", "apply", "--index", "/run/monoscope-admitted-test.patch"]
            repair.session.root $ "/bin/chown" :| ["-hR", "1000:1000", "/workspace"]
            writeIORef repair.stage $ Just $ Progress artifact Nothing
          pure $ AE.String $ "Regression reproduced and frozen. Source editing is now available.\n" <> decodeUtf8 (AE.encode report)
        _ -> throwE "The admitted regression did not fail on base; source edits remain locked"
    Just _ -> pure $ Left "A regression is already frozen for this run"


propose :: Repair -> Text -> IO (Either Text AE.Value)
propose repair rootCause =
  readIORef repair.stage
    >>= maybe
      (pure $ Left "Reproduce the issue before proposing a fix")
      ( \progress -> runExceptT do
          liftIO $ writeIORef repair.stage $ Just progress{proposal = Nothing}
          patch <- ExceptT $ capture repair
          input <- hoistEither $ maybeToRight "Invalid candidate patch or root cause" $ AET.parseMaybe AE.parseJSON $ AE.object ["patch" AE..= patch, "rootCause" AE..= rootCause]
          candidate <- ExceptT $ captureCandidate repair.artifacts repair.job repair.checkout input
          liftIO do
            repair.phase $ ProposeCandidate $ Candidate candidate.head rootCause
            writeIORef repair.stage $ Just progress{proposal = Just $ Proposal candidate Nothing}
          pure $ AE.toJSON candidate
      )


verify :: Repair -> IO (Either Text AE.Value)
verify repair =
  readIORef repair.stage >>= \case
    Just progress | Just proposal <- progress.proposal -> runExceptT do
      let record outcome = liftIO $ writeIORef repair.stage $ Just progress{proposal = Just proposal{verified = outcome}}
      record Nothing
      unchanged repair proposal.candidate "Workspace changed; propose and verify a new candidate"
      (report, receipts) <- ExceptT $ repair.replay progress.artifact [("base", repair.job.request.base), ("head", revisionText proposal.candidate.head)]
      let rendered = decodeUtf8 $ AE.encode report
      case receipts of
        [baseline, verified]
          | baseline.status == Failed
          , verified.status == Passed
          , Just (RetainedTest digest) <- verified.regression -> do
              record $ Just (verified.checkId, digest)
              pure $ AE.String $ "Frozen regression fails on base and passes on the candidate with its test retained.\n" <> rendered
        _ -> throwE $ "Candidate verification did not establish a failing base and passing retained test.\n" <> rendered
    _ -> pure $ Left "Capture a candidate after reproduction first"


-- | Completes the agent's summary into the verified result the host validates.
submission :: Repair -> AE.Value -> IO (Either Text AE.Value)
submission repair draft =
  readIORef repair.stage >>= \case
    Just (Progress _ (Just (Proposal candidate (Just (check, digest))))) -> runExceptT do
      summary <- hoistEither $ first toText $ AET.parseEither (AE.withObject "Fix submission" (AE..: "summary")) draft
      unchanged repair candidate "Workspace changed after verification; propose and verify a new candidate"
      pure $ AE.toJSON $ FixResult candidate.projectId candidate.runId candidate.issueId candidate.attemptId candidate.candidateId candidate.base candidate.head candidate.patchSha256 candidate.bundleSha256 candidate.rootCause check digest summary
    _ -> pure $ Left "Capture and verify a source fix before submission"


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
      | added /= Exited 0 || exit /= Exited 0 -> Left "Candidate patch could not be captured"
      | truncated -> Left "Candidate patch exceeds its 1 MiB limit"
      | otherwise -> Right $ decodeUtf8 bytes


-- | Guest paths resolve like the agent's file tools: home-relative, then workspace-relative.
resolve :: Text -> Text
resolve path
  | Just rest <- T.stripPrefix "~" path, T.null rest || "/" `T.isPrefixOf` rest = "/home/review-agent" <> rest
  | "/" `T.isPrefixOf` path = path
  | otherwise = "/workspace/" <> path
