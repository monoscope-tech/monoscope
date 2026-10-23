{-# LANGUAGE PackageImports #-}

module Data.Effectful.AgentWorkspace.Guest (Host (..), Workspace (..), Lease (..), Setup (..), Exec (..), Operation (..), Session (..), withGuest, runSession) where

import Conduit (await, leftover, runConduit, (.|))
import Control.Exception (AsyncException (UserInterrupt))
import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.Conduit.Combinators qualified as CC
import Data.Effectful.AgentWorkspace (AllocationOutcome (..), AllocationReceipt, Command (..), Copy (..), Execution (..), GuestCommand (..), Image (..), ImageId (..), Isolation (..), LogExit (..), LoggedCommand (..), Overflow (..), RegressionOptions (..), Revision, Sandbox (..), TestArtifact (..), agentCommand, agentResources, hostAllocation, revisionText, withSandboxRelease)
import Data.Effectful.AgentWorkspace qualified as AgentWorkspace
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import GHC.Clock (getMonotonicTimeNSec)
import Pkg.Deriving (UUIDId (..))
import Relude
import System.Directory (createDirectory, getFileSize, removeFile)
import System.FilePath (takeFileName, (</>))
import System.IO (withBinaryFile)
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (proc, readProcess_)
import UnliftIO.Concurrent (myThreadId)
import UnliftIO.Exception (bracket, mask, onException, throwIO, throwTo, tryAny)
import "base64" Data.ByteString.Base64 qualified as Base64


-- | Trusted host inputs for an agent VM; skill directories are listed in precedence order.
data Host = Host {image :: Text, bridge :: FilePath, skills :: [FilePath], workspace :: Maybe Workspace}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | A trusted preparation script and its optional dependency archive select a prepared checkpoint.
data Workspace = Workspace {script :: FilePath, dependencyArchive :: Maybe FilePath}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | Where this run's prepared checkpoint is cached, and which pinned project/repository/base it must match.
data Lease = Lease {cacheDir :: FilePath, request :: AgentWorkspace.Request}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | Role-specific guest preparation that runs before the agent sees the workspace.
data Setup = Explore | InspectAdmission TestArtifact | ReviewEvidence (Map Text ByteString) | LockForReproduction


data Exec = Exec {command :: Text, cwd :: Text, timeout :: Maybe Double}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | A shell command's result; @output@ is the base64 tail and @log@ the full log's guest path.
data Ran = Ran {exitCode :: Int, timedOut :: Bool, truncated :: Bool, log :: Text, output :: Text}
  deriving stock (Generic)
  deriving anyclass (AE.ToJSON)


-- | File and shell calls of Pi's built-in tools, executed as the unprivileged review account.
data Operation = Run Exec | Read Text | Access Text | Writable Text | Write Text Text | Mkdir Text
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | A running agent VM: Pi's file and shell calls, root copies into the guest, and verifier settings for fresh VMs.
-- @output@ runs a command in /workspace and returns its exit status, retained bytes and whether the limit truncated them;
-- @root@ and @place@ change the workspace or /run as root and throw on failure.
data Session = Session {prepared :: Maybe AgentWorkspace.Leased, skills :: FilePath, imageBuildId :: ImageId, baseline :: AgentWorkspace.Baseline, operate :: Operation -> IO (Either Text AE.Value), copyIn :: FilePath -> Text -> IO (), verifier :: FilePath -> RegressionOptions, output :: NonEmpty Text -> Word32 -> IO (LogExit, ByteString, Bool), root :: NonEmpty Text -> IO (), place :: Text -> ByteString -> IO ()}


-- | Owns the agent VM for the action's lifetime and records its allocation however it ends: @allocate@ after a
-- completed run, @retain@ (journal only) when interrupted. Preparation failures are reported before the action
-- starts; the action's own exceptions propagate. Allocation acknowledgement failures fail completed runs.
withGuest :: Host -> Maybe (UUIDId "guest_image") -> FilePath -> Revision -> Setup -> Maybe Lease -> UUIDId "agent_attempt" -> (AllocationReceipt -> IO ()) -> (AllocationReceipt -> IO ()) -> (Session -> IO a) -> IO (Either Text a)
withGuest host expected checkout revision setup lease attempt allocate retain action = withSystemTempDirectory "monoscope-agent-guest" \temporary -> withLease \leased -> mask \restore -> do
  let skills = temporary </> "skills"
      logs = temporary </> "logs"
  traverse_ createDirectory [skills, logs]
  for_ (zip [0 :: Int ..] host.skills) \(index, path) -> void $ readProcess_ $ proc "cp" ["-RL", path, skills </> show index]
  started <- getMonotonicTimeNSec
  vmIdentity <- newIORef Nothing
  release <- newIORef ReleaseFailed
  let measured = do
        released <- readIORef release
        finished <- getMonotonicTimeNSec
        fmap (\identifier -> hostAllocation identifier attempt (max 1 $ (finished - started) `div` 1000000) released) <$> readIORef vmIdentity
      record send = measured >>= traverse_ send
  outcome <- (`onException` (void $ tryAny $ record retain)) $ tryAny $ restore $ withSandboxRelease host.bridge (writeIORef release) \rpc -> do
    let execute cwd argv = rpc $ Execute $ GuestCommand argv (Just cwd) False
        run cwd argv = execute cwd argv >>= \result -> if result.exitCode == 0 then pure result.stdout else failure "Guest preparation failed"
        workspace = run "/workspace" . fromList
        copyIn source destination = rpc (CopyFile $ Copy source destination) >> void (run "/" $ "/bin/chmod" :| ["0644", destination])
        upload destination bytes = do
          let staged = temporary </> takeFileName (toString destination)
          writeFileBS staged bytes
          rpc $ CopyFile $ Copy staged destination
        lock = traverse_ workspace [["/usr/bin/git", "config", "--system", "--add", "safe.directory", "/workspace"], ["/bin/chown", "-hR", "0:0", "/workspace"], ["/bin/chmod", "-R", "u+rwX,go+rX,go-w", "/workspace"], ["/usr/bin/git", "update-index", "--refresh"]]
        logged argv cwd limit = do
          path <- (logs </>) . (<> ".log") . UUID.toString <$> UUID4.nextRandom
          writeFileBS path ""
          exit <- rpc $ ExecuteLogged $ LoggedCommand (GuestCommand argv (Just cwd) False) path limit DiscardOutput
          size <- getFileSize path
          pure (path, exit, size >= toInteger limit)
        output argv limit = do
          (path, exit, truncated) <- logged argv "/workspace" limit
          bytes <- readFileBS path <* removeFile path
          pure (exit, bytes, truncated)
        -- Pi's mutations: errors reach the model, and the agent account's permissions decide what may change.
        mutate argv =
          output (agentCommand argv) 65536 <&> \case
            (Exited 0, _, _) -> Right AE.Null
            (_, bytes, _) -> Left $ T.take 1000 $ decodeUtf8 bytes
        probe argv message = execute "/" (agentCommand argv) <&> \result -> if result.exitCode == 0 then Right AE.Null else Left message
        encoded = extractBase64 . Base64.encodeBase64
        operate = \case
          Run exec -> do
            let limited = maybe [] (\seconds -> ["/usr/bin/timeout", "--kill-after=5", show (ceiling seconds :: Word32)]) exec.timeout
            (path, exit, truncated) <- logged (agentCommand $ limited <> ["/bin/bash", "-lc", exec.command]) exec.cwd (64 * 1048576)
            let guestLog = "/run/monoscope-review-output/" <> toText (takeFileName path)
            copyIn path guestLog
            retained <- BS.takeEnd 1048576 <$> readFileBS path
            removeFile path
            pure $ case exit of
              LogLimit -> Left "Command output could not be retained"
              Exited code -> Right $ AE.toJSON $ Ran code (isJust exec.timeout && code == 124) truncated guestLog (encoded retained)
          Read path ->
            output (agentCommand ["/usr/bin/head", "-c", "8388609", "--", path]) (8 * 1048576 + 1) <&> \case
              (Exited 0, bytes, False) -> Right $ AE.String $ encoded bytes
              (Exited 0, _, True) -> Left "File exceeds the 8 MiB read limit"
              (_, bytes, _) -> Left $ T.take 1000 $ decodeUtf8 bytes
          Access path -> probe ["/usr/bin/test", "-r", path] ("ENOENT: " <> path)
          Writable path -> probe ["/bin/sh", "-c", "test -r \"$1\" && test -w \"$1\"", "monoscope-edit", path] ("Permission denied: " <> path)
          Mkdir path -> mutate ["/bin/mkdir", "-p", "--", path]
          Write path content -> do
            staged <- ("/run/monoscope-write-" <>) . UUID.toText <$> UUID4.nextRandom
            upload staged (encodeUtf8 content) >> void (run "/" $ "/bin/chmod" :| ["0444", staged])
            -- A redirect keeps an existing file's mode and creates new files from the agent's umask.
            mutate ["/bin/sh", "-c", "/bin/cat -- \"$1\" > \"$2\"", "monoscope-write", staged, path] <* run "/" ("/bin/rm" :| ["-f", "--", staged])
    prepared <- tryAny do
      image <- rpc $ Resolve host.image
      for_ expected \(UUIDId build) -> unless (image.buildId == ImageId build) $ failure "Configured guest image differs from the pinned image"
      let sandbox = uncurry (Sandbox image.assetDir) agentResources OfflineCow $ Map.fromList [("/checkout", checkout), ("/run/monoscope-skills", skills)]
      case leased of
        Just held -> rpc $ Resume (sandbox, held.resumePath)
        Nothing | any (isJust . (.dependencyArchive)) host.workspace -> failure "Configured dependency workspace has not been prepared for these pinned inputs"
        Nothing -> rpc $ Create sandbox
      rpc (GetIdentity ()) >>= writeIORef vmIdentity . Just
      -- Gondolin mounts are root-only, so the agent account reads guest-owned copies.
      traverse_ (run "/" . fromList) [["/bin/mkdir", "-p", "/workspace", "/skills", "/run/monoscope-review-output"], ["/bin/cp", "-R", "/run/monoscope-skills/.", "/skills"], ["/bin/chmod", "-R", "a+rX,go-w", "/skills"]]
      AgentWorkspace.checkoutGuest (run "/") (isJust leased) (revisionText revision)
      void $ workspace ["/bin/chown", "-hR", "1000:1000", "/workspace"]
      account <- workspace $ toList $ agentCommand ["/usr/bin/id", "-u"]
      unless (T.strip account == "1000") $ failure "Guest image requires the review-agent account and setpriv"
      case setup of
        Explore -> pass
        InspectAdmission artifact -> do
          lock
          upload "/run/monoscope-admission.patch" $ encodeUtf8 artifact.testPatch
          paths <- filter (not . T.null) . T.splitOn "\0" <$> workspace ["/usr/bin/git", "apply", "--numstat", "-z", "/run/monoscope-admission.patch"]
          unless (not (null paths) && length paths <= 100) $ failure "Regression patch requires between one and 100 paths"
          void $ workspace ["/usr/bin/git", "-c", "core.hooksPath=/dev/null", "apply", "--index", "/run/monoscope-admission.patch"]
          upload "/run/monoscope-admission.json" $ toStrict $ AE.encode artifact
          void $ workspace ["/bin/chmod", "0444", "/run/monoscope-admission.patch", "/run/monoscope-admission.json"]
          lock
        LockForReproduction -> lock
        ReviewEvidence files -> do
          lock
          for_ (Map.toList files) \(destination, bytes) -> upload destination bytes >> workspace ["/bin/chmod", "0444", destination]
      pure image.buildId
    whenRight (Left "Agent sandbox could not be prepared") prepared \imageBuildId ->
      Right <$> action (Session leased skills imageBuildId (AgentWorkspace.leasedBaseline leased) operate copyIn (\artifacts -> RegressionOptions artifacts checkout host.image host.bridge) output (void . workspace . toList) upload)
  receipt <- measured
  case outcome of
    Left failed -> void (tryAny $ traverse_ allocate receipt) >> throwIO failed
    Right result -> restore (traverse_ allocate receipt $> result) `onException` void (tryAny $ traverse_ retain receipt)
  where
    failure = throwIO . userError
    withLease use = case (host.workspace, lease) of
      (Just configured, Just held) -> AgentWorkspace.leaseWorkspace (AgentWorkspace.Options held.cacheDir host.image host.bridge configured.script configured.dependencyArchive) held.request use
      _ -> use Nothing


-- | Offline research uses the same VM lifetime, tools and verifier as the server.
data SessionRequest = SessionRequest {host :: Host, expectedImageBuildId :: Maybe (UUIDId "guest_image"), checkout :: FilePath, base :: Revision, head :: Revision, readOnly :: Bool, lease :: Maybe Lease, projectId :: UUIDId "project", runId :: UUIDId "pr_review", attemptId :: UUIDId "agent_attempt", artifactDir :: FilePath, ownerPid :: Maybe Word32}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


data Message = Operate Operation | Check AE.Value
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


runSession :: FilePath -> IO ()
runSession path = do
  thread <- myThreadId
  bracket (installHandler sigTERM (Catch $ throwTo thread UserInterrupt) Nothing) (\previous -> void $ installHandler sigTERM previous Nothing) $ const $ serve path
  where
    serve requestFile = do
      requestBytes <- withBinaryFile requestFile ReadMode (`BS.hGet` 65537)
      when (BS.length requestBytes > 65536) $ fail "Native session request exceeds its size limit"
      request <- either (const $ fail "Invalid native session request") pure $ AE.eitherDecodeStrict @SessionRequest requestBytes
      AgentWorkspace.withOwner request.ownerPid $ do
        verifierSession <- UUID4.nextRandom
        let send :: AE.ToJSON a => a -> IO ()
            send value = putLBS (AE.encode value <> "\n") >> hFlush Relude.stdout
            record receipt = send $ AE.object ["event" AE..= ("agent_allocation" :: Text), "receipt" AE..= receipt]
        result <- withGuest request.host request.expectedImageBuildId request.checkout request.head (if request.readOnly then LockForReproduction else Explore) request.lease request.attemptId record record $ \session -> do
          send $ Right @Text $ AE.object ["skills" AE..= session.skills, "prepared" AE..= session.prepared]
          let line size chunks =
                await >>= \case
                  Nothing -> if size == 0 then pure Nothing else fail "Incomplete native session message"
                  Just chunk -> do
                    let (prefix, suffix) = BS.break (== 10) chunk
                        total = size + BS.length prefix
                    when (total > 16 * 1024 * 1024) $ fail "Native session message exceeds its size limit"
                    if BS.null suffix
                      then line total (prefix : chunks)
                      else leftover (BS.drop 1 suffix) $> Just (BS.concat $ reverse $ prefix : chunks)
              receive =
                line 0 []
                  >>= traverse_
                    ( \bytes -> do
                        response <- liftIO $ do
                          message <- either (const $ fail "Invalid native session message") pure $ AE.eitherDecodeStrict @Message bytes
                          case message of
                            Operate operation -> session.operate operation
                            Check artifact -> do
                              let options = session.verifier request.artifactDir
                              checked <- tryAny $ do
                                regression <- either (const $ fail "Invalid native regression request") pure $ AgentWorkspace.mkRegressionRequest request.projectId request.runId request.attemptId verifierSession (revisionText request.base) session.imageBuildId session.baseline artifact [("base", revisionText request.base), ("head", revisionText request.head)]
                                verified <- AgentWorkspace.verifyRegression options regression
                                traverse_ (uncurry session.copyIn) $ AgentWorkspace.regressionLogs options verified
                                pure $ AE.toJSON verified
                              pure $ first (maybe "Local check could not run" AgentWorkspace.regressionErrorText . fromException) checked
                        liftIO $ send response
                        receive
                    )
          runConduit $ CC.sourceHandle stdin .| receive
        for_ (leftToMaybe result) $ send . Left @Text @AE.Value
