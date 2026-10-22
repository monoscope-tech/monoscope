module Data.Effectful.CodeReview.Guest (Host (..), Workspace (..), Lease (..), Setup (..), Exec (..), Operation (..), Session (..), withGuest) where

import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import "base64" Data.ByteString.Base64 qualified as Base64
import Data.Effectful.AgentWorkspace (Command (..), Copy (..), Execution (..), GuestCommand (..), Image (..), ImageId (..), Isolation (..), LogExit (..), LoggedCommand (..), Overflow (..), RegressionOptions (..), Sandbox (..), SandboxFailure (..), agentCommand, withSandbox)
import Data.Effectful.AgentWorkspace qualified as AgentWorkspace
import Data.Effectful.CodeReview.Types (AllocationOutcome (..), AllocationReceipt, Revision, TestArtifact (..), agentResources, hostAllocation, revisionText)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID4
import GHC.Clock (getMonotonicTimeNSec)
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Directory (createDirectory, getFileSize, removeFile)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (userError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess_)
import UnliftIO.Exception (onException, throwIO, tryAny)


-- | Trusted host inputs for an agent VM; skill directories are listed in precedence order.
data Host = Host {image :: Text, bridge :: FilePath, skills :: [FilePath], workspace :: Maybe Workspace}


-- | A trusted preparation script and its optional dependency archive select a prepared checkpoint.
data Workspace = Workspace {script :: FilePath, dependencyArchive :: Maybe FilePath}


-- | Where this run's prepared checkpoint is cached, and which pinned project/repository/base it must match.
data Lease = Lease {cacheDir :: FilePath, request :: AgentWorkspace.Request}


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
data Session = Session {skills :: FilePath, imageBuildId :: ImageId, baseline :: AgentWorkspace.Baseline, operate :: Operation -> IO (Either Text AE.Value), copyIn :: FilePath -> Text -> IO (), verifier :: FilePath -> RegressionOptions, output :: NonEmpty Text -> Word32 -> IO (LogExit, ByteString, Bool), root :: NonEmpty Text -> IO (), place :: Text -> ByteString -> IO ()}


-- | Owns the agent VM for the action's lifetime and records its allocation however it ends: @allocate@ after a
-- completed run, @retain@ (journal only) when interrupted. Preparation failures are reported before the action
-- starts; the action's own exceptions propagate.
withGuest :: Host -> Maybe (UUIDId "guest_image") -> FilePath -> Revision -> Setup -> Maybe Lease -> UUIDId "agent_attempt" -> (AllocationReceipt -> IO ()) -> (AllocationReceipt -> IO ()) -> (Session -> IO a) -> IO (Either Text a)
withGuest host expected checkout revision setup lease attempt allocate retain action = withSystemTempDirectory "monoscope-agent-guest" \temporary -> withLease \leased -> do
  let skills = temporary </> "skills"
      logs = temporary </> "logs"
  traverse_ createDirectory [skills, logs]
  for_ (zip [0 :: Int ..] host.skills) \(index, path) -> void $ readProcess_ $ proc "cp" ["-RL", path, skills </> show index]
  started <- getMonotonicTimeNSec
  vmIdentity <- newIORef Nothing
  let measured released = do
        finished <- getMonotonicTimeNSec
        fmap (\identifier -> hostAllocation identifier attempt (max 1 $ (finished - started) `div` 1000000) released) <$> readIORef vmIdentity
  outcome <- (`onException` (measured Released >>= traverse_ retain)) $ tryAny $ withSandbox host.bridge \rpc -> do
    let execute cwd argv = rpc @Execution $ Execute $ GuestCommand argv (Just cwd) False
        run cwd argv = execute cwd argv >>= \result -> if result.exitCode == 0 then pure result.stdout else failure "Guest preparation failed"
        workspace = run "/workspace" . fromList
        git args = workspace $ ["/usr/bin/git", "-c", "safe.directory=/workspace", "-c", "core.hooksPath=/dev/null", "-C", "/workspace"] <> args
        copyIn source destination = rpc @() (CopyFile $ Copy source destination) >> void (run "/" $ "/bin/chmod" :| ["0644", destination])
        upload destination bytes = do
          let staged = temporary </> takeFileName (toString destination)
          writeFileBS staged bytes
          rpc @() $ CopyFile $ Copy staged destination
        lock = traverse_ workspace [["/usr/bin/git", "config", "--system", "--add", "safe.directory", "/workspace"], ["/bin/chown", "-hR", "0:0", "/workspace"], ["/bin/chmod", "-R", "u+rwX,go+rX,go-w", "/workspace"], ["/usr/bin/git", "update-index", "--refresh"]]
        logged argv cwd limit = do
          path <- (logs </>) . (<> ".log") . UUID.toString <$> UUID4.nextRandom
          writeFileBS path ""
          exit <- rpc @LogExit $ ExecuteLogged $ LoggedCommand (GuestCommand argv (Just cwd) False) path limit DiscardOutput
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
      image <- rpc @Image $ Resolve host.image
      for_ expected \(UUIDId build) -> unless (image.buildId == ImageId build) $ failure "Configured guest image differs from the pinned image"
      let sandbox = uncurry (Sandbox image.assetDir) agentResources OfflineCow $ Map.fromList [("/checkout", checkout), ("/run/monoscope-skills", skills)]
      case leased of
        Just held -> rpc @() $ Resume (sandbox, held.resumePath)
        Nothing | any (isJust . (.dependencyArchive)) host.workspace -> failure "Configured dependency workspace has not been prepared for these pinned inputs"
        Nothing -> rpc @() $ Create sandbox
      rpc @Text (GetIdentity ()) >>= writeIORef vmIdentity . fmap UUIDId . UUID.fromText
      -- Gondolin mounts are root-only, so the agent account reads guest-owned copies.
      traverse_ (run "/" . fromList) [["/bin/mkdir", "-p", "/workspace", "/skills", "/run/monoscope-review-output"], ["/bin/cp", "-R", "/run/monoscope-skills/.", "/skills"], ["/bin/chmod", "-R", "a+rX,go-w", "/skills"]]
      -- A resumed checkpoint keeps its built tree and only fetches the pinned revision's objects.
      if isJust leased
        then do
          for_ ["/workspace", "/checkout/.git"] \path -> run "/" $ "/usr/bin/git" :| ["config", "--system", "--add", "safe.directory", path]
          void $ git ["fetch", "--quiet", "--no-tags", "--", "/checkout", revisionText revision]
        else void $ run "/" $ "/bin/cp" :| ["-R", "/checkout/.", "/workspace"]
      void $ git ["checkout", "--quiet", "--detach", revisionText revision]
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
      Right <$> action (Session skills imageBuildId (AgentWorkspace.leasedBaseline leased) operate copyIn (\artifacts -> RegressionOptions artifacts checkout host.image host.bridge) output (void . workspace . toList) upload)
  measured (case outcome of Left failed | Just SandboxCleanupFailed <- fromException failed -> ReleaseFailed; _ -> Released)
    >>= traverse_ (void . tryAny . allocate)
  either throwIO pure outcome
  where
    failure = throwIO . userError
    withLease use = case (host.workspace, lease) of
      (Just configured, Just held) -> AgentWorkspace.leaseWorkspace (AgentWorkspace.Options held.cacheDir host.image host.bridge configured.script configured.dependencyArchive) held.request use
      _ -> use Nothing
