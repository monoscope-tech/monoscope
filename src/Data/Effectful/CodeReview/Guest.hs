module Data.Effectful.CodeReview.Guest (Host (..), Setup (..), Exec (..), Operation (..), withGuest) where

import Data.Aeson qualified as AE
import Data.Base64.Types (extractBase64)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Effectful.AgentWorkspace (Command (..), Copy (..), Execution (..), GuestCommand (..), Image (..), ImageId (..), Isolation (..), LogExit (..), LoggedCommand (..), Overflow (..), Sandbox (..), SandboxFailure (..), agentCommand, withSandbox)
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
import UnliftIO.Exception (throwIO, tryAny)


-- | Trusted host inputs for an agent VM; skill directories are listed in precedence order.
-- @backend@ overrides the inherited Gondolin hypervisor selection.
data Host = Host {image :: Text, bridge :: FilePath, skills :: [FilePath], backend :: Maybe Text}


-- | Role-specific guest preparation that runs before the agent sees the workspace.
data Setup = Explore | InspectAdmission TestArtifact


data Exec = Exec {command :: Text, cwd :: Text, timeout :: Maybe Double}
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | File and shell calls of Pi's built-in tools, executed as the unprivileged review account.
data Operation = Run Exec | Read Text | Access Text
  deriving stock (Generic)
  deriving anyclass (AE.FromJSON)


-- | Owns the agent VM for the action's lifetime and journals its allocation however it ends.
-- Preparation failures are reported before the action starts; the action's own exceptions propagate.
withGuest :: Host -> Maybe (UUIDId "guest_image") -> FilePath -> Revision -> Setup -> UUIDId "agent_attempt" -> (AllocationReceipt -> IO ()) -> (FilePath -> (Operation -> IO (Either Text AE.Value)) -> IO a) -> IO (Either Text a)
withGuest host expected checkout revision setup attempt allocate action = withSystemTempDirectory "monoscope-agent-guest" \temporary -> do
  let skills = temporary </> "skills"
      logs = temporary </> "logs"
  traverse_ createDirectory [skills, logs]
  for_ (zip [0 :: Int ..] host.skills) \(index, path) -> void $ readProcess_ $ proc "cp" ["-RL", path, skills </> show index]
  started <- getMonotonicTimeNSec
  vmIdentity <- newIORef Nothing
  outcome <- tryAny $ withSandbox [("GONDOLIN_VMM", toString vmm) | Just vmm <- [host.backend]] host.bridge \rpc -> do
    let execute cwd argv = rpc @Execution $ Execute $ GuestCommand argv (Just cwd) False
        run cwd argv = execute cwd argv >>= \result -> if result.exitCode == 0 then pure result.stdout else failure "Guest preparation failed"
        workspace = run "/workspace" . fromList
        git args = workspace $ ["/usr/bin/git", "-c", "safe.directory=/workspace", "-c", "core.hooksPath=/dev/null", "-C", "/workspace"] <> args
        upload name bytes = do
          writeFileBS (temporary </> name) bytes
          rpc @() $ CopyFile $ Copy (temporary </> name) (toText $ "/run" </> name)
        lock = traverse_ workspace [["/usr/bin/git", "config", "--system", "--add", "safe.directory", "/workspace"], ["/bin/chown", "-hR", "0:0", "/workspace"], ["/bin/chmod", "-R", "u+rwX,go+rX,go-w", "/workspace"], ["/usr/bin/git", "update-index", "--refresh"]]
        logged argv cwd limit = do
          path <- (logs </>) . (<> ".log") . UUID.toString <$> UUID4.nextRandom
          writeFileBS path ""
          exit <- rpc @LogExit $ ExecuteLogged $ LoggedCommand (GuestCommand argv (Just cwd) False) path limit DiscardOutput
          size <- getFileSize path
          pure (path, exit, size >= toInteger limit)
        operate = \case
          Run exec -> do
            let limited = maybe [] (\seconds -> ["/usr/bin/timeout", "--kill-after=5", show (ceiling seconds :: Word32)]) exec.timeout
            (path, exit, truncated) <- logged (agentCommand $ limited <> ["/bin/bash", "-lc", exec.command]) exec.cwd (64 * 1048576)
            let guestLog = "/run/monoscope-review-output/" <> toText (takeFileName path)
            rpc @() $ CopyFile $ Copy path guestLog
            output <- BS.takeEnd 1048576 <$> readFileBS path
            removeFile path
            pure $ case exit of
              LogLimit -> Left "Command output could not be retained"
              Exited code -> Right $ AE.object ["exitCode" AE..= code, "timedOut" AE..= (isJust exec.timeout && code == 124), "truncated" AE..= truncated, "log" AE..= guestLog, "output" AE..= extractBase64 (Base64.encodeBase64 output)]
          Read path -> do
            (file, exit, truncated) <- logged (agentCommand ["/bin/cat", "--", path]) "/" (8 * 1048576 + 1)
            bytes <- readFileBS file <* removeFile file
            pure $ case exit of
              Exited 0 | not truncated -> Right $ AE.String $ extractBase64 $ Base64.encodeBase64 bytes
              Exited 0 -> Left "File exceeds the 8 MiB read limit"
              _ -> Left $ T.take 1000 $ decodeUtf8 bytes
          Access path -> execute "/" (agentCommand ["/usr/bin/test", "-r", path]) <&> \result -> if result.exitCode == 0 then Right AE.Null else Left ("ENOENT: " <> path)
    prepared <- tryAny do
      image <- rpc @Image $ Resolve host.image
      for_ expected \(UUIDId build) -> unless (image.buildId == ImageId build) $ failure "Configured guest image differs from the pinned image"
      rpc @() $ Create $ uncurry (Sandbox image.assetDir) agentResources OfflineCow $ Map.fromList [("/checkout", checkout), ("/skills", skills)]
      rpc @Text (GetIdentity ()) >>= writeIORef vmIdentity . fmap UUIDId . UUID.fromText
      traverse_ (run "/" . fromList) [["/bin/mkdir", "-p", "/workspace", "/run/monoscope-review-output"], ["/bin/cp", "-R", "/checkout/.", "/workspace"]]
      void $ git ["checkout", "--quiet", "--detach", revisionText revision]
      void $ workspace ["/bin/chown", "-hR", "1000:1000", "/workspace"]
      account <- workspace $ toList $ agentCommand ["/usr/bin/id", "-u"]
      unless (T.strip account == "1000") $ failure "Guest image requires the review-agent account and setpriv"
      case setup of
        Explore -> pass
        InspectAdmission artifact -> do
          lock
          upload "monoscope-admission.patch" $ encodeUtf8 artifact.testPatch
          paths <- filter (not . T.null) . T.splitOn "\0" <$> workspace ["/usr/bin/git", "apply", "--numstat", "-z", "/run/monoscope-admission.patch"]
          unless (not (null paths) && length paths <= 100) $ failure "Regression patch requires between one and 100 paths"
          void $ workspace ["/usr/bin/git", "-c", "core.hooksPath=/dev/null", "apply", "--index", "/run/monoscope-admission.patch"]
          upload "monoscope-admission.json" $ toStrict $ AE.encode artifact
          void $ workspace ["/bin/chmod", "0444", "/run/monoscope-admission.patch", "/run/monoscope-admission.json"]
          lock
    whenRight (Left "Agent sandbox could not be prepared") prepared $ const $ Right <$> action skills operate
  finished <- getMonotonicTimeNSec
  vm <- readIORef vmIdentity
  for_ vm \identifier -> do
    let released = case outcome of Left failed | Just SandboxCleanupFailed <- fromException failed -> ReleaseFailed; _ -> Released
    void $ tryAny $ allocate $ hostAllocation identifier attempt (max 1 $ (finished - started) `div` 1000000) released
  either throwIO pure outcome
  where
    failure = throwIO . userError
