-- | Drives the real CLI binary's code path in-process. Lives in the test suite
-- rather than "Pkg.TestUtils" because it straddles both packages: the CLI is
-- 'monoscope-cli' (which must never link libpq) while the fake transport routes
-- into lib:monoscope's servant handlers.
module CLI.Harness (runCLILifecycle) where

import CLI.Main qualified as CLIMain
import Data.UUID qualified as UUID
import Data.Version (makeVersion)
import Effectful (runEff)
import Effectful.Environment (runEnvironment)
import Effectful.FileSystem (runFileSystem)
import Options.Applicative qualified as OA
import Pkg.TestUtils (TestResources, runHTTPtoServant)
import Relude
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO.Silently qualified as Silently
import UnliftIO.Exception (bracket_, try)


-- | Drive the real CLI top-down: the actual optparse parser, the actual command
-- pipeline, with the HTTP effect routed to handlers via 'runHTTPtoServant'.
-- Returns captured stdout plus the exit code (a command's @exitFailure@ surfaces
-- as @ExitFailure 1@; anything else as 'ExitSuccess'). Pass @--json@ in args for
-- deterministic output regardless of TTY state.
runCLILifecycle :: TestResources -> [String] -> IO (ExitCode, Text)
runCLILifecycle tr args = do
  -- MONOSCOPE_TEST_API_KEY lets a test inject a real project key (needed by
  -- ingestion paths that authenticate the key, e.g. send-event → OTLP).
  key <- fromMaybe "test-key" <$> lookupEnv "MONOSCOPE_TEST_API_KEY"
  bracket_
    (setEnv "MONOSCOPE_API_KEY" key >> setEnv "MONOSCOPE_PROJECT" (UUID.toString UUID.nil))
    (unsetEnv "MONOSCOPE_API_KEY" >> unsetEnv "MONOSCOPE_PROJECT")
    do
      (global, cmd) <- case OA.execParserPure OA.defaultPrefs (CLIMain.parserInfo testVersion) args of
        OA.Success r -> pure r
        OA.Failure f -> error $ "CLI parse failure: " <> toText (fst (OA.renderFailure f "monoscope"))
        OA.CompletionInvoked _ -> error "CLI completion invoked in test"
      (out, res) <-
        Silently.capture
          $ try @IO @ExitCode
          $ runEff
          $ runHTTPtoServant tr
          $ runEnvironment
          $ runFileSystem
          $ CLIMain.run testVersion global cmd
      pure (fromLeft ExitSuccess res, toText out)
  where
    testVersion = makeVersion [0, 0, 0]
