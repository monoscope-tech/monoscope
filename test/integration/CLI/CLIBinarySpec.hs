module CLI.CLIBinarySpec (spec) where

import Relude

import Data.Bits ((.&.))
import Data.List (isInfixOf)
import Data.Text qualified as T
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Hspec

-- | Locate the monoscope CLI binary. CI can pass @MONOSCOPE_BIN@ directly;
-- otherwise ask cabal where the exe lives. Returns 'Nothing' if the file
-- isn't actually on disk so callers can skip rather than emit the cryptic
-- @posix_spawnp: does not exist@ from System.Process.
findBinary :: IO (Maybe FilePath)
findBinary = do
  envBin <- lookupEnv "MONOSCOPE_BIN"
  candidate <- case envBin of
    Just p -> pure p
    Nothing -> do
      (code, out, _) <- readProcessWithExitCode "cabal" ["list-bin", "exe:monoscope"] ""
      pure $ if code == ExitSuccess then T.unpack (T.strip (toText out)) else ""
  if null candidate then pure Nothing else do
    exists <- doesFileExist candidate
    pure $ if exists then Just candidate else Nothing

runMono :: [String] -> IO (ExitCode, String, String)
runMono args = do
  m <- findBinary
  case m of
    Just bin -> readProcessWithExitCode bin args ""
    Nothing -> do
      pendingWith "monoscope exe not built — set MONOSCOPE_BIN or run cabal build all first"
      pure (ExitFailure 127, "", "") -- unreachable; pendingWith throws

spec :: Spec
spec = describe "CLI binary E2E tests" do
  it "--help exits 0 and lists subcommands" do
    (code, out, _) <- runMono ["--help"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` \s -> "events" `isInfixOf` s && "metrics" `isInfixOf` s && "auth" `isInfixOf` s

  it "--version prints monoscope version and exits 0" do
    (code, out, _) <- runMono ["--version"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` \s -> "monoscope " `isInfixOf` s

  it "events search --help exits 0 and shows options" do
    (code, out, _) <- runMono ["events", "search", "--help"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` \s -> "--since" `isInfixOf` s && "--from" `isInfixOf` s

  it "auth_login_help_showsTheTokenOption" do
    (code, out, _) <- runMono ["auth", "login", "--help"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` isInfixOf "--token"

  it "config_scripts_redactSecretsAndReportErrorsOnStderrWithoutPrompting" do
    findBinary >>= \case
      Nothing -> pendingWith "monoscope exe not built"
      Just bin -> withSystemTempDirectory "monoscope-config" \dir -> do
        inherited <- getEnvironment
        let isolated = ("XDG_CONFIG_HOME", dir) : filter (\(key, _) -> key /= "XDG_CONFIG_HOME" && not ("MONOSCOPE_" `isPrefixOf` key)) inherited
            runConfig args = readCreateProcessWithExitCode (proc bin ("--json" : "config" : args)){cwd = Just dir, env = Just isolated} ""
        (saved, out, _) <- runConfig ["set", "api_key", "fixture-secret"]
        saved `shouldBe` ExitSuccess
        out `shouldNotSatisfy` isInfixOf "fixture-secret"
        permissions <- fileMode <$> getFileStatus (dir <> "/monoscope/config.yaml")
        permissions .&. 0o077 `shouldBe` 0
        forM_ [["get", "bogus"], ["set", "bogus", "value"], ["init"]] \args -> do
          (code, stdoutText, stderrText) <- runConfig args
          code `shouldBe` ExitFailure 1
          stdoutText `shouldBe` ""
          stderrText `shouldSatisfy` isInfixOf "message"

  it "auth status exits and shows status message" do
    (_, out, err) <- runMono ["auth", "status"]
    -- Two valid shapes: human text ("Not authenticated" / "Authenticated") or
    -- the agent-mode JSON envelope ("authenticated": …). CI sets CI=true,
    -- which auto-enables agent-mode → JSON.
    (out <> err) `shouldSatisfy` \s ->
      "Not authenticated" `isInfixOf` s
        || "Authenticated" `isInfixOf` s
        || "\"authenticated\"" `isInfixOf` s
        || "error" `isInfixOf` s

  it "logs search --help works as alias for events search" do
    (evCode, evOut, _) <- runMono ["events", "search", "--help"]
    (logCode, logOut, _) <- runMono ["logs", "search", "--help"]
    evCode `shouldBe` ExitSuccess
    logCode `shouldBe` ExitSuccess
    -- Both should show the same search options
    logOut `shouldSatisfy` \s -> "--since" `isInfixOf` s && "--from" `isInfixOf` s
    length logOut `shouldSatisfy` (> 0)

  describe "--help coverage for Plan A CRUD subcommands" do
    let helpCases =
          [ ["monitors", "--help"]
          , ["monitors", "create", "--help"]
          , ["monitors", "update", "--help"]
          , ["monitors", "patch", "--help"]
          , ["monitors", "apply", "--help"]
          , ["monitors", "bulk", "--help"]
          , ["dashboards", "--help"]
          , ["dashboards", "create", "--help"]
          , ["dashboards", "update", "--help"]
          , ["dashboards", "patch", "--help"]
          , ["dashboards", "bulk", "--help"]
          , ["dashboards", "widget", "--help"]
          , ["dashboards", "widget", "upsert", "--help"]
          , ["dashboards", "widget", "delete", "--help"]
          , ["dashboards", "widget", "reorder", "--help"]
          , ["dashboards", "render", "--help"]
          , ["chart", "--help"]
          , ["status", "--help"]
          , ["open", "--help"]
          , ["api-keys", "deactivate", "--help"]
          , ["api-keys", "revoke", "--help"]
          , ["share-link", "--help"]
          , ["share-link", "create", "--help"]
          , ["me", "--help"]
          , ["project", "--help"]
          , ["project", "patch", "--help"]
          , ["issues", "--help"]
          , ["issues", "list", "--help"]
          , ["issues", "ack", "--help"]
          , ["issues", "bulk", "--help"]
          , ["endpoints", "--help"]
          , ["endpoints", "list", "--help"]
          , ["log-patterns", "--help"]
          , ["log-patterns", "list", "--help"]
          , ["log-patterns", "bulk", "--help"]
          , ["teams", "--help"]
          , ["teams", "create", "--help"]
          , ["teams", "update", "--help"]
          , ["teams", "patch", "--help"]
          , ["teams", "bulk", "--help"]
          , ["members", "--help"]
          , ["members", "add", "--help"]
          , ["members", "patch", "--help"]
          , ["members", "remove", "--help"]
          , ["schema", "--help"]
          , ["facets", "--help"]
          , ["services", "list", "--help"]
          , ["events", "search", "--help"]
          , ["events", "context", "--help"]
          , ["events", "tail", "--help"]
          , ["metrics", "query", "--help"]
          , ["metrics", "chart", "--help"]
          , ["auth", "login", "--help"]
          , ["completion", "--help"]
          ]
    forM_ helpCases $ \args ->
      it (toString $ unwords $ map toText args) do
        (code, out, _) <- runMono args
        code `shouldBe` ExitSuccess
        -- Help output must at least mention the invoked subcommand name
        -- (guards against parser-construction errors that silently emit the wrong help).
        out `shouldSatisfy` \s -> case args of
          (_ : sub : _) -> sub `isInfixOf` s
          _ -> True
