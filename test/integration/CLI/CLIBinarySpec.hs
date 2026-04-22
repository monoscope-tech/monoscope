module CLI.CLIBinarySpec (spec) where

import Relude

import Data.List (isInfixOf)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

findBinary :: IO FilePath
findBinary = T.unpack . T.strip . toText <$> readProcess "cabal" ["list-bin", "exe:monoscope"] ""

readProcess :: FilePath -> [String] -> String -> IO String
readProcess cmd args input' = do
  (_, out, _) <- readProcessWithExitCode cmd args input'
  pure out

runMono :: [String] -> IO (ExitCode, String, String)
runMono args = do
  bin <- findBinary
  readProcessWithExitCode bin args ""

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

  it "auth status without credentials shows error message" do
    (_, out, err) <- runMono ["auth", "status"]
    (out <> err) `shouldSatisfy` \s -> "Not authenticated" `isInfixOf` s || "error" `isInfixOf` s

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
