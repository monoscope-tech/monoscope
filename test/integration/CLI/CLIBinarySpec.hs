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
