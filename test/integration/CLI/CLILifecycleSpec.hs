-- | Lifecycle tests that drive the real CLI top-down, in-process: the actual
-- optparse parser and command pipeline run against real handlers + test DB via
-- 'runCLILifecycle' (HTTP effect interpreted by 'runHTTPtoServant'). Each
-- @describe@ block walks one user workflow end-to-end the way the skills
-- (github.com/monoscope-tech/skills) document it — these tests pin the
-- envelopes and behaviors the skills promise.
module CLI.CLILifecycleSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import Pkg.TestUtils
import Relude
import System.Exit (ExitCode (..))
import Test.Hspec


-- | Rows from a CLI list response — accepts both the normalised
-- @{data: [...]}@ envelope and a bare JSON array.
listRows :: AE.Value -> [AE.Value]
listRows = \case
  AE.Object o | Just (AE.Array a) <- KM.lookup "data" o -> V.toList a
  AE.Array a -> V.toList a
  _ -> []


rowId :: AE.Value -> String
rowId = \case
  AE.Object o | Just (AE.String i) <- KM.lookup "id" o -> toString i
  v -> error $ "expected row object with id, got: " <> show v


-- | Decode captured stdout as a single JSON value.
jsonOut :: Text -> IO AE.Value
jsonOut out = case AE.eitherDecodeStrict' (encodeUtf8 out) of
  Right v -> pure v
  Left err -> do
    expectationFailure $ "expected single JSON value on stdout: " <> err <> "\n--- stdout ---\n" <> take 500 (toString out)
    error "unreachable"


shouldHaveKeys :: AE.Value -> [Text] -> Expectation
shouldHaveKeys (AE.Object obj) keys = forM_ keys \k -> unless (KM.member (fromString (toString k)) obj) do
  expectationFailure $ "missing key " <> toString k <> " in: " <> take 300 (decodeUtf8 (AE.encode obj))
shouldHaveKeys other _ = expectationFailure $ "expected JSON object, got: " <> show other


spec :: Spec
spec = aroundAll withTestResources do
  describe "CLI lifecycle (in-process, real parser + handlers)" do
    it "version prints and exits zero (harness smoke)" \tr -> do
      (ec, out) <- runCLILifecycle tr ["version"]
      ec `shouldBe` ExitSuccess
      toString out `shouldContain` "monoscope"

    it "me --json returns project identity envelope" \tr -> do
      (ec, out) <- runCLILifecycle tr ["--json", "me"]
      ec `shouldBe` ExitSuccess
      v <- jsonOut out
      v `shouldHaveKeys` ["project", "project_id"]

  describe "monitors-as-code lifecycle" do
    it "apply is idempotent by title; mute/unmute/delete round-trip" \tr -> do
      let yamlPath = "/tmp/monoscope-lifecycle-monitor.yaml"
      writeFileText yamlPath
        $ unlines
          [ "title: Lifecycle error-rate alert"
          , "query: 'severity.text == \"ERROR\"'"
          , "alert_threshold: 10"
          , "trigger_less_than: false"
          , "check_interval_mins: 5"
          , "time_window_mins: 15"
          , "emails: []"
          , "slack_channels: []"
          , "teams: []"
          ]

      (ec1, _) <- runCLILifecycle tr ["--json", "monitors", "apply", yamlPath]
      ec1 `shouldBe` ExitSuccess
      -- Re-applying the same id-less file must update the existing monitor
      -- (natural key: title), not create a duplicate.
      (ec2, _) <- runCLILifecycle tr ["--json", "monitors", "apply", yamlPath]
      ec2 `shouldBe` ExitSuccess

      (_, listOut) <- runCLILifecycle tr ["--json", "monitors", "list"]
      listV <- jsonOut listOut
      listRows listV `shouldSatisfy` ((== 1) . length)

      let mid = case listRows listV of
            [row] -> rowId row
            _ -> error "expected exactly one monitor"

      -- yaml dump round-trips through apply without creating a duplicate
      (ecY, yamlDump) <- runCLILifecycle tr ["monitors", "yaml", mid]
      ecY `shouldBe` ExitSuccess
      let dumpPath = "/tmp/monoscope-lifecycle-monitor-dump.yaml"
      writeFileText dumpPath yamlDump
      (ecA3, _) <- runCLILifecycle tr ["--json", "monitors", "apply", dumpPath]
      ecA3 `shouldBe` ExitSuccess
      (_, listOut3) <- runCLILifecycle tr ["--json", "monitors", "list"]
      jsonOut listOut3 >>= \v -> listRows v `shouldSatisfy` ((== 1) . length)

      -- mute with a duration, unmute, then delete — the triage-skill loop
      (ecMute, _) <- runCLILifecycle tr ["--json", "monitors", "mute", mid, "--for", "30"]
      ecMute `shouldBe` ExitSuccess
      (ecUnmute, _) <- runCLILifecycle tr ["--json", "monitors", "unmute", mid]
      ecUnmute `shouldBe` ExitSuccess
      (ecDel, _) <- runCLILifecycle tr ["--json", "monitors", "delete", mid]
      ecDel `shouldBe` ExitSuccess

      (_, listOut2) <- runCLILifecycle tr ["--json", "monitors", "list"]
      listV2 <- jsonOut listOut2
      listRows listV2 `shouldBe` []

  describe "dashboards-as-code lifecycle" do
    it "yaml dump round-trips through apply keyed on file_path; duplicate/star/delete" \tr -> do
      let createPath = "/tmp/monoscope-lifecycle-dashboard.yaml"
          dumpPath = "/tmp/monoscope-lifecycle-dashboard-dump.yaml"
      writeFileText createPath
        $ unlines
          [ "title: Lifecycle dashboard"
          , "file_path: dashboards/lifecycle.yaml"
          ]
      (ecC, _) <- runCLILifecycle tr ["--json", "dashboards", "create", createPath]
      ecC `shouldBe` ExitSuccess

      (_, listOut) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      did <-
        jsonOut listOut >>= \v -> case listRows v of
          [row] -> pure (rowId row)
          rows -> expectationFailure ("expected exactly one dashboard, got " <> show (length rows)) >> error "unreachable"

      -- the canonical as-code loop: dump → apply (twice) → still exactly one
      (ecY, yamlDump) <- runCLILifecycle tr ["dashboards", "yaml", did]
      ecY `shouldBe` ExitSuccess
      writeFileText dumpPath yamlDump
      (ecA1, _) <- runCLILifecycle tr ["--json", "dashboards", "apply", dumpPath]
      ecA1 `shouldBe` ExitSuccess
      (ecA2, _) <- runCLILifecycle tr ["--json", "dashboards", "apply", dumpPath]
      ecA2 `shouldBe` ExitSuccess
      (_, listOut2) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      jsonOut listOut2 >>= \v -> listRows v `shouldSatisfy` ((== 1) . length)

      (ecStar, _) <- runCLILifecycle tr ["--json", "dashboards", "star", did]
      ecStar `shouldBe` ExitSuccess
      (ecDup, _) <- runCLILifecycle tr ["--json", "dashboards", "duplicate", did]
      ecDup `shouldBe` ExitSuccess
      (_, listOut3) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      ids <- jsonOut listOut3 >>= \v -> pure (map rowId (listRows v))
      length ids `shouldBe` 2

      forM_ ids \i -> do
        (ecDel, _) <- runCLILifecycle tr ["--json", "dashboards", "delete", i]
        ecDel `shouldBe` ExitSuccess
      (_, listOut4) <- runCLILifecycle tr ["--json", "dashboards", "list"]
      jsonOut listOut4 >>= \v -> listRows v `shouldBe` []
