-- | End-to-end tests that exercise the real `monoscope` binary against a
-- running HTTP server. Unlike "CLI.CLISpec" (which short-circuits HTTP via
-- 'runHTTPtoServant') and "CLI.CLIBinarySpec" (which only exercises --help
-- output), these tests run the binary with @MONOSCOPE_API_URL@ pointed at a real
-- server so that wire-format bugs (URL paths, KQL operators, JSON envelope
-- mismatches) surface in CI.
--
-- == Pointing the suite at a server
--
-- The suite is driven by environment variables; if they are unset, every
-- test prints @[ pending ]@ instead of failing. This keeps `make test`
-- green when no dev server is running but lets the developer flip the
-- suite on for debugging without recompiling:
--
-- @
-- MONOSCOPE_API_URL=http://localhost:8080 \\
-- MONOSCOPE_API_KEY=...                    \\
-- MONOSCOPE_PROJECT=00000000-0000-0000-0000-000000000000 \\
--   USE_EXTERNAL_DB=true cabal test integration-tests \\
--     --test-options='--match \"CLI binary E2E\"'
-- @
--
-- == Why every audit finding gets a test
--
-- Each finding from the agentic-CLI audit (operators, kind mapping, error
-- bodies, validation, agentic JSON shape) has at least one assertion below.
-- If the bug regresses, the test for it must regress too — that is the
-- point of this file.
module CLI.CLIE2ESpec (spec) where

import Relude

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.List (isInfixOf)
import Data.Text qualified as T
import Network.HTTP.Client (defaultManagerSettings, httpLbs, newManager, parseRequest_, requestHeaders, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import System.Exit (ExitCode (..))
import System.Process qualified as Proc
import Test.Hspec
import UnliftIO.Exception (try)


-- | Resolved E2E config. Populated from environment in 'getE2EConfig'.
data E2EConfig = E2EConfig
  { baseUrl :: Text
  , apiKey :: Text
  , project :: Text
  , binPath :: FilePath
  }


-- | Look up the E2E env vars + binary path. Returns 'Nothing' if any of the
-- required variables are unset, in which case tests should mark themselves
-- pending rather than fail.
getE2EConfig :: IO (Maybe E2EConfig)
getE2EConfig = do
  mUrl <- lookupEnv "MONOSCOPE_API_URL"
  mKey <- lookupEnv "MONOSCOPE_API_KEY"
  mPid <- lookupEnv "MONOSCOPE_PROJECT"
  case (mUrl, mKey, mPid) of
    (Just url, Just key, Just pid) -> do
      bin <- T.unpack . T.strip . toText <$> readProcOut "cabal" ["list-bin", "exe:monoscope"]
      pure $
        Just
          E2EConfig
            { baseUrl = toText url
            , apiKey = toText key
            , project = toText pid
            , binPath = bin
            }
    _ -> pure Nothing


readProcOut :: FilePath -> [String] -> IO String
readProcOut cmd args = do
  (_, out, _) <- Proc.readProcessWithExitCode cmd args ""
  pure out


-- | Run the monoscope binary with E2E env overrides. Always sets
-- @MONOSCOPE_AGENT_MODE=1@ so the binary emits JSON regardless of the test
-- runner's TTY state.
runMono :: E2EConfig -> [String] -> IO (ExitCode, String, String)
runMono cfg args = do
  -- Pass through PATH so the binary's child processes (if any) work.
  pathEnv <- fromMaybe "" <$> lookupEnv "PATH"
  let env =
        [ ("MONOSCOPE_API_URL", toString cfg.baseUrl)
        , ("MONOSCOPE_API_KEY", toString cfg.apiKey)
        , ("MONOSCOPE_PROJECT", toString cfg.project)
        , ("MONOSCOPE_AGENT_MODE", "1")
        , ("PATH", pathEnv)
        ]
      cp = (Proc.proc cfg.binPath args) {Proc.env = Just env}
  Proc.readCreateProcessWithExitCode cp ""


-- | Variant for tests that intentionally /omit/ auth env vars (e.g. testing
-- the unauthenticated 'auth status' message).
runMonoNoAuth :: FilePath -> [String] -> IO (ExitCode, String, String)
runMonoNoAuth bin args = do
  pathEnv <- fromMaybe "" <$> lookupEnv "PATH"
  let env =
        [ ("HOME", "/tmp/no-such-monoscope-home-for-tests")
        , ("PATH", pathEnv)
        ]
      cp = (Proc.proc bin args) {Proc.env = Just env}
  Proc.readCreateProcessWithExitCode cp ""


-- | Skip a test if the server isn't reachable at the configured URL.
withReachableServer :: (E2EConfig -> IO ()) -> IO ()
withReachableServer body = do
  mCfg <- getE2EConfig
  case mCfg of
    Nothing ->
      pendingWith "Set MONOSCOPE_API_URL, MONOSCOPE_API_KEY, MONOSCOPE_PROJECT to enable"
    Just cfg -> do
      mgr <- newManager defaultManagerSettings
      let req =
            (parseRequest_ (toString cfg.baseUrl <> "/api/v1/me"))
              { requestHeaders =
                  [ ("Authorization", encodeUtf8 ("Bearer " <> cfg.apiKey))
                  , ("X-Project-Id", encodeUtf8 cfg.project)
                  ]
              }
      eResp <- try @SomeException (httpLbs req mgr)
      case eResp of
        Left e -> pendingWith $ "Server at " <> toString cfg.baseUrl <> " not reachable: " <> show e
        Right resp
          | statusCode (responseStatus resp) == 200 -> body cfg
          | otherwise ->
              pendingWith $
                "Server returned HTTP "
                  <> show (statusCode (responseStatus resp))
                  <> " for /api/v1/me — check MONOSCOPE_API_KEY/PROJECT"


-- | Decode the binary's stdout as JSON; fail with a snippet on parse error.
decodeJsonOut :: String -> IO AE.Value
decodeJsonOut out = case AE.eitherDecode (encodeUtf8 out) of
  Right v -> pure v
  Left err -> do
    expectationFailure ("expected JSON on stdout, got: " <> err <> "\n--- stdout snippet ---\n" <> take 400 out)
    error "unreachable"


shouldHaveKeys :: AE.Value -> [Text] -> Expectation
shouldHaveKeys (AE.Object obj) keys = forM_ keys $ \k ->
  KM.member (fromString (toString k)) obj `shouldBe` True
shouldHaveKeys other _ =
  expectationFailure $ "expected JSON object, got: " <> show other


-- | Find the binary path even when no E2E config is present (used by tests
-- that don't need a running server but do need to invoke the CLI).
findBin :: IO FilePath
findBin = T.unpack . T.strip . toText <$> readProcOut "cabal" ["list-bin", "exe:monoscope"]


spec :: Spec
spec = describe "CLI binary E2E (real server)" $ do
  describe "smoke" $ do
    it "version prints CLI version" $ withReachableServer $ \cfg -> do
      (code, out, _) <- runMono cfg ["version"]
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("monoscope " `isInfixOf`)

    it "me returns project identity JSON" $ withReachableServer $ \cfg -> do
      (code, out, err) <- runMono cfg ["me"]
      when (code /= ExitSuccess) $ expectationFailure $ "exit=" <> show code <> " stderr=" <> err
      v <- decodeJsonOut out
      v `shouldHaveKeys` ["project_id"]

    it "schema returns fields object" $ withReachableServer $ \cfg -> do
      (code, out, _) <- runMono cfg ["schema"]
      code `shouldBe` ExitSuccess
      v <- decodeJsonOut out
      v `shouldHaveKeys` ["fields"]

    it "facets returns a JSON object (possibly empty)" $ withReachableServer $ \cfg -> do
      (code, out, err) <- runMono cfg ["facets"]
      when (code /= ExitSuccess) $ expectationFailure $ "stderr=" <> err
      v <- decodeJsonOut out
      case v of
        AE.Object _ -> pure ()
        _ -> expectationFailure $ "expected facet object, got: " <> show v

    it "facets <FIELD> filters to a single field" $ withReachableServer $ \cfg -> do
      (code, out, err) <- runMono cfg ["facets", "resource.service.name", "--top", "3"]
      when (code /= ExitSuccess) $ expectationFailure $ "stderr=" <> err
      v <- decodeJsonOut out
      case v of
        AE.Object obj -> KM.size obj `shouldSatisfy` (<= 1)
        _ -> expectationFailure "expected object"

  describe "events" $ do
    it "events search returns count + has_more (D4 envelope)" $ withReachableServer $ \cfg -> do
      (code, out, err) <- runMono cfg ["events", "search", "", "--since", "1h", "--limit", "5"]
      when (code /= ExitSuccess) $ expectationFailure $ "stderr=" <> err
      v <- decodeJsonOut out
      v `shouldHaveKeys` ["events", "count", "has_more"]

    -- Audit B1: --service used to emit Lucene-style and 400.
    it "events search --service does not 400 (D1)" $ withReachableServer $ \cfg -> do
      (code, _, err) <- runMono cfg ["events", "search", "", "--service", "monoscope", "--since", "1h", "--limit", "1"]
      code `shouldBe` ExitSuccess
      err `shouldNotSatisfy` ("HTTP 400" `isInfixOf`)

    it "events search --level does not 400 (D1)" $ withReachableServer $ \cfg -> do
      (code, _, err) <- runMono cfg ["events", "search", "", "--level", "error", "--since", "1h", "--limit", "1"]
      code `shouldBe` ExitSuccess
      err `shouldNotSatisfy` ("HTTP 400" `isInfixOf`)

    -- Audit B3: traces should map kind=trace -> source=span on the wire.
    it "traces search returns events without 400 (D2)" $ withReachableServer $ \cfg -> do
      (code, out, err) <- runMono cfg ["traces", "search", "", "--since", "1h", "--limit", "1"]
      code `shouldBe` ExitSuccess
      err `shouldNotSatisfy` ("HTTP 400" `isInfixOf`)
      void $ decodeJsonOut out

    -- Audit B5: malformed --since rejected client-side.
    it "events search --since 1xyz exits non-zero with a clear message (D5)" $ withReachableServer $ \cfg -> do
      (code, _, err) <- runMono cfg ["events", "search", "", "--since", "1xyz", "--limit", "1"]
      code `shouldNotBe` ExitSuccess
      err `shouldSatisfy` ("--since" `isInfixOf`)

    -- Regression: uppercase suffixes are valid (the platform's TimePicker
    -- emits "1H", "24H" — the validator must not reject the defaults it ships).
    it "events search --since 1H is accepted" $ withReachableServer $ \cfg -> do
      (code, _, err) <- runMono cfg ["events", "search", "", "--since", "1H", "--limit", "1"]
      code `shouldBe` ExitSuccess
      err `shouldNotSatisfy` ("--since" `isInfixOf`)

    -- Audit B5: bad --kind rejected.
    it "events search --kind banana exits non-zero (D5)" $ withReachableServer $ \cfg -> do
      (code, _, err) <- runMono cfg ["events", "search", "", "--kind", "banana", "--since", "1h", "--limit", "1"]
      code `shouldNotBe` ExitSuccess
      err `shouldSatisfy` ("--kind" `isInfixOf`)

    -- Audit C2: server error bodies are surfaced on KQL parse errors.
    it "intentionally bad KQL surfaces server's parse-error body (D3)" $ withReachableServer $ \cfg -> do
      (code, _, err) <- runMono cfg ["events", "search", "AND OR", "--since", "1h", "--limit", "1"]
      code `shouldNotBe` ExitSuccess
      err `shouldSatisfy` (\e -> "HTTP 400" `isInfixOf` e || "parse" `isInfixOf` e || "expected" `isInfixOf` e)

  describe "services" $ do
    -- Pins the facets-backed implementation: services list hits
    -- @/api/v1/facets?field=resource.service.name@ and rewraps as
    -- @{services: [{name, events}], count}@. A regression that broke
    -- the field path or the response key would zero this out silently.
    it "services list returns {services, count} envelope" $ withReachableServer $ \cfg -> do
      (code, out, err) <- runMono cfg ["services", "list"]
      when (code /= ExitSuccess) $ expectationFailure $ "stderr=" <> err
      v <- decodeJsonOut out
      v `shouldHaveKeys` ["services", "count"]

  describe "list resources return JSON" $ do
    let listCases =
          [ "issues"
          , "endpoints"
          , "log-patterns"
          , "monitors"
          , "dashboards"
          , "api-keys"
          , "teams"
          , "members"
          ]
    forM_ listCases $ \resource ->
      it (resource <> " list returns {data, pagination} envelope (D4)") $ withReachableServer $ \cfg -> do
        (code, out, err) <- runMono cfg [resource, "list"]
        when (code /= ExitSuccess) $ expectationFailure $ "stderr=" <> err
        v <- decodeJsonOut out
        v `shouldHaveKeys` ["data", "pagination"]

  describe "auth (no real server required)" $ do
    -- Audit B11: error message used to say "mono auth login"; must say "monoscope".
    it "auth status with no credentials hints at 'monoscope auth login' (B11)" $ do
      bin <- findBin
      (_, out, err) <- runMonoNoAuth bin ["auth", "status"]
      let combined = out <> err
      combined `shouldSatisfy` (\s -> "monoscope auth login" `isInfixOf` s || "Authenticated" `isInfixOf` s)

    -- Audit C7: agent mode must refuse interactive flows.
    it "auth login (no token) in MONOSCOPE_AGENT_MODE exits non-zero (C7)" $ do
      bin <- findBin
      pathEnv <- fromMaybe "" <$> lookupEnv "PATH"
      let env =
            [ ("MONOSCOPE_AGENT_MODE", "1")
            , ("HOME", "/tmp/no-such-monoscope-home-for-tests")
            , ("PATH", pathEnv)
            ]
          cp = (Proc.proc bin ["auth", "login"]) {Proc.env = Just env}
      (code, _, err) <- Proc.readCreateProcessWithExitCode cp ""
      code `shouldNotBe` ExitSuccess
      err `shouldSatisfy` (\s -> "agent" `isInfixOf` s || "--token" `isInfixOf` s)

  describe "completion" $ do
    -- Audit C12: unknown shell argument should fail loudly, not silently emit bash.
    it "completion <unknown> fails loudly (C12)" $ do
      bin <- findBin
      (code, _, err) <- runMonoNoAuth bin ["completion", "tcsh"]
      code `shouldNotBe` ExitSuccess
      err `shouldSatisfy` (\s -> "tcsh" `isInfixOf` s || "shell" `isInfixOf` s || "bash|zsh|fish" `isInfixOf` s)
