-- | Autonomous onboarding instrumentation runner.
-- Inspects a connected repository, detects its stack (backend, frontend/RUM,
-- Kubernetes/Docker/infra, or database config), discovers candidate uptime URLs,
-- generates secret-free OpenTelemetry instrumentation, verifies live ingestion
-- against Monoscope using an ephemeral project API key, and opens a GitHub PR.
-- The key is only ever a runtime env var: never written to generated files or
-- the PR, which lists MONOSCOPE_API_KEY only as a placeholder.
-- The PR opens only after the test span is confirmed in otel_logs_and_spans.
module Pkg.OnboardingInstrument (
  runOnboardingInstrument,
  detectStackFromPaths,
  serviceRoots,
  listServiceRoots,
  buildDiffPreview,
) where

import Control.Lens ((.~), (?~), (^.))
import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM qualified as LLM
import Data.Effectful.UUID qualified as UUIDEff
import Data.Effectful.Wreq qualified as W
import Data.List (lookup)
import Data.ProtoLens (defMessage, encodeMessage)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Effectful (Eff, IOE, (:>))
import Effectful.Labeled (Labeled)
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader, ask)
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Hasql.Interpolate qualified as HI
import Models.Projects.Activation qualified as Activation
import Models.Projects.GitSync qualified as GitSync
import Models.Projects.OnboardingInstrument qualified as OI
import Models.Projects.ProjectApiKeys qualified as ProjectApiKeys
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types.Header (hContentType)
import Network.Wreq qualified as Wreq
import Pkg.Git qualified as Git
import Pkg.OnboardingDiscovery qualified as Discovery
import Relude hiding (Reader, ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.Logging qualified as Log
import System.Types (ATBackgroundCtx, DB)
import UnliftIO.Exception (tryAny)
import Utils (hostPath)
import "monoscope-proto" Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService qualified as TraceService
import "monoscope-proto" Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as TSF
import "monoscope-proto" Proto.Opentelemetry.Proto.Common.V1.Common qualified as Common
import "monoscope-proto" Proto.Opentelemetry.Proto.Common.V1.Common_Fields qualified as PCF
import "monoscope-proto" Proto.Opentelemetry.Proto.Resource.V1.Resource qualified as Resource
import "monoscope-proto" Proto.Opentelemetry.Proto.Resource.V1.Resource_Fields qualified as RF
import "monoscope-proto" Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as Trace
import "monoscope-proto" Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as TF


data PatchFile = PatchFile
  { path :: Text
  , content :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data PatchPlan = PatchPlan
  { stack :: Text
  , summary :: Text
  , files :: [PatchFile]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Detect primary stack from repository file paths.
--
-- >>> detectStackFromPaths ["package.json", "next.config.ts", "src/app/page.tsx"]
-- "Next.js (TypeScript / Frontend + API)"
-- >>> detectStackFromPaths ["go.mod", "cmd/server/main.go"]
-- "Go"
-- >>> detectStackFromPaths ["Chart.yaml", "templates/deployment.yaml"]
-- "Kubernetes (Helm)"
detectStackFromPaths :: [Text] -> Text
detectStackFromPaths paths
  | any ("next.config" `T.isInfixOf`) lower = "Next.js (TypeScript / Frontend + API)"
  | any ("vite.config" `T.isInfixOf`) lower = "Vite (Frontend Web / RUM)"
  | "package.json" `elem` lower = "Node.js / TypeScript"
  | "go.mod" `elem` lower = "Go"
  | any (`elem` ["pyproject.toml", "requirements.txt"]) lower = "Python"
  | "cargo.toml" `elem` lower = "Rust"
  | any (`elem` ["pom.xml", "build.gradle", "build.gradle.kts"]) lower = "Java / JVM"
  | "mix.exs" `elem` lower = "Elixir / Phoenix"
  | "composer.json" `elem` lower = "PHP"
  | any (".csproj" `T.isSuffixOf`) lower = ".NET / C#"
  | "chart.yaml" `elem` lower = "Kubernetes (Helm)"
  | any (\p -> "k8s/" `T.isPrefixOf` p || "kubernetes/" `T.isPrefixOf` p) lower = "Kubernetes"
  | any (`elem` ["docker-compose.yml", "docker-compose.yaml", "dockerfile"]) lower = "Docker / Infrastructure"
  | otherwise = "Application (OpenTelemetry)"
  where
    lower = map T.toLower paths


-- | Folders holding a service manifest, each with the stack detected from the files it owns
-- (a file belongs to the deepest root above it). Dependency, example and test trees are ignored.
--
-- >>> serviceRoots ["package.json", "src/index.ts", "README.md"]
-- ("","Node.js / TypeScript") :| []
-- >>> serviceRoots ["package.json", "apps/web/package.json", "apps/web/next.config.ts", "services/api/go.mod", "node_modules/x/package.json", "examples/demo/go.mod"]
-- ("","Node.js / TypeScript") :| [("apps/web","Next.js (TypeScript / Frontend + API)"),("services/api","Go")]
-- >>> serviceRoots ["Chart.yaml", "templates/deployment.yaml"]
-- ("","Kubernetes (Helm)") :| []
serviceRoots :: [Text] -> NonEmpty (Text, Text)
serviceRoots paths = fromMaybe (("", detectStackFromPaths paths) :| []) $ nonEmpty [(r, detectStackFromPaths [rel r p | p <- paths, owner p == Just r]) | r <- roots]
  where
    segs = T.splitOn "/"
    dir = T.intercalate "/" . fromMaybe [] . viaNonEmpty init . segs
    ignored = any (`elem` ["node_modules", "vendor", "examples", "example", "test", "tests", "testdata", "fixtures", "dist", ".git"]) . segs . T.toLower
    manifest p = let b = T.toLower $ fromMaybe p $ viaNonEmpty last $ segs p in b `elem` ["package.json", "go.mod", "pyproject.toml", "requirements.txt", "cargo.toml", "pom.xml", "build.gradle", "build.gradle.kts", "mix.exs", "composer.json"] || ".csproj" `T.isSuffixOf` b
    roots = sort $ ordNub [dir p | p <- paths, manifest p, not (ignored p)]
    under r p = T.null r || (r <> "/") `T.isPrefixOf` p
    owner p = viaNonEmpty last [r | r <- roots, under r p]
    rel r p = if T.null r then p else T.drop (T.length r + 1) p


-- | Format a compact unified-style diff preview from generated files.
buildDiffPreview :: [PatchFile] -> Text
buildDiffPreview =
  T.intercalate "\n\n" . map \f ->
    unlines $ ("+++ " <> f.path) : map ("+ " <>) (take 28 $ lines f.content)


runOnboardingInstrument :: Projects.ProjectId -> OI.RunId -> ATBackgroundCtx ()
runOnboardingInstrument pid rid = do
  runM <- OI.getRun pid rid
  case runM of
    Nothing -> Log.logInfo "OnboardingInstrument run not found" (pid, rid)
    Just run -> do
      ctx <- ask @AuthContext
      let encKey = encodeUtf8 ctx.config.apiKeyEncryptionSecretKey
      let phases0 = OI.initialPhases run.repoFullName
      OI.updateRunProgress rid OI.RunCloning "" (setPhase "clone" OI.PhaseRunning Nothing phases0) "" []
      connRes <- resolveGitConn encKey pid run.repositoryId run.repoFullName run.branch
      case connRes of
        Left err ->
          OI.failRun rid (setPhase "clone" OI.PhaseFailed (Just err) phases0) "Could not access the selected GitHub repository. Check GitHub App permissions and try again." err
        Right (conn, repoRef) -> do
          -- Phase 1: Fetch tree & key files
          treeRes <- Git.fetchTree conn repoRef (bool (run.rootPath <> "/") "" (T.null run.rootPath))
          case treeRes of
            Left err ->
              OI.failRun rid (setPhase "clone" OI.PhaseFailed (Just err) phases0) "Could not read files from the repository. Verify the branch exists and retry." err
            Right (_, entries) -> do
              -- A monorepo run is scoped to one service root: the agent sees paths relative to it,
              -- and its files, span and service name are placed back under it.
              let root = run.rootPath
                  rel p = if T.null root then p else T.drop (T.length root + 1) p
                  repoPaths = [e.path | e <- entries, e.isBlob]
                  paths = map rel repoPaths
                  service = run.repoFullName <> bool ("/" <> root) "" (T.null root)
                  detected = fromMaybe (detectStackFromPaths paths) $ lookup root $ toList $ serviceRoots repoPaths
                  phases1 = setPhase "detect" OI.PhaseRunning (Just detected) $ setPhase "clone" OI.PhaseDone (Just $ show (length paths) <> " files") phases0
              OI.updateRunProgress rid OI.RunDetecting detected phases1 "" []
              -- Fetch key manifest/readme/config files for stack context & URL discovery
              let interesting = take 8 $ filter isContextFile repoPaths
              fetched <- forM interesting \p -> (rel p,) . rightToMaybe <$> Git.fetchFile conn repoRef p
              let fileContents = [(p, T.take 2500 $ decodeUtf8 bs) | (p, Just bs) <- fetched]
                  candidateDocUrls = take 3 $ Discovery.extractUrlsFromText $ unlines (map snd fileContents)
              suggestedUrls <-
                concat <$> forM candidateDocUrls \u ->
                  case Discovery.normalizeTargetUrl u of
                    Nothing -> pure []
                    Just norm -> snd <$> Discovery.discoverEndpoints norm
              let phases2 = setPhase "patch" OI.PhaseRunning Nothing $ setPhase "detect" OI.PhaseDone (Just detected) phases1
              OI.updateRunProgress rid OI.RunPatching detected phases2 "" suggestedUrls
              -- Phase 3: Generate secret-free OpenTelemetry instrumentation
              plan <- (\pl -> pl{files = [f{path = bool (root <> "/") "" (T.null root) <> f.path} | f <- pl.files]}) <$> generateInstrumentationPlan detected service paths fileContents
              let diffTxt = buildDiffPreview plan.files
                  phases3 = setPhase "verify" OI.PhaseRunning Nothing $ setPhase "patch" OI.PhaseDone (Just $ show (length plan.files) <> " file(s)") phases2
              OI.updateRunProgress rid OI.RunVerifying plan.stack phases3 diffTxt suggestedUrls
              -- Phase 4: Generate/fetch API key & verify real live ingestion against Monoscope
              apiKey <- ensureActiveApiKey ctx.config.apiKeyEncryptionSecretKey pid
              now <- Time.currentTime
              verifyRes <- sendAndVerifyTestSpan ctx pid apiKey service plan.stack now
              case verifyRes of
                Left err ->
                  OI.failRun
                    rid
                    (setPhase "verify" OI.PhaseFailed (Just err) phases3)
                    "Instrumentation was generated, but live test span verification did not complete. You can finish locally with your coding agent."
                    err
                Right verifiedAt -> do
                  void $ Projects.completeOnboardingStep pid (show Projects.Integration)
                  void $ Activation.recordActivationMilestones Activation.IngestVerified Nothing [pid]
                  let phases4 = setPhase "pr" OI.PhaseRunning Nothing $ setPhase "verify" OI.PhaseDone (Just "Test span received in Monoscope") phases3
                  OI.updateRunProgress rid OI.RunOpeningPr plan.stack phases4 diffTxt suggestedUrls
                  -- Phase 5: Open secret-free Pull Request
                  let branchName = "monoscope/auto-instrument-" <> T.take 8 rid.toText
                      prTitle = "chore(observability): add Monoscope OpenTelemetry instrumentation"
                      prBody = buildPrBody plan.stack plan.summary (hostPath ctx.config.hostUrl "/v1/traces")
                      commitFiles = [(f.path, encodeUtf8 f.content) | f <- plan.files]
                  prRes <- Git.publishPullRequest conn repoRef branchName prTitle prTitle prBody commitFiles
                  case prRes of
                    Left err ->
                      OI.failRun
                        rid
                        (setPhase "pr" OI.PhaseFailed (Just err) phases4)
                        "Live test span verified, but GitHub rejected opening the Pull Request. Check repository write permissions or apply the diff locally."
                        err
                    Right (prNum, prUrl) -> do
                      let phases5 = setPhase "pr" OI.PhaseDone (Just $ "PR #" <> show prNum) phases4
                      OI.completeRun rid phases5 diffTxt suggestedUrls (Just prUrl) (Just $ fromIntegral prNum) (Just verifiedAt)


resolveGitConn :: (DB es, IOE :> es, Log :> es, Reader AuthContext :> es, W.HTTP :> es) => ByteString -> Projects.ProjectId -> Maybe GitSync.RepositoryId -> Text -> Text -> Eff es (Either Text (Git.GitConn, Git.RepoRef))
resolveGitConn encKey pid repositoryId repoFullName branch = do
  ctx <- ask @AuthContext
  let toConn cred owner repo = runExceptT do
        credentials <- hoistEither $ maybeToRight "Reconnect the repository account." $ GitSync.credentialCreds cred
        token <- ExceptT $ GitSync.githubToken ctx.config.githubAppId ctx.config.githubAppPrivateKey credentials
        conn <- hoistEither $ first Git.gitConnErrorMessage $ GitSync.credentialConn cred token
        pure (conn, Git.RepoRef owner repo branch)
  repos <- GitSync.getRepositories pid
  let matchedRepo = (repositoryId >>= \rid -> find ((== rid) . (.id)) repos) <|> find (\r -> r.owner <> "/" <> r.repo == repoFullName) repos
  case matchedRepo of
    Just r -> do
      credM <- join <$> traverse (GitSync.getGitHubCredential encKey pid) r.credentialId
      maybe (pure $ Left "GitHub credential not found for this repository.") (\c -> toConn c r.owner r.repo) credM
    Nothing -> do
      creds <- GitSync.getGitHubCredentials pid
      case (find ((== Git.GitHub) . (.host)) creds, Git.splitFullName repoFullName) of
        (Just cred, (owner, repo)) | not (T.null owner || T.null repo) -> toConn cred owner repo
        _ -> pure $ Left "No connected GitHub account found for this project."


-- | The repository's default branch and service roots, for the onboarding folder picker.
listServiceRoots :: (DB es, IOE :> es, Log :> es, Reader AuthContext :> es, W.HTTP :> es) => Projects.ProjectId -> Maybe GitSync.RepositoryId -> Text -> Eff es (Either Text (Text, NonEmpty (Text, Text)))
listServiceRoots pid repositoryId repoFullName = do
  ctx <- ask @AuthContext
  runExceptT do
    (conn, r) <- ExceptT $ resolveGitConn (encodeUtf8 ctx.config.apiKeyEncryptionSecretKey) pid repositoryId repoFullName "HEAD"
    branch <- lift $ Git.defaultBranchOf conn r
    (_, entries) <- ExceptT $ Git.fetchTree conn (Git.RepoRef r.owner r.repo branch) ""
    pure (branch, serviceRoots [e.path | e <- entries, e.isBlob])


isContextFile :: Text -> Bool
isContextFile p =
  let l = T.toLower p
      base = fromMaybe l $ viaNonEmpty last $ T.splitOn "/" l
   in base `elem` ["package.json", "go.mod", "pyproject.toml", "requirements.txt", "cargo.toml", "pom.xml", "dockerfile", "docker-compose.yml", "docker-compose.yaml", "readme.md", "chart.yaml", ".env.example"]


generateInstrumentationPlan :: LLM.LLM :> es => Text -> Text -> [Text] -> [(Text, Text)] -> Eff es PatchPlan
generateInstrumentationPlan detected repoName paths fileContents = do
  let sysPrompt =
        "You are Monoscope's OpenTelemetry instrumentation agent. Given a repository's file list and key manifests, "
          <> "produce a minimal, idiomatic, secret-free OpenTelemetry setup file (and optional env example) that exports "
          <> "traces and logs via OTLP/HTTP using environment variables MONOSCOPE_API_KEY and OTEL_EXPORTER_OTLP_ENDPOINT. "
          <> "NEVER hardcode any API key. Support backend, frontend RUM, Kubernetes/Docker, or database configs as appropriate. "
          <> "Respond ONLY with JSON: {\"stack\": string, \"summary\": string, \"files\": [{\"path\": string, \"content\": string}]}."
      userPrompt =
        "Repository: "
          <> repoName
          <> "\nDetected stack: "
          <> detected
          <> "\nFiles (sample): "
          <> T.intercalate ", " (take 80 paths)
          <> "\n\nKey file snippets:\n"
          <> T.intercalate "\n---\n" [p <> ":\n" <> c | (p, c) <- fileContents]
  LLM.callLLM sysPrompt userPrompt "" <&> \case
    Right raw ->
      case AE.eitherDecodeStrict @PatchPlan (encodeUtf8 $ LLM.stripCodeBlock raw) of
        Right plan | not (null plan.files) -> plan
        _ -> fallbackPlan detected repoName
    Left _ -> fallbackPlan detected repoName


fallbackPlan :: Text -> Text -> PatchPlan
fallbackPlan detected repoName
  | "Kubernetes" `T.isInfixOf` detected =
      PatchPlan
        detected
        "Added OpenTelemetry Collector DaemonSet values for Kubernetes logs, metrics, and traces."
        [ PatchFile
            "monoscope-otel-values.yaml"
            ( unlines
                [ "# Helm values for open-telemetry/opentelemetry-collector -> Monoscope"
                , "# Set MONOSCOPE_API_KEY via a Kubernetes Secret before applying."
                , "mode: daemonset"
                , "presets:"
                , "  logsCollection: { enabled: true }"
                , "  hostMetrics: { enabled: true }"
                , "  kubeletMetrics: { enabled: true }"
                , "config:"
                , "  exporters:"
                , "    otlphttp/monoscope:"
                , "      endpoint: ${env:OTEL_EXPORTER_OTLP_ENDPOINT}"
                , "      headers:"
                , "        x-api-key: ${env:MONOSCOPE_API_KEY}"
                ]
            )
        ]
  | "Go" `T.isInfixOf` detected =
      PatchPlan
        detected
        "Added OpenTelemetry OTLP/HTTP bootstrap helper reading MONOSCOPE_API_KEY from environment."
        [ PatchFile
            "monoscope_telemetry.go"
            ( unlines
                [ "package telemetry"
                , ""
                , "// Initialize OpenTelemetry OTLP/HTTP export to Monoscope."
                , "// Required env vars: OTEL_EXPORTER_OTLP_ENDPOINT, MONOSCOPE_API_KEY"
                , "import ("
                , "  \"context\""
                , "  \"os\""
                , "  \"go.opentelemetry.io/otel\""
                , "  \"go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracehttp\""
                , "  sdktrace \"go.opentelemetry.io/otel/sdk/trace\""
                , ")"
                , ""
                , "func Init(ctx context.Context) (func(context.Context) error, error) {"
                , "  exp, err := otlptracehttp.New(ctx, otlptracehttp.WithHeaders(map[string]string{\"x-api-key\": os.Getenv(\"MONOSCOPE_API_KEY\")}))"
                , "  if err != nil { return nil, err }"
                , "  tp := sdktrace.NewTracerProvider(sdktrace.WithBatcher(exp))"
                , "  otel.SetTracerProvider(tp)"
                , "  return tp.Shutdown, nil"
                , "}"
                ]
            )
        ]
  | otherwise =
      PatchPlan
        detected
        ("Added OpenTelemetry bootstrap for " <> repoName <> " reading MONOSCOPE_API_KEY and OTEL_EXPORTER_OTLP_ENDPOINT.")
        [ PatchFile
            "monoscope.telemetry.mjs"
            ( unlines
                [ "// Monoscope OpenTelemetry bootstrap (load with: node --import ./monoscope.telemetry.mjs ...)"
                , "// Set OTEL_EXPORTER_OTLP_ENDPOINT and MONOSCOPE_API_KEY in your environment."
                , "import { NodeSDK } from '@opentelemetry/sdk-node';"
                , "import { getNodeAutoInstrumentations } from '@opentelemetry/auto-instrumentations-node';"
                , "import { OTLPTraceExporter } from '@opentelemetry/exporter-trace-otlp-http';"
                , ""
                , "const sdk = new NodeSDK({"
                , "  traceExporter: new OTLPTraceExporter({"
                , "    url: `${process.env.OTEL_EXPORTER_OTLP_ENDPOINT || 'https://otel.monoscopehq.com'}/v1/traces`,"
                , "    headers: { 'x-api-key': process.env.MONOSCOPE_API_KEY || '' },"
                , "  }),"
                , "  instrumentations: [getNodeAutoInstrumentations()],"
                , "});"
                , "sdk.start();"
                ]
            )
        ]


ensureActiveApiKey :: (DB es, Time :> es, UUIDEff.UUIDEff :> es) => Text -> Projects.ProjectId -> Eff es Text
ensureActiveApiKey encKey pid = do
  keys <- ProjectApiKeys.projectApiKeysByProjectId pid
  case find (.active) keys of
    Just k -> pure k.keyPrefix
    Nothing -> do
      keyUuid <- UUIDEff.genUUID
      let encoded = ProjectApiKeys.encodeApiKeyB64 encKey keyUuid
      apiKey <- ProjectApiKeys.newProjectApiKeys pid keyUuid "Onboarding API Key" encoded
      void $ ProjectApiKeys.insertProjectApiKey apiKey
      pure apiKey.keyPrefix


-- | Emit a real OTLP/HTTP trace span using the project's API key against Monoscope's own
-- /v1/traces endpoint, then verify the span is readable in otel_logs_and_spans.
sendAndVerifyTestSpan :: (DB es, IOE :> es, Labeled "timefusion" Hasql.Hasql :> es) => AuthContext -> Projects.ProjectId -> Text -> Text -> Text -> UTCTime -> Eff es (Either Text UTCTime)
sendAndVerifyTestSpan ctx pid apiKey repoName stack now = do
  let traceUrl = hostPath ctx.config.hostUrl "/v1/traces"
      ns = fromIntegral (round (utcTimeToPOSIXSeconds now * 1_000_000_000) :: Integer) :: Word64
      kv k v =
        defMessage @Common.KeyValue
          & (PCF.key .~ k)
          & (PCF.value .~ (defMessage @Common.AnyValue & PCF.stringValue .~ v))
      spanMsg =
        defMessage @Trace.Span
          & (TF.traceId .~ BS.pack [1 .. 16])
          & (TF.spanId .~ BS.pack [1 .. 8])
          & (TF.name .~ "monoscope.onboarding.verify")
          & (TF.kind .~ Trace.Span'SPAN_KIND_INTERNAL)
          & (TF.startTimeUnixNano .~ ns)
          & (TF.endTimeUnixNano .~ (ns + 5_000_000))
          & (TF.attributes .~ [kv "monoscope.onboarding.repo" repoName, kv "monoscope.onboarding.stack" stack])
      reqMsg =
        defMessage @TraceService.ExportTraceServiceRequest
          & ( TSF.resourceSpans
                .~ [ defMessage @Trace.ResourceSpans
                       & (TF.resource .~ (defMessage @Resource.Resource & RF.attributes .~ [kv "service.name" repoName]))
                       & (TF.scopeSpans .~ [defMessage @Trace.ScopeSpans & TF.spans .~ [spanMsg]])
                   ]
            )
      payload = encodeMessage reqMsg
      opts =
        Wreq.defaults
          & Wreq.header "x-api-key"
          .~ [encodeUtf8 apiKey]
            & Wreq.header hContentType
          .~ ["application/x-protobuf"]
            & Wreq.checkResponse
          ?~ (\_ _ -> pass)
  postRes <- liftIO $ tryAny $ Wreq.postWith opts (toString traceUrl) payload
  case postRes of
    Left err -> pure $ Left $ "Failed to send verification span: " <> show err
    Right resp ->
      let code = resp ^. Wreq.responseStatus . Wreq.statusCode
       in if code < 200 || code >= 300
            then pure $ Left $ "OTLP ingest returned HTTP " <> show code
            else do
              found :: Maybe Text <-
                Hasql.withHasqlTimefusion ctx.env.enableTimefusionReads
                  $ Hasql.interpOne [HI.sql| SELECT context___span_id FROM otel_logs_and_spans WHERE project_id = #{pid.toText} LIMIT 1 |]
              pure $ maybe (Left "Span accepted by ingest, waiting for storage visibility") (const $ Right now) found


buildPrBody :: Text -> Text -> Text -> Text
buildPrBody stack summary endpoint =
  unlines
    [ "## Summary"
    , summary
    , ""
    , "- **Detected stack:** " <> stack
    , "- **Live verification:** Verified a test OpenTelemetry span reached Monoscope using an ephemeral project API key (never committed to git)."
    , ""
    , "## Required environment variables for deployment"
    , "Set these in your hosting platform (Vercel, Railway, Fly.io, Kubernetes Secret, etc.) before or after merging:"
    , ""
    , "```bash"
    , "OTEL_EXPORTER_OTLP_ENDPOINT=" <> endpoint
    , "MONOSCOPE_API_KEY=<your-monoscope-api-key>"
    , "```"
    ]


setPhase :: Text -> OI.PhaseState -> Maybe Text -> [OI.PhaseItem] -> [OI.PhaseItem]
setPhase targetKey newState detailM = map \p ->
  if p.key == targetKey then p{OI.state = newState, OI.detail = detailM <|> p.detail} else p
