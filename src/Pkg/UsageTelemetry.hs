-- | Anonymous usage reports: each self-hosted instance sends one bucketed report a day to Monoscope,
-- which ingests it into its own project. What is sent, how to stop it and how to read it before it
-- leaves are documented in docs/telemetry.md; add a field there and bump 'schemaVersion' together.
module Pkg.UsageTelemetry (Mode (..), telemetryMode, modeOf, bucket, schemaVersion, Report (..), Counts (..), Usage (..), Features (..), reportUsage, startupNotice) where

import Data.Aeson qualified as AE
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.Wreq qualified as W
import Data.Text qualified as T
import Data.Time (Day, UTCTime, utctDay)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Deriving.Aeson qualified as DAE
import Effectful.Exception (trySync)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Effectful.Timeout qualified as Timeout
import Hasql.Interpolate qualified as HI
import Relude hiding (ask)
import System.Config (AuthContext (..), DeploymentEnv (..), EnvConfig (..))
import System.Directory (doesFileExist)
import System.Info qualified as Info
import System.Logging qualified as Log
import System.Types (ATBackgroundCtx)


data Mode = Off | Debug | On
  deriving stock (Eq, Show)


-- | Opt-out, @DO_NOT_TRACK@ and debug switches, in that order of precedence. A switch is set by any
-- value but empty, @0@ or @false@ (the reading PostHog and Supabase give @DO_NOT_TRACK@).
--
-- >>> [telemetryMode d n g | (d, n, g) <- [(Nothing, Nothing, Nothing), (Just "1", Nothing, Nothing), (Nothing, Just "1", Just "1"), (Nothing, Just "false", Nothing), (Nothing, Just "", Nothing), (Just "0", Nothing, Just "true")]]
-- [On,Off,Off,On,On,Debug]
telemetryMode :: Maybe Text -> Maybe Text -> Maybe Text -> Mode
telemetryMode disabled dnt debug
  | set disabled || set dnt = Off
  | set debug = Debug
  | otherwise = On
  where
    set = maybe False ((`notElem` ["", "0", "false"]) . T.toLower . T.strip)


-- | Monoscope's own deployment collects reports and never sends one about itself.
modeOf :: EnvConfig -> Mode
modeOf cfg = if isJust cfg.telemetryProjectId then Off else telemetryMode cfg.monoscopeTelemetryDisabled cfg.doNotTrack cfg.monoscopeTelemetryDebug


-- | Counts travel as ranges: exact values could fingerprint an instance.
--
-- >>> map bucket [0, 1, 5, 6, 26, 101, 999, 1000, 2500000]
-- ["0","1-5","1-5","6-25","26-100","101-1k","101-1k","1k-10k","1M+"]
bucket :: Int64 -> Text
bucket n = maybe "1M+" snd $ find ((n <=) . fst) ([(0, "0"), (5, "1-5"), (25, "6-25"), (100, "26-100"), (999, "101-1k"), (9999, "1k-10k"), (99999, "10k-100k"), (999999, "100k-1M")] :: [(Int64, Text)])


schemaVersion :: Int
schemaVersion = 1


data Report = Report
  { schema :: Int
  , instanceId :: UUID
  , instanceCreatedAt :: Day
  , version :: Text
  , environment :: Text
  , deployment :: Text
  , os :: Text
  , arch :: Text
  , counts :: Counts
  , usage24h :: Usage
  , features :: Features
  }
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake]] Report


data Counts = Counts {projects :: Text, users :: Text, monitors :: Text, dashboards :: Text}
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.RejectUnknownFields] Counts


data Usage = Usage {events :: Text, metrics :: Text}
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.RejectUnknownFields] Usage


data Features = Features {ai :: Bool, slack :: Bool, githubApp :: Bool, timefusion :: Bool}
  deriving stock (Eq, Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake], DAE.RejectUnknownFields] Features


-- | Builds today's report and, unless switched off, sends it (or logs it in debug mode). Failure is
-- expected on air-gapped hosts, so it is logged at info and not retried until the next day.
reportUsage :: ATBackgroundCtx ()
reportUsage = do
  cfg <- (.config) <$> ask @AuthContext
  let store report = Hasql.interpExecute_ [HI.sql|UPDATE apis.instance_telemetry SET last_payload = #{AE.toJSON report}|]
  case modeOf cfg of
    Off -> pass
    Debug -> buildReport cfg >>= \report -> store report >> Log.logInfo "Usage report (debug mode, not sent)" report
    On -> do
      -- The daily job and every replica's startup all ask; claiming the day first lets only one send.
      claimed <- Hasql.interpExecute [HI.sql|UPDATE apis.instance_telemetry SET last_sent_at = now() WHERE last_sent_at IS NULL OR last_sent_at < now() - interval '20 hours'|]
      when (claimed > 0) do
        report <- buildReport cfg
        sent <- trySync $ Timeout.timeout 5_000_000 $ W.postWith W.defaults (toString cfg.telemetryEndpoint) (AE.toJSON report)
        case sent of
          Right (Just _) -> store report >> Log.logInfo "Usage report sent" report
          failure -> Log.logInfo "Usage report not sent" (cfg.telemetryEndpoint, either (Just . show @Text) (const Nothing) failure)


buildReport :: EnvConfig -> ATBackgroundCtx Report
buildReport cfg = do
  now <- Time.currentTime
  (instanceId, createdAt) <- fromMaybe (UUID.nil, now) <$> Hasql.interpOne @(UUID, UTCTime) [HI.sql|SELECT instance_id, created_at FROM apis.instance_telemetry|]
  (projects, users, monitors, dashboards, events, metrics) <-
    Hasql.interpOne @(Int64, Int64, Int64, Int64, Int64, Int64)
      [HI.sql|SELECT (SELECT count(*) FROM projects.projects WHERE deleted_at IS NULL), (SELECT count(*) FROM users.users),
        (SELECT count(*) FROM monitors.query_monitors), (SELECT count(*) FROM projects.dashboards),
        (SELECT coalesce(sum(total_requests), 0)::int8 FROM apis.daily_usage WHERE created_at > now() - interval '1 day'),
        (SELECT coalesce(sum(total_metrics), 0)::int8 FROM apis.daily_usage WHERE created_at > now() - interval '1 day')|]
      <&> fromMaybe (0, 0, 0, 0, 0, 0)
  ci <- anyM (fmap isJust . lookupEnv) ["CI", "GITHUB_ACTIONS", "GITLAB_CI", "BUILDKITE", "JENKINS_URL", "CIRCLECI", "TF_BUILD"]
  docker <- liftIO $ doesFileExist "/.dockerenv"
  kubernetes <- isJust <$> lookupEnv "KUBERNETES_SERVICE_HOST"
  gitHash <- fromMaybe "dev" <$> lookupEnv "GIT_HASH"
  pure
    Report
      { schema = schemaVersion
      , instanceId
      , instanceCreatedAt = utctDay createdAt
      , version = toText gitHash
      , environment = if ci then "ci" else if cfg.environment == Dev then "dev" else "prod"
      , deployment = if kubernetes then "kubernetes" else if docker then "docker" else "binary"
      , os = toText Info.os
      , arch = toText Info.arch
      , counts = Counts (bucket projects) (bucket users) (bucket monitors) (bucket dashboards)
      , usage24h = Usage (bucket events) (bucket metrics)
      , features = Features (not $ T.null cfg.openaiApiKey) (not $ T.null cfg.slackClientId) (not $ T.null cfg.githubAppId) cfg.enableTimefusionWrites
      }


-- | One line at startup says whether this instance reports and how to stop it.
startupNotice :: EnvConfig -> Text
startupNotice cfg = case modeOf cfg of
  On -> "Anonymous usage telemetry is ON (one bucketed report a day, no hostnames, names or queries). See docs/telemetry.md. Disable with MONOSCOPE_TELEMETRY_DISABLED=1 or DO_NOT_TRACK=1."
  Debug -> "Anonymous usage telemetry is in DEBUG mode: reports are logged, not sent."
  Off -> "Anonymous usage telemetry is OFF."
