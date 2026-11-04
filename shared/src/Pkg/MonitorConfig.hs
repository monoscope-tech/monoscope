-- | Database-free monitor document shared by API exports and CLI YAML rendering.
module Pkg.MonitorConfig (
  MonitorConfig (..),
  MonitorCondition (..),
  MonitorEvaluation (..),
  MonitorNotification (..),
  encodeMonitorConfig,
) where

import Data.Aeson qualified as AE
import Data.Default (Default)
import Data.List qualified as L
import Data.OpenApi (ToSchema)
import Data.Yaml.Pretty qualified as Yaml
import Deriving.Aeson qualified as DAE
import Pkg.Deriving (SnakeSchema (..), UUIDId)
import Relude


-- | Human-facing apply/export shape. Null options are omitted; required settings
-- stay explicit, and notification lists may be omitted when empty.
data MonitorConfig = MonitorConfig
  { title :: Text
  , severity :: Maybe Text
  , active :: Maybe Bool
  , environment :: Maybe Text
  , service :: Maybe Text
  , condition :: MonitorCondition
  , evaluation :: MonitorEvaluation
  , notify :: Maybe MonitorNotification
  , visualizationType :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via ConfigJSON MonitorConfig
  deriving (ToSchema) via SnakeSchema MonitorConfig


data MonitorCondition = MonitorCondition
  { query :: Text
  , unit :: Maybe Text
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , triggerLessThan :: Bool
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via ConfigJSON MonitorCondition
  deriving (ToSchema) via SnakeSchema MonitorCondition


data MonitorEvaluation = MonitorEvaluation
  { checkIntervalMins :: Int
  , timeWindowMins :: Int
  , thresholdSustainedForMins :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via ConfigJSON MonitorEvaluation
  deriving (ToSchema) via SnakeSchema MonitorEvaluation


data MonitorNotification = MonitorNotification
  { subject :: Maybe Text
  , message :: Maybe Text
  , emailAll :: Maybe Bool
  , emails :: Maybe [Text]
  , slackChannels :: Maybe [Text]
  , teams :: Maybe [UUIDId "team"]
  , notifyAfterMins :: Maybe Int
  , stopAfterCount :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.FromJSON, AE.ToJSON) via ConfigJSON MonitorNotification
  deriving (ToSchema) via SnakeSchema MonitorNotification


type ConfigJSON = DAE.CustomJSON '[DAE.FieldLabelModifier '[DAE.CamelToSnake], DAE.OmitNothingFields, DAE.RejectUnknownFields]


-- | Keep the same reading order in CLI dumps and repository sync exports.
encodeMonitorConfig :: MonitorConfig -> ByteString
encodeMonitorConfig = Yaml.encodePretty (Yaml.setConfCompare (comparing rank <> compare) Yaml.defConfig)
  where
    rank key =
      fromMaybe (maxBound :: Int)
        $ L.elemIndex
          key
          [ "title"
          , "severity"
          , "active"
          , "environment"
          , "service"
          , "condition"
          , "query"
          , "unit"
          , "alert_threshold"
          , "warning_threshold"
          , "trigger_less_than"
          , "alert_recovery_threshold"
          , "warning_recovery_threshold"
          , "evaluation"
          , "check_interval_mins"
          , "time_window_mins"
          , "threshold_sustained_for_mins"
          , "notify"
          , "subject"
          , "message"
          , "email_all"
          , "emails"
          , "slack_channels"
          , "teams"
          , "notify_after_mins"
          , "stop_after_count"
          , "visualization_type"
          ]
