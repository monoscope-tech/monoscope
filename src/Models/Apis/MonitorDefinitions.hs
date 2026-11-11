module Models.Apis.MonitorDefinitions (MonitorInput (..), monitorConfig, monitorFromInput, monitorToInput, compileAlertSql) where

import Control.Lens ((?~))
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.CaseInsensitive qualified as CI
import Data.Default (Default (..))
import Data.OpenApi (ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Models.Apis.Monitors qualified as Monitors
import Models.Ids qualified as Projects
import Models.Projects.ProjectMembers qualified as PM
import Pkg.DeriveUtils (SnakeJSON (..), SnakeSchema (..))
import Pkg.MonitorConfig (MonitorCondition (..), MonitorConfig (..), MonitorEvaluation (..), MonitorNotification (..))
import Pkg.Parser qualified as Parser
import Relude


-- | Input for creating or replacing (PUT) a monitor. Maps onto `QueryMonitor`
-- fields without surfacing db-internal lifecycle columns.
data MonitorInput = MonitorInput
  { unit :: Maybe Text
  , title :: Text
  , query :: Text
  , severity :: Maybe Text
  , subject :: Maybe Text
  , message :: Maybe Text
  , alertThreshold :: Double
  , warningThreshold :: Maybe Double
  , triggerLessThan :: Bool
  , checkIntervalMins :: Int
  , timeWindowMins :: Int
  , thresholdSustainedForMins :: Maybe Int
  , notifyAfterMins :: Maybe Int
  , stopAfterCount :: Maybe Int
  , emails :: [Text]
  , emailAll :: Maybe Bool
  , slackChannels :: [Text]
  , teams :: [PM.TeamId]
  , visualizationType :: Maybe Text
  , alertRecoveryThreshold :: Maybe Double
  , warningRecoveryThreshold :: Maybe Double
  , active :: Maybe Bool
  , environment :: Maybe Text
  , service :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (Default)
  deriving (AE.ToJSON) via SnakeJSON SnakeSchema MonitorInput


instance ToSchema MonitorInput where
  declareNamedSchema _ = do
    OpenApi.NamedSchema _ flat <- declareNamedSchema (Proxy @(SnakeSchema MonitorInput))
    nested <- OpenApi.declareSchemaRef (Proxy @MonitorConfig)
    pure $ OpenApi.NamedSchema (Just "MonitorInput") $ mempty & OpenApi.oneOf ?~ [OpenApi.Inline flat, nested]


-- | Flat API requests remain valid; as-code documents group related settings.
-- Mixed documents are rejected so an accidentally misplaced field cannot be ignored.
instance AE.FromJSON MonitorInput where
  parseJSON = AE.withObject "MonitorInput" \obj ->
    if any (`KM.member` obj) ["condition", "evaluation", "notify"]
      then configInput <$> AE.parseJSON (AE.Object obj)
      else coerce <$> AE.parseJSON @(SnakeJSON SnakeSchema MonitorInput) (AE.Object obj)
    where
      configInput :: MonitorConfig -> MonitorInput
      configInput cfg =
        let notification = fromMaybe def cfg.notify
         in MonitorInput
              { title = cfg.title
              , severity = cfg.severity
              , active = cfg.active
              , environment = cfg.environment
              , service = cfg.service
              , query = cfg.condition.query
              , unit = cfg.condition.unit
              , alertThreshold = cfg.condition.alertThreshold
              , warningThreshold = cfg.condition.warningThreshold
              , triggerLessThan = cfg.condition.triggerLessThan
              , alertRecoveryThreshold = cfg.condition.alertRecoveryThreshold
              , warningRecoveryThreshold = cfg.condition.warningRecoveryThreshold
              , checkIntervalMins = cfg.evaluation.checkIntervalMins
              , timeWindowMins = cfg.evaluation.timeWindowMins
              , thresholdSustainedForMins = cfg.evaluation.thresholdSustainedForMins
              , subject = notification.subject
              , message = notification.message
              , emailAll = notification.emailAll
              , emails = fold notification.emails
              , slackChannels = fold notification.slackChannels
              , teams = fold notification.teams
              , notifyAfterMins = notification.notifyAfterMins
              , stopAfterCount = notification.stopAfterCount
              , visualizationType = Just $ fromMaybe defaultVisualizationType cfg.visualizationType
              }


monitorConfig :: MonitorInput -> MonitorConfig
monitorConfig inp =
  MonitorConfig
    { title = inp.title
    , severity = inp.severity
    , active = inp.active
    , environment = inp.environment
    , service = inp.service
    , condition =
        MonitorCondition
          { query = inp.query
          , unit = inp.unit
          , alertThreshold = inp.alertThreshold
          , warningThreshold = inp.warningThreshold
          , triggerLessThan = inp.triggerLessThan
          , alertRecoveryThreshold = inp.alertRecoveryThreshold
          , warningRecoveryThreshold = inp.warningRecoveryThreshold
          }
    , evaluation =
        MonitorEvaluation
          { checkIntervalMins = inp.checkIntervalMins
          , timeWindowMins = inp.timeWindowMins
          , thresholdSustainedForMins = inp.thresholdSustainedForMins
          }
    , notify =
        Just
          $ MonitorNotification
            { subject = inp.subject
            , message = inp.message
            , emailAll = inp.emailAll
            , emails = viaNonEmpty toList inp.emails
            , slackChannels = viaNonEmpty toList inp.slackChannels
            , teams = viaNonEmpty toList inp.teams
            , notifyAfterMins = inp.notifyAfterMins
            , stopAfterCount = inp.stopAfterCount
            }
    , visualizationType = mfilter (/= defaultVisualizationType) inp.visualizationType
    }


defaultVisualizationType :: Text
defaultVisualizationType = "timeseries"


-- | The SQL alert evaluation runs for a monitor's KQL: its window, direction and scope.
-- Evaluation recompiles on every check (so parser fixes reach existing monitors), with
-- @tf@ set; writes store the PG form for display. A query that doesn't compile is a 'Left'.
compileAlertSql :: Bool -> Projects.ProjectId -> MonitorInput -> Either Text Text
compileAlertSql tf pid inp =
  let scope = Parser.mkScopedQuery pid (Nothing, Nothing) inp.environment inp.service
      cfg = (Parser.applyScopedQuery scope $ Parser.defSqlQueryCfg pid Parser.fixedUTCTime Nothing Nothing){Parser.alertLookbackMins = inp.timeWindowMins, Parser.alertBelow = inp.triggerLessThan, Parser.targetsTimefusion = tf}
      -- A widget's query keeps its dashboard placeholders; a monitor reads every variable as unset ("all").
      query = Parser.fillQueryTemplate Parser.KqlTemplate (Parser.variablePresetsKQL def pid.toText Nothing Nothing [] Parser.fixedUTCTime) inp.query
   in Parser.parseQueryToComponents cfg query >>= maybeToRight "The query produced no alert SQL" . (.finalAlertQuery) . snd


-- | Build a 'QueryMonitor' from a 'MonitorInput', carrying over what an input can't supply
-- (history, status, widget link) from @existingM@. Every write path goes through here:
-- the API, monitors-as-code, the alert form and widget monitors.
monitorFromInput :: Projects.ProjectId -> UTCTime -> Monitors.QueryMonitorId -> Maybe Monitors.QueryMonitor -> MonitorInput -> Either Text Monitors.QueryMonitor
monitorFromInput pid now mid existingM inp =
  compileAlertSql False pid inp <&> \sql ->
    Monitors.QueryMonitor
      { id = mid
      , projectId = pid
      , createdAt = maybe now (.createdAt) existingM
      , updatedAt = now
      , checkIntervalMins = inp.checkIntervalMins
      , alertThreshold = inp.alertThreshold
      , warningThreshold = inp.warningThreshold
      , logQuery = inp.query
      , logQueryAsSql = sql
      , -- Never evaluated is due now.
        lastEvaluated = existingM >>= (.lastEvaluated)
      , warningLastTriggered = existingM >>= (.warningLastTriggered)
      , alertLastTriggered = existingM >>= (.alertLastTriggered)
      , triggerLessThan = inp.triggerLessThan
      , thresholdSustainedForMins = fromMaybe 0 inp.thresholdSustainedForMins
      , alertConfig =
          Monitors.MonitorAlertConfig
            { unit = mfilter (not . T.null) $ T.strip <$> inp.unit
            , title = inp.title
            , severity = fromMaybe "error" inp.severity
            , subject = fromMaybe inp.title inp.subject
            , message = fromMaybe "" inp.message
            , emails = V.fromList $ CI.mk <$> inp.emails
            , emailAll = fromMaybe False inp.emailAll
            , slackChannels = V.fromList inp.slackChannels
            }
      , -- An omitted @active@ keeps the stored state, and a paused monitor keeps when it paused.
        deactivatedAt = guard (not $ fromMaybe (all (isNothing . (.deactivatedAt)) existingM) inp.active) *> ((existingM >>= (.deactivatedAt)) <|> Just now)
      , deletedAt = Nothing
      , visualizationType = fromMaybe defaultVisualizationType inp.visualizationType
      , teams = V.fromList inp.teams
      , widgetId = existingM >>= (.widgetId)
      , dashboardId = existingM >>= (.dashboardId)
      , alertRecoveryThreshold = inp.alertRecoveryThreshold
      , warningRecoveryThreshold = inp.warningRecoveryThreshold
      , currentStatus = maybe Monitors.MSNormal (.currentStatus) existingM
      , currentValue = maybe 0 (.currentValue) existingM
      , mutedUntil = existingM >>= (.mutedUntil)
      , renotifyIntervalMins = inp.notifyAfterMins
      , stopAfterCount = inp.stopAfterCount
      , notificationCount = maybe 0 (.notificationCount) existingM
      , timeWindowMins = inp.timeWindowMins
      , environment = inp.environment
      , service = inp.service
      }


monitorToInput :: Monitors.QueryMonitor -> MonitorInput
monitorToInput m =
  MonitorInput
    { unit = m.alertConfig.unit
    , title = m.alertConfig.title
    , query = m.logQuery
    , severity = Just m.alertConfig.severity
    , subject = Just m.alertConfig.subject
    , message = Just m.alertConfig.message
    , alertThreshold = m.alertThreshold
    , warningThreshold = m.warningThreshold
    , triggerLessThan = m.triggerLessThan
    , checkIntervalMins = m.checkIntervalMins
    , timeWindowMins = m.timeWindowMins
    , thresholdSustainedForMins = Just m.thresholdSustainedForMins
    , notifyAfterMins = m.renotifyIntervalMins
    , stopAfterCount = m.stopAfterCount
    , emails = V.toList (CI.original <$> m.alertConfig.emails)
    , emailAll = Just m.alertConfig.emailAll
    , slackChannels = V.toList m.alertConfig.slackChannels
    , teams = V.toList m.teams
    , visualizationType = Just m.visualizationType
    , alertRecoveryThreshold = m.alertRecoveryThreshold
    , warningRecoveryThreshold = m.warningRecoveryThreshold
    , active = Just (isNothing m.deactivatedAt)
    , environment = m.environment
    , service = m.service
    }
