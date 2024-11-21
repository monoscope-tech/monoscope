{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module System.Logging (
  makeLogger,
  runLog,
  timeAction,
  LoggingDestination (..),
)
where

import Data.Aeson qualified as AE
import Data.ByteString.Char8 qualified as BS
import Data.Default (Default (..))
import Data.Time.Clock as Time (NominalDiffTime, diffUTCTime)
import Effectful (
  Eff,
  Effect,
  IOE,
  MonadUnliftIO (withRunInIO),
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Log (Logger)
import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Log.Internal.Logger (withLogger)
import Relude
import System.Envy (ReadShowVar (..), Var)


data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving stock (Show, Read, Generic)
  deriving (Var) via (ReadShowVar LoggingDestination)


instance Default LoggingDestination where
  def = StdOut


-- | Wrapper around 'Log.runLogT' with necessary metadata
runLog
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => Text
  -> Logger
  -> Eff (Log ': es) a
  -> Eff es a
runLog envTxt logger = Log.runLog ("[AT]-" <> envTxt) logger Log.defaultLogLevel


makeLogger :: IOE :> es => LoggingDestination -> (Logger -> Eff es a) -> Eff es a
makeLogger StdOut = LogBulk.withBulkStdOutLogger
makeLogger Json = LogBulk.withBulkJsonStdOutLogger
makeLogger JSONFile = withJSONFileBackend FileBackendConfig{destinationFile = "logs/apitoolkit.json"}


newtype FileBackendConfig = FileBackendConfig
  { destinationFile :: FilePath
  }
  deriving stock (Eq, Ord, Show, Generic)


withJSONFileBackend
  :: forall (es :: [Effect]) (a :: Type)
   . IOE :> es
  => FileBackendConfig
  -> (Logger -> Eff es a)
  -> Eff es a
withJSONFileBackend FileBackendConfig{destinationFile} action = withRunInIO $ \unlift -> do
  liftIO $ BS.hPutStrLn stdout $ BS.pack $ "Redirecting logs to " <> destinationFile
  logger <- liftIO $ Log.mkLogger "file-json" $ \msg -> liftIO $ BS.appendFile destinationFile (toStrict $ AE.encode msg <> "\n")
  withLogger logger (unlift . action)


timeAction
  :: forall (es :: [Effect]) (a :: Type)
   . Time :> es
  => Eff es a
  -> Eff es (a, NominalDiffTime)
timeAction action = do
  start <- Time.currentTime
  result <- action
  end <- Time.currentTime
  pure (result, Time.diffUTCTime end start)
