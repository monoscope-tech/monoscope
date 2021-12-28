{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
  )
where

import Colog.Core (LogAction (..), logStringStdout, (<&))
import qualified Config
import Configuration.Dotenv as Dotenv
import Control.Lens ((&), (.~), (<&>), (?~), (^.), (^?), _Just)
import Control.Monad (when)
import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Data.Aeson
  ( FromJSON,
    Object,
    ToJSON,
    eitherDecode,
  )
import Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Base64 as B64 (decodeBase64)
import Data.Maybe (catMaybes)
import Data.Pool as Pool
import Database.PostgreSQL.Simple (Connection, Only (Only), close, connectPostgreSQL, query_)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory), runMigration)
import Database.PostgreSQL.Simple.Migration as Migrations
import Deriving.Aeson
  ( CamelToSnake,
    CustomJSON (CustomJSON),
    FieldLabelModifier,
    Generic,
    OmitNothingFields,
    StripPrefix,
  )
import GHC.Generics ()
import qualified Network.Google as Google
import qualified Network.Google.Env as Env
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Resource.PubSub.Projects.Subscriptions.Pull as PubSubPull
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import ProcessMessage
import Relude (Applicative (pure), Either (Left, Right), Eq, IO, Int, Maybe, Monad (return), MonadIO (liftIO), Proxy (..), Semigroup ((<>)), Show, String, Text, Traversable (mapM), encodeUtf8, error, fromRight, fromStrict, print, show, stdout, traceM, ($), (&), (++), (.), (<&>), (=<<))
import qualified Relude.Unsafe as Unsafe
import Servant
  ( Get,
    Handler,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import System.Envy (FromEnv, Option (Option), decodeEnv, gFromEnvCustom, runEnv)
import System.IO (stdout)

-- deriving instance Show (Env.Env)

-- {
--     "message": {
--         "attributes": {
--             "key": "value"
--         },
--         "data": "SGVsbG8gQ2xvdWQgUHViL1N1YiEgSGVyZSBpcyBteSBtZXNzYWdlIQ==",
--         "messageId": "2070443601311540",
--         "message_id": "2070443601311540",
--         "publishTime": "2021-02-26T19:13:55.749Z",
--         "publish_time": "2021-02-26T19:13:55.749Z",
--     },
--    "subscription": "projects/myproject/subscriptions/mysubscription"
-- }
data PubSubMessage = PubSubMessage
  { pmMessageId :: String,
    pmDataField :: String
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "pm", CamelToSnake]] PubSubMessage

data PubsubMessageWrapper = PubsubMessageWrapper
  { pmwMessage :: PubSubMessage,
    pmwSubscription :: String
  }
  deriving (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "pmw", CamelToSnake]] PubsubMessageWrapper

data User = User
  { userId :: Int,
    userFirstName :: String,
    userLastName :: String
  }
  deriving (Eq, Show)

$(deriveJSON Aeson.defaultOptions ''User)

type API =
  "users" :> Get '[JSON] [User]
    :<|> "pubsub-endpoint" :> ReqBody '[JSON] PubsubMessageWrapper :> Post '[JSON] NoContent

startApp :: IO ()
startApp = do
  let logger = logStringStdout
  Dotenv.loadFile Dotenv.defaultConfig
  env <- decodeEnv :: IO (Either String Config.EnvConfig)
  logger <& "..." <> show env
  case env of
    Left err -> logger <& "Error: " <> show err
    Right envConfig -> do
      logger <& "Starting server at port " <> show (Config.port envConfig)
      conn <- connectPostgreSQL $ encodeUtf8 (Config.databaseUrl envConfig)
      initializationRes <- Migrations.runMigration conn Migrations.defaultOptions MigrationInitialization
      logger <& "migration initialized " <> show initializationRes
      migrationRes <- Migrations.runMigration conn Migrations.defaultOptions $ MigrationDirectory (Config.migrationsDir envConfig)
      logger <& "migration result: " <> show migrationRes
      poolConn <- Pool.createPool (pure conn) close 1 100000000 50
      pubsubService logger envConfig poolConn

-- pubsubService connects to the pubsub service and listens for  messages,
-- then it calls the processMessage function to process the messages, and
-- acknoleges the list message in one request.
pubsubService :: LogAction IO String -> Config.EnvConfig -> Pool.Pool Connection -> IO ()
pubsubService logger envConfig conn = do
  lgr <- Google.newLogger Google.Trace stdout
  env <-
    Google.newEnv
      <&> (Google.envLogger .~ lgr)
        . (Google.envScopes .~ PubSub.pubSubScope)
  let pullReq = PubSub.pullRequest & PubSub.prMaxMessages ?~ 1
  let subscription = "projects/past-3/subscriptions/apitoolkit-go-client-sub"
  runResourceT . Google.runGoogle env $ do
    pullResp <- Google.send $ PubSub.projectsSubscriptionsPull pullReq subscription
    let messages = pullResp ^. PubSub.prReceivedMessages
    msgIds <- liftIO $ mapM (processMessage logger envConfig conn) messages
    -- _ <- Google.send $ PubSub.projectsSubscriptionsAcknowledge (PubSub.acknowledgeRequest & PubSub.arAckIds .~ catMaybes msgIds) subscription
    pure ()

--
--
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
  return users
    :<|> handlePubSubMessage

handlePubSubMessage :: PubsubMessageWrapper -> Handler NoContent
handlePubSubMessage PubsubMessageWrapper {pmwMessage = PubSubMessage {pmMessageId = messageId, pmDataField = dataField}} = do
  let message = "Received message with id " <> messageId <> " and data " <> dataField
  -- logStringStdout message
  return NoContent

users :: [User]
users =
  [ User 1 "Isaac" "Newton",
    User 2 "Albert" "Einstein",
    User 3 "Stephen" "Hawking"
  ]
