{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendEmail, sendSlackMessage) where

import Control.Lens ((.~))
import Data.Aeson.QQ (aesonQQ)
import Data.Pool ()
import Effectful (
  Eff,
  Effect,
  IOE,
  MonadIO (liftIO),
  runEff,
  type (:>),
 )
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Reader.Static (ask)
import Log qualified
import Models.Apis.Slack (SlackData (..), getProjectSlackData)
import Models.Projects.Projects qualified as Projects
import Network.HaskellNet.SMTP (
  AuthType (PLAIN),
  authenticate,
  doSMTPPort,
  sendMail,
 )
import Network.Mail.Mime (Address (Address), simpleMail)
import Network.Wreq (defaults, header, postWith)
import Relude hiding (ask)
import System.Config qualified as Config
import System.Types (ATBackgroundCtx, ATBaseCtx)


sendEmail :: Text -> Text -> LText -> ATBackgroundCtx ()
sendEmail reciever subject body = do
  appCtx <- ask @Config.AuthContext
  if appCtx.config.smtpHost /= "" && appCtx.config.smtpUsername /= ""
    then liftIO $ doSMTPPort (toString appCtx.config.smtpHost) (toEnum appCtx.config.smtpPort) $ \conn -> do
      authSucceed <- authenticate PLAIN (toString appCtx.config.smtpUsername) (toString appCtx.config.smtpPassword) conn
      if authSucceed
        then do
          mail <-
            simpleMail
              (Address Nothing reciever)
              (Address (Just "Apitoolkit") appCtx.config.smtpSender)
              subject
              body
              body
              []
          sendMail mail conn
        else error "SMTP Authentication failed."
    else Log.logAttention "sendEmail is not configured. But was called" (reciever, subject, body)


sendSlackMessage :: (DB :> es, Log :> es, IOE :> es) => Projects.ProjectId -> Text -> Eff es ()
sendSlackMessage pid message = do
  slackData <- dbtToEff $ getProjectSlackData pid
  case slackData of
    Just s -> liftIO $ slackPostWebhook s.webhookUrl message
    Nothing -> Log.logAttention "sendSlackMessage is not configured. But was called" (pid, message)


slackPostWebhook :: Text -> Text -> IO ()
slackPostWebhook webhookUrl message = do
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  let payload =
        [aesonQQ| {
                "text": #{message},
                "type":"mrkdwn"
              }
            |]
  response <- liftIO $ postWith opts (toString webhookUrl) payload
  pass
