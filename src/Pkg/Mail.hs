{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pkg.Mail (sendEmail, sendSlackMessage, sendSlackMessageBase) where

import Control.Lens ((.~))
import Data.Aeson.QQ (aesonQQ)
import Data.Pool ()
import Data.Text (Text)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
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
  liftIO $ doSMTPPort (toString appCtx.config.smtpHost) (toEnum appCtx.config.smtpPort) $ \conn -> do
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


sendSlackMessage :: Projects.ProjectId -> Text -> ATBackgroundCtx ()
sendSlackMessage pid message = do
  slackData <- dbtToEff $ getProjectSlackData pid
  liftIO $ slackPostWebhook (maybe "" (\s -> s.webhookUrl) slackData) message


sendSlackMessageBase :: Projects.ProjectId -> Text -> ATBaseCtx ()
sendSlackMessageBase pid message = do
  slackData <- dbtToEff $ getProjectSlackData pid
  liftIO $ slackPostWebhook (maybe "" (\s -> s.webhookUrl) slackData) message


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
