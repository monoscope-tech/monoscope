{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use safeToEnum" #-}
module Pkg.Mail (sendEmail, sendSlackMessage) where

import Control.Lens ((.~))
import Data.Aeson.QQ
import Data.Pool
import Data.Text
import Database.PostgreSQL.Entity.DBT (withPool)
import Database.PostgreSQL.Simple (Connection)
import Models.Apis.Slack
import Models.Projects.Projects qualified as Projects
import Network.HaskellNet.SMTP
import Network.Mail.Mime
import Network.Wreq
import Prelude
import System.Config qualified as Config


sendEmail :: Config.EnvConfig -> Text -> Text -> LText -> IO ()
sendEmail config reciever subject body = doSMTPPort (toString config.smtpHost) (toEnum config.smtpPort) $ \conn -> do
  authSucceed <- authenticate PLAIN (toString config.smtpUsername) (toString config.smtpPassword) conn
  if authSucceed
    then do
      mail <-
        simpleMail
          (Address Nothing reciever)
          (Address (Just "Apitoolkit") config.smtpSender)
          subject
          body
          body
          []
      sendMail mail conn
    else error "SMTP Authentication failed."


sendSlackMessage :: Pool Connection -> Projects.ProjectId -> Text -> IO ()
sendSlackMessage pool pid message = do
  slackData <- liftIO $ withPool pool $ getProjectSlackData pid
  let opts = defaults & header "Content-Type" .~ ["application/json"]
  let payload =
        [aesonQQ| {
                "text": #{message},
                "type":"mrkdwn"
              }
            |]
  response <- postWith opts (toString (maybe "" (\s -> s.webhookUrl) slackData)) payload
  pass
