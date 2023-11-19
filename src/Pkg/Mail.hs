{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use safeToEnum" #-}
module Pkg.Mail (sendEmail, sendSlackMessage) where

import Config qualified
import Control.Lens ((.~))
import Data.Aeson.QQ
import Data.Text
import Models.Apis.Slack (SlackData)
import Network.HaskellNet.SMTP
import Network.Mail.Mime
import Network.Wreq
import Relude


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


sendSlackMessage :: Config.EnvConfig -> SlackData -> Text -> IO ()
sendSlackMessage config slackData message = do
  let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ ["Bearer " <> slackData.accessToken]
  let payload =
        [aesonQQ| {
                "channel": #{slackData.channel},
                "text": #{message}
              }
            |]
  response <- postWith opts "https://slack.com/api/chat.postMessage" payload
  pass
