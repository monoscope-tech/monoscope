{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use safeToEnum" #-}
module Pkg.Mail (sendEmail, sendSlackMessage) where

import Config qualified
import Control.Lens ((.~))
import Data.Aeson.QQ
import Data.Text
import Models.Apis.Slack
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


sendSlackMessage :: Config.EnvConfig -> Text -> Text -> IO ()
sendSlackMessage config accessToken message = do
  let authToken = encodeUtf8 $ "Bearer " <> accessToken
  let opts = defaults & header "Content-Type" .~ ["application/json"] & header "Authorization" .~ [authToken]
  let payload =
        [aesonQQ| {
                "channel": "C065UEFQL0J",
                "text": #{message}
              }
            |]
  response <- postWith opts "https://slack.com/api/chat.postMessage" payload
  traceShowM response
  pass
