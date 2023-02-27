{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use safeToEnum" #-}
module Pkg.Mail (sendEmail) where

import Config qualified
import Data.Text
import Network.HaskellNet.SMTP
import Network.Mail.Mime
import Relude

sendEmail :: Config.EnvConfig -> Text -> Text -> LText -> IO ()
sendEmail config reciever subject body = doSMTPPort (toString $ config.smtpHost) (toEnum $ config.smtpPort) $ \conn -> do
  authSucceed <- authenticate PLAIN (toString $ config.smtpUsername) (toString $ config.smtpPassword) conn
  if authSucceed
    then do
      mail <-
        simpleMail
          (Address Nothing reciever)
          (Address (Just "Apitoolkit") (config.smtpSender))
          subject
          body
          body
          []
      sendMail mail conn
    else error "SMTP Authentication failed."
