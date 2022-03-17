{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Projects.ProjectsEmail
  ( sendEmail,
    sendInviteMail,
    -- sendEmailTrial
  )
where

import Control.Lens qualified as Lens
import Data.List.NonEmpty qualified as NonEmptyDataList
import Data.Text qualified as T
import GHC.Exts
import Config qualified
import Network.SendGridV3.Api
import Network.Wreq qualified as Wreq
import Relude
       
sendGridApiKeyE :: Config.EnvConfig -> ApiKey
sendGridApiKeyE Config.EnvConfig { sendGridApiKey } = 
  ApiKey sendGridApiKey

-- instances derived to avoid error no instance for (IsString ([Char] -> Text)) for contentMail due to the use of overloadedstrings
class GHC.Exts.IsString a => SafeIsString a where
  fromString :: String -> a
  fromString = GHC.Exts.fromString

instance SafeIsString String

instance SafeIsString T.Text

contentMail :: T.Text
contentMail =
  mailBody
    <> link
  where
    mailBody = "ApiToolKit Mail Invite. Click on the link below"
    link = "<a href =https://apitoolkit.io>"

patternMatchMailContent :: Maybe T.Text -> Maybe (NonEmpty MailContent)
patternMatchMailContent (Just txt) = Just (NonEmptyDataList.fromList [mailContentHtml txt])
patternMatchMailContent Nothing = Nothing

-- rAddress -> receiver email
emailCtx :: T.Text -> Mail () ()
emailCtx rAddress =
  let to = personalization $ NonEmptyDataList.fromList [MailAddress rAddress "User"]
      from = MailAddress "hello@apitoolkit.io" "Api Toolkit"
      subject = "Email Subject"
      content = patternMatchMailContent (Just contentMail)
   in mail [to] from subject content

sendEmail :: T.Text -> Mail () ()
sendEmail = emailCtx

sendInviteMail :: Config.EnvConfig -> Mail () () -> IO ()
sendInviteMail env sendEmailV = do
  eResponse <- sendMail (sendGridApiKeyE env) (sendEmailV {_mailSendAt = Just 1516468000})
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response Lens.^. Wreq.responseStatus . Wreq.statusCode) 

