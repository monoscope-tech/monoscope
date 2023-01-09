{-# LANGUAGE DuplicateRecordFields #-}

module Models.Projects.ProjectsEmail (
  sendEmail,
) where

import Control.Lens qualified as Lens
import Data.List.NonEmpty qualified as NonEmptyDataList
import Data.Text qualified as T
import Data.Text.Conversions qualified as TC
import GHC.Exts
import Network.SendGridV3.Api
import Network.Wreq qualified as Wreq
import Relude
import Relude.Unsafe qualified as Unsafe

-- sendgrid's ApiKey expects a Text paramater but lookupEnv returns IO (Maybe String). This class and instances are generated to allow conversion to Text
class ToText a where
  toText :: Maybe a -> T.Text

sendGridApiKey :: IO ApiKey
sendGridApiKey = do
  -- FIXME: We shoul load all env variables via the Config package like the other envs
  apiKeyEnv <- lookupEnv "SENDGRIDAPIKEY"
  let env = TC.toText $ Unsafe.fromJust apiKeyEnv
  pure $ ApiKey env

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
      from = MailAddress "apitoolkit@testemail.com" "Api Toolkit"
      subject = "Email Subject"
      content = patternMatchMailContent (Just contentMail)
   in mail [to] from subject content

sendEmail :: T.Text -> Mail () ()
sendEmail = emailCtx

sendInviteMail :: Mail () () -> IO ()
sendInviteMail sendEmailV = do
  apiKey <- sendGridApiKey
  eResponse <- sendMail apiKey (sendEmailV {_mailSendAt = Just 1516468000})
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response Lens.^. Wreq.responseStatus . Wreq.statusCode)
