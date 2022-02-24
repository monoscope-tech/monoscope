{-# LANGUAGE DuplicateRecordFields #-}

module Models.Projects.ProjectsEmail () where

import Control.Lens qualified as Lens
import Data.List.NonEmpty qualified as NonEmptyDataList
import Data.Text qualified as T
import Data.Text.Conversions qualified as TC
import GHC.Exts qualified
import Network.SendGridV3.Api
import Network.Wreq qualified as Wreq
import Relude
import System.Environment (lookupEnv)

-- sendgrid's ApiKey expects a Text paramater but lookupEnv returns IO (Maybe String). This class and instances are generated to allow conversion to Text
class ToText a where
  toText :: Maybe a -> T.Text

instance TC.ToText (IO (Maybe String))

sendGridApiKey :: ApiKey
sendGridApiKey =
  let env = TC.toText $ lookupEnv "SENDGRIDAPIKEY"
   in ApiKey env

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

-- rName rAddress -> receiver email and address sName sAddress sender email and address
emailCtx :: T.Text -> T.Text -> T.Text -> T.Text -> Mail () ()
emailCtx rName rAddress sName sAddress =
  let to = personalization $ NonEmptyDataList.fromList [MailAddress rAddress rName]
      from = MailAddress sAddress sName
      subject = "Email Subject"
      content = patternMatchMailContent (Just contentMail)
   in mail [to] from subject content

-- test values..values will be parsed from create project and invite members form
sendEmail :: Mail () ()
sendEmail = emailCtx "anthony" "anthonyalaribe@gmail.com" "david" "davidoluwatobi41@gmail.com"

sendInviteMail :: Mail () () -> IO ()
sendInviteMail sendEmailV = do
  eResponse <- sendMail sendGridApiKey (sendEmailV {_mailSendAt = Just 1516468000})
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response Lens.^. Wreq.responseStatus . Wreq.statusCode)
