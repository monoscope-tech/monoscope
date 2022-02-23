module Models.Projects.ProjectsEmail where

import qualified Data.Text as T
import qualified Data.Text.Conversions as TC
import Data.Text.Conversions
import Network.SendGridV3.Api
import qualified Control.Lens as Lens
import qualified Network.Wreq as Wreq
import qualified GHC.Exts
import qualified System.Environment as SE
import Relude
import qualified Data.List.NonEmpty as NonEmptyDataList


-- sendgrid's ApiKey expects a Text paramater but lookupEnv returns IO (Maybe String). This class and instances are generated to allow conversion to Text
class ToText a where
  toText :: Maybe a -> T.Text

instance TC.ToText (IO (Maybe String))

sendGridApiKey :: ApiKey
sendGridApiKey = 
  let 
    env = TC.toText $ SE.lookupEnv "SENDGRIDAPIKEY"
  in ApiKey env


-- instances derived to avoid error no instance for (IsString ([Char] -> Text)) for contentMail due to the use of overloadedstrings
class GHC.Exts.IsString a => SafeIsString a where
  fromString :: String -> a
  fromString = GHC.Exts.fromString

instance SafeIsString String 
instance SafeIsString T.Text 

contentMail :: T.Text
contentMail =
    mailBody <>
    link
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
sendInviteMail sendEmail = do
  eResponse <- sendMail sendGridApiKey (sendEmail { _mailSendAt = Just 1516468000 })
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response Lens.^. Wreq.responseStatus . Wreq.statusCode)