{-# LANGUAGE DuplicateRecordFields #-}

module Models.Projects.ProjectsEmail (
  sendEmail,
) where

import Data.List.NonEmpty qualified as NonEmptyDataList
import Data.Text qualified as T
import Network.SendGridV3.Api (
  Mail,
  MailAddress (MailAddress),
  MailContent,
  mail,
  mailContentHtml,
  personalization,
 )
import Relude (Maybe (..), NonEmpty, Semigroup ((<>)), ($))


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
