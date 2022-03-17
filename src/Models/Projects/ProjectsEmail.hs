{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Projects.ProjectsEmail
  ( sendEmail,
    sendInviteMail,
    inviteUUID,
    insertInviteID
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
import Data.Time (ZonedTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects as Project
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Transact qualified as PgT
import Deriving.Aeson qualified as DAE
import Data.Aeson (FromJSON, ToJSON)
import Data.Default
       
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
emailCtx :: UUID.UUID -> T.Text -> Mail () ()
emailCtx invID rAddress =
  let to = personalization $ NonEmptyDataList.fromList [MailAddress rAddress "User"]
      textInvID = UUID.toText invID
      from = MailAddress ("hello@apitoolkit.io/" <> textInvID) "Api Toolkit"
      subject = "Email Subject"
      content = patternMatchMailContent (Just contentMail)
   in mail [to] from subject content

sendEmail :: UUID.UUID -> T.Text -> Mail () ()
sendEmail invID txt = emailCtx invID txt

sendInviteMail :: Config.EnvConfig -> Mail () () -> IO ()
sendInviteMail env sendEmailV = do
  eResponse <- sendMail (sendGridApiKeyE env) (sendEmailV {_mailSendAt = Just 1516468000})
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response Lens.^. Wreq.responseStatus . Wreq.statusCode) 


data InviteTable = InviteTable 
  { inviteID :: UUID.UUID,
    createdAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    expired :: Bool,
    invitedProjectID :: Project.ProjectId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] InviteTable
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "inviteTable", PrimaryKey "id", FieldModifiers '[CamelToSnake]] InviteTable)

inviteUUID :: UUID.UUID -> Project.ProjectId-> ZonedTime -> IO InviteTable
inviteUUID invID pid tNow = do
  pure $ InviteTable 
    { inviteID = invID,
      createdAt = tNow,
      deletedAt = Nothing,
      expired = False,
      invitedProjectID = pid
    }

insertInviteID :: InviteTable -> PgT.DBT IO ()
insertInviteID = insert @InviteTable
    