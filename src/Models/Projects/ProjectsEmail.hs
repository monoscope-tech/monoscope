{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Models.Projects.ProjectsEmail
  ( sendEmail,
    sendInviteMail,
    inviteUUID,
    insertInviteID,
    invitedUserConstruct
  )
where

import Control.Lens qualified as Lens
import Data.List.NonEmpty qualified as NonEmptyDataList
import Models.Users.Users qualified as Users
import Web.Auth qualified as Auth
import Data.Text qualified as T
import Data.Time
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT ( QueryNature (..), query, queryOne, withPool )
import GHC.Exts
import Config qualified
import Config
import Network.SendGridV3.Api
import Network.Wreq qualified as Wreq
import Relude
import Data.Vector qualified as Vector
import Data.Time (ZonedTime, getZonedTime)
import Data.UUID qualified as UUID
import Models.Projects.Projects as Project
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple ( FromRow, ToRow )
import Database.PostgreSQL.Transact qualified as PgT
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple (Only (Only))
import Deriving.Aeson qualified as DAE
import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Servant ( Header, Headers, NoContent (..) )
import Web.Cookie (SetCookie)
       
sendGridApiKeyE :: Config.EnvConfig -> ApiKey
sendGridApiKeyE Config.EnvConfig { sendGridApiKey } = 
  ApiKey sendGridApiKey

-- instances derived to avoid error no instance for (IsString ([Char] -> Text)) for contentMail due to the use of overloadedstrings
class GHC.Exts.IsString a => SafeIsString a where
  fromString :: String -> a
  fromString = GHC.Exts.fromString

instance SafeIsString String

instance SafeIsString T.Text

instance IsString (ZonedTime)
instance IsString (Maybe ZonedTime)
instance IsString (Users.UserId)

contentMail :: T.Text -> T.Text
contentMail inviteid =
  mailBody
    <> link
  where
    mailBody = "ApiToolKit Mail Invite. Click on the link below"
    link = "<a href =https://apitoolkit.io/invite_link/>" <> inviteid

patternMatchMailContent :: Maybe T.Text -> Maybe (NonEmpty MailContent)
patternMatchMailContent (Just txt) = Just (NonEmptyDataList.fromList [mailContentHtml txt])
patternMatchMailContent Nothing = Nothing

-- rAddress -> receiver email
emailCtx :: UUID.UUID -> T.Text -> Mail () ()
emailCtx invID rAddress =
  let to = personalization $ NonEmptyDataList.fromList [MailAddress rAddress "User"]
      textInvID = UUID.toText invID
      from = MailAddress ("hello@apitoolkit.io/") "Api Toolkit"
      subject = "Email Subject"
      content = patternMatchMailContent (Just (contentMail textInvID))
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
    userID :: Users.UserId
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, Default)
  deriving (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] InviteTable
  deriving
    (Entity)
    via (GenericEntity '[Schema "users", TableName "inviteTable", PrimaryKey "invite_id", FieldModifiers '[CamelToSnake]] InviteTable)

inviteUUID :: UUID.UUID -> Users.UserId-> ZonedTime -> IO InviteTable
inviteUUID invID uid tNow = do
  pure $ InviteTable 
    { inviteID = invID,
      createdAt = tNow,
      deletedAt = Nothing,
      expired = False,
      userID = uid
    }

updateUserStatus :: Users.User -> Maybe UUID.UUID -> PgT.DBT IO Int64
updateUserStatus users user_id = PgT.execute q users
  where
    q = [sql|
    UPDATE users.users(active) VALUES (true) where users.users.invite_id = user_id|]

invitedUserConstruct :: T.Text -> Config.DashboardM ( Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent)
invitedUserConstruct inviteid  = do
  pool <- asks pool
  liftIO $ 
    withPool pool $ do
      status <- updateUserStatus Users.User { inviteId = UUID.fromText inviteid,
      id = "",
      createdAt = "",
      updatedAt = "",
      deletedAt = "",
      active = True,
      firstName = "",
      lastName = "",
      displayImageUrl = "",
      email = ""
      } (UUID.fromText inviteid)
      pass
  Auth.loginH

insertInviteID :: InviteTable -> PgT.DBT IO ()
insertInviteID = insert @InviteTable
    
