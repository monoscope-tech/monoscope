{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Models.Projects.Projects
  ( Project (..),
    ProjectId (..),
    CreateProject (..),
    insertProject,
    projectIdText,
    selectProjectsForUser,
    selectProjectForUser,
    updateProject,
    deleteProject,
    projectById,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AET
import Data.Default
import Data.Default.Instances
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, query, queryOne, query_, withPool)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Transact as PgT
import qualified Deriving.Aeson as DAE
import GHC.Generics (Generic)
import qualified Models.Users.Users as Users
import Optics.Operators
import Optics.TH
import Relude
import qualified Relude.Unsafe as Unsafe
import Web.HttpApiData
import qualified Data.List.NonEmpty as NonEmptyDataList
import Network.SendGridV3.Api
import qualified Control.Lens as Lens
import Network.Wreq (responseStatus, statusCode)

newtype ProjectId = ProjectId {unProjectId :: UUID.UUID}
  deriving stock (Generic, Show)
  deriving
    (Eq, Ord, ToJSON, FromJSON, FromField, ToField, FromHttpApiData, Default)
    via UUID.UUID
  deriving anyclass (FromRow, ToRow)

projectIdText :: ProjectId -> Text
projectIdText = UUID.toText . unProjectId

data Project = Project
  { id :: ProjectId,
    createdAt :: ZonedTime,
    updatedAt :: ZonedTime,
    deletedAt :: Maybe ZonedTime,
    active :: Bool,
    title :: Text,
    description :: Text,
    hosts :: Vector.Vector Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (FromJSON, ToJSON)
    via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] Project
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "projects", PET.TableName "projects", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] Project)

makeFieldLabelsNoPrefix ''Project

data CreateProject = CreateProject
  { id :: ProjectId,
    title :: Text,
    description :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (PET.Entity)
    via (PET.GenericEntity '[PET.Schema "projects", PET.TableName "projects", PET.PrimaryKey "id", PET.FieldModifiers '[PET.CamelToSnake]] CreateProject)

makeFieldLabelsNoPrefix ''CreateProject

insertProject :: CreateProject -> PgT.DBT IO ()
insertProject = insert @CreateProject

projectById :: ProjectId -> PgT.DBT IO (Maybe Project)
projectById = selectById @Project

selectProjectsForUser :: Users.UserId -> PgT.DBT IO (Vector.Vector Project)
selectProjectsForUser = query Select q
  where
    q = [sql| select pp.* from projects.projects as pp join projects.project_members as ppm on (pp.id=ppm.project_id) where ppm.user_id=? order by updated_at desc|]

selectProjectForUser :: (Users.UserId, ProjectId) -> PgT.DBT IO (Maybe Project)
selectProjectForUser = queryOne Select q
  where
    q = [sql| select pp.* from projects.projects as pp join projects.project_members as ppm on (pp.id=ppm.project_id) where ppm.user_id=? and ppm.project_id=? order by updated_at desc|]

editProjectGetH :: ProjectId -> PgT.DBT IO (Vector.Vector Project)
editProjectGetH pid = query Select q (Only pid)
  where
    q =
      [sql|
        SELECT pp*, ppm* FROM projects.projects AS pp 
            INNER JOIN projects.project_members AS ppm
            ON pp.id = pid 
        WHERE ppm.project_id = pp.id;|]

updateProject :: CreateProject -> PgT.DBT IO Int64
updateProject = PgT.execute q
  where
    q =
      [sql|
      UPDATE projects.projects(title, description) VALUES (?, ?);|]

deleteProject :: ProjectId -> PgT.DBT IO ()
deleteProject pid = delete @Project (Only pid)


-- email with sendgrid

-- get from env later plus set up actual sendgrid api key in env
sendGridApiKey :: ApiKey
sendGridApiKey = 
  ApiKey "SG........."

nmailContentText :: T.Text -> Maybe MailContent
nmailContentText txt = MailContent "text/plain" txt

nfromList :: [a] -> Maybe (NonEmpty a) 
nfromList [text] = Just text

mailContentHtml :: T.Text -> Maybe MailContent
mailContentHtml html = MailContent "text/html" html

-- rName rAddress -> receiver email and address sName sAddress sender email and address
emailCtx :: T.Text -> T.Text -> T.Text -> T.Text -> Mail () ()
emailCtx rName rAddress sName sAddress =
  let to = personalization $ NonEmptyDataList.fromList [MailAddress rAddress rName]
      from = MailAddress sAddress sName
      subject = "Email Subject"
      -- mailContentText will be swapped with mailContentText and the appropriate html email
      content = nfromList [nmailContentText "Mail Content"]
  in mail [to] from subject content

-- test values..values will be parsed from create project and invite members form
sendEmail :: Mail () ()
sendEmail = emailCtx "anthony" "anthonyalaribe@gmail.com" "david" "davidoluwatobi41@gmail.com"
  
sendInviteMail :: Mail () () -> IO ()
sendInviteMail sendEmail = do
  eResponse <- sendMail sendGridApiKey (sendEmail { _mailSendAt = Just 1516468000 })
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response Lens.^. responseStatus . statusCode)