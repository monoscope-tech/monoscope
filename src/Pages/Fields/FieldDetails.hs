module Pages.Fields.FieldDetails (fieldPutH, EditFieldForm) where

import Data.Aeson (FromJSON, encode)
import Data.Aeson.QQ (aesonQQ)
import Data.Digest.XXHash (xxHash)
import Data.Time (getZonedTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (QueryNature (Update), execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Lucid (Html)
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Numeric (showHex)
import Relude (
  Applicative (pure),
  Bool (..),
  ConvertUtf8 (decodeUtf8, encodeUtf8),
  Generic,
  Maybe (..),
  MonadIO (liftIO),
  Semigroup ((<>)),
  Show,
  Text,
  ToText (toText),
  fromMaybe,
  ($),
  (<$>),
 )
import Servant (Headers, addHeader)
import Servant.Htmx (HXTrigger)
import System.Types (ATAuthCtx)
import Web.FormUrlEncoded (FromForm)


data EditFieldForm = EditFieldForm
  { isRequired :: Maybe Text
  , isEnum :: Maybe Text
  , formats :: [Text]
  , description :: Text
  , fieldHash :: Text
  , fieldType :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm, FromJSON)


parseCheckbox :: Maybe Text -> Bool
parseCheckbox (Just _) = True
parseCheckbox Nothing = False


fieldPutH :: Projects.ProjectId -> Fields.FieldId -> EditFieldForm -> ATAuthCtx (Headers '[HXTrigger] (Html ()))
fieldPutH pid fid editData = do
  _ <- Sessions.sessionAndProject pid
  fi <- dbtToEff $ execute Update [sql|update apis.fields set is_required = ?, is_enum = ?, description=? where id=?|] (parseCheckbox editData.isRequired, parseCheckbox editData.isEnum, editData.description, fid)
  now <- liftIO getZonedTime
  let formats =
        ( \format ->
            Formats.Format
              { id = Formats.FormatId UUID.nil
              , createdAt = now
              , updatedAt = now
              , projectId = pid
              , fieldHash = editData.fieldHash
              , fieldType = fromMaybe Fields.FTString $ Fields.parseFieldTypes editData.fieldType
              , fieldFormat = format
              , examples = []
              , hash = editData.fieldHash <> toText (showHex (xxHash $ encodeUtf8 format) "")
              }
        )
          <$> editData.formats
  r <- dbtToEff $ Formats.insertFormats formats
  let hxTriggerData = decodeUtf8 $ encode [aesonQQ| {"closeModal": "","successToast": ["Field edited successfully"]}|]
  pure $ addHeader hxTriggerData ""
