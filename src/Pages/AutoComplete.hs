module Pages.AutoComplete (getH) where

import Config
import Data.Aeson
import Data.Aeson qualified as AE
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Fields (FieldCategoryEnum (FCRequestBody), parseFieldCategoryEnum)
import Models.Apis.Fields.Query (autoCompleteFields)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Relude


getH :: Sessions.PersistentSession -> Projects.ProjectId -> Maybe Text -> Maybe Text -> DashboardM AE.Value
getH sess pid fcategory prefix = do
  pool <- asks pool
  fields <- liftIO $ withPool pool do
    case (fcategory, prefix) of
      (Just c, Just p) -> autoCompleteFields pid (fromMaybe FCRequestBody category) p
        where
          category = parseFieldCategoryEnum c
      (_, _) -> pure []
  let js = toJSON fields
  pure js
