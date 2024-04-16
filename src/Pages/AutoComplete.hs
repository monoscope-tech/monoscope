module Pages.AutoComplete (getH) where

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson qualified as AE
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Models.Apis.Fields.Query (autoCompleteFields)
import Models.Apis.Fields.Types (parseFieldCategoryEnum)
import Models.Apis.RequestDumps (autoCompleteFromRequestDumps)
import Models.Projects.Projects qualified as Projects
import Relude (Applicative (pure), Maybe (..), Text)
import System.Types (ATAuthCtx)


getH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx AE.Value
getH pid fcategory prefix = do
  fields <- dbtToEff do
    case (fcategory, prefix) of
      (Just c, Just p) -> case category of
        Just cat -> autoCompleteFields pid cat p
        Nothing -> autoCompleteFromRequestDumps pid c p
        where
          category = parseFieldCategoryEnum c
      (_, _) -> pure []
  let jsn = toJSON fields
  pure jsn
