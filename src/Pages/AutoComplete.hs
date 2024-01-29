module Pages.AutoComplete (getH) where

import Data.Aeson
import Data.Aeson qualified as AE
import Database.PostgreSQL.Entity.DBT (withPool)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Models.Apis.Fields (parseFieldCategoryEnum)
import Models.Apis.Fields.Query (autoCompleteFields)
import Models.Apis.RequestDumps (autoCompleteFromRequestDumps)
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Relude hiding (ask, asks)
import Relude.Unsafe qualified as Unsafe
import System.Config
import System.Types


getH :: Projects.ProjectId -> Maybe Text -> Maybe Text -> ATAuthCtx AE.Value
getH pid fcategory prefix = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

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
