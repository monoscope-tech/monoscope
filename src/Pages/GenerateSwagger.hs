module Pages.GenerateSwagger (generateGetH) where

import Config
import Data.Default (def)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)

import Lucid
import Models.Apis.GenerateSwagger qualified as GenerateSwagger
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

generateGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
generateGetH sess pid = do
  pool <- asks pool
  (project, endpointStats) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      endpointStats <- GenerateSwagger.endpointsSwaggerData pid
      let endpoint_hashes = V.map (\x -> x.hash) endpointStats
      shape <- GenerateSwagger.shapeSwaggerData pid endpoint_hashes
      fields <- GenerateSwagger.fieldsSwaggerData pid endpoint_hashes
      let field_hashes = V.map (\x -> x.fieldHash) fields
      formats <- GenerateSwagger.formatSwaggerData pid field_hashes
      print formats
      pure (project, endpointStats)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Endpoints"
          }

  pure $ bodyWrapper bwconf $ endpointList endpointStats pid

endpointList :: Vector GenerateSwagger.Endpoint -> Projects.ProjectId -> Html ()
endpointList enps pid = do
  div_ [] $ show enps