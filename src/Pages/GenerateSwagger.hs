module Pages.GenerateSwagger (generateGetH) where

import Config
import Data.Default (def)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)

import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects

import Models.Apis.Fields.Query qualified as Fields
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

generateGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
generateGetH sess pid = do
  pool <- asks pool
  (project, endpointStats) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      endpointStats <- Endpoints.endpointsByProjectId pid
      let endpoint_hashes = V.map (\enp -> enp.hash) endpointStats
      shapes <- Shapes.shapesByEndpointHash pid endpoint_hashes
      fields <- Fields.fieldsByEndpointHashes pid endpoint_hashes
      let field_hashes = V.map (\field -> field.fHash) fields
      formats <- Formats.formatsByFieldsHashes pid field_hashes
      pure (project, endpointStats)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Endpoints"
          }

  pure $ bodyWrapper bwconf $ endpointList endpointStats pid

endpointList :: Vector Endpoints.SwEndpoint -> Projects.ProjectId -> Html ()
endpointList enps pid = do
  div_ [] $ show enps