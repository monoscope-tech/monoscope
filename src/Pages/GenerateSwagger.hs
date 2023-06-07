module Pages.GenerateSwagger (generateGetH) where

import Config
import Data.Default (def)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)

import Data.Aeson qualified as AE
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

data MergedEndpoints = MergedEndpoints
  { urlPath :: Text
  , urlParams :: AE.Value
  , method :: Text
  , hosts :: V.Vector Text
  , hash :: Text
  , shapes :: V.Vector Shapes.SwShape
  , fields :: V.Vector MergedFieldsAndFormats
  }
  deriving stock (Show, Generic)

data MergedFieldsAndFormats = MergedFieldsAndFormats
  { field :: Fields.SwField
  , format :: Maybe Formats.Format
  }
  deriving stock (Show, Generic)

mergeEndpoints :: V.Vector Endpoints.SwEndpoint -> V.Vector Shapes.SwShape -> V.Vector Fields.SwField -> V.Vector Formats.Format -> V.Vector MergedEndpoints
mergeEndpoints endpoints shapes fields formats =
  V.map mergeEndpoint endpoints
 where
  mergeEndpoint :: Endpoints.SwEndpoint -> MergedEndpoints
  mergeEndpoint endpoint =
    let endpointHash = endpoint.hash
        matchingShapes = V.filter (\shape -> shape.swEndpointHash == endpointHash) shapes
        matchingFields = V.filter (\field -> field.fEndpointHash == endpointHash) fields
        mergedFieldsAndFormats = V.map (`findMatchingFormat` formats) matchingFields
     in MergedEndpoints
          { urlPath = endpoint.urlPath
          , urlParams = endpoint.urlParams
          , method = endpoint.method
          , hosts = endpoint.hosts
          , hash = endpointHash
          , shapes = matchingShapes
          , fields = mergedFieldsAndFormats
          }

findMatchingFormat :: Fields.SwField -> V.Vector Formats.Format -> MergedFieldsAndFormats
findMatchingFormat field formats =
  let fieldHash = field.fHash
      matchingFormat = V.find (\format -> fieldHash == format.fieldHash) formats
   in MergedFieldsAndFormats
        { field = field
        , format = matchingFormat
        }

generateGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
generateGetH sess pid = do
  pool <- asks pool
  (project, endpointStats) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      endpoints <- Endpoints.endpointsByProjectId pid
      let endpoint_hashes = V.map (\enp -> enp.hash) endpoints
      print endpoints
      shapes <- Shapes.shapesByEndpointHash pid endpoint_hashes
      fields <- Fields.fieldsByEndpointHashes pid endpoint_hashes
      let field_hashes = V.map (\field -> field.fHash) fields
      formats <- Formats.formatsByFieldsHashes pid field_hashes
      let merged = mergeEndpoints endpoints shapes fields formats
      print merged
      pure (project, endpoints)

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