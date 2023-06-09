module Pages.GenerateSwagger (generateGetH) where

import Config
import Data.Aeson (Value (Object), object, (.=))
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)

import Data.Aeson ()
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as AEKeyM
import Data.Aeson.Types qualified as AET
import Data.HashMap.Strict qualified as HM
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes

-- import Data.Aeson (object, (.=))
import Data.Aeson.Key qualified as AEKey

import Data.Aeson
import Models.Projects.Projects qualified as Projects

import Models.Apis.Fields.Query qualified as Fields
import Models.Users.Sessions qualified as Sessions
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Relude

data MergedEndpoint = MergedEndpoint
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
  , format :: Maybe Formats.SwFormat
  }
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON)

-- Function to generate Swagger schema template from field paths

processItem :: T.Text -> Map.Map T.Text [T.Text] -> Map.Map T.Text [T.Text]
processItem item groups =
  let splitItems = T.splitOn "." item
      c = fromMaybe "" (viaNonEmpty head splitItems)
      root = toText $ dropWhile (== '.') (toString c)
      remainingItems =
        if length splitItems > 1
          then
            if fromMaybe "" (viaNonEmpty head splitItems) == "[*]"
              then T.intercalate "." $ fromMaybe [] (viaNonEmpty tail (fromMaybe [] (viaNonEmpty tail splitItems)))
              else T.intercalate "." $ fromMaybe [] (viaNonEmpty tail splitItems)
          else ""
      updatedGroups = case Map.lookup root groups of
        Just items -> case remainingItems of
          "" -> groups
          val -> Map.insert root (items ++ [remainingItems]) groups
        Nothing -> case remainingItems of
          "" -> Map.insert root [] groups
          val -> Map.insert root [remainingItems] groups
   in updatedGroups

processItems :: [T.Text] -> Map.Map T.Text [T.Text] -> Map.Map T.Text [T.Text]
processItems [] groups = groups
processItems (x : xs) groups = processItems xs updatedGroups
 where
  updatedGroups = processItem x groups

mergeObjects :: Value -> Value -> Maybe Value
mergeObjects (Object obj1) (Object obj2) = Just $ Object (HM.union (Object obj1) (Object obj2))
mergeObjects _ _ = Nothing

-- TODO: fix this function
convertToJson :: [T.Text] -> Value
convertToJson items = convertToJson' groups
 where
  groups = processItems items Map.empty
  processGroup :: (T.Text, [T.Text]) -> Value -> Value
  processGroup (grp, subGroups) parsedValue =
    let updatedJson = if null subGroups then Null else convertToJson subGroups
        ob = object [AEKey.fromText grp .= updatedJson]
        updateMap = case mergeObjects ob parsedValue of
          Just obj -> obj
          Nothing -> object []
     in updateMap
  objectValue :: Value
  objectValue = object []
  convertToJson' :: Map.Map T.Text [T.Text] -> Value
  convertToJson' grps = foldr processGroup objectValue (Map.toList grps)

-----------end

mergeEndpoints :: V.Vector Endpoints.SwEndpoint -> V.Vector Shapes.SwShape -> V.Vector Fields.SwField -> V.Vector Formats.SwFormat -> V.Vector MergedEndpoint
mergeEndpoints endpoints shapes fields formats = V.map mergeEndpoint endpoints
 where
  mergeEndpoint :: Endpoints.SwEndpoint -> MergedEndpoint
  mergeEndpoint endpoint =
    let endpointHash = endpoint.hash
        matchingShapes = V.filter (\shape -> shape.swEndpointHash == endpointHash) shapes
        matchingFields = V.filter (\field -> field.fEndpointHash == endpointHash) fields
        mergedFieldsAndFormats = V.map (`findMatchingFormat` formats) matchingFields
        path = specCompartiblePath endpoint.urlPath
     in MergedEndpoint
          { urlPath = path
          , urlParams = endpoint.urlParams
          , method = endpoint.method
          , hosts = endpoint.hosts
          , hash = endpointHash
          , shapes = matchingShapes
          , fields = mergedFieldsAndFormats
          }

findMatchingFormat :: Fields.SwField -> V.Vector Formats.SwFormat -> MergedFieldsAndFormats
findMatchingFormat field formats =
  let fieldHash = field.fHash
      matchingFormat = V.find (\format -> fieldHash == format.swFieldHash) formats
   in MergedFieldsAndFormats
        { field = field
        , format = matchingFormat
        }

-- For Servers part of the swagger
getUniqueHosts :: Vector Endpoints.SwEndpoint -> [Text]
getUniqueHosts endpoints = sortNub $ concatMap (\endpoint -> V.toList endpoint.hosts) endpoints

-- Make urlPaths openapi compartible
specCompartiblePath :: Text -> Text
specCompartiblePath path = toText $ intercalate "/" modifiedSegments
 where
  segments = T.splitOn "/" path
  modifiedSegments = map (modifySegment . toString) segments
  modifySegment :: String -> String
  modifySegment segment@(x : xs)
    | x == ':' = "{" ++ xs ++ "}"
    | otherwise = segment
  modifySegment "" = ""

groupEndpointsByUrlPath :: [MergedEndpoint] -> AE.Value
groupEndpointsByUrlPath endpoints =
  let grouped = Map.fromListWith (++) [(urlPath ep, [ep]) | ep <- endpoints]
   in object $ map constructUrlPathEntry (Map.toList grouped)
 where
  constructUrlPathEntry (urlPath, mergedEndpoints) =
    AEKey.fromText urlPath .= object (map constructMethodEntry mergedEndpoints)

  constructMethodEntry mergedEndpoint =
    AEKey.fromText (method mergedEndpoint) .= object ["responses" .= groupShapesByStatusCode (shapes mergedEndpoint)]
groupShapesByStatusCode :: V.Vector Shapes.SwShape -> AE.Value
groupShapesByStatusCode shapes =
  object $ constructStatusCodeEntry (V.toList shapes)

constructStatusCodeEntry :: [Shapes.SwShape] -> [AET.Pair]
constructStatusCodeEntry shapes = map (\shape -> show shape.swStatusCode .= object ["content" .= object ["*/*" .= shape]]) shapes

generateGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
generateGetH sess pid = do
  pool <- asks pool
  (project, endpointStats) <- liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      endpoints <- Endpoints.endpointsByProjectId pid
      let endpoint_hashes = V.map (\enp -> enp.hash) endpoints
      shapes <- Shapes.shapesByEndpointHash pid endpoint_hashes
      fields <- Fields.fieldsByEndpointHashes pid endpoint_hashes
      let field_hashes = V.map (\field -> field.fHash) fields
      formats <- Formats.formatsByFieldsHashes pid field_hashes
      let merged = mergeEndpoints endpoints shapes fields formats
      let hosts = getUniqueHosts endpoints
      let paths = groupEndpointsByUrlPath $ V.toList merged
      let minimalJson = object ["info" .= "Information about the server and stuff", "servers" .= object ["url" .= hosts], "paths" .= paths, "components" .= object ["schema" .= "Some schemas"]]
      pure (project, minimalJson)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = project
          , pageTitle = "Endpoints"
          }

  pure $ bodyWrapper bwconf $ endpointList endpointStats pid

endpointList :: AE.Value -> Projects.ProjectId -> Html ()
endpointList enps pid = do
  div_ [] $ toHtml (AE.encode enps)