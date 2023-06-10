module Pages.GenerateSwagger (generateGetH) where

import Config
import Data.Aeson qualified as AE
import Data.Aeson.Types qualified as AET
import Data.Default (def)
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Lucid
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields qualified as Fields
import Relude.Unsafe ((!!))

import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes

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
  , shapes :: V.Vector MergedShapesAndFields
  }
  deriving stock (Show, Generic)

data MergedFieldsAndFormats = MergedFieldsAndFormats
  { field :: Fields.SwField
  , format :: Maybe Formats.SwFormat
  }
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON)

data MergedShapesAndFields = MergedShapesAndFields
  { shape :: Shapes.SwShape
  , sField :: Map Fields.FieldCategoryEnum [MergedFieldsAndFormats]
  }
  deriving stock (Show, Generic)

data KeyPathGroup = KeyPathGroup
  { subGoups :: [Text]
  , keyPath :: Text
  }
  deriving stock (Show, Generic)

processItem :: T.Text -> Map.Map T.Text [T.Text] -> Map.Map T.Text [T.Text]
processItem item groups =
  let splitItems = T.splitOn "." item
      c = fromMaybe "" (viaNonEmpty head splitItems)
      tmpRoot = toText $ dropWhile (== '.') (toString c)
      -- newArr checks if it's a new arr identifier
      newArr = T.isSuffixOf "[*]" tmpRoot
      root
        | newArr = tmpRoot
        | length splitItems > 1 && fromMaybe "" (viaNonEmpty head (fromMaybe [] (viaNonEmpty tail splitItems))) == "[]" = tmpRoot <> "[*]"
        | otherwise = ""

      remainingItems
        | newArr = T.intercalate "." $ fromMaybe [] (viaNonEmpty tail splitItems)
        | length splitItems > 1 =
            if fromMaybe "" (viaNonEmpty head (fromMaybe [] (viaNonEmpty tail splitItems))) == "[]"
              then T.intercalate "." $ fromMaybe [] (viaNonEmpty tail (fromMaybe [] (viaNonEmpty tail splitItems)))
              else T.intercalate "." $ fromMaybe [] (viaNonEmpty tail splitItems)
        | otherwise = ""

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
mergeObjects (Object obj1) (Object obj2) = Just $ Object (obj1 <> obj2)
mergeObjects _ _ = Nothing

convertToJson :: [T.Text] -> Value
convertToJson items = convertToJson' groups
 where
  groups = processItems items Map.empty
  processGroup :: (T.Text, [T.Text]) -> Value -> Value
  processGroup (grp, subGroups) parsedValue =
    let updatedJson =
          if null subGroups
            then object [AEKey.fromText grp .= object ["description" .= String "", "type" .= String ""]]
            else
              let (key, t) = if T.isSuffixOf "[*]" grp then (T.take (T.length grp - 3) grp, "array") else (grp, "object")
               in object [AEKey.fromText key .= object ["properties" .= convertToJson subGroups, "type" .= String t]]
        updateMap = case mergeObjects updatedJson parsedValue of
          Just obj -> obj
          Nothing -> object []
     in updateMap
  objectValue :: Value
  objectValue = object []
  convertToJson' :: Map.Map T.Text [T.Text] -> Value
  convertToJson' grps = foldr processGroup objectValue (Map.toList grps)

mergeEndpoints :: V.Vector Endpoints.SwEndpoint -> V.Vector Shapes.SwShape -> V.Vector Fields.SwField -> V.Vector Formats.SwFormat -> V.Vector MergedEndpoint
mergeEndpoints endpoints shapes fields formats = V.map mergeEndpoint endpoints
 where
  mergeEndpoint :: Endpoints.SwEndpoint -> MergedEndpoint
  mergeEndpoint endpoint =
    let endpointHash = endpoint.hash
        matchingFields = V.filter (\field -> field.fEndpointHash == endpointHash) fields
        mergedFieldsAndFormats = V.map (`findMatchingFormat` formats) matchingFields
        matchingShapes = V.map (`findMatchingFields` mergedFieldsAndFormats) shapes

        path = specCompartiblePath endpoint.urlPath
     in MergedEndpoint
          { urlPath = path
          , urlParams = endpoint.urlParams
          , method = endpoint.method
          , hosts = endpoint.hosts
          , hash = endpointHash
          , shapes = matchingShapes
          }

findMatchingFormat :: Fields.SwField -> V.Vector Formats.SwFormat -> MergedFieldsAndFormats
findMatchingFormat field formats =
  let fieldHash = field.fHash
      matchingFormat = V.find (\format -> fieldHash == format.swFieldHash) formats
   in MergedFieldsAndFormats
        { field = field
        , format = matchingFormat
        }

findMatchingFields :: Shapes.SwShape -> V.Vector MergedFieldsAndFormats -> MergedShapesAndFields
findMatchingFields shape fields =
  let fieldHashes = Shapes.swFieldHashes shape
      filteredFields = V.filter (\mfaf -> mfaf.field.fHash `elem` fieldHashes) fields
      fields' = V.toList filteredFields
      fieldGroup = groupBy (\f1 f2 -> f1.field.fFieldCategory == f2.field.fFieldCategory) fields'
      fieldGroupTupple = map (\f -> ((f !! 0).field.fFieldCategory, f)) fieldGroup
      groupedMap = Map.fromList fieldGroupTupple
   in MergedShapesAndFields
        { shape = shape
        , sField = groupedMap
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
groupShapesByStatusCode :: V.Vector MergedShapesAndFields -> AE.Value
groupShapesByStatusCode shapes =
  object $ constructStatusCodeEntry (V.toList shapes)

constructStatusCodeEntry :: [MergedShapesAndFields] -> [AET.Pair]
-- shape
constructStatusCodeEntry shapes = map (\shape -> show shape.shape.swStatusCode .= object ["content" .= "shs"]) shapes

-- constructStatusCodeEntry shapes = map (\shape -> show shape.shape.swStatusCode .= object ["content" .= object ["*/*" .= (convertToJson $ V.toList shape.shape.swResponseBodyKeypaths)]]) shapes

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
      let field_hashes = V.map (\field -> field.fEndpointHash) fields
      formats <- Formats.formatsByFieldsHashes pid field_hashes

      print field_hashes
      print $ V.map (\field -> field.fEndpointHash) fields

      let merged = mergeEndpoints endpoints shapes fields formats
      let hosts = getUniqueHosts endpoints
      let paths = groupEndpointsByUrlPath $ V.toList merged
      let mi = paths
      -- let minimalJson = object ["info" .= String "Information about the server and stuff", "servers" .= object ["url" .= hosts], "paths" .= paths, "components" .= object ["schema" .= String "Some schemas"]]
      pure (project, mi)

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