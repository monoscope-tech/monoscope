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

import Models.Apis.Fields qualified as Field
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

processItem :: T.Text -> Map.Map T.Text KeyPathGroup -> Map.Map T.Text KeyPathGroup
processItem item groups =
  let splitItems = T.splitOn "." item
      c = fromMaybe "" (viaNonEmpty head splitItems)

      tmpRoot = T.dropWhile (== '.') c
      -- newArr checks if it's a new arr identifier
      newArr = T.isSuffixOf "[*]" tmpRoot
      root
        | newArr = tmpRoot
        | length splitItems > 1 && fromMaybe "" (viaNonEmpty head (fromMaybe [] (viaNonEmpty tail splitItems))) == "[]" = tmpRoot <> "[*]"
        | otherwise = tmpRoot
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
          val -> Map.insert root (KeyPathGroup{subGoups = items.subGoups ++ [remainingItems], keyPath = items.keyPath <> "." <> root}) groups
        Nothing -> case remainingItems of
          "" -> Map.insert root (KeyPathGroup{subGoups = [], keyPath = "." <> root}) groups
          val -> Map.insert root (KeyPathGroup{subGoups = [remainingItems], keyPath = "." <> root}) groups
   in updatedGroups

processItems :: [T.Text] -> Map.Map T.Text KeyPathGroup -> Map.Map T.Text KeyPathGroup
processItems [] groups = groups
processItems (x : xs) groups = processItems xs updatedGroups
 where
  updatedGroups = processItem x groups

mergeObjects :: Value -> Value -> Maybe Value
mergeObjects (Object obj1) (Object obj2) = Just $ Object (obj1 <> obj2)
mergeObjects _ _ = Nothing

convertKeyPathsToJson :: [T.Text] -> [MergedFieldsAndFormats] -> Text -> Value
convertKeyPathsToJson items categoryFields parentPath = convertToJson' groups
 where
  groups = processItems items Map.empty
  processGroup :: (T.Text, KeyPathGroup) -> Value -> Value
  processGroup (grp, keypath) parsedValue =
    let updatedJson =
          if null keypath.subGoups
            then
              let field = find (\fi -> T.tail (parentPath <> "." <> grp) == fi.field.fKeyPath) categoryFields
                  (desc, t) = case field of
                    Just f -> (f.field.fDescription, f.field.fFormat)
                    Nothing -> (parentPath <> "." <> grp, "text")
                  ob =
                    if T.isSuffixOf "[*]" grp
                      then object [AEKey.fromText (T.takeWhile (/= '[') grp) .= object ["description" .= String desc, "type" .= String "array", "items" .= object ["type" .= t]]]
                      else object [AEKey.fromText grp .= object ["description" .= String desc, "type" .= String t]]
               in ob
            else
              let (key, t) = if T.isSuffixOf "[*]" grp then (T.takeWhile (/= '[') grp, "array") else (grp, "object")
                  ob =
                    if t == "array"
                      then object [AEKey.fromText key .= object ["type" .= String t, "items" .= object ["type" .= String "object", "properties" .= convertKeyPathsToJson keypath.subGoups categoryFields (parentPath <> "." <> grp)]]]
                      else object [AEKey.fromText key .= object ["properties" .= convertKeyPathsToJson keypath.subGoups categoryFields (parentPath <> "." <> grp), "type" .= String t]]
               in ob
        updateMap = case mergeObjects updatedJson parsedValue of
          Just obj -> obj
          Nothing -> object []
     in updateMap
  objectValue :: Value
  objectValue = object []
  convertToJson' :: Map.Map T.Text KeyPathGroup -> Value
  convertToJson' grps = foldr processGroup objectValue (Map.toList grps)

mergeEndpoints :: V.Vector Endpoints.SwEndpoint -> V.Vector Shapes.SwShape -> V.Vector Fields.SwField -> V.Vector Formats.SwFormat -> V.Vector MergedEndpoint
mergeEndpoints endpoints shapes fields formats = V.map mergeEndpoint endpoints
 where
  mergeEndpoint :: Endpoints.SwEndpoint -> MergedEndpoint
  mergeEndpoint endpoint =
    let endpointHash = endpoint.hash
        matchingFields = V.filter (\field -> field.fEndpointHash == endpointHash) fields
        mergedFieldsAndFormats = V.map (`findMatchingFormat` formats) matchingFields
        filteredShapes = V.filter (\shape -> shape.swEndpointHash == endpointHash) shapes
        matchingShapes = V.map (`findMatchingFields` mergedFieldsAndFormats) filteredShapes

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
    let okShape = case V.find (\shape -> length shape.shape.swRequestBodyKeypaths > 1) mergedEndpoint.shapes of
          (Just s) -> s
          Nothing -> V.head mergedEndpoint.shapes
        rqBodyJson = convertKeyPathsToJson (V.toList okShape.shape.swRequestBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCRequestBody okShape.sField)) ""
     in AEKey.fromText (method mergedEndpoint) .= object ["responses" .= groupShapesByStatusCode (shapes mergedEndpoint), "requestBody" .= rqBodyJson]

groupShapesByStatusCode :: V.Vector MergedShapesAndFields -> AE.Value
groupShapesByStatusCode shapes =
  object $ constructStatusCodeEntry (V.toList shapes)

constructStatusCodeEntry :: [MergedShapesAndFields] -> [AET.Pair]
constructStatusCodeEntry =
  map mapFunc

mapFunc :: MergedShapesAndFields -> AET.Pair
mapFunc mShape =
  let content = object ["*/*" .= convertKeyPathsToJson (V.toList mShape.shape.swResponseBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseBody mShape.sField)) ""]
      headers = convertKeyPathsToJson (V.toList mShape.shape.swResponseHeadersKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseHeader mShape.sField)) ""
   in show mShape.shape.swStatusCode .= object ["description" .= String "", "headers" .= headers, "content" .= content]

-- ( \shape ->
--       show shape.shape.swStatusCode
--         .= object
--           [ "content"
--               .= object ["*/*" .= convertKeyPathsToJson (V.toList shape.shape.swResponseBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseBody shape.sField)) ""]
--           ]
--   )
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
      let merged = mergeEndpoints endpoints shapes fields formats
      let hosts = getUniqueHosts endpoints
      let paths = groupEndpointsByUrlPath $ V.toList merged
      let (projectTitle, projectDescription) = case project of
            (Just pr) -> (pr.title, pr.description)
            Nothing -> ("__APITOOLKIT", "Edit project description")
      let info = object ["description" .= String projectTitle, "title" .= String projectDescription, "version" .= String "1.0.0", "termsOfService" .= String "'https://apitoolkit.io/terms-and-conditions/'"]
      let minimalJson = object ["openapi" .= String "3.0.0", "info" .= info, "servers" .= object ["url" .= hosts], "paths" .= paths, "components" .= object ["schema" .= String "Some schemas"]]
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
