module Pages.GenerateSwagger (generateGetH, generateSwagger) where

import Config
import Data.Aeson
import Data.Aeson qualified as AE
import Data.Aeson.Key qualified as AEKey
import Data.Aeson.Types qualified as AET
import Data.List (groupBy)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields (fieldTypeToText)
import Models.Apis.Fields qualified as Field
import Models.Apis.Fields qualified as Fields
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Relude
import Relude.Unsafe ((!!))

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
  , format :: Formats.SwFormat
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

convertQueryParamsToJSON :: [T.Text] -> [MergedFieldsAndFormats] -> Value
convertQueryParamsToJSON params fields = paramsJSON
  where
    mapQParamsFunc param =
      let fieldM = find (\fld -> fld.field.fKeyPath == param) fields
          (des, t, ft, eg) = case fieldM of
            Just f -> (f.field.fDescription, fieldTypeToText f.format.swFieldType, f.field.fFormat, V.head f.format.swExamples)
            Nothing -> ("", "string", "text", "")
       in object ["in" .= String "query", "name" .= T.takeWhile (/= '.') (T.dropWhile (== '.') param), "description" .= String des, "schema" .= object ["type" .= String t, "format" .= ft, "example" .= eg]]
    paramsJSON =
      let ar = V.fromList $ map mapQParamsFunc params
       in Array ar

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
                    (desc, t, ft, eg) = case field of
                      Just f ->
                        if fieldTypeToText f.format.swFieldType == "bool"
                          then (f.field.fDescription, "boolean", f.field.fFormat, V.head f.format.swExamples)
                          else (f.field.fDescription, fieldTypeToText f.format.swFieldType, f.field.fFormat, V.head f.format.swExamples)
                      Nothing ->
                        let newK = T.replace "[*]" ".[]" (T.tail parentPath <> "." <> grp)
                            newF = find (\fi -> newK == fi.field.fKeyPath) categoryFields
                            obj = case newF of
                              Just f ->
                                if fieldTypeToText f.format.swFieldType == "bool"
                                  then (f.field.fDescription, "boolean", f.field.fFormat, V.head f.format.swExamples)
                                  else (f.field.fDescription, fieldTypeToText f.format.swFieldType, f.field.fFormat, V.head f.format.swExamples)
                              Nothing -> ("", "string", "text", "")
                         in obj
                    (key, ob) =
                      if T.isSuffixOf "[*]" grp
                        then (T.takeWhile (/= '[') grp, object ["description" .= String desc, "type" .= String "array", "items" .= object ["type" .= t, "format" .= ft, "example" .= eg]])
                        else (grp, object ["description" .= String desc, "type" .= t, "format" .= ft, "example" .= eg])
                    validKey = if key == "" then "schema" else key
                 in object [AEKey.fromText validKey .= ob]
              else
                let (key, t) = if T.isSuffixOf "[*]" grp then (T.takeWhile (/= '[') grp, "array" :: String) else (grp, "object")
                    validKey = if key == "" then "schema" else key
                    ob =
                      if t == "array"
                        then object [AEKey.fromText validKey .= object ["type" .= String "array", "items" .= object ["type" .= String "object", "properties" .= convertKeyPathsToJson keypath.subGoups categoryFields (parentPath <> "." <> grp)]]]
                        else object [AEKey.fromText validKey .= object ["properties" .= convertKeyPathsToJson keypath.subGoups categoryFields (parentPath <> "." <> grp), "type" .= String "object"]]
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
      matchingFormat =
        fromMaybe
          Formats.SwFormat
            { swFieldHash = fieldHash
            , swFieldFormat = "Text"
            , swFieldType = Fields.FTString
            , swHash = ""
            , swExamples = []
            }
          (V.find (\format -> fieldHash == format.swFieldHash) formats)
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
getUniqueHosts :: Vector Endpoints.SwEndpoint -> Vector Value
getUniqueHosts endpoints = V.fromList $ map (\h -> object ["url" .= String h]) $ sortNub $ concatMap (\endpoint -> V.toList endpoint.hosts) endpoints

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
      AEKey.fromText (if urlPath == "" then "/" else urlPath) .= object (map constructMethodEntry mergedEndpoints)

    constructMethodEntry mergedEndpoint =
      let rqShape = V.find (\shape -> length (V.filter (T.isPrefixOf "") shape.shape.swRequestBodyKeypaths) > 1) mergedEndpoint.shapes
          qShape = V.find (\shape -> length (V.filter (T.isPrefixOf "") shape.shape.swQueryParamsKeypaths) > 1) mergedEndpoint.shapes
          endPointJSON = case (rqShape, qShape) of
            (Just rqS, Just qS) ->
              let rqProps = convertKeyPathsToJson (V.toList rqS.shape.swRequestBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCRequestBody rqS.sField)) ""
                  qParams = convertQueryParamsToJSON (V.toList qS.shape.swQueryParamsKeypaths) (fromMaybe [] (Map.lookup Field.FCQueryParam qS.sField))
               in AEKey.fromText (T.toLower $ method mergedEndpoint) .= object ["parameters" .= qParams, "responses" .= groupShapesByStatusCode (shapes mergedEndpoint), "requestBody" .= object ["content" .= object ["*/*" .= rqProps]]]
            (Just rqS, Nothing) ->
              let rqProps = convertKeyPathsToJson (V.toList rqS.shape.swRequestBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCRequestBody rqS.sField)) ""
               in AEKey.fromText (T.toLower $ method mergedEndpoint) .= object ["responses" .= groupShapesByStatusCode (shapes mergedEndpoint), "requestBody" .= object ["content" .= object ["*/*" .= rqProps]]]
            (Nothing, Just qS) ->
              let qParams = convertQueryParamsToJSON (V.toList qS.shape.swQueryParamsKeypaths) (fromMaybe [] (Map.lookup Field.FCQueryParam qS.sField))
               in AEKey.fromText (T.toLower $ method mergedEndpoint) .= object ["parameters" .= qParams, "responses" .= groupShapesByStatusCode (shapes mergedEndpoint)]
            (_, _) -> AEKey.fromText (T.toLower $ method mergedEndpoint) .= object ["responses" .= groupShapesByStatusCode (shapes mergedEndpoint)]
       in endPointJSON

groupShapesByStatusCode :: V.Vector MergedShapesAndFields -> AE.Value
groupShapesByStatusCode shapes =
  object $ constructStatusCodeEntry (V.toList shapes)

constructStatusCodeEntry :: [MergedShapesAndFields] -> [AET.Pair]
constructStatusCodeEntry =
  map mapFunc

mapFunc :: MergedShapesAndFields -> AET.Pair
mapFunc mShape =
  let content = object ["*/*" .= convertKeyPathsToJson (V.toList mShape.shape.swResponseBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseBody mShape.sField)) ""]
      headers = object ["content" .= convertKeyPathsToJson (V.toList mShape.shape.swResponseHeadersKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseHeader mShape.sField)) ""]
   in show mShape.shape.swStatusCode .= object ["description" .= String "", "headers" .= headers, "content" .= content]

generateSwagger :: Text -> Text -> Vector Endpoints.SwEndpoint -> Vector Shapes.SwShape -> Vector Fields.SwField -> Vector Formats.SwFormat -> Value
generateSwagger projectTitle projectDescription endpoints shapes fields formats = swagger
  where
    merged = mergeEndpoints endpoints shapes fields formats
    hosts = getUniqueHosts endpoints
    paths = groupEndpointsByUrlPath $ V.toList merged
    info = object ["description" .= String projectDescription, "title" .= String projectTitle, "version" .= String "1.0.0", "termsOfService" .= String "https://apitoolkit.io/terms-and-conditions/"]
    swagger = object ["openapi" .= String "3.0.0", "info" .= info, "servers" .= Array hosts, "paths" .= paths]

generateGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM AE.Value
generateGetH sess pid = do
  pool <- asks pool
  liftIO $
    withPool pool $ do
      project <- Projects.selectProjectForUser (Sessions.userId sess, pid)
      endpoints <- Endpoints.endpointsByProjectId pid
      let endpoint_hashes = V.map (.hash) endpoints
      shapes <- Shapes.shapesByEndpointHashes pid endpoint_hashes
      fields <- Fields.fieldsByEndpointHashes pid endpoint_hashes
      let field_hashes = V.map (.fHash) fields
      formats <- Formats.formatsByFieldsHashes pid field_hashes
      let (projectTitle, projectDescription) = case project of
            (Just pr) -> (toText pr.title, toText pr.description)
            Nothing -> ("__APITOOLKIT", "Edit project description")
      let swagger = generateSwagger projectTitle projectDescription endpoints shapes fields formats
      pure swagger
