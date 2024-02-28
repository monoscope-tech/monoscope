module Pages.GenerateSwagger (generateGetH, generateSwagger) where

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
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields (fieldTypeToText)
import Models.Apis.Fields qualified as Field
import Models.Apis.Fields qualified as Fields
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Formats qualified as Formats
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Relude hiding (ask, asks)
import Relude.Unsafe ((!!))
import Relude.Unsafe qualified as Unsafe
import System.Config
import System.Types
import Utils

data MergedEndpoint = MergedEndpoint
  { urlPath :: Text,
    urlParams :: AE.Value,
    method :: Text,
    host :: Text,
    hash :: Text,
    description :: Text,
    shapes :: V.Vector MergedShapesAndFields
  }
  deriving stock (Show, Generic)

data MergedFieldsAndFormats = MergedFieldsAndFormats
  { field :: Fields.SwField,
    format :: Formats.SwFormat
  }
  deriving stock (Show, Generic)
  deriving anyclass (AE.ToJSON)

data MergedShapesAndFields = MergedShapesAndFields
  { shape :: Shapes.SwShape,
    sField :: Map Fields.FieldCategoryEnum [MergedFieldsAndFormats]
  }
  deriving stock (Show, Generic)

data KeyPathGroup = KeyPathGroup
  { subGoups :: [Text],
    keyPath :: Text
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
       in object
            [ "in" .= String "query",
              "name" .= T.takeWhile (/= '.') (T.dropWhile (== '.') param),
              "description" .= String des,
              "schema" .= object ["type" .= String t, "format" .= ft, "example" .= eg]
            ]
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
          val -> Map.insert root (KeyPathGroup {subGoups = items.subGoups ++ [remainingItems], keyPath = items.keyPath <> "." <> root}) groups
        Nothing -> case remainingItems of
          "" -> Map.insert root (KeyPathGroup {subGoups = [], keyPath = "." <> root}) groups
          val -> Map.insert root (KeyPathGroup {subGoups = [remainingItems], keyPath = "." <> root}) groups
   in updatedGroups

processItems :: [T.Text] -> Map.Map T.Text KeyPathGroup -> Map.Map T.Text KeyPathGroup
processItems [] groups = groups
processItems (x : xs) groups = processItems xs updatedGroups
  where
    updatedGroups = processItem x groups

mergeObjects :: Value -> Value -> Maybe Value
mergeObjects (Object obj1) (Object obj2) = Just $ Object (obj1 <> obj2)
mergeObjects _ _ = Nothing

---------------------------------------------------------

-- >>> import Models.Apis.Endpoints
-- >>> import Models.Apis.Fields
-- >>> import Models.Apis.Formats
-- >>> import Data.Aeson.QQ
-- >>> let items1 = [MergedFieldsAndFormats {field = SwField {fEndpointHash = "f65d8c5d", fKey = "Content-Type[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".Content-Type[*]", fFieldCategory = FCResponseHeader, fHash = "f65d8c5d7c35c08"}, format = SwFormat {swFieldHash = "f65d8c5d7c35c08", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "application/json; charset=utf-8"], swHash = "f65d8c5d7c35c0898dbff6b"}}]
-- >>> let resp = convertKeyPathsToJson [".errors",".message",".status",".timestamp"] items1 "."
-- >>> resp == [aesonQQ|{"schema":{"properties":{"errors":{"description":"","example":"","format":"text","type":"string"},"message":{"description":"","example":"","format":"text","type":"string"},"status":{"description":"","example":"","format":"text","type":"string"},"timestamp":{"description":"","example":"","format":"text","type":"string"}},"type":"object"}}|]
-- True
--
-- >>> let items2 = [MergedFieldsAndFormats {field = SwField {fEndpointHash = "8401409d", fKey = "cache-control[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".cache-control[*]", fFieldCategory = FCResponseHeader, fHash = "8401409d6e4ade5b"}, format = SwFormat {swFieldHash = "8401409d6e4ade5b", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "no-cache, private"], swHash = "8401409d6e4ade5b98dbff6b"}},MergedFieldsAndFormats {field = SwField {fEndpointHash = "8401409d", fKey = "content-type[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".content-type[*]", fFieldCategory = FCResponseHeader, fHash = "8401409d9db3dac0"}, format = SwFormat {swFieldHash = "8401409d9db3dac0", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "application/json",String "application/json; charset=utf-8"], swHash = "8401409d9db3dac098dbff6b"}},MergedFieldsAndFormats {field = SwField {fEndpointHash = "8401409d", fKey = "date[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".date[*]", fFieldCategory = FCResponseHeader, fHash = "8401409d3bcb4a44"}, format = SwFormat {swFieldHash = "8401409d3bcb4a44", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "[CLIENT_REDACTED]"], swHash = "8401409d3bcb4a4498dbff6b"}},MergedFieldsAndFormats {field = SwField {fEndpointHash = "8401409d", fKey = "vary[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".vary[*]", fFieldCategory = FCResponseHeader, fHash = "8401409dbca0c7da"}, format = SwFormat {swFieldHash = "8401409dbca0c7da", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "Accept"], swHash = "8401409dbca0c7da98dbff6b"}},MergedFieldsAndFormats {field = SwField {fEndpointHash = "8401409d", fKey = "x-content-type-options[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".x-content-type-options[*]", fFieldCategory = FCResponseHeader, fHash = "8401409d461f8ef6"}, format = SwFormat {swFieldHash = "8401409d461f8ef6", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "nosniff"], swHash = "8401409d461f8ef698dbff6b"}},MergedFieldsAndFormats {field = SwField {fEndpointHash = "8401409d", fKey = "x-frame-options[*]", fFieldType = FTString, fFormat = "text", fDescription = "", fKeyPath = ".x-frame-options[*]", fFieldCategory = FCResponseHeader, fHash = "8401409d86002f95"}, format = SwFormat {swFieldHash = "8401409d86002f95", swFieldType = FTString, swFieldFormat = "text", swExamples = [String "deny"], swHash = "8401409d86002f9598dbff6b"}}]
-- >>> let resp = convertKeyPathsToJson [""] items2 ""
-- >>> resp == [aesonQQ| |]
--
convertKeyPathsToJson :: [T.Text] -> [MergedFieldsAndFormats] -> Text -> Value
convertKeyPathsToJson items categoryFields parentPath = convertToJson' groups
  where
    -- Debug logs
    -- !() = trace (show items) ()
    -- !() = trace (show categoryFields) ()
    -- !() = trace (show parentPath) ()
    groups = processItems items Map.empty

    safeTail :: T.Text -> T.Text
    safeTail txt = if T.null txt then txt else T.tail txt

    processGroup :: (T.Text, KeyPathGroup) -> Value -> Value
    processGroup (grp, keypath) parsedValue =
      let updatedJson =
            if null keypath.subGoups
              then
                let field = find (\fi -> safeTail (parentPath <> "." <> grp) == fi.field.fKeyPath) categoryFields
                    (desc, t, ft, examples, is_required, is_enum) = extractInfo field categoryFields parentPath grp
                    (key, ob) =
                      if T.isSuffixOf "[*]" grp
                        then
                          ( T.takeWhile (/= '[') grp,
                            object
                              [ "description" .= String desc,
                                "type" .= String "array",
                                "items"
                                  .= ( object
                                         $ [ "type" .= t,
                                             "format" .= ft
                                           ]
                                         ++ if is_enum
                                           then ["enum" .= examples]
                                           else
                                             if V.length examples > 0
                                               then ["example" .= V.head examples]
                                               else
                                                 []
                                                   ++ if is_required then ["required" .= is_required] else []
                                     )
                              ]
                          )
                        else
                          ( grp,
                            object
                              $ [ "description" .= String desc,
                                  "type" .= t,
                                  "format" .= ft
                                ]
                              ++ if is_enum
                                then ["enum" .= examples]
                                else
                                  if V.length examples > 0
                                    then ["example" .= V.head examples]
                                    else
                                      []
                                        ++ if is_required then ["required" .= is_required] else []
                          )
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
    convertToJson' :: Map.Map T.Text KeyPathGroup -> Value
    convertToJson' grps = foldr processGroup (object []) (Map.toList grps)

-- Helper function to determine type and values
extractInfo :: Maybe MergedFieldsAndFormats -> [MergedFieldsAndFormats] -> Text -> Text -> (Text, Text, Text, Vector AE.Value, Bool, Bool)
extractInfo Nothing categoryFields parentPath grp =
  let newK = T.replace "[*]" ".[]" (T.tail parentPath <> "." <> grp)
      newF = find (\fi -> newK == fi.field.fKeyPath) categoryFields
      ob = case newF of
        Just f ->
          if fieldTypeToText f.format.swFieldType == "bool"
            then (f.field.fDescription, "boolean", f.field.fFormat, f.format.swExamples, f.field.fIsRequired, f.field.fIsEnum)
            else (f.field.fDescription, fieldTypeToText f.format.swFieldType, f.field.fFormat, f.format.swExamples, f.field.fIsRequired, f.field.fIsEnum)
        Nothing -> ("", "string", "text", [], False, False)
   in ob
extractInfo (Just f) _ _ _
  | fieldTypeToText f.format.swFieldType == "bool" = (f.field.fDescription, "boolean", f.field.fFormat, f.format.swExamples, f.field.fIsRequired, f.field.fIsEnum)
  | otherwise = (f.field.fDescription, fieldTypeToText f.format.swFieldType, f.field.fFormat, f.format.swExamples, f.field.fIsRequired, f.field.fIsEnum)

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
            { urlPath = path,
              urlParams = endpoint.urlParams,
              method = endpoint.method,
              host = endpoint.host,
              hash = endpointHash,
              shapes = matchingShapes,
              description = endpoint.description
            }

findMatchingFormat :: Fields.SwField -> V.Vector Formats.SwFormat -> MergedFieldsAndFormats
findMatchingFormat field formats =
  let fieldHash = field.fHash
      matchingFormat =
        fromMaybe
          Formats.SwFormat
            { swFieldHash = fieldHash,
              swFieldFormat = "Text",
              swFieldType = Fields.FTString,
              swHash = "",
              swExamples = []
            }
          (V.find (\format -> fieldHash == format.swFieldHash) formats)
   in MergedFieldsAndFormats
        { field = field,
          format = matchingFormat
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
        { shape = shape,
          sField = groupedMap
        }

-- For Servers part of the swagger
getUniqueHosts :: Vector Endpoints.SwEndpoint -> Vector Value
getUniqueHosts endpoints = V.fromList $ map (\h -> object ["url" .= String h]) $ sortNub $ concatMap (\endpoint -> [endpoint.host]) endpoints

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
               in AEKey.fromText (T.toLower $ method mergedEndpoint)
                    .= object
                      [ "description" .= description mergedEndpoint,
                        "parameters" .= qParams,
                        "responses" .= groupShapesByStatusCode (shapes mergedEndpoint),
                        "requestBody" .= object ["description" .= rqS.shape.swRequestDescription, "content" .= object ["application/json" .= rqProps]]
                      ]
            (Just rqS, Nothing) ->
              let rqProps = convertKeyPathsToJson (V.toList rqS.shape.swRequestBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCRequestBody rqS.sField)) ""
               in AEKey.fromText (T.toLower $ method mergedEndpoint)
                    .= object
                      [ "description" .= description mergedEndpoint,
                        "responses" .= groupShapesByStatusCode (shapes mergedEndpoint),
                        "requestBody" .= object ["description" .= rqS.shape.swRequestDescription, "content" .= object ["application/json" .= rqProps]]
                      ]
            (Nothing, Just qS) ->
              let qParams = convertQueryParamsToJSON (V.toList qS.shape.swQueryParamsKeypaths) (fromMaybe [] (Map.lookup Field.FCQueryParam qS.sField))
               in AEKey.fromText (T.toLower $ method mergedEndpoint)
                    .= object
                      [ "description" .= description mergedEndpoint,
                        "parameters" .= qParams,
                        "responses" .= groupShapesByStatusCode (shapes mergedEndpoint)
                      ]
            (_, _) ->
              AEKey.fromText (T.toLower $ method mergedEndpoint)
                .= object
                  [ "description" .= description mergedEndpoint,
                    "responses" .= groupShapesByStatusCode (shapes mergedEndpoint)
                  ]
       in endPointJSON

groupShapesByStatusCode :: V.Vector MergedShapesAndFields -> AE.Value
groupShapesByStatusCode shapes =
  object $ constructStatusCodeEntry (V.toList shapes)

constructStatusCodeEntry :: [MergedShapesAndFields] -> [AET.Pair]
constructStatusCodeEntry =
  map mapFunc

mapFunc :: MergedShapesAndFields -> AET.Pair
mapFunc mShape =
  let content = object ["application/json" .= convertKeyPathsToJson (V.toList mShape.shape.swResponseBodyKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseBody mShape.sField)) ""]
      headers = object ["content" .= convertKeyPathsToJson (V.toList mShape.shape.swResponseHeadersKeypaths) (fromMaybe [] (Map.lookup Field.FCResponseHeader mShape.sField)) ""]
   in show mShape.shape.swStatusCode .= object ["description" .= mShape.shape.swResponseDescription, "headers" .= headers, "content" .= content]

generateSwagger :: Text -> Text -> Vector Endpoints.SwEndpoint -> Vector Shapes.SwShape -> Vector Fields.SwField -> Vector Formats.SwFormat -> Value
generateSwagger projectTitle projectDescription endpoints shapes fields formats = swagger
  where
    merged = mergeEndpoints endpoints shapes fields formats
    hosts = getUniqueHosts endpoints
    paths = groupEndpointsByUrlPath $ V.toList merged
    info = object ["description" .= String projectDescription, "title" .= String projectTitle, "version" .= String "1.0.0", "termsOfService" .= String "https://apitoolkit.io/terms-and-conditions/"]
    swagger = object ["openapi" .= String "3.0.0", "info" .= info, "servers" .= Array hosts, "paths" .= paths]

generateGetH :: Projects.ProjectId -> ATAuthCtx AE.Value
generateGetH pid = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let envCfg = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ AE.object ["error" .= String "You are not a member of this project"]
    else dbtToEff do
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
