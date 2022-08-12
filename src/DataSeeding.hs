{-# LANGUAGE TemplateHaskell #-}

module DataSeeding (parseConfigToJson, dataSeedingGetH, dataSeedingPostH, DataSeedingForm) where

import Colog ((<&))
import Config (DashboardM, env, logger, pool, projectCache)
import Data.Aeson qualified as AE
import Data.ByteString.Base64 qualified as B64
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Time (NominalDiffTime, UTCTime, ZonedTime, addUTCTime, diffUTCTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Entity.DBT (withPool)
import Deriving.Aeson qualified as DAE
import Faker
import Faker.Address (fullAddress)
import Faker.Name qualified
import Lucid
import Lucid.HTMX
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.Core ((^.))
import Optics.TH (makeFieldLabelsNoPrefix)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import ProcessMessage qualified
import Relude
import Relude.Unsafe ((!!))
import RequestMessages qualified
import System.Random (RandomGen, getStdGen, randomRs)
import Text.RawString.QQ (r)
import Web.FormUrlEncoded (FromForm)

data FieldConfig = FieldConfig
  { name :: Text,
    fieldType :: Text,
    typeGenFormat :: Text,
    children :: [FieldConfig]
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FieldConfig

makeFieldLabelsNoPrefix ''FieldConfig

data SeedConfig = SeedConfig
  { from :: ZonedTime,
    to :: ZonedTime,
    method :: Text,
    durationTo :: Int,
    durationFrom :: Int,
    statusCodesOneof :: [Int],
    count :: Int,
    path :: Text,
    pathParams :: [FieldConfig],
    queryParams :: [FieldConfig],
    requestHeaders :: [FieldConfig],
    responseHeaders :: [FieldConfig],
    requestBody :: [FieldConfig],
    responseBody :: [FieldConfig]
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SeedConfig

makeFieldLabelsNoPrefix ''SeedConfig

fieldConfigToField :: FieldConfig -> Fake (Text, AE.Value)
fieldConfigToField fc = do
  val <-
    ( case fc ^. #fieldType of
        "string" ->
          AE.String <$> case fc ^. #typeGenFormat of
            "address" -> Faker.Address.fullAddress
            "name" -> Faker.Name.name
            "first_name" -> Faker.Name.firstName
            "last_name" -> Faker.Name.lastName
            _ -> Faker.Name.name
        -- "number" -> AE.Number . flip scientific 0 <$> (randomIO :: IO Integer)
        -- "null" -> AE.Null
        _ -> do
          AE.String <$> Faker.Address.fullAddress
      )
  pure (fc ^. #name, val)

randomTimesBtwToAndFrom :: RandomGen g => UTCTime -> Int -> g -> NominalDiffTime -> [ZonedTime]
randomTimesBtwToAndFrom startTime countToReturn rg maxDiff =
  take countToReturn $
    utcToZonedTime utc . flip addUTCTime startTime . realToFrac <$> randomRs (0, truncate maxDiff :: Int) rg

parseConfigToRequestMessages :: Projects.ProjectId -> ByteString -> IO (Either Yaml.ParseException [RequestMessages.RequestMessage])
parseConfigToRequestMessages pid input = do
  randGen <- getStdGen
  case (Yaml.decodeEither' input :: Either Yaml.ParseException [SeedConfig]) of
    Left err -> pure $ Left err
    Right configs -> do
      let fakerSettings = setRandomGen randGen defaultFakerSettings
      resp <-
        generateWithSettings fakerSettings $
          configs & mapM \config -> do
            let startTimeUTC = zonedTimeToUTC (config ^. #from)
            let maxDiffTime = diffUTCTime (zonedTimeToUTC (config ^. #to)) startTimeUTC
            let timestamps = randomTimesBtwToAndFrom startTimeUTC (config ^. #count) randGen maxDiffTime
            let durations = take (config ^. #count) $ randomRs (config ^. #durationFrom, config ^. #durationTo) randGen
            let allowedStatusCodes = config ^. #statusCodesOneof
            let statusCodes = take (config ^. #count) $ map (allowedStatusCodes !!) $ randomRs (0, length allowedStatusCodes -1) randGen

            zip3 timestamps durations statusCodes & mapM \(timestampV, duration', statusCode') -> do
              let duration = duration'
              let statusCode = statusCode'
              let method = config ^. #method
              let urlPath = config ^. #path
              let rawUrl = config ^. #path
              let protoMajor = 1
              let protoMinor = 1
              let referer = "https://google.com"
              let host = "https://apitoolkit.io/"
              let projectId = Projects.unProjectId pid
              let timestamp = timestampV
              let sdkType = RequestMessages.GoGin

              pathLog <- mapM fieldConfigToField (config ^. #queryParams)
              pathParams <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField (config ^. #pathParams)
              queryParams <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField (config ^. #queryParams)
              requestHeaders <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField (config ^. #requestHeaders)
              responseHeaders <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField (config ^. #responseHeaders)
              responseBody <- B64.encodeBase64 . toStrict . AE.encode <$> mapM fieldConfigToField (config ^. #responseBody)
              requestBody <- B64.encodeBase64 . toStrict . AE.encode <$> mapM fieldConfigToField (config ^. #responseBody)
              pure RequestMessages.RequestMessage {..}
      pure $ Right $ concat resp

parseConfigToJson :: Projects.ProjectId -> ByteString -> IO (Either Yaml.ParseException [ByteString])
parseConfigToJson pid input = do
  respE <- parseConfigToRequestMessages pid input
  case respE of
    Left err -> pure $ Left err
    Right resp -> do
      pure $ Right $ map (toStrict . AE.encode) resp

--------------------------------------------------------------------------------------------------------

data DataSeedingForm = DataSeedingForm
  { environment :: Text,
    config :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)

dataSeedingPostH :: Sessions.PersistentSession -> Projects.ProjectId -> DataSeedingForm -> DashboardM (Html ())
dataSeedingPostH sess pid form = do
  pool <- asks pool
  logger <- asks logger
  env <- asks env
  projectCache <- asks projectCache
  project <-
    liftIO $
      withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)

  respE <- liftIO $ parseConfigToRequestMessages pid (encodeUtf8 $ config form)
  case respE of
    Left err -> liftIO $ logger <& "ERROR processing req message " <> show err >> pure dataSeedingPage
    Right resp -> do
      let !seeds = resp & map (\x -> Right (Just "", x))
      _ <- liftIO $ ProcessMessage.processMessages' logger env pool seeds projectCache
      pure dataSeedingPage

dataSeedingGetH :: Sessions.PersistentSession -> Projects.ProjectId -> DashboardM (Html ())
dataSeedingGetH sess pid = do
  pool <- asks pool
  project <-
    liftIO $
      withPool pool $ Projects.selectProjectForUser (Sessions.userId sess, pid)

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess,
            currProject = project,
            pageTitle = "Data Seeding"
          }
  pure $ bodyWrapper bwconf dataSeedingPage

dataSeedingPage :: Html ()
dataSeedingPage = do
  section_ [id_ "mainContent", class_ "h-full overflow-scroll"] $ do
    section_ [class_ "container mx-auto  px-4 py-10"] $ do
      div_ [class_ "flex justify-between mb-6"] $ do
        h2_ [class_ "text-slate-700 text-2xl font-medium"] "Bulk Seed Data via Config"
      form_
        [ class_ "relative space-y-10 px-10 border border-gray-200 py-10  bg-white w-3/4 rounded-3xl",
          hxTarget_ "#mainContent",
          hxSwap_ "outerHTML",
          hxPost_ "",
          hxVals_
            [r|js: config:editor.getValue() 
            |]
        ]
        $ do
          div_ [] $ do
            label_ "Environment to run on"
            select_ [name_ "environment"] $ do
              option_ "local"
          div_
            [id_ "configElement", name_ "config", class_ "editor w-full border border-gray-200 h-96"]
            $ toHtml
              [text|
- from: 2022-03-01 01:00 +0000
  to: 2022-03-09 01:00 +0000
  count: 10
  method: GET
  duration_to: 500000000
  duration_from: 1000000
  status_codes_oneof: [200,404,503]
  path: /test/path
  path_params: []
  query_params:
    - name: key
      field_type: "string"
      type_gen_format: "address"
      children: []
  request_headers:             
    - name: key
      field_type: "string"
      type_gen_format: "address"
      children: []
  response_headers: 
    - name: key
      field_type: "string"
      type_gen_format: "address"
      children: []
  request_body:
    - name: key
      field_type: "string"
      type_gen_format: "address"
      children: []
  response_body:
    - name: key
      field_type: "string"
      type_gen_format: "address"
      children: []
            |]
          div_ $ do
            button_ [type_ "submit", class_ "btn-sm btn-indigo"] "Submit"

    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.14/ace.min.js"] ("" :: Text)
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.14/mode-yaml.min.js"] ("" :: Text)
    script_
      [text|
      var editor = ace.edit("configElement");
    editor.setTheme("ace/theme/monokai");
    editor.session.setMode("ace/mode/yaml");
      |]
