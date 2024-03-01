{-# LANGUAGE TemplateHaskell #-}

module DataSeeding (parseConfigToJson, dataSeedingGetH, dataSeedingPostH, DataSeedingForm) where

import Colog ((<&))
import Data.Aeson qualified as AE
import Data.ByteString.Base64 qualified as B64
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Time (NominalDiffTime, UTCTime, ZonedTime, addUTCTime, diffUTCTime, utc, utcToZonedTime, zonedTimeToUTC)
import Data.Yaml qualified as Yaml
import Database.PostgreSQL.Entity.DBT (withPool)
import Deriving.Aeson qualified as DAE
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (ask, asks)
import Faker
import Faker.Address (fullAddress)
import Faker.Name qualified
import Faker.Vehicle qualified
import Faker.Verbs qualified
import Lucid
import Lucid.Htmx
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import NeatInterpolation (text)
import Optics.TH (makeFieldLabelsNoPrefix)
import Pages.BodyWrapper (BWConfig (..), bodyWrapper)
import Pages.NonMember
import ProcessMessage qualified
import Relude hiding (ask, asks)
import Relude.Unsafe ((!!))
import Relude.Unsafe qualified as Unsafe
import RequestMessages qualified
import System.Config (AuthContext (..), DashboardM, env, logger, pool, projectCache)
import System.Random (RandomGen, getStdGen, randomRs)
import System.Types
import Utils
import Web.FormUrlEncoded (FromForm)


data FieldConfig = FieldConfig
  { name :: Text
  , fieldType :: Text
  , typeGenFormat :: Text
  , children :: [FieldConfig]
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] FieldConfig


makeFieldLabelsNoPrefix ''FieldConfig


data SeedConfig = SeedConfig
  { from :: ZonedTime
  , to :: ZonedTime
  , method :: Text
  , durationTo :: Int
  , durationFrom :: Int
  , statusCodesOneof :: [Int]
  , count :: Int
  , path :: Text
  , pathParams :: [FieldConfig]
  , queryParams :: [FieldConfig]
  , requestHeaders :: [FieldConfig]
  , responseHeaders :: [FieldConfig]
  , requestBody :: [FieldConfig]
  , responseBody :: [FieldConfig]
  }
  deriving stock (Show, Generic)
  deriving (AE.FromJSON) via DAE.CustomJSON '[DAE.OmitNothingFields, DAE.FieldLabelModifier '[DAE.CamelToSnake]] SeedConfig


makeFieldLabelsNoPrefix ''SeedConfig


fieldConfigToField :: FieldConfig -> Fake (Text, AE.Value)
fieldConfigToField fc = do
  val <-
    ( case fc.fieldType of
        "string" ->
          AE.String <$> case fc.typeGenFormat of
            "address" -> Faker.Address.fullAddress
            "name" -> Faker.Name.name
            "first_name" -> Faker.Name.firstName
            "last_name" -> Faker.Name.lastName
            "verbs" -> Faker.Verbs.base
            "vehicles" -> Faker.Vehicle.makes
            _ -> Faker.Name.name
        -- "number" -> AE.Number . flip scientific 0 <$> (randomIO :: IO Integer)
        -- "null" -> AE.Null
        _ -> do
          AE.String <$> Faker.Address.fullAddress
      )
  pure (fc.name, val)


randomTimesBtwToAndFrom :: RandomGen g => UTCTime -> Int -> g -> NominalDiffTime -> [ZonedTime]
randomTimesBtwToAndFrom startTime countToReturn rg maxDiff =
  take countToReturn
    $ utcToZonedTime utc
    . flip addUTCTime startTime
    . realToFrac
    <$> randomRs (0, truncate maxDiff :: Int) rg


parseConfigToRequestMessages :: Projects.ProjectId -> ByteString -> IO (Either Yaml.ParseException [RequestMessages.RequestMessage])
parseConfigToRequestMessages pid input = do
  randGen <- getStdGen
  case (Yaml.decodeEither' input :: Either Yaml.ParseException [SeedConfig]) of
    Left err -> pure $ Left err
    Right cfgs -> do
      let fakerSettings = setRandomGen randGen defaultFakerSettings
      resp <-
        generateWithSettings fakerSettings
          $ cfgs
          & mapM \cfg -> do
            let startTimeUTC = zonedTimeToUTC cfg.from
                maxDiffTime = diffUTCTime (zonedTimeToUTC cfg.to) startTimeUTC
                timestamps = randomTimesBtwToAndFrom startTimeUTC cfg.count randGen maxDiffTime
                durations = take cfg.count $ randomRs (cfg.durationFrom, cfg.durationTo) randGen
                allowedStatusCodes = cfg.statusCodesOneof
                statusCodes = take cfg.count $ map (allowedStatusCodes !!) $ randomRs (0, length allowedStatusCodes - 1) randGen

            zip3 timestamps durations statusCodes & mapM \(timestampV, duration', statusCode') -> do
              let duration = duration'
                  statusCode = statusCode'
                  method = cfg.method
                  urlPath = Just cfg.path
                  rawUrl = cfg.path
                  protoMajor = 1
                  protoMinor = 1
                  referer = "https://google.com"
                  host = Just "https://apitoolkit.io/"
                  projectId = Projects.unProjectId pid
                  timestamp = timestampV
                  sdkType = RequestDumps.GoGin
                  msgId = Nothing
                  parentId = Nothing
                  serviceVersion = Nothing
                  errors = Nothing
                  tags = Nothing

              pathLog <- mapM fieldConfigToField cfg.queryParams
              pathParams <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField cfg.pathParams
              queryParams <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField cfg.queryParams
              requestHeaders <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField cfg.requestHeaders
              responseHeaders <- AE.toJSON . HM.fromList <$> mapM fieldConfigToField cfg.responseHeaders
              responseBody <- B64.encodeBase64 . toStrict . AE.encode <$> mapM fieldConfigToField cfg.responseBody
              requestBody <- B64.encodeBase64 . toStrict . AE.encode <$> mapM fieldConfigToField cfg.responseBody
              pure RequestMessages.RequestMessage{..}
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
  { environment :: Text
  , config :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromForm)


dataSeedingPostH :: Projects.ProjectId -> DataSeedingForm -> ATAuthCtx (Html ())
dataSeedingPostH pid form = do
  pure ""


-- FIXME: commented out as processMessages is in a diff monad as well, and is not used much atm
--   -- TODO: temporary, to work with current logic
--   appCtx <- ask @AuthContext
--   let env = appCtx.config
--   sess' <- Sessions.getSession
--   let sess = Unsafe.fromJust sess'.persistentSession
--   let currUserId = sess.userId

--   isMember <- dbtToEff $ userIsProjectMember sess pid
--   if not isMember
--     then do
--       pure $ userNotMemeberPage sess
--     else do
--       logger <- asks logger
--       projectCache <- asks projectCache
--       project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)

--       respE <- liftIO $ parseConfigToRequestMessages pid (encodeUtf8 $ form.config)
--       case respE of
--         Left err -> liftIO $ logger <& "ERROR processing req message " <> show err >> pure dataSeedingPage
--         Right resp -> do
--           let !seeds = resp & map (\x -> (Just "", x))
--           _ <- liftIO $ ProcessMessage.processMessages' logger env appCtx.pool seeds projectCache
--           pure dataSeedingPage

dataSeedingGetH :: Projects.ProjectId -> ATAuthCtx (Html ())
dataSeedingGetH pid = do
  -- TODO: temporary, to work with current logic
  appCtx <- ask @AuthContext
  let env = appCtx.config
  sess' <- Sessions.getSession
  let sess = Unsafe.fromJust sess'.persistentSession

  isMember <- dbtToEff $ userIsProjectMember sess pid
  if not isMember
    then do
      pure $ userNotMemeberPage sess
    else do
      project <- dbtToEff $ Projects.selectProjectForUser (Sessions.userId sess, pid)

      let bwconf =
            (def :: BWConfig)
              { sessM = Just sess
              , currProject = project
              , pageTitle = "Data Seeding"
              }
      pure $ bodyWrapper bwconf dataSeedingPage


dataSeedingPage :: Html ()
dataSeedingPage = do
  section_ [id_ "mainContent", class_ "h-full overflow-scroll"] do
    section_ [class_ "container mx-auto  px-4 py-10"] do
      div_ [class_ "flex justify-between mb-6"] do
        h2_ [class_ "text-slate-700 text-2xl font-medium"] "Bulk Seed Data via Config"
      form_
        [ class_ "relative space-y-10 px-10 border border-gray-200 py-10  bg-white w-3/4 rounded-3xl"
        , hxTarget_ "#mainContent"
        , hxSwap_ "outerHTML"
        , hxPost_ ""
        , hxVals_
            [text|js: config:editor.getValue() 
            |]
        ]
        do
          div_ [] do
            label_ "Environment to run on"
            select_ [name_ "environment"] do
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
          div_ do
            button_ [type_ "submit", class_ "btn-sm btn-indigo"] "Submit"

    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.14/ace.min.js"] ("" :: Text)
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.14/mode-yaml.min.js"] ("" :: Text)
    script_
      [text|
      var editor = ace.edit("configElement");
    editor.setTheme("ace/theme/monokai");
    editor.session.setMode("ace/mode/yaml");
      |]
