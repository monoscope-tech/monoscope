{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ProcessMessage
  ( processMessage,
    valueToFields,
    requestMsgToDumpAndEndpoint,
    processRequestMessage,
    valueToFormatStr,
    valueToFormatNum,
  )
where

import Colog (LogAction)
import qualified Config
import Control.Lens ((^.), (^?), _Just)
import Data.Aeson (Object, Value (Array, Bool, Null, Number, Object, String), decodeStrict, eitherDecode, eitherDecodeStrict)
import Data.Aeson as AE
  ( FromJSON,
    Value (Array, Object),
    decodeStrict,
    eitherDecode,
    eitherDecodeStrict,
  )

import Text.Regex.TDFA ((=~))
import Text.RawString.QQ
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson.Types as AET
import qualified Data.ByteString.Base64 as B64
import Data.Fixed (Pico)
import qualified Data.HashMap.Strict as HM
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Base64 as B64T
import Data.Time (CalendarDiffTime, UTCTime, ZonedTime)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import Data.Time.Clock as Clock
import Data.Time.LocalTime as Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database.PostgreSQL.Entity (Entity (fields, tableName), insert)
import Database.PostgreSQL.Entity.DBT (QueryNature (..), execute, queryOne, query_, withPool)
import Database.PostgreSQL.Entity.Internal.QQ (field)
import Database.PostgreSQL.Entity.Types (Entity (primaryKey), FieldModifiers, GenericEntity, PrimaryKey, TableName)
import qualified Database.PostgreSQL.Entity.Types as PET
import Database.PostgreSQL.Simple (Connection, FromRow, Only (Only), ToRow, query_)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory), runMigration)
import Database.PostgreSQL.Simple.Migration as Migrations ()
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Time (ZonedTimestamp)
import qualified Data.Scientific as Scientific
import Database.PostgreSQL.Simple.TypeInfo.Static ()
import qualified Database.PostgreSQL.Transact as PgT
import Deriving.Aeson as DAE
import GHC.Generics ()
import qualified Network.Google as Google
import qualified Network.Google.Env as Env
import qualified Network.Google.PubSub as PubSub
import qualified Network.Google.Resource.PubSub.Projects.Subscriptions.Pull as PubSubPull
import Network.Wai (Application)
import Network.Wai.Handler.Warp ()
import Numeric.Lens ()
import Relude
import Relude.Unsafe (fromJust)
import qualified Relude.Unsafe as Unsafe
import Servant
  ( Get,
    Handler,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import Types
import Prelude (read)

processMessage :: LogAction IO String -> Config.EnvConfig -> Pool Connection -> PubSub.ReceivedMessage -> IO (Maybe Text)
processMessage logger envConfig conn msg = do
  let rmMsg = msg ^? PubSub.rmMessage . _Just . PubSub.pmData . _Just
  let decodedMsg = B64.decodeBase64 $ Unsafe.fromJust rmMsg
  let jsonByteStr = fromRight "{}" decodedMsg
  let decodedMsg = eitherDecode (fromStrict jsonByteStr) :: Either String RequestMessage
  traceM $ "Decoded message " ++ show decodedMsg
  case decodedMsg of
    Left x -> print x
    Right recMsg -> processRequestMessage conn recMsg

  -- i <- hello conn
  -- traceM $ "Hello " ++ show i
  pure $ msg ^. PubSub.rmAckId

-- | valueToFields takes an aeson object and converts it into a list of paths to
-- each primitive value in the json and the values.
-- >>> valueToFields exJSON
valueToFields :: AE.Value -> [(Text, AE.Value)]
valueToFields value = snd $ valueToFields' value ("", [])
  where
    valueToFields' :: AE.Value -> (Text, [(Text, AE.Value)]) -> (Text, [(Text, AE.Value)])
    valueToFields' (AE.Object v) akk = HM.toList v & foldl' (\(akkT, akkL) (key, val) -> (akkT, snd $ valueToFields' val (akkT <> "." <> key, akkL))) akk
    valueToFields' (AE.Array v) akk = foldl' (\(akkT, akkL) val -> (akkT, snd $ valueToFields' val (akkT <> ".[]", akkL))) akk v
    valueToFields' v (akk, l) = (akk, (akk, v) : l)

fieldsToHash :: [(Text, AE.Value)] -> Text
fieldsToHash = foldl' (\akk tp -> akk <> "," <> fst tp) ""

aeValueToText :: AE.Value -> Text
aeValueToText (AET.String _) = "string"
aeValueToText (AET.Number _) = "number"
aeValueToText AET.Null = "null"
aeValueToText (AET.Bool _) = "bool"
aeValueToText (AET.Object _) = "object"
aeValueToText (AET.Array _) = "array"

valueToFormat :: AE.Value -> Text
valueToFormat (AET.String val) = valueToFormatStr val
valueToFormat (AET.Number val) = valueToFormatNum val
valueToFormat AET.Null = "null"
valueToFormat (AET.Object _) = "object"
valueToFormat (AET.Array _) = "array"

valueToFormatStr :: Text -> Text
valueToFormatStr val
  | val =~ ([r|^[0-9]+$|] :: Text) = "integer"
  | val =~ ([r|^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$|] :: Text) = "float" 
  | val =~ ([r|^(0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])[- /.](19|20)\d\d$|] :: Text) = "mm/dd/yyyy"
  | val =~ ([r|^(0[1-9]|1[012])[- -.](0[1-9]|[12][0-9]|3[01])[- -.](19|20)\d\d$|] :: Text) = "mm-dd-yyyy"
  | val =~ ([r|^(0[1-9]|1[012])[- ..](0[1-9]|[12][0-9]|3[01])[- ..](19|20)\d\d$|] :: Text) = "mm.dd.yyyy"
  | otherwise = "text"

valueToFormatNum :: Scientific.Scientific -> Text
valueToFormatNum val
  | Scientific.isFloating val = "float"
  | Scientific.isInteger val = "integer"
  | otherwise = "unknown"

fieldsToFieldDTO :: Text -> UUID -> (Text, AE.Value) -> (Field, [Text])
fieldsToFieldDTO fieldCategory projectID (keyPath, val) =
  ( Field
      { fdCreatedAt = read "2019-08-31 05:14:37.537084021 UTC",
        fdUpdatedAt = read "2019-08-31 05:14:37.537084021 UTC",
        fdId = UUID.nil,
        fdEndpoint = UUID.nil,
        fdProjectId = projectID,
        fdKey = snd $ T.breakOnEnd "." keyPath,
        fdFieldType = aeValueToText val,
        fdFieldTypeOverride = Just "",
        fdFormat = valueToFormat val,
        fdFormatOverride = Just "",
        fdDescription = "",
        fdKeyPath = fromList $ T.splitOn "," keyPath,
        fdKeyPathStr = keyPath,
        fdFieldCategory = fieldCategory
      },
    []
  )

-- | eitherStrToText helps to convert Either String a to Either Text a,
-- to allow using both of them via do notation in the same monad.
eitherStrToText :: Either String a -> Either Text a
eitherStrToText (Left str) = Left $ toText str
eitherStrToText (Right a) = Right a

requestMsgToDumpAndEndpoint :: RequestMessage -> ZonedTime -> UUID.UUID -> Either Text (RequestDump, Endpoint, [(Field, [Text])])
requestMsgToDumpAndEndpoint rM now dumpID = do
  reqBodyB64 <- B64.decodeBase64 $ encodeUtf8 $ rmRequestBody rM
  reqBody <- eitherStrToText $ eitherDecodeStrict reqBodyB64 :: Either Text AE.Value
  respBodyB64 <- B64.decodeBase64 $ encodeUtf8 $ rmResponseBody rM
  respBody <- eitherStrToText $ eitherDecodeStrict respBodyB64 :: Either Text AE.Value

  let reqBodyFields = valueToFields reqBody
  let respBodyFields = valueToFields respBody

  let reqBodyFieldHashes = fieldsToHash reqBodyFields
  let respBodyFieldHashes = fieldsToHash respBodyFields

  let reqBodyFieldsDTO = reqBodyFields & map (fieldsToFieldDTO "request_body" (rmProjectId rM))
  let respBodyFieldsDTO = respBodyFields & map (fieldsToFieldDTO "response_body" (rmProjectId rM))

  let fieldsDTO = reqBodyFieldsDTO <> respBodyFieldsDTO

  let reqDump =
        RequestDump
          { rdCreatedAt = rmTimestamp rM,
            rdUpdatedAt = now,
            rdId = dumpID,
            rdProjectId = rmProjectId rM,
            rdHost = rmHost rM,
            rdUrlPath = rmUrlPath rM,
            rdMethod = rmMethod rM,
            rdReferer = rmReferer rM,
            rdProtoMajor = rmProtoMajor rM,
            rdProtoMinor = rmProtoMinor rM,
            rdDuration = calendarTimeTime $ secondsToNominalDiffTime $ fromIntegral $ rmDuration rM,
            rdStatusCode = rmStatusCode rM,
            rdRequestHeaders = AET.emptyArray,
            rdResponseHeaders = AET.emptyArray,
            rdRequestBody = reqBody,
            rdResponseBody = respBody
          }
  let endpoint =
        Endpoint
          { enpCreatedAt = rmTimestamp rM,
            enpUpdatedAt = now,
            enpID = dumpID,
            enpProjectId = rmProjectId rM,
            enpUrlPath = rmUrlPath rM,
            enpUrlParams = AET.emptyObject,
            enpMethod = rmMethod rM,
            enpHosts = [rmHost rM],
            enpRequestHashes = [reqBodyFieldHashes],
            enpResponseHashes = [respBodyFieldHashes],
            enpQueryparamHashes = []
          }
  pure (reqDump, endpoint, fieldsDTO)

-- | processRequestMessage would take a request message and
-- process it into formats which can be persisited into the database.
-- it processes the request message into the request dump,
-- the endpoint struct and then the request fields.
processRequestMessage :: Pool Connection -> RequestMessage -> IO ()
processRequestMessage pool requestMsg = do
  recId <- UUIDV4.nextRandom
  timestamp <- Time.getZonedTime
  let dumpAndEndpoint = requestMsgToDumpAndEndpoint requestMsg timestamp recId
  case dumpAndEndpoint of
    Left err -> putTextLn err
    Right dAndE -> do
      let (reqDump, endpoint, fields) = dAndE
      withPool pool $ do
        insert @RequestDump reqDump
        enpResp <- upsertEndpoints endpoint
        case enpResp of
          Nothing -> error "error"
          Just (enpID, _, _) -> do
            upsertFields enpID fields
            pure ()

upsertFields :: UUID.UUID -> [(Field, [Text])] -> PgT.DBT IO [Maybe (Only Text)]
upsertFields endpoint fields = options & mapM (\opt -> PgT.queryOne q opt)
  where
    q =
      [sql|
        select create_field_and_formats(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)::text;
   |]
    options =
      fields
        & map
          ( \(field, examples) ->
              ( fdProjectId field,
                endpoint,
                fdKey field,
                fdFieldType field,
                fdFieldTypeOverride field,
                fdFormat field,
                fdFormatOverride field,
                fdDescription field,
                fdKeyPath field,
                fdKeyPathStr field,
                fdFieldCategory field,
                Vector.fromList examples,
                5 :: Int64 -- max examples size
              )
          )

upsertEndpoints :: Endpoint -> PgT.DBT IO (Maybe (UUID.UUID, Text, Text))
upsertEndpoints endpoint = queryOne Insert q options
  where
    q =
      [sql|  
        INSERT INTO endpoints (project_id, url_path, url_params, method, hosts, request_hashes, response_hashes, queryparam_hashes)
        VALUES(?, ?, ?, ?, ?, ?, ?, ?) 
        ON CONFLICT (project_id, url_path, method) 
        DO 
           UPDATE SET 
            hosts = ARRAY(SELECT DISTINCT e from unnest(endpoints.hosts, excluded.hosts) as e order by e),
            request_hashes = ARRAY(SELECT DISTINCT e from unnest(endpoints.request_hashes, excluded.request_hashes) as e order by e),
            response_hashes = ARRAY(SELECT DISTINCT e from unnest(endpoints.response_hashes, excluded.response_hashes) as e order by e),
            queryparam_hashes = ARRAY(SELECT DISTINCT e from unnest(endpoints.queryparam_hashes, excluded.queryparam_hashes) as e order by e)
        RETURNING id, method, url_path 
      |]
    options =
      ( enpProjectId endpoint,
        enpUrlPath endpoint,
        enpUrlParams endpoint,
        enpMethod endpoint,
        enpHosts endpoint,
        enpRequestHashes endpoint,
        enpResponseHashes endpoint,
        enpQueryparamHashes endpoint
      )
