module Pkg.TestUtils (
  withSetup,
  abort,
  rollback,
  withTestResources,
  fromRightShow,
  TestResources (..),
  testSessionHeader,
  testRequestMsgs,
  TestRequestMessages (..),
  convert,
  runTestBackground,
  runAllBackgroundJobs,
  refreshMaterializedView,
  setBjRunAtInThePast,
) where

import Data.Default (Default (..))
import Data.Vector qualified as V
import Database.PostgreSQL.Simple.Types (Query (Query))
import Effectful.PostgreSQL.Transact.Effect qualified as DB
import Effectful.Time (runTime)

import BackgroundJobs (jobsRunner)
import Control.Exception (bracket_, finally, mask, throwIO)
import Data.Aeson (Result (Error, Success), Value, fromJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Cache (Cache (..), newCache)
import Data.Effectful.UUID (runStaticUUID)
import Data.Effectful.Wreq (runHTTPGolden)
import Data.Either.Extra
import Data.Pool (Pool, defaultPoolConfig, newPool)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, withPool)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL, execute, execute_)
import Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization))
import Database.PostgreSQL.Simple.Migration qualified as Migration
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Transaction (newSavepoint, rollbackToAndReleaseSavepoint)
import Database.PostgreSQL.Transact qualified as PgT
import Database.Postgres.Temp (cacheAction, cacheConfig, toConnectionString, withConfig, withDbCache)
import Database.Postgres.Temp qualified as TmpPostgres
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Log qualified
import NeatInterpolation (text)
import System.Config qualified as Config
import System.Types (ATBackgroundCtx, runBackground)

import Log.Backend.StandardOutput.Bulk qualified as LogBulk
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import OddJobs.Job (Job)
import Relude
import RequestMessages qualified
import Servant qualified
import System.Clock (TimeSpec (TimeSpec))
import System.Config (AuthContext (..), EnvConfig (..))
import System.Directory (getFileSize, listDirectory)
import Web.Auth qualified as Auth
import Web.Cookie (SetCookie)


migrationsDirr :: FilePath
migrationsDirr = "./static/migrations/"


migrate :: TmpPostgres.DB -> IO ()
migrate db = do
  conn <- liftIO $ connectPostgreSQL (TmpPostgres.toConnectionString db)
  initializationRes <- Migration.runMigration conn Migration.defaultOptions MigrationInitialization

  migrationRes <- Migration.runMigration conn Migration.defaultOptions $ MigrationDirectory migrationsDirr
  -- Create a nil user and projects to make subsequent tests easier
  let q =
        [sql| INSERT into users.users (id, first_name, last_name, email) VALUES ('00000000-0000-0000-0000-000000000000', 'FN', 'LN', 'test@apitoolkit.io');
        |]
  _ <- execute conn q ()
  pass


-- Setup function that spins up a database with the db migrations already executed.
-- source: https://jfischoff.github.io/blog/keeping-database-tests-fast.html
withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x
  throwE $ withDbCache $ \dbCache -> do
    let combinedConfig =
          cacheConfig dbCache
            <> TmpPostgres.verboseConfig
            <> mempty
              { TmpPostgres.postgresConfigFile = [("shared_preload_libraries", "'timescaledb'")]
              }
    dirSize <- sum <$> (listDirectory migrationsDirr >>= mapM (getFileSize . (migrationsDirr <>)))
    migratedConfig <- throwE $ cacheAction ("./.tmp/postgres/" <> show dirSize) migrate combinedConfig
    withConfig migratedConfig $ \db ->
      f =<< newPool (defaultPoolConfig (connectPostgreSQL $ toConnectionString db) close 60 10)


-- throw away all db changes that happened within this abort block
abort :: (Connection -> IO a) -> Connection -> IO a
abort f conn =
  bracket_
    (execute_ conn "BEGIN")
    (execute_ conn "ROLLBACK")
    (f conn)


-- can be a nested transaction, and creates a savepoint which then gets auto rolled back,
-- while reusing all the setup and db operations that happened up until that savepoint
rollback :: Connection -> IO a -> IO a
rollback conn actionToRollback = mask $ \restore -> do
  sp <- newSavepoint conn
  restore actionToRollback `finally` rollbackToAndReleaseSavepoint conn sp


-- | `testSessionHeader` would log a user in and automatically generate a session header
-- which can be reused in subsequent tests
testSessionHeader :: MonadIO m => Pool Connection -> m (Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session)
testSessionHeader pool = do
  pSessId <-
    Auth.authorizeUserAndPersist Nothing "firstName" "lastName" "https://placehold.it/500x500" "test@apitoolkit.io"
      & (runStaticUUID $ map (UUID.fromWords 0 0 0) [1 .. 10])
      & runHTTPGolden "./golden/"
      & DB.runDB pool
      & runTime
      & runEff
      & liftIO
  Auth.sessionByID (Just pSessId) "requestID" False
    & runErrorNoCallStack @Servant.ServerError
    & DB.runDB pool
    & runEff
    & liftIO
    <&> fromRightShow


fromRightShow :: Show a => Either a b -> b
fromRightShow (Right b) = b
fromRightShow (Left a) = error $ "Unexpected Left value: " <> show a


runTestBackground :: Config.AuthContext -> ATBackgroundCtx a -> IO a
runTestBackground authCtx action = LogBulk.withBulkStdOutLogger \logger ->
  runBackground logger authCtx $ action


-- New type to hold all our resources
data TestResources = TestResources
  { trPool :: Pool Connection
  , trProjectCache :: Cache Projects.ProjectId Projects.ProjectCache
  , trSessAndHeader :: Servant.Headers '[Servant.Header "Set-Cookie" SetCookie] Sessions.Session
  , trATCtx :: AuthContext
  , trLogger :: Log.Logger
  }


-- Compose withSetup with additional IO actions
withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources f = withSetup $ \pool -> LogBulk.withBulkStdOutLogger \logger -> do
  projectCache <- newCache (Just $ TimeSpec (60 * 60) 0)
  sessAndHeader <- testSessionHeader pool
  let atAuthCtx =
        AuthContext (def @EnvConfig) pool pool projectCache
          $ ( (def :: EnvConfig)
                { apiKeyEncryptionSecretKey = "apitoolkit123456123456apitoolkit"
                , convertkitApiKey = ""
                , convertkitApiSecret = ""
                , requestPubsubTopics = ["apitoolkit-prod-default"]
                , enableBackgroundJobs = True
                }
            )
  f
    TestResources
      { trPool = pool
      , trProjectCache = projectCache
      , trSessAndHeader = sessAndHeader
      , trATCtx = atAuthCtx
      , trLogger = logger
      }


msg1 :: Text -> Value
msg1 timestamp =
  [aesonQQ|{"duration":476434,
            "host":"172.31.29.11",
            "method":"GET",
            "path_params":{},
            "project_id":"00000000-0000-0000-0000-000000000000",
            "proto_minor":1,
            "proto_major":1,"query_params":{},
            "raw_url":"/","referer":"","request_body":"e30=",
            "request_headers":{
              "connection":["upgrade"],"host":["172.31.29.11"],
              "x-real-ip":["172.31.81.1"],"x-forwarded-for":["172.31.81.1"],
              "user-agent":["ELB-HealthChecker/2.0"],"accept-encoding":["gzip, compressed"]},
              "response_body":"V2VsY29tZSB0byBSZXRhaWxsb29w","response_headers":{"x-powered-by":["Express"],
              "vary":["Origin"],"access-control-allow-credentials":["true"],"content-type":["text/html; charset=utf-8"],
              "content-length":["21"],"etag":["W/\"15-2rFUmgZR2gmQik/+S8kDb7KSIZk\""]
            },
            "sdk_type":"JsExpress",
            "status_code":200,
            "timestamp": #{timestamp},
            "url_path":"/","errors":[],"tags":[]} 
      |]


msg2 :: Text -> Value
msg2 timestamp =
  [aesonQQ|{"timestamp": #{timestamp},
            "request_headers":{
                "Accept":["application/json, text/plain, */*"],
                "Accept-Encoding":["gzip, deflate, br"],
                "Accept-Language":["en-US,en;q=0.9"],"Access-Control-Allow-Headers":["Content-Type"],
                "Access-Control-Allow-Origin":["*"],"Authorization":["Bearer null"],"Content-Length":["62"],
                "Content-Type":["application/json"],"Forwarded":["for=\"[2a01:4b00:f65f:0:59b2:efe6:d68b:691c]\";proto=https"],
                "Origin":["https://grovepay-admin-git-selectbox-daemon-team.vercel.app"],
                "Referer":["https://grovepay-admin-git-selectbox-daemon-team.vercel.app/"],
                "Sec-Ch-Ua":["\"Google Chrome\";v=\"117\", \"Not;A=Brand\";v=\"8\", \"Chromium\";v=\"117\""],
                "Sec-Ch-Ua-Mobile":["?0"],"Sec-Ch-Ua-Platform":["\"macOS\""],"Sec-Fetch-Dest":["empty"],
                "Sec-Fetch-Mode":["cors"],"Sec-Fetch-Site":["cross-site"],
                "Traceparent":["00-064f45d39dadb50679db4755b295ff84-e79ae1ef6a4e42c0-00"],
                "User-Agent":["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"],
                "X-Cloud-Trace-Context":["064f45d39dadb50679db4755b295ff84/16688899787571741376"],
                "X-Forwarded-For":["2a01:4b00:f65f:0:59b2:efe6:d68b:691c"],"X-Forwarded-Proto":["https"]},
                "query_params":{},"path_params":{},"response_headers":{"Access-Control-Allow-Credentials":["true"],
                "Access-Control-Allow-Origin":["*"],
                "Access-Control-Expose-Headers":["Content-Length,Authorization,X-Access-Token,X-Refresh-Token,Content-Type"],
                "Content-Type":["application/json; charset=utf-8"]},"method":"POST","sdk_type":"GoGin","host":"api.grovepay.co.uk",
                "raw_url":"/api/v1/user/login","referer":"https://grovepay-admin-git-selectbox-daemon-team.vercel.app/",
            "project_id":"00000000-0000-0000-0000-000000000000","url_path":"/api/v1/user/login",
            "response_body":"eyJlcnJvcnMiOiJjcnlwdG8vYmNyeXB0OiBoYXNoZWRQYXNzd29yZCBpcyBub3QgdGhlIGhhc2ggb2YgdGhlIGdpdmVuIHBhc3N3b3JkIiwibWVzc2FnZSI6ImludmFsaWQgY3JlZGVudGlhbHMiLCJzdGF0dXMiOiJVbnByb2Nlc3NhYmxlIEVudGl0eSIsInRpbWVzdGFtcCI6Ik1vbmRheSwgMTYtT2N0LTIzIDIxOjQ3OjQxIFVUQyJ9",
            "request_body":"eyJwYXNzd29yZCI6IltDTElFTlRfUkVEQUNURURdIiwidXNlcm5hbWUiOiJhZG1pbkBncm92ZXBheS5jby51ayJ9",
            "proto_minor":1,
            "status_code":422,"proto_major":1,"duration":103636077} 
        |]


testRequestMsgs :: TestRequestMessages
testRequestMsgs =
  RequestMessages
    { reqMsg1 = msg1
    , reqMsg2 = msg2
    }


data TestRequestMessages = RequestMessages
  { reqMsg1 :: Text -> Value
  , reqMsg2 :: Text -> Value
  }


convert :: Value -> Maybe RequestMessages.RequestMessage
convert val = case fromJSON val of
  Success p -> Just p
  Error _ -> Nothing


runAllBackgroundJobs :: AuthContext -> IO (V.Vector Job)
runAllBackgroundJobs authCtx = do
  jobs <- withPool authCtx.pool $ getBackgroundJobs
  LogBulk.withBulkStdOutLogger \logger ->
    V.forM_ jobs \job -> jobsRunner logger authCtx job
  pure jobs


getBackgroundJobs :: PgT.DBT IO (V.Vector Job)
getBackgroundJobs = query Select q ()
  where
    q = [sql|SELECT id, created_at, updated_at, run_at, status, payload,last_error, attempts, locked_at, locked_by FROM background_jobs|]


setBjRunAtInThePast :: PgT.DBT IO ()
setBjRunAtInThePast = void $ PgT.execute q ()
  where
    q = [sql|UPDATE background_jobs SET run_at = CURRENT_DATE - INTERVAL '1 day' WHERE status = 'pending'|]


refreshMaterializedView :: Text -> PgT.DBT IO (Int64)
refreshMaterializedView name = PgT.execute (Query $ encodeUtf8 q) ()
  where
    q = [text|REFRESH MATERIALIZED VIEW $name|]
