module Pkg.DrainSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import Data.Text qualified as T
import RequestMessages qualified
import Data.Text (Text)
import Data.Time
import Pkg.Drain
import Relude

-- Helper function to create a test time
testTime :: UTCTime
testTime = UTCTime (fromGregorian 2024 1 1) 0

-- Helper function to create test time with offset
testTimeOffset :: Int -> UTCTime
testTimeOffset seconds = addUTCTime (fromIntegral seconds) testTime



processNewLog :: Text -> Text -> UTCTime -> DrainTree -> DrainTree
processNewLog logId logContent now tree = do
  let tokensVec = V.fromList $ T.words $ RequestMessages.replaceAllFormats $ logContent
      tokenCount = V.length tokensVec
      firstToken = if V.null tokensVec then "" else V.head tokensVec
   in if tokenCount == 0
        then tree -- Skip empty logs
        else updateTreeWithLog tree tokenCount firstToken tokensVec logId logContent now
processBatch :: V.Vector (Text, Text) -> UTCTime -> DrainTree -> DrainTree
processBatch logBatch now initialTree = do
  V.foldl (\tree (logId, logContent) -> processNewLog logId logContent now tree) initialTree logBatch

spec :: Spec
spec = describe "DRAIN updateTreeWithLog" $ do
  describe "End to End drain tree test" $ do
    it "should get correct log pattern for HTTP requests" $ do
      let initialTree = emptyDrainTree
          updatedTree = processBatch (V.fromList basicHttpLogs) (testTimeOffset 0) initialTree
          logGroups = getAllLogGroups updatedTree
      length logGroups `shouldBe` 3
      let patterns = V.map (\(p,_) -> p) logGroups 
      V.toList patterns `shouldMatchList` 
        [ "DELETE /api/users/{integer} HTTP/{float} {integer}"
        , "POST /api/users HTTP/{float} {integer}"
        , "GET <*> HTTP/{float} {integer}"
        ]     
      let log1 = V.find (\(tmp, logIds) -> tmp ==  "GET <*> HTTP/{float} {integer}") logGroups
      case log1 of
        Just (_, lg) -> lg `shouldBe` V.fromList ["log5", "log3", "log2", "log1"]
        Nothing -> error "log1 pattern not found"
      let log2 = V.find (\(tmp, logIds) -> tmp ==  "POST /api/users HTTP/{float} {integer}") logGroups
      case log2 of
        Just (_, lg) -> lg `shouldBe` V.fromList ["log4"]
        Nothing -> error "log2 pattern not found"
      let log3 = V.find (\(tmp, logIds) -> tmp ==  "DELETE /api/users/{integer} HTTP/{float} {integer}") logGroups
      case log3 of
        Just (_, lg) -> lg `shouldBe` V.fromList ["log6"]
        Nothing -> error "log3 pattern not found"
      pass

    it "should get correct log patterns for database connection logs" $ do
      let initialTree = emptyDrainTree
          updatedTree = processBatch (V.fromList databaseLogs) (testTimeOffset 0) initialTree
          logGroups = getAllLogGroups updatedTree
      let patterns = V.map (\(p,_) -> p) logGroups 
      V.toList patterns `shouldMatchList`
        [ "Connected to database <*>"
        , "Database query executed in {integer}ms"
        , "Connection pool exhausted max={integer} active={integer}"
        ]
      let log1 = V.find (\(tmp, logIds) -> tmp == "Connected to database <*>") logGroups
      case log1 of
        Just (_, lg) -> lg `shouldBe` V.fromList ["db3", "db2", "db1"]
        Nothing -> error "db1 pattern not found"
      let log2 = V.find (\(tmp, logIds) -> tmp == "Database query executed in {integer}ms") logGroups
      case log2 of
        Just (_, lg) -> lg `shouldBe` V.fromList ["db6", "db5", "db4"]
        Nothing -> error "db2 pattern not found"
      let log3 = V.find (\(tmp, logIds) -> tmp == "Connection pool exhausted max={integer} active={integer}") logGroups
      case log3 of
          Just (_, lg) -> lg `shouldBe` V.fromList ["db8", "db7"]
          Nothing -> error "db3 pattern not found"
      pass

    it "should get correct log patterns for application startup logs" $ do
      let initialTree = emptyDrainTree
          updatedTree = processBatch (V.fromList startupLogs) (testTimeOffset 0) initialTree
          logGroups = getAllLogGroups updatedTree
      length logGroups `shouldBe` 4
      let patterns = V.map (\(p,_) -> p) logGroups 
      V.toList patterns `shouldMatchList`
        [ "Initializing Redis connection <*>"
        , "Application ready to serve requests"
        , "Loading configuration from <*>"
        , "Starting application on port {integer}"
        ]
      let log1 = V.find (\(tmp, logIds) -> tmp == "Starting application on port {integer}") logGroups
      case log1 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["start3", "start2", "start1"]
        Nothing -> error "start1 pattern not found"
      let log2 = V.find (\(tmp, logIds) -> tmp == "Loading configuration from <*>") logGroups
      case log2 of
        Just (_, lg) -> V.toList lg `shouldMatchList`  ["start4", "start5"]
        Nothing -> error "start4 pattern not found"
      let log3 = V.find (\(tmp, logIds) -> tmp == "Initializing Redis connection <*>") logGroups
      case log3 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["start6", "start7"]
        Nothing -> error "start6 pattern not found"
      let log4 = V.find (\(tmp, logIds) -> tmp == "Application ready to serve requests") logGroups
      case log4 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["start9", "start8"]
        Nothing -> error "start8 pattern not found"
      pass

    it "should get correct log patterns for error logs" $ do
      let initialTree = emptyDrainTree
          updatedTree = processBatch (V.fromList errorLogs) (testTimeOffset 0) initialTree
          logGroups = getAllLogGroups updatedTree
      length logGroups `shouldBe` 4
      let patterns = V.map (\(p,_) -> p) logGroups 
      V.toList patterns `shouldMatchList`
        [ "ERROR Failed to authenticate user {email}"
        , "ERROR Database connection timeout after {integer}ms"
        , "WARN Retrying failed request attempt {integer} of {integer}"
        , "FATAL Out of memory heap size {integer}MB exceeded"
        ]
      let log1 = V.find (\(tmp, logIds) -> tmp == "WARN Retrying failed request attempt {integer} of {integer}") logGroups
      case log1 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["err7", "err6", "err5"]
        Nothing -> error "err1 pattern not found"
      let log2 = V.find (\(tmp, logIds) -> tmp == "ERROR Failed to authenticate user {email}") logGroups
      case log2 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["err2", "err1"]
        Nothing -> error "err2 pattern not found"
      let log3 = V.find (\(tmp, logIds) -> tmp == "ERROR Database connection timeout after {integer}ms") logGroups
      case log3 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["err4", "err3"]
        Nothing -> error "err3 pattern not found"
      let log4 = V.find (\(tmp, logIds) -> tmp == "FATAL Out of memory heap size {integer}MB exceeded") logGroups
      case log4 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["err9", "err8"]
        Nothing -> error "pattern not found"
      pass
    
    it "should get correct log patterns for timestamped logs" $ do
      let initialTree = emptyDrainTree
          updatedTree = processBatch (V.fromList timestampedLogs) (testTimeOffset 0) initialTree
          logGroups = getAllLogGroups updatedTree
      let patterns = V.map (\(p,_) -> p) logGroups
      V.toList patterns `shouldMatchList`
        [  "{YYYY-MM-DDThh:mm:ss.sTZD} INFO User <*> <*> <*>"
        , "{YYYY-MM-DDThh:mm:ss.sTZD} ERROR Invalid token provided <*>"
        , "{YYYY-MM-DDThh:mm:ss.sTZD} WARN Rate limit exceeded client={ipv4}"
        ]
      let log1 = V.find (\(tmp, logIds) -> tmp == "{YYYY-MM-DDThh:mm:ss.sTZD} INFO User <*> <*> <*>") logGroups
      case log1 of
        Just (_, lg) -> do 
          "ts1" `V.elem` lg `shouldBe` True
          "ts2" `V.elem` lg `shouldBe` True
        Nothing -> error "ts1 pattern not found"
      let log3 = V.find (\(tmp, logIds) -> tmp == "{YYYY-MM-DDThh:mm:ss.sTZD} ERROR Invalid token provided <*>") logGroups
      case log3 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["ts5", "ts6"]
        Nothing -> error "ts5 pattern not found"
      let log4 = V.find (\(tmp, logIds) -> tmp == "{YYYY-MM-DDThh:mm:ss.sTZD} WARN Rate limit exceeded client={ipv4}") logGroups
      case log4 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["ts7", "ts8"]
        Nothing -> error "ts7 pattern not found"

    it "should get correct log patterns for microservice logs" $ do
      let initialTree = emptyDrainTree
          updatedTree = processBatch (V.fromList microserviceLogs) (testTimeOffset 0) initialTree
          logGroups = getAllLogGroups updatedTree
      let patterns = V.map (\(p,_) -> p) logGroups
      V.toList patterns `shouldMatchList`
        [ "payment-service processing payment amount={float} <*>"
        , "auth-service JWT validation successful for user <*>"
        , "user-service database query SELECT * FROM users WHERE id={integer} took {integer}ms"
        , "user-service received request <*> <*> <*>"
        ]
      let log1 = V.find (\(tmp, logIds) -> tmp == "user-service received request <*> <*> <*>" ) logGroups
      case log1 of
        Just (_, lg) ->  V.toList lg `shouldMatchList` ["svc2", "svc1"]
        Nothing -> error "svc1 pattern not found"
      let log3 = V.find (\(tmp, logIds) -> tmp == "user-service database query SELECT * FROM users WHERE id={integer} took {integer}ms") logGroups
      case log3 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["svc4", "svc5"]
        Nothing -> error "svc4 pattern not found"
      let log4 = V.find (\(tmp, logIds) -> tmp == "auth-service JWT validation successful for user <*>") logGroups
      case log4 of
        Just (_, lg) -> V.toList lg `shouldMatchList` ["svc7", "svc8"]
        Nothing -> error "svc7 pattern not found"

basicHttpLogs :: [(Text, Text)]
basicHttpLogs = 
  [ ("log1", "GET /api/users/123 HTTP/1.1 200")
  , ("log2", "GET /api/users/456 HTTP/1.1 200") 
  , ("log3", "GET /api/users/789 HTTP/1.1 200")
  , ("log4", "POST /api/users HTTP/1.1 201")
  , ("log5", "GET /api/orders/abc HTTP/1.1 200")
  , ("log6", "DELETE /api/users/123 HTTP/1.1 204")
  ]

databaseLogs :: [(Text, Text)]
databaseLogs = 
  [ ("db1", "Connected to database postgres://localhost:5432/app")
  , ("db2", "Connected to database postgres://localhost:5432/test")
  , ("db3", "Connected to database postgres://prod-db:5432/app")
  , ("db4", "Database query executed in 145ms")
  , ("db5", "Database query executed in 67ms") 
  , ("db6", "Database query executed in 203ms")
  , ("db7", "Connection pool exhausted max=10 active=10")
  , ("db8", "Connection pool exhausted max=20 active=20")
  ]

startupLogs :: [(Text, Text)]
startupLogs =
  [ ("start1", "Starting application on port 8080")
  , ("start2", "Starting application on port 8081")
  , ("start3", "Starting application on port 3000")
  , ("start4", "Loading configuration from /etc/app/config.json")
  , ("start5", "Loading configuration from /opt/app/settings.yaml")
  , ("start6", "Initializing Redis connection redis://localhost:6379")
  , ("start7", "Initializing Redis connection redis://cache-server:6379")
  , ("start8", "Application ready to serve requests")
  , ("start9", "Application ready to serve requests")
  ]

errorLogs :: [(Text, Text)]
errorLogs = 
  [ ("err1", "ERROR Failed to authenticate user john.doe@example.com")
  , ("err2", "ERROR Failed to authenticate user jane.smith@company.org")  
  , ("err3", "ERROR Database connection timeout after 5000ms")
  , ("err4", "ERROR Database connection timeout after 10000ms")
  , ("err5", "WARN Retrying failed request attempt 1 of 3")
  , ("err6", "WARN Retrying failed request attempt 2 of 3") 
  , ("err7", "WARN Retrying failed request attempt 3 of 3")
  , ("err8", "FATAL Out of memory heap size 512MB exceeded")
  , ("err9", "FATAL Out of memory heap size 1024MB exceeded")
  ]

timestampedLogs :: [(Text, Text)]
timestampedLogs = 
  [ ("ts1", "2024-01-01T10:00:00Z INFO User login successful userId=12345")
  , ("ts2", "2024-01-01T10:00:05Z INFO User login successful userId=67890")
  , ("ts3", "2024-01-01T10:01:00Z INFO User logout userId=12345 sessionTime=3600s")
  , ("ts4", "2024-01-01T10:01:30Z INFO User logout userId=67890 sessionTime=1800s") 
  , ("ts5", "2024-01-01T10:02:00Z ERROR Invalid token provided token=abc123")
  , ("ts6", "2024-01-01T10:02:15Z ERROR Invalid token provided token=xyz789")
  , ("ts7", "2024-01-01T10:03:00Z WARN Rate limit exceeded client=192.168.1.100")
  , ("ts8", "2024-01-01T10:03:10Z WARN Rate limit exceeded client=10.0.0.50")
  ]

microserviceLogs :: [(Text, Text)]
microserviceLogs = 
  [ ("svc1", "user-service received request GET /users/profile traceId=abc123")
  , ("svc2", "user-service received request POST /users/create traceId=def456")
  , ("svc4", "user-service database query SELECT * FROM users WHERE id=123 took 45ms")
  , ("svc5", "user-service database query SELECT * FROM users WHERE id=456 took 67ms")
  , ("svc7", "auth-service JWT validation successful for user john.doe")
  , ("svc8", "auth-service JWT validation successful for user jane.smith")
  , ("svc9", "payment-service processing payment amount=99.99 currency=USD")
  , ("svc10", "payment-service processing payment amount=149.50 currency=EUR")
  ]







