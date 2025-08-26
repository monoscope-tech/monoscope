{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Pages.Anomalies.AnomalyList (
  anomalyListGetH,
  anomalyBulkActionsPostH,
  escapedQueryPartial,
  acknowlegeAnomalyGetH,
  unAcknowlegeAnomalyGetH,
  archiveAnomalyGetH,
  unArchiveAnomalyGetH,
  anomalyListSlider,
  AnomalyBulkForm (..),
  AnomalyListGet (..),
  anomalyAcknowlegeButton,
  anomalyArchiveButton,
  AnomalyAction (..),
  IssueVM (..),
)
where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, utc, utcToZonedTime, utctDayTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (execute)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.PostgreSQL.Transact.Effect (dbtToEff)
import Effectful.Reader.Static (ask)
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Htmx (hxGet_, hxSwap_, hxTarget_, hxTrigger_)
import Lucid.Hyperscript (__)
import Models.Apis.Anomalies (FieldChange (..), PayloadChange (..))
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users (User (id))
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pkg.Components.ItemsList (TabFilter (..), TabFilterOpt (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget qualified as Widget
import Relude hiding (ask)
import Relude.Unsafe qualified as Unsafe
import System.Config (AuthContext (pool))
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast)
import Text.Time.Pretty (prettyTimeAuto)
import Utils (checkFreeTierExceeded, escapedQueryPartial, faSprite_)
import Web.FormUrlEncoded (FromForm)


newtype AnomalyBulkForm = AnomalyBulk
  { anomalyId :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


acknowlegeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyAction)
acknowlegeAnomalyGetH pid aid hostM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let host = fromMaybe "" hostM
  appCtx <- ask @AuthContext
  -- Convert to Issues.IssueId for new system
  let issueId = Issues.IssueId aid.unAnomalyId
  -- Use new Issues acknowledge function
  _ <- dbtToEff $ Issues.acknowledgeIssue issueId sess.user.id
  -- Still use old cascade for compatibility
  let text_id = V.fromList [UUID.toText aid.unAnomalyId]
  v <- dbtToEff $ Anomalies.acknowledgeAnomalies sess.user.id text_id
  _ <- dbtToEff $ Anomalies.acknowlegeCascade sess.user.id v
  _ <- liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.GenSwagger pid sess.user.id host
  addRespHeaders $ Acknowlege pid (Issues.IssueId aid.unAnomalyId) True


unAcknowlegeAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unAcknowlegeAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set acknowledged_by=null, acknowledged_at=null where id=? |]
  let qI = [sql| update apis.issues set acknowledged_by=null, acknowledged_at=null where id=? |]
  _ <- dbtToEff $ execute qI (Only aid)
  _ <- dbtToEff $ execute q (Only aid)
  addRespHeaders $ Acknowlege pid (Issues.IssueId aid.unAnomalyId) False


archiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
archiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=NOW() where id=? |]
  let qI = [sql| update apis.issues set archived_at=NOW() where id=? |]
  _ <- dbtToEff $ execute qI (Only aid)
  _ <- dbtToEff $ execute q (Only aid)
  addRespHeaders $ Archive pid (Issues.IssueId aid.unAnomalyId) True


unArchiveAnomalyGetH :: Projects.ProjectId -> Anomalies.AnomalyId -> ATAuthCtx (RespHeaders AnomalyAction)
unArchiveAnomalyGetH pid aid = do
  (sess, project) <- Sessions.sessionAndProject pid
  let q = [sql| update apis.anomalies set archived_at=null where id=? |]
  let qI = [sql| update apis.issues set archived_at=null where id=? |]
  _ <- dbtToEff $ execute qI (Only aid)
  _ <- dbtToEff $ execute q (Only aid)
  addRespHeaders $ Archive pid (Issues.IssueId aid.unAnomalyId) False


data AnomalyAction
  = Acknowlege Projects.ProjectId Issues.IssueId Bool
  | Archive Projects.ProjectId Issues.IssueId Bool
  | Bulk


instance ToHtml AnomalyAction where
  toHtml (Acknowlege pid aid is_ack) = toHtml $ anomalyAcknowlegeButton pid aid is_ack ""
  toHtml (Archive pid aid is_arch) = toHtml $ anomalyArchiveButton pid aid is_arch
  toHtml Bulk = ""
  toHtmlRaw = toHtml


-- When given a list of anomalyIDs and an action, said action would be applied to the anomalyIDs.
-- Then a notification should be triggered, as well as an action to reload the anomaly List.
anomalyBulkActionsPostH :: Projects.ProjectId -> Text -> AnomalyBulkForm -> ATAuthCtx (RespHeaders AnomalyAction)
anomalyBulkActionsPostH pid action items = do
  (sess, project) <- Sessions.sessionAndProject pid
  appCtx <- ask @AuthContext
  if null items.anomalyId
    then do
      addErrorToast "No items selected" Nothing
      addRespHeaders Bulk
    else do
      _ <- case action of
        "acknowlege" -> do
          v <- dbtToEff $ Anomalies.acknowledgeAnomalies sess.user.id (V.fromList items.anomalyId)
          _ <- dbtToEff $ Anomalies.acknowlegeCascade sess.user.id v
          hosts <- dbtToEff $ Endpoints.getEndpointsByAnomalyTargetHash pid v
          forM_ hosts \h -> do
            _ <- liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" do BackgroundJobs.GenSwagger pid sess.user.id h.host
            pass
        "archive" -> do
          _ <- dbtToEff $ execute [sql| update apis.anomalies set archived_at=NOW() where id=ANY(?::uuid[]) |] (Only $ V.fromList items.anomalyId)
          pass
        _ -> error $ "unhandled anomaly bulk action state " <> action
      addSuccessToast (action <> "d items Successfully") Nothing
      addRespHeaders Bulk


anomalyListGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Endpoints.EndpointId
  -> Maybe Text
  -> Maybe Text
  -> ATAuthCtx (RespHeaders AnomalyListGet)
anomalyListGetH pid layoutM filterTM sortM timeFilter pageM loadM endpointM hxRequestM hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  let (ackd, archived, currentFilterTab) = case filterTM of
        Just "Inbox" -> (False, False, "Inbox")
        Just "Acknowleged" -> (True, False, "Acknowleged")
        Just "Archived" -> (False, True, "Archived")
        _ -> (False, False, "Inbox")

  -- let fLimit = 10
  let filterV = fromMaybe "14d" timeFilter

  let pageInt = maybe 0 (Unsafe.read . toString) pageM

  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan
  currTime <- liftIO getCurrentTime

  let fLimit = 10
  -- Toggle between mock data and real database queries
  let useMockData = False -- Set to False to use real database data
  issues <-
    if useMockData
      then do
        -- Mock data for new UI
        let mockCreatedTime = utcToZonedTime utc currTime
        let mockIssue1 =
              Issues.IssueL
                { id = Issues.IssueId $ UUID.fromString "00000000-0000-0000-0000-000000000001" & fromMaybe (error "Invalid UUID")
                , createdAt = mockCreatedTime
                , updatedAt = mockCreatedTime
                , projectId = pid
                , issueType = Issues.APIChange
                , endpointHash = "endpoint123"
                , acknowledgedAt = Nothing
                , acknowledgedBy = Nothing
                , archivedAt = Nothing
                , title = "User Authentication Schema Update"
                , service = "auth-service"
                , critical = True
                , severity = "critical"
                , affectedRequests = 342
                , affectedClients = 342
                , errorRate = Nothing
                , recommendedAction = "Implement graceful fallback for legacy clients and schedule migration timeline"
                , migrationComplexity = "high"
                , issueData =
                    Aeson
                      $ AE.toJSON
                      $ Issues.APIChangeData
                        { endpointMethod = "POST"
                        , endpointPath = "/api/v1/auth/login"
                        , endpointHost = "api.example.com"
                        , anomalyHashes = V.fromList ["hash123", "hash456", "hash789"]
                        , shapeChanges = V.empty -- Would be populated from actual anomalies
                        , formatChanges = V.empty
                        , newFields = V.fromList ["mfa_token", "device_fingerprint", "password"]
                        , deletedFields = V.empty
                        , modifiedFields = V.fromList ["password"]
                        }
                , llmEnhancedAt = Nothing
                , llmEnhancementVersion = Nothing
                , requestPayloads = Aeson [] -- Will be populated below
                , responsePayloads = Aeson [] -- Will be populated below
                , -- Aggregated data
                  eventCount = 8
                , lastSeen = currTime
                }
        -- Mock payload changes for issue 1
        let mockPayloadChanges1 =
              [ Anomalies.PayloadChange
                  { method = Just "POST"
                  , statusCode = Nothing
                  , statusText = Nothing
                  , contentType = "application/json"
                  , changeType = Anomalies.Breaking
                  , description = "Authentication flow updated with enhanced security requirements"
                  , changes =
                      [ Anomalies.FieldChange
                          { fieldName = "password"
                          , changeKind = Anomalies.Modified
                          , breaking = True
                          , path = "credentials.password"
                          , changeDescription = "Minimum length increased, special character requirement added"
                          , oldType = Just "string (min: 6)"
                          , newType = Just "string (min: 12, requires: special char)"
                          , oldValue = Just "{\n  \"minLength\": 6,\n  \"pattern\": null\n}"
                          , newValue = Just "{\n  \"minLength\": 12,\n  \"pattern\": \"^(?=.*[!@#$%^&*])\",\n  \"required\": true\n}"
                          }
                      , Anomalies.FieldChange
                          { fieldName = "mfa_token"
                          , changeKind = Anomalies.Added
                          , breaking = True
                          , path = "credentials.mfa_token"
                          , changeDescription = "Multi-factor authentication token now required for high-privilege accounts"
                          , oldType = Nothing
                          , newType = Just "string (conditional)"
                          , oldValue = Nothing
                          , newValue = Just "{\n  \"type\": \"string\",\n  \"required\": \"conditional\",\n  \"condition\": \"user.role === 'admin'\"\n}"
                          }
                      , Anomalies.FieldChange
                          { fieldName = "device_fingerprint"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "metadata.device_fingerprint"
                          , changeDescription = "Optional device identification for security analytics"
                          , oldType = Nothing
                          , newType = Just "object"
                          , oldValue = Nothing
                          , newValue = Just "{\n  \"browser\": \"string\",\n  \"os\": \"string\",\n  \"screen\": \"string\",\n  \"timezone\": \"string\"\n}"
                          }
                      ]
                  , exampleBefore = "{\n  \"email\": \"user@example.com\",\n  \"password\": \"pass123\",\n  \"remember_me\": true\n}"
                  , exampleAfter = "{\n  \"email\": \"user@example.com\",\n  \"password\": \"SecureP@ss123!\",\n  \"mfa_token\": \"123456\",\n  \"remember_me\": true,\n  \"device_fingerprint\": {\n    \"browser\": \"Chrome/122.0\",\n    \"os\": \"macOS 14.2\",\n    \"screen\": \"1920x1080\",\n    \"timezone\": \"America/New_York\"\n  }\n}"
                  }
              ]
        let mockResponsePayloads1 =
              [ Anomalies.PayloadChange
                  { method = Nothing
                  , statusCode = Just 200
                  , statusText = Just "OK"
                  , contentType = "application/json"
                  , changeType = Anomalies.Incremental
                  , description = "Successful authentication response"
                  , changes =
                      [ Anomalies.FieldChange
                          { fieldName = "access_token"
                          , changeKind = Anomalies.Modified
                          , breaking = False
                          , path = "data.access_token"
                          , changeDescription = "Token format updated to JWT with enhanced claims"
                          , oldType = Just "string (opaque)"
                          , newType = Just "string (JWT)"
                          , oldValue = Just "abc123xyz789..."
                          , newValue = Just "eyJhbGciOiJIUzI1NiIs..."
                          }
                      , Anomalies.FieldChange
                          { fieldName = "session_info"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "data.session_info"
                          , changeDescription = "Enhanced session metadata for client applications"
                          , oldType = Nothing
                          , newType = Just "object"
                          , oldValue = Nothing
                          , newValue = Just "{\n  \"expires_at\": \"2024-12-31T23:59:59Z\",\n  \"permissions\": [\"read\", \"write\"],\n  \"session_id\": \"uuid\"\n}"
                          }
                      ]
                  , exampleBefore = "{\n  \"success\": true,\n  \"data\": {\n    \"access_token\": \"abc123xyz789random\",\n    \"user_id\": \"12345\",\n    \"expires_in\": 3600\n  }\n}"
                  , exampleAfter = "{\n  \"success\": true,\n  \"data\": {\n    \"access_token\": \"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...\",\n    \"user_id\": \"12345\",\n    \"expires_in\": 3600,\n    \"session_info\": {\n      \"expires_at\": \"2024-12-31T23:59:59Z\",\n      \"permissions\": [\"read\", \"write\", \"admin\"],\n      \"session_id\": \"550e8400-e29b-41d4-a716-446655440000\"\n    }\n  }\n}"
                  }
              , Anomalies.PayloadChange
                  { method = Nothing
                  , statusCode = Just 401
                  , statusText = Just "Unauthorized"
                  , contentType = "application/json"
                  , changeType = Anomalies.Breaking
                  , description = "Authentication failed"
                  , changes =
                      [ Anomalies.FieldChange
                          { fieldName = "error_code"
                          , changeKind = Anomalies.Modified
                          , breaking = True
                          , path = "error.code"
                          , changeDescription = "More specific error codes for different failure types"
                          , oldType = Just "string (generic)"
                          , newType = Just "string (specific)"
                          , oldValue = Just "INVALID_CREDENTIALS"
                          , newValue = Just "PASSWORD_TOO_WEAK | MFA_REQUIRED | ACCOUNT_LOCKED"
                          }
                      , Anomalies.FieldChange
                          { fieldName = "retry_after"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "error.retry_after"
                          , changeDescription = "Suggests when client should retry after rate limiting"
                          , oldType = Nothing
                          , newType = Just "number (seconds)"
                          , oldValue = Nothing
                          , newValue = Just "300"
                          }
                      ]
                  , exampleBefore = "{\n  \"success\": false,\n  \"error\": {\n    \"code\": \"INVALID_CREDENTIALS\",\n    \"message\": \"Login failed\"\n  }\n}"
                  , exampleAfter = "{\n  \"success\": false,\n  \"error\": {\n    \"code\": \"MFA_REQUIRED\",\n    \"message\": \"Multi-factor authentication required for this account\",\n    \"retry_after\": 0,\n    \"mfa_methods\": [\"sms\", \"totp\", \"email\"]\n  }\n}"
                  }
              ]
        -- Store payload changes separately as they're not part of IssueL structure
        let mockIssue2 =
              Issues.IssueL
                { id = Issues.IssueId $ UUID.fromString "00000000-0000-0000-0000-000000000004" & fromMaybe (error "Invalid UUID")
                , createdAt = utcToZonedTime utc $ currTime{utctDayTime = utctDayTime currTime - 21600} -- 6 hours ago
                , updatedAt = utcToZonedTime utc $ currTime{utctDayTime = utctDayTime currTime - 21600}
                , projectId = pid
                , issueType = Issues.APIChange
                , endpointHash = "endpoint456"
                , acknowledgedAt = Nothing
                , acknowledgedBy = Nothing
                , archivedAt = Nothing
                , title = "Product Catalog API Enhancement"
                , service = "catalog-service"
                , critical = False
                , severity = "info"
                , affectedRequests = 89
                , affectedClients = 89
                , errorRate = Nothing
                , recommendedAction = "Clients can adopt new fields gradually as they become available"
                , migrationComplexity = "low"
                , issueData =
                    Aeson
                      $ AE.toJSON
                      $ Issues.APIChangeData
                        { endpointMethod = "GET"
                        , endpointPath = "/api/v2/products/{productId}"
                        , endpointHost = "api.example.com"
                        , anomalyHashes = V.fromList ["hash456", "hash457", "hash458"]
                        , shapeChanges = V.empty
                        , formatChanges = V.empty
                        , newFields = V.fromList ["sustainability_score", "sustainability_metrics", "manufacturer_details", "description"]
                        , deletedFields = V.empty
                        , modifiedFields = V.empty
                        }
                , llmEnhancedAt = Nothing
                , llmEnhancementVersion = Nothing
                , requestPayloads = Aeson []
                , responsePayloads = Aeson [] -- Will be populated below
                , eventCount = 12
                , lastSeen = currTime
                }
        let mockPayloadChanges2 =
              [ Anomalies.PayloadChange
                  { method = Nothing
                  , statusCode = Just 200
                  , statusText = Just "OK"
                  , contentType = "application/json"
                  , changeType = Anomalies.Incremental
                  , description = "Product details with enhanced information"
                  , changes =
                      [ Anomalies.FieldChange
                          { fieldName = "sustainability_score"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "data.sustainability.score"
                          , changeDescription = "Environmental impact rating from 1-100"
                          , oldType = Nothing
                          , newType = Just "number (1-100)"
                          , oldValue = Nothing
                          , newValue = Just "{\n  \"type\": \"number\",\n  \"minimum\": 1,\n  \"maximum\": 100,\n  \"description\": \"Higher is better\"\n}"
                          }
                      , Anomalies.FieldChange
                          { fieldName = "sustainability_metrics"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "data.sustainability.metrics"
                          , changeDescription = "Detailed environmental impact breakdown"
                          , oldType = Nothing
                          , newType = Just "object"
                          , oldValue = Nothing
                          , newValue = Just "{\n  \"carbon_footprint\": \"kg CO2\",\n  \"recyclability\": \"percentage\",\n  \"water_usage\": \"liters\",\n  \"certifications\": [\"array of strings\"]\n}"
                          }
                      , Anomalies.FieldChange
                          { fieldName = "manufacturer_details"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "data.manufacturer"
                          , changeDescription = "Enhanced manufacturer information"
                          , oldType = Nothing
                          , newType = Just "object"
                          , oldValue = Nothing
                          , newValue = Just "{\n  \"name\": \"string\",\n  \"location\": \"country_code\",\n  \"certifications\": [\"ISO9001\", \"etc\"],\n  \"contact\": \"object\"\n}"
                          }
                      , Anomalies.FieldChange
                          { fieldName = "description"
                          , changeKind = Anomalies.Modified
                          , breaking = False
                          , path = "data.description"
                          , changeDescription = "Extended maximum length for richer content"
                          , oldType = Just "string (max: 500)"
                          , newType = Just "string (max: 2000)"
                          , oldValue = Just "maxLength: 500"
                          , newValue = Just "maxLength: 2000, supports markdown"
                          }
                      ]
                  , exampleBefore = "{\n  \"success\": true,\n  \"data\": {\n    \"id\": \"prod_123\",\n    \"name\": \"Water Bottle\",\n    \"description\": \"Simple water bottle\",\n    \"price\": 29.99\n  }\n}"
                  , exampleAfter = "{\n  \"success\": true,\n  \"data\": {\n    \"id\": \"prod_123\",\n    \"name\": \"Eco-Friendly Water Bottle\",\n    \"description\": \"# Premium Stainless Steel Water Bottle\\n\\nMade from 100% recycled materials...\",\n    \"price\": 29.99,\n    \"sustainability\": {\n      \"score\": 87,\n      \"metrics\": {\n        \"carbon_footprint\": \"2.3 kg CO2\",\n        \"recyclability\": \"95%\",\n        \"water_usage\": \"15 liters\",\n        \"certifications\": [\"Carbon Neutral\", \"Fair Trade\"]\n      }\n    },\n    \"manufacturer\": {\n      \"name\": \"EcoBottle Co.\",\n      \"location\": \"USA\",\n      \"certifications\": [\"ISO14001\", \"B-Corp\"],\n      \"contact\": {\n        \"website\": \"https://ecobottle.com\",\n        \"support_email\": \"support@ecobottle.com\"\n      }\n    }\n  }\n}"
                  }
              , Anomalies.PayloadChange
                  { method = Nothing
                  , statusCode = Just 404
                  , statusText = Just "Not Found"
                  , contentType = "application/json"
                  , changeType = Anomalies.Incremental
                  , description = "Product not found"
                  , changes =
                      [ Anomalies.FieldChange
                          { fieldName = "suggestions"
                          , changeKind = Anomalies.Added
                          , breaking = False
                          , path = "error.suggestions"
                          , changeDescription = "Suggested alternative products when item is not found"
                          , oldType = Nothing
                          , newType = Just "array of objects"
                          , oldValue = Nothing
                          , newValue = Just "[{\"id\": \"string\", \"name\": \"string\", \"similarity\": \"number\"}]"
                          }
                      ]
                  , exampleBefore = "{\n  \"success\": false,\n  \"error\": {\n    \"code\": \"PRODUCT_NOT_FOUND\",\n    \"message\": \"Product with ID 'prod_123' was not found\"\n  }\n}"
                  , exampleAfter = "{\n  \"success\": false,\n  \"error\": {\n    \"code\": \"PRODUCT_NOT_FOUND\",\n    \"message\": \"Product with ID 'prod_123' was not found\",\n    \"suggestions\": [\n      {\n        \"id\": \"prod_124\",\n        \"name\": \"Similar Eco Water Bottle\",\n        \"similarity\": 0.89\n      },\n      {\n        \"id\": \"prod_125\",\n        \"name\": \"Stainless Steel Bottle Pro\",\n        \"similarity\": 0.76\n      }\n    ]\n  }\n}"
                  }
              ]
        -- Mock issue 3: Payment Processing Error (Runtime Exception)
        let mockIssue3 =
              Issues.IssueL
                { id = Issues.IssueId $ UUID.fromString "00000000-0000-0000-0000-000000000007" & fromMaybe (error "Invalid UUID")
                , createdAt = utcToZonedTime utc $ currTime{utctDayTime = utctDayTime currTime - 3600} -- 1 hour ago
                , updatedAt = utcToZonedTime utc $ currTime{utctDayTime = utctDayTime currTime - 3600}
                , projectId = pid
                , issueType = Issues.RuntimeException
                , endpointHash = ""
                , acknowledgedAt = Nothing
                , acknowledgedBy = Nothing
                , archivedAt = Nothing
                , title = "Payment Processing Error: TypeError"
                , service = "payment-service"
                , critical = True
                , severity = "critical"
                , affectedRequests = 1247
                , affectedClients = 1247
                , errorRate = Nothing
                , recommendedAction = "Immediate hotfix deployed - validate payment.details exists before accessing amount property"
                , migrationComplexity = "n/a"
                , issueData =
                    Aeson
                      $ AE.toJSON
                      $ Issues.RuntimeExceptionData
                        { errorType = "TypeError"
                        , errorMessage = "Cannot read property 'amount' of undefined"
                        , stackTrace = "TypeError: Cannot read property 'amount' of undefined in validatePayment(payment.details.amount) at line 247\n\nStack trace:\n- validatePayment (payment-validator.js:247)\n- processPayment (payment-processor.js:89)\n- POST /api/v1/payments/process (payment-handler.js:34)"
                        , requestPath = Just "/api/v1/payments/process"
                        , requestMethod = Just "POST"
                        , occurrenceCount = 1247
                        , firstSeen = currTime
                        , lastSeen = currTime
                        }
                , llmEnhancedAt = Nothing
                , llmEnhancementVersion = Nothing
                , requestPayloads = Aeson []
                , responsePayloads = Aeson []
                , eventCount = 1247
                , lastSeen = currTime
                }
        -- Mock issue 4: Query Alert
        let mockIssue4 =
              Issues.IssueL
                { id = Issues.IssueId $ UUID.fromString "00000000-0000-0000-0000-000000000009" & fromMaybe (error "Invalid UUID")
                , createdAt = utcToZonedTime utc $ currTime{utctDayTime = utctDayTime currTime - 1800} -- 30 minutes ago
                , updatedAt = utcToZonedTime utc $ currTime{utctDayTime = utctDayTime currTime - 1800}
                , projectId = pid
                , issueType = Issues.QueryAlert
                , endpointHash = ""
                , acknowledgedAt = Nothing
                , acknowledgedBy = Nothing
                , archivedAt = Nothing
                , title = "High Error Rate Alert"
                , service = "Monitoring"
                , critical = False
                , severity = "warning"
                , affectedRequests = 567
                , affectedClients = 567
                , errorRate = Just 5.2
                , recommendedAction = "Investigating database connection pool exhaustion - scaling database read replicas"
                , migrationComplexity = "n/a"
                , issueData =
                    Aeson
                      $ AE.toJSON
                      $ Issues.QueryAlertData
                        { queryId = "monitor-001"
                        , queryName = "High Error Rate Monitor"
                        , queryExpression = "error_rate > 5% OR status_code >= 500 OR (method == \"POST\" AND response_time > 2000ms) within last 15min"
                        , thresholdValue = 5.0
                        , actualValue = 5.2
                        , thresholdType = "above"
                        , triggeredAt = currTime
                        }
                , llmEnhancedAt = Nothing
                , llmEnhancementVersion = Nothing
                , requestPayloads = Aeson []
                , responsePayloads = Aeson []
                , eventCount = 567
                , lastSeen = currTime
                }
        -- Update issues with payload changes
        let mockIssue1' :: Issues.IssueL
            mockIssue1' = mockIssue1{Issues.requestPayloads = Aeson mockPayloadChanges1, Issues.responsePayloads = Aeson mockResponsePayloads1}
        let mockIssue2' :: Issues.IssueL
            mockIssue2' = mockIssue2{Issues.responsePayloads = Aeson mockPayloadChanges2}
        pure $ V.fromList [mockIssue1', mockIssue2', mockIssue3, mockIssue4]
      else -- Fetch real data from database
        dbtToEff $ Issues.selectIssues pid Nothing (Just ackd) (Just archived) fLimit (pageInt * fLimit)

  let currentURL = mconcat ["/p/", pid.toText, "/anomalies?layout=", fromMaybe "false" layoutM, "&ackd=", show ackd, "&archived=", show archived]
      nextFetchUrl = case layoutM of
        Just "slider" -> Nothing
        _ -> Just $ currentURL <> "&load_more=true&page=" <> show (pageInt + 1)
  let listCfg =
        ItemsList.ItemsListCfg
          { projectId = pid
          , nextFetchUrl
          , sort = Just $ ItemsList.SortCfg{current = fromMaybe "events" sortM}
          , filter = timeFilter
          , search = Just $ ItemsList.SearchCfg{viaQueryParam = Nothing} -- FIXME: search actual db
          , heading = Nothing
          , bulkActions =
              [ ItemsList.BulkAction{icon = Just "check", title = "acknowlege", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/acknowlege"}
              , ItemsList.BulkAction{icon = Just "inbox-full", title = "archive", uri = "/p/" <> pid.toText <> "/anomalies/bulk_actions/archive"}
              ]
          , zeroState =
              Just
                $ ItemsList.ZeroState
                  { icon = "empty-set"
                  , title = "No Issues Or Errors."
                  , description = "Start monitoring errors that happened during a request."
                  , actionText = "Error reporting guide"
                  , destination = Right "https://apitoolkit.io/docs/sdks/nodejs/expressjs/#reporting-errors-to-apitoolkit"
                  }
          , elemID = "anomalyListForm"
          , ..
          }
  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Issues: Changes, Alerts & Errors"
          , menuItem = Just "Changes & Errors"
          , freeTierExceeded = freeTierExceeded
          , navTabs =
              Just
                $ toHtml
                $ TabFilter
                  { current = currentFilterTab
                  , currentURL
                  , options =
                      [ TabFilterOpt "Inbox" Nothing
                      , TabFilterOpt "Acknowleged" Nothing
                      , TabFilterOpt "Archived" Nothing
                      ]
                  }
          }
      issuesVM = V.map (IssueVM False currTime filterV) issues
  addRespHeaders $ case (layoutM, hxRequestM, hxBoostedM, loadM) of
    (Just "slider", Just "true", _, _) -> ALSlider currTime pid endpointM (Just $ V.map (IssueVM True currTime filterV) issues)
    (_, _, _, Just "true") -> ALItemsRows $ ItemsList.ItemsRows nextFetchUrl issuesVM
    _ -> ALItemsPage $ PageCtx bwconf (ItemsList.ItemsPage listCfg issuesVM)


data AnomalyListGet
  = ALItemsPage (PageCtx (ItemsList.ItemsPage IssueVM))
  | ALItemsRows (ItemsList.ItemsRows IssueVM)
  | ALSlider UTCTime Projects.ProjectId (Maybe Endpoints.EndpointId) (Maybe (V.Vector IssueVM))


instance ToHtml AnomalyListGet where
  toHtml (ALSlider utcTime pid eid issue) = toHtmlRaw $ anomalyListSlider utcTime pid eid issue
  toHtml (ALItemsPage pg) = toHtml pg
  toHtml (ALItemsRows rows) = toHtml rows
  toHtmlRaw = toHtml


anomalyListSlider :: UTCTime -> Projects.ProjectId -> Maybe Endpoints.EndpointId -> Maybe (V.Vector IssueVM) -> Html ()
anomalyListSlider _ _ _ (Just []) = ""
anomalyListSlider _ pid eid Nothing = do
  div_ [hxGet_ $ "/p/" <> pid.toText <> "/anomalies?layout=slider" <> maybe "" (\x -> "&endpoint=" <> x.toText) eid, hxSwap_ "outerHTML", hxTrigger_ "load"] do
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-textStrong"] "Ongoing Issues and Monitors"
      div_ [class_ "flex flex-row mt-2"] ""
anomalyListSlider currTime _ _ (Just issues) = do
  let anomalyIds = T.replace "\"" "'" $ show $ fmap (Issues.issueIdText . (\(IssueVM _ _ _ issue) -> issue.id)) issues
  let totalAnomaliesTxt = toText $ if length issues > 10 then ("10+" :: Text) else show (length issues)
  div_ do
    script_ [text| var rem = (x,y)=>((x%y)==0?1:(x%y)); |]
    script_
      [type_ "text/hyperscript"]
      [text| init set $$currentAnomaly to 0 then set $$anomalyIds to $anomalyIds
          def setAnomalySliderPag()
            set #anomalySliderPagination.innerHTML to ($$currentAnomaly+1)+'/$totalAnomaliesTxt '
          end |]
    div_ [class_ "flex justify-between mt-5 pb-2"] do
      div_ [class_ "flex flex-row"] do
        a_ [href_ "#", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .parent-slider)|]] $ faSprite_ "chevron-down" "regular" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-textStrong"] "Ongoing Issues and Monitors"
      div_ [class_ "flex items-center gap-2 mt-2"] do
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                          js($currentAnomaly, $anomalyIds) return (Math.max(0, $currentAnomaly-1) % $anomalyIds.length) end then
                          set $currentAnomaly to it then show #{$anomalyIds[$currentAnomaly]} then setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-left" "regular" "h-4 w-4"
        span_ [src_ " mx-4", id_ "anomalySliderPagination"] "1/1"
        a_
          [ class_ "cursor-pointer"
          , [__|on click hide #{$anomalyIds[$currentAnomaly]} then
                js($currentAnomaly, $anomalyIds) return (($currentAnomaly+1) % $anomalyIds.length) end then
                set $currentAnomaly to it then show #{$anomalyIds[$currentAnomaly]} then setAnomalySliderPag()|]
          ]
          $ faSprite_ "arrow-right" "regular" "h-4 w-4"

    div_
      [ class_ "parent-slider"
      , [__|init setAnomalySliderPag() then show #{$anomalyIds[$currentAnomaly]} |]
      ]
      $ mapM_ toHtml issues


-- anomalyAccentColor isAcknowleged isArchived
anomalyAccentColor :: Bool -> Bool -> Text
anomalyAccentColor _ True = "bg-fillStrong"
anomalyAccentColor True False = "bg-fillSuccess-weak"
anomalyAccentColor False False = "bg-fillError-strong"


data IssueVM = IssueVM Bool UTCTime Text Issues.IssueL
  deriving stock (Show)


instance ToHtml IssueVM where
  {-# INLINE toHtml #-}
  toHtml (IssueVM hideByDefault currTime timeFilter issue) = toHtmlRaw $ renderIssue hideByDefault currTime timeFilter issue
  toHtmlRaw = toHtml


renderIssue :: Bool -> UTCTime -> Text -> Issues.IssueL -> Html ()
renderIssue hideByDefault currTime timeFilter issue = do
  let issueId = Issues.issueIdText issue.id
  let timeSinceString = prettyTimeAuto currTime $ zonedTimeToUTC issue.createdAt

  div_ [class_ $ "flex py-4 gap-8 items-start itemsListItem p-6 " <> if hideByDefault then "card-round" else "", style_ (if hideByDefault then "display:none" else "")] do
    -- Checkbox and accent color
    div_ [class_ $ "h-4 flex space-x-3 w-8 items-center justify-center " <> if hideByDefault then "hidden" else ""] do
      a_ [class_ $ anomalyAccentColor (isJust issue.acknowledgedAt) (isJust issue.archivedAt) <> " w-2 h-full"] ""
      input_ [term "aria-label" "Select Issue", class_ "bulkactionItemCheckbox checkbox checkbox-md checked:checkbox-primary", type_ "checkbox", name_ "anomalyId", value_ issueId]

    -- Main section with title, badges, and metadata
    div_ [class_ "flex-1 min-w-0"] do
      -- Title and badges row
      div_ [class_ "flex items-center gap-3 mb-3 flex-wrap"] do
        h3_ [class_ "text-textStrong text-base"] $ toHtml issue.title

        -- Issue type badge
        case issue.issueType of
          Issues.RuntimeException ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-strong text-textInverse-strong shadow-sm"] do
              faSprite_ "triangle-alert" "regular" "w-3 h-3"
              "ERROR"
          Issues.QueryAlert ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-strong text-textInverse-strong shadow-sm"] do
              faSprite_ "zap" "regular" "w-3 h-3"
              "ALERT"
          Issues.APIChange ->
            if issue.critical
              then span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-strong text-textInverse-strong shadow-sm"] do
                faSprite_ "exclamation-triangle" "regular" "w-3 h-3"
                "BREAKING"
              else span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillInformation-strong text-textInverse-strong shadow-sm"] do
                faSprite_ "info" "regular" "w-3 h-3 mr-0.5"
                "Incremental"

        -- Severity badge
        case issue.severity of
          "critical" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
          "warning" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
          _ -> pass

      -- Metadata row (method, endpoint, service, time)
      div_ [class_ "flex items-center gap-4 text-sm text-textWeak mb-3 flex-wrap"] do
        -- Method and endpoint (for API changes)
        when (issue.issueType == Issues.APIChange) do
          case AE.fromJSON (getAeson issue.issueData) of
            AE.Success (apiData :: Issues.APIChangeData) -> do
              div_ [class_ "flex items-center gap-2"] do
                -- Method badge
                let methodClass = case apiData.endpointMethod of
                      "GET" -> "bg-fillInformation-strong"
                      "POST" -> "bg-fillSuccess-strong"
                      "PUT" -> "bg-fillWarning-strong"
                      "DELETE" -> "bg-fillError-strong"
                      _ -> "bg-fillInformation-strong"
                span_ [class_ $ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-transparent " <> methodClass <> " text-textInverse-strong shadow-sm"] do
                  toHtml apiData.endpointMethod
                -- Endpoint path
                span_ [class_ "font-mono bg-fillWeak px-2 py-1 rounded text-xs text-textStrong"] $ toHtml apiData.endpointPath
            _ -> pass

        -- Service badge
        span_ [class_ "flex items-center gap-1"] do
          div_ [class_ "w-3 h-3 bg-fillYellow rounded-sm"] ""
          span_ [class_ "text-textStrong"] $ toHtml issue.service

        -- Time since
        span_ [class_ "text-textWeak"] $ toHtml timeSinceString

      -- Statistics row (only for API changes)
      when (issue.issueType == Issues.APIChange) do
        let requestChanges = getAeson issue.requestPayloads :: [Anomalies.PayloadChange]
        let responseChanges = getAeson issue.responsePayloads :: [Anomalies.PayloadChange]
        let allChanges = requestChanges ++ responseChanges
        let breakingChanges = length $ filter (\c -> c.changeType == Anomalies.Breaking) allChanges
        let incrementalChanges = length $ filter (\c -> c.changeType == Anomalies.Incremental) allChanges
        let totalChanges = length allChanges
        let affectedPayloads = if null requestChanges then length responseChanges else if null responseChanges then length requestChanges else length requestChanges + length responseChanges

        div_ [class_ "flex items-center gap-4 text-sm mb-4 p-3 bg-fillWeak rounded-lg"] do
          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-textStrong"] $ toHtml $ show totalChanges
            " total changes"

          div_ [class_ "w-px h-4 bg-strokeWeak"] ""

          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-fillError-strong"] $ toHtml $ show breakingChanges
            " breaking"
            when (breakingChanges > 0 && totalChanges > 0) do
              span_ [class_ "text-xs ml-1 bg-fillError-weak text-fillError-strong px-1.5 py-0.5 rounded"] do
                toHtml $ show (round (fromIntegral breakingChanges / fromIntegral totalChanges * 100 :: Float) :: Int) <> "%"

          div_ [class_ "w-px h-4 bg-strokeWeak"] ""

          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-fillSuccess-strong"] $ toHtml $ show incrementalChanges
            " incremental"

          div_ [class_ "w-px h-4 bg-strokeWeak"] ""

          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-textBrand"] $ toHtml $ show affectedPayloads
            " payloads affected"

      -- Stack trace for runtime exceptions or Query for alerts
      case issue.issueType of
        Issues.RuntimeException -> do
          case AE.fromJSON (getAeson issue.issueData) of
            AE.Success (exceptionData :: Issues.RuntimeExceptionData) -> do
              div_ [class_ "bg-fillError-weak border border-strokeError-weak rounded-lg p-4 text-sm font-mono text-fillError-strong mb-4"] do
                pre_ [class_ "whitespace-pre-wrap"] $ toHtml exceptionData.stackTrace
            _ -> pass
        Issues.QueryAlert -> do
          case AE.fromJSON (getAeson issue.issueData) of
            AE.Success (alertData :: Issues.QueryAlertData) -> do
              div_ [class_ "mb-4"] do
                span_ [class_ "text-sm text-textWeak mb-2 block font-medium"] "Query:"
                div_ [class_ "bg-fillInformation-weak border border-strokeInformation-weak rounded-lg p-3 text-sm font-mono text-fillInformation-strong max-w-2xl overflow-x-auto"]
                  $ toHtml alertData.queryExpression
            _ -> pass
        _ -> pass

      -- Recommended action
      div_ [class_ "border-l-4 border-strokeBrand pl-4 mb-4"] do
        p_ [class_ "text-sm text-textStrong leading-relaxed"] $ toHtml issue.recommendedAction

      -- Collapsible payload changes (only for API changes)
      when (issue.issueType == Issues.APIChange) do
        details_ [class_ "group mb-4"] do
          summary_ [class_ "inline-flex items-center cursor-pointer whitespace-nowrap text-sm font-medium transition-all rounded-md gap-1.5 text-textBrand hover:text-textBrand/80 list-none"] do
            faSprite_ "chevron-right" "regular" "h-4 w-4 mr-1 transition-transform group-open:rotate-90"
            "View detailed payload changes"

          -- Payload details content
          div_ [class_ "mt-4 border border-strokeWeak rounded-lg overflow-hidden bg-bgRaised"] do
            renderPayloadChanges issue

      -- Action buttons
      div_ [class_ "flex items-center gap-3 mt-4 pt-4 border-t border-strokeWeak"] do
        button_ [class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 text-textBrand hover:text-textBrand/80 hover:bg-fillBrand-weak"] do
          faSprite_ "eye" "regular" "w-4 h-4"
          span_ [class_ "leading-none"] "view related logs"

        button_ [class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 border bg-background hover:text-accent-foreground text-textBrand border-strokeBrand-strong hover:bg-fillBrand-weak"] do
          faSprite_ "code" "regular" "w-4 h-4"
          span_ [class_ "leading-none"] "View Full Schema"

        -- Acknowledge button
        let isAcknowledged = isJust issue.acknowledgedAt
        let acknowledgeEndpoint = "/p/" <> issue.projectId.toText <> "/anomalies/" <> Issues.issueIdText issue.id <> if isAcknowledged then "/unacknowlege" else "/acknowlege"
        button_
          [ class_
              $ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 "
              <> if isAcknowledged
                then "bg-fillSuccess-weak text-fillSuccess-strong border border-strokeSuccess-weak hover:bg-fillSuccess-weak/80"
                else "bg-fillPrimary text-textInverse-strong hover:bg-fillPrimary/90"
          , hxGet_ acknowledgeEndpoint
          , hxSwap_ "outerHTML"
          , hxTarget_ "closest .itemsListItem"
          ]
          do
            faSprite_ "check" "regular" "w-4 h-4"
            span_ [class_ "leading-none"] $ if isAcknowledged then "Acknowledged" else "Acknowledge"

        -- Archive button
        let isArchived = isJust issue.archivedAt
        let archiveEndpoint = "/p/" <> issue.projectId.toText <> "/anomalies/" <> Issues.issueIdText issue.id <> if isArchived then "/unarchive" else "/archive"
        button_
          [ class_
              $ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 "
              <> if isArchived
                then "bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak hover:bg-fillWarning-weak/80"
                else "border border-strokeWeak text-textStrong hover:bg-fillWeak"
          , hxGet_ archiveEndpoint
          , hxSwap_ "outerHTML"
          , hxTarget_ "closest .itemsListItem"
          ]
          do
            faSprite_ "archive" "regular" "w-4 h-4"
            span_ [class_ "leading-none"] $ if isArchived then "Unarchive" else "Archive"

    -- Events count
    div_ [class_ "w-36 flex items-start justify-center"]
      $ span_ [class_ "tabular-nums text-xl", term "data-tippy-content" "Events for this Issue in the last 14days"]
      $ show issue.eventCount

    -- Chart widget
    let mockChartData =
          if hideByDefault -- Only provide mock data for slider view
            then case issueId of
              "hash123" ->
                Just
                  $ AE.Array
                  $ V.fromList
                    [ AE.Array $ V.fromList [AE.Number 1735689600000, AE.Number 3] -- timestamp, count
                    , AE.Array $ V.fromList [AE.Number 1735693200000, AE.Number 5]
                    , AE.Array $ V.fromList [AE.Number 1735696800000, AE.Number 8]
                    , AE.Array $ V.fromList [AE.Number 1735700400000, AE.Number 12]
                    , AE.Array $ V.fromList [AE.Number 1735704000000, AE.Number 15]
                    , AE.Array $ V.fromList [AE.Number 1735707600000, AE.Number 10]
                    , AE.Array $ V.fromList [AE.Number 1735711200000, AE.Number 8]
                    , AE.Array $ V.fromList [AE.Number 1735714800000, AE.Number 5]
                    ]
              "hash456" ->
                Just
                  $ AE.Array
                  $ V.fromList
                    [ AE.Array $ V.fromList [AE.Number 1735689600000, AE.Number 2]
                    , AE.Array $ V.fromList [AE.Number 1735693200000, AE.Number 4]
                    , AE.Array $ V.fromList [AE.Number 1735696800000, AE.Number 7]
                    , AE.Array $ V.fromList [AE.Number 1735700400000, AE.Number 11]
                    , AE.Array $ V.fromList [AE.Number 1735704000000, AE.Number 15]
                    , AE.Array $ V.fromList [AE.Number 1735707600000, AE.Number 18]
                    , AE.Array $ V.fromList [AE.Number 1735711200000, AE.Number 12]
                    , AE.Array $ V.fromList [AE.Number 1735714800000, AE.Number 8]
                    ]
              "hash789" ->
                Just
                  $ AE.Array
                  $ V.fromList -- Payment error spike
                    [ AE.Array $ V.fromList [AE.Number 1735689600000, AE.Number 5]
                    , AE.Array $ V.fromList [AE.Number 1735693200000, AE.Number 3]
                    , AE.Array $ V.fromList [AE.Number 1735696800000, AE.Number 8]
                    , AE.Array $ V.fromList [AE.Number 1735700400000, AE.Number 45] -- Spike
                    , AE.Array $ V.fromList [AE.Number 1735704000000, AE.Number 120] -- Big spike
                    , AE.Array $ V.fromList [AE.Number 1735707600000, AE.Number 85]
                    , AE.Array $ V.fromList [AE.Number 1735711200000, AE.Number 20]
                    , AE.Array $ V.fromList [AE.Number 1735714800000, AE.Number 10]
                    ]
              "hash1011" ->
                Just
                  $ AE.Array
                  $ V.fromList -- High error rate
                    [ AE.Array $ V.fromList [AE.Number 1735689600000, AE.Number 10]
                    , AE.Array $ V.fromList [AE.Number 1735693200000, AE.Number 15]
                    , AE.Array $ V.fromList [AE.Number 1735696800000, AE.Number 25]
                    , AE.Array $ V.fromList [AE.Number 1735700400000, AE.Number 40]
                    , AE.Array $ V.fromList [AE.Number 1735704000000, AE.Number 65]
                    , AE.Array $ V.fromList [AE.Number 1735707600000, AE.Number 90] -- Peak
                    , AE.Array $ V.fromList [AE.Number 1735711200000, AE.Number 75]
                    , AE.Array $ V.fromList [AE.Number 1735714800000, AE.Number 60]
                    ]
              _ -> Nothing
            else Nothing
    div_ [class_ "flex items-start justify-center "]
      $ div_ [class_ "w-56 h-12 px-3"]
      $ Widget.widget_
      $ (def :: Widget.Widget)
        { Widget.standalone = Just True
        , Widget.id = Just issueId
        , Widget.title = Just issueId
        , Widget.showTooltip = Just False
        , Widget.naked = Just True
        , Widget.xAxis = Just (def{Widget.showAxisLabel = Just False})
        , Widget.yAxis = Just (def{Widget.showOnlyMaxLabel = Just True})
        , Widget.query = Just "status_code == \"ERROR\" | summarize count(*) by bin(timestamp, 1h)"
        , Widget._projectId = Just issue.projectId
        , Widget.hideLegend = Just True
        , Widget.eager = if isJust mockChartData then Just True else Nothing -- Enable eager mode when we have mock data
        , Widget.dataset =
            mockChartData >>= \chartData ->
              Just
                Widget.WidgetDataset
                  { Widget.source = chartData
                  , Widget.rowsPerMin = Nothing
                  , Widget.value = Nothing
                  , Widget.from = Nothing
                  , Widget.to = Nothing
                  , Widget.stats = Nothing
                  }
        }


-- Render payload changes section
renderPayloadChanges :: Issues.IssueL -> Html ()
renderPayloadChanges issue =
  when (issue.issueType == Issues.APIChange) do
    let requestChanges = getAeson issue.requestPayloads :: [Anomalies.PayloadChange]
    let responseChanges = getAeson issue.responsePayloads :: [Anomalies.PayloadChange]

    when (not (null requestChanges) || not (null responseChanges)) do
      div_ [class_ "border border-strokeWeak rounded-lg overflow-hidden bg-bgRaised group/payloadtabs"] do
        div_ [class_ "flex flex-col gap-2"] do
          -- Tab navigation using radio buttons
          div_ [role_ "tablist", Aria.orientation_ "horizontal", class_ "text-muted-foreground h-9 items-center justify-center rounded-xl p-[3px] w-full grid grid-cols-2 bg-fillWeak"] do
            -- Response tab (default active)
            label_
              [ role_ "tab"
              , class_ "h-[calc(100%-1px)] flex-1 justify-center rounded-xl border border-transparent px-2 py-1 text-sm font-medium whitespace-nowrap transition-all flex items-center gap-2 cursor-pointer has-[:checked]:bg-bgRaised has-[:checked]:text-textStrong bg-transparent text-textWeak"
              ]
              do
                input_ [type_ "radio", name_ ("payload-tab-" <> Issues.issueIdText issue.id), class_ "hidden payload-tab-response", checked_]
                faSprite_ "arrow-right" "regular" "w-4 h-4"
                span_ [] $ "Response Payloads (" <> show (length responseChanges) <> ")"

            -- Request tab
            label_
              [ role_ "tab"
              , class_ "h-[calc(100%-1px)] flex-1 justify-center rounded-xl border border-transparent px-2 py-1 text-sm font-medium whitespace-nowrap transition-all flex items-center gap-2 cursor-pointer has-[:checked]:bg-bgRaised has-[:checked]:text-textStrong bg-transparent text-textWeak"
              ]
              do
                input_ [type_ "radio", name_ ("payload-tab-" <> Issues.issueIdText issue.id), class_ "hidden payload-tab-request"]
                faSprite_ "arrow-right" "regular" "w-4 h-4 rotate-180"
                span_ [] $ "Request Payloads (" <> show (length requestChanges) <> ")"

          -- Tab panels
          -- Response panel (visible when response tab is selected)
          div_
            [ role_ "tabpanel"
            , class_ "flex-1 outline-none p-4 space-y-4 hidden group-has-[.payload-tab-response:checked]/payloadtabs:block"
            ]
            do
              if null responseChanges
                then div_ [class_ "text-center py-8 text-textWeak"] "No response payload changes"
                else forM_ responseChanges (renderPayloadChange True)

          -- Request panel (visible when request tab is selected)
          div_
            [ role_ "tabpanel"
            , class_ "flex-1 outline-none p-4 space-y-4 hidden group-has-[.payload-tab-request:checked]/payloadtabs:block"
            ]
            do
              if null requestChanges
                then div_ [class_ "text-center py-8 text-textWeak"] "No request payload changes"
                else forM_ requestChanges (renderPayloadChange False)


-- Render individual payload change
renderPayloadChange :: Bool -> Anomalies.PayloadChange -> Html ()
renderPayloadChange isResponse change =
  div_ [class_ "border border-strokeWeak rounded-lg p-4 bg-fillWeak"] do
    -- Status code/method badges and info
    div_ [class_ "flex items-center gap-3 mb-3 flex-wrap"] do
      -- Status code or method badge
      case (change.statusCode, change.method) of
        (Just statusCode, _) -> do
          let statusClass = case statusCode of
                200 -> "bg-fillSuccess-strong text-textInverse-strong shadow-sm"
                401 -> "bg-fillWarning-strong text-textInverse-strong shadow-sm"
                422 -> "bg-fillWarning-strong text-textInverse-strong shadow-sm"
                404 -> "bg-fillWarning-strong text-textInverse-strong shadow-sm"
                500 -> "bg-fillError-strong text-textInverse-strong shadow-sm"
                _ -> "bg-fillInformation-strong text-textInverse-strong shadow-sm"
          span_ [class_ $ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-transparent " <> statusClass] do
            toHtml $ show statusCode <> " " <> fromMaybe "" change.statusText
        (_, Just method) ->
          span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillInformation-strong text-textInverse-strong shadow-sm"] do
            toHtml method
        _ -> pass

      -- Content type badge
      span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-strokeWeak text-textWeak bg-bgRaised"] do
        toHtml change.contentType

      -- Change type badge
      case change.changeType of
        Anomalies.Breaking ->
          span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-transparent bg-fillError-strong text-textInverse-strong shadow-sm"] do
            faSprite_ "circle-x" "regular" "w-3 h-3 mr-1"
            "Breaking"
        Anomalies.Incremental ->
          span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-transparent bg-fillInformation-strong text-textInverse-strong shadow-sm"] do
            faSprite_ "info" "regular" "w-3 h-3 mr-1"
            "Incremental"
        Anomalies.Safe ->
          span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-transparent bg-fillSuccess-strong text-textInverse-strong shadow-sm"] do
            faSprite_ "circle-check" "regular" "w-3 h-3 mr-1"
            "Safe"

    -- Description
    when (T.length change.description > 0) do
      p_ [class_ "text-sm text-textWeak mb-4 leading-relaxed"] $ toHtml change.description

    -- Field changes section
    unless (null change.changes) do
      div_ [class_ "space-y-3"] do
        -- Section header
        div_ [class_ "flex items-center gap-2 pb-2 border-b border-strokeWeak"] do
          faSprite_ "code" "regular" "w-4 h-4 text-iconNeutral"
          span_ [class_ "font-medium text-textStrong"]
            $ case (change.statusCode, change.statusText) of
              (Just code, Just txt) -> toHtml $ show code <> " Response Changes"
              _ -> "Payload Changes"
          span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 font-medium w-fit whitespace-nowrap shrink-0 gap-1 text-xs border-strokeWeak text-textWeak bg-fillWeak"] do
            toHtml change.contentType

        -- Individual field changes
        div_ [class_ "space-y-3"] do
          forM_ change.changes renderFieldChange

    -- Examples section
    when (T.length change.exampleBefore > 0 || T.length change.exampleAfter > 0) do
      div_ [class_ "mt-4 space-y-3"] do
        span_ [class_ "text-sm font-medium text-textStrong"] "Example Payloads:"
        div_ [class_ "grid grid-cols-2 gap-4"] do
          when (T.length change.exampleBefore > 0) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "Before:"
              pre_ [class_ "bg-fillError-weak text-fillError-strong p-3 rounded text-xs overflow-x-auto border border-strokeError-weak"] do
                toHtml change.exampleBefore
          when (T.length change.exampleAfter > 0) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "After:"
              pre_ [class_ "bg-fillSuccess-weak text-fillSuccess-strong p-3 rounded text-xs overflow-x-auto border border-strokeSuccess-weak"] do
                toHtml change.exampleAfter


-- Render individual field change
renderFieldChange :: Anomalies.FieldChange -> Html ()
renderFieldChange fieldChange =
  div_ [class_ "border border-strokeWeak rounded-lg p-4 bg-bgRaised"] do
    -- Field name and change kind badges
    div_ [class_ "flex items-start justify-between gap-4 mb-3"] do
      div_ [class_ "flex items-center gap-2 flex-wrap"] do
        -- Field path in monospace
        span_ [class_ "font-mono text-sm bg-fillWeak px-2 py-1 rounded text-textStrong"] $ toHtml fieldChange.path

        -- Change kind badge
        case fieldChange.changeKind of
          Anomalies.Modified ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 text-fillInformation-strong border-strokeInformation-strong bg-fillInformation-weak"] "modified"
          Anomalies.Added ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 text-fillSuccess-strong border-strokeSuccess-strong bg-fillSuccess-weak"] "added"
          Anomalies.Removed ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 text-fillError-strong border-strokeError-strong bg-fillError-weak"] "removed"

        -- Breaking badge if applicable
        when fieldChange.breaking do
          span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 border-transparent bg-fillError-strong text-textInverse-strong shadow-sm"] do
            faSprite_ "triangle-alert" "regular" "w-3 h-3 mr-1"
            "Breaking"

    -- Change description
    when (T.length fieldChange.changeDescription > 0) do
      p_ [class_ "text-sm text-textWeak mb-3 leading-relaxed"] $ toHtml fieldChange.changeDescription

    -- Type and value changes
    div_ [class_ "space-y-3"] do
      -- Type changes (if modified)
      when (fieldChange.changeKind == Anomalies.Modified || fieldChange.changeKind == Anomalies.Added || fieldChange.changeKind == Anomalies.Removed) do
        div_ [class_ "grid grid-cols-2 gap-4"] do
          when (isJust fieldChange.oldType) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "Previous Type:"
              code_ [class_ "block bg-fillError-weak text-fillError-strong px-3 py-2 rounded text-xs border border-strokeError-weak"] do
                toHtml $ fromMaybe "" fieldChange.oldType
          when (isJust fieldChange.newType) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "New Type:"
              code_ [class_ "block bg-fillSuccess-weak text-fillSuccess-strong px-3 py-2 rounded text-xs border border-strokeSuccess-weak"] do
                toHtml $ fromMaybe "" fieldChange.newType

      -- Value examples (if available)
      when (isJust fieldChange.oldValue || isJust fieldChange.newValue) do
        div_ [class_ "grid grid-cols-2 gap-4"] do
          when (isJust fieldChange.oldValue) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "Previous Value:"
              code_ [class_ "block bg-fillError-weak text-fillError-strong px-3 py-2 rounded text-xs font-mono whitespace-pre-wrap border border-strokeError-weak"] do
                toHtml $ fromMaybe "" fieldChange.oldValue
          when (isJust fieldChange.newValue) do
            div_ [] do
              span_ [class_ "text-xs text-textWeak block mb-1 font-medium"] "New Value:"
              code_ [class_ "block bg-fillSuccess-weak text-fillSuccess-strong px-3 py-2 rounded text-xs font-mono whitespace-pre-wrap border border-strokeSuccess-weak"] do
                toHtml $ fromMaybe "" fieldChange.newValue


anomalyAcknowlegeButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Text -> Html ()
anomalyAcknowlegeButton pid aid acked host = do
  let acknowlegeAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText aid <> if acked then "/unacknowlege" else "/acknowlege?host=" <> host
  a_
    [ class_
        $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl  "
        <> (if acked then "bg-fillSuccess-weak text-textSuccess" else "btn-primary")
    , term "data-tippy-content" "acknowlege issue"
    , hxGet_ acknowlegeAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "check" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] $ if acked then "Acknowleged" else "Acknowlege"


anomalyArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
anomalyArchiveButton pid aid archived = do
  let archiveAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText aid <> if archived then "/unarchive" else "/archive"
  a_
    [ class_
        $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl "
        <> (if archived then " bg-fillSuccess-weak text-textSuccess" else "btn-primary")
    , term "data-tippy-content" $ if archived then "unarchive" else "archive"
    , hxGet_ archiveAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] $ if archived then "Unarchive" else "Archive"
