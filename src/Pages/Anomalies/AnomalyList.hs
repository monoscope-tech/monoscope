module Pages.Anomalies.AnomalyList (
  anomalyListGetH,
  anomalyDetailsGetH,
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
  AnomalyDetails,
  IssueVM (..),
)
where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Default (def)
import Data.Map qualified as Map
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime, parseTimeM, utc, utcToZonedTime, utctDayTime)
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
import Models.Apis.Anomalies (ChangeType (..), FieldChange (..), FieldChangeKind (..), PayloadChange (..))
import Models.Apis.Anomalies qualified as Anomalies
import Models.Apis.Endpoints qualified as Endpoints
import Models.Apis.Fields.Query qualified as Fields
import Models.Apis.Fields.Types (
  Field (fieldType),
  fieldTypeToText,
  fieldsToNormalized,
  groupFieldsByCategory,
 )
import Models.Apis.Fields.Types qualified as Fields
import Models.Apis.Issues qualified as Issues
import Models.Apis.RequestDumps qualified as RequestDumps
import Models.Apis.Shapes (ShapeId (..), getShapeFields)
import Models.Apis.Shapes qualified as Shapes
import Models.Projects.Projects qualified as Projects
import Models.Users.Sessions qualified as Sessions
import Models.Users.Users (User (id))
import NeatInterpolation (text)
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..))
import Pages.Components (dateTime, statBox_)
import Pkg.Components.ItemsList (TabFilter (..), TabFilterOpt (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.Components.Widget qualified as Widget
import PyF (fmt)
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
  let q = [sql| update apis.anomalies set acknowleged_by=null, acknowleged_at=null where id=? |]
  let qI = [sql| update apis.issues set acknowleged_by=null, acknowleged_at=null where id=? |]
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
  let useMockData = True -- Set to False to use real database data
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
        let mockIssue1' = mockIssue1{Issues.requestPayloads = Aeson mockPayloadChanges1, Issues.responsePayloads = Aeson mockResponsePayloads1} :: Issues.IssueL
        let mockIssue2' = mockIssue2{Issues.responsePayloads = Aeson mockPayloadChanges2} :: Issues.IssueL
        pure $ V.fromList [mockIssue1', mockIssue2', mockIssue3, mockIssue4]
      else
        -- Fetch real data from database
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
        span_ [class_ "text-lg text-slate-700"] "Ongoing Issues and Monitors"
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
        span_ [class_ "text-lg text-slate-700"] "Ongoing Issues and Monitors"
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


shapeParameterStats_ :: Int -> Int -> Int -> Html ()
shapeParameterStats_ newF deletedF updatedFF = div_ [class_ "flex items-center gap-2"] do
  span_ [] "Fields:"
  div_ [class_ "flex items-center gap-2 "] do
    fieldStats newF "new" "text-green-600"
    fieldStats updatedFF "updated" "text-brand"
    fieldStats deletedF "deleted" "text-red-500"


fieldStats :: Int -> Text -> Text -> Html ()
fieldStats newF field cls = div_ [class_ "flex items-center gap-2 "] do
  div_ [class_ $ "text-center text-lg font-medium " <> cls] do
    toHtml @String $ show newF
    small_ [class_ "ml-2 text-slate-500"] $ toHtml field
  when (field /= "deleted") $ do
    small_ [class_ "text-slate-200 "] "|"


-- anomalyAccentColor isAcknowleged isArchived
anomalyAccentColor :: Bool -> Bool -> Text
anomalyAccentColor _ True = "bg-slate-400"
anomalyAccentColor True False = "bg-green-200"
anomalyAccentColor False False = "bg-red-800"


anomalyDetailsGetH :: Projects.ProjectId -> Text -> Maybe Text -> ATAuthCtx (RespHeaders AnomalyDetails)
anomalyDetailsGetH pid targetHash hxBoostedM = do
  (sess, project) <- Sessions.sessionAndProject pid
  -- For now, still use old function until we have a replacement
  issueM <- dbtToEff $ Anomalies.selectIssueByHash pid targetHash
  freeTierExceeded <- dbtToEff $ checkFreeTierExceeded pid project.paymentPlan

  let bwconf =
        (def :: BWConfig)
          { sessM = Just sess
          , currProject = Just project
          , pageTitle = "Issue Details"
          , freeTierExceeded = freeTierExceeded
          }
  case issueM of
    Nothing -> addRespHeaders $ AnomalyDetailsNoFound $ PageCtx bwconf ()
    Just oldIssue -> do
      currTime <- liftIO getCurrentTime
      -- Convert old issue to new format for display
      let issue = convertOldIssueToNew oldIssue
      -- For now, return simplified view - can be enhanced later with actual data fetching
      case hxBoostedM of
        Just _ -> addRespHeaders $ AnomalyDetailsBoosted (issue, Nothing, Nothing, Nothing, currTime, Nothing, True)
        Nothing -> addRespHeaders $ AnomalyDetailsMain $ PageCtx bwconf (issue, Nothing, Nothing, Nothing, currTime, Nothing, False)
  where
    convertOldIssueToNew oldIssue =
      Issues.IssueL
        { id = Issues.IssueId oldIssue.id.unAnomalyId
        , createdAt = oldIssue.createdAt
        , updatedAt = oldIssue.updatedAt
        , projectId = oldIssue.projectId
        , issueType = case oldIssue.anomalyType of
            Anomalies.ATShape -> Issues.APIChange
            Anomalies.ATFormat -> Issues.APIChange
            Anomalies.ATEndpoint -> Issues.APIChange
            Anomalies.ATRuntimeException -> Issues.RuntimeException
            _ -> Issues.APIChange
        , endpointHash = oldIssue.endpointHash
        , acknowledgedAt = oldIssue.acknowlegedAt
        , acknowledgedBy = oldIssue.acknowlegedBy
        , archivedAt = oldIssue.archivedAt
        , title = oldIssue.title
        , service = oldIssue.service
        , critical = oldIssue.critical
        , severity = if oldIssue.critical then "critical" else "warning"
        , affectedRequests = oldIssue.affectedClients
        , affectedClients = oldIssue.affectedClients
        , errorRate = Nothing
        , recommendedAction = oldIssue.recommendedAction
        , migrationComplexity = oldIssue.migrationComplexity
        , issueData = Aeson AE.Null -- Simplified for now
        , requestPayloads = Aeson []
        , responsePayloads = Aeson []
        , llmEnhancedAt = Nothing
        , llmEnhancementVersion = Nothing
        , eventCount = oldIssue.eventsAgg.count
        , lastSeen = oldIssue.eventsAgg.lastSeen
        }


data AnomalyDetails
  = AnomalyDetailsMain (PageCtx (Issues.IssueL, Maybe (V.Vector Shapes.ShapeWithFields), Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]), Maybe (V.Vector Text), UTCTime, Maybe Text, Bool))
  | AnomalyDetailsBoosted (Issues.IssueL, Maybe (V.Vector Shapes.ShapeWithFields), Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]), Maybe (V.Vector Text), UTCTime, Maybe Text, Bool)
  | AnomalyDetailsNoFound (PageCtx ())


instance ToHtml AnomalyDetails where
  toHtml (AnomalyDetailsMain (PageCtx bwconf (issue, shapesWithFieldsMap, fields, prvFormatsM, currTime, timeFilter, modal))) = toHtml $ PageCtx bwconf $ anomalyDetailsPageM issue shapesWithFieldsMap fields prvFormatsM currTime modal timeFilter
  toHtml (AnomalyDetailsBoosted (issue, shapesWithFieldsMap, fields, prvFormatsM, currTime, timeFilter, modal)) = toHtml $ anomalyDetailsPage issue shapesWithFieldsMap fields prvFormatsM currTime timeFilter modal
  toHtml (AnomalyDetailsNoFound (PageCtx bwconf ())) = toHtml $ PageCtx bwconf notFoundPage
  toHtmlRaw = toHtml


notFoundPage :: Html ()
notFoundPage = do
  h4_ [] "ANOMALY NOT FOUND"


anomalyDetailsPageM :: Issues.IssueL -> Maybe (V.Vector Shapes.ShapeWithFields) -> Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]) -> Maybe (V.Vector Text) -> UTCTime -> Bool -> Maybe Text -> Html ()
anomalyDetailsPageM issue shapesWithFieldsMap fields prvFormatsM currTime modal timeFilter = do
  div_ [class_ "w-full px-32 overflow-y-scroll h-full"] do
    h1_ [class_ "my-10 py-2 border-b w-full text-lg font-semibold"] "Anomaly Details"
    anomalyDetailsPage issue shapesWithFieldsMap fields prvFormatsM currTime timeFilter modal


anomalyDetailsPage :: Issues.IssueL -> Maybe (V.Vector Shapes.ShapeWithFields) -> Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]) -> Maybe (V.Vector Text) -> UTCTime -> Maybe Text -> Bool -> Html ()
anomalyDetailsPage issue shapesWithFieldsMap fields prvFormatsM currTime timeFilter modal = do
  let filterV = fromMaybe "14d" timeFilter
  --   statBox_ (Just pid) Nothing "Passed" "Total number of steps passed in the last test run" (fmt (commaizeF passed)) Nothing

  div_ [class_ "w-full "] do
    div_ [class_ "w-full"] do
      case issue.issueType of
        Issues.APIChange -> do
          let content = div_ [class_ "flex gap-6 shrink-1"] do
                statBox_ Nothing Nothing "Breaking" "Breaking changes detected" (show $ if issue.critical then 1 else 0) Nothing (Just "text-red-600")
                statBox_ Nothing Nothing "Incremental" "Non-breaking changes" (show $ issue.affectedRequests) Nothing (Just "text-green-500")
              anButton :: Html ()
              anButton =
                if issue.critical
                  then button_ [class_ "h-6 flex items-center px-2 py-1 rounded-lg bg-fillStrong text-textInverse-strong"] "Breaking"
                  else button_ [class_ "h-6 flex items-center px-2 py-1 rounded-lg bg-fillWeak border text-textStrong border-strokeWeak"] "Incremental"
          detailsHeader "API Change" "GET" 200 issue currTime filterV (Just content) (Just anButton)
        Issues.RuntimeException -> do
          detailsHeader "Runtime Exception" "ERROR" 500 issue currTime filterV Nothing Nothing
        Issues.QueryAlert -> do
          detailsHeader "Query Alert" "ALERT" 200 issue currTime filterV Nothing Nothing

    div_ [class_ "mt-6 space-y-4"] do
      div_ [class_ "tabs tabs-bordered border rounded-3xl overflow-hidden", role_ "tablist"] do
        input_ [type_ "radio", name_ $ "anomaly-events-tabs-" <> issue.endpointHash, role_ "tab", class_ "tab", Aria.label_ "Overview", checked_]
        div_ [role_ "tabpanel", class_ "tab-content p-4 w-full bg-base-100 rounded-lg overflow-x-hidden", id_ "overview_content"] do
          case issue.issueType of
            Issues.APIChange ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (apiData :: Issues.APIChangeData) ->
                  apiChangeOverview apiData
                _ -> ""
            Issues.RuntimeException ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (errorData :: Issues.RuntimeExceptionData) ->
                  runtimeExceptionOverview errorData
                _ -> ""
            Issues.QueryAlert ->
              case AE.fromJSON (getAeson issue.issueData) of
                AE.Success (alertData :: Issues.QueryAlertData) ->
                  queryAlertOverview alertData
                _ -> ""

        input_ [type_ "radio", name_ $ "anomaly-events-tabs-" <> issue.endpointHash, role_ "tab", class_ "tab", Aria.label_ "Events"]
        div_ [role_ "tabpanel", class_ "tab-content grow whitespace-nowrap py-2 divide-y overflow-x-hidden ", id_ "events_content"] do
          let anomalyQueryPartial = "" -- Simplified for now
              events_url = "/p/" <> UUID.toText (Projects.unProjectId issue.projectId) <> "/log_explorer?layout=resultTable&query=" <> escapedQueryPartial anomalyQueryPartial
          div_ [hxGet_ events_url, hxTrigger_ "intersect once", hxSwap_ "outerHTML"] $ span_ [class_ "loading loading-dots loading-md"] ""

      -- Add payload changes section for API changes
      renderPayloadChanges issue


detailsHeader :: Text -> Text -> Int -> Issues.IssueL -> UTCTime -> Text -> Maybe (Html ()) -> Maybe (Html ()) -> Html ()
detailsHeader title method statusCode issue currTime filterV content anBtn = do
  let anomalyQueryPartial = "" -- Simplified for now
  div_ [class_ "flex flex-col w-full"] do
    div_ [class_ "flex justify-between"] do
      div_ [class_ "flex items-center gap-4"] do
        span_ [class_ "flex items-center rounded-lg px-2 py-1 font-medium gap-2 border border-blue-300 bg-blue-100 text-brand"] $ toHtml method
        span_ [class_ "flex items-center rounded-lg px-2 py-1 font-medium gap-2 border border-green-300 bg-green-100 text-green-500"] $ toHtml $ show statusCode
        dateTime (zonedTimeToUTC issue.createdAt) Nothing
      anomalyActionButtons issue.projectId issue.id (isJust issue.acknowledgedAt) (isJust issue.archivedAt) ""
    span_ [class_ "font-medium text-2xl text-slate-600 mt-6"] $ toHtml title
    div_ [class_ "flex justify-between items-center gap-4 mt-8"] do
      let currentURL' = "/charts_html?pid=" <> issue.projectId.toText <> ("&query=" <> escapedQueryPartial [fmt|{anomalyQueryPartial} | summarize count(*) by bin(timestamp, 1d)|])
      div_ [class_ "flex flex-col gap-4"] do
        div_ [class_ "flex items-center w-full gap-9 border border-slate-200 rounded-2xl px-10 py-4"] do
          stBox "Events" (show issue.eventCount)
          stBox "First seen" $ toText $ prettyTimeAuto currTime $ zonedTimeToUTC issue.createdAt
          stBox "Last seen" $ toText $ prettyTimeAuto currTime issue.lastSeen
        whenJust content Relude.id
      div_ [class_ "flex flex-col gap-1"] do
        div_ [class_ "flex justify-end"] do
          div_ [class_ "rounded-lg border grid grid-cols-2 w-max h-7 bg-slate-200 overflow-hidden"] do
            a_
              [ class_ $ "cursor-pointer px-1.5 flex items-center text-xs h-full rounded-sm " <> (if filterV == "24h" then "bg-white" else "")
              , hxGet_ $ currentURL' <> "&since=24h"
              , hxTarget_ "#reqsChartsEC"
              , hxSwap_ "innerHTML"
              ]
              "24h"
            a_
              [ class_ $ "cursor-pointer px-1.5 flex items-center text-xs h-full rounded-sm " <> (if filterV == "14d" then "bg-white" else "")
              , hxGet_ $ currentURL' <> "&since=14d"
              , hxTarget_ "#reqsChartsEC"
              , hxSwap_ "innerHTML"
              ]
              "14d"

        div_
          [ id_ "reqsChartsEC"
          , class_ "w-[550px] mt-4 shrink-0 h-[180px]"
          , hxGet_ $ currentURL' <> "&since=" <> (if filterV == "14d" then "14D" else "24h")
          , hxTrigger_ "intersect"
          , hxSwap_ "innerHTML"
          ]
          ""


stBox :: Text -> Text -> Html ()
stBox title value =
  div_ [class_ "flex flex-col items-center py-4 gap-2"] do
    span_ [class_ "text-slate-950 text-sm font-medium"] $ toHtml value
    span_ [class_ "font-medium text-slate-500 text-xs"] $ toHtml title


buildQueryForAnomaly :: Anomalies.AnomalyTypes -> Text -> Text
buildQueryForAnomaly Anomalies.ATEndpoint hash = "hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATShape hash = "hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATFormat hash = "hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATField hash = "hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATRuntimeException hash = "hashes[*]==\"" <> hash <> "\""
buildQueryForAnomaly Anomalies.ATUnknown hash = ""


endpointOverview :: Maybe (V.Vector Shapes.ShapeWithFields) -> Html ()
endpointOverview shapesWithFieldsMap =
  div_ [] do
    whenJust shapesWithFieldsMap \s -> do
      reqResSection "Request" True (V.toList s)
      reqResSection "Response" False (V.toList s)


requestShapeOverview :: Maybe (Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field], Map Fields.FieldCategoryEnum [Fields.Field]) -> Html ()
requestShapeOverview fieldChanges = div_ [class_ "flex flex-col gap-6"] do
  whenJust fieldChanges \(fs, sn, th) -> do
    shapeOSection_ "New Unique Fields" "text-green-800" fs
    shapeOSection_ "Updated Fields" "text-slate-800" sn
    shapeOSection_ "Deleted Fields" "text-red-800" th
  where
    shapeOSection_ :: Text -> Text -> Map Fields.FieldCategoryEnum [Field] -> Html ()
    shapeOSection_ title color fields = div_ [class_ "flex flex-col"] do
      h3_ [class_ $ color <> " py-1 w-fit font-semibold border-b border-b-" <> color <> "-500 mb-2"] (toHtml title)
      div_ [class_ "px-2"] do
        p_ [class_ "hidden last:block"] ("No " <> toHtml title)
        subSubSection "Request Path Params" (Map.lookup Fields.FCPathParam fields)
        subSubSection "Request Query Params" (Map.lookup Fields.FCQueryParam fields)
        subSubSection "Request Headers" (Map.lookup Fields.FCRequestHeader fields)
        subSubSection "Request Body" (Map.lookup Fields.FCRequestBody fields)
        subSubSection "Response Headers" (Map.lookup Fields.FCResponseHeader fields)
        subSubSection "Response Body" (Map.lookup Fields.FCResponseBody fields)


anomalyFormatOverview :: Anomalies.NewFormatIssue -> V.Vector Text -> Html ()
anomalyFormatOverview formatData prevFormats =
  section_ [class_ "space-y-10"] do
    div_ [class_ "flex items-center gap-6"] do
      -- div_ do
      --   h6_ [class_ " text-slate-800"] "FIELD NAME"
      --   h3_ [class_ "text-base text-slate-800"] $ toHtml $ formatData.fieldKey
      div_ do
        h6_ [class_ " text-slate-800 "] "FIELD PATH"
        h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml formatData.fieldKeyPath
    -- div_ do
    --   h6_ [class_ " text-slate-800"] "FIELD CATEGORY"
    --   h4_ [class_ "text-base text-slate-800"] $ EndpointComponents.fieldCategoryToDisplay $ fromMaybe FCRequestBody an.fieldCategory
    div_ [class_ "flex items-center gap-6"] do
      div_ do
        h5_ [class_ " text-slate-800"] "NEW FIELD FORMAT"
        h3_ [class_ "text-base text-slate-800 monospace"] $ toHtml $ fieldTypeToText formatData.formatType
      div_ do
        h5_ [class_ " text-slate-800"] "PREVIOUS FIELD FORMATS"
        ul_ [class_ "list-disc"] do
          prevFormats & mapM_ \f -> do
            li_ [class_ "ml-10 text-slate-800 "] $ toHtml f
    div_ do
      h6_ [class_ "text-slate-600 mt-4 "] "EXAMPLE VALUES"
      ul_ [class_ "list-disc"] do
        formatData.examples & mapM_ \exs -> do
          forM_ exs \ex -> do
            li_ [class_ "ml-10 text-slate-800 "] $ toHtml ex


issueDisplayConfig :: Issues.IssueL -> (Text, Text)
issueDisplayConfig issue = case issue.issueType of
  Issues.APIChange -> ("API Change", "/public/assets/svgs/anomalies/fields.svg")
  Issues.RuntimeException -> ("Runtime Exception", "/public/assets/svgs/anomalies/fields.svg")
  Issues.QueryAlert -> ("Query Alert", "/public/assets/svgs/anomalies/fields.svg")


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
        h3_ [class_ "font-semibold text-textStrong text-base"] $ toHtml issue.title

        -- Issue type badge
        case issue.issueType of
          Issues.RuntimeException ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-strong text-fillWhite shadow-sm"] do
              faSprite_ "triangle-alert" "regular" "w-3 h-3"
              "ERROR"
          Issues.QueryAlert ->
            span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-strong text-fillWhite shadow-sm"] do
              faSprite_ "zap" "regular" "w-3 h-3"
              "ALERT"
          Issues.APIChange ->
            if issue.critical
              then span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-strong text-fillWhite shadow-sm"] do
                faSprite_ "exclamation-triangle" "regular" "w-3 h-3"
                "BREAKING"
              else span_ [class_ "inline-flex items-center justify-center rounded-md border px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillInformation-strong text-fillWhite shadow-sm"] do
                faSprite_ "info" "regular" "w-3 h-3 mr-0.5"
                "Incremental"

        -- Severity badge
        case issue.severity of
          "critical" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillError-weak text-fillError-strong border-2 border-strokeError-strong shadow-sm"] "CRITICAL"
          "warning" -> span_ [class_ "inline-flex items-center justify-center rounded-md px-2 py-0.5 text-xs font-medium w-fit whitespace-nowrap shrink-0 gap-1 bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak shadow-sm"] "WARNING"
          _ -> pass

      -- Metadata row (method, endpoint, service, time)
      div_ [class_ "flex items-center gap-4 text-sm text-textWeak mb-3 flex-wrap"] do
        -- Service badge
        span_ [class_ "flex items-center gap-1"] do
          div_ [class_ "w-3 h-3 bg-fillYellow rounded-sm"] ""
          span_ [class_ "text-textStrong"] $ toHtml issue.service

        -- Time since
        span_ [class_ "text-textWeak"] $ toHtml timeSinceString

      -- Statistics row (only for API changes)
      when (issue.issueType == Issues.APIChange) do
        div_ [class_ "flex items-center gap-4 text-sm mb-4 p-3 bg-fillWeak rounded-lg"] do
          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-textStrong"] $ toHtml $ show issue.eventCount
            " total events"

          div_ [class_ "w-px h-4 bg-strokeWeak"] ""

          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-textStrong"] $ toHtml $ show issue.affectedRequests
            " affected requests"

          div_ [class_ "w-px h-4 bg-strokeWeak"] ""

          span_ [class_ "text-textWeak"] do
            strong_ [class_ "text-textBrand"] $ toHtml $ show issue.affectedClients
            " affected clients"

      -- Recommended action
      div_ [class_ "border-l-4 border-strokeBrand pl-4 mb-4"] do
        p_ [class_ "text-sm text-textStrong leading-relaxed"] $ toHtml issue.recommendedAction

      -- Migration complexity (for API changes)
      when (issue.issueType == Issues.APIChange && issue.migrationComplexity /= "n/a") do
        div_ [class_ "flex items-center gap-2 mb-4"] do
          span_ [class_ "text-sm text-textWeak"] "Migration complexity:"
          let (complexityClass, complexityIcon) = case issue.migrationComplexity of
                "low" -> ("text-fillSuccess-strong", "check-circle")
                "medium" -> ("text-fillWarning-strong", "exclamation-circle")
                "high" -> ("text-fillError-strong", "times-circle")
                _ -> ("text-textWeak", "question-circle")
          span_ [class_ $ "flex items-center gap-1 text-sm font-medium " <> complexityClass] do
            faSprite_ complexityIcon "regular" "w-4 h-4"
            toHtml $ T.toTitle issue.migrationComplexity

      -- Action buttons
      div_ [class_ "flex items-center gap-3 mt-4 pt-4 border-t border-strokeWeak"] do
        button_ [class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 text-textBrand hover:text-textBrand/80 hover:bg-fillBrand-weak"] do
          faSprite_ "eye" "regular" "w-4 h-4"
          span_ [class_ "leading-none"] "view related logs"

        button_ [class_ "inline-flex items-center justify-center whitespace-nowrap text-sm font-medium transition-all h-8 rounded-md gap-1.5 px-3 border bg-background hover:text-accent-foreground text-textBrand border-strokeBrand-strong hover:bg-fillBrand-weak"] do
          faSprite_ "code" "regular" "w-4 h-4"
          span_ [class_ "leading-none"] "View Full Schema"

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


-- | Render API change overview section
apiChangeOverview :: Issues.APIChangeData -> Html ()
apiChangeOverview apiData = div_ [class_ "space-y-6"] do
  -- Endpoint information
  div_ [class_ "bg-fillWeak rounded-lg p-4"] do
    h4_ [class_ "text-sm font-medium text-textWeak mb-2"] "Endpoint"
    div_ [class_ "flex items-center gap-2 text-lg font-mono"] do
      span_ [class_ "text-textBrand font-medium"] $ toHtml apiData.endpointMethod
      span_ [class_ "text-textStrong"] $ toHtml apiData.endpointPath

  -- Field changes summary
  div_ [class_ "grid grid-cols-3 gap-4"] do
    div_ [class_ "bg-fillSuccess-weak rounded-lg p-4 border border-strokeSuccess-weak"] do
      div_ [class_ "text-2xl font-bold text-fillSuccess-strong"] $ toHtml $ show $ V.length apiData.newFields
      div_ [class_ "text-sm text-textWeak"] "New fields"
    div_ [class_ "bg-fillWarning-weak rounded-lg p-4 border border-strokeWarning-weak"] do
      div_ [class_ "text-2xl font-bold text-fillWarning-strong"] $ toHtml $ show $ V.length apiData.modifiedFields
      div_ [class_ "text-sm text-textWeak"] "Modified fields"
    div_ [class_ "bg-fillError-weak rounded-lg p-4 border border-strokeError-weak"] do
      div_ [class_ "text-2xl font-bold text-fillError-strong"] $ toHtml $ show $ V.length apiData.deletedFields
      div_ [class_ "text-sm text-textWeak"] "Deleted fields"

  -- Detailed field changes
  when (V.length apiData.newFields > 0) do
    div_ [class_ "border-l-4 border-strokeSuccess-strong pl-4"] do
      h5_ [class_ "font-medium text-textStrong mb-2"] "New Fields"
      ul_ [class_ "space-y-1 text-sm text-textWeak"] do
        V.forM_ apiData.newFields $ \field -> do
          li_ [class_ "font-mono"] $ toHtml field

  when (V.length apiData.modifiedFields > 0) do
    div_ [class_ "border-l-4 border-strokeWarning-strong pl-4"] do
      h5_ [class_ "font-medium text-textStrong mb-2"] "Modified Fields"
      ul_ [class_ "space-y-1 text-sm text-textWeak"] do
        V.forM_ apiData.modifiedFields $ \field -> do
          li_ [class_ "font-mono"] $ toHtml field

  when (V.length apiData.deletedFields > 0) do
    div_ [class_ "border-l-4 border-strokeError-strong pl-4"] do
      h5_ [class_ "font-medium text-textStrong mb-2"] "Deleted Fields"
      ul_ [class_ "space-y-1 text-sm text-textWeak"] do
        V.forM_ apiData.deletedFields $ \field -> do
          li_ [class_ "font-mono"] $ toHtml field


-- | Render runtime exception overview section
runtimeExceptionOverview :: Issues.RuntimeExceptionData -> Html ()
runtimeExceptionOverview errorData = div_ [class_ "space-y-6"] do
  -- Error information
  div_ [class_ "bg-fillError-weak rounded-lg p-4 border border-strokeError-weak"] do
    h4_ [class_ "text-lg font-medium text-fillError-strong mb-2"] $ toHtml errorData.errorType
    p_ [class_ "text-sm text-textStrong"] $ toHtml errorData.errorMessage

  -- Stack trace
  when (T.length errorData.stackTrace > 0) do
    div_ [class_ "bg-fillWeak rounded-lg p-4"] do
      h5_ [class_ "font-medium text-textStrong mb-2"] "Stack Trace"
      pre_ [class_ "text-xs font-mono text-textWeak overflow-x-auto"] $ toHtml errorData.stackTrace

  -- Request context
  div_ [class_ "grid grid-cols-2 gap-4"] do
    whenJust errorData.requestMethod $ \method -> do
      div_ [class_ "bg-fillWeak rounded-lg p-3"] do
        div_ [class_ "text-sm text-textWeak"] "Request Method"
        div_ [class_ "font-medium"] $ toHtml method
    whenJust errorData.requestPath $ \path -> do
      div_ [class_ "bg-fillWeak rounded-lg p-3"] do
        div_ [class_ "text-sm text-textWeak"] "Request Path"
        div_ [class_ "font-mono text-sm"] $ toHtml path

  -- Occurrence information
  div_ [class_ "flex items-center gap-6 text-sm"] do
    div_ do
      span_ [class_ "text-textWeak"] "First seen: "
      dateTime errorData.firstSeen Nothing
    div_ do
      span_ [class_ "text-textWeak"] "Last seen: "
      dateTime errorData.lastSeen Nothing
    div_ do
      span_ [class_ "text-textWeak"] "Occurrences: "
      span_ [class_ "font-medium text-textStrong"] $ toHtml $ show errorData.occurrenceCount


-- | Render query alert overview section
queryAlertOverview :: Issues.QueryAlertData -> Html ()
queryAlertOverview alertData = div_ [class_ "space-y-6"] do
  -- Alert information
  div_ [class_ "bg-fillWarning-weak rounded-lg p-4 border border-strokeWarning-weak"] do
    h4_ [class_ "text-lg font-medium text-fillWarning-strong mb-2"] $ toHtml alertData.queryName
    div_ [class_ "mt-2 text-sm"] do
      span_ [class_ "text-textWeak"] "Query ID: "
      span_ [class_ "font-mono"] $ toHtml alertData.queryId

  -- Query expression
  div_ [class_ "bg-fillWeak rounded-lg p-4"] do
    h5_ [class_ "font-medium text-textStrong mb-2"] "Query Expression"
    pre_ [class_ "text-sm font-mono text-textWeak overflow-x-auto"] $ toHtml alertData.queryExpression

  -- Threshold violation
  div_ [class_ "bg-fillError-weak rounded-lg p-4 border border-strokeError-weak"] do
    h5_ [class_ "font-medium text-textStrong mb-2"] "Threshold Violation"
    div_ [class_ "grid grid-cols-2 gap-4 mt-3"] do
      div_ do
        div_ [class_ "text-sm text-textWeak"] "Threshold"
        div_ [class_ "text-xl font-bold"] $ toHtml $ show alertData.thresholdValue
      div_ do
        div_ [class_ "text-sm text-textWeak"] "Actual Value"
        div_ [class_ "text-xl font-bold text-fillError-strong"] $ toHtml $ show alertData.actualValue
    div_ [class_ "mt-3 text-sm"] do
      span_ [class_ "text-textWeak"] "Condition: Value should be "
      span_ [class_ "font-medium"] $ toHtml alertData.thresholdType
      span_ [class_ "text-textWeak"] " threshold"

  -- Triggered time
  div_ [class_ "text-sm"] do
    span_ [class_ "text-textWeak"] "Triggered at: "
    dateTime alertData.triggeredAt Nothing


-- Render payload changes section
renderPayloadChanges :: Issues.IssueL -> Html ()
renderPayloadChanges issue =
  when (issue.issueType == Issues.APIChange) do
    let requestChanges = getAeson issue.requestPayloads
    let responseChanges = getAeson issue.responsePayloads

    when (not (null requestChanges) || not (null responseChanges)) do
      div_ [class_ "mt-8 space-y-6"] do
        h4_ [class_ "text-lg font-medium text-textStrong border-b pb-2"] "Payload Changes"

        when (not (null requestChanges)) do
          div_ [class_ "space-y-4"] do
            h5_ [class_ "font-medium text-textStrong"] "Request Payload Changes"
            forM_ requestChanges renderPayloadChange

        when (not (null responseChanges)) do
          div_ [class_ "space-y-4"] do
            h5_ [class_ "font-medium text-textStrong"] "Response Payload Changes"
            forM_ responseChanges renderPayloadChange


-- Render individual payload change
renderPayloadChange :: Anomalies.PayloadChange -> Html ()
renderPayloadChange change =
  div_ [class_ "border rounded-lg p-4 space-y-3"] do
    -- Change type badge
    div_ [class_ "flex items-center gap-2"] do
      case change.changeType of
        Anomalies.Breaking ->
          span_ [class_ "px-2 py-1 rounded text-xs font-medium bg-fillError-weak text-fillError-strong"] "BREAKING"
        Anomalies.Incremental ->
          span_ [class_ "px-2 py-1 rounded text-xs font-medium bg-fillWarning-weak text-fillWarning-strong"] "INCREMENTAL"
        Anomalies.Safe ->
          span_ [class_ "px-2 py-1 rounded text-xs font-medium bg-fillSuccess-weak text-fillSuccess-strong"] "SAFE"

      -- Display content type
      span_ [class_ "font-mono text-sm text-textWeak"] $ toHtml change.contentType

    -- Field changes
    when (not (null change.changes)) do
      div_ [class_ "ml-4 space-y-2"] do
        forM_ change.changes renderFieldChange

    -- Examples (if available)
    when (T.length change.exampleBefore > 0 || T.length change.exampleAfter > 0) do
      div_ [class_ "bg-fillWeak rounded p-3 space-y-2"] do
        when (T.length change.exampleBefore > 0) do
          div_ do
            div_ [class_ "text-sm font-medium text-textWeak"] "Before:"
            code_ [class_ "block text-xs font-mono text-textStrong"] $ toHtml change.exampleBefore
        when (T.length change.exampleAfter > 0) do
          div_ do
            div_ [class_ "text-sm font-medium text-textWeak"] "After:"
            code_ [class_ "block text-xs font-mono text-textStrong"] $ toHtml change.exampleAfter


-- Render individual field change
renderFieldChange :: Anomalies.FieldChange -> Html ()
renderFieldChange fieldChange =
  div_ [class_ "flex items-start gap-3 text-sm"] do
    -- Field name and path
    div_ [class_ "min-w-[200px]"] do
      span_ [class_ "font-mono font-medium"] $ toHtml fieldChange.fieldName
      when (T.length fieldChange.path > 0) do
        div_ [class_ "text-xs text-textWeak font-mono"] $ toHtml fieldChange.path

    -- Change kind
    case fieldChange.changeKind of
      Anomalies.Modified -> do
        span_ [class_ "text-textWeak"] "Type changed:"
        span_ [class_ "font-mono"] $ toHtml $ fromMaybe "" fieldChange.oldType <> " → " <> fromMaybe "" fieldChange.newType
      Anomalies.Added -> do
        span_ [class_ "text-fillSuccess-strong"] "New field"
        whenJust fieldChange.newType $ \t ->
          span_ [class_ "font-mono text-textWeak"] $ "(" <> toHtml t <> ")"
      Anomalies.Removed -> do
        span_ [class_ "text-fillError-strong"] "Field removed"
        whenJust fieldChange.oldType $ \t ->
          span_ [class_ "font-mono text-textWeak"] $ "(" <> toHtml t <> ")"


anomalyActionButtons :: Projects.ProjectId -> Issues.IssueId -> Bool -> Bool -> Text -> Html ()
anomalyActionButtons pid aid acked achved host = do
  div_ [class_ "flex itms-center gap-2"] do
    anomalyAcknowlegeButton pid aid acked host
    anomalyArchiveButton pid aid achved


anomalyAcknowlegeButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Text -> Html ()
anomalyAcknowlegeButton pid aid acked host = do
  let acknowlegeAnomalyEndpoint = "/p/" <> pid.toText <> "/anomalies/" <> Issues.issueIdText aid <> if acked then "/unacknowlege" else "/acknowlege?host=" <> host
  a_
    [ class_
        $ "inline-flex items-center gap-2 cursor-pointer py-2 px-3 rounded-xl  "
          <> (if acked then "bg-green-100 text-green-900" else "btn-primary")
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
          <> (if archived then " bg-green-100 text-green-900" else "btn-primary")
    , term "data-tippy-content" $ if archived then "unarchive" else "archive"
    , hxGet_ archiveAnomalyEndpoint
    , hxSwap_ "outerHTML"
    ]
    do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "leading-none"] $ if archived then "Unarchive" else "Archive"


reqResSection :: Text -> Bool -> [Shapes.ShapeWithFields] -> Html ()
reqResSection title isRequest shapesWithFieldsMap =
  section_ [class_ "space-y-3"] do
    div_ [class_ "flex justify-between mt-5"] do
      div_ [class_ "flex flex-row"] do
        a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .reqResSubSection)|]]
          $ faSprite_ "chevron-down" "light" "h-4 mr-3 mt-1 w-4"
        span_ [class_ "text-lg text-slate-800"] $ toHtml title

    div_ [class_ "bg-base-100 border border-gray-100 rounded-xl py-5 px-5 space-y-6 reqResSubSection"]
      $ forM_ (zip [(1 :: Int) ..] shapesWithFieldsMap)
      $ \(index, s) -> do
        let sh = if index == 1 then title <> "_fields" else title <> "_fields hidden"
        div_ [class_ sh, id_ $ title <> "_" <> show index] do
          if isRequest
            then do
              subSubSection (title <> " Path Params") (Map.lookup Fields.FCPathParam s.fieldsMap)
              subSubSection (title <> " Query Params") (Map.lookup Fields.FCQueryParam s.fieldsMap)
              subSubSection (title <> " Headers") (Map.lookup Fields.FCRequestHeader s.fieldsMap)
              subSubSection (title <> " Body") (Map.lookup Fields.FCRequestBody s.fieldsMap)
            else do
              subSubSection (title <> " Headers") (Map.lookup Fields.FCResponseHeader s.fieldsMap)
              subSubSection (title <> " Body") (Map.lookup Fields.FCResponseBody s.fieldsMap)


-- | subSubSection ..
subSubSection :: Text -> Maybe [Fields.Field] -> Html ()
subSubSection title fieldsM = whenJust fieldsM \fields -> do
  div_ [class_ "space-y-1 mb-4"] do
    div_ [class_ "flex flex-row items-center"] do
      a_ [class_ "cursor-pointer", [__|on click toggle .neg-rotate-90 on me then toggle .hidden on (next .subSectionContent)|]] $ faSprite_ "chevron-down" "regular" "h-6 mr-3 w-6 p-1 cursor-pointer"
      div_ [class_ "px-4 rounded-xl w-full font-bold  text-textStrong"] $ toHtml title
    div_ [class_ "space-y-1 subSectionContent"] do
      fieldsToNormalized fields & mapM_ \(key, fieldM) -> do
        let segments = T.splitOn "." key
        let depth = length segments
        let depthPadding = "margin-left:" <> show (20 + (depth * 20)) <> "px"
        let displayKey = last ("" :| segments)
        case fieldM of
          Nothing -> do
            a_
              [ class_ "flex flex-row items-center"
              , style_ depthPadding
              , [__| on click toggle .neg-rotate-90 on <.chevron/> in me then collapseUntil((me), (my @data-depth))  |]
              ]
              do
                faSprite_ "chevron-down" "light" "h-6 w-6 mr-1 chevron cursor-pointer p-1"
                div_ [class_ "border flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full"] do
                  span_ [class_ " text-slate-800 inline-flex items-center"] $ toHtml displayKey
                  span_ [class_ " text-slate-600 inline-flex items-center ml-4"] do
                    if "[*]" `T.isSuffixOf` key
                      then fieldTypeToDisplay Fields.FTList
                      else fieldTypeToDisplay Fields.FTObject
          Just field -> do
            a_ [class_ "flex flex-row cursor-pointer", style_ depthPadding, term "data-depth" $ show depth] do
              faSprite_ "chevron-down" "light" "h-4 mr-3 mt-4 w-4 invisible"
              div_ [class_ "border-b flex flex-row border-gray-100 px-5 py-2 rounded-xl w-full items-center"] do
                span_ [class_ "grow  text-slate-800 inline-flex items-center"] $ toHtml displayKey
                span_ [class_ " text-slate-600 mx-12 inline-flex items-center"] $ fieldTypeToDisplay field.fieldType


fieldTypeToDisplay :: Fields.FieldTypes -> Html ()
fieldTypeToDisplay fieldType = case fieldType of
  Fields.FTUnknown -> span_ [class_ "px-2 rounded-xl bg-red-100 red-800 monospace"] "unknown"
  Fields.FTString -> span_ [class_ "px-2 rounded-xl bg-fillWeaker slate-800 monospace"] "abc"
  Fields.FTNumber -> span_ [class_ "px-2 rounded-xl bg-blue-100 blue-800 monospace"] "123"
  Fields.FTBool -> span_ [class_ "px-2 rounded-xl bg-gray-100 black-800 monospace"] "bool"
  Fields.FTObject -> span_ [class_ "px-2 rounded-xl bg-orange-100 orange-800 monospace"] "{obj}"
  Fields.FTList -> span_ [class_ "px-2 rounded-xl bg-stone-100 stone-800 monospace"] "[list]"
  Fields.FTNull -> span_ [class_ "px-2 rounded-xl bg-red-100 red-800 monospace"] "null"
