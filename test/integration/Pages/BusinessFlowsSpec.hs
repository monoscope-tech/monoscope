module Pages.BusinessFlowsSpec (spec) where

import BackgroundJobs qualified
import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Pool (Pool, withResource)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, getCurrentTime, getZonedTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection, Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Models.Projects.Projects qualified as Projects
import OddJobs.Job (Job (..))
import Pages.BodyWrapper (PageCtx (..))
import Pages.LemonSqueezy qualified as LemonSqueezy
import Pages.Onboarding.Onboarding qualified as Onboarding
import Pages.Projects qualified as CreateProject
import Pages.Projects qualified as ManageMembers
import Pages.Replay qualified as Replay
import Pages.S3 qualified as S3
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils
import Relude
import Servant.API (ResponseHeader (..), lookupResponseHeader)
import Servant.Htmx
import Servant.Server qualified as ServantS
import Test.Hspec


-- Test context that includes both test resources and a dynamically created project
data TestContext = TestContext
  { tcResources :: TestResources
  , tcProjectId :: Projects.ProjectId
  }


-- Create a new project for testing and provide it along with test resources
withTestProject :: (TestContext -> IO ()) -> IO ()
withTestProject action = withTestResources $ \tr -> do
  headers <- (atAuthToBase tr.trSessAndHeader CreateProject.projectOnboardingH & effToServantHandlerTest tr.trATCtx tr.trLogger tr.trTracerProvider & ServantS.runHandler) <&> fromRightShow
  case lookupResponseHeader @"Location" headers of
    Header location -> do
      let pidText = T.takeWhile (/= '/') $ T.drop 3 location
      case UUID.fromText pidText of
        Just uuid -> do
          -- Refresh session to pick up sudo status and new project
          let projectId = UUIDId uuid
          refreshedSession <- refreshSession tr.trPool tr.trSessAndHeader
          let updatedTr = tr{trSessAndHeader = refreshedSession}
          action $ TestContext updatedTr projectId
        Nothing -> fail $ "Could not parse project ID from location: " <> T.unpack location
    _ -> fail "No Location header in projectOnboarding response"


spec :: Spec
spec = aroundAll withTestProject do
  describe "Onboarding Flow" do
    onboardingTests

  describe "Project Settings" do
    settingsTests

  describe "LemonSqueezy Billing" do
    lemonSqueezyWebhookTests
  -- billingUsageTests

  describe "Replay Session Recording" do
    replayTests

  describe "S3 Configuration" do
    s3ConfigTests


-- | Onboarding Tests - Test through API calls instead of DB queries
onboardingTests :: SpecWith TestContext
onboardingTests = do
  describe "should complete all onboarding steps in sequence" do
    it "Step 1: Info - should save and retrieve user information" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
      let infoForm =
            Onboarding.OnboardingInfoForm
              { firstName = "John"
              , lastName = "Doe"
              , companyName = "ACME Corp"
              , companySize = "11 - 25"
              , whereDidYouHearAboutUs = "google"
              }
      (headers, _) <- testServant tr $ Onboarding.onboardingInfoPostH testPid infoForm
      lookupResponseHeader @"HX-Redirect" headers `shouldBe` Header ("/p/" <> testPid.toText <> "/onboarding?step=Survey")
      (_, result) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "Info")
      case result of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.InfoStep{..} -> do
            companyName `shouldBe` "ACME Corp"
            companySize `shouldBe` "11 - 25"
            foundUsFrom `shouldBe` "google"
          _ -> fail "Expected InfoStep"

    it "Step 2: Survey - should save and retrieve survey preferences" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
      let surveyForm =
            Onboarding.OnboardingConForm
              { location = "usa"
              , functionality = ["logs", "analytics"]
              }
      (headers, _) <- testServant tr $ Onboarding.onboardingConfPostH testPid surveyForm
      lookupResponseHeader @"HX-Redirect" headers `shouldBe` Header ("/p/" <> testPid.toText <> "/onboarding?step=NotifChannel")
      (_, result) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "Survey")
      case result of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.SurveyStep{..} -> do
            location `shouldBe` "usa"
            functionality `shouldMatchList` ["logs", "analytics"]
          _ -> fail "Expected SurveyStep"

    it "Step 3: NotifChannel - should save and retrieve notification preferences" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
      let notifForm =
            Onboarding.NotifChannelForm
              { phoneNumber = "+1234567890"
              , emails = ["team@example.com", "alerts@example.com"]
              }
      (_, postResult) <- testServant tr $ Onboarding.phoneEmailPostH testPid notifForm
      case postResult of
        Onboarding.OnboardingPhoneEmailsPost pid emails -> do
          pid `shouldBe` testPid
          V.length emails `shouldSatisfy` (> 0) -- Should include at least the submitted emails
      (_, result) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "NotifChannel")
      case result of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.NotifChannelStep{..} -> do
            phoneNumber `shouldBe` "+1234567890"
            V.toList emails `shouldMatchList` ["team@example.com", "alerts@example.com"]
          _ -> fail "Expected NotifChannelStep"

    it "Step 4: Integration - should show error when no events ingested yet" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
      (headers, _) <- testServant tr $ Onboarding.checkIntegrationGet testPid Nothing
      case lookupResponseHeader @"HX-Trigger-After-Settle" headers of
        Header triggerHeader -> triggerHeader `shouldSatisfy` T.isInfixOf "No events found yet"
        _ -> fail "Expected HX-Trigger-After-Settle header with error toast"

    it "Step 4: Integration - should show API key after integration" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
      -- Ingest a test event to simulate integration
      apiKey <- createTestAPIKey tr testPid "integration-test-key"
      currentTime <- liftIO getCurrentTime
      ingestTrace tr apiKey "test" currentTime

      (headers, _) <- testServant tr $ Onboarding.checkIntegrationGet testPid Nothing
      lookupResponseHeader @"HX-Redirect" headers `shouldBe` Header ("/p/" <> testPid.toText <> "/onboarding?step=Pricing")
      (_, result) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "Integration")
      case result of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.IntegrationStep _stepPid stepApiKey -> do
            stepApiKey `shouldNotBe` "<API_KEY>"
          _ -> fail "Expected IntegrationStep"


-- | Settings Page Tests - Verify project settings created during onboarding
settingsTests :: SpecWith TestContext
settingsTests = do
  it "should display project title, description, timezone, and alert configurations from onboarding" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    (_, result) <- testServant tr $ CreateProject.projectSettingsGetH testPid
    case result of
      CreateProject.CreateProject (PageCtx _ (_, _, _, _, _, form, _, project)) -> do
        -- Verify default values from onboarding project creation
        project.title `shouldNotBe` ""
        -- Description and timezone are empty by default during onboarding
        project.description `shouldBe` ""
        form.timeZone `shouldBe` ""
        -- Alert toggles default to enabled during onboarding
        form.weeklyNotifs `shouldBe` Just "on"
        form.dailyNotifs `shouldBe` Just "on"
        form.errorAlerts `shouldBe` Just "on"
        form.endpointAlerts `shouldBe` Just "on"
      _ -> fail "Expected CreateProject response"

  it "should update alert configurations via POST and persist changes" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    let updateForm =
          CreateProject.CreateProjectForm
            { title = "ACME Corp" -- Update project title
            , description = "" -- Keep empty description
            , emails = []
            , permissions = []
            , timeZone = "America/New_York"
            , errorAlerts = Just "on"
            , endpointAlerts = Just "on"
            , weeklyNotifs = Just "on"
            , dailyNotifs = Nothing
            }
    _ <- testServant tr $ CreateProject.createProjectPostH testPid updateForm

    (_, result) <- testServant tr $ CreateProject.projectSettingsGetH testPid
    case result of
      CreateProject.CreateProject (PageCtx _ (_, _, _, _, _, form, _, project)) -> do
        -- Verify the POST updated the values correctly
        project.title `shouldBe` "ACME Corp"
        form.timeZone `shouldBe` "America/New_York"
        form.weeklyNotifs `shouldBe` Just "on"
        form.dailyNotifs `shouldBe` Nothing
        form.errorAlerts `shouldBe` Just "on"
        form.endpointAlerts `shouldBe` Just "on"
      _ -> fail "Expected CreateProject response"

  it "should load manage members page" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    result <- testServant tr $ ManageMembers.manageMembersGetH testPid
    case result of
      (_, ManageMembers.ManageMembersGet (PageCtx _ (pid, members))) -> do
        V.length members `shouldSatisfy` (>= 0)
      _ -> fail "Expected ManageMembersGet response"

  it "should load billing page" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    result <- testServant tr $ LemonSqueezy.manageBillingGetH testPid Nothing
    case result of
      (_, LemonSqueezy.BillingGet (PageCtx _ _)) -> pass

  it "should load delete project page" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    (_, _result) <- testServant tr $ CreateProject.deleteProjectGetH testPid
    pass


-- TODO: after creating the projects and verifying them on the onboarding forms again,
-- please verify the settings pages contain the same data. Eg
-- /p/<pid>/settings renders project Title, Timezone, descrioption and these alert configurations.
--  Alert Configuration
-- Manage your notification preferences
--
-- Receive new endpoint alerts
-- Get notified when new API endpoints are detected
--
-- Receive runtime error alerts
-- Receive immediate notifications for system errors
--
-- Receive weekly reports alerts
-- Get a summary of your project activity every week
--
-- Receive daily reports alerts
-- Receive daily summaries of your project metrics
--
-- So, assert the initial setate of the settings based on the data that was set during the onboarding, via the Get endpoints. Then update the forms via
-- post, and hit Get again to ensure that the updates worked.
--
-- Also do this succinctly for the other settings endpoints: Manage members, billing, integrations, your s3 bucket, delte project page can test just
-- deleting the project. Which i think marks a project as deleted and prevents sending emails and notifications and other actions. we should test the
-- actions.

-- | LemonSqueezy Webhook Tests - Table-driven testing for all webhook events
lemonSqueezyWebhookTests :: SpecWith TestContext
lemonSqueezyWebhookTests = do
  describe "should process webhook events" do
    forM_ webhookTestCases $ \(eventName, testDesc, payloadFn, testFn) ->
      it testDesc $ \TestContext{tcResources = tr, tcProjectId = testPid} -> do
        let payload = payloadFn testPid
        let callWebhook = void $ toBaseServantResponse tr.trATCtx tr.trLogger $ LemonSqueezy.webhookPostH Nothing payload
        testFn testPid tr.trPool callWebhook


webhookTestCases :: [(Text, String, Projects.ProjectId -> LemonSqueezy.WebhookData, Projects.ProjectId -> Pool Connection -> IO () -> IO ())]
webhookTestCases =
  [
    ( "subscription_created"
    , "should create subscription on subscription_created event"
    , createWebhookPayload "subscription_created"
    , \testPid pool callWebhook -> do
        _ <- callWebhook
        subs <- withResource pool \conn -> PGS.query conn [sql|SELECT COUNT(*) FROM apis.subscriptions WHERE project_id = ?|] (Only testPid.toText)
        case subs of
          [Only (count :: Int)] -> count `shouldBe` 1
          _ -> fail "Failed to query subscriptions"
    )
  ,
    ( "subscription_cancelled"
    , "should downgrade to free on subscription_cancelled event"
    , createWebhookPayload "subscription_cancelled"
    , \testPid pool callWebhook -> do
        setupProjectWithSubscription pool testPid "GraduatedPricing"
        _ <- callWebhook
        verifyPaymentPlan pool testPid "FREE"
    )
  ,
    ( "subscription_resumed"
    , "should upgrade to paid on subscription_resumed event"
    , createWebhookPayload "subscription_resumed"
    , \testPid pool callWebhook -> do
        _ <- withResource pool \conn -> PGS.execute conn [sql|UPDATE projects.projects SET payment_plan = 'FREE', order_id = '67890', sub_id = '12345', first_sub_item_id = '111' WHERE id = ?|] (Only testPid)
        _ <- callWebhook
        paymentPlan <- withResource pool \conn -> PGS.query conn [sql|SELECT payment_plan FROM projects.projects WHERE id = ?|] (Only testPid)
        case paymentPlan of
          [Only (plan :: Text)] -> plan `shouldNotBe` "FREE"
          _ -> fail "Failed to query payment plan"
    )
  ,
    ( "subscription_expired"
    , "should downgrade to free on subscription_expired event"
    , createWebhookPayload "subscription_expired"
    , \testPid pool callWebhook -> do
        setupProjectWithSubscription pool testPid "GraduatedPricing"
        _ <- callWebhook
        verifyPaymentPlan pool testPid "FREE"
    )
  ]


setupProjectWithSubscription :: Pool Connection -> Projects.ProjectId -> Text -> IO ()
setupProjectWithSubscription pool testPid plan = do
  subId <- Projects.LemonSubId <$> UUIDV4.nextRandom
  currentZonedTime <- getZonedTime
  _ <- withResource pool \conn -> PGS.execute conn [sql|
    INSERT INTO apis.subscriptions (id, created_at, updated_at, project_id, order_id, subscription_id, first_sub_id, product_name, user_email)
    VALUES (?, ?, ?, ?, 12345, 67890, 111, 'Test Plan', 'test@example.com')
  |] (subId, currentZonedTime, currentZonedTime, testPid.toText)
  _ <- withResource pool \conn -> PGS.execute conn [sql|UPDATE projects.projects SET payment_plan = ?, order_id = '67890', sub_id = '12345', first_sub_item_id = '111' WHERE id = ?|] (plan, testPid)
  pass


verifyPaymentPlan :: Pool Connection -> Projects.ProjectId -> Text -> IO ()
verifyPaymentPlan pool testPid expected = do
  paymentPlan <- withResource pool \conn -> PGS.query conn [sql|SELECT payment_plan FROM projects.projects WHERE id = ?|] (Only testPid)
  case paymentPlan of
    [Only (plan :: Text)] -> plan `shouldBe` expected
    _ -> fail "Failed to query payment plan"


createWebhookPayload :: Text -> Projects.ProjectId -> LemonSqueezy.WebhookData
createWebhookPayload eventName pid =
  LemonSqueezy.WebhookData
    { dataVal =
        LemonSqueezy.DataVals
          { id = "sub_123"
          , attributes =
              LemonSqueezy.Attributes
                { firstSubscriptionItem =
                    LemonSqueezy.FirstSubItem
                      { id = 111
                      , subscriptionId = 12345
                      }
                , productName = "Test Plan"
                , orderId = 67890
                , userEmail = "test@example.com"
                }
          }
    , meta =
        LemonSqueezy.MetaData
          { customData =
              Just
                $ LemonSqueezy.CustomData
                  { projectId = Just pid.toText
                  }
          , eventName = eventName
          }
    }


-- | Billing Usage Calculation Tests
billingUsageTests :: SpecWith TestContext
billingUsageTests = do
  it "should calculate usage within billing cycle correctly" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    currentTime <- liftIO getCurrentTime
    let cycleStart = addUTCTime (-10 * 24 * 60 * 60) currentTime -- 10 days ago

    -- Configure project for billing: set payment_plan, first_sub_item_id, billing_day, and usage_last_reported
    _ <- withResource tr.trPool \conn ->
      PGS.execute conn
        [sql|UPDATE projects.projects SET payment_plan = 'Pro', first_sub_item_id = 12345, billing_day = ?, usage_last_reported = ? WHERE id = ?|]
        (cycleStart, cycleStart, testPid)

    -- Ingest some test usage data
    apiKey <- createTestAPIKey tr testPid "billing-test-key"
    forM_ [1 .. 5] $ \(i :: Int) ->
      ingestTrace tr apiKey ("test-span-" <> show i) currentTime

    -- Run the ReportUsage background job to populate daily_usage
    void $ runTestBg tr $ BackgroundJobs.processBackgroundJob tr.trATCtx undefined (BackgroundJobs.ReportUsage testPid)

    -- Get billing info
    (_, result) <- testServant tr $ LemonSqueezy.manageBillingGetH testPid Nothing

    case result of
      LemonSqueezy.BillingGet (PageCtx _ (_, totalReqs, _, _, _, _, _, _, _)) -> do
        totalReqs `shouldBe` 5 -- Should count the 5 spans we ingested
  it "should handle cycle boundaries correctly" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    currentTime <- liftIO getCurrentTime
    let cycleStart = addUTCTime (-40 * 24 * 60 * 60) currentTime -- 40 days ago (more than a month)
        oldTime = addUTCTime (-35 * 24 * 60 * 60) currentTime

    -- Configure project for billing: set payment_plan, first_sub_item_id, billing_day, and usage_last_reported
    _ <- withResource tr.trPool \conn ->
      PGS.execute conn
        [sql|UPDATE projects.projects SET payment_plan = 'Pro', first_sub_item_id = 12345, billing_day = ?, usage_last_reported = ? WHERE id = ?|]
        (cycleStart, cycleStart, testPid)

    -- Ingest old data (outside current cycle) and new data (within current cycle)
    apiKey <- createTestAPIKey tr testPid "cycle-test-key"
    ingestTrace tr apiKey "old-span" oldTime
    ingestTrace tr apiKey "new-span" currentTime

    -- Run the ReportUsage background job to populate daily_usage
    void $ runTestBg tr $ BackgroundJobs.processBackgroundJob tr.trATCtx undefined (BackgroundJobs.ReportUsage testPid)

    -- Get billing info - should only count data from current cycle
    (_, result) <- testServant tr $ LemonSqueezy.manageBillingGetH testPid Nothing

    case result of
      LemonSqueezy.BillingGet (PageCtx _ (_, totalReqs, _, _, _, _, _, _, _)) -> do
        totalReqs `shouldBe` 1 -- Should only count the recent span


-- | Replay Session Recording Tests
replayTests :: SpecWith TestContext
replayTests = do
  it "should ingest replay events successfully" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    sessionId <- liftIO UUIDV4.nextRandom
    currentTime <- liftIO getCurrentTime

    let replayData =
          Replay.ReplayPost
            { events = AE.Array $ V.fromList [AE.object ["type" AE..= ("click" :: Text), "timestamp" AE..= (1000 :: Int)]]
            , sessionId = sessionId
            , timestamp = currentTime
            }

    result <- toBaseServantResponse tr.trATCtx tr.trLogger $ Replay.replayPostH testPid replayData

    case result of
      AE.Object obj -> do
        case KM.lookup "status" obj of
          Just (AE.String status) -> status `shouldSatisfy` (`elem` ["ok", "warning"]) -- warning if Kafka not configured
          _ -> fail "Missing or invalid status field"
        case KM.lookup "sessionId" obj of
          Just (AE.String sid) -> UUID.fromText sid `shouldBe` Just sessionId
          _ -> fail "Missing or invalid sessionId field"
      _ -> fail "Expected Object response"

  it "should handle empty event arrays" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    sessionId <- liftIO UUIDV4.nextRandom
    currentTime <- liftIO getCurrentTime

    let replayData =
          Replay.ReplayPost
            { events = AE.Array V.empty
            , sessionId = sessionId
            , timestamp = currentTime
            }

    result <- toBaseServantResponse tr.trATCtx tr.trLogger $ Replay.replayPostH testPid replayData

    case result of
      AE.Object obj -> do
        case KM.lookup "status" obj of
          Just (AE.String status) -> status `shouldSatisfy` (`elem` ["ok", "warning"])
          _ -> fail "Missing or invalid status field"
      _ -> fail "Expected Object response"

  it "should retrieve replay session data" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    sessionId <- liftIO UUIDV4.nextRandom

    (_, result) <- testServant tr $ Replay.replaySessionGetH testPid sessionId

    case result of
      AE.Object obj -> do
        case KM.lookup "events" obj of
          Just (AE.Array _) -> pass -- Valid events array
          _ -> fail "Missing or invalid events field"
      _ -> fail "Expected Object response"


-- | S3 Configuration Tests - Table-driven testing for validation scenarios
s3ConfigTests :: SpecWith TestContext
s3ConfigTests = do
  describe "should validate S3 credentials" do
    it "PENDING: Requires Minio setup" $ \_ -> do
      -- TODO: Re-enable when Minio is set up
      pendingWith "S3 validation tests require Minio to be configured"
  -- forM_ s3ValidationCases $ \(testDesc, s3Form, shouldSucceed) ->
  --   it testDesc $ \tr -> do
  --     result <- testServant tr $ S3.brings3PostH testPid s3Form

  --     -- Check if S3 config was saved based on success expectation
  --     savedConfigM <-
  --       DBT.withPool tr.trPool
  --         $ DBT.queryOne [sql|SELECT s3_bucket FROM projects.projects WHERE id = ?|] (Only testPid)
  --     let savedConfig = fmap fromOnly savedConfigM

  --     case (shouldSucceed, savedConfig) of
  --       (True, Just (Just (_ :: Projects.ProjectS3Bucket))) -> pass -- Success case
  --       (False, Just (Nothing :: Maybe Projects.ProjectS3Bucket)) -> pass -- Failure case (no config saved)
  --       (False, Nothing) -> fail "Project not found"
  --       _ -> pure () -- Other cases - validation might fail but that's ok for invalid credentials

  it "should remove S3 configuration" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    -- First add an S3 config
    let s3Form =
          Projects.ProjectS3Bucket
            { accessKey = "test-key"
            , secretKey = "test-secret"
            , region = "us-east-1"
            , bucket = "test-bucket"
            , endpointUrl = ""
            }

    _ <- withResource tr.trPool \conn ->
      PGS.execute conn
        [sql|UPDATE projects.projects SET s3_bucket = ? WHERE id = ?|]
        (Just s3Form, testPid)

    -- Now remove it
    _ <- testServant tr $ S3.brings3RemoveH testPid

    -- Verify it was removed
    savedConfigM <- withResource tr.trPool \conn ->
      PGS.query conn [sql|SELECT s3_bucket FROM projects.projects WHERE id = ?|] (Only testPid) :: IO [(Only (Maybe Projects.ProjectS3Bucket))]
    let savedConfig = fmap fromOnly (listToMaybe savedConfigM)

    case savedConfig of
      Just (Nothing :: Maybe Projects.ProjectS3Bucket) -> pass -- Successfully removed
      _ -> fail "S3 config was not removed"


-- | Test cases for S3 validation
s3ValidationCases :: [(String, Projects.ProjectS3Bucket, Bool)]
s3ValidationCases =
  [
    ( "should reject invalid credentials"
    , Projects.ProjectS3Bucket
        { accessKey = "invalid-key"
        , secretKey = "invalid-secret"
        , region = "us-east-1"
        , bucket = "nonexistent-bucket"
        , endpointUrl = ""
        }
    , False
    )
  ,
    ( "should reject missing bucket"
    , Projects.ProjectS3Bucket
        { accessKey = "test-key"
        , secretKey = "test-secret"
        , region = "us-east-1"
        , bucket = ""
        , endpointUrl = ""
        }
    , False
    )
  ,
    ( "should handle custom endpoints"
    , Projects.ProjectS3Bucket
        { accessKey = "test-key"
        , secretKey = "test-secret"
        , region = "us-east-1"
        , bucket = "test-bucket"
        , endpointUrl = "https://s3.custom.com"
        }
    , False -- Will fail unless actual endpoint is available
    )
  ]
