module Pages.BusinessFlowsSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Pool (Pool, withResource)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (addUTCTime, getCurrentTime, getZonedTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection, Only (..))
import Database.PostgreSQL.Simple qualified as PGS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid (renderText)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.LogExplorer.Log qualified as Log
import Pages.Onboarding qualified as Onboarding
import Pages.Projects qualified as CreateProject
import Pages.Projects qualified as ManageMembers
import Pages.Replay qualified as Replay
import Pages.Settings qualified as LemonSqueezy
import Pages.Settings qualified as S3
import Pkg.DeriveUtils (UUIDId (..))
import Pkg.TestUtils hiding (testPid)
import Relude
import Servant.API (ResponseHeader (..), getResponse, lookupResponseHeader)
import Servant.Server qualified as ServantS
import System.Config qualified
import Test.Hspec
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


-- Test context that includes both test resources and a dynamically created project
data TestContext = TestContext
  { tcResources :: TestResources
  , tcProjectId :: Projects.ProjectId
  }


-- Create a new project for testing and provide it along with test resources
withTestProject :: (TestContext -> IO ()) -> IO ()
withTestProject action = withTestResources $ \tr -> do
  headers <- (atAuthToBase tr.trSessAndHeader CreateProject.projectOnboardingH & effToServantHandlerTest tr.trTestClock tr.trUUIDRef tr.trATCtx tr.trLogger tr.trTracerProvider & ServantS.runHandler) <&> fromRightShow
  case lookupResponseHeader @"Location" headers of
    Header location -> do
      let pidText = T.takeWhile (/= '/') $ T.drop 3 location
      case UUID.fromText pidText of
        Just uuid -> do
          -- Refresh session to pick up sudo status and new project
          let projectId = UUIDId uuid
          refreshedSession <- refreshSession tr.trPool (System.Config.hasqlPool tr.trATCtx) tr.trSessAndHeader
          let updatedTr = tr{trSessAndHeader = refreshedSession}
          action $ TestContext updatedTr projectId
        Nothing -> fail $ "Could not parse project ID from location: " <> toString location
    _ -> fail "No Location header in projectOnboarding response"


spec :: Spec
spec = around withTestProject do
  describe "Onboarding Flow" do
    onboardingTests

  describe "Project Settings" do
    settingsTests

  describe "LemonSqueezy Billing" do
    lemonSqueezyWebhookTests

  describe "Stripe Billing" do
    stripeBillingTests

  describe "Replay Session Recording" do
    replayTests

  describe "S3 Configuration" do
    s3ConfigTests


-- | Onboarding Tests - Test through API calls instead of DB queries
onboardingTests :: SpecWith TestContext
onboardingTests =
  describe "should complete all onboarding steps in sequence" do
    it "moves one new project from profile setup to its first queryable event" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
      let infoForm =
            Onboarding.OnboardingInfoForm
              { firstName = "John"
              , lastName = "Doe"
              , companyName = "ACME Corp"
              , companySize = "11 - 25"
              , whereDidYouHearAboutUs = "google"
              }
      (infoHeaders, _) <- testServant tr $ Onboarding.onboardingInfoPostH testPid infoForm
      lookupResponseHeader @"HX-Redirect" infoHeaders `shouldBe` Header ("/p/" <> testPid.toText <> "/onboarding?step=Survey")
      (_, infoResult) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "Info")
      case infoResult of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.InfoStep{..} -> do
            companyName `shouldBe` "ACME Corp"
            companySize `shouldBe` "11 - 25"
            foundUsFrom `shouldBe` "google"
          _ -> fail "Expected InfoStep"

      let surveyForm =
            Onboarding.OnboardingConfForm
              { location = "usa"
              , functionality = ["logs", "analytics"]
              }
      (surveyHeaders, _) <- testServant tr $ Onboarding.onboardingConfPostH testPid surveyForm
      lookupResponseHeader @"HX-Redirect" surveyHeaders `shouldBe` Header ("/p/" <> testPid.toText <> "/onboarding?step=NotifChannel")
      (_, surveyResult) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "Survey")
      case surveyResult of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.SurveyStep{..} -> do
            location `shouldBe` "usa"
            functionality `shouldMatchList` ["logs", "analytics"]
          _ -> fail "Expected SurveyStep"

      let notifForm =
            Onboarding.NotifChannelForm
              { phoneNumber = "+1234567890"
              , emails = ["team@example.com", "alerts@example.com"]
              }
      (_, postResult) <- testServant tr $ Onboarding.phoneEmailPostH testPid notifForm
      case postResult of
        Onboarding.OnboardingPhoneEmailsPost pid emails _ -> do
          pid `shouldBe` testPid
          V.length emails `shouldSatisfy` (> 0) -- Should include at least the submitted emails
      (_, notifResult) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "NotifChannel")
      case notifResult of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.NotifChannelStep{..} -> do
            phoneNumber `shouldBe` "+1234567890"
            V.toList emails `shouldMatchList` ["team@example.com", "alerts@example.com"]
          _ -> fail "Expected NotifChannelStep"

      (emptyHeaders, _) <- testServant tr $ Onboarding.checkIntegrationGet testPid Nothing
      case lookupResponseHeader @"HX-Trigger-After-Settle" emptyHeaders of
        Header triggerHeader -> triggerHeader `shouldSatisfy` T.isInfixOf "No events found yet"
        _ -> fail "Expected HX-Trigger-After-Settle header with error toast"

      apiKey <- createTestAPIKey tr testPid "integration-test-key"
      eventTime <- getCurrentTime
      ingestTrace tr apiKey "onboarding-first-span" eventTime

      (integratedHeaders, _) <- testServant tr $ Onboarding.checkIntegrationGet testPid Nothing
      lookupResponseHeader @"HX-Redirect" integratedHeaders `shouldBe` Header ("/p/" <> testPid.toText <> "/onboarding?step=Pricing")
      (_, integrationResult) <- testServant tr $ Onboarding.onboardingGetH testPid (Just "Integration")
      case integrationResult of
        Onboarding.OnboardingGet (PageCtx _ stepData) -> case stepData of
          Onboarding.IntegrationStep _stepPid stepApiKey -> do
            stepApiKey `shouldNotBe` "<API_KEY>"
          _ -> fail "Expected IntegrationStep"

      let from = toText $ iso8601Show $ addUTCTime (-60) eventTime
          to = toText $ iso8601Show $ addUTCTime 60 eventTime
      (_, events) <- testServant tr $ Log.logExplorerDataH testPid (Just "span_name == \"onboarding-first-span\"") Nothing Nothing Nothing Nothing (Just from) (Just to) Nothing Nothing
      V.length events.logsData `shouldBe` 1


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
      (_, ManageMembers.ManageMembersGet (PageCtx _ (_, members, _, _))) -> do
        V.length members `shouldSatisfy` (>= 0)
      _ -> fail "Expected ManageMembersGet response"

  it "should load billing page" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    result <- testServant tr $ LemonSqueezy.manageBillingGetH testPid
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

-- | Stripe write path round-trips the provider through the model: the Stripe
-- checkout write stores StripeProvider, and Project decodes the new trailing
-- billing_provider column back positionally so projectProvider reads it.
stripeBillingTests :: SpecWith TestContext
stripeBillingTests = do
  it "stores and decodes StripeProvider round-trip" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    _ <- runQueryEffect tr $ Projects.updateStripeProjectBilling testPid (Projects.PlanName "GraduatedPricing") (Projects.SubId "sub_test123") (Projects.SubItemId "si_test") (Projects.CustomerId "cus_test")
    projectM <- runQueryEffect tr $ Projects.projectById testPid
    case projectM of
      Just project -> Projects.projectProvider project `shouldBe` Projects.StripeProvider
      Nothing -> fail "project not found after updateStripeProjectBilling"

  -- Regression: deleting a project used to leave its subscription live, so the
  -- customer kept being billed for a project they could no longer see (a deleted
  -- project billed a full month on the day its trial ended).
  it "deleteProject cancels the project's Stripe subscription" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    _ <- runQueryEffect tr $ Projects.updateStripeProjectBilling testPid (Projects.PlanName "GraduatedPricing") (Projects.SubId "sub_test123") (Projects.SubItemId "si_test") (Projects.CustomerId "cus_test")
    (reqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ CreateProject.deleteProjectGetH testPid
    map fst reqs `shouldContain` ["https://api.stripe.com/v1/subscriptions/sub_test123"]

  it "deleteProject cancels the project's LemonSqueezy subscription" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    _ <- runQueryEffect tr $ Projects.updateProjectPricing testPid (Projects.PlanName "GraduatedPricing") (Projects.SubId "987654") (Projects.SubItemId "si_test") (Projects.OrderId "ord_test") V.empty
    (reqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ CreateProject.deleteProjectGetH testPid
    map fst reqs `shouldContain` ["https://api.lemonsqueezy.com/v1/subscriptions/987654"]

  it "deleteProject issues no cancellation for an unbilled project" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    (reqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ CreateProject.deleteProjectGetH testPid
    map fst reqs `shouldBe` []

  -- One human is one Stripe customer with one trial, however many projects they run.
  -- Previously each project minted its own customer and its own 30-day trial, which
  -- is how one account ended up with two subscriptions on the same card.
  it "userBilling carries a user's customer and trial history across their projects" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    let uid = (getResponse tr.trSessAndHeader).user.id
    fresh <- runQueryEffect tr $ Projects.userBilling uid
    fresh `shouldBe` Projects.UserBilling{stripeCustomerId = Nothing, hasSubscribedBefore = False}

    _ <- runQueryEffect tr $ Projects.updateStripeProjectBilling testPid (Projects.PlanName "GraduatedPricing") (Projects.SubId "sub_first") (Projects.SubItemId "si_first") (Projects.CustomerId "cus_first")
    subscribed <- runQueryEffect tr $ Projects.userBilling uid
    subscribed `shouldBe` Projects.UserBilling{stripeCustomerId = Just "cus_first", hasSubscribedBefore = True}

    -- Deleting that project must not reset either fact, or the next project hands
    -- out a fresh trial and a fresh customer — the exact double-billing shape.
    _ <- runQueryEffect tr $ Projects.deleteProject testPid
    afterDelete <- runQueryEffect tr $ Projects.userBilling uid
    afterDelete `shouldBe` subscribed

  it "checkout reuses the buyer's Stripe customer and withholds a repeat trial" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    let checkout = runAsBaseRecordingHTTP tr . atAuthToBase tr.trSessAndHeader . flip CreateProject.stripeCheckoutInitH (CreateProject.StripeCheckoutForm "GraduatedPricing")
        sessionBody reqs = snd <$> find ((== "https://api.stripe.com/v1/checkout/sessions") . fst) reqs

    -- First ever upgrade: no customer to reuse, trial granted.
    (firstReqs, _) <- checkout testPid
    case sessionBody firstReqs of
      Nothing -> fail "no checkout session request recorded"
      Just b -> do
        decodeUtf8 @Text b `shouldSatisfy` T.isInfixOf "trial_period_days"
        decodeUtf8 @Text b `shouldNotSatisfy` T.isInfixOf "customer="

    -- Once that buyer has any billing history, a further checkout reuses their
    -- customer and withholds the trial — this is what a second project now hits.
    _ <- runQueryEffect tr $ Projects.updateStripeProjectBilling testPid (Projects.PlanName "GraduatedPricing") (Projects.SubId "sub_first") (Projects.SubItemId "si_first") (Projects.CustomerId "cus_first")
    (secondReqs, _) <- checkout testPid
    case sessionBody secondReqs of
      Nothing -> fail "no checkout session request recorded"
      Just b -> do
        decodeUtf8 @Text b `shouldSatisfy` T.isInfixOf "customer=cus_first"
        decodeUtf8 @Text b `shouldNotSatisfy` T.isInfixOf "trial_period_days"

  -- Checkout overwrites sub_id with the new subscription, so a previous one left
  -- running here bills the customer twice with nothing left pointing at it. Pins
  -- that the cancel reads the OLD sub_id, i.e. that it stays ahead of the write.
  it "stripe checkout webhook cancels the subscription the project held before" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    _ <- runQueryEffect tr $ Projects.updateStripeProjectBilling testPid (Projects.PlanName "GraduatedPricing") (Projects.SubId "sub_old") (Projects.SubItemId "si_old") (Projects.CustomerId "cus_test")
    now <- getTestTime tr.trTestClock
    let body =
          BL.toStrict
            $ AE.encode
            $ AE.object
              [ "type" AE..= ("checkout.session.completed" :: Text)
              , "data"
                  AE..= AE.object
                    [ "object"
                        AE..= AE.object
                          [ "client_reference_id" AE..= testPid.toText
                          , "subscription" AE..= ("sub_new" :: Text)
                          , "customer" AE..= ("cus_test" :: Text)
                          ]
                    ]
              ]
        ts = show @Text @Integer $ round $ utcTimeToPOSIXSeconds now
        mac = HMAC.hmac (encodeUtf8 @Text @ByteString tr.trATCtx.env.stripeWebhookSecret) (encodeUtf8 @Text @ByteString ts <> "." <> body) :: HMAC.HMAC SHA256
        sigHeader = "t=" <> ts <> ",v1=" <> decodeUtf8 (B16.encode $ BA.convert mac)
    (reqs, _) <- runAsBaseRecordingHTTP tr $ LemonSqueezy.stripeWebhookPostH (Just sigHeader) body
    map fst reqs `shouldContain` ["https://api.stripe.com/v1/subscriptions/sub_old"]


-- | LemonSqueezy Webhook Tests - Table-driven testing for all webhook events
lemonSqueezyWebhookTests :: SpecWith TestContext
lemonSqueezyWebhookTests = do
  describe "should process webhook events" do
    forM_ webhookTestCases $ \(eventName, testDesc, payloadFn, testFn) ->
      it testDesc $ \TestContext{tcResources = tr, tcProjectId = testPid} -> do
        let payload = payloadFn testPid
        let rawBody = BL.toStrict (AE.encode payload)
        let callWebhook = void $ toBaseServantResponse tr $ LemonSqueezy.webhookPostH Nothing rawBody
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
        -- Phase A: the LemonSqueezy webhook stores the provider explicitly (no read-time guessing).
        provider <- withResource pool \conn -> PGS.query conn [sql|SELECT billing_provider FROM projects.projects WHERE id = ?|] (Only testPid)
        case provider of
          [Only (bp :: Text)] -> bp `shouldBe` "lemon_squeezy_provider"
          _ -> fail "Failed to query billing_provider"
    )
  ,
    ( "subscription_cancelled"
    , "should downgrade to free on subscription_cancelled event"
    , createWebhookPayload "subscription_cancelled"
    , \testPid pool callWebhook -> do
        setupProjectWithSubscription pool testPid "GraduatedPricing"
        _ <- callWebhook
        verifyPaymentPlan pool testPid "Free"
    )
  ,
    ( "subscription_resumed"
    , "should upgrade to paid on subscription_resumed event"
    , createWebhookPayload "subscription_resumed"
    , \testPid pool callWebhook -> do
        _ <- withResource pool \conn -> PGS.execute conn [sql|UPDATE projects.projects SET payment_plan = 'Free', order_id = '67890', sub_id = '12345', first_sub_item_id = '111' WHERE id = ?|] (Only testPid)
        _ <- callWebhook
        paymentPlan <- withResource pool \conn -> PGS.query conn [sql|SELECT payment_plan FROM projects.projects WHERE id = ?|] (Only testPid)
        case paymentPlan of
          [Only (plan :: Text)] -> plan `shouldNotBe` "Free"
          _ -> fail "Failed to query payment plan"
    )
  ,
    ( "subscription_expired"
    , "should downgrade to free on subscription_expired event"
    , createWebhookPayload "subscription_expired"
    , \testPid pool callWebhook -> do
        setupProjectWithSubscription pool testPid "GraduatedPricing"
        _ <- callWebhook
        verifyPaymentPlan pool testPid "Free"
    )
  ]


setupProjectWithSubscription :: Pool Connection -> Projects.ProjectId -> Text -> IO ()
setupProjectWithSubscription pool testPid plan = do
  subId <- Projects.LemonSubId <$> UUIDV4.nextRandom
  currentZonedTime <- getZonedTime
  _ <- withResource pool \conn ->
    PGS.execute
      conn
      [sql|
    INSERT INTO apis.subscriptions (id, created_at, updated_at, project_id, order_id, subscription_id, first_sub_id, product_name, user_email)
    VALUES (?, ?, ?, ?, 12345, 67890, 111, 'Test Plan', 'test@example.com')
    ON CONFLICT (subscription_id) DO UPDATE SET project_id=EXCLUDED.project_id, product_name=EXCLUDED.product_name
  |]
      (subId, currentZonedTime, currentZonedTime, testPid.toText)
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
            , userId = Nothing
            , userEmail = Nothing
            , userName = Nothing
            }

    result <- toBaseServantResponse tr $ Replay.replayPostH testPid replayData

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
            , userId = Nothing
            , userEmail = Nothing
            , userName = Nothing
            }

    result <- toBaseServantResponse tr $ Replay.replayPostH testPid replayData

    case result of
      AE.Object obj -> do
        case KM.lookup "status" obj of
          Just (AE.String status) -> status `shouldSatisfy` (`elem` ["ok", "warning"])
          _ -> fail "Missing or invalid status field"
      _ -> fail "Expected Object response"

  it "should retrieve replay session data" \TestContext{tcResources = tr, tcProjectId = testPid} -> do
    sessionId <- liftIO UUIDV4.nextRandom

    (_, result) <- testServant tr $ Replay.replaySessionGetH testPid sessionId

    -- Decode the serialized envelope (ReplaySessionResp splices events as raw bytes).
    case AE.decode (AE.encode result) of
      Just (AE.Object obj) -> case KM.lookup "events" obj of
        Just (AE.Array _) -> pass -- Valid events array
        _ -> fail "Missing or invalid events field"
      _ -> fail "Expected Object response"


-- | S3 settings use the same real MinIO instance as the replay integration tier.
s3ConfigTests :: SpecWith TestContext
s3ConfigTests =
  it "connects real S3 storage, renders it, and removes it" \TestContext{tcResources = tr, tcProjectId = testPid} ->
    requireMinio tr pendingWith do
      -- The endpoint/credentials must be the ones `withTestResources` actually probed
      -- (MINIO_ENDPOINT is http://minio:9000 in CI, 127.0.0.1:9000 locally); hardcoding
      -- either makes the handler's bucketExists fail and render "Not connected".
      let env = tr.trATCtx.env
          s3Form =
            Projects.ProjectS3Bucket
              { accessKey = env.s3AccessKey
              , secretKey = env.s3SecretKey
              , region = env.s3Region
              , bucket = env.s3Bucket
              , endpointUrl = env.s3Endpoint
              }
      (_, connected) <- testServant tr $ S3.brings3PostH testPid s3Form
      TL.toStrict (renderText connected) `shouldSatisfy` T.isInfixOf "Connected"

      (_, savedPage) <- testServant tr $ S3.bringS3GetH testPid
      let savedHtml = TL.toStrict $ renderText savedPage
      savedHtml `shouldSatisfy` T.isInfixOf env.s3Bucket
      savedHtml `shouldSatisfy` T.isInfixOf "Connected"

      (_, removed) <- testServant tr $ S3.brings3RemoveH testPid
      TL.toStrict (renderText removed) `shouldSatisfy` T.isInfixOf "Not connected"
      project <- runTestBg frozenTime tr (Projects.projectById testPid) >>= maybe (fail "the project was not found") pure
      project.s3Bucket `shouldSatisfy` isNothing
