-- | Billing end to end: onboarding to the first event and a plan → project settings and
-- storage → Stripe and Lemon Squeezy subscriptions (checkout, dunning, lapse, reactivation,
-- deletion) → ReportUsage metering per dimension → the free tier's cap and sampling.
--
-- Every outbound Stripe/Lemon Squeezy call runs under a recording interpreter or a fixture:
-- the golden interpreter makes a REAL request on a cache miss. The one golden-served state is
-- the billing page of a GraduatedPricing project on sub_test123.
module BillingSpec (spec) where

import BackgroundJobs qualified as BJ
import Control.Lens ((%~), (.~), (^.), _1)
import Data.Aeson qualified as AE
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.Cache qualified as Cache
import Data.Effectful.Wreq (HTTP, Options)
import Data.HashMap.Strict qualified as HM
import Data.List (lookup)
import Data.List qualified as L
import Data.Pool (withResource)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.Text qualified as T
import Data.Time (Day, UTCTime (..), addUTCTime, fromGregorian, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PGS
import Effectful.Dispatch.Dynamic (interpose)
import Effectful.Error.Static (catchError)
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Network.HTTP.Types.URI (parseSimpleQuery)
import OpenTelemetry.Instrumentation.Hasql qualified as OHasql
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (PageCtx (..))
import Pages.Dashboards qualified as Dashboards
import Pages.LogExplorer.Log qualified as Log
import Pages.Monitors qualified as Monitors
import Pages.Onboarding qualified as Onboarding
import Pages.Projects qualified as CreateProject
import Pages.Settings qualified as Settings
import Pkg.DeriveUtils (mkHasqlPool)
import Pkg.IngestBudget qualified as IngestBudget
import Pkg.TestUtils
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService qualified as LS
import Proto.Opentelemetry.Proto.Collector.Logs.V1.LogsService_Fields qualified as LSF
import Proto.Opentelemetry.Proto.Collector.Metrics.V1.MetricsService_Fields qualified as MSF
import Proto.Opentelemetry.Proto.Collector.Trace.V1.TraceService_Fields qualified as TSF
import Proto.Opentelemetry.Proto.Trace.V1.Trace qualified as PT
import Proto.Opentelemetry.Proto.Trace.V1.Trace_Fields qualified as PTF
import Servant.API (ResponseHeader (..), getResponse, lookupResponseHeader)
import Servant.API.UVerb (matchUnion)
import Servant.Server qualified as ServantS
import System.Config qualified as Config
import System.Types (RespHeaders)
import System.IO.Error (userError)
import UnliftIO.Exception (bracket, throwIO)
import System.Types (Done (..))
import "cryptonite" Crypto.Hash (SHA256)
import "cryptonite" Crypto.MAC.HMAC qualified as HMAC


spec :: Spec
spec = around withTestResources do
  it "onboarding walks one project from profile to its first event, a plan and every activation milestone" \tr0 -> do
    (tr, pid) <- onboarded tr0
    let step s = call tr (Onboarding.onboardingGetH pid (Just s)) <&> \(Onboarding.OnboardingGet (PageCtx _ d)) -> d
        redirect h = lookupResponseHeader @"HX-Redirect" h
        trigger h = lookupResponseHeader @"HX-Trigger" h
        noRedirect = MissingHeader :: ResponseHeader "HX-Redirect" Text
        infoStep =
          step Onboarding.Info >>= \case
            Onboarding.InfoStep{..} -> (companyName, companySize, foundUsFrom) `shouldBe` ("ACME Corp", "11 - 25", "google")
            _ -> expectationFailure "expected InfoStep"
    (infoHeaders, _) <- testServant tr $ Onboarding.onboardingInfoPostH pid Onboarding.OnboardingInfoForm{firstName = "John", lastName = "Doe", companyName = "ACME Corp", companySize = "11 - 25", whereDidYouHearAboutUs = "google"}
    redirect infoHeaders `shouldBe` Header ("/p/" <> pid.toText <> "/onboarding?step=Survey")
    infoStep
    (surveyHeaders, _) <- testServant tr $ Onboarding.onboardingConfPostH pid Onboarding.OnboardingConfForm{location = "usa", functionality = ["logs", "analytics"]}
    redirect surveyHeaders `shouldBe` Header ("/p/" <> pid.toText <> "/onboarding?step=NotifChannel")
    step Onboarding.Survey >>= \case
      Onboarding.SurveyStep{..} -> (location, sort functionality) `shouldBe` ("usa", ["analytics", "logs"])
      _ -> expectationFailure "expected SurveyStep"
    -- The survey merges into the same `questions` blob (a jsonb `||`), so the info answers survive it.
    infoStep
    Onboarding.OnboardingPhoneEmailsPost postedPid postedEmails _ <- call tr $ Onboarding.phoneEmailPostH pid Onboarding.NotifChannelForm{phoneNumber = "+1234567890", emails = ["team@example.com", "alerts@example.com"]}
    -- The signed-in member is always included.
    (postedPid, sort (V.toList postedEmails)) `shouldBe` (pid, ["alerts@example.com", "team@example.com", "test@monoscope.tech"])
    step Onboarding.NotifChannel >>= \case
      Onboarding.NotifChannelStep{..} -> (phoneNumber, sort (V.toList emails)) `shouldBe` ("+1234567890", ["alerts@example.com", "team@example.com"])
      _ -> expectationFailure "expected NotifChannelStep"

    -- No events yet: the automatic check stays silent and swaps in a fresh poller; Confirm & Proceed toasts.
    let checked lang = runAuthHandler tr (Onboarding.checkIntegrationGet pid lang) >>= maybe (fail "integration check refused") (\h -> let Done c = getResponse h in pure (h, c)) . matchUnion @(RespHeaders (Done Onboarding.IntegrationCheck))
    (automatic, waiting) <- checked (Just "Javascript")
    (waiting, trigger automatic, redirect automatic) `shouldBe` (Onboarding.AwaitingEvents pid "Javascript", Header ("{}" :: Text), noRedirect)
    goldenHtml "onboarding-integration-waiting" waiting
    call tr (Onboarding.checkIntegrationGet pid Nothing) `shouldReturn` Left Onboarding.NoEventsYet

    apiKey <- createTestAPIKey tr pid "integration-test-key"
    eventTime <- getCurrentTime
    ingestTrace tr apiKey "onboarding-first-span" eventTime
    -- The first verification carries the project's one-time ad activation instead of a toast.
    (verifiedHeaders, verified) <- checked (Just "Javascript")
    (verified, trigger verifiedHeaders, redirect verifiedHeaders) `shouldBe` (Onboarding.Verified, Header ("{\"adConversion\":{\"value\":[{\"id\":\"" <> pid.toText <> "\",\"name\":\"activated\"}]}}" :: Text), noRedirect)
    goldenHtml "onboarding-integration-verified" verified
    -- TF-only deployments write no Postgres telemetry: an event behind the "timefusion" pool
    -- (a schema search_path routes otel_logs_and_spans to) still verifies a project with none.
    tfOnly <- createTestProject tr "tf-only"
    db <- dbOne @_ @Text tr [sql|CREATE SCHEMA tf_only; CREATE TABLE tf_only.otel_logs_and_spans (project_id text, context___span_id text); INSERT INTO tf_only.otel_logs_and_spans VALUES (?, 'span-1'); SELECT current_database()|] (Only tfOnly.toText)
    host <- fromMaybe "localhost" <$> lookupEnv "DB_HOST"
    bracket (mkHasqlPool 1 $ encodeUtf8 $ "host=" <> toText host <> " user=postgres password=postgres dbname=" <> db <> " options='-c search_path=tf_only'") OHasql.release \tfPool -> do
      let ctx = tr.trATCtx
      Right tfVerified <- call tr{trATCtx = ctx{Config.hasqlTimefusionPool = tfPool, Config.env = ctx.env{Config.enableTimefusionReads = True}}} $ Onboarding.checkIntegrationGet tfOnly (Just "Javascript")
      goldenHtml "onboarding-integration-verified" tfVerified
    (integratedHeaders, integrated) <- checked Nothing
    (integrated, redirect integratedHeaders) `shouldBe` (Onboarding.ProceedToPricing pid, Header ("/p/" <> pid.toText <> "/onboarding?step=Pricing"))
    step Onboarding.Integration >>= \case
      Onboarding.IntegrationStep _ stepApiKey -> stepApiKey `shouldNotBe` "<API_KEY>"
      _ -> expectationFailure "expected IntegrationStep"
    let around' d = toText $ iso8601Show $ addUTCTime d eventTime
    events <- call tr $ Log.logExplorerDataH pid def{Log.query = Just "span_name == \"onboarding-first-span\"", Log.from = Just (around' (-60)), Log.to = Just (around' 60)}
    V.length events.logsData `shouldBe` 1

    -- The activation funnel: only the four privacy-safe milestones, reached through public handlers.
    _ <- testServant tr $ Dashboards.dashboardsPostH pid Dashboards.DashboardForm{file = "", teams = [], title = "First dashboard", fileDir = Nothing}
    _ <- testServant tr $ Monitors.alertUpsertPostH pid firstMonitor
    dbExec tr [sql|UPDATE projects.teams SET notify_emails = ARRAY['activation@example.com'] WHERE project_id = ? AND is_everyone AND deleted_at IS NULL|] (Only pid)
    (testNotifications, _) <- testServantWithNotifications tr $ Settings.notificationsTestPostH pid Settings.TestForm{Settings.issueType = "runtime_exception", Settings.channel = Settings.TCEmail, Settings.teamId = Nothing}
    length testNotifications `shouldBe` 1
    dbQuery tr [sql|SELECT milestone FROM projects.activation_milestones WHERE project_id = ? ORDER BY milestone|] (Only pid)
      `shouldReturn` map Only ["dashboard_created", "ingest_verified", "monitor_created", "notification_test_sent" :: Text]

    -- Steps were once recorded by a read-modify-write of the whole array, discarding a step
    -- completed in between; a pricing write must keep it. The conversion service failing must
    -- not block the project's redirect either.
    _ <- runQueryEffect tr $ Projects.completeOnboardingStep pid "explored_logs"
    let configured = tr{trATCtx = tr.trATCtx{Config.config = tr.trATCtx.config{Config.convertkitApiKey = "test-key"}}}
    call configured (interpose @HTTP (\_ _ -> throwIO $ userError "conversion service unavailable") $ CreateProject.pricingUpdateH pid def{CreateProject.isOnboarding = Just True})
      `shouldReturn` Right (CreateProject.PricingOnboarded pid)
    project <- projectOf tr pid
    (project.paymentPlan, sort (V.toList project.onboardingStepsCompleted)) `shouldBe` ("Free", ["Info", "Integration", "NotifChannel", "Pricing", "Survey", "created_monitor", "explored_logs"])
    landing <- toBaseServantResponse tr $ atAuthToBase tr.trSessAndHeader $ Dashboards.entrypointRedirectGetH "_overview.yaml" "Overview" [] pid []
    lookupResponseHeader @"Location" landing `shouldSatisfy` \case Header l -> ("/p/" <> pid.toText <> "/dashboards/") `T.isPrefixOf` l; _ -> False

  it "project settings: defaults, alert toggles, members, storage, super-admin notice and an unbilled deletion" \tr0 -> do
    (tr, pid) <- onboarded tr0
    let settings = do
          CreateProject.CreateProject (PageCtx _ (_, _, _, _, _, form, _, project)) <- call tr (CreateProject.projectSettingsGetH pid)
          pure (project.title, project.description, form.timeZone, (form.weeklyNotifs, form.dailyNotifs, form.errorAlerts, form.endpointAlerts))
        yes = Just "on"
    (title, description, timeZone, toggles) <- settings
    (title /= "", description, timeZone, toggles) `shouldBe` (True, "", "", (yes, yes, yes, yes))
    _ <- testServant tr $ CreateProject.createProjectPostH pid CreateProject.CreateProjectForm{title = "ACME Corp", description = "", emails = [], permissions = [], timeZone = "America/New_York", errorAlerts = yes, endpointAlerts = yes, weeklyNotifs = yes, dailyNotifs = Nothing}
    settings `shouldReturn` ("ACME Corp", "", "America/New_York", (yes, Nothing, yes, yes))
    CreateProject.ManageMembersGet (PageCtx _ (_, members, _, _, _)) <- call tr $ CreateProject.manageMembersGetH pid
    V.length members `shouldBe` 1

    -- A rejected replacement keeps the saved connection and what was typed; Remove clears it.
    let saved = Projects.ProjectS3Bucket "fixture-access" "fixture-secret" "us-east-1" "fixture-bucket" ""
        bucketOf = fmap (.bucket) . (.s3Bucket) <$> projectOf tr pid
        storageContent = elementsWith ["id=\"storage-connection-content\""]
    _ <- runQueryEffect tr $ Projects.updateProjectS3Bucket pid (Just saved)
    rejected <- call tr $ Settings.brings3PostH pid saved{Projects.bucket = "INVALID_bucket", Projects.secretKey = "replacement-secret"}
    (rejected.savedBucket <&> (.bucket), rejected.rejected <&> (.secretKey) . fst) `shouldBe` (Just "fixture-bucket", Just "replacement-secret")
    bucketOf `shouldReturn` Just "fixture-bucket"
    removed <- call tr $ Settings.brings3RemoveH pid
    for_ @[] ["fixture-access", "fixture-secret", "fixture-bucket"] \stale -> renderT removed `shouldNotSatisfy` T.isInfixOf stale
    bucketOf `shouldReturn` Nothing

    -- A viewer sees which bucket, never its credentials, and cannot change or remove it.
    let uid = (getResponse tr.trSessAndHeader).user.id
    _ <- runQueryEffect tr $ Projects.updateProjectS3Bucket pid (Just saved)
    dbExec tr [sql|UPDATE projects.project_members SET permission = 'view' WHERE project_id = ? AND user_id = ?|] (pid, uid)
    viewer <- call tr $ Settings.bringS3GetH pid
    for_ @[] ["fixture-access", "fixture-secret"] \secret -> renderT viewer `shouldNotSatisfy` T.isInfixOf secret
    goldenParts "settings-storage-states" $ concatMap storageContent [renderT rejected, renderT removed, renderT viewer]
    for_ @[] [void $ Settings.brings3PostH pid saved{Projects.bucket = "INVALID_bucket"}, void $ Settings.brings3RemoveH pid] \mutation ->
      runAuthHandler tr (catchError @ServantS.ServerError (mutation $> 200) (\_ err -> pure err.errHTTPCode)) `shouldReturn` (403 :: Int)
    bucketOf `shouldReturn` Just "fixture-bucket"
    -- Save once skipped the membership check: anyone signed in could repoint another project's archive.
    victim <- createTestProject tr "victim"
    assertNonMemberDenied tr victim $ Settings.brings3PostH victim (realS3Form tr)
    isNothing . (.s3Bucket) <$> projectOf tr victim `shouldReturn` True

    -- A super admin outside the project's memberships gets a notice above everything else.
    noticeOf pid tr "super-admin-banner" `shouldReturn` []
    dbExec tr [sql|DELETE FROM projects.project_members WHERE project_id = ? AND user_id = ?|] (pid, uid)
    notice <- noticeOf pid tr "super-admin-banner"
    goldenParts "settings-super-admin-notice" notice
    html <- settingsHtml tr pid
    fst (T.breakOn "id=\"side-nav-menu\"" html) `shouldSatisfy` T.isInfixOf "id=\"super-admin-banner\""
    dbExec tr [sql|UPDATE projects.project_members SET permission = 'admin' WHERE project_id = ? AND user_id = ?|] (pid, uid)
    dbExec tr [sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (?, ?, 'admin') ON CONFLICT DO NOTHING|] (pid, uid)
    (reqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ CreateProject.deleteProjectGetH pid
    reqs `shouldBe` []

  it "connects real S3 storage, renders it, and removes it" \tr0 ->
    requireMinio tr0 pendingWith do
      (tr, pid) <- onboarded tr0
      -- Must be the endpoint withTestResources probed (http://minio:9000 in CI, 127.0.0.1:9000 locally).
      let s3Form = realS3Form tr
          status page = elementsWith [">Connected<", ">Not connected<"] (renderT page)
      connected <- call tr $ Settings.brings3PostH pid s3Form
      savedPage <- call tr $ Settings.bringS3GetH pid
      renderT savedPage `shouldSatisfy` T.isInfixOf s3Form.bucket
      removed <- call tr $ Settings.brings3RemoveH pid
      map (T.isInfixOf ">Connected<") (status connected <> status savedPage <> status removed) `shouldBe` [True, True, False]
      isNothing . (.s3Bucket) <$> projectOf tr pid `shouldReturn` True

  it "a Stripe subscription: one trial per human, Stripe's cycle, replacement, dunning, lapse, reactivation and deletion" \tr0 -> do
    (tr, pid) <- onboarded tr0
    let uid = (getResponse tr.trSessAndHeader).user.id
        project = projectOf tr pid
        plan = (.paymentPlan) <$> project
        billing = fresh tr >>= \t -> call t (Settings.manageBillingGetH pid) <&> \(Settings.BillingGet (PageCtx _ d)) -> d
        stripeAs pl subId = void $ runQueryEffect tr $ Projects.updateStripeProjectBilling pid (Projects.PlanName pl) (Projects.SubId subId) (Projects.SubItemId "si_test") (Projects.CustomerId "cus_test")
        checkoutWith fixture = do
          t <- fresh tr
          runAsBaseRecordingHTTP tr $ catchError @ServantS.ServerError (withHTTPResponses fixture (atAuthToBase t.trSessAndHeader $ CreateProject.stripeCheckoutInitH pid (CreateProject.StripeCheckoutForm "GraduatedPricing")) $> 200) (\_ err -> pure err.errHTTPCode)
        checkoutForm fixture keys = do
          (reqs, status) <- checkoutWith fixture
          status `shouldBe` (200 :: Int)
          form <- maybe (fail "no checkout session request recorded") (pure . parseSimpleQuery . toStrict) $ lookup (stripeApi "checkout/sessions") reqs
          pure $ map (`lookup` form) keys
        sub status item = AE.object ["id" AE..= ("sub_new" :: Text), "status" AE..= (status :: Text), "items" AE..= AE.object ["data" AE..= ([AE.object ["id" AE..= (item :: Text), "price" AE..= AE.object ["id" AE..= ("price_graduated" :: Text)]]] :: [AE.Value])]]
        noFixture _ _ = pure Nothing
        banner = noticeOf pid tr "billing-downgrade-banner"

    -- One human is one Stripe customer with one trial: projects used to mint their own of
    -- each, which is how one account got two subscriptions on the same card.
    banner `shouldReturn` []
    runQueryEffect tr (Projects.userBilling uid) `shouldReturn` Projects.UserBilling{stripeCustomerId = Nothing, hasSubscribedBefore = False}
    (.trialEligible) <$> billing `shouldReturn` True
    let sessionKeys = ["subscription_data[trial_period_days]", "customer", "allow_promotion_codes", "managed_payments[enabled]"]
    checkoutForm noFixture sessionKeys `shouldReturn` [Just "30", Nothing, Just "true", Just "true"]
    stripeAs "GraduatedPricing" "sub_test123"
    Projects.projectProvider <$> project `shouldReturn` Projects.StripeProvider
    runQueryEffect tr (Projects.userBilling uid) `shouldReturn` Projects.UserBilling{stripeCustomerId = Just "cus_test", hasSubscribedBefore = True}
    checkoutForm noFixture sessionKeys `shouldReturn` [Nothing, Just "cus_test", Just "true", Just "true"]

    -- The cycle shown is the one Stripe bills (its item period, not one derived from created_at);
    -- the draft invoice is the headline, and neither it nor the $0 zero-length opener is a past cycle.
    d <- billing
    (d.cycleStart, d.cycleEnd, (.total) <$> d.currentInvoice) `shouldBe` (fromGregorian 2024 3 15, Just (fromGregorian 2024 4 15), Just 4150)
    map (\c -> (c.start, c.end, (.total) <$> c.invoice, (.hostedUrl) =<< c.invoice)) d.pastCycles
      `shouldBe` ( [ (fromGregorian 2024 2 15, fromGregorian 2024 3 15, Just 3400, Just "https://invoice.stripe.com/i/inv_two")
                   , (fromGregorian 2024 1 15, fromGregorian 2024 2 15, Just 2900, Just "https://invoice.stripe.com/i/inv_one")
                   ]
                     :: [(Day, Day, Maybe Int, Maybe Text)]
                 )
    goldenParts "billing-stripe-cycle" $ elementsWith ["$41.50", "inv_two"] (renderT $ Settings.billingPage d)

    -- Checkout overwrites sub_id, so the subscription held before must be cancelled first.
    replaced <- stripeCheckoutCompleted tr pid "sub_new" []
    map fst replaced `shouldContain` [stripeApi "subscriptions/sub_test123"]
    (.subId) <$> project `shouldReturn` Just "sub_new"

    -- An unsigned, malformed, wrong-secret, stale or tampered event changes nothing and calls no one.
    now <- getTestTime tr.trTestClock
    let unpaidEvent = stripeEvent "customer.subscription.updated" (sub "unpaid" "si_new")
        secret = tr.trATCtx.env.stripeWebhookSecret
    for_ @[] [Nothing, Just "garbage", Just (stripeSig "whsec_wrong" now unpaidEvent), Just (stripeSig secret (addUTCTime (-301) now) unpaidEvent), Just (stripeSig secret now "{}")] \h ->
      stripeDeliver tr noFixture h unpaidEvent `shouldReturn` ([], 400)
    (,) <$> plan <*> ((.subId) <$> project) `shouldReturn` ("GraduatedPricing", Just "sub_new")
    -- Dunning ending at `unpaid` keeps the subscription; paying anywhere flips it back to
    -- `active`, and only customer.subscription.updated says so.
    _ <- stripeWebhook tr noFixture "customer.subscription.updated" (sub "unpaid" "si_new")
    plan `shouldReturn` "Free"
    -- A cancelled Stripe sub can't be revived from the portal, only by a new checkout.
    lapsed <- billing
    lapsed.trialEligible `shouldBe` False
    notice <- banner
    goldenParts "billing-downgrade-notice-stripe" $ notice <> tagsWith ["/manage_subscription\""] (renderT $ Settings.billingPage lapsed)
    _ <- stripeWebhook tr noFixture "customer.subscription.updated" (sub "active" "si_new")
    plan `shouldReturn` "GraduatedPricing"
    banner `shouldReturn` []

    -- Paying a leftover invoice of a sub Stripe already cancelled must not pretend to reactivate.
    _ <- runQueryEffect tr $ Projects.downgradeToFreeBySubId "sub_new"
    let cancelled _ url = pure $ AE.encode (sub "canceled" "si_new") <$ guard (toText url == stripeApi "subscriptions/sub_new")
    (paidReqs, paidStatus) <- stripeWebhook tr cancelled "invoice.paid" $ AE.object ["id" AE..= ("in_gone" :: Text), "customer" AE..= ("cus_test" :: Text), "parent" AE..= AE.object ["subscription_details" AE..= AE.object ["subscription" AE..= ("sub_new" :: Text)]]]
    (paidStatus, stripeApi "subscriptions/sub_new" `elem` map fst paidReqs) `shouldBe` (200, True)
    plan `shouldReturn` "Free"

    -- Reactivating a lapsed sub is one Managed Payments checkout that also carries every open
    -- invoice (Shipbubble, 2026-10-23: $68.37 open on a sub cancelled 2026-09-29).
    let invoice id' remaining = AE.object ["id" AE..= (id' :: Text), "currency" AE..= ("usd" :: Text), "status" AE..= ("open" :: Text), "number" AE..= ("MONO-0003" :: Text), "total" AE..= (6837 :: Int), "amount_remaining" AE..= (remaining :: Int), "period_start" AE..= (1786787328 :: Int), "period_end" AE..= (1789465728 :: Int)]
        page more invs = AE.encode $ AE.object ["has_more" AE..= more, "data" AE..= (invs :: [AE.Value])]
        openInvoices _ url = pure do
          guard $ stripeApi "invoices?" `T.isPrefixOf` toText url
          pure $ if "starting_after=in_owed" `T.isInfixOf` toText url then page False [invoice "in_older" 100] else page True [invoice "in_owed" 6837]
    checkoutForm openInvoices ["line_items[2][price_data][unit_amount]", "line_items[3][price_data][unit_amount]", "line_items[4][price_data][unit_amount]", "metadata[settles_invoices]", "managed_payments[enabled]"]
      `shouldReturn` [Just "6837", Just "100", Nothing, Just "in_owed,in_older", Just "true"]
    settled <- stripeCheckoutCompleted tr pid "sub_back" [("settles_invoices", "in_owed")]
    lookup (stripeApi "invoices/in_owed/pay") settled `shouldBe` Just "paid_out_of_band=true"
    plan `shouldReturn` "GraduatedPricing"
    banner `shouldReturn` []

    -- No checkout without the debt when the invoice lookup fails.
    stripeAs "Free" "sub_lapsed"
    (lookupFailed, lookupStatus) <- checkoutWith \_ url -> if stripeApi "invoices?" `T.isPrefixOf` toText url then throwIO (userError "Stripe temporarily unavailable") else pure Nothing
    (lookupStatus, stripeApi "checkout/sessions" `elem` map fst lookupFailed) `shouldBe` (503, False)

    -- A failed settlement upgrades the project anyway and asks Stripe to retry, which then
    -- pays only what is still open and cancels nothing.
    let checkout status settles = AE.object ["client_reference_id" AE..= pid.toText, "subscription" AE..= ("sub_new" :: Text), "customer" AE..= ("cus_test" :: Text), "payment_status" AE..= (status :: Text), "metadata" AE..= AE.object ["settles_invoices" AE..= (settles :: Text)]]
        settlement failPayment _ url
          | toText url == stripeApi "invoices/in_settled" = pure $ Just "{\"status\":\"paid\"}"
          | failPayment && toText url == stripeApi "invoices/in_owed/pay" = throwIO $ userError "Stripe temporarily unavailable"
          | otherwise = pure Nothing
    snd <$> stripeWebhook tr (settlement True) "checkout.session.completed" (checkout "paid" "in_settled,in_owed") `shouldReturn` 503
    (.subId) <$> project `shouldReturn` Just "sub_new"
    (retried, retryStatus) <- stripeWebhook tr (settlement False) "checkout.session.completed" (checkout "paid" "in_settled,in_owed")
    (retryStatus, lookup (stripeApi "invoices/in_owed/pay") retried) `shouldBe` (200, Just "paid_out_of_band=true")
    map fst retried `shouldNotContain` [stripeApi "subscriptions/sub_new", stripeApi "invoices/in_settled/pay"]

    -- An unpaid (async) checkout waits for the payment before settling or upgrading.
    stripeAs "Free" "sub_lapsed"
    (unpaid, unpaidStatus) <- stripeWebhook tr noFixture "checkout.session.completed" (checkout "unpaid" "in_owed")
    (unpaidStatus, stripeApi "invoices/in_owed/pay" `elem` map fst unpaid) `shouldBe` (200, False)
    plan `shouldReturn` "Free"
    (asyncPaid, asyncStatus) <- stripeWebhook tr noFixture "checkout.session.async_payment_succeeded" (checkout "paid" "in_owed")
    (asyncStatus, lookup (stripeApi "invoices/in_owed/pay") asyncPaid) `shouldBe` (200, Just "paid_out_of_band=true")
    plan `shouldReturn` "GraduatedPricing"

    -- Choosing Free once cleared sub_id but left the Stripe sub billing.
    t <- fresh tr
    (freeReqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase t.trSessAndHeader $ CreateProject.pricingUpdateH pid def
    map fst freeReqs `shouldContain` [stripeApi "subscriptions/sub_new"]
    plan `shouldReturn` "Free"
    -- Deleting a project left its subscription live, billing a project nobody could see; and
    -- the deletion must not reset the human's customer or trial history.
    stripeAs "GraduatedPricing" "sub_test123"
    (deleteReqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ CreateProject.deleteProjectGetH pid
    map fst deleteReqs `shouldContain` [stripeApi "subscriptions/sub_test123"]
    runQueryEffect tr (Projects.userBilling uid) `shouldReturn` Projects.UserBilling{stripeCustomerId = Just "cus_test", hasSubscribedBefore = True}

  it "a Lemon Squeezy subscription: created, locally-cycled, cancelled, expired, resumed and deleted by webhook" \tr0 -> do
    (tr, pid) <- onboarded tr0
    let webhook event = do
          (reqs, _) <- runAsBaseRecordingHTTP tr $ Settings.webhookPostH Nothing $ toStrict $ AE.encode $ lsEvent event pid
          reqs `shouldBe` []
        plan = (.paymentPlan) <$> projectOf tr pid
        banner = noticeOf pid tr "billing-downgrade-banner"
        billing = do
          t <- fresh tr
          (reqs, Settings.BillingGet (PageCtx _ d)) <- second getResponse <$> runAsBaseRecordingHTTP tr (atAuthToBase t.trSessAndHeader $ Settings.manageBillingGetH pid)
          pure (reqs, d)
    -- The plan is the product bought, not a fixed tier name.
    webhook "subscription_created"
    dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.subscriptions WHERE project_id = ?|] (Only pid.toText) `shouldReturn` 1
    -- The provider is stored explicitly, not guessed at read time.
    dbOne @_ @Text tr [sql|SELECT billing_provider::text FROM projects.projects WHERE id = ?|] (Only pid) `shouldReturn` "lemon_squeezy_provider"
    plan `shouldReturn` "Test Plan"
    -- A Lemon Squeezy project keeps the locally-derived cycle, with no Stripe call made for it.
    (billingReqs, d) <- billing
    (billingReqs, d.cycleEnd, (.total) <$> d.currentInvoice) `shouldBe` ([], Nothing, Nothing)
    -- Enforced, an unsigned, malformed or wrong-secret event is refused and changes nothing.
    let enforced = tr{trATCtx = tr.trATCtx{Config.env = tr.trATCtx.env{Config.lsWebhookSigEnforce = True}}}
        cancel = toStrict $ AE.encode $ lsEvent "subscription_cancelled" pid
        lsSig key = decodeUtf8 $ B16.encode $ BA.convert (HMAC.hmac (encodeUtf8 @Text @ByteString key) cancel :: HMAC.HMAC SHA256)
        deliver h = runAsBaseRecordingHTTP enforced $ catchError @ServantS.ServerError (Settings.webhookPostH h cancel $> 200) (\_ err -> pure err.errHTTPCode)
    for_ @[] [Nothing, Just "not-hex", Just (lsSig "wrong-secret")] \h -> deliver h `shouldReturn` ([], 400 :: Int)
    plan `shouldReturn` "Test Plan"
    deliver (Just $ lsSig tr.trATCtx.env.lemonSqueezyWebhookSecret) `shouldReturn` ([], 200)
    plan `shouldReturn` "Free"
    (_, cancelled) <- billing
    notice <- banner
    goldenParts "billing-downgrade-notice-lemon-squeezy" $ notice <> tagsWith ["/manage_subscription\""] (renderT $ Settings.billingPage cancelled)
    webhook "subscription_expired"
    plan `shouldReturn` "Free"
    webhook "subscription_resumed"
    plan `shouldReturn` "Test Plan"
    banner `shouldReturn` []
    (deleteReqs, _) <- runAsBaseRecordingHTTP tr $ atAuthToBase tr.trSessAndHeader $ CreateProject.deleteProjectGetH pid
    map fst deleteReqs `shouldContain` ["https://api.lemonsqueezy.com/v1/subscriptions/12345"]

  -- ReportUsage bills people. It must never charge wrongly: no submission for a project it
  -- can't attribute, no window reaching back before the current cycle, an auditable failed
  -- row (not a quiet submission) when the provider is unusable, and never a re-sent chunk.
  -- Each case bills a project of its own: TimeFusion is shared across the run, keyed by project.
  it "Stripe metering: unattributable, leased, unaddressable, drained once, never re-sent" \tr -> do
    pid <- createTestProject tr "report-usage"
    seedUsage tr pid frozenTime 3 4 3
    let ancient = UTCTime (fromGregorian 2023 6 1) 0
    setBilling tr pid "GraduatedPricing" Nothing stripeSub ancient
    -- No subscription item: nothing to report against, and the watermark must not move past usage still owed.
    report tr pid frozenTime `shouldReturn` []
    (,,) <$> submissions tr pid <*> dailyUsage tr pid <*> lastReported tr pid `shouldReturn` ([], [], ancient)
    dbExec tr [sql|UPDATE projects.projects SET first_sub_item_id = 'sub_item_1' WHERE id = ?|] (Only pid)
    -- Hourly and daily ticks both enqueue at 00:00; the loser of the lease must not record the window again.
    withLease tr pid (report tr pid frozenTime) `shouldReturn` []
    (,) <$> dailyUsage tr pid <*> lastReported tr pid `shouldReturn` ([], ancient)
    -- No Stripe customer: an enabled meter with nowhere to send is money we're owed, so its chunk
    -- exists and is visibly failed, the window is clamped forward (cycle start 2024-12-02, then the
    -- 7-day retention horizon), and the watermark advances so an outage can't replay it forever.
    report tr pid frozenTime `shouldReturn` []
    rows <- submissions tr pid
    L.nub [(start, status) | (start, _, _, status) <- rows] `shouldBe` [(UTCTime (fromGregorian 2024 12 25) 0, "failed")]
    metered tr pid `shouldReturn` [("events", 3, "failed"), ("metric_datapoints", 4, "failed"), ("session_replays", 3, "failed")]
    (,) <$> dailyUsage tr pid <*> lastReported tr pid `shouldReturn` ([(7, 4, 3)], frozenTime)
    -- Once addressable the chunks drain, each on its OWN Stripe meter, exactly once.
    dbExec tr [sql|UPDATE projects.projects SET customer_id = 'cus_test_reportusage' WHERE id = ?|] (Only pid)
    drained <- report tr pid frozenTime
    (length drained, stripeMeters drained) `shouldBe` (3, [("events_usage", "3"), ("metrics_usage", "4"), ("session_replays_usage", "3")])
    metered tr pid `shouldReturn` [("events", 3, "submitted"), ("metric_datapoints", 4, "submitted"), ("session_replays", 3, "submitted")]
    -- Replays count on an immutable created_at over a half-open window: a later tick counts none again.
    report tr pid (addUTCTime 3600 frozenTime) `shouldReturn` []
    metered tr pid `shouldReturn` [("events", 3, "submitted"), ("metric_datapoints", 4, "submitted"), ("session_replays", 3, "submitted")]
    sum . map (\(_, _, r) -> r) <$> dailyUsage tr pid `shouldReturn` 3

  it "Stripe metering bills a replay-only window and AI usage in micro-dollars, each on its own meter" \tr -> do
    let paid seed = do
          pid <- createTestProject tr "report-usage"
          seed pid
          setBilling tr pid "GraduatedPricing" (Just "sub_item_1") stripeSub (addUTCTime (-86400 * 3) frozenTime)
          dbExec tr [sql|UPDATE projects.projects SET customer_id = 'cus_test_reportusage' WHERE id = ?|] (Only pid)
          (pid,) . stripeMeters <$> report tr pid frozenTime
    -- A project can have replays and no telemetry; a guard keyed on events+metrics billed nothing.
    (replayPid, replayPosts) <- paid \pid -> seedUsage tr pid frozenTime 0 0 2
    replayPosts `shouldBe` [("session_replays_usage", "2")]
    (,) <$> dailyUsage tr replayPid <*> metered tr replayPid `shouldReturn` ([(0, 0, 2)], [("session_replays", 2, "submitted")])
    (aiPid, aiPosts) <- paid \pid -> runTestBg frozenTime tr $ Projects.recordAIUsage pid "test-model" Projects.UsageRoutine $ Projects.AIUsageTotals 1200 300 37
    aiPosts `shouldBe` [("ai_usage_microusd", "37")]
    dbQuery tr [sql|SELECT total_ai_input_tokens::INT, total_ai_output_tokens::INT, total_ai_cost_microusd::INT FROM apis.daily_usage WHERE project_id = ?|] (Only aiPid)
      `shouldReturn` [(1200 :: Int, 300 :: Int, 37 :: Int)]
    metered tr aiPid `shouldReturn` [("ai_usage", 37, "submitted")]

  -- One LS item is billed at the events rate, so every dimension rides it restated in that
  -- meter's units — the factor IS the price, hence exact quantities.
  it "Lemon Squeezy metering: an itemless backlog kept owed, restated on the one item, raw on a per-meter item" \tr -> do
    pid <- createTestProject tr "report-usage"
    -- A numeric sub_id marks the project as Lemon Squeezy. With no item the run records nothing
    -- and keeps the watermark, so the backlog is still owed once the item is fixed.
    let before = addUTCTime (-86400 * 3) frozenTime
    seedUsage tr pid frozenTime 0 0 3
    setBilling tr pid "GraduatedPricing" Nothing (Just "812345") before
    report tr pid frozenTime `shouldReturn` []
    (,) <$> metered tr pid <*> lastReported tr pid `shouldReturn` ([], before)
    dbExec tr [sql|UPDATE projects.projects SET first_sub_item_id = '912345' WHERE id = ?|] (Only pid)
    seedUsage tr pid (addUTCTime 1800 frozenTime) 3 40 3
    restated <- report tr pid (addUTCTime 3600 frozenTime)
    -- 40 datapoints at $1/10M are 4 events-units at $1/1M; 6 replays (3 owed) at $1/1,000 are 6,000.
    metered tr pid `shouldReturn` [("events", 3, "submitted"), ("metric_datapoints", 4, "submitted"), ("session_replays", 6000, "submitted")]
    lsPosts restated `shouldSatisfy` \bs -> length bs == 3 && all (T.isInfixOf "912345") bs
    -- A per-meter item is priced for its own dimension: the raw count, against that item, never
    -- the events item first_sub_item_id falls back to.
    void $ runTestBg frozenTime tr $ Projects.setMeterSubItemId pid Projects.SessionReplays "912999"
    seedUsage tr pid (addUTCTime 9000 frozenTime) 0 0 2
    perMeter <- report tr pid (addUTCTime 7200 frozenTime)
    map (\b -> (T.isInfixOf "912999" b, T.isInfixOf "912345" b)) (lsPosts perMeter) `shouldBe` [(True, False)]
    metered tr pid `shouldReturn` [("events", 3, "submitted"), ("metric_datapoints", 4, "submitted"), ("session_replays", 6000, "submitted"), ("session_replays", 2, "submitted")]

  -- "Engine/API Prod" (LS sub 445873) was left active = false, deleted_at IS NULL by a direct DB
  -- write while its subscription kept billing; once API-key lookup joined on projects.active its
  -- ingestion stopped. Migration 0181 makes that pair unrepresentable.
  it "projects_subscribed_not_deactivated: off while unsubscribed, back on when subscribed, never off while billed" \tr -> do
    pid <- createTestProject tr "subscribed"
    let setActive a = dbExec tr [sql|UPDATE projects.projects SET active = ? WHERE id = ?|] (a, pid)
        flags = dbQuery tr [sql|SELECT active, deleted_at IS NOT NULL FROM projects.projects WHERE id = ?|] (Only pid)
    -- A 2024 cleanup switched off 749 unsubscribed projects on purpose.
    setBilling tr pid "Free" Nothing Nothing frozenTime
    setActive False
    flags `shouldReturn` [(False, False)]
    -- Taking the payment and then rejecting the plan change is the worse half of the bug.
    void $ runTestBg frozenTime tr $ Projects.updateProjectPricing pid (Projects.PlanName "GraduatedPricing") (Projects.SubId "445873") (Projects.SubItemId "377166") (Projects.OrderId "3062810")
    flags `shouldReturn` [(True, False)]
    setActive False `shouldThrow` \e -> PGS.sqlState e == "23514"
    -- Deleting stays fine: deleted_at is the soft-delete marker and the subscription is cancelled on that path.
    void $ runTestBg frozenTime tr $ Projects.deleteProject pid
    flags `shouldReturn` [(False, True)]

  -- The cap is metered from apis.daily_usage, which ReportUsage writes hourly. It used to count
  -- Postgres otel_logs_and_spans, empty under TimeFusion-only writes, so a downgraded project
  -- (Shipbubble, 2026-10) kept ingesting ~4.7M events/day on Free.
  it "free tier: usage recorded never billed, floored at the UTC day, capped ingest dropped and rejected, notices once" \tr0 -> do
    let tr = tr0{trATCtx = tr0.trATCtx{Config.config = tr0.trATCtx.config{Config.enableFreetier = True}}}
    pid <- createTestProject tr "free"
    key <- createTestAPIKey tr pid "free-tier-key"
    -- frozenTime is 00:00 UTC, where a free window floored at the day is empty: run an hour in.
    for_ @[] ["usage line 1", "usage line 2", "usage line 3"] \b -> ingestLog tr0 key b (addUTCTime 1800 frozenTime)
    ingestLog tr0 key "three days ago" (addUTCTime (-86400 * 3) frozenTime)
    setBilling tr pid "Free" (Just "sub_item_free") stripeSub (addUTCTime (-86400 * 5) frozenTime)
    report tr0 pid (addUTCTime 3600 frozenTime) `shouldReturn` []
    -- Recorded (the cap and billing page read it), never submitted, and weeks of backlog never land in one window.
    (,) <$> submissions tr pid <*> dailyUsage tr pid `shouldReturn` ([], [(3, 0, 0)])

    ingestLog tr key "under the cap" frozenTime
    logCount tr pid `shouldReturn` 5
    dbExec tr [sql|INSERT INTO apis.daily_usage (project_id, total_requests, total_metrics, window_start, window_end) VALUES (?, ?, 0, now() - interval '1 minute', now())|] (pid, IngestBudget.hardCap Projects.freeTierDailyMaxEvents)
    Cache.purge tr.trATCtx.projectCache
    ingestLog tr key "over the cap" frozenTime
    logCount tr pid `shouldReturn` 5
    -- Capped data is acked, not dead-lettered as unresolved (~10k capped messages/hour looped the
    -- DLQ tiers, 2026-10-22). An unresolved key still is, as is a payload mixing one in, and zero
    -- rows from a project that is not capped.
    uncappedKey <- createTestProject tr "uncapped" >>= \p -> createTestAPIKey tr p "uncapped-key"
    let logs k = createOtelLogAtTime k ["capped?"] frozenTime
        mixed = logs key & LSF.resourceLogs %~ (<> (logs "no-such-key" ^. LSF.resourceLogs))
    Right (acks, poison) <-
      runTestBg frozenTime tr
        $ OtlpServer.processList
          [("capped-ack", encodeMessage $ logs key), ("unknown-ack", encodeMessage $ logs "no-such-key"), ("mixed-ack", encodeMessage mixed), ("empty-ack", encodeMessage $ createOtelLogAtTime uncappedKey [] frozenTime)]
          (HM.singleton "ce-type" "org.opentelemetry.otlp.logs.v1")
    (acks, map (^. _1) poison) `shouldBe` (["capped-ack"], ["unknown-ack", "mixed-ack", "empty-ack"])
    Right body <- OtlpServer.httpLogsExport tr.trLogger tr.trATCtx tr.trTracerProvider (Just key) (encodeMessage $ createOtelLogAtTime key ["rejected"] frozenTime)
    Right (res :: LS.ExportLogsServiceResponse) <- pure $ decodeMessage body
    (res ^. LSF.partialSuccess . LSF.rejectedLogRecords, "00:00 UTC" `T.isInfixOf` (res ^. LSF.partialSuccess . LSF.errorMessage)) `shouldBe` (1, True)
    logCount tr pid `shouldReturn` 5
    runTestBg frozenTime tr (traverse (Projects.claimFreeTierNotice pid) [Projects.NoticeSampling, Projects.NoticeSampling, Projects.NoticeExceeded, Projects.NoticeExceeded, Projects.NoticeSampling])
      `shouldReturn` [True, False, True, False, False]

  -- Past its budget a capped project keeps a share paced across the day, each kept record
  -- weighted by how many it stands for. The bounds are statistical.
  it "free tier sampling keeps a paced weighted share, error traces whole and metric series whole" \tr0 -> do
    -- Yesterday from this hour on brought twice the 8,000-event (non-error) budget: keep half.
    (tr, pid, key) <- cappedExpecting tr0 16000
    runTestBg frozenTime tr $ OtlpServer.processLogsRequest Nothing (createOtelLogAtTime key [show i | i <- [1 .. 400 :: Int]] frozenTime)
    kept <- logCount tr pid
    kept `shouldSatisfy` \k -> k > 160 && k < 240
    dbQuery tr [sql|SELECT SUM(seen)::INT, SUM(kept)::INT FROM projects.ingest_sampling WHERE project_id = ? AND hour >= date_trunc('day', ?::timestamptz, 'UTC')|] (pid, frozenTime) `shouldReturn` [(400 :: Int, kept)]
    dbQuery tr [sql|SELECT DISTINCT (attributes->'monoscope'->>'sample_weight')::float8 FROM otel_logs_and_spans WHERE project_id = ?::text|] (Only pid) `shouldReturn` [Only (2 :: Double)]

    -- Errors draw on their own reserve (no history: kept at 1) while the rest is kept at 0.1.
    (trE, errPid, errKey) <- cappedExpecting tr0 80000
    let hex n i = T.justifyRight n '0' (show i)
        spanAt t s err = mkSpanRequest (hex 32 t) (hex 16 (t * 10 + s)) Nothing "op" [] ((defMessage & PTF.code .~ PT.Status'STATUS_CODE_ERROR) <$ guard err) [] (mkResource errKey []) frozenTime
        spans = [spanAt t s (s == 1) | t <- [1 .. 20 :: Int], s <- [1 .. 3]] <> [spanAt t 1 False | t <- [101 .. 300]]
    runTestBg frozenTime trE $ OtlpServer.processTraceRequest Nothing (defMessage & TSF.resourceSpans .~ concatMap (^. TSF.resourceSpans) spans)
    perTrace :: [(Text, Int)] <- dbQuery trE [sql|SELECT context___trace_id, COUNT(*)::INT FROM otel_logs_and_spans WHERE project_id = ?::text GROUP BY 1|] (Only errPid)
    let (errorTraces, okTraces) = L.partition ((`elem` map (hex 32) [1 .. 20 :: Int]) . fst) perTrace
    map snd errorTraces `shouldBe` replicate 20 3
    length okTraces `shouldSatisfy` \k -> k > 5 && k < 45

    (trM, metricPid, metricKey) <- cappedExpecting tr0 16000
    let points = [createGaugeMetricAtTime metricKey [] [] ("m" <> show i) 1 t | i <- [1 .. 200 :: Int], t <- [frozenTime, addUTCTime 60 frozenTime]]
    runTestBg frozenTime trM $ OtlpServer.processMetricsRequest Nothing (defMessage & MSF.resourceMetrics .~ concatMap (^. MSF.resourceMetrics) points)
    perSeries :: [(Text, Int)] <- dbQuery trM [sql|SELECT metric_name, COUNT(*)::INT FROM otel_metrics WHERE project_id = ? GROUP BY 1|] (Only metricPid)
    L.nub (map snd perSeries) `shouldBe` [2]
    length perSeries `shouldSatisfy` \k -> k > 60 && k < 140


-- | A project created through the onboarding handler, with the session refreshed to see it.
onboarded :: TestResources -> IO (TestResources, Projects.ProjectId)
onboarded tr = do
  Header location <- lookupResponseHeader @"Location" <$> runAuthHandler tr CreateProject.projectOnboardingH
  pid <- maybe (fail $ "no project id in " <> toString location) (pure . UUIDId) $ UUID.fromText $ T.takeWhile (/= '/') $ T.drop 3 location
  (,pid) <$> fresh tr


-- | @tr@ with its session refreshed, so it sees projects created or changed since.
fresh :: TestResources -> IO TestResources
fresh tr = refreshSession tr.trPool tr.trATCtx.hasqlPool tr.trSessAndHeader <&> \s -> tr{trSessAndHeader = s}


settingsHtml :: TestResources -> Projects.ProjectId -> IO Text
settingsHtml tr pid = fresh tr >>= \t -> renderT <$> call t (CreateProject.projectSettingsGetH pid)


-- | The project notice with this id on the settings page, if shown.
noticeOf :: Projects.ProjectId -> TestResources -> Text -> IO [Text]
noticeOf pid tr noticeId = elementsWith ["id=\"" <> noticeId <> "\""] <$> settingsHtml tr pid


realS3Form :: TestResources -> Projects.ProjectS3Bucket
realS3Form tr = let env = tr.trATCtx.env in Projects.ProjectS3Bucket{accessKey = env.s3AccessKey, secretKey = env.s3SecretKey, region = env.s3Region, bucket = env.s3Bucket, endpointUrl = env.s3Endpoint}


firstMonitor :: Monitors.AlertUpsertForm
firstMonitor = Monitors.AlertUpsertForm{unit = Nothing, alertId = Nothing, alertThreshold = 1, warningThreshold = Nothing, recipientEmails = [], recipientSlacks = [], recipientEmailAll = Nothing, direction = Issues.Above, title = "First monitor", severity = "Warning", subject = "First monitor", message = "First monitor", query = "kind == \"server\" | summarize count()", since = "1h", from = "", to = "", frequency = Nothing, timeWindow = Just "1h", conditionType = Just "threshold_exceeded", source = Nothing, vizType = Nothing, teams = [], alertRecoveryThreshold = Nothing, warningRecoveryThreshold = Nothing, widgetId = Nothing, dashboardId = Nothing, notifyAfterCheck = Nothing, notifyAfter = Nothing, stopAfterCheck = Nothing, stopAfter = Nothing, environment = Nothing, service = Nothing}


stripeApi :: Text -> Text
stripeApi = ("https://api.stripe.com/v1/" <>)


-- | A signed checkout.session.completed moving @pid@ onto @newSub@; returns the outgoing Stripe requests.
stripeCheckoutCompleted :: TestResources -> Projects.ProjectId -> Text -> [(AE.Key, Text)] -> IO [(Text, LByteString)]
stripeCheckoutCompleted tr pid newSub metadata = do
  (requests, status) <-
    stripeWebhook tr (\_ _ -> pure Nothing) "checkout.session.completed"
      $ AE.object ["client_reference_id" AE..= pid.toText, "subscription" AE..= newSub, "customer" AE..= ("cus_test" :: Text), "payment_status" AE..= ("paid" :: Text), "metadata" AE..= AE.object (map (fmap AE.toJSON) metadata)]
  status `shouldBe` 200
  pure requests


-- | Deliver a Stripe event of @eventType@ wrapping @obj@, signed now with the configured secret.
stripeWebhook :: TestResources -> (Options -> String -> IO (Maybe LByteString)) -> Text -> AE.Value -> IO ([(Text, LByteString)], Int)
stripeWebhook tr responses eventType obj = do
  now <- getTestTime tr.trTestClock
  let body = stripeEvent eventType obj
  stripeDeliver tr responses (Just $ stripeSig tr.trATCtx.env.stripeWebhookSecret now body) body


-- | POST @body@ with the Stripe-Signature header @sigHeader@, answering Stripe calls from
-- @responses@ (invoice reads default to open); returns the outgoing requests and HTTP status.
stripeDeliver :: TestResources -> (Options -> String -> IO (Maybe LByteString)) -> Maybe Text -> ByteString -> IO ([(Text, LByteString)], Int)
stripeDeliver tr responses sigHeader body = do
  let appCtx = tr.trATCtx
      isolated = tr{trATCtx = appCtx{Config.config = appCtx.config{Config.smtpHost = "127.0.0.1", Config.smtpPort = 1, Config.smtpUsername = "", Config.smtpPassword = "", Config.postmarkToken = ""}}}
  runAsBaseRecordingHTTP isolated $ catchError @ServantS.ServerError (withHTTPResponses (\opts url -> (<|> ("{\"status\":\"open\"}" <$ guard (stripeApi "invoices/" `T.isPrefixOf` toText url))) <$> responses opts url) (Settings.stripeWebhookPostH sigHeader body) $> 200) (\_ err -> pure err.errHTTPCode)


stripeEvent :: Text -> AE.Value -> ByteString
stripeEvent eventType obj = toStrict $ AE.encode $ AE.object ["type" AE..= eventType, "data" AE..= AE.object ["object" AE..= obj]]


-- | @t=<epoch>,v1=<hmac>@ over @"<t>.<body>"@, signed with @secret@ at @at@.
stripeSig :: Text -> UTCTime -> ByteString -> Text
stripeSig secret at body = "t=" <> ts <> ",v1=" <> decodeUtf8 (B16.encode $ BA.convert mac)
  where
    ts = show @Text @Integer $ round $ utcTimeToPOSIXSeconds at
    mac = HMAC.hmac (encodeUtf8 @Text @ByteString secret) (encodeUtf8 @Text @ByteString ts <> "." <> body) :: HMAC.HMAC SHA256


lsEvent :: Text -> Projects.ProjectId -> Settings.WebhookData
lsEvent eventName pid =
  Settings.WebhookData
    { dataVal = Settings.DataVals{id = "sub_123", attributes = Settings.Attributes{firstSubscriptionItem = Settings.FirstSubItem{id = 111, subscriptionId = 12345}, productName = "Test Plan", orderId = 67890, userEmail = "test@example.com"}}
    , meta = Settings.MetaData{customData = Just Settings.CustomData{projectId = Just pid.toText}, eventName}
    }


-- | Put a project on a plan/provider footing. `billing_day` anchors the cycle (2023-01-02, so
-- the current one began this month) and `usage_last_reported` is the watermark the job clamps.
-- A `sub_` prefixed sub_id means Stripe, a numeric one Lemon Squeezy.
setBilling :: TestResources -> Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> UTCTime -> IO ()
setBilling tr pid plan subItem subId lastReported' =
  dbExec tr [sql|UPDATE projects.projects SET payment_plan = ?, first_sub_item_id = ?, sub_id = ?, billing_day = ?, usage_last_reported = ? WHERE id = ?|] (plan, subItem, subId, UTCTime (fromGregorian 2023 1 2) 0, lastReported', pid)


stripeSub :: Maybe Text
stripeSub = Just "sub_test_reportusage"


-- | Ingest @logs@ log lines and @metrics@ datapoints at @at@, and start @replays@ sessions an
-- hour before it (`created_at`, written once, is what billing counts).
seedUsage :: TestResources -> Projects.ProjectId -> UTCTime -> Int -> Int -> Int -> IO ()
seedUsage tr pid at logs metrics replays = do
  key <- createTestAPIKey tr pid "report-usage-key"
  for_ @[] [1 .. logs] \i -> ingestLog tr key ("usage line " <> show i) at
  for_ @[] [1 .. metrics] \i -> ingestMetric tr key [] [] ("usage.metric." <> show i) (fromIntegral i) at
  for_ @[] [1 .. replays] \i ->
    dbExec tr [sql|INSERT INTO projects.replay_sessions (session_id, project_id, last_event_at, merged, created_at, updated_at) VALUES (gen_random_uuid(), ?, ?, FALSE, ?, ?)|] (pid, addUTCTime (-3600) at, addUTCTime (fromIntegral i - 3600) at, at)


-- | Run ReportUsage at @t@ with provider calls recorded, never sent.
report :: TestResources -> Projects.ProjectId -> UTCTime -> IO [(Text, LByteString)]
report tr pid t = fst <$> runTestBgRecordingHTTP t tr (BJ.processBackgroundJob tr.trATCtx (BJ.ReportUsage pid))


withLease :: TestResources -> Projects.ProjectId -> IO a -> IO a
withLease tr pid act = withResource tr.trPool \conn -> PGS.withTransaction conn do
  void (PGS.query conn [sql|SELECT pg_advisory_xact_lock(hashtext(?))|] (Only ("report_usage:" <> pid.toText)) :: IO [Only ()])
  act


-- | (event_name, payload[value]) of each Stripe meter event: the meter identity rides the body.
stripeMeters :: [(Text, LByteString)] -> [(ByteString, ByteString)]
stripeMeters reqs = sort [(n, v) | (u, b) <- reqs, "stripe.com" `T.isInfixOf` u, let f = parseSimpleQuery (toStrict b), Just n <- [lookup "event_name" f], Just v <- [lookup "payload[value]" f]]


lsPosts :: [(Text, LByteString)] -> [Text]
lsPosts reqs = [decodeUtf8 b | (u, b) <- reqs, "lemonsqueezy.com" `T.isInfixOf` u]


submissions :: TestResources -> Projects.ProjectId -> IO [(UTCTime, UTCTime, Int, Text)]
submissions tr pid = dbQuery tr [sql|SELECT window_start, window_end, quantity::INT, status FROM projects.usage_report_submissions WHERE project_id = ? ORDER BY created_at|] (Only pid)


-- | (meter_kind, quantity, status) per chunk.
metered :: TestResources -> Projects.ProjectId -> IO [(Text, Int, Text)]
metered tr pid = dbQuery tr [sql|SELECT meter_kind, quantity::INT, status FROM projects.usage_report_submissions WHERE project_id = ? ORDER BY created_at, meter_kind|] (Only pid)


-- | (total_requests, total_metrics, total_replays) per recorded window.
dailyUsage :: TestResources -> Projects.ProjectId -> IO [(Int, Int, Int)]
dailyUsage tr pid = dbQuery tr [sql|SELECT total_requests::INT, total_metrics::INT, total_replays::INT FROM apis.daily_usage WHERE project_id = ? ORDER BY window_end|] (Only pid)


lastReported :: TestResources -> Projects.ProjectId -> IO UTCTime
lastReported tr pid = dbOne tr [sql|SELECT usage_last_reported FROM projects.projects WHERE id = ?|] (Only pid)


logCount :: TestResources -> Projects.ProjectId -> IO Int
logCount tr pid = dbOne tr [sql|SELECT COUNT(*)::INT FROM otel_logs_and_spans WHERE project_id = ?::text|] (Only pid)


-- | A Free project with the free tier on and yesterday's non-error traffic from this hour on
-- (which paces today's keep rate), plus an API key.
cappedExpecting :: TestResources -> Int -> IO (TestResources, Projects.ProjectId, Text)
cappedExpecting tr0 seenAhead = do
  let tr = tr0{trATCtx = tr0.trATCtx{Config.config = tr0.trATCtx.config{Config.enableFreetier = True}}}
  pid <- createTestProject tr "free-tier-sampling"
  setBilling tr pid "Free" Nothing Nothing frozenTime
  key <- createTestAPIKey tr pid "free-tier-sampling-key"
  dbExec tr [sql|INSERT INTO projects.ingest_sampling (project_id, hour, is_error, seen) VALUES (?, date_trunc('hour', ?::timestamptz, 'UTC') - interval '1 day', false, ?)|] (pid, frozenTime, seenAhead)
  pure (tr, pid, key)
