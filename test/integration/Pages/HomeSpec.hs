module Pages.HomeSpec (spec) where

import BackgroundJobs qualified as Jobs
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.Wreq (runHTTPRecordWith)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Issues qualified as Issues
import Models.Projects.Projects qualified as Projects
import Pages.Dashboards qualified as Dashboards
import Pages.Home qualified as Home
import Pages.Settings qualified as Settings
import Pkg.TestUtils


spec :: Spec
spec = around withTestResources $ describe "Home screen" do
  it "home_showsTodaysIssuesImprovementsAndRoutineFlagsOnce" \tr -> do
    pid <- createTestProject tr "Acme Shop"
    dbExec tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, endpoint_hash, created_at) VALUES (?, 'runtime_exception', 'Checkout fails on a null cart', 'home-new', 'home-new', ?)|] (pid, frozenTime)
    resolvedId <- dbOne @_ @Issues.IssueId tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, endpoint_hash, created_at, archived_at) VALUES (?, 'runtime_exception', 'Payment gateway timeout', 'home-old', 'home-old', ?::timestamptz - interval '3 days', ?) RETURNING id|] (pid, frozenTime, frozenTime)
    dbExec tr [sql|INSERT INTO apis.issue_activity_log (issue_id, event, created_at) VALUES (?, 'resolved', ?)|] (resolvedId, frozenTime)
    -- A daily routine flagging the same thing twice refreshes one item instead of stacking two.
    let flag kind title = runQueryEffect tr $ EHasql.transaction TxS.ReadCommitted TxS.Write $ Issues.upsertHighlightTx pid Nothing Issues.NewHighlight{kind, title, detail = "412 to 1,980 in an hour.", link = Just $ "/p/" <> pid.toText <> "/issues", source = "Error regression radar"}
    replicateM_ 2 $ flag Issues.HKInsight "Checkout 5xx up 4.8x since deploy v3.2.1"
    flag Issues.HKImprovement "p95 latency on /cart down 38%"
    dbOne @_ @Int tr [sql|SELECT count(*)::int FROM apis.home_highlights WHERE project_id = ?|] (Only pid) `shouldReturn` 2

    -- Starring the overview dashboard must not render its golden signals twice: duplicate DOM ids left the second copy loading forever.
    dbExec tr [sql|INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, base_template, title, starred_since) VALUES (gen_random_uuid(), ?, now(), now(), '00000000-0000-0000-0000-000000000001', '_overview.yaml', 'Overview', now())|] (Only pid)

    page <- renderT <$> call tr (Home.homeGetH pid)
    map (\w -> T.count ("id=\"" <> w <> "_widgetEl\"") page) ["traffic", "p95-latency", "error-rate", "apdex-score", "home-stack-logs"] `shouldBe` [1, 1, 1, 1, 1]
    page `shouldContainAll` ["Today&#39;s issues", "Checkout fails on a null cart", "Today&#39;s improvements", "Payment gateway timeout", "Resolved", "p95 latency on /cart down 38%", "Flagged by AI", "Checkout 5xx up 4.8x since deploy v3.2.1", "Error regression radar", "Talk to Monoscope engineers in Slack", "home-ask-form", "Connect your coding agent", "/api/v1/mcp", "Nothing is being watched yet.", "Your stack", "Just you so far", "monoscope config set project " <> pid.toText]

    [hid] <- dbQuery tr [sql|SELECT id FROM apis.home_highlights WHERE project_id = ? AND kind = 'insight'|] (Only pid)
    void $ call tr $ Home.highlightDismissPostH pid (fromOnly hid)
    dismissed <- renderT <$> call tr (Home.homeGetH pid)
    T.isInfixOf "Checkout 5xx up 4.8x" dismissed `shouldBe` False

  it "auditTrail_recordsWhoChangedWhatAndJumpBackInRemembersWhatTheyOpened" \tr -> do
    pid <- createTestProject tr "Acme Shop"
    void $ call tr $ Dashboards.dashboardsPostH pid Dashboards.DashboardForm{file = "", teams = [], title = "Checkout health", fileDir = Nothing, copyWidgetId = Nothing, sourceDashboardId = Nothing, widgetJson = Nothing}
    did <- UUIDId <$> dbOne @_ @UUID.UUID tr [sql|SELECT id FROM projects.dashboards WHERE project_id = ? AND title = 'Checkout health'|] (Only pid)
    -- The change is in the audit trail with its actor and target; the visit is in this person's recents.
    entries <- runQueryEffect tr $ Projects.projectActivity pid Nothing Nothing Nothing 10
    [(e.event, e.targetTitle, e.targetId, e.actorKind) | e <- entries, e.source == Projects.ASAudit] `shouldBe` [("dashboard_created", Just "Checkout health", Just did.toText, Projects.AKUser)]
    void $ testServant tr $ Dashboards.dashboardGetH pid did Nothing Nothing Nothing Nothing Nothing []
    owner <- UUIDId <$> dbOne @_ @UUID.UUID tr [sql|SELECT '00000000-0000-0000-0000-000000000001'::uuid|] ()
    (map (.title) <$> runQueryEffect tr (Projects.recentViews owner pid)) `shouldReturn` ["Checkout health"]
    home <- renderT <$> call tr (Home.homeGetH pid)
    home `shouldContainAll` ["Jump back in", "Checkout health", "Dashboard · "]
    audit <- renderT <$> call tr (Settings.auditLogGetH pid Nothing Nothing Nothing)
    audit `shouldContainAll` ["Audit log", "Dashboard created", "Dashboard: Checkout health", "Export CSV"]
    csv <- call tr (Settings.auditLogCsvGetH pid (Just Projects.ASAudit) Nothing)
    csv `shouldContainAll` ["\"dashboard_created\"", "\"Checkout health\""]

  it "slackConnect_createsOneChannelAndInvitesEachMemberOnce" \tr -> do
    pid <- createTestProject tr "Acme Shop"
    owner <- dbOne @_ @Text tr [sql|SELECT lower(email) FROM users.users WHERE id = '00000000-0000-0000-0000-000000000001'|] ()
    ref0 <- newIORef []
    let sync create = do
          ref <- newIORef []
          void $ runTestBackgroundWithHTTP (runHTTPRecordWith slackReply ref) tr.trTracerProvider tr.trTestClock tr.trLogger tr.trATCtx $ Jobs.processBackgroundJob tr.trATCtx (Jobs.SlackConnectSync pid create)
          reverse <$> readIORef ref
        methods = map (T.drop (T.length "https://slack.com/api/") . fst)
        swept = do
          dbExec tr [sql|DELETE FROM background_jobs WHERE payload->>'tag' = 'SlackConnectSync'|] ()
          void $ runTestBackgroundWithHTTP (runHTTPRecordWith slackReply ref0) tr.trTracerProvider tr.trTestClock tr.trLogger tr.trATCtx $ Jobs.processBackgroundJob tr.trATCtx Jobs.SlackConnectSweep
          dbOne @_ @Int tr [sql|SELECT count(*)::int FROM background_jobs WHERE payload->>'tag' = 'SlackConnectSync' AND payload->'contents'->>0 = ?|] (Only pid)
    -- A member added before the project is activated waits for the channel; nothing is created.
    sync False `shouldReturn` []
    -- slackConnect_missingScope_namesTheScope: a bot token without groups:write says so, not just "missing_scope".
    scopeless <-
      runTestBackgroundWithHTTP (runHTTPRecordWith (const "{\"ok\":false,\"error\":\"missing_scope\",\"needed\":\"groups:write\"}") ref0) tr.trTracerProvider tr.trTestClock tr.trLogger tr.trATCtx
        $ Integrations.syncSlackConnect "xoxb-test" [] pid "Acme Shop" True [owner] Nothing
    leftToMaybe (snd scopeless) `shouldBe` Just Integrations.SlackRejected{method = "conversations.create", code = "missing_scope", neededScope = Just "groups:write"}
    -- The daily sweep picks up recently active projects still without a channel, and drops them once synced.
    dbExec tr [sql|INSERT INTO apis.daily_usage (project_id, total_requests, created_at) VALUES (?, 10, ?)|] (pid, frozenTime)
    swept `shouldReturn` 1
    created <- sync True
    methods created `shouldBe` ["conversations.create", "chat.postMessage", "conversations.inviteShared"]
    foldMap (decodeUtf8 . snd) (viaNonEmpty last created) `shouldContainAll` ["channel=C0HOME", "emails=" <> T.replace "@" "%40" owner, "external_limited=false"]
    -- A later member gets one invite; members already invited are not re-sent.
    dbExec tr [sql|INSERT INTO users.users (id, first_name, last_name, email) VALUES ('00000000-0000-0000-0000-00000000a11c', 'Ada', 'L', 'ada@acme.test')|] ()
    dbExec tr [sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (?, '00000000-0000-0000-0000-00000000a11c', 'edit')|] (Only pid)
    later <- sync False
    methods later `shouldBe` ["conversations.inviteShared"]
    foldMap (decodeUtf8 . snd) (viaNonEmpty last later) `shouldContainAll` ["emails=ada%40acme.test"]
    swept `shouldReturn` 0
    Just c <- runQueryEffect tr $ Integrations.getSlackConnect pid
    (c.channelName, sort [(i.email, i.status) | i <- c.invites]) `shouldBe` ("monoscope-acme-shop", sort [("ada@acme.test", Integrations.Invited "I0INVITE"), (owner, Integrations.Invited "I0INVITE")])
    page <- renderT <$> call tr (Home.homeGetH pid)
    page `shouldContainAll` ["#monoscope-acme-shop", "Shared with the Monoscope team", "slack.com/app_redirect?channel=C0HOME&amp;team=T0MONO"]
  where
    slackReply url
      | "conversations.create" `T.isSuffixOf` url = "{\"ok\":true,\"channel\":{\"id\":\"C0HOME\",\"name\":\"monoscope-acme-shop\",\"context_team_id\":\"T0MONO\"}}"
      | "conversations.inviteShared" `T.isSuffixOf` url = "{\"ok\":true,\"invite_id\":\"I0INVITE\"}"
      | otherwise = "{\"ok\":true}"
