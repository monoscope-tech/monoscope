module Pages.HomeSpec (spec) where

import BackgroundJobs qualified as Jobs
import Data.Effectful.Hasql qualified as EHasql
import Data.Effectful.Wreq (runHTTPRecordWith)
import Data.Text qualified as T
import Hasql.Transaction.Sessions qualified as TxS
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Issues qualified as Issues
import Pages.Home qualified as Home
import Pkg.TestUtils


spec :: Spec
spec = around withTestResources $ describe "Home screen" do
  it "home_showsTodaysIssuesImprovementsAndRoutineFlagsOnce" \tr -> do
    pid <- createTestProject tr "Acme Shop"
    dbExec tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, endpoint_hash, created_at) VALUES (?, 'runtime_exception', 'Checkout fails on a null cart', 'home-new', 'home-new', ?)|] (pid, frozenTime)
    resolvedId <- dbOne @_ @Issues.IssueId tr [sql|INSERT INTO apis.issues (project_id, issue_type, title, target_hash, endpoint_hash, created_at, archived_at) VALUES (?, 'runtime_exception', 'Payment gateway timeout', 'home-old', 'home-old', ?::timestamptz - interval '3 days', ?) RETURNING id|] (pid, frozenTime, frozenTime)
    dbExec tr [sql|INSERT INTO apis.issue_activity_log (issue_id, event, created_at) VALUES (?, 'resolved', ?)|] (resolvedId, frozenTime)
    -- A daily routine flagging the same thing twice refreshes one item instead of stacking two.
    let flag kind title = runQueryEffect tr $ EHasql.transaction TxS.ReadCommitted TxS.Write $ Issues.upsertHighlightTx pid Nothing "Error regression radar" kind title "412 to 1,980 in an hour." (Just $ "/p/" <> pid.toText <> "/issues")
    replicateM_ 2 $ flag Issues.HKInsight "Checkout 5xx up 4.8x since deploy v3.2.1"
    flag Issues.HKImprovement "p95 latency on /cart down 38%"
    dbOne @_ @Int tr [sql|SELECT count(*)::int FROM apis.home_highlights WHERE project_id = ?|] (Only pid) `shouldReturn` 2

    -- Starring the overview dashboard must not render its golden signals twice: duplicate DOM ids left the second copy loading forever.
    dbExec tr [sql|INSERT INTO projects.dashboards (id, project_id, created_at, updated_at, created_by, base_template, title, starred_since) VALUES (gen_random_uuid(), ?, now(), now(), '00000000-0000-0000-0000-000000000001', '_overview.yaml', 'Overview', now())|] (Only pid)

    page <- renderT <$> call tr (Home.homeGetH pid)
    map (\w -> T.count ("id=\"" <> w <> "_widgetEl\"") page) ["traffic", "p95-latency", "error-rate", "apdex-score"] `shouldBe` [1, 1, 1, 1]
    page `shouldContainAll` ["Today&#39;s issues", "Checkout fails on a null cart", "Today&#39;s improvements", "Payment gateway timeout", "Resolved", "p95 latency on /cart down 38%", "Flagged by AI", "Checkout 5xx up 4.8x since deploy v3.2.1", "Error regression radar", "Talk to Monoscope engineers in Slack", "home-ask-form", "Connect your coding agent", "/api/v1/mcp", "monoscope config set project " <> pid.toText]

    [hid] <- dbQuery tr [sql|SELECT id FROM apis.home_highlights WHERE project_id = ? AND kind = 'insight'|] (Only pid)
    void $ call tr $ Home.highlightDismissPostH pid (fromOnly hid)
    dismissed <- renderT <$> call tr (Home.homeGetH pid)
    T.isInfixOf "Checkout 5xx up 4.8x" dismissed `shouldBe` False

  it "slackConnect_createsOneChannelAndInvitesEachMemberOnce" \tr -> do
    pid <- createTestProject tr "Acme Shop"
    owner <- dbOne @_ @Text tr [sql|SELECT lower(email) FROM users.users WHERE id = '00000000-0000-0000-0000-000000000001'|] ()
    let sync create = do
          ref <- newIORef []
          void $ runTestBackgroundWithHTTP (runHTTPRecordWith slackReply ref) tr.trTracerProvider tr.trTestClock tr.trLogger tr.trATCtx $ Jobs.processBackgroundJob tr.trATCtx (Jobs.SlackConnectSync pid create)
          reverse <$> readIORef ref
        methods = map (T.drop (T.length "https://slack.com/api/") . fst)
    -- A member added before the project is activated waits for the channel; nothing is created.
    sync False `shouldReturn` []
    created <- sync True
    methods created `shouldBe` ["conversations.create", "chat.postMessage", "conversations.inviteShared"]
    foldMap (decodeUtf8 . snd) (viaNonEmpty last created) `shouldContainAll` ["channel=C0HOME", "emails=" <> T.replace "@" "%40" owner]
    -- A later member gets one invite; members already invited are not re-sent.
    dbExec tr [sql|INSERT INTO users.users (id, first_name, last_name, email) VALUES ('00000000-0000-0000-0000-00000000a11c', 'Ada', 'L', 'ada@acme.test')|] ()
    dbExec tr [sql|INSERT INTO projects.project_members (project_id, user_id, permission) VALUES (?, '00000000-0000-0000-0000-00000000a11c', 'edit')|] (Only pid)
    later <- sync False
    methods later `shouldBe` ["conversations.inviteShared"]
    foldMap (decodeUtf8 . snd) (viaNonEmpty last later) `shouldContainAll` ["emails=ada%40acme.test"]
    Just c <- runQueryEffect tr $ Integrations.getSlackConnect pid
    (c.channelName, sort [i.email | i <- c.invites, isJust i.inviteId]) `shouldBe` ("monoscope-acme-shop", sort ["ada@acme.test", owner])
    page <- renderT <$> call tr (Home.homeGetH pid)
    page `shouldContainAll` ["#monoscope-acme-shop", "Shared with the Monoscope team", "slack.com/app_redirect?channel=C0HOME&amp;team=T0MONO"]
  where
    slackReply url
      | "conversations.create" `T.isSuffixOf` url = "{\"ok\":true,\"channel\":{\"id\":\"C0HOME\",\"name\":\"monoscope-acme-shop\",\"context_team_id\":\"T0MONO\"}}"
      | "conversations.inviteShared" `T.isSuffixOf` url = "{\"ok\":true,\"invite_id\":\"I0INVITE\"}"
      | otherwise = "{\"ok\":true}"
