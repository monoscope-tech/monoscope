-- | A project end to end: its settings and API keys, members and teams, the @everyone
-- recipients, and every notification channel it can test, route, mute and disconnect.
module Pages.Projects.ProjectsSpec (spec) where

import BackgroundJobs qualified
import Control.Exception (evaluate, try)
import Control.Lens ((.~))
import Data.Aeson qualified as AE
import Data.CaseInsensitive qualified as CI
import Data.Effectful.Notify (Notification (..))
import Data.Effectful.Notify qualified as Notify
import Data.Generics.Product.Fields (field)
import Data.Text qualified as T
import Data.Time (addUTCTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Query)
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Integrations qualified as Integrations
import Models.Apis.Monitors qualified as Monitors
import Models.Projects.ProjectApiKeys qualified as Keys
import Models.Projects.ProjectMembers (Permissions (..))
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common (GrpcError (GrpcUnauthenticated), GrpcException (..))
import Pages.BodyWrapper (PageCtx (..))
import Pages.CommandPalette qualified as CommandPalette
import Pages.Projects qualified as Pages
import Pages.Settings (TestChannel (..), TestForm (TestForm), TestStatus (..))
import Pages.Settings qualified as Settings
import Pkg.Mail qualified as Mail
import Pkg.TestUtils
import Servant (getResponse)
import System.Config qualified as Config
import System.Types (ATBackgroundCtx)


-- | Set one text[] column of the project's @everyone team.
setEveryone :: TestResources -> Query -> [Text] -> IO ()
setEveryone tr col vals = dbExec tr ("UPDATE projects.teams SET " <> col <> " = ?::text[] WHERE project_id = ? AND is_everyone AND deleted_at IS NULL") (V.fromList vals, testPid)


everyone :: TestResources -> Query -> IO [Text]
everyone tr col = sort . V.toList <$> dbOne @_ @(V.Vector Text) tr ("SELECT " <> col <> " FROM projects.teams WHERE project_id = ? AND is_everyone AND deleted_at IS NULL") (Only testPid)


payloadText :: AE.Value -> Text
payloadText = decodeUtf8 . AE.encode


spec :: Spec
spec = around withTestResources do
  it "projects: listed, updated (the demo only by sudo), role-gated, members-only, deleted - stopping ingest, reports and alerts" \tr -> do
    let ownPid = UUIDId $ UUID.fromWords 0x12345678 0x9abcdef0 0x12345678 0x9abcdef0 -- the session's own project (testSessionHeader)
        listed = call tr Pages.listProjectsGetH <&> \pg -> let (ps, _, _) = pg.unwrap.content in V.toList ps
        form t = Pages.CreateProjectForm{title = t, description = "Test Description2", emails = ["test@monoscope.tech"], permissions = [PAdmin], timeZone = "Africa/Accra", weeklyNotifs = Nothing, dailyNotifs = Nothing, endpointAlerts = Nothing, errorAlerts = Nothing}
        nonSudo = tr{trSessAndHeader = tr.trSessAndHeader <&> field @"user" . field @"isSudo" .~ False}
        title = (.title) <$> projectOf tr testPid
    map (.id) <$> listed >>= (`shouldMatchList` [testPid, ownPid])
    html <- renderT <$> call tr Pages.listProjectsGetH
    filter (`T.isInfixOf` html) ["id=\"mobile-nav-toggle\"", "for=\"mobile-nav-toggle\""] `shouldBe` []

    -- The demo project ignores a non-sudo admin's edit (echoing the form back); a sudo edit lands.
    original <- title
    echoed <- call nonSudo $ Pages.createProjectPostH testPid (form "Demo Rename")
    (echoed.unwrapCreateProjectResp <&> (.form.title)) `shouldBe` Just "Demo Rename"
    title `shouldReturn` original
    updated <- call tr $ Pages.createProjectPostH testPid (form "Test Project CI2")
    (updated.unwrapCreateProjectResp <&> \r -> (r.form.title, r.form.description)) `shouldBe` Just ("Test Project CI2", "Test Description2")
    ps <- listed
    [(p.title, p.description, p.paymentPlan, p.timeZone) | p <- ps, p.id == testPid] `shouldBe` [("Test Project CI2", "Test Description2", "GraduatedPricing", "Africa/Accra")]

    -- Viewers change nothing; editors cannot delete; deletion is admin-only, and an explicit
    -- role binds sudo users too (sudo only stands in for a missing membership).
    setRole tr testPid PView
    traverse (httpStatus tr) [void $ Pages.createProjectPostH testPid (form "Forbidden"), void $ Pages.updateNotificationsChannel testPid (Pages.NotifListForm [] [] [] [] Nothing), void $ Pages.deleteProjectGetH testPid] `shouldReturn` [403, 403, 403]
    setRole tr testPid PEdit
    httpStatus tr (Pages.deleteProjectGetH testPid) `shouldReturn` 403
    setRole tr ownPid PView
    traverse (`httpStatus` Pages.deleteProjectGetH ownPid) [nonSudo, tr] `shouldReturn` [403, 403]
    setRole tr testPid PAdmin
    title `shouldReturn` "Test Project CI2"
    isJust <$> runQueryEffect tr (Projects.activeProjectById ownPid) `shouldReturn` True

    -- Membership is enforced per handler: none of these let an outsider delete, read or write.
    victim <- createTestProject tr "victim"
    for_ [void $ Pages.deleteProjectGetH victim, void $ CommandPalette.commandPaletteItemsH victim, void $ CommandPalette.commandPaletteRecentPostH victim (CommandPalette.RecentForm "planted" "/evil" "page"), void $ Settings.notificationsTestHistoryGetH victim] (assertNonMemberDenied tr victim)
    isJust <$> runQueryEffect tr (Projects.activeProjectById victim) `shouldReturn` True
    renderT <$> call tr (CommandPalette.commandPaletteItemsH victim) >>= (`shouldNotSatisfy` T.isInfixOf "/evil")

    -- Deleting the project stops its ingest, its reports and its alerts.
    let monitorId = Monitors.QueryMonitorId $ fromMaybe (error "invalid test UUID") $ UUID.fromText "de1e7ed0-0000-0000-0000-000000000001"
        countLogs marker = dbOne @_ @Int tr [sql| SELECT COUNT(*)::INT FROM otel_logs_and_spans WHERE project_id = ? AND body::text LIKE '%' || ? || '%' |] (testPid, marker :: Text)
        countReports = dbOne @_ @Int tr [sql|SELECT COUNT(*)::INT FROM apis.reports WHERE project_id = ?|] (Only testPid)
        monitor =
          def
            { Monitors.id = monitorId
            , Monitors.projectId = testPid
            , -- A real KQL query: evaluateQueryMonitor re-parses logQuery on every run, and an
              -- empty one throws (logged, silently) before it can notify.
              Monitors.logQuery = "summarize count()"
            , Monitors.logQueryAsSql = ""
            , Monitors.alertThreshold = 0 -- the log ingested before deletion fires it
            , Monitors.checkIntervalMins = 1
            , Monitors.timeWindowMins = 60 -- not `def`'s 0: query_monitors_positive_time_window rejects it
            , Monitors.alertConfig = def{Monitors.title = "Deleted project guard"}
            }
    apiKey <- createTestAPIKey tr testPid "deleted-project-ingest"
    ingestLog tr apiKey "before project deletion" frozenTime
    countLogs "before project deletion" `shouldReturn` 1
    void $ runTestBg frozenTime tr $ Monitors.queryMonitorUpsert monitor
    setEveryone tr "pagerduty_services" ["deleted-project-test"]
    setEveryone tr "disabled_channels" []
    fst <$> captureNotifs tr BackgroundJobs.checkTriggeredQueryMonitors >>= (`shouldSatisfy` any (\case PagerdutyNotification{} -> True; _ -> False))
    void $ testServant tr $ Pages.deleteProjectGetH testPid
    void $ try @SomeException $ ingestLog tr apiKey "after project deletion" (addUTCTime 1 frozenTime)
    countLogs "after project deletion" `shouldReturn` 0
    isNothing <$> runTestBgNoReset tr (Projects.activeProjectById testPid) `shouldReturn` True
    reportsBefore <- countReports
    fst <$> captureNotifs tr (BackgroundJobs.processBackgroundJob tr.trATCtx $ BackgroundJobs.DailyReports testPid) `shouldReturn` []
    countReports `shouldReturn` reportsBefore
    dbExec tr [sql|UPDATE monitors.query_monitors SET current_status = 'normal', alert_last_triggered = NULL, notification_count = 0, last_evaluated = '2020-01-01' WHERE id = ?|] (Only monitorId)
    fst <$> captureNotifs tr BackgroundJobs.checkTriggeredQueryMonitors `shouldReturn` []

  it "API keys: created, accepted, revoked, reactivated; viewers cannot mutate or see them" \tr -> do
    let keyActive kid = fmap (.active) . find ((== kid) . (.id))
    Settings.ApiGet (PageCtx _ settings) <- call tr $ Settings.apiGetH testPid
    length settings.keys `shouldBe` 1
    Settings.ApiPost pid created (Just (apiKey, keyText)) <- call tr $ Settings.apiPostH testPid Settings.GenerateAPIKeyForm{title = "Test", from = Nothing}
    (pid, length created, apiKey.title, apiKey.active, apiKey.keyPrefix) `shouldBe` (testPid, 2, "Test", True, keyText)
    ingestLog tr keyText "accepted before revoke" frozenTime
    Settings.ApiPost _ revoked Nothing <- call tr $ Settings.apiDeleteH testPid apiKey.id
    keyActive apiKey.id revoked `shouldBe` Just False
    ingestLog tr keyText "rejected after revoke" frozenTime `shouldThrow` \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False
    Settings.ApiPost _ reactivated Nothing <- call tr $ Settings.apiActivateH testPid apiKey.id
    keyActive apiKey.id reactivated `shouldBe` Just True
    ingestLog tr keyText "accepted after activation" frozenTime
    html <- renderT <$> call tr (Settings.apiGetH testPid)
    html `shouldContainAll` ["aria-label=\"Show value for Test\"", "aria-label=\"Copy Test\"", "aria-label=\"Revoke Test\"", "for=\"api-key-title\""]
    T.count "<main" html `shouldBe` 1

    keys <- runQueryEffect tr $ Keys.projectApiKeysByProjectId testPid
    setRole tr testPid PView
    viewer <- renderT <$> call tr (Settings.apiGetH testPid)
    filter (`T.isInfixOf` viewer) (["New Key", "aria-label=\"Revoke ", "aria-label=\"Activate ", "aria-label=\"Show value for ", "aria-label=\"Copy "] <> map (.keyPrefix) keys) `shouldBe` []
    traverse (httpStatus tr) [void $ Settings.apiPostH testPid (Settings.GenerateAPIKeyForm "Forbidden" Nothing), void $ Settings.apiDeleteH testPid apiKey.id, void $ Settings.apiActivateH testPid apiKey.id] `shouldReturn` [403, 403, 403]
    map (\k -> (k.id, k.active)) <$> runQueryEffect tr (Keys.projectApiKeysByProjectId testPid) `shouldReturn` map (\k -> (k.id, k.active)) keys

  it "members and teams: invite, re-role, remove, re-add; non-admins cannot; the free tier hides invitees; teams validate" \tr -> do
    dbExec tr [sql|UPDATE projects.projects SET payment_plan = 'PAID' WHERE id = ?|] (Only testPid)
    let uid = (getResponse tr.trSessAndHeader).user.id
        form emails permissions = Pages.ManageMembersForm{emails, permissions}
        save emails permissions =
          call tr (Pages.manageMembersPostH testPid Nothing (form emails permissions)) >>= \case
            Pages.ManageMembersPost (_, members, _, _, _) -> pure (V.toList members)
            _ -> fail "expected ManageMembersPost"
        roleOf email = fmap (.permission) . find ((== email) . (.email))
        setPlan :: Text -> IO ()
        setPlan plan = dbExec tr [sql|UPDATE projects.projects SET payment_plan = ? WHERE id = ?|] (plan, testPid)
    -- The invite form is a POST to the members endpoint.
    renderT <$> call tr (Pages.manageMembersGetH testPid) >>= (`shouldSatisfy` T.isInfixOf ("hx-post=\"/p/" <> testPid.toText <> "/manage_members\""))
    -- On the free tier an existing invitee has no permission, no email audience, no project
    -- in any listing, session or Slack link; upgrading restores all of it. The owner is the
    -- earliest membership row (the template's own member here).
    owner <- runQueryEffect tr (ProjectMembers.selectActiveProjectMembers testPid) >>= maybe (fail "owner missing") pure . listToMaybe
    Just invitee <- find ((== "free-invitee@example.com") . (.email)) <$> save [CI.original owner.email, "free-invitee@example.com"] [owner.permission, PView]
    setPlan "Free"
    (notifications, _) <- captureNotifs tr $ BackgroundJobs.processBackgroundJob tr.trATCtx (BackgroundJobs.InviteUserToProject owner.userId testPid "free-invitee@example.com" "Free project")
    [email.receiver | Notify.EmailNotification email <- notifications] `shouldBe` []
    runQueryEffect tr (ProjectMembers.getUserPermission testPid invitee.userId) `shouldReturn` Nothing
    runQueryEffect tr (ProjectMembers.getActiveProjectMemberByUserId testPid invitee.userId) >>= (`shouldSatisfy` isNothing)
    map (.id) <$> runQueryEffect tr (Projects.selectProjectsForUser invitee.userId) >>= (`shouldNotContain` [testPid])
    sid <- Projects.PersistentSessionId <$> UUIDV4.nextRandom
    runQueryEffect tr $ Projects.insertSession sid invitee.userId def
    session <- runQueryEffect tr (Projects.getPersistentSession sid) >>= maybe (fail "session missing") pure
    map (.id) (V.toList session.projects.getProjects) `shouldNotContain` [testPid]
    let asInvitee = tr{trSessAndHeader = tr.trSessAndHeader <&> \s -> s{Projects.user = session.user.getUser, Projects.persistentSession = session}}
        withDemo = asInvitee{trATCtx = tr.trATCtx{Config.env = tr.trATCtx.env{Config.showDemoProject = True}}}
    restricted <- call withDemo Pages.listProjectsGetH
    renderT restricted `shouldSatisfy` T.isInfixOf "Only the project owner can access it. Ask the owner to upgrade to enable team access."
    (.restrictedProjectTitles) <$> call asInvitee Pages.listProjectsGetH `shouldReturn` restricted.restrictedProjectTitles
    dbExec tr [sql|INSERT INTO apis.slack (project_id, team_id, channel_id, team_name, bot_token) VALUES (?, 'T_FREE', 'C_FREE', 'Free project', 'test-token') ON CONFLICT (project_id) DO UPDATE SET team_id = 'T_FREE'|] (Only testPid)
    runQueryEffect tr (Integrations.slackLinkProjects "T_FREE" invitee.userId) >>= (`shouldBe` 0) . length
    map (.userId) <$> runQueryEffect tr (ProjectMembers.selectActiveProjectMembers testPid) `shouldReturn` [owner.userId]
    runQueryEffect tr (ProjectMembers.getUserPermission testPid owner.userId) `shouldReturn` Just owner.permission
    map (.id) <$> runQueryEffect tr (Projects.usersByProjectId testPid) `shouldReturn` [owner.userId]
    setPlan "PAID"
    runQueryEffect tr (ProjectMembers.getUserPermission testPid invitee.userId) `shouldReturn` Just PView
    map (.email) <$> runQueryEffect tr (Projects.usersByProjectId testPid) >>= (`shouldContain` ["free-invitee@example.com"])
    renderT <$> call withDemo Pages.listProjectsGetH >>= (`shouldNotSatisfy` T.isInfixOf "Only the project owner can access it.")

    -- Invite, re-role, list, remove and re-add.
    roleOf "example@gmail.com" <$> save ["example@gmail.com"] [PAdmin] `shouldReturn` Just PAdmin
    roleOf "example@gmail.com" <$> save ["example@gmail.com"] [PView] `shouldReturn` Just PView
    Pages.ManageMembersGet (PageCtx _ (_, listed, _, _, _)) <- call tr $ Pages.manageMembersGetH testPid
    map (.email) (V.toList listed) `shouldMatchList` ["test@monoscope.tech", "example@gmail.com"]
    map (.email) <$> save [] [] >>= (`shouldNotContain` ["example@gmail.com"])
    roleOf "example@gmail.com" <$> save ["example@gmail.com"] [PView] `shouldReturn` Just PView

    -- Viewers and editors see no member controls and cannot re-role, invite or remove.
    Just guardMember <- find ((== "permission-guard@example.com") . (.email)) <$> save ["test@monoscope.tech", "permission-guard@example.com"] [PAdmin, PView]
    for_ [PView, PEdit] \role -> do
      setRole tr testPid role
      html <- renderT <$> call tr (Pages.manageMembersGetH testPid)
      filter (`T.isInfixOf` html) ["id=\"new-member-email\"", "id=\"saveMembersBtn\"", "aria-label=\"Remove member\""] `shouldBe` []
      traverse (httpStatus tr) [void $ Pages.manageMembersPostH testPid Nothing (form ["test@monoscope.tech", "permission-guard@example.com"] [PAdmin, PAdmin]), void $ Pages.deleteMemberH testPid guardMember.id] `shouldReturn` [403, 403]
      (fmap (.permission) . find ((== guardMember.id) . (.id))) <$> runQueryEffect tr (ProjectMembers.selectActiveProjectMembers testPid) `shouldReturn` Just PView
    setRole tr testPid PAdmin

    -- Team handles are unique per project and every field is validated.
    let team = Pages.TeamForm{teamName = "Hello", teamDescription = "", teamHandle = "hello", notifEmails = V.empty, teamMembers = [uid], discordChannels = V.empty, slackChannels = V.empty, phoneNumbers = V.empty, pagerdutyServices = V.empty, teamId = Nothing}
        teamError f = call tr (Pages.manageTeamPostH testPid f Nothing) >>= \case
          Pages.ManageTeamsPostError e -> pure e
          _ -> fail "expected ManageTeamsPostError"
    traverse teamError [team, team, team{Pages.teamName = ""}, team{Pages.teamHandle = "invalid handle"}, team{Pages.notifEmails = ["not-an-email"]}]
      `shouldReturn` ["", "Team handle already exists for this project.", "Team name is required", "Handle must be lowercase, no spaces, and hyphens only", "Invalid email format: not-an-email"]

    -- `INSERT … ON CONFLICT DO NOTHING RETURNING id` returns no row for an existing email, and
    -- the invite handler `error`ed on that: two people inviting one new address was a 500.
    first' <- runQueryEffect tr $ Projects.createEmptyUser "race@example.com"
    first' `shouldSatisfy` isJust
    runQueryEffect tr (Projects.createEmptyUser "race@example.com") `shouldReturn` first'

  it "@everyone notify_emails follow member changes yet stay the single source of who is paged" \tr -> do
    [alice, bob] <- forM @[] ["Alice@Example.com", "bob@example.com"] \email -> do
      u <- Projects.UserId <$> UUIDV4.nextRandom
      dbExec tr [sql| INSERT INTO users.users (id, first_name, last_name, email) VALUES (?, 'T', 'U', ?) |] (u, email :: Text)
      pure u
    let bg :: ATBackgroundCtx a -> IO a
        bg = runTestBg frozenTime tr
        team = bg (ProjectMembers.getEveryoneTeam testPid) >>= maybe (fail "no @everyone team") pure
        emails = sort . V.toList . (.notify_emails) <$> team
        addMember u = void $ bg $ ProjectMembers.insertProjectMembers [ProjectMembers.CreateProjectMembers testPid u PView]
    bg $ ProjectMembers.setEveryoneTeamEmails testPid V.empty
    addMember alice
    emails `shouldReturn` ["alice@example.com"]
    -- Membership alone pages nobody.
    bg $ ProjectMembers.setEveryoneTeamEmails testPid V.empty
    ProjectMembers.resolveTeamEmails <$> team `shouldReturn` []
    -- A manual removal persists across further additions.
    addMember bob
    emails `shouldReturn` ["bob@example.com"]
    -- Removing a member strips only their email; addresses tied to no member survive deactivation.
    bg $ ProjectMembers.setEveryoneTeamEmails testPid (V.fromList ["oncall@example.com", "alice@example.com", "bob@example.com"])
    bobMember <- dbOne @_ @ProjectMembers.ProjectMemberId tr [sql| SELECT id FROM projects.project_members WHERE project_id = ? AND user_id = ? |] (testPid, bob)
    bg $ ProjectMembers.softDeleteProjectMembers (bobMember :| [])
    emails `shouldReturn` ["alice@example.com", "oncall@example.com"]
    bg $ ProjectMembers.deactivateNonOwnerMembers testPid
    emails `shouldReturn` ["oncall@example.com"]

  it "notification channels: a test send per channel with history, trend fallback, team routing, report, disabled gating" \tr -> do
    -- History rows are stamped on the wall clock, so the 60s test-send rate limit (read on the
    -- test clock) needs a clean slate; the response is forced so a rejection fails the step.
    let send ty ch teamM = do
          dbExec tr [sql|DELETE FROM apis.notification_test_history WHERE project_id = ?|] (Only testPid)
          (ns, (h, _)) <- testServantWithNotifications tr (Settings.notificationsTestPostH testPid (TestForm ty ch teamM))
          ns <$ evaluate h
        history = (.tests) <$> call tr (Settings.notificationsTestHistoryGetH testPid)
        slackTo ns = [d.channelId | SlackNotification d <- ns]
        pagerdutyKey = "a1b2c3d4e5f67890abcdef1234567890"
        cases :: [(TestChannel, Text, IO (), [Notification] -> Expectation)]
        cases =
          [ (TCEmail, "runtime_exception", setEveryone tr "notify_emails" ["alerts@example.com"], \ns -> [(d.receiver, "[Test]" `T.isPrefixOf` d.subject) | EmailNotification d <- ns] `shouldBe` [("alerts@example.com", True)])
          , -- Slack must accept the shape: the test alert reached the default channel over the
            -- webhook transport, which never validates Block Kit.
            (TCSlack, "runtime_exception", setupSlackData tr testPid "T_SLACK_TEST", \ns -> [(d.channelId, T.isInfixOf "🧪 TEST" $ payloadText d.payload, slackPayloadViolations d.payload) | SlackNotification d <- ns] `shouldBe` [("C_NOTIF_CHANNEL", True, [])])
          , (TCDiscord, "api_change", dbExec tr [sql|INSERT INTO apis.discord (project_id, guild_id) VALUES (?, 'G_DISCORD_TEST')|] (Only testPid) >> setEveryone tr "discord_channels" ["C_DISCORD_NOTIF"], \ns -> [T.isInfixOf "🧪 TEST" $ payloadText d.payload | DiscordNotification d <- ns] `shouldBe` [True])
          , (TCWhatsapp, "runtime_exception", setEveryone tr "phone_numbers" ["+1234567890"], \ns -> [Notify.to d | WhatsAppNotification d <- ns] `shouldBe` ["+1234567890"])
          , (TCPagerduty, "runtime_exception", setEveryone tr "pagerduty_services" [pagerdutyKey], \ns -> [(Notify.integrationKey d, Notify.eventAction d) | PagerdutyNotification d <- ns] `shouldBe` [(pagerdutyKey, Notify.PDTrigger)])
          ]
    setEveryone tr "disabled_channels" []
    for_ cases \(channel, issueType, setup, verify) -> do
      setup
      send issueType channel Nothing >>= verify
      map (\h -> (h.status, h.issueType, h.channel)) <$> history `shouldReturn` [(TSSent, issueType, channel)]

    -- An unavailable runtime trend says so, and stays Slack-valid.
    let alert = Mail.RuntimeErrorAlert{issueId = "issue-trend-fallback", issueTitle = "Upstream timeout", errorData = (def :: ErrorPatterns.ATError){ErrorPatterns.hash = "slack-trend-fallback", ErrorPatterns.errorType = "UpstreamTimeout", ErrorPatterns.message = "The upstream request timed out"}, runtimeAlertType = Mail.NewRuntimeError, chartUrl = Nothing, occurrenceText = Nothing, firstSeenText = Nothing, ongoingFor = Nothing}
    (trend, _) <- captureNotifs tr $ Mail.sendSlackAlert Nothing alert testPid "Notifications" (Just "C_NOTIF_CHANNEL")
    [payload] <- pure [d.payload | SlackNotification d <- trend]
    payloadText payload `shouldContainAll` ["Trend unavailable", "Open issue"]
    (T.isInfixOf "image_url" (payloadText payload), slackPayloadViolations payload) `shouldBe` (False, [])

    -- A team routes Slack and PagerDuty to its own targets until it is deleted.
    dbExec tr [sql|DELETE FROM apis.discord WHERE project_id = ?|] (Only testPid)
    let teamKey = "svc-key-12345678901234567890abcd"
        teams = call tr (Pages.manageTeamsGetH testPid (Just "")) >>= \case
          Pages.ManageTeamsGet' (_, _, _, _, ts) -> pure ts
          _ -> fail "expected ManageTeamsGet'"
    Pages.ManageTeamsPostError "" <- call tr $ Pages.manageTeamPostH testPid Pages.TeamForm{teamName = "Routing Team", teamDescription = "Owns customer alerts", teamHandle = "routing-team", teamMembers = [(getResponse tr.trSessAndHeader).user.id], notifEmails = V.empty, slackChannels = ["#team-alerts"], discordChannels = V.empty, phoneNumbers = V.empty, pagerdutyServices = [teamKey], teamId = Nothing} Nothing
    Just team <- V.find ((== "routing-team") . (.handle)) <$> teams
    map (.memberEmail) (V.toList team.members) `shouldBe` ["test@monoscope.tech"]
    slackTo <$> send "runtime_exception" TCSlack (Just team.id) `shouldReturn` ["#team-alerts"]
    send "runtime_exception" TCPagerduty (Just team.id) >>= \ns -> [Notify.integrationKey d | PagerdutyNotification d <- ns] `shouldBe` [teamKey]
    Pages.ManageTeamsDelete <- call tr $ Pages.manageTeamBulkActionH testPid "delete" (Pages.TBulkActionForm [team.id.unwrap]) Nothing
    V.any ((== team.id) . (.id)) <$> teams `shouldReturn` False
    slackTo <$> send "runtime_exception" TCSlack (Just team.id) `shouldReturn` []

    -- A test weekly report goes to Slack.
    send "report" TCSlack Nothing >>= \ns -> [payloadText d.payload | SlackNotification d <- ns] `shouldSatisfy` any (T.isInfixOf "Weekly report")

    -- A disabled channel is skipped and recorded so; re-enabled, it fans out to every channel.
    setEveryone tr "disabled_channels" ["slack"]
    slackTo <$> send "runtime_exception" TCSlack Nothing `shouldReturn` []
    map (\h -> (h.status, h.error)) <$> history `shouldReturn` [(TSSkipped, Just "channel_disabled")]
    setEveryone tr "slack_channels" ["C_ONE", "C_TWO"]
    setEveryone tr "disabled_channels" []
    sort . slackTo <$> send "runtime_exception" TCSlack Nothing `shouldReturn` ["C_ONE", "C_TWO"]

  it "notification settings: PagerDuty and Slack connect and disconnect, saves are authoritative, identityOn is opt-out" \tr -> do
    let save enabled slack identityOn = void $ testServant tr $ Pages.updateNotificationsChannel testPid (Pages.NotifListForm enabled [] [] slack identityOn)
        slackRows = dbQuery @_ @(Text, Maybe Text) tr [sql|SELECT channel_id, team_name FROM apis.slack WHERE project_id = ?|] (Only testPid)
        key = "a1b2c3d4e5f67890abcdef1234567890"
    setEveryone tr "pagerduty_services" []
    void $ testServant tr $ Pages.pagerdutyConnectH testPid (Pages.PagerdutyConnectForm "short")
    everyone tr "pagerduty_services" `shouldReturn` []
    void $ testServant tr $ Pages.pagerdutyConnectH testPid (Pages.PagerdutyConnectForm key)
    everyone tr "pagerduty_services" `shouldReturn` [key]
    void $ testServant tr $ Pages.pagerdutyDisconnectH testPid
    everyone tr "pagerduty_services" `shouldReturn` []

    -- The Slack OAuth callback's two writes: credentials, and the default channel on @everyone.
    dbExec tr [sql|DELETE FROM apis.slack WHERE project_id = ?|] (Only testPid)
    setEveryone tr "slack_channels" []
    runTestBg frozenTime tr do
      void $ Integrations.insertAccessToken testPid "T_OAUTH" "C_OAUTH" "Workspace" "xoxb-oauth" "general" "https://hooks.slack.com/services/oauth" Nothing
      void $ ProjectMembers.addSlackChannelToEveryoneTeam testPid "C_OAUTH"
    slackRows `shouldReturn` [("C_OAUTH", Just "Workspace")]
    everyone tr "slack_channels" `shouldReturn` ["C_OAUTH"]
    void $ testServant tr $ Pages.slackDisconnectH testPid
    slackRows `shouldReturn` []
    everyone tr "slack_channels" `shouldReturn` []

    -- The @everyone save once unioned into the stored list and force-kept the OAuth default, so a
    -- channel could never be removed (alerts kept reaching #engineering for months).
    setupSlackData tr testPid "T_CHANNEL_REMOVAL"
    setEveryone tr "slack_channels" ["C_NOTIF_CHANNEL", "C_STALE"]
    save ["slack"] ["C_NOTIF_CHANNEL"] Nothing
    everyone tr "slack_channels" `shouldReturn` ["C_NOTIF_CHANNEL"]
    save ["slack"] [] Nothing
    everyone tr "slack_channels" `shouldReturn` []

    -- Disabled channels are the complement of the enabled ones; htmx 4's comma-joined value is rejected.
    save ["slack", "email"] [] Nothing
    everyone tr "disabled_channels" `shouldReturn` ["discord", "pagerduty", "phone"]
    save ["email,slack"] [] Nothing
    everyone tr "disabled_channels" `shouldReturn` ["discord", "pagerduty", "phone"]
    save [] [] Nothing
    everyone tr "disabled_channels" `shouldReturn` ["discord", "email", "pagerduty", "phone", "slack"]

    -- Alerts include user identityOn until explicitly opted out; an absent field keeps the choice.
    let identityOn = runQueryEffect tr $ Projects.includeAlertUserIdentity testPid
    identityOn `shouldReturn` True
    for_ [(Just False, False), (Nothing, False), (Just True, True)] \(choice, expected) -> do
      save [] [] choice
      identityOn `shouldReturn` expected
