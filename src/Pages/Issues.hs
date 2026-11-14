{-# LANGUAGE OrPatterns #-}

module Pages.Issues (
  issueListGetH,
  hourlyRollupSql,
  issueBulkActionsPostH,
  IssueBulkAction (..),
  acknowledgeIssueGetH,
  archiveIssueGetH,
  resolveIssueGetH,
  TriageForm (..),
  AssigneeRejection (..),
  TriageRejection (..),
  IssueTriage,
  memberLabel,
  CommentForm (..),
  SaveViewForm (..),
  saveViewPostH,
  deleteViewPostH,
  UnnamedView (..),
  ViewChanged,
  TimelineRejection (..),
  TimelineLogged,
  LinkForm (..),
  commentPostH,
  issueViewedPostH,
  linkPostH,
  triagePostH,
  issueDetailGetH,
  IssueDetail (..),
  IssueView (..),
  IssueSample (..),
  IssueSampleResult (..),
  IssueBulkForm (..),
  issueAcknowledgeButton,
  issueArchiveButton,
  issueDetailHashGetH,
  EventRef (..),
  issueStepGetH,
  IssueAction (..),
  IssueVM (..),
  AssignErrorForm (..),
  assignErrorPostH,
  resolveErrorPostH,
  ErrorPatternRejection (..),
  AssigneeChanged,
  ErrorResolved,
  ErrorSubscribed,
  ErrorSubscriptionForm (..),
  errorSubscriptionPostH,
  -- AI Chat
  AIChatForm (..),
  aiChatPostHWithTools,
  aiChatHistoryGetH,
  IssueChatHistory (..),
  -- Activity
  issueActivityGetH,
  IssueActivityFeed (..),
  issueSampleGetH,
  -- Pattern group members
  errorGroupMembersGetH,
  ErrorGroupMembers (..),
  errorUnmergePostH,
  PatternUnmerged,
  -- Shared rendering helpers
  issueCardCompact_,
  aiChatHistoryView_,
  chatAvatar_,
  aiChatWidgets_,
)
where

import BackgroundJobs qualified
import BackgroundJobs.Types qualified as Jobs
import Control.Exception qualified as Exception
import Control.Lens (preview, (%~))
import Data.Aeson qualified as AE
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.CaseInsensitive qualified as CI
import Data.Default (def)
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.UUID qualified as UUIDEff
import Data.Generics.Product.Fields qualified as Optics
import Data.Generics.Sum.Constructors (_Ctor)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Hashable (hash)
import Data.List (partition)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Ord (clamp)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Deriving.Aeson qualified as DAE
import Effectful.Concurrent.Async (concurrently)
import Effectful.Error.Static (throwError)
import Effectful.Exception (trySync)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Effectful.Timeout (timeout)
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxSelect_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.ApiChanges qualified as ApiChanges
import Models.Apis.ErrorPatterns (ErrorPatternId (..))
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Issues (EventRef (..), IssueBulkAction (..))
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (sourceFieldLabel)
import Models.Apis.LogPatterns qualified as LogPatterns
import Models.Apis.LogQueries qualified as LogQueries
import Models.Apis.PatternMerge qualified as PatternMerge
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (User (id))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Telemetry qualified as Telemetry
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), checkFreeTierStatus, mkPageCtx, navTabAttrs)
import Pages.Bots.Utils qualified as Bots
import Pages.Components (BadgeColor (..), EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), SavedView (..), SavedViewsMenu (..), agoText, codeContextUrl, colorChip_, copyButton_, curlCommand, detailsClosedBelowAttr_, disclosure_, durationMenu_, emptyState_, eventsCountCell_, externalLink_, fieldLabel_, filterInputAttr_, inlineIconBtn_, kvList_, lastSeenCell_, localTimeFmt_, metadataChip_, options_, periodToggle_, resetFormOnRefusalAttr_, resizer_, savedViewsMenu_, sectionLabel_, silenceBadge_, sparkline_, stackFrames_, statFigure_, tonePill_, untilLabel)
import Pages.LogExplorer.Log (virtualTable)
import Pages.Replay qualified as Replay
import Pages.Telemetry (traceFragmentUrl)
import Pkg.AI qualified as AI
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableResponse (..), TableRows (..), ZeroState (..), col, multiSelectFilter, withAttrs, withColHeaderExtra)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), assetUrl)
import Pkg.ErrorFingerprint qualified as EF
import Pkg.Mail qualified as Mail
import Pkg.Parser (ScopedQuery (..), applyScopedKqlContext, mkScopedQuery)
import Pkg.Parser.Expr (sqlStringLit)
import Pkg.SchemaLearning (FieldStruct (..))
import Pkg.SchemaLearning qualified as SchemaLearning
import PyF (fmt)
import Relude hiding (ask)
import Relude.Extra.Foldable1 (minimum1)
import Servant (Header, Headers, NoContent (..), ServerError (errBody), addHeader, err400, err403, err404, err409, err502)
import System.Config (AuthContext (..), EnvConfig (..))
import System.IO.Error (userError)
import System.Logging qualified as Log
import System.Types (ATAuthCtx, ActionResult, Done, Outcome (..), Refusal (..), Rendered (..), RespHeaders, Says (..), act, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent, done, successToast, useTfReads)
import Utils (LoadingSize (..), LoadingType (..), TabStrip (..), countNoun, faSprite_, fmtDate, formatOffset, formatUTC, formatWithCommas, hostPath, isoT, kqlQuoted, loadingIndicator_, lookupValueText, navTabStrip_, nonEmptyT, popoverPanel_, popoverTrigger_, renderMarkdown, summaryWords)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (parseQueryParamMaybe)
import Web.Links (CookieProtectedRoutes (..), IssuesRoutes' (..), LogExplorerRoutes' (..), MonitorsRoutes' (..), TelemetryRoutes' (..), explorerUrl, urls)


newtype IssueBulkForm = IssueBulk
  { itemId :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Acknowledge (on=True) or un-acknowledge (on=False) an issue. @durationM@ is
-- the silence window in minutes; absent means indefinitely — until the issue
-- regresses or someone un-acks it. Acknowledging is the *only* way to stop
-- notifications for an issue, so it cascades to the sibling issues sharing the
-- endpoint prefix.
acknowledgeIssueGetH :: Projects.ProjectId -> Bool -> Issues.IssueId -> Maybe Int -> ATAuthCtx (RespHeaders IssueAction)
acknowledgeIssueGetH pid enable issueId durationM = do
  (sess, _) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  let window = maybe Issues.AckIndefinite Issues.AckFor durationM
      until' = Issues.ackUntil now window
  ackState <-
    if enable
      then do
        -- Every swept sibling, not just the one clicked: the cascade acknowledges
        -- them all, so they all owe their timeline an entry saying why.
        acked <- Issues.ackCascade pid sess.user.id window [issueId]
        forM_ acked \i -> Issues.logIssueActivity i Issues.IEAcknowledged (Just sess.user.id) (Just $ AE.object ["until" AE..= until'])
        addSuccessToast (untilLabel "Acknowledged" now until' <> " \x2014 notifications paused") Nothing
        pure $ Just until'
      else do
        void $ Issues.setAckState pid [issueId] Nothing
        Issues.logIssueActivity issueId Issues.IEUnacknowledged (Just sess.user.id) Nothing
        addSuccessToast "Back in the Inbox \x2014 notifications resumed" Nothing
        pure Nothing
  addTriggerEvent "issuesListChanged" AE.Null
  addRespHeaders $ Acknowledge pid issueId now ackState


-- | Archive (Just window) or un-archive (Nothing) an issue. A timed or "until
-- escalating" archive lifts itself; see 'Issues.ArchiveWindow'.
archiveIssueGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Issues.ArchiveWindow -> ATAuthCtx (RespHeaders IssueAction)
archiveIssueGetH pid issueId windowM =
  setArchived pid issueId windowM (bool Issues.IEUnarchived Issues.IEArchived (isJust windowM)) (maybe "Restored to the Inbox" archivedToast windowM)
  where
    archivedToast = \case
      Issues.ArchiveIndefinite -> "Archived \x2014 notifications stopped"
      Issues.ArchiveFor mins -> "Archived for " <> show mins <> " min \x2014 it returns to the Inbox after that"
      Issues.ArchiveUntilEscalating -> "Archived until the error escalates"


-- | Resolve an issue that has no error pattern behind it: the fix is done, so it
-- leaves the Inbox, and the timeline says resolved rather than archived.
resolveIssueGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders IssueAction)
resolveIssueGetH pid issueId = setArchived pid issueId (Just Issues.ArchiveIndefinite) Issues.IEResolved "Resolved"


setArchived :: Projects.ProjectId -> Issues.IssueId -> Maybe Issues.ArchiveWindow -> Issues.IssueEvent -> Text -> ATAuthCtx (RespHeaders IssueAction)
setArchived pid issueId windowM event toast = do
  (sess, _) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  void $ Issues.setArchiveState pid [issueId] ((now,) <$> windowM)
  Issues.logIssueActivity issueId event (Just sess.user.id) Nothing
  addSuccessToast toast Nothing
  addTriggerEvent "issuesListChanged" AE.Null
  addRespHeaders $ Archive pid issueId (isJust windowM)


data TriageForm = TriageForm {severity :: Maybe Issues.IssueSeverity, assigneeId :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data AssigneeRejection = MalformedAssignee Text | NotAMember
  deriving stock (Eq, Show)


instance Refusal AssigneeRejection where
  refusalMessage = \case
    MalformedAssignee t -> "Not a user id: " <> t
    NotAMember -> "Assignee must be an active project member"


data TriageRejection = UnknownIssue | TriageAssignee AssigneeRejection
  deriving stock (Eq, Show)


instance Refusal TriageRejection where
  refusalMessage = \case
    UnknownIssue -> "Issue not found"
    TriageAssignee a -> refusalMessage a


data IssueTriage = IssueTriage Projects.ProjectId Issues.Issue [ProjectMembers.ProjectMemberVM]
  deriving (Outcome) via Rendered IssueTriage


instance ToHtml IssueTriage where
  toHtml (IssueTriage pid issue members) = toHtml $ issueTriage_ pid issue members
  toHtmlRaw = toHtml


-- | Priority and assignee, set from the issue header on any issue type. A refused assignee saves neither.
triagePostH :: Projects.ProjectId -> Issues.IssueId -> TriageForm -> ATAuthCtx (ActionResult TriageRejection IssueTriage)
triagePostH pid issueId form = act do
  (sess, _) <- Projects.sessionAndProject pid
  members <- ProjectMembers.selectActiveProjectMembers pid
  Issues.selectIssueById pid issueId >>= \case
    Nothing -> pure $ Left UnknownIssue
    Just issue ->
      forM (first TriageAssignee $ parseAssignee members form.assigneeId) \assigneeM -> do
        whenJust form.severity $ void . Issues.setIssuePriority pid [issueId]
        addTriggerEvent "issuesListChanged" AE.Null
        assignIssues pid sess.user.id [issue] assigneeM
        saved <- fromMaybe issue <$> Issues.selectIssueById pid issueId
        pure $ IssueTriage pid saved members


-- | The header's Priority and Assignee selects; one form, re-rendered on change.
issueTriage_ :: Projects.ProjectId -> Issues.Issue -> [ProjectMembers.ProjectMemberVM] -> Html ()
issueTriage_ pid issue members =
  form_ [id_ "issue-triage", class_ "ml-auto max-md:w-full flex flex-wrap items-center gap-3 text-xs text-textWeak", hxPost_ $ (urls.issues pid).triagePost issue.id, hxTrigger_ "change", hxSwap_ "outerHTML", resetFormOnRefusalAttr_] do
    label_ [class_ "flex items-center gap-1.5 max-md:flex-1 min-w-0"] do
      "Priority"
      select_ [id_ "issue-triage-priority", class_ "select select-sm md:w-28 max-md:flex-1 min-w-0", name_ "severity", Aria.label_ "Priority"]
        $ options_ (Just $ display issue.severity) [(display sev, T.toTitle $ display sev) | sev <- [minBound .. maxBound :: Issues.IssueSeverity]]
    label_ [class_ "flex items-center gap-1.5 max-md:flex-1 min-w-0"] do
      "Assignee"
      select_ [id_ "issue-triage-assignee", class_ "select select-sm md:w-44 max-md:flex-1 min-w-0", name_ "assigneeId", Aria.label_ "Assignee"]
        $ options_ (Just $ maybe "" (.toText) issue.assigneeId) (("", "Unassigned") : [(m.userId.toText, memberLabel m) | m <- members])


-- | The assignee on issues, mirrored onto their error patterns (whose assignment email and
-- resolve permission read it). Unchanged issues are skipped, so nothing is logged twice.
-- The assignee comes from 'parseAssignee'.
assignIssues :: Projects.ProjectId -> Projects.UserId -> [Issues.Issue] -> Maybe Projects.UserId -> ATAuthCtx ()
assignIssues pid actor issues assigneeM = do
  let changed = filter ((/= assigneeM) . (.assigneeId)) issues
  void $ Issues.setIssueAssignee pid ((.id) <$> changed) assigneeM
  forM_ changed \i -> Issues.logIssueActivity i.id (maybe Issues.IEUnassigned (const Issues.IEAssigned) assigneeM) (Just actor) (assigneeM <&> \u -> AE.object ["assignee_id" AE..= u])
  issueErrors pid changed >>= traverse_ (assignError pid assigneeM . snd)


-- | Set an error pattern's assignee, queueing the assignment email.
assignError :: Projects.ProjectId -> Maybe Projects.UserId -> ErrorPatterns.ErrorPattern -> ATAuthCtx ()
assignError pid assigneeM err = do
  now <- Time.currentTime
  appCtx <- ask @AuthContext
  void $ ErrorPatterns.setErrorPatternAssignee err.id assigneeM now
  whenJust assigneeM \a -> void $ liftIO $ withResource appCtx.pool \conn -> createJob conn "background_jobs" $ BackgroundJobs.ErrorAssigned pid err.id a


-- | An assignee field: empty means unassigned, anything else must be one of @members@' user ids. A
-- malformed id used to parse to "unassigned" and silently clear the assignee.
--
-- >>> map (parseAssignee []) [Nothing, Just "", Just "00000000-0000-0000-0000-000000000001", Just "bob"]
-- [Right Nothing,Right Nothing,Left NotAMember,Left (MalformedAssignee "bob")]
parseAssignee :: [ProjectMembers.ProjectMemberVM] -> Maybe Text -> Either AssigneeRejection (Maybe Projects.UserId)
parseAssignee members = \case
  Nothing -> Right Nothing
  Just "" -> Right Nothing
  Just t -> case UUIDId <$> UUID.fromText t of
    Nothing -> Left $ MalformedAssignee t
    Just uid -> bool (Left NotAMember) (Right $ Just uid) (any ((== uid) . (.userId)) members)


-- | The runtime-exception issues among these, each with its error pattern.
issueErrors :: Projects.ProjectId -> [Issues.Issue] -> ATAuthCtx [(Issues.Issue, ErrorPatterns.ErrorPattern)]
issueErrors pid issues = catMaybes <$> forM issues \i -> fmap (i,) <$> Issues.issueError pid i


memberLabel :: ProjectMembers.ProjectMemberVM -> Text
memberLabel m = let n = T.strip (m.first_name <> " " <> m.last_name) in bool n (CI.original m.email) (T.null n)


data IssueAction
  = -- | @Just until@ when acknowledged, alongside the render clock.
    Acknowledge Projects.ProjectId Issues.IssueId UTCTime (Maybe UTCTime)
  | Archive Projects.ProjectId Issues.IssueId Bool
  | Bulk


instance ToHtml IssueAction where
  toHtml (Acknowledge pid aid now untilM) = toHtml $ issueAcknowledgeButton pid aid now untilM
  toHtml (Archive pid aid is_arch) = toHtml $ issueArchiveButton pid aid is_arch
  toHtml Bulk = ""
  toHtmlRaw = toHtml


issueBulkActionsPostH :: Projects.ProjectId -> IssueBulkAction -> Maybe Int -> Maybe Text -> IssueBulkForm -> ATAuthCtx (RespHeaders IssueAction)
issueBulkActionsPostH pid action durationM valueM items = do
  (sess, _) <- Projects.sessionAndProject pid
  if null items.itemId
    then do
      addErrorToast "No items selected" Nothing
      addRespHeaders Bulk
    else do
      now <- Time.currentTime
      let issueIds = UUIDId <$> mapMaybe UUID.fromText items.itemId
          window = maybe Issues.AckIndefinite Issues.AckFor durationM
          until' = Issues.ackUntil now window
      issues <- catMaybes <$> traverse (Issues.selectIssueById pid) issueIds
      let selectedErrors = issueErrors pid issues
      -- Acknowledging sweeps siblings the selection never named, so each branch
      -- reports the rows it actually touched and those are what get logged.
      (logIds, eventM, msg) <- case action of
        BAAcknowledge -> do
          acked <- Issues.ackCascade pid sess.user.id window issueIds
          pure (acked, Just Issues.IEAcknowledged, untilLabel "Acknowledged" now until' <> " \x2014 notifications paused")
        BAUnacknowledge -> do
          void $ Issues.setAckState pid issueIds Nothing
          pure (issueIds, Just Issues.IEUnacknowledged, "Back in the Inbox \x2014 notifications resumed")
        BAArchive -> do
          void $ Issues.setArchiveState pid issueIds (Just (now, Issues.ArchiveIndefinite))
          pure (issueIds, Just Issues.IEArchived, "Archived \x2014 notifications stopped")
        BAUnarchive -> do
          void $ Issues.setArchiveState pid issueIds Nothing
          pure (issueIds, Just Issues.IEUnarchived, "Restored to the Inbox")
        -- An error-backed issue resolves its error pattern and incident; the rest leave the Inbox.
        BAResolve -> do
          selectedErrors >>= traverse_ \(_, err) -> void $ resolveErrorAs sess.user err now
          void $ Issues.setArchiveState pid issueIds (Just (now, Issues.ArchiveIndefinite))
          pure (issueIds, Just Issues.IEResolved, "Resolved")
        BAPriority -> case valueM >>= parseQueryParamMaybe @Issues.IssueSeverity of
          Just sev -> Issues.setIssuePriority pid issueIds sev $> ([], Nothing, "Priority set to " <> display sev)
          Nothing -> pure ([], Nothing, "Pick a priority")
        BAAssign -> do
          members <- ProjectMembers.selectActiveProjectMembers pid
          case parseAssignee members valueM of
            Right assigneeM -> assignIssues pid sess.user.id issues assigneeM $> ([], Nothing, maybe "Unassigned" (const "Assigned") assigneeM)
            Left _ -> pure ([], Nothing, "Pick a member to assign")
        BAMerge ->
          Issues.mergeIssues pid (Just sess.user.id) Nothing issues <&> \case
            Just (canon, merged) -> ([], Nothing, "Merged " <> show (length merged) <> " into " <> canon.title)
            Nothing -> ([], Nothing, "Select two or more issues to merge")
        BAUnmerge ->
          Issues.unmergeIssues pid (Just sess.user.id) issues <&> \case
            [] -> ([], Nothing, "None of these were merged")
            ids -> ([], Nothing, "Unmerged " <> show (length ids) <> " \x2014 back in the Inbox")
        BASpam -> Issues.setSpamState pid issueIds (Just now) $> (issueIds, Just Issues.IESpam, "Marked as spam")
        BAUnspam -> Issues.setSpamState pid issueIds Nothing $> (issueIds, Just Issues.IENotSpam, "No longer spam")
      forM_ eventM \ev -> forM_ logIds \u -> Issues.logIssueActivity u ev (Just sess.user.id) Nothing
      addSuccessToast msg Nothing
      addTriggerEvent "issuesListChanged" AE.Null
      addRespHeaders Bulk


issueDetailGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe EventRef -> ATAuthCtx (RespHeaders (PageCtx IssueDetail))
issueDetailGetH pid issueId firstM sinceM fromM toM eventM = issueDetailCore pid firstM eventM (TimePicker.TimePicker sinceM fromM toM) $ Issues.selectIssueById pid issueId


issueDetailHashGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe EventRef -> ATAuthCtx (RespHeaders (PageCtx IssueDetail))
issueDetailHashGetH pid issueId firstM sinceM fromM toM eventM = issueDetailCore pid firstM eventM (TimePicker.TimePicker sinceM fromM toM) $ Issues.selectIssueByHash pid issueId Issues.AnyIssue


issueDetailCore :: Projects.ProjectId -> Maybe Text -> Maybe EventRef -> TimePicker.TimePicker -> ATAuthCtx (Maybe Issues.Issue) -> ATAuthCtx (RespHeaders (PageCtx IssueDetail))
issueDetailCore pid firstM eventM requestedRange fetchIssue = do
  (sess, project, bw) <- mkPageCtx pid
  issueM <- fetchIssue
  now <- Time.currentTime
  useTf <- useTfReads
  let baseBwconf = bw{pageTitle = "Issues", menuItem = Just "Issues"}
  case issueM of
    Nothing ->
      addRespHeaders $ PageCtx baseBwconf{pageTitle = "Issue Not Found"} $ IssueNotFound pid
    Just issue -> do
      requestId <- UUIDId <$> UUIDEff.genUUID
      -- The default window is anchored on the issue's own last activity rather than
      -- on @now@, and is kept narrow. Both halves are load-bearing, and the width is
      -- measured rather than guessed.
      --
      -- The charts filter with @hashes[*]==@, a TimeFusion array scan with no index
      -- behind it, so cost tracks the rows in the window. Counting one error hash:
      --
      --     4h   3.2s        8h  10.3s        24h  47.8s        3d  60s (timeout)
      --
      -- `defaultSinceRange` used to hand a day-old issue 3D, so every runtime
      -- exception past its first day rendered "Query timed out" instead of a chart.
      -- Widening is one click on the picker; a chart nobody waits for is not.
      --
      -- Anchoring matters independently of width: this issue last fired 38h ago, so
      -- a now-anchored window of any size answers quickly and finds nothing. Between
      -- them the reader got a timeout or an empty chart, never the data.
      --
      -- The relative branch stays because a *live* issue's last activity is ~now, and
      -- a trailing 24H window over recent (sparse, hot) partitions measures ~3.4s —
      -- the same budget the 4h bracket buys over the dense older ones.
      -- A log-pattern issue row is written once; its pattern keeps the live seen/count.
      patternM <- case Issues.issuePayload issue of
        Just (Issues.LogPatternP d) -> LogPatterns.getLogPatternByHash pid d.sourceField d.patternHash
        Just (Issues.LogPatternRateChangeP d) -> LogPatterns.getLogPatternByHash pid d.sourceField d.patternHash
        Just (Issues.RuntimeExceptionP _) -> pure Nothing
        Just (Issues.ApiChangeP _) -> pure Nothing
        Just (Issues.QueryAlertP _) -> pure Nothing
        Just (Issues.PerformanceP _) -> pure Nothing
        Just (Issues.FrontendP _) -> pure Nothing
        Just (Issues.UptimeP _) -> pure Nothing
        Just (Issues.CronP _) -> pure Nothing
        Just (Issues.FeedbackP _) -> pure Nothing
        Nothing -> pure Nothing
      stateEvent <- enriching "latest_state_event" $ Issues.selectLatestStateEvent issue.id
      errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternLByHash pid issue.targetHash now) (issue.issueType == Issues.RuntimeException)
      canResolve <- case errorM of
        Nothing -> pure False
        Just errL -> do
          userPermission <- ProjectMembers.getUserPermission pid sess.user.id
          pure $ userPermission >= Just ProjectMembers.PEdit || errL.base.assigneeId == Just sess.user.id
      members <- ProjectMembers.selectActiveProjectMembers pid
      tagCounts <- foldMapM (ErrorPatterns.selectErrorTagCounts . (.base.id)) errorM
      errHourlyFrom <- join <$> traverse (ErrorPatterns.hourlyStatsFrom pid . (.base.id)) errorM
      let bwconf =
            baseBwconf
              { prePageTitle = Just "Issues"
              , pageTitle = "#" <> show issue.seqNum
              , headContent = Just do highlightJsHead_; style_ "#crisp-chatbox { display: none !important; }"
              }
      -- Every trace lookup carries the point-in-time the issue is known to have
      -- happened, so the query stays a ±5min window instead of a multi-day
      -- trace_id scan (a full-table scan on TF; see 2026-07-21 crash).
      let isFirst = isJust firstM
      -- An explicitly selected event (‹ › stepping, the events table) wins over First/Recent.
      let defaultTraceRef = case Issues.issuePayload issue of
            Just (Issues.RuntimeExceptionP _) -> case errorM of
              Nothing -> pure Nothing
              Just errL -> do
                refs <- enriching "trace_references" $ ErrorPatterns.selectErrorTraceRefs pid issue.targetHash
                let base = errL.base
                    (selectedTraceId, capturedAt) = case refs of
                      Just r -> if isFirst then (r.firstTraceId, r.firstTraceAt) else (r.recentTraceId, r.recentTraceAt)
                      Nothing -> (if isFirst then base.firstTraceId else base.recentTraceId, Nothing)
                    -- Historical rows can recover an exact time only when stored error
                    -- data describes the selected trace. Otherwise retain the bounded
                    -- legacy approximation; do not turn an unknown time into a broad scan.
                    recordedAt = guard (isJust selectedTraceId && selectedTraceId == base.errorData.traceId) $> base.errorData.when
                    approximateAt = zonedTimeToUTC $ if isFirst || selectedTraceId == base.firstTraceId then base.createdAt else base.updatedAt
                pure $ (,fromMaybe approximateAt (recordedAt <|> capturedAt)) <$> selectedTraceId
            Just (Issues.ApiChangeP d) -> case d.subject of
              Just (ApiChanges.SpanSchema _) -> pure Nothing
              _ -> Telemetry.getEndpointTraceId pid d.endpointMethod d.endpointPath isFirst now
            Just (Issues.QueryAlertP _) -> pure Nothing
            Just (Issues.LogPatternP _) -> pure Nothing
            Just (Issues.LogPatternRateChangeP _) -> pure Nothing
            Just (Issues.PerformanceP d) -> pure $ Just (d.traceId, d.observedAt)
            Just (Issues.FrontendP d) -> pure $ (,d.observedAt) <$> d.traceId
            Just (Issues.UptimeP _) -> pure Nothing
            Just (Issues.CronP _) -> pure Nothing
            Just (Issues.FeedbackP d) -> pure $ (,d.observedAt) <$> d.traceId
            Nothing -> pure Nothing
      mTraceRef <- maybe defaultTraceRef (\ev -> pure $ Just (ev.traceId, ev.at)) eventM
      let recordedAt = case Issues.issuePayload issue of
            Just (Issues.RuntimeExceptionP d) -> maybe (if isFirst then d.firstSeen else d.lastSeen) (\e -> zonedTimeToUTC $ if isFirst then e.base.createdAt else e.base.updatedAt) errorM
            Just (Issues.LogPatternP d) -> maybe d.firstSeenAt (zonedTimeToUTC . (.lastSeenAt)) patternM
            Just (Issues.LogPatternRateChangeP d) -> d.detectedAt
            Just (Issues.QueryAlertP d) -> d.triggeredAt
            Just (Issues.PerformanceP d) -> d.observedAt
            Just (Issues.FrontendP d) -> d.observedAt
            Just (Issues.FeedbackP d) -> d.observedAt
            Just (Issues.UptimeP d) -> d.downSince
            Just (Issues.CronP d) -> d.expectedBy
            Just (Issues.ApiChangeP _) -> zonedTimeToUTC issue.createdAt
            Nothing -> zonedTimeToUTC issue.createdAt
          selectedAt = maybe recordedAt snd mTraceRef
          defaultSinceRange
            | ageH < 1 = "1H" :: Text
            | ageH < 3 = "3H"
            | ageH < 6 = "6H"
            | otherwise = "24H"
            where
              ageH = diffUTCTime now selectedAt / 3600
          tp
            | any (maybe False (not . T.null)) [requestedRange.since, requestedRange.from, requestedRange.to] = requestedRange
            | isFirst || isJust eventM || issue.issueType == Issues.QueryAlert || diffUTCTime now selectedAt > 43200 = windowAround 7200 selectedAt
            | otherwise = TimePicker.TimePicker (Just defaultSinceRange) Nothing Nothing
      -- The trace is supporting evidence, not the page. It used to be fetched here
      -- and a cold read of a multi-thousand-span trace took >56s, so the gateway
      -- 504'd the whole issue. The Trace section now pulls it as its own
      -- HTMX fragment, and the only thing this page still needs from the trace is
      -- the session id for the replay section — one scalar, not 1300 rows.
      tracedSession <- enriching "replay_session_id" $ flip foldMapM mTraceRef \(tId, tTs) ->
        listToMaybe @Text
          <$> Hasql.interpTimefusion
            useTf
            [HI.sql| SELECT attributes___session___id FROM otel_logs_and_spans
                    WHERE project_id = #{pid.toText}
                      AND timestamp BETWEEN #{addUTCTime (-300) tTs} AND #{addUTCTime 300 tTs}
                      AND context___trace_id = #{tId}
                      AND attributes___session___id IS NOT NULL AND attributes___session___id <> ''
                    LIMIT 1 |]
      -- Having a session id is not having a recording: the id is telemetry, the
      -- recording is Postgres. An SDK that sets session.id without recording gets
      -- a player that can only say "no events found", so the panel is rendered
      -- only once a recording exists. A non-UUID session key (backend SDKs
      -- without setSession, where the session is derived from user identity)
      -- can never have one, so it never reaches the lookup.
      let sessionM = tracedSession <|> listToMaybe ([sid | Just (Issues.FrontendP d) <- [Issues.issuePayload issue], Just sid <- [d.sessionId]] <> [sid | Just (Issues.FeedbackP d) <- [Issues.issuePayload issue], Just sid <- [d.sessionId]])
      replaySession <-
        enriching "replay_recording"
          $ join
          <$> forM (UUID.fromText =<< sessionM) \sid ->
            fmap (UUID.toText sid,) <$> Replay.sessionMetadata pid sid (one ("issue_id", issue.id.toText))
      apiEvidence <- enriching "api_change_evidence" $ forM (apiChangeData issue) \d -> do
        changes <- ApiChanges.getAnomaliesVM pid d.anomalyHashes
        schemas <- SchemaCatalog.getByKeysBatch (V.singleton (pid, issue.targetHash))
        pure (changes, HM.lookup (pid, issue.targetHash) schemas)
      secret <- (.env.apiKeyEncryptionSecretKey) <$> ask @AuthContext
      addRespHeaders
        $ PageCtx bwconf
        $ IssueFound
        $ let view = IssueView{pid, issue, requestId, apiEvidence, traceRef = mTraceRef, selectedAt, replaySession, errM = errorM, patternM, now, isFirst, tp, stateEvent, canResolve, members, tagCounts, errHourlyFrom, rollupSqlSig = Nothing}
           in view{rollupSqlSig = issueRollupSql view <&> \(sql, _) -> Widget.sqlSignature secret pid Nothing (Just sql)}
  where
    -- A lookup that only decides whether an optional panel renders: a throw or a 10s stall
    -- degrades to Nothing. A transient TF error used to 500 the whole issue, precisely
    -- when TF is degraded and someone is reading about an incident.
    enriching :: Text -> ATAuthCtx (Maybe a) -> ATAuthCtx (Maybe a)
    enriching what = fmap join . tryWithin (Just 10_000_000) "ISSUE_DETAIL_OPTIONAL_LOOKUP" ["lookup" AE..= what]


-- | ‹ › stepping (and Recommended): the matching event carrying the issue's hash within a day of
-- @current@ (the hash predicate is an unindexed array scan, so the window is the cost bound). With
-- none the reader stays on @current@: a redirect cannot carry a toast, and the default event would
-- lose their place.
issueStepGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Telemetry.EventStep -> Maybe EventRef -> ATAuthCtx (Headers '[Header "Location" Text] NoContent)
issueStepGetH pid issueId dirM current = do
  _ <- Projects.sessionAndProject pid
  useTf <- useTfReads
  issueM <- Issues.selectIssueById pid issueId
  from <- maybe Time.currentTime (pure . (.at)) current
  nextM <-
    join . join <$> forM (issueHashKey =<< issueM) \key ->
      tryWithin (Just 5_000_000) "ISSUE_STEP" ["issue_id" AE..= issueId] $ Hasql.withHasqlTimefusion useTf $ Telemetry.adjacentHashEvent pid key (fromMaybe Telemetry.Older dirM) from
  pure $ addHeader ((urls.issues pid).detailGet issueId Nothing Nothing Nothing Nothing (uncurry EventRef <$> nextM <|> current)) NoContent


-- | @secs@ either side of @t@.
windowAround :: NominalDiffTime -> UTCTime -> TimePicker.TimePicker
windowAround secs t = TimePicker.TimePicker Nothing (Just $ isoT $ addUTCTime (-secs) t) (Just $ isoT $ addUTCTime secs t)


issueDetailUrl :: Projects.ProjectId -> Issues.IssueId -> Text
issueDetailUrl pid iid = (urls.issues pid).detailGet iid Nothing Nothing Nothing Nothing Nothing


issuesListUrl :: Projects.ProjectId -> Text
issuesListUrl pid = (urls.issues pid).listGet Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] []


-- | The key an issue's events carry in @hashes@, for types keyed by an event hash.
issueHashKey :: Issues.Issue -> Maybe Text
issueHashKey issue = (<> issue.targetHash) <$> Issues.hashPrefix issue.issueType


-- The snapshot is available immediately; telemetry never holds up the page shell.
data IssueSampleResult
  = SampleLoading
  | SampleUnavailable
  | SampleEmpty
  | SampleFound UTCTime (V.Vector Text) (Maybe Text)
  deriving stock (Eq, Show)


issueSampleGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders IssueSample)
issueSampleGetH pid issueId sinceM fromM toM = do
  _ <- Projects.sessionAndProject pid
  issueM <- Issues.selectIssueById pid issueId
  now <- Time.currentTime
  appCtx <- ask @AuthContext
  case issueM of
    Just issue | issue.issueType `elem` [Issues.LogPattern, Issues.LogPatternRateChange] -> do
      let (rangeStart, rangeEnd, _) = TimePicker.parseTimeRange now $ TimePicker.TimePicker sinceM fromM toM
          from = fromMaybe (addUTCTime (-3600) now) rangeStart
          to = fromMaybe now rangeEnd
          patHash = LogQueries.patHashPrefix <> issue.targetHash
          snapshot = case Issues.issuePayload issue of
            Just (Issues.LogPatternP d) -> d.sampleMessage
            Just (Issues.LogPatternRateChangeP d) -> d.sampleMessage
            _ -> Nothing
      result <-
        tryWithin (Just 5_000_000) "ISSUE_SAMPLE" ["issue_id" AE..= issueId]
          $ liftIO
          $ withResource (if appCtx.env.enableTimefusionReads then appCtx.timefusionPgPool else appCtx.pool) \conn -> do
            previous <- PG.query_ conn "SHOW statement_timeout" :: IO [PG.Only Text]
            case previous of
              [PG.Only previousTimeout] ->
                -- Restore even when the query fails; SET LOCAL is ignored by TimeFusion.
                Exception.bracket_
                  (void $ PG.execute conn "SET statement_timeout = ?" (PG.Only ("4s" :: Text)))
                  (void $ PG.execute conn "SET statement_timeout = ?" (PG.Only previousTimeout))
                  ( listToMaybe @(UTCTime, PGArray Text, Maybe Text)
                      <$> PG.query
                        conn
                        "SELECT timestamp, summary, context___trace_id FROM otel_logs_and_spans WHERE project_id = ? AND timestamp BETWEEN ? AND ? AND ? = ANY(hashes) ORDER BY timestamp DESC LIMIT 1"
                        (pid.toText, from, to, patHash)
                  )
              _ -> Exception.throwIO $ userError "Unable to read telemetry statement timeout"
      let sample = maybe SampleUnavailable (maybe SampleEmpty \(at, PGArray summary, sampleTraceId) -> SampleFound at (V.fromList summary) sampleTraceId) result
      addRespHeaders $ IssueSample pid snapshot sample
    _ -> addRespHeaders NoIssueSample


data IssueSample = IssueSample Projects.ProjectId (Maybe Text) IssueSampleResult | NoIssueSample


instance ToHtml IssueSample where
  toHtml (IssueSample pid snapshot sample) = toHtml $ issueSampleCard_ pid snapshot sample
  toHtml NoIssueSample = p_ [class_ "px-4 text-sm text-textWeak"] "This issue has no event sample."
  toHtmlRaw = toHtml


issueSampleCard_ :: Projects.ProjectId -> Maybe Text -> IssueSampleResult -> Html ()
issueSampleCard_ pid snapshot result = case result of
  SampleFound at summary sampleTraceId -> do
    div_ [class_ "px-4 pt-3 flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-textWeak"] do
      span_ "Latest event in range"
      time_ [datetime_ $ formatUTC at] $ toHtml $ fmtDate "%F %T UTC" at
      whenJust (mfilter (not . T.null) sampleTraceId) \tid ->
        a_ [href_ $ (urls.traces pid).tracesGet tid (Just at) Nothing Nothing Nothing Nothing, class_ "text-textBrand underline underline-offset-2"] "View trace"
    div_ [class_ "flex flex-wrap items-center gap-1 p-4 max-h-80 overflow-y-auto"] $ V.mapM_ (summaryToken_ True) summary
  SampleLoading -> withSnapshot "Looking for an event in the selected range…"
  SampleEmpty -> withSnapshot "No occurrences in this range. Try another range; older raw events may no longer be retained."
  SampleUnavailable -> withSnapshot do
    "The event sample could not be loaded. "
    button_ [type_ "button", class_ "text-textBrand underline underline-offset-2", [__|on click send retryIssueSample to #issue-sample|]] "Retry"
  where
    withSnapshot message = do
      div_ [class_ "px-4 py-3 text-sm text-textWeak", role_ "status"] message
      whenJust (nonBlank snapshot) \stored -> do
        div_ [class_ "px-4 pt-2 border-t border-strokeWeak text-xs text-textWeak"] "Stored sample · preserved with this issue independently of the date filter and raw-event retention"
        renderLogContent_ stored


-- | Run an optional lookup, bounded by @limitM@ microseconds where a slow read is
-- itself a failure mode. @Nothing@ means "could not be loaded" — which a caller
-- must be able to tell apart from "loaded, found nothing", so the timeout and the
-- exception are reported here rather than being flattened into an empty panel.
tryWithin :: Maybe Int -> Text -> [Pair] -> ATAuthCtx a -> ATAuthCtx (Maybe a)
tryWithin limitM what ctx run =
  trySync (maybe (Just <$> run) (`timeout` run) limitM) >>= \case
    Right res -> res <$ when (isNothing res) (Log.logAttention (what <> "_TIMEOUT") (AE.object ctx))
    Left e -> Nothing <$ Log.logAttention (what <> "_FAILED") (AE.object $ ctx <> ["error" AE..= show @Text e])


-- | Unescape JSON-ish whitespace/quotes embedded in summary tokens.
unescSummary :: Text -> Text
unescSummary = T.replace "\\\"" "\"" . T.replace "\\n" " " . T.replace "\\t" " "


-- | Render one @field;style⇒value@ summary token. In chip mode (@wrap@) whitespace
-- is preserved and the @right-@ style prefix (subdued right-rail metadata) is honoured.
summaryToken_ :: Monad m => Bool -> Text -> HtmlT m ()
summaryToken_ wrap token = case T.breakOn "⇒" token of
  (_, "") -> span_ [class_ $ bool "mr-1" "text-textWeak text-xs whitespace-pre-wrap break-words" wrap] $ toHtml $ unescSummary token
  (left, rest) ->
    let (field, style) = case T.breakOn ";" left of
          (f, s) | not (T.null s) -> (f, T.drop 1 s)
          _ -> ("", left)
     in span_
          ([class_ $ tokenClass (bool style (fromMaybe style $ T.stripPrefix "right-" style) wrap) <> bool " mr-1 inline-block" " inline-block max-w-full" wrap] <> [term "data-tippy-content" field | not (T.null field)])
          (toHtml $ unescSummary $ T.drop 1 rest)
  where
    -- In chip mode the classes gain whitespace-pre-wrap/break-* suffixes.
    badgeWrap = bool "" " whitespace-pre-wrap break-all" wrap
    textWrap = bool "" " whitespace-pre-wrap break-words" wrap
    tokenClass :: Text -> Text
    tokenClass = \case
      s | "badge-" `T.isPrefixOf` s -> "cbadge-sm " <> s <> badgeWrap
      s | s `elem` ["text-textWeak", "text-weak"] -> "text-textWeak text-xs" <> textWrap
      "text-textStrong" -> "text-textStrong text-xs font-medium" <> textWrap
      _ -> "cbadge-sm badge-neutral" <> badgeWrap


-- | A single user-journey event attached to a span as a JSON-encoded array under the
-- @breadcrumbs@ attribute. @kind@/@payload@ stand in for the JSON keys @type@/@data@
-- (renamed because @type_@ would clash with @Lucid.type_@ since field selectors are enabled).
data Breadcrumb = Breadcrumb
  { kind :: Text
  , message :: Maybe Text
  , payload :: Maybe AE.Value
  , timestamp :: Integer
  }
  deriving stock (Generic, Show)
  deriving
    (AE.FromJSON)
    via DAE.CustomJSON
          '[ DAE.OmitNothingFields
           , DAE.FieldLabelModifier '[DAE.Rename "kind" "type", DAE.Rename "payload" "data"]
           ]
          Breadcrumb


-- | User-journey breadcrumb section, rendered inline inside an existing card.
-- Emits nothing when the trace carries no parseable breadcrumbs.
userJourneySection_ :: V.Vector Telemetry.SpanRecord -> Html ()
userJourneySection_ spans = whenJust (extractBreadcrumbs spans) \crumbs -> do
  let crumbList = toList crumbs
      total = length crumbList
      base = (head crumbs).timestamp
      lastIdx = total - 1
      renderCrumb idx bc = do
        let (icn, iconColor) = breadcrumbVisual bc.kind
            message = nonBlank bc.message
            payload = nonBlank (bc.payload >>= breadcrumbDataSummary)
            rowHeader expandable = span_ [class_ "flex items-start gap-2 min-w-0 min-h-6"] do
              span_ [class_ "w-12 shrink-0 whitespace-nowrap font-mono text-2xs leading-5 tabular-nums text-textWeak", title_ $ fmtDate "%F %T UTC" $ POSIX.posixSecondsToUTCTime $ realToFrac (fromIntegral bc.timestamp / 1000 :: Double)] $ toHtml $ formatOffset base bc.timestamp
              span_ [class_ "min-w-0 flex-1 text-xs leading-5 text-textStrong break-normal [overflow-wrap:anywhere]"] (slashBreaks_ bc.kind)
              when expandable $ span_ [class_ "shrink-0 mt-1 text-textWeak", Aria.hidden_ "true"] $ faSprite_ "chevron-right" "regular" "w-3 h-3 group-open/crumb:rotate-90"
        div_ [class_ "crumb flex gap-2 px-4 py-0.5 hover:bg-fillWeaker"] do
          div_ [class_ "flex flex-col items-center pt-1 shrink-0"] do
            faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
            unless (idx == lastIdx) $ div_ [class_ "w-px flex-1 bg-strokeWeak mt-1"] ""
          if isNothing message && isNothing payload
            then div_ [class_ "min-w-0 flex-1"] $ rowHeader False
            else details_ [class_ "group/crumb min-w-0 flex-1"] do
              summary_ [class_ "cursor-pointer list-none [&::-webkit-details-marker]:hidden rounded focus-visible:outline-2 focus-visible:outline-offset-2"] $ rowHeader True
              div_ [class_ "pl-14 pt-1 space-y-1"] do
                whenJust message $ p_ [class_ "text-xs leading-relaxed text-textWeak whitespace-pre-wrap break-words"] . toHtml
                whenJust payload $ p_ [class_ "font-mono text-2xs leading-relaxed text-textWeak whitespace-pre-wrap break-normal [overflow-wrap:anywhere]"] . toHtml
      renderCrumbs rows = div_ [class_ "py-0.5 flex flex-col group-has-[.crumb-rev:checked]/journey:flex-col-reverse"] $ traverse_ (uncurry renderCrumb) rows
      indexedCrumbs = zip [0 :: Int ..] crumbList
  div_ [id_ "issue-journey", class_ "border-t border-strokeWeak group/journey"] do
    div_ [class_ "px-4 py-2 flex flex-wrap items-center gap-2 bg-fillWeaker/40"] do
      faSprite_ "route" "regular" "w-3 h-3 text-textWeak"
      fieldLabel_ "" "User journey"
      span_ [class_ "text-2xs text-textWeak"] $ toHtml $ countNoun total "event" <> " before error"
      div_ ([class_ "w-full min-w-0 flex items-center gap-1.5"] <> [[__|on input from #issue-journey-search set #issue-journey-events.open to true|] | total > 5]) do
        input_ [id_ "issue-journey-search", type_ "search", placeholder_ "Search", Aria.label_ "Search the user journey", class_ "input input-xs w-full min-w-0 flex-1 text-base sm:text-xs", filterInputAttr_ "#issue-journey .crumb"]
        label_ [class_ "btn btn-xs btn-ghost shrink-0 gap-1 has-[:checked]:text-textBrand has-[:focus-visible]:outline-2 has-[:focus-visible]:outline-offset-2", term "data-tippy-content" "Newest first"] do
          input_ [type_ "checkbox", class_ "crumb-rev sr-only", Aria.label_ "Newest events first"]
          faSprite_ "arrows-up-down" "regular" "w-3 h-3"
        copyButton_ "btn btn-xs btn-ghost" "w-3 h-3" "#issue-journey-text's textContent" []
        pre_ [id_ "issue-journey-text", class_ "hidden"] $ toHtml $ unlines [bc.kind <> foldMap (" " <>) bc.message | bc <- crumbList]
    if total <= 5
      then renderCrumbs indexedCrumbs
      else do
        div_ [class_ "group-has-[#issue-journey-events[open]]/journey:hidden"] do
          p_ [class_ "px-4 pt-2 pb-1 text-2xs text-textWeak"] "Last 5 events"
          renderCrumbs $ drop (total - 5) indexedCrumbs
        details_ [id_ "issue-journey-events", class_ "group/events"] do
          summary_ [class_ "px-4 py-1 text-xs text-textBrand cursor-pointer hover:bg-fillWeaker list-none [&::-webkit-details-marker]:hidden focus-visible:outline-2 focus-visible:outline-offset-[-2px]"] do
            span_ [class_ "group-open/events:hidden"] $ toHtml $ "View all " <> countNoun total "event"
            span_ [class_ "hidden group-open/events:inline"] "Show fewer"
          renderCrumbs indexedCrumbs
  where
    -- Every breadcrumb the trace can yield, deduped across overlapping
    -- instrumentation (an SDK may ship both the legacy attribute and OTel events) and
    -- sorted chronologically. Three sources, in order of how explicit they are:
    --
    --  1. the legacy stringified JSON array under @attributes.breadcrumbs@;
    --  2. OTel span events — @sentry.breadcrumb.*@ keys when the SDK sets them, else
    --     the event name as the kind and @message@\/@body@ as the message;
    --  3. every trace-scoped log record that is not the error span itself, which
    --     gives backend traces a journey with no custom instrumentation at all.
    extractBreadcrumbs :: V.Vector Telemetry.SpanRecord -> Maybe (NonEmpty Breadcrumb)
    extractBreadcrumbs spans' =
      let recs = V.toList spans'
          -- Breadcrumb timestamps are epoch-milliseconds.
          utcToEpochMs :: UTCTime -> Integer
          utcToEpochMs = floor . (* 1000) . POSIX.utcTimeToPOSIXSeconds
          errorSpanId = maybe "" (.spanId) $ viaNonEmpty last $ sortOn (.startTime) recs
          fromAttr sr = fromMaybe [] $ AE.decodeStrict . encodeUtf8 =<< Telemetry.atMapText "breadcrumbs" sr.attributes
          -- An event with neither text nor a selector/url (e.g. gRPC `message`
          -- events) would render as a bare kind label.
          fromEvents sr = filter (\bc -> isJust bc.message || isJust (bc.payload >>= breadcrumbDataSummary)) $ foldMap (map fromEvent) (parseMaybe AE.parseJSON sr.events :: Maybe [Telemetry.SpanEvent])
          fromEvent ev =
            Breadcrumb
              { kind = fromMaybe ev.eventName $ asum $ lookupValueText ev.eventAttributes <$> ["sentry.breadcrumb.category", "sentry.breadcrumb.type"]
              , message = asum $ lookupValueText ev.eventAttributes <$> ["sentry.breadcrumb.message", "message", "body", "exception.message"]
              , payload = Just ev.eventAttributes
              , timestamp = utcToEpochMs ev.eventTime
              }
          fromLog sr =
            [ Breadcrumb
                { kind = sr.spanName
                , message = sr.statusMessage <|> Telemetry.atMapText "body" sr.attributes <|> Telemetry.atMapText "message" sr.attributes
                , payload = AE.toJSON <$> sr.attributes
                , timestamp = utcToEpochMs sr.startTime
                }
            | sr.spanId /= errorSpanId
            ]
          dedupKey bc = (bc.timestamp, bc.kind, T.take 80 $ fromMaybe "" bc.message)
       in nonEmpty $ sortOn dedupKey $ ordNubOn dedupKey $ foldMap (`concatMap` recs) [fromAttr, fromEvents, fromLog]
    -- Icon id + tailwind colour class for a breadcrumb @type@.
    breadcrumbVisual :: Text -> (Text, Text)
    breadcrumbVisual = \case
      "click" -> ("arrow-pointer", "text-fillBrand-strong")
      "console.error" -> ("terminal", "text-fillError-strong")
      "console.warn" -> ("terminal", "text-fillWarning-strong")
      t
        | t `elem` ["navigation", "nav"] -> ("globe", "text-fillSuccess-strong")
        | t `elem` ["xhr", "fetch"] -> ("wifi", "text-fillInformation-strong")
        | otherwise -> ("terminal", "text-textWeak")
    -- Compact selector / url summary from a breadcrumb's @data@ blob.
    breadcrumbDataSummary :: AE.Value -> Maybe Text
    breadcrumbDataSummary (AE.String s) = Just s
    breadcrumbDataSummary v = asum $ lookupValueText v <$> ["selector", "url"]


-- | The user-journey half needs the issue's trace, which is the slow read on this
-- page — so the whole panel arrives through the one fragment it already used for
-- issue events, with the trace reference passed along rather than pre-fetched.
activityPanel_ :: Projects.ProjectId -> Issues.IssueId -> Maybe (Text, UTCTime) -> Html ()
activityPanel_ pid issueId traceRef = do
  let activityUrl = (urls.issues pid).activityGet issueId (fst <$> traceRef) (snd <$> traceRef)
  -- Recorded once the page is shown, not when served: the body preloads links on hover.
  div_ [hxPost_ $ (urls.issues pid).viewedPost issueId, hxTrigger_ "load", hxSwap_ "none"] ""
  railSection_ "Activity"
    $ div_ [id_ "issue-activity", class_ "-mx-4", hxGet_ activityUrl, hxTrigger_ "intersect once, issueActivityChanged from:body", hxSwap_ "innerHTML"]
    $ div_ [class_ "p-4 flex justify-center"]
    $ loadingIndicator_ LdSM LdDots


-- | A titled block of the right rail. Rail blocks are separated by rules rather than
-- boxed, so the rail reads as one column beside the evidence.
railSection_ :: Text -> Html () -> Html ()
railSection_ title body = section_ [class_ "py-4 border-b border-strokeWeak last:border-b-0"] do
  div_ [class_ "mb-2"] $ sectionLabel_ title
  body


-- | The facts every issue has, in one place and one order, whatever its type: the
-- rail's first block, as Sentry and Datadog both place it.
-- A runtime exception's real first/last seen live on its error pattern, not on the issue row.
--
-- Absent cells are omitted, not rendered as "Unknown service": an issue with no
-- environment set is not an issue in an environment called Unknown.
issueFactRow_ :: UTCTime -> Issues.Issue -> Maybe ErrorPatterns.ErrorPattern -> Maybe LogPatterns.LogPattern -> Html ()
issueFactRow_ now issue errM patternM =
  railSection_ "Issue details"
    $ kvList_ "text-sm"
    $ [ ( lbl
        , time_
            [ datetime_ $ isoT at
            , tabindex_ "0"
            , class_ "font-medium rounded-sm focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-strokeBrand-strong"
            , Aria.label_ $ lbl <> ": " <> agoText now at <> " (" <> formatUTC at <> ")"
            , term "data-tippy-content" $ formatUTC at
            ]
            $ toHtml
            $ agoText now at
        )
      | (lbl, at) <- [("Last seen", lastSeen), ("First seen", firstSeen)]
      ]
    <> [(lbl, span_ [class_ "font-medium", term "data-tippy-content" value] $ toHtml value) | (lbl, value) <- facts]
  where
    (firstSeen, lastSeen) = bimap zonedTimeToUTC zonedTimeToUTC $ fromMaybe (issue.createdAt, issue.updatedAt) $ ((\e -> (e.createdAt, e.updatedAt)) <$> errM) <|> ((\p -> (p.firstSeenAt, p.lastSeenAt)) <$> patternM)
    facts =
      catMaybes
        [ ("First release",) <$> (errM >>= (.firstRelease))
        , ("Last release",) <$> (errM >>= (.lastRelease))
        , issueSource issue
        , ("Environment",) <$> nonBlank issue.environment
        ]


data IssueDetail = IssueNotFound Projects.ProjectId | IssueFound IssueView


instance ToHtml IssueDetail where
  toHtml (IssueNotFound pid) =
    toHtml
      $ emptyState_
        def{icon = Just "circle-xmark", action = ESLink (issuesListUrl pid) "Back to Issues"}
        "Issue not found"
        "This issue may have been resolved, merged, or the link may be outdated."
  toHtml (IssueFound v) = toHtml $ issueDetailPage v
  toHtmlRaw = toHtml


-- | Everything the issue detail page's sections read. Bundled because the
-- values travelled together through every section as positional arguments.
--
-- @traceRef@ is the (trace id, when-it-happened) the Trace section loads its
-- waterfall from — the section fetches it itself, so a slow trace can't hold up the
-- page. @replaySession@ is the only value the page still needs out of that trace.
-- @errM@ carries a runtime exception's real first\/last seen, which live on the
-- error pattern rather than the issue row.
data IssueView = IssueView
  { pid :: Projects.ProjectId
  , issue :: Issues.Issue
  , requestId :: UUIDId "ai_routine_run"
  , apiEvidence :: Maybe ([ApiChanges.AnomalyVM], Maybe SchemaLearning.CatalogEntry)
  , traceRef :: Maybe (Text, UTCTime)
  , selectedAt :: UTCTime
  , replaySession :: Maybe (Text, Replay.SessionMeta)
  , errM :: Maybe ErrorPatterns.ErrorPatternL
  , patternM :: Maybe LogPatterns.LogPattern
  , now :: UTCTime
  , isFirst :: Bool
  , tp :: TimePicker.TimePicker
  , stateEvent :: Maybe Issues.IssueEvent
  , canResolve :: Bool
  , members :: [ProjectMembers.ProjectMemberVM]
  , tagCounts :: [(Text, Text, Int)]
  -- ^ The error's tag distribution rollup (key, value, count), most common first per key.
  , errHourlyFrom :: Maybe UTCTime
  -- ^ The first hour 'ErrorPatterns.hourlyStatsFrom' holds for the error, if any.
  , rollupSqlSig :: Maybe Text
  -- ^ 'Widget.sqlSignature' of 'issueRollupSql', so a viewer's chart may run it.
  }


-- | One collapsible block of the event card. @anchor@ is both the element id and the
-- "Jump to" target, so the navigator cannot list a section the page does not render.
data IssueSection = IssueSection
  { controls :: Maybe (Html ())
  , extra :: [Attribute]
  , anchor :: Text
  , glyph :: Text
  , heading :: Text
  , content :: Html ()
  }


schemaSubjectFor :: IssueView -> Maybe ApiChanges.SchemaSubject
schemaSubjectFor v = (apiChangeData v.issue >>= (.subject)) <|> fromCatalog <|> fromEndpoint
  where
    fromCatalog = do
      (_, Just entry) <- v.apiEvidence
      pure $ ApiChanges.schemaSubject entry.template.keyKind entry.scope
    fromEndpoint = do
      d <- apiChangeData v.issue
      method <- Issues.knownMethod d
      pure $ ApiChanges.EndpointSchema SchemaLearning.emptyScope{SchemaLearning.method = Just method, SchemaLearning.urlPath = Just d.endpointPath, SchemaLearning.host = Issues.knownHost d, SchemaLearning.service = v.issue.service}


-- | KQL for the issue's events: its span hash, or a span schema's service, name and kind.
-- A schema observation with no recorded source has neither.
issueEventsQuery :: IssueView -> Maybe Text
issueEventsQuery v = case schemaSubjectFor v of
  Just (ApiChanges.SpanSchema scope) -> do
    service <- nonBlank scope.service
    name <- nonBlank scope.spanName
    pure $ "resource.service.name==" <> kqlQuoted service <> " AND name==" <> kqlQuoted name <> foldMap ((" AND kind==" <>) . kqlQuoted) scope.kind
  Just (ApiChanges.EndpointSchema _) -> hashQuery
  Nothing -> hashQuery <* guard (v.issue.issueType /= Issues.ApiChange)
  where
    hashQuery = ("hashes[*]==" <>) . kqlQuoted <$> issueHashKey v.issue


-- | Sentry's issue layout in our visual language: a full-width title block with the
-- workflow bar under it, then the evidence column beside a rail. In the evidence
-- column the aggregate band (range, chart, context) comes first, then one event card
-- whose navigator stays pinned while its sections scroll under it.
issueDetailPage :: IssueView -> Html ()
issueDetailPage v@IssueView{..} = div_ [class_ "flex h-full overflow-hidden relative group/ai"] do
  div_ [class_ "flex-1 min-w-0 min-h-0 overflow-y-auto"] do
    issueHeader_ v
    div_ [class_ "grid grid-cols-1 xl:grid-cols-[minmax(0,1fr)_20rem]"] do
      div_ [class_ "min-w-0 max-md:p-3 p-4 max-md:space-y-3 space-y-4"] do
        -- Seed the URL with whichever form of range the page defaulted to, so the
        -- standalone chart widgets read the window the picker shows. A query alert
        -- defaults to an absolute from/to, so seeding `since` would override it.
        let seedParams = TimePicker.rangeJson tp
        script_ [fmt|(() => {{const seed=()=>{{const p=new URLSearchParams(location.search);if(!p.get('since')&&!p.get('from')&&!p.get('to'))window.setParams({seedParams})}};if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',seed,{{once:true}});else seed()}})();|]
        issueAggregate_ v
        eventCard_ v
      aside_ [class_ "min-w-0 px-4 xl:border-l max-xl:border-t border-strokeWeak"] do
        issueFactRow_ now issue ((.base) <$> errM) patternM
        railSection_ "AI analysis" do
          -- The background analysis job's verdict, when it has run for this error.
          case errM >>= \errL -> (,errL.base.errorCategory) <$> errL.base.rootCause of
            Just (rootCause, categoryM) -> div_ [class_ "mb-2 rounded-lg bg-fillWeaker/60 p-3 space-y-2"] do
              div_ [class_ "flex items-center gap-2"] do
                span_ [class_ "text-xs font-semibold text-textStrong"] "Likely cause"
                whenJust categoryM $ \category -> span_ [class_ "rounded bg-fillWeak px-1.5 py-0.5 text-2xs text-textWeak"] $ toHtml $ if T.toLower category == "data" then "Data handling" else T.toTitle category
              details_ [class_ "group/analysis"] do
                summary_ [class_ "cursor-pointer list-none [&::-webkit-details-marker]:hidden rounded focus-visible:outline-2 focus-visible:outline-offset-2"] do
                  span_ [class_ "text-sm leading-relaxed text-textStrong break-words line-clamp-3 group-open/analysis:hidden"] $ toHtml rootCause
                  span_ [class_ "block mt-2 text-xs text-textBrand"] do
                    span_ [class_ "group-open/analysis:hidden"] "Read full analysis"
                    span_ [class_ "hidden group-open/analysis:inline"] "Show less"
                p_ [class_ "mt-2 text-sm leading-relaxed text-textStrong break-words"] $ toHtml rootCause
            Nothing -> p_ [class_ "text-sm text-textWeak mb-2"] "Ask about the cause, the blast radius, or a fix — answered from this issue's telemetry."
          div_ [class_ "flex gap-2"] do
            button_
              (class_ "btn btn-sm btn-ghost bg-fillBrand-weak text-textBrand hover:bg-fillBrand-weak hover:brightness-110 flex-1 min-w-0 gap-1.5" : aiPanelOpenerAttrs)
              do
                faSprite_ "sparkles" "regular" "w-3.5 h-3.5"
                "Investigate"
            button_
              ( aiPanelToggleAttrs
                  <> [ class_ "btn btn-sm btn-ghost bg-fillWeaker flex-1 min-w-0 gap-1.5"
                     , [__|on click set #ai-panel-toggle's opener to me then set #ai-panel-toggle's checked to true then send change to #ai-panel-toggle
                    then set #ai-chat-input's value to 'Propose a step-by-step fix plan for this issue, with the code changes, citing the files and lines from the stack trace.'
                    then call (#ai-chat-input).form.requestSubmit()|]
                     ]
              )
              do
                faSprite_ "code" "regular" "w-3.5 h-3.5"
                "Plan a fix"
        whenJust errM \errL ->
          div_ [hxGet_ $ (urls.issues pid).errorGroupMembersGet errL.base.id.unErrorPatternId, hxTrigger_ "load, issuePatternGroupChanged from:body", hxSwap_ "innerHTML"] pass
        activityPanel_ pid issue.id traceRef
  aiSidePanel_ pid issue.id requestId


-- | Title, culprit, the state line, the Events figure, then the workflow bar — the
-- zone that identifies the issue and acts on it, full width above both columns.
issueHeader_ :: IssueView -> Html ()
issueHeader_ v@IssueView{..} = header_ [class_ "max-md:px-3 px-4 max-md:pt-4 pt-6 pb-3 border-b border-strokeWeak space-y-3"] do
  issueStatusStrip_
  -- h2, not h3: the shell's breadcrumb owns h1, and an h3 here ranked below the
  -- empty-state h2s further down the page.
  let detailTitle = case (Issues.issuePayload issue, schemaSubjectFor v) of
        (Just (Issues.QueryAlertP alertData), _) | not (T.null $ T.strip alertData.queryName) -> alertData.queryName
        (_, Just (ApiChanges.SpanSchema scope)) -> ApiChanges.spanSchemaTitle scope
        _ -> issue.title
  div_ [class_ "flex gap-6 items-start pr-10"] do
    div_ [class_ "min-w-0 flex-1 space-y-1.5"] do
      h2_ [class_ "max-md:text-xl text-2xl font-semibold text-textStrong break-words line-clamp-3", title_ detailTitle] $ if "⇒" `T.isInfixOf` detailTitle then renderSummaryText_ detailTitle else toHtml detailTitle
      unless (Issues.isBoilerplateAction issue.recommendedAction)
        $ p_ [class_ "text-sm text-textWeak max-w-3xl border-l-2 border-strokeError-strong pl-2"]
        $ toHtml issue.recommendedAction
      div_ [class_ "flex flex-wrap gap-x-3 gap-y-1.5 items-center"] do
        issueTypeChip_ False issue.issueType
        issueStateBadge_ stateEvent
        -- Only what is peculiar to the type belongs here; service, environment and
        -- first/last seen are common to every issue and live in the rail.
        case Issues.issuePayload issue of
          Just (Issues.LogPatternP d) -> do
            logLevelChip_ d.logLevel d.logPattern
            -- "all time" is explicit: the Events figure beside the title is scoped to
            -- the selected range, and an unqualified count reads as a contradiction.
            metadataChip_ "tally" $ show (maybe d.occurrenceCount (.occurrenceCount) patternM) <> " all time"
          Just (Issues.LogPatternRateChangeP d) -> do
            logLevelChip_ d.logLevel d.logPattern
            metadataChip_ "arrow-trend-up" $ display d.changeDirection
            metadataChip_ "percent" $ Issues.showPct d.changePercent <> " change"
            metadataChip_ "gauge-high" $ Issues.showRate d.currentRatePerHour <> " current"
            metadataChip_ "chart-line" $ Issues.showRate d.baselineMean <> " baseline"
          Just (Issues.RuntimeExceptionP d) -> do
            whenJust (errM >>= (.base.errorData.runtime)) $ metadataChip_ "code"
            whenJust ((\m p -> m <> " " <> p) <$> d.requestMethod <*> d.requestPath) $ span_ [class_ "text-xs font-mono text-textWeak break-all"] . toHtml
          Just (Issues.QueryAlertP _) -> pass
          Just (Issues.ApiChangeP _) -> pass
          Just (Issues.CronP d) -> metadataChip_ "clock" $ bool "Failed run" "Missed check-in" (d.failure == Issues.CFMissed)
          Just (Issues.FeedbackP d) -> metadataChip_ "comment" $ fromMaybe "Anonymous" (d.userName <|> d.contactEmail)
          Just (Issues.UptimeP d) -> metadataChip_ "clock" $ "Down since " <> agoText now d.downSince
          Just (Issues.FrontendP d) -> metadataChip_ "arrow-pointer" $ bool "Dead click" "Rage click" (d.kind == Issues.FKRageClick)
          Just (Issues.PerformanceP d) -> metadataChip_ "database" $ unwords $ catMaybes [Just (bool "Slow query" "N+1 query" (d.kind == Issues.PKNPlusOne)), d.dbSystem]
          Nothing -> unparsablePayload_
    -- The chart's own total, hoisted: `naked` suppresses the widget's value slot, so
    -- this is the only place it renders and it cannot disagree with the chart.
    div_ [class_ "shrink-0 flex gap-6"] do
      when (isJust $ issueEventsQuery v)
        $ statFigure_ "items-end" [term "data-tippy-content" "Events in the selected range"] "Events · this range" (Widget.widgetValueSlotAs_ (issueChartId issue) Nothing)
      -- Distinct users by user.id / user.email / client.address, all time.
      whenJust errM \errL -> statFigure_ "items-end" [term "data-tippy-content" "Distinct users affected, all time"] "Users · all time" $ toHtml $ formatWithCommas (fromIntegral errL.base.usersCount :: Double)
  div_ [class_ "flex flex-wrap items-center gap-2"] do
    issueAcknowledgeButton pid issue.id now ackUntil
    issueArchiveButton pid issue.id (isJust issue.archivedAt)
    case errM of
      Just errL -> do
        errorResolveAction pid errL.base errL.base.state canResolve
        errorSubscriptionAction pid errL.base
      Nothing ->
        unless (isJust issue.archivedAt)
          $ button_ [type_ "button", class_ "btn btn-sm btn-ghost gap-1.5 text-textSuccess hover:bg-fillSuccess-weak", hxPost_ $ (urls.issues pid).resolveGet issue.id, hxTarget_ $ "#archive-ctl-" <> issue.id.toText, hxSwap_ "outerHTML", Aria.label_ "Resolve issue", [__|on htmx:after:request[detail.ctx.response.status < 400] remove me|]] do
            faSprite_ "circle-check" "regular" "w-4 h-4"
            span_ [class_ "max-md:hidden"] "Resolve"
    issueTriage_ pid issue members
  where
    ackUntil = zonedTimeToUTC <$> issue.acknowledgedUntil <* issue.acknowledgedAt
    -- Banner stating, in words, what the issue's current state means for
    -- notifications. The whole point of the ack window is that a reader never has
    -- to guess whether alerts are still coming.
    issueStatusStrip_ = forM_ banners \(icon, cls, msg) ->
      div_ [class_ $ "flex items-center gap-2 rounded-lg border px-3 py-2 text-sm " <> cls] do
        faSprite_ icon "regular" "w-4 h-4 shrink-0"
        span_ [] $ toHtml msg
      where
        -- Archived and acknowledged are independent, and an issue can be both. This used
        -- to short-circuit on archived, so a both-states issue showed only the archive
        -- banner while the action bar still offered "Unacknowledge" for a state the page
        -- never mentioned. Each true clause now renders.
        banners =
          catMaybes
            [ ("archive", "border-strokeWeak bg-fillWeaker text-textWeak", "Archived — hidden from the Inbox and never notified. Unarchive to bring it back." :: Text) <$ issue.archivedAt
            , ackUntil <&> \until' -> ("bell-slash", "border-strokeSuccess-weak bg-fillSuccess-weak text-textSuccess", untilLabel "Acknowledged" now until' <> " \x2014 notifications are paused. This issue returns to the Inbox when the window ends or it regresses.")
            ]


issueChartId :: Issues.Issue -> Text
issueChartId issue = issue.id.toText <> if issue.issueType == Issues.ApiChange then "-schema-activity" else "-pattern-volume"


-- | A chart series over one key of an hourly stats table, bounded by the chart's window.
hourlyRollupSql :: Text -> Text -> Text
hourlyRollupSql tbl key = "SELECT * FROM (SELECT hour_bucket AS timestamp, 'value', event_count::float FROM " <> tbl <> " WHERE project_id = '{{project_id}}' AND " <> key <> ") s WHERE TRUE{{rollup_time_filter}} ORDER BY 1"


-- | The aggregate band: the range every panel below reads, then the chart beside the
-- type's context panel.
issueAggregate_ :: IssueView -> Html ()
issueAggregate_ v@IssueView{..} = do
  let (rangeFrom, rangeTo, currentRange) = TimePicker.parseTimeRange now tp
      eventWindow = windowAround 7200 selectedAt
      refreshId = "anomaly-chart-refresh"
  div_ [id_ refreshId, class_ "hidden", [__|on submit trigger 'update-query' on window|]] ""
  div_ [class_ "flex flex-wrap items-center gap-2 [&>button]:max-md:basis-full"]
    $ TimePicker.liveDataControls_ (Just refreshId) currentRange (Just $ "issue-" <> issueChartId issue) TimePicker.RefreshOnly
  when (tp.from == eventWindow.from && tp.to == eventWindow.to && isNothing tp.since)
    $ p_ [class_ "text-xs text-textWeak"] "Showing \xB12 hours around the selected event"
  when (any (> selectedAt) rangeFrom || any (< selectedAt) rangeTo)
    $ p_ [class_ "text-sm text-textWeak"] do
      "Selected event is outside this range. "
      a_ [href_ $ issueDetailUrl pid issue.id, class_ "text-textBrand hover:underline"] "Jump to latest occurrence"
  case Issues.issuePayload issue of
    Nothing -> unparsablePayload_
    Just (Issues.LogPatternP _) -> issueVolumeChart_ "Pattern Volume"
    Just (Issues.LogPatternRateChangeP _) -> issueVolumeChart_ "Pattern Volume"
    Just (Issues.PerformanceP d) ->
      contextCard_ "p-4 flex items-start gap-8" "Impact in the sample trace" $ figures_ [("Duration", show @Text (round d.durationImpactMs :: Int) <> " ms"), ("Queries", show d.repeatCount), ("Database", fromMaybe "\x2014" d.dbSystem)]
    Just (Issues.FeedbackP d) ->
      contextCard_ "p-4 flex flex-col gap-3" "What the user said" do
        p_ [class_ "text-sm text-textStrong whitespace-pre-wrap"] $ toHtml d.message
        whenJust d.relatedErrorHash \h -> a_ [href_ $ (urls.issues pid).detailHashGet h Nothing Nothing Nothing Nothing Nothing, class_ "text-xs text-textBrand hover:underline"] "Open the error from this user's trace"
    Just (Issues.CronP d) ->
      contextCard_ "p-4 flex flex-col gap-2" "Schedule" do
        detailRow_ [("clock", "text-fillError-strong", "Expected by", agoText now d.expectedBy)]
        detailRow_ [("check", "text-textWeak", "Last check-in", maybe "never" (agoText now) d.lastCheckinAt)]
    Just (Issues.UptimeP d) ->
      contextCard_ "p-4 flex items-start gap-8" "Failing check" $ figures_ [("Status", maybe "\x2014" show d.statusCode), ("Expected", show d.expectedStatus), ("Response", show d.durationMs <> " ms")]
    Just (Issues.FrontendP d) ->
      contextCard_ "p-4 flex flex-col gap-2" "Where it happened" do
        whenJust d.pageUrl \u -> a_ [href_ u, target_ "_blank", rel_ "noopener", class_ "text-sm text-textBrand break-all hover:underline"] $ toHtml u
        detailRow_ [("arrow-pointer", "text-fillBrand-strong", "Clicks in the burst", show d.clickCount)]
    Just (Issues.RuntimeExceptionP d) ->
      sideBySide_ (issueVolumeChart_ "Error Frequency")
        $ whenJust ((,) <$> d.requestMethod <*> d.requestPath) \(method, path) ->
          contextCard_ "p-4" "Request" do
            span_ [class_ $ "relative cbadge-sm whitespace-nowrap " <> Telemetry.methodStyle method] $ toHtml method
            span_ [class_ "ml-2 text-sm text-textWeak break-all"] $ toHtml path
    Just (Issues.QueryAlertP d) -> do
      let below = d.thresholdType == Issues.Below
          meetsThreshold = Issues.meetsThreshold d.thresholdType d.thresholdValue d.actualValue
      sideBySide_ (issueChartCard_ "Alert Query" "h-56" (Just d.thresholdValue) Nothing d.queryExpression)
        $ contextCard_ "p-4 flex flex-col gap-4" "Recorded evaluation" do
          div_ [class_ "flex items-start gap-6"] do
            statFigure_ "" [] "Recorded value" $ span_ [class_ $ bool "" "text-fillWarning-strong" meetsThreshold] $ toHtml $ formatWithCommas d.actualValue
            statFigure_ "" [] "Threshold" do
              toHtml $ formatWithCommas d.thresholdValue
              span_ [class_ "block mt-1 text-xs font-normal text-textWeak"] $ bool "At or above" "At or below" below
          unless meetsThreshold
            $ p_
              [class_ "text-sm text-textWeak"]
              "This value does not meet the recorded threshold. Check the monitor’s warning and recovery settings."
          detailRow_ [("bolt", "text-fillWarning-strong", "Recorded", agoText now d.triggeredAt)]
          when (isJust $ UUID.fromText d.queryId) $ externalLink_ [href_ $ (urls.monitors pid).overviewGet d.queryId] "View monitor"
    Just (Issues.ApiChangeP d) -> do
      div_ [class_ "flex flex-wrap items-center gap-x-4 gap-y-2 text-sm text-textStrong"] do
        case schemaSubjectFor v of
          Just (ApiChanges.SpanSchema scope) -> do
            span_ [class_ "text-textWeak"] "Span schema"
            whenJust scope.service $ span_ . toHtml
            whenJust scope.spanName $ code_ [class_ "font-mono break-normal [overflow-wrap:anywhere]"] . toHtml
          Just (ApiChanges.EndpointSchema scope) -> do
            whenJust scope.method $ \method -> span_ [class_ $ "cbadge-sm " <> Telemetry.methodStyle method] $ toHtml method
            whenJust scope.urlPath $ code_ [class_ "font-mono break-normal [overflow-wrap:anywhere]"] . toHtml
            whenJust scope.host $ span_ . toHtml
          Nothing -> span_ [class_ "text-textWeak"] "Schema observation · source details unavailable"
        span_ [class_ "text-textWeak"] $ toHtml $ countNoun (length $ ordNub $ V.toList $ d.newFields <> d.deletedFields <> d.modifiedFields) "changed field"
      p_ [class_ "text-xs text-textWeak"] "Observed changes describe recorded data. They do not by themselves confirm a breaking API change."
  where
    figures_ :: [(Text, Text)] -> Html ()
    figures_ = mapM_ \(lbl, val) -> statFigure_ "" [] lbl (toHtml val)
    -- A wrapping row of @(icon, iconColour, label, value)@ entries, as used by the
    -- query-alert and endpoint context cards.
    detailRow_ :: [(Text, Text, Text, Text)] -> Html ()
    detailRow_ =
      div_ [class_ "flex flex-wrap items-center gap-x-5 gap-y-2"] . mapM_ \(icn, iconColor, lbl, value) ->
        div_ [class_ "flex items-center gap-1.5 whitespace-nowrap"] do
          faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
          span_ [class_ "text-xs text-textWeak"] $ toHtml lbl <> ":"
          span_ [class_ "text-xs font-medium"] $ toHtml value
    -- Evidence on the left at whatever width is left over, its context panel on the
    -- right, stacking under lg. Every issue type lays out this way.
    sideBySide_ :: Html () -> Html () -> Html ()
    sideBySide_ evidence aside = div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
      div_ [class_ "min-w-0 flex-1"] evidence
      aside
    -- The right-hand context panel every type puts beside its chart.
    contextCard_ :: Text -> Text -> Html () -> Html ()
    contextCard_ bodyCls title body = div_ [class_ "surface-raised rounded-2xl overflow-hidden lg:w-72 shrink-0"] do
      div_ [class_ "px-4 py-3 border-b border-strokeWeak flex items-center gap-2"] do
        faSprite_ "circle-info" "regular" "w-3.5 h-3.5 text-textWeak"
        -- h3, not span: the chart and context cards are reachable by heading navigation.
        sectionLabel_ title
      div_ [class_ bodyCls] body
    -- The page's chart. A query alert's total is what the alert measures, not an event
    -- count, so it stays in the chart header; every other type's total is the Events
    -- figure in 'issueHeader_'. @thresholdM@ draws the alert's breach line; @heightCls@
    -- is taller when the chart *is* the evidence.
    issueChartCard_ :: Text -> Text -> Maybe Double -> Maybe (Text, UTCTime) -> Text -> Html ()
    issueChartCard_ chartTitle heightCls thresholdM rollupM chartQuery = do
      let chartId = issueChartId issue
          total = statFigure_ "" [] "Value" $ Widget.widgetValueSlotAs_ chartId Nothing
      div_ [class_ "surface-raised rounded-2xl overflow-hidden"] do
        div_ [class_ "px-4 py-2 flex flex-wrap items-center gap-x-4 gap-y-1 border-b border-strokeWeak"] do
          sectionLabel_ chartTitle
          when (isNothing $ Issues.hashPrefix issue.issueType) total
        div_ [class_ heightCls]
          $ Widget.widget_
            (def :: Widget.Widget)
              { Widget.standalone = Just True
              , Widget.naked = Just True
              , Widget.id = Just chartId
              , Widget.wType = Widget.WTTimeseries
              , Widget.showTooltip = Just True
              , Widget.query = Just chartQuery
              , Widget.rollupSql = fst <$> rollupM
              , Widget.sqlSig = rollupSqlSig <* rollupM
              , Widget.rollupFrom = isoT . snd <$> rollupM
              , Widget._projectId = Just issue.projectId
              , Widget.hideLegend = Just True
              , Widget.hideSubtitle = Just True
              , Widget.alertThreshold = thresholdM
              , Widget.showThresholdLines = Widget.TLAlways <$ thresholdM
              , Widget.markers = errM <&> \(e :: ErrorPatterns.ErrorPatternL) -> let ep = e.base; mk t r = Widget.WidgetMarker r (toText $ iso8601Show $ zonedTimeToUTC t) in catMaybes [mk ep.createdAt <$> ep.firstRelease, mk <$> ep.lastReleaseSince <*> (ep.lastRelease <* guard (ep.lastRelease /= ep.firstRelease))]
              , -- A query alert's series is whatever the alert counts, so it must not
                -- borrow the error colour just because it is on an issue page.
                Widget.seriesIntent = "error" <$ guard (issue.issueType `elem` [Issues.RuntimeException, Issues.LogPattern, Issues.LogPatternRateChange])
              }
    -- Volume of the issue's own signal over the selected range.
    issueVolumeChart_ :: Text -> Html ()
    issueVolumeChart_ chartTitle = whenJust (issueHashKey issue) \key ->
      issueChartCard_ chartTitle "h-24" Nothing issueHourlySql $ "hashes[*]==\"" <> key <> "\" | summarize count(*) by bin_auto(timestamp)"
    issueHourlySql = issueRollupSql v


-- | The volume chart's series from the hourly counts ingest already keeps. The KQL
-- form scans every row in the window on TimeFusion (@hashes@ has no index there),
-- 4-27s for a day; this is an index lookup. Paired with the first hour the table can
-- answer. A signal counted since it first appeared has no gap, since it had no events
-- before then. Otherwise pattern stats start at their retention horizon, and error
-- stats where the error's counting did. See 'Web.Routes.rollupRoute'. Top-level so the
-- handler can sign exactly the statement the view renders.
issueRollupSql :: IssueView -> Maybe (Text, UTCTime)
issueRollupSql IssueView{..} = case Issues.issuePayload issue of
  Just (Issues.LogPatternP d) -> logPattern d.sourceField d.patternHash
  Just (Issues.LogPatternRateChangeP d) -> logPattern d.sourceField d.patternHash
  _ ->
    errM >>= \e ->
      errHourlyFrom <&> \counted ->
        (hourlyRollupSql "apis.error_hourly_stats" $ "error_id = " <> sqlStringLit (UUID.toText e.base.id.unErrorPatternId), coveredFrom (zonedTimeToUTC e.base.createdAt) counted)
  where
    logPattern sf h = Just (hourlyRollupSql "apis.log_pattern_hourly_stats" $ "source_field = " <> sqlStringLit sf <> " AND pattern_hash = " <> sqlStringLit h, maybe horizon (\p -> coveredFrom (zonedTimeToUTC p.firstSeenAt) horizon) patternM)
    horizon = addUTCTime (fromIntegral $ -(3600 * LogPatterns.hourlyStatsRetentionHours)) now
    -- Counting that began within the hour of the first event leaves nothing uncounted.
    coveredFrom firstEvent countedFrom = bool countedFrom (POSIX.posixSecondsToUTCTime 0) (countedFrom <= addUTCTime 3600 firstEvent)


-- | One card for everything about the sampled event. Its navigator is the only sticky
-- element in the column: it names the occurrence being shown and links to every
-- section, and stays pinned while the sections scroll under it. The card clips
-- rather than hides overflow — hidden would make it a scroll container and trap
-- the navigator along with the waterfall's own sticky ruler.
eventCard_ :: IssueView -> Html ()
-- @--event-nav-h@ is the navigator's height (h-10 + h-9 + its 1px border); the trace
-- waterfall's sticky rows and the jump-link landing offset read it.
eventCard_ view@IssueView{..} = div_ [class_ "surface-raised rounded-2xl overflow-clip [--event-nav-h:77px]"] do
  let occurrenceUrl useFirst = (urls.issues pid).detailGet issue.id (guard useFirst $> "true") (nonEmptyT tp.since) (nonEmptyT tp.from) (nonEmptyT tp.to) Nothing
      -- Only issues keyed by an event hash have a stream of occurrences to step through.
      hasOccurrences = isJust (Issues.hashPrefix issue.issueType) && not isLogPatternIssue && issue.issueType /= Issues.ApiChange
  nav_ [id_ "issue-event-nav", class_ "sticky top-0 z-20 bg-bgRaised border-b border-strokeWeak", Aria.label_ "Issue evidence"] do
    div_ [class_ "max-md:px-3 px-4 h-10 flex items-center gap-3 overflow-x-auto whitespace-nowrap"] do
      span_ [class_ "text-sm font-semibold text-textStrong"] $ bool "Evidence" "Event" hasOccurrences
      span_ [class_ "text-xs text-textWeak tabular-nums", term "data-tippy-content" $ fmtDate "%F %T UTC" selectedAt] $ localTimeFmt_ "MMM d, HH:mm:ss" selectedAt
      -- Labelled, because "First | Recent" otherwise reads as a peer of the section
      -- links: one picks which occurrence, the others where to look in it.
      div_ [class_ "ml-auto flex items-center gap-1 text-xs"] do
        when hasOccurrences do
          span_ [class_ "text-textWeak mr-1 max-md:hidden"] "Occurrence"
          whenJust traceRef \(tid, at) -> forM_ ([(Telemetry.Older, "chevron-left", "Earlier event"), (Telemetry.Newer, "chevron-right", "Later event")] :: [(Telemetry.EventStep, Text, Text)]) \(dir, icon, tip) ->
            a_ [href_ $ (urls.issues pid).stepGet issue.id (Just dir) (Just $ EventRef tid at), class_ "px-1.5 py-1 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker", Aria.label_ tip, term "data-tippy-content" tip]
              $ faSprite_ icon "regular" "w-3 h-3"
          forM_ ([(True, isFirst, "Show the first occurrence", "First"), (False, not isFirst, "Show the most recent occurrence", "Recent")] :: [(Bool, Bool, Text, Text)]) \(useFirst, active, tip, lbl) ->
            a_ [href_ $ occurrenceUrl useFirst, class_ $ "px-2 py-1 rounded " <> bool "text-textWeak hover:text-textStrong hover:bg-fillWeaker" "bg-fillBrand-weak text-textBrand font-medium" active, term "data-tippy-content" tip] $ toHtml lbl
          a_ [href_ $ (urls.issues pid).stepGet issue.id (Just Telemetry.Recommended) Nothing, class_ "px-2 py-1 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker", term "data-tippy-content" "The last day's event with the most context: a replay, then a user, then a URL"] "Recommended"
          span_ [class_ "w-px h-4 bg-strokeWeak mx-1"] ""
        -- The native popover escapes this horizontally scrolling row.
        button_ ([type_ "button", class_ "px-2 py-1 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker flex items-center gap-1"] <> popoverTrigger_ "issue-copy-pop") do
          faSprite_ "copy" "regular" "w-3 h-3"
          "Copy as"
        div_ ([class_ "dropdown dropdown-end menu bg-bgRaised p-2 text-sm border border-strokeWeak rounded-md shadow-lg space-y-1"] <> popoverPanel_ "issue-copy-pop") do
          forM_ ([("JSON", "issue-copy-json"), ("Markdown", "issue-copy-md")] :: [(Text, Text)]) \(lbl, src) -> div_ [class_ "flex items-center justify-between gap-6"] do
            span_ [class_ "text-textStrong"] $ toHtml lbl
            copyButton_ "btn btn-xs btn-ghost" "w-3 h-3" ("#" <> src <> "'s textContent") []
          pre_ [id_ "issue-copy-json", class_ "hidden"] $ toHtml $ decodeUtf8 @Text $ AE.encode $ maybe (getAeson issue.issueData) (AE.toJSON . (.base.errorData)) errM
          pre_ [id_ "issue-copy-md", class_ "hidden"] $ toHtml issueMarkdown
    div_ [class_ "max-md:px-3 px-4 h-9 flex items-center gap-1 overflow-x-auto whitespace-nowrap border-t border-strokeWeak text-xs"] do
      span_ [class_ "text-textWeak mr-1"] "Jump to:"
      forM_ sections \s -> a_ [href_ $ "#" <> s.anchor, class_ "px-2 py-1 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker"] $ toHtml s.heading
  forM_ sections \s ->
    disclosure_ ([id_ s.anchor, class_ "border-t border-strokeWeak first-of-type:border-t-0 scroll-mt-(--event-nav-h)", open_ ""] <> s.extra) "max-md:px-3 px-4 py-2.5 hover:bg-fillWeaker" (sectionHead s) $ div_ [class_ "pb-3"] s.content
  where
    logRowsUrl q r = explorerUrl pid LogQueries.Data LogQueries.ExplorerUrl{query = Just q, cols = Nothing, cursor = Nothing, since = nonEmptyT r.since, from = nonEmptyT r.from, to = nonEmptyT r.to, layout = Nothing, source = Nothing, environment = Nothing, serviceScope = Nothing, recent = False}
    sectionHead :: IssueSection -> Html ()
    sectionHead s = do
      faSprite_ s.glyph "regular" "w-3.5 h-3.5 text-textWeak"
      sectionLabel_ s.heading
      whenJust s.controls $ div_ [class_ "ml-auto flex items-center gap-2"]
    issueMarkdown =
      unlines
        $ ["## " <> issue.title, "", "- Type: " <> display issue.issueType, "- Severity: " <> display issue.severity]
        <> ["- Service: " <> sv | Just sv <- [issue.service]]
        <> ["- Environment: " <> e | Just e <- [issue.environment]]
        <> foldMap (\errL -> let e = errL.base.errorData in ["- Release: " <> r | Just r <- [e.release]] <> ["", "```", e.errorType <> ": " <> e.message, errL.base.stacktrace, "```"]) errM
    -- The type's own evidence, then trace, logs and replay when the issue has them.
    -- A section with nothing to show is left out rather than rendered as an empty state.
    sections = typeSections <> apiActivitySection <> traceSection <> logsSection <> eventsSection <> replaySection
    -- Every event of the issue in the page's range, from its own hash; picking one's
    -- trace re-opens the page on that event.
    apiActivitySection =
      [ section "issue-schema-activity" "chart-line" "Matching span activity"
          $ div_ [class_ "h-32 px-4"]
          $ Widget.widget_
            (def :: Widget.Widget)
              { Widget.standalone = Just True
              , Widget.naked = Just True
              , Widget.id = Just $ issueChartId issue
              , Widget.wType = Widget.WTTimeseries
              , Widget.query = Just $ query <> " | summarize count(*) by bin_auto(timestamp)"
              , Widget._projectId = Just pid
              , Widget.hideLegend = Just True
              , Widget.hideSubtitle = Just True
              }
      | issue.issueType == Issues.ApiChange
      , Just query <- [issueEventsQuery view]
      ]
    eventsSection =
      [ section "issue-events" "list-check" (if issue.issueType == Issues.ApiChange then "Matching events" else "All events")
          $ div_ [class_ "max-md:px-1 px-2 flex flex-col max-h-[50vh]"]
          $ virtualTable pid [term "initialFetchUrl" $ logRowsUrl query tp, term "emptyMessage" "No events in this range."]
      | not isLogPatternIssue
      , Just query <- [issueEventsQuery view]
      ]
    section = IssueSection Nothing []
    evidence anchor glyph heading = pure . section anchor glyph heading . kvRows_ "max-md:px-3 px-4" . present
    present = mapMaybe \(k, v) -> (k,) <$> nonBlank v
    kvRows_ :: Text -> [(Text, Text)] -> Html ()
    kvRows_ cls rows = dl_ [class_ $ "grid gap-y-0.5 font-mono text-xs " <> cls] $ forM_ rows \(k, val) -> div_ [class_ "flex gap-3 px-2 py-1 rounded odd:bg-fillWeaker min-w-0"] do
      dt_ [class_ "w-28 shrink-0 text-textWeak"] $ toHtml k
      dd_ [class_ "min-w-0 break-all text-textStrong"] $ toHtml val
    isLogPatternIssue = issue.issueType `elem` [Issues.LogPattern, Issues.LogPatternRateChange]
    typeSections = case Issues.issuePayload issue of
      Nothing -> []
      Just (Issues.LogPatternP d) -> patternSections d.sourceField d.logPattern d.sampleMessage
      Just (Issues.LogPatternRateChangeP d) -> patternSections d.sourceField d.logPattern d.sampleMessage
      Just (Issues.RuntimeExceptionP d) ->
        let trimmedStack = T.strip d.stackTrace
            hasStack = not $ T.null trimmedStack
            runtimeM = errM >>= (.base.errorData.runtime)
            field :: (ErrorPatterns.ATError -> Maybe a) -> Maybe a
            field f = errM >>= f . (.base.errorData)
            userContext = case replaySession of
              Just (_, meta) | rows@(_ : _) <- present [("name", meta.userName), ("email", meta.userEmail), ("id", meta.userId)] -> rows
              _ -> present [("name", field (.userName)), ("email", field (.userEmail)), ("id", field (.userId)), ("location", place), ("ip", field (.userIp)), ("tenant", field ErrorPatterns.errorTenant)]
            highlights =
              present
                [ ("handled", bool "no" "yes" <$> field (.handled))
                , ("level", field (.level))
                , ("transaction", (\m p -> m <> " " <> p) <$> d.requestMethod <*> d.requestPath)
                , ("environment", field (.environment) <|> issue.environment)
                , ("release", field (.release))
                , ("trace.id", fst <$> traceRef)
                ]
            -- Sentry's context cards, each from the OTel namespace it is named after.
            -- A card with no values is omitted rather than rendered empty.
            contexts =
              filter
                (not . null . snd)
                [ ("User", userContext)
                , ("Client", present [("browser", field (.browser)), ("os", field (.os)), ("device", field (.device)), ("user_agent", field (.userAgent))])
                , ("Runtime", present [("runtime", runtimeM), ("service", field (.serviceName) <|> issue.service), ("release", field (.release)), ("thread", field (.threadName) <|> field (.threadId)), ("mechanism", display <$> field (.mechanism))])
                , ("Trace", present [("trace.id", fst <$> traceRef), ("span.id", field (.spanId)), ("parent_span.id", field (.parentSpanId)), ("session.id", field (.sessionId))])
                ]
            place = viaNonEmpty (T.intercalate ", " . toList) (catMaybes [field (.geoCity), field (.geoRegion), field (.geoCountry)])
         in [section "issue-highlights" "list-tree" "Highlights" $ kvRows_ "max-md:px-3 px-4 md:grid-cols-2 gap-x-6" highlights | not (null highlights)]
              <> [ IssueSection Nothing [detailsClosedBelowAttr_ 768] "issue-stack" "code" (if hasStack then "Stack trace" else "Error details") do
                     -- The title truncates and the reader expanded to read this in full,
                     -- so it shows whether or not there is a stack trace.
                     unless (T.null d.errorMessage) $ div_ [class_ "max-md:px-3 px-4 pb-3 border-b border-strokeWeak"] do
                       fieldLabel_ "mb-1" "Error message"
                       pre_ [class_ "text-sm leading-relaxed text-fillError-strong whitespace-pre-wrap break-words font-mono"] $ toHtml d.errorMessage
                     if hasStack
                       then
                         -- The fingerprint grammar, not framesFor: the issue hash is computed from these frames.
                         stackFrames_
                           (codeContextUrl pid ((errM >>= (.base.errorData.serviceName)) <|> issue.service) Nothing)
                           trimmedStack
                           (EF.parseStackTrace (EF.parseRuntime $ fromMaybe "" runtimeM) trimmedStack)
                       else
                         -- OTel's exception event carries `message` and `type` but no
                         -- `exception.stacktrace` unless the SDK opts in, which most do not
                         -- (151 of 151 demo exceptions land here). Name the runtime that
                         -- stayed silent and point at the evidence this page does have.
                         div_ [class_ "px-4 py-3 text-sm text-textWeak space-y-2"] do
                           p_ do
                             "No stack trace in this event. "
                             toHtml $ maybe "The SDK" (\r -> "The " <> r <> " SDK") runtimeM <> " reported this exception without frames."
                           let (target, lbl) = bool ("#issue-logs", "Inspect the related logs") ("#issue-trace", "Inspect the trace and service calls") (isJust traceRef)
                           a_ [href_ target, class_ "text-textBrand underline underline-offset-2 hover:no-underline"] lbl
                 ]
              <> [ section "issue-http" "globe" "HTTP request" do
                     let headers = maybe [] Map.toList (field (.requestHeaders))
                         query = [(k, T.drop 1 v) | kv <- maybe [] (T.splitOn "&") (field (.urlQuery)), let (k, v) = T.breakOn "=" kv, not (T.null k)]
                     div_ [class_ "max-md:px-3 px-4 space-y-3"] do
                       div_ [class_ "flex items-center gap-2 min-w-0"] do
                         span_ [class_ $ "cbadge-sm " <> Telemetry.methodStyle method] $ toHtml method
                         span_ [class_ "font-mono text-sm text-textStrong break-all"] $ toHtml url
                       forM_ ([("Query string", query), ("Headers", headers)] :: [(Text, [(Text, Text)])]) \(lbl, rows) -> unless (null rows) do
                         fieldLabel_ "mb-1" lbl
                         kvRows_ "" rows
                       disclosure_ [] "text-xs text-textBrand" "Show as curl" do
                         div_ [class_ "mt-2 flex items-start gap-2"] do
                           pre_ [id_ "issue-curl", class_ "flex-1 min-w-0 text-xs font-mono bg-fillWeaker rounded px-2 py-1.5 whitespace-pre-wrap break-all"] $ toHtml $ curlCommand method url headers Nothing
                           copyButton_ "btn btn-xs btn-ghost" "w-3 h-3" "#issue-curl's innerText" []
                 | Just method <- [d.requestMethod <|> field (.requestMethod)]
                 , Just url <- [field (.urlFull) <|> d.requestPath <|> field (.requestPath)]
                 ]
              <> [ section "issue-tags" "tags" "Tags"
                     $ div_ [class_ "max-md:px-3 px-4 grid md:grid-cols-2 gap-3"]
                     $ forM_ (NE.groupWith (\(k, _, _) -> k) tagCounts) \grp -> do
                       let (key, _, _) = head grp
                           total = sum [n | (_, _, n) <- toList grp]
                           top = take 3 (toList grp)
                           rest = total - sum [n | (_, _, n) <- top]
                           row lbl n = div_ [class_ "flex items-center gap-2 text-xs"] do
                             span_ [class_ "flex-1 min-w-0 truncate text-textStrong"] $ toHtml lbl
                             span_ [class_ "tabular-nums text-textWeak w-10 text-right"] $ toHtml $ show (n * 100 `div` max 1 total) <> "%"
                             div_ [class_ "w-20 h-1.5 rounded bg-fillWeaker overflow-hidden"] $ div_ [class_ "h-full bg-fillBrand-strong", style_ $ "width:" <> show (n * 100 `div` max 1 total) <> "%"] ""
                       div_ [class_ "rounded-lg border border-strokeWeak p-2 space-y-1"] do
                         h4_ [class_ "text-xs font-semibold text-textStrong"] $ toHtml key
                         forM_ top \(_, v, n) -> row v n
                         when (rest > 0) $ row ("Other" :: Text) rest
                 | not (null tagCounts)
                 ]
              <> [ section "issue-contexts" "user" "Contexts"
                     $ div_ [class_ "max-md:px-3 px-4 grid md:grid-cols-2 gap-3 items-start"]
                     $ forM_ contexts \(title, rows) -> div_ [class_ "rounded-lg border border-strokeWeak p-2"] do
                       h4_ [class_ "px-2 pb-1 text-xs font-semibold text-textStrong"] $ toHtml title
                       kvRows_ "" rows
                 | not (null contexts)
                 ]
              <> [ section "issue-grouping" "layer-group" "Event grouping"
                     $ div_ [class_ "max-md:px-3 px-4 space-y-2 text-xs text-textWeak"] do
                       p_ "Events join this issue when their fingerprint matches: the error type, the message with volatile parts (ids, numbers, hex) replaced, the service, the span name, and the in-app frames."
                       kvRows_ ""
                         $ present
                           [ ("normalized", Just $ EF.normalizeMessage d.errorMessage)
                           , ("fingerprint", (.base.hash) <$> errM)
                           , ("across routes", errM >>= (.base.parentHash))
                           , ("same shape", field (.shapeHash))
                           ]
                 | isJust errM
                 ]
              <> [ section "issue-attachments" "paperclip" "Attachments"
                     $ div_ [class_ "max-md:px-3 px-4 flex flex-wrap gap-3"]
                     $ forM_ attachmentUrls \u ->
                       a_ [href_ u, target_ "_blank", rel_ "noopener noreferrer", class_ "block max-w-60 text-xs text-textBrand break-all"]
                         $ if any (`T.isSuffixOf` T.toLower (T.takeWhile (/= '?') u)) [".png", ".jpg", ".jpeg", ".gif", ".webp"]
                           then img_ [src_ u, alt_ "Attachment", loading_ "lazy", class_ "max-h-40 rounded border border-strokeWeak"]
                           else toHtml u
                 | Just attachmentUrls <- [field (.attachments)]
                 ]
      Just (Issues.FeedbackP d) ->
        evidence
          "issue-feedback-evidence"
          "comment"
          "Submission"
          [ ("name", d.userName)
          , ("contact email", d.contactEmail)
          , ("page", d.pageUrl)
          , ("session.id", d.sessionId)
          , ("submitted", Just $ formatUTC d.observedAt)
          ]
      Just (Issues.CronP d) ->
        evidence
          "issue-cron-evidence"
          "clock"
          "Evidence"
          [ ("monitor", Just d.name)
          , ("monitor.slug", Just d.slug)
          , ("failure", Just $ display d.failure)
          , ("expected by", Just $ formatUTC d.expectedBy)
          , ("last check-in", formatUTC <$> d.lastCheckinAt)
          ]
      Just (Issues.UptimeP d) ->
        evidence
          "issue-uptime-evidence"
          "heart-pulse"
          "Evidence"
          [ ("check", Just d.name)
          , ("url", Just d.url)
          , ("failure reason", Just d.reason)
          , ("status code", show <$> d.statusCode)
          , ("expected", Just $ show d.expectedStatus)
          , ("response time", Just $ show d.durationMs <> " ms")
          ]
      Just (Issues.FrontendP d) ->
        evidence
          "issue-click-evidence"
          "arrow-pointer"
          "Evidence"
          [ ("clicked element", Just d.element)
          , ("selector", d.selector)
          , ("page", d.pageUrl)
          , ("clicks", Just $ show d.clickCount <> " within 2s")
          , ("session.id", d.sessionId)
          ]
      Just (Issues.PerformanceP d) ->
        evidence
          "issue-span-evidence"
          "database"
          "Span evidence"
          [ ("transaction", d.transaction)
          , ("parent span", d.parentSpan)
          , (bool "slow query" ("repeating query (\x00d7" <> show d.repeatCount <> ")") (d.kind == Issues.PKNPlusOne), Just d.query)
          , ("database", d.dbSystem)
          , ("duration impact", Just $ show @Text (round d.durationImpactMs :: Int) <> " ms")
          , ("trace.id", Just d.traceId)
          ]
      Just (Issues.QueryAlertP d) ->
        let scope = mkScopedQuery pid (Nothing, Nothing) issue.environment issue.service
            -- Hands the Explorer the alert's own query, its issue boundary, and the page's window.
            explorerLink = externalLink_ [href_ $ (urls.logExplorer pid).logExplorerGet (Just $ applyScopedKqlContext scope d.queryExpression) Nothing (nonEmptyT tp.since) (nonEmptyT tp.from) (nonEmptyT tp.to) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing, data_ "preserve-page-context" ""] "Open query in Explorer"
         in [IssueSection (Just explorerLink) [] "issue-query" "terminal" "Query" $ pre_ [class_ "max-md:px-3 px-4 text-sm font-mono text-textStrong whitespace-pre-wrap break-words"] $ toHtml d.queryExpression]
      Just (Issues.ApiChangeP _)
        | Nothing <- apiEvidence ->
            [section "issue-fields" "list-check" "Field changes" $ emptyState_ def{icon = Just "triangle-exclamation", size = ESCompact} "Field evidence unavailable" "Reload this page to retry."]
      Just (Issues.ApiChangeP d)
        | Just (apiChanges, apiSchema) <- apiEvidence ->
            let snapshots = [(zonedTimeToUTC a.createdAt, change) | a <- apiChanges, change <- V.toList $ getAeson a.fieldEvidence]
                snapshotCell :: Bool -> Bool -> Maybe ApiChanges.FieldSnapshot -> Html ()
                snapshotCell _ _ Nothing = span_ [class_ "text-textWeak"] "Not previously observed"
                snapshotCell typeChanged formatChanged (Just snapshot) = do
                  div_ [class_ $ "font-mono text-xs break-words " <> bool "text-textWeak" "text-textStrong font-medium" typeChanged]
                    $ toHtml
                    $ T.intercalate ", "
                    $ Relude.sort
                    $ (.toText)
                    <$> HS.toList snapshot.structure.types
                  when (formatChanged || not (HS.null snapshot.structure.formats)) $ div_ [class_ "text-xs text-textWeak break-words"] do
                    "Inferred format: "
                    span_ [class_ $ bool "text-textWeak" "text-textStrong font-medium" formatChanged]
                      $ toHtml
                      $ if HS.null snapshot.structure.formats then "None inferred" else T.intercalate ", " (Relude.sort $ map (T.dropAround (`elem` ['{', '}'])) $ HS.toList snapshot.structure.formats)
                  unless (V.null snapshot.examples) do
                    div_ [class_ "mt-2 text-2xs text-textWeak"] "Sample values"
                    let samples values = div_ [class_ "mt-1 space-y-1"] $ forM_ values $ \value ->
                          pre_ [class_ "text-xs font-mono whitespace-pre-wrap break-normal [overflow-wrap:anywhere]"] $ case value of
                            AE.String text -> slashBreaks_ text
                            _ -> toHtml $ decodeUtf8 @Text (AE.encode value)
                    samples (V.take 3 snapshot.examples)
                    when (V.length snapshot.examples > 3) $ details_ do
                      summary_ [class_ "cursor-pointer text-xs text-textBrand py-1"] $ toHtml $ "Show " <> show (V.length snapshot.examples - 3) <> " more sample values"
                      samples (V.drop 3 snapshot.examples)
                -- One tbody per detection time, headed by that time.
                detectedBatches :: [(Maybe UTCTime, a)] -> (a -> Html ()) -> Html ()
                detectedBatches rows row = forM_ (NE.groupAllWith fst rows) \batch@((timestamp, _) :| _) -> tbody_ do
                  tr_
                    $ th_ [scope_ "rowgroup", colspan_ "4", class_ "px-2 py-2 text-xs font-normal text-textWeak bg-fillWeaker/40 border-t border-strokeWeak"]
                    $ case timestamp of
                      Nothing -> "Detection time not recorded"
                      Just at -> do
                        "Detected "
                        time_ [datetime_ $ isoT at] $ toHtml $ fmtDate "%b %-e, %H:%M:%S UTC" at
                  traverse_ (row . snd) batch
                kindOf change = bool ApiChanges.Modified ApiChanges.Added (isNothing change.before)
                legacyFields = [(kind, path) | (kind, paths) <- [(ApiChanges.Added, d.newFields), (ApiChanges.Removed, d.deletedFields), (ApiChanges.Modified, d.modifiedFields)], path <- V.toList paths, (kind, path) `notElem` [(kindOf c, c.path) | (_, c) <- snapshots]]
             in [ section "issue-fields" "list-check" "Field changes"
                    $ if Issues.hasFieldDrift d
                      then div_ [class_ "max-md:px-3 px-4 space-y-3"] do
                        p_ [class_ "text-xs text-textWeak"] "Formats are inferred from sample values and may be inaccurate."
                        div_ [class_ "overflow-x-auto"] $ table_ [class_ "w-full min-w-[40rem] table-fixed text-left text-sm"] do
                          colgroup_ do
                            col_ [class_ "w-[22%]"]
                            col_ [class_ "w-[14%]"]
                            col_ [class_ "w-[32%]"]
                            col_ [class_ "w-[32%]"]
                          thead_ $ tr_ $ forM_ (["Field", "Change", "Before", "After"] :: [Text]) $ th_ [scope_ "col", class_ "p-2 font-medium text-textWeak"] . toHtml
                          detectedBatches (first Just <$> snapshots) \change -> tr_ [class_ "border-t border-strokeWeak align-top"] do
                            let typeChanged = maybe True ((/= change.after.structure.types) . (.structure.types)) change.before
                                formatChanged = maybe True ((/= change.after.structure.formats) . (.structure.formats)) change.before
                            th_ [scope_ "row", class_ "p-2 font-mono text-xs font-normal break-normal [overflow-wrap:anywhere]"] $ toHtml change.path
                            td_ [class_ "p-2 text-xs text-textWeak break-words"] $ toHtml $ case change.before of
                              Just _ | not typeChanged && formatChanged -> "Format changed"
                              _ -> show @Text $ kindOf change
                            td_ [class_ "p-2"] $ snapshotCell typeChanged formatChanged change.before
                            td_ [class_ "p-2"] $ snapshotCell typeChanged formatChanged (Just change.after)
                          let legacyRows =
                                [ ( viaNonEmpty
                                      minimum1
                                      [ zonedTimeToUTC a.createdAt
                                      | a <- apiChanges
                                      , path `V.elem` case kind of
                                          ApiChanges.Added -> a.newFields
                                          ApiChanges.Modified -> a.modifiedFields
                                          ApiChanges.Removed -> V.empty
                                      ]
                                  , (kind, path)
                                  )
                                | (kind, path) <- legacyFields
                                ]
                          detectedBatches legacyRows \(kind, path) -> tr_ [class_ "border-t border-strokeWeak align-top"] do
                            th_ [scope_ "row", class_ "p-2 font-mono text-xs font-normal break-normal [overflow-wrap:anywhere]"] $ toHtml path
                            td_ [class_ "p-2 text-xs text-textWeak break-words"] $ toHtml $ show @Text kind
                            td_ [class_ "p-2 text-xs text-textWeak"] "Historical types and values were not captured for this change."
                            td_ [class_ "p-2"] $ case apiSchema >>= ApiChanges.fieldSnapshot path of
                              Nothing -> span_ [class_ "text-xs text-textWeak"] "No current samples available"
                              Just current -> do
                                div_ [class_ "text-xs text-textWeak mb-1"] "Current observations"
                                snapshotCell False False (Just current)
                      else
                        let (heading, message) = case schemaSubjectFor view of
                              Just (ApiChanges.SpanSchema _) -> ("Span schema observed", "Matching events below show the recorded spans.")
                              Just (ApiChanges.EndpointSchema _) -> ("New endpoint discovered", "This endpoint started receiving traffic. Matching events below show its recorded requests.")
                              Nothing -> ("Schema observed", "Source identity was not recorded for this observation.")
                         in emptyState_ def{icon = Just "list-check", size = ESCompact} heading message
                ]
    -- The log-pattern types differ only in their chip row, which the header owns.
    patternSections sourceField logPattern sampleMessage =
      [ IssueSection (Just $ span_ [class_ "badge badge-sm badge-ghost"] $ toHtml $ sourceFieldLabel sourceField) [] "issue-pattern" "file-lines" "Log pattern" $ renderLogContent_ logPattern
      , -- The sample is re-fetched on every range change, from the URL when it
        -- carries one and from the page's own default otherwise.
        section "issue-sample-section" "terminal" "Event sample"
          $ div_
            [ id_ "issue-sample"
            , hxGet_ $ (urls.issues pid).sampleGet issue.id Nothing Nothing Nothing
            , hxTrigger_ "load, update-query from:window delay:200ms, retryIssueSample"
            , hxSwap_ "innerHTML"
            , term "hx-sync" "this:replace"
            , hxVals_ $ "js:{...(()=>{const p=new URLSearchParams(location.search);return p.get('since')||p.get('from')||p.get('to')?Object.fromEntries(p):" <> TimePicker.rangeJson tp <> "})()}"
            ]
          $ issueSampleCard_ pid sampleMessage SampleLoading
      ]
    traceSection =
      [ IssueSection
          { anchor = "issue-trace"
          , glyph = "chart-waterfall"
          , heading = "Trace"
          , -- Icon state is CSS-driven off the section's fullscreen class; the
            -- click only toggles it. tippy, not daisyUI, whose ::before
            -- bubble is clipped by the card's overflow.
            controls = Just
              $ button_ [type_ "button", class_ "p-1.5 rounded hover:bg-fillWeaker cursor-pointer transition-colors max-md:hidden", Aria.label_ "Toggle fullscreen", term "data-tippy-content" "Expand · Esc to exit", [__|on click toggle .investigation-fullscreen on #issue-trace|]] do
                faSprite_ "expand" "regular" "w-3 h-3 text-textWeak group-[.investigation-fullscreen]/sec:hidden"
                faSprite_ "compress" "regular" "w-3 h-3 text-textWeak hidden group-[.investigation-fullscreen]/sec:block"
          , -- Escape closes an open span panel before it exits
            -- fullscreen. `the first <…/> exists`, not a bare `<…/>`: a query
            -- literal is a lazy object that is truthy even when it matches
            -- nothing, so `if <sel/>` never falls through.
            extra =
              [ tabindex_ "-1"
              , [__|on keydown[key is 'Escape'] from window
                          if the first <#trace-details-open:checked/> exists
                            send closeDetailPanel to #trace_details_container
                          otherwise remove .investigation-fullscreen from me
                          end|]
              ]
          , -- No fixed height: the trace flows at natural height so the page
            -- scroll carries the whole waterfall (see the .investigation-content
            -- overrides in tailwind.css). Fetched here rather than with the page:
            -- a cold read of a multi-thousand-span trace took >56s and 504'd the
            -- whole issue. Opens on the span that actually failed; landing on the
            -- root of a 40-span trace is the work the stack trace should save.
            content =
              div_ [class_ "max-md:px-1 px-2 w-full overflow-x-clip investigation-content", id_ "span-content"]
                $ div_ [id_ "trace_container", class_ "w-full h-full min-w-0"]
                $ div_
                  [ hxGet_ $ traceFragmentUrl pid tId (Just tTs) True Nothing (errM >>= (.base.errorData.spanId))
                  , hxTrigger_ "load"
                  , hxSwap_ "outerHTML"
                  , class_ "h-48 flex items-center justify-center"
                  ]
                $ loadingIndicator_ LdMD LdSpinner
          }
      | Just (tId, tTs) <- [traceRef]
      ]
    -- A query alert is a threshold crossing on an aggregate — no originating request,
    -- so no trace-scoped logs (and 'traceRef' is always Nothing, so no Trace either).
    logsSection
      | issue.issueType == Issues.ApiChange && isNothing traceRef =
          [section "issue-logs" "list-view" "Related logs" $ p_ [class_ "px-4 text-sm text-textWeak"] "Related logs are unavailable because this observation has no recorded trace."]
      | otherwise = [logs | issue.issueType /= Issues.QueryAlert]
      where
        -- A trace happens at an instant, so trace- and service-scoped reads get a
        -- +/-5min window rather than the page's range: 0.37s for the same 14 rows
        -- against 35.9s over +/-2h; the table fetches once it scrolls into view.
        around = windowAround 300
        -- An issue is a record of an occurrence, so its own service and environment
        -- are stronger context than whatever the reader last selected globally. The
        -- shared helper quotes telemetry values and keeps this KQL hand-off aligned
        -- with SQL-backed investigation views. `traceRef` honours First/Recent.
        scoped = applyScopedKqlContext $ ScopedQuery pid (Nothing, Nothing) issue.environment issue.service (fst <$> traceRef)
        (logsQuery, logsRange, logsEmpty) = case (issueHashKey issue, traceRef) of
          (Just key, _) | isLogPatternIssue -> (scoped $ "hashes[*]==\"" <> key <> "\"", tp, "No logs match this pattern in this range.")
          (_, Just (tId, tTs)) -> (scoped $ "kind==\"log\" AND context___trace_id==\"" <> tId <> "\"", around tTs, "No logs share this event\x2019s trace.")
          -- ~24% of error patterns never captured a trace id (log records carry no
          -- trace context; spans always do). The old empty-string fallback rendered
          -- `context___trace_id==""`, which filters nothing and dumped the project's
          -- entire retention window.
          _ -> (scoped "kind==\"log\"", around $ zonedTimeToUTC $ maybe issue.createdAt (.base.updatedAt) errM, "No logs from this service in the five minutes around this event.")
        explorerLink = externalLink_ [href_ $ (urls.logExplorer pid).logExplorerGet (Just logsQuery) Nothing (nonEmptyT logsRange.since) (nonEmptyT logsRange.from) (nonEmptyT logsRange.to) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing, term "data-tippy-content" "Open these logs in the Explorer, with this issue's filter and window applied"] "Open in Explorer"
        logs =
          IssueSection (Just explorerLink) [] "issue-logs" "list-view" "Logs" do
            div_ [id_ "log-content", class_ "max-md:px-1 px-2 flex flex-col lg:flex-row w-full lg:max-h-[70vh]"] do
              div_ [class_ "grow min-w-0 min-h-0 flex flex-col"]
                $ virtualTable pid [term "initialFetchUrl" $ logRowsUrl logsQuery logsRange, term "emptyMessage" logsEmpty]

    replaySection =
      [ section "replay-section" "video" "Session replay"
          $ termRaw "session-replay" [id_ "sessionReplay", term "initialSession" sessionId, term "consoleOpen" "true", term "fullWidth" "true", class_ "block w-full", term "projectId" pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)
      | Just (sessionId, _) <- [replaySession]
      ]


-- | A button that opens the AI panel; @opener@ is where closing returns focus.
aiPanelToggleAttrs, aiPanelOpenerAttrs :: [Attribute]
aiPanelToggleAttrs = [type_ "button", Aria.controls_ "ai_chat_container", term "aria-expanded" "false", term "hx-live:aria-expanded" "q('#ai-panel-toggle').checked ? 'true' : 'false'"]
aiPanelOpenerAttrs = aiPanelToggleAttrs <> [[__|on click set #ai-panel-toggle's opener to me then set #ai-panel-toggle's checked to true then send change to #ai-panel-toggle then call (#ai-chat-input).focus({preventScroll: true})|]]


-- | Collapsible AI chat, open-state in localStorage and driven by a checkbox +
-- group-has variants so it survives htmx swaps.
aiSidePanel_ :: Projects.ProjectId -> Issues.IssueId -> UUIDId "ai_routine_run" -> Html ()
aiSidePanel_ pid issueId requestId = do
  input_
    [ type_ "checkbox"
    , id_ "ai-panel-toggle"
    , class_ "hidden"
    , -- The event name must be quoted: hyperscript reads the `-` in a bare
      -- `load-chat` as minus, which left the panel never loading.
      [__|init set my.checked to (localStorage.getItem('ai-panel-open') == 'true')
            if my.checked trigger 'load-chat' on #ai-response-container end
          end
          on change
            call localStorage.setItem('ai-panel-open', my.checked)
            if my.checked trigger 'load-chat' on #ai-response-container end
          end|]
    ]
  button_
    ( [ id_ "issue-ai-launcher"
      , class_ "absolute right-0 top-3 z-10 flex items-center gap-1.5 bg-fillBrand-strong text-white px-2 py-2.5 rounded-l-lg cursor-pointer shadow-md hover:opacity-90 active:scale-[0.96] transition-[opacity,scale] duration-150 group-has-[#ai-panel-toggle:checked]/ai:hidden"
      , Aria.label_ "Open AI Assistant"
      ]
        <> aiPanelOpenerAttrs
    )
    $ faSprite_ "sparkles" "regular" "w-3.5 h-3.5"
  div_ [class_ "hidden group-has-[#ai-panel-toggle:checked]/ai:block"] $ resizer_ "ai_chat_container" "ai_width" False
  div_ [id_ "ai_chat_container", class_ "hidden group-has-[#ai-panel-toggle:checked]/ai:flex w-[420px] shrink-0 h-full overflow-hidden flex-col bg-bgBase border-l border-t border-strokeWeak"] do
    div_ [class_ "shrink-0 px-4 py-2.5 border-b border-strokeWeak flex items-center justify-between"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "sparkles" "regular" "w-3.5 h-3.5 text-fillBrand-strong"
        sectionLabel_ "AI Assistant"
      button_
        [ type_ "button"
        , class_ "p-1.5 rounded-lg hover:bg-fillWeaker cursor-pointer transition-colors tap-target"
        , Aria.label_ "Close AI Assistant"
        , Aria.controls_ "ai_chat_container"
        , [__|on click set #ai-panel-toggle's checked to false then send change to #ai-panel-toggle then if #ai-panel-toggle's opener call (#ai-panel-toggle).opener.focus({preventScroll: true}) else call (#issue-ai-launcher).focus({preventScroll: true}) end|]
        ]
        $ faSprite_ "xmark" "regular" "w-3 h-3 text-textWeak"
    div_
      [ id_ "ai-response-container"
      , class_ "flex-1 overflow-y-auto flex flex-col px-3"
      , hxGet_ $ (urls.issues pid).aiChatHistoryGet issueId
      , hxTrigger_ "load-chat once"
      , -- htmx 4's native event detail carries no `elt` (the compat shim backfills it only on
        -- the legacy aliases), so branch on the swap target via `event.target` instead.
        term "hx-on::after:swap" "window.evalScriptsFromContent && window.evalScriptsFromContent(event.target === this ? this : this.lastElementChild); this.lastElementChild?.scrollIntoView({behavior: 'smooth', block: 'start'})"
      ]
      ""
    div_ [class_ "shrink-0 border-t border-strokeWeak p-3 flex flex-col gap-2"] do
      aiChatForm_ pid issueId requestId []
      div_ [class_ "flex gap-1.5 flex-wrap"] $ forM_ ["What could cause this?", "Show related logs", "Suggest a fix"] \txt ->
        button_
          [ type_ "button"
          , class_ "text-xs px-2 py-1.5 rounded-full bg-fillWeaker text-textWeak hover:text-textStrong hover:bg-fillWeak transition-colors cursor-pointer tap-target"
          , -- The label travels as a data attribute, not interpolated into the script:
            -- Lucid escapes attribute values, so an apostrophe in a suggestion can't
            -- terminate the hyperscript string literal and break the handler.
            data_ "q" txt
          , [__|on click set #ai-chat-input.value to my @data-q then call #ai-chat-input.form.requestSubmit()|]
          ]
          $ toHtml @Text txt


-- | A successful reply replaces this form and gives the next turn a fresh identity.
aiChatForm_ :: Projects.ProjectId -> Issues.IssueId -> UUIDId "ai_routine_run" -> [Attribute] -> Html ()
aiChatForm_ pid issueId requestId attributes =
  form_
    ( [ id_ "issue-ai-form"
      , hxPost_ $ (urls.issues pid).aiChatPost issueId
      , hxTarget_ "#ai-response-container"
      , hxSwap_ "innerHTML"
      , hxIndicator_ "#ai-chat-loader"
      ]
        <> attributes
    )
    $ div_ [class_ "flex items-center gap-2 bg-fillWeaker rounded-lg px-3 py-2 has-[:focus]:ring-1 has-[:focus]:ring-strokeBrand-weak transition-shadow"] do
      input_ [type_ "hidden", name_ "requestId", value_ requestId.toText]
      input_
        [ class_ "flex-1 bg-transparent border-none outline-none text-textStrong placeholder-textWeak text-sm"
        , placeholder_ "Ask about this issue..."
        , name_ "query"
        , id_ "ai-chat-input"
        , autocomplete_ "off"
        ]
      span_ [class_ "htmx-indicator", id_ "ai-chat-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin text-iconBrand"
      button_ [type_ "submit", class_ "p-1.5 rounded-lg bg-fillBrand-strong text-white hover:opacity-90 active:scale-[0.96] transition-[opacity,scale] duration-150 tap-target cursor-pointer", Aria.label_ "Send message"] $ faSprite_ "arrow-right" "regular" "w-3.5 h-3.5"


-- | Resolve control; @errState@ is passed separately because the handler renders the
-- post-resolve state before the pattern is re-read.
errorResolveAction :: Projects.ProjectId -> ErrorPatterns.ErrorPattern -> ErrorPatterns.ErrorState -> Bool -> Html ()
errorResolveAction pid err errState canResolve =
  when canResolve do
    let actionUrl = (urls.issues pid).resolveErrorPost err.id.unErrorPatternId
        resolveBtn url lbl tip = button_ [class_ "btn btn-sm btn-ghost join-item gap-1.5 text-textSuccess hover:bg-fillSuccess-weak", Aria.label_ tip, term "data-tippy-content" tip, hxPost_ url, hxTarget_ "#error-resolve-action", hxSwap_ "outerHTML"] lbl
    div_ [id_ "error-resolve-action", class_ "join"] do
      if errState == ErrorPatterns.ESResolved
        then button_ [class_ "btn btn-sm btn-ghost text-textWeak", disabled_ "true"] do
          faSprite_ "circle-check" "regular" "w-4 h-4"
          span_ [class_ "max-md:hidden"] $ toHtml $ maybe "Resolved" ("Resolved after " <>) err.resolvedInRelease
        else do
          resolveBtn (actionUrl False) (faSprite_ "circle-check" "regular" "w-4 h-4" >> span_ [class_ "max-md:hidden"] "Resolve") "Resolve issue"
          -- Occurrences still reporting this release will not reopen it; a newer one will.
          whenJust err.lastRelease \rel -> resolveBtn (actionUrl True) (span_ [class_ "text-xs"] "in next release") ("Resolve in the release after " <> rel)


errorSubscriptionAction :: Projects.ProjectId -> ErrorPatterns.ErrorPattern -> Html ()
errorSubscriptionAction pid err = do
  let isActive = err.subscribed
  form_
    [ id_ "issue-subscription-action"
    , class_ "flex items-center gap-2"
    , hxPost_ $ (urls.issues pid).errorSubscriptionPost err.id.unErrorPatternId
    , hxTarget_ "#issue-subscription-action"
    , hxSwap_ "outerHTML"
    , hxTrigger_ "change"
    ]
    do
      span_ [class_ "text-xs text-textWeak flex items-center gap-1"] do
        faSprite_ "bell" "regular" "w-3 h-3"
        span_ [class_ "max-md:hidden"] "Notify"
      select_ [class_ "select select-sm max-md:w-28 w-36", name_ "notifyEveryMinutes", Aria.label_ "Notification frequency"]
        $ options_ (Just $ bool "0" (show err.notifyEveryMinutes) isActive) (("0", "Off") : [(show @Text @Int val, "Every " <> label) | (val, label) <- [(10, "10 min"), (20, "20 min"), (30, "30 min"), (60, "hour"), (360, "6 hrs"), (1440, "24 hrs")]])


newtype AssignErrorForm = AssignErrorForm
  { assigneeId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


newtype ErrorSubscriptionForm = ErrorSubscriptionForm
  { notifyEveryMinutes :: Maybe Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data ErrorPatternRejection = ErrorNotFound | ErrorAssignee AssigneeRejection | ResolveDenied | IncidentChanged
  deriving stock (Eq, Show)


instance Refusal ErrorPatternRejection where
  refusalMessage = \case
    ErrorNotFound -> "Error not found"
    ErrorAssignee a -> refusalMessage a
    ResolveDenied -> "You do not have permission to resolve this error"
    IncidentChanged -> "The incident changed. Refresh and try resolving again."


-- | The error's assignee select, with the assignee before and after the change.
data AssigneeChanged = AssigneeChanged Projects.ProjectId ErrorPatterns.ErrorPatternId (Maybe Projects.UserId, Maybe Projects.UserId) [ProjectMembers.ProjectMemberVM]


instance Outcome AssigneeChanged where
  outcomeBody = toHtml
  outcomeToast (AssigneeChanged _ _ (before, after) _) = Just $ bool "Assignee updated" "Assignee unchanged" (before == after)


instance ToHtml AssigneeChanged where
  toHtml (AssigneeChanged pid eid (_, aidM) mems) = toHtml @(Html ()) do
    div_ [id_ "error-assignee", class_ "flex flex-col gap-2 border-t border-strokeWeak pt-3"] do
      span_ [class_ "text-xs text-textWeak"] "Assignee"
      form_ [hxPost_ $ (urls.issues pid).assignErrorPost eid.unErrorPatternId, hxTarget_ "#error-assignee", hxSwap_ "outerHTML", hxTrigger_ "change", resetFormOnRefusalAttr_]
        $ select_ ([class_ "select select-sm w-full", name_ "assigneeId"] <> [disabled_ "true" | null mems])
        $ options_ (Just $ maybe "" (.toText) aidM) (("", "Unassigned") : [(m.userId.toText, memberLabel m) | m <- mems])
  toHtmlRaw = toHtml


assignErrorPostH :: Projects.ProjectId -> UUID.UUID -> AssignErrorForm -> ATAuthCtx (ActionResult ErrorPatternRejection AssigneeChanged)
assignErrorPostH pid errUuid form = act do
  (sess, _) <- Projects.sessionAndProject pid
  members <- ProjectMembers.selectActiveProjectMembers pid
  withErrorPattern pid errUuid \err -> forM (first ErrorAssignee $ parseAssignee members form.assigneeId) \assigneeIdM -> do
    -- An error older than issue tracking has no issue row to carry the assignee.
    when (assigneeIdM /= err.assigneeId)
      $ Issues.selectIssueByHash pid err.hash Issues.AnyIssue
      >>= maybe (assignError pid assigneeIdM err) \issue -> assignIssues pid sess.user.id [issue{Issues.assigneeId = err.assigneeId}] assigneeIdM
    pure $ AssigneeChanged pid err.id (err.assigneeId, assigneeIdM) members


-- | The project's error pattern, else 'ErrorNotFound'.
withErrorPattern :: Projects.ProjectId -> UUID.UUID -> (ErrorPatterns.ErrorPattern -> ATAuthCtx (Either ErrorPatternRejection a)) -> ATAuthCtx (Either ErrorPatternRejection a)
withErrorPattern pid errUuid found =
  ErrorPatterns.getErrorPatternById (ErrorPatternId errUuid) >>= maybe (pure $ Left ErrorNotFound) found . mfilter ((== pid) . (.projectId))


-- | The resolve control after a resolution; @nextRelease@ when resolved in the release after the error's last.
data ErrorResolved = ErrorResolved Projects.ProjectId ErrorPatterns.ErrorPattern Bool


instance Outcome ErrorResolved where
  outcomeBody = toHtml
  outcomeToast (ErrorResolved _ err nextRelease) = Just $ successToast $ maybe "Error resolved" ("Resolved in the release after " <>) (err.lastRelease <* guard nextRelease)


instance ToHtml ErrorResolved where
  toHtml (ErrorResolved pid err _) = toHtml $ errorResolveAction pid err ErrorPatterns.ESResolved True
  toHtmlRaw = toHtml


resolveErrorPostH :: Projects.ProjectId -> UUID.UUID -> Bool -> ATAuthCtx (ActionResult ErrorPatternRejection ErrorResolved)
resolveErrorPostH pid errUuid inNextRelease = act do
  (sess, _) <- Projects.sessionAndProject pid
  userPermission <- ProjectMembers.getUserPermission pid sess.user.id
  withErrorPattern pid errUuid \err -> do
    let resolved = Right (ErrorResolved pid err inNextRelease) <$ ErrorPatterns.setResolvedInRelease err.id inNextRelease
    if not (userPermission >= Just ProjectMembers.PEdit || err.assigneeId == Just sess.user.id)
      then pure $ Left ResolveDenied
      else
        Time.currentTime >>= resolveErrorAs sess.user err >>= \case
          Incidents.ErrorResolutionDenied -> pure $ Left ResolveDenied
          Incidents.ErrorResolutionConflict conflict -> do
            Log.logAttention "Error resolution could not commit incident updates" (pid, err.id, show @Text conflict)
            pure $ Left IncidentChanged
          Incidents.ErrorResolved -> resolved
          Incidents.ErrorAlreadyResolved -> resolved


-- | Resolve an error pattern and close its incident episode, notifying as the user.
resolveErrorAs :: Projects.User -> ErrorPatterns.ErrorPattern -> UTCTime -> ATAuthCtx Incidents.ErrorResolution
resolveErrorAs user err now = do
  ctx <- ask @AuthContext
  let message iid = Mail.resolvedErrorMessage err.errorData (Projects.userLabel user) now (hostPath ctx.env.hostUrl $ urls.dashboardRedirectGet err.projectId []) (hostPath ctx.env.hostUrl . issueDetailUrl err.projectId <$> iid)
  Incidents.resolveErrorIncident err.projectId err.id user.id now message


data ErrorSubscribed = ErrorSubscribed Projects.ProjectId ErrorPatterns.ErrorPattern


instance Outcome ErrorSubscribed where
  outcomeBody = toHtml
  outcomeToast (ErrorSubscribed _ err) = Just $ bool "Notifications disabled" "Notifications enabled" err.subscribed


instance ToHtml ErrorSubscribed where
  toHtml (ErrorSubscribed pid err) = toHtml $ errorSubscriptionAction pid err
  toHtmlRaw = toHtml


errorSubscriptionPostH :: Projects.ProjectId -> UUID.UUID -> ErrorSubscriptionForm -> ATAuthCtx (ActionResult ErrorPatternRejection ErrorSubscribed)
errorSubscriptionPostH pid errUuid form = act do
  _ <- Projects.sessionAndProject pid
  withErrorPattern pid errUuid \err -> do
    let notifyEveryRaw = fromMaybe 0 form.notifyEveryMinutes
        notifyEvery = clamp (1, 1440) $ if notifyEveryRaw == 0 then 30 else notifyEveryRaw
        shouldSubscribe = notifyEveryRaw > 0
    now <- Time.currentTime
    void $ ErrorPatterns.updateErrorPatternSubscription err.id shouldSubscribe notifyEvery now
    pure $ Right $ ErrorSubscribed pid err{ErrorPatterns.subscribed = shouldSubscribe, ErrorPatterns.notifyEveryMinutes = notifyEvery}


-- | Form for AI chat input
data AIChatForm = AIChatForm {query :: Text, requestId :: Maybe (UUIDId "ai_routine_run")}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Handle AI chat POST request
-- Designed to power the AI chat in the anomalies page. The chat thread is loaded via htmx and theres an input which when submitted gets sent here.
aiChatPostHWithTools :: Maybe AI.ProjectTools -> Projects.ProjectId -> Issues.IssueId -> AIChatForm -> ATAuthCtx (RespHeaders (Html ()))
aiChatPostHWithTools projectTools pid issueId form = do
  (sess, _) <- Projects.sessionAndProject pid
  unlessM (isJust <$> ProjectMembers.getUserPermission pid sess.user.id) $ throwError err403
  when (T.length form.query > 4000) $ throwError err400{errBody = "Query too long. Maximum 4000 characters allowed."}
  issue <- Issues.selectIssueById pid issueId >>= maybe (throwError err404) pure
  appCtx <- ask @AuthContext
  now <- Time.currentTime
  requestId <- maybe (UUIDId <$> UUIDEff.genUUID) pure form.requestId
  let convId = UUIDId issueId.unUUIDId :: UUIDId "conversation"
  unlessM (AI.canReadEvidence (AI.MemberAccess AI.WebSession sess.user.id) pid convId) $ throwError err403
  retained <- Issues.getRoutineRun pid requestId
  let browserOnly = case (.source) <$> retained of Just Issues.InteractiveRun{} -> True; _ -> False
      admit = if browserOnly then Issues.startInteractiveRun else Jobs.startConversation
  admit Nothing pid convId sess.user.id requestId form.query Issues.Observe (Issues.WebIssue issueId) >>= \case
    Issues.ConflictingRequest -> throwError err409{errBody = "Request ID already belongs to another request"}
    Issues.AcceptedRequest _ -> pass
  fullSystemPrompt <- Bots.buildIssuePrompt pid issue now
  when browserOnly do
    mode <- fmap AI.DurableRun $ AI.requireRunContext pid $ Issues.InteractiveClaim sess.user.id convId requestId
    let access = AI.MemberAccess AI.WebSession sess.user.id
        config = (AI.defaultAgenticConfig pid){AI.facetContext = Nothing, AI.customContext = Nothing, AI.conversationId = Just convId, AI.conversationType = Just Issues.CTAnomaly, AI.systemPromptOverride = Just fullSystemPrompt, AI.sourceConfig = Just appCtx.config, AI.useTimefusion = appCtx.env.enableTimefusionReads, AI.access = access, AI.projectTools = projectTools, AI.invocationMode = mode}
    answer <- AI.runAgenticChatWithHistory config form.query appCtx.config.openaiModel appCtx.config.openaiApiKey >>= either (\err -> throwError err502{errBody = encodeUtf8 err}) pure
    void $ AI.completeAgenticResponse config form.query appCtx.config.openaiModel appCtx.config.openaiApiKey answer
  messages <- Issues.selectChatHistory pid convId
  runs <- Issues.listConversationRuns pid convId 10
  nextRequestId <- UUIDId <$> UUIDEff.genUUID
  addRespHeaders do
    issueChatHistory_ pid issueId (any ((== Issues.RunRunning) . (.status)) runs) fullSystemPrompt messages
    aiChatForm_ pid issueId nextRequestId [term "hx-swap-oob" "outerHTML"]


data IssueChatHistory
  = IssueChatMissing Projects.ProjectId
  | -- | The 'Bool' is whether a run is still in flight, which keeps the panel polling.
    IssueChatHistory Projects.ProjectId Issues.IssueId Bool Text [Issues.AIChatMessage]


instance ToHtml IssueChatHistory where
  toHtml (IssueChatMissing pid) = toHtml $ aiChatHistoryView_ (pure . href_) pid []
  toHtml (IssueChatHistory pid issueId running systemPrompt messages) = toHtml $ issueChatHistory_ pid issueId running systemPrompt messages
  toHtmlRaw = toHtml


-- | Handle AI chat history GET request
aiChatHistoryGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders IssueChatHistory)
aiChatHistoryGetH pid issueId = do
  (session, _) <- Projects.sessionAndProject pid
  unlessM (AI.canReadEvidence (AI.MemberAccess AI.WebSession session.user.id) pid (UUIDId issueId.unUUIDId)) $ throwError err403
  now <- Time.currentTime
  Issues.selectIssueById pid issueId >>= \case
    Nothing -> addRespHeaders $ IssueChatMissing pid
    Just issue -> do
      systemPrompt <- Bots.buildIssuePrompt pid issue now
      messages <- Issues.selectChatHistory pid (UUIDId issueId.unUUIDId :: UUIDId "conversation")
      runs <- Issues.listConversationRuns pid (UUIDId issueId.unUUIDId) 10
      addRespHeaders $ IssueChatHistory pid issueId (any ((== Issues.RunRunning) . (.status)) runs) systemPrompt messages


issueChatHistory_ :: Projects.ProjectId -> Issues.IssueId -> Bool -> Text -> [Issues.AIChatMessage] -> Html ()
issueChatHistory_ pid issueId running systemPrompt messages =
  div_
    ([id_ "issue-ai-results"] <> if running then [hxGet_ $ (urls.issues pid).aiChatHistoryGet issueId, hxTrigger_ "every 2s", hxSelect_ "#issue-ai-results", hxSwap_ "outerHTML"] else [])
    do
      disclosure_ [class_ "my-2 border border-strokeWeak rounded-lg"] "px-2.5 py-1.5 text-xs text-textWeak hover:bg-fillWeaker" (faSprite_ "file-lines" "regular" "w-3 h-3" >> "System Prompt")
        $ div_ [class_ "px-2.5 py-2 border-t border-strokeWeak bg-fillWeaker/50 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-48 overflow-y-auto"]
        $ toHtml systemPrompt
      aiChatHistoryView_ (pure . href_) pid messages


-- | Render a single chat response (user question + AI answer)
aiChatResponse_ :: (Text -> [Attribute]) -> Projects.ProjectId -> Maybe Text -> Text -> Maybe [Widget.Widget] -> Maybe [AI.ToolCallInfo] -> Maybe Text -> Html ()
aiChatResponse_ artifactLinkAttrs pid userQuery explanation widgetsM toolCallsM systemPromptM =
  div_ [class_ "animate-fade-in py-3 border-b border-strokeWeak last:border-b-0"] do
    -- User question; routine runs answer the conversation's standing task instead
    whenJust userQuery \q -> div_ [class_ "flex items-start gap-2.5 mb-3"] do
      chatAvatar_ "bg-fillWeak" "user" "text-textWeak"
      p_ [class_ "text-sm text-textStrong mt-0.5"] $ toHtml q
    -- AI response
    div_ [class_ "flex items-start gap-2.5"] do
      chatAvatar_ "bg-fillBrand-weak" "sparkles" "text-fillBrand-strong"
      div_ [class_ "flex-1 min-w-0 mt-0.5"] do
        div_ [class_ "prose prose-sm text-textStrong max-w-none leading-relaxed"] $ renderMarkdown explanation
        let savedDocs = ordNub [toText k | tc <- fold toolCallsM, tc.name == "save_artifact", Just (AE.String raw) <- [tc.rawData], Right k <- [Issues.mkArtifactKey raw]]
        unless (null savedDocs) $ div_ [class_ "mt-3 flex flex-wrap gap-2"] $ forM_ savedDocs \k ->
          a_ (class_ "inline-flex items-center gap-1.5 rounded-md border border-strokeWeak px-2 py-1 text-xs font-medium text-textStrong hover:bg-fillHover" : artifactLinkAttrs ("?doc=" <> k)) do
            faSprite_ "file-lines" "regular" "h-3 w-3 text-iconBrand"
            toHtml k
        whenJust widgetsM $ \widgets -> aiChatWidgets_ pid ("chat-" <> show (abs $ hash (userQuery, explanation)) <> "-") widgets toolCallsM
    -- Collapsed debug info (tool calls + system prompt)
    let toolCalls = fromMaybe [] toolCallsM
    unless (null toolCalls && isNothing systemPromptM)
      $ disclosure_ [class_ "mt-2 ml-[2.125rem] border border-strokeWeak rounded-lg text-xs", data_ "chat-indent" ""] "px-2.5 py-1.5 text-textWeak hover:bg-fillWeaker" (toHtml @Text $ bool "System context" (show (length toolCalls) <> " tool calls") (not (null toolCalls)))
      $ div_ [class_ "px-2.5 py-2 border-t border-strokeWeak bg-fillWeaker/50"] do
        forM_ toolCalls toolCallView_
        whenJust systemPromptM
          $ disclosure_ [class_ "mt-2 border-t border-strokeWeak pt-2" | not (null toolCalls)] "text-textWeak hover:text-textStrong" "System Prompt"
          . div_ [class_ "mt-1 font-mono whitespace-pre-wrap text-textWeak max-h-48 overflow-y-auto"]
          . toHtml
  where
    toolCallView_ :: AI.ToolCallInfo -> Html ()
    toolCallView_ tc =
      div_ [class_ "flex flex-col gap-1 py-2 border-b border-strokeWeak last:border-0"] do
        div_ [class_ "flex items-center gap-2 flex-wrap"] do
          span_ [class_ "font-mono text-xs px-2 py-0.5 bg-fillWeak rounded"] $ toHtml tc.name
          whenJust (Map.lookup "query" tc.args) $ span_ [class_ "text-xs text-textWeak break-all"] . toHtml . show
        unless (T.null tc.resultPreview) $ div_ [class_ "text-xs text-textWeak font-mono pl-4 whitespace-pre-wrap break-all"] $ toHtml $ "→ " <> tc.resultPreview


chatAvatar_ :: Text -> Text -> Text -> Html ()
chatAvatar_ bg icon color = div_ [class_ $ "shrink-0 w-6 h-6 rounded-full flex items-center justify-center " <> bg, data_ "chat-avatar" ""] $ faSprite_ icon "regular" ("w-3 h-3 " <> color)


aiChatWidgets_ :: Projects.ProjectId -> Text -> [Widget.Widget] -> Maybe [AI.ToolCallInfo] -> Html ()
aiChatWidgets_ pid prefix widgets toolCallsM =
  let processedWidgets = maybe widgets (`processWidgetsWithToolData` widgets) toolCallsM
   in div_ [class_ "grid grid-cols-1 gap-3 mt-3"] $ forM_ (zip [0 ..] processedWidgets) $ \(i, widget) ->
        div_ [class_ $ "w-full " <> if widget.wType == Widget.WTStat then "h-28" else "aspect-[3/1] min-h-48"] $ Widget.widget_ widget{Widget._projectId = Just pid, Widget.id = Just (prefix <> show @Text @Int i)}
  where
    -- A widget whose query matches a cached run_query tool call renders from that data
    -- rather than re-querying.
    processWidgetsWithToolData :: [AI.ToolCallInfo] -> [Widget.Widget] -> [Widget.Widget]
    processWidgetsWithToolData toolCalls = map \w -> case w.query >>= findToolCallData toolCalls of
      Nothing -> w
      Just rawJson -> maybe w (\ds -> w{Widget.dataset = Just ds, Widget.eager = Just True}) (toolDataToDataset rawJson)
    findToolCallData toolCalls widgetQuery =
      listToMaybe [rd | tc <- toolCalls, tc.name == "run_query", Just (AE.String q) <- [Map.lookup "query" tc.args], norm q == norm widgetQuery, Just rd <- [tc.rawData]]
      where
        norm = unwords . words -- whitespace-insensitive comparison
    toolDataToDataset json = flip parseMaybe json $ AE.withObject "RawData" \obj -> do
      headers <- obj AE..: "headers" :: Parser [Text]
      dataRows <- obj AE..: "data" :: Parser (V.Vector (V.Vector AE.Value))
      count <- obj AE..:? "count"
      let source = AE.toJSON $ V.cons (AE.toJSON <$> V.fromList headers) (fmap AE.toJSON <$> dataRows)
      pure Widget.WidgetDataset{source, rowsPerMin = Nothing, value = count, from = Nothing, to = Nothing, stats = Nothing}


-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import Data.Aeson qualified as AE
-- >>> import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
-- >>> import Lucid (href_, renderText, toHtml)
-- >>> import Data.Default (def)
-- >>> import Data.Text.Lazy qualified as TL
-- >>> import Data.UUID qualified as UUID
-- >>> import Models.Apis.Issues qualified as Issues
-- >>> import Pkg.DeriveUtils (UUIDId (..))


-- | Shown where an issue's stored @issue_type@ and @issue_data@ do not agree.
--
-- Reachable for rows written before 'Issues.IssuePayload' made the pairing the only
-- constructible thing. The previous code rendered *nothing* for these: an empty
-- region on an incident page, with no log line and no metric to notice it by. A blank
-- panel is indistinguishable from "this issue has no details", which is the worst way
-- for this to fail — the on-call reader's reasonable conclusion is that there is
-- nothing to see. A visible gap is debuggable; an invisible one is not.
--
-- >>> renderText unparsablePayload_
-- "<span class=\"text-xs italic text-textWeak\">details unavailable</span>"
unparsablePayload_ :: Html ()
unparsablePayload_ = span_ [class_ "text-xs italic text-textWeak"] "details unavailable"


-- | Render model turns and operational execution events. Events remain visible
-- to people without being presented to the model as assistant output.
--
-- Model widgets carry no id, yet a table derives its hx-target from one; each gets a
-- fallback, never an empty one:
--
-- >>> let turn = [def{Issues.role = Issues.ChatUser, Issues.content = "slow checkouts"}, def{Issues.role = Issues.ChatAssistant, Issues.content = "{\"query\":\"name != null\",\"visualization\":\"table\"}"}]
-- >>> let html = renderText $ aiChatHistoryView_ (pure . href_) (UUIDId UUID.nil) turn
-- >>> ("hx-target=\"[id=&#39;chat-" `TL.isInfixOf` html, "hx-target=\"[id=&#39;&#39;]\"" `TL.isInfixOf` html)
-- (True,False)
aiChatHistoryView_ :: (Text -> [Attribute]) -> Projects.ProjectId -> [Issues.AIChatMessage] -> Html ()
aiChatHistoryView_ artifactLinkAttrs pid = render
  where
    render (u : a : rest) | u.role == Issues.ChatUser && a.role == Issues.ChatAssistant = answer (Just u.content) a >> render rest
    render (a : rest) | a.role == Issues.ChatAssistant = answer Nothing a >> render rest
    render (event : rest) | event.role == Issues.ChatExecutionEvent = do
      div_ [class_ "my-3 rounded-lg border border-strokeWeak bg-fillWeak px-3 py-2 text-xs text-textWeak"] do
        faSprite_ "circle-info" "regular" "mr-1.5 inline-block h-3.5 w-3.5"
        toHtml event.content
      render rest
    render (_ : rest) = render rest
    render [] = pass
    answer q a = let (explanation, widgets) = parseStoredContent a in aiChatResponse_ artifactLinkAttrs pid q explanation widgets (parseStoredJSON @[AI.ToolCallInfo] a.metadata) Nothing
    -- Stored content is JSON (code blocks stripped) where the LLM produced it, plain text otherwise.
    -- Its charts read the window the answer was written about, not the window ending now.
    parseStoredContent a = case AI.parseLLMResponse a.content of
      Right aiResp ->
        let widgets = AI.runWidgets (Just a.createdAt) aiResp
            structured = isJust aiResp.query || isJust aiResp.visualization || not (null widgets)
         in ( fromMaybe (bool a.content "Here are the requested visualizations:" structured) aiResp.explanation
            , guarded (not . null) widgets <|> stored
            )
      Left _ -> (a.content, stored)
      where
        stored = AI.runWidgets (Just a.createdAt) . (\ws -> AI.LLMResponse Nothing Nothing Nothing ws Nothing Nothing) . mapMaybe (parseMaybe AE.parseJSON) <$> parseStoredJSON @[AE.Value] a.widgets


-- | Decode a stored JSONB value (anomaly metadata, widget lists) into a typed payload.
parseStoredJSON :: AE.FromJSON a => Maybe (Aeson AE.Value) -> Maybe a
parseStoredJSON = (>>= parseMaybe AE.parseJSON . getAeson)


issueListGetH
  :: Projects.ProjectId
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> [Text]
  -> [Text]
  -> ATAuthCtx (RespHeaders (TableResponse IssueVM))
issueListGetH pid filterTM sortM _since pageM perPageM loadM periodM serviceFilters typeFilters = do
  (session, project, bw) <- mkPageCtx pid
  let tab = parseTab filterTM
      currentFilterTab = tabParam tab
      tabFilters = tabIssueFilters tab
      pageInt = fromMaybe 0 $ readMaybe . toString =<< pageM
      perPage = fromMaybe 25 $ readMaybe . toString =<< perPageM
      currentSort = fromMaybe "-last_seen" sortM
      period = fromMaybe "24h" periodM
      scope = mkScopedQuery pid (Nothing, Nothing) session.environment session.service
      issueFilters =
        Issues.applyIssueScope
          scope
          tabFilters
            { Issues.limit = perPage
            , Issues.offset = pageInt * perPage
            , Issues.order = Just currentSort
            , Issues.period = period
            , Issues.services = serviceFilters
            , Issues.types = typeFilters
            }
  freeTierStatus <- checkFreeTierStatus pid project.paymentPlan
  currTime <- Time.currentTime
  members <- ProjectMembers.selectActiveProjectMembers pid
  savedViews <- Issues.selectIssueViews pid
  let names = Map.fromList [(m.userId, memberLabel m) | m <- members]
  ((issues, totalCount), (availableServices, availableTypes)) <-
    concurrently
      ( Issues.selectIssues
          pid
          Issues.PIssueL
          issueFilters
      )
      ( concurrently
          (Hasql.interp [HI.sql| SELECT DISTINCT service FROM apis.issues WHERE project_id = #{pid} AND service IS NOT NULL AND (#{session.environment}::text IS NULL OR environment = #{session.environment}) AND (#{session.service}::text IS NULL OR service = #{session.service}) |])
          (Hasql.interp [HI.sql| SELECT DISTINCT issue_type::text FROM apis.issues WHERE project_id = #{pid} |])
      )

  -- Recover source identity for issues written before subjects were stored.
  schemas <- SchemaCatalog.getByKeysBatch $ V.fromList [(pid, row.base.targetHash) | row <- issues, row.base.issueType == Issues.ApiChange]
  let withSchema row = case (apiChangeData row.base, HM.lookup (pid, row.base.targetHash) schemas) of
        (Just d, Just entry) ->
          row
            & Optics.field @"base"
            %~ \base ->
              base
                { Issues.service = entry.scope.service <|> base.service
                , Issues.issueData =
                    Aeson
                      $ AE.toJSON
                        d
                          { Issues.subject = Just $ ApiChanges.schemaSubject entry.template.keyKind entry.scope
                          , Issues.endpointMethod = fromMaybe d.endpointMethod entry.scope.method
                          , Issues.endpointPath = fromMaybe d.endpointPath entry.scope.urlPath
                          , Issues.endpointHost = fromMaybe d.endpointHost entry.scope.host
                          }
                }
        _ -> row
  let listQuery types = "filter=" <> currentFilterTab <> "&sort=" <> currentSort <> "&period=" <> period <> foldMap ("&service=" <>) serviceFilters <> foldMap ("&type=" <>) types
      listUrl = (urls.issues pid).listGet (Just currentFilterTab) (Just currentSort) Nothing Nothing Nothing Nothing (Just period) serviceFilters
      baseUrl = listUrl typeFilters
      paginationConfig =
        Pagination
          { currentPage = pageInt
          , perPage = perPage
          , totalCount = totalCount
          , baseUrl = baseUrl
          , targetId = "anomalyListContainer"
          }
      serviceMenu = multiSelectFilter "Service" "service" serviceFilters availableServices
      typeMenu = multiSelectFilter "Type" "type" typeFilters availableTypes
      issuesVM = V.fromList $ map (IssueVM currTime . withSchema) issues
      tableActions =
        TableHeaderActions
          { baseUrl
          , targetId = "anomalyListContainer"
          , sortOptions =
              [ ("Newest", "Most recently created", "-created_at")
              , ("Oldest", "Oldest issues first", "+created_at")
              , ("Recently Updated", "Most recently updated", "-updated_at")
              , ("Recently Active", "Most recently seen", "-last_seen")
              , ("Highest count", "Counts use the time range shown in each row", "-event_count")
              , ("Most users", "Most distinct users affected", "-users_count")
              , ("Name (A-Z)", "Sort alphabetically", "+title")
              , ("Name (Z-A)", "Sort reverse alphabetically", "-title")
              ]
          , currentSort
          , filterMenus = [serviceMenu | not (null availableServices)] <> [typeMenu | not (null availableTypes)]
          , activeFilters = [("Service", serviceFilters) | not (null serviceFilters)] <> [("Type", typeFilters) | not (null typeFilters)]
          }
      issuesTable =
        Table
          { config =
              def
                { elemID = "anomalyListForm"
                , containerId = Just "anomalyListContainer"
                , addPadding = True
                , renderAsTable = True
                , bulkActionsInHeader = Just 0
                , refreshOnEvent = Just ("issuesListChanged", baseUrl)
                }
          , columns = issueColumns pid period names (Just $ periodToggle_ baseUrl "anomalyListContainer" period)
          , rows = issuesVM
          , features =
              def
                { rowId = Just issueRowId
                , rowAttrs = Just issueRowAttrs
                , rowLink = Just $ issueRowLink pid
                , rowGroup = Just apiChangeGroup
                , bulkActions = issueBulkActions pid tab members
                , search = Just ClientSide
                , tableHeaderActions = Just tableActions
                , pagination = if totalCount > 0 then Just paginationConfig else Nothing
                , zeroState = Just $ issueZeroState pid tab
                }
          }
      bwconf =
        bw
          { pageTitle = "Issues"
          , menuItem = Just "Issues"
          , freeTierStatus = freeTierStatus
          , headContent = Just highlightJsHead_
          , navTabs = Just $ div_ [class_ "flex items-center gap-1.5"] do
              toHtml
                $ TabFilter
                  { current = currentFilterTab
                  , currentURL = baseUrl
                  , options = [TabFilterOpt (tabParam t) Nothing | t <- [minBound .. maxBound]]
                  }
              -- Each tab differs only in how long the silence lasts and whether
              -- the issue can come back; saying so is what stops "acknowledged"
              -- from being a mystery.
              span_ [class_ "tooltip tooltip-bottom", data_ "tip" (tabBlurb tab)]
                $ faSprite_ "circle-info" "regular" "h-4 w-4 text-iconNeutral"
              -- Category views are presets of the type filter; saved views are stored query strings.
              span_ [class_ "w-px h-4 bg-strokeWeak mx-1"] ""
              navTabStrip_ ViewTabs "Issue type" "tabs-xs md:tabs-sm" [(toHtml lbl, listUrl types, sortNub typeFilters == sortNub types) | (lbl, types) <- [("All", []), ("Errors", ["runtime_exception"]), ("Alerts", ["query_alert"]), ("Log patterns", ["log_pattern", "log_pattern_rate_change"]), ("API changes", ["api_change"]), ("Feedback", ["feedback"])] :: [(Text, [Text])]]
              savedViewsMenu_
                SavedViewsMenu
                  { menuId = "issue-views-menu"
                  , active = (.name) <$> find ((== listQuery typeFilters) . (.query)) savedViews
                  , edited = False
                  , views = [SavedView v.name [href_ $ issuesListUrl pid <> "?" <> v.query] (Just [hxPost_ $ (urls.issues pid).viewDeletePost v.id, hxSwap_ "none"]) | v <- savedViews]
                  , save = [hxPost_ (urls.issues pid).viewSavePost, hxSwap_ "none"]
                  , saveFields = input_ [type_ "hidden", name_ "query", value_ $ listQuery typeFilters]
                  , clear = Nothing
                  }
          }
  addRespHeaders
    $ if loadM == Just "true"
      then TableRowsOnly $ TableRows{columns = issueColumns pid period names Nothing, rows = issuesVM, renderAsTable = True, rowId = Just issueRowId, rowAttrs = Just issueRowAttrs, rowLink = Just $ issueRowLink pid, rowGroup = Just apiChangeGroup, pagination = if totalCount > 0 then Just paginationConfig else Nothing}
      else TablePage $ PageCtx bwconf issuesTable
  where
    apiChangeGroup :: IssueVM -> Maybe Text
    apiChangeGroup (IssueVM _ issue) = do
      d <- apiChangeData issue.base
      let label = case d.subject of
            Just (ApiChanges.SpanSchema _) -> "Span schema changes"
            _ -> "API changes"
      pure $ label <> " · " <> maybe "Source unavailable" snd (issueSource issue.base)

    -- Inbox additionally hides severity='low' so demoted silent drops don't clutter it.
    tabIssueFilters :: IssueTab -> Issues.IssueFilters
    tabIssueFilters = \case
      TabAcknowledged -> notSpam{Issues.ack = Issues.IsNotNull}
      TabArchived -> notSpam{Issues.archive = Issues.IsNotNull}
      TabInbox -> notSpam{Issues.ack = Issues.IsNull, Issues.archive = Issues.IsNull, Issues.hideLowSeverity = True}
      TabSpam -> Issues.defIssueFilters{Issues.spam = Issues.IsNotNull}
      TabSynthetic -> notSpam{Issues.synthetic = True}
      where
        notSpam = Issues.defIssueFilters{Issues.spam = Issues.IsNull}


-- | Which issues tab is being viewed.
--
-- This was 'Text' matched against string literals in four separate places — the
-- filter/label pair in 'issueListGetH', 'tabBlurb', 'issueBulkActions' and
-- 'issueZeroState' — each with its own silent fall-through to Inbox. A renamed or
-- mistyped tab therefore degraded /one/ surface to Inbox behaviour while the others kept
-- working, and nothing failed anywhere. Parsing once here makes all four exhaustive.
--
-- The wire spelling is capitalised because that is what live issue URLs and bookmarks
-- already carry (@?filter=Acknowledged@); 'tabParam' is the single source of it.
--
-- >>> map tabParam [minBound .. maxBound]
-- ["Inbox","Acknowledged","Archived","Spam","Synthetic"]
data IssueTab = TabInbox | TabAcknowledged | TabArchived | TabSpam | TabSynthetic
  deriving stock (Bounded, Enum, Eq, Ord, Show)


-- | The tab's wire spelling, used for both the URL and the visible label.
tabParam :: IssueTab -> Text
tabParam = \case
  TabInbox -> "Inbox"
  TabAcknowledged -> "Acknowledged"
  TabArchived -> "Archived"
  TabSpam -> "Spam"
  TabSynthetic -> "Synthetic"


-- | An absent or unrecognised @filter@ lands on Inbox: the tab is a navigation
-- affordance, so a stale bookmark should show something useful rather than fail.
--
-- >>> map parseTab [Just "Archived", Just "Acknowledged", Just "Spam", Just "nonsense", Nothing]
-- [TabArchived,TabAcknowledged,TabSpam,TabInbox,TabInbox]
parseTab :: Maybe Text -> IssueTab
parseTab = fromMaybe TabInbox . (inverseMap tabParam =<<)


-- | One line under the tab strip saying what the tab *means*.
tabBlurb :: IssueTab -> Text
tabBlurb = \case
  TabAcknowledged -> "Someone owns these. Notifications are paused until the acknowledgement expires or the issue regresses."
  TabArchived -> "Not actionable. Hidden and never notified — unarchive to bring one back."
  TabInbox -> "Needs triage. Acknowledge to pause notifications, or archive if it isn't actionable."
  TabSpam -> "Marked as spam, mostly user feedback. Hidden from every other tab."
  TabSynthetic -> "Errors only automated browsers (end-to-end tests, bots) have hit. Hidden from every other tab until a real user hits one."


-- | Bulk actions offered on each tab: only transitions that make sense from the
-- state you're looking at.
issueBulkActions :: Projects.ProjectId -> IssueTab -> [ProjectMembers.ProjectMemberVM] -> [BulkAction]
issueBulkActions pid tab members =
  [ BulkAction{icon = Just i, title = t, uri = url a, choices = cs}
  | (i, t, a, cs) <- case tab of
      TabAcknowledged -> [("arrow-rotate-left", "Unacknowledge", BAUnacknowledge, []), ("archive", "Archive", BAArchive, [])] <> triage
      TabArchived -> [("arrow-rotate-left", "Unarchive", BAUnarchive, []), ("code-merge", "Unmerge", BAUnmerge, [])]
      TabSpam -> [("arrow-rotate-left", "Not spam", BAUnspam, [])]
      (TabInbox; TabSynthetic) -> [("check", "Acknowledge", BAAcknowledge, []), ("archive", "Archive", BAArchive, [])] <> triage
  ]
  where
    url a = (urls.issues pid).bulkActionsPost a Nothing Nothing
    withValue a = (urls.issues pid).bulkActionsPost a Nothing . Just
    triage =
      [ ("circle-check", "Resolve", BAResolve, [])
      , ("flag", "Priority", BAPriority, [(T.toTitle (display sev), withValue BAPriority (display sev)) | sev <- [minBound .. maxBound :: Issues.IssueSeverity]])
      , ("user", "Assign", BAAssign, ("Unassigned", url BAAssign) : [(memberLabel m, withValue BAAssign m.userId.toText) | m <- members])
      , ("ellipsis", "More", BAMerge, [("Merge into oldest", url BAMerge), ("Mark as spam", url BASpam)])
      ]


issueZeroState :: Projects.ProjectId -> IssueTab -> ZeroState
issueZeroState pid = \case
  TabAcknowledged ->
    ZeroState "circle-check" "Nothing acknowledged" "Acknowledge an issue to pause its notifications while you work on it." (ESLink inboxUrl "Go to Inbox")
  TabSpam ->
    ZeroState "ban" "No spam" "Feedback or issues marked as spam land here, out of the Inbox." (ESLink inboxUrl "Go to Inbox")
  TabSynthetic ->
    ZeroState "robot" "No synthetic errors" "Errors seen only from automated browsers land here, out of the Inbox, as an early warning." (ESLink inboxUrl "Go to Inbox")
  TabArchived ->
    ZeroState "archive" "Nothing archived" "Archive the issues that aren't worth acting on. They stay hidden and never notify." (ESLink inboxUrl "Go to Inbox")
  TabInbox ->
    ZeroState "empty-set" "Nothing to triage" "New issues and errors land here automatically once you integrate an SDK." (ESLink "https://monoscopehq.com/docs/sdks/" "View SDK setup guides")
  where
    inboxUrl = (urls.issues pid).listGet (Just $ tabParam TabInbox) Nothing Nothing Nothing Nothing Nothing Nothing [] []


issueRowAttrs :: IssueVM -> [Attribute]
issueRowAttrs (IssueVM _ issue) = class_ "group/row hover:bg-fillWeaker" : sty
  where
    -- The edge bar and the severity pill already say it; a row tint read as zebra striping.
    sty = case issue.base.severity of
      Issues.Critical -> [style_ "box-shadow: inset 3px 0 0 var(--color-fillError-strong)"]
      Issues.Warning -> [style_ "box-shadow: inset 3px 0 0 var(--color-fillWarning-strong)"]
      Issues.Info -> []
      Issues.Low -> []


apiChangeData :: Issues.Issue -> Maybe Issues.APIChangeData
apiChangeData issue = Issues.issuePayload issue >>= preview (_Ctor @"ApiChangeP")


-- API-change ownership is the monitored host; telemetry scopes still use the recorded service.
issueSource :: Issues.Issue -> Maybe (Text, Text)
issueSource issue = case apiChangeData issue of
  Just d | Just (ApiChanges.SpanSchema scope) <- d.subject -> ("Service",) <$> (scope.service <|> issue.service)
  Just d | Just host <- Issues.knownHost d -> Just ("Host", host)
  _ -> ("Service",) <$> nonBlank issue.service


nonBlank :: Maybe Text -> Maybe Text
nonBlank = mfilter (not . T.null . T.strip)


-- | A @<wbr>@ after each @/@ so long paths can wrap.
slashBreaks_ :: Text -> Html ()
slashBreaks_ = sequence_ . intersperse (toHtml @Text "/" >> wbr_ []) . map toHtml . T.splitOn "/"


issueRowLink :: Projects.ProjectId -> IssueVM -> Text
issueRowLink pid (IssueVM _ issue) = issueDetailUrl pid issue.base.id


issueRowId :: IssueVM -> Text
issueRowId (IssueVM _ issue) = issue.base.id.toText


-- | (icon, colorClass, tooltip) — uses shape+color so status isn't color-only.
-- Archived and acknowledged outrank severity; below them, Critical and Warning get
-- their own icon. A plain active issue gets none: a neutral icon on most rows hid the ones that mattered.
anomalyStatusIndicator :: Issues.Issue -> Maybe (Text, Text, Text)
anomalyStatusIndicator issue
  | isJust issue.archivedAt = Just ("archive", "text-fillStrong", "Archived \x2014 hidden, no notifications")
  | isJust issue.acknowledgedAt = Just ("bell-slash", "text-fillSuccess-strong", "Acknowledged \x2014 notifications paused")
  | otherwise = case issue.severity of
      Issues.Critical -> Just ("octagon-exclamation", "text-fillError-strong", "Critical")
      Issues.Warning -> Just ("triangle-alert", "text-fillWarning-strong", "Warning")
      Issues.Info -> Nothing
      Issues.Low -> Nothing


statusIcon_ :: Text -> Issues.Issue -> Html ()
statusIcon_ cls b = case anomalyStatusIndicator b of
  Just (icon, iconColor, tooltip) -> span_ [class_ $ cls <> " w-3.5 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon "regular" "w-3.5 h-3.5"
  Nothing -> span_ [class_ $ cls <> " w-3.5", Aria.hidden_ "true"] mempty


data IssueVM = IssueVM UTCTime Issues.IssueL
  deriving stock (Show)


issueColumns :: Projects.ProjectId -> Text -> Map.Map Projects.UserId Text -> Maybe (Html ()) -> [Column IssueVM]
issueColumns pid period names toggleM =
  [ col "Issue" renderIssueMainCol & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col "Last seen" (\(IssueVM now issue) -> lastSeenCell_ now (Just issue.lastSeen)) & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Activity" activityCol & withAttrs [class_ "w-40 max-md:hidden"] & maybe identity withColHeaderExtra toggleM
  , col "Count" countCol & withAttrs [class_ "w-36 min-w-36 max-md:hidden"]
  ]
  where
    countContext issue =
      ( if issue.base.issueType == Issues.ApiChange then "detection" else "event"
      , if issue.base.issueType `elem` [Issues.RuntimeException, Issues.LogPattern, Issues.LogPatternRateChange] then period else "all time"
      )
    countCol (IssueVM _ issue) = do
      let (noun, scope) = countContext issue
      eventsCountCell_ (scope /= "all time") issue.eventCount
      span_ [class_ "block text-2xs text-textWeak whitespace-nowrap"] $ toHtml $ noun <> (if issue.eventCount == 1 then "" else "s") <> " · " <> scope <> bool (" · " <> countNoun issue.usersCount "user") "" (issue.usersCount == 0)
    activityCol (IssueVM _ issue) = sparkline_ $ V.toList issue.activityBuckets
    renderIssueMainCol (IssueVM currTime issue) = do
      let b = issue.base
          isAcknowledged = isJust b.acknowledgedAt
          isArchived = isJust b.archivedAt
          issueUrl = issueDetailUrl pid b.id
          ackUrl = if isAcknowledged then (urls.issues pid).unAcknowledgeGet b.id else (urls.issues pid).acknowledgeGet b.id Nothing
          archiveUrl = if isArchived then (urls.issues pid).unarchiveGet b.id else (urls.issues pid).archiveGet b.id Nothing
          stateBadges = do
            severityBadge_ b.severity
            issueStateBadge_ issue.latestStateEvent
            -- Silence has an end, and the list is where you need to see it without opening anything.
            whenJust (zonedTimeToUTC <$> b.acknowledgedUntil <* b.acknowledgedAt) $ silenceBadge_ "Ack'd" "Acknowledged" currTime
            whenJust ((`Map.lookup` names) =<< b.assigneeId) \n ->
              span_ [class_ "badge badge-sm badge-ghost gap-1 shrink-0", term "data-tippy-content" "Assignee"] $ faSprite_ "user" "regular" "h-3 w-3" >> toHtml n
      div_ [class_ "flex flex-col gap-1 py-0.5 min-w-0"] do
        div_ [class_ "flex items-center gap-2 min-w-0"] do
          div_ [class_ "text-sm truncate min-w-0"] do
            statusIcon_ "inline-flex align-middle mr-1" b
            span_ [class_ "text-xs tabular-nums mr-1 text-textWeak max-md:text-textStrong max-md:font-medium"] $ toHtml $ "#" <> show b.seqNum <> " "
            a_ ([href_ issueUrl, class_ "font-medium text-textStrong hover:text-textBrand transition-colors"] <> navTabAttrs) $ renderIssueTitle_ issue
          span_ [class_ "shrink-0 flex items-center gap-1.5 max-md:hidden"] stateBadges
          div_ [class_ "ml-auto shrink-0 flex gap-1 items-center opacity-0 pointer-events-none group-hover/row:opacity-100 group-hover/row:pointer-events-auto focus-within:opacity-100 focus-within:pointer-events-auto transition-opacity max-md:hidden"] do
            inlineIconBtn_ (bool "Acknowledge \x2014 pause notifications" "Unacknowledge \x2014 resume notifications" isAcknowledged) (bool "check" "arrow-rotate-left" isAcknowledged) [hxPost_ ackUrl]
            unless isAcknowledged
              $ durationMenu_ ("ack-pop-" <> b.id.toText) "Acknowledge for\x2026" [] (\q -> [hxPost_ $ (urls.issues pid).acknowledgeGet b.id (readMaybe $ toString q), hxSwap_ "none"]) (inlineIconBtn_ "Acknowledge for a set time" "clock" . popoverTrigger_)
            inlineIconBtn_ (bool "Archive \x2014 hide it and stop notifying" "Unarchive \x2014 move back to the Inbox" isArchived) "archive" [hxPost_ archiveUrl]
        div_ [class_ "hidden max-md:flex items-center gap-1.5 flex-wrap"] stateBadges
        div_ [class_ "max-md:hidden"] $ issuePreview_ (Just currTime) issue
        div_ [class_ "hidden max-md:flex items-center justify-between text-xs text-textWeak"] do
          div_ [class_ "flex items-center gap-1.5"] do
            span_ [class_ $ "tabular-nums" <> bool "" " font-medium text-textStrong" (issue.eventCount > 100)] $ toHtml $ let (noun, scope) = countContext issue in countNoun issue.eventCount noun <> " · " <> scope
            span_ [class_ "opacity-30"] "·"
            span_ [] $ toHtml $ agoText currTime $ zonedTimeToUTC b.createdAt
          div_ [class_ "flex items-center gap-3"] do
            button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "none", hxPost_ ackUrl] $ toHtml $ bool "Ack" "Unack" isAcknowledged
            button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "none", hxPost_ archiveUrl] $ toHtml $ bool "Archive" "Unarchive" isArchived


highlightJsHead_ :: Monad m => HtmlT m ()
highlightJsHead_ = do
  link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-light.min.css"), media_ "screen", id_ "hljs-light"]
  link_ [rel_ "stylesheet", href_ (assetUrl "/public/assets/deps/highlightjs/atom-one-dark.min.css"), media_ "screen", id_ "hljs-dark"]
  script_ [src_ (assetUrl "/public/assets/deps/highlightjs/highlight.min.js")] ("" :: Text)
  script_ [src_ (assetUrl "/public/assets/deps/highlightjs/sql.min.js")] ("" :: Text)
  script_
    """
    function setHljsTheme() {
      const dark = document.body.getAttribute('data-theme') === 'dark';
      document.getElementById('hljs-light').disabled = dark;
      document.getElementById('hljs-dark').disabled = !dark;
    }
    function highlightSnippets(root) { root.querySelectorAll('code:not(.hljs)').forEach(el => hljs.highlightElement(el)); }
    document.addEventListener('DOMContentLoaded', () => {
      setHljsTheme();
      highlightSnippets(document);
      // The theme is a data-theme attribute on body, written by the toggle, the cookie
      // restore and the OS-preference listener alike. Observing the attribute keeps the
      // stylesheet in step with all three; setHljsTheme used to run once at load, so
      // toggling the theme afterwards left the wrong sheet enabled.
      new MutationObserver(setHljsTheme).observe(document.body, { attributeFilter: ['data-theme'] });
    });
    // htmx 4's native event detail has no `elt` (see the comment on the chat container's
    // hx-on::after:swap), so `e.detail.elt` was undefined and highlighting never re-ran
    // after a swap.
    document.addEventListener('htmx:after:swap', e => highlightSnippets(e.target));
    """


renderLogContent_ :: Monad m => Text -> HtmlT m ()
renderLogContent_ txt =
  if "⇒" `T.isInfixOf` txt
    then div_ [class_ "flex flex-wrap items-center gap-1 p-4 max-h-80 overflow-y-auto"] $ renderSummaryText_ txt
    else div_ [class_ "p-4 max-h-80 overflow-y-auto"] $ pre_ [class_ "text-sm text-textWeak font-mono whitespace-pre-wrap [&_code.hljs]:!bg-transparent [&_code.hljs]:!p-0"] $ code_ [] $ toHtml txt


renderSummaryText_ :: Monad m => Text -> HtmlT m ()
renderSummaryText_ = traverse_ (summaryToken_ False) . summaryWords


renderIssueTitle_ :: Issues.IssueL -> Html ()
renderIssueTitle_ Issues.IssueL{base}
  | Just d <- apiChangeData base =
      let sourceTitle = case d.subject of
            Just (ApiChanges.SpanSchema scope) -> fromMaybe "Span schema observation" scope.spanName
            _ | Just method <- Issues.knownMethod d -> method <> " " <> d.endpointPath
            _ -> "Schema observation"
       in span_ [term "data-tippy-content" sourceTitle] $ toHtml sourceTitle
  | T.null title = "(Untitled)"
  | "⇒" `T.isInfixOf` title = renderSummaryText_ title
  | looksLikeRawPattern title = span_ [class_ "font-mono text-xs break-all"] $ renderWithPlaceholders_ title
  | otherwise = renderWithPlaceholders_ title
  where
    title =
      foldl'
        (\t pfx -> fromMaybe t $ T.stripPrefix pfx t)
        base.title
        ["New Log Pattern: ", "Log Pattern Spike: ", "Log Pattern Drop: ", "New Log Pattern Detected: "]
    looksLikeRawPattern t = any (`T.isInfixOf` t) [";right-", "v{", "<*>", "]{", "ERROR ERROR"]


-- | Render text with <> placeholders styled as distinct tokens
renderWithPlaceholders_ :: Monad m => Text -> HtmlT m ()
renderWithPlaceholders_ = mconcat . intersperse (span_ [class_ "text-textWeak opacity-60"] "<>") . map toHtml . T.splitOn "<>"


issueCardCompact_ :: Projects.ProjectId -> UTCTime -> Issues.IssueL -> Html ()
issueCardCompact_ pid now issue = do
  let b = issue.base
      issueUrl = issueDetailUrl pid b.id
  a_ ([href_ issueUrl, class_ "block border border-strokeWeak rounded-xl p-3 hover:bg-bgRaised transition-colors"] <> navTabAttrs) do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      statusIcon_ "shrink-0" b
      span_ [class_ "text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ "#" <> show b.seqNum
      span_ [class_ "text-sm font-medium text-textStrong truncate min-w-0"] $ renderIssueTitle_ issue
      severityBadge_ b.severity
      span_ [class_ "text-xs text-textWeak shrink-0 ml-auto"] $ toHtml $ agoText now $ zonedTimeToUTC b.createdAt
    issuePreview_ Nothing issue


-- | Only Critical and Warning carry a badge; Info and Low are deliberately unbadged.
--
-- Took 'Text' and matched @"critical"@/@"warning"@ with a fall-through, while all three
-- callers held an 'Issues.IssueSeverity' and 'display'ed it just to be re-matched. That
-- round-trip meant renaming a constructor would silently drop *both* badges instead of
-- failing to compile. Spelling the two silent cases out makes the omission a decision.
severityBadge_ :: Issues.IssueSeverity -> Html ()
severityBadge_ = \case
  Issues.Critical -> tonePill_ ErrorBadge "CRITICAL"
  Issues.Warning -> tonePill_ WarningBadge "WARNING"
  Issues.Info -> pass
  Issues.Low -> pass


issueStateBadge_ :: Maybe Issues.IssueEvent -> Html ()
issueStateBadge_ = \case
  Just Issues.IERegressed -> tonePill_ ErrorBadge "REGRESSED"
  Just Issues.IEEscalated -> tonePill_ ErrorBadge "ESCALATED"
  Just Issues.IEResolved -> tonePill_ SuccessBadge "RESOLVED"
  Just Issues.IEAutoResolved -> tonePill_ SuccessBadge "RESOLVED"
  Just Issues.IEReopened -> tonePill_ WarningBadge "REOPENED"
  Just Issues.IEAckExpired -> tonePill_ WarningBadge "ACK EXPIRED"
  _ -> pass


-- | Type, service, age (when given the render clock) and a payload snippet on one line.
issuePreview_ :: Maybe UTCTime -> Issues.IssueL -> Html ()
issuePreview_ nowM Issues.IssueL{base} = div_ [class_ "flex items-center gap-2 min-w-0 overflow-hidden text-xs text-textWeak"] do
  issueTypeChip_ True base.issueType
  whenJust (issueSource base) $ \(label, value) -> span_ [class_ "shrink-0", term "data-tippy-content" label] $ toHtml value
  whenJust nowM \now -> do
    let created = zonedTimeToUTC base.createdAt; age = agoText now created
    span_ [class_ "shrink-0 opacity-40"] "·"
    span_ [class_ "shrink-0 whitespace-nowrap", term "data-tippy-content" $ "First seen " <> formatUTC created] $ toHtml $ maybe age (<> " old") $ T.stripSuffix " ago" age
  span_ [class_ "shrink-0 opacity-40"] "·"
  snippet
  where
    snippet = case Issues.issuePayload base of
      Just (Issues.RuntimeExceptionP d) -> previewSnippet $ d.errorType <> ": " <> d.errorMessage
      Just (Issues.QueryAlertP d) -> previewSnippet d.queryExpression
      Just (Issues.LogPatternP d) -> logPatternPreview d.logPattern d.sampleMessage
      Just (Issues.LogPatternRateChangeP d) -> logPatternPreview d.logPattern d.sampleMessage
      Just (Issues.PerformanceP d) -> previewSnippet d.query
      Just (Issues.CronP d) -> previewSnippet $ d.slug <> " \x2014 " <> display d.failure
      Just (Issues.FeedbackP d) -> previewSnippet d.message
      Just (Issues.UptimeP d) -> previewSnippet $ d.url <> " \x2014 " <> d.reason
      Just (Issues.FrontendP d) -> previewSnippet $ d.element <> foldMap (" on " <>) d.pageUrl
      Just (Issues.ApiChangeP d) ->
        previewSnippet $ case [countNoun (V.length fs) "field" <> " " <> verb | (verb, fs) <- [("added", d.newFields), ("modified", d.modifiedFields), ("removed", d.deletedFields)], not (V.null fs)] of
          [] -> case d.subject of
            Just (ApiChanges.SpanSchema _) -> "Span schema observed"
            _ -> "New endpoint observed"
          changes -> T.intercalate " · " changes
      Nothing -> unparsablePayload_
    previewSnippet txt = span_ [class_ "font-mono truncate min-w-0", term "data-tippy-content" txt] $ renderWithPlaceholders_ $ unescSummary txt
    logPatternPreview pat sampleMsg
      | "⇒" `T.isInfixOf` pat = span_ [class_ "truncate min-w-0"] $ renderSummaryText_ pat
      | Just msg <- sampleMsg, not (T.null msg) = previewSnippet msg
      | otherwise = previewSnippet pat


-- | Acknowledge control for the issue detail header. Unacknowledged: a primary
-- button (silence until it regresses) joined to a caret opening the duration
-- menu. Acknowledged: the remaining time, which un-acknowledges on click.
issueAcknowledgeButton :: Projects.ProjectId -> Issues.IssueId -> UTCTime -> Maybe UTCTime -> Html ()
issueAcknowledgeButton pid aid now untilM = div_ [id_ ctlId, class_ "inline-flex"] case untilM of
  Just until' ->
    button_
      ( [ type_ "button"
        , class_ "btn btn-sm gap-1.5 bg-fillSuccess-weak text-textSuccess border-strokeSuccess-weak tooltip tooltip-bottom"
        , data_ "tip" "Notifications are paused. Unacknowledge to resume them."
        , Aria.label_ "Unacknowledge issue"
        ]
          <> req ((urls.issues pid).unAcknowledgeGet aid)
      )
      do
        faSprite_ "bell-slash" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] $ toHtml $ untilLabel "Acknowledged" now until'
  Nothing -> div_ [class_ "join"] do
    button_
      ( [ type_ "button"
        , class_ "btn btn-sm btn-primary join-item gap-1.5 tooltip tooltip-bottom"
        , data_ "tip" "Pause notifications until this regresses"
        , Aria.label_ "Acknowledge issue"
        ]
          <> req ((urls.issues pid).acknowledgeGet aid Nothing)
      )
      do
        faSprite_ "check" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] "Acknowledge"
    durationMenu_ (ctlId <> "-menu") "Acknowledge for\x2026" [] (req . (urls.issues pid).acknowledgeGet aid . readMaybe . toString) \popId ->
      button_ ([type_ "button", class_ "btn btn-sm btn-primary join-item px-2", Aria.label_ "Acknowledge for a set time"] <> popoverTrigger_ popId)
        $ faSprite_ "chevron-down" "regular" "w-3 h-3"
  where
    ctlId = "ack-ctl-" <> aid.toText
    req url = [hxPost_ url, hxTarget_ ("#" <> ctlId), hxSwap_ "outerHTML"]


issueArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
issueArchiveButton pid aid archived = div_ [id_ ctlId, class_ "inline-flex"] do
  if archived
    then button_ ([type_ "button", class_ "btn btn-sm gap-1.5 tooltip tooltip-bottom btn-ghost bg-fillWarning-weak text-textWarning border-strokeWarning-weak", data_ "tip" "Move back to the Inbox", Aria.label_ "Unarchive issue"] <> req ((urls.issues pid).unarchiveGet aid)) do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "max-md:hidden"] "Unarchive"
    else div_ [class_ "join"] do
      button_ ([type_ "button", class_ "btn btn-sm btn-ghost join-item gap-1.5 tooltip tooltip-bottom", data_ "tip" "Not actionable \x2014 hide it and stop notifying", Aria.label_ "Archive issue"] <> req ((urls.issues pid).archiveGet aid Nothing)) do
        faSprite_ "archive" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] "Archive"
      durationMenu_ (ctlId <> "-menu") "Archive for\x2026" [("Until it escalates", "escalating")] (req . (urls.issues pid).archiveGet aid . parseQueryParamMaybe) \popId ->
        button_ ([type_ "button", class_ "btn btn-sm btn-ghost join-item px-2", Aria.label_ "Archive for a set time"] <> popoverTrigger_ popId)
          $ faSprite_ "chevron-down" "regular" "w-3 h-3"
  where
    ctlId = "archive-ctl-" <> aid.toText
    req url = [hxPost_ url, hxTarget_ ("#" <> ctlId), hxSwap_ "outerHTML"]


-- | The issue's type as icon + label. @compact@ is the list\'s chip: tighter,
-- abbreviated where the full label would not fit in a row, with the full text on
-- hover. The detail header takes the roomier form.
issueTypeChip_ :: Bool -> Issues.IssueType -> Html ()
issueTypeChip_ compact issueType =
  span_
    ( [class_ $ bool "flex items-center gap-1.5 text-xs font-medium " "flex items-center gap-1 text-2xs whitespace-nowrap " compact <> color]
        <> [term "data-tippy-content" fullTxt | compact]
    )
    do
      faSprite_ icon "regular" $ bool "w-3 h-3" "w-3 h-3 shrink-0" compact
      toHtml $ bool fullTxt shortTxt compact
  where
    color, icon, fullTxt, shortTxt :: Text
    (color, icon, fullTxt) = case issueType of
      Issues.RuntimeException -> ("text-fillError-strong", "triangle-alert", "Error")
      Issues.QueryAlert -> ("text-fillWarning-strong", "zap", "Alert")
      Issues.LogPattern -> ("text-fillInformation-strong", "file-text", "Log Pattern")
      Issues.LogPatternRateChange -> ("text-fillWarning-strong", "activity", "Rate Change")
      Issues.Performance -> ("text-fillWarning-strong", "gauge-high", "Performance")
      Issues.Frontend -> ("text-fillWarning-strong", "arrow-pointer", "Frontend")
      Issues.Uptime -> ("text-fillError-strong", "heart-pulse", "Uptime")
      Issues.Cron -> ("text-fillError-strong", "clock", "Cron")
      Issues.Feedback -> ("text-fillBrand-strong", "comment", "Feedback")
      Issues.ApiChange -> ("text-fillInformation-strong", "info", "Schema change")
    shortTxt = case issueType of
      Issues.LogPattern -> "Log"
      Issues.LogPatternRateChange -> "Rate"
      _ -> fullTxt


-- | Render a log level chip, inferring ERROR severity from the pattern when the
-- log_level field is missing (e.g. http logs lacking an explicit level but with
-- 4xx/5xx status). Mirrors @isIssueWorthy@ in BackgroundJobs.
logLevelChip_ :: Monad m => Maybe Text -> Text -> HtmlT m ()
logLevelChip_ logLevel pat =
  let effective = case logLevel of
        Just (Telemetry.parseSeverity -> Just (_, sl))
          | sl >= Telemetry.SLError -> Just "ERROR"
          | sl == Telemetry.SLWarn -> Just "WARN"
        other
          | any (`T.isInfixOf` pat) ["status;badge-error⇒ERROR", "status_code;badge-4xx", "status_code;badge-5xx"] -> Just "ERROR"
          | otherwise -> T.toUpper <$> other
      (tone, icon, label) = case effective of
        Just "ERROR" -> (ErrorBadge, "triangle-alert", "ERROR")
        Just "WARN" -> (WarningBadge, "triangle-alert", "WARN")
        Just l -> (NeutralBadge, "circle-dot", l)
        Nothing -> (NeutralBadge, "circle-dot", "Unknown")
   in colorChip_ tone icon label


data IssueActivityFeed = IssueActivityFeed
  { projectId :: Projects.ProjectId
  , issueId :: Issues.IssueId
  , now :: UTCTime
  , activities :: [Issues.IssueActivity]
  , users :: Map.Map Projects.UserId Projects.User
  , journey :: V.Vector Telemetry.SpanRecord
  }


instance ToHtml IssueActivityFeed where
  toHtml f = toHtml feed
    where
      -- Issue events first: it is the short, high-signal section (often a single
      -- "Created" row) and was buried under a journey scrollbox. The journey renders
      -- last at natural height so the page scroll carries it instead of a nested box.
      feed :: Html ()
      feed = do
        form_ [class_ "mx-4 mb-3 p-2 rounded-lg border border-strokeWeak bg-fillWeaker/40 flex flex-col gap-2 focus-within:border-strokeBrand-strong", hxPost_ $ (urls.issues f.projectId).commentPost f.issueId, hxSwap_ "none"] do
          textarea_ [name_ "body", required_ "", rows_ "2", placeholder_ "Add a comment\x2026", Aria.label_ "Comment", class_ "textarea textarea-ghost textarea-sm w-full min-h-16 border-0 bg-transparent shadow-none focus:outline-none text-base sm:text-sm"] ""
          button_ [type_ "submit", class_ "btn btn-xs btn-ghost bg-fillWeak border-0 shadow-none self-end"] "Comment"
        unless (null links) $ div_ [class_ "px-4 pb-3 space-y-1"] do
          h4_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "External links"
          forM_ links \(url, title) -> a_ [href_ url, target_ "_blank", rel_ "noopener", class_ "flex items-center gap-1.5 text-sm text-textBrand hover:underline truncate"] do
            faSprite_ "link" "regular" "w-3 h-3 shrink-0"
            span_ [class_ "text-xs text-textWeak shrink-0"] $ toHtml $ linkProvider url
            span_ [class_ "truncate"] $ toHtml title
        details_ [class_ "px-4 pb-3"] do
          summary_ [class_ "text-xs text-textBrand cursor-pointer list-none [&::-webkit-details-marker]:hidden"] "+ Link issue"
          form_ [class_ "mt-2 flex flex-col gap-2", hxPost_ $ (urls.issues f.projectId).linkPost f.issueId, hxSwap_ "none"] do
            input_ [type_ "url", name_ "url", required_ "", placeholder_ "https://github.com/org/repo/issues/42", Aria.label_ "Issue URL", class_ "input input-sm w-full"]
            input_ [type_ "text", name_ "title", placeholder_ "Title (optional)", Aria.label_ "Link title", class_ "input input-sm w-full"]
            button_ [type_ "submit", class_ "btn btn-xs self-end"] "Link"
        unless (null participants && null viewers) $ div_ [class_ "px-4 pb-3 space-y-1 text-xs leading-relaxed text-textWeak break-words"] do
          h4_ [class_ "text-2xs font-semibold uppercase tracking-wide"] "People"
          details_ [class_ "group/people"] do
            summary_ [class_ "flex items-center gap-2 cursor-pointer list-none [&::-webkit-details-marker]:hidden rounded focus-visible:outline-2 focus-visible:outline-offset-2"] do
              span_ [class_ "flex -space-x-1"] $ forM_ (take 5 $ participants <> viewers) \uid ->
                img_ [src_ $ "/api/avatar/" <> uid.toText, alt_ $ name uid, title_ $ name uid, loading_ "lazy", width_ "24", height_ "24", class_ "h-6 w-6 rounded-full bg-fillWeak outline outline-1 outline-black/10 dark:outline-white/10"]
              span_ $ toHtml $ let n = length participants + length viewers in show n <> bool " people" " person" (n == 1)
              faSprite_ "chevron-down" "regular" "w-3 h-3 group-open/people:rotate-180"
            div_ [class_ "pt-2 space-y-1"] do
              unless (null participants) $ p_ $ toHtml $ "Participating: " <> T.intercalate ", " (name <$> participants)
              unless (null viewers) $ p_ $ toHtml $ "Viewed: " <> T.intercalate ", " (name <$> viewers)
        div_ [class_ "border-t border-strokeWeak"] do
          div_ [class_ "px-4 py-2 flex items-center gap-2 bg-fillWeaker/40"] do
            faSprite_ "circle-info" "regular" "w-3 h-3 text-textWeak"
            fieldLabel_ "" "Timeline"
        issueActivityTimeline_ f.users f.now timeline
        userJourneySection_ f.journey
      isView a = a.event == Issues.Lifecycle Issues.IEViewed
      (views, timeline) = partition isView f.activities
      name uid = maybe "Someone" (\u -> bool (u.firstName <> " " <> u.lastName) (CI.original u.email) (T.null u.firstName)) (Map.lookup uid f.users)
      participants = ordNub $ mapMaybe (.createdBy) timeline
      viewers = filter (`notElem` participants) $ ordNub $ mapMaybe (.createdBy) views
      links = [(url, fromMaybe url (lookupValueText m "title")) | a <- timeline, a.event == Issues.Lifecycle Issues.IELinked, Just m <- [a.metadata], Just url <- [lookupValueText m "url"]]
  toHtmlRaw = toHtml


issueActivityGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe UTCTime -> ATAuthCtx (RespHeaders IssueActivityFeed)
issueActivityGetH projectId issueId traceIdM traceTsM = do
  (_sess, _project) <- Projects.sessionAndProject projectId
  activities <- Issues.selectIssueActivity projectId issueId
  now <- Time.currentTime
  env <- (.env) <$> ask @AuthContext
  -- The journey comes out of the issue's trace, the one read on this page big
  -- enough to time out; it is bounded here so a slow trace costs the journey, not
  -- the issue-events timeline beside it.
  journey <- flip foldMapM ((,) <$> traceIdM <*> traceTsM) \(tId, tTs) ->
    foldMap (\(_, spans, _) -> spans)
      . join
      <$> tryWithin
        (Just $ env.traceViewTimeoutSecs * 1_000_000)
        "ISSUE_JOURNEY_FETCH"
        ["issue_id" AE..= issueId, "trace_id" AE..= tId]
        (Telemetry.getTraceDetailsForView env.enableTimefusionReads projectId tId (Just tTs) now Nothing)
  users <- Projects.usersByIds (V.fromList $ ordNub $ mapMaybe (.createdBy) activities)
  addRespHeaders IssueActivityFeed{projectId, issueId, now, activities, users = Map.fromList $ map (\u -> (u.id, u)) users, journey}


issueActivityTimeline_ :: Map.Map Projects.UserId Projects.User -> UTCTime -> [Issues.IssueActivity] -> Html ()
issueActivityTimeline_ userMap now activities
  | null activities = emptyState_ def{size = ESCompact} "No activity yet." ""
  | otherwise = div_ [class_ "p-4 flex flex-col gap-0"] $ forM_ activities \a -> do
      let (icon, color, label) = eventDisplay a.event
          actorText = foldMap (\uid -> foldMap (\u -> " by " <> CI.original u.email) $ Map.lookup uid userMap) a.createdBy
      div_ [class_ "flex items-start gap-3 relative pl-4 pb-4 last:pb-0 border-l border-strokeWeak ml-2"] do
        div_ [class_ $ "absolute -left-[9px] top-0.5 w-4 h-4 rounded-full flex items-center justify-center " <> color]
          $ faSprite_ icon "regular" "w-2.5 h-2.5"
        div_ [class_ "flex flex-col gap-0.5 min-w-0"] do
          span_ [class_ "text-sm text-textStrong"] $ toHtml $ label <> actorText
          whenJust (a.metadata >>= (`lookupValueText` "body")) $ p_ [class_ "text-sm text-textStrong whitespace-pre-wrap break-words bg-fillWeaker rounded px-2 py-1"] . toHtml
          span_ [class_ "text-xs text-textWeak"] $ toHtml $ agoText now a.createdAt
  where
    -- Icon, badge colour, and label for one timeline row. Episode rows read as the
    -- alert a reader saw in Slack — "Alerted", not "alert".
    eventDisplay :: Issues.ActivityEvent -> (Text, Text, Text)
    eventDisplay = \case
      Issues.Episode Issues.EKAlert -> ("bell", "bg-fillError-weak text-fillError-strong", "Alerted")
      Issues.Episode Issues.EKObservation -> ("eye", "bg-fillWarning-weak text-fillWarning-strong", "Still firing")
      Issues.Episode Issues.EKReminder -> ("bell-on", "bg-fillWarning-weak text-fillWarning-strong", "Reminder sent")
      Issues.Episode Issues.EKDataUnavailable -> ("plug-circle-exclamation", "bg-fillWeaker text-textWeak", "No data to evaluate")
      Issues.Episode Issues.EKRecovered -> ("heart-pulse", "bg-fillSuccess-weak text-fillSuccess-strong", "Recovered")
      Issues.Episode Issues.EKResolved -> ("check-double", "bg-fillSuccess-weak text-fillSuccess-strong", "Resolved")
      Issues.Lifecycle Issues.IECreated -> ("plus", "bg-fillSuccess-weak text-fillSuccess-strong", "Created")
      Issues.Lifecycle Issues.IEAcknowledged -> ("bell-slash", "bg-fillBrand-weak text-fillBrand-strong", "Acknowledged")
      Issues.Lifecycle Issues.IEUnacknowledged -> ("arrow-rotate-left", "bg-fillWeaker text-textWeak", "Unacknowledged")
      Issues.Lifecycle Issues.IEAckExpired -> ("clock", "bg-fillWarning-weak text-fillWarning-strong", "Acknowledgement expired \x2014 back in the Inbox")
      Issues.Lifecycle Issues.IEArchived -> ("box-archive", "bg-fillWeaker text-textWeak", "Archived")
      Issues.Lifecycle Issues.IEUnarchived -> ("box-archive", "bg-fillWeaker text-textWeak", "Unarchived")
      Issues.Lifecycle Issues.IEResolved -> ("check-double", "bg-fillSuccess-weak text-fillSuccess-strong", "Resolved")
      Issues.Lifecycle Issues.IEReopened -> ("arrow-rotate-left", "bg-fillWarning-weak text-fillWarning-strong", "Reopened")
      Issues.Lifecycle Issues.IERegressed -> ("arrow-trend-up", "bg-fillError-weak text-fillError-strong", "Regressed")
      Issues.Lifecycle Issues.IEAssigned -> ("user-plus", "bg-fillBrand-weak text-fillBrand-strong", "Assigned")
      Issues.Lifecycle Issues.IEUnassigned -> ("user-minus", "bg-fillWeaker text-textWeak", "Unassigned")
      Issues.Lifecycle Issues.IEAutoResolved -> ("wand-magic-sparkles", "bg-fillSuccess-weak text-fillSuccess-strong", "Auto-resolved")
      Issues.Lifecycle Issues.IEEscalated -> ("arrow-up", "bg-fillError-weak text-fillError-strong", "Escalated")
      Issues.Lifecycle Issues.IECommented -> ("comment", "bg-fillBrand-weak text-fillBrand-strong", "Commented")
      Issues.Lifecycle Issues.IEViewed -> ("eye", "bg-fillWeaker text-textWeak", "Viewed")
      Issues.Lifecycle Issues.IELinked -> ("link", "bg-fillBrand-weak text-fillBrand-strong", "Linked an external issue")
      Issues.Lifecycle Issues.IEMerged -> ("code-merge", "bg-fillWeaker text-textWeak", "Merged into another issue")
      Issues.Lifecycle Issues.IEUnmerged -> ("arrow-rotate-left", "bg-fillWeaker text-textWeak", "Unmerged")
      Issues.Lifecycle Issues.IESpam -> ("ban", "bg-fillWeaker text-textWeak", "Marked as spam")
      Issues.Lifecycle Issues.IENotSpam -> ("arrow-rotate-left", "bg-fillWeaker text-textWeak", "Marked as not spam")


data SaveViewForm = SaveViewForm {name :: Text, query :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data UnnamedView = UnnamedView
  deriving stock (Eq, Generic, Show)
  deriving (Refusal) via Says "Name the view" UnnamedView


-- | A saved view changed; the browser goes to the issue list, filtered by a saved view's query.
data ViewChanged = ViewSaved Projects.ProjectId Text | ViewDeleted Projects.ProjectId


instance Outcome ViewChanged where
  outcomeRedirect =
    Just . \case
      ViewSaved pid query -> issuesListUrl pid <> "?" <> query
      ViewDeleted pid -> issuesListUrl pid


saveViewPostH :: Projects.ProjectId -> SaveViewForm -> ATAuthCtx (ActionResult UnnamedView ViewChanged)
saveViewPostH pid form = act do
  (sess, _) <- Projects.sessionAndProject pid
  let name = T.strip form.name
  if T.null name
    then pure $ Left UnnamedView
    else Right (ViewSaved pid form.query) <$ Issues.saveIssueView pid sess.user.id name form.query


deleteViewPostH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Done ViewChanged))
deleteViewPostH pid vid = do
  _ <- Projects.sessionAndProject pid
  void $ Issues.deleteIssueView pid vid
  done $ ViewDeleted pid


-- | The tracker an external issue URL points at, by host.
--
-- >>> map linkProvider ["https://github.com/o/r/issues/1", "https://acme.atlassian.net/browse/X-1", "https://linear.app/a/issue/B-2", "https://x.io/1"]
-- ["GitHub","Jira","Linear","Link"]
linkProvider :: Text -> Text
linkProvider url = maybe "Link" snd $ find ((`T.isInfixOf` url) . fst) ([("github.com", "GitHub"), ("gitlab.", "GitLab"), ("atlassian.net", "Jira"), ("linear.app", "Linear")] :: [(Text, Text)])


newtype CommentForm = CommentForm {body :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data LinkForm = LinkForm {url :: Text, title :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


data TimelineRejection = TimelineIssueMissing | EmptyComment | NotAnHttpLink
  deriving stock (Eq, Show)


instance Refusal TimelineRejection where
  refusalMessage = \case
    TimelineIssueMissing -> "Issue not found"
    EmptyComment -> "Write a comment first"
    NotAnHttpLink -> "Enter an http(s) link to the external issue"


-- | An entry landed on the issue's timeline.
data TimelineLogged = TimelineLogged
  deriving anyclass (Outcome)


-- | Log that the user opened the issue (at most once a day), for the People list.
issueViewedPostH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders (Done TimelineLogged))
issueViewedPostH pid issueId = do
  (sess, _) <- Projects.sessionAndProject pid
  Issues.selectIssueById pid issueId >>= traverse_ \_ -> Issues.recordIssueView issueId sess.user.id
  done TimelineLogged


-- | A comment on the issue's timeline.
commentPostH :: Projects.ProjectId -> Issues.IssueId -> CommentForm -> ATAuthCtx (ActionResult TimelineRejection TimelineLogged)
commentPostH pid issueId form = logTimelineEntry pid issueId $ maybeToRight EmptyComment $ guarded (not . T.null) (T.strip form.body) <&> \body -> (Issues.IECommented, ["body" AE..= body])


-- | Attach an external issue (GitHub, Jira, Linear, ...) by URL.
linkPostH :: Projects.ProjectId -> Issues.IssueId -> LinkForm -> ATAuthCtx (ActionResult TimelineRejection TimelineLogged)
linkPostH pid issueId form =
  logTimelineEntry pid issueId
    $ maybeToRight NotAnHttpLink
    $ guarded (\u -> any (`T.isPrefixOf` u) ["https://", "http://"]) (T.strip form.url)
    <&> \url -> (Issues.IELinked, ["url" AE..= url, "title" AE..= mfilter (not . T.null) (T.strip <$> form.title)])


-- | Log a user's timeline entry on an existing issue.
logTimelineEntry :: Projects.ProjectId -> Issues.IssueId -> Either TimelineRejection (Issues.IssueEvent, [(AE.Key, AE.Value)]) -> ATAuthCtx (ActionResult TimelineRejection TimelineLogged)
logTimelineEntry pid issueId entry = act do
  (sess, _) <- Projects.sessionAndProject pid
  Issues.selectIssueById pid issueId >>= \case
    Nothing -> pure $ Left TimelineIssueMissing
    Just _ -> forM entry \(ev, meta) -> TimelineLogged <$ (Issues.logIssueActivity issueId ev (Just sess.user.id) (Just $ AE.object meta) >> addTriggerEvent "issueActivityChanged" AE.Null)


data ErrorGroupMembers = ErrorGroupMembers {projectId :: Projects.ProjectId, members :: [ErrorPatterns.ErrorPattern]}


instance ToHtml ErrorGroupMembers where
  toHtml g =
    toHtml
      $ unless (null g.members)
      $ railSection_ ("Merged patterns · " <> show (length g.members))
      $ div_ [class_ "flex flex-col gap-2"] do
        forM_ g.members \member -> do
          let memberId = UUID.toText member.id.unErrorPatternId
              unmergeUrl = (urls.issues g.projectId).errorUnmergePost member.id.unErrorPatternId
          div_ [class_ "p-3 bg-fillWeaker rounded-lg space-y-2", id_ $ "member-" <> memberId] do
            div_ [class_ "flex items-center justify-between gap-2"] do
              span_ [class_ "text-xs font-medium text-textWeak min-w-0 break-words"] $ toHtml member.errorType
              button_
                [ type_ "button"
                , class_ "btn btn-xs btn-ghost tap-target shrink-0"
                , Aria.label_ $ "Unmerge pattern " <> member.hash
                , hxPost_ unmergeUrl
                , hxSwap_ "none"
                ]
                do
                  faSprite_ "code-branch" "regular" "w-3 h-3"
                  "Unmerge"
            details_ [class_ "group/pattern"] do
              summary_ [class_ "cursor-pointer list-none [&::-webkit-details-marker]:hidden rounded focus-visible:outline-2 focus-visible:outline-offset-2"] do
                span_ [class_ "text-sm text-textStrong break-normal [overflow-wrap:anywhere] line-clamp-2 group-open/pattern:hidden"] $ toHtml member.message
                span_ [class_ "block mt-1 text-xs text-textBrand"] do
                  span_ [class_ "group-open/pattern:hidden"] "Read full message"
                  span_ [class_ "hidden group-open/pattern:inline"] "Show less"
              p_ [class_ "mt-2 text-sm text-textStrong whitespace-pre-wrap break-normal [overflow-wrap:anywhere]"] $ toHtml member.message
            span_ [class_ "block font-mono text-2xs text-textWeak"] $ toHtml $ "Hash: " <> member.hash
  toHtmlRaw = toHtml


errorGroupMembersGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders ErrorGroupMembers)
errorGroupMembersGetH projectId errorId = do
  _ <- Projects.sessionAndProject projectId
  members <- PatternMerge.getPatternGroupMembers projectId (ErrorPatternId errorId)
  addRespHeaders ErrorGroupMembers{projectId, members}


data PatternUnmerged = PatternUnmerged
  deriving stock (Generic)
  deriving (Outcome) via Says "Pattern unmerged" PatternUnmerged


errorUnmergePostH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Done PatternUnmerged))
errorUnmergePostH pid errorId = do
  _ <- Projects.sessionAndProject pid
  void $ PatternMerge.unmergePattern pid (ErrorPatternId errorId)
  addTriggerEvent "issuePatternGroupChanged" AE.Null
  done PatternUnmerged
