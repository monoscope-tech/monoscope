module Pages.Issues (
  issueListGetH,
  issueBulkActionsPostH,
  IssueBulkAction (..),
  acknowledgeIssueGetH,
  archiveIssueGetH,
  resolveIssueGetH,
  TriageForm (..),
  memberLabel,
  triagePostH,
  issueDetailGetH,
  IssueBulkForm (..),
  IssueListGet (..),
  issueAcknowledgeButton,
  issueArchiveButton,
  issueDetailHashGetH,
  IssueAction (..),
  IssueVM (..),
  AssignErrorForm (..),
  assignErrorPostH,
  resolveErrorPostH,
  ErrorSubscriptionForm (..),
  errorSubscriptionPostH,
  -- AI Chat
  AIChatForm (..),
  aiChatPostH,
  aiChatHistoryGetH,
  -- Activity
  issueActivityGetH,
  issueSampleGetH,
  -- Pattern group members
  errorGroupMembersGetH,
  errorUnmergePostH,
  -- Shared rendering helpers
  issueCardCompact_,
  aiChatHistoryView_,
)
where

import BackgroundJobs qualified
import Control.Exception qualified as Exception
import Data.Aeson qualified as AE
import Data.Aeson.Types (Pair, Parser, parseMaybe)
import Data.CaseInsensitive qualified as CI
import Data.Char (isHexDigit)
import Data.Default (Default, def)
import Data.Effectful.Hasql qualified as Hasql
import Data.HashMap.Strict qualified as HM
import Data.Map qualified as Map
import Data.Ord (clamp)
import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Display (display)
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, diffUTCTime, formatTime)
import Data.Time.Clock.POSIX qualified as POSIX
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.Newtypes (Aeson (..), getAeson)
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Deriving.Aeson qualified as DAE
import Effectful.Concurrent.Async (concurrently)
import Effectful.Exception (trySync)
import Effectful.Reader.Static (ask)
import Effectful.Time qualified as Time
import Effectful.Timeout (timeout)
import Hasql.Interpolate qualified as HI
import Lucid
import Lucid.Aria qualified as Aria
import Lucid.Base (TermRaw (termRaw))
import Lucid.Htmx (hxGet_, hxIndicator_, hxPost_, hxSwap_, hxTarget_, hxTrigger_, hxVals_)
import Lucid.Hyperscript (__)
import Models.Apis.ErrorPatterns (ErrorPatternId (..))
import Models.Apis.ErrorPatterns qualified as ErrorPatterns
import Models.Apis.Incidents qualified as Incidents
import Models.Apis.Issues qualified as Issues
import Models.Apis.LogPatterns (sourceFieldLabel)
import Models.Apis.Monitors qualified as Monitors
import Models.Apis.PatternMerge qualified as PatternMerge
import Models.Apis.SchemaCatalog qualified as SchemaCatalog
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects (User (id))
import Models.Projects.Projects qualified as Projects
import Models.Telemetry.Schema qualified as Schema
import Models.Telemetry.Telemetry qualified as Telemetry
import OddJobs.Job (createJob)
import Pages.BodyWrapper (BWConfig (..), PageCtx (..), mkPageCtx, navTabAttrs)
import Pages.Charts.Charts qualified as Charts
import Pages.Components (EmptyStateAction (..), EmptyStateCfg (..), EmptyStateSize (..), agoText, colorChip_, copyButton_, detailsClosedBelowAttr_, durationMenu_, durationQuery, emptyState_, filterInputAttr_, metadataChip_, periodToggle_, resizer_, sectionLabel_, sparkline_, untilLabel)
import Pages.LogExplorer.Log (virtualTable)
import Pages.LogExplorer.LogItem qualified as LogItem
import Pages.Telemetry (traceFragmentUrl)
import Pkg.AI qualified as AI
import Pkg.Components.Table (BulkAction (..), Column (..), Config (..), Features (..), Pagination (..), SearchMode (..), TabFilter (..), TabFilterOpt (..), Table (..), TableHeaderActions (..), TableRows (..), ZeroState (..), col, multiSelectFilter, withAttrs, withColHeaderExtra)
import Pkg.Components.TimePicker qualified as TimePicker
import Pkg.Components.Widget qualified as Widget
import Pkg.DeriveUtils (UUIDId (..), WrappedEnumSC (..), assetUrl, bulkActionSlug)
import Pkg.ErrorFingerprint qualified as EF
import Pkg.Mail qualified as Mail
import Pkg.Parser (ScopedQuery (..), applyScopedKqlContext, mkScopedQuery)
import Pkg.SchemaLearning.Catalog (FacetData (..), FacetSummary (..), FacetValue (..))
import PyF (fmt)
import Relude hiding (ask)
import System.Config (AuthContext (..), EnvConfig (..))
import System.IO.Error (userError)
import System.Logging qualified as Log
import System.Types (ATAuthCtx, RespHeaders, addErrorToast, addRespHeaders, addSuccessToast, addTriggerEvent, useTfReads)
import Utils (LoadingSize (..), LoadingType (..), checkFreeTierStatus, countNoun, faSprite_, formatOffset, formatUTC, formatWithCommas, hostPath, isoT, loadingIndicator_, lookupValueText, renderMarkdown, timeScopedUrl, toUriStr)
import Web.FormUrlEncoded (FromForm)
import Web.HttpApiData (FromHttpApiData (..))


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


-- | Archive (on=True) or un-archive (on=False) an issue.
-- | Archive (Just window) or un-archive (Nothing) an issue. A timed or "until
-- escalating" archive lifts itself; see 'Issues.ArchiveWindow'.
archiveIssueGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Issues.ArchiveWindow -> ATAuthCtx (RespHeaders IssueAction)
archiveIssueGetH pid issueId windowM = do
  (sess, _) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  void $ Issues.setArchiveState pid [issueId] ((now,) <$> windowM)
  Issues.logIssueActivity issueId (bool Issues.IEUnarchived Issues.IEArchived (isJust windowM)) (Just sess.user.id) Nothing
  addSuccessToast (maybe "Restored to the Inbox" archivedToast windowM) Nothing
  addTriggerEvent "issuesListChanged" AE.Null
  addRespHeaders $ Archive pid issueId (isJust windowM)
  where
    archivedToast = \case
      Issues.ArchiveIndefinite -> "Archived \x2014 notifications stopped"
      Issues.ArchiveFor mins -> "Archived for " <> show mins <> " min \x2014 it returns to the Inbox after that"
      Issues.ArchiveUntilEscalating -> "Archived until the error escalates"


-- | Resolve an issue that has no error pattern behind it: the fix is done, so it
-- leaves the Inbox, and the timeline says resolved rather than archived.
resolveIssueGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders IssueAction)
resolveIssueGetH pid issueId = do
  (sess, _) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  void $ Issues.setArchiveState pid [issueId] (Just (now, Issues.ArchiveIndefinite))
  Issues.logIssueActivity issueId Issues.IEResolved (Just sess.user.id) Nothing
  addSuccessToast "Resolved" Nothing
  addTriggerEvent "issuesListChanged" AE.Null
  addRespHeaders $ Archive pid issueId True


data TriageForm = TriageForm {severity :: Maybe Issues.IssueSeverity, assigneeId :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | Priority and assignee, set from the issue header on any issue type. A runtime
-- exception's assignee is mirrored onto its error pattern, whose assignment email
-- and resolve permission read it.
triagePostH :: Projects.ProjectId -> Issues.IssueId -> TriageForm -> ATAuthCtx (RespHeaders (Html ()))
triagePostH pid issueId form = do
  (sess, _) <- Projects.sessionAndProject pid
  now <- Time.currentTime
  issueM <- Issues.selectIssueById pid issueId
  members <- ProjectMembers.selectActiveProjectMembers pid
  let assigneeM = form.assigneeId >>= UUID.fromText <&> Projects.UserId
  case issueM of
    Nothing -> addErrorToast "Issue not found" Nothing >> addRespHeaders mempty
    Just issue -> do
      whenJust form.severity $ void . Issues.setIssuePriority pid [issueId]
      when (assigneeM /= issue.assigneeId) do
        void $ Issues.setIssueAssignee pid [issueId] assigneeM
        Issues.logIssueActivity issueId (maybe Issues.IEUnassigned (const Issues.IEAssigned) assigneeM) (Just sess.user.id) Nothing
        when (issue.issueType == Issues.RuntimeException)
          $ ErrorPatterns.getErrorPatternByHash pid issue.targetHash
          >>= traverse_ \err -> ErrorPatterns.setErrorPatternAssignee err.id assigneeM now
      addTriggerEvent "issuesListChanged" AE.Null
      addRespHeaders $ issueTriage_ pid issue{Issues.severity = fromMaybe issue.severity form.severity, Issues.assigneeId = assigneeM} members


-- | The header's Priority and Assignee selects; one form, re-rendered on change.
issueTriage_ :: Projects.ProjectId -> Issues.Issue -> [ProjectMembers.ProjectMemberVM] -> Html ()
issueTriage_ pid issue members =
  form_ [id_ "issue-triage", class_ "ml-auto flex items-center gap-3 text-xs text-textWeak", hxPost_ $ "/p/" <> pid.toText <> "/issues/" <> issue.id.toText <> "/triage", hxTrigger_ "change", hxSwap_ "outerHTML"] do
    label_ [class_ "flex items-center gap-1.5"] do
      "Priority"
      select_ [class_ "select select-sm w-28", name_ "severity", Aria.label_ "Priority"]
        $ forM_ [minBound .. maxBound :: Issues.IssueSeverity] \sev -> option_ ([value_ (display sev)] <> [selected_ "true" | sev == issue.severity]) $ toHtml (T.toTitle $ display sev)
    label_ [class_ "flex items-center gap-1.5"] do
      "Assignee"
      select_ [class_ "select select-sm w-44", name_ "assigneeId", Aria.label_ "Assignee"] do
        option_ ([value_ ""] <> [selected_ "true" | isNothing issue.assigneeId]) "Unassigned"
        forM_ members \m -> option_ ([value_ m.userId.toText] <> [selected_ "true" | issue.assigneeId == Just m.userId]) $ toHtml $ memberLabel m


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


-- | Bulk lifecycle transitions, triggering a toast and list reload. @duration@
-- (minutes) applies to @acknowledge@ only; absent acknowledges indefinitely.
-- The slugs are the existing wire spellings, so live URLs are unchanged:
--
-- >>> map bulkActionSlug [minBound .. maxBound :: IssueBulkAction]
-- ["acknowledge","unacknowledge","archive","unarchive","resolve","priority","assign"]
data IssueBulkAction = BAAcknowledge | BAUnacknowledge | BAArchive | BAUnarchive | BAResolve | BAPriority | BAAssign
  deriving stock (Bounded, Enum, Eq, Generic, Read, Show)
  deriving (FromHttpApiData) via WrappedEnumSC 'Nothing "BA" IssueBulkAction


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
          issues <- catMaybes <$> traverse (Issues.selectIssueById pid) issueIds
          forM_ issues \i ->
            when (i.issueType == Issues.RuntimeException)
              $ ErrorPatterns.getErrorPatternByHash pid i.targetHash
              >>= traverse_ \err -> void $ resolveErrorAs sess.user err now
          void $ Issues.setArchiveState pid issueIds (Just (now, Issues.ArchiveIndefinite))
          pure (issueIds, Just Issues.IEResolved, "Resolved")
        BAPriority -> case parseQueryParam @Issues.IssueSeverity =<< maybeToRight "" valueM of
          Right sev -> Issues.setIssuePriority pid issueIds sev $> ([], Nothing, "Priority set to " <> display sev)
          Left _ -> pure ([], Nothing, "Pick a priority")
        BAAssign -> do
          let assigneeM = valueM >>= UUID.fromText <&> Projects.UserId
          void $ Issues.setIssueAssignee pid issueIds assigneeM
          pure (issueIds, Just (maybe Issues.IEUnassigned (const Issues.IEAssigned) assigneeM), maybe "Unassigned" (const "Assigned") assigneeM)
      forM_ eventM \ev -> forM_ logIds \u -> Issues.logIssueActivity u ev (Just sess.user.id) Nothing
      addSuccessToast msg Nothing
      addTriggerEvent "issuesListChanged" AE.Null
      addRespHeaders Bulk


issueDetailGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
issueDetailGetH pid issueId firstM sinceM fromM toM = issueDetailCore pid firstM (TimePicker.TimePicker sinceM fromM toM) $ Issues.selectIssueById pid issueId


issueDetailHashGetH :: Projects.ProjectId -> Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
issueDetailHashGetH pid issueId firstM sinceM fromM toM = issueDetailCore pid firstM (TimePicker.TimePicker sinceM fromM toM) $ Issues.selectIssueByHash pid issueId Issues.AnyIssue


issueDetailCore :: Projects.ProjectId -> Maybe Text -> TimePicker.TimePicker -> ATAuthCtx (Maybe Issues.Issue) -> ATAuthCtx (RespHeaders (PageCtx (Html ())))
issueDetailCore pid firstM requestedRange fetchIssue = do
  (sess, project, bw) <- mkPageCtx pid
  issueM <- fetchIssue
  now <- Time.currentTime
  useTf <- useTfReads
  let baseBwconf = bw{pageTitle = "Issues", menuItem = Just "Issues"}
  case issueM of
    Nothing ->
      addRespHeaders
        $ PageCtx baseBwconf{pageTitle = "Issue Not Found"}
        $ emptyState_
          def{icon = Just "circle-xmark", action = ESLink ("/p/" <> pid.toText <> "/issues") "Back to Issues"}
          "Issue not found"
          "This issue may have been resolved, merged, or the link may be outdated."
    Just issue -> do
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
      let
        -- +/-2h: the widest bracket that stays inside ~3s (see the table above).
        bracketAround t = TimePicker.TimePicker Nothing (Just $ isoT $ addUTCTime (-7200) t) (Just $ isoT $ addUTCTime 7200 t)
        lastActive = zonedTimeToUTC issue.updatedAt
        -- ~2x the issue's age so the data fills the chart, capped at 24H by the cost above.
        defaultSinceRange createdAt
          | ageH < 1 = "1H" :: Text
          | ageH < 3 = "3H"
          | ageH < 6 = "6H"
          | otherwise = "24H"
          where
            ageH = diffUTCTime now (zonedTimeToUTC createdAt) / 3600
        -- The URL's range drives the picker, sample lookup and log links together.
        -- Previously only widgets saw absolute dates; the server silently used defaults.
        tp = case Issues.issuePayload issue of
          _ | any (maybe False (not . T.null)) [requestedRange.since, requestedRange.from, requestedRange.to] -> requestedRange
          Just (Issues.QueryAlertP d) -> bracketAround d.triggeredAt
          _ | diffUTCTime now lastActive > 43200 -> bracketAround lastActive
          _ -> TimePicker.TimePicker (Just $ defaultSinceRange issue.createdAt) Nothing Nothing
      stateEvent <- enriching "latest_state_event" $ Issues.selectLatestStateEvent issue.id
      errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternLByHash pid issue.targetHash now) (issue.issueType == Issues.RuntimeException)
      canResolve <- case errorM of
        Nothing -> pure False
        Just errL -> do
          userPermission <- ProjectMembers.getUserPermission pid sess.user.id
          pure $ userPermission >= Just ProjectMembers.PEdit || errL.base.assigneeId == Just sess.user.id
      members <- ProjectMembers.selectActiveProjectMembers pid
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
      mTraceRef <- case Issues.issuePayload issue of
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
        Just (Issues.ApiChangeP d) -> Telemetry.getEndpointTraceId pid d.endpointMethod d.endpointPath isFirst now
        Just (Issues.QueryAlertP _) -> pure Nothing
        Just (Issues.LogPatternP _) -> pure Nothing
        Just (Issues.LogPatternRateChangeP _) -> pure Nothing
        Nothing -> pure Nothing
      -- The trace is supporting evidence, not the page. It used to be fetched here
      -- and a cold read of a multi-thousand-span trace took >56s, so the gateway
      -- 504'd the whole issue. The Trace section now pulls it as its own
      -- HTMX fragment, and the only thing this page still needs from the trace is
      -- the session id for the replay section — one scalar, not 1300 rows.
      tracedSession <- enriching "replay_session_id" $ flip foldMapM mTraceRef \(tId, tTs) ->
        Hasql.withHasqlTimefusion useTf
          $ listToMaybe @Text
          <$> Hasql.interp
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
      replaySession <- enriching "replay_recording" $ flip foldMapM (UUID.fromText =<< tracedSession) \sid ->
        listToMaybe @Text
          <$> Hasql.interp [HI.sql| SELECT session_id::text FROM projects.replay_sessions WHERE project_id = #{pid} AND session_id = #{sid} LIMIT 1 |]
      addRespHeaders
        $ PageCtx bwconf
        $ issueDetailPage
          IssueView{pid = pid, issue = issue, traceRef = mTraceRef, replaySession = replaySession, errM = errorM, now = now, isFirst = isFirst, tp = tp, stateEvent = stateEvent, canResolve = canResolve, members = members}


-- The snapshot is available immediately; telemetry never holds up the page shell.
data IssueSampleResult
  = SampleLoading
  | SampleUnavailable
  | SampleEmpty
  | SampleFound UTCTime (V.Vector Text) (Maybe Text)


issueSampleGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe Text -> Maybe Text -> ATAuthCtx (RespHeaders (Html ()))
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
          patHash = "pat:" <> issue.targetHash
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
      addRespHeaders $ issueSampleCard_ pid snapshot sample
    _ -> addRespHeaders $ p_ [class_ "px-4 text-sm text-textWeak"] "This issue has no event sample."


issueSampleCard_ :: Projects.ProjectId -> Maybe Text -> IssueSampleResult -> Html ()
issueSampleCard_ pid snapshot result = case result of
  SampleFound at summary sampleTraceId -> do
    div_ [class_ "px-4 pt-3 flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-textWeak"] do
      span_ "Latest event in range"
      time_ [datetime_ $ formatUTC at] $ toHtml $ formatTime defaultTimeLocale "%F %T UTC" at
      whenJust (mfilter (not . T.null) sampleTraceId) \tid ->
        a_ [href_ $ "/p/" <> pid.toText <> "/traces/" <> toUriStr tid <> "?timestamp=" <> toUriStr (formatUTC at), class_ "text-textBrand underline underline-offset-2"] "View trace"
    div_ [class_ "flex flex-wrap items-center gap-1 p-4 max-h-80 overflow-y-auto"] $ V.mapM_ (summaryToken_ True) summary
  SampleLoading -> withSnapshot "Looking for an event in the selected range…"
  SampleEmpty -> withSnapshot "No matching event in the selected range. Try another range or inspect the logs below."
  SampleUnavailable -> withSnapshot do
    "The event sample could not be loaded. "
    button_ [type_ "button", class_ "text-textBrand underline underline-offset-2", [__|on click send retryIssueSample to #issue-sample|]] "Retry"
  where
    withSnapshot message = do
      div_ [class_ "px-4 py-3 text-sm text-textWeak", role_ "status"] message
      whenJust (mfilter (not . T.null . T.strip) snapshot) \stored -> do
        div_ [class_ "px-4 pt-2 border-t border-strokeWeak text-xs text-textWeak"] "Stored sample · saved with this issue, outside the range filter"
        renderLogContent_ stored


-- | A stack trace rendered the way Sentry renders one: frames split into the code you
-- wrote and the code you did not, with the runtime\'s own frames folded away behind a
-- count until asked for.
--
-- The parser already existed and this page never used it. @parseStackTrace@ classifies
-- frames per language and marks @isInApp@, and it is what @computeErrorHashes@ is
-- computed from — so the frames shown here are the same ones the issue is keyed on,
-- not a second opinion about the text.
--
-- Falls back to the raw block when the parser recognises nothing, and keeps the
-- original text one disclosure away regardless: a reader chasing a stack trace should
-- never be forced to trust our parse of it.
stackTrace_ :: Projects.ProjectId -> Maybe Text -> Maybe Text -> Text -> Html ()
stackTrace_ pid serviceM runtimeM raw = case EF.parseStackTrace (EF.parseRuntime $ fromMaybe "" runtimeM) raw of
  [] -> rawBlock
  frames -> do
    let systemCount = length $ filter (not . (.isInApp)) frames
    div_ [class_ "group/frames"] do
      div_ [class_ "max-h-80 overflow-y-auto divide-y divide-strokeWeak"] $ forM_ frames \f ->
        -- Runtime frames stay in position rather than being grouped to the bottom: the
        -- order is the call path, and resequencing it to tidy the list would misreport
        -- what called what.
        div_ [class_ $ "px-4 py-2 text-sm " <> bool "hidden group-has-[.sys-frames:checked]/frames:block bg-fillWeaker" "" f.isInApp] do
          let frameLabel = div_ [class_ "flex flex-wrap items-baseline gap-x-2"] do
                span_ [class_ $ "font-mono " <> bool "text-textWeak" "text-textStrong font-medium" f.isInApp] $ toHtml f.functionName
                whenJust (guarded (not . T.null) f.filePath) \fp ->
                  span_ [class_ "font-mono text-xs text-textWeak break-all"]
                    $ toHtml
                    $ fp
                    <> foldMap (\l -> ":" <> show l) f.lineNumber
          -- Source context is what makes a Sentry frame readable, and we already had the
          -- machinery: /code_context resolves file+line to real source through the
          -- project's Git integration, cached, and nothing had ever called it. Loaded per
          -- frame on `intersect once` *inside* a closed <details>, so it fires when the
          -- reader opens that frame and never before — one GitHub read on demand rather
          -- than one per frame per page load.
          case (guarded (not . T.null) f.filePath, f.lineNumber) of
            (Just fp, Just ln) -> details_ [class_ "group/frame"] do
              summary_ [class_ "cursor-pointer list-none [&::-webkit-details-marker]:hidden"] frameLabel
              div_
                [ class_ "mt-1"
                , hxGet_ $ "/p/" <> pid.toText <> "/code_context?file=" <> toUriStr fp <> "&line=" <> show ln <> foldMap (\sv -> "&service=" <> toUriStr sv) serviceM
                , hxTrigger_ "intersect once"
                , hxSwap_ "innerHTML"
                ]
                $ div_ [class_ "px-2 py-1"]
                $ loadingIndicator_ LdSM LdDots
            _ -> frameLabel
          -- An SDK that ships context inline is rendered directly; nothing we ingest does
          -- yet, but the field is in the parser and costs one line to honour.
          whenJust f.contextLine
            $ pre_ [class_ "mt-1 text-xs font-mono text-textStrong bg-fillWeaker rounded px-2 py-1 overflow-x-auto"]
            . toHtml
      when (systemCount > 0)
        $ label_ [class_ "flex items-center gap-1.5 px-4 py-2 text-xs text-textBrand cursor-pointer border-t border-strokeWeak hover:bg-fillWeaker transition-colors"] do
          input_ [type_ "checkbox", class_ "sys-frames sr-only"]
          faSprite_ "chevron-down" "regular" "w-3 h-3 shrink-0 group-has-[.sys-frames:checked]/frames:rotate-180 transition-transform"
          span_ [class_ "group-has-[.sys-frames:checked]/frames:hidden"] $ toHtml $ "Show " <> countNoun systemCount "runtime frame"
          span_ [class_ "hidden group-has-[.sys-frames:checked]/frames:inline"] "Hide runtime frames"
      details_ [class_ "border-t border-strokeWeak"] do
        summary_ [class_ "px-4 py-2 text-xs text-textWeak cursor-pointer list-none [&::-webkit-details-marker]:hidden hover:text-textStrong"] "Raw"
        rawBlock
  where
    rawBlock =
      div_ [class_ "max-h-80 overflow-y-auto"]
        $ pre_ [class_ "text-sm leading-relaxed overflow-x-auto whitespace-pre-wrap px-4 py-3"]
        $ code_ []
        $ toHtml raw


-- | Run a lookup that only decides whether an *optional* panel renders, degrading
-- to @fallback@ if it throws rather than taking the page with it.
--
-- All three call sites read TimeFusion or the replay table to enrich the issue
-- page; none of them is the page. A transient TF connection error used to
-- propagate out of the session-id lookup and return a bare 500 for the whole
-- issue — a failure that lands precisely when TF is degraded, which is when
-- someone is most likely reading about an incident. @issueDetailCore@ already
-- states the principle in its own comments ("the trace is supporting evidence,
-- not the page"); this makes it true of the queries as well as the rendering.
enriching :: Text -> ATAuthCtx (Maybe a) -> ATAuthCtx (Maybe a)
enriching what = fmap join . tryWithin Nothing "ISSUE_DETAIL_OPTIONAL_LOOKUP" ["lookup" AE..= what]


-- | Run an optional lookup, bounded by @limitM@ microseconds where a slow read is
-- itself a failure mode. @Nothing@ means "could not be loaded" — which a caller
-- must be able to tell apart from "loaded, found nothing", so the timeout and the
-- exception are reported here rather than being flattened into an empty panel.
tryWithin :: Maybe Int -> Text -> [Pair] -> ATAuthCtx a -> ATAuthCtx (Maybe a)
tryWithin limitM what ctx act =
  trySync (maybe (Just <$> act) (`timeout` act) limitM) >>= \case
    Right res -> res <$ when (isNothing res) (Log.logAttention (what <> "_TIMEOUT") (AE.object ctx))
    Left e -> Nothing <$ Log.logAttention (what <> "_FAILED") (AE.object $ ctx <> ["error" AE..= show @Text e])


-- | Recover a monitor id from @QueryAlertData.queryId@.
--
-- The writer persists it as a @show@ of the newtype, so every row already in the
-- table reads @QueryMonitorId {unQueryMonitorId = <uuid>}@ rather than a bare
-- UUID. Accept either, so the monitor link works for stored rows without a
-- backfill (changing the writer would also change @targetHash@, which is the
-- alert's dedup identity — see docs/issues-page-redesign.md).
--
-- >>> monitorIdFromStored "QueryMonitorId {unQueryMonitorId = b408c446-c0b3-4542-b099-cca3d55425c0}"
-- Just "b408c446-c0b3-4542-b099-cca3d55425c0"
-- >>> monitorIdFromStored "b408c446-c0b3-4542-b099-cca3d55425c0"
-- Just "b408c446-c0b3-4542-b099-cca3d55425c0"
-- >>> monitorIdFromStored "not-a-monitor"
-- Nothing
monitorIdFromStored :: Text -> Maybe Text
monitorIdFromStored = find (isJust . UUID.fromText) . T.split (\c -> not (isHexDigit c || c == '-'))


-- | The page's one spelling of "3h ago", for any timestamp it shows.
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
            isTerminal = idx == lastIdx
            timeLabel
              | idx == 0 = toText $ formatTime defaultTimeLocale "%b %-e, %H:%M:%S" $ POSIX.posixSecondsToUTCTime $ realToFrac (fromIntegral bc.timestamp / 1000 :: Double)
              | otherwise = formatOffset base bc.timestamp
        div_ [class_ $ bool "crumb relative flex gap-2.5 px-4 py-2 border-l-2 border-transparent hover:bg-fillWeaker" "crumb relative flex gap-2.5 px-4 py-2 border-l-2 border-strokeError-strong bg-fillError-weak" isTerminal] do
          div_ [class_ "flex flex-col items-center pt-0.5 shrink-0"] do
            faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
            unless isTerminal $ div_ [class_ "w-px flex-1 bg-strokeWeak mt-1"] ""
          div_ [class_ "min-w-0 flex-1 flex flex-col gap-0.5"] do
            div_ [class_ "flex items-center gap-2 flex-wrap"] do
              span_ [class_ "text-xs tabular-nums text-textWeak shrink-0"] $ toHtml timeLabel
              span_ [class_ $ "text-xs font-medium " <> iconColor] $ toHtml bc.kind
            -- Long messages clamp to 3 lines; a hidden checkbox + `group-has` removes the
            -- clamp, so expansion is pure CSS and survives htmx morphs with no re-init.
            let expandable cls val =
                  label_ [class_ "group/bc cursor-pointer flex items-start gap-1"] do
                    input_ [type_ "checkbox", class_ "hidden"]
                    span_ [class_ $ cls <> " group-has-[:checked]/bc:line-clamp-none group-has-[:checked]/bc:!block min-w-0 flex-1"] $ toHtml val
                    faSprite_ "chevron-down" "regular" "w-3 h-3 text-textWeak shrink-0 mt-1 group-has-[:checked]/bc:rotate-180 transition-transform"
            whenJust bc.message
              $ expandable "text-sm text-textStrong line-clamp-3 break-words whitespace-pre-wrap"
            whenJust (bc.payload >>= breadcrumbDataSummary)
              $ expandable "font-mono text-xs text-textWeak line-clamp-2 break-all"
  div_ [id_ "issue-journey", class_ "border-t border-strokeWeak group/journey"] do
    div_ [class_ "px-4 py-2 flex flex-wrap items-center gap-2 bg-fillWeaker/40"] do
      faSprite_ "route" "regular" "w-3 h-3 text-textWeak"
      span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "User journey"
      span_ [class_ "text-2xs text-textWeak"] $ toHtml $ countNoun total "event" <> " before error"
      div_ [class_ "ml-auto flex items-center gap-2"] do
        input_ [type_ "search", placeholder_ "Search", Aria.label_ "Search the user journey", class_ "input input-xs w-32", filterInputAttr_ ".crumb in #issue-journey"]
        label_ [class_ "btn btn-xs btn-ghost gap-1 has-[:checked]:text-textBrand", term "data-tippy-content" "Newest first"] do
          input_ [type_ "checkbox", class_ "crumb-rev sr-only"]
          faSprite_ "arrows-up-down" "regular" "w-3 h-3"
        copyButton_ "btn btn-xs btn-ghost" "w-3 h-3" "#issue-journey-text's textContent" []
        pre_ [id_ "issue-journey-text", class_ "hidden"] $ toHtml $ unlines [unwords $ catMaybes [Just bc.kind, bc.message] | bc <- crumbList]
    div_ [class_ "py-1 flex flex-col group-has-[.crumb-rev:checked]/journey:flex-col-reverse"]
      $ traverse_ (uncurry renderCrumb) (zip [0 :: Int ..] crumbList)
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
          fromEvents sr = foldMap (map fromEvent) (parseMaybe AE.parseJSON sr.events :: Maybe [Telemetry.SpanEvent])
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
activityPanel_ :: Projects.ProjectId -> Text -> Maybe (Text, UTCTime) -> Html ()
activityPanel_ pid issueId traceRef = do
  let activityUrl =
        "/p/"
          <> pid.toText
          <> "/issues/"
          <> issueId
          <> "/activity"
          <> foldMap (\(tId, tTs) -> "?trace_id=" <> toUriStr tId <> "&trace_ts=" <> toUriStr (formatUTC tTs)) traceRef
  railSection_ "Activity"
    $ div_ [id_ "issue-activity", class_ "-mx-4", hxGet_ activityUrl, hxTrigger_ "intersect once", hxSwap_ "innerHTML"]
    $ div_ [class_ "p-4 flex justify-center"]
    $ loadingIndicator_ LdSM LdDots


-- | A wrapping row of @(icon, iconColour, label, value)@ entries, as used by the
-- query-alert and endpoint context cards.
detailRow_ :: [(Text, Text, Text, Text)] -> Html ()
detailRow_ =
  div_ [class_ "flex flex-wrap items-center gap-x-5 gap-y-2"] . mapM_ \(icn, iconColor, lbl, value) ->
    div_ [class_ "flex items-center gap-1.5 whitespace-nowrap"] do
      faSprite_ icn "regular" $ "w-3 h-3 " <> iconColor
      span_ [class_ "text-xs text-textWeak"] $ toHtml lbl <> ":"
      span_ [class_ "text-xs font-medium"] $ toHtml value


data CardCfg = CardCfg
  { wrapCls :: Maybe Text -- extra classes on the card wrapper
  , headCls :: Maybe Text -- full override of the header bar's classes
  , bodyCls :: Maybe Text -- Nothing renders the body bare, with no padding wrapper
  , trailing :: Maybe (Html ()) -- header content after the label
  }
  deriving stock (Generic)
  deriving anyclass (Default)


-- | The page's card shape: a @surface-raised@ panel whose header bar carries an optional
-- icon, an uppercase section label, and optional trailing controls.
detailCard_ :: Maybe Text -> CardCfg -> Text -> Html () -> Html ()
detailCard_ iconM cfg title body = div_ [class_ $ "surface-raised rounded-2xl overflow-hidden " <> fromMaybe "" cfg.wrapCls] do
  div_ [class_ $ fromMaybe "px-4 py-3 border-b border-strokeWeak flex items-center gap-2" cfg.headCls] do
    whenJust iconM \ic -> faSprite_ ic "regular" "w-3.5 h-3.5 text-textWeak"
    -- h3, not span: the chart and context cards are reachable by heading navigation.
    h3_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] $ toHtml title
    sequence_ cfg.trailing
  maybe body (\c -> div_ [class_ c] body) cfg.bodyCls


-- | A titled block of the right rail. Rail blocks are separated by rules rather than
-- boxed, so the rail reads as one column beside the evidence.
railSection_ :: Text -> Html () -> Html ()
railSection_ title body = section_ [class_ "py-4 border-b border-strokeWeak last:border-b-0"] do
  div_ [class_ "mb-2"] $ sectionLabel_ title
  body


-- | The facts every issue has, in one place and one order, whatever its type: the
-- rail's first block, as Sentry and Datadog both place it.
--
-- @seen@ is passed rather than read off the issue because a runtime exception's
-- real first/last seen live on its error pattern, not on the issue row.
--
-- Absent cells are omitted, not rendered as "Unknown service": an issue with no
-- environment set is not an issue in an environment called Unknown.
issueFactRow_ :: UTCTime -> Issues.Issue -> Maybe ErrorPatterns.ErrorPattern -> (UTCTime, UTCTime) -> Html ()
issueFactRow_ now issue errM (firstSeen, lastSeen) =
  railSection_ "Seen"
    $ dl_ [class_ "grid grid-cols-[auto_minmax(0,1fr)] gap-x-4 gap-y-1.5 text-sm"]
    $ forM_ facts \(lbl, value, tip) -> do
      dt_ [class_ "text-textWeak"] $ toHtml lbl
      dd_ [class_ "text-textStrong font-medium truncate", term "data-tippy-content" tip] $ toHtml value
  where
    nonBlank = mfilter (not . T.null . T.strip)
    facts =
      [("Last seen", agoText now lastSeen, formatUTC lastSeen), ("First seen", agoText now firstSeen, formatUTC firstSeen)]
        <> catMaybes
          [ (\r -> ("First release", r, r)) <$> (errM >>= (.firstRelease))
          , (\r -> ("Last release", r, r)) <$> (errM >>= (.lastRelease))
          , (\sv -> ("Service", sv, sv)) <$> nonBlank issue.service
          , (\e -> ("Environment", e, e)) <$> nonBlank issue.environment
          ]


-- | Banner stating, in words, what the issue's current state means for
-- notifications. The whole point of the ack window is that a reader never has
-- to guess whether alerts are still coming.
issueStatusStrip_ :: UTCTime -> Issues.Issue -> Html ()
issueStatusStrip_ now issue = forM_ banners \(icon, cls, msg) ->
  div_ [class_ $ "flex items-center gap-2 rounded-lg border px-3 py-2 text-sm " <> cls] do
    faSprite_ icon "regular" "w-4 h-4 shrink-0"
    span_ [] $ toHtml msg
  where
    -- Archived and acknowledged are independent, and an issue can be both. This used
    -- to short-circuit on archived, so a both-states issue showed only the archive
    -- banner while the action bar still offered "Unacknowledge" for a state the page
    -- never mentioned. Each true clause now renders.
    ackUntil = zonedTimeToUTC <$> issue.acknowledgedUntil <* issue.acknowledgedAt
    banners =
      catMaybes
        [ ("archive", "border-strokeWeak bg-fillWeaker text-textWeak", "Archived — hidden from the Inbox and never notified. Unarchive to bring it back." :: Text) <$ issue.archivedAt
        , ackUntil <&> \until' -> ("bell-slash", "border-strokeSuccess-weak bg-fillSuccess-weak text-textSuccess", untilLabel "Acknowledged" now until' <> " \x2014 notifications are paused. This issue returns to the Inbox when the window ends or it regresses.")
        ]


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
  , traceRef :: Maybe (Text, UTCTime)
  , replaySession :: Maybe Text
  , errM :: Maybe ErrorPatterns.ErrorPatternL
  , now :: UTCTime
  , isFirst :: Bool
  , tp :: TimePicker.TimePicker
  , stateEvent :: Maybe Issues.IssueEvent
  , canResolve :: Bool
  , members :: [ProjectMembers.ProjectMemberVM]
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
        script_ [fmt|document.addEventListener('DOMContentLoaded',function(){{const p=new URLSearchParams(location.search);if(!p.get('since')&&!p.get('from')&&!p.get('to'))window.setParams({seedParams})}});|]
        issueAggregate_ v
        eventCard_ v
      aside_ [class_ "min-w-0 max-md:px-3 px-4 xl:border-l max-xl:border-t border-strokeWeak"] do
        issueFactRow_ now issue ((.base) <$> errM)
          $ maybe
            (zonedTimeToUTC issue.createdAt, zonedTimeToUTC issue.updatedAt)
            (\errL -> (zonedTimeToUTC errL.base.createdAt, zonedTimeToUTC errL.base.updatedAt))
            errM
        railSection_ "AI assistant" do
          p_ [class_ "text-sm text-textWeak mb-2"] "Ask about the cause, the blast radius, or a fix — answered from this issue's telemetry."
          label_ [Lucid.for_ "ai-panel-toggle", class_ "btn btn-sm btn-outline w-full gap-1.5"] do
            faSprite_ "sparkles" "regular" "w-3.5 h-3.5"
            "Investigate with AI"
        whenJust errM \errL ->
          div_ [hxGet_ $ "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText errL.base.id.unErrorPatternId <> "/group_members", hxTrigger_ "load", hxSwap_ "innerHTML"] pass
        activityPanel_ pid issue.id.toText traceRef
  aiSidePanel_ pid issue.id


-- | Title, culprit, the state line, the Events figure, then the workflow bar — the
-- zone that identifies the issue and acts on it, full width above both columns.
issueHeader_ :: IssueView -> Html ()
issueHeader_ IssueView{..} = header_ [class_ "max-md:px-3 px-4 max-md:pt-4 pt-6 pb-3 border-b border-strokeWeak space-y-3"] do
  issueStatusStrip_ now issue
  -- h2, not h3: the shell's breadcrumb owns h1, and an h3 here ranked below the
  -- empty-state h2s further down the page.
  let detailTitle = case Issues.issuePayload issue of
        Just (Issues.QueryAlertP alertData) | not (T.null $ T.strip alertData.queryName) -> alertData.queryName
        _ -> issue.title
  div_ [class_ "flex gap-6 items-start pr-10"] do
    div_ [class_ "min-w-0 flex-1 space-y-1.5"] do
      h2_ [class_ "max-md:text-xl text-2xl font-semibold text-textStrong break-words"] $ if "⇒" `T.isInfixOf` detailTitle then renderSummaryText_ detailTitle else toHtml detailTitle
      unless (Issues.isBoilerplateAction issue.recommendedAction)
        $ p_ [class_ "text-sm text-textWeak max-w-3xl border-l-2 border-strokeError-strong pl-2"]
        $ toHtml issue.recommendedAction
      div_ [class_ "flex flex-wrap gap-x-3 gap-y-1.5 items-center"] do
        severityBadge_ issue.severity
        issueTypeChip_ False issue.issueType issue.critical
        issueStateBadge_ stateEvent
        -- Only what is peculiar to the type belongs here; service, environment and
        -- first/last seen are common to every issue and live in the rail.
        case Issues.issuePayload issue of
          Just (Issues.LogPatternP d) -> do
            logLevelChip_ d.logLevel d.logPattern
            -- "all time" is explicit: the Events figure beside the title is scoped to
            -- the selected range, and an unqualified count reads as a contradiction.
            metadataChip_ "tally" $ show d.occurrenceCount <> " all time"
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
          Nothing -> unparsablePayload_
    -- The chart's own total, hoisted: `naked` suppresses the widget's value slot, so
    -- this is the only place it renders and it cannot disagree with the chart.
    div_ [class_ "shrink-0 flex gap-6"] do
      when (isJust $ Issues.hashPrefix issue.issueType)
        $ div_ [class_ "flex flex-col items-end gap-1 leading-none", term "data-tippy-content" "Events in the selected range"] do
          span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Events"
          Widget.widgetValueSlotAs_ (issueChartId issue) Nothing
      -- Distinct users by user.id / user.email / client.address, all time.
      whenJust errM \errL -> div_ [class_ "flex flex-col items-end gap-1 leading-none", term "data-tippy-content" "Distinct users affected, all time"] do
        span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Users"
        span_ [class_ "text-2xl font-semibold text-textStrong tabular-nums leading-none"] $ toHtml $ formatWithCommas (fromIntegral errL.base.usersCount :: Double)
  div_ [class_ "flex flex-wrap items-center gap-2"] do
    issueAcknowledgeButton pid issue.id now (zonedTimeToUTC <$> issue.acknowledgedUntil <* issue.acknowledgedAt)
    issueArchiveButton pid issue.id (isJust issue.archivedAt)
    case errM of
      Just errL -> do
        errorResolveAction pid errL.base errL.base.state canResolve
        errorSubscriptionAction pid errL.base
      Nothing ->
        unless (isJust issue.archivedAt)
          $ button_ [type_ "button", class_ "btn btn-sm btn-ghost gap-1.5 text-textSuccess hover:bg-fillSuccess-weak", term "hx-preload" "false", hxGet_ $ "/p/" <> pid.toText <> "/issues/" <> issue.id.toText <> "/resolve", hxTarget_ $ "#archive-ctl-" <> issue.id.toText, hxSwap_ "outerHTML", Aria.label_ "Resolve issue", [__|on htmx:afterRequest remove me|]] do
            faSprite_ "circle-check" "regular" "w-4 h-4"
            span_ [class_ "max-md:hidden"] "Resolve"
    issueTriage_ pid issue members


issueChartId :: Issues.Issue -> Text
issueChartId issue = issue.id.toText <> "-pattern-volume"


-- | Evidence on the left at whatever width is left over, its context panel on the
-- right, stacking under lg. Every issue type lays out this way.
sideBySide_ :: Html () -> Html () -> Html ()
sideBySide_ evidence aside = div_ [class_ "flex flex-col lg:flex-row gap-4 lg:items-start"] do
  div_ [class_ "min-w-0 flex-1"] evidence
  aside


-- | The right-hand context panel every type puts beside its chart.
contextCard_ :: Text -> Text -> Html () -> Html ()
contextCard_ bodyCls = detailCard_ (Just "circle-info") def{wrapCls = Just "lg:w-72 shrink-0", bodyCls = Just bodyCls}


-- | The page's chart. A query alert's total is what the alert measures, not an event
-- count, so it stays in the chart header; every other type's total is the Events
-- figure in 'issueHeader_'. @thresholdM@ draws the alert's breach line; @heightCls@
-- is taller when the chart *is* the evidence.
issueChartCard_ :: IssueView -> Text -> Text -> Maybe Double -> Text -> Html ()
issueChartCard_ IssueView{..} chartTitle heightCls thresholdM chartQuery = do
  let chartId = issueChartId issue
      total = div_ [class_ "flex flex-col gap-0.5 leading-none"] do
        span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Value"
        Widget.widgetValueSlotAs_ chartId Nothing
  detailCard_ Nothing def{headCls = Just "px-4 py-2 flex flex-wrap items-center gap-x-4 gap-y-1 border-b border-strokeWeak", trailing = total <$ guard (isNothing $ Issues.hashPrefix issue.issueType)} chartTitle
    $ div_ [class_ heightCls]
    $ Widget.widget_
      (def :: Widget.Widget)
        { Widget.standalone = Just True
        , Widget.naked = Just True
        , Widget.id = Just chartId
        , Widget.wType = Widget.WTTimeseries
        , Widget.showTooltip = Just True
        , Widget.query = Just chartQuery
        , Widget._projectId = Just issue.projectId
        , Widget.hideLegend = Just True
        , Widget.hideSubtitle = Just True
        , Widget.alertThreshold = thresholdM
        , Widget.showThresholdLines = "always" <$ thresholdM
        , -- A query alert's series is whatever the alert counts, so it must not
          -- borrow the error colour just because it is on an issue page.
          Widget.seriesIntent = "error" <$ guard (issue.issueType `elem` [Issues.RuntimeException, Issues.LogPattern, Issues.LogPatternRateChange])
        }


-- | Volume of the issue's own signal over the selected range.
issueVolumeChart_ :: IssueView -> Text -> Html ()
issueVolumeChart_ v chartTitle = whenJust (Issues.hashPrefix v.issue.issueType) \prefix ->
  issueChartCard_ v chartTitle "h-24" Nothing $ "hashes[*]==\"" <> prefix <> v.issue.targetHash <> "\" | summarize count(*) by bin_auto(timestamp)"


-- | The aggregate band: the range every panel below reads, then the chart beside the
-- type's context panel.
issueAggregate_ :: IssueView -> Html ()
issueAggregate_ v@IssueView{..} = do
  let (_, _, currentRange) = TimePicker.parseTimeRange now tp
      refreshId = "anomaly-chart-refresh"
  div_ [id_ refreshId, class_ "hidden", [__|on submit trigger 'update-query' on window|]] ""
  div_ [class_ "flex flex-wrap items-center gap-2 [&>button]:max-md:basis-full"]
    $ TimePicker.liveDataControls_ (Just refreshId) currentRange (Just $ "issue-" <> issueChartId issue) TimePicker.RefreshOnly
  case Issues.issuePayload issue of
    Nothing -> unparsablePayload_
    Just (Issues.LogPatternP _) -> issueVolumeChart_ v "Pattern Volume"
    Just (Issues.LogPatternRateChangeP _) -> issueVolumeChart_ v "Pattern Volume"
    Just (Issues.RuntimeExceptionP d) ->
      sideBySide_ (issueVolumeChart_ v "Error Frequency")
        $ whenJust ((,) <$> d.requestMethod <*> d.requestPath) \(method, path) ->
          contextCard_ "p-4" "Request" do
            span_ [class_ $ "relative cbadge-sm badge-" <> method <> " whitespace-nowrap"] $ toHtml method
            span_ [class_ "ml-2 text-sm text-textWeak break-all"] $ toHtml path
    Just (Issues.QueryAlertP d) -> do
      let below = d.thresholdType == Issues.Below
          meetsThreshold = if below then d.actualValue <= d.thresholdValue else d.actualValue >= d.thresholdValue
      sideBySide_ (issueChartCard_ v "Alert Query" "h-56" (Just d.thresholdValue) d.queryExpression)
        $ contextCard_ "p-4 flex flex-col gap-4" "Recorded evaluation" do
          div_ [class_ "flex items-start gap-6"] do
            div_ [class_ "flex flex-col gap-1"] do
              span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Recorded value"
              span_ [class_ $ "text-2xl font-semibold tabular-nums leading-none " <> if meetsThreshold then "text-fillWarning-strong" else "text-textStrong"]
                $ toHtml
                $ formatWithCommas d.actualValue
            div_ [class_ "flex flex-col gap-1"] do
              span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Threshold"
              span_ [class_ "text-2xl font-semibold text-textStrong tabular-nums leading-none"] $ toHtml $ formatWithCommas d.thresholdValue
              span_ [class_ "text-xs text-textWeak"] $ toHtml $ bool "At or above" "At or below" below
          unless meetsThreshold
            $ p_
              [class_ "text-sm text-textWeak"]
              "This value does not meet the recorded threshold. Check the monitor’s warning and recovery settings."
          detailRow_ [("bolt", "text-fillWarning-strong", "Recorded", agoText now d.triggeredAt)]
          whenJust (monitorIdFromStored d.queryId) \mid_ ->
            a_ [href_ $ "/p/" <> pid.toText <> "/monitors/" <> mid_ <> "/overview", class_ "text-xs text-textBrand hover:underline flex items-center gap-1.5 w-fit"] do
              faSprite_ "arrow-up-right-from-square" "regular" "w-3 h-3 shrink-0"
              "View monitor"
    Just (Issues.ApiChangeP d) -> do
      div_ [class_ "flex flex-wrap items-center gap-3"] do
        span_ [class_ $ "cbadge-sm whitespace-nowrap badge-" <> d.endpointMethod] $ toHtml d.endpointMethod
        span_ [class_ "font-mono bg-fillWeaker px-2 py-1 rounded text-sm text-textStrong"] $ toHtml d.endpointPath
        span_ [class_ "flex items-center gap-1.5 text-sm text-textWeak"] do
          faSprite_ "server" "regular" "h-3 w-3"
          toHtml d.endpointHost
      sideBySide_ (issueVolumeChart_ v "Request Trend")
        $ contextCard_ "p-4 flex flex-col gap-3" "Endpoint Details"
        $ detailRow_ [("hashtag", "text-fillBrand-strong", "Requests", formatWithCommas (fromIntegral issue.affectedRequests :: Double))]


-- | One card for everything about the sampled event. Its navigator is the only sticky
-- element in the column: it names the occurrence being shown and links to every
-- section, and stays pinned while the sections scroll under it. The card clips
-- rather than hides overflow — hidden would make it a scroll container and trap
-- the navigator along with the waterfall's own sticky ruler.
eventCard_ :: IssueView -> Html ()
-- @--event-nav-h@ is the navigator's height (h-10 + h-9 + its 1px border); the trace
-- waterfall's sticky rows and the jump-link landing offset read it.
eventCard_ IssueView{..} = div_ [class_ "surface-raised rounded-2xl overflow-clip [--event-nav-h:77px]"] do
  let occurrenceUrl useFirst = "/p/" <> pid.toText <> "/issues/" <> issue.id.toText <> "?" <> T.drop 1 (mconcat ["&first_occurrence=true" | useFirst] <> TimePicker.rangeQuery tp)
      hasOccurrences = issue.issueType /= Issues.QueryAlert && not isLogPatternIssue
  nav_ [id_ "issue-event-nav", class_ "sticky top-0 z-20 bg-bgRaised border-b border-strokeWeak", Aria.label_ "Issue evidence"] do
    div_ [class_ "max-md:px-3 px-4 h-10 flex items-center gap-3 overflow-x-auto whitespace-nowrap"] do
      span_ [class_ "text-sm font-semibold text-textStrong"] $ bool "Evidence" "Event" hasOccurrences
      -- Labelled, because "First | Recent" otherwise reads as a peer of the section
      -- links: one picks which occurrence, the others where to look in it.
      div_ [class_ "ml-auto flex items-center gap-1 text-xs"] do
        when hasOccurrences do
          span_ [class_ "text-textWeak mr-1 max-md:hidden"] "Occurrence"
          forM_ ([(True, isFirst, "Show the first occurrence", "First"), (False, not isFirst, "Show the most recent occurrence", "Recent")] :: [(Bool, Bool, Text, Text)]) \(useFirst, active, tip, lbl) ->
            a_ [href_ $ occurrenceUrl useFirst, class_ $ "px-2 py-1 rounded " <> bool "text-textWeak hover:text-textStrong hover:bg-fillWeaker" "bg-fillBrand-weak text-textBrand font-medium" active, term "data-tippy-content" tip] $ toHtml lbl
          span_ [class_ "w-px h-4 bg-strokeWeak mx-1"] ""
        -- A popover, not a dropdown: this row scrolls horizontally and would clip one.
        button_ [type_ "button", class_ "px-2 py-1 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker flex items-center gap-1", term "popovertarget" "issue-copy-pop", style_ "anchor-name: --anchor-issue-copy-pop"] do
          faSprite_ "copy" "regular" "w-3 h-3"
          "Copy as"
        div_ [id_ "issue-copy-pop", term "popover" "auto", class_ "menu bg-bgRaised p-2 text-sm border border-strokeWeak rounded-md shadow-lg space-y-1", style_ "position-try: flip-block; position-anchor: --anchor-issue-copy-pop; top: anchor(bottom); right: anchor(right)"] do
          forM_ ([("JSON", "issue-copy-json"), ("Markdown", "issue-copy-md")] :: [(Text, Text)]) \(lbl, src) -> div_ [class_ "flex items-center justify-between gap-6"] do
            span_ [class_ "text-textStrong"] $ toHtml lbl
            copyButton_ "btn btn-xs btn-ghost" "w-3 h-3" ("#" <> src <> "'s textContent") []
          pre_ [id_ "issue-copy-json", class_ "hidden"] $ toHtml $ decodeUtf8 @Text $ AE.encode $ maybe (getAeson issue.issueData) (AE.toJSON . (.base.errorData)) errM
          pre_ [id_ "issue-copy-md", class_ "hidden"] $ toHtml issueMarkdown
    div_ [class_ "max-md:px-3 px-4 h-9 flex items-center gap-1 overflow-x-auto whitespace-nowrap border-t border-strokeWeak text-xs"] do
      span_ [class_ "text-textWeak mr-1"] "Jump to:"
      forM_ sections \s -> a_ [href_ $ "#" <> s.anchor, class_ "px-2 py-1 rounded text-textWeak hover:text-textStrong hover:bg-fillWeaker"] $ toHtml s.heading
  forM_ sections \s ->
    details_ ([id_ s.anchor, class_ "group/sec border-t border-strokeWeak first-of-type:border-t-0 scroll-mt-(--event-nav-h)", open_ ""] <> s.extra) do
      summary_ [class_ "max-md:px-3 px-4 py-2.5 flex items-center gap-2 cursor-pointer list-none [&::-webkit-details-marker]:hidden hover:bg-fillWeaker"] do
        faSprite_ "chevron-right" "regular" "w-3 h-3 text-textWeak shrink-0 group-open/sec:rotate-90 transition-transform"
        faSprite_ s.glyph "regular" "w-3.5 h-3.5 text-textWeak"
        sectionLabel_ s.heading
        whenJust s.controls $ div_ [class_ "ml-auto flex items-center gap-2"]
      div_ [class_ "pb-3"] s.content
  where
    issueMarkdown =
      unlines
        $ ["## " <> issue.title, "", "- Type: " <> display issue.issueType, "- Severity: " <> display issue.severity]
        <> ["- Service: " <> sv | Just sv <- [issue.service]]
        <> ["- Environment: " <> e | Just e <- [issue.environment]]
        <> foldMap (\errL -> let e = errL.base.errorData in ["- Release: " <> r | Just r <- [e.release]] <> ["", "```", e.errorType <> ": " <> e.message, errL.base.stacktrace, "```"]) errM
    -- The type's own evidence, then trace, logs and replay when the issue has them.
    -- A section with nothing to show is left out rather than rendered as an empty state.
    sections = typeSections <> traceSection <> logsSection <> replaySection
    section = IssueSection Nothing []
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
            present = mapMaybe \(k, v) -> (k,) <$> mfilter (not . T.null . T.strip) v
            kvRows_ :: Text -> [(Text, Text)] -> Html ()
            kvRows_ cls rows = dl_ [class_ $ "grid gap-y-0.5 font-mono text-xs " <> cls] $ forM_ rows \(k, val) -> div_ [class_ "flex gap-3 px-2 py-1 rounded odd:bg-fillWeaker min-w-0"] do
              dt_ [class_ "w-28 shrink-0 text-textWeak"] $ toHtml k
              dd_ [class_ "min-w-0 break-all text-textStrong"] $ toHtml val
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
                [ ("User", present [("id", field (.userId)), ("email", field (.userEmail)), ("name", field (.userName)), ("location", place), ("ip", field (.userIp)), ("tenant", field (.tenantName))])
                , ("Client", present [("browser", field (.browser)), ("os", field (.os)), ("device", field (.device)), ("user_agent", field (.userAgent))])
                , ("Runtime", present [("runtime", runtimeM), ("service", field (.serviceName) <|> issue.service), ("release", field (.release)), ("thread", field (.threadName) <|> field (.threadId)), ("mechanism", display <$> field (.mechanism))])
                , ("Trace", present [("trace.id", fst <$> traceRef), ("span.id", field (.spanId)), ("parent_span.id", field (.parentSpanId)), ("session.id", field (.sessionId))])
                ]
            place = viaNonEmpty (T.intercalate ", " . toList) (catMaybes [field (.geoCity), field (.geoRegion), field (.geoCountry)])
         in [section "issue-highlights" "list-tree" "Highlights" $ kvRows_ "max-md:px-3 px-4 md:grid-cols-2 gap-x-6" highlights | not (null highlights)]
              <> [ section "issue-contexts" "user" "Contexts"
                     $ div_ [class_ "max-md:px-3 px-4 grid md:grid-cols-2 gap-3 items-start"]
                     $ forM_ contexts \(title, rows) -> div_ [class_ "rounded-lg border border-strokeWeak p-2"] do
                       h4_ [class_ "px-2 pb-1 text-xs font-semibold text-textStrong"] $ toHtml title
                       kvRows_ "" rows
                 | not (null contexts)
                 ]
              <> [ IssueSection Nothing [detailsClosedBelowAttr_ 768] "issue-stack" "code" (if hasStack then "Stack trace" else "Error details") do
                     -- The title truncates and the reader expanded to read this in full,
                     -- so it shows whether or not there is a stack trace.
                     unless (T.null d.errorMessage) $ div_ [class_ "max-md:px-3 px-4 pb-3 border-b border-strokeWeak"] do
                       span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide block mb-1"] "Error message"
                       pre_ [class_ "text-sm leading-relaxed text-fillError-strong whitespace-pre-wrap break-words font-mono"] $ toHtml d.errorMessage
                     if hasStack
                       then stackTrace_ pid ((errM >>= (.base.errorData.serviceName)) <|> issue.service) runtimeM trimmedStack
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
              <> [ section "issue-http" "globe" "HTTP request" do
                     let headers = maybe [] Map.toList (field (.requestHeaders))
                         query = [(k, T.drop 1 v) | kv <- maybe [] (T.splitOn "&") (field (.urlQuery)), let (k, v) = T.breakOn "=" kv, not (T.null k)]
                         curl = unwords $ ["curl", "-X", method, shellQuote url] <> concat [["-H", shellQuote (k <> ": " <> v)] | (k, v) <- headers]
                         shellQuote t = "'" <> T.replace "'" "'\\''" t <> "'"
                     div_ [class_ "max-md:px-3 px-4 space-y-3"] do
                       div_ [class_ "flex items-center gap-2 min-w-0"] do
                         span_ [class_ $ "cbadge-sm badge-" <> method] $ toHtml method
                         span_ [class_ "font-mono text-sm text-textStrong break-all"] $ toHtml url
                       forM_ ([("Query string", query), ("Headers", headers)] :: [(Text, [(Text, Text)])]) \(lbl, rows) -> unless (null rows) do
                         div_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide mb-1"] $ toHtml lbl
                         kvRows_ "" rows
                       details_ [class_ "group/curl"] do
                         summary_ [class_ "text-xs text-textBrand cursor-pointer list-none [&::-webkit-details-marker]:hidden"] "Show as curl"
                         div_ [class_ "mt-2 flex items-start gap-2"] do
                           pre_ [id_ "issue-curl", class_ "flex-1 min-w-0 text-xs font-mono bg-fillWeaker rounded px-2 py-1.5 whitespace-pre-wrap break-all"] $ toHtml curl
                           copyButton_ "btn btn-xs btn-ghost" "w-3 h-3" "#issue-curl's innerText" []
                 | Just method <- [d.requestMethod <|> field (.requestMethod)]
                 , Just url <- [field (.urlFull) <|> d.requestPath <|> field (.requestPath)]
                 ]
      Just (Issues.QueryAlertP d) ->
        let scope = mkScopedQuery pid (Nothing, Nothing) issue.environment issue.service
            -- Hands the Explorer the alert's own query, its issue boundary, and the page's window.
            explorerLink = a_
              [ href_ $ timeScopedUrl ("/p/" <> pid.toText <> "/log_explorer") [("query", applyScopedKqlContext scope d.queryExpression)] tp.from tp.to tp.since
              , data_ "preserve-page-context" ""
              , class_ "text-xs text-textBrand hover:underline flex items-center gap-1"
              ]
              do
                "Open query in Explorer"
                faSprite_ "arrow-up-right-from-square" "regular" "h-3 w-3 shrink-0"
         in [IssueSection (Just explorerLink) [] "issue-query" "terminal" "Query" $ pre_ [class_ "max-md:px-3 px-4 text-sm font-mono text-textStrong whitespace-pre-wrap break-words"] $ toHtml d.queryExpression]
      Just (Issues.ApiChangeP d) ->
        let fieldList :: Text -> Text -> Text -> V.Vector Text -> Html ()
            fieldList lbl color icn fields = unless (V.null fields) $ div_ [class_ "flex flex-col gap-1.5"] do
              div_ [class_ "flex items-center gap-1.5"] do
                faSprite_ icn "regular" $ "w-3 h-3 " <> color
                span_ [class_ $ "text-xs font-semibold uppercase tracking-wide " <> color] $ toHtml lbl
                span_ [class_ "text-xs text-textWeak"] $ toHtml $ "(" <> show (V.length fields) <> ")"
              div_ [class_ "flex flex-wrap gap-1"]
                $ V.forM_ fields
                $ span_ [class_ $ "font-mono text-xs px-2 py-0.5 rounded bg-fillWeaker " <> color]
                . toHtml
         in [ section "issue-fields" "list-check" "Field changes"
                $ if not (all V.null [d.newFields, d.deletedFields, d.modifiedFields])
                  then div_ [class_ "max-md:px-3 px-4 flex flex-col gap-4"] do
                    fieldList "New" "text-fillSuccess-strong" "plus" d.newFields
                    fieldList "Deleted" "text-fillError-strong" "minus" d.deletedFields
                    fieldList "Modified" "text-fillWarning-strong" "code" d.modifiedFields
                  else
                    emptyState_
                      def{icon = Just "rocket", size = ESCompact}
                      "New endpoint discovered"
                      "This endpoint started receiving traffic. Inspect the originating request in the Trace section to see headers, body, and call site."
            ]
    -- The log-pattern types differ only in their chip row, which the header owns.
    patternSections sourceField logPattern sampleMessage =
      [ IssueSection (Just $ span_ [class_ "badge badge-sm badge-ghost"] $ toHtml $ sourceFieldLabel sourceField) [] "issue-pattern" "file-lines" "Log pattern" $ renderLogContent_ logPattern
      , -- The sample is re-fetched on every range change, from the URL when it
        -- carries one and from the page's own default otherwise.
        section "issue-sample-section" "terminal" "Event sample"
          $ div_
            [ id_ "issue-sample"
            , hxGet_ $ "/p/" <> pid.toText <> "/issues/" <> issue.id.toText <> "/sample"
            , hxTrigger_ "load, update-query from:window delay:200ms, retryIssueSample"
            , hxSwap_ "innerHTML"
            , term "hx-sync" "this:replace"
            , hxVals_ $ "js:{...(()=>{const p=new URLSearchParams(location.search);return p.get('since')||p.get('from')||p.get('to')?Object.fromEntries(p):" <> TimePicker.rangeJson tp <> "})()}"
            ]
          $ issueSampleCard_ pid sampleMessage SampleLoading
      ]
    traceSection = case traceRef of
      Just (tId, tTs) ->
        [ IssueSection
            { anchor = "issue-trace"
            , glyph = "chart-waterfall"
            , heading = "Trace"
            , -- Icon state is CSS-driven off the section's fullscreen class; the
              -- click only sends the event. tippy, not daisyUI, whose ::before
              -- bubble is clipped by the card's overflow.
              controls = Just
                $ button_ [type_ "button", class_ "p-1.5 rounded hover:bg-fillWeaker cursor-pointer transition-colors max-md:hidden", Aria.label_ "Toggle fullscreen", term "data-tippy-content" "Expand · Esc to exit", [__|on click send toggleFullscreen to #issue-trace|]] do
                  faSprite_ "expand" "regular" "w-3 h-3 text-textWeak group-[.investigation-fullscreen]/sec:hidden"
                  faSprite_ "compress" "regular" "w-3 h-3 text-textWeak hidden group-[.investigation-fullscreen]/sec:block"
            , -- Senders `send toggleFullscreen` here, and this is the only
              -- receiver. Escape closes an open span panel before it exits
              -- fullscreen. `the first <…/> exists`, not a bare `<…/>`: a query
              -- literal is a lazy object that is truthy even when it matches
              -- nothing, so `if <sel/>` never falls through.
              extra =
                [ tabindex_ "-1"
                , [__|on toggleFullscreen(active)
                          default active to (I do not match .investigation-fullscreen)
                          if active add .investigation-fullscreen to me
                          otherwise remove .investigation-fullscreen from me
                          end
                        end
                        on keydown[key is 'Escape'] from window
                          if the first <#trace_details_container.open/> exists
                            send closeDetailPanel to #trace_details_container
                          otherwise if I match .investigation-fullscreen
                            send toggleFullscreen(active: false) to me
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
        ]
      _ -> []
    -- A query alert is a threshold crossing on an aggregate — no originating request,
    -- so no trace-scoped logs (and 'traceRef' is always Nothing, so no Trace either).
    logsSection = [logs | issue.issueType /= Issues.QueryAlert]
      where
        -- A trace happens at an instant, so trace- and service-scoped reads get a
        -- +/-5min window rather than the page's range: 0.37s for the same 14 rows
        -- against 35.9s over +/-2h; the table fetches once it scrolls into view.
        around t = "&from=" <> toUriStr (isoT $ addUTCTime (-300) t) <> "&to=" <> toUriStr (isoT $ addUTCTime 300 t)
        -- An issue is a record of an occurrence, so its own service and environment
        -- are stronger context than whatever the reader last selected globally. The
        -- shared helper quotes telemetry values and keeps this KQL hand-off aligned
        -- with SQL-backed investigation views. `traceRef` honours First/Recent.
        scoped = applyScopedKqlContext $ ScopedQuery pid (Nothing, Nothing) issue.environment issue.service (fst <$> traceRef)
        (logsQuery, logsParams) = case (Issues.hashPrefix issue.issueType, traceRef) of
          (Just prefix, _) | isLogPatternIssue -> (scoped $ "hashes[*]==\"" <> prefix <> issue.targetHash <> "\"", TimePicker.rangeQuery tp)
          (_, Just (tId, tTs)) -> (scoped $ "kind==\"log\" AND context___trace_id==\"" <> tId <> "\"", around tTs)
          -- ~24% of error patterns never captured a trace id (log records carry no
          -- trace context; spans always do). The old empty-string fallback rendered
          -- `context___trace_id==""`, which filters nothing and dumped the project's
          -- entire retention window.
          _ -> (scoped "kind==\"log\"", around $ zonedTimeToUTC $ maybe issue.createdAt (.base.updatedAt) errM)
        explorerLink =
          a_
            [ href_ $ "/p/" <> pid.toText <> "/log_explorer?query=" <> toUriStr logsQuery <> logsParams
            , class_ "text-xs text-textBrand hover:underline flex items-center gap-1"
            , term "data-tippy-content" "Open these logs in the Explorer, with this issue's filter and window applied"
            ]
            do
              "Open in Explorer"
              faSprite_ "arrow-up-right-from-square" "regular" "w-3 h-3 shrink-0"
        logs =
          IssueSection (Just explorerLink) [] "issue-logs" "list-view" "Logs" do
            div_ [id_ "log-content", class_ "max-md:px-1 px-2 flex flex-col lg:flex-row w-full lg:h-[70vh]"] do
              div_ [class_ "grow min-w-0 min-h-0 h-full"]
                $ virtualTable pid (Just ("/p/" <> pid.toText <> "/log_explorer/data?json=true&query=" <> toUriStr logsQuery <> logsParams)) Nothing
              -- Starts hidden alongside the collapsed pane; the swap handler reveals
              -- both together, and closeDetailPanel puts them back.
              div_ [class_ "transition-opacity duration-200 mx-1 hidden lg:block opacity-0 pointer-events-none", id_ "resizer-details_width-wrapper"] $ resizer_ "log_details_container" "details_width" False
              LogItem.detailsPanel_ pid Nothing LogItem.IssuesPanel
    replaySection =
      [ section "replay-section" "video" "Session replay"
          $ termRaw "session-replay" [id_ "sessionReplay", term "initialSession" sessionId, term "consoleOpen" "true", term "fullWidth" "true", class_ "block w-full", term "projectId" pid.toText, term "containerId" "sessionPlayerWrapper"] ("" :: Text)
      | Just sessionId <- [replaySession]
      ]


-- | Collapsible AI chat, open-state in localStorage and driven by a checkbox +
-- group-has variants so it survives htmx swaps.
aiSidePanel_ :: Projects.ProjectId -> Issues.IssueId -> Html ()
aiSidePanel_ pid issueId = do
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
  label_ [Lucid.for_ "ai-panel-toggle", class_ "absolute right-0 top-3 z-10 flex items-center gap-1.5 bg-fillBrand-strong text-white px-2 py-2.5 rounded-l-lg cursor-pointer shadow-md hover:opacity-90 transition-opacity group-has-[#ai-panel-toggle:checked]/ai:hidden", Aria.label_ "Open AI Assistant"]
    $ faSprite_ "sparkles" "regular" "w-3.5 h-3.5"
  div_ [class_ "hidden group-has-[#ai-panel-toggle:checked]/ai:block"] $ resizer_ "ai_chat_container" "ai_width" False
  div_ [id_ "ai_chat_container", class_ "hidden group-has-[#ai-panel-toggle:checked]/ai:flex w-[420px] shrink-0 h-full overflow-hidden flex-col bg-bgBase border-l border-t border-strokeWeak"] do
    div_ [class_ "shrink-0 px-4 py-2.5 border-b border-strokeWeak flex items-center justify-between"] do
      div_ [class_ "flex items-center gap-2"] do
        faSprite_ "sparkles" "regular" "w-3.5 h-3.5 text-fillBrand-strong"
        span_ [class_ "text-xs font-semibold text-textWeak uppercase tracking-wide"] "AI Assistant"
      label_ [Lucid.for_ "ai-panel-toggle", class_ "p-1.5 rounded-lg hover:bg-fillWeaker cursor-pointer transition-colors tap-target", Aria.label_ "Close AI Assistant"]
        $ faSprite_ "xmark" "regular" "w-3 h-3 text-textWeak"
    anomalyAIChatBody_ pid issueId


-- | Resolve control; @errState@ is passed separately because the handler renders the
-- post-resolve state before the pattern is re-read.
errorResolveAction :: Projects.ProjectId -> ErrorPatterns.ErrorPattern -> ErrorPatterns.ErrorState -> Bool -> Html ()
errorResolveAction pid err errState canResolve =
  when canResolve do
    let actionUrl = "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText err.id.unErrorPatternId <> "/resolve"
        resolveBtn url lbl tip = button_ [class_ "btn btn-sm btn-ghost join-item gap-1.5 text-textSuccess hover:bg-fillSuccess-weak", Aria.label_ tip, term "data-tippy-content" tip, hxPost_ url, hxTarget_ "#error-resolve-action", hxSwap_ "outerHTML"] lbl
    div_ [id_ "error-resolve-action", class_ "join"] do
      if errState == ErrorPatterns.ESResolved
        then button_ [class_ "btn btn-sm btn-ghost text-textWeak", disabled_ "true"] do
          faSprite_ "circle-check" "regular" "w-4 h-4"
          span_ [class_ "max-md:hidden"] $ toHtml $ maybe "Resolved" ("Resolved after " <>) err.resolvedInRelease
        else do
          resolveBtn actionUrl (faSprite_ "circle-check" "regular" "w-4 h-4" >> span_ [class_ "max-md:hidden"] "Resolve") "Resolve issue"
          -- Occurrences still reporting this release will not reopen it; a newer one will.
          whenJust err.lastRelease \rel -> resolveBtn (actionUrl <> "?next_release=true") (span_ [class_ "text-xs"] "in next release") ("Resolve in the release after " <> rel)


errorSubscriptionAction :: Projects.ProjectId -> ErrorPatterns.ErrorPattern -> Html ()
errorSubscriptionAction pid err = do
  let isActive = err.subscribed
  form_
    [ id_ "issue-subscription-action"
    , class_ "flex items-center gap-2"
    , hxPost_ $ "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText err.id.unErrorPatternId <> "/subscribe"
    , hxTarget_ "#issue-subscription-action"
    , hxSwap_ "outerHTML"
    , hxTrigger_ "change"
    ]
    do
      span_ [class_ "text-xs text-textWeak flex items-center gap-1"] do
        faSprite_ "bell" "regular" "w-3 h-3"
        span_ [class_ "max-md:hidden"] "Notify every"
      select_ [class_ "select select-sm max-md:w-20 w-36", name_ "notifyEveryMinutes", Aria.label_ "Notification frequency"] do
        option_ ([value_ "0"] <> [selected_ "true" | not isActive]) "Off"
        forM_ ([(10, "10 min"), (20, "20 min"), (30, "30 min"), (60, "1 hr"), (360, "6 hrs"), (1440, "24 hrs")] :: [(Int, Text)]) \(val, label) ->
          option_ ([value_ (show val)] <> [selected_ "true" | isActive && val == err.notifyEveryMinutes]) (toHtml label)


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


assignErrorPostH :: Projects.ProjectId -> UUID.UUID -> AssignErrorForm -> ATAuthCtx (RespHeaders (Html ()))
assignErrorPostH pid errUuid form = do
  (sess, _project) <- Projects.sessionAndProject pid
  appCtx <- ask @AuthContext
  let errId = ErrorPatterns.ErrorPatternId errUuid
      assigneeIdM = form.assigneeId >>= UUID.fromText <&> Projects.UserId
  members <- V.fromList <$> ProjectMembers.selectActiveProjectMembers pid
  errM <- ErrorPatterns.getErrorPatternById errId
  let render eidM aidM = addRespHeaders $ assigneeSection eidM aidM members
      isMember = all (\uid -> any (\m -> m.userId == uid) members) assigneeIdM
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> render Nothing Nothing
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> render (Just err.id) err.assigneeId
      | not isMember -> addErrorToast "Assignee must be an active project member" Nothing >> render (Just err.id) err.assigneeId
      | assigneeIdM == err.assigneeId -> addSuccessToast "Assignee unchanged" Nothing >> render (Just err.id) err.assigneeId
      | otherwise -> do
          now <- Time.currentTime
          void $ ErrorPatterns.setErrorPatternAssignee err.id assigneeIdM now
          whenJust assigneeIdM \assigneeId ->
            void $ liftIO $ withResource appCtx.pool \conn ->
              createJob conn "background_jobs" $ BackgroundJobs.ErrorAssigned pid err.id assigneeId
          issueM <- Issues.selectIssueByHash pid err.hash Issues.AnyIssue
          let event = maybe Issues.IEUnassigned (const Issues.IEAssigned) assigneeIdM
              meta = assigneeIdM <&> \uid -> AE.object ["assignee_id" AE..= uid]
          whenJust issueM \issue -> Issues.logIssueActivity issue.id event (Just sess.user.id) meta
          addSuccessToast "Assignee updated" Nothing
          render (Just err.id) assigneeIdM
  where
    assigneeSection :: Maybe ErrorPatterns.ErrorPatternId -> Maybe Projects.UserId -> V.Vector ProjectMembers.ProjectMemberVM -> Html ()
    assigneeSection eidM aidM mems =
      div_ [id_ "error-assignee", class_ "flex flex-col gap-2 border-t border-strokeWeak pt-3"] do
        span_ [class_ "text-xs text-textWeak"] "Assignee"
        case eidM of
          Nothing -> select_ [class_ "select select-sm w-full", disabled_ "true", name_ "assigneeId"] $ option_ [value_ ""] "Unassigned"
          Just eid ->
            form_ [hxPost_ $ "/p/" <> pid.toText <> "/issues/errors/" <> UUID.toText eid.unErrorPatternId <> "/assign", hxTarget_ "#error-assignee", hxSwap_ "outerHTML", hxTrigger_ "change"]
              $ select_ ([class_ "select select-sm w-full", name_ "assigneeId"] <> [disabled_ "true" | V.null mems]) do
                option_ ([value_ ""] <> [selected_ "true" | isNothing aidM]) "Unassigned"
                forM_ mems \member -> do
                  let emailText = CI.original member.email
                      fullName = T.strip $ member.first_name <> " " <> member.last_name
                  option_ ([value_ member.userId.toText] <> [selected_ "true" | aidM == Just member.userId])
                    $ toHtml
                    $ bool (fullName <> " (" <> emailText <> ")") emailText (T.null fullName)


resolveErrorPostH :: Projects.ProjectId -> UUID.UUID -> Bool -> ATAuthCtx (RespHeaders (Html ()))
resolveErrorPostH pid errUuid inNextRelease = do
  (sess, _project) <- Projects.sessionAndProject pid
  errM <- ErrorPatterns.getErrorPatternById (ErrorPatterns.ErrorPatternId errUuid)
  userPermission <- ProjectMembers.getUserPermission pid sess.user.id
  let canResolve err = userPermission >= Just ProjectMembers.PEdit || err.assigneeId == Just sess.user.id
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> addRespHeaders mempty
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> addRespHeaders mempty
      | not (canResolve err) -> do
          addErrorToast "You do not have permission to resolve this error" Nothing
          addRespHeaders $ errorResolveAction pid err err.state False
      | otherwise -> do
          now <- Time.currentTime
          let resolved = do
                void $ ErrorPatterns.setResolvedInRelease err.id inNextRelease
                addSuccessToast (maybe "Error resolved" ("Resolved in the release after " <>) (err.lastRelease <* guard inNextRelease)) Nothing
                addRespHeaders $ errorResolveAction pid err ErrorPatterns.ESResolved True
          resolveErrorAs sess.user err now >>= \case
            Incidents.ErrorResolutionDenied -> do
              addErrorToast "You do not have permission to resolve this error" Nothing
              addRespHeaders $ errorResolveAction pid err err.state False
            Incidents.ErrorResolutionConflict conflict -> do
              Log.logAttention "Error resolution could not commit incident updates" (pid, err.id, show @Text conflict)
              addErrorToast "The incident changed. Refresh and try resolving again." Nothing
              addRespHeaders $ errorResolveAction pid err err.state False
            Incidents.ErrorResolved -> resolved
            Incidents.ErrorAlreadyResolved -> resolved


-- | Resolve an error pattern and close its incident episode, notifying as the user.
resolveErrorAs :: Projects.User -> ErrorPatterns.ErrorPattern -> UTCTime -> ATAuthCtx Incidents.ErrorResolution
resolveErrorAs user err now = do
  ctx <- ask @AuthContext
  let projectUrl = hostPath ctx.env.hostUrl $ "p/" <> err.projectId.toText
      message iid = Mail.resolvedErrorMessage err user now projectUrl (((projectUrl <> "/issues/") <>) . (.toText) <$> iid)
  Incidents.resolveErrorIncident err.projectId err.id user.id now message


errorSubscriptionPostH :: Projects.ProjectId -> UUID.UUID -> ErrorSubscriptionForm -> ATAuthCtx (RespHeaders (Html ()))
errorSubscriptionPostH pid errUuid form = do
  (_sess, _project) <- Projects.sessionAndProject pid
  let errId = ErrorPatterns.ErrorPatternId errUuid
  errM <- ErrorPatterns.getErrorPatternById errId
  case errM of
    Nothing -> addErrorToast "Error not found" Nothing >> addRespHeaders mempty
    Just err
      | err.projectId /= pid -> addErrorToast "Error not found for this project" Nothing >> addRespHeaders mempty
      | otherwise -> do
          let notifyEveryRaw = fromMaybe 0 form.notifyEveryMinutes
              notifyEvery = clamp (1, 1440) $ if notifyEveryRaw == 0 then 30 else notifyEveryRaw
              shouldSubscribe = notifyEveryRaw > 0
          now <- Time.currentTime
          void $ ErrorPatterns.updateErrorPatternSubscription err.id shouldSubscribe notifyEvery now
          addSuccessToast (if shouldSubscribe then "Notifications enabled" else "Notifications disabled") Nothing
          addRespHeaders $ errorSubscriptionAction pid err{ErrorPatterns.subscribed = shouldSubscribe, ErrorPatterns.notifyEveryMinutes = notifyEvery}


-- | Form for AI chat input
newtype AIChatForm = AIChatForm {query :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromForm)


-- | System prompt for anomaly investigation AI
issueSystemPrompt :: UTCTime -> Text
issueSystemPrompt now =
  unlines
    [ "You are Monoscope's issue-investigation assistant — an expert debugger embedded in the issue detail page. The user is on-call and trying to understand a specific issue. You have access to its details, errors, stack traces, and trace data, plus tools that fetch live telemetry."
    , ""
    , "Tone: precise, technical, calm. Answer like a senior SRE pairing on a debug — direct, no fluff."
    , ""
    , "## Current Context"
    , "CURRENT TIME (UTC): " <> show now
    , "Use the current time to interpret relative phrases (e.g. \"last 2 hours\" → `{\"since\": \"2H\"}`)."
    , ""
    , "## Telemetry Schema"
    , "<schema>"
    , Schema.generateSchemaForAI Schema.telemetrySchema
    , "</schema>"
    , ""
    , AI.kqlGuide
    , ""
    , AI.outputFormatInstructions
    , ""
    , "## How To Investigate"
    , "1. Identify the likely root cause from the error type, stack trace, and surrounding telemetry."
    , "2. Use the issue's service / method / path context to narrow down."
    , "3. Suggest concrete debugging steps or fixes — name files, fields, queries when possible."
    , ""
    , "## Tool-Use Policy (overrides the workflow in <output_format>)"
    , "- Analysis questions (\"What could cause this?\", \"Suggest a fix\") → answer DIRECTLY from the issue context. Do NOT call tools, and do NOT call `run_query`."
    , "- Chart / visualization requests (\"plot errors over time\", \"show a chart of...\") → build the KQL query and `widgets` config directly from <schema>. Do NOT call `get_schema`, `get_field_values`, or `run_query` — the chat panel renders the chart from the query alone."
    , "- Call tools ONLY when the answer must contain actual data values from the live store (e.g. \"top 5 services by error count\" where real numbers are required)."
    , ""
    , "## Response Format"
    , "- Lead with a single-sentence summary."
    , "- Follow with bullets or short paragraphs — never walls of text."
    , "- The chat panel is ~400px wide, so brevity matters."
    , "- For chart requests, prioritize a correct KQL query and a data-driven explanation."
    ]


-- | Handle AI chat POST request
-- Designed to power the AI chat in the anomalies page. The chat thread is loaded via htmx and theres an input which when submitted gets sent here.
aiChatPostH :: Projects.ProjectId -> Issues.IssueId -> AIChatForm -> ATAuthCtx (RespHeaders (Html ()))
aiChatPostH pid issueId form
  | T.length form.query > 4000 = addRespHeaders $ aiChatResponse_ pid form.query "Query too long. Maximum 4000 characters allowed." Nothing Nothing Nothing
  | otherwise = do
      appCtx <- ask @AuthContext
      now <- Time.currentTime
      let convId = UUIDId issueId.unUUIDId :: UUIDId "conversation"
      void $ Issues.getOrCreateConversation pid convId Issues.CTAnomaly (AE.object ["issue_id" AE..= issueId])
      issueM <- Issues.selectIssueById pid issueId
      maybe (respond Nothing convId "Issue not found. Unable to analyze." Nothing Nothing True) (processIssue appCtx now convId) issueM
  where
    respond systemPromptM convId response widgets toolCalls includeUserMsg = do
      when includeUserMsg $ Issues.insertChatMessage pid convId Issues.ChatUser form.query Nothing Nothing
      Issues.insertChatMessage pid convId Issues.ChatAssistant response (AE.toJSON <$> widgets) (AE.toJSON <$> toolCalls)
      addRespHeaders $ aiChatResponse_ pid form.query response widgets toolCalls systemPromptM

    processIssue appCtx now convId issue = do
      fullSystemPrompt <- buildSystemPromptForIssue pid issue now
      let config = (AI.defaultAgenticConfig pid){AI.facetContext = Nothing, AI.customContext = Just fullSystemPrompt, AI.conversationId = Just convId, AI.conversationType = Just Issues.CTAnomaly, AI.systemPromptOverride = Just $ issueSystemPrompt now, AI.sourceConfig = Just appCtx.config, AI.useTimefusion = appCtx.env.enableTimefusionReads}
      result <- AI.runAgenticChatWithHistory config form.query appCtx.config.openaiModel appCtx.config.openaiApiKey
      either
        (\err -> respond (Just fullSystemPrompt) convId ("I encountered an error while analyzing this issue: " <> err) Nothing Nothing False)
        (handleChatResult (Just fullSystemPrompt) convId)
        result

    handleChatResult systemPromptM convId chatResult = do
      appCtx <- ask @AuthContext
      AI.ensureConversationTitle (Just appCtx.config) pid convId form.query chatResult.response appCtx.config.openaiSmallModel appCtx.config.openaiApiKey
      either
        (\_ -> respond systemPromptM convId chatResult.response Nothing (Just chatResult.toolCalls) False)
        ( \aiResp ->
            let ws = guarded (not . null) $ take 10 $ AI.responseWidgets aiResp
                txt = fromMaybe (bool chatResult.response "Here are the requested visualizations:" $ isJust ws) $ mfilter (not . T.null) aiResp.explanation
             in respond systemPromptM convId txt ws (Just chatResult.toolCalls) False
        )
        (AI.parseAgenticResponse chatResult)


-- | Handle AI chat history GET request
aiChatHistoryGetH :: Projects.ProjectId -> Issues.IssueId -> ATAuthCtx (RespHeaders (Html ()))
aiChatHistoryGetH pid issueId = do
  _ <- Projects.sessionAndProject pid
  now <- Time.currentTime
  Issues.selectIssueById pid issueId >>= \case
    Nothing -> addRespHeaders $ aiChatHistoryView_ pid []
    Just issue -> do
      systemPrompt <- buildSystemPromptForIssue pid issue now
      messages <- Issues.selectChatHistory pid (UUIDId issueId.unUUIDId :: UUIDId "conversation")
      addRespHeaders $ aiChatHistoryWithSystemPrompt_ pid systemPrompt messages


-- | Build complete system prompt for an issue (shared between POST and GET)
buildSystemPromptForIssue :: Projects.ProjectId -> Issues.Issue -> UTCTime -> ATAuthCtx Text
buildSystemPromptForIssue pid issue now = do
  useTf <- useTfReads
  errorM <- bool (pure Nothing) (ErrorPatterns.getErrorPatternByHash pid issue.endpointHash) (issue.issueType == Issues.RuntimeException)
  (traceDataM, spans) <- maybe (pure (Nothing, V.empty)) (fetchTrace useTf) errorM
  alertContextM <- case Issues.issuePayload issue of
    Just (Issues.QueryAlertP alertData) -> do
      let twoDaysAgo = addUTCTime (-172800) now
      monitorM <- runMaybeT do
        monitorId <- hoistMaybe $ UUID.fromText alertData.queryId
        MaybeT $ Monitors.queryMonitorById (Monitors.QueryMonitorId monitorId)
      metricsData <- Charts.queryMetrics Nothing (Just Charts.DTMetric) (Just pid) (Just alertData.queryExpression) Nothing Nothing (Just $ show twoDaysAgo) (Just $ show now) Nothing Nothing []
      pure $ Just (alertData, monitorM, metricsData)
    _ -> pure Nothing
  facetSummaryM <- SchemaCatalog.getFacetSummary pid "otel_logs_and_spans" (addUTCTime (-86400) now) now
  pure
    $ unlines
      [ issueSystemPrompt now
      , ""
      , "--- FACET SUMMARY ---"
      , maybe "" formatFacetSummaryForAI facetSummaryM
      , ""
      , "--- ISSUE CONTEXT ---"
      , buildAIContext issue errorM traceDataM spans alertContextM
      ]
  where
    fetchTrace useTf err =
      fromMaybe (Nothing, V.empty) <$> runMaybeT do
        tId <- hoistMaybe err.recentTraceId
        (trData, spans) <- MaybeT $ Telemetry.getTraceDetails useTf pid tId (Just $ zonedTimeToUTC err.updatedAt) now
        pure (Just trData, V.fromList spans)
    buildAIContext iss errM trDataM spans alertContextM =
      unlines
        $ [ "## Issue Details"
          , "- **Title**: " <> iss.title
          , "- **Type**: " <> show iss.issueType
          , "- **Severity**: " <> display iss.severity
          , "- **Service**: " <> Issues.serviceLabel iss.service
          , "- **Recommended Action**: " <> iss.recommendedAction
          ]
        <> catMaybes
          [ alertContextM <&> \(alertData, monitorM, metricsData) -> formatCompleteAlertContext alertData monitorM metricsData
          , errM <&> \err ->
              unlines
                [ ""
                , "## Error Details"
                , "- **Error Type**: " <> err.errorType
                , "- **Message**: " <> err.message
                , "- **Stack Trace**:"
                , "```"
                , err.errorData.stackTrace
                , "```"
                , maybe "" ("- **Service Name**: " <>) err.errorData.serviceName
                , maybe "" ("- **Request Method**: " <>) err.errorData.requestMethod
                , maybe "" ("- **Request Path**: " <>) err.errorData.requestPath
                ]
          , trDataM <&> \tr ->
              unlines
                [ ""
                , "## Trace Context"
                , "- **Trace ID**: " <> tr.traceId
                , "- **Duration**: " <> show tr.traceDurationNs <> "ns"
                , "- **Span Count**: " <> show (V.length spans)
                ]
          , guard (not $ V.null spans)
              $> unlines
                [ ""
                , "## Span Breakdown"
                , unlines $ V.toList $ flip V.map (V.take 10 spans) $ \s ->
                    "- " <> fromMaybe "unknown" s.name <> " (" <> maybe "n/a" show s.duration <> "ns)"
                ]
          ]
    formatCompleteAlertContext alertData monitorM metricsData =
      unlines
        $ [ ""
          , "## Alert Configuration"
          , ""
          , "### Query & Thresholds"
          , "- **Alert Query (KQL)**: `" <> alertData.queryExpression <> "`"
          , "- **Alert Threshold**: " <> show alertData.thresholdValue <> " (trigger when " <> display alertData.thresholdType <> ")"
          , "- **Current Value**: " <> show alertData.actualValue
          , "- **Triggered At**: " <> formatUTC alertData.triggeredAt
          ]
        -- Every remaining section is read off the monitor row, so each empties out
        -- under its own heading when the monitor has been deleted.
        <> concat
          [ ["", heading] <> foldMap body monitorM
          | (heading, body) <-
              [ ("### Execution Schedule", \m -> ["- **Check Interval**: Every " <> show m.checkIntervalMins <> " minutes", "- **Sustained Duration Required**: " <> show m.thresholdSustainedForMins <> " minutes (threshold must be exceeded for this long)", "- **Last Evaluated**: " <> maybe "never" formatUTC m.lastEvaluated])
              ,
                ( "### Trigger Conditions"
                , \m ->
                    ("- **Trigger Direction**: " <> bool "Alert when value EXCEEDS threshold (>)" "Alert when value DROPS BELOW threshold (<)" m.triggerLessThan)
                      : catMaybes
                        [ m.warningThreshold <&> \wt -> "- **Warning Threshold**: " <> show wt <> " (warning level)"
                        , m.alertRecoveryThreshold <&> \art -> "- **Alert Recovery**: Alert clears when value returns to " <> show art
                        , m.warningRecoveryThreshold <&> \wrt -> "- **Warning Recovery**: Warning clears when value returns to " <> show wrt
                        ]
                )
              ,
                ( "### Current Status"
                , \m ->
                    ("- **Status**: " <> show m.currentStatus <> " (last check: " <> show m.currentValue <> ")")
                      : catMaybes
                        [ m.alertLastTriggered <&> \t -> "- **Last Alert Triggered**: " <> formatUTC t
                        , m.warningLastTriggered <&> \t -> "- **Last Warning Triggered**: " <> formatUTC t
                        ]
                )
              ,
                ( "### Notification Configuration"
                , \m ->
                    [ "- **Alert Title**: " <> m.alertConfig.title
                    , "- **Severity**: " <> m.alertConfig.severity
                    , "- **Email Recipients**: " <> if m.alertConfig.emailAll then "All project members" else T.intercalate ", " (V.toList $ fmap CI.original m.alertConfig.emails)
                    , if V.null m.alertConfig.slackChannels then "" else "- **Slack Channels**: " <> T.intercalate ", " (V.toList m.alertConfig.slackChannels)
                    ]
                )
              ]
          ]
        <> catMaybes
          [ guard (metricsData.rowsCount > 0) $> formatQueryResults metricsData
          , guard (metricsData.rowsCount == 0 && isNothing monitorM) $> "\n_Note: Monitor record was deleted. Only basic alert data available._"
          ]
    formatQueryResults md =
      let timestampIdx = V.elemIndex "timestamp" md.headers
          formatRow = V.imap \idx -> \case
            Just n | Just idx == timestampIdx -> formatUTC $ POSIX.posixSecondsToUTCTime $ realToFrac n
            Just val -> show val
            Nothing -> "N/A"
       in unlines
            [ ""
            , "## Alert Query Results"
            , ""
            , "Recent data points from the alert query (last " <> show (floor md.rowsCount :: Int) <> " measurements):"
            , ""
            , maybe "" formatQueryStats md.stats
            , ""
            , "| " <> T.intercalate " | " (V.toList md.headers) <> " |"
            , "|" <> T.replicate (V.length md.headers) "-------|"
            , unlines $ V.toList $ flip V.map (V.take 20 md.dataset) \row ->
                "| " <> T.intercalate " | " (V.toList $ formatRow row) <> " |"
            , if md.rowsCount > 20 then "... (" <> show (floor md.rowsCount - 20 :: Int) <> " more rows)" else ""
            ]
      where
        formatQueryStats stats =
          unlines
            [ "**Statistics:**"
            , "- Min: " <> show stats.min
            , "- Max: " <> show stats.max
            , "- Mean: " <> show stats.mean <> " (average)"
            , "- Sum: " <> show stats.sum
            , "- Count: " <> show stats.count <> " data points"
            ]
    formatFacetSummaryForAI summary =
      let FacetData facetMap = summary.facetJson
          formatField (fieldName, values) =
            "- "
              <> fieldName
              <> ": "
              <> T.intercalate ", " (map (\fv -> fv.value <> " (" <> show fv.count <> ")") $ take 10 values)
              <> bool "" ", ..." (length values > 10)
          topFields = take 30 $ sortOn (Down . sum . map (.count) . snd) $ HM.toList facetMap
       in unlines
            $ "Available telemetry fields (top values by frequency):"
            : map formatField topFields
              <> ["... and " <> show (HM.size facetMap - 30) <> " more fields" | HM.size facetMap > 30]


-- | Render a single chat response (user question + AI answer)
aiChatResponse_ :: Projects.ProjectId -> Text -> Text -> Maybe [Widget.Widget] -> Maybe [AI.ToolCallInfo] -> Maybe Text -> Html ()
aiChatResponse_ pid userQuery explanation widgetsM toolCallsM systemPromptM =
  div_ [class_ "animate-fade-in py-3 border-b border-strokeWeak last:border-b-0"] do
    -- User question
    div_ [class_ "flex items-start gap-2.5 mb-3"] do
      chatAvatar_ "bg-fillWeak" "user" "text-textWeak"
      p_ [class_ "text-sm text-textStrong mt-0.5"] $ toHtml userQuery
    -- AI response
    div_ [class_ "flex items-start gap-2.5"] do
      chatAvatar_ "bg-fillBrand-weak" "sparkles" "text-fillBrand-strong"
      div_ [class_ "flex-1 min-w-0 mt-0.5"] do
        div_ [class_ "prose prose-sm text-textStrong max-w-none leading-relaxed"] $ renderMarkdown explanation
        whenJust widgetsM \widgets -> do
          let processedWidgets = maybe widgets (`processWidgetsWithToolData` widgets) toolCallsM
          div_ [class_ "grid grid-cols-1 gap-3 mt-3"] $ forM_ processedWidgets \widget ->
            div_ [class_ "w-full aspect-[3/1]"] $ Widget.widget_ widget{Widget._projectId = Just pid}
    -- Collapsed debug info (tool calls + system prompt)
    let toolCalls = fromMaybe [] toolCallsM
    unless (null toolCalls && isNothing systemPromptM)
      $ details_ [class_ "mt-2 ml-[2.125rem] border border-strokeWeak rounded-lg text-xs group/debug"] do
        summary_ [class_ "cursor-pointer px-2.5 py-1.5 text-textWeak hover:bg-fillWeaker list-none flex items-center gap-1.5"] do
          faSprite_ "chevron-right" "regular" "w-2.5 h-2.5 transition-transform group-open/debug:rotate-90"
          span_ [] $ toHtml @Text $ bool "System context" (show (length toolCalls) <> " tool calls") (not (null toolCalls))
        div_ [class_ "px-2.5 py-2 border-t border-strokeWeak bg-fillWeaker/50"] do
          forM_ toolCalls toolCallView_
          whenJust systemPromptM \sp ->
            details_ [class_ $ bool "" "mt-2 border-t border-strokeWeak pt-2 " (not (null toolCalls)) <> "group/sp"] do
              summary_ [class_ "cursor-pointer text-textWeak hover:text-textStrong list-none flex items-center gap-1.5"] do
                faSprite_ "chevron-right" "regular" "w-2.5 h-2.5 transition-transform group-open/sp:rotate-90"
                span_ [] "System Prompt"
              div_ [class_ "mt-1 font-mono whitespace-pre-wrap text-textWeak max-h-48 overflow-y-auto"] $ toHtml sp
  where
    chatAvatar_ bg icon color = div_ [class_ $ "shrink-0 w-6 h-6 rounded-full flex items-center justify-center " <> bg] $ faSprite_ icon "regular" ("w-3 h-3 " <> color)
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


-- | Render a single tool call
toolCallView_ :: AI.ToolCallInfo -> Html ()
toolCallView_ tc =
  div_ [class_ "flex flex-col gap-1 py-2 border-b border-strokeWeak last:border-0"] do
    div_ [class_ "flex items-center gap-2 flex-wrap"] do
      span_ [class_ "font-mono text-xs px-2 py-0.5 bg-fillWeak rounded"] $ toHtml tc.name
      whenJust (Map.lookup "query" tc.args) $ span_ [class_ "text-xs text-textWeak break-all"] . toHtml . show
    unless (T.null tc.resultPreview) $ div_ [class_ "text-xs text-textWeak font-mono pl-4 whitespace-pre-wrap break-all"] $ toHtml $ "→ " <> tc.resultPreview


-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import Data.Aeson qualified as AE
-- >>> import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
-- >>> import Lucid (renderText, toHtml)


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
aiChatHistoryView_ :: Projects.ProjectId -> [Issues.AIChatMessage] -> Html ()
aiChatHistoryView_ pid = render
  where
    render (u : a : rest) | u.role == Issues.ChatUser && a.role == Issues.ChatAssistant = do
      let (explanation, widgets) = parseStoredContent a.content a.widgets
      aiChatResponse_ pid u.content explanation widgets (parseStoredJSON @[AI.ToolCallInfo] a.metadata) Nothing
      render rest
    render (event : rest) | event.role == Issues.ChatExecutionEvent = do
      div_ [class_ "my-3 rounded-lg border border-strokeWeak bg-fillWeak px-3 py-2 text-xs text-textWeak"] do
        faSprite_ "circle-info" "regular" "mr-1.5 inline-block h-3.5 w-3.5"
        toHtml event.content
      render rest
    render (_ : rest) = render rest
    render [] = pass
    -- Stored content is JSON (code blocks stripped) where the LLM produced it, plain text otherwise.
    parseStoredContent content storedWidgets = case AI.parseLLMResponse content of
      Right aiResp ->
        let widgets = AI.responseWidgets aiResp
            structured = isJust aiResp.query || isJust aiResp.visualization || not (null widgets)
         in ( fromMaybe (bool content "Here are the requested visualizations:" structured) aiResp.explanation
            , guarded (not . null) widgets <|> parseStoredJSON @[Widget.Widget] storedWidgets
            )
      Left _ -> (content, parseStoredJSON @[Widget.Widget] storedWidgets)


-- | Render chat history with system prompt as first message
aiChatHistoryWithSystemPrompt_ :: Projects.ProjectId -> Text -> [Issues.AIChatMessage] -> Html ()
aiChatHistoryWithSystemPrompt_ pid systemPrompt msgs = do
  details_ [class_ "my-2 border border-strokeWeak rounded-lg group/sp"] do
    summary_ [class_ "cursor-pointer px-2.5 py-1.5 text-xs text-textWeak hover:bg-fillWeaker list-none flex items-center gap-1.5"] do
      faSprite_ "chevron-right" "regular" "w-2.5 h-2.5 transition-transform group-open/sp:rotate-90"
      faSprite_ "file-lines" "regular" "w-3 h-3"
      span_ [] "System Prompt"
    div_ [class_ "px-2.5 py-2 border-t border-strokeWeak bg-fillWeaker/50 text-xs font-mono whitespace-pre-wrap text-textWeak max-h-48 overflow-y-auto"] $ toHtml systemPrompt
  aiChatHistoryView_ pid msgs


-- | Decode a stored JSONB value (anomaly metadata, widget lists) into a typed payload.
parseStoredJSON :: AE.FromJSON a => Maybe (Aeson AE.Value) -> Maybe a
parseStoredJSON = (>>= parseMaybe AE.parseJSON . getAeson)


-- | AI Chat body (response container + input bar, no header)
anomalyAIChatBody_ :: Projects.ProjectId -> Issues.IssueId -> Html ()
anomalyAIChatBody_ pid issueId = do
  let baseUrl = "/p/" <> pid.toText <> "/issues/" <> issueId.toText
  div_
    [ id_ "ai-response-container"
    , class_ "flex-1 overflow-y-auto flex flex-col px-3"
    , hxGet_ $ baseUrl <> "/ai_chat/history"
    , hxTrigger_ "load-chat once"
    , -- htmx 4's native event detail carries no `elt` (the compat shim backfills it only on
      -- the legacy aliases), so branch on the swap target via `event.target` instead.
      term "hx-on::after:swap" "window.evalScriptsFromContent && window.evalScriptsFromContent(event.target === this ? this : this.lastElementChild); this.lastElementChild?.scrollIntoView({behavior: 'smooth', block: 'start'})"
    ]
    ""
  div_ [class_ "shrink-0 border-t border-strokeWeak p-3 flex flex-col gap-2"] do
    form_
      [ hxPost_ $ baseUrl <> "/ai_chat"
      , hxTarget_ "#ai-response-container"
      , hxSwap_ "beforeend"
      , hxIndicator_ "#ai-chat-loader"
      , term "hx-on::after:request" "this.reset()"
      ]
      $ div_ [class_ "flex items-center gap-2 bg-fillWeaker rounded-lg px-3 py-2 has-[:focus]:ring-1 has-[:focus]:ring-strokeBrand-weak transition-shadow"] do
        input_
          [ class_ "flex-1 bg-transparent border-none outline-none text-textStrong placeholder-textWeak text-sm"
          , placeholder_ "Ask about this issue..."
          , name_ "query"
          , id_ "ai-chat-input"
          , autocomplete_ "off"
          ]
        span_ [class_ "htmx-indicator", id_ "ai-chat-loader"] $ faSprite_ "spinner" "regular" "w-4 h-4 animate-spin text-iconBrand"
        button_ [type_ "submit", class_ "p-1.5 rounded-lg bg-fillBrand-strong text-white hover:opacity-90 transition-opacity tap-target cursor-pointer", Aria.label_ "Send message"] $ faSprite_ "arrow-right" "regular" "w-3.5 h-3.5"
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
  -> ATAuthCtx (RespHeaders IssueListGet)
issueListGetH pid filterTM sortM timeFilter pageM perPageM loadM periodM serviceFilters typeFilters = do
  (session, project, bw) <- mkPageCtx pid
  let tab = parseTab filterTM
      currentFilterTab = tabParam tab
      tabFilters = tabIssueFilters tab
      filterV = fromMaybe "14d" timeFilter
      pageInt = fromMaybe 0 $ readMaybe . toString =<< pageM
      perPage = fromMaybe 25 $ readMaybe . toString =<< perPageM
      currentSort = fromMaybe "-created_at" sortM
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

  let filterParams = foldMap ("&service=" <>) serviceFilters <> foldMap ("&type=" <>) typeFilters
      baseUrl = "/p/" <> pid.toText <> "/issues?filter=" <> currentFilterTab <> "&sort=" <> currentSort <> "&period=" <> period <> filterParams
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
      issuesVM = V.fromList $ map (IssueVM currTime filterV) issues
      tableActions =
        TableHeaderActions
          { baseUrl
          , targetId = "anomalyListContainer"
          , sortOptions =
              [ ("Newest", "Most recently created", "-created_at")
              , ("Oldest", "Oldest issues first", "+created_at")
              , ("Recently Updated", "Most recently updated", "-updated_at")
              , ("Most events", "Most events in the period", "-event_count")
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
          }
  addRespHeaders
    $ if loadM == Just "true"
      then ALRows $ TableRows{columns = issueColumns pid period names Nothing, rows = issuesVM, renderAsTable = True, rowId = Just issueRowId, rowAttrs = Just issueRowAttrs, pagination = if totalCount > 0 then Just paginationConfig else Nothing}
      else ALPage $ PageCtx bwconf issuesTable


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
-- ["Inbox","Acknowledged","Archived"]
data IssueTab = TabInbox | TabAcknowledged | TabArchived
  deriving stock (Bounded, Enum, Eq, Ord, Show)


-- | The tab's wire spelling, used for both the URL and the visible label.
tabParam :: IssueTab -> Text
tabParam = \case
  TabInbox -> "Inbox"
  TabAcknowledged -> "Acknowledged"
  TabArchived -> "Archived"


-- | An absent or unrecognised @filter@ lands on Inbox: the tab is a navigation
-- affordance, so a stale bookmark should show something useful rather than fail.
--
-- >>> map parseTab [Just "Archived", Just "Acknowledged", Just "nonsense", Nothing]
-- [TabArchived,TabAcknowledged,TabInbox,TabInbox]
parseTab :: Maybe Text -> IssueTab
parseTab = fromMaybe TabInbox . (inverseMap tabParam =<<)


-- | Inbox additionally hides severity='low' so demoted silent drops don't clutter it.
tabIssueFilters :: IssueTab -> Issues.IssueFilters
tabIssueFilters = \case
  TabAcknowledged -> Issues.defIssueFilters{Issues.ack = Issues.IsNotNull}
  TabArchived -> Issues.defIssueFilters{Issues.archive = Issues.IsNotNull}
  TabInbox -> Issues.defIssueFilters{Issues.ack = Issues.IsNull, Issues.archive = Issues.IsNull, Issues.hideLowSeverity = True}


-- | One line under the tab strip saying what the tab *means*.
tabBlurb :: IssueTab -> Text
tabBlurb = \case
  TabAcknowledged -> "Someone owns these. Notifications are paused until the acknowledgement expires or the issue regresses."
  TabArchived -> "Not actionable. Hidden and never notified — unarchive to bring one back."
  TabInbox -> "Needs triage. Acknowledge to pause notifications, or archive if it isn't actionable."


-- | Bulk actions offered on each tab: only transitions that make sense from the
-- state you're looking at.
issueBulkActions :: Projects.ProjectId -> IssueTab -> [ProjectMembers.ProjectMemberVM] -> [BulkAction]
issueBulkActions pid tab members =
  [ BulkAction{icon = Just i, title = t, uri = url a, choices = cs}
  | (i, t, a, cs) <- case tab of
      TabAcknowledged -> [("arrow-rotate-left", "Unacknowledge", BAUnacknowledge, []), ("archive", "Archive", BAArchive, [])] <> triage
      TabArchived -> [("arrow-rotate-left", "Unarchive", BAUnarchive, [])]
      TabInbox -> [("check", "Acknowledge", BAAcknowledge, []), ("archive", "Archive", BAArchive, [])] <> triage
  ]
  where
    url a = "/p/" <> pid.toText <> "/issues/bulk_actions/" <> bulkActionSlug a
    withValue a v = url a <> "?value=" <> toUriStr v
    triage =
      [ ("circle-check", "Resolve", BAResolve, [])
      , ("flag", "Priority", BAPriority, [(T.toTitle (display sev), withValue BAPriority (display sev)) | sev <- [minBound .. maxBound :: Issues.IssueSeverity]])
      , ("user", "Assign", BAAssign, ("Unassigned", url BAAssign) : [(memberLabel m, withValue BAAssign m.userId.toText) | m <- members])
      ]


issueZeroState :: Projects.ProjectId -> IssueTab -> ZeroState
issueZeroState pid = \case
  TabAcknowledged ->
    ZeroState "circle-check" "Nothing acknowledged" "Acknowledge an issue to pause its notifications while you work on it." (ESLink inboxUrl "Go to Inbox")
  TabArchived ->
    ZeroState "archive" "Nothing archived" "Archive the issues that aren't worth acting on. They stay hidden and never notify." (ESLink inboxUrl "Go to Inbox")
  TabInbox ->
    ZeroState "empty-set" "Nothing to triage" "New issues and errors land here automatically once you integrate an SDK." (ESLink "https://monoscope.tech/docs/sdks/" "View SDK setup guides")
  where
    inboxUrl = "/p/" <> pid.toText <> "/issues?filter=" <> tabParam TabInbox


data IssueListGet
  = ALPage (PageCtx (Table IssueVM))
  | ALRows (TableRows IssueVM)


instance ToHtml IssueListGet where
  toHtml (ALPage pg) = toHtml pg
  toHtml (ALRows rows) = toHtml rows
  toHtmlRaw = toHtml


issueRowAttrs :: IssueVM -> [Attribute]
issueRowAttrs (IssueVM _ _ issue) = class_ ("group/row hover:bg-fillWeaker " <> bg) : sty
  where
    -- Matched on the constructor, not on @display@: severity is typed everywhere
    -- else on this page, and a Text round-trip would drop both tints silently if a
    -- constructor were ever renamed.
    (bg, sty) = case issue.base.severity of
      Issues.Critical -> ("bg-fillError-weak", [style_ "box-shadow: inset 3px 0 0 var(--color-fillError-strong)"])
      Issues.Warning -> ("bg-fillWarning-weak", [style_ "box-shadow: inset 3px 0 0 var(--color-fillWarning-strong)"])
      Issues.Info -> ("", [])
      Issues.Low -> ("", [])


issueRowId :: IssueVM -> Text
issueRowId (IssueVM _ _ issue) = issue.base.id.toText


-- | (icon, colorClass, tooltip) — uses shape+color so status isn't color-only.
-- Archived and acknowledged outrank severity; below them, Critical and Warning get
-- their own icon and everything else reads as plain Active. Severity was 'Text' here for
-- the same reason it was in 'severityBadge_' — callers 'display'ed a typed value to have
-- it re-matched.
anomalyStatusIndicator :: Issues.Issue -> (Text, Text, Text)
anomalyStatusIndicator issue
  | isJust issue.archivedAt = ("archive", "text-fillStrong", "Archived \x2014 hidden, no notifications")
  | isJust issue.acknowledgedAt = ("bell-slash", "text-fillSuccess-strong", "Acknowledged \x2014 notifications paused")
  | otherwise = case issue.severity of
      Issues.Critical -> ("octagon-exclamation", "text-fillError-strong", "Critical")
      Issues.Warning -> ("triangle-alert", "text-fillWarning-strong", "Warning")
      Issues.Info -> active
      Issues.Low -> active
  where
    active = ("circle-alert", "text-textWeak", "Active")


data IssueVM = IssueVM UTCTime Text Issues.IssueL
  deriving stock (Show)


issueColumns :: Projects.ProjectId -> Text -> Map.Map Projects.UserId Text -> Maybe (Html ()) -> [Column IssueVM]
issueColumns pid period names toggleM =
  [ col "Issue" (renderIssueMainCol pid) & withAttrs [class_ "min-w-0 max-w-0 w-full"]
  , col "Last Seen" lastSeenCol & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Age" ageCol & withAttrs [class_ "w-16 max-md:hidden"]
  , col "Activity" activityCol & withAttrs [class_ "w-40 max-md:hidden"] & maybe identity withColHeaderExtra toggleM
  , col ("Events (" <> period <> ")") eventsCol & withAttrs [class_ "w-24 max-md:hidden"]
  , col "Users" usersCol & withAttrs [class_ "w-16 max-md:hidden"]
  , col "Priority" (\(IssueVM _ _ i) -> span_ [class_ "text-xs text-textWeak"] $ toHtml $ T.toTitle $ display i.base.severity) & withAttrs [class_ "w-20 max-lg:hidden"]
  , col "Assignee" assigneeCol & withAttrs [class_ "w-28 max-lg:hidden"]
  ]
  where
    ageCol (IssueVM currTime _ issue) = span_ [class_ "text-xs text-textWeak"] $ toHtml $ agoText currTime $ zonedTimeToUTC issue.base.createdAt
    usersCol (IssueVM _ _ issue) = span_ [class_ "tabular-nums text-sm text-textStrong"] $ toHtml $ bool (formatWithCommas (fromIntegral issue.usersCount :: Double)) "\x2014" (issue.usersCount == 0)
    assigneeCol (IssueVM _ _ issue) = span_ [class_ "text-xs text-textWeak truncate block"] $ toHtml $ fromMaybe "\x2014" $ (`Map.lookup` names) =<< issue.base.assigneeId
    eventsCol (IssueVM _ _ issue) =
      span_ [class_ $ "tabular-nums font-medium text-sm " <> countStyle issue.eventCount]
        $ toHtml
        $ formatWithCommas (fromIntegral issue.eventCount)
    countStyle n
      | n >= 100 = "text-fillError-strong" :: Text
      | n >= 10 = "text-fillWarning-strong"
      | otherwise = "text-textStrong"
    lastSeenCol (IssueVM currTime _ issue) =
      span_ [class_ "text-xs text-textWeak"] $ toHtml $ agoText currTime issue.lastSeen
    activityCol (IssueVM _ _ issue) = sparkline_ $ V.toList issue.activityBuckets


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
renderSummaryText_ = traverse_ (summaryToken_ False) . words


renderIssueTitle_ :: Issues.IssueL -> Html ()
renderIssueTitle_ Issues.IssueL{base}
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


renderIssueMainCol :: Projects.ProjectId -> IssueVM -> Html ()
renderIssueMainCol pid (IssueVM currTime period issue) = do
  let b = issue.base
      isAcknowledged = isJust b.acknowledgedAt
      isArchived = isJust b.archivedAt
      (icon, iconColor, tooltip) = anomalyStatusIndicator b
      issueUrl = "/p/" <> pid.toText <> "/issues/" <> b.id.toText
      stateBadges = do
        severityBadge_ b.severity
        issueStateBadge_ issue.latestStateEvent
        ackBadge_ currTime b
  div_ [class_ "flex flex-col gap-1 py-0.5 min-w-0"] do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      div_ [class_ "text-sm line-clamp-2 min-w-0"] do
        span_ [class_ $ "inline-flex align-middle mr-1 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon "regular" "w-3.5 h-3.5"
        span_ [class_ "text-xs tabular-nums mr-1 text-textWeak max-md:text-textStrong max-md:font-medium"] $ toHtml $ "#" <> show b.seqNum <> " "
        a_ ([href_ issueUrl, class_ "font-medium text-textStrong hover:text-textBrand transition-colors"] <> navTabAttrs) $ renderIssueTitle_ issue
      span_ [class_ "shrink-0 flex items-center gap-1.5 max-md:hidden"] stateBadges
      div_ [class_ "shrink-0 flex gap-1 items-center opacity-0 group-hover/row:opacity-100 has-[:focus-within]:opacity-100 transition-opacity max-md:hidden"] do
        inlineBtn (bool "Acknowledge \x2014 pause notifications" "Unacknowledge \x2014 resume notifications" isAcknowledged) (bool "check" "arrow-rotate-left" isAcknowledged) (hxGet_ $ issueUrl <> bool "/acknowledge" "/unacknowledge" isAcknowledged) []
        unless isAcknowledged
          $ durationMenu_ ("ack-pop-" <> b.id.toText) "Acknowledge for\x2026" [] (\q -> [hxGet_ $ issueUrl <> "/acknowledge" <> durationQuery "duration" q, hxSwap_ "none"]) \popId ->
            inlineBtn "Acknowledge for a set time" "clock" (term "popovertarget" popId) [style_ $ "anchor-name: --anchor-" <> popId]
        inlineBtn (bool "Archive \x2014 hide it and stop notifying" "Unarchive \x2014 move back to the Inbox" isArchived) "archive" (hxGet_ $ issueUrl <> bool "/archive" "/unarchive" isArchived) []
    div_ [class_ "hidden max-md:flex items-center gap-1.5 flex-wrap"] stateBadges
    div_ [class_ "max-md:hidden"] $ issuePreview_ issue
    div_ [class_ "hidden max-md:flex items-center justify-between text-xs text-textWeak"] do
      div_ [class_ "flex items-center gap-1.5"] do
        span_ [class_ $ "tabular-nums" <> bool "" " font-medium text-textStrong" (issue.eventCount > 100)] $ toHtml $ countNoun issue.eventCount "event" <> " (" <> period <> ")"
        span_ [class_ "opacity-30"] "·"
        span_ [] $ toHtml $ agoText currTime $ zonedTimeToUTC b.createdAt
      div_ [class_ "flex items-center gap-3"] do
        button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "none", hxGet_ $ issueUrl <> bool "/acknowledge" "/unacknowledge" isAcknowledged] $ toHtml $ bool "Ack" "Unack" isAcknowledged
        button_ [type_ "button", class_ "cursor-pointer text-textBrand tap-target font-medium", hxSwap_ "none", hxGet_ $ issueUrl <> bool "/archive" "/unarchive" isArchived] $ toHtml $ bool "Archive" "Unarchive" isArchived
  where
    -- Rows swap nothing: the handler fires `issuesListChanged` and the table
    -- reloads, so an acknowledged row actually leaves the Inbox.
    inlineBtn tip icon hxAction extraAttrs =
      button_ ([type_ "button", term "data-tippy-content" tip, Aria.label_ tip, class_ "cursor-pointer hover:text-textBrand transition-colors tap-target", hxSwap_ "none", hxAction] <> extraAttrs)
        $ faSprite_ icon "regular" "h-3.5 w-3.5"


issueCardCompact_ :: Projects.ProjectId -> UTCTime -> Issues.IssueL -> Html ()
issueCardCompact_ pid now issue = do
  let b = issue.base
      (icon, iconColor, tooltip) = anomalyStatusIndicator b
      issueUrl = "/p/" <> pid.toText <> "/issues/" <> b.id.toText
  a_ ([href_ issueUrl, class_ "block border border-strokeWeak rounded-xl p-3 hover:bg-bgRaised transition-colors"] <> navTabAttrs) do
    div_ [class_ "flex items-center gap-2 min-w-0"] do
      span_ [class_ $ "shrink-0 " <> iconColor, title_ tooltip, Aria.label_ tooltip] $ faSprite_ icon "regular" "w-3.5 h-3.5"
      span_ [class_ "text-xs text-textWeak shrink-0 tabular-nums"] $ toHtml $ "#" <> show b.seqNum
      span_ [class_ "text-sm font-medium text-textStrong truncate min-w-0"] $ renderIssueTitle_ issue
      severityBadge_ b.severity
      span_ [class_ "text-xs text-textWeak shrink-0 ml-auto"] $ toHtml $ agoText now $ zonedTimeToUTC b.createdAt
    issuePreview_ issue


-- | Only Critical and Warning carry a badge; Info and Low are deliberately unbadged.
--
-- Took 'Text' and matched @"critical"@/@"warning"@ with a fall-through, while all three
-- callers held an 'Issues.IssueSeverity' and 'display'ed it just to be re-matched. That
-- round-trip meant renaming a constructor would silently drop *both* badges instead of
-- failing to compile. Spelling the two silent cases out makes the omission a decision.
severityBadge_ :: Issues.IssueSeverity -> Html ()
severityBadge_ = \case
  Issues.Critical -> span_ [class_ "badge badge-sm bg-fillError-weak text-fillError-strong border border-strokeError-strong"] "CRITICAL"
  Issues.Warning -> span_ [class_ "badge badge-sm bg-fillWarning-weak text-fillWarning-strong border border-strokeWarning-weak"] "WARNING"
  Issues.Info -> pass
  Issues.Low -> pass


issueStateBadge_ :: Maybe Issues.IssueEvent -> Html ()
issueStateBadge_ = \case
  Just Issues.IERegressed -> badge "bg-fillError-weak text-fillError-strong border-strokeError-strong" "REGRESSED"
  Just Issues.IEEscalated -> badge "bg-fillError-weak text-fillError-strong border-strokeError-strong" "ESCALATED"
  Just Issues.IEResolved -> badge "bg-fillSuccess-weak text-fillSuccess-strong border-strokeSuccess-strong" "RESOLVED"
  Just Issues.IEAutoResolved -> badge "bg-fillSuccess-weak text-fillSuccess-strong border-strokeSuccess-strong" "RESOLVED"
  Just Issues.IEReopened -> badge "bg-fillWarning-weak text-fillWarning-strong border-strokeWarning-weak" "REOPENED"
  Just Issues.IEAckExpired -> badge "bg-fillWarning-weak text-fillWarning-strong border-strokeWarning-weak" "ACK EXPIRED"
  _ -> pass
  where
    badge cls = span_ [class_ $ "badge badge-sm border " <> cls]


issuePreview_ :: Issues.IssueL -> Html ()
issuePreview_ Issues.IssueL{base} = div_ [class_ "flex items-center gap-2 min-w-0 overflow-hidden text-xs text-textWeak"] do
  issueTypeChip_ True base.issueType base.critical
  whenJust base.service $ span_ [class_ "shrink-0", term "data-tippy-content" "Service"] . toHtml
  span_ [class_ "shrink-0 opacity-40"] "·"
  snippet
  where
    snippet = case Issues.issuePayload base of
      Just (Issues.RuntimeExceptionP d) -> previewSnippet $ d.errorType <> ": " <> d.errorMessage
      Just (Issues.QueryAlertP d) -> previewSnippet d.queryExpression
      Just (Issues.LogPatternP d) -> logPatternPreview d.logPattern d.sampleMessage
      Just (Issues.LogPatternRateChangeP d) -> logPatternPreview d.logPattern d.sampleMessage
      Just (Issues.ApiChangeP d) ->
        previewSnippet $ d.endpointMethod <> " " <> d.endpointPath <> if T.null d.endpointHost then "" else " on " <> d.endpointHost
      Nothing -> unparsablePayload_
    previewSnippet txt = span_ [class_ "font-mono truncate min-w-0", term "data-tippy-content" txt] $ renderWithPlaceholders_ $ unescSummary txt
    logPatternPreview pat sampleMsg
      | "⇒" `T.isInfixOf` pat = span_ [class_ "truncate min-w-0"] $ renderSummaryText_ pat
      | Just msg <- sampleMsg, not (T.null msg) = previewSnippet msg
      | otherwise = previewSnippet pat


-- | "Ack'd · 6h left" / "Acknowledged indefinitely" chip. Silence has an end, and
-- the list is where you need to see it without opening anything.
ackBadge_ :: UTCTime -> Issues.Issue -> Html ()
ackBadge_ now b = whenJust (zonedTimeToUTC <$> b.acknowledgedUntil <* b.acknowledgedAt) \until' ->
  let lbl = untilLabel "Ack'd" now until'
   in span_ [class_ "badge badge-sm badge-ghost gap-1 shrink-0", term "data-tippy-content" $ untilLabel "Acknowledged" now until' <> " \x2014 notifications are paused"] do
        faSprite_ "bell-slash" "regular" "h-3 w-3"
        toHtml lbl


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
          <> req "/unacknowledge"
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
          <> req "/acknowledge"
      )
      do
        faSprite_ "check" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] "Acknowledge"
    durationMenu_ (ctlId <> "-menu") "Acknowledge for\x2026" [] (\q -> req $ "/acknowledge" <> durationQuery "duration" q) \popId ->
      button_ [type_ "button", class_ "btn btn-sm btn-primary join-item px-2", term "popovertarget" popId, style_ $ "anchor-name: --anchor-" <> popId, Aria.label_ "Acknowledge for a set time"]
        $ faSprite_ "chevron-down" "regular" "w-3 h-3"
  where
    ctlId = "ack-ctl-" <> aid.toText
    req path = [term "hx-preload" "false", hxGet_ $ "/p/" <> pid.toText <> "/issues/" <> aid.toText <> path, hxTarget_ ("#" <> ctlId), hxSwap_ "outerHTML"]


issueArchiveButton :: Projects.ProjectId -> Issues.IssueId -> Bool -> Html ()
issueArchiveButton pid aid archived = div_ [id_ ctlId, class_ "inline-flex"] do
  if archived
    then button_ ([type_ "button", class_ "btn btn-sm gap-1.5 tooltip tooltip-bottom btn-ghost bg-fillWarning-weak text-textWarning border-strokeWarning-weak", data_ "tip" "Move back to the Inbox", Aria.label_ "Unarchive issue"] <> req "/unarchive") do
      faSprite_ "archive" "regular" "w-4 h-4"
      span_ [class_ "max-md:hidden"] "Unarchive"
    else div_ [class_ "join"] do
      button_ ([type_ "button", class_ "btn btn-sm btn-ghost join-item gap-1.5 tooltip tooltip-bottom", data_ "tip" "Not actionable \x2014 hide it and stop notifying", Aria.label_ "Archive issue"] <> req "/archive") do
        faSprite_ "archive" "regular" "w-4 h-4"
        span_ [class_ "max-md:hidden"] "Archive"
      durationMenu_ (ctlId <> "-menu") "Archive for\x2026" [("Until it escalates", "escalating")] (\q -> req $ "/archive" <> durationQuery "window" q) \popId ->
        button_ [type_ "button", class_ "btn btn-sm btn-ghost join-item px-2", term "popovertarget" popId, style_ $ "anchor-name: --anchor-" <> popId, Aria.label_ "Archive for a set time"]
          $ faSprite_ "chevron-down" "regular" "w-3 h-3"
  where
    ctlId = "archive-ctl-" <> aid.toText
    req path = [term "hx-preload" "false", hxGet_ $ "/p/" <> pid.toText <> "/issues/" <> aid.toText <> path, hxTarget_ ("#" <> ctlId), hxSwap_ "outerHTML"]


-- | The issue's type as icon + label. @compact@ is the list\'s chip: tighter,
-- abbreviated where the full label would not fit in a row, with the full text on
-- hover. The detail header takes the roomier form.
issueTypeChip_ :: Bool -> Issues.IssueType -> Bool -> Html ()
issueTypeChip_ compact issueType critical =
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
      Issues.ApiChange | critical -> ("text-fillError-strong", "exclamation-triangle", "Breaking")
      Issues.ApiChange -> ("text-fillInformation-strong", "info", "Incremental")
    shortTxt = case issueType of
      Issues.LogPattern -> "Log"
      Issues.LogPatternRateChange -> "Rate"
      _ -> fullTxt


-- | Render a log level chip, inferring ERROR severity from the pattern when the
-- log_level field is missing (e.g. http logs lacking an explicit level but with
-- 4xx/5xx status). Mirrors @isIssueWorthy@ in BackgroundJobs.
logLevelChip_ :: Monad m => Maybe Text -> Text -> HtmlT m ()
logLevelChip_ logLevel pat =
  let effective = case T.toUpper <$> logLevel of
        Just l
          | l `elem` ["ERROR", "FATAL", "CRITICAL"] -> Just "ERROR"
          | l `elem` ["WARN", "WARNING"] -> Just "WARN"
        other
          | any (`T.isInfixOf` pat) ["status;badge-error⇒ERROR", "status_code;badge-4xx", "status_code;badge-5xx"] -> Just "ERROR"
          | otherwise -> other
      (cls, icon, label) = case effective of
        Just "ERROR" -> ("text-fillError-strong bg-fillError-weak", "triangle-alert", "ERROR")
        Just "WARN" -> ("text-fillWarning-strong bg-fillWarning-weak", "triangle-alert", "WARN")
        Just l -> ("", "circle-dot", l)
        Nothing -> ("", "circle-dot", "Unknown")
   in colorChip_ cls icon label


issueActivityGetH :: Projects.ProjectId -> Issues.IssueId -> Maybe Text -> Maybe UTCTime -> ATAuthCtx (RespHeaders (Html ()))
issueActivityGetH pid issueId traceIdM traceTsM = do
  (_sess, _project) <- Projects.sessionAndProject pid
  activities <- Issues.selectIssueActivity pid issueId
  now <- Time.currentTime
  env <- (.env) <$> ask @AuthContext
  -- The journey comes out of the issue's trace, the one read on this page big
  -- enough to time out; it is bounded here so a slow trace costs the journey, not
  -- the issue-events timeline beside it.
  journeySpans <- flip foldMapM ((,) <$> traceIdM <*> traceTsM) \(tId, tTs) ->
    foldMap (\(_, spans, _) -> spans)
      . join
      <$> tryWithin
        (Just $ env.traceViewTimeoutSecs * 1_000_000)
        "ISSUE_JOURNEY_FETCH"
        ["issue_id" AE..= issueId, "trace_id" AE..= tId]
        (Telemetry.getTraceDetailsForView env.enableTimefusionReads pid tId (Just tTs) now Nothing)
  let userIds = ordNub $ mapMaybe (.createdBy) activities
  users :: [Projects.User] <-
    if null userIds
      then pure []
      else Hasql.interp [HI.sql| SELECT id, created_at, updated_at, deleted_at, active, first_name, last_name, display_image_url, email, is_sudo, phone_number FROM users.users WHERE id = ANY(#{userIds}::uuid[]) |]
  let userMap = Map.fromList $ map (\u -> (u.id, u)) users
  -- Issue events first: it is the short, high-signal section (often a single
  -- "Created" row) and was buried under a journey scrollbox. The journey renders
  -- last at natural height so the page scroll carries it instead of a nested box.
  addRespHeaders do
    div_ [class_ "border-t border-strokeWeak"] do
      div_ [class_ "px-4 py-2 flex items-center gap-2 bg-fillWeaker/40"] do
        faSprite_ "circle-info" "regular" "w-3 h-3 text-textWeak"
        span_ [class_ "text-2xs font-semibold text-textWeak uppercase tracking-wide"] "Timeline"
    issueActivityTimeline_ userMap now activities
    userJourneySection_ journeySpans


issueActivityTimeline_ :: Map.Map Projects.UserId Projects.User -> UTCTime -> [Issues.IssueActivity] -> Html ()
issueActivityTimeline_ userMap now activities
  | null activities = emptyState_ def{size = ESCompact} "No activity yet." ""
  | otherwise = div_ [class_ "p-4 flex flex-col gap-0"] $ forM_ activities \a -> do
      let (icon, color, label) = eventDisplay a.event
          actorText = foldMap (\uid -> foldMap (\u -> " by " <> CI.original u.email) $ Map.lookup uid userMap) a.createdBy
      div_ [class_ "flex items-start gap-3 relative pl-4 pb-4 border-l-2 border-strokeWeak ml-2"] do
        div_ [class_ $ "absolute -left-[9px] top-0.5 w-4 h-4 rounded-full flex items-center justify-center " <> color]
          $ faSprite_ icon "regular" "w-2.5 h-2.5"
        div_ [class_ "flex flex-col gap-0.5 min-w-0"] do
          span_ [class_ "text-sm text-textStrong"] $ toHtml $ label <> actorText
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


errorGroupMembersGetH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
errorGroupMembersGetH pid errorId = do
  _ <- Projects.sessionAndProject pid
  members <- PatternMerge.getErrorPatternGroupMembers pid (ErrorPatternId errorId)
  addRespHeaders
    $ unless (null members)
    $ railSection_ ("Merged patterns · " <> show (length members))
    $ div_ [class_ "flex flex-col gap-2"] do
      forM_ members \member -> do
        let memberId = UUID.toText member.id.unErrorPatternId
            unmergeUrl = "/p/" <> pid.toText <> "/issues/errors/" <> memberId <> "/unmerge"
        div_ [class_ "flex items-center justify-between p-3 bg-fillWeaker rounded-lg", id_ $ "member-" <> memberId] do
          div_ [class_ "flex flex-col gap-1 min-w-0"] do
            span_ [class_ "text-sm font-medium text-textStrong truncate"] $ toHtml $ member.errorType <> ": " <> member.message
            span_ [class_ "text-xs text-textWeak"] $ toHtml $ "Hash: " <> member.hash
          button_
            [ class_ "btn btn-xs btn-ghost tap-target"
            , Aria.label_ "Unmerge pattern"
            , hxPost_ unmergeUrl
            , hxTarget_ $ "#member-" <> memberId
            , hxSwap_ "outerHTML"
            ]
            do
              faSprite_ "code-branch" "regular" "w-3 h-3"
              "Unmerge"


errorUnmergePostH :: Projects.ProjectId -> UUID.UUID -> ATAuthCtx (RespHeaders (Html ()))
errorUnmergePostH pid errorId = do
  _ <- Projects.sessionAndProject pid
  void $ PatternMerge.unmergeErrorPattern pid (ErrorPatternId errorId)
  addSuccessToast "Pattern unmerged" Nothing
  addRespHeaders $ div_ [class_ "p-3 bg-fillSuccess-weak rounded-lg text-sm text-fillSuccess-strong"] "Pattern unmerged successfully"
