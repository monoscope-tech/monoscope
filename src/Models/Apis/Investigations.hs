{-# LANGUAGE OrPatterns #-}

module Models.Apis.Investigations (
  Scope (..),
  Turn (..),
  Step (..),
  Checkpoint (..),
  loadCheckpoint,
  loadCheckpointFor,
  CheckpointTarget (..),
  RunTurn (..),
  RunLease (..),
  CheckpointAccessDenied (..),
  SavedCheckpoint (..),
  CheckpointCursor (..),
  CheckpointConflict (..),
  startCheckpoint,
  startCheckpointFor,
  prepareNativeOperation,
  adoptSlackCheckpoint,
  operationResponse,
  mkToolCallInfo,
  commitProgress,
  AgenticChatResult (..),
  ToolCallInfo (..),
  loadAnswer,
  saveAnswer,
  ReplyBatch (..),
  loadReplyBatch,
  saveReplyBatch,
  claimReplyPart,
  confirmReplyPublication,
  rejectReplyPublication,
  captureReplyPublication,
  PublicationSearch (..),
  loadReplySearch,
  saveReplySearchCursor,
  Event (..),
  Followup (..),
  pendingFollowups,
  isAcceptedFollowup,
  ModelOutcome (..),
  Entry (..),
  History (..),
  ToolResult (..),
  OperationProposal (..),
  OperationClaim,
  operationId,
  OperationDecision (..),
  OperationResult (..),
  OperationOutcome (..),
  OperationConflict (..),
  OperationEntry (..),
  beginOperation,
  completeOperation,
  atomicOperation,
  listOperations,
  recordEvent,
  recentEvents,
  ProgressTarget (..),
  ProgressMessage (..),
  loadProgress,
  loadProgressRefresh,
  loadProgressSearch,
  saveProgressSearchCursor,
  claimProgress,
  confirmProgress,
  rejectProgress,
  captureProgress,
  recentProgressEvents,
  conversationEvidencePermission,
  retainEvidencePermission,
  classifyLegacyEvidence,
  visibleConversationIds,
) where

import Data.Aeson qualified as AE
import Data.ByteString qualified as BS
import Data.Effectful.Hasql qualified as Hasql
import Data.Effectful.LLM ()
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID.V5 qualified as UUID5
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Deriving.Aeson.Stock qualified as DAE
import Effectful (Eff)
import Effectful.Exception (throwIO)
import Hasql.Interpolate qualified as HI
import Hasql.Transaction qualified as Tx
import Hasql.Transaction.Sessions qualified as TxS
import Langchain.LLM.Core qualified as LLM
import Models.Apis.Integrations qualified as Integrations
import Models.Projects.ProjectMembers qualified as ProjectMembers
import Models.Projects.Projects qualified as Projects
import OpenAI.V1.Chat.Completions qualified as OpenAIV1
import Pkg.AgentCatalog qualified as Catalog
import Pkg.AgentOperation qualified as Operation
import Pkg.DeriveUtils (UUIDId (..))
import Relude
import System.Types (DB)
import Prelude (until)


data Scope = Scope
  { runId :: UUIDId "investigation_run"
  , projectId :: Projects.ProjectId
  , userId :: Projects.UserId
  , teamId :: Text
  , channelId :: Text
  , threadTs :: Text
  , messageTs :: Text
  }


-- | A returned tool value may contain a domain failure. Returning does not mean
-- the hypothesis was confirmed or that an external operation succeeded.
data ToolResult = ToolResult {formatted :: Text, rawData :: Maybe AE.Value}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | The proposal is immutable. Its policy is a typed caller-supplied audit snapshot.
data OperationProposal policy = OperationProposal {call :: Operation.OperationCall, policy :: policy}
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson (OperationProposal policy)


newtype OperationClaim = OperationClaim (UUIDId "agent_operation")
  deriving stock (Show)


operationId :: OperationClaim -> UUIDId "agent_operation"
operationId (OperationClaim identifier) = identifier


data OperationResult = RecordedResult ToolResult | UncertainResult OperationClaim Operation.OperationCall (UUIDId "ai_routine_run")
  deriving stock (Show)


data OperationDecision
  = ExecuteOperation OperationClaim
  | RetainedOperation ToolResult
  | ReconcileOperation OperationClaim Operation.OperationCall (UUIDId "ai_routine_run")
  deriving stock (Show)


-- Returning a tool response does not assert that the domain operation succeeded.
data OperationOutcome = OperationRunning | OperationUnknown | OperationReturned ToolResult
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson OperationOutcome


data OperationConflict = OperationScopeMismatch | OperationStepMoved | OperationProposalChanged | OperationOutcomeChanged | OperationResponseInvalid
  deriving stock (Show)
  deriving anyclass (Exception)


data OperationEntry policy = OperationEntry
  { id :: UUIDId "agent_operation"
  , runId :: UUIDId "ai_routine_run"
  , requestedBy :: Projects.UserId
  , proposal :: OperationProposal policy
  , outcome :: OperationOutcome
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


data ModelOutcome = ModelAnswered LLM.Message | ModelRequestFailed
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


-- | Signed human messages waiting in this investigation's thread.
data Followup = Followup
  { receiptId :: UUIDId "slack_event"
  , slackUserId :: Text
  , messageTs :: Text
  , text :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON, HI.DecodeRow)


pendingFollowups :: DB es => Scope -> Eff es [Followup]
pendingFollowups scope =
  Hasql.interp
    $ [HI.sql|WITH candidates AS (
    SELECT id, payload->'event'->>'user' AS slack_user_id, payload->'event'->>'ts' AS message_ts,
      payload->'event'->>'text' AS text,
      CASE WHEN payload->'event'->>'ts' ~ '^[0-9]+[.][0-9]+$' THEN (payload->'event'->>'ts')::numeric END AS message_order
    FROM apis.slack_events
    WHERE EXISTS (
      SELECT 1 FROM apis.slack_identities identity
      JOIN projects.project_members member ON member.project_id = #{scope.projectId} AND member.user_id = identity.user_id
      JOIN users.users account ON account.id = identity.user_id
      JOIN projects.projects project ON project.id = member.project_id
      JOIN apis.slack installation ON installation.project_id = member.project_id AND installation.team_id = identity.team_id
      WHERE identity.team_id = #{scope.teamId} AND identity.slack_user_id = payload->'event'->>'user' AND member.user_id = #{scope.userId}|]
    <> Integrations.activeMembership
    <> [HI.sql|) AND processed_at IS NULL AND team_id = #{scope.teamId} AND payload->>'team_id' = #{scope.teamId}
      AND payload->'event'->>'channel' = #{scope.channelId}
      AND payload->'event'->>'thread_ts' = #{scope.threadTs}
      AND payload->'event'->>'type' IN ('message', 'app_mention', 'monoscope_message_shortcut')
      AND payload->'event'->>'subtype' IS NULL AND payload->'event'->>'bot_id' IS NULL
      AND NOT EXISTS (
        SELECT 1 FROM apis.investigation_journal journal
        WHERE journal.project_id = #{scope.projectId} AND journal.team_id = #{scope.teamId}
          AND journal.channel_id = #{scope.channelId} AND journal.thread_ts = #{scope.threadTs}
          AND journal.event->>'tag' = 'FollowupsAccepted'
          AND journal.event->'contents'->1 @> jsonb_build_array(jsonb_build_object('messageTs', payload->'event'->>'ts'))
      )
      AND CASE WHEN payload->'event'->>'ts' ~ '^[0-9]+[.][0-9]+$'
        THEN (payload->'event'->>'ts')::numeric > #{scope.messageTs}::numeric ELSE FALSE END
    ) SELECT DISTINCT ON (message_order) id, slack_user_id, message_ts, text
      FROM candidates ORDER BY message_order, id LIMIT 20|]


-- | A later Slack event for an already accepted message belongs to the original
-- investigation. Its journal entry commits with the checkpoint and receipt.
isAcceptedFollowup :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Text -> Eff es Bool
isAcceptedFollowup pid team channel thread messageTs = do
  accepted <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|SELECT TRUE FROM apis.investigation_journal
    WHERE project_id = #{pid} AND team_id = #{team} AND channel_id = #{channel} AND thread_ts = #{thread}
      AND event->>'tag' = 'FollowupsAccepted'
      AND event->'contents'->1 @> jsonb_build_array(jsonb_build_object('messageTs', #{messageTs}::text))
    LIMIT 1|]
  pure $ isJust accepted


data Event
  = InvestigationStarted Text Text
  | ModelStarted Int
  | ModelReturned Int ModelOutcome
  | ToolStarted Int LLM.ToolCall
  | ToolReturned Int LLM.ToolCall ToolResult
  | -- | Do not recursively embed the journal inside itself. The preceding
    -- ToolStarted event retains the call ID and arguments.
    InvestigationHistoryRead Int
  | FollowupsAccepted Int (NonEmpty Followup)
  | InvestigationFinished
  | InvestigationFailed
  | InvestigationInterrupted
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Event


data Entry = Entry
  { runId :: UUIDId "investigation_run"
  , messageTs :: Text
  , observedAt :: UTCTime
  , event :: Event
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


recordEvent :: DB es => Scope -> Event -> Eff es ()
recordEvent scope event = Hasql.interpExecute_ $ recordEventSql scope event


recordEventSql :: Scope -> Event -> HI.Sql
recordEventSql scope event =
  [HI.sql|INSERT INTO apis.investigation_journal
    (run_id, project_id, user_id, team_id, channel_id, thread_ts, message_ts, event)
    VALUES (#{scope.runId}, #{scope.projectId}, #{scope.userId}, #{scope.teamId},
      #{scope.channelId}, #{scope.threadTs}, #{scope.messageTs}, #{Aeson event})|]


data History = History {entries :: [Entry], limitReached :: Bool}
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON)


-- | The most recent fifty events, in chronological order. A full page may be
-- incomplete; model/tool outputs stay evidence rather than conversation roles.
recentEvents :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Eff es History
recentEvents pid team channel thread = readEvents pid team channel thread Nothing


readEvents :: DB es => Projects.ProjectId -> Text -> Text -> Text -> Maybe Text -> Eff es History
readEvents pid team channel thread messageTs = do
  rows <-
    Hasql.interp
      [HI.sql|SELECT run_id, message_ts, observed_at, event FROM apis.investigation_journal
      WHERE project_id = #{pid} AND team_id = #{team} AND channel_id = #{channel} AND thread_ts = #{thread}
        AND (#{messageTs}::text IS NULL OR message_ts = #{messageTs})
      ORDER BY id DESC LIMIT 50|]
  pure $ History (reverse rows) (length rows == 50)


-- | Information about a tool call made during agentic execution
data ToolCallInfo = ToolCallInfo
  { name :: Text
  , args :: Map.Map Text AE.Value
  , resultPreview :: Text
  , rawData :: Maybe AE.Value -- Structured query results for widget data reuse
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ToolCallInfo


-- | Result of an agentic chat with tool call history
data AgenticChatResult = AgenticChatResult
  { response :: Text
  , toolCalls :: [ToolCallInfo]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Turn = Turn
  { projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , userId :: Projects.UserId
  , messageTs :: Text
  }
  deriving stock (Show)


loadAnswer :: DB es => Turn -> Eff es (Maybe AgenticChatResult)
loadAnswer turn = do
  canonical <-
    Hasql.interpOne @SavedCheckpoint
      [HI.sql|SELECT checkpoint.revision, checkpoint.checkpoint FROM apis.slack_turn_checkpoints checkpoint
    JOIN apis.ai_routine_runs run ON run.root_run_id = checkpoint.run_id AND run.project_id = checkpoint.project_id
    JOIN apis.slack_events receipt ON receipt.id::text = run.source #>> '{contents,contents}'
    WHERE checkpoint.project_id = #{turn.projectId} AND checkpoint.conversation_id = #{turn.conversationId} AND checkpoint.user_id = #{turn.userId}
      AND run.source #>> '{contents,tag}' = 'SlackMessage' AND receipt.payload #>> '{event,ts}' = #{turn.messageTs}
    ORDER BY run.started_at DESC, run.id DESC LIMIT 1|]
  maybe (loadCheckpoint turn) (pure . Just) canonical >>= \case
    Just SavedCheckpoint{checkpoint = AnswerReady answer} -> pure $ Just answer
    Just SavedCheckpoint{checkpoint = LegacyUncertain answer _} -> pure $ Just answer
    _ ->
      Hasql.interpOneJson
        [HI.sql|SELECT answer FROM apis.slack_turn_answers
      WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
        AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}|]


-- | The checkpoint retains the answer; recording its message is replay-safe.
saveAnswer :: DB es => Turn -> AgenticChatResult -> Eff es AgenticChatResult
saveAnswer turn answer =
  Hasql.interpExecute_
    [HI.sql|INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, slack_message_ts, created_at)
    VALUES (#{turn.projectId}, #{turn.conversationId}, 'assistant', #{answer.response}, #{turn.messageTs}, clock_timestamp())
    ON CONFLICT (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL DO NOTHING|]
    $> answer


-- | Freeze every rendered part before the first send. A confirmed part is never
-- replayed by a later delivery attempt for this turn.
data ReplyBatch = ReplyBatch {replies :: NonEmpty AE.Value, deliveredCount :: Int}
  deriving stock (Generic, Show)
  deriving (AE.FromJSON, AE.ToJSON) via DAE.Snake ReplyBatch


loadReplyBatch :: DB es => Turn -> Eff es (Maybe ReplyBatch)
loadReplyBatch turn =
  Hasql.interpOneJson
    [HI.sql|SELECT to_jsonb(batch) FROM apis.slack_reply_batches batch
      WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
        AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}|]


saveReplyBatch :: DB es => Turn -> NonEmpty AE.Value -> Eff es ReplyBatch
saveReplyBatch turn replies =
  Hasql.interpOneJsonOrThrow
    "Saving a Slack reply batch"
    [HI.sql|WITH saved AS (
      INSERT INTO apis.slack_reply_batches (project_id, conversation_id, user_id, message_ts, replies)
      VALUES (#{turn.projectId}, #{turn.conversationId}, #{turn.userId}, #{turn.messageTs}, #{Aeson replies})
      ON CONFLICT (project_id, conversation_id, user_id, message_ts)
      DO UPDATE SET replies = slack_reply_batches.replies RETURNING *
    ) SELECT to_jsonb(saved) FROM saved|]


-- | Reserve before HTTP. An existing unacknowledged reservation must not repost.
claimReplyPart :: DB es => Turn -> Text -> Text -> Text -> Int -> Eff es (Maybe (UUIDId "slack_reply"))
claimReplyPart turn team channel thread part =
  fmap HI.getOneColumn
    <$> Hasql.interpOne
      [HI.sql|INSERT INTO apis.slack_reply_publications
      (project_id, conversation_id, user_id, message_ts, part, team_id, channel_id, thread_ts)
    SELECT project_id, conversation_id, user_id, message_ts, #{part}, #{team}, #{channel}, #{thread}
    FROM apis.slack_reply_batches WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
      AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}
      AND delivered_count = #{part} AND delivered_count < jsonb_array_length(replies)
    ON CONFLICT (project_id, conversation_id, user_id, message_ts, part) DO NOTHING
    RETURNING publication_id|]


-- | Acknowledgement and batch advancement are atomic. Repeated matching evidence
-- is harmless; a conflicting timestamp cannot change the delivery position.
confirmReplyPublication :: DB es => UUIDId "slack_reply" -> Text -> Eff es Bool
confirmReplyPublication publicationId timestamp = do
  confirmed <-
    Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|WITH confirmed AS (
      UPDATE apis.slack_reply_publications SET slack_ts = #{timestamp}
      WHERE publication_id = #{publicationId} AND (slack_ts IS NULL OR slack_ts = #{timestamp}) RETURNING *
    ), advanced AS (
      UPDATE apis.slack_reply_batches b SET delivered_count = delivered_count + 1 FROM confirmed p
      WHERE b.project_id = p.project_id AND b.conversation_id = p.conversation_id
        AND b.user_id = p.user_id AND b.message_ts = p.message_ts AND b.delivered_count = p.part
    ) SELECT TRUE FROM confirmed|]
  pure $ isJust confirmed


rejectReplyPublication :: DB es => UUIDId "slack_reply" -> Eff es ()
rejectReplyPublication publicationId =
  Hasql.interpExecute_
    [HI.sql|DELETE FROM apis.slack_reply_publications WHERE publication_id = #{publicationId} AND slack_ts IS NULL|]


-- | Only a signed observation from the configured app in the stored destination
-- can confirm a send whose HTTP acknowledgement was lost.
captureReplyPublication :: DB es => Text -> UUIDId "slack_event" -> Eff es ()
captureReplyPublication appId receiptId = do
  observed <-
    Hasql.interp @[(UUIDId "slack_reply", Text)]
      [HI.sql|SELECT p.publication_id, se.payload #>> '{event,ts}'
    FROM apis.slack_reply_publications p, apis.slack_events se, apis.slack s
    WHERE se.id = #{receiptId} AND #{appId} <> ''
      AND s.project_id = p.project_id AND s.team_id = p.team_id
      AND p.publication_id::text = se.payload #>> '{event,metadata,event_payload,publication_id}'
      AND se.team_id = p.team_id AND se.payload->>'api_app_id' = #{appId}
      AND se.payload #>> '{event,channel}' = p.channel_id
      AND se.payload #>> '{event,thread_ts}' = p.thread_ts
      AND se.payload #>> '{event,type}' = 'message'
      AND COALESCE(se.payload #>> '{event,subtype}', 'bot_message') = 'bot_message'
      AND COALESCE(se.payload #>> '{event,app_id}', se.payload #>> '{event,bot_profile,app_id}') = #{appId}
      AND COALESCE(se.payload #>> '{event,bot_id}', '') <> ''
      AND se.payload #>> '{event,metadata,event_type}' = 'monoscope_investigation_reply'
      AND (se.payload #>> '{event,ts}') ~ '^[0-9]+\.[0-9]+$'|]
  for_ observed $ \(publication, timestamp) -> void $ confirmReplyPublication publication timestamp


data PublicationSearch kind = PublicationSearch
  { publicationId :: UUIDId kind
  , teamId :: Text
  , channelId :: Text
  , threadTs :: Text
  , messageTs :: Text
  , cursor :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


loadReplySearch :: DB es => Turn -> Int -> Eff es (Maybe (PublicationSearch "slack_reply"))
loadReplySearch turn part =
  Hasql.interpOne
    [HI.sql|SELECT publication_id, team_id, channel_id, thread_ts, message_ts, history_cursor
    FROM apis.slack_reply_publications WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
      AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs} AND part = #{part} AND slack_ts IS NULL|]


saveReplySearchCursor :: DB es => PublicationSearch "slack_reply" -> Maybe Text -> Eff es ()
saveReplySearchCursor search next =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.slack_reply_publications SET history_cursor = #{next}
    WHERE publication_id = #{search.publicationId} AND slack_ts IS NULL
      AND history_cursor IS NOT DISTINCT FROM #{search.cursor}|]


-- | Exact model context and progress for one round. The API key is supplied by
-- the worker at execution time and is never part of the stored request.
data Step = Step
  { history :: LLM.ChatHistory
  , request :: OpenAIV1.CreateChatCompletion
  , iteration :: Int
  , timeRange :: (Maybe UTCTime, Maybe UTCTime)
  , toolCalls :: [ToolCallInfo]
  }
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)


data Checkpoint
  = ModelPending Step
  | ToolsPending Step LLM.Message [(LLM.ToolCall, ToolResult)] [LLM.ToolCall]
  | AnswerReady AgenticChatResult
  | LegacyUncertain AgenticChatResult Checkpoint
  | NativePending Operation.NativeOperation
  | NativeReady
  deriving stock (Generic, Show)
  deriving anyclass (AE.FromJSON, AE.ToJSON)
  deriving (HI.DecodeValue) via Aeson Checkpoint


data SavedCheckpoint = SavedCheckpoint {revision :: Int64, checkpoint :: Checkpoint}
  deriving stock (Generic, Show)
  deriving anyclass (AE.ToJSON, HI.DecodeRow)


data RunTurn = RunTurn
  { projectId :: Projects.ProjectId
  , conversationId :: UUIDId "conversation"
  , userId :: Projects.UserId
  , runId :: UUIDId "ai_routine_run"
  }
  deriving stock (Show)


data RunLease = RunLease {turn :: RunTurn, startedAt :: UTCTime}


data CheckpointTarget = SlackCheckpoint Turn | RunCheckpoint RunTurn
  deriving stock (Show)


data CheckpointCursor = CheckpointCursor {target :: CheckpointTarget, revision :: Int64}
  deriving stock (Show)


data CheckpointConflict = CheckpointConflict
  deriving stock (Generic, Show)
  deriving anyclass (Exception)


data CheckpointAccessDenied = CheckpointAccessDenied
  deriving stock (Generic, Show)
  deriving anyclass (Exception)


checkpointIdentity :: CheckpointTarget -> (Projects.ProjectId, UUIDId "conversation", Projects.UserId, Maybe Text, Maybe (UUIDId "ai_routine_run"))
checkpointIdentity = \case
  SlackCheckpoint turn -> (turn.projectId, turn.conversationId, turn.userId, Just turn.messageTs, Nothing)
  RunCheckpoint turn -> (turn.projectId, turn.conversationId, turn.userId, Nothing, Just turn.runId)


checkpointSql :: CheckpointTarget -> HI.Sql
checkpointSql target =
  let (project, conversation, user, message, run) = checkpointIdentity target
   in [HI.sql|SELECT revision, checkpoint FROM apis.slack_turn_checkpoints
      WHERE project_id = #{project} AND conversation_id = #{conversation} AND user_id = #{user}
        AND message_ts IS NOT DISTINCT FROM #{message} AND run_id IS NOT DISTINCT FROM #{run}|]


-- | A checkpoint revision identifies one pending tool position. An interrupted
-- retained invocation becomes explicitly uncertain; it is never claimed twice.
beginOperation :: (AE.FromJSON policy, AE.ToJSON policy, DB es) => RunLease -> CheckpointCursor -> OperationProposal policy -> Eff es OperationDecision
beginOperation lease cursor proposal = Hasql.transaction TxS.ReadCommitted TxS.Write (beginOperationTx lease cursor proposal) >>= either throwIO pure


beginOperationTx :: forall policy. (AE.FromJSON policy, AE.ToJSON policy) => RunLease -> CheckpointCursor -> OperationProposal policy -> Tx.Transaction (Either OperationConflict OperationDecision)
beginOperationTx lease cursor proposal = runExceptT do
  let turn = lease.turn
      (project, conversation, user, message, checkpointRun) = checkpointIdentity cursor.target
      identifier = operationIdentity cursor
      matchingScope = project == turn.projectId && conversation == turn.conversationId && user == turn.userId
  unless matchingScope $ hoistEither $ Left OperationScopeMismatch
  runs <-
    lift
      $ Hasql.queryTx @[HI.OneColumn (UUIDId "ai_routine_run")]
        [HI.sql|SELECT root_run_id FROM apis.ai_routine_runs WHERE id = #{turn.runId} AND project_id = #{project}
      AND conversation_id = #{conversation} AND requested_by = #{user} AND started_at = #{lease.startedAt} AND status = 'running' FOR UPDATE|]
  unless (case runs of [HI.OneColumn root] -> all (== root) checkpointRun; _ -> False) $ hoistEither $ Left OperationScopeMismatch
  checkpoints <-
    lift
      $ Hasql.queryTx @[HI.OneColumn Checkpoint]
        [HI.sql|SELECT checkpoint FROM apis.slack_turn_checkpoints
      WHERE project_id = #{project} AND conversation_id = #{conversation} AND user_id = #{user}
      AND message_ts IS NOT DISTINCT FROM #{message} AND run_id IS NOT DISTINCT FROM #{checkpointRun}
      AND revision = #{cursor.revision} FOR UPDATE|]
  when (null checkpoints) $ hoistEither $ Left OperationStepMoved
  unless
    ( case checkpoints of
        [HI.OneColumn (ToolsPending _ _ _ (expected : _))] -> Operation.ModelCall expected == proposal.call
        [HI.OneColumn (NativePending expected)] -> Operation.NativeCall expected == proposal.call
        _ -> False
    )
    $ hoistEither
    $ Left OperationProposalChanged
  let matchingCall = case proposal.call of
        Operation.ModelCall call -> [HI.sql|operation.proposal->'call'->'function' = #{Aeson $ LLM.toolCallFunction call}|]
        Operation.NativeCall (Operation.WriteWiki write) ->
          [HI.sql|operation.proposal #>> '{call,contents,tag}' = 'WriteWiki'
          AND operation.proposal #> '{call,contents,contents,edit}' = #{Aeson write.edit}|]
        Operation.NativeCall operation@(Operation.PublishPullRequestComment{}; Operation.EnrichIssue{}) ->
          [HI.sql|operation.proposal->'call' = #{Aeson $ Operation.NativeCall operation}|]
  prior <-
    lift
      $ Hasql.queryTx @[OperationEntry policy]
      $ operationsSql project turn.runId
      <> [HI.sql| AND (operation.id = #{identifier}
        OR operation.outcome->>'tag' IN ('OperationRunning', 'OperationUnknown')
        OR ^{matchingCall})
        ORDER BY operation.created_at, operation.id FOR UPDATE OF operation|]
  case find ((== identifier) . (.id)) prior of
    Just entry -> do
      unless (entry.proposal.call == proposal.call) $ hoistEither $ Left OperationProposalChanged
      reuse entry
    Nothing -> case find unresolved prior <|> find (Operation.sameOperation proposal.call . (.call) . (.proposal)) prior of
      Just entry -> reuse entry
      Nothing -> do
        inserted <-
          lift
            $ Hasql.queryTx @[UUIDId "agent_operation"]
              [HI.sql|INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome)
            VALUES (#{identifier}, #{project}, #{turn.runId}, #{user}, #{cursor.revision}, #{Aeson proposal}, #{Aeson OperationRunning})
            ON CONFLICT DO NOTHING RETURNING id|]
        case inserted of
          [_] -> pure $ ExecuteOperation $ OperationClaim identifier
          _ -> hoistEither $ Left OperationProposalChanged
  where
    unresolved entry = case entry.outcome of OperationRunning -> True; OperationUnknown -> True; OperationReturned{} -> False
    reuse :: OperationEntry policy -> ExceptT OperationConflict Tx.Transaction OperationDecision
    reuse entry = case entry.outcome of
      OperationReturned response -> pure $ RetainedOperation response
      OperationRunning -> do
        lift $ Hasql.executeTx [HI.sql|UPDATE apis.ai_agent_operations SET outcome = #{Aeson OperationUnknown}, updated_at = clock_timestamp() WHERE id = #{entry.id}|]
        pure $ ReconcileOperation (OperationClaim entry.id) entry.proposal.call entry.runId
      OperationUnknown -> pure $ ReconcileOperation (OperationClaim entry.id) entry.proposal.call entry.runId


-- | The mutation and its retained outcome commit together. A failed transaction
-- leaves no claimed operation, so a later worker can safely execute it.
atomicOperation :: (AE.FromJSON policy, AE.ToJSON policy, DB es) => RunLease -> CheckpointCursor -> OperationProposal policy -> Tx.Transaction ToolResult -> Eff es OperationResult
atomicOperation lease cursor proposal action = do
  result <- Hasql.transaction TxS.ReadCommitted TxS.Write do
    outcome <-
      beginOperationTx lease cursor proposal >>= \case
        Left conflict -> pure $ Left conflict
        Right (RetainedOperation retained) -> pure $ Right $ RecordedResult retained
        Right (ReconcileOperation claim original run) -> pure $ Right $ UncertainResult claim original run
        Right (ExecuteOperation claim) -> do
          response <- action
          fmap (RecordedResult response <$) $ completeOperationTx claim response
    when (isLeft outcome) Tx.condemn
    pure outcome
  either throwIO pure result


completeOperation :: DB es => OperationClaim -> ToolResult -> Eff es ()
completeOperation claim result = Hasql.transaction TxS.ReadCommitted TxS.Write (completeOperationTx claim result) >>= either throwIO pure


completeOperationTx :: OperationClaim -> ToolResult -> Tx.Transaction (Either OperationConflict ())
completeOperationTx (OperationClaim identifier) result = do
  changed <-
    Hasql.queryTx @[UUIDId "agent_operation"]
      [HI.sql|UPDATE apis.ai_agent_operations SET outcome = #{Aeson $ OperationReturned result}, updated_at = clock_timestamp()
      WHERE id = #{identifier} AND outcome->>'tag' IN ('OperationRunning', 'OperationUnknown') RETURNING id|]
  if not (null changed)
    then pure $ Right ()
    else do
      same <- Hasql.queryTx @[Bool] [HI.sql|SELECT outcome = #{Aeson $ OperationReturned result} FROM apis.ai_agent_operations WHERE id = #{identifier}|]
      pure $ if same == [True] then Right () else Left OperationOutcomeChanged


listOperations :: (AE.FromJSON policy, DB es) => Projects.ProjectId -> UUIDId "ai_routine_run" -> Eff es [OperationEntry policy]
listOperations project run = Hasql.interp $ operationsSql project run <> [HI.sql| ORDER BY operation.created_at DESC, operation.id DESC LIMIT 200|]


operationsSql :: Projects.ProjectId -> UUIDId "ai_routine_run" -> HI.Sql
operationsSql project run =
  [HI.sql|SELECT operation.id, operation.run_id, operation.requested_by, operation.proposal, operation.outcome, operation.created_at
    FROM apis.ai_agent_operations operation
    JOIN apis.ai_routine_runs origin ON origin.id = operation.run_id
    JOIN apis.ai_routine_runs selected ON selected.id = #{run} AND selected.project_id = #{project}
    WHERE operation.project_id = #{project} AND (origin.root_run_id = selected.root_run_id
      OR (selected.source->>'tag' = 'TriggeredRun' AND origin.source = selected.source
        AND operation.outcome->>'tag' IN ('OperationRunning', 'OperationUnknown')))|]


loadCheckpoint :: DB es => Turn -> Eff es (Maybe SavedCheckpoint)
loadCheckpoint = loadCheckpointFor . SlackCheckpoint


loadCheckpointFor :: DB es => CheckpointTarget -> Eff es (Maybe SavedCheckpoint)
loadCheckpointFor = Hasql.interpOne . checkpointSql


startCheckpoint :: DB es => Turn -> Checkpoint -> Eff es SavedCheckpoint
startCheckpoint = startCheckpointFor . SlackCheckpoint


prepareNativeOperation :: DB es => RunTurn -> Operation.NativeOperation -> Eff es (CheckpointCursor, Operation.NativeOperation)
prepareNativeOperation turn proposed = do
  let target = RunCheckpoint turn
  saved <- loadCheckpointFor target >>= maybe (startCheckpointFor target $ NativePending proposed) pure
  let cursor = CheckpointCursor target saved.revision
  case saved.checkpoint of
    NativePending retained -> pure (cursor, retained)
    NativeReady -> do
      next <- commitProgress (Just cursor) Nothing (NativePending proposed) Nothing
      maybe (throwIO CheckpointAccessDenied) (\position -> pure (position, proposed)) next
    (ModelPending{}; ToolsPending{}; AnswerReady{}; LegacyUncertain{}) -> throwIO CheckpointAccessDenied


operationIdentity :: CheckpointCursor -> UUIDId "agent_operation"
operationIdentity cursor = UUIDId $ UUID5.generateNamed UUID5.namespaceOID $ BS.unpack $ toStrict $ AE.encode (checkpointIdentity cursor.target, cursor.revision)


-- | Adopt the historical message checkpoint under its original conservative grant.
-- A pending mutation without a retained response cannot be replayed.
adoptSlackCheckpoint :: forall policy es. (AE.FromJSON policy, AE.ToJSON policy, DB es) => RunLease -> Turn -> (Catalog.AgentPolicy -> policy) -> Eff es (Maybe SavedCheckpoint)
adoptSlackCheckpoint lease turn policy = do
  unless (lease.turn.projectId == turn.projectId && lease.turn.conversationId == turn.conversationId && lease.turn.userId == turn.userId) $ throwIO CheckpointAccessDenied
  result <- Hasql.transaction TxS.ReadCommitted TxS.Write do
    outcome <- runExceptT do
      roots <-
        lift
          $ Hasql.queryTx @[(UUIDId "ai_routine_run", Bool)]
            [HI.sql|SELECT run.root_run_id, run.policy = 'observe' FROM apis.ai_routine_runs run
        JOIN apis.ai_routine_runs root ON root.id = run.root_run_id
        JOIN apis.slack_events receipt ON receipt.id::text = run.source #>> '{contents,contents}'
        WHERE run.id = #{lease.turn.runId} AND run.project_id = #{turn.projectId} AND run.conversation_id = #{turn.conversationId}
          AND run.requested_by = #{turn.userId} AND run.started_at = #{lease.startedAt} AND run.status = 'running'
          AND run.source #>> '{contents,tag}' = 'SlackMessage' AND receipt.payload #>> '{event,ts}' = #{turn.messageTs}
        FOR UPDATE OF root|]
      (root, readOnly) <- case roots of [bound] -> pure bound; _ -> hoistEither $ Left CheckpointAccessDenied
      legacy <- lift $ Hasql.queryTx @[SavedCheckpoint] $ checkpointSql (SlackCheckpoint turn) <> [HI.sql| FOR UPDATE|]
      historical <-
        lift
          $ Hasql.queryTx @[HI.OneColumn (Aeson AgenticChatResult)]
            [HI.sql|SELECT answer FROM apis.slack_turn_answers WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
          AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs}|]
      let saved = listToMaybe legacy <|> (\(HI.OneColumn (Aeson answer)) -> SavedCheckpoint 0 $ AnswerReady answer) <$> listToMaybe historical
          target = RunCheckpoint lease.turn{runId = root}
      forM saved \original -> do
        unless readOnly $ hoistEither $ Left CheckpointAccessDenied
        revision <-
          lift
            $ Hasql.oneTx @(HI.OneColumn Int64)
              [HI.sql|SELECT greatest(#{original.revision}, coalesce(max(operation.checkpoint_revision) + 1, 0))
          FROM apis.ai_agent_operations operation JOIN apis.ai_routine_runs run ON run.id = operation.run_id WHERE run.root_run_id = #{root}|]
        let cursor = CheckpointCursor target revision.getOneColumn
        checkpoint <- case original.checkpoint of
          ToolsPending step message completed (call : pending) -> do
            let capability = fromMaybe (Catalog.Administration ProjectMembers.PAdmin) $ join $ Map.lookup (LLM.toolFunctionName call.toolCallFunction) Catalog.policiesByName
                uncertain identifier run =
                  let result = operationResponse turn.projectId $ UncertainResult (OperationClaim identifier) (Operation.ModelCall call) run
                   in LegacyUncertain (AgenticChatResult result.formatted $ step.toolCalls <> map (uncurry mkToolCallInfo) completed <> [mkToolCallInfo call result]) original.checkpoint
            case capability of
              Catalog.ReadOnly{} -> pure original.checkpoint
              _ -> do
                prior <-
                  lift
                    $ Hasql.queryTx @[OperationEntry policy]
                    $ operationsSql turn.projectId lease.turn.runId
                    <> [HI.sql| AND operation.id = #{operationIdentity $ CheckpointCursor (SlackCheckpoint turn) original.revision} FOR UPDATE OF operation|]
                case prior of
                  entry : _ -> do
                    unless (entry.proposal.call == Operation.ModelCall call) $ hoistEither $ Left CheckpointAccessDenied
                    pure $ case entry.outcome of
                      OperationReturned result -> ToolsPending step message (completed <> [(call, result)]) pending
                      (OperationRunning; OperationUnknown) -> uncertain entry.id entry.runId
                  [] -> do
                    let identifier = operationIdentity cursor
                        proposal = OperationProposal (Operation.ModelCall call) $ policy capability
                    lift
                      $ Hasql.executeTx
                        [HI.sql|INSERT INTO apis.ai_agent_operations (id, project_id, run_id, requested_by, checkpoint_revision, proposal, outcome)
                    VALUES (#{identifier}, #{turn.projectId}, #{lease.turn.runId}, #{turn.userId}, #{cursor.revision}, #{Aeson proposal}, #{Aeson OperationUnknown})|]
                    pure $ uncertain identifier lease.turn.runId
          (ModelPending{}; ToolsPending{}; AnswerReady{}; LegacyUncertain{}) -> pure original.checkpoint
          (NativePending{}; NativeReady) -> hoistEither $ Left CheckpointAccessDenied
        created <-
          lift
            $ Hasql.oneTx @SavedCheckpoint
              [HI.sql|INSERT INTO apis.slack_turn_checkpoints (project_id, conversation_id, user_id, run_id, revision, checkpoint)
          VALUES (#{turn.projectId}, #{turn.conversationId}, #{turn.userId}, #{root}, #{cursor.revision}, #{Aeson checkpoint})
          ON CONFLICT (project_id, conversation_id, user_id, message_ts, run_id)
          DO UPDATE SET checkpoint = slack_turn_checkpoints.checkpoint RETURNING revision, checkpoint|]
        lift
          $ Hasql.executeTx
            [HI.sql|DELETE FROM apis.slack_turn_checkpoints WHERE project_id = #{turn.projectId} AND conversation_id = #{turn.conversationId}
          AND user_id = #{turn.userId} AND message_ts = #{turn.messageTs} AND run_id IS NULL|]
        pure created
    when (isLeft outcome) Tx.condemn
    pure outcome
  either throwIO pure result


-- | The run lock serializes initialization only. Model calls never hold it.
-- Event input and the initial checkpoint commit together, once per request.
startCheckpointFor :: DB es => CheckpointTarget -> Checkpoint -> Eff es SavedCheckpoint
startCheckpointFor target checkpoint = do
  let (project, conversation, user, message, run) = checkpointIdentity target
  result <- Hasql.transaction TxS.ReadCommitted TxS.Write do
    permitted <- case run of
      Nothing -> pure True
      Just identifier ->
        not
          . null
          <$> Hasql.queryTx @[UUIDId "ai_routine_run"]
            [HI.sql|SELECT id FROM apis.ai_routine_runs
          WHERE id = #{identifier} AND project_id = #{project} AND conversation_id = #{conversation}
            AND requested_by = #{user} FOR UPDATE|]
    if not permitted
      then pure $ Left CheckpointAccessDenied
      else do
        saved <- Hasql.queryTx @[SavedCheckpoint] $ checkpointSql target
        case saved of
          existing : _ -> pure $ Right existing
          [] -> do
            created <-
              Hasql.oneTx
                [HI.sql|INSERT INTO apis.slack_turn_checkpoints (project_id, conversation_id, user_id, message_ts, run_id, checkpoint, revision)
              SELECT #{project}, #{conversation}, #{user}, #{message}, #{run}, #{Aeson checkpoint}, coalesce(max(operation.checkpoint_revision) + 1, 0)
              FROM apis.ai_agent_operations operation JOIN apis.ai_routine_runs origin ON origin.id = operation.run_id
              WHERE origin.root_run_id = #{run}
              ON CONFLICT (project_id, conversation_id, user_id, message_ts, run_id)
              DO UPDATE SET checkpoint = slack_turn_checkpoints.checkpoint RETURNING revision, checkpoint|]
            case checkpoint of
              (NativePending{}; NativeReady) -> pass
              (ModelPending{}; ToolsPending{}; AnswerReady{}; LegacyUncertain{}) -> for_ run $ \identifier ->
                Hasql.executeTx
                  [HI.sql|INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, slack_message_ts, created_at)
              SELECT run.project_id, run.conversation_id, 'user', run.input, receipt.payload #>> '{event,ts}', clock_timestamp()
              FROM apis.ai_routine_runs run LEFT JOIN apis.slack_events receipt
                ON receipt.id = CASE WHEN run.source #>> '{contents,tag}' = 'SlackMessage' THEN (run.source #>> '{contents,contents}')::uuid END
              WHERE run.id = #{identifier} AND run.input IS NOT NULL AND run.source <> '{"tag":"ScheduledRun"}'::jsonb
              ON CONFLICT (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL DO NOTHING|]
            pure $ Right created
  either throwIO pure result


-- | A stale worker cannot replace newer progress. Updating the checkpoint and
-- appending its activity event commit together. Accepted follow-ups also save
-- their conversation messages and receipt completion in this transaction.
commitProgress :: DB es => Maybe CheckpointCursor -> Maybe Scope -> Checkpoint -> Maybe Event -> Eff es (Maybe CheckpointCursor)
commitProgress cursor scope checkpoint event
  | isNothing cursor && (isNothing scope || isNothing event) = pure Nothing
  | otherwise = do
      result <- Hasql.transaction TxS.ReadCommitted TxS.Write do
        updated <- case cursor of
          Nothing -> pure $ Right Nothing
          Just key -> do
            let (project, conversation, user, message, run) = checkpointIdentity key.target
            versions <-
              Hasql.queryTx @[Int64]
                [HI.sql|UPDATE apis.slack_turn_checkpoints SET checkpoint = #{Aeson checkpoint},
                revision = revision + 1, updated_at = clock_timestamp()
                WHERE project_id = #{project} AND conversation_id = #{conversation} AND user_id = #{user}
                  AND message_ts IS NOT DISTINCT FROM #{message} AND run_id IS NOT DISTINCT FROM #{run}
                  AND revision = #{key.revision} RETURNING revision|]
            pure $ case versions of
              [version] -> Right $ Just (key{revision = version} :: CheckpointCursor)
              _ -> Left CheckpointConflict
        for_ (rightToMaybe updated) $ \_ -> for_ scope $ \key -> for_ event $ \activity -> do
          Hasql.executeTx $ recordEventSql key activity
          case (cursor, activity) of
            (Just turnCursor, FollowupsAccepted _ followups) -> for_ followups $ \followup -> do
              let (project, conversation, _, _, _) = checkpointIdentity turnCursor.target
              Hasql.executeTx
                [HI.sql|WITH accepted AS (
                  UPDATE apis.slack_events SET processed_at = clock_timestamp()
                  WHERE team_id = #{key.teamId} AND processed_at IS NULL
                    AND payload->'event'->>'channel' = #{key.channelId}
                    AND payload->'event'->>'thread_ts' = #{key.threadTs}
                    AND payload->'event'->>'ts' = #{followup.messageTs}
                    AND payload->'event'->>'user' = #{followup.slackUserId}
                    AND payload->'event'->>'type' IN ('message', 'app_mention', 'monoscope_message_shortcut')
                    AND payload->'event'->>'subtype' IS NULL AND payload->'event'->>'bot_id' IS NULL
                  RETURNING id
                ) INSERT INTO apis.ai_chat_messages (project_id, conversation_id, role, content, slack_message_ts, created_at)
                  SELECT #{project}, #{conversation}, 'user', #{followup.text}, #{followup.messageTs}, clock_timestamp()
                  WHERE EXISTS (SELECT 1 FROM accepted)
                  ON CONFLICT (project_id, conversation_id, slack_message_ts, role) WHERE slack_message_ts IS NOT NULL DO NOTHING|]
            _ -> pass
        pure updated
      either throwIO pure result


-- | A separate identity for a Slack turn's progress publication. A row without
-- a timestamp is awaiting acknowledgement or a signed observation; do not repost.
data ProgressTarget = ProgressTarget
  { projectId :: Projects.ProjectId
  , teamId :: Text
  , channelId :: Text
  , threadTs :: Text
  , messageTs :: Text
  , userId :: Projects.UserId
  , slackUserId :: Text
  }
  deriving stock (Generic, Show)
  deriving (AE.FromJSON) via DAE.Snake ProgressTarget


recentProgressEvents :: DB es => ProgressTarget -> Eff es History
recentProgressEvents target = readEvents target.projectId target.teamId target.channelId target.threadTs (Just target.messageTs)


data ProgressMessage = ProgressMessage {publicationId :: UUIDId "slack_progress", timestamp :: Maybe Text}
  deriving stock (Generic, Show)
  deriving anyclass (HI.DecodeRow)


loadProgress :: DB es => ProgressTarget -> Eff es (Maybe ProgressMessage)
loadProgress target =
  Hasql.interpOne
    [HI.sql|SELECT publication_id, progress_ts FROM apis.slack_investigation_progress
    WHERE project_id = #{target.projectId} AND team_id = #{target.teamId}
      AND channel_id = #{target.channelId} AND thread_ts = #{target.threadTs} AND message_ts = #{target.messageTs}|]


loadProgressRefresh :: DB es => UUIDId "slack_progress" -> Eff es (Maybe (ProgressTarget, Maybe Text))
loadProgressRefresh publicationId =
  fmap (\(Aeson target, timestamp) -> (target, timestamp))
    <$> Hasql.interpOne
      [HI.sql|SELECT to_jsonb(p), progress_ts FROM apis.slack_investigation_progress p
      WHERE publication_id = #{publicationId} AND user_id IS NOT NULL AND slack_user_id IS NOT NULL|]


loadProgressSearch :: DB es => UUIDId "slack_progress" -> Eff es (Maybe (PublicationSearch "slack_progress"))
loadProgressSearch publicationId =
  Hasql.interpOne
    [HI.sql|SELECT publication_id, team_id, channel_id, thread_ts, message_ts, history_cursor
    FROM apis.slack_investigation_progress WHERE publication_id = #{publicationId} AND progress_ts IS NULL|]


saveProgressSearchCursor :: DB es => PublicationSearch "slack_progress" -> Maybe Text -> Eff es ()
saveProgressSearchCursor search next =
  Hasql.interpExecute_
    [HI.sql|UPDATE apis.slack_investigation_progress SET history_cursor = #{next}
    WHERE publication_id = #{search.publicationId} AND progress_ts IS NULL
      AND history_cursor IS NOT DISTINCT FROM #{search.cursor}|]


claimProgress :: DB es => ProgressTarget -> Eff es (Maybe ProgressMessage)
claimProgress target =
  Hasql.interpOne
    [HI.sql|INSERT INTO apis.slack_investigation_progress
    (project_id, team_id, channel_id, thread_ts, message_ts, user_id, slack_user_id)
    VALUES (#{target.projectId}, #{target.teamId}, #{target.channelId}, #{target.threadTs}, #{target.messageTs}, #{target.userId}, #{target.slackUserId})
    ON CONFLICT (project_id, team_id, channel_id, thread_ts, message_ts) DO NOTHING
    RETURNING publication_id, progress_ts|]


confirmProgress :: DB es => UUIDId "slack_progress" -> Text -> Eff es Bool
confirmProgress publicationId timestamp =
  isJust
    <$> Hasql.interpOne @(HI.OneColumn Bool)
      [HI.sql|UPDATE apis.slack_investigation_progress SET progress_ts = #{timestamp}
    WHERE publication_id = #{publicationId} AND (progress_ts IS NULL OR progress_ts = #{timestamp}) RETURNING TRUE|]


-- | Release only a definitely rejected publication. A concurrent signed
-- observation wins over the rejection and must not be deleted.
rejectProgress :: DB es => UUIDId "slack_progress" -> Eff es ()
rejectProgress publicationId =
  Hasql.interpExecute_
    [HI.sql|DELETE FROM apis.slack_investigation_progress
    WHERE publication_id = #{publicationId} AND progress_ts IS NULL|]


-- | Mirror incident-root reconciliation: require a signed stored receipt and
-- verify both the receiving app and message author before accepting its scope.
captureProgress :: DB es => Text -> UUIDId "slack_event" -> Eff es [(ProgressTarget, Text)]
captureProgress appId receiptId =
  map (\(Aeson target, timestamp) -> (target, timestamp))
    <$> Hasql.interp
      [HI.sql|UPDATE apis.slack_investigation_progress p SET progress_ts = se.payload #>> '{event,ts}'
    FROM apis.slack_events se, apis.slack s
    WHERE se.id = #{receiptId} AND #{appId} <> ''
      AND p.user_id IS NOT NULL AND p.slack_user_id IS NOT NULL
      AND s.project_id = p.project_id AND s.team_id = p.team_id
      AND p.publication_id::text = se.payload #>> '{event,metadata,event_payload,publication_id}'
      AND se.team_id = p.team_id AND se.payload->>'api_app_id' = #{appId}
      AND se.payload #>> '{event,channel}' = p.channel_id
      AND se.payload #>> '{event,thread_ts}' = p.thread_ts
      AND se.payload #>> '{event,type}' = 'message'
      AND COALESCE(se.payload #>> '{event,subtype}', 'bot_message') = 'bot_message'
      AND COALESCE(se.payload #>> '{event,app_id}', se.payload #>> '{event,bot_profile,app_id}') = #{appId}
      AND COALESCE(se.payload #>> '{event,bot_id}', '') <> ''
      AND se.payload #>> '{event,metadata,event_type}' = 'monoscope_investigation_progress'
      AND (se.payload #>> '{event,ts}') ~ '^[0-9]+\.[0-9]+$'
      AND (p.progress_ts IS NULL OR p.progress_ts = se.payload #>> '{event,ts}')
    RETURNING to_jsonb(p), p.progress_ts|]


-- | A conversation and its retained runs share the strongest evidence requirement.
conversationEvidencePermission :: DB es => Projects.ProjectId -> UUIDId "conversation" -> Eff es ProjectMembers.Permissions
conversationEvidencePermission project conversation = do
  classifyLegacyEvidence project (Just conversation)
  Hasql.interpScalar
    [HI.sql|SELECT COALESCE(apis.conversation_evidence_permission(#{project}, #{conversation}), 'view'::projects.project_permissions)|]


-- | Locks match completion's run-before-conversation order. Updates are monotonic.
retainEvidencePermission :: DB es => Projects.ProjectId -> [UUIDId "conversation"] -> ProjectMembers.Permissions -> Eff es ()
retainEvidencePermission project conversations permission = unless (null conversations) $ Hasql.transaction TxS.ReadCommitted TxS.Write do
  void
    $ Hasql.queryTx @[UUIDId "ai_routine_run"]
      [HI.sql|SELECT id FROM apis.ai_routine_runs WHERE project_id = #{project} AND conversation_id = ANY(#{conversations})
      AND (evidence_permission IS NULL OR apis.evidence_permission_rank(evidence_permission) < apis.evidence_permission_rank(#{permission})) ORDER BY id FOR UPDATE|]
  Hasql.executeTx
    [HI.sql|UPDATE apis.ai_routine_runs SET evidence_permission = #{permission}
    WHERE project_id = #{project} AND conversation_id = ANY(#{conversations})
      AND (evidence_permission IS NULL OR apis.evidence_permission_rank(evidence_permission) < apis.evidence_permission_rank(#{permission}))|]
  Hasql.executeTx
    [HI.sql|UPDATE apis.ai_conversations SET evidence_permission = #{permission}
    WHERE project_id = #{project} AND conversation_id = ANY(#{conversations})
      AND (evidence_permission IS NULL OR apis.evidence_permission_rank(evidence_permission) < apis.evidence_permission_rank(#{permission}))|]


-- | Classify historical tool evidence and copied instructions through the shared catalog.
-- Unknown/malformed historical evidence remains administrator-only.
classifyLegacyEvidence :: DB es => Projects.ProjectId -> Maybe (UUIDId "conversation") -> Eff es ()
classifyLegacyEvidence project selected = do
  pending <-
    Hasql.interp @[UUIDId "conversation"]
      [HI.sql|WITH RECURSIVE subjects(conversation_id) AS (
      SELECT conversation_id FROM (
        SELECT conversation_id FROM apis.ai_conversations WHERE project_id = #{project} AND evidence_permission IS NULL
        UNION SELECT conversation_id FROM apis.ai_routine_runs WHERE project_id = #{project} AND evidence_permission IS NULL
      ) legacy WHERE #{selected} IS NULL OR conversation_id = #{selected}
      UNION SELECT routine.conversation_id FROM subjects
        JOIN apis.ai_routine_runs run ON run.project_id = #{project} AND run.conversation_id = subjects.conversation_id
        JOIN apis.ai_routines routine ON routine.id = run.routine_id AND routine.project_id = #{project}
    ) SELECT conversation_id FROM subjects|]
  unless (null pending) do
    existing <-
      Hasql.interp @[(UUIDId "conversation", ProjectMembers.Permissions)]
        [HI.sql|SELECT conversation_id, evidence_permission FROM apis.ai_conversations
        WHERE project_id = #{project} AND conversation_id = ANY(#{pending}) AND evidence_permission IS NOT NULL
        UNION ALL SELECT conversation_id, evidence_permission FROM apis.ai_routine_runs
        WHERE project_id = #{project} AND conversation_id = ANY(#{pending}) AND evidence_permission IS NOT NULL|]
    sources <-
      Hasql.interp @[(UUIDId "conversation", Maybe (UUIDId "conversation"))]
        [HI.sql|SELECT DISTINCT run.conversation_id, routine.conversation_id FROM apis.ai_routine_runs run
        LEFT JOIN apis.ai_routines routine ON routine.id = run.routine_id AND routine.project_id = #{project}
        WHERE run.project_id = #{project} AND run.conversation_id = ANY(#{pending}) AND run.source->>'tag' <> 'InteractiveRun'|]
    calls <-
      Hasql.interp @[(UUIDId "conversation", Aeson AE.Value)]
        [HI.sql|SELECT conversation_id, metadata FROM apis.ai_chat_messages
        WHERE project_id = #{project} AND conversation_id = ANY(#{pending}) AND role = 'assistant' AND metadata IS NOT NULL
        UNION ALL SELECT conversation_id, actions FROM apis.ai_routine_runs
        WHERE project_id = #{project} AND conversation_id = ANY(#{pending}) AND actions IS NOT NULL|]
    checkpoints <-
      Hasql.interp @[(UUIDId "conversation", Aeson AE.Value)]
        [HI.sql|SELECT conversation_id, checkpoint FROM apis.slack_turn_checkpoints
        WHERE project_id = #{project} AND conversation_id = ANY(#{pending})|]
    operations <-
      Hasql.interp @[(UUIDId "conversation", Aeson AE.Value, Aeson AE.Value)]
        [HI.sql|SELECT run.conversation_id, COALESCE(operation.proposal->'call', 'null'::jsonb),
          COALESCE(operation.proposal #> '{policy,capability}', 'null'::jsonb)
        FROM apis.ai_agent_operations operation JOIN apis.ai_routine_runs run ON run.id = operation.run_id
        WHERE run.project_id = #{project} AND run.conversation_id = ANY(#{pending})|]
    let policies =
          Map.fromListWith max
            $ [(conversation, ProjectMembers.PView) | conversation <- pending]
            <> existing
            <> [(conversation, parsed (toolsPermission . (map (.name) :: [ToolCallInfo] -> [Text])) payload) | (conversation, Aeson payload) <- calls]
            <> [(conversation, parsed (toolsPermission . checkpointNames) payload) | (conversation, Aeson payload) <- checkpoints]
            <> [(conversation, max (parsed (toolPermission . Catalog.operationName) call) (parsed Catalog.disclosurePermission capability)) | (conversation, Aeson call, Aeson capability) <- operations]
        inherit roles = Map.unionWith max roles $ Map.fromListWith max [(target, maybe ProjectMembers.PAdmin (\source -> Map.findWithDefault ProjectMembers.PAdmin source roles) origin) | (target, origin) <- sources]
        resolved = until (\roles -> inherit roles == roles) inherit policies
    for_ (Map.toList $ Map.fromListWith (<>) [(role, [conversation]) | (conversation, role) <- Map.toList resolved]) $ \(role, conversations) -> retainEvidencePermission project conversations role
  where
    parsed :: AE.FromJSON value => (value -> ProjectMembers.Permissions) -> AE.Value -> ProjectMembers.Permissions
    parsed permission value = case AE.fromJSON value of
      AE.Error _ -> ProjectMembers.PAdmin
      AE.Success stored -> permission stored
    toolsPermission = foldl' max ProjectMembers.PView . map toolPermission
    toolPermission name = maybe ProjectMembers.PAdmin Catalog.disclosurePermission $ join $ Map.lookup name Catalog.policiesByName
    checkpointNames = \case
      AnswerReady answer -> map (.name) answer.toolCalls
      ModelPending step -> map (.name) step.toolCalls
      ToolsPending step _ completed pending -> map (.name) step.toolCalls <> map (LLM.toolFunctionName . LLM.toolCallFunction) (map fst completed <> pending)
      NativePending operation -> [Catalog.operationName $ Operation.NativeCall operation]
      NativeReady -> []
      LegacyUncertain _ original -> checkpointNames original


visibleConversationIds :: DB es => Projects.ProjectId -> ProjectMembers.Permissions -> Eff es [UUIDId "conversation"]
visibleConversationIds project permission = do
  classifyLegacyEvidence project Nothing
  Hasql.interp
    [HI.sql|SELECT conversation_id FROM (
        SELECT conversation_id, evidence_permission FROM apis.ai_conversations WHERE project_id = #{project}
        UNION ALL SELECT conversation_id, evidence_permission FROM apis.ai_routine_runs WHERE project_id = #{project}
      ) scopes GROUP BY conversation_id
      HAVING max(apis.evidence_permission_rank(evidence_permission)) <= apis.evidence_permission_rank(#{permission})|]


operationResponse :: Projects.ProjectId -> OperationResult -> ToolResult
operationResponse project = \case
  RecordedResult result -> result
  UncertainResult claim _ run ->
    let identifier = operationId claim
     in ToolResult
          ("This request has an action with an uncertain outcome. It may still complete. Further changes were not executed. [View action history](/p/" <> project.toText <> "/ai/runs/" <> run.toText <> "#operation-" <> identifier.toText <> ") before making another request.")
          (Just $ AE.object ["operation_id" AE..= identifier, "run_id" AE..= run, "outcome" AE..= OperationUnknown])


-- | Create ToolCallInfo from a tool call and its result
mkToolCallInfo :: LLM.ToolCall -> ToolResult -> ToolCallInfo
mkToolCallInfo tc result =
  ToolCallInfo
    { name = LLM.toolFunctionName (LLM.toolCallFunction tc)
    , args = LLM.toolFunctionArguments (LLM.toolCallFunction tc)
    , resultPreview = T.take 2000 result.formatted
    , rawData = result.rawData
    }
