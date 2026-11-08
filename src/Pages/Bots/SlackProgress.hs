{-# LANGUAGE OrPatterns #-}

module Pages.Bots.SlackProgress (Snapshot (..), Task (..), TaskStatus (..), ReviewNotice (..), snapshot, endReview, payload, streamFrame, streamChunks, reviewDecision) where

import Data.Aeson qualified as AE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Langchain.LLM.Core qualified as LLM
import Models.Apis.Investigations (Task (..), TaskStatus (..))
import Models.Apis.Investigations qualified as Investigations
import Models.Projects.Projects qualified as Projects
import Pkg.DeriveUtils (UUIDId, encodeEnumSC)
import Relude
import Utils (fmtDate, hostPath, truncateText)
import Web.HttpApiData (parseUrlPiece)
import Web.Links (CookieProtectedRoutes (..), urls)


-- $setup
-- >>> import Data.UUID qualified as UUID
-- >>> import Data.Time (UTCTime(..), fromGregorian)
-- >>> import Pkg.DeriveUtils (UUIDId(..))
-- >>> let activity :: Investigations.Event -> Investigations.Entry; activity = Investigations.Entry (UUIDId $ UUID.fromWords 0 0 0 1) "1.0" (UTCTime (fromGregorian 2025 1 1) 0)


data TaskKind = Decision | HistoryRead | FollowupRead | Evidence Text
  deriving stock (Eq, Show)


data ReviewNotice = PendingNotice (UUIDId "agent_operation") UTCTime | EndedNotice (UUIDId "agent_operation")
  deriving stock (Eq, Show)


data Snapshot = Snapshot {title :: Text, tasks :: [Task], observedAt :: UTCTime, limited :: Bool, review :: Maybe ReviewNotice}
  deriving stock (Eq, Show)


-- | Project recorded activity, never model reasoning, arguments or tool output.
-- A completed task means its response was recorded, not that a cause was proved.
--
-- >>> let history = Investigations.History (map activity [Investigations.ModelStarted 0, Investigations.InvestigationInterrupted]) False
-- >>> fmap (map (.status) . (.tasks)) $ snapshot "1.0" history
-- Just [Error]
-- >>> snapshot "2.0" history
-- Nothing
-- >>> AE.encode (Task "check" "Read incident" InProgress)
-- "{\"status\":\"in_progress\",\"task_id\":\"check\",\"title\":\"Read incident\"}"
-- >>> let reply = Investigations.Followup (UUIDId $ UUID.fromWords 0 0 0 2) "U1" "1.1" "Private hypothesis"
-- >>> let replies = Investigations.History [activity $ Investigations.FollowupsAccepted 0 (reply :| [])] False
-- >>> fmap (map (\task -> (task.title, task.status)) . (.tasks)) $ snapshot "1.0" replies
-- Just [("Read new thread replies",Complete)]
snapshot :: Text -> Investigations.History -> Maybe Snapshot
snapshot messageTs history = do
  entries <- nonEmpty $ filter ((== messageTs) . (.messageTs)) history.entries
  let latest = last entries
      tasks = map snd $ foldl' update [] entries
      title = case latest.event of
        Investigations.InvestigationFinished -> "Investigation response ready"
        Investigations.InvestigationFailed -> "Investigation could not finish"
        Investigations.InvestigationInterrupted -> "Investigation interrupted"
        Investigations.InvestigationAwaitingReview{} -> "Awaiting action review"
        (Investigations.InvestigationStarted{}; Investigations.ModelStarted{}; Investigations.ModelReturned{}; Investigations.ToolStarted{}; Investigations.ToolReturned{}; Investigations.InvestigationHistoryRead{}; Investigations.FollowupsAccepted{}) -> "Investigating"
  guard $ not $ null tasks
  pure $ Snapshot title tasks latest.observedAt history.limitReached $ case latest.event of
    Investigations.InvestigationAwaitingReview identifier expiresAt -> Just (PendingNotice identifier expiresAt)
    (Investigations.InvestigationStarted{}; Investigations.ModelStarted{}; Investigations.ModelReturned{}; Investigations.ToolStarted{}; Investigations.ToolReturned{}; Investigations.InvestigationHistoryRead{}; Investigations.FollowupsAccepted{}; Investigations.InvestigationFinished; Investigations.InvestigationFailed; Investigations.InvestigationInterrupted) -> Nothing
  where
    update prior entry =
      let tasks = case entry.event of
            Investigations.ToolReturned _ tool _ -> map (\(taskKey@(_, _, kind), task) -> (taskKey, if kind == toolKind tool && task.status == Pending then task{status = Complete} else task)) prior
            (Investigations.InvestigationStarted{}; Investigations.ModelStarted{}; Investigations.ModelReturned{}; Investigations.ToolStarted{}; Investigations.InvestigationHistoryRead{}; Investigations.FollowupsAccepted{}; Investigations.InvestigationAwaitingReview{}; Investigations.InvestigationFinished; Investigations.InvestigationFailed; Investigations.InvestigationInterrupted) -> prior
          key iteration kind = (entry.runId, iteration, kind)
          upsertTask taskKey title status =
            if any ((== taskKey) . fst) tasks
              then map (\(oldKey, old) -> (oldKey, if oldKey == taskKey then old{title = title, status = status} else old)) tasks
              else tasks <> [(taskKey, Task (show $ length tasks) title status)]
          toolKind tool = if LLM.toolFunctionName (LLM.toolCallFunction tool) == "get_investigation_history" then HistoryRead else Evidence (LLM.toolCallId tool)
          stop status = map (\(taskKey@(runId, _, _), task) -> (taskKey, if runId == entry.runId && task.status == InProgress then task{status = status} else task)) tasks
       in case entry.event of
            Investigations.InvestigationStarted{} -> tasks
            -- One reasoning step per run that turns busy on each model turn, rather
            -- than a fresh "assess" line for every turn burying the evidence steps.
            Investigations.ModelStarted _ -> upsertTask (key 0 Decision) "Analyze the evidence" InProgress
            Investigations.ModelReturned _ outcome -> upsertTask (key 0 Decision) "Analyze the evidence" $ case outcome of
              Investigations.ModelAnswered{} -> Complete
              Investigations.ModelRequestFailed -> Error
            Investigations.ToolStarted iteration tool -> upsertTask (key iteration $ toolKind tool) (toolTitle tool) InProgress
            Investigations.ToolReturned iteration tool _ -> upsertTask (key iteration $ toolKind tool) (toolTitle tool) Complete
            Investigations.InvestigationHistoryRead iteration -> upsertTask (key iteration HistoryRead) "Read prior investigation activity" Complete
            Investigations.FollowupsAccepted iteration _ -> upsertTask (key iteration FollowupRead) "Read new thread replies" Complete
            Investigations.InvestigationAwaitingReview{} -> stop Pending
            Investigations.InvestigationFinished -> tasks
            (Investigations.InvestigationFailed; Investigations.InvestigationInterrupted) -> stop Error
    -- A step names its subject (the query, field or file) so a reader can follow
    -- what was checked. Plain text, one line, bounded.
    toolTitle tool =
      let call = LLM.toolCallFunction tool
          subject k = clip <$> (Map.lookup k (LLM.toolFunctionArguments call) >>= \case AE.String t | not (T.null (T.strip t)) -> Just t; _ -> Nothing)
          about k generic specific = maybe generic specific (subject k)
       in case LLM.toolFunctionName call of
            "get_incident_context" -> "Read the incident record"
            "get_investigation_history" -> "Read prior investigation activity"
            "get_related_incidents" -> "Look for related incidents"
            "get_linked_repositories" -> about "service" "Find linked repositories" ("Find repositories for " <>)
            "get_deployments" -> "Check recent deployments"
            "get_code_context" -> about "path" "Inspect source at the reported revision" ("Read " <>)
            "list_runbooks" -> "Look for runbooks"
            "read_runbook" -> about "path" "Read a runbook" ("Read runbook " <>)
            "get_services" -> "List services in telemetry"
            "get_schema" -> "Check available telemetry fields"
            "get_field_values" -> about "field" "Read telemetry field values" ("Read values of " <>)
            "get_facets" -> "Read telemetry summaries"
            "count_query" -> about "query" "Count matching telemetry" ("Count " <>)
            "sample_logs" -> about "query" "Sample matching logs" ("Sample logs: " <>)
            "run_query" -> about "query" "Query telemetry" ("Query " <>)
            "run_sql_query" -> "Query telemetry with SQL"
            "list_metrics" -> "List available metrics"
            _ -> "Check an evidence request"
    clip = truncateText 89 . unwords . words


-- | Retain the history link after stopping; a claimed action may have taken effect.
endReview :: ReviewNotice -> Maybe UTCTime -> Investigations.OperationOutcome -> Snapshot -> Snapshot
endReview notice finishedAt outcome progress =
  progress{title = "Investigation stopped" <> suffix, review = Just $ EndedNotice $ noticeOperation notice, observedAt = fromMaybe progress.observedAt finishedAt, tasks = map stopTask progress.tasks}
  where
    (status, suffix) = case outcome of
      (Investigations.OperationRunning; Investigations.OperationUnknown) -> (Error, " (outcome uncertain)")
      (Investigations.OperationUnstarted; Investigations.OperationRefused{}) -> (Error, " (not completed)")
      Investigations.OperationReturned{} -> (Complete, "")
    stopTask task = case task.status of
      (Pending; InProgress) -> task{status = status, title = task.title <> suffix}
      (Complete; Error) -> task


noticeOperation :: ReviewNotice -> UUIDId "agent_operation"
noticeOperation = \case
  PendingNotice identifier _ -> identifier
  EndedNotice identifier -> identifier


-- | Decode only this app's typed review controls.
--
-- >>> reviewDecision "monoscope_review_approved"
-- Just ReviewApproved
-- >>> reviewDecision "monoscope_review_delete"
-- Nothing
-- >>> reviewDecision "dashboard_approved"
-- Nothing
reviewDecision :: Text -> Maybe Investigations.ReviewDecision
reviewDecision identifier = T.stripPrefix "monoscope_review_" identifier >>= rightToMaybe . parseUrlPiece


-- | Slack plan blocks accept at most fifty tasks. The journal reader bounds the
-- projection to fifty events; each event creates at most one task.
payload :: Text -> Projects.ProjectId -> Snapshot -> AE.Value
payload hostUrl project progress =
  AE.object
    [ "text" AE..= (unlines (progress.title : map taskText progress.tasks) <> caption)
    , "blocks"
        AE..= AE.Array
          ( [ AE.object ["type" AE..= ("plan" :: Text), "title" AE..= progress.title, "tasks" AE..= progress.tasks]
            , AE.object
                [ "type" AE..= ("context" :: Text)
                , "elements"
                    AE..= AE.Array
                      [ AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= caption]
                      ]
                ]
            ]
              <> foldMap
                ( \notice ->
                    let identifier = noticeOperation notice
                        decisions = case notice of
                          PendingNotice{} -> [button Investigations.ReviewApproved "Approve action" identifier, button Investigations.ReviewRejected "Decline action" identifier]
                          EndedNotice{} -> []
                     in V.singleton
                          $ AE.object
                            [ "type" AE..= ("actions" :: Text)
                            , "elements" AE..= AE.Array ([AE.object ["type" AE..= ("button" :: Text), "action_id" AE..= ("monoscope_review_details" :: Text), "url" AE..= hostPath hostUrl (urls.aiOperationGet project identifier), "text" AE..= AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= ("Review details" :: Text)]]] <> decisions)
                            ]
                )
                progress.review
          )
    ]
  where
    -- A finished step means it ran, not that it proved anything.
    caption = "Updated " <> fmtDate "%H:%M:%S UTC" progress.observedAt <> " · steps show what was checked, not what was proven" <> bool "" " · recent activity only" progress.limited
    button decision label identifier =
      AE.object
        [ "type" AE..= ("button" :: Text)
        , "action_id" AE..= ("monoscope_review_" <> toText (encodeEnumSC @"Review" decision) :: Text)
        , "value" AE..= identifier
        , "text" AE..= AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= (label :: Text)]
        ]
    taskText task =
      task.title <> ": " <> case task.status of
        Pending -> "awaiting review"
        InProgress -> "in progress"
        Complete -> "response recorded"
        Error -> "did not finish"


streamFrame :: Snapshot -> Either Text Investigations.StreamFrame
streamFrame progress = Investigations.mkStreamFrame progress.title progress.tasks


streamChunks :: UUIDId "slack_progress" -> Investigations.StreamFrame -> [AE.Value]
streamChunks publication frame =
  AE.object ["type" AE..= ("plan_update" :: Text), "title" AE..= Investigations.streamTitle frame]
    : [ AE.object ["type" AE..= ("task_update" :: Text), "id" AE..= (publication.toText <> ":" <> task.task_id), "title" AE..= task.title, "status" AE..= task.status]
      | task <- toList $ Investigations.streamTasks frame
      ]
