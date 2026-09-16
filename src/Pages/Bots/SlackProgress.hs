module Pages.Bots.SlackProgress (Snapshot (..), Task (..), TaskStatus (..), snapshot, payload) where

import Data.Aeson qualified as AE
import Data.Time (UTCTime)
import Deriving.Aeson qualified as DAE
import Langchain.LLM.Core qualified as LLM
import Models.Apis.Investigations qualified as Investigations
import Relude


-- $setup
-- >>> import Data.UUID qualified as UUID
-- >>> import Data.Time (UTCTime(..), fromGregorian)
-- >>> import Pkg.DeriveUtils (UUIDId(..))
-- >>> let activity :: Investigations.Event -> Investigations.Entry; activity = Investigations.Entry (UUIDId $ UUID.fromWords 0 0 0 1) "1.0" (UTCTime (fromGregorian 2025 1 1) 0)


data TaskStatus = InProgress | Complete | Error
  deriving stock (Eq, Generic, Show)
  deriving (AE.ToJSON) via DAE.CustomJSON '[DAE.ConstructorTagModifier DAE.CamelToSnake] TaskStatus


data Task = Task {task_id :: Text, title :: Text, status :: TaskStatus}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (AE.ToJSON)


data TaskKind = Decision | HistoryRead | FollowupRead | Evidence Text
  deriving stock (Eq, Show)


data Snapshot = Snapshot {title :: Text, tasks :: [Task], observedAt :: UTCTime, limited :: Bool}
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
        _ -> "Investigating"
  guard $ not $ null tasks
  pure $ Snapshot title tasks latest.observedAt history.limitReached
  where
    update tasks entry =
      let key iteration kind = (entry.runId, iteration, kind)
          upsertTask taskKey title status =
            if any ((== taskKey) . fst) tasks
              then map (\(oldKey, old) -> (oldKey, if oldKey == taskKey then old{title = title, status = status} else old)) tasks
              else tasks <> [(taskKey, Task (show $ length tasks) title status)]
          toolKind tool = if LLM.toolFunctionName (LLM.toolCallFunction tool) == "get_investigation_history" then HistoryRead else Evidence (LLM.toolCallId tool)
          stop = map (\(taskKey@(runId, _, _), task) -> (taskKey, if runId == entry.runId && task.status == InProgress then task{status = Error} else task)) tasks
       in case entry.event of
            Investigations.InvestigationStarted{} -> tasks
            Investigations.ModelStarted iteration -> upsertTask (key iteration Decision) "Assess the evidence" InProgress
            Investigations.ModelReturned iteration outcome -> upsertTask (key iteration Decision) "Assess the evidence" $ case outcome of
              Investigations.ModelAnswered{} -> Complete
              Investigations.ModelRequestFailed -> Error
            Investigations.ToolStarted iteration tool -> upsertTask (key iteration $ toolKind tool) (toolTitle tool) InProgress
            Investigations.ToolReturned iteration tool _ -> upsertTask (key iteration $ toolKind tool) (toolTitle tool) Complete
            Investigations.InvestigationHistoryRead iteration -> upsertTask (key iteration HistoryRead) "Read prior investigation activity" Complete
            Investigations.FollowupsAccepted iteration _ -> upsertTask (key iteration FollowupRead) "Read new thread replies" Complete
            Investigations.InvestigationFinished -> tasks
            Investigations.InvestigationFailed -> stop
            Investigations.InvestigationInterrupted -> stop


toolTitle :: LLM.ToolCall -> Text
toolTitle tool = case LLM.toolFunctionName $ LLM.toolCallFunction tool of
  "get_incident_context" -> "Read the incident record"
  "get_investigation_history" -> "Read prior investigation activity"
  "get_linked_repositories" -> "Find linked repositories"
  "get_deployments" -> "Check deployment history"
  "get_code_context" -> "Inspect source at the requested revision"
  "get_services" -> "Find services in telemetry"
  "get_schema" -> "Check available telemetry fields"
  "get_field_values" -> "Read telemetry field values"
  "get_facets" -> "Read telemetry summaries"
  "count_query" -> "Count matching telemetry"
  "sample_logs" -> "Inspect a sample of logs"
  "run_query" -> "Query telemetry"
  "run_sql_query" -> "Query telemetry"
  _ -> "Check an evidence request"


-- | Slack plan blocks accept at most fifty tasks. The journal reader bounds the
-- projection to fifty events; each event creates at most one task.
payload :: Snapshot -> AE.Value
payload progress =
  AE.object
    [ "text" AE..= (unlines (progress.title : map taskText progress.tasks) <> "Updated " <> show progress.observedAt <> ". Completed steps mean a response was recorded; check the findings for results.")
    , "blocks"
        AE..= AE.Array
          [ AE.object ["type" AE..= ("plan" :: Text), "title" AE..= progress.title, "tasks" AE..= progress.tasks]
          , AE.object
              [ "type" AE..= ("context" :: Text)
              , "elements"
                  AE..= AE.Array
                    [ AE.object ["type" AE..= ("plain_text" :: Text), "text" AE..= ("Updated " <> show progress.observedAt <> ". Completed steps record responses, not confirmed causes." <> if progress.limited then " Showing recent activity only." else "")]
                    ]
              ]
          ]
    ]
  where
    taskText task =
      task.title <> ": " <> case task.status of
        InProgress -> "in progress"
        Complete -> "response recorded"
        Error -> "did not finish"
