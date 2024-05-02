module Pages.Anomalies.Server (server) where

import Models.Projects.Projects qualified as Projects
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.Anomalies.Routes (Routes, Routes' (..))
import Servant qualified
import System.Types (ATAuthCtx)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { acknowlegeGet = AnomalyList.acknowlegeAnomalyGetH pid
    , unAcknowlegeGet = AnomalyList.unAcknowlegeAnomalyGetH pid
    , archiveGet = AnomalyList.archiveAnomalyGetH pid
    , unarchiveGet = AnomalyList.unArchiveAnomalyGetH pid
    , bulkActionsPost = AnomalyList.anomalyBulkActionsPostH pid
    , listGet = AnomalyList.anomalyListGetH pid
    , detailsGet = AnomalyList.anomalyDetailsGetH pid
    }
