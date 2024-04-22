module Pages.Monitors.Server (server) where

import Models.Projects.Projects qualified as Projects
import Pages.Monitors.Alerts qualified as Alerts
import Pages.Monitors.Routes (Routes, Routes' (..))
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pages.Monitors.Testing qualified as Testing
import Servant qualified
import System.Types (ATAuthCtx)


server :: Projects.ProjectId -> Servant.ServerT Routes ATAuthCtx
server pid =
  Routes'
    { alertUpsertPost = Alerts.alertUpsertPostH pid
    , alertListGet = Alerts.alertListGetH pid
    , alertSingleGet = Alerts.alertSingleGetH pid
    , alertSingleToggleActive = Alerts.alertSingleToggleActiveH pid
    , collectionsGet = Testing.testingGetH pid
    , newCollectionPost = Testing.testingPostH pid
    , collectionGet = TestCollectionEditor.collectionGetH pid
    , collectionPut = Testing.testingPutH pid

    , collectionStepsUpdate = TestCollectionEditor.collectionStepsUpdateH pid
  
    , collectionStepPost = Testing.collectionStepPostH pid
    , collectionStepPut = Testing.collectionStepPutH pid
    , saveFromCodePost = Testing.saveStepsFromCodePostH pid

    , deleteCollectionStep = Testing.deleteStepH pid
    , runTestCollection = Testing.runTestCollectionH pid
    , runTestCollectionStep = Testing.runTestStepH pid
    }
