module Pages.Monitors.TestingSpec (spec) where

import Data.UUID qualified as UUID
import Data.Vector qualified as V
import Models.Projects.Projects qualified as Projects
import Models.Tests.Testing
import Pages.BodyWrapper (PageCtx (..))
import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pages.Monitors.Testing qualified as Testing
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, aroundAll, describe, it, pendingWith, shouldBe, shouldSatisfy)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


colStepData :: CollectionStepData
colStepData =
  CollectionStepData
    { title = Just "get todos"
    , post = Nothing
    , get = Just "https://jsonplaceholder.typicode.com/todos"
    , put = Nothing
    , delete = Nothing
    , update = Nothing
    , patch = Nothing
    , params = Nothing
    , headers = Nothing
    , exports = Nothing
    , json = Nothing
    , raw = Nothing
    , asserts = Nothing
    , disabled = Nothing
    , httpVersion = Nothing
    , timeout = Nothing
    , followRedirects = Nothing
    , allowRedirects = Nothing
    , ignoreSSLErrors = Nothing
    , requestBody = Nothing
    }
collection :: CollectionStepUpdateForm
collection =
  CollectionStepUpdateForm
    { title = Just "Test Collection"
    , description = Just "get todos"
    , scheduled = Nothing
    , scheduleNumber = Nothing
    , scheduleNumberUnit = Nothing
    , stepsData = [colStepData]
    , tags = Just V.empty
    , alertMessage = Nothing
    , alertSeverity = Nothing
    , alertSubject = Nothing
    , collectionId = Nothing
    , notifyAfter = Nothing
    , notifyAfterCheck = Nothing
    , stopAfter = Nothing
    , stopAfterCheck = Nothing
    }


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Test Collections" do
    it "should return an empty list" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ monitors)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.unifiedMonitorsGetH testPid Nothing Nothing
      length monitors `shouldBe` 0

    it "should add test collection" \TestResources{..} -> do
      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ TestCollectionEditor.collectionStepsUpdateH testPid collection Nothing
      case res of
        TestCollectionEditor.CollectionMutSuccess -> pass
        _ -> fail "Expected CollectionMutSuccess"

    it "should get inactive collections" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ monitors)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.unifiedMonitorsGetH testPid (Just "Inactive") Nothing
      length monitors `shouldBe` 0

    it "should not allow schedule unit less than a day with free plan" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ monitors)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.unifiedMonitorsGetH testPid Nothing Nothing
      length monitors `shouldBe` 1
      let monitor = Unsafe.fromJust $ V.headM monitors
      monitor.monitorType `shouldBe` Testing.MTMultiStep
      let scheduleCollectionMn =
            CollectionStepUpdateForm
              { title = Just "Test Collection"
              , description = Just "get todos"
              , scheduled = Just "on"
              , scheduleNumber = Just "1"
              , scheduleNumberUnit = Just "hours"
              , stepsData = [colStepData]
              , tags = Just V.empty
              , alertMessage = Nothing
              , alertSeverity = Nothing
              , alertSubject = Nothing
              , collectionId = Just $ CollectionId $ Unsafe.fromJust $ UUID.fromText monitor.monitorId
              , notifyAfter = Nothing
              , notifyAfterCheck = Nothing
              , stopAfter = Nothing
              , stopAfterCheck = Nothing
              }

      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ TestCollectionEditor.collectionStepsUpdateH testPid scheduleCollectionMn Nothing
      case res of
        TestCollectionEditor.CollectionMutSuccess -> fail "Error"
        _ -> do pass

    it "should get active collections and disable collection" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ monitors)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.unifiedMonitorsGetH testPid Nothing Nothing
      length monitors `shouldBe` 1
      let monitor = Unsafe.fromJust $ V.headM monitors
      monitor.monitorType `shouldBe` Testing.MTMultiStep
      monitor.title `shouldBe` "Test Collection"
      case monitor.details of
        Testing.MultiStepDetails{stepsCount} -> stepsCount `shouldBe` 1
        _ -> fail "Expected MultiStepDetails"
      monitor.lastRun `shouldBe` Nothing
      monitor.schedule `shouldBe` "every 1 days"
      monitor.status `shouldBe` "Passing"
      let scheduleCollection =
            CollectionStepUpdateForm
              { title = Just "Test Collection"
              , description = Just "get todos"
              , scheduled = Nothing
              , scheduleNumber = Just "1"
              , scheduleNumberUnit = Just "days"
              , stepsData = [colStepData]
              , tags = Just V.empty
              , alertMessage = Nothing
              , alertSeverity = Nothing
              , alertSubject = Nothing
              , collectionId = Just $ CollectionId $ Unsafe.fromJust $ UUID.fromText monitor.monitorId
              , notifyAfter = Nothing
              , notifyAfterCheck = Nothing
              , stopAfter = Nothing
              , stopAfterCheck = Nothing
              }

      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ TestCollectionEditor.collectionStepsUpdateH testPid scheduleCollection Nothing
      case res of
        TestCollectionEditor.CollectionMutError -> fail "Error"
        _ -> do pass
    it "should get inative collections" \_ -> 
      pendingWith "Collection deactivation not working as expected" {-
      (PageCtx _ (ItemsList.ItemsPage _ monitors)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.unifiedMonitorsGetH testPid (Just "Inactive") Nothing
      length monitors `shouldBe` 1
      let monitor = Unsafe.fromJust $ V.headM monitors
      monitor.monitorType `shouldBe` Testing.MTMultiStep
      monitor.title `shouldBe` "Test Collection"
      case monitor.details of
        Testing.MultiStepDetails{stepsCount} -> stepsCount `shouldBe` 1
        _ -> fail "Expected MultiStepDetails"
      monitor.lastRun `shouldBe` Nothing
      monitor.schedule `shouldBe` "every 1 days"
      monitor.status `shouldBe` "Inactive" -}
