module Pages.Monitors.TestingSpec (spec) where

import Data.Vector qualified as V
import Models.Tests.Testing
import Test.Hspec

import Pages.Monitors.TestCollectionEditor qualified as TestCollectionEditor
import Pages.Monitors.Testing qualified as Testing

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pkg.TestUtils
import Relude

import Pkg.Components.ItemsList qualified as ItemsList


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
    }
collection :: TestCollectionEditor.CollectionStepUpdateForm
collection =
  TestCollectionEditor.CollectionStepUpdateForm
    { title = Just "Test Collection"
    , description = Just "get todos"
    , scheduled = Nothing
    , scheduleNumber = Nothing
    , scheduleNumberUnit = Nothing
    , stepsData = [colStepData]
    }


scheduleCollection :: TestCollectionEditor.CollectionStepUpdateForm
scheduleCollection =
  TestCollectionEditor.CollectionStepUpdateForm
    { title = Just "Test Collection"
    , description = Just "get todos"
    , scheduled = Nothing
    , scheduleNumber = Just "1"
    , scheduleNumberUnit = Just "days"
    , stepsData = [colStepData]
    }


scheduleCollectionMn :: TestCollectionEditor.CollectionStepUpdateForm
scheduleCollectionMn =
  TestCollectionEditor.CollectionStepUpdateForm
    { title = Just "Test Collection"
    , description = Just "get todos"
    , scheduled = Just "on"
    , scheduleNumber = Just "1"
    , scheduleNumberUnit = Just "hours"
    , stepsData = [colStepData]
    }


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Test Collections" do
    it "should return an empty list" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ collections)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.testingGetH testPid Nothing Nothing
      length collections `shouldBe` 0

    it "should add test collection" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ collections)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.testingPostH testPid collection
      length collections `shouldBe` 1

    it "should get inactive collections" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ collections)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.testingGetH testPid (Just "Inactive") Nothing
      length collections `shouldBe` 0

    it "should not allow schedule unit less than a day with free plan" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ collections)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.testingGetH testPid Nothing Nothing
      length collections `shouldBe` 1
      let col = V.head $ (\(Testing.CollectionListItemVM _ co _) -> co) <$> collections
      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ TestCollectionEditor.collectionStepsUpdateH testPid col.id scheduleCollectionMn
      case res of
        TestCollectionEditor.CollectionMutSuccess -> fail "Error"
        _ -> do pass

    it "should get active collections and disable collection" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ collections)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.testingGetH testPid Nothing Nothing
      length collections `shouldBe` 1
      let col = V.head $ (\(Testing.CollectionListItemVM _ co _) -> co) <$> collections
      col.title `shouldBe` "Test Collection"
      col.stepsCount `shouldBe` 1
      col.lastRun `shouldBe` Nothing
      col.schedule `shouldBe` "1 days"
      col.isScheduled `shouldBe` True
      col.description `shouldBe` "get todos"

      res <-
        toServantResponse trATCtx trSessAndHeader trLogger $ TestCollectionEditor.collectionStepsUpdateH testPid col.id scheduleCollection
      case res of
        TestCollectionEditor.CollectionMutError -> fail "Error"
        _ -> do pass
    it "should get inative collections" \TestResources{..} -> do
      (PageCtx _ (ItemsList.ItemsPage _ collections)) <-
        toServantResponse trATCtx trSessAndHeader trLogger $ Testing.testingGetH testPid (Just "Inactive") Nothing
      length collections `shouldBe` 1
      let col = V.head $ (\(Testing.CollectionListItemVM _ co _) -> co) <$> collections
      col.title `shouldBe` "Test Collection"
      col.stepsCount `shouldBe` 1
      col.lastRun `shouldBe` Nothing
      col.schedule `shouldBe` "1 days"
      col.isScheduled `shouldBe` False
