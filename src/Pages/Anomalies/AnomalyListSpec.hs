module Pages.Anomalies.AnomalyListSpec (spec) where

import Data.UUID qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import Relude
import Servant qualified
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Anomaly List" do
    it "should return an empty list" \TestResources{..} -> do
      pg <-
        AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          length anomalies `shouldBe` 0
        _ -> error "Unexpected response"
