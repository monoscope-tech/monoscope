module Pages.Anomalies.AnomalyListSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID

import Data.Vector qualified as V
import Models.Apis.Anomalies
import Models.Projects.Projects qualified as Projects
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Servant qualified

import Relude.Unsafe qualified as Unsafe
import Servant.Server qualified as ServantS
import System.Types (atAuthToBase, effToServantHandlerTest)

import Test.Hspec (Spec, aroundAll, describe, it, shouldBe, shouldNotBe)


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

    it "should return a list of anomalies" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      -- let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg1) -- same message
            , ("m3", reqMsg1) -- same message
            , ("m4", reqMsg1) -- same message
            ]
      res <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      pg <-
        AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies
          let shapesAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATShape) anomalies
          let formatAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATFormat) anomalies
          (length endpointAnomalies) `shouldBe` 1
          (length shapesAnomalies) `shouldBe` 0 -- All anomalies under the endpoint are not counted until the endpoint is acknowledged.
          (length formatAnomalies) `shouldBe` 0 -- Same as above
          (length anomalies) `shouldNotBe` 0
        -- TODO: add more tests
        _ -> error "Unexpected response"

    -- it "should acknowledge anomaly" \TestResources{_} -> do
    --   1 `shouldBe` 1

    it "should detect new endpoint" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            [ ("m1", reqMsg1)
            , ("m2", reqMsg2)
            , ("m3", reqMsg1)
            , ("m4", reqMsg2)
            ]
      res <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx

      pg <-
        AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          & atAuthToBase trSessAndHeader
          & effToServantHandlerTest trATCtx trLogger
          & ServantS.runHandler
          <&> fromRightShow
          <&> Servant.getResponse

      case pg of
        AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
          let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies
          let shapesAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATShape) anomalies
          let formatAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATFormat) anomalies
          (length endpointAnomalies) `shouldBe` 2
          (length shapesAnomalies) `shouldBe` 0 -- All anomalies under the endpoint are not counted until the endpoint is acknowledged.
          (length formatAnomalies) `shouldBe` 0 -- Same as above
          (length anomalies) `shouldNotBe` 0
        -- TODO: add more tests
        _ -> error "Unexpected response"
