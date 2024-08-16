module Pages.Endpoints.EndpointListSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as V
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Anomalies
import Models.Apis.Endpoints
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pages.Anomalies.AnomalyList qualified as AnomalyList
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints.EndpointList qualified as EndpointList
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)

import Relude
import Relude.Unsafe qualified as Unsafe
import RequestMessages (toXXHash)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Endpoint List" do
    it "should return an empty list" \TestResources{..} -> do
      enpId <- Endpoints.EndpointId <$> UUID.nextRandom
      enp <-
        toServantResponse trATCtx trSessAndHeader trLogger $ EndpointList.endpointListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      case enp of
        EndpointList.EndpointsListPage (PageCtx _ (ItemsList.ItemsPage _ enpList)) -> do
          length enpList `shouldBe` 0
        _ -> error "Unexpected response"
    it "should return inbox endpoints list and acknowledge endpoints" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            concat $
              replicate 100 $
                [ ("m1", reqMsg1)
                , ("m2", reqMsg2)
                ]
      _ <- runTestBackground trATCtx $ processRequestMessages msgs
      _ <- runAllBackgroundJobs trATCtx
      _ <- withPool trPool $ refreshMaterializedView "apis.endpoint_request_stats"

      enp <-
        toServantResponse trATCtx trSessAndHeader trLogger $ EndpointList.endpointListGetH testPid Nothing Nothing (Just "Inbox") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      case enp of
        EndpointList.EndpointsListPage (PageCtx _ (ItemsList.ItemsPage _ enpList)) -> do
          length enpList `shouldBe` 2
          let enp1 = (\(EndpointList.EnpReqStatsVM a b c) -> c) <$> Unsafe.fromJust $ find (\(EndpointList.EnpReqStatsVM a b c) -> c.urlPath == "/") enpList
          let enp2 = (\(EndpointList.EnpReqStatsVM a b c) -> c) <$> Unsafe.fromJust $ find (\(EndpointList.EnpReqStatsVM a b c) -> c.urlPath == "/api/v1/user/login") enpList
          enp1.endpointHash `shouldBe` toXXHash (testPid.toText <> "172.31.29.11" <> "GET" <> "/")
          enp2.endpointHash `shouldBe` toXXHash (testPid.toText <> "api.test.com" <> "POST" <> "/api/v1/user/login")
          pg <-
            toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
          case pg of
            AnomalyList.ALItemsPage (PageCtx _ (ItemsList.ItemsPage _ anomalies)) -> do
              let endpointAnomalies = V.filter (\(AnomalyList.IssueVM _ _ c) -> c.anomalyType == ATEndpoint) anomalies <&> (\(AnomalyList.IssueVM a b issue) -> anomalyIdText issue.id)
              let bulkFrm = AnomalyList.AnomalyBulk{anomalyId = V.toList endpointAnomalies}
              _ <- toServantResponse trATCtx trSessAndHeader trLogger $ AnomalyList.anomalyBulkActionsPostH testPid "acknowlege" bulkFrm
              pass
            _ -> error "Unexpected response"
        _ -> error "Unexpected response"
      pass

    it "should return active endpoints list" \TestResources{..} -> do
      evm <-
        toServantResponse trATCtx trSessAndHeader trLogger $ EndpointList.endpointListGetH testPid Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      case evm of
        EndpointList.EndpointsListPage (PageCtx _ (ItemsList.ItemsPage _ enpList)) -> do
          length enpList `shouldBe` 2
          let enp1 = (\(EndpointList.EnpReqStatsVM _ _ c) -> c) <$> Unsafe.fromJust $ find (\(EndpointList.EnpReqStatsVM _ _ c) -> c.urlPath == "/") enpList
          let enp2 = (\(EndpointList.EnpReqStatsVM _ _ c) -> c) <$> Unsafe.fromJust $ find (\(EndpointList.EnpReqStatsVM _ _ c) -> c.urlPath == "/api/v1/user/login") enpList
          enp1.endpointHash `shouldBe` toXXHash (testPid.toText <> "172.31.29.11" <> "GET" <> "/")
          enp2.endpointHash `shouldBe` toXXHash (testPid.toText <> "api.test.com" <> "POST" <> "/api/v1/user/login")
        _ -> error "Unexpected response"
