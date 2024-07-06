module Pages.Endpoints.EndpointDetailsSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Apis.Endpoints qualified as Endpoints
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints.EndpointDetails qualified as EndpointDetails
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
  describe "Check Endpoint Details" do
    it "should return an empty list" \TestResources{..} -> do
      enpId <- Endpoints.EndpointId <$> UUID.nextRandom
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ EndpointDetails.endpointDetailsH testPid enpId Nothing Nothing Nothing Nothing Nothing
      case pg of
        EndpointDetails.EndpointsDetailsNotFound (PageCtx _ ()) -> do
          1 `shouldBe` 1
        _ -> error "Unexpected response"
    it "should return endpoints details" \TestResources{..} -> do
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

      let msg1EndpointHash = toXXHash $ testPid.toText <> "172.31.29.11" <> "GET" <> "/"

      endpointM <- withPool trPool $ Endpoints.endpointByHash testPid msg1EndpointHash

      isJust endpointM `shouldBe` True
      let endpoint = Unsafe.fromJust endpointM
      let endpointId = endpoint.id

      let endpointDuration = (476434 * 100) / 1000000 -- duration in request message example
      let totalTimeProj = endpointDuration + ((103636077 * 100) / 1000000)
      pg <-
        toServantResponse trATCtx trSessAndHeader trLogger $ EndpointDetails.endpointDetailsH testPid endpointId Nothing Nothing Nothing Nothing Nothing
      case pg of
        EndpointDetails.EndpointsDetailsMain (PageCtx _ content) -> do
          let (pid, _paramInput, _currTime, endpt, enpStats, _shapesWithFieldsMap, _fieldsMap, _shapesList, _shapeHashM, _reqLatenciesRolledByStepsJ, _) = content
          testPid `shouldBe` pid
          endpt.id `shouldBe` endpointId
          endpt.urlPath `shouldBe` "/"
          endpt.method `shouldBe` "GET"
          enpStats.totalRequests `shouldBe` 100
          enpStats.totalRequestsProj `shouldBe` 200
          enpStats.totalTime `shouldBe` endpointDuration
          let timeText = show enpStats.totalTimeProj <> "00000001"
          let timeText' = show $ totalTimeProj
          timeText `shouldBe` timeText'
        _ -> error "Unexpected response"
