module Pages.Endpoints.ApiCatalogSpec (spec) where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Entity.DBT (withPool)
import Models.Projects.Projects qualified as Projects
import Pages.BodyWrapper (PageCtx (..))
import Pages.Endpoints.ApiCatalog qualified as ApiCatalog
import Pkg.Components.ItemsList qualified as ItemsList
import Pkg.TestUtils
import ProcessMessage (processMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Api Catalog List" do
    it "should return an empty list" \TestResources{..} -> do
      PageCtx _ (ItemsList.ItemsPage _ hostsAndEvents) <- toServantResponse trATCtx trSessAndHeader trLogger $ ApiCatalog.apiCatalogH testPid Nothing Nothing Nothing
      length hostsAndEvents `shouldBe` 0

    it "should return incoming hosts list and outgoing host list" \TestResources{..} -> do
      currentTime <- getCurrentTime
      let nowTxt = toText $ formatTime defaultTimeLocale "%FT%T%QZ" currentTime
      let reqMsg1 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg1 nowTxt
      let reqMsg2 = Unsafe.fromJust $ convert $ testRequestMsgs.reqMsg2 nowTxt
      let msgs =
            concat
              $ replicate
                100
                [ ("m1", BL.toStrict $ AE.encode reqMsg1)
                , ("m2", BL.toStrict $ AE.encode reqMsg2)
                ]
      _ <- runTestBackground trATCtx $ processMessages msgs HashMap.empty
      _ <- runAllBackgroundJobs trATCtx
      _ <- withPool trPool $ refreshMaterializedView "apis.endpoint_request_stats"

      PageCtx _ (ItemsList.ItemsPage _ hostsAndEvents) <- toServantResponse trATCtx trSessAndHeader trLogger $ ApiCatalog.apiCatalogH testPid Nothing Nothing (Just "Incoming")
      length hostsAndEvents `shouldBe` 1
