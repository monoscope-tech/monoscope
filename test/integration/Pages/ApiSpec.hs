module Pages.ApiSpec (spec) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Lucid qualified
import Models.Projects.ProjectApiKeys (ProjectApiKey (..))
import Network.GRPC.Common (GrpcError (GrpcUnauthenticated), GrpcException (..))
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (PageCtx (..))
import Pkg.TestUtils
import Relude
import Test.Hspec

import Pages.Settings qualified as Api


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Check API Keys" do
    it "creates, revokes, and reactivates a key around real ingest" \tr -> do
      let ingest key body = void $ OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createOtelLogAtTime key [body] frozenTime)
      (_, Api.ApiGet (PageCtx _ (_, initialKeys))) <- testServant tr $ Api.apiGetH testPid
      length initialKeys `shouldBe` 1
      let apikeyForm = Api.GenerateAPIKeyForm{title = "Test", from = Nothing}
      (_, Api.ApiPost pid createdKeys (Just (apiKey, keyText))) <- testServant tr $ Api.apiPostH testPid apikeyForm
      (pid, length createdKeys) `shouldBe` (testPid, 2)
      apiKey.title `shouldBe` "Test"
      apiKey.active `shouldBe` True
      apiKey.keyPrefix `shouldBe` keyText
      ingest keyText "accepted before revoke"

      (_, Api.ApiPost _ revokedKeys Nothing) <- testServant tr $ Api.apiDeleteH testPid apiKey.id
      (find ((== apiKey.id) . (.id)) revokedKeys <&> (.active)) `shouldBe` Just False
      ingest keyText "rejected after revoke"
        `shouldThrow` \case GrpcException{grpcError = GrpcUnauthenticated} -> True; _ -> False

      (_, Api.ApiPost _ activeKeys Nothing) <- testServant tr $ Api.apiActivateH testPid apiKey.id
      (find ((== apiKey.id) . (.id)) activeKeys <&> (.active)) `shouldBe` Just True
      ingest keyText "accepted after activation"

      (_, page) <- testServant tr $ Api.apiGetH testPid
      let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml page
      let missing = filter (\action -> not $ ("aria-label=\"" <> action <> "\"") `T.isInfixOf` html) ["Show value for Test", "Copy Test", "Revoke Test"]
      missing `shouldBe` []
      html `shouldSatisfy` T.isInfixOf "for=\"api-key-title\""
      T.count "<main" html `shouldBe` 1
