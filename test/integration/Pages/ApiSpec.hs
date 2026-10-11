module Pages.ApiSpec (spec) where

import Data.Pool (withResource)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful.Error.Static (catchError)
import Lucid qualified
import Models.Projects.ProjectApiKeys (ProjectApiKey (..))
import Models.Projects.ProjectApiKeys qualified as Keys
import Models.Projects.Projects qualified as Projects
import Network.GRPC.Common (GrpcError (GrpcUnauthenticated), GrpcException (..))
import Network.GRPC.Common.Protobuf (Proto (..))
import Opentelemetry.OtlpServer qualified as OtlpServer
import Pages.BodyWrapper (PageCtx (..))
import Pkg.TestUtils
import Relude
import Servant (ServerError (..), getResponse)
import Test.Hspec
import UnliftIO.Exception (bracket_)

import Pages.Settings qualified as Api


spec :: Spec
spec = sequential $ aroundAll withTestResources do
  describe "Check API Keys" do
    it "creates, revokes, and reactivates a key around real ingest" \tr -> do
      let ingest key body = void $ OtlpServer.logsServiceExport tr.trLogger tr.trATCtx tr.trTracerProvider (Proto $ createOtelLogAtTime key [body] frozenTime)
      (_, Api.ApiGet (PageCtx _ settings)) <- testServant tr $ Api.apiGetH testPid
      length settings.keys `shouldBe` 1
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

    it "apiKeyViewer_cannotCreateRevokeOrActivateKeys" \tr -> do
      initial <- runQueryEffect tr $ Keys.projectApiKeysByProjectId testPid
      saved <- maybe (fail "fixture key missing") pure (listToMaybe initial)
      let uid = (getResponse tr.trSessAndHeader).user.id
          setPermission permission = void $ withResource tr.trPool \conn -> PG.execute conn [sql|UPDATE projects.project_members SET permission = ? WHERE project_id = ? AND user_id = ?|] (permission :: Text, testPid, uid)
      bracket_ (setPermission "view") (setPermission "admin") do
        (_, viewerPage) <- testServant tr $ Api.apiGetH testPid
        let html = LT.toStrict $ Lucid.renderText $ Lucid.toHtml viewerPage
            leaked = filter (`T.isInfixOf` html) (["New Key", "aria-label=\"Revoke ", "aria-label=\"Activate ", "aria-label=\"Show value for ", "aria-label=\"Copy "] <> map (.keyPrefix) initial)
        statuses <- forM
          [ void $ Api.apiPostH testPid (Api.GenerateAPIKeyForm "Forbidden" Nothing)
          , void $ Api.apiDeleteH testPid saved.id
          , void $ Api.apiActivateH testPid saved.id
          ]
          \mutation -> runAuthHandler tr $ catchError @ServerError (mutation $> 200) (\_ err -> pure err.errHTTPCode)
        (statuses, leaked) `shouldBe` ([403, 403, 403], [])
        retained <- runQueryEffect tr $ Keys.projectApiKeysByProjectId testPid
        map (\key -> (key.id, key.active)) retained `shouldBe` map (\key -> (key.id, key.active)) initial
