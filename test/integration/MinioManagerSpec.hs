module MinioManagerSpec (spec) where

import Conduit qualified as CC
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.List qualified as List
import Network.HTTP.Client qualified as HC
import Network.HTTP.Client.TLS qualified as HCTLS
import Network.HTTP.Types (hAuthorization, status200)
import Network.Minio qualified as Minio
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Pkg.Minio qualified as Storage
import Relude
import Test.Hspec


spec :: Spec
spec = describe "shared object-storage transport" do
  it "reuses transport while honoring each operation's credentials, region and endpoint" do
    first <- newIORef []
    second <- newIORef []
    let server observed = pure $ \request respond -> do
          body <- Wai.strictRequestBody request
          atomicModifyIORef' observed $ \requests -> ((Wai.remoteHost request, List.lookup hAuthorization (Wai.requestHeaders request), body) : requests, ())
          respond $ Wai.responseLBS status200 [("ETag", "\"etag\"")] ""
    manager <- HC.newManager HCTLS.tlsManagerSettings
    let upload port access region = do
          let info = fromString $ "http://127.0.0.1:" <> show port
              config = Minio.setCreds (Minio.CredentialValue access "test-secret" Nothing) (Minio.setRegion region info)
          result <- Storage.runMinio manager config $ Minio.putObject "test-bucket" "object" (CC.sourceLazy "body") (Just 4) Minio.defaultPutObjectOptions
          result `shouldSatisfy` isRight
    Warp.testWithApplication (server first) $ \firstPort ->
      Warp.testWithApplication (server second) $ \secondPort -> do
        upload firstPort "first-key" "us-east-1"
        upload firstPort "second-key" "eu-west-1"
        upload secondPort "third-key" "us-east-1"
    [(peer1, auth1, body1), (peer2, auth2, body2)] <- reverse <$> readIORef first
    [(_, auth3, body3)] <- readIORef second
    peer1 `shouldBe` peer2
    forM_ [body1, body2, body3] $ \body -> case BL.lines body of
      [chunkHeader, payload, finalChunk, blank] -> do
        chunkHeader `shouldSatisfy` BL.isPrefixOf "4;chunk-signature="
        payload `shouldBe` "body\r"
        finalChunk `shouldSatisfy` BL.isPrefixOf "0;chunk-signature="
        blank `shouldBe` "\r"
      _ -> expectationFailure "expected one signed payload chunk and a final empty chunk"
    auth1 `shouldSatisfy` maybe False (BS.isInfixOf "Credential=first-key/")
    auth2 `shouldSatisfy` maybe False (\header -> BS.isInfixOf "Credential=second-key/" header && BS.isInfixOf "/eu-west-1/s3/aws4_request" header)
    auth3 `shouldSatisfy` maybe False (BS.isInfixOf "Credential=third-key/")
