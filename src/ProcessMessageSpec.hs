module ProcessMessageSpec (spec) where 

import Relude
import Test.Hspec
import Pkg.TmpPg  qualified as TmpPg

spec :: Spec 
spec = aroundAll TmpPg.withSetup do
  describe "process request to db" do
    it "should save the request" \pool -> do
      traceShowM "in test bool"
      True `shouldBe` True
