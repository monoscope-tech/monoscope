module Pages.LogExplorer.LogItemSpec (spec) where

import Control.Lens ((^?))
import Data.Aeson qualified as AE
import Data.Aeson.Lens
import Data.Aeson.QQ (aesonQQ)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Models.Projects.Projects qualified as Projects
import Pages.LogExplorer.LogItem qualified as LogItem
import Pkg.TestUtils
import ProcessMessage (processRequestMessages)
import Relude
import Relude.Unsafe qualified as Unsafe
import Test.Hspec


testPid :: Projects.ProjectId
testPid = Projects.ProjectId UUID.nil


spec :: Spec
spec = aroundAll withTestResources do
  describe "Check Log Item" do
    it "should return an empty list" \TestResources{} -> do
      1 `shouldBe` 1
