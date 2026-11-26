module RequestMessagesSpec (spec) where

import Data.Aeson qualified as AE
import Data.Aeson.QQ
import ProcessMessage qualified as RequestMessages
import Relude
import Test.Hspec


spec :: Spec
spec = do
  describe "Process Messages" do
    it "value to fields" do
      let exJSON =
            [aesonQQ| {
              "menu": {
                "id": "file",
                "popup": {
                  "menuitem": [
                    {"value": "v1", "onclick": "oc1"},
                    {"value": "v2", "onclick": "oc2"}
                  ]
                }
              }
            }|]
      let expectedResp =
            [ -- FIXME: We can correctly handle objects in arrays.
              ("menu.popup.menuitem[*].value", [AE.String "v1", AE.String "v2"])
            , ("menu.id", [AE.String "file"])
            , ("menu.popup.menuitem[*].onclick", [AE.String "oc1", AE.String "oc2"])
            ]
      RequestMessages.valueToFields exJSON `shouldBe` expectedResp

