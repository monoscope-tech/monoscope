{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RequestMessagesSpec where

import Data.Aeson (Object, Value (Array, Bool, Null, Number, Object, String), decodeStrict, eitherDecode, eitherDecodeStrict)
import Data.Aeson as AE
import Data.Aeson.QQ
import Data.Aeson.Types as AET
import Relude
import qualified RequestMessages
import Test.Hspec
import Test.Hspec.DB (TestDB, describeDB, itDB)

spec :: Spec
spec = do
  describe "Process Messages" $ do
    it "value to fields" $ do
      let exJSON =
            [aesonQQ| {
              "menu": {
                "id": "file",
                "value": "File",
                "popup": {
                  "menuitem": [
                    {"value": "New", "onclick": "CreateNewDoc()"},
                    {"value": "Open", "onclick": "OpenDoc()"},
                    {"value": "Close", "onclick": "CloseDoc()"}
                  ]
                }
              }
            }|]
      let expectedResp =
            [ (".menu.id", AE.String "file"),
              (".menu.value", AE.String "File"),
              (".menu.popup.menuitem.[].value", AE.String "Close"),
              (".menu.popup.menuitem.[].onclick", AE.String "CloseDoc()"),
              (".menu.popup.menuitem.[].value", AE.String "Open"),
              (".menu.popup.menuitem.[].onclick", AE.String "OpenDoc()"),
              (".menu.popup.menuitem.[].value", AE.String "New"),
              (".menu.popup.menuitem.[].onclick", AE.String "CreateNewDoc()")
            ]
      RequestMessages.valueToFields exJSON `shouldBe` expectedResp

  describe "Regex Formats Gen" $ do
    it "should get support string types" $ do
      RequestMessages.valueToFormatStr "123" `shouldBe` "integer"
      RequestMessages.valueToFormatStr "abc" `shouldBe` "text"
